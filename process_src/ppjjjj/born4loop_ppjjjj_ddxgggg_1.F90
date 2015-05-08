
module ol_tree_ppjjjj_ddxgggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(2)
  complex(REALKIND), save :: den(215)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 64 ! number of helicity configurations
  integer(intkind2), save :: nhel = 64 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(64) ! physical helicity states
  complex(DREALKIND) :: M1helarr(24,64) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = CI*gQCD**4
    f(2) = gQCD**4

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,3))
  den(2) = 1 / (Q(5,12))
  den(4) = 1 / (Q(5,20))
  den(6) = 1 / (Q(5,36))
  den(8) = 1 / (Q(5,56))
  den(10) = 1 / (Q(5,24))
  den(12) = 1 / (Q(5,40))
  den(14) = 1 / (Q(5,52))
  den(16) = 1 / (Q(5,48))
  den(18) = 1 / (Q(5,44))
  den(20) = 1 / (Q(5,28))
  den(22) = 1 / (Q(5,5))
  den(24) = 1 / (Q(5,6))
  den(26) = 1 / (Q(5,9))
  den(28) = 1 / (Q(5,10))
  den(30) = 1 / (Q(5,17))
  den(32) = 1 / (Q(5,18))
  den(34) = 1 / (Q(5,33))
  den(36) = 1 / (Q(5,34))
  den(39) = 1 / (Q(5,19))
  den(45) = 1 / (Q(5,11))
  den(51) = 1 / (Q(5,7))
  den(67) = 1 / (Q(5,21))
  den(70) = 1 / (Q(5,26))
  den(75) = 1 / (Q(5,13))
  den(87) = 1 / (Q(5,42))
  den(94) = 1 / (Q(5,50))
  den(99) = 1 / (Q(5,25))
  den(102) = 1 / (Q(5,22))
  den(109) = 1 / (Q(5,14))
  den(118) = 1 / (Q(5,41))
  den(126) = 1 / (Q(5,49))
  den(142) = 1 / (Q(5,38))
  den(162) = 1 / (Q(5,37))
  den(183) = 1 / (Q(5,35))

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
  den(23) = den(8)*den(22)
  den(25) = den(8)*den(24)
  den(27) = den(14)*den(26)
  den(29) = den(14)*den(28)
  den(31) = den(18)*den(30)
  den(33) = den(18)*den(32)
  den(35) = den(20)*den(34)
  den(37) = den(20)*den(36)
  den(38) = den(3)*den(16)
  den(40) = den(1)*den(39)
  den(41) = den(2)*den(40)
  den(42) = den(2)*den(20)
  den(43) = den(1)*den(42)
  den(44) = den(5)*den(12)
  den(46) = den(1)*den(45)
  den(47) = den(4)*den(46)
  den(48) = den(4)*den(20)
  den(49) = den(1)*den(48)
  den(50) = den(7)*den(10)
  den(52) = den(1)*den(51)
  den(53) = den(10)*den(52)
  den(54) = den(10)*den(20)
  den(55) = den(1)*den(54)
  den(56) = den(6)*den(46)
  den(57) = den(6)*den(18)
  den(58) = den(1)*den(57)
  den(59) = den(12)*den(52)
  den(60) = den(12)*den(18)
  den(61) = den(1)*den(60)
  den(62) = den(16)*den(52)
  den(63) = den(14)*den(16)
  den(64) = den(1)*den(63)
  den(65) = den(22)*den(28)
  den(66) = den(16)*den(65)
  den(68) = den(22)*den(67)
  den(69) = den(28)*den(68)
  den(71) = den(28)*den(70)
  den(72) = den(22)*den(71)
  den(73) = den(22)*den(32)
  den(74) = den(12)*den(73)
  den(76) = den(22)*den(75)
  den(77) = den(32)*den(76)
  den(78) = den(32)*den(70)
  den(79) = den(22)*den(78)
  den(80) = den(22)*den(36)
  den(81) = den(10)*den(80)
  den(82) = den(22)*den(51)
  den(83) = den(10)*den(82)
  den(84) = den(10)*den(70)
  den(85) = den(22)*den(84)
  den(86) = den(36)*den(76)
  den(88) = den(36)*den(87)
  den(89) = den(22)*den(88)
  den(90) = den(12)*den(82)
  den(91) = den(12)*den(87)
  den(92) = den(22)*den(91)
  den(93) = den(16)*den(82)
  den(95) = den(16)*den(94)
  den(96) = den(22)*den(95)
  den(97) = den(24)*den(26)
  den(98) = den(16)*den(97)
  den(100) = den(26)*den(99)
  den(101) = den(24)*den(100)
  den(103) = den(24)*den(102)
  den(104) = den(26)*den(103)
  den(105) = den(24)*den(30)
  den(106) = den(12)*den(105)
  den(107) = den(30)*den(99)
  den(108) = den(24)*den(107)
  den(110) = den(24)*den(109)
  den(111) = den(30)*den(110)
  den(112) = den(24)*den(34)
  den(113) = den(10)*den(112)
  den(114) = den(24)*den(51)
  den(115) = den(10)*den(114)
  den(116) = den(10)*den(99)
  den(117) = den(24)*den(116)
  den(119) = den(34)*den(118)
  den(120) = den(24)*den(119)
  den(121) = den(34)*den(110)
  den(122) = den(12)*den(114)
  den(123) = den(12)*den(118)
  den(124) = den(24)*den(123)
  den(125) = den(16)*den(114)
  den(127) = den(16)*den(126)
  den(128) = den(24)*den(127)
  den(129) = den(26)*den(32)
  den(130) = den(6)*den(129)
  den(131) = den(26)*den(75)
  den(132) = den(32)*den(131)
  den(133) = den(32)*den(102)
  den(134) = den(26)*den(133)
  den(135) = den(26)*den(36)
  den(136) = den(4)*den(135)
  den(137) = den(26)*den(45)
  den(138) = den(4)*den(137)
  den(139) = den(4)*den(102)
  den(140) = den(26)*den(139)
  den(141) = den(36)*den(131)
  den(143) = den(36)*den(142)
  den(144) = den(26)*den(143)
  den(145) = den(6)*den(137)
  den(146) = den(6)*den(142)
  den(147) = den(26)*den(146)
  den(148) = den(16)*den(137)
  den(149) = den(26)*den(95)
  den(150) = den(28)*den(30)
  den(151) = den(6)*den(150)
  den(152) = den(30)*den(67)
  den(153) = den(28)*den(152)
  den(154) = den(28)*den(109)
  den(155) = den(30)*den(154)
  den(156) = den(28)*den(34)
  den(157) = den(4)*den(156)
  den(158) = den(28)*den(45)
  den(159) = den(4)*den(158)
  den(160) = den(4)*den(67)
  den(161) = den(28)*den(160)
  den(163) = den(34)*den(162)
  den(164) = den(28)*den(163)
  den(165) = den(34)*den(154)
  den(166) = den(6)*den(158)
  den(167) = den(6)*den(162)
  den(168) = den(28)*den(167)
  den(169) = den(16)*den(158)
  den(170) = den(28)*den(127)
  den(171) = den(30)*den(36)
  den(172) = den(2)*den(171)
  den(173) = den(30)*den(39)
  den(174) = den(2)*den(173)
  den(175) = den(2)*den(109)
  den(176) = den(30)*den(175)
  den(177) = den(32)*den(34)
  den(178) = den(2)*den(177)
  den(179) = den(32)*den(39)
  den(180) = den(2)*den(179)
  den(181) = den(2)*den(75)
  den(182) = den(32)*den(181)
  den(184) = den(34)*den(183)
  den(185) = den(2)*den(184)
  den(186) = den(34)*den(175)
  den(187) = den(36)*den(183)
  den(188) = den(2)*den(187)
  den(189) = den(36)*den(181)
  den(190) = den(16)*den(181)
  den(191) = den(2)*den(127)
  den(192) = den(36)*den(152)
  den(193) = den(30)*den(143)
  den(194) = den(6)*den(173)
  den(195) = den(30)*den(146)
  den(196) = den(12)*den(173)
  den(197) = den(30)*den(91)
  den(198) = den(32)*den(163)
  den(199) = den(34)*den(133)
  den(200) = den(6)*den(179)
  den(201) = den(32)*den(167)
  den(202) = den(12)*den(179)
  den(203) = den(32)*den(123)
  den(204) = den(4)*den(184)
  den(205) = den(34)*den(139)
  den(206) = den(4)*den(187)
  den(207) = den(36)*den(160)
  den(208) = den(12)*den(160)
  den(209) = den(4)*den(123)
  den(210) = den(10)*den(184)
  den(211) = den(34)*den(84)
  den(212) = den(10)*den(187)
  den(213) = den(36)*den(116)
  den(214) = den(10)*den(167)
  den(215) = den(6)*den(116)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppjjjj_ddxgggg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppjjjj_ddxgggg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for down anti-down glue glue glue glue -> 0
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
  use ol_external_ppjjjj_ddxgggg_1, only: external_perm_ppjjjj_ddxgggg_1, &
    & external_perm_inv_ppjjjj_ddxgggg_1, extcomb_perm_ppjjjj_ddxgggg_1, &
    & average_factor_ppjjjj_ddxgggg_1
  use ol_external_ppjjjj_ddxgggg_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_ppjjjj_ddxgggg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppjjjj_ddxgggg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_ppjjjj_ddxgggg_1
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
  complex(REALKIND) :: MOM_LC(4), M1(24), M1helarray(24,64)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,23), wf8(8,100), wf16(16,33), wf64(64,159)

  type(polcont) :: A(64,159)
  complex(REALKIND) :: Aj(159)

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
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppjjjj_ddxgggg_1,6)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,6)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppjjjj_ddxgggg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppjjjj_ddxgggg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppjjjj_ddxgggg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rZERO, H1, ex1)
  call wf_A(P(:,2), rZERO, H2, ex2)
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
  call vert_QA_V(ntry, ex1, ex2, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_UV_W(ntry, ex3, Q(:,4), ex4, Q(:,8), wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_GGG_G(ntry, wf4(:,1), ex5, ex6, wf16(:,1), n4(:,1), t4x16(:,:,1))
  call vert_GGG_G(ntry, ex5, ex6, wf4(:,1), wf16(:,2), n4(:,2), t4x16(:,:,2))
  call vert_GGG_G(ntry, ex6, wf4(:,1), ex5, wf16(:,3), n4(:,3), t4x16(:,:,3))
  call vert_UV_W(ntry, ex3, Q(:,4), ex5, Q(:,16), wf4(:,3), n3(:,3), t3x4(:,:,3))
  call vert_GGG_G(ntry, wf4(:,1), ex4, ex6, wf16(:,4), n4(:,4), t4x16(:,:,4))
  call vert_GGG_G(ntry, ex4, ex6, wf4(:,1), wf16(:,5), n4(:,5), t4x16(:,:,5))
  call vert_GGG_G(ntry, ex6, wf4(:,1), ex4, wf16(:,6), n4(:,6), t4x16(:,:,6))
  call vert_UV_W(ntry, ex3, Q(:,4), ex6, Q(:,32), wf4(:,4), n3(:,4), t3x4(:,:,4))
  call vert_GGG_G(ntry, wf4(:,1), ex4, ex5, wf16(:,7), n4(:,7), t4x16(:,:,7))
  call vert_GGG_G(ntry, ex4, ex5, wf4(:,1), wf16(:,8), n4(:,8), t4x16(:,:,8))
  call vert_GGG_G(ntry, ex5, wf4(:,1), ex4, wf16(:,9), n4(:,9), t4x16(:,:,9))
  call vert_GGG_G(ntry, ex4, ex5, ex6, wf8(:,1), n4(:,10), t4x8(:,:,1))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), ex3, Q(:,4), wf8(:,2), n3(:,5), t3x8(:,:,1))
  call vert_GGG_G(ntry, ex5, ex6, ex4, wf8(:,3), n4(:,11), t4x8(:,:,2))
  call vert_GGG_G(ntry, ex6, ex4, ex5, wf8(:,4), n4(:,12), t4x8(:,:,3))
  call vert_UV_W(ntry, ex4, Q(:,8), ex5, Q(:,16), wf4(:,5), n3(:,6), t3x4(:,:,5))
  call vert_GGG_G(ntry, wf4(:,1), ex3, ex6, wf16(:,10), n4(:,13), t4x16(:,:,10))
  call vert_GGG_G(ntry, ex3, ex6, wf4(:,1), wf16(:,11), n4(:,14), t4x16(:,:,11))
  call vert_GGG_G(ntry, ex6, wf4(:,1), ex3, wf16(:,12), n4(:,15), t4x16(:,:,12))
  call vert_UV_W(ntry, ex4, Q(:,8), ex6, Q(:,32), wf4(:,6), n3(:,7), t3x4(:,:,6))
  call vert_GGG_G(ntry, wf4(:,1), ex3, ex5, wf16(:,13), n4(:,16), t4x16(:,:,13))
  call vert_GGG_G(ntry, ex3, ex5, wf4(:,1), wf16(:,14), n4(:,17), t4x16(:,:,14))
  call vert_GGG_G(ntry, ex5, wf4(:,1), ex3, wf16(:,15), n4(:,18), t4x16(:,:,15))
  call vert_GGG_G(ntry, ex3, ex5, ex6, wf8(:,5), n4(:,19), t4x8(:,:,4))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), ex4, Q(:,8), wf8(:,6), n3(:,8), t3x8(:,:,2))
  call vert_GGG_G(ntry, ex5, ex6, ex3, wf8(:,7), n4(:,20), t4x8(:,:,5))
  call vert_GGG_G(ntry, ex6, ex3, ex5, wf8(:,8), n4(:,21), t4x8(:,:,6))
  call vert_UV_W(ntry, ex5, Q(:,16), ex6, Q(:,32), wf4(:,7), n3(:,9), t3x4(:,:,7))
  call vert_GGG_G(ntry, wf4(:,1), ex3, ex4, wf16(:,16), n4(:,22), t4x16(:,:,16))
  call vert_GGG_G(ntry, ex3, ex4, wf4(:,1), wf16(:,17), n4(:,23), t4x16(:,:,17))
  call vert_GGG_G(ntry, ex4, wf4(:,1), ex3, wf16(:,18), n4(:,24), t4x16(:,:,18))
  call vert_GGG_G(ntry, ex3, ex4, ex6, wf8(:,9), n4(:,25), t4x8(:,:,7))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), ex5, Q(:,16), wf8(:,10), n3(:,10), t3x8(:,:,3))
  call vert_GGG_G(ntry, ex4, ex6, ex3, wf8(:,11), n4(:,26), t4x8(:,:,8))
  call vert_GGG_G(ntry, ex6, ex3, ex4, wf8(:,12), n4(:,27), t4x8(:,:,9))
  call vert_GGG_G(ntry, ex3, ex4, ex5, wf8(:,13), n4(:,28), t4x8(:,:,10))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), ex6, Q(:,32), wf8(:,14), n3(:,11), t3x8(:,:,4))
  call vert_GGG_G(ntry, ex4, ex5, ex3, wf8(:,15), n4(:,29), t4x8(:,:,11))
  call vert_GGG_G(ntry, ex5, ex3, ex4, wf8(:,16), n4(:,30), t4x8(:,:,12))
  call vert_VQ_A(ntry, ex3, ex1, wf4(:,8), n3(:,12), t3x4(:,:,8))
  call prop_Q_A(ntry, wf4(:,8), Q(:,5), ZERO, 0_intkind1, wf4(:,9), n2(1))
  call vert_QA_V(ntry, wf4(:,9), ex2, wf8(:,17), n3(:,13), t3x8(:,:,5))
  call vert_AV_Q(ntry, ex2, ex3, wf4(:,10), n3(:,14), t3x4(:,:,9))
  call prop_A_Q(ntry, wf4(:,10), Q(:,6), ZERO, 0_intkind1, wf4(:,11), n2(2))
  call vert_QA_V(ntry, ex1, wf4(:,11), wf8(:,18), n3(:,15), t3x8(:,:,6))
  call vert_VQ_A(ntry, ex4, ex1, wf4(:,12), n3(:,16), t3x4(:,:,10))
  call prop_Q_A(ntry, wf4(:,12), Q(:,9), ZERO, 0_intkind1, wf4(:,13), n2(3))
  call vert_QA_V(ntry, wf4(:,13), ex2, wf8(:,19), n3(:,17), t3x8(:,:,7))
  call vert_AV_Q(ntry, ex2, ex4, wf4(:,14), n3(:,18), t3x4(:,:,11))
  call prop_A_Q(ntry, wf4(:,14), Q(:,10), ZERO, 0_intkind1, wf4(:,15), n2(4))
  call vert_QA_V(ntry, ex1, wf4(:,15), wf8(:,20), n3(:,19), t3x8(:,:,8))
  call vert_VQ_A(ntry, ex5, ex1, wf4(:,16), n3(:,20), t3x4(:,:,12))
  call prop_Q_A(ntry, wf4(:,16), Q(:,17), ZERO, 0_intkind1, wf4(:,17), n2(5))
  call vert_QA_V(ntry, wf4(:,17), ex2, wf8(:,21), n3(:,21), t3x8(:,:,9))
  call vert_AV_Q(ntry, ex2, ex5, wf4(:,18), n3(:,22), t3x4(:,:,13))
  call prop_A_Q(ntry, wf4(:,18), Q(:,18), ZERO, 0_intkind1, wf4(:,19), n2(6))
  call vert_QA_V(ntry, ex1, wf4(:,19), wf8(:,22), n3(:,23), t3x8(:,:,10))
  call vert_VQ_A(ntry, ex6, ex1, wf4(:,20), n3(:,24), t3x4(:,:,14))
  call prop_Q_A(ntry, wf4(:,20), Q(:,33), ZERO, 0_intkind1, wf4(:,21), n2(7))
  call vert_QA_V(ntry, wf4(:,21), ex2, wf8(:,23), n3(:,25), t3x8(:,:,11))
  call vert_AV_Q(ntry, ex2, ex6, wf4(:,22), n3(:,26), t3x4(:,:,15))
  call prop_A_Q(ntry, wf4(:,22), Q(:,34), ZERO, 0_intkind1, wf4(:,23), n2(8))
  call vert_QA_V(ntry, ex1, wf4(:,23), wf8(:,24), n3(:,27), t3x8(:,:,12))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), wf4(:,2), Q(:,12), wf16(:,19), n3(:,28), t3x16(:,:,1))
  call vert_UV_W(ntry, wf4(:,2), Q(:,12), ex6, Q(:,32), wf8(:,25), n3(:,29), t3x8(:,:,13))
  call vert_UV_W(ntry, wf4(:,2), Q(:,12), ex5, Q(:,16), wf8(:,26), n3(:,30), t3x8(:,:,14))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), wf4(:,3), Q(:,20), wf16(:,20), n3(:,31), t3x16(:,:,2))
  call vert_UV_W(ntry, wf4(:,3), Q(:,20), ex6, Q(:,32), wf8(:,27), n3(:,32), t3x8(:,:,15))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,3), Q(:,20), wf8(:,28), n3(:,33), t3x8(:,:,16))
  call vert_UV_W(ntry, wf4(:,1), Q(:,3), wf4(:,4), Q(:,36), wf16(:,21), n3(:,34), t3x16(:,:,3))
  call vert_UV_W(ntry, wf4(:,5), Q(:,24), ex6, Q(:,32), wf8(:,29), n3(:,35), t3x8(:,:,17))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,5), Q(:,24), wf8(:,30), n3(:,36), t3x8(:,:,18))
  call vert_UV_W(ntry, ex5, Q(:,16), wf4(:,4), Q(:,36), wf8(:,31), n3(:,37), t3x8(:,:,19))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,4), Q(:,36), wf8(:,32), n3(:,38), t3x8(:,:,20))
  call vert_UV_W(ntry, ex5, Q(:,16), wf4(:,6), Q(:,40), wf8(:,33), n3(:,39), t3x8(:,:,21))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,6), Q(:,40), wf8(:,34), n3(:,40), t3x8(:,:,22))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,7), Q(:,48), wf8(:,35), n3(:,41), t3x8(:,:,23))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,7), Q(:,48), wf8(:,36), n3(:,42), t3x8(:,:,24))
  call vert_QA_V(ntry, wf4(:,9), wf4(:,15), wf16(:,22), n3(:,43), t3x16(:,:,4))
  call vert_VQ_A(ntry, ex5, wf4(:,9), wf8(:,37), n3(:,44), t3x8(:,:,25))
  call vert_AV_Q(ntry, wf4(:,15), ex6, wf8(:,38), n3(:,45), t3x8(:,:,26))
  call prop_Q_A(ntry, wf8(:,37), Q(:,21), ZERO, 0_intkind1, wf8(:,39), n2(9))
  call vert_AV_Q(ntry, wf4(:,15), ex5, wf8(:,40), n3(:,46), t3x8(:,:,27))
  call vert_VQ_A(ntry, ex6, wf4(:,9), wf8(:,41), n3(:,47), t3x8(:,:,28))
  call prop_A_Q(ntry, wf8(:,40), Q(:,26), ZERO, 0_intkind1, wf8(:,42), n2(10))
  call vert_QA_V(ntry, wf4(:,9), wf4(:,19), wf16(:,23), n3(:,48), t3x16(:,:,5))
  call vert_VQ_A(ntry, ex4, wf4(:,9), wf8(:,43), n3(:,49), t3x8(:,:,29))
  call vert_AV_Q(ntry, wf4(:,19), ex6, wf8(:,44), n3(:,50), t3x8(:,:,30))
  call prop_Q_A(ntry, wf8(:,43), Q(:,13), ZERO, 0_intkind1, wf8(:,45), n2(11))
  call vert_AV_Q(ntry, wf4(:,19), ex4, wf8(:,46), n3(:,51), t3x8(:,:,31))
  call prop_A_Q(ntry, wf8(:,46), Q(:,26), ZERO, 0_intkind1, wf8(:,47), n2(12))
  call vert_QA_V(ntry, wf4(:,9), wf4(:,23), wf16(:,24), n3(:,52), t3x16(:,:,6))
  call vert_AV_Q(ntry, ex2, wf4(:,5), wf8(:,48), n3(:,53), t3x8(:,:,32))
  call prop_A_Q(ntry, wf8(:,48), Q(:,26), ZERO, 0_intkind1, wf8(:,49), n2(13))
  call vert_AV_Q(ntry, wf4(:,23), ex5, wf8(:,50), n3(:,54), t3x8(:,:,33))
  call vert_AV_Q(ntry, wf4(:,23), ex4, wf8(:,51), n3(:,55), t3x8(:,:,34))
  call prop_A_Q(ntry, wf8(:,51), Q(:,42), ZERO, 0_intkind1, wf8(:,52), n2(14))
  call vert_AV_Q(ntry, ex2, wf4(:,6), wf8(:,53), n3(:,56), t3x8(:,:,35))
  call prop_A_Q(ntry, wf8(:,53), Q(:,42), ZERO, 0_intkind1, wf8(:,54), n2(15))
  call vert_AV_Q(ntry, ex2, wf4(:,7), wf8(:,55), n3(:,57), t3x8(:,:,36))
  call prop_A_Q(ntry, wf8(:,55), Q(:,50), ZERO, 0_intkind1, wf8(:,56), n2(16))
  call vert_QA_V(ntry, wf4(:,13), wf4(:,11), wf16(:,25), n3(:,58), t3x16(:,:,7))
  call vert_VQ_A(ntry, ex5, wf4(:,13), wf8(:,57), n3(:,59), t3x8(:,:,37))
  call vert_AV_Q(ntry, wf4(:,11), ex6, wf8(:,58), n3(:,60), t3x8(:,:,38))
  call prop_Q_A(ntry, wf8(:,57), Q(:,25), ZERO, 0_intkind1, wf8(:,59), n2(17))
  call vert_AV_Q(ntry, wf4(:,11), ex5, wf8(:,60), n3(:,61), t3x8(:,:,39))
  call vert_VQ_A(ntry, ex6, wf4(:,13), wf8(:,61), n3(:,62), t3x8(:,:,40))
  call prop_A_Q(ntry, wf8(:,60), Q(:,22), ZERO, 0_intkind1, wf8(:,62), n2(18))
  call vert_QA_V(ntry, wf4(:,17), wf4(:,11), wf16(:,26), n3(:,63), t3x16(:,:,8))
  call vert_VQ_A(ntry, ex4, wf4(:,17), wf8(:,63), n3(:,64), t3x8(:,:,41))
  call prop_Q_A(ntry, wf8(:,63), Q(:,25), ZERO, 0_intkind1, wf8(:,64), n2(19))
  call vert_AV_Q(ntry, wf4(:,11), ex4, wf8(:,65), n3(:,65), t3x8(:,:,42))
  call vert_VQ_A(ntry, ex6, wf4(:,17), wf8(:,66), n3(:,66), t3x8(:,:,43))
  call prop_A_Q(ntry, wf8(:,65), Q(:,14), ZERO, 0_intkind1, wf8(:,67), n2(20))
  call vert_QA_V(ntry, wf4(:,21), wf4(:,11), wf16(:,27), n3(:,67), t3x16(:,:,9))
  call vert_VQ_A(ntry, wf4(:,5), ex1, wf8(:,68), n3(:,68), t3x8(:,:,44))
  call prop_Q_A(ntry, wf8(:,68), Q(:,25), ZERO, 0_intkind1, wf8(:,69), n2(21))
  call vert_VQ_A(ntry, ex4, wf4(:,21), wf8(:,70), n3(:,69), t3x8(:,:,45))
  call prop_Q_A(ntry, wf8(:,70), Q(:,41), ZERO, 0_intkind1, wf8(:,71), n2(22))
  call vert_VQ_A(ntry, ex5, wf4(:,21), wf8(:,72), n3(:,70), t3x8(:,:,46))
  call vert_VQ_A(ntry, wf4(:,6), ex1, wf8(:,73), n3(:,71), t3x8(:,:,47))
  call prop_Q_A(ntry, wf8(:,73), Q(:,41), ZERO, 0_intkind1, wf8(:,74), n2(23))
  call vert_VQ_A(ntry, wf4(:,7), ex1, wf8(:,75), n3(:,72), t3x8(:,:,48))
  call prop_Q_A(ntry, wf8(:,75), Q(:,49), ZERO, 0_intkind1, wf8(:,76), n2(24))
  call vert_QA_V(ntry, wf4(:,13), wf4(:,19), wf16(:,28), n3(:,73), t3x16(:,:,10))
  call vert_VQ_A(ntry, ex3, wf4(:,13), wf8(:,77), n3(:,74), t3x8(:,:,49))
  call prop_Q_A(ntry, wf8(:,77), Q(:,13), ZERO, 0_intkind1, wf8(:,78), n2(25))
  call vert_AV_Q(ntry, wf4(:,19), ex3, wf8(:,79), n3(:,75), t3x8(:,:,50))
  call prop_A_Q(ntry, wf8(:,79), Q(:,22), ZERO, 0_intkind1, wf8(:,80), n2(26))
  call vert_QA_V(ntry, wf4(:,13), wf4(:,23), wf16(:,29), n3(:,76), t3x16(:,:,11))
  call vert_AV_Q(ntry, ex2, wf4(:,3), wf8(:,81), n3(:,77), t3x8(:,:,51))
  call prop_A_Q(ntry, wf8(:,81), Q(:,22), ZERO, 0_intkind1, wf8(:,82), n2(27))
  call vert_AV_Q(ntry, wf4(:,23), ex3, wf8(:,83), n3(:,78), t3x8(:,:,52))
  call prop_A_Q(ntry, wf8(:,83), Q(:,38), ZERO, 0_intkind1, wf8(:,84), n2(28))
  call vert_AV_Q(ntry, ex2, wf4(:,4), wf8(:,85), n3(:,79), t3x8(:,:,53))
  call prop_A_Q(ntry, wf8(:,85), Q(:,38), ZERO, 0_intkind1, wf8(:,86), n2(29))
  call vert_QA_V(ntry, wf4(:,17), wf4(:,15), wf16(:,30), n3(:,80), t3x16(:,:,12))
  call vert_VQ_A(ntry, ex3, wf4(:,17), wf8(:,87), n3(:,81), t3x8(:,:,54))
  call prop_Q_A(ntry, wf8(:,87), Q(:,21), ZERO, 0_intkind1, wf8(:,88), n2(30))
  call vert_AV_Q(ntry, wf4(:,15), ex3, wf8(:,89), n3(:,82), t3x8(:,:,55))
  call prop_A_Q(ntry, wf8(:,89), Q(:,14), ZERO, 0_intkind1, wf8(:,90), n2(31))
  call vert_QA_V(ntry, wf4(:,21), wf4(:,15), wf16(:,31), n3(:,83), t3x16(:,:,13))
  call vert_VQ_A(ntry, wf4(:,3), ex1, wf8(:,91), n3(:,84), t3x8(:,:,56))
  call prop_Q_A(ntry, wf8(:,91), Q(:,21), ZERO, 0_intkind1, wf8(:,92), n2(32))
  call vert_VQ_A(ntry, ex3, wf4(:,21), wf8(:,93), n3(:,85), t3x8(:,:,57))
  call prop_Q_A(ntry, wf8(:,93), Q(:,37), ZERO, 0_intkind1, wf8(:,94), n2(33))
  call vert_VQ_A(ntry, wf4(:,4), ex1, wf8(:,95), n3(:,86), t3x8(:,:,58))
  call prop_Q_A(ntry, wf8(:,95), Q(:,37), ZERO, 0_intkind1, wf8(:,96), n2(34))
  call vert_QA_V(ntry, wf4(:,17), wf4(:,23), wf16(:,32), n3(:,87), t3x16(:,:,14))
  call vert_AV_Q(ntry, ex2, wf4(:,2), wf8(:,97), n3(:,88), t3x8(:,:,59))
  call prop_A_Q(ntry, wf8(:,97), Q(:,14), ZERO, 0_intkind1, wf8(:,98), n2(35))
  call vert_QA_V(ntry, wf4(:,21), wf4(:,19), wf16(:,33), n3(:,89), t3x16(:,:,15))
  call vert_VQ_A(ntry, wf4(:,2), ex1, wf8(:,99), n3(:,90), t3x8(:,:,60))
  call prop_Q_A(ntry, wf8(:,99), Q(:,13), ZERO, 0_intkind1, wf8(:,100), n2(36))


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
  M2add = M2 / average_factor_ppjjjj_ddxgggg_1

  do k = 0, 23-1
    M2(k) = M2add(extcomb_perm_ppjjjj_ddxgggg_1(k))
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

    call cont_VV(nsync, wf4(:,2), wf16(:,1), A(:,1), n3(:,91), t3x64(:,:,1), nhel, den(3))
    call cont_VV(nsync, wf4(:,2), wf16(:,2), A(:,2), n3(:,92), t3x64(:,:,2), nhel, den(3))
    call cont_VV(nsync, wf4(:,2), wf16(:,3), A(:,3), n3(:,93), t3x64(:,:,3), nhel, den(3))
    call cont_VV(nsync, wf4(:,3), wf16(:,4), A(:,4), n3(:,94), t3x64(:,:,4), nhel, den(5))
    call cont_VV(nsync, wf4(:,3), wf16(:,5), A(:,5), n3(:,95), t3x64(:,:,5), nhel, den(5))
    call cont_VV(nsync, wf4(:,3), wf16(:,6), A(:,6), n3(:,96), t3x64(:,:,6), nhel, den(5))
    call cont_VV(nsync, wf4(:,4), wf16(:,7), A(:,7), n3(:,97), t3x64(:,:,7), nhel, den(7))
    call cont_VV(nsync, wf4(:,4), wf16(:,8), A(:,8), n3(:,98), t3x64(:,:,8), nhel, den(7))
    call cont_VV(nsync, wf4(:,4), wf16(:,9), A(:,9), n3(:,99), t3x64(:,:,9), nhel, den(7))
    call cont_VV(nsync, wf8(:,1), wf8(:,2), A(:,10), n3(:,100), t3x64(:,:,10), nhel, den(9))
    call cont_VV(nsync, wf8(:,2), wf8(:,3), A(:,11), n3(:,101), t3x64(:,:,11), nhel, den(9))
    call cont_VV(nsync, wf8(:,2), wf8(:,4), A(:,12), n3(:,102), t3x64(:,:,12), nhel, den(9))
    call cont_VV(nsync, wf4(:,5), wf16(:,10), A(:,13), n3(:,103), t3x64(:,:,13), nhel, den(11))
    call cont_VV(nsync, wf4(:,5), wf16(:,11), A(:,14), n3(:,104), t3x64(:,:,14), nhel, den(11))
    call cont_VV(nsync, wf4(:,5), wf16(:,12), A(:,15), n3(:,105), t3x64(:,:,15), nhel, den(11))
    call cont_VV(nsync, wf4(:,6), wf16(:,13), A(:,16), n3(:,106), t3x64(:,:,16), nhel, den(13))
    call cont_VV(nsync, wf4(:,6), wf16(:,14), A(:,17), n3(:,107), t3x64(:,:,17), nhel, den(13))
    call cont_VV(nsync, wf4(:,6), wf16(:,15), A(:,18), n3(:,108), t3x64(:,:,18), nhel, den(13))
    call cont_VV(nsync, wf8(:,5), wf8(:,6), A(:,19), n3(:,109), t3x64(:,:,19), nhel, den(15))
    call cont_VV(nsync, wf8(:,6), wf8(:,7), A(:,20), n3(:,110), t3x64(:,:,20), nhel, den(15))
    call cont_VV(nsync, wf8(:,6), wf8(:,8), A(:,21), n3(:,111), t3x64(:,:,21), nhel, den(15))
    call cont_VV(nsync, wf4(:,7), wf16(:,16), A(:,22), n3(:,112), t3x64(:,:,22), nhel, den(17))
    call cont_VV(nsync, wf4(:,7), wf16(:,17), A(:,23), n3(:,113), t3x64(:,:,23), nhel, den(17))
    call cont_VV(nsync, wf4(:,7), wf16(:,18), A(:,24), n3(:,114), t3x64(:,:,24), nhel, den(17))
    call cont_VV(nsync, wf8(:,9), wf8(:,10), A(:,25), n3(:,115), t3x64(:,:,25), nhel, den(19))
    call cont_VV(nsync, wf8(:,10), wf8(:,11), A(:,26), n3(:,116), t3x64(:,:,26), nhel, den(19))
    call cont_VV(nsync, wf8(:,10), wf8(:,12), A(:,27), n3(:,117), t3x64(:,:,27), nhel, den(19))
    call cont_VV(nsync, wf8(:,13), wf8(:,14), A(:,28), n3(:,118), t3x64(:,:,28), nhel, den(21))
    call cont_VV(nsync, wf8(:,14), wf8(:,15), A(:,29), n3(:,119), t3x64(:,:,29), nhel, den(21))
    call cont_VV(nsync, wf8(:,14), wf8(:,16), A(:,30), n3(:,120), t3x64(:,:,30), nhel, den(21))
    call cont_VV(nsync, wf8(:,1), wf8(:,17), A(:,31), n3(:,121), t3x64(:,:,31), nhel, den(23))
    call cont_VV(nsync, wf8(:,3), wf8(:,17), A(:,32), n3(:,122), t3x64(:,:,32), nhel, den(23))
    call cont_VV(nsync, wf8(:,4), wf8(:,17), A(:,33), n3(:,123), t3x64(:,:,33), nhel, den(23))
    call cont_VV(nsync, wf8(:,1), wf8(:,18), A(:,34), n3(:,124), t3x64(:,:,34), nhel, den(25))
    call cont_VV(nsync, wf8(:,3), wf8(:,18), A(:,35), n3(:,125), t3x64(:,:,35), nhel, den(25))
    call cont_VV(nsync, wf8(:,4), wf8(:,18), A(:,36), n3(:,126), t3x64(:,:,36), nhel, den(25))
    call cont_VV(nsync, wf8(:,5), wf8(:,19), A(:,37), n3(:,127), t3x64(:,:,37), nhel, den(27))
    call cont_VV(nsync, wf8(:,7), wf8(:,19), A(:,38), n3(:,128), t3x64(:,:,38), nhel, den(27))
    call cont_VV(nsync, wf8(:,8), wf8(:,19), A(:,39), n3(:,129), t3x64(:,:,39), nhel, den(27))
    call cont_VV(nsync, wf8(:,5), wf8(:,20), A(:,40), n3(:,130), t3x64(:,:,40), nhel, den(29))
    call cont_VV(nsync, wf8(:,7), wf8(:,20), A(:,41), n3(:,131), t3x64(:,:,41), nhel, den(29))
    call cont_VV(nsync, wf8(:,8), wf8(:,20), A(:,42), n3(:,132), t3x64(:,:,42), nhel, den(29))
    call cont_VV(nsync, wf8(:,9), wf8(:,21), A(:,43), n3(:,133), t3x64(:,:,43), nhel, den(31))
    call cont_VV(nsync, wf8(:,11), wf8(:,21), A(:,44), n3(:,134), t3x64(:,:,44), nhel, den(31))
    call cont_VV(nsync, wf8(:,12), wf8(:,21), A(:,45), n3(:,135), t3x64(:,:,45), nhel, den(31))
    call cont_VV(nsync, wf8(:,9), wf8(:,22), A(:,46), n3(:,136), t3x64(:,:,46), nhel, den(33))
    call cont_VV(nsync, wf8(:,11), wf8(:,22), A(:,47), n3(:,137), t3x64(:,:,47), nhel, den(33))
    call cont_VV(nsync, wf8(:,12), wf8(:,22), A(:,48), n3(:,138), t3x64(:,:,48), nhel, den(33))
    call cont_VV(nsync, wf8(:,13), wf8(:,23), A(:,49), n3(:,139), t3x64(:,:,49), nhel, den(35))
    call cont_VV(nsync, wf8(:,15), wf8(:,23), A(:,50), n3(:,140), t3x64(:,:,50), nhel, den(35))
    call cont_VV(nsync, wf8(:,16), wf8(:,23), A(:,51), n3(:,141), t3x64(:,:,51), nhel, den(35))
    call cont_VV(nsync, wf8(:,13), wf8(:,24), A(:,52), n3(:,142), t3x64(:,:,52), nhel, den(37))
    call cont_VV(nsync, wf8(:,15), wf8(:,24), A(:,53), n3(:,143), t3x64(:,:,53), nhel, den(37))
    call cont_VV(nsync, wf8(:,16), wf8(:,24), A(:,54), n3(:,144), t3x64(:,:,54), nhel, den(37))
    call cont_VV(nsync, wf4(:,7), wf16(:,19), A(:,55), n3(:,145), t3x64(:,:,55), nhel, den(38))
    call cont_VV(nsync, wf8(:,10), wf8(:,25), A(:,56), n3(:,146), t3x64(:,:,56), nhel, den(41))
    call cont_VV(nsync, wf8(:,14), wf8(:,26), A(:,57), n3(:,147), t3x64(:,:,57), nhel, den(43))
    call cont_VV(nsync, wf4(:,6), wf16(:,20), A(:,58), n3(:,148), t3x64(:,:,58), nhel, den(44))
    call cont_VV(nsync, wf8(:,6), wf8(:,27), A(:,59), n3(:,149), t3x64(:,:,59), nhel, den(47))
    call cont_VV(nsync, wf8(:,14), wf8(:,28), A(:,60), n3(:,150), t3x64(:,:,60), nhel, den(49))
    call cont_VV(nsync, wf4(:,5), wf16(:,21), A(:,61), n3(:,151), t3x64(:,:,61), nhel, den(50))
    call cont_VV(nsync, wf8(:,2), wf8(:,29), A(:,62), n3(:,152), t3x64(:,:,62), nhel, den(53))
    call cont_VV(nsync, wf8(:,14), wf8(:,30), A(:,63), n3(:,153), t3x64(:,:,63), nhel, den(55))
    call cont_VV(nsync, wf8(:,6), wf8(:,31), A(:,64), n3(:,154), t3x64(:,:,64), nhel, den(56))
    call cont_VV(nsync, wf8(:,10), wf8(:,32), A(:,65), n3(:,155), t3x64(:,:,65), nhel, den(58))
    call cont_VV(nsync, wf8(:,2), wf8(:,33), A(:,66), n3(:,156), t3x64(:,:,66), nhel, den(59))
    call cont_VV(nsync, wf8(:,10), wf8(:,34), A(:,67), n3(:,157), t3x64(:,:,67), nhel, den(61))
    call cont_VV(nsync, wf8(:,2), wf8(:,35), A(:,68), n3(:,158), t3x64(:,:,68), nhel, den(62))
    call cont_VV(nsync, wf8(:,6), wf8(:,36), A(:,69), n3(:,159), t3x64(:,:,69), nhel, den(64))
    call cont_VV(nsync, wf4(:,7), wf16(:,22), A(:,70), n3(:,160), t3x64(:,:,70), nhel, den(66))
    call cont_QA(nsync, wf8(:,38), wf8(:,39), A(:,71), n3(:,161), t3x64(:,:,71), nhel, den(69))
    call cont_QA(nsync, wf8(:,41), wf8(:,42), A(:,72), n3(:,162), t3x64(:,:,72), nhel, den(72))
    call cont_VV(nsync, wf4(:,6), wf16(:,23), A(:,73), n3(:,163), t3x64(:,:,73), nhel, den(74))
    call cont_QA(nsync, wf8(:,44), wf8(:,45), A(:,74), n3(:,164), t3x64(:,:,74), nhel, den(77))
    call cont_QA(nsync, wf8(:,41), wf8(:,47), A(:,75), n3(:,165), t3x64(:,:,75), nhel, den(79))
    call cont_VV(nsync, wf4(:,5), wf16(:,24), A(:,76), n3(:,166), t3x64(:,:,76), nhel, den(81))
    call cont_VV(nsync, wf8(:,17), wf8(:,29), A(:,77), n3(:,167), t3x64(:,:,77), nhel, den(83))
    call cont_QA(nsync, wf8(:,41), wf8(:,49), A(:,78), n3(:,168), t3x64(:,:,78), nhel, den(85))
    call cont_QA(nsync, wf8(:,45), wf8(:,50), A(:,79), n3(:,169), t3x64(:,:,79), nhel, den(86))
    call cont_QA(nsync, wf8(:,37), wf8(:,52), A(:,80), n3(:,170), t3x64(:,:,80), nhel, den(89))
    call cont_VV(nsync, wf8(:,17), wf8(:,33), A(:,81), n3(:,171), t3x64(:,:,81), nhel, den(90))
    call cont_QA(nsync, wf8(:,37), wf8(:,54), A(:,82), n3(:,172), t3x64(:,:,82), nhel, den(92))
    call cont_VV(nsync, wf8(:,17), wf8(:,35), A(:,83), n3(:,173), t3x64(:,:,83), nhel, den(93))
    call cont_QA(nsync, wf8(:,43), wf8(:,56), A(:,84), n3(:,174), t3x64(:,:,84), nhel, den(96))
    call cont_VV(nsync, wf4(:,7), wf16(:,25), A(:,85), n3(:,175), t3x64(:,:,85), nhel, den(98))
    call cont_QA(nsync, wf8(:,58), wf8(:,59), A(:,86), n3(:,176), t3x64(:,:,86), nhel, den(101))
    call cont_QA(nsync, wf8(:,61), wf8(:,62), A(:,87), n3(:,177), t3x64(:,:,87), nhel, den(104))
    call cont_VV(nsync, wf4(:,6), wf16(:,26), A(:,88), n3(:,178), t3x64(:,:,88), nhel, den(106))
    call cont_QA(nsync, wf8(:,58), wf8(:,64), A(:,89), n3(:,179), t3x64(:,:,89), nhel, den(108))
    call cont_QA(nsync, wf8(:,66), wf8(:,67), A(:,90), n3(:,180), t3x64(:,:,90), nhel, den(111))
    call cont_VV(nsync, wf4(:,5), wf16(:,27), A(:,91), n3(:,181), t3x64(:,:,91), nhel, den(113))
    call cont_VV(nsync, wf8(:,18), wf8(:,29), A(:,92), n3(:,182), t3x64(:,:,92), nhel, den(115))
    call cont_QA(nsync, wf8(:,58), wf8(:,69), A(:,93), n3(:,183), t3x64(:,:,93), nhel, den(117))
    call cont_QA(nsync, wf8(:,60), wf8(:,71), A(:,94), n3(:,184), t3x64(:,:,94), nhel, den(120))
    call cont_QA(nsync, wf8(:,67), wf8(:,72), A(:,95), n3(:,185), t3x64(:,:,95), nhel, den(121))
    call cont_VV(nsync, wf8(:,18), wf8(:,33), A(:,96), n3(:,186), t3x64(:,:,96), nhel, den(122))
    call cont_QA(nsync, wf8(:,60), wf8(:,74), A(:,97), n3(:,187), t3x64(:,:,97), nhel, den(124))
    call cont_VV(nsync, wf8(:,18), wf8(:,35), A(:,98), n3(:,188), t3x64(:,:,98), nhel, den(125))
    call cont_QA(nsync, wf8(:,65), wf8(:,76), A(:,99), n3(:,189), t3x64(:,:,99), nhel, den(128))
    call cont_VV(nsync, wf4(:,4), wf16(:,28), A(:,100), n3(:,190), t3x64(:,:,100), nhel, den(130))
    call cont_QA(nsync, wf8(:,44), wf8(:,78), A(:,101), n3(:,191), t3x64(:,:,101), nhel, den(132))
    call cont_QA(nsync, wf8(:,61), wf8(:,80), A(:,102), n3(:,192), t3x64(:,:,102), nhel, den(134))
    call cont_VV(nsync, wf4(:,3), wf16(:,29), A(:,103), n3(:,193), t3x64(:,:,103), nhel, den(136))
    call cont_VV(nsync, wf8(:,19), wf8(:,27), A(:,104), n3(:,194), t3x64(:,:,104), nhel, den(138))
    call cont_QA(nsync, wf8(:,61), wf8(:,82), A(:,105), n3(:,195), t3x64(:,:,105), nhel, den(140))
    call cont_QA(nsync, wf8(:,50), wf8(:,78), A(:,106), n3(:,196), t3x64(:,:,106), nhel, den(141))
    call cont_QA(nsync, wf8(:,57), wf8(:,84), A(:,107), n3(:,197), t3x64(:,:,107), nhel, den(144))
    call cont_VV(nsync, wf8(:,19), wf8(:,31), A(:,108), n3(:,198), t3x64(:,:,108), nhel, den(145))
    call cont_QA(nsync, wf8(:,57), wf8(:,86), A(:,109), n3(:,199), t3x64(:,:,109), nhel, den(147))
    call cont_VV(nsync, wf8(:,19), wf8(:,36), A(:,110), n3(:,200), t3x64(:,:,110), nhel, den(148))
    call cont_QA(nsync, wf8(:,56), wf8(:,77), A(:,111), n3(:,201), t3x64(:,:,111), nhel, den(149))
    call cont_VV(nsync, wf4(:,4), wf16(:,30), A(:,112), n3(:,202), t3x64(:,:,112), nhel, den(151))
    call cont_QA(nsync, wf8(:,38), wf8(:,88), A(:,113), n3(:,203), t3x64(:,:,113), nhel, den(153))
    call cont_QA(nsync, wf8(:,66), wf8(:,90), A(:,114), n3(:,204), t3x64(:,:,114), nhel, den(155))
    call cont_VV(nsync, wf4(:,3), wf16(:,31), A(:,115), n3(:,205), t3x64(:,:,115), nhel, den(157))
    call cont_VV(nsync, wf8(:,20), wf8(:,27), A(:,116), n3(:,206), t3x64(:,:,116), nhel, den(159))
    call cont_QA(nsync, wf8(:,38), wf8(:,92), A(:,117), n3(:,207), t3x64(:,:,117), nhel, den(161))
    call cont_QA(nsync, wf8(:,40), wf8(:,94), A(:,118), n3(:,208), t3x64(:,:,118), nhel, den(164))
    call cont_QA(nsync, wf8(:,72), wf8(:,90), A(:,119), n3(:,209), t3x64(:,:,119), nhel, den(165))
    call cont_VV(nsync, wf8(:,20), wf8(:,31), A(:,120), n3(:,210), t3x64(:,:,120), nhel, den(166))
    call cont_QA(nsync, wf8(:,40), wf8(:,96), A(:,121), n3(:,211), t3x64(:,:,121), nhel, den(168))
    call cont_VV(nsync, wf8(:,20), wf8(:,36), A(:,122), n3(:,212), t3x64(:,:,122), nhel, den(169))
    call cont_QA(nsync, wf8(:,76), wf8(:,89), A(:,123), n3(:,213), t3x64(:,:,123), nhel, den(170))
    call cont_VV(nsync, wf4(:,2), wf16(:,32), A(:,124), n3(:,214), t3x64(:,:,124), nhel, den(172))
    call cont_VV(nsync, wf8(:,21), wf8(:,25), A(:,125), n3(:,215), t3x64(:,:,125), nhel, den(174))
    call cont_QA(nsync, wf8(:,66), wf8(:,98), A(:,126), n3(:,216), t3x64(:,:,126), nhel, den(176))
    call cont_VV(nsync, wf4(:,2), wf16(:,33), A(:,127), n3(:,217), t3x64(:,:,127), nhel, den(178))
    call cont_VV(nsync, wf8(:,22), wf8(:,25), A(:,128), n3(:,218), t3x64(:,:,128), nhel, den(180))
    call cont_QA(nsync, wf8(:,44), wf8(:,100), A(:,129), n3(:,219), t3x64(:,:,129), nhel, den(182))
    call cont_VV(nsync, wf8(:,23), wf8(:,26), A(:,130), n3(:,220), t3x64(:,:,130), nhel, den(185))
    call cont_QA(nsync, wf8(:,72), wf8(:,98), A(:,131), n3(:,221), t3x64(:,:,131), nhel, den(186))
    call cont_VV(nsync, wf8(:,24), wf8(:,26), A(:,132), n3(:,222), t3x64(:,:,132), nhel, den(188))
    call cont_QA(nsync, wf8(:,50), wf8(:,100), A(:,133), n3(:,223), t3x64(:,:,133), nhel, den(189))
    call cont_QA(nsync, wf8(:,55), wf8(:,100), A(:,134), n3(:,224), t3x64(:,:,134), nhel, den(190))
    call cont_QA(nsync, wf8(:,76), wf8(:,97), A(:,135), n3(:,225), t3x64(:,:,135), nhel, den(191))
    call cont_QA(nsync, wf8(:,51), wf8(:,88), A(:,136), n3(:,226), t3x64(:,:,136), nhel, den(192))
    call cont_QA(nsync, wf8(:,63), wf8(:,84), A(:,137), n3(:,227), t3x64(:,:,137), nhel, den(193))
    call cont_VV(nsync, wf8(:,21), wf8(:,32), A(:,138), n3(:,228), t3x64(:,:,138), nhel, den(194))
    call cont_QA(nsync, wf8(:,63), wf8(:,86), A(:,139), n3(:,229), t3x64(:,:,139), nhel, den(195))
    call cont_VV(nsync, wf8(:,21), wf8(:,34), A(:,140), n3(:,230), t3x64(:,:,140), nhel, den(196))
    call cont_QA(nsync, wf8(:,54), wf8(:,87), A(:,141), n3(:,231), t3x64(:,:,141), nhel, den(197))
    call cont_QA(nsync, wf8(:,46), wf8(:,94), A(:,142), n3(:,232), t3x64(:,:,142), nhel, den(198))
    call cont_QA(nsync, wf8(:,70), wf8(:,80), A(:,143), n3(:,233), t3x64(:,:,143), nhel, den(199))
    call cont_VV(nsync, wf8(:,22), wf8(:,32), A(:,144), n3(:,234), t3x64(:,:,144), nhel, den(200))
    call cont_QA(nsync, wf8(:,46), wf8(:,96), A(:,145), n3(:,235), t3x64(:,:,145), nhel, den(201))
    call cont_VV(nsync, wf8(:,22), wf8(:,34), A(:,146), n3(:,236), t3x64(:,:,146), nhel, den(202))
    call cont_QA(nsync, wf8(:,74), wf8(:,79), A(:,147), n3(:,237), t3x64(:,:,147), nhel, den(203))
    call cont_VV(nsync, wf8(:,23), wf8(:,28), A(:,148), n3(:,238), t3x64(:,:,148), nhel, den(204))
    call cont_QA(nsync, wf8(:,70), wf8(:,82), A(:,149), n3(:,239), t3x64(:,:,149), nhel, den(205))
    call cont_VV(nsync, wf8(:,24), wf8(:,28), A(:,150), n3(:,240), t3x64(:,:,150), nhel, den(206))
    call cont_QA(nsync, wf8(:,51), wf8(:,92), A(:,151), n3(:,241), t3x64(:,:,151), nhel, den(207))
    call cont_QA(nsync, wf8(:,53), wf8(:,92), A(:,152), n3(:,242), t3x64(:,:,152), nhel, den(208))
    call cont_QA(nsync, wf8(:,74), wf8(:,81), A(:,153), n3(:,243), t3x64(:,:,153), nhel, den(209))
    call cont_VV(nsync, wf8(:,23), wf8(:,30), A(:,154), n3(:,244), t3x64(:,:,154), nhel, den(210))
    call cont_QA(nsync, wf8(:,49), wf8(:,93), A(:,155), n3(:,245), t3x64(:,:,155), nhel, den(211))
    call cont_VV(nsync, wf8(:,24), wf8(:,30), A(:,156), n3(:,246), t3x64(:,:,156), nhel, den(212))
    call cont_QA(nsync, wf8(:,69), wf8(:,83), A(:,157), n3(:,247), t3x64(:,:,157), nhel, den(213))
    call cont_QA(nsync, wf8(:,48), wf8(:,96), A(:,158), n3(:,248), t3x64(:,:,158), nhel, den(214))
    call cont_QA(nsync, wf8(:,69), wf8(:,85), A(:,159), n3(:,249), t3x64(:,:,159), nhel, den(215))

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
  complex(REALKIND), intent(out) :: M1(24) ! M1helarray(24,64)
  integer :: empty(0)

  M1( 1) = (A(j,34)%j-A(j,35)%j+A(j,49)%j-A(j,50)%j+A(j,92)%j+A(j,95)%j+A(j,98)%j+A(j,130)%j+A(j,135)%j+A(j,154)%j)*f(1) &
        +CI*(A(j,2)%j-A(j,3)%j+A(j,10)%j-A(j,11)%j-A(j,13)%j+A(j,15)%j+A(j,22)%j-A(j,23)%j-A(j,28)%j+A(j,29)%j+A(j,55)%j-A(j,57)%j &
        +A(j,62)%j-A(j,63)%j+A(j,68)%j+A(j,91)%j+A(j,99)%j+A(j,131)%j)*f(2)
  M1( 2) = (A(j,35)%j-A(j,36)%j+A(j,43)%j-A(j,44)%j+A(j,90)%j-A(j,96)%j-A(j,98)%j+A(j,125)%j-A(j,135)%j+A(j,140)%j)*f(1) &
        +CI*(A(j,1)%j-A(j,2)%j+A(j,11)%j-A(j,12)%j-A(j,16)%j+A(j,18)%j-A(j,22)%j+A(j,23)%j-A(j,25)%j+A(j,26)%j-A(j,55)%j-A(j,56)%j &
        -A(j,66)%j-A(j,67)%j-A(j,68)%j+A(j,88)%j-A(j,99)%j+A(j,126)%j)*f(2)
  M1( 3) = (-A(j,34)%j+A(j,36)%j+A(j,50)%j-A(j,51)%j-A(j,92)%j+A(j,94)%j+A(j,96)%j-A(j,148)%j+A(j,153)%j-A(j,154)%j)*f(1) &
        +CI*(A(j,5)%j-A(j,6)%j-A(j,10)%j+A(j,12)%j+A(j,13)%j-A(j,15)%j+A(j,16)%j-A(j,17)%j-A(j,29)%j+A(j,30)%j+A(j,58)%j+A(j,60)%j &
        -A(j,62)%j+A(j,63)%j+A(j,66)%j-A(j,91)%j+A(j,97)%j+A(j,149)%j)*f(2)
  M1( 4) = (A(j,35)%j-A(j,36)%j+A(j,37)%j-A(j,38)%j+A(j,87)%j-A(j,96)%j-A(j,98)%j+A(j,104)%j+A(j,110)%j-A(j,153)%j)*f(1) &
        +CI*(A(j,4)%j-A(j,5)%j+A(j,11)%j-A(j,12)%j-A(j,16)%j+A(j,17)%j-A(j,19)%j+A(j,20)%j-A(j,22)%j+A(j,24)%j-A(j,58)%j-A(j,59)%j &
        -A(j,66)%j-A(j,68)%j-A(j,69)%j+A(j,85)%j-A(j,97)%j+A(j,105)%j)*f(2)
  M1( 5) = (-A(j,34)%j+A(j,36)%j+A(j,44)%j-A(j,45)%j+A(j,89)%j-A(j,92)%j+A(j,96)%j-A(j,138)%j-A(j,140)%j+A(j,159)%j)*f(1) &
        +CI*(A(j,8)%j-A(j,9)%j-A(j,10)%j+A(j,12)%j+A(j,13)%j-A(j,14)%j+A(j,16)%j-A(j,18)%j-A(j,26)%j+A(j,27)%j+A(j,61)%j-A(j,62)%j &
        +A(j,65)%j+A(j,66)%j+A(j,67)%j-A(j,88)%j+A(j,93)%j+A(j,139)%j)*f(2)
  M1( 6) = (A(j,34)%j-A(j,35)%j+A(j,38)%j-A(j,39)%j+A(j,86)%j+A(j,92)%j+A(j,98)%j-A(j,108)%j-A(j,110)%j-A(j,159)%j)*f(1) &
        +CI*(A(j,7)%j-A(j,8)%j+A(j,10)%j-A(j,11)%j-A(j,13)%j+A(j,14)%j-A(j,20)%j+A(j,21)%j+A(j,22)%j-A(j,24)%j-A(j,61)%j+A(j,62)%j &
        +A(j,64)%j+A(j,68)%j+A(j,69)%j-A(j,85)%j-A(j,93)%j+A(j,109)%j)*f(2)
  M1( 7) = (A(j,40)%j-A(j,41)%j-A(j,49)%j+A(j,51)%j+A(j,116)%j+A(j,119)%j+A(j,122)%j-A(j,130)%j-A(j,135)%j+A(j,148)%j)*f(1)+CI*( &
        -A(j,2)%j+A(j,3)%j-A(j,4)%j+A(j,6)%j+A(j,19)%j-A(j,20)%j+A(j,23)%j-A(j,24)%j+A(j,28)%j-A(j,30)%j-A(j,55)%j+A(j,57)%j &
        +A(j,59)%j-A(j,60)%j+A(j,69)%j+A(j,115)%j+A(j,123)%j-A(j,131)%j)*f(2)
  M1( 8) = (A(j,41)%j-A(j,42)%j-A(j,43)%j+A(j,45)%j+A(j,114)%j-A(j,120)%j-A(j,122)%j-A(j,125)%j+A(j,135)%j+A(j,138)%j)*f(1)+CI*( &
        -A(j,1)%j+A(j,2)%j-A(j,7)%j+A(j,9)%j+A(j,20)%j-A(j,21)%j-A(j,23)%j+A(j,24)%j+A(j,25)%j-A(j,27)%j+A(j,55)%j+A(j,56)%j &
        -A(j,64)%j-A(j,65)%j-A(j,69)%j+A(j,112)%j-A(j,123)%j-A(j,126)%j)*f(2)
  M1( 9) = (-A(j,40)%j+A(j,42)%j+A(j,50)%j-A(j,51)%j-A(j,116)%j+A(j,118)%j+A(j,120)%j-A(j,148)%j-A(j,154)%j+A(j,158)%j)*f(1) &
        +CI*(A(j,4)%j-A(j,6)%j+A(j,7)%j-A(j,8)%j+A(j,14)%j-A(j,15)%j-A(j,19)%j+A(j,21)%j-A(j,29)%j+A(j,30)%j-A(j,59)%j+A(j,60)%j &
        -A(j,61)%j+A(j,63)%j+A(j,64)%j-A(j,115)%j+A(j,121)%j+A(j,155)%j)*f(2)
  M1(10) = (A(j,31)%j-A(j,32)%j+A(j,41)%j-A(j,42)%j+A(j,72)%j+A(j,77)%j+A(j,83)%j-A(j,120)%j-A(j,122)%j-A(j,158)%j)*f(1)+CI*( &
        -A(j,7)%j+A(j,8)%j-A(j,10)%j+A(j,11)%j+A(j,13)%j-A(j,14)%j+A(j,20)%j-A(j,21)%j-A(j,22)%j+A(j,24)%j+A(j,61)%j-A(j,62)%j &
        -A(j,64)%j-A(j,68)%j-A(j,69)%j+A(j,70)%j+A(j,78)%j-A(j,121)%j)*f(2)
  M1(11) = (-A(j,40)%j+A(j,42)%j+A(j,44)%j-A(j,45)%j+A(j,113)%j-A(j,116)%j+A(j,120)%j-A(j,138)%j-A(j,140)%j+A(j,152)%j)*f(1) &
        +CI*(A(j,4)%j-A(j,5)%j+A(j,7)%j-A(j,9)%j+A(j,17)%j-A(j,18)%j-A(j,19)%j+A(j,21)%j-A(j,26)%j+A(j,27)%j-A(j,58)%j-A(j,59)%j &
        +A(j,64)%j+A(j,65)%j+A(j,67)%j-A(j,112)%j+A(j,117)%j+A(j,141)%j)*f(2)
  M1(12) = (A(j,32)%j-A(j,33)%j+A(j,40)%j-A(j,41)%j+A(j,71)%j-A(j,81)%j-A(j,83)%j+A(j,116)%j+A(j,122)%j-A(j,152)%j)*f(1)+CI*( &
        -A(j,4)%j+A(j,5)%j-A(j,11)%j+A(j,12)%j+A(j,16)%j-A(j,17)%j+A(j,19)%j-A(j,20)%j+A(j,22)%j-A(j,24)%j+A(j,58)%j+A(j,59)%j &
        +A(j,66)%j+A(j,68)%j+A(j,69)%j-A(j,70)%j+A(j,82)%j-A(j,117)%j)*f(2)
  M1(13) = (A(j,46)%j-A(j,47)%j-A(j,49)%j+A(j,51)%j+A(j,128)%j-A(j,130)%j+A(j,143)%j+A(j,146)%j+A(j,148)%j-A(j,153)%j)*f(1)+CI*( &
        -A(j,1)%j+A(j,3)%j-A(j,5)%j+A(j,6)%j+A(j,17)%j-A(j,18)%j+A(j,25)%j-A(j,26)%j+A(j,28)%j-A(j,30)%j+A(j,56)%j+A(j,57)%j &
        -A(j,58)%j-A(j,60)%j+A(j,67)%j+A(j,127)%j+A(j,147)%j-A(j,149)%j)*f(2)
  M1(14) = (-A(j,37)%j+A(j,39)%j+A(j,47)%j-A(j,48)%j+A(j,102)%j-A(j,104)%j+A(j,108)%j-A(j,144)%j-A(j,146)%j+A(j,153)%j)*f(1)+CI*( &
        -A(j,4)%j+A(j,5)%j-A(j,7)%j+A(j,9)%j-A(j,17)%j+A(j,18)%j+A(j,19)%j-A(j,21)%j+A(j,26)%j-A(j,27)%j+A(j,58)%j+A(j,59)%j &
        -A(j,64)%j-A(j,65)%j-A(j,67)%j+A(j,100)%j-A(j,105)%j-A(j,147)%j)*f(2)
  M1(15) = (-A(j,46)%j+A(j,48)%j+A(j,49)%j-A(j,50)%j-A(j,128)%j+A(j,130)%j+A(j,142)%j+A(j,144)%j+A(j,154)%j-A(j,158)%j)*f(1) &
        +CI*(A(j,1)%j-A(j,3)%j+A(j,8)%j-A(j,9)%j-A(j,14)%j+A(j,15)%j-A(j,25)%j+A(j,27)%j-A(j,28)%j+A(j,29)%j-A(j,56)%j-A(j,57)%j &
        +A(j,61)%j-A(j,63)%j+A(j,65)%j-A(j,127)%j+A(j,145)%j-A(j,155)%j)*f(2)
  M1(16) = (-A(j,31)%j+A(j,33)%j+A(j,47)%j-A(j,48)%j+A(j,75)%j-A(j,77)%j+A(j,81)%j-A(j,144)%j-A(j,146)%j+A(j,158)%j)*f(1)+CI*( &
        -A(j,8)%j+A(j,9)%j+A(j,10)%j-A(j,12)%j-A(j,13)%j+A(j,14)%j-A(j,16)%j+A(j,18)%j+A(j,26)%j-A(j,27)%j-A(j,61)%j+A(j,62)%j &
        -A(j,65)%j-A(j,66)%j-A(j,67)%j+A(j,73)%j-A(j,78)%j-A(j,145)%j)*f(2)
  M1(17) = (A(j,38)%j-A(j,39)%j-A(j,46)%j+A(j,48)%j+A(j,101)%j-A(j,108)%j-A(j,110)%j-A(j,128)%j+A(j,134)%j+A(j,144)%j)*f(1) &
        +CI*(A(j,1)%j-A(j,2)%j+A(j,7)%j-A(j,9)%j-A(j,20)%j+A(j,21)%j+A(j,23)%j-A(j,24)%j-A(j,25)%j+A(j,27)%j-A(j,55)%j-A(j,56)%j &
        +A(j,64)%j+A(j,65)%j+A(j,69)%j-A(j,100)%j+A(j,111)%j+A(j,129)%j)*f(2)
  M1(18) = (A(j,32)%j-A(j,33)%j+A(j,46)%j-A(j,47)%j+A(j,74)%j-A(j,81)%j-A(j,83)%j+A(j,128)%j-A(j,134)%j+A(j,146)%j)*f(1)+CI*( &
        -A(j,1)%j+A(j,2)%j-A(j,11)%j+A(j,12)%j+A(j,16)%j-A(j,18)%j+A(j,22)%j-A(j,23)%j+A(j,25)%j-A(j,26)%j+A(j,55)%j+A(j,56)%j &
        +A(j,66)%j+A(j,67)%j+A(j,68)%j-A(j,73)%j+A(j,84)%j-A(j,129)%j)*f(2)
  M1(19) = (-A(j,43)%j+A(j,45)%j+A(j,52)%j-A(j,53)%j-A(j,125)%j+A(j,132)%j+A(j,137)%j+A(j,138)%j+A(j,156)%j-A(j,159)%j)*f(1)+CI*( &
        -A(j,1)%j+A(j,3)%j-A(j,8)%j+A(j,9)%j+A(j,14)%j-A(j,15)%j+A(j,25)%j-A(j,27)%j+A(j,28)%j-A(j,29)%j+A(j,56)%j+A(j,57)%j &
        -A(j,61)%j+A(j,63)%j-A(j,65)%j+A(j,124)%j-A(j,139)%j+A(j,157)%j)*f(2)
  M1(20) = (-A(j,37)%j+A(j,39)%j+A(j,53)%j-A(j,54)%j-A(j,104)%j+A(j,107)%j+A(j,108)%j-A(j,150)%j-A(j,156)%j+A(j,159)%j)*f(1)+CI*( &
        -A(j,4)%j+A(j,6)%j-A(j,7)%j+A(j,8)%j-A(j,14)%j+A(j,15)%j+A(j,19)%j-A(j,21)%j+A(j,29)%j-A(j,30)%j+A(j,59)%j-A(j,60)%j &
        +A(j,61)%j-A(j,63)%j-A(j,64)%j+A(j,103)%j-A(j,109)%j-A(j,157)%j)*f(2)
  M1(21) = (A(j,43)%j-A(j,44)%j-A(j,52)%j+A(j,54)%j+A(j,125)%j-A(j,132)%j+A(j,136)%j+A(j,140)%j+A(j,150)%j-A(j,152)%j)*f(1) &
        +CI*(A(j,1)%j-A(j,3)%j+A(j,5)%j-A(j,6)%j-A(j,17)%j+A(j,18)%j-A(j,25)%j+A(j,26)%j-A(j,28)%j+A(j,30)%j-A(j,56)%j-A(j,57)%j &
        +A(j,58)%j+A(j,60)%j-A(j,67)%j-A(j,124)%j-A(j,141)%j+A(j,151)%j)*f(2)
  M1(22) = (-A(j,31)%j+A(j,33)%j+A(j,53)%j-A(j,54)%j-A(j,77)%j+A(j,80)%j+A(j,81)%j-A(j,150)%j+A(j,152)%j-A(j,156)%j)*f(1)+CI*( &
        -A(j,5)%j+A(j,6)%j+A(j,10)%j-A(j,12)%j-A(j,13)%j+A(j,15)%j-A(j,16)%j+A(j,17)%j+A(j,29)%j-A(j,30)%j-A(j,58)%j-A(j,60)%j &
        +A(j,62)%j-A(j,63)%j-A(j,66)%j+A(j,76)%j-A(j,82)%j-A(j,151)%j)*f(2)
  M1(23) = (A(j,37)%j-A(j,38)%j-A(j,52)%j+A(j,54)%j+A(j,104)%j+A(j,106)%j+A(j,110)%j-A(j,132)%j-A(j,134)%j+A(j,150)%j)*f(1) &
        +CI*(A(j,2)%j-A(j,3)%j+A(j,4)%j-A(j,6)%j-A(j,19)%j+A(j,20)%j-A(j,23)%j+A(j,24)%j-A(j,28)%j+A(j,30)%j+A(j,55)%j-A(j,57)%j &
        -A(j,59)%j+A(j,60)%j-A(j,69)%j-A(j,103)%j-A(j,111)%j+A(j,133)%j)*f(2)
  M1(24) = (A(j,31)%j-A(j,32)%j+A(j,52)%j-A(j,53)%j+A(j,77)%j+A(j,79)%j+A(j,83)%j+A(j,132)%j+A(j,134)%j+A(j,156)%j)*f(1)+CI*( &
        -A(j,2)%j+A(j,3)%j-A(j,10)%j+A(j,11)%j+A(j,13)%j-A(j,15)%j-A(j,22)%j+A(j,23)%j+A(j,28)%j-A(j,29)%j-A(j,55)%j+A(j,57)%j &
        -A(j,62)%j+A(j,63)%j-A(j,68)%j-A(j,76)%j-A(j,84)%j-A(j,133)%j)*f(2)

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
  use ol_colourmatrix_ppjjjj_ddxgggg_1_/**/REALKIND, only: K1
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
subroutine colintmunu(M1, M2, M2colint)
! M(i)   = <M|Ci> colour component of matrix element
! COLINT = <M|M>
!        = Sum_{i,j} <M|Ci> * <Ci|Cj> * <Cj|M>
!        = colour-summed squared matrix element
! K1(i,j) = <Ci|Cj>
! M2colint is an array which contains the colour interference for each colour matrix
! **********************************************************************
  use ol_colourmatrix_ppjjjj_ddxgggg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(24)
  complex(REALKIND), intent(in)  :: M2(24)
  real(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 24
    do j = 1, 24
      M2colint = M2colint + real(conjg(M1(i))*K1(i,j)*M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_ppjjjj_ddxgggg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(24,64)
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
    & bind(c,name="ol_f_amp2tree_ppjjjj_ddxgggg_1")
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
    & bind(c,name="ol_f_amp2ccone_ppjjjj_ddxgggg_1")
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
    & bind(c,name="ol_f_amp2ccall_ppjjjj_ddxgggg_1")
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
    & bind(c,name="ol_f_amp2hcone_ppjjjj_ddxgggg_1")
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
    & bind(c,name="ol_f_amp2hcall_ppjjjj_ddxgggg_1")
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
    & bind(c,name="ol_amp2tree_ppjjjj_ddxgggg_1")
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
    & bind(c,name="ol_amp2ccone_ppjjjj_ddxgggg_1")
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
    & bind(c,name="ol_amp2ccall_ppjjjj_ddxgggg_1")
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
    & bind(c,name="ol_amp2hcone_ppjjjj_ddxgggg_1")
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
    & bind(c,name="ol_amp2hcall_ppjjjj_ddxgggg_1")
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
    & bind(c,name="amp2tree_ppjjjj_ddxgggg_1_")
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
    & bind(c,name="amp2ccone_ppjjjj_ddxgggg_1_")
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
    & bind(c,name="amp2ccall_ppjjjj_ddxgggg_1_")
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
    & bind(c,name="amp2hcone_ppjjjj_ddxgggg_1_")
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
    & bind(c,name="amp2hcall_ppjjjj_ddxgggg_1_")
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

end module ol_tree_ppjjjj_ddxgggg_1_/**/REALKIND
