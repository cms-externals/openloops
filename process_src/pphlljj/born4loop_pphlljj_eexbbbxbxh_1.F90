
module ol_tree_pphlljj_eexbbbxbxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(3)
  complex(REALKIND), save :: den(152)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 64 ! number of helicity configurations
  integer(intkind2), save :: nhel = 64 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(64) ! physical helicity states
  complex(DREALKIND) :: M1helarr(2,64) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED**3*gQCD**2*lambdaHZZ*MW)/(cw**2*sw)
    f(2) = (CI*eQED**3*gQCD**2*YB)/(6._/**/REALKIND*MW*sw)
    f(3) = (CI*eQED**3*gQCD**2*YB)/(2._/**/REALKIND*MW*sw)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,3))
  den(2) = 1 / (Q(5,20))
  den(3) = 1 / (Q(5,72) - MB2)
  den(5) = 1 / (Q(5,35) - MB2)
  den(8) = 1 / (Q(5,3) - MZ2)
  den(12) = 1 / (Q(5,52) - MB2)
  den(17) = 1 / (Q(5,96) - MB2)
  den(19) = 1 / (Q(5,11) - MB2)
  den(25) = 1 / (Q(5,28) - MB2)
  den(34) = 1 / (Q(5,67) - MZ2)
  den(38) = 1 / (Q(5,68) - MB2)
  den(39) = 1 / (Q(5,24))
  den(44) = 1 / (Q(5,56) - MB2)
  den(50) = 1 / (Q(5,7) - MB2)
  den(64) = 1 / (Q(5,36))
  den(66) = 1 / (Q(5,19) - MB2)
  den(74) = 1 / (Q(5,80) - MB2)
  den(79) = 1 / (Q(5,44) - MB2)
  den(90) = 1 / (Q(5,40))
  den(109) = 1 / (Q(5,84))
  den(113) = 1 / (Q(5,100))
  den(121) = 1 / (Q(5,104))
  den(125) = 1 / (Q(5,88))

  ! denominators

  den(4) = den(2)*den(3)
  den(6) = den(1)*den(5)
  den(7) = den(4)*den(6)
  den(9) = den(5)*den(8)
  den(10) = den(4)*den(9)
  den(11) = den(1)*den(3)
  den(13) = den(2)*den(12)
  den(14) = den(11)*den(13)
  den(15) = den(3)*den(8)
  den(16) = den(13)*den(15)
  den(18) = den(2)*den(17)
  den(20) = den(1)*den(19)
  den(21) = den(18)*den(20)
  den(22) = den(8)*den(19)
  den(23) = den(18)*den(22)
  den(24) = den(1)*den(17)
  den(26) = den(2)*den(25)
  den(27) = den(24)*den(26)
  den(28) = den(8)*den(17)
  den(29) = den(26)*den(28)
  den(30) = den(13)*den(20)
  den(31) = den(13)*den(22)
  den(32) = den(6)*den(26)
  den(33) = den(9)*den(26)
  den(35) = den(8)*den(34)
  den(36) = den(26)*den(35)
  den(37) = den(13)*den(35)
  den(40) = den(38)*den(39)
  den(41) = den(6)*den(40)
  den(42) = den(9)*den(40)
  den(43) = den(1)*den(38)
  den(45) = den(39)*den(44)
  den(46) = den(43)*den(45)
  den(47) = den(8)*den(38)
  den(48) = den(45)*den(47)
  den(49) = den(17)*den(39)
  den(51) = den(1)*den(50)
  den(52) = den(49)*den(51)
  den(53) = den(8)*den(50)
  den(54) = den(49)*den(53)
  den(55) = den(25)*den(39)
  den(56) = den(24)*den(55)
  den(57) = den(28)*den(55)
  den(58) = den(45)*den(51)
  den(59) = den(45)*den(53)
  den(60) = den(6)*den(55)
  den(61) = den(9)*den(55)
  den(62) = den(35)*den(55)
  den(63) = den(35)*den(45)
  den(65) = den(3)*den(64)
  den(67) = den(1)*den(66)
  den(68) = den(65)*den(67)
  den(69) = den(8)*den(66)
  den(70) = den(65)*den(69)
  den(71) = den(12)*den(64)
  den(72) = den(11)*den(71)
  den(73) = den(15)*den(71)
  den(75) = den(64)*den(74)
  den(76) = den(20)*den(75)
  den(77) = den(22)*den(75)
  den(78) = den(1)*den(74)
  den(80) = den(64)*den(79)
  den(81) = den(78)*den(80)
  den(82) = den(8)*den(74)
  den(83) = den(80)*den(82)
  den(84) = den(20)*den(71)
  den(85) = den(22)*den(71)
  den(86) = den(67)*den(80)
  den(87) = den(69)*den(80)
  den(88) = den(35)*den(80)
  den(89) = den(35)*den(71)
  den(91) = den(38)*den(90)
  den(92) = den(67)*den(91)
  den(93) = den(69)*den(91)
  den(94) = den(44)*den(90)
  den(95) = den(43)*den(94)
  den(96) = den(47)*den(94)
  den(97) = den(74)*den(90)
  den(98) = den(51)*den(97)
  den(99) = den(53)*den(97)
  den(100) = den(79)*den(90)
  den(101) = den(78)*den(100)
  den(102) = den(82)*den(100)
  den(103) = den(51)*den(94)
  den(104) = den(53)*den(94)
  den(105) = den(67)*den(100)
  den(106) = den(69)*den(100)
  den(107) = den(35)*den(100)
  den(108) = den(35)*den(94)
  den(110) = den(38)*den(109)
  den(111) = den(20)*den(110)
  den(112) = den(22)*den(110)
  den(114) = den(38)*den(113)
  den(115) = den(20)*den(114)
  den(116) = den(22)*den(114)
  den(117) = den(67)*den(114)
  den(118) = den(69)*den(114)
  den(119) = den(6)*den(110)
  den(120) = den(9)*den(110)
  den(122) = den(3)*den(121)
  den(123) = den(51)*den(122)
  den(124) = den(53)*den(122)
  den(126) = den(3)*den(125)
  den(127) = den(51)*den(126)
  den(128) = den(53)*den(126)
  den(129) = den(67)*den(122)
  den(130) = den(69)*den(122)
  den(131) = den(6)*den(126)
  den(132) = den(9)*den(126)
  den(133) = den(74)*den(125)
  den(134) = den(51)*den(133)
  den(135) = den(53)*den(133)
  den(136) = den(74)*den(109)
  den(137) = den(20)*den(136)
  den(138) = den(22)*den(136)
  den(139) = den(6)*den(136)
  den(140) = den(9)*den(136)
  den(141) = den(6)*den(133)
  den(142) = den(9)*den(133)
  den(143) = den(17)*den(121)
  den(144) = den(51)*den(143)
  den(145) = den(53)*den(143)
  den(146) = den(17)*den(113)
  den(147) = den(20)*den(146)
  den(148) = den(22)*den(146)
  den(149) = den(67)*den(143)
  den(150) = den(69)*den(143)
  den(151) = den(67)*den(146)
  den(152) = den(69)*den(146)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_pphlljj_eexbbbxbxh_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_pphlljj_eexbbbxbxh_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for e- e+ bottom bottom anti-bottom anti-bottom higgs -> 0
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
  use ol_external_pphlljj_eexbbbxbxh_1, only: external_perm_pphlljj_eexbbbxbxh_1, &
    & external_perm_inv_pphlljj_eexbbbxbxh_1, extcomb_perm_pphlljj_eexbbbxbxh_1, &
    & average_factor_pphlljj_eexbbbxbxh_1
  use ol_external_pphlljj_eexbbbxbxh_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_pphlljj_eexbbbxbxh_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_pphlljj_eexbbbxbxh_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_pphlljj_eexbbbxbxh_1
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
  complex(REALKIND) :: MOM_LC(4), M1(2), M1helarray(2,64)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(2), ex7(1)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf2(2,8), wf4(4,17), wf8(8,64), wf16(16,20), wf64(64,88)

  type(polcont) :: A(64,88)
  complex(REALKIND) :: Aj(88)

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
  extmasses2 = [ rZERO2, rZERO2, rMB2, rMB2, rMB2, rMB2, rMH2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_pphlljj_eexbbbxbxh_1,7)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,7)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_pphlljj_eexbbbxbxh_1(I)
  else if (I < 0) then
    JBmunu = external_perm_pphlljj_eexbbbxbxh_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_pphlljj_eexbbbxbxh_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rZERO, H1, ex1)
  call wf_A(P(:,2), rZERO, H2, ex2)
  call wf_Q(P(:,3), rMB, H3, ex3)
  call wf_Q(P(:,4), rMB, H4, ex4)
  call wf_A(P(:,5), rMB, H5, ex5)
  call wf_A(P(:,6), rMB, H6, ex6)
  call wf_S(P(:,7), rMH, H7, ex7)


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
  call vert_QA_V(ntry, ex3, ex5, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_QS_A(gH,ntry, ex4, ex7, wf2(:,1), n3(:,3), t3x2(:,:,1))
  call prop_Q_A(ntry, wf2(:,1), Q(:,72), MB, 1_intkind1, wf2(:,2), n2(1))
  call vert_AV_Q(ntry, ex6, wf4(:,1), wf8(:,1), n3(:,4), t3x8(:,:,1))
  call vert_VQ_A(ntry, wf4(:,2), wf2(:,2), wf8(:,2), n3(:,5), t3x8(:,:,2))
  call prop_A_Q(ntry, wf8(:,1), Q(:,35), MB, 1_intkind1, wf8(:,3), n2(2))
  call vert_QA_Z(gZl,ntry, ex1, ex2, wf4(:,3), n3(:,6), t3x4(:,:,3))
  call prop_W_W(ntry, wf4(:,3), Q(:,3), MZ, 1_intkind1, wf4(:,4), n2(3))
  call vert_AZ_Q(gZd,ntry, ex6, wf4(:,4), wf8(:,4), n3(:,7), t3x8(:,:,3))
  call prop_A_Q(ntry, wf8(:,4), Q(:,35), MB, 1_intkind1, wf8(:,5), n2(4))
  call vert_AV_Q(ntry, ex6, wf4(:,2), wf8(:,6), n3(:,8), t3x8(:,:,4))
  call vert_VQ_A(ntry, wf4(:,1), wf2(:,2), wf8(:,7), n3(:,9), t3x8(:,:,5))
  call prop_A_Q(ntry, wf8(:,6), Q(:,52), MB, 1_intkind1, wf8(:,8), n2(5))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), wf2(:,2), wf8(:,9), n3(:,10), t3x8(:,:,6))
  call vert_SA_Q(gH,ntry, ex7, ex6, wf2(:,3), n3(:,11), t3x2(:,:,2))
  call prop_A_Q(ntry, wf2(:,3), Q(:,96), MB, 1_intkind1, wf2(:,4), n2(6))
  call vert_VQ_A(ntry, wf4(:,1), ex4, wf8(:,10), n3(:,12), t3x8(:,:,7))
  call vert_AV_Q(ntry, wf2(:,4), wf4(:,2), wf8(:,11), n3(:,13), t3x8(:,:,8))
  call prop_Q_A(ntry, wf8(:,10), Q(:,11), MB, 1_intkind1, wf8(:,12), n2(7))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), ex4, wf8(:,13), n3(:,14), t3x8(:,:,9))
  call prop_Q_A(ntry, wf8(:,13), Q(:,11), MB, 1_intkind1, wf8(:,14), n2(8))
  call vert_VQ_A(ntry, wf4(:,2), ex4, wf8(:,15), n3(:,15), t3x8(:,:,10))
  call vert_AV_Q(ntry, wf2(:,4), wf4(:,1), wf8(:,16), n3(:,16), t3x8(:,:,11))
  call prop_Q_A(ntry, wf8(:,15), Q(:,28), MB, 1_intkind1, wf8(:,17), n2(9))
  call vert_AZ_Q(gZd,ntry, wf2(:,4), wf4(:,4), wf8(:,18), n3(:,17), t3x8(:,:,12))
  call vert_QS_A(gH,ntry, wf8(:,12), ex7, wf8(:,19), n3(:,18), t3x8(:,:,13))
  call vert_QS_A(gH,ntry, wf8(:,14), ex7, wf8(:,20), n3(:,19), t3x8(:,:,14))
  call vert_QS_A(gH,ntry, wf8(:,17), ex7, wf8(:,21), n3(:,20), t3x8(:,:,15))
  call vert_SV_V(ntry, ex7, wf4(:,4), wf4(:,5), n3(:,21), t3x4(:,:,4))
  call prop_W_W(ntry, wf4(:,5), Q(:,67), MZ, 1_intkind1, wf4(:,6), n2(10))
  call vert_QA_Z(gZd,ntry, wf8(:,17), ex6, wf16(:,1), n3(:,22), t3x16(:,:,1))
  call vert_QA_Z(gZd,ntry, ex4, wf8(:,8), wf16(:,2), n3(:,23), t3x16(:,:,2))
  call vert_QS_A(gH,ntry, ex3, ex7, wf2(:,5), n3(:,24), t3x2(:,:,3))
  call vert_QA_V(ntry, ex4, ex5, wf4(:,7), n3(:,25), t3x4(:,:,5))
  call prop_Q_A(ntry, wf2(:,5), Q(:,68), MB, 1_intkind1, wf2(:,6), n2(11))
  call vert_VQ_A(ntry, wf4(:,7), wf2(:,6), wf8(:,22), n3(:,26), t3x8(:,:,16))
  call vert_AV_Q(ntry, ex6, wf4(:,7), wf8(:,23), n3(:,27), t3x8(:,:,17))
  call vert_VQ_A(ntry, wf4(:,1), wf2(:,6), wf8(:,24), n3(:,28), t3x8(:,:,18))
  call prop_A_Q(ntry, wf8(:,23), Q(:,56), MB, 1_intkind1, wf8(:,25), n2(12))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), wf2(:,6), wf8(:,26), n3(:,29), t3x8(:,:,19))
  call vert_VQ_A(ntry, wf4(:,1), ex3, wf8(:,27), n3(:,30), t3x8(:,:,20))
  call vert_AV_Q(ntry, wf2(:,4), wf4(:,7), wf8(:,28), n3(:,31), t3x8(:,:,21))
  call prop_Q_A(ntry, wf8(:,27), Q(:,7), MB, 1_intkind1, wf8(:,29), n2(13))
  call vert_ZQ_A(gZd,ntry, wf4(:,4), ex3, wf8(:,30), n3(:,32), t3x8(:,:,22))
  call prop_Q_A(ntry, wf8(:,30), Q(:,7), MB, 1_intkind1, wf8(:,31), n2(14))
  call vert_VQ_A(ntry, wf4(:,7), ex3, wf8(:,32), n3(:,33), t3x8(:,:,23))
  call prop_Q_A(ntry, wf8(:,32), Q(:,28), MB, 1_intkind1, wf8(:,33), n2(15))
  call vert_QS_A(gH,ntry, wf8(:,29), ex7, wf8(:,34), n3(:,34), t3x8(:,:,24))
  call vert_QS_A(gH,ntry, wf8(:,31), ex7, wf8(:,35), n3(:,35), t3x8(:,:,25))
  call vert_QS_A(gH,ntry, wf8(:,33), ex7, wf8(:,36), n3(:,36), t3x8(:,:,26))
  call vert_QA_Z(gZd,ntry, wf8(:,33), ex6, wf16(:,3), n3(:,37), t3x16(:,:,3))
  call vert_QA_Z(gZd,ntry, ex3, wf8(:,25), wf16(:,4), n3(:,38), t3x16(:,:,4))
  call vert_QA_V(ntry, ex3, ex6, wf4(:,8), n3(:,39), t3x4(:,:,6))
  call vert_AV_Q(ntry, ex5, wf4(:,1), wf8(:,37), n3(:,40), t3x8(:,:,27))
  call vert_VQ_A(ntry, wf4(:,8), wf2(:,2), wf8(:,38), n3(:,41), t3x8(:,:,28))
  call prop_A_Q(ntry, wf8(:,37), Q(:,19), MB, 1_intkind1, wf8(:,39), n2(16))
  call vert_AZ_Q(gZd,ntry, ex5, wf4(:,4), wf8(:,40), n3(:,42), t3x8(:,:,29))
  call prop_A_Q(ntry, wf8(:,40), Q(:,19), MB, 1_intkind1, wf8(:,41), n2(17))
  call vert_AV_Q(ntry, ex5, wf4(:,8), wf8(:,42), n3(:,43), t3x8(:,:,30))
  call prop_A_Q(ntry, wf8(:,42), Q(:,52), MB, 1_intkind1, wf8(:,43), n2(18))
  call vert_SA_Q(gH,ntry, ex7, ex5, wf2(:,7), n3(:,44), t3x2(:,:,4))
  call prop_A_Q(ntry, wf2(:,7), Q(:,80), MB, 1_intkind1, wf2(:,8), n2(19))
  call vert_AV_Q(ntry, wf2(:,8), wf4(:,8), wf8(:,44), n3(:,45), t3x8(:,:,31))
  call vert_VQ_A(ntry, wf4(:,8), ex4, wf8(:,45), n3(:,46), t3x8(:,:,32))
  call vert_AV_Q(ntry, wf2(:,8), wf4(:,1), wf8(:,46), n3(:,47), t3x8(:,:,33))
  call prop_Q_A(ntry, wf8(:,45), Q(:,44), MB, 1_intkind1, wf8(:,47), n2(20))
  call vert_AZ_Q(gZd,ntry, wf2(:,8), wf4(:,4), wf8(:,48), n3(:,48), t3x8(:,:,34))
  call vert_QS_A(gH,ntry, wf8(:,47), ex7, wf8(:,49), n3(:,49), t3x8(:,:,35))
  call vert_QA_Z(gZd,ntry, wf8(:,47), ex5, wf16(:,5), n3(:,50), t3x16(:,:,5))
  call vert_QA_Z(gZd,ntry, ex4, wf8(:,43), wf16(:,6), n3(:,51), t3x16(:,:,6))
  call vert_QA_V(ntry, ex4, ex6, wf4(:,9), n3(:,52), t3x4(:,:,7))
  call vert_VQ_A(ntry, wf4(:,9), wf2(:,6), wf8(:,50), n3(:,53), t3x8(:,:,36))
  call vert_AV_Q(ntry, ex5, wf4(:,9), wf8(:,51), n3(:,54), t3x8(:,:,37))
  call prop_A_Q(ntry, wf8(:,51), Q(:,56), MB, 1_intkind1, wf8(:,52), n2(21))
  call vert_AV_Q(ntry, wf2(:,8), wf4(:,9), wf8(:,53), n3(:,55), t3x8(:,:,38))
  call vert_VQ_A(ntry, wf4(:,9), ex3, wf8(:,54), n3(:,56), t3x8(:,:,39))
  call prop_Q_A(ntry, wf8(:,54), Q(:,44), MB, 1_intkind1, wf8(:,55), n2(22))
  call vert_QS_A(gH,ntry, wf8(:,55), ex7, wf8(:,56), n3(:,57), t3x8(:,:,40))
  call vert_QA_Z(gZd,ntry, wf8(:,55), ex5, wf16(:,7), n3(:,58), t3x16(:,:,7))
  call vert_QA_Z(gZd,ntry, ex3, wf8(:,52), wf16(:,8), n3(:,59), t3x16(:,:,8))
  call vert_QA_V(ntry, wf2(:,6), ex5, wf4(:,10), n3(:,60), t3x4(:,:,8))
  call vert_QA_V(ntry, wf8(:,12), ex6, wf16(:,9), n3(:,61), t3x16(:,:,9))
  call vert_QA_V(ntry, wf8(:,14), ex6, wf16(:,10), n3(:,62), t3x16(:,:,10))
  call vert_QA_V(ntry, wf2(:,6), ex6, wf4(:,11), n3(:,63), t3x4(:,:,9))
  call vert_QA_V(ntry, wf8(:,12), ex5, wf16(:,11), n3(:,64), t3x16(:,:,11))
  call vert_QA_V(ntry, wf8(:,14), ex5, wf16(:,12), n3(:,65), t3x16(:,:,12))
  call vert_QA_V(ntry, ex4, wf8(:,39), wf16(:,13), n3(:,66), t3x16(:,:,13))
  call vert_QA_V(ntry, ex4, wf8(:,41), wf16(:,14), n3(:,67), t3x16(:,:,14))
  call vert_VQ_A(ntry, wf4(:,10), ex4, wf8(:,57), n3(:,68), t3x8(:,:,41))
  call vert_QA_V(ntry, wf2(:,2), ex6, wf4(:,12), n3(:,69), t3x4(:,:,10))
  call vert_QA_V(ntry, wf8(:,29), ex5, wf16(:,15), n3(:,70), t3x16(:,:,15))
  call vert_QA_V(ntry, wf8(:,31), ex5, wf16(:,16), n3(:,71), t3x16(:,:,16))
  call vert_QA_V(ntry, wf2(:,2), ex5, wf4(:,13), n3(:,72), t3x4(:,:,11))
  call vert_QA_V(ntry, wf8(:,29), ex6, wf16(:,17), n3(:,73), t3x16(:,:,17))
  call vert_QA_V(ntry, wf8(:,31), ex6, wf16(:,18), n3(:,74), t3x16(:,:,18))
  call vert_QA_V(ntry, ex3, wf8(:,39), wf16(:,19), n3(:,75), t3x16(:,:,19))
  call vert_QA_V(ntry, ex3, wf8(:,41), wf16(:,20), n3(:,76), t3x16(:,:,20))
  call vert_VQ_A(ntry, wf4(:,13), ex3, wf8(:,58), n3(:,77), t3x8(:,:,42))
  call vert_QA_V(ntry, ex4, wf2(:,8), wf4(:,14), n3(:,78), t3x4(:,:,12))
  call vert_QA_V(ntry, ex3, wf2(:,8), wf4(:,15), n3(:,79), t3x4(:,:,13))
  call vert_AV_Q(ntry, ex6, wf4(:,15), wf8(:,59), n3(:,80), t3x8(:,:,43))
  call vert_VQ_A(ntry, wf4(:,15), ex4, wf8(:,60), n3(:,81), t3x8(:,:,44))
  call vert_VQ_A(ntry, wf4(:,14), ex3, wf8(:,61), n3(:,82), t3x8(:,:,45))
  call vert_QA_V(ntry, ex4, wf2(:,4), wf4(:,16), n3(:,83), t3x4(:,:,14))
  call vert_QA_V(ntry, ex3, wf2(:,4), wf4(:,17), n3(:,84), t3x4(:,:,15))
  call vert_AV_Q(ntry, ex5, wf4(:,17), wf8(:,62), n3(:,85), t3x8(:,:,46))
  call vert_VQ_A(ntry, wf4(:,16), ex3, wf8(:,63), n3(:,86), t3x8(:,:,47))
  call vert_VQ_A(ntry, wf4(:,17), ex4, wf8(:,64), n3(:,87), t3x8(:,:,48))


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
  M2add = M2 / average_factor_pphlljj_eexbbbxbxh_1

  do k = 0, 30-1
    M2(k) = M2add(extcomb_perm_pphlljj_eexbbbxbxh_1(k))
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

    call cont_QA(nsync, wf8(:,2), wf8(:,3), A(:,1), n3(:,88), t3x64(:,:,1), nhel, den(7))
    call cont_QA(nsync, wf8(:,2), wf8(:,5), A(:,2), n3(:,89), t3x64(:,:,2), nhel, den(10))
    call cont_QA(nsync, wf8(:,7), wf8(:,8), A(:,3), n3(:,90), t3x64(:,:,3), nhel, den(14))
    call cont_QA(nsync, wf8(:,8), wf8(:,9), A(:,4), n3(:,91), t3x64(:,:,4), nhel, den(16))
    call cont_QA(nsync, wf8(:,11), wf8(:,12), A(:,5), n3(:,92), t3x64(:,:,5), nhel, den(21))
    call cont_QA(nsync, wf8(:,11), wf8(:,14), A(:,6), n3(:,93), t3x64(:,:,6), nhel, den(23))
    call cont_QA(nsync, wf8(:,16), wf8(:,17), A(:,7), n3(:,94), t3x64(:,:,7), nhel, den(27))
    call cont_QA(nsync, wf8(:,17), wf8(:,18), A(:,8), n3(:,95), t3x64(:,:,8), nhel, den(29))
    call cont_QA(nsync, wf8(:,8), wf8(:,19), A(:,9), n3(:,96), t3x64(:,:,9), nhel, den(30))
    call cont_QA(nsync, wf8(:,8), wf8(:,20), A(:,10), n3(:,97), t3x64(:,:,10), nhel, den(31))
    call cont_QA(nsync, wf8(:,3), wf8(:,21), A(:,11), n3(:,98), t3x64(:,:,11), nhel, den(32))
    call cont_QA(nsync, wf8(:,5), wf8(:,21), A(:,12), n3(:,99), t3x64(:,:,12), nhel, den(33))
    call cont_VV(nsync, wf4(:,6), wf16(:,1), A(:,13), n3(:,100), t3x64(:,:,13), nhel, den(36))
    call cont_VV(nsync, wf4(:,6), wf16(:,2), A(:,14), n3(:,101), t3x64(:,:,14), nhel, den(37))
    call cont_QA(nsync, wf8(:,3), wf8(:,22), A(:,15), n3(:,102), t3x64(:,:,15), nhel, den(41))
    call cont_QA(nsync, wf8(:,5), wf8(:,22), A(:,16), n3(:,103), t3x64(:,:,16), nhel, den(42))
    call cont_QA(nsync, wf8(:,24), wf8(:,25), A(:,17), n3(:,104), t3x64(:,:,17), nhel, den(46))
    call cont_QA(nsync, wf8(:,25), wf8(:,26), A(:,18), n3(:,105), t3x64(:,:,18), nhel, den(48))
    call cont_QA(nsync, wf8(:,28), wf8(:,29), A(:,19), n3(:,106), t3x64(:,:,19), nhel, den(52))
    call cont_QA(nsync, wf8(:,28), wf8(:,31), A(:,20), n3(:,107), t3x64(:,:,20), nhel, den(54))
    call cont_QA(nsync, wf8(:,16), wf8(:,33), A(:,21), n3(:,108), t3x64(:,:,21), nhel, den(56))
    call cont_QA(nsync, wf8(:,18), wf8(:,33), A(:,22), n3(:,109), t3x64(:,:,22), nhel, den(57))
    call cont_QA(nsync, wf8(:,25), wf8(:,34), A(:,23), n3(:,110), t3x64(:,:,23), nhel, den(58))
    call cont_QA(nsync, wf8(:,25), wf8(:,35), A(:,24), n3(:,111), t3x64(:,:,24), nhel, den(59))
    call cont_QA(nsync, wf8(:,3), wf8(:,36), A(:,25), n3(:,112), t3x64(:,:,25), nhel, den(60))
    call cont_QA(nsync, wf8(:,5), wf8(:,36), A(:,26), n3(:,113), t3x64(:,:,26), nhel, den(61))
    call cont_VV(nsync, wf4(:,6), wf16(:,3), A(:,27), n3(:,114), t3x64(:,:,27), nhel, den(62))
    call cont_VV(nsync, wf4(:,6), wf16(:,4), A(:,28), n3(:,115), t3x64(:,:,28), nhel, den(63))
    call cont_QA(nsync, wf8(:,38), wf8(:,39), A(:,29), n3(:,116), t3x64(:,:,29), nhel, den(68))
    call cont_QA(nsync, wf8(:,38), wf8(:,41), A(:,30), n3(:,117), t3x64(:,:,30), nhel, den(70))
    call cont_QA(nsync, wf8(:,7), wf8(:,43), A(:,31), n3(:,118), t3x64(:,:,31), nhel, den(72))
    call cont_QA(nsync, wf8(:,9), wf8(:,43), A(:,32), n3(:,119), t3x64(:,:,32), nhel, den(73))
    call cont_QA(nsync, wf8(:,12), wf8(:,44), A(:,33), n3(:,120), t3x64(:,:,33), nhel, den(76))
    call cont_QA(nsync, wf8(:,14), wf8(:,44), A(:,34), n3(:,121), t3x64(:,:,34), nhel, den(77))
    call cont_QA(nsync, wf8(:,46), wf8(:,47), A(:,35), n3(:,122), t3x64(:,:,35), nhel, den(81))
    call cont_QA(nsync, wf8(:,47), wf8(:,48), A(:,36), n3(:,123), t3x64(:,:,36), nhel, den(83))
    call cont_QA(nsync, wf8(:,19), wf8(:,43), A(:,37), n3(:,124), t3x64(:,:,37), nhel, den(84))
    call cont_QA(nsync, wf8(:,20), wf8(:,43), A(:,38), n3(:,125), t3x64(:,:,38), nhel, den(85))
    call cont_QA(nsync, wf8(:,39), wf8(:,49), A(:,39), n3(:,126), t3x64(:,:,39), nhel, den(86))
    call cont_QA(nsync, wf8(:,41), wf8(:,49), A(:,40), n3(:,127), t3x64(:,:,40), nhel, den(87))
    call cont_VV(nsync, wf4(:,6), wf16(:,5), A(:,41), n3(:,128), t3x64(:,:,41), nhel, den(88))
    call cont_VV(nsync, wf4(:,6), wf16(:,6), A(:,42), n3(:,129), t3x64(:,:,42), nhel, den(89))
    call cont_QA(nsync, wf8(:,39), wf8(:,50), A(:,43), n3(:,130), t3x64(:,:,43), nhel, den(92))
    call cont_QA(nsync, wf8(:,41), wf8(:,50), A(:,44), n3(:,131), t3x64(:,:,44), nhel, den(93))
    call cont_QA(nsync, wf8(:,24), wf8(:,52), A(:,45), n3(:,132), t3x64(:,:,45), nhel, den(95))
    call cont_QA(nsync, wf8(:,26), wf8(:,52), A(:,46), n3(:,133), t3x64(:,:,46), nhel, den(96))
    call cont_QA(nsync, wf8(:,29), wf8(:,53), A(:,47), n3(:,134), t3x64(:,:,47), nhel, den(98))
    call cont_QA(nsync, wf8(:,31), wf8(:,53), A(:,48), n3(:,135), t3x64(:,:,48), nhel, den(99))
    call cont_QA(nsync, wf8(:,46), wf8(:,55), A(:,49), n3(:,136), t3x64(:,:,49), nhel, den(101))
    call cont_QA(nsync, wf8(:,48), wf8(:,55), A(:,50), n3(:,137), t3x64(:,:,50), nhel, den(102))
    call cont_QA(nsync, wf8(:,34), wf8(:,52), A(:,51), n3(:,138), t3x64(:,:,51), nhel, den(103))
    call cont_QA(nsync, wf8(:,35), wf8(:,52), A(:,52), n3(:,139), t3x64(:,:,52), nhel, den(104))
    call cont_QA(nsync, wf8(:,39), wf8(:,56), A(:,53), n3(:,140), t3x64(:,:,53), nhel, den(105))
    call cont_QA(nsync, wf8(:,41), wf8(:,56), A(:,54), n3(:,141), t3x64(:,:,54), nhel, den(106))
    call cont_VV(nsync, wf4(:,6), wf16(:,7), A(:,55), n3(:,142), t3x64(:,:,55), nhel, den(107))
    call cont_VV(nsync, wf4(:,6), wf16(:,8), A(:,56), n3(:,143), t3x64(:,:,56), nhel, den(108))
    call cont_VV(nsync, wf4(:,10), wf16(:,9), A(:,57), n3(:,144), t3x64(:,:,57), nhel, den(111))
    call cont_VV(nsync, wf4(:,10), wf16(:,10), A(:,58), n3(:,145), t3x64(:,:,58), nhel, den(112))
    call cont_VV(nsync, wf4(:,11), wf16(:,11), A(:,59), n3(:,146), t3x64(:,:,59), nhel, den(115))
    call cont_VV(nsync, wf4(:,11), wf16(:,12), A(:,60), n3(:,147), t3x64(:,:,60), nhel, den(116))
    call cont_VV(nsync, wf4(:,11), wf16(:,13), A(:,61), n3(:,148), t3x64(:,:,61), nhel, den(117))
    call cont_VV(nsync, wf4(:,11), wf16(:,14), A(:,62), n3(:,149), t3x64(:,:,62), nhel, den(118))
    call cont_QA(nsync, wf8(:,3), wf8(:,57), A(:,63), n3(:,150), t3x64(:,:,63), nhel, den(119))
    call cont_QA(nsync, wf8(:,5), wf8(:,57), A(:,64), n3(:,151), t3x64(:,:,64), nhel, den(120))
    call cont_VV(nsync, wf4(:,12), wf16(:,15), A(:,65), n3(:,152), t3x64(:,:,65), nhel, den(123))
    call cont_VV(nsync, wf4(:,12), wf16(:,16), A(:,66), n3(:,153), t3x64(:,:,66), nhel, den(124))
    call cont_VV(nsync, wf4(:,13), wf16(:,17), A(:,67), n3(:,154), t3x64(:,:,67), nhel, den(127))
    call cont_VV(nsync, wf4(:,13), wf16(:,18), A(:,68), n3(:,155), t3x64(:,:,68), nhel, den(128))
    call cont_VV(nsync, wf4(:,12), wf16(:,19), A(:,69), n3(:,156), t3x64(:,:,69), nhel, den(129))
    call cont_VV(nsync, wf4(:,12), wf16(:,20), A(:,70), n3(:,157), t3x64(:,:,70), nhel, den(130))
    call cont_QA(nsync, wf8(:,3), wf8(:,58), A(:,71), n3(:,158), t3x64(:,:,71), nhel, den(131))
    call cont_QA(nsync, wf8(:,5), wf8(:,58), A(:,72), n3(:,159), t3x64(:,:,72), nhel, den(132))
    call cont_VV(nsync, wf16(:,17), wf4(:,14), A(:,73), n3(:,160), t3x64(:,:,73), nhel, den(134))
    call cont_VV(nsync, wf16(:,18), wf4(:,14), A(:,74), n3(:,161), t3x64(:,:,74), nhel, den(135))
    call cont_QA(nsync, wf8(:,12), wf8(:,59), A(:,75), n3(:,162), t3x64(:,:,75), nhel, den(137))
    call cont_QA(nsync, wf8(:,14), wf8(:,59), A(:,76), n3(:,163), t3x64(:,:,76), nhel, den(138))
    call cont_QA(nsync, wf8(:,3), wf8(:,60), A(:,77), n3(:,164), t3x64(:,:,77), nhel, den(139))
    call cont_QA(nsync, wf8(:,5), wf8(:,60), A(:,78), n3(:,165), t3x64(:,:,78), nhel, den(140))
    call cont_QA(nsync, wf8(:,3), wf8(:,61), A(:,79), n3(:,166), t3x64(:,:,79), nhel, den(141))
    call cont_QA(nsync, wf8(:,5), wf8(:,61), A(:,80), n3(:,167), t3x64(:,:,80), nhel, den(142))
    call cont_VV(nsync, wf16(:,15), wf4(:,16), A(:,81), n3(:,168), t3x64(:,:,81), nhel, den(144))
    call cont_VV(nsync, wf16(:,16), wf4(:,16), A(:,82), n3(:,169), t3x64(:,:,82), nhel, den(145))
    call cont_QA(nsync, wf8(:,12), wf8(:,62), A(:,83), n3(:,170), t3x64(:,:,83), nhel, den(147))
    call cont_QA(nsync, wf8(:,14), wf8(:,62), A(:,84), n3(:,171), t3x64(:,:,84), nhel, den(148))
    call cont_QA(nsync, wf8(:,39), wf8(:,63), A(:,85), n3(:,172), t3x64(:,:,85), nhel, den(149))
    call cont_QA(nsync, wf8(:,41), wf8(:,63), A(:,86), n3(:,173), t3x64(:,:,86), nhel, den(150))
    call cont_QA(nsync, wf8(:,39), wf8(:,64), A(:,87), n3(:,174), t3x64(:,:,87), nhel, den(151))
    call cont_QA(nsync, wf8(:,41), wf8(:,64), A(:,88), n3(:,175), t3x64(:,:,88), nhel, den(152))

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
  complex(REALKIND), intent(out) :: M1(2) ! M1helarray(2,64)
  integer :: empty(0)

  M1(1) = ((-A(j,27)%j-A(j,28)%j-A(j,41)%j-A(j,42)%j)*f(1))/6._/**/REALKIND+((-A(j,13)%j-A(j,14)%j-A(j,55)%j &
       -A(j,56)%j)*f(1))/2._/**/REALKIND+((-A(j,1)%j-A(j,3)%j-A(j,5)%j-A(j,7)%j-A(j,9)%j-A(j,11)%j-A(j,43)%j-A(j,45)%j-A(j,47)%j &
       -A(j,49)%j-A(j,51)%j-A(j,53)%j-A(j,57)%j-A(j,63)%j-A(j,65)%j-A(j,69)%j-A(j,75)%j-A(j,77)%j-A(j,81)%j &
       -A(j,85)%j)*f(2))/2._/**/REALKIND+((-A(j,15)%j-A(j,17)%j-A(j,19)%j-A(j,21)%j-A(j,23)%j-A(j,25)%j-A(j,29)%j-A(j,31)%j &
       -A(j,33)%j-A(j,35)%j-A(j,37)%j-A(j,39)%j-A(j,59)%j-A(j,61)%j-A(j,67)%j-A(j,71)%j-A(j,73)%j-A(j,79)%j-A(j,83)%j &
       -A(j,87)%j)*f(2))/6._/**/REALKIND+((-A(j,2)%j-A(j,4)%j-A(j,6)%j-A(j,8)%j-A(j,10)%j-A(j,12)%j-A(j,44)%j-A(j,46)%j-A(j,48)%j &
       -A(j,50)%j-A(j,52)%j-A(j,54)%j-A(j,58)%j-A(j,64)%j-A(j,66)%j-A(j,70)%j-A(j,76)%j-A(j,78)%j-A(j,82)%j &
       -A(j,86)%j)*f(3))/2._/**/REALKIND+((-A(j,16)%j-A(j,18)%j-A(j,20)%j-A(j,22)%j-A(j,24)%j-A(j,26)%j-A(j,30)%j-A(j,32)%j &
       -A(j,34)%j-A(j,36)%j-A(j,38)%j-A(j,40)%j-A(j,60)%j-A(j,62)%j-A(j,68)%j-A(j,72)%j-A(j,74)%j-A(j,80)%j-A(j,84)%j &
       -A(j,88)%j)*f(3))/6._/**/REALKIND
  M1(2) = ((A(j,27)%j+A(j,28)%j+A(j,41)%j+A(j,42)%j)*f(1))/2._/**/REALKIND+((A(j,13)%j+A(j,14)%j+A(j,55)%j &
       +A(j,56)%j)*f(1))/6._/**/REALKIND+((A(j,1)%j+A(j,3)%j+A(j,5)%j+A(j,7)%j+A(j,9)%j+A(j,11)%j+A(j,43)%j+A(j,45)%j+A(j,47)%j &
       +A(j,49)%j+A(j,51)%j+A(j,53)%j+A(j,57)%j+A(j,63)%j+A(j,65)%j+A(j,69)%j+A(j,75)%j+A(j,77)%j+A(j,81)%j &
       +A(j,85)%j)*f(2))/6._/**/REALKIND+((A(j,15)%j+A(j,17)%j+A(j,19)%j+A(j,21)%j+A(j,23)%j+A(j,25)%j+A(j,29)%j+A(j,31)%j &
       +A(j,33)%j+A(j,35)%j+A(j,37)%j+A(j,39)%j+A(j,59)%j+A(j,61)%j+A(j,67)%j+A(j,71)%j+A(j,73)%j+A(j,79)%j+A(j,83)%j &
       +A(j,87)%j)*f(2))/2._/**/REALKIND+((A(j,2)%j+A(j,4)%j+A(j,6)%j+A(j,8)%j+A(j,10)%j+A(j,12)%j+A(j,44)%j+A(j,46)%j+A(j,48)%j &
       +A(j,50)%j+A(j,52)%j+A(j,54)%j+A(j,58)%j+A(j,64)%j+A(j,66)%j+A(j,70)%j+A(j,76)%j+A(j,78)%j+A(j,82)%j &
       +A(j,86)%j)*f(3))/6._/**/REALKIND+((A(j,16)%j+A(j,18)%j+A(j,20)%j+A(j,22)%j+A(j,24)%j+A(j,26)%j+A(j,30)%j+A(j,32)%j &
       +A(j,34)%j+A(j,36)%j+A(j,38)%j+A(j,40)%j+A(j,60)%j+A(j,62)%j+A(j,68)%j+A(j,72)%j+A(j,74)%j+A(j,80)%j+A(j,84)%j &
       +A(j,88)%j)*f(3))/2._/**/REALKIND

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
  use ol_colourmatrix_pphlljj_eexbbbxbxh_1_/**/REALKIND, only: K1
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
  use ol_colourmatrix_pphlljj_eexbbbxbxh_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_pphlljj_eexbbbxbxh_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(2,64)
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
    & bind(c,name="ol_f_amp2tree_pphlljj_eexbbbxbxh_1")
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
    & bind(c,name="ol_f_amp2ccone_pphlljj_eexbbbxbxh_1")
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
    & bind(c,name="ol_f_amp2ccall_pphlljj_eexbbbxbxh_1")
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
    & bind(c,name="ol_f_amp2hcone_pphlljj_eexbbbxbxh_1")
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
    & bind(c,name="ol_f_amp2hcall_pphlljj_eexbbbxbxh_1")
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
    & bind(c,name="ol_amp2tree_pphlljj_eexbbbxbxh_1")
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
    & bind(c,name="ol_amp2ccone_pphlljj_eexbbbxbxh_1")
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
    & bind(c,name="ol_amp2ccall_pphlljj_eexbbbxbxh_1")
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
    & bind(c,name="ol_amp2hcone_pphlljj_eexbbbxbxh_1")
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
    & bind(c,name="ol_amp2hcall_pphlljj_eexbbbxbxh_1")
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
    & bind(c,name="amp2tree_pphlljj_eexbbbxbxh_1_")
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
    & bind(c,name="amp2ccone_pphlljj_eexbbbxbxh_1_")
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
    & bind(c,name="amp2ccall_pphlljj_eexbbbxbxh_1_")
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
    & bind(c,name="amp2hcone_pphlljj_eexbbbxbxh_1_")
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
    & bind(c,name="amp2hcall_pphlljj_eexbbbxbxh_1_")
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

end module ol_tree_pphlljj_eexbbbxbxh_1_/**/REALKIND
