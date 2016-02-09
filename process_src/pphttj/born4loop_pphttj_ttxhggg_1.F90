
module ol_tree_pphttj_ttxhggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(2)
  complex(REALKIND), save :: den(105)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 32 ! number of helicity configurations
  integer(intkind2), save :: nhel = 32 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(32) ! physical helicity states
  complex(DREALKIND) :: M1helarr(6,32) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED*gQCD**3*YT)/(2._/**/REALKIND*MW*sw)
    f(2) = (eQED*gQCD**3*YT)/(MW*sw*2._/**/REALKIND)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,5) - MT2)
  den(2) = 1 / (Q(5,56))
  den(4) = 1 / (Q(5,6) - MT2)
  den(6) = 1 / (Q(5,10) - MT2)
  den(7) = 1 / (Q(5,48))
  den(10) = 1 / (Q(5,21) - MT2)
  den(13) = 1 / (Q(5,26) - MT2)
  den(16) = 1 / (Q(5,18) - MT2)
  den(17) = 1 / (Q(5,40))
  den(20) = 1 / (Q(5,13) - MT2)
  den(25) = 1 / (Q(5,34) - MT2)
  den(26) = 1 / (Q(5,24))
  den(29) = 1 / (Q(5,7))
  den(35) = 1 / (Q(5,42) - MT2)
  den(42) = 1 / (Q(5,50) - MT2)
  den(45) = 1 / (Q(5,9) - MT2)
  den(48) = 1 / (Q(5,25) - MT2)
  den(51) = 1 / (Q(5,22) - MT2)
  den(54) = 1 / (Q(5,17) - MT2)
  den(59) = 1 / (Q(5,14) - MT2)
  den(62) = 1 / (Q(5,33) - MT2)
  den(69) = 1 / (Q(5,41) - MT2)
  den(77) = 1 / (Q(5,49) - MT2)
  den(85) = 1 / (Q(5,38) - MT2)
  den(93) = 1 / (Q(5,37) - MT2)

  ! denominators

  den(3) = den(1)*den(2)
  den(5) = den(2)*den(4)
  den(8) = den(1)*den(6)
  den(9) = den(7)*den(8)
  den(11) = den(1)*den(10)
  den(12) = den(6)*den(11)
  den(14) = den(6)*den(13)
  den(15) = den(1)*den(14)
  den(18) = den(1)*den(16)
  den(19) = den(17)*den(18)
  den(21) = den(1)*den(20)
  den(22) = den(16)*den(21)
  den(23) = den(13)*den(16)
  den(24) = den(1)*den(23)
  den(27) = den(1)*den(25)
  den(28) = den(26)*den(27)
  den(30) = den(1)*den(29)
  den(31) = den(26)*den(30)
  den(32) = den(13)*den(26)
  den(33) = den(1)*den(32)
  den(34) = den(21)*den(25)
  den(36) = den(25)*den(35)
  den(37) = den(1)*den(36)
  den(38) = den(17)*den(30)
  den(39) = den(17)*den(35)
  den(40) = den(1)*den(39)
  den(41) = den(7)*den(30)
  den(43) = den(7)*den(42)
  den(44) = den(1)*den(43)
  den(46) = den(4)*den(45)
  den(47) = den(7)*den(46)
  den(49) = den(45)*den(48)
  den(50) = den(4)*den(49)
  den(52) = den(4)*den(51)
  den(53) = den(45)*den(52)
  den(55) = den(4)*den(54)
  den(56) = den(17)*den(55)
  den(57) = den(48)*den(54)
  den(58) = den(4)*den(57)
  den(60) = den(4)*den(59)
  den(61) = den(54)*den(60)
  den(63) = den(4)*den(62)
  den(64) = den(26)*den(63)
  den(65) = den(4)*den(29)
  den(66) = den(26)*den(65)
  den(67) = den(26)*den(48)
  den(68) = den(4)*den(67)
  den(70) = den(62)*den(69)
  den(71) = den(4)*den(70)
  den(72) = den(60)*den(62)
  den(73) = den(17)*den(65)
  den(74) = den(17)*den(69)
  den(75) = den(4)*den(74)
  den(76) = den(7)*den(65)
  den(78) = den(7)*den(77)
  den(79) = den(4)*den(78)
  den(80) = den(20)*den(45)
  den(81) = den(16)*den(80)
  den(82) = den(16)*den(51)
  den(83) = den(45)*den(82)
  den(84) = den(25)*den(80)
  den(86) = den(25)*den(85)
  den(87) = den(45)*den(86)
  den(88) = den(43)*den(45)
  den(89) = den(10)*den(54)
  den(90) = den(6)*den(89)
  den(91) = den(6)*den(59)
  den(92) = den(54)*den(91)
  den(94) = den(62)*den(93)
  den(95) = den(6)*den(94)
  den(96) = den(62)*den(91)
  den(97) = den(6)*den(78)
  den(98) = den(25)*den(89)
  den(99) = den(54)*den(86)
  den(100) = den(39)*den(54)
  den(101) = den(16)*den(94)
  den(102) = den(62)*den(82)
  den(103) = den(16)*den(74)
  den(104) = den(32)*den(62)
  den(105) = den(25)*den(67)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_pphttj_ttxhggg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_pphttj_ttxhggg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for top anti-top higgs glue glue glue -> 0
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
  use ol_external_pphttj_ttxhggg_1, only: external_perm_pphttj_ttxhggg_1, &
    & external_perm_inv_pphttj_ttxhggg_1, extcomb_perm_pphttj_ttxhggg_1, &
    & average_factor_pphttj_ttxhggg_1
  use ol_external_pphttj_ttxhggg_1, only: H, hel_not_initialised, hel_init, POLSEL
  use ol_colourmatrix_pphttj_ttxhggg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_pphttj_ttxhggg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_pphttj_ttxhggg_1
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
  complex(REALKIND) :: MOM_LC(4), M1(6), M1helarray(6,32)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(1), ex4(2), ex5(2), ex6(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf2(2,4), wf4(4,39), wf8(8,42), wf32(32,54)

  type(polcont) :: A(32,54)
  complex(REALKIND) :: Aj(54)

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
  extmasses2 = [ rMT2, rMT2, rMH2, rZERO2, rZERO2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_pphttj_ttxhggg_1,6)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,6)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_pphttj_ttxhggg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_pphttj_ttxhggg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_pphttj_ttxhggg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  42 continue
  ! external WFs
  call pol_wf_Q(P(:,1), rMT, H1, ex1, POLSEL(1))
  call pol_wf_A(P(:,2), rMT, H2, ex2, POLSEL(2))
  call pol_wf_S(P(:,3), rMH, H3, ex3, POLSEL(3))
  call pol_wf_V(P(:,4), rZERO, H4, ex4, POLSEL(4))
  call pol_wf_V(P(:,5), rZERO, H5, ex5, POLSEL(5))
  call pol_wf_V(P(:,6), rZERO, H6, ex6, POLSEL(6))


  if (ntry == 1) then
    shift = 1
    ! call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    ! call helbookkeeping_wf(H1, ex1, shift) ...

    if (any(POLSEL /= 0)) then

      call pol_wf_Q(P(:,1), rMT, H1, ex1, 0)
      call pol_wf_A(P(:,2), rMT, H2, ex2, 0)
      call pol_wf_S(P(:,3), rMH, H3, ex3, 0)
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
  call vert_QS_A(gH,ntry, ex1, ex3, wf2(:,1), n3(:,1), t3x2(:,:,1))
  call vert_GGG_G(ntry, ex4, ex5, ex6, wf8(:,1), n4(:,1), t4x8(:,:,1))
  call prop_Q_A(ntry, wf2(:,1), Q(:,5), MT, 1_intkind1, wf2(:,2), n2(1))
  call vert_QA_V(ntry, wf2(:,2), ex2, wf4(:,1), n3(:,2), t3x4(:,:,1))
  call vert_GGG_G(ntry, ex5, ex6, ex4, wf8(:,2), n4(:,2), t4x8(:,:,2))
  call vert_GGG_G(ntry, ex6, ex4, ex5, wf8(:,3), n4(:,3), t4x8(:,:,3))
  call vert_SA_Q(gH,ntry, ex3, ex2, wf2(:,3), n3(:,3), t3x2(:,:,2))
  call prop_A_Q(ntry, wf2(:,3), Q(:,6), MT, 1_intkind1, wf2(:,4), n2(2))
  call vert_QA_V(ntry, ex1, wf2(:,4), wf4(:,2), n3(:,4), t3x4(:,:,2))
  call vert_AV_Q(ntry, ex2, ex4, wf4(:,3), n3(:,5), t3x4(:,:,3))
  call vert_UV_W(ntry, ex5, Q(:,16), ex6, Q(:,32), wf4(:,4), n3(:,6), t3x4(:,:,4))
  call prop_A_Q(ntry, wf4(:,3), Q(:,10), MT, 1_intkind1, wf4(:,5), n2(3))
  call vert_QA_V(ntry, wf2(:,2), wf4(:,5), wf8(:,4), n3(:,7), t3x8(:,:,1))
  call vert_VQ_A(ntry, ex5, wf2(:,2), wf4(:,6), n3(:,8), t3x4(:,:,5))
  call vert_AV_Q(ntry, wf4(:,5), ex6, wf8(:,5), n3(:,9), t3x8(:,:,2))
  call prop_Q_A(ntry, wf4(:,6), Q(:,21), MT, 1_intkind1, wf4(:,7), n2(4))
  call vert_AV_Q(ntry, wf4(:,5), ex5, wf8(:,6), n3(:,10), t3x8(:,:,3))
  call vert_VQ_A(ntry, ex6, wf2(:,2), wf4(:,8), n3(:,11), t3x4(:,:,6))
  call prop_A_Q(ntry, wf8(:,6), Q(:,26), MT, 1_intkind1, wf8(:,7), n2(5))
  call vert_AV_Q(ntry, ex2, ex5, wf4(:,9), n3(:,12), t3x4(:,:,7))
  call vert_UV_W(ntry, ex4, Q(:,8), ex6, Q(:,32), wf4(:,10), n3(:,13), t3x4(:,:,8))
  call prop_A_Q(ntry, wf4(:,9), Q(:,18), MT, 1_intkind1, wf4(:,11), n2(6))
  call vert_QA_V(ntry, wf2(:,2), wf4(:,11), wf8(:,8), n3(:,14), t3x8(:,:,4))
  call vert_VQ_A(ntry, ex4, wf2(:,2), wf4(:,12), n3(:,15), t3x4(:,:,9))
  call vert_AV_Q(ntry, wf4(:,11), ex6, wf8(:,9), n3(:,16), t3x8(:,:,5))
  call prop_Q_A(ntry, wf4(:,12), Q(:,13), MT, 1_intkind1, wf4(:,13), n2(7))
  call vert_AV_Q(ntry, wf4(:,11), ex4, wf8(:,10), n3(:,17), t3x8(:,:,6))
  call prop_A_Q(ntry, wf8(:,10), Q(:,26), MT, 1_intkind1, wf8(:,11), n2(8))
  call vert_AV_Q(ntry, ex2, ex6, wf4(:,14), n3(:,18), t3x4(:,:,10))
  call vert_UV_W(ntry, ex4, Q(:,8), ex5, Q(:,16), wf4(:,15), n3(:,19), t3x4(:,:,11))
  call prop_A_Q(ntry, wf4(:,14), Q(:,34), MT, 1_intkind1, wf4(:,16), n2(9))
  call vert_QA_V(ntry, wf2(:,2), wf4(:,16), wf8(:,12), n3(:,20), t3x8(:,:,7))
  call vert_UV_W(ntry, wf4(:,15), Q(:,24), ex6, Q(:,32), wf8(:,13), n3(:,21), t3x8(:,:,8))
  call vert_AV_Q(ntry, ex2, wf4(:,15), wf8(:,14), n3(:,22), t3x8(:,:,9))
  call prop_A_Q(ntry, wf8(:,14), Q(:,26), MT, 1_intkind1, wf8(:,15), n2(10))
  call vert_AV_Q(ntry, wf4(:,16), ex5, wf8(:,16), n3(:,23), t3x8(:,:,10))
  call vert_AV_Q(ntry, wf4(:,16), ex4, wf8(:,17), n3(:,24), t3x8(:,:,11))
  call prop_A_Q(ntry, wf8(:,17), Q(:,42), MT, 1_intkind1, wf8(:,18), n2(11))
  call vert_UV_W(ntry, ex5, Q(:,16), wf4(:,10), Q(:,40), wf8(:,19), n3(:,25), t3x8(:,:,12))
  call vert_AV_Q(ntry, ex2, wf4(:,10), wf8(:,20), n3(:,26), t3x8(:,:,13))
  call prop_A_Q(ntry, wf8(:,20), Q(:,42), MT, 1_intkind1, wf8(:,21), n2(12))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,4), Q(:,48), wf8(:,22), n3(:,27), t3x8(:,:,14))
  call vert_AV_Q(ntry, ex2, wf4(:,4), wf8(:,23), n3(:,28), t3x8(:,:,15))
  call prop_A_Q(ntry, wf8(:,23), Q(:,50), MT, 1_intkind1, wf8(:,24), n2(13))
  call vert_VQ_A(ntry, ex4, ex1, wf4(:,17), n3(:,29), t3x4(:,:,12))
  call prop_Q_A(ntry, wf4(:,17), Q(:,9), MT, 1_intkind1, wf4(:,18), n2(14))
  call vert_QA_V(ntry, wf4(:,18), wf2(:,4), wf8(:,25), n3(:,30), t3x8(:,:,16))
  call vert_VQ_A(ntry, ex5, wf4(:,18), wf8(:,26), n3(:,31), t3x8(:,:,17))
  call vert_AV_Q(ntry, wf2(:,4), ex6, wf4(:,19), n3(:,32), t3x4(:,:,13))
  call prop_Q_A(ntry, wf8(:,26), Q(:,25), MT, 1_intkind1, wf8(:,27), n2(15))
  call vert_AV_Q(ntry, wf2(:,4), ex5, wf4(:,20), n3(:,33), t3x4(:,:,14))
  call vert_VQ_A(ntry, ex6, wf4(:,18), wf8(:,28), n3(:,34), t3x8(:,:,18))
  call prop_A_Q(ntry, wf4(:,20), Q(:,22), MT, 1_intkind1, wf4(:,21), n2(16))
  call vert_VQ_A(ntry, ex5, ex1, wf4(:,22), n3(:,35), t3x4(:,:,15))
  call prop_Q_A(ntry, wf4(:,22), Q(:,17), MT, 1_intkind1, wf4(:,23), n2(17))
  call vert_QA_V(ntry, wf4(:,23), wf2(:,4), wf8(:,29), n3(:,36), t3x8(:,:,19))
  call vert_VQ_A(ntry, ex4, wf4(:,23), wf8(:,30), n3(:,37), t3x8(:,:,20))
  call prop_Q_A(ntry, wf8(:,30), Q(:,25), MT, 1_intkind1, wf8(:,31), n2(18))
  call vert_AV_Q(ntry, wf2(:,4), ex4, wf4(:,24), n3(:,38), t3x4(:,:,16))
  call vert_VQ_A(ntry, ex6, wf4(:,23), wf8(:,32), n3(:,39), t3x8(:,:,21))
  call prop_A_Q(ntry, wf4(:,24), Q(:,14), MT, 1_intkind1, wf4(:,25), n2(19))
  call vert_VQ_A(ntry, ex6, ex1, wf4(:,26), n3(:,40), t3x4(:,:,17))
  call prop_Q_A(ntry, wf4(:,26), Q(:,33), MT, 1_intkind1, wf4(:,27), n2(20))
  call vert_QA_V(ntry, wf4(:,27), wf2(:,4), wf8(:,33), n3(:,41), t3x8(:,:,22))
  call vert_VQ_A(ntry, wf4(:,15), ex1, wf8(:,34), n3(:,42), t3x8(:,:,23))
  call prop_Q_A(ntry, wf8(:,34), Q(:,25), MT, 1_intkind1, wf8(:,35), n2(21))
  call vert_VQ_A(ntry, ex4, wf4(:,27), wf8(:,36), n3(:,43), t3x8(:,:,24))
  call prop_Q_A(ntry, wf8(:,36), Q(:,41), MT, 1_intkind1, wf8(:,37), n2(22))
  call vert_VQ_A(ntry, ex5, wf4(:,27), wf8(:,38), n3(:,44), t3x8(:,:,25))
  call vert_VQ_A(ntry, wf4(:,10), ex1, wf8(:,39), n3(:,45), t3x8(:,:,26))
  call prop_Q_A(ntry, wf8(:,39), Q(:,41), MT, 1_intkind1, wf8(:,40), n2(23))
  call vert_VQ_A(ntry, wf4(:,4), ex1, wf8(:,41), n3(:,46), t3x8(:,:,27))
  call prop_Q_A(ntry, wf8(:,41), Q(:,49), MT, 1_intkind1, wf8(:,42), n2(24))
  call vert_QS_A(gH,ntry, wf4(:,18), ex3, wf4(:,28), n3(:,47), t3x4(:,:,18))
  call prop_Q_A(ntry, wf4(:,28), Q(:,13), MT, 1_intkind1, wf4(:,29), n2(25))
  call vert_SA_Q(gH,ntry, ex3, wf4(:,11), wf4(:,30), n3(:,48), t3x4(:,:,19))
  call prop_A_Q(ntry, wf4(:,30), Q(:,22), MT, 1_intkind1, wf4(:,31), n2(26))
  call vert_SA_Q(gH,ntry, ex3, wf4(:,16), wf4(:,32), n3(:,49), t3x4(:,:,20))
  call prop_A_Q(ntry, wf4(:,32), Q(:,38), MT, 1_intkind1, wf4(:,33), n2(27))
  call vert_QS_A(gH,ntry, wf4(:,23), ex3, wf4(:,34), n3(:,50), t3x4(:,:,21))
  call prop_Q_A(ntry, wf4(:,34), Q(:,21), MT, 1_intkind1, wf4(:,35), n2(28))
  call vert_SA_Q(gH,ntry, ex3, wf4(:,5), wf4(:,36), n3(:,51), t3x4(:,:,22))
  call prop_A_Q(ntry, wf4(:,36), Q(:,14), MT, 1_intkind1, wf4(:,37), n2(29))
  call vert_QS_A(gH,ntry, wf4(:,27), ex3, wf4(:,38), n3(:,52), t3x4(:,:,23))
  call prop_Q_A(ntry, wf4(:,38), Q(:,37), MT, 1_intkind1, wf4(:,39), n2(30))


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
      call colint(M1, M2add, extcombs_permuted)
      M2 = M2 + M2add
    end do
    M2 = 0.25_/**/REALKIND * M2

  end if

  if ( JBmunu /= 0 ) then ! Bmunu helicity correlation
    M2munu = 0
  end if

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2add = M2 / average_factor_pphttj_ttxhggg_1

  do k = 0, 23-1
    M2(k) = M2add(extcomb_perm_pphttj_ttxhggg_1(k))
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

    call cont_VV(nsync, wf8(:,1), wf4(:,1), A(:,1), n3(:,53), t3x32(:,:,1), nhel, den(3))
    call cont_VV(nsync, wf4(:,1), wf8(:,2), A(:,2), n3(:,54), t3x32(:,:,2), nhel, den(3))
    call cont_VV(nsync, wf4(:,1), wf8(:,3), A(:,3), n3(:,55), t3x32(:,:,3), nhel, den(3))
    call cont_VV(nsync, wf8(:,1), wf4(:,2), A(:,4), n3(:,56), t3x32(:,:,4), nhel, den(5))
    call cont_VV(nsync, wf8(:,2), wf4(:,2), A(:,5), n3(:,57), t3x32(:,:,5), nhel, den(5))
    call cont_VV(nsync, wf8(:,3), wf4(:,2), A(:,6), n3(:,58), t3x32(:,:,6), nhel, den(5))
    call cont_VV(nsync, wf4(:,4), wf8(:,4), A(:,7), n3(:,59), t3x32(:,:,7), nhel, den(9))
    call cont_QA(nsync, wf8(:,5), wf4(:,7), A(:,8), n3(:,60), t3x32(:,:,8), nhel, den(12))
    call cont_QA(nsync, wf4(:,8), wf8(:,7), A(:,9), n3(:,61), t3x32(:,:,9), nhel, den(15))
    call cont_VV(nsync, wf4(:,10), wf8(:,8), A(:,10), n3(:,62), t3x32(:,:,10), nhel, den(19))
    call cont_QA(nsync, wf8(:,9), wf4(:,13), A(:,11), n3(:,63), t3x32(:,:,11), nhel, den(22))
    call cont_QA(nsync, wf4(:,8), wf8(:,11), A(:,12), n3(:,64), t3x32(:,:,12), nhel, den(24))
    call cont_VV(nsync, wf4(:,15), wf8(:,12), A(:,13), n3(:,65), t3x32(:,:,13), nhel, den(28))
    call cont_VV(nsync, wf4(:,1), wf8(:,13), A(:,14), n3(:,66), t3x32(:,:,14), nhel, den(31))
    call cont_QA(nsync, wf4(:,8), wf8(:,15), A(:,15), n3(:,67), t3x32(:,:,15), nhel, den(33))
    call cont_QA(nsync, wf4(:,13), wf8(:,16), A(:,16), n3(:,68), t3x32(:,:,16), nhel, den(34))
    call cont_QA(nsync, wf4(:,6), wf8(:,18), A(:,17), n3(:,69), t3x32(:,:,17), nhel, den(37))
    call cont_VV(nsync, wf4(:,1), wf8(:,19), A(:,18), n3(:,70), t3x32(:,:,18), nhel, den(38))
    call cont_QA(nsync, wf4(:,6), wf8(:,21), A(:,19), n3(:,71), t3x32(:,:,19), nhel, den(40))
    call cont_VV(nsync, wf4(:,1), wf8(:,22), A(:,20), n3(:,72), t3x32(:,:,20), nhel, den(41))
    call cont_QA(nsync, wf4(:,12), wf8(:,24), A(:,21), n3(:,73), t3x32(:,:,21), nhel, den(44))
    call cont_VV(nsync, wf4(:,4), wf8(:,25), A(:,22), n3(:,74), t3x32(:,:,22), nhel, den(47))
    call cont_QA(nsync, wf4(:,19), wf8(:,27), A(:,23), n3(:,75), t3x32(:,:,23), nhel, den(50))
    call cont_QA(nsync, wf8(:,28), wf4(:,21), A(:,24), n3(:,76), t3x32(:,:,24), nhel, den(53))
    call cont_VV(nsync, wf4(:,10), wf8(:,29), A(:,25), n3(:,77), t3x32(:,:,25), nhel, den(56))
    call cont_QA(nsync, wf4(:,19), wf8(:,31), A(:,26), n3(:,78), t3x32(:,:,26), nhel, den(58))
    call cont_QA(nsync, wf8(:,32), wf4(:,25), A(:,27), n3(:,79), t3x32(:,:,27), nhel, den(61))
    call cont_VV(nsync, wf4(:,15), wf8(:,33), A(:,28), n3(:,80), t3x32(:,:,28), nhel, den(64))
    call cont_VV(nsync, wf4(:,2), wf8(:,13), A(:,29), n3(:,81), t3x32(:,:,29), nhel, den(66))
    call cont_QA(nsync, wf4(:,19), wf8(:,35), A(:,30), n3(:,82), t3x32(:,:,30), nhel, den(68))
    call cont_QA(nsync, wf4(:,20), wf8(:,37), A(:,31), n3(:,83), t3x32(:,:,31), nhel, den(71))
    call cont_QA(nsync, wf4(:,25), wf8(:,38), A(:,32), n3(:,84), t3x32(:,:,32), nhel, den(72))
    call cont_VV(nsync, wf4(:,2), wf8(:,19), A(:,33), n3(:,85), t3x32(:,:,33), nhel, den(73))
    call cont_QA(nsync, wf4(:,20), wf8(:,40), A(:,34), n3(:,86), t3x32(:,:,34), nhel, den(75))
    call cont_VV(nsync, wf4(:,2), wf8(:,22), A(:,35), n3(:,87), t3x32(:,:,35), nhel, den(76))
    call cont_QA(nsync, wf4(:,24), wf8(:,42), A(:,36), n3(:,88), t3x32(:,:,36), nhel, den(79))
    call cont_QA(nsync, wf8(:,9), wf4(:,29), A(:,37), n3(:,89), t3x32(:,:,37), nhel, den(81))
    call cont_QA(nsync, wf8(:,28), wf4(:,31), A(:,38), n3(:,90), t3x32(:,:,38), nhel, den(83))
    call cont_QA(nsync, wf8(:,16), wf4(:,29), A(:,39), n3(:,91), t3x32(:,:,39), nhel, den(84))
    call cont_QA(nsync, wf8(:,26), wf4(:,33), A(:,40), n3(:,92), t3x32(:,:,40), nhel, den(87))
    call cont_QA(nsync, wf8(:,24), wf4(:,28), A(:,41), n3(:,93), t3x32(:,:,41), nhel, den(88))
    call cont_QA(nsync, wf8(:,5), wf4(:,35), A(:,42), n3(:,94), t3x32(:,:,42), nhel, den(90))
    call cont_QA(nsync, wf8(:,32), wf4(:,37), A(:,43), n3(:,95), t3x32(:,:,43), nhel, den(92))
    call cont_QA(nsync, wf8(:,6), wf4(:,39), A(:,44), n3(:,96), t3x32(:,:,44), nhel, den(95))
    call cont_QA(nsync, wf8(:,38), wf4(:,37), A(:,45), n3(:,97), t3x32(:,:,45), nhel, den(96))
    call cont_QA(nsync, wf8(:,42), wf4(:,36), A(:,46), n3(:,98), t3x32(:,:,46), nhel, den(97))
    call cont_QA(nsync, wf8(:,17), wf4(:,35), A(:,47), n3(:,99), t3x32(:,:,47), nhel, den(98))
    call cont_QA(nsync, wf8(:,30), wf4(:,33), A(:,48), n3(:,100), t3x32(:,:,48), nhel, den(99))
    call cont_QA(nsync, wf8(:,21), wf4(:,34), A(:,49), n3(:,101), t3x32(:,:,49), nhel, den(100))
    call cont_QA(nsync, wf8(:,10), wf4(:,39), A(:,50), n3(:,102), t3x32(:,:,50), nhel, den(101))
    call cont_QA(nsync, wf8(:,36), wf4(:,31), A(:,51), n3(:,103), t3x32(:,:,51), nhel, den(102))
    call cont_QA(nsync, wf8(:,40), wf4(:,30), A(:,52), n3(:,104), t3x32(:,:,52), nhel, den(103))
    call cont_QA(nsync, wf8(:,15), wf4(:,38), A(:,53), n3(:,105), t3x32(:,:,53), nhel, den(104))
    call cont_QA(nsync, wf8(:,35), wf4(:,32), A(:,54), n3(:,106), t3x32(:,:,54), nhel, den(105))

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
  complex(REALKIND), intent(out) :: M1(6) ! M1helarray(6,32)
  integer :: empty(0)

  M1(1) = (A(j,1)%j-A(j,2)%j+A(j,4)%j-A(j,5)%j+A(j,9)%j+A(j,14)%j+A(j,20)%j+A(j,29)%j+A(j,32)%j+A(j,35)%j+A(j,44)%j &
       +A(j,45)%j)*f(1)+CI*(A(j,7)%j+A(j,15)%j+A(j,28)%j+A(j,36)%j+A(j,46)%j+A(j,53)%j)*f(2)
  M1(2) = (A(j,2)%j-A(j,3)%j+A(j,5)%j-A(j,6)%j+A(j,8)%j-A(j,18)%j-A(j,20)%j+A(j,27)%j-A(j,33)%j-A(j,35)%j+A(j,42)%j &
       +A(j,43)%j)*f(1)+CI*(-A(j,7)%j+A(j,19)%j+A(j,25)%j-A(j,36)%j-A(j,46)%j+A(j,49)%j)*f(2)
  M1(3) = (-A(j,1)%j+A(j,3)%j-A(j,4)%j+A(j,6)%j+A(j,12)%j-A(j,14)%j+A(j,18)%j-A(j,29)%j+A(j,31)%j+A(j,33)%j+A(j,50)%j &
       +A(j,51)%j)*f(1)+CI*(A(j,10)%j-A(j,15)%j-A(j,28)%j+A(j,34)%j+A(j,52)%j-A(j,53)%j)*f(2)
  M1(4) = (A(j,2)%j-A(j,3)%j+A(j,5)%j-A(j,6)%j+A(j,11)%j-A(j,18)%j-A(j,20)%j+A(j,24)%j-A(j,33)%j-A(j,35)%j+A(j,37)%j &
       +A(j,38)%j)*f(1)+CI*(-A(j,10)%j+A(j,21)%j+A(j,22)%j-A(j,34)%j+A(j,41)%j-A(j,52)%j)*f(2)
  M1(5) = (-A(j,1)%j+A(j,3)%j-A(j,4)%j+A(j,6)%j-A(j,14)%j+A(j,17)%j+A(j,18)%j+A(j,26)%j-A(j,29)%j+A(j,33)%j+A(j,47)%j &
       +A(j,48)%j)*f(1)+CI*(A(j,13)%j-A(j,19)%j-A(j,25)%j+A(j,30)%j-A(j,49)%j+A(j,54)%j)*f(2)
  M1(6) = (A(j,1)%j-A(j,2)%j+A(j,4)%j-A(j,5)%j+A(j,14)%j+A(j,16)%j+A(j,20)%j+A(j,23)%j+A(j,29)%j+A(j,35)%j+A(j,39)%j &
       +A(j,40)%j)*f(1)+CI*(-A(j,13)%j-A(j,21)%j-A(j,22)%j-A(j,30)%j-A(j,41)%j-A(j,54)%j)*f(2)

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
  use ol_colourmatrix_pphttj_ttxhggg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(6)
  real(REALKIND),    intent(out) :: M2colint(0:23-1)
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
  use ol_colourmatrix_pphttj_ttxhggg_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_pphttj_ttxhggg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(6,32)
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
    & bind(c,name="ol_f_amp2tree_pphttj_ttxhggg_1")
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
    & bind(c,name="ol_f_amp2ccone_pphttj_ttxhggg_1")
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
    & bind(c,name="ol_f_amp2ccall_pphttj_ttxhggg_1")
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
    & bind(c,name="ol_f_amp2hcone_pphttj_ttxhggg_1")
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
    & bind(c,name="ol_f_amp2hcall_pphttj_ttxhggg_1")
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
    & bind(c,name="ol_amp2tree_pphttj_ttxhggg_1")
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
    & bind(c,name="ol_amp2ccone_pphttj_ttxhggg_1")
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
    & bind(c,name="ol_amp2ccall_pphttj_ttxhggg_1")
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
    & bind(c,name="ol_amp2hcone_pphttj_ttxhggg_1")
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
    & bind(c,name="ol_amp2hcall_pphttj_ttxhggg_1")
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
    & bind(c,name="amp2tree_pphttj_ttxhggg_1_")
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
    & bind(c,name="amp2ccone_pphttj_ttxhggg_1_")
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
    & bind(c,name="amp2ccall_pphttj_ttxhggg_1_")
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
    & bind(c,name="amp2hcone_pphttj_ttxhggg_1_")
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
    & bind(c,name="amp2hcall_pphttj_ttxhggg_1_")
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

end module ol_tree_pphttj_ttxhggg_1_/**/REALKIND
