
module ol_tree_ppllaa_eexuuxaa_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(4)
  complex(REALKIND), save :: den(82)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 64 ! number of helicity configurations
  integer(intkind2), save :: nhel = 64 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(64) ! physical helicity states
  complex(DREALKIND) :: M1helarr(1,64) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (8*CI*eQED**4)/27._/**/REALKIND
    f(2) = (4*CI*eQED**4)/9._/**/REALKIND
    f(3) = (2*CI*eQED**4)/3._/**/REALKIND
    f(4) = CI*eQED**4

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,3))
  den(2) = 1 / (Q(5,20))
  den(3) = 1 / (Q(5,40))
  den(6) = 1 / (Q(5,3) - MZ2)
  den(9) = 1 / (Q(5,11))
  den(14) = 1 / (Q(5,36))
  den(15) = 1 / (Q(5,24))
  den(20) = 1 / (Q(5,7))
  den(29) = 1 / (Q(5,17))
  den(30) = 1 / (Q(5,34))
  den(31) = 1 / (Q(5,12))
  den(34) = 1 / (Q(5,12) - MZ2)
  den(36) = 1 / (Q(5,14))
  den(41) = 1 / (Q(5,33))
  den(42) = 1 / (Q(5,18))
  den(46) = 1 / (Q(5,13))
  den(55) = 1 / (Q(5,19))
  den(58) = 1 / (Q(5,19) - MZ2)
  den(69) = 1 / (Q(5,35))
  den(72) = 1 / (Q(5,35) - MZ2)

  ! denominators

  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(3)*den(7)
  den(10) = den(1)*den(9)
  den(11) = den(2)*den(10)
  den(12) = den(6)*den(9)
  den(13) = den(2)*den(12)
  den(16) = den(1)*den(14)
  den(17) = den(15)*den(16)
  den(18) = den(6)*den(14)
  den(19) = den(15)*den(18)
  den(21) = den(1)*den(20)
  den(22) = den(15)*den(21)
  den(23) = den(6)*den(20)
  den(24) = den(15)*den(23)
  den(25) = den(10)*den(14)
  den(26) = den(12)*den(14)
  den(27) = den(3)*den(21)
  den(28) = den(3)*den(23)
  den(32) = den(29)*den(30)
  den(33) = den(31)*den(32)
  den(35) = den(32)*den(34)
  den(37) = den(31)*den(36)
  den(38) = den(29)*den(37)
  den(39) = den(34)*den(36)
  den(40) = den(29)*den(39)
  den(43) = den(41)*den(42)
  den(44) = den(31)*den(43)
  den(45) = den(34)*den(43)
  den(47) = den(31)*den(46)
  den(48) = den(42)*den(47)
  den(49) = den(34)*den(46)
  den(50) = den(42)*den(49)
  den(51) = den(37)*den(41)
  den(52) = den(39)*den(41)
  den(53) = den(30)*den(47)
  den(54) = den(30)*den(49)
  den(56) = den(29)*den(55)
  den(57) = den(14)*den(56)
  den(59) = den(29)*den(58)
  den(60) = den(14)*den(59)
  den(61) = den(3)*den(56)
  den(62) = den(3)*den(59)
  den(63) = den(42)*den(55)
  den(64) = den(14)*den(63)
  den(65) = den(42)*den(58)
  den(66) = den(14)*den(65)
  den(67) = den(3)*den(63)
  den(68) = den(3)*den(65)
  den(70) = den(41)*den(69)
  den(71) = den(2)*den(70)
  den(73) = den(41)*den(72)
  den(74) = den(2)*den(73)
  den(75) = den(30)*den(69)
  den(76) = den(2)*den(75)
  den(77) = den(30)*den(72)
  den(78) = den(2)*den(77)
  den(79) = den(15)*den(70)
  den(80) = den(15)*den(73)
  den(81) = den(15)*den(75)
  den(82) = den(15)*den(77)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppllaa_eexuuxaa_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppllaa_eexuuxaa_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for e- e+ up anti-up gamma gamma -> 0
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
  use ol_external_ppllaa_eexuuxaa_1, only: external_perm_ppllaa_eexuuxaa_1, &
    & external_perm_inv_ppllaa_eexuuxaa_1, extcomb_perm_ppllaa_eexuuxaa_1, &
    & average_factor_ppllaa_eexuuxaa_1
  use ol_external_ppllaa_eexuuxaa_1, only: H, hel_not_initialised, hel_init, POLSEL
  use ol_colourmatrix_ppllaa_eexuuxaa_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppllaa_eexuuxaa_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_ppllaa_eexuuxaa_1
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
  complex(REALKIND) :: MOM_LC(4), M1(1), M1helarray(1,64)
  real(REALKIND)    :: P_scatt_intern(0:3,6)
  complex(REALKIND) :: epLC(1:4), epStd(1:4,2)
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,22), wf8(8,44), wf16(16,8), wf64(64,40)

  type(polcont) :: A(64,40)
  complex(REALKIND) :: Aj(40)

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
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppllaa_eexuuxaa_1,6)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,6)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppllaa_eexuuxaa_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppllaa_eexuuxaa_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppllaa_eexuuxaa_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  42 continue
  ! external WFs
  call pol_wf_Q(P(:,1), rZERO, H1, ex1, POLSEL(1))
  call pol_wf_A(P(:,2), rZERO, H2, ex2, POLSEL(2))
  call pol_wf_Q(P(:,3), rZERO, H3, ex3, POLSEL(3))
  call pol_wf_A(P(:,4), rZERO, H4, ex4, POLSEL(4))
  call pol_wf_V(P(:,5), rZERO, H5, ex5, POLSEL(5))
  call pol_wf_V(P(:,6), rZERO, H6, ex6, POLSEL(6))


  if (ntry == 1) then
    shift = 1
    ! call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    ! call helbookkeeping_wf(H1, ex1, shift) ...

    if (any(POLSEL /= 0)) then

      call pol_wf_Q(P(:,1), rZERO, H1, ex1, 0)
      call pol_wf_A(P(:,2), rZERO, H2, ex2, 0)
      call pol_wf_Q(P(:,3), rZERO, H3, ex3, 0)
      call pol_wf_A(P(:,4), rZERO, H4, ex4, 0)
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
  call vert_QA_V(ntry, ex1, ex2, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_VQ_A(ntry, ex5, ex3, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_AV_Q(ntry, ex4, ex6, wf4(:,3), n3(:,3), t3x4(:,:,3))
  call prop_Q_A(ntry, wf4(:,2), Q(:,20), ZERO, 0_intkind1, wf4(:,4), n2(1))
  call prop_A_Q(ntry, wf4(:,3), Q(:,40), ZERO, 0_intkind1, wf4(:,5), n2(2))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,4), wf16(:,1), n3(:,4), t3x16(:,:,1))
  call vert_QA_Z(gZl,ntry, ex1, ex2, wf4(:,6), n3(:,5), t3x4(:,:,4))
  call prop_W_W(ntry, wf4(:,6), Q(:,3), MZ, 1_intkind1, wf4(:,7), n2(3))
  call vert_ZQ_A(gZu,ntry, wf4(:,7), wf4(:,4), wf16(:,2), n3(:,6), t3x16(:,:,2))
  call vert_AV_Q(ntry, ex4, wf4(:,1), wf8(:,1), n3(:,7), t3x8(:,:,1))
  call vert_VQ_A(ntry, ex6, wf4(:,4), wf8(:,2), n3(:,8), t3x8(:,:,2))
  call prop_A_Q(ntry, wf8(:,1), Q(:,11), ZERO, 0_intkind1, wf8(:,3), n2(4))
  call vert_AZ_Q(gZu,ntry, ex4, wf4(:,7), wf8(:,4), n3(:,9), t3x8(:,:,3))
  call prop_A_Q(ntry, wf8(:,4), Q(:,11), ZERO, 0_intkind1, wf8(:,5), n2(5))
  call vert_VQ_A(ntry, ex6, ex3, wf4(:,8), n3(:,10), t3x4(:,:,5))
  call vert_AV_Q(ntry, ex4, ex5, wf4(:,9), n3(:,11), t3x4(:,:,6))
  call prop_Q_A(ntry, wf4(:,8), Q(:,36), ZERO, 0_intkind1, wf4(:,10), n2(6))
  call prop_A_Q(ntry, wf4(:,9), Q(:,24), ZERO, 0_intkind1, wf4(:,11), n2(7))
  call vert_VQ_A(ntry, wf4(:,1), wf4(:,10), wf16(:,3), n3(:,12), t3x16(:,:,3))
  call vert_ZQ_A(gZu,ntry, wf4(:,7), wf4(:,10), wf16(:,4), n3(:,13), t3x16(:,:,4))
  call vert_VQ_A(ntry, wf4(:,1), ex3, wf8(:,6), n3(:,14), t3x8(:,:,4))
  call vert_AV_Q(ntry, wf4(:,11), ex6, wf8(:,7), n3(:,15), t3x8(:,:,5))
  call prop_Q_A(ntry, wf8(:,6), Q(:,7), ZERO, 0_intkind1, wf8(:,8), n2(8))
  call vert_ZQ_A(gZu,ntry, wf4(:,7), ex3, wf8(:,9), n3(:,16), t3x8(:,:,6))
  call prop_Q_A(ntry, wf8(:,9), Q(:,7), ZERO, 0_intkind1, wf8(:,10), n2(9))
  call vert_VQ_A(ntry, ex5, wf4(:,10), wf8(:,11), n3(:,17), t3x8(:,:,7))
  call vert_AV_Q(ntry, wf4(:,5), ex5, wf8(:,12), n3(:,18), t3x8(:,:,8))
  call vert_VQ_A(ntry, ex5, ex1, wf4(:,12), n3(:,19), t3x4(:,:,7))
  call vert_AV_Q(ntry, ex2, ex6, wf4(:,13), n3(:,20), t3x4(:,:,8))
  call vert_QA_V(ntry, ex3, ex4, wf4(:,14), n3(:,21), t3x4(:,:,9))
  call prop_Q_A(ntry, wf4(:,12), Q(:,17), ZERO, 0_intkind1, wf4(:,15), n2(10))
  call prop_A_Q(ntry, wf4(:,13), Q(:,34), ZERO, 0_intkind1, wf4(:,16), n2(11))
  call vert_QA_V(ntry, wf4(:,15), wf4(:,16), wf16(:,5), n3(:,22), t3x16(:,:,5))
  call vert_QA_Z(gZu,ntry, ex3, ex4, wf4(:,17), n3(:,23), t3x4(:,:,10))
  call prop_W_W(ntry, wf4(:,17), Q(:,12), MZ, 1_intkind1, wf4(:,18), n2(12))
  call vert_QA_Z(gZl,ntry, wf4(:,15), wf4(:,16), wf16(:,6), n3(:,24), t3x16(:,:,6))
  call vert_AV_Q(ntry, ex2, wf4(:,14), wf8(:,13), n3(:,25), t3x8(:,:,9))
  call vert_VQ_A(ntry, ex6, wf4(:,15), wf8(:,14), n3(:,26), t3x8(:,:,10))
  call prop_A_Q(ntry, wf8(:,13), Q(:,14), ZERO, 0_intkind1, wf8(:,15), n2(13))
  call vert_AZ_Q(gZl,ntry, ex2, wf4(:,18), wf8(:,16), n3(:,27), t3x8(:,:,11))
  call prop_A_Q(ntry, wf8(:,16), Q(:,14), ZERO, 0_intkind1, wf8(:,17), n2(14))
  call vert_VQ_A(ntry, ex6, ex1, wf4(:,19), n3(:,28), t3x4(:,:,11))
  call vert_AV_Q(ntry, ex2, ex5, wf4(:,20), n3(:,29), t3x4(:,:,12))
  call prop_Q_A(ntry, wf4(:,19), Q(:,33), ZERO, 0_intkind1, wf4(:,21), n2(15))
  call prop_A_Q(ntry, wf4(:,20), Q(:,18), ZERO, 0_intkind1, wf4(:,22), n2(16))
  call vert_QA_V(ntry, wf4(:,21), wf4(:,22), wf16(:,7), n3(:,30), t3x16(:,:,7))
  call vert_QA_Z(gZl,ntry, wf4(:,21), wf4(:,22), wf16(:,8), n3(:,31), t3x16(:,:,8))
  call vert_VQ_A(ntry, wf4(:,14), ex1, wf8(:,18), n3(:,32), t3x8(:,:,12))
  call vert_AV_Q(ntry, wf4(:,22), ex6, wf8(:,19), n3(:,33), t3x8(:,:,13))
  call prop_Q_A(ntry, wf8(:,18), Q(:,13), ZERO, 0_intkind1, wf8(:,20), n2(17))
  call vert_ZQ_A(gZl,ntry, wf4(:,18), ex1, wf8(:,21), n3(:,34), t3x8(:,:,14))
  call prop_Q_A(ntry, wf8(:,21), Q(:,13), ZERO, 0_intkind1, wf8(:,22), n2(18))
  call vert_VQ_A(ntry, ex5, wf4(:,21), wf8(:,23), n3(:,35), t3x8(:,:,15))
  call vert_AV_Q(ntry, wf4(:,16), ex5, wf8(:,24), n3(:,36), t3x8(:,:,16))
  call vert_QA_V(ntry, wf4(:,15), ex2, wf8(:,25), n3(:,37), t3x8(:,:,17))
  call vert_QA_V(ntry, wf4(:,10), ex4, wf8(:,26), n3(:,38), t3x8(:,:,18))
  call vert_QA_Z(gZl,ntry, wf4(:,15), ex2, wf8(:,27), n3(:,39), t3x8(:,:,19))
  call vert_QA_Z(gZu,ntry, wf4(:,10), ex4, wf8(:,28), n3(:,40), t3x8(:,:,20))
  call prop_W_W(ntry, wf8(:,27), Q(:,19), MZ, 1_intkind1, wf8(:,29), n2(19))
  call vert_QA_V(ntry, ex3, wf4(:,5), wf8(:,30), n3(:,41), t3x8(:,:,21))
  call vert_QA_Z(gZu,ntry, ex3, wf4(:,5), wf8(:,31), n3(:,42), t3x8(:,:,22))
  call vert_QA_V(ntry, ex1, wf4(:,22), wf8(:,32), n3(:,43), t3x8(:,:,23))
  call vert_QA_Z(gZl,ntry, ex1, wf4(:,22), wf8(:,33), n3(:,44), t3x8(:,:,24))
  call prop_W_W(ntry, wf8(:,33), Q(:,19), MZ, 1_intkind1, wf8(:,34), n2(20))
  call vert_QA_V(ntry, wf4(:,21), ex2, wf8(:,35), n3(:,45), t3x8(:,:,25))
  call vert_QA_V(ntry, wf4(:,4), ex4, wf8(:,36), n3(:,46), t3x8(:,:,26))
  call vert_QA_Z(gZl,ntry, wf4(:,21), ex2, wf8(:,37), n3(:,47), t3x8(:,:,27))
  call vert_QA_Z(gZu,ntry, wf4(:,4), ex4, wf8(:,38), n3(:,48), t3x8(:,:,28))
  call prop_W_W(ntry, wf8(:,37), Q(:,35), MZ, 1_intkind1, wf8(:,39), n2(21))
  call vert_QA_V(ntry, ex1, wf4(:,16), wf8(:,40), n3(:,49), t3x8(:,:,29))
  call vert_QA_Z(gZl,ntry, ex1, wf4(:,16), wf8(:,41), n3(:,50), t3x8(:,:,30))
  call prop_W_W(ntry, wf8(:,41), Q(:,35), MZ, 1_intkind1, wf8(:,42), n2(22))
  call vert_QA_V(ntry, ex3, wf4(:,11), wf8(:,43), n3(:,51), t3x8(:,:,31))
  call vert_QA_Z(gZu,ntry, ex3, wf4(:,11), wf8(:,44), n3(:,52), t3x8(:,:,32))


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

    M2munu = M2munu / average_factor_ppllaa_eexuuxaa_1
  end if

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2add = M2 / average_factor_ppllaa_eexuuxaa_1

  do k = 0, 23-1
    M2(k) = M2add(extcomb_perm_ppllaa_eexuuxaa_1(k))
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

    call cont_QA(nsync, wf4(:,5), wf16(:,1), A(:,1), n3(:,53), t3x64(:,:,1), nhel, den(5))
    call cont_QA(nsync, wf4(:,5), wf16(:,2), A(:,2), n3(:,54), t3x64(:,:,2), nhel, den(8))
    call cont_QA(nsync, wf8(:,2), wf8(:,3), A(:,3), n3(:,55), t3x64(:,:,3), nhel, den(11))
    call cont_QA(nsync, wf8(:,2), wf8(:,5), A(:,4), n3(:,56), t3x64(:,:,4), nhel, den(13))
    call cont_QA(nsync, wf4(:,11), wf16(:,3), A(:,5), n3(:,57), t3x64(:,:,5), nhel, den(17))
    call cont_QA(nsync, wf4(:,11), wf16(:,4), A(:,6), n3(:,58), t3x64(:,:,6), nhel, den(19))
    call cont_QA(nsync, wf8(:,7), wf8(:,8), A(:,7), n3(:,59), t3x64(:,:,7), nhel, den(22))
    call cont_QA(nsync, wf8(:,7), wf8(:,10), A(:,8), n3(:,60), t3x64(:,:,8), nhel, den(24))
    call cont_QA(nsync, wf8(:,3), wf8(:,11), A(:,9), n3(:,61), t3x64(:,:,9), nhel, den(25))
    call cont_QA(nsync, wf8(:,5), wf8(:,11), A(:,10), n3(:,62), t3x64(:,:,10), nhel, den(26))
    call cont_QA(nsync, wf8(:,8), wf8(:,12), A(:,11), n3(:,63), t3x64(:,:,11), nhel, den(27))
    call cont_QA(nsync, wf8(:,10), wf8(:,12), A(:,12), n3(:,64), t3x64(:,:,12), nhel, den(28))
    call cont_VV(nsync, wf4(:,14), wf16(:,5), A(:,13), n3(:,65), t3x64(:,:,13), nhel, den(33))
    call cont_VV(nsync, wf4(:,18), wf16(:,6), A(:,14), n3(:,66), t3x64(:,:,14), nhel, den(35))
    call cont_QA(nsync, wf8(:,14), wf8(:,15), A(:,15), n3(:,67), t3x64(:,:,15), nhel, den(38))
    call cont_QA(nsync, wf8(:,14), wf8(:,17), A(:,16), n3(:,68), t3x64(:,:,16), nhel, den(40))
    call cont_VV(nsync, wf4(:,14), wf16(:,7), A(:,17), n3(:,69), t3x64(:,:,17), nhel, den(44))
    call cont_VV(nsync, wf4(:,18), wf16(:,8), A(:,18), n3(:,70), t3x64(:,:,18), nhel, den(45))
    call cont_QA(nsync, wf8(:,19), wf8(:,20), A(:,19), n3(:,71), t3x64(:,:,19), nhel, den(48))
    call cont_QA(nsync, wf8(:,19), wf8(:,22), A(:,20), n3(:,72), t3x64(:,:,20), nhel, den(50))
    call cont_QA(nsync, wf8(:,15), wf8(:,23), A(:,21), n3(:,73), t3x64(:,:,21), nhel, den(51))
    call cont_QA(nsync, wf8(:,17), wf8(:,23), A(:,22), n3(:,74), t3x64(:,:,22), nhel, den(52))
    call cont_QA(nsync, wf8(:,20), wf8(:,24), A(:,23), n3(:,75), t3x64(:,:,23), nhel, den(53))
    call cont_QA(nsync, wf8(:,22), wf8(:,24), A(:,24), n3(:,76), t3x64(:,:,24), nhel, den(54))
    call cont_VV(nsync, wf8(:,25), wf8(:,26), A(:,25), n3(:,77), t3x64(:,:,25), nhel, den(57))
    call cont_VV(nsync, wf8(:,28), wf8(:,29), A(:,26), n3(:,78), t3x64(:,:,26), nhel, den(60))
    call cont_VV(nsync, wf8(:,25), wf8(:,30), A(:,27), n3(:,79), t3x64(:,:,27), nhel, den(61))
    call cont_VV(nsync, wf8(:,29), wf8(:,31), A(:,28), n3(:,80), t3x64(:,:,28), nhel, den(62))
    call cont_VV(nsync, wf8(:,26), wf8(:,32), A(:,29), n3(:,81), t3x64(:,:,29), nhel, den(64))
    call cont_VV(nsync, wf8(:,28), wf8(:,34), A(:,30), n3(:,82), t3x64(:,:,30), nhel, den(66))
    call cont_VV(nsync, wf8(:,30), wf8(:,32), A(:,31), n3(:,83), t3x64(:,:,31), nhel, den(67))
    call cont_VV(nsync, wf8(:,31), wf8(:,34), A(:,32), n3(:,84), t3x64(:,:,32), nhel, den(68))
    call cont_VV(nsync, wf8(:,35), wf8(:,36), A(:,33), n3(:,85), t3x64(:,:,33), nhel, den(71))
    call cont_VV(nsync, wf8(:,38), wf8(:,39), A(:,34), n3(:,86), t3x64(:,:,34), nhel, den(74))
    call cont_VV(nsync, wf8(:,36), wf8(:,40), A(:,35), n3(:,87), t3x64(:,:,35), nhel, den(76))
    call cont_VV(nsync, wf8(:,38), wf8(:,42), A(:,36), n3(:,88), t3x64(:,:,36), nhel, den(78))
    call cont_VV(nsync, wf8(:,35), wf8(:,43), A(:,37), n3(:,89), t3x64(:,:,37), nhel, den(79))
    call cont_VV(nsync, wf8(:,39), wf8(:,44), A(:,38), n3(:,90), t3x64(:,:,38), nhel, den(80))
    call cont_VV(nsync, wf8(:,40), wf8(:,43), A(:,39), n3(:,91), t3x64(:,:,39), nhel, den(81))
    call cont_VV(nsync, wf8(:,42), wf8(:,44), A(:,40), n3(:,92), t3x64(:,:,40), nhel, den(82))

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
  complex(REALKIND), intent(out) :: M1(1) ! M1helarray(1,64)
  integer :: empty(0)

  M1(1) = (-A(j,1)%j-A(j,3)%j-A(j,5)%j-A(j,7)%j-A(j,9)%j-A(j,11)%j)*f(1)+(A(j,2)%j+A(j,4)%j+A(j,6)%j+A(j,8)%j+A(j,10)%j+A(j,12)%j &
       +A(j,25)%j+A(j,27)%j+A(j,29)%j+A(j,31)%j+A(j,33)%j+A(j,35)%j+A(j,37)%j+A(j,39)%j)*f(2)+(-A(j,13)%j-A(j,15)%j-A(j,17)%j &
       -A(j,19)%j-A(j,21)%j-A(j,23)%j-A(j,26)%j-A(j,28)%j-A(j,30)%j-A(j,32)%j-A(j,34)%j-A(j,36)%j-A(j,38)%j-A(j,40)%j)*f(3) &
       +(A(j,14)%j+A(j,16)%j+A(j,18)%j+A(j,20)%j+A(j,22)%j+A(j,24)%j)*f(4)

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
  use ol_colourmatrix_ppllaa_eexuuxaa_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(1)
  real(REALKIND),    intent(out) :: M2colint(0:23-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 1*extcomb
    do i = 1, 1
      do j = 1, 1
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
  use ol_colourmatrix_ppllaa_eexuuxaa_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(1), M2(1)
  real(REALKIND),    intent(out) :: M2colint(0:23-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 1*extcomb
    do i = 1, 1
      do j = 1, 1
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
  use ol_colourmatrix_ppllaa_eexuuxaa_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(1)
  complex(REALKIND), intent(in)  :: M2(1)
  complex(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 1
    do j = 1, 1
      M2colint = M2colint + M1(i)*K1(i,j)*conjg(M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_ppllaa_eexuuxaa_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(1,64)
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
    & bind(c,name="ol_f_amp2tree_ppllaa_eexuuxaa_1")
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
    & bind(c,name="ol_f_amp2ccone_ppllaa_eexuuxaa_1")
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
    & bind(c,name="ol_f_amp2ccall_ppllaa_eexuuxaa_1")
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
    & bind(c,name="ol_f_amp2hcone_ppllaa_eexuuxaa_1")
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
    & bind(c,name="ol_f_amp2hcall_ppllaa_eexuuxaa_1")
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
    & bind(c,name="ol_amp2tree_ppllaa_eexuuxaa_1")
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
    & bind(c,name="ol_amp2ccone_ppllaa_eexuuxaa_1")
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
    & bind(c,name="ol_amp2ccall_ppllaa_eexuuxaa_1")
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
    & bind(c,name="ol_amp2hcone_ppllaa_eexuuxaa_1")
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
    & bind(c,name="ol_amp2hcall_ppllaa_eexuuxaa_1")
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
    & bind(c,name="amp2tree_ppllaa_eexuuxaa_1_")
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
    & bind(c,name="amp2ccone_ppllaa_eexuuxaa_1_")
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
    & bind(c,name="amp2ccall_ppllaa_eexuuxaa_1_")
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
    & bind(c,name="amp2hcone_ppllaa_eexuuxaa_1_")
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
    & bind(c,name="amp2hcall_ppllaa_eexuuxaa_1_")
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

end module ol_tree_ppllaa_eexuuxaa_1_/**/REALKIND
