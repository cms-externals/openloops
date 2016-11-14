
module ol_colourmatrix_pptllj_eexutxdxbg_1_/**/REALKIND
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

  K1( 1,:) = [  12,   0]
  K1( 2,:) = [   0,  12]
  K1( 3,:) = [   0,   0]
  K1( 4,:) = [   0,   0]
  K1( 5,:) = [   0,   0]
  K1( 6,:) = [   0,   0]
  K1( 7,:) = [   0,   0]
  K1( 8,:) = [   0,   0]
  K1( 9,:) = [   0,   0]
  K1(10,:) = [   0,   0]
  K1(11,:) = [   0,   0]
  K1(12,:) = [   0,   0]
  K1(13,:) = [  16,   0]
  K1(14,:) = [   0,  16]
  K1(15,:) = [   0,   0]
  K1(16,:) = [   0,   0]
  K1(17,:) = [   0,   0]
  K1(18,:) = [   0,   0]
  K1(19,:) = [   0,  -2]
  K1(20,:) = [  -2,   0]
  K1(21,:) = [  16,   0]
  K1(22,:) = [   0,  16]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [ -16,   0]
  K1(28,:) = [   0,   2]
  K1(29,:) = [   0,   2]
  K1(30,:) = [   2,   0]
  K1(31,:) = [  16,   0]
  K1(32,:) = [   0,  16]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   2]
  K1(38,:) = [   2,   0]
  K1(39,:) = [   2,   0]
  K1(40,:) = [   0, -16]
  K1(41,:) = [   0,  -2]
  K1(42,:) = [  -2,   0]
  K1(43,:) = [  16,   0]
  K1(44,:) = [   0,  16]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]
  K1(47,:) = [   0,   0]
  K1(48,:) = [   0,   0]
  K1(49,:) = [   0,   0]
  K1(50,:) = [   0, -18]
  K1(51,:) = [ -18,   0]
  K1(52,:) = [   0,   0]
  K1(53,:) = [   0,   0]
  K1(54,:) = [   0, -18]
  K1(55,:) = [ -18,   0]
  K1(56,:) = [   0,   0]
  K1(57,:) = [  36,   0]
  K1(58,:) = [   0,  36]
  K1(59,:) = [   0,   0]
  K1(60,:) = [   0,   0]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pptllj_eexutxdxbg_1_/**/REALKIND



module ol_forced_parameters_pptllj_eexutxdxbg_1_/**/REALKIND
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
  if (wME /= 0) write(*,101) 'wME = 0'
  if (wME /= 0) write(*,101) 'wME = 0'
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pptllj_eexutxdxbg_1_/**/REALKIND

module ol_tree_pptllj_eexutxdxbg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(11)
  complex(REALKIND), save :: den(127)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 128 ! number of helicity configurations
  integer(intkind2), save :: nhel = 128 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(128) ! physical helicity states
  complex(DREALKIND) :: M1helarr(2,128) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED**4*gQCD)/(4._/**/REALKIND*sw**4)
    f( 2) = (CI*eQED**4*gQCD*ME)/(4._/**/REALKIND*sw**4)
    f( 3) = (CI*eQED**4*gQCD*ME)/(8._/**/REALKIND*MW**2*sw**4)
    f( 4) = (CI*eQED**4*gQCD*ME)/(4._/**/REALKIND*MW**2*sw**4)
    f( 5) = (CI*eQED**4*gQCD*MB*ME)/(8._/**/REALKIND*MW**2*sw**4)
    f( 6) = (CI*eQED**4*gQCD*ME*MT)/(8._/**/REALKIND*MW**2*sw**4)
    f( 7) = (CI*cw*eQED**4*gQCD)/(2._/**/REALKIND*sw**3)
    f( 8) = (CI*eQED**4*gQCD)/(6._/**/REALKIND*sw**2)
    f( 9) = (CI*eQED**4*gQCD)/(3._/**/REALKIND*sw**2)
    f(10) = (CI*eQED**4*gQCD)/(2._/**/REALKIND*sw**2)
    f(11) = (CI*eQED**4*gQCD)/(2._/**/REALKIND*cw*sw)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,3) - MH2)
  den(2) = 1 / (Q(5,20) - MW2)
  den(3) = 1 / (Q(5,72) - MT2)
  den(5) = 1 / (Q(5,35) - MB2)
  den(8) = 1 / (Q(5,3) - MZ2)
  den(11) = 1 / (Q(5,3))
  den(15) = 1 / (Q(5,52) - MT2)
  den(23) = 1 / (Q(5,104) - MW2)
  den(30) = 1 / (Q(5,96) - MB2)
  den(32) = 1 / (Q(5,11) - MT2)
  den(40) = 1 / (Q(5,28) - MB2)
  den(57) = 1 / (Q(5,68))
  den(58) = 1 / (Q(5,40) - MW2)
  den(60) = 1 / (Q(5,19))
  den(66) = 1 / (Q(5,84) - MW2)
  den(74) = 1 / (Q(5,56))
  den(79) = 1 / (Q(5,80))
  den(81) = 1 / (Q(5,7))
  den(87) = 1 / (Q(5,44))
  den(120) = 1 / (Q(5,21))
  den(124) = 1 / (Q(5,42))

  ! denominators

  den(4) = den(2)*den(3)
  den(6) = den(1)*den(5)
  den(7) = den(4)*den(6)
  den(9) = den(5)*den(8)
  den(10) = den(4)*den(9)
  den(12) = den(5)*den(11)
  den(13) = den(4)*den(12)
  den(14) = den(1)*den(3)
  den(16) = den(2)*den(15)
  den(17) = den(14)*den(16)
  den(18) = den(3)*den(8)
  den(19) = den(16)*den(18)
  den(20) = den(3)*den(11)
  den(21) = den(16)*den(20)
  den(22) = den(1)*den(2)
  den(24) = den(3)*den(23)
  den(25) = den(22)*den(24)
  den(26) = den(2)*den(8)
  den(27) = den(24)*den(26)
  den(28) = den(2)*den(11)
  den(29) = den(24)*den(28)
  den(31) = den(2)*den(30)
  den(33) = den(1)*den(32)
  den(34) = den(31)*den(33)
  den(35) = den(8)*den(32)
  den(36) = den(31)*den(35)
  den(37) = den(11)*den(32)
  den(38) = den(31)*den(37)
  den(39) = den(1)*den(30)
  den(41) = den(2)*den(40)
  den(42) = den(39)*den(41)
  den(43) = den(8)*den(30)
  den(44) = den(41)*den(43)
  den(45) = den(11)*den(30)
  den(46) = den(41)*den(45)
  den(47) = den(23)*den(30)
  den(48) = den(22)*den(47)
  den(49) = den(26)*den(47)
  den(50) = den(28)*den(47)
  den(51) = den(16)*den(33)
  den(52) = den(16)*den(35)
  den(53) = den(16)*den(37)
  den(54) = den(6)*den(41)
  den(55) = den(9)*den(41)
  den(56) = den(12)*den(41)
  den(59) = den(57)*den(58)
  den(61) = den(11)*den(60)
  den(62) = den(59)*den(61)
  den(63) = den(8)*den(60)
  den(64) = den(59)*den(63)
  den(65) = den(1)*den(58)
  den(67) = den(57)*den(66)
  den(68) = den(65)*den(67)
  den(69) = den(8)*den(58)
  den(70) = den(67)*den(69)
  den(71) = den(11)*den(58)
  den(72) = den(67)*den(71)
  den(73) = den(11)*den(57)
  den(75) = den(58)*den(74)
  den(76) = den(73)*den(75)
  den(77) = den(8)*den(57)
  den(78) = den(75)*den(77)
  den(80) = den(58)*den(79)
  den(82) = den(11)*den(81)
  den(83) = den(80)*den(82)
  den(84) = den(8)*den(81)
  den(85) = den(80)*den(84)
  den(86) = den(11)*den(79)
  den(88) = den(58)*den(87)
  den(89) = den(86)*den(88)
  den(90) = den(8)*den(79)
  den(91) = den(88)*den(90)
  den(92) = den(66)*den(79)
  den(93) = den(65)*den(92)
  den(94) = den(69)*den(92)
  den(95) = den(71)*den(92)
  den(96) = den(75)*den(82)
  den(97) = den(75)*den(84)
  den(98) = den(61)*den(88)
  den(99) = den(63)*den(88)
  den(100) = den(33)*den(67)
  den(101) = den(35)*den(67)
  den(102) = den(37)*den(67)
  den(103) = den(6)*den(67)
  den(104) = den(9)*den(67)
  den(105) = den(12)*den(67)
  den(106) = den(24)*den(82)
  den(107) = den(24)*den(84)
  den(108) = den(24)*den(61)
  den(109) = den(24)*den(63)
  den(110) = den(33)*den(92)
  den(111) = den(35)*den(92)
  den(112) = den(37)*den(92)
  den(113) = den(6)*den(92)
  den(114) = den(9)*den(92)
  den(115) = den(12)*den(92)
  den(116) = den(47)*den(82)
  den(117) = den(47)*den(84)
  den(118) = den(47)*den(61)
  den(119) = den(47)*den(63)
  den(121) = den(2)*den(120)
  den(122) = den(24)*den(121)
  den(123) = den(47)*den(121)
  den(125) = den(58)*den(124)
  den(126) = den(67)*den(125)
  den(127) = den(92)*den(125)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_pptllj_eexutxdxbg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_pptllj_eexutxdxbg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for e- e+ up anti-top anti-down bottom glue -> 0
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
  use ol_external_pptllj_eexutxdxbg_1, only: external_perm_pptllj_eexutxdxbg_1, &
    & external_perm_inv_pptllj_eexutxdxbg_1, extcomb_perm_pptllj_eexutxdxbg_1, &
    & average_factor_pptllj_eexutxdxbg_1
  use ol_external_pptllj_eexutxdxbg_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_pptllj_eexutxdxbg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_pptllj_eexutxdxbg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_pptllj_eexutxdxbg_1
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
  complex(REALKIND) :: MOM_LC(4), M1(2), M1helarray(2,128)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(2), ex7(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,15), wf8(8,44), wf16(16,54), wf128(128,96)

  type(polcont) :: A(128,96)
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
  extmasses2 = [ rME2, rME2, rZERO2, rMT2, rZERO2, rMB2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_pptllj_eexutxdxbg_1,7)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,7)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_pptllj_eexutxdxbg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_pptllj_eexutxdxbg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_pptllj_eexutxdxbg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rME, H1, ex1)
  call wf_A(P(:,2), rME, H2, ex2)
  call wf_Q(P(:,3), rZERO, H3, ex3)
  call wf_A(P(:,4), rMT, H4, ex4)
  call wf_A(P(:,5), rZERO, H5, ex5)
  call wf_Q(P(:,6), rMB, H6, ex6)
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
  call vert_AQ_S(gH,ntry, ex2, ex1, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_QA_W(ntry, ex3, ex5, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_AV_Q(ntry, ex4, ex7, wf4(:,3), n3(:,3), t3x4(:,:,3))
  call prop_A_Q(ntry, wf4(:,3), Q(:,72), MT, 1_intkind1, wf4(:,4), n2(1))
  call vert_QS_A(gH,ntry, ex6, wf4(:,1), wf8(:,1), n3(:,4), t3x8(:,:,1))
  call vert_AW_Q(ntry, wf4(:,4), wf4(:,2), wf16(:,1), n3(:,5), t3x16(:,:,1))
  call prop_Q_A(ntry, wf8(:,1), Q(:,35), MB, 1_intkind1, wf8(:,2), n2(2))
  call vert_AQ_S(gX,ntry, ex2, ex1, wf4(:,5), n3(:,6), t3x4(:,:,4))
  call vert_QS_A(gX,ntry, ex6, wf4(:,5), wf8(:,3), n3(:,7), t3x8(:,:,2))
  call prop_Q_A(ntry, wf8(:,3), Q(:,35), MB, 1_intkind1, wf8(:,4), n2(3))
  call vert_QA_V(ntry, ex1, ex2, wf4(:,6), n3(:,8), t3x4(:,:,5))
  call vert_VQ_A(ntry, wf4(:,6), ex6, wf8(:,5), n3(:,9), t3x8(:,:,3))
  call prop_Q_A(ntry, wf8(:,5), Q(:,35), MB, 1_intkind1, wf8(:,6), n2(4))
  call vert_QA_Z(gZl,ntry, ex1, ex2, wf4(:,7), n3(:,10), t3x4(:,:,6))
  call vert_ZQ_A(gZd,ntry, wf4(:,7), ex6, wf8(:,7), n3(:,11), t3x8(:,:,4))
  call prop_Q_A(ntry, wf8(:,7), Q(:,35), MB, 1_intkind1, wf8(:,8), n2(5))
  call vert_WQ_A(ntry, wf4(:,2), ex6, wf8(:,9), n3(:,12), t3x8(:,:,5))
  call vert_SA_Q(gH,ntry, wf4(:,1), wf4(:,4), wf16(:,2), n3(:,13), t3x16(:,:,2))
  call prop_Q_A(ntry, wf8(:,9), Q(:,52), MT, 1_intkind1, wf8(:,10), n2(6))
  call vert_SA_Q(gX,ntry, wf4(:,5), wf4(:,4), wf16(:,3), n3(:,14), t3x16(:,:,3))
  call vert_AV_Q(ntry, wf4(:,4), wf4(:,6), wf16(:,4), n3(:,15), t3x16(:,:,4))
  call vert_AZ_Q(gZu,ntry, wf4(:,4), wf4(:,7), wf16(:,5), n3(:,16), t3x16(:,:,5))
  call vert_AQ_S(gPtb,ntry, wf4(:,4), ex6, wf8(:,11), n3(:,17), t3x8(:,:,6))
  call vert_TV_S(ntry, wf4(:,1), Q(:,3), wf4(:,2), Q(:,20), wf16(:,6), n3(:,18), t3x16(:,:,6))
  call vert_TV_S(ntry, wf4(:,5), Q(:,3), wf4(:,2), Q(:,20), wf16(:,7), n3(:,19), t3x16(:,:,7))
  call vert_QA_W(ntry, ex6, wf4(:,4), wf8(:,12), n3(:,20), t3x8(:,:,7))
  call vert_SV_V(ntry, wf4(:,1), wf4(:,2), wf16(:,8), n3(:,21), t3x16(:,:,8))
  call vert_VV_S(ntry, wf4(:,6), wf4(:,2), wf16(:,9), n3(:,22), t3x16(:,:,9))
  call vert_VV_S(ntry, wf4(:,7), wf4(:,2), wf16(:,10), n3(:,23), t3x16(:,:,10))
  call vert_UV_W(ntry, wf4(:,6), Q(:,3), wf4(:,2), Q(:,20), wf16(:,11), n3(:,24), t3x16(:,:,11))
  call vert_UV_W(ntry, wf4(:,7), Q(:,3), wf4(:,2), Q(:,20), wf16(:,12), n3(:,25), t3x16(:,:,12))
  call vert_VQ_A(ntry, ex7, ex6, wf4(:,8), n3(:,26), t3x4(:,:,7))
  call prop_Q_A(ntry, wf4(:,8), Q(:,96), MB, 1_intkind1, wf4(:,9), n2(7))
  call vert_SA_Q(gH,ntry, wf4(:,1), ex4, wf8(:,13), n3(:,27), t3x8(:,:,8))
  call vert_WQ_A(ntry, wf4(:,2), wf4(:,9), wf16(:,13), n3(:,28), t3x16(:,:,13))
  call prop_A_Q(ntry, wf8(:,13), Q(:,11), MT, 1_intkind1, wf8(:,14), n2(8))
  call vert_SA_Q(gX,ntry, wf4(:,5), ex4, wf8(:,15), n3(:,29), t3x8(:,:,9))
  call prop_A_Q(ntry, wf8(:,15), Q(:,11), MT, 1_intkind1, wf8(:,16), n2(9))
  call vert_AV_Q(ntry, ex4, wf4(:,6), wf8(:,17), n3(:,30), t3x8(:,:,10))
  call prop_A_Q(ntry, wf8(:,17), Q(:,11), MT, 1_intkind1, wf8(:,18), n2(10))
  call vert_AZ_Q(gZu,ntry, ex4, wf4(:,7), wf8(:,19), n3(:,31), t3x8(:,:,11))
  call prop_A_Q(ntry, wf8(:,19), Q(:,11), MT, 1_intkind1, wf8(:,20), n2(11))
  call vert_AW_Q(ntry, ex4, wf4(:,2), wf8(:,21), n3(:,32), t3x8(:,:,12))
  call vert_QS_A(gH,ntry, wf4(:,9), wf4(:,1), wf16(:,14), n3(:,33), t3x16(:,:,14))
  call prop_A_Q(ntry, wf8(:,21), Q(:,28), MB, 1_intkind1, wf8(:,22), n2(12))
  call vert_QS_A(gX,ntry, wf4(:,9), wf4(:,5), wf16(:,15), n3(:,34), t3x16(:,:,15))
  call vert_VQ_A(ntry, wf4(:,6), wf4(:,9), wf16(:,16), n3(:,35), t3x16(:,:,16))
  call vert_ZQ_A(gZd,ntry, wf4(:,7), wf4(:,9), wf16(:,17), n3(:,36), t3x16(:,:,17))
  call vert_AQ_S(gPtb,ntry, ex4, wf4(:,9), wf8(:,23), n3(:,37), t3x8(:,:,13))
  call vert_QA_W(ntry, wf4(:,9), ex4, wf8(:,24), n3(:,38), t3x8(:,:,14))
  call vert_AV_Q(ntry, wf8(:,14), ex7, wf16(:,18), n3(:,39), t3x16(:,:,18))
  call vert_AV_Q(ntry, wf8(:,16), ex7, wf16(:,19), n3(:,40), t3x16(:,:,19))
  call vert_AV_Q(ntry, wf8(:,18), ex7, wf16(:,20), n3(:,41), t3x16(:,:,20))
  call vert_AV_Q(ntry, wf8(:,20), ex7, wf16(:,21), n3(:,42), t3x16(:,:,21))
  call vert_AV_Q(ntry, wf8(:,22), ex7, wf16(:,22), n3(:,43), t3x16(:,:,22))
  call vert_VQ_A(ntry, ex7, ex3, wf4(:,10), n3(:,44), t3x4(:,:,8))
  call vert_QA_W(ntry, ex6, ex4, wf4(:,11), n3(:,45), t3x4(:,:,9))
  call prop_Q_A(ntry, wf4(:,10), Q(:,68), ZERO, 0_intkind1, wf4(:,12), n2(13))
  call vert_AV_Q(ntry, ex5, wf4(:,6), wf8(:,25), n3(:,46), t3x8(:,:,15))
  call vert_WQ_A(ntry, wf4(:,11), wf4(:,12), wf16(:,23), n3(:,47), t3x16(:,:,23))
  call prop_A_Q(ntry, wf8(:,25), Q(:,19), ZERO, 0_intkind1, wf8(:,26), n2(14))
  call vert_AZ_Q(gZd,ntry, ex5, wf4(:,7), wf8(:,27), n3(:,48), t3x8(:,:,16))
  call prop_A_Q(ntry, wf8(:,27), Q(:,19), ZERO, 0_intkind1, wf8(:,28), n2(15))
  call vert_AQ_S(gPtb,ntry, ex4, ex6, wf4(:,13), n3(:,49), t3x4(:,:,10))
  call vert_QA_W(ntry, wf4(:,12), ex5, wf8(:,29), n3(:,50), t3x8(:,:,17))
  call vert_ST_V(ntry, wf4(:,13), Q(:,40), wf4(:,1), Q(:,3), wf16(:,24), n3(:,51), t3x16(:,:,24))
  call vert_ST_V(ntry, wf4(:,13), Q(:,40), wf4(:,5), Q(:,3), wf16(:,25), n3(:,52), t3x16(:,:,25))
  call vert_SV_V(ntry, wf4(:,1), wf4(:,11), wf16(:,26), n3(:,53), t3x16(:,:,26))
  call vert_SV_V(ntry, wf4(:,13), wf4(:,6), wf16(:,27), n3(:,54), t3x16(:,:,27))
  call vert_SV_V(ntry, wf4(:,13), wf4(:,7), wf16(:,28), n3(:,55), t3x16(:,:,28))
  call vert_UV_W(ntry, wf4(:,11), Q(:,40), wf4(:,6), Q(:,3), wf16(:,29), n3(:,56), t3x16(:,:,29))
  call vert_UV_W(ntry, wf4(:,11), Q(:,40), wf4(:,7), Q(:,3), wf16(:,30), n3(:,57), t3x16(:,:,30))
  call vert_AW_Q(ntry, ex5, wf4(:,11), wf8(:,30), n3(:,58), t3x8(:,:,18))
  call vert_VQ_A(ntry, wf4(:,6), wf4(:,12), wf16(:,31), n3(:,59), t3x16(:,:,31))
  call prop_A_Q(ntry, wf8(:,30), Q(:,56), ZERO, 0_intkind1, wf8(:,31), n2(16))
  call vert_ZQ_A(gZu,ntry, wf4(:,7), wf4(:,12), wf16(:,32), n3(:,60), t3x16(:,:,32))
  call vert_AV_Q(ntry, ex5, ex7, wf4(:,14), n3(:,61), t3x4(:,:,11))
  call prop_A_Q(ntry, wf4(:,14), Q(:,80), ZERO, 0_intkind1, wf4(:,15), n2(17))
  call vert_VQ_A(ntry, wf4(:,6), ex3, wf8(:,32), n3(:,62), t3x8(:,:,19))
  call vert_AW_Q(ntry, wf4(:,15), wf4(:,11), wf16(:,33), n3(:,63), t3x16(:,:,33))
  call prop_Q_A(ntry, wf8(:,32), Q(:,7), ZERO, 0_intkind1, wf8(:,33), n2(18))
  call vert_ZQ_A(gZu,ntry, wf4(:,7), ex3, wf8(:,34), n3(:,64), t3x8(:,:,20))
  call prop_Q_A(ntry, wf8(:,34), Q(:,7), ZERO, 0_intkind1, wf8(:,35), n2(19))
  call vert_WQ_A(ntry, wf4(:,11), ex3, wf8(:,36), n3(:,65), t3x8(:,:,21))
  call vert_AV_Q(ntry, wf4(:,15), wf4(:,6), wf16(:,34), n3(:,66), t3x16(:,:,34))
  call prop_Q_A(ntry, wf8(:,36), Q(:,44), ZERO, 0_intkind1, wf8(:,37), n2(20))
  call vert_AZ_Q(gZd,ntry, wf4(:,15), wf4(:,7), wf16(:,35), n3(:,67), t3x16(:,:,35))
  call vert_QA_W(ntry, ex3, wf4(:,15), wf8(:,38), n3(:,68), t3x8(:,:,22))
  call vert_VQ_A(ntry, ex7, wf8(:,33), wf16(:,36), n3(:,69), t3x16(:,:,36))
  call vert_VQ_A(ntry, ex7, wf8(:,35), wf16(:,37), n3(:,70), t3x16(:,:,37))
  call vert_VQ_A(ntry, ex7, wf8(:,37), wf16(:,38), n3(:,71), t3x16(:,:,38))
  call vert_QA_W(ntry, ex6, wf8(:,14), wf16(:,39), n3(:,72), t3x16(:,:,39))
  call vert_QA_W(ntry, ex6, wf8(:,16), wf16(:,40), n3(:,73), t3x16(:,:,40))
  call vert_QA_W(ntry, ex6, wf8(:,18), wf16(:,41), n3(:,74), t3x16(:,:,41))
  call vert_QA_W(ntry, ex6, wf8(:,20), wf16(:,42), n3(:,75), t3x16(:,:,42))
  call vert_AW_Q(ntry, ex4, wf8(:,29), wf16(:,43), n3(:,76), t3x16(:,:,43))
  call vert_QA_W(ntry, wf8(:,33), ex5, wf16(:,44), n3(:,77), t3x16(:,:,44))
  call vert_QA_W(ntry, wf8(:,35), ex5, wf16(:,45), n3(:,78), t3x16(:,:,45))
  call vert_QA_W(ntry, ex3, wf8(:,26), wf16(:,46), n3(:,79), t3x16(:,:,46))
  call vert_QA_W(ntry, ex3, wf8(:,28), wf16(:,47), n3(:,80), t3x16(:,:,47))
  call vert_WQ_A(ntry, wf8(:,38), ex6, wf16(:,48), n3(:,81), t3x16(:,:,48))
  call vert_AW_Q(ntry, ex4, wf8(:,38), wf16(:,49), n3(:,82), t3x16(:,:,49))
  call vert_WQ_A(ntry, wf8(:,24), ex3, wf16(:,50), n3(:,83), t3x16(:,:,50))
  call vert_WQ_A(ntry, wf4(:,2), ex1, wf8(:,39), n3(:,84), t3x8(:,:,23))
  call prop_Q_A(ntry, wf8(:,39), Q(:,21), ZERO, 0_intkind1, wf8(:,40), n2(21))
  call vert_AQ_S(gPln,ntry, ex2, wf8(:,40), wf16(:,51), n3(:,85), t3x16(:,:,51))
  call vert_QA_W(ntry, wf8(:,40), ex2, wf16(:,52), n3(:,86), t3x16(:,:,52))
  call vert_SA_Q(gPln,ntry, wf4(:,13), ex2, wf8(:,41), n3(:,87), t3x8(:,:,24))
  call prop_A_Q(ntry, wf8(:,41), Q(:,42), ZERO, 0_intkind1, wf8(:,42), n2(22))
  call vert_QA_W(ntry, ex1, wf8(:,42), wf16(:,53), n3(:,88), t3x16(:,:,53))
  call vert_AW_Q(ntry, ex2, wf4(:,11), wf8(:,43), n3(:,89), t3x8(:,:,25))
  call prop_A_Q(ntry, wf8(:,43), Q(:,42), ZERO, 0_intkind1, wf8(:,44), n2(23))
  call vert_QA_W(ntry, ex1, wf8(:,44), wf16(:,54), n3(:,90), t3x16(:,:,54))


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
  M2add = M2 / average_factor_pptllj_eexutxdxbg_1

  do k = 0, 30-1
    M2(k) = M2add(extcomb_perm_pptllj_eexutxdxbg_1(k))
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

    call cont_QA(nsync, wf16(:,1), wf8(:,2), A(:,1), n3(:,91), t3x128(:,:,1), nhel, den(7))
    call cont_QA(nsync, wf16(:,1), wf8(:,4), A(:,2), n3(:,92), t3x128(:,:,2), nhel, den(10))
    call cont_QA(nsync, wf16(:,1), wf8(:,6), A(:,3), n3(:,93), t3x128(:,:,3), nhel, den(13))
    call cont_QA(nsync, wf16(:,1), wf8(:,8), A(:,4), n3(:,94), t3x128(:,:,4), nhel, den(10))
    call cont_QA(nsync, wf16(:,2), wf8(:,10), A(:,5), n3(:,95), t3x128(:,:,5), nhel, den(17))
    call cont_QA(nsync, wf8(:,10), wf16(:,3), A(:,6), n3(:,96), t3x128(:,:,6), nhel, den(19))
    call cont_QA(nsync, wf8(:,10), wf16(:,4), A(:,7), n3(:,97), t3x128(:,:,7), nhel, den(21))
    call cont_QA(nsync, wf8(:,10), wf16(:,5), A(:,8), n3(:,98), t3x128(:,:,8), nhel, den(19))
    call cont_SS(nsync, wf8(:,11), wf16(:,6), A(:,9), n3(:,99), t3x128(:,:,9), nhel, den(25))
    call cont_SS(nsync, wf8(:,11), wf16(:,7), A(:,10), n3(:,100), t3x128(:,:,10), nhel, den(27))
    call cont_VV(nsync, wf8(:,12), wf16(:,8), A(:,11), n3(:,101), t3x128(:,:,11), nhel, den(25))
    call cont_SS(nsync, wf8(:,11), wf16(:,9), A(:,12), n3(:,102), t3x128(:,:,12), nhel, den(29))
    call cont_SS(nsync, wf8(:,11), wf16(:,10), A(:,13), n3(:,103), t3x128(:,:,13), nhel, den(27))
    call cont_VV(nsync, wf8(:,12), wf16(:,11), A(:,14), n3(:,104), t3x128(:,:,14), nhel, den(29))
    call cont_VV(nsync, wf8(:,12), wf16(:,12), A(:,15), n3(:,105), t3x128(:,:,15), nhel, den(27))
    call cont_QA(nsync, wf16(:,13), wf8(:,14), A(:,16), n3(:,106), t3x128(:,:,16), nhel, den(34))
    call cont_QA(nsync, wf16(:,13), wf8(:,16), A(:,17), n3(:,107), t3x128(:,:,17), nhel, den(36))
    call cont_QA(nsync, wf16(:,13), wf8(:,18), A(:,18), n3(:,108), t3x128(:,:,18), nhel, den(38))
    call cont_QA(nsync, wf16(:,13), wf8(:,20), A(:,19), n3(:,109), t3x128(:,:,19), nhel, den(36))
    call cont_QA(nsync, wf16(:,14), wf8(:,22), A(:,20), n3(:,110), t3x128(:,:,20), nhel, den(42))
    call cont_QA(nsync, wf8(:,22), wf16(:,15), A(:,21), n3(:,111), t3x128(:,:,21), nhel, den(44))
    call cont_QA(nsync, wf8(:,22), wf16(:,16), A(:,22), n3(:,112), t3x128(:,:,22), nhel, den(46))
    call cont_QA(nsync, wf8(:,22), wf16(:,17), A(:,23), n3(:,113), t3x128(:,:,23), nhel, den(44))
    call cont_SS(nsync, wf16(:,6), wf8(:,23), A(:,24), n3(:,114), t3x128(:,:,24), nhel, den(48))
    call cont_SS(nsync, wf16(:,7), wf8(:,23), A(:,25), n3(:,115), t3x128(:,:,25), nhel, den(49))
    call cont_VV(nsync, wf16(:,8), wf8(:,24), A(:,26), n3(:,116), t3x128(:,:,26), nhel, den(48))
    call cont_SS(nsync, wf16(:,9), wf8(:,23), A(:,27), n3(:,117), t3x128(:,:,27), nhel, den(50))
    call cont_SS(nsync, wf16(:,10), wf8(:,23), A(:,28), n3(:,118), t3x128(:,:,28), nhel, den(49))
    call cont_VV(nsync, wf16(:,11), wf8(:,24), A(:,29), n3(:,119), t3x128(:,:,29), nhel, den(50))
    call cont_VV(nsync, wf16(:,12), wf8(:,24), A(:,30), n3(:,120), t3x128(:,:,30), nhel, den(49))
    call cont_QA(nsync, wf8(:,10), wf16(:,18), A(:,31), n3(:,121), t3x128(:,:,31), nhel, den(51))
    call cont_QA(nsync, wf8(:,10), wf16(:,19), A(:,32), n3(:,122), t3x128(:,:,32), nhel, den(52))
    call cont_QA(nsync, wf8(:,10), wf16(:,20), A(:,33), n3(:,123), t3x128(:,:,33), nhel, den(53))
    call cont_QA(nsync, wf8(:,10), wf16(:,21), A(:,34), n3(:,124), t3x128(:,:,34), nhel, den(52))
    call cont_QA(nsync, wf8(:,2), wf16(:,22), A(:,35), n3(:,125), t3x128(:,:,35), nhel, den(54))
    call cont_QA(nsync, wf8(:,4), wf16(:,22), A(:,36), n3(:,126), t3x128(:,:,36), nhel, den(55))
    call cont_QA(nsync, wf8(:,6), wf16(:,22), A(:,37), n3(:,127), t3x128(:,:,37), nhel, den(56))
    call cont_QA(nsync, wf8(:,8), wf16(:,22), A(:,38), n3(:,128), t3x128(:,:,38), nhel, den(55))
    call cont_QA(nsync, wf16(:,23), wf8(:,26), A(:,39), n3(:,129), t3x128(:,:,39), nhel, den(62))
    call cont_QA(nsync, wf16(:,23), wf8(:,28), A(:,40), n3(:,130), t3x128(:,:,40), nhel, den(64))
    call cont_VV(nsync, wf8(:,29), wf16(:,24), A(:,41), n3(:,131), t3x128(:,:,41), nhel, den(68))
    call cont_VV(nsync, wf8(:,29), wf16(:,25), A(:,42), n3(:,132), t3x128(:,:,42), nhel, den(70))
    call cont_VV(nsync, wf8(:,29), wf16(:,26), A(:,43), n3(:,133), t3x128(:,:,43), nhel, den(68))
    call cont_VV(nsync, wf8(:,29), wf16(:,27), A(:,44), n3(:,134), t3x128(:,:,44), nhel, den(72))
    call cont_VV(nsync, wf8(:,29), wf16(:,28), A(:,45), n3(:,135), t3x128(:,:,45), nhel, den(70))
    call cont_VV(nsync, wf8(:,29), wf16(:,29), A(:,46), n3(:,136), t3x128(:,:,46), nhel, den(72))
    call cont_VV(nsync, wf8(:,29), wf16(:,30), A(:,47), n3(:,137), t3x128(:,:,47), nhel, den(70))
    call cont_QA(nsync, wf16(:,31), wf8(:,31), A(:,48), n3(:,138), t3x128(:,:,48), nhel, den(76))
    call cont_QA(nsync, wf8(:,31), wf16(:,32), A(:,49), n3(:,139), t3x128(:,:,49), nhel, den(78))
    call cont_QA(nsync, wf16(:,33), wf8(:,33), A(:,50), n3(:,140), t3x128(:,:,50), nhel, den(83))
    call cont_QA(nsync, wf16(:,33), wf8(:,35), A(:,51), n3(:,141), t3x128(:,:,51), nhel, den(85))
    call cont_QA(nsync, wf16(:,34), wf8(:,37), A(:,52), n3(:,142), t3x128(:,:,52), nhel, den(89))
    call cont_QA(nsync, wf8(:,37), wf16(:,35), A(:,53), n3(:,143), t3x128(:,:,53), nhel, den(91))
    call cont_VV(nsync, wf16(:,24), wf8(:,38), A(:,54), n3(:,144), t3x128(:,:,54), nhel, den(93))
    call cont_VV(nsync, wf16(:,25), wf8(:,38), A(:,55), n3(:,145), t3x128(:,:,55), nhel, den(94))
    call cont_VV(nsync, wf16(:,26), wf8(:,38), A(:,56), n3(:,146), t3x128(:,:,56), nhel, den(93))
    call cont_VV(nsync, wf16(:,27), wf8(:,38), A(:,57), n3(:,147), t3x128(:,:,57), nhel, den(95))
    call cont_VV(nsync, wf16(:,28), wf8(:,38), A(:,58), n3(:,148), t3x128(:,:,58), nhel, den(94))
    call cont_VV(nsync, wf16(:,29), wf8(:,38), A(:,59), n3(:,149), t3x128(:,:,59), nhel, den(95))
    call cont_VV(nsync, wf16(:,30), wf8(:,38), A(:,60), n3(:,150), t3x128(:,:,60), nhel, den(94))
    call cont_QA(nsync, wf8(:,31), wf16(:,36), A(:,61), n3(:,151), t3x128(:,:,61), nhel, den(96))
    call cont_QA(nsync, wf8(:,31), wf16(:,37), A(:,62), n3(:,152), t3x128(:,:,62), nhel, den(97))
    call cont_QA(nsync, wf8(:,26), wf16(:,38), A(:,63), n3(:,153), t3x128(:,:,63), nhel, den(98))
    call cont_QA(nsync, wf8(:,28), wf16(:,38), A(:,64), n3(:,154), t3x128(:,:,64), nhel, den(99))
    call cont_VV(nsync, wf8(:,29), wf16(:,39), A(:,65), n3(:,155), t3x128(:,:,65), nhel, den(100))
    call cont_VV(nsync, wf8(:,29), wf16(:,40), A(:,66), n3(:,156), t3x128(:,:,66), nhel, den(101))
    call cont_VV(nsync, wf8(:,29), wf16(:,41), A(:,67), n3(:,157), t3x128(:,:,67), nhel, den(102))
    call cont_VV(nsync, wf8(:,29), wf16(:,42), A(:,68), n3(:,158), t3x128(:,:,68), nhel, den(101))
    call cont_QA(nsync, wf8(:,2), wf16(:,43), A(:,69), n3(:,159), t3x128(:,:,69), nhel, den(103))
    call cont_QA(nsync, wf8(:,4), wf16(:,43), A(:,70), n3(:,160), t3x128(:,:,70), nhel, den(104))
    call cont_QA(nsync, wf8(:,6), wf16(:,43), A(:,71), n3(:,161), t3x128(:,:,71), nhel, den(105))
    call cont_QA(nsync, wf8(:,8), wf16(:,43), A(:,72), n3(:,162), t3x128(:,:,72), nhel, den(104))
    call cont_VV(nsync, wf8(:,12), wf16(:,44), A(:,73), n3(:,163), t3x128(:,:,73), nhel, den(106))
    call cont_VV(nsync, wf8(:,12), wf16(:,45), A(:,74), n3(:,164), t3x128(:,:,74), nhel, den(107))
    call cont_VV(nsync, wf8(:,12), wf16(:,46), A(:,75), n3(:,165), t3x128(:,:,75), nhel, den(108))
    call cont_VV(nsync, wf8(:,12), wf16(:,47), A(:,76), n3(:,166), t3x128(:,:,76), nhel, den(109))
    call cont_QA(nsync, wf8(:,14), wf16(:,48), A(:,77), n3(:,167), t3x128(:,:,77), nhel, den(110))
    call cont_QA(nsync, wf8(:,16), wf16(:,48), A(:,78), n3(:,168), t3x128(:,:,78), nhel, den(111))
    call cont_QA(nsync, wf8(:,18), wf16(:,48), A(:,79), n3(:,169), t3x128(:,:,79), nhel, den(112))
    call cont_QA(nsync, wf8(:,20), wf16(:,48), A(:,80), n3(:,170), t3x128(:,:,80), nhel, den(111))
    call cont_QA(nsync, wf8(:,2), wf16(:,49), A(:,81), n3(:,171), t3x128(:,:,81), nhel, den(113))
    call cont_QA(nsync, wf8(:,4), wf16(:,49), A(:,82), n3(:,172), t3x128(:,:,82), nhel, den(114))
    call cont_QA(nsync, wf8(:,6), wf16(:,49), A(:,83), n3(:,173), t3x128(:,:,83), nhel, den(115))
    call cont_QA(nsync, wf8(:,8), wf16(:,49), A(:,84), n3(:,174), t3x128(:,:,84), nhel, den(114))
    call cont_VV(nsync, wf8(:,24), wf16(:,44), A(:,85), n3(:,175), t3x128(:,:,85), nhel, den(116))
    call cont_VV(nsync, wf8(:,24), wf16(:,45), A(:,86), n3(:,176), t3x128(:,:,86), nhel, den(117))
    call cont_QA(nsync, wf8(:,26), wf16(:,50), A(:,87), n3(:,177), t3x128(:,:,87), nhel, den(118))
    call cont_QA(nsync, wf8(:,28), wf16(:,50), A(:,88), n3(:,178), t3x128(:,:,88), nhel, den(119))
    call cont_SS(nsync, wf8(:,11), wf16(:,51), A(:,89), n3(:,179), t3x128(:,:,89), nhel, den(122))
    call cont_VV(nsync, wf8(:,12), wf16(:,52), A(:,90), n3(:,180), t3x128(:,:,90), nhel, den(122))
    call cont_SS(nsync, wf8(:,23), wf16(:,51), A(:,91), n3(:,181), t3x128(:,:,91), nhel, den(123))
    call cont_VV(nsync, wf8(:,24), wf16(:,52), A(:,92), n3(:,182), t3x128(:,:,92), nhel, den(123))
    call cont_VV(nsync, wf8(:,29), wf16(:,53), A(:,93), n3(:,183), t3x128(:,:,93), nhel, den(126))
    call cont_VV(nsync, wf8(:,29), wf16(:,54), A(:,94), n3(:,184), t3x128(:,:,94), nhel, den(126))
    call cont_VV(nsync, wf8(:,38), wf16(:,53), A(:,95), n3(:,185), t3x128(:,:,95), nhel, den(127))
    call cont_VV(nsync, wf8(:,38), wf16(:,54), A(:,96), n3(:,186), t3x128(:,:,96), nhel, den(127))

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
  complex(REALKIND), intent(out) :: M1(2) ! M1helarray(2,128)
  integer :: empty(0)

  M1(1) = (A(j,90)%j+A(j,92)%j)*f(1)+(-A(j,11)%j-A(j,26)%j)*f(2)+(-A(j,9)%j-A(j,10)%j-A(j,24)%j-A(j,25)%j)*f(3)+(A(j,89)%j &
       +A(j,91)%j)*f(4)+(-A(j,1)%j+A(j,2)%j-A(j,20)%j+A(j,21)%j-A(j,35)%j+A(j,36)%j)*f(5)+(-A(j,5)%j-A(j,6)%j-A(j,16)%j-A(j,17)%j &
       -A(j,31)%j-A(j,32)%j)*f(6)+(A(j,15)%j+A(j,30)%j)*f(7)+(A(j,3)%j+A(j,22)%j+A(j,37)%j+A(j,75)%j+A(j,87)%j)*f(8)+(-A(j,7)%j &
       -A(j,18)%j-A(j,33)%j-A(j,73)%j-A(j,85)%j)*f(9)+(A(j,4)%j+A(j,8)%j-A(j,12)%j-A(j,14)%j+A(j,19)%j+A(j,23)%j-A(j,27)%j &
       -A(j,29)%j+A(j,34)%j+A(j,38)%j+A(j,74)%j+A(j,76)%j+A(j,86)%j+A(j,88)%j)*f(10)+(-A(j,13)%j-A(j,28)%j)*f(11)
  M1(2) = (A(j,94)%j+A(j,96)%j)*f(1)+(-A(j,43)%j-A(j,56)%j)*f(2)+(-A(j,41)%j-A(j,42)%j-A(j,54)%j-A(j,55)%j)*f(3)+(A(j,93)%j &
       +A(j,95)%j)*f(4)+(-A(j,69)%j+A(j,70)%j-A(j,81)%j+A(j,82)%j)*f(5)+(-A(j,65)%j-A(j,66)%j-A(j,77)%j-A(j,78)%j)*f(6)+(A(j,47)%j &
       +A(j,60)%j)*f(7)+(A(j,39)%j+A(j,52)%j+A(j,63)%j+A(j,71)%j+A(j,83)%j)*f(8)+(-A(j,48)%j-A(j,50)%j-A(j,61)%j-A(j,67)%j &
       -A(j,79)%j)*f(9)+(A(j,40)%j-A(j,44)%j-A(j,46)%j+A(j,49)%j+A(j,51)%j+A(j,53)%j-A(j,57)%j-A(j,59)%j+A(j,62)%j+A(j,64)%j &
       +A(j,68)%j+A(j,72)%j+A(j,80)%j+A(j,84)%j)*f(10)+(-A(j,45)%j-A(j,58)%j)*f(11)

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
  use ol_colourmatrix_pptllj_eexutxdxbg_1_/**/REALKIND, only: K1
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
  use ol_colourmatrix_pptllj_eexutxdxbg_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_pptllj_eexutxdxbg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(2,128)
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
    & bind(c,name="ol_f_amp2tree_pptllj_eexutxdxbg_1")
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
    & bind(c,name="ol_f_amp2ccone_pptllj_eexutxdxbg_1")
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
    & bind(c,name="ol_f_amp2ccall_pptllj_eexutxdxbg_1")
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
    & bind(c,name="ol_f_amp2hcone_pptllj_eexutxdxbg_1")
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
    & bind(c,name="ol_f_amp2hcall_pptllj_eexutxdxbg_1")
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
    & bind(c,name="ol_amp2tree_pptllj_eexutxdxbg_1")
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
    & bind(c,name="ol_amp2ccone_pptllj_eexutxdxbg_1")
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
    & bind(c,name="ol_amp2ccall_pptllj_eexutxdxbg_1")
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
    & bind(c,name="ol_amp2hcone_pptllj_eexutxdxbg_1")
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
    & bind(c,name="ol_amp2hcall_pptllj_eexutxdxbg_1")
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
    & bind(c,name="amp2tree_pptllj_eexutxdxbg_1_")
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
    & bind(c,name="amp2ccone_pptllj_eexutxdxbg_1_")
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
    & bind(c,name="amp2ccall_pptllj_eexutxdxbg_1_")
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
    & bind(c,name="amp2hcone_pptllj_eexutxdxbg_1_")
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
    & bind(c,name="amp2hcall_pptllj_eexutxdxbg_1_")
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

end module ol_tree_pptllj_eexutxdxbg_1_/**/REALKIND
