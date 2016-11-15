
module ol_colourmatrix_ppzwj_uxdzwxgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(46,2)
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
  K1(23,:) = [ -72,   9]
  K1(24,:) = [   9,   9]
  K1(25,:) = [   9,   9]
  K1(26,:) = [   9, -72]
  K1(27,:) = [   0,   0]
  K1(28,:) = [   0,   0]
  K1(29,:) = [   0,   0]
  K1(30,:) = [   0,   0]
  K1(31,:) = [ 144, -18]
  K1(32,:) = [ -18, 144]
  K1(33,:) = [   9,   9]
  K1(34,:) = [   9, -72]
  K1(35,:) = [ -72,   9]
  K1(36,:) = [   9,   9]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [ -81,   0]
  K1(42,:) = [   0, -81]
  K1(43,:) = [ 144, -18]
  K1(44,:) = [ -18, 144]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]
  K1 = (1._/**/REALKIND / 9) * K1

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppzwj_uxdzwxgg_1_/**/REALKIND



module ol_forced_parameters_ppzwj_uxdzwxgg_1_/**/REALKIND
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
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppzwj_uxdzwxgg_1_/**/REALKIND

module ol_tree_ppzwj_uxdzwxgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(4)
  complex(REALKIND), save :: den(84)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 144 ! number of helicity configurations
  integer(intkind2), save :: nhel = 144 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(144) ! physical helicity states
  complex(DREALKIND) :: M1helarr(2,144) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (CI*cw*eQED**2*gQCD**2)/(sqrt2*sw**2)
    f(2) = (cw*eQED**2*gQCD**2)/(sqrt2*sw**2)
    f(3) = (CI*eQED**2*gQCD**2)/(sqrt2*sw)
    f(4) = (eQED**2*gQCD**2)/(sqrt2*sw)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,5))
  den(2) = 1 / (Q(5,10))
  den(3) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,21))
  den(9) = 1 / (Q(5,26))
  den(12) = 1 / (Q(5,18))
  den(13) = 1 / (Q(5,13))
  den(18) = 1 / (Q(5,34))
  den(20) = 1 / (Q(5,42))
  den(23) = 1 / (Q(5,50))
  den(26) = 1 / (Q(5,9))
  den(27) = 1 / (Q(5,6))
  den(30) = 1 / (Q(5,25))
  den(33) = 1 / (Q(5,22))
  den(36) = 1 / (Q(5,17))
  den(39) = 1 / (Q(5,14))
  den(42) = 1 / (Q(5,33))
  den(43) = 1 / (Q(5,41))
  den(47) = 1 / (Q(5,49))
  den(55) = 1 / (Q(5,38))
  den(63) = 1 / (Q(5,37))
  den(68) = 1 / (Q(5,12) - MW2)

  ! denominators

  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(14) = den(1)*den(13)
  den(15) = den(12)*den(14)
  den(16) = den(9)*den(12)
  den(17) = den(1)*den(16)
  den(19) = den(14)*den(18)
  den(21) = den(18)*den(20)
  den(22) = den(1)*den(21)
  den(24) = den(3)*den(23)
  den(25) = den(1)*den(24)
  den(28) = den(26)*den(27)
  den(29) = den(3)*den(28)
  den(31) = den(26)*den(30)
  den(32) = den(27)*den(31)
  den(34) = den(27)*den(33)
  den(35) = den(26)*den(34)
  den(37) = den(30)*den(36)
  den(38) = den(27)*den(37)
  den(40) = den(27)*den(39)
  den(41) = den(36)*den(40)
  den(44) = den(42)*den(43)
  den(45) = den(27)*den(44)
  den(46) = den(40)*den(42)
  den(48) = den(3)*den(47)
  den(49) = den(27)*den(48)
  den(50) = den(13)*den(26)
  den(51) = den(12)*den(50)
  den(52) = den(12)*den(33)
  den(53) = den(26)*den(52)
  den(54) = den(18)*den(50)
  den(56) = den(18)*den(55)
  den(57) = den(26)*den(56)
  den(58) = den(24)*den(26)
  den(59) = den(6)*den(36)
  den(60) = den(2)*den(59)
  den(61) = den(2)*den(39)
  den(62) = den(36)*den(61)
  den(64) = den(42)*den(63)
  den(65) = den(2)*den(64)
  den(66) = den(42)*den(61)
  den(67) = den(2)*den(48)
  den(69) = den(18)*den(36)
  den(70) = den(68)*den(69)
  den(71) = den(39)*den(68)
  den(72) = den(36)*den(71)
  den(73) = den(12)*den(42)
  den(74) = den(68)*den(73)
  den(75) = den(13)*den(68)
  den(76) = den(12)*den(75)
  den(77) = den(42)*den(71)
  den(78) = den(18)*den(75)
  den(79) = den(3)*den(75)
  den(80) = den(48)*den(68)
  den(81) = den(18)*den(59)
  den(82) = den(36)*den(56)
  den(83) = den(12)*den(64)
  den(84) = den(42)*den(52)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_ppzwj_uxdzwxgg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppzwj_uxdzwxgg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for anti-up down Z W+ glue glue -> 0
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
  use ol_external_ppzwj_uxdzwxgg_1, only: external_perm_ppzwj_uxdzwxgg_1, &
    & external_perm_inv_ppzwj_uxdzwxgg_1, extcomb_perm_ppzwj_uxdzwxgg_1, &
    & average_factor_ppzwj_uxdzwxgg_1
  use ol_external_ppzwj_uxdzwxgg_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_ppzwj_uxdzwxgg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppzwj_uxdzwxgg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_ppzwj_uxdzwxgg_1
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
  complex(REALKIND) :: MOM_LC(4), M1(2), M1helarray(2,144)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(3), ex4(3), ex5(2), ex6(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,9), wf6(6,8), wf8(8,8), wf9(9,2), wf12(12,28), wf16(16,2), wf18(18,12), wf36(36,2), wf144(144,38)

  type(polcont) :: A(144,38)
  complex(REALKIND) :: Aj(38)

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
  extmasses2 = [ rZERO2, rZERO2, rMZ2, rMW2, rZERO2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppzwj_uxdzwxgg_1,6)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,6)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppzwj_uxdzwxgg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppzwj_uxdzwxgg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppzwj_uxdzwxgg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_A(P(:,1), rZERO, H1, ex1)
  call wf_Q(P(:,2), rZERO, H2, ex2)
  call wf_V(P(:,3), rMZ, H3, ex3)
  call wf_V(P(:,4), rMW, H4, ex4)
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
  call vert_AZ_Q(gZu,ntry, ex1, ex3, wf6(:,1), n3(:,1), t3x6(:,:,1))
  call vert_WQ_A(ntry, ex4, ex2, wf6(:,2), n3(:,2), t3x6(:,:,2))
  call vert_UV_W(ntry, ex5, Q(:,16), ex6, Q(:,32), wf4(:,1), n3(:,3), t3x4(:,:,1))
  call prop_A_Q(ntry, wf6(:,1), Q(:,5), ZERO, 0_intkind1, wf6(:,3), n2(1))
  call prop_Q_A(ntry, wf6(:,2), Q(:,10), ZERO, 0_intkind1, wf6(:,4), n2(2))
  call vert_QA_V(ntry, wf6(:,4), wf6(:,3), wf36(:,1), n3(:,4), t3x36(:,:,1))
  call vert_AV_Q(ntry, wf6(:,3), ex5, wf12(:,1), n3(:,5), t3x12(:,:,1))
  call vert_VQ_A(ntry, ex6, wf6(:,4), wf12(:,2), n3(:,6), t3x12(:,:,2))
  call prop_A_Q(ntry, wf12(:,1), Q(:,21), ZERO, 0_intkind1, wf12(:,3), n2(3))
  call vert_VQ_A(ntry, ex5, wf6(:,4), wf12(:,4), n3(:,7), t3x12(:,:,3))
  call vert_AV_Q(ntry, wf6(:,3), ex6, wf12(:,5), n3(:,8), t3x12(:,:,4))
  call prop_Q_A(ntry, wf12(:,4), Q(:,26), ZERO, 0_intkind1, wf12(:,6), n2(4))
  call vert_VQ_A(ntry, ex5, ex2, wf4(:,2), n3(:,9), t3x4(:,:,2))
  call prop_Q_A(ntry, wf4(:,2), Q(:,18), ZERO, 0_intkind1, wf4(:,3), n2(5))
  call vert_AW_Q(ntry, wf6(:,3), ex4, wf18(:,1), n3(:,10), t3x18(:,:,1))
  call vert_VQ_A(ntry, ex6, wf4(:,3), wf8(:,1), n3(:,11), t3x8(:,:,1))
  call prop_A_Q(ntry, wf18(:,1), Q(:,13), ZERO, 0_intkind1, wf18(:,2), n2(6))
  call vert_WQ_A(ntry, ex4, wf4(:,3), wf12(:,7), n3(:,12), t3x12(:,:,5))
  call prop_Q_A(ntry, wf12(:,7), Q(:,26), ZERO, 0_intkind1, wf12(:,8), n2(7))
  call vert_VQ_A(ntry, ex6, ex2, wf4(:,4), n3(:,13), t3x4(:,:,3))
  call prop_Q_A(ntry, wf4(:,4), Q(:,34), ZERO, 0_intkind1, wf4(:,5), n2(8))
  call vert_VQ_A(ntry, ex5, wf4(:,5), wf8(:,2), n3(:,14), t3x8(:,:,2))
  call vert_WQ_A(ntry, ex4, wf4(:,5), wf12(:,9), n3(:,15), t3x12(:,:,6))
  call prop_Q_A(ntry, wf12(:,9), Q(:,42), ZERO, 0_intkind1, wf12(:,10), n2(9))
  call vert_VQ_A(ntry, wf4(:,1), ex2, wf8(:,3), n3(:,16), t3x8(:,:,3))
  call prop_Q_A(ntry, wf8(:,3), Q(:,50), ZERO, 0_intkind1, wf8(:,4), n2(10))
  call vert_AW_Q(ntry, ex1, ex4, wf6(:,5), n3(:,17), t3x6(:,:,3))
  call vert_ZQ_A(gZd,ntry, ex3, ex2, wf6(:,6), n3(:,18), t3x6(:,:,4))
  call prop_A_Q(ntry, wf6(:,5), Q(:,9), ZERO, 0_intkind1, wf6(:,7), n2(11))
  call prop_Q_A(ntry, wf6(:,6), Q(:,6), ZERO, 0_intkind1, wf6(:,8), n2(12))
  call vert_QA_V(ntry, wf6(:,8), wf6(:,7), wf36(:,2), n3(:,19), t3x36(:,:,2))
  call vert_AV_Q(ntry, wf6(:,7), ex5, wf12(:,11), n3(:,20), t3x12(:,:,7))
  call vert_VQ_A(ntry, ex6, wf6(:,8), wf12(:,12), n3(:,21), t3x12(:,:,8))
  call prop_A_Q(ntry, wf12(:,11), Q(:,25), ZERO, 0_intkind1, wf12(:,13), n2(13))
  call vert_VQ_A(ntry, ex5, wf6(:,8), wf12(:,14), n3(:,22), t3x12(:,:,9))
  call vert_AV_Q(ntry, wf6(:,7), ex6, wf12(:,15), n3(:,23), t3x12(:,:,10))
  call prop_Q_A(ntry, wf12(:,14), Q(:,22), ZERO, 0_intkind1, wf12(:,16), n2(14))
  call vert_AV_Q(ntry, ex1, ex5, wf4(:,6), n3(:,24), t3x4(:,:,4))
  call prop_A_Q(ntry, wf4(:,6), Q(:,17), ZERO, 0_intkind1, wf4(:,7), n2(15))
  call vert_AW_Q(ntry, wf4(:,7), ex4, wf12(:,17), n3(:,25), t3x12(:,:,11))
  call prop_A_Q(ntry, wf12(:,17), Q(:,25), ZERO, 0_intkind1, wf12(:,18), n2(16))
  call vert_WQ_A(ntry, ex4, wf6(:,8), wf18(:,3), n3(:,26), t3x18(:,:,2))
  call vert_AV_Q(ntry, wf4(:,7), ex6, wf8(:,5), n3(:,27), t3x8(:,:,4))
  call prop_Q_A(ntry, wf18(:,3), Q(:,14), ZERO, 0_intkind1, wf18(:,4), n2(17))
  call vert_AV_Q(ntry, ex1, ex6, wf4(:,8), n3(:,28), t3x4(:,:,5))
  call prop_A_Q(ntry, wf4(:,8), Q(:,33), ZERO, 0_intkind1, wf4(:,9), n2(18))
  call vert_AW_Q(ntry, wf4(:,9), ex4, wf12(:,19), n3(:,29), t3x12(:,:,12))
  call prop_A_Q(ntry, wf12(:,19), Q(:,41), ZERO, 0_intkind1, wf12(:,20), n2(19))
  call vert_AV_Q(ntry, wf4(:,9), ex5, wf8(:,6), n3(:,30), t3x8(:,:,5))
  call vert_AV_Q(ntry, ex1, wf4(:,1), wf8(:,7), n3(:,31), t3x8(:,:,6))
  call prop_A_Q(ntry, wf8(:,7), Q(:,49), ZERO, 0_intkind1, wf8(:,8), n2(20))
  call vert_AZ_Q(gZd,ntry, wf6(:,7), ex3, wf18(:,5), n3(:,32), t3x18(:,:,3))
  call prop_A_Q(ntry, wf18(:,5), Q(:,13), ZERO, 0_intkind1, wf18(:,6), n2(21))
  call vert_ZQ_A(gZd,ntry, ex3, wf4(:,3), wf12(:,21), n3(:,33), t3x12(:,:,13))
  call prop_Q_A(ntry, wf12(:,21), Q(:,22), ZERO, 0_intkind1, wf12(:,22), n2(22))
  call vert_ZQ_A(gZd,ntry, ex3, wf4(:,5), wf12(:,23), n3(:,34), t3x12(:,:,14))
  call prop_Q_A(ntry, wf12(:,23), Q(:,38), ZERO, 0_intkind1, wf12(:,24), n2(23))
  call vert_AZ_Q(gZu,ntry, wf4(:,7), ex3, wf12(:,25), n3(:,35), t3x12(:,:,15))
  call prop_A_Q(ntry, wf12(:,25), Q(:,21), ZERO, 0_intkind1, wf12(:,26), n2(24))
  call vert_ZQ_A(gZu,ntry, ex3, wf6(:,4), wf18(:,7), n3(:,36), t3x18(:,:,4))
  call prop_Q_A(ntry, wf18(:,7), Q(:,14), ZERO, 0_intkind1, wf18(:,8), n2(25))
  call vert_AZ_Q(gZu,ntry, wf4(:,9), ex3, wf12(:,27), n3(:,37), t3x12(:,:,16))
  call prop_A_Q(ntry, wf12(:,27), Q(:,37), ZERO, 0_intkind1, wf12(:,28), n2(26))
  call vert_UV_W(ntry, ex3, Q(:,4), ex4, Q(:,8), wf9(:,1), n3(:,38), t3x9(:,:,1))
  call prop_W_W(ntry, wf9(:,1), Q(:,12), MW, 1_intkind1, wf9(:,2), n2(27))
  call vert_QA_W(ntry, wf4(:,5), wf4(:,7), wf16(:,1), n3(:,39), t3x16(:,:,1))
  call vert_WQ_A(ntry, wf9(:,2), ex2, wf18(:,9), n3(:,40), t3x18(:,:,5))
  call prop_Q_A(ntry, wf18(:,9), Q(:,14), ZERO, 0_intkind1, wf18(:,10), n2(28))
  call vert_QA_W(ntry, wf4(:,3), wf4(:,9), wf16(:,2), n3(:,41), t3x16(:,:,2))
  call vert_AW_Q(ntry, ex1, wf9(:,2), wf18(:,11), n3(:,42), t3x18(:,:,6))
  call prop_A_Q(ntry, wf18(:,11), Q(:,13), ZERO, 0_intkind1, wf18(:,12), n2(29))


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
  M2add = M2 / average_factor_ppzwj_uxdzwxgg_1

  do k = 0, 23-1
    M2(k) = M2add(extcomb_perm_ppzwj_uxdzwxgg_1(k))
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

    call cont_VV(nsync, wf4(:,1), wf36(:,1), A(:,1), n3(:,43), t3x144(:,:,1), nhel, den(5))
    call cont_QA(nsync, wf12(:,2), wf12(:,3), A(:,2), n3(:,44), t3x144(:,:,2), nhel, den(8))
    call cont_QA(nsync, wf12(:,5), wf12(:,6), A(:,3), n3(:,45), t3x144(:,:,3), nhel, den(11))
    call cont_QA(nsync, wf8(:,1), wf18(:,2), A(:,4), n3(:,46), t3x144(:,:,4), nhel, den(15))
    call cont_QA(nsync, wf12(:,5), wf12(:,8), A(:,5), n3(:,47), t3x144(:,:,5), nhel, den(17))
    call cont_QA(nsync, wf18(:,2), wf8(:,2), A(:,6), n3(:,48), t3x144(:,:,6), nhel, den(19))
    call cont_QA(nsync, wf12(:,1), wf12(:,10), A(:,7), n3(:,49), t3x144(:,:,7), nhel, den(22))
    call cont_QA(nsync, wf18(:,1), wf8(:,4), A(:,8), n3(:,50), t3x144(:,:,8), nhel, den(25))
    call cont_VV(nsync, wf4(:,1), wf36(:,2), A(:,9), n3(:,51), t3x144(:,:,9), nhel, den(29))
    call cont_QA(nsync, wf12(:,12), wf12(:,13), A(:,10), n3(:,52), t3x144(:,:,10), nhel, den(32))
    call cont_QA(nsync, wf12(:,15), wf12(:,16), A(:,11), n3(:,53), t3x144(:,:,11), nhel, den(35))
    call cont_QA(nsync, wf12(:,12), wf12(:,18), A(:,12), n3(:,54), t3x144(:,:,12), nhel, den(38))
    call cont_QA(nsync, wf8(:,5), wf18(:,4), A(:,13), n3(:,55), t3x144(:,:,13), nhel, den(41))
    call cont_QA(nsync, wf12(:,14), wf12(:,20), A(:,14), n3(:,56), t3x144(:,:,14), nhel, den(45))
    call cont_QA(nsync, wf18(:,4), wf8(:,6), A(:,15), n3(:,57), t3x144(:,:,15), nhel, den(46))
    call cont_QA(nsync, wf18(:,3), wf8(:,8), A(:,16), n3(:,58), t3x144(:,:,16), nhel, den(49))
    call cont_QA(nsync, wf8(:,1), wf18(:,6), A(:,17), n3(:,59), t3x144(:,:,17), nhel, den(51))
    call cont_QA(nsync, wf12(:,15), wf12(:,22), A(:,18), n3(:,60), t3x144(:,:,18), nhel, den(53))
    call cont_QA(nsync, wf8(:,2), wf18(:,6), A(:,19), n3(:,61), t3x144(:,:,19), nhel, den(54))
    call cont_QA(nsync, wf12(:,11), wf12(:,24), A(:,20), n3(:,62), t3x144(:,:,20), nhel, den(57))
    call cont_QA(nsync, wf8(:,4), wf18(:,5), A(:,21), n3(:,63), t3x144(:,:,21), nhel, den(58))
    call cont_QA(nsync, wf12(:,2), wf12(:,26), A(:,22), n3(:,64), t3x144(:,:,22), nhel, den(60))
    call cont_QA(nsync, wf8(:,5), wf18(:,8), A(:,23), n3(:,65), t3x144(:,:,23), nhel, den(62))
    call cont_QA(nsync, wf12(:,4), wf12(:,28), A(:,24), n3(:,66), t3x144(:,:,24), nhel, den(65))
    call cont_QA(nsync, wf8(:,6), wf18(:,8), A(:,25), n3(:,67), t3x144(:,:,25), nhel, den(66))
    call cont_QA(nsync, wf8(:,8), wf18(:,7), A(:,26), n3(:,68), t3x144(:,:,26), nhel, den(67))
    call cont_VV(nsync, wf9(:,2), wf16(:,1), A(:,27), n3(:,69), t3x144(:,:,27), nhel, den(70))
    call cont_QA(nsync, wf8(:,5), wf18(:,10), A(:,28), n3(:,70), t3x144(:,:,28), nhel, den(72))
    call cont_VV(nsync, wf9(:,2), wf16(:,2), A(:,29), n3(:,71), t3x144(:,:,29), nhel, den(74))
    call cont_QA(nsync, wf8(:,1), wf18(:,12), A(:,30), n3(:,72), t3x144(:,:,30), nhel, den(76))
    call cont_QA(nsync, wf8(:,6), wf18(:,10), A(:,31), n3(:,73), t3x144(:,:,31), nhel, den(77))
    call cont_QA(nsync, wf8(:,2), wf18(:,12), A(:,32), n3(:,74), t3x144(:,:,32), nhel, den(78))
    call cont_QA(nsync, wf8(:,3), wf18(:,12), A(:,33), n3(:,75), t3x144(:,:,33), nhel, den(79))
    call cont_QA(nsync, wf8(:,8), wf18(:,9), A(:,34), n3(:,76), t3x144(:,:,34), nhel, den(80))
    call cont_QA(nsync, wf12(:,9), wf12(:,26), A(:,35), n3(:,77), t3x144(:,:,35), nhel, den(81))
    call cont_QA(nsync, wf12(:,17), wf12(:,24), A(:,36), n3(:,78), t3x144(:,:,36), nhel, den(82))
    call cont_QA(nsync, wf12(:,7), wf12(:,28), A(:,37), n3(:,79), t3x144(:,:,37), nhel, den(83))
    call cont_QA(nsync, wf12(:,19), wf12(:,22), A(:,38), n3(:,80), t3x144(:,:,38), nhel, den(84))

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
  complex(REALKIND), intent(out) :: M1(2) ! M1helarray(2,144)
  integer :: empty(0)

  M1(1) = (-A(j,27)%j-A(j,28)%j-A(j,32)%j)*f(1)+CI*(-A(j,33)%j-A(j,34)%j)*f(2)+(-A(j,2)%j-A(j,6)%j-A(j,7)%j-A(j,10)%j-A(j,12)%j &
       -A(j,13)%j-A(j,19)%j-A(j,20)%j-A(j,22)%j-A(j,23)%j-A(j,35)%j-A(j,36)%j)*f(3)+CI*(-A(j,1)%j-A(j,8)%j-A(j,9)%j-A(j,16)%j &
       -A(j,21)%j-A(j,26)%j)*f(4)
  M1(2) = (-A(j,29)%j-A(j,30)%j-A(j,31)%j)*f(1)+CI*(A(j,33)%j+A(j,34)%j)*f(2)+(-A(j,3)%j-A(j,4)%j-A(j,5)%j-A(j,11)%j-A(j,14)%j &
       -A(j,15)%j-A(j,17)%j-A(j,18)%j-A(j,24)%j-A(j,25)%j-A(j,37)%j-A(j,38)%j)*f(3)+CI*(A(j,1)%j+A(j,8)%j+A(j,9)%j+A(j,16)%j &
       +A(j,21)%j+A(j,26)%j)*f(4)

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
  use ol_colourmatrix_ppzwj_uxdzwxgg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(2)
  real(REALKIND),    intent(out) :: M2colint(0:23-1)
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
  use ol_colourmatrix_ppzwj_uxdzwxgg_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_ppzwj_uxdzwxgg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(2,144)
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
    & bind(c,name="ol_f_amp2tree_ppzwj_uxdzwxgg_1")
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
    & bind(c,name="ol_f_amp2ccone_ppzwj_uxdzwxgg_1")
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
    & bind(c,name="ol_f_amp2ccall_ppzwj_uxdzwxgg_1")
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
    & bind(c,name="ol_f_amp2hcone_ppzwj_uxdzwxgg_1")
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
    & bind(c,name="ol_f_amp2hcall_ppzwj_uxdzwxgg_1")
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
    & bind(c,name="ol_amp2tree_ppzwj_uxdzwxgg_1")
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
    & bind(c,name="ol_amp2ccone_ppzwj_uxdzwxgg_1")
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
    & bind(c,name="ol_amp2ccall_ppzwj_uxdzwxgg_1")
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
    & bind(c,name="ol_amp2hcone_ppzwj_uxdzwxgg_1")
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
    & bind(c,name="ol_amp2hcall_ppzwj_uxdzwxgg_1")
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
    & bind(c,name="amp2tree_ppzwj_uxdzwxgg_1_")
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
    & bind(c,name="amp2ccone_ppzwj_uxdzwxgg_1_")
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
    & bind(c,name="amp2ccall_ppzwj_uxdzwxgg_1_")
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
    & bind(c,name="amp2hcone_ppzwj_uxdzwxgg_1_")
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
    & bind(c,name="amp2hcall_ppzwj_uxdzwxgg_1_")
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

end module ol_tree_ppzwj_uxdzwxgg_1_/**/REALKIND
