
module ol_colourmatrix_pphhjj_dddxdxhhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(120,4)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1(  1,:) = [  36,  12,  12,   0]
  K1(  2,:) = [  12,  36,   0,  12]
  K1(  3,:) = [  12,   0,  36,  12]
  K1(  4,:) = [   0,  12,  12,  36]
  K1(  5,:) = [  48,  16,  16,   0]
  K1(  6,:) = [  16,  48,   0,  16]
  K1(  7,:) = [  16,   0,  48,  16]
  K1(  8,:) = [   0,  16,  16,  48]
  K1(  9,:) = [   0,  16,  -2,   6]
  K1( 10,:) = [  16,   0,   6,  -2]
  K1( 11,:) = [  -2,   6,   0,  16]
  K1( 12,:) = [   6,  -2,  16,   0]
  K1( 13,:) = [  48,  16,  16,   0]
  K1( 14,:) = [  16,  48,   0,  16]
  K1( 15,:) = [  16,   0,  48,  16]
  K1( 16,:) = [   0,  16,  16,  48]
  K1( 17,:) = [   6,   2,   2,   0]
  K1( 18,:) = [   2,   0,  -6, -16]
  K1( 19,:) = [   2,  -6,   0, -16]
  K1( 20,:) = [   0, -16, -16, -48]
  K1( 21,:) = [   0,   2, -16,  -6]
  K1( 22,:) = [   2,   6,   0,   2]
  K1( 23,:) = [ -16,   0, -48, -16]
  K1( 24,:) = [  -6,   2, -16,   0]
  K1( 25,:) = [  48,  16,  16,   0]
  K1( 26,:) = [  16,  48,   0,  16]
  K1( 27,:) = [  16,   0,  48,  16]
  K1( 28,:) = [   0,  16,  16,  48]
  K1( 29,:) = [   0, -16,   2,  -6]
  K1( 30,:) = [ -16, -48,   0, -16]
  K1( 31,:) = [   2,   0,   6,   2]
  K1( 32,:) = [  -6, -16,   2,   0]
  K1( 33,:) = [ -48, -16, -16,   0]
  K1( 34,:) = [ -16,   0,  -6,   2]
  K1( 35,:) = [ -16,  -6,   0,   2]
  K1( 36,:) = [   0,   2,   2,   6]
  K1( 37,:) = [   0,  -2,  16,   6]
  K1( 38,:) = [  -2,   0,   6,  16]
  K1( 39,:) = [  16,   6,   0,  -2]
  K1( 40,:) = [   6,  16,  -2,   0]
  K1( 41,:) = [  48,  16,  16,   0]
  K1( 42,:) = [  16,  48,   0,  16]
  K1( 43,:) = [  16,   0,  48,  16]
  K1( 44,:) = [   0,  16,  16,  48]
  K1( 45,:) = [   0,   0,   0,   0]
  K1( 46,:) = [   0,   0,   0,   0]
  K1( 47,:) = [   0,   0,   0,   0]
  K1( 48,:) = [   0,   0,   0,   0]
  K1( 49,:) = [   0,   0,   0,   0]
  K1( 50,:) = [   0,   0,   0,   0]
  K1( 51,:) = [   0,   0,   0,   0]
  K1( 52,:) = [   0,   0,   0,   0]
  K1( 53,:) = [   0,   0,   0,   0]
  K1( 54,:) = [   0,   0,   0,   0]
  K1( 55,:) = [   0,   0,   0,   0]
  K1( 56,:) = [   0,   0,   0,   0]
  K1( 57,:) = [   0,   0,   0,   0]
  K1( 58,:) = [   0,   0,   0,   0]
  K1( 59,:) = [   0,   0,   0,   0]
  K1( 60,:) = [   0,   0,   0,   0]
  K1( 61,:) = [   0,   0,   0,   0]
  K1( 62,:) = [   0,   0,   0,   0]
  K1( 63,:) = [   0,   0,   0,   0]
  K1( 64,:) = [   0,   0,   0,   0]
  K1( 65,:) = [   0,   0,   0,   0]
  K1( 66,:) = [   0,   0,   0,   0]
  K1( 67,:) = [   0,   0,   0,   0]
  K1( 68,:) = [   0,   0,   0,   0]
  K1( 69,:) = [   0,   0,   0,   0]
  K1( 70,:) = [   0,   0,   0,   0]
  K1( 71,:) = [   0,   0,   0,   0]
  K1( 72,:) = [   0,   0,   0,   0]
  K1( 73,:) = [   0,   0,   0,   0]
  K1( 74,:) = [   0,   0,   0,   0]
  K1( 75,:) = [   0,   0,   0,   0]
  K1( 76,:) = [   0,   0,   0,   0]
  K1( 77,:) = [   0,   0,   0,   0]
  K1( 78,:) = [   0,   0,   0,   0]
  K1( 79,:) = [   0,   0,   0,   0]
  K1( 80,:) = [   0,   0,   0,   0]
  K1( 81,:) = [   0,   0,   0,   0]
  K1( 82,:) = [   0,   0,   0,   0]
  K1( 83,:) = [   0,   0,   0,   0]
  K1( 84,:) = [   0,   0,   0,   0]
  K1( 85,:) = [   0,   0,   0,   0]
  K1( 86,:) = [   0,   0,   0,   0]
  K1( 87,:) = [   0,   0,   0,   0]
  K1( 88,:) = [   0,   0,   0,   0]
  K1( 89,:) = [ -54, -18, -18,   0]
  K1( 90,:) = [ -18,   0,   0,  18]
  K1( 91,:) = [ -18,   0, -54, -18]
  K1( 92,:) = [   0,  18, -18,   0]
  K1( 93,:) = [   0, -18,  18,   0]
  K1( 94,:) = [ -18, -54,   0, -18]
  K1( 95,:) = [  18,   0,   0, -18]
  K1( 96,:) = [   0, -18, -18, -54]
  K1( 97,:) = [ -54, -18, -18,   0]
  K1( 98,:) = [ -18, -54,   0, -18]
  K1( 99,:) = [ -18,   0,   0,  18]
  K1(100,:) = [   0, -18,  18,   0]
  K1(101,:) = [   0,  18, -18,   0]
  K1(102,:) = [  18,   0,   0, -18]
  K1(103,:) = [ -18,   0, -54, -18]
  K1(104,:) = [   0, -18, -18, -54]
  K1(105,:) = [   0,   0,   0,   0]
  K1(106,:) = [   0,   0,   0,   0]
  K1(107,:) = [   0,   0,   0,   0]
  K1(108,:) = [   0,   0,   0,   0]
  K1(109,:) = [   0,   0,   0,   0]
  K1(110,:) = [   0,   0,   0,   0]
  K1(111,:) = [   0,   0,   0,   0]
  K1(112,:) = [   0,   0,   0,   0]
  K1(113,:) = [ 108,  36,  36,   0]
  K1(114,:) = [  36, 108,   0,  36]
  K1(115,:) = [  36,   0, 108,  36]
  K1(116,:) = [   0,  36,  36, 108]
  K1(117,:) = [   0,   0,   0,   0]
  K1(118,:) = [   0,   0,   0,   0]
  K1(119,:) = [   0,   0,   0,   0]
  K1(120,:) = [   0,   0,   0,   0]
  K1 = (1._/**/REALKIND / 3) * K1

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphhjj_dddxdxhhg_1_/**/REALKIND



module ol_forced_parameters_pphhjj_dddxdxhhg_1_/**/REALKIND
  implicit none
  contains
  subroutine check_forced_parameters
    use ol_parameters_decl_/**/REALKIND
    use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
    use ol_loop_parameters_decl_/**/DREALKIND, only: LeadingColour, nc, nf, CKMORDER
#endif
    implicit none
    logical, save :: checks_not_written = .true.

    if (checks_not_written) then
    ! e.g.
    ! if (ME /= 0) write(*,101) 'ME = 0'
  if (CKMORDER /= 0) write(*,101) 'CKMORDER = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (YU /= 0) write(*,101) 'YU = 0'
  if (YD /= 0) write(*,101) 'YD = 0'
  if (YS /= 0) write(*,101) 'YS = 0'
  if (YC /= 0) write(*,101) 'YC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphhjj_dddxdxhhg_1_/**/REALKIND

module ol_tree_pphhjj_dddxdxhhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(4)
  complex(REALKIND), save :: den(73)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 32 ! number of helicity configurations
  integer(intkind2), save :: nhel = 32 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(32) ! physical helicity states
  complex(DREALKIND) :: M1helarr(4,32) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED**4*gQCD)/(4._/**/REALKIND*cw**2*sw**2)
    f(2) = (CI*eQED**4*gQCD)/(2._/**/REALKIND*cw**2*sw**2)
    f(3) = (3*CI*eQED**4*gQCD*lambdaHHH*lambdaHZZ*MH**2)/(2._/**/REALKIND*cw**2*sw**2)
    f(4) = (CI*eQED**4*gQCD*lambdaHZZ**2*MW**2)/(cw**4*sw**2)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,66))
  den(2) = 1 / (Q(5,5) - MZ2)
  den(3) = 1 / (Q(5,74) - MZ2)
  den(6) = 1 / (Q(5,72))
  den(9) = 1 / (Q(5,65))
  den(10) = 1 / (Q(5,6) - MZ2)
  den(11) = 1 / (Q(5,73) - MZ2)
  den(16) = 1 / (Q(5,9) - MZ2)
  den(17) = 1 / (Q(5,70) - MZ2)
  den(20) = 1 / (Q(5,68))
  den(23) = 1 / (Q(5,10) - MZ2)
  den(24) = 1 / (Q(5,69) - MZ2)
  den(29) = 1 / (Q(5,48) - MH2)
  den(33) = 1 / (Q(5,21) - MZ2)
  den(36) = 1 / (Q(5,37) - MZ2)
  den(44) = 1 / (Q(5,22) - MZ2)
  den(47) = 1 / (Q(5,38) - MZ2)
  den(55) = 1 / (Q(5,25) - MZ2)
  den(58) = 1 / (Q(5,41) - MZ2)
  den(66) = 1 / (Q(5,26) - MZ2)
  den(69) = 1 / (Q(5,42) - MZ2)

  ! denominators

  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(3)*den(6)
  den(8) = den(2)*den(7)
  den(12) = den(9)*den(11)
  den(13) = den(10)*den(12)
  den(14) = den(6)*den(11)
  den(15) = den(10)*den(14)
  den(18) = den(1)*den(17)
  den(19) = den(16)*den(18)
  den(21) = den(17)*den(20)
  den(22) = den(16)*den(21)
  den(25) = den(9)*den(24)
  den(26) = den(23)*den(25)
  den(27) = den(20)*den(24)
  den(28) = den(23)*den(27)
  den(30) = den(2)*den(29)
  den(31) = den(4)*den(30)
  den(32) = den(7)*den(30)
  den(34) = den(2)*den(33)
  den(35) = den(4)*den(34)
  den(37) = den(2)*den(36)
  den(38) = den(4)*den(37)
  den(39) = den(7)*den(34)
  den(40) = den(7)*den(37)
  den(41) = den(10)*den(29)
  den(42) = den(12)*den(41)
  den(43) = den(14)*den(41)
  den(45) = den(10)*den(44)
  den(46) = den(12)*den(45)
  den(48) = den(10)*den(47)
  den(49) = den(12)*den(48)
  den(50) = den(14)*den(48)
  den(51) = den(14)*den(45)
  den(52) = den(16)*den(29)
  den(53) = den(18)*den(52)
  den(54) = den(21)*den(52)
  den(56) = den(16)*den(55)
  den(57) = den(18)*den(56)
  den(59) = den(16)*den(58)
  den(60) = den(18)*den(59)
  den(61) = den(21)*den(56)
  den(62) = den(21)*den(59)
  den(63) = den(23)*den(29)
  den(64) = den(25)*den(63)
  den(65) = den(27)*den(63)
  den(67) = den(23)*den(66)
  den(68) = den(25)*den(67)
  den(70) = den(23)*den(69)
  den(71) = den(25)*den(70)
  den(72) = den(27)*den(70)
  den(73) = den(27)*den(67)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_pphhjj_dddxdxhhg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_pphhjj_dddxdxhhg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for down down anti-down anti-down higgs higgs glue -> 0
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
  use ol_external_pphhjj_dddxdxhhg_1, only: external_perm_pphhjj_dddxdxhhg_1, &
    & external_perm_inv_pphhjj_dddxdxhhg_1, extcomb_perm_pphhjj_dddxdxhhg_1, &
    & average_factor_pphhjj_dddxdxhhg_1
  use ol_external_pphhjj_dddxdxhhg_1, only: H, hel_not_initialised, hel_init, POLSEL
  use ol_colourmatrix_pphhjj_dddxdxhhg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_pphhjj_dddxdxhhg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_pphhjj_dddxdxhhg_1
  use ol_kinematics_/**/REALKIND, only: LC2Std_Rep_cmplx
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,7)
  real(REALKIND),  intent(out) :: M2(0:30-1)
  real(REALKIND),  intent(out) :: M2munu(4,4)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer,         intent(in)  :: nextcombs
  integer,         intent(in)  :: extcombs(nextcombs)

  integer           :: ReplacePol, JBmunu, extcombs_permuted(nextcombs), shift, k, r, m, n
  real(REALKIND)    :: P(0:3,7)
  real(REALKIND)    :: extmasses2(7)
  real(REALKIND)    :: M2add(0:30-1)
  complex(REALKIND) :: M2munuadd(2)
  complex(REALKIND) :: MOM_LC(4), M1(4), M1helarray(4,32)
  real(REALKIND)    :: P_scatt_intern(0:3,7)
  complex(REALKIND) :: epLC(1:4), epStd(1:4,2)
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(1), ex6(1), ex7(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf1(1,1), wf4(4,36), wf8(8,40), wf32(32,48)

  type(polcont) :: A(32,48)
  complex(REALKIND) :: Aj(48)

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
  extmasses2 = [ rZERO2, rZERO2, rZERO2, rZERO2, rMH2, rMH2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_pphhjj_dddxdxhhg_1,7)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,7)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_pphhjj_dddxdxhhg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_pphhjj_dddxdxhhg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_pphhjj_dddxdxhhg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  42 continue
  ! external WFs
  call pol_wf_Q(P(:,1), rZERO, H1, ex1, POLSEL(1))
  call pol_wf_Q(P(:,2), rZERO, H2, ex2, POLSEL(2))
  call pol_wf_A(P(:,3), rZERO, H3, ex3, POLSEL(3))
  call pol_wf_A(P(:,4), rZERO, H4, ex4, POLSEL(4))
  call pol_wf_S(P(:,5), rMH, H5, ex5, POLSEL(5))
  call pol_wf_S(P(:,6), rMH, H6, ex6, POLSEL(6))
  call pol_wf_V(P(:,7), rZERO, H7, ex7, POLSEL(7))


  if (ntry == 1) then
    shift = 1
    ! call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    ! call helbookkeeping_wf(H1, ex1, shift) ...

    if (any(POLSEL /= 0)) then

      call pol_wf_Q(P(:,1), rZERO, H1, ex1, 0)
      call pol_wf_Q(P(:,2), rZERO, H2, ex2, 0)
      call pol_wf_A(P(:,3), rZERO, H3, ex3, 0)
      call pol_wf_A(P(:,4), rZERO, H4, ex4, 0)
      call pol_wf_S(P(:,5), rMH, H5, ex5, 0)
      call pol_wf_S(P(:,6), rMH, H6, ex6, 0)
      call pol_wf_V(P(:,7), rZERO, H7, ex7, 0)

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
    call helbookkeeping_flip(H7, 7, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H7, ex7, shift)

  end if

  ! internal WFs
  ! e.g. call vert_VQ_A(ntry, ex3, ex1, wf1, n1, t1) ...
  call vert_QA_Z(gZd,ntry, ex1, ex3, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_VQ_A(ntry, ex7, ex2, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call prop_Q_A(ntry, wf4(:,2), Q(:,66), ZERO, 0_intkind1, wf4(:,3), n2(1))
  call vert_QA_Z(gZd,ntry, wf4(:,3), ex4, wf8(:,1), n3(:,3), t3x8(:,:,1))
  call vert_SSV_V(ntry, ex5, ex6, wf4(:,1), wf4(:,4), n4(:,1), t4x4(:,:,1))
  call vert_AV_Q(ntry, ex4, ex7, wf4(:,5), n3(:,4), t3x4(:,:,3))
  call prop_A_Q(ntry, wf4(:,5), Q(:,72), ZERO, 0_intkind1, wf4(:,6), n2(2))
  call vert_QA_Z(gZd,ntry, ex2, wf4(:,6), wf8(:,2), n3(:,5), t3x8(:,:,2))
  call vert_VQ_A(ntry, ex7, ex1, wf4(:,7), n3(:,6), t3x4(:,:,4))
  call vert_QA_Z(gZd,ntry, ex2, ex3, wf4(:,8), n3(:,7), t3x4(:,:,5))
  call prop_Q_A(ntry, wf4(:,7), Q(:,65), ZERO, 0_intkind1, wf4(:,9), n2(3))
  call vert_QA_Z(gZd,ntry, wf4(:,9), ex4, wf8(:,3), n3(:,8), t3x8(:,:,3))
  call vert_SSV_V(ntry, ex5, ex6, wf4(:,8), wf4(:,10), n4(:,2), t4x4(:,:,2))
  call vert_QA_Z(gZd,ntry, ex1, wf4(:,6), wf8(:,4), n3(:,9), t3x8(:,:,4))
  call vert_QA_Z(gZd,ntry, ex1, ex4, wf4(:,11), n3(:,10), t3x4(:,:,6))
  call vert_QA_Z(gZd,ntry, wf4(:,3), ex3, wf8(:,5), n3(:,11), t3x8(:,:,5))
  call vert_SSV_V(ntry, ex5, ex6, wf4(:,11), wf4(:,12), n4(:,3), t4x4(:,:,3))
  call vert_AV_Q(ntry, ex3, ex7, wf4(:,13), n3(:,12), t3x4(:,:,7))
  call prop_A_Q(ntry, wf4(:,13), Q(:,68), ZERO, 0_intkind1, wf4(:,14), n2(4))
  call vert_QA_Z(gZd,ntry, ex2, wf4(:,14), wf8(:,6), n3(:,13), t3x8(:,:,6))
  call vert_QA_Z(gZd,ntry, ex2, ex4, wf4(:,15), n3(:,14), t3x4(:,:,8))
  call vert_QA_Z(gZd,ntry, wf4(:,9), ex3, wf8(:,7), n3(:,15), t3x8(:,:,7))
  call vert_SSV_V(ntry, ex5, ex6, wf4(:,15), wf4(:,16), n4(:,4), t4x4(:,:,4))
  call vert_QA_Z(gZd,ntry, ex1, wf4(:,14), wf8(:,8), n3(:,16), t3x8(:,:,8))
  call vert_SS_S(ntry, ex5, ex6, wf1(:,1), n3(:,17), t3x1(:,:,1))
  call vert_SV_V(ntry, wf1(:,1), wf4(:,1), wf4(:,17), n3(:,18), t3x4(:,:,9))
  call vert_TV_S(ntry, ex5, Q(:,16), wf4(:,1), Q(:,5), wf4(:,18), n3(:,19), t3x4(:,:,10))
  call vert_TV_S(ntry, ex6, Q(:,32), wf8(:,1), Q(:,74), wf8(:,9), n3(:,20), t3x8(:,:,9))
  call vert_SV_V(ntry, ex5, wf4(:,1), wf4(:,19), n3(:,21), t3x4(:,:,11))
  call vert_SV_V(ntry, ex6, wf8(:,1), wf8(:,10), n3(:,22), t3x8(:,:,10))
  call vert_TV_S(ntry, ex6, Q(:,32), wf4(:,1), Q(:,5), wf4(:,20), n3(:,23), t3x4(:,:,12))
  call vert_TV_S(ntry, ex5, Q(:,16), wf8(:,1), Q(:,74), wf8(:,11), n3(:,24), t3x8(:,:,11))
  call vert_SV_V(ntry, ex6, wf4(:,1), wf4(:,21), n3(:,25), t3x4(:,:,13))
  call vert_SV_V(ntry, ex5, wf8(:,1), wf8(:,12), n3(:,26), t3x8(:,:,12))
  call vert_TV_S(ntry, ex6, Q(:,32), wf8(:,2), Q(:,74), wf8(:,13), n3(:,27), t3x8(:,:,13))
  call vert_SV_V(ntry, ex6, wf8(:,2), wf8(:,14), n3(:,28), t3x8(:,:,14))
  call vert_TV_S(ntry, ex5, Q(:,16), wf8(:,2), Q(:,74), wf8(:,15), n3(:,29), t3x8(:,:,15))
  call vert_SV_V(ntry, ex5, wf8(:,2), wf8(:,16), n3(:,30), t3x8(:,:,16))
  call vert_SV_V(ntry, wf1(:,1), wf4(:,8), wf4(:,22), n3(:,31), t3x4(:,:,14))
  call vert_TV_S(ntry, ex5, Q(:,16), wf4(:,8), Q(:,6), wf4(:,23), n3(:,32), t3x4(:,:,15))
  call vert_TV_S(ntry, ex6, Q(:,32), wf8(:,3), Q(:,73), wf8(:,17), n3(:,33), t3x8(:,:,17))
  call vert_SV_V(ntry, ex5, wf4(:,8), wf4(:,24), n3(:,34), t3x4(:,:,16))
  call vert_SV_V(ntry, ex6, wf8(:,3), wf8(:,18), n3(:,35), t3x8(:,:,18))
  call vert_TV_S(ntry, ex6, Q(:,32), wf4(:,8), Q(:,6), wf4(:,25), n3(:,36), t3x4(:,:,17))
  call vert_TV_S(ntry, ex5, Q(:,16), wf8(:,3), Q(:,73), wf8(:,19), n3(:,37), t3x8(:,:,19))
  call vert_SV_V(ntry, ex6, wf4(:,8), wf4(:,26), n3(:,38), t3x4(:,:,18))
  call vert_SV_V(ntry, ex5, wf8(:,3), wf8(:,20), n3(:,39), t3x8(:,:,20))
  call vert_TV_S(ntry, ex5, Q(:,16), wf8(:,4), Q(:,73), wf8(:,21), n3(:,40), t3x8(:,:,21))
  call vert_SV_V(ntry, ex5, wf8(:,4), wf8(:,22), n3(:,41), t3x8(:,:,22))
  call vert_TV_S(ntry, ex6, Q(:,32), wf8(:,4), Q(:,73), wf8(:,23), n3(:,42), t3x8(:,:,23))
  call vert_SV_V(ntry, ex6, wf8(:,4), wf8(:,24), n3(:,43), t3x8(:,:,24))
  call vert_SV_V(ntry, wf1(:,1), wf4(:,11), wf4(:,27), n3(:,44), t3x4(:,:,19))
  call vert_TV_S(ntry, ex5, Q(:,16), wf4(:,11), Q(:,9), wf4(:,28), n3(:,45), t3x4(:,:,20))
  call vert_TV_S(ntry, ex6, Q(:,32), wf8(:,5), Q(:,70), wf8(:,25), n3(:,46), t3x8(:,:,25))
  call vert_SV_V(ntry, ex5, wf4(:,11), wf4(:,29), n3(:,47), t3x4(:,:,21))
  call vert_SV_V(ntry, ex6, wf8(:,5), wf8(:,26), n3(:,48), t3x8(:,:,26))
  call vert_TV_S(ntry, ex6, Q(:,32), wf4(:,11), Q(:,9), wf4(:,30), n3(:,49), t3x4(:,:,22))
  call vert_TV_S(ntry, ex5, Q(:,16), wf8(:,5), Q(:,70), wf8(:,27), n3(:,50), t3x8(:,:,27))
  call vert_SV_V(ntry, ex6, wf4(:,11), wf4(:,31), n3(:,51), t3x4(:,:,23))
  call vert_SV_V(ntry, ex5, wf8(:,5), wf8(:,28), n3(:,52), t3x8(:,:,28))
  call vert_TV_S(ntry, ex6, Q(:,32), wf8(:,6), Q(:,70), wf8(:,29), n3(:,53), t3x8(:,:,29))
  call vert_SV_V(ntry, ex6, wf8(:,6), wf8(:,30), n3(:,54), t3x8(:,:,30))
  call vert_TV_S(ntry, ex5, Q(:,16), wf8(:,6), Q(:,70), wf8(:,31), n3(:,55), t3x8(:,:,31))
  call vert_SV_V(ntry, ex5, wf8(:,6), wf8(:,32), n3(:,56), t3x8(:,:,32))
  call vert_SV_V(ntry, wf1(:,1), wf4(:,15), wf4(:,32), n3(:,57), t3x4(:,:,24))
  call vert_TV_S(ntry, ex5, Q(:,16), wf4(:,15), Q(:,10), wf4(:,33), n3(:,58), t3x4(:,:,25))
  call vert_TV_S(ntry, ex6, Q(:,32), wf8(:,7), Q(:,69), wf8(:,33), n3(:,59), t3x8(:,:,33))
  call vert_SV_V(ntry, ex5, wf4(:,15), wf4(:,34), n3(:,60), t3x4(:,:,26))
  call vert_SV_V(ntry, ex6, wf8(:,7), wf8(:,34), n3(:,61), t3x8(:,:,34))
  call vert_TV_S(ntry, ex6, Q(:,32), wf4(:,15), Q(:,10), wf4(:,35), n3(:,62), t3x4(:,:,27))
  call vert_TV_S(ntry, ex5, Q(:,16), wf8(:,7), Q(:,69), wf8(:,35), n3(:,63), t3x8(:,:,35))
  call vert_SV_V(ntry, ex6, wf4(:,15), wf4(:,36), n3(:,64), t3x4(:,:,28))
  call vert_SV_V(ntry, ex5, wf8(:,7), wf8(:,36), n3(:,65), t3x8(:,:,36))
  call vert_TV_S(ntry, ex5, Q(:,16), wf8(:,8), Q(:,69), wf8(:,37), n3(:,66), t3x8(:,:,37))
  call vert_SV_V(ntry, ex5, wf8(:,8), wf8(:,38), n3(:,67), t3x8(:,:,38))
  call vert_TV_S(ntry, ex6, Q(:,32), wf8(:,8), Q(:,69), wf8(:,39), n3(:,68), t3x8(:,:,39))
  call vert_SV_V(ntry, ex6, wf8(:,8), wf8(:,40), n3(:,69), t3x8(:,:,40))


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

    M2munu = M2munu / average_factor_pphhjj_dddxdxhhg_1
  end if

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2add = M2 / average_factor_pphhjj_dddxdxhhg_1

  do k = 0, 30-1
    M2(k) = M2add(extcomb_perm_pphhjj_dddxdxhhg_1(k))
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

    call cont_VV(nsync, wf8(:,1), wf4(:,4), A(:,1), n3(:,70), t3x32(:,:,1), nhel, den(5))
    call cont_VV(nsync, wf4(:,4), wf8(:,2), A(:,2), n3(:,71), t3x32(:,:,2), nhel, den(8))
    call cont_VV(nsync, wf8(:,3), wf4(:,10), A(:,3), n3(:,72), t3x32(:,:,3), nhel, den(13))
    call cont_VV(nsync, wf4(:,10), wf8(:,4), A(:,4), n3(:,73), t3x32(:,:,4), nhel, den(15))
    call cont_VV(nsync, wf8(:,5), wf4(:,12), A(:,5), n3(:,74), t3x32(:,:,5), nhel, den(19))
    call cont_VV(nsync, wf4(:,12), wf8(:,6), A(:,6), n3(:,75), t3x32(:,:,6), nhel, den(22))
    call cont_VV(nsync, wf8(:,7), wf4(:,16), A(:,7), n3(:,76), t3x32(:,:,7), nhel, den(26))
    call cont_VV(nsync, wf4(:,16), wf8(:,8), A(:,8), n3(:,77), t3x32(:,:,8), nhel, den(28))
    call cont_VV(nsync, wf8(:,1), wf4(:,17), A(:,9), n3(:,78), t3x32(:,:,9), nhel, den(31))
    call cont_VV(nsync, wf8(:,2), wf4(:,17), A(:,10), n3(:,79), t3x32(:,:,10), nhel, den(32))
    call cont_SS(nsync, wf4(:,18), wf8(:,9), A(:,11), n3(:,80), t3x32(:,:,11), nhel, den(35))
    call cont_VV(nsync, wf4(:,19), wf8(:,10), A(:,12), n3(:,81), t3x32(:,:,12), nhel, den(35))
    call cont_SS(nsync, wf4(:,20), wf8(:,11), A(:,13), n3(:,82), t3x32(:,:,13), nhel, den(38))
    call cont_VV(nsync, wf4(:,21), wf8(:,12), A(:,14), n3(:,83), t3x32(:,:,14), nhel, den(38))
    call cont_SS(nsync, wf4(:,18), wf8(:,13), A(:,15), n3(:,84), t3x32(:,:,15), nhel, den(39))
    call cont_VV(nsync, wf4(:,19), wf8(:,14), A(:,16), n3(:,85), t3x32(:,:,16), nhel, den(39))
    call cont_SS(nsync, wf4(:,20), wf8(:,15), A(:,17), n3(:,86), t3x32(:,:,17), nhel, den(40))
    call cont_VV(nsync, wf4(:,21), wf8(:,16), A(:,18), n3(:,87), t3x32(:,:,18), nhel, den(40))
    call cont_VV(nsync, wf8(:,3), wf4(:,22), A(:,19), n3(:,88), t3x32(:,:,19), nhel, den(42))
    call cont_VV(nsync, wf8(:,4), wf4(:,22), A(:,20), n3(:,89), t3x32(:,:,20), nhel, den(43))
    call cont_SS(nsync, wf4(:,23), wf8(:,17), A(:,21), n3(:,90), t3x32(:,:,21), nhel, den(46))
    call cont_VV(nsync, wf4(:,24), wf8(:,18), A(:,22), n3(:,91), t3x32(:,:,22), nhel, den(46))
    call cont_SS(nsync, wf4(:,25), wf8(:,19), A(:,23), n3(:,92), t3x32(:,:,23), nhel, den(49))
    call cont_VV(nsync, wf4(:,26), wf8(:,20), A(:,24), n3(:,93), t3x32(:,:,24), nhel, den(49))
    call cont_SS(nsync, wf4(:,25), wf8(:,21), A(:,25), n3(:,94), t3x32(:,:,25), nhel, den(50))
    call cont_VV(nsync, wf4(:,26), wf8(:,22), A(:,26), n3(:,95), t3x32(:,:,26), nhel, den(50))
    call cont_SS(nsync, wf4(:,23), wf8(:,23), A(:,27), n3(:,96), t3x32(:,:,27), nhel, den(51))
    call cont_VV(nsync, wf4(:,24), wf8(:,24), A(:,28), n3(:,97), t3x32(:,:,28), nhel, den(51))
    call cont_VV(nsync, wf8(:,5), wf4(:,27), A(:,29), n3(:,98), t3x32(:,:,29), nhel, den(53))
    call cont_VV(nsync, wf8(:,6), wf4(:,27), A(:,30), n3(:,99), t3x32(:,:,30), nhel, den(54))
    call cont_SS(nsync, wf4(:,28), wf8(:,25), A(:,31), n3(:,100), t3x32(:,:,31), nhel, den(57))
    call cont_VV(nsync, wf4(:,29), wf8(:,26), A(:,32), n3(:,101), t3x32(:,:,32), nhel, den(57))
    call cont_SS(nsync, wf4(:,30), wf8(:,27), A(:,33), n3(:,102), t3x32(:,:,33), nhel, den(60))
    call cont_VV(nsync, wf4(:,31), wf8(:,28), A(:,34), n3(:,103), t3x32(:,:,34), nhel, den(60))
    call cont_SS(nsync, wf4(:,28), wf8(:,29), A(:,35), n3(:,104), t3x32(:,:,35), nhel, den(61))
    call cont_VV(nsync, wf4(:,29), wf8(:,30), A(:,36), n3(:,105), t3x32(:,:,36), nhel, den(61))
    call cont_SS(nsync, wf4(:,30), wf8(:,31), A(:,37), n3(:,106), t3x32(:,:,37), nhel, den(62))
    call cont_VV(nsync, wf4(:,31), wf8(:,32), A(:,38), n3(:,107), t3x32(:,:,38), nhel, den(62))
    call cont_VV(nsync, wf8(:,7), wf4(:,32), A(:,39), n3(:,108), t3x32(:,:,39), nhel, den(64))
    call cont_VV(nsync, wf8(:,8), wf4(:,32), A(:,40), n3(:,109), t3x32(:,:,40), nhel, den(65))
    call cont_SS(nsync, wf4(:,33), wf8(:,33), A(:,41), n3(:,110), t3x32(:,:,41), nhel, den(68))
    call cont_VV(nsync, wf4(:,34), wf8(:,34), A(:,42), n3(:,111), t3x32(:,:,42), nhel, den(68))
    call cont_SS(nsync, wf4(:,35), wf8(:,35), A(:,43), n3(:,112), t3x32(:,:,43), nhel, den(71))
    call cont_VV(nsync, wf4(:,36), wf8(:,36), A(:,44), n3(:,113), t3x32(:,:,44), nhel, den(71))
    call cont_SS(nsync, wf4(:,35), wf8(:,37), A(:,45), n3(:,114), t3x32(:,:,45), nhel, den(72))
    call cont_VV(nsync, wf4(:,36), wf8(:,38), A(:,46), n3(:,115), t3x32(:,:,46), nhel, den(72))
    call cont_SS(nsync, wf4(:,33), wf8(:,39), A(:,47), n3(:,116), t3x32(:,:,47), nhel, den(73))
    call cont_VV(nsync, wf4(:,34), wf8(:,40), A(:,48), n3(:,117), t3x32(:,:,48), nhel, den(73))

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
  complex(REALKIND), intent(out) :: M1(4) ! M1helarray(4,32)
  integer :: empty(0)

  M1(1) = (-A(j,41)%j-A(j,43)%j-A(j,45)%j-A(j,47)%j)*f(1)+(-A(j,7)%j-A(j,8)%j)*f(2)+(-A(j,39)%j-A(j,40)%j)*f(3)+(-A(j,42)%j &
       -A(j,44)%j-A(j,46)%j-A(j,48)%j)*f(4)
  M1(2) = (A(j,31)%j+A(j,33)%j+A(j,35)%j+A(j,37)%j)*f(1)+(A(j,5)%j+A(j,6)%j)*f(2)+(A(j,29)%j+A(j,30)%j)*f(3)+(A(j,32)%j+A(j,34)%j &
       +A(j,36)%j+A(j,38)%j)*f(4)
  M1(3) = (A(j,21)%j+A(j,23)%j+A(j,25)%j+A(j,27)%j)*f(1)+(A(j,3)%j+A(j,4)%j)*f(2)+(A(j,19)%j+A(j,20)%j)*f(3)+(A(j,22)%j+A(j,24)%j &
       +A(j,26)%j+A(j,28)%j)*f(4)
  M1(4) = (-A(j,11)%j-A(j,13)%j-A(j,15)%j-A(j,17)%j)*f(1)+(-A(j,1)%j-A(j,2)%j)*f(2)+(-A(j,9)%j-A(j,10)%j)*f(3)+(-A(j,12)%j &
       -A(j,14)%j-A(j,16)%j-A(j,18)%j)*f(4)

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
  use ol_colourmatrix_pphhjj_dddxdxhhg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(4)
  real(REALKIND),    intent(out) :: M2colint(0:30-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 4*extcomb
    do i = 1, 4
      do j = 1, 4
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
  use ol_colourmatrix_pphhjj_dddxdxhhg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(4), M2(4)
  real(REALKIND),    intent(out) :: M2colint(0:30-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 4*extcomb
    do i = 1, 4
      do j = 1, 4
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
  use ol_colourmatrix_pphhjj_dddxdxhhg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(4)
  complex(REALKIND), intent(in)  :: M2(4)
  complex(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 4
    do j = 1, 4
      M2colint = M2colint + M1(i)*K1(i,j)*conjg(M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_pphhjj_dddxdxhhg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(4,32)
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
    & bind(c,name="ol_f_amp2tree_pphhjj_dddxdxhhg_1")
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
    & bind(c,name="ol_f_amp2ccone_pphhjj_dddxdxhhg_1")
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
    & bind(c,name="ol_f_amp2ccall_pphhjj_dddxdxhhg_1")
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
    & bind(c,name="ol_f_amp2hcone_pphhjj_dddxdxhhg_1")
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
    & bind(c,name="ol_f_amp2hcall_pphhjj_dddxdxhhg_1")
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
    & bind(c,name="ol_amp2tree_pphhjj_dddxdxhhg_1")
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
    & bind(c,name="ol_amp2ccone_pphhjj_dddxdxhhg_1")
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
    & bind(c,name="ol_amp2ccall_pphhjj_dddxdxhhg_1")
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
    & bind(c,name="ol_amp2hcone_pphhjj_dddxdxhhg_1")
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
    & bind(c,name="ol_amp2hcall_pphhjj_dddxdxhhg_1")
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
    & bind(c,name="amp2tree_pphhjj_dddxdxhhg_1_")
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
    & bind(c,name="amp2ccone_pphhjj_dddxdxhhg_1_")
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
    & bind(c,name="amp2ccall_pphhjj_dddxdxhhg_1_")
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
    & bind(c,name="amp2hcone_pphhjj_dddxdxhhg_1_")
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
    & bind(c,name="amp2hcall_pphhjj_dddxdxhhg_1_")
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

end module ol_tree_pphhjj_dddxdxhhg_1_/**/REALKIND
