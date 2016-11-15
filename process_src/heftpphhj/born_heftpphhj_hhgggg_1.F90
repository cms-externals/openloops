
module ol_colourmatrix_heftpphhj_hhgggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(138,6)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1(  1,:) = [  38,  -4,  -4,  -4,  -4,   8]
  K1(  2,:) = [  -4,  38,  -4,   8,  -4,  -4]
  K1(  3,:) = [  -4,  -4,  38,  -4,   8,  -4]
  K1(  4,:) = [  -4,   8,  -4,  38,  -4,  -4]
  K1(  5,:) = [  -4,  -4,   8,  -4,  38,  -4]
  K1(  6,:) = [   8,  -4,  -4,  -4,  -4,  38]
  K1(  7,:) = [   0,   0,   0,   0,   0,   0]
  K1(  8,:) = [   0,   0,   0,   0,   0,   0]
  K1(  9,:) = [   0,   0,   0,   0,   0,   0]
  K1( 10,:) = [   0,   0,   0,   0,   0,   0]
  K1( 11,:) = [   0,   0,   0,   0,   0,   0]
  K1( 12,:) = [   0,   0,   0,   0,   0,   0]
  K1( 13,:) = [   0,   0,   0,   0,   0,   0]
  K1( 14,:) = [   0,   0,   0,   0,   0,   0]
  K1( 15,:) = [   0,   0,   0,   0,   0,   0]
  K1( 16,:) = [   0,   0,   0,   0,   0,   0]
  K1( 17,:) = [   0,   0,   0,   0,   0,   0]
  K1( 18,:) = [   0,   0,   0,   0,   0,   0]
  K1( 19,:) = [   0,   0,   0,   0,   0,   0]
  K1( 20,:) = [   0,   0,   0,   0,   0,   0]
  K1( 21,:) = [   0,   0,   0,   0,   0,   0]
  K1( 22,:) = [   0,   0,   0,   0,   0,   0]
  K1( 23,:) = [   0,   0,   0,   0,   0,   0]
  K1( 24,:) = [   0,   0,   0,   0,   0,   0]
  K1( 25,:) = [   0,   0,   0,   0,   0,   0]
  K1( 26,:) = [   0,   0,   0,   0,   0,   0]
  K1( 27,:) = [   0,   0,   0,   0,   0,   0]
  K1( 28,:) = [   0,   0,   0,   0,   0,   0]
  K1( 29,:) = [   0,   0,   0,   0,   0,   0]
  K1( 30,:) = [   0,   0,   0,   0,   0,   0]
  K1( 31,:) = [   0,   0,   0,   0,   0,   0]
  K1( 32,:) = [   0,   0,   0,   0,   0,   0]
  K1( 33,:) = [   0,   0,   0,   0,   0,   0]
  K1( 34,:) = [   0,   0,   0,   0,   0,   0]
  K1( 35,:) = [   0,   0,   0,   0,   0,   0]
  K1( 36,:) = [   0,   0,   0,   0,   0,   0]
  K1( 37,:) = [ 114, -12, -12, -12, -12,  24]
  K1( 38,:) = [ -12, 114, -12,  24, -12, -12]
  K1( 39,:) = [ -12, -12, 114, -12,  24, -12]
  K1( 40,:) = [ -12,  24, -12, 114, -12, -12]
  K1( 41,:) = [ -12, -12,  24, -12, 114, -12]
  K1( 42,:) = [  24, -12, -12, -12, -12, 114]
  K1( 43,:) = [   0,   0,   0,   0,   0,   0]
  K1( 44,:) = [   0,   0,   0,   0,   0,   0]
  K1( 45,:) = [   0,   0,   0,   0,   0,   0]
  K1( 46,:) = [   0,   0,   0,   0,   0,   0]
  K1( 47,:) = [   0,   0,   0,   0,   0,   0]
  K1( 48,:) = [   0,   0,   0,   0,   0,   0]
  K1( 49,:) = [   0,   0,   0,   0,   0,   0]
  K1( 50,:) = [   0,   0,   0,   0,   0,   0]
  K1( 51,:) = [   0,   0,   0,   0,   0,   0]
  K1( 52,:) = [   0,   0,   0,   0,   0,   0]
  K1( 53,:) = [   0,   0,   0,   0,   0,   0]
  K1( 54,:) = [   0,   0,   0,   0,   0,   0]
  K1( 55,:) = [ -65,  -2,   7,  -2,   7, -20]
  K1( 56,:) = [  -2, -65,   7, -20,   7,  -2]
  K1( 57,:) = [   7,   7,  16,   7,  16,   7]
  K1( 58,:) = [  -2, -20,   7, -65,   7,  -2]
  K1( 59,:) = [   7,   7,  16,   7,  16,   7]
  K1( 60,:) = [ -20,  -2,   7,  -2,   7, -65]
  K1( 61,:) = [ 114, -12, -12, -12, -12,  24]
  K1( 62,:) = [ -12, 114, -12,  24, -12, -12]
  K1( 63,:) = [ -12, -12, 114, -12,  24, -12]
  K1( 64,:) = [ -12,  24, -12, 114, -12, -12]
  K1( 65,:) = [ -12, -12,  24, -12, 114, -12]
  K1( 66,:) = [  24, -12, -12, -12, -12, 114]
  K1( 67,:) = [   0,   0,   0,   0,   0,   0]
  K1( 68,:) = [   0,   0,   0,   0,   0,   0]
  K1( 69,:) = [   0,   0,   0,   0,   0,   0]
  K1( 70,:) = [   0,   0,   0,   0,   0,   0]
  K1( 71,:) = [   0,   0,   0,   0,   0,   0]
  K1( 72,:) = [   0,   0,   0,   0,   0,   0]
  K1( 73,:) = [   0,   0,   0,   0,   0,   0]
  K1( 74,:) = [   0,   0,   0,   0,   0,   0]
  K1( 75,:) = [   0,   0,   0,   0,   0,   0]
  K1( 76,:) = [   0,   0,   0,   0,   0,   0]
  K1( 77,:) = [   0,   0,   0,   0,   0,   0]
  K1( 78,:) = [   0,   0,   0,   0,   0,   0]
  K1( 79,:) = [  16,   7,   7,   7,   7,  16]
  K1( 80,:) = [   7, -65,  -2, -20,  -2,   7]
  K1( 81,:) = [   7,  -2, -65,  -2, -20,   7]
  K1( 82,:) = [   7, -20,  -2, -65,  -2,   7]
  K1( 83,:) = [   7,  -2, -20,  -2, -65,   7]
  K1( 84,:) = [  16,   7,   7,   7,   7,  16]
  K1( 85,:) = [ -65,   7,  -2,   7,  -2, -20]
  K1( 86,:) = [   7,  16,   7,  16,   7,   7]
  K1( 87,:) = [  -2,   7, -65,   7, -20,  -2]
  K1( 88,:) = [   7,  16,   7,  16,   7,   7]
  K1( 89,:) = [  -2,   7, -20,   7, -65,  -2]
  K1( 90,:) = [ -20,   7,  -2,   7,  -2, -65]
  K1( 91,:) = [ 114, -12, -12, -12, -12,  24]
  K1( 92,:) = [ -12, 114, -12,  24, -12, -12]
  K1( 93,:) = [ -12, -12, 114, -12,  24, -12]
  K1( 94,:) = [ -12,  24, -12, 114, -12, -12]
  K1( 95,:) = [ -12, -12,  24, -12, 114, -12]
  K1( 96,:) = [  24, -12, -12, -12, -12, 114]
  K1( 97,:) = [   0,   0,   0,   0,   0,   0]
  K1( 98,:) = [   0,   0,   0,   0,   0,   0]
  K1( 99,:) = [   0,   0,   0,   0,   0,   0]
  K1(100,:) = [   0,   0,   0,   0,   0,   0]
  K1(101,:) = [   0,   0,   0,   0,   0,   0]
  K1(102,:) = [   0,   0,   0,   0,   0,   0]
  K1(103,:) = [   0,   0,   0,   0,   0,   0]
  K1(104,:) = [   0,   0,   0,   0,   0,   0]
  K1(105,:) = [   0,   0,   0,   0,   0,   0]
  K1(106,:) = [   0,   0,   0,   0,   0,   0]
  K1(107,:) = [   0,   0,   0,   0,   0,   0]
  K1(108,:) = [   0,   0,   0,   0,   0,   0]
  K1(109,:) = [ -65,   7,  -2,   7,  -2, -20]
  K1(110,:) = [   7,  16,   7,  16,   7,   7]
  K1(111,:) = [  -2,   7, -65,   7, -20,  -2]
  K1(112,:) = [   7,  16,   7,  16,   7,   7]
  K1(113,:) = [  -2,   7, -20,   7, -65,  -2]
  K1(114,:) = [ -20,   7,  -2,   7,  -2, -65]
  K1(115,:) = [  16,   7,   7,   7,   7,  16]
  K1(116,:) = [   7, -65,  -2, -20,  -2,   7]
  K1(117,:) = [   7,  -2, -65,  -2, -20,   7]
  K1(118,:) = [   7, -20,  -2, -65,  -2,   7]
  K1(119,:) = [   7,  -2, -20,  -2, -65,   7]
  K1(120,:) = [  16,   7,   7,   7,   7,  16]
  K1(121,:) = [ -65,  -2,   7,  -2,   7, -20]
  K1(122,:) = [  -2, -65,   7, -20,   7,  -2]
  K1(123,:) = [   7,   7,  16,   7,  16,   7]
  K1(124,:) = [  -2, -20,   7, -65,   7,  -2]
  K1(125,:) = [   7,   7,  16,   7,  16,   7]
  K1(126,:) = [ -20,  -2,   7,  -2,   7, -65]
  K1(127,:) = [ 114, -12, -12, -12, -12,  24]
  K1(128,:) = [ -12, 114, -12,  24, -12, -12]
  K1(129,:) = [ -12, -12, 114, -12,  24, -12]
  K1(130,:) = [ -12,  24, -12, 114, -12, -12]
  K1(131,:) = [ -12, -12,  24, -12, 114, -12]
  K1(132,:) = [  24, -12, -12, -12, -12, 114]
  K1(133,:) = [   0,   0,   0,   0,   0,   0]
  K1(134,:) = [   0,   0,   0,   0,   0,   0]
  K1(135,:) = [   0,   0,   0,   0,   0,   0]
  K1(136,:) = [   0,   0,   0,   0,   0,   0]
  K1(137,:) = [   0,   0,   0,   0,   0,   0]
  K1(138,:) = [   0,   0,   0,   0,   0,   0]
  K1 = (1._/**/REALKIND / 12) * K1

  end subroutine colourmatrix_init
end module ol_colourmatrix_heftpphhj_hhgggg_1_/**/REALKIND



module ol_forced_parameters_heftpphhj_hhgggg_1_/**/REALKIND
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
  if (MB /= 0) write(*,101) 'MB = 0'
  if (YB /= 0) write(*,101) 'YB = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 5) write(*,101) 'nf = 5'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_heftpphhj_hhgggg_1_/**/REALKIND

module ol_tree_heftpphhj_hhgggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(2)
  complex(REALKIND), save :: den(64)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 16 ! number of helicity configurations
  integer(intkind2), save :: nhel = 16 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(16) ! physical helicity states
  complex(DREALKIND) :: M1helarr(6,16) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED**2*gQCD**4)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f(2) = (CI*eQED**2*gQCD**4*lambdaHHH*MH**2)/(16._/**/REALKIND*MW**2*pi**2*sw**2)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,3) - MH2)
  den(2) = 1 / (Q(5,7))
  den(3) = 1 / (Q(5,11))
  den(4) = 1 / (Q(5,19))
  den(5) = 1 / (Q(5,35))
  den(6) = 1 / (Q(5,12))
  den(7) = 1 / (Q(5,20))
  den(8) = 1 / (Q(5,36))
  den(9) = 1 / (Q(5,24))
  den(10) = 1 / (Q(5,40))
  den(11) = 1 / (Q(5,48))
  den(15) = 1 / (Q(5,56))
  den(19) = 1 / (Q(5,52))
  den(22) = 1 / (Q(5,44))
  den(24) = 1 / (Q(5,28))

  ! denominators

  den(12) = den(1)*den(6)
  den(13) = den(1)*den(7)
  den(14) = den(1)*den(8)
  den(16) = den(1)*den(15)
  den(17) = den(1)*den(9)
  den(18) = den(1)*den(10)
  den(20) = den(1)*den(19)
  den(21) = den(1)*den(11)
  den(23) = den(1)*den(22)
  den(25) = den(1)*den(24)
  den(26) = den(6)*den(11)
  den(27) = den(5)*den(6)
  den(28) = den(4)*den(6)
  den(29) = den(7)*den(10)
  den(30) = den(5)*den(7)
  den(31) = den(8)*den(9)
  den(32) = den(5)*den(9)
  den(33) = den(4)*den(8)
  den(34) = den(4)*den(10)
  den(35) = den(3)*den(7)
  den(36) = den(3)*den(8)
  den(37) = den(3)*den(11)
  den(38) = den(2)*den(9)
  den(39) = den(2)*den(10)
  den(40) = den(2)*den(11)
  den(41) = den(11)*den(12)
  den(42) = den(1)*den(4)
  den(43) = den(6)*den(42)
  den(44) = den(6)*den(24)
  den(45) = den(1)*den(44)
  den(46) = den(10)*den(13)
  den(47) = den(1)*den(3)
  den(48) = den(7)*den(47)
  den(49) = den(7)*den(24)
  den(50) = den(1)*den(49)
  den(51) = den(9)*den(14)
  den(52) = den(1)*den(2)
  den(53) = den(9)*den(52)
  den(54) = den(9)*den(24)
  den(55) = den(1)*den(54)
  den(56) = den(8)*den(47)
  den(57) = den(8)*den(22)
  den(58) = den(1)*den(57)
  den(59) = den(10)*den(52)
  den(60) = den(10)*den(22)
  den(61) = den(1)*den(60)
  den(62) = den(11)*den(52)
  den(63) = den(11)*den(19)
  den(64) = den(1)*den(63)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_heftpphhj_hhgggg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_heftpphhj_hhgggg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for higgs higgs glue glue glue glue -> 0
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
  use ol_external_heftpphhj_hhgggg_1, only: external_perm_heftpphhj_hhgggg_1, &
    & external_perm_inv_heftpphhj_hhgggg_1, extcomb_perm_heftpphhj_hhgggg_1, &
    & average_factor_heftpphhj_hhgggg_1
  use ol_external_heftpphhj_hhgggg_1, only: H, hel_not_initialised, hel_init, POLSEL
  use ol_colourmatrix_heftpphhj_hhgggg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_heftpphhj_hhgggg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_heftpphhj_hhgggg_1
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
  complex(REALKIND) :: MOM_LC(4), M1(6), M1helarray(6,16)
  real(REALKIND)    :: P_scatt_intern(0:3,6)
  complex(REALKIND) :: epLC(1:4), epStd(1:4,2)
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(1), ex2(1), ex3(2), ex4(2), ex5(2), ex6(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf1(1,1), wf2(2,8), wf4(4,24), wf8(8,27), wf16(16,75)

  type(polcont) :: A(16,72)
  complex(REALKIND) :: Aj(72)

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
  extmasses2 = [ rMH2, rMH2, rZERO2, rZERO2, rZERO2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_heftpphhj_hhgggg_1,6)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,6)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_heftpphhj_hhgggg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_heftpphhj_hhgggg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_heftpphhj_hhgggg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  42 continue
  ! external WFs
  call pol_wf_S(P(:,1), rMH, H1, ex1, POLSEL(1))
  call pol_wf_S(P(:,2), rMH, H2, ex2, POLSEL(2))
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
      call pol_wf_S(P(:,2), rMH, H2, ex2, 0)
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
  call vert_HHGGG_G(ntry, ex1, ex2, ex3, ex4, ex5, wf8(:,1), n6(:,1), t6x8(:,:,1))
  call vert_HHGGG_G(ntry, ex1, ex2, ex4, ex5, ex3, wf8(:,2), n6(:,2), t6x8(:,:,2))
  call vert_HHGGG_G(ntry, ex1, ex2, ex5, ex3, ex4, wf8(:,3), n6(:,3), t6x8(:,:,3))
  call vert_SS_S(ntry, ex1, ex2, wf1(:,1), n3(:,1), t3x1(:,:,1))
  call vert_GGGG_H(ntry, ex3, ex4, ex5, ex6, wf16(:,1), n5(:,1), t5x16(:,:,1))
  call vert_GGGG_H(ntry, ex3, ex5, ex6, ex4, wf16(:,2), n5(:,2), t5x16(:,:,2))
  call vert_GGGG_H(ntry, ex3, ex6, ex4, ex5, wf16(:,3), n5(:,3), t5x16(:,:,3))
  call vert_HHG_G(ntry, ex1, ex2, ex3, Q(:,4), wf2(:,1), Q(:,7), n4(:,1), t4x2(:,:,1))
  call vert_GGG_G(ntry, ex4, ex5, ex6, wf8(:,4), n4(:,2), t4x8(:,:,1))
  call vert_GGG_G(ntry, ex5, ex6, ex4, wf8(:,5), n4(:,3), t4x8(:,:,2))
  call vert_GGG_G(ntry, ex6, ex4, ex5, wf8(:,6), n4(:,4), t4x8(:,:,3))
  call vert_HHG_G(ntry, ex1, ex2, ex4, Q(:,8), wf2(:,2), Q(:,11), n4(:,5), t4x2(:,:,2))
  call vert_GGG_G(ntry, ex3, ex5, ex6, wf8(:,7), n4(:,6), t4x8(:,:,4))
  call vert_GGG_G(ntry, ex5, ex6, ex3, wf8(:,8), n4(:,7), t4x8(:,:,5))
  call vert_GGG_G(ntry, ex6, ex3, ex5, wf8(:,9), n4(:,8), t4x8(:,:,6))
  call vert_HHG_G(ntry, ex1, ex2, ex5, Q(:,16), wf2(:,3), Q(:,19), n4(:,9), t4x2(:,:,3))
  call vert_GGG_G(ntry, ex3, ex4, ex6, wf8(:,10), n4(:,10), t4x8(:,:,7))
  call vert_GGG_G(ntry, ex4, ex6, ex3, wf8(:,11), n4(:,11), t4x8(:,:,8))
  call vert_GGG_G(ntry, ex6, ex3, ex4, wf8(:,12), n4(:,12), t4x8(:,:,9))
  call vert_HHG_G(ntry, ex1, ex2, ex6, Q(:,32), wf2(:,4), Q(:,35), n4(:,13), t4x2(:,:,4))
  call vert_GGG_G(ntry, ex3, ex4, ex5, wf8(:,13), n4(:,14), t4x8(:,:,10))
  call vert_GGG_G(ntry, ex4, ex5, ex3, wf8(:,14), n4(:,15), t4x8(:,:,11))
  call vert_GGG_G(ntry, ex5, ex3, ex4, wf8(:,15), n4(:,16), t4x8(:,:,12))
  call vert_UV_W(ntry, ex3, Q(:,4), ex4, Q(:,8), wf4(:,1), n3(:,2), t3x4(:,:,1))
  call vert_HHGG_G(ntry, ex1, ex2, ex5, Q(:,16), ex6, Q(:,32), wf4(:,2), Q(:,51), n5(:,4), t5x4(:,:,1))
  call vert_UV_W(ntry, ex3, Q(:,4), ex5, Q(:,16), wf4(:,3), n3(:,3), t3x4(:,:,2))
  call vert_HHGG_G(ntry, ex1, ex2, ex4, Q(:,8), ex6, Q(:,32), wf4(:,4), Q(:,43), n5(:,5), t5x4(:,:,2))
  call vert_UV_W(ntry, ex3, Q(:,4), ex6, Q(:,32), wf4(:,5), n3(:,4), t3x4(:,:,3))
  call vert_HHGG_G(ntry, ex1, ex2, ex4, Q(:,8), ex5, Q(:,16), wf4(:,6), Q(:,27), n5(:,6), t5x4(:,:,3))
  call vert_UV_W(ntry, ex4, Q(:,8), ex5, Q(:,16), wf4(:,7), n3(:,5), t3x4(:,:,4))
  call vert_HHGG_G(ntry, ex1, ex2, ex3, Q(:,4), ex6, Q(:,32), wf4(:,8), Q(:,39), n5(:,7), t5x4(:,:,4))
  call vert_UV_W(ntry, ex4, Q(:,8), ex6, Q(:,32), wf4(:,9), n3(:,6), t3x4(:,:,5))
  call vert_HHGG_G(ntry, ex1, ex2, ex3, Q(:,4), ex5, Q(:,16), wf4(:,10), Q(:,23), n5(:,8), t5x4(:,:,5))
  call vert_UV_W(ntry, ex5, Q(:,16), ex6, Q(:,32), wf4(:,11), n3(:,7), t3x4(:,:,6))
  call vert_HHGG_G(ntry, ex1, ex2, ex3, Q(:,4), ex4, Q(:,8), wf4(:,12), Q(:,15), n5(:,9), t5x4(:,:,6))
  call vert_HGG_G(ntry, wf1(:,1), ex5, Q(:,16), ex6, Q(:,32), wf4(:,13), Q(:,51), n4(:,17), t4x4(:,:,1))
  call vert_HGG_G(ntry, wf1(:,1), ex4, Q(:,8), ex6, Q(:,32), wf4(:,14), Q(:,43), n4(:,18), t4x4(:,:,2))
  call vert_HGG_G(ntry, wf1(:,1), ex4, Q(:,8), ex5, Q(:,16), wf4(:,15), Q(:,27), n4(:,19), t4x4(:,:,3))
  call vert_HG_G(ntry, wf1(:,1), ex3, Q(:,4), wf2(:,5), Q(:,7), n3(:,8), t3x2(:,:,1))
  call vert_HGG_G(ntry, wf1(:,1), ex3, Q(:,4), ex6, Q(:,32), wf4(:,16), Q(:,39), n4(:,20), t4x4(:,:,4))
  call vert_HGG_G(ntry, wf1(:,1), ex3, Q(:,4), ex5, Q(:,16), wf4(:,17), Q(:,23), n4(:,21), t4x4(:,:,5))
  call vert_HG_G(ntry, wf1(:,1), ex4, Q(:,8), wf2(:,6), Q(:,11), n3(:,9), t3x2(:,:,2))
  call vert_HGG_G(ntry, wf1(:,1), ex3, Q(:,4), ex4, Q(:,8), wf4(:,18), Q(:,15), n4(:,22), t4x4(:,:,6))
  call vert_HG_G(ntry, wf1(:,1), ex5, Q(:,16), wf2(:,7), Q(:,19), n3(:,10), t3x2(:,:,3))
  call vert_HG_G(ntry, wf1(:,1), ex6, Q(:,32), wf2(:,8), Q(:,35), n3(:,11), t3x2(:,:,4))
  call vert_HHG_G(ntry, ex1, ex2, wf4(:,1), Q(:,12), wf4(:,19), Q(:,15), n4(:,23), t4x4(:,:,7))
  call vert_UV_W(ntry, wf4(:,1), Q(:,12), ex5, Q(:,16), wf8(:,16), n3(:,12), t3x8(:,:,1))
  call vert_UV_W(ntry, wf4(:,1), Q(:,12), ex6, Q(:,32), wf8(:,17), n3(:,13), t3x8(:,:,2))
  call vert_HHG_G(ntry, ex1, ex2, wf4(:,3), Q(:,20), wf4(:,20), Q(:,23), n4(:,24), t4x4(:,:,8))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,3), Q(:,20), wf8(:,18), n3(:,14), t3x8(:,:,3))
  call vert_HHG_G(ntry, ex1, ex2, wf4(:,5), Q(:,36), wf4(:,21), Q(:,39), n4(:,25), t4x4(:,:,9))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,7), Q(:,24), wf8(:,19), n3(:,15), t3x8(:,:,4))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,5), Q(:,36), wf8(:,20), n3(:,16), t3x8(:,:,5))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,9), Q(:,40), wf8(:,21), n3(:,17), t3x8(:,:,6))
  call vert_UV_W(ntry, wf4(:,3), Q(:,20), ex6, Q(:,32), wf8(:,22), n3(:,18), t3x8(:,:,7))
  call vert_UV_W(ntry, ex5, Q(:,16), wf4(:,5), Q(:,36), wf8(:,23), n3(:,19), t3x8(:,:,8))
  call vert_UV_W(ntry, ex3, Q(:,4), wf4(:,11), Q(:,48), wf8(:,24), n3(:,20), t3x8(:,:,9))
  call vert_UV_W(ntry, wf4(:,7), Q(:,24), ex6, Q(:,32), wf8(:,25), n3(:,21), t3x8(:,:,10))
  call vert_UV_W(ntry, ex5, Q(:,16), wf4(:,9), Q(:,40), wf8(:,26), n3(:,22), t3x8(:,:,11))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,11), Q(:,48), wf8(:,27), n3(:,23), t3x8(:,:,12))
  call vert_HG_G(ntry, wf1(:,1), wf4(:,1), Q(:,12), wf4(:,22), Q(:,15), n3(:,24), t3x4(:,:,7))
  call vert_HG_G(ntry, wf1(:,1), wf4(:,3), Q(:,20), wf4(:,23), Q(:,23), n3(:,25), t3x4(:,:,8))
  call vert_HG_G(ntry, wf1(:,1), wf4(:,5), Q(:,36), wf4(:,24), Q(:,39), n3(:,26), t3x4(:,:,9))


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

    M2munu = M2munu / average_factor_heftpphhj_hhgggg_1
  end if

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2add = M2 / average_factor_heftpphhj_hhgggg_1

  do k = 0, 23-1
    M2(k) = M2add(extcomb_perm_heftpphhj_hhgggg_1(k))
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

    call cont_VV(nsync, ex6, wf8(:,1), A(:,1), n3(:,27), t3x16(:,:,1), nhel, cONE)
    call cont_VV(nsync, ex6, wf8(:,2), A(:,2), n3(:,28), t3x16(:,:,2), nhel, cONE)
    call cont_VV(nsync, ex6, wf8(:,3), A(:,3), n3(:,29), t3x16(:,:,3), nhel, cONE)
    call cont_SS(nsync, wf1(:,1), wf16(:,1), A(:,4), n3(:,30), t3x16(:,:,4), nhel, den(1))
    call cont_SS(nsync, wf1(:,1), wf16(:,2), A(:,5), n3(:,31), t3x16(:,:,5), nhel, den(1))
    call cont_SS(nsync, wf1(:,1), wf16(:,3), A(:,6), n3(:,32), t3x16(:,:,6), nhel, den(1))
    call cont_VV(nsync, wf2(:,1), wf8(:,4), A(:,7), n3(:,33), t3x16(:,:,7), nhel, den(2))
    call cont_VV(nsync, wf2(:,1), wf8(:,5), A(:,8), n3(:,34), t3x16(:,:,8), nhel, den(2))
    call cont_VV(nsync, wf2(:,1), wf8(:,6), A(:,9), n3(:,35), t3x16(:,:,9), nhel, den(2))
    call cont_VV(nsync, wf2(:,2), wf8(:,7), A(:,10), n3(:,36), t3x16(:,:,10), nhel, den(3))
    call cont_VV(nsync, wf2(:,2), wf8(:,8), A(:,11), n3(:,37), t3x16(:,:,11), nhel, den(3))
    call cont_VV(nsync, wf2(:,2), wf8(:,9), A(:,12), n3(:,38), t3x16(:,:,12), nhel, den(3))
    call cont_VV(nsync, wf2(:,3), wf8(:,10), A(:,13), n3(:,39), t3x16(:,:,13), nhel, den(4))
    call cont_VV(nsync, wf2(:,3), wf8(:,11), A(:,14), n3(:,40), t3x16(:,:,14), nhel, den(4))
    call cont_VV(nsync, wf2(:,3), wf8(:,12), A(:,15), n3(:,41), t3x16(:,:,15), nhel, den(4))
    call cont_VV(nsync, wf2(:,4), wf8(:,13), A(:,16), n3(:,42), t3x16(:,:,16), nhel, den(5))
    call cont_VV(nsync, wf2(:,4), wf8(:,14), A(:,17), n3(:,43), t3x16(:,:,17), nhel, den(5))
    call cont_VV(nsync, wf2(:,4), wf8(:,15), A(:,18), n3(:,44), t3x16(:,:,18), nhel, den(5))
    call cont_VV(nsync, wf4(:,1), wf4(:,2), A(:,19), n3(:,45), t3x16(:,:,19), nhel, den(6))
    call cont_VV(nsync, wf4(:,3), wf4(:,4), A(:,20), n3(:,46), t3x16(:,:,20), nhel, den(7))
    call cont_VV(nsync, wf4(:,5), wf4(:,6), A(:,21), n3(:,47), t3x16(:,:,21), nhel, den(8))
    call cont_VV(nsync, wf4(:,7), wf4(:,8), A(:,22), n3(:,48), t3x16(:,:,22), nhel, den(9))
    call cont_VV(nsync, wf4(:,9), wf4(:,10), A(:,23), n3(:,49), t3x16(:,:,23), nhel, den(10))
    call cont_VV(nsync, wf4(:,11), wf4(:,12), A(:,24), n3(:,50), t3x16(:,:,24), nhel, den(11))
    call cont_VV(nsync, wf4(:,1), wf4(:,13), A(:,25), n3(:,51), t3x16(:,:,25), nhel, den(12))
    call cont_VV(nsync, wf4(:,3), wf4(:,14), A(:,26), n3(:,52), t3x16(:,:,26), nhel, den(13))
    call cont_VV(nsync, wf4(:,5), wf4(:,15), A(:,27), n3(:,53), t3x16(:,:,27), nhel, den(14))
    call cont_VV(nsync, wf8(:,4), wf2(:,5), A(:,28), n3(:,54), t3x16(:,:,28), nhel, den(16))
    call cont_VV(nsync, wf8(:,5), wf2(:,5), A(:,29), n3(:,55), t3x16(:,:,29), nhel, den(16))
    call cont_VV(nsync, wf8(:,6), wf2(:,5), A(:,30), n3(:,56), t3x16(:,:,30), nhel, den(16))
    call cont_VV(nsync, wf4(:,7), wf4(:,16), A(:,31), n3(:,57), t3x16(:,:,31), nhel, den(17))
    call cont_VV(nsync, wf4(:,9), wf4(:,17), A(:,32), n3(:,58), t3x16(:,:,32), nhel, den(18))
    call cont_VV(nsync, wf8(:,7), wf2(:,6), A(:,33), n3(:,59), t3x16(:,:,33), nhel, den(20))
    call cont_VV(nsync, wf8(:,8), wf2(:,6), A(:,34), n3(:,60), t3x16(:,:,34), nhel, den(20))
    call cont_VV(nsync, wf8(:,9), wf2(:,6), A(:,35), n3(:,61), t3x16(:,:,35), nhel, den(20))
    call cont_VV(nsync, wf4(:,11), wf4(:,18), A(:,36), n3(:,62), t3x16(:,:,36), nhel, den(21))
    call cont_VV(nsync, wf8(:,10), wf2(:,7), A(:,37), n3(:,63), t3x16(:,:,37), nhel, den(23))
    call cont_VV(nsync, wf8(:,11), wf2(:,7), A(:,38), n3(:,64), t3x16(:,:,38), nhel, den(23))
    call cont_VV(nsync, wf8(:,12), wf2(:,7), A(:,39), n3(:,65), t3x16(:,:,39), nhel, den(23))
    call cont_VV(nsync, wf8(:,13), wf2(:,8), A(:,40), n3(:,66), t3x16(:,:,40), nhel, den(25))
    call cont_VV(nsync, wf8(:,14), wf2(:,8), A(:,41), n3(:,67), t3x16(:,:,41), nhel, den(25))
    call cont_VV(nsync, wf8(:,15), wf2(:,8), A(:,42), n3(:,68), t3x16(:,:,42), nhel, den(25))
    call cont_VV(nsync, wf4(:,11), wf4(:,19), A(:,43), n3(:,69), t3x16(:,:,43), nhel, den(26))
    call cont_VV(nsync, wf2(:,4), wf8(:,16), A(:,44), n3(:,70), t3x16(:,:,44), nhel, den(27))
    call cont_VV(nsync, wf2(:,3), wf8(:,17), A(:,45), n3(:,71), t3x16(:,:,45), nhel, den(28))
    call cont_VV(nsync, wf4(:,9), wf4(:,20), A(:,46), n3(:,72), t3x16(:,:,46), nhel, den(29))
    call cont_VV(nsync, wf2(:,4), wf8(:,18), A(:,47), n3(:,73), t3x16(:,:,47), nhel, den(30))
    call cont_VV(nsync, wf4(:,7), wf4(:,21), A(:,48), n3(:,74), t3x16(:,:,48), nhel, den(31))
    call cont_VV(nsync, wf2(:,4), wf8(:,19), A(:,49), n3(:,75), t3x16(:,:,49), nhel, den(32))
    call cont_VV(nsync, wf2(:,3), wf8(:,20), A(:,50), n3(:,76), t3x16(:,:,50), nhel, den(33))
    call cont_VV(nsync, wf2(:,3), wf8(:,21), A(:,51), n3(:,77), t3x16(:,:,51), nhel, den(34))
    call cont_VV(nsync, wf2(:,2), wf8(:,22), A(:,52), n3(:,78), t3x16(:,:,52), nhel, den(35))
    call cont_VV(nsync, wf2(:,2), wf8(:,23), A(:,53), n3(:,79), t3x16(:,:,53), nhel, den(36))
    call cont_VV(nsync, wf2(:,2), wf8(:,24), A(:,54), n3(:,80), t3x16(:,:,54), nhel, den(37))
    call cont_VV(nsync, wf2(:,1), wf8(:,25), A(:,55), n3(:,81), t3x16(:,:,55), nhel, den(38))
    call cont_VV(nsync, wf2(:,1), wf8(:,26), A(:,56), n3(:,82), t3x16(:,:,56), nhel, den(39))
    call cont_VV(nsync, wf2(:,1), wf8(:,27), A(:,57), n3(:,83), t3x16(:,:,57), nhel, den(40))
    call cont_VV(nsync, wf4(:,11), wf4(:,22), A(:,58), n3(:,84), t3x16(:,:,58), nhel, den(41))
    call cont_VV(nsync, wf2(:,7), wf8(:,17), A(:,59), n3(:,85), t3x16(:,:,59), nhel, den(43))
    call cont_VV(nsync, wf2(:,8), wf8(:,16), A(:,60), n3(:,86), t3x16(:,:,60), nhel, den(45))
    call cont_VV(nsync, wf4(:,9), wf4(:,23), A(:,61), n3(:,87), t3x16(:,:,61), nhel, den(46))
    call cont_VV(nsync, wf2(:,6), wf8(:,22), A(:,62), n3(:,88), t3x16(:,:,62), nhel, den(48))
    call cont_VV(nsync, wf2(:,8), wf8(:,18), A(:,63), n3(:,89), t3x16(:,:,63), nhel, den(50))
    call cont_VV(nsync, wf4(:,7), wf4(:,24), A(:,64), n3(:,90), t3x16(:,:,64), nhel, den(51))
    call cont_VV(nsync, wf2(:,5), wf8(:,25), A(:,65), n3(:,91), t3x16(:,:,65), nhel, den(53))
    call cont_VV(nsync, wf2(:,8), wf8(:,19), A(:,66), n3(:,92), t3x16(:,:,66), nhel, den(55))
    call cont_VV(nsync, wf2(:,6), wf8(:,23), A(:,67), n3(:,93), t3x16(:,:,67), nhel, den(56))
    call cont_VV(nsync, wf2(:,7), wf8(:,20), A(:,68), n3(:,94), t3x16(:,:,68), nhel, den(58))
    call cont_VV(nsync, wf2(:,5), wf8(:,26), A(:,69), n3(:,95), t3x16(:,:,69), nhel, den(59))
    call cont_VV(nsync, wf2(:,7), wf8(:,21), A(:,70), n3(:,96), t3x16(:,:,70), nhel, den(61))
    call cont_VV(nsync, wf2(:,5), wf8(:,27), A(:,71), n3(:,97), t3x16(:,:,71), nhel, den(62))
    call cont_VV(nsync, wf2(:,6), wf8(:,24), A(:,72), n3(:,98), t3x16(:,:,72), nhel, den(64))

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
  complex(REALKIND), intent(out) :: M1(6) ! M1helarray(6,16)
  integer :: empty(0)

  M1(1) = 2*(A(j,1)%j-A(j,2)%j+A(j,7)%j-A(j,8)%j+A(j,11)%j-A(j,12)%j-A(j,13)%j+A(j,15)%j+A(j,16)%j-A(j,17)%j+A(j,19)%j-A(j,21)%j &
       -A(j,22)%j+A(j,24)%j+A(j,43)%j+A(j,44)%j-A(j,45)%j-A(j,48)%j+A(j,49)%j+A(j,50)%j-A(j,53)%j-A(j,54)%j+A(j,55)%j &
       +A(j,57)%j)*f(1)+2*(-A(j,4)%j+A(j,6)%j-A(j,25)%j+A(j,27)%j-A(j,28)%j+A(j,29)%j+A(j,31)%j-A(j,34)%j+A(j,35)%j-A(j,36)%j &
       +A(j,37)%j-A(j,39)%j-A(j,40)%j+A(j,41)%j-A(j,58)%j+A(j,59)%j-A(j,60)%j+A(j,64)%j-A(j,65)%j-A(j,66)%j+A(j,67)%j-A(j,68)%j &
       -A(j,71)%j+A(j,72)%j)*f(2)
  M1(2) = 2*(-A(j,1)%j+A(j,3)%j+A(j,8)%j-A(j,9)%j+A(j,10)%j-A(j,11)%j+A(j,13)%j-A(j,14)%j-A(j,16)%j+A(j,18)%j-A(j,19)%j-A(j,20)%j &
       -A(j,23)%j-A(j,24)%j-A(j,43)%j-A(j,44)%j+A(j,45)%j-A(j,46)%j+A(j,47)%j+A(j,51)%j+A(j,52)%j+A(j,54)%j-A(j,56)%j &
       -A(j,57)%j)*f(1)+2*(A(j,4)%j-A(j,5)%j+A(j,25)%j+A(j,26)%j-A(j,29)%j+A(j,30)%j+A(j,32)%j-A(j,33)%j+A(j,34)%j+A(j,36)%j &
       -A(j,37)%j+A(j,38)%j+A(j,40)%j-A(j,42)%j+A(j,58)%j-A(j,59)%j+A(j,60)%j+A(j,61)%j-A(j,62)%j-A(j,63)%j+A(j,69)%j-A(j,70)%j &
       +A(j,71)%j-A(j,72)%j)*f(2)
  M1(3) = 2*(A(j,2)%j-A(j,3)%j-A(j,7)%j+A(j,9)%j-A(j,10)%j+A(j,12)%j+A(j,14)%j-A(j,15)%j+A(j,17)%j-A(j,18)%j+A(j,20)%j+A(j,21)%j &
       +A(j,22)%j+A(j,23)%j+A(j,46)%j-A(j,47)%j+A(j,48)%j-A(j,49)%j-A(j,50)%j-A(j,51)%j-A(j,52)%j+A(j,53)%j-A(j,55)%j &
       +A(j,56)%j)*f(1)+2*(A(j,5)%j-A(j,6)%j-A(j,26)%j-A(j,27)%j+A(j,28)%j-A(j,30)%j-A(j,31)%j-A(j,32)%j+A(j,33)%j-A(j,35)%j &
       -A(j,38)%j+A(j,39)%j-A(j,41)%j+A(j,42)%j-A(j,61)%j+A(j,62)%j+A(j,63)%j-A(j,64)%j+A(j,65)%j+A(j,66)%j-A(j,67)%j+A(j,68)%j &
       -A(j,69)%j+A(j,70)%j)*f(2)
  M1(4) = 2*(-A(j,1)%j+A(j,3)%j+A(j,8)%j-A(j,9)%j+A(j,10)%j-A(j,11)%j+A(j,13)%j-A(j,14)%j-A(j,16)%j+A(j,18)%j-A(j,19)%j-A(j,20)%j &
       -A(j,23)%j-A(j,24)%j-A(j,43)%j-A(j,44)%j+A(j,45)%j-A(j,46)%j+A(j,47)%j+A(j,51)%j+A(j,52)%j+A(j,54)%j-A(j,56)%j &
       -A(j,57)%j)*f(1)+2*(A(j,4)%j-A(j,5)%j+A(j,25)%j+A(j,26)%j-A(j,29)%j+A(j,30)%j+A(j,32)%j-A(j,33)%j+A(j,34)%j+A(j,36)%j &
       -A(j,37)%j+A(j,38)%j+A(j,40)%j-A(j,42)%j+A(j,58)%j-A(j,59)%j+A(j,60)%j+A(j,61)%j-A(j,62)%j-A(j,63)%j+A(j,69)%j-A(j,70)%j &
       +A(j,71)%j-A(j,72)%j)*f(2)
  M1(5) = 2*(A(j,2)%j-A(j,3)%j-A(j,7)%j+A(j,9)%j-A(j,10)%j+A(j,12)%j+A(j,14)%j-A(j,15)%j+A(j,17)%j-A(j,18)%j+A(j,20)%j+A(j,21)%j &
       +A(j,22)%j+A(j,23)%j+A(j,46)%j-A(j,47)%j+A(j,48)%j-A(j,49)%j-A(j,50)%j-A(j,51)%j-A(j,52)%j+A(j,53)%j-A(j,55)%j &
       +A(j,56)%j)*f(1)+2*(A(j,5)%j-A(j,6)%j-A(j,26)%j-A(j,27)%j+A(j,28)%j-A(j,30)%j-A(j,31)%j-A(j,32)%j+A(j,33)%j-A(j,35)%j &
       -A(j,38)%j+A(j,39)%j-A(j,41)%j+A(j,42)%j-A(j,61)%j+A(j,62)%j+A(j,63)%j-A(j,64)%j+A(j,65)%j+A(j,66)%j-A(j,67)%j+A(j,68)%j &
       -A(j,69)%j+A(j,70)%j)*f(2)
  M1(6) = 2*(A(j,1)%j-A(j,2)%j+A(j,7)%j-A(j,8)%j+A(j,11)%j-A(j,12)%j-A(j,13)%j+A(j,15)%j+A(j,16)%j-A(j,17)%j+A(j,19)%j-A(j,21)%j &
       -A(j,22)%j+A(j,24)%j+A(j,43)%j+A(j,44)%j-A(j,45)%j-A(j,48)%j+A(j,49)%j+A(j,50)%j-A(j,53)%j-A(j,54)%j+A(j,55)%j &
       +A(j,57)%j)*f(1)+2*(-A(j,4)%j+A(j,6)%j-A(j,25)%j+A(j,27)%j-A(j,28)%j+A(j,29)%j+A(j,31)%j-A(j,34)%j+A(j,35)%j-A(j,36)%j &
       +A(j,37)%j-A(j,39)%j-A(j,40)%j+A(j,41)%j-A(j,58)%j+A(j,59)%j-A(j,60)%j+A(j,64)%j-A(j,65)%j-A(j,66)%j+A(j,67)%j-A(j,68)%j &
       -A(j,71)%j+A(j,72)%j)*f(2)

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
  use ol_colourmatrix_heftpphhj_hhgggg_1_/**/REALKIND, only: K1
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
! M1(i)    = <M1|Ci> colour component of matrix element
! M2(i)    = <M2|Ci> colour component of matrix element
! M2colint = <M1|M2>
!          = Sum_{i,j} <M1|Ci> * <Ci|Cj> * <Cj|M2>
!          = colour-summed squared matrix element
! K1(i,j) = <Ci|Cj>
! **********************************************************************
  use ol_colourmatrix_heftpphhj_hhgggg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(6)
  complex(REALKIND), intent(in)  :: M2(6)
  complex(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 6
    do j = 1, 6
      M2colint = M2colint + M1(i)*K1(i,j)*conjg(M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_heftpphhj_hhgggg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(6,16)
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
    & bind(c,name="ol_f_amp2tree_heftpphhj_hhgggg_1")
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
    & bind(c,name="ol_f_amp2ccone_heftpphhj_hhgggg_1")
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
    & bind(c,name="ol_f_amp2ccall_heftpphhj_hhgggg_1")
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
    & bind(c,name="ol_f_amp2hcone_heftpphhj_hhgggg_1")
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
    & bind(c,name="ol_f_amp2hcall_heftpphhj_hhgggg_1")
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
    & bind(c,name="ol_amp2tree_heftpphhj_hhgggg_1")
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
    & bind(c,name="ol_amp2ccone_heftpphhj_hhgggg_1")
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
    & bind(c,name="ol_amp2ccall_heftpphhj_hhgggg_1")
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
    & bind(c,name="ol_amp2hcone_heftpphhj_hhgggg_1")
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
    & bind(c,name="ol_amp2hcall_heftpphhj_hhgggg_1")
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
    & bind(c,name="amp2tree_heftpphhj_hhgggg_1_")
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
    & bind(c,name="amp2ccone_heftpphhj_hhgggg_1_")
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
    & bind(c,name="amp2ccall_heftpphhj_hhgggg_1_")
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
    & bind(c,name="amp2hcone_heftpphhj_hhgggg_1_")
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
    & bind(c,name="amp2hcall_heftpphhj_hhgggg_1_")
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

end module ol_tree_heftpphhj_hhgggg_1_/**/REALKIND
