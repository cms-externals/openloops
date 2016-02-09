
module ol_colourmatrix_pphlljj_eexdddxdxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(152,4)
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
  K1(  5,:) = [   0,   0,   0,   0]
  K1(  6,:) = [   0,   0,   0,   0]
  K1(  7,:) = [   0,   0,   0,   0]
  K1(  8,:) = [   0,   0,   0,   0]
  K1(  9,:) = [   0,   0,   0,   0]
  K1( 10,:) = [   0,   0,   0,   0]
  K1( 11,:) = [   0,   0,   0,   0]
  K1( 12,:) = [   0,   0,   0,   0]
  K1( 13,:) = [   0,   0,   0,   0]
  K1( 14,:) = [   0,   0,   0,   0]
  K1( 15,:) = [   0,   0,   0,   0]
  K1( 16,:) = [   0,   0,   0,   0]
  K1( 17,:) = [   0,   0,   0,   0]
  K1( 18,:) = [   0,   0,   0,   0]
  K1( 19,:) = [   0,   0,   0,   0]
  K1( 20,:) = [   0,   0,   0,   0]
  K1( 21,:) = [   0,   0,   0,   0]
  K1( 22,:) = [   0,   0,   0,   0]
  K1( 23,:) = [   0,   0,   0,   0]
  K1( 24,:) = [   0,   0,   0,   0]
  K1( 25,:) = [  48,  16,  16,   0]
  K1( 26,:) = [  16,  48,   0,  16]
  K1( 27,:) = [  16,   0,  48,  16]
  K1( 28,:) = [   0,  16,  16,  48]
  K1( 29,:) = [   0,   0,   0,   0]
  K1( 30,:) = [   0,   0,   0,   0]
  K1( 31,:) = [   0,   0,   0,   0]
  K1( 32,:) = [   0,   0,   0,   0]
  K1( 33,:) = [   0,   0,   0,   0]
  K1( 34,:) = [   0,   0,   0,   0]
  K1( 35,:) = [   0,   0,   0,   0]
  K1( 36,:) = [   0,   0,   0,   0]
  K1( 37,:) = [   0,  16,  -2,   6]
  K1( 38,:) = [  16,   0,   6,  -2]
  K1( 39,:) = [  -2,   6,   0,  16]
  K1( 40,:) = [   6,  -2,  16,   0]
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
  K1( 53,:) = [   6,   2,   2,   0]
  K1( 54,:) = [   2,   0,  -6, -16]
  K1( 55,:) = [   2,  -6,   0, -16]
  K1( 56,:) = [   0, -16, -16, -48]
  K1( 57,:) = [   0,   2, -16,  -6]
  K1( 58,:) = [   2,   6,   0,   2]
  K1( 59,:) = [ -16,   0, -48, -16]
  K1( 60,:) = [  -6,   2, -16,   0]
  K1( 61,:) = [  48,  16,  16,   0]
  K1( 62,:) = [  16,  48,   0,  16]
  K1( 63,:) = [  16,   0,  48,  16]
  K1( 64,:) = [   0,  16,  16,  48]
  K1( 65,:) = [   0,   0,   0,   0]
  K1( 66,:) = [   0,   0,   0,   0]
  K1( 67,:) = [   0,   0,   0,   0]
  K1( 68,:) = [   0,   0,   0,   0]
  K1( 69,:) = [   0,   0,   0,   0]
  K1( 70,:) = [   0,   0,   0,   0]
  K1( 71,:) = [   0,   0,   0,   0]
  K1( 72,:) = [   0,   0,   0,   0]
  K1( 73,:) = [   0, -16,   2,  -6]
  K1( 74,:) = [ -16, -48,   0, -16]
  K1( 75,:) = [   2,   0,   6,   2]
  K1( 76,:) = [  -6, -16,   2,   0]
  K1( 77,:) = [ -48, -16, -16,   0]
  K1( 78,:) = [ -16,   0,  -6,   2]
  K1( 79,:) = [ -16,  -6,   0,   2]
  K1( 80,:) = [   0,   2,   2,   6]
  K1( 81,:) = [   0,  -2,  16,   6]
  K1( 82,:) = [  -2,   0,   6,  16]
  K1( 83,:) = [  16,   6,   0,  -2]
  K1( 84,:) = [   6,  16,  -2,   0]
  K1( 85,:) = [  48,  16,  16,   0]
  K1( 86,:) = [  16,  48,   0,  16]
  K1( 87,:) = [  16,   0,  48,  16]
  K1( 88,:) = [   0,  16,  16,  48]
  K1( 89,:) = [   0,   0,   0,   0]
  K1( 90,:) = [   0,   0,   0,   0]
  K1( 91,:) = [   0,   0,   0,   0]
  K1( 92,:) = [   0,   0,   0,   0]
  K1( 93,:) = [   0,   0,   0,   0]
  K1( 94,:) = [   0,   0,   0,   0]
  K1( 95,:) = [   0,   0,   0,   0]
  K1( 96,:) = [   0,   0,   0,   0]
  K1( 97,:) = [   0,   0,   0,   0]
  K1( 98,:) = [   0,   0,   0,   0]
  K1( 99,:) = [   0,   0,   0,   0]
  K1(100,:) = [   0,   0,   0,   0]
  K1(101,:) = [   0,   0,   0,   0]
  K1(102,:) = [   0,   0,   0,   0]
  K1(103,:) = [   0,   0,   0,   0]
  K1(104,:) = [   0,   0,   0,   0]
  K1(105,:) = [   0,   0,   0,   0]
  K1(106,:) = [   0,   0,   0,   0]
  K1(107,:) = [   0,   0,   0,   0]
  K1(108,:) = [   0,   0,   0,   0]
  K1(109,:) = [   0,   0,   0,   0]
  K1(110,:) = [   0,   0,   0,   0]
  K1(111,:) = [   0,   0,   0,   0]
  K1(112,:) = [   0,   0,   0,   0]
  K1(113,:) = [   0,   0,   0,   0]
  K1(114,:) = [   0,   0,   0,   0]
  K1(115,:) = [   0,   0,   0,   0]
  K1(116,:) = [   0,   0,   0,   0]
  K1(117,:) = [   0,   0,   0,   0]
  K1(118,:) = [   0,   0,   0,   0]
  K1(119,:) = [   0,   0,   0,   0]
  K1(120,:) = [   0,   0,   0,   0]
  K1(121,:) = [   0,   0,   0,   0]
  K1(122,:) = [   0,   0,   0,   0]
  K1(123,:) = [   0,   0,   0,   0]
  K1(124,:) = [   0,   0,   0,   0]
  K1(125,:) = [ -54, -18, -18,   0]
  K1(126,:) = [ -18,   0,   0,  18]
  K1(127,:) = [ -18,   0, -54, -18]
  K1(128,:) = [   0,  18, -18,   0]
  K1(129,:) = [   0, -18,  18,   0]
  K1(130,:) = [ -18, -54,   0, -18]
  K1(131,:) = [  18,   0,   0, -18]
  K1(132,:) = [   0, -18, -18, -54]
  K1(133,:) = [ -54, -18, -18,   0]
  K1(134,:) = [ -18, -54,   0, -18]
  K1(135,:) = [ -18,   0,   0,  18]
  K1(136,:) = [   0, -18,  18,   0]
  K1(137,:) = [   0,  18, -18,   0]
  K1(138,:) = [  18,   0,   0, -18]
  K1(139,:) = [ -18,   0, -54, -18]
  K1(140,:) = [   0, -18, -18, -54]
  K1(141,:) = [   0,   0,   0,   0]
  K1(142,:) = [   0,   0,   0,   0]
  K1(143,:) = [   0,   0,   0,   0]
  K1(144,:) = [   0,   0,   0,   0]
  K1(145,:) = [ 108,  36,  36,   0]
  K1(146,:) = [  36, 108,   0,  36]
  K1(147,:) = [  36,   0, 108,  36]
  K1(148,:) = [   0,  36,  36, 108]
  K1(149,:) = [   0,   0,   0,   0]
  K1(150,:) = [   0,   0,   0,   0]
  K1(151,:) = [   0,   0,   0,   0]
  K1(152,:) = [   0,   0,   0,   0]
  K1 = (1._/**/REALKIND / 3) * K1

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphlljj_eexdddxdxhg_1_/**/REALKIND



module ol_forced_parameters_pphlljj_eexdddxdxhg_1_/**/REALKIND
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
  if (ME /= 0) write(*,101) 'ME = 0'
  if (YE /= 0) write(*,101) 'YE = 0'
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


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphlljj_eexdddxdxhg_1_/**/REALKIND

module ol_tree_pphlljj_eexdddxdxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(2)
  complex(REALKIND), save :: den(135)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 128 ! number of helicity configurations
  integer(intkind2), save :: nhel = 128 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(128) ! physical helicity states
  complex(DREALKIND) :: M1helarr(4,128) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f(2) = (eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,20))
  den(2) = 1 / (Q(5,3) - MZ2)
  den(3) = 1 / (Q(5,136))
  den(4) = 1 / (Q(5,52))
  den(6) = 1 / (Q(5,67) - MZ2)
  den(11) = 1 / (Q(5,156))
  den(14) = 1 / (Q(5,28))
  den(16) = 1 / (Q(5,160))
  den(20) = 1 / (Q(5,180))
  den(23) = 1 / (Q(5,99))
  den(26) = 1 / (Q(5,75))
  den(29) = 1 / (Q(5,148))
  den(34) = 1 / (Q(5,24))
  den(35) = 1 / (Q(5,132))
  den(36) = 1 / (Q(5,56))
  den(47) = 1 / (Q(5,184))
  den(51) = 1 / (Q(5,152))
  den(53) = 1 / (Q(5,71))
  den(59) = 1 / (Q(5,36))
  den(64) = 1 / (Q(5,172))
  den(67) = 1 / (Q(5,44))
  den(69) = 1 / (Q(5,144))
  den(75) = 1 / (Q(5,83))
  den(79) = 1 / (Q(5,164))
  den(84) = 1 / (Q(5,40))
  den(98) = 1 / (Q(5,168))

  ! denominators

  den(5) = den(1)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(3)*den(5)
  den(9) = den(7)*den(8)
  den(10) = den(1)*den(3)
  den(12) = den(10)*den(11)
  den(13) = den(7)*den(12)
  den(15) = den(1)*den(14)
  den(17) = den(15)*den(16)
  den(18) = den(7)*den(17)
  den(19) = den(1)*den(16)
  den(21) = den(19)*den(20)
  den(22) = den(7)*den(21)
  den(24) = den(7)*den(23)
  den(25) = den(15)*den(24)
  den(27) = den(7)*den(26)
  den(28) = den(5)*den(27)
  den(30) = den(1)*den(29)
  den(31) = den(27)*den(30)
  den(32) = den(11)*den(30)
  den(33) = den(7)*den(32)
  den(37) = den(34)*den(36)
  den(38) = den(35)*den(37)
  den(39) = den(7)*den(38)
  den(40) = den(34)*den(35)
  den(41) = den(11)*den(40)
  den(42) = den(7)*den(41)
  den(43) = den(14)*den(34)
  den(44) = den(16)*den(43)
  den(45) = den(7)*den(44)
  den(46) = den(16)*den(34)
  den(48) = den(46)*den(47)
  den(49) = den(7)*den(48)
  den(50) = den(24)*den(43)
  den(52) = den(34)*den(51)
  den(54) = den(7)*den(53)
  den(55) = den(52)*den(54)
  den(56) = den(11)*den(52)
  den(57) = den(7)*den(56)
  den(58) = den(37)*den(54)
  den(60) = den(4)*den(59)
  den(61) = den(3)*den(60)
  den(62) = den(7)*den(61)
  den(63) = den(3)*den(59)
  den(65) = den(63)*den(64)
  den(66) = den(7)*den(65)
  den(68) = den(59)*den(67)
  den(70) = den(68)*den(69)
  den(71) = den(7)*den(70)
  den(72) = den(59)*den(69)
  den(73) = den(20)*den(72)
  den(74) = den(7)*den(73)
  den(76) = den(7)*den(75)
  den(77) = den(68)*den(76)
  den(78) = den(27)*den(60)
  den(80) = den(59)*den(79)
  den(81) = den(27)*den(80)
  den(82) = den(64)*den(80)
  den(83) = den(7)*den(82)
  den(85) = den(36)*den(84)
  den(86) = den(35)*den(85)
  den(87) = den(7)*den(86)
  den(88) = den(35)*den(84)
  den(89) = den(64)*den(88)
  den(90) = den(7)*den(89)
  den(91) = den(67)*den(84)
  den(92) = den(69)*den(91)
  den(93) = den(7)*den(92)
  den(94) = den(69)*den(84)
  den(95) = den(47)*den(94)
  den(96) = den(7)*den(95)
  den(97) = den(76)*den(91)
  den(99) = den(84)*den(98)
  den(100) = den(54)*den(99)
  den(101) = den(64)*den(99)
  den(102) = den(7)*den(101)
  den(103) = den(54)*den(85)
  den(104) = den(29)*den(35)
  den(105) = den(11)*den(104)
  den(106) = den(7)*den(105)
  den(107) = den(27)*den(104)
  den(108) = den(35)*den(79)
  den(109) = den(64)*den(108)
  den(110) = den(7)*den(109)
  den(111) = den(27)*den(108)
  den(112) = den(3)*den(51)
  den(113) = den(11)*den(112)
  den(114) = den(7)*den(113)
  den(115) = den(3)*den(98)
  den(116) = den(64)*den(115)
  den(117) = den(7)*den(116)
  den(118) = den(54)*den(115)
  den(119) = den(54)*den(112)
  den(120) = den(29)*den(69)
  den(121) = den(11)*den(120)
  den(122) = den(7)*den(121)
  den(123) = den(51)*den(69)
  den(124) = den(11)*den(123)
  den(125) = den(7)*den(124)
  den(126) = den(27)*den(120)
  den(127) = den(54)*den(123)
  den(128) = den(16)*den(98)
  den(129) = den(64)*den(128)
  den(130) = den(7)*den(129)
  den(131) = den(16)*den(79)
  den(132) = den(64)*den(131)
  den(133) = den(7)*den(132)
  den(134) = den(27)*den(131)
  den(135) = den(54)*den(128)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_pphlljj_eexdddxdxhg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_pphlljj_eexdddxdxhg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for e- e+ down down anti-down anti-down higgs glue -> 0
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
  use ol_external_pphlljj_eexdddxdxhg_1, only: external_perm_pphlljj_eexdddxdxhg_1, &
    & external_perm_inv_pphlljj_eexdddxdxhg_1, extcomb_perm_pphlljj_eexdddxdxhg_1, &
    & average_factor_pphlljj_eexdddxdxhg_1
  use ol_external_pphlljj_eexdddxdxhg_1, only: H, hel_not_initialised, hel_init
  use ol_colourmatrix_pphlljj_eexdddxdxhg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_pphlljj_eexdddxdxhg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_pphlljj_eexdddxdxhg_1
  use ol_kinematics_/**/REALKIND, only: LC2Std_Rep_cmplx
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,8)
  real(REALKIND),  intent(out) :: M2(0:38-1)
  real(REALKIND),  intent(out) :: M2munu(4,4)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer,         intent(in)  :: nextcombs
  integer,         intent(in)  :: extcombs(nextcombs)

  integer           :: ReplacePol, JBmunu, extcombs_permuted(nextcombs), shift, k, r, l, m, n
  real(REALKIND)    :: P(0:3,8)
  real(REALKIND)    :: extmasses2(8)
  real(REALKIND)    :: M2add(0:38-1), M2munuadd
  complex(REALKIND) :: MOM_LC(4), M1(4), M1helarray(4,128)
  type(wfun) :: ep(2)
  complex(REALKIND) :: epStd(1:4,2)
  integer(intkind1) :: hmunu
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(2), ex6(2), ex7(1), ex8(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,16), wf8(8,36), wf16(16,60), wf32(32,8), wf128(128,48)

  type(polcont) :: A(128,48)
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
  extmasses2 = [ rZERO2, rZERO2, rZERO2, rZERO2, rZERO2, rZERO2, rMH2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_pphlljj_eexdddxdxhg_1,8)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,8)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_pphlljj_eexdddxdxhg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_pphlljj_eexdddxdxhg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_pphlljj_eexdddxdxhg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  ! external WFs
  call wf_Q(P(:,1), rZERO, H1, ex1)
  call wf_A(P(:,2), rZERO, H2, ex2)
  call wf_Q(P(:,3), rZERO, H3, ex3)
  call wf_Q(P(:,4), rZERO, H4, ex4)
  call wf_A(P(:,5), rZERO, H5, ex5)
  call wf_A(P(:,6), rZERO, H6, ex6)
  call wf_S(P(:,7), rMH, H7, ex7)
  call wf_V(P(:,8), rZERO, H8, ex8)


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
    call helbookkeeping_flip(H8, 8, shift, eflip, exthel, firstpol)
    call helbookkeeping_wf(H8, ex8, shift)

  end if

  ! internal WFs
  ! e.g. call vert_VQ_A(ntry, ex3, ex1, wf1, n1, t1) ...
  call vert_QA_Z(gZl,ntry, ex1, ex2, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_QA_V(ntry, ex3, ex5, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call vert_VQ_A(ntry, ex8, ex4, wf4(:,3), n3(:,3), t3x4(:,:,3))
  call prop_W_W(ntry, wf4(:,1), Q(:,3), MZ, 1_intkind1, wf4(:,4), n2(1))
  call vert_AV_Q(ntry, ex6, wf4(:,2), wf8(:,1), n3(:,4), t3x8(:,:,1))
  call vert_SV_V(ntry, ex7, wf4(:,4), wf4(:,5), n3(:,5), t3x4(:,:,4))
  call prop_Q_A(ntry, wf4(:,3), Q(:,136), ZERO, 0_intkind1, wf4(:,6), n2(2))
  call prop_A_Q(ntry, wf8(:,1), Q(:,52), ZERO, 0_intkind1, wf8(:,2), n2(3))
  call prop_W_W(ntry, wf4(:,5), Q(:,67), MZ, 1_intkind1, wf4(:,7), n2(4))
  call vert_QA_Z(gZd,ntry, wf4(:,6), wf8(:,2), wf32(:,1), n3(:,6), t3x32(:,:,1))
  call vert_VQ_A(ntry, wf4(:,2), wf4(:,6), wf16(:,1), n3(:,7), t3x16(:,:,1))
  call prop_Q_A(ntry, wf16(:,1), Q(:,156), ZERO, 0_intkind1, wf16(:,2), n2(5))
  call vert_AZ_Q(gZd,ntry, ex6, wf4(:,7), wf8(:,3), n3(:,8), t3x8(:,:,2))
  call vert_AV_Q(ntry, ex6, ex8, wf4(:,8), n3(:,9), t3x4(:,:,5))
  call vert_VQ_A(ntry, wf4(:,2), ex4, wf8(:,4), n3(:,10), t3x8(:,:,3))
  call prop_Q_A(ntry, wf8(:,4), Q(:,28), ZERO, 0_intkind1, wf8(:,5), n2(6))
  call prop_A_Q(ntry, wf4(:,8), Q(:,160), ZERO, 0_intkind1, wf4(:,9), n2(7))
  call vert_QA_Z(gZd,ntry, wf8(:,5), wf4(:,9), wf32(:,2), n3(:,11), t3x32(:,:,2))
  call vert_AV_Q(ntry, wf4(:,9), wf4(:,2), wf16(:,3), n3(:,12), t3x16(:,:,2))
  call prop_A_Q(ntry, wf16(:,3), Q(:,180), ZERO, 0_intkind1, wf16(:,4), n2(8))
  call vert_ZQ_A(gZd,ntry, wf4(:,7), ex4, wf8(:,6), n3(:,13), t3x8(:,:,4))
  call vert_VQ_A(ntry, ex8, wf8(:,5), wf16(:,5), n3(:,14), t3x16(:,:,3))
  call prop_A_Q(ntry, wf8(:,3), Q(:,99), ZERO, 0_intkind1, wf8(:,7), n2(9))
  call vert_AV_Q(ntry, wf8(:,2), ex8, wf16(:,6), n3(:,15), t3x16(:,:,4))
  call prop_Q_A(ntry, wf8(:,6), Q(:,75), ZERO, 0_intkind1, wf8(:,8), n2(10))
  call vert_UV_W(ntry, wf4(:,2), Q(:,20), ex8, Q(:,128), wf8(:,9), n3(:,16), t3x8(:,:,5))
  call vert_AV_Q(ntry, ex6, wf8(:,9), wf16(:,7), n3(:,17), t3x16(:,:,5))
  call vert_VQ_A(ntry, wf8(:,9), ex4, wf16(:,8), n3(:,18), t3x16(:,:,6))
  call prop_Q_A(ntry, wf16(:,8), Q(:,156), ZERO, 0_intkind1, wf16(:,9), n2(11))
  call vert_VQ_A(ntry, ex8, ex3, wf4(:,10), n3(:,19), t3x4(:,:,6))
  call vert_QA_V(ntry, ex4, ex5, wf4(:,11), n3(:,20), t3x4(:,:,7))
  call vert_AV_Q(ntry, ex6, wf4(:,11), wf8(:,10), n3(:,21), t3x8(:,:,6))
  call prop_Q_A(ntry, wf4(:,10), Q(:,132), ZERO, 0_intkind1, wf4(:,12), n2(12))
  call prop_A_Q(ntry, wf8(:,10), Q(:,56), ZERO, 0_intkind1, wf8(:,11), n2(13))
  call vert_QA_Z(gZd,ntry, wf4(:,12), wf8(:,11), wf32(:,3), n3(:,22), t3x32(:,:,3))
  call vert_VQ_A(ntry, wf4(:,11), wf4(:,12), wf16(:,10), n3(:,23), t3x16(:,:,7))
  call prop_Q_A(ntry, wf16(:,10), Q(:,156), ZERO, 0_intkind1, wf16(:,11), n2(14))
  call vert_VQ_A(ntry, wf4(:,11), ex3, wf8(:,12), n3(:,24), t3x8(:,:,7))
  call prop_Q_A(ntry, wf8(:,12), Q(:,28), ZERO, 0_intkind1, wf8(:,13), n2(15))
  call vert_QA_Z(gZd,ntry, wf8(:,13), wf4(:,9), wf32(:,4), n3(:,25), t3x32(:,:,4))
  call vert_AV_Q(ntry, wf4(:,9), wf4(:,11), wf16(:,12), n3(:,26), t3x16(:,:,8))
  call prop_A_Q(ntry, wf16(:,12), Q(:,184), ZERO, 0_intkind1, wf16(:,13), n2(16))
  call vert_ZQ_A(gZd,ntry, wf4(:,7), ex3, wf8(:,14), n3(:,27), t3x8(:,:,8))
  call vert_VQ_A(ntry, ex8, wf8(:,13), wf16(:,14), n3(:,28), t3x16(:,:,9))
  call vert_UV_W(ntry, wf4(:,11), Q(:,24), ex8, Q(:,128), wf8(:,15), n3(:,29), t3x8(:,:,9))
  call vert_AV_Q(ntry, ex6, wf8(:,15), wf16(:,15), n3(:,30), t3x16(:,:,10))
  call prop_Q_A(ntry, wf8(:,14), Q(:,71), ZERO, 0_intkind1, wf8(:,16), n2(17))
  call vert_VQ_A(ntry, wf8(:,15), ex3, wf16(:,16), n3(:,31), t3x16(:,:,11))
  call prop_Q_A(ntry, wf16(:,16), Q(:,156), ZERO, 0_intkind1, wf16(:,17), n2(18))
  call vert_AV_Q(ntry, wf8(:,11), ex8, wf16(:,18), n3(:,32), t3x16(:,:,12))
  call vert_QA_V(ntry, ex3, ex6, wf4(:,13), n3(:,33), t3x4(:,:,8))
  call vert_AV_Q(ntry, ex5, wf4(:,13), wf8(:,17), n3(:,34), t3x8(:,:,10))
  call prop_A_Q(ntry, wf8(:,17), Q(:,52), ZERO, 0_intkind1, wf8(:,18), n2(19))
  call vert_QA_Z(gZd,ntry, wf4(:,6), wf8(:,18), wf32(:,5), n3(:,35), t3x32(:,:,5))
  call vert_VQ_A(ntry, wf4(:,13), wf4(:,6), wf16(:,19), n3(:,36), t3x16(:,:,13))
  call prop_Q_A(ntry, wf16(:,19), Q(:,172), ZERO, 0_intkind1, wf16(:,20), n2(20))
  call vert_AZ_Q(gZd,ntry, ex5, wf4(:,7), wf8(:,19), n3(:,37), t3x8(:,:,11))
  call vert_AV_Q(ntry, ex5, ex8, wf4(:,14), n3(:,38), t3x4(:,:,9))
  call vert_VQ_A(ntry, wf4(:,13), ex4, wf8(:,20), n3(:,39), t3x8(:,:,12))
  call prop_Q_A(ntry, wf8(:,20), Q(:,44), ZERO, 0_intkind1, wf8(:,21), n2(21))
  call prop_A_Q(ntry, wf4(:,14), Q(:,144), ZERO, 0_intkind1, wf4(:,15), n2(22))
  call vert_QA_Z(gZd,ntry, wf8(:,21), wf4(:,15), wf32(:,6), n3(:,40), t3x32(:,:,6))
  call vert_AV_Q(ntry, wf4(:,15), wf4(:,13), wf16(:,21), n3(:,41), t3x16(:,:,14))
  call prop_A_Q(ntry, wf16(:,21), Q(:,180), ZERO, 0_intkind1, wf16(:,22), n2(23))
  call vert_VQ_A(ntry, ex8, wf8(:,21), wf16(:,23), n3(:,42), t3x16(:,:,15))
  call prop_A_Q(ntry, wf8(:,19), Q(:,83), ZERO, 0_intkind1, wf8(:,22), n2(24))
  call vert_AV_Q(ntry, wf8(:,18), ex8, wf16(:,24), n3(:,43), t3x16(:,:,16))
  call vert_UV_W(ntry, wf4(:,13), Q(:,36), ex8, Q(:,128), wf8(:,23), n3(:,44), t3x8(:,:,13))
  call vert_AV_Q(ntry, ex5, wf8(:,23), wf16(:,25), n3(:,45), t3x16(:,:,17))
  call vert_VQ_A(ntry, wf8(:,23), ex4, wf16(:,26), n3(:,46), t3x16(:,:,18))
  call prop_Q_A(ntry, wf16(:,26), Q(:,172), ZERO, 0_intkind1, wf16(:,27), n2(25))
  call vert_QA_V(ntry, ex4, ex6, wf4(:,16), n3(:,47), t3x4(:,:,10))
  call vert_AV_Q(ntry, ex5, wf4(:,16), wf8(:,24), n3(:,48), t3x8(:,:,14))
  call prop_A_Q(ntry, wf8(:,24), Q(:,56), ZERO, 0_intkind1, wf8(:,25), n2(26))
  call vert_QA_Z(gZd,ntry, wf4(:,12), wf8(:,25), wf32(:,7), n3(:,49), t3x32(:,:,7))
  call vert_VQ_A(ntry, wf4(:,16), wf4(:,12), wf16(:,28), n3(:,50), t3x16(:,:,19))
  call prop_Q_A(ntry, wf16(:,28), Q(:,172), ZERO, 0_intkind1, wf16(:,29), n2(27))
  call vert_VQ_A(ntry, wf4(:,16), ex3, wf8(:,26), n3(:,51), t3x8(:,:,15))
  call prop_Q_A(ntry, wf8(:,26), Q(:,44), ZERO, 0_intkind1, wf8(:,27), n2(28))
  call vert_QA_Z(gZd,ntry, wf8(:,27), wf4(:,15), wf32(:,8), n3(:,52), t3x32(:,:,8))
  call vert_AV_Q(ntry, wf4(:,15), wf4(:,16), wf16(:,30), n3(:,53), t3x16(:,:,20))
  call prop_A_Q(ntry, wf16(:,30), Q(:,184), ZERO, 0_intkind1, wf16(:,31), n2(29))
  call vert_VQ_A(ntry, ex8, wf8(:,27), wf16(:,32), n3(:,54), t3x16(:,:,21))
  call vert_UV_W(ntry, wf4(:,16), Q(:,40), ex8, Q(:,128), wf8(:,28), n3(:,55), t3x8(:,:,16))
  call vert_AV_Q(ntry, ex5, wf8(:,28), wf16(:,33), n3(:,56), t3x16(:,:,22))
  call vert_VQ_A(ntry, wf8(:,28), ex3, wf16(:,34), n3(:,57), t3x16(:,:,23))
  call prop_Q_A(ntry, wf16(:,34), Q(:,172), ZERO, 0_intkind1, wf16(:,35), n2(30))
  call vert_AV_Q(ntry, wf8(:,25), ex8, wf16(:,36), n3(:,58), t3x16(:,:,24))
  call vert_QA_V(ntry, wf4(:,12), ex5, wf8(:,29), n3(:,59), t3x8(:,:,17))
  call vert_VQ_A(ntry, wf8(:,29), ex4, wf16(:,37), n3(:,60), t3x16(:,:,25))
  call prop_Q_A(ntry, wf16(:,37), Q(:,156), ZERO, 0_intkind1, wf16(:,38), n2(31))
  call vert_AV_Q(ntry, ex6, wf8(:,29), wf16(:,39), n3(:,61), t3x16(:,:,26))
  call vert_QA_V(ntry, wf4(:,12), ex6, wf8(:,30), n3(:,62), t3x8(:,:,18))
  call vert_VQ_A(ntry, wf8(:,30), ex4, wf16(:,40), n3(:,63), t3x16(:,:,27))
  call prop_Q_A(ntry, wf16(:,40), Q(:,172), ZERO, 0_intkind1, wf16(:,41), n2(32))
  call vert_AV_Q(ntry, ex5, wf8(:,30), wf16(:,42), n3(:,64), t3x16(:,:,28))
  call vert_QA_V(ntry, wf4(:,6), ex5, wf8(:,31), n3(:,65), t3x8(:,:,19))
  call vert_VQ_A(ntry, wf8(:,31), ex3, wf16(:,43), n3(:,66), t3x16(:,:,29))
  call prop_Q_A(ntry, wf16(:,43), Q(:,156), ZERO, 0_intkind1, wf16(:,44), n2(33))
  call vert_QA_V(ntry, wf4(:,6), ex6, wf8(:,32), n3(:,67), t3x8(:,:,20))
  call vert_VQ_A(ntry, wf8(:,32), ex3, wf16(:,45), n3(:,68), t3x16(:,:,30))
  call prop_Q_A(ntry, wf16(:,45), Q(:,172), ZERO, 0_intkind1, wf16(:,46), n2(34))
  call vert_AV_Q(ntry, ex5, wf8(:,32), wf16(:,47), n3(:,69), t3x16(:,:,31))
  call vert_AV_Q(ntry, ex6, wf8(:,31), wf16(:,48), n3(:,70), t3x16(:,:,32))
  call vert_QA_V(ntry, ex3, wf4(:,15), wf8(:,33), n3(:,71), t3x8(:,:,21))
  call vert_VQ_A(ntry, wf8(:,33), ex4, wf16(:,49), n3(:,72), t3x16(:,:,33))
  call prop_Q_A(ntry, wf16(:,49), Q(:,156), ZERO, 0_intkind1, wf16(:,50), n2(35))
  call vert_QA_V(ntry, ex4, wf4(:,15), wf8(:,34), n3(:,73), t3x8(:,:,22))
  call vert_VQ_A(ntry, wf8(:,34), ex3, wf16(:,51), n3(:,74), t3x16(:,:,34))
  call prop_Q_A(ntry, wf16(:,51), Q(:,156), ZERO, 0_intkind1, wf16(:,52), n2(36))
  call vert_AV_Q(ntry, ex6, wf8(:,33), wf16(:,53), n3(:,75), t3x16(:,:,35))
  call vert_AV_Q(ntry, ex6, wf8(:,34), wf16(:,54), n3(:,76), t3x16(:,:,36))
  call vert_QA_V(ntry, ex4, wf4(:,9), wf8(:,35), n3(:,77), t3x8(:,:,23))
  call vert_VQ_A(ntry, wf8(:,35), ex3, wf16(:,55), n3(:,78), t3x16(:,:,37))
  call prop_Q_A(ntry, wf16(:,55), Q(:,172), ZERO, 0_intkind1, wf16(:,56), n2(37))
  call vert_QA_V(ntry, ex3, wf4(:,9), wf8(:,36), n3(:,79), t3x8(:,:,24))
  call vert_VQ_A(ntry, wf8(:,36), ex4, wf16(:,57), n3(:,80), t3x16(:,:,38))
  call prop_Q_A(ntry, wf16(:,57), Q(:,172), ZERO, 0_intkind1, wf16(:,58), n2(38))
  call vert_AV_Q(ntry, ex5, wf8(:,36), wf16(:,59), n3(:,81), t3x16(:,:,39))
  call vert_AV_Q(ntry, ex5, wf8(:,35), wf16(:,60), n3(:,82), t3x16(:,:,40))


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
  M2add = M2 / average_factor_pphlljj_eexdddxdxhg_1

  do k = 0, 38-1
    M2(k) = M2add(extcomb_perm_pphlljj_eexdddxdxhg_1(k))
  end do

  if (ntry == 1) ntry = 2

  if (scalefactor /= old_scalefactor) then
    scalebackfactor = scalefactor**(2*8-8)
    old_scalefactor = scalefactor
  end if
  M2 = scalebackfactor * M2

  contains

subroutine diagrams()
  implicit none
  ! e.g. call cont_VV(nsync, wf3, wf6, A(:,1), n64, t64, nhel, den(5)) ...

    call cont_VV(nsync, wf4(:,7), wf32(:,1), A(:,1), n3(:,83), t3x128(:,:,1), nhel, den(9))
    call cont_QA(nsync, wf16(:,2), wf8(:,3), A(:,2), n3(:,84), t3x128(:,:,2), nhel, den(13))
    call cont_VV(nsync, wf4(:,7), wf32(:,2), A(:,3), n3(:,85), t3x128(:,:,3), nhel, den(18))
    call cont_QA(nsync, wf16(:,4), wf8(:,6), A(:,4), n3(:,86), t3x128(:,:,4), nhel, den(22))
    call cont_QA(nsync, wf16(:,5), wf8(:,7), A(:,5), n3(:,87), t3x128(:,:,5), nhel, den(25))
    call cont_QA(nsync, wf16(:,6), wf8(:,8), A(:,6), n3(:,88), t3x128(:,:,6), nhel, den(28))
    call cont_QA(nsync, wf8(:,8), wf16(:,7), A(:,7), n3(:,89), t3x128(:,:,7), nhel, den(31))
    call cont_QA(nsync, wf8(:,3), wf16(:,9), A(:,8), n3(:,90), t3x128(:,:,8), nhel, den(33))
    call cont_VV(nsync, wf4(:,7), wf32(:,3), A(:,9), n3(:,91), t3x128(:,:,9), nhel, den(39))
    call cont_QA(nsync, wf8(:,3), wf16(:,11), A(:,10), n3(:,92), t3x128(:,:,10), nhel, den(42))
    call cont_VV(nsync, wf4(:,7), wf32(:,4), A(:,11), n3(:,93), t3x128(:,:,11), nhel, den(45))
    call cont_QA(nsync, wf16(:,13), wf8(:,14), A(:,12), n3(:,94), t3x128(:,:,12), nhel, den(49))
    call cont_QA(nsync, wf8(:,7), wf16(:,14), A(:,13), n3(:,95), t3x128(:,:,13), nhel, den(50))
    call cont_QA(nsync, wf16(:,15), wf8(:,16), A(:,14), n3(:,96), t3x128(:,:,14), nhel, den(55))
    call cont_QA(nsync, wf8(:,3), wf16(:,17), A(:,15), n3(:,97), t3x128(:,:,15), nhel, den(57))
    call cont_QA(nsync, wf8(:,16), wf16(:,18), A(:,16), n3(:,98), t3x128(:,:,16), nhel, den(58))
    call cont_VV(nsync, wf4(:,7), wf32(:,5), A(:,17), n3(:,99), t3x128(:,:,17), nhel, den(62))
    call cont_QA(nsync, wf16(:,20), wf8(:,19), A(:,18), n3(:,100), t3x128(:,:,18), nhel, den(66))
    call cont_VV(nsync, wf4(:,7), wf32(:,6), A(:,19), n3(:,101), t3x128(:,:,19), nhel, den(71))
    call cont_QA(nsync, wf8(:,6), wf16(:,22), A(:,20), n3(:,102), t3x128(:,:,20), nhel, den(74))
    call cont_QA(nsync, wf16(:,23), wf8(:,22), A(:,21), n3(:,103), t3x128(:,:,21), nhel, den(77))
    call cont_QA(nsync, wf8(:,8), wf16(:,24), A(:,22), n3(:,104), t3x128(:,:,22), nhel, den(78))
    call cont_QA(nsync, wf8(:,8), wf16(:,25), A(:,23), n3(:,105), t3x128(:,:,23), nhel, den(81))
    call cont_QA(nsync, wf8(:,19), wf16(:,27), A(:,24), n3(:,106), t3x128(:,:,24), nhel, den(83))
    call cont_VV(nsync, wf4(:,7), wf32(:,7), A(:,25), n3(:,107), t3x128(:,:,25), nhel, den(87))
    call cont_QA(nsync, wf8(:,19), wf16(:,29), A(:,26), n3(:,108), t3x128(:,:,26), nhel, den(90))
    call cont_VV(nsync, wf4(:,7), wf32(:,8), A(:,27), n3(:,109), t3x128(:,:,27), nhel, den(93))
    call cont_QA(nsync, wf8(:,14), wf16(:,31), A(:,28), n3(:,110), t3x128(:,:,28), nhel, den(96))
    call cont_QA(nsync, wf8(:,22), wf16(:,32), A(:,29), n3(:,111), t3x128(:,:,29), nhel, den(97))
    call cont_QA(nsync, wf8(:,16), wf16(:,33), A(:,30), n3(:,112), t3x128(:,:,30), nhel, den(100))
    call cont_QA(nsync, wf8(:,19), wf16(:,35), A(:,31), n3(:,113), t3x128(:,:,31), nhel, den(102))
    call cont_QA(nsync, wf8(:,16), wf16(:,36), A(:,32), n3(:,114), t3x128(:,:,32), nhel, den(103))
    call cont_QA(nsync, wf8(:,3), wf16(:,38), A(:,33), n3(:,115), t3x128(:,:,33), nhel, den(106))
    call cont_QA(nsync, wf8(:,8), wf16(:,39), A(:,34), n3(:,116), t3x128(:,:,34), nhel, den(107))
    call cont_QA(nsync, wf8(:,19), wf16(:,41), A(:,35), n3(:,117), t3x128(:,:,35), nhel, den(110))
    call cont_QA(nsync, wf8(:,8), wf16(:,42), A(:,36), n3(:,118), t3x128(:,:,36), nhel, den(111))
    call cont_QA(nsync, wf8(:,3), wf16(:,44), A(:,37), n3(:,119), t3x128(:,:,37), nhel, den(114))
    call cont_QA(nsync, wf8(:,19), wf16(:,46), A(:,38), n3(:,120), t3x128(:,:,38), nhel, den(117))
    call cont_QA(nsync, wf8(:,16), wf16(:,47), A(:,39), n3(:,121), t3x128(:,:,39), nhel, den(118))
    call cont_QA(nsync, wf8(:,16), wf16(:,48), A(:,40), n3(:,122), t3x128(:,:,40), nhel, den(119))
    call cont_QA(nsync, wf8(:,3), wf16(:,50), A(:,41), n3(:,123), t3x128(:,:,41), nhel, den(122))
    call cont_QA(nsync, wf8(:,3), wf16(:,52), A(:,42), n3(:,124), t3x128(:,:,42), nhel, den(125))
    call cont_QA(nsync, wf8(:,8), wf16(:,53), A(:,43), n3(:,125), t3x128(:,:,43), nhel, den(126))
    call cont_QA(nsync, wf8(:,16), wf16(:,54), A(:,44), n3(:,126), t3x128(:,:,44), nhel, den(127))
    call cont_QA(nsync, wf8(:,19), wf16(:,56), A(:,45), n3(:,127), t3x128(:,:,45), nhel, den(130))
    call cont_QA(nsync, wf8(:,19), wf16(:,58), A(:,46), n3(:,128), t3x128(:,:,46), nhel, den(133))
    call cont_QA(nsync, wf8(:,8), wf16(:,59), A(:,47), n3(:,129), t3x128(:,:,47), nhel, den(134))
    call cont_QA(nsync, wf8(:,16), wf16(:,60), A(:,48), n3(:,130), t3x128(:,:,48), nhel, den(135))

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
  complex(REALKIND), intent(out) :: M1(4) ! M1helarray(4,128)
  integer :: empty(0)

  M1(1) = ((A(j,25)%j+A(j,26)%j+A(j,27)%j+A(j,28)%j+A(j,29)%j+A(j,32)%j+A(j,33)%j+A(j,34)%j+A(j,41)%j &
       +A(j,43)%j)*f(1))/6._/**/REALKIND+((A(j,9)%j+A(j,10)%j+A(j,16)%j+A(j,19)%j+A(j,20)%j+A(j,21)%j+A(j,35)%j+A(j,36)%j &
       +A(j,42)%j+A(j,44)%j)*f(1))/2._/**/REALKIND+(CI*(A(j,14)%j+A(j,15)%j-A(j,23)%j-A(j,24)%j)*f(2))/2._/**/REALKIND
  M1(2) = ((-A(j,1)%j-A(j,2)%j-A(j,6)%j-A(j,27)%j-A(j,28)%j-A(j,29)%j-A(j,38)%j-A(j,39)%j-A(j,41)%j &
       -A(j,43)%j)*f(1))/2._/**/REALKIND+((-A(j,17)%j-A(j,18)%j-A(j,19)%j-A(j,20)%j-A(j,21)%j-A(j,22)%j-A(j,37)%j-A(j,40)%j &
       -A(j,42)%j-A(j,44)%j)*f(1))/6._/**/REALKIND+(CI*(-A(j,7)%j-A(j,8)%j+A(j,30)%j+A(j,31)%j)*f(2))/2._/**/REALKIND
  M1(3) = ((-A(j,9)%j-A(j,10)%j-A(j,11)%j-A(j,12)%j-A(j,13)%j-A(j,16)%j-A(j,35)%j-A(j,36)%j-A(j,46)%j &
       -A(j,47)%j)*f(1))/6._/**/REALKIND+((-A(j,3)%j-A(j,4)%j-A(j,5)%j-A(j,25)%j-A(j,26)%j-A(j,32)%j-A(j,33)%j-A(j,34)%j-A(j,45)%j &
       -A(j,48)%j)*f(1))/2._/**/REALKIND+(CI*(A(j,7)%j+A(j,8)%j-A(j,30)%j-A(j,31)%j)*f(2))/2._/**/REALKIND
  M1(4) = ((A(j,11)%j+A(j,12)%j+A(j,13)%j+A(j,17)%j+A(j,18)%j+A(j,22)%j+A(j,37)%j+A(j,40)%j+A(j,46)%j &
       +A(j,47)%j)*f(1))/2._/**/REALKIND+((A(j,1)%j+A(j,2)%j+A(j,3)%j+A(j,4)%j+A(j,5)%j+A(j,6)%j+A(j,38)%j+A(j,39)%j+A(j,45)%j &
       +A(j,48)%j)*f(1))/6._/**/REALKIND+(CI*(-A(j,14)%j-A(j,15)%j+A(j,23)%j+A(j,24)%j)*f(2))/2._/**/REALKIND

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
  use ol_colourmatrix_pphlljj_eexdddxdxhg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M(4)
  real(REALKIND),    intent(out) :: M2colint(0:38-1)
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
subroutine colintmunu(M1, M2, M2colint)
! M(i)   = <M|Ci> colour component of matrix element
! COLINT = <M|M>
!        = Sum_{i,j} <M|Ci> * <Ci|Cj> * <Cj|M>
!        = colour-summed squared matrix element
! K1(i,j) = <Ci|Cj>
! M2colint is an array which contains the colour interference for each colour matrix
! **********************************************************************
  use ol_colourmatrix_pphlljj_eexdddxdxhg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(4)
  complex(REALKIND), intent(in)  :: M2(4)
  real(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 4
    do j = 1, 4
      M2colint = M2colint + real(conjg(M1(i))*K1(i,j)*M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_pphlljj_eexdddxdxhg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(4,128)
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
    & bind(c,name="ol_f_amp2tree_pphlljj_eexdddxdxhg_1")
#else
subroutine amp2tree(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix element without fuss.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,8)
  real(REALKIND),  intent(out) :: M2
  real(REALKIND) :: M2tmp(0:38-1)
  real(REALKIND) :: M2munu(4,4)
  call amp2(P, M2tmp, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], 1, [ 0 ], M2munu)
  M2 = M2tmp(0)
end subroutine amp2tree


#ifdef PRECISION_dp
subroutine amp2ccone(P, M2, I, J) &
    & bind(c,name="ol_f_amp2ccone_pphlljj_eexdddxdxhg_1")
#else
subroutine amp2ccone(P, M2, I, J)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for the colour correlation matrix for particles I and J.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,8)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:38-1)
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
    & bind(c,name="ol_f_amp2ccall_pphlljj_eexdddxdxhg_1")
#else
subroutine amp2ccall(P, M2)
#endif
  ! This is an interface for AMP2.
  ! Calculates the squared matrix for all colour correlation matrices.
  ! The correlation between particles i and j is at position i*(i-1)/2+j of the array M2.
  ! M2(0) is AMP2tree
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,8)
  real(REALKIND),  intent(out) :: M2(0:38-1)
  real(REALKIND) :: M2munu(4,4)
  integer :: k
  call amp2(P, M2, 0, [ 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND, 0._/**/REALKIND ], &
    38, [ (k, k = 0, 38-1) ], M2munu)
end subroutine amp2ccall


#ifdef PRECISION_dp
subroutine amp2hcone(P, M2, I, J, MOM) &
    & bind(c,name="ol_f_amp2hcone_pphlljj_eexdddxdxhg_1")
#else
subroutine amp2hcone(P, M2, I, J, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates the helicity correlation for emitter I with momentum MOM and spectator J
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,8)
  real(REALKIND),  intent(out) :: M2
  integer,         intent(in)  :: I, J
  real(REALKIND),  intent(in)  :: MOM(0:3)
  integer        :: extcomb
  real(REALKIND) :: M2tmp(0:38-1)
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
    & bind(c,name="ol_f_amp2hcall_pphlljj_eexdddxdxhg_1")
#else
subroutine amp2hcall(P, M2, I, MOM)
#endif
  ! This is an interface for AMP2.
  ! Calculates all helicity correlations for emitter I with momentum MOM.
  ! The correlator for spectator j is at position j of the array M2.
  implicit none
  real(DREALKIND), intent(in)  :: P(0:3,8)
  real(REALKIND),  intent(out) :: M2(8)
  integer,         intent(in)  :: I
  real(REALKIND),  intent(in)  :: MOM(0:3)
  real(REALKIND) :: M2tmp(0:38-1)
  real(REALKIND) :: M2munu(4,4)
  integer        :: J, extcombs(8)
  do J = 1, 8
    if (J <= I) then
      extcombs(J) = I*(I-1)/2 + J
    else
      extcombs(J) = J*(J-1)/2 + I
    end if
  end do
  call amp2(P, M2tmp, I, MOM, 8,extcombs, M2munu)
  do J = 1, 8
    M2(J) = M2tmp(extcombs(J))
  end do
end subroutine amp2hcall


#ifdef PRECISION_dp

subroutine amp2tree_c(p, m2) &
    & bind(c,name="ol_amp2tree_pphlljj_eexdddxdxhg_1")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_c

subroutine amp2ccone_c(p, m2, i, j) &
    & bind(c,name="ol_amp2ccone_pphlljj_eexdddxdxhg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_c

subroutine amp2ccall_c(p, m2) &
    & bind(c,name="ol_amp2ccall_pphlljj_eexdddxdxhg_1")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2(0:38-1)
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2(0:38-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_c

subroutine amp2hcone_c(p, m2, i, j, mom) &
    & bind(c,name="ol_amp2hcone_pphlljj_eexdddxdxhg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,8)
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
    & bind(c,name="ol_amp2hcall_pphlljj_eexdddxdxhg_1")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2(8)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2(8)
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
    & bind(c,name="amp2tree_pphlljj_eexdddxdxhg_1_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2
  f_p = p
  call amp2tree(f_p, f_m2)
  m2 = f_m2
end subroutine amp2tree_legacy

subroutine amp2ccone_legacy(p, m2, i, j) &
    & bind(c,name="amp2ccone_pphlljj_eexdddxdxhg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2
  integer :: f_i, f_j
  f_p = p
  f_i = i
  f_j = j
  call amp2ccone(f_p, f_m2, f_i, f_j)
  m2 = f_m2
end subroutine amp2ccone_legacy

subroutine amp2ccall_legacy(p, m2) &
    & bind(c,name="amp2ccall_pphlljj_eexdddxdxhg_1_")
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2(0:38-1)
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2(0:38-1)
  f_p = p
  call amp2ccall(f_p, f_m2)
  m2 = f_m2
end subroutine amp2ccall_legacy

subroutine amp2hcone_legacy(p, m2, i, j, mom) &
    & bind(c,name="amp2hcone_pphlljj_eexdddxdxhg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2
  integer(c_int), intent(in) :: i, j
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,8)
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
    & bind(c,name="amp2hcall_pphlljj_eexdddxdxhg_1_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in)  :: p(0:3,8)
  real(c_double), intent(out) :: m2(8)
  integer(c_int), intent(in) :: i
  real(c_double), intent(in) :: mom(0:3)
  real(DREALKIND) :: f_p(0:3,8)
  real(DREALKIND) :: f_m2(8)
  integer :: f_i
  real(DREALKIND) :: f_mom(0:3)
  f_p = p
  f_i = i
  f_mom = mom
  call amp2hcall(f_p, f_m2, f_i, f_mom)
  m2 = f_m2
end subroutine amp2hcall_legacy

#endif

end module ol_tree_pphlljj_eexdddxdxhg_1_/**/REALKIND
