
module ol_colourmatrix_ppwjj_ckm_usxwggg_1_/**/REALKIND
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

  K1(  1,:) = [  384,  -48,  -48,    6,    6,   60]
  K1(  2,:) = [  -48,  384,    6,   60,  -48,    6]
  K1(  3,:) = [  -48,    6,  384,  -48,   60,    6]
  K1(  4,:) = [    6,   60,  -48,  384,    6,  -48]
  K1(  5,:) = [    6,  -48,   60,    6,  384,  -48]
  K1(  6,:) = [   60,    6,    6,  -48,  -48,  384]
  K1(  7,:) = [  512,  -64,  -64,    8,    8,   80]
  K1(  8,:) = [  -64,  512,    8,   80,  -64,    8]
  K1(  9,:) = [  -64,    8,  512,  -64,   80,    8]
  K1( 10,:) = [    8,   80,  -64,  512,    8,  -64]
  K1( 11,:) = [    8,  -64,   80,    8,  512,  -64]
  K1( 12,:) = [   80,    8,    8,  -64,  -64,  512]
  K1( 13,:) = [    1,   10,   10,  -62,  -62,   28]
  K1( 14,:) = [   10,    1,  -62,   28,   10,  -62]
  K1( 15,:) = [   10,  -62,    1,   10,   28,  -62]
  K1( 16,:) = [  -62,   28,   10,    1,  -62,   10]
  K1( 17,:) = [  -62,   10,   28,  -62,    1,   10]
  K1( 18,:) = [   28,  -62,  -62,   10,   10,    1]
  K1( 19,:) = [  512,  -64,  -64,    8,    8,   80]
  K1( 20,:) = [  -64,  512,    8,   80,  -64,    8]
  K1( 21,:) = [  -64,    8,  512,  -64,   80,    8]
  K1( 22,:) = [    8,   80,  -64,  512,    8,  -64]
  K1( 23,:) = [    8,  -64,   80,    8,  512,  -64]
  K1( 24,:) = [   80,    8,    8,  -64,  -64,  512]
  K1( 25,:) = [    0,    0,    0,    0,    0,    0]
  K1( 26,:) = [    0,    0,    0,    0,    0,    0]
  K1( 27,:) = [    0,    0,    0,    0,    0,    0]
  K1( 28,:) = [    0,    0,    0,    0,    0,    0]
  K1( 29,:) = [    0,    0,    0,    0,    0,    0]
  K1( 30,:) = [    0,    0,    0,    0,    0,    0]
  K1( 31,:) = [    0,    0,    0,    0,    0,    0]
  K1( 32,:) = [    0,    0,    0,    0,    0,    0]
  K1( 33,:) = [    0,    0,    0,    0,    0,    0]
  K1( 34,:) = [    0,    0,    0,    0,    0,    0]
  K1( 35,:) = [    0,    0,    0,    0,    0,    0]
  K1( 36,:) = [    0,    0,    0,    0,    0,    0]
  K1( 37,:) = [    0,    0,    0,    0,    0,    0]
  K1( 38,:) = [    0,    0,    0,    0,    0,    0]
  K1( 39,:) = [    0,    0,    0,    0,    0,    0]
  K1( 40,:) = [    0,    0,    0,    0,    0,    0]
  K1( 41,:) = [    0,    0,    0,    0,    0,    0]
  K1( 42,:) = [    0,    0,    0,    0,    0,    0]
  K1( 43,:) = [   -9,  -90,   -9,   -9,   72,  -90]
  K1( 44,:) = [  -90,   -9,   72,  -90,   -9,   -9]
  K1( 45,:) = [   -9,   72,   72,   72,   72,   -9]
  K1( 46,:) = [   -9,  -90,   72, -576,   -9,   72]
  K1( 47,:) = [   72,   -9,   72,   -9,   72,   72]
  K1( 48,:) = [  -90,   -9,   -9,   72,   72, -576]
  K1( 49,:) = [ -576,   72,   72,   -9,   -9,  -90]
  K1( 50,:) = [   72, -576,   -9,  -90,   72,   -9]
  K1( 51,:) = [   72,   -9,   72,   -9,   72,   72]
  K1( 52,:) = [   -9,  -90,   -9,   -9,   72,  -90]
  K1( 53,:) = [   -9,   72,   72,   72,   72,   -9]
  K1( 54,:) = [  -90,   -9,   72,  -90,   -9,   -9]
  K1( 55,:) = [    0,    0,    0,    0,    0,    0]
  K1( 56,:) = [    0,    0,    0,    0,    0,    0]
  K1( 57,:) = [    0,    0,    0,    0,    0,    0]
  K1( 58,:) = [    0,    0,    0,    0,    0,    0]
  K1( 59,:) = [    0,    0,    0,    0,    0,    0]
  K1( 60,:) = [    0,    0,    0,    0,    0,    0]
  K1( 61,:) = [ 1152, -144, -144,   18,   18,  180]
  K1( 62,:) = [ -144, 1152,   18,  180, -144,   18]
  K1( 63,:) = [ -144,   18, 1152, -144,  180,   18]
  K1( 64,:) = [   18,  180, -144, 1152,   18, -144]
  K1( 65,:) = [   18, -144,  180,   18, 1152, -144]
  K1( 66,:) = [  180,   18,   18, -144, -144, 1152]
  K1( 67,:) = [   72,   72,   -9,   72,   -9,   72]
  K1( 68,:) = [   72, -576,   -9,  -90,   72,   -9]
  K1( 69,:) = [   -9,   -9,   -9,  -90,  -90,   72]
  K1( 70,:) = [   72,  -90,  -90,   -9,   -9,   -9]
  K1( 71,:) = [   -9,   72,  -90,   -9, -576,   72]
  K1( 72,:) = [   72,   -9,   72,   -9,   72,   72]
  K1( 73,:) = [   72,   -9,   72,   -9,   72,   72]
  K1( 74,:) = [   -9,   -9,   -9,  -90,  -90,   72]
  K1( 75,:) = [   72,   -9, -576,   72,  -90,   -9]
  K1( 76,:) = [   -9,  -90,   72, -576,   -9,   72]
  K1( 77,:) = [   72,  -90,  -90,   -9,   -9,   -9]
  K1( 78,:) = [   72,   72,   -9,   72,   -9,   72]
  K1( 79,:) = [    0,    0,    0,    0,    0,    0]
  K1( 80,:) = [    0,    0,    0,    0,    0,    0]
  K1( 81,:) = [    0,    0,    0,    0,    0,    0]
  K1( 82,:) = [    0,    0,    0,    0,    0,    0]
  K1( 83,:) = [    0,    0,    0,    0,    0,    0]
  K1( 84,:) = [    0,    0,    0,    0,    0,    0]
  K1( 85,:) = [ -648,   81,    0,    0,  -81, -162]
  K1( 86,:) = [   81,   81,    0,  162,   81,    0]
  K1( 87,:) = [    0,    0, -648,   81, -162,  -81]
  K1( 88,:) = [    0,  162,   81,   81,    0,   81]
  K1( 89,:) = [  -81,   81, -162,    0, -648,    0]
  K1( 90,:) = [ -162,    0,  -81,   81,    0, -648]
  K1( 91,:) = [ 1152, -144, -144,   18,   18,  180]
  K1( 92,:) = [ -144, 1152,   18,  180, -144,   18]
  K1( 93,:) = [ -144,   18, 1152, -144,  180,   18]
  K1( 94,:) = [   18,  180, -144, 1152,   18, -144]
  K1( 95,:) = [   18, -144,  180,   18, 1152, -144]
  K1( 96,:) = [  180,   18,   18, -144, -144, 1152]
  K1( 97,:) = [ -576,   72,   72,   -9,   -9,  -90]
  K1( 98,:) = [   72,   72,   -9,   72,   -9,   72]
  K1( 99,:) = [   72,   -9, -576,   72,  -90,   -9]
  K1(100,:) = [   -9,   72,   72,   72,   72,   -9]
  K1(101,:) = [   -9,   -9,  -90,   72,   -9,  -90]
  K1(102,:) = [  -90,   72,   -9,   -9,  -90,   -9]
  K1(103,:) = [   -9,   -9,  -90,   72,   -9,  -90]
  K1(104,:) = [   -9,   72,   72,   72,   72,   -9]
  K1(105,:) = [  -90,   72,   -9,   -9,  -90,   -9]
  K1(106,:) = [   72,   72,   -9,   72,   -9,   72]
  K1(107,:) = [   -9,   72,  -90,   -9, -576,   72]
  K1(108,:) = [  -90,   -9,   -9,   72,   72, -576]
  K1(109,:) = [    0,    0,    0,    0,    0,    0]
  K1(110,:) = [    0,    0,    0,    0,    0,    0]
  K1(111,:) = [    0,    0,    0,    0,    0,    0]
  K1(112,:) = [    0,    0,    0,    0,    0,    0]
  K1(113,:) = [    0,    0,    0,    0,    0,    0]
  K1(114,:) = [    0,    0,    0,    0,    0,    0]
  K1(115,:) = [   81,   81,   81,    0,    0,  162]
  K1(116,:) = [   81, -648,  -81, -162,    0,    0]
  K1(117,:) = [   81,  -81, -648,    0, -162,    0]
  K1(118,:) = [    0, -162,    0, -648,  -81,   81]
  K1(119,:) = [    0,    0, -162,  -81, -648,   81]
  K1(120,:) = [  162,    0,    0,   81,   81,   81]
  K1(121,:) = [ -648,    0,   81,  -81,    0, -162]
  K1(122,:) = [    0, -648,    0, -162,   81,  -81]
  K1(123,:) = [   81,    0,   81,   81,  162,    0]
  K1(124,:) = [  -81, -162,   81, -648,    0,    0]
  K1(125,:) = [    0,   81,  162,    0,   81,   81]
  K1(126,:) = [ -162,  -81,    0,    0,   81, -648]
  K1(127,:) = [ 1152, -144, -144,   18,   18,  180]
  K1(128,:) = [ -144, 1152,   18,  180, -144,   18]
  K1(129,:) = [ -144,   18, 1152, -144,  180,   18]
  K1(130,:) = [   18,  180, -144, 1152,   18, -144]
  K1(131,:) = [   18, -144,  180,   18, 1152, -144]
  K1(132,:) = [  180,   18,   18, -144, -144, 1152]
  K1(133,:) = [    0,    0,    0,    0,    0,    0]
  K1(134,:) = [    0,    0,    0,    0,    0,    0]
  K1(135,:) = [    0,    0,    0,    0,    0,    0]
  K1(136,:) = [    0,    0,    0,    0,    0,    0]
  K1(137,:) = [    0,    0,    0,    0,    0,    0]
  K1(138,:) = [    0,    0,    0,    0,    0,    0]
  K1 = (1._/**/REALKIND / 54) * K1

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppwjj_ckm_usxwggg_1_/**/REALKIND



module ol_forced_parameters_ppwjj_ckm_usxwggg_1_/**/REALKIND
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
  if (ME /= 0) write(*,101) 'ME = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppwjj_ckm_usxwggg_1_/**/REALKIND

module ol_tree_ppwjj_ckm_usxwggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(2)
  complex(REALKIND), save :: den(105)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 96 ! number of helicity configurations
  integer(intkind2), save :: nhel = 96 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(96) ! physical helicity states
  complex(DREALKIND) :: M1helarr(6,96) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f(1) = (CI*eQED*gQCD**3*VCKMsu)/(sqrt2*sw)
    f(2) = (eQED*gQCD**3*VCKMsu)/(sqrt2*sw)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,5))
  den(2) = 1 / (Q(5,56))
  den(4) = 1 / (Q(5,6))
  den(6) = 1 / (Q(5,10))
  den(7) = 1 / (Q(5,48))
  den(10) = 1 / (Q(5,21))
  den(13) = 1 / (Q(5,26))
  den(16) = 1 / (Q(5,18))
  den(17) = 1 / (Q(5,40))
  den(20) = 1 / (Q(5,13))
  den(25) = 1 / (Q(5,34))
  den(26) = 1 / (Q(5,24))
  den(29) = 1 / (Q(5,7))
  den(35) = 1 / (Q(5,42))
  den(42) = 1 / (Q(5,50))
  den(45) = 1 / (Q(5,9))
  den(48) = 1 / (Q(5,25))
  den(51) = 1 / (Q(5,22))
  den(54) = 1 / (Q(5,17))
  den(59) = 1 / (Q(5,14))
  den(62) = 1 / (Q(5,33))
  den(69) = 1 / (Q(5,41))
  den(77) = 1 / (Q(5,49))
  den(85) = 1 / (Q(5,38))
  den(93) = 1 / (Q(5,37))

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
    & bind(c,name="ol_f_amp2_ppwjj_ckm_usxwggg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_ppwjj_ckm_usxwggg_1_/**/DREALKIND, only: &
    & ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for up anti-strange W- glue glue glue -> 0
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
  use ol_external_ppwjj_ckm_usxwggg_1, only: &
    & external_perm_ppwjj_ckm_usxwggg_1, &
    & external_perm_inv_ppwjj_ckm_usxwggg_1, &
    & extcomb_perm_ppwjj_ckm_usxwggg_1, &
    & average_factor_ppwjj_ckm_usxwggg_1
  use ol_external_ppwjj_ckm_usxwggg_1, only: &
    & H, hel_not_initialised, hel_init, POLSEL
  use ol_colourmatrix_ppwjj_ckm_usxwggg_1_/**/REALKIND, only: &
    & colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_ppwjj_ckm_usxwggg_1_/**/REALKIND, only: &
    & check_forced_parameters
  use ol_heltables_ppwjj_ckm_usxwggg_1
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
  complex(REALKIND) :: MOM_LC(4), M1(6), M1helarray(6,96)
  real(REALKIND)    :: P_scatt_intern(0:3,6)
  complex(REALKIND) :: epLC(1:4), epStd(1:4,2)
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(3), ex4(2), ex5(2), ex6(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf4(4,15), wf6(6,4), wf8(8,36), wf12(12,24), wf24(24,6), wf96(96,54)

  type(polcont) :: A(96,54)
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
  extmasses2 = [ rZERO2, rZERO2, rMW2, rZERO2, rZERO2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, &
    & external_perm_inv_ppwjj_ckm_usxwggg_1,6)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,6)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_ppwjj_ckm_usxwggg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_ppwjj_ckm_usxwggg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppwjj_ckm_usxwggg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  42 continue
  ! external WFs
  call pol_wf_Q(P(:,1), rZERO, H1, ex1, POLSEL(1))
  call pol_wf_A(P(:,2), rZERO, H2, ex2, POLSEL(2))
  call pol_wf_V(P(:,3), rMW, H3, ex3, POLSEL(3))
  call pol_wf_V(P(:,4), rZERO, H4, ex4, POLSEL(4))
  call pol_wf_V(P(:,5), rZERO, H5, ex5, POLSEL(5))
  call pol_wf_V(P(:,6), rZERO, H6, ex6, POLSEL(6))


  if (ntry == 1) then
    shift = 1
    ! call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    ! call helbookkeeping_wf(H1, ex1, shift) ...

    if (any(POLSEL /= 0)) then

      call pol_wf_Q(P(:,1), rZERO, H1, ex1, 0)
      call pol_wf_A(P(:,2), rZERO, H2, ex2, 0)
      call pol_wf_V(P(:,3), rMW, H3, ex3, 0)
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
  call vert_WQ_A(ntry, ex3, ex1, wf6(:,1), n3(:,1), t3x6(:,:,1))
  call vert_GGG_G(ntry, ex4, ex5, ex6, wf8(:,1), n4(:,1), t4x8(:,:,1))
  call prop_Q_A(ntry, wf6(:,1), Q(:,5), ZERO, 0_intkind1, wf6(:,2), n2(1))
  call vert_QA_V(ntry, wf6(:,2), ex2, wf12(:,1), n3(:,2), t3x12(:,:,1))
  call vert_GGG_G(ntry, ex5, ex6, ex4, wf8(:,2), n4(:,2), t4x8(:,:,2))
  call vert_GGG_G(ntry, ex6, ex4, ex5, wf8(:,3), n4(:,3), t4x8(:,:,3))
  call vert_AW_Q(ntry, ex2, ex3, wf6(:,3), n3(:,3), t3x6(:,:,2))
  call prop_A_Q(ntry, wf6(:,3), Q(:,6), ZERO, 0_intkind1, wf6(:,4), n2(2))
  call vert_QA_V(ntry, ex1, wf6(:,4), wf12(:,2), n3(:,4), t3x12(:,:,2))
  call vert_AV_Q(ntry, ex2, ex4, wf4(:,1), n3(:,5), t3x4(:,:,1))
  call vert_UV_W(ntry, ex5, Q(:,16), ex6, Q(:,32), wf4(:,2), n3(:,6), t3x4(:,:,2))
  call prop_A_Q(ntry, wf4(:,1), Q(:,10), ZERO, 0_intkind1, wf4(:,3), n2(3))
  call vert_QA_V(ntry, wf6(:,2), wf4(:,3), wf24(:,1), n3(:,7), t3x24(:,:,1))
  call vert_VQ_A(ntry, ex5, wf6(:,2), wf12(:,3), n3(:,8), t3x12(:,:,3))
  call vert_AV_Q(ntry, wf4(:,3), ex6, wf8(:,4), n3(:,9), t3x8(:,:,1))
  call prop_Q_A(ntry, wf12(:,3), Q(:,21), ZERO, 0_intkind1, wf12(:,4), n2(4))
  call vert_AV_Q(ntry, wf4(:,3), ex5, wf8(:,5), n3(:,10), t3x8(:,:,2))
  call vert_VQ_A(ntry, ex6, wf6(:,2), wf12(:,5), n3(:,11), t3x12(:,:,4))
  call prop_A_Q(ntry, wf8(:,5), Q(:,26), ZERO, 0_intkind1, wf8(:,6), n2(5))
  call vert_AV_Q(ntry, ex2, ex5, wf4(:,4), n3(:,12), t3x4(:,:,3))
  call vert_UV_W(ntry, ex4, Q(:,8), ex6, Q(:,32), wf4(:,5), n3(:,13), t3x4(:,:,4))
  call prop_A_Q(ntry, wf4(:,4), Q(:,18), ZERO, 0_intkind1, wf4(:,6), n2(6))
  call vert_QA_V(ntry, wf6(:,2), wf4(:,6), wf24(:,2), n3(:,14), t3x24(:,:,2))
  call vert_VQ_A(ntry, ex4, wf6(:,2), wf12(:,6), n3(:,15), t3x12(:,:,5))
  call vert_AV_Q(ntry, wf4(:,6), ex6, wf8(:,7), n3(:,16), t3x8(:,:,3))
  call prop_Q_A(ntry, wf12(:,6), Q(:,13), ZERO, 0_intkind1, wf12(:,7), n2(7))
  call vert_AV_Q(ntry, wf4(:,6), ex4, wf8(:,8), n3(:,17), t3x8(:,:,4))
  call prop_A_Q(ntry, wf8(:,8), Q(:,26), ZERO, 0_intkind1, wf8(:,9), n2(8))
  call vert_AV_Q(ntry, ex2, ex6, wf4(:,7), n3(:,18), t3x4(:,:,5))
  call vert_UV_W(ntry, ex4, Q(:,8), ex5, Q(:,16), wf4(:,8), n3(:,19), t3x4(:,:,6))
  call prop_A_Q(ntry, wf4(:,7), Q(:,34), ZERO, 0_intkind1, wf4(:,9), n2(9))
  call vert_QA_V(ntry, wf6(:,2), wf4(:,9), wf24(:,3), n3(:,20), t3x24(:,:,3))
  call vert_UV_W(ntry, wf4(:,8), Q(:,24), ex6, Q(:,32), wf8(:,10), n3(:,21), t3x8(:,:,5))
  call vert_AV_Q(ntry, ex2, wf4(:,8), wf8(:,11), n3(:,22), t3x8(:,:,6))
  call prop_A_Q(ntry, wf8(:,11), Q(:,26), ZERO, 0_intkind1, wf8(:,12), n2(10))
  call vert_AV_Q(ntry, wf4(:,9), ex5, wf8(:,13), n3(:,23), t3x8(:,:,7))
  call vert_AV_Q(ntry, wf4(:,9), ex4, wf8(:,14), n3(:,24), t3x8(:,:,8))
  call prop_A_Q(ntry, wf8(:,14), Q(:,42), ZERO, 0_intkind1, wf8(:,15), n2(11))
  call vert_UV_W(ntry, ex5, Q(:,16), wf4(:,5), Q(:,40), wf8(:,16), n3(:,25), t3x8(:,:,9))
  call vert_AV_Q(ntry, ex2, wf4(:,5), wf8(:,17), n3(:,26), t3x8(:,:,10))
  call prop_A_Q(ntry, wf8(:,17), Q(:,42), ZERO, 0_intkind1, wf8(:,18), n2(12))
  call vert_UV_W(ntry, ex4, Q(:,8), wf4(:,2), Q(:,48), wf8(:,19), n3(:,27), t3x8(:,:,11))
  call vert_AV_Q(ntry, ex2, wf4(:,2), wf8(:,20), n3(:,28), t3x8(:,:,12))
  call prop_A_Q(ntry, wf8(:,20), Q(:,50), ZERO, 0_intkind1, wf8(:,21), n2(13))
  call vert_VQ_A(ntry, ex4, ex1, wf4(:,10), n3(:,29), t3x4(:,:,7))
  call prop_Q_A(ntry, wf4(:,10), Q(:,9), ZERO, 0_intkind1, wf4(:,11), n2(14))
  call vert_QA_V(ntry, wf4(:,11), wf6(:,4), wf24(:,4), n3(:,30), t3x24(:,:,4))
  call vert_VQ_A(ntry, ex5, wf4(:,11), wf8(:,22), n3(:,31), t3x8(:,:,13))
  call vert_AV_Q(ntry, wf6(:,4), ex6, wf12(:,8), n3(:,32), t3x12(:,:,6))
  call prop_Q_A(ntry, wf8(:,22), Q(:,25), ZERO, 0_intkind1, wf8(:,23), n2(15))
  call vert_AV_Q(ntry, wf6(:,4), ex5, wf12(:,9), n3(:,33), t3x12(:,:,7))
  call vert_VQ_A(ntry, ex6, wf4(:,11), wf8(:,24), n3(:,34), t3x8(:,:,14))
  call prop_A_Q(ntry, wf12(:,9), Q(:,22), ZERO, 0_intkind1, wf12(:,10), n2(16))
  call vert_VQ_A(ntry, ex5, ex1, wf4(:,12), n3(:,35), t3x4(:,:,8))
  call prop_Q_A(ntry, wf4(:,12), Q(:,17), ZERO, 0_intkind1, wf4(:,13), n2(17))
  call vert_QA_V(ntry, wf4(:,13), wf6(:,4), wf24(:,5), n3(:,36), t3x24(:,:,5))
  call vert_VQ_A(ntry, ex4, wf4(:,13), wf8(:,25), n3(:,37), t3x8(:,:,15))
  call prop_Q_A(ntry, wf8(:,25), Q(:,25), ZERO, 0_intkind1, wf8(:,26), n2(18))
  call vert_AV_Q(ntry, wf6(:,4), ex4, wf12(:,11), n3(:,38), t3x12(:,:,8))
  call vert_VQ_A(ntry, ex6, wf4(:,13), wf8(:,27), n3(:,39), t3x8(:,:,16))
  call prop_A_Q(ntry, wf12(:,11), Q(:,14), ZERO, 0_intkind1, wf12(:,12), n2(19))
  call vert_VQ_A(ntry, ex6, ex1, wf4(:,14), n3(:,40), t3x4(:,:,9))
  call prop_Q_A(ntry, wf4(:,14), Q(:,33), ZERO, 0_intkind1, wf4(:,15), n2(20))
  call vert_QA_V(ntry, wf4(:,15), wf6(:,4), wf24(:,6), n3(:,41), t3x24(:,:,6))
  call vert_VQ_A(ntry, wf4(:,8), ex1, wf8(:,28), n3(:,42), t3x8(:,:,17))
  call prop_Q_A(ntry, wf8(:,28), Q(:,25), ZERO, 0_intkind1, wf8(:,29), n2(21))
  call vert_VQ_A(ntry, ex4, wf4(:,15), wf8(:,30), n3(:,43), t3x8(:,:,18))
  call prop_Q_A(ntry, wf8(:,30), Q(:,41), ZERO, 0_intkind1, wf8(:,31), n2(22))
  call vert_VQ_A(ntry, ex5, wf4(:,15), wf8(:,32), n3(:,44), t3x8(:,:,19))
  call vert_VQ_A(ntry, wf4(:,5), ex1, wf8(:,33), n3(:,45), t3x8(:,:,20))
  call prop_Q_A(ntry, wf8(:,33), Q(:,41), ZERO, 0_intkind1, wf8(:,34), n2(23))
  call vert_VQ_A(ntry, wf4(:,2), ex1, wf8(:,35), n3(:,46), t3x8(:,:,21))
  call prop_Q_A(ntry, wf8(:,35), Q(:,49), ZERO, 0_intkind1, wf8(:,36), n2(24))
  call vert_WQ_A(ntry, ex3, wf4(:,11), wf12(:,13), n3(:,47), t3x12(:,:,9))
  call prop_Q_A(ntry, wf12(:,13), Q(:,13), ZERO, 0_intkind1, wf12(:,14), n2(25))
  call vert_AW_Q(ntry, wf4(:,6), ex3, wf12(:,15), n3(:,48), t3x12(:,:,10))
  call prop_A_Q(ntry, wf12(:,15), Q(:,22), ZERO, 0_intkind1, wf12(:,16), n2(26))
  call vert_AW_Q(ntry, wf4(:,9), ex3, wf12(:,17), n3(:,49), t3x12(:,:,11))
  call prop_A_Q(ntry, wf12(:,17), Q(:,38), ZERO, 0_intkind1, wf12(:,18), n2(27))
  call vert_WQ_A(ntry, ex3, wf4(:,13), wf12(:,19), n3(:,50), t3x12(:,:,12))
  call prop_Q_A(ntry, wf12(:,19), Q(:,21), ZERO, 0_intkind1, wf12(:,20), n2(28))
  call vert_AW_Q(ntry, wf4(:,3), ex3, wf12(:,21), n3(:,51), t3x12(:,:,13))
  call prop_A_Q(ntry, wf12(:,21), Q(:,14), ZERO, 0_intkind1, wf12(:,22), n2(29))
  call vert_WQ_A(ntry, ex3, wf4(:,15), wf12(:,23), n3(:,52), t3x12(:,:,14))
  call prop_Q_A(ntry, wf12(:,23), Q(:,37), ZERO, 0_intkind1, wf12(:,24), n2(30))


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

    M2munu = M2munu / average_factor_ppwjj_ckm_usxwggg_1
  end if

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2add = M2 / average_factor_ppwjj_ckm_usxwggg_1

  do k = 0, 23-1
    M2(k) = M2add(extcomb_perm_ppwjj_ckm_usxwggg_1(k))
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

    call cont_VV(nsync, wf8(:,1), wf12(:,1), A(:,1), n3(:,53), t3x96(:,:,1), nhel, den(3))
    call cont_VV(nsync, wf12(:,1), wf8(:,2), A(:,2), n3(:,54), t3x96(:,:,2), nhel, den(3))
    call cont_VV(nsync, wf12(:,1), wf8(:,3), A(:,3), n3(:,55), t3x96(:,:,3), nhel, den(3))
    call cont_VV(nsync, wf8(:,1), wf12(:,2), A(:,4), n3(:,56), t3x96(:,:,4), nhel, den(5))
    call cont_VV(nsync, wf8(:,2), wf12(:,2), A(:,5), n3(:,57), t3x96(:,:,5), nhel, den(5))
    call cont_VV(nsync, wf8(:,3), wf12(:,2), A(:,6), n3(:,58), t3x96(:,:,6), nhel, den(5))
    call cont_VV(nsync, wf4(:,2), wf24(:,1), A(:,7), n3(:,59), t3x96(:,:,7), nhel, den(9))
    call cont_QA(nsync, wf8(:,4), wf12(:,4), A(:,8), n3(:,60), t3x96(:,:,8), nhel, den(12))
    call cont_QA(nsync, wf12(:,5), wf8(:,6), A(:,9), n3(:,61), t3x96(:,:,9), nhel, den(15))
    call cont_VV(nsync, wf4(:,5), wf24(:,2), A(:,10), n3(:,62), t3x96(:,:,10), nhel, den(19))
    call cont_QA(nsync, wf8(:,7), wf12(:,7), A(:,11), n3(:,63), t3x96(:,:,11), nhel, den(22))
    call cont_QA(nsync, wf12(:,5), wf8(:,9), A(:,12), n3(:,64), t3x96(:,:,12), nhel, den(24))
    call cont_VV(nsync, wf4(:,8), wf24(:,3), A(:,13), n3(:,65), t3x96(:,:,13), nhel, den(28))
    call cont_VV(nsync, wf12(:,1), wf8(:,10), A(:,14), n3(:,66), t3x96(:,:,14), nhel, den(31))
    call cont_QA(nsync, wf12(:,5), wf8(:,12), A(:,15), n3(:,67), t3x96(:,:,15), nhel, den(33))
    call cont_QA(nsync, wf12(:,7), wf8(:,13), A(:,16), n3(:,68), t3x96(:,:,16), nhel, den(34))
    call cont_QA(nsync, wf12(:,3), wf8(:,15), A(:,17), n3(:,69), t3x96(:,:,17), nhel, den(37))
    call cont_VV(nsync, wf12(:,1), wf8(:,16), A(:,18), n3(:,70), t3x96(:,:,18), nhel, den(38))
    call cont_QA(nsync, wf12(:,3), wf8(:,18), A(:,19), n3(:,71), t3x96(:,:,19), nhel, den(40))
    call cont_VV(nsync, wf12(:,1), wf8(:,19), A(:,20), n3(:,72), t3x96(:,:,20), nhel, den(41))
    call cont_QA(nsync, wf12(:,6), wf8(:,21), A(:,21), n3(:,73), t3x96(:,:,21), nhel, den(44))
    call cont_VV(nsync, wf4(:,2), wf24(:,4), A(:,22), n3(:,74), t3x96(:,:,22), nhel, den(47))
    call cont_QA(nsync, wf12(:,8), wf8(:,23), A(:,23), n3(:,75), t3x96(:,:,23), nhel, den(50))
    call cont_QA(nsync, wf8(:,24), wf12(:,10), A(:,24), n3(:,76), t3x96(:,:,24), nhel, den(53))
    call cont_VV(nsync, wf4(:,5), wf24(:,5), A(:,25), n3(:,77), t3x96(:,:,25), nhel, den(56))
    call cont_QA(nsync, wf12(:,8), wf8(:,26), A(:,26), n3(:,78), t3x96(:,:,26), nhel, den(58))
    call cont_QA(nsync, wf8(:,27), wf12(:,12), A(:,27), n3(:,79), t3x96(:,:,27), nhel, den(61))
    call cont_VV(nsync, wf4(:,8), wf24(:,6), A(:,28), n3(:,80), t3x96(:,:,28), nhel, den(64))
    call cont_VV(nsync, wf12(:,2), wf8(:,10), A(:,29), n3(:,81), t3x96(:,:,29), nhel, den(66))
    call cont_QA(nsync, wf12(:,8), wf8(:,29), A(:,30), n3(:,82), t3x96(:,:,30), nhel, den(68))
    call cont_QA(nsync, wf12(:,9), wf8(:,31), A(:,31), n3(:,83), t3x96(:,:,31), nhel, den(71))
    call cont_QA(nsync, wf12(:,12), wf8(:,32), A(:,32), n3(:,84), t3x96(:,:,32), nhel, den(72))
    call cont_VV(nsync, wf12(:,2), wf8(:,16), A(:,33), n3(:,85), t3x96(:,:,33), nhel, den(73))
    call cont_QA(nsync, wf12(:,9), wf8(:,34), A(:,34), n3(:,86), t3x96(:,:,34), nhel, den(75))
    call cont_VV(nsync, wf12(:,2), wf8(:,19), A(:,35), n3(:,87), t3x96(:,:,35), nhel, den(76))
    call cont_QA(nsync, wf12(:,11), wf8(:,36), A(:,36), n3(:,88), t3x96(:,:,36), nhel, den(79))
    call cont_QA(nsync, wf8(:,7), wf12(:,14), A(:,37), n3(:,89), t3x96(:,:,37), nhel, den(81))
    call cont_QA(nsync, wf8(:,24), wf12(:,16), A(:,38), n3(:,90), t3x96(:,:,38), nhel, den(83))
    call cont_QA(nsync, wf8(:,13), wf12(:,14), A(:,39), n3(:,91), t3x96(:,:,39), nhel, den(84))
    call cont_QA(nsync, wf8(:,22), wf12(:,18), A(:,40), n3(:,92), t3x96(:,:,40), nhel, den(87))
    call cont_QA(nsync, wf8(:,21), wf12(:,13), A(:,41), n3(:,93), t3x96(:,:,41), nhel, den(88))
    call cont_QA(nsync, wf8(:,4), wf12(:,20), A(:,42), n3(:,94), t3x96(:,:,42), nhel, den(90))
    call cont_QA(nsync, wf8(:,27), wf12(:,22), A(:,43), n3(:,95), t3x96(:,:,43), nhel, den(92))
    call cont_QA(nsync, wf8(:,5), wf12(:,24), A(:,44), n3(:,96), t3x96(:,:,44), nhel, den(95))
    call cont_QA(nsync, wf8(:,32), wf12(:,22), A(:,45), n3(:,97), t3x96(:,:,45), nhel, den(96))
    call cont_QA(nsync, wf8(:,36), wf12(:,21), A(:,46), n3(:,98), t3x96(:,:,46), nhel, den(97))
    call cont_QA(nsync, wf8(:,14), wf12(:,20), A(:,47), n3(:,99), t3x96(:,:,47), nhel, den(98))
    call cont_QA(nsync, wf8(:,25), wf12(:,18), A(:,48), n3(:,100), t3x96(:,:,48), nhel, den(99))
    call cont_QA(nsync, wf8(:,18), wf12(:,19), A(:,49), n3(:,101), t3x96(:,:,49), nhel, den(100))
    call cont_QA(nsync, wf8(:,8), wf12(:,24), A(:,50), n3(:,102), t3x96(:,:,50), nhel, den(101))
    call cont_QA(nsync, wf8(:,30), wf12(:,16), A(:,51), n3(:,103), t3x96(:,:,51), nhel, den(102))
    call cont_QA(nsync, wf8(:,34), wf12(:,15), A(:,52), n3(:,104), t3x96(:,:,52), nhel, den(103))
    call cont_QA(nsync, wf8(:,12), wf12(:,23), A(:,53), n3(:,105), t3x96(:,:,53), nhel, den(104))
    call cont_QA(nsync, wf8(:,29), wf12(:,17), A(:,54), n3(:,106), t3x96(:,:,54), nhel, den(105))

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
  complex(REALKIND), intent(out) :: M1(6) ! M1helarray(6,96)
  integer :: empty(0)

  M1(1) = (-A(j,1)%j+A(j,2)%j-A(j,4)%j+A(j,5)%j-A(j,9)%j-A(j,14)%j-A(j,20)%j-A(j,29)%j-A(j,32)%j-A(j,35)%j-A(j,44)%j &
       -A(j,45)%j)*f(1)+CI*(-A(j,7)%j-A(j,15)%j-A(j,28)%j-A(j,36)%j-A(j,46)%j-A(j,53)%j)*f(2)
  M1(2) = (-A(j,2)%j+A(j,3)%j-A(j,5)%j+A(j,6)%j-A(j,8)%j+A(j,18)%j+A(j,20)%j-A(j,27)%j+A(j,33)%j+A(j,35)%j-A(j,42)%j &
       -A(j,43)%j)*f(1)+CI*(A(j,7)%j-A(j,19)%j-A(j,25)%j+A(j,36)%j+A(j,46)%j-A(j,49)%j)*f(2)
  M1(3) = (A(j,1)%j-A(j,3)%j+A(j,4)%j-A(j,6)%j-A(j,12)%j+A(j,14)%j-A(j,18)%j+A(j,29)%j-A(j,31)%j-A(j,33)%j-A(j,50)%j &
       -A(j,51)%j)*f(1)+CI*(-A(j,10)%j+A(j,15)%j+A(j,28)%j-A(j,34)%j-A(j,52)%j+A(j,53)%j)*f(2)
  M1(4) = (-A(j,2)%j+A(j,3)%j-A(j,5)%j+A(j,6)%j-A(j,11)%j+A(j,18)%j+A(j,20)%j-A(j,24)%j+A(j,33)%j+A(j,35)%j-A(j,37)%j &
       -A(j,38)%j)*f(1)+CI*(A(j,10)%j-A(j,21)%j-A(j,22)%j+A(j,34)%j-A(j,41)%j+A(j,52)%j)*f(2)
  M1(5) = (A(j,1)%j-A(j,3)%j+A(j,4)%j-A(j,6)%j+A(j,14)%j-A(j,17)%j-A(j,18)%j-A(j,26)%j+A(j,29)%j-A(j,33)%j-A(j,47)%j &
       -A(j,48)%j)*f(1)+CI*(-A(j,13)%j+A(j,19)%j+A(j,25)%j-A(j,30)%j+A(j,49)%j-A(j,54)%j)*f(2)
  M1(6) = (-A(j,1)%j+A(j,2)%j-A(j,4)%j+A(j,5)%j-A(j,14)%j-A(j,16)%j-A(j,20)%j-A(j,23)%j-A(j,29)%j-A(j,35)%j-A(j,39)%j &
       -A(j,40)%j)*f(1)+CI*(A(j,13)%j+A(j,21)%j+A(j,22)%j+A(j,30)%j+A(j,41)%j+A(j,54)%j)*f(2)

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
  use ol_colourmatrix_ppwjj_ckm_usxwggg_1_/**/REALKIND, only: K1
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
  use ol_colourmatrix_ppwjj_ckm_usxwggg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(6), M2(6)
  real(REALKIND),    intent(out) :: M2colint(0:23-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 6*extcomb
    do i = 1, 6
      do j = 1, 6
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
  use ol_colourmatrix_ppwjj_ckm_usxwggg_1_/**/REALKIND, only: K1
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
  & bind(c,name="ol_tree_colvect_ppwjj_ckm_usxwggg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(6,96)
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
    & bind(c,name="ol_f_amp2tree_ppwjj_ckm_usxwggg_1")
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
    & bind(c,name="ol_f_amp2ccone_ppwjj_ckm_usxwggg_1")
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
    & bind(c,name="ol_f_amp2ccall_ppwjj_ckm_usxwggg_1")
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
    & bind(c,name="ol_f_amp2hcone_ppwjj_ckm_usxwggg_1")
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
    & bind(c,name="ol_f_amp2hcall_ppwjj_ckm_usxwggg_1")
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
    & bind(c,name="ol_amp2tree_ppwjj_ckm_usxwggg_1")
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
    & bind(c,name="ol_amp2ccone_ppwjj_ckm_usxwggg_1")
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
    & bind(c,name="ol_amp2ccall_ppwjj_ckm_usxwggg_1")
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
    & bind(c,name="ol_amp2hcone_ppwjj_ckm_usxwggg_1")
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
    & bind(c,name="ol_amp2hcall_ppwjj_ckm_usxwggg_1")
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
    & bind(c,name="amp2tree_ppwjj_ckm_usxwggg_1_")
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
    & bind(c,name="amp2ccone_ppwjj_ckm_usxwggg_1_")
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
    & bind(c,name="amp2ccall_ppwjj_ckm_usxwggg_1_")
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
    & bind(c,name="amp2hcone_ppwjj_ckm_usxwggg_1_")
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
    & bind(c,name="amp2hcall_ppwjj_ckm_usxwggg_1_")
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

end module ol_tree_ppwjj_ckm_usxwggg_1_/**/REALKIND
