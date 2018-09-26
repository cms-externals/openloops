
module ol_colourmatrix_pphhjj_ddxbbxhhg_1_/**/REALKIND
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
  K1( 3,:) = [  16,   0]
  K1( 4,:) = [   0,  16]
  K1( 5,:) = [   2,   0]
  K1( 6,:) = [   0, -16]
  K1( 7,:) = [  16,   0]
  K1( 8,:) = [   0,  16]
  K1( 9,:) = [   0,   2]
  K1(10,:) = [   2,   0]
  K1(11,:) = [   0,  -2]
  K1(12,:) = [  -2,   0]
  K1(13,:) = [  16,   0]
  K1(14,:) = [   0,  16]
  K1(15,:) = [   0,  -2]
  K1(16,:) = [  -2,   0]
  K1(17,:) = [   0,   2]
  K1(18,:) = [   2,   0]
  K1(19,:) = [ -16,   0]
  K1(20,:) = [   0,   2]
  K1(21,:) = [  16,   0]
  K1(22,:) = [   0,  16]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   0,   0]
  K1(28,:) = [   0,   0]
  K1(29,:) = [   0,   0]
  K1(30,:) = [   0,   0]
  K1(31,:) = [   0,   0]
  K1(32,:) = [   0,   0]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [   0,   0]
  K1(42,:) = [   0,   0]
  K1(43,:) = [   0,   0]
  K1(44,:) = [   0,   0]
  K1(45,:) = [ -18,   0]
  K1(46,:) = [   0,   0]
  K1(47,:) = [ -18,   0]
  K1(48,:) = [   0,   0]
  K1(49,:) = [   0,   0]
  K1(50,:) = [   0, -18]
  K1(51,:) = [   0,   0]
  K1(52,:) = [   0, -18]
  K1(53,:) = [   0,   0]
  K1(54,:) = [   0,   0]
  K1(55,:) = [   0,   0]
  K1(56,:) = [   0,   0]
  K1(57,:) = [  36,   0]
  K1(58,:) = [   0,  36]
  K1(59,:) = [   0,   0]
  K1(60,:) = [   0,   0]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphhjj_ddxbbxhhg_1_/**/REALKIND



module ol_forced_parameters_pphhjj_ddxbbxhhg_1_/**/REALKIND
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
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphhjj_ddxbbxhhg_1_/**/REALKIND

module ol_tree_pphhjj_ddxbbxhhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, DREALKIND, intkind1, intkind2
  implicit none
  integer,           save :: factors_status = -1
  complex(REALKIND), save :: f(13)
  complex(REALKIND), save :: den(209)
#ifdef PRECISION_dp
  integer(intkind1), save :: ntry = 1
  integer(intkind2), parameter :: nheltot = 32 ! number of helicity configurations
  integer(intkind2), save :: nhel = 32 ! number of non-vanishing helicity configurations (adapted at runtime)
  integer(intkind2), save :: Hel(32) ! physical helicity states
  complex(DREALKIND) :: M1helarr(2,32) ! cache
#endif

  contains

subroutine factors_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  implicit none
  factors_status = parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED**4*gQCD)/(4._/**/REALKIND*cw**2*sw**2)
    f( 2) = (CI*eQED**4*gQCD)/(2._/**/REALKIND*cw**2*sw**2)
    f( 3) = (3*CI*eQED**4*gQCD*lambdaHHH*lambdaHZZ*MH**2)/(2._/**/REALKIND*cw**2*sw**2)
    f( 4) = (CI*eQED**4*gQCD*lambdaHZZ**2*MW**2)/(cw**4*sw**2)
    f( 5) = (CI*eQED**4*gQCD*lambdaHZZ*YB)/(4._/**/REALKIND*cw**3*sw**3)
    f( 6) = (CI*eQED**4*gQCD*MH**2*YB)/(8._/**/REALKIND*cw*MW**2*sw**3)
    f( 7) = (3*CI*eQED**4*gQCD*lambdaHHH*MH**2*YB)/(8._/**/REALKIND*cw*MW**2*sw**3)
    f( 8) = (CI*eQED**4*gQCD*lambdaHZZ*YB)/(2._/**/REALKIND*cw**2*sw**2)
    f( 9) = (CI*eQED**4*gQCD*lambdaHHH*MH**2*YB)/(12._/**/REALKIND*MW**2*sw**2)
    f(10) = (3*CI*eQED**4*gQCD*lambdaHHH*MH**2*YB)/(4._/**/REALKIND*MW**2*sw**2)
    f(11) = (CI*eQED**4*gQCD*YB**2)/(8._/**/REALKIND*cw*MW**2*sw**3)
    f(12) = (CI*eQED**4*gQCD*YB**2)/(36._/**/REALKIND*MW**2*sw**2)
    f(13) = (CI*eQED**4*gQCD*YB**2)/(4._/**/REALKIND*MW**2*sw**2)

end subroutine factors_init


subroutine born_denominators_init()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  ! propagators

  den(1) = 1 / (Q(5,68) - MB2)
  den(2) = 1 / (Q(5,3) - MZ2)
  den(3) = 1 / (Q(5,76) - MZ2)
  den(6) = 1 / (Q(5,72) - MB2)
  den(9) = 1 / (Q(5,65))
  den(10) = 1 / (Q(5,12) - MZ2)
  den(11) = 1 / (Q(5,67) - MZ2)
  den(14) = 1 / (Q(5,66))
  den(17) = 1 / (Q(5,20) - MB2)
  den(18) = 1 / (Q(5,3))
  den(19) = 1 / (Q(5,40) - MB2)
  den(21) = 1 / (Q(5,84) - MB2)
  den(27) = 1 / (Q(5,104) - MB2)
  den(33) = 1 / (Q(5,35) - MZ2)
  den(37) = 1 / (Q(5,52) - MB2)
  den(45) = 1 / (Q(5,11) - MB2)
  den(53) = 1 / (Q(5,36) - MB2)
  den(54) = 1 / (Q(5,24) - MB2)
  den(56) = 1 / (Q(5,100) - MB2)
  den(62) = 1 / (Q(5,88) - MB2)
  den(73) = 1 / (Q(5,56) - MB2)
  den(78) = 1 / (Q(5,7) - MB2)
  den(87) = 1 / (Q(5,19) - MZ2)
  den(114) = 1 / (Q(5,48) - MH2)
  den(153) = 1 / (Q(5,44) - MZ2)
  den(156) = 1 / (Q(5,28) - MZ2)
  den(162) = 1 / (Q(5,67))

  ! denominators

  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(3)*den(6)
  den(8) = den(2)*den(7)
  den(12) = den(9)*den(11)
  den(13) = den(10)*den(12)
  den(15) = den(11)*den(14)
  den(16) = den(10)*den(15)
  den(20) = den(18)*den(19)
  den(22) = den(17)*den(21)
  den(23) = den(20)*den(22)
  den(24) = den(2)*den(19)
  den(25) = den(22)*den(24)
  den(26) = den(17)*den(18)
  den(28) = den(19)*den(27)
  den(29) = den(26)*den(28)
  den(30) = den(2)*den(17)
  den(31) = den(28)*den(30)
  den(32) = den(6)*den(17)
  den(34) = den(2)*den(33)
  den(35) = den(32)*den(34)
  den(36) = den(6)*den(18)
  den(38) = den(17)*den(37)
  den(39) = den(36)*den(38)
  den(40) = den(2)*den(6)
  den(41) = den(38)*den(40)
  den(42) = den(6)*den(27)
  den(43) = den(26)*den(42)
  den(44) = den(30)*den(42)
  den(46) = den(18)*den(45)
  den(47) = den(38)*den(46)
  den(48) = den(2)*den(45)
  den(49) = den(38)*den(48)
  den(50) = den(22)*den(46)
  den(51) = den(22)*den(48)
  den(52) = den(22)*den(34)
  den(55) = den(18)*den(54)
  den(57) = den(53)*den(56)
  den(58) = den(55)*den(57)
  den(59) = den(2)*den(54)
  den(60) = den(57)*den(59)
  den(61) = den(18)*den(53)
  den(63) = den(54)*den(62)
  den(64) = den(61)*den(63)
  den(65) = den(2)*den(53)
  den(66) = den(63)*den(65)
  den(67) = den(1)*den(54)
  den(68) = den(34)*den(67)
  den(69) = den(1)*den(56)
  den(70) = den(55)*den(69)
  den(71) = den(59)*den(69)
  den(72) = den(1)*den(18)
  den(74) = den(54)*den(73)
  den(75) = den(72)*den(74)
  den(76) = den(1)*den(2)
  den(77) = den(74)*den(76)
  den(79) = den(18)*den(78)
  den(80) = den(63)*den(79)
  den(81) = den(2)*den(78)
  den(82) = den(63)*den(81)
  den(83) = den(74)*den(79)
  den(84) = den(74)*den(81)
  den(85) = den(34)*den(63)
  den(86) = den(6)*den(53)
  den(88) = den(2)*den(87)
  den(89) = den(86)*den(88)
  den(90) = den(37)*den(53)
  den(91) = den(36)*den(90)
  den(92) = den(40)*den(90)
  den(93) = den(6)*den(62)
  den(94) = den(61)*den(93)
  den(95) = den(65)*den(93)
  den(96) = den(46)*den(90)
  den(97) = den(48)*den(90)
  den(98) = den(46)*den(57)
  den(99) = den(48)*den(57)
  den(100) = den(57)*den(88)
  den(101) = den(1)*den(19)
  den(102) = den(88)*den(101)
  den(103) = den(1)*den(21)
  den(104) = den(20)*den(103)
  den(105) = den(24)*den(103)
  den(106) = den(19)*den(73)
  den(107) = den(72)*den(106)
  den(108) = den(76)*den(106)
  den(109) = den(28)*den(79)
  den(110) = den(28)*den(81)
  den(111) = den(79)*den(106)
  den(112) = den(81)*den(106)
  den(113) = den(28)*den(88)
  den(115) = den(1)*den(114)
  den(116) = den(46)*den(115)
  den(117) = den(48)*den(115)
  den(118) = den(2)*den(114)
  den(119) = den(4)*den(118)
  den(120) = den(73)*den(114)
  den(121) = den(72)*den(120)
  den(122) = den(76)*den(120)
  den(123) = den(6)*den(114)
  den(124) = den(79)*den(123)
  den(125) = den(81)*den(123)
  den(126) = den(7)*den(118)
  den(127) = den(37)*den(114)
  den(128) = den(36)*den(127)
  den(129) = den(40)*den(127)
  den(130) = den(79)*den(120)
  den(131) = den(81)*den(120)
  den(132) = den(46)*den(127)
  den(133) = den(48)*den(127)
  den(134) = den(46)*den(103)
  den(135) = den(48)*den(103)
  den(136) = den(46)*den(69)
  den(137) = den(48)*den(69)
  den(138) = den(4)*den(88)
  den(139) = den(69)*den(88)
  den(140) = den(4)*den(34)
  den(141) = den(34)*den(103)
  den(142) = den(42)*den(79)
  den(143) = den(42)*den(81)
  den(144) = den(79)*den(93)
  den(145) = den(81)*den(93)
  den(146) = den(7)*den(88)
  den(147) = den(42)*den(88)
  den(148) = den(7)*den(34)
  den(149) = den(34)*den(93)
  den(150) = den(10)*den(114)
  den(151) = den(12)*den(150)
  den(152) = den(15)*den(150)
  den(154) = den(10)*den(153)
  den(155) = den(12)*den(154)
  den(157) = den(10)*den(156)
  den(158) = den(12)*den(157)
  den(159) = den(15)*den(154)
  den(160) = den(15)*den(157)
  den(161) = den(17)*den(19)
  den(163) = den(9)*den(162)
  den(164) = den(161)*den(163)
  den(165) = den(12)*den(161)
  den(166) = den(14)*den(162)
  den(167) = den(161)*den(166)
  den(168) = den(15)*den(161)
  den(169) = den(38)*den(163)
  den(170) = den(12)*den(38)
  den(171) = den(17)*den(156)
  den(172) = den(12)*den(171)
  den(173) = den(38)*den(166)
  den(174) = den(15)*den(38)
  den(175) = den(15)*den(171)
  den(176) = den(53)*den(54)
  den(177) = den(163)*den(176)
  den(178) = den(12)*den(176)
  den(179) = den(166)*den(176)
  den(180) = den(15)*den(176)
  den(181) = den(74)*den(163)
  den(182) = den(12)*den(74)
  den(183) = den(54)*den(156)
  den(184) = den(12)*den(183)
  den(185) = den(74)*den(166)
  den(186) = den(15)*den(74)
  den(187) = den(15)*den(183)
  den(188) = den(90)*den(163)
  den(189) = den(12)*den(90)
  den(190) = den(53)*den(153)
  den(191) = den(12)*den(190)
  den(192) = den(90)*den(166)
  den(193) = den(15)*den(90)
  den(194) = den(15)*den(190)
  den(195) = den(106)*den(163)
  den(196) = den(12)*den(106)
  den(197) = den(19)*den(153)
  den(198) = den(12)*den(197)
  den(199) = den(106)*den(166)
  den(200) = den(15)*den(106)
  den(201) = den(15)*den(197)
  den(202) = den(120)*den(163)
  den(203) = den(12)*den(120)
  den(204) = den(127)*den(163)
  den(205) = den(12)*den(127)
  den(206) = den(120)*den(166)
  den(207) = den(15)*den(120)
  den(208) = den(127)*den(166)
  den(209) = den(15)*den(127)

end subroutine born_denominators_init


! **********************************************************************
#ifdef PRECISION_dp
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu) &
    & bind(c,name="ol_f_amp2_pphhjj_ddxbbxhhg_1")
#else
subroutine amp2(P_scatt, M2, I, MOM, nextcombs, extcombs, M2munu)
  use ol_tree_pphhjj_ddxbbxhhg_1_/**/DREALKIND, only: ntry, nhel, Hel, M1helarr
#endif
! P_scatt(0:3,Npart) = incoming external momenta
! M2  = helicity-summed squared matrix element for down anti-down bottom anti-bottom higgs higgs glue -> 0
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
  use ol_external_pphhjj_ddxbbxhhg_1, only: external_perm_pphhjj_ddxbbxhhg_1, &
    & external_perm_inv_pphhjj_ddxbbxhhg_1, extcomb_perm_pphhjj_ddxbbxhhg_1, &
    & average_factor_pphhjj_ddxbbxhhg_1
  use ol_external_pphhjj_ddxbbxhhg_1, only: H, hel_not_initialised, hel_init, POLSEL
  use ol_colourmatrix_pphhjj_ddxbbxhhg_1_/**/REALKIND, only: colmat_not_initialised, colourmatrix_init
  use ol_forced_parameters_pphhjj_ddxbbxhhg_1_/**/REALKIND, only: check_forced_parameters
  use ol_heltables_pphhjj_ddxbbxhhg_1
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
  complex(REALKIND) :: MOM_LC(4), M1(2), M1helarray(2,32)
  real(REALKIND)    :: P_scatt_intern(0:3,7)
  complex(REALKIND) :: epLC(1:4), epStd(1:4,2)
  real(REALKIND), save :: scalebackfactor, old_scalefactor = 0
  integer(intkind1) :: nsync

  ! type(wfun) :: ex1(h1), ex2(h2), ... ! external wave functions for h<n> helicities
  type(wfun) :: ex1(2), ex2(2), ex3(2), ex4(2), ex5(1), ex6(1), ex7(2)

  ! type(wfun) :: wf<h>(h,n), ... ! n wave functions with h helicity configurations
  type(wfun) :: wf1(1,1), wf2(2,20), wf4(4,61), wf8(8,76), wf16(16,16), wf32(32,176)

  type(polcont) :: A(32,176)
  complex(REALKIND) :: Aj(176)

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
  extmasses2 = [ rZERO2, rZERO2, rMB2, rMB2, rMH2, rMH2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_pphhjj_ddxbbxhhg_1,7)

  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P,7)

  ! denominators
  call born_denominators_init()

  ReplacePol = 0
  JBmunu = 0
  if (I > 0) then
    ReplacePol = external_perm_pphhjj_ddxbbxhhg_1(I)
  else if (I < 0) then
    JBmunu = external_perm_pphhjj_ddxbbxhhg_1(-I)
  end if

  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_pphhjj_ddxbbxhhg_1(extcombs(k))
  end do

  if (heltables_not_init) call init_heltables()

  42 continue
  ! external WFs
  call pol_wf_Q(P(:,1), rZERO, H1, ex1, POLSEL(1))
  call pol_wf_A(P(:,2), rZERO, H2, ex2, POLSEL(2))
  call pol_wf_Q(P(:,3), rMB, H3, ex3, POLSEL(3))
  call pol_wf_A(P(:,4), rMB, H4, ex4, POLSEL(4))
  call pol_wf_S(P(:,5), rMH, H5, ex5, POLSEL(5))
  call pol_wf_S(P(:,6), rMH, H6, ex6, POLSEL(6))
  call pol_wf_V(P(:,7), rZERO, H7, ex7, POLSEL(7))


  if (ntry == 1) then
    shift = 1
    ! call helbookkeeping_flip(H1, 1, shift, eflip, exthel, firstpol)
    ! call helbookkeeping_wf(H1, ex1, shift) ...

    if (any(POLSEL /= 0)) then

      call pol_wf_Q(P(:,1), rZERO, H1, ex1, 0)
      call pol_wf_A(P(:,2), rZERO, H2, ex2, 0)
      call pol_wf_Q(P(:,3), rMB, H3, ex3, 0)
      call pol_wf_A(P(:,4), rMB, H4, ex4, 0)
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
  call vert_QA_Z(gZd,ntry, ex1, ex2, wf4(:,1), n3(:,1), t3x4(:,:,1))
  call vert_VQ_A(ntry, ex7, ex3, wf4(:,2), n3(:,2), t3x4(:,:,2))
  call prop_Q_A(ntry, wf4(:,2), Q(:,68), MB, 1_intkind1, wf4(:,3), n2(1))
  call vert_QA_Z(gZd,ntry, wf4(:,3), ex4, wf8(:,1), n3(:,3), t3x8(:,:,1))
  call vert_SSV_V(ntry, ex5, ex6, wf4(:,1), wf4(:,4), n4(:,1), t4x4(:,:,1))
  call vert_AV_Q(ntry, ex4, ex7, wf4(:,5), n3(:,4), t3x4(:,:,3))
  call prop_A_Q(ntry, wf4(:,5), Q(:,72), MB, 1_intkind1, wf4(:,6), n2(2))
  call vert_QA_Z(gZd,ntry, ex3, wf4(:,6), wf8(:,2), n3(:,5), t3x8(:,:,2))
  call vert_VQ_A(ntry, ex7, ex1, wf4(:,7), n3(:,6), t3x4(:,:,4))
  call vert_QA_Z(gZd,ntry, ex3, ex4, wf4(:,8), n3(:,7), t3x4(:,:,5))
  call prop_Q_A(ntry, wf4(:,7), Q(:,65), ZERO, 0_intkind1, wf4(:,9), n2(3))
  call vert_QA_Z(gZd,ntry, wf4(:,9), ex2, wf8(:,3), n3(:,8), t3x8(:,:,3))
  call vert_SSV_V(ntry, ex5, ex6, wf4(:,8), wf4(:,10), n4(:,2), t4x4(:,:,2))
  call vert_AV_Q(ntry, ex2, ex7, wf4(:,11), n3(:,9), t3x4(:,:,6))
  call prop_A_Q(ntry, wf4(:,11), Q(:,66), ZERO, 0_intkind1, wf4(:,12), n2(4))
  call vert_QA_Z(gZd,ntry, ex1, wf4(:,12), wf8(:,4), n3(:,10), t3x8(:,:,4))
  call vert_QA_V(ntry, ex1, ex2, wf4(:,13), n3(:,11), t3x4(:,:,7))
  call vert_QS_A(gH,ntry, ex3, ex5, wf2(:,1), n3(:,12), t3x2(:,:,1))
  call vert_SA_Q(gH,ntry, ex6, ex4, wf2(:,2), n3(:,13), t3x2(:,:,2))
  call prop_Q_A(ntry, wf2(:,1), Q(:,20), MB, 1_intkind1, wf2(:,3), n2(5))
  call prop_A_Q(ntry, wf2(:,2), Q(:,40), MB, 1_intkind1, wf2(:,4), n2(6))
  call vert_VQ_A(ntry, ex7, wf2(:,3), wf4(:,14), n3(:,14), t3x4(:,:,8))
  call vert_AV_Q(ntry, wf2(:,4), wf4(:,13), wf8(:,5), n3(:,15), t3x8(:,:,5))
  call prop_Q_A(ntry, wf4(:,14), Q(:,84), MB, 1_intkind1, wf4(:,15), n2(7))
  call vert_AZ_Q(gZd,ntry, wf2(:,4), wf4(:,1), wf8(:,6), n3(:,16), t3x8(:,:,6))
  call vert_AV_Q(ntry, wf2(:,4), ex7, wf4(:,16), n3(:,17), t3x4(:,:,9))
  call vert_VQ_A(ntry, wf4(:,13), wf2(:,3), wf8(:,7), n3(:,18), t3x8(:,:,7))
  call prop_A_Q(ntry, wf4(:,16), Q(:,104), MB, 1_intkind1, wf4(:,17), n2(8))
  call vert_ZQ_A(gZd,ntry, wf4(:,1), wf2(:,3), wf8(:,8), n3(:,19), t3x8(:,:,8))
  call vert_TV_S(ntry, ex6, Q(:,32), wf4(:,1), Q(:,3), wf4(:,18), n3(:,20), t3x4(:,:,10))
  call vert_AQ_S(gX,ntry, wf4(:,6), wf2(:,3), wf8(:,9), n3(:,21), t3x8(:,:,9))
  call vert_SV_V(ntry, ex6, wf4(:,1), wf4(:,19), n3(:,22), t3x4(:,:,11))
  call vert_QA_Z(gZd,ntry, wf2(:,3), wf4(:,6), wf8(:,10), n3(:,23), t3x8(:,:,10))
  call vert_QS_A(gH,ntry, wf2(:,3), ex6, wf2(:,5), n3(:,24), t3x2(:,:,3))
  call vert_AV_Q(ntry, wf4(:,6), wf4(:,13), wf16(:,1), n3(:,25), t3x16(:,:,1))
  call prop_Q_A(ntry, wf2(:,5), Q(:,52), MB, 1_intkind1, wf2(:,6), n2(9))
  call vert_AZ_Q(gZd,ntry, wf4(:,6), wf4(:,1), wf16(:,2), n3(:,26), t3x16(:,:,2))
  call vert_SA_Q(gH,ntry, ex6, wf4(:,6), wf4(:,20), n3(:,27), t3x4(:,:,12))
  call prop_A_Q(ntry, wf4(:,20), Q(:,104), MB, 1_intkind1, wf4(:,21), n2(10))
  call vert_AV_Q(ntry, ex4, wf4(:,13), wf8(:,11), n3(:,28), t3x8(:,:,11))
  call prop_A_Q(ntry, wf8(:,11), Q(:,11), MB, 1_intkind1, wf8(:,12), n2(11))
  call vert_AV_Q(ntry, wf8(:,12), ex7, wf16(:,3), n3(:,29), t3x16(:,:,3))
  call vert_AZ_Q(gZd,ntry, ex4, wf4(:,1), wf8(:,13), n3(:,30), t3x8(:,:,12))
  call prop_A_Q(ntry, wf8(:,13), Q(:,11), MB, 1_intkind1, wf8(:,14), n2(12))
  call vert_AV_Q(ntry, wf8(:,14), ex7, wf16(:,4), n3(:,31), t3x16(:,:,4))
  call vert_SA_Q(gH,ntry, ex6, wf8(:,12), wf8(:,15), n3(:,32), t3x8(:,:,13))
  call vert_SA_Q(gH,ntry, ex6, wf8(:,14), wf8(:,16), n3(:,33), t3x8(:,:,14))
  call vert_SA_Q(gX,ntry, wf4(:,18), ex4, wf8(:,17), n3(:,34), t3x8(:,:,15))
  call vert_AZ_Q(gZd,ntry, ex4, wf4(:,19), wf8(:,18), n3(:,35), t3x8(:,:,16))
  call vert_QS_A(gH,ntry, ex3, ex6, wf2(:,7), n3(:,36), t3x2(:,:,4))
  call vert_SA_Q(gH,ntry, ex5, ex4, wf2(:,8), n3(:,37), t3x2(:,:,5))
  call prop_Q_A(ntry, wf2(:,7), Q(:,36), MB, 1_intkind1, wf2(:,9), n2(13))
  call prop_A_Q(ntry, wf2(:,8), Q(:,24), MB, 1_intkind1, wf2(:,10), n2(14))
  call vert_VQ_A(ntry, ex7, wf2(:,9), wf4(:,22), n3(:,38), t3x4(:,:,13))
  call vert_AV_Q(ntry, wf2(:,10), wf4(:,13), wf8(:,19), n3(:,39), t3x8(:,:,17))
  call prop_Q_A(ntry, wf4(:,22), Q(:,100), MB, 1_intkind1, wf4(:,23), n2(15))
  call vert_AZ_Q(gZd,ntry, wf2(:,10), wf4(:,1), wf8(:,20), n3(:,40), t3x8(:,:,18))
  call vert_AV_Q(ntry, wf2(:,10), ex7, wf4(:,24), n3(:,41), t3x4(:,:,14))
  call vert_VQ_A(ntry, wf4(:,13), wf2(:,9), wf8(:,21), n3(:,42), t3x8(:,:,19))
  call prop_A_Q(ntry, wf4(:,24), Q(:,88), MB, 1_intkind1, wf4(:,25), n2(16))
  call vert_ZQ_A(gZd,ntry, wf4(:,1), wf2(:,9), wf8(:,22), n3(:,43), t3x8(:,:,20))
  call vert_AQ_S(gX,ntry, wf2(:,10), wf4(:,3), wf8(:,23), n3(:,44), t3x8(:,:,21))
  call vert_QA_Z(gZd,ntry, wf4(:,3), wf2(:,10), wf8(:,24), n3(:,45), t3x8(:,:,22))
  call vert_QS_A(gH,ntry, wf4(:,3), ex6, wf4(:,26), n3(:,46), t3x4(:,:,15))
  call prop_Q_A(ntry, wf4(:,26), Q(:,100), MB, 1_intkind1, wf4(:,27), n2(17))
  call vert_SA_Q(gH,ntry, ex6, wf2(:,10), wf2(:,11), n3(:,47), t3x2(:,:,6))
  call vert_VQ_A(ntry, wf4(:,13), wf4(:,3), wf16(:,5), n3(:,48), t3x16(:,:,5))
  call prop_A_Q(ntry, wf2(:,11), Q(:,56), MB, 1_intkind1, wf2(:,12), n2(18))
  call vert_ZQ_A(gZd,ntry, wf4(:,1), wf4(:,3), wf16(:,6), n3(:,49), t3x16(:,:,6))
  call vert_VQ_A(ntry, wf4(:,13), ex3, wf8(:,25), n3(:,50), t3x8(:,:,23))
  call prop_Q_A(ntry, wf8(:,25), Q(:,7), MB, 1_intkind1, wf8(:,26), n2(19))
  call vert_QS_A(gH,ntry, wf8(:,26), ex6, wf8(:,27), n3(:,51), t3x8(:,:,24))
  call vert_ZQ_A(gZd,ntry, wf4(:,1), ex3, wf8(:,28), n3(:,52), t3x8(:,:,25))
  call prop_Q_A(ntry, wf8(:,28), Q(:,7), MB, 1_intkind1, wf8(:,29), n2(20))
  call vert_QS_A(gH,ntry, wf8(:,29), ex6, wf8(:,30), n3(:,53), t3x8(:,:,26))
  call vert_VQ_A(ntry, ex7, wf8(:,26), wf16(:,7), n3(:,54), t3x16(:,:,7))
  call vert_VQ_A(ntry, ex7, wf8(:,29), wf16(:,8), n3(:,55), t3x16(:,:,8))
  call vert_QS_A(gX,ntry, ex3, wf4(:,18), wf8(:,31), n3(:,56), t3x8(:,:,27))
  call vert_ZQ_A(gZd,ntry, wf4(:,19), ex3, wf8(:,32), n3(:,57), t3x8(:,:,28))
  call vert_TV_S(ntry, ex5, Q(:,16), wf4(:,1), Q(:,3), wf4(:,28), n3(:,58), t3x4(:,:,16))
  call vert_AQ_S(gX,ntry, wf4(:,6), wf2(:,9), wf8(:,33), n3(:,59), t3x8(:,:,29))
  call vert_SV_V(ntry, ex5, wf4(:,1), wf4(:,29), n3(:,60), t3x4(:,:,17))
  call vert_QA_Z(gZd,ntry, wf2(:,9), wf4(:,6), wf8(:,34), n3(:,61), t3x8(:,:,30))
  call vert_QS_A(gH,ntry, wf2(:,9), ex5, wf2(:,13), n3(:,62), t3x2(:,:,7))
  call prop_Q_A(ntry, wf2(:,13), Q(:,52), MB, 1_intkind1, wf2(:,14), n2(21))
  call vert_SA_Q(gH,ntry, ex5, wf4(:,6), wf4(:,30), n3(:,63), t3x4(:,:,18))
  call prop_A_Q(ntry, wf4(:,30), Q(:,88), MB, 1_intkind1, wf4(:,31), n2(22))
  call vert_SA_Q(gH,ntry, ex5, wf8(:,12), wf8(:,35), n3(:,64), t3x8(:,:,31))
  call vert_SA_Q(gH,ntry, ex5, wf8(:,14), wf8(:,36), n3(:,65), t3x8(:,:,32))
  call vert_SA_Q(gX,ntry, wf4(:,28), ex4, wf8(:,37), n3(:,66), t3x8(:,:,33))
  call vert_AZ_Q(gZd,ntry, ex4, wf4(:,29), wf8(:,38), n3(:,67), t3x8(:,:,34))
  call vert_AQ_S(gX,ntry, wf2(:,4), wf4(:,3), wf8(:,39), n3(:,68), t3x8(:,:,35))
  call vert_QA_Z(gZd,ntry, wf4(:,3), wf2(:,4), wf8(:,40), n3(:,69), t3x8(:,:,36))
  call vert_QS_A(gH,ntry, wf4(:,3), ex5, wf4(:,32), n3(:,70), t3x4(:,:,19))
  call prop_Q_A(ntry, wf4(:,32), Q(:,84), MB, 1_intkind1, wf4(:,33), n2(23))
  call vert_SA_Q(gH,ntry, ex5, wf2(:,4), wf2(:,15), n3(:,71), t3x2(:,:,8))
  call prop_A_Q(ntry, wf2(:,15), Q(:,56), MB, 1_intkind1, wf2(:,16), n2(24))
  call vert_QS_A(gH,ntry, wf8(:,26), ex5, wf8(:,41), n3(:,72), t3x8(:,:,37))
  call vert_QS_A(gH,ntry, wf8(:,29), ex5, wf8(:,42), n3(:,73), t3x8(:,:,38))
  call vert_QS_A(gX,ntry, ex3, wf4(:,28), wf8(:,43), n3(:,74), t3x8(:,:,39))
  call vert_ZQ_A(gZd,ntry, wf4(:,29), ex3, wf8(:,44), n3(:,75), t3x8(:,:,40))
  call vert_SS_S(ntry, ex5, ex6, wf1(:,1), n3(:,76), t3x1(:,:,1))
  call vert_QS_A(gH,ntry, wf4(:,3), wf1(:,1), wf4(:,34), n3(:,77), t3x4(:,:,20))
  call vert_AQ_S(gX,ntry, ex4, wf4(:,3), wf8(:,45), n3(:,78), t3x8(:,:,41))
  call vert_TV_S(ntry, wf1(:,1), Q(:,48), wf4(:,1), Q(:,3), wf4(:,35), n3(:,79), t3x4(:,:,21))
  call vert_SV_V(ntry, wf1(:,1), wf4(:,1), wf4(:,36), n3(:,80), t3x4(:,:,22))
  call vert_SA_Q(gH,ntry, wf1(:,1), ex4, wf2(:,17), n3(:,81), t3x2(:,:,9))
  call prop_A_Q(ntry, wf2(:,17), Q(:,56), MB, 1_intkind1, wf2(:,18), n2(25))
  call vert_SA_Q(gH,ntry, wf1(:,1), wf4(:,6), wf4(:,37), n3(:,82), t3x4(:,:,23))
  call vert_AQ_S(gX,ntry, wf4(:,6), ex3, wf8(:,46), n3(:,83), t3x8(:,:,42))
  call vert_QS_A(gH,ntry, ex3, wf1(:,1), wf2(:,19), n3(:,84), t3x2(:,:,10))
  call prop_Q_A(ntry, wf2(:,19), Q(:,52), MB, 1_intkind1, wf2(:,20), n2(26))
  call vert_VQ_A(ntry, ex7, wf2(:,20), wf4(:,38), n3(:,85), t3x4(:,:,24))
  call vert_SS_S(ntry, ex6, wf8(:,45), wf8(:,47), n3(:,86), t3x8(:,:,43))
  call vert_ST_V(ntry, wf8(:,45), Q(:,76), ex6, Q(:,32), wf8(:,48), n3(:,87), t3x8(:,:,44))
  call vert_TV_S(ntry, ex6, Q(:,32), wf8(:,1), Q(:,76), wf8(:,49), n3(:,88), t3x8(:,:,45))
  call vert_SV_V(ntry, ex6, wf8(:,1), wf8(:,50), n3(:,89), t3x8(:,:,46))
  call vert_SS_S(ntry, ex5, wf8(:,45), wf8(:,51), n3(:,90), t3x8(:,:,47))
  call vert_ST_V(ntry, wf8(:,45), Q(:,76), ex5, Q(:,16), wf8(:,52), n3(:,91), t3x8(:,:,48))
  call vert_TV_S(ntry, ex5, Q(:,16), wf8(:,1), Q(:,76), wf8(:,53), n3(:,92), t3x8(:,:,49))
  call vert_SV_V(ntry, ex5, wf8(:,1), wf8(:,54), n3(:,93), t3x8(:,:,50))
  call vert_AQ_S(gX,ntry, ex4, wf4(:,33), wf8(:,55), n3(:,94), t3x8(:,:,51))
  call vert_QA_Z(gZd,ntry, wf4(:,33), ex4, wf8(:,56), n3(:,95), t3x8(:,:,52))
  call vert_SS_S(ntry, ex6, wf8(:,46), wf8(:,57), n3(:,96), t3x8(:,:,53))
  call vert_ST_V(ntry, wf8(:,46), Q(:,76), ex6, Q(:,32), wf8(:,58), n3(:,97), t3x8(:,:,54))
  call vert_TV_S(ntry, ex6, Q(:,32), wf8(:,2), Q(:,76), wf8(:,59), n3(:,98), t3x8(:,:,55))
  call vert_SV_V(ntry, ex6, wf8(:,2), wf8(:,60), n3(:,99), t3x8(:,:,56))
  call vert_SS_S(ntry, ex5, wf8(:,46), wf8(:,61), n3(:,100), t3x8(:,:,57))
  call vert_ST_V(ntry, wf8(:,46), Q(:,76), ex5, Q(:,16), wf8(:,62), n3(:,101), t3x8(:,:,58))
  call vert_TV_S(ntry, ex5, Q(:,16), wf8(:,2), Q(:,76), wf8(:,63), n3(:,102), t3x8(:,:,59))
  call vert_SV_V(ntry, ex5, wf8(:,2), wf8(:,64), n3(:,103), t3x8(:,:,60))
  call vert_AQ_S(gX,ntry, wf4(:,31), ex3, wf8(:,65), n3(:,104), t3x8(:,:,61))
  call vert_QA_Z(gZd,ntry, ex3, wf4(:,31), wf8(:,66), n3(:,105), t3x8(:,:,62))
  call vert_AQ_S(gX,ntry, ex4, ex3, wf4(:,39), n3(:,106), t3x4(:,:,25))
  call vert_ST_V(ntry, wf4(:,39), Q(:,12), wf1(:,1), Q(:,48), wf4(:,40), n3(:,107), t3x4(:,:,26))
  call vert_SV_V(ntry, wf1(:,1), wf4(:,8), wf4(:,41), n3(:,108), t3x4(:,:,27))
  call vert_SS_S(ntry, wf4(:,39), ex6, wf4(:,42), n3(:,109), t3x4(:,:,28))
  call vert_TV_S(ntry, ex5, Q(:,16), wf8(:,3), Q(:,67), wf8(:,67), n3(:,110), t3x8(:,:,63))
  call vert_ST_V(ntry, wf4(:,39), Q(:,12), ex6, Q(:,32), wf4(:,43), n3(:,111), t3x4(:,:,29))
  call vert_SV_V(ntry, ex5, wf8(:,3), wf8(:,68), n3(:,112), t3x8(:,:,64))
  call vert_TV_S(ntry, ex6, Q(:,32), wf4(:,8), Q(:,12), wf4(:,44), n3(:,113), t3x4(:,:,30))
  call vert_SV_V(ntry, ex6, wf4(:,8), wf4(:,45), n3(:,114), t3x4(:,:,31))
  call vert_SS_S(ntry, wf4(:,39), ex5, wf4(:,46), n3(:,115), t3x4(:,:,32))
  call vert_TV_S(ntry, ex6, Q(:,32), wf8(:,3), Q(:,67), wf8(:,69), n3(:,116), t3x8(:,:,65))
  call vert_ST_V(ntry, wf4(:,39), Q(:,12), ex5, Q(:,16), wf4(:,47), n3(:,117), t3x4(:,:,33))
  call vert_SV_V(ntry, ex6, wf8(:,3), wf8(:,70), n3(:,118), t3x8(:,:,66))
  call vert_TV_S(ntry, ex5, Q(:,16), wf4(:,8), Q(:,12), wf4(:,48), n3(:,119), t3x4(:,:,34))
  call vert_SV_V(ntry, ex5, wf4(:,8), wf4(:,49), n3(:,120), t3x4(:,:,35))
  call vert_TV_S(ntry, ex5, Q(:,16), wf8(:,4), Q(:,67), wf8(:,71), n3(:,121), t3x8(:,:,67))
  call vert_SV_V(ntry, ex5, wf8(:,4), wf8(:,72), n3(:,122), t3x8(:,:,68))
  call vert_TV_S(ntry, ex6, Q(:,32), wf8(:,4), Q(:,67), wf8(:,73), n3(:,123), t3x8(:,:,69))
  call vert_SV_V(ntry, ex6, wf8(:,4), wf8(:,74), n3(:,124), t3x8(:,:,70))
  call vert_QA_V(ntry, wf4(:,9), ex2, wf8(:,75), n3(:,125), t3x8(:,:,71))
  call vert_QA_V(ntry, wf2(:,3), wf2(:,4), wf4(:,50), n3(:,126), t3x4(:,:,36))
  call vert_QA_Z(gZd,ntry, wf2(:,3), wf2(:,4), wf4(:,51), n3(:,127), t3x4(:,:,37))
  call vert_QA_V(ntry, ex1, wf4(:,12), wf8(:,76), n3(:,128), t3x8(:,:,72))
  call vert_AV_Q(ntry, ex4, wf8(:,75), wf16(:,9), n3(:,129), t3x16(:,:,9))
  call vert_AZ_Q(gZd,ntry, ex4, wf8(:,3), wf16(:,10), n3(:,130), t3x16(:,:,10))
  call vert_AQ_S(gX,ntry, ex4, wf2(:,3), wf4(:,52), n3(:,131), t3x4(:,:,38))
  call vert_QA_Z(gZd,ntry, wf2(:,3), ex4, wf4(:,53), n3(:,132), t3x4(:,:,39))
  call vert_AV_Q(ntry, ex4, wf8(:,76), wf16(:,11), n3(:,133), t3x16(:,:,11))
  call vert_AZ_Q(gZd,ntry, ex4, wf8(:,4), wf16(:,12), n3(:,134), t3x16(:,:,12))
  call vert_QA_V(ntry, wf2(:,9), wf2(:,10), wf4(:,54), n3(:,135), t3x4(:,:,40))
  call vert_QA_Z(gZd,ntry, wf2(:,9), wf2(:,10), wf4(:,55), n3(:,136), t3x4(:,:,41))
  call vert_VQ_A(ntry, wf8(:,75), ex3, wf16(:,13), n3(:,137), t3x16(:,:,13))
  call vert_ZQ_A(gZd,ntry, wf8(:,3), ex3, wf16(:,14), n3(:,138), t3x16(:,:,14))
  call vert_AQ_S(gX,ntry, wf2(:,10), ex3, wf4(:,56), n3(:,139), t3x4(:,:,42))
  call vert_QA_Z(gZd,ntry, ex3, wf2(:,10), wf4(:,57), n3(:,140), t3x4(:,:,43))
  call vert_VQ_A(ntry, wf8(:,76), ex3, wf16(:,15), n3(:,141), t3x16(:,:,15))
  call vert_ZQ_A(gZd,ntry, wf8(:,4), ex3, wf16(:,16), n3(:,142), t3x16(:,:,16))
  call vert_AQ_S(gX,ntry, ex4, wf2(:,9), wf4(:,58), n3(:,143), t3x4(:,:,44))
  call vert_QA_Z(gZd,ntry, wf2(:,9), ex4, wf4(:,59), n3(:,144), t3x4(:,:,45))
  call vert_AQ_S(gX,ntry, wf2(:,4), ex3, wf4(:,60), n3(:,145), t3x4(:,:,46))
  call vert_QA_Z(gZd,ntry, ex3, wf2(:,4), wf4(:,61), n3(:,146), t3x4(:,:,47))


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

    M2munu = M2munu / average_factor_pphhjj_ddxbbxhhg_1
  end if

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2add = M2 / average_factor_pphhjj_ddxbbxhhg_1

  do k = 0, 30-1
    M2(k) = M2add(extcomb_perm_pphhjj_ddxbbxhhg_1(k))
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

    call cont_VV(nsync, wf8(:,1), wf4(:,4), A(:,1), n3(:,147), t3x32(:,:,1), nhel, den(5))
    call cont_VV(nsync, wf4(:,4), wf8(:,2), A(:,2), n3(:,148), t3x32(:,:,2), nhel, den(8))
    call cont_VV(nsync, wf8(:,3), wf4(:,10), A(:,3), n3(:,149), t3x32(:,:,3), nhel, den(13))
    call cont_VV(nsync, wf4(:,10), wf8(:,4), A(:,4), n3(:,150), t3x32(:,:,4), nhel, den(16))
    call cont_QA(nsync, wf8(:,5), wf4(:,15), A(:,5), n3(:,151), t3x32(:,:,5), nhel, den(23))
    call cont_QA(nsync, wf4(:,15), wf8(:,6), A(:,6), n3(:,152), t3x32(:,:,6), nhel, den(25))
    call cont_QA(nsync, wf8(:,7), wf4(:,17), A(:,7), n3(:,153), t3x32(:,:,7), nhel, den(29))
    call cont_QA(nsync, wf4(:,17), wf8(:,8), A(:,8), n3(:,154), t3x32(:,:,8), nhel, den(31))
    call cont_SS(nsync, wf4(:,18), wf8(:,9), A(:,9), n3(:,155), t3x32(:,:,9), nhel, den(35))
    call cont_VV(nsync, wf4(:,19), wf8(:,10), A(:,10), n3(:,156), t3x32(:,:,10), nhel, den(35))
    call cont_QA(nsync, wf16(:,1), wf2(:,6), A(:,11), n3(:,157), t3x32(:,:,11), nhel, den(39))
    call cont_QA(nsync, wf2(:,6), wf16(:,2), A(:,12), n3(:,158), t3x32(:,:,12), nhel, den(41))
    call cont_QA(nsync, wf8(:,7), wf4(:,21), A(:,13), n3(:,159), t3x32(:,:,13), nhel, den(43))
    call cont_QA(nsync, wf8(:,8), wf4(:,21), A(:,14), n3(:,160), t3x32(:,:,14), nhel, den(44))
    call cont_QA(nsync, wf2(:,6), wf16(:,3), A(:,15), n3(:,161), t3x32(:,:,15), nhel, den(47))
    call cont_QA(nsync, wf2(:,6), wf16(:,4), A(:,16), n3(:,162), t3x32(:,:,16), nhel, den(49))
    call cont_QA(nsync, wf4(:,15), wf8(:,15), A(:,17), n3(:,163), t3x32(:,:,17), nhel, den(50))
    call cont_QA(nsync, wf4(:,15), wf8(:,16), A(:,18), n3(:,164), t3x32(:,:,18), nhel, den(51))
    call cont_QA(nsync, wf4(:,15), wf8(:,17), A(:,19), n3(:,165), t3x32(:,:,19), nhel, den(52))
    call cont_QA(nsync, wf4(:,15), wf8(:,18), A(:,20), n3(:,166), t3x32(:,:,20), nhel, den(52))
    call cont_QA(nsync, wf8(:,19), wf4(:,23), A(:,21), n3(:,167), t3x32(:,:,21), nhel, den(58))
    call cont_QA(nsync, wf4(:,23), wf8(:,20), A(:,22), n3(:,168), t3x32(:,:,22), nhel, den(60))
    call cont_QA(nsync, wf8(:,21), wf4(:,25), A(:,23), n3(:,169), t3x32(:,:,23), nhel, den(64))
    call cont_QA(nsync, wf4(:,25), wf8(:,22), A(:,24), n3(:,170), t3x32(:,:,24), nhel, den(66))
    call cont_SS(nsync, wf4(:,18), wf8(:,23), A(:,25), n3(:,171), t3x32(:,:,25), nhel, den(68))
    call cont_VV(nsync, wf4(:,19), wf8(:,24), A(:,26), n3(:,172), t3x32(:,:,26), nhel, den(68))
    call cont_QA(nsync, wf8(:,19), wf4(:,27), A(:,27), n3(:,173), t3x32(:,:,27), nhel, den(70))
    call cont_QA(nsync, wf8(:,20), wf4(:,27), A(:,28), n3(:,174), t3x32(:,:,28), nhel, den(71))
    call cont_QA(nsync, wf16(:,5), wf2(:,12), A(:,29), n3(:,175), t3x32(:,:,29), nhel, den(75))
    call cont_QA(nsync, wf2(:,12), wf16(:,6), A(:,30), n3(:,176), t3x32(:,:,30), nhel, den(77))
    call cont_QA(nsync, wf4(:,25), wf8(:,27), A(:,31), n3(:,177), t3x32(:,:,31), nhel, den(80))
    call cont_QA(nsync, wf4(:,25), wf8(:,30), A(:,32), n3(:,178), t3x32(:,:,32), nhel, den(82))
    call cont_QA(nsync, wf2(:,12), wf16(:,7), A(:,33), n3(:,179), t3x32(:,:,33), nhel, den(83))
    call cont_QA(nsync, wf2(:,12), wf16(:,8), A(:,34), n3(:,180), t3x32(:,:,34), nhel, den(84))
    call cont_QA(nsync, wf4(:,25), wf8(:,31), A(:,35), n3(:,181), t3x32(:,:,35), nhel, den(85))
    call cont_QA(nsync, wf4(:,25), wf8(:,32), A(:,36), n3(:,182), t3x32(:,:,36), nhel, den(85))
    call cont_SS(nsync, wf4(:,28), wf8(:,33), A(:,37), n3(:,183), t3x32(:,:,37), nhel, den(89))
    call cont_VV(nsync, wf4(:,29), wf8(:,34), A(:,38), n3(:,184), t3x32(:,:,38), nhel, den(89))
    call cont_QA(nsync, wf16(:,1), wf2(:,14), A(:,39), n3(:,185), t3x32(:,:,39), nhel, den(91))
    call cont_QA(nsync, wf16(:,2), wf2(:,14), A(:,40), n3(:,186), t3x32(:,:,40), nhel, den(92))
    call cont_QA(nsync, wf8(:,21), wf4(:,31), A(:,41), n3(:,187), t3x32(:,:,41), nhel, den(94))
    call cont_QA(nsync, wf8(:,22), wf4(:,31), A(:,42), n3(:,188), t3x32(:,:,42), nhel, den(95))
    call cont_QA(nsync, wf16(:,3), wf2(:,14), A(:,43), n3(:,189), t3x32(:,:,43), nhel, den(96))
    call cont_QA(nsync, wf16(:,4), wf2(:,14), A(:,44), n3(:,190), t3x32(:,:,44), nhel, den(97))
    call cont_QA(nsync, wf4(:,23), wf8(:,35), A(:,45), n3(:,191), t3x32(:,:,45), nhel, den(98))
    call cont_QA(nsync, wf4(:,23), wf8(:,36), A(:,46), n3(:,192), t3x32(:,:,46), nhel, den(99))
    call cont_QA(nsync, wf4(:,23), wf8(:,37), A(:,47), n3(:,193), t3x32(:,:,47), nhel, den(100))
    call cont_QA(nsync, wf4(:,23), wf8(:,38), A(:,48), n3(:,194), t3x32(:,:,48), nhel, den(100))
    call cont_SS(nsync, wf4(:,28), wf8(:,39), A(:,49), n3(:,195), t3x32(:,:,49), nhel, den(102))
    call cont_VV(nsync, wf4(:,29), wf8(:,40), A(:,50), n3(:,196), t3x32(:,:,50), nhel, den(102))
    call cont_QA(nsync, wf8(:,5), wf4(:,33), A(:,51), n3(:,197), t3x32(:,:,51), nhel, den(104))
    call cont_QA(nsync, wf8(:,6), wf4(:,33), A(:,52), n3(:,198), t3x32(:,:,52), nhel, den(105))
    call cont_QA(nsync, wf16(:,5), wf2(:,16), A(:,53), n3(:,199), t3x32(:,:,53), nhel, den(107))
    call cont_QA(nsync, wf16(:,6), wf2(:,16), A(:,54), n3(:,200), t3x32(:,:,54), nhel, den(108))
    call cont_QA(nsync, wf4(:,17), wf8(:,41), A(:,55), n3(:,201), t3x32(:,:,55), nhel, den(109))
    call cont_QA(nsync, wf4(:,17), wf8(:,42), A(:,56), n3(:,202), t3x32(:,:,56), nhel, den(110))
    call cont_QA(nsync, wf16(:,7), wf2(:,16), A(:,57), n3(:,203), t3x32(:,:,57), nhel, den(111))
    call cont_QA(nsync, wf16(:,8), wf2(:,16), A(:,58), n3(:,204), t3x32(:,:,58), nhel, den(112))
    call cont_QA(nsync, wf4(:,17), wf8(:,43), A(:,59), n3(:,205), t3x32(:,:,59), nhel, den(113))
    call cont_QA(nsync, wf4(:,17), wf8(:,44), A(:,60), n3(:,206), t3x32(:,:,60), nhel, den(113))
    call cont_QA(nsync, wf8(:,12), wf4(:,34), A(:,61), n3(:,207), t3x32(:,:,61), nhel, den(116))
    call cont_QA(nsync, wf8(:,14), wf4(:,34), A(:,62), n3(:,208), t3x32(:,:,62), nhel, den(117))
    call cont_SS(nsync, wf8(:,45), wf4(:,35), A(:,63), n3(:,209), t3x32(:,:,63), nhel, den(119))
    call cont_VV(nsync, wf8(:,1), wf4(:,36), A(:,64), n3(:,210), t3x32(:,:,64), nhel, den(119))
    call cont_QA(nsync, wf16(:,5), wf2(:,18), A(:,65), n3(:,211), t3x32(:,:,65), nhel, den(121))
    call cont_QA(nsync, wf16(:,6), wf2(:,18), A(:,66), n3(:,212), t3x32(:,:,66), nhel, den(122))
    call cont_QA(nsync, wf8(:,26), wf4(:,37), A(:,67), n3(:,213), t3x32(:,:,67), nhel, den(124))
    call cont_QA(nsync, wf8(:,29), wf4(:,37), A(:,68), n3(:,214), t3x32(:,:,68), nhel, den(125))
    call cont_SS(nsync, wf4(:,35), wf8(:,46), A(:,69), n3(:,215), t3x32(:,:,69), nhel, den(126))
    call cont_VV(nsync, wf8(:,2), wf4(:,36), A(:,70), n3(:,216), t3x32(:,:,70), nhel, den(126))
    call cont_QA(nsync, wf16(:,1), wf2(:,20), A(:,71), n3(:,217), t3x32(:,:,71), nhel, den(128))
    call cont_QA(nsync, wf16(:,2), wf2(:,20), A(:,72), n3(:,218), t3x32(:,:,72), nhel, den(129))
    call cont_QA(nsync, wf16(:,7), wf2(:,18), A(:,73), n3(:,219), t3x32(:,:,73), nhel, den(130))
    call cont_QA(nsync, wf16(:,8), wf2(:,18), A(:,74), n3(:,220), t3x32(:,:,74), nhel, den(131))
    call cont_QA(nsync, wf8(:,12), wf4(:,38), A(:,75), n3(:,221), t3x32(:,:,75), nhel, den(132))
    call cont_QA(nsync, wf8(:,14), wf4(:,38), A(:,76), n3(:,222), t3x32(:,:,76), nhel, den(133))
    call cont_QA(nsync, wf8(:,15), wf4(:,33), A(:,77), n3(:,223), t3x32(:,:,77), nhel, den(134))
    call cont_QA(nsync, wf8(:,16), wf4(:,33), A(:,78), n3(:,224), t3x32(:,:,78), nhel, den(135))
    call cont_QA(nsync, wf4(:,27), wf8(:,35), A(:,79), n3(:,225), t3x32(:,:,79), nhel, den(136))
    call cont_QA(nsync, wf4(:,27), wf8(:,36), A(:,80), n3(:,226), t3x32(:,:,80), nhel, den(137))
    call cont_SS(nsync, wf4(:,28), wf8(:,47), A(:,81), n3(:,227), t3x32(:,:,81), nhel, den(138))
    call cont_VV(nsync, wf4(:,29), wf8(:,48), A(:,82), n3(:,228), t3x32(:,:,82), nhel, den(138))
    call cont_SS(nsync, wf4(:,28), wf8(:,49), A(:,83), n3(:,229), t3x32(:,:,83), nhel, den(138))
    call cont_VV(nsync, wf4(:,29), wf8(:,50), A(:,84), n3(:,230), t3x32(:,:,84), nhel, den(138))
    call cont_QA(nsync, wf4(:,27), wf8(:,37), A(:,85), n3(:,231), t3x32(:,:,85), nhel, den(139))
    call cont_QA(nsync, wf4(:,27), wf8(:,38), A(:,86), n3(:,232), t3x32(:,:,86), nhel, den(139))
    call cont_SS(nsync, wf4(:,18), wf8(:,51), A(:,87), n3(:,233), t3x32(:,:,87), nhel, den(140))
    call cont_VV(nsync, wf4(:,19), wf8(:,52), A(:,88), n3(:,234), t3x32(:,:,88), nhel, den(140))
    call cont_SS(nsync, wf4(:,18), wf8(:,53), A(:,89), n3(:,235), t3x32(:,:,89), nhel, den(140))
    call cont_VV(nsync, wf4(:,19), wf8(:,54), A(:,90), n3(:,236), t3x32(:,:,90), nhel, den(140))
    call cont_SS(nsync, wf4(:,18), wf8(:,55), A(:,91), n3(:,237), t3x32(:,:,91), nhel, den(141))
    call cont_VV(nsync, wf4(:,19), wf8(:,56), A(:,92), n3(:,238), t3x32(:,:,92), nhel, den(141))
    call cont_QA(nsync, wf4(:,21), wf8(:,41), A(:,93), n3(:,239), t3x32(:,:,93), nhel, den(142))
    call cont_QA(nsync, wf4(:,21), wf8(:,42), A(:,94), n3(:,240), t3x32(:,:,94), nhel, den(143))
    call cont_QA(nsync, wf8(:,27), wf4(:,31), A(:,95), n3(:,241), t3x32(:,:,95), nhel, den(144))
    call cont_QA(nsync, wf8(:,30), wf4(:,31), A(:,96), n3(:,242), t3x32(:,:,96), nhel, den(145))
    call cont_SS(nsync, wf4(:,28), wf8(:,57), A(:,97), n3(:,243), t3x32(:,:,97), nhel, den(146))
    call cont_VV(nsync, wf4(:,29), wf8(:,58), A(:,98), n3(:,244), t3x32(:,:,98), nhel, den(146))
    call cont_SS(nsync, wf4(:,28), wf8(:,59), A(:,99), n3(:,245), t3x32(:,:,99), nhel, den(146))
    call cont_VV(nsync, wf4(:,29), wf8(:,60), A(:,100), n3(:,246), t3x32(:,:,100), nhel, den(146))
    call cont_QA(nsync, wf4(:,21), wf8(:,43), A(:,101), n3(:,247), t3x32(:,:,101), nhel, den(147))
    call cont_QA(nsync, wf4(:,21), wf8(:,44), A(:,102), n3(:,248), t3x32(:,:,102), nhel, den(147))
    call cont_SS(nsync, wf4(:,18), wf8(:,61), A(:,103), n3(:,249), t3x32(:,:,103), nhel, den(148))
    call cont_VV(nsync, wf4(:,19), wf8(:,62), A(:,104), n3(:,250), t3x32(:,:,104), nhel, den(148))
    call cont_SS(nsync, wf4(:,18), wf8(:,63), A(:,105), n3(:,251), t3x32(:,:,105), nhel, den(148))
    call cont_VV(nsync, wf4(:,19), wf8(:,64), A(:,106), n3(:,252), t3x32(:,:,106), nhel, den(148))
    call cont_SS(nsync, wf4(:,18), wf8(:,65), A(:,107), n3(:,253), t3x32(:,:,107), nhel, den(149))
    call cont_VV(nsync, wf4(:,19), wf8(:,66), A(:,108), n3(:,254), t3x32(:,:,108), nhel, den(149))
    call cont_VV(nsync, wf8(:,3), wf4(:,40), A(:,109), n3(:,255), t3x32(:,:,109), nhel, den(151))
    call cont_VV(nsync, wf8(:,3), wf4(:,41), A(:,110), n3(:,256), t3x32(:,:,110), nhel, den(151))
    call cont_VV(nsync, wf8(:,4), wf4(:,40), A(:,111), n3(:,257), t3x32(:,:,111), nhel, den(152))
    call cont_VV(nsync, wf8(:,4), wf4(:,41), A(:,112), n3(:,258), t3x32(:,:,112), nhel, den(152))
    call cont_SS(nsync, wf4(:,42), wf8(:,67), A(:,113), n3(:,259), t3x32(:,:,113), nhel, den(155))
    call cont_VV(nsync, wf4(:,43), wf8(:,68), A(:,114), n3(:,260), t3x32(:,:,114), nhel, den(155))
    call cont_SS(nsync, wf8(:,67), wf4(:,44), A(:,115), n3(:,261), t3x32(:,:,115), nhel, den(155))
    call cont_VV(nsync, wf8(:,68), wf4(:,45), A(:,116), n3(:,262), t3x32(:,:,116), nhel, den(155))
    call cont_SS(nsync, wf4(:,46), wf8(:,69), A(:,117), n3(:,263), t3x32(:,:,117), nhel, den(158))
    call cont_VV(nsync, wf4(:,47), wf8(:,70), A(:,118), n3(:,264), t3x32(:,:,118), nhel, den(158))
    call cont_SS(nsync, wf8(:,69), wf4(:,48), A(:,119), n3(:,265), t3x32(:,:,119), nhel, den(158))
    call cont_VV(nsync, wf8(:,70), wf4(:,49), A(:,120), n3(:,266), t3x32(:,:,120), nhel, den(158))
    call cont_SS(nsync, wf4(:,42), wf8(:,71), A(:,121), n3(:,267), t3x32(:,:,121), nhel, den(159))
    call cont_VV(nsync, wf4(:,43), wf8(:,72), A(:,122), n3(:,268), t3x32(:,:,122), nhel, den(159))
    call cont_SS(nsync, wf4(:,44), wf8(:,71), A(:,123), n3(:,269), t3x32(:,:,123), nhel, den(159))
    call cont_VV(nsync, wf4(:,45), wf8(:,72), A(:,124), n3(:,270), t3x32(:,:,124), nhel, den(159))
    call cont_SS(nsync, wf4(:,46), wf8(:,73), A(:,125), n3(:,271), t3x32(:,:,125), nhel, den(160))
    call cont_VV(nsync, wf4(:,47), wf8(:,74), A(:,126), n3(:,272), t3x32(:,:,126), nhel, den(160))
    call cont_SS(nsync, wf4(:,48), wf8(:,73), A(:,127), n3(:,273), t3x32(:,:,127), nhel, den(160))
    call cont_VV(nsync, wf4(:,49), wf8(:,74), A(:,128), n3(:,274), t3x32(:,:,128), nhel, den(160))
    call cont_VV(nsync, wf8(:,75), wf4(:,50), A(:,129), n3(:,275), t3x32(:,:,129), nhel, den(164))
    call cont_VV(nsync, wf8(:,3), wf4(:,51), A(:,130), n3(:,276), t3x32(:,:,130), nhel, den(165))
    call cont_VV(nsync, wf4(:,50), wf8(:,76), A(:,131), n3(:,277), t3x32(:,:,131), nhel, den(167))
    call cont_VV(nsync, wf8(:,4), wf4(:,51), A(:,132), n3(:,278), t3x32(:,:,132), nhel, den(168))
    call cont_QA(nsync, wf2(:,6), wf16(:,9), A(:,133), n3(:,279), t3x32(:,:,133), nhel, den(169))
    call cont_QA(nsync, wf2(:,6), wf16(:,10), A(:,134), n3(:,280), t3x32(:,:,134), nhel, den(170))
    call cont_SS(nsync, wf8(:,69), wf4(:,52), A(:,135), n3(:,281), t3x32(:,:,135), nhel, den(172))
    call cont_VV(nsync, wf8(:,70), wf4(:,53), A(:,136), n3(:,282), t3x32(:,:,136), nhel, den(172))
    call cont_QA(nsync, wf2(:,6), wf16(:,11), A(:,137), n3(:,283), t3x32(:,:,137), nhel, den(173))
    call cont_QA(nsync, wf2(:,6), wf16(:,12), A(:,138), n3(:,284), t3x32(:,:,138), nhel, den(174))
    call cont_SS(nsync, wf8(:,73), wf4(:,52), A(:,139), n3(:,285), t3x32(:,:,139), nhel, den(175))
    call cont_VV(nsync, wf8(:,74), wf4(:,53), A(:,140), n3(:,286), t3x32(:,:,140), nhel, den(175))
    call cont_VV(nsync, wf8(:,75), wf4(:,54), A(:,141), n3(:,287), t3x32(:,:,141), nhel, den(177))
    call cont_VV(nsync, wf8(:,3), wf4(:,55), A(:,142), n3(:,288), t3x32(:,:,142), nhel, den(178))
    call cont_VV(nsync, wf8(:,76), wf4(:,54), A(:,143), n3(:,289), t3x32(:,:,143), nhel, den(179))
    call cont_VV(nsync, wf8(:,4), wf4(:,55), A(:,144), n3(:,290), t3x32(:,:,144), nhel, den(180))
    call cont_QA(nsync, wf2(:,12), wf16(:,13), A(:,145), n3(:,291), t3x32(:,:,145), nhel, den(181))
    call cont_QA(nsync, wf2(:,12), wf16(:,14), A(:,146), n3(:,292), t3x32(:,:,146), nhel, den(182))
    call cont_SS(nsync, wf8(:,69), wf4(:,56), A(:,147), n3(:,293), t3x32(:,:,147), nhel, den(184))
    call cont_VV(nsync, wf8(:,70), wf4(:,57), A(:,148), n3(:,294), t3x32(:,:,148), nhel, den(184))
    call cont_QA(nsync, wf2(:,12), wf16(:,15), A(:,149), n3(:,295), t3x32(:,:,149), nhel, den(185))
    call cont_QA(nsync, wf2(:,12), wf16(:,16), A(:,150), n3(:,296), t3x32(:,:,150), nhel, den(186))
    call cont_SS(nsync, wf8(:,73), wf4(:,56), A(:,151), n3(:,297), t3x32(:,:,151), nhel, den(187))
    call cont_VV(nsync, wf8(:,74), wf4(:,57), A(:,152), n3(:,298), t3x32(:,:,152), nhel, den(187))
    call cont_QA(nsync, wf2(:,14), wf16(:,9), A(:,153), n3(:,299), t3x32(:,:,153), nhel, den(188))
    call cont_QA(nsync, wf2(:,14), wf16(:,10), A(:,154), n3(:,300), t3x32(:,:,154), nhel, den(189))
    call cont_SS(nsync, wf8(:,67), wf4(:,58), A(:,155), n3(:,301), t3x32(:,:,155), nhel, den(191))
    call cont_VV(nsync, wf8(:,68), wf4(:,59), A(:,156), n3(:,302), t3x32(:,:,156), nhel, den(191))
    call cont_QA(nsync, wf2(:,14), wf16(:,11), A(:,157), n3(:,303), t3x32(:,:,157), nhel, den(192))
    call cont_QA(nsync, wf2(:,14), wf16(:,12), A(:,158), n3(:,304), t3x32(:,:,158), nhel, den(193))
    call cont_SS(nsync, wf8(:,71), wf4(:,58), A(:,159), n3(:,305), t3x32(:,:,159), nhel, den(194))
    call cont_VV(nsync, wf8(:,72), wf4(:,59), A(:,160), n3(:,306), t3x32(:,:,160), nhel, den(194))
    call cont_QA(nsync, wf2(:,16), wf16(:,13), A(:,161), n3(:,307), t3x32(:,:,161), nhel, den(195))
    call cont_QA(nsync, wf2(:,16), wf16(:,14), A(:,162), n3(:,308), t3x32(:,:,162), nhel, den(196))
    call cont_SS(nsync, wf8(:,67), wf4(:,60), A(:,163), n3(:,309), t3x32(:,:,163), nhel, den(198))
    call cont_VV(nsync, wf8(:,68), wf4(:,61), A(:,164), n3(:,310), t3x32(:,:,164), nhel, den(198))
    call cont_QA(nsync, wf2(:,16), wf16(:,15), A(:,165), n3(:,311), t3x32(:,:,165), nhel, den(199))
    call cont_QA(nsync, wf2(:,16), wf16(:,16), A(:,166), n3(:,312), t3x32(:,:,166), nhel, den(200))
    call cont_SS(nsync, wf8(:,71), wf4(:,60), A(:,167), n3(:,313), t3x32(:,:,167), nhel, den(201))
    call cont_VV(nsync, wf8(:,72), wf4(:,61), A(:,168), n3(:,314), t3x32(:,:,168), nhel, den(201))
    call cont_QA(nsync, wf2(:,18), wf16(:,13), A(:,169), n3(:,315), t3x32(:,:,169), nhel, den(202))
    call cont_QA(nsync, wf2(:,18), wf16(:,14), A(:,170), n3(:,316), t3x32(:,:,170), nhel, den(203))
    call cont_QA(nsync, wf2(:,20), wf16(:,9), A(:,171), n3(:,317), t3x32(:,:,171), nhel, den(204))
    call cont_QA(nsync, wf2(:,20), wf16(:,10), A(:,172), n3(:,318), t3x32(:,:,172), nhel, den(205))
    call cont_QA(nsync, wf2(:,18), wf16(:,15), A(:,173), n3(:,319), t3x32(:,:,173), nhel, den(206))
    call cont_QA(nsync, wf2(:,18), wf16(:,16), A(:,174), n3(:,320), t3x32(:,:,174), nhel, den(207))
    call cont_QA(nsync, wf2(:,20), wf16(:,11), A(:,175), n3(:,321), t3x32(:,:,175), nhel, den(208))
    call cont_QA(nsync, wf2(:,20), wf16(:,12), A(:,176), n3(:,322), t3x32(:,:,176), nhel, den(209))

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
  complex(REALKIND), intent(out) :: M1(2) ! M1helarray(2,32)
  integer :: empty(0)

  M1(1) = (A(j,115)%j+A(j,119)%j+A(j,123)%j+A(j,127)%j)*f(1)+(A(j,3)%j+A(j,4)%j)*f(2)+(A(j,110)%j+A(j,112)%j)*f(3)+(A(j,116)%j &
       +A(j,120)%j+A(j,124)%j+A(j,128)%j)*f(4)+(-A(j,114)%j-A(j,118)%j-A(j,122)%j-A(j,126)%j)*f(5)+(-A(j,113)%j-A(j,117)%j &
       -A(j,121)%j-A(j,125)%j)*f(6)+(-A(j,109)%j-A(j,111)%j)*f(7)+(A(j,136)%j+A(j,140)%j+A(j,148)%j+A(j,152)%j+A(j,156)%j &
       +A(j,160)%j+A(j,164)%j+A(j,168)%j)*f(8)+(A(j,169)%j+A(j,171)%j+A(j,173)%j+A(j,175)%j)*f(9)+(A(j,170)%j+A(j,172)%j &
       +A(j,174)%j+A(j,176)%j)*f(10)+(-A(j,135)%j-A(j,139)%j-A(j,147)%j-A(j,151)%j-A(j,155)%j-A(j,159)%j-A(j,163)%j &
       -A(j,167)%j)*f(11)+(A(j,129)%j+A(j,131)%j+A(j,133)%j+A(j,137)%j+A(j,141)%j+A(j,143)%j+A(j,145)%j+A(j,149)%j+A(j,153)%j &
       +A(j,157)%j+A(j,161)%j+A(j,165)%j)*f(12)+(A(j,130)%j+A(j,132)%j+A(j,134)%j+A(j,138)%j+A(j,142)%j+A(j,144)%j+A(j,146)%j &
       +A(j,150)%j+A(j,154)%j+A(j,158)%j+A(j,162)%j+A(j,166)%j)*f(13)
  M1(2) = (A(j,83)%j+A(j,89)%j+A(j,99)%j+A(j,105)%j)*f(1)+(A(j,1)%j+A(j,2)%j)*f(2)+(A(j,64)%j+A(j,70)%j)*f(3)+(A(j,84)%j+A(j,90)%j &
       +A(j,100)%j+A(j,106)%j)*f(4)+(-A(j,82)%j-A(j,88)%j-A(j,98)%j-A(j,104)%j)*f(5)+(-A(j,81)%j-A(j,87)%j-A(j,97)%j &
       -A(j,103)%j)*f(6)+(-A(j,63)%j-A(j,69)%j)*f(7)+(A(j,10)%j+A(j,20)%j+A(j,26)%j+A(j,36)%j+A(j,38)%j+A(j,48)%j+A(j,50)%j &
       +A(j,60)%j+A(j,86)%j+A(j,92)%j+A(j,102)%j+A(j,108)%j)*f(8)+(A(j,61)%j+A(j,65)%j+A(j,67)%j+A(j,71)%j+A(j,73)%j &
       +A(j,75)%j)*f(9)+(A(j,62)%j+A(j,66)%j+A(j,68)%j+A(j,72)%j+A(j,74)%j+A(j,76)%j)*f(10)+(-A(j,9)%j-A(j,19)%j-A(j,25)%j &
       -A(j,35)%j-A(j,37)%j-A(j,47)%j-A(j,49)%j-A(j,59)%j-A(j,85)%j-A(j,91)%j-A(j,101)%j-A(j,107)%j)*f(11)+(A(j,5)%j+A(j,7)%j &
       +A(j,11)%j+A(j,13)%j+A(j,15)%j+A(j,17)%j+A(j,21)%j+A(j,23)%j+A(j,27)%j+A(j,29)%j+A(j,31)%j+A(j,33)%j+A(j,39)%j+A(j,41)%j &
       +A(j,43)%j+A(j,45)%j+A(j,51)%j+A(j,53)%j+A(j,55)%j+A(j,57)%j+A(j,77)%j+A(j,79)%j+A(j,93)%j+A(j,95)%j)*f(12)+(A(j,6)%j &
       +A(j,8)%j+A(j,12)%j+A(j,14)%j+A(j,16)%j+A(j,18)%j+A(j,22)%j+A(j,24)%j+A(j,28)%j+A(j,30)%j+A(j,32)%j+A(j,34)%j+A(j,40)%j &
       +A(j,42)%j+A(j,44)%j+A(j,46)%j+A(j,52)%j+A(j,54)%j+A(j,56)%j+A(j,58)%j+A(j,78)%j+A(j,80)%j+A(j,94)%j+A(j,96)%j)*f(13)

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
  use ol_colourmatrix_pphhjj_ddxbbxhhg_1_/**/REALKIND, only: K1
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
  use ol_colourmatrix_pphhjj_ddxbbxhhg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(2), M2(2)
  real(REALKIND),    intent(out) :: M2colint(0:30-1)
  integer,           intent(in)  :: extcombs(:)
  integer :: extcomb, colmatpos, i, j, eco

  M2colint = 0

  do eco = 1, size(extcombs)
    extcomb = extcombs(eco)
    colmatpos = 2*extcomb
    do i = 1, 2
      do j = 1, 2
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
  use ol_colourmatrix_pphhjj_ddxbbxhhg_1_/**/REALKIND, only: K1
  implicit none

  complex(REALKIND), intent(in)  :: M1(2)
  complex(REALKIND), intent(in)  :: M2(2)
  complex(REALKIND),    intent(out) :: M2colint
  integer :: i, j

  M2colint = 0

  do i = 1, 2
    do j = 1, 2
      M2colint = M2colint + M1(i)*K1(i,j)*conjg(M2(j))
    end do
  end do

end subroutine colintmunu

end subroutine amp2


#ifdef PRECISION_dp
subroutine colourvector(M1out, nhelout) &
  & bind(c,name="ol_tree_colvect_pphhjj_ddxbbxhhg_1")
  ! Retrieve M1helarr from cache. Cache must have been filled before by calling amp2().
  ! [out] M1out(:,h): colour vector for each helicity configuration h
  ! [out] nhelout: number of non-vanishing helicity configurations
  implicit none
  complex(DREALKIND) :: M1out(2,32)
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
    & bind(c,name="ol_f_amp2tree_pphhjj_ddxbbxhhg_1")
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
    & bind(c,name="ol_f_amp2ccone_pphhjj_ddxbbxhhg_1")
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
    & bind(c,name="ol_f_amp2ccall_pphhjj_ddxbbxhhg_1")
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
    & bind(c,name="ol_f_amp2hcone_pphhjj_ddxbbxhhg_1")
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
    & bind(c,name="ol_f_amp2hcall_pphhjj_ddxbbxhhg_1")
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
    & bind(c,name="ol_amp2tree_pphhjj_ddxbbxhhg_1")
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
    & bind(c,name="ol_amp2ccone_pphhjj_ddxbbxhhg_1")
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
    & bind(c,name="ol_amp2ccall_pphhjj_ddxbbxhhg_1")
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
    & bind(c,name="ol_amp2hcone_pphhjj_ddxbbxhhg_1")
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
    & bind(c,name="ol_amp2hcall_pphhjj_ddxbbxhhg_1")
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
    & bind(c,name="amp2tree_pphhjj_ddxbbxhhg_1_")
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
    & bind(c,name="amp2ccone_pphhjj_ddxbbxhhg_1_")
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
    & bind(c,name="amp2ccall_pphhjj_ddxbbxhhg_1_")
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
    & bind(c,name="amp2hcone_pphhjj_ddxbbxhhg_1_")
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
    & bind(c,name="amp2hcall_pphhjj_ddxbbxhhg_1_")
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

end module ol_tree_pphhjj_ddxbbxhhg_1_/**/REALKIND
