
module ol_colourmatrix_pplljj_nenexbbxgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(46,2), K2(2,3), KL(2,3)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  48,  -6]
  K1( 2,:) = [  -6,  48]
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
  K1(13,:) = [  64,  -8]
  K1(14,:) = [  -8,  64]
  K1(15,:) = [   0,   0]
  K1(16,:) = [   0,   0]
  K1(17,:) = [   0,   0]
  K1(18,:) = [   0,   0]
  K1(19,:) = [  -1, -10]
  K1(20,:) = [ -10,  -1]
  K1(21,:) = [  64,  -8]
  K1(22,:) = [  -8,  64]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   9,   9]
  K1(28,:) = [   9, -72]
  K1(29,:) = [ -72,   9]
  K1(30,:) = [   9,   9]
  K1(31,:) = [ 144, -18]
  K1(32,:) = [ -18, 144]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [ -72,   9]
  K1(38,:) = [   9,   9]
  K1(39,:) = [   9,   9]
  K1(40,:) = [   9, -72]
  K1(41,:) = [ -81,   0]
  K1(42,:) = [   0, -81]
  K1(43,:) = [ 144, -18]
  K1(44,:) = [ -18, 144]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]
  K1 = (1._/**/REALKIND / 9) * K1

  K2(1,:) = [ 16, -2,  6]
  K2(2,:) = [ -2, 16,  6]
  K2 = (1._/**/REALKIND / 3) * K2

  KL(1,:) = [ 16, -2,  6]
  KL(2,:) = [ -2, 16,  6]
  KL = (1._/**/REALKIND / 3) * KL

  end subroutine colourmatrix_init
end module ol_colourmatrix_pplljj_nenexbbxgg_1_/**/REALKIND



module ol_forced_parameters_pplljj_nenexbbxgg_1_/**/REALKIND
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
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pplljj_nenexbbxgg_1_/**/REALKIND

module ol_loop_pplljj_nenexbbxgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(18), c(17)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:103)
  ! denominators
  complex(REALKIND), save :: den(99)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,64)
  ! zero helicity identifier
  logical,           save :: zerohel(64) = .true., zerohel_ct(64) = .true.

  contains

! **********************************************************************
subroutine fac_init_loop()
! Writes diagram prefactors to 'f', rsp. 'c'
! **********************************************************************
  use ol_parameters_decl_/**/REALKIND
  use ol_parameters_init_/**/REALKIND, only: parameters_init, loop_parameters_init
  use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = CI*eQED**2*gQCD**2
    f( 2) = eQED**2*gQCD**2
    f( 3) = CI*countertermnorm*eQED**2*gQCD**4
    f( 4) = countertermnorm*eQED**2*gQCD**4
    f( 5) = CI*countertermnorm*ctGbb*eQED**2*gQCD**4
    f( 6) = countertermnorm*ctGbb*eQED**2*gQCD**4
    f( 7) = CI*countertermnorm*ctVbb*eQED**2*gQCD**4
    f( 8) = countertermnorm*ctVbb*eQED**2*gQCD**4
    f( 9) = countertermnorm*ctVVV*eQED**2*gQCD**4
    f(10) = CI*countertermnorm*ctZGG*eQED**2*gQCD**4
    f(11) = countertermnorm*ctZGG*eQED**2*gQCD**4
    f(12) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/2._/**/REALKIND
    f(13) = CI*eQED**2*gQCD**4*integralnorm*SwB
    f(14) = eQED**2*gQCD**4*integralnorm*SwB
    f(15) = CI*eQED**2*gQCD**4*integralnorm*SwF
    f(16) = 2*CI*eQED**2*gQCD**4*integralnorm*SwF
    f(17) = eQED**2*gQCD**4*integralnorm*SwF
    f(18) = 2*eQED**2*gQCD**4*integralnorm*SwF

  c = [ 9*CI*f(12), 18*CI*f(12), CI*f(13), 3*CI*f(13), 8*CI*f(13), 9*CI*f(13), 18*CI*f(13), f(14), 3*f(14), 8*f(14), 9*f(14) &
    , 3*CI*f(15), 3*CI*f(16), f(17), 3*f(17), f(18), 3*f(18) ]
  c = (1._/**/REALKIND / 6) * c
end subroutine fac_init_loop


! **********************************************************************
subroutine tree_wavefunctions(P, H, M1, M2)
! P(0:3,npart) = 2 -> n-2 external momenta (standard representation)
! H(npart)     = external-particle helicities
! Writes the tree wave functions to 'wf', denominators to 'den'.
! Returns the Born and counterterm colour vectors M1 and M2.
! **********************************************************************
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/REALKIND ! counterterms
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_wavefunctions_/**/REALKIND
  use ol_propagators_/**/REALKIND
  use ol_vertices_/**/REALKIND
  use ol_counterterms_/**/REALKIND
  implicit none
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  complex(REALKIND), intent(out) :: M1(2), M2(3)
  complex(REALKIND) :: A(57)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rMB, H(3), wf(:,-2))
  call wf_A(P(:,4), rMB, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_VQ_A(wf(:,-4),wf(:,-2),wf(:,2))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call prop_Q_A(wf(:,2),Q(:,20),MB,1_intkind1,wf(:,5))
  call prop_A_Q(wf(:,3),Q(:,40),MB,1_intkind1,wf(:,6))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,5),wf(:,7))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,4),wf(:,8))
  call vert_VQ_A(wf(:,-5),wf(:,5),wf(:,9))
  call prop_A_Q(wf(:,8),Q(:,11),MB,1_intkind1,wf(:,10))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,11))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,36),MB,1_intkind1,wf(:,13))
  call prop_A_Q(wf(:,12),Q(:,24),MB,1_intkind1,wf(:,14))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,13),wf(:,15))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,-2),wf(:,16))
  call vert_AV_Q(wf(:,14),wf(:,-5),wf(:,17))
  call prop_Q_A(wf(:,16),Q(:,7),MB,1_intkind1,wf(:,18))
  call vert_VQ_A(wf(:,-4),wf(:,13),wf(:,19))
  call vert_AV_Q(wf(:,6),wf(:,-4),wf(:,20))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,21))
  call vert_AV_Q(wf(:,-3),wf(:,21),wf(:,22))
  call vert_VQ_A(wf(:,21),wf(:,-2),wf(:,23))
  call prop_Q_A(wf(:,23),Q(:,52),MB,1_intkind1,wf(:,24))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,25))
  call counter_VGG_G(ctZGGG,wf(:,4),wf(:,-4),wf(:,-5),wf(:,26))
  call counter_VGG_G(ctZGGG,wf(:,4),wf(:,-5),wf(:,-4),wf(:,27))
  call counter_VG_G(wf(:,4),wf(:,25),Q(:,12),wf(:,28),Q(:,15))
  call vert_UV_W(wf(:,25),Q(:,12),wf(:,-4),Q(:,16),wf(:,29))
  call counter_VG_G(wf(:,4),wf(:,-5),Q(:,32),wf(:,30),Q(:,35))
  call vert_UV_W(wf(:,25),Q(:,12),wf(:,-5),Q(:,32),wf(:,31))
  call counter_VG_G(wf(:,4),wf(:,-4),Q(:,16),wf(:,32),Q(:,19))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,5),wf(:,33))
  call counter_VQ_A(wf(:,-5),wf(:,5),wf(:,34))
  call vert_QA_V(wf(:,5),wf(:,-3),wf(:,35))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,13),wf(:,36))
  call counter_AV_Q(wf(:,14),wf(:,-5),wf(:,37))
  call vert_QA_V(wf(:,-2),wf(:,14),wf(:,38))
  call counter_VQ_A(wf(:,-4),wf(:,13),wf(:,39))
  call vert_QA_V(wf(:,13),wf(:,-3),wf(:,40))
  call counter_AV_Q(wf(:,6),wf(:,-4),wf(:,41))
  call vert_QA_V(wf(:,-2),wf(:,6),wf(:,42))
  call counter_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,43))
  call vert_AV_Q(wf(:,-3),wf(:,43),wf(:,44))
  call vert_VQ_A(wf(:,43),wf(:,-2),wf(:,45))
  call prop_Q_A(wf(:,45),Q(:,52),MB,1_intkind1,wf(:,46))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,4),wf(:,47))
  call prop_Q_A(wf(:,9),Q(:,52),MB,1_intkind1,wf(:,48))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,49))
  call prop_A_Q(wf(:,49),Q(:,40),MB,1_intkind1,wf(:,50))
  call prop_Q_A(wf(:,19),Q(:,52),MB,1_intkind1,wf(:,51))
  call counter_AV_Q(wf(:,-3),wf(:,21),wf(:,52))
  call vert_AV_Q(wf(:,50),wf(:,-4),wf(:,53))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,54))
  call prop_A_Q(wf(:,54),Q(:,24),MB,1_intkind1,wf(:,55))
  call vert_AV_Q(wf(:,55),wf(:,-5),wf(:,56))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,-2),wf(:,57))
  call prop_A_Q(wf(:,17),Q(:,56),MB,1_intkind1,wf(:,58))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,59))
  call prop_Q_A(wf(:,59),Q(:,36),MB,1_intkind1,wf(:,60))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,60),wf(:,61))
  call prop_A_Q(wf(:,20),Q(:,56),MB,1_intkind1,wf(:,62))
  call counter_VQ_A(wf(:,21),wf(:,-2),wf(:,63))
  call prop_A_Q(wf(:,22),Q(:,56),MB,1_intkind1,wf(:,64))
  call vert_VQ_A(wf(:,-4),wf(:,60),wf(:,65))
  call counter_VQ_A(wf(:,-4),wf(:,-2),wf(:,66))
  call prop_Q_A(wf(:,66),Q(:,20),MB,1_intkind1,wf(:,67))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,67),wf(:,68))
  call vert_VQ_A(wf(:,-5),wf(:,67),wf(:,69))
  call vert_AZ_Q(gZd,wf(:,6),wf(:,4),wf(:,70))
  call counter_Q_A(ctbb,wf(:,5),Q(:,20),wf(:,71))
  call prop_A_Q(wf(:,70),Q(:,43),MB,1_intkind1,wf(:,72))
  call counter_A_Q(ctbb,wf(:,6),Q(:,40),wf(:,73))
  call prop_Q_A(wf(:,7),Q(:,23),MB,1_intkind1,wf(:,74))
  call prop_Q_A(wf(:,71),Q(:,20),MB,1_intkind1,wf(:,75))
  call vert_VQ_A(wf(:,-5),wf(:,75),wf(:,76))
  call counter_A_Q(ctbb,wf(:,10),Q(:,11),wf(:,77))
  call vert_AZ_Q(gZd,wf(:,14),wf(:,4),wf(:,78))
  call counter_Q_A(ctbb,wf(:,13),Q(:,36),wf(:,79))
  call prop_A_Q(wf(:,78),Q(:,27),MB,1_intkind1,wf(:,80))
  call counter_A_Q(ctbb,wf(:,14),Q(:,24),wf(:,81))
  call prop_Q_A(wf(:,15),Q(:,39),MB,1_intkind1,wf(:,82))
  call counter_Q_A(ctbb,wf(:,18),Q(:,7),wf(:,83))
  call prop_A_Q(wf(:,81),Q(:,24),MB,1_intkind1,wf(:,84))
  call vert_AV_Q(wf(:,84),wf(:,-5),wf(:,85))
  call prop_Q_A(wf(:,79),Q(:,36),MB,1_intkind1,wf(:,86))
  call vert_VQ_A(wf(:,-4),wf(:,86),wf(:,87))
  call prop_A_Q(wf(:,73),Q(:,40),MB,1_intkind1,wf(:,88))
  call vert_AV_Q(wf(:,88),wf(:,-4),wf(:,89))
  call counter_Q_A(ctbb,wf(:,24),Q(:,52),wf(:,90))
  call counter_V_V(ctGG,wf(:,21),Q(:,48),wf(:,91))
  call vert_VQ_A(wf(:,91),wf(:,-2),wf(:,92))
  call vert_AV_Q(wf(:,-3),wf(:,91),wf(:,93))
  call vert_QA_V(wf(:,18),wf(:,-3),wf(:,94))
  call vert_VQ_A(wf(:,-4),wf(:,18),wf(:,95))
  call prop_Q_A(wf(:,95),Q(:,23),MB,1_intkind1,wf(:,96))
  call vert_VQ_A(wf(:,-5),wf(:,18),wf(:,97))
  call prop_Q_A(wf(:,97),Q(:,39),MB,1_intkind1,wf(:,98))
  call vert_QA_V(wf(:,-2),wf(:,10),wf(:,99))
  call vert_AV_Q(wf(:,10),wf(:,-4),wf(:,100))
  call prop_A_Q(wf(:,100),Q(:,27),MB,1_intkind1,wf(:,101))
  call vert_AV_Q(wf(:,10),wf(:,-5),wf(:,102))
  call prop_A_Q(wf(:,102),Q(:,43),MB,1_intkind1,wf(:,103))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3) - MZ2)
  den(2) = 1 / (Q(5,20) - MB2)
  den(3) = 1 / (Q(5,40) - MB2)
  den(6) = 1 / (Q(5,11) - MB2)
  den(9) = 1 / (Q(5,36) - MB2)
  den(10) = 1 / (Q(5,24) - MB2)
  den(13) = 1 / (Q(5,7) - MB2)
  den(18) = 1 / (Q(5,48))
  den(20) = 1 / (Q(5,52) - MB2)
  den(23) = 1 / (Q(5,12))
  den(26) = 1 / (Q(5,28))
  den(29) = 1 / (Q(5,44))
  den(44) = 1 / (Q(5,56) - MB2)
  den(53) = 1 / (Q(5,43) - MB2)
  den(56) = 1 / (Q(5,23) - MB2)
  den(63) = 1 / (Q(5,27) - MB2)
  den(66) = 1 / (Q(5,39) - MB2)
  den(83) = 1 / (Q(5,15))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(11) = den(1)*den(9)
  den(12) = den(10)*den(11)
  den(14) = den(1)*den(13)
  den(15) = den(10)*den(14)
  den(16) = den(7)*den(9)
  den(17) = den(3)*den(14)
  den(19) = den(14)*den(18)
  den(21) = den(18)*den(20)
  den(22) = den(1)*den(21)
  den(24) = den(1)*den(23)
  den(25) = den(18)*den(24)
  den(27) = den(23)*den(26)
  den(28) = den(1)*den(27)
  den(30) = den(23)*den(29)
  den(31) = den(1)*den(30)
  den(32) = den(2)*den(26)
  den(33) = den(1)*den(32)
  den(34) = den(10)*den(26)
  den(35) = den(1)*den(34)
  den(36) = den(9)*den(29)
  den(37) = den(1)*den(36)
  den(38) = den(3)*den(29)
  den(39) = den(1)*den(38)
  den(40) = den(2)*den(20)
  den(41) = den(1)*den(40)
  den(42) = den(9)*den(20)
  den(43) = den(1)*den(42)
  den(45) = den(10)*den(44)
  den(46) = den(1)*den(45)
  den(47) = den(3)*den(44)
  den(48) = den(1)*den(47)
  den(49) = den(7)*den(18)
  den(50) = den(18)*den(44)
  den(51) = den(1)*den(50)
  den(52) = den(1)*den(3)
  den(54) = den(52)*den(53)
  den(55) = den(2)*den(54)
  den(57) = den(4)*den(56)
  den(58) = den(3)*den(57)
  den(59) = den(2)**2
  den(60) = den(7)*den(59)
  den(61) = den(7)*den(40)
  den(62) = den(1)*den(10)
  den(64) = den(62)*den(63)
  den(65) = den(9)*den(64)
  den(67) = den(11)*den(66)
  den(68) = den(10)*den(67)
  den(69) = den(14)*den(45)
  den(70) = den(10)**2
  den(71) = den(14)*den(70)
  den(72) = den(9)**2
  den(73) = den(7)*den(72)
  den(74) = den(7)*den(42)
  den(75) = den(14)*den(47)
  den(76) = den(3)**2
  den(77) = den(14)*den(76)
  den(78) = den(14)*den(50)
  den(79) = den(7)*den(21)
  den(80) = den(18)**2
  den(81) = den(7)*den(80)
  den(82) = den(14)*den(80)
  den(84) = den(14)*den(83)
  den(85) = den(14)*den(56)
  den(86) = den(14)*den(66)
  den(87) = den(7)*den(83)
  den(88) = den(7)*den(63)
  den(89) = den(7)*den(53)
  den(90) = den(1)*den(18)*den(23)
  den(91) = den(1)*den(2)*den(3)
  den(92) = den(1)*den(9)*den(10)
  den(93) = den(1)*den(18)
  den(94) = den(2)*den(89)
  den(95) = den(10)*den(86)
  den(96) = den(9)*den(88)
  den(97) = den(3)*den(85)
  den(98) = den(18)*den(84)
  den(99) = den(18)*den(87)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(57)

  A(1) = cont_QA(wf(:,6),wf(:,7)) * den(5)
  A(2) = cont_QA(wf(:,9),wf(:,10)) * den(8)
  A(3) = cont_QA(wf(:,14),wf(:,15)) * den(12)
  A(4) = cont_QA(wf(:,17),wf(:,18)) * den(15)
  A(5) = cont_QA(wf(:,10),wf(:,19)) * den(16)
  A(6) = cont_QA(wf(:,18),wf(:,20)) * den(17)
  A(7) = cont_QA(wf(:,18),wf(:,22)) * den(19)
  A(8) = cont_QA(wf(:,8),wf(:,24)) * den(22)

  A(9) = cont_VV(wf(:,25),wf(:,26)) * den(24)
  A(10) = cont_VV(wf(:,25),wf(:,27)) * den(24)
  A(11) = cont_VV(wf(:,21),wf(:,28)) * den(25)
  A(12) = cont_VV(wf(:,29),wf(:,30)) * den(28)
  A(13) = cont_VV(wf(:,31),wf(:,32)) * den(31)
  A(14) = cont_QA(wf(:,6),wf(:,33)) * den(5)
  A(15) = cont_QA(wf(:,10),wf(:,34)) * den(8)
  A(16) = cont_VV(wf(:,30),wf(:,35)) * den(33)
  A(17) = cont_QA(wf(:,14),wf(:,36)) * den(12)
  A(18) = cont_QA(wf(:,18),wf(:,37)) * den(15)
  A(19) = cont_VV(wf(:,30),wf(:,38)) * den(35)
  A(20) = cont_QA(wf(:,10),wf(:,39)) * den(16)
  A(21) = cont_VV(wf(:,32),wf(:,40)) * den(37)
  A(22) = cont_QA(wf(:,18),wf(:,41)) * den(17)
  A(23) = cont_VV(wf(:,32),wf(:,42)) * den(39)
  A(24) = cont_QA(wf(:,18),wf(:,44)) * den(19)
  A(25) = cont_QA(wf(:,8),wf(:,46)) * den(22)
  A(26) = cont_QA(wf(:,47),wf(:,48)) * den(41)
  A(27) = cont_QA(wf(:,7),wf(:,50)) * den(5)
  A(28) = cont_QA(wf(:,47),wf(:,51)) * den(43)
  A(29) = cont_QA(wf(:,18),wf(:,52)) * den(19)
  A(30) = cont_QA(wf(:,24),wf(:,47)) * den(22)
  A(31) = cont_QA(wf(:,18),wf(:,53)) * den(17)
  A(32) = cont_QA(wf(:,15),wf(:,55)) * den(12)
  A(33) = cont_QA(wf(:,18),wf(:,56)) * den(15)
  A(34) = cont_QA(wf(:,57),wf(:,58)) * den(46)
  A(35) = cont_QA(wf(:,14),wf(:,61)) * den(12)
  A(36) = cont_QA(wf(:,57),wf(:,62)) * den(48)
  A(37) = cont_QA(wf(:,10),wf(:,63)) * den(49)
  A(38) = cont_QA(wf(:,57),wf(:,64)) * den(51)
  A(39) = cont_QA(wf(:,10),wf(:,65)) * den(16)
  A(40) = cont_QA(wf(:,6),wf(:,68)) * den(5)
  A(41) = cont_QA(wf(:,10),wf(:,69)) * den(8)
  A(42) = cont_QA(wf(:,71),wf(:,72)) * den(55)
  A(43) = cont_QA(wf(:,73),wf(:,74)) * den(58)
  A(44) = cont_QA(wf(:,10),wf(:,76)) * den(60)
  A(45) = cont_QA(wf(:,48),wf(:,77)) * den(61)
  A(46) = cont_QA(wf(:,79),wf(:,80)) * den(65)
  A(47) = cont_QA(wf(:,81),wf(:,82)) * den(68)
  A(48) = cont_QA(wf(:,58),wf(:,83)) * den(69)
  A(49) = cont_QA(wf(:,18),wf(:,85)) * den(71)
  A(50) = cont_QA(wf(:,10),wf(:,87)) * den(73)
  A(51) = cont_QA(wf(:,51),wf(:,77)) * den(74)
  A(52) = cont_QA(wf(:,62),wf(:,83)) * den(75)
  A(53) = cont_QA(wf(:,18),wf(:,89)) * den(77)
  A(54) = cont_QA(wf(:,64),wf(:,83)) * den(78)
  A(55) = cont_QA(wf(:,10),wf(:,90)) * den(79)
  A(56) = cont_QA(wf(:,10),wf(:,92)) * den(81)
  A(57) = cont_QA(wf(:,18),wf(:,93)) * den(82)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(57)
  complex(REALKIND), intent(out) :: M1(2), M2(3)

  M1(1) = (A(3)+A(4)+A(5))*f(1)+CI*(A(7)+A(8))*f(2)
  M1(2) = (A(1)+A(2)+A(6))*f(1)+CI*(-A(7)-A(8))*f(2)

  M2(1) = (A(9)*f(3))/2._/**/REALKIND+(-A(46)-A(47)-A(48)-A(49)-A(50)-A(51))*f(3)+CI*(-A(54)-A(55)-A(56)-A(57))*f(4)+(A(18)+A(20) &
       +A(32)+A(33)+A(35)+A(39))*f(5)+CI*(A(29)+A(37))*f(6)+(A(17)+A(28)+A(34))*f(7)+CI*(A(30)+A(38))*f(8)+CI*(A(24)+A(25))*f(9) &
       +CI*(A(11)+A(12)-A(13))*f(10)+(-A(19)-A(21))*f(11)
  M2(2) = (A(10)*f(3))/2._/**/REALKIND+(-A(42)-A(43)-A(44)-A(45)-A(52)-A(53))*f(3)+CI*(A(54)+A(55)+A(56)+A(57))*f(4)+(A(15)+A(22) &
       +A(27)+A(31)+A(40)+A(41))*f(5)+CI*(-A(29)-A(37))*f(6)+(A(14)+A(26)+A(36))*f(7)+CI*(-A(30)-A(38))*f(8)+CI*(-A(24) &
       -A(25))*f(9)+CI*(-A(11)-A(12)+A(13))*f(10)+(-A(16)-A(23))*f(11)
  M2(3) = ((-A(9)-A(10))*f(3))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pplljj_nenexbbxgg_1_/**/REALKIND
