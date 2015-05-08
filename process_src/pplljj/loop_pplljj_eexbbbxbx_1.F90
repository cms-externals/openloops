
module ol_colourmatrix_pplljj_eexbbbxbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(46,2), K2(2,2), KL(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9,   3]
  K1( 2,:) = [   3,   9]
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
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [   0,   0]
  K1(16,:) = [   0,   0]
  K1(17,:) = [   0,   0]
  K1(18,:) = [   0,   0]
  K1(19,:) = [   0,   4]
  K1(20,:) = [   4,   0]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   0,  -4]
  K1(28,:) = [  -4, -12]
  K1(29,:) = [ -12,  -4]
  K1(30,:) = [  -4,   0]
  K1(31,:) = [  12,   4]
  K1(32,:) = [   4,  12]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [ -12,  -4]
  K1(38,:) = [  -4,   0]
  K1(39,:) = [   0,  -4]
  K1(40,:) = [  -4, -12]
  K1(41,:) = [   0,   4]
  K1(42,:) = [   4,   0]
  K1(43,:) = [  12,   4]
  K1(44,:) = [   4,  12]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pplljj_eexbbbxbx_1_/**/REALKIND



module ol_forced_parameters_pplljj_eexbbbxbx_1_/**/REALKIND
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
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pplljj_eexbbbxbx_1_/**/REALKIND

module ol_loop_pplljj_eexbbbxbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(20), c(36)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:109)
  ! denominators
  complex(REALKIND), save :: den(148)
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
    f( 1) = (CI*eQED**2*gQCD**2)/3._/**/REALKIND
    f( 2) = CI*eQED**2*gQCD**2
    f( 3) = (CI*countertermnorm*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 4) = CI*countertermnorm*eQED**2*gQCD**4
    f( 5) = (CI*countertermnorm*ctGbb*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 6) = CI*countertermnorm*ctGbb*eQED**2*gQCD**4
    f( 7) = (CI*countertermnorm*ctVbb*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 8) = CI*countertermnorm*ctVbb*eQED**2*gQCD**4
    f( 9) = countertermnorm*ctZGG*eQED**2*gQCD**4
    f(10) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(11) = CI*eQED**2*gQCD**4*integralnorm*SwB
    f(12) = (eQED**2*gQCD**4*integralnorm*SwB)/6._/**/REALKIND
    f(13) = (eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(14) = (eQED**2*gQCD**4*integralnorm*SwB)/2._/**/REALKIND
    f(15) = eQED**2*gQCD**4*integralnorm*SwB
    f(16) = (eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(17) = (2*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(18) = eQED**2*gQCD**4*integralnorm*SwF
    f(19) = (4*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(20) = 2*eQED**2*gQCD**4*integralnorm*SwF

  c = [ 9*CI*f(10), 27*CI*f(10), 9*CI*f(11), 27*CI*f(11), 18*f(12), 54*f(12), f(13), 3*f(13), 6*f(13), 8*f(13), 10*f(13), 18*f(13) &
    , 21*f(13), 24*f(13), 54*f(13), 18*f(14), 54*f(14), f(15), 3*f(15), 6*f(15), 8*f(15), 10*f(15), 18*f(15), 21*f(15), 24*f(15) &
    , 54*f(15), 3*f(16), 9*f(16), 3*f(17), 9*f(17), 3*f(18), 9*f(18), 3*f(19), 9*f(19), 3*f(20), 9*f(20) ]
  c = (1._/**/REALKIND / 36) * c
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
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(98)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rMB, H(3), wf(:,-2))
  call wf_Q(P(:,4), rMB, H(4), wf(:,-3))
  call wf_A(P(:,5), rMB, H(5), wf(:,-4))
  call wf_A(P(:,6), rMB, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-4),wf(:,2))
  call vert_VQ_A(wf(:,1),wf(:,-3),wf(:,3))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,4))
  call prop_Q_A(wf(:,3),Q(:,11),MB,1_intkind1,wf(:,5))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,6))
  call prop_W_W(wf(:,6),Q(:,3),MZ,1_intkind1,wf(:,7))
  call vert_ZQ_A(gZd,wf(:,7),wf(:,-3),wf(:,8))
  call prop_Q_A(wf(:,8),Q(:,11),MB,1_intkind1,wf(:,9))
  call vert_VQ_A(wf(:,2),wf(:,-3),wf(:,10))
  call vert_AV_Q(wf(:,-5),wf(:,1),wf(:,11))
  call prop_Q_A(wf(:,10),Q(:,28),MB,1_intkind1,wf(:,12))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,7),wf(:,13))
  call vert_QA_V(wf(:,-3),wf(:,-4),wf(:,14))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,15))
  call vert_AV_Q(wf(:,-5),wf(:,14),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,7),MB,1_intkind1,wf(:,17))
  call vert_ZQ_A(gZd,wf(:,7),wf(:,-2),wf(:,18))
  call prop_Q_A(wf(:,18),Q(:,7),MB,1_intkind1,wf(:,19))
  call vert_VQ_A(wf(:,14),wf(:,-2),wf(:,20))
  call prop_Q_A(wf(:,20),Q(:,28),MB,1_intkind1,wf(:,21))
  call vert_QA_V(wf(:,-2),wf(:,-5),wf(:,22))
  call vert_AV_Q(wf(:,-4),wf(:,22),wf(:,23))
  call vert_VQ_A(wf(:,22),wf(:,-3),wf(:,24))
  call vert_AV_Q(wf(:,-4),wf(:,1),wf(:,25))
  call prop_Q_A(wf(:,24),Q(:,44),MB,1_intkind1,wf(:,26))
  call vert_AZ_Q(gZd,wf(:,-4),wf(:,7),wf(:,27))
  call vert_QA_V(wf(:,-3),wf(:,-5),wf(:,28))
  call vert_AV_Q(wf(:,-4),wf(:,28),wf(:,29))
  call vert_VQ_A(wf(:,28),wf(:,-2),wf(:,30))
  call prop_Q_A(wf(:,30),Q(:,44),MB,1_intkind1,wf(:,31))
  call counter_VG_G(wf(:,7),wf(:,2),Q(:,20),wf(:,32),Q(:,23))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,33))
  call counter_AV_Q(wf(:,-5),wf(:,1),wf(:,34))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,7),wf(:,35))
  call counter_VG_G(wf(:,7),wf(:,22),Q(:,36),wf(:,36),Q(:,39))
  call counter_AV_Q(wf(:,-5),wf(:,14),wf(:,37))
  call counter_AV_Q(wf(:,-4),wf(:,22),wf(:,38))
  call counter_AV_Q(wf(:,-4),wf(:,1),wf(:,39))
  call counter_AZ_Q(gZd,wf(:,-4),wf(:,7),wf(:,40))
  call counter_AV_Q(wf(:,-4),wf(:,28),wf(:,41))
  call counter_VQ_A(wf(:,2),wf(:,-3),wf(:,42))
  call prop_A_Q(wf(:,11),Q(:,35),MB,1_intkind1,wf(:,43))
  call prop_A_Q(wf(:,13),Q(:,35),MB,1_intkind1,wf(:,44))
  call counter_VQ_A(wf(:,1),wf(:,-3),wf(:,45))
  call prop_A_Q(wf(:,4),Q(:,52),MB,1_intkind1,wf(:,46))
  call counter_ZQ_A(gZd,wf(:,7),wf(:,-3),wf(:,47))
  call counter_VQ_A(wf(:,22),wf(:,-3),wf(:,48))
  call prop_A_Q(wf(:,25),Q(:,19),MB,1_intkind1,wf(:,49))
  call prop_A_Q(wf(:,27),Q(:,19),MB,1_intkind1,wf(:,50))
  call prop_A_Q(wf(:,23),Q(:,52),MB,1_intkind1,wf(:,51))
  call counter_QA_V(wf(:,-3),wf(:,-5),wf(:,52))
  call vert_AV_Q(wf(:,-4),wf(:,52),wf(:,53))
  call vert_VQ_A(wf(:,52),wf(:,-2),wf(:,54))
  call prop_Q_A(wf(:,54),Q(:,44),MB,1_intkind1,wf(:,55))
  call counter_QA_V(wf(:,-3),wf(:,-4),wf(:,56))
  call vert_AV_Q(wf(:,-5),wf(:,56),wf(:,57))
  call vert_VQ_A(wf(:,56),wf(:,-2),wf(:,58))
  call prop_Q_A(wf(:,58),Q(:,28),MB,1_intkind1,wf(:,59))
  call counter_VQ_A(wf(:,14),wf(:,-2),wf(:,60))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,61))
  call prop_A_Q(wf(:,16),Q(:,56),MB,1_intkind1,wf(:,62))
  call counter_ZQ_A(gZd,wf(:,7),wf(:,-2),wf(:,63))
  call counter_VQ_A(wf(:,28),wf(:,-2),wf(:,64))
  call prop_A_Q(wf(:,29),Q(:,56),MB,1_intkind1,wf(:,65))
  call counter_QA_V(wf(:,-2),wf(:,-5),wf(:,66))
  call vert_AV_Q(wf(:,-4),wf(:,66),wf(:,67))
  call vert_VQ_A(wf(:,66),wf(:,-3),wf(:,68))
  call prop_Q_A(wf(:,68),Q(:,44),MB,1_intkind1,wf(:,69))
  call counter_QA_V(wf(:,-2),wf(:,-4),wf(:,70))
  call vert_AV_Q(wf(:,-5),wf(:,70),wf(:,71))
  call vert_VQ_A(wf(:,70),wf(:,-3),wf(:,72))
  call prop_Q_A(wf(:,72),Q(:,28),MB,1_intkind1,wf(:,73))
  call counter_V_V(ctGG,wf(:,2),Q(:,20),wf(:,74))
  call vert_VQ_A(wf(:,74),wf(:,-3),wf(:,75))
  call vert_AV_Q(wf(:,-5),wf(:,74),wf(:,76))
  call counter_Q_A(ctbb,wf(:,5),Q(:,11),wf(:,77))
  call counter_Q_A(ctbb,wf(:,9),Q(:,11),wf(:,78))
  call counter_Q_A(ctbb,wf(:,12),Q(:,28),wf(:,79))
  call counter_V_V(ctGG,wf(:,14),Q(:,24),wf(:,80))
  call vert_VQ_A(wf(:,80),wf(:,-2),wf(:,81))
  call counter_Q_A(ctbb,wf(:,17),Q(:,7),wf(:,82))
  call counter_Q_A(ctbb,wf(:,19),Q(:,7),wf(:,83))
  call counter_Q_A(ctbb,wf(:,21),Q(:,28),wf(:,84))
  call vert_AV_Q(wf(:,-5),wf(:,80),wf(:,85))
  call counter_V_V(ctGG,wf(:,22),Q(:,36),wf(:,86))
  call vert_VQ_A(wf(:,86),wf(:,-3),wf(:,87))
  call vert_AV_Q(wf(:,-4),wf(:,86),wf(:,88))
  call counter_Q_A(ctbb,wf(:,26),Q(:,44),wf(:,89))
  call counter_V_V(ctGG,wf(:,28),Q(:,40),wf(:,90))
  call vert_VQ_A(wf(:,90),wf(:,-2),wf(:,91))
  call counter_Q_A(ctbb,wf(:,31),Q(:,44),wf(:,92))
  call vert_AV_Q(wf(:,-4),wf(:,90),wf(:,93))
  call vert_QA_V(wf(:,17),wf(:,-4),wf(:,94))
  call vert_QA_V(wf(:,19),wf(:,-4),wf(:,95))
  call vert_QA_V(wf(:,17),wf(:,-5),wf(:,96))
  call vert_QA_V(wf(:,19),wf(:,-5),wf(:,97))
  call vert_QA_V(wf(:,5),wf(:,-4),wf(:,98))
  call vert_QA_V(wf(:,9),wf(:,-4),wf(:,99))
  call vert_QA_V(wf(:,5),wf(:,-5),wf(:,100))
  call vert_QA_V(wf(:,9),wf(:,-5),wf(:,101))
  call vert_QA_V(wf(:,-2),wf(:,49),wf(:,102))
  call vert_QA_V(wf(:,-2),wf(:,50),wf(:,103))
  call vert_QA_V(wf(:,-3),wf(:,49),wf(:,104))
  call vert_QA_V(wf(:,-3),wf(:,50),wf(:,105))
  call vert_QA_V(wf(:,-2),wf(:,43),wf(:,106))
  call vert_QA_V(wf(:,-2),wf(:,44),wf(:,107))
  call vert_QA_V(wf(:,-3),wf(:,43),wf(:,108))
  call vert_QA_V(wf(:,-3),wf(:,44),wf(:,109))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3))
  den(2) = 1 / (Q(5,20))
  den(3) = 1 / (Q(5,11) - MB2)
  den(6) = 1 / (Q(5,3) - MZ2)
  den(9) = 1 / (Q(5,28) - MB2)
  den(13) = 1 / (Q(5,24))
  den(14) = 1 / (Q(5,7) - MB2)
  den(22) = 1 / (Q(5,36))
  den(25) = 1 / (Q(5,44) - MB2)
  den(29) = 1 / (Q(5,40))
  den(39) = 1 / (Q(5,35) - MB2)
  den(44) = 1 / (Q(5,52) - MB2)
  den(48) = 1 / (Q(5,19) - MB2)
  den(58) = 1 / (Q(5,56) - MB2)
  den(103) = 1 / (Q(5,23))
  den(106) = 1 / (Q(5,39))
  den(109) = 1 / (Q(5,27))
  den(112) = 1 / (Q(5,43))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(3)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(12) = den(6)*den(10)
  den(15) = den(1)*den(14)
  den(16) = den(13)*den(15)
  den(17) = den(6)*den(14)
  den(18) = den(13)*den(17)
  den(19) = den(9)*den(13)
  den(20) = den(1)*den(19)
  den(21) = den(6)*den(19)
  den(23) = den(4)*den(22)
  den(24) = den(7)*den(22)
  den(26) = den(22)*den(25)
  den(27) = den(1)*den(26)
  den(28) = den(6)*den(26)
  den(30) = den(15)*den(29)
  den(31) = den(17)*den(29)
  den(32) = den(25)*den(29)
  den(33) = den(1)*den(32)
  den(34) = den(6)*den(32)
  den(35) = den(2)*den(6)
  den(36) = den(29)*den(35)
  den(37) = den(6)*den(22)
  den(38) = den(13)*den(37)
  den(40) = den(1)*den(39)
  den(41) = den(2)*den(40)
  den(42) = den(6)*den(39)
  den(43) = den(2)*den(42)
  den(45) = den(2)*den(44)
  den(46) = den(1)*den(45)
  den(47) = den(6)*den(45)
  den(49) = den(1)*den(48)
  den(50) = den(22)*den(49)
  den(51) = den(6)*den(48)
  den(52) = den(22)*den(51)
  den(53) = den(22)*den(44)
  den(54) = den(1)*den(53)
  den(55) = den(6)*den(53)
  den(56) = den(13)*den(40)
  den(57) = den(13)*den(42)
  den(59) = den(13)*den(58)
  den(60) = den(1)*den(59)
  den(61) = den(6)*den(59)
  den(62) = den(29)*den(49)
  den(63) = den(29)*den(51)
  den(64) = den(29)*den(58)
  den(65) = den(1)*den(64)
  den(66) = den(6)*den(64)
  den(67) = den(2)**2
  den(68) = den(40)*den(67)
  den(69) = den(42)*den(67)
  den(70) = den(4)*den(67)
  den(71) = den(7)*den(67)
  den(72) = den(4)*den(45)
  den(73) = den(7)*den(45)
  den(74) = den(10)*den(40)
  den(75) = den(10)*den(42)
  den(76) = den(13)**2
  den(77) = den(40)*den(76)
  den(78) = den(42)*den(76)
  den(79) = den(15)*den(59)
  den(80) = den(17)*den(59)
  den(81) = den(19)*den(40)
  den(82) = den(19)*den(42)
  den(83) = den(15)*den(76)
  den(84) = den(17)*den(76)
  den(85) = den(22)**2
  den(86) = den(49)*den(85)
  den(87) = den(51)*den(85)
  den(88) = den(4)*den(85)
  den(89) = den(7)*den(85)
  den(90) = den(4)*den(53)
  den(91) = den(7)*den(53)
  den(92) = den(26)*den(49)
  den(93) = den(26)*den(51)
  den(94) = den(29)**2
  den(95) = den(49)*den(94)
  den(96) = den(51)*den(94)
  den(97) = den(15)*den(64)
  den(98) = den(17)*den(64)
  den(99) = den(32)*den(49)
  den(100) = den(32)*den(51)
  den(101) = den(15)*den(94)
  den(102) = den(17)*den(94)
  den(104) = den(15)*den(103)
  den(105) = den(17)*den(103)
  den(107) = den(15)*den(106)
  den(108) = den(17)*den(106)
  den(110) = den(4)*den(109)
  den(111) = den(7)*den(109)
  den(113) = den(4)*den(112)
  den(114) = den(7)*den(112)
  den(115) = den(49)*den(103)
  den(116) = den(51)*den(103)
  den(117) = den(49)*den(109)
  den(118) = den(51)*den(109)
  den(119) = den(40)*den(106)
  den(120) = den(42)*den(106)
  den(121) = den(40)*den(112)
  den(122) = den(42)*den(112)
  den(123) = den(1)*den(2)*den(29)
  den(124) = den(2)*den(6)*den(29)
  den(125) = den(1)*den(2)
  den(126) = den(1)*den(13)*den(22)
  den(127) = den(6)*den(13)*den(22)
  den(128) = den(1)*den(13)
  den(129) = den(6)*den(13)
  den(130) = den(1)*den(22)
  den(131) = den(1)*den(29)
  den(132) = den(6)*den(29)
  den(133) = den(2)*den(113)
  den(134) = den(2)*den(114)
  den(135) = den(2)*den(121)
  den(136) = den(2)*den(122)
  den(137) = den(13)*den(107)
  den(138) = den(13)*den(108)
  den(139) = den(13)*den(119)
  den(140) = den(13)*den(120)
  den(141) = den(22)*den(110)
  den(142) = den(22)*den(111)
  den(143) = den(22)*den(117)
  den(144) = den(22)*den(118)
  den(145) = den(29)*den(104)
  den(146) = den(29)*den(105)
  den(147) = den(29)*den(115)
  den(148) = den(29)*den(116)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(98)

  A(1) = cont_QA(wf(:,4),wf(:,5)) * den(5)
  A(2) = cont_QA(wf(:,4),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,11),wf(:,12)) * den(11)
  A(4) = cont_QA(wf(:,12),wf(:,13)) * den(12)
  A(5) = cont_QA(wf(:,16),wf(:,17)) * den(16)
  A(6) = cont_QA(wf(:,16),wf(:,19)) * den(18)
  A(7) = cont_QA(wf(:,11),wf(:,21)) * den(20)
  A(8) = cont_QA(wf(:,13),wf(:,21)) * den(21)
  A(9) = cont_QA(wf(:,5),wf(:,23)) * den(23)
  A(10) = cont_QA(wf(:,9),wf(:,23)) * den(24)
  A(11) = cont_QA(wf(:,25),wf(:,26)) * den(27)
  A(12) = cont_QA(wf(:,26),wf(:,27)) * den(28)
  A(13) = cont_QA(wf(:,17),wf(:,29)) * den(30)
  A(14) = cont_QA(wf(:,19),wf(:,29)) * den(31)
  A(15) = cont_QA(wf(:,25),wf(:,31)) * den(33)
  A(16) = cont_QA(wf(:,27),wf(:,31)) * den(34)

  A(17) = cont_VV(wf(:,28),wf(:,32)) * den(36)
  A(18) = cont_QA(wf(:,5),wf(:,33)) * den(5)
  A(19) = cont_QA(wf(:,9),wf(:,33)) * den(8)
  A(20) = cont_QA(wf(:,12),wf(:,34)) * den(11)
  A(21) = cont_QA(wf(:,12),wf(:,35)) * den(12)
  A(22) = cont_VV(wf(:,14),wf(:,36)) * den(38)
  A(23) = cont_QA(wf(:,17),wf(:,37)) * den(16)
  A(24) = cont_QA(wf(:,19),wf(:,37)) * den(18)
  A(25) = cont_QA(wf(:,21),wf(:,34)) * den(20)
  A(26) = cont_QA(wf(:,21),wf(:,35)) * den(21)
  A(27) = cont_QA(wf(:,5),wf(:,38)) * den(23)
  A(28) = cont_QA(wf(:,9),wf(:,38)) * den(24)
  A(29) = cont_QA(wf(:,26),wf(:,39)) * den(27)
  A(30) = cont_QA(wf(:,26),wf(:,40)) * den(28)
  A(31) = cont_QA(wf(:,17),wf(:,41)) * den(30)
  A(32) = cont_QA(wf(:,19),wf(:,41)) * den(31)
  A(33) = cont_QA(wf(:,31),wf(:,39)) * den(33)
  A(34) = cont_QA(wf(:,31),wf(:,40)) * den(34)
  A(35) = cont_QA(wf(:,42),wf(:,43)) * den(41)
  A(36) = cont_QA(wf(:,42),wf(:,44)) * den(43)
  A(37) = cont_QA(wf(:,45),wf(:,46)) * den(46)
  A(38) = cont_QA(wf(:,46),wf(:,47)) * den(47)
  A(39) = cont_QA(wf(:,48),wf(:,49)) * den(50)
  A(40) = cont_QA(wf(:,48),wf(:,50)) * den(52)
  A(41) = cont_QA(wf(:,45),wf(:,51)) * den(54)
  A(42) = cont_QA(wf(:,47),wf(:,51)) * den(55)
  A(43) = cont_QA(wf(:,17),wf(:,53)) * den(30)
  A(44) = cont_QA(wf(:,19),wf(:,53)) * den(31)
  A(45) = cont_QA(wf(:,25),wf(:,55)) * den(33)
  A(46) = cont_QA(wf(:,27),wf(:,55)) * den(34)
  A(47) = cont_QA(wf(:,17),wf(:,57)) * den(16)
  A(48) = cont_QA(wf(:,19),wf(:,57)) * den(18)
  A(49) = cont_QA(wf(:,11),wf(:,59)) * den(20)
  A(50) = cont_QA(wf(:,13),wf(:,59)) * den(21)
  A(51) = cont_QA(wf(:,43),wf(:,60)) * den(56)
  A(52) = cont_QA(wf(:,44),wf(:,60)) * den(57)
  A(53) = cont_QA(wf(:,61),wf(:,62)) * den(60)
  A(54) = cont_QA(wf(:,62),wf(:,63)) * den(61)
  A(55) = cont_QA(wf(:,49),wf(:,64)) * den(62)
  A(56) = cont_QA(wf(:,50),wf(:,64)) * den(63)
  A(57) = cont_QA(wf(:,61),wf(:,65)) * den(65)
  A(58) = cont_QA(wf(:,63),wf(:,65)) * den(66)
  A(59) = cont_QA(wf(:,5),wf(:,67)) * den(23)
  A(60) = cont_QA(wf(:,9),wf(:,67)) * den(24)
  A(61) = cont_QA(wf(:,25),wf(:,69)) * den(27)
  A(62) = cont_QA(wf(:,27),wf(:,69)) * den(28)
  A(63) = cont_QA(wf(:,5),wf(:,71)) * den(5)
  A(64) = cont_QA(wf(:,9),wf(:,71)) * den(8)
  A(65) = cont_QA(wf(:,11),wf(:,73)) * den(11)
  A(66) = cont_QA(wf(:,13),wf(:,73)) * den(12)
  A(67) = cont_QA(wf(:,43),wf(:,75)) * den(68)
  A(68) = cont_QA(wf(:,44),wf(:,75)) * den(69)
  A(69) = cont_QA(wf(:,5),wf(:,76)) * den(70)
  A(70) = cont_QA(wf(:,9),wf(:,76)) * den(71)
  A(71) = cont_QA(wf(:,46),wf(:,77)) * den(72)
  A(72) = cont_QA(wf(:,46),wf(:,78)) * den(73)
  A(73) = cont_QA(wf(:,43),wf(:,79)) * den(74)
  A(74) = cont_QA(wf(:,44),wf(:,79)) * den(75)
  A(75) = cont_QA(wf(:,43),wf(:,81)) * den(77)
  A(76) = cont_QA(wf(:,44),wf(:,81)) * den(78)
  A(77) = cont_QA(wf(:,62),wf(:,82)) * den(79)
  A(78) = cont_QA(wf(:,62),wf(:,83)) * den(80)
  A(79) = cont_QA(wf(:,43),wf(:,84)) * den(81)
  A(80) = cont_QA(wf(:,44),wf(:,84)) * den(82)
  A(81) = cont_QA(wf(:,17),wf(:,85)) * den(83)
  A(82) = cont_QA(wf(:,19),wf(:,85)) * den(84)
  A(83) = cont_QA(wf(:,49),wf(:,87)) * den(86)
  A(84) = cont_QA(wf(:,50),wf(:,87)) * den(87)
  A(85) = cont_QA(wf(:,5),wf(:,88)) * den(88)
  A(86) = cont_QA(wf(:,9),wf(:,88)) * den(89)
  A(87) = cont_QA(wf(:,51),wf(:,77)) * den(90)
  A(88) = cont_QA(wf(:,51),wf(:,78)) * den(91)
  A(89) = cont_QA(wf(:,49),wf(:,89)) * den(92)
  A(90) = cont_QA(wf(:,50),wf(:,89)) * den(93)
  A(91) = cont_QA(wf(:,49),wf(:,91)) * den(95)
  A(92) = cont_QA(wf(:,50),wf(:,91)) * den(96)
  A(93) = cont_QA(wf(:,65),wf(:,82)) * den(97)
  A(94) = cont_QA(wf(:,65),wf(:,83)) * den(98)
  A(95) = cont_QA(wf(:,49),wf(:,92)) * den(99)
  A(96) = cont_QA(wf(:,50),wf(:,92)) * den(100)
  A(97) = cont_QA(wf(:,17),wf(:,93)) * den(101)
  A(98) = cont_QA(wf(:,19),wf(:,93)) * den(102)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(98)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(5)-A(7)-A(9)-A(11))*f(1))/6._/**/REALKIND+((-A(1)-A(3)-A(13)-A(15))*f(1))/2._/**/REALKIND+((-A(6)-A(8)-A(10) &
       -A(12))*f(2))/6._/**/REALKIND+((-A(2)-A(4)-A(14)-A(16))*f(2))/2._/**/REALKIND
  M1(2) = ((A(5)+A(7)+A(9)+A(11))*f(1))/2._/**/REALKIND+((A(1)+A(3)+A(13)+A(15))*f(1))/6._/**/REALKIND+((A(6)+A(8)+A(10) &
       +A(12))*f(2))/2._/**/REALKIND+((A(2)+A(4)+A(14)+A(16))*f(2))/6._/**/REALKIND

  M2(1) = ((A(75)+A(77)+A(79)+A(81)+A(83)+A(85)+A(87)+A(89))*f(3))/6._/**/REALKIND+((A(67)+A(69)+A(71)+A(73)+A(91)+A(93)+A(95) &
       +A(97))*f(3))/2._/**/REALKIND+((A(76)+A(78)+A(80)+A(82)+A(84)+A(86)+A(88)+A(90))*f(4))/6._/**/REALKIND+((A(68)+A(70)+A(72) &
       +A(74)+A(92)+A(94)+A(96)+A(98))*f(4))/2._/**/REALKIND+((-A(23)-A(27)-A(39)-A(47)-A(49)-A(51)-A(59) &
       -A(61))*f(5))/6._/**/REALKIND+((-A(18)-A(31)-A(35)-A(43)-A(45)-A(55)-A(63)-A(65))*f(5))/2._/**/REALKIND+((-A(24)-A(28) &
       -A(40)-A(48)-A(50)-A(52)-A(60)-A(62))*f(6))/6._/**/REALKIND+((-A(19)-A(32)-A(36)-A(44)-A(46)-A(56)-A(64) &
       -A(66))*f(6))/2._/**/REALKIND+((-A(25)-A(29)-A(41)-A(53))*f(7))/6._/**/REALKIND+((-A(20)-A(33)-A(37) &
       -A(57))*f(7))/2._/**/REALKIND+((-A(26)-A(30)-A(42)-A(54))*f(8))/6._/**/REALKIND+((-A(21)-A(34)-A(38) &
       -A(58))*f(8))/2._/**/REALKIND+(A(17)*f(9))/2._/**/REALKIND+(A(22)*f(9))/6._/**/REALKIND
  M2(2) = ((-A(75)-A(77)-A(79)-A(81)-A(83)-A(85)-A(87)-A(89))*f(3))/2._/**/REALKIND+((-A(67)-A(69)-A(71)-A(73)-A(91)-A(93)-A(95) &
       -A(97))*f(3))/6._/**/REALKIND+((-A(76)-A(78)-A(80)-A(82)-A(84)-A(86)-A(88)-A(90))*f(4))/2._/**/REALKIND+((-A(68)-A(70) &
       -A(72)-A(74)-A(92)-A(94)-A(96)-A(98))*f(4))/6._/**/REALKIND+((A(23)+A(27)+A(39)+A(47)+A(49)+A(51)+A(59) &
       +A(61))*f(5))/2._/**/REALKIND+((A(18)+A(31)+A(35)+A(43)+A(45)+A(55)+A(63)+A(65))*f(5))/6._/**/REALKIND+((A(24)+A(28)+A(40) &
       +A(48)+A(50)+A(52)+A(60)+A(62))*f(6))/2._/**/REALKIND+((A(19)+A(32)+A(36)+A(44)+A(46)+A(56)+A(64) &
       +A(66))*f(6))/6._/**/REALKIND+((A(25)+A(29)+A(41)+A(53))*f(7))/2._/**/REALKIND+((A(20)+A(33)+A(37) &
       +A(57))*f(7))/6._/**/REALKIND+((A(26)+A(30)+A(42)+A(54))*f(8))/2._/**/REALKIND+((A(21)+A(34)+A(38) &
       +A(58))*f(8))/6._/**/REALKIND-(A(17)*f(9))/6._/**/REALKIND-(A(22)*f(9))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pplljj_eexbbbxbx_1_/**/REALKIND
