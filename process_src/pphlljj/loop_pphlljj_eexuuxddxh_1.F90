
module ol_colourmatrix_pphlljj_eexuuxddxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(60,2), K2(2,2), KL(2,2)
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
  K1(19,:) = [   0,  -4]
  K1(20,:) = [  -4, -12]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   0,   4]
  K1(28,:) = [   4,   0]
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
  K1(39,:) = [   0,   4]
  K1(40,:) = [   4,   0]
  K1(41,:) = [   0,  -4]
  K1(42,:) = [  -4, -12]
  K1(43,:) = [  12,   4]
  K1(44,:) = [   4,  12]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]
  K1(47,:) = [   0,   0]
  K1(48,:) = [   0,   0]
  K1(49,:) = [   0,   0]
  K1(50,:) = [   0,   0]
  K1(51,:) = [   0,   0]
  K1(52,:) = [   0,   0]
  K1(53,:) = [   0,   0]
  K1(54,:) = [   0,   0]
  K1(55,:) = [   0,   0]
  K1(56,:) = [   0,   0]
  K1(57,:) = [   0,   0]
  K1(58,:) = [   0,   0]
  K1(59,:) = [   0,   0]
  K1(60,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphlljj_eexuuxddxh_1_/**/REALKIND



module ol_forced_parameters_pphlljj_eexuuxddxh_1_/**/REALKIND
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
end module ol_forced_parameters_pphlljj_eexuuxddxh_1_/**/REALKIND

module ol_loop_pphlljj_eexuuxddxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(22), c(29)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:100)
  ! denominators
  complex(REALKIND), save :: den(101)
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
    f( 1) = (CI*eQED**3*gQCD**2*lambdaHZZ*MW)/(cw**2*sw)
    f( 2) = (CI*countertermnorm*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 3) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 4) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 5) = (countertermnorm*ctZGG*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 6) = (CI*eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f( 7) = (eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw*2._/**/REALKIND)
    f( 8) = (eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f( 9) = (eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(10) = (2*eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(11) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MB*YB)/(3._/**/REALKIND*MQ2sum)
    f(12) = (2*CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MB*YB)/(3._/**/REALKIND*MQ2sum)
    f(13) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MB*YB)/MQ2sum
    f(14) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw*6._/**/REALKIND)
    f(15) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw*3._/**/REALKIND)
    f(16) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(17) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MT*YT)/(3._/**/REALKIND*MQ2sum)
    f(18) = (2*CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MT*YT)/(3._/**/REALKIND*MQ2sum)
    f(19) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MT*YT)/MQ2sum
    f(20) = (eQED**3*gQCD**4*integralnorm*SwF*YT)/(MW*sw*6._/**/REALKIND)
    f(21) = (eQED**3*gQCD**4*integralnorm*SwF*YT)/(MW*sw*3._/**/REALKIND)
    f(22) = (eQED**3*gQCD**4*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

  c = [ 9*CI*f(6), 27*CI*f(6), 18*f(7), 54*f(7), f(8), 3*f(8), 6*f(8), 8*f(8), 10*f(8), 18*f(8), 21*f(8), 24*f(8), 54*f(8), 3*f(9) &
    , 9*f(9), 3*f(10), 9*f(10), 3*f(14), 9*f(14), 3*f(15), 9*f(15), 3*f(16), 9*f(16), 3*f(20), 9*f(20), 3*f(21), 9*f(21), 3*f(22) &
    , 9*f(22) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,7)
  integer,           intent(in)  :: H(7)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(41)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_S(P(:,7), rMH, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,3))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,4))
  call vert_SV_V(wf(:,-6),wf(:,3),wf(:,5))
  call prop_Q_A(wf(:,4),Q(:,28),ZERO,0_intkind1,wf(:,6))
  call prop_W_W(wf(:,5),Q(:,67),MZ,1_intkind1,wf(:,7))
  call vert_QA_Z(gZd,wf(:,6),wf(:,-5),wf(:,8))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,9))
  call prop_A_Q(wf(:,9),Q(:,44),ZERO,0_intkind1,wf(:,10))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,10),wf(:,11))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,12))
  call vert_VQ_A(wf(:,12),wf(:,-2),wf(:,13))
  call prop_Q_A(wf(:,13),Q(:,52),ZERO,0_intkind1,wf(:,14))
  call vert_QA_Z(gZu,wf(:,14),wf(:,-3),wf(:,15))
  call vert_AV_Q(wf(:,-3),wf(:,12),wf(:,16))
  call prop_A_Q(wf(:,16),Q(:,56),ZERO,0_intkind1,wf(:,17))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,17),wf(:,18))
  call counter_GG_V(wf(:,2),Q(:,12),wf(:,12),Q(:,48),wf(:,19))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,20))
  call vert_VQ_A(wf(:,20),wf(:,-4),wf(:,21))
  call counter_SG_G(wf(:,-6),wf(:,2),wf(:,22))
  call prop_Q_A(wf(:,21),Q(:,19),ZERO,0_intkind1,wf(:,23))
  call vert_QA_V(wf(:,23),wf(:,-5),wf(:,24))
  call vert_ZQ_A(gZd,wf(:,3),wf(:,-4),wf(:,25))
  call prop_Q_A(wf(:,25),Q(:,19),ZERO,0_intkind1,wf(:,26))
  call vert_QA_V(wf(:,26),wf(:,-5),wf(:,27))
  call vert_AV_Q(wf(:,-5),wf(:,20),wf(:,28))
  call prop_A_Q(wf(:,28),Q(:,35),ZERO,0_intkind1,wf(:,29))
  call vert_QA_V(wf(:,-4),wf(:,29),wf(:,30))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,3),wf(:,31))
  call prop_A_Q(wf(:,31),Q(:,35),ZERO,0_intkind1,wf(:,32))
  call vert_QA_V(wf(:,-4),wf(:,32),wf(:,33))
  call counter_QA_Z(gZd,wf(:,6),wf(:,-5),wf(:,34))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,35))
  call prop_A_Q(wf(:,35),Q(:,44),ZERO,0_intkind1,wf(:,36))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,36),wf(:,37))
  call counter_QA_Z(gZd,wf(:,-4),wf(:,10),wf(:,38))
  call counter_VQ_A(wf(:,2),wf(:,-4),wf(:,39))
  call prop_Q_A(wf(:,39),Q(:,28),ZERO,0_intkind1,wf(:,40))
  call vert_QA_Z(gZd,wf(:,40),wf(:,-5),wf(:,41))
  call vert_VQ_A(wf(:,20),wf(:,-2),wf(:,42))
  call counter_SG_G(wf(:,-6),wf(:,12),wf(:,43))
  call prop_Q_A(wf(:,42),Q(:,7),ZERO,0_intkind1,wf(:,44))
  call vert_QA_V(wf(:,44),wf(:,-3),wf(:,45))
  call vert_ZQ_A(gZu,wf(:,3),wf(:,-2),wf(:,46))
  call prop_Q_A(wf(:,46),Q(:,7),ZERO,0_intkind1,wf(:,47))
  call vert_QA_V(wf(:,47),wf(:,-3),wf(:,48))
  call vert_AV_Q(wf(:,-3),wf(:,20),wf(:,49))
  call prop_A_Q(wf(:,49),Q(:,11),ZERO,0_intkind1,wf(:,50))
  call vert_QA_V(wf(:,-2),wf(:,50),wf(:,51))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,3),wf(:,52))
  call prop_A_Q(wf(:,52),Q(:,11),ZERO,0_intkind1,wf(:,53))
  call vert_QA_V(wf(:,-2),wf(:,53),wf(:,54))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,55))
  call vert_AV_Q(wf(:,-3),wf(:,55),wf(:,56))
  call prop_A_Q(wf(:,56),Q(:,56),ZERO,0_intkind1,wf(:,57))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,57),wf(:,58))
  call vert_VQ_A(wf(:,55),wf(:,-2),wf(:,59))
  call prop_Q_A(wf(:,59),Q(:,52),ZERO,0_intkind1,wf(:,60))
  call vert_QA_Z(gZu,wf(:,60),wf(:,-3),wf(:,61))
  call counter_QA_Z(gZu,wf(:,14),wf(:,-3),wf(:,62))
  call counter_AV_Q(wf(:,-3),wf(:,12),wf(:,63))
  call prop_A_Q(wf(:,63),Q(:,56),ZERO,0_intkind1,wf(:,64))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,64),wf(:,65))
  call counter_VQ_A(wf(:,12),wf(:,-2),wf(:,66))
  call prop_Q_A(wf(:,66),Q(:,52),ZERO,0_intkind1,wf(:,67))
  call vert_QA_Z(gZu,wf(:,67),wf(:,-3),wf(:,68))
  call counter_QA_Z(gZu,wf(:,-2),wf(:,17),wf(:,69))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,70))
  call vert_VQ_A(wf(:,70),wf(:,-4),wf(:,71))
  call prop_Q_A(wf(:,71),Q(:,28),ZERO,0_intkind1,wf(:,72))
  call vert_QA_Z(gZd,wf(:,72),wf(:,-5),wf(:,73))
  call vert_AV_Q(wf(:,-5),wf(:,70),wf(:,74))
  call prop_A_Q(wf(:,74),Q(:,44),ZERO,0_intkind1,wf(:,75))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,75),wf(:,76))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,77))
  call vert_VQ_A(wf(:,77),wf(:,-4),wf(:,78))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,7),wf(:,79))
  call prop_Q_A(wf(:,78),Q(:,28),ZERO,0_intkind1,wf(:,80))
  call vert_ZQ_A(gZd,wf(:,7),wf(:,-4),wf(:,81))
  call vert_AV_Q(wf(:,-5),wf(:,77),wf(:,82))
  call prop_Q_A(wf(:,81),Q(:,83),ZERO,0_intkind1,wf(:,83))
  call counter_Q_A(ctqq,wf(:,6),Q(:,28),wf(:,84))
  call prop_A_Q(wf(:,79),Q(:,99),ZERO,0_intkind1,wf(:,85))
  call counter_A_Q(ctqq,wf(:,10),Q(:,44),wf(:,86))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,7),wf(:,87))
  call counter_Q_A(ctqq,wf(:,14),Q(:,52),wf(:,88))
  call prop_A_Q(wf(:,87),Q(:,75),ZERO,0_intkind1,wf(:,89))
  call vert_ZQ_A(gZu,wf(:,7),wf(:,-2),wf(:,90))
  call counter_A_Q(ctqq,wf(:,17),Q(:,56),wf(:,91))
  call prop_Q_A(wf(:,90),Q(:,71),ZERO,0_intkind1,wf(:,92))
  call counter_V_V(ctGG,wf(:,12),Q(:,48),wf(:,93))
  call vert_VQ_A(wf(:,93),wf(:,-2),wf(:,94))
  call prop_Q_A(wf(:,94),Q(:,52),ZERO,0_intkind1,wf(:,95))
  call vert_AV_Q(wf(:,-3),wf(:,93),wf(:,96))
  call vert_QA_V(wf(:,92),wf(:,-3),wf(:,97))
  call vert_QA_V(wf(:,-2),wf(:,89),wf(:,98))
  call vert_QA_V(wf(:,83),wf(:,-5),wf(:,99))
  call vert_QA_V(wf(:,-4),wf(:,85),wf(:,100))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,12))
  den(2) = 1 / (Q(5,3) - MZ2)
  den(3) = 1 / (Q(5,28))
  den(5) = 1 / (Q(5,67) - MZ2)
  den(8) = 1 / (Q(5,44))
  den(11) = 1 / (Q(5,48))
  den(12) = 1 / (Q(5,52))
  den(15) = 1 / (Q(5,56))
  den(20) = 1 / (Q(5,3))
  den(21) = 1 / (Q(5,19))
  den(23) = 1 / (Q(5,76))
  den(28) = 1 / (Q(5,35))
  den(33) = 1 / (Q(5,7))
  den(35) = 1 / (Q(5,112))
  den(40) = 1 / (Q(5,11))
  den(48) = 1 / (Q(5,83))
  den(51) = 1 / (Q(5,99))
  den(55) = 1 / (Q(5,75))
  den(58) = 1 / (Q(5,71))
  den(65) = 1 / (Q(5,51))
  den(70) = 1 / (Q(5,15))
  den(75) = 1 / (Q(5,79))
  den(78) = 1 / (Q(5,115))

  ! denominators
  den(4) = den(1)*den(3)
  den(6) = den(2)*den(5)
  den(7) = den(4)*den(6)
  den(9) = den(1)*den(8)
  den(10) = den(6)*den(9)
  den(13) = den(11)*den(12)
  den(14) = den(6)*den(13)
  den(16) = den(11)*den(15)
  den(17) = den(6)*den(16)
  den(18) = den(1)*den(11)
  den(19) = den(6)*den(18)
  den(22) = den(20)*den(21)
  den(24) = den(1)*den(23)
  den(25) = den(22)*den(24)
  den(26) = den(2)*den(21)
  den(27) = den(24)*den(26)
  den(29) = den(20)*den(28)
  den(30) = den(24)*den(29)
  den(31) = den(2)*den(28)
  den(32) = den(24)*den(31)
  den(34) = den(20)*den(33)
  den(36) = den(11)*den(35)
  den(37) = den(34)*den(36)
  den(38) = den(2)*den(33)
  den(39) = den(36)*den(38)
  den(41) = den(20)*den(40)
  den(42) = den(36)*den(41)
  den(43) = den(2)*den(40)
  den(44) = den(36)*den(43)
  den(45) = den(1)**2
  den(46) = den(3)*den(45)
  den(47) = den(6)*den(46)
  den(49) = den(6)*den(48)
  den(50) = den(45)*den(49)
  den(52) = den(6)*den(51)
  den(53) = den(4)*den(52)
  den(54) = den(9)*den(49)
  den(56) = den(6)*den(55)
  den(57) = den(13)*den(56)
  den(59) = den(6)*den(58)
  den(60) = den(16)*den(59)
  den(61) = den(11)**2
  den(62) = den(12)*den(61)
  den(63) = den(6)*den(62)
  den(64) = den(59)*den(61)
  den(66) = den(22)*den(65)
  den(67) = den(26)*den(65)
  den(68) = den(29)*den(65)
  den(69) = den(31)*den(65)
  den(71) = den(34)*den(70)
  den(72) = den(38)*den(70)
  den(73) = den(41)*den(70)
  den(74) = den(43)*den(70)
  den(76) = den(59)*den(75)
  den(77) = den(56)*den(75)
  den(79) = den(49)*den(78)
  den(80) = den(52)*den(78)
  den(81) = den(1)*den(6)*den(11)
  den(82) = den(1)*den(11)*den(20)
  den(83) = den(1)*den(2)*den(11)
  den(84) = den(1)*den(66)
  den(85) = den(1)*den(67)
  den(86) = den(1)*den(68)
  den(87) = den(1)*den(69)
  den(88) = den(1)*den(49)
  den(89) = den(1)*den(52)
  den(90) = den(1)*den(6)
  den(91) = den(11)*den(71)
  den(92) = den(11)*den(72)
  den(93) = den(11)*den(73)
  den(94) = den(11)*den(74)
  den(95) = den(11)*den(59)
  den(96) = den(11)*den(56)
  den(97) = den(6)*den(11)
  den(98) = den(1)*den(79)
  den(99) = den(1)*den(80)
  den(100) = den(11)*den(76)
  den(101) = den(11)*den(77)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(41)

  A(1) = cont_VV(wf(:,7),wf(:,8)) * den(7)
  A(2) = cont_VV(wf(:,7),wf(:,11)) * den(10)
  A(3) = cont_VV(wf(:,7),wf(:,15)) * den(14)
  A(4) = cont_VV(wf(:,7),wf(:,18)) * den(17)

  A(5) = cont_VV(wf(:,7),wf(:,19)) * den(19)
  A(6) = cont_VV(wf(:,22),wf(:,24)) * den(25)
  A(7) = cont_VV(wf(:,22),wf(:,24)) * den(25)
  A(8) = cont_VV(wf(:,22),wf(:,27)) * den(27)
  A(9) = cont_VV(wf(:,22),wf(:,27)) * den(27)
  A(10) = cont_VV(wf(:,22),wf(:,30)) * den(30)
  A(11) = cont_VV(wf(:,22),wf(:,30)) * den(30)
  A(12) = cont_VV(wf(:,22),wf(:,33)) * den(32)
  A(13) = cont_VV(wf(:,22),wf(:,33)) * den(32)
  A(14) = cont_VV(wf(:,7),wf(:,34)) * den(7)
  A(15) = cont_VV(wf(:,7),wf(:,37)) * den(10)
  A(16) = cont_VV(wf(:,7),wf(:,38)) * den(10)
  A(17) = cont_VV(wf(:,7),wf(:,41)) * den(7)
  A(18) = cont_VV(wf(:,43),wf(:,45)) * den(37)
  A(19) = cont_VV(wf(:,43),wf(:,45)) * den(37)
  A(20) = cont_VV(wf(:,43),wf(:,48)) * den(39)
  A(21) = cont_VV(wf(:,43),wf(:,48)) * den(39)
  A(22) = cont_VV(wf(:,43),wf(:,51)) * den(42)
  A(23) = cont_VV(wf(:,43),wf(:,51)) * den(42)
  A(24) = cont_VV(wf(:,43),wf(:,54)) * den(44)
  A(25) = cont_VV(wf(:,43),wf(:,54)) * den(44)
  A(26) = cont_VV(wf(:,7),wf(:,58)) * den(17)
  A(27) = cont_VV(wf(:,7),wf(:,61)) * den(14)
  A(28) = cont_VV(wf(:,7),wf(:,62)) * den(14)
  A(29) = cont_VV(wf(:,7),wf(:,65)) * den(17)
  A(30) = cont_VV(wf(:,7),wf(:,68)) * den(14)
  A(31) = cont_VV(wf(:,7),wf(:,69)) * den(17)
  A(32) = cont_VV(wf(:,7),wf(:,73)) * den(7)
  A(33) = cont_VV(wf(:,7),wf(:,76)) * den(10)
  A(34) = cont_QA(wf(:,79),wf(:,80)) * den(47)
  A(35) = cont_QA(wf(:,82),wf(:,83)) * den(50)
  A(36) = cont_QA(wf(:,84),wf(:,85)) * den(53)
  A(37) = cont_QA(wf(:,83),wf(:,86)) * den(54)
  A(38) = cont_QA(wf(:,88),wf(:,89)) * den(57)
  A(39) = cont_QA(wf(:,91),wf(:,92)) * den(60)
  A(40) = cont_QA(wf(:,87),wf(:,95)) * den(63)
  A(41) = cont_QA(wf(:,92),wf(:,96)) * den(64)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(41)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(1)+A(2)+A(3)+A(4))*f(1))/2._/**/REALKIND
  M1(2) = ((-A(1)-A(2)-A(3)-A(4))*f(1))/6._/**/REALKIND

  M2(1) = ((-A(34)-A(35)-A(36)-A(37)-A(38)-A(39)-A(40)-A(41))*f(2))/2._/**/REALKIND+((A(15)+A(17)+A(26)+A(27)+A(29)+A(30)+A(32) &
       +A(33))*f(3))/2._/**/REALKIND+((A(14)+A(16)+A(28)+A(31))*f(4))/2._/**/REALKIND-(A(5)*f(5))/2._/**/REALKIND+((A(7) &
       +A(11))*f(11))/2._/**/REALKIND+((-A(19)-A(23))*f(12))/2._/**/REALKIND+((A(9)+A(13)+A(21)+A(25))*f(13))/2._/**/REALKIND &
       +((A(6)+A(10))*f(17))/2._/**/REALKIND+((-A(18)-A(22))*f(18))/2._/**/REALKIND+((A(8)+A(12)+A(20) &
       +A(24))*f(19))/2._/**/REALKIND
  M2(2) = ((A(34)+A(35)+A(36)+A(37)+A(38)+A(39)+A(40)+A(41))*f(2))/6._/**/REALKIND+((-A(15)-A(17)-A(26)-A(27)-A(29)-A(30)-A(32) &
       -A(33))*f(3))/6._/**/REALKIND+((-A(14)-A(16)-A(28)-A(31))*f(4))/6._/**/REALKIND+(A(5)*f(5))/6._/**/REALKIND+((-A(7) &
       -A(11))*f(11))/6._/**/REALKIND+((A(19)+A(23))*f(12))/6._/**/REALKIND+((-A(9)-A(13)-A(21)-A(25))*f(13))/6._/**/REALKIND+(( &
       -A(6)-A(10))*f(17))/6._/**/REALKIND+((A(18)+A(22))*f(18))/6._/**/REALKIND+((-A(8)-A(12)-A(20) &
       -A(24))*f(19))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pphlljj_eexuuxddxh_1_/**/REALKIND
