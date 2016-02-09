
module ol_colourmatrix_pphlljj_nenexuuuxuxh_1_/**/REALKIND
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
end module ol_colourmatrix_pphlljj_nenexuuuxuxh_1_/**/REALKIND



module ol_forced_parameters_pphlljj_nenexuuuxuxh_1_/**/REALKIND
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
end module ol_forced_parameters_pphlljj_nenexuuuxuxh_1_/**/REALKIND

module ol_loop_pphlljj_nenexuuuxuxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(14), c(21)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:154)
  ! denominators
  complex(REALKIND), save :: den(143)
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
    f(11) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MB*YB)/MQ2sum
    f(12) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(13) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MT*YT)/MQ2sum
    f(14) = (eQED**3*gQCD**4*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

  c = [ 9*CI*f(6), 27*CI*f(6), 18*f(7), 54*f(7), f(8), 3*f(8), 6*f(8), 8*f(8), 10*f(8), 18*f(8), 21*f(8), 24*f(8), 54*f(8), 3*f(9) &
    , 9*f(9), 3*f(10), 9*f(10), 3*f(12), 9*f(12), 3*f(14), 9*f(14) ]
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
  complex(REALKIND) :: A(66)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_Q(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_A(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_S(P(:,7), rMH, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-4),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,3))
  call vert_VQ_A(wf(:,2),wf(:,-3),wf(:,4))
  call vert_SV_V(wf(:,-6),wf(:,3),wf(:,5))
  call prop_Q_A(wf(:,4),Q(:,28),ZERO,0_intkind1,wf(:,6))
  call prop_W_W(wf(:,5),Q(:,67),MZ,1_intkind1,wf(:,7))
  call vert_QA_Z(gZu,wf(:,6),wf(:,-5),wf(:,8))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,9))
  call prop_A_Q(wf(:,9),Q(:,52),ZERO,0_intkind1,wf(:,10))
  call vert_QA_Z(gZu,wf(:,-3),wf(:,10),wf(:,11))
  call vert_QA_V(wf(:,-3),wf(:,-4),wf(:,12))
  call vert_VQ_A(wf(:,12),wf(:,-2),wf(:,13))
  call prop_Q_A(wf(:,13),Q(:,28),ZERO,0_intkind1,wf(:,14))
  call vert_QA_Z(gZu,wf(:,14),wf(:,-5),wf(:,15))
  call vert_AV_Q(wf(:,-5),wf(:,12),wf(:,16))
  call prop_A_Q(wf(:,16),Q(:,56),ZERO,0_intkind1,wf(:,17))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,17),wf(:,18))
  call vert_QA_V(wf(:,-2),wf(:,-5),wf(:,19))
  call vert_VQ_A(wf(:,19),wf(:,-3),wf(:,20))
  call prop_Q_A(wf(:,20),Q(:,44),ZERO,0_intkind1,wf(:,21))
  call vert_QA_Z(gZu,wf(:,21),wf(:,-4),wf(:,22))
  call vert_AV_Q(wf(:,-4),wf(:,19),wf(:,23))
  call prop_A_Q(wf(:,23),Q(:,52),ZERO,0_intkind1,wf(:,24))
  call vert_QA_Z(gZu,wf(:,-3),wf(:,24),wf(:,25))
  call vert_QA_V(wf(:,-3),wf(:,-5),wf(:,26))
  call vert_VQ_A(wf(:,26),wf(:,-2),wf(:,27))
  call prop_Q_A(wf(:,27),Q(:,44),ZERO,0_intkind1,wf(:,28))
  call vert_QA_Z(gZu,wf(:,28),wf(:,-4),wf(:,29))
  call vert_AV_Q(wf(:,-4),wf(:,26),wf(:,30))
  call prop_A_Q(wf(:,30),Q(:,56),ZERO,0_intkind1,wf(:,31))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,31),wf(:,32))
  call counter_GG_V(wf(:,2),Q(:,20),wf(:,26),Q(:,40),wf(:,33))
  call vert_ZQ_A(gZu,wf(:,3),wf(:,-3),wf(:,34))
  call counter_SG_G(wf(:,-6),wf(:,2),wf(:,35))
  call prop_Q_A(wf(:,34),Q(:,11),ZERO,0_intkind1,wf(:,36))
  call vert_QA_V(wf(:,36),wf(:,-5),wf(:,37))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,3),wf(:,38))
  call prop_A_Q(wf(:,38),Q(:,35),ZERO,0_intkind1,wf(:,39))
  call vert_QA_V(wf(:,-3),wf(:,39),wf(:,40))
  call counter_QA_Z(gZu,wf(:,6),wf(:,-5),wf(:,41))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,42))
  call prop_A_Q(wf(:,42),Q(:,52),ZERO,0_intkind1,wf(:,43))
  call vert_QA_Z(gZu,wf(:,-3),wf(:,43),wf(:,44))
  call counter_GG_V(wf(:,12),Q(:,24),wf(:,19),Q(:,36),wf(:,45))
  call vert_ZQ_A(gZu,wf(:,3),wf(:,-2),wf(:,46))
  call counter_SG_G(wf(:,-6),wf(:,12),wf(:,47))
  call prop_Q_A(wf(:,46),Q(:,7),ZERO,0_intkind1,wf(:,48))
  call vert_QA_V(wf(:,48),wf(:,-5),wf(:,49))
  call vert_QA_V(wf(:,-2),wf(:,39),wf(:,50))
  call counter_QA_Z(gZu,wf(:,14),wf(:,-5),wf(:,51))
  call counter_AV_Q(wf(:,-5),wf(:,12),wf(:,52))
  call prop_A_Q(wf(:,52),Q(:,56),ZERO,0_intkind1,wf(:,53))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,53),wf(:,54))
  call counter_SG_G(wf(:,-6),wf(:,19),wf(:,55))
  call vert_QA_V(wf(:,36),wf(:,-4),wf(:,56))
  call vert_AZ_Q(gZu,wf(:,-4),wf(:,3),wf(:,57))
  call prop_A_Q(wf(:,57),Q(:,19),ZERO,0_intkind1,wf(:,58))
  call vert_QA_V(wf(:,-3),wf(:,58),wf(:,59))
  call counter_SG_G(wf(:,-6),wf(:,26),wf(:,60))
  call vert_QA_V(wf(:,48),wf(:,-4),wf(:,61))
  call vert_QA_V(wf(:,-2),wf(:,58),wf(:,62))
  call counter_QA_Z(gZu,wf(:,21),wf(:,-4),wf(:,63))
  call counter_AV_Q(wf(:,-4),wf(:,19),wf(:,64))
  call prop_A_Q(wf(:,64),Q(:,52),ZERO,0_intkind1,wf(:,65))
  call vert_QA_Z(gZu,wf(:,-3),wf(:,65),wf(:,66))
  call counter_QA_Z(gZu,wf(:,28),wf(:,-4),wf(:,67))
  call counter_AV_Q(wf(:,-4),wf(:,26),wf(:,68))
  call prop_A_Q(wf(:,68),Q(:,56),ZERO,0_intkind1,wf(:,69))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,69),wf(:,70))
  call counter_QA_Z(gZu,wf(:,-3),wf(:,10),wf(:,71))
  call counter_VQ_A(wf(:,2),wf(:,-3),wf(:,72))
  call prop_Q_A(wf(:,72),Q(:,28),ZERO,0_intkind1,wf(:,73))
  call vert_QA_Z(gZu,wf(:,73),wf(:,-5),wf(:,74))
  call counter_QA_Z(gZu,wf(:,-3),wf(:,24),wf(:,75))
  call counter_VQ_A(wf(:,19),wf(:,-3),wf(:,76))
  call prop_Q_A(wf(:,76),Q(:,44),ZERO,0_intkind1,wf(:,77))
  call vert_QA_Z(gZu,wf(:,77),wf(:,-4),wf(:,78))
  call counter_QA_V(wf(:,-3),wf(:,-5),wf(:,79))
  call vert_AV_Q(wf(:,-4),wf(:,79),wf(:,80))
  call prop_A_Q(wf(:,80),Q(:,56),ZERO,0_intkind1,wf(:,81))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,81),wf(:,82))
  call vert_VQ_A(wf(:,79),wf(:,-2),wf(:,83))
  call prop_Q_A(wf(:,83),Q(:,44),ZERO,0_intkind1,wf(:,84))
  call vert_QA_Z(gZu,wf(:,84),wf(:,-4),wf(:,85))
  call counter_QA_V(wf(:,-3),wf(:,-4),wf(:,86))
  call vert_AV_Q(wf(:,-5),wf(:,86),wf(:,87))
  call prop_A_Q(wf(:,87),Q(:,56),ZERO,0_intkind1,wf(:,88))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,88),wf(:,89))
  call vert_VQ_A(wf(:,86),wf(:,-2),wf(:,90))
  call prop_Q_A(wf(:,90),Q(:,28),ZERO,0_intkind1,wf(:,91))
  call vert_QA_Z(gZu,wf(:,91),wf(:,-5),wf(:,92))
  call counter_VQ_A(wf(:,12),wf(:,-2),wf(:,93))
  call prop_Q_A(wf(:,93),Q(:,28),ZERO,0_intkind1,wf(:,94))
  call vert_QA_Z(gZu,wf(:,94),wf(:,-5),wf(:,95))
  call counter_QA_Z(gZu,wf(:,-2),wf(:,17),wf(:,96))
  call counter_VQ_A(wf(:,26),wf(:,-2),wf(:,97))
  call prop_Q_A(wf(:,97),Q(:,44),ZERO,0_intkind1,wf(:,98))
  call vert_QA_Z(gZu,wf(:,98),wf(:,-4),wf(:,99))
  call counter_QA_Z(gZu,wf(:,-2),wf(:,31),wf(:,100))
  call counter_QA_V(wf(:,-2),wf(:,-5),wf(:,101))
  call vert_VQ_A(wf(:,101),wf(:,-3),wf(:,102))
  call prop_Q_A(wf(:,102),Q(:,44),ZERO,0_intkind1,wf(:,103))
  call vert_QA_Z(gZu,wf(:,103),wf(:,-4),wf(:,104))
  call vert_AV_Q(wf(:,-4),wf(:,101),wf(:,105))
  call prop_A_Q(wf(:,105),Q(:,52),ZERO,0_intkind1,wf(:,106))
  call vert_QA_Z(gZu,wf(:,-3),wf(:,106),wf(:,107))
  call counter_QA_V(wf(:,-2),wf(:,-4),wf(:,108))
  call vert_VQ_A(wf(:,108),wf(:,-3),wf(:,109))
  call prop_Q_A(wf(:,109),Q(:,28),ZERO,0_intkind1,wf(:,110))
  call vert_QA_Z(gZu,wf(:,110),wf(:,-5),wf(:,111))
  call vert_AV_Q(wf(:,-5),wf(:,108),wf(:,112))
  call prop_A_Q(wf(:,112),Q(:,52),ZERO,0_intkind1,wf(:,113))
  call vert_QA_Z(gZu,wf(:,-3),wf(:,113),wf(:,114))
  call counter_V_V(ctGG,wf(:,2),Q(:,20),wf(:,115))
  call vert_VQ_A(wf(:,115),wf(:,-3),wf(:,116))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,7),wf(:,117))
  call prop_Q_A(wf(:,116),Q(:,28),ZERO,0_intkind1,wf(:,118))
  call vert_ZQ_A(gZu,wf(:,7),wf(:,-3),wf(:,119))
  call vert_AV_Q(wf(:,-5),wf(:,115),wf(:,120))
  call prop_Q_A(wf(:,119),Q(:,75),ZERO,0_intkind1,wf(:,121))
  call counter_Q_A(ctqq,wf(:,6),Q(:,28),wf(:,122))
  call prop_A_Q(wf(:,117),Q(:,99),ZERO,0_intkind1,wf(:,123))
  call counter_A_Q(ctqq,wf(:,10),Q(:,52),wf(:,124))
  call counter_V_V(ctGG,wf(:,12),Q(:,24),wf(:,125))
  call vert_VQ_A(wf(:,125),wf(:,-2),wf(:,126))
  call prop_Q_A(wf(:,126),Q(:,28),ZERO,0_intkind1,wf(:,127))
  call counter_Q_A(ctqq,wf(:,14),Q(:,28),wf(:,128))
  call vert_ZQ_A(gZu,wf(:,7),wf(:,-2),wf(:,129))
  call counter_A_Q(ctqq,wf(:,17),Q(:,56),wf(:,130))
  call prop_Q_A(wf(:,129),Q(:,71),ZERO,0_intkind1,wf(:,131))
  call vert_AV_Q(wf(:,-5),wf(:,125),wf(:,132))
  call counter_V_V(ctGG,wf(:,19),Q(:,36),wf(:,133))
  call vert_VQ_A(wf(:,133),wf(:,-3),wf(:,134))
  call vert_AZ_Q(gZu,wf(:,-4),wf(:,7),wf(:,135))
  call prop_Q_A(wf(:,134),Q(:,44),ZERO,0_intkind1,wf(:,136))
  call vert_AV_Q(wf(:,-4),wf(:,133),wf(:,137))
  call counter_Q_A(ctqq,wf(:,21),Q(:,44),wf(:,138))
  call prop_A_Q(wf(:,135),Q(:,83),ZERO,0_intkind1,wf(:,139))
  call counter_A_Q(ctqq,wf(:,24),Q(:,52),wf(:,140))
  call counter_V_V(ctGG,wf(:,26),Q(:,40),wf(:,141))
  call vert_VQ_A(wf(:,141),wf(:,-2),wf(:,142))
  call prop_Q_A(wf(:,142),Q(:,44),ZERO,0_intkind1,wf(:,143))
  call counter_Q_A(ctqq,wf(:,28),Q(:,44),wf(:,144))
  call counter_A_Q(ctqq,wf(:,31),Q(:,56),wf(:,145))
  call vert_AV_Q(wf(:,-4),wf(:,141),wf(:,146))
  call vert_QA_V(wf(:,131),wf(:,-4),wf(:,147))
  call vert_QA_V(wf(:,-2),wf(:,139),wf(:,148))
  call vert_QA_V(wf(:,131),wf(:,-5),wf(:,149))
  call vert_QA_V(wf(:,-2),wf(:,123),wf(:,150))
  call vert_QA_V(wf(:,121),wf(:,-4),wf(:,151))
  call vert_QA_V(wf(:,-3),wf(:,139),wf(:,152))
  call vert_QA_V(wf(:,121),wf(:,-5),wf(:,153))
  call vert_QA_V(wf(:,-3),wf(:,123),wf(:,154))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,20))
  den(2) = 1 / (Q(5,3) - MZ2)
  den(3) = 1 / (Q(5,28))
  den(5) = 1 / (Q(5,67) - MZ2)
  den(8) = 1 / (Q(5,52))
  den(11) = 1 / (Q(5,24))
  den(14) = 1 / (Q(5,56))
  den(17) = 1 / (Q(5,36))
  den(18) = 1 / (Q(5,44))
  den(23) = 1 / (Q(5,40))
  den(30) = 1 / (Q(5,11))
  den(32) = 1 / (Q(5,84))
  den(35) = 1 / (Q(5,35))
  den(40) = 1 / (Q(5,7))
  den(42) = 1 / (Q(5,88))
  den(46) = 1 / (Q(5,100))
  den(49) = 1 / (Q(5,19))
  den(52) = 1 / (Q(5,104))
  den(59) = 1 / (Q(5,75))
  den(62) = 1 / (Q(5,99))
  den(70) = 1 / (Q(5,71))
  den(78) = 1 / (Q(5,83))
  den(88) = 1 / (Q(5,43))
  den(91) = 1 / (Q(5,39))
  den(94) = 1 / (Q(5,27))
  den(97) = 1 / (Q(5,23))
  den(100) = 1 / (Q(5,87))
  den(103) = 1 / (Q(5,103))
  den(106) = 1 / (Q(5,91))
  den(109) = 1 / (Q(5,107))

  ! denominators
  den(4) = den(1)*den(3)
  den(6) = den(2)*den(5)
  den(7) = den(4)*den(6)
  den(9) = den(1)*den(8)
  den(10) = den(6)*den(9)
  den(12) = den(3)*den(11)
  den(13) = den(6)*den(12)
  den(15) = den(11)*den(14)
  den(16) = den(6)*den(15)
  den(19) = den(17)*den(18)
  den(20) = den(6)*den(19)
  den(21) = den(8)*den(17)
  den(22) = den(6)*den(21)
  den(24) = den(18)*den(23)
  den(25) = den(6)*den(24)
  den(26) = den(14)*den(23)
  den(27) = den(6)*den(26)
  den(28) = den(1)*den(23)
  den(29) = den(6)*den(28)
  den(31) = den(2)*den(30)
  den(33) = den(1)*den(32)
  den(34) = den(31)*den(33)
  den(36) = den(2)*den(35)
  den(37) = den(33)*den(36)
  den(38) = den(11)*den(17)
  den(39) = den(6)*den(38)
  den(41) = den(2)*den(40)
  den(43) = den(11)*den(42)
  den(44) = den(41)*den(43)
  den(45) = den(36)*den(43)
  den(47) = den(17)*den(46)
  den(48) = den(31)*den(47)
  den(50) = den(2)*den(49)
  den(51) = den(47)*den(50)
  den(53) = den(23)*den(52)
  den(54) = den(41)*den(53)
  den(55) = den(50)*den(53)
  den(56) = den(1)**2
  den(57) = den(3)*den(56)
  den(58) = den(6)*den(57)
  den(60) = den(6)*den(59)
  den(61) = den(56)*den(60)
  den(63) = den(6)*den(62)
  den(64) = den(4)*den(63)
  den(65) = den(9)*den(60)
  den(66) = den(11)**2
  den(67) = den(3)*den(66)
  den(68) = den(6)*den(67)
  den(69) = den(12)*den(63)
  den(71) = den(6)*den(70)
  den(72) = den(15)*den(71)
  den(73) = den(66)*den(71)
  den(74) = den(17)**2
  den(75) = den(18)*den(74)
  den(76) = den(6)*den(75)
  den(77) = den(60)*den(74)
  den(79) = den(6)*den(78)
  den(80) = den(19)*den(79)
  den(81) = den(21)*den(60)
  den(82) = den(23)**2
  den(83) = den(18)*den(82)
  den(84) = den(6)*den(83)
  den(85) = den(24)*den(79)
  den(86) = den(26)*den(71)
  den(87) = den(71)*den(82)
  den(89) = den(31)*den(88)
  den(90) = den(36)*den(88)
  den(92) = den(41)*den(91)
  den(93) = den(36)*den(91)
  den(95) = den(31)*den(94)
  den(96) = den(50)*den(94)
  den(98) = den(41)*den(97)
  den(99) = den(50)*den(97)
  den(101) = den(71)*den(100)
  den(102) = den(79)*den(100)
  den(104) = den(71)*den(103)
  den(105) = den(63)*den(103)
  den(107) = den(60)*den(106)
  den(108) = den(79)*den(106)
  den(110) = den(60)*den(109)
  den(111) = den(63)*den(109)
  den(112) = den(1)*den(6)*den(23)
  den(113) = den(1)*den(2)*den(23)
  den(114) = den(1)*den(89)
  den(115) = den(1)*den(90)
  den(116) = den(1)*den(60)
  den(117) = den(1)*den(63)
  den(118) = den(1)*den(6)
  den(119) = den(6)*den(11)*den(17)
  den(120) = den(2)*den(11)*den(17)
  den(121) = den(11)*den(92)
  den(122) = den(11)*den(93)
  den(123) = den(11)*den(71)
  den(124) = den(11)*den(63)
  den(125) = den(6)*den(11)
  den(126) = den(17)*den(95)
  den(127) = den(17)*den(96)
  den(128) = den(17)*den(60)
  den(129) = den(17)*den(79)
  den(130) = den(6)*den(17)
  den(131) = den(23)*den(98)
  den(132) = den(23)*den(99)
  den(133) = den(23)*den(71)
  den(134) = den(23)*den(79)
  den(135) = den(6)*den(23)
  den(136) = den(1)*den(110)
  den(137) = den(1)*den(111)
  den(138) = den(11)*den(104)
  den(139) = den(11)*den(105)
  den(140) = den(17)*den(107)
  den(141) = den(17)*den(108)
  den(142) = den(23)*den(101)
  den(143) = den(23)*den(102)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(66)

  A(1) = cont_VV(wf(:,7),wf(:,8)) * den(7)
  A(2) = cont_VV(wf(:,7),wf(:,11)) * den(10)
  A(3) = cont_VV(wf(:,7),wf(:,15)) * den(13)
  A(4) = cont_VV(wf(:,7),wf(:,18)) * den(16)
  A(5) = cont_VV(wf(:,7),wf(:,22)) * den(20)
  A(6) = cont_VV(wf(:,7),wf(:,25)) * den(22)
  A(7) = cont_VV(wf(:,7),wf(:,29)) * den(25)
  A(8) = cont_VV(wf(:,7),wf(:,32)) * den(27)

  A(9) = cont_VV(wf(:,7),wf(:,33)) * den(29)
  A(10) = cont_VV(wf(:,35),wf(:,37)) * den(34)
  A(11) = cont_VV(wf(:,35),wf(:,37)) * den(34)
  A(12) = cont_VV(wf(:,35),wf(:,40)) * den(37)
  A(13) = cont_VV(wf(:,35),wf(:,40)) * den(37)
  A(14) = cont_VV(wf(:,7),wf(:,41)) * den(7)
  A(15) = cont_VV(wf(:,7),wf(:,44)) * den(10)
  A(16) = cont_VV(wf(:,7),wf(:,45)) * den(39)
  A(17) = cont_VV(wf(:,47),wf(:,49)) * den(44)
  A(18) = cont_VV(wf(:,47),wf(:,49)) * den(44)
  A(19) = cont_VV(wf(:,47),wf(:,50)) * den(45)
  A(20) = cont_VV(wf(:,47),wf(:,50)) * den(45)
  A(21) = cont_VV(wf(:,7),wf(:,51)) * den(13)
  A(22) = cont_VV(wf(:,7),wf(:,54)) * den(16)
  A(23) = cont_VV(wf(:,55),wf(:,56)) * den(48)
  A(24) = cont_VV(wf(:,55),wf(:,56)) * den(48)
  A(25) = cont_VV(wf(:,55),wf(:,59)) * den(51)
  A(26) = cont_VV(wf(:,55),wf(:,59)) * den(51)
  A(27) = cont_VV(wf(:,60),wf(:,61)) * den(54)
  A(28) = cont_VV(wf(:,60),wf(:,61)) * den(54)
  A(29) = cont_VV(wf(:,60),wf(:,62)) * den(55)
  A(30) = cont_VV(wf(:,60),wf(:,62)) * den(55)
  A(31) = cont_VV(wf(:,7),wf(:,63)) * den(20)
  A(32) = cont_VV(wf(:,7),wf(:,66)) * den(22)
  A(33) = cont_VV(wf(:,7),wf(:,67)) * den(25)
  A(34) = cont_VV(wf(:,7),wf(:,70)) * den(27)
  A(35) = cont_VV(wf(:,7),wf(:,71)) * den(10)
  A(36) = cont_VV(wf(:,7),wf(:,74)) * den(7)
  A(37) = cont_VV(wf(:,7),wf(:,75)) * den(22)
  A(38) = cont_VV(wf(:,7),wf(:,78)) * den(20)
  A(39) = cont_VV(wf(:,7),wf(:,82)) * den(27)
  A(40) = cont_VV(wf(:,7),wf(:,85)) * den(25)
  A(41) = cont_VV(wf(:,7),wf(:,89)) * den(16)
  A(42) = cont_VV(wf(:,7),wf(:,92)) * den(13)
  A(43) = cont_VV(wf(:,7),wf(:,95)) * den(13)
  A(44) = cont_VV(wf(:,7),wf(:,96)) * den(16)
  A(45) = cont_VV(wf(:,7),wf(:,99)) * den(25)
  A(46) = cont_VV(wf(:,7),wf(:,100)) * den(27)
  A(47) = cont_VV(wf(:,7),wf(:,104)) * den(20)
  A(48) = cont_VV(wf(:,7),wf(:,107)) * den(22)
  A(49) = cont_VV(wf(:,7),wf(:,111)) * den(7)
  A(50) = cont_VV(wf(:,7),wf(:,114)) * den(10)
  A(51) = cont_QA(wf(:,117),wf(:,118)) * den(58)
  A(52) = cont_QA(wf(:,120),wf(:,121)) * den(61)
  A(53) = cont_QA(wf(:,122),wf(:,123)) * den(64)
  A(54) = cont_QA(wf(:,121),wf(:,124)) * den(65)
  A(55) = cont_QA(wf(:,117),wf(:,127)) * den(68)
  A(56) = cont_QA(wf(:,123),wf(:,128)) * den(69)
  A(57) = cont_QA(wf(:,130),wf(:,131)) * den(72)
  A(58) = cont_QA(wf(:,131),wf(:,132)) * den(73)
  A(59) = cont_QA(wf(:,135),wf(:,136)) * den(76)
  A(60) = cont_QA(wf(:,121),wf(:,137)) * den(77)
  A(61) = cont_QA(wf(:,138),wf(:,139)) * den(80)
  A(62) = cont_QA(wf(:,121),wf(:,140)) * den(81)
  A(63) = cont_QA(wf(:,135),wf(:,143)) * den(84)
  A(64) = cont_QA(wf(:,139),wf(:,144)) * den(85)
  A(65) = cont_QA(wf(:,131),wf(:,145)) * den(86)
  A(66) = cont_QA(wf(:,131),wf(:,146)) * den(87)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(66)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(3)-A(4)-A(5)-A(6))*f(1))/6._/**/REALKIND+((-A(1)-A(2)-A(7)-A(8))*f(1))/2._/**/REALKIND
  M1(2) = ((A(3)+A(4)+A(5)+A(6))*f(1))/2._/**/REALKIND+((A(1)+A(2)+A(7)+A(8))*f(1))/6._/**/REALKIND

  M2(1) = ((A(55)+A(56)+A(57)+A(58)+A(59)+A(60)+A(61)+A(62))*f(2))/6._/**/REALKIND+((A(51)+A(52)+A(53)+A(54)+A(63)+A(64)+A(65) &
       +A(66))*f(2))/2._/**/REALKIND+((-A(22)-A(32)-A(38)-A(41)-A(42)-A(43)-A(47)-A(48))*f(3))/6._/**/REALKIND+((-A(15)-A(34) &
       -A(36)-A(39)-A(40)-A(45)-A(49)-A(50))*f(3))/2._/**/REALKIND+((-A(21)-A(31)-A(37)-A(44))*f(4))/6._/**/REALKIND+((-A(14) &
       -A(33)-A(35)-A(46))*f(4))/2._/**/REALKIND+(A(9)*f(5))/2._/**/REALKIND+(A(16)*f(5))/6._/**/REALKIND+((-A(18)-A(20)-A(24) &
       -A(26))*f(11))/6._/**/REALKIND+((-A(11)-A(13)-A(28)-A(30))*f(11))/2._/**/REALKIND+((-A(17)-A(19)-A(23) &
       -A(25))*f(13))/6._/**/REALKIND+((-A(10)-A(12)-A(27)-A(29))*f(13))/2._/**/REALKIND
  M2(2) = ((-A(55)-A(56)-A(57)-A(58)-A(59)-A(60)-A(61)-A(62))*f(2))/2._/**/REALKIND+((-A(51)-A(52)-A(53)-A(54)-A(63)-A(64)-A(65) &
       -A(66))*f(2))/6._/**/REALKIND+((A(22)+A(32)+A(38)+A(41)+A(42)+A(43)+A(47)+A(48))*f(3))/2._/**/REALKIND+((A(15)+A(34)+A(36) &
       +A(39)+A(40)+A(45)+A(49)+A(50))*f(3))/6._/**/REALKIND+((A(21)+A(31)+A(37)+A(44))*f(4))/2._/**/REALKIND+((A(14)+A(33)+A(35) &
       +A(46))*f(4))/6._/**/REALKIND-(A(9)*f(5))/6._/**/REALKIND-(A(16)*f(5))/2._/**/REALKIND+((A(18)+A(20)+A(24) &
       +A(26))*f(11))/2._/**/REALKIND+((A(11)+A(13)+A(28)+A(30))*f(11))/6._/**/REALKIND+((A(17)+A(19)+A(23) &
       +A(25))*f(13))/2._/**/REALKIND+((A(10)+A(12)+A(27)+A(29))*f(13))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pphlljj_nenexuuuxuxh_1_/**/REALKIND
