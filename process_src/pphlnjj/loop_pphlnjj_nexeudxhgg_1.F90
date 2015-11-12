
module ol_colourmatrix_pphlnjj_nexeudxhgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(60,2), K2(2,2), KL(2,3)
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
  K1(37,:) = [   9,   9]
  K1(38,:) = [   9, -72]
  K1(39,:) = [ -72,   9]
  K1(40,:) = [   9,   9]
  K1(41,:) = [   0,   0]
  K1(42,:) = [   0,   0]
  K1(43,:) = [ 144, -18]
  K1(44,:) = [ -18, 144]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]
  K1(47,:) = [   0,   0]
  K1(48,:) = [   0,   0]
  K1(49,:) = [ -72,   9]
  K1(50,:) = [   9,   9]
  K1(51,:) = [   9,   9]
  K1(52,:) = [   9, -72]
  K1(53,:) = [   0,   0]
  K1(54,:) = [   0,   0]
  K1(55,:) = [ -81,   0]
  K1(56,:) = [   0, -81]
  K1(57,:) = [ 144, -18]
  K1(58,:) = [ -18, 144]
  K1(59,:) = [   0,   0]
  K1(60,:) = [   0,   0]
  K1 = (1._/**/REALKIND / 9) * K1

  K2(1,:) = [ 16, -2]
  K2(2,:) = [ -2, 16]
  K2 = (1._/**/REALKIND / 3) * K2

  KL(1,:) = [ 16, -2,  6]
  KL(2,:) = [ -2, 16,  6]
  KL = (1._/**/REALKIND / 3) * KL

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphlnjj_nexeudxhgg_1_/**/REALKIND



module ol_forced_parameters_pphlnjj_nexeudxhgg_1_/**/REALKIND
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
end module ol_forced_parameters_pphlnjj_nexeudxhgg_1_/**/REALKIND

module ol_loop_pphlnjj_nexeudxhgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(24), c(23)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:181)
  ! denominators
  complex(REALKIND), save :: den(177)
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
    f( 1) = (CI*eQED**3*gQCD**2*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f( 2) = (eQED**3*gQCD**2*lambdaHWW*MW)/(sw**3*2._/**/REALKIND)
    f( 3) = (CI*countertermnorm*eQED**3*gQCD**4*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f( 4) = (countertermnorm*eQED**3*gQCD**4*lambdaHWW*MW)/(sw**3*2._/**/REALKIND)
    f( 5) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f( 6) = (countertermnorm*ctGqq*eQED**3*gQCD**4*lambdaHWW*MW)/(sw**3*2._/**/REALKIND)
    f( 7) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f( 8) = (countertermnorm*ctVqq*eQED**3*gQCD**4*lambdaHWW*MW)/(sw**3*2._/**/REALKIND)
    f( 9) = (countertermnorm*ctVVV*eQED**3*gQCD**4*lambdaHWW*MW)/(sw**3*2._/**/REALKIND)
    f(10) = (CI*eQED**3*gQCD**4*integralnorm*lambdaHWW*MW*SwB)/(4._/**/REALKIND*sw**3)
    f(11) = (CI*eQED**3*gQCD**4*integralnorm*lambdaHWW*MW*SwB)/(2._/**/REALKIND*sw**3)
    f(12) = (eQED**3*gQCD**4*integralnorm*lambdaHWW*MW*SwB)/(sw**3*2._/**/REALKIND)
    f(13) = (CI*eQED**3*gQCD**4*integralnorm*lambdaHWW*MW*SwF)/(2._/**/REALKIND*sw**3)
    f(14) = (CI*eQED**3*gQCD**4*integralnorm*lambdaHWW*MW*SwF)/sw**3
    f(15) = (eQED**3*gQCD**4*integralnorm*lambdaHWW*MW*SwF)/(sw**3*2._/**/REALKIND)
    f(16) = (eQED**3*gQCD**4*integralnorm*lambdaHWW*MW*SwF)/sw**3
    f(17) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MB*YB)/(2._/**/REALKIND*MQ2sum*sw**2)
    f(18) = (countertermnorm*ctHGG*eQED**3*gQCD**4*MB*YB)/(MQ2sum*sw**2*2._/**/REALKIND)
    f(19) = (CI*eQED**3*gQCD**4*integralnorm*SwF*YB)/(4._/**/REALKIND*MW*sw**3)
    f(20) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw**3*4._/**/REALKIND)
    f(21) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MT*YT)/(2._/**/REALKIND*MQ2sum*sw**2)
    f(22) = (countertermnorm*ctHGG*eQED**3*gQCD**4*MT*YT)/(MQ2sum*sw**2*2._/**/REALKIND)
    f(23) = (CI*eQED**3*gQCD**4*integralnorm*SwF*YT)/(4._/**/REALKIND*MW*sw**3)
    f(24) = (eQED**3*gQCD**4*integralnorm*SwF*YT)/(MW*sw**3*4._/**/REALKIND)

  c = [ 9*CI*f(10), 18*CI*f(10), CI*f(11), 3*CI*f(11), 8*CI*f(11), 9*CI*f(11), 18*CI*f(11), f(12), 3*f(12), 8*f(12), 9*f(12) &
    , 3*CI*f(13), 3*CI*f(14), f(15), 3*f(15), f(16), 3*f(16), 3*CI*f(19), f(20), 3*f(20), 3*CI*f(23), f(24), 3*f(24) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,7)
  integer,           intent(in)  :: H(7)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(84)
  ! external WFs
  call wf_A(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_S(P(:,5), rMH, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_W(wf(:,-1),wf(:,0),wf(:,1))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,2))
  call vert_AV_Q(wf(:,-3),wf(:,-6),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MW,1_intkind1,wf(:,4))
  call prop_Q_A(wf(:,2),Q(:,36),ZERO,0_intkind1,wf(:,5))
  call prop_A_Q(wf(:,3),Q(:,72),ZERO,0_intkind1,wf(:,6))
  call vert_SV_V(wf(:,-4),wf(:,4),wf(:,7))
  call vert_QA_W(wf(:,5),wf(:,6),wf(:,8))
  call prop_W_W(wf(:,7),Q(:,19),MW,1_intkind1,wf(:,9))
  call vert_VQ_A(wf(:,-6),wf(:,5),wf(:,10))
  call prop_Q_A(wf(:,10),Q(:,100),ZERO,0_intkind1,wf(:,11))
  call vert_AW_Q(wf(:,-3),wf(:,9),wf(:,12))
  call vert_VQ_A(wf(:,-6),wf(:,-2),wf(:,13))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,14))
  call prop_Q_A(wf(:,13),Q(:,68),ZERO,0_intkind1,wf(:,15))
  call prop_A_Q(wf(:,14),Q(:,40),ZERO,0_intkind1,wf(:,16))
  call vert_QA_W(wf(:,15),wf(:,16),wf(:,17))
  call vert_AV_Q(wf(:,16),wf(:,-6),wf(:,18))
  call prop_A_Q(wf(:,18),Q(:,104),ZERO,0_intkind1,wf(:,19))
  call vert_WQ_A(wf(:,9),wf(:,-2),wf(:,20))
  call vert_VQ_A(wf(:,-5),wf(:,15),wf(:,21))
  call prop_Q_A(wf(:,21),Q(:,100),ZERO,0_intkind1,wf(:,22))
  call vert_AV_Q(wf(:,6),wf(:,-5),wf(:,23))
  call prop_A_Q(wf(:,23),Q(:,104),ZERO,0_intkind1,wf(:,24))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-6),Q(:,64),wf(:,25))
  call vert_AV_Q(wf(:,-3),wf(:,25),wf(:,26))
  call prop_A_Q(wf(:,26),Q(:,104),ZERO,0_intkind1,wf(:,27))
  call vert_QA_W(wf(:,-2),wf(:,27),wf(:,28))
  call vert_VQ_A(wf(:,25),wf(:,-2),wf(:,29))
  call prop_Q_A(wf(:,29),Q(:,100),ZERO,0_intkind1,wf(:,30))
  call vert_QA_W(wf(:,30),wf(:,-3),wf(:,31))
  call counter_QA_W(wf(:,5),wf(:,6),wf(:,32))
  call counter_VQ_A(wf(:,-6),wf(:,5),wf(:,33))
  call prop_Q_A(wf(:,33),Q(:,100),ZERO,0_intkind1,wf(:,34))
  call counter_QA_W(wf(:,15),wf(:,16),wf(:,35))
  call counter_AV_Q(wf(:,16),wf(:,-6),wf(:,36))
  call prop_A_Q(wf(:,36),Q(:,104),ZERO,0_intkind1,wf(:,37))
  call counter_VQ_A(wf(:,-5),wf(:,15),wf(:,38))
  call prop_Q_A(wf(:,38),Q(:,100),ZERO,0_intkind1,wf(:,39))
  call counter_AV_Q(wf(:,6),wf(:,-5),wf(:,40))
  call prop_A_Q(wf(:,40),Q(:,104),ZERO,0_intkind1,wf(:,41))
  call counter_UV_W(wf(:,-5),Q(:,32),wf(:,-6),Q(:,64),wf(:,42))
  call vert_AV_Q(wf(:,-3),wf(:,42),wf(:,43))
  call prop_A_Q(wf(:,43),Q(:,104),ZERO,0_intkind1,wf(:,44))
  call vert_QA_W(wf(:,-2),wf(:,44),wf(:,45))
  call vert_VQ_A(wf(:,42),wf(:,-2),wf(:,46))
  call prop_Q_A(wf(:,46),Q(:,100),ZERO,0_intkind1,wf(:,47))
  call vert_QA_W(wf(:,47),wf(:,-3),wf(:,48))
  call counter_SG_G(wf(:,-4),wf(:,-6),wf(:,49))
  call vert_AW_Q(wf(:,-3),wf(:,4),wf(:,50))
  call vert_VQ_A(wf(:,49),wf(:,5),wf(:,51))
  call prop_A_Q(wf(:,50),Q(:,11),ZERO,0_intkind1,wf(:,52))
  call vert_AV_Q(wf(:,-3),wf(:,49),wf(:,53))
  call vert_WQ_A(wf(:,4),wf(:,5),wf(:,54))
  call prop_A_Q(wf(:,53),Q(:,88),ZERO,0_intkind1,wf(:,55))
  call vert_WQ_A(wf(:,4),wf(:,-2),wf(:,56))
  call vert_AV_Q(wf(:,16),wf(:,49),wf(:,57))
  call prop_Q_A(wf(:,56),Q(:,7),ZERO,0_intkind1,wf(:,58))
  call vert_VQ_A(wf(:,49),wf(:,-2),wf(:,59))
  call vert_AW_Q(wf(:,16),wf(:,4),wf(:,60))
  call prop_Q_A(wf(:,59),Q(:,84),ZERO,0_intkind1,wf(:,61))
  call counter_SG_G(wf(:,-4),wf(:,25),wf(:,62))
  call vert_QA_V(wf(:,58),wf(:,-3),wf(:,63))
  call vert_QA_V(wf(:,-2),wf(:,52),wf(:,64))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,49),Q(:,80),wf(:,65))
  call vert_VQ_A(wf(:,-5),wf(:,58),wf(:,66))
  call vert_VQ_A(wf(:,-5),wf(:,61),wf(:,67))
  call counter_SG_G(wf(:,-4),wf(:,-5),wf(:,68))
  call vert_VQ_A(wf(:,68),wf(:,15),wf(:,69))
  call vert_AV_Q(wf(:,-3),wf(:,68),wf(:,70))
  call vert_WQ_A(wf(:,4),wf(:,15),wf(:,71))
  call prop_A_Q(wf(:,70),Q(:,56),ZERO,0_intkind1,wf(:,72))
  call vert_AV_Q(wf(:,6),wf(:,68),wf(:,73))
  call vert_VQ_A(wf(:,68),wf(:,-2),wf(:,74))
  call vert_AW_Q(wf(:,6),wf(:,4),wf(:,75))
  call prop_Q_A(wf(:,74),Q(:,52),ZERO,0_intkind1,wf(:,76))
  call vert_UV_W(wf(:,68),Q(:,48),wf(:,-6),Q(:,64),wf(:,77))
  call vert_VQ_A(wf(:,-6),wf(:,58),wf(:,78))
  call vert_VQ_A(wf(:,-6),wf(:,76),wf(:,79))
  call counter_AW_Q(wf(:,-3),wf(:,9),wf(:,80))
  call counter_AV_Q(wf(:,-3),wf(:,-6),wf(:,81))
  call prop_A_Q(wf(:,81),Q(:,72),ZERO,0_intkind1,wf(:,82))
  call vert_QA_W(wf(:,5),wf(:,82),wf(:,83))
  call counter_AV_Q(wf(:,-3),wf(:,25),wf(:,84))
  call prop_A_Q(wf(:,84),Q(:,104),ZERO,0_intkind1,wf(:,85))
  call vert_QA_W(wf(:,-2),wf(:,85),wf(:,86))
  call counter_QA_W(wf(:,30),wf(:,-3),wf(:,87))
  call vert_AV_Q(wf(:,82),wf(:,-5),wf(:,88))
  call prop_A_Q(wf(:,88),Q(:,104),ZERO,0_intkind1,wf(:,89))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,90))
  call prop_A_Q(wf(:,90),Q(:,40),ZERO,0_intkind1,wf(:,91))
  call vert_QA_W(wf(:,15),wf(:,91),wf(:,92))
  call vert_AV_Q(wf(:,91),wf(:,-6),wf(:,93))
  call prop_A_Q(wf(:,93),Q(:,104),ZERO,0_intkind1,wf(:,94))
  call counter_WQ_A(wf(:,9),wf(:,-2),wf(:,95))
  call counter_VQ_A(wf(:,-6),wf(:,-2),wf(:,96))
  call prop_Q_A(wf(:,96),Q(:,68),ZERO,0_intkind1,wf(:,97))
  call vert_QA_W(wf(:,97),wf(:,16),wf(:,98))
  call counter_QA_W(wf(:,-2),wf(:,27),wf(:,99))
  call counter_VQ_A(wf(:,25),wf(:,-2),wf(:,100))
  call prop_Q_A(wf(:,100),Q(:,100),ZERO,0_intkind1,wf(:,101))
  call vert_QA_W(wf(:,101),wf(:,-3),wf(:,102))
  call vert_VQ_A(wf(:,-5),wf(:,97),wf(:,103))
  call prop_Q_A(wf(:,103),Q(:,100),ZERO,0_intkind1,wf(:,104))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,105))
  call prop_Q_A(wf(:,105),Q(:,36),ZERO,0_intkind1,wf(:,106))
  call vert_QA_W(wf(:,106),wf(:,6),wf(:,107))
  call vert_VQ_A(wf(:,-6),wf(:,106),wf(:,108))
  call prop_Q_A(wf(:,108),Q(:,100),ZERO,0_intkind1,wf(:,109))
  call counter_Q_A(ctqq,wf(:,5),Q(:,36),wf(:,110))
  call prop_Q_A(wf(:,110),Q(:,36),ZERO,0_intkind1,wf(:,111))
  call vert_QA_W(wf(:,111),wf(:,6),wf(:,112))
  call counter_A_Q(ctqq,wf(:,6),Q(:,72),wf(:,113))
  call prop_A_Q(wf(:,113),Q(:,72),ZERO,0_intkind1,wf(:,114))
  call vert_QA_W(wf(:,5),wf(:,114),wf(:,115))
  call vert_VQ_A(wf(:,-6),wf(:,111),wf(:,116))
  call prop_A_Q(wf(:,12),Q(:,27),ZERO,0_intkind1,wf(:,117))
  call counter_Q_A(ctqq,wf(:,11),Q(:,100),wf(:,118))
  call counter_Q_A(ctqq,wf(:,15),Q(:,68),wf(:,119))
  call prop_Q_A(wf(:,119),Q(:,68),ZERO,0_intkind1,wf(:,120))
  call vert_QA_W(wf(:,120),wf(:,16),wf(:,121))
  call counter_A_Q(ctqq,wf(:,16),Q(:,40),wf(:,122))
  call prop_A_Q(wf(:,122),Q(:,40),ZERO,0_intkind1,wf(:,123))
  call vert_QA_W(wf(:,15),wf(:,123),wf(:,124))
  call counter_A_Q(ctqq,wf(:,19),Q(:,104),wf(:,125))
  call prop_Q_A(wf(:,20),Q(:,23),ZERO,0_intkind1,wf(:,126))
  call vert_AV_Q(wf(:,123),wf(:,-6),wf(:,127))
  call vert_VQ_A(wf(:,-5),wf(:,120),wf(:,128))
  call counter_Q_A(ctqq,wf(:,22),Q(:,100),wf(:,129))
  call counter_A_Q(ctqq,wf(:,24),Q(:,104),wf(:,130))
  call vert_AV_Q(wf(:,114),wf(:,-5),wf(:,131))
  call counter_A_Q(ctqq,wf(:,27),Q(:,104),wf(:,132))
  call counter_Q_A(ctqq,wf(:,30),Q(:,100),wf(:,133))
  call counter_V_V(ctGG,wf(:,25),Q(:,96),wf(:,134))
  call vert_VQ_A(wf(:,134),wf(:,-2),wf(:,135))
  call prop_Q_A(wf(:,135),Q(:,100),ZERO,0_intkind1,wf(:,136))
  call vert_AV_Q(wf(:,-3),wf(:,134),wf(:,137))
  call vert_QA_V(wf(:,5),wf(:,52),wf(:,138))
  call vert_WQ_A(wf(:,9),wf(:,5),wf(:,139))
  call prop_Q_A(wf(:,139),Q(:,55),ZERO,0_intkind1,wf(:,140))
  call prop_Q_A(wf(:,54),Q(:,39),ZERO,0_intkind1,wf(:,141))
  call vert_QA_V(wf(:,141),wf(:,-3),wf(:,142))
  call vert_QA_V(wf(:,58),wf(:,16),wf(:,143))
  call vert_AW_Q(wf(:,16),wf(:,9),wf(:,144))
  call prop_A_Q(wf(:,144),Q(:,59),ZERO,0_intkind1,wf(:,145))
  call prop_A_Q(wf(:,60),Q(:,43),ZERO,0_intkind1,wf(:,146))
  call vert_QA_V(wf(:,-2),wf(:,146),wf(:,147))
  call vert_QA_V(wf(:,15),wf(:,52),wf(:,148))
  call vert_WQ_A(wf(:,9),wf(:,15),wf(:,149))
  call prop_Q_A(wf(:,149),Q(:,87),ZERO,0_intkind1,wf(:,150))
  call prop_Q_A(wf(:,71),Q(:,71),ZERO,0_intkind1,wf(:,151))
  call vert_QA_V(wf(:,151),wf(:,-3),wf(:,152))
  call vert_QA_V(wf(:,58),wf(:,6),wf(:,153))
  call vert_AW_Q(wf(:,6),wf(:,9),wf(:,154))
  call prop_A_Q(wf(:,154),Q(:,91),ZERO,0_intkind1,wf(:,155))
  call prop_A_Q(wf(:,75),Q(:,75),ZERO,0_intkind1,wf(:,156))
  call vert_QA_V(wf(:,-2),wf(:,156),wf(:,157))
  call vert_UV_W(wf(:,63),Q(:,15),wf(:,-5),Q(:,32),wf(:,158))
  call vert_UV_W(wf(:,63),Q(:,15),wf(:,-6),Q(:,64),wf(:,159))
  call prop_Q_A(wf(:,66),Q(:,39),ZERO,0_intkind1,wf(:,160))
  call vert_QA_V(wf(:,160),wf(:,-3),wf(:,161))
  call prop_Q_A(wf(:,78),Q(:,71),ZERO,0_intkind1,wf(:,162))
  call vert_QA_V(wf(:,162),wf(:,-3),wf(:,163))
  call vert_UV_W(wf(:,64),Q(:,15),wf(:,-5),Q(:,32),wf(:,164))
  call vert_UV_W(wf(:,64),Q(:,15),wf(:,-6),Q(:,64),wf(:,165))
  call vert_AV_Q(wf(:,52),wf(:,-5),wf(:,166))
  call prop_A_Q(wf(:,166),Q(:,43),ZERO,0_intkind1,wf(:,167))
  call vert_QA_V(wf(:,-2),wf(:,167),wf(:,168))
  call vert_AV_Q(wf(:,52),wf(:,-6),wf(:,169))
  call prop_A_Q(wf(:,169),Q(:,75),ZERO,0_intkind1,wf(:,170))
  call vert_QA_V(wf(:,-2),wf(:,170),wf(:,171))
  call vert_QA_V(wf(:,126),wf(:,-3),wf(:,172))
  call vert_QA_V(wf(:,-2),wf(:,117),wf(:,173))
  call vert_VQ_A(wf(:,-5),wf(:,126),wf(:,174))
  call prop_Q_A(wf(:,174),Q(:,55),ZERO,0_intkind1,wf(:,175))
  call vert_VQ_A(wf(:,-6),wf(:,126),wf(:,176))
  call prop_Q_A(wf(:,176),Q(:,87),ZERO,0_intkind1,wf(:,177))
  call vert_AV_Q(wf(:,117),wf(:,-5),wf(:,178))
  call prop_A_Q(wf(:,178),Q(:,59),ZERO,0_intkind1,wf(:,179))
  call vert_AV_Q(wf(:,117),wf(:,-6),wf(:,180))
  call prop_A_Q(wf(:,180),Q(:,91),ZERO,0_intkind1,wf(:,181))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3) - MW2)
  den(2) = 1 / (Q(5,36))
  den(3) = 1 / (Q(5,72))
  den(5) = 1 / (Q(5,19) - MW2)
  den(8) = 1 / (Q(5,100))
  den(11) = 1 / (Q(5,68))
  den(12) = 1 / (Q(5,40))
  den(15) = 1 / (Q(5,104))
  den(22) = 1 / (Q(5,96))
  den(27) = 1 / (Q(5,80))
  den(29) = 1 / (Q(5,11))
  den(33) = 1 / (Q(5,88))
  den(37) = 1 / (Q(5,7))
  den(41) = 1 / (Q(5,84))
  den(44) = 1 / (Q(5,112))
  den(53) = 1 / (Q(5,48))
  den(57) = 1 / (Q(5,56))
  den(63) = 1 / (Q(5,52))
  den(77) = 1 / (Q(5,27))
  den(87) = 1 / (Q(5,23))
  den(102) = 1 / (Q(5,47))
  den(105) = 1 / (Q(5,55))
  den(107) = 1 / (Q(5,39))
  den(113) = 1 / (Q(5,59))
  den(115) = 1 / (Q(5,43))
  den(119) = 1 / (Q(5,79))
  den(122) = 1 / (Q(5,87))
  den(124) = 1 / (Q(5,71))
  den(130) = 1 / (Q(5,91))
  den(132) = 1 / (Q(5,75))
  den(135) = 1 / (Q(5,15))
  den(150) = 1 / (Q(5,31))

  ! denominators
  den(4) = den(2)*den(3)
  den(6) = den(1)*den(5)
  den(7) = den(4)*den(6)
  den(9) = den(2)*den(8)
  den(10) = den(6)*den(9)
  den(13) = den(11)*den(12)
  den(14) = den(6)*den(13)
  den(16) = den(12)*den(15)
  den(17) = den(6)*den(16)
  den(18) = den(8)*den(11)
  den(19) = den(6)*den(18)
  den(20) = den(3)*den(15)
  den(21) = den(6)*den(20)
  den(23) = den(15)*den(22)
  den(24) = den(6)*den(23)
  den(25) = den(8)*den(22)
  den(26) = den(6)*den(25)
  den(28) = den(2)*den(27)
  den(30) = den(1)*den(29)
  den(31) = den(28)*den(30)
  den(32) = den(1)*den(2)
  den(34) = den(27)*den(33)
  den(35) = den(32)*den(34)
  den(36) = den(12)*den(27)
  den(38) = den(1)*den(37)
  den(39) = den(36)*den(38)
  den(40) = den(1)*den(12)
  den(42) = den(27)*den(41)
  den(43) = den(40)*den(42)
  den(45) = den(22)*den(44)
  den(46) = den(38)*den(45)
  den(47) = den(30)*den(45)
  den(48) = den(27)*den(44)
  den(49) = den(38)*den(48)
  den(50) = den(34)*den(38)
  den(51) = den(30)*den(48)
  den(52) = den(30)*den(42)
  den(54) = den(11)*den(53)
  den(55) = den(30)*den(54)
  den(56) = den(1)*den(11)
  den(58) = den(53)*den(57)
  den(59) = den(56)*den(58)
  den(60) = den(3)*den(53)
  den(61) = den(38)*den(60)
  den(62) = den(1)*den(3)
  den(64) = den(53)*den(63)
  den(65) = den(62)*den(64)
  den(66) = den(44)*den(53)
  den(67) = den(38)*den(66)
  den(68) = den(38)*den(58)
  den(69) = den(30)*den(66)
  den(70) = den(30)*den(64)
  den(71) = den(2)**2
  den(72) = den(3)*den(71)
  den(73) = den(6)*den(72)
  den(74) = den(3)**2
  den(75) = den(2)*den(74)
  den(76) = den(6)*den(75)
  den(78) = den(6)*den(77)
  den(79) = den(71)*den(78)
  den(80) = den(9)*den(78)
  den(81) = den(11)**2
  den(82) = den(12)*den(81)
  den(83) = den(6)*den(82)
  den(84) = den(12)**2
  den(85) = den(11)*den(84)
  den(86) = den(6)*den(85)
  den(88) = den(6)*den(87)
  den(89) = den(16)*den(88)
  den(90) = den(84)*den(88)
  den(91) = den(78)*den(81)
  den(92) = den(18)*den(78)
  den(93) = den(20)*den(88)
  den(94) = den(74)*den(88)
  den(95) = den(23)*den(88)
  den(96) = den(25)*den(78)
  den(97) = den(22)**2
  den(98) = den(8)*den(97)
  den(99) = den(6)*den(98)
  den(100) = den(88)*den(97)
  den(101) = den(2)*den(30)
  den(103) = den(101)*den(102)
  den(104) = den(2)*den(6)
  den(106) = den(104)*den(105)
  den(108) = den(32)*den(107)
  den(109) = den(102)*den(108)
  den(110) = den(12)*den(38)
  den(111) = den(102)*den(110)
  den(112) = den(6)*den(12)
  den(114) = den(112)*den(113)
  den(116) = den(40)*den(115)
  den(117) = den(102)*den(116)
  den(118) = den(11)*den(30)
  den(120) = den(118)*den(119)
  den(121) = den(6)*den(11)
  den(123) = den(121)*den(122)
  den(125) = den(56)*den(124)
  den(126) = den(119)*den(125)
  den(127) = den(3)*den(38)
  den(128) = den(119)*den(127)
  den(129) = den(3)*den(6)
  den(131) = den(129)*den(130)
  den(133) = den(62)*den(132)
  den(134) = den(119)*den(133)
  den(136) = den(38)*den(135)
  den(137) = den(30)*den(135)
  den(138) = den(102)*den(136)
  den(139) = den(119)*den(136)
  den(140) = den(38)*den(107)
  den(141) = den(102)*den(140)
  den(142) = den(38)*den(124)
  den(143) = den(119)*den(142)
  den(144) = den(102)*den(137)
  den(145) = den(119)*den(137)
  den(146) = den(30)*den(115)
  den(147) = den(102)*den(146)
  den(148) = den(30)*den(132)
  den(149) = den(119)*den(148)
  den(151) = den(88)*den(150)
  den(152) = den(78)*den(150)
  den(153) = den(88)*den(105)
  den(154) = den(88)*den(122)
  den(155) = den(78)*den(113)
  den(156) = den(78)*den(130)
  den(157) = den(2)*den(3)*den(6)
  den(158) = den(2)*den(78)
  den(159) = den(6)*den(11)*den(12)
  den(160) = den(12)*den(88)
  den(161) = den(11)*den(78)
  den(162) = den(3)*den(88)
  den(163) = den(22)*den(136)
  den(164) = den(22)*den(137)
  den(165) = den(22)*den(88)
  den(166) = den(22)*den(78)
  den(167) = den(6)*den(22)
  den(168) = den(3)*den(106)
  den(169) = den(2)*den(131)
  den(170) = den(2)*den(156)
  den(171) = den(12)*den(123)
  den(172) = den(11)*den(114)
  den(173) = den(12)*den(154)
  den(174) = den(11)*den(155)
  den(175) = den(3)*den(153)
  den(176) = den(22)*den(151)
  den(177) = den(22)*den(152)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(84)

  A(1) = cont_VV(wf(:,8),wf(:,9)) * den(7)
  A(2) = cont_QA(wf(:,11),wf(:,12)) * den(10)
  A(3) = cont_VV(wf(:,9),wf(:,17)) * den(14)
  A(4) = cont_QA(wf(:,19),wf(:,20)) * den(17)
  A(5) = cont_QA(wf(:,12),wf(:,22)) * den(19)
  A(6) = cont_QA(wf(:,20),wf(:,24)) * den(21)
  A(7) = cont_VV(wf(:,9),wf(:,28)) * den(24)
  A(8) = cont_VV(wf(:,9),wf(:,31)) * den(26)

  A(9) = cont_VV(wf(:,9),wf(:,32)) * den(7)
  A(10) = cont_QA(wf(:,12),wf(:,34)) * den(10)
  A(11) = cont_VV(wf(:,9),wf(:,35)) * den(14)
  A(12) = cont_QA(wf(:,20),wf(:,37)) * den(17)
  A(13) = cont_QA(wf(:,12),wf(:,39)) * den(19)
  A(14) = cont_QA(wf(:,20),wf(:,41)) * den(21)
  A(15) = cont_VV(wf(:,9),wf(:,45)) * den(24)
  A(16) = cont_VV(wf(:,9),wf(:,48)) * den(26)
  A(17) = cont_QA(wf(:,51),wf(:,52)) * den(31)
  A(18) = cont_QA(wf(:,51),wf(:,52)) * den(31)
  A(19) = cont_QA(wf(:,54),wf(:,55)) * den(35)
  A(20) = cont_QA(wf(:,54),wf(:,55)) * den(35)
  A(21) = cont_QA(wf(:,57),wf(:,58)) * den(39)
  A(22) = cont_QA(wf(:,57),wf(:,58)) * den(39)
  A(23) = cont_QA(wf(:,60),wf(:,61)) * den(43)
  A(24) = cont_QA(wf(:,60),wf(:,61)) * den(43)
  A(25) = cont_VV(wf(:,62),wf(:,63)) * den(46)
  A(26) = cont_VV(wf(:,62),wf(:,63)) * den(46)
  A(27) = cont_VV(wf(:,62),wf(:,64)) * den(47)
  A(28) = cont_VV(wf(:,62),wf(:,64)) * den(47)
  A(29) = cont_VV(wf(:,63),wf(:,65)) * den(49)
  A(30) = cont_VV(wf(:,63),wf(:,65)) * den(49)
  A(31) = cont_QA(wf(:,55),wf(:,66)) * den(50)
  A(32) = cont_QA(wf(:,55),wf(:,66)) * den(50)
  A(33) = cont_VV(wf(:,64),wf(:,65)) * den(51)
  A(34) = cont_VV(wf(:,64),wf(:,65)) * den(51)
  A(35) = cont_QA(wf(:,52),wf(:,67)) * den(52)
  A(36) = cont_QA(wf(:,52),wf(:,67)) * den(52)
  A(37) = cont_QA(wf(:,52),wf(:,69)) * den(55)
  A(38) = cont_QA(wf(:,52),wf(:,69)) * den(55)
  A(39) = cont_QA(wf(:,71),wf(:,72)) * den(59)
  A(40) = cont_QA(wf(:,71),wf(:,72)) * den(59)
  A(41) = cont_QA(wf(:,58),wf(:,73)) * den(61)
  A(42) = cont_QA(wf(:,58),wf(:,73)) * den(61)
  A(43) = cont_QA(wf(:,75),wf(:,76)) * den(65)
  A(44) = cont_QA(wf(:,75),wf(:,76)) * den(65)
  A(45) = cont_VV(wf(:,63),wf(:,77)) * den(67)
  A(46) = cont_VV(wf(:,63),wf(:,77)) * den(67)
  A(47) = cont_QA(wf(:,72),wf(:,78)) * den(68)
  A(48) = cont_QA(wf(:,72),wf(:,78)) * den(68)
  A(49) = cont_VV(wf(:,64),wf(:,77)) * den(69)
  A(50) = cont_VV(wf(:,64),wf(:,77)) * den(69)
  A(51) = cont_QA(wf(:,52),wf(:,79)) * den(70)
  A(52) = cont_QA(wf(:,52),wf(:,79)) * den(70)
  A(53) = cont_QA(wf(:,11),wf(:,80)) * den(10)
  A(54) = cont_VV(wf(:,9),wf(:,83)) * den(7)
  A(55) = cont_QA(wf(:,22),wf(:,80)) * den(19)
  A(56) = cont_VV(wf(:,9),wf(:,86)) * den(24)
  A(57) = cont_VV(wf(:,9),wf(:,87)) * den(26)
  A(58) = cont_QA(wf(:,20),wf(:,89)) * den(21)
  A(59) = cont_VV(wf(:,9),wf(:,92)) * den(14)
  A(60) = cont_QA(wf(:,20),wf(:,94)) * den(17)
  A(61) = cont_QA(wf(:,19),wf(:,95)) * den(17)
  A(62) = cont_VV(wf(:,9),wf(:,98)) * den(14)
  A(63) = cont_QA(wf(:,24),wf(:,95)) * den(21)
  A(64) = cont_VV(wf(:,9),wf(:,99)) * den(24)
  A(65) = cont_VV(wf(:,9),wf(:,102)) * den(26)
  A(66) = cont_QA(wf(:,12),wf(:,104)) * den(19)
  A(67) = cont_VV(wf(:,9),wf(:,107)) * den(7)
  A(68) = cont_QA(wf(:,12),wf(:,109)) * den(10)
  A(69) = cont_VV(wf(:,9),wf(:,112)) * den(73)
  A(70) = cont_VV(wf(:,9),wf(:,115)) * den(76)
  A(71) = cont_QA(wf(:,116),wf(:,117)) * den(79)
  A(72) = cont_QA(wf(:,117),wf(:,118)) * den(80)
  A(73) = cont_VV(wf(:,9),wf(:,121)) * den(83)
  A(74) = cont_VV(wf(:,9),wf(:,124)) * den(86)
  A(75) = cont_QA(wf(:,125),wf(:,126)) * den(89)
  A(76) = cont_QA(wf(:,126),wf(:,127)) * den(90)
  A(77) = cont_QA(wf(:,117),wf(:,128)) * den(91)
  A(78) = cont_QA(wf(:,117),wf(:,129)) * den(92)
  A(79) = cont_QA(wf(:,126),wf(:,130)) * den(93)
  A(80) = cont_QA(wf(:,126),wf(:,131)) * den(94)
  A(81) = cont_QA(wf(:,126),wf(:,132)) * den(95)
  A(82) = cont_QA(wf(:,117),wf(:,133)) * den(96)
  A(83) = cont_QA(wf(:,12),wf(:,136)) * den(99)
  A(84) = cont_QA(wf(:,126),wf(:,137)) * den(100)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(84)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = (-A(3)-A(4)-A(5))*f(1)+CI*(-A(7)-A(8))*f(2)
  M1(2) = (-A(1)-A(2)-A(6))*f(1)+CI*(A(7)+A(8))*f(2)

  M2(1) = (A(73)+A(74)+A(75)+A(76)+A(77)+A(78))*f(3)+CI*(A(81)+A(82)+A(83)+A(84))*f(4)+(-A(12)-A(13)-A(59)-A(60)-A(62)-A(66))*f(5) &
       +CI*(-A(56)-A(65))*f(6)+(-A(11)-A(55)-A(61))*f(7)+CI*(-A(57)-A(64))*f(8)+CI*(-A(15)-A(16))*f(9)+(-A(22)-A(24)-A(36)-A(38) &
       -A(40)-A(48))*f(17)+CI*(-A(26)-A(28)-A(30)-A(34)-A(46)-A(50))*f(18)+(-A(21)-A(23)-A(35)-A(37)-A(39)-A(47))*f(21)+CI*(-A(25) &
       -A(27)-A(29)-A(33)-A(45)-A(49))*f(22)
  M2(2) = (A(69)+A(70)+A(71)+A(72)+A(79)+A(80))*f(3)+CI*(-A(81)-A(82)-A(83)-A(84))*f(4)+(-A(10)-A(14)-A(54)-A(58)-A(67) &
       -A(68))*f(5)+CI*(A(56)+A(65))*f(6)+(-A(9)-A(53)-A(63))*f(7)+CI*(A(57)+A(64))*f(8)+CI*(A(15)+A(16))*f(9)+(-A(18)-A(20)-A(32) &
       -A(42)-A(44)-A(52))*f(17)+CI*(A(26)+A(28)+A(30)+A(34)+A(46)+A(50))*f(18)+(-A(17)-A(19)-A(31)-A(41)-A(43)-A(51))*f(21) &
       +CI*(A(25)+A(27)+A(29)+A(33)+A(45)+A(49))*f(22)

end subroutine colourvectors

end module ol_loop_pphlnjj_nexeudxhgg_1_/**/REALKIND
