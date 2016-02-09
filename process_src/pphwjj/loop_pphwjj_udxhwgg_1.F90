
module ol_colourmatrix_pphwjj_udxhwgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(46,2), K2(2,2), KL(2,3)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  48,  -6]
  K1( 2,:) = [  -6,  48]
  K1( 3,:) = [  64,  -8]
  K1( 4,:) = [  -8,  64]
  K1( 5,:) = [  -1, -10]
  K1( 6,:) = [ -10,  -1]
  K1( 7,:) = [  64,  -8]
  K1( 8,:) = [  -8,  64]
  K1( 9,:) = [   0,   0]
  K1(10,:) = [   0,   0]
  K1(11,:) = [   0,   0]
  K1(12,:) = [   0,   0]
  K1(13,:) = [   0,   0]
  K1(14,:) = [   0,   0]
  K1(15,:) = [   0,   0]
  K1(16,:) = [   0,   0]
  K1(17,:) = [   0,   0]
  K1(18,:) = [   0,   0]
  K1(19,:) = [   0,   0]
  K1(20,:) = [   0,   0]
  K1(21,:) = [   0,   0]
  K1(22,:) = [   0,   0]
  K1(23,:) = [   9,   9]
  K1(24,:) = [   9, -72]
  K1(25,:) = [ -72,   9]
  K1(26,:) = [   9,   9]
  K1(27,:) = [   0,   0]
  K1(28,:) = [   0,   0]
  K1(29,:) = [   0,   0]
  K1(30,:) = [   0,   0]
  K1(31,:) = [ 144, -18]
  K1(32,:) = [ -18, 144]
  K1(33,:) = [ -72,   9]
  K1(34,:) = [   9,   9]
  K1(35,:) = [   9,   9]
  K1(36,:) = [   9, -72]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [ -81,   0]
  K1(42,:) = [   0, -81]
  K1(43,:) = [ 144, -18]
  K1(44,:) = [ -18, 144]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]
  K1 = (1._/**/REALKIND / 9) * K1

  K2(1,:) = [ 16, -2]
  K2(2,:) = [ -2, 16]
  K2 = (1._/**/REALKIND / 3) * K2

  KL(1,:) = [ 16, -2,  6]
  KL(2,:) = [ -2, 16,  6]
  KL = (1._/**/REALKIND / 3) * KL

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphwjj_udxhwgg_1_/**/REALKIND



module ol_forced_parameters_pphwjj_udxhwgg_1_/**/REALKIND
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
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphwjj_udxhwgg_1_/**/REALKIND

module ol_loop_pphwjj_udxhwgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(24), c(23)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:146)
  ! denominators
  complex(REALKIND), save :: den(150)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,48)
  ! zero helicity identifier
  logical,           save :: zerohel(48) = .true., zerohel_ct(48) = .true.

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
    f( 1) = (CI*eQED**2*gQCD**2*lambdaHWW*MW)/(sqrt2*sw**2)
    f( 2) = (eQED**2*gQCD**2*lambdaHWW*MW)/(sqrt2*sw**2)
    f( 3) = (CI*countertermnorm*eQED**2*gQCD**4*lambdaHWW*MW)/(sqrt2*sw**2)
    f( 4) = (countertermnorm*eQED**2*gQCD**4*lambdaHWW*MW)/(sqrt2*sw**2)
    f( 5) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4*lambdaHWW*MW)/(sqrt2*sw**2)
    f( 6) = (countertermnorm*ctGqq*eQED**2*gQCD**4*lambdaHWW*MW)/(sqrt2*sw**2)
    f( 7) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4*lambdaHWW*MW)/(sqrt2*sw**2)
    f( 8) = (countertermnorm*ctVqq*eQED**2*gQCD**4*lambdaHWW*MW)/(sqrt2*sw**2)
    f( 9) = (countertermnorm*ctVVV*eQED**2*gQCD**4*lambdaHWW*MW)/(sqrt2*sw**2)
    f(10) = (CI*eQED**2*gQCD**4*integralnorm*lambdaHWW*MW*SwB)/(2._/**/REALKIND*sqrt2*sw**2)
    f(11) = (CI*eQED**2*gQCD**4*integralnorm*lambdaHWW*MW*SwB)/(sqrt2*sw**2)
    f(12) = (eQED**2*gQCD**4*integralnorm*lambdaHWW*MW*SwB)/(sqrt2*sw**2)
    f(13) = (CI*eQED**2*gQCD**4*integralnorm*lambdaHWW*MW*SwF)/(sqrt2*sw**2)
    f(14) = (2*CI*eQED**2*gQCD**4*integralnorm*lambdaHWW*MW*SwF)/(sqrt2*sw**2)
    f(15) = (eQED**2*gQCD**4*integralnorm*lambdaHWW*MW*SwF)/(sqrt2*sw**2)
    f(16) = (2*eQED**2*gQCD**4*integralnorm*lambdaHWW*MW*SwF)/(sqrt2*sw**2)
    f(17) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MB*YB)/(MQ2sum*sqrt2*sw)
    f(18) = (countertermnorm*ctHGG*eQED**2*gQCD**4*MB*YB)/(MQ2sum*sqrt2*sw)
    f(19) = (CI*eQED**2*gQCD**4*integralnorm*SwF*YB)/(2._/**/REALKIND*MW*sqrt2*sw**2)
    f(20) = (eQED**2*gQCD**4*integralnorm*SwF*YB)/(MW*sqrt2*sw**2*2._/**/REALKIND)
    f(21) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MT*YT)/(MQ2sum*sqrt2*sw)
    f(22) = (countertermnorm*ctHGG*eQED**2*gQCD**4*MT*YT)/(MQ2sum*sqrt2*sw)
    f(23) = (CI*eQED**2*gQCD**4*integralnorm*SwF*YT)/(2._/**/REALKIND*MW*sqrt2*sw**2)
    f(24) = (eQED**2*gQCD**4*integralnorm*SwF*YT)/(MW*sqrt2*sw**2*2._/**/REALKIND)

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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(84)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_S(P(:,3), rMH, H(3), wf(:,-2))
  call wf_V(P(:,4), rMW, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,1))
  call vert_AV_Q(wf(:,-1),wf(:,-5),wf(:,2))
  call vert_SV_V(wf(:,-2),wf(:,-3),wf(:,3))
  call prop_Q_A(wf(:,1),Q(:,17),ZERO,0_intkind1,wf(:,4))
  call prop_A_Q(wf(:,2),Q(:,34),ZERO,0_intkind1,wf(:,5))
  call prop_W_W(wf(:,3),Q(:,12),MW,1_intkind1,wf(:,6))
  call vert_QA_W(wf(:,4),wf(:,5),wf(:,7))
  call vert_AW_Q(wf(:,-1),wf(:,6),wf(:,8))
  call vert_VQ_A(wf(:,-5),wf(:,4),wf(:,9))
  call prop_A_Q(wf(:,8),Q(:,14),ZERO,0_intkind1,wf(:,10))
  call vert_VQ_A(wf(:,-5),wf(:,0),wf(:,11))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,33),ZERO,0_intkind1,wf(:,13))
  call prop_A_Q(wf(:,12),Q(:,18),ZERO,0_intkind1,wf(:,14))
  call vert_QA_W(wf(:,13),wf(:,14),wf(:,15))
  call vert_WQ_A(wf(:,6),wf(:,0),wf(:,16))
  call vert_AV_Q(wf(:,14),wf(:,-5),wf(:,17))
  call prop_Q_A(wf(:,16),Q(:,13),ZERO,0_intkind1,wf(:,18))
  call vert_VQ_A(wf(:,-4),wf(:,13),wf(:,19))
  call vert_AV_Q(wf(:,5),wf(:,-4),wf(:,20))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,21))
  call vert_AV_Q(wf(:,-1),wf(:,21),wf(:,22))
  call vert_VQ_A(wf(:,21),wf(:,0),wf(:,23))
  call prop_Q_A(wf(:,23),Q(:,49),ZERO,0_intkind1,wf(:,24))
  call counter_QA_W(wf(:,4),wf(:,5),wf(:,25))
  call counter_VQ_A(wf(:,-5),wf(:,4),wf(:,26))
  call counter_QA_W(wf(:,13),wf(:,14),wf(:,27))
  call counter_AV_Q(wf(:,14),wf(:,-5),wf(:,28))
  call counter_VQ_A(wf(:,-4),wf(:,13),wf(:,29))
  call counter_AV_Q(wf(:,5),wf(:,-4),wf(:,30))
  call counter_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,31))
  call vert_AV_Q(wf(:,-1),wf(:,31),wf(:,32))
  call vert_VQ_A(wf(:,31),wf(:,0),wf(:,33))
  call prop_Q_A(wf(:,33),Q(:,49),ZERO,0_intkind1,wf(:,34))
  call vert_WQ_A(wf(:,-3),wf(:,0),wf(:,35))
  call counter_SG_G(wf(:,-2),wf(:,-5),wf(:,36))
  call prop_Q_A(wf(:,35),Q(:,9),ZERO,0_intkind1,wf(:,37))
  call vert_QA_V(wf(:,37),wf(:,14),wf(:,38))
  call vert_QA_V(wf(:,37),wf(:,-1),wf(:,39))
  call counter_SG_G(wf(:,-2),wf(:,21),wf(:,40))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,36),Q(:,36),wf(:,41))
  call vert_AV_Q(wf(:,-1),wf(:,36),wf(:,42))
  call vert_VQ_A(wf(:,-4),wf(:,37),wf(:,43))
  call prop_A_Q(wf(:,42),Q(:,38),ZERO,0_intkind1,wf(:,44))
  call counter_SG_G(wf(:,-2),wf(:,-4),wf(:,45))
  call vert_QA_V(wf(:,37),wf(:,5),wf(:,46))
  call vert_UV_W(wf(:,45),Q(:,20),wf(:,-5),Q(:,32),wf(:,47))
  call vert_AV_Q(wf(:,-1),wf(:,45),wf(:,48))
  call vert_VQ_A(wf(:,-5),wf(:,37),wf(:,49))
  call prop_A_Q(wf(:,48),Q(:,22),ZERO,0_intkind1,wf(:,50))
  call vert_AW_Q(wf(:,-1),wf(:,-3),wf(:,51))
  call prop_A_Q(wf(:,51),Q(:,10),ZERO,0_intkind1,wf(:,52))
  call vert_QA_V(wf(:,4),wf(:,52),wf(:,53))
  call vert_QA_V(wf(:,0),wf(:,52),wf(:,54))
  call vert_VQ_A(wf(:,36),wf(:,0),wf(:,55))
  call vert_AV_Q(wf(:,52),wf(:,-4),wf(:,56))
  call prop_Q_A(wf(:,55),Q(:,37),ZERO,0_intkind1,wf(:,57))
  call vert_QA_V(wf(:,13),wf(:,52),wf(:,58))
  call vert_VQ_A(wf(:,45),wf(:,0),wf(:,59))
  call vert_AV_Q(wf(:,52),wf(:,-5),wf(:,60))
  call prop_Q_A(wf(:,59),Q(:,21),ZERO,0_intkind1,wf(:,61))
  call vert_WQ_A(wf(:,-3),wf(:,4),wf(:,62))
  call vert_AW_Q(wf(:,14),wf(:,-3),wf(:,63))
  call vert_WQ_A(wf(:,-3),wf(:,13),wf(:,64))
  call vert_AW_Q(wf(:,5),wf(:,-3),wf(:,65))
  call counter_AW_Q(wf(:,-1),wf(:,6),wf(:,66))
  call prop_Q_A(wf(:,9),Q(:,49),ZERO,0_intkind1,wf(:,67))
  call counter_AV_Q(wf(:,-1),wf(:,-5),wf(:,68))
  call prop_A_Q(wf(:,68),Q(:,34),ZERO,0_intkind1,wf(:,69))
  call vert_QA_W(wf(:,4),wf(:,69),wf(:,70))
  call prop_Q_A(wf(:,19),Q(:,49),ZERO,0_intkind1,wf(:,71))
  call counter_AV_Q(wf(:,-1),wf(:,21),wf(:,72))
  call vert_AV_Q(wf(:,69),wf(:,-4),wf(:,73))
  call counter_AV_Q(wf(:,-1),wf(:,-4),wf(:,74))
  call prop_A_Q(wf(:,74),Q(:,18),ZERO,0_intkind1,wf(:,75))
  call vert_QA_W(wf(:,13),wf(:,75),wf(:,76))
  call vert_AV_Q(wf(:,75),wf(:,-5),wf(:,77))
  call counter_WQ_A(wf(:,6),wf(:,0),wf(:,78))
  call prop_A_Q(wf(:,17),Q(:,50),ZERO,0_intkind1,wf(:,79))
  call counter_VQ_A(wf(:,-5),wf(:,0),wf(:,80))
  call prop_Q_A(wf(:,80),Q(:,33),ZERO,0_intkind1,wf(:,81))
  call vert_QA_W(wf(:,81),wf(:,14),wf(:,82))
  call prop_A_Q(wf(:,20),Q(:,50),ZERO,0_intkind1,wf(:,83))
  call prop_A_Q(wf(:,22),Q(:,50),ZERO,0_intkind1,wf(:,84))
  call counter_VQ_A(wf(:,21),wf(:,0),wf(:,85))
  call vert_VQ_A(wf(:,-4),wf(:,81),wf(:,86))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,87))
  call prop_Q_A(wf(:,87),Q(:,17),ZERO,0_intkind1,wf(:,88))
  call vert_QA_W(wf(:,88),wf(:,5),wf(:,89))
  call vert_VQ_A(wf(:,-5),wf(:,88),wf(:,90))
  call vert_AW_Q(wf(:,5),wf(:,6),wf(:,91))
  call counter_Q_A(ctqq,wf(:,4),Q(:,17),wf(:,92))
  call prop_A_Q(wf(:,91),Q(:,46),ZERO,0_intkind1,wf(:,93))
  call vert_WQ_A(wf(:,6),wf(:,4),wf(:,94))
  call counter_A_Q(ctqq,wf(:,5),Q(:,34),wf(:,95))
  call prop_Q_A(wf(:,94),Q(:,29),ZERO,0_intkind1,wf(:,96))
  call prop_Q_A(wf(:,92),Q(:,17),ZERO,0_intkind1,wf(:,97))
  call vert_VQ_A(wf(:,-5),wf(:,97),wf(:,98))
  call counter_A_Q(ctqq,wf(:,10),Q(:,14),wf(:,99))
  call vert_AW_Q(wf(:,14),wf(:,6),wf(:,100))
  call counter_Q_A(ctqq,wf(:,13),Q(:,33),wf(:,101))
  call prop_A_Q(wf(:,100),Q(:,30),ZERO,0_intkind1,wf(:,102))
  call vert_WQ_A(wf(:,6),wf(:,13),wf(:,103))
  call counter_A_Q(ctqq,wf(:,14),Q(:,18),wf(:,104))
  call prop_Q_A(wf(:,103),Q(:,45),ZERO,0_intkind1,wf(:,105))
  call counter_Q_A(ctqq,wf(:,18),Q(:,13),wf(:,106))
  call prop_A_Q(wf(:,104),Q(:,18),ZERO,0_intkind1,wf(:,107))
  call vert_AV_Q(wf(:,107),wf(:,-5),wf(:,108))
  call prop_Q_A(wf(:,101),Q(:,33),ZERO,0_intkind1,wf(:,109))
  call vert_VQ_A(wf(:,-4),wf(:,109),wf(:,110))
  call prop_A_Q(wf(:,95),Q(:,34),ZERO,0_intkind1,wf(:,111))
  call vert_AV_Q(wf(:,111),wf(:,-4),wf(:,112))
  call counter_Q_A(ctqq,wf(:,24),Q(:,49),wf(:,113))
  call counter_V_V(ctGG,wf(:,21),Q(:,48),wf(:,114))
  call vert_VQ_A(wf(:,114),wf(:,0),wf(:,115))
  call vert_AV_Q(wf(:,-1),wf(:,114),wf(:,116))
  call vert_UV_W(wf(:,39),Q(:,11),wf(:,-4),Q(:,16),wf(:,117))
  call vert_UV_W(wf(:,39),Q(:,11),wf(:,-5),Q(:,32),wf(:,118))
  call prop_Q_A(wf(:,43),Q(:,25),ZERO,0_intkind1,wf(:,119))
  call vert_QA_V(wf(:,119),wf(:,-1),wf(:,120))
  call prop_Q_A(wf(:,49),Q(:,41),ZERO,0_intkind1,wf(:,121))
  call vert_QA_V(wf(:,121),wf(:,-1),wf(:,122))
  call vert_UV_W(wf(:,54),Q(:,11),wf(:,-4),Q(:,16),wf(:,123))
  call vert_UV_W(wf(:,54),Q(:,11),wf(:,-5),Q(:,32),wf(:,124))
  call prop_A_Q(wf(:,56),Q(:,26),ZERO,0_intkind1,wf(:,125))
  call vert_QA_V(wf(:,0),wf(:,125),wf(:,126))
  call prop_A_Q(wf(:,60),Q(:,42),ZERO,0_intkind1,wf(:,127))
  call vert_QA_V(wf(:,0),wf(:,127),wf(:,128))
  call vert_QA_V(wf(:,18),wf(:,-1),wf(:,129))
  call vert_QA_V(wf(:,0),wf(:,10),wf(:,130))
  call vert_VQ_A(wf(:,-4),wf(:,18),wf(:,131))
  call prop_Q_A(wf(:,131),Q(:,29),ZERO,0_intkind1,wf(:,132))
  call vert_VQ_A(wf(:,-5),wf(:,18),wf(:,133))
  call prop_Q_A(wf(:,133),Q(:,45),ZERO,0_intkind1,wf(:,134))
  call vert_AV_Q(wf(:,10),wf(:,-4),wf(:,135))
  call prop_A_Q(wf(:,135),Q(:,30),ZERO,0_intkind1,wf(:,136))
  call vert_AV_Q(wf(:,10),wf(:,-5),wf(:,137))
  call prop_A_Q(wf(:,137),Q(:,46),ZERO,0_intkind1,wf(:,138))
  call prop_Q_A(wf(:,62),Q(:,25),ZERO,0_intkind1,wf(:,139))
  call vert_QA_V(wf(:,139),wf(:,-1),wf(:,140))
  call prop_A_Q(wf(:,63),Q(:,26),ZERO,0_intkind1,wf(:,141))
  call vert_QA_V(wf(:,0),wf(:,141),wf(:,142))
  call prop_Q_A(wf(:,64),Q(:,41),ZERO,0_intkind1,wf(:,143))
  call vert_QA_V(wf(:,143),wf(:,-1),wf(:,144))
  call prop_A_Q(wf(:,65),Q(:,42),ZERO,0_intkind1,wf(:,145))
  call vert_QA_V(wf(:,0),wf(:,145),wf(:,146))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,17))
  den(2) = 1 / (Q(5,34))
  den(3) = 1 / (Q(5,12) - MW2)
  den(6) = 1 / (Q(5,14))
  den(9) = 1 / (Q(5,33))
  den(10) = 1 / (Q(5,18))
  den(13) = 1 / (Q(5,13))
  den(18) = 1 / (Q(5,48))
  den(20) = 1 / (Q(5,49))
  den(23) = 1 / (Q(5,9))
  den(24) = 1 / (Q(5,36))
  den(27) = 1 / (Q(5,11))
  den(31) = 1 / (Q(5,38))
  den(34) = 1 / (Q(5,20))
  den(38) = 1 / (Q(5,22))
  den(41) = 1 / (Q(5,10))
  den(47) = 1 / (Q(5,37))
  den(53) = 1 / (Q(5,21))
  den(64) = 1 / (Q(5,50))
  den(73) = 1 / (Q(5,46))
  den(77) = 1 / (Q(5,29))
  den(84) = 1 / (Q(5,30))
  den(88) = 1 / (Q(5,45))
  den(105) = 1 / (Q(5,27))
  den(107) = 1 / (Q(5,43))
  den(111) = 1 / (Q(5,25))
  den(114) = 1 / (Q(5,41))
  den(121) = 1 / (Q(5,26))
  den(124) = 1 / (Q(5,42))
  den(127) = 1 / (Q(5,15))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(3)*den(6)
  den(8) = den(1)*den(7)
  den(11) = den(9)*den(10)
  den(12) = den(3)*den(11)
  den(14) = den(3)*den(13)
  den(15) = den(10)*den(14)
  den(16) = den(7)*den(9)
  den(17) = den(2)*den(14)
  den(19) = den(14)*den(18)
  den(21) = den(18)*den(20)
  den(22) = den(3)*den(21)
  den(25) = den(10)*den(23)
  den(26) = den(24)*den(25)
  den(28) = den(23)*den(27)
  den(29) = den(18)*den(28)
  den(30) = den(24)*den(28)
  den(32) = den(24)*den(31)
  den(33) = den(23)*den(32)
  den(35) = den(2)*den(23)
  den(36) = den(34)*den(35)
  den(37) = den(28)*den(34)
  den(39) = den(34)*den(38)
  den(40) = den(23)*den(39)
  den(42) = den(1)*den(41)
  den(43) = den(24)*den(42)
  den(44) = den(27)*den(41)
  den(45) = den(18)*den(44)
  den(46) = den(24)*den(44)
  den(48) = den(24)*den(47)
  den(49) = den(41)*den(48)
  den(50) = den(9)*den(41)
  den(51) = den(34)*den(50)
  den(52) = den(34)*den(44)
  den(54) = den(34)*den(53)
  den(55) = den(41)*den(54)
  den(56) = den(1)*den(32)
  den(57) = den(10)*den(48)
  den(58) = den(9)*den(39)
  den(59) = den(2)*den(54)
  den(60) = den(1)*den(20)
  den(61) = den(3)*den(60)
  den(62) = den(9)*den(20)
  den(63) = den(3)*den(62)
  den(65) = den(10)*den(64)
  den(66) = den(3)*den(65)
  den(67) = den(2)*den(64)
  den(68) = den(3)*den(67)
  den(69) = den(18)*den(64)
  den(70) = den(3)*den(69)
  den(71) = den(7)*den(18)
  den(72) = den(2)*den(3)
  den(74) = den(72)*den(73)
  den(75) = den(1)*den(74)
  den(76) = den(1)*den(3)
  den(78) = den(76)*den(77)
  den(79) = den(2)*den(78)
  den(80) = den(1)**2
  den(81) = den(7)*den(80)
  den(82) = den(7)*den(60)
  den(83) = den(3)*den(10)
  den(85) = den(83)*den(84)
  den(86) = den(9)*den(85)
  den(87) = den(3)*den(9)
  den(89) = den(87)*den(88)
  den(90) = den(10)*den(89)
  den(91) = den(14)*den(65)
  den(92) = den(10)**2
  den(93) = den(14)*den(92)
  den(94) = den(9)**2
  den(95) = den(7)*den(94)
  den(96) = den(7)*den(62)
  den(97) = den(14)*den(67)
  den(98) = den(2)**2
  den(99) = den(14)*den(98)
  den(100) = den(14)*den(69)
  den(101) = den(7)*den(21)
  den(102) = den(18)**2
  den(103) = den(7)*den(102)
  den(104) = den(14)*den(102)
  den(106) = den(25)*den(105)
  den(108) = den(35)*den(107)
  den(109) = den(28)*den(105)
  den(110) = den(28)*den(107)
  den(112) = den(23)*den(111)
  den(113) = den(105)*den(112)
  den(115) = den(23)*den(114)
  den(116) = den(107)*den(115)
  den(117) = den(42)*den(105)
  den(118) = den(50)*den(107)
  den(119) = den(44)*den(105)
  den(120) = den(44)*den(107)
  den(122) = den(41)*den(121)
  den(123) = den(105)*den(122)
  den(125) = den(41)*den(124)
  den(126) = den(107)*den(125)
  den(128) = den(14)*den(127)
  den(129) = den(7)*den(127)
  den(130) = den(14)*den(77)
  den(131) = den(14)*den(88)
  den(132) = den(7)*den(84)
  den(133) = den(7)*den(73)
  den(134) = den(1)*den(111)
  den(135) = den(105)*den(134)
  den(136) = den(10)*den(121)
  den(137) = den(105)*den(136)
  den(138) = den(9)*den(114)
  den(139) = den(107)*den(138)
  den(140) = den(2)*den(124)
  den(141) = den(107)*den(140)
  den(142) = den(1)*den(2)*den(3)
  den(143) = den(3)*den(9)*den(10)
  den(144) = den(3)*den(18)
  den(145) = den(1)*den(133)
  den(146) = den(10)*den(131)
  den(147) = den(9)*den(132)
  den(148) = den(2)*den(130)
  den(149) = den(18)*den(128)
  den(150) = den(18)*den(129)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(84)

  A(1) = cont_VV(wf(:,6),wf(:,7)) * den(5)
  A(2) = cont_QA(wf(:,9),wf(:,10)) * den(8)
  A(3) = cont_VV(wf(:,6),wf(:,15)) * den(12)
  A(4) = cont_QA(wf(:,17),wf(:,18)) * den(15)
  A(5) = cont_QA(wf(:,10),wf(:,19)) * den(16)
  A(6) = cont_QA(wf(:,18),wf(:,20)) * den(17)
  A(7) = cont_QA(wf(:,18),wf(:,22)) * den(19)
  A(8) = cont_QA(wf(:,8),wf(:,24)) * den(22)

  A(9) = cont_VV(wf(:,6),wf(:,25)) * den(5)
  A(10) = cont_QA(wf(:,10),wf(:,26)) * den(8)
  A(11) = cont_VV(wf(:,6),wf(:,27)) * den(12)
  A(12) = cont_QA(wf(:,18),wf(:,28)) * den(15)
  A(13) = cont_QA(wf(:,10),wf(:,29)) * den(16)
  A(14) = cont_QA(wf(:,18),wf(:,30)) * den(17)
  A(15) = cont_QA(wf(:,18),wf(:,32)) * den(19)
  A(16) = cont_QA(wf(:,8),wf(:,34)) * den(22)
  A(17) = cont_VV(wf(:,36),wf(:,38)) * den(26)
  A(18) = cont_VV(wf(:,36),wf(:,38)) * den(26)
  A(19) = cont_VV(wf(:,39),wf(:,40)) * den(29)
  A(20) = cont_VV(wf(:,39),wf(:,40)) * den(29)
  A(21) = cont_VV(wf(:,39),wf(:,41)) * den(30)
  A(22) = cont_VV(wf(:,39),wf(:,41)) * den(30)
  A(23) = cont_QA(wf(:,43),wf(:,44)) * den(33)
  A(24) = cont_QA(wf(:,43),wf(:,44)) * den(33)
  A(25) = cont_VV(wf(:,45),wf(:,46)) * den(36)
  A(26) = cont_VV(wf(:,45),wf(:,46)) * den(36)
  A(27) = cont_VV(wf(:,39),wf(:,47)) * den(37)
  A(28) = cont_VV(wf(:,39),wf(:,47)) * den(37)
  A(29) = cont_QA(wf(:,49),wf(:,50)) * den(40)
  A(30) = cont_QA(wf(:,49),wf(:,50)) * den(40)
  A(31) = cont_VV(wf(:,36),wf(:,53)) * den(43)
  A(32) = cont_VV(wf(:,36),wf(:,53)) * den(43)
  A(33) = cont_VV(wf(:,40),wf(:,54)) * den(45)
  A(34) = cont_VV(wf(:,40),wf(:,54)) * den(45)
  A(35) = cont_VV(wf(:,41),wf(:,54)) * den(46)
  A(36) = cont_VV(wf(:,41),wf(:,54)) * den(46)
  A(37) = cont_QA(wf(:,56),wf(:,57)) * den(49)
  A(38) = cont_QA(wf(:,56),wf(:,57)) * den(49)
  A(39) = cont_VV(wf(:,45),wf(:,58)) * den(51)
  A(40) = cont_VV(wf(:,45),wf(:,58)) * den(51)
  A(41) = cont_VV(wf(:,47),wf(:,54)) * den(52)
  A(42) = cont_VV(wf(:,47),wf(:,54)) * den(52)
  A(43) = cont_QA(wf(:,60),wf(:,61)) * den(55)
  A(44) = cont_QA(wf(:,60),wf(:,61)) * den(55)
  A(45) = cont_QA(wf(:,44),wf(:,62)) * den(56)
  A(46) = cont_QA(wf(:,44),wf(:,62)) * den(56)
  A(47) = cont_QA(wf(:,57),wf(:,63)) * den(57)
  A(48) = cont_QA(wf(:,57),wf(:,63)) * den(57)
  A(49) = cont_QA(wf(:,50),wf(:,64)) * den(58)
  A(50) = cont_QA(wf(:,50),wf(:,64)) * den(58)
  A(51) = cont_QA(wf(:,61),wf(:,65)) * den(59)
  A(52) = cont_QA(wf(:,61),wf(:,65)) * den(59)
  A(53) = cont_QA(wf(:,66),wf(:,67)) * den(61)
  A(54) = cont_VV(wf(:,6),wf(:,70)) * den(5)
  A(55) = cont_QA(wf(:,66),wf(:,71)) * den(63)
  A(56) = cont_QA(wf(:,18),wf(:,72)) * den(19)
  A(57) = cont_QA(wf(:,24),wf(:,66)) * den(22)
  A(58) = cont_QA(wf(:,18),wf(:,73)) * den(17)
  A(59) = cont_VV(wf(:,6),wf(:,76)) * den(12)
  A(60) = cont_QA(wf(:,18),wf(:,77)) * den(15)
  A(61) = cont_QA(wf(:,78),wf(:,79)) * den(66)
  A(62) = cont_VV(wf(:,6),wf(:,82)) * den(12)
  A(63) = cont_QA(wf(:,78),wf(:,83)) * den(68)
  A(64) = cont_QA(wf(:,78),wf(:,84)) * den(70)
  A(65) = cont_QA(wf(:,10),wf(:,85)) * den(71)
  A(66) = cont_QA(wf(:,10),wf(:,86)) * den(16)
  A(67) = cont_VV(wf(:,6),wf(:,89)) * den(5)
  A(68) = cont_QA(wf(:,10),wf(:,90)) * den(8)
  A(69) = cont_QA(wf(:,92),wf(:,93)) * den(75)
  A(70) = cont_QA(wf(:,95),wf(:,96)) * den(79)
  A(71) = cont_QA(wf(:,10),wf(:,98)) * den(81)
  A(72) = cont_QA(wf(:,67),wf(:,99)) * den(82)
  A(73) = cont_QA(wf(:,101),wf(:,102)) * den(86)
  A(74) = cont_QA(wf(:,104),wf(:,105)) * den(90)
  A(75) = cont_QA(wf(:,79),wf(:,106)) * den(91)
  A(76) = cont_QA(wf(:,18),wf(:,108)) * den(93)
  A(77) = cont_QA(wf(:,10),wf(:,110)) * den(95)
  A(78) = cont_QA(wf(:,71),wf(:,99)) * den(96)
  A(79) = cont_QA(wf(:,83),wf(:,106)) * den(97)
  A(80) = cont_QA(wf(:,18),wf(:,112)) * den(99)
  A(81) = cont_QA(wf(:,84),wf(:,106)) * den(100)
  A(82) = cont_QA(wf(:,10),wf(:,113)) * den(101)
  A(83) = cont_QA(wf(:,10),wf(:,115)) * den(103)
  A(84) = cont_QA(wf(:,18),wf(:,116)) * den(104)

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
       +CI*(-A(56)-A(65))*f(6)+(-A(11)-A(55)-A(61))*f(7)+CI*(-A(57)-A(64))*f(8)+CI*(-A(15)-A(16))*f(9)+(-A(18)-A(30)-A(38)-A(40) &
       -A(48)-A(50))*f(17)+CI*(-A(20)-A(22)-A(28)-A(34)-A(36)-A(42))*f(18)+(-A(17)-A(29)-A(37)-A(39)-A(47)-A(49))*f(21)+CI*(-A(19) &
       -A(21)-A(27)-A(33)-A(35)-A(41))*f(22)
  M2(2) = (A(69)+A(70)+A(71)+A(72)+A(79)+A(80))*f(3)+CI*(-A(81)-A(82)-A(83)-A(84))*f(4)+(-A(10)-A(14)-A(54)-A(58)-A(67) &
       -A(68))*f(5)+CI*(A(56)+A(65))*f(6)+(-A(9)-A(53)-A(63))*f(7)+CI*(A(57)+A(64))*f(8)+CI*(A(15)+A(16))*f(9)+(-A(24)-A(26)-A(32) &
       -A(44)-A(46)-A(52))*f(17)+CI*(A(20)+A(22)+A(28)+A(34)+A(36)+A(42))*f(18)+(-A(23)-A(25)-A(31)-A(43)-A(45)-A(51))*f(21) &
       +CI*(A(19)+A(21)+A(27)+A(33)+A(35)+A(41))*f(22)

end subroutine colourvectors

end module ol_loop_pphwjj_udxhwgg_1_/**/REALKIND
