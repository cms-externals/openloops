
module ol_colourmatrix_pphlljj_eexdddxdxh_1_/**/REALKIND
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
end module ol_colourmatrix_pphlljj_eexdddxdxh_1_/**/REALKIND



module ol_forced_parameters_pphlljj_eexdddxdxh_1_/**/REALKIND
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
end module ol_forced_parameters_pphlljj_eexdddxdxh_1_/**/REALKIND

module ol_loop_pphlljj_eexdddxdxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(19), c(27)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:171)
  ! denominators
  complex(REALKIND), save :: den(174)
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
    f(12) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MB*YB)/MQ2sum
    f(13) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw*6._/**/REALKIND)
    f(14) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(15) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MT*YT)/(3._/**/REALKIND*MQ2sum)
    f(16) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MT*YT)/MQ2sum
    f(17) = (eQED**3*gQCD**4*integralnorm*SwF*YT)/(MW*sw*6._/**/REALKIND)
    f(18) = (eQED**3*gQCD**4*integralnorm*SwF*YT)/(MW*sw*3._/**/REALKIND)
    f(19) = (eQED**3*gQCD**4*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

  c = [ 9*CI*f(6), 27*CI*f(6), 18*f(7), 54*f(7), f(8), 3*f(8), 6*f(8), 8*f(8), 10*f(8), 18*f(8), 21*f(8), 24*f(8), 54*f(8), 3*f(9) &
    , 9*f(9), 3*f(10), 9*f(10), 3*f(13), 9*f(13), 3*f(14), 9*f(14), 3*f(17), 9*f(17), 3*f(18), 9*f(18), 3*f(19), 9*f(19) ]
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
  complex(REALKIND) :: A(82)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_Q(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_A(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_S(P(:,7), rMH, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-4),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,3))
  call vert_VQ_A(wf(:,2),wf(:,-3),wf(:,4))
  call vert_SV_V(wf(:,-6),wf(:,3),wf(:,5))
  call prop_Q_A(wf(:,4),Q(:,28),ZERO,0_intkind1,wf(:,6))
  call prop_W_W(wf(:,5),Q(:,67),MZ,1_intkind1,wf(:,7))
  call vert_QA_Z(gZd,wf(:,6),wf(:,-5),wf(:,8))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,9))
  call prop_A_Q(wf(:,9),Q(:,52),ZERO,0_intkind1,wf(:,10))
  call vert_QA_Z(gZd,wf(:,-3),wf(:,10),wf(:,11))
  call vert_QA_V(wf(:,-3),wf(:,-4),wf(:,12))
  call vert_VQ_A(wf(:,12),wf(:,-2),wf(:,13))
  call prop_Q_A(wf(:,13),Q(:,28),ZERO,0_intkind1,wf(:,14))
  call vert_QA_Z(gZd,wf(:,14),wf(:,-5),wf(:,15))
  call vert_AV_Q(wf(:,-5),wf(:,12),wf(:,16))
  call prop_A_Q(wf(:,16),Q(:,56),ZERO,0_intkind1,wf(:,17))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,17),wf(:,18))
  call vert_QA_V(wf(:,-2),wf(:,-5),wf(:,19))
  call vert_VQ_A(wf(:,19),wf(:,-3),wf(:,20))
  call prop_Q_A(wf(:,20),Q(:,44),ZERO,0_intkind1,wf(:,21))
  call vert_QA_Z(gZd,wf(:,21),wf(:,-4),wf(:,22))
  call vert_AV_Q(wf(:,-4),wf(:,19),wf(:,23))
  call prop_A_Q(wf(:,23),Q(:,52),ZERO,0_intkind1,wf(:,24))
  call vert_QA_Z(gZd,wf(:,-3),wf(:,24),wf(:,25))
  call vert_QA_V(wf(:,-3),wf(:,-5),wf(:,26))
  call vert_VQ_A(wf(:,26),wf(:,-2),wf(:,27))
  call prop_Q_A(wf(:,27),Q(:,44),ZERO,0_intkind1,wf(:,28))
  call vert_QA_Z(gZd,wf(:,28),wf(:,-4),wf(:,29))
  call vert_AV_Q(wf(:,-4),wf(:,26),wf(:,30))
  call prop_A_Q(wf(:,30),Q(:,56),ZERO,0_intkind1,wf(:,31))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,31),wf(:,32))
  call counter_GG_V(wf(:,2),Q(:,20),wf(:,26),Q(:,40),wf(:,33))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,34))
  call vert_VQ_A(wf(:,34),wf(:,-3),wf(:,35))
  call counter_SG_G(wf(:,-6),wf(:,2),wf(:,36))
  call prop_Q_A(wf(:,35),Q(:,11),ZERO,0_intkind1,wf(:,37))
  call vert_QA_V(wf(:,37),wf(:,-5),wf(:,38))
  call vert_ZQ_A(gZd,wf(:,3),wf(:,-3),wf(:,39))
  call prop_Q_A(wf(:,39),Q(:,11),ZERO,0_intkind1,wf(:,40))
  call vert_QA_V(wf(:,40),wf(:,-5),wf(:,41))
  call vert_AV_Q(wf(:,-5),wf(:,34),wf(:,42))
  call prop_A_Q(wf(:,42),Q(:,35),ZERO,0_intkind1,wf(:,43))
  call vert_QA_V(wf(:,-3),wf(:,43),wf(:,44))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,3),wf(:,45))
  call prop_A_Q(wf(:,45),Q(:,35),ZERO,0_intkind1,wf(:,46))
  call vert_QA_V(wf(:,-3),wf(:,46),wf(:,47))
  call counter_QA_Z(gZd,wf(:,6),wf(:,-5),wf(:,48))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,49))
  call prop_A_Q(wf(:,49),Q(:,52),ZERO,0_intkind1,wf(:,50))
  call vert_QA_Z(gZd,wf(:,-3),wf(:,50),wf(:,51))
  call counter_GG_V(wf(:,12),Q(:,24),wf(:,19),Q(:,36),wf(:,52))
  call vert_VQ_A(wf(:,34),wf(:,-2),wf(:,53))
  call counter_SG_G(wf(:,-6),wf(:,12),wf(:,54))
  call prop_Q_A(wf(:,53),Q(:,7),ZERO,0_intkind1,wf(:,55))
  call vert_QA_V(wf(:,55),wf(:,-5),wf(:,56))
  call vert_ZQ_A(gZd,wf(:,3),wf(:,-2),wf(:,57))
  call prop_Q_A(wf(:,57),Q(:,7),ZERO,0_intkind1,wf(:,58))
  call vert_QA_V(wf(:,58),wf(:,-5),wf(:,59))
  call vert_QA_V(wf(:,-2),wf(:,43),wf(:,60))
  call vert_QA_V(wf(:,-2),wf(:,46),wf(:,61))
  call counter_QA_Z(gZd,wf(:,14),wf(:,-5),wf(:,62))
  call counter_AV_Q(wf(:,-5),wf(:,12),wf(:,63))
  call prop_A_Q(wf(:,63),Q(:,56),ZERO,0_intkind1,wf(:,64))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,64),wf(:,65))
  call counter_SG_G(wf(:,-6),wf(:,19),wf(:,66))
  call vert_QA_V(wf(:,37),wf(:,-4),wf(:,67))
  call vert_QA_V(wf(:,40),wf(:,-4),wf(:,68))
  call vert_AV_Q(wf(:,-4),wf(:,34),wf(:,69))
  call prop_A_Q(wf(:,69),Q(:,19),ZERO,0_intkind1,wf(:,70))
  call vert_QA_V(wf(:,-3),wf(:,70),wf(:,71))
  call vert_AZ_Q(gZd,wf(:,-4),wf(:,3),wf(:,72))
  call prop_A_Q(wf(:,72),Q(:,19),ZERO,0_intkind1,wf(:,73))
  call vert_QA_V(wf(:,-3),wf(:,73),wf(:,74))
  call counter_SG_G(wf(:,-6),wf(:,26),wf(:,75))
  call vert_QA_V(wf(:,55),wf(:,-4),wf(:,76))
  call vert_QA_V(wf(:,58),wf(:,-4),wf(:,77))
  call vert_QA_V(wf(:,-2),wf(:,70),wf(:,78))
  call vert_QA_V(wf(:,-2),wf(:,73),wf(:,79))
  call counter_QA_Z(gZd,wf(:,21),wf(:,-4),wf(:,80))
  call counter_AV_Q(wf(:,-4),wf(:,19),wf(:,81))
  call prop_A_Q(wf(:,81),Q(:,52),ZERO,0_intkind1,wf(:,82))
  call vert_QA_Z(gZd,wf(:,-3),wf(:,82),wf(:,83))
  call counter_QA_Z(gZd,wf(:,28),wf(:,-4),wf(:,84))
  call counter_AV_Q(wf(:,-4),wf(:,26),wf(:,85))
  call prop_A_Q(wf(:,85),Q(:,56),ZERO,0_intkind1,wf(:,86))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,86),wf(:,87))
  call counter_QA_Z(gZd,wf(:,-3),wf(:,10),wf(:,88))
  call counter_VQ_A(wf(:,2),wf(:,-3),wf(:,89))
  call prop_Q_A(wf(:,89),Q(:,28),ZERO,0_intkind1,wf(:,90))
  call vert_QA_Z(gZd,wf(:,90),wf(:,-5),wf(:,91))
  call counter_QA_Z(gZd,wf(:,-3),wf(:,24),wf(:,92))
  call counter_VQ_A(wf(:,19),wf(:,-3),wf(:,93))
  call prop_Q_A(wf(:,93),Q(:,44),ZERO,0_intkind1,wf(:,94))
  call vert_QA_Z(gZd,wf(:,94),wf(:,-4),wf(:,95))
  call counter_QA_V(wf(:,-3),wf(:,-5),wf(:,96))
  call vert_AV_Q(wf(:,-4),wf(:,96),wf(:,97))
  call prop_A_Q(wf(:,97),Q(:,56),ZERO,0_intkind1,wf(:,98))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,98),wf(:,99))
  call vert_VQ_A(wf(:,96),wf(:,-2),wf(:,100))
  call prop_Q_A(wf(:,100),Q(:,44),ZERO,0_intkind1,wf(:,101))
  call vert_QA_Z(gZd,wf(:,101),wf(:,-4),wf(:,102))
  call counter_QA_V(wf(:,-3),wf(:,-4),wf(:,103))
  call vert_AV_Q(wf(:,-5),wf(:,103),wf(:,104))
  call prop_A_Q(wf(:,104),Q(:,56),ZERO,0_intkind1,wf(:,105))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,105),wf(:,106))
  call vert_VQ_A(wf(:,103),wf(:,-2),wf(:,107))
  call prop_Q_A(wf(:,107),Q(:,28),ZERO,0_intkind1,wf(:,108))
  call vert_QA_Z(gZd,wf(:,108),wf(:,-5),wf(:,109))
  call counter_VQ_A(wf(:,12),wf(:,-2),wf(:,110))
  call prop_Q_A(wf(:,110),Q(:,28),ZERO,0_intkind1,wf(:,111))
  call vert_QA_Z(gZd,wf(:,111),wf(:,-5),wf(:,112))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,17),wf(:,113))
  call counter_VQ_A(wf(:,26),wf(:,-2),wf(:,114))
  call prop_Q_A(wf(:,114),Q(:,44),ZERO,0_intkind1,wf(:,115))
  call vert_QA_Z(gZd,wf(:,115),wf(:,-4),wf(:,116))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,31),wf(:,117))
  call counter_QA_V(wf(:,-2),wf(:,-5),wf(:,118))
  call vert_VQ_A(wf(:,118),wf(:,-3),wf(:,119))
  call prop_Q_A(wf(:,119),Q(:,44),ZERO,0_intkind1,wf(:,120))
  call vert_QA_Z(gZd,wf(:,120),wf(:,-4),wf(:,121))
  call vert_AV_Q(wf(:,-4),wf(:,118),wf(:,122))
  call prop_A_Q(wf(:,122),Q(:,52),ZERO,0_intkind1,wf(:,123))
  call vert_QA_Z(gZd,wf(:,-3),wf(:,123),wf(:,124))
  call counter_QA_V(wf(:,-2),wf(:,-4),wf(:,125))
  call vert_VQ_A(wf(:,125),wf(:,-3),wf(:,126))
  call prop_Q_A(wf(:,126),Q(:,28),ZERO,0_intkind1,wf(:,127))
  call vert_QA_Z(gZd,wf(:,127),wf(:,-5),wf(:,128))
  call vert_AV_Q(wf(:,-5),wf(:,125),wf(:,129))
  call prop_A_Q(wf(:,129),Q(:,52),ZERO,0_intkind1,wf(:,130))
  call vert_QA_Z(gZd,wf(:,-3),wf(:,130),wf(:,131))
  call counter_V_V(ctGG,wf(:,2),Q(:,20),wf(:,132))
  call vert_VQ_A(wf(:,132),wf(:,-3),wf(:,133))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,7),wf(:,134))
  call prop_Q_A(wf(:,133),Q(:,28),ZERO,0_intkind1,wf(:,135))
  call vert_ZQ_A(gZd,wf(:,7),wf(:,-3),wf(:,136))
  call vert_AV_Q(wf(:,-5),wf(:,132),wf(:,137))
  call prop_Q_A(wf(:,136),Q(:,75),ZERO,0_intkind1,wf(:,138))
  call counter_Q_A(ctqq,wf(:,6),Q(:,28),wf(:,139))
  call prop_A_Q(wf(:,134),Q(:,99),ZERO,0_intkind1,wf(:,140))
  call counter_A_Q(ctqq,wf(:,10),Q(:,52),wf(:,141))
  call counter_V_V(ctGG,wf(:,12),Q(:,24),wf(:,142))
  call vert_VQ_A(wf(:,142),wf(:,-2),wf(:,143))
  call prop_Q_A(wf(:,143),Q(:,28),ZERO,0_intkind1,wf(:,144))
  call counter_Q_A(ctqq,wf(:,14),Q(:,28),wf(:,145))
  call vert_ZQ_A(gZd,wf(:,7),wf(:,-2),wf(:,146))
  call counter_A_Q(ctqq,wf(:,17),Q(:,56),wf(:,147))
  call prop_Q_A(wf(:,146),Q(:,71),ZERO,0_intkind1,wf(:,148))
  call vert_AV_Q(wf(:,-5),wf(:,142),wf(:,149))
  call counter_V_V(ctGG,wf(:,19),Q(:,36),wf(:,150))
  call vert_VQ_A(wf(:,150),wf(:,-3),wf(:,151))
  call vert_AZ_Q(gZd,wf(:,-4),wf(:,7),wf(:,152))
  call prop_Q_A(wf(:,151),Q(:,44),ZERO,0_intkind1,wf(:,153))
  call vert_AV_Q(wf(:,-4),wf(:,150),wf(:,154))
  call counter_Q_A(ctqq,wf(:,21),Q(:,44),wf(:,155))
  call prop_A_Q(wf(:,152),Q(:,83),ZERO,0_intkind1,wf(:,156))
  call counter_A_Q(ctqq,wf(:,24),Q(:,52),wf(:,157))
  call counter_V_V(ctGG,wf(:,26),Q(:,40),wf(:,158))
  call vert_VQ_A(wf(:,158),wf(:,-2),wf(:,159))
  call prop_Q_A(wf(:,159),Q(:,44),ZERO,0_intkind1,wf(:,160))
  call counter_Q_A(ctqq,wf(:,28),Q(:,44),wf(:,161))
  call counter_A_Q(ctqq,wf(:,31),Q(:,56),wf(:,162))
  call vert_AV_Q(wf(:,-4),wf(:,158),wf(:,163))
  call vert_QA_V(wf(:,148),wf(:,-4),wf(:,164))
  call vert_QA_V(wf(:,-2),wf(:,156),wf(:,165))
  call vert_QA_V(wf(:,148),wf(:,-5),wf(:,166))
  call vert_QA_V(wf(:,-2),wf(:,140),wf(:,167))
  call vert_QA_V(wf(:,138),wf(:,-4),wf(:,168))
  call vert_QA_V(wf(:,-3),wf(:,156),wf(:,169))
  call vert_QA_V(wf(:,138),wf(:,-5),wf(:,170))
  call vert_QA_V(wf(:,-3),wf(:,140),wf(:,171))

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
  den(30) = 1 / (Q(5,3))
  den(31) = 1 / (Q(5,11))
  den(33) = 1 / (Q(5,84))
  den(38) = 1 / (Q(5,35))
  den(45) = 1 / (Q(5,7))
  den(47) = 1 / (Q(5,88))
  den(54) = 1 / (Q(5,100))
  den(58) = 1 / (Q(5,19))
  den(63) = 1 / (Q(5,104))
  den(72) = 1 / (Q(5,75))
  den(75) = 1 / (Q(5,99))
  den(83) = 1 / (Q(5,71))
  den(91) = 1 / (Q(5,83))
  den(101) = 1 / (Q(5,43))
  den(106) = 1 / (Q(5,39))
  den(111) = 1 / (Q(5,27))
  den(116) = 1 / (Q(5,23))
  den(121) = 1 / (Q(5,87))
  den(124) = 1 / (Q(5,103))
  den(127) = 1 / (Q(5,91))
  den(130) = 1 / (Q(5,107))

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
  den(32) = den(30)*den(31)
  den(34) = den(1)*den(33)
  den(35) = den(32)*den(34)
  den(36) = den(2)*den(31)
  den(37) = den(34)*den(36)
  den(39) = den(30)*den(38)
  den(40) = den(34)*den(39)
  den(41) = den(2)*den(38)
  den(42) = den(34)*den(41)
  den(43) = den(11)*den(17)
  den(44) = den(6)*den(43)
  den(46) = den(30)*den(45)
  den(48) = den(11)*den(47)
  den(49) = den(46)*den(48)
  den(50) = den(2)*den(45)
  den(51) = den(48)*den(50)
  den(52) = den(39)*den(48)
  den(53) = den(41)*den(48)
  den(55) = den(17)*den(54)
  den(56) = den(32)*den(55)
  den(57) = den(36)*den(55)
  den(59) = den(30)*den(58)
  den(60) = den(55)*den(59)
  den(61) = den(2)*den(58)
  den(62) = den(55)*den(61)
  den(64) = den(23)*den(63)
  den(65) = den(46)*den(64)
  den(66) = den(50)*den(64)
  den(67) = den(59)*den(64)
  den(68) = den(61)*den(64)
  den(69) = den(1)**2
  den(70) = den(3)*den(69)
  den(71) = den(6)*den(70)
  den(73) = den(6)*den(72)
  den(74) = den(69)*den(73)
  den(76) = den(6)*den(75)
  den(77) = den(4)*den(76)
  den(78) = den(9)*den(73)
  den(79) = den(11)**2
  den(80) = den(3)*den(79)
  den(81) = den(6)*den(80)
  den(82) = den(12)*den(76)
  den(84) = den(6)*den(83)
  den(85) = den(15)*den(84)
  den(86) = den(79)*den(84)
  den(87) = den(17)**2
  den(88) = den(18)*den(87)
  den(89) = den(6)*den(88)
  den(90) = den(73)*den(87)
  den(92) = den(6)*den(91)
  den(93) = den(19)*den(92)
  den(94) = den(21)*den(73)
  den(95) = den(23)**2
  den(96) = den(18)*den(95)
  den(97) = den(6)*den(96)
  den(98) = den(24)*den(92)
  den(99) = den(26)*den(84)
  den(100) = den(84)*den(95)
  den(102) = den(32)*den(101)
  den(103) = den(36)*den(101)
  den(104) = den(39)*den(101)
  den(105) = den(41)*den(101)
  den(107) = den(46)*den(106)
  den(108) = den(50)*den(106)
  den(109) = den(39)*den(106)
  den(110) = den(41)*den(106)
  den(112) = den(32)*den(111)
  den(113) = den(36)*den(111)
  den(114) = den(59)*den(111)
  den(115) = den(61)*den(111)
  den(117) = den(46)*den(116)
  den(118) = den(50)*den(116)
  den(119) = den(59)*den(116)
  den(120) = den(61)*den(116)
  den(122) = den(84)*den(121)
  den(123) = den(92)*den(121)
  den(125) = den(84)*den(124)
  den(126) = den(76)*den(124)
  den(128) = den(73)*den(127)
  den(129) = den(92)*den(127)
  den(131) = den(73)*den(130)
  den(132) = den(76)*den(130)
  den(133) = den(1)*den(6)*den(23)
  den(134) = den(1)*den(23)*den(30)
  den(135) = den(1)*den(2)*den(23)
  den(136) = den(1)*den(102)
  den(137) = den(1)*den(103)
  den(138) = den(1)*den(104)
  den(139) = den(1)*den(105)
  den(140) = den(1)*den(73)
  den(141) = den(1)*den(76)
  den(142) = den(1)*den(6)
  den(143) = den(6)*den(11)*den(17)
  den(144) = den(11)*den(17)*den(30)
  den(145) = den(2)*den(11)*den(17)
  den(146) = den(11)*den(107)
  den(147) = den(11)*den(108)
  den(148) = den(11)*den(109)
  den(149) = den(11)*den(110)
  den(150) = den(11)*den(84)
  den(151) = den(11)*den(76)
  den(152) = den(6)*den(11)
  den(153) = den(17)*den(112)
  den(154) = den(17)*den(113)
  den(155) = den(17)*den(114)
  den(156) = den(17)*den(115)
  den(157) = den(17)*den(73)
  den(158) = den(17)*den(92)
  den(159) = den(6)*den(17)
  den(160) = den(23)*den(117)
  den(161) = den(23)*den(118)
  den(162) = den(23)*den(119)
  den(163) = den(23)*den(120)
  den(164) = den(23)*den(84)
  den(165) = den(23)*den(92)
  den(166) = den(6)*den(23)
  den(167) = den(1)*den(131)
  den(168) = den(1)*den(132)
  den(169) = den(11)*den(125)
  den(170) = den(11)*den(126)
  den(171) = den(17)*den(128)
  den(172) = den(17)*den(129)
  den(173) = den(23)*den(122)
  den(174) = den(23)*den(123)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(82)

  A(1) = cont_VV(wf(:,7),wf(:,8)) * den(7)
  A(2) = cont_VV(wf(:,7),wf(:,11)) * den(10)
  A(3) = cont_VV(wf(:,7),wf(:,15)) * den(13)
  A(4) = cont_VV(wf(:,7),wf(:,18)) * den(16)
  A(5) = cont_VV(wf(:,7),wf(:,22)) * den(20)
  A(6) = cont_VV(wf(:,7),wf(:,25)) * den(22)
  A(7) = cont_VV(wf(:,7),wf(:,29)) * den(25)
  A(8) = cont_VV(wf(:,7),wf(:,32)) * den(27)

  A(9) = cont_VV(wf(:,7),wf(:,33)) * den(29)
  A(10) = cont_VV(wf(:,36),wf(:,38)) * den(35)
  A(11) = cont_VV(wf(:,36),wf(:,38)) * den(35)
  A(12) = cont_VV(wf(:,36),wf(:,41)) * den(37)
  A(13) = cont_VV(wf(:,36),wf(:,41)) * den(37)
  A(14) = cont_VV(wf(:,36),wf(:,44)) * den(40)
  A(15) = cont_VV(wf(:,36),wf(:,44)) * den(40)
  A(16) = cont_VV(wf(:,36),wf(:,47)) * den(42)
  A(17) = cont_VV(wf(:,36),wf(:,47)) * den(42)
  A(18) = cont_VV(wf(:,7),wf(:,48)) * den(7)
  A(19) = cont_VV(wf(:,7),wf(:,51)) * den(10)
  A(20) = cont_VV(wf(:,7),wf(:,52)) * den(44)
  A(21) = cont_VV(wf(:,54),wf(:,56)) * den(49)
  A(22) = cont_VV(wf(:,54),wf(:,56)) * den(49)
  A(23) = cont_VV(wf(:,54),wf(:,59)) * den(51)
  A(24) = cont_VV(wf(:,54),wf(:,59)) * den(51)
  A(25) = cont_VV(wf(:,54),wf(:,60)) * den(52)
  A(26) = cont_VV(wf(:,54),wf(:,60)) * den(52)
  A(27) = cont_VV(wf(:,54),wf(:,61)) * den(53)
  A(28) = cont_VV(wf(:,54),wf(:,61)) * den(53)
  A(29) = cont_VV(wf(:,7),wf(:,62)) * den(13)
  A(30) = cont_VV(wf(:,7),wf(:,65)) * den(16)
  A(31) = cont_VV(wf(:,66),wf(:,67)) * den(56)
  A(32) = cont_VV(wf(:,66),wf(:,67)) * den(56)
  A(33) = cont_VV(wf(:,66),wf(:,68)) * den(57)
  A(34) = cont_VV(wf(:,66),wf(:,68)) * den(57)
  A(35) = cont_VV(wf(:,66),wf(:,71)) * den(60)
  A(36) = cont_VV(wf(:,66),wf(:,71)) * den(60)
  A(37) = cont_VV(wf(:,66),wf(:,74)) * den(62)
  A(38) = cont_VV(wf(:,66),wf(:,74)) * den(62)
  A(39) = cont_VV(wf(:,75),wf(:,76)) * den(65)
  A(40) = cont_VV(wf(:,75),wf(:,76)) * den(65)
  A(41) = cont_VV(wf(:,75),wf(:,77)) * den(66)
  A(42) = cont_VV(wf(:,75),wf(:,77)) * den(66)
  A(43) = cont_VV(wf(:,75),wf(:,78)) * den(67)
  A(44) = cont_VV(wf(:,75),wf(:,78)) * den(67)
  A(45) = cont_VV(wf(:,75),wf(:,79)) * den(68)
  A(46) = cont_VV(wf(:,75),wf(:,79)) * den(68)
  A(47) = cont_VV(wf(:,7),wf(:,80)) * den(20)
  A(48) = cont_VV(wf(:,7),wf(:,83)) * den(22)
  A(49) = cont_VV(wf(:,7),wf(:,84)) * den(25)
  A(50) = cont_VV(wf(:,7),wf(:,87)) * den(27)
  A(51) = cont_VV(wf(:,7),wf(:,88)) * den(10)
  A(52) = cont_VV(wf(:,7),wf(:,91)) * den(7)
  A(53) = cont_VV(wf(:,7),wf(:,92)) * den(22)
  A(54) = cont_VV(wf(:,7),wf(:,95)) * den(20)
  A(55) = cont_VV(wf(:,7),wf(:,99)) * den(27)
  A(56) = cont_VV(wf(:,7),wf(:,102)) * den(25)
  A(57) = cont_VV(wf(:,7),wf(:,106)) * den(16)
  A(58) = cont_VV(wf(:,7),wf(:,109)) * den(13)
  A(59) = cont_VV(wf(:,7),wf(:,112)) * den(13)
  A(60) = cont_VV(wf(:,7),wf(:,113)) * den(16)
  A(61) = cont_VV(wf(:,7),wf(:,116)) * den(25)
  A(62) = cont_VV(wf(:,7),wf(:,117)) * den(27)
  A(63) = cont_VV(wf(:,7),wf(:,121)) * den(20)
  A(64) = cont_VV(wf(:,7),wf(:,124)) * den(22)
  A(65) = cont_VV(wf(:,7),wf(:,128)) * den(7)
  A(66) = cont_VV(wf(:,7),wf(:,131)) * den(10)
  A(67) = cont_QA(wf(:,134),wf(:,135)) * den(71)
  A(68) = cont_QA(wf(:,137),wf(:,138)) * den(74)
  A(69) = cont_QA(wf(:,139),wf(:,140)) * den(77)
  A(70) = cont_QA(wf(:,138),wf(:,141)) * den(78)
  A(71) = cont_QA(wf(:,134),wf(:,144)) * den(81)
  A(72) = cont_QA(wf(:,140),wf(:,145)) * den(82)
  A(73) = cont_QA(wf(:,147),wf(:,148)) * den(85)
  A(74) = cont_QA(wf(:,148),wf(:,149)) * den(86)
  A(75) = cont_QA(wf(:,152),wf(:,153)) * den(89)
  A(76) = cont_QA(wf(:,138),wf(:,154)) * den(90)
  A(77) = cont_QA(wf(:,155),wf(:,156)) * den(93)
  A(78) = cont_QA(wf(:,138),wf(:,157)) * den(94)
  A(79) = cont_QA(wf(:,152),wf(:,160)) * den(97)
  A(80) = cont_QA(wf(:,156),wf(:,161)) * den(98)
  A(81) = cont_QA(wf(:,148),wf(:,162)) * den(99)
  A(82) = cont_QA(wf(:,148),wf(:,163)) * den(100)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(82)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(3)-A(4)-A(5)-A(6))*f(1))/6._/**/REALKIND+((-A(1)-A(2)-A(7)-A(8))*f(1))/2._/**/REALKIND
  M1(2) = ((A(3)+A(4)+A(5)+A(6))*f(1))/2._/**/REALKIND+((A(1)+A(2)+A(7)+A(8))*f(1))/6._/**/REALKIND

  M2(1) = ((A(71)+A(72)+A(73)+A(74)+A(75)+A(76)+A(77)+A(78))*f(2))/6._/**/REALKIND+((A(67)+A(68)+A(69)+A(70)+A(79)+A(80)+A(81) &
       +A(82))*f(2))/2._/**/REALKIND+((-A(30)-A(48)-A(54)-A(57)-A(58)-A(59)-A(63)-A(64))*f(3))/6._/**/REALKIND+((-A(19)-A(50) &
       -A(52)-A(55)-A(56)-A(61)-A(65)-A(66))*f(3))/2._/**/REALKIND+((-A(29)-A(47)-A(53)-A(60))*f(4))/6._/**/REALKIND+((-A(18) &
       -A(49)-A(51)-A(62))*f(4))/2._/**/REALKIND+(A(9)*f(5))/2._/**/REALKIND+(A(20)*f(5))/6._/**/REALKIND+((-A(22)-A(26)-A(32) &
       -A(36))*f(11))/6._/**/REALKIND+((-A(11)-A(15)-A(40)-A(44))*f(11))/2._/**/REALKIND+((-A(24)-A(28)-A(34) &
       -A(38))*f(12))/6._/**/REALKIND+((-A(13)-A(17)-A(42)-A(46))*f(12))/2._/**/REALKIND+((-A(21)-A(25)-A(31) &
       -A(35))*f(15))/6._/**/REALKIND+((-A(10)-A(14)-A(39)-A(43))*f(15))/2._/**/REALKIND+((-A(23)-A(27)-A(33) &
       -A(37))*f(16))/6._/**/REALKIND+((-A(12)-A(16)-A(41)-A(45))*f(16))/2._/**/REALKIND
  M2(2) = ((-A(71)-A(72)-A(73)-A(74)-A(75)-A(76)-A(77)-A(78))*f(2))/2._/**/REALKIND+((-A(67)-A(68)-A(69)-A(70)-A(79)-A(80)-A(81) &
       -A(82))*f(2))/6._/**/REALKIND+((A(30)+A(48)+A(54)+A(57)+A(58)+A(59)+A(63)+A(64))*f(3))/2._/**/REALKIND+((A(19)+A(50)+A(52) &
       +A(55)+A(56)+A(61)+A(65)+A(66))*f(3))/6._/**/REALKIND+((A(29)+A(47)+A(53)+A(60))*f(4))/2._/**/REALKIND+((A(18)+A(49)+A(51) &
       +A(62))*f(4))/6._/**/REALKIND-(A(9)*f(5))/6._/**/REALKIND-(A(20)*f(5))/2._/**/REALKIND+((A(22)+A(26)+A(32) &
       +A(36))*f(11))/2._/**/REALKIND+((A(11)+A(15)+A(40)+A(44))*f(11))/6._/**/REALKIND+((A(24)+A(28)+A(34) &
       +A(38))*f(12))/2._/**/REALKIND+((A(13)+A(17)+A(42)+A(46))*f(12))/6._/**/REALKIND+((A(21)+A(25)+A(31) &
       +A(35))*f(15))/2._/**/REALKIND+((A(10)+A(14)+A(39)+A(43))*f(15))/6._/**/REALKIND+((A(23)+A(27)+A(33) &
       +A(37))*f(16))/2._/**/REALKIND+((A(12)+A(16)+A(41)+A(45))*f(16))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pphlljj_eexdddxdxh_1_/**/REALKIND
