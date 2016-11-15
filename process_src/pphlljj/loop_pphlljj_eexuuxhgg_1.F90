
module ol_colourmatrix_pphlljj_eexuuxhgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(60,2), K2(2,3), KL(2,3)
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

  K2(1,:) = [ 16, -2,  6]
  K2(2,:) = [ -2, 16,  6]
  K2 = (1._/**/REALKIND / 3) * K2

  KL(1,:) = [ 16, -2,  6]
  KL(2,:) = [ -2, 16,  6]
  KL = (1._/**/REALKIND / 3) * KL

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphlljj_eexuuxhgg_1_/**/REALKIND



module ol_forced_parameters_pphlljj_eexuuxhgg_1_/**/REALKIND
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
end module ol_forced_parameters_pphlljj_eexuuxhgg_1_/**/REALKIND

module ol_loop_pphlljj_eexuuxhgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(36), c(32)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:236)
  ! denominators
  complex(REALKIND), save :: den(269)
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
    f( 2) = (eQED**3*gQCD**2*lambdaHZZ*MW)/(cw**2*sw)
    f( 3) = (CI*countertermnorm*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 4) = (countertermnorm*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 5) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 6) = (countertermnorm*ctGqq*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 7) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 8) = (countertermnorm*ctVqq*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 9) = (countertermnorm*ctVVV*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f(10) = (CI*countertermnorm*ctZGG*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f(11) = (countertermnorm*ctZGG*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f(12) = (CI*eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(2._/**/REALKIND*cw**2*sw)
    f(13) = (CI*eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f(14) = (eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f(15) = (CI*eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(16) = (2*CI*eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(17) = (eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(18) = (2*eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(19) = (2*CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MB*YB)/(3._/**/REALKIND*MQ2sum)
    f(20) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MB*YB)/MQ2sum
    f(21) = (2*countertermnorm*ctHGG*eQED**3*gQCD**4*MB*YB)/(MQ2sum*3._/**/REALKIND)
    f(22) = (countertermnorm*ctHGG*eQED**3*gQCD**4*MB*YB)/MQ2sum
    f(23) = (CI*eQED**3*gQCD**4*integralnorm*SwF*YB)/(6._/**/REALKIND*MW*sw)
    f(24) = (CI*eQED**3*gQCD**4*integralnorm*SwF*YB)/(3._/**/REALKIND*MW*sw)
    f(25) = (CI*eQED**3*gQCD**4*integralnorm*SwF*YB)/(2._/**/REALKIND*MW*sw)
    f(26) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw*6._/**/REALKIND)
    f(27) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw*3._/**/REALKIND)
    f(28) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(29) = (2*CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MT*YT)/(3._/**/REALKIND*MQ2sum)
    f(30) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MT*YT)/MQ2sum
    f(31) = (2*countertermnorm*ctHGG*eQED**3*gQCD**4*MT*YT)/(MQ2sum*3._/**/REALKIND)
    f(32) = (countertermnorm*ctHGG*eQED**3*gQCD**4*MT*YT)/MQ2sum
    f(33) = (CI*eQED**3*gQCD**4*integralnorm*SwF*YT)/(3._/**/REALKIND*MW*sw)
    f(34) = (CI*eQED**3*gQCD**4*integralnorm*SwF*YT)/(2._/**/REALKIND*MW*sw)
    f(35) = (eQED**3*gQCD**4*integralnorm*SwF*YT)/(MW*sw*3._/**/REALKIND)
    f(36) = (eQED**3*gQCD**4*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

  c = [ 9*CI*f(12), 18*CI*f(12), CI*f(13), 3*CI*f(13), 8*CI*f(13), 9*CI*f(13), 18*CI*f(13), f(14), 3*f(14), 8*f(14), 9*f(14) &
    , 3*CI*f(15), 3*CI*f(16), f(17), 3*f(17), f(18), 3*f(18), 3*CI*f(23), 3*CI*f(24), 3*CI*f(25), f(26), 3*f(26), f(27), 3*f(27) &
    , f(28), 3*f(28), 3*CI*f(33), 3*CI*f(34), f(35), 3*f(35), f(36), 3*f(36) ]
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
  complex(REALKIND), intent(out) :: M1(2), M2(3)
  complex(REALKIND) :: A(129)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_S(P(:,5), rMH, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,1))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,2))
  call vert_AV_Q(wf(:,-3),wf(:,-6),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call prop_Q_A(wf(:,2),Q(:,36),ZERO,0_intkind1,wf(:,5))
  call prop_A_Q(wf(:,3),Q(:,72),ZERO,0_intkind1,wf(:,6))
  call vert_SV_V(wf(:,-4),wf(:,4),wf(:,7))
  call vert_QA_Z(gZu,wf(:,5),wf(:,6),wf(:,8))
  call prop_W_W(wf(:,7),Q(:,19),MZ,1_intkind1,wf(:,9))
  call vert_VQ_A(wf(:,-6),wf(:,5),wf(:,10))
  call prop_Q_A(wf(:,10),Q(:,100),ZERO,0_intkind1,wf(:,11))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,9),wf(:,12))
  call vert_VQ_A(wf(:,-6),wf(:,-2),wf(:,13))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,14))
  call prop_Q_A(wf(:,13),Q(:,68),ZERO,0_intkind1,wf(:,15))
  call prop_A_Q(wf(:,14),Q(:,40),ZERO,0_intkind1,wf(:,16))
  call vert_QA_Z(gZu,wf(:,15),wf(:,16),wf(:,17))
  call vert_AV_Q(wf(:,16),wf(:,-6),wf(:,18))
  call prop_A_Q(wf(:,18),Q(:,104),ZERO,0_intkind1,wf(:,19))
  call vert_ZQ_A(gZu,wf(:,9),wf(:,-2),wf(:,20))
  call vert_VQ_A(wf(:,-5),wf(:,15),wf(:,21))
  call prop_Q_A(wf(:,21),Q(:,100),ZERO,0_intkind1,wf(:,22))
  call vert_AV_Q(wf(:,6),wf(:,-5),wf(:,23))
  call prop_A_Q(wf(:,23),Q(:,104),ZERO,0_intkind1,wf(:,24))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-6),Q(:,64),wf(:,25))
  call vert_AV_Q(wf(:,-3),wf(:,25),wf(:,26))
  call prop_A_Q(wf(:,26),Q(:,104),ZERO,0_intkind1,wf(:,27))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,27),wf(:,28))
  call vert_VQ_A(wf(:,25),wf(:,-2),wf(:,29))
  call prop_Q_A(wf(:,29),Q(:,100),ZERO,0_intkind1,wf(:,30))
  call vert_QA_Z(gZu,wf(:,30),wf(:,-3),wf(:,31))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,32))
  call counter_GGG_V(ctZGGG,wf(:,32),wf(:,-5),wf(:,-6),wf(:,33))
  call counter_GGG_V(ctZGGG,wf(:,32),wf(:,-6),wf(:,-5),wf(:,34))
  call counter_GG_V(wf(:,32),Q(:,12),wf(:,25),Q(:,96),wf(:,35))
  call vert_UV_W(wf(:,32),Q(:,12),wf(:,-5),Q(:,32),wf(:,36))
  call counter_VG_G(wf(:,9),wf(:,-6),Q(:,64),wf(:,37),Q(:,83))
  call vert_UV_W(wf(:,32),Q(:,12),wf(:,-6),Q(:,64),wf(:,38))
  call counter_VG_G(wf(:,9),wf(:,-5),Q(:,32),wf(:,39),Q(:,51))
  call counter_QA_Z(gZu,wf(:,5),wf(:,6),wf(:,40))
  call vert_QA_V(wf(:,5),wf(:,-3),wf(:,41))
  call counter_GG_V(wf(:,41),Q(:,44),wf(:,-6),Q(:,64),wf(:,42))
  call counter_VQ_A(wf(:,-6),wf(:,5),wf(:,43))
  call prop_Q_A(wf(:,43),Q(:,100),ZERO,0_intkind1,wf(:,44))
  call counter_QA_Z(gZu,wf(:,15),wf(:,16),wf(:,45))
  call vert_QA_V(wf(:,-2),wf(:,16),wf(:,46))
  call counter_GG_V(wf(:,46),Q(:,44),wf(:,-6),Q(:,64),wf(:,47))
  call counter_AV_Q(wf(:,16),wf(:,-6),wf(:,48))
  call prop_A_Q(wf(:,48),Q(:,104),ZERO,0_intkind1,wf(:,49))
  call vert_QA_V(wf(:,15),wf(:,-3),wf(:,50))
  call counter_GG_V(wf(:,-5),Q(:,32),wf(:,50),Q(:,76),wf(:,51))
  call counter_VQ_A(wf(:,-5),wf(:,15),wf(:,52))
  call prop_Q_A(wf(:,52),Q(:,100),ZERO,0_intkind1,wf(:,53))
  call vert_QA_V(wf(:,-2),wf(:,6),wf(:,54))
  call counter_GG_V(wf(:,-5),Q(:,32),wf(:,54),Q(:,76),wf(:,55))
  call counter_AV_Q(wf(:,6),wf(:,-5),wf(:,56))
  call prop_A_Q(wf(:,56),Q(:,104),ZERO,0_intkind1,wf(:,57))
  call counter_UV_W(wf(:,-5),Q(:,32),wf(:,-6),Q(:,64),wf(:,58))
  call vert_AV_Q(wf(:,-3),wf(:,58),wf(:,59))
  call prop_A_Q(wf(:,59),Q(:,104),ZERO,0_intkind1,wf(:,60))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,60),wf(:,61))
  call vert_VQ_A(wf(:,58),wf(:,-2),wf(:,62))
  call prop_Q_A(wf(:,62),Q(:,100),ZERO,0_intkind1,wf(:,63))
  call vert_QA_Z(gZu,wf(:,63),wf(:,-3),wf(:,64))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,65))
  call counter_SG_G(wf(:,-4),wf(:,-6),wf(:,66))
  call vert_AV_Q(wf(:,-3),wf(:,65),wf(:,67))
  call vert_VQ_A(wf(:,66),wf(:,5),wf(:,68))
  call prop_A_Q(wf(:,67),Q(:,11),ZERO,0_intkind1,wf(:,69))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,4),wf(:,70))
  call prop_A_Q(wf(:,70),Q(:,11),ZERO,0_intkind1,wf(:,71))
  call vert_AV_Q(wf(:,-3),wf(:,66),wf(:,72))
  call vert_VQ_A(wf(:,65),wf(:,5),wf(:,73))
  call prop_A_Q(wf(:,72),Q(:,88),ZERO,0_intkind1,wf(:,74))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,5),wf(:,75))
  call vert_VQ_A(wf(:,65),wf(:,-2),wf(:,76))
  call vert_AV_Q(wf(:,16),wf(:,66),wf(:,77))
  call prop_Q_A(wf(:,76),Q(:,7),ZERO,0_intkind1,wf(:,78))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,-2),wf(:,79))
  call prop_Q_A(wf(:,79),Q(:,7),ZERO,0_intkind1,wf(:,80))
  call vert_VQ_A(wf(:,66),wf(:,-2),wf(:,81))
  call vert_AV_Q(wf(:,16),wf(:,65),wf(:,82))
  call prop_Q_A(wf(:,81),Q(:,84),ZERO,0_intkind1,wf(:,83))
  call vert_AZ_Q(gZu,wf(:,16),wf(:,4),wf(:,84))
  call counter_SG_G(wf(:,-4),wf(:,25),wf(:,85))
  call vert_QA_V(wf(:,78),wf(:,-3),wf(:,86))
  call vert_QA_V(wf(:,80),wf(:,-3),wf(:,87))
  call vert_QA_V(wf(:,-2),wf(:,69),wf(:,88))
  call vert_QA_V(wf(:,-2),wf(:,71),wf(:,89))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,66),Q(:,80),wf(:,90))
  call vert_VQ_A(wf(:,-5),wf(:,78),wf(:,91))
  call vert_VQ_A(wf(:,-5),wf(:,80),wf(:,92))
  call vert_VQ_A(wf(:,-5),wf(:,83),wf(:,93))
  call counter_SG_G(wf(:,-4),wf(:,-5),wf(:,94))
  call vert_VQ_A(wf(:,94),wf(:,15),wf(:,95))
  call vert_AV_Q(wf(:,-3),wf(:,94),wf(:,96))
  call vert_VQ_A(wf(:,65),wf(:,15),wf(:,97))
  call prop_A_Q(wf(:,96),Q(:,56),ZERO,0_intkind1,wf(:,98))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,15),wf(:,99))
  call vert_AV_Q(wf(:,6),wf(:,94),wf(:,100))
  call vert_VQ_A(wf(:,94),wf(:,-2),wf(:,101))
  call vert_AV_Q(wf(:,6),wf(:,65),wf(:,102))
  call prop_Q_A(wf(:,101),Q(:,52),ZERO,0_intkind1,wf(:,103))
  call vert_AZ_Q(gZu,wf(:,6),wf(:,4),wf(:,104))
  call vert_UV_W(wf(:,94),Q(:,48),wf(:,-6),Q(:,64),wf(:,105))
  call vert_VQ_A(wf(:,-6),wf(:,78),wf(:,106))
  call vert_VQ_A(wf(:,-6),wf(:,80),wf(:,107))
  call vert_VQ_A(wf(:,-6),wf(:,103),wf(:,108))
  call counter_AZ_Q(gZu,wf(:,-3),wf(:,9),wf(:,109))
  call counter_AV_Q(wf(:,-3),wf(:,-6),wf(:,110))
  call prop_A_Q(wf(:,110),Q(:,72),ZERO,0_intkind1,wf(:,111))
  call vert_QA_Z(gZu,wf(:,5),wf(:,111),wf(:,112))
  call counter_AV_Q(wf(:,-3),wf(:,25),wf(:,113))
  call prop_A_Q(wf(:,113),Q(:,104),ZERO,0_intkind1,wf(:,114))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,114),wf(:,115))
  call counter_QA_Z(gZu,wf(:,30),wf(:,-3),wf(:,116))
  call vert_AV_Q(wf(:,111),wf(:,-5),wf(:,117))
  call prop_A_Q(wf(:,117),Q(:,104),ZERO,0_intkind1,wf(:,118))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,119))
  call prop_A_Q(wf(:,119),Q(:,40),ZERO,0_intkind1,wf(:,120))
  call vert_QA_Z(gZu,wf(:,15),wf(:,120),wf(:,121))
  call vert_AV_Q(wf(:,120),wf(:,-6),wf(:,122))
  call prop_A_Q(wf(:,122),Q(:,104),ZERO,0_intkind1,wf(:,123))
  call counter_ZQ_A(gZu,wf(:,9),wf(:,-2),wf(:,124))
  call counter_VQ_A(wf(:,-6),wf(:,-2),wf(:,125))
  call prop_Q_A(wf(:,125),Q(:,68),ZERO,0_intkind1,wf(:,126))
  call vert_QA_Z(gZu,wf(:,126),wf(:,16),wf(:,127))
  call counter_QA_Z(gZu,wf(:,-2),wf(:,27),wf(:,128))
  call counter_VQ_A(wf(:,25),wf(:,-2),wf(:,129))
  call prop_Q_A(wf(:,129),Q(:,100),ZERO,0_intkind1,wf(:,130))
  call vert_QA_Z(gZu,wf(:,130),wf(:,-3),wf(:,131))
  call vert_VQ_A(wf(:,-5),wf(:,126),wf(:,132))
  call prop_Q_A(wf(:,132),Q(:,100),ZERO,0_intkind1,wf(:,133))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,134))
  call prop_Q_A(wf(:,134),Q(:,36),ZERO,0_intkind1,wf(:,135))
  call vert_QA_Z(gZu,wf(:,135),wf(:,6),wf(:,136))
  call vert_VQ_A(wf(:,-6),wf(:,135),wf(:,137))
  call prop_Q_A(wf(:,137),Q(:,100),ZERO,0_intkind1,wf(:,138))
  call counter_Q_A(ctqq,wf(:,5),Q(:,36),wf(:,139))
  call prop_Q_A(wf(:,139),Q(:,36),ZERO,0_intkind1,wf(:,140))
  call vert_QA_Z(gZu,wf(:,140),wf(:,6),wf(:,141))
  call counter_A_Q(ctqq,wf(:,6),Q(:,72),wf(:,142))
  call prop_A_Q(wf(:,142),Q(:,72),ZERO,0_intkind1,wf(:,143))
  call vert_QA_Z(gZu,wf(:,5),wf(:,143),wf(:,144))
  call vert_VQ_A(wf(:,-6),wf(:,140),wf(:,145))
  call prop_A_Q(wf(:,12),Q(:,27),ZERO,0_intkind1,wf(:,146))
  call counter_Q_A(ctqq,wf(:,11),Q(:,100),wf(:,147))
  call counter_Q_A(ctqq,wf(:,15),Q(:,68),wf(:,148))
  call prop_Q_A(wf(:,148),Q(:,68),ZERO,0_intkind1,wf(:,149))
  call vert_QA_Z(gZu,wf(:,149),wf(:,16),wf(:,150))
  call counter_A_Q(ctqq,wf(:,16),Q(:,40),wf(:,151))
  call prop_A_Q(wf(:,151),Q(:,40),ZERO,0_intkind1,wf(:,152))
  call vert_QA_Z(gZu,wf(:,15),wf(:,152),wf(:,153))
  call counter_A_Q(ctqq,wf(:,19),Q(:,104),wf(:,154))
  call prop_Q_A(wf(:,20),Q(:,23),ZERO,0_intkind1,wf(:,155))
  call vert_AV_Q(wf(:,152),wf(:,-6),wf(:,156))
  call vert_VQ_A(wf(:,-5),wf(:,149),wf(:,157))
  call counter_Q_A(ctqq,wf(:,22),Q(:,100),wf(:,158))
  call counter_A_Q(ctqq,wf(:,24),Q(:,104),wf(:,159))
  call vert_AV_Q(wf(:,143),wf(:,-5),wf(:,160))
  call counter_A_Q(ctqq,wf(:,27),Q(:,104),wf(:,161))
  call counter_Q_A(ctqq,wf(:,30),Q(:,100),wf(:,162))
  call counter_V_V(ctGG,wf(:,25),Q(:,96),wf(:,163))
  call vert_VQ_A(wf(:,163),wf(:,-2),wf(:,164))
  call prop_Q_A(wf(:,164),Q(:,100),ZERO,0_intkind1,wf(:,165))
  call vert_AV_Q(wf(:,-3),wf(:,163),wf(:,166))
  call vert_QA_V(wf(:,5),wf(:,69),wf(:,167))
  call vert_QA_V(wf(:,5),wf(:,71),wf(:,168))
  call vert_ZQ_A(gZu,wf(:,9),wf(:,5),wf(:,169))
  call prop_Q_A(wf(:,169),Q(:,55),ZERO,0_intkind1,wf(:,170))
  call prop_Q_A(wf(:,73),Q(:,39),ZERO,0_intkind1,wf(:,171))
  call vert_QA_V(wf(:,171),wf(:,-3),wf(:,172))
  call prop_Q_A(wf(:,75),Q(:,39),ZERO,0_intkind1,wf(:,173))
  call vert_QA_V(wf(:,173),wf(:,-3),wf(:,174))
  call vert_QA_V(wf(:,78),wf(:,16),wf(:,175))
  call vert_QA_V(wf(:,80),wf(:,16),wf(:,176))
  call vert_AZ_Q(gZu,wf(:,16),wf(:,9),wf(:,177))
  call prop_A_Q(wf(:,177),Q(:,59),ZERO,0_intkind1,wf(:,178))
  call prop_A_Q(wf(:,82),Q(:,43),ZERO,0_intkind1,wf(:,179))
  call vert_QA_V(wf(:,-2),wf(:,179),wf(:,180))
  call prop_A_Q(wf(:,84),Q(:,43),ZERO,0_intkind1,wf(:,181))
  call vert_QA_V(wf(:,-2),wf(:,181),wf(:,182))
  call vert_QA_V(wf(:,15),wf(:,69),wf(:,183))
  call vert_QA_V(wf(:,15),wf(:,71),wf(:,184))
  call vert_ZQ_A(gZu,wf(:,9),wf(:,15),wf(:,185))
  call prop_Q_A(wf(:,185),Q(:,87),ZERO,0_intkind1,wf(:,186))
  call prop_Q_A(wf(:,97),Q(:,71),ZERO,0_intkind1,wf(:,187))
  call vert_QA_V(wf(:,187),wf(:,-3),wf(:,188))
  call prop_Q_A(wf(:,99),Q(:,71),ZERO,0_intkind1,wf(:,189))
  call vert_QA_V(wf(:,189),wf(:,-3),wf(:,190))
  call vert_QA_V(wf(:,78),wf(:,6),wf(:,191))
  call vert_QA_V(wf(:,80),wf(:,6),wf(:,192))
  call vert_AZ_Q(gZu,wf(:,6),wf(:,9),wf(:,193))
  call prop_A_Q(wf(:,193),Q(:,91),ZERO,0_intkind1,wf(:,194))
  call prop_A_Q(wf(:,102),Q(:,75),ZERO,0_intkind1,wf(:,195))
  call vert_QA_V(wf(:,-2),wf(:,195),wf(:,196))
  call prop_A_Q(wf(:,104),Q(:,75),ZERO,0_intkind1,wf(:,197))
  call vert_QA_V(wf(:,-2),wf(:,197),wf(:,198))
  call vert_UV_W(wf(:,86),Q(:,15),wf(:,-5),Q(:,32),wf(:,199))
  call vert_UV_W(wf(:,87),Q(:,15),wf(:,-5),Q(:,32),wf(:,200))
  call vert_UV_W(wf(:,86),Q(:,15),wf(:,-6),Q(:,64),wf(:,201))
  call vert_UV_W(wf(:,87),Q(:,15),wf(:,-6),Q(:,64),wf(:,202))
  call prop_Q_A(wf(:,91),Q(:,39),ZERO,0_intkind1,wf(:,203))
  call vert_QA_V(wf(:,203),wf(:,-3),wf(:,204))
  call prop_Q_A(wf(:,92),Q(:,39),ZERO,0_intkind1,wf(:,205))
  call vert_QA_V(wf(:,205),wf(:,-3),wf(:,206))
  call prop_Q_A(wf(:,106),Q(:,71),ZERO,0_intkind1,wf(:,207))
  call vert_QA_V(wf(:,207),wf(:,-3),wf(:,208))
  call prop_Q_A(wf(:,107),Q(:,71),ZERO,0_intkind1,wf(:,209))
  call vert_QA_V(wf(:,209),wf(:,-3),wf(:,210))
  call vert_UV_W(wf(:,88),Q(:,15),wf(:,-5),Q(:,32),wf(:,211))
  call vert_UV_W(wf(:,89),Q(:,15),wf(:,-5),Q(:,32),wf(:,212))
  call vert_UV_W(wf(:,88),Q(:,15),wf(:,-6),Q(:,64),wf(:,213))
  call vert_UV_W(wf(:,89),Q(:,15),wf(:,-6),Q(:,64),wf(:,214))
  call vert_AV_Q(wf(:,69),wf(:,-5),wf(:,215))
  call prop_A_Q(wf(:,215),Q(:,43),ZERO,0_intkind1,wf(:,216))
  call vert_QA_V(wf(:,-2),wf(:,216),wf(:,217))
  call vert_AV_Q(wf(:,71),wf(:,-5),wf(:,218))
  call prop_A_Q(wf(:,218),Q(:,43),ZERO,0_intkind1,wf(:,219))
  call vert_QA_V(wf(:,-2),wf(:,219),wf(:,220))
  call vert_AV_Q(wf(:,69),wf(:,-6),wf(:,221))
  call prop_A_Q(wf(:,221),Q(:,75),ZERO,0_intkind1,wf(:,222))
  call vert_QA_V(wf(:,-2),wf(:,222),wf(:,223))
  call vert_AV_Q(wf(:,71),wf(:,-6),wf(:,224))
  call prop_A_Q(wf(:,224),Q(:,75),ZERO,0_intkind1,wf(:,225))
  call vert_QA_V(wf(:,-2),wf(:,225),wf(:,226))
  call vert_QA_V(wf(:,155),wf(:,-3),wf(:,227))
  call vert_QA_V(wf(:,-2),wf(:,146),wf(:,228))
  call vert_VQ_A(wf(:,-5),wf(:,155),wf(:,229))
  call prop_Q_A(wf(:,229),Q(:,55),ZERO,0_intkind1,wf(:,230))
  call vert_VQ_A(wf(:,-6),wf(:,155),wf(:,231))
  call prop_Q_A(wf(:,231),Q(:,87),ZERO,0_intkind1,wf(:,232))
  call vert_AV_Q(wf(:,146),wf(:,-5),wf(:,233))
  call prop_A_Q(wf(:,233),Q(:,59),ZERO,0_intkind1,wf(:,234))
  call vert_AV_Q(wf(:,146),wf(:,-6),wf(:,235))
  call prop_A_Q(wf(:,235),Q(:,91),ZERO,0_intkind1,wf(:,236))

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
  den(2) = 1 / (Q(5,36))
  den(3) = 1 / (Q(5,72))
  den(5) = 1 / (Q(5,19) - MZ2)
  den(8) = 1 / (Q(5,100))
  den(11) = 1 / (Q(5,68))
  den(12) = 1 / (Q(5,40))
  den(15) = 1 / (Q(5,104))
  den(22) = 1 / (Q(5,96))
  den(27) = 1 / (Q(5,12))
  den(31) = 1 / (Q(5,44))
  den(34) = 1 / (Q(5,76))
  den(45) = 1 / (Q(5,3))
  den(46) = 1 / (Q(5,80))
  den(48) = 1 / (Q(5,11))
  den(54) = 1 / (Q(5,88))
  den(60) = 1 / (Q(5,7))
  den(66) = 1 / (Q(5,84))
  den(71) = 1 / (Q(5,112))
  den(86) = 1 / (Q(5,48))
  den(91) = 1 / (Q(5,56))
  den(100) = 1 / (Q(5,52))
  den(120) = 1 / (Q(5,27))
  den(130) = 1 / (Q(5,23))
  den(145) = 1 / (Q(5,47))
  den(150) = 1 / (Q(5,55))
  den(152) = 1 / (Q(5,39))
  den(162) = 1 / (Q(5,59))
  den(164) = 1 / (Q(5,43))
  den(170) = 1 / (Q(5,79))
  den(175) = 1 / (Q(5,87))
  den(177) = 1 / (Q(5,71))
  den(187) = 1 / (Q(5,91))
  den(189) = 1 / (Q(5,75))
  den(194) = 1 / (Q(5,15))
  den(223) = 1 / (Q(5,31))

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
  den(28) = den(6)*den(27)
  den(29) = den(22)*den(27)
  den(30) = den(6)*den(29)
  den(32) = den(27)*den(31)
  den(33) = den(6)*den(32)
  den(35) = den(27)*den(34)
  den(36) = den(6)*den(35)
  den(37) = den(2)*den(31)
  den(38) = den(6)*den(37)
  den(39) = den(12)*den(31)
  den(40) = den(6)*den(39)
  den(41) = den(11)*den(34)
  den(42) = den(6)*den(41)
  den(43) = den(3)*den(34)
  den(44) = den(6)*den(43)
  den(47) = den(2)*den(46)
  den(49) = den(45)*den(48)
  den(50) = den(47)*den(49)
  den(51) = den(1)*den(48)
  den(52) = den(47)*den(51)
  den(53) = den(2)*den(45)
  den(55) = den(46)*den(54)
  den(56) = den(53)*den(55)
  den(57) = den(1)*den(2)
  den(58) = den(55)*den(57)
  den(59) = den(12)*den(46)
  den(61) = den(45)*den(60)
  den(62) = den(59)*den(61)
  den(63) = den(1)*den(60)
  den(64) = den(59)*den(63)
  den(65) = den(12)*den(45)
  den(67) = den(46)*den(66)
  den(68) = den(65)*den(67)
  den(69) = den(1)*den(12)
  den(70) = den(67)*den(69)
  den(72) = den(22)*den(71)
  den(73) = den(61)*den(72)
  den(74) = den(63)*den(72)
  den(75) = den(49)*den(72)
  den(76) = den(51)*den(72)
  den(77) = den(46)*den(71)
  den(78) = den(61)*den(77)
  den(79) = den(63)*den(77)
  den(80) = den(55)*den(61)
  den(81) = den(55)*den(63)
  den(82) = den(49)*den(77)
  den(83) = den(51)*den(77)
  den(84) = den(49)*den(67)
  den(85) = den(51)*den(67)
  den(87) = den(11)*den(86)
  den(88) = den(49)*den(87)
  den(89) = den(51)*den(87)
  den(90) = den(11)*den(45)
  den(92) = den(86)*den(91)
  den(93) = den(90)*den(92)
  den(94) = den(1)*den(11)
  den(95) = den(92)*den(94)
  den(96) = den(3)*den(86)
  den(97) = den(61)*den(96)
  den(98) = den(63)*den(96)
  den(99) = den(3)*den(45)
  den(101) = den(86)*den(100)
  den(102) = den(99)*den(101)
  den(103) = den(1)*den(3)
  den(104) = den(101)*den(103)
  den(105) = den(71)*den(86)
  den(106) = den(61)*den(105)
  den(107) = den(63)*den(105)
  den(108) = den(61)*den(92)
  den(109) = den(63)*den(92)
  den(110) = den(49)*den(105)
  den(111) = den(51)*den(105)
  den(112) = den(49)*den(101)
  den(113) = den(51)*den(101)
  den(114) = den(2)**2
  den(115) = den(3)*den(114)
  den(116) = den(6)*den(115)
  den(117) = den(3)**2
  den(118) = den(2)*den(117)
  den(119) = den(6)*den(118)
  den(121) = den(6)*den(120)
  den(122) = den(114)*den(121)
  den(123) = den(9)*den(121)
  den(124) = den(11)**2
  den(125) = den(12)*den(124)
  den(126) = den(6)*den(125)
  den(127) = den(12)**2
  den(128) = den(11)*den(127)
  den(129) = den(6)*den(128)
  den(131) = den(6)*den(130)
  den(132) = den(16)*den(131)
  den(133) = den(127)*den(131)
  den(134) = den(121)*den(124)
  den(135) = den(18)*den(121)
  den(136) = den(20)*den(131)
  den(137) = den(117)*den(131)
  den(138) = den(23)*den(131)
  den(139) = den(25)*den(121)
  den(140) = den(22)**2
  den(141) = den(8)*den(140)
  den(142) = den(6)*den(141)
  den(143) = den(131)*den(140)
  den(144) = den(2)*den(49)
  den(146) = den(144)*den(145)
  den(147) = den(2)*den(51)
  den(148) = den(145)*den(147)
  den(149) = den(2)*den(6)
  den(151) = den(149)*den(150)
  den(153) = den(53)*den(152)
  den(154) = den(145)*den(153)
  den(155) = den(57)*den(152)
  den(156) = den(145)*den(155)
  den(157) = den(12)*den(61)
  den(158) = den(145)*den(157)
  den(159) = den(12)*den(63)
  den(160) = den(145)*den(159)
  den(161) = den(6)*den(12)
  den(163) = den(161)*den(162)
  den(165) = den(65)*den(164)
  den(166) = den(145)*den(165)
  den(167) = den(69)*den(164)
  den(168) = den(145)*den(167)
  den(169) = den(11)*den(49)
  den(171) = den(169)*den(170)
  den(172) = den(11)*den(51)
  den(173) = den(170)*den(172)
  den(174) = den(6)*den(11)
  den(176) = den(174)*den(175)
  den(178) = den(90)*den(177)
  den(179) = den(170)*den(178)
  den(180) = den(94)*den(177)
  den(181) = den(170)*den(180)
  den(182) = den(3)*den(61)
  den(183) = den(170)*den(182)
  den(184) = den(3)*den(63)
  den(185) = den(170)*den(184)
  den(186) = den(3)*den(6)
  den(188) = den(186)*den(187)
  den(190) = den(99)*den(189)
  den(191) = den(170)*den(190)
  den(192) = den(103)*den(189)
  den(193) = den(170)*den(192)
  den(195) = den(61)*den(194)
  den(196) = den(63)*den(194)
  den(197) = den(49)*den(194)
  den(198) = den(51)*den(194)
  den(199) = den(145)*den(195)
  den(200) = den(145)*den(196)
  den(201) = den(170)*den(195)
  den(202) = den(170)*den(196)
  den(203) = den(61)*den(152)
  den(204) = den(145)*den(203)
  den(205) = den(63)*den(152)
  den(206) = den(145)*den(205)
  den(207) = den(61)*den(177)
  den(208) = den(170)*den(207)
  den(209) = den(63)*den(177)
  den(210) = den(170)*den(209)
  den(211) = den(145)*den(197)
  den(212) = den(145)*den(198)
  den(213) = den(170)*den(197)
  den(214) = den(170)*den(198)
  den(215) = den(49)*den(164)
  den(216) = den(145)*den(215)
  den(217) = den(51)*den(164)
  den(218) = den(145)*den(217)
  den(219) = den(49)*den(189)
  den(220) = den(170)*den(219)
  den(221) = den(51)*den(189)
  den(222) = den(170)*den(221)
  den(224) = den(131)*den(223)
  den(225) = den(121)*den(223)
  den(226) = den(131)*den(150)
  den(227) = den(131)*den(175)
  den(228) = den(121)*den(162)
  den(229) = den(121)*den(187)
  den(230) = den(6)*den(22)*den(27)
  den(231) = den(22)*den(27)*den(45)
  den(232) = den(1)*den(22)*den(27)
  den(233) = den(32)*den(45)
  den(234) = den(1)*den(32)
  den(235) = den(35)*den(45)
  den(236) = den(1)*den(35)
  den(237) = den(27)*den(45)
  den(238) = den(1)*den(27)
  den(239) = den(2)*den(3)*den(6)
  den(240) = den(2)*den(121)
  den(241) = den(37)*den(45)
  den(242) = den(1)*den(37)
  den(243) = den(6)*den(11)*den(12)
  den(244) = den(12)*den(131)
  den(245) = den(39)*den(45)
  den(246) = den(1)*den(39)
  den(247) = den(11)*den(121)
  den(248) = den(41)*den(45)
  den(249) = den(1)*den(41)
  den(250) = den(3)*den(131)
  den(251) = den(43)*den(45)
  den(252) = den(1)*den(43)
  den(253) = den(22)*den(195)
  den(254) = den(22)*den(196)
  den(255) = den(22)*den(197)
  den(256) = den(22)*den(198)
  den(257) = den(22)*den(131)
  den(258) = den(22)*den(121)
  den(259) = den(6)*den(22)
  den(260) = den(3)*den(151)
  den(261) = den(2)*den(188)
  den(262) = den(2)*den(229)
  den(263) = den(12)*den(176)
  den(264) = den(11)*den(163)
  den(265) = den(12)*den(227)
  den(266) = den(11)*den(228)
  den(267) = den(3)*den(226)
  den(268) = den(22)*den(224)
  den(269) = den(22)*den(225)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(129)

  A(1) = cont_VV(wf(:,8),wf(:,9)) * den(7)
  A(2) = cont_QA(wf(:,11),wf(:,12)) * den(10)
  A(3) = cont_VV(wf(:,9),wf(:,17)) * den(14)
  A(4) = cont_QA(wf(:,19),wf(:,20)) * den(17)
  A(5) = cont_QA(wf(:,12),wf(:,22)) * den(19)
  A(6) = cont_QA(wf(:,20),wf(:,24)) * den(21)
  A(7) = cont_VV(wf(:,9),wf(:,28)) * den(24)
  A(8) = cont_VV(wf(:,9),wf(:,31)) * den(26)

  A(9) = cont_VV(wf(:,9),wf(:,33)) * den(28)
  A(10) = cont_VV(wf(:,9),wf(:,34)) * den(28)
  A(11) = cont_VV(wf(:,9),wf(:,35)) * den(30)
  A(12) = cont_VV(wf(:,36),wf(:,37)) * den(33)
  A(13) = cont_VV(wf(:,38),wf(:,39)) * den(36)
  A(14) = cont_VV(wf(:,9),wf(:,40)) * den(7)
  A(15) = cont_VV(wf(:,9),wf(:,42)) * den(38)
  A(16) = cont_QA(wf(:,12),wf(:,44)) * den(10)
  A(17) = cont_VV(wf(:,9),wf(:,45)) * den(14)
  A(18) = cont_VV(wf(:,9),wf(:,47)) * den(40)
  A(19) = cont_QA(wf(:,20),wf(:,49)) * den(17)
  A(20) = cont_VV(wf(:,9),wf(:,51)) * den(42)
  A(21) = cont_QA(wf(:,12),wf(:,53)) * den(19)
  A(22) = cont_VV(wf(:,9),wf(:,55)) * den(44)
  A(23) = cont_QA(wf(:,20),wf(:,57)) * den(21)
  A(24) = cont_VV(wf(:,9),wf(:,61)) * den(24)
  A(25) = cont_VV(wf(:,9),wf(:,64)) * den(26)
  A(26) = cont_QA(wf(:,68),wf(:,69)) * den(50)
  A(27) = cont_QA(wf(:,68),wf(:,69)) * den(50)
  A(28) = cont_QA(wf(:,68),wf(:,71)) * den(52)
  A(29) = cont_QA(wf(:,68),wf(:,71)) * den(52)
  A(30) = cont_QA(wf(:,73),wf(:,74)) * den(56)
  A(31) = cont_QA(wf(:,73),wf(:,74)) * den(56)
  A(32) = cont_QA(wf(:,74),wf(:,75)) * den(58)
  A(33) = cont_QA(wf(:,74),wf(:,75)) * den(58)
  A(34) = cont_QA(wf(:,77),wf(:,78)) * den(62)
  A(35) = cont_QA(wf(:,77),wf(:,78)) * den(62)
  A(36) = cont_QA(wf(:,77),wf(:,80)) * den(64)
  A(37) = cont_QA(wf(:,77),wf(:,80)) * den(64)
  A(38) = cont_QA(wf(:,82),wf(:,83)) * den(68)
  A(39) = cont_QA(wf(:,82),wf(:,83)) * den(68)
  A(40) = cont_QA(wf(:,83),wf(:,84)) * den(70)
  A(41) = cont_QA(wf(:,83),wf(:,84)) * den(70)
  A(42) = cont_VV(wf(:,85),wf(:,86)) * den(73)
  A(43) = cont_VV(wf(:,85),wf(:,86)) * den(73)
  A(44) = cont_VV(wf(:,85),wf(:,87)) * den(74)
  A(45) = cont_VV(wf(:,85),wf(:,87)) * den(74)
  A(46) = cont_VV(wf(:,85),wf(:,88)) * den(75)
  A(47) = cont_VV(wf(:,85),wf(:,88)) * den(75)
  A(48) = cont_VV(wf(:,85),wf(:,89)) * den(76)
  A(49) = cont_VV(wf(:,85),wf(:,89)) * den(76)
  A(50) = cont_VV(wf(:,86),wf(:,90)) * den(78)
  A(51) = cont_VV(wf(:,86),wf(:,90)) * den(78)
  A(52) = cont_VV(wf(:,87),wf(:,90)) * den(79)
  A(53) = cont_VV(wf(:,87),wf(:,90)) * den(79)
  A(54) = cont_QA(wf(:,74),wf(:,91)) * den(80)
  A(55) = cont_QA(wf(:,74),wf(:,91)) * den(80)
  A(56) = cont_QA(wf(:,74),wf(:,92)) * den(81)
  A(57) = cont_QA(wf(:,74),wf(:,92)) * den(81)
  A(58) = cont_VV(wf(:,88),wf(:,90)) * den(82)
  A(59) = cont_VV(wf(:,88),wf(:,90)) * den(82)
  A(60) = cont_VV(wf(:,89),wf(:,90)) * den(83)
  A(61) = cont_VV(wf(:,89),wf(:,90)) * den(83)
  A(62) = cont_QA(wf(:,69),wf(:,93)) * den(84)
  A(63) = cont_QA(wf(:,69),wf(:,93)) * den(84)
  A(64) = cont_QA(wf(:,71),wf(:,93)) * den(85)
  A(65) = cont_QA(wf(:,71),wf(:,93)) * den(85)
  A(66) = cont_QA(wf(:,69),wf(:,95)) * den(88)
  A(67) = cont_QA(wf(:,69),wf(:,95)) * den(88)
  A(68) = cont_QA(wf(:,71),wf(:,95)) * den(89)
  A(69) = cont_QA(wf(:,71),wf(:,95)) * den(89)
  A(70) = cont_QA(wf(:,97),wf(:,98)) * den(93)
  A(71) = cont_QA(wf(:,97),wf(:,98)) * den(93)
  A(72) = cont_QA(wf(:,98),wf(:,99)) * den(95)
  A(73) = cont_QA(wf(:,98),wf(:,99)) * den(95)
  A(74) = cont_QA(wf(:,78),wf(:,100)) * den(97)
  A(75) = cont_QA(wf(:,78),wf(:,100)) * den(97)
  A(76) = cont_QA(wf(:,80),wf(:,100)) * den(98)
  A(77) = cont_QA(wf(:,80),wf(:,100)) * den(98)
  A(78) = cont_QA(wf(:,102),wf(:,103)) * den(102)
  A(79) = cont_QA(wf(:,102),wf(:,103)) * den(102)
  A(80) = cont_QA(wf(:,103),wf(:,104)) * den(104)
  A(81) = cont_QA(wf(:,103),wf(:,104)) * den(104)
  A(82) = cont_VV(wf(:,86),wf(:,105)) * den(106)
  A(83) = cont_VV(wf(:,86),wf(:,105)) * den(106)
  A(84) = cont_VV(wf(:,87),wf(:,105)) * den(107)
  A(85) = cont_VV(wf(:,87),wf(:,105)) * den(107)
  A(86) = cont_QA(wf(:,98),wf(:,106)) * den(108)
  A(87) = cont_QA(wf(:,98),wf(:,106)) * den(108)
  A(88) = cont_QA(wf(:,98),wf(:,107)) * den(109)
  A(89) = cont_QA(wf(:,98),wf(:,107)) * den(109)
  A(90) = cont_VV(wf(:,88),wf(:,105)) * den(110)
  A(91) = cont_VV(wf(:,88),wf(:,105)) * den(110)
  A(92) = cont_VV(wf(:,89),wf(:,105)) * den(111)
  A(93) = cont_VV(wf(:,89),wf(:,105)) * den(111)
  A(94) = cont_QA(wf(:,69),wf(:,108)) * den(112)
  A(95) = cont_QA(wf(:,69),wf(:,108)) * den(112)
  A(96) = cont_QA(wf(:,71),wf(:,108)) * den(113)
  A(97) = cont_QA(wf(:,71),wf(:,108)) * den(113)
  A(98) = cont_QA(wf(:,11),wf(:,109)) * den(10)
  A(99) = cont_VV(wf(:,9),wf(:,112)) * den(7)
  A(100) = cont_QA(wf(:,22),wf(:,109)) * den(19)
  A(101) = cont_VV(wf(:,9),wf(:,115)) * den(24)
  A(102) = cont_VV(wf(:,9),wf(:,116)) * den(26)
  A(103) = cont_QA(wf(:,20),wf(:,118)) * den(21)
  A(104) = cont_VV(wf(:,9),wf(:,121)) * den(14)
  A(105) = cont_QA(wf(:,20),wf(:,123)) * den(17)
  A(106) = cont_QA(wf(:,19),wf(:,124)) * den(17)
  A(107) = cont_VV(wf(:,9),wf(:,127)) * den(14)
  A(108) = cont_QA(wf(:,24),wf(:,124)) * den(21)
  A(109) = cont_VV(wf(:,9),wf(:,128)) * den(24)
  A(110) = cont_VV(wf(:,9),wf(:,131)) * den(26)
  A(111) = cont_QA(wf(:,12),wf(:,133)) * den(19)
  A(112) = cont_VV(wf(:,9),wf(:,136)) * den(7)
  A(113) = cont_QA(wf(:,12),wf(:,138)) * den(10)
  A(114) = cont_VV(wf(:,9),wf(:,141)) * den(116)
  A(115) = cont_VV(wf(:,9),wf(:,144)) * den(119)
  A(116) = cont_QA(wf(:,145),wf(:,146)) * den(122)
  A(117) = cont_QA(wf(:,146),wf(:,147)) * den(123)
  A(118) = cont_VV(wf(:,9),wf(:,150)) * den(126)
  A(119) = cont_VV(wf(:,9),wf(:,153)) * den(129)
  A(120) = cont_QA(wf(:,154),wf(:,155)) * den(132)
  A(121) = cont_QA(wf(:,155),wf(:,156)) * den(133)
  A(122) = cont_QA(wf(:,146),wf(:,157)) * den(134)
  A(123) = cont_QA(wf(:,146),wf(:,158)) * den(135)
  A(124) = cont_QA(wf(:,155),wf(:,159)) * den(136)
  A(125) = cont_QA(wf(:,155),wf(:,160)) * den(137)
  A(126) = cont_QA(wf(:,155),wf(:,161)) * den(138)
  A(127) = cont_QA(wf(:,146),wf(:,162)) * den(139)
  A(128) = cont_QA(wf(:,12),wf(:,165)) * den(142)
  A(129) = cont_QA(wf(:,155),wf(:,166)) * den(143)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(129)
  complex(REALKIND), intent(out) :: M1(2), M2(3)

  M1(1) = (A(3)+A(4)+A(5))*f(1)+CI*(A(7)+A(8))*f(2)
  M1(2) = (A(1)+A(2)+A(6))*f(1)+CI*(-A(7)-A(8))*f(2)

  M2(1) = (A(9)*f(3))/2._/**/REALKIND+(-A(118)-A(119)-A(120)-A(121)-A(122)-A(123))*f(3)+CI*(-A(126)-A(127)-A(128)-A(129))*f(4) &
       +(A(19)+A(21)+A(104)+A(105)+A(107)+A(111))*f(5)+CI*(A(101)+A(110))*f(6)+(A(17)+A(100)+A(106))*f(7)+CI*(A(102)+A(109))*f(8) &
       +CI*(A(24)+A(25))*f(9)+CI*(A(11)+A(12)-A(13))*f(10)+(-A(18)-A(20))*f(11)+(-A(35)-A(39)-A(63)-A(67)-A(71)-A(87))*f(19) &
       +(A(37)+A(41)+A(65)+A(69)+A(73)+A(89))*f(20)+CI*(-A(43)-A(47)-A(51)-A(59)-A(83)-A(91))*f(21)+CI*(A(45)+A(49)+A(53)+A(61) &
       +A(85)+A(93))*f(22)+(-A(34)-A(38)-A(62)-A(66)-A(70)-A(86))*f(29)+(A(36)+A(40)+A(64)+A(68)+A(72)+A(88))*f(30)+CI*(-A(42) &
       -A(46)-A(50)-A(58)-A(82)-A(90))*f(31)+CI*(A(44)+A(48)+A(52)+A(60)+A(84)+A(92))*f(32)
  M2(2) = (A(10)*f(3))/2._/**/REALKIND+(-A(114)-A(115)-A(116)-A(117)-A(124)-A(125))*f(3)+CI*(A(126)+A(127)+A(128)+A(129))*f(4) &
       +(A(16)+A(23)+A(99)+A(103)+A(112)+A(113))*f(5)+CI*(-A(101)-A(110))*f(6)+(A(14)+A(98)+A(108))*f(7)+CI*(-A(102)-A(109))*f(8) &
       +CI*(-A(24)-A(25))*f(9)+CI*(-A(11)-A(12)+A(13))*f(10)+(-A(15)-A(22))*f(11)+(-A(27)-A(31)-A(55)-A(75)-A(79)-A(95))*f(19) &
       +(A(29)+A(33)+A(57)+A(77)+A(81)+A(97))*f(20)+CI*(A(43)+A(47)+A(51)+A(59)+A(83)+A(91))*f(21)+CI*(-A(45)-A(49)-A(53)-A(61) &
       -A(85)-A(93))*f(22)+(-A(26)-A(30)-A(54)-A(74)-A(78)-A(94))*f(29)+(A(28)+A(32)+A(56)+A(76)+A(80)+A(96))*f(30)+CI*(A(42) &
       +A(46)+A(50)+A(58)+A(82)+A(90))*f(31)+CI*(-A(44)-A(48)-A(52)-A(60)-A(84)-A(92))*f(32)
  M2(3) = ((-A(9)-A(10))*f(3))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pphlljj_eexuuxhgg_1_/**/REALKIND
