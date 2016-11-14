
module ol_colourmatrix_pphlljj_nenexddxbbxh_1_/**/REALKIND
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
end module ol_colourmatrix_pphlljj_nenexddxbbxh_1_/**/REALKIND



module ol_forced_parameters_pphlljj_nenexddxbbxh_1_/**/REALKIND
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
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphlljj_nenexddxbbxh_1_/**/REALKIND

module ol_loop_pphlljj_nenexddxbbxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(28), c(36)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:194)
  ! denominators
  complex(REALKIND), save :: den(225)
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
    f( 3) = (CI*countertermnorm*ctGbb*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 4) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 5) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 6) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 7) = (countertermnorm*ctZGG*eQED**3*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 8) = (CI*eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f( 9) = (eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw*2._/**/REALKIND)
    f(10) = (eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f(11) = (eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(12) = (2*eQED**3*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(13) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MB*YB)/MQ2sum
    f(14) = (CI*eQED**3*gQCD**2*YB)/(2._/**/REALKIND*MW*sw)
    f(15) = (CI*countertermnorm*eQED**3*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(16) = (CI*countertermnorm*ctGbb*eQED**3*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(17) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(18) = (CI*countertermnorm*ctSbb*eQED**3*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(19) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(20) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(21) = (countertermnorm*ctZGG*eQED**3*gQCD**4*YB)/(MW*sw*2._/**/REALKIND)
    f(22) = (CI*eQED**3*gQCD**4*integralnorm*SwB*YB)/(2._/**/REALKIND*MW*sw)
    f(23) = (eQED**3*gQCD**4*integralnorm*SwB*YB)/(MW*sw*4._/**/REALKIND)
    f(24) = (eQED**3*gQCD**4*integralnorm*SwB*YB)/(MW*sw*2._/**/REALKIND)
    f(25) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(26) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw)
    f(27) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MT*YT)/MQ2sum
    f(28) = (eQED**3*gQCD**4*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

  c = [ 9*CI*f(8), 27*CI*f(8), 18*f(9), 54*f(9), f(10), 3*f(10), 6*f(10), 8*f(10), 10*f(10), 18*f(10), 21*f(10), 24*f(10) &
    , 54*f(10), 3*f(11), 9*f(11), 3*f(12), 9*f(12), 9*CI*f(22), 27*CI*f(22), 18*f(23), 54*f(23), f(24), 3*f(24), 6*f(24), 8*f(24) &
    , 10*f(24), 18*f(24), 21*f(24), 24*f(24), 54*f(24), 3*f(25), 9*f(25), 3*f(26), 9*f(26), 3*f(28), 9*f(28) ]
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
  complex(REALKIND) :: A(115)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rMB, H(5), wf(:,-4))
  call wf_A(P(:,6), rMB, H(6), wf(:,-5))
  call wf_S(P(:,7), rMH, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_QS_A(gH,wf(:,-4),wf(:,-6),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call prop_Q_A(wf(:,3),Q(:,80),MB,1_intkind1,wf(:,5))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,6))
  call vert_VQ_A(wf(:,2),wf(:,5),wf(:,7))
  call prop_A_Q(wf(:,6),Q(:,35),MB,1_intkind1,wf(:,8))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,9))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,5),wf(:,10))
  call prop_A_Q(wf(:,9),Q(:,44),MB,1_intkind1,wf(:,11))
  call vert_SA_Q(gH,wf(:,-6),wf(:,-5),wf(:,12))
  call prop_A_Q(wf(:,12),Q(:,96),MB,1_intkind1,wf(:,13))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,14))
  call vert_AV_Q(wf(:,13),wf(:,2),wf(:,15))
  call prop_Q_A(wf(:,14),Q(:,19),MB,1_intkind1,wf(:,16))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,17))
  call vert_AZ_Q(gZd,wf(:,13),wf(:,4),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,28),MB,1_intkind1,wf(:,19))
  call vert_QS_A(gH,wf(:,16),wf(:,-6),wf(:,20))
  call vert_QS_A(gH,wf(:,19),wf(:,-6),wf(:,21))
  call vert_SV_V(wf(:,-6),wf(:,4),wf(:,22))
  call prop_W_W(wf(:,22),Q(:,67),MZ,1_intkind1,wf(:,23))
  call vert_QA_Z(gZd,wf(:,19),wf(:,-5),wf(:,24))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,11),wf(:,25))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,26))
  call vert_VQ_A(wf(:,26),wf(:,-2),wf(:,27))
  call prop_Q_A(wf(:,27),Q(:,52),ZERO,0_intkind1,wf(:,28))
  call vert_QA_Z(gZd,wf(:,28),wf(:,-3),wf(:,29))
  call vert_AV_Q(wf(:,-3),wf(:,26),wf(:,30))
  call prop_A_Q(wf(:,30),Q(:,56),ZERO,0_intkind1,wf(:,31))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,31),wf(:,32))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,-2),wf(:,33))
  call vert_QA_V(wf(:,5),wf(:,-5),wf(:,34))
  call prop_Q_A(wf(:,33),Q(:,7),ZERO,0_intkind1,wf(:,35))
  call vert_QA_V(wf(:,35),wf(:,-3),wf(:,36))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,4),wf(:,37))
  call prop_A_Q(wf(:,37),Q(:,11),ZERO,0_intkind1,wf(:,38))
  call vert_QA_V(wf(:,-2),wf(:,38),wf(:,39))
  call vert_QA_V(wf(:,-4),wf(:,13),wf(:,40))
  call counter_GG_V(wf(:,2),Q(:,12),wf(:,26),Q(:,48),wf(:,41))
  call counter_VQ_A(wf(:,2),wf(:,5),wf(:,42))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,5),wf(:,43))
  call counter_VG_G(wf(:,4),wf(:,2),Q(:,12),wf(:,44),Q(:,15))
  call counter_AV_Q(wf(:,13),wf(:,2),wf(:,45))
  call counter_AZ_Q(gZd,wf(:,13),wf(:,4),wf(:,46))
  call counter_QS_A(gH,wf(:,16),wf(:,-6),wf(:,47))
  call counter_SG_G(wf(:,-6),wf(:,2),wf(:,48))
  call vert_QA_V(wf(:,16),wf(:,-5),wf(:,49))
  call counter_QS_A(gH,wf(:,19),wf(:,-6),wf(:,50))
  call vert_QA_V(wf(:,-4),wf(:,8),wf(:,51))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,52))
  call prop_Q_A(wf(:,10),Q(:,83),MB,1_intkind1,wf(:,53))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,54))
  call prop_Q_A(wf(:,7),Q(:,92),MB,1_intkind1,wf(:,55))
  call prop_A_Q(wf(:,52),Q(:,44),MB,1_intkind1,wf(:,56))
  call counter_QA_Z(gZd,wf(:,19),wf(:,-5),wf(:,57))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,56),wf(:,58))
  call prop_A_Q(wf(:,54),Q(:,35),MB,1_intkind1,wf(:,59))
  call counter_SA_Q(gH,wf(:,-6),wf(:,-5),wf(:,60))
  call prop_A_Q(wf(:,60),Q(:,96),MB,1_intkind1,wf(:,61))
  call vert_AV_Q(wf(:,61),wf(:,2),wf(:,62))
  call vert_AZ_Q(gZd,wf(:,61),wf(:,4),wf(:,63))
  call counter_VQ_A(wf(:,2),wf(:,-4),wf(:,64))
  call prop_A_Q(wf(:,18),Q(:,99),MB,1_intkind1,wf(:,65))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,66))
  call prop_A_Q(wf(:,15),Q(:,108),MB,1_intkind1,wf(:,67))
  call prop_Q_A(wf(:,64),Q(:,28),MB,1_intkind1,wf(:,68))
  call vert_QS_A(gH,wf(:,68),wf(:,-6),wf(:,69))
  call counter_QA_Z(gZd,wf(:,-4),wf(:,11),wf(:,70))
  call vert_QA_Z(gZd,wf(:,68),wf(:,-5),wf(:,71))
  call prop_Q_A(wf(:,66),Q(:,19),MB,1_intkind1,wf(:,72))
  call vert_QS_A(gH,wf(:,72),wf(:,-6),wf(:,73))
  call counter_QS_A(gH,wf(:,-4),wf(:,-6),wf(:,74))
  call prop_Q_A(wf(:,74),Q(:,80),MB,1_intkind1,wf(:,75))
  call vert_VQ_A(wf(:,2),wf(:,75),wf(:,76))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,75),wf(:,77))
  call counter_SG_G(wf(:,-6),wf(:,26),wf(:,78))
  call counter_QA_V(wf(:,5),wf(:,-5),wf(:,79))
  call vert_QA_V(wf(:,-4),wf(:,61),wf(:,80))
  call counter_QA_V(wf(:,-4),wf(:,13),wf(:,81))
  call vert_QA_V(wf(:,75),wf(:,-5),wf(:,82))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,83))
  call vert_AV_Q(wf(:,-3),wf(:,83),wf(:,84))
  call prop_A_Q(wf(:,84),Q(:,56),ZERO,0_intkind1,wf(:,85))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,85),wf(:,86))
  call vert_VQ_A(wf(:,83),wf(:,-2),wf(:,87))
  call prop_Q_A(wf(:,87),Q(:,52),ZERO,0_intkind1,wf(:,88))
  call vert_QA_Z(gZd,wf(:,88),wf(:,-3),wf(:,89))
  call counter_QA_Z(gZd,wf(:,28),wf(:,-3),wf(:,90))
  call counter_AV_Q(wf(:,-3),wf(:,26),wf(:,91))
  call prop_A_Q(wf(:,91),Q(:,56),ZERO,0_intkind1,wf(:,92))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,92),wf(:,93))
  call counter_QA_V(wf(:,35),wf(:,-3),wf(:,94))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,4),wf(:,95))
  call prop_A_Q(wf(:,95),Q(:,11),ZERO,0_intkind1,wf(:,96))
  call vert_QA_V(wf(:,-2),wf(:,96),wf(:,97))
  call counter_VQ_A(wf(:,26),wf(:,-2),wf(:,98))
  call prop_Q_A(wf(:,98),Q(:,52),ZERO,0_intkind1,wf(:,99))
  call vert_QA_Z(gZd,wf(:,99),wf(:,-3),wf(:,100))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,31),wf(:,101))
  call counter_QA_V(wf(:,-2),wf(:,38),wf(:,102))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,-2),wf(:,103))
  call prop_Q_A(wf(:,103),Q(:,7),ZERO,0_intkind1,wf(:,104))
  call vert_QA_V(wf(:,104),wf(:,-3),wf(:,105))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,106))
  call vert_VQ_A(wf(:,106),wf(:,5),wf(:,107))
  call vert_AV_Q(wf(:,-5),wf(:,106),wf(:,108))
  call prop_A_Q(wf(:,108),Q(:,44),MB,1_intkind1,wf(:,109))
  call vert_AV_Q(wf(:,13),wf(:,106),wf(:,110))
  call vert_VQ_A(wf(:,106),wf(:,-4),wf(:,111))
  call prop_Q_A(wf(:,111),Q(:,28),MB,1_intkind1,wf(:,112))
  call vert_QS_A(gH,wf(:,112),wf(:,-6),wf(:,113))
  call vert_QA_Z(gZd,wf(:,112),wf(:,-5),wf(:,114))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,109),wf(:,115))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,116))
  call vert_AV_Q(wf(:,-5),wf(:,116),wf(:,117))
  call vert_VQ_A(wf(:,116),wf(:,5),wf(:,118))
  call counter_Q_A(ctbb,wf(:,5),Q(:,80),wf(:,119))
  call prop_Q_A(wf(:,119),Q(:,80),MB,1_intkind1,wf(:,120))
  call vert_VQ_A(wf(:,2),wf(:,120),wf(:,121))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,120),wf(:,122))
  call counter_A_Q(ctbb,wf(:,8),Q(:,35),wf(:,123))
  call counter_A_Q(ctbb,wf(:,11),Q(:,44),wf(:,124))
  call vert_VQ_A(wf(:,116),wf(:,-4),wf(:,125))
  call vert_VQ_A(wf(:,116),wf(:,16),wf(:,126))
  call counter_Q_A(ctbb,wf(:,16),Q(:,19),wf(:,127))
  call counter_Q_A(ctbb,wf(:,19),Q(:,28),wf(:,128))
  call counter_A_Q(ctbb,wf(:,13),Q(:,96),wf(:,129))
  call prop_A_Q(wf(:,129),Q(:,96),MB,1_intkind1,wf(:,130))
  call vert_AV_Q(wf(:,130),wf(:,2),wf(:,131))
  call vert_AZ_Q(gZd,wf(:,130),wf(:,4),wf(:,132))
  call vert_SA_Q(gH,wf(:,-6),wf(:,8),wf(:,133))
  call prop_Q_A(wf(:,125),Q(:,28),MB,1_intkind1,wf(:,134))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,23),wf(:,135))
  call prop_A_Q(wf(:,117),Q(:,44),MB,1_intkind1,wf(:,136))
  call vert_ZQ_A(gZd,wf(:,23),wf(:,-4),wf(:,137))
  call prop_Q_A(wf(:,137),Q(:,83),MB,1_intkind1,wf(:,138))
  call prop_A_Q(wf(:,135),Q(:,99),MB,1_intkind1,wf(:,139))
  call vert_SA_Q(gH,wf(:,-6),wf(:,11),wf(:,140))
  call prop_A_Q(wf(:,140),Q(:,108),MB,1_intkind1,wf(:,141))
  call prop_A_Q(wf(:,133),Q(:,99),MB,1_intkind1,wf(:,142))
  call prop_Q_A(wf(:,20),Q(:,83),MB,1_intkind1,wf(:,143))
  call prop_Q_A(wf(:,21),Q(:,92),MB,1_intkind1,wf(:,144))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,23),wf(:,145))
  call counter_Q_A(ctqq,wf(:,28),Q(:,52),wf(:,146))
  call prop_A_Q(wf(:,145),Q(:,75),ZERO,0_intkind1,wf(:,147))
  call vert_ZQ_A(gZd,wf(:,23),wf(:,-2),wf(:,148))
  call counter_A_Q(ctqq,wf(:,31),Q(:,56),wf(:,149))
  call prop_Q_A(wf(:,148),Q(:,71),ZERO,0_intkind1,wf(:,150))
  call counter_V_V(ctGG,wf(:,26),Q(:,48),wf(:,151))
  call vert_VQ_A(wf(:,151),wf(:,-2),wf(:,152))
  call prop_Q_A(wf(:,152),Q(:,52),ZERO,0_intkind1,wf(:,153))
  call vert_AV_Q(wf(:,-3),wf(:,151),wf(:,154))
  call vert_AV_Q(wf(:,-3),wf(:,34),wf(:,155))
  call counter_Q_A(ctqq,wf(:,35),Q(:,7),wf(:,156))
  call prop_A_Q(wf(:,155),Q(:,120),ZERO,0_intkind1,wf(:,157))
  call vert_VQ_A(wf(:,34),wf(:,-2),wf(:,158))
  call counter_A_Q(ctqq,wf(:,38),Q(:,11),wf(:,159))
  call prop_Q_A(wf(:,158),Q(:,116),ZERO,0_intkind1,wf(:,160))
  call counter_V_V(ctGG,wf(:,34),Q(:,112),wf(:,161))
  call vert_QA_V(wf(:,120),wf(:,-5),wf(:,162))
  call vert_AV_Q(wf(:,-3),wf(:,40),wf(:,163))
  call prop_A_Q(wf(:,163),Q(:,120),ZERO,0_intkind1,wf(:,164))
  call vert_VQ_A(wf(:,40),wf(:,-2),wf(:,165))
  call prop_Q_A(wf(:,165),Q(:,116),ZERO,0_intkind1,wf(:,166))
  call counter_V_V(ctGG,wf(:,40),Q(:,112),wf(:,167))
  call vert_QA_V(wf(:,-4),wf(:,130),wf(:,168))
  call vert_VQ_A(wf(:,2),wf(:,16),wf(:,169))
  call prop_Q_A(wf(:,169),Q(:,31),MB,1_intkind1,wf(:,170))
  call vert_AV_Q(wf(:,8),wf(:,2),wf(:,171))
  call prop_A_Q(wf(:,171),Q(:,47),MB,1_intkind1,wf(:,172))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,19),wf(:,173))
  call prop_Q_A(wf(:,173),Q(:,31),MB,1_intkind1,wf(:,174))
  call vert_AZ_Q(gZd,wf(:,11),wf(:,4),wf(:,175))
  call prop_A_Q(wf(:,175),Q(:,47),MB,1_intkind1,wf(:,176))
  call vert_QA_V(wf(:,5),wf(:,8),wf(:,177))
  call vert_QA_V(wf(:,53),wf(:,-5),wf(:,178))
  call vert_QA_V(wf(:,16),wf(:,13),wf(:,179))
  call vert_QA_V(wf(:,-4),wf(:,65),wf(:,180))
  call vert_VQ_A(wf(:,36),wf(:,-4),wf(:,181))
  call prop_Q_A(wf(:,181),Q(:,31),MB,1_intkind1,wf(:,182))
  call vert_AV_Q(wf(:,-5),wf(:,36),wf(:,183))
  call prop_A_Q(wf(:,183),Q(:,47),MB,1_intkind1,wf(:,184))
  call vert_VQ_A(wf(:,39),wf(:,-4),wf(:,185))
  call prop_Q_A(wf(:,185),Q(:,31),MB,1_intkind1,wf(:,186))
  call vert_AV_Q(wf(:,-5),wf(:,39),wf(:,187))
  call prop_A_Q(wf(:,187),Q(:,47),MB,1_intkind1,wf(:,188))
  call vert_QA_V(wf(:,143),wf(:,-5),wf(:,189))
  call vert_QA_V(wf(:,-4),wf(:,142),wf(:,190))
  call vert_QA_V(wf(:,150),wf(:,-3),wf(:,191))
  call vert_QA_V(wf(:,-2),wf(:,147),wf(:,192))
  call vert_QA_V(wf(:,138),wf(:,-5),wf(:,193))
  call vert_QA_V(wf(:,-4),wf(:,139),wf(:,194))

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
  den(2) = 1 / (Q(5,12))
  den(3) = 1 / (Q(5,80) - MB2)
  den(5) = 1 / (Q(5,35) - MB2)
  den(9) = 1 / (Q(5,44) - MB2)
  den(12) = 1 / (Q(5,96) - MB2)
  den(14) = 1 / (Q(5,19) - MB2)
  den(18) = 1 / (Q(5,28) - MB2)
  den(23) = 1 / (Q(5,67) - MZ2)
  den(27) = 1 / (Q(5,48))
  den(28) = 1 / (Q(5,52))
  den(31) = 1 / (Q(5,56))
  den(34) = 1 / (Q(5,7))
  den(36) = 1 / (Q(5,112))
  den(39) = 1 / (Q(5,11))
  den(50) = 1 / (Q(5,76))
  den(54) = 1 / (Q(5,83) - MB2)
  den(57) = 1 / (Q(5,92) - MB2)
  den(60) = 1 / (Q(5,99) - MB2)
  den(63) = 1 / (Q(5,108) - MB2)
  den(108) = 1 / (Q(5,75))
  den(111) = 1 / (Q(5,71))
  den(118) = 1 / (Q(5,120))
  den(121) = 1 / (Q(5,116))
  den(124) = 1 / (Q(5,15))
  den(139) = 1 / (Q(5,51))
  den(142) = 1 / (Q(5,31) - MB2)
  den(146) = 1 / (Q(5,47) - MB2)
  den(153) = 1 / (Q(5,115))
  den(165) = 1 / (Q(5,79))

  ! denominators
  den(4) = den(2)*den(3)
  den(6) = den(1)*den(5)
  den(7) = den(4)*den(6)
  den(8) = den(1)*den(3)
  den(10) = den(2)*den(9)
  den(11) = den(8)*den(10)
  den(13) = den(2)*den(12)
  den(15) = den(1)*den(14)
  den(16) = den(13)*den(15)
  den(17) = den(1)*den(12)
  den(19) = den(2)*den(18)
  den(20) = den(17)*den(19)
  den(21) = den(10)*den(15)
  den(22) = den(6)*den(19)
  den(24) = den(1)*den(23)
  den(25) = den(19)*den(24)
  den(26) = den(10)*den(24)
  den(29) = den(27)*den(28)
  den(30) = den(24)*den(29)
  den(32) = den(27)*den(31)
  den(33) = den(24)*den(32)
  den(35) = den(1)*den(34)
  den(37) = den(3)*den(36)
  den(38) = den(35)*den(37)
  den(40) = den(1)*den(39)
  den(41) = den(37)*den(40)
  den(42) = den(12)*den(36)
  den(43) = den(35)*den(42)
  den(44) = den(40)*den(42)
  den(45) = den(2)*den(27)
  den(46) = den(24)*den(45)
  den(47) = den(1)*den(2)
  den(48) = den(37)*den(47)
  den(49) = den(42)*den(47)
  den(51) = den(2)*den(50)
  den(52) = den(15)*den(51)
  den(53) = den(6)*den(51)
  den(55) = den(8)*den(54)
  den(56) = den(2)*den(55)
  den(58) = den(4)*den(57)
  den(59) = den(1)*den(58)
  den(61) = den(17)*den(60)
  den(62) = den(2)*den(61)
  den(64) = den(13)*den(63)
  den(65) = den(1)*den(64)
  den(66) = den(27)*den(36)
  den(67) = den(35)*den(66)
  den(68) = den(40)*den(66)
  den(69) = den(2)**2
  den(70) = den(55)*den(69)
  den(71) = den(3)*den(69)
  den(72) = den(6)*den(71)
  den(73) = den(3)**2
  den(74) = den(2)*den(73)
  den(75) = den(6)*den(74)
  den(76) = den(1)*den(73)
  den(77) = den(10)*den(76)
  den(78) = den(6)*den(58)
  den(79) = den(10)*den(55)
  den(80) = den(61)*den(69)
  den(81) = den(15)*den(69)
  den(82) = den(12)*den(81)
  den(83) = den(15)*den(64)
  den(84) = den(19)*den(61)
  den(85) = den(12)**2
  den(86) = den(2)*den(85)
  den(87) = den(15)*den(86)
  den(88) = den(1)*den(85)
  den(89) = den(19)*den(88)
  den(90) = den(18)*den(69)
  den(91) = den(6)*den(90)
  den(92) = den(24)*den(90)
  den(93) = den(9)*den(69)
  den(94) = den(15)*den(93)
  den(95) = den(24)*den(54)
  den(96) = den(69)*den(95)
  den(97) = den(24)*den(60)
  den(98) = den(19)*den(97)
  den(99) = den(10)*den(95)
  den(100) = den(10)*den(63)
  den(101) = den(15)*den(100)
  den(102) = den(6)*den(60)
  den(103) = den(19)*den(102)
  den(104) = den(15)*den(54)
  den(105) = den(10)*den(104)
  den(106) = den(19)*den(57)
  den(107) = den(6)*den(106)
  den(109) = den(24)*den(108)
  den(110) = den(29)*den(109)
  den(112) = den(24)*den(111)
  den(113) = den(32)*den(112)
  den(114) = den(27)**2
  den(115) = den(28)*den(114)
  den(116) = den(24)*den(115)
  den(117) = den(112)*den(114)
  den(119) = den(37)*den(118)
  den(120) = den(35)*den(119)
  den(122) = den(37)*den(121)
  den(123) = den(40)*den(122)
  den(125) = den(40)*den(124)
  den(126) = den(37)*den(125)
  den(127) = den(35)*den(124)
  den(128) = den(37)*den(127)
  den(129) = den(73)*den(127)
  den(130) = den(73)*den(125)
  den(131) = den(42)*den(118)
  den(132) = den(35)*den(131)
  den(133) = den(42)*den(121)
  den(134) = den(40)*den(133)
  den(135) = den(42)*den(125)
  den(136) = den(42)*den(127)
  den(137) = den(85)*den(127)
  den(138) = den(85)*den(125)
  den(140) = den(15)*den(139)
  den(141) = den(2)*den(15)
  den(143) = den(141)*den(142)
  den(144) = den(6)*den(139)
  den(145) = den(2)*den(6)
  den(147) = den(145)*den(146)
  den(148) = den(1)*den(19)
  den(149) = den(142)*den(148)
  den(150) = den(1)*den(10)
  den(151) = den(146)*den(150)
  den(152) = den(3)*den(6)
  den(154) = den(152)*den(153)
  den(155) = den(55)*den(153)
  den(156) = den(12)*den(15)
  den(157) = den(153)*den(156)
  den(158) = den(61)*den(153)
  den(159) = den(127)*den(142)
  den(160) = den(127)*den(146)
  den(161) = den(125)*den(142)
  den(162) = den(125)*den(146)
  den(163) = den(104)*den(153)
  den(164) = den(102)*den(153)
  den(166) = den(112)*den(165)
  den(167) = den(109)*den(165)
  den(168) = den(95)*den(153)
  den(169) = den(97)*den(153)
  den(170) = den(2)*den(24)*den(27)
  den(171) = den(1)*den(2)*den(27)
  den(172) = den(2)*den(3)*den(6)
  den(173) = den(1)*den(3)*den(10)
  den(174) = den(1)*den(2)*den(37)
  den(175) = den(1)*den(2)*den(3)
  den(176) = den(2)*den(12)*den(15)
  den(177) = den(1)*den(12)*den(19)
  den(178) = den(1)*den(2)*den(42)
  den(179) = den(1)*den(2)*den(12)
  den(180) = den(2)*den(140)
  den(181) = den(2)*den(104)
  den(182) = den(2)*den(144)
  den(183) = den(2)*den(102)
  den(184) = den(2)*den(95)
  den(185) = den(2)*den(97)
  den(186) = den(2)*den(24)
  den(187) = den(1)*den(106)
  den(188) = den(1)*den(100)
  den(189) = den(27)*den(127)
  den(190) = den(27)*den(125)
  den(191) = den(27)*den(112)
  den(192) = den(27)*den(109)
  den(193) = den(24)*den(27)
  den(194) = den(3)*den(127)
  den(195) = den(3)*den(35)
  den(196) = den(3)*den(125)
  den(197) = den(3)*den(40)
  den(198) = den(1)*den(122)
  den(199) = den(1)*den(119)
  den(200) = den(1)*den(37)
  den(201) = den(12)*den(127)
  den(202) = den(12)*den(35)
  den(203) = den(12)*den(125)
  den(204) = den(12)*den(40)
  den(205) = den(1)*den(133)
  den(206) = den(1)*den(131)
  den(207) = den(1)*den(42)
  den(208) = den(3)*den(147)
  den(209) = den(2)*den(154)
  den(210) = den(3)*den(151)
  den(211) = den(2)*den(155)
  den(212) = den(12)*den(143)
  den(213) = den(2)*den(157)
  den(214) = den(12)*den(149)
  den(215) = den(2)*den(158)
  den(216) = den(2)*den(163)
  den(217) = den(2)*den(164)
  den(218) = den(2)*den(168)
  den(219) = den(2)*den(169)
  den(220) = den(27)*den(166)
  den(221) = den(27)*den(167)
  den(222) = den(3)*den(160)
  den(223) = den(3)*den(162)
  den(224) = den(12)*den(159)
  den(225) = den(12)*den(161)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(115)

  A(1) = cont_QA(wf(:,7),wf(:,8)) * den(7)
  A(2) = cont_QA(wf(:,10),wf(:,11)) * den(11)
  A(3) = cont_QA(wf(:,15),wf(:,16)) * den(16)
  A(4) = cont_QA(wf(:,18),wf(:,19)) * den(20)
  A(5) = cont_QA(wf(:,11),wf(:,20)) * den(21)
  A(6) = cont_QA(wf(:,8),wf(:,21)) * den(22)
  A(7) = cont_VV(wf(:,23),wf(:,24)) * den(25)
  A(8) = cont_VV(wf(:,23),wf(:,25)) * den(26)
  A(9) = cont_VV(wf(:,23),wf(:,29)) * den(30)
  A(10) = cont_VV(wf(:,23),wf(:,32)) * den(33)
  A(11) = cont_VV(wf(:,34),wf(:,36)) * den(38)
  A(12) = cont_VV(wf(:,34),wf(:,39)) * den(41)
  A(13) = cont_VV(wf(:,36),wf(:,40)) * den(43)
  A(14) = cont_VV(wf(:,39),wf(:,40)) * den(44)

  A(15) = cont_VV(wf(:,23),wf(:,41)) * den(46)
  A(16) = cont_QA(wf(:,8),wf(:,42)) * den(7)
  A(17) = cont_QA(wf(:,11),wf(:,43)) * den(11)
  A(18) = cont_VV(wf(:,34),wf(:,44)) * den(48)
  A(19) = cont_QA(wf(:,16),wf(:,45)) * den(16)
  A(20) = cont_QA(wf(:,19),wf(:,46)) * den(20)
  A(21) = cont_VV(wf(:,40),wf(:,44)) * den(49)
  A(22) = cont_QA(wf(:,11),wf(:,47)) * den(21)
  A(23) = cont_VV(wf(:,48),wf(:,49)) * den(52)
  A(24) = cont_VV(wf(:,48),wf(:,49)) * den(52)
  A(25) = cont_QA(wf(:,8),wf(:,50)) * den(22)
  A(26) = cont_VV(wf(:,48),wf(:,51)) * den(53)
  A(27) = cont_VV(wf(:,48),wf(:,51)) * den(53)
  A(28) = cont_QA(wf(:,52),wf(:,53)) * den(56)
  A(29) = cont_QA(wf(:,54),wf(:,55)) * den(59)
  A(30) = cont_QA(wf(:,20),wf(:,56)) * den(21)
  A(31) = cont_VV(wf(:,23),wf(:,57)) * den(25)
  A(32) = cont_VV(wf(:,23),wf(:,58)) * den(26)
  A(33) = cont_QA(wf(:,21),wf(:,59)) * den(22)
  A(34) = cont_QA(wf(:,16),wf(:,62)) * den(16)
  A(35) = cont_QA(wf(:,19),wf(:,63)) * den(20)
  A(36) = cont_QA(wf(:,64),wf(:,65)) * den(62)
  A(37) = cont_QA(wf(:,66),wf(:,67)) * den(65)
  A(38) = cont_QA(wf(:,8),wf(:,69)) * den(22)
  A(39) = cont_VV(wf(:,23),wf(:,70)) * den(26)
  A(40) = cont_VV(wf(:,23),wf(:,71)) * den(25)
  A(41) = cont_QA(wf(:,11),wf(:,73)) * den(21)
  A(42) = cont_QA(wf(:,8),wf(:,76)) * den(7)
  A(43) = cont_QA(wf(:,11),wf(:,77)) * den(11)
  A(44) = cont_VV(wf(:,36),wf(:,78)) * den(67)
  A(45) = cont_VV(wf(:,36),wf(:,78)) * den(67)
  A(46) = cont_VV(wf(:,39),wf(:,78)) * den(68)
  A(47) = cont_VV(wf(:,39),wf(:,78)) * den(68)
  A(48) = cont_VV(wf(:,36),wf(:,79)) * den(38)
  A(49) = cont_VV(wf(:,39),wf(:,79)) * den(41)
  A(50) = cont_VV(wf(:,36),wf(:,80)) * den(43)
  A(51) = cont_VV(wf(:,39),wf(:,80)) * den(44)
  A(52) = cont_VV(wf(:,36),wf(:,81)) * den(43)
  A(53) = cont_VV(wf(:,39),wf(:,81)) * den(44)
  A(54) = cont_VV(wf(:,36),wf(:,82)) * den(38)
  A(55) = cont_VV(wf(:,39),wf(:,82)) * den(41)
  A(56) = cont_VV(wf(:,23),wf(:,86)) * den(33)
  A(57) = cont_VV(wf(:,23),wf(:,89)) * den(30)
  A(58) = cont_VV(wf(:,23),wf(:,90)) * den(30)
  A(59) = cont_VV(wf(:,23),wf(:,93)) * den(33)
  A(60) = cont_VV(wf(:,34),wf(:,94)) * den(38)
  A(61) = cont_VV(wf(:,34),wf(:,97)) * den(41)
  A(62) = cont_VV(wf(:,40),wf(:,94)) * den(43)
  A(63) = cont_VV(wf(:,40),wf(:,97)) * den(44)
  A(64) = cont_VV(wf(:,23),wf(:,100)) * den(30)
  A(65) = cont_VV(wf(:,23),wf(:,101)) * den(33)
  A(66) = cont_VV(wf(:,34),wf(:,102)) * den(41)
  A(67) = cont_VV(wf(:,34),wf(:,105)) * den(38)
  A(68) = cont_VV(wf(:,40),wf(:,102)) * den(44)
  A(69) = cont_VV(wf(:,40),wf(:,105)) * den(43)
  A(70) = cont_QA(wf(:,8),wf(:,107)) * den(7)
  A(71) = cont_QA(wf(:,10),wf(:,109)) * den(11)
  A(72) = cont_QA(wf(:,16),wf(:,110)) * den(16)
  A(73) = cont_QA(wf(:,18),wf(:,112)) * den(20)
  A(74) = cont_QA(wf(:,20),wf(:,109)) * den(21)
  A(75) = cont_QA(wf(:,8),wf(:,113)) * den(22)
  A(76) = cont_VV(wf(:,23),wf(:,114)) * den(25)
  A(77) = cont_VV(wf(:,23),wf(:,115)) * den(26)
  A(78) = cont_QA(wf(:,53),wf(:,117)) * den(70)
  A(79) = cont_QA(wf(:,8),wf(:,118)) * den(72)
  A(80) = cont_QA(wf(:,8),wf(:,121)) * den(75)
  A(81) = cont_QA(wf(:,11),wf(:,122)) * den(77)
  A(82) = cont_QA(wf(:,55),wf(:,123)) * den(78)
  A(83) = cont_QA(wf(:,53),wf(:,124)) * den(79)
  A(84) = cont_QA(wf(:,65),wf(:,125)) * den(80)
  A(85) = cont_QA(wf(:,13),wf(:,126)) * den(82)
  A(86) = cont_QA(wf(:,67),wf(:,127)) * den(83)
  A(87) = cont_QA(wf(:,65),wf(:,128)) * den(84)
  A(88) = cont_QA(wf(:,16),wf(:,131)) * den(87)
  A(89) = cont_QA(wf(:,19),wf(:,132)) * den(89)
  A(90) = cont_QA(wf(:,133),wf(:,134)) * den(91)
  A(91) = cont_QA(wf(:,134),wf(:,135)) * den(92)
  A(92) = cont_QA(wf(:,20),wf(:,136)) * den(94)
  A(93) = cont_QA(wf(:,117),wf(:,138)) * den(96)
  A(94) = cont_QA(wf(:,128),wf(:,139)) * den(98)
  A(95) = cont_QA(wf(:,124),wf(:,138)) * den(99)
  A(96) = cont_QA(wf(:,127),wf(:,141)) * den(101)
  A(97) = cont_QA(wf(:,128),wf(:,142)) * den(103)
  A(98) = cont_QA(wf(:,124),wf(:,143)) * den(105)
  A(99) = cont_QA(wf(:,123),wf(:,144)) * den(107)
  A(100) = cont_QA(wf(:,146),wf(:,147)) * den(110)
  A(101) = cont_QA(wf(:,149),wf(:,150)) * den(113)
  A(102) = cont_QA(wf(:,145),wf(:,153)) * den(116)
  A(103) = cont_QA(wf(:,150),wf(:,154)) * den(117)
  A(104) = cont_QA(wf(:,156),wf(:,157)) * den(120)
  A(105) = cont_QA(wf(:,159),wf(:,160)) * den(123)
  A(106) = cont_VV(wf(:,39),wf(:,161)) * den(126)
  A(107) = cont_VV(wf(:,36),wf(:,161)) * den(128)
  A(108) = cont_VV(wf(:,36),wf(:,162)) * den(129)
  A(109) = cont_VV(wf(:,39),wf(:,162)) * den(130)
  A(110) = cont_QA(wf(:,156),wf(:,164)) * den(132)
  A(111) = cont_QA(wf(:,159),wf(:,166)) * den(134)
  A(112) = cont_VV(wf(:,39),wf(:,167)) * den(135)
  A(113) = cont_VV(wf(:,36),wf(:,167)) * den(136)
  A(114) = cont_VV(wf(:,36),wf(:,168)) * den(137)
  A(115) = cont_VV(wf(:,39),wf(:,168)) * den(138)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(115)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(7)+A(8)+A(9)+A(10))*f(1))/2._/**/REALKIND+((A(1)+A(2)+A(3)+A(4)+A(5)+A(6)+A(11)+A(12)+A(13) &
       +A(14))*f(14))/2._/**/REALKIND
  M1(2) = ((-A(7)-A(8)-A(9)-A(10))*f(1))/6._/**/REALKIND+((-A(1)-A(2)-A(3)-A(4)-A(5)-A(6)-A(11)-A(12)-A(13) &
       -A(14))*f(14))/6._/**/REALKIND

  M2(1) = ((-A(91)-A(93)-A(94)-A(95)-A(100)-A(101)-A(102)-A(103))*f(2))/2._/**/REALKIND+((A(32)+A(40)+A(56) &
       +A(57))*f(3))/2._/**/REALKIND+((A(59)+A(64)+A(76)+A(77))*f(4))/2._/**/REALKIND+((A(31)+A(39))*f(5))/2._/**/REALKIND+((A(58) &
       +A(65))*f(6))/2._/**/REALKIND-(A(15)*f(7))/2._/**/REALKIND+((A(24)+A(27)+A(45)+A(47))*f(13))/2._/**/REALKIND+((-A(78)-A(79) &
       -A(80)-A(81)-A(82)-A(83)-A(84)-A(85)-A(86)-A(87)-A(88)-A(89)-A(90)-A(92)-A(96)-A(97)-A(98)-A(99)-A(104)-A(105)-A(106) &
       -A(107)-A(108)-A(109)-A(110)-A(111)-A(112)-A(113)-A(114)-A(115))*f(15))/2._/**/REALKIND+((A(16)+A(19)+A(28)+A(30)+A(36) &
       +A(38)+A(48)+A(49)+A(52)+A(53))*f(16))/2._/**/REALKIND+((A(60)+A(62)+A(66)+A(68)+A(70)+A(71)+A(72)+A(73)+A(74) &
       +A(75))*f(17))/2._/**/REALKIND+((A(22)+A(25)+A(34)+A(35)+A(42)+A(43)+A(50)+A(51)+A(54)+A(55))*f(18))/2._/**/REALKIND &
       +((A(17)+A(20)+A(29)+A(33)+A(37)+A(41))*f(19))/2._/**/REALKIND+((A(61)+A(63)+A(67)+A(69))*f(20))/2._/**/REALKIND+((-A(18) &
       -A(21))*f(21))/2._/**/REALKIND+((A(23)+A(26)+A(44)+A(46))*f(27))/2._/**/REALKIND
  M2(2) = ((A(91)+A(93)+A(94)+A(95)+A(100)+A(101)+A(102)+A(103))*f(2))/6._/**/REALKIND+((-A(32)-A(40)-A(56) &
       -A(57))*f(3))/6._/**/REALKIND+((-A(59)-A(64)-A(76)-A(77))*f(4))/6._/**/REALKIND+((-A(31)-A(39))*f(5))/6._/**/REALKIND+(( &
       -A(58)-A(65))*f(6))/6._/**/REALKIND+(A(15)*f(7))/6._/**/REALKIND+((-A(24)-A(27)-A(45)-A(47))*f(13))/6._/**/REALKIND+((A(78) &
       +A(79)+A(80)+A(81)+A(82)+A(83)+A(84)+A(85)+A(86)+A(87)+A(88)+A(89)+A(90)+A(92)+A(96)+A(97)+A(98)+A(99)+A(104)+A(105)+A(106) &
       +A(107)+A(108)+A(109)+A(110)+A(111)+A(112)+A(113)+A(114)+A(115))*f(15))/6._/**/REALKIND+((-A(16)-A(19)-A(28)-A(30)-A(36) &
       -A(38)-A(48)-A(49)-A(52)-A(53))*f(16))/6._/**/REALKIND+((-A(60)-A(62)-A(66)-A(68)-A(70)-A(71)-A(72)-A(73)-A(74) &
       -A(75))*f(17))/6._/**/REALKIND+((-A(22)-A(25)-A(34)-A(35)-A(42)-A(43)-A(50)-A(51)-A(54)-A(55))*f(18))/6._/**/REALKIND+(( &
       -A(17)-A(20)-A(29)-A(33)-A(37)-A(41))*f(19))/6._/**/REALKIND+((-A(61)-A(63)-A(67)-A(69))*f(20))/6._/**/REALKIND+((A(18) &
       +A(21))*f(21))/6._/**/REALKIND+((-A(23)-A(26)-A(44)-A(46))*f(27))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pphlljj_nenexddxbbxh_1_/**/REALKIND
