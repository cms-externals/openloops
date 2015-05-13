
module ol_colourmatrix_pphlljj_eexddxbbxh_1_/**/REALKIND
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
end module ol_colourmatrix_pphlljj_eexddxbbxh_1_/**/REALKIND



module ol_forced_parameters_pphlljj_eexddxbbxh_1_/**/REALKIND
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
end module ol_forced_parameters_pphlljj_eexddxbbxh_1_/**/REALKIND

module ol_loop_pphlljj_eexddxbbxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(45), c(59)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:262)
  ! denominators
  complex(REALKIND), save :: den(357)
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
    f(13) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MB*YB)/(3._/**/REALKIND*MQ2sum)
    f(14) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MB*YB)/MQ2sum
    f(15) = (CI*eQED**3*gQCD**2*YB)/(6._/**/REALKIND*MW*sw)
    f(16) = (CI*eQED**3*gQCD**2*YB)/(2._/**/REALKIND*MW*sw)
    f(17) = (CI*countertermnorm*eQED**3*gQCD**4*YB)/(6._/**/REALKIND*MW*sw)
    f(18) = (CI*countertermnorm*eQED**3*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(19) = (CI*countertermnorm*ctGbb*eQED**3*gQCD**4*YB)/(6._/**/REALKIND*MW*sw)
    f(20) = (CI*countertermnorm*ctGbb*eQED**3*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(21) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4*YB)/(6._/**/REALKIND*MW*sw)
    f(22) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(23) = (CI*countertermnorm*ctSbb*eQED**3*gQCD**4*YB)/(6._/**/REALKIND*MW*sw)
    f(24) = (CI*countertermnorm*ctSbb*eQED**3*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(25) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**4*YB)/(6._/**/REALKIND*MW*sw)
    f(26) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(27) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4*YB)/(6._/**/REALKIND*MW*sw)
    f(28) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(29) = (countertermnorm*ctZGG*eQED**3*gQCD**4*YB)/(MW*sw*2._/**/REALKIND)
    f(30) = (CI*eQED**3*gQCD**4*integralnorm*SwB*YB)/(6._/**/REALKIND*MW*sw)
    f(31) = (CI*eQED**3*gQCD**4*integralnorm*SwB*YB)/(2._/**/REALKIND*MW*sw)
    f(32) = (eQED**3*gQCD**4*integralnorm*SwB*YB)/(MW*sw*12._/**/REALKIND)
    f(33) = (eQED**3*gQCD**4*integralnorm*SwB*YB)/(MW*sw*6._/**/REALKIND)
    f(34) = (eQED**3*gQCD**4*integralnorm*SwB*YB)/(MW*sw*4._/**/REALKIND)
    f(35) = (eQED**3*gQCD**4*integralnorm*SwB*YB)/(MW*sw*2._/**/REALKIND)
    f(36) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw*6._/**/REALKIND)
    f(37) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw*3._/**/REALKIND)
    f(38) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(39) = (2*eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw*3._/**/REALKIND)
    f(40) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw)
    f(41) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MT*YT)/(3._/**/REALKIND*MQ2sum)
    f(42) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MT*YT)/MQ2sum
    f(43) = (eQED**3*gQCD**4*integralnorm*SwF*YT)/(MW*sw*6._/**/REALKIND)
    f(44) = (eQED**3*gQCD**4*integralnorm*SwF*YT)/(MW*sw*3._/**/REALKIND)
    f(45) = (eQED**3*gQCD**4*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

  c = [ 9*CI*f(8), 27*CI*f(8), 18*f(9), 54*f(9), f(10), 3*f(10), 6*f(10), 8*f(10), 10*f(10), 18*f(10), 21*f(10), 24*f(10) &
    , 54*f(10), 3*f(11), 9*f(11), 3*f(12), 9*f(12), 9*CI*f(30), 27*CI*f(30), 9*CI*f(31), 27*CI*f(31), 18*f(32), 54*f(32), f(33) &
    , 3*f(33), 6*f(33), 8*f(33), 10*f(33), 18*f(33), 21*f(33), 24*f(33), 54*f(33), 18*f(34), 54*f(34), f(35), 3*f(35), 6*f(35) &
    , 8*f(35), 10*f(35), 18*f(35), 21*f(35), 24*f(35), 54*f(35), 3*f(36), 9*f(36), 3*f(37), 9*f(37), 3*f(38), 9*f(38), 3*f(39) &
    , 9*f(39), 3*f(40), 9*f(40), 3*f(43), 9*f(43), 3*f(44), 9*f(44), 3*f(45), 9*f(45) ]
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
  complex(REALKIND) :: A(203)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rMB, H(5), wf(:,-4))
  call wf_A(P(:,6), rMB, H(6), wf(:,-5))
  call wf_S(P(:,7), rMH, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_QS_A(gH,wf(:,-4),wf(:,-6),wf(:,3))
  call prop_Q_A(wf(:,3),Q(:,80),MB,1_intkind1,wf(:,4))
  call vert_AV_Q(wf(:,-5),wf(:,1),wf(:,5))
  call vert_VQ_A(wf(:,2),wf(:,4),wf(:,6))
  call prop_A_Q(wf(:,5),Q(:,35),MB,1_intkind1,wf(:,7))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,8))
  call prop_W_W(wf(:,8),Q(:,3),MZ,1_intkind1,wf(:,9))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,9),wf(:,10))
  call prop_A_Q(wf(:,10),Q(:,35),MB,1_intkind1,wf(:,11))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,12))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,13))
  call prop_A_Q(wf(:,12),Q(:,44),MB,1_intkind1,wf(:,14))
  call vert_ZQ_A(gZd,wf(:,9),wf(:,4),wf(:,15))
  call vert_SA_Q(gH,wf(:,-6),wf(:,-5),wf(:,16))
  call prop_A_Q(wf(:,16),Q(:,96),MB,1_intkind1,wf(:,17))
  call vert_VQ_A(wf(:,1),wf(:,-4),wf(:,18))
  call vert_AV_Q(wf(:,17),wf(:,2),wf(:,19))
  call prop_Q_A(wf(:,18),Q(:,19),MB,1_intkind1,wf(:,20))
  call vert_ZQ_A(gZd,wf(:,9),wf(:,-4),wf(:,21))
  call prop_Q_A(wf(:,21),Q(:,19),MB,1_intkind1,wf(:,22))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,23))
  call vert_AV_Q(wf(:,17),wf(:,1),wf(:,24))
  call prop_Q_A(wf(:,23),Q(:,28),MB,1_intkind1,wf(:,25))
  call vert_AZ_Q(gZd,wf(:,17),wf(:,9),wf(:,26))
  call vert_QS_A(gH,wf(:,20),wf(:,-6),wf(:,27))
  call vert_QS_A(gH,wf(:,22),wf(:,-6),wf(:,28))
  call vert_QS_A(gH,wf(:,25),wf(:,-6),wf(:,29))
  call vert_SV_V(wf(:,-6),wf(:,9),wf(:,30))
  call prop_W_W(wf(:,30),Q(:,67),MZ,1_intkind1,wf(:,31))
  call vert_QA_Z(gZd,wf(:,25),wf(:,-5),wf(:,32))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,14),wf(:,33))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,34))
  call vert_VQ_A(wf(:,34),wf(:,-2),wf(:,35))
  call prop_Q_A(wf(:,35),Q(:,52),ZERO,0_intkind1,wf(:,36))
  call vert_QA_Z(gZd,wf(:,36),wf(:,-3),wf(:,37))
  call vert_AV_Q(wf(:,-3),wf(:,34),wf(:,38))
  call prop_A_Q(wf(:,38),Q(:,56),ZERO,0_intkind1,wf(:,39))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,39),wf(:,40))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,41))
  call vert_QA_V(wf(:,4),wf(:,-5),wf(:,42))
  call prop_Q_A(wf(:,41),Q(:,7),ZERO,0_intkind1,wf(:,43))
  call vert_QA_V(wf(:,43),wf(:,-3),wf(:,44))
  call vert_ZQ_A(gZd,wf(:,9),wf(:,-2),wf(:,45))
  call prop_Q_A(wf(:,45),Q(:,7),ZERO,0_intkind1,wf(:,46))
  call vert_QA_V(wf(:,46),wf(:,-3),wf(:,47))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,48))
  call prop_A_Q(wf(:,48),Q(:,11),ZERO,0_intkind1,wf(:,49))
  call vert_QA_V(wf(:,-2),wf(:,49),wf(:,50))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,9),wf(:,51))
  call prop_A_Q(wf(:,51),Q(:,11),ZERO,0_intkind1,wf(:,52))
  call vert_QA_V(wf(:,-2),wf(:,52),wf(:,53))
  call vert_QA_V(wf(:,-4),wf(:,17),wf(:,54))
  call counter_GG_V(wf(:,2),Q(:,12),wf(:,34),Q(:,48),wf(:,55))
  call counter_VQ_A(wf(:,2),wf(:,4),wf(:,56))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,57))
  call counter_ZQ_A(gZd,wf(:,9),wf(:,4),wf(:,58))
  call counter_VG_G(wf(:,9),wf(:,2),Q(:,12),wf(:,59),Q(:,15))
  call counter_AV_Q(wf(:,17),wf(:,2),wf(:,60))
  call counter_AV_Q(wf(:,17),wf(:,1),wf(:,61))
  call counter_AZ_Q(gZd,wf(:,17),wf(:,9),wf(:,62))
  call counter_QS_A(gH,wf(:,20),wf(:,-6),wf(:,63))
  call counter_QS_A(gH,wf(:,22),wf(:,-6),wf(:,64))
  call counter_SG_G(wf(:,-6),wf(:,2),wf(:,65))
  call vert_QA_V(wf(:,20),wf(:,-5),wf(:,66))
  call vert_QA_V(wf(:,22),wf(:,-5),wf(:,67))
  call counter_QS_A(gH,wf(:,25),wf(:,-6),wf(:,68))
  call vert_QA_V(wf(:,-4),wf(:,7),wf(:,69))
  call vert_QA_V(wf(:,-4),wf(:,11),wf(:,70))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,71))
  call prop_Q_A(wf(:,13),Q(:,83),MB,1_intkind1,wf(:,72))
  call prop_Q_A(wf(:,15),Q(:,83),MB,1_intkind1,wf(:,73))
  call counter_AV_Q(wf(:,-5),wf(:,1),wf(:,74))
  call prop_Q_A(wf(:,6),Q(:,92),MB,1_intkind1,wf(:,75))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,9),wf(:,76))
  call prop_A_Q(wf(:,71),Q(:,44),MB,1_intkind1,wf(:,77))
  call counter_QA_Z(gZd,wf(:,25),wf(:,-5),wf(:,78))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,77),wf(:,79))
  call prop_A_Q(wf(:,74),Q(:,35),MB,1_intkind1,wf(:,80))
  call prop_A_Q(wf(:,76),Q(:,35),MB,1_intkind1,wf(:,81))
  call counter_SA_Q(gH,wf(:,-6),wf(:,-5),wf(:,82))
  call prop_A_Q(wf(:,82),Q(:,96),MB,1_intkind1,wf(:,83))
  call vert_AV_Q(wf(:,83),wf(:,2),wf(:,84))
  call vert_AV_Q(wf(:,83),wf(:,1),wf(:,85))
  call vert_AZ_Q(gZd,wf(:,83),wf(:,9),wf(:,86))
  call counter_VQ_A(wf(:,2),wf(:,-4),wf(:,87))
  call prop_A_Q(wf(:,24),Q(:,99),MB,1_intkind1,wf(:,88))
  call prop_A_Q(wf(:,26),Q(:,99),MB,1_intkind1,wf(:,89))
  call counter_VQ_A(wf(:,1),wf(:,-4),wf(:,90))
  call prop_A_Q(wf(:,19),Q(:,108),MB,1_intkind1,wf(:,91))
  call counter_ZQ_A(gZd,wf(:,9),wf(:,-4),wf(:,92))
  call prop_Q_A(wf(:,87),Q(:,28),MB,1_intkind1,wf(:,93))
  call vert_QS_A(gH,wf(:,93),wf(:,-6),wf(:,94))
  call counter_QA_Z(gZd,wf(:,-4),wf(:,14),wf(:,95))
  call vert_QA_Z(gZd,wf(:,93),wf(:,-5),wf(:,96))
  call prop_Q_A(wf(:,90),Q(:,19),MB,1_intkind1,wf(:,97))
  call vert_QS_A(gH,wf(:,97),wf(:,-6),wf(:,98))
  call prop_Q_A(wf(:,92),Q(:,19),MB,1_intkind1,wf(:,99))
  call vert_QS_A(gH,wf(:,99),wf(:,-6),wf(:,100))
  call counter_QS_A(gH,wf(:,-4),wf(:,-6),wf(:,101))
  call prop_Q_A(wf(:,101),Q(:,80),MB,1_intkind1,wf(:,102))
  call vert_VQ_A(wf(:,2),wf(:,102),wf(:,103))
  call vert_VQ_A(wf(:,1),wf(:,102),wf(:,104))
  call vert_ZQ_A(gZd,wf(:,9),wf(:,102),wf(:,105))
  call counter_SG_G(wf(:,-6),wf(:,34),wf(:,106))
  call counter_QA_V(wf(:,4),wf(:,-5),wf(:,107))
  call vert_QA_V(wf(:,-4),wf(:,83),wf(:,108))
  call counter_QA_V(wf(:,-4),wf(:,17),wf(:,109))
  call vert_QA_V(wf(:,102),wf(:,-5),wf(:,110))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,111))
  call vert_AV_Q(wf(:,-3),wf(:,111),wf(:,112))
  call prop_A_Q(wf(:,112),Q(:,56),ZERO,0_intkind1,wf(:,113))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,113),wf(:,114))
  call vert_VQ_A(wf(:,111),wf(:,-2),wf(:,115))
  call prop_Q_A(wf(:,115),Q(:,52),ZERO,0_intkind1,wf(:,116))
  call vert_QA_Z(gZd,wf(:,116),wf(:,-3),wf(:,117))
  call counter_QA_Z(gZd,wf(:,36),wf(:,-3),wf(:,118))
  call counter_AV_Q(wf(:,-3),wf(:,34),wf(:,119))
  call prop_A_Q(wf(:,119),Q(:,56),ZERO,0_intkind1,wf(:,120))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,120),wf(:,121))
  call counter_QA_V(wf(:,43),wf(:,-3),wf(:,122))
  call counter_QA_V(wf(:,46),wf(:,-3),wf(:,123))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,124))
  call prop_A_Q(wf(:,124),Q(:,11),ZERO,0_intkind1,wf(:,125))
  call vert_QA_V(wf(:,-2),wf(:,125),wf(:,126))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,9),wf(:,127))
  call prop_A_Q(wf(:,127),Q(:,11),ZERO,0_intkind1,wf(:,128))
  call vert_QA_V(wf(:,-2),wf(:,128),wf(:,129))
  call counter_VQ_A(wf(:,34),wf(:,-2),wf(:,130))
  call prop_Q_A(wf(:,130),Q(:,52),ZERO,0_intkind1,wf(:,131))
  call vert_QA_Z(gZd,wf(:,131),wf(:,-3),wf(:,132))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,39),wf(:,133))
  call counter_QA_V(wf(:,-2),wf(:,49),wf(:,134))
  call counter_QA_V(wf(:,-2),wf(:,52),wf(:,135))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,136))
  call prop_Q_A(wf(:,136),Q(:,7),ZERO,0_intkind1,wf(:,137))
  call vert_QA_V(wf(:,137),wf(:,-3),wf(:,138))
  call counter_ZQ_A(gZd,wf(:,9),wf(:,-2),wf(:,139))
  call prop_Q_A(wf(:,139),Q(:,7),ZERO,0_intkind1,wf(:,140))
  call vert_QA_V(wf(:,140),wf(:,-3),wf(:,141))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,142))
  call vert_VQ_A(wf(:,142),wf(:,4),wf(:,143))
  call vert_AV_Q(wf(:,-5),wf(:,142),wf(:,144))
  call prop_A_Q(wf(:,144),Q(:,44),MB,1_intkind1,wf(:,145))
  call vert_AV_Q(wf(:,17),wf(:,142),wf(:,146))
  call vert_VQ_A(wf(:,142),wf(:,-4),wf(:,147))
  call prop_Q_A(wf(:,147),Q(:,28),MB,1_intkind1,wf(:,148))
  call vert_QS_A(gH,wf(:,148),wf(:,-6),wf(:,149))
  call vert_QA_Z(gZd,wf(:,148),wf(:,-5),wf(:,150))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,145),wf(:,151))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,152))
  call vert_AV_Q(wf(:,-5),wf(:,152),wf(:,153))
  call vert_VQ_A(wf(:,152),wf(:,4),wf(:,154))
  call counter_Q_A(ctbb,wf(:,4),Q(:,80),wf(:,155))
  call prop_Q_A(wf(:,155),Q(:,80),MB,1_intkind1,wf(:,156))
  call vert_VQ_A(wf(:,2),wf(:,156),wf(:,157))
  call vert_VQ_A(wf(:,1),wf(:,156),wf(:,158))
  call vert_ZQ_A(gZd,wf(:,9),wf(:,156),wf(:,159))
  call counter_A_Q(ctbb,wf(:,7),Q(:,35),wf(:,160))
  call counter_A_Q(ctbb,wf(:,11),Q(:,35),wf(:,161))
  call counter_A_Q(ctbb,wf(:,14),Q(:,44),wf(:,162))
  call vert_VQ_A(wf(:,152),wf(:,-4),wf(:,163))
  call vert_VQ_A(wf(:,152),wf(:,20),wf(:,164))
  call vert_VQ_A(wf(:,152),wf(:,22),wf(:,165))
  call counter_Q_A(ctbb,wf(:,20),Q(:,19),wf(:,166))
  call counter_Q_A(ctbb,wf(:,22),Q(:,19),wf(:,167))
  call counter_Q_A(ctbb,wf(:,25),Q(:,28),wf(:,168))
  call counter_A_Q(ctbb,wf(:,17),Q(:,96),wf(:,169))
  call prop_A_Q(wf(:,169),Q(:,96),MB,1_intkind1,wf(:,170))
  call vert_AV_Q(wf(:,170),wf(:,2),wf(:,171))
  call vert_AV_Q(wf(:,170),wf(:,1),wf(:,172))
  call vert_AZ_Q(gZd,wf(:,170),wf(:,9),wf(:,173))
  call vert_SA_Q(gH,wf(:,-6),wf(:,7),wf(:,174))
  call prop_Q_A(wf(:,163),Q(:,28),MB,1_intkind1,wf(:,175))
  call vert_SA_Q(gH,wf(:,-6),wf(:,11),wf(:,176))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,31),wf(:,177))
  call prop_A_Q(wf(:,153),Q(:,44),MB,1_intkind1,wf(:,178))
  call vert_ZQ_A(gZd,wf(:,31),wf(:,-4),wf(:,179))
  call prop_Q_A(wf(:,179),Q(:,83),MB,1_intkind1,wf(:,180))
  call prop_A_Q(wf(:,177),Q(:,99),MB,1_intkind1,wf(:,181))
  call vert_SA_Q(gH,wf(:,-6),wf(:,14),wf(:,182))
  call prop_A_Q(wf(:,182),Q(:,108),MB,1_intkind1,wf(:,183))
  call prop_A_Q(wf(:,174),Q(:,99),MB,1_intkind1,wf(:,184))
  call prop_A_Q(wf(:,176),Q(:,99),MB,1_intkind1,wf(:,185))
  call prop_Q_A(wf(:,27),Q(:,83),MB,1_intkind1,wf(:,186))
  call prop_Q_A(wf(:,28),Q(:,83),MB,1_intkind1,wf(:,187))
  call prop_Q_A(wf(:,29),Q(:,92),MB,1_intkind1,wf(:,188))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,31),wf(:,189))
  call counter_Q_A(ctqq,wf(:,36),Q(:,52),wf(:,190))
  call prop_A_Q(wf(:,189),Q(:,75),ZERO,0_intkind1,wf(:,191))
  call vert_ZQ_A(gZd,wf(:,31),wf(:,-2),wf(:,192))
  call counter_A_Q(ctqq,wf(:,39),Q(:,56),wf(:,193))
  call prop_Q_A(wf(:,192),Q(:,71),ZERO,0_intkind1,wf(:,194))
  call counter_V_V(ctGG,wf(:,34),Q(:,48),wf(:,195))
  call vert_VQ_A(wf(:,195),wf(:,-2),wf(:,196))
  call prop_Q_A(wf(:,196),Q(:,52),ZERO,0_intkind1,wf(:,197))
  call vert_AV_Q(wf(:,-3),wf(:,195),wf(:,198))
  call vert_AV_Q(wf(:,-3),wf(:,42),wf(:,199))
  call counter_Q_A(ctqq,wf(:,43),Q(:,7),wf(:,200))
  call prop_A_Q(wf(:,199),Q(:,120),ZERO,0_intkind1,wf(:,201))
  call counter_Q_A(ctqq,wf(:,46),Q(:,7),wf(:,202))
  call vert_VQ_A(wf(:,42),wf(:,-2),wf(:,203))
  call counter_A_Q(ctqq,wf(:,49),Q(:,11),wf(:,204))
  call prop_Q_A(wf(:,203),Q(:,116),ZERO,0_intkind1,wf(:,205))
  call counter_A_Q(ctqq,wf(:,52),Q(:,11),wf(:,206))
  call counter_V_V(ctGG,wf(:,42),Q(:,112),wf(:,207))
  call vert_QA_V(wf(:,156),wf(:,-5),wf(:,208))
  call vert_AV_Q(wf(:,-3),wf(:,54),wf(:,209))
  call prop_A_Q(wf(:,209),Q(:,120),ZERO,0_intkind1,wf(:,210))
  call vert_VQ_A(wf(:,54),wf(:,-2),wf(:,211))
  call prop_Q_A(wf(:,211),Q(:,116),ZERO,0_intkind1,wf(:,212))
  call counter_V_V(ctGG,wf(:,54),Q(:,112),wf(:,213))
  call vert_QA_V(wf(:,-4),wf(:,170),wf(:,214))
  call vert_VQ_A(wf(:,2),wf(:,20),wf(:,215))
  call prop_Q_A(wf(:,215),Q(:,31),MB,1_intkind1,wf(:,216))
  call vert_VQ_A(wf(:,2),wf(:,22),wf(:,217))
  call prop_Q_A(wf(:,217),Q(:,31),MB,1_intkind1,wf(:,218))
  call vert_AV_Q(wf(:,7),wf(:,2),wf(:,219))
  call prop_A_Q(wf(:,219),Q(:,47),MB,1_intkind1,wf(:,220))
  call vert_AV_Q(wf(:,11),wf(:,2),wf(:,221))
  call prop_A_Q(wf(:,221),Q(:,47),MB,1_intkind1,wf(:,222))
  call vert_VQ_A(wf(:,1),wf(:,25),wf(:,223))
  call prop_Q_A(wf(:,223),Q(:,31),MB,1_intkind1,wf(:,224))
  call vert_ZQ_A(gZd,wf(:,9),wf(:,25),wf(:,225))
  call prop_Q_A(wf(:,225),Q(:,31),MB,1_intkind1,wf(:,226))
  call vert_AV_Q(wf(:,14),wf(:,1),wf(:,227))
  call prop_A_Q(wf(:,227),Q(:,47),MB,1_intkind1,wf(:,228))
  call vert_AZ_Q(gZd,wf(:,14),wf(:,9),wf(:,229))
  call prop_A_Q(wf(:,229),Q(:,47),MB,1_intkind1,wf(:,230))
  call vert_QA_V(wf(:,4),wf(:,7),wf(:,231))
  call vert_QA_V(wf(:,4),wf(:,11),wf(:,232))
  call vert_QA_V(wf(:,72),wf(:,-5),wf(:,233))
  call vert_QA_V(wf(:,73),wf(:,-5),wf(:,234))
  call vert_QA_V(wf(:,20),wf(:,17),wf(:,235))
  call vert_QA_V(wf(:,22),wf(:,17),wf(:,236))
  call vert_QA_V(wf(:,-4),wf(:,88),wf(:,237))
  call vert_QA_V(wf(:,-4),wf(:,89),wf(:,238))
  call vert_VQ_A(wf(:,44),wf(:,-4),wf(:,239))
  call prop_Q_A(wf(:,239),Q(:,31),MB,1_intkind1,wf(:,240))
  call vert_VQ_A(wf(:,47),wf(:,-4),wf(:,241))
  call prop_Q_A(wf(:,241),Q(:,31),MB,1_intkind1,wf(:,242))
  call vert_AV_Q(wf(:,-5),wf(:,44),wf(:,243))
  call prop_A_Q(wf(:,243),Q(:,47),MB,1_intkind1,wf(:,244))
  call vert_AV_Q(wf(:,-5),wf(:,47),wf(:,245))
  call prop_A_Q(wf(:,245),Q(:,47),MB,1_intkind1,wf(:,246))
  call vert_VQ_A(wf(:,50),wf(:,-4),wf(:,247))
  call prop_Q_A(wf(:,247),Q(:,31),MB,1_intkind1,wf(:,248))
  call vert_VQ_A(wf(:,53),wf(:,-4),wf(:,249))
  call prop_Q_A(wf(:,249),Q(:,31),MB,1_intkind1,wf(:,250))
  call vert_AV_Q(wf(:,-5),wf(:,50),wf(:,251))
  call prop_A_Q(wf(:,251),Q(:,47),MB,1_intkind1,wf(:,252))
  call vert_AV_Q(wf(:,-5),wf(:,53),wf(:,253))
  call prop_A_Q(wf(:,253),Q(:,47),MB,1_intkind1,wf(:,254))
  call vert_QA_V(wf(:,186),wf(:,-5),wf(:,255))
  call vert_QA_V(wf(:,187),wf(:,-5),wf(:,256))
  call vert_QA_V(wf(:,-4),wf(:,184),wf(:,257))
  call vert_QA_V(wf(:,-4),wf(:,185),wf(:,258))
  call vert_QA_V(wf(:,194),wf(:,-3),wf(:,259))
  call vert_QA_V(wf(:,-2),wf(:,191),wf(:,260))
  call vert_QA_V(wf(:,180),wf(:,-5),wf(:,261))
  call vert_QA_V(wf(:,-4),wf(:,181),wf(:,262))

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
  den(2) = 1 / (Q(5,12))
  den(3) = 1 / (Q(5,80) - MB2)
  den(5) = 1 / (Q(5,35) - MB2)
  den(8) = 1 / (Q(5,3) - MZ2)
  den(12) = 1 / (Q(5,44) - MB2)
  den(17) = 1 / (Q(5,96) - MB2)
  den(19) = 1 / (Q(5,19) - MB2)
  den(25) = 1 / (Q(5,28) - MB2)
  den(34) = 1 / (Q(5,67) - MZ2)
  den(38) = 1 / (Q(5,48))
  den(39) = 1 / (Q(5,52))
  den(42) = 1 / (Q(5,56))
  den(45) = 1 / (Q(5,7))
  den(47) = 1 / (Q(5,112))
  den(52) = 1 / (Q(5,11))
  den(67) = 1 / (Q(5,76))
  den(73) = 1 / (Q(5,83) - MB2)
  den(78) = 1 / (Q(5,92) - MB2)
  den(82) = 1 / (Q(5,99) - MB2)
  den(87) = 1 / (Q(5,108) - MB2)
  den(158) = 1 / (Q(5,75))
  den(161) = 1 / (Q(5,71))
  den(168) = 1 / (Q(5,120))
  den(172) = 1 / (Q(5,116))
  den(176) = 1 / (Q(5,15))
  den(203) = 1 / (Q(5,51))
  den(207) = 1 / (Q(5,31) - MB2)
  den(214) = 1 / (Q(5,47) - MB2)
  den(227) = 1 / (Q(5,115))
  den(251) = 1 / (Q(5,79))

  ! denominators
  den(4) = den(2)*den(3)
  den(6) = den(1)*den(5)
  den(7) = den(4)*den(6)
  den(9) = den(5)*den(8)
  den(10) = den(4)*den(9)
  den(11) = den(1)*den(3)
  den(13) = den(2)*den(12)
  den(14) = den(11)*den(13)
  den(15) = den(3)*den(8)
  den(16) = den(13)*den(15)
  den(18) = den(2)*den(17)
  den(20) = den(1)*den(19)
  den(21) = den(18)*den(20)
  den(22) = den(8)*den(19)
  den(23) = den(18)*den(22)
  den(24) = den(1)*den(17)
  den(26) = den(2)*den(25)
  den(27) = den(24)*den(26)
  den(28) = den(8)*den(17)
  den(29) = den(26)*den(28)
  den(30) = den(13)*den(20)
  den(31) = den(13)*den(22)
  den(32) = den(6)*den(26)
  den(33) = den(9)*den(26)
  den(35) = den(8)*den(34)
  den(36) = den(26)*den(35)
  den(37) = den(13)*den(35)
  den(40) = den(38)*den(39)
  den(41) = den(35)*den(40)
  den(43) = den(38)*den(42)
  den(44) = den(35)*den(43)
  den(46) = den(1)*den(45)
  den(48) = den(3)*den(47)
  den(49) = den(46)*den(48)
  den(50) = den(8)*den(45)
  den(51) = den(48)*den(50)
  den(53) = den(1)*den(52)
  den(54) = den(48)*den(53)
  den(55) = den(8)*den(52)
  den(56) = den(48)*den(55)
  den(57) = den(17)*den(47)
  den(58) = den(46)*den(57)
  den(59) = den(50)*den(57)
  den(60) = den(53)*den(57)
  den(61) = den(55)*den(57)
  den(62) = den(2)*den(38)
  den(63) = den(35)*den(62)
  den(64) = den(2)*den(8)
  den(65) = den(48)*den(64)
  den(66) = den(57)*den(64)
  den(68) = den(2)*den(67)
  den(69) = den(20)*den(68)
  den(70) = den(22)*den(68)
  den(71) = den(6)*den(68)
  den(72) = den(9)*den(68)
  den(74) = den(11)*den(73)
  den(75) = den(2)*den(74)
  den(76) = den(15)*den(73)
  den(77) = den(2)*den(76)
  den(79) = den(4)*den(78)
  den(80) = den(1)*den(79)
  den(81) = den(8)*den(79)
  den(83) = den(24)*den(82)
  den(84) = den(2)*den(83)
  den(85) = den(28)*den(82)
  den(86) = den(2)*den(85)
  den(88) = den(18)*den(87)
  den(89) = den(1)*den(88)
  den(90) = den(8)*den(88)
  den(91) = den(38)*den(47)
  den(92) = den(46)*den(91)
  den(93) = den(50)*den(91)
  den(94) = den(53)*den(91)
  den(95) = den(55)*den(91)
  den(96) = den(2)**2
  den(97) = den(74)*den(96)
  den(98) = den(76)*den(96)
  den(99) = den(3)*den(96)
  den(100) = den(6)*den(99)
  den(101) = den(9)*den(99)
  den(102) = den(3)**2
  den(103) = den(2)*den(102)
  den(104) = den(6)*den(103)
  den(105) = den(9)*den(103)
  den(106) = den(1)*den(102)
  den(107) = den(13)*den(106)
  den(108) = den(8)*den(102)
  den(109) = den(13)*den(108)
  den(110) = den(6)*den(79)
  den(111) = den(9)*den(79)
  den(112) = den(13)*den(74)
  den(113) = den(13)*den(76)
  den(114) = den(83)*den(96)
  den(115) = den(85)*den(96)
  den(116) = den(20)*den(96)
  den(117) = den(17)*den(116)
  den(118) = den(22)*den(96)
  den(119) = den(17)*den(118)
  den(120) = den(20)*den(88)
  den(121) = den(22)*den(88)
  den(122) = den(26)*den(83)
  den(123) = den(26)*den(85)
  den(124) = den(17)**2
  den(125) = den(2)*den(124)
  den(126) = den(20)*den(125)
  den(127) = den(22)*den(125)
  den(128) = den(1)*den(124)
  den(129) = den(26)*den(128)
  den(130) = den(8)*den(124)
  den(131) = den(26)*den(130)
  den(132) = den(25)*den(96)
  den(133) = den(6)*den(132)
  den(134) = den(9)*den(132)
  den(135) = den(35)*den(132)
  den(136) = den(12)*den(96)
  den(137) = den(20)*den(136)
  den(138) = den(22)*den(136)
  den(139) = den(35)*den(73)
  den(140) = den(96)*den(139)
  den(141) = den(35)*den(82)
  den(142) = den(26)*den(141)
  den(143) = den(13)*den(139)
  den(144) = den(13)*den(87)
  den(145) = den(20)*den(144)
  den(146) = den(22)*den(144)
  den(147) = den(6)*den(82)
  den(148) = den(26)*den(147)
  den(149) = den(9)*den(82)
  den(150) = den(26)*den(149)
  den(151) = den(20)*den(73)
  den(152) = den(13)*den(151)
  den(153) = den(22)*den(73)
  den(154) = den(13)*den(153)
  den(155) = den(26)*den(78)
  den(156) = den(6)*den(155)
  den(157) = den(9)*den(155)
  den(159) = den(35)*den(158)
  den(160) = den(40)*den(159)
  den(162) = den(35)*den(161)
  den(163) = den(43)*den(162)
  den(164) = den(38)**2
  den(165) = den(39)*den(164)
  den(166) = den(35)*den(165)
  den(167) = den(162)*den(164)
  den(169) = den(48)*den(168)
  den(170) = den(46)*den(169)
  den(171) = den(50)*den(169)
  den(173) = den(48)*den(172)
  den(174) = den(53)*den(173)
  den(175) = den(55)*den(173)
  den(177) = den(53)*den(176)
  den(178) = den(48)*den(177)
  den(179) = den(55)*den(176)
  den(180) = den(48)*den(179)
  den(181) = den(46)*den(176)
  den(182) = den(48)*den(181)
  den(183) = den(50)*den(176)
  den(184) = den(48)*den(183)
  den(185) = den(102)*den(181)
  den(186) = den(102)*den(183)
  den(187) = den(102)*den(177)
  den(188) = den(102)*den(179)
  den(189) = den(57)*den(168)
  den(190) = den(46)*den(189)
  den(191) = den(50)*den(189)
  den(192) = den(57)*den(172)
  den(193) = den(53)*den(192)
  den(194) = den(55)*den(192)
  den(195) = den(57)*den(177)
  den(196) = den(57)*den(179)
  den(197) = den(57)*den(181)
  den(198) = den(57)*den(183)
  den(199) = den(124)*den(181)
  den(200) = den(124)*den(183)
  den(201) = den(124)*den(177)
  den(202) = den(124)*den(179)
  den(204) = den(20)*den(203)
  den(205) = den(22)*den(203)
  den(206) = den(2)*den(20)
  den(208) = den(206)*den(207)
  den(209) = den(2)*den(22)
  den(210) = den(207)*den(209)
  den(211) = den(6)*den(203)
  den(212) = den(9)*den(203)
  den(213) = den(2)*den(6)
  den(215) = den(213)*den(214)
  den(216) = den(2)*den(9)
  den(217) = den(214)*den(216)
  den(218) = den(1)*den(26)
  den(219) = den(207)*den(218)
  den(220) = den(8)*den(26)
  den(221) = den(207)*den(220)
  den(222) = den(1)*den(13)
  den(223) = den(214)*den(222)
  den(224) = den(8)*den(13)
  den(225) = den(214)*den(224)
  den(226) = den(3)*den(6)
  den(228) = den(226)*den(227)
  den(229) = den(3)*den(9)
  den(230) = den(227)*den(229)
  den(231) = den(74)*den(227)
  den(232) = den(76)*den(227)
  den(233) = den(17)*den(20)
  den(234) = den(227)*den(233)
  den(235) = den(17)*den(22)
  den(236) = den(227)*den(235)
  den(237) = den(83)*den(227)
  den(238) = den(85)*den(227)
  den(239) = den(181)*den(207)
  den(240) = den(183)*den(207)
  den(241) = den(181)*den(214)
  den(242) = den(183)*den(214)
  den(243) = den(177)*den(207)
  den(244) = den(179)*den(207)
  den(245) = den(177)*den(214)
  den(246) = den(179)*den(214)
  den(247) = den(151)*den(227)
  den(248) = den(153)*den(227)
  den(249) = den(147)*den(227)
  den(250) = den(149)*den(227)
  den(252) = den(162)*den(251)
  den(253) = den(159)*den(251)
  den(254) = den(139)*den(227)
  den(255) = den(141)*den(227)
  den(256) = den(2)*den(35)*den(38)
  den(257) = den(1)*den(2)*den(38)
  den(258) = den(2)*den(8)*den(38)
  den(259) = den(2)*den(3)*den(6)
  den(260) = den(2)*den(3)*den(9)
  den(261) = den(1)*den(3)*den(13)
  den(262) = den(3)*den(8)*den(13)
  den(263) = den(1)*den(2)*den(48)
  den(264) = den(2)*den(8)*den(48)
  den(265) = den(1)*den(2)*den(3)
  den(266) = den(2)*den(3)*den(8)
  den(267) = den(2)*den(17)*den(20)
  den(268) = den(2)*den(17)*den(22)
  den(269) = den(1)*den(17)*den(26)
  den(270) = den(8)*den(17)*den(26)
  den(271) = den(1)*den(2)*den(57)
  den(272) = den(2)*den(8)*den(57)
  den(273) = den(1)*den(2)*den(17)
  den(274) = den(2)*den(8)*den(17)
  den(275) = den(2)*den(204)
  den(276) = den(2)*den(205)
  den(277) = den(2)*den(151)
  den(278) = den(2)*den(153)
  den(279) = den(2)*den(211)
  den(280) = den(2)*den(212)
  den(281) = den(2)*den(147)
  den(282) = den(2)*den(149)
  den(283) = den(2)*den(139)
  den(284) = den(2)*den(141)
  den(285) = den(2)*den(35)
  den(286) = den(1)*den(155)
  den(287) = den(8)*den(155)
  den(288) = den(1)*den(144)
  den(289) = den(8)*den(144)
  den(290) = den(1)*den(2)
  den(291) = den(38)*den(181)
  den(292) = den(38)*den(183)
  den(293) = den(38)*den(177)
  den(294) = den(38)*den(179)
  den(295) = den(38)*den(162)
  den(296) = den(38)*den(159)
  den(297) = den(35)*den(38)
  den(298) = den(3)*den(181)
  den(299) = den(3)*den(183)
  den(300) = den(3)*den(46)
  den(301) = den(3)*den(50)
  den(302) = den(3)*den(177)
  den(303) = den(3)*den(179)
  den(304) = den(3)*den(53)
  den(305) = den(3)*den(55)
  den(306) = den(1)*den(173)
  den(307) = den(8)*den(173)
  den(308) = den(1)*den(169)
  den(309) = den(8)*den(169)
  den(310) = den(1)*den(48)
  den(311) = den(8)*den(48)
  den(312) = den(17)*den(181)
  den(313) = den(17)*den(183)
  den(314) = den(17)*den(46)
  den(315) = den(17)*den(50)
  den(316) = den(17)*den(177)
  den(317) = den(17)*den(179)
  den(318) = den(17)*den(53)
  den(319) = den(17)*den(55)
  den(320) = den(1)*den(192)
  den(321) = den(8)*den(192)
  den(322) = den(1)*den(189)
  den(323) = den(8)*den(189)
  den(324) = den(1)*den(57)
  den(325) = den(8)*den(57)
  den(326) = den(3)*den(215)
  den(327) = den(3)*den(217)
  den(328) = den(2)*den(228)
  den(329) = den(2)*den(230)
  den(330) = den(3)*den(223)
  den(331) = den(3)*den(225)
  den(332) = den(2)*den(231)
  den(333) = den(2)*den(232)
  den(334) = den(17)*den(208)
  den(335) = den(17)*den(210)
  den(336) = den(2)*den(234)
  den(337) = den(2)*den(236)
  den(338) = den(17)*den(219)
  den(339) = den(17)*den(221)
  den(340) = den(2)*den(237)
  den(341) = den(2)*den(238)
  den(342) = den(2)*den(247)
  den(343) = den(2)*den(248)
  den(344) = den(2)*den(249)
  den(345) = den(2)*den(250)
  den(346) = den(2)*den(254)
  den(347) = den(2)*den(255)
  den(348) = den(38)*den(252)
  den(349) = den(38)*den(253)
  den(350) = den(3)*den(241)
  den(351) = den(3)*den(242)
  den(352) = den(3)*den(245)
  den(353) = den(3)*den(246)
  den(354) = den(17)*den(239)
  den(355) = den(17)*den(240)
  den(356) = den(17)*den(243)
  den(357) = den(17)*den(244)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(203)

  A(1) = cont_QA(wf(:,6),wf(:,7)) * den(7)
  A(2) = cont_QA(wf(:,6),wf(:,11)) * den(10)
  A(3) = cont_QA(wf(:,13),wf(:,14)) * den(14)
  A(4) = cont_QA(wf(:,14),wf(:,15)) * den(16)
  A(5) = cont_QA(wf(:,19),wf(:,20)) * den(21)
  A(6) = cont_QA(wf(:,19),wf(:,22)) * den(23)
  A(7) = cont_QA(wf(:,24),wf(:,25)) * den(27)
  A(8) = cont_QA(wf(:,25),wf(:,26)) * den(29)
  A(9) = cont_QA(wf(:,14),wf(:,27)) * den(30)
  A(10) = cont_QA(wf(:,14),wf(:,28)) * den(31)
  A(11) = cont_QA(wf(:,7),wf(:,29)) * den(32)
  A(12) = cont_QA(wf(:,11),wf(:,29)) * den(33)
  A(13) = cont_VV(wf(:,31),wf(:,32)) * den(36)
  A(14) = cont_VV(wf(:,31),wf(:,33)) * den(37)
  A(15) = cont_VV(wf(:,31),wf(:,37)) * den(41)
  A(16) = cont_VV(wf(:,31),wf(:,40)) * den(44)
  A(17) = cont_VV(wf(:,42),wf(:,44)) * den(49)
  A(18) = cont_VV(wf(:,42),wf(:,47)) * den(51)
  A(19) = cont_VV(wf(:,42),wf(:,50)) * den(54)
  A(20) = cont_VV(wf(:,42),wf(:,53)) * den(56)
  A(21) = cont_VV(wf(:,44),wf(:,54)) * den(58)
  A(22) = cont_VV(wf(:,47),wf(:,54)) * den(59)
  A(23) = cont_VV(wf(:,50),wf(:,54)) * den(60)
  A(24) = cont_VV(wf(:,53),wf(:,54)) * den(61)

  A(25) = cont_VV(wf(:,31),wf(:,55)) * den(63)
  A(26) = cont_QA(wf(:,7),wf(:,56)) * den(7)
  A(27) = cont_QA(wf(:,11),wf(:,56)) * den(10)
  A(28) = cont_QA(wf(:,14),wf(:,57)) * den(14)
  A(29) = cont_QA(wf(:,14),wf(:,58)) * den(16)
  A(30) = cont_VV(wf(:,42),wf(:,59)) * den(65)
  A(31) = cont_QA(wf(:,20),wf(:,60)) * den(21)
  A(32) = cont_QA(wf(:,22),wf(:,60)) * den(23)
  A(33) = cont_QA(wf(:,25),wf(:,61)) * den(27)
  A(34) = cont_QA(wf(:,25),wf(:,62)) * den(29)
  A(35) = cont_VV(wf(:,54),wf(:,59)) * den(66)
  A(36) = cont_QA(wf(:,14),wf(:,63)) * den(30)
  A(37) = cont_QA(wf(:,14),wf(:,64)) * den(31)
  A(38) = cont_VV(wf(:,65),wf(:,66)) * den(69)
  A(39) = cont_VV(wf(:,65),wf(:,66)) * den(69)
  A(40) = cont_VV(wf(:,65),wf(:,67)) * den(70)
  A(41) = cont_VV(wf(:,65),wf(:,67)) * den(70)
  A(42) = cont_QA(wf(:,7),wf(:,68)) * den(32)
  A(43) = cont_QA(wf(:,11),wf(:,68)) * den(33)
  A(44) = cont_VV(wf(:,65),wf(:,69)) * den(71)
  A(45) = cont_VV(wf(:,65),wf(:,69)) * den(71)
  A(46) = cont_VV(wf(:,65),wf(:,70)) * den(72)
  A(47) = cont_VV(wf(:,65),wf(:,70)) * den(72)
  A(48) = cont_QA(wf(:,71),wf(:,72)) * den(75)
  A(49) = cont_QA(wf(:,71),wf(:,73)) * den(77)
  A(50) = cont_QA(wf(:,74),wf(:,75)) * den(80)
  A(51) = cont_QA(wf(:,75),wf(:,76)) * den(81)
  A(52) = cont_QA(wf(:,27),wf(:,77)) * den(30)
  A(53) = cont_QA(wf(:,28),wf(:,77)) * den(31)
  A(54) = cont_VV(wf(:,31),wf(:,78)) * den(36)
  A(55) = cont_VV(wf(:,31),wf(:,79)) * den(37)
  A(56) = cont_QA(wf(:,29),wf(:,80)) * den(32)
  A(57) = cont_QA(wf(:,29),wf(:,81)) * den(33)
  A(58) = cont_QA(wf(:,20),wf(:,84)) * den(21)
  A(59) = cont_QA(wf(:,22),wf(:,84)) * den(23)
  A(60) = cont_QA(wf(:,25),wf(:,85)) * den(27)
  A(61) = cont_QA(wf(:,25),wf(:,86)) * den(29)
  A(62) = cont_QA(wf(:,87),wf(:,88)) * den(84)
  A(63) = cont_QA(wf(:,87),wf(:,89)) * den(86)
  A(64) = cont_QA(wf(:,90),wf(:,91)) * den(89)
  A(65) = cont_QA(wf(:,91),wf(:,92)) * den(90)
  A(66) = cont_QA(wf(:,7),wf(:,94)) * den(32)
  A(67) = cont_QA(wf(:,11),wf(:,94)) * den(33)
  A(68) = cont_VV(wf(:,31),wf(:,95)) * den(37)
  A(69) = cont_VV(wf(:,31),wf(:,96)) * den(36)
  A(70) = cont_QA(wf(:,14),wf(:,98)) * den(30)
  A(71) = cont_QA(wf(:,14),wf(:,100)) * den(31)
  A(72) = cont_QA(wf(:,7),wf(:,103)) * den(7)
  A(73) = cont_QA(wf(:,11),wf(:,103)) * den(10)
  A(74) = cont_QA(wf(:,14),wf(:,104)) * den(14)
  A(75) = cont_QA(wf(:,14),wf(:,105)) * den(16)
  A(76) = cont_VV(wf(:,44),wf(:,106)) * den(92)
  A(77) = cont_VV(wf(:,44),wf(:,106)) * den(92)
  A(78) = cont_VV(wf(:,47),wf(:,106)) * den(93)
  A(79) = cont_VV(wf(:,47),wf(:,106)) * den(93)
  A(80) = cont_VV(wf(:,50),wf(:,106)) * den(94)
  A(81) = cont_VV(wf(:,50),wf(:,106)) * den(94)
  A(82) = cont_VV(wf(:,53),wf(:,106)) * den(95)
  A(83) = cont_VV(wf(:,53),wf(:,106)) * den(95)
  A(84) = cont_VV(wf(:,44),wf(:,107)) * den(49)
  A(85) = cont_VV(wf(:,47),wf(:,107)) * den(51)
  A(86) = cont_VV(wf(:,50),wf(:,107)) * den(54)
  A(87) = cont_VV(wf(:,53),wf(:,107)) * den(56)
  A(88) = cont_VV(wf(:,44),wf(:,108)) * den(58)
  A(89) = cont_VV(wf(:,47),wf(:,108)) * den(59)
  A(90) = cont_VV(wf(:,50),wf(:,108)) * den(60)
  A(91) = cont_VV(wf(:,53),wf(:,108)) * den(61)
  A(92) = cont_VV(wf(:,44),wf(:,109)) * den(58)
  A(93) = cont_VV(wf(:,47),wf(:,109)) * den(59)
  A(94) = cont_VV(wf(:,50),wf(:,109)) * den(60)
  A(95) = cont_VV(wf(:,53),wf(:,109)) * den(61)
  A(96) = cont_VV(wf(:,44),wf(:,110)) * den(49)
  A(97) = cont_VV(wf(:,47),wf(:,110)) * den(51)
  A(98) = cont_VV(wf(:,50),wf(:,110)) * den(54)
  A(99) = cont_VV(wf(:,53),wf(:,110)) * den(56)
  A(100) = cont_VV(wf(:,31),wf(:,114)) * den(44)
  A(101) = cont_VV(wf(:,31),wf(:,117)) * den(41)
  A(102) = cont_VV(wf(:,31),wf(:,118)) * den(41)
  A(103) = cont_VV(wf(:,31),wf(:,121)) * den(44)
  A(104) = cont_VV(wf(:,42),wf(:,122)) * den(49)
  A(105) = cont_VV(wf(:,42),wf(:,123)) * den(51)
  A(106) = cont_VV(wf(:,42),wf(:,126)) * den(54)
  A(107) = cont_VV(wf(:,42),wf(:,129)) * den(56)
  A(108) = cont_VV(wf(:,54),wf(:,122)) * den(58)
  A(109) = cont_VV(wf(:,54),wf(:,123)) * den(59)
  A(110) = cont_VV(wf(:,54),wf(:,126)) * den(60)
  A(111) = cont_VV(wf(:,54),wf(:,129)) * den(61)
  A(112) = cont_VV(wf(:,31),wf(:,132)) * den(41)
  A(113) = cont_VV(wf(:,31),wf(:,133)) * den(44)
  A(114) = cont_VV(wf(:,42),wf(:,134)) * den(54)
  A(115) = cont_VV(wf(:,42),wf(:,135)) * den(56)
  A(116) = cont_VV(wf(:,42),wf(:,138)) * den(49)
  A(117) = cont_VV(wf(:,42),wf(:,141)) * den(51)
  A(118) = cont_VV(wf(:,54),wf(:,134)) * den(60)
  A(119) = cont_VV(wf(:,54),wf(:,135)) * den(61)
  A(120) = cont_VV(wf(:,54),wf(:,138)) * den(58)
  A(121) = cont_VV(wf(:,54),wf(:,141)) * den(59)
  A(122) = cont_QA(wf(:,7),wf(:,143)) * den(7)
  A(123) = cont_QA(wf(:,11),wf(:,143)) * den(10)
  A(124) = cont_QA(wf(:,13),wf(:,145)) * den(14)
  A(125) = cont_QA(wf(:,15),wf(:,145)) * den(16)
  A(126) = cont_QA(wf(:,20),wf(:,146)) * den(21)
  A(127) = cont_QA(wf(:,22),wf(:,146)) * den(23)
  A(128) = cont_QA(wf(:,24),wf(:,148)) * den(27)
  A(129) = cont_QA(wf(:,26),wf(:,148)) * den(29)
  A(130) = cont_QA(wf(:,27),wf(:,145)) * den(30)
  A(131) = cont_QA(wf(:,28),wf(:,145)) * den(31)
  A(132) = cont_QA(wf(:,7),wf(:,149)) * den(32)
  A(133) = cont_QA(wf(:,11),wf(:,149)) * den(33)
  A(134) = cont_VV(wf(:,31),wf(:,150)) * den(36)
  A(135) = cont_VV(wf(:,31),wf(:,151)) * den(37)
  A(136) = cont_QA(wf(:,72),wf(:,153)) * den(97)
  A(137) = cont_QA(wf(:,73),wf(:,153)) * den(98)
  A(138) = cont_QA(wf(:,7),wf(:,154)) * den(100)
  A(139) = cont_QA(wf(:,11),wf(:,154)) * den(101)
  A(140) = cont_QA(wf(:,7),wf(:,157)) * den(104)
  A(141) = cont_QA(wf(:,11),wf(:,157)) * den(105)
  A(142) = cont_QA(wf(:,14),wf(:,158)) * den(107)
  A(143) = cont_QA(wf(:,14),wf(:,159)) * den(109)
  A(144) = cont_QA(wf(:,75),wf(:,160)) * den(110)
  A(145) = cont_QA(wf(:,75),wf(:,161)) * den(111)
  A(146) = cont_QA(wf(:,72),wf(:,162)) * den(112)
  A(147) = cont_QA(wf(:,73),wf(:,162)) * den(113)
  A(148) = cont_QA(wf(:,88),wf(:,163)) * den(114)
  A(149) = cont_QA(wf(:,89),wf(:,163)) * den(115)
  A(150) = cont_QA(wf(:,17),wf(:,164)) * den(117)
  A(151) = cont_QA(wf(:,17),wf(:,165)) * den(119)
  A(152) = cont_QA(wf(:,91),wf(:,166)) * den(120)
  A(153) = cont_QA(wf(:,91),wf(:,167)) * den(121)
  A(154) = cont_QA(wf(:,88),wf(:,168)) * den(122)
  A(155) = cont_QA(wf(:,89),wf(:,168)) * den(123)
  A(156) = cont_QA(wf(:,20),wf(:,171)) * den(126)
  A(157) = cont_QA(wf(:,22),wf(:,171)) * den(127)
  A(158) = cont_QA(wf(:,25),wf(:,172)) * den(129)
  A(159) = cont_QA(wf(:,25),wf(:,173)) * den(131)
  A(160) = cont_QA(wf(:,174),wf(:,175)) * den(133)
  A(161) = cont_QA(wf(:,175),wf(:,176)) * den(134)
  A(162) = cont_QA(wf(:,175),wf(:,177)) * den(135)
  A(163) = cont_QA(wf(:,27),wf(:,178)) * den(137)
  A(164) = cont_QA(wf(:,28),wf(:,178)) * den(138)
  A(165) = cont_QA(wf(:,153),wf(:,180)) * den(140)
  A(166) = cont_QA(wf(:,168),wf(:,181)) * den(142)
  A(167) = cont_QA(wf(:,162),wf(:,180)) * den(143)
  A(168) = cont_QA(wf(:,166),wf(:,183)) * den(145)
  A(169) = cont_QA(wf(:,167),wf(:,183)) * den(146)
  A(170) = cont_QA(wf(:,168),wf(:,184)) * den(148)
  A(171) = cont_QA(wf(:,168),wf(:,185)) * den(150)
  A(172) = cont_QA(wf(:,162),wf(:,186)) * den(152)
  A(173) = cont_QA(wf(:,162),wf(:,187)) * den(154)
  A(174) = cont_QA(wf(:,160),wf(:,188)) * den(156)
  A(175) = cont_QA(wf(:,161),wf(:,188)) * den(157)
  A(176) = cont_QA(wf(:,190),wf(:,191)) * den(160)
  A(177) = cont_QA(wf(:,193),wf(:,194)) * den(163)
  A(178) = cont_QA(wf(:,189),wf(:,197)) * den(166)
  A(179) = cont_QA(wf(:,194),wf(:,198)) * den(167)
  A(180) = cont_QA(wf(:,200),wf(:,201)) * den(170)
  A(181) = cont_QA(wf(:,201),wf(:,202)) * den(171)
  A(182) = cont_QA(wf(:,204),wf(:,205)) * den(174)
  A(183) = cont_QA(wf(:,205),wf(:,206)) * den(175)
  A(184) = cont_VV(wf(:,50),wf(:,207)) * den(178)
  A(185) = cont_VV(wf(:,53),wf(:,207)) * den(180)
  A(186) = cont_VV(wf(:,44),wf(:,207)) * den(182)
  A(187) = cont_VV(wf(:,47),wf(:,207)) * den(184)
  A(188) = cont_VV(wf(:,44),wf(:,208)) * den(185)
  A(189) = cont_VV(wf(:,47),wf(:,208)) * den(186)
  A(190) = cont_VV(wf(:,50),wf(:,208)) * den(187)
  A(191) = cont_VV(wf(:,53),wf(:,208)) * den(188)
  A(192) = cont_QA(wf(:,200),wf(:,210)) * den(190)
  A(193) = cont_QA(wf(:,202),wf(:,210)) * den(191)
  A(194) = cont_QA(wf(:,204),wf(:,212)) * den(193)
  A(195) = cont_QA(wf(:,206),wf(:,212)) * den(194)
  A(196) = cont_VV(wf(:,50),wf(:,213)) * den(195)
  A(197) = cont_VV(wf(:,53),wf(:,213)) * den(196)
  A(198) = cont_VV(wf(:,44),wf(:,213)) * den(197)
  A(199) = cont_VV(wf(:,47),wf(:,213)) * den(198)
  A(200) = cont_VV(wf(:,44),wf(:,214)) * den(199)
  A(201) = cont_VV(wf(:,47),wf(:,214)) * den(200)
  A(202) = cont_VV(wf(:,50),wf(:,214)) * den(201)
  A(203) = cont_VV(wf(:,53),wf(:,214)) * den(202)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(203)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(13)+A(14)+A(15)+A(16))*f(1))/2._/**/REALKIND+((A(1)+A(3)+A(5)+A(7)+A(9)+A(11)+A(17)+A(19)+A(21) &
       +A(23))*f(15))/2._/**/REALKIND+((A(2)+A(4)+A(6)+A(8)+A(10)+A(12)+A(18)+A(20)+A(22)+A(24))*f(16))/2._/**/REALKIND
  M1(2) = ((-A(13)-A(14)-A(15)-A(16))*f(1))/6._/**/REALKIND+((-A(1)-A(3)-A(5)-A(7)-A(9)-A(11)-A(17)-A(19)-A(21) &
       -A(23))*f(15))/6._/**/REALKIND+((-A(2)-A(4)-A(6)-A(8)-A(10)-A(12)-A(18)-A(20)-A(22)-A(24))*f(16))/6._/**/REALKIND

  M2(1) = ((-A(162)-A(165)-A(166)-A(167)-A(176)-A(177)-A(178)-A(179))*f(2))/2._/**/REALKIND+((A(55)+A(69)+A(100) &
       +A(101))*f(3))/2._/**/REALKIND+((A(103)+A(112)+A(134)+A(135))*f(4))/2._/**/REALKIND+((A(54)+A(68))*f(5))/2._/**/REALKIND &
       +((A(102)+A(113))*f(6))/2._/**/REALKIND-(A(25)*f(7))/2._/**/REALKIND+((A(39)+A(45)+A(77)+A(81))*f(13))/2._/**/REALKIND &
       +((A(41)+A(47)+A(79)+A(83))*f(14))/2._/**/REALKIND+((-A(136)-A(138)-A(140)-A(142)-A(144)-A(146)-A(148)-A(150)-A(152)-A(154) &
       -A(156)-A(158)-A(160)-A(163)-A(168)-A(170)-A(172)-A(174)-A(180)-A(182)-A(184)-A(186)-A(188)-A(190)-A(192)-A(194)-A(196) &
       -A(198)-A(200)-A(202))*f(17))/2._/**/REALKIND+((-A(137)-A(139)-A(141)-A(143)-A(145)-A(147)-A(149)-A(151)-A(153)-A(155) &
       -A(157)-A(159)-A(161)-A(164)-A(169)-A(171)-A(173)-A(175)-A(181)-A(183)-A(185)-A(187)-A(189)-A(191)-A(193)-A(195)-A(197) &
       -A(199)-A(201)-A(203))*f(18))/2._/**/REALKIND+((A(26)+A(31)+A(48)+A(52)+A(62)+A(66)+A(84)+A(86)+A(92) &
       +A(94))*f(19))/2._/**/REALKIND+((A(27)+A(32)+A(49)+A(53)+A(63)+A(67)+A(85)+A(87)+A(93)+A(95))*f(20))/2._/**/REALKIND &
       +((A(104)+A(108)+A(114)+A(118)+A(122)+A(124)+A(126)+A(128)+A(130)+A(132))*f(21))/2._/**/REALKIND+((A(105)+A(109)+A(115) &
       +A(119)+A(123)+A(125)+A(127)+A(129)+A(131)+A(133))*f(22))/2._/**/REALKIND+((A(36)+A(42)+A(58)+A(60)+A(72)+A(74)+A(88)+A(90) &
       +A(96)+A(98))*f(23))/2._/**/REALKIND+((A(37)+A(43)+A(59)+A(61)+A(73)+A(75)+A(89)+A(91)+A(97)+A(99))*f(24))/2._/**/REALKIND &
       +((A(28)+A(33)+A(50)+A(56)+A(64)+A(70))*f(25))/2._/**/REALKIND+((A(29)+A(34)+A(51)+A(57)+A(65) &
       +A(71))*f(26))/2._/**/REALKIND+((A(106)+A(110)+A(116)+A(120))*f(27))/2._/**/REALKIND+((A(107)+A(111)+A(117) &
       +A(121))*f(28))/2._/**/REALKIND+((-A(30)-A(35))*f(29))/2._/**/REALKIND+((A(38)+A(44)+A(76)+A(80))*f(41))/2._/**/REALKIND &
       +((A(40)+A(46)+A(78)+A(82))*f(42))/2._/**/REALKIND
  M2(2) = ((A(162)+A(165)+A(166)+A(167)+A(176)+A(177)+A(178)+A(179))*f(2))/6._/**/REALKIND+((-A(55)-A(69)-A(100) &
       -A(101))*f(3))/6._/**/REALKIND+((-A(103)-A(112)-A(134)-A(135))*f(4))/6._/**/REALKIND+((-A(54)-A(68))*f(5))/6._/**/REALKIND &
       +((-A(102)-A(113))*f(6))/6._/**/REALKIND+(A(25)*f(7))/6._/**/REALKIND+((-A(39)-A(45)-A(77)-A(81))*f(13))/6._/**/REALKIND+(( &
       -A(41)-A(47)-A(79)-A(83))*f(14))/6._/**/REALKIND+((A(136)+A(138)+A(140)+A(142)+A(144)+A(146)+A(148)+A(150)+A(152)+A(154) &
       +A(156)+A(158)+A(160)+A(163)+A(168)+A(170)+A(172)+A(174)+A(180)+A(182)+A(184)+A(186)+A(188)+A(190)+A(192)+A(194)+A(196) &
       +A(198)+A(200)+A(202))*f(17))/6._/**/REALKIND+((A(137)+A(139)+A(141)+A(143)+A(145)+A(147)+A(149)+A(151)+A(153)+A(155) &
       +A(157)+A(159)+A(161)+A(164)+A(169)+A(171)+A(173)+A(175)+A(181)+A(183)+A(185)+A(187)+A(189)+A(191)+A(193)+A(195)+A(197) &
       +A(199)+A(201)+A(203))*f(18))/6._/**/REALKIND+((-A(26)-A(31)-A(48)-A(52)-A(62)-A(66)-A(84)-A(86)-A(92) &
       -A(94))*f(19))/6._/**/REALKIND+((-A(27)-A(32)-A(49)-A(53)-A(63)-A(67)-A(85)-A(87)-A(93)-A(95))*f(20))/6._/**/REALKIND+(( &
       -A(104)-A(108)-A(114)-A(118)-A(122)-A(124)-A(126)-A(128)-A(130)-A(132))*f(21))/6._/**/REALKIND+((-A(105)-A(109)-A(115) &
       -A(119)-A(123)-A(125)-A(127)-A(129)-A(131)-A(133))*f(22))/6._/**/REALKIND+((-A(36)-A(42)-A(58)-A(60)-A(72)-A(74)-A(88) &
       -A(90)-A(96)-A(98))*f(23))/6._/**/REALKIND+((-A(37)-A(43)-A(59)-A(61)-A(73)-A(75)-A(89)-A(91)-A(97) &
       -A(99))*f(24))/6._/**/REALKIND+((-A(28)-A(33)-A(50)-A(56)-A(64)-A(70))*f(25))/6._/**/REALKIND+((-A(29)-A(34)-A(51)-A(57) &
       -A(65)-A(71))*f(26))/6._/**/REALKIND+((-A(106)-A(110)-A(116)-A(120))*f(27))/6._/**/REALKIND+((-A(107)-A(111)-A(117) &
       -A(121))*f(28))/6._/**/REALKIND+((A(30)+A(35))*f(29))/6._/**/REALKIND+((-A(38)-A(44)-A(76)-A(80))*f(41))/6._/**/REALKIND+(( &
       -A(40)-A(46)-A(78)-A(82))*f(42))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pphlljj_eexddxbbxh_1_/**/REALKIND
