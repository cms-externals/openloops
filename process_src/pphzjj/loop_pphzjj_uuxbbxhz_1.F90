
module ol_colourmatrix_pphzjj_uuxbbxhz_1_/**/REALKIND
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
  K1( 3,:) = [  12,   4]
  K1( 4,:) = [   4,  12]
  K1( 5,:) = [   0,  -4]
  K1( 6,:) = [  -4, -12]
  K1( 7,:) = [  12,   4]
  K1( 8,:) = [   4,  12]
  K1( 9,:) = [   0,   4]
  K1(10,:) = [   4,   0]
  K1(11,:) = [ -12,  -4]
  K1(12,:) = [  -4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [ -12,  -4]
  K1(16,:) = [  -4,   0]
  K1(17,:) = [   0,   4]
  K1(18,:) = [   4,   0]
  K1(19,:) = [   0,  -4]
  K1(20,:) = [  -4, -12]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
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
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [   0,   0]
  K1(42,:) = [   0,   0]
  K1(43,:) = [   0,   0]
  K1(44,:) = [   0,   0]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphzjj_uuxbbxhz_1_/**/REALKIND



module ol_forced_parameters_pphzjj_uuxbbxhz_1_/**/REALKIND
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
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphzjj_uuxbbxhz_1_/**/REALKIND

module ol_loop_pphzjj_uuxbbxhz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(28), c(36)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:176)
  ! denominators
  complex(REALKIND), save :: den(187)
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
    f( 1) = (CI*eQED**2*gQCD**2*lambdaHZZ*MW)/(cw**2*sw)
    f( 2) = (CI*countertermnorm*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 3) = (CI*countertermnorm*ctGbb*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 4) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 5) = (CI*countertermnorm*ctVbb*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 6) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 7) = (countertermnorm*ctZGG*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 8) = (CI*eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f( 9) = (eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw*2._/**/REALKIND)
    f(10) = (eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f(11) = (eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(12) = (2*eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(13) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MB*YB)/MQ2sum
    f(14) = (CI*eQED**2*gQCD**2*YB)/(2._/**/REALKIND*MW*sw)
    f(15) = (CI*countertermnorm*eQED**2*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(16) = (CI*countertermnorm*ctGbb*eQED**2*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(17) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(18) = (CI*countertermnorm*ctSbb*eQED**2*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(19) = (CI*countertermnorm*ctVbb*eQED**2*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(20) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f(21) = (countertermnorm*ctZGG*eQED**2*gQCD**4*YB)/(MW*sw*2._/**/REALKIND)
    f(22) = (CI*eQED**2*gQCD**4*integralnorm*SwB*YB)/(2._/**/REALKIND*MW*sw)
    f(23) = (eQED**2*gQCD**4*integralnorm*SwB*YB)/(MW*sw*4._/**/REALKIND)
    f(24) = (eQED**2*gQCD**4*integralnorm*SwB*YB)/(MW*sw*2._/**/REALKIND)
    f(25) = (eQED**2*gQCD**4*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(26) = (eQED**2*gQCD**4*integralnorm*SwF*YB)/(MW*sw)
    f(27) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MT*YT)/MQ2sum
    f(28) = (eQED**2*gQCD**4*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(115)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rMB, H(3), wf(:,-2))
  call wf_A(P(:,4), rMB, H(4), wf(:,-3))
  call wf_S(P(:,5), rMH, H(5), wf(:,-4))
  call wf_V(P(:,6), rMZ, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QS_A(gH,wf(:,-2),wf(:,-4),wf(:,2))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,-5),wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,20),MB,1_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,40),MB,1_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,6))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,7))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,4),wf(:,8))
  call prop_A_Q(wf(:,7),Q(:,11),MB,1_intkind1,wf(:,9))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,-2),wf(:,10))
  call vert_SA_Q(gH,wf(:,-4),wf(:,-3),wf(:,11))
  call prop_Q_A(wf(:,10),Q(:,36),MB,1_intkind1,wf(:,12))
  call prop_A_Q(wf(:,11),Q(:,24),MB,1_intkind1,wf(:,13))
  call vert_VQ_A(wf(:,1),wf(:,12),wf(:,14))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,15))
  call vert_AZ_Q(gZd,wf(:,13),wf(:,-5),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,7),MB,1_intkind1,wf(:,17))
  call vert_QS_A(gH,wf(:,12),wf(:,-4),wf(:,18))
  call vert_SA_Q(gH,wf(:,-4),wf(:,5),wf(:,19))
  call vert_SV_V(wf(:,-4),wf(:,-5),wf(:,20))
  call prop_W_W(wf(:,20),Q(:,48),MZ,1_intkind1,wf(:,21))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,21),wf(:,22))
  call vert_ZQ_A(gZd,wf(:,21),wf(:,-2),wf(:,23))
  call prop_Q_A(wf(:,23),Q(:,52),MB,1_intkind1,wf(:,24))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,25))
  call vert_VQ_A(wf(:,25),wf(:,0),wf(:,26))
  call vert_AZ_Q(gZu,wf(:,-1),wf(:,21),wf(:,27))
  call prop_Q_A(wf(:,26),Q(:,13),ZERO,0_intkind1,wf(:,28))
  call vert_ZQ_A(gZu,wf(:,21),wf(:,0),wf(:,29))
  call vert_AV_Q(wf(:,-1),wf(:,25),wf(:,30))
  call prop_Q_A(wf(:,29),Q(:,49),ZERO,0_intkind1,wf(:,31))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,0),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,33),ZERO,0_intkind1,wf(:,33))
  call vert_QA_V(wf(:,33),wf(:,-1),wf(:,34))
  call vert_QA_V(wf(:,4),wf(:,-3),wf(:,35))
  call vert_AZ_Q(gZu,wf(:,-1),wf(:,-5),wf(:,36))
  call prop_A_Q(wf(:,36),Q(:,34),ZERO,0_intkind1,wf(:,37))
  call vert_QA_V(wf(:,0),wf(:,37),wf(:,38))
  call vert_QA_V(wf(:,-2),wf(:,13),wf(:,39))
  call counter_GG_V(wf(:,1),Q(:,3),wf(:,25),Q(:,12),wf(:,40))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,41))
  call counter_ZQ_A(gZd,wf(:,-5),wf(:,4),wf(:,42))
  call counter_VG_G(wf(:,-5),wf(:,1),Q(:,3),wf(:,43),Q(:,35))
  call counter_VQ_A(wf(:,1),wf(:,12),wf(:,44))
  call counter_AZ_Q(gZd,wf(:,13),wf(:,-5),wf(:,45))
  call counter_QS_A(gH,wf(:,12),wf(:,-4),wf(:,46))
  call vert_QA_V(wf(:,12),wf(:,-3),wf(:,47))
  call counter_SG_G(wf(:,-4),wf(:,1),wf(:,48))
  call counter_SA_Q(gH,wf(:,-4),wf(:,5),wf(:,49))
  call vert_QA_V(wf(:,-2),wf(:,5),wf(:,50))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,51))
  call prop_Q_A(wf(:,8),Q(:,52),MB,1_intkind1,wf(:,52))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,-5),wf(:,53))
  call prop_A_Q(wf(:,53),Q(:,40),MB,1_intkind1,wf(:,54))
  call prop_Q_A(wf(:,18),Q(:,52),MB,1_intkind1,wf(:,55))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,21),wf(:,56))
  call vert_SA_Q(gH,wf(:,-4),wf(:,54),wf(:,57))
  call counter_SA_Q(gH,wf(:,-4),wf(:,-3),wf(:,58))
  call prop_A_Q(wf(:,58),Q(:,24),MB,1_intkind1,wf(:,59))
  call vert_AZ_Q(gZd,wf(:,59),wf(:,-5),wf(:,60))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,61))
  call prop_A_Q(wf(:,16),Q(:,56),MB,1_intkind1,wf(:,62))
  call counter_ZQ_A(gZd,wf(:,-5),wf(:,-2),wf(:,63))
  call prop_Q_A(wf(:,63),Q(:,36),MB,1_intkind1,wf(:,64))
  call vert_VQ_A(wf(:,1),wf(:,64),wf(:,65))
  call prop_A_Q(wf(:,19),Q(:,56),MB,1_intkind1,wf(:,66))
  call counter_ZQ_A(gZd,wf(:,21),wf(:,-2),wf(:,67))
  call prop_A_Q(wf(:,22),Q(:,56),MB,1_intkind1,wf(:,68))
  call vert_QS_A(gH,wf(:,64),wf(:,-4),wf(:,69))
  call counter_QS_A(gH,wf(:,-2),wf(:,-4),wf(:,70))
  call prop_Q_A(wf(:,70),Q(:,20),MB,1_intkind1,wf(:,71))
  call vert_VQ_A(wf(:,1),wf(:,71),wf(:,72))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,71),wf(:,73))
  call counter_SG_G(wf(:,-4),wf(:,25),wf(:,74))
  call counter_QA_V(wf(:,4),wf(:,-3),wf(:,75))
  call vert_QA_V(wf(:,-2),wf(:,59),wf(:,76))
  call counter_QA_V(wf(:,-2),wf(:,13),wf(:,77))
  call vert_QA_V(wf(:,71),wf(:,-3),wf(:,78))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,79))
  call vert_AV_Q(wf(:,-1),wf(:,79),wf(:,80))
  call vert_VQ_A(wf(:,79),wf(:,0),wf(:,81))
  call prop_Q_A(wf(:,81),Q(:,13),ZERO,0_intkind1,wf(:,82))
  call counter_AZ_Q(gZu,wf(:,-1),wf(:,21),wf(:,83))
  call counter_AV_Q(wf(:,-1),wf(:,25),wf(:,84))
  call counter_QA_V(wf(:,33),wf(:,-1),wf(:,85))
  call counter_AZ_Q(gZu,wf(:,-1),wf(:,-5),wf(:,86))
  call prop_A_Q(wf(:,86),Q(:,34),ZERO,0_intkind1,wf(:,87))
  call vert_QA_V(wf(:,0),wf(:,87),wf(:,88))
  call counter_VQ_A(wf(:,25),wf(:,0),wf(:,89))
  call prop_A_Q(wf(:,27),Q(:,50),ZERO,0_intkind1,wf(:,90))
  call counter_ZQ_A(gZu,wf(:,21),wf(:,0),wf(:,91))
  call prop_A_Q(wf(:,30),Q(:,14),ZERO,0_intkind1,wf(:,92))
  call counter_QA_V(wf(:,0),wf(:,37),wf(:,93))
  call counter_ZQ_A(gZu,wf(:,-5),wf(:,0),wf(:,94))
  call prop_Q_A(wf(:,94),Q(:,33),ZERO,0_intkind1,wf(:,95))
  call vert_QA_V(wf(:,95),wf(:,-1),wf(:,96))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,97))
  call vert_VQ_A(wf(:,97),wf(:,4),wf(:,98))
  call vert_AV_Q(wf(:,-3),wf(:,97),wf(:,99))
  call prop_A_Q(wf(:,99),Q(:,11),MB,1_intkind1,wf(:,100))
  call vert_VQ_A(wf(:,97),wf(:,12),wf(:,101))
  call vert_VQ_A(wf(:,97),wf(:,-2),wf(:,102))
  call prop_Q_A(wf(:,102),Q(:,7),MB,1_intkind1,wf(:,103))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,104))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,105))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,106))
  call counter_Q_A(ctbb,wf(:,4),Q(:,20),wf(:,107))
  call prop_A_Q(wf(:,106),Q(:,43),MB,1_intkind1,wf(:,108))
  call counter_A_Q(ctbb,wf(:,5),Q(:,40),wf(:,109))
  call prop_Q_A(wf(:,6),Q(:,23),MB,1_intkind1,wf(:,110))
  call vert_AV_Q(wf(:,-3),wf(:,105),wf(:,111))
  call prop_Q_A(wf(:,107),Q(:,20),MB,1_intkind1,wf(:,112))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,112),wf(:,113))
  call counter_A_Q(ctbb,wf(:,9),Q(:,11),wf(:,114))
  call vert_QA_V(wf(:,12),wf(:,13),wf(:,115))
  call vert_AV_Q(wf(:,13),wf(:,1),wf(:,116))
  call counter_Q_A(ctbb,wf(:,12),Q(:,36),wf(:,117))
  call prop_A_Q(wf(:,116),Q(:,27),MB,1_intkind1,wf(:,118))
  call counter_A_Q(ctbb,wf(:,13),Q(:,24),wf(:,119))
  call prop_Q_A(wf(:,14),Q(:,39),MB,1_intkind1,wf(:,120))
  call vert_VQ_A(wf(:,105),wf(:,-2),wf(:,121))
  call counter_Q_A(ctbb,wf(:,17),Q(:,7),wf(:,122))
  call prop_A_Q(wf(:,119),Q(:,24),MB,1_intkind1,wf(:,123))
  call vert_AZ_Q(gZd,wf(:,123),wf(:,-5),wf(:,124))
  call prop_Q_A(wf(:,117),Q(:,36),MB,1_intkind1,wf(:,125))
  call vert_QS_A(gH,wf(:,125),wf(:,-4),wf(:,126))
  call prop_A_Q(wf(:,109),Q(:,40),MB,1_intkind1,wf(:,127))
  call vert_SA_Q(gH,wf(:,-4),wf(:,127),wf(:,128))
  call counter_Q_A(ctbb,wf(:,24),Q(:,52),wf(:,129))
  call counter_Q_A(ctqq,wf(:,28),Q(:,13),wf(:,130))
  call counter_Q_A(ctqq,wf(:,31),Q(:,49),wf(:,131))
  call counter_V_V(ctGG,wf(:,25),Q(:,12),wf(:,132))
  call vert_VQ_A(wf(:,132),wf(:,0),wf(:,133))
  call vert_AV_Q(wf(:,-1),wf(:,132),wf(:,134))
  call counter_Q_A(ctqq,wf(:,33),Q(:,33),wf(:,135))
  call prop_Q_A(wf(:,135),Q(:,33),ZERO,0_intkind1,wf(:,136))
  call vert_QA_V(wf(:,136),wf(:,-1),wf(:,137))
  call counter_V_V(ctGG,wf(:,34),Q(:,35),wf(:,138))
  call vert_QA_V(wf(:,112),wf(:,-3),wf(:,139))
  call counter_A_Q(ctqq,wf(:,37),Q(:,34),wf(:,140))
  call prop_A_Q(wf(:,140),Q(:,34),ZERO,0_intkind1,wf(:,141))
  call vert_QA_V(wf(:,0),wf(:,141),wf(:,142))
  call counter_V_V(ctGG,wf(:,38),Q(:,35),wf(:,143))
  call vert_QA_V(wf(:,-2),wf(:,123),wf(:,144))
  call vert_QS_A(gH,wf(:,17),wf(:,-4),wf(:,145))
  call prop_Q_A(wf(:,145),Q(:,23),MB,1_intkind1,wf(:,146))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,17),wf(:,147))
  call prop_Q_A(wf(:,147),Q(:,39),MB,1_intkind1,wf(:,148))
  call vert_SA_Q(gH,wf(:,-4),wf(:,9),wf(:,149))
  call prop_A_Q(wf(:,149),Q(:,27),MB,1_intkind1,wf(:,150))
  call vert_AZ_Q(gZd,wf(:,9),wf(:,-5),wf(:,151))
  call prop_A_Q(wf(:,151),Q(:,43),MB,1_intkind1,wf(:,152))
  call vert_VQ_A(wf(:,35),wf(:,0),wf(:,153))
  call prop_Q_A(wf(:,153),Q(:,29),ZERO,0_intkind1,wf(:,154))
  call vert_AV_Q(wf(:,-1),wf(:,35),wf(:,155))
  call prop_A_Q(wf(:,155),Q(:,30),ZERO,0_intkind1,wf(:,156))
  call vert_QA_V(wf(:,52),wf(:,-3),wf(:,157))
  call vert_VQ_A(wf(:,39),wf(:,0),wf(:,158))
  call prop_Q_A(wf(:,158),Q(:,29),ZERO,0_intkind1,wf(:,159))
  call vert_AV_Q(wf(:,-1),wf(:,39),wf(:,160))
  call prop_A_Q(wf(:,160),Q(:,30),ZERO,0_intkind1,wf(:,161))
  call vert_QA_V(wf(:,-2),wf(:,62),wf(:,162))
  call vert_VQ_A(wf(:,34),wf(:,-2),wf(:,163))
  call prop_Q_A(wf(:,163),Q(:,39),MB,1_intkind1,wf(:,164))
  call vert_AV_Q(wf(:,-3),wf(:,34),wf(:,165))
  call prop_A_Q(wf(:,165),Q(:,43),MB,1_intkind1,wf(:,166))
  call vert_VQ_A(wf(:,38),wf(:,-2),wf(:,167))
  call prop_Q_A(wf(:,167),Q(:,39),MB,1_intkind1,wf(:,168))
  call vert_AV_Q(wf(:,-3),wf(:,38),wf(:,169))
  call prop_A_Q(wf(:,169),Q(:,43),MB,1_intkind1,wf(:,170))
  call vert_QA_V(wf(:,55),wf(:,-3),wf(:,171))
  call vert_QA_V(wf(:,-2),wf(:,66),wf(:,172))
  call vert_QA_V(wf(:,31),wf(:,-1),wf(:,173))
  call vert_QA_V(wf(:,0),wf(:,90),wf(:,174))
  call vert_QA_V(wf(:,24),wf(:,-3),wf(:,175))
  call vert_QA_V(wf(:,-2),wf(:,68),wf(:,176))

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
  den(2) = 1 / (Q(5,20) - MB2)
  den(3) = 1 / (Q(5,40) - MB2)
  den(6) = 1 / (Q(5,11) - MB2)
  den(9) = 1 / (Q(5,36) - MB2)
  den(10) = 1 / (Q(5,24) - MB2)
  den(13) = 1 / (Q(5,7) - MB2)
  den(18) = 1 / (Q(5,48) - MZ2)
  den(20) = 1 / (Q(5,52) - MB2)
  den(23) = 1 / (Q(5,12))
  den(24) = 1 / (Q(5,13))
  den(27) = 1 / (Q(5,49))
  den(30) = 1 / (Q(5,33))
  den(31) = 1 / (Q(5,35))
  den(34) = 1 / (Q(5,34))
  den(41) = 1 / (Q(5,28))
  den(46) = 1 / (Q(5,44))
  den(55) = 1 / (Q(5,56) - MB2)
  den(67) = 1 / (Q(5,50))
  den(70) = 1 / (Q(5,14))
  den(76) = 1 / (Q(5,60))
  den(80) = 1 / (Q(5,43) - MB2)
  den(83) = 1 / (Q(5,23) - MB2)
  den(95) = 1 / (Q(5,27) - MB2)
  den(98) = 1 / (Q(5,39) - MB2)
  den(140) = 1 / (Q(5,29))
  den(142) = 1 / (Q(5,30))
  den(154) = 1 / (Q(5,51))

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
  den(25) = den(23)*den(24)
  den(26) = den(18)*den(25)
  den(28) = den(18)*den(27)
  den(29) = den(23)*den(28)
  den(32) = den(30)*den(31)
  den(33) = den(2)*den(32)
  den(35) = den(31)*den(34)
  den(36) = den(2)*den(35)
  den(37) = den(10)*den(32)
  den(38) = den(10)*den(35)
  den(39) = den(1)*den(23)
  den(40) = den(18)*den(39)
  den(42) = den(2)*den(41)
  den(43) = den(1)*den(42)
  den(44) = den(10)*den(41)
  den(45) = den(1)*den(44)
  den(47) = den(9)*den(46)
  den(48) = den(1)*den(47)
  den(49) = den(3)*den(46)
  den(50) = den(1)*den(49)
  den(51) = den(2)*den(20)
  den(52) = den(1)*den(51)
  den(53) = den(9)*den(20)
  den(54) = den(1)*den(53)
  den(56) = den(10)*den(55)
  den(57) = den(1)*den(56)
  den(58) = den(3)*den(55)
  den(59) = den(1)*den(58)
  den(60) = den(7)*den(18)
  den(61) = den(18)*den(55)
  den(62) = den(1)*den(61)
  den(63) = den(23)*den(32)
  den(64) = den(23)*den(35)
  den(65) = den(30)*den(42)
  den(66) = den(30)*den(44)
  den(68) = den(18)*den(67)
  den(69) = den(23)*den(68)
  den(71) = den(23)*den(70)
  den(72) = den(18)*den(71)
  den(73) = den(34)*den(42)
  den(74) = den(34)*den(44)
  den(75) = den(2)*den(3)
  den(77) = den(75)*den(76)
  den(78) = den(1)*den(77)
  den(79) = den(1)*den(3)
  den(81) = den(79)*den(80)
  den(82) = den(2)*den(81)
  den(84) = den(4)*den(83)
  den(85) = den(3)*den(84)
  den(86) = den(1)**2
  den(87) = den(51)*den(86)
  den(88) = den(2)**2
  den(89) = den(7)*den(88)
  den(90) = den(7)*den(51)
  den(91) = den(9)*den(10)
  den(92) = den(76)*den(91)
  den(93) = den(1)*den(92)
  den(94) = den(1)*den(10)
  den(96) = den(94)*den(95)
  den(97) = den(9)*den(96)
  den(99) = den(11)*den(98)
  den(100) = den(10)*den(99)
  den(101) = den(56)*den(86)
  den(102) = den(14)*den(56)
  den(103) = den(10)**2
  den(104) = den(14)*den(103)
  den(105) = den(53)*den(86)
  den(106) = den(9)**2
  den(107) = den(7)*den(106)
  den(108) = den(7)*den(53)
  den(109) = den(58)*den(86)
  den(110) = den(14)*den(58)
  den(111) = den(3)**2
  den(112) = den(14)*den(111)
  den(113) = den(61)*den(86)
  den(114) = den(21)*den(86)
  den(115) = den(14)*den(61)
  den(116) = den(7)*den(21)
  den(117) = den(25)*den(68)
  den(118) = den(28)*den(71)
  den(119) = den(23)**2
  den(120) = den(68)*den(119)
  den(121) = den(28)*den(119)
  den(122) = den(30)**2
  den(123) = den(42)*den(122)
  den(124) = den(32)*den(42)
  den(125) = den(32)*den(88)
  den(126) = den(34)**2
  den(127) = den(42)*den(126)
  den(128) = den(35)*den(42)
  den(129) = den(35)*den(88)
  den(130) = den(44)*den(122)
  den(131) = den(32)*den(44)
  den(132) = den(32)*den(103)
  den(133) = den(44)*den(126)
  den(134) = den(35)*den(44)
  den(135) = den(35)*den(103)
  den(136) = den(14)*den(83)
  den(137) = den(14)*den(98)
  den(138) = den(7)*den(95)
  den(139) = den(7)*den(80)
  den(141) = den(42)*den(140)
  den(143) = den(42)*den(142)
  den(144) = den(51)*den(76)
  den(145) = den(44)*den(140)
  den(146) = den(44)*den(142)
  den(147) = den(56)*den(76)
  den(148) = den(32)*den(98)
  den(149) = den(32)*den(80)
  den(150) = den(35)*den(98)
  den(151) = den(35)*den(80)
  den(152) = den(53)*den(76)
  den(153) = den(58)*den(76)
  den(155) = den(28)*den(154)
  den(156) = den(68)*den(154)
  den(157) = den(21)*den(76)
  den(158) = den(61)*den(76)
  den(159) = den(1)*den(18)*den(23)
  den(160) = den(1)*den(2)*den(3)
  den(161) = den(1)*den(9)*den(10)
  den(162) = den(1)*den(18)
  den(163) = den(18)*den(23)
  den(164) = den(2)*den(30)
  den(165) = den(2)*den(34)
  den(166) = den(10)*den(30)
  den(167) = den(10)*den(34)
  den(168) = den(2)*den(139)
  den(169) = den(1)*den(144)
  den(170) = den(10)*den(137)
  den(171) = den(1)*den(147)
  den(172) = den(9)*den(138)
  den(173) = den(1)*den(152)
  den(174) = den(3)*den(136)
  den(175) = den(1)*den(153)
  den(176) = den(1)*den(157)
  den(177) = den(1)*den(158)
  den(178) = den(23)*den(155)
  den(179) = den(23)*den(156)
  den(180) = den(2)*den(149)
  den(181) = den(30)*den(143)
  den(182) = den(2)*den(151)
  den(183) = den(34)*den(141)
  den(184) = den(10)*den(148)
  den(185) = den(30)*den(146)
  den(186) = den(10)*den(150)
  den(187) = den(34)*den(145)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(115)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,13),wf(:,14)) * den(12)
  A(4) = cont_QA(wf(:,16),wf(:,17)) * den(15)
  A(5) = cont_QA(wf(:,9),wf(:,18)) * den(16)
  A(6) = cont_QA(wf(:,17),wf(:,19)) * den(17)
  A(7) = cont_QA(wf(:,17),wf(:,22)) * den(19)
  A(8) = cont_QA(wf(:,7),wf(:,24)) * den(22)
  A(9) = cont_QA(wf(:,27),wf(:,28)) * den(26)
  A(10) = cont_QA(wf(:,30),wf(:,31)) * den(29)
  A(11) = cont_VV(wf(:,34),wf(:,35)) * den(33)
  A(12) = cont_VV(wf(:,35),wf(:,38)) * den(36)
  A(13) = cont_VV(wf(:,34),wf(:,39)) * den(37)
  A(14) = cont_VV(wf(:,38),wf(:,39)) * den(38)

  A(15) = cont_VV(wf(:,21),wf(:,40)) * den(40)
  A(16) = cont_QA(wf(:,5),wf(:,41)) * den(5)
  A(17) = cont_QA(wf(:,9),wf(:,42)) * den(8)
  A(18) = cont_VV(wf(:,35),wf(:,43)) * den(43)
  A(19) = cont_QA(wf(:,13),wf(:,44)) * den(12)
  A(20) = cont_QA(wf(:,17),wf(:,45)) * den(15)
  A(21) = cont_VV(wf(:,39),wf(:,43)) * den(45)
  A(22) = cont_QA(wf(:,9),wf(:,46)) * den(16)
  A(23) = cont_VV(wf(:,47),wf(:,48)) * den(48)
  A(24) = cont_VV(wf(:,47),wf(:,48)) * den(48)
  A(25) = cont_QA(wf(:,17),wf(:,49)) * den(17)
  A(26) = cont_VV(wf(:,48),wf(:,50)) * den(50)
  A(27) = cont_VV(wf(:,48),wf(:,50)) * den(50)
  A(28) = cont_QA(wf(:,51),wf(:,52)) * den(52)
  A(29) = cont_QA(wf(:,6),wf(:,54)) * den(5)
  A(30) = cont_QA(wf(:,51),wf(:,55)) * den(54)
  A(31) = cont_QA(wf(:,17),wf(:,56)) * den(19)
  A(32) = cont_QA(wf(:,24),wf(:,51)) * den(22)
  A(33) = cont_QA(wf(:,17),wf(:,57)) * den(17)
  A(34) = cont_QA(wf(:,14),wf(:,59)) * den(12)
  A(35) = cont_QA(wf(:,17),wf(:,60)) * den(15)
  A(36) = cont_QA(wf(:,61),wf(:,62)) * den(57)
  A(37) = cont_QA(wf(:,13),wf(:,65)) * den(12)
  A(38) = cont_QA(wf(:,61),wf(:,66)) * den(59)
  A(39) = cont_QA(wf(:,9),wf(:,67)) * den(60)
  A(40) = cont_QA(wf(:,61),wf(:,68)) * den(62)
  A(41) = cont_QA(wf(:,9),wf(:,69)) * den(16)
  A(42) = cont_QA(wf(:,5),wf(:,72)) * den(5)
  A(43) = cont_QA(wf(:,9),wf(:,73)) * den(8)
  A(44) = cont_VV(wf(:,34),wf(:,74)) * den(63)
  A(45) = cont_VV(wf(:,34),wf(:,74)) * den(63)
  A(46) = cont_VV(wf(:,38),wf(:,74)) * den(64)
  A(47) = cont_VV(wf(:,38),wf(:,74)) * den(64)
  A(48) = cont_VV(wf(:,34),wf(:,75)) * den(33)
  A(49) = cont_VV(wf(:,38),wf(:,75)) * den(36)
  A(50) = cont_VV(wf(:,34),wf(:,76)) * den(37)
  A(51) = cont_VV(wf(:,38),wf(:,76)) * den(38)
  A(52) = cont_VV(wf(:,34),wf(:,77)) * den(37)
  A(53) = cont_VV(wf(:,38),wf(:,77)) * den(38)
  A(54) = cont_VV(wf(:,34),wf(:,78)) * den(33)
  A(55) = cont_VV(wf(:,38),wf(:,78)) * den(36)
  A(56) = cont_QA(wf(:,31),wf(:,80)) * den(29)
  A(57) = cont_QA(wf(:,27),wf(:,82)) * den(26)
  A(58) = cont_QA(wf(:,28),wf(:,83)) * den(26)
  A(59) = cont_QA(wf(:,31),wf(:,84)) * den(29)
  A(60) = cont_VV(wf(:,35),wf(:,85)) * den(65)
  A(61) = cont_VV(wf(:,35),wf(:,88)) * den(36)
  A(62) = cont_VV(wf(:,39),wf(:,85)) * den(66)
  A(63) = cont_VV(wf(:,39),wf(:,88)) * den(38)
  A(64) = cont_QA(wf(:,89),wf(:,90)) * den(69)
  A(65) = cont_QA(wf(:,91),wf(:,92)) * den(72)
  A(66) = cont_VV(wf(:,35),wf(:,93)) * den(73)
  A(67) = cont_VV(wf(:,35),wf(:,96)) * den(33)
  A(68) = cont_VV(wf(:,39),wf(:,93)) * den(74)
  A(69) = cont_VV(wf(:,39),wf(:,96)) * den(37)
  A(70) = cont_QA(wf(:,5),wf(:,98)) * den(5)
  A(71) = cont_QA(wf(:,8),wf(:,100)) * den(8)
  A(72) = cont_QA(wf(:,13),wf(:,101)) * den(12)
  A(73) = cont_QA(wf(:,16),wf(:,103)) * den(15)
  A(74) = cont_QA(wf(:,18),wf(:,100)) * den(16)
  A(75) = cont_QA(wf(:,19),wf(:,103)) * den(17)
  A(76) = cont_QA(wf(:,22),wf(:,103)) * den(19)
  A(77) = cont_QA(wf(:,24),wf(:,99)) * den(22)
  A(78) = cont_VV(wf(:,104),wf(:,105)) * den(78)
  A(79) = cont_QA(wf(:,107),wf(:,108)) * den(82)
  A(80) = cont_QA(wf(:,109),wf(:,110)) * den(85)
  A(81) = cont_QA(wf(:,52),wf(:,111)) * den(87)
  A(82) = cont_QA(wf(:,9),wf(:,113)) * den(89)
  A(83) = cont_QA(wf(:,52),wf(:,114)) * den(90)
  A(84) = cont_VV(wf(:,105),wf(:,115)) * den(93)
  A(85) = cont_QA(wf(:,117),wf(:,118)) * den(97)
  A(86) = cont_QA(wf(:,119),wf(:,120)) * den(100)
  A(87) = cont_QA(wf(:,62),wf(:,121)) * den(101)
  A(88) = cont_QA(wf(:,62),wf(:,122)) * den(102)
  A(89) = cont_QA(wf(:,17),wf(:,124)) * den(104)
  A(90) = cont_QA(wf(:,55),wf(:,111)) * den(105)
  A(91) = cont_QA(wf(:,9),wf(:,126)) * den(107)
  A(92) = cont_QA(wf(:,55),wf(:,114)) * den(108)
  A(93) = cont_QA(wf(:,66),wf(:,121)) * den(109)
  A(94) = cont_QA(wf(:,66),wf(:,122)) * den(110)
  A(95) = cont_QA(wf(:,17),wf(:,128)) * den(112)
  A(96) = cont_QA(wf(:,68),wf(:,121)) * den(113)
  A(97) = cont_QA(wf(:,24),wf(:,111)) * den(114)
  A(98) = cont_QA(wf(:,68),wf(:,122)) * den(115)
  A(99) = cont_QA(wf(:,9),wf(:,129)) * den(116)
  A(100) = cont_QA(wf(:,90),wf(:,130)) * den(117)
  A(101) = cont_QA(wf(:,92),wf(:,131)) * den(118)
  A(102) = cont_QA(wf(:,90),wf(:,133)) * den(120)
  A(103) = cont_QA(wf(:,31),wf(:,134)) * den(121)
  A(104) = cont_VV(wf(:,35),wf(:,137)) * den(123)
  A(105) = cont_VV(wf(:,35),wf(:,138)) * den(124)
  A(106) = cont_VV(wf(:,34),wf(:,139)) * den(125)
  A(107) = cont_VV(wf(:,35),wf(:,142)) * den(127)
  A(108) = cont_VV(wf(:,35),wf(:,143)) * den(128)
  A(109) = cont_VV(wf(:,38),wf(:,139)) * den(129)
  A(110) = cont_VV(wf(:,39),wf(:,137)) * den(130)
  A(111) = cont_VV(wf(:,39),wf(:,138)) * den(131)
  A(112) = cont_VV(wf(:,34),wf(:,144)) * den(132)
  A(113) = cont_VV(wf(:,39),wf(:,142)) * den(133)
  A(114) = cont_VV(wf(:,39),wf(:,143)) * den(134)
  A(115) = cont_VV(wf(:,38),wf(:,144)) * den(135)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(115)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(7)-A(8)-A(9)-A(10))*f(1))/2._/**/REALKIND+((-A(1)-A(2)-A(3)-A(4)-A(5)-A(6)-A(11)-A(12)-A(13) &
       -A(14))*f(14))/2._/**/REALKIND
  M1(2) = ((A(7)+A(8)+A(9)+A(10))*f(1))/6._/**/REALKIND+((A(1)+A(2)+A(3)+A(4)+A(5)+A(6)+A(11)+A(12)+A(13) &
       +A(14))*f(14))/6._/**/REALKIND

  M2(1) = ((A(96)+A(97)+A(98)+A(99)+A(100)+A(101)+A(102)+A(103))*f(2))/2._/**/REALKIND+((-A(32)-A(40)-A(56) &
       -A(57))*f(3))/2._/**/REALKIND+((-A(59)-A(64)-A(76)-A(77))*f(4))/2._/**/REALKIND+((-A(31)-A(39))*f(5))/2._/**/REALKIND+(( &
       -A(58)-A(65))*f(6))/2._/**/REALKIND+(A(15)*f(7))/2._/**/REALKIND+((-A(24)-A(27)-A(45)-A(47))*f(13))/2._/**/REALKIND+((A(78) &
       +A(79)+A(80)+A(81)+A(82)+A(83)+A(84)+A(85)+A(86)+A(87)+A(88)+A(89)+A(90)+A(91)+A(92)+A(93)+A(94)+A(95)+A(104)+A(105)+A(106) &
       +A(107)+A(108)+A(109)+A(110)+A(111)+A(112)+A(113)+A(114)+A(115))*f(15))/2._/**/REALKIND+((-A(16)-A(19)-A(28)-A(30)-A(36) &
       -A(38)-A(48)-A(49)-A(52)-A(53))*f(16))/2._/**/REALKIND+((-A(60)-A(62)-A(66)-A(68)-A(70)-A(71)-A(72)-A(73)-A(74) &
       -A(75))*f(17))/2._/**/REALKIND+((-A(22)-A(25)-A(34)-A(35)-A(42)-A(43)-A(50)-A(51)-A(54)-A(55))*f(18))/2._/**/REALKIND+(( &
       -A(17)-A(20)-A(29)-A(33)-A(37)-A(41))*f(19))/2._/**/REALKIND+((-A(61)-A(63)-A(67)-A(69))*f(20))/2._/**/REALKIND+((A(18) &
       +A(21))*f(21))/2._/**/REALKIND+((-A(23)-A(26)-A(44)-A(46))*f(27))/2._/**/REALKIND
  M2(2) = ((-A(96)-A(97)-A(98)-A(99)-A(100)-A(101)-A(102)-A(103))*f(2))/6._/**/REALKIND+((A(32)+A(40)+A(56) &
       +A(57))*f(3))/6._/**/REALKIND+((A(59)+A(64)+A(76)+A(77))*f(4))/6._/**/REALKIND+((A(31)+A(39))*f(5))/6._/**/REALKIND+((A(58) &
       +A(65))*f(6))/6._/**/REALKIND-(A(15)*f(7))/6._/**/REALKIND+((A(24)+A(27)+A(45)+A(47))*f(13))/6._/**/REALKIND+((-A(78)-A(79) &
       -A(80)-A(81)-A(82)-A(83)-A(84)-A(85)-A(86)-A(87)-A(88)-A(89)-A(90)-A(91)-A(92)-A(93)-A(94)-A(95)-A(104)-A(105)-A(106) &
       -A(107)-A(108)-A(109)-A(110)-A(111)-A(112)-A(113)-A(114)-A(115))*f(15))/6._/**/REALKIND+((A(16)+A(19)+A(28)+A(30)+A(36) &
       +A(38)+A(48)+A(49)+A(52)+A(53))*f(16))/6._/**/REALKIND+((A(60)+A(62)+A(66)+A(68)+A(70)+A(71)+A(72)+A(73)+A(74) &
       +A(75))*f(17))/6._/**/REALKIND+((A(22)+A(25)+A(34)+A(35)+A(42)+A(43)+A(50)+A(51)+A(54)+A(55))*f(18))/6._/**/REALKIND &
       +((A(17)+A(20)+A(29)+A(33)+A(37)+A(41))*f(19))/6._/**/REALKIND+((A(61)+A(63)+A(67)+A(69))*f(20))/6._/**/REALKIND+((-A(18) &
       -A(21))*f(21))/6._/**/REALKIND+((A(23)+A(26)+A(44)+A(46))*f(27))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pphzjj_uuxbbxhz_1_/**/REALKIND
