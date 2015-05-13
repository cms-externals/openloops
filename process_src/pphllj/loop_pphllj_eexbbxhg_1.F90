
module ol_colourmatrix_pphllj_eexbbxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(23,1), K2(1,1), KL(1,1)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  12]
  K1( 2,:) = [   0]
  K1( 3,:) = [   0]
  K1( 4,:) = [   0]
  K1( 5,:) = [   0]
  K1( 6,:) = [   0]
  K1( 7,:) = [  16]
  K1( 8,:) = [   0]
  K1( 9,:) = [   0]
  K1(10,:) = [   2]
  K1(11,:) = [  16]
  K1(12,:) = [   0]
  K1(13,:) = [   0]
  K1(14,:) = [   0]
  K1(15,:) = [   0]
  K1(16,:) = [   0]
  K1(17,:) = [   0]
  K1(18,:) = [   0]
  K1(19,:) = [ -18]
  K1(20,:) = [ -18]
  K1(21,:) = [   0]
  K1(22,:) = [  36]
  K1(23,:) = [   0]
  K1 = (1._/**/REALKIND / 3) * K1

  K2(1,:) = [ 4]

  KL(1,:) = [ 4]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphllj_eexbbxhg_1_/**/REALKIND



module ol_forced_parameters_pphllj_eexbbxhg_1_/**/REALKIND
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
end module ol_forced_parameters_pphllj_eexbbxhg_1_/**/REALKIND

module ol_loop_pphllj_eexbbxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(36), c(19)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:134)
  ! denominators
  complex(REALKIND), save :: den(143)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,32)
  ! zero helicity identifier
  logical,           save :: zerohel(32) = .true., zerohel_ct(32) = .true.

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
    f( 1) = (CI*eQED**3*gQCD*lambdaHZZ*MW)/(cw**2*sw)
    f( 2) = (CI*countertermnorm*eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f( 3) = (CI*countertermnorm*ctGbb*eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f( 4) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f( 5) = (countertermnorm*ctZGG*eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f( 6) = (CI*eQED**3*gQCD**3*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f( 7) = (eQED**3*gQCD**3*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f( 8) = (eQED**3*gQCD**3*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f( 9) = (2*eQED**3*gQCD**3*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(10) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**3*MB*YB)/(3._/**/REALKIND*MQ2sum)
    f(11) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**3*MB*YB)/MQ2sum
    f(12) = (CI*eQED**3*gQCD*YB)/(6._/**/REALKIND*MW*sw)
    f(13) = (CI*eQED**3*gQCD*YB)/(2._/**/REALKIND*MW*sw)
    f(14) = (CI*countertermnorm*eQED**3*gQCD**3*YB)/(6._/**/REALKIND*MW*sw)
    f(15) = (CI*countertermnorm*eQED**3*gQCD**3*YB)/(2._/**/REALKIND*MW*sw)
    f(16) = (CI*countertermnorm*ctGbb*eQED**3*gQCD**3*YB)/(6._/**/REALKIND*MW*sw)
    f(17) = (CI*countertermnorm*ctGbb*eQED**3*gQCD**3*YB)/(2._/**/REALKIND*MW*sw)
    f(18) = (CI*countertermnorm*ctSbb*eQED**3*gQCD**3*YB)/(6._/**/REALKIND*MW*sw)
    f(19) = (CI*countertermnorm*ctSbb*eQED**3*gQCD**3*YB)/(2._/**/REALKIND*MW*sw)
    f(20) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**3*YB)/(6._/**/REALKIND*MW*sw)
    f(21) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**3*YB)/(2._/**/REALKIND*MW*sw)
    f(22) = (countertermnorm*ctZGG*eQED**3*gQCD**3*YB)/(MW*sw*2._/**/REALKIND)
    f(23) = (CI*eQED**3*gQCD**3*integralnorm*SwB*YB)/(6._/**/REALKIND*MW*sw)
    f(24) = (CI*eQED**3*gQCD**3*integralnorm*SwB*YB)/(2._/**/REALKIND*MW*sw)
    f(25) = (eQED**3*gQCD**3*integralnorm*SwB*YB)/(MW*sw*6._/**/REALKIND)
    f(26) = (eQED**3*gQCD**3*integralnorm*SwB*YB)/(MW*sw*2._/**/REALKIND)
    f(27) = (eQED**3*gQCD**3*integralnorm*SwF*YB)/(MW*sw*6._/**/REALKIND)
    f(28) = (eQED**3*gQCD**3*integralnorm*SwF*YB)/(MW*sw*3._/**/REALKIND)
    f(29) = (eQED**3*gQCD**3*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(30) = (2*eQED**3*gQCD**3*integralnorm*SwF*YB)/(MW*sw*3._/**/REALKIND)
    f(31) = (eQED**3*gQCD**3*integralnorm*SwF*YB)/(MW*sw)
    f(32) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**3*MT*YT)/(3._/**/REALKIND*MQ2sum)
    f(33) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**3*MT*YT)/MQ2sum
    f(34) = (eQED**3*gQCD**3*integralnorm*SwF*YT)/(MW*sw*6._/**/REALKIND)
    f(35) = (eQED**3*gQCD**3*integralnorm*SwF*YT)/(MW*sw*3._/**/REALKIND)
    f(36) = (eQED**3*gQCD**3*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

  c = [ 9*CI*f(6), f(7), 8*f(7), 3*f(8), 3*f(9), 9*CI*f(23), 9*CI*f(24), f(25), 8*f(25), f(26), 8*f(26), 3*f(27), 3*f(28), 3*f(29) &
    , 3*f(30), 3*f(31), 3*f(34), 3*f(35), 3*f(36) ]
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
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(91)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rMB, H(3), wf(:,-2))
  call wf_A(P(:,4), rMB, H(4), wf(:,-3))
  call wf_S(P(:,5), rMH, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QS_A(gH,wf(:,-2),wf(:,-4),wf(:,2))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,20),MB,1_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,40),MB,1_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,6))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,7))
  call prop_W_W(wf(:,7),Q(:,3),MZ,1_intkind1,wf(:,8))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,4),wf(:,9))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,10))
  call vert_VQ_A(wf(:,-5),wf(:,4),wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,11),MB,1_intkind1,wf(:,12))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,8),wf(:,13))
  call prop_A_Q(wf(:,13),Q(:,11),MB,1_intkind1,wf(:,14))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,15))
  call vert_SA_Q(gH,wf(:,-4),wf(:,-3),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,36),MB,1_intkind1,wf(:,17))
  call prop_A_Q(wf(:,16),Q(:,24),MB,1_intkind1,wf(:,18))
  call vert_VQ_A(wf(:,1),wf(:,17),wf(:,19))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,17),wf(:,20))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,21))
  call vert_AV_Q(wf(:,18),wf(:,-5),wf(:,22))
  call prop_Q_A(wf(:,21),Q(:,7),MB,1_intkind1,wf(:,23))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,-2),wf(:,24))
  call prop_Q_A(wf(:,24),Q(:,7),MB,1_intkind1,wf(:,25))
  call vert_QS_A(gH,wf(:,17),wf(:,-4),wf(:,26))
  call vert_QA_Z(gZd,wf(:,17),wf(:,-3),wf(:,27))
  call vert_SV_V(wf(:,-4),wf(:,8),wf(:,28))
  call prop_W_W(wf(:,27),Q(:,44),MZ,1_intkind1,wf(:,29))
  call vert_SA_Q(gH,wf(:,-4),wf(:,5),wf(:,30))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,5),wf(:,31))
  call prop_W_W(wf(:,31),Q(:,44),MZ,1_intkind1,wf(:,32))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,33))
  call counter_GG_V(wf(:,33),Q(:,12),wf(:,-5),Q(:,32),wf(:,34))
  call prop_W_W(wf(:,28),Q(:,19),MZ,1_intkind1,wf(:,35))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,36))
  call counter_ZQ_A(gZd,wf(:,8),wf(:,4),wf(:,37))
  call counter_VQ_A(wf(:,-5),wf(:,4),wf(:,38))
  call vert_QA_V(wf(:,4),wf(:,-3),wf(:,39))
  call counter_VG_G(wf(:,8),wf(:,-5),Q(:,32),wf(:,40),Q(:,35))
  call counter_VQ_A(wf(:,1),wf(:,17),wf(:,41))
  call counter_ZQ_A(gZd,wf(:,8),wf(:,17),wf(:,42))
  call counter_AV_Q(wf(:,18),wf(:,-5),wf(:,43))
  call vert_QA_V(wf(:,-2),wf(:,18),wf(:,44))
  call counter_QS_A(gH,wf(:,17),wf(:,-4),wf(:,45))
  call counter_SA_Q(gH,wf(:,-4),wf(:,5),wf(:,46))
  call counter_SG_G(wf(:,-4),wf(:,-5),wf(:,47))
  call vert_AV_Q(wf(:,-3),wf(:,47),wf(:,48))
  call vert_VQ_A(wf(:,47),wf(:,-2),wf(:,49))
  call prop_Q_A(wf(:,49),Q(:,52),MB,1_intkind1,wf(:,50))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,51))
  call prop_Q_A(wf(:,11),Q(:,52),MB,1_intkind1,wf(:,52))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,8),wf(:,53))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,54))
  call prop_A_Q(wf(:,54),Q(:,40),MB,1_intkind1,wf(:,55))
  call counter_QA_Z(gZd,wf(:,17),wf(:,-3),wf(:,56))
  call prop_Q_A(wf(:,26),Q(:,52),MB,1_intkind1,wf(:,57))
  call vert_SA_Q(gH,wf(:,-4),wf(:,55),wf(:,58))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,55),wf(:,59))
  call prop_W_W(wf(:,59),Q(:,44),MZ,1_intkind1,wf(:,60))
  call counter_SA_Q(gH,wf(:,-4),wf(:,-3),wf(:,61))
  call prop_A_Q(wf(:,61),Q(:,24),MB,1_intkind1,wf(:,62))
  call vert_AV_Q(wf(:,62),wf(:,-5),wf(:,63))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,64))
  call prop_A_Q(wf(:,22),Q(:,56),MB,1_intkind1,wf(:,65))
  call counter_ZQ_A(gZd,wf(:,8),wf(:,-2),wf(:,66))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,67))
  call prop_Q_A(wf(:,67),Q(:,36),MB,1_intkind1,wf(:,68))
  call vert_VQ_A(wf(:,1),wf(:,68),wf(:,69))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,68),wf(:,70))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,5),wf(:,71))
  call prop_A_Q(wf(:,30),Q(:,56),MB,1_intkind1,wf(:,72))
  call vert_QS_A(gH,wf(:,68),wf(:,-4),wf(:,73))
  call vert_QA_Z(gZd,wf(:,68),wf(:,-3),wf(:,74))
  call prop_W_W(wf(:,74),Q(:,44),MZ,1_intkind1,wf(:,75))
  call counter_QS_A(gH,wf(:,-2),wf(:,-4),wf(:,76))
  call prop_Q_A(wf(:,76),Q(:,20),MB,1_intkind1,wf(:,77))
  call vert_VQ_A(wf(:,1),wf(:,77),wf(:,78))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,77),wf(:,79))
  call vert_VQ_A(wf(:,-5),wf(:,77),wf(:,80))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,81))
  call counter_Q_A(ctbb,wf(:,4),Q(:,20),wf(:,82))
  call prop_A_Q(wf(:,81),Q(:,43),MB,1_intkind1,wf(:,83))
  call vert_AZ_Q(gZd,wf(:,5),wf(:,8),wf(:,84))
  call prop_A_Q(wf(:,84),Q(:,43),MB,1_intkind1,wf(:,85))
  call counter_A_Q(ctbb,wf(:,5),Q(:,40),wf(:,86))
  call prop_Q_A(wf(:,6),Q(:,23),MB,1_intkind1,wf(:,87))
  call prop_Q_A(wf(:,9),Q(:,23),MB,1_intkind1,wf(:,88))
  call prop_Q_A(wf(:,82),Q(:,20),MB,1_intkind1,wf(:,89))
  call vert_VQ_A(wf(:,-5),wf(:,89),wf(:,90))
  call counter_A_Q(ctbb,wf(:,12),Q(:,11),wf(:,91))
  call counter_A_Q(ctbb,wf(:,14),Q(:,11),wf(:,92))
  call vert_AV_Q(wf(:,18),wf(:,1),wf(:,93))
  call counter_Q_A(ctbb,wf(:,17),Q(:,36),wf(:,94))
  call prop_A_Q(wf(:,93),Q(:,27),MB,1_intkind1,wf(:,95))
  call vert_AZ_Q(gZd,wf(:,18),wf(:,8),wf(:,96))
  call prop_A_Q(wf(:,96),Q(:,27),MB,1_intkind1,wf(:,97))
  call counter_A_Q(ctbb,wf(:,18),Q(:,24),wf(:,98))
  call prop_Q_A(wf(:,19),Q(:,39),MB,1_intkind1,wf(:,99))
  call prop_Q_A(wf(:,20),Q(:,39),MB,1_intkind1,wf(:,100))
  call counter_Q_A(ctbb,wf(:,23),Q(:,7),wf(:,101))
  call counter_Q_A(ctbb,wf(:,25),Q(:,7),wf(:,102))
  call prop_A_Q(wf(:,98),Q(:,24),MB,1_intkind1,wf(:,103))
  call vert_AV_Q(wf(:,103),wf(:,-5),wf(:,104))
  call prop_Q_A(wf(:,94),Q(:,36),MB,1_intkind1,wf(:,105))
  call vert_QA_Z(gZd,wf(:,105),wf(:,-3),wf(:,106))
  call vert_QS_A(gH,wf(:,105),wf(:,-4),wf(:,107))
  call prop_A_Q(wf(:,86),Q(:,40),MB,1_intkind1,wf(:,108))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,108),wf(:,109))
  call vert_SA_Q(gH,wf(:,-4),wf(:,108),wf(:,110))
  call vert_QA_V(wf(:,23),wf(:,-3),wf(:,111))
  call vert_QA_V(wf(:,25),wf(:,-3),wf(:,112))
  call vert_QS_A(gH,wf(:,23),wf(:,-4),wf(:,113))
  call prop_Q_A(wf(:,113),Q(:,23),MB,1_intkind1,wf(:,114))
  call vert_QS_A(gH,wf(:,25),wf(:,-4),wf(:,115))
  call prop_Q_A(wf(:,115),Q(:,23),MB,1_intkind1,wf(:,116))
  call vert_VQ_A(wf(:,-5),wf(:,23),wf(:,117))
  call prop_Q_A(wf(:,117),Q(:,39),MB,1_intkind1,wf(:,118))
  call vert_VQ_A(wf(:,-5),wf(:,25),wf(:,119))
  call prop_Q_A(wf(:,119),Q(:,39),MB,1_intkind1,wf(:,120))
  call vert_QA_V(wf(:,-2),wf(:,12),wf(:,121))
  call vert_QA_V(wf(:,-2),wf(:,14),wf(:,122))
  call vert_SA_Q(gH,wf(:,-4),wf(:,12),wf(:,123))
  call prop_A_Q(wf(:,123),Q(:,27),MB,1_intkind1,wf(:,124))
  call vert_SA_Q(gH,wf(:,-4),wf(:,14),wf(:,125))
  call prop_A_Q(wf(:,125),Q(:,27),MB,1_intkind1,wf(:,126))
  call vert_AV_Q(wf(:,12),wf(:,-5),wf(:,127))
  call prop_A_Q(wf(:,127),Q(:,43),MB,1_intkind1,wf(:,128))
  call vert_AV_Q(wf(:,14),wf(:,-5),wf(:,129))
  call prop_A_Q(wf(:,129),Q(:,43),MB,1_intkind1,wf(:,130))
  call vert_ZQ_A(gZd,wf(:,35),wf(:,-2),wf(:,131))
  call prop_Q_A(wf(:,131),Q(:,23),MB,1_intkind1,wf(:,132))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,35),wf(:,133))
  call prop_A_Q(wf(:,133),Q(:,27),MB,1_intkind1,wf(:,134))

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
  den(6) = 1 / (Q(5,3) - MZ2)
  den(9) = 1 / (Q(5,11) - MB2)
  den(14) = 1 / (Q(5,36) - MB2)
  den(15) = 1 / (Q(5,24) - MB2)
  den(20) = 1 / (Q(5,7) - MB2)
  den(27) = 1 / (Q(5,44) - MZ2)
  den(34) = 1 / (Q(5,12))
  den(35) = 1 / (Q(5,19) - MZ2)
  den(38) = 1 / (Q(5,28))
  den(43) = 1 / (Q(5,48))
  den(46) = 1 / (Q(5,52) - MB2)
  den(57) = 1 / (Q(5,56) - MB2)
  den(66) = 1 / (Q(5,43) - MB2)
  den(72) = 1 / (Q(5,23) - MB2)
  den(83) = 1 / (Q(5,27) - MB2)
  den(89) = 1 / (Q(5,39) - MB2)
  den(111) = 1 / (Q(5,15))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(3)*den(7)
  den(10) = den(1)*den(9)
  den(11) = den(2)*den(10)
  den(12) = den(6)*den(9)
  den(13) = den(2)*den(12)
  den(16) = den(1)*den(14)
  den(17) = den(15)*den(16)
  den(18) = den(6)*den(14)
  den(19) = den(15)*den(18)
  den(21) = den(1)*den(20)
  den(22) = den(15)*den(21)
  den(23) = den(6)*den(20)
  den(24) = den(15)*den(23)
  den(25) = den(10)*den(14)
  den(26) = den(12)*den(14)
  den(28) = den(14)*den(27)
  den(29) = den(6)*den(28)
  den(30) = den(3)*den(21)
  den(31) = den(3)*den(23)
  den(32) = den(3)*den(27)
  den(33) = den(6)*den(32)
  den(36) = den(6)*den(35)
  den(37) = den(34)*den(36)
  den(39) = den(2)*den(38)
  den(40) = den(6)*den(39)
  den(41) = den(15)*den(38)
  den(42) = den(6)*den(41)
  den(44) = den(21)*den(43)
  den(45) = den(23)*den(43)
  den(47) = den(43)*den(46)
  den(48) = den(1)*den(47)
  den(49) = den(6)*den(47)
  den(50) = den(2)*den(46)
  den(51) = den(1)*den(50)
  den(52) = den(6)*den(50)
  den(53) = den(14)*den(36)
  den(54) = den(14)*den(46)
  den(55) = den(1)*den(54)
  den(56) = den(6)*den(54)
  den(58) = den(15)*den(57)
  den(59) = den(1)*den(58)
  den(60) = den(6)*den(58)
  den(61) = den(3)*den(36)
  den(62) = den(3)*den(57)
  den(63) = den(1)*den(62)
  den(64) = den(6)*den(62)
  den(65) = den(1)*den(3)
  den(67) = den(65)*den(66)
  den(68) = den(2)*den(67)
  den(69) = den(3)*den(6)
  den(70) = den(66)*den(69)
  den(71) = den(2)*den(70)
  den(73) = den(4)*den(72)
  den(74) = den(3)*den(73)
  den(75) = den(7)*den(72)
  den(76) = den(3)*den(75)
  den(77) = den(2)**2
  den(78) = den(10)*den(77)
  den(79) = den(12)*den(77)
  den(80) = den(10)*den(50)
  den(81) = den(12)*den(50)
  den(82) = den(1)*den(15)
  den(84) = den(82)*den(83)
  den(85) = den(14)*den(84)
  den(86) = den(6)*den(15)
  den(87) = den(83)*den(86)
  den(88) = den(14)*den(87)
  den(90) = den(16)*den(89)
  den(91) = den(15)*den(90)
  den(92) = den(18)*den(89)
  den(93) = den(15)*den(92)
  den(94) = den(21)*den(58)
  den(95) = den(23)*den(58)
  den(96) = den(15)**2
  den(97) = den(21)*den(96)
  den(98) = den(23)*den(96)
  den(99) = den(14)**2
  den(100) = den(36)*den(99)
  den(101) = den(10)*den(99)
  den(102) = den(12)*den(99)
  den(103) = den(10)*den(54)
  den(104) = den(12)*den(54)
  den(105) = den(3)**2
  den(106) = den(36)*den(105)
  den(107) = den(21)*den(62)
  den(108) = den(23)*den(62)
  den(109) = den(21)*den(105)
  den(110) = den(23)*den(105)
  den(112) = den(21)*den(111)
  den(113) = den(23)*den(111)
  den(114) = den(21)*den(72)
  den(115) = den(23)*den(72)
  den(116) = den(21)*den(89)
  den(117) = den(23)*den(89)
  den(118) = den(10)*den(111)
  den(119) = den(12)*den(111)
  den(120) = den(10)*den(83)
  den(121) = den(12)*den(83)
  den(122) = den(10)*den(66)
  den(123) = den(12)*den(66)
  den(124) = den(36)*den(72)
  den(125) = den(36)*den(83)
  den(126) = den(1)*den(34)
  den(127) = den(6)*den(34)
  den(128) = den(1)*den(2)*den(3)
  den(129) = den(2)*den(3)*den(6)
  den(130) = den(1)*den(39)
  den(131) = den(1)*den(14)*den(15)
  den(132) = den(6)*den(14)*den(15)
  den(133) = den(1)*den(41)
  den(134) = den(2)*den(122)
  den(135) = den(2)*den(123)
  den(136) = den(15)*den(116)
  den(137) = den(15)*den(117)
  den(138) = den(14)*den(120)
  den(139) = den(14)*den(121)
  den(140) = den(14)*den(125)
  den(141) = den(3)*den(114)
  den(142) = den(3)*den(115)
  den(143) = den(3)*den(124)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(91)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,5),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,11),wf(:,12)) * den(11)
  A(4) = cont_QA(wf(:,11),wf(:,14)) * den(13)
  A(5) = cont_QA(wf(:,18),wf(:,19)) * den(17)
  A(6) = cont_QA(wf(:,18),wf(:,20)) * den(19)
  A(7) = cont_QA(wf(:,22),wf(:,23)) * den(22)
  A(8) = cont_QA(wf(:,22),wf(:,25)) * den(24)
  A(9) = cont_QA(wf(:,12),wf(:,26)) * den(25)
  A(10) = cont_QA(wf(:,14),wf(:,26)) * den(26)
  A(11) = cont_VV(wf(:,28),wf(:,29)) * den(29)
  A(12) = cont_QA(wf(:,23),wf(:,30)) * den(30)
  A(13) = cont_QA(wf(:,25),wf(:,30)) * den(31)
  A(14) = cont_VV(wf(:,28),wf(:,32)) * den(33)

  A(15) = cont_VV(wf(:,34),wf(:,35)) * den(37)
  A(16) = cont_QA(wf(:,5),wf(:,36)) * den(5)
  A(17) = cont_QA(wf(:,5),wf(:,37)) * den(8)
  A(18) = cont_QA(wf(:,12),wf(:,38)) * den(11)
  A(19) = cont_QA(wf(:,14),wf(:,38)) * den(13)
  A(20) = cont_VV(wf(:,39),wf(:,40)) * den(40)
  A(21) = cont_QA(wf(:,18),wf(:,41)) * den(17)
  A(22) = cont_QA(wf(:,18),wf(:,42)) * den(19)
  A(23) = cont_QA(wf(:,23),wf(:,43)) * den(22)
  A(24) = cont_QA(wf(:,25),wf(:,43)) * den(24)
  A(25) = cont_VV(wf(:,40),wf(:,44)) * den(42)
  A(26) = cont_QA(wf(:,12),wf(:,45)) * den(25)
  A(27) = cont_QA(wf(:,14),wf(:,45)) * den(26)
  A(28) = cont_QA(wf(:,23),wf(:,46)) * den(30)
  A(29) = cont_QA(wf(:,25),wf(:,46)) * den(31)
  A(30) = cont_QA(wf(:,23),wf(:,48)) * den(44)
  A(31) = cont_QA(wf(:,23),wf(:,48)) * den(44)
  A(32) = cont_QA(wf(:,25),wf(:,48)) * den(45)
  A(33) = cont_QA(wf(:,25),wf(:,48)) * den(45)
  A(34) = cont_QA(wf(:,10),wf(:,50)) * den(48)
  A(35) = cont_QA(wf(:,10),wf(:,50)) * den(48)
  A(36) = cont_QA(wf(:,13),wf(:,50)) * den(49)
  A(37) = cont_QA(wf(:,13),wf(:,50)) * den(49)
  A(38) = cont_QA(wf(:,51),wf(:,52)) * den(51)
  A(39) = cont_QA(wf(:,52),wf(:,53)) * den(52)
  A(40) = cont_QA(wf(:,6),wf(:,55)) * den(5)
  A(41) = cont_QA(wf(:,9),wf(:,55)) * den(8)
  A(42) = cont_VV(wf(:,35),wf(:,56)) * den(53)
  A(43) = cont_QA(wf(:,51),wf(:,57)) * den(55)
  A(44) = cont_QA(wf(:,53),wf(:,57)) * den(56)
  A(45) = cont_QA(wf(:,23),wf(:,58)) * den(30)
  A(46) = cont_QA(wf(:,25),wf(:,58)) * den(31)
  A(47) = cont_VV(wf(:,28),wf(:,60)) * den(33)
  A(48) = cont_QA(wf(:,19),wf(:,62)) * den(17)
  A(49) = cont_QA(wf(:,20),wf(:,62)) * den(19)
  A(50) = cont_QA(wf(:,23),wf(:,63)) * den(22)
  A(51) = cont_QA(wf(:,25),wf(:,63)) * den(24)
  A(52) = cont_QA(wf(:,64),wf(:,65)) * den(59)
  A(53) = cont_QA(wf(:,65),wf(:,66)) * den(60)
  A(54) = cont_QA(wf(:,18),wf(:,69)) * den(17)
  A(55) = cont_QA(wf(:,18),wf(:,70)) * den(19)
  A(56) = cont_VV(wf(:,35),wf(:,71)) * den(61)
  A(57) = cont_QA(wf(:,64),wf(:,72)) * den(63)
  A(58) = cont_QA(wf(:,66),wf(:,72)) * den(64)
  A(59) = cont_QA(wf(:,12),wf(:,73)) * den(25)
  A(60) = cont_QA(wf(:,14),wf(:,73)) * den(26)
  A(61) = cont_VV(wf(:,28),wf(:,75)) * den(29)
  A(62) = cont_QA(wf(:,5),wf(:,78)) * den(5)
  A(63) = cont_QA(wf(:,5),wf(:,79)) * den(8)
  A(64) = cont_QA(wf(:,12),wf(:,80)) * den(11)
  A(65) = cont_QA(wf(:,14),wf(:,80)) * den(13)
  A(66) = cont_QA(wf(:,82),wf(:,83)) * den(68)
  A(67) = cont_QA(wf(:,82),wf(:,85)) * den(71)
  A(68) = cont_QA(wf(:,86),wf(:,87)) * den(74)
  A(69) = cont_QA(wf(:,86),wf(:,88)) * den(76)
  A(70) = cont_QA(wf(:,12),wf(:,90)) * den(78)
  A(71) = cont_QA(wf(:,14),wf(:,90)) * den(79)
  A(72) = cont_QA(wf(:,52),wf(:,91)) * den(80)
  A(73) = cont_QA(wf(:,52),wf(:,92)) * den(81)
  A(74) = cont_QA(wf(:,94),wf(:,95)) * den(85)
  A(75) = cont_QA(wf(:,94),wf(:,97)) * den(88)
  A(76) = cont_QA(wf(:,98),wf(:,99)) * den(91)
  A(77) = cont_QA(wf(:,98),wf(:,100)) * den(93)
  A(78) = cont_QA(wf(:,65),wf(:,101)) * den(94)
  A(79) = cont_QA(wf(:,65),wf(:,102)) * den(95)
  A(80) = cont_QA(wf(:,23),wf(:,104)) * den(97)
  A(81) = cont_QA(wf(:,25),wf(:,104)) * den(98)
  A(82) = cont_VV(wf(:,35),wf(:,106)) * den(100)
  A(83) = cont_QA(wf(:,12),wf(:,107)) * den(101)
  A(84) = cont_QA(wf(:,14),wf(:,107)) * den(102)
  A(85) = cont_QA(wf(:,57),wf(:,91)) * den(103)
  A(86) = cont_QA(wf(:,57),wf(:,92)) * den(104)
  A(87) = cont_VV(wf(:,35),wf(:,109)) * den(106)
  A(88) = cont_QA(wf(:,72),wf(:,101)) * den(107)
  A(89) = cont_QA(wf(:,72),wf(:,102)) * den(108)
  A(90) = cont_QA(wf(:,23),wf(:,110)) * den(109)
  A(91) = cont_QA(wf(:,25),wf(:,110)) * den(110)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(91)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(11)+A(14))*f(1)+(A(1)+A(3)+A(5)+A(7)+A(9)+A(12))*f(12)+(A(2)+A(4)+A(6)+A(8)+A(10)+A(13))*f(13)

  M2(1) = (-A(82)-A(87))*f(2)+(A(47)+A(61))*f(3)+(A(42)+A(56))*f(4)-A(15)*f(5)+(A(31)+A(35))*f(10)+(A(33)+A(37))*f(11)+(-A(66) &
       -A(68)-A(70)-A(72)-A(74)-A(76)-A(78)-A(80)-A(83)-A(85)-A(88)-A(90))*f(14)+(-A(67)-A(69)-A(71)-A(73)-A(75)-A(77)-A(79)-A(81) &
       -A(84)-A(86)-A(89)-A(91))*f(15)+(A(18)+A(23)+A(40)+A(45)+A(54)+A(59))*f(16)+(A(19)+A(24)+A(41)+A(46)+A(55)+A(60))*f(17) &
       +(A(26)+A(28)+A(48)+A(50)+A(62)+A(64))*f(18)+(A(27)+A(29)+A(49)+A(51)+A(63)+A(65))*f(19)+(A(16)+A(21)+A(38)+A(43)+A(52) &
       +A(57))*f(20)+(A(17)+A(22)+A(39)+A(44)+A(53)+A(58))*f(21)+(-A(20)-A(25))*f(22)+(A(30)+A(34))*f(32)+(A(32)+A(36))*f(33)

end subroutine colourvectors

end module ol_loop_pphllj_eexbbxhg_1_/**/REALKIND
