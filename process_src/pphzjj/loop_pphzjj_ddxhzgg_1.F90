
module ol_colourmatrix_pphzjj_ddxhzgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(46,2), K2(2,3), KL(2,3)
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

  K2(1,:) = [ 16, -2,  6]
  K2(2,:) = [ -2, 16,  6]
  K2 = (1._/**/REALKIND / 3) * K2

  KL(1,:) = [ 16, -2,  6]
  KL(2,:) = [ -2, 16,  6]
  KL = (1._/**/REALKIND / 3) * KL

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphzjj_ddxhzgg_1_/**/REALKIND



module ol_forced_parameters_pphzjj_ddxhzgg_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphzjj_ddxhzgg_1_/**/REALKIND

module ol_loop_pphzjj_ddxhzgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(26), c(23)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:158)
  ! denominators
  complex(REALKIND), save :: den(169)
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
    f( 2) = (eQED**2*gQCD**2*lambdaHZZ*MW)/(cw**2*sw)
    f( 3) = (CI*countertermnorm*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 4) = (countertermnorm*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 5) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 6) = (countertermnorm*ctGqq*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 7) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 8) = (countertermnorm*ctVqq*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 9) = (countertermnorm*ctVVV*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f(10) = (CI*countertermnorm*ctZGG*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f(11) = (countertermnorm*ctZGG*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f(12) = (CI*eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(2._/**/REALKIND*cw**2*sw)
    f(13) = (CI*eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f(14) = (eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f(15) = (CI*eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(16) = (2*CI*eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(17) = (eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(18) = (2*eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(19) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MB*YB)/MQ2sum
    f(20) = (countertermnorm*ctHGG*eQED**2*gQCD**4*MB*YB)/MQ2sum
    f(21) = (CI*eQED**2*gQCD**4*integralnorm*SwF*YB)/(2._/**/REALKIND*MW*sw)
    f(22) = (eQED**2*gQCD**4*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(23) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MT*YT)/MQ2sum
    f(24) = (countertermnorm*ctHGG*eQED**2*gQCD**4*MT*YT)/MQ2sum
    f(25) = (CI*eQED**2*gQCD**4*integralnorm*SwF*YT)/(2._/**/REALKIND*MW*sw)
    f(26) = (eQED**2*gQCD**4*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

  c = [ 9*CI*f(12), 18*CI*f(12), CI*f(13), 3*CI*f(13), 8*CI*f(13), 9*CI*f(13), 18*CI*f(13), f(14), 3*f(14), 8*f(14), 9*f(14) &
    , 3*CI*f(15), 3*CI*f(16), f(17), 3*f(17), f(18), 3*f(18), 3*CI*f(21), f(22), 3*f(22), 3*CI*f(25), f(26), 3*f(26) ]
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
  complex(REALKIND), intent(out) :: M1(2), M2(3)
  complex(REALKIND) :: A(93)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_S(P(:,3), rMH, H(3), wf(:,-2))
  call wf_V(P(:,4), rMZ, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,1))
  call vert_AV_Q(wf(:,-1),wf(:,-5),wf(:,2))
  call vert_SV_V(wf(:,-2),wf(:,-3),wf(:,3))
  call prop_Q_A(wf(:,1),Q(:,17),ZERO,0_intkind1,wf(:,4))
  call prop_A_Q(wf(:,2),Q(:,34),ZERO,0_intkind1,wf(:,5))
  call prop_W_W(wf(:,3),Q(:,12),MZ,1_intkind1,wf(:,6))
  call vert_QA_Z(gZd,wf(:,4),wf(:,5),wf(:,7))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,6),wf(:,8))
  call vert_VQ_A(wf(:,-5),wf(:,4),wf(:,9))
  call prop_A_Q(wf(:,8),Q(:,14),ZERO,0_intkind1,wf(:,10))
  call vert_VQ_A(wf(:,-5),wf(:,0),wf(:,11))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,33),ZERO,0_intkind1,wf(:,13))
  call prop_A_Q(wf(:,12),Q(:,18),ZERO,0_intkind1,wf(:,14))
  call vert_QA_Z(gZd,wf(:,13),wf(:,14),wf(:,15))
  call vert_ZQ_A(gZd,wf(:,6),wf(:,0),wf(:,16))
  call vert_AV_Q(wf(:,14),wf(:,-5),wf(:,17))
  call prop_Q_A(wf(:,16),Q(:,13),ZERO,0_intkind1,wf(:,18))
  call vert_VQ_A(wf(:,-4),wf(:,13),wf(:,19))
  call vert_AV_Q(wf(:,5),wf(:,-4),wf(:,20))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,21))
  call vert_AV_Q(wf(:,-1),wf(:,21),wf(:,22))
  call vert_VQ_A(wf(:,21),wf(:,0),wf(:,23))
  call prop_Q_A(wf(:,23),Q(:,49),ZERO,0_intkind1,wf(:,24))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,25))
  call counter_GGG_V(ctZGGG,wf(:,25),wf(:,-4),wf(:,-5),wf(:,26))
  call counter_GGG_V(ctZGGG,wf(:,25),wf(:,-5),wf(:,-4),wf(:,27))
  call counter_VG_G(wf(:,6),wf(:,25),Q(:,3),wf(:,28),Q(:,15))
  call vert_UV_W(wf(:,25),Q(:,3),wf(:,-4),Q(:,16),wf(:,29))
  call counter_VG_G(wf(:,6),wf(:,-5),Q(:,32),wf(:,30),Q(:,44))
  call vert_UV_W(wf(:,25),Q(:,3),wf(:,-5),Q(:,32),wf(:,31))
  call counter_VG_G(wf(:,6),wf(:,-4),Q(:,16),wf(:,32),Q(:,28))
  call counter_QA_Z(gZd,wf(:,4),wf(:,5),wf(:,33))
  call vert_QA_V(wf(:,4),wf(:,-1),wf(:,34))
  call counter_VQ_A(wf(:,-5),wf(:,4),wf(:,35))
  call counter_QA_Z(gZd,wf(:,13),wf(:,14),wf(:,36))
  call vert_QA_V(wf(:,0),wf(:,14),wf(:,37))
  call counter_AV_Q(wf(:,14),wf(:,-5),wf(:,38))
  call vert_QA_V(wf(:,13),wf(:,-1),wf(:,39))
  call counter_VQ_A(wf(:,-4),wf(:,13),wf(:,40))
  call vert_QA_V(wf(:,0),wf(:,5),wf(:,41))
  call counter_AV_Q(wf(:,5),wf(:,-4),wf(:,42))
  call counter_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,43))
  call vert_AV_Q(wf(:,-1),wf(:,43),wf(:,44))
  call vert_VQ_A(wf(:,43),wf(:,0),wf(:,45))
  call prop_Q_A(wf(:,45),Q(:,49),ZERO,0_intkind1,wf(:,46))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,0),wf(:,47))
  call counter_SG_G(wf(:,-2),wf(:,-5),wf(:,48))
  call prop_Q_A(wf(:,47),Q(:,9),ZERO,0_intkind1,wf(:,49))
  call vert_QA_V(wf(:,49),wf(:,14),wf(:,50))
  call vert_QA_V(wf(:,49),wf(:,-1),wf(:,51))
  call counter_SG_G(wf(:,-2),wf(:,21),wf(:,52))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,48),Q(:,36),wf(:,53))
  call vert_AV_Q(wf(:,-1),wf(:,48),wf(:,54))
  call vert_VQ_A(wf(:,-4),wf(:,49),wf(:,55))
  call prop_A_Q(wf(:,54),Q(:,38),ZERO,0_intkind1,wf(:,56))
  call counter_SG_G(wf(:,-2),wf(:,-4),wf(:,57))
  call vert_QA_V(wf(:,49),wf(:,5),wf(:,58))
  call vert_UV_W(wf(:,57),Q(:,20),wf(:,-5),Q(:,32),wf(:,59))
  call vert_AV_Q(wf(:,-1),wf(:,57),wf(:,60))
  call vert_VQ_A(wf(:,-5),wf(:,49),wf(:,61))
  call prop_A_Q(wf(:,60),Q(:,22),ZERO,0_intkind1,wf(:,62))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-3),wf(:,63))
  call prop_A_Q(wf(:,63),Q(:,10),ZERO,0_intkind1,wf(:,64))
  call vert_QA_V(wf(:,4),wf(:,64),wf(:,65))
  call vert_QA_V(wf(:,0),wf(:,64),wf(:,66))
  call vert_VQ_A(wf(:,48),wf(:,0),wf(:,67))
  call vert_AV_Q(wf(:,64),wf(:,-4),wf(:,68))
  call prop_Q_A(wf(:,67),Q(:,37),ZERO,0_intkind1,wf(:,69))
  call vert_QA_V(wf(:,13),wf(:,64),wf(:,70))
  call vert_VQ_A(wf(:,57),wf(:,0),wf(:,71))
  call vert_AV_Q(wf(:,64),wf(:,-5),wf(:,72))
  call prop_Q_A(wf(:,71),Q(:,21),ZERO,0_intkind1,wf(:,73))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,4),wf(:,74))
  call vert_AZ_Q(gZd,wf(:,14),wf(:,-3),wf(:,75))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,13),wf(:,76))
  call vert_AZ_Q(gZd,wf(:,5),wf(:,-3),wf(:,77))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,6),wf(:,78))
  call prop_Q_A(wf(:,9),Q(:,49),ZERO,0_intkind1,wf(:,79))
  call counter_AV_Q(wf(:,-1),wf(:,-5),wf(:,80))
  call prop_A_Q(wf(:,80),Q(:,34),ZERO,0_intkind1,wf(:,81))
  call vert_QA_Z(gZd,wf(:,4),wf(:,81),wf(:,82))
  call prop_Q_A(wf(:,19),Q(:,49),ZERO,0_intkind1,wf(:,83))
  call counter_AV_Q(wf(:,-1),wf(:,21),wf(:,84))
  call vert_AV_Q(wf(:,81),wf(:,-4),wf(:,85))
  call counter_AV_Q(wf(:,-1),wf(:,-4),wf(:,86))
  call prop_A_Q(wf(:,86),Q(:,18),ZERO,0_intkind1,wf(:,87))
  call vert_QA_Z(gZd,wf(:,13),wf(:,87),wf(:,88))
  call vert_AV_Q(wf(:,87),wf(:,-5),wf(:,89))
  call counter_ZQ_A(gZd,wf(:,6),wf(:,0),wf(:,90))
  call prop_A_Q(wf(:,17),Q(:,50),ZERO,0_intkind1,wf(:,91))
  call counter_VQ_A(wf(:,-5),wf(:,0),wf(:,92))
  call prop_Q_A(wf(:,92),Q(:,33),ZERO,0_intkind1,wf(:,93))
  call vert_QA_Z(gZd,wf(:,93),wf(:,14),wf(:,94))
  call prop_A_Q(wf(:,20),Q(:,50),ZERO,0_intkind1,wf(:,95))
  call prop_A_Q(wf(:,22),Q(:,50),ZERO,0_intkind1,wf(:,96))
  call counter_VQ_A(wf(:,21),wf(:,0),wf(:,97))
  call vert_VQ_A(wf(:,-4),wf(:,93),wf(:,98))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,99))
  call prop_Q_A(wf(:,99),Q(:,17),ZERO,0_intkind1,wf(:,100))
  call vert_QA_Z(gZd,wf(:,100),wf(:,5),wf(:,101))
  call vert_VQ_A(wf(:,-5),wf(:,100),wf(:,102))
  call vert_AZ_Q(gZd,wf(:,5),wf(:,6),wf(:,103))
  call counter_Q_A(ctqq,wf(:,4),Q(:,17),wf(:,104))
  call prop_A_Q(wf(:,103),Q(:,46),ZERO,0_intkind1,wf(:,105))
  call vert_ZQ_A(gZd,wf(:,6),wf(:,4),wf(:,106))
  call counter_A_Q(ctqq,wf(:,5),Q(:,34),wf(:,107))
  call prop_Q_A(wf(:,106),Q(:,29),ZERO,0_intkind1,wf(:,108))
  call prop_Q_A(wf(:,104),Q(:,17),ZERO,0_intkind1,wf(:,109))
  call vert_VQ_A(wf(:,-5),wf(:,109),wf(:,110))
  call counter_A_Q(ctqq,wf(:,10),Q(:,14),wf(:,111))
  call vert_AZ_Q(gZd,wf(:,14),wf(:,6),wf(:,112))
  call counter_Q_A(ctqq,wf(:,13),Q(:,33),wf(:,113))
  call prop_A_Q(wf(:,112),Q(:,30),ZERO,0_intkind1,wf(:,114))
  call vert_ZQ_A(gZd,wf(:,6),wf(:,13),wf(:,115))
  call counter_A_Q(ctqq,wf(:,14),Q(:,18),wf(:,116))
  call prop_Q_A(wf(:,115),Q(:,45),ZERO,0_intkind1,wf(:,117))
  call counter_Q_A(ctqq,wf(:,18),Q(:,13),wf(:,118))
  call prop_A_Q(wf(:,116),Q(:,18),ZERO,0_intkind1,wf(:,119))
  call vert_AV_Q(wf(:,119),wf(:,-5),wf(:,120))
  call prop_Q_A(wf(:,113),Q(:,33),ZERO,0_intkind1,wf(:,121))
  call vert_VQ_A(wf(:,-4),wf(:,121),wf(:,122))
  call prop_A_Q(wf(:,107),Q(:,34),ZERO,0_intkind1,wf(:,123))
  call vert_AV_Q(wf(:,123),wf(:,-4),wf(:,124))
  call counter_Q_A(ctqq,wf(:,24),Q(:,49),wf(:,125))
  call counter_V_V(ctGG,wf(:,21),Q(:,48),wf(:,126))
  call vert_VQ_A(wf(:,126),wf(:,0),wf(:,127))
  call vert_AV_Q(wf(:,-1),wf(:,126),wf(:,128))
  call vert_UV_W(wf(:,51),Q(:,11),wf(:,-4),Q(:,16),wf(:,129))
  call vert_UV_W(wf(:,51),Q(:,11),wf(:,-5),Q(:,32),wf(:,130))
  call prop_Q_A(wf(:,55),Q(:,25),ZERO,0_intkind1,wf(:,131))
  call vert_QA_V(wf(:,131),wf(:,-1),wf(:,132))
  call prop_Q_A(wf(:,61),Q(:,41),ZERO,0_intkind1,wf(:,133))
  call vert_QA_V(wf(:,133),wf(:,-1),wf(:,134))
  call vert_UV_W(wf(:,66),Q(:,11),wf(:,-4),Q(:,16),wf(:,135))
  call vert_UV_W(wf(:,66),Q(:,11),wf(:,-5),Q(:,32),wf(:,136))
  call prop_A_Q(wf(:,68),Q(:,26),ZERO,0_intkind1,wf(:,137))
  call vert_QA_V(wf(:,0),wf(:,137),wf(:,138))
  call prop_A_Q(wf(:,72),Q(:,42),ZERO,0_intkind1,wf(:,139))
  call vert_QA_V(wf(:,0),wf(:,139),wf(:,140))
  call vert_QA_V(wf(:,18),wf(:,-1),wf(:,141))
  call vert_QA_V(wf(:,0),wf(:,10),wf(:,142))
  call vert_VQ_A(wf(:,-4),wf(:,18),wf(:,143))
  call prop_Q_A(wf(:,143),Q(:,29),ZERO,0_intkind1,wf(:,144))
  call vert_VQ_A(wf(:,-5),wf(:,18),wf(:,145))
  call prop_Q_A(wf(:,145),Q(:,45),ZERO,0_intkind1,wf(:,146))
  call vert_AV_Q(wf(:,10),wf(:,-4),wf(:,147))
  call prop_A_Q(wf(:,147),Q(:,30),ZERO,0_intkind1,wf(:,148))
  call vert_AV_Q(wf(:,10),wf(:,-5),wf(:,149))
  call prop_A_Q(wf(:,149),Q(:,46),ZERO,0_intkind1,wf(:,150))
  call prop_Q_A(wf(:,74),Q(:,25),ZERO,0_intkind1,wf(:,151))
  call vert_QA_V(wf(:,151),wf(:,-1),wf(:,152))
  call prop_A_Q(wf(:,75),Q(:,26),ZERO,0_intkind1,wf(:,153))
  call vert_QA_V(wf(:,0),wf(:,153),wf(:,154))
  call prop_Q_A(wf(:,76),Q(:,41),ZERO,0_intkind1,wf(:,155))
  call vert_QA_V(wf(:,155),wf(:,-1),wf(:,156))
  call prop_A_Q(wf(:,77),Q(:,42),ZERO,0_intkind1,wf(:,157))
  call vert_QA_V(wf(:,0),wf(:,157),wf(:,158))

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
  den(3) = 1 / (Q(5,12) - MZ2)
  den(6) = 1 / (Q(5,14))
  den(9) = 1 / (Q(5,33))
  den(10) = 1 / (Q(5,18))
  den(13) = 1 / (Q(5,13))
  den(18) = 1 / (Q(5,48))
  den(20) = 1 / (Q(5,49))
  den(23) = 1 / (Q(5,3))
  den(26) = 1 / (Q(5,19))
  den(29) = 1 / (Q(5,35))
  den(40) = 1 / (Q(5,9))
  den(41) = 1 / (Q(5,36))
  den(44) = 1 / (Q(5,11))
  den(48) = 1 / (Q(5,38))
  den(51) = 1 / (Q(5,20))
  den(55) = 1 / (Q(5,22))
  den(58) = 1 / (Q(5,10))
  den(64) = 1 / (Q(5,37))
  den(70) = 1 / (Q(5,21))
  den(81) = 1 / (Q(5,50))
  den(90) = 1 / (Q(5,46))
  den(94) = 1 / (Q(5,29))
  den(101) = 1 / (Q(5,30))
  den(105) = 1 / (Q(5,45))
  den(122) = 1 / (Q(5,27))
  den(124) = 1 / (Q(5,43))
  den(128) = 1 / (Q(5,25))
  den(131) = 1 / (Q(5,41))
  den(138) = 1 / (Q(5,26))
  den(141) = 1 / (Q(5,42))
  den(144) = 1 / (Q(5,15))

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
  den(24) = den(3)*den(23)
  den(25) = den(18)*den(24)
  den(27) = den(23)*den(26)
  den(28) = den(3)*den(27)
  den(30) = den(23)*den(29)
  den(31) = den(3)*den(30)
  den(32) = den(1)*den(26)
  den(33) = den(3)*den(32)
  den(34) = den(10)*den(26)
  den(35) = den(3)*den(34)
  den(36) = den(9)*den(29)
  den(37) = den(3)*den(36)
  den(38) = den(2)*den(29)
  den(39) = den(3)*den(38)
  den(42) = den(10)*den(40)
  den(43) = den(41)*den(42)
  den(45) = den(40)*den(44)
  den(46) = den(18)*den(45)
  den(47) = den(41)*den(45)
  den(49) = den(41)*den(48)
  den(50) = den(40)*den(49)
  den(52) = den(2)*den(40)
  den(53) = den(51)*den(52)
  den(54) = den(45)*den(51)
  den(56) = den(51)*den(55)
  den(57) = den(40)*den(56)
  den(59) = den(1)*den(58)
  den(60) = den(41)*den(59)
  den(61) = den(44)*den(58)
  den(62) = den(18)*den(61)
  den(63) = den(41)*den(61)
  den(65) = den(41)*den(64)
  den(66) = den(58)*den(65)
  den(67) = den(9)*den(58)
  den(68) = den(51)*den(67)
  den(69) = den(51)*den(61)
  den(71) = den(51)*den(70)
  den(72) = den(58)*den(71)
  den(73) = den(1)*den(49)
  den(74) = den(10)*den(65)
  den(75) = den(9)*den(56)
  den(76) = den(2)*den(71)
  den(77) = den(1)*den(20)
  den(78) = den(3)*den(77)
  den(79) = den(9)*den(20)
  den(80) = den(3)*den(79)
  den(82) = den(10)*den(81)
  den(83) = den(3)*den(82)
  den(84) = den(2)*den(81)
  den(85) = den(3)*den(84)
  den(86) = den(18)*den(81)
  den(87) = den(3)*den(86)
  den(88) = den(7)*den(18)
  den(89) = den(2)*den(3)
  den(91) = den(89)*den(90)
  den(92) = den(1)*den(91)
  den(93) = den(1)*den(3)
  den(95) = den(93)*den(94)
  den(96) = den(2)*den(95)
  den(97) = den(1)**2
  den(98) = den(7)*den(97)
  den(99) = den(7)*den(77)
  den(100) = den(3)*den(10)
  den(102) = den(100)*den(101)
  den(103) = den(9)*den(102)
  den(104) = den(3)*den(9)
  den(106) = den(104)*den(105)
  den(107) = den(10)*den(106)
  den(108) = den(14)*den(82)
  den(109) = den(10)**2
  den(110) = den(14)*den(109)
  den(111) = den(9)**2
  den(112) = den(7)*den(111)
  den(113) = den(7)*den(79)
  den(114) = den(14)*den(84)
  den(115) = den(2)**2
  den(116) = den(14)*den(115)
  den(117) = den(14)*den(86)
  den(118) = den(7)*den(21)
  den(119) = den(18)**2
  den(120) = den(7)*den(119)
  den(121) = den(14)*den(119)
  den(123) = den(42)*den(122)
  den(125) = den(52)*den(124)
  den(126) = den(45)*den(122)
  den(127) = den(45)*den(124)
  den(129) = den(40)*den(128)
  den(130) = den(122)*den(129)
  den(132) = den(40)*den(131)
  den(133) = den(124)*den(132)
  den(134) = den(59)*den(122)
  den(135) = den(67)*den(124)
  den(136) = den(61)*den(122)
  den(137) = den(61)*den(124)
  den(139) = den(58)*den(138)
  den(140) = den(122)*den(139)
  den(142) = den(58)*den(141)
  den(143) = den(124)*den(142)
  den(145) = den(14)*den(144)
  den(146) = den(7)*den(144)
  den(147) = den(14)*den(94)
  den(148) = den(14)*den(105)
  den(149) = den(7)*den(101)
  den(150) = den(7)*den(90)
  den(151) = den(1)*den(128)
  den(152) = den(122)*den(151)
  den(153) = den(10)*den(138)
  den(154) = den(122)*den(153)
  den(155) = den(9)*den(131)
  den(156) = den(124)*den(155)
  den(157) = den(2)*den(141)
  den(158) = den(124)*den(157)
  den(159) = den(3)*den(18)*den(23)
  den(160) = den(18)*den(23)
  den(161) = den(1)*den(2)*den(3)
  den(162) = den(3)*den(9)*den(10)
  den(163) = den(3)*den(18)
  den(164) = den(1)*den(150)
  den(165) = den(10)*den(148)
  den(166) = den(9)*den(149)
  den(167) = den(2)*den(147)
  den(168) = den(18)*den(145)
  den(169) = den(18)*den(146)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(93)

  A(1) = cont_VV(wf(:,6),wf(:,7)) * den(5)
  A(2) = cont_QA(wf(:,9),wf(:,10)) * den(8)
  A(3) = cont_VV(wf(:,6),wf(:,15)) * den(12)
  A(4) = cont_QA(wf(:,17),wf(:,18)) * den(15)
  A(5) = cont_QA(wf(:,10),wf(:,19)) * den(16)
  A(6) = cont_QA(wf(:,18),wf(:,20)) * den(17)
  A(7) = cont_QA(wf(:,18),wf(:,22)) * den(19)
  A(8) = cont_QA(wf(:,8),wf(:,24)) * den(22)

  A(9) = cont_VV(wf(:,6),wf(:,26)) * den(24)
  A(10) = cont_VV(wf(:,6),wf(:,27)) * den(24)
  A(11) = cont_VV(wf(:,21),wf(:,28)) * den(25)
  A(12) = cont_VV(wf(:,29),wf(:,30)) * den(28)
  A(13) = cont_VV(wf(:,31),wf(:,32)) * den(31)
  A(14) = cont_VV(wf(:,6),wf(:,33)) * den(5)
  A(15) = cont_VV(wf(:,30),wf(:,34)) * den(33)
  A(16) = cont_QA(wf(:,10),wf(:,35)) * den(8)
  A(17) = cont_VV(wf(:,6),wf(:,36)) * den(12)
  A(18) = cont_VV(wf(:,30),wf(:,37)) * den(35)
  A(19) = cont_QA(wf(:,18),wf(:,38)) * den(15)
  A(20) = cont_VV(wf(:,32),wf(:,39)) * den(37)
  A(21) = cont_QA(wf(:,10),wf(:,40)) * den(16)
  A(22) = cont_VV(wf(:,32),wf(:,41)) * den(39)
  A(23) = cont_QA(wf(:,18),wf(:,42)) * den(17)
  A(24) = cont_QA(wf(:,18),wf(:,44)) * den(19)
  A(25) = cont_QA(wf(:,8),wf(:,46)) * den(22)
  A(26) = cont_VV(wf(:,48),wf(:,50)) * den(43)
  A(27) = cont_VV(wf(:,48),wf(:,50)) * den(43)
  A(28) = cont_VV(wf(:,51),wf(:,52)) * den(46)
  A(29) = cont_VV(wf(:,51),wf(:,52)) * den(46)
  A(30) = cont_VV(wf(:,51),wf(:,53)) * den(47)
  A(31) = cont_VV(wf(:,51),wf(:,53)) * den(47)
  A(32) = cont_QA(wf(:,55),wf(:,56)) * den(50)
  A(33) = cont_QA(wf(:,55),wf(:,56)) * den(50)
  A(34) = cont_VV(wf(:,57),wf(:,58)) * den(53)
  A(35) = cont_VV(wf(:,57),wf(:,58)) * den(53)
  A(36) = cont_VV(wf(:,51),wf(:,59)) * den(54)
  A(37) = cont_VV(wf(:,51),wf(:,59)) * den(54)
  A(38) = cont_QA(wf(:,61),wf(:,62)) * den(57)
  A(39) = cont_QA(wf(:,61),wf(:,62)) * den(57)
  A(40) = cont_VV(wf(:,48),wf(:,65)) * den(60)
  A(41) = cont_VV(wf(:,48),wf(:,65)) * den(60)
  A(42) = cont_VV(wf(:,52),wf(:,66)) * den(62)
  A(43) = cont_VV(wf(:,52),wf(:,66)) * den(62)
  A(44) = cont_VV(wf(:,53),wf(:,66)) * den(63)
  A(45) = cont_VV(wf(:,53),wf(:,66)) * den(63)
  A(46) = cont_QA(wf(:,68),wf(:,69)) * den(66)
  A(47) = cont_QA(wf(:,68),wf(:,69)) * den(66)
  A(48) = cont_VV(wf(:,57),wf(:,70)) * den(68)
  A(49) = cont_VV(wf(:,57),wf(:,70)) * den(68)
  A(50) = cont_VV(wf(:,59),wf(:,66)) * den(69)
  A(51) = cont_VV(wf(:,59),wf(:,66)) * den(69)
  A(52) = cont_QA(wf(:,72),wf(:,73)) * den(72)
  A(53) = cont_QA(wf(:,72),wf(:,73)) * den(72)
  A(54) = cont_QA(wf(:,56),wf(:,74)) * den(73)
  A(55) = cont_QA(wf(:,56),wf(:,74)) * den(73)
  A(56) = cont_QA(wf(:,69),wf(:,75)) * den(74)
  A(57) = cont_QA(wf(:,69),wf(:,75)) * den(74)
  A(58) = cont_QA(wf(:,62),wf(:,76)) * den(75)
  A(59) = cont_QA(wf(:,62),wf(:,76)) * den(75)
  A(60) = cont_QA(wf(:,73),wf(:,77)) * den(76)
  A(61) = cont_QA(wf(:,73),wf(:,77)) * den(76)
  A(62) = cont_QA(wf(:,78),wf(:,79)) * den(78)
  A(63) = cont_VV(wf(:,6),wf(:,82)) * den(5)
  A(64) = cont_QA(wf(:,78),wf(:,83)) * den(80)
  A(65) = cont_QA(wf(:,18),wf(:,84)) * den(19)
  A(66) = cont_QA(wf(:,24),wf(:,78)) * den(22)
  A(67) = cont_QA(wf(:,18),wf(:,85)) * den(17)
  A(68) = cont_VV(wf(:,6),wf(:,88)) * den(12)
  A(69) = cont_QA(wf(:,18),wf(:,89)) * den(15)
  A(70) = cont_QA(wf(:,90),wf(:,91)) * den(83)
  A(71) = cont_VV(wf(:,6),wf(:,94)) * den(12)
  A(72) = cont_QA(wf(:,90),wf(:,95)) * den(85)
  A(73) = cont_QA(wf(:,90),wf(:,96)) * den(87)
  A(74) = cont_QA(wf(:,10),wf(:,97)) * den(88)
  A(75) = cont_QA(wf(:,10),wf(:,98)) * den(16)
  A(76) = cont_VV(wf(:,6),wf(:,101)) * den(5)
  A(77) = cont_QA(wf(:,10),wf(:,102)) * den(8)
  A(78) = cont_QA(wf(:,104),wf(:,105)) * den(92)
  A(79) = cont_QA(wf(:,107),wf(:,108)) * den(96)
  A(80) = cont_QA(wf(:,10),wf(:,110)) * den(98)
  A(81) = cont_QA(wf(:,79),wf(:,111)) * den(99)
  A(82) = cont_QA(wf(:,113),wf(:,114)) * den(103)
  A(83) = cont_QA(wf(:,116),wf(:,117)) * den(107)
  A(84) = cont_QA(wf(:,91),wf(:,118)) * den(108)
  A(85) = cont_QA(wf(:,18),wf(:,120)) * den(110)
  A(86) = cont_QA(wf(:,10),wf(:,122)) * den(112)
  A(87) = cont_QA(wf(:,83),wf(:,111)) * den(113)
  A(88) = cont_QA(wf(:,95),wf(:,118)) * den(114)
  A(89) = cont_QA(wf(:,18),wf(:,124)) * den(116)
  A(90) = cont_QA(wf(:,96),wf(:,118)) * den(117)
  A(91) = cont_QA(wf(:,10),wf(:,125)) * den(118)
  A(92) = cont_QA(wf(:,10),wf(:,127)) * den(120)
  A(93) = cont_QA(wf(:,18),wf(:,128)) * den(121)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(93)
  complex(REALKIND), intent(out) :: M1(2), M2(3)

  M1(1) = (-A(3)-A(4)-A(5))*f(1)+CI*(-A(7)-A(8))*f(2)
  M1(2) = (-A(1)-A(2)-A(6))*f(1)+CI*(A(7)+A(8))*f(2)

  M2(1) = -(A(9)*f(3))/2._/**/REALKIND+(A(82)+A(83)+A(84)+A(85)+A(86)+A(87))*f(3)+CI*(A(90)+A(91)+A(92)+A(93))*f(4)+(-A(19)-A(21) &
       -A(68)-A(69)-A(71)-A(75))*f(5)+CI*(-A(65)-A(74))*f(6)+(-A(17)-A(64)-A(70))*f(7)+CI*(-A(66)-A(73))*f(8)+CI*(-A(24) &
       -A(25))*f(9)+CI*(-A(11)-A(12)+A(13))*f(10)+(A(18)+A(20))*f(11)+(-A(27)-A(39)-A(47)-A(49)-A(57)-A(59))*f(19)+CI*(-A(29) &
       -A(31)-A(37)-A(43)-A(45)-A(51))*f(20)+(-A(26)-A(38)-A(46)-A(48)-A(56)-A(58))*f(23)+CI*(-A(28)-A(30)-A(36)-A(42)-A(44) &
       -A(50))*f(24)
  M2(2) = -(A(10)*f(3))/2._/**/REALKIND+(A(78)+A(79)+A(80)+A(81)+A(88)+A(89))*f(3)+CI*(-A(90)-A(91)-A(92)-A(93))*f(4)+(-A(16) &
       -A(23)-A(63)-A(67)-A(76)-A(77))*f(5)+CI*(A(65)+A(74))*f(6)+(-A(14)-A(62)-A(72))*f(7)+CI*(A(66)+A(73))*f(8)+CI*(A(24) &
       +A(25))*f(9)+CI*(A(11)+A(12)-A(13))*f(10)+(A(15)+A(22))*f(11)+(-A(33)-A(35)-A(41)-A(53)-A(55)-A(61))*f(19)+CI*(A(29)+A(31) &
       +A(37)+A(43)+A(45)+A(51))*f(20)+(-A(32)-A(34)-A(40)-A(52)-A(54)-A(60))*f(23)+CI*(A(28)+A(30)+A(36)+A(42)+A(44) &
       +A(50))*f(24)
  M2(3) = ((A(9)+A(10))*f(3))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pphzjj_ddxhzgg_1_/**/REALKIND
