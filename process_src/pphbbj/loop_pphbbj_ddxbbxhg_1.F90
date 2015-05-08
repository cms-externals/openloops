
module ol_colourmatrix_pphbbj_ddxbbxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(92,4), K2(4,4), KL(4,4)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  36,  12,  12,   0]
  K1( 2,:) = [  12,  36,   0,  12]
  K1( 3,:) = [  12,   0,  36,  12]
  K1( 4,:) = [   0,  12,  12,  36]
  K1( 5,:) = [  48,  16,  16,   0]
  K1( 6,:) = [  16,  48,   0,  16]
  K1( 7,:) = [  16,   0,  48,  16]
  K1( 8,:) = [   0,  16,  16,  48]
  K1( 9,:) = [   6,   2,   2,   0]
  K1(10,:) = [   2,   0,  -6, -16]
  K1(11,:) = [   2,  -6,   0, -16]
  K1(12,:) = [   0, -16, -16, -48]
  K1(13,:) = [  48,  16,  16,   0]
  K1(14,:) = [  16,  48,   0,  16]
  K1(15,:) = [  16,   0,  48,  16]
  K1(16,:) = [   0,  16,  16,  48]
  K1(17,:) = [   0,  16,  -2,   6]
  K1(18,:) = [  16,   0,   6,  -2]
  K1(19,:) = [  -2,   6,   0,  16]
  K1(20,:) = [   6,  -2,  16,   0]
  K1(21,:) = [   0,   2, -16,  -6]
  K1(22,:) = [   2,   6,   0,   2]
  K1(23,:) = [ -16,   0, -48, -16]
  K1(24,:) = [  -6,   2, -16,   0]
  K1(25,:) = [  48,  16,  16,   0]
  K1(26,:) = [  16,  48,   0,  16]
  K1(27,:) = [  16,   0,  48,  16]
  K1(28,:) = [   0,  16,  16,  48]
  K1(29,:) = [   0, -16,   2,  -6]
  K1(30,:) = [ -16, -48,   0, -16]
  K1(31,:) = [   2,   0,   6,   2]
  K1(32,:) = [  -6, -16,   2,   0]
  K1(33,:) = [   0,  -2,  16,   6]
  K1(34,:) = [  -2,   0,   6,  16]
  K1(35,:) = [  16,   6,   0,  -2]
  K1(36,:) = [   6,  16,  -2,   0]
  K1(37,:) = [ -48, -16, -16,   0]
  K1(38,:) = [ -16,   0,  -6,   2]
  K1(39,:) = [ -16,  -6,   0,   2]
  K1(40,:) = [   0,   2,   2,   6]
  K1(41,:) = [  48,  16,  16,   0]
  K1(42,:) = [  16,  48,   0,  16]
  K1(43,:) = [  16,   0,  48,  16]
  K1(44,:) = [   0,  16,  16,  48]
  K1(45,:) = [   0,   0,   0,   0]
  K1(46,:) = [   0,   0,   0,   0]
  K1(47,:) = [   0,   0,   0,   0]
  K1(48,:) = [   0,   0,   0,   0]
  K1(49,:) = [   0,   0,   0,   0]
  K1(50,:) = [   0,   0,   0,   0]
  K1(51,:) = [   0,   0,   0,   0]
  K1(52,:) = [   0,   0,   0,   0]
  K1(53,:) = [   0,   0,   0,   0]
  K1(54,:) = [   0,   0,   0,   0]
  K1(55,:) = [   0,   0,   0,   0]
  K1(56,:) = [   0,   0,   0,   0]
  K1(57,:) = [   0,   0,   0,   0]
  K1(58,:) = [   0,   0,   0,   0]
  K1(59,:) = [   0,   0,   0,   0]
  K1(60,:) = [   0,   0,   0,   0]
  K1(61,:) = [   0,   0,   0,   0]
  K1(62,:) = [   0,   0,   0,   0]
  K1(63,:) = [   0,   0,   0,   0]
  K1(64,:) = [   0,   0,   0,   0]
  K1(65,:) = [ -54, -18, -18,   0]
  K1(66,:) = [ -18,   0,   0,  18]
  K1(67,:) = [ -18,   0, -54, -18]
  K1(68,:) = [   0,  18, -18,   0]
  K1(69,:) = [ -54, -18, -18,   0]
  K1(70,:) = [ -18, -54,   0, -18]
  K1(71,:) = [ -18,   0,   0,  18]
  K1(72,:) = [   0, -18,  18,   0]
  K1(73,:) = [   0, -18,  18,   0]
  K1(74,:) = [ -18, -54,   0, -18]
  K1(75,:) = [  18,   0,   0, -18]
  K1(76,:) = [   0, -18, -18, -54]
  K1(77,:) = [   0,  18, -18,   0]
  K1(78,:) = [  18,   0,   0, -18]
  K1(79,:) = [ -18,   0, -54, -18]
  K1(80,:) = [   0, -18, -18, -54]
  K1(81,:) = [   0,   0,   0,   0]
  K1(82,:) = [   0,   0,   0,   0]
  K1(83,:) = [   0,   0,   0,   0]
  K1(84,:) = [   0,   0,   0,   0]
  K1(85,:) = [ 108,  36,  36,   0]
  K1(86,:) = [  36, 108,   0,  36]
  K1(87,:) = [  36,   0, 108,  36]
  K1(88,:) = [   0,  36,  36, 108]
  K1(89,:) = [   0,   0,   0,   0]
  K1(90,:) = [   0,   0,   0,   0]
  K1(91,:) = [   0,   0,   0,   0]
  K1(92,:) = [   0,   0,   0,   0]
  K1 = (1._/**/REALKIND / 3) * K1

  K2(1,:) = [ 12,  4,  4,  0]
  K2(2,:) = [  4, 12,  0,  4]
  K2(3,:) = [  4,  0, 12,  4]
  K2(4,:) = [  0,  4,  4, 12]

  KL(1,:) = [ 12,  4,  4,  0]
  KL(2,:) = [  4, 12,  0,  4]
  KL(3,:) = [  4,  0, 12,  4]
  KL(4,:) = [  0,  4,  4, 12]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphbbj_ddxbbxhg_1_/**/REALKIND



module ol_forced_parameters_pphbbj_ddxbbxhg_1_/**/REALKIND
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


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphbbj_ddxbbxhg_1_/**/REALKIND

module ol_loop_pphbbj_ddxbbxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(25), c(29)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:169)
  ! denominators
  complex(REALKIND), save :: den(185)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(4,32)
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
    f( 1) = (CI*countertermnorm*ctHGG*eQED*gQCD**5*MB*YB)/MQ2sum
    f( 2) = (countertermnorm*ctHGG*eQED*gQCD**5*MB*YB)/MQ2sum
    f( 3) = (CI*eQED*gQCD**3*YB)/(2._/**/REALKIND*MW*sw)
    f( 4) = (eQED*gQCD**3*YB)/(MW*sw*2._/**/REALKIND)
    f( 5) = (CI*countertermnorm*eQED*gQCD**5*YB)/(2._/**/REALKIND*MW*sw)
    f( 6) = (countertermnorm*eQED*gQCD**5*YB)/(MW*sw*2._/**/REALKIND)
    f( 7) = (CI*countertermnorm*ctGbb*eQED*gQCD**5*YB)/(2._/**/REALKIND*MW*sw)
    f( 8) = (countertermnorm*ctGbb*eQED*gQCD**5*YB)/(MW*sw*2._/**/REALKIND)
    f( 9) = (CI*countertermnorm*ctGqq*eQED*gQCD**5*YB)/(2._/**/REALKIND*MW*sw)
    f(10) = (countertermnorm*ctGqq*eQED*gQCD**5*YB)/(MW*sw*2._/**/REALKIND)
    f(11) = (CI*countertermnorm*ctSbb*eQED*gQCD**5*YB)/(2._/**/REALKIND*MW*sw)
    f(12) = (countertermnorm*ctSbb*eQED*gQCD**5*YB)/(MW*sw*2._/**/REALKIND)
    f(13) = (countertermnorm*ctVVV*eQED*gQCD**5*YB)/(MW*sw*2._/**/REALKIND)
    f(14) = (CI*eQED*gQCD**5*integralnorm*SwB*YB)/(4._/**/REALKIND*MW*sw)
    f(15) = (CI*eQED*gQCD**5*integralnorm*SwB*YB)/(2._/**/REALKIND*MW*sw)
    f(16) = (eQED*gQCD**5*integralnorm*SwB*YB)/(MW*sw*4._/**/REALKIND)
    f(17) = (eQED*gQCD**5*integralnorm*SwB*YB)/(MW*sw*2._/**/REALKIND)
    f(18) = (CI*eQED*gQCD**5*integralnorm*SwF*YB)/(2._/**/REALKIND*MW*sw)
    f(19) = (CI*eQED*gQCD**5*integralnorm*SwF*YB)/(MW*sw)
    f(20) = (eQED*gQCD**5*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(21) = (eQED*gQCD**5*integralnorm*SwF*YB)/(MW*sw)
    f(22) = (CI*countertermnorm*ctHGG*eQED*gQCD**5*MT*YT)/MQ2sum
    f(23) = (countertermnorm*ctHGG*eQED*gQCD**5*MT*YT)/MQ2sum
    f(24) = (CI*eQED*gQCD**5*integralnorm*SwF*YT)/(2._/**/REALKIND*MW*sw)
    f(25) = (eQED*gQCD**5*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

  c = [ 27*CI*f(14), 54*CI*f(14), 3*CI*f(15), 9*CI*f(15), 24*CI*f(15), 27*CI*f(15), 54*CI*f(15), 18*f(16), 54*f(16), f(17) &
    , 3*f(17), 6*f(17), 8*f(17), 9*f(17), 10*f(17), 18*f(17), 21*f(17), 24*f(17), 27*f(17), 54*f(17), 9*CI*f(18), 9*CI*f(19) &
    , 3*f(20), 9*f(20), 3*f(21), 9*f(21), 9*CI*f(24), 3*f(25), 9*f(25) ]
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
  complex(REALKIND), intent(out) :: M1(4), M2(4)
  complex(REALKIND) :: A(118)
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
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,7))
  call vert_VQ_A(wf(:,-5),wf(:,4),wf(:,8))
  call prop_A_Q(wf(:,7),Q(:,11),MB,1_intkind1,wf(:,9))
  call vert_QA_V(wf(:,4),wf(:,-3),wf(:,10))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,11))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,12))
  call vert_SA_Q(gH,wf(:,-4),wf(:,-3),wf(:,13))
  call prop_Q_A(wf(:,12),Q(:,36),MB,1_intkind1,wf(:,14))
  call prop_A_Q(wf(:,13),Q(:,24),MB,1_intkind1,wf(:,15))
  call vert_VQ_A(wf(:,1),wf(:,14),wf(:,16))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,17))
  call vert_AV_Q(wf(:,15),wf(:,-5),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,7),MB,1_intkind1,wf(:,19))
  call vert_QA_V(wf(:,-2),wf(:,15),wf(:,20))
  call vert_QS_A(gH,wf(:,14),wf(:,-4),wf(:,21))
  call vert_SA_Q(gH,wf(:,-4),wf(:,5),wf(:,22))
  call vert_VQ_A(wf(:,-5),wf(:,0),wf(:,23))
  call prop_Q_A(wf(:,23),Q(:,33),ZERO,0_intkind1,wf(:,24))
  call vert_QA_V(wf(:,24),wf(:,-1),wf(:,25))
  call vert_AV_Q(wf(:,-1),wf(:,-5),wf(:,26))
  call prop_A_Q(wf(:,26),Q(:,34),ZERO,0_intkind1,wf(:,27))
  call vert_QA_V(wf(:,0),wf(:,27),wf(:,28))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,29))
  call counter_SG_G(wf(:,-4),wf(:,29),wf(:,30))
  call vert_UV_W(wf(:,29),Q(:,12),wf(:,-5),Q(:,32),wf(:,31))
  call counter_SG_G(wf(:,-4),wf(:,1),wf(:,32))
  call counter_SG_G(wf(:,-4),wf(:,-5),wf(:,33))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,29),Q(:,12),wf(:,34))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,35))
  call counter_VQ_A(wf(:,-5),wf(:,4),wf(:,36))
  call counter_UV_W(wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,37))
  call counter_VQ_A(wf(:,1),wf(:,14),wf(:,38))
  call counter_AV_Q(wf(:,15),wf(:,-5),wf(:,39))
  call counter_QS_A(gH,wf(:,14),wf(:,-4),wf(:,40))
  call vert_QA_V(wf(:,14),wf(:,-3),wf(:,41))
  call counter_SA_Q(gH,wf(:,-4),wf(:,5),wf(:,42))
  call vert_QA_V(wf(:,-2),wf(:,5),wf(:,43))
  call vert_AV_Q(wf(:,-3),wf(:,33),wf(:,44))
  call vert_VQ_A(wf(:,33),wf(:,-2),wf(:,45))
  call prop_Q_A(wf(:,45),Q(:,52),MB,1_intkind1,wf(:,46))
  call counter_QA_V(wf(:,4),wf(:,-3),wf(:,47))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,48))
  call prop_Q_A(wf(:,8),Q(:,52),MB,1_intkind1,wf(:,49))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,50))
  call prop_A_Q(wf(:,50),Q(:,40),MB,1_intkind1,wf(:,51))
  call prop_Q_A(wf(:,21),Q(:,52),MB,1_intkind1,wf(:,52))
  call vert_SA_Q(gH,wf(:,-4),wf(:,51),wf(:,53))
  call counter_SA_Q(gH,wf(:,-4),wf(:,-3),wf(:,54))
  call prop_A_Q(wf(:,54),Q(:,24),MB,1_intkind1,wf(:,55))
  call vert_AV_Q(wf(:,55),wf(:,-5),wf(:,56))
  call vert_QA_V(wf(:,-2),wf(:,55),wf(:,57))
  call counter_QA_V(wf(:,-2),wf(:,15),wf(:,58))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,59))
  call prop_A_Q(wf(:,18),Q(:,56),MB,1_intkind1,wf(:,60))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,61))
  call prop_Q_A(wf(:,61),Q(:,36),MB,1_intkind1,wf(:,62))
  call vert_VQ_A(wf(:,1),wf(:,62),wf(:,63))
  call prop_A_Q(wf(:,22),Q(:,56),MB,1_intkind1,wf(:,64))
  call vert_QS_A(gH,wf(:,62),wf(:,-4),wf(:,65))
  call counter_QS_A(gH,wf(:,-2),wf(:,-4),wf(:,66))
  call prop_Q_A(wf(:,66),Q(:,20),MB,1_intkind1,wf(:,67))
  call vert_VQ_A(wf(:,1),wf(:,67),wf(:,68))
  call vert_VQ_A(wf(:,-5),wf(:,67),wf(:,69))
  call vert_QA_V(wf(:,67),wf(:,-3),wf(:,70))
  call vert_VQ_A(wf(:,29),wf(:,0),wf(:,71))
  call vert_AV_Q(wf(:,-1),wf(:,33),wf(:,72))
  call prop_Q_A(wf(:,71),Q(:,13),ZERO,0_intkind1,wf(:,73))
  call vert_VQ_A(wf(:,33),wf(:,0),wf(:,74))
  call vert_AV_Q(wf(:,-1),wf(:,29),wf(:,75))
  call prop_Q_A(wf(:,74),Q(:,49),ZERO,0_intkind1,wf(:,76))
  call counter_QA_V(wf(:,24),wf(:,-1),wf(:,77))
  call counter_AV_Q(wf(:,-1),wf(:,-5),wf(:,78))
  call prop_A_Q(wf(:,78),Q(:,34),ZERO,0_intkind1,wf(:,79))
  call vert_QA_V(wf(:,0),wf(:,79),wf(:,80))
  call counter_QA_V(wf(:,0),wf(:,27),wf(:,81))
  call counter_VQ_A(wf(:,-5),wf(:,0),wf(:,82))
  call prop_Q_A(wf(:,82),Q(:,33),ZERO,0_intkind1,wf(:,83))
  call vert_QA_V(wf(:,83),wf(:,-1),wf(:,84))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,85))
  call vert_VQ_A(wf(:,85),wf(:,4),wf(:,86))
  call vert_AV_Q(wf(:,-3),wf(:,85),wf(:,87))
  call prop_A_Q(wf(:,87),Q(:,11),MB,1_intkind1,wf(:,88))
  call vert_UV_W(wf(:,85),Q(:,3),wf(:,-5),Q(:,32),wf(:,89))
  call vert_VQ_A(wf(:,85),wf(:,14),wf(:,90))
  call vert_VQ_A(wf(:,85),wf(:,-2),wf(:,91))
  call prop_Q_A(wf(:,91),Q(:,7),MB,1_intkind1,wf(:,92))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,93))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,94))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,95))
  call counter_Q_A(ctbb,wf(:,4),Q(:,20),wf(:,96))
  call prop_A_Q(wf(:,95),Q(:,43),MB,1_intkind1,wf(:,97))
  call counter_A_Q(ctbb,wf(:,5),Q(:,40),wf(:,98))
  call prop_Q_A(wf(:,6),Q(:,23),MB,1_intkind1,wf(:,99))
  call vert_AV_Q(wf(:,-3),wf(:,94),wf(:,100))
  call vert_UV_W(wf(:,94),Q(:,3),wf(:,-5),Q(:,32),wf(:,101))
  call prop_Q_A(wf(:,96),Q(:,20),MB,1_intkind1,wf(:,102))
  call vert_QA_V(wf(:,102),wf(:,-3),wf(:,103))
  call vert_VQ_A(wf(:,-5),wf(:,102),wf(:,104))
  call counter_A_Q(ctbb,wf(:,9),Q(:,11),wf(:,105))
  call counter_V_V(ctGG,wf(:,10),Q(:,28),wf(:,106))
  call vert_QA_V(wf(:,14),wf(:,15),wf(:,107))
  call vert_AV_Q(wf(:,15),wf(:,1),wf(:,108))
  call counter_Q_A(ctbb,wf(:,14),Q(:,36),wf(:,109))
  call prop_A_Q(wf(:,108),Q(:,27),MB,1_intkind1,wf(:,110))
  call counter_A_Q(ctbb,wf(:,15),Q(:,24),wf(:,111))
  call prop_Q_A(wf(:,16),Q(:,39),MB,1_intkind1,wf(:,112))
  call vert_VQ_A(wf(:,94),wf(:,-2),wf(:,113))
  call prop_A_Q(wf(:,111),Q(:,24),MB,1_intkind1,wf(:,114))
  call vert_QA_V(wf(:,-2),wf(:,114),wf(:,115))
  call counter_Q_A(ctbb,wf(:,19),Q(:,7),wf(:,116))
  call counter_V_V(ctGG,wf(:,20),Q(:,28),wf(:,117))
  call vert_AV_Q(wf(:,114),wf(:,-5),wf(:,118))
  call prop_Q_A(wf(:,109),Q(:,36),MB,1_intkind1,wf(:,119))
  call vert_QS_A(gH,wf(:,119),wf(:,-4),wf(:,120))
  call prop_A_Q(wf(:,98),Q(:,40),MB,1_intkind1,wf(:,121))
  call vert_SA_Q(gH,wf(:,-4),wf(:,121),wf(:,122))
  call counter_Q_A(ctqq,wf(:,24),Q(:,33),wf(:,123))
  call prop_Q_A(wf(:,123),Q(:,33),ZERO,0_intkind1,wf(:,124))
  call vert_QA_V(wf(:,124),wf(:,-1),wf(:,125))
  call counter_V_V(ctGG,wf(:,25),Q(:,35),wf(:,126))
  call counter_A_Q(ctqq,wf(:,27),Q(:,34),wf(:,127))
  call prop_A_Q(wf(:,127),Q(:,34),ZERO,0_intkind1,wf(:,128))
  call vert_QA_V(wf(:,0),wf(:,128),wf(:,129))
  call counter_V_V(ctGG,wf(:,28),Q(:,35),wf(:,130))
  call vert_QA_V(wf(:,19),wf(:,-3),wf(:,131))
  call vert_QS_A(gH,wf(:,19),wf(:,-4),wf(:,132))
  call prop_Q_A(wf(:,132),Q(:,23),MB,1_intkind1,wf(:,133))
  call vert_VQ_A(wf(:,-5),wf(:,19),wf(:,134))
  call prop_Q_A(wf(:,134),Q(:,39),MB,1_intkind1,wf(:,135))
  call vert_QA_V(wf(:,-2),wf(:,9),wf(:,136))
  call vert_SA_Q(gH,wf(:,-4),wf(:,9),wf(:,137))
  call prop_A_Q(wf(:,137),Q(:,27),MB,1_intkind1,wf(:,138))
  call vert_AV_Q(wf(:,9),wf(:,-5),wf(:,139))
  call prop_A_Q(wf(:,139),Q(:,43),MB,1_intkind1,wf(:,140))
  call vert_VQ_A(wf(:,11),wf(:,-2),wf(:,141))
  call prop_Q_A(wf(:,141),Q(:,39),MB,1_intkind1,wf(:,142))
  call vert_AV_Q(wf(:,-3),wf(:,11),wf(:,143))
  call prop_A_Q(wf(:,143),Q(:,43),MB,1_intkind1,wf(:,144))
  call vert_QA_V(wf(:,73),wf(:,-1),wf(:,145))
  call prop_A_Q(wf(:,75),Q(:,14),ZERO,0_intkind1,wf(:,146))
  call vert_QA_V(wf(:,0),wf(:,146),wf(:,147))
  call vert_VQ_A(wf(:,10),wf(:,0),wf(:,148))
  call prop_Q_A(wf(:,148),Q(:,29),ZERO,0_intkind1,wf(:,149))
  call vert_AV_Q(wf(:,-1),wf(:,10),wf(:,150))
  call prop_A_Q(wf(:,150),Q(:,30),ZERO,0_intkind1,wf(:,151))
  call vert_UV_W(wf(:,10),Q(:,28),wf(:,-5),Q(:,32),wf(:,152))
  call vert_QA_V(wf(:,49),wf(:,-3),wf(:,153))
  call vert_VQ_A(wf(:,20),wf(:,0),wf(:,154))
  call prop_Q_A(wf(:,154),Q(:,29),ZERO,0_intkind1,wf(:,155))
  call vert_AV_Q(wf(:,-1),wf(:,20),wf(:,156))
  call prop_A_Q(wf(:,156),Q(:,30),ZERO,0_intkind1,wf(:,157))
  call vert_UV_W(wf(:,20),Q(:,28),wf(:,-5),Q(:,32),wf(:,158))
  call vert_QA_V(wf(:,-2),wf(:,60),wf(:,159))
  call vert_VQ_A(wf(:,25),wf(:,-2),wf(:,160))
  call prop_Q_A(wf(:,160),Q(:,39),MB,1_intkind1,wf(:,161))
  call vert_AV_Q(wf(:,-3),wf(:,25),wf(:,162))
  call prop_A_Q(wf(:,162),Q(:,43),MB,1_intkind1,wf(:,163))
  call vert_VQ_A(wf(:,28),wf(:,-2),wf(:,164))
  call prop_Q_A(wf(:,164),Q(:,39),MB,1_intkind1,wf(:,165))
  call vert_AV_Q(wf(:,-3),wf(:,28),wf(:,166))
  call prop_A_Q(wf(:,166),Q(:,43),MB,1_intkind1,wf(:,167))
  call vert_QA_V(wf(:,52),wf(:,-3),wf(:,168))
  call vert_QA_V(wf(:,-2),wf(:,64),wf(:,169))

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
  den(9) = 1 / (Q(5,28))
  den(12) = 1 / (Q(5,36) - MB2)
  den(13) = 1 / (Q(5,24) - MB2)
  den(16) = 1 / (Q(5,7) - MB2)
  den(23) = 1 / (Q(5,33))
  den(24) = 1 / (Q(5,35))
  den(27) = 1 / (Q(5,34))
  den(32) = 1 / (Q(5,12))
  den(35) = 1 / (Q(5,44))
  den(38) = 1 / (Q(5,48))
  den(46) = 1 / (Q(5,52) - MB2)
  den(55) = 1 / (Q(5,56) - MB2)
  den(62) = 1 / (Q(5,13))
  den(65) = 1 / (Q(5,49))
  den(73) = 1 / (Q(5,60))
  den(77) = 1 / (Q(5,43) - MB2)
  den(80) = 1 / (Q(5,23) - MB2)
  den(95) = 1 / (Q(5,27) - MB2)
  den(98) = 1 / (Q(5,39) - MB2)
  den(130) = 1 / (Q(5,15))
  den(141) = 1 / (Q(5,14))
  den(144) = 1 / (Q(5,29))
  den(146) = 1 / (Q(5,30))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(14) = den(1)*den(12)
  den(15) = den(13)*den(14)
  den(17) = den(1)*den(16)
  den(18) = den(13)*den(17)
  den(19) = den(9)*den(13)
  den(20) = den(1)*den(19)
  den(21) = den(7)*den(12)
  den(22) = den(3)*den(17)
  den(25) = den(23)*den(24)
  den(26) = den(2)*den(25)
  den(28) = den(24)*den(27)
  den(29) = den(2)*den(28)
  den(30) = den(13)*den(25)
  den(31) = den(13)*den(28)
  den(33) = den(1)*den(24)
  den(34) = den(32)*den(33)
  den(36) = den(32)*den(35)
  den(37) = den(1)*den(36)
  den(39) = den(1)*den(32)
  den(40) = den(38)*den(39)
  den(41) = den(12)*den(35)
  den(42) = den(1)*den(41)
  den(43) = den(3)*den(35)
  den(44) = den(1)*den(43)
  den(45) = den(17)*den(38)
  den(47) = den(38)*den(46)
  den(48) = den(1)*den(47)
  den(49) = den(2)*den(33)
  den(50) = den(2)*den(46)
  den(51) = den(1)*den(50)
  den(52) = den(12)*den(46)
  den(53) = den(1)*den(52)
  den(54) = den(13)*den(33)
  den(56) = den(13)*den(55)
  den(57) = den(1)*den(56)
  den(58) = den(3)*den(55)
  den(59) = den(1)*den(58)
  den(60) = den(25)*den(32)
  den(61) = den(28)*den(32)
  den(63) = den(32)*den(62)
  den(64) = den(38)*den(63)
  den(66) = den(38)*den(65)
  den(67) = den(32)*den(66)
  den(68) = den(10)*den(23)
  den(69) = den(19)*den(23)
  den(70) = den(10)*den(27)
  den(71) = den(19)*den(27)
  den(72) = den(2)*den(3)
  den(74) = den(72)*den(73)
  den(75) = den(1)*den(74)
  den(76) = den(1)*den(3)
  den(78) = den(76)*den(77)
  den(79) = den(2)*den(78)
  den(81) = den(4)*den(80)
  den(82) = den(3)*den(81)
  den(83) = den(1)**2
  den(84) = den(50)*den(83)
  den(85) = den(10)*den(83)
  den(86) = den(2)**2
  den(87) = den(33)*den(86)
  den(88) = den(7)*den(86)
  den(89) = den(7)*den(50)
  den(90) = den(10)*den(33)
  den(91) = den(12)*den(13)
  den(92) = den(73)*den(91)
  den(93) = den(1)*den(92)
  den(94) = den(1)*den(13)
  den(96) = den(94)*den(95)
  den(97) = den(12)*den(96)
  den(99) = den(14)*den(98)
  den(100) = den(13)*den(99)
  den(101) = den(56)*den(83)
  den(102) = den(19)*den(83)
  den(103) = den(13)**2
  den(104) = den(33)*den(103)
  den(105) = den(17)*den(56)
  den(106) = den(19)*den(33)
  den(107) = den(17)*den(103)
  den(108) = den(52)*den(83)
  den(109) = den(12)**2
  den(110) = den(7)*den(109)
  den(111) = den(7)*den(52)
  den(112) = den(58)*den(83)
  den(113) = den(17)*den(58)
  den(114) = den(3)**2
  den(115) = den(17)*den(114)
  den(116) = den(23)**2
  den(117) = den(10)*den(116)
  den(118) = den(10)*den(25)
  den(119) = den(25)*den(86)
  den(120) = den(27)**2
  den(121) = den(10)*den(120)
  den(122) = den(10)*den(28)
  den(123) = den(28)*den(86)
  den(124) = den(19)*den(116)
  den(125) = den(19)*den(25)
  den(126) = den(25)*den(103)
  den(127) = den(19)*den(120)
  den(128) = den(19)*den(28)
  den(129) = den(28)*den(103)
  den(131) = den(39)*den(130)
  den(132) = den(17)*den(130)
  den(133) = den(17)*den(80)
  den(134) = den(17)*den(98)
  den(135) = den(7)*den(130)
  den(136) = den(7)*den(95)
  den(137) = den(7)*den(77)
  den(138) = den(33)*den(98)
  den(139) = den(33)*den(77)
  den(140) = den(63)*den(130)
  den(142) = den(32)*den(141)
  den(143) = den(130)*den(142)
  den(145) = den(10)*den(144)
  den(147) = den(10)*den(146)
  den(148) = den(10)*den(73)
  den(149) = den(50)*den(73)
  den(150) = den(19)*den(144)
  den(151) = den(19)*den(146)
  den(152) = den(19)*den(73)
  den(153) = den(56)*den(73)
  den(154) = den(25)*den(98)
  den(155) = den(25)*den(77)
  den(156) = den(28)*den(98)
  den(157) = den(28)*den(77)
  den(158) = den(52)*den(73)
  den(159) = den(58)*den(73)
  den(160) = den(1)*den(2)*den(3)
  den(161) = den(1)*den(12)*den(13)
  den(162) = den(2)*den(23)
  den(163) = den(2)*den(27)
  den(164) = den(13)*den(23)
  den(165) = den(13)*den(27)
  den(166) = den(2)*den(137)
  den(167) = den(2)*den(139)
  den(168) = den(1)*den(148)
  den(169) = den(1)*den(149)
  den(170) = den(13)*den(134)
  den(171) = den(13)*den(138)
  den(172) = den(1)*den(152)
  den(173) = den(1)*den(153)
  den(174) = den(12)*den(136)
  den(175) = den(1)*den(158)
  den(176) = den(3)*den(133)
  den(177) = den(1)*den(159)
  den(178) = den(2)*den(155)
  den(179) = den(23)*den(147)
  den(180) = den(2)*den(157)
  den(181) = den(27)*den(145)
  den(182) = den(13)*den(154)
  den(183) = den(23)*den(151)
  den(184) = den(13)*den(156)
  den(185) = den(27)*den(150)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(118)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_VV(wf(:,10),wf(:,11)) * den(11)
  A(4) = cont_QA(wf(:,15),wf(:,16)) * den(15)
  A(5) = cont_QA(wf(:,18),wf(:,19)) * den(18)
  A(6) = cont_VV(wf(:,11),wf(:,20)) * den(20)
  A(7) = cont_QA(wf(:,9),wf(:,21)) * den(21)
  A(8) = cont_QA(wf(:,19),wf(:,22)) * den(22)
  A(9) = cont_VV(wf(:,10),wf(:,25)) * den(26)
  A(10) = cont_VV(wf(:,10),wf(:,28)) * den(29)
  A(11) = cont_VV(wf(:,20),wf(:,25)) * den(30)
  A(12) = cont_VV(wf(:,20),wf(:,28)) * den(31)

  A(13) = cont_VV(wf(:,11),wf(:,30)) * den(34)
  A(14) = cont_VV(wf(:,11),wf(:,30)) * den(34)
  A(15) = cont_VV(wf(:,31),wf(:,32)) * den(37)
  A(16) = cont_VV(wf(:,31),wf(:,32)) * den(37)
  A(17) = cont_VV(wf(:,33),wf(:,34)) * den(40)
  A(18) = cont_VV(wf(:,33),wf(:,34)) * den(40)
  A(19) = cont_QA(wf(:,5),wf(:,35)) * den(5)
  A(20) = cont_QA(wf(:,9),wf(:,36)) * den(8)
  A(21) = cont_VV(wf(:,10),wf(:,37)) * den(11)
  A(22) = cont_QA(wf(:,15),wf(:,38)) * den(15)
  A(23) = cont_QA(wf(:,19),wf(:,39)) * den(18)
  A(24) = cont_VV(wf(:,20),wf(:,37)) * den(20)
  A(25) = cont_QA(wf(:,9),wf(:,40)) * den(21)
  A(26) = cont_VV(wf(:,32),wf(:,41)) * den(42)
  A(27) = cont_VV(wf(:,32),wf(:,41)) * den(42)
  A(28) = cont_QA(wf(:,19),wf(:,42)) * den(22)
  A(29) = cont_VV(wf(:,32),wf(:,43)) * den(44)
  A(30) = cont_VV(wf(:,32),wf(:,43)) * den(44)
  A(31) = cont_QA(wf(:,19),wf(:,44)) * den(45)
  A(32) = cont_QA(wf(:,19),wf(:,44)) * den(45)
  A(33) = cont_QA(wf(:,7),wf(:,46)) * den(48)
  A(34) = cont_QA(wf(:,7),wf(:,46)) * den(48)
  A(35) = cont_VV(wf(:,11),wf(:,47)) * den(49)
  A(36) = cont_QA(wf(:,48),wf(:,49)) * den(51)
  A(37) = cont_QA(wf(:,6),wf(:,51)) * den(5)
  A(38) = cont_QA(wf(:,48),wf(:,52)) * den(53)
  A(39) = cont_QA(wf(:,19),wf(:,53)) * den(22)
  A(40) = cont_QA(wf(:,16),wf(:,55)) * den(15)
  A(41) = cont_QA(wf(:,19),wf(:,56)) * den(18)
  A(42) = cont_VV(wf(:,11),wf(:,57)) * den(20)
  A(43) = cont_VV(wf(:,11),wf(:,58)) * den(54)
  A(44) = cont_QA(wf(:,59),wf(:,60)) * den(57)
  A(45) = cont_QA(wf(:,15),wf(:,63)) * den(15)
  A(46) = cont_QA(wf(:,59),wf(:,64)) * den(59)
  A(47) = cont_QA(wf(:,9),wf(:,65)) * den(21)
  A(48) = cont_QA(wf(:,5),wf(:,68)) * den(5)
  A(49) = cont_QA(wf(:,9),wf(:,69)) * den(8)
  A(50) = cont_VV(wf(:,11),wf(:,70)) * den(11)
  A(51) = cont_VV(wf(:,25),wf(:,30)) * den(60)
  A(52) = cont_VV(wf(:,25),wf(:,30)) * den(60)
  A(53) = cont_VV(wf(:,28),wf(:,30)) * den(61)
  A(54) = cont_VV(wf(:,28),wf(:,30)) * den(61)
  A(55) = cont_QA(wf(:,72),wf(:,73)) * den(64)
  A(56) = cont_QA(wf(:,72),wf(:,73)) * den(64)
  A(57) = cont_QA(wf(:,75),wf(:,76)) * den(67)
  A(58) = cont_QA(wf(:,75),wf(:,76)) * den(67)
  A(59) = cont_VV(wf(:,25),wf(:,47)) * den(26)
  A(60) = cont_VV(wf(:,28),wf(:,47)) * den(29)
  A(61) = cont_VV(wf(:,25),wf(:,57)) * den(30)
  A(62) = cont_VV(wf(:,28),wf(:,57)) * den(31)
  A(63) = cont_VV(wf(:,25),wf(:,58)) * den(30)
  A(64) = cont_VV(wf(:,28),wf(:,58)) * den(31)
  A(65) = cont_VV(wf(:,25),wf(:,70)) * den(26)
  A(66) = cont_VV(wf(:,28),wf(:,70)) * den(29)
  A(67) = cont_VV(wf(:,10),wf(:,77)) * den(68)
  A(68) = cont_VV(wf(:,10),wf(:,80)) * den(29)
  A(69) = cont_VV(wf(:,20),wf(:,77)) * den(69)
  A(70) = cont_VV(wf(:,20),wf(:,80)) * den(31)
  A(71) = cont_VV(wf(:,10),wf(:,81)) * den(70)
  A(72) = cont_VV(wf(:,10),wf(:,84)) * den(26)
  A(73) = cont_VV(wf(:,20),wf(:,81)) * den(71)
  A(74) = cont_VV(wf(:,20),wf(:,84)) * den(30)
  A(75) = cont_QA(wf(:,5),wf(:,86)) * den(5)
  A(76) = cont_QA(wf(:,8),wf(:,88)) * den(8)
  A(77) = cont_VV(wf(:,10),wf(:,89)) * den(11)
  A(78) = cont_QA(wf(:,15),wf(:,90)) * den(15)
  A(79) = cont_QA(wf(:,18),wf(:,92)) * den(18)
  A(80) = cont_VV(wf(:,20),wf(:,89)) * den(20)
  A(81) = cont_QA(wf(:,21),wf(:,88)) * den(21)
  A(82) = cont_QA(wf(:,22),wf(:,92)) * den(22)
  A(83) = cont_VV(wf(:,93),wf(:,94)) * den(75)
  A(84) = cont_QA(wf(:,96),wf(:,97)) * den(79)
  A(85) = cont_QA(wf(:,98),wf(:,99)) * den(82)
  A(86) = cont_QA(wf(:,49),wf(:,100)) * den(84)
  A(87) = cont_VV(wf(:,10),wf(:,101)) * den(85)
  A(88) = cont_VV(wf(:,11),wf(:,103)) * den(87)
  A(89) = cont_QA(wf(:,9),wf(:,104)) * den(88)
  A(90) = cont_QA(wf(:,49),wf(:,105)) * den(89)
  A(91) = cont_VV(wf(:,11),wf(:,106)) * den(90)
  A(92) = cont_VV(wf(:,94),wf(:,107)) * den(93)
  A(93) = cont_QA(wf(:,109),wf(:,110)) * den(97)
  A(94) = cont_QA(wf(:,111),wf(:,112)) * den(100)
  A(95) = cont_QA(wf(:,60),wf(:,113)) * den(101)
  A(96) = cont_VV(wf(:,20),wf(:,101)) * den(102)
  A(97) = cont_VV(wf(:,11),wf(:,115)) * den(104)
  A(98) = cont_QA(wf(:,60),wf(:,116)) * den(105)
  A(99) = cont_VV(wf(:,11),wf(:,117)) * den(106)
  A(100) = cont_QA(wf(:,19),wf(:,118)) * den(107)
  A(101) = cont_QA(wf(:,52),wf(:,100)) * den(108)
  A(102) = cont_QA(wf(:,9),wf(:,120)) * den(110)
  A(103) = cont_QA(wf(:,52),wf(:,105)) * den(111)
  A(104) = cont_QA(wf(:,64),wf(:,113)) * den(112)
  A(105) = cont_QA(wf(:,64),wf(:,116)) * den(113)
  A(106) = cont_QA(wf(:,19),wf(:,122)) * den(115)
  A(107) = cont_VV(wf(:,10),wf(:,125)) * den(117)
  A(108) = cont_VV(wf(:,10),wf(:,126)) * den(118)
  A(109) = cont_VV(wf(:,25),wf(:,103)) * den(119)
  A(110) = cont_VV(wf(:,10),wf(:,129)) * den(121)
  A(111) = cont_VV(wf(:,10),wf(:,130)) * den(122)
  A(112) = cont_VV(wf(:,28),wf(:,103)) * den(123)
  A(113) = cont_VV(wf(:,20),wf(:,125)) * den(124)
  A(114) = cont_VV(wf(:,20),wf(:,126)) * den(125)
  A(115) = cont_VV(wf(:,25),wf(:,115)) * den(126)
  A(116) = cont_VV(wf(:,20),wf(:,129)) * den(127)
  A(117) = cont_VV(wf(:,20),wf(:,130)) * den(128)
  A(118) = cont_VV(wf(:,28),wf(:,115)) * den(129)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(118)
  complex(REALKIND), intent(out) :: M1(4), M2(4)

  M1(1) = ((-A(9)-A(10)-A(11)-A(12))*f(3))/6._/**/REALKIND
  M1(2) = ((A(2)+A(4)+A(7)+A(10)+A(12))*f(3))/2._/**/REALKIND+(CI*(A(3)+A(6))*f(4))/2._/**/REALKIND
  M1(3) = ((A(1)+A(5)+A(8)+A(9)+A(11))*f(3))/2._/**/REALKIND+(CI*(-A(3)-A(6))*f(4))/2._/**/REALKIND
  M1(4) = ((-A(1)-A(2)-A(4)-A(5)-A(7)-A(8))*f(3))/6._/**/REALKIND

  M2(1) = ((-A(52)-A(54)-A(56)-A(58))*f(1))/6._/**/REALKIND+((A(107)+A(108)+A(109)+A(110)+A(111)+A(112)+A(113)+A(114)+A(115) &
       +A(116)+A(117)+A(118))*f(5))/6._/**/REALKIND+((-A(59)-A(60)-A(63)-A(64))*f(7))/6._/**/REALKIND+((-A(67)-A(68)-A(69)-A(70) &
       -A(71)-A(72)-A(73)-A(74))*f(9))/6._/**/REALKIND+((-A(61)-A(62)-A(65)-A(66))*f(11))/6._/**/REALKIND+((-A(51)-A(53)-A(55) &
       -A(57))*f(22))/6._/**/REALKIND
  M2(2) = ((A(27)+A(34)+A(54)+A(56))*f(1))/2._/**/REALKIND+(CI*(A(14)-A(16)-A(18))*f(2))/2._/**/REALKIND+((-A(86)-A(89)-A(90) &
       -A(92)-A(93)-A(94)-A(101)-A(102)-A(103)-A(110)-A(111)-A(112)-A(116)-A(117)-A(118))*f(5))/2._/**/REALKIND+(CI*(-A(87)-A(88) &
       -A(91)-A(96)-A(97)-A(99))*f(6))/2._/**/REALKIND+((A(20)+A(22)+A(36)+A(38)+A(45)+A(47)+A(60)+A(64))*f(7))/2._/**/REALKIND &
       +(CI*(A(35)+A(43))*f(8))/2._/**/REALKIND+((A(68)+A(70)+A(71)+A(73)+A(76)+A(78)+A(81))*f(9))/2._/**/REALKIND+(CI*(A(77) &
       +A(80))*f(10))/2._/**/REALKIND+((A(25)+A(40)+A(49)+A(62)+A(66))*f(11))/2._/**/REALKIND+(CI*(A(42) &
       +A(50))*f(12))/2._/**/REALKIND+(CI*(A(21)+A(24))*f(13))/2._/**/REALKIND+((A(26)+A(33)+A(53)+A(55))*f(22))/2._/**/REALKIND &
       +(CI*(A(13)-A(15)-A(17))*f(23))/2._/**/REALKIND
  M2(3) = ((A(30)+A(32)+A(52)+A(58))*f(1))/2._/**/REALKIND+(CI*(-A(14)+A(16)+A(18))*f(2))/2._/**/REALKIND+((-A(83)-A(84)-A(85) &
       -A(95)-A(98)-A(100)-A(104)-A(105)-A(106)-A(107)-A(108)-A(109)-A(113)-A(114)-A(115))*f(5))/2._/**/REALKIND+(CI*(A(87)+A(88) &
       +A(91)+A(96)+A(97)+A(99))*f(6))/2._/**/REALKIND+((A(19)+A(23)+A(37)+A(39)+A(44)+A(46)+A(59)+A(63))*f(7))/2._/**/REALKIND &
       +(CI*(-A(35)-A(43))*f(8))/2._/**/REALKIND+((A(67)+A(69)+A(72)+A(74)+A(75)+A(79)+A(82))*f(9))/2._/**/REALKIND+(CI*(-A(77) &
       -A(80))*f(10))/2._/**/REALKIND+((A(28)+A(41)+A(48)+A(61)+A(65))*f(11))/2._/**/REALKIND+(CI*(-A(42) &
       -A(50))*f(12))/2._/**/REALKIND+(CI*(-A(21)-A(24))*f(13))/2._/**/REALKIND+((A(29)+A(31)+A(51)+A(57))*f(22))/2._/**/REALKIND &
       +(CI*(-A(13)+A(15)+A(17))*f(23))/2._/**/REALKIND
  M2(4) = ((-A(27)-A(30)-A(32)-A(34))*f(1))/6._/**/REALKIND+((A(83)+A(84)+A(85)+A(86)+A(89)+A(90)+A(92)+A(93)+A(94)+A(95)+A(98) &
       +A(100)+A(101)+A(102)+A(103)+A(104)+A(105)+A(106))*f(5))/6._/**/REALKIND+((-A(19)-A(20)-A(22)-A(23)-A(36)-A(37)-A(38)-A(39) &
       -A(44)-A(45)-A(46)-A(47))*f(7))/6._/**/REALKIND+((-A(75)-A(76)-A(78)-A(79)-A(81)-A(82))*f(9))/6._/**/REALKIND+((-A(25) &
       -A(28)-A(40)-A(41)-A(48)-A(49))*f(11))/6._/**/REALKIND+((-A(26)-A(29)-A(31)-A(33))*f(22))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pphbbj_ddxbbxhg_1_/**/REALKIND
