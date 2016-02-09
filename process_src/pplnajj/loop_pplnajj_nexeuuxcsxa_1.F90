
module ol_colourmatrix_pplnajj_nexeuuxcsxa_1_/**/REALKIND
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
end module ol_colourmatrix_pplnajj_nexeuuxcsxa_1_/**/REALKIND



module ol_forced_parameters_pplnajj_nexeuuxcsxa_1_/**/REALKIND
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
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pplnajj_nexeuuxcsxa_1_/**/REALKIND

module ol_loop_pplnajj_nexeuuxcsxa_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(31), c(47)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:176)
  ! denominators
  complex(REALKIND), save :: den(201)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,128)
  ! zero helicity identifier
  logical,           save :: zerohel(128) = .true., zerohel_ct(128) = .true.

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
    f( 1) = (CI*eQED**3*gQCD**2)/(6._/**/REALKIND*sw**2)
    f( 2) = (CI*eQED**3*gQCD**2)/(3._/**/REALKIND*sw**2)
    f( 3) = (CI*eQED**3*gQCD**2)/(2._/**/REALKIND*sw**2)
    f( 4) = (CI*countertermnorm*eQED**3*gQCD**4)/(6._/**/REALKIND*sw**2)
    f( 5) = (CI*countertermnorm*eQED**3*gQCD**4)/(3._/**/REALKIND*sw**2)
    f( 6) = (CI*countertermnorm*eQED**3*gQCD**4)/(2._/**/REALKIND*sw**2)
    f( 7) = (CI*countertermnorm*ctGcc*eQED**3*gQCD**4)/(6._/**/REALKIND*sw**2)
    f( 8) = (CI*countertermnorm*ctGcc*eQED**3*gQCD**4)/(3._/**/REALKIND*sw**2)
    f( 9) = (CI*countertermnorm*ctGcc*eQED**3*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(10) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4)/(6._/**/REALKIND*sw**2)
    f(11) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4)/(3._/**/REALKIND*sw**2)
    f(12) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(13) = (CI*countertermnorm*ctVcc*eQED**3*gQCD**4)/(3._/**/REALKIND*sw**2)
    f(14) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4)/(6._/**/REALKIND*sw**2)
    f(15) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4)/(3._/**/REALKIND*sw**2)
    f(16) = (CI*countertermnorm*ctVsc*eQED**3*gQCD**4)/(6._/**/REALKIND*sw**2)
    f(17) = (CI*countertermnorm*ctVsc*eQED**3*gQCD**4)/(3._/**/REALKIND*sw**2)
    f(18) = (CI*countertermnorm*ctVsc*eQED**3*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(19) = (CI*eQED**3*gQCD**4*integralnorm*SwB)/(6._/**/REALKIND*sw**2)
    f(20) = (CI*eQED**3*gQCD**4*integralnorm*SwB)/(3._/**/REALKIND*sw**2)
    f(21) = (CI*eQED**3*gQCD**4*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(22) = (eQED**3*gQCD**4*integralnorm*SwB)/(sw**2*12._/**/REALKIND)
    f(23) = (eQED**3*gQCD**4*integralnorm*SwB)/(sw**2*6._/**/REALKIND)
    f(24) = (eQED**3*gQCD**4*integralnorm*SwB)/(sw**2*4._/**/REALKIND)
    f(25) = (eQED**3*gQCD**4*integralnorm*SwB)/(sw**2*3._/**/REALKIND)
    f(26) = (eQED**3*gQCD**4*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(27) = (eQED**3*gQCD**4*integralnorm*SwF)/(sw**2*6._/**/REALKIND)
    f(28) = (eQED**3*gQCD**4*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(29) = (eQED**3*gQCD**4*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(30) = (2*eQED**3*gQCD**4*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(31) = (eQED**3*gQCD**4*integralnorm*SwF)/sw**2

  c = [ 9*CI*f(19), 27*CI*f(19), 9*CI*f(20), 27*CI*f(20), 9*CI*f(21), 27*CI*f(21), 18*f(22), 54*f(22), f(23), 3*f(23), 6*f(23) &
    , 8*f(23), 10*f(23), 18*f(23), 21*f(23), 24*f(23), 54*f(23), 18*f(24), 54*f(24), f(25), 3*f(25), 6*f(25), 8*f(25), 10*f(25) &
    , 18*f(25), 21*f(25), 24*f(25), 54*f(25), f(26), 3*f(26), 6*f(26), 8*f(26), 10*f(26), 18*f(26), 21*f(26), 24*f(26), 54*f(26) &
    , 3*f(27), 9*f(27), 3*f(28), 9*f(28), 3*f(29), 9*f(29), 3*f(30), 9*f(30), 3*f(31), 9*f(31) ]
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
  complex(REALKIND) :: A(104)
  ! external WFs
  call wf_A(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_W(wf(:,-1),wf(:,0),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_VQ_A(wf(:,-6),wf(:,-4),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MW,1_intkind1,wf(:,4))
  call prop_Q_A(wf(:,3),Q(:,80),ZERO,0_intkind1,wf(:,5))
  call vert_AW_Q(wf(:,-5),wf(:,4),wf(:,6))
  call vert_VQ_A(wf(:,2),wf(:,5),wf(:,7))
  call prop_A_Q(wf(:,6),Q(:,35),ZERO,0_intkind1,wf(:,8))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,9))
  call vert_WQ_A(wf(:,4),wf(:,5),wf(:,10))
  call prop_A_Q(wf(:,9),Q(:,44),ZERO,0_intkind1,wf(:,11))
  call vert_AV_Q(wf(:,-5),wf(:,-6),wf(:,12))
  call prop_A_Q(wf(:,12),Q(:,96),ZERO,0_intkind1,wf(:,13))
  call vert_WQ_A(wf(:,4),wf(:,-4),wf(:,14))
  call vert_AV_Q(wf(:,13),wf(:,2),wf(:,15))
  call prop_Q_A(wf(:,14),Q(:,19),ZERO,0_intkind1,wf(:,16))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,17))
  call vert_AW_Q(wf(:,13),wf(:,4),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,28),ZERO,0_intkind1,wf(:,19))
  call vert_VQ_A(wf(:,-6),wf(:,16),wf(:,20))
  call vert_VQ_A(wf(:,-6),wf(:,19),wf(:,21))
  call vert_UV_W(wf(:,4),Q(:,3),wf(:,-6),Q(:,64),wf(:,22))
  call prop_W_W(wf(:,22),Q(:,67),MW,1_intkind1,wf(:,23))
  call vert_QA_W(wf(:,19),wf(:,-5),wf(:,24))
  call vert_QA_W(wf(:,-4),wf(:,11),wf(:,25))
  call vert_VQ_A(wf(:,-6),wf(:,-2),wf(:,26))
  call prop_Q_A(wf(:,26),Q(:,68),ZERO,0_intkind1,wf(:,27))
  call vert_QA_V(wf(:,27),wf(:,-3),wf(:,28))
  call vert_AV_Q(wf(:,-5),wf(:,28),wf(:,29))
  call vert_VQ_A(wf(:,28),wf(:,-4),wf(:,30))
  call vert_AV_Q(wf(:,-3),wf(:,-6),wf(:,31))
  call prop_A_Q(wf(:,31),Q(:,72),ZERO,0_intkind1,wf(:,32))
  call vert_QA_V(wf(:,-2),wf(:,32),wf(:,33))
  call vert_AV_Q(wf(:,-5),wf(:,33),wf(:,34))
  call vert_VQ_A(wf(:,33),wf(:,-4),wf(:,35))
  call vert_VQ_A(wf(:,-6),wf(:,-1),wf(:,36))
  call prop_Q_A(wf(:,36),Q(:,66),ZERO,0_intkind1,wf(:,37))
  call vert_QA_W(wf(:,37),wf(:,0),wf(:,38))
  call prop_W_W(wf(:,38),Q(:,67),MW,1_intkind1,wf(:,39))
  call vert_WQ_A(wf(:,39),wf(:,-4),wf(:,40))
  call vert_AW_Q(wf(:,-5),wf(:,39),wf(:,41))
  call counter_VQ_A(wf(:,2),wf(:,5),wf(:,42))
  call counter_WQ_A(wf(:,4),wf(:,5),wf(:,43))
  call counter_AV_Q(wf(:,13),wf(:,2),wf(:,44))
  call counter_AW_Q(wf(:,13),wf(:,4),wf(:,45))
  call counter_VQ_A(wf(:,-6),wf(:,16),wf(:,46))
  call counter_VQ_A(wf(:,-6),wf(:,19),wf(:,47))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,48))
  call prop_Q_A(wf(:,10),Q(:,83),ZERO,0_intkind1,wf(:,49))
  call counter_AW_Q(wf(:,-5),wf(:,4),wf(:,50))
  call prop_Q_A(wf(:,7),Q(:,92),ZERO,0_intkind1,wf(:,51))
  call prop_A_Q(wf(:,48),Q(:,44),ZERO,0_intkind1,wf(:,52))
  call counter_QA_W(wf(:,19),wf(:,-5),wf(:,53))
  call vert_QA_W(wf(:,-4),wf(:,52),wf(:,54))
  call prop_A_Q(wf(:,50),Q(:,35),ZERO,0_intkind1,wf(:,55))
  call counter_AV_Q(wf(:,-5),wf(:,-6),wf(:,56))
  call prop_A_Q(wf(:,56),Q(:,96),ZERO,0_intkind1,wf(:,57))
  call vert_AV_Q(wf(:,57),wf(:,2),wf(:,58))
  call vert_AW_Q(wf(:,57),wf(:,4),wf(:,59))
  call counter_VQ_A(wf(:,2),wf(:,-4),wf(:,60))
  call prop_A_Q(wf(:,18),Q(:,99),ZERO,0_intkind1,wf(:,61))
  call counter_WQ_A(wf(:,4),wf(:,-4),wf(:,62))
  call prop_A_Q(wf(:,15),Q(:,108),ZERO,0_intkind1,wf(:,63))
  call prop_Q_A(wf(:,60),Q(:,28),ZERO,0_intkind1,wf(:,64))
  call vert_VQ_A(wf(:,-6),wf(:,64),wf(:,65))
  call counter_QA_W(wf(:,-4),wf(:,11),wf(:,66))
  call vert_QA_W(wf(:,64),wf(:,-5),wf(:,67))
  call prop_Q_A(wf(:,62),Q(:,19),ZERO,0_intkind1,wf(:,68))
  call vert_VQ_A(wf(:,-6),wf(:,68),wf(:,69))
  call counter_VQ_A(wf(:,-6),wf(:,-4),wf(:,70))
  call prop_Q_A(wf(:,70),Q(:,80),ZERO,0_intkind1,wf(:,71))
  call vert_VQ_A(wf(:,2),wf(:,71),wf(:,72))
  call vert_WQ_A(wf(:,4),wf(:,71),wf(:,73))
  call counter_AV_Q(wf(:,-5),wf(:,28),wf(:,74))
  call counter_AV_Q(wf(:,-5),wf(:,33),wf(:,75))
  call counter_VQ_A(wf(:,28),wf(:,-4),wf(:,76))
  call counter_VQ_A(wf(:,33),wf(:,-4),wf(:,77))
  call counter_QA_V(wf(:,27),wf(:,-3),wf(:,78))
  call vert_AV_Q(wf(:,-5),wf(:,78),wf(:,79))
  call vert_VQ_A(wf(:,78),wf(:,-4),wf(:,80))
  call counter_AV_Q(wf(:,-3),wf(:,-6),wf(:,81))
  call prop_A_Q(wf(:,81),Q(:,72),ZERO,0_intkind1,wf(:,82))
  call vert_QA_V(wf(:,-2),wf(:,82),wf(:,83))
  call vert_AV_Q(wf(:,-5),wf(:,83),wf(:,84))
  call vert_VQ_A(wf(:,83),wf(:,-4),wf(:,85))
  call counter_QA_V(wf(:,-2),wf(:,32),wf(:,86))
  call vert_AV_Q(wf(:,-5),wf(:,86),wf(:,87))
  call vert_VQ_A(wf(:,86),wf(:,-4),wf(:,88))
  call counter_VQ_A(wf(:,-6),wf(:,-2),wf(:,89))
  call prop_Q_A(wf(:,89),Q(:,68),ZERO,0_intkind1,wf(:,90))
  call vert_QA_V(wf(:,90),wf(:,-3),wf(:,91))
  call vert_AV_Q(wf(:,-5),wf(:,91),wf(:,92))
  call vert_VQ_A(wf(:,91),wf(:,-4),wf(:,93))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,94))
  call vert_VQ_A(wf(:,94),wf(:,5),wf(:,95))
  call vert_AV_Q(wf(:,-5),wf(:,94),wf(:,96))
  call prop_A_Q(wf(:,96),Q(:,44),ZERO,0_intkind1,wf(:,97))
  call vert_AV_Q(wf(:,13),wf(:,94),wf(:,98))
  call vert_VQ_A(wf(:,94),wf(:,-4),wf(:,99))
  call prop_Q_A(wf(:,99),Q(:,28),ZERO,0_intkind1,wf(:,100))
  call vert_VQ_A(wf(:,-6),wf(:,100),wf(:,101))
  call vert_QA_W(wf(:,100),wf(:,-5),wf(:,102))
  call vert_QA_W(wf(:,-4),wf(:,97),wf(:,103))
  call counter_AW_Q(wf(:,-5),wf(:,39),wf(:,104))
  call counter_WQ_A(wf(:,39),wf(:,-4),wf(:,105))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,106))
  call vert_AV_Q(wf(:,-5),wf(:,106),wf(:,107))
  call vert_VQ_A(wf(:,106),wf(:,5),wf(:,108))
  call counter_Q_A(ctcc,wf(:,5),Q(:,80),wf(:,109))
  call prop_Q_A(wf(:,109),Q(:,80),ZERO,0_intkind1,wf(:,110))
  call vert_VQ_A(wf(:,2),wf(:,110),wf(:,111))
  call vert_WQ_A(wf(:,4),wf(:,110),wf(:,112))
  call counter_A_Q(ctcc,wf(:,8),Q(:,35),wf(:,113))
  call counter_A_Q(ctqq,wf(:,11),Q(:,44),wf(:,114))
  call vert_VQ_A(wf(:,106),wf(:,-4),wf(:,115))
  call vert_VQ_A(wf(:,106),wf(:,16),wf(:,116))
  call counter_Q_A(ctqq,wf(:,16),Q(:,19),wf(:,117))
  call counter_Q_A(ctcc,wf(:,19),Q(:,28),wf(:,118))
  call counter_A_Q(ctqq,wf(:,13),Q(:,96),wf(:,119))
  call prop_A_Q(wf(:,119),Q(:,96),ZERO,0_intkind1,wf(:,120))
  call vert_AV_Q(wf(:,120),wf(:,2),wf(:,121))
  call vert_AW_Q(wf(:,120),wf(:,4),wf(:,122))
  call vert_AV_Q(wf(:,8),wf(:,-6),wf(:,123))
  call prop_Q_A(wf(:,115),Q(:,28),ZERO,0_intkind1,wf(:,124))
  call vert_AW_Q(wf(:,-5),wf(:,23),wf(:,125))
  call prop_A_Q(wf(:,107),Q(:,44),ZERO,0_intkind1,wf(:,126))
  call vert_WQ_A(wf(:,23),wf(:,-4),wf(:,127))
  call prop_Q_A(wf(:,127),Q(:,83),ZERO,0_intkind1,wf(:,128))
  call prop_A_Q(wf(:,125),Q(:,99),ZERO,0_intkind1,wf(:,129))
  call vert_AV_Q(wf(:,11),wf(:,-6),wf(:,130))
  call prop_A_Q(wf(:,130),Q(:,108),ZERO,0_intkind1,wf(:,131))
  call prop_A_Q(wf(:,123),Q(:,99),ZERO,0_intkind1,wf(:,132))
  call prop_Q_A(wf(:,20),Q(:,83),ZERO,0_intkind1,wf(:,133))
  call prop_Q_A(wf(:,21),Q(:,92),ZERO,0_intkind1,wf(:,134))
  call counter_Q_A(ctqq,wf(:,27),Q(:,68),wf(:,135))
  call prop_Q_A(wf(:,135),Q(:,68),ZERO,0_intkind1,wf(:,136))
  call vert_QA_V(wf(:,136),wf(:,-3),wf(:,137))
  call vert_QA_V(wf(:,16),wf(:,-5),wf(:,138))
  call vert_QA_V(wf(:,-4),wf(:,8),wf(:,139))
  call counter_V_V(ctGG,wf(:,28),Q(:,76),wf(:,140))
  call prop_A_Q(wf(:,29),Q(:,108),ZERO,0_intkind1,wf(:,141))
  call prop_Q_A(wf(:,30),Q(:,92),ZERO,0_intkind1,wf(:,142))
  call counter_A_Q(ctqq,wf(:,32),Q(:,72),wf(:,143))
  call prop_A_Q(wf(:,143),Q(:,72),ZERO,0_intkind1,wf(:,144))
  call vert_QA_V(wf(:,-2),wf(:,144),wf(:,145))
  call counter_V_V(ctGG,wf(:,33),Q(:,76),wf(:,146))
  call prop_A_Q(wf(:,34),Q(:,108),ZERO,0_intkind1,wf(:,147))
  call prop_Q_A(wf(:,35),Q(:,92),ZERO,0_intkind1,wf(:,148))
  call prop_Q_A(wf(:,40),Q(:,83),ZERO,0_intkind1,wf(:,149))
  call prop_A_Q(wf(:,41),Q(:,99),ZERO,0_intkind1,wf(:,150))
  call vert_VQ_A(wf(:,2),wf(:,16),wf(:,151))
  call prop_Q_A(wf(:,151),Q(:,31),ZERO,0_intkind1,wf(:,152))
  call vert_AV_Q(wf(:,8),wf(:,2),wf(:,153))
  call prop_A_Q(wf(:,153),Q(:,47),ZERO,0_intkind1,wf(:,154))
  call vert_WQ_A(wf(:,4),wf(:,19),wf(:,155))
  call prop_Q_A(wf(:,155),Q(:,31),ZERO,0_intkind1,wf(:,156))
  call vert_AW_Q(wf(:,11),wf(:,4),wf(:,157))
  call prop_A_Q(wf(:,157),Q(:,47),ZERO,0_intkind1,wf(:,158))
  call vert_QA_V(wf(:,5),wf(:,8),wf(:,159))
  call vert_QA_V(wf(:,49),wf(:,-5),wf(:,160))
  call vert_QA_V(wf(:,16),wf(:,13),wf(:,161))
  call vert_QA_V(wf(:,-4),wf(:,61),wf(:,162))
  call vert_VQ_A(wf(:,138),wf(:,-2),wf(:,163))
  call prop_Q_A(wf(:,163),Q(:,55),ZERO,0_intkind1,wf(:,164))
  call vert_AV_Q(wf(:,-3),wf(:,138),wf(:,165))
  call prop_A_Q(wf(:,165),Q(:,59),ZERO,0_intkind1,wf(:,166))
  call vert_QA_V(wf(:,133),wf(:,-5),wf(:,167))
  call vert_VQ_A(wf(:,139),wf(:,-2),wf(:,168))
  call prop_Q_A(wf(:,168),Q(:,55),ZERO,0_intkind1,wf(:,169))
  call vert_AV_Q(wf(:,-3),wf(:,139),wf(:,170))
  call prop_A_Q(wf(:,170),Q(:,59),ZERO,0_intkind1,wf(:,171))
  call vert_QA_V(wf(:,-4),wf(:,132),wf(:,172))
  call vert_QA_V(wf(:,128),wf(:,-5),wf(:,173))
  call vert_QA_V(wf(:,-4),wf(:,129),wf(:,174))
  call vert_QA_V(wf(:,149),wf(:,-5),wf(:,175))
  call vert_QA_V(wf(:,-4),wf(:,150),wf(:,176))

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
  den(2) = 1 / (Q(5,12))
  den(3) = 1 / (Q(5,80))
  den(5) = 1 / (Q(5,35))
  den(9) = 1 / (Q(5,44))
  den(12) = 1 / (Q(5,96))
  den(14) = 1 / (Q(5,19))
  den(18) = 1 / (Q(5,28))
  den(23) = 1 / (Q(5,67) - MW2)
  den(27) = 1 / (Q(5,68))
  den(28) = 1 / (Q(5,76))
  den(32) = 1 / (Q(5,72))
  den(36) = 1 / (Q(5,66))
  den(40) = 1 / (Q(5,83))
  den(43) = 1 / (Q(5,92))
  den(46) = 1 / (Q(5,99))
  den(49) = 1 / (Q(5,108))
  den(95) = 1 / (Q(5,51))
  den(121) = 1 / (Q(5,31))
  den(124) = 1 / (Q(5,47))
  den(131) = 1 / (Q(5,115))
  den(137) = 1 / (Q(5,55))
  den(139) = 1 / (Q(5,59))

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
  den(30) = den(15)*den(29)
  den(31) = den(6)*den(29)
  den(33) = den(28)*den(32)
  den(34) = den(15)*den(33)
  den(35) = den(6)*den(33)
  den(37) = den(23)*den(36)
  den(38) = den(10)*den(37)
  den(39) = den(19)*den(37)
  den(41) = den(8)*den(40)
  den(42) = den(2)*den(41)
  den(44) = den(4)*den(43)
  den(45) = den(1)*den(44)
  den(47) = den(17)*den(46)
  den(48) = den(2)*den(47)
  den(50) = den(13)*den(49)
  den(51) = den(1)*den(50)
  den(52) = den(2)**2
  den(53) = den(41)*den(52)
  den(54) = den(3)*den(52)
  den(55) = den(6)*den(54)
  den(56) = den(3)**2
  den(57) = den(2)*den(56)
  den(58) = den(6)*den(57)
  den(59) = den(1)*den(56)
  den(60) = den(10)*den(59)
  den(61) = den(6)*den(44)
  den(62) = den(10)*den(41)
  den(63) = den(47)*den(52)
  den(64) = den(15)*den(52)
  den(65) = den(12)*den(64)
  den(66) = den(15)*den(50)
  den(67) = den(19)*den(47)
  den(68) = den(12)**2
  den(69) = den(2)*den(68)
  den(70) = den(15)*den(69)
  den(71) = den(1)*den(68)
  den(72) = den(19)*den(71)
  den(73) = den(18)*den(52)
  den(74) = den(6)*den(73)
  den(75) = den(24)*den(73)
  den(76) = den(9)*den(52)
  den(77) = den(15)*den(76)
  den(78) = den(24)*den(40)
  den(79) = den(52)*den(78)
  den(80) = den(24)*den(46)
  den(81) = den(19)*den(80)
  den(82) = den(10)*den(78)
  den(83) = den(10)*den(49)
  den(84) = den(15)*den(83)
  den(85) = den(6)*den(46)
  den(86) = den(19)*den(85)
  den(87) = den(15)*den(40)
  den(88) = den(10)*den(87)
  den(89) = den(19)*den(43)
  den(90) = den(6)*den(89)
  den(91) = den(27)**2
  den(92) = den(28)*den(91)
  den(93) = den(15)*den(92)
  den(94) = den(6)*den(92)
  den(96) = den(6)*den(95)
  den(97) = den(29)*den(96)
  den(98) = den(15)*den(95)
  den(99) = den(29)*den(98)
  den(100) = den(29)*den(49)
  den(101) = den(15)*den(100)
  den(102) = den(29)*den(43)
  den(103) = den(6)*den(102)
  den(104) = den(32)**2
  den(105) = den(28)*den(104)
  den(106) = den(15)*den(105)
  den(107) = den(6)*den(105)
  den(108) = den(33)*den(96)
  den(109) = den(33)*den(98)
  den(110) = den(33)*den(49)
  den(111) = den(15)*den(110)
  den(112) = den(33)*den(43)
  den(113) = den(6)*den(112)
  den(114) = den(37)*den(73)
  den(115) = den(37)*den(40)
  den(116) = den(52)*den(115)
  den(117) = den(10)*den(115)
  den(118) = den(37)*den(46)
  den(119) = den(19)*den(118)
  den(120) = den(2)*den(15)
  den(122) = den(120)*den(121)
  den(123) = den(2)*den(6)
  den(125) = den(123)*den(124)
  den(126) = den(1)*den(19)
  den(127) = den(121)*den(126)
  den(128) = den(1)*den(10)
  den(129) = den(124)*den(128)
  den(130) = den(3)*den(6)
  den(132) = den(130)*den(131)
  den(133) = den(41)*den(131)
  den(134) = den(12)*den(15)
  den(135) = den(131)*den(134)
  den(136) = den(47)*den(131)
  den(138) = den(98)*den(137)
  den(140) = den(98)*den(139)
  den(141) = den(87)*den(131)
  den(142) = den(96)*den(137)
  den(143) = den(96)*den(139)
  den(144) = den(85)*den(131)
  den(145) = den(78)*den(131)
  den(146) = den(80)*den(131)
  den(147) = den(115)*den(131)
  den(148) = den(118)*den(131)
  den(149) = den(2)*den(3)*den(6)
  den(150) = den(1)*den(3)*den(10)
  den(151) = den(1)*den(2)*den(3)
  den(152) = den(2)*den(12)*den(15)
  den(153) = den(1)*den(12)*den(19)
  den(154) = den(1)*den(2)*den(12)
  den(155) = den(2)*den(98)
  den(156) = den(2)*den(87)
  den(157) = den(2)*den(96)
  den(158) = den(2)*den(85)
  den(159) = den(2)*den(78)
  den(160) = den(2)*den(80)
  den(161) = den(2)*den(24)
  den(162) = den(1)*den(89)
  den(163) = den(1)*den(83)
  den(164) = den(1)*den(2)
  den(165) = den(27)*den(98)
  den(166) = den(15)*den(27)
  den(167) = den(27)*den(96)
  den(168) = den(6)*den(27)
  den(169) = den(1)*den(102)
  den(170) = den(1)*den(100)
  den(171) = den(1)*den(29)
  den(172) = den(1)*den(27)
  den(173) = den(32)*den(98)
  den(174) = den(15)*den(32)
  den(175) = den(32)*den(96)
  den(176) = den(6)*den(32)
  den(177) = den(1)*den(112)
  den(178) = den(1)*den(110)
  den(179) = den(1)*den(33)
  den(180) = den(1)*den(32)
  den(181) = den(2)*den(115)
  den(182) = den(2)*den(118)
  den(183) = den(2)*den(37)
  den(184) = den(3)*den(125)
  den(185) = den(2)*den(132)
  den(186) = den(3)*den(129)
  den(187) = den(2)*den(133)
  den(188) = den(12)*den(122)
  den(189) = den(2)*den(135)
  den(190) = den(12)*den(127)
  den(191) = den(2)*den(136)
  den(192) = den(2)*den(141)
  den(193) = den(2)*den(144)
  den(194) = den(2)*den(145)
  den(195) = den(2)*den(146)
  den(196) = den(27)*den(140)
  den(197) = den(27)*den(143)
  den(198) = den(32)*den(138)
  den(199) = den(32)*den(142)
  den(200) = den(2)*den(147)
  den(201) = den(2)*den(148)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(104)

  A(1) = cont_QA(wf(:,7),wf(:,8)) * den(7)
  A(2) = cont_QA(wf(:,10),wf(:,11)) * den(11)
  A(3) = cont_QA(wf(:,15),wf(:,16)) * den(16)
  A(4) = cont_QA(wf(:,18),wf(:,19)) * den(20)
  A(5) = cont_QA(wf(:,11),wf(:,20)) * den(21)
  A(6) = cont_QA(wf(:,8),wf(:,21)) * den(22)
  A(7) = cont_VV(wf(:,23),wf(:,24)) * den(25)
  A(8) = cont_VV(wf(:,23),wf(:,25)) * den(26)
  A(9) = cont_QA(wf(:,16),wf(:,29)) * den(30)
  A(10) = cont_QA(wf(:,8),wf(:,30)) * den(31)
  A(11) = cont_QA(wf(:,16),wf(:,34)) * den(34)
  A(12) = cont_QA(wf(:,8),wf(:,35)) * den(35)
  A(13) = cont_QA(wf(:,11),wf(:,40)) * den(38)
  A(14) = cont_QA(wf(:,19),wf(:,41)) * den(39)

  A(15) = cont_QA(wf(:,8),wf(:,42)) * den(7)
  A(16) = cont_QA(wf(:,11),wf(:,43)) * den(11)
  A(17) = cont_QA(wf(:,16),wf(:,44)) * den(16)
  A(18) = cont_QA(wf(:,19),wf(:,45)) * den(20)
  A(19) = cont_QA(wf(:,11),wf(:,46)) * den(21)
  A(20) = cont_QA(wf(:,8),wf(:,47)) * den(22)
  A(21) = cont_QA(wf(:,48),wf(:,49)) * den(42)
  A(22) = cont_QA(wf(:,50),wf(:,51)) * den(45)
  A(23) = cont_QA(wf(:,20),wf(:,52)) * den(21)
  A(24) = cont_VV(wf(:,23),wf(:,53)) * den(25)
  A(25) = cont_VV(wf(:,23),wf(:,54)) * den(26)
  A(26) = cont_QA(wf(:,21),wf(:,55)) * den(22)
  A(27) = cont_QA(wf(:,16),wf(:,58)) * den(16)
  A(28) = cont_QA(wf(:,19),wf(:,59)) * den(20)
  A(29) = cont_QA(wf(:,60),wf(:,61)) * den(48)
  A(30) = cont_QA(wf(:,62),wf(:,63)) * den(51)
  A(31) = cont_QA(wf(:,8),wf(:,65)) * den(22)
  A(32) = cont_VV(wf(:,23),wf(:,66)) * den(26)
  A(33) = cont_VV(wf(:,23),wf(:,67)) * den(25)
  A(34) = cont_QA(wf(:,11),wf(:,69)) * den(21)
  A(35) = cont_QA(wf(:,8),wf(:,72)) * den(7)
  A(36) = cont_QA(wf(:,11),wf(:,73)) * den(11)
  A(37) = cont_QA(wf(:,16),wf(:,74)) * den(30)
  A(38) = cont_QA(wf(:,30),wf(:,55)) * den(31)
  A(39) = cont_QA(wf(:,16),wf(:,75)) * den(34)
  A(40) = cont_QA(wf(:,35),wf(:,55)) * den(35)
  A(41) = cont_QA(wf(:,8),wf(:,76)) * den(31)
  A(42) = cont_QA(wf(:,29),wf(:,68)) * den(30)
  A(43) = cont_QA(wf(:,8),wf(:,77)) * den(35)
  A(44) = cont_QA(wf(:,34),wf(:,68)) * den(34)
  A(45) = cont_QA(wf(:,16),wf(:,79)) * den(30)
  A(46) = cont_QA(wf(:,8),wf(:,80)) * den(31)
  A(47) = cont_QA(wf(:,16),wf(:,84)) * den(34)
  A(48) = cont_QA(wf(:,8),wf(:,85)) * den(35)
  A(49) = cont_QA(wf(:,16),wf(:,87)) * den(34)
  A(50) = cont_QA(wf(:,8),wf(:,88)) * den(35)
  A(51) = cont_QA(wf(:,16),wf(:,92)) * den(30)
  A(52) = cont_QA(wf(:,8),wf(:,93)) * den(31)
  A(53) = cont_QA(wf(:,8),wf(:,95)) * den(7)
  A(54) = cont_QA(wf(:,10),wf(:,97)) * den(11)
  A(55) = cont_QA(wf(:,16),wf(:,98)) * den(16)
  A(56) = cont_QA(wf(:,18),wf(:,100)) * den(20)
  A(57) = cont_QA(wf(:,20),wf(:,97)) * den(21)
  A(58) = cont_QA(wf(:,8),wf(:,101)) * den(22)
  A(59) = cont_VV(wf(:,23),wf(:,102)) * den(25)
  A(60) = cont_VV(wf(:,23),wf(:,103)) * den(26)
  A(61) = cont_QA(wf(:,40),wf(:,52)) * den(38)
  A(62) = cont_QA(wf(:,19),wf(:,104)) * den(39)
  A(63) = cont_QA(wf(:,41),wf(:,64)) * den(39)
  A(64) = cont_QA(wf(:,11),wf(:,105)) * den(38)
  A(65) = cont_QA(wf(:,40),wf(:,97)) * den(38)
  A(66) = cont_QA(wf(:,41),wf(:,100)) * den(39)
  A(67) = cont_QA(wf(:,49),wf(:,107)) * den(53)
  A(68) = cont_QA(wf(:,8),wf(:,108)) * den(55)
  A(69) = cont_QA(wf(:,8),wf(:,111)) * den(58)
  A(70) = cont_QA(wf(:,11),wf(:,112)) * den(60)
  A(71) = cont_QA(wf(:,51),wf(:,113)) * den(61)
  A(72) = cont_QA(wf(:,49),wf(:,114)) * den(62)
  A(73) = cont_QA(wf(:,61),wf(:,115)) * den(63)
  A(74) = cont_QA(wf(:,13),wf(:,116)) * den(65)
  A(75) = cont_QA(wf(:,63),wf(:,117)) * den(66)
  A(76) = cont_QA(wf(:,61),wf(:,118)) * den(67)
  A(77) = cont_QA(wf(:,16),wf(:,121)) * den(70)
  A(78) = cont_QA(wf(:,19),wf(:,122)) * den(72)
  A(79) = cont_QA(wf(:,123),wf(:,124)) * den(74)
  A(80) = cont_QA(wf(:,124),wf(:,125)) * den(75)
  A(81) = cont_QA(wf(:,20),wf(:,126)) * den(77)
  A(82) = cont_QA(wf(:,107),wf(:,128)) * den(79)
  A(83) = cont_QA(wf(:,118),wf(:,129)) * den(81)
  A(84) = cont_QA(wf(:,114),wf(:,128)) * den(82)
  A(85) = cont_QA(wf(:,117),wf(:,131)) * den(84)
  A(86) = cont_QA(wf(:,118),wf(:,132)) * den(86)
  A(87) = cont_QA(wf(:,114),wf(:,133)) * den(88)
  A(88) = cont_QA(wf(:,113),wf(:,134)) * den(90)
  A(89) = cont_VV(wf(:,137),wf(:,138)) * den(93)
  A(90) = cont_VV(wf(:,137),wf(:,139)) * den(94)
  A(91) = cont_VV(wf(:,139),wf(:,140)) * den(97)
  A(92) = cont_VV(wf(:,138),wf(:,140)) * den(99)
  A(93) = cont_QA(wf(:,117),wf(:,141)) * den(101)
  A(94) = cont_QA(wf(:,113),wf(:,142)) * den(103)
  A(95) = cont_VV(wf(:,138),wf(:,145)) * den(106)
  A(96) = cont_VV(wf(:,139),wf(:,145)) * den(107)
  A(97) = cont_VV(wf(:,139),wf(:,146)) * den(108)
  A(98) = cont_VV(wf(:,138),wf(:,146)) * den(109)
  A(99) = cont_QA(wf(:,117),wf(:,147)) * den(111)
  A(100) = cont_QA(wf(:,113),wf(:,148)) * den(113)
  A(101) = cont_QA(wf(:,41),wf(:,124)) * den(114)
  A(102) = cont_QA(wf(:,107),wf(:,149)) * den(116)
  A(103) = cont_QA(wf(:,114),wf(:,149)) * den(117)
  A(104) = cont_QA(wf(:,118),wf(:,150)) * den(119)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(104)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(3)+A(4)+A(5))*f(1))/2._/**/REALKIND+((-A(1)-A(2)-A(6)-A(9)-A(10)-A(11)-A(12))*f(2))/2._/**/REALKIND+((-A(7)-A(8) &
       +A(13)+A(14))*f(3))/2._/**/REALKIND
  M1(2) = ((-A(3)-A(4)-A(5))*f(1))/6._/**/REALKIND+((A(1)+A(2)+A(6)+A(9)+A(10)+A(11)+A(12))*f(2))/6._/**/REALKIND+((A(7)+A(8) &
       -A(13)-A(14))*f(3))/6._/**/REALKIND

  M2(1) = ((-A(73)-A(74)-A(75)-A(76)-A(77)-A(78)-A(81)-A(85)-A(87))*f(4))/2._/**/REALKIND+((A(67)+A(68)+A(69)+A(70)+A(71)+A(72) &
       +A(79)+A(86)+A(88)+A(89)+A(90)+A(91)+A(92)+A(93)+A(94)+A(95)+A(96)+A(97)+A(98)+A(99)+A(100))*f(5))/2._/**/REALKIND+((A(80) &
       +A(82)+A(83)+A(84)-A(101)-A(102)-A(103)-A(104))*f(6))/2._/**/REALKIND+(A(29)*f(7))/2._/**/REALKIND+((-A(15)-A(31)-A(41) &
       -A(43))*f(8))/2._/**/REALKIND+((-A(33)+A(63))*f(9))/2._/**/REALKIND+((A(17)+A(23)+A(55)+A(56)+A(57))*f(10))/2._/**/REALKIND &
       +((-A(21)-A(37)-A(39)-A(45)-A(46)-A(49)-A(50)-A(53)-A(54)-A(58))*f(11))/2._/**/REALKIND+((-A(25)-A(59)-A(60)+A(61)+A(65) &
       +A(66))*f(12))/2._/**/REALKIND+((-A(20)-A(35)-A(36))*f(13))/2._/**/REALKIND+((A(19)+A(27)+A(28))*f(14))/2._/**/REALKIND+(( &
       -A(47)-A(48)-A(51)-A(52))*f(15))/2._/**/REALKIND+((A(18)+A(30)+A(34))*f(16))/2._/**/REALKIND+((-A(16)-A(22)-A(26)-A(38) &
       -A(40)-A(42)-A(44))*f(17))/2._/**/REALKIND+((-A(24)-A(32)+A(62)+A(64))*f(18))/2._/**/REALKIND
  M2(2) = ((A(73)+A(74)+A(75)+A(76)+A(77)+A(78)+A(81)+A(85)+A(87))*f(4))/6._/**/REALKIND+((-A(67)-A(68)-A(69)-A(70)-A(71)-A(72) &
       -A(79)-A(86)-A(88)-A(89)-A(90)-A(91)-A(92)-A(93)-A(94)-A(95)-A(96)-A(97)-A(98)-A(99)-A(100))*f(5))/6._/**/REALKIND+((-A(80) &
       -A(82)-A(83)-A(84)+A(101)+A(102)+A(103)+A(104))*f(6))/6._/**/REALKIND-(A(29)*f(7))/6._/**/REALKIND+((A(15)+A(31)+A(41) &
       +A(43))*f(8))/6._/**/REALKIND+((A(33)-A(63))*f(9))/6._/**/REALKIND+((-A(17)-A(23)-A(55)-A(56)-A(57))*f(10))/6._/**/REALKIND &
       +((A(21)+A(37)+A(39)+A(45)+A(46)+A(49)+A(50)+A(53)+A(54)+A(58))*f(11))/6._/**/REALKIND+((A(25)+A(59)+A(60)-A(61)-A(65) &
       -A(66))*f(12))/6._/**/REALKIND+((A(20)+A(35)+A(36))*f(13))/6._/**/REALKIND+((-A(19)-A(27)-A(28))*f(14))/6._/**/REALKIND &
       +((A(47)+A(48)+A(51)+A(52))*f(15))/6._/**/REALKIND+((-A(18)-A(30)-A(34))*f(16))/6._/**/REALKIND+((A(16)+A(22)+A(26)+A(38) &
       +A(40)+A(42)+A(44))*f(17))/6._/**/REALKIND+((A(24)+A(32)-A(62)-A(64))*f(18))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pplnajj_nexeuuxcsxa_1_/**/REALKIND
