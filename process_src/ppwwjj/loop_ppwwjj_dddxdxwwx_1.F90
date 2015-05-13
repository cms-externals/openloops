
module ol_colourmatrix_ppwwjj_dddxdxwwx_1_/**/REALKIND
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
  K1( 5,:) = [   0,   4]
  K1( 6,:) = [   4,   0]
  K1( 7,:) = [  12,   4]
  K1( 8,:) = [   4,  12]
  K1( 9,:) = [   0,  -4]
  K1(10,:) = [  -4, -12]
  K1(11,:) = [ -12,  -4]
  K1(12,:) = [  -4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [ -12,  -4]
  K1(16,:) = [  -4,   0]
  K1(17,:) = [   0,  -4]
  K1(18,:) = [  -4, -12]
  K1(19,:) = [   0,   4]
  K1(20,:) = [   4,   0]
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
end module ol_colourmatrix_ppwwjj_dddxdxwwx_1_/**/REALKIND



module ol_forced_parameters_ppwwjj_dddxdxwwx_1_/**/REALKIND
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
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppwwjj_dddxdxwwx_1_/**/REALKIND

module ol_loop_ppwwjj_dddxdxwwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(33), c(57)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:221)
  ! denominators
  complex(REALKIND), save :: den(287)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,144)
  ! zero helicity identifier
  logical,           save :: zerohel(144) = .true., zerohel_ct(144) = .true.

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
    f( 1) = (CI*eQED**2*gQCD**2)/3._/**/REALKIND
    f( 2) = (CI*countertermnorm*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 3) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 4) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 5) = CI*countertermnorm*ctWWGG*eQED**2*gQCD**4
    f( 6) = (CI*eQED**2*gQCD**2)/(2._/**/REALKIND*sw**2)
    f( 7) = (CI*countertermnorm*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f( 8) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f( 9) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(10) = (CI*cw*eQED**2*gQCD**2)/sw
    f(11) = (CI*countertermnorm*cw*eQED**2*gQCD**4)/sw
    f(12) = (CI*countertermnorm*ctGqq*cw*eQED**2*gQCD**4)/sw
    f(13) = (CI*countertermnorm*ctVqq*cw*eQED**2*gQCD**4)/sw
    f(14) = (countertermnorm*ctZGG*cw*eQED**2*gQCD**4)/sw
    f(15) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MW)/sw
    f(16) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(17) = (eQED**2*gQCD**4*integralnorm*SwB)/6._/**/REALKIND
    f(18) = (eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(19) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(20) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*4._/**/REALKIND)
    f(21) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(22) = (CI*cw*eQED**2*gQCD**4*integralnorm*SwB)/sw
    f(23) = (cw*eQED**2*gQCD**4*integralnorm*SwB)/(sw*2._/**/REALKIND)
    f(24) = (cw*eQED**2*gQCD**4*integralnorm*SwB)/sw
    f(25) = (eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(26) = (2*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(27) = (4*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(28) = (eQED**2*gQCD**4*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(29) = (eQED**2*gQCD**4*integralnorm*SwF)/sw**2
    f(30) = (eQED**2*gQCD**4*integralnorm*MB*SwF)/(sw**2*2._/**/REALKIND)
    f(31) = (eQED**2*gQCD**4*integralnorm*MT*SwF)/(sw**2*2._/**/REALKIND)
    f(32) = (cw*eQED**2*gQCD**4*integralnorm*SwF)/sw
    f(33) = (2*cw*eQED**2*gQCD**4*integralnorm*SwF)/sw

  c = [ 9*CI*f(16), 27*CI*f(16), 18*f(17), 54*f(17), f(18), 3*f(18), 6*f(18), 8*f(18), 10*f(18), 18*f(18), 21*f(18), 24*f(18) &
    , 54*f(18), 9*CI*f(19), 27*CI*f(19), 18*f(20), 54*f(20), f(21), 3*f(21), 6*f(21), 8*f(21), 10*f(21), 18*f(21), 21*f(21) &
    , 24*f(21), 54*f(21), 9*CI*f(22), 27*CI*f(22), 18*f(23), 54*f(23), f(24), 3*f(24), 6*f(24), 8*f(24), 10*f(24), 18*f(24) &
    , 21*f(24), 24*f(24), 54*f(24), 3*f(25), 9*f(25), 3*f(26), 9*f(26), 3*f(27), 9*f(27), 3*f(28), 9*f(28), 3*f(29), 9*f(29) &
    , 3*f(30), 9*f(30), 3*f(31), 9*f(31), 3*f(32), 9*f(32), 3*f(33), 9*f(33) ]
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
  complex(REALKIND) :: A(198)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rMW, H(5), wf(:,-4))
  call wf_V(P(:,6), rMW, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,1))
  call vert_WQ_A(wf(:,-5),wf(:,-1),wf(:,2))
  call vert_AW_Q(wf(:,-3),wf(:,-4),wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,34),ZERO,0_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,24),ZERO,0_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,6))
  call vert_VQ_A(wf(:,1),wf(:,-1),wf(:,7))
  call vert_AW_Q(wf(:,5),wf(:,-5),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,7),ZERO,0_intkind1,wf(:,9))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,10))
  call vert_WQ_A(wf(:,-4),wf(:,4),wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,13),ZERO,0_intkind1,wf(:,12))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-4),Q(:,16),wf(:,13))
  call vert_AV_Q(wf(:,-3),wf(:,13),wf(:,14))
  call prop_W_W(wf(:,13),Q(:,48),MZ,1_intkind1,wf(:,15))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,15),wf(:,16))
  call vert_VQ_A(wf(:,13),wf(:,-1),wf(:,17))
  call prop_Q_A(wf(:,17),Q(:,50),ZERO,0_intkind1,wf(:,18))
  call vert_ZQ_A(gZd,wf(:,15),wf(:,-1),wf(:,19))
  call prop_Q_A(wf(:,19),Q(:,50),ZERO,0_intkind1,wf(:,20))
  call vert_WQ_A(wf(:,-5),wf(:,0),wf(:,21))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,22))
  call prop_Q_A(wf(:,21),Q(:,33),ZERO,0_intkind1,wf(:,23))
  call vert_VQ_A(wf(:,22),wf(:,23),wf(:,24))
  call vert_VQ_A(wf(:,22),wf(:,0),wf(:,25))
  call prop_Q_A(wf(:,25),Q(:,7),ZERO,0_intkind1,wf(:,26))
  call vert_AV_Q(wf(:,-3),wf(:,22),wf(:,27))
  call vert_WQ_A(wf(:,-4),wf(:,23),wf(:,28))
  call prop_A_Q(wf(:,27),Q(:,14),ZERO,0_intkind1,wf(:,29))
  call vert_VQ_A(wf(:,13),wf(:,0),wf(:,30))
  call prop_Q_A(wf(:,30),Q(:,49),ZERO,0_intkind1,wf(:,31))
  call vert_ZQ_A(gZd,wf(:,15),wf(:,0),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,49),ZERO,0_intkind1,wf(:,33))
  call vert_QA_V(wf(:,0),wf(:,-3),wf(:,34))
  call vert_AW_Q(wf(:,-2),wf(:,-4),wf(:,35))
  call prop_A_Q(wf(:,35),Q(:,20),ZERO,0_intkind1,wf(:,36))
  call vert_VQ_A(wf(:,34),wf(:,4),wf(:,37))
  call vert_VQ_A(wf(:,34),wf(:,-1),wf(:,38))
  call vert_AW_Q(wf(:,36),wf(:,-5),wf(:,39))
  call prop_Q_A(wf(:,38),Q(:,11),ZERO,0_intkind1,wf(:,40))
  call vert_AV_Q(wf(:,-2),wf(:,34),wf(:,41))
  call prop_A_Q(wf(:,41),Q(:,13),ZERO,0_intkind1,wf(:,42))
  call vert_AV_Q(wf(:,-2),wf(:,13),wf(:,43))
  call vert_AZ_Q(gZd,wf(:,-2),wf(:,15),wf(:,44))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,45))
  call vert_VQ_A(wf(:,45),wf(:,23),wf(:,46))
  call vert_VQ_A(wf(:,45),wf(:,0),wf(:,47))
  call prop_Q_A(wf(:,47),Q(:,11),ZERO,0_intkind1,wf(:,48))
  call vert_AV_Q(wf(:,-2),wf(:,45),wf(:,49))
  call prop_A_Q(wf(:,49),Q(:,14),ZERO,0_intkind1,wf(:,50))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,1),wf(:,51))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,34),wf(:,52))
  call vert_VV_S(wf(:,-4),wf(:,-5),wf(:,53))
  call counter_GG_S(wf(:,1),wf(:,45),wf(:,54))
  call counter_GG_V(wf(:,1),Q(:,5),wf(:,45),Q(:,10),wf(:,55))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,56))
  call counter_AW_Q(wf(:,5),wf(:,-5),wf(:,57))
  call counter_WQ_A(wf(:,-4),wf(:,4),wf(:,58))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,59))
  call prop_Q_A(wf(:,11),Q(:,50),ZERO,0_intkind1,wf(:,60))
  call counter_AV_Q(wf(:,-3),wf(:,13),wf(:,61))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,15),wf(:,62))
  call counter_AW_Q(wf(:,-3),wf(:,-4),wf(:,63))
  call prop_A_Q(wf(:,63),Q(:,24),ZERO,0_intkind1,wf(:,64))
  call vert_AW_Q(wf(:,64),wf(:,-5),wf(:,65))
  call counter_GG_S(wf(:,22),wf(:,34),wf(:,66))
  call counter_GG_V(wf(:,22),Q(:,6),wf(:,34),Q(:,9),wf(:,67))
  call counter_VQ_A(wf(:,22),wf(:,23),wf(:,68))
  call counter_WQ_A(wf(:,-4),wf(:,23),wf(:,69))
  call counter_AV_Q(wf(:,-3),wf(:,22),wf(:,70))
  call prop_Q_A(wf(:,28),Q(:,49),ZERO,0_intkind1,wf(:,71))
  call counter_VQ_A(wf(:,34),wf(:,4),wf(:,72))
  call counter_AW_Q(wf(:,36),wf(:,-5),wf(:,73))
  call counter_VQ_A(wf(:,45),wf(:,23),wf(:,74))
  call counter_AV_Q(wf(:,-2),wf(:,34),wf(:,75))
  call counter_AV_Q(wf(:,-2),wf(:,13),wf(:,76))
  call counter_AZ_Q(gZd,wf(:,-2),wf(:,15),wf(:,77))
  call counter_AW_Q(wf(:,-2),wf(:,-4),wf(:,78))
  call prop_A_Q(wf(:,78),Q(:,20),ZERO,0_intkind1,wf(:,79))
  call vert_AW_Q(wf(:,79),wf(:,-5),wf(:,80))
  call counter_AV_Q(wf(:,-2),wf(:,45),wf(:,81))
  call counter_VQ_A(wf(:,1),wf(:,-1),wf(:,82))
  call prop_A_Q(wf(:,8),Q(:,56),ZERO,0_intkind1,wf(:,83))
  call counter_WQ_A(wf(:,-5),wf(:,-1),wf(:,84))
  call prop_Q_A(wf(:,84),Q(:,34),ZERO,0_intkind1,wf(:,85))
  call vert_VQ_A(wf(:,1),wf(:,85),wf(:,86))
  call counter_VQ_A(wf(:,13),wf(:,-1),wf(:,87))
  call counter_ZQ_A(gZd,wf(:,15),wf(:,-1),wf(:,88))
  call prop_A_Q(wf(:,14),Q(:,56),ZERO,0_intkind1,wf(:,89))
  call prop_A_Q(wf(:,16),Q(:,56),ZERO,0_intkind1,wf(:,90))
  call vert_WQ_A(wf(:,-4),wf(:,85),wf(:,91))
  call counter_VQ_A(wf(:,34),wf(:,-1),wf(:,92))
  call prop_A_Q(wf(:,39),Q(:,52),ZERO,0_intkind1,wf(:,93))
  call vert_VQ_A(wf(:,34),wf(:,85),wf(:,94))
  call prop_A_Q(wf(:,43),Q(:,52),ZERO,0_intkind1,wf(:,95))
  call prop_A_Q(wf(:,44),Q(:,52),ZERO,0_intkind1,wf(:,96))
  call counter_QA_V(wf(:,-1),wf(:,-3),wf(:,97))
  call vert_VQ_A(wf(:,97),wf(:,23),wf(:,98))
  call vert_VQ_A(wf(:,97),wf(:,0),wf(:,99))
  call prop_Q_A(wf(:,99),Q(:,11),ZERO,0_intkind1,wf(:,100))
  call vert_AV_Q(wf(:,-2),wf(:,97),wf(:,101))
  call prop_A_Q(wf(:,101),Q(:,14),ZERO,0_intkind1,wf(:,102))
  call counter_QA_V(wf(:,-1),wf(:,-2),wf(:,103))
  call vert_VQ_A(wf(:,103),wf(:,23),wf(:,104))
  call vert_VQ_A(wf(:,103),wf(:,0),wf(:,105))
  call prop_Q_A(wf(:,105),Q(:,7),ZERO,0_intkind1,wf(:,106))
  call vert_AV_Q(wf(:,-3),wf(:,103),wf(:,107))
  call prop_A_Q(wf(:,107),Q(:,14),ZERO,0_intkind1,wf(:,108))
  call counter_VQ_A(wf(:,22),wf(:,0),wf(:,109))
  call counter_WQ_A(wf(:,-5),wf(:,0),wf(:,110))
  call prop_Q_A(wf(:,110),Q(:,33),ZERO,0_intkind1,wf(:,111))
  call vert_VQ_A(wf(:,22),wf(:,111),wf(:,112))
  call counter_VQ_A(wf(:,13),wf(:,0),wf(:,113))
  call counter_ZQ_A(gZd,wf(:,15),wf(:,0),wf(:,114))
  call vert_WQ_A(wf(:,-4),wf(:,111),wf(:,115))
  call counter_VQ_A(wf(:,45),wf(:,0),wf(:,116))
  call vert_VQ_A(wf(:,45),wf(:,111),wf(:,117))
  call counter_QA_V(wf(:,0),wf(:,-3),wf(:,118))
  call vert_VQ_A(wf(:,118),wf(:,4),wf(:,119))
  call vert_VQ_A(wf(:,118),wf(:,-1),wf(:,120))
  call prop_Q_A(wf(:,120),Q(:,11),ZERO,0_intkind1,wf(:,121))
  call vert_AV_Q(wf(:,-2),wf(:,118),wf(:,122))
  call prop_A_Q(wf(:,122),Q(:,13),ZERO,0_intkind1,wf(:,123))
  call counter_QA_V(wf(:,0),wf(:,-2),wf(:,124))
  call vert_VQ_A(wf(:,124),wf(:,4),wf(:,125))
  call vert_VQ_A(wf(:,124),wf(:,-1),wf(:,126))
  call prop_Q_A(wf(:,126),Q(:,7),ZERO,0_intkind1,wf(:,127))
  call vert_AV_Q(wf(:,-3),wf(:,124),wf(:,128))
  call prop_A_Q(wf(:,128),Q(:,13),ZERO,0_intkind1,wf(:,129))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,130))
  call counter_V_V(ctGG,wf(:,1),Q(:,5),wf(:,131))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,132))
  call counter_Q_A(ctqq,wf(:,4),Q(:,34),wf(:,133))
  call prop_A_Q(wf(:,132),Q(:,29),ZERO,0_intkind1,wf(:,134))
  call counter_A_Q(ctqq,wf(:,5),Q(:,24),wf(:,135))
  call prop_Q_A(wf(:,6),Q(:,39),ZERO,0_intkind1,wf(:,136))
  call vert_VQ_A(wf(:,131),wf(:,-1),wf(:,137))
  call counter_Q_A(ctqq,wf(:,9),Q(:,7),wf(:,138))
  call prop_A_Q(wf(:,135),Q(:,24),ZERO,0_intkind1,wf(:,139))
  call vert_AW_Q(wf(:,139),wf(:,-5),wf(:,140))
  call vert_AV_Q(wf(:,-3),wf(:,131),wf(:,141))
  call prop_Q_A(wf(:,133),Q(:,34),ZERO,0_intkind1,wf(:,142))
  call vert_WQ_A(wf(:,-4),wf(:,142),wf(:,143))
  call counter_A_Q(ctqq,wf(:,12),Q(:,13),wf(:,144))
  call counter_Q_A(ctqq,wf(:,18),Q(:,50),wf(:,145))
  call counter_Q_A(ctqq,wf(:,20),Q(:,50),wf(:,146))
  call vert_AV_Q(wf(:,5),wf(:,22),wf(:,147))
  call counter_Q_A(ctqq,wf(:,23),Q(:,33),wf(:,148))
  call prop_A_Q(wf(:,147),Q(:,30),ZERO,0_intkind1,wf(:,149))
  call vert_QA_V(wf(:,23),wf(:,5),wf(:,150))
  call counter_V_V(ctGG,wf(:,22),Q(:,6),wf(:,151))
  call prop_Q_A(wf(:,24),Q(:,39),ZERO,0_intkind1,wf(:,152))
  call vert_VQ_A(wf(:,151),wf(:,0),wf(:,153))
  call counter_Q_A(ctqq,wf(:,26),Q(:,7),wf(:,154))
  call prop_Q_A(wf(:,148),Q(:,33),ZERO,0_intkind1,wf(:,155))
  call vert_WQ_A(wf(:,-4),wf(:,155),wf(:,156))
  call vert_AV_Q(wf(:,-3),wf(:,151),wf(:,157))
  call counter_A_Q(ctqq,wf(:,29),Q(:,14),wf(:,158))
  call counter_Q_A(ctqq,wf(:,31),Q(:,49),wf(:,159))
  call counter_Q_A(ctqq,wf(:,33),Q(:,49),wf(:,160))
  call vert_QA_V(wf(:,4),wf(:,36),wf(:,161))
  call counter_V_V(ctGG,wf(:,34),Q(:,9),wf(:,162))
  call vert_AV_Q(wf(:,36),wf(:,34),wf(:,163))
  call prop_A_Q(wf(:,163),Q(:,29),ZERO,0_intkind1,wf(:,164))
  call counter_A_Q(ctqq,wf(:,36),Q(:,20),wf(:,165))
  call prop_Q_A(wf(:,37),Q(:,43),ZERO,0_intkind1,wf(:,166))
  call vert_VQ_A(wf(:,162),wf(:,-1),wf(:,167))
  call counter_Q_A(ctqq,wf(:,40),Q(:,11),wf(:,168))
  call prop_A_Q(wf(:,165),Q(:,20),ZERO,0_intkind1,wf(:,169))
  call vert_AW_Q(wf(:,169),wf(:,-5),wf(:,170))
  call vert_AV_Q(wf(:,-2),wf(:,162),wf(:,171))
  call counter_A_Q(ctqq,wf(:,42),Q(:,13),wf(:,172))
  call vert_AV_Q(wf(:,36),wf(:,45),wf(:,173))
  call prop_A_Q(wf(:,173),Q(:,30),ZERO,0_intkind1,wf(:,174))
  call vert_QA_V(wf(:,23),wf(:,36),wf(:,175))
  call counter_V_V(ctGG,wf(:,45),Q(:,10),wf(:,176))
  call prop_Q_A(wf(:,46),Q(:,43),ZERO,0_intkind1,wf(:,177))
  call vert_VQ_A(wf(:,176),wf(:,0),wf(:,178))
  call counter_Q_A(ctqq,wf(:,48),Q(:,11),wf(:,179))
  call vert_AV_Q(wf(:,-2),wf(:,176),wf(:,180))
  call counter_A_Q(ctqq,wf(:,50),Q(:,14),wf(:,181))
  call vert_WQ_A(wf(:,-5),wf(:,9),wf(:,182))
  call prop_Q_A(wf(:,182),Q(:,39),ZERO,0_intkind1,wf(:,183))
  call vert_AW_Q(wf(:,12),wf(:,-4),wf(:,184))
  call prop_A_Q(wf(:,184),Q(:,29),ZERO,0_intkind1,wf(:,185))
  call vert_WQ_A(wf(:,-5),wf(:,26),wf(:,186))
  call prop_Q_A(wf(:,186),Q(:,39),ZERO,0_intkind1,wf(:,187))
  call vert_AW_Q(wf(:,29),wf(:,-4),wf(:,188))
  call prop_A_Q(wf(:,188),Q(:,30),ZERO,0_intkind1,wf(:,189))
  call vert_WQ_A(wf(:,-5),wf(:,40),wf(:,190))
  call prop_Q_A(wf(:,190),Q(:,43),ZERO,0_intkind1,wf(:,191))
  call vert_AW_Q(wf(:,42),wf(:,-4),wf(:,192))
  call prop_A_Q(wf(:,192),Q(:,29),ZERO,0_intkind1,wf(:,193))
  call vert_WQ_A(wf(:,-5),wf(:,48),wf(:,194))
  call prop_Q_A(wf(:,194),Q(:,43),ZERO,0_intkind1,wf(:,195))
  call vert_AW_Q(wf(:,50),wf(:,-4),wf(:,196))
  call prop_A_Q(wf(:,196),Q(:,30),ZERO,0_intkind1,wf(:,197))
  call vert_QA_V(wf(:,0),wf(:,93),wf(:,198))
  call vert_QA_V(wf(:,-1),wf(:,93),wf(:,199))
  call vert_QA_V(wf(:,0),wf(:,83),wf(:,200))
  call vert_QA_V(wf(:,-1),wf(:,83),wf(:,201))
  call vert_QA_V(wf(:,71),wf(:,-2),wf(:,202))
  call vert_QA_V(wf(:,71),wf(:,-3),wf(:,203))
  call vert_QA_V(wf(:,60),wf(:,-2),wf(:,204))
  call vert_QA_V(wf(:,60),wf(:,-3),wf(:,205))
  call vert_QA_V(wf(:,31),wf(:,-2),wf(:,206))
  call vert_QA_V(wf(:,33),wf(:,-2),wf(:,207))
  call vert_QA_V(wf(:,0),wf(:,95),wf(:,208))
  call vert_QA_V(wf(:,0),wf(:,96),wf(:,209))
  call vert_QA_V(wf(:,31),wf(:,-3),wf(:,210))
  call vert_QA_V(wf(:,33),wf(:,-3),wf(:,211))
  call vert_QA_V(wf(:,0),wf(:,89),wf(:,212))
  call vert_QA_V(wf(:,0),wf(:,90),wf(:,213))
  call vert_QA_V(wf(:,18),wf(:,-2),wf(:,214))
  call vert_QA_V(wf(:,20),wf(:,-2),wf(:,215))
  call vert_QA_V(wf(:,-1),wf(:,95),wf(:,216))
  call vert_QA_V(wf(:,-1),wf(:,96),wf(:,217))
  call vert_QA_V(wf(:,18),wf(:,-3),wf(:,218))
  call vert_QA_V(wf(:,20),wf(:,-3),wf(:,219))
  call vert_QA_V(wf(:,-1),wf(:,89),wf(:,220))
  call vert_QA_V(wf(:,-1),wf(:,90),wf(:,221))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5))
  den(2) = 1 / (Q(5,34))
  den(3) = 1 / (Q(5,24))
  den(6) = 1 / (Q(5,7))
  den(9) = 1 / (Q(5,13))
  den(12) = 1 / (Q(5,48))
  den(14) = 1 / (Q(5,48) - MZ2)
  den(16) = 1 / (Q(5,50))
  den(21) = 1 / (Q(5,33))
  den(22) = 1 / (Q(5,6))
  den(27) = 1 / (Q(5,14))
  den(32) = 1 / (Q(5,49))
  den(37) = 1 / (Q(5,9))
  den(38) = 1 / (Q(5,20))
  den(41) = 1 / (Q(5,11))
  den(50) = 1 / (Q(5,10))
  den(63) = 1 / (Q(5,48) - MH2)
  den(74) = 1 / (Q(5,56))
  den(83) = 1 / (Q(5,52))
  den(103) = 1 / (Q(5,58))
  den(107) = 1 / (Q(5,29))
  den(110) = 1 / (Q(5,39))
  den(131) = 1 / (Q(5,30))
  den(135) = 1 / (Q(5,57))
  den(157) = 1 / (Q(5,54))
  den(163) = 1 / (Q(5,43))
  den(186) = 1 / (Q(5,53))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(3)*den(7)
  den(10) = den(1)*den(9)
  den(11) = den(2)*den(10)
  den(13) = den(7)*den(12)
  den(15) = den(7)*den(14)
  den(17) = den(12)*den(16)
  den(18) = den(1)*den(17)
  den(19) = den(14)*den(16)
  den(20) = den(1)*den(19)
  den(23) = den(21)*den(22)
  den(24) = den(3)*den(23)
  den(25) = den(6)*den(22)
  den(26) = den(3)*den(25)
  den(28) = den(22)*den(27)
  den(29) = den(21)*den(28)
  den(30) = den(12)*den(25)
  den(31) = den(14)*den(25)
  den(33) = den(12)*den(32)
  den(34) = den(22)*den(33)
  den(35) = den(14)*den(32)
  den(36) = den(22)*den(35)
  den(39) = den(2)*den(37)
  den(40) = den(38)*den(39)
  den(42) = den(37)*den(41)
  den(43) = den(38)*den(42)
  den(44) = den(9)*den(37)
  den(45) = den(2)*den(44)
  den(46) = den(12)*den(42)
  den(47) = den(14)*den(42)
  den(48) = den(17)*den(37)
  den(49) = den(19)*den(37)
  den(51) = den(21)*den(50)
  den(52) = den(38)*den(51)
  den(53) = den(41)*den(50)
  den(54) = den(38)*den(53)
  den(55) = den(27)*den(50)
  den(56) = den(21)*den(55)
  den(57) = den(12)*den(53)
  den(58) = den(14)*den(53)
  den(59) = den(33)*den(50)
  den(60) = den(35)*den(50)
  den(61) = den(1)*den(50)
  den(62) = den(22)*den(37)
  den(64) = den(61)*den(63)
  den(65) = den(14)*den(61)
  den(66) = den(2)*den(16)
  den(67) = den(1)*den(66)
  den(68) = den(62)*den(63)
  den(69) = den(14)*den(62)
  den(70) = den(21)*den(32)
  den(71) = den(22)*den(70)
  den(72) = den(37)*den(66)
  den(73) = den(50)*den(70)
  den(75) = den(3)*den(74)
  den(76) = den(1)*den(75)
  den(77) = den(10)*den(12)
  den(78) = den(10)*den(14)
  den(79) = den(12)*den(74)
  den(80) = den(1)*den(79)
  den(81) = den(14)*den(74)
  den(82) = den(1)*den(81)
  den(84) = den(38)*den(83)
  den(85) = den(37)*den(84)
  den(86) = den(12)*den(44)
  den(87) = den(14)*den(44)
  den(88) = den(12)*den(83)
  den(89) = den(37)*den(88)
  den(90) = den(14)*den(83)
  den(91) = den(37)*den(90)
  den(92) = den(22)*den(75)
  den(93) = den(22)*den(79)
  den(94) = den(22)*den(81)
  den(95) = den(12)*den(28)
  den(96) = den(14)*den(28)
  den(97) = den(50)*den(84)
  den(98) = den(50)*den(88)
  den(99) = den(50)*den(90)
  den(100) = den(12)*den(55)
  den(101) = den(14)*den(55)
  den(102) = den(2)*den(3)
  den(104) = den(102)*den(103)
  den(105) = den(1)*den(104)
  den(106) = den(1)*den(3)
  den(108) = den(106)*den(107)
  den(109) = den(2)*den(108)
  den(111) = den(4)*den(110)
  den(112) = den(3)*den(111)
  den(113) = den(1)**2
  den(114) = den(75)*den(113)
  den(115) = den(7)*den(75)
  den(116) = den(3)**2
  den(117) = den(7)*den(116)
  den(118) = den(66)*den(113)
  den(119) = den(2)**2
  den(120) = den(10)*den(119)
  den(121) = den(10)*den(66)
  den(122) = den(79)*den(113)
  den(123) = den(81)*den(113)
  den(124) = den(17)*den(113)
  den(125) = den(19)*den(113)
  den(126) = den(7)*den(79)
  den(127) = den(7)*den(81)
  den(128) = den(10)*den(17)
  den(129) = den(10)*den(19)
  den(130) = den(3)*den(22)
  den(132) = den(130)*den(131)
  den(133) = den(21)*den(132)
  den(134) = den(3)*den(21)
  den(136) = den(134)*den(135)
  den(137) = den(22)*den(136)
  den(138) = den(23)*den(110)
  den(139) = den(3)*den(138)
  den(140) = den(22)**2
  den(141) = den(75)*den(140)
  den(142) = den(25)*den(75)
  den(143) = den(25)*den(116)
  den(144) = den(21)**2
  den(145) = den(28)*den(144)
  den(146) = den(70)*den(140)
  den(147) = den(28)*den(70)
  den(148) = den(79)*den(140)
  den(149) = den(81)*den(140)
  den(150) = den(25)*den(79)
  den(151) = den(25)*den(81)
  den(152) = den(28)*den(33)
  den(153) = den(28)*den(35)
  den(154) = den(33)*den(140)
  den(155) = den(35)*den(140)
  den(156) = den(2)*den(38)
  den(158) = den(156)*den(157)
  den(159) = den(37)*den(158)
  den(160) = den(37)*den(38)
  den(161) = den(107)*den(160)
  den(162) = den(2)*den(161)
  den(164) = den(39)*den(163)
  den(165) = den(38)*den(164)
  den(166) = den(37)**2
  den(167) = den(84)*den(166)
  den(168) = den(42)*den(84)
  den(169) = den(38)**2
  den(170) = den(42)*den(169)
  den(171) = den(66)*den(166)
  den(172) = den(44)*den(119)
  den(173) = den(44)*den(66)
  den(174) = den(88)*den(166)
  den(175) = den(90)*den(166)
  den(176) = den(17)*den(166)
  den(177) = den(19)*den(166)
  den(178) = den(42)*den(88)
  den(179) = den(42)*den(90)
  den(180) = den(17)*den(44)
  den(181) = den(19)*den(44)
  den(182) = den(38)*den(50)
  den(183) = den(131)*den(182)
  den(184) = den(21)*den(183)
  den(185) = den(21)*den(38)
  den(187) = den(185)*den(186)
  den(188) = den(50)*den(187)
  den(189) = den(51)*den(163)
  den(190) = den(38)*den(189)
  den(191) = den(50)**2
  den(192) = den(84)*den(191)
  den(193) = den(53)*den(84)
  den(194) = den(53)*den(169)
  den(195) = den(55)*den(144)
  den(196) = den(70)*den(191)
  den(197) = den(55)*den(70)
  den(198) = den(88)*den(191)
  den(199) = den(90)*den(191)
  den(200) = den(53)*den(88)
  den(201) = den(53)*den(90)
  den(202) = den(33)*den(55)
  den(203) = den(35)*den(55)
  den(204) = den(33)*den(191)
  den(205) = den(35)*den(191)
  den(206) = den(7)*den(110)
  den(207) = den(10)*den(107)
  den(208) = den(25)*den(110)
  den(209) = den(28)*den(131)
  den(210) = den(42)*den(163)
  den(211) = den(44)*den(107)
  den(212) = den(53)*den(163)
  den(213) = den(55)*den(131)
  den(214) = den(84)*den(186)
  den(215) = den(84)*den(157)
  den(216) = den(75)*den(135)
  den(217) = den(75)*den(103)
  den(218) = den(70)*den(186)
  den(219) = den(70)*den(135)
  den(220) = den(66)*den(157)
  den(221) = den(66)*den(103)
  den(222) = den(33)*den(186)
  den(223) = den(35)*den(186)
  den(224) = den(88)*den(186)
  den(225) = den(90)*den(186)
  den(226) = den(33)*den(135)
  den(227) = den(35)*den(135)
  den(228) = den(79)*den(135)
  den(229) = den(81)*den(135)
  den(230) = den(17)*den(157)
  den(231) = den(19)*den(157)
  den(232) = den(88)*den(157)
  den(233) = den(90)*den(157)
  den(234) = den(17)*den(103)
  den(235) = den(19)*den(103)
  den(236) = den(79)*den(103)
  den(237) = den(81)*den(103)
  den(238) = den(1)*den(50)*den(63)
  den(239) = den(1)*den(12)*den(50)
  den(240) = den(1)*den(14)*den(50)
  den(241) = den(1)*den(2)*den(3)
  den(242) = den(1)*den(12)
  den(243) = den(1)*den(14)
  den(244) = den(22)*den(37)*den(63)
  den(245) = den(12)*den(22)*den(37)
  den(246) = den(14)*den(22)*den(37)
  den(247) = den(3)*den(21)*den(22)
  den(248) = den(12)*den(22)
  den(249) = den(14)*den(22)
  den(250) = den(2)*den(37)*den(38)
  den(251) = den(12)*den(37)
  den(252) = den(14)*den(37)
  den(253) = den(21)*den(38)*den(50)
  den(254) = den(12)*den(50)
  den(255) = den(14)*den(50)
  den(256) = den(3)*den(206)
  den(257) = den(1)*den(217)
  den(258) = den(2)*den(207)
  den(259) = den(1)*den(221)
  den(260) = den(1)*den(234)
  den(261) = den(1)*den(235)
  den(262) = den(1)*den(236)
  den(263) = den(1)*den(237)
  den(264) = den(3)*den(208)
  den(265) = den(22)*den(216)
  den(266) = den(22)*den(219)
  den(267) = den(21)*den(209)
  den(268) = den(22)*den(226)
  den(269) = den(22)*den(227)
  den(270) = den(22)*den(228)
  den(271) = den(22)*den(229)
  den(272) = den(38)*den(210)
  den(273) = den(37)*den(215)
  den(274) = den(2)*den(211)
  den(275) = den(37)*den(220)
  den(276) = den(37)*den(230)
  den(277) = den(37)*den(231)
  den(278) = den(37)*den(232)
  den(279) = den(37)*den(233)
  den(280) = den(38)*den(212)
  den(281) = den(50)*den(214)
  den(282) = den(50)*den(218)
  den(283) = den(21)*den(213)
  den(284) = den(50)*den(222)
  den(285) = den(50)*den(223)
  den(286) = den(50)*den(224)
  den(287) = den(50)*den(225)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(198)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,11),wf(:,12)) * den(11)
  A(4) = cont_QA(wf(:,9),wf(:,14)) * den(13)
  A(5) = cont_QA(wf(:,9),wf(:,16)) * den(15)
  A(6) = cont_QA(wf(:,10),wf(:,18)) * den(18)
  A(7) = cont_QA(wf(:,10),wf(:,20)) * den(20)
  A(8) = cont_QA(wf(:,5),wf(:,24)) * den(24)
  A(9) = cont_QA(wf(:,8),wf(:,26)) * den(26)
  A(10) = cont_QA(wf(:,28),wf(:,29)) * den(29)
  A(11) = cont_QA(wf(:,14),wf(:,26)) * den(30)
  A(12) = cont_QA(wf(:,16),wf(:,26)) * den(31)
  A(13) = cont_QA(wf(:,27),wf(:,31)) * den(34)
  A(14) = cont_QA(wf(:,27),wf(:,33)) * den(36)
  A(15) = cont_QA(wf(:,36),wf(:,37)) * den(40)
  A(16) = cont_QA(wf(:,39),wf(:,40)) * den(43)
  A(17) = cont_QA(wf(:,11),wf(:,42)) * den(45)
  A(18) = cont_QA(wf(:,40),wf(:,43)) * den(46)
  A(19) = cont_QA(wf(:,40),wf(:,44)) * den(47)
  A(20) = cont_QA(wf(:,18),wf(:,41)) * den(48)
  A(21) = cont_QA(wf(:,20),wf(:,41)) * den(49)
  A(22) = cont_QA(wf(:,36),wf(:,46)) * den(52)
  A(23) = cont_QA(wf(:,39),wf(:,48)) * den(54)
  A(24) = cont_QA(wf(:,28),wf(:,50)) * den(56)
  A(25) = cont_QA(wf(:,43),wf(:,48)) * den(57)
  A(26) = cont_QA(wf(:,44),wf(:,48)) * den(58)
  A(27) = cont_QA(wf(:,31),wf(:,49)) * den(59)
  A(28) = cont_QA(wf(:,33),wf(:,49)) * den(60)

  A(29) = cont_VV(wf(:,45),wf(:,51)) * den(61)
  A(30) = cont_VV(wf(:,22),wf(:,52)) * den(62)
  A(31) = cont_SS(wf(:,53),wf(:,54)) * den(64)
  A(32) = cont_VV(wf(:,15),wf(:,55)) * den(65)
  A(33) = cont_QA(wf(:,5),wf(:,56)) * den(5)
  A(34) = cont_QA(wf(:,9),wf(:,57)) * den(8)
  A(35) = cont_QA(wf(:,12),wf(:,58)) * den(11)
  A(36) = cont_QA(wf(:,59),wf(:,60)) * den(67)
  A(37) = cont_QA(wf(:,9),wf(:,61)) * den(13)
  A(38) = cont_QA(wf(:,9),wf(:,62)) * den(15)
  A(39) = cont_QA(wf(:,18),wf(:,59)) * den(18)
  A(40) = cont_QA(wf(:,20),wf(:,59)) * den(20)
  A(41) = cont_QA(wf(:,6),wf(:,64)) * den(5)
  A(42) = cont_QA(wf(:,9),wf(:,65)) * den(8)
  A(43) = cont_SS(wf(:,53),wf(:,66)) * den(68)
  A(44) = cont_VV(wf(:,15),wf(:,67)) * den(69)
  A(45) = cont_QA(wf(:,5),wf(:,68)) * den(24)
  A(46) = cont_QA(wf(:,26),wf(:,57)) * den(26)
  A(47) = cont_QA(wf(:,29),wf(:,69)) * den(29)
  A(48) = cont_QA(wf(:,70),wf(:,71)) * den(71)
  A(49) = cont_QA(wf(:,26),wf(:,61)) * den(30)
  A(50) = cont_QA(wf(:,26),wf(:,62)) * den(31)
  A(51) = cont_QA(wf(:,31),wf(:,70)) * den(34)
  A(52) = cont_QA(wf(:,33),wf(:,70)) * den(36)
  A(53) = cont_QA(wf(:,24),wf(:,64)) * den(24)
  A(54) = cont_QA(wf(:,26),wf(:,65)) * den(26)
  A(55) = cont_QA(wf(:,36),wf(:,72)) * den(40)
  A(56) = cont_QA(wf(:,40),wf(:,73)) * den(43)
  A(57) = cont_QA(wf(:,42),wf(:,58)) * den(45)
  A(58) = cont_QA(wf(:,36),wf(:,74)) * den(52)
  A(59) = cont_QA(wf(:,48),wf(:,73)) * den(54)
  A(60) = cont_QA(wf(:,50),wf(:,69)) * den(56)
  A(61) = cont_QA(wf(:,60),wf(:,75)) * den(72)
  A(62) = cont_QA(wf(:,40),wf(:,76)) * den(46)
  A(63) = cont_QA(wf(:,40),wf(:,77)) * den(47)
  A(64) = cont_QA(wf(:,18),wf(:,75)) * den(48)
  A(65) = cont_QA(wf(:,20),wf(:,75)) * den(49)
  A(66) = cont_QA(wf(:,37),wf(:,79)) * den(40)
  A(67) = cont_QA(wf(:,40),wf(:,80)) * den(43)
  A(68) = cont_QA(wf(:,71),wf(:,81)) * den(73)
  A(69) = cont_QA(wf(:,48),wf(:,76)) * den(57)
  A(70) = cont_QA(wf(:,48),wf(:,77)) * den(58)
  A(71) = cont_QA(wf(:,31),wf(:,81)) * den(59)
  A(72) = cont_QA(wf(:,33),wf(:,81)) * den(60)
  A(73) = cont_QA(wf(:,46),wf(:,79)) * den(52)
  A(74) = cont_QA(wf(:,48),wf(:,80)) * den(54)
  A(75) = cont_QA(wf(:,82),wf(:,83)) * den(76)
  A(76) = cont_QA(wf(:,5),wf(:,86)) * den(5)
  A(77) = cont_QA(wf(:,12),wf(:,87)) * den(77)
  A(78) = cont_QA(wf(:,12),wf(:,88)) * den(78)
  A(79) = cont_QA(wf(:,82),wf(:,89)) * den(80)
  A(80) = cont_QA(wf(:,82),wf(:,90)) * den(82)
  A(81) = cont_QA(wf(:,12),wf(:,91)) * den(11)
  A(82) = cont_QA(wf(:,92),wf(:,93)) * den(85)
  A(83) = cont_QA(wf(:,36),wf(:,94)) * den(40)
  A(84) = cont_QA(wf(:,42),wf(:,87)) * den(86)
  A(85) = cont_QA(wf(:,42),wf(:,88)) * den(87)
  A(86) = cont_QA(wf(:,92),wf(:,95)) * den(89)
  A(87) = cont_QA(wf(:,92),wf(:,96)) * den(91)
  A(88) = cont_QA(wf(:,42),wf(:,91)) * den(45)
  A(89) = cont_QA(wf(:,36),wf(:,98)) * den(52)
  A(90) = cont_QA(wf(:,39),wf(:,100)) * den(54)
  A(91) = cont_QA(wf(:,28),wf(:,102)) * den(56)
  A(92) = cont_QA(wf(:,31),wf(:,101)) * den(59)
  A(93) = cont_QA(wf(:,33),wf(:,101)) * den(60)
  A(94) = cont_QA(wf(:,43),wf(:,100)) * den(57)
  A(95) = cont_QA(wf(:,44),wf(:,100)) * den(58)
  A(96) = cont_QA(wf(:,5),wf(:,104)) * den(24)
  A(97) = cont_QA(wf(:,8),wf(:,106)) * den(26)
  A(98) = cont_QA(wf(:,28),wf(:,108)) * den(29)
  A(99) = cont_QA(wf(:,31),wf(:,107)) * den(34)
  A(100) = cont_QA(wf(:,33),wf(:,107)) * den(36)
  A(101) = cont_QA(wf(:,14),wf(:,106)) * den(30)
  A(102) = cont_QA(wf(:,16),wf(:,106)) * den(31)
  A(103) = cont_QA(wf(:,83),wf(:,109)) * den(92)
  A(104) = cont_QA(wf(:,5),wf(:,112)) * den(24)
  A(105) = cont_QA(wf(:,89),wf(:,109)) * den(93)
  A(106) = cont_QA(wf(:,90),wf(:,109)) * den(94)
  A(107) = cont_QA(wf(:,29),wf(:,113)) * den(95)
  A(108) = cont_QA(wf(:,29),wf(:,114)) * den(96)
  A(109) = cont_QA(wf(:,29),wf(:,115)) * den(29)
  A(110) = cont_QA(wf(:,93),wf(:,116)) * den(97)
  A(111) = cont_QA(wf(:,36),wf(:,117)) * den(52)
  A(112) = cont_QA(wf(:,95),wf(:,116)) * den(98)
  A(113) = cont_QA(wf(:,96),wf(:,116)) * den(99)
  A(114) = cont_QA(wf(:,50),wf(:,113)) * den(100)
  A(115) = cont_QA(wf(:,50),wf(:,114)) * den(101)
  A(116) = cont_QA(wf(:,50),wf(:,115)) * den(56)
  A(117) = cont_QA(wf(:,36),wf(:,119)) * den(40)
  A(118) = cont_QA(wf(:,39),wf(:,121)) * den(43)
  A(119) = cont_QA(wf(:,11),wf(:,123)) * den(45)
  A(120) = cont_QA(wf(:,43),wf(:,121)) * den(46)
  A(121) = cont_QA(wf(:,44),wf(:,121)) * den(47)
  A(122) = cont_QA(wf(:,18),wf(:,122)) * den(48)
  A(123) = cont_QA(wf(:,20),wf(:,122)) * den(49)
  A(124) = cont_QA(wf(:,5),wf(:,125)) * den(5)
  A(125) = cont_QA(wf(:,8),wf(:,127)) * den(8)
  A(126) = cont_QA(wf(:,11),wf(:,129)) * den(11)
  A(127) = cont_QA(wf(:,14),wf(:,127)) * den(13)
  A(128) = cont_QA(wf(:,16),wf(:,127)) * den(15)
  A(129) = cont_QA(wf(:,18),wf(:,128)) * den(18)
  A(130) = cont_QA(wf(:,20),wf(:,128)) * den(20)
  A(131) = cont_VV(wf(:,130),wf(:,131)) * den(105)
  A(132) = cont_QA(wf(:,133),wf(:,134)) * den(109)
  A(133) = cont_QA(wf(:,135),wf(:,136)) * den(112)
  A(134) = cont_QA(wf(:,83),wf(:,137)) * den(114)
  A(135) = cont_QA(wf(:,83),wf(:,138)) * den(115)
  A(136) = cont_QA(wf(:,9),wf(:,140)) * den(117)
  A(137) = cont_QA(wf(:,60),wf(:,141)) * den(118)
  A(138) = cont_QA(wf(:,12),wf(:,143)) * den(120)
  A(139) = cont_QA(wf(:,60),wf(:,144)) * den(121)
  A(140) = cont_QA(wf(:,89),wf(:,137)) * den(122)
  A(141) = cont_QA(wf(:,90),wf(:,137)) * den(123)
  A(142) = cont_QA(wf(:,18),wf(:,141)) * den(124)
  A(143) = cont_QA(wf(:,20),wf(:,141)) * den(125)
  A(144) = cont_QA(wf(:,89),wf(:,138)) * den(126)
  A(145) = cont_QA(wf(:,90),wf(:,138)) * den(127)
  A(146) = cont_QA(wf(:,12),wf(:,145)) * den(128)
  A(147) = cont_QA(wf(:,12),wf(:,146)) * den(129)
  A(148) = cont_QA(wf(:,148),wf(:,149)) * den(133)
  A(149) = cont_VV(wf(:,150),wf(:,151)) * den(137)
  A(150) = cont_QA(wf(:,135),wf(:,152)) * den(139)
  A(151) = cont_QA(wf(:,83),wf(:,153)) * den(141)
  A(152) = cont_QA(wf(:,83),wf(:,154)) * den(142)
  A(153) = cont_QA(wf(:,26),wf(:,140)) * den(143)
  A(154) = cont_QA(wf(:,29),wf(:,156)) * den(145)
  A(155) = cont_QA(wf(:,71),wf(:,157)) * den(146)
  A(156) = cont_QA(wf(:,71),wf(:,158)) * den(147)
  A(157) = cont_QA(wf(:,89),wf(:,153)) * den(148)
  A(158) = cont_QA(wf(:,90),wf(:,153)) * den(149)
  A(159) = cont_QA(wf(:,89),wf(:,154)) * den(150)
  A(160) = cont_QA(wf(:,90),wf(:,154)) * den(151)
  A(161) = cont_QA(wf(:,29),wf(:,159)) * den(152)
  A(162) = cont_QA(wf(:,29),wf(:,160)) * den(153)
  A(163) = cont_QA(wf(:,31),wf(:,157)) * den(154)
  A(164) = cont_QA(wf(:,33),wf(:,157)) * den(155)
  A(165) = cont_VV(wf(:,161),wf(:,162)) * den(159)
  A(166) = cont_QA(wf(:,133),wf(:,164)) * den(162)
  A(167) = cont_QA(wf(:,165),wf(:,166)) * den(165)
  A(168) = cont_QA(wf(:,93),wf(:,167)) * den(167)
  A(169) = cont_QA(wf(:,93),wf(:,168)) * den(168)
  A(170) = cont_QA(wf(:,40),wf(:,170)) * den(170)
  A(171) = cont_QA(wf(:,60),wf(:,171)) * den(171)
  A(172) = cont_QA(wf(:,42),wf(:,143)) * den(172)
  A(173) = cont_QA(wf(:,60),wf(:,172)) * den(173)
  A(174) = cont_QA(wf(:,95),wf(:,167)) * den(174)
  A(175) = cont_QA(wf(:,96),wf(:,167)) * den(175)
  A(176) = cont_QA(wf(:,18),wf(:,171)) * den(176)
  A(177) = cont_QA(wf(:,20),wf(:,171)) * den(177)
  A(178) = cont_QA(wf(:,95),wf(:,168)) * den(178)
  A(179) = cont_QA(wf(:,96),wf(:,168)) * den(179)
  A(180) = cont_QA(wf(:,42),wf(:,145)) * den(180)
  A(181) = cont_QA(wf(:,42),wf(:,146)) * den(181)
  A(182) = cont_QA(wf(:,148),wf(:,174)) * den(184)
  A(183) = cont_VV(wf(:,175),wf(:,176)) * den(188)
  A(184) = cont_QA(wf(:,165),wf(:,177)) * den(190)
  A(185) = cont_QA(wf(:,93),wf(:,178)) * den(192)
  A(186) = cont_QA(wf(:,93),wf(:,179)) * den(193)
  A(187) = cont_QA(wf(:,48),wf(:,170)) * den(194)
  A(188) = cont_QA(wf(:,50),wf(:,156)) * den(195)
  A(189) = cont_QA(wf(:,71),wf(:,180)) * den(196)
  A(190) = cont_QA(wf(:,71),wf(:,181)) * den(197)
  A(191) = cont_QA(wf(:,95),wf(:,178)) * den(198)
  A(192) = cont_QA(wf(:,96),wf(:,178)) * den(199)
  A(193) = cont_QA(wf(:,95),wf(:,179)) * den(200)
  A(194) = cont_QA(wf(:,96),wf(:,179)) * den(201)
  A(195) = cont_QA(wf(:,50),wf(:,159)) * den(202)
  A(196) = cont_QA(wf(:,50),wf(:,160)) * den(203)
  A(197) = cont_QA(wf(:,31),wf(:,180)) * den(204)
  A(198) = cont_QA(wf(:,33),wf(:,180)) * den(205)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(198)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(11)+A(13)+A(18)+A(20))*f(1))/6._/**/REALKIND+((A(4)+A(6)+A(25)+A(27))*f(1))/2._/**/REALKIND+((-A(8)-A(9)-A(10)-A(15) &
       -A(16)-A(17))*f(6))/6._/**/REALKIND+((-A(1)-A(2)-A(3)-A(22)-A(23)-A(24))*f(6))/2._/**/REALKIND+((-A(12)-A(14)-A(19) &
       -A(21))*f(10))/6._/**/REALKIND+((-A(5)-A(7)-A(26)-A(28))*f(10))/2._/**/REALKIND
  M1(2) = ((-A(11)-A(13)-A(18)-A(20))*f(1))/2._/**/REALKIND+((-A(4)-A(6)-A(25)-A(27))*f(1))/6._/**/REALKIND+((A(8)+A(9)+A(10) &
       +A(15)+A(16)+A(17))*f(6))/2._/**/REALKIND+((A(1)+A(2)+A(3)+A(22)+A(23)+A(24))*f(6))/6._/**/REALKIND+((A(12)+A(14)+A(19) &
       +A(21))*f(10))/2._/**/REALKIND+((A(5)+A(7)+A(26)+A(28))*f(10))/6._/**/REALKIND

  M2(1) = ((-A(157)-A(159)-A(161)-A(163)-A(174)-A(176)-A(178)-A(180))*f(2))/6._/**/REALKIND+((-A(140)-A(142)-A(144)-A(146)-A(191) &
       -A(193)-A(195)-A(197))*f(2))/2._/**/REALKIND+((A(51)+A(64)+A(86)+A(99)+A(101)+A(105)+A(120)+A(122))*f(3))/6._/**/REALKIND &
       +((A(39)+A(71)+A(79)+A(92)+A(94)+A(112)+A(127)+A(129))*f(3))/2._/**/REALKIND+((A(49)+A(62)+A(84) &
       +A(107))*f(4))/6._/**/REALKIND+((A(37)+A(69)+A(77)+A(114))*f(4))/2._/**/REALKIND-(A(29)*f(5))/2._/**/REALKIND &
       -(A(30)*f(5))/6._/**/REALKIND+((A(148)+A(149)+A(150)+A(151)+A(152)+A(153)+A(154)+A(155)+A(156)+A(165)+A(166)+A(167)+A(168) &
       +A(169)+A(170)+A(171)+A(172)+A(173))*f(7))/6._/**/REALKIND+((A(131)+A(132)+A(133)+A(134)+A(135)+A(136)+A(137)+A(138)+A(139) &
       +A(182)+A(183)+A(184)+A(185)+A(186)+A(187)+A(188)+A(189)+A(190))*f(7))/2._/**/REALKIND+((-A(45)-A(48)-A(55)-A(61)-A(82) &
       -A(96)-A(97)-A(98)-A(103)-A(117)-A(118)-A(119))*f(8))/6._/**/REALKIND+((-A(33)-A(36)-A(58)-A(68)-A(75)-A(89)-A(90)-A(91) &
       -A(110)-A(124)-A(125)-A(126))*f(8))/2._/**/REALKIND+((-A(46)-A(47)-A(53)-A(54)-A(56)-A(57)-A(66)-A(67)-A(83)-A(88)-A(104) &
       -A(109))*f(9))/6._/**/REALKIND+((-A(34)-A(35)-A(41)-A(42)-A(59)-A(60)-A(73)-A(74)-A(76)-A(81)-A(111) &
       -A(116))*f(9))/2._/**/REALKIND+((A(158)+A(160)+A(162)+A(164)+A(175)+A(177)+A(179)+A(181))*f(11))/6._/**/REALKIND+((A(141) &
       +A(143)+A(145)+A(147)+A(192)+A(194)+A(196)+A(198))*f(11))/2._/**/REALKIND+((-A(52)-A(65)-A(87)-A(100)-A(102)-A(106)-A(121) &
       -A(123))*f(12))/6._/**/REALKIND+((-A(40)-A(72)-A(80)-A(93)-A(95)-A(113)-A(128)-A(130))*f(12))/2._/**/REALKIND+((-A(50) &
       -A(63)-A(85)-A(108))*f(13))/6._/**/REALKIND+((-A(38)-A(70)-A(78)-A(115))*f(13))/2._/**/REALKIND &
       +(A(32)*f(14))/2._/**/REALKIND+(A(44)*f(14))/6._/**/REALKIND+(A(31)*f(15))/2._/**/REALKIND+(A(43)*f(15))/6._/**/REALKIND
  M2(2) = ((A(157)+A(159)+A(161)+A(163)+A(174)+A(176)+A(178)+A(180))*f(2))/2._/**/REALKIND+((A(140)+A(142)+A(144)+A(146)+A(191) &
       +A(193)+A(195)+A(197))*f(2))/6._/**/REALKIND+((-A(51)-A(64)-A(86)-A(99)-A(101)-A(105)-A(120)-A(122))*f(3))/2._/**/REALKIND &
       +((-A(39)-A(71)-A(79)-A(92)-A(94)-A(112)-A(127)-A(129))*f(3))/6._/**/REALKIND+((-A(49)-A(62)-A(84) &
       -A(107))*f(4))/2._/**/REALKIND+((-A(37)-A(69)-A(77)-A(114))*f(4))/6._/**/REALKIND+(A(29)*f(5))/6._/**/REALKIND &
       +(A(30)*f(5))/2._/**/REALKIND+((-A(148)-A(149)-A(150)-A(151)-A(152)-A(153)-A(154)-A(155)-A(156)-A(165)-A(166)-A(167)-A(168) &
       -A(169)-A(170)-A(171)-A(172)-A(173))*f(7))/2._/**/REALKIND+((-A(131)-A(132)-A(133)-A(134)-A(135)-A(136)-A(137)-A(138) &
       -A(139)-A(182)-A(183)-A(184)-A(185)-A(186)-A(187)-A(188)-A(189)-A(190))*f(7))/6._/**/REALKIND+((A(45)+A(48)+A(55)+A(61) &
       +A(82)+A(96)+A(97)+A(98)+A(103)+A(117)+A(118)+A(119))*f(8))/2._/**/REALKIND+((A(33)+A(36)+A(58)+A(68)+A(75)+A(89)+A(90) &
       +A(91)+A(110)+A(124)+A(125)+A(126))*f(8))/6._/**/REALKIND+((A(46)+A(47)+A(53)+A(54)+A(56)+A(57)+A(66)+A(67)+A(83)+A(88) &
       +A(104)+A(109))*f(9))/2._/**/REALKIND+((A(34)+A(35)+A(41)+A(42)+A(59)+A(60)+A(73)+A(74)+A(76)+A(81)+A(111) &
       +A(116))*f(9))/6._/**/REALKIND+((-A(158)-A(160)-A(162)-A(164)-A(175)-A(177)-A(179)-A(181))*f(11))/2._/**/REALKIND+((-A(141) &
       -A(143)-A(145)-A(147)-A(192)-A(194)-A(196)-A(198))*f(11))/6._/**/REALKIND+((A(52)+A(65)+A(87)+A(100)+A(102)+A(106)+A(121) &
       +A(123))*f(12))/2._/**/REALKIND+((A(40)+A(72)+A(80)+A(93)+A(95)+A(113)+A(128)+A(130))*f(12))/6._/**/REALKIND+((A(50)+A(63) &
       +A(85)+A(108))*f(13))/2._/**/REALKIND+((A(38)+A(70)+A(78)+A(115))*f(13))/6._/**/REALKIND-(A(32)*f(14))/6._/**/REALKIND &
       -(A(44)*f(14))/2._/**/REALKIND-(A(31)*f(15))/6._/**/REALKIND-(A(43)*f(15))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppwwjj_dddxdxwwx_1_/**/REALKIND
