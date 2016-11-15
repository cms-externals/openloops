
module ol_colourmatrix_ppwwjj_ddxbbxwwx_1_/**/REALKIND
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
end module ol_colourmatrix_ppwwjj_ddxbbxwwx_1_/**/REALKIND



module ol_forced_parameters_ppwwjj_ddxbbxwwx_1_/**/REALKIND
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
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppwwjj_ddxbbxwwx_1_/**/REALKIND

module ol_loop_ppwwjj_ddxbbxwwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(49), c(72)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:157)
  ! denominators
  complex(REALKIND), save :: den(176)
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
    f( 3) = (CI*countertermnorm*ctGbb*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 4) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 5) = (CI*countertermnorm*ctVbb*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 6) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 7) = CI*countertermnorm*ctWWGG*eQED**2*gQCD**4
    f( 8) = (CI*eQED**2*gQCD**2)/(2._/**/REALKIND*sw**2)
    f( 9) = (CI*countertermnorm*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(10) = (CI*countertermnorm*ctGbb*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(11) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(12) = (CI*countertermnorm*ctGtt*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(13) = (CI*countertermnorm*ctVbt*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(14) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(15) = (CI*eQED**2*gQCD**2*MB)/(2._/**/REALKIND*sw**2)
    f(16) = (CI*countertermnorm*eQED**2*gQCD**4*MB)/(2._/**/REALKIND*sw**2)
    f(17) = (CI*countertermnorm*ctGbb*eQED**2*gQCD**4*MB)/(2._/**/REALKIND*sw**2)
    f(18) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4*MB)/(2._/**/REALKIND*sw**2)
    f(19) = (CI*countertermnorm*ctSbb*eQED**2*gQCD**4*MB)/(2._/**/REALKIND*sw**2)
    f(20) = (CI*cw*eQED**2*gQCD**2)/sw
    f(21) = (CI*countertermnorm*cw*eQED**2*gQCD**4)/sw
    f(22) = (CI*countertermnorm*ctGbb*cw*eQED**2*gQCD**4)/sw
    f(23) = (CI*countertermnorm*ctGqq*cw*eQED**2*gQCD**4)/sw
    f(24) = (CI*countertermnorm*ctVbb*cw*eQED**2*gQCD**4)/sw
    f(25) = (CI*countertermnorm*ctVqq*cw*eQED**2*gQCD**4)/sw
    f(26) = (countertermnorm*ctZGG*cw*eQED**2*gQCD**4)/sw
    f(27) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MW)/sw
    f(28) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(29) = (eQED**2*gQCD**4*integralnorm*SwB)/6._/**/REALKIND
    f(30) = (eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(31) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(32) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*4._/**/REALKIND)
    f(33) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(34) = (CI*eQED**2*gQCD**4*integralnorm*MB*SwB)/(2._/**/REALKIND*sw**2)
    f(35) = (eQED**2*gQCD**4*integralnorm*MB*SwB)/(sw**2*4._/**/REALKIND)
    f(36) = (eQED**2*gQCD**4*integralnorm*MB*SwB)/(sw**2*2._/**/REALKIND)
    f(37) = (CI*cw*eQED**2*gQCD**4*integralnorm*SwB)/sw
    f(38) = (cw*eQED**2*gQCD**4*integralnorm*SwB)/(sw*2._/**/REALKIND)
    f(39) = (cw*eQED**2*gQCD**4*integralnorm*SwB)/sw
    f(40) = (eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(41) = (2*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(42) = (4*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(43) = (eQED**2*gQCD**4*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(44) = (eQED**2*gQCD**4*integralnorm*SwF)/sw**2
    f(45) = (eQED**2*gQCD**4*integralnorm*MB*SwF)/(sw**2*2._/**/REALKIND)
    f(46) = (eQED**2*gQCD**4*integralnorm*MB*SwF)/sw**2
    f(47) = (eQED**2*gQCD**4*integralnorm*MT*SwF)/(sw**2*2._/**/REALKIND)
    f(48) = (cw*eQED**2*gQCD**4*integralnorm*SwF)/sw
    f(49) = (2*cw*eQED**2*gQCD**4*integralnorm*SwF)/sw

  c = [ 9*CI*f(28), 27*CI*f(28), 18*f(29), 54*f(29), f(30), 3*f(30), 6*f(30), 8*f(30), 10*f(30), 18*f(30), 21*f(30), 24*f(30) &
    , 54*f(30), 9*CI*f(31), 27*CI*f(31), 18*f(32), 54*f(32), f(33), 3*f(33), 6*f(33), 8*f(33), 10*f(33), 18*f(33), 21*f(33) &
    , 24*f(33), 54*f(33), 9*CI*f(34), 27*CI*f(34), 18*f(35), 54*f(35), f(36), 3*f(36), 6*f(36), 8*f(36), 10*f(36), 18*f(36) &
    , 21*f(36), 24*f(36), 54*f(36), 9*CI*f(37), 27*CI*f(37), 18*f(38), 54*f(38), f(39), 3*f(39), 6*f(39), 8*f(39), 10*f(39) &
    , 18*f(39), 21*f(39), 24*f(39), 54*f(39), 3*f(40), 9*f(40), 3*f(41), 9*f(41), 3*f(42), 9*f(42), 3*f(43), 9*f(43), 3*f(44) &
    , 9*f(44), 3*f(45), 9*f(45), 3*f(46), 9*f(46), 3*f(47), 9*f(47), 3*f(48), 9*f(48), 3*f(49), 9*f(49) ]
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
  complex(REALKIND) :: A(111)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rMB, H(3), wf(:,-2))
  call wf_A(P(:,4), rMB, H(4), wf(:,-3))
  call wf_V(P(:,5), rMW, H(5), wf(:,-4))
  call wf_V(P(:,6), rMW, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_WQ_A(wf(:,-5),wf(:,-2),wf(:,2))
  call vert_AW_Q(wf(:,-3),wf(:,-4),wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,36),MT,1_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,24),MT,1_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,6))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,7))
  call vert_AW_Q(wf(:,5),wf(:,-5),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,7),MB,1_intkind1,wf(:,9))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,10))
  call vert_WQ_A(wf(:,-4),wf(:,4),wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,11),MB,1_intkind1,wf(:,12))
  call vert_VV_S(wf(:,-4),wf(:,-5),wf(:,13))
  call vert_SA_Q(gH,wf(:,13),wf(:,-3),wf(:,14))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-4),Q(:,16),wf(:,15))
  call vert_AV_Q(wf(:,-3),wf(:,15),wf(:,16))
  call prop_W_W(wf(:,15),Q(:,48),MZ,1_intkind1,wf(:,17))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,17),wf(:,18))
  call vert_QS_A(gH,wf(:,-2),wf(:,13),wf(:,19))
  call prop_Q_A(wf(:,19),Q(:,52),MB,1_intkind1,wf(:,20))
  call vert_VQ_A(wf(:,15),wf(:,-2),wf(:,21))
  call prop_Q_A(wf(:,21),Q(:,52),MB,1_intkind1,wf(:,22))
  call vert_ZQ_A(gZd,wf(:,17),wf(:,-2),wf(:,23))
  call prop_Q_A(wf(:,23),Q(:,52),MB,1_intkind1,wf(:,24))
  call vert_WQ_A(wf(:,-5),wf(:,0),wf(:,25))
  call vert_AW_Q(wf(:,-1),wf(:,-4),wf(:,26))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,27))
  call prop_Q_A(wf(:,25),Q(:,33),ZERO,0_intkind1,wf(:,28))
  call prop_A_Q(wf(:,26),Q(:,18),ZERO,0_intkind1,wf(:,29))
  call vert_QA_V(wf(:,28),wf(:,29),wf(:,30))
  call vert_VQ_A(wf(:,27),wf(:,0),wf(:,31))
  call vert_AW_Q(wf(:,29),wf(:,-5),wf(:,32))
  call prop_Q_A(wf(:,31),Q(:,13),ZERO,0_intkind1,wf(:,33))
  call vert_AV_Q(wf(:,-1),wf(:,27),wf(:,34))
  call vert_WQ_A(wf(:,-4),wf(:,28),wf(:,35))
  call prop_A_Q(wf(:,34),Q(:,14),ZERO,0_intkind1,wf(:,36))
  call vert_AV_Q(wf(:,-1),wf(:,15),wf(:,37))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,17),wf(:,38))
  call vert_VQ_A(wf(:,15),wf(:,0),wf(:,39))
  call prop_Q_A(wf(:,39),Q(:,49),ZERO,0_intkind1,wf(:,40))
  call vert_ZQ_A(gZd,wf(:,17),wf(:,0),wf(:,41))
  call prop_Q_A(wf(:,41),Q(:,49),ZERO,0_intkind1,wf(:,42))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,1),wf(:,43))
  call counter_GG_S(wf(:,1),wf(:,27),wf(:,44))
  call counter_GG_V(wf(:,1),Q(:,3),wf(:,27),Q(:,12),wf(:,45))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,46))
  call counter_AW_Q(wf(:,5),wf(:,-5),wf(:,47))
  call counter_WQ_A(wf(:,-4),wf(:,4),wf(:,48))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,49))
  call prop_Q_A(wf(:,11),Q(:,52),MB,1_intkind1,wf(:,50))
  call counter_SA_Q(gH,wf(:,13),wf(:,-3),wf(:,51))
  call counter_AV_Q(wf(:,-3),wf(:,15),wf(:,52))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,17),wf(:,53))
  call counter_AW_Q(wf(:,-3),wf(:,-4),wf(:,54))
  call prop_A_Q(wf(:,54),Q(:,24),MT,1_intkind1,wf(:,55))
  call vert_AW_Q(wf(:,55),wf(:,-5),wf(:,56))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,57))
  call prop_A_Q(wf(:,8),Q(:,56),MB,1_intkind1,wf(:,58))
  call counter_WQ_A(wf(:,-5),wf(:,-2),wf(:,59))
  call prop_Q_A(wf(:,59),Q(:,36),MT,1_intkind1,wf(:,60))
  call vert_VQ_A(wf(:,1),wf(:,60),wf(:,61))
  call counter_QS_A(gH,wf(:,-2),wf(:,13),wf(:,62))
  call counter_VQ_A(wf(:,15),wf(:,-2),wf(:,63))
  call counter_ZQ_A(gZd,wf(:,17),wf(:,-2),wf(:,64))
  call prop_A_Q(wf(:,14),Q(:,56),MB,1_intkind1,wf(:,65))
  call prop_A_Q(wf(:,16),Q(:,56),MB,1_intkind1,wf(:,66))
  call prop_A_Q(wf(:,18),Q(:,56),MB,1_intkind1,wf(:,67))
  call vert_WQ_A(wf(:,-4),wf(:,60),wf(:,68))
  call counter_QA_V(wf(:,28),wf(:,29),wf(:,69))
  call counter_AW_Q(wf(:,29),wf(:,-5),wf(:,70))
  call counter_WQ_A(wf(:,-4),wf(:,28),wf(:,71))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,72))
  call vert_VQ_A(wf(:,72),wf(:,0),wf(:,73))
  call prop_Q_A(wf(:,73),Q(:,13),ZERO,0_intkind1,wf(:,74))
  call vert_AV_Q(wf(:,-1),wf(:,72),wf(:,75))
  call prop_A_Q(wf(:,75),Q(:,14),ZERO,0_intkind1,wf(:,76))
  call counter_AV_Q(wf(:,-1),wf(:,27),wf(:,77))
  call prop_Q_A(wf(:,35),Q(:,49),ZERO,0_intkind1,wf(:,78))
  call counter_AV_Q(wf(:,-1),wf(:,15),wf(:,79))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,17),wf(:,80))
  call counter_AW_Q(wf(:,-1),wf(:,-4),wf(:,81))
  call prop_A_Q(wf(:,81),Q(:,18),ZERO,0_intkind1,wf(:,82))
  call vert_QA_V(wf(:,28),wf(:,82),wf(:,83))
  call vert_AW_Q(wf(:,82),wf(:,-5),wf(:,84))
  call counter_VQ_A(wf(:,27),wf(:,0),wf(:,85))
  call prop_A_Q(wf(:,32),Q(:,50),ZERO,0_intkind1,wf(:,86))
  call counter_WQ_A(wf(:,-5),wf(:,0),wf(:,87))
  call prop_Q_A(wf(:,87),Q(:,33),ZERO,0_intkind1,wf(:,88))
  call vert_QA_V(wf(:,88),wf(:,29),wf(:,89))
  call prop_A_Q(wf(:,37),Q(:,50),ZERO,0_intkind1,wf(:,90))
  call prop_A_Q(wf(:,38),Q(:,50),ZERO,0_intkind1,wf(:,91))
  call counter_VQ_A(wf(:,15),wf(:,0),wf(:,92))
  call counter_ZQ_A(gZd,wf(:,17),wf(:,0),wf(:,93))
  call vert_WQ_A(wf(:,-4),wf(:,88),wf(:,94))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,95))
  call vert_VQ_A(wf(:,95),wf(:,4),wf(:,96))
  call vert_VQ_A(wf(:,95),wf(:,-2),wf(:,97))
  call prop_Q_A(wf(:,97),Q(:,7),MB,1_intkind1,wf(:,98))
  call vert_AV_Q(wf(:,-3),wf(:,95),wf(:,99))
  call prop_A_Q(wf(:,99),Q(:,11),MB,1_intkind1,wf(:,100))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,101))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,102))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,103))
  call counter_Q_A(cttt,wf(:,4),Q(:,36),wf(:,104))
  call prop_A_Q(wf(:,103),Q(:,27),MT,1_intkind1,wf(:,105))
  call counter_A_Q(cttt,wf(:,5),Q(:,24),wf(:,106))
  call prop_Q_A(wf(:,6),Q(:,39),MT,1_intkind1,wf(:,107))
  call vert_VQ_A(wf(:,102),wf(:,-2),wf(:,108))
  call counter_Q_A(ctbb,wf(:,9),Q(:,7),wf(:,109))
  call prop_A_Q(wf(:,106),Q(:,24),MT,1_intkind1,wf(:,110))
  call vert_AW_Q(wf(:,110),wf(:,-5),wf(:,111))
  call vert_AV_Q(wf(:,-3),wf(:,102),wf(:,112))
  call prop_Q_A(wf(:,104),Q(:,36),MT,1_intkind1,wf(:,113))
  call vert_WQ_A(wf(:,-4),wf(:,113),wf(:,114))
  call counter_A_Q(ctbb,wf(:,12),Q(:,11),wf(:,115))
  call counter_Q_A(ctbb,wf(:,20),Q(:,52),wf(:,116))
  call counter_Q_A(ctbb,wf(:,22),Q(:,52),wf(:,117))
  call counter_Q_A(ctbb,wf(:,24),Q(:,52),wf(:,118))
  call vert_AV_Q(wf(:,29),wf(:,27),wf(:,119))
  call counter_Q_A(ctqq,wf(:,28),Q(:,33),wf(:,120))
  call prop_A_Q(wf(:,119),Q(:,30),ZERO,0_intkind1,wf(:,121))
  call vert_VQ_A(wf(:,27),wf(:,28),wf(:,122))
  call counter_A_Q(ctqq,wf(:,29),Q(:,18),wf(:,123))
  call prop_Q_A(wf(:,122),Q(:,45),ZERO,0_intkind1,wf(:,124))
  call counter_V_V(ctGG,wf(:,27),Q(:,12),wf(:,125))
  call vert_VQ_A(wf(:,125),wf(:,0),wf(:,126))
  call counter_Q_A(ctqq,wf(:,33),Q(:,13),wf(:,127))
  call prop_A_Q(wf(:,123),Q(:,18),ZERO,0_intkind1,wf(:,128))
  call vert_AW_Q(wf(:,128),wf(:,-5),wf(:,129))
  call prop_Q_A(wf(:,120),Q(:,33),ZERO,0_intkind1,wf(:,130))
  call vert_WQ_A(wf(:,-4),wf(:,130),wf(:,131))
  call vert_AV_Q(wf(:,-1),wf(:,125),wf(:,132))
  call counter_A_Q(ctqq,wf(:,36),Q(:,14),wf(:,133))
  call counter_Q_A(ctqq,wf(:,40),Q(:,49),wf(:,134))
  call counter_Q_A(ctqq,wf(:,42),Q(:,49),wf(:,135))
  call vert_WQ_A(wf(:,-5),wf(:,9),wf(:,136))
  call prop_Q_A(wf(:,136),Q(:,39),MT,1_intkind1,wf(:,137))
  call vert_AW_Q(wf(:,12),wf(:,-4),wf(:,138))
  call prop_A_Q(wf(:,138),Q(:,27),MT,1_intkind1,wf(:,139))
  call vert_WQ_A(wf(:,-5),wf(:,33),wf(:,140))
  call prop_Q_A(wf(:,140),Q(:,45),ZERO,0_intkind1,wf(:,141))
  call vert_AW_Q(wf(:,36),wf(:,-4),wf(:,142))
  call prop_A_Q(wf(:,142),Q(:,30),ZERO,0_intkind1,wf(:,143))
  call vert_QA_V(wf(:,0),wf(:,86),wf(:,144))
  call vert_QA_V(wf(:,-2),wf(:,58),wf(:,145))
  call vert_QA_V(wf(:,78),wf(:,-1),wf(:,146))
  call vert_QA_V(wf(:,50),wf(:,-3),wf(:,147))
  call vert_QA_V(wf(:,40),wf(:,-1),wf(:,148))
  call vert_QA_V(wf(:,42),wf(:,-1),wf(:,149))
  call vert_QA_V(wf(:,0),wf(:,90),wf(:,150))
  call vert_QA_V(wf(:,0),wf(:,91),wf(:,151))
  call vert_QA_V(wf(:,20),wf(:,-3),wf(:,152))
  call vert_QA_V(wf(:,22),wf(:,-3),wf(:,153))
  call vert_QA_V(wf(:,24),wf(:,-3),wf(:,154))
  call vert_QA_V(wf(:,-2),wf(:,65),wf(:,155))
  call vert_QA_V(wf(:,-2),wf(:,66),wf(:,156))
  call vert_QA_V(wf(:,-2),wf(:,67),wf(:,157))

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
  den(2) = 1 / (Q(5,36) - MT2)
  den(3) = 1 / (Q(5,24) - MT2)
  den(6) = 1 / (Q(5,7) - MB2)
  den(9) = 1 / (Q(5,11) - MB2)
  den(12) = 1 / (Q(5,48) - MH2)
  den(14) = 1 / (Q(5,48))
  den(16) = 1 / (Q(5,48) - MZ2)
  den(18) = 1 / (Q(5,52) - MB2)
  den(25) = 1 / (Q(5,33))
  den(26) = 1 / (Q(5,18))
  den(27) = 1 / (Q(5,12))
  den(30) = 1 / (Q(5,13))
  den(33) = 1 / (Q(5,14))
  den(38) = 1 / (Q(5,49))
  den(48) = 1 / (Q(5,56) - MB2)
  den(62) = 1 / (Q(5,50))
  den(72) = 1 / (Q(5,60))
  den(76) = 1 / (Q(5,27) - MT2)
  den(79) = 1 / (Q(5,39) - MT2)
  den(104) = 1 / (Q(5,30))
  den(108) = 1 / (Q(5,45))
  den(111) = 1 / (Q(5,51))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(3)*den(7)
  den(10) = den(1)*den(9)
  den(11) = den(2)*den(10)
  den(13) = den(7)*den(12)
  den(15) = den(7)*den(14)
  den(17) = den(7)*den(16)
  den(19) = den(12)*den(18)
  den(20) = den(1)*den(19)
  den(21) = den(14)*den(18)
  den(22) = den(1)*den(21)
  den(23) = den(16)*den(18)
  den(24) = den(1)*den(23)
  den(28) = den(25)*den(26)
  den(29) = den(27)*den(28)
  den(31) = den(27)*den(30)
  den(32) = den(26)*den(31)
  den(34) = den(27)*den(33)
  den(35) = den(25)*den(34)
  den(36) = den(14)*den(31)
  den(37) = den(16)*den(31)
  den(39) = den(14)*den(38)
  den(40) = den(27)*den(39)
  den(41) = den(16)*den(38)
  den(42) = den(27)*den(41)
  den(43) = den(1)*den(27)
  den(44) = den(12)*den(43)
  den(45) = den(16)*den(43)
  den(46) = den(2)*den(18)
  den(47) = den(1)*den(46)
  den(49) = den(3)*den(48)
  den(50) = den(1)*den(49)
  den(51) = den(10)*den(12)
  den(52) = den(10)*den(14)
  den(53) = den(10)*den(16)
  den(54) = den(12)*den(48)
  den(55) = den(1)*den(54)
  den(56) = den(14)*den(48)
  den(57) = den(1)*den(56)
  den(58) = den(16)*den(48)
  den(59) = den(1)*den(58)
  den(60) = den(25)*den(38)
  den(61) = den(27)*den(60)
  den(63) = den(26)*den(62)
  den(64) = den(27)*den(63)
  den(65) = den(14)*den(62)
  den(66) = den(27)*den(65)
  den(67) = den(16)*den(62)
  den(68) = den(27)*den(67)
  den(69) = den(14)*den(34)
  den(70) = den(16)*den(34)
  den(71) = den(2)*den(3)
  den(73) = den(71)*den(72)
  den(74) = den(1)*den(73)
  den(75) = den(1)*den(3)
  den(77) = den(75)*den(76)
  den(78) = den(2)*den(77)
  den(80) = den(4)*den(79)
  den(81) = den(3)*den(80)
  den(82) = den(1)**2
  den(83) = den(49)*den(82)
  den(84) = den(7)*den(49)
  den(85) = den(3)**2
  den(86) = den(7)*den(85)
  den(87) = den(46)*den(82)
  den(88) = den(2)**2
  den(89) = den(10)*den(88)
  den(90) = den(10)*den(46)
  den(91) = den(54)*den(82)
  den(92) = den(56)*den(82)
  den(93) = den(58)*den(82)
  den(94) = den(19)*den(82)
  den(95) = den(21)*den(82)
  den(96) = den(23)*den(82)
  den(97) = den(7)*den(54)
  den(98) = den(7)*den(56)
  den(99) = den(7)*den(58)
  den(100) = den(10)*den(19)
  den(101) = den(10)*den(21)
  den(102) = den(10)*den(23)
  den(103) = den(26)*den(27)
  den(105) = den(103)*den(104)
  den(106) = den(25)*den(105)
  den(107) = den(25)*den(27)
  den(109) = den(107)*den(108)
  den(110) = den(26)*den(109)
  den(112) = den(28)*den(111)
  den(113) = den(27)*den(112)
  den(114) = den(27)**2
  den(115) = den(63)*den(114)
  den(116) = den(31)*den(63)
  den(117) = den(26)**2
  den(118) = den(31)*den(117)
  den(119) = den(25)**2
  den(120) = den(34)*den(119)
  den(121) = den(60)*den(114)
  den(122) = den(34)*den(60)
  den(123) = den(31)*den(65)
  den(124) = den(31)*den(67)
  den(125) = den(34)*den(39)
  den(126) = den(34)*den(41)
  den(127) = den(65)*den(114)
  den(128) = den(67)*den(114)
  den(129) = den(39)*den(114)
  den(130) = den(41)*den(114)
  den(131) = den(7)*den(79)
  den(132) = den(10)*den(76)
  den(133) = den(31)*den(108)
  den(134) = den(34)*den(104)
  den(135) = den(63)*den(111)
  den(136) = den(49)*den(72)
  den(137) = den(60)*den(111)
  den(138) = den(46)*den(72)
  den(139) = den(39)*den(111)
  den(140) = den(41)*den(111)
  den(141) = den(65)*den(111)
  den(142) = den(67)*den(111)
  den(143) = den(19)*den(72)
  den(144) = den(21)*den(72)
  den(145) = den(23)*den(72)
  den(146) = den(54)*den(72)
  den(147) = den(56)*den(72)
  den(148) = den(58)*den(72)
  den(149) = den(1)*den(12)*den(27)
  den(150) = den(1)*den(14)*den(27)
  den(151) = den(1)*den(16)*den(27)
  den(152) = den(1)*den(2)*den(3)
  den(153) = den(1)*den(12)
  den(154) = den(1)*den(14)
  den(155) = den(1)*den(16)
  den(156) = den(25)*den(26)*den(27)
  den(157) = den(14)*den(27)
  den(158) = den(16)*den(27)
  den(159) = den(3)*den(131)
  den(160) = den(1)*den(136)
  den(161) = den(2)*den(132)
  den(162) = den(1)*den(138)
  den(163) = den(1)*den(143)
  den(164) = den(1)*den(144)
  den(165) = den(1)*den(145)
  den(166) = den(1)*den(146)
  den(167) = den(1)*den(147)
  den(168) = den(1)*den(148)
  den(169) = den(26)*den(133)
  den(170) = den(27)*den(135)
  den(171) = den(27)*den(137)
  den(172) = den(25)*den(134)
  den(173) = den(27)*den(139)
  den(174) = den(27)*den(140)
  den(175) = den(27)*den(141)
  den(176) = den(27)*den(142)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(111)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,11),wf(:,12)) * den(11)
  A(4) = cont_QA(wf(:,9),wf(:,14)) * den(13)
  A(5) = cont_QA(wf(:,9),wf(:,16)) * den(15)
  A(6) = cont_QA(wf(:,9),wf(:,18)) * den(17)
  A(7) = cont_QA(wf(:,10),wf(:,20)) * den(20)
  A(8) = cont_QA(wf(:,10),wf(:,22)) * den(22)
  A(9) = cont_QA(wf(:,10),wf(:,24)) * den(24)
  A(10) = cont_VV(wf(:,27),wf(:,30)) * den(29)
  A(11) = cont_QA(wf(:,32),wf(:,33)) * den(32)
  A(12) = cont_QA(wf(:,35),wf(:,36)) * den(35)
  A(13) = cont_QA(wf(:,33),wf(:,37)) * den(36)
  A(14) = cont_QA(wf(:,33),wf(:,38)) * den(37)
  A(15) = cont_QA(wf(:,34),wf(:,40)) * den(40)
  A(16) = cont_QA(wf(:,34),wf(:,42)) * den(42)

  A(17) = cont_VV(wf(:,27),wf(:,43)) * den(43)
  A(18) = cont_SS(wf(:,13),wf(:,44)) * den(44)
  A(19) = cont_VV(wf(:,17),wf(:,45)) * den(45)
  A(20) = cont_QA(wf(:,5),wf(:,46)) * den(5)
  A(21) = cont_QA(wf(:,9),wf(:,47)) * den(8)
  A(22) = cont_QA(wf(:,12),wf(:,48)) * den(11)
  A(23) = cont_QA(wf(:,49),wf(:,50)) * den(47)
  A(24) = cont_QA(wf(:,9),wf(:,51)) * den(13)
  A(25) = cont_QA(wf(:,9),wf(:,52)) * den(15)
  A(26) = cont_QA(wf(:,9),wf(:,53)) * den(17)
  A(27) = cont_QA(wf(:,20),wf(:,49)) * den(20)
  A(28) = cont_QA(wf(:,22),wf(:,49)) * den(22)
  A(29) = cont_QA(wf(:,24),wf(:,49)) * den(24)
  A(30) = cont_QA(wf(:,6),wf(:,55)) * den(5)
  A(31) = cont_QA(wf(:,9),wf(:,56)) * den(8)
  A(32) = cont_QA(wf(:,57),wf(:,58)) * den(50)
  A(33) = cont_QA(wf(:,5),wf(:,61)) * den(5)
  A(34) = cont_QA(wf(:,12),wf(:,62)) * den(51)
  A(35) = cont_QA(wf(:,12),wf(:,63)) * den(52)
  A(36) = cont_QA(wf(:,12),wf(:,64)) * den(53)
  A(37) = cont_QA(wf(:,57),wf(:,65)) * den(55)
  A(38) = cont_QA(wf(:,57),wf(:,66)) * den(57)
  A(39) = cont_QA(wf(:,57),wf(:,67)) * den(59)
  A(40) = cont_QA(wf(:,12),wf(:,68)) * den(11)
  A(41) = cont_VV(wf(:,27),wf(:,69)) * den(29)
  A(42) = cont_QA(wf(:,33),wf(:,70)) * den(32)
  A(43) = cont_QA(wf(:,36),wf(:,71)) * den(35)
  A(44) = cont_VV(wf(:,30),wf(:,72)) * den(29)
  A(45) = cont_QA(wf(:,32),wf(:,74)) * den(32)
  A(46) = cont_QA(wf(:,35),wf(:,76)) * den(35)
  A(47) = cont_QA(wf(:,40),wf(:,75)) * den(40)
  A(48) = cont_QA(wf(:,42),wf(:,75)) * den(42)
  A(49) = cont_QA(wf(:,37),wf(:,74)) * den(36)
  A(50) = cont_QA(wf(:,38),wf(:,74)) * den(37)
  A(51) = cont_QA(wf(:,77),wf(:,78)) * den(61)
  A(52) = cont_QA(wf(:,33),wf(:,79)) * den(36)
  A(53) = cont_QA(wf(:,33),wf(:,80)) * den(37)
  A(54) = cont_QA(wf(:,40),wf(:,77)) * den(40)
  A(55) = cont_QA(wf(:,42),wf(:,77)) * den(42)
  A(56) = cont_VV(wf(:,27),wf(:,83)) * den(29)
  A(57) = cont_QA(wf(:,33),wf(:,84)) * den(32)
  A(58) = cont_QA(wf(:,85),wf(:,86)) * den(64)
  A(59) = cont_VV(wf(:,27),wf(:,89)) * den(29)
  A(60) = cont_QA(wf(:,85),wf(:,90)) * den(66)
  A(61) = cont_QA(wf(:,85),wf(:,91)) * den(68)
  A(62) = cont_QA(wf(:,36),wf(:,92)) * den(69)
  A(63) = cont_QA(wf(:,36),wf(:,93)) * den(70)
  A(64) = cont_QA(wf(:,36),wf(:,94)) * den(35)
  A(65) = cont_QA(wf(:,5),wf(:,96)) * den(5)
  A(66) = cont_QA(wf(:,8),wf(:,98)) * den(8)
  A(67) = cont_QA(wf(:,11),wf(:,100)) * den(11)
  A(68) = cont_QA(wf(:,14),wf(:,98)) * den(13)
  A(69) = cont_QA(wf(:,16),wf(:,98)) * den(15)
  A(70) = cont_QA(wf(:,18),wf(:,98)) * den(17)
  A(71) = cont_QA(wf(:,20),wf(:,99)) * den(20)
  A(72) = cont_QA(wf(:,22),wf(:,99)) * den(22)
  A(73) = cont_QA(wf(:,24),wf(:,99)) * den(24)
  A(74) = cont_VV(wf(:,101),wf(:,102)) * den(74)
  A(75) = cont_QA(wf(:,104),wf(:,105)) * den(78)
  A(76) = cont_QA(wf(:,106),wf(:,107)) * den(81)
  A(77) = cont_QA(wf(:,58),wf(:,108)) * den(83)
  A(78) = cont_QA(wf(:,58),wf(:,109)) * den(84)
  A(79) = cont_QA(wf(:,9),wf(:,111)) * den(86)
  A(80) = cont_QA(wf(:,50),wf(:,112)) * den(87)
  A(81) = cont_QA(wf(:,12),wf(:,114)) * den(89)
  A(82) = cont_QA(wf(:,50),wf(:,115)) * den(90)
  A(83) = cont_QA(wf(:,65),wf(:,108)) * den(91)
  A(84) = cont_QA(wf(:,66),wf(:,108)) * den(92)
  A(85) = cont_QA(wf(:,67),wf(:,108)) * den(93)
  A(86) = cont_QA(wf(:,20),wf(:,112)) * den(94)
  A(87) = cont_QA(wf(:,22),wf(:,112)) * den(95)
  A(88) = cont_QA(wf(:,24),wf(:,112)) * den(96)
  A(89) = cont_QA(wf(:,65),wf(:,109)) * den(97)
  A(90) = cont_QA(wf(:,66),wf(:,109)) * den(98)
  A(91) = cont_QA(wf(:,67),wf(:,109)) * den(99)
  A(92) = cont_QA(wf(:,12),wf(:,116)) * den(100)
  A(93) = cont_QA(wf(:,12),wf(:,117)) * den(101)
  A(94) = cont_QA(wf(:,12),wf(:,118)) * den(102)
  A(95) = cont_QA(wf(:,120),wf(:,121)) * den(106)
  A(96) = cont_QA(wf(:,123),wf(:,124)) * den(110)
  A(97) = cont_VV(wf(:,30),wf(:,125)) * den(113)
  A(98) = cont_QA(wf(:,86),wf(:,126)) * den(115)
  A(99) = cont_QA(wf(:,86),wf(:,127)) * den(116)
  A(100) = cont_QA(wf(:,33),wf(:,129)) * den(118)
  A(101) = cont_QA(wf(:,36),wf(:,131)) * den(120)
  A(102) = cont_QA(wf(:,78),wf(:,132)) * den(121)
  A(103) = cont_QA(wf(:,78),wf(:,133)) * den(122)
  A(104) = cont_QA(wf(:,90),wf(:,127)) * den(123)
  A(105) = cont_QA(wf(:,91),wf(:,127)) * den(124)
  A(106) = cont_QA(wf(:,36),wf(:,134)) * den(125)
  A(107) = cont_QA(wf(:,36),wf(:,135)) * den(126)
  A(108) = cont_QA(wf(:,90),wf(:,126)) * den(127)
  A(109) = cont_QA(wf(:,91),wf(:,126)) * den(128)
  A(110) = cont_QA(wf(:,40),wf(:,132)) * den(129)
  A(111) = cont_QA(wf(:,42),wf(:,132)) * den(130)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(111)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(5)-A(8)-A(13)-A(15))*f(1))/2._/**/REALKIND+((A(1)+A(2)+A(3)+A(10)+A(11)+A(12))*f(8))/2._/**/REALKIND+((-A(4) &
       -A(7))*f(15))/2._/**/REALKIND+((A(6)+A(9)+A(14)+A(16))*f(20))/2._/**/REALKIND
  M1(2) = ((A(5)+A(8)+A(13)+A(15))*f(1))/6._/**/REALKIND+((-A(1)-A(2)-A(3)-A(10)-A(11)-A(12))*f(8))/6._/**/REALKIND+((A(4) &
       +A(7))*f(15))/6._/**/REALKIND+((-A(6)-A(9)-A(14)-A(16))*f(20))/6._/**/REALKIND

  M2(1) = ((A(84)+A(87)+A(90)+A(93)+A(104)+A(106)+A(108)+A(110))*f(2))/2._/**/REALKIND+((-A(28)-A(38)-A(47) &
       -A(49))*f(3))/2._/**/REALKIND+((-A(54)-A(60)-A(69)-A(72))*f(4))/2._/**/REALKIND+((-A(25)-A(35))*f(5))/2._/**/REALKIND+(( &
       -A(52)-A(62))*f(6))/2._/**/REALKIND+(A(17)*f(7))/2._/**/REALKIND+((-A(74)-A(75)-A(76)-A(77)-A(78)-A(79)-A(80)-A(81)-A(82) &
       -A(95)-A(96)-A(97)-A(98)-A(99)-A(100)-A(101)-A(102)-A(103))*f(9))/2._/**/REALKIND+((A(23)+A(32)+A(44)+A(45) &
       +A(46))*f(10))/2._/**/REALKIND+((A(41)+A(51)+A(58)+A(65)+A(66)+A(67))*f(11))/2._/**/REALKIND+(A(20)*f(12))/2._/**/REALKIND &
       +((A(21)+A(22)+A(30)+A(31)+A(33)+A(40))*f(13))/2._/**/REALKIND+((A(42)+A(43)+A(56)+A(57)+A(59) &
       +A(64))*f(14))/2._/**/REALKIND+((A(83)+A(86)+A(89)+A(92))*f(16))/2._/**/REALKIND+((-A(27)-A(37))*f(17))/2._/**/REALKIND+(( &
       -A(68)-A(71))*f(18))/2._/**/REALKIND+((-A(24)-A(34))*f(19))/2._/**/REALKIND+((-A(85)-A(88)-A(91)-A(94)-A(105)-A(107)-A(109) &
       -A(111))*f(21))/2._/**/REALKIND+((A(29)+A(39)+A(48)+A(50))*f(22))/2._/**/REALKIND+((A(55)+A(61)+A(70) &
       +A(73))*f(23))/2._/**/REALKIND+((A(26)+A(36))*f(24))/2._/**/REALKIND+((A(53)+A(63))*f(25))/2._/**/REALKIND &
       -(A(19)*f(26))/2._/**/REALKIND-(A(18)*f(27))/2._/**/REALKIND
  M2(2) = ((-A(84)-A(87)-A(90)-A(93)-A(104)-A(106)-A(108)-A(110))*f(2))/6._/**/REALKIND+((A(28)+A(38)+A(47) &
       +A(49))*f(3))/6._/**/REALKIND+((A(54)+A(60)+A(69)+A(72))*f(4))/6._/**/REALKIND+((A(25)+A(35))*f(5))/6._/**/REALKIND+((A(52) &
       +A(62))*f(6))/6._/**/REALKIND-(A(17)*f(7))/6._/**/REALKIND+((A(74)+A(75)+A(76)+A(77)+A(78)+A(79)+A(80)+A(81)+A(82)+A(95) &
       +A(96)+A(97)+A(98)+A(99)+A(100)+A(101)+A(102)+A(103))*f(9))/6._/**/REALKIND+((-A(23)-A(32)-A(44)-A(45) &
       -A(46))*f(10))/6._/**/REALKIND+((-A(41)-A(51)-A(58)-A(65)-A(66)-A(67))*f(11))/6._/**/REALKIND-(A(20)*f(12))/6._/**/REALKIND &
       +((-A(21)-A(22)-A(30)-A(31)-A(33)-A(40))*f(13))/6._/**/REALKIND+((-A(42)-A(43)-A(56)-A(57)-A(59) &
       -A(64))*f(14))/6._/**/REALKIND+((-A(83)-A(86)-A(89)-A(92))*f(16))/6._/**/REALKIND+((A(27)+A(37))*f(17))/6._/**/REALKIND &
       +((A(68)+A(71))*f(18))/6._/**/REALKIND+((A(24)+A(34))*f(19))/6._/**/REALKIND+((A(85)+A(88)+A(91)+A(94)+A(105)+A(107)+A(109) &
       +A(111))*f(21))/6._/**/REALKIND+((-A(29)-A(39)-A(48)-A(50))*f(22))/6._/**/REALKIND+((-A(55)-A(61)-A(70) &
       -A(73))*f(23))/6._/**/REALKIND+((-A(26)-A(36))*f(24))/6._/**/REALKIND+((-A(53)-A(63))*f(25))/6._/**/REALKIND &
       +(A(19)*f(26))/6._/**/REALKIND+(A(18)*f(27))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppwwjj_ddxbbxwwx_1_/**/REALKIND
