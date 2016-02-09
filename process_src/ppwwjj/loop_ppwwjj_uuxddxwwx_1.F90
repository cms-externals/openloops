
module ol_colourmatrix_ppwwjj_uuxddxwwx_1_/**/REALKIND
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
end module ol_colourmatrix_ppwwjj_uuxddxwwx_1_/**/REALKIND



module ol_forced_parameters_ppwwjj_uuxddxwwx_1_/**/REALKIND
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
end module ol_forced_parameters_ppwwjj_uuxddxwwx_1_/**/REALKIND

module ol_loop_ppwwjj_uuxddxwwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(39), c(68)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:183)
  ! denominators
  complex(REALKIND), save :: den(205)
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
    f( 2) = (2*CI*eQED**2*gQCD**2)/3._/**/REALKIND
    f( 3) = (CI*countertermnorm*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 4) = (2*CI*countertermnorm*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 5) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 6) = (2*CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 7) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 8) = (2*CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 9) = CI*countertermnorm*ctWWGG*eQED**2*gQCD**4
    f(10) = (CI*eQED**2*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(11) = (CI*countertermnorm*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(12) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(13) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(14) = (CI*cw*eQED**2*gQCD**2)/sw
    f(15) = (CI*countertermnorm*cw*eQED**2*gQCD**4)/sw
    f(16) = (CI*countertermnorm*ctGqq*cw*eQED**2*gQCD**4)/sw
    f(17) = (CI*countertermnorm*ctVqq*cw*eQED**2*gQCD**4)/sw
    f(18) = (countertermnorm*ctZGG*cw*eQED**2*gQCD**4)/sw
    f(19) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MW)/sw
    f(20) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(21) = (2*CI*eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(22) = (eQED**2*gQCD**4*integralnorm*SwB)/6._/**/REALKIND
    f(23) = (eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(24) = (2*eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(25) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(26) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*4._/**/REALKIND)
    f(27) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(28) = (CI*cw*eQED**2*gQCD**4*integralnorm*SwB)/sw
    f(29) = (cw*eQED**2*gQCD**4*integralnorm*SwB)/(sw*2._/**/REALKIND)
    f(30) = (cw*eQED**2*gQCD**4*integralnorm*SwB)/sw
    f(31) = (eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(32) = (2*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(33) = (4*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(34) = (eQED**2*gQCD**4*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(35) = (eQED**2*gQCD**4*integralnorm*SwF)/sw**2
    f(36) = (eQED**2*gQCD**4*integralnorm*MB*SwF)/(sw**2*2._/**/REALKIND)
    f(37) = (eQED**2*gQCD**4*integralnorm*MT*SwF)/(sw**2*2._/**/REALKIND)
    f(38) = (cw*eQED**2*gQCD**4*integralnorm*SwF)/sw
    f(39) = (2*cw*eQED**2*gQCD**4*integralnorm*SwF)/sw

  c = [ 9*CI*f(20), 27*CI*f(20), 9*CI*f(21), 27*CI*f(21), 18*f(22), 54*f(22), f(23), 3*f(23), 6*f(23), 8*f(23), 10*f(23), 18*f(23) &
    , 21*f(23), 24*f(23), 54*f(23), f(24), 3*f(24), 6*f(24), 8*f(24), 10*f(24), 18*f(24), 21*f(24), 24*f(24), 54*f(24), 9*CI*f(25) &
    , 27*CI*f(25), 18*f(26), 54*f(26), f(27), 3*f(27), 6*f(27), 8*f(27), 10*f(27), 18*f(27), 21*f(27), 24*f(27), 54*f(27) &
    , 9*CI*f(28), 27*CI*f(28), 18*f(29), 54*f(29), f(30), 3*f(30), 6*f(30), 8*f(30), 10*f(30), 18*f(30), 21*f(30), 24*f(30) &
    , 54*f(30), 3*f(31), 9*f(31), 3*f(32), 9*f(32), 3*f(33), 9*f(33), 3*f(34), 9*f(34), 3*f(35), 9*f(35), 3*f(36), 9*f(36) &
    , 3*f(37), 9*f(37), 3*f(38), 9*f(38), 3*f(39), 9*f(39) ]
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
  complex(REALKIND) :: A(131)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rMW, H(5), wf(:,-4))
  call wf_V(P(:,6), rMW, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_WQ_A(wf(:,-5),wf(:,-2),wf(:,2))
  call vert_AW_Q(wf(:,-3),wf(:,-4),wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,36),ZERO,0_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,24),ZERO,0_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,6))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,7))
  call vert_AW_Q(wf(:,5),wf(:,-5),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,7),ZERO,0_intkind1,wf(:,9))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,10))
  call vert_WQ_A(wf(:,-4),wf(:,4),wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,11),ZERO,0_intkind1,wf(:,12))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-4),Q(:,16),wf(:,13))
  call vert_AV_Q(wf(:,-3),wf(:,13),wf(:,14))
  call prop_W_W(wf(:,13),Q(:,48),MZ,1_intkind1,wf(:,15))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,15),wf(:,16))
  call vert_VQ_A(wf(:,13),wf(:,-2),wf(:,17))
  call prop_Q_A(wf(:,17),Q(:,52),ZERO,0_intkind1,wf(:,18))
  call vert_ZQ_A(gZd,wf(:,15),wf(:,-2),wf(:,19))
  call prop_Q_A(wf(:,19),Q(:,52),ZERO,0_intkind1,wf(:,20))
  call vert_WQ_A(wf(:,-4),wf(:,0),wf(:,21))
  call vert_AW_Q(wf(:,-1),wf(:,-5),wf(:,22))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,23))
  call prop_Q_A(wf(:,21),Q(:,17),ZERO,0_intkind1,wf(:,24))
  call prop_A_Q(wf(:,22),Q(:,34),ZERO,0_intkind1,wf(:,25))
  call vert_QA_V(wf(:,24),wf(:,25),wf(:,26))
  call vert_AV_Q(wf(:,-1),wf(:,23),wf(:,27))
  call vert_WQ_A(wf(:,-5),wf(:,24),wf(:,28))
  call prop_A_Q(wf(:,27),Q(:,14),ZERO,0_intkind1,wf(:,29))
  call vert_VQ_A(wf(:,23),wf(:,0),wf(:,30))
  call vert_AW_Q(wf(:,25),wf(:,-4),wf(:,31))
  call prop_Q_A(wf(:,30),Q(:,13),ZERO,0_intkind1,wf(:,32))
  call vert_AV_Q(wf(:,-1),wf(:,13),wf(:,33))
  call vert_AZ_Q(gZu,wf(:,-1),wf(:,15),wf(:,34))
  call vert_VQ_A(wf(:,13),wf(:,0),wf(:,35))
  call prop_Q_A(wf(:,35),Q(:,49),ZERO,0_intkind1,wf(:,36))
  call vert_ZQ_A(gZu,wf(:,15),wf(:,0),wf(:,37))
  call prop_Q_A(wf(:,37),Q(:,49),ZERO,0_intkind1,wf(:,38))
  call vert_QA_V(wf(:,-2),wf(:,25),wf(:,39))
  call vert_QA_V(wf(:,24),wf(:,-3),wf(:,40))
  call vert_QA_V(wf(:,4),wf(:,-1),wf(:,41))
  call vert_QA_V(wf(:,0),wf(:,5),wf(:,42))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,1),wf(:,43))
  call vert_VV_S(wf(:,-4),wf(:,-5),wf(:,44))
  call counter_GG_S(wf(:,1),wf(:,23),wf(:,45))
  call counter_GG_V(wf(:,1),Q(:,3),wf(:,23),Q(:,12),wf(:,46))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,47))
  call counter_AW_Q(wf(:,5),wf(:,-5),wf(:,48))
  call counter_WQ_A(wf(:,-4),wf(:,4),wf(:,49))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,50))
  call prop_Q_A(wf(:,11),Q(:,52),ZERO,0_intkind1,wf(:,51))
  call counter_AV_Q(wf(:,-3),wf(:,13),wf(:,52))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,15),wf(:,53))
  call counter_AW_Q(wf(:,-3),wf(:,-4),wf(:,54))
  call prop_A_Q(wf(:,54),Q(:,24),ZERO,0_intkind1,wf(:,55))
  call vert_AW_Q(wf(:,55),wf(:,-5),wf(:,56))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,57))
  call prop_A_Q(wf(:,8),Q(:,56),ZERO,0_intkind1,wf(:,58))
  call counter_WQ_A(wf(:,-5),wf(:,-2),wf(:,59))
  call prop_Q_A(wf(:,59),Q(:,36),ZERO,0_intkind1,wf(:,60))
  call vert_VQ_A(wf(:,1),wf(:,60),wf(:,61))
  call counter_VQ_A(wf(:,13),wf(:,-2),wf(:,62))
  call counter_ZQ_A(gZd,wf(:,15),wf(:,-2),wf(:,63))
  call prop_A_Q(wf(:,14),Q(:,56),ZERO,0_intkind1,wf(:,64))
  call prop_A_Q(wf(:,16),Q(:,56),ZERO,0_intkind1,wf(:,65))
  call vert_WQ_A(wf(:,-4),wf(:,60),wf(:,66))
  call counter_QA_V(wf(:,24),wf(:,25),wf(:,67))
  call counter_WQ_A(wf(:,-5),wf(:,24),wf(:,68))
  call counter_AW_Q(wf(:,25),wf(:,-4),wf(:,69))
  call counter_QA_V(wf(:,24),wf(:,-3),wf(:,70))
  call vert_QA_V(wf(:,0),wf(:,55),wf(:,71))
  call counter_QA_V(wf(:,-2),wf(:,25),wf(:,72))
  call vert_QA_V(wf(:,60),wf(:,-1),wf(:,73))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,74))
  call vert_AV_Q(wf(:,-1),wf(:,74),wf(:,75))
  call prop_A_Q(wf(:,75),Q(:,14),ZERO,0_intkind1,wf(:,76))
  call vert_VQ_A(wf(:,74),wf(:,0),wf(:,77))
  call prop_Q_A(wf(:,77),Q(:,13),ZERO,0_intkind1,wf(:,78))
  call counter_AV_Q(wf(:,-1),wf(:,23),wf(:,79))
  call prop_Q_A(wf(:,28),Q(:,49),ZERO,0_intkind1,wf(:,80))
  call counter_AW_Q(wf(:,-1),wf(:,-5),wf(:,81))
  call prop_A_Q(wf(:,81),Q(:,34),ZERO,0_intkind1,wf(:,82))
  call vert_QA_V(wf(:,24),wf(:,82),wf(:,83))
  call counter_AV_Q(wf(:,-1),wf(:,13),wf(:,84))
  call counter_AZ_Q(gZu,wf(:,-1),wf(:,15),wf(:,85))
  call vert_AW_Q(wf(:,82),wf(:,-4),wf(:,86))
  call counter_QA_V(wf(:,4),wf(:,-1),wf(:,87))
  call vert_QA_V(wf(:,-2),wf(:,82),wf(:,88))
  call counter_VQ_A(wf(:,23),wf(:,0),wf(:,89))
  call prop_A_Q(wf(:,31),Q(:,50),ZERO,0_intkind1,wf(:,90))
  call prop_A_Q(wf(:,33),Q(:,50),ZERO,0_intkind1,wf(:,91))
  call prop_A_Q(wf(:,34),Q(:,50),ZERO,0_intkind1,wf(:,92))
  call counter_VQ_A(wf(:,13),wf(:,0),wf(:,93))
  call counter_ZQ_A(gZu,wf(:,15),wf(:,0),wf(:,94))
  call counter_WQ_A(wf(:,-4),wf(:,0),wf(:,95))
  call prop_Q_A(wf(:,95),Q(:,17),ZERO,0_intkind1,wf(:,96))
  call vert_QA_V(wf(:,96),wf(:,25),wf(:,97))
  call vert_WQ_A(wf(:,-5),wf(:,96),wf(:,98))
  call counter_QA_V(wf(:,0),wf(:,5),wf(:,99))
  call vert_QA_V(wf(:,96),wf(:,-3),wf(:,100))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,101))
  call vert_VQ_A(wf(:,101),wf(:,4),wf(:,102))
  call vert_VQ_A(wf(:,101),wf(:,-2),wf(:,103))
  call prop_Q_A(wf(:,103),Q(:,7),ZERO,0_intkind1,wf(:,104))
  call vert_AV_Q(wf(:,-3),wf(:,101),wf(:,105))
  call prop_A_Q(wf(:,105),Q(:,11),ZERO,0_intkind1,wf(:,106))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,107))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,108))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,109))
  call counter_Q_A(ctqq,wf(:,4),Q(:,36),wf(:,110))
  call prop_A_Q(wf(:,109),Q(:,27),ZERO,0_intkind1,wf(:,111))
  call counter_A_Q(ctqq,wf(:,5),Q(:,24),wf(:,112))
  call prop_Q_A(wf(:,6),Q(:,39),ZERO,0_intkind1,wf(:,113))
  call vert_VQ_A(wf(:,108),wf(:,-2),wf(:,114))
  call counter_Q_A(ctqq,wf(:,9),Q(:,7),wf(:,115))
  call prop_A_Q(wf(:,112),Q(:,24),ZERO,0_intkind1,wf(:,116))
  call vert_AW_Q(wf(:,116),wf(:,-5),wf(:,117))
  call vert_AV_Q(wf(:,-3),wf(:,108),wf(:,118))
  call prop_Q_A(wf(:,110),Q(:,36),ZERO,0_intkind1,wf(:,119))
  call vert_WQ_A(wf(:,-4),wf(:,119),wf(:,120))
  call counter_A_Q(ctqq,wf(:,12),Q(:,11),wf(:,121))
  call counter_Q_A(ctqq,wf(:,18),Q(:,52),wf(:,122))
  call counter_Q_A(ctqq,wf(:,20),Q(:,52),wf(:,123))
  call vert_AV_Q(wf(:,25),wf(:,23),wf(:,124))
  call counter_Q_A(ctqq,wf(:,24),Q(:,17),wf(:,125))
  call prop_A_Q(wf(:,124),Q(:,46),ZERO,0_intkind1,wf(:,126))
  call vert_VQ_A(wf(:,23),wf(:,24),wf(:,127))
  call counter_A_Q(ctqq,wf(:,25),Q(:,34),wf(:,128))
  call prop_Q_A(wf(:,127),Q(:,29),ZERO,0_intkind1,wf(:,129))
  call counter_V_V(ctGG,wf(:,23),Q(:,12),wf(:,130))
  call prop_Q_A(wf(:,125),Q(:,17),ZERO,0_intkind1,wf(:,131))
  call vert_WQ_A(wf(:,-5),wf(:,131),wf(:,132))
  call vert_AV_Q(wf(:,-1),wf(:,130),wf(:,133))
  call counter_A_Q(ctqq,wf(:,29),Q(:,14),wf(:,134))
  call vert_VQ_A(wf(:,130),wf(:,0),wf(:,135))
  call counter_Q_A(ctqq,wf(:,32),Q(:,13),wf(:,136))
  call prop_A_Q(wf(:,128),Q(:,34),ZERO,0_intkind1,wf(:,137))
  call vert_AW_Q(wf(:,137),wf(:,-4),wf(:,138))
  call counter_Q_A(ctqq,wf(:,36),Q(:,49),wf(:,139))
  call counter_Q_A(ctqq,wf(:,38),Q(:,49),wf(:,140))
  call vert_QA_V(wf(:,131),wf(:,-3),wf(:,141))
  call vert_QA_V(wf(:,-2),wf(:,137),wf(:,142))
  call counter_V_V(ctGG,wf(:,39),Q(:,38),wf(:,143))
  call vert_QA_V(wf(:,119),wf(:,-1),wf(:,144))
  call counter_V_V(ctGG,wf(:,41),Q(:,38),wf(:,145))
  call counter_V_V(ctGG,wf(:,42),Q(:,25),wf(:,146))
  call vert_QA_V(wf(:,0),wf(:,116),wf(:,147))
  call vert_WQ_A(wf(:,-5),wf(:,9),wf(:,148))
  call prop_Q_A(wf(:,148),Q(:,39),ZERO,0_intkind1,wf(:,149))
  call vert_AW_Q(wf(:,12),wf(:,-4),wf(:,150))
  call prop_A_Q(wf(:,150),Q(:,27),ZERO,0_intkind1,wf(:,151))
  call vert_WQ_A(wf(:,-4),wf(:,32),wf(:,152))
  call prop_Q_A(wf(:,152),Q(:,29),ZERO,0_intkind1,wf(:,153))
  call vert_AW_Q(wf(:,29),wf(:,-5),wf(:,154))
  call prop_A_Q(wf(:,154),Q(:,46),ZERO,0_intkind1,wf(:,155))
  call vert_AV_Q(wf(:,-1),wf(:,40),wf(:,156))
  call prop_A_Q(wf(:,156),Q(:,27),ZERO,0_intkind1,wf(:,157))
  call vert_VQ_A(wf(:,40),wf(:,-2),wf(:,158))
  call prop_Q_A(wf(:,158),Q(:,29),ZERO,0_intkind1,wf(:,159))
  call vert_QA_V(wf(:,80),wf(:,-1),wf(:,160))
  call vert_AV_Q(wf(:,-1),wf(:,42),wf(:,161))
  call prop_A_Q(wf(:,161),Q(:,27),ZERO,0_intkind1,wf(:,162))
  call vert_VQ_A(wf(:,42),wf(:,-2),wf(:,163))
  call prop_Q_A(wf(:,163),Q(:,29),ZERO,0_intkind1,wf(:,164))
  call vert_QA_V(wf(:,-2),wf(:,58),wf(:,165))
  call vert_VQ_A(wf(:,39),wf(:,0),wf(:,166))
  call prop_Q_A(wf(:,166),Q(:,39),ZERO,0_intkind1,wf(:,167))
  call vert_QA_V(wf(:,0),wf(:,90),wf(:,168))
  call vert_AV_Q(wf(:,-3),wf(:,39),wf(:,169))
  call prop_A_Q(wf(:,169),Q(:,46),ZERO,0_intkind1,wf(:,170))
  call vert_VQ_A(wf(:,41),wf(:,0),wf(:,171))
  call prop_Q_A(wf(:,171),Q(:,39),ZERO,0_intkind1,wf(:,172))
  call vert_AV_Q(wf(:,-3),wf(:,41),wf(:,173))
  call prop_A_Q(wf(:,173),Q(:,46),ZERO,0_intkind1,wf(:,174))
  call vert_QA_V(wf(:,51),wf(:,-3),wf(:,175))
  call vert_QA_V(wf(:,36),wf(:,-1),wf(:,176))
  call vert_QA_V(wf(:,38),wf(:,-1),wf(:,177))
  call vert_QA_V(wf(:,0),wf(:,91),wf(:,178))
  call vert_QA_V(wf(:,0),wf(:,92),wf(:,179))
  call vert_QA_V(wf(:,18),wf(:,-3),wf(:,180))
  call vert_QA_V(wf(:,20),wf(:,-3),wf(:,181))
  call vert_QA_V(wf(:,-2),wf(:,64),wf(:,182))
  call vert_QA_V(wf(:,-2),wf(:,65),wf(:,183))

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
  den(2) = 1 / (Q(5,36))
  den(3) = 1 / (Q(5,24))
  den(6) = 1 / (Q(5,7))
  den(9) = 1 / (Q(5,11))
  den(12) = 1 / (Q(5,48))
  den(14) = 1 / (Q(5,48) - MZ2)
  den(16) = 1 / (Q(5,52))
  den(21) = 1 / (Q(5,17))
  den(22) = 1 / (Q(5,34))
  den(23) = 1 / (Q(5,12))
  den(26) = 1 / (Q(5,14))
  den(29) = 1 / (Q(5,13))
  den(34) = 1 / (Q(5,49))
  den(39) = 1 / (Q(5,38))
  den(44) = 1 / (Q(5,25))
  den(49) = 1 / (Q(5,48) - MH2)
  den(54) = 1 / (Q(5,56))
  den(68) = 1 / (Q(5,50))
  den(80) = 1 / (Q(5,60))
  den(84) = 1 / (Q(5,27))
  den(87) = 1 / (Q(5,39))
  den(108) = 1 / (Q(5,46))
  den(112) = 1 / (Q(5,29))
  den(115) = 1 / (Q(5,51))

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
  den(24) = den(21)*den(22)
  den(25) = den(23)*den(24)
  den(27) = den(23)*den(26)
  den(28) = den(21)*den(27)
  den(30) = den(23)*den(29)
  den(31) = den(22)*den(30)
  den(32) = den(12)*den(30)
  den(33) = den(14)*den(30)
  den(35) = den(12)*den(34)
  den(36) = den(23)*den(35)
  den(37) = den(14)*den(34)
  den(38) = den(23)*den(37)
  den(40) = den(22)*den(39)
  den(41) = den(21)*den(40)
  den(42) = den(2)*den(39)
  den(43) = den(21)*den(42)
  den(45) = den(3)*den(44)
  den(46) = den(22)*den(45)
  den(47) = den(2)*den(45)
  den(48) = den(1)*den(23)
  den(50) = den(48)*den(49)
  den(51) = den(14)*den(48)
  den(52) = den(2)*den(16)
  den(53) = den(1)*den(52)
  den(55) = den(3)*den(54)
  den(56) = den(1)*den(55)
  den(57) = den(10)*den(12)
  den(58) = den(10)*den(14)
  den(59) = den(12)*den(54)
  den(60) = den(1)*den(59)
  den(61) = den(14)*den(54)
  den(62) = den(1)*den(61)
  den(63) = den(21)*den(44)
  den(64) = den(22)*den(63)
  den(65) = den(21)*den(34)
  den(66) = den(23)*den(65)
  den(67) = den(2)*den(63)
  den(69) = den(22)*den(68)
  den(70) = den(23)*den(69)
  den(71) = den(12)*den(68)
  den(72) = den(23)*den(71)
  den(73) = den(14)*den(68)
  den(74) = den(23)*den(73)
  den(75) = den(12)*den(27)
  den(76) = den(14)*den(27)
  den(77) = den(3)*den(40)
  den(78) = den(3)*den(42)
  den(79) = den(2)*den(3)
  den(81) = den(79)*den(80)
  den(82) = den(1)*den(81)
  den(83) = den(1)*den(3)
  den(85) = den(83)*den(84)
  den(86) = den(2)*den(85)
  den(88) = den(4)*den(87)
  den(89) = den(3)*den(88)
  den(90) = den(1)**2
  den(91) = den(55)*den(90)
  den(92) = den(7)*den(55)
  den(93) = den(3)**2
  den(94) = den(7)*den(93)
  den(95) = den(52)*den(90)
  den(96) = den(2)**2
  den(97) = den(10)*den(96)
  den(98) = den(10)*den(52)
  den(99) = den(59)*den(90)
  den(100) = den(61)*den(90)
  den(101) = den(17)*den(90)
  den(102) = den(19)*den(90)
  den(103) = den(7)*den(59)
  den(104) = den(7)*den(61)
  den(105) = den(10)*den(17)
  den(106) = den(10)*den(19)
  den(107) = den(22)*den(23)
  den(109) = den(107)*den(108)
  den(110) = den(21)*den(109)
  den(111) = den(21)*den(23)
  den(113) = den(111)*den(112)
  den(114) = den(22)*den(113)
  den(116) = den(24)*den(115)
  den(117) = den(23)*den(116)
  den(118) = den(21)**2
  den(119) = den(27)*den(118)
  den(120) = den(23)**2
  den(121) = den(65)*den(120)
  den(122) = den(27)*den(65)
  den(123) = den(69)*den(120)
  den(124) = den(30)*den(69)
  den(125) = den(22)**2
  den(126) = den(30)*den(125)
  den(127) = den(30)*den(71)
  den(128) = den(30)*den(73)
  den(129) = den(27)*den(35)
  den(130) = den(27)*den(37)
  den(131) = den(71)*den(120)
  den(132) = den(73)*den(120)
  den(133) = den(35)*den(120)
  den(134) = den(37)*den(120)
  den(135) = den(40)*den(118)
  den(136) = den(63)*den(125)
  den(137) = den(40)*den(63)
  den(138) = den(42)*den(118)
  den(139) = den(63)*den(96)
  den(140) = den(42)*den(63)
  den(141) = den(40)*den(45)
  den(142) = den(40)*den(93)
  den(143) = den(45)*den(125)
  den(144) = den(42)*den(45)
  den(145) = den(42)*den(93)
  den(146) = den(45)*den(96)
  den(147) = den(7)*den(87)
  den(148) = den(10)*den(84)
  den(149) = den(30)*den(112)
  den(150) = den(27)*den(108)
  den(151) = den(63)*den(84)
  den(152) = den(63)*den(112)
  den(153) = den(65)*den(115)
  den(154) = den(45)*den(84)
  den(155) = den(45)*den(112)
  den(156) = den(55)*den(80)
  den(157) = den(40)*den(87)
  den(158) = den(69)*den(115)
  den(159) = den(40)*den(108)
  den(160) = den(42)*den(87)
  den(161) = den(42)*den(108)
  den(162) = den(52)*den(80)
  den(163) = den(35)*den(115)
  den(164) = den(37)*den(115)
  den(165) = den(71)*den(115)
  den(166) = den(73)*den(115)
  den(167) = den(17)*den(80)
  den(168) = den(19)*den(80)
  den(169) = den(59)*den(80)
  den(170) = den(61)*den(80)
  den(171) = den(1)*den(23)*den(49)
  den(172) = den(1)*den(12)*den(23)
  den(173) = den(1)*den(14)*den(23)
  den(174) = den(1)*den(2)*den(3)
  den(175) = den(1)*den(12)
  den(176) = den(1)*den(14)
  den(177) = den(21)*den(22)*den(23)
  den(178) = den(12)*den(23)
  den(179) = den(14)*den(23)
  den(180) = den(2)*den(21)
  den(181) = den(3)*den(22)
  den(182) = den(3)*den(147)
  den(183) = den(1)*den(156)
  den(184) = den(2)*den(148)
  den(185) = den(1)*den(162)
  den(186) = den(1)*den(167)
  den(187) = den(1)*den(168)
  den(188) = den(1)*den(169)
  den(189) = den(1)*den(170)
  den(190) = den(23)*den(153)
  den(191) = den(21)*den(150)
  den(192) = den(22)*den(149)
  den(193) = den(23)*den(158)
  den(194) = den(23)*den(163)
  den(195) = den(23)*den(164)
  den(196) = den(23)*den(165)
  den(197) = den(23)*den(166)
  den(198) = den(22)*den(152)
  den(199) = den(21)*den(159)
  den(200) = den(2)*den(151)
  den(201) = den(21)*den(161)
  den(202) = den(22)*den(155)
  den(203) = den(3)*den(157)
  den(204) = den(2)*den(154)
  den(205) = den(3)*den(160)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(131)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,11),wf(:,12)) * den(11)
  A(4) = cont_QA(wf(:,9),wf(:,14)) * den(13)
  A(5) = cont_QA(wf(:,9),wf(:,16)) * den(15)
  A(6) = cont_QA(wf(:,10),wf(:,18)) * den(18)
  A(7) = cont_QA(wf(:,10),wf(:,20)) * den(20)
  A(8) = cont_VV(wf(:,23),wf(:,26)) * den(25)
  A(9) = cont_QA(wf(:,28),wf(:,29)) * den(28)
  A(10) = cont_QA(wf(:,31),wf(:,32)) * den(31)
  A(11) = cont_QA(wf(:,32),wf(:,33)) * den(32)
  A(12) = cont_QA(wf(:,32),wf(:,34)) * den(33)
  A(13) = cont_QA(wf(:,27),wf(:,36)) * den(36)
  A(14) = cont_QA(wf(:,27),wf(:,38)) * den(38)
  A(15) = cont_VV(wf(:,39),wf(:,40)) * den(41)
  A(16) = cont_VV(wf(:,40),wf(:,41)) * den(43)
  A(17) = cont_VV(wf(:,39),wf(:,42)) * den(46)
  A(18) = cont_VV(wf(:,41),wf(:,42)) * den(47)

  A(19) = cont_VV(wf(:,23),wf(:,43)) * den(48)
  A(20) = cont_SS(wf(:,44),wf(:,45)) * den(50)
  A(21) = cont_VV(wf(:,15),wf(:,46)) * den(51)
  A(22) = cont_QA(wf(:,5),wf(:,47)) * den(5)
  A(23) = cont_QA(wf(:,9),wf(:,48)) * den(8)
  A(24) = cont_QA(wf(:,12),wf(:,49)) * den(11)
  A(25) = cont_QA(wf(:,50),wf(:,51)) * den(53)
  A(26) = cont_QA(wf(:,9),wf(:,52)) * den(13)
  A(27) = cont_QA(wf(:,9),wf(:,53)) * den(15)
  A(28) = cont_QA(wf(:,18),wf(:,50)) * den(18)
  A(29) = cont_QA(wf(:,20),wf(:,50)) * den(20)
  A(30) = cont_QA(wf(:,6),wf(:,55)) * den(5)
  A(31) = cont_QA(wf(:,9),wf(:,56)) * den(8)
  A(32) = cont_QA(wf(:,57),wf(:,58)) * den(56)
  A(33) = cont_QA(wf(:,5),wf(:,61)) * den(5)
  A(34) = cont_QA(wf(:,12),wf(:,62)) * den(57)
  A(35) = cont_QA(wf(:,12),wf(:,63)) * den(58)
  A(36) = cont_QA(wf(:,57),wf(:,64)) * den(60)
  A(37) = cont_QA(wf(:,57),wf(:,65)) * den(62)
  A(38) = cont_QA(wf(:,12),wf(:,66)) * den(11)
  A(39) = cont_VV(wf(:,23),wf(:,67)) * den(25)
  A(40) = cont_QA(wf(:,29),wf(:,68)) * den(28)
  A(41) = cont_QA(wf(:,32),wf(:,69)) * den(31)
  A(42) = cont_VV(wf(:,39),wf(:,70)) * den(41)
  A(43) = cont_VV(wf(:,41),wf(:,70)) * den(43)
  A(44) = cont_VV(wf(:,39),wf(:,71)) * den(46)
  A(45) = cont_VV(wf(:,41),wf(:,71)) * den(47)
  A(46) = cont_VV(wf(:,40),wf(:,72)) * den(64)
  A(47) = cont_VV(wf(:,40),wf(:,73)) * den(43)
  A(48) = cont_VV(wf(:,42),wf(:,72)) * den(46)
  A(49) = cont_VV(wf(:,42),wf(:,73)) * den(47)
  A(50) = cont_VV(wf(:,26),wf(:,74)) * den(25)
  A(51) = cont_QA(wf(:,28),wf(:,76)) * den(28)
  A(52) = cont_QA(wf(:,31),wf(:,78)) * den(31)
  A(53) = cont_QA(wf(:,36),wf(:,75)) * den(36)
  A(54) = cont_QA(wf(:,38),wf(:,75)) * den(38)
  A(55) = cont_QA(wf(:,33),wf(:,78)) * den(32)
  A(56) = cont_QA(wf(:,34),wf(:,78)) * den(33)
  A(57) = cont_QA(wf(:,79),wf(:,80)) * den(66)
  A(58) = cont_VV(wf(:,23),wf(:,83)) * den(25)
  A(59) = cont_QA(wf(:,32),wf(:,84)) * den(32)
  A(60) = cont_QA(wf(:,32),wf(:,85)) * den(33)
  A(61) = cont_QA(wf(:,36),wf(:,79)) * den(36)
  A(62) = cont_QA(wf(:,38),wf(:,79)) * den(38)
  A(63) = cont_QA(wf(:,32),wf(:,86)) * den(31)
  A(64) = cont_VV(wf(:,40),wf(:,87)) * den(67)
  A(65) = cont_VV(wf(:,40),wf(:,88)) * den(41)
  A(66) = cont_VV(wf(:,42),wf(:,87)) * den(47)
  A(67) = cont_VV(wf(:,42),wf(:,88)) * den(46)
  A(68) = cont_QA(wf(:,89),wf(:,90)) * den(70)
  A(69) = cont_QA(wf(:,89),wf(:,91)) * den(72)
  A(70) = cont_QA(wf(:,89),wf(:,92)) * den(74)
  A(71) = cont_QA(wf(:,29),wf(:,93)) * den(75)
  A(72) = cont_QA(wf(:,29),wf(:,94)) * den(76)
  A(73) = cont_VV(wf(:,23),wf(:,97)) * den(25)
  A(74) = cont_QA(wf(:,29),wf(:,98)) * den(28)
  A(75) = cont_VV(wf(:,39),wf(:,99)) * den(77)
  A(76) = cont_VV(wf(:,41),wf(:,99)) * den(78)
  A(77) = cont_VV(wf(:,39),wf(:,100)) * den(41)
  A(78) = cont_VV(wf(:,41),wf(:,100)) * den(43)
  A(79) = cont_QA(wf(:,5),wf(:,102)) * den(5)
  A(80) = cont_QA(wf(:,8),wf(:,104)) * den(8)
  A(81) = cont_QA(wf(:,11),wf(:,106)) * den(11)
  A(82) = cont_QA(wf(:,14),wf(:,104)) * den(13)
  A(83) = cont_QA(wf(:,16),wf(:,104)) * den(15)
  A(84) = cont_QA(wf(:,18),wf(:,105)) * den(18)
  A(85) = cont_QA(wf(:,20),wf(:,105)) * den(20)
  A(86) = cont_VV(wf(:,107),wf(:,108)) * den(82)
  A(87) = cont_QA(wf(:,110),wf(:,111)) * den(86)
  A(88) = cont_QA(wf(:,112),wf(:,113)) * den(89)
  A(89) = cont_QA(wf(:,58),wf(:,114)) * den(91)
  A(90) = cont_QA(wf(:,58),wf(:,115)) * den(92)
  A(91) = cont_QA(wf(:,9),wf(:,117)) * den(94)
  A(92) = cont_QA(wf(:,51),wf(:,118)) * den(95)
  A(93) = cont_QA(wf(:,12),wf(:,120)) * den(97)
  A(94) = cont_QA(wf(:,51),wf(:,121)) * den(98)
  A(95) = cont_QA(wf(:,64),wf(:,114)) * den(99)
  A(96) = cont_QA(wf(:,65),wf(:,114)) * den(100)
  A(97) = cont_QA(wf(:,18),wf(:,118)) * den(101)
  A(98) = cont_QA(wf(:,20),wf(:,118)) * den(102)
  A(99) = cont_QA(wf(:,64),wf(:,115)) * den(103)
  A(100) = cont_QA(wf(:,65),wf(:,115)) * den(104)
  A(101) = cont_QA(wf(:,12),wf(:,122)) * den(105)
  A(102) = cont_QA(wf(:,12),wf(:,123)) * den(106)
  A(103) = cont_QA(wf(:,125),wf(:,126)) * den(110)
  A(104) = cont_QA(wf(:,128),wf(:,129)) * den(114)
  A(105) = cont_VV(wf(:,26),wf(:,130)) * den(117)
  A(106) = cont_QA(wf(:,29),wf(:,132)) * den(119)
  A(107) = cont_QA(wf(:,80),wf(:,133)) * den(121)
  A(108) = cont_QA(wf(:,80),wf(:,134)) * den(122)
  A(109) = cont_QA(wf(:,90),wf(:,135)) * den(123)
  A(110) = cont_QA(wf(:,90),wf(:,136)) * den(124)
  A(111) = cont_QA(wf(:,32),wf(:,138)) * den(126)
  A(112) = cont_QA(wf(:,91),wf(:,136)) * den(127)
  A(113) = cont_QA(wf(:,92),wf(:,136)) * den(128)
  A(114) = cont_QA(wf(:,29),wf(:,139)) * den(129)
  A(115) = cont_QA(wf(:,29),wf(:,140)) * den(130)
  A(116) = cont_QA(wf(:,91),wf(:,135)) * den(131)
  A(117) = cont_QA(wf(:,92),wf(:,135)) * den(132)
  A(118) = cont_QA(wf(:,36),wf(:,133)) * den(133)
  A(119) = cont_QA(wf(:,38),wf(:,133)) * den(134)
  A(120) = cont_VV(wf(:,39),wf(:,141)) * den(135)
  A(121) = cont_VV(wf(:,40),wf(:,142)) * den(136)
  A(122) = cont_VV(wf(:,40),wf(:,143)) * den(137)
  A(123) = cont_VV(wf(:,41),wf(:,141)) * den(138)
  A(124) = cont_VV(wf(:,40),wf(:,144)) * den(139)
  A(125) = cont_VV(wf(:,40),wf(:,145)) * den(140)
  A(126) = cont_VV(wf(:,39),wf(:,146)) * den(141)
  A(127) = cont_VV(wf(:,39),wf(:,147)) * den(142)
  A(128) = cont_VV(wf(:,42),wf(:,142)) * den(143)
  A(129) = cont_VV(wf(:,41),wf(:,146)) * den(144)
  A(130) = cont_VV(wf(:,41),wf(:,147)) * den(145)
  A(131) = cont_VV(wf(:,42),wf(:,144)) * den(146)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(131)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(4)-A(6))*f(1))/2._/**/REALKIND+((A(11)+A(13))*f(2))/2._/**/REALKIND+((A(1)+A(2)+A(3)+A(8)+A(9) &
       +A(10))*f(10))/2._/**/REALKIND+((A(15)+A(16)+A(17)+A(18))*f(10))/6._/**/REALKIND+((A(5)+A(7)+A(12) &
       +A(14))*f(14))/2._/**/REALKIND
  M1(2) = ((A(4)+A(6))*f(1))/6._/**/REALKIND+((-A(11)-A(13))*f(2))/6._/**/REALKIND+((-A(1)-A(2)-A(3)-A(8)-A(9) &
       -A(10))*f(10))/6._/**/REALKIND+((-A(15)-A(16)-A(17)-A(18))*f(10))/2._/**/REALKIND+((-A(5)-A(7)-A(12) &
       -A(14))*f(14))/6._/**/REALKIND

  M2(1) = ((A(95)+A(97)+A(99)+A(101))*f(3))/2._/**/REALKIND+((-A(112)-A(114)-A(116)-A(118))*f(4))/2._/**/REALKIND+((-A(28)-A(36) &
       -A(82)-A(84))*f(5))/2._/**/REALKIND+((A(53)+A(55)+A(61)+A(69))*f(6))/2._/**/REALKIND+((-A(26)-A(34))*f(7))/2._/**/REALKIND &
       +((A(59)+A(71))*f(8))/2._/**/REALKIND+(A(19)*f(9))/2._/**/REALKIND+((-A(86)-A(87)-A(88)-A(89)-A(90)-A(91)-A(92)-A(93)-A(94) &
       -A(103)-A(104)-A(105)-A(106)-A(107)-A(108)-A(109)-A(110)-A(111))*f(11))/2._/**/REALKIND+((-A(120)-A(121)-A(122)-A(123) &
       -A(124)-A(125)-A(126)-A(127)-A(128)-A(129)-A(130)-A(131))*f(11))/6._/**/REALKIND+((A(42)+A(43)+A(46)+A(48)+A(64)+A(66) &
       +A(75)+A(76))*f(12))/6._/**/REALKIND+((A(22)+A(25)+A(32)+A(39)+A(50)+A(51)+A(52)+A(57)+A(68)+A(79)+A(80) &
       +A(81))*f(12))/2._/**/REALKIND+((A(23)+A(24)+A(30)+A(31)+A(33)+A(38)+A(40)+A(41)+A(58)+A(63)+A(73) &
       +A(74))*f(13))/2._/**/REALKIND+((A(44)+A(45)+A(47)+A(49)+A(65)+A(67)+A(77)+A(78))*f(13))/6._/**/REALKIND+((-A(96)-A(98) &
       -A(100)-A(102)-A(113)-A(115)-A(117)-A(119))*f(15))/2._/**/REALKIND+((A(29)+A(37)+A(54)+A(56)+A(62)+A(70)+A(83) &
       +A(85))*f(16))/2._/**/REALKIND+((A(27)+A(35)+A(60)+A(72))*f(17))/2._/**/REALKIND-(A(21)*f(18))/2._/**/REALKIND &
       -(A(20)*f(19))/2._/**/REALKIND
  M2(2) = ((-A(95)-A(97)-A(99)-A(101))*f(3))/6._/**/REALKIND+((A(112)+A(114)+A(116)+A(118))*f(4))/6._/**/REALKIND+((A(28)+A(36) &
       +A(82)+A(84))*f(5))/6._/**/REALKIND+((-A(53)-A(55)-A(61)-A(69))*f(6))/6._/**/REALKIND+((A(26)+A(34))*f(7))/6._/**/REALKIND &
       +((-A(59)-A(71))*f(8))/6._/**/REALKIND-(A(19)*f(9))/6._/**/REALKIND+((A(86)+A(87)+A(88)+A(89)+A(90)+A(91)+A(92)+A(93)+A(94) &
       +A(103)+A(104)+A(105)+A(106)+A(107)+A(108)+A(109)+A(110)+A(111))*f(11))/6._/**/REALKIND+((A(120)+A(121)+A(122)+A(123) &
       +A(124)+A(125)+A(126)+A(127)+A(128)+A(129)+A(130)+A(131))*f(11))/2._/**/REALKIND+((-A(42)-A(43)-A(46)-A(48)-A(64)-A(66) &
       -A(75)-A(76))*f(12))/2._/**/REALKIND+((-A(22)-A(25)-A(32)-A(39)-A(50)-A(51)-A(52)-A(57)-A(68)-A(79)-A(80) &
       -A(81))*f(12))/6._/**/REALKIND+((-A(23)-A(24)-A(30)-A(31)-A(33)-A(38)-A(40)-A(41)-A(58)-A(63)-A(73) &
       -A(74))*f(13))/6._/**/REALKIND+((-A(44)-A(45)-A(47)-A(49)-A(65)-A(67)-A(77)-A(78))*f(13))/2._/**/REALKIND+((A(96)+A(98) &
       +A(100)+A(102)+A(113)+A(115)+A(117)+A(119))*f(15))/6._/**/REALKIND+((-A(29)-A(37)-A(54)-A(56)-A(62)-A(70)-A(83) &
       -A(85))*f(16))/6._/**/REALKIND+((-A(27)-A(35)-A(60)-A(72))*f(17))/6._/**/REALKIND+(A(21)*f(18))/6._/**/REALKIND &
       +(A(20)*f(19))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppwwjj_uuxddxwwx_1_/**/REALKIND
