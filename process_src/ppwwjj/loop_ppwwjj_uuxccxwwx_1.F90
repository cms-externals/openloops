
module ol_colourmatrix_ppwwjj_uuxccxwwx_1_/**/REALKIND
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
end module ol_colourmatrix_ppwwjj_uuxccxwwx_1_/**/REALKIND



module ol_forced_parameters_ppwwjj_uuxccxwwx_1_/**/REALKIND
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
end module ol_forced_parameters_ppwwjj_uuxccxwwx_1_/**/REALKIND

module ol_loop_ppwwjj_uuxccxwwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(39), c(57)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:148)
  ! denominators
  complex(REALKIND), save :: den(161)
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
    f( 1) = (2*CI*eQED**2*gQCD**2)/3._/**/REALKIND
    f( 2) = (2*CI*countertermnorm*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 3) = (2*CI*countertermnorm*ctGcc*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 4) = (2*CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 5) = (2*CI*countertermnorm*ctVcc*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 6) = (2*CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 7) = CI*countertermnorm*ctWWGG*eQED**2*gQCD**4
    f( 8) = (CI*eQED**2*gQCD**2)/(2._/**/REALKIND*sw**2)
    f( 9) = (CI*countertermnorm*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(10) = (CI*countertermnorm*ctGcc*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(11) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(12) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(13) = (CI*countertermnorm*ctVsc*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(14) = (CI*cw*eQED**2*gQCD**2)/sw
    f(15) = (CI*countertermnorm*cw*eQED**2*gQCD**4)/sw
    f(16) = (CI*countertermnorm*ctGcc*cw*eQED**2*gQCD**4)/sw
    f(17) = (CI*countertermnorm*ctGqq*cw*eQED**2*gQCD**4)/sw
    f(18) = (CI*countertermnorm*ctVcc*cw*eQED**2*gQCD**4)/sw
    f(19) = (CI*countertermnorm*ctVqq*cw*eQED**2*gQCD**4)/sw
    f(20) = (countertermnorm*ctZGG*cw*eQED**2*gQCD**4)/sw
    f(21) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MW)/sw
    f(22) = (2*CI*eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
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

  c = [ 9*CI*f(22), 27*CI*f(22), 18*f(23), 54*f(23), f(24), 3*f(24), 6*f(24), 8*f(24), 10*f(24), 18*f(24), 21*f(24), 24*f(24) &
    , 54*f(24), 9*CI*f(25), 27*CI*f(25), 18*f(26), 54*f(26), f(27), 3*f(27), 6*f(27), 8*f(27), 10*f(27), 18*f(27), 21*f(27) &
    , 24*f(27), 54*f(27), 9*CI*f(28), 27*CI*f(28), 18*f(29), 54*f(29), f(30), 3*f(30), 6*f(30), 8*f(30), 10*f(30), 18*f(30) &
    , 21*f(30), 24*f(30), 54*f(30), 3*f(31), 9*f(31), 3*f(32), 9*f(32), 3*f(33), 9*f(33), 3*f(34), 9*f(34), 3*f(35), 9*f(35) &
    , 3*f(36), 9*f(36), 3*f(37), 9*f(37), 3*f(38), 9*f(38), 3*f(39), 9*f(39) ]
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
  complex(REALKIND) :: A(99)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rMW, H(5), wf(:,-4))
  call wf_V(P(:,6), rMW, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_WQ_A(wf(:,-4),wf(:,-2),wf(:,2))
  call vert_AW_Q(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,20),ZERO,0_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,40),ZERO,0_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,6))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,7))
  call vert_WQ_A(wf(:,-5),wf(:,4),wf(:,8))
  call prop_A_Q(wf(:,7),Q(:,11),ZERO,0_intkind1,wf(:,9))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,10))
  call vert_AW_Q(wf(:,5),wf(:,-4),wf(:,11))
  call prop_Q_A(wf(:,10),Q(:,7),ZERO,0_intkind1,wf(:,12))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-4),Q(:,16),wf(:,13))
  call vert_AV_Q(wf(:,-3),wf(:,13),wf(:,14))
  call prop_W_W(wf(:,13),Q(:,48),MZ,1_intkind1,wf(:,15))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,15),wf(:,16))
  call vert_VQ_A(wf(:,13),wf(:,-2),wf(:,17))
  call prop_Q_A(wf(:,17),Q(:,52),ZERO,0_intkind1,wf(:,18))
  call vert_ZQ_A(gZu,wf(:,15),wf(:,-2),wf(:,19))
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
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,1),wf(:,39))
  call vert_VV_S(wf(:,-4),wf(:,-5),wf(:,40))
  call counter_GG_S(wf(:,1),wf(:,23),wf(:,41))
  call counter_GG_V(wf(:,1),Q(:,3),wf(:,23),Q(:,12),wf(:,42))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,43))
  call counter_WQ_A(wf(:,-5),wf(:,4),wf(:,44))
  call counter_AW_Q(wf(:,5),wf(:,-4),wf(:,45))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,46))
  call prop_Q_A(wf(:,8),Q(:,52),ZERO,0_intkind1,wf(:,47))
  call counter_AW_Q(wf(:,-3),wf(:,-5),wf(:,48))
  call prop_A_Q(wf(:,48),Q(:,40),ZERO,0_intkind1,wf(:,49))
  call counter_AV_Q(wf(:,-3),wf(:,13),wf(:,50))
  call counter_AZ_Q(gZu,wf(:,-3),wf(:,15),wf(:,51))
  call vert_AW_Q(wf(:,49),wf(:,-4),wf(:,52))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,53))
  call prop_A_Q(wf(:,11),Q(:,56),ZERO,0_intkind1,wf(:,54))
  call counter_VQ_A(wf(:,13),wf(:,-2),wf(:,55))
  call counter_ZQ_A(gZu,wf(:,15),wf(:,-2),wf(:,56))
  call prop_A_Q(wf(:,14),Q(:,56),ZERO,0_intkind1,wf(:,57))
  call prop_A_Q(wf(:,16),Q(:,56),ZERO,0_intkind1,wf(:,58))
  call counter_WQ_A(wf(:,-4),wf(:,-2),wf(:,59))
  call prop_Q_A(wf(:,59),Q(:,20),ZERO,0_intkind1,wf(:,60))
  call vert_VQ_A(wf(:,1),wf(:,60),wf(:,61))
  call vert_WQ_A(wf(:,-5),wf(:,60),wf(:,62))
  call counter_QA_V(wf(:,24),wf(:,25),wf(:,63))
  call counter_WQ_A(wf(:,-5),wf(:,24),wf(:,64))
  call counter_AW_Q(wf(:,25),wf(:,-4),wf(:,65))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,66))
  call vert_AV_Q(wf(:,-1),wf(:,66),wf(:,67))
  call prop_A_Q(wf(:,67),Q(:,14),ZERO,0_intkind1,wf(:,68))
  call vert_VQ_A(wf(:,66),wf(:,0),wf(:,69))
  call prop_Q_A(wf(:,69),Q(:,13),ZERO,0_intkind1,wf(:,70))
  call counter_AV_Q(wf(:,-1),wf(:,23),wf(:,71))
  call prop_Q_A(wf(:,28),Q(:,49),ZERO,0_intkind1,wf(:,72))
  call counter_AW_Q(wf(:,-1),wf(:,-5),wf(:,73))
  call prop_A_Q(wf(:,73),Q(:,34),ZERO,0_intkind1,wf(:,74))
  call vert_QA_V(wf(:,24),wf(:,74),wf(:,75))
  call counter_AV_Q(wf(:,-1),wf(:,13),wf(:,76))
  call counter_AZ_Q(gZu,wf(:,-1),wf(:,15),wf(:,77))
  call vert_AW_Q(wf(:,74),wf(:,-4),wf(:,78))
  call counter_VQ_A(wf(:,23),wf(:,0),wf(:,79))
  call prop_A_Q(wf(:,31),Q(:,50),ZERO,0_intkind1,wf(:,80))
  call prop_A_Q(wf(:,33),Q(:,50),ZERO,0_intkind1,wf(:,81))
  call prop_A_Q(wf(:,34),Q(:,50),ZERO,0_intkind1,wf(:,82))
  call counter_VQ_A(wf(:,13),wf(:,0),wf(:,83))
  call counter_ZQ_A(gZu,wf(:,15),wf(:,0),wf(:,84))
  call counter_WQ_A(wf(:,-4),wf(:,0),wf(:,85))
  call prop_Q_A(wf(:,85),Q(:,17),ZERO,0_intkind1,wf(:,86))
  call vert_QA_V(wf(:,86),wf(:,25),wf(:,87))
  call vert_WQ_A(wf(:,-5),wf(:,86),wf(:,88))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,89))
  call vert_VQ_A(wf(:,89),wf(:,4),wf(:,90))
  call vert_AV_Q(wf(:,-3),wf(:,89),wf(:,91))
  call prop_A_Q(wf(:,91),Q(:,11),ZERO,0_intkind1,wf(:,92))
  call vert_VQ_A(wf(:,89),wf(:,-2),wf(:,93))
  call prop_Q_A(wf(:,93),Q(:,7),ZERO,0_intkind1,wf(:,94))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,95))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,96))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,97))
  call counter_Q_A(ctqq,wf(:,4),Q(:,20),wf(:,98))
  call prop_A_Q(wf(:,97),Q(:,43),ZERO,0_intkind1,wf(:,99))
  call counter_A_Q(ctqq,wf(:,5),Q(:,40),wf(:,100))
  call prop_Q_A(wf(:,6),Q(:,23),ZERO,0_intkind1,wf(:,101))
  call vert_AV_Q(wf(:,-3),wf(:,96),wf(:,102))
  call prop_Q_A(wf(:,98),Q(:,20),ZERO,0_intkind1,wf(:,103))
  call vert_WQ_A(wf(:,-5),wf(:,103),wf(:,104))
  call counter_A_Q(ctcc,wf(:,9),Q(:,11),wf(:,105))
  call vert_VQ_A(wf(:,96),wf(:,-2),wf(:,106))
  call counter_Q_A(ctcc,wf(:,12),Q(:,7),wf(:,107))
  call prop_A_Q(wf(:,100),Q(:,40),ZERO,0_intkind1,wf(:,108))
  call vert_AW_Q(wf(:,108),wf(:,-4),wf(:,109))
  call counter_Q_A(ctcc,wf(:,18),Q(:,52),wf(:,110))
  call counter_Q_A(ctcc,wf(:,20),Q(:,52),wf(:,111))
  call vert_AV_Q(wf(:,25),wf(:,23),wf(:,112))
  call counter_Q_A(ctqq,wf(:,24),Q(:,17),wf(:,113))
  call prop_A_Q(wf(:,112),Q(:,46),ZERO,0_intkind1,wf(:,114))
  call vert_VQ_A(wf(:,23),wf(:,24),wf(:,115))
  call counter_A_Q(ctqq,wf(:,25),Q(:,34),wf(:,116))
  call prop_Q_A(wf(:,115),Q(:,29),ZERO,0_intkind1,wf(:,117))
  call counter_V_V(ctGG,wf(:,23),Q(:,12),wf(:,118))
  call prop_Q_A(wf(:,113),Q(:,17),ZERO,0_intkind1,wf(:,119))
  call vert_WQ_A(wf(:,-5),wf(:,119),wf(:,120))
  call vert_AV_Q(wf(:,-1),wf(:,118),wf(:,121))
  call counter_A_Q(ctqq,wf(:,29),Q(:,14),wf(:,122))
  call vert_VQ_A(wf(:,118),wf(:,0),wf(:,123))
  call counter_Q_A(ctqq,wf(:,32),Q(:,13),wf(:,124))
  call prop_A_Q(wf(:,116),Q(:,34),ZERO,0_intkind1,wf(:,125))
  call vert_AW_Q(wf(:,125),wf(:,-4),wf(:,126))
  call counter_Q_A(ctqq,wf(:,36),Q(:,49),wf(:,127))
  call counter_Q_A(ctqq,wf(:,38),Q(:,49),wf(:,128))
  call vert_WQ_A(wf(:,-4),wf(:,12),wf(:,129))
  call prop_Q_A(wf(:,129),Q(:,23),ZERO,0_intkind1,wf(:,130))
  call vert_AW_Q(wf(:,9),wf(:,-5),wf(:,131))
  call prop_A_Q(wf(:,131),Q(:,43),ZERO,0_intkind1,wf(:,132))
  call vert_WQ_A(wf(:,-4),wf(:,32),wf(:,133))
  call prop_Q_A(wf(:,133),Q(:,29),ZERO,0_intkind1,wf(:,134))
  call vert_AW_Q(wf(:,29),wf(:,-5),wf(:,135))
  call prop_A_Q(wf(:,135),Q(:,46),ZERO,0_intkind1,wf(:,136))
  call vert_QA_V(wf(:,72),wf(:,-1),wf(:,137))
  call vert_QA_V(wf(:,47),wf(:,-3),wf(:,138))
  call vert_QA_V(wf(:,0),wf(:,80),wf(:,139))
  call vert_QA_V(wf(:,-2),wf(:,54),wf(:,140))
  call vert_QA_V(wf(:,36),wf(:,-1),wf(:,141))
  call vert_QA_V(wf(:,38),wf(:,-1),wf(:,142))
  call vert_QA_V(wf(:,0),wf(:,81),wf(:,143))
  call vert_QA_V(wf(:,0),wf(:,82),wf(:,144))
  call vert_QA_V(wf(:,18),wf(:,-3),wf(:,145))
  call vert_QA_V(wf(:,20),wf(:,-3),wf(:,146))
  call vert_QA_V(wf(:,-2),wf(:,57),wf(:,147))
  call vert_QA_V(wf(:,-2),wf(:,58),wf(:,148))

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
  den(2) = 1 / (Q(5,20))
  den(3) = 1 / (Q(5,40))
  den(6) = 1 / (Q(5,11))
  den(9) = 1 / (Q(5,7))
  den(12) = 1 / (Q(5,48))
  den(14) = 1 / (Q(5,48) - MZ2)
  den(16) = 1 / (Q(5,52))
  den(21) = 1 / (Q(5,17))
  den(22) = 1 / (Q(5,34))
  den(23) = 1 / (Q(5,12))
  den(26) = 1 / (Q(5,14))
  den(29) = 1 / (Q(5,13))
  den(34) = 1 / (Q(5,49))
  den(40) = 1 / (Q(5,48) - MH2)
  den(45) = 1 / (Q(5,56))
  den(56) = 1 / (Q(5,50))
  den(66) = 1 / (Q(5,60))
  den(70) = 1 / (Q(5,43))
  den(73) = 1 / (Q(5,23))
  den(94) = 1 / (Q(5,46))
  den(98) = 1 / (Q(5,29))
  den(101) = 1 / (Q(5,51))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(1)*den(9)
  den(11) = den(3)*den(10)
  den(13) = den(10)*den(12)
  den(15) = den(10)*den(14)
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
  den(39) = den(1)*den(23)
  den(41) = den(39)*den(40)
  den(42) = den(14)*den(39)
  den(43) = den(2)*den(16)
  den(44) = den(1)*den(43)
  den(46) = den(3)*den(45)
  den(47) = den(1)*den(46)
  den(48) = den(7)*den(12)
  den(49) = den(7)*den(14)
  den(50) = den(12)*den(45)
  den(51) = den(1)*den(50)
  den(52) = den(14)*den(45)
  den(53) = den(1)*den(52)
  den(54) = den(21)*den(34)
  den(55) = den(23)*den(54)
  den(57) = den(22)*den(56)
  den(58) = den(23)*den(57)
  den(59) = den(12)*den(56)
  den(60) = den(23)*den(59)
  den(61) = den(14)*den(56)
  den(62) = den(23)*den(61)
  den(63) = den(12)*den(27)
  den(64) = den(14)*den(27)
  den(65) = den(2)*den(3)
  den(67) = den(65)*den(66)
  den(68) = den(1)*den(67)
  den(69) = den(1)*den(3)
  den(71) = den(69)*den(70)
  den(72) = den(2)*den(71)
  den(74) = den(4)*den(73)
  den(75) = den(3)*den(74)
  den(76) = den(1)**2
  den(77) = den(43)*den(76)
  den(78) = den(2)**2
  den(79) = den(7)*den(78)
  den(80) = den(7)*den(43)
  den(81) = den(46)*den(76)
  den(82) = den(10)*den(46)
  den(83) = den(3)**2
  den(84) = den(10)*den(83)
  den(85) = den(50)*den(76)
  den(86) = den(52)*den(76)
  den(87) = den(17)*den(76)
  den(88) = den(19)*den(76)
  den(89) = den(10)*den(50)
  den(90) = den(10)*den(52)
  den(91) = den(7)*den(17)
  den(92) = den(7)*den(19)
  den(93) = den(22)*den(23)
  den(95) = den(93)*den(94)
  den(96) = den(21)*den(95)
  den(97) = den(21)*den(23)
  den(99) = den(97)*den(98)
  den(100) = den(22)*den(99)
  den(102) = den(24)*den(101)
  den(103) = den(23)*den(102)
  den(104) = den(21)**2
  den(105) = den(27)*den(104)
  den(106) = den(23)**2
  den(107) = den(54)*den(106)
  den(108) = den(27)*den(54)
  den(109) = den(57)*den(106)
  den(110) = den(30)*den(57)
  den(111) = den(22)**2
  den(112) = den(30)*den(111)
  den(113) = den(30)*den(59)
  den(114) = den(30)*den(61)
  den(115) = den(27)*den(35)
  den(116) = den(27)*den(37)
  den(117) = den(59)*den(106)
  den(118) = den(61)*den(106)
  den(119) = den(35)*den(106)
  den(120) = den(37)*den(106)
  den(121) = den(10)*den(73)
  den(122) = den(7)*den(70)
  den(123) = den(30)*den(98)
  den(124) = den(27)*den(94)
  den(125) = den(54)*den(101)
  den(126) = den(43)*den(66)
  den(127) = den(57)*den(101)
  den(128) = den(46)*den(66)
  den(129) = den(35)*den(101)
  den(130) = den(37)*den(101)
  den(131) = den(59)*den(101)
  den(132) = den(61)*den(101)
  den(133) = den(17)*den(66)
  den(134) = den(19)*den(66)
  den(135) = den(50)*den(66)
  den(136) = den(52)*den(66)
  den(137) = den(1)*den(23)*den(40)
  den(138) = den(1)*den(12)*den(23)
  den(139) = den(1)*den(14)*den(23)
  den(140) = den(1)*den(2)*den(3)
  den(141) = den(1)*den(12)
  den(142) = den(1)*den(14)
  den(143) = den(21)*den(22)*den(23)
  den(144) = den(12)*den(23)
  den(145) = den(14)*den(23)
  den(146) = den(2)*den(122)
  den(147) = den(1)*den(126)
  den(148) = den(3)*den(121)
  den(149) = den(1)*den(128)
  den(150) = den(1)*den(133)
  den(151) = den(1)*den(134)
  den(152) = den(1)*den(135)
  den(153) = den(1)*den(136)
  den(154) = den(23)*den(125)
  den(155) = den(21)*den(124)
  den(156) = den(22)*den(123)
  den(157) = den(23)*den(127)
  den(158) = den(23)*den(129)
  den(159) = den(23)*den(130)
  den(160) = den(23)*den(131)
  den(161) = den(23)*den(132)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(99)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,11),wf(:,12)) * den(11)
  A(4) = cont_QA(wf(:,12),wf(:,14)) * den(13)
  A(5) = cont_QA(wf(:,12),wf(:,16)) * den(15)
  A(6) = cont_QA(wf(:,7),wf(:,18)) * den(18)
  A(7) = cont_QA(wf(:,7),wf(:,20)) * den(20)
  A(8) = cont_VV(wf(:,23),wf(:,26)) * den(25)
  A(9) = cont_QA(wf(:,28),wf(:,29)) * den(28)
  A(10) = cont_QA(wf(:,31),wf(:,32)) * den(31)
  A(11) = cont_QA(wf(:,32),wf(:,33)) * den(32)
  A(12) = cont_QA(wf(:,32),wf(:,34)) * den(33)
  A(13) = cont_QA(wf(:,27),wf(:,36)) * den(36)
  A(14) = cont_QA(wf(:,27),wf(:,38)) * den(38)

  A(15) = cont_VV(wf(:,23),wf(:,39)) * den(39)
  A(16) = cont_SS(wf(:,40),wf(:,41)) * den(41)
  A(17) = cont_VV(wf(:,15),wf(:,42)) * den(42)
  A(18) = cont_QA(wf(:,5),wf(:,43)) * den(5)
  A(19) = cont_QA(wf(:,9),wf(:,44)) * den(8)
  A(20) = cont_QA(wf(:,12),wf(:,45)) * den(11)
  A(21) = cont_QA(wf(:,46),wf(:,47)) * den(44)
  A(22) = cont_QA(wf(:,6),wf(:,49)) * den(5)
  A(23) = cont_QA(wf(:,12),wf(:,50)) * den(13)
  A(24) = cont_QA(wf(:,12),wf(:,51)) * den(15)
  A(25) = cont_QA(wf(:,18),wf(:,46)) * den(18)
  A(26) = cont_QA(wf(:,20),wf(:,46)) * den(20)
  A(27) = cont_QA(wf(:,12),wf(:,52)) * den(11)
  A(28) = cont_QA(wf(:,53),wf(:,54)) * den(47)
  A(29) = cont_QA(wf(:,9),wf(:,55)) * den(48)
  A(30) = cont_QA(wf(:,9),wf(:,56)) * den(49)
  A(31) = cont_QA(wf(:,53),wf(:,57)) * den(51)
  A(32) = cont_QA(wf(:,53),wf(:,58)) * den(53)
  A(33) = cont_QA(wf(:,5),wf(:,61)) * den(5)
  A(34) = cont_QA(wf(:,9),wf(:,62)) * den(8)
  A(35) = cont_VV(wf(:,23),wf(:,63)) * den(25)
  A(36) = cont_QA(wf(:,29),wf(:,64)) * den(28)
  A(37) = cont_QA(wf(:,32),wf(:,65)) * den(31)
  A(38) = cont_VV(wf(:,26),wf(:,66)) * den(25)
  A(39) = cont_QA(wf(:,28),wf(:,68)) * den(28)
  A(40) = cont_QA(wf(:,31),wf(:,70)) * den(31)
  A(41) = cont_QA(wf(:,36),wf(:,67)) * den(36)
  A(42) = cont_QA(wf(:,38),wf(:,67)) * den(38)
  A(43) = cont_QA(wf(:,33),wf(:,70)) * den(32)
  A(44) = cont_QA(wf(:,34),wf(:,70)) * den(33)
  A(45) = cont_QA(wf(:,71),wf(:,72)) * den(55)
  A(46) = cont_VV(wf(:,23),wf(:,75)) * den(25)
  A(47) = cont_QA(wf(:,32),wf(:,76)) * den(32)
  A(48) = cont_QA(wf(:,32),wf(:,77)) * den(33)
  A(49) = cont_QA(wf(:,36),wf(:,71)) * den(36)
  A(50) = cont_QA(wf(:,38),wf(:,71)) * den(38)
  A(51) = cont_QA(wf(:,32),wf(:,78)) * den(31)
  A(52) = cont_QA(wf(:,79),wf(:,80)) * den(58)
  A(53) = cont_QA(wf(:,79),wf(:,81)) * den(60)
  A(54) = cont_QA(wf(:,79),wf(:,82)) * den(62)
  A(55) = cont_QA(wf(:,29),wf(:,83)) * den(63)
  A(56) = cont_QA(wf(:,29),wf(:,84)) * den(64)
  A(57) = cont_VV(wf(:,23),wf(:,87)) * den(25)
  A(58) = cont_QA(wf(:,29),wf(:,88)) * den(28)
  A(59) = cont_QA(wf(:,5),wf(:,90)) * den(5)
  A(60) = cont_QA(wf(:,8),wf(:,92)) * den(8)
  A(61) = cont_QA(wf(:,11),wf(:,94)) * den(11)
  A(62) = cont_QA(wf(:,14),wf(:,94)) * den(13)
  A(63) = cont_QA(wf(:,16),wf(:,94)) * den(15)
  A(64) = cont_QA(wf(:,18),wf(:,91)) * den(18)
  A(65) = cont_QA(wf(:,20),wf(:,91)) * den(20)
  A(66) = cont_VV(wf(:,95),wf(:,96)) * den(68)
  A(67) = cont_QA(wf(:,98),wf(:,99)) * den(72)
  A(68) = cont_QA(wf(:,100),wf(:,101)) * den(75)
  A(69) = cont_QA(wf(:,47),wf(:,102)) * den(77)
  A(70) = cont_QA(wf(:,9),wf(:,104)) * den(79)
  A(71) = cont_QA(wf(:,47),wf(:,105)) * den(80)
  A(72) = cont_QA(wf(:,54),wf(:,106)) * den(81)
  A(73) = cont_QA(wf(:,54),wf(:,107)) * den(82)
  A(74) = cont_QA(wf(:,12),wf(:,109)) * den(84)
  A(75) = cont_QA(wf(:,57),wf(:,106)) * den(85)
  A(76) = cont_QA(wf(:,58),wf(:,106)) * den(86)
  A(77) = cont_QA(wf(:,18),wf(:,102)) * den(87)
  A(78) = cont_QA(wf(:,20),wf(:,102)) * den(88)
  A(79) = cont_QA(wf(:,57),wf(:,107)) * den(89)
  A(80) = cont_QA(wf(:,58),wf(:,107)) * den(90)
  A(81) = cont_QA(wf(:,9),wf(:,110)) * den(91)
  A(82) = cont_QA(wf(:,9),wf(:,111)) * den(92)
  A(83) = cont_QA(wf(:,113),wf(:,114)) * den(96)
  A(84) = cont_QA(wf(:,116),wf(:,117)) * den(100)
  A(85) = cont_VV(wf(:,26),wf(:,118)) * den(103)
  A(86) = cont_QA(wf(:,29),wf(:,120)) * den(105)
  A(87) = cont_QA(wf(:,72),wf(:,121)) * den(107)
  A(88) = cont_QA(wf(:,72),wf(:,122)) * den(108)
  A(89) = cont_QA(wf(:,80),wf(:,123)) * den(109)
  A(90) = cont_QA(wf(:,80),wf(:,124)) * den(110)
  A(91) = cont_QA(wf(:,32),wf(:,126)) * den(112)
  A(92) = cont_QA(wf(:,81),wf(:,124)) * den(113)
  A(93) = cont_QA(wf(:,82),wf(:,124)) * den(114)
  A(94) = cont_QA(wf(:,29),wf(:,127)) * den(115)
  A(95) = cont_QA(wf(:,29),wf(:,128)) * den(116)
  A(96) = cont_QA(wf(:,81),wf(:,123)) * den(117)
  A(97) = cont_QA(wf(:,82),wf(:,123)) * den(118)
  A(98) = cont_QA(wf(:,36),wf(:,121)) * den(119)
  A(99) = cont_QA(wf(:,38),wf(:,121)) * den(120)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(99)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(4)+A(6)+A(11)+A(13))*f(1))/2._/**/REALKIND+((A(1)+A(2)+A(3)+A(8)+A(9)+A(10))*f(8))/2._/**/REALKIND+((A(5)+A(7)+A(12) &
       +A(14))*f(14))/2._/**/REALKIND
  M1(2) = ((-A(4)-A(6)-A(11)-A(13))*f(1))/6._/**/REALKIND+((-A(1)-A(2)-A(3)-A(8)-A(9)-A(10))*f(8))/6._/**/REALKIND+((-A(5)-A(7) &
       -A(12)-A(14))*f(14))/6._/**/REALKIND

  M2(1) = ((-A(75)-A(77)-A(79)-A(81)-A(92)-A(94)-A(96)-A(98))*f(2))/2._/**/REALKIND+((A(25)+A(31)+A(41) &
       +A(43))*f(3))/2._/**/REALKIND+((A(49)+A(53)+A(62)+A(64))*f(4))/2._/**/REALKIND+((A(23)+A(29))*f(5))/2._/**/REALKIND+((A(47) &
       +A(55))*f(6))/2._/**/REALKIND+(A(15)*f(7))/2._/**/REALKIND+((-A(66)-A(67)-A(68)-A(69)-A(70)-A(71)-A(72)-A(73)-A(74)-A(83) &
       -A(84)-A(85)-A(86)-A(87)-A(88)-A(89)-A(90)-A(91))*f(9))/2._/**/REALKIND+((A(21)+A(28)+A(38)+A(39) &
       +A(40))*f(10))/2._/**/REALKIND+((A(18)+A(35)+A(45)+A(52)+A(59)+A(60)+A(61))*f(11))/2._/**/REALKIND+((A(19)+A(22)+A(27) &
       +A(36)+A(37)+A(46)+A(51)+A(57)+A(58))*f(12))/2._/**/REALKIND+((A(20)+A(33)+A(34))*f(13))/2._/**/REALKIND+((-A(76)-A(78) &
       -A(80)-A(82)-A(93)-A(95)-A(97)-A(99))*f(15))/2._/**/REALKIND+((A(26)+A(32)+A(42)+A(44))*f(16))/2._/**/REALKIND+((A(50) &
       +A(54)+A(63)+A(65))*f(17))/2._/**/REALKIND+((A(24)+A(30))*f(18))/2._/**/REALKIND+((A(48)+A(56))*f(19))/2._/**/REALKIND &
       -(A(17)*f(20))/2._/**/REALKIND-(A(16)*f(21))/2._/**/REALKIND
  M2(2) = ((A(75)+A(77)+A(79)+A(81)+A(92)+A(94)+A(96)+A(98))*f(2))/6._/**/REALKIND+((-A(25)-A(31)-A(41) &
       -A(43))*f(3))/6._/**/REALKIND+((-A(49)-A(53)-A(62)-A(64))*f(4))/6._/**/REALKIND+((-A(23)-A(29))*f(5))/6._/**/REALKIND+(( &
       -A(47)-A(55))*f(6))/6._/**/REALKIND-(A(15)*f(7))/6._/**/REALKIND+((A(66)+A(67)+A(68)+A(69)+A(70)+A(71)+A(72)+A(73)+A(74) &
       +A(83)+A(84)+A(85)+A(86)+A(87)+A(88)+A(89)+A(90)+A(91))*f(9))/6._/**/REALKIND+((-A(21)-A(28)-A(38)-A(39) &
       -A(40))*f(10))/6._/**/REALKIND+((-A(18)-A(35)-A(45)-A(52)-A(59)-A(60)-A(61))*f(11))/6._/**/REALKIND+((-A(19)-A(22)-A(27) &
       -A(36)-A(37)-A(46)-A(51)-A(57)-A(58))*f(12))/6._/**/REALKIND+((-A(20)-A(33)-A(34))*f(13))/6._/**/REALKIND+((A(76)+A(78) &
       +A(80)+A(82)+A(93)+A(95)+A(97)+A(99))*f(15))/6._/**/REALKIND+((-A(26)-A(32)-A(42)-A(44))*f(16))/6._/**/REALKIND+((-A(50) &
       -A(54)-A(63)-A(65))*f(17))/6._/**/REALKIND+((-A(24)-A(30))*f(18))/6._/**/REALKIND+((-A(48)-A(56))*f(19))/6._/**/REALKIND &
       +(A(17)*f(20))/6._/**/REALKIND+(A(16)*f(21))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppwwjj_uuxccxwwx_1_/**/REALKIND
