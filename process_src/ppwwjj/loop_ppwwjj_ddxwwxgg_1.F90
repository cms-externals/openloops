
module ol_colourmatrix_ppwwjj_ddxwwxgg_1_/**/REALKIND
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
end module ol_colourmatrix_ppwwjj_ddxwwxgg_1_/**/REALKIND



module ol_forced_parameters_ppwwjj_ddxwwxgg_1_/**/REALKIND
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
end module ol_forced_parameters_ppwwjj_ddxwwxgg_1_/**/REALKIND

module ol_loop_ppwwjj_ddxwwxgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(61), c(60)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:292)
  ! denominators
  complex(REALKIND), save :: den(345)
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
    f( 2) = (eQED**2*gQCD**2)/3._/**/REALKIND
    f( 3) = (CI*countertermnorm*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 4) = CI*countertermnorm*eQED**2*gQCD**4
    f( 5) = (countertermnorm*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 6) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 7) = (countertermnorm*ctGqq*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 8) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 9) = (countertermnorm*ctVqq*eQED**2*gQCD**4)/3._/**/REALKIND
    f(10) = (countertermnorm*ctVVV*eQED**2*gQCD**4)/3._/**/REALKIND
    f(11) = CI*countertermnorm*ctWWGG*eQED**2*gQCD**4
    f(12) = countertermnorm*ctWWGG*eQED**2*gQCD**4
    f(13) = (CI*eQED**2*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(14) = (eQED**2*gQCD**2)/(sw**2*2._/**/REALKIND)
    f(15) = (CI*countertermnorm*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(16) = (countertermnorm*eQED**2*gQCD**4)/(sw**2*2._/**/REALKIND)
    f(17) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(18) = (countertermnorm*ctGqq*eQED**2*gQCD**4)/(sw**2*2._/**/REALKIND)
    f(19) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(20) = (countertermnorm*ctVqq*eQED**2*gQCD**4)/(sw**2*2._/**/REALKIND)
    f(21) = (countertermnorm*ctVVV*eQED**2*gQCD**4)/(sw**2*2._/**/REALKIND)
    f(22) = (CI*cw*eQED**2*gQCD**2)/sw
    f(23) = (cw*eQED**2*gQCD**2)/sw
    f(24) = (CI*countertermnorm*cw*eQED**2*gQCD**4)/sw
    f(25) = (countertermnorm*cw*eQED**2*gQCD**4)/sw
    f(26) = (CI*countertermnorm*ctGqq*cw*eQED**2*gQCD**4)/sw
    f(27) = (countertermnorm*ctGqq*cw*eQED**2*gQCD**4)/sw
    f(28) = (CI*countertermnorm*ctVqq*cw*eQED**2*gQCD**4)/sw
    f(29) = (countertermnorm*ctVqq*cw*eQED**2*gQCD**4)/sw
    f(30) = (countertermnorm*ctVVV*cw*eQED**2*gQCD**4)/sw
    f(31) = (CI*countertermnorm*ctZGG*cw*eQED**2*gQCD**4)/sw
    f(32) = (countertermnorm*ctZGG*cw*eQED**2*gQCD**4)/sw
    f(33) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MW)/sw
    f(34) = (countertermnorm*ctHGG*eQED**2*gQCD**4*MW)/sw
    f(35) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/6._/**/REALKIND
    f(36) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(37) = (eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(38) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(4._/**/REALKIND*sw**2)
    f(39) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(40) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(41) = (CI*cw*eQED**2*gQCD**4*integralnorm*SwB)/(2._/**/REALKIND*sw)
    f(42) = (CI*cw*eQED**2*gQCD**4*integralnorm*SwB)/sw
    f(43) = (cw*eQED**2*gQCD**4*integralnorm*SwB)/sw
    f(44) = (CI*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(45) = (2*CI*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(46) = (4*CI*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(47) = (eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(48) = (2*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(49) = (4*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(50) = (CI*eQED**2*gQCD**4*integralnorm*SwF)/(2._/**/REALKIND*sw**2)
    f(51) = (CI*eQED**2*gQCD**4*integralnorm*SwF)/sw**2
    f(52) = (eQED**2*gQCD**4*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(53) = (eQED**2*gQCD**4*integralnorm*SwF)/sw**2
    f(54) = (CI*eQED**2*gQCD**4*integralnorm*MB*SwF)/(2._/**/REALKIND*sw**2)
    f(55) = (eQED**2*gQCD**4*integralnorm*MB*SwF)/(sw**2*2._/**/REALKIND)
    f(56) = (CI*eQED**2*gQCD**4*integralnorm*MT*SwF)/(2._/**/REALKIND*sw**2)
    f(57) = (eQED**2*gQCD**4*integralnorm*MT*SwF)/(sw**2*2._/**/REALKIND)
    f(58) = (CI*cw*eQED**2*gQCD**4*integralnorm*SwF)/sw
    f(59) = (2*CI*cw*eQED**2*gQCD**4*integralnorm*SwF)/sw
    f(60) = (cw*eQED**2*gQCD**4*integralnorm*SwF)/sw
    f(61) = (2*cw*eQED**2*gQCD**4*integralnorm*SwF)/sw

  c = [ 9*CI*f(35), 18*CI*f(35), CI*f(36), 3*CI*f(36), 8*CI*f(36), 9*CI*f(36), 18*CI*f(36), f(37), 3*f(37), 8*f(37), 9*f(37) &
    , 9*CI*f(38), 18*CI*f(38), CI*f(39), 3*CI*f(39), 8*CI*f(39), 9*CI*f(39), 18*CI*f(39), f(40), 3*f(40), 8*f(40), 9*f(40) &
    , 9*CI*f(41), 18*CI*f(41), CI*f(42), 3*CI*f(42), 8*CI*f(42), 9*CI*f(42), 18*CI*f(42), f(43), 3*f(43), 8*f(43), 9*f(43) &
    , 3*CI*f(44), 3*CI*f(45), 3*CI*f(46), f(47), 3*f(47), f(48), 3*f(48), f(49), 3*f(49), 3*CI*f(50), 3*CI*f(51), f(52), 3*f(52) &
    , f(53), 3*f(53), 3*CI*f(54), f(55), 3*f(55), 3*CI*f(56), f(57), 3*f(57), 3*CI*f(58), 3*CI*f(59), f(60), 3*f(60), f(61) &
    , 3*f(61) ]
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
  complex(REALKIND) :: A(241)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_V(P(:,3), rMW, H(3), wf(:,-2))
  call wf_V(P(:,4), rMW, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_WQ_A(wf(:,-3),wf(:,0),wf(:,1))
  call vert_AW_Q(wf(:,-1),wf(:,-2),wf(:,2))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,3))
  call prop_Q_A(wf(:,1),Q(:,9),ZERO,0_intkind1,wf(:,4))
  call prop_A_Q(wf(:,2),Q(:,6),ZERO,0_intkind1,wf(:,5))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,6))
  call vert_VQ_A(wf(:,-4),wf(:,4),wf(:,7))
  call vert_AV_Q(wf(:,5),wf(:,-5),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,25),ZERO,0_intkind1,wf(:,9))
  call vert_AV_Q(wf(:,5),wf(:,-4),wf(:,10))
  call vert_VQ_A(wf(:,-5),wf(:,4),wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,22),ZERO,0_intkind1,wf(:,12))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,13))
  call prop_Q_A(wf(:,13),Q(:,17),ZERO,0_intkind1,wf(:,14))
  call vert_WQ_A(wf(:,-3),wf(:,14),wf(:,15))
  call prop_Q_A(wf(:,15),Q(:,25),ZERO,0_intkind1,wf(:,16))
  call vert_AW_Q(wf(:,5),wf(:,-3),wf(:,17))
  call vert_VQ_A(wf(:,-5),wf(:,14),wf(:,18))
  call prop_A_Q(wf(:,17),Q(:,14),ZERO,0_intkind1,wf(:,19))
  call vert_VQ_A(wf(:,-5),wf(:,0),wf(:,20))
  call prop_Q_A(wf(:,20),Q(:,33),ZERO,0_intkind1,wf(:,21))
  call vert_WQ_A(wf(:,-3),wf(:,21),wf(:,22))
  call prop_Q_A(wf(:,22),Q(:,41),ZERO,0_intkind1,wf(:,23))
  call vert_VQ_A(wf(:,-4),wf(:,21),wf(:,24))
  call vert_VQ_A(wf(:,3),wf(:,0),wf(:,25))
  call prop_Q_A(wf(:,25),Q(:,49),ZERO,0_intkind1,wf(:,26))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,27))
  call prop_A_Q(wf(:,27),Q(:,18),ZERO,0_intkind1,wf(:,28))
  call vert_WQ_A(wf(:,-2),wf(:,4),wf(:,29))
  call vert_AV_Q(wf(:,28),wf(:,-5),wf(:,30))
  call prop_Q_A(wf(:,29),Q(:,13),ZERO,0_intkind1,wf(:,31))
  call vert_AW_Q(wf(:,28),wf(:,-2),wf(:,32))
  call prop_A_Q(wf(:,32),Q(:,22),ZERO,0_intkind1,wf(:,33))
  call vert_AV_Q(wf(:,-1),wf(:,-5),wf(:,34))
  call prop_A_Q(wf(:,34),Q(:,34),ZERO,0_intkind1,wf(:,35))
  call vert_AV_Q(wf(:,35),wf(:,-4),wf(:,36))
  call vert_AW_Q(wf(:,35),wf(:,-2),wf(:,37))
  call prop_A_Q(wf(:,37),Q(:,38),ZERO,0_intkind1,wf(:,38))
  call vert_AV_Q(wf(:,-1),wf(:,3),wf(:,39))
  call prop_A_Q(wf(:,39),Q(:,50),ZERO,0_intkind1,wf(:,40))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-2),Q(:,4),wf(:,41))
  call vert_QA_V(wf(:,14),wf(:,35),wf(:,42))
  call prop_W_W(wf(:,41),Q(:,12),MZ,1_intkind1,wf(:,43))
  call vert_QA_Z(gZd,wf(:,14),wf(:,35),wf(:,44))
  call vert_AV_Q(wf(:,-1),wf(:,41),wf(:,45))
  call prop_A_Q(wf(:,45),Q(:,14),ZERO,0_intkind1,wf(:,46))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,43),wf(:,47))
  call prop_A_Q(wf(:,47),Q(:,14),ZERO,0_intkind1,wf(:,48))
  call vert_QA_V(wf(:,21),wf(:,28),wf(:,49))
  call vert_QA_Z(gZd,wf(:,21),wf(:,28),wf(:,50))
  call vert_VQ_A(wf(:,41),wf(:,0),wf(:,51))
  call prop_Q_A(wf(:,51),Q(:,13),ZERO,0_intkind1,wf(:,52))
  call vert_ZQ_A(gZd,wf(:,43),wf(:,0),wf(:,53))
  call prop_Q_A(wf(:,53),Q(:,13),ZERO,0_intkind1,wf(:,54))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,55))
  call counter_GGG_V(ctAGGG,wf(:,55),wf(:,-4),wf(:,-5),wf(:,56))
  call counter_GGG_V(ctAGGG,wf(:,55),wf(:,-5),wf(:,-4),wf(:,57))
  call counter_GGG_V(ctZGGG,wf(:,55),wf(:,-4),wf(:,-5),wf(:,58))
  call counter_GGG_V(ctZGGG,wf(:,55),wf(:,-5),wf(:,-4),wf(:,59))
  call counter_VVG_G(wf(:,-2),wf(:,-3),wf(:,55),wf(:,60))
  call counter_VVG_G(wf(:,-2),wf(:,-3),wf(:,-5),wf(:,61))
  call vert_UV_W(wf(:,55),Q(:,3),wf(:,-4),Q(:,16),wf(:,62))
  call counter_VVG_G(wf(:,-2),wf(:,-3),wf(:,-4),wf(:,63))
  call vert_UV_W(wf(:,55),Q(:,3),wf(:,-5),Q(:,32),wf(:,64))
  call vert_QA_V(wf(:,14),wf(:,-1),wf(:,65))
  call vert_QA_V(wf(:,0),wf(:,28),wf(:,66))
  call vert_QA_V(wf(:,21),wf(:,-1),wf(:,67))
  call vert_QA_V(wf(:,0),wf(:,35),wf(:,68))
  call vert_VV_S(wf(:,-2),wf(:,-3),wf(:,69))
  call counter_SG_G(wf(:,69),wf(:,55),wf(:,70))
  call counter_VG_G(wf(:,43),wf(:,55),Q(:,3),wf(:,71),Q(:,15))
  call counter_SG_G(wf(:,69),wf(:,-5),wf(:,72))
  call counter_VG_G(wf(:,43),wf(:,-5),Q(:,32),wf(:,73),Q(:,44))
  call counter_SG_G(wf(:,69),wf(:,-4),wf(:,74))
  call counter_VG_G(wf(:,43),wf(:,-4),Q(:,16),wf(:,75),Q(:,28))
  call counter_QA_V(wf(:,4),wf(:,5),wf(:,76))
  call counter_AV_Q(wf(:,5),wf(:,-5),wf(:,77))
  call counter_VQ_A(wf(:,-5),wf(:,4),wf(:,78))
  call counter_AV_Q(wf(:,5),wf(:,-4),wf(:,79))
  call prop_Q_A(wf(:,11),Q(:,41),ZERO,0_intkind1,wf(:,80))
  call counter_VQ_A(wf(:,-4),wf(:,4),wf(:,81))
  call prop_A_Q(wf(:,8),Q(:,38),ZERO,0_intkind1,wf(:,82))
  call counter_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,83))
  call counter_VQ_A(wf(:,-5),wf(:,14),wf(:,84))
  call counter_VQ_A(wf(:,-4),wf(:,21),wf(:,85))
  call vert_VQ_A(wf(:,83),wf(:,0),wf(:,86))
  call prop_Q_A(wf(:,86),Q(:,49),ZERO,0_intkind1,wf(:,87))
  call counter_AW_Q(wf(:,5),wf(:,-3),wf(:,88))
  call prop_Q_A(wf(:,18),Q(:,49),ZERO,0_intkind1,wf(:,89))
  call counter_WQ_A(wf(:,-3),wf(:,14),wf(:,90))
  call prop_Q_A(wf(:,24),Q(:,49),ZERO,0_intkind1,wf(:,91))
  call counter_WQ_A(wf(:,-3),wf(:,21),wf(:,92))
  call counter_AV_Q(wf(:,28),wf(:,-5),wf(:,93))
  call counter_AV_Q(wf(:,35),wf(:,-4),wf(:,94))
  call vert_AV_Q(wf(:,-1),wf(:,83),wf(:,95))
  call prop_A_Q(wf(:,95),Q(:,50),ZERO,0_intkind1,wf(:,96))
  call counter_QA_V(wf(:,14),wf(:,35),wf(:,97))
  call counter_QA_Z(gZd,wf(:,14),wf(:,35),wf(:,98))
  call counter_QA_V(wf(:,21),wf(:,28),wf(:,99))
  call counter_QA_Z(gZd,wf(:,21),wf(:,28),wf(:,100))
  call counter_AW_Q(wf(:,28),wf(:,-2),wf(:,101))
  call counter_WQ_A(wf(:,-2),wf(:,4),wf(:,102))
  call prop_A_Q(wf(:,30),Q(:,50),ZERO,0_intkind1,wf(:,103))
  call counter_AW_Q(wf(:,35),wf(:,-2),wf(:,104))
  call prop_A_Q(wf(:,36),Q(:,50),ZERO,0_intkind1,wf(:,105))
  call counter_AV_Q(wf(:,-1),wf(:,3),wf(:,106))
  call counter_AV_Q(wf(:,-1),wf(:,-5),wf(:,107))
  call prop_A_Q(wf(:,107),Q(:,34),ZERO,0_intkind1,wf(:,108))
  call vert_AV_Q(wf(:,108),wf(:,-4),wf(:,109))
  call vert_AW_Q(wf(:,108),wf(:,-2),wf(:,110))
  call prop_A_Q(wf(:,110),Q(:,38),ZERO,0_intkind1,wf(:,111))
  call counter_AV_Q(wf(:,-1),wf(:,-4),wf(:,112))
  call prop_A_Q(wf(:,112),Q(:,18),ZERO,0_intkind1,wf(:,113))
  call vert_AV_Q(wf(:,113),wf(:,-5),wf(:,114))
  call vert_AW_Q(wf(:,113),wf(:,-2),wf(:,115))
  call prop_A_Q(wf(:,115),Q(:,22),ZERO,0_intkind1,wf(:,116))
  call counter_AV_Q(wf(:,-1),wf(:,41),wf(:,117))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,43),wf(:,118))
  call vert_QA_V(wf(:,14),wf(:,108),wf(:,119))
  call vert_QA_Z(gZd,wf(:,14),wf(:,108),wf(:,120))
  call vert_QA_V(wf(:,21),wf(:,113),wf(:,121))
  call vert_QA_Z(gZd,wf(:,21),wf(:,113),wf(:,122))
  call counter_AW_Q(wf(:,-1),wf(:,-2),wf(:,123))
  call prop_A_Q(wf(:,123),Q(:,6),ZERO,0_intkind1,wf(:,124))
  call vert_QA_V(wf(:,4),wf(:,124),wf(:,125))
  call vert_AV_Q(wf(:,124),wf(:,-5),wf(:,126))
  call vert_AV_Q(wf(:,124),wf(:,-4),wf(:,127))
  call prop_A_Q(wf(:,127),Q(:,22),ZERO,0_intkind1,wf(:,128))
  call vert_AW_Q(wf(:,124),wf(:,-3),wf(:,129))
  call prop_A_Q(wf(:,129),Q(:,14),ZERO,0_intkind1,wf(:,130))
  call counter_VQ_A(wf(:,3),wf(:,0),wf(:,131))
  call counter_VQ_A(wf(:,-5),wf(:,0),wf(:,132))
  call prop_Q_A(wf(:,132),Q(:,33),ZERO,0_intkind1,wf(:,133))
  call vert_WQ_A(wf(:,-3),wf(:,133),wf(:,134))
  call prop_Q_A(wf(:,134),Q(:,41),ZERO,0_intkind1,wf(:,135))
  call vert_VQ_A(wf(:,-4),wf(:,133),wf(:,136))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,137))
  call prop_Q_A(wf(:,137),Q(:,17),ZERO,0_intkind1,wf(:,138))
  call vert_WQ_A(wf(:,-3),wf(:,138),wf(:,139))
  call prop_Q_A(wf(:,139),Q(:,25),ZERO,0_intkind1,wf(:,140))
  call vert_VQ_A(wf(:,-5),wf(:,138),wf(:,141))
  call counter_WQ_A(wf(:,-3),wf(:,0),wf(:,142))
  call prop_Q_A(wf(:,142),Q(:,9),ZERO,0_intkind1,wf(:,143))
  call vert_QA_V(wf(:,143),wf(:,5),wf(:,144))
  call vert_VQ_A(wf(:,-4),wf(:,143),wf(:,145))
  call prop_Q_A(wf(:,145),Q(:,25),ZERO,0_intkind1,wf(:,146))
  call vert_VQ_A(wf(:,-5),wf(:,143),wf(:,147))
  call counter_VQ_A(wf(:,41),wf(:,0),wf(:,148))
  call counter_ZQ_A(gZd,wf(:,43),wf(:,0),wf(:,149))
  call vert_QA_V(wf(:,133),wf(:,28),wf(:,150))
  call vert_QA_Z(gZd,wf(:,133),wf(:,28),wf(:,151))
  call vert_QA_V(wf(:,138),wf(:,35),wf(:,152))
  call vert_QA_Z(gZd,wf(:,138),wf(:,35),wf(:,153))
  call vert_WQ_A(wf(:,-2),wf(:,143),wf(:,154))
  call prop_Q_A(wf(:,154),Q(:,13),ZERO,0_intkind1,wf(:,155))
  call vert_AV_Q(wf(:,5),wf(:,3),wf(:,156))
  call counter_Q_A(ctqq,wf(:,4),Q(:,9),wf(:,157))
  call prop_A_Q(wf(:,156),Q(:,54),ZERO,0_intkind1,wf(:,158))
  call vert_VQ_A(wf(:,3),wf(:,4),wf(:,159))
  call counter_A_Q(ctqq,wf(:,5),Q(:,6),wf(:,160))
  call prop_Q_A(wf(:,159),Q(:,57),ZERO,0_intkind1,wf(:,161))
  call counter_V_V(ctGG,wf(:,3),Q(:,48),wf(:,162))
  call prop_Q_A(wf(:,157),Q(:,9),ZERO,0_intkind1,wf(:,163))
  call vert_VQ_A(wf(:,-4),wf(:,163),wf(:,164))
  call vert_VQ_A(wf(:,-5),wf(:,163),wf(:,165))
  call prop_A_Q(wf(:,160),Q(:,6),ZERO,0_intkind1,wf(:,166))
  call vert_AV_Q(wf(:,166),wf(:,-4),wf(:,167))
  call vert_AV_Q(wf(:,166),wf(:,-5),wf(:,168))
  call counter_Q_A(ctqq,wf(:,9),Q(:,25),wf(:,169))
  call counter_A_Q(ctqq,wf(:,12),Q(:,22),wf(:,170))
  call counter_Q_A(ctqq,wf(:,14),Q(:,17),wf(:,171))
  call prop_Q_A(wf(:,171),Q(:,17),ZERO,0_intkind1,wf(:,172))
  call vert_WQ_A(wf(:,-3),wf(:,172),wf(:,173))
  call vert_VQ_A(wf(:,-5),wf(:,172),wf(:,174))
  call vert_AW_Q(wf(:,166),wf(:,-3),wf(:,175))
  call counter_Q_A(ctqq,wf(:,16),Q(:,25),wf(:,176))
  call counter_A_Q(ctqq,wf(:,19),Q(:,14),wf(:,177))
  call counter_Q_A(ctqq,wf(:,21),Q(:,33),wf(:,178))
  call prop_Q_A(wf(:,178),Q(:,33),ZERO,0_intkind1,wf(:,179))
  call vert_WQ_A(wf(:,-3),wf(:,179),wf(:,180))
  call vert_VQ_A(wf(:,-4),wf(:,179),wf(:,181))
  call counter_Q_A(ctqq,wf(:,23),Q(:,41),wf(:,182))
  call counter_Q_A(ctqq,wf(:,26),Q(:,49),wf(:,183))
  call vert_VQ_A(wf(:,162),wf(:,0),wf(:,184))
  call vert_WQ_A(wf(:,-2),wf(:,163),wf(:,185))
  call counter_A_Q(ctqq,wf(:,28),Q(:,18),wf(:,186))
  call prop_A_Q(wf(:,186),Q(:,18),ZERO,0_intkind1,wf(:,187))
  call vert_AW_Q(wf(:,187),wf(:,-2),wf(:,188))
  call vert_AV_Q(wf(:,187),wf(:,-5),wf(:,189))
  call counter_Q_A(ctqq,wf(:,31),Q(:,13),wf(:,190))
  call counter_A_Q(ctqq,wf(:,33),Q(:,22),wf(:,191))
  call counter_A_Q(ctqq,wf(:,35),Q(:,34),wf(:,192))
  call prop_A_Q(wf(:,192),Q(:,34),ZERO,0_intkind1,wf(:,193))
  call vert_AW_Q(wf(:,193),wf(:,-2),wf(:,194))
  call vert_AV_Q(wf(:,193),wf(:,-4),wf(:,195))
  call counter_A_Q(ctqq,wf(:,38),Q(:,38),wf(:,196))
  call counter_A_Q(ctqq,wf(:,40),Q(:,50),wf(:,197))
  call vert_AV_Q(wf(:,-1),wf(:,162),wf(:,198))
  call vert_AV_Q(wf(:,35),wf(:,41),wf(:,199))
  call prop_A_Q(wf(:,199),Q(:,46),ZERO,0_intkind1,wf(:,200))
  call vert_AZ_Q(gZd,wf(:,35),wf(:,43),wf(:,201))
  call prop_A_Q(wf(:,201),Q(:,46),ZERO,0_intkind1,wf(:,202))
  call vert_VQ_A(wf(:,41),wf(:,14),wf(:,203))
  call prop_Q_A(wf(:,203),Q(:,29),ZERO,0_intkind1,wf(:,204))
  call vert_ZQ_A(gZd,wf(:,43),wf(:,14),wf(:,205))
  call prop_Q_A(wf(:,205),Q(:,29),ZERO,0_intkind1,wf(:,206))
  call counter_A_Q(ctqq,wf(:,46),Q(:,14),wf(:,207))
  call counter_A_Q(ctqq,wf(:,48),Q(:,14),wf(:,208))
  call vert_AV_Q(wf(:,28),wf(:,41),wf(:,209))
  call prop_A_Q(wf(:,209),Q(:,30),ZERO,0_intkind1,wf(:,210))
  call vert_AZ_Q(gZd,wf(:,28),wf(:,43),wf(:,211))
  call prop_A_Q(wf(:,211),Q(:,30),ZERO,0_intkind1,wf(:,212))
  call vert_VQ_A(wf(:,41),wf(:,21),wf(:,213))
  call prop_Q_A(wf(:,213),Q(:,45),ZERO,0_intkind1,wf(:,214))
  call vert_ZQ_A(gZd,wf(:,43),wf(:,21),wf(:,215))
  call prop_Q_A(wf(:,215),Q(:,45),ZERO,0_intkind1,wf(:,216))
  call counter_Q_A(ctqq,wf(:,52),Q(:,13),wf(:,217))
  call counter_Q_A(ctqq,wf(:,54),Q(:,13),wf(:,218))
  call vert_QA_V(wf(:,0),wf(:,19),wf(:,219))
  call vert_AV_Q(wf(:,19),wf(:,-4),wf(:,220))
  call prop_A_Q(wf(:,220),Q(:,30),ZERO,0_intkind1,wf(:,221))
  call vert_AV_Q(wf(:,19),wf(:,-5),wf(:,222))
  call prop_A_Q(wf(:,222),Q(:,46),ZERO,0_intkind1,wf(:,223))
  call vert_AW_Q(wf(:,12),wf(:,-3),wf(:,224))
  call prop_A_Q(wf(:,224),Q(:,30),ZERO,0_intkind1,wf(:,225))
  call vert_AV_Q(wf(:,12),wf(:,-5),wf(:,226))
  call prop_A_Q(wf(:,226),Q(:,54),ZERO,0_intkind1,wf(:,227))
  call vert_AW_Q(wf(:,82),wf(:,-3),wf(:,228))
  call prop_A_Q(wf(:,228),Q(:,46),ZERO,0_intkind1,wf(:,229))
  call vert_AV_Q(wf(:,82),wf(:,-4),wf(:,230))
  call prop_A_Q(wf(:,230),Q(:,54),ZERO,0_intkind1,wf(:,231))
  call vert_QA_V(wf(:,31),wf(:,-1),wf(:,232))
  call vert_VQ_A(wf(:,-4),wf(:,31),wf(:,233))
  call prop_Q_A(wf(:,233),Q(:,29),ZERO,0_intkind1,wf(:,234))
  call vert_VQ_A(wf(:,-5),wf(:,31),wf(:,235))
  call prop_Q_A(wf(:,235),Q(:,45),ZERO,0_intkind1,wf(:,236))
  call vert_WQ_A(wf(:,-2),wf(:,9),wf(:,237))
  call prop_Q_A(wf(:,237),Q(:,29),ZERO,0_intkind1,wf(:,238))
  call vert_VQ_A(wf(:,-5),wf(:,9),wf(:,239))
  call prop_Q_A(wf(:,239),Q(:,57),ZERO,0_intkind1,wf(:,240))
  call vert_WQ_A(wf(:,-2),wf(:,80),wf(:,241))
  call prop_Q_A(wf(:,241),Q(:,45),ZERO,0_intkind1,wf(:,242))
  call vert_VQ_A(wf(:,-4),wf(:,80),wf(:,243))
  call prop_Q_A(wf(:,243),Q(:,57),ZERO,0_intkind1,wf(:,244))
  call vert_QA_V(wf(:,52),wf(:,-1),wf(:,245))
  call vert_QA_V(wf(:,54),wf(:,-1),wf(:,246))
  call vert_QA_V(wf(:,0),wf(:,46),wf(:,247))
  call vert_QA_V(wf(:,0),wf(:,48),wf(:,248))
  call vert_VQ_A(wf(:,-4),wf(:,52),wf(:,249))
  call prop_Q_A(wf(:,249),Q(:,29),ZERO,0_intkind1,wf(:,250))
  call vert_VQ_A(wf(:,-4),wf(:,54),wf(:,251))
  call prop_Q_A(wf(:,251),Q(:,29),ZERO,0_intkind1,wf(:,252))
  call vert_VQ_A(wf(:,-5),wf(:,52),wf(:,253))
  call prop_Q_A(wf(:,253),Q(:,45),ZERO,0_intkind1,wf(:,254))
  call vert_VQ_A(wf(:,-5),wf(:,54),wf(:,255))
  call prop_Q_A(wf(:,255),Q(:,45),ZERO,0_intkind1,wf(:,256))
  call vert_AV_Q(wf(:,46),wf(:,-4),wf(:,257))
  call prop_A_Q(wf(:,257),Q(:,30),ZERO,0_intkind1,wf(:,258))
  call vert_AV_Q(wf(:,48),wf(:,-4),wf(:,259))
  call prop_A_Q(wf(:,259),Q(:,30),ZERO,0_intkind1,wf(:,260))
  call vert_AV_Q(wf(:,46),wf(:,-5),wf(:,261))
  call prop_A_Q(wf(:,261),Q(:,46),ZERO,0_intkind1,wf(:,262))
  call vert_AV_Q(wf(:,48),wf(:,-5),wf(:,263))
  call prop_A_Q(wf(:,263),Q(:,46),ZERO,0_intkind1,wf(:,264))
  call vert_WQ_A(wf(:,-2),wf(:,16),wf(:,265))
  call prop_Q_A(wf(:,265),Q(:,29),ZERO,0_intkind1,wf(:,266))
  call vert_VQ_A(wf(:,-5),wf(:,16),wf(:,267))
  call prop_Q_A(wf(:,267),Q(:,57),ZERO,0_intkind1,wf(:,268))
  call vert_WQ_A(wf(:,-3),wf(:,89),wf(:,269))
  call prop_Q_A(wf(:,269),Q(:,57),ZERO,0_intkind1,wf(:,270))
  call vert_AW_Q(wf(:,33),wf(:,-3),wf(:,271))
  call prop_A_Q(wf(:,271),Q(:,30),ZERO,0_intkind1,wf(:,272))
  call vert_AV_Q(wf(:,33),wf(:,-5),wf(:,273))
  call prop_A_Q(wf(:,273),Q(:,54),ZERO,0_intkind1,wf(:,274))
  call vert_AW_Q(wf(:,103),wf(:,-2),wf(:,275))
  call prop_A_Q(wf(:,275),Q(:,54),ZERO,0_intkind1,wf(:,276))
  call vert_WQ_A(wf(:,-2),wf(:,23),wf(:,277))
  call prop_Q_A(wf(:,277),Q(:,45),ZERO,0_intkind1,wf(:,278))
  call vert_VQ_A(wf(:,-4),wf(:,23),wf(:,279))
  call prop_Q_A(wf(:,279),Q(:,57),ZERO,0_intkind1,wf(:,280))
  call vert_WQ_A(wf(:,-3),wf(:,91),wf(:,281))
  call prop_Q_A(wf(:,281),Q(:,57),ZERO,0_intkind1,wf(:,282))
  call vert_AW_Q(wf(:,38),wf(:,-3),wf(:,283))
  call prop_A_Q(wf(:,283),Q(:,46),ZERO,0_intkind1,wf(:,284))
  call vert_AV_Q(wf(:,38),wf(:,-4),wf(:,285))
  call prop_A_Q(wf(:,285),Q(:,54),ZERO,0_intkind1,wf(:,286))
  call vert_AW_Q(wf(:,105),wf(:,-2),wf(:,287))
  call prop_A_Q(wf(:,287),Q(:,54),ZERO,0_intkind1,wf(:,288))
  call vert_WQ_A(wf(:,-3),wf(:,26),wf(:,289))
  call prop_Q_A(wf(:,289),Q(:,57),ZERO,0_intkind1,wf(:,290))
  call vert_AW_Q(wf(:,40),wf(:,-2),wf(:,291))
  call prop_A_Q(wf(:,291),Q(:,54),ZERO,0_intkind1,wf(:,292))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,9))
  den(2) = 1 / (Q(5,6))
  den(3) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,25))
  den(9) = 1 / (Q(5,22))
  den(12) = 1 / (Q(5,17))
  den(15) = 1 / (Q(5,14))
  den(18) = 1 / (Q(5,33))
  den(19) = 1 / (Q(5,41))
  den(23) = 1 / (Q(5,49))
  den(26) = 1 / (Q(5,18))
  den(27) = 1 / (Q(5,13))
  den(32) = 1 / (Q(5,34))
  den(34) = 1 / (Q(5,38))
  den(37) = 1 / (Q(5,50))
  den(40) = 1 / (Q(5,12))
  den(43) = 1 / (Q(5,12) - MZ2)
  den(66) = 1 / (Q(5,3))
  den(70) = 1 / (Q(5,44))
  den(72) = 1 / (Q(5,28))
  den(78) = 1 / (Q(5,12) - MH2)
  den(82) = 1 / (Q(5,19))
  den(86) = 1 / (Q(5,35))
  den(135) = 1 / (Q(5,54))
  den(139) = 1 / (Q(5,57))
  den(142) = 1 / (Q(5,15))
  den(189) = 1 / (Q(5,46))
  den(196) = 1 / (Q(5,29))
  den(207) = 1 / (Q(5,30))
  den(214) = 1 / (Q(5,45))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(13) = den(6)*den(12)
  den(14) = den(2)*den(13)
  den(16) = den(2)*den(15)
  den(17) = den(12)*den(16)
  den(20) = den(18)*den(19)
  den(21) = den(2)*den(20)
  den(22) = den(16)*den(18)
  den(24) = den(3)*den(23)
  den(25) = den(2)*den(24)
  den(28) = den(1)*den(27)
  den(29) = den(26)*den(28)
  den(30) = den(9)*den(26)
  den(31) = den(1)*den(30)
  den(33) = den(28)*den(32)
  den(35) = den(32)*den(34)
  den(36) = den(1)*den(35)
  den(38) = den(3)*den(37)
  den(39) = den(1)*den(38)
  den(41) = den(12)*den(32)
  den(42) = den(40)*den(41)
  den(44) = den(41)*den(43)
  den(45) = den(15)*den(40)
  den(46) = den(12)*den(45)
  den(47) = den(15)*den(43)
  den(48) = den(12)*den(47)
  den(49) = den(18)*den(26)
  den(50) = den(40)*den(49)
  den(51) = den(43)*den(49)
  den(52) = den(27)*den(40)
  den(53) = den(26)*den(52)
  den(54) = den(27)*den(43)
  den(55) = den(26)*den(54)
  den(56) = den(18)*den(45)
  den(57) = den(18)*den(47)
  den(58) = den(32)*den(52)
  den(59) = den(32)*den(54)
  den(60) = den(3)*den(52)
  den(61) = den(3)*den(54)
  den(62) = den(24)*den(40)
  den(63) = den(24)*den(43)
  den(64) = den(12)*den(35)
  den(65) = den(18)*den(30)
  den(67) = den(40)*den(66)
  den(68) = den(43)*den(66)
  den(69) = den(3)*den(66)
  den(71) = den(66)*den(70)
  den(73) = den(66)*den(72)
  den(74) = den(12)*den(70)
  den(75) = den(26)*den(70)
  den(76) = den(18)*den(72)
  den(77) = den(32)*den(72)
  den(79) = den(66)*den(78)
  den(80) = den(3)*den(79)
  den(81) = den(3)*den(68)
  den(83) = den(66)*den(82)
  den(84) = den(78)*den(83)
  den(85) = den(43)*den(83)
  den(87) = den(66)*den(86)
  den(88) = den(78)*den(87)
  den(89) = den(43)*den(87)
  den(90) = den(1)*den(19)
  den(91) = den(2)*den(90)
  den(92) = den(2)*den(34)
  den(93) = den(1)*den(92)
  den(94) = den(12)*den(23)
  den(95) = den(2)*den(94)
  den(96) = den(12)*den(92)
  den(97) = den(18)*den(23)
  den(98) = den(2)*den(97)
  den(99) = den(10)*den(18)
  den(100) = den(12)*den(82)
  den(101) = den(78)*den(100)
  den(102) = den(43)*den(100)
  den(103) = den(26)*den(82)
  den(104) = den(78)*den(103)
  den(105) = den(43)*den(103)
  den(106) = den(18)*den(86)
  den(107) = den(78)*den(106)
  den(108) = den(43)*den(106)
  den(109) = den(32)*den(86)
  den(110) = den(78)*den(109)
  den(111) = den(43)*den(109)
  den(112) = den(26)*den(90)
  den(113) = den(26)*den(37)
  den(114) = den(1)*den(113)
  den(115) = den(7)*den(32)
  den(116) = den(32)*den(37)
  den(117) = den(1)*den(116)
  den(118) = den(13)*den(32)
  den(119) = den(20)*den(26)
  den(120) = den(3)*den(28)
  den(121) = den(40)*den(94)
  den(122) = den(43)*den(94)
  den(123) = den(40)*den(97)
  den(124) = den(43)*den(97)
  den(125) = den(3)*den(16)
  den(126) = den(40)*den(113)
  den(127) = den(43)*den(113)
  den(128) = den(40)*den(116)
  den(129) = den(43)*den(116)
  den(130) = den(38)*den(40)
  den(131) = den(38)*den(43)
  den(132) = den(3)*den(45)
  den(133) = den(3)*den(47)
  den(134) = den(2)*den(3)
  den(136) = den(134)*den(135)
  den(137) = den(1)*den(136)
  den(138) = den(1)*den(3)
  den(140) = den(138)*den(139)
  den(141) = den(2)*den(140)
  den(143) = den(4)*den(142)
  den(144) = den(3)*den(143)
  den(145) = den(1)**2
  den(146) = den(92)*den(145)
  den(147) = den(10)*den(145)
  den(148) = den(2)**2
  den(149) = den(90)*den(148)
  den(150) = den(7)*den(148)
  den(151) = den(7)*den(92)
  den(152) = den(10)*den(90)
  den(153) = den(12)**2
  den(154) = den(92)*den(153)
  den(155) = den(16)*den(153)
  den(156) = den(94)*den(148)
  den(157) = den(13)*den(148)
  den(158) = den(13)*den(92)
  den(159) = den(16)*den(94)
  den(160) = den(18)**2
  den(161) = den(10)*den(160)
  den(162) = den(16)*den(160)
  den(163) = den(97)*den(148)
  den(164) = den(20)*den(148)
  den(165) = den(10)*den(20)
  den(166) = den(16)*den(97)
  den(167) = den(16)*den(24)
  den(168) = den(3)**2
  den(169) = den(16)*den(168)
  den(170) = den(24)*den(148)
  den(171) = den(113)*den(145)
  den(172) = den(30)*den(145)
  den(173) = den(26)**2
  den(174) = den(90)*den(173)
  den(175) = den(28)*den(173)
  den(176) = den(28)*den(113)
  den(177) = den(30)*den(90)
  den(178) = den(116)*den(145)
  den(179) = den(35)*den(145)
  den(180) = den(32)**2
  den(181) = den(7)*den(180)
  den(182) = den(28)*den(180)
  den(183) = den(28)*den(116)
  den(184) = den(7)*den(35)
  den(185) = den(38)*den(145)
  den(186) = den(28)*den(38)
  den(187) = den(28)*den(168)
  den(188) = den(32)*den(40)
  den(190) = den(188)*den(189)
  den(191) = den(12)*den(190)
  den(192) = den(32)*den(43)
  den(193) = den(189)*den(192)
  den(194) = den(12)*den(193)
  den(195) = den(12)*den(40)
  den(197) = den(195)*den(196)
  den(198) = den(32)*den(197)
  den(199) = den(12)*den(43)
  den(200) = den(196)*den(199)
  den(201) = den(32)*den(200)
  den(202) = den(45)*den(153)
  den(203) = den(47)*den(153)
  den(204) = den(45)*den(94)
  den(205) = den(47)*den(94)
  den(206) = den(26)*den(40)
  den(208) = den(206)*den(207)
  den(209) = den(18)*den(208)
  den(210) = den(26)*den(43)
  den(211) = den(207)*den(210)
  den(212) = den(18)*den(211)
  den(213) = den(18)*den(40)
  den(215) = den(213)*den(214)
  den(216) = den(26)*den(215)
  den(217) = den(18)*den(43)
  den(218) = den(214)*den(217)
  den(219) = den(26)*den(218)
  den(220) = den(52)*den(113)
  den(221) = den(54)*den(113)
  den(222) = den(52)*den(173)
  den(223) = den(54)*den(173)
  den(224) = den(45)*den(160)
  den(225) = den(47)*den(160)
  den(226) = den(45)*den(97)
  den(227) = den(47)*den(97)
  den(228) = den(52)*den(116)
  den(229) = den(54)*den(116)
  den(230) = den(52)*den(180)
  den(231) = den(54)*den(180)
  den(232) = den(38)*den(52)
  den(233) = den(38)*den(54)
  den(234) = den(24)*den(45)
  den(235) = den(24)*den(47)
  den(236) = den(45)*den(168)
  den(237) = den(47)*den(168)
  den(238) = den(52)*den(168)
  den(239) = den(54)*den(168)
  den(240) = den(35)*den(153)
  den(241) = den(13)*den(180)
  den(242) = den(13)*den(35)
  den(243) = den(30)*den(160)
  den(244) = den(20)*den(173)
  den(245) = den(20)*den(30)
  den(246) = den(16)*den(142)
  den(247) = den(16)*den(207)
  den(248) = den(16)*den(189)
  den(249) = den(10)*den(207)
  den(250) = den(10)*den(135)
  den(251) = den(92)*den(189)
  den(252) = den(92)*den(135)
  den(253) = den(28)*den(142)
  den(254) = den(28)*den(196)
  den(255) = den(28)*den(214)
  den(256) = den(7)*den(196)
  den(257) = den(7)*den(139)
  den(258) = den(90)*den(214)
  den(259) = den(90)*den(139)
  den(260) = den(52)*den(142)
  den(261) = den(54)*den(142)
  den(262) = den(45)*den(142)
  den(263) = den(47)*den(142)
  den(264) = den(52)*den(196)
  den(265) = den(54)*den(196)
  den(266) = den(52)*den(214)
  den(267) = den(54)*den(214)
  den(268) = den(45)*den(207)
  den(269) = den(47)*den(207)
  den(270) = den(45)*den(189)
  den(271) = den(47)*den(189)
  den(272) = den(13)*den(196)
  den(273) = den(13)*den(139)
  den(274) = den(94)*den(139)
  den(275) = den(30)*den(207)
  den(276) = den(30)*den(135)
  den(277) = den(113)*den(135)
  den(278) = den(20)*den(214)
  den(279) = den(20)*den(139)
  den(280) = den(97)*den(139)
  den(281) = den(35)*den(189)
  den(282) = den(35)*den(135)
  den(283) = den(116)*den(135)
  den(284) = den(24)*den(139)
  den(285) = den(38)*den(135)
  den(286) = den(3)*den(66)*den(78)
  den(287) = den(3)*den(40)*den(66)
  den(288) = den(3)*den(43)*den(66)
  den(289) = den(40)*den(83)
  den(290) = den(40)*den(87)
  den(291) = den(1)*den(2)*den(3)
  den(292) = den(2)*den(12)
  den(293) = den(2)*den(18)
  den(294) = den(1)*den(26)
  den(295) = den(1)*den(32)
  den(296) = den(12)*den(32)*den(40)
  den(297) = den(12)*den(32)*den(43)
  den(298) = den(40)*den(100)
  den(299) = den(18)*den(26)*den(40)
  den(300) = den(18)*den(26)*den(43)
  den(301) = den(40)*den(103)
  den(302) = den(40)*den(106)
  den(303) = den(40)*den(109)
  den(304) = den(3)*den(40)
  den(305) = den(3)*den(43)
  den(306) = den(2)*den(257)
  den(307) = den(2)*den(259)
  den(308) = den(1)*den(250)
  den(309) = den(1)*den(252)
  den(310) = den(2)*den(273)
  den(311) = den(2)*den(274)
  den(312) = den(12)*den(248)
  den(313) = den(12)*den(251)
  den(314) = den(2)*den(279)
  den(315) = den(2)*den(280)
  den(316) = den(18)*den(247)
  den(317) = den(18)*den(249)
  den(318) = den(2)*den(284)
  den(319) = den(3)*den(246)
  den(320) = den(26)*den(255)
  den(321) = den(26)*den(258)
  den(322) = den(1)*den(276)
  den(323) = den(1)*den(277)
  den(324) = den(32)*den(254)
  den(325) = den(32)*den(256)
  den(326) = den(1)*den(282)
  den(327) = den(1)*den(283)
  den(328) = den(3)*den(253)
  den(329) = den(1)*den(285)
  den(330) = den(12)*den(270)
  den(331) = den(12)*den(271)
  den(332) = den(26)*den(266)
  den(333) = den(26)*den(267)
  den(334) = den(18)*den(268)
  den(335) = den(18)*den(269)
  den(336) = den(32)*den(264)
  den(337) = den(32)*den(265)
  den(338) = den(3)*den(260)
  den(339) = den(3)*den(261)
  den(340) = den(3)*den(262)
  den(341) = den(3)*den(263)
  den(342) = den(32)*den(272)
  den(343) = den(12)*den(281)
  den(344) = den(26)*den(278)
  den(345) = den(18)*den(275)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(241)

  A(1) = cont_VV(wf(:,3),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,11),wf(:,12)) * den(11)
  A(4) = cont_QA(wf(:,8),wf(:,16)) * den(14)
  A(5) = cont_QA(wf(:,18),wf(:,19)) * den(17)
  A(6) = cont_QA(wf(:,10),wf(:,23)) * den(21)
  A(7) = cont_QA(wf(:,19),wf(:,24)) * den(22)
  A(8) = cont_QA(wf(:,17),wf(:,26)) * den(25)
  A(9) = cont_QA(wf(:,30),wf(:,31)) * den(29)
  A(10) = cont_QA(wf(:,11),wf(:,33)) * den(31)
  A(11) = cont_QA(wf(:,31),wf(:,36)) * den(33)
  A(12) = cont_QA(wf(:,7),wf(:,38)) * den(36)
  A(13) = cont_QA(wf(:,29),wf(:,40)) * den(39)
  A(14) = cont_VV(wf(:,41),wf(:,42)) * den(42)
  A(15) = cont_VV(wf(:,43),wf(:,44)) * den(44)
  A(16) = cont_QA(wf(:,18),wf(:,46)) * den(46)
  A(17) = cont_QA(wf(:,18),wf(:,48)) * den(48)
  A(18) = cont_VV(wf(:,41),wf(:,49)) * den(50)
  A(19) = cont_VV(wf(:,43),wf(:,50)) * den(51)
  A(20) = cont_QA(wf(:,30),wf(:,52)) * den(53)
  A(21) = cont_QA(wf(:,30),wf(:,54)) * den(55)
  A(22) = cont_QA(wf(:,24),wf(:,46)) * den(56)
  A(23) = cont_QA(wf(:,24),wf(:,48)) * den(57)
  A(24) = cont_QA(wf(:,36),wf(:,52)) * den(58)
  A(25) = cont_QA(wf(:,36),wf(:,54)) * den(59)
  A(26) = cont_QA(wf(:,39),wf(:,52)) * den(60)
  A(27) = cont_QA(wf(:,39),wf(:,54)) * den(61)
  A(28) = cont_QA(wf(:,26),wf(:,45)) * den(62)
  A(29) = cont_QA(wf(:,26),wf(:,47)) * den(63)
  A(30) = cont_QA(wf(:,15),wf(:,38)) * den(64)
  A(31) = cont_QA(wf(:,22),wf(:,33)) * den(65)

  A(32) = cont_VV(wf(:,41),wf(:,56)) * den(67)
  A(33) = cont_VV(wf(:,41),wf(:,57)) * den(67)
  A(34) = cont_VV(wf(:,43),wf(:,58)) * den(68)
  A(35) = cont_VV(wf(:,43),wf(:,59)) * den(68)
  A(36) = cont_VV(wf(:,3),wf(:,60)) * den(69)
  A(37) = cont_VV(wf(:,61),wf(:,62)) * den(71)
  A(38) = cont_VV(wf(:,63),wf(:,64)) * den(73)
  A(39) = cont_VV(wf(:,61),wf(:,65)) * den(74)
  A(40) = cont_VV(wf(:,61),wf(:,66)) * den(75)
  A(41) = cont_VV(wf(:,63),wf(:,67)) * den(76)
  A(42) = cont_VV(wf(:,63),wf(:,68)) * den(77)
  A(43) = cont_VV(wf(:,3),wf(:,70)) * den(80)
  A(44) = cont_VV(wf(:,3),wf(:,71)) * den(81)
  A(45) = cont_VV(wf(:,62),wf(:,72)) * den(84)
  A(46) = cont_VV(wf(:,62),wf(:,73)) * den(85)
  A(47) = cont_VV(wf(:,64),wf(:,74)) * den(88)
  A(48) = cont_VV(wf(:,64),wf(:,75)) * den(89)
  A(49) = cont_VV(wf(:,3),wf(:,76)) * den(5)
  A(50) = cont_QA(wf(:,9),wf(:,77)) * den(8)
  A(51) = cont_QA(wf(:,12),wf(:,78)) * den(11)
  A(52) = cont_QA(wf(:,79),wf(:,80)) * den(91)
  A(53) = cont_QA(wf(:,81),wf(:,82)) * den(93)
  A(54) = cont_VV(wf(:,6),wf(:,83)) * den(5)
  A(55) = cont_QA(wf(:,16),wf(:,77)) * den(14)
  A(56) = cont_QA(wf(:,19),wf(:,84)) * den(17)
  A(57) = cont_QA(wf(:,23),wf(:,79)) * den(21)
  A(58) = cont_QA(wf(:,19),wf(:,85)) * den(22)
  A(59) = cont_QA(wf(:,17),wf(:,87)) * den(25)
  A(60) = cont_QA(wf(:,88),wf(:,89)) * den(95)
  A(61) = cont_QA(wf(:,82),wf(:,90)) * den(96)
  A(62) = cont_QA(wf(:,88),wf(:,91)) * den(98)
  A(63) = cont_QA(wf(:,12),wf(:,92)) * den(99)
  A(64) = cont_QA(wf(:,26),wf(:,88)) * den(25)
  A(65) = cont_QA(wf(:,31),wf(:,93)) * den(29)
  A(66) = cont_QA(wf(:,33),wf(:,78)) * den(31)
  A(67) = cont_QA(wf(:,31),wf(:,94)) * den(33)
  A(68) = cont_QA(wf(:,38),wf(:,81)) * den(36)
  A(69) = cont_QA(wf(:,29),wf(:,96)) * den(39)
  A(70) = cont_VV(wf(:,41),wf(:,97)) * den(42)
  A(71) = cont_VV(wf(:,43),wf(:,98)) * den(44)
  A(72) = cont_VV(wf(:,65),wf(:,72)) * den(101)
  A(73) = cont_VV(wf(:,65),wf(:,73)) * den(102)
  A(74) = cont_QA(wf(:,46),wf(:,84)) * den(46)
  A(75) = cont_QA(wf(:,48),wf(:,84)) * den(48)
  A(76) = cont_VV(wf(:,41),wf(:,99)) * den(50)
  A(77) = cont_VV(wf(:,43),wf(:,100)) * den(51)
  A(78) = cont_VV(wf(:,66),wf(:,72)) * den(104)
  A(79) = cont_VV(wf(:,66),wf(:,73)) * den(105)
  A(80) = cont_QA(wf(:,52),wf(:,93)) * den(53)
  A(81) = cont_QA(wf(:,54),wf(:,93)) * den(55)
  A(82) = cont_VV(wf(:,67),wf(:,74)) * den(107)
  A(83) = cont_VV(wf(:,67),wf(:,75)) * den(108)
  A(84) = cont_QA(wf(:,46),wf(:,85)) * den(56)
  A(85) = cont_QA(wf(:,48),wf(:,85)) * den(57)
  A(86) = cont_VV(wf(:,68),wf(:,74)) * den(110)
  A(87) = cont_VV(wf(:,68),wf(:,75)) * den(111)
  A(88) = cont_QA(wf(:,52),wf(:,94)) * den(58)
  A(89) = cont_QA(wf(:,54),wf(:,94)) * den(59)
  A(90) = cont_QA(wf(:,52),wf(:,95)) * den(60)
  A(91) = cont_QA(wf(:,54),wf(:,95)) * den(61)
  A(92) = cont_QA(wf(:,45),wf(:,87)) * den(62)
  A(93) = cont_QA(wf(:,47),wf(:,87)) * den(63)
  A(94) = cont_QA(wf(:,38),wf(:,90)) * den(64)
  A(95) = cont_QA(wf(:,33),wf(:,92)) * den(65)
  A(96) = cont_QA(wf(:,80),wf(:,101)) * den(112)
  A(97) = cont_QA(wf(:,102),wf(:,103)) * den(114)
  A(98) = cont_QA(wf(:,9),wf(:,104)) * den(115)
  A(99) = cont_QA(wf(:,102),wf(:,105)) * den(117)
  A(100) = cont_QA(wf(:,40),wf(:,102)) * den(39)
  A(101) = cont_QA(wf(:,16),wf(:,104)) * den(118)
  A(102) = cont_QA(wf(:,23),wf(:,101)) * den(119)
  A(103) = cont_QA(wf(:,31),wf(:,106)) * den(120)
  A(104) = cont_QA(wf(:,31),wf(:,109)) * den(33)
  A(105) = cont_QA(wf(:,7),wf(:,111)) * den(36)
  A(106) = cont_QA(wf(:,31),wf(:,114)) * den(29)
  A(107) = cont_QA(wf(:,11),wf(:,116)) * den(31)
  A(108) = cont_QA(wf(:,89),wf(:,117)) * den(121)
  A(109) = cont_QA(wf(:,89),wf(:,118)) * den(122)
  A(110) = cont_VV(wf(:,41),wf(:,119)) * den(42)
  A(111) = cont_VV(wf(:,43),wf(:,120)) * den(44)
  A(112) = cont_QA(wf(:,91),wf(:,117)) * den(123)
  A(113) = cont_QA(wf(:,91),wf(:,118)) * den(124)
  A(114) = cont_QA(wf(:,52),wf(:,106)) * den(60)
  A(115) = cont_QA(wf(:,54),wf(:,106)) * den(61)
  A(116) = cont_QA(wf(:,26),wf(:,117)) * den(62)
  A(117) = cont_QA(wf(:,26),wf(:,118)) * den(63)
  A(118) = cont_QA(wf(:,52),wf(:,109)) * den(58)
  A(119) = cont_QA(wf(:,54),wf(:,109)) * den(59)
  A(120) = cont_VV(wf(:,41),wf(:,121)) * den(50)
  A(121) = cont_VV(wf(:,43),wf(:,122)) * den(51)
  A(122) = cont_QA(wf(:,52),wf(:,114)) * den(53)
  A(123) = cont_QA(wf(:,54),wf(:,114)) * den(55)
  A(124) = cont_QA(wf(:,15),wf(:,111)) * den(64)
  A(125) = cont_QA(wf(:,22),wf(:,116)) * den(65)
  A(126) = cont_VV(wf(:,3),wf(:,125)) * den(5)
  A(127) = cont_QA(wf(:,9),wf(:,126)) * den(8)
  A(128) = cont_QA(wf(:,11),wf(:,128)) * den(11)
  A(129) = cont_QA(wf(:,16),wf(:,126)) * den(14)
  A(130) = cont_QA(wf(:,18),wf(:,130)) * den(17)
  A(131) = cont_QA(wf(:,23),wf(:,127)) * den(21)
  A(132) = cont_QA(wf(:,24),wf(:,130)) * den(22)
  A(133) = cont_QA(wf(:,26),wf(:,129)) * den(25)
  A(134) = cont_QA(wf(:,19),wf(:,131)) * den(125)
  A(135) = cont_QA(wf(:,10),wf(:,135)) * den(21)
  A(136) = cont_QA(wf(:,19),wf(:,136)) * den(22)
  A(137) = cont_QA(wf(:,8),wf(:,140)) * den(14)
  A(138) = cont_QA(wf(:,19),wf(:,141)) * den(17)
  A(139) = cont_VV(wf(:,3),wf(:,144)) * den(5)
  A(140) = cont_QA(wf(:,8),wf(:,146)) * den(8)
  A(141) = cont_QA(wf(:,12),wf(:,147)) * den(11)
  A(142) = cont_QA(wf(:,103),wf(:,148)) * den(126)
  A(143) = cont_QA(wf(:,103),wf(:,149)) * den(127)
  A(144) = cont_VV(wf(:,41),wf(:,150)) * den(50)
  A(145) = cont_VV(wf(:,43),wf(:,151)) * den(51)
  A(146) = cont_QA(wf(:,105),wf(:,148)) * den(128)
  A(147) = cont_QA(wf(:,105),wf(:,149)) * den(129)
  A(148) = cont_QA(wf(:,40),wf(:,148)) * den(130)
  A(149) = cont_QA(wf(:,40),wf(:,149)) * den(131)
  A(150) = cont_QA(wf(:,46),wf(:,131)) * den(132)
  A(151) = cont_QA(wf(:,48),wf(:,131)) * den(133)
  A(152) = cont_QA(wf(:,46),wf(:,136)) * den(56)
  A(153) = cont_QA(wf(:,48),wf(:,136)) * den(57)
  A(154) = cont_VV(wf(:,41),wf(:,152)) * den(42)
  A(155) = cont_VV(wf(:,43),wf(:,153)) * den(44)
  A(156) = cont_QA(wf(:,46),wf(:,141)) * den(46)
  A(157) = cont_QA(wf(:,48),wf(:,141)) * den(48)
  A(158) = cont_QA(wf(:,33),wf(:,134)) * den(65)
  A(159) = cont_QA(wf(:,38),wf(:,139)) * den(64)
  A(160) = cont_QA(wf(:,30),wf(:,155)) * den(29)
  A(161) = cont_QA(wf(:,33),wf(:,147)) * den(31)
  A(162) = cont_QA(wf(:,36),wf(:,155)) * den(33)
  A(163) = cont_QA(wf(:,38),wf(:,145)) * den(36)
  A(164) = cont_QA(wf(:,40),wf(:,154)) * den(39)
  A(165) = cont_QA(wf(:,157),wf(:,158)) * den(137)
  A(166) = cont_QA(wf(:,160),wf(:,161)) * den(141)
  A(167) = cont_VV(wf(:,6),wf(:,162)) * den(144)
  A(168) = cont_QA(wf(:,82),wf(:,164)) * den(146)
  A(169) = cont_QA(wf(:,12),wf(:,165)) * den(147)
  A(170) = cont_QA(wf(:,80),wf(:,167)) * den(149)
  A(171) = cont_QA(wf(:,9),wf(:,168)) * den(150)
  A(172) = cont_QA(wf(:,82),wf(:,169)) * den(151)
  A(173) = cont_QA(wf(:,80),wf(:,170)) * den(152)
  A(174) = cont_QA(wf(:,82),wf(:,173)) * den(154)
  A(175) = cont_QA(wf(:,19),wf(:,174)) * den(155)
  A(176) = cont_QA(wf(:,89),wf(:,175)) * den(156)
  A(177) = cont_QA(wf(:,16),wf(:,168)) * den(157)
  A(178) = cont_QA(wf(:,82),wf(:,176)) * den(158)
  A(179) = cont_QA(wf(:,89),wf(:,177)) * den(159)
  A(180) = cont_QA(wf(:,12),wf(:,180)) * den(161)
  A(181) = cont_QA(wf(:,19),wf(:,181)) * den(162)
  A(182) = cont_QA(wf(:,91),wf(:,175)) * den(163)
  A(183) = cont_QA(wf(:,23),wf(:,167)) * den(164)
  A(184) = cont_QA(wf(:,12),wf(:,182)) * den(165)
  A(185) = cont_QA(wf(:,91),wf(:,177)) * den(166)
  A(186) = cont_QA(wf(:,19),wf(:,183)) * den(167)
  A(187) = cont_QA(wf(:,19),wf(:,184)) * den(169)
  A(188) = cont_QA(wf(:,26),wf(:,175)) * den(170)
  A(189) = cont_QA(wf(:,103),wf(:,185)) * den(171)
  A(190) = cont_QA(wf(:,33),wf(:,165)) * den(172)
  A(191) = cont_QA(wf(:,80),wf(:,188)) * den(174)
  A(192) = cont_QA(wf(:,31),wf(:,189)) * den(175)
  A(193) = cont_QA(wf(:,103),wf(:,190)) * den(176)
  A(194) = cont_QA(wf(:,80),wf(:,191)) * den(177)
  A(195) = cont_QA(wf(:,105),wf(:,185)) * den(178)
  A(196) = cont_QA(wf(:,38),wf(:,164)) * den(179)
  A(197) = cont_QA(wf(:,9),wf(:,194)) * den(181)
  A(198) = cont_QA(wf(:,31),wf(:,195)) * den(182)
  A(199) = cont_QA(wf(:,105),wf(:,190)) * den(183)
  A(200) = cont_QA(wf(:,9),wf(:,196)) * den(184)
  A(201) = cont_QA(wf(:,40),wf(:,185)) * den(185)
  A(202) = cont_QA(wf(:,31),wf(:,197)) * den(186)
  A(203) = cont_QA(wf(:,31),wf(:,198)) * den(187)
  A(204) = cont_QA(wf(:,171),wf(:,200)) * den(191)
  A(205) = cont_QA(wf(:,171),wf(:,202)) * den(194)
  A(206) = cont_QA(wf(:,192),wf(:,204)) * den(198)
  A(207) = cont_QA(wf(:,192),wf(:,206)) * den(201)
  A(208) = cont_QA(wf(:,46),wf(:,174)) * den(202)
  A(209) = cont_QA(wf(:,48),wf(:,174)) * den(203)
  A(210) = cont_QA(wf(:,89),wf(:,207)) * den(204)
  A(211) = cont_QA(wf(:,89),wf(:,208)) * den(205)
  A(212) = cont_QA(wf(:,178),wf(:,210)) * den(209)
  A(213) = cont_QA(wf(:,178),wf(:,212)) * den(212)
  A(214) = cont_QA(wf(:,186),wf(:,214)) * den(216)
  A(215) = cont_QA(wf(:,186),wf(:,216)) * den(219)
  A(216) = cont_QA(wf(:,103),wf(:,217)) * den(220)
  A(217) = cont_QA(wf(:,103),wf(:,218)) * den(221)
  A(218) = cont_QA(wf(:,52),wf(:,189)) * den(222)
  A(219) = cont_QA(wf(:,54),wf(:,189)) * den(223)
  A(220) = cont_QA(wf(:,46),wf(:,181)) * den(224)
  A(221) = cont_QA(wf(:,48),wf(:,181)) * den(225)
  A(222) = cont_QA(wf(:,91),wf(:,207)) * den(226)
  A(223) = cont_QA(wf(:,91),wf(:,208)) * den(227)
  A(224) = cont_QA(wf(:,105),wf(:,217)) * den(228)
  A(225) = cont_QA(wf(:,105),wf(:,218)) * den(229)
  A(226) = cont_QA(wf(:,52),wf(:,195)) * den(230)
  A(227) = cont_QA(wf(:,54),wf(:,195)) * den(231)
  A(228) = cont_QA(wf(:,40),wf(:,217)) * den(232)
  A(229) = cont_QA(wf(:,40),wf(:,218)) * den(233)
  A(230) = cont_QA(wf(:,46),wf(:,183)) * den(234)
  A(231) = cont_QA(wf(:,48),wf(:,183)) * den(235)
  A(232) = cont_QA(wf(:,46),wf(:,184)) * den(236)
  A(233) = cont_QA(wf(:,48),wf(:,184)) * den(237)
  A(234) = cont_QA(wf(:,52),wf(:,198)) * den(238)
  A(235) = cont_QA(wf(:,54),wf(:,198)) * den(239)
  A(236) = cont_QA(wf(:,38),wf(:,173)) * den(240)
  A(237) = cont_QA(wf(:,16),wf(:,194)) * den(241)
  A(238) = cont_QA(wf(:,16),wf(:,196)) * den(242)
  A(239) = cont_QA(wf(:,33),wf(:,180)) * den(243)
  A(240) = cont_QA(wf(:,23),wf(:,188)) * den(244)
  A(241) = cont_QA(wf(:,23),wf(:,191)) * den(245)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(241)
  complex(REALKIND), intent(out) :: M1(2), M2(3)

  M1(1) = (-A(18)-A(20)-A(22))*f(1)+CI*(-A(26)-A(28))*f(2)+(A(3)+A(6)+A(7)+A(9)+A(10)+A(31))*f(13)+CI*(A(1)+A(8)+A(13))*f(14) &
       +(A(19)+A(21)+A(23))*f(22)+CI*(A(27)+A(29))*f(23)
  M1(2) = (-A(14)-A(16)-A(24))*f(1)+CI*(A(26)+A(28))*f(2)+(A(2)+A(4)+A(5)+A(11)+A(12)+A(30))*f(13)+CI*(-A(1)-A(8)-A(13))*f(14) &
       +(A(15)+A(17)+A(25))*f(22)+CI*(-A(27)-A(29))*f(23)

  M2(1) = (A(212)+A(214)+A(216)+A(218)+A(220)+A(222))*f(3)-(A(32)*f(4))/2._/**/REALKIND+CI*(A(228)+A(230)+A(232)+A(234))*f(5)+( &
       -A(80)-A(84)-A(120)-A(122)-A(144)-A(152))*f(6)+CI*(-A(114)-A(150))*f(7)+(-A(76)-A(112)-A(142))*f(8)+CI*(-A(116) &
       -A(148))*f(9)+CI*(-A(90)-A(92))*f(10)+(A(40)+A(41))*f(11)+CI*(A(36)+A(37)-A(38))*f(12)+(-A(169)-A(170)-A(173)-A(180)-A(181) &
       -A(182)-A(183)-A(184)-A(185)-A(189)-A(190)-A(191)-A(192)-A(193)-A(194)-A(239)-A(240)-A(241))*f(15)+CI*(-A(165)-A(166) &
       -A(167)-A(186)-A(187)-A(188)-A(201)-A(202)-A(203))*f(16)+(A(51)+A(52)+A(57)+A(58)+A(65)+A(66)+A(106)+A(107)+A(125)+A(135) &
       +A(136)+A(158))*f(17)+CI*(A(49)+A(103)+A(134))*f(18)+(A(62)+A(63)+A(95)+A(96)+A(97)+A(102)+A(128)+A(131)+A(132)+A(141) &
       +A(160)+A(161))*f(19)+CI*(A(64)+A(100)+A(126)+A(133)+A(139)+A(164))*f(20)+CI*(A(54)+A(59)+A(69))*f(21) &
       +(A(34)*f(24))/2._/**/REALKIND+(-A(213)-A(215)-A(217)-A(219)-A(221)-A(223))*f(24)+CI*(-A(229)-A(231)-A(233)-A(235))*f(25) &
       +(A(81)+A(85)+A(121)+A(123)+A(145)+A(153))*f(26)+CI*(A(115)+A(151))*f(27)+(A(77)+A(113)+A(143))*f(28)+CI*(A(117) &
       +A(149))*f(29)+CI*(A(91)+A(93))*f(30)+CI*(A(44)+A(46)-A(48))*f(31)+(-A(79)-A(83))*f(32)+(-A(78)-A(82))*f(33)+CI*(-A(43) &
       -A(45)+A(47))*f(34)
  M2(2) = (A(204)+A(206)+A(208)+A(210)+A(224)+A(226))*f(3)-(A(33)*f(4))/2._/**/REALKIND+CI*(-A(228)-A(230)-A(232)-A(234))*f(5)+( &
       -A(74)-A(88)-A(110)-A(118)-A(154)-A(156))*f(6)+CI*(A(114)+A(150))*f(7)+(-A(70)-A(108)-A(146))*f(8)+CI*(A(116)+A(148))*f(9) &
       +CI*(A(90)+A(92))*f(10)+(A(39)+A(42))*f(11)+CI*(-A(36)-A(37)+A(38))*f(12)+(-A(168)-A(171)-A(172)-A(174)-A(175)-A(176) &
       -A(177)-A(178)-A(179)-A(195)-A(196)-A(197)-A(198)-A(199)-A(200)-A(236)-A(237)-A(238))*f(15)+CI*(A(165)+A(166)+A(167)+A(186) &
       +A(187)+A(188)+A(201)+A(202)+A(203))*f(16)+(A(50)+A(53)+A(55)+A(56)+A(67)+A(68)+A(104)+A(105)+A(124)+A(137)+A(138) &
       +A(159))*f(17)+CI*(-A(49)-A(103)-A(134))*f(18)+(A(60)+A(61)+A(94)+A(98)+A(99)+A(101)+A(127)+A(129)+A(130)+A(140)+A(162) &
       +A(163))*f(19)+CI*(-A(64)-A(100)-A(126)-A(133)-A(139)-A(164))*f(20)+CI*(-A(54)-A(59)-A(69))*f(21) &
       +(A(35)*f(24))/2._/**/REALKIND+(-A(205)-A(207)-A(209)-A(211)-A(225)-A(227))*f(24)+CI*(A(229)+A(231)+A(233)+A(235))*f(25) &
       +(A(75)+A(89)+A(111)+A(119)+A(155)+A(157))*f(26)+CI*(-A(115)-A(151))*f(27)+(A(71)+A(109)+A(147))*f(28)+CI*(-A(117) &
       -A(149))*f(29)+CI*(-A(91)-A(93))*f(30)+CI*(-A(44)-A(46)+A(48))*f(31)+(-A(73)-A(87))*f(32)+(-A(72)-A(86))*f(33)+CI*(A(43) &
       +A(45)-A(47))*f(34)
  M2(3) = ((A(32)+A(33))*f(4))/6._/**/REALKIND+((-A(34)-A(35))*f(24))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppwwjj_ddxwwxgg_1_/**/REALKIND
