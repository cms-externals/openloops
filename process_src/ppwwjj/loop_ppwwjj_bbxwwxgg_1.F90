
module ol_colourmatrix_ppwwjj_bbxwwxgg_1_/**/REALKIND
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
end module ol_colourmatrix_ppwwjj_bbxwwxgg_1_/**/REALKIND



module ol_forced_parameters_ppwwjj_bbxwwxgg_1_/**/REALKIND
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
end module ol_forced_parameters_ppwwjj_bbxwwxgg_1_/**/REALKIND

module ol_loop_ppwwjj_bbxwwxgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(77), c(74)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:326)
  ! denominators
  complex(REALKIND), save :: den(400)
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
    f( 6) = (CI*countertermnorm*ctGbb*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 7) = (countertermnorm*ctGbb*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 8) = (CI*countertermnorm*ctVbb*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 9) = (countertermnorm*ctVbb*eQED**2*gQCD**4)/3._/**/REALKIND
    f(10) = (countertermnorm*ctVVV*eQED**2*gQCD**4)/3._/**/REALKIND
    f(11) = CI*countertermnorm*ctWWGG*eQED**2*gQCD**4
    f(12) = countertermnorm*ctWWGG*eQED**2*gQCD**4
    f(13) = (CI*eQED**2*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(14) = (eQED**2*gQCD**2)/(sw**2*2._/**/REALKIND)
    f(15) = (CI*countertermnorm*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(16) = (countertermnorm*eQED**2*gQCD**4)/(sw**2*2._/**/REALKIND)
    f(17) = (CI*countertermnorm*ctGbb*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(18) = (countertermnorm*ctGbb*eQED**2*gQCD**4)/(sw**2*2._/**/REALKIND)
    f(19) = (CI*countertermnorm*ctGtt*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(20) = (countertermnorm*ctGtt*eQED**2*gQCD**4)/(sw**2*2._/**/REALKIND)
    f(21) = (CI*countertermnorm*ctVbt*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(22) = (countertermnorm*ctVbt*eQED**2*gQCD**4)/(sw**2*2._/**/REALKIND)
    f(23) = (countertermnorm*ctVVV*eQED**2*gQCD**4)/(sw**2*2._/**/REALKIND)
    f(24) = (CI*eQED**2*gQCD**2*MB)/(2._/**/REALKIND*sw**2)
    f(25) = (eQED**2*gQCD**2*MB)/(sw**2*2._/**/REALKIND)
    f(26) = (CI*countertermnorm*eQED**2*gQCD**4*MB)/(2._/**/REALKIND*sw**2)
    f(27) = (countertermnorm*eQED**2*gQCD**4*MB)/(sw**2*2._/**/REALKIND)
    f(28) = (CI*countertermnorm*ctGbb*eQED**2*gQCD**4*MB)/(2._/**/REALKIND*sw**2)
    f(29) = (countertermnorm*ctGbb*eQED**2*gQCD**4*MB)/(sw**2*2._/**/REALKIND)
    f(30) = (CI*countertermnorm*ctSbb*eQED**2*gQCD**4*MB)/(2._/**/REALKIND*sw**2)
    f(31) = (countertermnorm*ctSbb*eQED**2*gQCD**4*MB)/(sw**2*2._/**/REALKIND)
    f(32) = (countertermnorm*ctVVV*eQED**2*gQCD**4*MB)/(sw**2*2._/**/REALKIND)
    f(33) = (CI*cw*eQED**2*gQCD**2)/sw
    f(34) = (cw*eQED**2*gQCD**2)/sw
    f(35) = (CI*countertermnorm*cw*eQED**2*gQCD**4)/sw
    f(36) = (countertermnorm*cw*eQED**2*gQCD**4)/sw
    f(37) = (CI*countertermnorm*ctGbb*cw*eQED**2*gQCD**4)/sw
    f(38) = (countertermnorm*ctGbb*cw*eQED**2*gQCD**4)/sw
    f(39) = (CI*countertermnorm*ctVbb*cw*eQED**2*gQCD**4)/sw
    f(40) = (countertermnorm*ctVbb*cw*eQED**2*gQCD**4)/sw
    f(41) = (countertermnorm*ctVVV*cw*eQED**2*gQCD**4)/sw
    f(42) = (CI*countertermnorm*ctZGG*cw*eQED**2*gQCD**4)/sw
    f(43) = (countertermnorm*ctZGG*cw*eQED**2*gQCD**4)/sw
    f(44) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MW)/sw
    f(45) = (countertermnorm*ctHGG*eQED**2*gQCD**4*MW)/sw
    f(46) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/6._/**/REALKIND
    f(47) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(48) = (eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(49) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(4._/**/REALKIND*sw**2)
    f(50) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(51) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(52) = (CI*eQED**2*gQCD**4*integralnorm*MB*SwB)/(4._/**/REALKIND*sw**2)
    f(53) = (CI*eQED**2*gQCD**4*integralnorm*MB*SwB)/(2._/**/REALKIND*sw**2)
    f(54) = (eQED**2*gQCD**4*integralnorm*MB*SwB)/(sw**2*2._/**/REALKIND)
    f(55) = (CI*cw*eQED**2*gQCD**4*integralnorm*SwB)/(2._/**/REALKIND*sw)
    f(56) = (CI*cw*eQED**2*gQCD**4*integralnorm*SwB)/sw
    f(57) = (cw*eQED**2*gQCD**4*integralnorm*SwB)/sw
    f(58) = (CI*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(59) = (2*CI*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(60) = (4*CI*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(61) = (eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(62) = (2*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(63) = (4*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(64) = (CI*eQED**2*gQCD**4*integralnorm*SwF)/(2._/**/REALKIND*sw**2)
    f(65) = (CI*eQED**2*gQCD**4*integralnorm*SwF)/sw**2
    f(66) = (eQED**2*gQCD**4*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(67) = (eQED**2*gQCD**4*integralnorm*SwF)/sw**2
    f(68) = (CI*eQED**2*gQCD**4*integralnorm*MB*SwF)/(2._/**/REALKIND*sw**2)
    f(69) = (CI*eQED**2*gQCD**4*integralnorm*MB*SwF)/sw**2
    f(70) = (eQED**2*gQCD**4*integralnorm*MB*SwF)/(sw**2*2._/**/REALKIND)
    f(71) = (eQED**2*gQCD**4*integralnorm*MB*SwF)/sw**2
    f(72) = (CI*eQED**2*gQCD**4*integralnorm*MT*SwF)/(2._/**/REALKIND*sw**2)
    f(73) = (eQED**2*gQCD**4*integralnorm*MT*SwF)/(sw**2*2._/**/REALKIND)
    f(74) = (CI*cw*eQED**2*gQCD**4*integralnorm*SwF)/sw
    f(75) = (2*CI*cw*eQED**2*gQCD**4*integralnorm*SwF)/sw
    f(76) = (cw*eQED**2*gQCD**4*integralnorm*SwF)/sw
    f(77) = (2*cw*eQED**2*gQCD**4*integralnorm*SwF)/sw

  c = [ 9*CI*f(46), 18*CI*f(46), CI*f(47), 3*CI*f(47), 8*CI*f(47), 9*CI*f(47), 18*CI*f(47), f(48), 3*f(48), 8*f(48), 9*f(48) &
    , 9*CI*f(49), 18*CI*f(49), CI*f(50), 3*CI*f(50), 8*CI*f(50), 9*CI*f(50), 18*CI*f(50), f(51), 3*f(51), 8*f(51), 9*f(51) &
    , 9*CI*f(52), 18*CI*f(52), CI*f(53), 3*CI*f(53), 8*CI*f(53), 9*CI*f(53), 18*CI*f(53), f(54), 3*f(54), 8*f(54), 9*f(54) &
    , 9*CI*f(55), 18*CI*f(55), CI*f(56), 3*CI*f(56), 8*CI*f(56), 9*CI*f(56), 18*CI*f(56), f(57), 3*f(57), 8*f(57), 9*f(57) &
    , 3*CI*f(58), 3*CI*f(59), 3*CI*f(60), f(61), 3*f(61), f(62), 3*f(62), f(63), 3*f(63), 3*CI*f(64), 3*CI*f(65), f(66), 3*f(66) &
    , f(67), 3*f(67), 3*CI*f(68), 3*CI*f(69), f(70), 3*f(70), f(71), 3*f(71), 3*CI*f(72), f(73), 3*f(73), 3*CI*f(74), 3*CI*f(75) &
    , f(76), 3*f(76), f(77), 3*f(77) ]
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
  complex(REALKIND) :: A(289)
  ! external WFs
  call wf_Q(P(:,1), rMB, H(1), wf(:,0))
  call wf_A(P(:,2), rMB, H(2), wf(:,-1))
  call wf_V(P(:,3), rMW, H(3), wf(:,-2))
  call wf_V(P(:,4), rMW, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_WQ_A(wf(:,-3),wf(:,0),wf(:,1))
  call vert_AW_Q(wf(:,-1),wf(:,-2),wf(:,2))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,3))
  call prop_Q_A(wf(:,1),Q(:,9),MT,1_intkind1,wf(:,4))
  call prop_A_Q(wf(:,2),Q(:,6),MT,1_intkind1,wf(:,5))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,6))
  call vert_VQ_A(wf(:,-4),wf(:,4),wf(:,7))
  call vert_AV_Q(wf(:,5),wf(:,-5),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,25),MT,1_intkind1,wf(:,9))
  call vert_AV_Q(wf(:,5),wf(:,-4),wf(:,10))
  call vert_VQ_A(wf(:,-5),wf(:,4),wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,22),MT,1_intkind1,wf(:,12))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,13))
  call prop_Q_A(wf(:,13),Q(:,17),MB,1_intkind1,wf(:,14))
  call vert_WQ_A(wf(:,-3),wf(:,14),wf(:,15))
  call prop_Q_A(wf(:,15),Q(:,25),MT,1_intkind1,wf(:,16))
  call vert_AW_Q(wf(:,5),wf(:,-3),wf(:,17))
  call vert_VQ_A(wf(:,-5),wf(:,14),wf(:,18))
  call prop_A_Q(wf(:,17),Q(:,14),MB,1_intkind1,wf(:,19))
  call vert_VQ_A(wf(:,-5),wf(:,0),wf(:,20))
  call prop_Q_A(wf(:,20),Q(:,33),MB,1_intkind1,wf(:,21))
  call vert_WQ_A(wf(:,-3),wf(:,21),wf(:,22))
  call prop_Q_A(wf(:,22),Q(:,41),MT,1_intkind1,wf(:,23))
  call vert_VQ_A(wf(:,-4),wf(:,21),wf(:,24))
  call vert_VQ_A(wf(:,3),wf(:,0),wf(:,25))
  call prop_Q_A(wf(:,25),Q(:,49),MB,1_intkind1,wf(:,26))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,27))
  call prop_A_Q(wf(:,27),Q(:,18),MB,1_intkind1,wf(:,28))
  call vert_WQ_A(wf(:,-2),wf(:,4),wf(:,29))
  call vert_AV_Q(wf(:,28),wf(:,-5),wf(:,30))
  call prop_Q_A(wf(:,29),Q(:,13),MB,1_intkind1,wf(:,31))
  call vert_AW_Q(wf(:,28),wf(:,-2),wf(:,32))
  call prop_A_Q(wf(:,32),Q(:,22),MT,1_intkind1,wf(:,33))
  call vert_AV_Q(wf(:,-1),wf(:,-5),wf(:,34))
  call prop_A_Q(wf(:,34),Q(:,34),MB,1_intkind1,wf(:,35))
  call vert_AV_Q(wf(:,35),wf(:,-4),wf(:,36))
  call vert_AW_Q(wf(:,35),wf(:,-2),wf(:,37))
  call prop_A_Q(wf(:,37),Q(:,38),MT,1_intkind1,wf(:,38))
  call vert_AV_Q(wf(:,-1),wf(:,3),wf(:,39))
  call prop_A_Q(wf(:,39),Q(:,50),MB,1_intkind1,wf(:,40))
  call vert_VV_S(wf(:,-2),wf(:,-3),wf(:,41))
  call vert_AQ_S(gH,wf(:,35),wf(:,14),wf(:,42))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-2),Q(:,4),wf(:,43))
  call vert_QA_V(wf(:,14),wf(:,35),wf(:,44))
  call prop_W_W(wf(:,43),Q(:,12),MZ,1_intkind1,wf(:,45))
  call vert_QA_Z(gZd,wf(:,14),wf(:,35),wf(:,46))
  call vert_SA_Q(gH,wf(:,41),wf(:,-1),wf(:,47))
  call prop_A_Q(wf(:,47),Q(:,14),MB,1_intkind1,wf(:,48))
  call vert_AV_Q(wf(:,-1),wf(:,43),wf(:,49))
  call prop_A_Q(wf(:,49),Q(:,14),MB,1_intkind1,wf(:,50))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,45),wf(:,51))
  call prop_A_Q(wf(:,51),Q(:,14),MB,1_intkind1,wf(:,52))
  call vert_AQ_S(gH,wf(:,28),wf(:,21),wf(:,53))
  call vert_QA_V(wf(:,21),wf(:,28),wf(:,54))
  call vert_QA_Z(gZd,wf(:,21),wf(:,28),wf(:,55))
  call vert_QS_A(gH,wf(:,0),wf(:,41),wf(:,56))
  call prop_Q_A(wf(:,56),Q(:,13),MB,1_intkind1,wf(:,57))
  call vert_VQ_A(wf(:,43),wf(:,0),wf(:,58))
  call prop_Q_A(wf(:,58),Q(:,13),MB,1_intkind1,wf(:,59))
  call vert_ZQ_A(gZd,wf(:,45),wf(:,0),wf(:,60))
  call prop_Q_A(wf(:,60),Q(:,13),MB,1_intkind1,wf(:,61))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,62))
  call counter_GGG_V(ctAGGG,wf(:,62),wf(:,-4),wf(:,-5),wf(:,63))
  call counter_GGG_V(ctAGGG,wf(:,62),wf(:,-5),wf(:,-4),wf(:,64))
  call counter_GGG_V(ctZGGG,wf(:,62),wf(:,-4),wf(:,-5),wf(:,65))
  call counter_GGG_V(ctZGGG,wf(:,62),wf(:,-5),wf(:,-4),wf(:,66))
  call counter_VVG_G(wf(:,-2),wf(:,-3),wf(:,62),wf(:,67))
  call counter_VVG_G(wf(:,-2),wf(:,-3),wf(:,-5),wf(:,68))
  call vert_UV_W(wf(:,62),Q(:,3),wf(:,-4),Q(:,16),wf(:,69))
  call counter_VVG_G(wf(:,-2),wf(:,-3),wf(:,-4),wf(:,70))
  call vert_UV_W(wf(:,62),Q(:,3),wf(:,-5),Q(:,32),wf(:,71))
  call vert_QA_V(wf(:,14),wf(:,-1),wf(:,72))
  call vert_QA_V(wf(:,0),wf(:,28),wf(:,73))
  call vert_QA_V(wf(:,21),wf(:,-1),wf(:,74))
  call vert_QA_V(wf(:,0),wf(:,35),wf(:,75))
  call counter_SG_G(wf(:,41),wf(:,62),wf(:,76))
  call counter_VG_G(wf(:,45),wf(:,62),Q(:,3),wf(:,77),Q(:,15))
  call counter_SG_G(wf(:,41),wf(:,-5),wf(:,78))
  call counter_VG_G(wf(:,45),wf(:,-5),Q(:,32),wf(:,79),Q(:,44))
  call counter_SG_G(wf(:,41),wf(:,-4),wf(:,80))
  call counter_VG_G(wf(:,45),wf(:,-4),Q(:,16),wf(:,81),Q(:,28))
  call counter_QA_V(wf(:,4),wf(:,5),wf(:,82))
  call counter_AV_Q(wf(:,5),wf(:,-5),wf(:,83))
  call counter_VQ_A(wf(:,-5),wf(:,4),wf(:,84))
  call counter_AV_Q(wf(:,5),wf(:,-4),wf(:,85))
  call prop_Q_A(wf(:,11),Q(:,41),MT,1_intkind1,wf(:,86))
  call counter_VQ_A(wf(:,-4),wf(:,4),wf(:,87))
  call prop_A_Q(wf(:,8),Q(:,38),MT,1_intkind1,wf(:,88))
  call counter_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,89))
  call counter_VQ_A(wf(:,-5),wf(:,14),wf(:,90))
  call counter_VQ_A(wf(:,-4),wf(:,21),wf(:,91))
  call vert_VQ_A(wf(:,89),wf(:,0),wf(:,92))
  call prop_Q_A(wf(:,92),Q(:,49),MB,1_intkind1,wf(:,93))
  call counter_AW_Q(wf(:,5),wf(:,-3),wf(:,94))
  call prop_Q_A(wf(:,18),Q(:,49),MB,1_intkind1,wf(:,95))
  call counter_WQ_A(wf(:,-3),wf(:,14),wf(:,96))
  call prop_Q_A(wf(:,24),Q(:,49),MB,1_intkind1,wf(:,97))
  call counter_WQ_A(wf(:,-3),wf(:,21),wf(:,98))
  call counter_AV_Q(wf(:,28),wf(:,-5),wf(:,99))
  call counter_AV_Q(wf(:,35),wf(:,-4),wf(:,100))
  call vert_AV_Q(wf(:,-1),wf(:,89),wf(:,101))
  call prop_A_Q(wf(:,101),Q(:,50),MB,1_intkind1,wf(:,102))
  call counter_AQ_S(gH,wf(:,35),wf(:,14),wf(:,103))
  call counter_QA_V(wf(:,14),wf(:,35),wf(:,104))
  call counter_QA_Z(gZd,wf(:,14),wf(:,35),wf(:,105))
  call counter_AQ_S(gH,wf(:,28),wf(:,21),wf(:,106))
  call counter_QA_V(wf(:,21),wf(:,28),wf(:,107))
  call counter_QA_Z(gZd,wf(:,21),wf(:,28),wf(:,108))
  call counter_AW_Q(wf(:,28),wf(:,-2),wf(:,109))
  call counter_WQ_A(wf(:,-2),wf(:,4),wf(:,110))
  call prop_A_Q(wf(:,30),Q(:,50),MB,1_intkind1,wf(:,111))
  call counter_AW_Q(wf(:,35),wf(:,-2),wf(:,112))
  call prop_A_Q(wf(:,36),Q(:,50),MB,1_intkind1,wf(:,113))
  call counter_AV_Q(wf(:,-1),wf(:,3),wf(:,114))
  call counter_AV_Q(wf(:,-1),wf(:,-5),wf(:,115))
  call prop_A_Q(wf(:,115),Q(:,34),MB,1_intkind1,wf(:,116))
  call vert_AV_Q(wf(:,116),wf(:,-4),wf(:,117))
  call vert_AW_Q(wf(:,116),wf(:,-2),wf(:,118))
  call prop_A_Q(wf(:,118),Q(:,38),MT,1_intkind1,wf(:,119))
  call counter_AV_Q(wf(:,-1),wf(:,-4),wf(:,120))
  call prop_A_Q(wf(:,120),Q(:,18),MB,1_intkind1,wf(:,121))
  call vert_AV_Q(wf(:,121),wf(:,-5),wf(:,122))
  call vert_AW_Q(wf(:,121),wf(:,-2),wf(:,123))
  call prop_A_Q(wf(:,123),Q(:,22),MT,1_intkind1,wf(:,124))
  call counter_SA_Q(gH,wf(:,41),wf(:,-1),wf(:,125))
  call counter_AV_Q(wf(:,-1),wf(:,43),wf(:,126))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,45),wf(:,127))
  call vert_AQ_S(gH,wf(:,116),wf(:,14),wf(:,128))
  call vert_QA_V(wf(:,14),wf(:,116),wf(:,129))
  call vert_QA_Z(gZd,wf(:,14),wf(:,116),wf(:,130))
  call vert_AQ_S(gH,wf(:,121),wf(:,21),wf(:,131))
  call vert_QA_V(wf(:,21),wf(:,121),wf(:,132))
  call vert_QA_Z(gZd,wf(:,21),wf(:,121),wf(:,133))
  call counter_AW_Q(wf(:,-1),wf(:,-2),wf(:,134))
  call prop_A_Q(wf(:,134),Q(:,6),MT,1_intkind1,wf(:,135))
  call vert_QA_V(wf(:,4),wf(:,135),wf(:,136))
  call vert_AV_Q(wf(:,135),wf(:,-5),wf(:,137))
  call vert_AV_Q(wf(:,135),wf(:,-4),wf(:,138))
  call prop_A_Q(wf(:,138),Q(:,22),MT,1_intkind1,wf(:,139))
  call vert_AW_Q(wf(:,135),wf(:,-3),wf(:,140))
  call prop_A_Q(wf(:,140),Q(:,14),MB,1_intkind1,wf(:,141))
  call counter_VQ_A(wf(:,3),wf(:,0),wf(:,142))
  call counter_VQ_A(wf(:,-5),wf(:,0),wf(:,143))
  call prop_Q_A(wf(:,143),Q(:,33),MB,1_intkind1,wf(:,144))
  call vert_WQ_A(wf(:,-3),wf(:,144),wf(:,145))
  call prop_Q_A(wf(:,145),Q(:,41),MT,1_intkind1,wf(:,146))
  call vert_VQ_A(wf(:,-4),wf(:,144),wf(:,147))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,148))
  call prop_Q_A(wf(:,148),Q(:,17),MB,1_intkind1,wf(:,149))
  call vert_WQ_A(wf(:,-3),wf(:,149),wf(:,150))
  call prop_Q_A(wf(:,150),Q(:,25),MT,1_intkind1,wf(:,151))
  call vert_VQ_A(wf(:,-5),wf(:,149),wf(:,152))
  call counter_WQ_A(wf(:,-3),wf(:,0),wf(:,153))
  call prop_Q_A(wf(:,153),Q(:,9),MT,1_intkind1,wf(:,154))
  call vert_QA_V(wf(:,154),wf(:,5),wf(:,155))
  call vert_VQ_A(wf(:,-4),wf(:,154),wf(:,156))
  call prop_Q_A(wf(:,156),Q(:,25),MT,1_intkind1,wf(:,157))
  call vert_VQ_A(wf(:,-5),wf(:,154),wf(:,158))
  call counter_QS_A(gH,wf(:,0),wf(:,41),wf(:,159))
  call counter_VQ_A(wf(:,43),wf(:,0),wf(:,160))
  call counter_ZQ_A(gZd,wf(:,45),wf(:,0),wf(:,161))
  call vert_AQ_S(gH,wf(:,28),wf(:,144),wf(:,162))
  call vert_QA_V(wf(:,144),wf(:,28),wf(:,163))
  call vert_QA_Z(gZd,wf(:,144),wf(:,28),wf(:,164))
  call vert_AQ_S(gH,wf(:,35),wf(:,149),wf(:,165))
  call vert_QA_V(wf(:,149),wf(:,35),wf(:,166))
  call vert_QA_Z(gZd,wf(:,149),wf(:,35),wf(:,167))
  call vert_WQ_A(wf(:,-2),wf(:,154),wf(:,168))
  call prop_Q_A(wf(:,168),Q(:,13),MB,1_intkind1,wf(:,169))
  call vert_AV_Q(wf(:,5),wf(:,3),wf(:,170))
  call counter_Q_A(cttt,wf(:,4),Q(:,9),wf(:,171))
  call prop_A_Q(wf(:,170),Q(:,54),MT,1_intkind1,wf(:,172))
  call vert_VQ_A(wf(:,3),wf(:,4),wf(:,173))
  call counter_A_Q(cttt,wf(:,5),Q(:,6),wf(:,174))
  call prop_Q_A(wf(:,173),Q(:,57),MT,1_intkind1,wf(:,175))
  call counter_V_V(ctGG,wf(:,3),Q(:,48),wf(:,176))
  call prop_Q_A(wf(:,171),Q(:,9),MT,1_intkind1,wf(:,177))
  call vert_VQ_A(wf(:,-4),wf(:,177),wf(:,178))
  call vert_VQ_A(wf(:,-5),wf(:,177),wf(:,179))
  call prop_A_Q(wf(:,174),Q(:,6),MT,1_intkind1,wf(:,180))
  call vert_AV_Q(wf(:,180),wf(:,-4),wf(:,181))
  call vert_AV_Q(wf(:,180),wf(:,-5),wf(:,182))
  call counter_Q_A(cttt,wf(:,9),Q(:,25),wf(:,183))
  call counter_A_Q(cttt,wf(:,12),Q(:,22),wf(:,184))
  call counter_Q_A(ctbb,wf(:,14),Q(:,17),wf(:,185))
  call prop_Q_A(wf(:,185),Q(:,17),MB,1_intkind1,wf(:,186))
  call vert_WQ_A(wf(:,-3),wf(:,186),wf(:,187))
  call vert_VQ_A(wf(:,-5),wf(:,186),wf(:,188))
  call vert_AW_Q(wf(:,180),wf(:,-3),wf(:,189))
  call counter_Q_A(cttt,wf(:,16),Q(:,25),wf(:,190))
  call counter_A_Q(ctbb,wf(:,19),Q(:,14),wf(:,191))
  call counter_Q_A(ctbb,wf(:,21),Q(:,33),wf(:,192))
  call prop_Q_A(wf(:,192),Q(:,33),MB,1_intkind1,wf(:,193))
  call vert_WQ_A(wf(:,-3),wf(:,193),wf(:,194))
  call vert_VQ_A(wf(:,-4),wf(:,193),wf(:,195))
  call counter_Q_A(cttt,wf(:,23),Q(:,41),wf(:,196))
  call counter_Q_A(ctbb,wf(:,26),Q(:,49),wf(:,197))
  call vert_VQ_A(wf(:,176),wf(:,0),wf(:,198))
  call vert_WQ_A(wf(:,-2),wf(:,177),wf(:,199))
  call counter_A_Q(ctbb,wf(:,28),Q(:,18),wf(:,200))
  call prop_A_Q(wf(:,200),Q(:,18),MB,1_intkind1,wf(:,201))
  call vert_AW_Q(wf(:,201),wf(:,-2),wf(:,202))
  call vert_AV_Q(wf(:,201),wf(:,-5),wf(:,203))
  call counter_Q_A(ctbb,wf(:,31),Q(:,13),wf(:,204))
  call counter_A_Q(cttt,wf(:,33),Q(:,22),wf(:,205))
  call counter_A_Q(ctbb,wf(:,35),Q(:,34),wf(:,206))
  call prop_A_Q(wf(:,206),Q(:,34),MB,1_intkind1,wf(:,207))
  call vert_AW_Q(wf(:,207),wf(:,-2),wf(:,208))
  call vert_AV_Q(wf(:,207),wf(:,-4),wf(:,209))
  call counter_A_Q(cttt,wf(:,38),Q(:,38),wf(:,210))
  call counter_A_Q(ctbb,wf(:,40),Q(:,50),wf(:,211))
  call vert_AV_Q(wf(:,-1),wf(:,176),wf(:,212))
  call vert_SA_Q(gH,wf(:,41),wf(:,35),wf(:,213))
  call prop_A_Q(wf(:,213),Q(:,46),MB,1_intkind1,wf(:,214))
  call vert_AV_Q(wf(:,35),wf(:,43),wf(:,215))
  call prop_A_Q(wf(:,215),Q(:,46),MB,1_intkind1,wf(:,216))
  call vert_AZ_Q(gZd,wf(:,35),wf(:,45),wf(:,217))
  call prop_A_Q(wf(:,217),Q(:,46),MB,1_intkind1,wf(:,218))
  call vert_QS_A(gH,wf(:,14),wf(:,41),wf(:,219))
  call prop_Q_A(wf(:,219),Q(:,29),MB,1_intkind1,wf(:,220))
  call vert_VQ_A(wf(:,43),wf(:,14),wf(:,221))
  call prop_Q_A(wf(:,221),Q(:,29),MB,1_intkind1,wf(:,222))
  call vert_ZQ_A(gZd,wf(:,45),wf(:,14),wf(:,223))
  call prop_Q_A(wf(:,223),Q(:,29),MB,1_intkind1,wf(:,224))
  call counter_A_Q(ctbb,wf(:,48),Q(:,14),wf(:,225))
  call counter_A_Q(ctbb,wf(:,50),Q(:,14),wf(:,226))
  call counter_A_Q(ctbb,wf(:,52),Q(:,14),wf(:,227))
  call vert_SA_Q(gH,wf(:,41),wf(:,28),wf(:,228))
  call prop_A_Q(wf(:,228),Q(:,30),MB,1_intkind1,wf(:,229))
  call vert_AV_Q(wf(:,28),wf(:,43),wf(:,230))
  call prop_A_Q(wf(:,230),Q(:,30),MB,1_intkind1,wf(:,231))
  call vert_AZ_Q(gZd,wf(:,28),wf(:,45),wf(:,232))
  call prop_A_Q(wf(:,232),Q(:,30),MB,1_intkind1,wf(:,233))
  call vert_QS_A(gH,wf(:,21),wf(:,41),wf(:,234))
  call prop_Q_A(wf(:,234),Q(:,45),MB,1_intkind1,wf(:,235))
  call vert_VQ_A(wf(:,43),wf(:,21),wf(:,236))
  call prop_Q_A(wf(:,236),Q(:,45),MB,1_intkind1,wf(:,237))
  call vert_ZQ_A(gZd,wf(:,45),wf(:,21),wf(:,238))
  call prop_Q_A(wf(:,238),Q(:,45),MB,1_intkind1,wf(:,239))
  call counter_Q_A(ctbb,wf(:,57),Q(:,13),wf(:,240))
  call counter_Q_A(ctbb,wf(:,59),Q(:,13),wf(:,241))
  call counter_Q_A(ctbb,wf(:,61),Q(:,13),wf(:,242))
  call vert_QA_V(wf(:,0),wf(:,19),wf(:,243))
  call vert_AV_Q(wf(:,19),wf(:,-4),wf(:,244))
  call prop_A_Q(wf(:,244),Q(:,30),MB,1_intkind1,wf(:,245))
  call vert_AV_Q(wf(:,19),wf(:,-5),wf(:,246))
  call prop_A_Q(wf(:,246),Q(:,46),MB,1_intkind1,wf(:,247))
  call vert_AW_Q(wf(:,12),wf(:,-3),wf(:,248))
  call prop_A_Q(wf(:,248),Q(:,30),MB,1_intkind1,wf(:,249))
  call vert_AV_Q(wf(:,12),wf(:,-5),wf(:,250))
  call prop_A_Q(wf(:,250),Q(:,54),MT,1_intkind1,wf(:,251))
  call vert_AW_Q(wf(:,88),wf(:,-3),wf(:,252))
  call prop_A_Q(wf(:,252),Q(:,46),MB,1_intkind1,wf(:,253))
  call vert_AV_Q(wf(:,88),wf(:,-4),wf(:,254))
  call prop_A_Q(wf(:,254),Q(:,54),MT,1_intkind1,wf(:,255))
  call vert_QA_V(wf(:,31),wf(:,-1),wf(:,256))
  call vert_VQ_A(wf(:,-4),wf(:,31),wf(:,257))
  call prop_Q_A(wf(:,257),Q(:,29),MB,1_intkind1,wf(:,258))
  call vert_VQ_A(wf(:,-5),wf(:,31),wf(:,259))
  call prop_Q_A(wf(:,259),Q(:,45),MB,1_intkind1,wf(:,260))
  call vert_WQ_A(wf(:,-2),wf(:,9),wf(:,261))
  call prop_Q_A(wf(:,261),Q(:,29),MB,1_intkind1,wf(:,262))
  call vert_VQ_A(wf(:,-5),wf(:,9),wf(:,263))
  call prop_Q_A(wf(:,263),Q(:,57),MT,1_intkind1,wf(:,264))
  call vert_WQ_A(wf(:,-2),wf(:,86),wf(:,265))
  call prop_Q_A(wf(:,265),Q(:,45),MB,1_intkind1,wf(:,266))
  call vert_VQ_A(wf(:,-4),wf(:,86),wf(:,267))
  call prop_Q_A(wf(:,267),Q(:,57),MT,1_intkind1,wf(:,268))
  call vert_QA_V(wf(:,57),wf(:,-1),wf(:,269))
  call vert_QA_V(wf(:,59),wf(:,-1),wf(:,270))
  call vert_QA_V(wf(:,61),wf(:,-1),wf(:,271))
  call vert_QA_V(wf(:,0),wf(:,48),wf(:,272))
  call vert_QA_V(wf(:,0),wf(:,50),wf(:,273))
  call vert_QA_V(wf(:,0),wf(:,52),wf(:,274))
  call vert_VQ_A(wf(:,-4),wf(:,57),wf(:,275))
  call prop_Q_A(wf(:,275),Q(:,29),MB,1_intkind1,wf(:,276))
  call vert_VQ_A(wf(:,-4),wf(:,59),wf(:,277))
  call prop_Q_A(wf(:,277),Q(:,29),MB,1_intkind1,wf(:,278))
  call vert_VQ_A(wf(:,-4),wf(:,61),wf(:,279))
  call prop_Q_A(wf(:,279),Q(:,29),MB,1_intkind1,wf(:,280))
  call vert_VQ_A(wf(:,-5),wf(:,57),wf(:,281))
  call prop_Q_A(wf(:,281),Q(:,45),MB,1_intkind1,wf(:,282))
  call vert_VQ_A(wf(:,-5),wf(:,59),wf(:,283))
  call prop_Q_A(wf(:,283),Q(:,45),MB,1_intkind1,wf(:,284))
  call vert_VQ_A(wf(:,-5),wf(:,61),wf(:,285))
  call prop_Q_A(wf(:,285),Q(:,45),MB,1_intkind1,wf(:,286))
  call vert_AV_Q(wf(:,48),wf(:,-4),wf(:,287))
  call prop_A_Q(wf(:,287),Q(:,30),MB,1_intkind1,wf(:,288))
  call vert_AV_Q(wf(:,50),wf(:,-4),wf(:,289))
  call prop_A_Q(wf(:,289),Q(:,30),MB,1_intkind1,wf(:,290))
  call vert_AV_Q(wf(:,52),wf(:,-4),wf(:,291))
  call prop_A_Q(wf(:,291),Q(:,30),MB,1_intkind1,wf(:,292))
  call vert_AV_Q(wf(:,48),wf(:,-5),wf(:,293))
  call prop_A_Q(wf(:,293),Q(:,46),MB,1_intkind1,wf(:,294))
  call vert_AV_Q(wf(:,50),wf(:,-5),wf(:,295))
  call prop_A_Q(wf(:,295),Q(:,46),MB,1_intkind1,wf(:,296))
  call vert_AV_Q(wf(:,52),wf(:,-5),wf(:,297))
  call prop_A_Q(wf(:,297),Q(:,46),MB,1_intkind1,wf(:,298))
  call vert_WQ_A(wf(:,-2),wf(:,16),wf(:,299))
  call prop_Q_A(wf(:,299),Q(:,29),MB,1_intkind1,wf(:,300))
  call vert_VQ_A(wf(:,-5),wf(:,16),wf(:,301))
  call prop_Q_A(wf(:,301),Q(:,57),MT,1_intkind1,wf(:,302))
  call vert_WQ_A(wf(:,-3),wf(:,95),wf(:,303))
  call prop_Q_A(wf(:,303),Q(:,57),MT,1_intkind1,wf(:,304))
  call vert_AW_Q(wf(:,33),wf(:,-3),wf(:,305))
  call prop_A_Q(wf(:,305),Q(:,30),MB,1_intkind1,wf(:,306))
  call vert_AV_Q(wf(:,33),wf(:,-5),wf(:,307))
  call prop_A_Q(wf(:,307),Q(:,54),MT,1_intkind1,wf(:,308))
  call vert_AW_Q(wf(:,111),wf(:,-2),wf(:,309))
  call prop_A_Q(wf(:,309),Q(:,54),MT,1_intkind1,wf(:,310))
  call vert_WQ_A(wf(:,-2),wf(:,23),wf(:,311))
  call prop_Q_A(wf(:,311),Q(:,45),MB,1_intkind1,wf(:,312))
  call vert_VQ_A(wf(:,-4),wf(:,23),wf(:,313))
  call prop_Q_A(wf(:,313),Q(:,57),MT,1_intkind1,wf(:,314))
  call vert_WQ_A(wf(:,-3),wf(:,97),wf(:,315))
  call prop_Q_A(wf(:,315),Q(:,57),MT,1_intkind1,wf(:,316))
  call vert_AW_Q(wf(:,38),wf(:,-3),wf(:,317))
  call prop_A_Q(wf(:,317),Q(:,46),MB,1_intkind1,wf(:,318))
  call vert_AV_Q(wf(:,38),wf(:,-4),wf(:,319))
  call prop_A_Q(wf(:,319),Q(:,54),MT,1_intkind1,wf(:,320))
  call vert_AW_Q(wf(:,113),wf(:,-2),wf(:,321))
  call prop_A_Q(wf(:,321),Q(:,54),MT,1_intkind1,wf(:,322))
  call vert_WQ_A(wf(:,-3),wf(:,26),wf(:,323))
  call prop_Q_A(wf(:,323),Q(:,57),MT,1_intkind1,wf(:,324))
  call vert_AW_Q(wf(:,40),wf(:,-2),wf(:,325))
  call prop_A_Q(wf(:,325),Q(:,54),MT,1_intkind1,wf(:,326))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,9) - MT2)
  den(2) = 1 / (Q(5,6) - MT2)
  den(3) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,25) - MT2)
  den(9) = 1 / (Q(5,22) - MT2)
  den(12) = 1 / (Q(5,17) - MB2)
  den(15) = 1 / (Q(5,14) - MB2)
  den(18) = 1 / (Q(5,33) - MB2)
  den(19) = 1 / (Q(5,41) - MT2)
  den(23) = 1 / (Q(5,49) - MB2)
  den(26) = 1 / (Q(5,18) - MB2)
  den(27) = 1 / (Q(5,13) - MB2)
  den(32) = 1 / (Q(5,34) - MB2)
  den(34) = 1 / (Q(5,38) - MT2)
  den(37) = 1 / (Q(5,50) - MB2)
  den(40) = 1 / (Q(5,12) - MH2)
  den(43) = 1 / (Q(5,12))
  den(45) = 1 / (Q(5,12) - MZ2)
  den(77) = 1 / (Q(5,3))
  den(81) = 1 / (Q(5,44))
  den(83) = 1 / (Q(5,28))
  den(92) = 1 / (Q(5,19))
  den(96) = 1 / (Q(5,35))
  den(151) = 1 / (Q(5,54) - MT2)
  den(155) = 1 / (Q(5,57) - MT2)
  den(158) = 1 / (Q(5,15))
  den(205) = 1 / (Q(5,46) - MB2)
  den(215) = 1 / (Q(5,29) - MB2)
  den(231) = 1 / (Q(5,30) - MB2)
  den(241) = 1 / (Q(5,45) - MB2)

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
  den(46) = den(41)*den(45)
  den(47) = den(15)*den(40)
  den(48) = den(12)*den(47)
  den(49) = den(15)*den(43)
  den(50) = den(12)*den(49)
  den(51) = den(15)*den(45)
  den(52) = den(12)*den(51)
  den(53) = den(18)*den(26)
  den(54) = den(40)*den(53)
  den(55) = den(43)*den(53)
  den(56) = den(45)*den(53)
  den(57) = den(27)*den(40)
  den(58) = den(26)*den(57)
  den(59) = den(27)*den(43)
  den(60) = den(26)*den(59)
  den(61) = den(27)*den(45)
  den(62) = den(26)*den(61)
  den(63) = den(18)*den(47)
  den(64) = den(18)*den(49)
  den(65) = den(18)*den(51)
  den(66) = den(32)*den(57)
  den(67) = den(32)*den(59)
  den(68) = den(32)*den(61)
  den(69) = den(3)*den(57)
  den(70) = den(3)*den(59)
  den(71) = den(3)*den(61)
  den(72) = den(24)*den(40)
  den(73) = den(24)*den(43)
  den(74) = den(24)*den(45)
  den(75) = den(12)*den(35)
  den(76) = den(18)*den(30)
  den(78) = den(43)*den(77)
  den(79) = den(45)*den(77)
  den(80) = den(3)*den(77)
  den(82) = den(77)*den(81)
  den(84) = den(77)*den(83)
  den(85) = den(12)*den(81)
  den(86) = den(26)*den(81)
  den(87) = den(18)*den(83)
  den(88) = den(32)*den(83)
  den(89) = den(40)*den(77)
  den(90) = den(3)*den(89)
  den(91) = den(3)*den(79)
  den(93) = den(77)*den(92)
  den(94) = den(40)*den(93)
  den(95) = den(45)*den(93)
  den(97) = den(77)*den(96)
  den(98) = den(40)*den(97)
  den(99) = den(45)*den(97)
  den(100) = den(1)*den(19)
  den(101) = den(2)*den(100)
  den(102) = den(2)*den(34)
  den(103) = den(1)*den(102)
  den(104) = den(12)*den(23)
  den(105) = den(2)*den(104)
  den(106) = den(12)*den(102)
  den(107) = den(18)*den(23)
  den(108) = den(2)*den(107)
  den(109) = den(10)*den(18)
  den(110) = den(12)*den(92)
  den(111) = den(40)*den(110)
  den(112) = den(45)*den(110)
  den(113) = den(26)*den(92)
  den(114) = den(40)*den(113)
  den(115) = den(45)*den(113)
  den(116) = den(18)*den(96)
  den(117) = den(40)*den(116)
  den(118) = den(45)*den(116)
  den(119) = den(32)*den(96)
  den(120) = den(40)*den(119)
  den(121) = den(45)*den(119)
  den(122) = den(26)*den(100)
  den(123) = den(26)*den(37)
  den(124) = den(1)*den(123)
  den(125) = den(7)*den(32)
  den(126) = den(32)*den(37)
  den(127) = den(1)*den(126)
  den(128) = den(13)*den(32)
  den(129) = den(20)*den(26)
  den(130) = den(3)*den(28)
  den(131) = den(40)*den(104)
  den(132) = den(43)*den(104)
  den(133) = den(45)*den(104)
  den(134) = den(40)*den(107)
  den(135) = den(43)*den(107)
  den(136) = den(45)*den(107)
  den(137) = den(3)*den(16)
  den(138) = den(40)*den(123)
  den(139) = den(43)*den(123)
  den(140) = den(45)*den(123)
  den(141) = den(40)*den(126)
  den(142) = den(43)*den(126)
  den(143) = den(45)*den(126)
  den(144) = den(38)*den(40)
  den(145) = den(38)*den(43)
  den(146) = den(38)*den(45)
  den(147) = den(3)*den(47)
  den(148) = den(3)*den(49)
  den(149) = den(3)*den(51)
  den(150) = den(2)*den(3)
  den(152) = den(150)*den(151)
  den(153) = den(1)*den(152)
  den(154) = den(1)*den(3)
  den(156) = den(154)*den(155)
  den(157) = den(2)*den(156)
  den(159) = den(4)*den(158)
  den(160) = den(3)*den(159)
  den(161) = den(1)**2
  den(162) = den(102)*den(161)
  den(163) = den(10)*den(161)
  den(164) = den(2)**2
  den(165) = den(100)*den(164)
  den(166) = den(7)*den(164)
  den(167) = den(7)*den(102)
  den(168) = den(10)*den(100)
  den(169) = den(12)**2
  den(170) = den(102)*den(169)
  den(171) = den(16)*den(169)
  den(172) = den(104)*den(164)
  den(173) = den(13)*den(164)
  den(174) = den(13)*den(102)
  den(175) = den(16)*den(104)
  den(176) = den(18)**2
  den(177) = den(10)*den(176)
  den(178) = den(16)*den(176)
  den(179) = den(107)*den(164)
  den(180) = den(20)*den(164)
  den(181) = den(10)*den(20)
  den(182) = den(16)*den(107)
  den(183) = den(16)*den(24)
  den(184) = den(3)**2
  den(185) = den(16)*den(184)
  den(186) = den(24)*den(164)
  den(187) = den(123)*den(161)
  den(188) = den(30)*den(161)
  den(189) = den(26)**2
  den(190) = den(100)*den(189)
  den(191) = den(28)*den(189)
  den(192) = den(28)*den(123)
  den(193) = den(30)*den(100)
  den(194) = den(126)*den(161)
  den(195) = den(35)*den(161)
  den(196) = den(32)**2
  den(197) = den(7)*den(196)
  den(198) = den(28)*den(196)
  den(199) = den(28)*den(126)
  den(200) = den(7)*den(35)
  den(201) = den(38)*den(161)
  den(202) = den(28)*den(38)
  den(203) = den(28)*den(184)
  den(204) = den(32)*den(40)
  den(206) = den(204)*den(205)
  den(207) = den(12)*den(206)
  den(208) = den(32)*den(43)
  den(209) = den(205)*den(208)
  den(210) = den(12)*den(209)
  den(211) = den(32)*den(45)
  den(212) = den(205)*den(211)
  den(213) = den(12)*den(212)
  den(214) = den(12)*den(40)
  den(216) = den(214)*den(215)
  den(217) = den(32)*den(216)
  den(218) = den(12)*den(43)
  den(219) = den(215)*den(218)
  den(220) = den(32)*den(219)
  den(221) = den(12)*den(45)
  den(222) = den(215)*den(221)
  den(223) = den(32)*den(222)
  den(224) = den(47)*den(169)
  den(225) = den(49)*den(169)
  den(226) = den(51)*den(169)
  den(227) = den(47)*den(104)
  den(228) = den(49)*den(104)
  den(229) = den(51)*den(104)
  den(230) = den(26)*den(40)
  den(232) = den(230)*den(231)
  den(233) = den(18)*den(232)
  den(234) = den(26)*den(43)
  den(235) = den(231)*den(234)
  den(236) = den(18)*den(235)
  den(237) = den(26)*den(45)
  den(238) = den(231)*den(237)
  den(239) = den(18)*den(238)
  den(240) = den(18)*den(40)
  den(242) = den(240)*den(241)
  den(243) = den(26)*den(242)
  den(244) = den(18)*den(43)
  den(245) = den(241)*den(244)
  den(246) = den(26)*den(245)
  den(247) = den(18)*den(45)
  den(248) = den(241)*den(247)
  den(249) = den(26)*den(248)
  den(250) = den(57)*den(123)
  den(251) = den(59)*den(123)
  den(252) = den(61)*den(123)
  den(253) = den(57)*den(189)
  den(254) = den(59)*den(189)
  den(255) = den(61)*den(189)
  den(256) = den(47)*den(176)
  den(257) = den(49)*den(176)
  den(258) = den(51)*den(176)
  den(259) = den(47)*den(107)
  den(260) = den(49)*den(107)
  den(261) = den(51)*den(107)
  den(262) = den(57)*den(126)
  den(263) = den(59)*den(126)
  den(264) = den(61)*den(126)
  den(265) = den(57)*den(196)
  den(266) = den(59)*den(196)
  den(267) = den(61)*den(196)
  den(268) = den(38)*den(57)
  den(269) = den(38)*den(59)
  den(270) = den(38)*den(61)
  den(271) = den(24)*den(47)
  den(272) = den(24)*den(49)
  den(273) = den(24)*den(51)
  den(274) = den(47)*den(184)
  den(275) = den(49)*den(184)
  den(276) = den(51)*den(184)
  den(277) = den(57)*den(184)
  den(278) = den(59)*den(184)
  den(279) = den(61)*den(184)
  den(280) = den(35)*den(169)
  den(281) = den(13)*den(196)
  den(282) = den(13)*den(35)
  den(283) = den(30)*den(176)
  den(284) = den(20)*den(189)
  den(285) = den(20)*den(30)
  den(286) = den(16)*den(158)
  den(287) = den(16)*den(231)
  den(288) = den(16)*den(205)
  den(289) = den(10)*den(231)
  den(290) = den(10)*den(151)
  den(291) = den(102)*den(205)
  den(292) = den(102)*den(151)
  den(293) = den(28)*den(158)
  den(294) = den(28)*den(215)
  den(295) = den(28)*den(241)
  den(296) = den(7)*den(215)
  den(297) = den(7)*den(155)
  den(298) = den(100)*den(241)
  den(299) = den(100)*den(155)
  den(300) = den(57)*den(158)
  den(301) = den(59)*den(158)
  den(302) = den(61)*den(158)
  den(303) = den(47)*den(158)
  den(304) = den(49)*den(158)
  den(305) = den(51)*den(158)
  den(306) = den(57)*den(215)
  den(307) = den(59)*den(215)
  den(308) = den(61)*den(215)
  den(309) = den(57)*den(241)
  den(310) = den(59)*den(241)
  den(311) = den(61)*den(241)
  den(312) = den(47)*den(231)
  den(313) = den(49)*den(231)
  den(314) = den(51)*den(231)
  den(315) = den(47)*den(205)
  den(316) = den(49)*den(205)
  den(317) = den(51)*den(205)
  den(318) = den(13)*den(215)
  den(319) = den(13)*den(155)
  den(320) = den(104)*den(155)
  den(321) = den(30)*den(231)
  den(322) = den(30)*den(151)
  den(323) = den(123)*den(151)
  den(324) = den(20)*den(241)
  den(325) = den(20)*den(155)
  den(326) = den(107)*den(155)
  den(327) = den(35)*den(205)
  den(328) = den(35)*den(151)
  den(329) = den(126)*den(151)
  den(330) = den(24)*den(155)
  den(331) = den(38)*den(151)
  den(332) = den(3)*den(40)*den(77)
  den(333) = den(3)*den(43)*den(77)
  den(334) = den(3)*den(45)*den(77)
  den(335) = den(43)*den(93)
  den(336) = den(43)*den(97)
  den(337) = den(1)*den(2)*den(3)
  den(338) = den(2)*den(12)
  den(339) = den(2)*den(18)
  den(340) = den(1)*den(26)
  den(341) = den(1)*den(32)
  den(342) = den(12)*den(32)*den(40)
  den(343) = den(12)*den(32)*den(43)
  den(344) = den(12)*den(32)*den(45)
  den(345) = den(43)*den(110)
  den(346) = den(18)*den(26)*den(40)
  den(347) = den(18)*den(26)*den(43)
  den(348) = den(18)*den(26)*den(45)
  den(349) = den(43)*den(113)
  den(350) = den(43)*den(116)
  den(351) = den(43)*den(119)
  den(352) = den(3)*den(40)
  den(353) = den(3)*den(43)
  den(354) = den(3)*den(45)
  den(355) = den(2)*den(297)
  den(356) = den(2)*den(299)
  den(357) = den(1)*den(290)
  den(358) = den(1)*den(292)
  den(359) = den(2)*den(319)
  den(360) = den(2)*den(320)
  den(361) = den(12)*den(288)
  den(362) = den(12)*den(291)
  den(363) = den(2)*den(325)
  den(364) = den(2)*den(326)
  den(365) = den(18)*den(287)
  den(366) = den(18)*den(289)
  den(367) = den(2)*den(330)
  den(368) = den(3)*den(286)
  den(369) = den(26)*den(295)
  den(370) = den(26)*den(298)
  den(371) = den(1)*den(322)
  den(372) = den(1)*den(323)
  den(373) = den(32)*den(294)
  den(374) = den(32)*den(296)
  den(375) = den(1)*den(328)
  den(376) = den(1)*den(329)
  den(377) = den(3)*den(293)
  den(378) = den(1)*den(331)
  den(379) = den(12)*den(315)
  den(380) = den(12)*den(316)
  den(381) = den(12)*den(317)
  den(382) = den(26)*den(309)
  den(383) = den(26)*den(310)
  den(384) = den(26)*den(311)
  den(385) = den(18)*den(312)
  den(386) = den(18)*den(313)
  den(387) = den(18)*den(314)
  den(388) = den(32)*den(306)
  den(389) = den(32)*den(307)
  den(390) = den(32)*den(308)
  den(391) = den(3)*den(300)
  den(392) = den(3)*den(301)
  den(393) = den(3)*den(302)
  den(394) = den(3)*den(303)
  den(395) = den(3)*den(304)
  den(396) = den(3)*den(305)
  den(397) = den(32)*den(318)
  den(398) = den(12)*den(327)
  den(399) = den(26)*den(324)
  den(400) = den(18)*den(321)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(289)

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
  A(14) = cont_SS(wf(:,41),wf(:,42)) * den(42)
  A(15) = cont_VV(wf(:,43),wf(:,44)) * den(44)
  A(16) = cont_VV(wf(:,45),wf(:,46)) * den(46)
  A(17) = cont_QA(wf(:,18),wf(:,48)) * den(48)
  A(18) = cont_QA(wf(:,18),wf(:,50)) * den(50)
  A(19) = cont_QA(wf(:,18),wf(:,52)) * den(52)
  A(20) = cont_SS(wf(:,41),wf(:,53)) * den(54)
  A(21) = cont_VV(wf(:,43),wf(:,54)) * den(55)
  A(22) = cont_VV(wf(:,45),wf(:,55)) * den(56)
  A(23) = cont_QA(wf(:,30),wf(:,57)) * den(58)
  A(24) = cont_QA(wf(:,30),wf(:,59)) * den(60)
  A(25) = cont_QA(wf(:,30),wf(:,61)) * den(62)
  A(26) = cont_QA(wf(:,24),wf(:,48)) * den(63)
  A(27) = cont_QA(wf(:,24),wf(:,50)) * den(64)
  A(28) = cont_QA(wf(:,24),wf(:,52)) * den(65)
  A(29) = cont_QA(wf(:,36),wf(:,57)) * den(66)
  A(30) = cont_QA(wf(:,36),wf(:,59)) * den(67)
  A(31) = cont_QA(wf(:,36),wf(:,61)) * den(68)
  A(32) = cont_QA(wf(:,39),wf(:,57)) * den(69)
  A(33) = cont_QA(wf(:,39),wf(:,59)) * den(70)
  A(34) = cont_QA(wf(:,39),wf(:,61)) * den(71)
  A(35) = cont_QA(wf(:,26),wf(:,47)) * den(72)
  A(36) = cont_QA(wf(:,26),wf(:,49)) * den(73)
  A(37) = cont_QA(wf(:,26),wf(:,51)) * den(74)
  A(38) = cont_QA(wf(:,15),wf(:,38)) * den(75)
  A(39) = cont_QA(wf(:,22),wf(:,33)) * den(76)

  A(40) = cont_VV(wf(:,43),wf(:,63)) * den(78)
  A(41) = cont_VV(wf(:,43),wf(:,64)) * den(78)
  A(42) = cont_VV(wf(:,45),wf(:,65)) * den(79)
  A(43) = cont_VV(wf(:,45),wf(:,66)) * den(79)
  A(44) = cont_VV(wf(:,3),wf(:,67)) * den(80)
  A(45) = cont_VV(wf(:,68),wf(:,69)) * den(82)
  A(46) = cont_VV(wf(:,70),wf(:,71)) * den(84)
  A(47) = cont_VV(wf(:,68),wf(:,72)) * den(85)
  A(48) = cont_VV(wf(:,68),wf(:,73)) * den(86)
  A(49) = cont_VV(wf(:,70),wf(:,74)) * den(87)
  A(50) = cont_VV(wf(:,70),wf(:,75)) * den(88)
  A(51) = cont_VV(wf(:,3),wf(:,76)) * den(90)
  A(52) = cont_VV(wf(:,3),wf(:,77)) * den(91)
  A(53) = cont_VV(wf(:,69),wf(:,78)) * den(94)
  A(54) = cont_VV(wf(:,69),wf(:,79)) * den(95)
  A(55) = cont_VV(wf(:,71),wf(:,80)) * den(98)
  A(56) = cont_VV(wf(:,71),wf(:,81)) * den(99)
  A(57) = cont_VV(wf(:,3),wf(:,82)) * den(5)
  A(58) = cont_QA(wf(:,9),wf(:,83)) * den(8)
  A(59) = cont_QA(wf(:,12),wf(:,84)) * den(11)
  A(60) = cont_QA(wf(:,85),wf(:,86)) * den(101)
  A(61) = cont_QA(wf(:,87),wf(:,88)) * den(103)
  A(62) = cont_VV(wf(:,6),wf(:,89)) * den(5)
  A(63) = cont_QA(wf(:,16),wf(:,83)) * den(14)
  A(64) = cont_QA(wf(:,19),wf(:,90)) * den(17)
  A(65) = cont_QA(wf(:,23),wf(:,85)) * den(21)
  A(66) = cont_QA(wf(:,19),wf(:,91)) * den(22)
  A(67) = cont_QA(wf(:,17),wf(:,93)) * den(25)
  A(68) = cont_QA(wf(:,94),wf(:,95)) * den(105)
  A(69) = cont_QA(wf(:,88),wf(:,96)) * den(106)
  A(70) = cont_QA(wf(:,94),wf(:,97)) * den(108)
  A(71) = cont_QA(wf(:,12),wf(:,98)) * den(109)
  A(72) = cont_QA(wf(:,26),wf(:,94)) * den(25)
  A(73) = cont_QA(wf(:,31),wf(:,99)) * den(29)
  A(74) = cont_QA(wf(:,33),wf(:,84)) * den(31)
  A(75) = cont_QA(wf(:,31),wf(:,100)) * den(33)
  A(76) = cont_QA(wf(:,38),wf(:,87)) * den(36)
  A(77) = cont_QA(wf(:,29),wf(:,102)) * den(39)
  A(78) = cont_SS(wf(:,41),wf(:,103)) * den(42)
  A(79) = cont_VV(wf(:,43),wf(:,104)) * den(44)
  A(80) = cont_VV(wf(:,45),wf(:,105)) * den(46)
  A(81) = cont_VV(wf(:,72),wf(:,78)) * den(111)
  A(82) = cont_VV(wf(:,72),wf(:,79)) * den(112)
  A(83) = cont_QA(wf(:,48),wf(:,90)) * den(48)
  A(84) = cont_QA(wf(:,50),wf(:,90)) * den(50)
  A(85) = cont_QA(wf(:,52),wf(:,90)) * den(52)
  A(86) = cont_SS(wf(:,41),wf(:,106)) * den(54)
  A(87) = cont_VV(wf(:,43),wf(:,107)) * den(55)
  A(88) = cont_VV(wf(:,45),wf(:,108)) * den(56)
  A(89) = cont_VV(wf(:,73),wf(:,78)) * den(114)
  A(90) = cont_VV(wf(:,73),wf(:,79)) * den(115)
  A(91) = cont_QA(wf(:,57),wf(:,99)) * den(58)
  A(92) = cont_QA(wf(:,59),wf(:,99)) * den(60)
  A(93) = cont_QA(wf(:,61),wf(:,99)) * den(62)
  A(94) = cont_VV(wf(:,74),wf(:,80)) * den(117)
  A(95) = cont_VV(wf(:,74),wf(:,81)) * den(118)
  A(96) = cont_QA(wf(:,48),wf(:,91)) * den(63)
  A(97) = cont_QA(wf(:,50),wf(:,91)) * den(64)
  A(98) = cont_QA(wf(:,52),wf(:,91)) * den(65)
  A(99) = cont_VV(wf(:,75),wf(:,80)) * den(120)
  A(100) = cont_VV(wf(:,75),wf(:,81)) * den(121)
  A(101) = cont_QA(wf(:,57),wf(:,100)) * den(66)
  A(102) = cont_QA(wf(:,59),wf(:,100)) * den(67)
  A(103) = cont_QA(wf(:,61),wf(:,100)) * den(68)
  A(104) = cont_QA(wf(:,57),wf(:,101)) * den(69)
  A(105) = cont_QA(wf(:,59),wf(:,101)) * den(70)
  A(106) = cont_QA(wf(:,61),wf(:,101)) * den(71)
  A(107) = cont_QA(wf(:,47),wf(:,93)) * den(72)
  A(108) = cont_QA(wf(:,49),wf(:,93)) * den(73)
  A(109) = cont_QA(wf(:,51),wf(:,93)) * den(74)
  A(110) = cont_QA(wf(:,38),wf(:,96)) * den(75)
  A(111) = cont_QA(wf(:,33),wf(:,98)) * den(76)
  A(112) = cont_QA(wf(:,86),wf(:,109)) * den(122)
  A(113) = cont_QA(wf(:,110),wf(:,111)) * den(124)
  A(114) = cont_QA(wf(:,9),wf(:,112)) * den(125)
  A(115) = cont_QA(wf(:,110),wf(:,113)) * den(127)
  A(116) = cont_QA(wf(:,40),wf(:,110)) * den(39)
  A(117) = cont_QA(wf(:,16),wf(:,112)) * den(128)
  A(118) = cont_QA(wf(:,23),wf(:,109)) * den(129)
  A(119) = cont_QA(wf(:,31),wf(:,114)) * den(130)
  A(120) = cont_QA(wf(:,31),wf(:,117)) * den(33)
  A(121) = cont_QA(wf(:,7),wf(:,119)) * den(36)
  A(122) = cont_QA(wf(:,31),wf(:,122)) * den(29)
  A(123) = cont_QA(wf(:,11),wf(:,124)) * den(31)
  A(124) = cont_QA(wf(:,95),wf(:,125)) * den(131)
  A(125) = cont_QA(wf(:,95),wf(:,126)) * den(132)
  A(126) = cont_QA(wf(:,95),wf(:,127)) * den(133)
  A(127) = cont_SS(wf(:,41),wf(:,128)) * den(42)
  A(128) = cont_VV(wf(:,43),wf(:,129)) * den(44)
  A(129) = cont_VV(wf(:,45),wf(:,130)) * den(46)
  A(130) = cont_QA(wf(:,97),wf(:,125)) * den(134)
  A(131) = cont_QA(wf(:,97),wf(:,126)) * den(135)
  A(132) = cont_QA(wf(:,97),wf(:,127)) * den(136)
  A(133) = cont_QA(wf(:,57),wf(:,114)) * den(69)
  A(134) = cont_QA(wf(:,59),wf(:,114)) * den(70)
  A(135) = cont_QA(wf(:,61),wf(:,114)) * den(71)
  A(136) = cont_QA(wf(:,26),wf(:,125)) * den(72)
  A(137) = cont_QA(wf(:,26),wf(:,126)) * den(73)
  A(138) = cont_QA(wf(:,26),wf(:,127)) * den(74)
  A(139) = cont_QA(wf(:,57),wf(:,117)) * den(66)
  A(140) = cont_QA(wf(:,59),wf(:,117)) * den(67)
  A(141) = cont_QA(wf(:,61),wf(:,117)) * den(68)
  A(142) = cont_SS(wf(:,41),wf(:,131)) * den(54)
  A(143) = cont_VV(wf(:,43),wf(:,132)) * den(55)
  A(144) = cont_VV(wf(:,45),wf(:,133)) * den(56)
  A(145) = cont_QA(wf(:,57),wf(:,122)) * den(58)
  A(146) = cont_QA(wf(:,59),wf(:,122)) * den(60)
  A(147) = cont_QA(wf(:,61),wf(:,122)) * den(62)
  A(148) = cont_QA(wf(:,15),wf(:,119)) * den(75)
  A(149) = cont_QA(wf(:,22),wf(:,124)) * den(76)
  A(150) = cont_VV(wf(:,3),wf(:,136)) * den(5)
  A(151) = cont_QA(wf(:,9),wf(:,137)) * den(8)
  A(152) = cont_QA(wf(:,11),wf(:,139)) * den(11)
  A(153) = cont_QA(wf(:,16),wf(:,137)) * den(14)
  A(154) = cont_QA(wf(:,18),wf(:,141)) * den(17)
  A(155) = cont_QA(wf(:,23),wf(:,138)) * den(21)
  A(156) = cont_QA(wf(:,24),wf(:,141)) * den(22)
  A(157) = cont_QA(wf(:,26),wf(:,140)) * den(25)
  A(158) = cont_QA(wf(:,19),wf(:,142)) * den(137)
  A(159) = cont_QA(wf(:,10),wf(:,146)) * den(21)
  A(160) = cont_QA(wf(:,19),wf(:,147)) * den(22)
  A(161) = cont_QA(wf(:,8),wf(:,151)) * den(14)
  A(162) = cont_QA(wf(:,19),wf(:,152)) * den(17)
  A(163) = cont_VV(wf(:,3),wf(:,155)) * den(5)
  A(164) = cont_QA(wf(:,8),wf(:,157)) * den(8)
  A(165) = cont_QA(wf(:,12),wf(:,158)) * den(11)
  A(166) = cont_QA(wf(:,111),wf(:,159)) * den(138)
  A(167) = cont_QA(wf(:,111),wf(:,160)) * den(139)
  A(168) = cont_QA(wf(:,111),wf(:,161)) * den(140)
  A(169) = cont_SS(wf(:,41),wf(:,162)) * den(54)
  A(170) = cont_VV(wf(:,43),wf(:,163)) * den(55)
  A(171) = cont_VV(wf(:,45),wf(:,164)) * den(56)
  A(172) = cont_QA(wf(:,113),wf(:,159)) * den(141)
  A(173) = cont_QA(wf(:,113),wf(:,160)) * den(142)
  A(174) = cont_QA(wf(:,113),wf(:,161)) * den(143)
  A(175) = cont_QA(wf(:,40),wf(:,159)) * den(144)
  A(176) = cont_QA(wf(:,40),wf(:,160)) * den(145)
  A(177) = cont_QA(wf(:,40),wf(:,161)) * den(146)
  A(178) = cont_QA(wf(:,48),wf(:,142)) * den(147)
  A(179) = cont_QA(wf(:,50),wf(:,142)) * den(148)
  A(180) = cont_QA(wf(:,52),wf(:,142)) * den(149)
  A(181) = cont_QA(wf(:,48),wf(:,147)) * den(63)
  A(182) = cont_QA(wf(:,50),wf(:,147)) * den(64)
  A(183) = cont_QA(wf(:,52),wf(:,147)) * den(65)
  A(184) = cont_SS(wf(:,41),wf(:,165)) * den(42)
  A(185) = cont_VV(wf(:,43),wf(:,166)) * den(44)
  A(186) = cont_VV(wf(:,45),wf(:,167)) * den(46)
  A(187) = cont_QA(wf(:,48),wf(:,152)) * den(48)
  A(188) = cont_QA(wf(:,50),wf(:,152)) * den(50)
  A(189) = cont_QA(wf(:,52),wf(:,152)) * den(52)
  A(190) = cont_QA(wf(:,33),wf(:,145)) * den(76)
  A(191) = cont_QA(wf(:,38),wf(:,150)) * den(75)
  A(192) = cont_QA(wf(:,30),wf(:,169)) * den(29)
  A(193) = cont_QA(wf(:,33),wf(:,158)) * den(31)
  A(194) = cont_QA(wf(:,36),wf(:,169)) * den(33)
  A(195) = cont_QA(wf(:,38),wf(:,156)) * den(36)
  A(196) = cont_QA(wf(:,40),wf(:,168)) * den(39)
  A(197) = cont_QA(wf(:,171),wf(:,172)) * den(153)
  A(198) = cont_QA(wf(:,174),wf(:,175)) * den(157)
  A(199) = cont_VV(wf(:,6),wf(:,176)) * den(160)
  A(200) = cont_QA(wf(:,88),wf(:,178)) * den(162)
  A(201) = cont_QA(wf(:,12),wf(:,179)) * den(163)
  A(202) = cont_QA(wf(:,86),wf(:,181)) * den(165)
  A(203) = cont_QA(wf(:,9),wf(:,182)) * den(166)
  A(204) = cont_QA(wf(:,88),wf(:,183)) * den(167)
  A(205) = cont_QA(wf(:,86),wf(:,184)) * den(168)
  A(206) = cont_QA(wf(:,88),wf(:,187)) * den(170)
  A(207) = cont_QA(wf(:,19),wf(:,188)) * den(171)
  A(208) = cont_QA(wf(:,95),wf(:,189)) * den(172)
  A(209) = cont_QA(wf(:,16),wf(:,182)) * den(173)
  A(210) = cont_QA(wf(:,88),wf(:,190)) * den(174)
  A(211) = cont_QA(wf(:,95),wf(:,191)) * den(175)
  A(212) = cont_QA(wf(:,12),wf(:,194)) * den(177)
  A(213) = cont_QA(wf(:,19),wf(:,195)) * den(178)
  A(214) = cont_QA(wf(:,97),wf(:,189)) * den(179)
  A(215) = cont_QA(wf(:,23),wf(:,181)) * den(180)
  A(216) = cont_QA(wf(:,12),wf(:,196)) * den(181)
  A(217) = cont_QA(wf(:,97),wf(:,191)) * den(182)
  A(218) = cont_QA(wf(:,19),wf(:,197)) * den(183)
  A(219) = cont_QA(wf(:,19),wf(:,198)) * den(185)
  A(220) = cont_QA(wf(:,26),wf(:,189)) * den(186)
  A(221) = cont_QA(wf(:,111),wf(:,199)) * den(187)
  A(222) = cont_QA(wf(:,33),wf(:,179)) * den(188)
  A(223) = cont_QA(wf(:,86),wf(:,202)) * den(190)
  A(224) = cont_QA(wf(:,31),wf(:,203)) * den(191)
  A(225) = cont_QA(wf(:,111),wf(:,204)) * den(192)
  A(226) = cont_QA(wf(:,86),wf(:,205)) * den(193)
  A(227) = cont_QA(wf(:,113),wf(:,199)) * den(194)
  A(228) = cont_QA(wf(:,38),wf(:,178)) * den(195)
  A(229) = cont_QA(wf(:,9),wf(:,208)) * den(197)
  A(230) = cont_QA(wf(:,31),wf(:,209)) * den(198)
  A(231) = cont_QA(wf(:,113),wf(:,204)) * den(199)
  A(232) = cont_QA(wf(:,9),wf(:,210)) * den(200)
  A(233) = cont_QA(wf(:,40),wf(:,199)) * den(201)
  A(234) = cont_QA(wf(:,31),wf(:,211)) * den(202)
  A(235) = cont_QA(wf(:,31),wf(:,212)) * den(203)
  A(236) = cont_QA(wf(:,185),wf(:,214)) * den(207)
  A(237) = cont_QA(wf(:,185),wf(:,216)) * den(210)
  A(238) = cont_QA(wf(:,185),wf(:,218)) * den(213)
  A(239) = cont_QA(wf(:,206),wf(:,220)) * den(217)
  A(240) = cont_QA(wf(:,206),wf(:,222)) * den(220)
  A(241) = cont_QA(wf(:,206),wf(:,224)) * den(223)
  A(242) = cont_QA(wf(:,48),wf(:,188)) * den(224)
  A(243) = cont_QA(wf(:,50),wf(:,188)) * den(225)
  A(244) = cont_QA(wf(:,52),wf(:,188)) * den(226)
  A(245) = cont_QA(wf(:,95),wf(:,225)) * den(227)
  A(246) = cont_QA(wf(:,95),wf(:,226)) * den(228)
  A(247) = cont_QA(wf(:,95),wf(:,227)) * den(229)
  A(248) = cont_QA(wf(:,192),wf(:,229)) * den(233)
  A(249) = cont_QA(wf(:,192),wf(:,231)) * den(236)
  A(250) = cont_QA(wf(:,192),wf(:,233)) * den(239)
  A(251) = cont_QA(wf(:,200),wf(:,235)) * den(243)
  A(252) = cont_QA(wf(:,200),wf(:,237)) * den(246)
  A(253) = cont_QA(wf(:,200),wf(:,239)) * den(249)
  A(254) = cont_QA(wf(:,111),wf(:,240)) * den(250)
  A(255) = cont_QA(wf(:,111),wf(:,241)) * den(251)
  A(256) = cont_QA(wf(:,111),wf(:,242)) * den(252)
  A(257) = cont_QA(wf(:,57),wf(:,203)) * den(253)
  A(258) = cont_QA(wf(:,59),wf(:,203)) * den(254)
  A(259) = cont_QA(wf(:,61),wf(:,203)) * den(255)
  A(260) = cont_QA(wf(:,48),wf(:,195)) * den(256)
  A(261) = cont_QA(wf(:,50),wf(:,195)) * den(257)
  A(262) = cont_QA(wf(:,52),wf(:,195)) * den(258)
  A(263) = cont_QA(wf(:,97),wf(:,225)) * den(259)
  A(264) = cont_QA(wf(:,97),wf(:,226)) * den(260)
  A(265) = cont_QA(wf(:,97),wf(:,227)) * den(261)
  A(266) = cont_QA(wf(:,113),wf(:,240)) * den(262)
  A(267) = cont_QA(wf(:,113),wf(:,241)) * den(263)
  A(268) = cont_QA(wf(:,113),wf(:,242)) * den(264)
  A(269) = cont_QA(wf(:,57),wf(:,209)) * den(265)
  A(270) = cont_QA(wf(:,59),wf(:,209)) * den(266)
  A(271) = cont_QA(wf(:,61),wf(:,209)) * den(267)
  A(272) = cont_QA(wf(:,40),wf(:,240)) * den(268)
  A(273) = cont_QA(wf(:,40),wf(:,241)) * den(269)
  A(274) = cont_QA(wf(:,40),wf(:,242)) * den(270)
  A(275) = cont_QA(wf(:,48),wf(:,197)) * den(271)
  A(276) = cont_QA(wf(:,50),wf(:,197)) * den(272)
  A(277) = cont_QA(wf(:,52),wf(:,197)) * den(273)
  A(278) = cont_QA(wf(:,48),wf(:,198)) * den(274)
  A(279) = cont_QA(wf(:,50),wf(:,198)) * den(275)
  A(280) = cont_QA(wf(:,52),wf(:,198)) * den(276)
  A(281) = cont_QA(wf(:,57),wf(:,212)) * den(277)
  A(282) = cont_QA(wf(:,59),wf(:,212)) * den(278)
  A(283) = cont_QA(wf(:,61),wf(:,212)) * den(279)
  A(284) = cont_QA(wf(:,38),wf(:,187)) * den(280)
  A(285) = cont_QA(wf(:,16),wf(:,208)) * den(281)
  A(286) = cont_QA(wf(:,16),wf(:,210)) * den(282)
  A(287) = cont_QA(wf(:,33),wf(:,194)) * den(283)
  A(288) = cont_QA(wf(:,23),wf(:,202)) * den(284)
  A(289) = cont_QA(wf(:,23),wf(:,205)) * den(285)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(289)
  complex(REALKIND), intent(out) :: M1(2), M2(3)

  M1(1) = (-A(21)-A(24)-A(27))*f(1)+CI*(-A(33)-A(36))*f(2)+(A(3)+A(6)+A(7)+A(9)+A(10)+A(39))*f(13)+CI*(A(1)+A(8)+A(13))*f(14)+( &
       -A(20)-A(23)-A(26))*f(24)+CI*(-A(32)-A(35))*f(25)+(A(22)+A(25)+A(28))*f(33)+CI*(A(34)+A(37))*f(34)
  M1(2) = (-A(15)-A(18)-A(30))*f(1)+CI*(A(33)+A(36))*f(2)+(A(2)+A(4)+A(5)+A(11)+A(12)+A(38))*f(13)+CI*(-A(1)-A(8)-A(13))*f(14)+( &
       -A(14)-A(17)-A(29))*f(24)+CI*(A(32)+A(35))*f(25)+(A(16)+A(19)+A(31))*f(33)+CI*(-A(34)-A(37))*f(34)

  M2(1) = (A(249)+A(252)+A(255)+A(258)+A(261)+A(264))*f(3)-(A(40)*f(4))/2._/**/REALKIND+CI*(A(273)+A(276)+A(279)+A(282))*f(5)+( &
       -A(92)-A(97)-A(143)-A(146)-A(170)-A(182))*f(6)+CI*(-A(134)-A(179))*f(7)+(-A(87)-A(131)-A(167))*f(8)+CI*(-A(137) &
       -A(176))*f(9)+CI*(-A(105)-A(108))*f(10)+(A(48)+A(49))*f(11)+CI*(A(44)+A(45)-A(46))*f(12)+(-A(201)-A(202)-A(205)-A(212) &
       -A(213)-A(214)-A(215)-A(216)-A(217)-A(221)-A(222)-A(223)-A(224)-A(225)-A(226)-A(287)-A(288)-A(289))*f(15)+CI*(-A(197) &
       -A(198)-A(199)-A(218)-A(219)-A(220)-A(233)-A(234)-A(235))*f(16)+(A(66)+A(73)+A(122)+A(123)+A(149)+A(159)+A(160) &
       +A(190))*f(17)+CI*(A(119)+A(158))*f(18)+(A(59)+A(60)+A(65)+A(74))*f(19)+CI*A(57)*f(20)+(A(70)+A(71)+A(111)+A(112)+A(113) &
       +A(118)+A(152)+A(155)+A(156)+A(165)+A(192)+A(193))*f(21)+CI*(A(72)+A(116)+A(150)+A(157)+A(163)+A(196))*f(22)+CI*(A(62) &
       +A(67)+A(77))*f(23)+(A(248)+A(251)+A(254)+A(257)+A(260)+A(263))*f(26)+CI*(A(272)+A(275)+A(278)+A(281))*f(27)+(-A(91)-A(96) &
       -A(142)-A(145)-A(169)-A(181))*f(28)+CI*(-A(133)-A(178))*f(29)+(-A(86)-A(130)-A(166))*f(30)+CI*(-A(136)-A(175))*f(31)+CI*( &
       -A(104)-A(107))*f(32)+(A(42)*f(35))/2._/**/REALKIND+(-A(250)-A(253)-A(256)-A(259)-A(262)-A(265))*f(35)+CI*(-A(274)-A(277) &
       -A(280)-A(283))*f(36)+(A(93)+A(98)+A(144)+A(147)+A(171)+A(183))*f(37)+CI*(A(135)+A(180))*f(38)+(A(88)+A(132)+A(168))*f(39) &
       +CI*(A(138)+A(177))*f(40)+CI*(A(106)+A(109))*f(41)+CI*(A(52)+A(54)-A(56))*f(42)+(-A(90)-A(95))*f(43)+(-A(89)-A(94))*f(44) &
       +CI*(-A(51)-A(53)+A(55))*f(45)
  M2(2) = (A(237)+A(240)+A(243)+A(246)+A(267)+A(270))*f(3)-(A(41)*f(4))/2._/**/REALKIND+CI*(-A(273)-A(276)-A(279)-A(282))*f(5)+( &
       -A(84)-A(102)-A(128)-A(140)-A(185)-A(188))*f(6)+CI*(A(134)+A(179))*f(7)+(-A(79)-A(125)-A(173))*f(8)+CI*(A(137)+A(176))*f(9) &
       +CI*(A(105)+A(108))*f(10)+(A(47)+A(50))*f(11)+CI*(-A(44)-A(45)+A(46))*f(12)+(-A(200)-A(203)-A(204)-A(206)-A(207)-A(208) &
       -A(209)-A(210)-A(211)-A(227)-A(228)-A(229)-A(230)-A(231)-A(232)-A(284)-A(285)-A(286))*f(15)+CI*(A(197)+A(198)+A(199)+A(218) &
       +A(219)+A(220)+A(233)+A(234)+A(235))*f(16)+(A(64)+A(75)+A(120)+A(121)+A(148)+A(161)+A(162)+A(191))*f(17)+CI*(-A(119) &
       -A(158))*f(18)+(A(58)+A(61)+A(63)+A(76))*f(19)-CI*A(57)*f(20)+(A(68)+A(69)+A(110)+A(114)+A(115)+A(117)+A(151)+A(153)+A(154) &
       +A(164)+A(194)+A(195))*f(21)+CI*(-A(72)-A(116)-A(150)-A(157)-A(163)-A(196))*f(22)+CI*(-A(62)-A(67)-A(77))*f(23)+(A(236) &
       +A(239)+A(242)+A(245)+A(266)+A(269))*f(26)+CI*(-A(272)-A(275)-A(278)-A(281))*f(27)+(-A(83)-A(101)-A(127)-A(139)-A(184) &
       -A(187))*f(28)+CI*(A(133)+A(178))*f(29)+(-A(78)-A(124)-A(172))*f(30)+CI*(A(136)+A(175))*f(31)+CI*(A(104)+A(107))*f(32) &
       +(A(43)*f(35))/2._/**/REALKIND+(-A(238)-A(241)-A(244)-A(247)-A(268)-A(271))*f(35)+CI*(A(274)+A(277)+A(280)+A(283))*f(36) &
       +(A(85)+A(103)+A(129)+A(141)+A(186)+A(189))*f(37)+CI*(-A(135)-A(180))*f(38)+(A(80)+A(126)+A(174))*f(39)+CI*(-A(138) &
       -A(177))*f(40)+CI*(-A(106)-A(109))*f(41)+CI*(-A(52)-A(54)+A(56))*f(42)+(-A(82)-A(100))*f(43)+(-A(81)-A(99))*f(44)+CI*(A(51) &
       +A(53)-A(55))*f(45)
  M2(3) = ((A(40)+A(41))*f(4))/6._/**/REALKIND+((-A(42)-A(43))*f(35))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppwwjj_bbxwwxgg_1_/**/REALKIND
