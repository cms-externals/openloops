
module ol_colourmatrix_ppwwjj_bbbxbxwwx_1_/**/REALKIND
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
end module ol_colourmatrix_ppwwjj_bbbxbxwwx_1_/**/REALKIND



module ol_forced_parameters_ppwwjj_bbbxbxwwx_1_/**/REALKIND
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
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppwwjj_bbbxbxwwx_1_/**/REALKIND

module ol_loop_ppwwjj_bbbxbxwwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(42), c(72)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:243)
  ! denominators
  complex(REALKIND), save :: den(343)
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
    f( 4) = (CI*countertermnorm*ctVbb*eQED**2*gQCD**4)/3._/**/REALKIND
    f( 5) = CI*countertermnorm*ctWWGG*eQED**2*gQCD**4
    f( 6) = (CI*eQED**2*gQCD**2)/(2._/**/REALKIND*sw**2)
    f( 7) = (CI*countertermnorm*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f( 8) = (CI*countertermnorm*ctGbb*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f( 9) = (CI*countertermnorm*ctGtt*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(10) = (CI*countertermnorm*ctVbt*eQED**2*gQCD**4)/(2._/**/REALKIND*sw**2)
    f(11) = (CI*eQED**2*gQCD**2*MB)/(2._/**/REALKIND*sw**2)
    f(12) = (CI*countertermnorm*eQED**2*gQCD**4*MB)/(2._/**/REALKIND*sw**2)
    f(13) = (CI*countertermnorm*ctGbb*eQED**2*gQCD**4*MB)/(2._/**/REALKIND*sw**2)
    f(14) = (CI*countertermnorm*ctSbb*eQED**2*gQCD**4*MB)/(2._/**/REALKIND*sw**2)
    f(15) = (CI*cw*eQED**2*gQCD**2)/sw
    f(16) = (CI*countertermnorm*cw*eQED**2*gQCD**4)/sw
    f(17) = (CI*countertermnorm*ctGbb*cw*eQED**2*gQCD**4)/sw
    f(18) = (CI*countertermnorm*ctVbb*cw*eQED**2*gQCD**4)/sw
    f(19) = (countertermnorm*ctZGG*cw*eQED**2*gQCD**4)/sw
    f(20) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MW)/sw
    f(21) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(22) = (eQED**2*gQCD**4*integralnorm*SwB)/6._/**/REALKIND
    f(23) = (eQED**2*gQCD**4*integralnorm*SwB)/3._/**/REALKIND
    f(24) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(25) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*4._/**/REALKIND)
    f(26) = (eQED**2*gQCD**4*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(27) = (CI*eQED**2*gQCD**4*integralnorm*MB*SwB)/(2._/**/REALKIND*sw**2)
    f(28) = (eQED**2*gQCD**4*integralnorm*MB*SwB)/(sw**2*4._/**/REALKIND)
    f(29) = (eQED**2*gQCD**4*integralnorm*MB*SwB)/(sw**2*2._/**/REALKIND)
    f(30) = (CI*cw*eQED**2*gQCD**4*integralnorm*SwB)/sw
    f(31) = (cw*eQED**2*gQCD**4*integralnorm*SwB)/(sw*2._/**/REALKIND)
    f(32) = (cw*eQED**2*gQCD**4*integralnorm*SwB)/sw
    f(33) = (eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(34) = (2*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(35) = (4*eQED**2*gQCD**4*integralnorm*SwF)/3._/**/REALKIND
    f(36) = (eQED**2*gQCD**4*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(37) = (eQED**2*gQCD**4*integralnorm*SwF)/sw**2
    f(38) = (eQED**2*gQCD**4*integralnorm*MB*SwF)/(sw**2*2._/**/REALKIND)
    f(39) = (eQED**2*gQCD**4*integralnorm*MB*SwF)/sw**2
    f(40) = (eQED**2*gQCD**4*integralnorm*MT*SwF)/(sw**2*2._/**/REALKIND)
    f(41) = (cw*eQED**2*gQCD**4*integralnorm*SwF)/sw
    f(42) = (2*cw*eQED**2*gQCD**4*integralnorm*SwF)/sw

  c = [ 9*CI*f(21), 27*CI*f(21), 18*f(22), 54*f(22), f(23), 3*f(23), 6*f(23), 8*f(23), 10*f(23), 18*f(23), 21*f(23), 24*f(23) &
    , 54*f(23), 9*CI*f(24), 27*CI*f(24), 18*f(25), 54*f(25), f(26), 3*f(26), 6*f(26), 8*f(26), 10*f(26), 18*f(26), 21*f(26) &
    , 24*f(26), 54*f(26), 9*CI*f(27), 27*CI*f(27), 18*f(28), 54*f(28), f(29), 3*f(29), 6*f(29), 8*f(29), 10*f(29), 18*f(29) &
    , 21*f(29), 24*f(29), 54*f(29), 9*CI*f(30), 27*CI*f(30), 18*f(31), 54*f(31), f(32), 3*f(32), 6*f(32), 8*f(32), 10*f(32) &
    , 18*f(32), 21*f(32), 24*f(32), 54*f(32), 3*f(33), 9*f(33), 3*f(34), 9*f(34), 3*f(35), 9*f(35), 3*f(36), 9*f(36), 3*f(37) &
    , 9*f(37), 3*f(38), 9*f(38), 3*f(39), 9*f(39), 3*f(40), 9*f(40), 3*f(41), 9*f(41), 3*f(42), 9*f(42) ]
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
  complex(REALKIND) :: A(246)
  ! external WFs
  call wf_Q(P(:,1), rMB, H(1), wf(:,0))
  call wf_Q(P(:,2), rMB, H(2), wf(:,-1))
  call wf_A(P(:,3), rMB, H(3), wf(:,-2))
  call wf_A(P(:,4), rMB, H(4), wf(:,-3))
  call wf_V(P(:,5), rMW, H(5), wf(:,-4))
  call wf_V(P(:,6), rMW, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,1))
  call vert_WQ_A(wf(:,-5),wf(:,-1),wf(:,2))
  call vert_AW_Q(wf(:,-3),wf(:,-4),wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,34),MT,1_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,24),MT,1_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,6))
  call vert_VQ_A(wf(:,1),wf(:,-1),wf(:,7))
  call vert_AW_Q(wf(:,5),wf(:,-5),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,7),MB,1_intkind1,wf(:,9))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,10))
  call vert_WQ_A(wf(:,-4),wf(:,4),wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,13),MB,1_intkind1,wf(:,12))
  call vert_VV_S(wf(:,-4),wf(:,-5),wf(:,13))
  call vert_SA_Q(gH,wf(:,13),wf(:,-3),wf(:,14))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-4),Q(:,16),wf(:,15))
  call vert_AV_Q(wf(:,-3),wf(:,15),wf(:,16))
  call prop_W_W(wf(:,15),Q(:,48),MZ,1_intkind1,wf(:,17))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,17),wf(:,18))
  call vert_QS_A(gH,wf(:,-1),wf(:,13),wf(:,19))
  call prop_Q_A(wf(:,19),Q(:,50),MB,1_intkind1,wf(:,20))
  call vert_VQ_A(wf(:,15),wf(:,-1),wf(:,21))
  call prop_Q_A(wf(:,21),Q(:,50),MB,1_intkind1,wf(:,22))
  call vert_ZQ_A(gZd,wf(:,17),wf(:,-1),wf(:,23))
  call prop_Q_A(wf(:,23),Q(:,50),MB,1_intkind1,wf(:,24))
  call vert_WQ_A(wf(:,-5),wf(:,0),wf(:,25))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,26))
  call prop_Q_A(wf(:,25),Q(:,33),MT,1_intkind1,wf(:,27))
  call vert_VQ_A(wf(:,26),wf(:,27),wf(:,28))
  call vert_VQ_A(wf(:,26),wf(:,0),wf(:,29))
  call prop_Q_A(wf(:,29),Q(:,7),MB,1_intkind1,wf(:,30))
  call vert_AV_Q(wf(:,-3),wf(:,26),wf(:,31))
  call vert_WQ_A(wf(:,-4),wf(:,27),wf(:,32))
  call prop_A_Q(wf(:,31),Q(:,14),MB,1_intkind1,wf(:,33))
  call vert_QS_A(gH,wf(:,0),wf(:,13),wf(:,34))
  call prop_Q_A(wf(:,34),Q(:,49),MB,1_intkind1,wf(:,35))
  call vert_VQ_A(wf(:,15),wf(:,0),wf(:,36))
  call prop_Q_A(wf(:,36),Q(:,49),MB,1_intkind1,wf(:,37))
  call vert_ZQ_A(gZd,wf(:,17),wf(:,0),wf(:,38))
  call prop_Q_A(wf(:,38),Q(:,49),MB,1_intkind1,wf(:,39))
  call vert_QA_V(wf(:,0),wf(:,-3),wf(:,40))
  call vert_AW_Q(wf(:,-2),wf(:,-4),wf(:,41))
  call prop_A_Q(wf(:,41),Q(:,20),MT,1_intkind1,wf(:,42))
  call vert_VQ_A(wf(:,40),wf(:,4),wf(:,43))
  call vert_VQ_A(wf(:,40),wf(:,-1),wf(:,44))
  call vert_AW_Q(wf(:,42),wf(:,-5),wf(:,45))
  call prop_Q_A(wf(:,44),Q(:,11),MB,1_intkind1,wf(:,46))
  call vert_AV_Q(wf(:,-2),wf(:,40),wf(:,47))
  call prop_A_Q(wf(:,47),Q(:,13),MB,1_intkind1,wf(:,48))
  call vert_SA_Q(gH,wf(:,13),wf(:,-2),wf(:,49))
  call vert_AV_Q(wf(:,-2),wf(:,15),wf(:,50))
  call vert_AZ_Q(gZd,wf(:,-2),wf(:,17),wf(:,51))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,52))
  call vert_VQ_A(wf(:,52),wf(:,27),wf(:,53))
  call vert_VQ_A(wf(:,52),wf(:,0),wf(:,54))
  call prop_Q_A(wf(:,54),Q(:,11),MB,1_intkind1,wf(:,55))
  call vert_AV_Q(wf(:,-2),wf(:,52),wf(:,56))
  call prop_A_Q(wf(:,56),Q(:,14),MB,1_intkind1,wf(:,57))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,1),wf(:,58))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,40),wf(:,59))
  call counter_GG_S(wf(:,1),wf(:,52),wf(:,60))
  call counter_GG_V(wf(:,1),Q(:,5),wf(:,52),Q(:,10),wf(:,61))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,62))
  call counter_AW_Q(wf(:,5),wf(:,-5),wf(:,63))
  call counter_WQ_A(wf(:,-4),wf(:,4),wf(:,64))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,65))
  call prop_Q_A(wf(:,11),Q(:,50),MB,1_intkind1,wf(:,66))
  call counter_SA_Q(gH,wf(:,13),wf(:,-3),wf(:,67))
  call counter_AV_Q(wf(:,-3),wf(:,15),wf(:,68))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,17),wf(:,69))
  call counter_AW_Q(wf(:,-3),wf(:,-4),wf(:,70))
  call prop_A_Q(wf(:,70),Q(:,24),MT,1_intkind1,wf(:,71))
  call vert_AW_Q(wf(:,71),wf(:,-5),wf(:,72))
  call counter_GG_S(wf(:,26),wf(:,40),wf(:,73))
  call counter_GG_V(wf(:,26),Q(:,6),wf(:,40),Q(:,9),wf(:,74))
  call counter_VQ_A(wf(:,26),wf(:,27),wf(:,75))
  call counter_WQ_A(wf(:,-4),wf(:,27),wf(:,76))
  call counter_AV_Q(wf(:,-3),wf(:,26),wf(:,77))
  call prop_Q_A(wf(:,32),Q(:,49),MB,1_intkind1,wf(:,78))
  call counter_VQ_A(wf(:,40),wf(:,4),wf(:,79))
  call counter_AW_Q(wf(:,42),wf(:,-5),wf(:,80))
  call counter_VQ_A(wf(:,52),wf(:,27),wf(:,81))
  call counter_AV_Q(wf(:,-2),wf(:,40),wf(:,82))
  call counter_SA_Q(gH,wf(:,13),wf(:,-2),wf(:,83))
  call counter_AV_Q(wf(:,-2),wf(:,15),wf(:,84))
  call counter_AZ_Q(gZd,wf(:,-2),wf(:,17),wf(:,85))
  call counter_AW_Q(wf(:,-2),wf(:,-4),wf(:,86))
  call prop_A_Q(wf(:,86),Q(:,20),MT,1_intkind1,wf(:,87))
  call vert_AW_Q(wf(:,87),wf(:,-5),wf(:,88))
  call counter_AV_Q(wf(:,-2),wf(:,52),wf(:,89))
  call counter_VQ_A(wf(:,1),wf(:,-1),wf(:,90))
  call prop_A_Q(wf(:,8),Q(:,56),MB,1_intkind1,wf(:,91))
  call counter_WQ_A(wf(:,-5),wf(:,-1),wf(:,92))
  call prop_Q_A(wf(:,92),Q(:,34),MT,1_intkind1,wf(:,93))
  call vert_VQ_A(wf(:,1),wf(:,93),wf(:,94))
  call counter_QS_A(gH,wf(:,-1),wf(:,13),wf(:,95))
  call counter_VQ_A(wf(:,15),wf(:,-1),wf(:,96))
  call counter_ZQ_A(gZd,wf(:,17),wf(:,-1),wf(:,97))
  call prop_A_Q(wf(:,14),Q(:,56),MB,1_intkind1,wf(:,98))
  call prop_A_Q(wf(:,16),Q(:,56),MB,1_intkind1,wf(:,99))
  call prop_A_Q(wf(:,18),Q(:,56),MB,1_intkind1,wf(:,100))
  call vert_WQ_A(wf(:,-4),wf(:,93),wf(:,101))
  call counter_VQ_A(wf(:,40),wf(:,-1),wf(:,102))
  call prop_A_Q(wf(:,45),Q(:,52),MB,1_intkind1,wf(:,103))
  call vert_VQ_A(wf(:,40),wf(:,93),wf(:,104))
  call prop_A_Q(wf(:,49),Q(:,52),MB,1_intkind1,wf(:,105))
  call prop_A_Q(wf(:,50),Q(:,52),MB,1_intkind1,wf(:,106))
  call prop_A_Q(wf(:,51),Q(:,52),MB,1_intkind1,wf(:,107))
  call counter_QA_V(wf(:,-1),wf(:,-3),wf(:,108))
  call vert_VQ_A(wf(:,108),wf(:,27),wf(:,109))
  call vert_VQ_A(wf(:,108),wf(:,0),wf(:,110))
  call prop_Q_A(wf(:,110),Q(:,11),MB,1_intkind1,wf(:,111))
  call vert_AV_Q(wf(:,-2),wf(:,108),wf(:,112))
  call prop_A_Q(wf(:,112),Q(:,14),MB,1_intkind1,wf(:,113))
  call counter_QA_V(wf(:,-1),wf(:,-2),wf(:,114))
  call vert_VQ_A(wf(:,114),wf(:,27),wf(:,115))
  call vert_VQ_A(wf(:,114),wf(:,0),wf(:,116))
  call prop_Q_A(wf(:,116),Q(:,7),MB,1_intkind1,wf(:,117))
  call vert_AV_Q(wf(:,-3),wf(:,114),wf(:,118))
  call prop_A_Q(wf(:,118),Q(:,14),MB,1_intkind1,wf(:,119))
  call counter_VQ_A(wf(:,26),wf(:,0),wf(:,120))
  call counter_WQ_A(wf(:,-5),wf(:,0),wf(:,121))
  call prop_Q_A(wf(:,121),Q(:,33),MT,1_intkind1,wf(:,122))
  call vert_VQ_A(wf(:,26),wf(:,122),wf(:,123))
  call counter_QS_A(gH,wf(:,0),wf(:,13),wf(:,124))
  call counter_VQ_A(wf(:,15),wf(:,0),wf(:,125))
  call counter_ZQ_A(gZd,wf(:,17),wf(:,0),wf(:,126))
  call vert_WQ_A(wf(:,-4),wf(:,122),wf(:,127))
  call counter_VQ_A(wf(:,52),wf(:,0),wf(:,128))
  call vert_VQ_A(wf(:,52),wf(:,122),wf(:,129))
  call counter_QA_V(wf(:,0),wf(:,-3),wf(:,130))
  call vert_VQ_A(wf(:,130),wf(:,4),wf(:,131))
  call vert_VQ_A(wf(:,130),wf(:,-1),wf(:,132))
  call prop_Q_A(wf(:,132),Q(:,11),MB,1_intkind1,wf(:,133))
  call vert_AV_Q(wf(:,-2),wf(:,130),wf(:,134))
  call prop_A_Q(wf(:,134),Q(:,13),MB,1_intkind1,wf(:,135))
  call counter_QA_V(wf(:,0),wf(:,-2),wf(:,136))
  call vert_VQ_A(wf(:,136),wf(:,4),wf(:,137))
  call vert_VQ_A(wf(:,136),wf(:,-1),wf(:,138))
  call prop_Q_A(wf(:,138),Q(:,7),MB,1_intkind1,wf(:,139))
  call vert_AV_Q(wf(:,-3),wf(:,136),wf(:,140))
  call prop_A_Q(wf(:,140),Q(:,13),MB,1_intkind1,wf(:,141))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,142))
  call counter_V_V(ctGG,wf(:,1),Q(:,5),wf(:,143))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,144))
  call counter_Q_A(cttt,wf(:,4),Q(:,34),wf(:,145))
  call prop_A_Q(wf(:,144),Q(:,29),MT,1_intkind1,wf(:,146))
  call counter_A_Q(cttt,wf(:,5),Q(:,24),wf(:,147))
  call prop_Q_A(wf(:,6),Q(:,39),MT,1_intkind1,wf(:,148))
  call vert_VQ_A(wf(:,143),wf(:,-1),wf(:,149))
  call counter_Q_A(ctbb,wf(:,9),Q(:,7),wf(:,150))
  call prop_A_Q(wf(:,147),Q(:,24),MT,1_intkind1,wf(:,151))
  call vert_AW_Q(wf(:,151),wf(:,-5),wf(:,152))
  call vert_AV_Q(wf(:,-3),wf(:,143),wf(:,153))
  call prop_Q_A(wf(:,145),Q(:,34),MT,1_intkind1,wf(:,154))
  call vert_WQ_A(wf(:,-4),wf(:,154),wf(:,155))
  call counter_A_Q(ctbb,wf(:,12),Q(:,13),wf(:,156))
  call counter_Q_A(ctbb,wf(:,20),Q(:,50),wf(:,157))
  call counter_Q_A(ctbb,wf(:,22),Q(:,50),wf(:,158))
  call counter_Q_A(ctbb,wf(:,24),Q(:,50),wf(:,159))
  call vert_AV_Q(wf(:,5),wf(:,26),wf(:,160))
  call counter_Q_A(cttt,wf(:,27),Q(:,33),wf(:,161))
  call prop_A_Q(wf(:,160),Q(:,30),MT,1_intkind1,wf(:,162))
  call vert_QA_V(wf(:,27),wf(:,5),wf(:,163))
  call counter_V_V(ctGG,wf(:,26),Q(:,6),wf(:,164))
  call prop_Q_A(wf(:,28),Q(:,39),MT,1_intkind1,wf(:,165))
  call vert_VQ_A(wf(:,164),wf(:,0),wf(:,166))
  call counter_Q_A(ctbb,wf(:,30),Q(:,7),wf(:,167))
  call prop_Q_A(wf(:,161),Q(:,33),MT,1_intkind1,wf(:,168))
  call vert_WQ_A(wf(:,-4),wf(:,168),wf(:,169))
  call vert_AV_Q(wf(:,-3),wf(:,164),wf(:,170))
  call counter_A_Q(ctbb,wf(:,33),Q(:,14),wf(:,171))
  call counter_Q_A(ctbb,wf(:,35),Q(:,49),wf(:,172))
  call counter_Q_A(ctbb,wf(:,37),Q(:,49),wf(:,173))
  call counter_Q_A(ctbb,wf(:,39),Q(:,49),wf(:,174))
  call vert_QA_V(wf(:,4),wf(:,42),wf(:,175))
  call counter_V_V(ctGG,wf(:,40),Q(:,9),wf(:,176))
  call vert_AV_Q(wf(:,42),wf(:,40),wf(:,177))
  call prop_A_Q(wf(:,177),Q(:,29),MT,1_intkind1,wf(:,178))
  call counter_A_Q(cttt,wf(:,42),Q(:,20),wf(:,179))
  call prop_Q_A(wf(:,43),Q(:,43),MT,1_intkind1,wf(:,180))
  call vert_VQ_A(wf(:,176),wf(:,-1),wf(:,181))
  call counter_Q_A(ctbb,wf(:,46),Q(:,11),wf(:,182))
  call prop_A_Q(wf(:,179),Q(:,20),MT,1_intkind1,wf(:,183))
  call vert_AW_Q(wf(:,183),wf(:,-5),wf(:,184))
  call vert_AV_Q(wf(:,-2),wf(:,176),wf(:,185))
  call counter_A_Q(ctbb,wf(:,48),Q(:,13),wf(:,186))
  call vert_AV_Q(wf(:,42),wf(:,52),wf(:,187))
  call prop_A_Q(wf(:,187),Q(:,30),MT,1_intkind1,wf(:,188))
  call vert_QA_V(wf(:,27),wf(:,42),wf(:,189))
  call counter_V_V(ctGG,wf(:,52),Q(:,10),wf(:,190))
  call prop_Q_A(wf(:,53),Q(:,43),MT,1_intkind1,wf(:,191))
  call vert_VQ_A(wf(:,190),wf(:,0),wf(:,192))
  call counter_Q_A(ctbb,wf(:,55),Q(:,11),wf(:,193))
  call vert_AV_Q(wf(:,-2),wf(:,190),wf(:,194))
  call counter_A_Q(ctbb,wf(:,57),Q(:,14),wf(:,195))
  call vert_WQ_A(wf(:,-5),wf(:,9),wf(:,196))
  call prop_Q_A(wf(:,196),Q(:,39),MT,1_intkind1,wf(:,197))
  call vert_AW_Q(wf(:,12),wf(:,-4),wf(:,198))
  call prop_A_Q(wf(:,198),Q(:,29),MT,1_intkind1,wf(:,199))
  call vert_WQ_A(wf(:,-5),wf(:,30),wf(:,200))
  call prop_Q_A(wf(:,200),Q(:,39),MT,1_intkind1,wf(:,201))
  call vert_AW_Q(wf(:,33),wf(:,-4),wf(:,202))
  call prop_A_Q(wf(:,202),Q(:,30),MT,1_intkind1,wf(:,203))
  call vert_WQ_A(wf(:,-5),wf(:,46),wf(:,204))
  call prop_Q_A(wf(:,204),Q(:,43),MT,1_intkind1,wf(:,205))
  call vert_AW_Q(wf(:,48),wf(:,-4),wf(:,206))
  call prop_A_Q(wf(:,206),Q(:,29),MT,1_intkind1,wf(:,207))
  call vert_WQ_A(wf(:,-5),wf(:,55),wf(:,208))
  call prop_Q_A(wf(:,208),Q(:,43),MT,1_intkind1,wf(:,209))
  call vert_AW_Q(wf(:,57),wf(:,-4),wf(:,210))
  call prop_A_Q(wf(:,210),Q(:,30),MT,1_intkind1,wf(:,211))
  call vert_QA_V(wf(:,0),wf(:,103),wf(:,212))
  call vert_QA_V(wf(:,-1),wf(:,103),wf(:,213))
  call vert_QA_V(wf(:,0),wf(:,91),wf(:,214))
  call vert_QA_V(wf(:,-1),wf(:,91),wf(:,215))
  call vert_QA_V(wf(:,78),wf(:,-2),wf(:,216))
  call vert_QA_V(wf(:,78),wf(:,-3),wf(:,217))
  call vert_QA_V(wf(:,66),wf(:,-2),wf(:,218))
  call vert_QA_V(wf(:,66),wf(:,-3),wf(:,219))
  call vert_QA_V(wf(:,35),wf(:,-2),wf(:,220))
  call vert_QA_V(wf(:,37),wf(:,-2),wf(:,221))
  call vert_QA_V(wf(:,39),wf(:,-2),wf(:,222))
  call vert_QA_V(wf(:,0),wf(:,105),wf(:,223))
  call vert_QA_V(wf(:,0),wf(:,106),wf(:,224))
  call vert_QA_V(wf(:,0),wf(:,107),wf(:,225))
  call vert_QA_V(wf(:,35),wf(:,-3),wf(:,226))
  call vert_QA_V(wf(:,37),wf(:,-3),wf(:,227))
  call vert_QA_V(wf(:,39),wf(:,-3),wf(:,228))
  call vert_QA_V(wf(:,0),wf(:,98),wf(:,229))
  call vert_QA_V(wf(:,0),wf(:,99),wf(:,230))
  call vert_QA_V(wf(:,0),wf(:,100),wf(:,231))
  call vert_QA_V(wf(:,20),wf(:,-2),wf(:,232))
  call vert_QA_V(wf(:,22),wf(:,-2),wf(:,233))
  call vert_QA_V(wf(:,24),wf(:,-2),wf(:,234))
  call vert_QA_V(wf(:,-1),wf(:,105),wf(:,235))
  call vert_QA_V(wf(:,-1),wf(:,106),wf(:,236))
  call vert_QA_V(wf(:,-1),wf(:,107),wf(:,237))
  call vert_QA_V(wf(:,20),wf(:,-3),wf(:,238))
  call vert_QA_V(wf(:,22),wf(:,-3),wf(:,239))
  call vert_QA_V(wf(:,24),wf(:,-3),wf(:,240))
  call vert_QA_V(wf(:,-1),wf(:,98),wf(:,241))
  call vert_QA_V(wf(:,-1),wf(:,99),wf(:,242))
  call vert_QA_V(wf(:,-1),wf(:,100),wf(:,243))

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
  den(2) = 1 / (Q(5,34) - MT2)
  den(3) = 1 / (Q(5,24) - MT2)
  den(6) = 1 / (Q(5,7) - MB2)
  den(9) = 1 / (Q(5,13) - MB2)
  den(12) = 1 / (Q(5,48) - MH2)
  den(14) = 1 / (Q(5,48))
  den(16) = 1 / (Q(5,48) - MZ2)
  den(18) = 1 / (Q(5,50) - MB2)
  den(25) = 1 / (Q(5,33) - MT2)
  den(26) = 1 / (Q(5,6))
  den(31) = 1 / (Q(5,14) - MB2)
  den(37) = 1 / (Q(5,49) - MB2)
  den(44) = 1 / (Q(5,9))
  den(45) = 1 / (Q(5,20) - MT2)
  den(48) = 1 / (Q(5,11) - MB2)
  den(59) = 1 / (Q(5,10))
  den(84) = 1 / (Q(5,56) - MB2)
  den(96) = 1 / (Q(5,52) - MB2)
  den(123) = 1 / (Q(5,58))
  den(127) = 1 / (Q(5,29) - MT2)
  den(130) = 1 / (Q(5,39) - MT2)
  den(155) = 1 / (Q(5,30) - MT2)
  den(159) = 1 / (Q(5,57))
  den(185) = 1 / (Q(5,54))
  den(191) = 1 / (Q(5,43) - MT2)
  den(218) = 1 / (Q(5,53))

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
  den(27) = den(25)*den(26)
  den(28) = den(3)*den(27)
  den(29) = den(6)*den(26)
  den(30) = den(3)*den(29)
  den(32) = den(26)*den(31)
  den(33) = den(25)*den(32)
  den(34) = den(12)*den(29)
  den(35) = den(14)*den(29)
  den(36) = den(16)*den(29)
  den(38) = den(12)*den(37)
  den(39) = den(26)*den(38)
  den(40) = den(14)*den(37)
  den(41) = den(26)*den(40)
  den(42) = den(16)*den(37)
  den(43) = den(26)*den(42)
  den(46) = den(2)*den(44)
  den(47) = den(45)*den(46)
  den(49) = den(44)*den(48)
  den(50) = den(45)*den(49)
  den(51) = den(9)*den(44)
  den(52) = den(2)*den(51)
  den(53) = den(12)*den(49)
  den(54) = den(14)*den(49)
  den(55) = den(16)*den(49)
  den(56) = den(19)*den(44)
  den(57) = den(21)*den(44)
  den(58) = den(23)*den(44)
  den(60) = den(25)*den(59)
  den(61) = den(45)*den(60)
  den(62) = den(48)*den(59)
  den(63) = den(45)*den(62)
  den(64) = den(31)*den(59)
  den(65) = den(25)*den(64)
  den(66) = den(12)*den(62)
  den(67) = den(14)*den(62)
  den(68) = den(16)*den(62)
  den(69) = den(38)*den(59)
  den(70) = den(40)*den(59)
  den(71) = den(42)*den(59)
  den(72) = den(1)*den(59)
  den(73) = den(26)*den(44)
  den(74) = den(12)*den(72)
  den(75) = den(16)*den(72)
  den(76) = den(2)*den(18)
  den(77) = den(1)*den(76)
  den(78) = den(12)*den(73)
  den(79) = den(16)*den(73)
  den(80) = den(25)*den(37)
  den(81) = den(26)*den(80)
  den(82) = den(44)*den(76)
  den(83) = den(59)*den(80)
  den(85) = den(3)*den(84)
  den(86) = den(1)*den(85)
  den(87) = den(10)*den(12)
  den(88) = den(10)*den(14)
  den(89) = den(10)*den(16)
  den(90) = den(12)*den(84)
  den(91) = den(1)*den(90)
  den(92) = den(14)*den(84)
  den(93) = den(1)*den(92)
  den(94) = den(16)*den(84)
  den(95) = den(1)*den(94)
  den(97) = den(45)*den(96)
  den(98) = den(44)*den(97)
  den(99) = den(12)*den(51)
  den(100) = den(14)*den(51)
  den(101) = den(16)*den(51)
  den(102) = den(12)*den(96)
  den(103) = den(44)*den(102)
  den(104) = den(14)*den(96)
  den(105) = den(44)*den(104)
  den(106) = den(16)*den(96)
  den(107) = den(44)*den(106)
  den(108) = den(26)*den(85)
  den(109) = den(26)*den(90)
  den(110) = den(26)*den(92)
  den(111) = den(26)*den(94)
  den(112) = den(12)*den(32)
  den(113) = den(14)*den(32)
  den(114) = den(16)*den(32)
  den(115) = den(59)*den(97)
  den(116) = den(59)*den(102)
  den(117) = den(59)*den(104)
  den(118) = den(59)*den(106)
  den(119) = den(12)*den(64)
  den(120) = den(14)*den(64)
  den(121) = den(16)*den(64)
  den(122) = den(2)*den(3)
  den(124) = den(122)*den(123)
  den(125) = den(1)*den(124)
  den(126) = den(1)*den(3)
  den(128) = den(126)*den(127)
  den(129) = den(2)*den(128)
  den(131) = den(4)*den(130)
  den(132) = den(3)*den(131)
  den(133) = den(1)**2
  den(134) = den(85)*den(133)
  den(135) = den(7)*den(85)
  den(136) = den(3)**2
  den(137) = den(7)*den(136)
  den(138) = den(76)*den(133)
  den(139) = den(2)**2
  den(140) = den(10)*den(139)
  den(141) = den(10)*den(76)
  den(142) = den(90)*den(133)
  den(143) = den(92)*den(133)
  den(144) = den(94)*den(133)
  den(145) = den(19)*den(133)
  den(146) = den(21)*den(133)
  den(147) = den(23)*den(133)
  den(148) = den(7)*den(90)
  den(149) = den(7)*den(92)
  den(150) = den(7)*den(94)
  den(151) = den(10)*den(19)
  den(152) = den(10)*den(21)
  den(153) = den(10)*den(23)
  den(154) = den(3)*den(26)
  den(156) = den(154)*den(155)
  den(157) = den(25)*den(156)
  den(158) = den(3)*den(25)
  den(160) = den(158)*den(159)
  den(161) = den(26)*den(160)
  den(162) = den(27)*den(130)
  den(163) = den(3)*den(162)
  den(164) = den(26)**2
  den(165) = den(85)*den(164)
  den(166) = den(29)*den(85)
  den(167) = den(29)*den(136)
  den(168) = den(25)**2
  den(169) = den(32)*den(168)
  den(170) = den(80)*den(164)
  den(171) = den(32)*den(80)
  den(172) = den(90)*den(164)
  den(173) = den(92)*den(164)
  den(174) = den(94)*den(164)
  den(175) = den(29)*den(90)
  den(176) = den(29)*den(92)
  den(177) = den(29)*den(94)
  den(178) = den(32)*den(38)
  den(179) = den(32)*den(40)
  den(180) = den(32)*den(42)
  den(181) = den(38)*den(164)
  den(182) = den(40)*den(164)
  den(183) = den(42)*den(164)
  den(184) = den(2)*den(45)
  den(186) = den(184)*den(185)
  den(187) = den(44)*den(186)
  den(188) = den(44)*den(45)
  den(189) = den(127)*den(188)
  den(190) = den(2)*den(189)
  den(192) = den(46)*den(191)
  den(193) = den(45)*den(192)
  den(194) = den(44)**2
  den(195) = den(97)*den(194)
  den(196) = den(49)*den(97)
  den(197) = den(45)**2
  den(198) = den(49)*den(197)
  den(199) = den(76)*den(194)
  den(200) = den(51)*den(139)
  den(201) = den(51)*den(76)
  den(202) = den(102)*den(194)
  den(203) = den(104)*den(194)
  den(204) = den(106)*den(194)
  den(205) = den(19)*den(194)
  den(206) = den(21)*den(194)
  den(207) = den(23)*den(194)
  den(208) = den(49)*den(102)
  den(209) = den(49)*den(104)
  den(210) = den(49)*den(106)
  den(211) = den(19)*den(51)
  den(212) = den(21)*den(51)
  den(213) = den(23)*den(51)
  den(214) = den(45)*den(59)
  den(215) = den(155)*den(214)
  den(216) = den(25)*den(215)
  den(217) = den(25)*den(45)
  den(219) = den(217)*den(218)
  den(220) = den(59)*den(219)
  den(221) = den(60)*den(191)
  den(222) = den(45)*den(221)
  den(223) = den(59)**2
  den(224) = den(97)*den(223)
  den(225) = den(62)*den(97)
  den(226) = den(62)*den(197)
  den(227) = den(64)*den(168)
  den(228) = den(80)*den(223)
  den(229) = den(64)*den(80)
  den(230) = den(102)*den(223)
  den(231) = den(104)*den(223)
  den(232) = den(106)*den(223)
  den(233) = den(62)*den(102)
  den(234) = den(62)*den(104)
  den(235) = den(62)*den(106)
  den(236) = den(38)*den(64)
  den(237) = den(40)*den(64)
  den(238) = den(42)*den(64)
  den(239) = den(38)*den(223)
  den(240) = den(40)*den(223)
  den(241) = den(42)*den(223)
  den(242) = den(7)*den(130)
  den(243) = den(10)*den(127)
  den(244) = den(29)*den(130)
  den(245) = den(32)*den(155)
  den(246) = den(49)*den(191)
  den(247) = den(51)*den(127)
  den(248) = den(62)*den(191)
  den(249) = den(64)*den(155)
  den(250) = den(97)*den(218)
  den(251) = den(97)*den(185)
  den(252) = den(85)*den(159)
  den(253) = den(85)*den(123)
  den(254) = den(80)*den(218)
  den(255) = den(80)*den(159)
  den(256) = den(76)*den(185)
  den(257) = den(76)*den(123)
  den(258) = den(38)*den(218)
  den(259) = den(40)*den(218)
  den(260) = den(42)*den(218)
  den(261) = den(102)*den(218)
  den(262) = den(104)*den(218)
  den(263) = den(106)*den(218)
  den(264) = den(38)*den(159)
  den(265) = den(40)*den(159)
  den(266) = den(42)*den(159)
  den(267) = den(90)*den(159)
  den(268) = den(92)*den(159)
  den(269) = den(94)*den(159)
  den(270) = den(19)*den(185)
  den(271) = den(21)*den(185)
  den(272) = den(23)*den(185)
  den(273) = den(102)*den(185)
  den(274) = den(104)*den(185)
  den(275) = den(106)*den(185)
  den(276) = den(19)*den(123)
  den(277) = den(21)*den(123)
  den(278) = den(23)*den(123)
  den(279) = den(90)*den(123)
  den(280) = den(92)*den(123)
  den(281) = den(94)*den(123)
  den(282) = den(1)*den(12)*den(59)
  den(283) = den(1)*den(14)*den(59)
  den(284) = den(1)*den(16)*den(59)
  den(285) = den(1)*den(2)*den(3)
  den(286) = den(1)*den(12)
  den(287) = den(1)*den(14)
  den(288) = den(1)*den(16)
  den(289) = den(12)*den(26)*den(44)
  den(290) = den(14)*den(26)*den(44)
  den(291) = den(16)*den(26)*den(44)
  den(292) = den(3)*den(25)*den(26)
  den(293) = den(12)*den(26)
  den(294) = den(14)*den(26)
  den(295) = den(16)*den(26)
  den(296) = den(2)*den(44)*den(45)
  den(297) = den(12)*den(44)
  den(298) = den(14)*den(44)
  den(299) = den(16)*den(44)
  den(300) = den(25)*den(45)*den(59)
  den(301) = den(12)*den(59)
  den(302) = den(14)*den(59)
  den(303) = den(16)*den(59)
  den(304) = den(3)*den(242)
  den(305) = den(1)*den(253)
  den(306) = den(2)*den(243)
  den(307) = den(1)*den(257)
  den(308) = den(1)*den(276)
  den(309) = den(1)*den(277)
  den(310) = den(1)*den(278)
  den(311) = den(1)*den(279)
  den(312) = den(1)*den(280)
  den(313) = den(1)*den(281)
  den(314) = den(3)*den(244)
  den(315) = den(26)*den(252)
  den(316) = den(26)*den(255)
  den(317) = den(25)*den(245)
  den(318) = den(26)*den(264)
  den(319) = den(26)*den(265)
  den(320) = den(26)*den(266)
  den(321) = den(26)*den(267)
  den(322) = den(26)*den(268)
  den(323) = den(26)*den(269)
  den(324) = den(45)*den(246)
  den(325) = den(44)*den(251)
  den(326) = den(2)*den(247)
  den(327) = den(44)*den(256)
  den(328) = den(44)*den(270)
  den(329) = den(44)*den(271)
  den(330) = den(44)*den(272)
  den(331) = den(44)*den(273)
  den(332) = den(44)*den(274)
  den(333) = den(44)*den(275)
  den(334) = den(45)*den(248)
  den(335) = den(59)*den(250)
  den(336) = den(59)*den(254)
  den(337) = den(25)*den(249)
  den(338) = den(59)*den(258)
  den(339) = den(59)*den(259)
  den(340) = den(59)*den(260)
  den(341) = den(59)*den(261)
  den(342) = den(59)*den(262)
  den(343) = den(59)*den(263)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(246)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,11),wf(:,12)) * den(11)
  A(4) = cont_QA(wf(:,9),wf(:,14)) * den(13)
  A(5) = cont_QA(wf(:,9),wf(:,16)) * den(15)
  A(6) = cont_QA(wf(:,9),wf(:,18)) * den(17)
  A(7) = cont_QA(wf(:,10),wf(:,20)) * den(20)
  A(8) = cont_QA(wf(:,10),wf(:,22)) * den(22)
  A(9) = cont_QA(wf(:,10),wf(:,24)) * den(24)
  A(10) = cont_QA(wf(:,5),wf(:,28)) * den(28)
  A(11) = cont_QA(wf(:,8),wf(:,30)) * den(30)
  A(12) = cont_QA(wf(:,32),wf(:,33)) * den(33)
  A(13) = cont_QA(wf(:,14),wf(:,30)) * den(34)
  A(14) = cont_QA(wf(:,16),wf(:,30)) * den(35)
  A(15) = cont_QA(wf(:,18),wf(:,30)) * den(36)
  A(16) = cont_QA(wf(:,31),wf(:,35)) * den(39)
  A(17) = cont_QA(wf(:,31),wf(:,37)) * den(41)
  A(18) = cont_QA(wf(:,31),wf(:,39)) * den(43)
  A(19) = cont_QA(wf(:,42),wf(:,43)) * den(47)
  A(20) = cont_QA(wf(:,45),wf(:,46)) * den(50)
  A(21) = cont_QA(wf(:,11),wf(:,48)) * den(52)
  A(22) = cont_QA(wf(:,46),wf(:,49)) * den(53)
  A(23) = cont_QA(wf(:,46),wf(:,50)) * den(54)
  A(24) = cont_QA(wf(:,46),wf(:,51)) * den(55)
  A(25) = cont_QA(wf(:,20),wf(:,47)) * den(56)
  A(26) = cont_QA(wf(:,22),wf(:,47)) * den(57)
  A(27) = cont_QA(wf(:,24),wf(:,47)) * den(58)
  A(28) = cont_QA(wf(:,42),wf(:,53)) * den(61)
  A(29) = cont_QA(wf(:,45),wf(:,55)) * den(63)
  A(30) = cont_QA(wf(:,32),wf(:,57)) * den(65)
  A(31) = cont_QA(wf(:,49),wf(:,55)) * den(66)
  A(32) = cont_QA(wf(:,50),wf(:,55)) * den(67)
  A(33) = cont_QA(wf(:,51),wf(:,55)) * den(68)
  A(34) = cont_QA(wf(:,35),wf(:,56)) * den(69)
  A(35) = cont_QA(wf(:,37),wf(:,56)) * den(70)
  A(36) = cont_QA(wf(:,39),wf(:,56)) * den(71)

  A(37) = cont_VV(wf(:,52),wf(:,58)) * den(72)
  A(38) = cont_VV(wf(:,26),wf(:,59)) * den(73)
  A(39) = cont_SS(wf(:,13),wf(:,60)) * den(74)
  A(40) = cont_VV(wf(:,17),wf(:,61)) * den(75)
  A(41) = cont_QA(wf(:,5),wf(:,62)) * den(5)
  A(42) = cont_QA(wf(:,9),wf(:,63)) * den(8)
  A(43) = cont_QA(wf(:,12),wf(:,64)) * den(11)
  A(44) = cont_QA(wf(:,65),wf(:,66)) * den(77)
  A(45) = cont_QA(wf(:,9),wf(:,67)) * den(13)
  A(46) = cont_QA(wf(:,9),wf(:,68)) * den(15)
  A(47) = cont_QA(wf(:,9),wf(:,69)) * den(17)
  A(48) = cont_QA(wf(:,20),wf(:,65)) * den(20)
  A(49) = cont_QA(wf(:,22),wf(:,65)) * den(22)
  A(50) = cont_QA(wf(:,24),wf(:,65)) * den(24)
  A(51) = cont_QA(wf(:,6),wf(:,71)) * den(5)
  A(52) = cont_QA(wf(:,9),wf(:,72)) * den(8)
  A(53) = cont_SS(wf(:,13),wf(:,73)) * den(78)
  A(54) = cont_VV(wf(:,17),wf(:,74)) * den(79)
  A(55) = cont_QA(wf(:,5),wf(:,75)) * den(28)
  A(56) = cont_QA(wf(:,30),wf(:,63)) * den(30)
  A(57) = cont_QA(wf(:,33),wf(:,76)) * den(33)
  A(58) = cont_QA(wf(:,77),wf(:,78)) * den(81)
  A(59) = cont_QA(wf(:,30),wf(:,67)) * den(34)
  A(60) = cont_QA(wf(:,30),wf(:,68)) * den(35)
  A(61) = cont_QA(wf(:,30),wf(:,69)) * den(36)
  A(62) = cont_QA(wf(:,35),wf(:,77)) * den(39)
  A(63) = cont_QA(wf(:,37),wf(:,77)) * den(41)
  A(64) = cont_QA(wf(:,39),wf(:,77)) * den(43)
  A(65) = cont_QA(wf(:,28),wf(:,71)) * den(28)
  A(66) = cont_QA(wf(:,30),wf(:,72)) * den(30)
  A(67) = cont_QA(wf(:,42),wf(:,79)) * den(47)
  A(68) = cont_QA(wf(:,46),wf(:,80)) * den(50)
  A(69) = cont_QA(wf(:,48),wf(:,64)) * den(52)
  A(70) = cont_QA(wf(:,42),wf(:,81)) * den(61)
  A(71) = cont_QA(wf(:,55),wf(:,80)) * den(63)
  A(72) = cont_QA(wf(:,57),wf(:,76)) * den(65)
  A(73) = cont_QA(wf(:,66),wf(:,82)) * den(82)
  A(74) = cont_QA(wf(:,46),wf(:,83)) * den(53)
  A(75) = cont_QA(wf(:,46),wf(:,84)) * den(54)
  A(76) = cont_QA(wf(:,46),wf(:,85)) * den(55)
  A(77) = cont_QA(wf(:,20),wf(:,82)) * den(56)
  A(78) = cont_QA(wf(:,22),wf(:,82)) * den(57)
  A(79) = cont_QA(wf(:,24),wf(:,82)) * den(58)
  A(80) = cont_QA(wf(:,43),wf(:,87)) * den(47)
  A(81) = cont_QA(wf(:,46),wf(:,88)) * den(50)
  A(82) = cont_QA(wf(:,78),wf(:,89)) * den(83)
  A(83) = cont_QA(wf(:,55),wf(:,83)) * den(66)
  A(84) = cont_QA(wf(:,55),wf(:,84)) * den(67)
  A(85) = cont_QA(wf(:,55),wf(:,85)) * den(68)
  A(86) = cont_QA(wf(:,35),wf(:,89)) * den(69)
  A(87) = cont_QA(wf(:,37),wf(:,89)) * den(70)
  A(88) = cont_QA(wf(:,39),wf(:,89)) * den(71)
  A(89) = cont_QA(wf(:,53),wf(:,87)) * den(61)
  A(90) = cont_QA(wf(:,55),wf(:,88)) * den(63)
  A(91) = cont_QA(wf(:,90),wf(:,91)) * den(86)
  A(92) = cont_QA(wf(:,5),wf(:,94)) * den(5)
  A(93) = cont_QA(wf(:,12),wf(:,95)) * den(87)
  A(94) = cont_QA(wf(:,12),wf(:,96)) * den(88)
  A(95) = cont_QA(wf(:,12),wf(:,97)) * den(89)
  A(96) = cont_QA(wf(:,90),wf(:,98)) * den(91)
  A(97) = cont_QA(wf(:,90),wf(:,99)) * den(93)
  A(98) = cont_QA(wf(:,90),wf(:,100)) * den(95)
  A(99) = cont_QA(wf(:,12),wf(:,101)) * den(11)
  A(100) = cont_QA(wf(:,102),wf(:,103)) * den(98)
  A(101) = cont_QA(wf(:,42),wf(:,104)) * den(47)
  A(102) = cont_QA(wf(:,48),wf(:,95)) * den(99)
  A(103) = cont_QA(wf(:,48),wf(:,96)) * den(100)
  A(104) = cont_QA(wf(:,48),wf(:,97)) * den(101)
  A(105) = cont_QA(wf(:,102),wf(:,105)) * den(103)
  A(106) = cont_QA(wf(:,102),wf(:,106)) * den(105)
  A(107) = cont_QA(wf(:,102),wf(:,107)) * den(107)
  A(108) = cont_QA(wf(:,48),wf(:,101)) * den(52)
  A(109) = cont_QA(wf(:,42),wf(:,109)) * den(61)
  A(110) = cont_QA(wf(:,45),wf(:,111)) * den(63)
  A(111) = cont_QA(wf(:,32),wf(:,113)) * den(65)
  A(112) = cont_QA(wf(:,35),wf(:,112)) * den(69)
  A(113) = cont_QA(wf(:,37),wf(:,112)) * den(70)
  A(114) = cont_QA(wf(:,39),wf(:,112)) * den(71)
  A(115) = cont_QA(wf(:,49),wf(:,111)) * den(66)
  A(116) = cont_QA(wf(:,50),wf(:,111)) * den(67)
  A(117) = cont_QA(wf(:,51),wf(:,111)) * den(68)
  A(118) = cont_QA(wf(:,5),wf(:,115)) * den(28)
  A(119) = cont_QA(wf(:,8),wf(:,117)) * den(30)
  A(120) = cont_QA(wf(:,32),wf(:,119)) * den(33)
  A(121) = cont_QA(wf(:,35),wf(:,118)) * den(39)
  A(122) = cont_QA(wf(:,37),wf(:,118)) * den(41)
  A(123) = cont_QA(wf(:,39),wf(:,118)) * den(43)
  A(124) = cont_QA(wf(:,14),wf(:,117)) * den(34)
  A(125) = cont_QA(wf(:,16),wf(:,117)) * den(35)
  A(126) = cont_QA(wf(:,18),wf(:,117)) * den(36)
  A(127) = cont_QA(wf(:,91),wf(:,120)) * den(108)
  A(128) = cont_QA(wf(:,5),wf(:,123)) * den(28)
  A(129) = cont_QA(wf(:,98),wf(:,120)) * den(109)
  A(130) = cont_QA(wf(:,99),wf(:,120)) * den(110)
  A(131) = cont_QA(wf(:,100),wf(:,120)) * den(111)
  A(132) = cont_QA(wf(:,33),wf(:,124)) * den(112)
  A(133) = cont_QA(wf(:,33),wf(:,125)) * den(113)
  A(134) = cont_QA(wf(:,33),wf(:,126)) * den(114)
  A(135) = cont_QA(wf(:,33),wf(:,127)) * den(33)
  A(136) = cont_QA(wf(:,103),wf(:,128)) * den(115)
  A(137) = cont_QA(wf(:,42),wf(:,129)) * den(61)
  A(138) = cont_QA(wf(:,105),wf(:,128)) * den(116)
  A(139) = cont_QA(wf(:,106),wf(:,128)) * den(117)
  A(140) = cont_QA(wf(:,107),wf(:,128)) * den(118)
  A(141) = cont_QA(wf(:,57),wf(:,124)) * den(119)
  A(142) = cont_QA(wf(:,57),wf(:,125)) * den(120)
  A(143) = cont_QA(wf(:,57),wf(:,126)) * den(121)
  A(144) = cont_QA(wf(:,57),wf(:,127)) * den(65)
  A(145) = cont_QA(wf(:,42),wf(:,131)) * den(47)
  A(146) = cont_QA(wf(:,45),wf(:,133)) * den(50)
  A(147) = cont_QA(wf(:,11),wf(:,135)) * den(52)
  A(148) = cont_QA(wf(:,49),wf(:,133)) * den(53)
  A(149) = cont_QA(wf(:,50),wf(:,133)) * den(54)
  A(150) = cont_QA(wf(:,51),wf(:,133)) * den(55)
  A(151) = cont_QA(wf(:,20),wf(:,134)) * den(56)
  A(152) = cont_QA(wf(:,22),wf(:,134)) * den(57)
  A(153) = cont_QA(wf(:,24),wf(:,134)) * den(58)
  A(154) = cont_QA(wf(:,5),wf(:,137)) * den(5)
  A(155) = cont_QA(wf(:,8),wf(:,139)) * den(8)
  A(156) = cont_QA(wf(:,11),wf(:,141)) * den(11)
  A(157) = cont_QA(wf(:,14),wf(:,139)) * den(13)
  A(158) = cont_QA(wf(:,16),wf(:,139)) * den(15)
  A(159) = cont_QA(wf(:,18),wf(:,139)) * den(17)
  A(160) = cont_QA(wf(:,20),wf(:,140)) * den(20)
  A(161) = cont_QA(wf(:,22),wf(:,140)) * den(22)
  A(162) = cont_QA(wf(:,24),wf(:,140)) * den(24)
  A(163) = cont_VV(wf(:,142),wf(:,143)) * den(125)
  A(164) = cont_QA(wf(:,145),wf(:,146)) * den(129)
  A(165) = cont_QA(wf(:,147),wf(:,148)) * den(132)
  A(166) = cont_QA(wf(:,91),wf(:,149)) * den(134)
  A(167) = cont_QA(wf(:,91),wf(:,150)) * den(135)
  A(168) = cont_QA(wf(:,9),wf(:,152)) * den(137)
  A(169) = cont_QA(wf(:,66),wf(:,153)) * den(138)
  A(170) = cont_QA(wf(:,12),wf(:,155)) * den(140)
  A(171) = cont_QA(wf(:,66),wf(:,156)) * den(141)
  A(172) = cont_QA(wf(:,98),wf(:,149)) * den(142)
  A(173) = cont_QA(wf(:,99),wf(:,149)) * den(143)
  A(174) = cont_QA(wf(:,100),wf(:,149)) * den(144)
  A(175) = cont_QA(wf(:,20),wf(:,153)) * den(145)
  A(176) = cont_QA(wf(:,22),wf(:,153)) * den(146)
  A(177) = cont_QA(wf(:,24),wf(:,153)) * den(147)
  A(178) = cont_QA(wf(:,98),wf(:,150)) * den(148)
  A(179) = cont_QA(wf(:,99),wf(:,150)) * den(149)
  A(180) = cont_QA(wf(:,100),wf(:,150)) * den(150)
  A(181) = cont_QA(wf(:,12),wf(:,157)) * den(151)
  A(182) = cont_QA(wf(:,12),wf(:,158)) * den(152)
  A(183) = cont_QA(wf(:,12),wf(:,159)) * den(153)
  A(184) = cont_QA(wf(:,161),wf(:,162)) * den(157)
  A(185) = cont_VV(wf(:,163),wf(:,164)) * den(161)
  A(186) = cont_QA(wf(:,147),wf(:,165)) * den(163)
  A(187) = cont_QA(wf(:,91),wf(:,166)) * den(165)
  A(188) = cont_QA(wf(:,91),wf(:,167)) * den(166)
  A(189) = cont_QA(wf(:,30),wf(:,152)) * den(167)
  A(190) = cont_QA(wf(:,33),wf(:,169)) * den(169)
  A(191) = cont_QA(wf(:,78),wf(:,170)) * den(170)
  A(192) = cont_QA(wf(:,78),wf(:,171)) * den(171)
  A(193) = cont_QA(wf(:,98),wf(:,166)) * den(172)
  A(194) = cont_QA(wf(:,99),wf(:,166)) * den(173)
  A(195) = cont_QA(wf(:,100),wf(:,166)) * den(174)
  A(196) = cont_QA(wf(:,98),wf(:,167)) * den(175)
  A(197) = cont_QA(wf(:,99),wf(:,167)) * den(176)
  A(198) = cont_QA(wf(:,100),wf(:,167)) * den(177)
  A(199) = cont_QA(wf(:,33),wf(:,172)) * den(178)
  A(200) = cont_QA(wf(:,33),wf(:,173)) * den(179)
  A(201) = cont_QA(wf(:,33),wf(:,174)) * den(180)
  A(202) = cont_QA(wf(:,35),wf(:,170)) * den(181)
  A(203) = cont_QA(wf(:,37),wf(:,170)) * den(182)
  A(204) = cont_QA(wf(:,39),wf(:,170)) * den(183)
  A(205) = cont_VV(wf(:,175),wf(:,176)) * den(187)
  A(206) = cont_QA(wf(:,145),wf(:,178)) * den(190)
  A(207) = cont_QA(wf(:,179),wf(:,180)) * den(193)
  A(208) = cont_QA(wf(:,103),wf(:,181)) * den(195)
  A(209) = cont_QA(wf(:,103),wf(:,182)) * den(196)
  A(210) = cont_QA(wf(:,46),wf(:,184)) * den(198)
  A(211) = cont_QA(wf(:,66),wf(:,185)) * den(199)
  A(212) = cont_QA(wf(:,48),wf(:,155)) * den(200)
  A(213) = cont_QA(wf(:,66),wf(:,186)) * den(201)
  A(214) = cont_QA(wf(:,105),wf(:,181)) * den(202)
  A(215) = cont_QA(wf(:,106),wf(:,181)) * den(203)
  A(216) = cont_QA(wf(:,107),wf(:,181)) * den(204)
  A(217) = cont_QA(wf(:,20),wf(:,185)) * den(205)
  A(218) = cont_QA(wf(:,22),wf(:,185)) * den(206)
  A(219) = cont_QA(wf(:,24),wf(:,185)) * den(207)
  A(220) = cont_QA(wf(:,105),wf(:,182)) * den(208)
  A(221) = cont_QA(wf(:,106),wf(:,182)) * den(209)
  A(222) = cont_QA(wf(:,107),wf(:,182)) * den(210)
  A(223) = cont_QA(wf(:,48),wf(:,157)) * den(211)
  A(224) = cont_QA(wf(:,48),wf(:,158)) * den(212)
  A(225) = cont_QA(wf(:,48),wf(:,159)) * den(213)
  A(226) = cont_QA(wf(:,161),wf(:,188)) * den(216)
  A(227) = cont_VV(wf(:,189),wf(:,190)) * den(220)
  A(228) = cont_QA(wf(:,179),wf(:,191)) * den(222)
  A(229) = cont_QA(wf(:,103),wf(:,192)) * den(224)
  A(230) = cont_QA(wf(:,103),wf(:,193)) * den(225)
  A(231) = cont_QA(wf(:,55),wf(:,184)) * den(226)
  A(232) = cont_QA(wf(:,57),wf(:,169)) * den(227)
  A(233) = cont_QA(wf(:,78),wf(:,194)) * den(228)
  A(234) = cont_QA(wf(:,78),wf(:,195)) * den(229)
  A(235) = cont_QA(wf(:,105),wf(:,192)) * den(230)
  A(236) = cont_QA(wf(:,106),wf(:,192)) * den(231)
  A(237) = cont_QA(wf(:,107),wf(:,192)) * den(232)
  A(238) = cont_QA(wf(:,105),wf(:,193)) * den(233)
  A(239) = cont_QA(wf(:,106),wf(:,193)) * den(234)
  A(240) = cont_QA(wf(:,107),wf(:,193)) * den(235)
  A(241) = cont_QA(wf(:,57),wf(:,172)) * den(236)
  A(242) = cont_QA(wf(:,57),wf(:,173)) * den(237)
  A(243) = cont_QA(wf(:,57),wf(:,174)) * den(238)
  A(244) = cont_QA(wf(:,35),wf(:,194)) * den(239)
  A(245) = cont_QA(wf(:,37),wf(:,194)) * den(240)
  A(246) = cont_QA(wf(:,39),wf(:,194)) * den(241)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(246)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(14)+A(17)+A(23)+A(26))*f(1))/6._/**/REALKIND+((A(5)+A(8)+A(32)+A(35))*f(1))/2._/**/REALKIND+((-A(10)-A(11)-A(12) &
       -A(19)-A(20)-A(21))*f(6))/6._/**/REALKIND+((-A(1)-A(2)-A(3)-A(28)-A(29)-A(30))*f(6))/2._/**/REALKIND+((A(13)+A(16)+A(22) &
       +A(25))*f(11))/6._/**/REALKIND+((A(4)+A(7)+A(31)+A(34))*f(11))/2._/**/REALKIND+((-A(15)-A(18)-A(24) &
       -A(27))*f(15))/6._/**/REALKIND+((-A(6)-A(9)-A(33)-A(36))*f(15))/2._/**/REALKIND
  M1(2) = ((-A(14)-A(17)-A(23)-A(26))*f(1))/2._/**/REALKIND+((-A(5)-A(8)-A(32)-A(35))*f(1))/6._/**/REALKIND+((A(10)+A(11)+A(12) &
       +A(19)+A(20)+A(21))*f(6))/2._/**/REALKIND+((A(1)+A(2)+A(3)+A(28)+A(29)+A(30))*f(6))/6._/**/REALKIND+((-A(13)-A(16)-A(22) &
       -A(25))*f(11))/2._/**/REALKIND+((-A(4)-A(7)-A(31)-A(34))*f(11))/6._/**/REALKIND+((A(15)+A(18)+A(24) &
       +A(27))*f(15))/2._/**/REALKIND+((A(6)+A(9)+A(33)+A(36))*f(15))/6._/**/REALKIND

  M2(1) = ((-A(194)-A(197)-A(200)-A(203)-A(215)-A(218)-A(221)-A(224))*f(2))/6._/**/REALKIND+((-A(173)-A(176)-A(179)-A(182)-A(236) &
       -A(239)-A(242)-A(245))*f(2))/2._/**/REALKIND+((A(63)+A(78)+A(106)+A(122)+A(125)+A(130)+A(149)+A(152))*f(3))/6._/**/REALKIND &
       +((A(49)+A(87)+A(97)+A(113)+A(116)+A(139)+A(158)+A(161))*f(3))/2._/**/REALKIND+((A(60)+A(75)+A(103) &
       +A(133))*f(4))/6._/**/REALKIND+((A(46)+A(84)+A(94)+A(142))*f(4))/2._/**/REALKIND-(A(37)*f(5))/2._/**/REALKIND &
       -(A(38)*f(5))/6._/**/REALKIND+((A(184)+A(185)+A(186)+A(187)+A(188)+A(189)+A(190)+A(191)+A(192)+A(205)+A(206)+A(207)+A(208) &
       +A(209)+A(210)+A(211)+A(212)+A(213))*f(7))/6._/**/REALKIND+((A(163)+A(164)+A(165)+A(166)+A(167)+A(168)+A(169)+A(170)+A(171) &
       +A(226)+A(227)+A(228)+A(229)+A(230)+A(231)+A(232)+A(233)+A(234))*f(7))/2._/**/REALKIND+((-A(58)-A(73)-A(100)-A(118)-A(119) &
       -A(120)-A(127)-A(145)-A(146)-A(147))*f(8))/6._/**/REALKIND+((-A(44)-A(82)-A(91)-A(109)-A(110)-A(111)-A(136)-A(154)-A(155) &
       -A(156))*f(8))/2._/**/REALKIND+((-A(55)-A(67))*f(9))/6._/**/REALKIND+((-A(41)-A(70))*f(9))/2._/**/REALKIND+((-A(56)-A(57) &
       -A(65)-A(66)-A(68)-A(69)-A(80)-A(81)-A(101)-A(108)-A(128)-A(135))*f(10))/6._/**/REALKIND+((-A(42)-A(43)-A(51)-A(52)-A(71) &
       -A(72)-A(89)-A(90)-A(92)-A(99)-A(137)-A(144))*f(10))/2._/**/REALKIND+((-A(193)-A(196)-A(199)-A(202)-A(214)-A(217)-A(220) &
       -A(223))*f(12))/6._/**/REALKIND+((-A(172)-A(175)-A(178)-A(181)-A(235)-A(238)-A(241)-A(244))*f(12))/2._/**/REALKIND+((A(62) &
       +A(77)+A(105)+A(121)+A(124)+A(129)+A(148)+A(151))*f(13))/6._/**/REALKIND+((A(48)+A(86)+A(96)+A(112)+A(115)+A(138)+A(157) &
       +A(160))*f(13))/2._/**/REALKIND+((A(59)+A(74)+A(102)+A(132))*f(14))/6._/**/REALKIND+((A(45)+A(83)+A(93) &
       +A(141))*f(14))/2._/**/REALKIND+((A(195)+A(198)+A(201)+A(204)+A(216)+A(219)+A(222)+A(225))*f(16))/6._/**/REALKIND+((A(174) &
       +A(177)+A(180)+A(183)+A(237)+A(240)+A(243)+A(246))*f(16))/2._/**/REALKIND+((-A(64)-A(79)-A(107)-A(123)-A(126)-A(131)-A(150) &
       -A(153))*f(17))/6._/**/REALKIND+((-A(50)-A(88)-A(98)-A(114)-A(117)-A(140)-A(159)-A(162))*f(17))/2._/**/REALKIND+((-A(61) &
       -A(76)-A(104)-A(134))*f(18))/6._/**/REALKIND+((-A(47)-A(85)-A(95)-A(143))*f(18))/2._/**/REALKIND &
       +(A(40)*f(19))/2._/**/REALKIND+(A(54)*f(19))/6._/**/REALKIND+(A(39)*f(20))/2._/**/REALKIND+(A(53)*f(20))/6._/**/REALKIND
  M2(2) = ((A(194)+A(197)+A(200)+A(203)+A(215)+A(218)+A(221)+A(224))*f(2))/2._/**/REALKIND+((A(173)+A(176)+A(179)+A(182)+A(236) &
       +A(239)+A(242)+A(245))*f(2))/6._/**/REALKIND+((-A(63)-A(78)-A(106)-A(122)-A(125)-A(130)-A(149) &
       -A(152))*f(3))/2._/**/REALKIND+((-A(49)-A(87)-A(97)-A(113)-A(116)-A(139)-A(158)-A(161))*f(3))/6._/**/REALKIND+((-A(60) &
       -A(75)-A(103)-A(133))*f(4))/2._/**/REALKIND+((-A(46)-A(84)-A(94)-A(142))*f(4))/6._/**/REALKIND+(A(37)*f(5))/6._/**/REALKIND &
       +(A(38)*f(5))/2._/**/REALKIND+((-A(184)-A(185)-A(186)-A(187)-A(188)-A(189)-A(190)-A(191)-A(192)-A(205)-A(206)-A(207)-A(208) &
       -A(209)-A(210)-A(211)-A(212)-A(213))*f(7))/2._/**/REALKIND+((-A(163)-A(164)-A(165)-A(166)-A(167)-A(168)-A(169)-A(170) &
       -A(171)-A(226)-A(227)-A(228)-A(229)-A(230)-A(231)-A(232)-A(233)-A(234))*f(7))/6._/**/REALKIND+((A(58)+A(73)+A(100)+A(118) &
       +A(119)+A(120)+A(127)+A(145)+A(146)+A(147))*f(8))/2._/**/REALKIND+((A(44)+A(82)+A(91)+A(109)+A(110)+A(111)+A(136)+A(154) &
       +A(155)+A(156))*f(8))/6._/**/REALKIND+((A(55)+A(67))*f(9))/2._/**/REALKIND+((A(41)+A(70))*f(9))/6._/**/REALKIND+((A(56) &
       +A(57)+A(65)+A(66)+A(68)+A(69)+A(80)+A(81)+A(101)+A(108)+A(128)+A(135))*f(10))/2._/**/REALKIND+((A(42)+A(43)+A(51)+A(52) &
       +A(71)+A(72)+A(89)+A(90)+A(92)+A(99)+A(137)+A(144))*f(10))/6._/**/REALKIND+((A(193)+A(196)+A(199)+A(202)+A(214)+A(217) &
       +A(220)+A(223))*f(12))/2._/**/REALKIND+((A(172)+A(175)+A(178)+A(181)+A(235)+A(238)+A(241)+A(244))*f(12))/6._/**/REALKIND+(( &
       -A(62)-A(77)-A(105)-A(121)-A(124)-A(129)-A(148)-A(151))*f(13))/2._/**/REALKIND+((-A(48)-A(86)-A(96)-A(112)-A(115)-A(138) &
       -A(157)-A(160))*f(13))/6._/**/REALKIND+((-A(59)-A(74)-A(102)-A(132))*f(14))/2._/**/REALKIND+((-A(45)-A(83)-A(93) &
       -A(141))*f(14))/6._/**/REALKIND+((-A(195)-A(198)-A(201)-A(204)-A(216)-A(219)-A(222)-A(225))*f(16))/2._/**/REALKIND+(( &
       -A(174)-A(177)-A(180)-A(183)-A(237)-A(240)-A(243)-A(246))*f(16))/6._/**/REALKIND+((A(64)+A(79)+A(107)+A(123)+A(126)+A(131) &
       +A(150)+A(153))*f(17))/2._/**/REALKIND+((A(50)+A(88)+A(98)+A(114)+A(117)+A(140)+A(159)+A(162))*f(17))/6._/**/REALKIND &
       +((A(61)+A(76)+A(104)+A(134))*f(18))/2._/**/REALKIND+((A(47)+A(85)+A(95)+A(143))*f(18))/6._/**/REALKIND &
       -(A(40)*f(19))/6._/**/REALKIND-(A(54)*f(19))/2._/**/REALKIND-(A(39)*f(20))/6._/**/REALKIND-(A(53)*f(20))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppwwjj_bbbxbxwwx_1_/**/REALKIND
