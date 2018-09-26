
module ol_colourmatrix_ppzzjj_uuxbbxzz_1_/**/REALKIND
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
end module ol_colourmatrix_ppzzjj_uuxbbxzz_1_/**/REALKIND



module ol_forced_parameters_ppzzjj_uuxbbxzz_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppzzjj_uuxbbxzz_1_/**/REALKIND

module ol_loop_ppzzjj_uuxbbxzz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(25), c(36)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:262)
  ! denominators
  complex(REALKIND), save :: den(285)
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
    f( 1) = CI*eQED**2*gQCD**2
    f( 2) = CI*countertermnorm*eQED**2*gQCD**4
    f( 3) = CI*countertermnorm*ctGbb*eQED**2*gQCD**4
    f( 4) = CI*countertermnorm*ctGqq*eQED**2*gQCD**4
    f( 5) = CI*countertermnorm*ctVbb*eQED**2*gQCD**4
    f( 6) = CI*countertermnorm*ctVqq*eQED**2*gQCD**4
    f( 7) = countertermnorm*ctZGG*eQED**2*gQCD**4
    f( 8) = CI*countertermnorm*ctZZGG*eQED**2*gQCD**4
    f( 9) = (CI*eQED**2*gQCD**2*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(10) = (CI*countertermnorm*eQED**2*gQCD**4*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(11) = (CI*countertermnorm*ctGbb*eQED**2*gQCD**4*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(12) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(13) = (CI*countertermnorm*ctSbb*eQED**2*gQCD**4*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(14) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MW)/(cw**2*sw)
    f(15) = CI*eQED**2*gQCD**4*integralnorm*SwB
    f(16) = (eQED**2*gQCD**4*integralnorm*SwB)/2._/**/REALKIND
    f(17) = eQED**2*gQCD**4*integralnorm*SwB
    f(18) = (CI*eQED**2*gQCD**4*integralnorm*MB*SwB)/(2._/**/REALKIND*cw**2*sw**2)
    f(19) = (eQED**2*gQCD**4*integralnorm*MB*SwB)/(cw**2*sw**2*4._/**/REALKIND)
    f(20) = (eQED**2*gQCD**4*integralnorm*MB*SwB)/(cw**2*sw**2*2._/**/REALKIND)
    f(21) = eQED**2*gQCD**4*integralnorm*SwF
    f(22) = 2*eQED**2*gQCD**4*integralnorm*SwF
    f(23) = (eQED**2*gQCD**4*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(24) = (eQED**2*gQCD**4*integralnorm*MB*SwF)/(cw**2*sw**2)
    f(25) = (eQED**2*gQCD**4*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)

  c = [ 9*CI*f(15), 27*CI*f(15), 18*f(16), 54*f(16), f(17), 3*f(17), 6*f(17), 8*f(17), 10*f(17), 18*f(17), 21*f(17), 24*f(17) &
    , 54*f(17), 9*CI*f(18), 27*CI*f(18), 18*f(19), 54*f(19), f(20), 3*f(20), 6*f(20), 8*f(20), 10*f(20), 18*f(20), 21*f(20) &
    , 24*f(20), 54*f(20), 3*f(21), 9*f(21), 3*f(22), 9*f(22), 3*f(23), 9*f(23), 3*f(24), 9*f(24), 3*f(25), 9*f(25) ]
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
  complex(REALKIND) :: A(182)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rMB, H(3), wf(:,-2))
  call wf_A(P(:,4), rMB, H(4), wf(:,-3))
  call wf_V(P(:,5), rMZ, H(5), wf(:,-4))
  call wf_V(P(:,6), rMZ, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,-2),wf(:,2))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,-5),wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,20),MB,1_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,40),MB,1_intkind1,wf(:,5))
  call vert_VQ_A(wf(:,1),wf(:,4),wf(:,6))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,7))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,4),wf(:,8))
  call prop_A_Q(wf(:,7),Q(:,11),MB,1_intkind1,wf(:,9))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,-2),wf(:,10))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,-4),wf(:,11))
  call prop_Q_A(wf(:,10),Q(:,36),MB,1_intkind1,wf(:,12))
  call prop_A_Q(wf(:,11),Q(:,24),MB,1_intkind1,wf(:,13))
  call vert_VQ_A(wf(:,1),wf(:,12),wf(:,14))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,15))
  call vert_AZ_Q(gZd,wf(:,13),wf(:,-5),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,7),MB,1_intkind1,wf(:,17))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,12),wf(:,18))
  call vert_AZ_Q(gZd,wf(:,5),wf(:,-4),wf(:,19))
  call vert_VV_S(wf(:,-4),wf(:,-5),wf(:,20))
  call vert_SA_Q(gH,wf(:,20),wf(:,-3),wf(:,21))
  call vert_QS_A(gH,wf(:,-2),wf(:,20),wf(:,22))
  call prop_Q_A(wf(:,22),Q(:,52),MB,1_intkind1,wf(:,23))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,0),wf(:,24))
  call vert_AZ_Q(gZu,wf(:,-1),wf(:,-5),wf(:,25))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,26))
  call prop_Q_A(wf(:,24),Q(:,17),ZERO,0_intkind1,wf(:,27))
  call prop_A_Q(wf(:,25),Q(:,34),ZERO,0_intkind1,wf(:,28))
  call vert_QA_V(wf(:,27),wf(:,28),wf(:,29))
  call vert_AV_Q(wf(:,-1),wf(:,26),wf(:,30))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,27),wf(:,31))
  call prop_A_Q(wf(:,30),Q(:,14),ZERO,0_intkind1,wf(:,32))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,0),wf(:,33))
  call vert_AZ_Q(gZu,wf(:,-1),wf(:,-4),wf(:,34))
  call prop_Q_A(wf(:,33),Q(:,33),ZERO,0_intkind1,wf(:,35))
  call prop_A_Q(wf(:,34),Q(:,18),ZERO,0_intkind1,wf(:,36))
  call vert_QA_V(wf(:,35),wf(:,36),wf(:,37))
  call vert_VQ_A(wf(:,26),wf(:,0),wf(:,38))
  call vert_AZ_Q(gZu,wf(:,36),wf(:,-5),wf(:,39))
  call prop_Q_A(wf(:,38),Q(:,13),ZERO,0_intkind1,wf(:,40))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,35),wf(:,41))
  call vert_AZ_Q(gZu,wf(:,28),wf(:,-4),wf(:,42))
  call vert_QA_V(wf(:,27),wf(:,-1),wf(:,43))
  call vert_QA_V(wf(:,12),wf(:,-3),wf(:,44))
  call vert_QA_V(wf(:,-2),wf(:,5),wf(:,45))
  call vert_QA_V(wf(:,0),wf(:,36),wf(:,46))
  call vert_QA_V(wf(:,35),wf(:,-1),wf(:,47))
  call vert_QA_V(wf(:,4),wf(:,-3),wf(:,48))
  call vert_QA_V(wf(:,0),wf(:,28),wf(:,49))
  call vert_QA_V(wf(:,-2),wf(:,13),wf(:,50))
  call counter_VVG_G(wf(:,-4),wf(:,-5),wf(:,1),wf(:,51))
  call counter_GG_S(wf(:,1),wf(:,26),wf(:,52))
  call counter_VQ_A(wf(:,1),wf(:,4),wf(:,53))
  call counter_ZQ_A(gZd,wf(:,-5),wf(:,4),wf(:,54))
  call counter_VG_G(wf(:,-5),wf(:,1),Q(:,3),wf(:,55),Q(:,35))
  call counter_VQ_A(wf(:,1),wf(:,12),wf(:,56))
  call counter_AZ_Q(gZd,wf(:,13),wf(:,-5),wf(:,57))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,12),wf(:,58))
  call counter_VG_G(wf(:,-4),wf(:,1),Q(:,3),wf(:,59),Q(:,19))
  call counter_AZ_Q(gZd,wf(:,5),wf(:,-4),wf(:,60))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,61))
  call prop_Q_A(wf(:,8),Q(:,52),MB,1_intkind1,wf(:,62))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,-5),wf(:,63))
  call prop_A_Q(wf(:,63),Q(:,40),MB,1_intkind1,wf(:,64))
  call prop_Q_A(wf(:,18),Q(:,52),MB,1_intkind1,wf(:,65))
  call counter_SA_Q(gH,wf(:,20),wf(:,-3),wf(:,66))
  call vert_AZ_Q(gZd,wf(:,64),wf(:,-4),wf(:,67))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,-4),wf(:,68))
  call prop_A_Q(wf(:,68),Q(:,24),MB,1_intkind1,wf(:,69))
  call vert_AZ_Q(gZd,wf(:,69),wf(:,-5),wf(:,70))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,71))
  call prop_A_Q(wf(:,16),Q(:,56),MB,1_intkind1,wf(:,72))
  call counter_ZQ_A(gZd,wf(:,-5),wf(:,-2),wf(:,73))
  call prop_Q_A(wf(:,73),Q(:,36),MB,1_intkind1,wf(:,74))
  call vert_VQ_A(wf(:,1),wf(:,74),wf(:,75))
  call prop_A_Q(wf(:,19),Q(:,56),MB,1_intkind1,wf(:,76))
  call counter_QS_A(gH,wf(:,-2),wf(:,20),wf(:,77))
  call prop_A_Q(wf(:,21),Q(:,56),MB,1_intkind1,wf(:,78))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,74),wf(:,79))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,-2),wf(:,80))
  call prop_Q_A(wf(:,80),Q(:,20),MB,1_intkind1,wf(:,81))
  call vert_VQ_A(wf(:,1),wf(:,81),wf(:,82))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,81),wf(:,83))
  call counter_QA_V(wf(:,27),wf(:,28),wf(:,84))
  call counter_VG_G(wf(:,-5),wf(:,26),Q(:,12),wf(:,85),Q(:,44))
  call counter_ZQ_A(gZu,wf(:,-5),wf(:,27),wf(:,86))
  call counter_QA_V(wf(:,35),wf(:,36),wf(:,87))
  call counter_AZ_Q(gZu,wf(:,36),wf(:,-5),wf(:,88))
  call counter_VG_G(wf(:,-4),wf(:,26),Q(:,12),wf(:,89),Q(:,28))
  call counter_ZQ_A(gZu,wf(:,-4),wf(:,35),wf(:,90))
  call counter_AZ_Q(gZu,wf(:,28),wf(:,-4),wf(:,91))
  call counter_QA_V(wf(:,12),wf(:,-3),wf(:,92))
  call vert_QA_V(wf(:,-2),wf(:,64),wf(:,93))
  call counter_QA_V(wf(:,4),wf(:,-3),wf(:,94))
  call vert_QA_V(wf(:,-2),wf(:,69),wf(:,95))
  call counter_QA_V(wf(:,-2),wf(:,5),wf(:,96))
  call vert_QA_V(wf(:,74),wf(:,-3),wf(:,97))
  call counter_QA_V(wf(:,-2),wf(:,13),wf(:,98))
  call vert_QA_V(wf(:,81),wf(:,-3),wf(:,99))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,100))
  call vert_AV_Q(wf(:,-1),wf(:,100),wf(:,101))
  call prop_A_Q(wf(:,101),Q(:,14),ZERO,0_intkind1,wf(:,102))
  call vert_VQ_A(wf(:,100),wf(:,0),wf(:,103))
  call prop_Q_A(wf(:,103),Q(:,13),ZERO,0_intkind1,wf(:,104))
  call counter_AV_Q(wf(:,-1),wf(:,26),wf(:,105))
  call prop_Q_A(wf(:,31),Q(:,49),ZERO,0_intkind1,wf(:,106))
  call counter_AZ_Q(gZu,wf(:,-1),wf(:,-5),wf(:,107))
  call prop_A_Q(wf(:,107),Q(:,34),ZERO,0_intkind1,wf(:,108))
  call vert_QA_V(wf(:,27),wf(:,108),wf(:,109))
  call prop_Q_A(wf(:,41),Q(:,49),ZERO,0_intkind1,wf(:,110))
  call vert_AZ_Q(gZu,wf(:,108),wf(:,-4),wf(:,111))
  call counter_AZ_Q(gZu,wf(:,-1),wf(:,-4),wf(:,112))
  call prop_A_Q(wf(:,112),Q(:,18),ZERO,0_intkind1,wf(:,113))
  call vert_QA_V(wf(:,35),wf(:,113),wf(:,114))
  call vert_AZ_Q(gZu,wf(:,113),wf(:,-5),wf(:,115))
  call counter_QA_V(wf(:,27),wf(:,-1),wf(:,116))
  call counter_QA_V(wf(:,35),wf(:,-1),wf(:,117))
  call vert_QA_V(wf(:,0),wf(:,108),wf(:,118))
  call vert_QA_V(wf(:,0),wf(:,113),wf(:,119))
  call counter_VQ_A(wf(:,26),wf(:,0),wf(:,120))
  call prop_A_Q(wf(:,39),Q(:,50),ZERO,0_intkind1,wf(:,121))
  call counter_ZQ_A(gZu,wf(:,-5),wf(:,0),wf(:,122))
  call prop_Q_A(wf(:,122),Q(:,33),ZERO,0_intkind1,wf(:,123))
  call vert_QA_V(wf(:,123),wf(:,36),wf(:,124))
  call prop_A_Q(wf(:,42),Q(:,50),ZERO,0_intkind1,wf(:,125))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,123),wf(:,126))
  call counter_ZQ_A(gZu,wf(:,-4),wf(:,0),wf(:,127))
  call prop_Q_A(wf(:,127),Q(:,17),ZERO,0_intkind1,wf(:,128))
  call vert_QA_V(wf(:,128),wf(:,28),wf(:,129))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,128),wf(:,130))
  call counter_QA_V(wf(:,0),wf(:,36),wf(:,131))
  call counter_QA_V(wf(:,0),wf(:,28),wf(:,132))
  call vert_QA_V(wf(:,123),wf(:,-1),wf(:,133))
  call vert_QA_V(wf(:,128),wf(:,-1),wf(:,134))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,135))
  call vert_VQ_A(wf(:,135),wf(:,4),wf(:,136))
  call vert_AV_Q(wf(:,-3),wf(:,135),wf(:,137))
  call prop_A_Q(wf(:,137),Q(:,11),MB,1_intkind1,wf(:,138))
  call vert_VQ_A(wf(:,135),wf(:,12),wf(:,139))
  call vert_VQ_A(wf(:,135),wf(:,-2),wf(:,140))
  call prop_Q_A(wf(:,140),Q(:,7),MB,1_intkind1,wf(:,141))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,142))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,143))
  call vert_AV_Q(wf(:,5),wf(:,1),wf(:,144))
  call counter_Q_A(ctbb,wf(:,4),Q(:,20),wf(:,145))
  call prop_A_Q(wf(:,144),Q(:,43),MB,1_intkind1,wf(:,146))
  call counter_A_Q(ctbb,wf(:,5),Q(:,40),wf(:,147))
  call prop_Q_A(wf(:,6),Q(:,23),MB,1_intkind1,wf(:,148))
  call vert_AV_Q(wf(:,-3),wf(:,143),wf(:,149))
  call prop_Q_A(wf(:,145),Q(:,20),MB,1_intkind1,wf(:,150))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,150),wf(:,151))
  call counter_A_Q(ctbb,wf(:,9),Q(:,11),wf(:,152))
  call vert_QA_V(wf(:,12),wf(:,13),wf(:,153))
  call vert_AV_Q(wf(:,13),wf(:,1),wf(:,154))
  call counter_Q_A(ctbb,wf(:,12),Q(:,36),wf(:,155))
  call prop_A_Q(wf(:,154),Q(:,27),MB,1_intkind1,wf(:,156))
  call counter_A_Q(ctbb,wf(:,13),Q(:,24),wf(:,157))
  call prop_Q_A(wf(:,14),Q(:,39),MB,1_intkind1,wf(:,158))
  call vert_VQ_A(wf(:,143),wf(:,-2),wf(:,159))
  call counter_Q_A(ctbb,wf(:,17),Q(:,7),wf(:,160))
  call prop_A_Q(wf(:,157),Q(:,24),MB,1_intkind1,wf(:,161))
  call vert_AZ_Q(gZd,wf(:,161),wf(:,-5),wf(:,162))
  call prop_Q_A(wf(:,155),Q(:,36),MB,1_intkind1,wf(:,163))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,163),wf(:,164))
  call prop_A_Q(wf(:,147),Q(:,40),MB,1_intkind1,wf(:,165))
  call vert_AZ_Q(gZd,wf(:,165),wf(:,-4),wf(:,166))
  call counter_Q_A(ctbb,wf(:,23),Q(:,52),wf(:,167))
  call vert_AV_Q(wf(:,28),wf(:,26),wf(:,168))
  call counter_Q_A(ctqq,wf(:,27),Q(:,17),wf(:,169))
  call prop_A_Q(wf(:,168),Q(:,46),ZERO,0_intkind1,wf(:,170))
  call vert_VQ_A(wf(:,26),wf(:,27),wf(:,171))
  call counter_A_Q(ctqq,wf(:,28),Q(:,34),wf(:,172))
  call prop_Q_A(wf(:,171),Q(:,29),ZERO,0_intkind1,wf(:,173))
  call counter_V_V(ctGG,wf(:,26),Q(:,12),wf(:,174))
  call prop_Q_A(wf(:,169),Q(:,17),ZERO,0_intkind1,wf(:,175))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,175),wf(:,176))
  call vert_AV_Q(wf(:,-1),wf(:,174),wf(:,177))
  call counter_A_Q(ctqq,wf(:,32),Q(:,14),wf(:,178))
  call vert_AV_Q(wf(:,36),wf(:,26),wf(:,179))
  call counter_Q_A(ctqq,wf(:,35),Q(:,33),wf(:,180))
  call prop_A_Q(wf(:,179),Q(:,30),ZERO,0_intkind1,wf(:,181))
  call vert_VQ_A(wf(:,26),wf(:,35),wf(:,182))
  call counter_A_Q(ctqq,wf(:,36),Q(:,18),wf(:,183))
  call prop_Q_A(wf(:,182),Q(:,45),ZERO,0_intkind1,wf(:,184))
  call vert_VQ_A(wf(:,174),wf(:,0),wf(:,185))
  call counter_Q_A(ctqq,wf(:,40),Q(:,13),wf(:,186))
  call prop_A_Q(wf(:,183),Q(:,18),ZERO,0_intkind1,wf(:,187))
  call vert_AZ_Q(gZu,wf(:,187),wf(:,-5),wf(:,188))
  call prop_Q_A(wf(:,180),Q(:,33),ZERO,0_intkind1,wf(:,189))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,189),wf(:,190))
  call prop_A_Q(wf(:,172),Q(:,34),ZERO,0_intkind1,wf(:,191))
  call vert_AZ_Q(gZu,wf(:,191),wf(:,-4),wf(:,192))
  call vert_QA_V(wf(:,175),wf(:,-1),wf(:,193))
  call counter_V_V(ctGG,wf(:,43),Q(:,19),wf(:,194))
  call vert_QA_V(wf(:,163),wf(:,-3),wf(:,195))
  call vert_QA_V(wf(:,-2),wf(:,165),wf(:,196))
  call vert_QA_V(wf(:,0),wf(:,187),wf(:,197))
  call counter_V_V(ctGG,wf(:,46),Q(:,19),wf(:,198))
  call vert_QA_V(wf(:,189),wf(:,-1),wf(:,199))
  call counter_V_V(ctGG,wf(:,47),Q(:,35),wf(:,200))
  call vert_QA_V(wf(:,150),wf(:,-3),wf(:,201))
  call vert_QA_V(wf(:,0),wf(:,191),wf(:,202))
  call counter_V_V(ctGG,wf(:,49),Q(:,35),wf(:,203))
  call vert_QA_V(wf(:,-2),wf(:,161),wf(:,204))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,17),wf(:,205))
  call prop_Q_A(wf(:,205),Q(:,23),MB,1_intkind1,wf(:,206))
  call vert_ZQ_A(gZd,wf(:,-5),wf(:,17),wf(:,207))
  call prop_Q_A(wf(:,207),Q(:,39),MB,1_intkind1,wf(:,208))
  call vert_AZ_Q(gZd,wf(:,9),wf(:,-4),wf(:,209))
  call prop_A_Q(wf(:,209),Q(:,27),MB,1_intkind1,wf(:,210))
  call vert_AZ_Q(gZd,wf(:,9),wf(:,-5),wf(:,211))
  call prop_A_Q(wf(:,211),Q(:,43),MB,1_intkind1,wf(:,212))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,40),wf(:,213))
  call prop_Q_A(wf(:,213),Q(:,29),ZERO,0_intkind1,wf(:,214))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,40),wf(:,215))
  call prop_Q_A(wf(:,215),Q(:,45),ZERO,0_intkind1,wf(:,216))
  call vert_AZ_Q(gZu,wf(:,32),wf(:,-4),wf(:,217))
  call prop_A_Q(wf(:,217),Q(:,30),ZERO,0_intkind1,wf(:,218))
  call vert_AZ_Q(gZu,wf(:,32),wf(:,-5),wf(:,219))
  call prop_A_Q(wf(:,219),Q(:,46),ZERO,0_intkind1,wf(:,220))
  call vert_VQ_A(wf(:,43),wf(:,-2),wf(:,221))
  call prop_Q_A(wf(:,221),Q(:,23),MB,1_intkind1,wf(:,222))
  call vert_AV_Q(wf(:,-3),wf(:,43),wf(:,223))
  call prop_A_Q(wf(:,223),Q(:,27),MB,1_intkind1,wf(:,224))
  call vert_QA_V(wf(:,106),wf(:,-1),wf(:,225))
  call vert_VQ_A(wf(:,46),wf(:,-2),wf(:,226))
  call prop_Q_A(wf(:,226),Q(:,23),MB,1_intkind1,wf(:,227))
  call vert_AV_Q(wf(:,-3),wf(:,46),wf(:,228))
  call prop_A_Q(wf(:,228),Q(:,27),MB,1_intkind1,wf(:,229))
  call vert_QA_V(wf(:,0),wf(:,121),wf(:,230))
  call vert_VQ_A(wf(:,48),wf(:,0),wf(:,231))
  call prop_Q_A(wf(:,231),Q(:,29),ZERO,0_intkind1,wf(:,232))
  call vert_AV_Q(wf(:,-1),wf(:,48),wf(:,233))
  call prop_A_Q(wf(:,233),Q(:,30),ZERO,0_intkind1,wf(:,234))
  call vert_QA_V(wf(:,62),wf(:,-3),wf(:,235))
  call vert_VQ_A(wf(:,50),wf(:,0),wf(:,236))
  call prop_Q_A(wf(:,236),Q(:,29),ZERO,0_intkind1,wf(:,237))
  call vert_AV_Q(wf(:,-1),wf(:,50),wf(:,238))
  call prop_A_Q(wf(:,238),Q(:,30),ZERO,0_intkind1,wf(:,239))
  call vert_QA_V(wf(:,-2),wf(:,72),wf(:,240))
  call vert_VQ_A(wf(:,47),wf(:,-2),wf(:,241))
  call prop_Q_A(wf(:,241),Q(:,39),MB,1_intkind1,wf(:,242))
  call vert_AV_Q(wf(:,-3),wf(:,47),wf(:,243))
  call prop_A_Q(wf(:,243),Q(:,43),MB,1_intkind1,wf(:,244))
  call vert_QA_V(wf(:,110),wf(:,-1),wf(:,245))
  call vert_VQ_A(wf(:,49),wf(:,-2),wf(:,246))
  call prop_Q_A(wf(:,246),Q(:,39),MB,1_intkind1,wf(:,247))
  call vert_AV_Q(wf(:,-3),wf(:,49),wf(:,248))
  call prop_A_Q(wf(:,248),Q(:,43),MB,1_intkind1,wf(:,249))
  call vert_QA_V(wf(:,0),wf(:,125),wf(:,250))
  call vert_VQ_A(wf(:,44),wf(:,0),wf(:,251))
  call prop_Q_A(wf(:,251),Q(:,45),ZERO,0_intkind1,wf(:,252))
  call vert_AV_Q(wf(:,-1),wf(:,44),wf(:,253))
  call prop_A_Q(wf(:,253),Q(:,46),ZERO,0_intkind1,wf(:,254))
  call vert_QA_V(wf(:,65),wf(:,-3),wf(:,255))
  call vert_VQ_A(wf(:,45),wf(:,0),wf(:,256))
  call prop_Q_A(wf(:,256),Q(:,45),ZERO,0_intkind1,wf(:,257))
  call vert_AV_Q(wf(:,-1),wf(:,45),wf(:,258))
  call prop_A_Q(wf(:,258),Q(:,46),ZERO,0_intkind1,wf(:,259))
  call vert_QA_V(wf(:,-2),wf(:,76),wf(:,260))
  call vert_QA_V(wf(:,23),wf(:,-3),wf(:,261))
  call vert_QA_V(wf(:,-2),wf(:,78),wf(:,262))

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
  den(9) = 1 / (Q(5,36) - MB2)
  den(10) = 1 / (Q(5,24) - MB2)
  den(13) = 1 / (Q(5,7) - MB2)
  den(18) = 1 / (Q(5,48) - MH2)
  den(20) = 1 / (Q(5,52) - MB2)
  den(23) = 1 / (Q(5,17))
  den(24) = 1 / (Q(5,34))
  den(25) = 1 / (Q(5,12))
  den(28) = 1 / (Q(5,14))
  den(31) = 1 / (Q(5,33))
  den(32) = 1 / (Q(5,18))
  den(35) = 1 / (Q(5,13))
  den(40) = 1 / (Q(5,19))
  den(47) = 1 / (Q(5,35))
  den(56) = 1 / (Q(5,28))
  den(61) = 1 / (Q(5,44))
  den(70) = 1 / (Q(5,56) - MB2)
  den(82) = 1 / (Q(5,49))
  den(91) = 1 / (Q(5,50))
  den(101) = 1 / (Q(5,60))
  den(105) = 1 / (Q(5,43) - MB2)
  den(108) = 1 / (Q(5,23) - MB2)
  den(120) = 1 / (Q(5,27) - MB2)
  den(123) = 1 / (Q(5,39) - MB2)
  den(143) = 1 / (Q(5,46))
  den(147) = 1 / (Q(5,29))
  den(150) = 1 / (Q(5,51))
  den(159) = 1 / (Q(5,30))
  den(163) = 1 / (Q(5,45))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(11) = den(1)*den(9)
  den(12) = den(10)*den(11)
  den(14) = den(1)*den(13)
  den(15) = den(10)*den(14)
  den(16) = den(7)*den(9)
  den(17) = den(3)*den(14)
  den(19) = den(14)*den(18)
  den(21) = den(18)*den(20)
  den(22) = den(1)*den(21)
  den(26) = den(23)*den(24)
  den(27) = den(25)*den(26)
  den(29) = den(25)*den(28)
  den(30) = den(23)*den(29)
  den(33) = den(31)*den(32)
  den(34) = den(25)*den(33)
  den(36) = den(25)*den(35)
  den(37) = den(32)*den(36)
  den(38) = den(29)*den(31)
  den(39) = den(24)*den(36)
  den(41) = den(23)*den(40)
  den(42) = den(9)*den(41)
  den(43) = den(3)*den(41)
  den(44) = den(32)*den(40)
  den(45) = den(9)*den(44)
  den(46) = den(3)*den(44)
  den(48) = den(31)*den(47)
  den(49) = den(2)*den(48)
  den(50) = den(24)*den(47)
  den(51) = den(2)*den(50)
  den(52) = den(10)*den(48)
  den(53) = den(10)*den(50)
  den(54) = den(1)*den(25)
  den(55) = den(18)*den(54)
  den(57) = den(2)*den(56)
  den(58) = den(1)*den(57)
  den(59) = den(10)*den(56)
  den(60) = den(1)*den(59)
  den(62) = den(9)*den(61)
  den(63) = den(1)*den(62)
  den(64) = den(3)*den(61)
  den(65) = den(1)*den(64)
  den(66) = den(2)*den(20)
  den(67) = den(1)*den(66)
  den(68) = den(9)*den(20)
  den(69) = den(1)*den(68)
  den(71) = den(10)*den(70)
  den(72) = den(1)*den(71)
  den(73) = den(3)*den(70)
  den(74) = den(1)*den(73)
  den(75) = den(7)*den(18)
  den(76) = den(18)*den(70)
  den(77) = den(1)*den(76)
  den(78) = den(25)*den(41)
  den(79) = den(25)*den(44)
  den(80) = den(25)*den(48)
  den(81) = den(25)*den(50)
  den(83) = den(23)*den(82)
  den(84) = den(25)*den(83)
  den(85) = den(31)*den(82)
  den(86) = den(25)*den(85)
  den(87) = den(23)*den(62)
  den(88) = den(23)*den(64)
  den(89) = den(31)*den(57)
  den(90) = den(31)*den(59)
  den(92) = den(32)*den(91)
  den(93) = den(25)*den(92)
  den(94) = den(24)*den(91)
  den(95) = den(25)*den(94)
  den(96) = den(32)*den(62)
  den(97) = den(32)*den(64)
  den(98) = den(24)*den(57)
  den(99) = den(24)*den(59)
  den(100) = den(2)*den(3)
  den(102) = den(100)*den(101)
  den(103) = den(1)*den(102)
  den(104) = den(1)*den(3)
  den(106) = den(104)*den(105)
  den(107) = den(2)*den(106)
  den(109) = den(4)*den(108)
  den(110) = den(3)*den(109)
  den(111) = den(1)**2
  den(112) = den(66)*den(111)
  den(113) = den(2)**2
  den(114) = den(7)*den(113)
  den(115) = den(7)*den(66)
  den(116) = den(9)*den(10)
  den(117) = den(101)*den(116)
  den(118) = den(1)*den(117)
  den(119) = den(1)*den(10)
  den(121) = den(119)*den(120)
  den(122) = den(9)*den(121)
  den(124) = den(11)*den(123)
  den(125) = den(10)*den(124)
  den(126) = den(71)*den(111)
  den(127) = den(14)*den(71)
  den(128) = den(10)**2
  den(129) = den(14)*den(128)
  den(130) = den(68)*den(111)
  den(131) = den(9)**2
  den(132) = den(7)*den(131)
  den(133) = den(7)*den(68)
  den(134) = den(73)*den(111)
  den(135) = den(14)*den(73)
  den(136) = den(3)**2
  den(137) = den(14)*den(136)
  den(138) = den(76)*den(111)
  den(139) = den(21)*den(111)
  den(140) = den(14)*den(76)
  den(141) = den(7)*den(21)
  den(142) = den(24)*den(25)
  den(144) = den(142)*den(143)
  den(145) = den(23)*den(144)
  den(146) = den(23)*den(25)
  den(148) = den(146)*den(147)
  den(149) = den(24)*den(148)
  den(151) = den(26)*den(150)
  den(152) = den(25)*den(151)
  den(153) = den(23)**2
  den(154) = den(29)*den(153)
  den(155) = den(25)**2
  den(156) = den(83)*den(155)
  den(157) = den(29)*den(83)
  den(158) = den(25)*den(32)
  den(160) = den(158)*den(159)
  den(161) = den(31)*den(160)
  den(162) = den(25)*den(31)
  den(164) = den(162)*den(163)
  den(165) = den(32)*den(164)
  den(166) = den(33)*den(150)
  den(167) = den(25)*den(166)
  den(168) = den(92)*den(155)
  den(169) = den(36)*den(92)
  den(170) = den(32)**2
  den(171) = den(36)*den(170)
  den(172) = den(31)**2
  den(173) = den(29)*den(172)
  den(174) = den(85)*den(155)
  den(175) = den(29)*den(85)
  den(176) = den(94)*den(155)
  den(177) = den(36)*den(94)
  den(178) = den(24)**2
  den(179) = den(36)*den(178)
  den(180) = den(62)*den(153)
  den(181) = den(41)*den(62)
  den(182) = den(41)*den(131)
  den(183) = den(64)*den(153)
  den(184) = den(41)*den(64)
  den(185) = den(41)*den(136)
  den(186) = den(62)*den(170)
  den(187) = den(44)*den(62)
  den(188) = den(44)*den(131)
  den(189) = den(64)*den(170)
  den(190) = den(44)*den(64)
  den(191) = den(44)*den(136)
  den(192) = den(57)*den(172)
  den(193) = den(48)*den(57)
  den(194) = den(48)*den(113)
  den(195) = den(57)*den(178)
  den(196) = den(50)*den(57)
  den(197) = den(50)*den(113)
  den(198) = den(59)*den(172)
  den(199) = den(48)*den(59)
  den(200) = den(48)*den(128)
  den(201) = den(59)*den(178)
  den(202) = den(50)*den(59)
  den(203) = den(50)*den(128)
  den(204) = den(14)*den(108)
  den(205) = den(14)*den(123)
  den(206) = den(7)*den(120)
  den(207) = den(7)*den(105)
  den(208) = den(36)*den(147)
  den(209) = den(36)*den(163)
  den(210) = den(29)*den(159)
  den(211) = den(29)*den(143)
  den(212) = den(41)*den(108)
  den(213) = den(41)*den(120)
  den(214) = den(83)*den(150)
  den(215) = den(44)*den(108)
  den(216) = den(44)*den(120)
  den(217) = den(92)*den(150)
  den(218) = den(57)*den(147)
  den(219) = den(57)*den(159)
  den(220) = den(66)*den(101)
  den(221) = den(59)*den(147)
  den(222) = den(59)*den(159)
  den(223) = den(71)*den(101)
  den(224) = den(48)*den(123)
  den(225) = den(48)*den(105)
  den(226) = den(85)*den(150)
  den(227) = den(50)*den(123)
  den(228) = den(50)*den(105)
  den(229) = den(94)*den(150)
  den(230) = den(62)*den(163)
  den(231) = den(62)*den(143)
  den(232) = den(68)*den(101)
  den(233) = den(64)*den(163)
  den(234) = den(64)*den(143)
  den(235) = den(73)*den(101)
  den(236) = den(21)*den(101)
  den(237) = den(76)*den(101)
  den(238) = den(1)*den(18)*den(25)
  den(239) = den(1)*den(2)*den(3)
  den(240) = den(1)*den(9)*den(10)
  den(241) = den(1)*den(18)
  den(242) = den(23)*den(24)*den(25)
  den(243) = den(25)*den(31)*den(32)
  den(244) = den(9)*den(23)
  den(245) = den(3)*den(23)
  den(246) = den(9)*den(32)
  den(247) = den(3)*den(32)
  den(248) = den(2)*den(31)
  den(249) = den(2)*den(24)
  den(250) = den(10)*den(31)
  den(251) = den(10)*den(24)
  den(252) = den(2)*den(207)
  den(253) = den(1)*den(220)
  den(254) = den(10)*den(205)
  den(255) = den(1)*den(223)
  den(256) = den(9)*den(206)
  den(257) = den(1)*den(232)
  den(258) = den(3)*den(204)
  den(259) = den(1)*den(235)
  den(260) = den(1)*den(236)
  den(261) = den(1)*den(237)
  den(262) = den(25)*den(214)
  den(263) = den(23)*den(211)
  den(264) = den(32)*den(209)
  den(265) = den(25)*den(217)
  den(266) = den(25)*den(226)
  den(267) = den(31)*den(210)
  den(268) = den(24)*den(208)
  den(269) = den(25)*den(229)
  den(270) = den(9)*den(213)
  den(271) = den(23)*den(231)
  den(272) = den(3)*den(212)
  den(273) = den(23)*den(234)
  den(274) = den(9)*den(216)
  den(275) = den(32)*den(230)
  den(276) = den(3)*den(215)
  den(277) = den(32)*den(233)
  den(278) = den(2)*den(225)
  den(279) = den(31)*den(219)
  den(280) = den(2)*den(228)
  den(281) = den(24)*den(218)
  den(282) = den(10)*den(224)
  den(283) = den(31)*den(222)
  den(284) = den(10)*den(227)
  den(285) = den(24)*den(221)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(182)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,13),wf(:,14)) * den(12)
  A(4) = cont_QA(wf(:,16),wf(:,17)) * den(15)
  A(5) = cont_QA(wf(:,9),wf(:,18)) * den(16)
  A(6) = cont_QA(wf(:,17),wf(:,19)) * den(17)
  A(7) = cont_QA(wf(:,17),wf(:,21)) * den(19)
  A(8) = cont_QA(wf(:,7),wf(:,23)) * den(22)
  A(9) = cont_VV(wf(:,26),wf(:,29)) * den(27)
  A(10) = cont_QA(wf(:,31),wf(:,32)) * den(30)
  A(11) = cont_VV(wf(:,26),wf(:,37)) * den(34)
  A(12) = cont_QA(wf(:,39),wf(:,40)) * den(37)
  A(13) = cont_QA(wf(:,32),wf(:,41)) * den(38)
  A(14) = cont_QA(wf(:,40),wf(:,42)) * den(39)
  A(15) = cont_VV(wf(:,43),wf(:,44)) * den(42)
  A(16) = cont_VV(wf(:,43),wf(:,45)) * den(43)
  A(17) = cont_VV(wf(:,44),wf(:,46)) * den(45)
  A(18) = cont_VV(wf(:,45),wf(:,46)) * den(46)
  A(19) = cont_VV(wf(:,47),wf(:,48)) * den(49)
  A(20) = cont_VV(wf(:,48),wf(:,49)) * den(51)
  A(21) = cont_VV(wf(:,47),wf(:,50)) * den(52)
  A(22) = cont_VV(wf(:,49),wf(:,50)) * den(53)

  A(23) = cont_VV(wf(:,26),wf(:,51)) * den(54)
  A(24) = cont_SS(wf(:,20),wf(:,52)) * den(55)
  A(25) = cont_QA(wf(:,5),wf(:,53)) * den(5)
  A(26) = cont_QA(wf(:,9),wf(:,54)) * den(8)
  A(27) = cont_VV(wf(:,48),wf(:,55)) * den(58)
  A(28) = cont_QA(wf(:,13),wf(:,56)) * den(12)
  A(29) = cont_QA(wf(:,17),wf(:,57)) * den(15)
  A(30) = cont_VV(wf(:,50),wf(:,55)) * den(60)
  A(31) = cont_QA(wf(:,9),wf(:,58)) * den(16)
  A(32) = cont_VV(wf(:,44),wf(:,59)) * den(63)
  A(33) = cont_QA(wf(:,17),wf(:,60)) * den(17)
  A(34) = cont_VV(wf(:,45),wf(:,59)) * den(65)
  A(35) = cont_QA(wf(:,61),wf(:,62)) * den(67)
  A(36) = cont_QA(wf(:,6),wf(:,64)) * den(5)
  A(37) = cont_QA(wf(:,61),wf(:,65)) * den(69)
  A(38) = cont_QA(wf(:,17),wf(:,66)) * den(19)
  A(39) = cont_QA(wf(:,23),wf(:,61)) * den(22)
  A(40) = cont_QA(wf(:,17),wf(:,67)) * den(17)
  A(41) = cont_QA(wf(:,14),wf(:,69)) * den(12)
  A(42) = cont_QA(wf(:,17),wf(:,70)) * den(15)
  A(43) = cont_QA(wf(:,71),wf(:,72)) * den(72)
  A(44) = cont_QA(wf(:,13),wf(:,75)) * den(12)
  A(45) = cont_QA(wf(:,71),wf(:,76)) * den(74)
  A(46) = cont_QA(wf(:,9),wf(:,77)) * den(75)
  A(47) = cont_QA(wf(:,71),wf(:,78)) * den(77)
  A(48) = cont_QA(wf(:,9),wf(:,79)) * den(16)
  A(49) = cont_QA(wf(:,5),wf(:,82)) * den(5)
  A(50) = cont_QA(wf(:,9),wf(:,83)) * den(8)
  A(51) = cont_VV(wf(:,26),wf(:,84)) * den(27)
  A(52) = cont_VV(wf(:,43),wf(:,85)) * den(78)
  A(53) = cont_QA(wf(:,32),wf(:,86)) * den(30)
  A(54) = cont_VV(wf(:,26),wf(:,87)) * den(34)
  A(55) = cont_VV(wf(:,46),wf(:,85)) * den(79)
  A(56) = cont_QA(wf(:,40),wf(:,88)) * den(37)
  A(57) = cont_VV(wf(:,47),wf(:,89)) * den(80)
  A(58) = cont_QA(wf(:,32),wf(:,90)) * den(38)
  A(59) = cont_VV(wf(:,49),wf(:,89)) * den(81)
  A(60) = cont_QA(wf(:,40),wf(:,91)) * den(39)
  A(61) = cont_VV(wf(:,43),wf(:,92)) * den(42)
  A(62) = cont_VV(wf(:,43),wf(:,93)) * den(43)
  A(63) = cont_VV(wf(:,46),wf(:,92)) * den(45)
  A(64) = cont_VV(wf(:,46),wf(:,93)) * den(46)
  A(65) = cont_VV(wf(:,47),wf(:,94)) * den(49)
  A(66) = cont_VV(wf(:,49),wf(:,94)) * den(51)
  A(67) = cont_VV(wf(:,47),wf(:,95)) * den(52)
  A(68) = cont_VV(wf(:,49),wf(:,95)) * den(53)
  A(69) = cont_VV(wf(:,43),wf(:,96)) * den(43)
  A(70) = cont_VV(wf(:,43),wf(:,97)) * den(42)
  A(71) = cont_VV(wf(:,46),wf(:,96)) * den(46)
  A(72) = cont_VV(wf(:,46),wf(:,97)) * den(45)
  A(73) = cont_VV(wf(:,47),wf(:,98)) * den(52)
  A(74) = cont_VV(wf(:,49),wf(:,98)) * den(53)
  A(75) = cont_VV(wf(:,47),wf(:,99)) * den(49)
  A(76) = cont_VV(wf(:,49),wf(:,99)) * den(51)
  A(77) = cont_VV(wf(:,29),wf(:,100)) * den(27)
  A(78) = cont_QA(wf(:,31),wf(:,102)) * den(30)
  A(79) = cont_VV(wf(:,37),wf(:,100)) * den(34)
  A(80) = cont_QA(wf(:,39),wf(:,104)) * den(37)
  A(81) = cont_QA(wf(:,41),wf(:,102)) * den(38)
  A(82) = cont_QA(wf(:,42),wf(:,104)) * den(39)
  A(83) = cont_QA(wf(:,105),wf(:,106)) * den(84)
  A(84) = cont_VV(wf(:,26),wf(:,109)) * den(27)
  A(85) = cont_QA(wf(:,105),wf(:,110)) * den(86)
  A(86) = cont_QA(wf(:,40),wf(:,111)) * den(39)
  A(87) = cont_VV(wf(:,26),wf(:,114)) * den(34)
  A(88) = cont_QA(wf(:,40),wf(:,115)) * den(37)
  A(89) = cont_VV(wf(:,44),wf(:,116)) * den(87)
  A(90) = cont_VV(wf(:,45),wf(:,116)) * den(88)
  A(91) = cont_VV(wf(:,48),wf(:,117)) * den(89)
  A(92) = cont_VV(wf(:,48),wf(:,118)) * den(51)
  A(93) = cont_VV(wf(:,50),wf(:,117)) * den(90)
  A(94) = cont_VV(wf(:,50),wf(:,118)) * den(53)
  A(95) = cont_VV(wf(:,44),wf(:,119)) * den(45)
  A(96) = cont_VV(wf(:,45),wf(:,119)) * den(46)
  A(97) = cont_QA(wf(:,120),wf(:,121)) * den(93)
  A(98) = cont_VV(wf(:,26),wf(:,124)) * den(34)
  A(99) = cont_QA(wf(:,120),wf(:,125)) * den(95)
  A(100) = cont_QA(wf(:,32),wf(:,126)) * den(38)
  A(101) = cont_VV(wf(:,26),wf(:,129)) * den(27)
  A(102) = cont_QA(wf(:,32),wf(:,130)) * den(30)
  A(103) = cont_VV(wf(:,44),wf(:,131)) * den(96)
  A(104) = cont_VV(wf(:,45),wf(:,131)) * den(97)
  A(105) = cont_VV(wf(:,48),wf(:,132)) * den(98)
  A(106) = cont_VV(wf(:,48),wf(:,133)) * den(49)
  A(107) = cont_VV(wf(:,50),wf(:,132)) * den(99)
  A(108) = cont_VV(wf(:,50),wf(:,133)) * den(52)
  A(109) = cont_VV(wf(:,44),wf(:,134)) * den(42)
  A(110) = cont_VV(wf(:,45),wf(:,134)) * den(43)
  A(111) = cont_QA(wf(:,5),wf(:,136)) * den(5)
  A(112) = cont_QA(wf(:,8),wf(:,138)) * den(8)
  A(113) = cont_QA(wf(:,13),wf(:,139)) * den(12)
  A(114) = cont_QA(wf(:,16),wf(:,141)) * den(15)
  A(115) = cont_QA(wf(:,18),wf(:,138)) * den(16)
  A(116) = cont_QA(wf(:,19),wf(:,141)) * den(17)
  A(117) = cont_QA(wf(:,21),wf(:,141)) * den(19)
  A(118) = cont_QA(wf(:,23),wf(:,137)) * den(22)
  A(119) = cont_VV(wf(:,142),wf(:,143)) * den(103)
  A(120) = cont_QA(wf(:,145),wf(:,146)) * den(107)
  A(121) = cont_QA(wf(:,147),wf(:,148)) * den(110)
  A(122) = cont_QA(wf(:,62),wf(:,149)) * den(112)
  A(123) = cont_QA(wf(:,9),wf(:,151)) * den(114)
  A(124) = cont_QA(wf(:,62),wf(:,152)) * den(115)
  A(125) = cont_VV(wf(:,143),wf(:,153)) * den(118)
  A(126) = cont_QA(wf(:,155),wf(:,156)) * den(122)
  A(127) = cont_QA(wf(:,157),wf(:,158)) * den(125)
  A(128) = cont_QA(wf(:,72),wf(:,159)) * den(126)
  A(129) = cont_QA(wf(:,72),wf(:,160)) * den(127)
  A(130) = cont_QA(wf(:,17),wf(:,162)) * den(129)
  A(131) = cont_QA(wf(:,65),wf(:,149)) * den(130)
  A(132) = cont_QA(wf(:,9),wf(:,164)) * den(132)
  A(133) = cont_QA(wf(:,65),wf(:,152)) * den(133)
  A(134) = cont_QA(wf(:,76),wf(:,159)) * den(134)
  A(135) = cont_QA(wf(:,76),wf(:,160)) * den(135)
  A(136) = cont_QA(wf(:,17),wf(:,166)) * den(137)
  A(137) = cont_QA(wf(:,78),wf(:,159)) * den(138)
  A(138) = cont_QA(wf(:,23),wf(:,149)) * den(139)
  A(139) = cont_QA(wf(:,78),wf(:,160)) * den(140)
  A(140) = cont_QA(wf(:,9),wf(:,167)) * den(141)
  A(141) = cont_QA(wf(:,169),wf(:,170)) * den(145)
  A(142) = cont_QA(wf(:,172),wf(:,173)) * den(149)
  A(143) = cont_VV(wf(:,29),wf(:,174)) * den(152)
  A(144) = cont_QA(wf(:,32),wf(:,176)) * den(154)
  A(145) = cont_QA(wf(:,106),wf(:,177)) * den(156)
  A(146) = cont_QA(wf(:,106),wf(:,178)) * den(157)
  A(147) = cont_QA(wf(:,180),wf(:,181)) * den(161)
  A(148) = cont_QA(wf(:,183),wf(:,184)) * den(165)
  A(149) = cont_VV(wf(:,37),wf(:,174)) * den(167)
  A(150) = cont_QA(wf(:,121),wf(:,185)) * den(168)
  A(151) = cont_QA(wf(:,121),wf(:,186)) * den(169)
  A(152) = cont_QA(wf(:,40),wf(:,188)) * den(171)
  A(153) = cont_QA(wf(:,32),wf(:,190)) * den(173)
  A(154) = cont_QA(wf(:,110),wf(:,177)) * den(174)
  A(155) = cont_QA(wf(:,110),wf(:,178)) * den(175)
  A(156) = cont_QA(wf(:,125),wf(:,185)) * den(176)
  A(157) = cont_QA(wf(:,125),wf(:,186)) * den(177)
  A(158) = cont_QA(wf(:,40),wf(:,192)) * den(179)
  A(159) = cont_VV(wf(:,44),wf(:,193)) * den(180)
  A(160) = cont_VV(wf(:,44),wf(:,194)) * den(181)
  A(161) = cont_VV(wf(:,43),wf(:,195)) * den(182)
  A(162) = cont_VV(wf(:,45),wf(:,193)) * den(183)
  A(163) = cont_VV(wf(:,45),wf(:,194)) * den(184)
  A(164) = cont_VV(wf(:,43),wf(:,196)) * den(185)
  A(165) = cont_VV(wf(:,44),wf(:,197)) * den(186)
  A(166) = cont_VV(wf(:,44),wf(:,198)) * den(187)
  A(167) = cont_VV(wf(:,46),wf(:,195)) * den(188)
  A(168) = cont_VV(wf(:,45),wf(:,197)) * den(189)
  A(169) = cont_VV(wf(:,45),wf(:,198)) * den(190)
  A(170) = cont_VV(wf(:,46),wf(:,196)) * den(191)
  A(171) = cont_VV(wf(:,48),wf(:,199)) * den(192)
  A(172) = cont_VV(wf(:,48),wf(:,200)) * den(193)
  A(173) = cont_VV(wf(:,47),wf(:,201)) * den(194)
  A(174) = cont_VV(wf(:,48),wf(:,202)) * den(195)
  A(175) = cont_VV(wf(:,48),wf(:,203)) * den(196)
  A(176) = cont_VV(wf(:,49),wf(:,201)) * den(197)
  A(177) = cont_VV(wf(:,50),wf(:,199)) * den(198)
  A(178) = cont_VV(wf(:,50),wf(:,200)) * den(199)
  A(179) = cont_VV(wf(:,47),wf(:,204)) * den(200)
  A(180) = cont_VV(wf(:,50),wf(:,202)) * den(201)
  A(181) = cont_VV(wf(:,50),wf(:,203)) * den(202)
  A(182) = cont_VV(wf(:,49),wf(:,204)) * den(203)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(182)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(1)+A(2)+A(3)+A(4)+A(5)+A(6)+A(9)+A(10)+A(11)+A(12)+A(13)+A(14)+A(15)+A(16)+A(17)+A(18)+A(19)+A(20)+A(21) &
       +A(22))*f(1))/2._/**/REALKIND+((-A(7)-A(8))*f(9))/2._/**/REALKIND
  M1(2) = ((-A(1)-A(2)-A(3)-A(4)-A(5)-A(6)-A(9)-A(10)-A(11)-A(12)-A(13)-A(14)-A(15)-A(16)-A(17)-A(18)-A(19)-A(20)-A(21) &
       -A(22))*f(1))/6._/**/REALKIND+((A(7)+A(8))*f(9))/6._/**/REALKIND

  M2(1) = ((-A(119)-A(120)-A(121)-A(122)-A(123)-A(124)-A(125)-A(126)-A(127)-A(128)-A(129)-A(130)-A(131)-A(132)-A(133)-A(134) &
       -A(135)-A(136)-A(141)-A(142)-A(143)-A(144)-A(145)-A(146)-A(147)-A(148)-A(149)-A(150)-A(151)-A(152)-A(153)-A(154)-A(155) &
       -A(156)-A(157)-A(158)-A(159)-A(160)-A(161)-A(162)-A(163)-A(164)-A(165)-A(166)-A(167)-A(168)-A(169)-A(170)-A(171)-A(172) &
       -A(173)-A(174)-A(175)-A(176)-A(177)-A(178)-A(179)-A(180)-A(181)-A(182))*f(2))/2._/**/REALKIND+((A(25)+A(28)+A(35)+A(37) &
       +A(43)+A(45)+A(61)+A(63)+A(65)+A(66)+A(69)+A(71)+A(73)+A(74)+A(77)+A(78)+A(79)+A(80)+A(81)+A(82))*f(3))/2._/**/REALKIND &
       +((A(51)+A(54)+A(83)+A(85)+A(89)+A(90)+A(91)+A(93)+A(97)+A(99)+A(103)+A(104)+A(105)+A(107)+A(111)+A(112)+A(113)+A(114) &
       +A(115)+A(116))*f(4))/2._/**/REALKIND+((A(26)+A(29)+A(31)+A(33)+A(36)+A(40)+A(41)+A(42)+A(44)+A(48)+A(49)+A(50)+A(62)+A(64) &
       +A(67)+A(68)+A(70)+A(72)+A(75)+A(76))*f(5))/2._/**/REALKIND+((A(53)+A(56)+A(58)+A(60)+A(84)+A(86)+A(87)+A(88)+A(92)+A(94) &
       +A(95)+A(96)+A(98)+A(100)+A(101)+A(102)+A(106)+A(108)+A(109)+A(110))*f(6))/2._/**/REALKIND+((-A(27)-A(30)-A(32)-A(34)-A(52) &
       -A(55)-A(57)-A(59))*f(7))/2._/**/REALKIND+(A(23)*f(8))/2._/**/REALKIND+((A(137)+A(138)+A(139) &
       +A(140))*f(10))/2._/**/REALKIND+((-A(39)-A(47))*f(11))/2._/**/REALKIND+((-A(117)-A(118))*f(12))/2._/**/REALKIND+((-A(38) &
       -A(46))*f(13))/2._/**/REALKIND-(A(24)*f(14))/2._/**/REALKIND
  M2(2) = ((A(119)+A(120)+A(121)+A(122)+A(123)+A(124)+A(125)+A(126)+A(127)+A(128)+A(129)+A(130)+A(131)+A(132)+A(133)+A(134)+A(135) &
       +A(136)+A(141)+A(142)+A(143)+A(144)+A(145)+A(146)+A(147)+A(148)+A(149)+A(150)+A(151)+A(152)+A(153)+A(154)+A(155)+A(156) &
       +A(157)+A(158)+A(159)+A(160)+A(161)+A(162)+A(163)+A(164)+A(165)+A(166)+A(167)+A(168)+A(169)+A(170)+A(171)+A(172)+A(173) &
       +A(174)+A(175)+A(176)+A(177)+A(178)+A(179)+A(180)+A(181)+A(182))*f(2))/6._/**/REALKIND+((-A(25)-A(28)-A(35)-A(37)-A(43) &
       -A(45)-A(61)-A(63)-A(65)-A(66)-A(69)-A(71)-A(73)-A(74)-A(77)-A(78)-A(79)-A(80)-A(81)-A(82))*f(3))/6._/**/REALKIND+((-A(51) &
       -A(54)-A(83)-A(85)-A(89)-A(90)-A(91)-A(93)-A(97)-A(99)-A(103)-A(104)-A(105)-A(107)-A(111)-A(112)-A(113)-A(114)-A(115) &
       -A(116))*f(4))/6._/**/REALKIND+((-A(26)-A(29)-A(31)-A(33)-A(36)-A(40)-A(41)-A(42)-A(44)-A(48)-A(49)-A(50)-A(62)-A(64)-A(67) &
       -A(68)-A(70)-A(72)-A(75)-A(76))*f(5))/6._/**/REALKIND+((-A(53)-A(56)-A(58)-A(60)-A(84)-A(86)-A(87)-A(88)-A(92)-A(94)-A(95) &
       -A(96)-A(98)-A(100)-A(101)-A(102)-A(106)-A(108)-A(109)-A(110))*f(6))/6._/**/REALKIND+((A(27)+A(30)+A(32)+A(34)+A(52)+A(55) &
       +A(57)+A(59))*f(7))/6._/**/REALKIND-(A(23)*f(8))/6._/**/REALKIND+((-A(137)-A(138)-A(139)-A(140))*f(10))/6._/**/REALKIND &
       +((A(39)+A(47))*f(11))/6._/**/REALKIND+((A(117)+A(118))*f(12))/6._/**/REALKIND+((A(38)+A(46))*f(13))/6._/**/REALKIND &
       +(A(24)*f(14))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppzzjj_uuxbbxzz_1_/**/REALKIND
