
module ol_colourmatrix_pplnjjj_neexuxdddxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(120,4), K2(4,4), KL(4,4)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1(  1,:) = [  36,  12,  12,   0]
  K1(  2,:) = [  12,  36,   0,  12]
  K1(  3,:) = [  12,   0,  36,  12]
  K1(  4,:) = [   0,  12,  12,  36]
  K1(  5,:) = [   0,   0,   0,   0]
  K1(  6,:) = [   0,   0,   0,   0]
  K1(  7,:) = [   0,   0,   0,   0]
  K1(  8,:) = [   0,   0,   0,   0]
  K1(  9,:) = [   0,   0,   0,   0]
  K1( 10,:) = [   0,   0,   0,   0]
  K1( 11,:) = [   0,   0,   0,   0]
  K1( 12,:) = [   0,   0,   0,   0]
  K1( 13,:) = [   0,   0,   0,   0]
  K1( 14,:) = [   0,   0,   0,   0]
  K1( 15,:) = [   0,   0,   0,   0]
  K1( 16,:) = [   0,   0,   0,   0]
  K1( 17,:) = [   0,   0,   0,   0]
  K1( 18,:) = [   0,   0,   0,   0]
  K1( 19,:) = [   0,   0,   0,   0]
  K1( 20,:) = [   0,   0,   0,   0]
  K1( 21,:) = [   0,   0,   0,   0]
  K1( 22,:) = [   0,   0,   0,   0]
  K1( 23,:) = [   0,   0,   0,   0]
  K1( 24,:) = [   0,   0,   0,   0]
  K1( 25,:) = [  48,  16,  16,   0]
  K1( 26,:) = [  16,  48,   0,  16]
  K1( 27,:) = [  16,   0,  48,  16]
  K1( 28,:) = [   0,  16,  16,  48]
  K1( 29,:) = [   0,   0,   0,   0]
  K1( 30,:) = [   0,   0,   0,   0]
  K1( 31,:) = [   0,   0,   0,   0]
  K1( 32,:) = [   0,   0,   0,   0]
  K1( 33,:) = [   0,   0,   0,   0]
  K1( 34,:) = [   0,   0,   0,   0]
  K1( 35,:) = [   0,   0,   0,   0]
  K1( 36,:) = [   0,   0,   0,   0]
  K1( 37,:) = [   6,   2,   2,   0]
  K1( 38,:) = [   2,   0,  -6, -16]
  K1( 39,:) = [   2,  -6,   0, -16]
  K1( 40,:) = [   0, -16, -16, -48]
  K1( 41,:) = [  48,  16,  16,   0]
  K1( 42,:) = [  16,  48,   0,  16]
  K1( 43,:) = [  16,   0,  48,  16]
  K1( 44,:) = [   0,  16,  16,  48]
  K1( 45,:) = [   0,   0,   0,   0]
  K1( 46,:) = [   0,   0,   0,   0]
  K1( 47,:) = [   0,   0,   0,   0]
  K1( 48,:) = [   0,   0,   0,   0]
  K1( 49,:) = [   0,   0,   0,   0]
  K1( 50,:) = [   0,   0,   0,   0]
  K1( 51,:) = [   0,   0,   0,   0]
  K1( 52,:) = [   0,   0,   0,   0]
  K1( 53,:) = [   0,   2, -16,  -6]
  K1( 54,:) = [   2,   6,   0,   2]
  K1( 55,:) = [ -16,   0, -48, -16]
  K1( 56,:) = [  -6,   2, -16,   0]
  K1( 57,:) = [   0,  16,  -2,   6]
  K1( 58,:) = [  16,   0,   6,  -2]
  K1( 59,:) = [  -2,   6,   0,  16]
  K1( 60,:) = [   6,  -2,  16,   0]
  K1( 61,:) = [  48,  16,  16,   0]
  K1( 62,:) = [  16,  48,   0,  16]
  K1( 63,:) = [  16,   0,  48,  16]
  K1( 64,:) = [   0,  16,  16,  48]
  K1( 65,:) = [   0,   0,   0,   0]
  K1( 66,:) = [   0,   0,   0,   0]
  K1( 67,:) = [   0,   0,   0,   0]
  K1( 68,:) = [   0,   0,   0,   0]
  K1( 69,:) = [   0,   0,   0,   0]
  K1( 70,:) = [   0,   0,   0,   0]
  K1( 71,:) = [   0,   0,   0,   0]
  K1( 72,:) = [   0,   0,   0,   0]
  K1( 73,:) = [   0,  -2,  16,   6]
  K1( 74,:) = [  -2,   0,   6,  16]
  K1( 75,:) = [  16,   6,   0,  -2]
  K1( 76,:) = [   6,  16,  -2,   0]
  K1( 77,:) = [   0, -16,   2,  -6]
  K1( 78,:) = [ -16, -48,   0, -16]
  K1( 79,:) = [   2,   0,   6,   2]
  K1( 80,:) = [  -6, -16,   2,   0]
  K1( 81,:) = [ -48, -16, -16,   0]
  K1( 82,:) = [ -16,   0,  -6,   2]
  K1( 83,:) = [ -16,  -6,   0,   2]
  K1( 84,:) = [   0,   2,   2,   6]
  K1( 85,:) = [  48,  16,  16,   0]
  K1( 86,:) = [  16,  48,   0,  16]
  K1( 87,:) = [  16,   0,  48,  16]
  K1( 88,:) = [   0,  16,  16,  48]
  K1( 89,:) = [   0,   0,   0,   0]
  K1( 90,:) = [   0,   0,   0,   0]
  K1( 91,:) = [   0,   0,   0,   0]
  K1( 92,:) = [   0,   0,   0,   0]
  K1( 93,:) = [   0,   0,   0,   0]
  K1( 94,:) = [   0,   0,   0,   0]
  K1( 95,:) = [   0,   0,   0,   0]
  K1( 96,:) = [   0,   0,   0,   0]
  K1( 97,:) = [ -54, -18, -18,   0]
  K1( 98,:) = [ -18, -54,   0, -18]
  K1( 99,:) = [ -18,   0,   0,  18]
  K1(100,:) = [   0, -18,  18,   0]
  K1(101,:) = [ -54, -18, -18,   0]
  K1(102,:) = [ -18,   0,   0,  18]
  K1(103,:) = [ -18,   0, -54, -18]
  K1(104,:) = [   0,  18, -18,   0]
  K1(105,:) = [   0, -18,  18,   0]
  K1(106,:) = [ -18, -54,   0, -18]
  K1(107,:) = [  18,   0,   0, -18]
  K1(108,:) = [   0, -18, -18, -54]
  K1(109,:) = [   0,  18, -18,   0]
  K1(110,:) = [  18,   0,   0, -18]
  K1(111,:) = [ -18,   0, -54, -18]
  K1(112,:) = [   0, -18, -18, -54]
  K1(113,:) = [ 108,  36,  36,   0]
  K1(114,:) = [  36, 108,   0,  36]
  K1(115,:) = [  36,   0, 108,  36]
  K1(116,:) = [   0,  36,  36, 108]
  K1(117,:) = [   0,   0,   0,   0]
  K1(118,:) = [   0,   0,   0,   0]
  K1(119,:) = [   0,   0,   0,   0]
  K1(120,:) = [   0,   0,   0,   0]
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
end module ol_colourmatrix_pplnjjj_neexuxdddxg_1_/**/REALKIND



module ol_forced_parameters_pplnjjj_neexuxdddxg_1_/**/REALKIND
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
end module ol_forced_parameters_pplnjjj_neexuxdddxg_1_/**/REALKIND

module ol_loop_pplnjjj_neexuxdddxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(17), c(26)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:265)
  ! denominators
  complex(REALKIND), save :: den(339)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(4,128)
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
    f( 1) = (CI*eQED**2*gQCD**3)/(2._/**/REALKIND*sw**2)
    f( 2) = (eQED**2*gQCD**3)/(sw**2*2._/**/REALKIND)
    f( 3) = (CI*countertermnorm*eQED**2*gQCD**5)/(2._/**/REALKIND*sw**2)
    f( 4) = (countertermnorm*eQED**2*gQCD**5)/(sw**2*2._/**/REALKIND)
    f( 5) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**5)/(2._/**/REALKIND*sw**2)
    f( 6) = (countertermnorm*ctGqq*eQED**2*gQCD**5)/(sw**2*2._/**/REALKIND)
    f( 7) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**5)/(2._/**/REALKIND*sw**2)
    f( 8) = (countertermnorm*ctVqq*eQED**2*gQCD**5)/(sw**2*2._/**/REALKIND)
    f( 9) = (countertermnorm*ctVVV*eQED**2*gQCD**5)/(sw**2*2._/**/REALKIND)
    f(10) = (CI*eQED**2*gQCD**5*integralnorm*SwB)/(4._/**/REALKIND*sw**2)
    f(11) = (CI*eQED**2*gQCD**5*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(12) = (eQED**2*gQCD**5*integralnorm*SwB)/(sw**2*4._/**/REALKIND)
    f(13) = (eQED**2*gQCD**5*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(14) = (CI*eQED**2*gQCD**5*integralnorm*SwF)/(2._/**/REALKIND*sw**2)
    f(15) = (CI*eQED**2*gQCD**5*integralnorm*SwF)/sw**2
    f(16) = (eQED**2*gQCD**5*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(17) = (eQED**2*gQCD**5*integralnorm*SwF)/sw**2

  c = [ 27*CI*f(10), 54*CI*f(10), 3*CI*f(11), 9*CI*f(11), 24*CI*f(11), 27*CI*f(11), 54*CI*f(11), 18*f(12), 54*f(12), f(13) &
    , 3*f(13), 6*f(13), 8*f(13), 9*f(13), 10*f(13), 18*f(13), 21*f(13), 24*f(13), 27*f(13), 54*f(13), 9*CI*f(14), 9*CI*f(15) &
    , 3*f(16), 9*f(16), 3*f(17), 9*f(17) ]
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
  complex(REALKIND), intent(out) :: M1(4), M2(4)
  complex(REALKIND) :: A(192)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_Q(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_W(wf(:,0),wf(:,-1),wf(:,1))
  call vert_AV_Q(wf(:,-2),wf(:,-6),wf(:,2))
  call vert_QA_V(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MW,1_intkind1,wf(:,4))
  call prop_A_Q(wf(:,2),Q(:,68),ZERO,0_intkind1,wf(:,5))
  call vert_WQ_A(wf(:,4),wf(:,-4),wf(:,6))
  call vert_AV_Q(wf(:,5),wf(:,3),wf(:,7))
  call prop_Q_A(wf(:,6),Q(:,19),ZERO,0_intkind1,wf(:,8))
  call vert_VQ_A(wf(:,3),wf(:,-4),wf(:,9))
  call vert_AW_Q(wf(:,5),wf(:,4),wf(:,10))
  call prop_Q_A(wf(:,9),Q(:,56),ZERO,0_intkind1,wf(:,11))
  call vert_VQ_A(wf(:,-6),wf(:,-4),wf(:,12))
  call prop_Q_A(wf(:,12),Q(:,80),ZERO,0_intkind1,wf(:,13))
  call vert_AW_Q(wf(:,-2),wf(:,4),wf(:,14))
  call vert_VQ_A(wf(:,3),wf(:,13),wf(:,15))
  call prop_A_Q(wf(:,14),Q(:,7),ZERO,0_intkind1,wf(:,16))
  call vert_AV_Q(wf(:,-2),wf(:,3),wf(:,17))
  call vert_WQ_A(wf(:,4),wf(:,13),wf(:,18))
  call prop_A_Q(wf(:,17),Q(:,44),ZERO,0_intkind1,wf(:,19))
  call vert_UV_W(wf(:,3),Q(:,40),wf(:,-6),Q(:,64),wf(:,20))
  call vert_QA_V(wf(:,-4),wf(:,16),wf(:,21))
  call vert_AV_Q(wf(:,16),wf(:,-6),wf(:,22))
  call vert_AV_Q(wf(:,19),wf(:,-6),wf(:,23))
  call vert_QA_V(wf(:,8),wf(:,-2),wf(:,24))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,25))
  call vert_WQ_A(wf(:,4),wf(:,-3),wf(:,26))
  call vert_AV_Q(wf(:,5),wf(:,25),wf(:,27))
  call prop_Q_A(wf(:,26),Q(:,11),ZERO,0_intkind1,wf(:,28))
  call vert_VQ_A(wf(:,25),wf(:,-3),wf(:,29))
  call prop_Q_A(wf(:,29),Q(:,56),ZERO,0_intkind1,wf(:,30))
  call vert_VQ_A(wf(:,-6),wf(:,-3),wf(:,31))
  call prop_Q_A(wf(:,31),Q(:,72),ZERO,0_intkind1,wf(:,32))
  call vert_VQ_A(wf(:,25),wf(:,32),wf(:,33))
  call vert_AV_Q(wf(:,-2),wf(:,25),wf(:,34))
  call vert_WQ_A(wf(:,4),wf(:,32),wf(:,35))
  call prop_A_Q(wf(:,34),Q(:,52),ZERO,0_intkind1,wf(:,36))
  call vert_UV_W(wf(:,25),Q(:,48),wf(:,-6),Q(:,64),wf(:,37))
  call vert_QA_V(wf(:,-3),wf(:,16),wf(:,38))
  call vert_QA_V(wf(:,28),wf(:,-2),wf(:,39))
  call vert_AV_Q(wf(:,36),wf(:,-6),wf(:,40))
  call vert_QA_V(wf(:,32),wf(:,-5),wf(:,41))
  call vert_QA_V(wf(:,13),wf(:,-5),wf(:,42))
  call vert_AV_Q(wf(:,-5),wf(:,-6),wf(:,43))
  call prop_A_Q(wf(:,43),Q(:,96),ZERO,0_intkind1,wf(:,44))
  call vert_QA_V(wf(:,-4),wf(:,44),wf(:,45))
  call vert_QA_V(wf(:,-3),wf(:,44),wf(:,46))
  call vert_AV_Q(wf(:,-2),wf(:,46),wf(:,47))
  call counter_AV_Q(wf(:,5),wf(:,3),wf(:,48))
  call counter_AW_Q(wf(:,5),wf(:,4),wf(:,49))
  call counter_VQ_A(wf(:,3),wf(:,13),wf(:,50))
  call counter_WQ_A(wf(:,4),wf(:,13),wf(:,51))
  call counter_UV_W(wf(:,3),Q(:,40),wf(:,-6),Q(:,64),wf(:,52))
  call counter_AV_Q(wf(:,16),wf(:,-6),wf(:,53))
  call counter_AV_Q(wf(:,19),wf(:,-6),wf(:,54))
  call counter_AV_Q(wf(:,5),wf(:,25),wf(:,55))
  call counter_VQ_A(wf(:,25),wf(:,32),wf(:,56))
  call counter_WQ_A(wf(:,4),wf(:,32),wf(:,57))
  call counter_UV_W(wf(:,25),Q(:,48),wf(:,-6),Q(:,64),wf(:,58))
  call counter_AV_Q(wf(:,36),wf(:,-6),wf(:,59))
  call counter_QA_V(wf(:,32),wf(:,-5),wf(:,60))
  call counter_QA_V(wf(:,13),wf(:,-5),wf(:,61))
  call counter_AV_Q(wf(:,-5),wf(:,-6),wf(:,62))
  call prop_A_Q(wf(:,62),Q(:,96),ZERO,0_intkind1,wf(:,63))
  call vert_QA_V(wf(:,-4),wf(:,63),wf(:,64))
  call vert_QA_V(wf(:,-3),wf(:,63),wf(:,65))
  call vert_AV_Q(wf(:,-2),wf(:,65),wf(:,66))
  call counter_VQ_A(wf(:,3),wf(:,-4),wf(:,67))
  call prop_A_Q(wf(:,10),Q(:,71),ZERO,0_intkind1,wf(:,68))
  call counter_WQ_A(wf(:,4),wf(:,-4),wf(:,69))
  call prop_A_Q(wf(:,7),Q(:,108),ZERO,0_intkind1,wf(:,70))
  call prop_Q_A(wf(:,67),Q(:,56),ZERO,0_intkind1,wf(:,71))
  call counter_QA_V(wf(:,-4),wf(:,16),wf(:,72))
  call prop_Q_A(wf(:,69),Q(:,19),ZERO,0_intkind1,wf(:,73))
  call vert_QA_V(wf(:,73),wf(:,-2),wf(:,74))
  call counter_VQ_A(wf(:,-6),wf(:,-4),wf(:,75))
  call prop_Q_A(wf(:,75),Q(:,80),ZERO,0_intkind1,wf(:,76))
  call vert_VQ_A(wf(:,3),wf(:,76),wf(:,77))
  call vert_WQ_A(wf(:,4),wf(:,76),wf(:,78))
  call counter_QA_V(wf(:,-4),wf(:,44),wf(:,79))
  call vert_QA_V(wf(:,76),wf(:,-5),wf(:,80))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,81))
  call vert_AV_Q(wf(:,5),wf(:,81),wf(:,82))
  call vert_VQ_A(wf(:,81),wf(:,-3),wf(:,83))
  call prop_Q_A(wf(:,83),Q(:,56),ZERO,0_intkind1,wf(:,84))
  call vert_VQ_A(wf(:,81),wf(:,32),wf(:,85))
  call vert_AV_Q(wf(:,-2),wf(:,81),wf(:,86))
  call prop_A_Q(wf(:,86),Q(:,52),ZERO,0_intkind1,wf(:,87))
  call vert_UV_W(wf(:,81),Q(:,48),wf(:,-6),Q(:,64),wf(:,88))
  call vert_AV_Q(wf(:,87),wf(:,-6),wf(:,89))
  call counter_VQ_A(wf(:,25),wf(:,-3),wf(:,90))
  call counter_WQ_A(wf(:,4),wf(:,-3),wf(:,91))
  call prop_A_Q(wf(:,27),Q(:,116),ZERO,0_intkind1,wf(:,92))
  call prop_Q_A(wf(:,90),Q(:,56),ZERO,0_intkind1,wf(:,93))
  call counter_QA_V(wf(:,-3),wf(:,16),wf(:,94))
  call prop_Q_A(wf(:,91),Q(:,11),ZERO,0_intkind1,wf(:,95))
  call vert_QA_V(wf(:,95),wf(:,-2),wf(:,96))
  call counter_VQ_A(wf(:,-6),wf(:,-3),wf(:,97))
  call prop_Q_A(wf(:,97),Q(:,72),ZERO,0_intkind1,wf(:,98))
  call vert_VQ_A(wf(:,25),wf(:,98),wf(:,99))
  call vert_WQ_A(wf(:,4),wf(:,98),wf(:,100))
  call counter_QA_V(wf(:,-3),wf(:,44),wf(:,101))
  call vert_AV_Q(wf(:,-2),wf(:,101),wf(:,102))
  call vert_QA_V(wf(:,98),wf(:,-5),wf(:,103))
  call counter_QA_V(wf(:,-3),wf(:,-5),wf(:,104))
  call vert_AV_Q(wf(:,5),wf(:,104),wf(:,105))
  call vert_VQ_A(wf(:,104),wf(:,-4),wf(:,106))
  call prop_Q_A(wf(:,106),Q(:,56),ZERO,0_intkind1,wf(:,107))
  call vert_VQ_A(wf(:,104),wf(:,13),wf(:,108))
  call vert_AV_Q(wf(:,-2),wf(:,104),wf(:,109))
  call prop_A_Q(wf(:,109),Q(:,44),ZERO,0_intkind1,wf(:,110))
  call vert_UV_W(wf(:,104),Q(:,40),wf(:,-6),Q(:,64),wf(:,111))
  call vert_AV_Q(wf(:,110),wf(:,-6),wf(:,112))
  call counter_AV_Q(wf(:,-2),wf(:,3),wf(:,113))
  call prop_Q_A(wf(:,18),Q(:,83),ZERO,0_intkind1,wf(:,114))
  call counter_AW_Q(wf(:,-2),wf(:,4),wf(:,115))
  call prop_Q_A(wf(:,15),Q(:,120),ZERO,0_intkind1,wf(:,116))
  call prop_A_Q(wf(:,113),Q(:,44),ZERO,0_intkind1,wf(:,117))
  call vert_AV_Q(wf(:,117),wf(:,-6),wf(:,118))
  call counter_QA_V(wf(:,8),wf(:,-2),wf(:,119))
  call prop_A_Q(wf(:,115),Q(:,7),ZERO,0_intkind1,wf(:,120))
  call vert_QA_V(wf(:,-4),wf(:,120),wf(:,121))
  call vert_AV_Q(wf(:,120),wf(:,-6),wf(:,122))
  call counter_AV_Q(wf(:,-2),wf(:,-6),wf(:,123))
  call prop_A_Q(wf(:,123),Q(:,68),ZERO,0_intkind1,wf(:,124))
  call vert_AV_Q(wf(:,124),wf(:,3),wf(:,125))
  call vert_AW_Q(wf(:,124),wf(:,4),wf(:,126))
  call counter_AV_Q(wf(:,-2),wf(:,25),wf(:,127))
  call prop_Q_A(wf(:,35),Q(:,75),ZERO,0_intkind1,wf(:,128))
  call prop_Q_A(wf(:,33),Q(:,120),ZERO,0_intkind1,wf(:,129))
  call counter_QA_V(wf(:,28),wf(:,-2),wf(:,130))
  call prop_A_Q(wf(:,127),Q(:,52),ZERO,0_intkind1,wf(:,131))
  call vert_AV_Q(wf(:,131),wf(:,-6),wf(:,132))
  call vert_QA_V(wf(:,-3),wf(:,120),wf(:,133))
  call vert_AV_Q(wf(:,124),wf(:,25),wf(:,134))
  call counter_AV_Q(wf(:,-2),wf(:,46),wf(:,135))
  call counter_A_Q(ctqq,wf(:,5),Q(:,68),wf(:,136))
  call prop_A_Q(wf(:,136),Q(:,68),ZERO,0_intkind1,wf(:,137))
  call vert_AV_Q(wf(:,137),wf(:,3),wf(:,138))
  call vert_AW_Q(wf(:,137),wf(:,4),wf(:,139))
  call counter_V_V(ctGG,wf(:,3),Q(:,40),wf(:,140))
  call vert_VQ_A(wf(:,140),wf(:,-4),wf(:,141))
  call vert_AV_Q(wf(:,5),wf(:,140),wf(:,142))
  call counter_Q_A(ctqq,wf(:,8),Q(:,19),wf(:,143))
  call counter_Q_A(ctqq,wf(:,11),Q(:,56),wf(:,144))
  call vert_AV_Q(wf(:,-2),wf(:,140),wf(:,145))
  call counter_A_Q(ctqq,wf(:,16),Q(:,7),wf(:,146))
  call counter_A_Q(ctqq,wf(:,19),Q(:,44),wf(:,147))
  call vert_AV_Q(wf(:,16),wf(:,140),wf(:,148))
  call counter_Q_A(ctqq,wf(:,13),Q(:,80),wf(:,149))
  call prop_Q_A(wf(:,149),Q(:,80),ZERO,0_intkind1,wf(:,150))
  call vert_QA_V(wf(:,150),wf(:,16),wf(:,151))
  call vert_WQ_A(wf(:,4),wf(:,150),wf(:,152))
  call vert_VQ_A(wf(:,-6),wf(:,8),wf(:,153))
  call prop_A_Q(wf(:,145),Q(:,44),ZERO,0_intkind1,wf(:,154))
  call vert_VQ_A(wf(:,20),wf(:,-4),wf(:,155))
  call prop_Q_A(wf(:,155),Q(:,120),ZERO,0_intkind1,wf(:,156))
  call vert_AV_Q(wf(:,-2),wf(:,20),wf(:,157))
  call prop_A_Q(wf(:,157),Q(:,108),ZERO,0_intkind1,wf(:,158))
  call vert_VQ_A(wf(:,-6),wf(:,11),wf(:,159))
  call prop_Q_A(wf(:,159),Q(:,120),ZERO,0_intkind1,wf(:,160))
  call prop_Q_A(wf(:,153),Q(:,83),ZERO,0_intkind1,wf(:,161))
  call counter_V_V(ctGG,wf(:,20),Q(:,104),wf(:,162))
  call prop_Q_A(wf(:,141),Q(:,56),ZERO,0_intkind1,wf(:,163))
  call vert_UV_W(wf(:,140),Q(:,40),wf(:,-6),Q(:,64),wf(:,164))
  call prop_A_Q(wf(:,22),Q(:,71),ZERO,0_intkind1,wf(:,165))
  call prop_A_Q(wf(:,23),Q(:,108),ZERO,0_intkind1,wf(:,166))
  call vert_QA_V(wf(:,28),wf(:,137),wf(:,167))
  call counter_V_V(ctGG,wf(:,25),Q(:,48),wf(:,168))
  call vert_VQ_A(wf(:,168),wf(:,-3),wf(:,169))
  call counter_Q_A(ctqq,wf(:,28),Q(:,11),wf(:,170))
  call counter_Q_A(ctqq,wf(:,30),Q(:,56),wf(:,171))
  call vert_AV_Q(wf(:,5),wf(:,168),wf(:,172))
  call vert_AV_Q(wf(:,-2),wf(:,168),wf(:,173))
  call counter_A_Q(ctqq,wf(:,36),Q(:,52),wf(:,174))
  call counter_Q_A(ctqq,wf(:,32),Q(:,72),wf(:,175))
  call prop_Q_A(wf(:,175),Q(:,72),ZERO,0_intkind1,wf(:,176))
  call vert_QA_V(wf(:,176),wf(:,16),wf(:,177))
  call vert_WQ_A(wf(:,4),wf(:,176),wf(:,178))
  call vert_AV_Q(wf(:,16),wf(:,168),wf(:,179))
  call vert_VQ_A(wf(:,37),wf(:,-3),wf(:,180))
  call prop_Q_A(wf(:,180),Q(:,120),ZERO,0_intkind1,wf(:,181))
  call vert_AV_Q(wf(:,-2),wf(:,37),wf(:,182))
  call prop_A_Q(wf(:,182),Q(:,116),ZERO,0_intkind1,wf(:,183))
  call vert_VQ_A(wf(:,-6),wf(:,28),wf(:,184))
  call prop_A_Q(wf(:,173),Q(:,52),ZERO,0_intkind1,wf(:,185))
  call vert_VQ_A(wf(:,-6),wf(:,30),wf(:,186))
  call prop_Q_A(wf(:,186),Q(:,120),ZERO,0_intkind1,wf(:,187))
  call counter_V_V(ctGG,wf(:,37),Q(:,112),wf(:,188))
  call prop_Q_A(wf(:,184),Q(:,75),ZERO,0_intkind1,wf(:,189))
  call prop_Q_A(wf(:,169),Q(:,56),ZERO,0_intkind1,wf(:,190))
  call prop_A_Q(wf(:,40),Q(:,116),ZERO,0_intkind1,wf(:,191))
  call vert_UV_W(wf(:,168),Q(:,48),wf(:,-6),Q(:,64),wf(:,192))
  call vert_VQ_A(wf(:,41),wf(:,-4),wf(:,193))
  call prop_Q_A(wf(:,193),Q(:,120),ZERO,0_intkind1,wf(:,194))
  call vert_AV_Q(wf(:,-2),wf(:,41),wf(:,195))
  call prop_A_Q(wf(:,195),Q(:,108),ZERO,0_intkind1,wf(:,196))
  call counter_V_V(ctGG,wf(:,41),Q(:,104),wf(:,197))
  call vert_QA_V(wf(:,176),wf(:,-5),wf(:,198))
  call vert_VQ_A(wf(:,42),wf(:,-3),wf(:,199))
  call prop_Q_A(wf(:,199),Q(:,120),ZERO,0_intkind1,wf(:,200))
  call vert_AV_Q(wf(:,-2),wf(:,42),wf(:,201))
  call prop_A_Q(wf(:,201),Q(:,116),ZERO,0_intkind1,wf(:,202))
  call counter_V_V(ctGG,wf(:,42),Q(:,112),wf(:,203))
  call vert_QA_V(wf(:,150),wf(:,-5),wf(:,204))
  call vert_VQ_A(wf(:,45),wf(:,-3),wf(:,205))
  call prop_Q_A(wf(:,205),Q(:,120),ZERO,0_intkind1,wf(:,206))
  call vert_AV_Q(wf(:,-2),wf(:,45),wf(:,207))
  call prop_A_Q(wf(:,207),Q(:,116),ZERO,0_intkind1,wf(:,208))
  call counter_V_V(ctGG,wf(:,46),Q(:,104),wf(:,209))
  call vert_VQ_A(wf(:,46),wf(:,-4),wf(:,210))
  call prop_Q_A(wf(:,210),Q(:,120),ZERO,0_intkind1,wf(:,211))
  call counter_V_V(ctGG,wf(:,45),Q(:,112),wf(:,212))
  call prop_A_Q(wf(:,47),Q(:,108),ZERO,0_intkind1,wf(:,213))
  call counter_A_Q(ctqq,wf(:,44),Q(:,96),wf(:,214))
  call prop_A_Q(wf(:,214),Q(:,96),ZERO,0_intkind1,wf(:,215))
  call vert_QA_V(wf(:,-3),wf(:,215),wf(:,216))
  call vert_QA_V(wf(:,-4),wf(:,215),wf(:,217))
  call vert_AV_Q(wf(:,16),wf(:,3),wf(:,218))
  call prop_A_Q(wf(:,218),Q(:,47),ZERO,0_intkind1,wf(:,219))
  call vert_VQ_A(wf(:,3),wf(:,8),wf(:,220))
  call prop_Q_A(wf(:,220),Q(:,59),ZERO,0_intkind1,wf(:,221))
  call vert_AW_Q(wf(:,19),wf(:,4),wf(:,222))
  call prop_A_Q(wf(:,222),Q(:,47),ZERO,0_intkind1,wf(:,223))
  call vert_WQ_A(wf(:,4),wf(:,11),wf(:,224))
  call prop_Q_A(wf(:,224),Q(:,59),ZERO,0_intkind1,wf(:,225))
  call vert_AV_Q(wf(:,16),wf(:,25),wf(:,226))
  call prop_A_Q(wf(:,226),Q(:,55),ZERO,0_intkind1,wf(:,227))
  call vert_VQ_A(wf(:,25),wf(:,28),wf(:,228))
  call prop_Q_A(wf(:,228),Q(:,59),ZERO,0_intkind1,wf(:,229))
  call vert_AW_Q(wf(:,36),wf(:,4),wf(:,230))
  call prop_A_Q(wf(:,230),Q(:,55),ZERO,0_intkind1,wf(:,231))
  call vert_WQ_A(wf(:,4),wf(:,30),wf(:,232))
  call prop_Q_A(wf(:,232),Q(:,59),ZERO,0_intkind1,wf(:,233))
  call vert_QA_V(wf(:,28),wf(:,5),wf(:,234))
  call vert_QA_V(wf(:,8),wf(:,5),wf(:,235))
  call vert_QA_V(wf(:,-3),wf(:,68),wf(:,236))
  call vert_QA_V(wf(:,-4),wf(:,68),wf(:,237))
  call vert_QA_V(wf(:,32),wf(:,16),wf(:,238))
  call vert_QA_V(wf(:,128),wf(:,-2),wf(:,239))
  call vert_QA_V(wf(:,13),wf(:,16),wf(:,240))
  call vert_QA_V(wf(:,114),wf(:,-2),wf(:,241))
  call vert_VQ_A(wf(:,38),wf(:,-4),wf(:,242))
  call prop_Q_A(wf(:,242),Q(:,31),ZERO,0_intkind1,wf(:,243))
  call vert_AV_Q(wf(:,-5),wf(:,38),wf(:,244))
  call prop_A_Q(wf(:,244),Q(:,47),ZERO,0_intkind1,wf(:,245))
  call vert_UV_W(wf(:,38),Q(:,15),wf(:,-6),Q(:,64),wf(:,246))
  call vert_VQ_A(wf(:,21),wf(:,-3),wf(:,247))
  call prop_Q_A(wf(:,247),Q(:,31),ZERO,0_intkind1,wf(:,248))
  call vert_AV_Q(wf(:,-5),wf(:,21),wf(:,249))
  call prop_A_Q(wf(:,249),Q(:,55),ZERO,0_intkind1,wf(:,250))
  call vert_UV_W(wf(:,21),Q(:,23),wf(:,-6),Q(:,64),wf(:,251))
  call vert_QA_V(wf(:,-3),wf(:,165),wf(:,252))
  call vert_QA_V(wf(:,-4),wf(:,165),wf(:,253))
  call vert_VQ_A(wf(:,39),wf(:,-4),wf(:,254))
  call prop_Q_A(wf(:,254),Q(:,31),ZERO,0_intkind1,wf(:,255))
  call vert_AV_Q(wf(:,-5),wf(:,39),wf(:,256))
  call prop_A_Q(wf(:,256),Q(:,47),ZERO,0_intkind1,wf(:,257))
  call vert_UV_W(wf(:,39),Q(:,15),wf(:,-6),Q(:,64),wf(:,258))
  call vert_QA_V(wf(:,189),wf(:,-2),wf(:,259))
  call vert_VQ_A(wf(:,24),wf(:,-3),wf(:,260))
  call prop_Q_A(wf(:,260),Q(:,31),ZERO,0_intkind1,wf(:,261))
  call vert_AV_Q(wf(:,-5),wf(:,24),wf(:,262))
  call prop_A_Q(wf(:,262),Q(:,55),ZERO,0_intkind1,wf(:,263))
  call vert_UV_W(wf(:,24),Q(:,23),wf(:,-6),Q(:,64),wf(:,264))
  call vert_QA_V(wf(:,161),wf(:,-2),wf(:,265))

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
  den(2) = 1 / (Q(5,68))
  den(3) = 1 / (Q(5,40))
  den(5) = 1 / (Q(5,19))
  den(9) = 1 / (Q(5,56))
  den(12) = 1 / (Q(5,80))
  den(14) = 1 / (Q(5,7))
  den(18) = 1 / (Q(5,44))
  den(21) = 1 / (Q(5,104))
  den(27) = 1 / (Q(5,48))
  den(29) = 1 / (Q(5,11))
  den(34) = 1 / (Q(5,72))
  den(38) = 1 / (Q(5,52))
  den(41) = 1 / (Q(5,112))
  den(53) = 1 / (Q(5,96))
  den(60) = 1 / (Q(5,71))
  den(63) = 1 / (Q(5,108))
  den(67) = 1 / (Q(5,116))
  den(70) = 1 / (Q(5,83))
  den(73) = 1 / (Q(5,120))
  den(76) = 1 / (Q(5,75))
  den(112) = 1 / (Q(5,23))
  den(152) = 1 / (Q(5,15))
  den(201) = 1 / (Q(5,47))
  den(204) = 1 / (Q(5,59))
  den(211) = 1 / (Q(5,55))
  den(220) = 1 / (Q(5,79))
  den(223) = 1 / (Q(5,87))
  den(233) = 1 / (Q(5,31))

  ! denominators
  den(4) = den(2)*den(3)
  den(6) = den(1)*den(5)
  den(7) = den(4)*den(6)
  den(8) = den(1)*den(2)
  den(10) = den(3)*den(9)
  den(11) = den(8)*den(10)
  den(13) = den(3)*den(12)
  den(15) = den(1)*den(14)
  den(16) = den(13)*den(15)
  den(17) = den(1)*den(12)
  den(19) = den(3)*den(18)
  den(20) = den(17)*den(19)
  den(22) = den(3)*den(21)
  den(23) = den(15)*den(22)
  den(24) = den(10)*den(15)
  den(25) = den(6)*den(19)
  den(26) = den(6)*den(22)
  den(28) = den(2)*den(27)
  den(30) = den(1)*den(29)
  den(31) = den(28)*den(30)
  den(32) = den(9)*den(27)
  den(33) = den(8)*den(32)
  den(35) = den(27)*den(34)
  den(36) = den(15)*den(35)
  den(37) = den(1)*den(34)
  den(39) = den(27)*den(38)
  den(40) = den(37)*den(39)
  den(42) = den(27)*den(41)
  den(43) = den(15)*den(42)
  den(44) = den(15)*den(32)
  den(45) = den(30)*den(42)
  den(46) = den(30)*den(39)
  den(47) = den(21)*den(34)
  den(48) = den(15)*den(47)
  den(49) = den(6)*den(47)
  den(50) = den(12)*den(41)
  den(51) = den(15)*den(50)
  den(52) = den(30)*den(50)
  den(54) = den(41)*den(53)
  den(55) = den(15)*den(54)
  den(56) = den(21)*den(53)
  den(57) = den(15)*den(56)
  den(58) = den(30)*den(54)
  den(59) = den(6)*den(56)
  den(61) = den(8)*den(60)
  den(62) = den(3)*den(61)
  den(64) = den(4)*den(63)
  den(65) = den(1)*den(64)
  den(66) = den(27)*den(61)
  den(68) = den(28)*den(67)
  den(69) = den(1)*den(68)
  den(71) = den(17)*den(70)
  den(72) = den(3)*den(71)
  den(74) = den(13)*den(73)
  den(75) = den(1)*den(74)
  den(77) = den(37)*den(76)
  den(78) = den(27)*den(77)
  den(79) = den(35)*den(73)
  den(80) = den(1)*den(79)
  den(81) = den(2)**2
  den(82) = den(3)*den(81)
  den(83) = den(6)*den(82)
  den(84) = den(1)*den(81)
  den(85) = den(10)*den(84)
  den(86) = den(3)**2
  den(87) = den(61)*den(86)
  den(88) = den(2)*den(86)
  den(89) = den(6)*den(88)
  den(90) = den(6)*den(64)
  den(91) = den(10)*den(61)
  den(92) = den(71)*den(86)
  den(93) = den(15)*den(74)
  den(94) = den(19)*den(71)
  den(95) = den(15)*den(86)
  den(96) = den(12)*den(95)
  den(97) = den(12)**2
  den(98) = den(15)*den(97)
  den(99) = den(3)*den(98)
  den(100) = den(1)*den(97)
  den(101) = den(19)*den(100)
  den(102) = den(18)*den(86)
  den(103) = den(6)*den(102)
  den(104) = den(22)*den(73)
  den(105) = den(15)*den(104)
  den(106) = den(22)*den(63)
  den(107) = den(6)*den(106)
  den(108) = den(10)*den(73)
  den(109) = den(15)*den(108)
  den(110) = den(6)*den(70)
  den(111) = den(19)*den(110)
  den(113) = den(6)*den(112)
  den(114) = den(22)*den(113)
  den(115) = den(9)*den(86)
  den(116) = den(15)*den(115)
  den(117) = den(15)*den(112)
  den(118) = den(86)*den(117)
  den(119) = den(86)*den(113)
  den(120) = den(22)*den(117)
  den(121) = den(15)*den(60)
  den(122) = den(10)*den(121)
  den(123) = den(19)*den(63)
  den(124) = den(6)*den(123)
  den(125) = den(30)*den(81)
  den(126) = den(27)*den(125)
  den(127) = den(32)*den(84)
  den(128) = den(27)**2
  den(129) = den(61)*den(128)
  den(130) = den(30)*den(68)
  den(131) = den(32)*den(61)
  den(132) = den(2)*den(128)
  den(133) = den(30)*den(132)
  den(134) = den(77)*den(128)
  den(135) = den(15)*den(79)
  den(136) = den(39)*den(77)
  den(137) = den(34)**2
  den(138) = den(15)*den(137)
  den(139) = den(27)*den(138)
  den(140) = den(1)*den(137)
  den(141) = den(39)*den(140)
  den(142) = den(15)*den(128)
  den(143) = den(34)*den(142)
  den(144) = den(42)*den(73)
  den(145) = den(15)*den(144)
  den(146) = den(42)*den(67)
  den(147) = den(30)*den(146)
  den(148) = den(38)*den(128)
  den(149) = den(30)*den(148)
  den(150) = den(32)*den(73)
  den(151) = den(15)*den(150)
  den(153) = den(30)*den(152)
  den(154) = den(42)*den(153)
  den(155) = den(30)*den(76)
  den(156) = den(39)*den(155)
  den(157) = den(9)*den(128)
  den(158) = den(15)*den(157)
  den(159) = den(15)*den(152)
  den(160) = den(42)*den(159)
  den(161) = den(32)*den(121)
  den(162) = den(39)*den(67)
  den(163) = den(30)*den(162)
  den(164) = den(128)*den(159)
  den(165) = den(128)*den(153)
  den(166) = den(47)*den(73)
  den(167) = den(15)*den(166)
  den(168) = den(47)*den(63)
  den(169) = den(6)*den(168)
  den(170) = den(47)*den(113)
  den(171) = den(117)*den(137)
  den(172) = den(113)*den(137)
  den(173) = den(47)*den(117)
  den(174) = den(50)*den(73)
  den(175) = den(15)*den(174)
  den(176) = den(50)*den(67)
  den(177) = den(30)*den(176)
  den(178) = den(50)*den(153)
  den(179) = den(50)*den(159)
  den(180) = den(97)*den(159)
  den(181) = den(97)*den(153)
  den(182) = den(54)*den(73)
  den(183) = den(15)*den(182)
  den(184) = den(54)*den(67)
  den(185) = den(30)*den(184)
  den(186) = den(56)*den(113)
  den(187) = den(56)*den(73)
  den(188) = den(15)*den(187)
  den(189) = den(54)*den(153)
  den(190) = den(56)*den(63)
  den(191) = den(6)*den(190)
  den(192) = den(54)*den(159)
  den(193) = den(56)*den(117)
  den(194) = den(53)**2
  den(195) = den(21)*den(194)
  den(196) = den(15)*den(195)
  den(197) = den(113)*den(194)
  den(198) = den(159)*den(194)
  den(199) = den(153)*den(194)
  den(200) = den(3)*den(15)
  den(202) = den(200)*den(201)
  den(203) = den(3)*den(6)
  den(205) = den(203)*den(204)
  den(206) = den(1)*den(19)
  den(207) = den(201)*den(206)
  den(208) = den(1)*den(10)
  den(209) = den(204)*den(208)
  den(210) = den(15)*den(27)
  den(212) = den(210)*den(211)
  den(213) = den(27)*den(30)
  den(214) = den(204)*den(213)
  den(215) = den(1)*den(39)
  den(216) = den(211)*den(215)
  den(217) = den(1)*den(32)
  den(218) = den(204)*den(217)
  den(219) = den(2)*den(30)
  den(221) = den(219)*den(220)
  den(222) = den(2)*den(6)
  den(224) = den(222)*den(223)
  den(225) = den(61)*den(220)
  den(226) = den(61)*den(223)
  den(227) = den(15)*den(34)
  den(228) = den(220)*den(227)
  den(229) = den(77)*den(220)
  den(230) = den(12)*den(15)
  den(231) = den(223)*den(230)
  den(232) = den(71)*den(223)
  den(234) = den(159)*den(233)
  den(235) = den(159)*den(201)
  den(236) = den(159)*den(220)
  den(237) = den(117)*den(233)
  den(238) = den(117)*den(211)
  den(239) = den(117)*den(223)
  den(240) = den(121)*den(220)
  den(241) = den(121)*den(223)
  den(242) = den(153)*den(233)
  den(243) = den(153)*den(201)
  den(244) = den(153)*den(220)
  den(245) = den(155)*den(220)
  den(246) = den(113)*den(233)
  den(247) = den(113)*den(211)
  den(248) = den(113)*den(223)
  den(249) = den(110)*den(223)
  den(250) = den(1)*den(3)
  den(251) = den(1)*den(27)
  den(252) = den(2)*den(3)*den(6)
  den(253) = den(1)*den(2)*den(10)
  den(254) = den(1)*den(2)*den(3)
  den(255) = den(3)*den(12)*den(15)
  den(256) = den(1)*den(12)*den(19)
  den(257) = den(1)*den(3)*den(12)
  den(258) = den(3)*den(117)
  den(259) = den(3)*den(121)
  den(260) = den(3)*den(113)
  den(261) = den(3)*den(110)
  den(262) = den(1)*den(123)
  den(263) = den(1)*den(106)
  den(264) = den(1)*den(108)
  den(265) = den(1)*den(104)
  den(266) = den(1)*den(22)
  den(267) = den(2)*den(27)*den(30)
  den(268) = den(1)*den(2)*den(32)
  den(269) = den(1)*den(2)*den(27)
  den(270) = den(15)*den(27)*den(34)
  den(271) = den(1)*den(34)*den(39)
  den(272) = den(1)*den(27)*den(34)
  den(273) = den(27)*den(159)
  den(274) = den(27)*den(121)
  den(275) = den(27)*den(153)
  den(276) = den(27)*den(155)
  den(277) = den(1)*den(162)
  den(278) = den(1)*den(146)
  den(279) = den(1)*den(150)
  den(280) = den(1)*den(144)
  den(281) = den(1)*den(42)
  den(282) = den(34)*den(117)
  den(283) = den(34)*den(113)
  den(284) = den(6)*den(34)
  den(285) = den(1)*den(168)
  den(286) = den(1)*den(166)
  den(287) = den(1)*den(47)
  den(288) = den(12)*den(159)
  den(289) = den(12)*den(153)
  den(290) = den(12)*den(30)
  den(291) = den(1)*den(176)
  den(292) = den(1)*den(174)
  den(293) = den(1)*den(50)
  den(294) = den(53)*den(159)
  den(295) = den(53)*den(117)
  den(296) = den(15)*den(53)
  den(297) = den(53)*den(153)
  den(298) = den(30)*den(53)
  den(299) = den(53)*den(113)
  den(300) = den(6)*den(53)
  den(301) = den(1)*den(190)
  den(302) = den(1)*den(184)
  den(303) = den(1)*den(187)
  den(304) = den(1)*den(182)
  den(305) = den(1)*den(56)
  den(306) = den(1)*den(54)
  den(307) = den(1)*den(53)
  den(308) = den(3)*den(224)
  den(309) = den(2)*den(205)
  den(310) = den(3)*den(226)
  den(311) = den(2)*den(209)
  den(312) = den(12)*den(202)
  den(313) = den(3)*den(231)
  den(314) = den(12)*den(207)
  den(315) = den(3)*den(232)
  den(316) = den(3)*den(239)
  den(317) = den(3)*den(241)
  den(318) = den(3)*den(248)
  den(319) = den(3)*den(249)
  den(320) = den(27)*den(221)
  den(321) = den(2)*den(214)
  den(322) = den(27)*den(225)
  den(323) = den(2)*den(218)
  den(324) = den(27)*den(228)
  den(325) = den(34)*den(212)
  den(326) = den(34)*den(216)
  den(327) = den(27)*den(229)
  den(328) = den(27)*den(236)
  den(329) = den(27)*den(240)
  den(330) = den(27)*den(244)
  den(331) = den(27)*den(245)
  den(332) = den(34)*den(238)
  den(333) = den(34)*den(247)
  den(334) = den(12)*den(235)
  den(335) = den(12)*den(243)
  den(336) = den(53)*den(234)
  den(337) = den(53)*den(237)
  den(338) = den(53)*den(242)
  den(339) = den(53)*den(246)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(192)

  A(1) = cont_QA(wf(:,7),wf(:,8)) * den(7)
  A(2) = cont_QA(wf(:,10),wf(:,11)) * den(11)
  A(3) = cont_QA(wf(:,15),wf(:,16)) * den(16)
  A(4) = cont_QA(wf(:,18),wf(:,19)) * den(20)
  A(5) = cont_VV(wf(:,20),wf(:,21)) * den(23)
  A(6) = cont_QA(wf(:,11),wf(:,22)) * den(24)
  A(7) = cont_QA(wf(:,8),wf(:,23)) * den(25)
  A(8) = cont_VV(wf(:,20),wf(:,24)) * den(26)
  A(9) = cont_QA(wf(:,27),wf(:,28)) * den(31)
  A(10) = cont_QA(wf(:,10),wf(:,30)) * den(33)
  A(11) = cont_QA(wf(:,16),wf(:,33)) * den(36)
  A(12) = cont_QA(wf(:,35),wf(:,36)) * den(40)
  A(13) = cont_VV(wf(:,37),wf(:,38)) * den(43)
  A(14) = cont_QA(wf(:,22),wf(:,30)) * den(44)
  A(15) = cont_VV(wf(:,37),wf(:,39)) * den(45)
  A(16) = cont_QA(wf(:,28),wf(:,40)) * den(46)
  A(17) = cont_VV(wf(:,21),wf(:,41)) * den(48)
  A(18) = cont_VV(wf(:,24),wf(:,41)) * den(49)
  A(19) = cont_VV(wf(:,38),wf(:,42)) * den(51)
  A(20) = cont_VV(wf(:,39),wf(:,42)) * den(52)
  A(21) = cont_VV(wf(:,38),wf(:,45)) * den(55)
  A(22) = cont_VV(wf(:,21),wf(:,46)) * den(57)
  A(23) = cont_VV(wf(:,39),wf(:,45)) * den(58)
  A(24) = cont_QA(wf(:,8),wf(:,47)) * den(59)

  A(25) = cont_QA(wf(:,8),wf(:,48)) * den(7)
  A(26) = cont_QA(wf(:,11),wf(:,49)) * den(11)
  A(27) = cont_QA(wf(:,16),wf(:,50)) * den(16)
  A(28) = cont_QA(wf(:,19),wf(:,51)) * den(20)
  A(29) = cont_VV(wf(:,21),wf(:,52)) * den(23)
  A(30) = cont_QA(wf(:,11),wf(:,53)) * den(24)
  A(31) = cont_QA(wf(:,8),wf(:,54)) * den(25)
  A(32) = cont_VV(wf(:,24),wf(:,52)) * den(26)
  A(33) = cont_QA(wf(:,28),wf(:,55)) * den(31)
  A(34) = cont_QA(wf(:,30),wf(:,49)) * den(33)
  A(35) = cont_QA(wf(:,16),wf(:,56)) * den(36)
  A(36) = cont_QA(wf(:,36),wf(:,57)) * den(40)
  A(37) = cont_VV(wf(:,38),wf(:,58)) * den(43)
  A(38) = cont_QA(wf(:,30),wf(:,53)) * den(44)
  A(39) = cont_VV(wf(:,39),wf(:,58)) * den(45)
  A(40) = cont_QA(wf(:,28),wf(:,59)) * den(46)
  A(41) = cont_VV(wf(:,21),wf(:,60)) * den(48)
  A(42) = cont_VV(wf(:,24),wf(:,60)) * den(49)
  A(43) = cont_VV(wf(:,38),wf(:,61)) * den(51)
  A(44) = cont_VV(wf(:,39),wf(:,61)) * den(52)
  A(45) = cont_VV(wf(:,38),wf(:,64)) * den(55)
  A(46) = cont_VV(wf(:,21),wf(:,65)) * den(57)
  A(47) = cont_VV(wf(:,39),wf(:,64)) * den(58)
  A(48) = cont_QA(wf(:,8),wf(:,66)) * den(59)
  A(49) = cont_QA(wf(:,67),wf(:,68)) * den(62)
  A(50) = cont_QA(wf(:,69),wf(:,70)) * den(65)
  A(51) = cont_QA(wf(:,22),wf(:,71)) * den(24)
  A(52) = cont_VV(wf(:,20),wf(:,72)) * den(23)
  A(53) = cont_QA(wf(:,23),wf(:,73)) * den(25)
  A(54) = cont_VV(wf(:,20),wf(:,74)) * den(26)
  A(55) = cont_QA(wf(:,16),wf(:,77)) * den(16)
  A(56) = cont_QA(wf(:,19),wf(:,78)) * den(20)
  A(57) = cont_VV(wf(:,41),wf(:,72)) * den(48)
  A(58) = cont_VV(wf(:,41),wf(:,74)) * den(49)
  A(59) = cont_VV(wf(:,38),wf(:,79)) * den(55)
  A(60) = cont_VV(wf(:,46),wf(:,72)) * den(57)
  A(61) = cont_VV(wf(:,39),wf(:,79)) * den(58)
  A(62) = cont_QA(wf(:,47),wf(:,73)) * den(59)
  A(63) = cont_VV(wf(:,38),wf(:,80)) * den(51)
  A(64) = cont_VV(wf(:,39),wf(:,80)) * den(52)
  A(65) = cont_QA(wf(:,28),wf(:,82)) * den(31)
  A(66) = cont_QA(wf(:,10),wf(:,84)) * den(33)
  A(67) = cont_QA(wf(:,16),wf(:,85)) * den(36)
  A(68) = cont_QA(wf(:,35),wf(:,87)) * den(40)
  A(69) = cont_VV(wf(:,38),wf(:,88)) * den(43)
  A(70) = cont_QA(wf(:,22),wf(:,84)) * den(44)
  A(71) = cont_VV(wf(:,39),wf(:,88)) * den(45)
  A(72) = cont_QA(wf(:,28),wf(:,89)) * den(46)
  A(73) = cont_QA(wf(:,68),wf(:,90)) * den(66)
  A(74) = cont_QA(wf(:,91),wf(:,92)) * den(69)
  A(75) = cont_QA(wf(:,22),wf(:,93)) * den(44)
  A(76) = cont_VV(wf(:,37),wf(:,94)) * den(43)
  A(77) = cont_QA(wf(:,40),wf(:,95)) * den(46)
  A(78) = cont_VV(wf(:,37),wf(:,96)) * den(45)
  A(79) = cont_QA(wf(:,16),wf(:,99)) * den(36)
  A(80) = cont_QA(wf(:,36),wf(:,100)) * den(40)
  A(81) = cont_VV(wf(:,42),wf(:,94)) * den(51)
  A(82) = cont_VV(wf(:,42),wf(:,96)) * den(52)
  A(83) = cont_VV(wf(:,21),wf(:,101)) * den(57)
  A(84) = cont_VV(wf(:,45),wf(:,94)) * den(55)
  A(85) = cont_QA(wf(:,8),wf(:,102)) * den(59)
  A(86) = cont_VV(wf(:,45),wf(:,96)) * den(58)
  A(87) = cont_VV(wf(:,21),wf(:,103)) * den(48)
  A(88) = cont_VV(wf(:,24),wf(:,103)) * den(49)
  A(89) = cont_QA(wf(:,8),wf(:,105)) * den(7)
  A(90) = cont_QA(wf(:,10),wf(:,107)) * den(11)
  A(91) = cont_QA(wf(:,16),wf(:,108)) * den(16)
  A(92) = cont_QA(wf(:,18),wf(:,110)) * den(20)
  A(93) = cont_VV(wf(:,21),wf(:,111)) * den(23)
  A(94) = cont_QA(wf(:,22),wf(:,107)) * den(24)
  A(95) = cont_VV(wf(:,24),wf(:,111)) * den(26)
  A(96) = cont_QA(wf(:,8),wf(:,112)) * den(25)
  A(97) = cont_QA(wf(:,113),wf(:,114)) * den(72)
  A(98) = cont_QA(wf(:,115),wf(:,116)) * den(75)
  A(99) = cont_QA(wf(:,8),wf(:,118)) * den(25)
  A(100) = cont_VV(wf(:,20),wf(:,119)) * den(26)
  A(101) = cont_VV(wf(:,20),wf(:,121)) * den(23)
  A(102) = cont_QA(wf(:,11),wf(:,122)) * den(24)
  A(103) = cont_QA(wf(:,8),wf(:,125)) * den(7)
  A(104) = cont_QA(wf(:,11),wf(:,126)) * den(11)
  A(105) = cont_QA(wf(:,127),wf(:,128)) * den(78)
  A(106) = cont_QA(wf(:,115),wf(:,129)) * den(80)
  A(107) = cont_VV(wf(:,37),wf(:,130)) * den(45)
  A(108) = cont_QA(wf(:,28),wf(:,132)) * den(46)
  A(109) = cont_VV(wf(:,37),wf(:,133)) * den(43)
  A(110) = cont_QA(wf(:,30),wf(:,122)) * den(44)
  A(111) = cont_QA(wf(:,28),wf(:,134)) * den(31)
  A(112) = cont_QA(wf(:,30),wf(:,126)) * den(33)
  A(113) = cont_VV(wf(:,41),wf(:,119)) * den(49)
  A(114) = cont_VV(wf(:,41),wf(:,121)) * den(48)
  A(115) = cont_VV(wf(:,42),wf(:,130)) * den(52)
  A(116) = cont_VV(wf(:,42),wf(:,133)) * den(51)
  A(117) = cont_VV(wf(:,45),wf(:,130)) * den(58)
  A(118) = cont_QA(wf(:,8),wf(:,135)) * den(59)
  A(119) = cont_VV(wf(:,45),wf(:,133)) * den(55)
  A(120) = cont_VV(wf(:,46),wf(:,121)) * den(57)
  A(121) = cont_QA(wf(:,8),wf(:,138)) * den(83)
  A(122) = cont_QA(wf(:,11),wf(:,139)) * den(85)
  A(123) = cont_QA(wf(:,68),wf(:,141)) * den(87)
  A(124) = cont_QA(wf(:,8),wf(:,142)) * den(89)
  A(125) = cont_QA(wf(:,70),wf(:,143)) * den(90)
  A(126) = cont_QA(wf(:,68),wf(:,144)) * den(91)
  A(127) = cont_QA(wf(:,114),wf(:,145)) * den(92)
  A(128) = cont_QA(wf(:,116),wf(:,146)) * den(93)
  A(129) = cont_QA(wf(:,114),wf(:,147)) * den(94)
  A(130) = cont_QA(wf(:,13),wf(:,148)) * den(96)
  A(131) = cont_VV(wf(:,3),wf(:,151)) * den(99)
  A(132) = cont_QA(wf(:,19),wf(:,152)) * den(101)
  A(133) = cont_QA(wf(:,153),wf(:,154)) * den(103)
  A(134) = cont_QA(wf(:,146),wf(:,156)) * den(105)
  A(135) = cont_QA(wf(:,143),wf(:,158)) * den(107)
  A(136) = cont_QA(wf(:,146),wf(:,160)) * den(109)
  A(137) = cont_QA(wf(:,147),wf(:,161)) * den(111)
  A(138) = cont_VV(wf(:,24),wf(:,162)) * den(114)
  A(139) = cont_QA(wf(:,22),wf(:,163)) * den(116)
  A(140) = cont_VV(wf(:,21),wf(:,164)) * den(118)
  A(141) = cont_VV(wf(:,24),wf(:,164)) * den(119)
  A(142) = cont_VV(wf(:,21),wf(:,162)) * den(120)
  A(143) = cont_QA(wf(:,144),wf(:,165)) * den(122)
  A(144) = cont_QA(wf(:,143),wf(:,166)) * den(124)
  A(145) = cont_VV(wf(:,25),wf(:,167)) * den(126)
  A(146) = cont_QA(wf(:,30),wf(:,139)) * den(127)
  A(147) = cont_QA(wf(:,68),wf(:,169)) * den(129)
  A(148) = cont_QA(wf(:,92),wf(:,170)) * den(130)
  A(149) = cont_QA(wf(:,68),wf(:,171)) * den(131)
  A(150) = cont_QA(wf(:,28),wf(:,172)) * den(133)
  A(151) = cont_QA(wf(:,128),wf(:,173)) * den(134)
  A(152) = cont_QA(wf(:,129),wf(:,146)) * den(135)
  A(153) = cont_QA(wf(:,128),wf(:,174)) * den(136)
  A(154) = cont_VV(wf(:,25),wf(:,177)) * den(139)
  A(155) = cont_QA(wf(:,36),wf(:,178)) * den(141)
  A(156) = cont_QA(wf(:,32),wf(:,179)) * den(143)
  A(157) = cont_QA(wf(:,146),wf(:,181)) * den(145)
  A(158) = cont_QA(wf(:,170),wf(:,183)) * den(147)
  A(159) = cont_QA(wf(:,184),wf(:,185)) * den(149)
  A(160) = cont_QA(wf(:,146),wf(:,187)) * den(151)
  A(161) = cont_VV(wf(:,39),wf(:,188)) * den(154)
  A(162) = cont_QA(wf(:,174),wf(:,189)) * den(156)
  A(163) = cont_QA(wf(:,22),wf(:,190)) * den(158)
  A(164) = cont_VV(wf(:,38),wf(:,188)) * den(160)
  A(165) = cont_QA(wf(:,165),wf(:,171)) * den(161)
  A(166) = cont_QA(wf(:,170),wf(:,191)) * den(163)
  A(167) = cont_VV(wf(:,38),wf(:,192)) * den(164)
  A(168) = cont_VV(wf(:,39),wf(:,192)) * den(165)
  A(169) = cont_QA(wf(:,146),wf(:,194)) * den(167)
  A(170) = cont_QA(wf(:,143),wf(:,196)) * den(169)
  A(171) = cont_VV(wf(:,24),wf(:,197)) * den(170)
  A(172) = cont_VV(wf(:,21),wf(:,198)) * den(171)
  A(173) = cont_VV(wf(:,24),wf(:,198)) * den(172)
  A(174) = cont_VV(wf(:,21),wf(:,197)) * den(173)
  A(175) = cont_QA(wf(:,146),wf(:,200)) * den(175)
  A(176) = cont_QA(wf(:,170),wf(:,202)) * den(177)
  A(177) = cont_VV(wf(:,39),wf(:,203)) * den(178)
  A(178) = cont_VV(wf(:,38),wf(:,203)) * den(179)
  A(179) = cont_VV(wf(:,38),wf(:,204)) * den(180)
  A(180) = cont_VV(wf(:,39),wf(:,204)) * den(181)
  A(181) = cont_QA(wf(:,146),wf(:,206)) * den(183)
  A(182) = cont_QA(wf(:,170),wf(:,208)) * den(185)
  A(183) = cont_VV(wf(:,24),wf(:,209)) * den(186)
  A(184) = cont_QA(wf(:,146),wf(:,211)) * den(188)
  A(185) = cont_VV(wf(:,39),wf(:,212)) * den(189)
  A(186) = cont_QA(wf(:,143),wf(:,213)) * den(191)
  A(187) = cont_VV(wf(:,38),wf(:,212)) * den(192)
  A(188) = cont_VV(wf(:,21),wf(:,209)) * den(193)
  A(189) = cont_VV(wf(:,21),wf(:,216)) * den(196)
  A(190) = cont_VV(wf(:,24),wf(:,216)) * den(197)
  A(191) = cont_VV(wf(:,38),wf(:,217)) * den(198)
  A(192) = cont_VV(wf(:,39),wf(:,217)) * den(199)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(192)
  complex(REALKIND), intent(out) :: M1(4), M2(4)

  M1(1) = ((A(9)+A(10)+A(11)+A(12)+A(14)+A(16))*f(1))/6._/**/REALKIND+((A(1)+A(2)+A(6)+A(17)+A(18))*f(1))/2._/**/REALKIND+(CI*( &
       -A(5)-A(8))*f(2))/2._/**/REALKIND
  M1(2) = ((-A(1)-A(2)-A(3)-A(4)-A(6)-A(7))*f(1))/6._/**/REALKIND+((-A(9)-A(10)-A(14)-A(19)-A(20))*f(1))/2._/**/REALKIND &
       +(CI*(A(13)+A(15))*f(2))/2._/**/REALKIND
  M1(3) = ((-A(11)-A(12)-A(16)-A(21)-A(23))*f(1))/2._/**/REALKIND+((-A(17)-A(18)-A(22)-A(24))*f(1))/6._/**/REALKIND+(CI*(-A(13) &
       -A(15))*f(2))/2._/**/REALKIND
  M1(4) = ((A(19)+A(20)+A(21)+A(23))*f(1))/6._/**/REALKIND+((A(3)+A(4)+A(7)+A(22)+A(24))*f(1))/2._/**/REALKIND+(CI*(A(5) &
       +A(8))*f(2))/2._/**/REALKIND

  M2(1) = ((-A(145)-A(146)-A(147)-A(148)-A(149)-A(150)-A(151)-A(152)-A(153)-A(154)-A(155)-A(156)-A(159)-A(160)-A(162)-A(163) &
       -A(165)-A(166))*f(3))/6._/**/REALKIND+((-A(121)-A(122)-A(123)-A(124)-A(125)-A(126)-A(136)-A(139)-A(143)-A(169)-A(170) &
       -A(171)-A(172)-A(173)-A(174))*f(3))/2._/**/REALKIND+(CI*(A(134)+A(135)+A(138)+A(140)+A(141)+A(142))*f(4))/2._/**/REALKIND &
       +((A(33)+A(35)+A(38)+A(40)+A(65)+A(66)+A(67)+A(68)+A(70)+A(72)+A(73)+A(75)+A(79)+A(80)+A(105)+A(108)+A(111) &
       +A(112))*f(5))/6._/**/REALKIND+((A(25)+A(30)+A(41)+A(42)+A(49)+A(51)+A(57)+A(87)+A(88)+A(89)+A(90)+A(94)+A(103)+A(104) &
       +A(113))*f(5))/2._/**/REALKIND+(CI*(-A(52)-A(93)-A(95)-A(100))*f(6))/2._/**/REALKIND+((A(34)+A(36)+A(74)+A(77)+A(106) &
       +A(110))*f(7))/6._/**/REALKIND+((A(26)+A(50)+A(58)+A(102)+A(114))*f(7))/2._/**/REALKIND+(CI*(-A(54) &
       -A(101))*f(8))/2._/**/REALKIND+(CI*(-A(29)-A(32))*f(9))/2._/**/REALKIND
  M2(2) = ((A(121)+A(122)+A(123)+A(124)+A(125)+A(126)+A(127)+A(128)+A(129)+A(130)+A(131)+A(132)+A(133)+A(136)+A(137)+A(139)+A(143) &
       +A(144))*f(3))/6._/**/REALKIND+((A(145)+A(146)+A(147)+A(148)+A(149)+A(150)+A(160)+A(163)+A(165)+A(175)+A(176)+A(177)+A(178) &
       +A(179)+A(180))*f(3))/2._/**/REALKIND+(CI*(-A(157)-A(158)-A(161)-A(164)-A(167)-A(168))*f(4))/2._/**/REALKIND+((-A(25)-A(27) &
       -A(30)-A(31)-A(49)-A(51)-A(55)-A(56)-A(89)-A(90)-A(91)-A(92)-A(94)-A(96)-A(97)-A(99)-A(103)-A(104))*f(5))/6._/**/REALKIND &
       +((-A(33)-A(38)-A(43)-A(44)-A(63)-A(64)-A(65)-A(66)-A(70)-A(73)-A(75)-A(81)-A(111)-A(112)-A(115))*f(5))/2._/**/REALKIND &
       +(CI*(A(69)+A(71)+A(76)+A(107))*f(6))/2._/**/REALKIND+((-A(26)-A(28)-A(50)-A(53)-A(98)-A(102))*f(7))/6._/**/REALKIND+(( &
       -A(34)-A(74)-A(82)-A(110)-A(116))*f(7))/2._/**/REALKIND+(CI*(A(78)+A(109))*f(8))/2._/**/REALKIND+(CI*(A(37) &
       +A(39))*f(9))/2._/**/REALKIND
  M2(3) = ((A(169)+A(170)+A(171)+A(172)+A(173)+A(174)+A(183)+A(184)+A(186)+A(188)+A(189)+A(190))*f(3))/6._/**/REALKIND+((A(151) &
       +A(152)+A(153)+A(154)+A(155)+A(156)+A(159)+A(162)+A(166)+A(181)+A(182)+A(185)+A(187)+A(191)+A(192))*f(3))/2._/**/REALKIND &
       +(CI*(A(157)+A(158)+A(161)+A(164)+A(167)+A(168))*f(4))/2._/**/REALKIND+((-A(35)-A(40)-A(45)-A(47)-A(59)-A(61)-A(67)-A(68) &
       -A(72)-A(79)-A(80)-A(84)-A(105)-A(108)-A(117))*f(5))/2._/**/REALKIND+((-A(41)-A(42)-A(46)-A(48)-A(57)-A(60)-A(83)-A(85) &
       -A(87)-A(88)-A(113)-A(118))*f(5))/6._/**/REALKIND+(CI*(-A(69)-A(71)-A(76)-A(107))*f(6))/2._/**/REALKIND+((-A(36)-A(77) &
       -A(86)-A(106)-A(119))*f(7))/2._/**/REALKIND+((-A(58)-A(62)-A(114)-A(120))*f(7))/6._/**/REALKIND+(CI*(-A(78) &
       -A(109))*f(8))/2._/**/REALKIND+(CI*(-A(37)-A(39))*f(9))/2._/**/REALKIND
  M2(4) = ((-A(127)-A(128)-A(129)-A(130)-A(131)-A(132)-A(133)-A(137)-A(144)-A(183)-A(184)-A(186)-A(188)-A(189) &
       -A(190))*f(3))/2._/**/REALKIND+((-A(175)-A(176)-A(177)-A(178)-A(179)-A(180)-A(181)-A(182)-A(185)-A(187)-A(191) &
       -A(192))*f(3))/6._/**/REALKIND+(CI*(-A(134)-A(135)-A(138)-A(140)-A(141)-A(142))*f(4))/2._/**/REALKIND+((A(43)+A(44)+A(45) &
       +A(47)+A(59)+A(61)+A(63)+A(64)+A(81)+A(84)+A(115)+A(117))*f(5))/6._/**/REALKIND+((A(27)+A(31)+A(46)+A(48)+A(55)+A(56)+A(60) &
       +A(83)+A(85)+A(91)+A(92)+A(96)+A(97)+A(99)+A(118))*f(5))/2._/**/REALKIND+(CI*(A(52)+A(93)+A(95) &
       +A(100))*f(6))/2._/**/REALKIND+((A(82)+A(86)+A(116)+A(119))*f(7))/6._/**/REALKIND+((A(28)+A(53)+A(62)+A(98) &
       +A(120))*f(7))/2._/**/REALKIND+(CI*(A(54)+A(101))*f(8))/2._/**/REALKIND+(CI*(A(29)+A(32))*f(9))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pplnjjj_neexuxdddxg_1_/**/REALKIND
