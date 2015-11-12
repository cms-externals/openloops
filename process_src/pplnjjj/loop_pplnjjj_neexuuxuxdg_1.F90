
module ol_colourmatrix_pplnjjj_neexuuxuxdg_1_/**/REALKIND
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
  K1( 53,:) = [   0, -16,   2,  -6]
  K1( 54,:) = [ -16, -48,   0, -16]
  K1( 55,:) = [   2,   0,   6,   2]
  K1( 56,:) = [  -6, -16,   2,   0]
  K1( 57,:) = [   0,  -2,  16,   6]
  K1( 58,:) = [  -2,   0,   6,  16]
  K1( 59,:) = [  16,   6,   0,  -2]
  K1( 60,:) = [   6,  16,  -2,   0]
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
  K1( 73,:) = [   0,  16,  -2,   6]
  K1( 74,:) = [  16,   0,   6,  -2]
  K1( 75,:) = [  -2,   6,   0,  16]
  K1( 76,:) = [   6,  -2,  16,   0]
  K1( 77,:) = [   0,   2, -16,  -6]
  K1( 78,:) = [   2,   6,   0,   2]
  K1( 79,:) = [ -16,   0, -48, -16]
  K1( 80,:) = [  -6,   2, -16,   0]
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
  K1( 98,:) = [ -18,   0,   0,  18]
  K1( 99,:) = [ -18,   0, -54, -18]
  K1(100,:) = [   0,  18, -18,   0]
  K1(101,:) = [ -54, -18, -18,   0]
  K1(102,:) = [ -18, -54,   0, -18]
  K1(103,:) = [ -18,   0,   0,  18]
  K1(104,:) = [   0, -18,  18,   0]
  K1(105,:) = [   0,  18, -18,   0]
  K1(106,:) = [  18,   0,   0, -18]
  K1(107,:) = [ -18,   0, -54, -18]
  K1(108,:) = [   0, -18, -18, -54]
  K1(109,:) = [   0, -18,  18,   0]
  K1(110,:) = [ -18, -54,   0, -18]
  K1(111,:) = [  18,   0,   0, -18]
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
end module ol_colourmatrix_pplnjjj_neexuuxuxdg_1_/**/REALKIND



module ol_forced_parameters_pplnjjj_neexuuxuxdg_1_/**/REALKIND
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
end module ol_forced_parameters_pplnjjj_neexuuxuxdg_1_/**/REALKIND

module ol_loop_pplnjjj_neexuuxuxdg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(17), c(26)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:285)
  ! denominators
  complex(REALKIND), save :: den(342)
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
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_A(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_Q(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_W(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_AV_Q(wf(:,-4),wf(:,-6),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MW,1_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,80),ZERO,0_intkind1,wf(:,5))
  call vert_WQ_A(wf(:,4),wf(:,-5),wf(:,6))
  call vert_AV_Q(wf(:,5),wf(:,2),wf(:,7))
  call prop_Q_A(wf(:,6),Q(:,35),ZERO,0_intkind1,wf(:,8))
  call vert_VQ_A(wf(:,2),wf(:,-5),wf(:,9))
  call vert_AW_Q(wf(:,5),wf(:,4),wf(:,10))
  call prop_Q_A(wf(:,9),Q(:,44),ZERO,0_intkind1,wf(:,11))
  call vert_VQ_A(wf(:,-6),wf(:,-5),wf(:,12))
  call prop_Q_A(wf(:,12),Q(:,96),ZERO,0_intkind1,wf(:,13))
  call vert_AW_Q(wf(:,-4),wf(:,4),wf(:,14))
  call vert_VQ_A(wf(:,2),wf(:,13),wf(:,15))
  call prop_A_Q(wf(:,14),Q(:,19),ZERO,0_intkind1,wf(:,16))
  call vert_AV_Q(wf(:,-4),wf(:,2),wf(:,17))
  call vert_WQ_A(wf(:,4),wf(:,13),wf(:,18))
  call prop_A_Q(wf(:,17),Q(:,28),ZERO,0_intkind1,wf(:,19))
  call vert_AV_Q(wf(:,16),wf(:,-6),wf(:,20))
  call vert_UV_W(wf(:,2),Q(:,12),wf(:,-6),Q(:,64),wf(:,21))
  call vert_QA_V(wf(:,-5),wf(:,16),wf(:,22))
  call vert_AV_Q(wf(:,19),wf(:,-6),wf(:,23))
  call vert_QA_V(wf(:,8),wf(:,-4),wf(:,24))
  call vert_QA_V(wf(:,-2),wf(:,-4),wf(:,25))
  call vert_AV_Q(wf(:,-3),wf(:,-6),wf(:,26))
  call prop_A_Q(wf(:,26),Q(:,72),ZERO,0_intkind1,wf(:,27))
  call vert_AV_Q(wf(:,27),wf(:,25),wf(:,28))
  call vert_VQ_A(wf(:,25),wf(:,-5),wf(:,29))
  call vert_AW_Q(wf(:,27),wf(:,4),wf(:,30))
  call prop_Q_A(wf(:,29),Q(:,52),ZERO,0_intkind1,wf(:,31))
  call vert_AW_Q(wf(:,-3),wf(:,4),wf(:,32))
  call vert_VQ_A(wf(:,25),wf(:,13),wf(:,33))
  call prop_A_Q(wf(:,32),Q(:,11),ZERO,0_intkind1,wf(:,34))
  call vert_AV_Q(wf(:,-3),wf(:,25),wf(:,35))
  call prop_A_Q(wf(:,35),Q(:,28),ZERO,0_intkind1,wf(:,36))
  call vert_AV_Q(wf(:,34),wf(:,-6),wf(:,37))
  call vert_UV_W(wf(:,25),Q(:,20),wf(:,-6),Q(:,64),wf(:,38))
  call vert_QA_V(wf(:,-5),wf(:,34),wf(:,39))
  call vert_AV_Q(wf(:,36),wf(:,-6),wf(:,40))
  call vert_QA_V(wf(:,8),wf(:,-3),wf(:,41))
  call vert_VQ_A(wf(:,-6),wf(:,-2),wf(:,42))
  call prop_Q_A(wf(:,42),Q(:,68),ZERO,0_intkind1,wf(:,43))
  call vert_QA_V(wf(:,43),wf(:,-4),wf(:,44))
  call vert_QA_V(wf(:,43),wf(:,-3),wf(:,45))
  call vert_VQ_A(wf(:,45),wf(:,-5),wf(:,46))
  call vert_AV_Q(wf(:,-4),wf(:,45),wf(:,47))
  call vert_AV_Q(wf(:,-3),wf(:,44),wf(:,48))
  call vert_QA_V(wf(:,-2),wf(:,27),wf(:,49))
  call vert_VQ_A(wf(:,49),wf(:,-5),wf(:,50))
  call vert_AV_Q(wf(:,-4),wf(:,49),wf(:,51))
  call vert_QA_V(wf(:,-2),wf(:,5),wf(:,52))
  call vert_VQ_A(wf(:,52),wf(:,-5),wf(:,53))
  call vert_AV_Q(wf(:,-3),wf(:,52),wf(:,54))
  call counter_AV_Q(wf(:,5),wf(:,2),wf(:,55))
  call counter_AW_Q(wf(:,5),wf(:,4),wf(:,56))
  call counter_VQ_A(wf(:,2),wf(:,13),wf(:,57))
  call counter_WQ_A(wf(:,4),wf(:,13),wf(:,58))
  call counter_AV_Q(wf(:,16),wf(:,-6),wf(:,59))
  call counter_UV_W(wf(:,2),Q(:,12),wf(:,-6),Q(:,64),wf(:,60))
  call counter_AV_Q(wf(:,19),wf(:,-6),wf(:,61))
  call counter_VQ_A(wf(:,2),wf(:,-5),wf(:,62))
  call prop_A_Q(wf(:,10),Q(:,83),ZERO,0_intkind1,wf(:,63))
  call counter_WQ_A(wf(:,4),wf(:,-5),wf(:,64))
  call prop_A_Q(wf(:,7),Q(:,92),ZERO,0_intkind1,wf(:,65))
  call counter_QA_V(wf(:,-5),wf(:,16),wf(:,66))
  call prop_Q_A(wf(:,62),Q(:,44),ZERO,0_intkind1,wf(:,67))
  call prop_Q_A(wf(:,64),Q(:,35),ZERO,0_intkind1,wf(:,68))
  call vert_QA_V(wf(:,68),wf(:,-4),wf(:,69))
  call counter_VQ_A(wf(:,-6),wf(:,-5),wf(:,70))
  call prop_Q_A(wf(:,70),Q(:,96),ZERO,0_intkind1,wf(:,71))
  call vert_VQ_A(wf(:,2),wf(:,71),wf(:,72))
  call vert_WQ_A(wf(:,4),wf(:,71),wf(:,73))
  call counter_AV_Q(wf(:,-4),wf(:,2),wf(:,74))
  call prop_Q_A(wf(:,18),Q(:,99),ZERO,0_intkind1,wf(:,75))
  call counter_AW_Q(wf(:,-4),wf(:,4),wf(:,76))
  call prop_Q_A(wf(:,15),Q(:,108),ZERO,0_intkind1,wf(:,77))
  call counter_QA_V(wf(:,8),wf(:,-4),wf(:,78))
  call prop_A_Q(wf(:,74),Q(:,28),ZERO,0_intkind1,wf(:,79))
  call vert_AV_Q(wf(:,79),wf(:,-6),wf(:,80))
  call prop_A_Q(wf(:,76),Q(:,19),ZERO,0_intkind1,wf(:,81))
  call vert_AV_Q(wf(:,81),wf(:,-6),wf(:,82))
  call vert_QA_V(wf(:,-5),wf(:,81),wf(:,83))
  call counter_AV_Q(wf(:,-4),wf(:,-6),wf(:,84))
  call prop_A_Q(wf(:,84),Q(:,80),ZERO,0_intkind1,wf(:,85))
  call vert_AV_Q(wf(:,85),wf(:,2),wf(:,86))
  call vert_AW_Q(wf(:,85),wf(:,4),wf(:,87))
  call counter_AV_Q(wf(:,27),wf(:,25),wf(:,88))
  call counter_AW_Q(wf(:,27),wf(:,4),wf(:,89))
  call counter_VQ_A(wf(:,25),wf(:,13),wf(:,90))
  call counter_AV_Q(wf(:,34),wf(:,-6),wf(:,91))
  call counter_UV_W(wf(:,25),Q(:,20),wf(:,-6),Q(:,64),wf(:,92))
  call counter_AV_Q(wf(:,36),wf(:,-6),wf(:,93))
  call counter_VQ_A(wf(:,25),wf(:,-5),wf(:,94))
  call prop_A_Q(wf(:,30),Q(:,75),ZERO,0_intkind1,wf(:,95))
  call prop_A_Q(wf(:,28),Q(:,92),ZERO,0_intkind1,wf(:,96))
  call counter_QA_V(wf(:,-5),wf(:,34),wf(:,97))
  call prop_Q_A(wf(:,94),Q(:,52),ZERO,0_intkind1,wf(:,98))
  call vert_QA_V(wf(:,68),wf(:,-3),wf(:,99))
  call vert_VQ_A(wf(:,25),wf(:,71),wf(:,100))
  call counter_VQ_A(wf(:,45),wf(:,-5),wf(:,101))
  call counter_VQ_A(wf(:,49),wf(:,-5),wf(:,102))
  call counter_VQ_A(wf(:,52),wf(:,-5),wf(:,103))
  call counter_QA_V(wf(:,43),wf(:,-4),wf(:,104))
  call counter_AV_Q(wf(:,-4),wf(:,45),wf(:,105))
  call vert_AV_Q(wf(:,-3),wf(:,104),wf(:,106))
  call counter_AV_Q(wf(:,-4),wf(:,49),wf(:,107))
  call vert_QA_V(wf(:,-2),wf(:,85),wf(:,108))
  call vert_VQ_A(wf(:,108),wf(:,-5),wf(:,109))
  call vert_AV_Q(wf(:,-3),wf(:,108),wf(:,110))
  call counter_AV_Q(wf(:,-3),wf(:,25),wf(:,111))
  call counter_AW_Q(wf(:,-3),wf(:,4),wf(:,112))
  call prop_Q_A(wf(:,33),Q(:,116),ZERO,0_intkind1,wf(:,113))
  call counter_QA_V(wf(:,8),wf(:,-3),wf(:,114))
  call prop_A_Q(wf(:,111),Q(:,28),ZERO,0_intkind1,wf(:,115))
  call vert_AV_Q(wf(:,115),wf(:,-6),wf(:,116))
  call prop_A_Q(wf(:,112),Q(:,11),ZERO,0_intkind1,wf(:,117))
  call vert_AV_Q(wf(:,117),wf(:,-6),wf(:,118))
  call vert_QA_V(wf(:,-5),wf(:,117),wf(:,119))
  call counter_AV_Q(wf(:,-3),wf(:,-6),wf(:,120))
  call prop_A_Q(wf(:,120),Q(:,72),ZERO,0_intkind1,wf(:,121))
  call vert_AV_Q(wf(:,121),wf(:,25),wf(:,122))
  call vert_AW_Q(wf(:,121),wf(:,4),wf(:,123))
  call counter_QA_V(wf(:,43),wf(:,-3),wf(:,124))
  call vert_VQ_A(wf(:,124),wf(:,-5),wf(:,125))
  call counter_AV_Q(wf(:,-3),wf(:,44),wf(:,126))
  call vert_AV_Q(wf(:,-4),wf(:,124),wf(:,127))
  call counter_AV_Q(wf(:,-3),wf(:,52),wf(:,128))
  call vert_QA_V(wf(:,-2),wf(:,121),wf(:,129))
  call vert_VQ_A(wf(:,129),wf(:,-5),wf(:,130))
  call vert_AV_Q(wf(:,-4),wf(:,129),wf(:,131))
  call counter_QA_V(wf(:,-2),wf(:,27),wf(:,132))
  call vert_VQ_A(wf(:,132),wf(:,-5),wf(:,133))
  call vert_AV_Q(wf(:,-4),wf(:,132),wf(:,134))
  call counter_QA_V(wf(:,-2),wf(:,5),wf(:,135))
  call vert_VQ_A(wf(:,135),wf(:,-5),wf(:,136))
  call vert_AV_Q(wf(:,-3),wf(:,135),wf(:,137))
  call counter_VQ_A(wf(:,-6),wf(:,-2),wf(:,138))
  call prop_Q_A(wf(:,138),Q(:,68),ZERO,0_intkind1,wf(:,139))
  call vert_QA_V(wf(:,139),wf(:,-4),wf(:,140))
  call vert_QA_V(wf(:,139),wf(:,-3),wf(:,141))
  call vert_VQ_A(wf(:,141),wf(:,-5),wf(:,142))
  call vert_AV_Q(wf(:,-4),wf(:,141),wf(:,143))
  call vert_AV_Q(wf(:,-3),wf(:,140),wf(:,144))
  call counter_QA_V(wf(:,-2),wf(:,-4),wf(:,145))
  call vert_AV_Q(wf(:,27),wf(:,145),wf(:,146))
  call vert_VQ_A(wf(:,145),wf(:,-5),wf(:,147))
  call prop_Q_A(wf(:,147),Q(:,52),ZERO,0_intkind1,wf(:,148))
  call vert_VQ_A(wf(:,145),wf(:,13),wf(:,149))
  call vert_AV_Q(wf(:,-3),wf(:,145),wf(:,150))
  call prop_A_Q(wf(:,150),Q(:,28),ZERO,0_intkind1,wf(:,151))
  call vert_UV_W(wf(:,145),Q(:,20),wf(:,-6),Q(:,64),wf(:,152))
  call vert_AV_Q(wf(:,151),wf(:,-6),wf(:,153))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,154))
  call vert_AV_Q(wf(:,5),wf(:,154),wf(:,155))
  call vert_VQ_A(wf(:,154),wf(:,-5),wf(:,156))
  call prop_Q_A(wf(:,156),Q(:,44),ZERO,0_intkind1,wf(:,157))
  call vert_VQ_A(wf(:,154),wf(:,13),wf(:,158))
  call vert_AV_Q(wf(:,-4),wf(:,154),wf(:,159))
  call prop_A_Q(wf(:,159),Q(:,28),ZERO,0_intkind1,wf(:,160))
  call vert_UV_W(wf(:,154),Q(:,12),wf(:,-6),Q(:,64),wf(:,161))
  call vert_AV_Q(wf(:,160),wf(:,-6),wf(:,162))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,163))
  call vert_VQ_A(wf(:,163),wf(:,-5),wf(:,164))
  call vert_AV_Q(wf(:,5),wf(:,163),wf(:,165))
  call counter_A_Q(ctqq,wf(:,5),Q(:,80),wf(:,166))
  call prop_A_Q(wf(:,166),Q(:,80),ZERO,0_intkind1,wf(:,167))
  call vert_AV_Q(wf(:,167),wf(:,2),wf(:,168))
  call vert_AW_Q(wf(:,167),wf(:,4),wf(:,169))
  call counter_Q_A(ctqq,wf(:,8),Q(:,35),wf(:,170))
  call counter_Q_A(ctqq,wf(:,11),Q(:,44),wf(:,171))
  call vert_AV_Q(wf(:,-4),wf(:,163),wf(:,172))
  call vert_AV_Q(wf(:,16),wf(:,163),wf(:,173))
  call counter_A_Q(ctqq,wf(:,16),Q(:,19),wf(:,174))
  call counter_A_Q(ctqq,wf(:,19),Q(:,28),wf(:,175))
  call counter_Q_A(ctqq,wf(:,13),Q(:,96),wf(:,176))
  call prop_Q_A(wf(:,176),Q(:,96),ZERO,0_intkind1,wf(:,177))
  call vert_VQ_A(wf(:,2),wf(:,177),wf(:,178))
  call vert_WQ_A(wf(:,4),wf(:,177),wf(:,179))
  call vert_VQ_A(wf(:,-6),wf(:,8),wf(:,180))
  call prop_A_Q(wf(:,172),Q(:,28),ZERO,0_intkind1,wf(:,181))
  call prop_Q_A(wf(:,164),Q(:,44),ZERO,0_intkind1,wf(:,182))
  call vert_UV_W(wf(:,163),Q(:,12),wf(:,-6),Q(:,64),wf(:,183))
  call vert_VQ_A(wf(:,21),wf(:,-5),wf(:,184))
  call prop_Q_A(wf(:,184),Q(:,108),ZERO,0_intkind1,wf(:,185))
  call vert_AV_Q(wf(:,-4),wf(:,21),wf(:,186))
  call prop_A_Q(wf(:,186),Q(:,92),ZERO,0_intkind1,wf(:,187))
  call vert_VQ_A(wf(:,-6),wf(:,11),wf(:,188))
  call prop_Q_A(wf(:,188),Q(:,108),ZERO,0_intkind1,wf(:,189))
  call prop_Q_A(wf(:,180),Q(:,99),ZERO,0_intkind1,wf(:,190))
  call counter_V_V(ctGG,wf(:,21),Q(:,76),wf(:,191))
  call prop_A_Q(wf(:,20),Q(:,83),ZERO,0_intkind1,wf(:,192))
  call prop_A_Q(wf(:,23),Q(:,92),ZERO,0_intkind1,wf(:,193))
  call counter_V_V(ctGG,wf(:,25),Q(:,20),wf(:,194))
  call vert_VQ_A(wf(:,194),wf(:,-5),wf(:,195))
  call vert_AV_Q(wf(:,27),wf(:,194),wf(:,196))
  call counter_A_Q(ctqq,wf(:,27),Q(:,72),wf(:,197))
  call prop_A_Q(wf(:,197),Q(:,72),ZERO,0_intkind1,wf(:,198))
  call vert_AV_Q(wf(:,198),wf(:,25),wf(:,199))
  call vert_AW_Q(wf(:,198),wf(:,4),wf(:,200))
  call counter_Q_A(ctqq,wf(:,31),Q(:,52),wf(:,201))
  call vert_AV_Q(wf(:,-3),wf(:,194),wf(:,202))
  call vert_AV_Q(wf(:,34),wf(:,194),wf(:,203))
  call counter_A_Q(ctqq,wf(:,34),Q(:,11),wf(:,204))
  call counter_A_Q(ctqq,wf(:,36),Q(:,28),wf(:,205))
  call vert_VQ_A(wf(:,25),wf(:,177),wf(:,206))
  call prop_A_Q(wf(:,202),Q(:,28),ZERO,0_intkind1,wf(:,207))
  call prop_Q_A(wf(:,195),Q(:,52),ZERO,0_intkind1,wf(:,208))
  call vert_UV_W(wf(:,194),Q(:,20),wf(:,-6),Q(:,64),wf(:,209))
  call vert_VQ_A(wf(:,38),wf(:,-5),wf(:,210))
  call prop_Q_A(wf(:,210),Q(:,116),ZERO,0_intkind1,wf(:,211))
  call vert_AV_Q(wf(:,-3),wf(:,38),wf(:,212))
  call prop_A_Q(wf(:,212),Q(:,92),ZERO,0_intkind1,wf(:,213))
  call vert_VQ_A(wf(:,-6),wf(:,31),wf(:,214))
  call prop_Q_A(wf(:,214),Q(:,116),ZERO,0_intkind1,wf(:,215))
  call counter_V_V(ctGG,wf(:,38),Q(:,84),wf(:,216))
  call prop_A_Q(wf(:,37),Q(:,75),ZERO,0_intkind1,wf(:,217))
  call prop_A_Q(wf(:,40),Q(:,92),ZERO,0_intkind1,wf(:,218))
  call counter_Q_A(ctqq,wf(:,43),Q(:,68),wf(:,219))
  call prop_Q_A(wf(:,219),Q(:,68),ZERO,0_intkind1,wf(:,220))
  call vert_QA_V(wf(:,220),wf(:,-3),wf(:,221))
  call vert_QA_V(wf(:,220),wf(:,-4),wf(:,222))
  call counter_V_V(ctGG,wf(:,45),Q(:,76),wf(:,223))
  call counter_V_V(ctGG,wf(:,44),Q(:,84),wf(:,224))
  call vert_VQ_A(wf(:,44),wf(:,-5),wf(:,225))
  call prop_Q_A(wf(:,225),Q(:,116),ZERO,0_intkind1,wf(:,226))
  call prop_A_Q(wf(:,48),Q(:,92),ZERO,0_intkind1,wf(:,227))
  call prop_Q_A(wf(:,46),Q(:,108),ZERO,0_intkind1,wf(:,228))
  call prop_A_Q(wf(:,47),Q(:,92),ZERO,0_intkind1,wf(:,229))
  call vert_QA_V(wf(:,-2),wf(:,198),wf(:,230))
  call counter_V_V(ctGG,wf(:,49),Q(:,76),wf(:,231))
  call prop_Q_A(wf(:,50),Q(:,108),ZERO,0_intkind1,wf(:,232))
  call prop_A_Q(wf(:,51),Q(:,92),ZERO,0_intkind1,wf(:,233))
  call counter_V_V(ctGG,wf(:,52),Q(:,84),wf(:,234))
  call vert_QA_V(wf(:,-2),wf(:,167),wf(:,235))
  call prop_Q_A(wf(:,53),Q(:,116),ZERO,0_intkind1,wf(:,236))
  call prop_A_Q(wf(:,54),Q(:,92),ZERO,0_intkind1,wf(:,237))
  call vert_AV_Q(wf(:,16),wf(:,2),wf(:,238))
  call prop_A_Q(wf(:,238),Q(:,31),ZERO,0_intkind1,wf(:,239))
  call vert_VQ_A(wf(:,2),wf(:,8),wf(:,240))
  call prop_Q_A(wf(:,240),Q(:,47),ZERO,0_intkind1,wf(:,241))
  call vert_AW_Q(wf(:,19),wf(:,4),wf(:,242))
  call prop_A_Q(wf(:,242),Q(:,31),ZERO,0_intkind1,wf(:,243))
  call vert_WQ_A(wf(:,4),wf(:,11),wf(:,244))
  call prop_Q_A(wf(:,244),Q(:,47),ZERO,0_intkind1,wf(:,245))
  call vert_AV_Q(wf(:,34),wf(:,25),wf(:,246))
  call prop_A_Q(wf(:,246),Q(:,31),ZERO,0_intkind1,wf(:,247))
  call vert_VQ_A(wf(:,25),wf(:,8),wf(:,248))
  call prop_Q_A(wf(:,248),Q(:,55),ZERO,0_intkind1,wf(:,249))
  call vert_AW_Q(wf(:,36),wf(:,4),wf(:,250))
  call prop_A_Q(wf(:,250),Q(:,31),ZERO,0_intkind1,wf(:,251))
  call vert_WQ_A(wf(:,4),wf(:,31),wf(:,252))
  call prop_Q_A(wf(:,252),Q(:,55),ZERO,0_intkind1,wf(:,253))
  call vert_QA_V(wf(:,8),wf(:,27),wf(:,254))
  call vert_QA_V(wf(:,-5),wf(:,95),wf(:,255))
  call vert_QA_V(wf(:,8),wf(:,5),wf(:,256))
  call vert_QA_V(wf(:,-5),wf(:,63),wf(:,257))
  call vert_QA_V(wf(:,13),wf(:,34),wf(:,258))
  call vert_QA_V(wf(:,13),wf(:,16),wf(:,259))
  call vert_QA_V(wf(:,75),wf(:,-3),wf(:,260))
  call vert_QA_V(wf(:,75),wf(:,-4),wf(:,261))
  call vert_VQ_A(wf(:,39),wf(:,-2),wf(:,262))
  call prop_Q_A(wf(:,262),Q(:,47),ZERO,0_intkind1,wf(:,263))
  call vert_AV_Q(wf(:,-4),wf(:,39),wf(:,264))
  call prop_A_Q(wf(:,264),Q(:,59),ZERO,0_intkind1,wf(:,265))
  call vert_UV_W(wf(:,39),Q(:,43),wf(:,-6),Q(:,64),wf(:,266))
  call vert_QA_V(wf(:,-5),wf(:,217),wf(:,267))
  call vert_VQ_A(wf(:,22),wf(:,-2),wf(:,268))
  call prop_Q_A(wf(:,268),Q(:,55),ZERO,0_intkind1,wf(:,269))
  call vert_AV_Q(wf(:,-3),wf(:,22),wf(:,270))
  call prop_A_Q(wf(:,270),Q(:,59),ZERO,0_intkind1,wf(:,271))
  call vert_UV_W(wf(:,22),Q(:,51),wf(:,-6),Q(:,64),wf(:,272))
  call vert_QA_V(wf(:,-5),wf(:,192),wf(:,273))
  call vert_VQ_A(wf(:,41),wf(:,-2),wf(:,274))
  call prop_Q_A(wf(:,274),Q(:,47),ZERO,0_intkind1,wf(:,275))
  call vert_VQ_A(wf(:,24),wf(:,-2),wf(:,276))
  call prop_Q_A(wf(:,276),Q(:,55),ZERO,0_intkind1,wf(:,277))
  call vert_AV_Q(wf(:,-4),wf(:,41),wf(:,278))
  call prop_A_Q(wf(:,278),Q(:,59),ZERO,0_intkind1,wf(:,279))
  call vert_AV_Q(wf(:,-3),wf(:,24),wf(:,280))
  call prop_A_Q(wf(:,280),Q(:,59),ZERO,0_intkind1,wf(:,281))
  call vert_UV_W(wf(:,41),Q(:,43),wf(:,-6),Q(:,64),wf(:,282))
  call vert_QA_V(wf(:,190),wf(:,-3),wf(:,283))
  call vert_UV_W(wf(:,24),Q(:,51),wf(:,-6),Q(:,64),wf(:,284))
  call vert_QA_V(wf(:,190),wf(:,-4),wf(:,285))

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
  den(22) = 1 / (Q(5,76))
  den(27) = 1 / (Q(5,20))
  den(28) = 1 / (Q(5,72))
  den(32) = 1 / (Q(5,52))
  den(36) = 1 / (Q(5,11))
  den(42) = 1 / (Q(5,84))
  den(47) = 1 / (Q(5,68))
  den(60) = 1 / (Q(5,83))
  den(63) = 1 / (Q(5,92))
  den(66) = 1 / (Q(5,99))
  den(69) = 1 / (Q(5,108))
  den(72) = 1 / (Q(5,75))
  den(78) = 1 / (Q(5,116))
  den(106) = 1 / (Q(5,51))
  den(148) = 1 / (Q(5,43))
  den(204) = 1 / (Q(5,31))
  den(207) = 1 / (Q(5,47))
  den(216) = 1 / (Q(5,55))
  den(223) = 1 / (Q(5,107))
  den(227) = 1 / (Q(5,115))
  den(237) = 1 / (Q(5,59))

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
  den(23) = den(2)*den(22)
  den(24) = den(15)*den(23)
  den(25) = den(6)*den(19)
  den(26) = den(6)*den(23)
  den(29) = den(27)*den(28)
  den(30) = den(6)*den(29)
  den(31) = den(1)*den(28)
  den(33) = den(27)*den(32)
  den(34) = den(31)*den(33)
  den(35) = den(12)*den(27)
  den(37) = den(1)*den(36)
  den(38) = den(35)*den(37)
  den(39) = den(18)*den(27)
  den(40) = den(17)*den(39)
  den(41) = den(33)*den(37)
  den(43) = den(27)*den(42)
  den(44) = den(37)*den(43)
  den(45) = den(6)*den(39)
  den(46) = den(6)*den(43)
  den(48) = den(42)*den(47)
  den(49) = den(37)*den(48)
  den(50) = den(22)*den(47)
  den(51) = den(15)*den(50)
  den(52) = den(6)*den(50)
  den(53) = den(6)*den(48)
  den(54) = den(22)*den(28)
  den(55) = den(15)*den(54)
  den(56) = den(6)*den(54)
  den(57) = den(3)*den(42)
  den(58) = den(37)*den(57)
  den(59) = den(6)*den(57)
  den(61) = den(8)*den(60)
  den(62) = den(2)*den(61)
  den(64) = den(4)*den(63)
  den(65) = den(1)*den(64)
  den(67) = den(17)*den(66)
  den(68) = den(2)*den(67)
  den(70) = den(13)*den(69)
  den(71) = den(1)*den(70)
  den(73) = den(31)*den(72)
  den(74) = den(27)*den(73)
  den(75) = den(29)*den(63)
  den(76) = den(1)*den(75)
  den(77) = den(27)*den(67)
  den(79) = den(35)*den(78)
  den(80) = den(1)*den(79)
  den(81) = den(2)**2
  den(82) = den(61)*den(81)
  den(83) = den(3)*den(81)
  den(84) = den(6)*den(83)
  den(85) = den(3)**2
  den(86) = den(2)*den(85)
  den(87) = den(6)*den(86)
  den(88) = den(1)*den(85)
  den(89) = den(10)*den(88)
  den(90) = den(6)*den(64)
  den(91) = den(10)*den(61)
  den(92) = den(67)*den(81)
  den(93) = den(15)*den(81)
  den(94) = den(12)*den(93)
  den(95) = den(15)*den(70)
  den(96) = den(19)*den(67)
  den(97) = den(12)**2
  den(98) = den(2)*den(97)
  den(99) = den(15)*den(98)
  den(100) = den(1)*den(97)
  den(101) = den(19)*den(100)
  den(102) = den(18)*den(81)
  den(103) = den(6)*den(102)
  den(104) = den(9)*den(81)
  den(105) = den(15)*den(104)
  den(107) = den(15)*den(106)
  den(108) = den(81)*den(107)
  den(109) = den(6)*den(106)
  den(110) = den(81)*den(109)
  den(111) = den(23)*den(69)
  den(112) = den(15)*den(111)
  den(113) = den(23)*den(63)
  den(114) = den(6)*den(113)
  den(115) = den(10)*den(69)
  den(116) = den(15)*den(115)
  den(117) = den(6)*den(66)
  den(118) = den(19)*den(117)
  den(119) = den(23)*den(109)
  den(120) = den(15)*den(60)
  den(121) = den(10)*den(120)
  den(122) = den(23)*den(107)
  den(123) = den(19)*den(63)
  den(124) = den(6)*den(123)
  den(125) = den(27)**2
  den(126) = den(73)*den(125)
  den(127) = den(28)*den(125)
  den(128) = den(6)*den(127)
  den(129) = den(28)**2
  den(130) = den(27)*den(129)
  den(131) = den(6)*den(130)
  den(132) = den(1)*den(129)
  den(133) = den(33)*den(132)
  den(134) = den(6)*den(75)
  den(135) = den(33)*den(73)
  den(136) = den(67)*den(125)
  den(137) = den(37)*den(125)
  den(138) = den(12)*den(137)
  den(139) = den(37)*den(79)
  den(140) = den(39)*den(67)
  den(141) = den(27)*den(97)
  den(142) = den(37)*den(141)
  den(143) = den(39)*den(100)
  den(144) = den(18)*den(125)
  den(145) = den(6)*den(144)
  den(146) = den(32)*den(125)
  den(147) = den(37)*den(146)
  den(149) = den(37)*den(148)
  den(150) = den(125)*den(149)
  den(151) = den(6)*den(148)
  den(152) = den(125)*den(151)
  den(153) = den(43)*den(78)
  den(154) = den(37)*den(153)
  den(155) = den(43)*den(63)
  den(156) = den(6)*den(155)
  den(157) = den(33)*den(78)
  den(158) = den(37)*den(157)
  den(159) = den(39)*den(117)
  den(160) = den(43)*den(151)
  den(161) = den(37)*den(72)
  den(162) = den(33)*den(161)
  den(163) = den(43)*den(149)
  den(164) = den(39)*den(63)
  den(165) = den(6)*den(164)
  den(166) = den(47)**2
  den(167) = den(22)*den(166)
  den(168) = den(15)*den(167)
  den(169) = den(6)*den(167)
  den(170) = den(42)*den(166)
  den(171) = den(37)*den(170)
  den(172) = den(151)*den(166)
  den(173) = den(50)*den(109)
  den(174) = den(48)*den(151)
  den(175) = den(48)*den(78)
  den(176) = den(37)*den(175)
  den(177) = den(50)*den(107)
  den(178) = den(48)*den(63)
  den(179) = den(6)*den(178)
  den(180) = den(48)*den(149)
  den(181) = den(50)*den(69)
  den(182) = den(15)*den(181)
  den(183) = den(50)*den(63)
  den(184) = den(6)*den(183)
  den(185) = den(22)*den(129)
  den(186) = den(15)*den(185)
  den(187) = den(6)*den(185)
  den(188) = den(54)*den(109)
  den(189) = den(54)*den(107)
  den(190) = den(54)*den(69)
  den(191) = den(15)*den(190)
  den(192) = den(54)*den(63)
  den(193) = den(6)*den(192)
  den(194) = den(57)*den(151)
  den(195) = den(42)*den(85)
  den(196) = den(37)*den(195)
  den(197) = den(6)*den(195)
  den(198) = den(57)*den(149)
  den(199) = den(57)*den(78)
  den(200) = den(37)*den(199)
  den(201) = den(57)*den(63)
  den(202) = den(6)*den(201)
  den(203) = den(2)*den(15)
  den(205) = den(203)*den(204)
  den(206) = den(2)*den(6)
  den(208) = den(206)*den(207)
  den(209) = den(1)*den(19)
  den(210) = den(204)*den(209)
  den(211) = den(1)*den(10)
  den(212) = den(207)*den(211)
  den(213) = den(27)*den(37)
  den(214) = den(204)*den(213)
  den(215) = den(6)*den(27)
  den(217) = den(215)*den(216)
  den(218) = den(1)*den(39)
  den(219) = den(204)*den(218)
  den(220) = den(1)*den(33)
  den(221) = den(216)*den(220)
  den(222) = den(6)*den(28)
  den(224) = den(222)*den(223)
  den(225) = den(73)*den(223)
  den(226) = den(3)*den(6)
  den(228) = den(226)*den(227)
  den(229) = den(61)*den(227)
  den(230) = den(12)*den(37)
  den(231) = den(223)*den(230)
  den(232) = den(12)*den(15)
  den(233) = den(227)*den(232)
  den(234) = den(67)*den(223)
  den(235) = den(67)*den(227)
  den(236) = den(149)*den(207)
  den(238) = den(149)*den(237)
  den(239) = den(149)*den(223)
  den(240) = den(161)*den(223)
  den(241) = den(107)*den(216)
  den(242) = den(107)*den(237)
  den(243) = den(107)*den(227)
  den(244) = den(120)*den(227)
  den(245) = den(151)*den(207)
  den(246) = den(109)*den(216)
  den(247) = den(151)*den(237)
  den(248) = den(109)*den(237)
  den(249) = den(151)*den(223)
  den(250) = den(117)*den(223)
  den(251) = den(109)*den(227)
  den(252) = den(117)*den(227)
  den(253) = den(1)*den(2)
  den(254) = den(1)*den(27)
  den(255) = den(2)*den(3)*den(6)
  den(256) = den(1)*den(3)*den(10)
  den(257) = den(1)*den(2)*den(3)
  den(258) = den(2)*den(12)*den(15)
  den(259) = den(1)*den(12)*den(19)
  den(260) = den(1)*den(2)*den(12)
  den(261) = den(2)*den(107)
  den(262) = den(2)*den(120)
  den(263) = den(2)*den(109)
  den(264) = den(2)*den(117)
  den(265) = den(1)*den(123)
  den(266) = den(1)*den(115)
  den(267) = den(1)*den(113)
  den(268) = den(1)*den(111)
  den(269) = den(1)*den(23)
  den(270) = den(6)*den(27)*den(28)
  den(271) = den(1)*den(28)*den(33)
  den(272) = den(1)*den(27)*den(28)
  den(273) = den(12)*den(27)*den(37)
  den(274) = den(1)*den(12)*den(39)
  den(275) = den(1)*den(12)*den(27)
  den(276) = den(27)*den(149)
  den(277) = den(27)*den(161)
  den(278) = den(27)*den(151)
  den(279) = den(27)*den(117)
  den(280) = den(1)*den(164)
  den(281) = den(1)*den(157)
  den(282) = den(1)*den(155)
  den(283) = den(1)*den(153)
  den(284) = den(1)*den(43)
  den(285) = den(47)*den(149)
  den(286) = den(37)*den(47)
  den(287) = den(47)*den(107)
  den(288) = den(15)*den(47)
  den(289) = den(47)*den(151)
  den(290) = den(47)*den(109)
  den(291) = den(6)*den(47)
  den(292) = den(1)*den(183)
  den(293) = den(1)*den(181)
  den(294) = den(1)*den(50)
  den(295) = den(1)*den(178)
  den(296) = den(1)*den(175)
  den(297) = den(1)*den(48)
  den(298) = den(1)*den(47)
  den(299) = den(28)*den(107)
  den(300) = den(15)*den(28)
  den(301) = den(28)*den(109)
  den(302) = den(1)*den(192)
  den(303) = den(1)*den(190)
  den(304) = den(1)*den(54)
  den(305) = den(3)*den(149)
  den(306) = den(3)*den(37)
  den(307) = den(3)*den(151)
  den(308) = den(1)*den(201)
  den(309) = den(1)*den(199)
  den(310) = den(1)*den(57)
  den(311) = den(3)*den(208)
  den(312) = den(2)*den(228)
  den(313) = den(3)*den(212)
  den(314) = den(2)*den(229)
  den(315) = den(12)*den(205)
  den(316) = den(2)*den(233)
  den(317) = den(12)*den(210)
  den(318) = den(2)*den(235)
  den(319) = den(2)*den(243)
  den(320) = den(2)*den(244)
  den(321) = den(2)*den(251)
  den(322) = den(2)*den(252)
  den(323) = den(28)*den(217)
  den(324) = den(27)*den(224)
  den(325) = den(28)*den(221)
  den(326) = den(27)*den(225)
  den(327) = den(12)*den(214)
  den(328) = den(27)*den(231)
  den(329) = den(12)*den(219)
  den(330) = den(27)*den(234)
  den(331) = den(27)*den(239)
  den(332) = den(27)*den(240)
  den(333) = den(27)*den(249)
  den(334) = den(27)*den(250)
  den(335) = den(47)*den(238)
  den(336) = den(47)*den(242)
  den(337) = den(47)*den(247)
  den(338) = den(47)*den(248)
  den(339) = den(28)*den(241)
  den(340) = den(28)*den(246)
  den(341) = den(3)*den(236)
  den(342) = den(3)*den(245)

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
  A(5) = cont_QA(wf(:,11),wf(:,20)) * den(21)
  A(6) = cont_VV(wf(:,21),wf(:,22)) * den(24)
  A(7) = cont_QA(wf(:,8),wf(:,23)) * den(25)
  A(8) = cont_VV(wf(:,21),wf(:,24)) * den(26)
  A(9) = cont_QA(wf(:,8),wf(:,28)) * den(30)
  A(10) = cont_QA(wf(:,30),wf(:,31)) * den(34)
  A(11) = cont_QA(wf(:,33),wf(:,34)) * den(38)
  A(12) = cont_QA(wf(:,18),wf(:,36)) * den(40)
  A(13) = cont_QA(wf(:,31),wf(:,37)) * den(41)
  A(14) = cont_VV(wf(:,38),wf(:,39)) * den(44)
  A(15) = cont_QA(wf(:,8),wf(:,40)) * den(45)
  A(16) = cont_VV(wf(:,38),wf(:,41)) * den(46)
  A(17) = cont_VV(wf(:,39),wf(:,44)) * den(49)
  A(18) = cont_QA(wf(:,16),wf(:,46)) * den(51)
  A(19) = cont_QA(wf(:,8),wf(:,47)) * den(52)
  A(20) = cont_QA(wf(:,8),wf(:,48)) * den(53)
  A(21) = cont_QA(wf(:,16),wf(:,50)) * den(55)
  A(22) = cont_QA(wf(:,8),wf(:,51)) * den(56)
  A(23) = cont_QA(wf(:,34),wf(:,53)) * den(58)
  A(24) = cont_QA(wf(:,8),wf(:,54)) * den(59)

  A(25) = cont_QA(wf(:,8),wf(:,55)) * den(7)
  A(26) = cont_QA(wf(:,11),wf(:,56)) * den(11)
  A(27) = cont_QA(wf(:,16),wf(:,57)) * den(16)
  A(28) = cont_QA(wf(:,19),wf(:,58)) * den(20)
  A(29) = cont_QA(wf(:,11),wf(:,59)) * den(21)
  A(30) = cont_VV(wf(:,22),wf(:,60)) * den(24)
  A(31) = cont_QA(wf(:,8),wf(:,61)) * den(25)
  A(32) = cont_VV(wf(:,24),wf(:,60)) * den(26)
  A(33) = cont_QA(wf(:,62),wf(:,63)) * den(62)
  A(34) = cont_QA(wf(:,64),wf(:,65)) * den(65)
  A(35) = cont_VV(wf(:,21),wf(:,66)) * den(24)
  A(36) = cont_QA(wf(:,20),wf(:,67)) * den(21)
  A(37) = cont_QA(wf(:,23),wf(:,68)) * den(25)
  A(38) = cont_VV(wf(:,21),wf(:,69)) * den(26)
  A(39) = cont_QA(wf(:,16),wf(:,72)) * den(16)
  A(40) = cont_QA(wf(:,19),wf(:,73)) * den(20)
  A(41) = cont_QA(wf(:,74),wf(:,75)) * den(68)
  A(42) = cont_QA(wf(:,76),wf(:,77)) * den(71)
  A(43) = cont_VV(wf(:,21),wf(:,78)) * den(26)
  A(44) = cont_QA(wf(:,8),wf(:,80)) * den(25)
  A(45) = cont_QA(wf(:,11),wf(:,82)) * den(21)
  A(46) = cont_VV(wf(:,21),wf(:,83)) * den(24)
  A(47) = cont_QA(wf(:,8),wf(:,86)) * den(7)
  A(48) = cont_QA(wf(:,11),wf(:,87)) * den(11)
  A(49) = cont_QA(wf(:,8),wf(:,88)) * den(30)
  A(50) = cont_QA(wf(:,31),wf(:,89)) * den(34)
  A(51) = cont_QA(wf(:,34),wf(:,90)) * den(38)
  A(52) = cont_QA(wf(:,36),wf(:,58)) * den(40)
  A(53) = cont_QA(wf(:,31),wf(:,91)) * den(41)
  A(54) = cont_VV(wf(:,39),wf(:,92)) * den(44)
  A(55) = cont_QA(wf(:,8),wf(:,93)) * den(45)
  A(56) = cont_VV(wf(:,41),wf(:,92)) * den(46)
  A(57) = cont_QA(wf(:,94),wf(:,95)) * den(74)
  A(58) = cont_QA(wf(:,64),wf(:,96)) * den(76)
  A(59) = cont_VV(wf(:,38),wf(:,97)) * den(44)
  A(60) = cont_QA(wf(:,37),wf(:,98)) * den(41)
  A(61) = cont_QA(wf(:,40),wf(:,68)) * den(45)
  A(62) = cont_VV(wf(:,38),wf(:,99)) * den(46)
  A(63) = cont_QA(wf(:,34),wf(:,100)) * den(38)
  A(64) = cont_QA(wf(:,36),wf(:,73)) * den(40)
  A(65) = cont_VV(wf(:,44),wf(:,97)) * den(49)
  A(66) = cont_QA(wf(:,16),wf(:,101)) * den(51)
  A(67) = cont_QA(wf(:,47),wf(:,68)) * den(52)
  A(68) = cont_QA(wf(:,48),wf(:,68)) * den(53)
  A(69) = cont_QA(wf(:,16),wf(:,102)) * den(55)
  A(70) = cont_QA(wf(:,51),wf(:,68)) * den(56)
  A(71) = cont_QA(wf(:,34),wf(:,103)) * den(58)
  A(72) = cont_QA(wf(:,54),wf(:,68)) * den(59)
  A(73) = cont_VV(wf(:,39),wf(:,104)) * den(49)
  A(74) = cont_QA(wf(:,8),wf(:,105)) * den(52)
  A(75) = cont_QA(wf(:,8),wf(:,106)) * den(53)
  A(76) = cont_QA(wf(:,46),wf(:,81)) * den(51)
  A(77) = cont_QA(wf(:,8),wf(:,107)) * den(56)
  A(78) = cont_QA(wf(:,50),wf(:,81)) * den(55)
  A(79) = cont_QA(wf(:,34),wf(:,109)) * den(58)
  A(80) = cont_QA(wf(:,8),wf(:,110)) * den(59)
  A(81) = cont_QA(wf(:,75),wf(:,111)) * den(77)
  A(82) = cont_QA(wf(:,112),wf(:,113)) * den(80)
  A(83) = cont_VV(wf(:,38),wf(:,114)) * den(46)
  A(84) = cont_QA(wf(:,8),wf(:,116)) * den(45)
  A(85) = cont_QA(wf(:,31),wf(:,118)) * den(41)
  A(86) = cont_VV(wf(:,38),wf(:,119)) * den(44)
  A(87) = cont_QA(wf(:,8),wf(:,122)) * den(30)
  A(88) = cont_QA(wf(:,31),wf(:,123)) * den(34)
  A(89) = cont_QA(wf(:,16),wf(:,125)) * den(51)
  A(90) = cont_QA(wf(:,8),wf(:,126)) * den(53)
  A(91) = cont_QA(wf(:,8),wf(:,127)) * den(52)
  A(92) = cont_VV(wf(:,44),wf(:,119)) * den(49)
  A(93) = cont_QA(wf(:,8),wf(:,128)) * den(59)
  A(94) = cont_QA(wf(:,53),wf(:,117)) * den(58)
  A(95) = cont_QA(wf(:,16),wf(:,130)) * den(55)
  A(96) = cont_QA(wf(:,8),wf(:,131)) * den(56)
  A(97) = cont_QA(wf(:,16),wf(:,133)) * den(55)
  A(98) = cont_QA(wf(:,8),wf(:,134)) * den(56)
  A(99) = cont_QA(wf(:,34),wf(:,136)) * den(58)
  A(100) = cont_QA(wf(:,8),wf(:,137)) * den(59)
  A(101) = cont_VV(wf(:,39),wf(:,140)) * den(49)
  A(102) = cont_QA(wf(:,16),wf(:,142)) * den(51)
  A(103) = cont_QA(wf(:,8),wf(:,143)) * den(52)
  A(104) = cont_QA(wf(:,8),wf(:,144)) * den(53)
  A(105) = cont_QA(wf(:,8),wf(:,146)) * den(30)
  A(106) = cont_QA(wf(:,30),wf(:,148)) * den(34)
  A(107) = cont_QA(wf(:,34),wf(:,149)) * den(38)
  A(108) = cont_QA(wf(:,18),wf(:,151)) * den(40)
  A(109) = cont_QA(wf(:,37),wf(:,148)) * den(41)
  A(110) = cont_VV(wf(:,39),wf(:,152)) * den(44)
  A(111) = cont_QA(wf(:,8),wf(:,153)) * den(45)
  A(112) = cont_VV(wf(:,41),wf(:,152)) * den(46)
  A(113) = cont_QA(wf(:,8),wf(:,155)) * den(7)
  A(114) = cont_QA(wf(:,10),wf(:,157)) * den(11)
  A(115) = cont_QA(wf(:,16),wf(:,158)) * den(16)
  A(116) = cont_QA(wf(:,18),wf(:,160)) * den(20)
  A(117) = cont_QA(wf(:,20),wf(:,157)) * den(21)
  A(118) = cont_VV(wf(:,22),wf(:,161)) * den(24)
  A(119) = cont_QA(wf(:,8),wf(:,162)) * den(25)
  A(120) = cont_VV(wf(:,24),wf(:,161)) * den(26)
  A(121) = cont_QA(wf(:,63),wf(:,164)) * den(82)
  A(122) = cont_QA(wf(:,8),wf(:,165)) * den(84)
  A(123) = cont_QA(wf(:,8),wf(:,168)) * den(87)
  A(124) = cont_QA(wf(:,11),wf(:,169)) * den(89)
  A(125) = cont_QA(wf(:,65),wf(:,170)) * den(90)
  A(126) = cont_QA(wf(:,63),wf(:,171)) * den(91)
  A(127) = cont_QA(wf(:,75),wf(:,172)) * den(92)
  A(128) = cont_QA(wf(:,13),wf(:,173)) * den(94)
  A(129) = cont_QA(wf(:,77),wf(:,174)) * den(95)
  A(130) = cont_QA(wf(:,75),wf(:,175)) * den(96)
  A(131) = cont_QA(wf(:,16),wf(:,178)) * den(99)
  A(132) = cont_QA(wf(:,19),wf(:,179)) * den(101)
  A(133) = cont_QA(wf(:,180),wf(:,181)) * den(103)
  A(134) = cont_QA(wf(:,20),wf(:,182)) * den(105)
  A(135) = cont_VV(wf(:,22),wf(:,183)) * den(108)
  A(136) = cont_VV(wf(:,24),wf(:,183)) * den(110)
  A(137) = cont_QA(wf(:,174),wf(:,185)) * den(112)
  A(138) = cont_QA(wf(:,170),wf(:,187)) * den(114)
  A(139) = cont_QA(wf(:,174),wf(:,189)) * den(116)
  A(140) = cont_QA(wf(:,175),wf(:,190)) * den(118)
  A(141) = cont_VV(wf(:,24),wf(:,191)) * den(119)
  A(142) = cont_QA(wf(:,171),wf(:,192)) * den(121)
  A(143) = cont_VV(wf(:,22),wf(:,191)) * den(122)
  A(144) = cont_QA(wf(:,170),wf(:,193)) * den(124)
  A(145) = cont_QA(wf(:,95),wf(:,195)) * den(126)
  A(146) = cont_QA(wf(:,8),wf(:,196)) * den(128)
  A(147) = cont_QA(wf(:,8),wf(:,199)) * den(131)
  A(148) = cont_QA(wf(:,31),wf(:,200)) * den(133)
  A(149) = cont_QA(wf(:,96),wf(:,170)) * den(134)
  A(150) = cont_QA(wf(:,95),wf(:,201)) * den(135)
  A(151) = cont_QA(wf(:,75),wf(:,202)) * den(136)
  A(152) = cont_QA(wf(:,13),wf(:,203)) * den(138)
  A(153) = cont_QA(wf(:,113),wf(:,204)) * den(139)
  A(154) = cont_QA(wf(:,75),wf(:,205)) * den(140)
  A(155) = cont_QA(wf(:,34),wf(:,206)) * den(142)
  A(156) = cont_QA(wf(:,36),wf(:,179)) * den(143)
  A(157) = cont_QA(wf(:,180),wf(:,207)) * den(145)
  A(158) = cont_QA(wf(:,37),wf(:,208)) * den(147)
  A(159) = cont_VV(wf(:,39),wf(:,209)) * den(150)
  A(160) = cont_VV(wf(:,41),wf(:,209)) * den(152)
  A(161) = cont_QA(wf(:,204),wf(:,211)) * den(154)
  A(162) = cont_QA(wf(:,170),wf(:,213)) * den(156)
  A(163) = cont_QA(wf(:,204),wf(:,215)) * den(158)
  A(164) = cont_QA(wf(:,190),wf(:,205)) * den(159)
  A(165) = cont_VV(wf(:,41),wf(:,216)) * den(160)
  A(166) = cont_QA(wf(:,201),wf(:,217)) * den(162)
  A(167) = cont_VV(wf(:,39),wf(:,216)) * den(163)
  A(168) = cont_QA(wf(:,170),wf(:,218)) * den(165)
  A(169) = cont_VV(wf(:,22),wf(:,221)) * den(168)
  A(170) = cont_VV(wf(:,24),wf(:,221)) * den(169)
  A(171) = cont_VV(wf(:,39),wf(:,222)) * den(171)
  A(172) = cont_VV(wf(:,41),wf(:,222)) * den(172)
  A(173) = cont_VV(wf(:,24),wf(:,223)) * den(173)
  A(174) = cont_VV(wf(:,41),wf(:,224)) * den(174)
  A(175) = cont_QA(wf(:,204),wf(:,226)) * den(176)
  A(176) = cont_VV(wf(:,22),wf(:,223)) * den(177)
  A(177) = cont_QA(wf(:,170),wf(:,227)) * den(179)
  A(178) = cont_VV(wf(:,39),wf(:,224)) * den(180)
  A(179) = cont_QA(wf(:,174),wf(:,228)) * den(182)
  A(180) = cont_QA(wf(:,170),wf(:,229)) * den(184)
  A(181) = cont_VV(wf(:,22),wf(:,230)) * den(186)
  A(182) = cont_VV(wf(:,24),wf(:,230)) * den(187)
  A(183) = cont_VV(wf(:,24),wf(:,231)) * den(188)
  A(184) = cont_VV(wf(:,22),wf(:,231)) * den(189)
  A(185) = cont_QA(wf(:,174),wf(:,232)) * den(191)
  A(186) = cont_QA(wf(:,170),wf(:,233)) * den(193)
  A(187) = cont_VV(wf(:,41),wf(:,234)) * den(194)
  A(188) = cont_VV(wf(:,39),wf(:,235)) * den(196)
  A(189) = cont_VV(wf(:,41),wf(:,235)) * den(197)
  A(190) = cont_VV(wf(:,39),wf(:,234)) * den(198)
  A(191) = cont_QA(wf(:,204),wf(:,236)) * den(200)
  A(192) = cont_QA(wf(:,170),wf(:,237)) * den(202)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(192)
  complex(REALKIND), intent(out) :: M1(4), M2(4)

  M1(1) = ((A(9)+A(10)+A(13)+A(17)+A(20))*f(1))/2._/**/REALKIND+((A(18)+A(19)+A(21)+A(22))*f(1))/6._/**/REALKIND+(CI*(-A(14) &
       -A(16))*f(2))/2._/**/REALKIND
  M1(2) = ((-A(9)-A(10)-A(11)-A(12)-A(13)-A(15))*f(1))/6._/**/REALKIND+((-A(3)-A(4)-A(7)-A(21)-A(22))*f(1))/2._/**/REALKIND+(CI*( &
       -A(6)-A(8))*f(2))/2._/**/REALKIND
  M1(3) = ((-A(1)-A(2)-A(5)-A(18)-A(19))*f(1))/2._/**/REALKIND+((-A(17)-A(20)-A(23)-A(24))*f(1))/6._/**/REALKIND+(CI*(A(6) &
       +A(8))*f(2))/2._/**/REALKIND
  M1(4) = ((A(1)+A(2)+A(3)+A(4)+A(5)+A(7))*f(1))/6._/**/REALKIND+((A(11)+A(12)+A(15)+A(23)+A(24))*f(1))/2._/**/REALKIND+(CI*(A(14) &
       +A(16))*f(2))/2._/**/REALKIND

  M2(1) = ((-A(145)-A(146)-A(147)-A(148)-A(149)-A(150)-A(158)-A(163)-A(166)-A(171)-A(172)-A(174)-A(175)-A(177) &
       -A(178))*f(3))/2._/**/REALKIND+((-A(169)-A(170)-A(173)-A(176)-A(179)-A(180)-A(181)-A(182)-A(183)-A(184)-A(185) &
       -A(186))*f(3))/6._/**/REALKIND+(CI*(A(159)+A(160)+A(161)+A(162)+A(165)+A(167))*f(4))/2._/**/REALKIND+((A(66)+A(69)+A(74) &
       +A(77)+A(89)+A(91)+A(95)+A(96)+A(97)+A(98)+A(102)+A(103))*f(5))/6._/**/REALKIND+((A(49)+A(53)+A(57)+A(60)+A(65)+A(73)+A(75) &
       +A(87)+A(88)+A(90)+A(101)+A(104)+A(105)+A(106)+A(109))*f(5))/2._/**/REALKIND+(CI*(-A(59)-A(83)-A(110) &
       -A(112))*f(6))/2._/**/REALKIND+((A(67)+A(70)+A(76)+A(78))*f(7))/6._/**/REALKIND+((A(50)+A(58)+A(68)+A(85) &
       +A(92))*f(7))/2._/**/REALKIND+(CI*(-A(62)-A(86))*f(8))/2._/**/REALKIND+(CI*(-A(54)-A(56))*f(9))/2._/**/REALKIND
  M2(2) = ((A(145)+A(146)+A(147)+A(148)+A(149)+A(150)+A(151)+A(152)+A(153)+A(154)+A(155)+A(156)+A(157)+A(158)+A(163)+A(164)+A(166) &
       +A(168))*f(3))/6._/**/REALKIND+((A(127)+A(128)+A(129)+A(130)+A(131)+A(132)+A(133)+A(140)+A(144)+A(181)+A(182)+A(183)+A(184) &
       +A(185)+A(186))*f(3))/2._/**/REALKIND+(CI*(A(135)+A(136)+A(137)+A(138)+A(141)+A(143))*f(4))/2._/**/REALKIND+((-A(49)-A(51) &
       -A(53)-A(55)-A(57)-A(60)-A(63)-A(64)-A(81)-A(84)-A(87)-A(88)-A(105)-A(106)-A(107)-A(108)-A(109) &
       -A(111))*f(5))/6._/**/REALKIND+((-A(27)-A(31)-A(39)-A(40)-A(41)-A(44)-A(69)-A(77)-A(95)-A(96)-A(97)-A(98)-A(115)-A(116) &
       -A(119))*f(5))/2._/**/REALKIND+(CI*(-A(35)-A(43)-A(118)-A(120))*f(6))/2._/**/REALKIND+((-A(28)-A(37)-A(42)-A(70) &
       -A(78))*f(7))/2._/**/REALKIND+((-A(50)-A(52)-A(58)-A(61)-A(82)-A(85))*f(7))/6._/**/REALKIND+(CI*(-A(38) &
       -A(46))*f(8))/2._/**/REALKIND+(CI*(-A(30)-A(32))*f(9))/2._/**/REALKIND
  M2(3) = ((A(121)+A(122)+A(123)+A(124)+A(125)+A(126)+A(134)+A(139)+A(142)+A(169)+A(170)+A(173)+A(176)+A(179) &
       +A(180))*f(3))/2._/**/REALKIND+((A(171)+A(172)+A(174)+A(175)+A(177)+A(178)+A(187)+A(188)+A(189)+A(190)+A(191) &
       +A(192))*f(3))/6._/**/REALKIND+(CI*(-A(135)-A(136)-A(137)-A(138)-A(141)-A(143))*f(4))/2._/**/REALKIND+((-A(65)-A(71)-A(73) &
       -A(75)-A(79)-A(80)-A(90)-A(93)-A(99)-A(100)-A(101)-A(104))*f(5))/6._/**/REALKIND+((-A(25)-A(29)-A(33)-A(36)-A(47)-A(48) &
       -A(66)-A(74)-A(89)-A(91)-A(102)-A(103)-A(113)-A(114)-A(117))*f(5))/2._/**/REALKIND+(CI*(A(35)+A(43)+A(118) &
       +A(120))*f(6))/2._/**/REALKIND+((-A(26)-A(34)-A(45)-A(67)-A(76))*f(7))/2._/**/REALKIND+((-A(68)-A(72)-A(92) &
       -A(94))*f(7))/6._/**/REALKIND+(CI*(A(38)+A(46))*f(8))/2._/**/REALKIND+(CI*(A(30)+A(32))*f(9))/2._/**/REALKIND
  M2(4) = ((-A(121)-A(122)-A(123)-A(124)-A(125)-A(126)-A(127)-A(128)-A(129)-A(130)-A(131)-A(132)-A(133)-A(134)-A(139)-A(140) &
       -A(142)-A(144))*f(3))/6._/**/REALKIND+((-A(151)-A(152)-A(153)-A(154)-A(155)-A(156)-A(157)-A(164)-A(168)-A(187)-A(188) &
       -A(189)-A(190)-A(191)-A(192))*f(3))/2._/**/REALKIND+(CI*(-A(159)-A(160)-A(161)-A(162)-A(165)-A(167))*f(4))/2._/**/REALKIND &
       +((A(51)+A(55)+A(63)+A(64)+A(71)+A(79)+A(80)+A(81)+A(84)+A(93)+A(99)+A(100)+A(107)+A(108)+A(111))*f(5))/2._/**/REALKIND &
       +((A(25)+A(27)+A(29)+A(31)+A(33)+A(36)+A(39)+A(40)+A(41)+A(44)+A(47)+A(48)+A(113)+A(114)+A(115)+A(116)+A(117) &
       +A(119))*f(5))/6._/**/REALKIND+(CI*(A(59)+A(83)+A(110)+A(112))*f(6))/2._/**/REALKIND+((A(26)+A(28)+A(34)+A(37)+A(42) &
       +A(45))*f(7))/6._/**/REALKIND+((A(52)+A(61)+A(72)+A(82)+A(94))*f(7))/2._/**/REALKIND+(CI*(A(62) &
       +A(86))*f(8))/2._/**/REALKIND+(CI*(A(54)+A(56))*f(9))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pplnjjj_neexuuxuxdg_1_/**/REALKIND
