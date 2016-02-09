
module ol_colourmatrix_pplnttj_nexeuttxdxg_1_/**/REALKIND
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
  K1( 37,:) = [   0,  16,  -2,   6]
  K1( 38,:) = [  16,   0,   6,  -2]
  K1( 39,:) = [  -2,   6,   0,  16]
  K1( 40,:) = [   6,  -2,  16,   0]
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
  K1( 53,:) = [   6,   2,   2,   0]
  K1( 54,:) = [   2,   0,  -6, -16]
  K1( 55,:) = [   2,  -6,   0, -16]
  K1( 56,:) = [   0, -16, -16, -48]
  K1( 57,:) = [   0,   2, -16,  -6]
  K1( 58,:) = [   2,   6,   0,   2]
  K1( 59,:) = [ -16,   0, -48, -16]
  K1( 60,:) = [  -6,   2, -16,   0]
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
  K1( 73,:) = [   0, -16,   2,  -6]
  K1( 74,:) = [ -16, -48,   0, -16]
  K1( 75,:) = [   2,   0,   6,   2]
  K1( 76,:) = [  -6, -16,   2,   0]
  K1( 77,:) = [ -48, -16, -16,   0]
  K1( 78,:) = [ -16,   0,  -6,   2]
  K1( 79,:) = [ -16,  -6,   0,   2]
  K1( 80,:) = [   0,   2,   2,   6]
  K1( 81,:) = [   0,  -2,  16,   6]
  K1( 82,:) = [  -2,   0,   6,  16]
  K1( 83,:) = [  16,   6,   0,  -2]
  K1( 84,:) = [   6,  16,  -2,   0]
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
  K1(101,:) = [   0, -18,  18,   0]
  K1(102,:) = [ -18, -54,   0, -18]
  K1(103,:) = [  18,   0,   0, -18]
  K1(104,:) = [   0, -18, -18, -54]
  K1(105,:) = [ -54, -18, -18,   0]
  K1(106,:) = [ -18, -54,   0, -18]
  K1(107,:) = [ -18,   0,   0,  18]
  K1(108,:) = [   0, -18,  18,   0]
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
end module ol_colourmatrix_pplnttj_nexeuttxdxg_1_/**/REALKIND



module ol_forced_parameters_pplnttj_nexeuttxdxg_1_/**/REALKIND
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
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMT /= 0) write(*,101) 'wMT = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pplnttj_nexeuttxdxg_1_/**/REALKIND

module ol_loop_pplnttj_nexeuttxdxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(19), c(26)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:157)
  ! denominators
  complex(REALKIND), save :: den(185)
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
    f( 7) = (CI*countertermnorm*ctGtt*eQED**2*gQCD**5)/(2._/**/REALKIND*sw**2)
    f( 8) = (countertermnorm*ctGtt*eQED**2*gQCD**5)/(sw**2*2._/**/REALKIND)
    f( 9) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**5)/(2._/**/REALKIND*sw**2)
    f(10) = (countertermnorm*ctVqq*eQED**2*gQCD**5)/(sw**2*2._/**/REALKIND)
    f(11) = (countertermnorm*ctVVV*eQED**2*gQCD**5)/(sw**2*2._/**/REALKIND)
    f(12) = (CI*eQED**2*gQCD**5*integralnorm*SwB)/(4._/**/REALKIND*sw**2)
    f(13) = (CI*eQED**2*gQCD**5*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(14) = (eQED**2*gQCD**5*integralnorm*SwB)/(sw**2*4._/**/REALKIND)
    f(15) = (eQED**2*gQCD**5*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(16) = (CI*eQED**2*gQCD**5*integralnorm*SwF)/(2._/**/REALKIND*sw**2)
    f(17) = (CI*eQED**2*gQCD**5*integralnorm*SwF)/sw**2
    f(18) = (eQED**2*gQCD**5*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(19) = (eQED**2*gQCD**5*integralnorm*SwF)/sw**2

  c = [ 27*CI*f(12), 54*CI*f(12), 3*CI*f(13), 9*CI*f(13), 24*CI*f(13), 27*CI*f(13), 54*CI*f(13), 18*f(14), 54*f(14), f(15) &
    , 3*f(15), 6*f(15), 8*f(15), 9*f(15), 10*f(15), 18*f(15), 21*f(15), 24*f(15), 27*f(15), 54*f(15), 9*CI*f(16), 9*CI*f(17) &
    , 3*f(18), 9*f(18), 3*f(19), 9*f(19) ]
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
  complex(REALKIND) :: A(96)
  ! external WFs
  call wf_A(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_Q(P(:,4), rMT, H(4), wf(:,-3))
  call wf_A(P(:,5), rMT, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_W(wf(:,-1),wf(:,0),wf(:,1))
  call vert_VQ_A(wf(:,-6),wf(:,-2),wf(:,2))
  call vert_QA_V(wf(:,-3),wf(:,-4),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MW,1_intkind1,wf(:,4))
  call prop_Q_A(wf(:,2),Q(:,68),ZERO,0_intkind1,wf(:,5))
  call vert_AW_Q(wf(:,-5),wf(:,4),wf(:,6))
  call vert_VQ_A(wf(:,3),wf(:,5),wf(:,7))
  call prop_A_Q(wf(:,6),Q(:,35),ZERO,0_intkind1,wf(:,8))
  call vert_AV_Q(wf(:,-5),wf(:,3),wf(:,9))
  call vert_WQ_A(wf(:,4),wf(:,5),wf(:,10))
  call prop_A_Q(wf(:,9),Q(:,56),ZERO,0_intkind1,wf(:,11))
  call vert_AV_Q(wf(:,-5),wf(:,-6),wf(:,12))
  call prop_A_Q(wf(:,12),Q(:,96),ZERO,0_intkind1,wf(:,13))
  call vert_WQ_A(wf(:,4),wf(:,-2),wf(:,14))
  call vert_AV_Q(wf(:,13),wf(:,3),wf(:,15))
  call prop_Q_A(wf(:,14),Q(:,7),ZERO,0_intkind1,wf(:,16))
  call vert_VQ_A(wf(:,3),wf(:,-2),wf(:,17))
  call vert_AW_Q(wf(:,13),wf(:,4),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,28),ZERO,0_intkind1,wf(:,19))
  call vert_UV_W(wf(:,3),Q(:,24),wf(:,-6),Q(:,64),wf(:,20))
  call vert_QA_V(wf(:,16),wf(:,-5),wf(:,21))
  call vert_VQ_A(wf(:,-6),wf(:,16),wf(:,22))
  call vert_VQ_A(wf(:,-6),wf(:,19),wf(:,23))
  call vert_QA_V(wf(:,-2),wf(:,8),wf(:,24))
  call vert_VQ_A(wf(:,-6),wf(:,-3),wf(:,25))
  call prop_Q_A(wf(:,25),Q(:,72),MT,1_intkind1,wf(:,26))
  call vert_QA_V(wf(:,26),wf(:,-4),wf(:,27))
  call vert_VQ_A(wf(:,27),wf(:,-2),wf(:,28))
  call vert_AV_Q(wf(:,-4),wf(:,-6),wf(:,29))
  call prop_A_Q(wf(:,29),Q(:,80),MT,1_intkind1,wf(:,30))
  call vert_QA_V(wf(:,-3),wf(:,30),wf(:,31))
  call vert_VQ_A(wf(:,31),wf(:,-2),wf(:,32))
  call counter_VQ_A(wf(:,3),wf(:,5),wf(:,33))
  call counter_WQ_A(wf(:,4),wf(:,5),wf(:,34))
  call counter_AV_Q(wf(:,13),wf(:,3),wf(:,35))
  call counter_AW_Q(wf(:,13),wf(:,4),wf(:,36))
  call counter_UV_W(wf(:,3),Q(:,24),wf(:,-6),Q(:,64),wf(:,37))
  call counter_VQ_A(wf(:,-6),wf(:,16),wf(:,38))
  call counter_VQ_A(wf(:,-6),wf(:,19),wf(:,39))
  call counter_AV_Q(wf(:,-5),wf(:,3),wf(:,40))
  call prop_Q_A(wf(:,10),Q(:,71),ZERO,0_intkind1,wf(:,41))
  call counter_AW_Q(wf(:,-5),wf(:,4),wf(:,42))
  call prop_Q_A(wf(:,7),Q(:,92),ZERO,0_intkind1,wf(:,43))
  call prop_A_Q(wf(:,40),Q(:,56),ZERO,0_intkind1,wf(:,44))
  call counter_QA_V(wf(:,16),wf(:,-5),wf(:,45))
  call prop_A_Q(wf(:,42),Q(:,35),ZERO,0_intkind1,wf(:,46))
  call vert_QA_V(wf(:,-2),wf(:,46),wf(:,47))
  call counter_AV_Q(wf(:,-5),wf(:,-6),wf(:,48))
  call prop_A_Q(wf(:,48),Q(:,96),ZERO,0_intkind1,wf(:,49))
  call vert_AV_Q(wf(:,49),wf(:,3),wf(:,50))
  call vert_AW_Q(wf(:,49),wf(:,4),wf(:,51))
  call counter_QA_V(wf(:,26),wf(:,-4),wf(:,52))
  call vert_VQ_A(wf(:,52),wf(:,-2),wf(:,53))
  call counter_AV_Q(wf(:,-4),wf(:,-6),wf(:,54))
  call prop_A_Q(wf(:,54),Q(:,80),MT,1_intkind1,wf(:,55))
  call vert_QA_V(wf(:,-3),wf(:,55),wf(:,56))
  call vert_VQ_A(wf(:,56),wf(:,-2),wf(:,57))
  call counter_QA_V(wf(:,-3),wf(:,30),wf(:,58))
  call vert_VQ_A(wf(:,58),wf(:,-2),wf(:,59))
  call counter_VQ_A(wf(:,-6),wf(:,-3),wf(:,60))
  call prop_Q_A(wf(:,60),Q(:,72),MT,1_intkind1,wf(:,61))
  call vert_QA_V(wf(:,61),wf(:,-4),wf(:,62))
  call vert_VQ_A(wf(:,62),wf(:,-2),wf(:,63))
  call counter_QA_V(wf(:,-3),wf(:,-4),wf(:,64))
  call vert_VQ_A(wf(:,64),wf(:,5),wf(:,65))
  call vert_AV_Q(wf(:,-5),wf(:,64),wf(:,66))
  call prop_A_Q(wf(:,66),Q(:,56),ZERO,0_intkind1,wf(:,67))
  call vert_AV_Q(wf(:,13),wf(:,64),wf(:,68))
  call vert_VQ_A(wf(:,64),wf(:,-2),wf(:,69))
  call prop_Q_A(wf(:,69),Q(:,28),ZERO,0_intkind1,wf(:,70))
  call vert_UV_W(wf(:,64),Q(:,24),wf(:,-6),Q(:,64),wf(:,71))
  call vert_VQ_A(wf(:,-6),wf(:,70),wf(:,72))
  call counter_VQ_A(wf(:,3),wf(:,-2),wf(:,73))
  call prop_A_Q(wf(:,18),Q(:,99),ZERO,0_intkind1,wf(:,74))
  call counter_WQ_A(wf(:,4),wf(:,-2),wf(:,75))
  call prop_A_Q(wf(:,15),Q(:,120),ZERO,0_intkind1,wf(:,76))
  call prop_Q_A(wf(:,73),Q(:,28),ZERO,0_intkind1,wf(:,77))
  call vert_VQ_A(wf(:,-6),wf(:,77),wf(:,78))
  call counter_QA_V(wf(:,-2),wf(:,8),wf(:,79))
  call prop_Q_A(wf(:,75),Q(:,7),ZERO,0_intkind1,wf(:,80))
  call vert_QA_V(wf(:,80),wf(:,-5),wf(:,81))
  call vert_VQ_A(wf(:,-6),wf(:,80),wf(:,82))
  call counter_VQ_A(wf(:,-6),wf(:,-2),wf(:,83))
  call prop_Q_A(wf(:,83),Q(:,68),ZERO,0_intkind1,wf(:,84))
  call vert_VQ_A(wf(:,3),wf(:,84),wf(:,85))
  call vert_WQ_A(wf(:,4),wf(:,84),wf(:,86))
  call counter_VQ_A(wf(:,27),wf(:,-2),wf(:,87))
  call counter_VQ_A(wf(:,31),wf(:,-2),wf(:,88))
  call counter_Q_A(ctqq,wf(:,5),Q(:,68),wf(:,89))
  call prop_Q_A(wf(:,89),Q(:,68),ZERO,0_intkind1,wf(:,90))
  call vert_VQ_A(wf(:,3),wf(:,90),wf(:,91))
  call vert_WQ_A(wf(:,4),wf(:,90),wf(:,92))
  call counter_V_V(ctGG,wf(:,3),Q(:,24),wf(:,93))
  call vert_AV_Q(wf(:,-5),wf(:,93),wf(:,94))
  call vert_VQ_A(wf(:,93),wf(:,5),wf(:,95))
  call counter_A_Q(ctqq,wf(:,8),Q(:,35),wf(:,96))
  call counter_A_Q(ctqq,wf(:,11),Q(:,56),wf(:,97))
  call vert_VQ_A(wf(:,93),wf(:,-2),wf(:,98))
  call counter_Q_A(ctqq,wf(:,16),Q(:,7),wf(:,99))
  call counter_Q_A(ctqq,wf(:,19),Q(:,28),wf(:,100))
  call vert_VQ_A(wf(:,93),wf(:,16),wf(:,101))
  call counter_A_Q(ctqq,wf(:,13),Q(:,96),wf(:,102))
  call prop_A_Q(wf(:,102),Q(:,96),ZERO,0_intkind1,wf(:,103))
  call vert_QA_V(wf(:,16),wf(:,103),wf(:,104))
  call vert_AW_Q(wf(:,103),wf(:,4),wf(:,105))
  call vert_AV_Q(wf(:,8),wf(:,-6),wf(:,106))
  call prop_Q_A(wf(:,98),Q(:,28),ZERO,0_intkind1,wf(:,107))
  call vert_AV_Q(wf(:,-5),wf(:,20),wf(:,108))
  call prop_A_Q(wf(:,108),Q(:,120),ZERO,0_intkind1,wf(:,109))
  call vert_VQ_A(wf(:,20),wf(:,-2),wf(:,110))
  call prop_Q_A(wf(:,110),Q(:,92),ZERO,0_intkind1,wf(:,111))
  call vert_AV_Q(wf(:,11),wf(:,-6),wf(:,112))
  call prop_A_Q(wf(:,112),Q(:,120),ZERO,0_intkind1,wf(:,113))
  call prop_A_Q(wf(:,106),Q(:,99),ZERO,0_intkind1,wf(:,114))
  call counter_V_V(ctGG,wf(:,20),Q(:,88),wf(:,115))
  call prop_A_Q(wf(:,94),Q(:,56),ZERO,0_intkind1,wf(:,116))
  call vert_UV_W(wf(:,93),Q(:,24),wf(:,-6),Q(:,64),wf(:,117))
  call prop_Q_A(wf(:,22),Q(:,71),ZERO,0_intkind1,wf(:,118))
  call prop_Q_A(wf(:,23),Q(:,92),ZERO,0_intkind1,wf(:,119))
  call counter_V_V(ctGG,wf(:,27),Q(:,88),wf(:,120))
  call vert_AV_Q(wf(:,-5),wf(:,27),wf(:,121))
  call prop_A_Q(wf(:,121),Q(:,120),ZERO,0_intkind1,wf(:,122))
  call prop_Q_A(wf(:,28),Q(:,92),ZERO,0_intkind1,wf(:,123))
  call counter_Q_A(cttt,wf(:,26),Q(:,72),wf(:,124))
  call prop_Q_A(wf(:,124),Q(:,72),MT,1_intkind1,wf(:,125))
  call vert_QA_V(wf(:,125),wf(:,-4),wf(:,126))
  call counter_V_V(ctGG,wf(:,31),Q(:,88),wf(:,127))
  call vert_AV_Q(wf(:,-5),wf(:,31),wf(:,128))
  call prop_A_Q(wf(:,128),Q(:,120),ZERO,0_intkind1,wf(:,129))
  call prop_Q_A(wf(:,32),Q(:,92),ZERO,0_intkind1,wf(:,130))
  call counter_A_Q(cttt,wf(:,30),Q(:,80),wf(:,131))
  call prop_A_Q(wf(:,131),Q(:,80),MT,1_intkind1,wf(:,132))
  call vert_QA_V(wf(:,-3),wf(:,132),wf(:,133))
  call vert_VQ_A(wf(:,3),wf(:,16),wf(:,134))
  call prop_Q_A(wf(:,134),Q(:,31),ZERO,0_intkind1,wf(:,135))
  call vert_AV_Q(wf(:,8),wf(:,3),wf(:,136))
  call prop_A_Q(wf(:,136),Q(:,59),ZERO,0_intkind1,wf(:,137))
  call vert_WQ_A(wf(:,4),wf(:,19),wf(:,138))
  call prop_Q_A(wf(:,138),Q(:,31),ZERO,0_intkind1,wf(:,139))
  call vert_AW_Q(wf(:,11),wf(:,4),wf(:,140))
  call prop_A_Q(wf(:,140),Q(:,59),ZERO,0_intkind1,wf(:,141))
  call vert_QA_V(wf(:,5),wf(:,8),wf(:,142))
  call vert_QA_V(wf(:,41),wf(:,-5),wf(:,143))
  call vert_QA_V(wf(:,16),wf(:,13),wf(:,144))
  call vert_QA_V(wf(:,-2),wf(:,74),wf(:,145))
  call vert_VQ_A(wf(:,21),wf(:,-3),wf(:,146))
  call prop_Q_A(wf(:,146),Q(:,47),MT,1_intkind1,wf(:,147))
  call vert_AV_Q(wf(:,-4),wf(:,21),wf(:,148))
  call prop_A_Q(wf(:,148),Q(:,55),MT,1_intkind1,wf(:,149))
  call vert_UV_W(wf(:,21),Q(:,39),wf(:,-6),Q(:,64),wf(:,150))
  call vert_QA_V(wf(:,118),wf(:,-5),wf(:,151))
  call vert_VQ_A(wf(:,24),wf(:,-3),wf(:,152))
  call prop_Q_A(wf(:,152),Q(:,47),MT,1_intkind1,wf(:,153))
  call vert_AV_Q(wf(:,-4),wf(:,24),wf(:,154))
  call prop_A_Q(wf(:,154),Q(:,55),MT,1_intkind1,wf(:,155))
  call vert_UV_W(wf(:,24),Q(:,39),wf(:,-6),Q(:,64),wf(:,156))
  call vert_QA_V(wf(:,-2),wf(:,114),wf(:,157))

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
  den(3) = 1 / (Q(5,24))
  den(5) = 1 / (Q(5,35))
  den(9) = 1 / (Q(5,56))
  den(12) = 1 / (Q(5,96))
  den(14) = 1 / (Q(5,7))
  den(18) = 1 / (Q(5,28))
  den(21) = 1 / (Q(5,88))
  den(27) = 1 / (Q(5,72) - MT2)
  den(31) = 1 / (Q(5,80) - MT2)
  den(35) = 1 / (Q(5,71))
  den(38) = 1 / (Q(5,92))
  den(41) = 1 / (Q(5,99))
  den(44) = 1 / (Q(5,120))
  den(78) = 1 / (Q(5,39))
  den(112) = 1 / (Q(5,31))
  den(115) = 1 / (Q(5,59))
  den(122) = 1 / (Q(5,103))
  den(128) = 1 / (Q(5,47) - MT2)
  den(130) = 1 / (Q(5,55) - MT2)

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
  den(28) = den(21)*den(27)
  den(29) = den(15)*den(28)
  den(30) = den(6)*den(28)
  den(32) = den(21)*den(31)
  den(33) = den(15)*den(32)
  den(34) = den(6)*den(32)
  den(36) = den(8)*den(35)
  den(37) = den(3)*den(36)
  den(39) = den(4)*den(38)
  den(40) = den(1)*den(39)
  den(42) = den(17)*den(41)
  den(43) = den(3)*den(42)
  den(45) = den(13)*den(44)
  den(46) = den(1)*den(45)
  den(47) = den(2)**2
  den(48) = den(3)*den(47)
  den(49) = den(6)*den(48)
  den(50) = den(1)*den(47)
  den(51) = den(10)*den(50)
  den(52) = den(3)**2
  den(53) = den(36)*den(52)
  den(54) = den(2)*den(52)
  den(55) = den(6)*den(54)
  den(56) = den(6)*den(39)
  den(57) = den(10)*den(36)
  den(58) = den(42)*den(52)
  den(59) = den(15)*den(45)
  den(60) = den(19)*den(42)
  den(61) = den(15)*den(52)
  den(62) = den(12)*den(61)
  den(63) = den(12)**2
  den(64) = den(15)*den(63)
  den(65) = den(3)*den(64)
  den(66) = den(1)*den(63)
  den(67) = den(19)*den(66)
  den(68) = den(18)*den(52)
  den(69) = den(6)*den(68)
  den(70) = den(22)*den(44)
  den(71) = den(15)*den(70)
  den(72) = den(22)*den(38)
  den(73) = den(6)*den(72)
  den(74) = den(10)*den(44)
  den(75) = den(15)*den(74)
  den(76) = den(6)*den(41)
  den(77) = den(19)*den(76)
  den(79) = den(6)*den(78)
  den(80) = den(22)*den(79)
  den(81) = den(9)*den(52)
  den(82) = den(15)*den(81)
  den(83) = den(15)*den(78)
  den(84) = den(52)*den(83)
  den(85) = den(52)*den(79)
  den(86) = den(22)*den(83)
  den(87) = den(15)*den(35)
  den(88) = den(10)*den(87)
  den(89) = den(19)*den(38)
  den(90) = den(6)*den(89)
  den(91) = den(28)*den(79)
  den(92) = den(28)*den(44)
  den(93) = den(15)*den(92)
  den(94) = den(28)*den(38)
  den(95) = den(6)*den(94)
  den(96) = den(27)**2
  den(97) = den(21)*den(96)
  den(98) = den(15)*den(97)
  den(99) = den(79)*den(96)
  den(100) = den(28)*den(83)
  den(101) = den(32)*den(79)
  den(102) = den(32)*den(44)
  den(103) = den(15)*den(102)
  den(104) = den(32)*den(38)
  den(105) = den(6)*den(104)
  den(106) = den(31)**2
  den(107) = den(21)*den(106)
  den(108) = den(15)*den(107)
  den(109) = den(79)*den(106)
  den(110) = den(32)*den(83)
  den(111) = den(3)*den(15)
  den(113) = den(111)*den(112)
  den(114) = den(3)*den(6)
  den(116) = den(114)*den(115)
  den(117) = den(1)*den(19)
  den(118) = den(112)*den(117)
  den(119) = den(1)*den(10)
  den(120) = den(115)*den(119)
  den(121) = den(2)*den(6)
  den(123) = den(121)*den(122)
  den(124) = den(36)*den(122)
  den(125) = den(12)*den(15)
  den(126) = den(122)*den(125)
  den(127) = den(42)*den(122)
  den(129) = den(83)*den(128)
  den(131) = den(83)*den(130)
  den(132) = den(83)*den(122)
  den(133) = den(87)*den(122)
  den(134) = den(79)*den(128)
  den(135) = den(79)*den(130)
  den(136) = den(79)*den(122)
  den(137) = den(76)*den(122)
  den(138) = den(1)*den(3)
  den(139) = den(2)*den(3)*den(6)
  den(140) = den(1)*den(2)*den(10)
  den(141) = den(1)*den(2)*den(3)
  den(142) = den(3)*den(12)*den(15)
  den(143) = den(1)*den(12)*den(19)
  den(144) = den(1)*den(3)*den(12)
  den(145) = den(3)*den(83)
  den(146) = den(3)*den(87)
  den(147) = den(3)*den(79)
  den(148) = den(3)*den(76)
  den(149) = den(1)*den(89)
  den(150) = den(1)*den(72)
  den(151) = den(1)*den(74)
  den(152) = den(1)*den(70)
  den(153) = den(1)*den(22)
  den(154) = den(27)*den(83)
  den(155) = den(15)*den(27)
  den(156) = den(27)*den(79)
  den(157) = den(6)*den(27)
  den(158) = den(1)*den(94)
  den(159) = den(1)*den(92)
  den(160) = den(1)*den(28)
  den(161) = den(1)*den(27)
  den(162) = den(31)*den(83)
  den(163) = den(15)*den(31)
  den(164) = den(31)*den(79)
  den(165) = den(6)*den(31)
  den(166) = den(1)*den(104)
  den(167) = den(1)*den(102)
  den(168) = den(1)*den(32)
  den(169) = den(1)*den(31)
  den(170) = den(3)*den(123)
  den(171) = den(2)*den(116)
  den(172) = den(3)*den(124)
  den(173) = den(2)*den(120)
  den(174) = den(12)*den(113)
  den(175) = den(3)*den(126)
  den(176) = den(12)*den(118)
  den(177) = den(3)*den(127)
  den(178) = den(3)*den(132)
  den(179) = den(3)*den(133)
  den(180) = den(3)*den(136)
  den(181) = den(3)*den(137)
  den(182) = den(27)*den(131)
  den(183) = den(27)*den(135)
  den(184) = den(31)*den(129)
  den(185) = den(31)*den(134)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(96)

  A(1) = cont_QA(wf(:,7),wf(:,8)) * den(7)
  A(2) = cont_QA(wf(:,10),wf(:,11)) * den(11)
  A(3) = cont_QA(wf(:,15),wf(:,16)) * den(16)
  A(4) = cont_QA(wf(:,18),wf(:,19)) * den(20)
  A(5) = cont_VV(wf(:,20),wf(:,21)) * den(23)
  A(6) = cont_QA(wf(:,11),wf(:,22)) * den(24)
  A(7) = cont_QA(wf(:,8),wf(:,23)) * den(25)
  A(8) = cont_VV(wf(:,20),wf(:,24)) * den(26)
  A(9) = cont_VV(wf(:,21),wf(:,27)) * den(29)
  A(10) = cont_QA(wf(:,8),wf(:,28)) * den(30)
  A(11) = cont_VV(wf(:,21),wf(:,31)) * den(33)
  A(12) = cont_QA(wf(:,8),wf(:,32)) * den(34)

  A(13) = cont_QA(wf(:,8),wf(:,33)) * den(7)
  A(14) = cont_QA(wf(:,11),wf(:,34)) * den(11)
  A(15) = cont_QA(wf(:,16),wf(:,35)) * den(16)
  A(16) = cont_QA(wf(:,19),wf(:,36)) * den(20)
  A(17) = cont_VV(wf(:,21),wf(:,37)) * den(23)
  A(18) = cont_QA(wf(:,11),wf(:,38)) * den(24)
  A(19) = cont_QA(wf(:,8),wf(:,39)) * den(25)
  A(20) = cont_VV(wf(:,24),wf(:,37)) * den(26)
  A(21) = cont_QA(wf(:,40),wf(:,41)) * den(37)
  A(22) = cont_QA(wf(:,42),wf(:,43)) * den(40)
  A(23) = cont_QA(wf(:,22),wf(:,44)) * den(24)
  A(24) = cont_VV(wf(:,20),wf(:,45)) * den(23)
  A(25) = cont_QA(wf(:,23),wf(:,46)) * den(25)
  A(26) = cont_VV(wf(:,20),wf(:,47)) * den(26)
  A(27) = cont_QA(wf(:,16),wf(:,50)) * den(16)
  A(28) = cont_QA(wf(:,19),wf(:,51)) * den(20)
  A(29) = cont_VV(wf(:,27),wf(:,45)) * den(29)
  A(30) = cont_QA(wf(:,28),wf(:,46)) * den(30)
  A(31) = cont_VV(wf(:,31),wf(:,45)) * den(33)
  A(32) = cont_QA(wf(:,32),wf(:,46)) * den(34)
  A(33) = cont_VV(wf(:,21),wf(:,52)) * den(29)
  A(34) = cont_QA(wf(:,8),wf(:,53)) * den(30)
  A(35) = cont_VV(wf(:,21),wf(:,56)) * den(33)
  A(36) = cont_QA(wf(:,8),wf(:,57)) * den(34)
  A(37) = cont_VV(wf(:,21),wf(:,58)) * den(33)
  A(38) = cont_QA(wf(:,8),wf(:,59)) * den(34)
  A(39) = cont_VV(wf(:,21),wf(:,62)) * den(29)
  A(40) = cont_QA(wf(:,8),wf(:,63)) * den(30)
  A(41) = cont_QA(wf(:,8),wf(:,65)) * den(7)
  A(42) = cont_QA(wf(:,10),wf(:,67)) * den(11)
  A(43) = cont_QA(wf(:,16),wf(:,68)) * den(16)
  A(44) = cont_QA(wf(:,18),wf(:,70)) * den(20)
  A(45) = cont_VV(wf(:,21),wf(:,71)) * den(23)
  A(46) = cont_QA(wf(:,22),wf(:,67)) * den(24)
  A(47) = cont_VV(wf(:,24),wf(:,71)) * den(26)
  A(48) = cont_QA(wf(:,8),wf(:,72)) * den(25)
  A(49) = cont_QA(wf(:,73),wf(:,74)) * den(43)
  A(50) = cont_QA(wf(:,75),wf(:,76)) * den(46)
  A(51) = cont_QA(wf(:,8),wf(:,78)) * den(25)
  A(52) = cont_VV(wf(:,20),wf(:,79)) * den(26)
  A(53) = cont_VV(wf(:,20),wf(:,81)) * den(23)
  A(54) = cont_QA(wf(:,11),wf(:,82)) * den(24)
  A(55) = cont_QA(wf(:,8),wf(:,85)) * den(7)
  A(56) = cont_QA(wf(:,11),wf(:,86)) * den(11)
  A(57) = cont_QA(wf(:,8),wf(:,87)) * den(30)
  A(58) = cont_VV(wf(:,27),wf(:,81)) * den(29)
  A(59) = cont_QA(wf(:,8),wf(:,88)) * den(34)
  A(60) = cont_VV(wf(:,31),wf(:,81)) * den(33)
  A(61) = cont_QA(wf(:,8),wf(:,91)) * den(49)
  A(62) = cont_QA(wf(:,11),wf(:,92)) * den(51)
  A(63) = cont_QA(wf(:,41),wf(:,94)) * den(53)
  A(64) = cont_QA(wf(:,8),wf(:,95)) * den(55)
  A(65) = cont_QA(wf(:,43),wf(:,96)) * den(56)
  A(66) = cont_QA(wf(:,41),wf(:,97)) * den(57)
  A(67) = cont_QA(wf(:,74),wf(:,98)) * den(58)
  A(68) = cont_QA(wf(:,76),wf(:,99)) * den(59)
  A(69) = cont_QA(wf(:,74),wf(:,100)) * den(60)
  A(70) = cont_QA(wf(:,13),wf(:,101)) * den(62)
  A(71) = cont_VV(wf(:,3),wf(:,104)) * den(65)
  A(72) = cont_QA(wf(:,19),wf(:,105)) * den(67)
  A(73) = cont_QA(wf(:,106),wf(:,107)) * den(69)
  A(74) = cont_QA(wf(:,99),wf(:,109)) * den(71)
  A(75) = cont_QA(wf(:,96),wf(:,111)) * den(73)
  A(76) = cont_QA(wf(:,99),wf(:,113)) * den(75)
  A(77) = cont_QA(wf(:,100),wf(:,114)) * den(77)
  A(78) = cont_VV(wf(:,24),wf(:,115)) * den(80)
  A(79) = cont_QA(wf(:,22),wf(:,116)) * den(82)
  A(80) = cont_VV(wf(:,21),wf(:,117)) * den(84)
  A(81) = cont_VV(wf(:,24),wf(:,117)) * den(85)
  A(82) = cont_VV(wf(:,21),wf(:,115)) * den(86)
  A(83) = cont_QA(wf(:,97),wf(:,118)) * den(88)
  A(84) = cont_QA(wf(:,96),wf(:,119)) * den(90)
  A(85) = cont_VV(wf(:,24),wf(:,120)) * den(91)
  A(86) = cont_QA(wf(:,99),wf(:,122)) * den(93)
  A(87) = cont_QA(wf(:,96),wf(:,123)) * den(95)
  A(88) = cont_VV(wf(:,21),wf(:,126)) * den(98)
  A(89) = cont_VV(wf(:,24),wf(:,126)) * den(99)
  A(90) = cont_VV(wf(:,21),wf(:,120)) * den(100)
  A(91) = cont_VV(wf(:,24),wf(:,127)) * den(101)
  A(92) = cont_QA(wf(:,99),wf(:,129)) * den(103)
  A(93) = cont_QA(wf(:,96),wf(:,130)) * den(105)
  A(94) = cont_VV(wf(:,21),wf(:,133)) * den(108)
  A(95) = cont_VV(wf(:,24),wf(:,133)) * den(109)
  A(96) = cont_VV(wf(:,21),wf(:,127)) * den(110)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(96)
  complex(REALKIND), intent(out) :: M1(4), M2(4)

  M1(1) = ((-A(1)-A(2)-A(6)-A(11)-A(12))*f(1))/2._/**/REALKIND+(CI*(-A(5)-A(8))*f(2))/2._/**/REALKIND
  M1(2) = ((A(9)+A(10)+A(11)+A(12))*f(1))/6._/**/REALKIND
  M1(3) = ((A(1)+A(2)+A(3)+A(4)+A(6)+A(7))*f(1))/6._/**/REALKIND
  M1(4) = ((-A(3)-A(4)-A(7)-A(9)-A(10))*f(1))/2._/**/REALKIND+(CI*(A(5)+A(8))*f(2))/2._/**/REALKIND

  M2(1) = ((A(61)+A(62)+A(63)+A(64)+A(65)+A(66)+A(76)+A(79)+A(83)+A(91)+A(92)+A(93)+A(94)+A(95)+A(96))*f(3))/2._/**/REALKIND &
       +(CI*(A(74)+A(75)+A(78)+A(80)+A(81)+A(82))*f(4))/2._/**/REALKIND+((-A(13)-A(18)-A(21)-A(23)-A(31)-A(55)-A(56) &
       -A(59))*f(5))/2._/**/REALKIND+(CI*(-A(24)-A(52))*f(6))/2._/**/REALKIND+((-A(35)-A(36)-A(37)-A(38)-A(41)-A(42) &
       -A(46))*f(7))/2._/**/REALKIND+(CI*(-A(45)-A(47))*f(8))/2._/**/REALKIND+((-A(14)-A(22)-A(32)-A(54) &
       -A(60))*f(9))/2._/**/REALKIND+(CI*(-A(26)-A(53))*f(10))/2._/**/REALKIND+(CI*(-A(17)-A(20))*f(11))/2._/**/REALKIND
  M2(2) = ((-A(85)-A(86)-A(87)-A(88)-A(89)-A(90)-A(91)-A(92)-A(93)-A(94)-A(95)-A(96))*f(3))/6._/**/REALKIND+((A(29)+A(31)+A(57) &
       +A(59))*f(5))/6._/**/REALKIND+((A(33)+A(34)+A(35)+A(36)+A(37)+A(38)+A(39)+A(40))*f(7))/6._/**/REALKIND+((A(30)+A(32)+A(58) &
       +A(60))*f(9))/6._/**/REALKIND
  M2(3) = ((-A(61)-A(62)-A(63)-A(64)-A(65)-A(66)-A(67)-A(68)-A(69)-A(70)-A(71)-A(72)-A(73)-A(76)-A(77)-A(79)-A(83) &
       -A(84))*f(3))/6._/**/REALKIND+((A(13)+A(15)+A(18)+A(19)+A(21)+A(23)+A(27)+A(28)+A(49)+A(51)+A(55) &
       +A(56))*f(5))/6._/**/REALKIND+((A(41)+A(42)+A(43)+A(44)+A(46)+A(48))*f(7))/6._/**/REALKIND+((A(14)+A(16)+A(22)+A(25)+A(50) &
       +A(54))*f(9))/6._/**/REALKIND
  M2(4) = ((A(67)+A(68)+A(69)+A(70)+A(71)+A(72)+A(73)+A(77)+A(84)+A(85)+A(86)+A(87)+A(88)+A(89)+A(90))*f(3))/2._/**/REALKIND+(CI*( &
       -A(74)-A(75)-A(78)-A(80)-A(81)-A(82))*f(4))/2._/**/REALKIND+((-A(15)-A(19)-A(27)-A(28)-A(29)-A(49)-A(51) &
       -A(57))*f(5))/2._/**/REALKIND+(CI*(A(24)+A(52))*f(6))/2._/**/REALKIND+((-A(33)-A(34)-A(39)-A(40)-A(43)-A(44) &
       -A(48))*f(7))/2._/**/REALKIND+(CI*(A(45)+A(47))*f(8))/2._/**/REALKIND+((-A(16)-A(25)-A(30)-A(50) &
       -A(58))*f(9))/2._/**/REALKIND+(CI*(A(26)+A(53))*f(10))/2._/**/REALKIND+(CI*(A(17)+A(20))*f(11))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pplnttj_nexeuttxdxg_1_/**/REALKIND
