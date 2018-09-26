
module ol_colourmatrix_ppnnjjj_nenexddxssxg_1_/**/REALKIND
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
  K1( 53,:) = [   0,  16,  -2,   6]
  K1( 54,:) = [  16,   0,   6,  -2]
  K1( 55,:) = [  -2,   6,   0,  16]
  K1( 56,:) = [   6,  -2,  16,   0]
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
  K1( 77,:) = [   0,  -2,  16,   6]
  K1( 78,:) = [  -2,   0,   6,  16]
  K1( 79,:) = [  16,   6,   0,  -2]
  K1( 80,:) = [   6,  16,  -2,   0]
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
end module ol_colourmatrix_ppnnjjj_nenexddxssxg_1_/**/REALKIND



module ol_forced_parameters_ppnnjjj_nenexddxssxg_1_/**/REALKIND
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
end module ol_forced_parameters_ppnnjjj_nenexddxssxg_1_/**/REALKIND

module ol_loop_ppnnjjj_nenexddxssxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(19), c(26)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:298)
  ! denominators
  complex(REALKIND), save :: den(383)
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
    f( 1) = CI*eQED**2*gQCD**3
    f( 2) = eQED**2*gQCD**3
    f( 3) = CI*countertermnorm*eQED**2*gQCD**5
    f( 4) = countertermnorm*eQED**2*gQCD**5
    f( 5) = CI*countertermnorm*ctGqq*eQED**2*gQCD**5
    f( 6) = countertermnorm*ctGqq*eQED**2*gQCD**5
    f( 7) = CI*countertermnorm*ctVqq*eQED**2*gQCD**5
    f( 8) = countertermnorm*ctVqq*eQED**2*gQCD**5
    f( 9) = countertermnorm*ctVVV*eQED**2*gQCD**5
    f(10) = CI*countertermnorm*ctZGG*eQED**2*gQCD**5
    f(11) = countertermnorm*ctZGG*eQED**2*gQCD**5
    f(12) = (CI*eQED**2*gQCD**5*integralnorm*SwB)/2._/**/REALKIND
    f(13) = CI*eQED**2*gQCD**5*integralnorm*SwB
    f(14) = (eQED**2*gQCD**5*integralnorm*SwB)/2._/**/REALKIND
    f(15) = eQED**2*gQCD**5*integralnorm*SwB
    f(16) = CI*eQED**2*gQCD**5*integralnorm*SwF
    f(17) = 2*CI*eQED**2*gQCD**5*integralnorm*SwF
    f(18) = eQED**2*gQCD**5*integralnorm*SwF
    f(19) = 2*eQED**2*gQCD**5*integralnorm*SwF

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
  complex(REALKIND) :: A(205)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_VQ_A(wf(:,-6),wf(:,-4),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call prop_Q_A(wf(:,3),Q(:,80),ZERO,0_intkind1,wf(:,5))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,6))
  call vert_VQ_A(wf(:,2),wf(:,5),wf(:,7))
  call prop_A_Q(wf(:,6),Q(:,35),ZERO,0_intkind1,wf(:,8))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,9))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,5),wf(:,10))
  call prop_A_Q(wf(:,9),Q(:,44),ZERO,0_intkind1,wf(:,11))
  call vert_AV_Q(wf(:,-5),wf(:,-6),wf(:,12))
  call prop_A_Q(wf(:,12),Q(:,96),ZERO,0_intkind1,wf(:,13))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,14))
  call vert_AV_Q(wf(:,13),wf(:,2),wf(:,15))
  call prop_Q_A(wf(:,14),Q(:,19),ZERO,0_intkind1,wf(:,16))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,17))
  call vert_AZ_Q(gZd,wf(:,13),wf(:,4),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,28),ZERO,0_intkind1,wf(:,19))
  call vert_VQ_A(wf(:,-6),wf(:,16),wf(:,20))
  call vert_UV_W(wf(:,2),Q(:,12),wf(:,-6),Q(:,64),wf(:,21))
  call vert_QA_V(wf(:,16),wf(:,-5),wf(:,22))
  call vert_VQ_A(wf(:,-6),wf(:,19),wf(:,23))
  call vert_QA_V(wf(:,-4),wf(:,8),wf(:,24))
  call vert_VQ_A(wf(:,-6),wf(:,-2),wf(:,25))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,26))
  call prop_Q_A(wf(:,25),Q(:,68),ZERO,0_intkind1,wf(:,27))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,4),wf(:,28))
  call vert_VQ_A(wf(:,26),wf(:,27),wf(:,29))
  call prop_A_Q(wf(:,28),Q(:,11),ZERO,0_intkind1,wf(:,30))
  call vert_AV_Q(wf(:,-3),wf(:,26),wf(:,31))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,27),wf(:,32))
  call prop_A_Q(wf(:,31),Q(:,56),ZERO,0_intkind1,wf(:,33))
  call vert_AV_Q(wf(:,-3),wf(:,-6),wf(:,34))
  call prop_A_Q(wf(:,34),Q(:,72),ZERO,0_intkind1,wf(:,35))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,-2),wf(:,36))
  call vert_AV_Q(wf(:,35),wf(:,26),wf(:,37))
  call prop_Q_A(wf(:,36),Q(:,7),ZERO,0_intkind1,wf(:,38))
  call vert_VQ_A(wf(:,26),wf(:,-2),wf(:,39))
  call vert_AZ_Q(gZd,wf(:,35),wf(:,4),wf(:,40))
  call prop_Q_A(wf(:,39),Q(:,52),ZERO,0_intkind1,wf(:,41))
  call vert_UV_W(wf(:,26),Q(:,48),wf(:,-6),Q(:,64),wf(:,42))
  call vert_QA_V(wf(:,38),wf(:,-3),wf(:,43))
  call vert_VQ_A(wf(:,-6),wf(:,38),wf(:,44))
  call vert_QA_V(wf(:,-2),wf(:,30),wf(:,45))
  call vert_VQ_A(wf(:,-6),wf(:,41),wf(:,46))
  call vert_QA_V(wf(:,27),wf(:,-3),wf(:,47))
  call vert_AV_Q(wf(:,-5),wf(:,47),wf(:,48))
  call vert_VQ_A(wf(:,47),wf(:,-4),wf(:,49))
  call vert_QA_V(wf(:,-2),wf(:,35),wf(:,50))
  call vert_AV_Q(wf(:,-5),wf(:,50),wf(:,51))
  call vert_VQ_A(wf(:,50),wf(:,-4),wf(:,52))
  call vert_QA_V(wf(:,5),wf(:,-5),wf(:,53))
  call vert_QA_V(wf(:,-4),wf(:,13),wf(:,54))
  call counter_VGG_G(ctZGGG,wf(:,4),wf(:,2),wf(:,-6),wf(:,55))
  call counter_VGG_G(ctZGGG,wf(:,4),wf(:,-6),wf(:,2),wf(:,56))
  call counter_VG_G(wf(:,4),wf(:,26),Q(:,48),wf(:,57),Q(:,51))
  call counter_VG_G(wf(:,4),wf(:,2),Q(:,12),wf(:,58),Q(:,15))
  call vert_UV_W(wf(:,2),Q(:,12),wf(:,26),Q(:,48),wf(:,59))
  call counter_VG_G(wf(:,4),wf(:,-6),Q(:,64),wf(:,60),Q(:,67))
  call counter_VQ_A(wf(:,2),wf(:,5),wf(:,61))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,5),wf(:,62))
  call counter_AV_Q(wf(:,13),wf(:,2),wf(:,63))
  call counter_AZ_Q(gZd,wf(:,13),wf(:,4),wf(:,64))
  call counter_VQ_A(wf(:,-6),wf(:,16),wf(:,65))
  call counter_UV_W(wf(:,2),Q(:,12),wf(:,-6),Q(:,64),wf(:,66))
  call counter_VQ_A(wf(:,-6),wf(:,19),wf(:,67))
  call vert_QA_V(wf(:,19),wf(:,-5),wf(:,68))
  call vert_QA_V(wf(:,-4),wf(:,11),wf(:,69))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,70))
  call prop_Q_A(wf(:,10),Q(:,83),ZERO,0_intkind1,wf(:,71))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,72))
  call prop_Q_A(wf(:,7),Q(:,92),ZERO,0_intkind1,wf(:,73))
  call counter_QA_V(wf(:,16),wf(:,-5),wf(:,74))
  call prop_A_Q(wf(:,70),Q(:,44),ZERO,0_intkind1,wf(:,75))
  call prop_A_Q(wf(:,72),Q(:,35),ZERO,0_intkind1,wf(:,76))
  call vert_QA_V(wf(:,-4),wf(:,76),wf(:,77))
  call counter_AV_Q(wf(:,-5),wf(:,-6),wf(:,78))
  call prop_A_Q(wf(:,78),Q(:,96),ZERO,0_intkind1,wf(:,79))
  call vert_AV_Q(wf(:,79),wf(:,2),wf(:,80))
  call vert_AZ_Q(gZd,wf(:,79),wf(:,4),wf(:,81))
  call counter_VQ_A(wf(:,2),wf(:,-4),wf(:,82))
  call prop_A_Q(wf(:,18),Q(:,99),ZERO,0_intkind1,wf(:,83))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,84))
  call prop_A_Q(wf(:,15),Q(:,108),ZERO,0_intkind1,wf(:,85))
  call counter_QA_V(wf(:,-4),wf(:,8),wf(:,86))
  call prop_Q_A(wf(:,82),Q(:,28),ZERO,0_intkind1,wf(:,87))
  call vert_VQ_A(wf(:,-6),wf(:,87),wf(:,88))
  call prop_Q_A(wf(:,84),Q(:,19),ZERO,0_intkind1,wf(:,89))
  call vert_VQ_A(wf(:,-6),wf(:,89),wf(:,90))
  call vert_QA_V(wf(:,89),wf(:,-5),wf(:,91))
  call counter_VQ_A(wf(:,-6),wf(:,-4),wf(:,92))
  call prop_Q_A(wf(:,92),Q(:,80),ZERO,0_intkind1,wf(:,93))
  call vert_VQ_A(wf(:,2),wf(:,93),wf(:,94))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,93),wf(:,95))
  call counter_VQ_A(wf(:,26),wf(:,27),wf(:,96))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,27),wf(:,97))
  call counter_AV_Q(wf(:,35),wf(:,26),wf(:,98))
  call counter_AZ_Q(gZd,wf(:,35),wf(:,4),wf(:,99))
  call counter_UV_W(wf(:,26),Q(:,48),wf(:,-6),Q(:,64),wf(:,100))
  call counter_VQ_A(wf(:,-6),wf(:,38),wf(:,101))
  call counter_VQ_A(wf(:,-6),wf(:,41),wf(:,102))
  call vert_QA_V(wf(:,41),wf(:,-3),wf(:,103))
  call vert_QA_V(wf(:,-2),wf(:,33),wf(:,104))
  call counter_AV_Q(wf(:,-5),wf(:,47),wf(:,105))
  call counter_AV_Q(wf(:,-5),wf(:,50),wf(:,106))
  call counter_QA_V(wf(:,5),wf(:,-5),wf(:,107))
  call vert_QA_V(wf(:,-4),wf(:,79),wf(:,108))
  call counter_VQ_A(wf(:,47),wf(:,-4),wf(:,109))
  call counter_VQ_A(wf(:,50),wf(:,-4),wf(:,110))
  call counter_QA_V(wf(:,-4),wf(:,13),wf(:,111))
  call vert_QA_V(wf(:,93),wf(:,-5),wf(:,112))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,113))
  call vert_VQ_A(wf(:,113),wf(:,27),wf(:,114))
  call vert_AV_Q(wf(:,-3),wf(:,113),wf(:,115))
  call prop_A_Q(wf(:,115),Q(:,56),ZERO,0_intkind1,wf(:,116))
  call vert_AV_Q(wf(:,35),wf(:,113),wf(:,117))
  call vert_VQ_A(wf(:,113),wf(:,-2),wf(:,118))
  call prop_Q_A(wf(:,118),Q(:,52),ZERO,0_intkind1,wf(:,119))
  call vert_UV_W(wf(:,113),Q(:,48),wf(:,-6),Q(:,64),wf(:,120))
  call vert_VQ_A(wf(:,-6),wf(:,119),wf(:,121))
  call counter_AV_Q(wf(:,-3),wf(:,26),wf(:,122))
  call prop_Q_A(wf(:,32),Q(:,71),ZERO,0_intkind1,wf(:,123))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,4),wf(:,124))
  call prop_Q_A(wf(:,29),Q(:,116),ZERO,0_intkind1,wf(:,125))
  call prop_A_Q(wf(:,122),Q(:,56),ZERO,0_intkind1,wf(:,126))
  call counter_QA_V(wf(:,38),wf(:,-3),wf(:,127))
  call prop_A_Q(wf(:,124),Q(:,11),ZERO,0_intkind1,wf(:,128))
  call vert_QA_V(wf(:,-2),wf(:,128),wf(:,129))
  call counter_AV_Q(wf(:,-3),wf(:,-6),wf(:,130))
  call prop_A_Q(wf(:,130),Q(:,72),ZERO,0_intkind1,wf(:,131))
  call vert_AV_Q(wf(:,131),wf(:,26),wf(:,132))
  call vert_AZ_Q(gZd,wf(:,131),wf(:,4),wf(:,133))
  call counter_QA_V(wf(:,27),wf(:,-3),wf(:,134))
  call vert_AV_Q(wf(:,-5),wf(:,134),wf(:,135))
  call vert_VQ_A(wf(:,134),wf(:,-4),wf(:,136))
  call vert_QA_V(wf(:,-2),wf(:,131),wf(:,137))
  call vert_AV_Q(wf(:,-5),wf(:,137),wf(:,138))
  call vert_VQ_A(wf(:,137),wf(:,-4),wf(:,139))
  call counter_VQ_A(wf(:,26),wf(:,-2),wf(:,140))
  call prop_A_Q(wf(:,40),Q(:,75),ZERO,0_intkind1,wf(:,141))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,-2),wf(:,142))
  call prop_A_Q(wf(:,37),Q(:,120),ZERO,0_intkind1,wf(:,143))
  call counter_QA_V(wf(:,-2),wf(:,30),wf(:,144))
  call prop_Q_A(wf(:,140),Q(:,52),ZERO,0_intkind1,wf(:,145))
  call vert_VQ_A(wf(:,-6),wf(:,145),wf(:,146))
  call prop_Q_A(wf(:,142),Q(:,7),ZERO,0_intkind1,wf(:,147))
  call vert_QA_V(wf(:,147),wf(:,-3),wf(:,148))
  call vert_VQ_A(wf(:,-6),wf(:,147),wf(:,149))
  call counter_VQ_A(wf(:,-6),wf(:,-2),wf(:,150))
  call prop_Q_A(wf(:,150),Q(:,68),ZERO,0_intkind1,wf(:,151))
  call vert_VQ_A(wf(:,26),wf(:,151),wf(:,152))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,151),wf(:,153))
  call counter_QA_V(wf(:,-2),wf(:,35),wf(:,154))
  call vert_AV_Q(wf(:,-5),wf(:,154),wf(:,155))
  call vert_VQ_A(wf(:,154),wf(:,-4),wf(:,156))
  call vert_QA_V(wf(:,151),wf(:,-3),wf(:,157))
  call vert_AV_Q(wf(:,-5),wf(:,157),wf(:,158))
  call vert_VQ_A(wf(:,157),wf(:,-4),wf(:,159))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,160))
  call vert_VQ_A(wf(:,160),wf(:,5),wf(:,161))
  call vert_AV_Q(wf(:,-5),wf(:,160),wf(:,162))
  call prop_A_Q(wf(:,162),Q(:,44),ZERO,0_intkind1,wf(:,163))
  call vert_AV_Q(wf(:,13),wf(:,160),wf(:,164))
  call vert_VQ_A(wf(:,160),wf(:,-4),wf(:,165))
  call prop_Q_A(wf(:,165),Q(:,28),ZERO,0_intkind1,wf(:,166))
  call vert_UV_W(wf(:,160),Q(:,12),wf(:,-6),Q(:,64),wf(:,167))
  call vert_VQ_A(wf(:,-6),wf(:,166),wf(:,168))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,169))
  call vert_AV_Q(wf(:,-5),wf(:,169),wf(:,170))
  call vert_VQ_A(wf(:,169),wf(:,5),wf(:,171))
  call counter_Q_A(ctqq,wf(:,5),Q(:,80),wf(:,172))
  call prop_Q_A(wf(:,172),Q(:,80),ZERO,0_intkind1,wf(:,173))
  call vert_VQ_A(wf(:,2),wf(:,173),wf(:,174))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,173),wf(:,175))
  call counter_A_Q(ctqq,wf(:,8),Q(:,35),wf(:,176))
  call counter_A_Q(ctqq,wf(:,11),Q(:,44),wf(:,177))
  call vert_VQ_A(wf(:,169),wf(:,-4),wf(:,178))
  call vert_VQ_A(wf(:,169),wf(:,16),wf(:,179))
  call counter_Q_A(ctqq,wf(:,16),Q(:,19),wf(:,180))
  call counter_Q_A(ctqq,wf(:,19),Q(:,28),wf(:,181))
  call counter_A_Q(ctqq,wf(:,13),Q(:,96),wf(:,182))
  call prop_A_Q(wf(:,182),Q(:,96),ZERO,0_intkind1,wf(:,183))
  call vert_AV_Q(wf(:,183),wf(:,2),wf(:,184))
  call vert_AZ_Q(gZd,wf(:,183),wf(:,4),wf(:,185))
  call vert_AV_Q(wf(:,8),wf(:,-6),wf(:,186))
  call prop_Q_A(wf(:,178),Q(:,28),ZERO,0_intkind1,wf(:,187))
  call prop_A_Q(wf(:,170),Q(:,44),ZERO,0_intkind1,wf(:,188))
  call vert_UV_W(wf(:,169),Q(:,12),wf(:,-6),Q(:,64),wf(:,189))
  call vert_AV_Q(wf(:,-5),wf(:,21),wf(:,190))
  call prop_A_Q(wf(:,190),Q(:,108),ZERO,0_intkind1,wf(:,191))
  call vert_VQ_A(wf(:,21),wf(:,-4),wf(:,192))
  call prop_Q_A(wf(:,192),Q(:,92),ZERO,0_intkind1,wf(:,193))
  call vert_AV_Q(wf(:,11),wf(:,-6),wf(:,194))
  call prop_A_Q(wf(:,194),Q(:,108),ZERO,0_intkind1,wf(:,195))
  call prop_A_Q(wf(:,186),Q(:,99),ZERO,0_intkind1,wf(:,196))
  call counter_V_V(ctGG,wf(:,21),Q(:,76),wf(:,197))
  call prop_Q_A(wf(:,20),Q(:,83),ZERO,0_intkind1,wf(:,198))
  call prop_Q_A(wf(:,23),Q(:,92),ZERO,0_intkind1,wf(:,199))
  call counter_Q_A(ctqq,wf(:,27),Q(:,68),wf(:,200))
  call prop_Q_A(wf(:,200),Q(:,68),ZERO,0_intkind1,wf(:,201))
  call vert_QA_V(wf(:,201),wf(:,30),wf(:,202))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,201),wf(:,203))
  call counter_V_V(ctGG,wf(:,26),Q(:,48),wf(:,204))
  call vert_AV_Q(wf(:,-3),wf(:,204),wf(:,205))
  call counter_A_Q(ctqq,wf(:,30),Q(:,11),wf(:,206))
  call counter_A_Q(ctqq,wf(:,33),Q(:,56),wf(:,207))
  call vert_VQ_A(wf(:,204),wf(:,27),wf(:,208))
  call vert_VQ_A(wf(:,204),wf(:,-2),wf(:,209))
  call counter_Q_A(ctqq,wf(:,38),Q(:,7),wf(:,210))
  call counter_Q_A(ctqq,wf(:,41),Q(:,52),wf(:,211))
  call counter_A_Q(ctqq,wf(:,35),Q(:,72),wf(:,212))
  call prop_A_Q(wf(:,212),Q(:,72),ZERO,0_intkind1,wf(:,213))
  call vert_QA_V(wf(:,38),wf(:,213),wf(:,214))
  call vert_AZ_Q(gZd,wf(:,213),wf(:,4),wf(:,215))
  call vert_VQ_A(wf(:,204),wf(:,38),wf(:,216))
  call vert_AV_Q(wf(:,-3),wf(:,42),wf(:,217))
  call prop_A_Q(wf(:,217),Q(:,120),ZERO,0_intkind1,wf(:,218))
  call vert_VQ_A(wf(:,42),wf(:,-2),wf(:,219))
  call prop_Q_A(wf(:,219),Q(:,116),ZERO,0_intkind1,wf(:,220))
  call vert_AV_Q(wf(:,30),wf(:,-6),wf(:,221))
  call prop_Q_A(wf(:,209),Q(:,52),ZERO,0_intkind1,wf(:,222))
  call vert_AV_Q(wf(:,33),wf(:,-6),wf(:,223))
  call prop_A_Q(wf(:,223),Q(:,120),ZERO,0_intkind1,wf(:,224))
  call counter_V_V(ctGG,wf(:,42),Q(:,112),wf(:,225))
  call prop_A_Q(wf(:,221),Q(:,75),ZERO,0_intkind1,wf(:,226))
  call prop_A_Q(wf(:,205),Q(:,56),ZERO,0_intkind1,wf(:,227))
  call prop_Q_A(wf(:,44),Q(:,71),ZERO,0_intkind1,wf(:,228))
  call prop_Q_A(wf(:,46),Q(:,116),ZERO,0_intkind1,wf(:,229))
  call vert_UV_W(wf(:,204),Q(:,48),wf(:,-6),Q(:,64),wf(:,230))
  call vert_QA_V(wf(:,201),wf(:,-3),wf(:,231))
  call counter_V_V(ctGG,wf(:,47),Q(:,76),wf(:,232))
  call prop_A_Q(wf(:,48),Q(:,108),ZERO,0_intkind1,wf(:,233))
  call prop_Q_A(wf(:,49),Q(:,92),ZERO,0_intkind1,wf(:,234))
  call vert_QA_V(wf(:,-2),wf(:,213),wf(:,235))
  call counter_V_V(ctGG,wf(:,50),Q(:,76),wf(:,236))
  call prop_A_Q(wf(:,51),Q(:,108),ZERO,0_intkind1,wf(:,237))
  call prop_Q_A(wf(:,52),Q(:,92),ZERO,0_intkind1,wf(:,238))
  call vert_AV_Q(wf(:,-3),wf(:,53),wf(:,239))
  call prop_A_Q(wf(:,239),Q(:,120),ZERO,0_intkind1,wf(:,240))
  call vert_VQ_A(wf(:,53),wf(:,-2),wf(:,241))
  call prop_Q_A(wf(:,241),Q(:,116),ZERO,0_intkind1,wf(:,242))
  call counter_V_V(ctGG,wf(:,53),Q(:,112),wf(:,243))
  call vert_QA_V(wf(:,173),wf(:,-5),wf(:,244))
  call vert_AV_Q(wf(:,-3),wf(:,54),wf(:,245))
  call prop_A_Q(wf(:,245),Q(:,120),ZERO,0_intkind1,wf(:,246))
  call vert_VQ_A(wf(:,54),wf(:,-2),wf(:,247))
  call prop_Q_A(wf(:,247),Q(:,116),ZERO,0_intkind1,wf(:,248))
  call counter_V_V(ctGG,wf(:,54),Q(:,112),wf(:,249))
  call vert_QA_V(wf(:,-4),wf(:,183),wf(:,250))
  call vert_VQ_A(wf(:,2),wf(:,16),wf(:,251))
  call prop_Q_A(wf(:,251),Q(:,31),ZERO,0_intkind1,wf(:,252))
  call vert_AV_Q(wf(:,8),wf(:,2),wf(:,253))
  call prop_A_Q(wf(:,253),Q(:,47),ZERO,0_intkind1,wf(:,254))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,19),wf(:,255))
  call prop_Q_A(wf(:,255),Q(:,31),ZERO,0_intkind1,wf(:,256))
  call vert_AZ_Q(gZd,wf(:,11),wf(:,4),wf(:,257))
  call prop_A_Q(wf(:,257),Q(:,47),ZERO,0_intkind1,wf(:,258))
  call vert_VQ_A(wf(:,26),wf(:,38),wf(:,259))
  call prop_Q_A(wf(:,259),Q(:,55),ZERO,0_intkind1,wf(:,260))
  call vert_AV_Q(wf(:,30),wf(:,26),wf(:,261))
  call prop_A_Q(wf(:,261),Q(:,59),ZERO,0_intkind1,wf(:,262))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,41),wf(:,263))
  call prop_Q_A(wf(:,263),Q(:,55),ZERO,0_intkind1,wf(:,264))
  call vert_AZ_Q(gZd,wf(:,33),wf(:,4),wf(:,265))
  call prop_A_Q(wf(:,265),Q(:,59),ZERO,0_intkind1,wf(:,266))
  call vert_QA_V(wf(:,27),wf(:,30),wf(:,267))
  call vert_QA_V(wf(:,123),wf(:,-3),wf(:,268))
  call vert_QA_V(wf(:,38),wf(:,35),wf(:,269))
  call vert_QA_V(wf(:,-2),wf(:,141),wf(:,270))
  call vert_QA_V(wf(:,5),wf(:,8),wf(:,271))
  call vert_QA_V(wf(:,71),wf(:,-5),wf(:,272))
  call vert_QA_V(wf(:,16),wf(:,13),wf(:,273))
  call vert_QA_V(wf(:,-4),wf(:,83),wf(:,274))
  call vert_VQ_A(wf(:,43),wf(:,-4),wf(:,275))
  call prop_Q_A(wf(:,275),Q(:,31),ZERO,0_intkind1,wf(:,276))
  call vert_AV_Q(wf(:,-5),wf(:,43),wf(:,277))
  call prop_A_Q(wf(:,277),Q(:,47),ZERO,0_intkind1,wf(:,278))
  call vert_UV_W(wf(:,43),Q(:,15),wf(:,-6),Q(:,64),wf(:,279))
  call vert_QA_V(wf(:,228),wf(:,-3),wf(:,280))
  call vert_VQ_A(wf(:,45),wf(:,-4),wf(:,281))
  call prop_Q_A(wf(:,281),Q(:,31),ZERO,0_intkind1,wf(:,282))
  call vert_AV_Q(wf(:,-5),wf(:,45),wf(:,283))
  call prop_A_Q(wf(:,283),Q(:,47),ZERO,0_intkind1,wf(:,284))
  call vert_UV_W(wf(:,45),Q(:,15),wf(:,-6),Q(:,64),wf(:,285))
  call vert_QA_V(wf(:,-2),wf(:,226),wf(:,286))
  call vert_VQ_A(wf(:,22),wf(:,-2),wf(:,287))
  call prop_Q_A(wf(:,287),Q(:,55),ZERO,0_intkind1,wf(:,288))
  call vert_AV_Q(wf(:,-3),wf(:,22),wf(:,289))
  call prop_A_Q(wf(:,289),Q(:,59),ZERO,0_intkind1,wf(:,290))
  call vert_UV_W(wf(:,22),Q(:,51),wf(:,-6),Q(:,64),wf(:,291))
  call vert_QA_V(wf(:,198),wf(:,-5),wf(:,292))
  call vert_VQ_A(wf(:,24),wf(:,-2),wf(:,293))
  call prop_Q_A(wf(:,293),Q(:,55),ZERO,0_intkind1,wf(:,294))
  call vert_AV_Q(wf(:,-3),wf(:,24),wf(:,295))
  call prop_A_Q(wf(:,295),Q(:,59),ZERO,0_intkind1,wf(:,296))
  call vert_UV_W(wf(:,24),Q(:,51),wf(:,-6),Q(:,64),wf(:,297))
  call vert_QA_V(wf(:,-4),wf(:,196),wf(:,298))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3) - MZ2)
  den(2) = 1 / (Q(5,12))
  den(3) = 1 / (Q(5,80))
  den(5) = 1 / (Q(5,35))
  den(9) = 1 / (Q(5,44))
  den(12) = 1 / (Q(5,96))
  den(14) = 1 / (Q(5,19))
  den(18) = 1 / (Q(5,28))
  den(22) = 1 / (Q(5,76))
  den(27) = 1 / (Q(5,68))
  den(28) = 1 / (Q(5,48))
  den(30) = 1 / (Q(5,11))
  den(34) = 1 / (Q(5,56))
  den(37) = 1 / (Q(5,72))
  den(39) = 1 / (Q(5,7))
  den(43) = 1 / (Q(5,52))
  den(46) = 1 / (Q(5,112))
  den(70) = 1 / (Q(5,60))
  den(75) = 1 / (Q(5,67))
  den(79) = 1 / (Q(5,83))
  den(82) = 1 / (Q(5,92))
  den(85) = 1 / (Q(5,99))
  den(88) = 1 / (Q(5,108))
  den(95) = 1 / (Q(5,71))
  den(98) = 1 / (Q(5,116))
  den(101) = 1 / (Q(5,75))
  den(104) = 1 / (Q(5,120))
  den(132) = 1 / (Q(5,51))
  den(180) = 1 / (Q(5,15))
  den(230) = 1 / (Q(5,31))
  den(233) = 1 / (Q(5,47))
  den(242) = 1 / (Q(5,55))
  den(245) = 1 / (Q(5,59))
  den(254) = 1 / (Q(5,79))
  den(261) = 1 / (Q(5,115))

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
  den(31) = den(1)*den(30)
  den(32) = den(29)*den(31)
  den(33) = den(1)*den(27)
  den(35) = den(28)*den(34)
  den(36) = den(33)*den(35)
  den(38) = den(28)*den(37)
  den(40) = den(1)*den(39)
  den(41) = den(38)*den(40)
  den(42) = den(1)*den(37)
  den(44) = den(28)*den(43)
  den(45) = den(42)*den(44)
  den(47) = den(28)*den(46)
  den(48) = den(40)*den(47)
  den(49) = den(35)*den(40)
  den(50) = den(31)*den(47)
  den(51) = den(31)*den(44)
  den(52) = den(22)*den(27)
  den(53) = den(15)*den(52)
  den(54) = den(6)*den(52)
  den(55) = den(22)*den(37)
  den(56) = den(15)*den(55)
  den(57) = den(6)*den(55)
  den(58) = den(3)*den(46)
  den(59) = den(40)*den(58)
  den(60) = den(31)*den(58)
  den(61) = den(12)*den(46)
  den(62) = den(40)*den(61)
  den(63) = den(31)*den(61)
  den(64) = den(1)*den(2)
  den(65) = den(28)*den(64)
  den(66) = den(1)*den(28)
  den(67) = den(23)*den(66)
  den(68) = den(47)*den(64)
  den(69) = den(2)*den(28)
  den(71) = den(69)*den(70)
  den(72) = den(1)*den(71)
  den(73) = den(58)*den(64)
  den(74) = den(61)*den(64)
  den(76) = den(1)*den(75)
  den(77) = den(19)*den(76)
  den(78) = den(10)*den(76)
  den(80) = den(8)*den(79)
  den(81) = den(2)*den(80)
  den(83) = den(4)*den(82)
  den(84) = den(1)*den(83)
  den(86) = den(17)*den(85)
  den(87) = den(2)*den(86)
  den(89) = den(13)*den(88)
  den(90) = den(1)*den(89)
  den(91) = den(52)*den(66)
  den(92) = den(55)*den(66)
  den(93) = den(44)*den(76)
  den(94) = den(35)*den(76)
  den(96) = den(33)*den(95)
  den(97) = den(28)*den(96)
  den(99) = den(29)*den(98)
  den(100) = den(1)*den(99)
  den(102) = den(42)*den(101)
  den(103) = den(28)*den(102)
  den(105) = den(38)*den(104)
  den(106) = den(1)*den(105)
  den(107) = den(2)**2
  den(108) = den(80)*den(107)
  den(109) = den(3)*den(107)
  den(110) = den(6)*den(109)
  den(111) = den(3)**2
  den(112) = den(2)*den(111)
  den(113) = den(6)*den(112)
  den(114) = den(1)*den(111)
  den(115) = den(10)*den(114)
  den(116) = den(6)*den(83)
  den(117) = den(10)*den(80)
  den(118) = den(86)*den(107)
  den(119) = den(15)*den(107)
  den(120) = den(12)*den(119)
  den(121) = den(15)*den(89)
  den(122) = den(19)*den(86)
  den(123) = den(12)**2
  den(124) = den(2)*den(123)
  den(125) = den(15)*den(124)
  den(126) = den(1)*den(123)
  den(127) = den(19)*den(126)
  den(128) = den(18)*den(107)
  den(129) = den(6)*den(128)
  den(130) = den(9)*den(107)
  den(131) = den(15)*den(130)
  den(133) = den(15)*den(132)
  den(134) = den(107)*den(133)
  den(135) = den(6)*den(132)
  den(136) = den(107)*den(135)
  den(137) = den(23)*den(88)
  den(138) = den(15)*den(137)
  den(139) = den(23)*den(82)
  den(140) = den(6)*den(139)
  den(141) = den(10)*den(88)
  den(142) = den(15)*den(141)
  den(143) = den(6)*den(85)
  den(144) = den(19)*den(143)
  den(145) = den(23)*den(135)
  den(146) = den(15)*den(79)
  den(147) = den(10)*den(146)
  den(148) = den(23)*den(133)
  den(149) = den(19)*den(82)
  den(150) = den(6)*den(149)
  den(151) = den(27)**2
  den(152) = den(31)*den(151)
  den(153) = den(28)*den(152)
  den(154) = den(1)*den(151)
  den(155) = den(35)*den(154)
  den(156) = den(28)**2
  den(157) = den(96)*den(156)
  den(158) = den(31)*den(99)
  den(159) = den(35)*den(96)
  den(160) = den(27)*den(156)
  den(161) = den(31)*den(160)
  den(162) = den(102)*den(156)
  den(163) = den(40)*den(105)
  den(164) = den(44)*den(102)
  den(165) = den(37)**2
  den(166) = den(40)*den(165)
  den(167) = den(28)*den(166)
  den(168) = den(1)*den(165)
  den(169) = den(44)*den(168)
  den(170) = den(40)*den(156)
  den(171) = den(37)*den(170)
  den(172) = den(47)*den(104)
  den(173) = den(40)*den(172)
  den(174) = den(47)*den(98)
  den(175) = den(31)*den(174)
  den(176) = den(43)*den(156)
  den(177) = den(31)*den(176)
  den(178) = den(35)*den(104)
  den(179) = den(40)*den(178)
  den(181) = den(31)*den(180)
  den(182) = den(47)*den(181)
  den(183) = den(31)*den(101)
  den(184) = den(44)*den(183)
  den(185) = den(34)*den(156)
  den(186) = den(40)*den(185)
  den(187) = den(40)*den(180)
  den(188) = den(47)*den(187)
  den(189) = den(40)*den(95)
  den(190) = den(35)*den(189)
  den(191) = den(44)*den(98)
  den(192) = den(31)*den(191)
  den(193) = den(156)*den(187)
  den(194) = den(156)*den(181)
  den(195) = den(22)*den(151)
  den(196) = den(15)*den(195)
  den(197) = den(6)*den(195)
  den(198) = den(52)*den(135)
  den(199) = den(52)*den(133)
  den(200) = den(52)*den(88)
  den(201) = den(15)*den(200)
  den(202) = den(52)*den(82)
  den(203) = den(6)*den(202)
  den(204) = den(22)*den(165)
  den(205) = den(15)*den(204)
  den(206) = den(6)*den(204)
  den(207) = den(55)*den(135)
  den(208) = den(55)*den(133)
  den(209) = den(55)*den(88)
  den(210) = den(15)*den(209)
  den(211) = den(55)*den(82)
  den(212) = den(6)*den(211)
  den(213) = den(58)*den(104)
  den(214) = den(40)*den(213)
  den(215) = den(58)*den(98)
  den(216) = den(31)*den(215)
  den(217) = den(58)*den(181)
  den(218) = den(58)*den(187)
  den(219) = den(111)*den(187)
  den(220) = den(111)*den(181)
  den(221) = den(61)*den(104)
  den(222) = den(40)*den(221)
  den(223) = den(61)*den(98)
  den(224) = den(31)*den(223)
  den(225) = den(61)*den(181)
  den(226) = den(61)*den(187)
  den(227) = den(123)*den(187)
  den(228) = den(123)*den(181)
  den(229) = den(2)*den(15)
  den(231) = den(229)*den(230)
  den(232) = den(2)*den(6)
  den(234) = den(232)*den(233)
  den(235) = den(19)*den(70)
  den(236) = den(1)*den(19)
  den(237) = den(230)*den(236)
  den(238) = den(10)*den(70)
  den(239) = den(1)*den(10)
  den(240) = den(233)*den(239)
  den(241) = den(28)*den(40)
  den(243) = den(241)*den(242)
  den(244) = den(28)*den(31)
  den(246) = den(244)*den(245)
  den(247) = den(44)*den(70)
  den(248) = den(35)*den(70)
  den(249) = den(1)*den(44)
  den(250) = den(242)*den(249)
  den(251) = den(1)*den(35)
  den(252) = den(245)*den(251)
  den(253) = den(27)*den(31)
  den(255) = den(253)*den(254)
  den(256) = den(96)*den(254)
  den(257) = den(37)*den(40)
  den(258) = den(254)*den(257)
  den(259) = den(102)*den(254)
  den(260) = den(3)*den(6)
  den(262) = den(260)*den(261)
  den(263) = den(80)*den(261)
  den(264) = den(12)*den(15)
  den(265) = den(261)*den(264)
  den(266) = den(86)*den(261)
  den(267) = den(187)*den(230)
  den(268) = den(187)*den(233)
  den(269) = den(187)*den(254)
  den(270) = den(189)*den(254)
  den(271) = den(181)*den(230)
  den(272) = den(181)*den(233)
  den(273) = den(181)*den(254)
  den(274) = den(183)*den(254)
  den(275) = den(133)*den(242)
  den(276) = den(133)*den(245)
  den(277) = den(133)*den(261)
  den(278) = den(146)*den(261)
  den(279) = den(135)*den(242)
  den(280) = den(135)*den(245)
  den(281) = den(135)*den(261)
  den(282) = den(143)*den(261)
  den(283) = den(1)*den(23)*den(28)
  den(284) = den(1)*den(2)*den(47)
  den(285) = den(1)*den(2)*den(28)
  den(286) = den(2)*den(3)*den(6)
  den(287) = den(1)*den(3)*den(10)
  den(288) = den(1)*den(2)*den(58)
  den(289) = den(1)*den(2)*den(3)
  den(290) = den(2)*den(12)*den(15)
  den(291) = den(1)*den(12)*den(19)
  den(292) = den(1)*den(2)*den(61)
  den(293) = den(1)*den(2)*den(12)
  den(294) = den(2)*den(133)
  den(295) = den(2)*den(146)
  den(296) = den(2)*den(135)
  den(297) = den(2)*den(143)
  den(298) = den(1)*den(235)
  den(299) = den(1)*den(149)
  den(300) = den(1)*den(238)
  den(301) = den(1)*den(141)
  den(302) = den(1)*den(139)
  den(303) = den(1)*den(137)
  den(304) = den(1)*den(23)
  den(305) = den(27)*den(28)*den(31)
  den(306) = den(1)*den(28)*den(52)
  den(307) = den(1)*den(27)*den(35)
  den(308) = den(1)*den(27)*den(28)
  den(309) = den(28)*den(37)*den(40)
  den(310) = den(1)*den(28)*den(55)
  den(311) = den(1)*den(37)*den(44)
  den(312) = den(1)*den(28)*den(37)
  den(313) = den(28)*den(187)
  den(314) = den(28)*den(189)
  den(315) = den(28)*den(181)
  den(316) = den(28)*den(183)
  den(317) = den(1)*den(247)
  den(318) = den(1)*den(248)
  den(319) = den(1)*den(191)
  den(320) = den(1)*den(174)
  den(321) = den(1)*den(178)
  den(322) = den(1)*den(172)
  den(323) = den(1)*den(47)
  den(324) = den(27)*den(133)
  den(325) = den(15)*den(27)
  den(326) = den(27)*den(135)
  den(327) = den(6)*den(27)
  den(328) = den(1)*den(202)
  den(329) = den(1)*den(200)
  den(330) = den(1)*den(52)
  den(331) = den(37)*den(133)
  den(332) = den(15)*den(37)
  den(333) = den(37)*den(135)
  den(334) = den(6)*den(37)
  den(335) = den(1)*den(211)
  den(336) = den(1)*den(209)
  den(337) = den(1)*den(55)
  den(338) = den(3)*den(187)
  den(339) = den(3)*den(40)
  den(340) = den(3)*den(181)
  den(341) = den(3)*den(31)
  den(342) = den(1)*den(215)
  den(343) = den(1)*den(213)
  den(344) = den(1)*den(58)
  den(345) = den(12)*den(187)
  den(346) = den(12)*den(40)
  den(347) = den(12)*den(181)
  den(348) = den(12)*den(31)
  den(349) = den(1)*den(223)
  den(350) = den(1)*den(221)
  den(351) = den(1)*den(61)
  den(352) = den(3)*den(234)
  den(353) = den(2)*den(262)
  den(354) = den(3)*den(240)
  den(355) = den(2)*den(263)
  den(356) = den(12)*den(231)
  den(357) = den(2)*den(265)
  den(358) = den(12)*den(237)
  den(359) = den(2)*den(266)
  den(360) = den(2)*den(277)
  den(361) = den(2)*den(278)
  den(362) = den(2)*den(281)
  den(363) = den(2)*den(282)
  den(364) = den(28)*den(255)
  den(365) = den(27)*den(246)
  den(366) = den(28)*den(256)
  den(367) = den(27)*den(252)
  den(368) = den(28)*den(258)
  den(369) = den(37)*den(243)
  den(370) = den(37)*den(250)
  den(371) = den(28)*den(259)
  den(372) = den(28)*den(269)
  den(373) = den(28)*den(270)
  den(374) = den(28)*den(273)
  den(375) = den(28)*den(274)
  den(376) = den(27)*den(276)
  den(377) = den(27)*den(280)
  den(378) = den(37)*den(275)
  den(379) = den(37)*den(279)
  den(380) = den(3)*den(268)
  den(381) = den(3)*den(272)
  den(382) = den(12)*den(267)
  den(383) = den(12)*den(271)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(205)

  A(1) = cont_QA(wf(:,7),wf(:,8)) * den(7)
  A(2) = cont_QA(wf(:,10),wf(:,11)) * den(11)
  A(3) = cont_QA(wf(:,15),wf(:,16)) * den(16)
  A(4) = cont_QA(wf(:,18),wf(:,19)) * den(20)
  A(5) = cont_QA(wf(:,11),wf(:,20)) * den(21)
  A(6) = cont_VV(wf(:,21),wf(:,22)) * den(24)
  A(7) = cont_QA(wf(:,8),wf(:,23)) * den(25)
  A(8) = cont_VV(wf(:,21),wf(:,24)) * den(26)
  A(9) = cont_QA(wf(:,29),wf(:,30)) * den(32)
  A(10) = cont_QA(wf(:,32),wf(:,33)) * den(36)
  A(11) = cont_QA(wf(:,37),wf(:,38)) * den(41)
  A(12) = cont_QA(wf(:,40),wf(:,41)) * den(45)
  A(13) = cont_VV(wf(:,42),wf(:,43)) * den(48)
  A(14) = cont_QA(wf(:,33),wf(:,44)) * den(49)
  A(15) = cont_VV(wf(:,42),wf(:,45)) * den(50)
  A(16) = cont_QA(wf(:,30),wf(:,46)) * den(51)
  A(17) = cont_QA(wf(:,16),wf(:,48)) * den(53)
  A(18) = cont_QA(wf(:,8),wf(:,49)) * den(54)
  A(19) = cont_QA(wf(:,16),wf(:,51)) * den(56)
  A(20) = cont_QA(wf(:,8),wf(:,52)) * den(57)
  A(21) = cont_VV(wf(:,43),wf(:,53)) * den(59)
  A(22) = cont_VV(wf(:,45),wf(:,53)) * den(60)
  A(23) = cont_VV(wf(:,43),wf(:,54)) * den(62)
  A(24) = cont_VV(wf(:,45),wf(:,54)) * den(63)

  A(25) = cont_VV(wf(:,26),wf(:,55)) * den(65)
  A(26) = cont_VV(wf(:,26),wf(:,56)) * den(65)
  A(27) = cont_VV(wf(:,21),wf(:,57)) * den(67)
  A(28) = cont_VV(wf(:,42),wf(:,58)) * den(68)
  A(29) = cont_VV(wf(:,59),wf(:,60)) * den(72)
  A(30) = cont_QA(wf(:,8),wf(:,61)) * den(7)
  A(31) = cont_QA(wf(:,11),wf(:,62)) * den(11)
  A(32) = cont_VV(wf(:,53),wf(:,58)) * den(73)
  A(33) = cont_QA(wf(:,16),wf(:,63)) * den(16)
  A(34) = cont_QA(wf(:,19),wf(:,64)) * den(20)
  A(35) = cont_VV(wf(:,54),wf(:,58)) * den(74)
  A(36) = cont_QA(wf(:,11),wf(:,65)) * den(21)
  A(37) = cont_VV(wf(:,22),wf(:,66)) * den(24)
  A(38) = cont_QA(wf(:,8),wf(:,67)) * den(25)
  A(39) = cont_VV(wf(:,24),wf(:,66)) * den(26)
  A(40) = cont_VV(wf(:,60),wf(:,68)) * den(77)
  A(41) = cont_VV(wf(:,60),wf(:,69)) * den(78)
  A(42) = cont_QA(wf(:,70),wf(:,71)) * den(81)
  A(43) = cont_QA(wf(:,72),wf(:,73)) * den(84)
  A(44) = cont_VV(wf(:,21),wf(:,74)) * den(24)
  A(45) = cont_QA(wf(:,20),wf(:,75)) * den(21)
  A(46) = cont_QA(wf(:,23),wf(:,76)) * den(25)
  A(47) = cont_VV(wf(:,21),wf(:,77)) * den(26)
  A(48) = cont_QA(wf(:,16),wf(:,80)) * den(16)
  A(49) = cont_QA(wf(:,19),wf(:,81)) * den(20)
  A(50) = cont_QA(wf(:,82),wf(:,83)) * den(87)
  A(51) = cont_QA(wf(:,84),wf(:,85)) * den(90)
  A(52) = cont_VV(wf(:,21),wf(:,86)) * den(26)
  A(53) = cont_QA(wf(:,8),wf(:,88)) * den(25)
  A(54) = cont_QA(wf(:,11),wf(:,90)) * den(21)
  A(55) = cont_VV(wf(:,21),wf(:,91)) * den(24)
  A(56) = cont_QA(wf(:,8),wf(:,94)) * den(7)
  A(57) = cont_QA(wf(:,11),wf(:,95)) * den(11)
  A(58) = cont_QA(wf(:,30),wf(:,96)) * den(32)
  A(59) = cont_VV(wf(:,47),wf(:,57)) * den(91)
  A(60) = cont_QA(wf(:,33),wf(:,97)) * den(36)
  A(61) = cont_QA(wf(:,38),wf(:,98)) * den(41)
  A(62) = cont_VV(wf(:,50),wf(:,57)) * den(92)
  A(63) = cont_QA(wf(:,41),wf(:,99)) * den(45)
  A(64) = cont_VV(wf(:,43),wf(:,100)) * den(48)
  A(65) = cont_QA(wf(:,33),wf(:,101)) * den(49)
  A(66) = cont_VV(wf(:,45),wf(:,100)) * den(50)
  A(67) = cont_QA(wf(:,30),wf(:,102)) * den(51)
  A(68) = cont_VV(wf(:,60),wf(:,103)) * den(93)
  A(69) = cont_VV(wf(:,60),wf(:,104)) * den(94)
  A(70) = cont_QA(wf(:,16),wf(:,105)) * den(53)
  A(71) = cont_QA(wf(:,49),wf(:,76)) * den(54)
  A(72) = cont_QA(wf(:,16),wf(:,106)) * den(56)
  A(73) = cont_QA(wf(:,52),wf(:,76)) * den(57)
  A(74) = cont_VV(wf(:,43),wf(:,107)) * den(59)
  A(75) = cont_VV(wf(:,45),wf(:,107)) * den(60)
  A(76) = cont_VV(wf(:,43),wf(:,108)) * den(62)
  A(77) = cont_VV(wf(:,45),wf(:,108)) * den(63)
  A(78) = cont_QA(wf(:,8),wf(:,109)) * den(54)
  A(79) = cont_QA(wf(:,48),wf(:,89)) * den(53)
  A(80) = cont_QA(wf(:,8),wf(:,110)) * den(57)
  A(81) = cont_QA(wf(:,51),wf(:,89)) * den(56)
  A(82) = cont_VV(wf(:,43),wf(:,111)) * den(62)
  A(83) = cont_VV(wf(:,45),wf(:,111)) * den(63)
  A(84) = cont_VV(wf(:,43),wf(:,112)) * den(59)
  A(85) = cont_VV(wf(:,45),wf(:,112)) * den(60)
  A(86) = cont_QA(wf(:,30),wf(:,114)) * den(32)
  A(87) = cont_QA(wf(:,32),wf(:,116)) * den(36)
  A(88) = cont_QA(wf(:,38),wf(:,117)) * den(41)
  A(89) = cont_QA(wf(:,40),wf(:,119)) * den(45)
  A(90) = cont_VV(wf(:,43),wf(:,120)) * den(48)
  A(91) = cont_QA(wf(:,44),wf(:,116)) * den(49)
  A(92) = cont_VV(wf(:,45),wf(:,120)) * den(50)
  A(93) = cont_QA(wf(:,30),wf(:,121)) * den(51)
  A(94) = cont_QA(wf(:,122),wf(:,123)) * den(97)
  A(95) = cont_QA(wf(:,124),wf(:,125)) * den(100)
  A(96) = cont_QA(wf(:,44),wf(:,126)) * den(49)
  A(97) = cont_VV(wf(:,42),wf(:,127)) * den(48)
  A(98) = cont_QA(wf(:,46),wf(:,128)) * den(51)
  A(99) = cont_VV(wf(:,42),wf(:,129)) * den(50)
  A(100) = cont_QA(wf(:,38),wf(:,132)) * den(41)
  A(101) = cont_QA(wf(:,41),wf(:,133)) * den(45)
  A(102) = cont_QA(wf(:,16),wf(:,135)) * den(53)
  A(103) = cont_QA(wf(:,8),wf(:,136)) * den(54)
  A(104) = cont_VV(wf(:,53),wf(:,127)) * den(59)
  A(105) = cont_VV(wf(:,53),wf(:,129)) * den(60)
  A(106) = cont_VV(wf(:,54),wf(:,127)) * den(62)
  A(107) = cont_VV(wf(:,54),wf(:,129)) * den(63)
  A(108) = cont_QA(wf(:,16),wf(:,138)) * den(56)
  A(109) = cont_QA(wf(:,8),wf(:,139)) * den(57)
  A(110) = cont_QA(wf(:,140),wf(:,141)) * den(103)
  A(111) = cont_QA(wf(:,142),wf(:,143)) * den(106)
  A(112) = cont_VV(wf(:,42),wf(:,144)) * den(50)
  A(113) = cont_QA(wf(:,30),wf(:,146)) * den(51)
  A(114) = cont_VV(wf(:,42),wf(:,148)) * den(48)
  A(115) = cont_QA(wf(:,33),wf(:,149)) * den(49)
  A(116) = cont_QA(wf(:,30),wf(:,152)) * den(32)
  A(117) = cont_QA(wf(:,33),wf(:,153)) * den(36)
  A(118) = cont_QA(wf(:,16),wf(:,155)) * den(56)
  A(119) = cont_QA(wf(:,8),wf(:,156)) * den(57)
  A(120) = cont_VV(wf(:,53),wf(:,144)) * den(60)
  A(121) = cont_VV(wf(:,53),wf(:,148)) * den(59)
  A(122) = cont_VV(wf(:,54),wf(:,144)) * den(63)
  A(123) = cont_VV(wf(:,54),wf(:,148)) * den(62)
  A(124) = cont_QA(wf(:,16),wf(:,158)) * den(53)
  A(125) = cont_QA(wf(:,8),wf(:,159)) * den(54)
  A(126) = cont_QA(wf(:,8),wf(:,161)) * den(7)
  A(127) = cont_QA(wf(:,10),wf(:,163)) * den(11)
  A(128) = cont_QA(wf(:,16),wf(:,164)) * den(16)
  A(129) = cont_QA(wf(:,18),wf(:,166)) * den(20)
  A(130) = cont_QA(wf(:,20),wf(:,163)) * den(21)
  A(131) = cont_VV(wf(:,22),wf(:,167)) * den(24)
  A(132) = cont_QA(wf(:,8),wf(:,168)) * den(25)
  A(133) = cont_VV(wf(:,24),wf(:,167)) * den(26)
  A(134) = cont_QA(wf(:,71),wf(:,170)) * den(108)
  A(135) = cont_QA(wf(:,8),wf(:,171)) * den(110)
  A(136) = cont_QA(wf(:,8),wf(:,174)) * den(113)
  A(137) = cont_QA(wf(:,11),wf(:,175)) * den(115)
  A(138) = cont_QA(wf(:,73),wf(:,176)) * den(116)
  A(139) = cont_QA(wf(:,71),wf(:,177)) * den(117)
  A(140) = cont_QA(wf(:,83),wf(:,178)) * den(118)
  A(141) = cont_QA(wf(:,13),wf(:,179)) * den(120)
  A(142) = cont_QA(wf(:,85),wf(:,180)) * den(121)
  A(143) = cont_QA(wf(:,83),wf(:,181)) * den(122)
  A(144) = cont_QA(wf(:,16),wf(:,184)) * den(125)
  A(145) = cont_QA(wf(:,19),wf(:,185)) * den(127)
  A(146) = cont_QA(wf(:,186),wf(:,187)) * den(129)
  A(147) = cont_QA(wf(:,20),wf(:,188)) * den(131)
  A(148) = cont_VV(wf(:,22),wf(:,189)) * den(134)
  A(149) = cont_VV(wf(:,24),wf(:,189)) * den(136)
  A(150) = cont_QA(wf(:,180),wf(:,191)) * den(138)
  A(151) = cont_QA(wf(:,176),wf(:,193)) * den(140)
  A(152) = cont_QA(wf(:,180),wf(:,195)) * den(142)
  A(153) = cont_QA(wf(:,181),wf(:,196)) * den(144)
  A(154) = cont_VV(wf(:,24),wf(:,197)) * den(145)
  A(155) = cont_QA(wf(:,177),wf(:,198)) * den(147)
  A(156) = cont_VV(wf(:,22),wf(:,197)) * den(148)
  A(157) = cont_QA(wf(:,176),wf(:,199)) * den(150)
  A(158) = cont_VV(wf(:,26),wf(:,202)) * den(153)
  A(159) = cont_QA(wf(:,33),wf(:,203)) * den(155)
  A(160) = cont_QA(wf(:,123),wf(:,205)) * den(157)
  A(161) = cont_QA(wf(:,125),wf(:,206)) * den(158)
  A(162) = cont_QA(wf(:,123),wf(:,207)) * den(159)
  A(163) = cont_QA(wf(:,30),wf(:,208)) * den(161)
  A(164) = cont_QA(wf(:,141),wf(:,209)) * den(162)
  A(165) = cont_QA(wf(:,143),wf(:,210)) * den(163)
  A(166) = cont_QA(wf(:,141),wf(:,211)) * den(164)
  A(167) = cont_VV(wf(:,26),wf(:,214)) * den(167)
  A(168) = cont_QA(wf(:,41),wf(:,215)) * den(169)
  A(169) = cont_QA(wf(:,35),wf(:,216)) * den(171)
  A(170) = cont_QA(wf(:,210),wf(:,218)) * den(173)
  A(171) = cont_QA(wf(:,206),wf(:,220)) * den(175)
  A(172) = cont_QA(wf(:,221),wf(:,222)) * den(177)
  A(173) = cont_QA(wf(:,210),wf(:,224)) * den(179)
  A(174) = cont_VV(wf(:,45),wf(:,225)) * den(182)
  A(175) = cont_QA(wf(:,211),wf(:,226)) * den(184)
  A(176) = cont_QA(wf(:,44),wf(:,227)) * den(186)
  A(177) = cont_VV(wf(:,43),wf(:,225)) * den(188)
  A(178) = cont_QA(wf(:,207),wf(:,228)) * den(190)
  A(179) = cont_QA(wf(:,206),wf(:,229)) * den(192)
  A(180) = cont_VV(wf(:,43),wf(:,230)) * den(193)
  A(181) = cont_VV(wf(:,45),wf(:,230)) * den(194)
  A(182) = cont_VV(wf(:,22),wf(:,231)) * den(196)
  A(183) = cont_VV(wf(:,24),wf(:,231)) * den(197)
  A(184) = cont_VV(wf(:,24),wf(:,232)) * den(198)
  A(185) = cont_VV(wf(:,22),wf(:,232)) * den(199)
  A(186) = cont_QA(wf(:,180),wf(:,233)) * den(201)
  A(187) = cont_QA(wf(:,176),wf(:,234)) * den(203)
  A(188) = cont_VV(wf(:,22),wf(:,235)) * den(205)
  A(189) = cont_VV(wf(:,24),wf(:,235)) * den(206)
  A(190) = cont_VV(wf(:,24),wf(:,236)) * den(207)
  A(191) = cont_VV(wf(:,22),wf(:,236)) * den(208)
  A(192) = cont_QA(wf(:,180),wf(:,237)) * den(210)
  A(193) = cont_QA(wf(:,176),wf(:,238)) * den(212)
  A(194) = cont_QA(wf(:,210),wf(:,240)) * den(214)
  A(195) = cont_QA(wf(:,206),wf(:,242)) * den(216)
  A(196) = cont_VV(wf(:,45),wf(:,243)) * den(217)
  A(197) = cont_VV(wf(:,43),wf(:,243)) * den(218)
  A(198) = cont_VV(wf(:,43),wf(:,244)) * den(219)
  A(199) = cont_VV(wf(:,45),wf(:,244)) * den(220)
  A(200) = cont_QA(wf(:,210),wf(:,246)) * den(222)
  A(201) = cont_QA(wf(:,206),wf(:,248)) * den(224)
  A(202) = cont_VV(wf(:,45),wf(:,249)) * den(225)
  A(203) = cont_VV(wf(:,43),wf(:,249)) * den(226)
  A(204) = cont_VV(wf(:,43),wf(:,250)) * den(227)
  A(205) = cont_VV(wf(:,45),wf(:,250)) * den(228)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(205)
  complex(REALKIND), intent(out) :: M1(4), M2(4)

  M1(1) = ((-A(9)-A(10)-A(11)-A(12)-A(14)-A(16)-A(17)-A(18)-A(19)-A(20))*f(1))/6._/**/REALKIND
  M1(2) = ((A(1)+A(2)+A(5)+A(11)+A(12)+A(16)+A(19)+A(20)+A(21)+A(22))*f(1))/2._/**/REALKIND+(CI*(A(6)+A(8)-A(13) &
       -A(15))*f(2))/2._/**/REALKIND
  M1(3) = ((A(3)+A(4)+A(7)+A(9)+A(10)+A(14)+A(17)+A(18)+A(23)+A(24))*f(1))/2._/**/REALKIND+(CI*(-A(6)-A(8)+A(13) &
       +A(15))*f(2))/2._/**/REALKIND
  M1(4) = ((-A(1)-A(2)-A(3)-A(4)-A(5)-A(7)-A(21)-A(22)-A(23)-A(24))*f(1))/6._/**/REALKIND

  M2(1) = ((-A(25)-A(26))*f(3))/12._/**/REALKIND+((A(158)+A(159)+A(160)+A(161)+A(162)+A(163)+A(164)+A(165)+A(166)+A(167)+A(168) &
       +A(169)+A(172)+A(173)+A(175)+A(176)+A(178)+A(179)+A(182)+A(183)+A(184)+A(185)+A(186)+A(187)+A(188)+A(189)+A(190)+A(191) &
       +A(192)+A(193))*f(3))/6._/**/REALKIND+((-A(58)-A(61)-A(65)-A(67)-A(70)-A(72)-A(78)-A(80)-A(86)-A(87)-A(88)-A(89)-A(91) &
       -A(93)-A(94)-A(96)-A(100)-A(101)-A(102)-A(103)-A(108)-A(109)-A(110)-A(113)-A(116)-A(117)-A(118)-A(119)-A(124) &
       -A(125))*f(5))/6._/**/REALKIND+((-A(60)-A(63)-A(71)-A(73)-A(79)-A(81)-A(95)-A(98)-A(111)-A(115))*f(7))/6._/**/REALKIND &
       +((A(59)+A(62)+A(68)+A(69))*f(11))/6._/**/REALKIND
  M2(2) = (A(25)*f(3))/4._/**/REALKIND+((-A(134)-A(135)-A(136)-A(137)-A(138)-A(139)-A(147)-A(152)-A(155)-A(164)-A(165)-A(166) &
       -A(167)-A(168)-A(169)-A(172)-A(175)-A(179)-A(188)-A(189)-A(190)-A(191)-A(192)-A(193)-A(194)-A(195)-A(196)-A(197)-A(198) &
       -A(199))*f(3))/2._/**/REALKIND+(CI*(-A(148)-A(149)-A(150)-A(151)-A(154)-A(156)+A(170)+A(171)+A(174)+A(177)+A(180) &
       +A(181))*f(4))/2._/**/REALKIND+((A(30)+A(36)+A(42)+A(45)+A(56)+A(57)+A(61)+A(67)+A(72)+A(74)+A(75)+A(80)+A(84)+A(85)+A(88) &
       +A(89)+A(93)+A(100)+A(101)+A(104)+A(108)+A(109)+A(110)+A(113)+A(118)+A(119)+A(120)+A(126)+A(127) &
       +A(130))*f(5))/2._/**/REALKIND+(CI*(A(44)+A(52)-A(90)-A(92)-A(97)-A(112)+A(131)+A(133))*f(6))/2._/**/REALKIND+((A(31)+A(43) &
       +A(54)+A(63)+A(73)+A(81)+A(98)+A(105)+A(111)+A(121))*f(7))/2._/**/REALKIND+(CI*(A(47)+A(55)-A(99) &
       -A(114))*f(8))/2._/**/REALKIND+(CI*(A(37)+A(39)-A(64)-A(66))*f(9))/2._/**/REALKIND+(CI*(A(27)-A(28) &
       -A(29))*f(10))/2._/**/REALKIND+((-A(32)-A(41)-A(62)-A(68))*f(11))/2._/**/REALKIND
  M2(3) = (A(26)*f(3))/4._/**/REALKIND+((-A(140)-A(141)-A(142)-A(143)-A(144)-A(145)-A(146)-A(153)-A(157)-A(158)-A(159)-A(160) &
       -A(161)-A(162)-A(163)-A(173)-A(176)-A(178)-A(182)-A(183)-A(184)-A(185)-A(186)-A(187)-A(200)-A(201)-A(202)-A(203)-A(204) &
       -A(205))*f(3))/2._/**/REALKIND+(CI*(A(148)+A(149)+A(150)+A(151)+A(154)+A(156)-A(170)-A(171)-A(174)-A(177)-A(180) &
       -A(181))*f(4))/2._/**/REALKIND+((A(33)+A(38)+A(48)+A(49)+A(50)+A(53)+A(58)+A(65)+A(70)+A(76)+A(77)+A(78)+A(82)+A(83)+A(86) &
       +A(87)+A(91)+A(94)+A(96)+A(102)+A(103)+A(106)+A(116)+A(117)+A(122)+A(124)+A(125)+A(128)+A(129) &
       +A(132))*f(5))/2._/**/REALKIND+(CI*(-A(44)-A(52)+A(90)+A(92)+A(97)+A(112)-A(131)-A(133))*f(6))/2._/**/REALKIND+((A(34) &
       +A(46)+A(51)+A(60)+A(71)+A(79)+A(95)+A(107)+A(115)+A(123))*f(7))/2._/**/REALKIND+(CI*(-A(47)-A(55)+A(99) &
       +A(114))*f(8))/2._/**/REALKIND+(CI*(-A(37)-A(39)+A(64)+A(66))*f(9))/2._/**/REALKIND+(CI*(-A(27)+A(28) &
       +A(29))*f(10))/2._/**/REALKIND+((-A(35)-A(40)-A(59)-A(69))*f(11))/2._/**/REALKIND
  M2(4) = ((-A(25)-A(26))*f(3))/12._/**/REALKIND+((A(134)+A(135)+A(136)+A(137)+A(138)+A(139)+A(140)+A(141)+A(142)+A(143)+A(144) &
       +A(145)+A(146)+A(147)+A(152)+A(153)+A(155)+A(157)+A(194)+A(195)+A(196)+A(197)+A(198)+A(199)+A(200)+A(201)+A(202)+A(203) &
       +A(204)+A(205))*f(3))/6._/**/REALKIND+((-A(30)-A(33)-A(36)-A(38)-A(42)-A(45)-A(48)-A(49)-A(50)-A(53)-A(56)-A(57)-A(74) &
       -A(75)-A(76)-A(77)-A(82)-A(83)-A(84)-A(85)-A(104)-A(106)-A(120)-A(122)-A(126)-A(127)-A(128)-A(129)-A(130) &
       -A(132))*f(5))/6._/**/REALKIND+((-A(31)-A(34)-A(43)-A(46)-A(51)-A(54)-A(105)-A(107)-A(121)-A(123))*f(7))/6._/**/REALKIND &
       +((A(32)+A(35)+A(40)+A(41))*f(11))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppnnjjj_nenexddxssxg_1_/**/REALKIND
