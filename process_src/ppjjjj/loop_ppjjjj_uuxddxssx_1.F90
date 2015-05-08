
module ol_colourmatrix_ppjjjj_uuxddxssx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(138,6), K2(6,6), KL(6,6)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1(  1,:) = [  27,   9,   9,   3,   3,   9]
  K1(  2,:) = [   9,  27,   3,   9,   9,   3]
  K1(  3,:) = [   9,   3,  27,   9,   9,   3]
  K1(  4,:) = [   3,   9,   9,  27,   3,   9]
  K1(  5,:) = [   3,   9,   9,   3,  27,   9]
  K1(  6,:) = [   9,   3,   3,   9,   9,  27]
  K1(  7,:) = [  36,  12,  12,   4,   4,  12]
  K1(  8,:) = [  12,  36,   4,  12,  12,   4]
  K1(  9,:) = [  12,   4,  36,  12,  12,   4]
  K1( 10,:) = [   4,  12,  12,  36,   4,  12]
  K1( 11,:) = [   4,  12,  12,   4,  36,  12]
  K1( 12,:) = [  12,   4,   4,  12,  12,  36]
  K1( 13,:) = [   0,   0,   0,  -4,  -4, -12]
  K1( 14,:) = [   0,   0,  -4, -12,   0,  -4]
  K1( 15,:) = [   0,  -4,   0, -12,   0,  -4]
  K1( 16,:) = [  -4, -12, -12, -36,  -4, -12]
  K1( 17,:) = [  -4,   0,   0,  -4,   0, -12]
  K1( 18,:) = [ -12,  -4,  -4, -12, -12, -36]
  K1( 19,:) = [  36,  12,  12,   4,   4,  12]
  K1( 20,:) = [  12,  36,   4,  12,  12,   4]
  K1( 21,:) = [  12,   4,  36,  12,  12,   4]
  K1( 22,:) = [   4,  12,  12,  36,   4,  12]
  K1( 23,:) = [   4,  12,  12,   4,  36,  12]
  K1( 24,:) = [  12,   4,   4,  12,  12,  36]
  K1( 25,:) = [   0,   0,  12,   4,   4,   0]
  K1( 26,:) = [   0,   0,   4,  12,   0,   4]
  K1( 27,:) = [  12,   4,   0,   0,   0,   4]
  K1( 28,:) = [   4,  12,   0,   0,   4,   0]
  K1( 29,:) = [   4,   0,   0,   4,   0,  12]
  K1( 30,:) = [   0,   4,   4,   0,  12,   0]
  K1( 31,:) = [   0, -12,   0,  -4,  -4,   0]
  K1( 32,:) = [ -12, -36,  -4, -12, -12,  -4]
  K1( 33,:) = [   0,  -4,   0,   0, -12,  -4]
  K1( 34,:) = [  -4, -12,   0,   0,  -4,   0]
  K1( 35,:) = [  -4, -12, -12,  -4, -36, -12]
  K1( 36,:) = [   0,  -4,  -4,   0, -12,   0]
  K1( 37,:) = [  36,  12,  12,   4,   4,  12]
  K1( 38,:) = [  12,  36,   4,  12,  12,   4]
  K1( 39,:) = [  12,   4,  36,  12,  12,   4]
  K1( 40,:) = [   4,  12,  12,  36,   4,  12]
  K1( 41,:) = [   4,  12,  12,   4,  36,  12]
  K1( 42,:) = [  12,   4,   4,  12,  12,  36]
  K1( 43,:) = [   0,   0, -12,  -4,  -4,   0]
  K1( 44,:) = [   0,   0,  -4,   0, -12,  -4]
  K1( 45,:) = [ -12,  -4, -36, -12, -12,  -4]
  K1( 46,:) = [  -4,   0, -12,   0,  -4,   0]
  K1( 47,:) = [  -4, -12, -12,  -4, -36, -12]
  K1( 48,:) = [   0,  -4,  -4,   0, -12,   0]
  K1( 49,:) = [   0,  12,   0,   4,   4,   0]
  K1( 50,:) = [  12,   0,   4,   0,   0,   4]
  K1( 51,:) = [   0,   4,   0,  12,   0,   4]
  K1( 52,:) = [   4,   0,  12,   0,   4,   0]
  K1( 53,:) = [   4,   0,   0,   4,   0,  12]
  K1( 54,:) = [   0,   4,   4,   0,  12,   0]
  K1( 55,:) = [ -36, -12, -12,  -4,  -4, -12]
  K1( 56,:) = [ -12,   0,  -4,   0,   0,  -4]
  K1( 57,:) = [ -12,  -4,   0,   0,   0,  -4]
  K1( 58,:) = [  -4,   0,   0,   0,  -4, -12]
  K1( 59,:) = [  -4,   0,   0,  -4,   0, -12]
  K1( 60,:) = [ -12,  -4,  -4, -12, -12, -36]
  K1( 61,:) = [  36,  12,  12,   4,   4,  12]
  K1( 62,:) = [  12,  36,   4,  12,  12,   4]
  K1( 63,:) = [  12,   4,  36,  12,  12,   4]
  K1( 64,:) = [   4,  12,  12,  36,   4,  12]
  K1( 65,:) = [   4,  12,  12,   4,  36,  12]
  K1( 66,:) = [  12,   4,   4,  12,  12,  36]
  K1( 67,:) = [   0,   0,   0,   4,   4,  12]
  K1( 68,:) = [   0,   0,   4,   0,  12,   4]
  K1( 69,:) = [   0,   4,   0,  12,   0,   4]
  K1( 70,:) = [   4,   0,  12,   0,   4,   0]
  K1( 71,:) = [   4,  12,   0,   4,   0,   0]
  K1( 72,:) = [  12,   4,   4,   0,   0,   0]
  K1( 73,:) = [ -36, -12, -12,  -4,  -4, -12]
  K1( 74,:) = [ -12,   0,  -4,   0,   0,  -4]
  K1( 75,:) = [ -12,  -4, -36, -12, -12,  -4]
  K1( 76,:) = [  -4,   0, -12,   0,  -4,   0]
  K1( 77,:) = [  -4,   0, -12,  -4,   0,   0]
  K1( 78,:) = [ -12,  -4,  -4,   0,   0,   0]
  K1( 79,:) = [   0,  12,   0,   4,   4,   0]
  K1( 80,:) = [  12,   0,   4,   0,   0,   4]
  K1( 81,:) = [   0,   4,   0,   0,  12,   4]
  K1( 82,:) = [   4,   0,   0,   0,   4,  12]
  K1( 83,:) = [   4,   0,  12,   4,   0,   0]
  K1( 84,:) = [   0,   4,   4,  12,   0,   0]
  K1( 85,:) = [   0, -12,   0,  -4,  -4,   0]
  K1( 86,:) = [ -12, -36,  -4, -12, -12,  -4]
  K1( 87,:) = [   0,  -4,   0, -12,   0,  -4]
  K1( 88,:) = [  -4, -12, -12, -36,  -4, -12]
  K1( 89,:) = [  -4, -12,   0,  -4,   0,   0]
  K1( 90,:) = [   0,  -4,  -4, -12,   0,   0]
  K1( 91,:) = [  36,  12,  12,   4,   4,  12]
  K1( 92,:) = [  12,  36,   4,  12,  12,   4]
  K1( 93,:) = [  12,   4,  36,  12,  12,   4]
  K1( 94,:) = [   4,  12,  12,  36,   4,  12]
  K1( 95,:) = [   4,  12,  12,   4,  36,  12]
  K1( 96,:) = [  12,   4,   4,  12,  12,  36]
  K1( 97,:) = [ -36, -12, -12,  -4,  -4, -12]
  K1( 98,:) = [ -12, -36,  -4, -12, -12,  -4]
  K1( 99,:) = [ -12,  -4,   0,   0,   0,  -4]
  K1(100,:) = [  -4, -12,   0,   0,  -4,   0]
  K1(101,:) = [  -4, -12,   0,  -4,   0,   0]
  K1(102,:) = [ -12,  -4,  -4,   0,   0,   0]
  K1(103,:) = [   0,   0,   0,   4,   4,  12]
  K1(104,:) = [   0,   0,   4,  12,   0,   4]
  K1(105,:) = [   0,   4,   0,   0,  12,   4]
  K1(106,:) = [   4,  12,   0,   0,   4,   0]
  K1(107,:) = [   4,   0,  12,   4,   0,   0]
  K1(108,:) = [  12,   4,   4,   0,   0,   0]
  K1(109,:) = [   0,   0, -12,  -4,  -4,   0]
  K1(110,:) = [   0,   0,  -4, -12,   0,  -4]
  K1(111,:) = [ -12,  -4, -36, -12, -12,  -4]
  K1(112,:) = [  -4, -12, -12, -36,  -4, -12]
  K1(113,:) = [  -4,   0, -12,  -4,   0,   0]
  K1(114,:) = [   0,  -4,  -4, -12,   0,   0]
  K1(115,:) = [   0,   0,  12,   4,   4,   0]
  K1(116,:) = [   0,   0,   4,   0,  12,   4]
  K1(117,:) = [  12,   4,   0,   0,   0,   4]
  K1(118,:) = [   4,   0,   0,   0,   4,  12]
  K1(119,:) = [   4,  12,   0,   4,   0,   0]
  K1(120,:) = [   0,   4,   4,  12,   0,   0]
  K1(121,:) = [   0,   0,   0,  -4,  -4, -12]
  K1(122,:) = [   0,   0,  -4,   0, -12,  -4]
  K1(123,:) = [   0,  -4,   0,   0, -12,  -4]
  K1(124,:) = [  -4,   0,   0,   0,  -4, -12]
  K1(125,:) = [  -4, -12, -12,  -4, -36, -12]
  K1(126,:) = [ -12,  -4,  -4, -12, -12, -36]
  K1(127,:) = [  36,  12,  12,   4,   4,  12]
  K1(128,:) = [  12,  36,   4,  12,  12,   4]
  K1(129,:) = [  12,   4,  36,  12,  12,   4]
  K1(130,:) = [   4,  12,  12,  36,   4,  12]
  K1(131,:) = [   4,  12,  12,   4,  36,  12]
  K1(132,:) = [  12,   4,   4,  12,  12,  36]
  K1(133,:) = [   0,   0,   0,   0,   0,   0]
  K1(134,:) = [   0,   0,   0,   0,   0,   0]
  K1(135,:) = [   0,   0,   0,   0,   0,   0]
  K1(136,:) = [   0,   0,   0,   0,   0,   0]
  K1(137,:) = [   0,   0,   0,   0,   0,   0]
  K1(138,:) = [   0,   0,   0,   0,   0,   0]

  K2(1,:) = [ 27,  9,  9,  3,  3,  9]
  K2(2,:) = [  9, 27,  3,  9,  9,  3]
  K2(3,:) = [  9,  3, 27,  9,  9,  3]
  K2(4,:) = [  3,  9,  9, 27,  3,  9]
  K2(5,:) = [  3,  9,  9,  3, 27,  9]
  K2(6,:) = [  9,  3,  3,  9,  9, 27]

  KL(1,:) = [ 27,  9,  9,  3,  3,  9]
  KL(2,:) = [  9, 27,  3,  9,  9,  3]
  KL(3,:) = [  9,  3, 27,  9,  9,  3]
  KL(4,:) = [  3,  9,  9, 27,  3,  9]
  KL(5,:) = [  3,  9,  9,  3, 27,  9]
  KL(6,:) = [  9,  3,  3,  9,  9, 27]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppjjjj_uuxddxssx_1_/**/REALKIND



module ol_forced_parameters_ppjjjj_uuxddxssx_1_/**/REALKIND
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


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppjjjj_uuxddxssx_1_/**/REALKIND

module ol_loop_ppjjjj_uuxddxssx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(15), c(36)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:99)
  ! denominators
  complex(REALKIND), save :: den(100)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(6,64)
  ! zero helicity identifier
  logical,           save :: zerohel(64) = .true., zerohel_ct(64) = .true.

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
    f( 1) = CI*gQCD**4
    f( 2) = gQCD**4
    f( 3) = CI*countertermnorm*gQCD**6
    f( 4) = countertermnorm*gQCD**6
    f( 5) = CI*countertermnorm*ctGqq*gQCD**6
    f( 6) = countertermnorm*ctGqq*gQCD**6
    f( 7) = countertermnorm*ctVVV*gQCD**6
    f( 8) = (CI*gQCD**6*integralnorm*SwB)/2._/**/REALKIND
    f( 9) = CI*gQCD**6*integralnorm*SwB
    f(10) = (gQCD**6*integralnorm*SwB)/2._/**/REALKIND
    f(11) = gQCD**6*integralnorm*SwB
    f(12) = CI*gQCD**6*integralnorm*SwF
    f(13) = 2*CI*gQCD**6*integralnorm*SwF
    f(14) = gQCD**6*integralnorm*SwF
    f(15) = 2*gQCD**6*integralnorm*SwF

  c = [ 81*CI*f(8), 162*CI*f(8), 9*CI*f(9), 27*CI*f(9), 72*CI*f(9), 81*CI*f(9), 162*CI*f(9), 18*f(10), 54*f(10), 162*f(10), f(11) &
    , 3*f(11), 6*f(11), 8*f(11), 9*f(11), 10*f(11), 18*f(11), 21*f(11), 24*f(11), 27*f(11), 30*f(11), 54*f(11), 63*f(11), 72*f(11) &
    , 81*f(11), 162*f(11), 27*CI*f(12), 27*CI*f(13), 3*f(14), 6*f(14), 9*f(14), 27*f(14), 3*f(15), 6*f(15), 9*f(15), 27*f(15) ]
  c = (1._/**/REALKIND / 216) * c
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
  complex(REALKIND), intent(out) :: M1(6), M2(6)
  complex(REALKIND) :: A(56)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,3))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,2),Q(:,12),wf(:,4))
  call vert_VQ_A(wf(:,1),wf(:,-4),wf(:,5))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,6))
  call prop_Q_A(wf(:,5),Q(:,19),ZERO,0_intkind1,wf(:,7))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,8))
  call vert_AV_Q(wf(:,-5),wf(:,1),wf(:,9))
  call prop_Q_A(wf(:,8),Q(:,28),ZERO,0_intkind1,wf(:,10))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,11))
  call vert_AV_Q(wf(:,-3),wf(:,3),wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,7),ZERO,0_intkind1,wf(:,13))
  call vert_VQ_A(wf(:,3),wf(:,-2),wf(:,14))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,15))
  call prop_Q_A(wf(:,14),Q(:,52),ZERO,0_intkind1,wf(:,16))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,17))
  call vert_AV_Q(wf(:,-1),wf(:,3),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,13),ZERO,0_intkind1,wf(:,19))
  call vert_VQ_A(wf(:,3),wf(:,0),wf(:,20))
  call vert_AV_Q(wf(:,-1),wf(:,2),wf(:,21))
  call prop_Q_A(wf(:,20),Q(:,49),ZERO,0_intkind1,wf(:,22))
  call counter_UV_W(wf(:,1),Q(:,3),wf(:,2),Q(:,12),wf(:,23))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,24))
  call counter_AV_Q(wf(:,-5),wf(:,1),wf(:,25))
  call counter_VQ_A(wf(:,2),wf(:,-4),wf(:,26))
  call prop_A_Q(wf(:,9),Q(:,35),ZERO,0_intkind1,wf(:,27))
  call counter_VQ_A(wf(:,1),wf(:,-4),wf(:,28))
  call prop_A_Q(wf(:,6),Q(:,44),ZERO,0_intkind1,wf(:,29))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,30))
  call vert_AV_Q(wf(:,-3),wf(:,30),wf(:,31))
  call vert_VQ_A(wf(:,30),wf(:,-2),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,52),ZERO,0_intkind1,wf(:,33))
  call counter_AV_Q(wf(:,-3),wf(:,3),wf(:,34))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,35))
  call counter_VQ_A(wf(:,3),wf(:,-2),wf(:,36))
  call prop_A_Q(wf(:,15),Q(:,11),ZERO,0_intkind1,wf(:,37))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,38))
  call prop_A_Q(wf(:,12),Q(:,56),ZERO,0_intkind1,wf(:,39))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,40))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,40),Q(:,12),wf(:,41))
  call vert_AV_Q(wf(:,-5),wf(:,40),wf(:,42))
  call vert_VQ_A(wf(:,40),wf(:,-4),wf(:,43))
  call prop_Q_A(wf(:,43),Q(:,28),ZERO,0_intkind1,wf(:,44))
  call vert_AV_Q(wf(:,-1),wf(:,30),wf(:,45))
  call vert_VQ_A(wf(:,30),wf(:,0),wf(:,46))
  call prop_Q_A(wf(:,46),Q(:,49),ZERO,0_intkind1,wf(:,47))
  call vert_AV_Q(wf(:,-1),wf(:,40),wf(:,48))
  call vert_VQ_A(wf(:,40),wf(:,0),wf(:,49))
  call prop_Q_A(wf(:,49),Q(:,13),ZERO,0_intkind1,wf(:,50))
  call counter_AV_Q(wf(:,-1),wf(:,3),wf(:,51))
  call counter_AV_Q(wf(:,-1),wf(:,2),wf(:,52))
  call counter_VQ_A(wf(:,2),wf(:,0),wf(:,53))
  call prop_A_Q(wf(:,18),Q(:,50),ZERO,0_intkind1,wf(:,54))
  call counter_VQ_A(wf(:,3),wf(:,0),wf(:,55))
  call prop_A_Q(wf(:,21),Q(:,14),ZERO,0_intkind1,wf(:,56))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,57))
  call vert_UV_W(wf(:,57),Q(:,3),wf(:,2),Q(:,12),wf(:,58))
  call vert_VQ_A(wf(:,57),wf(:,-4),wf(:,59))
  call prop_Q_A(wf(:,59),Q(:,19),ZERO,0_intkind1,wf(:,60))
  call vert_AV_Q(wf(:,-5),wf(:,57),wf(:,61))
  call vert_VQ_A(wf(:,57),wf(:,-2),wf(:,62))
  call prop_Q_A(wf(:,62),Q(:,7),ZERO,0_intkind1,wf(:,63))
  call vert_AV_Q(wf(:,-3),wf(:,57),wf(:,64))
  call vert_UV_W(wf(:,2),Q(:,12),wf(:,3),Q(:,48),wf(:,65))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,66))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,3),Q(:,48),wf(:,67))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,68))
  call counter_V_V(ctGG,wf(:,3),Q(:,48),wf(:,69))
  call vert_VQ_A(wf(:,66),wf(:,-4),wf(:,70))
  call vert_AV_Q(wf(:,-5),wf(:,66),wf(:,71))
  call vert_VQ_A(wf(:,68),wf(:,-4),wf(:,72))
  call vert_AV_Q(wf(:,-5),wf(:,68),wf(:,73))
  call counter_Q_A(ctqq,wf(:,7),Q(:,19),wf(:,74))
  call counter_Q_A(ctqq,wf(:,10),Q(:,28),wf(:,75))
  call vert_VQ_A(wf(:,66),wf(:,-2),wf(:,76))
  call vert_AV_Q(wf(:,-3),wf(:,66),wf(:,77))
  call counter_Q_A(ctqq,wf(:,13),Q(:,7),wf(:,78))
  call counter_Q_A(ctqq,wf(:,16),Q(:,52),wf(:,79))
  call vert_VQ_A(wf(:,69),wf(:,-2),wf(:,80))
  call vert_AV_Q(wf(:,-3),wf(:,69),wf(:,81))
  call counter_Q_A(ctqq,wf(:,19),Q(:,13),wf(:,82))
  call counter_Q_A(ctqq,wf(:,22),Q(:,49),wf(:,83))
  call vert_VQ_A(wf(:,68),wf(:,0),wf(:,84))
  call vert_VQ_A(wf(:,69),wf(:,0),wf(:,85))
  call vert_AV_Q(wf(:,-1),wf(:,68),wf(:,86))
  call vert_AV_Q(wf(:,-1),wf(:,69),wf(:,87))
  call vert_QA_V(wf(:,13),wf(:,-3),wf(:,88))
  call vert_QA_V(wf(:,-2),wf(:,37),wf(:,89))
  call vert_QA_V(wf(:,7),wf(:,-5),wf(:,90))
  call vert_QA_V(wf(:,-4),wf(:,27),wf(:,91))
  call vert_QA_V(wf(:,19),wf(:,-1),wf(:,92))
  call vert_QA_V(wf(:,0),wf(:,56),wf(:,93))
  call vert_QA_V(wf(:,10),wf(:,-5),wf(:,94))
  call vert_QA_V(wf(:,-4),wf(:,29),wf(:,95))
  call vert_QA_V(wf(:,22),wf(:,-1),wf(:,96))
  call vert_QA_V(wf(:,0),wf(:,54),wf(:,97))
  call vert_QA_V(wf(:,16),wf(:,-3),wf(:,98))
  call vert_QA_V(wf(:,-2),wf(:,39),wf(:,99))

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
  den(2) = 1 / (Q(5,12))
  den(3) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,19))
  den(9) = 1 / (Q(5,28))
  den(12) = 1 / (Q(5,7))
  den(15) = 1 / (Q(5,52))
  den(18) = 1 / (Q(5,13))
  den(21) = 1 / (Q(5,49))
  den(24) = 1 / (Q(5,35))
  den(27) = 1 / (Q(5,44))
  den(30) = 1 / (Q(5,11))
  den(33) = 1 / (Q(5,56))
  den(36) = 1 / (Q(5,50))
  den(39) = 1 / (Q(5,14))
  den(43) = 1 / (Q(5,60))
  den(47) = 1 / (Q(5,51))
  den(50) = 1 / (Q(5,15))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(13) = den(1)*den(12)
  den(14) = den(3)*den(13)
  den(16) = den(3)*den(15)
  den(17) = den(1)*den(16)
  den(19) = den(2)*den(18)
  den(20) = den(3)*den(19)
  den(22) = den(3)*den(21)
  den(23) = den(2)*den(22)
  den(25) = den(1)*den(24)
  den(26) = den(2)*den(25)
  den(28) = den(2)*den(27)
  den(29) = den(1)*den(28)
  den(31) = den(1)*den(30)
  den(32) = den(3)*den(31)
  den(34) = den(3)*den(33)
  den(35) = den(1)*den(34)
  den(37) = den(3)*den(36)
  den(38) = den(2)*den(37)
  den(40) = den(2)*den(39)
  den(41) = den(3)*den(40)
  den(42) = den(2)*den(3)
  den(44) = den(42)*den(43)
  den(45) = den(1)*den(44)
  den(46) = den(1)*den(3)
  den(48) = den(46)*den(47)
  den(49) = den(2)*den(48)
  den(51) = den(4)*den(50)
  den(52) = den(3)*den(51)
  den(53) = den(1)**2
  den(54) = den(28)*den(53)
  den(55) = den(10)*den(53)
  den(56) = den(2)**2
  den(57) = den(25)*den(56)
  den(58) = den(7)*den(56)
  den(59) = den(7)*den(28)
  den(60) = den(10)*den(25)
  den(61) = den(34)*den(53)
  den(62) = den(16)*den(53)
  den(63) = den(13)*den(34)
  den(64) = den(16)*den(31)
  den(65) = den(3)**2
  den(66) = den(31)*den(65)
  den(67) = den(13)*den(65)
  den(68) = den(19)*den(37)
  den(69) = den(22)*den(40)
  den(70) = den(37)*den(56)
  den(71) = den(40)*den(65)
  den(72) = den(22)*den(56)
  den(73) = den(19)*den(65)
  den(74) = den(13)*den(50)
  den(75) = den(31)*den(50)
  den(76) = den(7)*den(47)
  den(77) = den(25)*den(47)
  den(78) = den(19)*den(50)
  den(79) = den(40)*den(50)
  den(80) = den(10)*den(43)
  den(81) = den(28)*den(43)
  den(82) = den(22)*den(47)
  den(83) = den(37)*den(47)
  den(84) = den(16)*den(43)
  den(85) = den(34)*den(43)
  den(86) = den(1)*den(2)*den(3)
  den(87) = den(1)*den(42)
  den(88) = den(2)*den(46)
  den(89) = den(2)*den(76)
  den(90) = den(2)*den(77)
  den(91) = den(1)*den(80)
  den(92) = den(1)*den(81)
  den(93) = den(3)*den(74)
  den(94) = den(3)*den(75)
  den(95) = den(1)*den(84)
  den(96) = den(1)*den(85)
  den(97) = den(3)*den(78)
  den(98) = den(2)*den(82)
  den(99) = den(3)*den(79)
  den(100) = den(2)*den(83)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(56)

  A(1) = cont_VV(wf(:,3),wf(:,4)) * den(5)
  A(2) = cont_QA(wf(:,6),wf(:,7)) * den(8)
  A(3) = cont_QA(wf(:,9),wf(:,10)) * den(11)
  A(4) = cont_QA(wf(:,12),wf(:,13)) * den(14)
  A(5) = cont_QA(wf(:,15),wf(:,16)) * den(17)
  A(6) = cont_QA(wf(:,18),wf(:,19)) * den(20)
  A(7) = cont_QA(wf(:,21),wf(:,22)) * den(23)

  A(8) = cont_VV(wf(:,3),wf(:,23)) * den(5)
  A(9) = cont_QA(wf(:,7),wf(:,24)) * den(8)
  A(10) = cont_QA(wf(:,10),wf(:,25)) * den(11)
  A(11) = cont_QA(wf(:,26),wf(:,27)) * den(26)
  A(12) = cont_QA(wf(:,28),wf(:,29)) * den(29)
  A(13) = cont_VV(wf(:,4),wf(:,30)) * den(5)
  A(14) = cont_QA(wf(:,13),wf(:,31)) * den(14)
  A(15) = cont_QA(wf(:,15),wf(:,33)) * den(17)
  A(16) = cont_QA(wf(:,13),wf(:,34)) * den(14)
  A(17) = cont_QA(wf(:,16),wf(:,35)) * den(17)
  A(18) = cont_QA(wf(:,36),wf(:,37)) * den(32)
  A(19) = cont_QA(wf(:,38),wf(:,39)) * den(35)
  A(20) = cont_VV(wf(:,3),wf(:,41)) * den(5)
  A(21) = cont_QA(wf(:,7),wf(:,42)) * den(8)
  A(22) = cont_QA(wf(:,9),wf(:,44)) * den(11)
  A(23) = cont_QA(wf(:,19),wf(:,45)) * den(20)
  A(24) = cont_QA(wf(:,21),wf(:,47)) * den(23)
  A(25) = cont_QA(wf(:,22),wf(:,48)) * den(23)
  A(26) = cont_QA(wf(:,18),wf(:,50)) * den(20)
  A(27) = cont_QA(wf(:,19),wf(:,51)) * den(20)
  A(28) = cont_QA(wf(:,22),wf(:,52)) * den(23)
  A(29) = cont_QA(wf(:,53),wf(:,54)) * den(38)
  A(30) = cont_QA(wf(:,55),wf(:,56)) * den(41)
  A(31) = cont_VV(wf(:,3),wf(:,58)) * den(5)
  A(32) = cont_QA(wf(:,6),wf(:,60)) * den(8)
  A(33) = cont_QA(wf(:,10),wf(:,61)) * den(11)
  A(34) = cont_QA(wf(:,12),wf(:,63)) * den(14)
  A(35) = cont_QA(wf(:,16),wf(:,64)) * den(17)
  A(36) = cont_VV(wf(:,65),wf(:,66)) * den(45)
  A(37) = cont_VV(wf(:,67),wf(:,68)) * den(49)
  A(38) = cont_VV(wf(:,4),wf(:,69)) * den(52)
  A(39) = cont_QA(wf(:,29),wf(:,70)) * den(54)
  A(40) = cont_QA(wf(:,10),wf(:,71)) * den(55)
  A(41) = cont_QA(wf(:,27),wf(:,72)) * den(57)
  A(42) = cont_QA(wf(:,7),wf(:,73)) * den(58)
  A(43) = cont_QA(wf(:,29),wf(:,74)) * den(59)
  A(44) = cont_QA(wf(:,27),wf(:,75)) * den(60)
  A(45) = cont_QA(wf(:,39),wf(:,76)) * den(61)
  A(46) = cont_QA(wf(:,16),wf(:,77)) * den(62)
  A(47) = cont_QA(wf(:,39),wf(:,78)) * den(63)
  A(48) = cont_QA(wf(:,37),wf(:,79)) * den(64)
  A(49) = cont_QA(wf(:,37),wf(:,80)) * den(66)
  A(50) = cont_QA(wf(:,13),wf(:,81)) * den(67)
  A(51) = cont_QA(wf(:,54),wf(:,82)) * den(68)
  A(52) = cont_QA(wf(:,56),wf(:,83)) * den(69)
  A(53) = cont_QA(wf(:,54),wf(:,84)) * den(70)
  A(54) = cont_QA(wf(:,56),wf(:,85)) * den(71)
  A(55) = cont_QA(wf(:,22),wf(:,86)) * den(72)
  A(56) = cont_QA(wf(:,19),wf(:,87)) * den(73)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(56)
  complex(REALKIND), intent(out) :: M1(6), M2(6)

  M1(1) = ((-A(2)-A(3)-A(6)-A(7))*f(1))/12._/**/REALKIND
  M1(2) = ((A(3)+A(4)+A(7))*f(1))/4._/**/REALKIND+(CI*A(1)*f(2))/4._/**/REALKIND
  M1(3) = ((A(2)+A(5)+A(6))*f(1))/4._/**/REALKIND-(CI*A(1)*f(2))/4._/**/REALKIND
  M1(4) = ((-A(2)-A(3)-A(4)-A(5))*f(1))/12._/**/REALKIND
  M1(5) = ((-A(4)-A(5)-A(6)-A(7))*f(1))/12._/**/REALKIND
  M1(6) = ((A(2)+A(3)+A(4)+A(5)+A(6)+A(7))*f(1))/36._/**/REALKIND

  M2(1) = ((A(39)+A(40)+A(41)+A(42)+A(43)+A(44)+A(51)+A(52)+A(53)+A(54)+A(55)+A(56))*f(3))/12._/**/REALKIND+((-A(9)-A(10)-A(11) &
       -A(12)-A(21)-A(22)-A(23)-A(24)-A(25)-A(26)-A(27)-A(28)-A(29)-A(30)-A(32)-A(33))*f(5))/12._/**/REALKIND
  M2(2) = ((-A(40)-A(41)-A(44)-A(45)-A(47)-A(50)-A(52)-A(54)-A(55))*f(3))/4._/**/REALKIND+(CI*(-A(36)+A(37) &
       -A(38))*f(4))/4._/**/REALKIND+((A(10)+A(11)+A(14)+A(16)+A(19)+A(22)+A(24)+A(25)+A(28)+A(30)+A(33) &
       +A(34))*f(5))/4._/**/REALKIND+(CI*(A(13)+A(20)+A(31))*f(6))/4._/**/REALKIND+(CI*A(8)*f(7))/4._/**/REALKIND
  M2(3) = ((-A(39)-A(42)-A(43)-A(46)-A(48)-A(49)-A(51)-A(53)-A(56))*f(3))/4._/**/REALKIND+(CI*(A(36)-A(37) &
       +A(38))*f(4))/4._/**/REALKIND+((A(9)+A(12)+A(15)+A(17)+A(18)+A(21)+A(23)+A(26)+A(27)+A(29)+A(32) &
       +A(35))*f(5))/4._/**/REALKIND+(CI*(-A(13)-A(20)-A(31))*f(6))/4._/**/REALKIND-(CI*A(8)*f(7))/4._/**/REALKIND
  M2(4) = ((A(39)+A(40)+A(41)+A(42)+A(43)+A(44)+A(45)+A(46)+A(47)+A(48)+A(49)+A(50))*f(3))/12._/**/REALKIND+((-A(9)-A(10)-A(11) &
       -A(12)-A(14)-A(15)-A(16)-A(17)-A(18)-A(19)-A(21)-A(22)-A(32)-A(33)-A(34)-A(35))*f(5))/12._/**/REALKIND
  M2(5) = ((A(45)+A(46)+A(47)+A(48)+A(49)+A(50)+A(51)+A(52)+A(53)+A(54)+A(55)+A(56))*f(3))/12._/**/REALKIND+((-A(14)-A(15)-A(16) &
       -A(17)-A(18)-A(19)-A(23)-A(24)-A(25)-A(26)-A(27)-A(28)-A(29)-A(30)-A(34)-A(35))*f(5))/12._/**/REALKIND
  M2(6) = ((-A(39)-A(40)-A(41)-A(42)-A(43)-A(44)-A(45)-A(46)-A(47)-A(48)-A(49)-A(50)-A(51)-A(52)-A(53)-A(54)-A(55) &
       -A(56))*f(3))/36._/**/REALKIND+((A(9)+A(10)+A(11)+A(12)+A(14)+A(15)+A(16)+A(17)+A(18)+A(19)+A(21)+A(22)+A(23)+A(24)+A(25) &
       +A(26)+A(27)+A(28)+A(29)+A(30)+A(32)+A(33)+A(34)+A(35))*f(5))/36._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppjjjj_uuxddxssx_1_/**/REALKIND
