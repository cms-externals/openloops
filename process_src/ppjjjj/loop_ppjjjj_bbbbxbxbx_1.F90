
module ol_colourmatrix_ppjjjj_bbbbxbxbx_1_/**/REALKIND
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
  K1( 13,:) = [   0,   0,  12,   4,   4,   0]
  K1( 14,:) = [   0,   0,   4,  12,   0,   4]
  K1( 15,:) = [  12,   4,   0,   0,   0,   4]
  K1( 16,:) = [   4,  12,   0,   0,   4,   0]
  K1( 17,:) = [   4,   0,   0,   4,   0,  12]
  K1( 18,:) = [   0,   4,   4,   0,  12,   0]
  K1( 19,:) = [  36,  12,  12,   4,   4,  12]
  K1( 20,:) = [  12,  36,   4,  12,  12,   4]
  K1( 21,:) = [  12,   4,  36,  12,  12,   4]
  K1( 22,:) = [   4,  12,  12,  36,   4,  12]
  K1( 23,:) = [   4,  12,  12,   4,  36,  12]
  K1( 24,:) = [  12,   4,   4,  12,  12,  36]
  K1( 25,:) = [   0,   0,   0,   4,   4,  12]
  K1( 26,:) = [   0,   0,   4,   0,  12,   4]
  K1( 27,:) = [   0,   4,   0,  12,   0,   4]
  K1( 28,:) = [   4,   0,  12,   0,   4,   0]
  K1( 29,:) = [   4,  12,   0,   4,   0,   0]
  K1( 30,:) = [  12,   4,   4,   0,   0,   0]
  K1( 31,:) = [   0,  12,   0,   4,   4,   0]
  K1( 32,:) = [  12,   0,   4,   0,   0,   4]
  K1( 33,:) = [   0,   4,   0,   0,  12,   4]
  K1( 34,:) = [   4,   0,   0,   0,   4,  12]
  K1( 35,:) = [   4,   0,  12,   4,   0,   0]
  K1( 36,:) = [   0,   4,   4,  12,   0,   0]
  K1( 37,:) = [  36,  12,  12,   4,   4,  12]
  K1( 38,:) = [  12,  36,   4,  12,  12,   4]
  K1( 39,:) = [  12,   4,  36,  12,  12,   4]
  K1( 40,:) = [   4,  12,  12,  36,   4,  12]
  K1( 41,:) = [   4,  12,  12,   4,  36,  12]
  K1( 42,:) = [  12,   4,   4,  12,  12,  36]
  K1( 43,:) = [   0,   0,   0,  -4,  -4, -12]
  K1( 44,:) = [   0,   0,  -4, -12,   0,  -4]
  K1( 45,:) = [   0,  -4,   0, -12,   0,  -4]
  K1( 46,:) = [  -4, -12, -12, -36,  -4, -12]
  K1( 47,:) = [  -4,   0,   0,  -4,   0, -12]
  K1( 48,:) = [ -12,  -4,  -4, -12, -12, -36]
  K1( 49,:) = [   0, -12,   0,  -4,  -4,   0]
  K1( 50,:) = [ -12, -36,  -4, -12, -12,  -4]
  K1( 51,:) = [   0,  -4,   0,   0, -12,  -4]
  K1( 52,:) = [  -4, -12,   0,   0,  -4,   0]
  K1( 53,:) = [  -4, -12, -12,  -4, -36, -12]
  K1( 54,:) = [   0,  -4,  -4,   0, -12,   0]
  K1( 55,:) = [ -36, -12, -12,  -4,  -4, -12]
  K1( 56,:) = [ -12,   0,  -4,   0,   0,  -4]
  K1( 57,:) = [ -12,  -4, -36, -12, -12,  -4]
  K1( 58,:) = [  -4,   0, -12,   0,  -4,   0]
  K1( 59,:) = [  -4,   0, -12,  -4,   0,   0]
  K1( 60,:) = [ -12,  -4,  -4,   0,   0,   0]
  K1( 61,:) = [  36,  12,  12,   4,   4,  12]
  K1( 62,:) = [  12,  36,   4,  12,  12,   4]
  K1( 63,:) = [  12,   4,  36,  12,  12,   4]
  K1( 64,:) = [   4,  12,  12,  36,   4,  12]
  K1( 65,:) = [   4,  12,  12,   4,  36,  12]
  K1( 66,:) = [  12,   4,   4,  12,  12,  36]
  K1( 67,:) = [   0,   0, -12,  -4,  -4,   0]
  K1( 68,:) = [   0,   0,  -4,   0, -12,  -4]
  K1( 69,:) = [ -12,  -4, -36, -12, -12,  -4]
  K1( 70,:) = [  -4,   0, -12,   0,  -4,   0]
  K1( 71,:) = [  -4, -12, -12,  -4, -36, -12]
  K1( 72,:) = [   0,  -4,  -4,   0, -12,   0]
  K1( 73,:) = [ -36, -12, -12,  -4,  -4, -12]
  K1( 74,:) = [ -12,   0,  -4,   0,   0,  -4]
  K1( 75,:) = [ -12,  -4,   0,   0,   0,  -4]
  K1( 76,:) = [  -4,   0,   0,   0,  -4, -12]
  K1( 77,:) = [  -4,   0,   0,  -4,   0, -12]
  K1( 78,:) = [ -12,  -4,  -4, -12, -12, -36]
  K1( 79,:) = [   0, -12,   0,  -4,  -4,   0]
  K1( 80,:) = [ -12, -36,  -4, -12, -12,  -4]
  K1( 81,:) = [   0,  -4,   0, -12,   0,  -4]
  K1( 82,:) = [  -4, -12, -12, -36,  -4, -12]
  K1( 83,:) = [  -4, -12,   0,  -4,   0,   0]
  K1( 84,:) = [   0,  -4,  -4, -12,   0,   0]
  K1( 85,:) = [   0,  12,   0,   4,   4,   0]
  K1( 86,:) = [  12,   0,   4,   0,   0,   4]
  K1( 87,:) = [   0,   4,   0,  12,   0,   4]
  K1( 88,:) = [   4,   0,  12,   0,   4,   0]
  K1( 89,:) = [   4,   0,   0,   4,   0,  12]
  K1( 90,:) = [   0,   4,   4,   0,  12,   0]
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
  K1(103,:) = [   0,   0, -12,  -4,  -4,   0]
  K1(104,:) = [   0,   0,  -4, -12,   0,  -4]
  K1(105,:) = [ -12,  -4, -36, -12, -12,  -4]
  K1(106,:) = [  -4, -12, -12, -36,  -4, -12]
  K1(107,:) = [  -4,   0, -12,  -4,   0,   0]
  K1(108,:) = [   0,  -4,  -4, -12,   0,   0]
  K1(109,:) = [   0,   0,   0,  -4,  -4, -12]
  K1(110,:) = [   0,   0,  -4,   0, -12,  -4]
  K1(111,:) = [   0,  -4,   0,   0, -12,  -4]
  K1(112,:) = [  -4,   0,   0,   0,  -4, -12]
  K1(113,:) = [  -4, -12, -12,  -4, -36, -12]
  K1(114,:) = [ -12,  -4,  -4, -12, -12, -36]
  K1(115,:) = [   0,   0,   0,   4,   4,  12]
  K1(116,:) = [   0,   0,   4,  12,   0,   4]
  K1(117,:) = [   0,   4,   0,   0,  12,   4]
  K1(118,:) = [   4,  12,   0,   0,   4,   0]
  K1(119,:) = [   4,   0,  12,   4,   0,   0]
  K1(120,:) = [  12,   4,   4,   0,   0,   0]
  K1(121,:) = [   0,   0,  12,   4,   4,   0]
  K1(122,:) = [   0,   0,   4,   0,  12,   4]
  K1(123,:) = [  12,   4,   0,   0,   0,   4]
  K1(124,:) = [   4,   0,   0,   0,   4,  12]
  K1(125,:) = [   4,  12,   0,   4,   0,   0]
  K1(126,:) = [   0,   4,   4,  12,   0,   0]
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
end module ol_colourmatrix_ppjjjj_bbbbxbxbx_1_/**/REALKIND



module ol_forced_parameters_ppjjjj_bbbbxbxbx_1_/**/REALKIND
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
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppjjjj_bbbbxbxbx_1_/**/REALKIND

module ol_loop_ppjjjj_bbbbxbxbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(15), c(36)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:351)
  ! denominators
  complex(REALKIND), save :: den(483)
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
    f( 5) = CI*countertermnorm*ctGbb*gQCD**6
    f( 6) = countertermnorm*ctGbb*gQCD**6
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
  complex(REALKIND) :: A(336)
  ! external WFs
  call wf_Q(P(:,1), rMB, H(1), wf(:,0))
  call wf_Q(P(:,2), rMB, H(2), wf(:,-1))
  call wf_Q(P(:,3), rMB, H(3), wf(:,-2))
  call wf_A(P(:,4), rMB, H(4), wf(:,-3))
  call wf_A(P(:,5), rMB, H(5), wf(:,-4))
  call wf_A(P(:,6), rMB, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-3),wf(:,1))
  call vert_QA_V(wf(:,-1),wf(:,-4),wf(:,2))
  call vert_QA_V(wf(:,-2),wf(:,-5),wf(:,3))
  call vert_UV_W(wf(:,1),Q(:,9),wf(:,2),Q(:,18),wf(:,4))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,5))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,6))
  call prop_Q_A(wf(:,5),Q(:,13),MB,1_intkind1,wf(:,7))
  call vert_VQ_A(wf(:,2),wf(:,-2),wf(:,8))
  call vert_AV_Q(wf(:,-5),wf(:,1),wf(:,9))
  call prop_Q_A(wf(:,8),Q(:,22),MB,1_intkind1,wf(:,10))
  call vert_QA_V(wf(:,-1),wf(:,-5),wf(:,11))
  call vert_QA_V(wf(:,-2),wf(:,-4),wf(:,12))
  call vert_UV_W(wf(:,1),Q(:,9),wf(:,11),Q(:,34),wf(:,13))
  call vert_VQ_A(wf(:,1),wf(:,-1),wf(:,14))
  call vert_AV_Q(wf(:,-5),wf(:,12),wf(:,15))
  call prop_Q_A(wf(:,14),Q(:,11),MB,1_intkind1,wf(:,16))
  call vert_VQ_A(wf(:,12),wf(:,-1),wf(:,17))
  call prop_Q_A(wf(:,17),Q(:,22),MB,1_intkind1,wf(:,18))
  call vert_AV_Q(wf(:,-4),wf(:,11),wf(:,19))
  call vert_VQ_A(wf(:,11),wf(:,-2),wf(:,20))
  call vert_AV_Q(wf(:,-4),wf(:,1),wf(:,21))
  call prop_Q_A(wf(:,20),Q(:,38),MB,1_intkind1,wf(:,22))
  call vert_AV_Q(wf(:,-4),wf(:,3),wf(:,23))
  call vert_VQ_A(wf(:,3),wf(:,-1),wf(:,24))
  call prop_Q_A(wf(:,24),Q(:,38),MB,1_intkind1,wf(:,25))
  call vert_QA_V(wf(:,0),wf(:,-4),wf(:,26))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,27))
  call vert_UV_W(wf(:,27),Q(:,10),wf(:,26),Q(:,17),wf(:,28))
  call vert_VQ_A(wf(:,26),wf(:,-2),wf(:,29))
  call vert_AV_Q(wf(:,-5),wf(:,27),wf(:,30))
  call prop_Q_A(wf(:,29),Q(:,21),MB,1_intkind1,wf(:,31))
  call vert_VQ_A(wf(:,27),wf(:,-2),wf(:,32))
  call vert_AV_Q(wf(:,-5),wf(:,26),wf(:,33))
  call prop_Q_A(wf(:,32),Q(:,14),MB,1_intkind1,wf(:,34))
  call vert_QA_V(wf(:,0),wf(:,-5),wf(:,35))
  call vert_UV_W(wf(:,27),Q(:,10),wf(:,35),Q(:,33),wf(:,36))
  call vert_VQ_A(wf(:,27),wf(:,0),wf(:,37))
  call prop_Q_A(wf(:,37),Q(:,11),MB,1_intkind1,wf(:,38))
  call vert_VQ_A(wf(:,12),wf(:,0),wf(:,39))
  call prop_Q_A(wf(:,39),Q(:,21),MB,1_intkind1,wf(:,40))
  call vert_VQ_A(wf(:,35),wf(:,-2),wf(:,41))
  call vert_AV_Q(wf(:,-4),wf(:,27),wf(:,42))
  call prop_Q_A(wf(:,41),Q(:,37),MB,1_intkind1,wf(:,43))
  call vert_AV_Q(wf(:,-4),wf(:,35),wf(:,44))
  call vert_VQ_A(wf(:,3),wf(:,0),wf(:,45))
  call prop_Q_A(wf(:,45),Q(:,37),MB,1_intkind1,wf(:,46))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,47))
  call vert_UV_W(wf(:,26),Q(:,17),wf(:,11),Q(:,34),wf(:,48))
  call vert_VQ_A(wf(:,26),wf(:,-1),wf(:,49))
  call vert_AV_Q(wf(:,-5),wf(:,47),wf(:,50))
  call prop_Q_A(wf(:,49),Q(:,19),MB,1_intkind1,wf(:,51))
  call vert_VQ_A(wf(:,47),wf(:,-1),wf(:,52))
  call prop_Q_A(wf(:,52),Q(:,14),MB,1_intkind1,wf(:,53))
  call vert_UV_W(wf(:,2),Q(:,18),wf(:,35),Q(:,33),wf(:,54))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,55))
  call prop_Q_A(wf(:,55),Q(:,19),MB,1_intkind1,wf(:,56))
  call vert_VQ_A(wf(:,47),wf(:,0),wf(:,57))
  call prop_Q_A(wf(:,57),Q(:,13),MB,1_intkind1,wf(:,58))
  call vert_VQ_A(wf(:,35),wf(:,-1),wf(:,59))
  call vert_AV_Q(wf(:,-4),wf(:,47),wf(:,60))
  call prop_Q_A(wf(:,59),Q(:,35),MB,1_intkind1,wf(:,61))
  call vert_VQ_A(wf(:,11),wf(:,0),wf(:,62))
  call prop_Q_A(wf(:,62),Q(:,35),MB,1_intkind1,wf(:,63))
  call vert_AV_Q(wf(:,-3),wf(:,11),wf(:,64))
  call vert_AV_Q(wf(:,-3),wf(:,26),wf(:,65))
  call vert_AV_Q(wf(:,-3),wf(:,3),wf(:,66))
  call vert_AV_Q(wf(:,-3),wf(:,2),wf(:,67))
  call vert_AV_Q(wf(:,-3),wf(:,35),wf(:,68))
  call vert_AV_Q(wf(:,-3),wf(:,12),wf(:,69))
  call counter_UV_W(wf(:,1),Q(:,9),wf(:,2),Q(:,18),wf(:,70))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,71))
  call counter_AV_Q(wf(:,-5),wf(:,1),wf(:,72))
  call counter_UV_W(wf(:,1),Q(:,9),wf(:,11),Q(:,34),wf(:,73))
  call counter_AV_Q(wf(:,-5),wf(:,12),wf(:,74))
  call counter_AV_Q(wf(:,-4),wf(:,11),wf(:,75))
  call counter_AV_Q(wf(:,-4),wf(:,1),wf(:,76))
  call counter_AV_Q(wf(:,-4),wf(:,3),wf(:,77))
  call counter_UV_W(wf(:,27),Q(:,10),wf(:,26),Q(:,17),wf(:,78))
  call counter_AV_Q(wf(:,-5),wf(:,27),wf(:,79))
  call counter_AV_Q(wf(:,-5),wf(:,26),wf(:,80))
  call counter_UV_W(wf(:,27),Q(:,10),wf(:,35),Q(:,33),wf(:,81))
  call counter_AV_Q(wf(:,-4),wf(:,27),wf(:,82))
  call counter_AV_Q(wf(:,-4),wf(:,35),wf(:,83))
  call counter_UV_W(wf(:,26),Q(:,17),wf(:,11),Q(:,34),wf(:,84))
  call counter_AV_Q(wf(:,-5),wf(:,47),wf(:,85))
  call counter_UV_W(wf(:,2),Q(:,18),wf(:,35),Q(:,33),wf(:,86))
  call counter_AV_Q(wf(:,-4),wf(:,47),wf(:,87))
  call counter_AV_Q(wf(:,-3),wf(:,11),wf(:,88))
  call counter_AV_Q(wf(:,-3),wf(:,26),wf(:,89))
  call counter_AV_Q(wf(:,-3),wf(:,3),wf(:,90))
  call counter_AV_Q(wf(:,-3),wf(:,2),wf(:,91))
  call counter_AV_Q(wf(:,-3),wf(:,35),wf(:,92))
  call counter_AV_Q(wf(:,-3),wf(:,12),wf(:,93))
  call counter_VQ_A(wf(:,2),wf(:,-2),wf(:,94))
  call prop_A_Q(wf(:,9),Q(:,41),MB,1_intkind1,wf(:,95))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,96))
  call prop_A_Q(wf(:,6),Q(:,50),MB,1_intkind1,wf(:,97))
  call counter_QA_V(wf(:,-2),wf(:,-5),wf(:,98))
  call counter_VQ_A(wf(:,11),wf(:,-2),wf(:,99))
  call prop_A_Q(wf(:,21),Q(:,25),MB,1_intkind1,wf(:,100))
  call prop_A_Q(wf(:,19),Q(:,50),MB,1_intkind1,wf(:,101))
  call vert_AV_Q(wf(:,-4),wf(:,98),wf(:,102))
  call vert_VQ_A(wf(:,98),wf(:,-1),wf(:,103))
  call prop_Q_A(wf(:,103),Q(:,38),MB,1_intkind1,wf(:,104))
  call counter_QA_V(wf(:,-2),wf(:,-4),wf(:,105))
  call vert_AV_Q(wf(:,-5),wf(:,105),wf(:,106))
  call vert_VQ_A(wf(:,105),wf(:,-1),wf(:,107))
  call prop_Q_A(wf(:,107),Q(:,22),MB,1_intkind1,wf(:,108))
  call counter_VQ_A(wf(:,27),wf(:,-2),wf(:,109))
  call prop_A_Q(wf(:,33),Q(:,49),MB,1_intkind1,wf(:,110))
  call counter_VQ_A(wf(:,26),wf(:,-2),wf(:,111))
  call prop_A_Q(wf(:,30),Q(:,42),MB,1_intkind1,wf(:,112))
  call prop_A_Q(wf(:,44),Q(:,49),MB,1_intkind1,wf(:,113))
  call counter_VQ_A(wf(:,35),wf(:,-2),wf(:,114))
  call prop_A_Q(wf(:,42),Q(:,26),MB,1_intkind1,wf(:,115))
  call vert_VQ_A(wf(:,98),wf(:,0),wf(:,116))
  call prop_Q_A(wf(:,116),Q(:,37),MB,1_intkind1,wf(:,117))
  call vert_VQ_A(wf(:,105),wf(:,0),wf(:,118))
  call prop_Q_A(wf(:,118),Q(:,21),MB,1_intkind1,wf(:,119))
  call prop_A_Q(wf(:,65),Q(:,25),MB,1_intkind1,wf(:,120))
  call prop_A_Q(wf(:,64),Q(:,42),MB,1_intkind1,wf(:,121))
  call vert_AV_Q(wf(:,-3),wf(:,98),wf(:,122))
  call prop_A_Q(wf(:,68),Q(:,41),MB,1_intkind1,wf(:,123))
  call prop_A_Q(wf(:,67),Q(:,26),MB,1_intkind1,wf(:,124))
  call vert_AV_Q(wf(:,-3),wf(:,105),wf(:,125))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,126))
  call vert_AV_Q(wf(:,-5),wf(:,126),wf(:,127))
  call vert_VQ_A(wf(:,126),wf(:,-1),wf(:,128))
  call prop_Q_A(wf(:,128),Q(:,14),MB,1_intkind1,wf(:,129))
  call vert_VQ_A(wf(:,126),wf(:,0),wf(:,130))
  call prop_Q_A(wf(:,130),Q(:,13),MB,1_intkind1,wf(:,131))
  call vert_AV_Q(wf(:,-4),wf(:,126),wf(:,132))
  call counter_VQ_A(wf(:,12),wf(:,-1),wf(:,133))
  call counter_VQ_A(wf(:,1),wf(:,-1),wf(:,134))
  call prop_A_Q(wf(:,15),Q(:,52),MB,1_intkind1,wf(:,135))
  call counter_QA_V(wf(:,-1),wf(:,-5),wf(:,136))
  call vert_UV_W(wf(:,1),Q(:,9),wf(:,136),Q(:,34),wf(:,137))
  call counter_VQ_A(wf(:,3),wf(:,-1),wf(:,138))
  call prop_A_Q(wf(:,23),Q(:,52),MB,1_intkind1,wf(:,139))
  call vert_AV_Q(wf(:,-4),wf(:,136),wf(:,140))
  call vert_VQ_A(wf(:,136),wf(:,-2),wf(:,141))
  call prop_Q_A(wf(:,141),Q(:,38),MB,1_intkind1,wf(:,142))
  call counter_QA_V(wf(:,-1),wf(:,-4),wf(:,143))
  call vert_UV_W(wf(:,1),Q(:,9),wf(:,143),Q(:,18),wf(:,144))
  call vert_AV_Q(wf(:,-5),wf(:,143),wf(:,145))
  call vert_VQ_A(wf(:,143),wf(:,-2),wf(:,146))
  call prop_Q_A(wf(:,146),Q(:,22),MB,1_intkind1,wf(:,147))
  call counter_VQ_A(wf(:,47),wf(:,-1),wf(:,148))
  call counter_VQ_A(wf(:,26),wf(:,-1),wf(:,149))
  call prop_A_Q(wf(:,50),Q(:,44),MB,1_intkind1,wf(:,150))
  call vert_UV_W(wf(:,26),Q(:,17),wf(:,136),Q(:,34),wf(:,151))
  call counter_VQ_A(wf(:,35),wf(:,-1),wf(:,152))
  call prop_A_Q(wf(:,60),Q(:,28),MB,1_intkind1,wf(:,153))
  call vert_VQ_A(wf(:,136),wf(:,0),wf(:,154))
  call prop_Q_A(wf(:,154),Q(:,35),MB,1_intkind1,wf(:,155))
  call vert_UV_W(wf(:,143),Q(:,18),wf(:,35),Q(:,33),wf(:,156))
  call vert_VQ_A(wf(:,143),wf(:,0),wf(:,157))
  call prop_Q_A(wf(:,157),Q(:,19),MB,1_intkind1,wf(:,158))
  call prop_A_Q(wf(:,66),Q(:,44),MB,1_intkind1,wf(:,159))
  call vert_AV_Q(wf(:,-3),wf(:,136),wf(:,160))
  call prop_A_Q(wf(:,69),Q(:,28),MB,1_intkind1,wf(:,161))
  call vert_AV_Q(wf(:,-3),wf(:,143),wf(:,162))
  call counter_QA_V(wf(:,-1),wf(:,-3),wf(:,163))
  call vert_UV_W(wf(:,163),Q(:,10),wf(:,26),Q(:,17),wf(:,164))
  call vert_AV_Q(wf(:,-5),wf(:,163),wf(:,165))
  call vert_VQ_A(wf(:,163),wf(:,-2),wf(:,166))
  call prop_Q_A(wf(:,166),Q(:,14),MB,1_intkind1,wf(:,167))
  call vert_UV_W(wf(:,163),Q(:,10),wf(:,35),Q(:,33),wf(:,168))
  call vert_VQ_A(wf(:,163),wf(:,0),wf(:,169))
  call prop_Q_A(wf(:,169),Q(:,11),MB,1_intkind1,wf(:,170))
  call vert_AV_Q(wf(:,-4),wf(:,163),wf(:,171))
  call counter_VQ_A(wf(:,27),wf(:,0),wf(:,172))
  call counter_VQ_A(wf(:,12),wf(:,0),wf(:,173))
  call counter_QA_V(wf(:,0),wf(:,-5),wf(:,174))
  call vert_UV_W(wf(:,27),Q(:,10),wf(:,174),Q(:,33),wf(:,175))
  call counter_VQ_A(wf(:,3),wf(:,0),wf(:,176))
  call vert_VQ_A(wf(:,174),wf(:,-2),wf(:,177))
  call prop_Q_A(wf(:,177),Q(:,37),MB,1_intkind1,wf(:,178))
  call vert_AV_Q(wf(:,-4),wf(:,174),wf(:,179))
  call counter_QA_V(wf(:,0),wf(:,-4),wf(:,180))
  call vert_UV_W(wf(:,27),Q(:,10),wf(:,180),Q(:,17),wf(:,181))
  call vert_VQ_A(wf(:,180),wf(:,-2),wf(:,182))
  call prop_Q_A(wf(:,182),Q(:,21),MB,1_intkind1,wf(:,183))
  call vert_AV_Q(wf(:,-5),wf(:,180),wf(:,184))
  call counter_VQ_A(wf(:,2),wf(:,0),wf(:,185))
  call counter_VQ_A(wf(:,47),wf(:,0),wf(:,186))
  call vert_UV_W(wf(:,2),Q(:,18),wf(:,174),Q(:,33),wf(:,187))
  call counter_VQ_A(wf(:,11),wf(:,0),wf(:,188))
  call vert_VQ_A(wf(:,174),wf(:,-1),wf(:,189))
  call prop_Q_A(wf(:,189),Q(:,35),MB,1_intkind1,wf(:,190))
  call vert_UV_W(wf(:,180),Q(:,17),wf(:,11),Q(:,34),wf(:,191))
  call vert_VQ_A(wf(:,180),wf(:,-1),wf(:,192))
  call prop_Q_A(wf(:,192),Q(:,19),MB,1_intkind1,wf(:,193))
  call vert_AV_Q(wf(:,-3),wf(:,174),wf(:,194))
  call vert_AV_Q(wf(:,-3),wf(:,180),wf(:,195))
  call counter_QA_V(wf(:,0),wf(:,-3),wf(:,196))
  call vert_UV_W(wf(:,196),Q(:,9),wf(:,2),Q(:,18),wf(:,197))
  call vert_VQ_A(wf(:,196),wf(:,-2),wf(:,198))
  call prop_Q_A(wf(:,198),Q(:,13),MB,1_intkind1,wf(:,199))
  call vert_AV_Q(wf(:,-5),wf(:,196),wf(:,200))
  call vert_UV_W(wf(:,196),Q(:,9),wf(:,11),Q(:,34),wf(:,201))
  call vert_VQ_A(wf(:,196),wf(:,-1),wf(:,202))
  call prop_Q_A(wf(:,202),Q(:,11),MB,1_intkind1,wf(:,203))
  call vert_AV_Q(wf(:,-4),wf(:,196),wf(:,204))
  call vert_UV_W(wf(:,2),Q(:,18),wf(:,3),Q(:,36),wf(:,205))
  call counter_V_V(ctGG,wf(:,1),Q(:,9),wf(:,206))
  call vert_UV_W(wf(:,1),Q(:,9),wf(:,3),Q(:,36),wf(:,207))
  call counter_V_V(ctGG,wf(:,2),Q(:,18),wf(:,208))
  call counter_V_V(ctGG,wf(:,3),Q(:,36),wf(:,209))
  call vert_VQ_A(wf(:,206),wf(:,-2),wf(:,210))
  call vert_AV_Q(wf(:,-5),wf(:,206),wf(:,211))
  call vert_VQ_A(wf(:,208),wf(:,-2),wf(:,212))
  call vert_AV_Q(wf(:,-5),wf(:,208),wf(:,213))
  call counter_Q_A(ctbb,wf(:,7),Q(:,13),wf(:,214))
  call counter_Q_A(ctbb,wf(:,10),Q(:,22),wf(:,215))
  call vert_UV_W(wf(:,12),Q(:,20),wf(:,11),Q(:,34),wf(:,216))
  call vert_UV_W(wf(:,1),Q(:,9),wf(:,12),Q(:,20),wf(:,217))
  call counter_V_V(ctGG,wf(:,11),Q(:,34),wf(:,218))
  call counter_V_V(ctGG,wf(:,12),Q(:,20),wf(:,219))
  call vert_VQ_A(wf(:,206),wf(:,-1),wf(:,220))
  call vert_VQ_A(wf(:,219),wf(:,-1),wf(:,221))
  call counter_Q_A(ctbb,wf(:,16),Q(:,11),wf(:,222))
  call counter_Q_A(ctbb,wf(:,18),Q(:,22),wf(:,223))
  call vert_AV_Q(wf(:,-5),wf(:,219),wf(:,224))
  call vert_AV_Q(wf(:,-4),wf(:,206),wf(:,225))
  call vert_VQ_A(wf(:,218),wf(:,-2),wf(:,226))
  call vert_AV_Q(wf(:,-4),wf(:,218),wf(:,227))
  call counter_Q_A(ctbb,wf(:,22),Q(:,38),wf(:,228))
  call vert_VQ_A(wf(:,209),wf(:,-1),wf(:,229))
  call counter_Q_A(ctbb,wf(:,25),Q(:,38),wf(:,230))
  call vert_AV_Q(wf(:,-4),wf(:,209),wf(:,231))
  call vert_UV_W(wf(:,27),Q(:,10),wf(:,3),Q(:,36),wf(:,232))
  call counter_V_V(ctGG,wf(:,26),Q(:,17),wf(:,233))
  call vert_UV_W(wf(:,26),Q(:,17),wf(:,3),Q(:,36),wf(:,234))
  call counter_V_V(ctGG,wf(:,27),Q(:,10),wf(:,235))
  call vert_VQ_A(wf(:,233),wf(:,-2),wf(:,236))
  call vert_AV_Q(wf(:,-5),wf(:,233),wf(:,237))
  call vert_VQ_A(wf(:,235),wf(:,-2),wf(:,238))
  call vert_AV_Q(wf(:,-5),wf(:,235),wf(:,239))
  call counter_Q_A(ctbb,wf(:,31),Q(:,21),wf(:,240))
  call counter_Q_A(ctbb,wf(:,34),Q(:,14),wf(:,241))
  call vert_UV_W(wf(:,27),Q(:,10),wf(:,12),Q(:,20),wf(:,242))
  call counter_V_V(ctGG,wf(:,35),Q(:,33),wf(:,243))
  call vert_UV_W(wf(:,12),Q(:,20),wf(:,35),Q(:,33),wf(:,244))
  call vert_VQ_A(wf(:,235),wf(:,0),wf(:,245))
  call vert_VQ_A(wf(:,219),wf(:,0),wf(:,246))
  call counter_Q_A(ctbb,wf(:,38),Q(:,11),wf(:,247))
  call counter_Q_A(ctbb,wf(:,40),Q(:,21),wf(:,248))
  call vert_VQ_A(wf(:,243),wf(:,-2),wf(:,249))
  call vert_AV_Q(wf(:,-4),wf(:,243),wf(:,250))
  call vert_AV_Q(wf(:,-4),wf(:,235),wf(:,251))
  call counter_Q_A(ctbb,wf(:,43),Q(:,37),wf(:,252))
  call vert_VQ_A(wf(:,209),wf(:,0),wf(:,253))
  call counter_Q_A(ctbb,wf(:,46),Q(:,37),wf(:,254))
  call vert_UV_W(wf(:,47),Q(:,12),wf(:,11),Q(:,34),wf(:,255))
  call vert_UV_W(wf(:,47),Q(:,12),wf(:,26),Q(:,17),wf(:,256))
  call counter_V_V(ctGG,wf(:,47),Q(:,12),wf(:,257))
  call vert_VQ_A(wf(:,233),wf(:,-1),wf(:,258))
  call vert_VQ_A(wf(:,257),wf(:,-1),wf(:,259))
  call counter_Q_A(ctbb,wf(:,51),Q(:,19),wf(:,260))
  call counter_Q_A(ctbb,wf(:,53),Q(:,14),wf(:,261))
  call vert_AV_Q(wf(:,-5),wf(:,257),wf(:,262))
  call vert_UV_W(wf(:,47),Q(:,12),wf(:,2),Q(:,18),wf(:,263))
  call vert_UV_W(wf(:,47),Q(:,12),wf(:,35),Q(:,33),wf(:,264))
  call vert_VQ_A(wf(:,208),wf(:,0),wf(:,265))
  call vert_VQ_A(wf(:,257),wf(:,0),wf(:,266))
  call counter_Q_A(ctbb,wf(:,56),Q(:,19),wf(:,267))
  call counter_Q_A(ctbb,wf(:,58),Q(:,13),wf(:,268))
  call vert_VQ_A(wf(:,243),wf(:,-1),wf(:,269))
  call counter_Q_A(ctbb,wf(:,61),Q(:,35),wf(:,270))
  call vert_AV_Q(wf(:,-4),wf(:,257),wf(:,271))
  call vert_VQ_A(wf(:,218),wf(:,0),wf(:,272))
  call counter_Q_A(ctbb,wf(:,63),Q(:,35),wf(:,273))
  call vert_AV_Q(wf(:,-3),wf(:,233),wf(:,274))
  call vert_AV_Q(wf(:,-3),wf(:,218),wf(:,275))
  call vert_AV_Q(wf(:,-3),wf(:,209),wf(:,276))
  call vert_AV_Q(wf(:,-3),wf(:,243),wf(:,277))
  call vert_AV_Q(wf(:,-3),wf(:,208),wf(:,278))
  call vert_AV_Q(wf(:,-3),wf(:,219),wf(:,279))
  call vert_QA_V(wf(:,16),wf(:,-4),wf(:,280))
  call vert_QA_V(wf(:,16),wf(:,-5),wf(:,281))
  call vert_QA_V(wf(:,7),wf(:,-4),wf(:,282))
  call vert_QA_V(wf(:,7),wf(:,-5),wf(:,283))
  call vert_QA_V(wf(:,-1),wf(:,100),wf(:,284))
  call vert_QA_V(wf(:,-2),wf(:,100),wf(:,285))
  call vert_QA_V(wf(:,-1),wf(:,95),wf(:,286))
  call vert_QA_V(wf(:,-2),wf(:,95),wf(:,287))
  call vert_QA_V(wf(:,38),wf(:,-4),wf(:,288))
  call vert_QA_V(wf(:,38),wf(:,-5),wf(:,289))
  call vert_QA_V(wf(:,0),wf(:,115),wf(:,290))
  call vert_QA_V(wf(:,0),wf(:,112),wf(:,291))
  call vert_QA_V(wf(:,34),wf(:,-4),wf(:,292))
  call vert_QA_V(wf(:,34),wf(:,-5),wf(:,293))
  call vert_QA_V(wf(:,-2),wf(:,115),wf(:,294))
  call vert_QA_V(wf(:,-2),wf(:,112),wf(:,295))
  call vert_QA_V(wf(:,58),wf(:,-4),wf(:,296))
  call vert_QA_V(wf(:,58),wf(:,-5),wf(:,297))
  call vert_QA_V(wf(:,0),wf(:,153),wf(:,298))
  call vert_QA_V(wf(:,0),wf(:,150),wf(:,299))
  call vert_QA_V(wf(:,53),wf(:,-4),wf(:,300))
  call vert_QA_V(wf(:,53),wf(:,-5),wf(:,301))
  call vert_QA_V(wf(:,-1),wf(:,153),wf(:,302))
  call vert_QA_V(wf(:,-1),wf(:,150),wf(:,303))
  call vert_QA_V(wf(:,51),wf(:,-3),wf(:,304))
  call vert_QA_V(wf(:,51),wf(:,-5),wf(:,305))
  call vert_QA_V(wf(:,31),wf(:,-3),wf(:,306))
  call vert_QA_V(wf(:,31),wf(:,-5),wf(:,307))
  call vert_QA_V(wf(:,-1),wf(:,120),wf(:,308))
  call vert_QA_V(wf(:,-2),wf(:,120),wf(:,309))
  call vert_QA_V(wf(:,-1),wf(:,110),wf(:,310))
  call vert_QA_V(wf(:,-2),wf(:,110),wf(:,311))
  call vert_QA_V(wf(:,56),wf(:,-3),wf(:,312))
  call vert_QA_V(wf(:,56),wf(:,-5),wf(:,313))
  call vert_QA_V(wf(:,0),wf(:,124),wf(:,314))
  call vert_QA_V(wf(:,0),wf(:,97),wf(:,315))
  call vert_QA_V(wf(:,10),wf(:,-3),wf(:,316))
  call vert_QA_V(wf(:,10),wf(:,-5),wf(:,317))
  call vert_QA_V(wf(:,-2),wf(:,124),wf(:,318))
  call vert_QA_V(wf(:,-2),wf(:,97),wf(:,319))
  call vert_QA_V(wf(:,40),wf(:,-3),wf(:,320))
  call vert_QA_V(wf(:,40),wf(:,-5),wf(:,321))
  call vert_QA_V(wf(:,0),wf(:,161),wf(:,322))
  call vert_QA_V(wf(:,0),wf(:,135),wf(:,323))
  call vert_QA_V(wf(:,18),wf(:,-3),wf(:,324))
  call vert_QA_V(wf(:,18),wf(:,-5),wf(:,325))
  call vert_QA_V(wf(:,-1),wf(:,161),wf(:,326))
  call vert_QA_V(wf(:,-1),wf(:,135),wf(:,327))
  call vert_QA_V(wf(:,61),wf(:,-3),wf(:,328))
  call vert_QA_V(wf(:,61),wf(:,-4),wf(:,329))
  call vert_QA_V(wf(:,43),wf(:,-3),wf(:,330))
  call vert_QA_V(wf(:,43),wf(:,-4),wf(:,331))
  call vert_QA_V(wf(:,-1),wf(:,123),wf(:,332))
  call vert_QA_V(wf(:,-2),wf(:,123),wf(:,333))
  call vert_QA_V(wf(:,-1),wf(:,113),wf(:,334))
  call vert_QA_V(wf(:,-2),wf(:,113),wf(:,335))
  call vert_QA_V(wf(:,63),wf(:,-3),wf(:,336))
  call vert_QA_V(wf(:,63),wf(:,-4),wf(:,337))
  call vert_QA_V(wf(:,0),wf(:,121),wf(:,338))
  call vert_QA_V(wf(:,0),wf(:,101),wf(:,339))
  call vert_QA_V(wf(:,22),wf(:,-3),wf(:,340))
  call vert_QA_V(wf(:,22),wf(:,-4),wf(:,341))
  call vert_QA_V(wf(:,-2),wf(:,121),wf(:,342))
  call vert_QA_V(wf(:,-2),wf(:,101),wf(:,343))
  call vert_QA_V(wf(:,46),wf(:,-3),wf(:,344))
  call vert_QA_V(wf(:,46),wf(:,-4),wf(:,345))
  call vert_QA_V(wf(:,0),wf(:,159),wf(:,346))
  call vert_QA_V(wf(:,0),wf(:,139),wf(:,347))
  call vert_QA_V(wf(:,25),wf(:,-3),wf(:,348))
  call vert_QA_V(wf(:,25),wf(:,-4),wf(:,349))
  call vert_QA_V(wf(:,-1),wf(:,159),wf(:,350))
  call vert_QA_V(wf(:,-1),wf(:,139),wf(:,351))

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
  den(2) = 1 / (Q(5,18))
  den(3) = 1 / (Q(5,36))
  den(6) = 1 / (Q(5,13) - MB2)
  den(9) = 1 / (Q(5,22) - MB2)
  den(12) = 1 / (Q(5,34))
  den(13) = 1 / (Q(5,20))
  den(16) = 1 / (Q(5,11) - MB2)
  den(22) = 1 / (Q(5,38) - MB2)
  den(28) = 1 / (Q(5,17))
  den(29) = 1 / (Q(5,10))
  den(32) = 1 / (Q(5,21) - MB2)
  den(35) = 1 / (Q(5,14) - MB2)
  den(38) = 1 / (Q(5,33))
  den(45) = 1 / (Q(5,37) - MB2)
  den(52) = 1 / (Q(5,12))
  den(55) = 1 / (Q(5,19) - MB2)
  den(66) = 1 / (Q(5,35) - MB2)
  den(85) = 1 / (Q(5,41) - MB2)
  den(88) = 1 / (Q(5,50) - MB2)
  den(91) = 1 / (Q(5,25) - MB2)
  den(96) = 1 / (Q(5,49) - MB2)
  den(99) = 1 / (Q(5,42) - MB2)
  den(104) = 1 / (Q(5,26) - MB2)
  den(116) = 1 / (Q(5,52) - MB2)
  den(123) = 1 / (Q(5,44) - MB2)
  den(127) = 1 / (Q(5,28) - MB2)
  den(149) = 1 / (Q(5,54))
  den(153) = 1 / (Q(5,45))
  den(156) = 1 / (Q(5,27))
  den(171) = 1 / (Q(5,29))
  den(174) = 1 / (Q(5,43))
  den(199) = 1 / (Q(5,46))
  den(203) = 1 / (Q(5,53))
  den(217) = 1 / (Q(5,30))
  den(250) = 1 / (Q(5,51))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(14) = den(1)*den(12)
  den(15) = den(13)*den(14)
  den(17) = den(1)*den(16)
  den(18) = den(13)*den(17)
  den(19) = den(9)*den(13)
  den(20) = den(1)*den(19)
  den(21) = den(7)*den(12)
  den(23) = den(12)*den(22)
  den(24) = den(1)*den(23)
  den(25) = den(3)*den(17)
  den(26) = den(3)*den(22)
  den(27) = den(1)*den(26)
  den(30) = den(28)*den(29)
  den(31) = den(3)*den(30)
  den(33) = den(28)*den(32)
  den(34) = den(29)*den(33)
  den(36) = den(29)*den(35)
  den(37) = den(28)*den(36)
  den(39) = den(29)*den(38)
  den(40) = den(13)*den(39)
  den(41) = den(16)*den(29)
  den(42) = den(13)*den(41)
  den(43) = den(13)*den(32)
  den(44) = den(29)*den(43)
  den(46) = den(38)*den(45)
  den(47) = den(29)*den(46)
  den(48) = den(36)*den(38)
  den(49) = den(3)*den(41)
  den(50) = den(3)*den(45)
  den(51) = den(29)*den(50)
  den(53) = den(12)*den(28)
  den(54) = den(52)*den(53)
  den(56) = den(28)*den(55)
  den(57) = den(52)*den(56)
  den(58) = den(35)*den(52)
  den(59) = den(28)*den(58)
  den(60) = den(2)*den(38)
  den(61) = den(52)*den(60)
  den(62) = den(2)*den(55)
  den(63) = den(52)*den(62)
  den(64) = den(6)*den(52)
  den(65) = den(2)*den(64)
  den(67) = den(38)*den(66)
  den(68) = den(52)*den(67)
  den(69) = den(38)*den(58)
  den(70) = den(12)*den(66)
  den(71) = den(52)*den(70)
  den(72) = den(12)*den(64)
  den(73) = den(12)*den(33)
  den(74) = den(23)*den(28)
  den(75) = den(3)*den(56)
  den(76) = den(26)*den(28)
  den(77) = den(2)*den(46)
  den(78) = den(10)*den(38)
  den(79) = den(3)*den(62)
  den(80) = den(2)*den(50)
  den(81) = den(13)*den(67)
  den(82) = den(19)*den(38)
  den(83) = den(13)*den(70)
  den(84) = den(12)*den(43)
  den(86) = den(1)*den(85)
  den(87) = den(2)*den(86)
  den(89) = den(2)*den(88)
  den(90) = den(1)*den(89)
  den(92) = den(1)*den(91)
  den(93) = den(12)*den(92)
  den(94) = den(12)*den(88)
  den(95) = den(1)*den(94)
  den(97) = den(28)*den(96)
  den(98) = den(29)*den(97)
  den(100) = den(29)*den(99)
  den(101) = den(28)*den(100)
  den(102) = den(38)*den(96)
  den(103) = den(29)*den(102)
  den(105) = den(29)*den(104)
  den(106) = den(38)*den(105)
  den(107) = den(28)*den(91)
  den(108) = den(12)*den(107)
  den(109) = den(12)*den(99)
  den(110) = den(28)*den(109)
  den(111) = den(38)*den(85)
  den(112) = den(2)*den(111)
  den(113) = den(2)*den(104)
  den(114) = den(38)*den(113)
  den(115) = den(13)*den(86)
  den(117) = den(13)*den(116)
  den(118) = den(1)*den(117)
  den(119) = den(3)*den(92)
  den(120) = den(3)*den(116)
  den(121) = den(1)*den(120)
  den(122) = den(52)*den(97)
  den(124) = den(52)*den(123)
  den(125) = den(28)*den(124)
  den(126) = den(52)*den(102)
  den(128) = den(52)*den(127)
  den(129) = den(38)*den(128)
  den(130) = den(3)*den(107)
  den(131) = den(3)*den(123)
  den(132) = den(28)*den(131)
  den(133) = den(13)*den(111)
  den(134) = den(13)*den(127)
  den(135) = den(38)*den(134)
  den(136) = den(29)*den(117)
  den(137) = den(13)*den(100)
  den(138) = den(29)*den(120)
  den(139) = den(3)*den(105)
  den(140) = den(2)*den(124)
  den(141) = den(52)*den(89)
  den(142) = den(12)*den(128)
  den(143) = den(52)*den(94)
  den(144) = den(2)*den(131)
  den(145) = den(3)*den(113)
  den(146) = den(12)*den(134)
  den(147) = den(13)*den(109)
  den(148) = den(2)*den(3)
  den(150) = den(148)*den(149)
  den(151) = den(1)*den(150)
  den(152) = den(1)*den(3)
  den(154) = den(152)*den(153)
  den(155) = den(2)*den(154)
  den(157) = den(4)*den(156)
  den(158) = den(3)*den(157)
  den(159) = den(1)**2
  den(160) = den(89)*den(159)
  den(161) = den(10)*den(159)
  den(162) = den(2)**2
  den(163) = den(86)*den(162)
  den(164) = den(7)*den(162)
  den(165) = den(7)*den(89)
  den(166) = den(10)*den(86)
  den(167) = den(12)*den(13)
  den(168) = den(149)*den(167)
  den(169) = den(1)*den(168)
  den(170) = den(1)*den(13)
  den(172) = den(170)*den(171)
  den(173) = den(12)*den(172)
  den(175) = den(14)*den(174)
  den(176) = den(13)*den(175)
  den(177) = den(117)*den(159)
  den(178) = den(19)*den(159)
  den(179) = den(13)**2
  den(180) = den(86)*den(179)
  den(181) = den(17)*den(117)
  den(182) = den(19)*den(86)
  den(183) = den(17)*den(179)
  den(184) = den(94)*den(159)
  den(185) = den(23)*den(159)
  den(186) = den(12)**2
  den(187) = den(92)*den(186)
  den(188) = den(7)*den(186)
  den(189) = den(7)*den(94)
  den(190) = den(23)*den(92)
  den(191) = den(120)*den(159)
  den(192) = den(26)*den(159)
  den(193) = den(3)**2
  den(194) = den(92)*den(193)
  den(195) = den(17)*den(120)
  den(196) = den(26)*den(92)
  den(197) = den(17)*den(193)
  den(198) = den(3)*den(29)
  den(200) = den(198)*den(199)
  den(201) = den(28)*den(200)
  den(202) = den(3)*den(28)
  den(204) = den(202)*den(203)
  den(205) = den(29)*den(204)
  den(206) = den(30)*den(156)
  den(207) = den(3)*den(206)
  den(208) = den(28)**2
  den(209) = den(100)*den(208)
  den(210) = den(36)*den(208)
  den(211) = den(29)**2
  den(212) = den(97)*den(211)
  den(213) = den(33)*den(211)
  den(214) = den(33)*den(100)
  den(215) = den(36)*den(97)
  den(216) = den(13)*den(29)
  den(218) = den(216)*den(217)
  den(219) = den(38)*den(218)
  den(220) = den(13)*den(38)
  den(221) = den(203)*den(220)
  den(222) = den(29)*den(221)
  den(223) = den(39)*den(174)
  den(224) = den(13)*den(223)
  den(225) = den(117)*den(211)
  den(226) = den(100)*den(179)
  den(227) = den(41)*den(117)
  den(228) = den(43)*den(100)
  den(229) = den(43)*den(211)
  den(230) = den(41)*den(179)
  den(231) = den(38)**2
  den(232) = den(105)*den(231)
  den(233) = den(36)*den(231)
  den(234) = den(102)*den(211)
  den(235) = den(46)*den(211)
  den(236) = den(46)*den(105)
  den(237) = den(36)*den(102)
  den(238) = den(120)*den(211)
  den(239) = den(105)*den(193)
  den(240) = den(41)*den(120)
  den(241) = den(50)*den(105)
  den(242) = den(50)*den(211)
  den(243) = den(41)*den(193)
  den(244) = den(12)*den(52)
  den(245) = den(199)*den(244)
  den(246) = den(28)*den(245)
  den(247) = den(28)*den(52)
  den(248) = den(171)*den(247)
  den(249) = den(12)*den(248)
  den(251) = den(53)*den(250)
  den(252) = den(52)*den(251)
  den(253) = den(124)*den(208)
  den(254) = den(58)*den(208)
  den(255) = den(52)**2
  den(256) = den(97)*den(255)
  den(257) = den(56)*den(124)
  den(258) = den(58)*den(97)
  den(259) = den(56)*den(255)
  den(260) = den(2)*den(52)
  den(261) = den(217)*den(260)
  den(262) = den(38)*den(261)
  den(263) = den(38)*den(52)
  den(264) = den(153)*den(263)
  den(265) = den(2)*den(264)
  den(266) = den(60)*den(250)
  den(267) = den(52)*den(266)
  den(268) = den(124)*den(162)
  den(269) = den(89)*den(255)
  den(270) = den(62)*den(124)
  den(271) = den(64)*den(89)
  den(272) = den(64)*den(162)
  den(273) = den(62)*den(255)
  den(274) = den(128)*den(231)
  den(275) = den(58)*den(231)
  den(276) = den(102)*den(255)
  den(277) = den(67)*den(128)
  den(278) = den(58)*den(102)
  den(279) = den(67)*den(255)
  den(280) = den(128)*den(186)
  den(281) = den(94)*den(255)
  den(282) = den(70)*den(128)
  den(283) = den(64)*den(94)
  den(284) = den(64)*den(186)
  den(285) = den(70)*den(255)
  den(286) = den(109)*den(208)
  den(287) = den(23)*den(208)
  den(288) = den(107)*den(186)
  den(289) = den(33)*den(186)
  den(290) = den(33)*den(109)
  den(291) = den(23)*den(107)
  den(292) = den(131)*den(208)
  den(293) = den(26)*den(208)
  den(294) = den(107)*den(193)
  den(295) = den(56)*den(131)
  den(296) = den(26)*den(107)
  den(297) = den(56)*den(193)
  den(298) = den(113)*den(231)
  den(299) = den(10)*den(231)
  den(300) = den(111)*den(162)
  den(301) = den(46)*den(162)
  den(302) = den(46)*den(113)
  den(303) = den(10)*den(111)
  den(304) = den(131)*den(162)
  den(305) = den(113)*den(193)
  den(306) = den(62)*den(131)
  den(307) = den(50)*den(113)
  den(308) = den(50)*den(162)
  den(309) = den(62)*den(193)
  den(310) = den(134)*den(231)
  den(311) = den(19)*den(231)
  den(312) = den(111)*den(179)
  den(313) = den(67)*den(134)
  den(314) = den(19)*den(111)
  den(315) = den(67)*den(179)
  den(316) = den(134)*den(186)
  den(317) = den(109)*den(179)
  den(318) = den(70)*den(134)
  den(319) = den(43)*den(109)
  den(320) = den(43)*den(186)
  den(321) = den(70)*den(179)
  den(322) = den(17)*den(156)
  den(323) = den(17)*den(174)
  den(324) = den(7)*den(171)
  den(325) = den(7)*den(153)
  den(326) = den(92)*den(156)
  den(327) = den(92)*den(171)
  den(328) = den(86)*den(174)
  den(329) = den(86)*den(153)
  den(330) = den(41)*den(156)
  den(331) = den(41)*den(174)
  den(332) = den(105)*den(156)
  den(333) = den(100)*den(174)
  den(334) = den(36)*den(217)
  den(335) = den(36)*den(199)
  den(336) = den(105)*den(217)
  den(337) = den(100)*den(199)
  den(338) = den(64)*den(171)
  den(339) = den(64)*den(153)
  den(340) = den(128)*den(171)
  den(341) = den(124)*den(153)
  den(342) = den(58)*den(217)
  den(343) = den(58)*den(199)
  den(344) = den(128)*den(217)
  den(345) = den(124)*den(199)
  den(346) = den(56)*den(156)
  den(347) = den(56)*den(250)
  den(348) = den(33)*den(171)
  den(349) = den(33)*den(203)
  den(350) = den(107)*den(156)
  den(351) = den(107)*den(171)
  den(352) = den(97)*den(250)
  den(353) = den(97)*den(203)
  den(354) = den(62)*den(156)
  den(355) = den(62)*den(250)
  den(356) = den(113)*den(156)
  den(357) = den(89)*den(250)
  den(358) = den(10)*den(217)
  den(359) = den(10)*den(149)
  den(360) = den(113)*den(217)
  den(361) = den(89)*den(149)
  den(362) = den(43)*den(171)
  den(363) = den(43)*den(203)
  den(364) = den(134)*den(171)
  den(365) = den(117)*den(203)
  den(366) = den(19)*den(217)
  den(367) = den(19)*den(149)
  den(368) = den(134)*den(217)
  den(369) = den(117)*den(149)
  den(370) = den(67)*den(174)
  den(371) = den(67)*den(250)
  den(372) = den(46)*den(153)
  den(373) = den(46)*den(203)
  den(374) = den(111)*den(174)
  den(375) = den(111)*den(153)
  den(376) = den(102)*den(250)
  den(377) = den(102)*den(203)
  den(378) = den(70)*den(174)
  den(379) = den(70)*den(250)
  den(380) = den(109)*den(174)
  den(381) = den(94)*den(250)
  den(382) = den(23)*den(199)
  den(383) = den(23)*den(149)
  den(384) = den(109)*den(199)
  den(385) = den(94)*den(149)
  den(386) = den(50)*den(153)
  den(387) = den(50)*den(203)
  den(388) = den(131)*den(153)
  den(389) = den(120)*den(203)
  den(390) = den(26)*den(199)
  den(391) = den(26)*den(149)
  den(392) = den(131)*den(199)
  den(393) = den(120)*den(149)
  den(394) = den(1)*den(2)*den(3)
  den(395) = den(1)*den(12)*den(13)
  den(396) = den(3)*den(28)*den(29)
  den(397) = den(13)*den(29)*den(38)
  den(398) = den(12)*den(28)*den(52)
  den(399) = den(2)*den(38)*den(52)
  den(400) = den(1)*den(148)
  den(401) = den(2)*den(152)
  den(402) = den(1)*den(167)
  den(403) = den(12)*den(170)
  den(404) = den(28)*den(198)
  den(405) = den(29)*den(202)
  den(406) = den(38)*den(216)
  den(407) = den(29)*den(220)
  den(408) = den(28)*den(244)
  den(409) = den(12)*den(247)
  den(410) = den(38)*den(260)
  den(411) = den(2)*den(263)
  den(412) = den(2)*den(325)
  den(413) = den(2)*den(329)
  den(414) = den(1)*den(359)
  den(415) = den(1)*den(361)
  den(416) = den(13)*den(323)
  den(417) = den(13)*den(328)
  den(418) = den(1)*den(367)
  den(419) = den(1)*den(369)
  den(420) = den(12)*den(324)
  den(421) = den(12)*den(327)
  den(422) = den(1)*den(383)
  den(423) = den(1)*den(385)
  den(424) = den(3)*den(322)
  den(425) = den(3)*den(326)
  den(426) = den(1)*den(391)
  den(427) = den(1)*den(393)
  den(428) = den(29)*den(349)
  den(429) = den(29)*den(353)
  den(430) = den(28)*den(335)
  den(431) = den(28)*den(337)
  den(432) = den(13)*den(331)
  den(433) = den(29)*den(363)
  den(434) = den(13)*den(333)
  den(435) = den(29)*den(365)
  den(436) = den(29)*den(373)
  den(437) = den(29)*den(377)
  den(438) = den(38)*den(334)
  den(439) = den(38)*den(336)
  den(440) = den(3)*den(330)
  den(441) = den(29)*den(387)
  den(442) = den(3)*den(332)
  den(443) = den(29)*den(389)
  den(444) = den(52)*den(347)
  den(445) = den(52)*den(352)
  den(446) = den(28)*den(343)
  den(447) = den(28)*den(345)
  den(448) = den(52)*den(355)
  den(449) = den(2)*den(339)
  den(450) = den(52)*den(357)
  den(451) = den(2)*den(341)
  den(452) = den(52)*den(371)
  den(453) = den(52)*den(376)
  den(454) = den(38)*den(342)
  den(455) = den(38)*den(344)
  den(456) = den(52)*den(379)
  den(457) = den(12)*den(338)
  den(458) = den(52)*den(381)
  den(459) = den(12)*den(340)
  den(460) = den(12)*den(348)
  den(461) = den(12)*den(351)
  den(462) = den(28)*den(382)
  den(463) = den(28)*den(384)
  den(464) = den(3)*den(346)
  den(465) = den(3)*den(350)
  den(466) = den(28)*den(390)
  den(467) = den(28)*den(392)
  den(468) = den(2)*den(372)
  den(469) = den(2)*den(375)
  den(470) = den(38)*den(358)
  den(471) = den(38)*den(360)
  den(472) = den(3)*den(354)
  den(473) = den(2)*den(386)
  den(474) = den(3)*den(356)
  den(475) = den(2)*den(388)
  den(476) = den(13)*den(370)
  den(477) = den(13)*den(374)
  den(478) = den(38)*den(366)
  den(479) = den(38)*den(368)
  den(480) = den(13)*den(378)
  den(481) = den(12)*den(362)
  den(482) = den(13)*den(380)
  den(483) = den(12)*den(364)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(336)

  A(1) = cont_VV(wf(:,3),wf(:,4)) * den(5)
  A(2) = cont_QA(wf(:,6),wf(:,7)) * den(8)
  A(3) = cont_QA(wf(:,9),wf(:,10)) * den(11)
  A(4) = cont_VV(wf(:,12),wf(:,13)) * den(15)
  A(5) = cont_QA(wf(:,15),wf(:,16)) * den(18)
  A(6) = cont_QA(wf(:,9),wf(:,18)) * den(20)
  A(7) = cont_QA(wf(:,7),wf(:,19)) * den(21)
  A(8) = cont_QA(wf(:,21),wf(:,22)) * den(24)
  A(9) = cont_QA(wf(:,16),wf(:,23)) * den(25)
  A(10) = cont_QA(wf(:,21),wf(:,25)) * den(27)
  A(11) = cont_VV(wf(:,3),wf(:,28)) * den(31)
  A(12) = cont_QA(wf(:,30),wf(:,31)) * den(34)
  A(13) = cont_QA(wf(:,33),wf(:,34)) * den(37)
  A(14) = cont_VV(wf(:,12),wf(:,36)) * den(40)
  A(15) = cont_QA(wf(:,15),wf(:,38)) * den(42)
  A(16) = cont_QA(wf(:,30),wf(:,40)) * den(44)
  A(17) = cont_QA(wf(:,42),wf(:,43)) * den(47)
  A(18) = cont_QA(wf(:,34),wf(:,44)) * den(48)
  A(19) = cont_QA(wf(:,23),wf(:,38)) * den(49)
  A(20) = cont_QA(wf(:,42),wf(:,46)) * den(51)
  A(21) = cont_VV(wf(:,47),wf(:,48)) * den(54)
  A(22) = cont_QA(wf(:,50),wf(:,51)) * den(57)
  A(23) = cont_QA(wf(:,33),wf(:,53)) * den(59)
  A(24) = cont_VV(wf(:,47),wf(:,54)) * den(61)
  A(25) = cont_QA(wf(:,50),wf(:,56)) * den(63)
  A(26) = cont_QA(wf(:,6),wf(:,58)) * den(65)
  A(27) = cont_QA(wf(:,60),wf(:,61)) * den(68)
  A(28) = cont_QA(wf(:,44),wf(:,53)) * den(69)
  A(29) = cont_QA(wf(:,60),wf(:,63)) * den(71)
  A(30) = cont_QA(wf(:,19),wf(:,58)) * den(72)
  A(31) = cont_QA(wf(:,31),wf(:,64)) * den(73)
  A(32) = cont_QA(wf(:,22),wf(:,65)) * den(74)
  A(33) = cont_QA(wf(:,51),wf(:,66)) * den(75)
  A(34) = cont_QA(wf(:,25),wf(:,65)) * den(76)
  A(35) = cont_QA(wf(:,43),wf(:,67)) * den(77)
  A(36) = cont_QA(wf(:,10),wf(:,68)) * den(78)
  A(37) = cont_QA(wf(:,56),wf(:,66)) * den(79)
  A(38) = cont_QA(wf(:,46),wf(:,67)) * den(80)
  A(39) = cont_QA(wf(:,61),wf(:,69)) * den(81)
  A(40) = cont_QA(wf(:,18),wf(:,68)) * den(82)
  A(41) = cont_QA(wf(:,63),wf(:,69)) * den(83)
  A(42) = cont_QA(wf(:,40),wf(:,64)) * den(84)

  A(43) = cont_VV(wf(:,3),wf(:,70)) * den(5)
  A(44) = cont_QA(wf(:,7),wf(:,71)) * den(8)
  A(45) = cont_QA(wf(:,10),wf(:,72)) * den(11)
  A(46) = cont_VV(wf(:,12),wf(:,73)) * den(15)
  A(47) = cont_QA(wf(:,16),wf(:,74)) * den(18)
  A(48) = cont_QA(wf(:,18),wf(:,72)) * den(20)
  A(49) = cont_QA(wf(:,7),wf(:,75)) * den(21)
  A(50) = cont_QA(wf(:,22),wf(:,76)) * den(24)
  A(51) = cont_QA(wf(:,16),wf(:,77)) * den(25)
  A(52) = cont_QA(wf(:,25),wf(:,76)) * den(27)
  A(53) = cont_VV(wf(:,3),wf(:,78)) * den(31)
  A(54) = cont_QA(wf(:,31),wf(:,79)) * den(34)
  A(55) = cont_QA(wf(:,34),wf(:,80)) * den(37)
  A(56) = cont_VV(wf(:,12),wf(:,81)) * den(40)
  A(57) = cont_QA(wf(:,38),wf(:,74)) * den(42)
  A(58) = cont_QA(wf(:,40),wf(:,79)) * den(44)
  A(59) = cont_QA(wf(:,43),wf(:,82)) * den(47)
  A(60) = cont_QA(wf(:,34),wf(:,83)) * den(48)
  A(61) = cont_QA(wf(:,38),wf(:,77)) * den(49)
  A(62) = cont_QA(wf(:,46),wf(:,82)) * den(51)
  A(63) = cont_VV(wf(:,47),wf(:,84)) * den(54)
  A(64) = cont_QA(wf(:,51),wf(:,85)) * den(57)
  A(65) = cont_QA(wf(:,53),wf(:,80)) * den(59)
  A(66) = cont_VV(wf(:,47),wf(:,86)) * den(61)
  A(67) = cont_QA(wf(:,56),wf(:,85)) * den(63)
  A(68) = cont_QA(wf(:,58),wf(:,71)) * den(65)
  A(69) = cont_QA(wf(:,61),wf(:,87)) * den(68)
  A(70) = cont_QA(wf(:,53),wf(:,83)) * den(69)
  A(71) = cont_QA(wf(:,63),wf(:,87)) * den(71)
  A(72) = cont_QA(wf(:,58),wf(:,75)) * den(72)
  A(73) = cont_QA(wf(:,31),wf(:,88)) * den(73)
  A(74) = cont_QA(wf(:,22),wf(:,89)) * den(74)
  A(75) = cont_QA(wf(:,51),wf(:,90)) * den(75)
  A(76) = cont_QA(wf(:,25),wf(:,89)) * den(76)
  A(77) = cont_QA(wf(:,43),wf(:,91)) * den(77)
  A(78) = cont_QA(wf(:,10),wf(:,92)) * den(78)
  A(79) = cont_QA(wf(:,56),wf(:,90)) * den(79)
  A(80) = cont_QA(wf(:,46),wf(:,91)) * den(80)
  A(81) = cont_QA(wf(:,61),wf(:,93)) * den(81)
  A(82) = cont_QA(wf(:,18),wf(:,92)) * den(82)
  A(83) = cont_QA(wf(:,63),wf(:,93)) * den(83)
  A(84) = cont_QA(wf(:,40),wf(:,88)) * den(84)
  A(85) = cont_QA(wf(:,94),wf(:,95)) * den(87)
  A(86) = cont_QA(wf(:,96),wf(:,97)) * den(90)
  A(87) = cont_VV(wf(:,4),wf(:,98)) * den(5)
  A(88) = cont_QA(wf(:,99),wf(:,100)) * den(93)
  A(89) = cont_QA(wf(:,96),wf(:,101)) * den(95)
  A(90) = cont_QA(wf(:,16),wf(:,102)) * den(25)
  A(91) = cont_QA(wf(:,21),wf(:,104)) * den(27)
  A(92) = cont_VV(wf(:,13),wf(:,105)) * den(15)
  A(93) = cont_QA(wf(:,16),wf(:,106)) * den(18)
  A(94) = cont_QA(wf(:,9),wf(:,108)) * den(20)
  A(95) = cont_QA(wf(:,109),wf(:,110)) * den(98)
  A(96) = cont_QA(wf(:,111),wf(:,112)) * den(101)
  A(97) = cont_VV(wf(:,28),wf(:,98)) * den(31)
  A(98) = cont_QA(wf(:,109),wf(:,113)) * den(103)
  A(99) = cont_QA(wf(:,114),wf(:,115)) * den(106)
  A(100) = cont_QA(wf(:,38),wf(:,102)) * den(49)
  A(101) = cont_QA(wf(:,42),wf(:,117)) * den(51)
  A(102) = cont_VV(wf(:,36),wf(:,105)) * den(40)
  A(103) = cont_QA(wf(:,38),wf(:,106)) * den(42)
  A(104) = cont_QA(wf(:,30),wf(:,119)) * den(44)
  A(105) = cont_QA(wf(:,99),wf(:,120)) * den(108)
  A(106) = cont_QA(wf(:,111),wf(:,121)) * den(110)
  A(107) = cont_QA(wf(:,51),wf(:,122)) * den(75)
  A(108) = cont_QA(wf(:,65),wf(:,104)) * den(76)
  A(109) = cont_QA(wf(:,94),wf(:,123)) * den(112)
  A(110) = cont_QA(wf(:,114),wf(:,124)) * den(114)
  A(111) = cont_QA(wf(:,56),wf(:,122)) * den(79)
  A(112) = cont_QA(wf(:,67),wf(:,117)) * den(80)
  A(113) = cont_QA(wf(:,61),wf(:,125)) * den(81)
  A(114) = cont_QA(wf(:,68),wf(:,108)) * den(82)
  A(115) = cont_QA(wf(:,63),wf(:,125)) * den(83)
  A(116) = cont_QA(wf(:,64),wf(:,119)) * den(84)
  A(117) = cont_VV(wf(:,48),wf(:,126)) * den(54)
  A(118) = cont_QA(wf(:,51),wf(:,127)) * den(57)
  A(119) = cont_QA(wf(:,33),wf(:,129)) * den(59)
  A(120) = cont_VV(wf(:,54),wf(:,126)) * den(61)
  A(121) = cont_QA(wf(:,56),wf(:,127)) * den(63)
  A(122) = cont_QA(wf(:,6),wf(:,131)) * den(65)
  A(123) = cont_QA(wf(:,61),wf(:,132)) * den(68)
  A(124) = cont_QA(wf(:,44),wf(:,129)) * den(69)
  A(125) = cont_QA(wf(:,63),wf(:,132)) * den(71)
  A(126) = cont_QA(wf(:,19),wf(:,131)) * den(72)
  A(127) = cont_QA(wf(:,95),wf(:,133)) * den(115)
  A(128) = cont_QA(wf(:,134),wf(:,135)) * den(118)
  A(129) = cont_VV(wf(:,12),wf(:,137)) * den(15)
  A(130) = cont_QA(wf(:,100),wf(:,138)) * den(119)
  A(131) = cont_QA(wf(:,134),wf(:,139)) * den(121)
  A(132) = cont_QA(wf(:,7),wf(:,140)) * den(21)
  A(133) = cont_QA(wf(:,21),wf(:,142)) * den(24)
  A(134) = cont_VV(wf(:,3),wf(:,144)) * den(5)
  A(135) = cont_QA(wf(:,7),wf(:,145)) * den(8)
  A(136) = cont_QA(wf(:,9),wf(:,147)) * den(11)
  A(137) = cont_QA(wf(:,110),wf(:,148)) * den(122)
  A(138) = cont_QA(wf(:,149),wf(:,150)) * den(125)
  A(139) = cont_VV(wf(:,47),wf(:,151)) * den(54)
  A(140) = cont_QA(wf(:,113),wf(:,148)) * den(126)
  A(141) = cont_QA(wf(:,152),wf(:,153)) * den(129)
  A(142) = cont_QA(wf(:,58),wf(:,140)) * den(72)
  A(143) = cont_QA(wf(:,60),wf(:,155)) * den(71)
  A(144) = cont_VV(wf(:,47),wf(:,156)) * den(61)
  A(145) = cont_QA(wf(:,58),wf(:,145)) * den(65)
  A(146) = cont_QA(wf(:,50),wf(:,158)) * den(63)
  A(147) = cont_QA(wf(:,120),wf(:,138)) * den(130)
  A(148) = cont_QA(wf(:,149),wf(:,159)) * den(132)
  A(149) = cont_QA(wf(:,31),wf(:,160)) * den(73)
  A(150) = cont_QA(wf(:,65),wf(:,142)) * den(74)
  A(151) = cont_QA(wf(:,123),wf(:,133)) * den(133)
  A(152) = cont_QA(wf(:,152),wf(:,161)) * den(135)
  A(153) = cont_QA(wf(:,40),wf(:,160)) * den(84)
  A(154) = cont_QA(wf(:,69),wf(:,155)) * den(83)
  A(155) = cont_QA(wf(:,43),wf(:,162)) * den(77)
  A(156) = cont_QA(wf(:,68),wf(:,147)) * den(78)
  A(157) = cont_QA(wf(:,46),wf(:,162)) * den(80)
  A(158) = cont_QA(wf(:,66),wf(:,158)) * den(79)
  A(159) = cont_VV(wf(:,3),wf(:,164)) * den(31)
  A(160) = cont_QA(wf(:,31),wf(:,165)) * den(34)
  A(161) = cont_QA(wf(:,33),wf(:,167)) * den(37)
  A(162) = cont_VV(wf(:,12),wf(:,168)) * den(40)
  A(163) = cont_QA(wf(:,40),wf(:,165)) * den(44)
  A(164) = cont_QA(wf(:,15),wf(:,170)) * den(42)
  A(165) = cont_QA(wf(:,43),wf(:,171)) * den(47)
  A(166) = cont_QA(wf(:,44),wf(:,167)) * den(48)
  A(167) = cont_QA(wf(:,46),wf(:,171)) * den(51)
  A(168) = cont_QA(wf(:,23),wf(:,170)) * den(49)
  A(169) = cont_QA(wf(:,135),wf(:,172)) * den(136)
  A(170) = cont_QA(wf(:,112),wf(:,173)) * den(137)
  A(171) = cont_VV(wf(:,12),wf(:,175)) * den(40)
  A(172) = cont_QA(wf(:,139),wf(:,172)) * den(138)
  A(173) = cont_QA(wf(:,115),wf(:,176)) * den(139)
  A(174) = cont_QA(wf(:,42),wf(:,178)) * den(47)
  A(175) = cont_QA(wf(:,34),wf(:,179)) * den(48)
  A(176) = cont_VV(wf(:,3),wf(:,181)) * den(31)
  A(177) = cont_QA(wf(:,30),wf(:,183)) * den(34)
  A(178) = cont_QA(wf(:,34),wf(:,184)) * den(37)
  A(179) = cont_QA(wf(:,150),wf(:,185)) * den(140)
  A(180) = cont_QA(wf(:,97),wf(:,186)) * den(141)
  A(181) = cont_VV(wf(:,47),wf(:,187)) * den(61)
  A(182) = cont_QA(wf(:,153),wf(:,188)) * den(142)
  A(183) = cont_QA(wf(:,101),wf(:,186)) * den(143)
  A(184) = cont_QA(wf(:,60),wf(:,190)) * den(68)
  A(185) = cont_QA(wf(:,53),wf(:,179)) * den(69)
  A(186) = cont_VV(wf(:,47),wf(:,191)) * den(54)
  A(187) = cont_QA(wf(:,50),wf(:,193)) * den(57)
  A(188) = cont_QA(wf(:,53),wf(:,184)) * den(59)
  A(189) = cont_QA(wf(:,159),wf(:,185)) * den(144)
  A(190) = cont_QA(wf(:,124),wf(:,176)) * den(145)
  A(191) = cont_QA(wf(:,67),wf(:,178)) * den(77)
  A(192) = cont_QA(wf(:,10),wf(:,194)) * den(78)
  A(193) = cont_QA(wf(:,161),wf(:,188)) * den(146)
  A(194) = cont_QA(wf(:,121),wf(:,173)) * den(147)
  A(195) = cont_QA(wf(:,69),wf(:,190)) * den(81)
  A(196) = cont_QA(wf(:,18),wf(:,194)) * den(82)
  A(197) = cont_QA(wf(:,64),wf(:,183)) * den(73)
  A(198) = cont_QA(wf(:,22),wf(:,195)) * den(74)
  A(199) = cont_QA(wf(:,66),wf(:,193)) * den(75)
  A(200) = cont_QA(wf(:,25),wf(:,195)) * den(76)
  A(201) = cont_VV(wf(:,3),wf(:,197)) * den(5)
  A(202) = cont_QA(wf(:,6),wf(:,199)) * den(8)
  A(203) = cont_QA(wf(:,10),wf(:,200)) * den(11)
  A(204) = cont_VV(wf(:,12),wf(:,201)) * den(15)
  A(205) = cont_QA(wf(:,15),wf(:,203)) * den(18)
  A(206) = cont_QA(wf(:,18),wf(:,200)) * den(20)
  A(207) = cont_QA(wf(:,19),wf(:,199)) * den(21)
  A(208) = cont_QA(wf(:,22),wf(:,204)) * den(24)
  A(209) = cont_QA(wf(:,23),wf(:,203)) * den(25)
  A(210) = cont_QA(wf(:,25),wf(:,204)) * den(27)
  A(211) = cont_VV(wf(:,205),wf(:,206)) * den(151)
  A(212) = cont_VV(wf(:,207),wf(:,208)) * den(155)
  A(213) = cont_VV(wf(:,4),wf(:,209)) * den(158)
  A(214) = cont_QA(wf(:,97),wf(:,210)) * den(160)
  A(215) = cont_QA(wf(:,10),wf(:,211)) * den(161)
  A(216) = cont_QA(wf(:,95),wf(:,212)) * den(163)
  A(217) = cont_QA(wf(:,7),wf(:,213)) * den(164)
  A(218) = cont_QA(wf(:,97),wf(:,214)) * den(165)
  A(219) = cont_QA(wf(:,95),wf(:,215)) * den(166)
  A(220) = cont_VV(wf(:,206),wf(:,216)) * den(169)
  A(221) = cont_VV(wf(:,217),wf(:,218)) * den(173)
  A(222) = cont_VV(wf(:,13),wf(:,219)) * den(176)
  A(223) = cont_QA(wf(:,135),wf(:,220)) * den(177)
  A(224) = cont_QA(wf(:,18),wf(:,211)) * den(178)
  A(225) = cont_QA(wf(:,95),wf(:,221)) * den(180)
  A(226) = cont_QA(wf(:,135),wf(:,222)) * den(181)
  A(227) = cont_QA(wf(:,95),wf(:,223)) * den(182)
  A(228) = cont_QA(wf(:,16),wf(:,224)) * den(183)
  A(229) = cont_QA(wf(:,101),wf(:,210)) * den(184)
  A(230) = cont_QA(wf(:,22),wf(:,225)) * den(185)
  A(231) = cont_QA(wf(:,100),wf(:,226)) * den(187)
  A(232) = cont_QA(wf(:,7),wf(:,227)) * den(188)
  A(233) = cont_QA(wf(:,101),wf(:,214)) * den(189)
  A(234) = cont_QA(wf(:,100),wf(:,228)) * den(190)
  A(235) = cont_QA(wf(:,139),wf(:,220)) * den(191)
  A(236) = cont_QA(wf(:,25),wf(:,225)) * den(192)
  A(237) = cont_QA(wf(:,100),wf(:,229)) * den(194)
  A(238) = cont_QA(wf(:,139),wf(:,222)) * den(195)
  A(239) = cont_QA(wf(:,100),wf(:,230)) * den(196)
  A(240) = cont_QA(wf(:,16),wf(:,231)) * den(197)
  A(241) = cont_VV(wf(:,232),wf(:,233)) * den(201)
  A(242) = cont_VV(wf(:,234),wf(:,235)) * den(205)
  A(243) = cont_VV(wf(:,28),wf(:,209)) * den(207)
  A(244) = cont_QA(wf(:,112),wf(:,236)) * den(209)
  A(245) = cont_QA(wf(:,34),wf(:,237)) * den(210)
  A(246) = cont_QA(wf(:,110),wf(:,238)) * den(212)
  A(247) = cont_QA(wf(:,31),wf(:,239)) * den(213)
  A(248) = cont_QA(wf(:,112),wf(:,240)) * den(214)
  A(249) = cont_QA(wf(:,110),wf(:,241)) * den(215)
  A(250) = cont_VV(wf(:,242),wf(:,243)) * den(219)
  A(251) = cont_VV(wf(:,235),wf(:,244)) * den(222)
  A(252) = cont_VV(wf(:,36),wf(:,219)) * den(224)
  A(253) = cont_QA(wf(:,135),wf(:,245)) * den(225)
  A(254) = cont_QA(wf(:,112),wf(:,246)) * den(226)
  A(255) = cont_QA(wf(:,135),wf(:,247)) * den(227)
  A(256) = cont_QA(wf(:,112),wf(:,248)) * den(228)
  A(257) = cont_QA(wf(:,40),wf(:,239)) * den(229)
  A(258) = cont_QA(wf(:,38),wf(:,224)) * den(230)
  A(259) = cont_QA(wf(:,115),wf(:,249)) * den(232)
  A(260) = cont_QA(wf(:,34),wf(:,250)) * den(233)
  A(261) = cont_QA(wf(:,113),wf(:,238)) * den(234)
  A(262) = cont_QA(wf(:,43),wf(:,251)) * den(235)
  A(263) = cont_QA(wf(:,115),wf(:,252)) * den(236)
  A(264) = cont_QA(wf(:,113),wf(:,241)) * den(237)
  A(265) = cont_QA(wf(:,139),wf(:,245)) * den(238)
  A(266) = cont_QA(wf(:,115),wf(:,253)) * den(239)
  A(267) = cont_QA(wf(:,139),wf(:,247)) * den(240)
  A(268) = cont_QA(wf(:,115),wf(:,254)) * den(241)
  A(269) = cont_QA(wf(:,46),wf(:,251)) * den(242)
  A(270) = cont_QA(wf(:,38),wf(:,231)) * den(243)
  A(271) = cont_VV(wf(:,233),wf(:,255)) * den(246)
  A(272) = cont_VV(wf(:,218),wf(:,256)) * den(249)
  A(273) = cont_VV(wf(:,48),wf(:,257)) * den(252)
  A(274) = cont_QA(wf(:,150),wf(:,258)) * den(253)
  A(275) = cont_QA(wf(:,53),wf(:,237)) * den(254)
  A(276) = cont_QA(wf(:,110),wf(:,259)) * den(256)
  A(277) = cont_QA(wf(:,150),wf(:,260)) * den(257)
  A(278) = cont_QA(wf(:,110),wf(:,261)) * den(258)
  A(279) = cont_QA(wf(:,51),wf(:,262)) * den(259)
  A(280) = cont_VV(wf(:,243),wf(:,263)) * den(262)
  A(281) = cont_VV(wf(:,208),wf(:,264)) * den(265)
  A(282) = cont_VV(wf(:,54),wf(:,257)) * den(267)
  A(283) = cont_QA(wf(:,150),wf(:,265)) * den(268)
  A(284) = cont_QA(wf(:,97),wf(:,266)) * den(269)
  A(285) = cont_QA(wf(:,150),wf(:,267)) * den(270)
  A(286) = cont_QA(wf(:,97),wf(:,268)) * den(271)
  A(287) = cont_QA(wf(:,58),wf(:,213)) * den(272)
  A(288) = cont_QA(wf(:,56),wf(:,262)) * den(273)
  A(289) = cont_QA(wf(:,153),wf(:,269)) * den(274)
  A(290) = cont_QA(wf(:,53),wf(:,250)) * den(275)
  A(291) = cont_QA(wf(:,113),wf(:,259)) * den(276)
  A(292) = cont_QA(wf(:,153),wf(:,270)) * den(277)
  A(293) = cont_QA(wf(:,113),wf(:,261)) * den(278)
  A(294) = cont_QA(wf(:,61),wf(:,271)) * den(279)
  A(295) = cont_QA(wf(:,153),wf(:,272)) * den(280)
  A(296) = cont_QA(wf(:,101),wf(:,266)) * den(281)
  A(297) = cont_QA(wf(:,153),wf(:,273)) * den(282)
  A(298) = cont_QA(wf(:,101),wf(:,268)) * den(283)
  A(299) = cont_QA(wf(:,58),wf(:,227)) * den(284)
  A(300) = cont_QA(wf(:,63),wf(:,271)) * den(285)
  A(301) = cont_QA(wf(:,121),wf(:,236)) * den(286)
  A(302) = cont_QA(wf(:,22),wf(:,274)) * den(287)
  A(303) = cont_QA(wf(:,120),wf(:,226)) * den(288)
  A(304) = cont_QA(wf(:,31),wf(:,275)) * den(289)
  A(305) = cont_QA(wf(:,121),wf(:,240)) * den(290)
  A(306) = cont_QA(wf(:,120),wf(:,228)) * den(291)
  A(307) = cont_QA(wf(:,159),wf(:,258)) * den(292)
  A(308) = cont_QA(wf(:,25),wf(:,274)) * den(293)
  A(309) = cont_QA(wf(:,120),wf(:,229)) * den(294)
  A(310) = cont_QA(wf(:,159),wf(:,260)) * den(295)
  A(311) = cont_QA(wf(:,120),wf(:,230)) * den(296)
  A(312) = cont_QA(wf(:,51),wf(:,276)) * den(297)
  A(313) = cont_QA(wf(:,124),wf(:,249)) * den(298)
  A(314) = cont_QA(wf(:,10),wf(:,277)) * den(299)
  A(315) = cont_QA(wf(:,123),wf(:,212)) * den(300)
  A(316) = cont_QA(wf(:,43),wf(:,278)) * den(301)
  A(317) = cont_QA(wf(:,124),wf(:,252)) * den(302)
  A(318) = cont_QA(wf(:,123),wf(:,215)) * den(303)
  A(319) = cont_QA(wf(:,159),wf(:,265)) * den(304)
  A(320) = cont_QA(wf(:,124),wf(:,253)) * den(305)
  A(321) = cont_QA(wf(:,159),wf(:,267)) * den(306)
  A(322) = cont_QA(wf(:,124),wf(:,254)) * den(307)
  A(323) = cont_QA(wf(:,46),wf(:,278)) * den(308)
  A(324) = cont_QA(wf(:,56),wf(:,276)) * den(309)
  A(325) = cont_QA(wf(:,161),wf(:,269)) * den(310)
  A(326) = cont_QA(wf(:,18),wf(:,277)) * den(311)
  A(327) = cont_QA(wf(:,123),wf(:,221)) * den(312)
  A(328) = cont_QA(wf(:,161),wf(:,270)) * den(313)
  A(329) = cont_QA(wf(:,123),wf(:,223)) * den(314)
  A(330) = cont_QA(wf(:,61),wf(:,279)) * den(315)
  A(331) = cont_QA(wf(:,161),wf(:,272)) * den(316)
  A(332) = cont_QA(wf(:,121),wf(:,246)) * den(317)
  A(333) = cont_QA(wf(:,161),wf(:,273)) * den(318)
  A(334) = cont_QA(wf(:,121),wf(:,248)) * den(319)
  A(335) = cont_QA(wf(:,40),wf(:,275)) * den(320)
  A(336) = cont_QA(wf(:,63),wf(:,279)) * den(321)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(336)
  complex(REALKIND), intent(out) :: M1(6), M2(6)

  M1(1) = ((A(25)+A(26)+A(27)+A(28)+A(35)+A(36))*f(1))/36._/**/REALKIND+((A(2)+A(3)+A(17)+A(18)+A(22)+A(23)+A(29)+A(30)+A(37) &
       +A(38)+A(39)+A(40))*f(1))/12._/**/REALKIND+((A(6)+A(7)+A(13)+A(20)+A(33)+A(41))*f(1))/4._/**/REALKIND+(CI*(-A(4) &
       -A(11))*f(2))/4._/**/REALKIND
  M1(2) = ((-A(3)-A(9)-A(23)-A(29)-A(31)-A(38))*f(1))/4._/**/REALKIND+((-A(15)-A(16)-A(17)-A(18)-A(39) &
       -A(40))*f(1))/36._/**/REALKIND+((-A(5)-A(6)-A(12)-A(13)-A(19)-A(20)-A(27)-A(28)-A(35)-A(36)-A(41) &
       -A(42))*f(1))/12._/**/REALKIND+(CI*(-A(1)+A(21))*f(2))/4._/**/REALKIND
  M1(3) = ((-A(22)-A(23)-A(29)-A(30)-A(31)-A(32))*f(1))/36._/**/REALKIND+((-A(2)-A(10)-A(16)-A(18)-A(37) &
       -A(39))*f(1))/4._/**/REALKIND+((-A(7)-A(8)-A(12)-A(13)-A(25)-A(26)-A(27)-A(28)-A(33)-A(34)-A(41) &
       -A(42))*f(1))/12._/**/REALKIND+(CI*(A(1)+A(14))*f(2))/4._/**/REALKIND
  M1(4) = ((A(12)+A(19)+A(26)+A(27)+A(34)+A(36))*f(1))/4._/**/REALKIND+((A(2)+A(3)+A(9)+A(10)+A(15)+A(16)+A(29)+A(30)+A(31)+A(32) &
       +A(39)+A(40))*f(1))/12._/**/REALKIND+((A(5)+A(6)+A(7)+A(8)+A(41)+A(42))*f(1))/36._/**/REALKIND+(CI*(A(11) &
       -A(24))*f(2))/4._/**/REALKIND
  M1(5) = ((A(12)+A(13)+A(19)+A(20)+A(33)+A(34))*f(1))/36._/**/REALKIND+((A(9)+A(10)+A(15)+A(16)+A(17)+A(18)+A(22)+A(23)+A(31) &
       +A(32)+A(37)+A(38))*f(1))/12._/**/REALKIND+((A(5)+A(8)+A(25)+A(28)+A(35)+A(42))*f(1))/4._/**/REALKIND+(CI*(A(4) &
       +A(24))*f(2))/4._/**/REALKIND
  M1(6) = ((-A(5)-A(6)-A(7)-A(8)-A(19)-A(20)-A(25)-A(26)-A(33)-A(34)-A(35)-A(36))*f(1))/12._/**/REALKIND+((-A(2)-A(3)-A(9)-A(10) &
       -A(37)-A(38))*f(1))/36._/**/REALKIND+((-A(15)-A(17)-A(22)-A(30)-A(32)-A(40))*f(1))/4._/**/REALKIND+(CI*(-A(14) &
       -A(21))*f(2))/4._/**/REALKIND

  M2(1) = ((-A(283)-A(284)-A(285)-A(286)-A(287)-A(288)-A(289)-A(290)-A(291)-A(292)-A(293)-A(294)-A(313)-A(314)-A(315)-A(316) &
       -A(317)-A(318))*f(3))/36._/**/REALKIND+((-A(214)-A(215)-A(216)-A(217)-A(218)-A(219)-A(259)-A(260)-A(261)-A(262)-A(263) &
       -A(264)-A(274)-A(275)-A(276)-A(277)-A(278)-A(279)-A(295)-A(296)-A(297)-A(298)-A(299)-A(300)-A(319)-A(320)-A(321)-A(322) &
       -A(323)-A(324)-A(325)-A(326)-A(327)-A(328)-A(329)-A(330))*f(3))/12._/**/REALKIND+((-A(224)-A(225)-A(227)-A(229)-A(232) &
       -A(233)-A(245)-A(246)-A(249)-A(266)-A(268)-A(269)-A(307)-A(310)-A(312)-A(331)-A(333)-A(336))*f(3))/4._/**/REALKIND+(CI*( &
       -A(220)-A(221)+A(222)-A(241)+A(242)+A(243))*f(4))/4._/**/REALKIND+((A(67)+A(68)+A(69)+A(70)+A(77)+A(78)+A(109)+A(110) &
       +A(121)+A(122)+A(123)+A(124)+A(140)+A(141)+A(145)+A(146)+A(155)+A(156)+A(179)+A(180)+A(184)+A(185)+A(191) &
       +A(192))*f(5))/36._/**/REALKIND+((A(44)+A(45)+A(59)+A(60)+A(64)+A(65)+A(71)+A(72)+A(79)+A(80)+A(81)+A(82)+A(85)+A(86)+A(98) &
       +A(99)+A(111)+A(112)+A(113)+A(114)+A(118)+A(119)+A(125)+A(126)+A(135)+A(136)+A(137)+A(138)+A(142)+A(143)+A(151)+A(152) &
       +A(157)+A(158)+A(165)+A(166)+A(174)+A(175)+A(182)+A(183)+A(187)+A(188)+A(189)+A(190)+A(195)+A(196)+A(202) &
       +A(203))*f(5))/12._/**/REALKIND+((A(48)+A(49)+A(55)+A(62)+A(75)+A(83)+A(89)+A(94)+A(95)+A(101)+A(107)+A(115)+A(127)+A(132) &
       +A(148)+A(154)+A(161)+A(167)+A(173)+A(178)+A(193)+A(199)+A(206)+A(207))*f(5))/4._/**/REALKIND+(CI*(-A(92)-A(97)-A(129) &
       -A(159)-A(176)-A(204))*f(6))/4._/**/REALKIND+(CI*(-A(46)-A(53))*f(7))/4._/**/REALKIND
  M2(2) = ((A(215)+A(216)+A(219)+A(235)+A(238)+A(240)+A(275)+A(276)+A(278)+A(295)+A(297)+A(300)+A(301)+A(304)+A(305)+A(320)+A(322) &
       +A(323))*f(3))/4._/**/REALKIND+((A(253)+A(254)+A(255)+A(256)+A(257)+A(258)+A(259)+A(260)+A(261)+A(262)+A(263)+A(264)+A(325) &
       +A(326)+A(327)+A(328)+A(329)+A(330))*f(3))/36._/**/REALKIND+((A(223)+A(224)+A(225)+A(226)+A(227)+A(228)+A(244)+A(245) &
       +A(246)+A(247)+A(248)+A(249)+A(265)+A(266)+A(267)+A(268)+A(269)+A(270)+A(289)+A(290)+A(291)+A(292)+A(293)+A(294)+A(313) &
       +A(314)+A(315)+A(316)+A(317)+A(318)+A(331)+A(332)+A(333)+A(334)+A(335)+A(336))*f(3))/12._/**/REALKIND+(CI*(A(211)-A(212) &
       +A(213)+A(271)-A(272)-A(273))*f(4))/4._/**/REALKIND+((-A(57)-A(58)-A(59)-A(60)-A(81)-A(82)-A(98)-A(99)-A(103)-A(104)-A(113) &
       -A(114)-A(151)-A(152)-A(163)-A(164)-A(165)-A(166)-A(169)-A(170)-A(174)-A(175)-A(195)-A(196))*f(5))/36._/**/REALKIND+(( &
       -A(47)-A(48)-A(54)-A(55)-A(61)-A(62)-A(69)-A(70)-A(77)-A(78)-A(83)-A(84)-A(93)-A(94)-A(95)-A(96)-A(100)-A(101)-A(109) &
       -A(110)-A(115)-A(116)-A(123)-A(124)-A(127)-A(128)-A(140)-A(141)-A(153)-A(154)-A(155)-A(156)-A(160)-A(161)-A(167)-A(168) &
       -A(172)-A(173)-A(177)-A(178)-A(184)-A(185)-A(191)-A(192)-A(193)-A(194)-A(205)-A(206))*f(5))/12._/**/REALKIND+((-A(45)-A(51) &
       -A(65)-A(71)-A(73)-A(80)-A(85)-A(90)-A(106)-A(112)-A(119)-A(125)-A(131)-A(136)-A(137)-A(143)-A(149)-A(157)-A(182)-A(188) &
       -A(190)-A(197)-A(203)-A(209))*f(5))/4._/**/REALKIND+(CI*(-A(87)+A(117)-A(134)+A(139)+A(186)-A(201))*f(6))/4._/**/REALKIND &
       +(CI*(-A(43)+A(63))*f(7))/4._/**/REALKIND
  M2(3) = ((A(274)+A(275)+A(276)+A(277)+A(278)+A(279)+A(295)+A(296)+A(297)+A(298)+A(299)+A(300)+A(301)+A(302)+A(303)+A(304)+A(305) &
       +A(306))*f(3))/36._/**/REALKIND+((A(214)+A(217)+A(218)+A(236)+A(237)+A(239)+A(254)+A(256)+A(257)+A(260)+A(261)+A(264) &
       +A(319)+A(321)+A(324)+A(325)+A(328)+A(330))*f(3))/4._/**/REALKIND+((A(229)+A(230)+A(231)+A(232)+A(233)+A(234)+A(244)+A(245) &
       +A(246)+A(247)+A(248)+A(249)+A(283)+A(284)+A(285)+A(286)+A(287)+A(288)+A(289)+A(290)+A(291)+A(292)+A(293)+A(294)+A(307) &
       +A(308)+A(309)+A(310)+A(311)+A(312)+A(331)+A(332)+A(333)+A(334)+A(335)+A(336))*f(3))/12._/**/REALKIND+(CI*(-A(211)+A(212) &
       -A(213)+A(250)+A(251)-A(252))*f(4))/4._/**/REALKIND+((-A(64)-A(65)-A(71)-A(72)-A(73)-A(74)-A(105)-A(106)-A(118)-A(119) &
       -A(125)-A(126)-A(137)-A(138)-A(142)-A(143)-A(149)-A(150)-A(182)-A(183)-A(187)-A(188)-A(197)-A(198))*f(5))/36._/**/REALKIND &
       +((-A(49)-A(50)-A(54)-A(55)-A(67)-A(68)-A(69)-A(70)-A(75)-A(76)-A(83)-A(84)-A(88)-A(89)-A(95)-A(96)-A(107)-A(108)-A(115) &
       -A(116)-A(121)-A(122)-A(123)-A(124)-A(132)-A(133)-A(140)-A(141)-A(145)-A(146)-A(147)-A(148)-A(153)-A(154)-A(160)-A(161) &
       -A(177)-A(178)-A(179)-A(180)-A(184)-A(185)-A(193)-A(194)-A(199)-A(200)-A(207)-A(208))*f(5))/12._/**/REALKIND+((-A(44)-A(52) &
       -A(58)-A(60)-A(79)-A(81)-A(86)-A(91)-A(98)-A(104)-A(111)-A(113)-A(130)-A(135)-A(152)-A(158)-A(163)-A(166)-A(170)-A(175) &
       -A(189)-A(195)-A(202)-A(210))*f(5))/4._/**/REALKIND+(CI*(A(87)+A(102)+A(134)+A(162)+A(171)+A(201))*f(6))/4._/**/REALKIND &
       +(CI*(A(43)+A(56))*f(7))/4._/**/REALKIND
  M2(4) = ((-A(244)-A(247)-A(248)-A(265)-A(267)-A(270)-A(284)-A(286)-A(287)-A(289)-A(292)-A(294)-A(308)-A(309)-A(311)-A(314) &
       -A(315)-A(318))*f(3))/4._/**/REALKIND+((-A(214)-A(215)-A(216)-A(217)-A(218)-A(219)-A(235)-A(236)-A(237)-A(238)-A(239) &
       -A(240)-A(253)-A(254)-A(255)-A(256)-A(257)-A(258)-A(295)-A(296)-A(297)-A(298)-A(299)-A(300)-A(301)-A(302)-A(303)-A(304) &
       -A(305)-A(306)-A(325)-A(326)-A(327)-A(328)-A(329)-A(330))*f(3))/12._/**/REALKIND+((-A(223)-A(224)-A(225)-A(226)-A(227) &
       -A(228)-A(229)-A(230)-A(231)-A(232)-A(233)-A(234)-A(331)-A(332)-A(333)-A(334)-A(335)-A(336))*f(3))/36._/**/REALKIND &
       +(CI*(A(241)-A(242)-A(243)+A(280)-A(281)+A(282))*f(4))/4._/**/REALKIND+((A(54)+A(61)+A(68)+A(69)+A(76)+A(78)+A(96)+A(100) &
       +A(108)+A(109)+A(122)+A(123)+A(141)+A(145)+A(147)+A(156)+A(160)+A(168)+A(172)+A(177)+A(180)+A(184)+A(192) &
       +A(200))*f(5))/4._/**/REALKIND+((A(47)+A(48)+A(49)+A(50)+A(83)+A(84)+A(88)+A(89)+A(93)+A(94)+A(115)+A(116)+A(127)+A(128) &
       +A(132)+A(133)+A(153)+A(154)+A(193)+A(194)+A(205)+A(206)+A(207)+A(208))*f(5))/36._/**/REALKIND+((A(44)+A(45)+A(51)+A(52) &
       +A(57)+A(58)+A(71)+A(72)+A(73)+A(74)+A(81)+A(82)+A(85)+A(86)+A(90)+A(91)+A(103)+A(104)+A(105)+A(106)+A(113)+A(114)+A(125) &
       +A(126)+A(130)+A(131)+A(135)+A(136)+A(142)+A(143)+A(149)+A(150)+A(151)+A(152)+A(163)+A(164)+A(169)+A(170)+A(182)+A(183) &
       +A(195)+A(196)+A(197)+A(198)+A(202)+A(203)+A(209)+A(210))*f(5))/12._/**/REALKIND+(CI*(A(97)-A(120)-A(144)+A(159)+A(176) &
       -A(181))*f(6))/4._/**/REALKIND+(CI*(A(53)-A(66))*f(7))/4._/**/REALKIND
  M2(5) = ((-A(244)-A(245)-A(246)-A(247)-A(248)-A(249)-A(265)-A(266)-A(267)-A(268)-A(269)-A(270)-A(307)-A(308)-A(309)-A(310) &
       -A(311)-A(312))*f(3))/36._/**/REALKIND+((-A(235)-A(236)-A(237)-A(238)-A(239)-A(240)-A(253)-A(254)-A(255)-A(256)-A(257) &
       -A(258)-A(259)-A(260)-A(261)-A(262)-A(263)-A(264)-A(274)-A(275)-A(276)-A(277)-A(278)-A(279)-A(301)-A(302)-A(303)-A(304) &
       -A(305)-A(306)-A(319)-A(320)-A(321)-A(322)-A(323)-A(324))*f(3))/12._/**/REALKIND+((-A(223)-A(226)-A(228)-A(230)-A(231) &
       -A(234)-A(283)-A(285)-A(288)-A(290)-A(291)-A(293)-A(313)-A(316)-A(317)-A(332)-A(334)-A(335))*f(3))/4._/**/REALKIND &
       +(CI*(A(220)+A(221)-A(222)-A(280)+A(281)-A(282))*f(4))/4._/**/REALKIND+((A(54)+A(55)+A(61)+A(62)+A(75)+A(76)+A(95)+A(96) &
       +A(100)+A(101)+A(107)+A(108)+A(147)+A(148)+A(160)+A(161)+A(167)+A(168)+A(172)+A(173)+A(177)+A(178)+A(199) &
       +A(200))*f(5))/36._/**/REALKIND+((A(47)+A(50)+A(67)+A(70)+A(77)+A(84)+A(88)+A(93)+A(110)+A(116)+A(121)+A(124)+A(128)+A(133) &
       +A(140)+A(146)+A(153)+A(155)+A(179)+A(185)+A(191)+A(194)+A(205)+A(208))*f(5))/4._/**/REALKIND+((A(51)+A(52)+A(57)+A(58) &
       +A(59)+A(60)+A(64)+A(65)+A(73)+A(74)+A(79)+A(80)+A(90)+A(91)+A(98)+A(99)+A(103)+A(104)+A(105)+A(106)+A(111)+A(112)+A(118) &
       +A(119)+A(130)+A(131)+A(137)+A(138)+A(149)+A(150)+A(157)+A(158)+A(163)+A(164)+A(165)+A(166)+A(169)+A(170)+A(174)+A(175) &
       +A(187)+A(188)+A(189)+A(190)+A(197)+A(198)+A(209)+A(210))*f(5))/12._/**/REALKIND+(CI*(A(92)+A(120)+A(129)+A(144)+A(181) &
       +A(204))*f(6))/4._/**/REALKIND+(CI*(A(46)+A(66))*f(7))/4._/**/REALKIND
  M2(6) = ((A(223)+A(224)+A(225)+A(226)+A(227)+A(228)+A(229)+A(230)+A(231)+A(232)+A(233)+A(234)+A(265)+A(266)+A(267)+A(268)+A(269) &
       +A(270)+A(283)+A(284)+A(285)+A(286)+A(287)+A(288)+A(307)+A(308)+A(309)+A(310)+A(311)+A(312)+A(313)+A(314)+A(315)+A(316) &
       +A(317)+A(318))*f(3))/12._/**/REALKIND+((A(214)+A(215)+A(216)+A(217)+A(218)+A(219)+A(235)+A(236)+A(237)+A(238)+A(239) &
       +A(240)+A(319)+A(320)+A(321)+A(322)+A(323)+A(324))*f(3))/36._/**/REALKIND+((A(253)+A(255)+A(258)+A(259)+A(262)+A(263) &
       +A(274)+A(277)+A(279)+A(296)+A(298)+A(299)+A(302)+A(303)+A(306)+A(326)+A(327)+A(329))*f(3))/4._/**/REALKIND+(CI*(-A(250) &
       -A(251)+A(252)-A(271)+A(272)+A(273))*f(4))/4._/**/REALKIND+((-A(57)-A(59)-A(64)-A(72)-A(74)-A(82)-A(99)-A(103)-A(105) &
       -A(114)-A(118)-A(126)-A(138)-A(142)-A(150)-A(151)-A(164)-A(165)-A(169)-A(174)-A(183)-A(187)-A(196) &
       -A(198))*f(5))/4._/**/REALKIND+((-A(47)-A(48)-A(49)-A(50)-A(61)-A(62)-A(67)-A(68)-A(75)-A(76)-A(77)-A(78)-A(88)-A(89)-A(93) &
       -A(94)-A(100)-A(101)-A(107)-A(108)-A(109)-A(110)-A(121)-A(122)-A(127)-A(128)-A(132)-A(133)-A(145)-A(146)-A(147)-A(148) &
       -A(155)-A(156)-A(167)-A(168)-A(172)-A(173)-A(179)-A(180)-A(191)-A(192)-A(199)-A(200)-A(205)-A(206)-A(207) &
       -A(208))*f(5))/12._/**/REALKIND+((-A(44)-A(45)-A(51)-A(52)-A(79)-A(80)-A(85)-A(86)-A(90)-A(91)-A(111)-A(112)-A(130)-A(131) &
       -A(135)-A(136)-A(157)-A(158)-A(189)-A(190)-A(202)-A(203)-A(209)-A(210))*f(5))/36._/**/REALKIND+(CI*(-A(102)-A(117)-A(139) &
       -A(162)-A(171)-A(186))*f(6))/4._/**/REALKIND+(CI*(-A(56)-A(63))*f(7))/4._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppjjjj_bbbbxbxbx_1_/**/REALKIND
