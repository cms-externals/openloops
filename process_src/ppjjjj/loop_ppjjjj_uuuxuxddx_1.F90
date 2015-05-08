
module ol_colourmatrix_ppjjjj_uuuxuxddx_1_/**/REALKIND
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
  K1( 25,:) = [   0,   0,   0,  -4,  -4, -12]
  K1( 26,:) = [   0,   0,  -4, -12,   0,  -4]
  K1( 27,:) = [   0,  -4,   0, -12,   0,  -4]
  K1( 28,:) = [  -4, -12, -12, -36,  -4, -12]
  K1( 29,:) = [  -4,   0,   0,  -4,   0, -12]
  K1( 30,:) = [ -12,  -4,  -4, -12, -12, -36]
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
  K1( 49,:) = [ -36, -12, -12,  -4,  -4, -12]
  K1( 50,:) = [ -12,   0,  -4,   0,   0,  -4]
  K1( 51,:) = [ -12,  -4,   0,   0,   0,  -4]
  K1( 52,:) = [  -4,   0,   0,   0,  -4, -12]
  K1( 53,:) = [  -4,   0,   0,  -4,   0, -12]
  K1( 54,:) = [ -12,  -4,  -4, -12, -12, -36]
  K1( 55,:) = [   0,  12,   0,   4,   4,   0]
  K1( 56,:) = [  12,   0,   4,   0,   0,   4]
  K1( 57,:) = [   0,   4,   0,  12,   0,   4]
  K1( 58,:) = [   4,   0,  12,   0,   4,   0]
  K1( 59,:) = [   4,   0,   0,   4,   0,  12]
  K1( 60,:) = [   0,   4,   4,   0,  12,   0]
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
  K1( 73,:) = [   0,  12,   0,   4,   4,   0]
  K1( 74,:) = [  12,   0,   4,   0,   0,   4]
  K1( 75,:) = [   0,   4,   0,   0,  12,   4]
  K1( 76,:) = [   4,   0,   0,   0,   4,  12]
  K1( 77,:) = [   4,   0,  12,   4,   0,   0]
  K1( 78,:) = [   0,   4,   4,  12,   0,   0]
  K1( 79,:) = [ -36, -12, -12,  -4,  -4, -12]
  K1( 80,:) = [ -12,   0,  -4,   0,   0,  -4]
  K1( 81,:) = [ -12,  -4, -36, -12, -12,  -4]
  K1( 82,:) = [  -4,   0, -12,   0,  -4,   0]
  K1( 83,:) = [  -4,   0, -12,  -4,   0,   0]
  K1( 84,:) = [ -12,  -4,  -4,   0,   0,   0]
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
  K1(103,:) = [   0,   0, -12,  -4,  -4,   0]
  K1(104,:) = [   0,   0,  -4, -12,   0,  -4]
  K1(105,:) = [ -12,  -4, -36, -12, -12,  -4]
  K1(106,:) = [  -4, -12, -12, -36,  -4, -12]
  K1(107,:) = [  -4,   0, -12,  -4,   0,   0]
  K1(108,:) = [   0,  -4,  -4, -12,   0,   0]
  K1(109,:) = [   0,   0,   0,   4,   4,  12]
  K1(110,:) = [   0,   0,   4,  12,   0,   4]
  K1(111,:) = [   0,   4,   0,   0,  12,   4]
  K1(112,:) = [   4,  12,   0,   0,   4,   0]
  K1(113,:) = [   4,   0,  12,   4,   0,   0]
  K1(114,:) = [  12,   4,   4,   0,   0,   0]
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
end module ol_colourmatrix_ppjjjj_uuuxuxddx_1_/**/REALKIND



module ol_forced_parameters_ppjjjj_uuuxuxddx_1_/**/REALKIND
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
end module ol_forced_parameters_ppjjjj_uuuxuxddx_1_/**/REALKIND

module ol_loop_ppjjjj_uuuxuxddx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(15), c(36)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:171)
  ! denominators
  complex(REALKIND), save :: den(185)
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
  complex(REALKIND) :: A(112)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,2))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,3))
  call vert_UV_W(wf(:,1),Q(:,5),wf(:,2),Q(:,10),wf(:,4))
  call vert_VQ_A(wf(:,1),wf(:,-4),wf(:,5))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,6))
  call prop_Q_A(wf(:,5),Q(:,21),ZERO,0_intkind1,wf(:,7))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,8))
  call vert_AV_Q(wf(:,-5),wf(:,1),wf(:,9))
  call prop_Q_A(wf(:,8),Q(:,26),ZERO,0_intkind1,wf(:,10))
  call vert_VQ_A(wf(:,1),wf(:,-1),wf(:,11))
  call vert_AV_Q(wf(:,-3),wf(:,3),wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,7),ZERO,0_intkind1,wf(:,13))
  call vert_VQ_A(wf(:,3),wf(:,-1),wf(:,14))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,15))
  call prop_Q_A(wf(:,14),Q(:,50),ZERO,0_intkind1,wf(:,16))
  call vert_QA_V(wf(:,0),wf(:,-3),wf(:,17))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,18))
  call vert_UV_W(wf(:,18),Q(:,6),wf(:,17),Q(:,9),wf(:,19))
  call vert_VQ_A(wf(:,17),wf(:,-4),wf(:,20))
  call vert_AV_Q(wf(:,-5),wf(:,18),wf(:,21))
  call prop_Q_A(wf(:,20),Q(:,25),ZERO,0_intkind1,wf(:,22))
  call vert_VQ_A(wf(:,18),wf(:,-4),wf(:,23))
  call vert_AV_Q(wf(:,-5),wf(:,17),wf(:,24))
  call prop_Q_A(wf(:,23),Q(:,22),ZERO,0_intkind1,wf(:,25))
  call vert_VQ_A(wf(:,18),wf(:,0),wf(:,26))
  call prop_Q_A(wf(:,26),Q(:,7),ZERO,0_intkind1,wf(:,27))
  call vert_VQ_A(wf(:,3),wf(:,0),wf(:,28))
  call vert_AV_Q(wf(:,-3),wf(:,18),wf(:,29))
  call prop_Q_A(wf(:,28),Q(:,49),ZERO,0_intkind1,wf(:,30))
  call vert_VQ_A(wf(:,17),wf(:,-1),wf(:,31))
  call vert_AV_Q(wf(:,-2),wf(:,3),wf(:,32))
  call prop_Q_A(wf(:,31),Q(:,11),ZERO,0_intkind1,wf(:,33))
  call vert_AV_Q(wf(:,-2),wf(:,17),wf(:,34))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,35))
  call prop_Q_A(wf(:,35),Q(:,11),ZERO,0_intkind1,wf(:,36))
  call vert_AV_Q(wf(:,-2),wf(:,2),wf(:,37))
  call counter_UV_W(wf(:,1),Q(:,5),wf(:,2),Q(:,10),wf(:,38))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,39))
  call counter_AV_Q(wf(:,-5),wf(:,1),wf(:,40))
  call counter_VQ_A(wf(:,2),wf(:,-4),wf(:,41))
  call prop_A_Q(wf(:,9),Q(:,37),ZERO,0_intkind1,wf(:,42))
  call counter_VQ_A(wf(:,1),wf(:,-4),wf(:,43))
  call prop_A_Q(wf(:,6),Q(:,42),ZERO,0_intkind1,wf(:,44))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,45))
  call vert_AV_Q(wf(:,-3),wf(:,45),wf(:,46))
  call vert_VQ_A(wf(:,45),wf(:,-1),wf(:,47))
  call prop_Q_A(wf(:,47),Q(:,50),ZERO,0_intkind1,wf(:,48))
  call counter_AV_Q(wf(:,-3),wf(:,3),wf(:,49))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,50))
  call counter_UV_W(wf(:,18),Q(:,6),wf(:,17),Q(:,9),wf(:,51))
  call counter_AV_Q(wf(:,-5),wf(:,18),wf(:,52))
  call counter_AV_Q(wf(:,-5),wf(:,17),wf(:,53))
  call counter_VQ_A(wf(:,18),wf(:,-4),wf(:,54))
  call prop_A_Q(wf(:,24),Q(:,41),ZERO,0_intkind1,wf(:,55))
  call counter_VQ_A(wf(:,17),wf(:,-4),wf(:,56))
  call prop_A_Q(wf(:,21),Q(:,38),ZERO,0_intkind1,wf(:,57))
  call vert_VQ_A(wf(:,45),wf(:,0),wf(:,58))
  call prop_Q_A(wf(:,58),Q(:,49),ZERO,0_intkind1,wf(:,59))
  call counter_AV_Q(wf(:,-3),wf(:,18),wf(:,60))
  call vert_AV_Q(wf(:,-2),wf(:,45),wf(:,61))
  call counter_AV_Q(wf(:,-2),wf(:,3),wf(:,62))
  call counter_AV_Q(wf(:,-2),wf(:,17),wf(:,63))
  call counter_AV_Q(wf(:,-2),wf(:,2),wf(:,64))
  call counter_VQ_A(wf(:,3),wf(:,-1),wf(:,65))
  call prop_A_Q(wf(:,15),Q(:,13),ZERO,0_intkind1,wf(:,66))
  call counter_VQ_A(wf(:,1),wf(:,-1),wf(:,67))
  call prop_A_Q(wf(:,12),Q(:,56),ZERO,0_intkind1,wf(:,68))
  call counter_QA_V(wf(:,-1),wf(:,-3),wf(:,69))
  call vert_UV_W(wf(:,1),Q(:,5),wf(:,69),Q(:,10),wf(:,70))
  call vert_AV_Q(wf(:,-5),wf(:,69),wf(:,71))
  call vert_VQ_A(wf(:,69),wf(:,-4),wf(:,72))
  call prop_Q_A(wf(:,72),Q(:,26),ZERO,0_intkind1,wf(:,73))
  call prop_A_Q(wf(:,34),Q(:,13),ZERO,0_intkind1,wf(:,74))
  call counter_VQ_A(wf(:,17),wf(:,-1),wf(:,75))
  call prop_A_Q(wf(:,32),Q(:,52),ZERO,0_intkind1,wf(:,76))
  call vert_AV_Q(wf(:,-2),wf(:,69),wf(:,77))
  call vert_VQ_A(wf(:,69),wf(:,0),wf(:,78))
  call prop_Q_A(wf(:,78),Q(:,11),ZERO,0_intkind1,wf(:,79))
  call counter_QA_V(wf(:,-1),wf(:,-2),wf(:,80))
  call vert_UV_W(wf(:,80),Q(:,6),wf(:,17),Q(:,9),wf(:,81))
  call vert_AV_Q(wf(:,-5),wf(:,80),wf(:,82))
  call vert_VQ_A(wf(:,80),wf(:,-4),wf(:,83))
  call prop_Q_A(wf(:,83),Q(:,22),ZERO,0_intkind1,wf(:,84))
  call vert_AV_Q(wf(:,-3),wf(:,80),wf(:,85))
  call vert_VQ_A(wf(:,80),wf(:,0),wf(:,86))
  call prop_Q_A(wf(:,86),Q(:,7),ZERO,0_intkind1,wf(:,87))
  call counter_VQ_A(wf(:,18),wf(:,0),wf(:,88))
  call counter_VQ_A(wf(:,3),wf(:,0),wf(:,89))
  call prop_A_Q(wf(:,29),Q(:,14),ZERO,0_intkind1,wf(:,90))
  call counter_QA_V(wf(:,0),wf(:,-3),wf(:,91))
  call vert_UV_W(wf(:,18),Q(:,6),wf(:,91),Q(:,9),wf(:,92))
  call vert_VQ_A(wf(:,91),wf(:,-4),wf(:,93))
  call prop_Q_A(wf(:,93),Q(:,25),ZERO,0_intkind1,wf(:,94))
  call vert_AV_Q(wf(:,-5),wf(:,91),wf(:,95))
  call counter_VQ_A(wf(:,2),wf(:,0),wf(:,96))
  call prop_A_Q(wf(:,37),Q(:,14),ZERO,0_intkind1,wf(:,97))
  call vert_VQ_A(wf(:,91),wf(:,-1),wf(:,98))
  call prop_Q_A(wf(:,98),Q(:,11),ZERO,0_intkind1,wf(:,99))
  call vert_AV_Q(wf(:,-2),wf(:,91),wf(:,100))
  call counter_QA_V(wf(:,0),wf(:,-2),wf(:,101))
  call vert_UV_W(wf(:,101),Q(:,5),wf(:,2),Q(:,10),wf(:,102))
  call vert_VQ_A(wf(:,101),wf(:,-4),wf(:,103))
  call prop_Q_A(wf(:,103),Q(:,21),ZERO,0_intkind1,wf(:,104))
  call vert_AV_Q(wf(:,-5),wf(:,101),wf(:,105))
  call vert_VQ_A(wf(:,101),wf(:,-1),wf(:,106))
  call prop_Q_A(wf(:,106),Q(:,7),ZERO,0_intkind1,wf(:,107))
  call vert_AV_Q(wf(:,-3),wf(:,101),wf(:,108))
  call vert_UV_W(wf(:,2),Q(:,10),wf(:,3),Q(:,48),wf(:,109))
  call counter_V_V(ctGG,wf(:,1),Q(:,5),wf(:,110))
  call vert_UV_W(wf(:,1),Q(:,5),wf(:,3),Q(:,48),wf(:,111))
  call counter_V_V(ctGG,wf(:,2),Q(:,10),wf(:,112))
  call counter_V_V(ctGG,wf(:,3),Q(:,48),wf(:,113))
  call vert_VQ_A(wf(:,110),wf(:,-4),wf(:,114))
  call vert_AV_Q(wf(:,-5),wf(:,110),wf(:,115))
  call vert_VQ_A(wf(:,112),wf(:,-4),wf(:,116))
  call vert_AV_Q(wf(:,-5),wf(:,112),wf(:,117))
  call counter_Q_A(ctqq,wf(:,7),Q(:,21),wf(:,118))
  call counter_Q_A(ctqq,wf(:,10),Q(:,26),wf(:,119))
  call vert_VQ_A(wf(:,110),wf(:,-1),wf(:,120))
  call vert_AV_Q(wf(:,-3),wf(:,110),wf(:,121))
  call counter_Q_A(ctqq,wf(:,13),Q(:,7),wf(:,122))
  call counter_Q_A(ctqq,wf(:,16),Q(:,50),wf(:,123))
  call vert_VQ_A(wf(:,113),wf(:,-1),wf(:,124))
  call vert_AV_Q(wf(:,-3),wf(:,113),wf(:,125))
  call vert_UV_W(wf(:,18),Q(:,6),wf(:,3),Q(:,48),wf(:,126))
  call counter_V_V(ctGG,wf(:,17),Q(:,9),wf(:,127))
  call vert_UV_W(wf(:,17),Q(:,9),wf(:,3),Q(:,48),wf(:,128))
  call counter_V_V(ctGG,wf(:,18),Q(:,6),wf(:,129))
  call vert_VQ_A(wf(:,127),wf(:,-4),wf(:,130))
  call vert_AV_Q(wf(:,-5),wf(:,127),wf(:,131))
  call vert_VQ_A(wf(:,129),wf(:,-4),wf(:,132))
  call vert_AV_Q(wf(:,-5),wf(:,129),wf(:,133))
  call counter_Q_A(ctqq,wf(:,22),Q(:,25),wf(:,134))
  call counter_Q_A(ctqq,wf(:,25),Q(:,22),wf(:,135))
  call vert_VQ_A(wf(:,129),wf(:,0),wf(:,136))
  call counter_Q_A(ctqq,wf(:,27),Q(:,7),wf(:,137))
  call counter_Q_A(ctqq,wf(:,30),Q(:,49),wf(:,138))
  call vert_VQ_A(wf(:,113),wf(:,0),wf(:,139))
  call vert_AV_Q(wf(:,-3),wf(:,129),wf(:,140))
  call vert_VQ_A(wf(:,127),wf(:,-1),wf(:,141))
  call vert_AV_Q(wf(:,-2),wf(:,127),wf(:,142))
  call counter_Q_A(ctqq,wf(:,33),Q(:,11),wf(:,143))
  call vert_AV_Q(wf(:,-2),wf(:,113),wf(:,144))
  call vert_VQ_A(wf(:,112),wf(:,0),wf(:,145))
  call counter_Q_A(ctqq,wf(:,36),Q(:,11),wf(:,146))
  call vert_AV_Q(wf(:,-2),wf(:,112),wf(:,147))
  call vert_QA_V(wf(:,13),wf(:,-3),wf(:,148))
  call vert_QA_V(wf(:,-1),wf(:,66),wf(:,149))
  call vert_QA_V(wf(:,7),wf(:,-5),wf(:,150))
  call vert_QA_V(wf(:,-4),wf(:,42),wf(:,151))
  call vert_QA_V(wf(:,27),wf(:,-3),wf(:,152))
  call vert_QA_V(wf(:,0),wf(:,90),wf(:,153))
  call vert_QA_V(wf(:,25),wf(:,-5),wf(:,154))
  call vert_QA_V(wf(:,-4),wf(:,57),wf(:,155))
  call vert_QA_V(wf(:,33),wf(:,-2),wf(:,156))
  call vert_QA_V(wf(:,-1),wf(:,74),wf(:,157))
  call vert_QA_V(wf(:,22),wf(:,-5),wf(:,158))
  call vert_QA_V(wf(:,-4),wf(:,55),wf(:,159))
  call vert_QA_V(wf(:,36),wf(:,-2),wf(:,160))
  call vert_QA_V(wf(:,0),wf(:,97),wf(:,161))
  call vert_QA_V(wf(:,10),wf(:,-5),wf(:,162))
  call vert_QA_V(wf(:,-4),wf(:,44),wf(:,163))
  call vert_QA_V(wf(:,30),wf(:,-2),wf(:,164))
  call vert_QA_V(wf(:,0),wf(:,76),wf(:,165))
  call vert_QA_V(wf(:,30),wf(:,-3),wf(:,166))
  call vert_QA_V(wf(:,0),wf(:,68),wf(:,167))
  call vert_QA_V(wf(:,16),wf(:,-2),wf(:,168))
  call vert_QA_V(wf(:,-1),wf(:,76),wf(:,169))
  call vert_QA_V(wf(:,16),wf(:,-3),wf(:,170))
  call vert_QA_V(wf(:,-1),wf(:,68),wf(:,171))

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
  den(2) = 1 / (Q(5,10))
  den(3) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,21))
  den(9) = 1 / (Q(5,26))
  den(12) = 1 / (Q(5,7))
  den(15) = 1 / (Q(5,50))
  den(18) = 1 / (Q(5,9))
  den(19) = 1 / (Q(5,6))
  den(22) = 1 / (Q(5,25))
  den(25) = 1 / (Q(5,22))
  den(30) = 1 / (Q(5,49))
  den(33) = 1 / (Q(5,11))
  den(40) = 1 / (Q(5,37))
  den(43) = 1 / (Q(5,42))
  den(46) = 1 / (Q(5,41))
  den(49) = 1 / (Q(5,38))
  den(52) = 1 / (Q(5,13))
  den(55) = 1 / (Q(5,56))
  den(60) = 1 / (Q(5,52))
  den(64) = 1 / (Q(5,14))
  den(71) = 1 / (Q(5,58))
  den(75) = 1 / (Q(5,53))
  den(78) = 1 / (Q(5,15))
  den(97) = 1 / (Q(5,54))
  den(101) = 1 / (Q(5,57))

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
  den(20) = den(18)*den(19)
  den(21) = den(3)*den(20)
  den(23) = den(18)*den(22)
  den(24) = den(19)*den(23)
  den(26) = den(19)*den(25)
  den(27) = den(18)*den(26)
  den(28) = den(12)*den(19)
  den(29) = den(3)*den(28)
  den(31) = den(3)*den(30)
  den(32) = den(19)*den(31)
  den(34) = den(18)*den(33)
  den(35) = den(3)*den(34)
  den(36) = den(16)*den(18)
  den(37) = den(2)*den(33)
  den(38) = den(3)*den(37)
  den(39) = den(2)*den(31)
  den(41) = den(1)*den(40)
  den(42) = den(2)*den(41)
  den(44) = den(2)*den(43)
  den(45) = den(1)*den(44)
  den(47) = den(18)*den(46)
  den(48) = den(19)*den(47)
  den(50) = den(19)*den(49)
  den(51) = den(18)*den(50)
  den(53) = den(1)*den(52)
  den(54) = den(3)*den(53)
  den(56) = den(3)*den(55)
  den(57) = den(1)*den(56)
  den(58) = den(18)*den(52)
  den(59) = den(3)*den(58)
  den(61) = den(3)*den(60)
  den(62) = den(18)*den(61)
  den(63) = den(19)*den(56)
  den(65) = den(19)*den(64)
  den(66) = den(3)*den(65)
  den(67) = den(2)*den(61)
  den(68) = den(2)*den(64)
  den(69) = den(3)*den(68)
  den(70) = den(2)*den(3)
  den(72) = den(70)*den(71)
  den(73) = den(1)*den(72)
  den(74) = den(1)*den(3)
  den(76) = den(74)*den(75)
  den(77) = den(2)*den(76)
  den(79) = den(4)*den(78)
  den(80) = den(3)*den(79)
  den(81) = den(1)**2
  den(82) = den(44)*den(81)
  den(83) = den(10)*den(81)
  den(84) = den(2)**2
  den(85) = den(41)*den(84)
  den(86) = den(7)*den(84)
  den(87) = den(7)*den(44)
  den(88) = den(10)*den(41)
  den(89) = den(56)*den(81)
  den(90) = den(16)*den(81)
  den(91) = den(13)*den(56)
  den(92) = den(16)*den(53)
  den(93) = den(3)**2
  den(94) = den(53)*den(93)
  den(95) = den(13)*den(93)
  den(96) = den(3)*den(19)
  den(98) = den(96)*den(97)
  den(99) = den(18)*den(98)
  den(100) = den(3)*den(18)
  den(102) = den(100)*den(101)
  den(103) = den(19)*den(102)
  den(104) = den(20)*den(78)
  den(105) = den(3)*den(104)
  den(106) = den(18)**2
  den(107) = den(50)*den(106)
  den(108) = den(26)*den(106)
  den(109) = den(19)**2
  den(110) = den(47)*den(109)
  den(111) = den(23)*den(109)
  den(112) = den(23)*den(50)
  den(113) = den(26)*den(47)
  den(114) = den(56)*den(109)
  den(115) = den(28)*den(56)
  den(116) = den(31)*den(65)
  den(117) = den(65)*den(93)
  den(118) = den(31)*den(109)
  den(119) = den(28)*den(93)
  den(120) = den(61)*den(106)
  den(121) = den(16)*den(106)
  den(122) = den(34)*den(61)
  den(123) = den(16)*den(58)
  den(124) = den(58)*den(93)
  den(125) = den(34)*den(93)
  den(126) = den(61)*den(84)
  den(127) = den(37)*den(61)
  den(128) = den(31)*den(68)
  den(129) = den(68)*den(93)
  den(130) = den(31)*den(84)
  den(131) = den(37)*den(93)
  den(132) = den(13)*den(78)
  den(133) = den(53)*den(78)
  den(134) = den(7)*den(75)
  den(135) = den(41)*den(75)
  den(136) = den(28)*den(78)
  den(137) = den(65)*den(78)
  den(138) = den(26)*den(97)
  den(139) = den(50)*den(97)
  den(140) = den(34)*den(78)
  den(141) = den(58)*den(78)
  den(142) = den(23)*den(101)
  den(143) = den(47)*den(101)
  den(144) = den(37)*den(78)
  den(145) = den(68)*den(78)
  den(146) = den(10)*den(71)
  den(147) = den(44)*den(71)
  den(148) = den(31)*den(75)
  den(149) = den(61)*den(75)
  den(150) = den(31)*den(101)
  den(151) = den(56)*den(101)
  den(152) = den(16)*den(97)
  den(153) = den(61)*den(97)
  den(154) = den(16)*den(71)
  den(155) = den(56)*den(71)
  den(156) = den(1)*den(2)*den(3)
  den(157) = den(3)*den(18)*den(19)
  den(158) = den(1)*den(70)
  den(159) = den(2)*den(74)
  den(160) = den(18)*den(96)
  den(161) = den(19)*den(100)
  den(162) = den(2)*den(134)
  den(163) = den(2)*den(135)
  den(164) = den(1)*den(146)
  den(165) = den(1)*den(147)
  den(166) = den(3)*den(132)
  den(167) = den(3)*den(133)
  den(168) = den(1)*den(154)
  den(169) = den(1)*den(155)
  den(170) = den(19)*den(142)
  den(171) = den(19)*den(143)
  den(172) = den(18)*den(138)
  den(173) = den(18)*den(139)
  den(174) = den(3)*den(136)
  den(175) = den(19)*den(150)
  den(176) = den(3)*den(137)
  den(177) = den(19)*den(151)
  den(178) = den(3)*den(140)
  den(179) = den(3)*den(141)
  den(180) = den(18)*den(152)
  den(181) = den(18)*den(153)
  den(182) = den(3)*den(144)
  den(183) = den(2)*den(148)
  den(184) = den(3)*den(145)
  den(185) = den(2)*den(149)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(112)

  A(1) = cont_VV(wf(:,3),wf(:,4)) * den(5)
  A(2) = cont_QA(wf(:,6),wf(:,7)) * den(8)
  A(3) = cont_QA(wf(:,9),wf(:,10)) * den(11)
  A(4) = cont_QA(wf(:,12),wf(:,13)) * den(14)
  A(5) = cont_QA(wf(:,15),wf(:,16)) * den(17)
  A(6) = cont_VV(wf(:,3),wf(:,19)) * den(21)
  A(7) = cont_QA(wf(:,21),wf(:,22)) * den(24)
  A(8) = cont_QA(wf(:,24),wf(:,25)) * den(27)
  A(9) = cont_QA(wf(:,12),wf(:,27)) * den(29)
  A(10) = cont_QA(wf(:,29),wf(:,30)) * den(32)
  A(11) = cont_QA(wf(:,32),wf(:,33)) * den(35)
  A(12) = cont_QA(wf(:,16),wf(:,34)) * den(36)
  A(13) = cont_QA(wf(:,32),wf(:,36)) * den(38)
  A(14) = cont_QA(wf(:,30),wf(:,37)) * den(39)

  A(15) = cont_VV(wf(:,3),wf(:,38)) * den(5)
  A(16) = cont_QA(wf(:,7),wf(:,39)) * den(8)
  A(17) = cont_QA(wf(:,10),wf(:,40)) * den(11)
  A(18) = cont_QA(wf(:,41),wf(:,42)) * den(42)
  A(19) = cont_QA(wf(:,43),wf(:,44)) * den(45)
  A(20) = cont_VV(wf(:,4),wf(:,45)) * den(5)
  A(21) = cont_QA(wf(:,13),wf(:,46)) * den(14)
  A(22) = cont_QA(wf(:,15),wf(:,48)) * den(17)
  A(23) = cont_QA(wf(:,13),wf(:,49)) * den(14)
  A(24) = cont_QA(wf(:,16),wf(:,50)) * den(17)
  A(25) = cont_VV(wf(:,3),wf(:,51)) * den(21)
  A(26) = cont_QA(wf(:,22),wf(:,52)) * den(24)
  A(27) = cont_QA(wf(:,25),wf(:,53)) * den(27)
  A(28) = cont_QA(wf(:,54),wf(:,55)) * den(48)
  A(29) = cont_QA(wf(:,56),wf(:,57)) * den(51)
  A(30) = cont_VV(wf(:,19),wf(:,45)) * den(21)
  A(31) = cont_QA(wf(:,27),wf(:,46)) * den(29)
  A(32) = cont_QA(wf(:,29),wf(:,59)) * den(32)
  A(33) = cont_QA(wf(:,27),wf(:,49)) * den(29)
  A(34) = cont_QA(wf(:,30),wf(:,60)) * den(32)
  A(35) = cont_QA(wf(:,33),wf(:,61)) * den(35)
  A(36) = cont_QA(wf(:,34),wf(:,48)) * den(36)
  A(37) = cont_QA(wf(:,36),wf(:,61)) * den(38)
  A(38) = cont_QA(wf(:,37),wf(:,59)) * den(39)
  A(39) = cont_QA(wf(:,33),wf(:,62)) * den(35)
  A(40) = cont_QA(wf(:,16),wf(:,63)) * den(36)
  A(41) = cont_QA(wf(:,36),wf(:,62)) * den(38)
  A(42) = cont_QA(wf(:,30),wf(:,64)) * den(39)
  A(43) = cont_QA(wf(:,65),wf(:,66)) * den(54)
  A(44) = cont_QA(wf(:,67),wf(:,68)) * den(57)
  A(45) = cont_VV(wf(:,3),wf(:,70)) * den(5)
  A(46) = cont_QA(wf(:,7),wf(:,71)) * den(8)
  A(47) = cont_QA(wf(:,9),wf(:,73)) * den(11)
  A(48) = cont_QA(wf(:,65),wf(:,74)) * den(59)
  A(49) = cont_QA(wf(:,75),wf(:,76)) * den(62)
  A(50) = cont_QA(wf(:,30),wf(:,77)) * den(39)
  A(51) = cont_QA(wf(:,32),wf(:,79)) * den(38)
  A(52) = cont_VV(wf(:,3),wf(:,81)) * den(21)
  A(53) = cont_QA(wf(:,22),wf(:,82)) * den(24)
  A(54) = cont_QA(wf(:,24),wf(:,84)) * den(27)
  A(55) = cont_QA(wf(:,30),wf(:,85)) * den(32)
  A(56) = cont_QA(wf(:,12),wf(:,87)) * den(29)
  A(57) = cont_QA(wf(:,68),wf(:,88)) * den(63)
  A(58) = cont_QA(wf(:,89),wf(:,90)) * den(66)
  A(59) = cont_VV(wf(:,3),wf(:,92)) * den(21)
  A(60) = cont_QA(wf(:,21),wf(:,94)) * den(24)
  A(61) = cont_QA(wf(:,25),wf(:,95)) * den(27)
  A(62) = cont_QA(wf(:,76),wf(:,96)) * den(67)
  A(63) = cont_QA(wf(:,89),wf(:,97)) * den(69)
  A(64) = cont_QA(wf(:,32),wf(:,99)) * den(35)
  A(65) = cont_QA(wf(:,16),wf(:,100)) * den(36)
  A(66) = cont_VV(wf(:,3),wf(:,102)) * den(5)
  A(67) = cont_QA(wf(:,6),wf(:,104)) * den(8)
  A(68) = cont_QA(wf(:,10),wf(:,105)) * den(11)
  A(69) = cont_QA(wf(:,12),wf(:,107)) * den(14)
  A(70) = cont_QA(wf(:,16),wf(:,108)) * den(17)
  A(71) = cont_VV(wf(:,109),wf(:,110)) * den(73)
  A(72) = cont_VV(wf(:,111),wf(:,112)) * den(77)
  A(73) = cont_VV(wf(:,4),wf(:,113)) * den(80)
  A(74) = cont_QA(wf(:,44),wf(:,114)) * den(82)
  A(75) = cont_QA(wf(:,10),wf(:,115)) * den(83)
  A(76) = cont_QA(wf(:,42),wf(:,116)) * den(85)
  A(77) = cont_QA(wf(:,7),wf(:,117)) * den(86)
  A(78) = cont_QA(wf(:,44),wf(:,118)) * den(87)
  A(79) = cont_QA(wf(:,42),wf(:,119)) * den(88)
  A(80) = cont_QA(wf(:,68),wf(:,120)) * den(89)
  A(81) = cont_QA(wf(:,16),wf(:,121)) * den(90)
  A(82) = cont_QA(wf(:,68),wf(:,122)) * den(91)
  A(83) = cont_QA(wf(:,66),wf(:,123)) * den(92)
  A(84) = cont_QA(wf(:,66),wf(:,124)) * den(94)
  A(85) = cont_QA(wf(:,13),wf(:,125)) * den(95)
  A(86) = cont_VV(wf(:,126),wf(:,127)) * den(99)
  A(87) = cont_VV(wf(:,128),wf(:,129)) * den(103)
  A(88) = cont_VV(wf(:,19),wf(:,113)) * den(105)
  A(89) = cont_QA(wf(:,57),wf(:,130)) * den(107)
  A(90) = cont_QA(wf(:,25),wf(:,131)) * den(108)
  A(91) = cont_QA(wf(:,55),wf(:,132)) * den(110)
  A(92) = cont_QA(wf(:,22),wf(:,133)) * den(111)
  A(93) = cont_QA(wf(:,57),wf(:,134)) * den(112)
  A(94) = cont_QA(wf(:,55),wf(:,135)) * den(113)
  A(95) = cont_QA(wf(:,68),wf(:,136)) * den(114)
  A(96) = cont_QA(wf(:,68),wf(:,137)) * den(115)
  A(97) = cont_QA(wf(:,90),wf(:,138)) * den(116)
  A(98) = cont_QA(wf(:,90),wf(:,139)) * den(117)
  A(99) = cont_QA(wf(:,30),wf(:,140)) * den(118)
  A(100) = cont_QA(wf(:,27),wf(:,125)) * den(119)
  A(101) = cont_QA(wf(:,76),wf(:,141)) * den(120)
  A(102) = cont_QA(wf(:,16),wf(:,142)) * den(121)
  A(103) = cont_QA(wf(:,76),wf(:,143)) * den(122)
  A(104) = cont_QA(wf(:,74),wf(:,123)) * den(123)
  A(105) = cont_QA(wf(:,74),wf(:,124)) * den(124)
  A(106) = cont_QA(wf(:,33),wf(:,144)) * den(125)
  A(107) = cont_QA(wf(:,76),wf(:,145)) * den(126)
  A(108) = cont_QA(wf(:,76),wf(:,146)) * den(127)
  A(109) = cont_QA(wf(:,97),wf(:,138)) * den(128)
  A(110) = cont_QA(wf(:,97),wf(:,139)) * den(129)
  A(111) = cont_QA(wf(:,30),wf(:,147)) * den(130)
  A(112) = cont_QA(wf(:,36),wf(:,144)) * den(131)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(112)
  complex(REALKIND), intent(out) :: M1(6), M2(6)

  M1(1) = ((A(8)+A(10)+A(11))*f(1))/4._/**/REALKIND+((A(2)+A(3)+A(13)+A(14))*f(1))/12._/**/REALKIND &
       -(CI*A(6)*f(2))/4._/**/REALKIND
  M1(2) = ((-A(7)-A(8)-A(9)-A(10))*f(1))/12._/**/REALKIND+((-A(3)-A(4)-A(14))*f(1))/4._/**/REALKIND &
       -(CI*A(1)*f(2))/4._/**/REALKIND
  M1(3) = ((-A(7)-A(8)-A(11)-A(12))*f(1))/12._/**/REALKIND+((-A(2)-A(5)-A(13))*f(1))/4._/**/REALKIND &
       +(CI*A(1)*f(2))/4._/**/REALKIND
  M1(4) = ((A(2)+A(3)+A(4)+A(5))*f(1))/12._/**/REALKIND+((A(7)+A(9)+A(12))*f(1))/4._/**/REALKIND+(CI*A(6)*f(2))/4._/**/REALKIND
  M1(5) = ((A(7)+A(8)+A(9)+A(10)+A(11)+A(12))*f(1))/36._/**/REALKIND+((A(4)+A(5)+A(13)+A(14))*f(1))/12._/**/REALKIND
  M1(6) = ((-A(9)-A(10)-A(11)-A(12))*f(1))/12._/**/REALKIND+((-A(2)-A(3)-A(4)-A(5)-A(13)-A(14))*f(1))/36._/**/REALKIND

  M2(1) = ((-A(90)-A(91)-A(94)-A(97)-A(98)-A(99)-A(101)-A(103)-A(106))*f(3))/4._/**/REALKIND+((-A(74)-A(75)-A(76)-A(77)-A(78) &
       -A(79)-A(107)-A(108)-A(109)-A(110)-A(111)-A(112))*f(3))/12._/**/REALKIND+(CI*(-A(86)+A(87)+A(88))*f(4))/4._/**/REALKIND &
       +((A(27)+A(28)+A(32)+A(34)+A(35)+A(39)+A(49)+A(54)+A(55)+A(58)+A(61)+A(64))*f(5))/4._/**/REALKIND+((A(16)+A(17)+A(18)+A(19) &
       +A(37)+A(38)+A(41)+A(42)+A(46)+A(47)+A(50)+A(51)+A(62)+A(63)+A(67)+A(68))*f(5))/12._/**/REALKIND+(CI*(-A(30)-A(52) &
       -A(59))*f(6))/4._/**/REALKIND-(CI*A(25)*f(7))/4._/**/REALKIND
  M2(2) = ((A(89)+A(90)+A(91)+A(92)+A(93)+A(94)+A(95)+A(96)+A(97)+A(98)+A(99)+A(100))*f(3))/12._/**/REALKIND+((A(75)+A(76)+A(79) &
       +A(80)+A(82)+A(85)+A(109)+A(110)+A(111))*f(3))/4._/**/REALKIND+(CI*(A(71)-A(72)+A(73))*f(4))/4._/**/REALKIND+((-A(26)-A(27) &
       -A(28)-A(29)-A(31)-A(32)-A(33)-A(34)-A(53)-A(54)-A(55)-A(56)-A(57)-A(58)-A(60)-A(61))*f(5))/12._/**/REALKIND+((-A(17)-A(18) &
       -A(21)-A(23)-A(38)-A(42)-A(44)-A(47)-A(50)-A(63)-A(68)-A(69))*f(5))/4._/**/REALKIND+(CI*(-A(20)-A(45) &
       -A(66))*f(6))/4._/**/REALKIND-(CI*A(15)*f(7))/4._/**/REALKIND
  M2(3) = ((A(89)+A(90)+A(91)+A(92)+A(93)+A(94)+A(101)+A(102)+A(103)+A(104)+A(105)+A(106))*f(3))/12._/**/REALKIND+((A(74)+A(77) &
       +A(78)+A(81)+A(83)+A(84)+A(107)+A(108)+A(112))*f(3))/4._/**/REALKIND+(CI*(-A(71)+A(72)-A(73))*f(4))/4._/**/REALKIND+(( &
       -A(26)-A(27)-A(28)-A(29)-A(35)-A(36)-A(39)-A(40)-A(48)-A(49)-A(53)-A(54)-A(60)-A(61)-A(64)-A(65))*f(5))/12._/**/REALKIND+(( &
       -A(16)-A(19)-A(22)-A(24)-A(37)-A(41)-A(43)-A(46)-A(51)-A(62)-A(67)-A(70))*f(5))/4._/**/REALKIND+(CI*(A(20)+A(45) &
       +A(66))*f(6))/4._/**/REALKIND+(CI*A(15)*f(7))/4._/**/REALKIND
  M2(4) = ((-A(74)-A(75)-A(76)-A(77)-A(78)-A(79)-A(80)-A(81)-A(82)-A(83)-A(84)-A(85))*f(3))/12._/**/REALKIND+((-A(89)-A(92)-A(93) &
       -A(95)-A(96)-A(100)-A(102)-A(104)-A(105))*f(3))/4._/**/REALKIND+(CI*(A(86)-A(87)-A(88))*f(4))/4._/**/REALKIND+((A(26)+A(29) &
       +A(31)+A(33)+A(36)+A(40)+A(48)+A(53)+A(56)+A(57)+A(60)+A(65))*f(5))/4._/**/REALKIND+((A(16)+A(17)+A(18)+A(19)+A(21)+A(22) &
       +A(23)+A(24)+A(43)+A(44)+A(46)+A(47)+A(67)+A(68)+A(69)+A(70))*f(5))/12._/**/REALKIND+(CI*(A(30)+A(52) &
       +A(59))*f(6))/4._/**/REALKIND+(CI*A(25)*f(7))/4._/**/REALKIND
  M2(5) = ((-A(89)-A(90)-A(91)-A(92)-A(93)-A(94)-A(95)-A(96)-A(97)-A(98)-A(99)-A(100)-A(101)-A(102)-A(103)-A(104)-A(105) &
       -A(106))*f(3))/36._/**/REALKIND+((-A(80)-A(81)-A(82)-A(83)-A(84)-A(85)-A(107)-A(108)-A(109)-A(110)-A(111) &
       -A(112))*f(3))/12._/**/REALKIND+((A(26)+A(27)+A(28)+A(29)+A(31)+A(32)+A(33)+A(34)+A(35)+A(36)+A(39)+A(40)+A(48)+A(49)+A(53) &
       +A(54)+A(55)+A(56)+A(57)+A(58)+A(60)+A(61)+A(64)+A(65))*f(5))/36._/**/REALKIND+((A(21)+A(22)+A(23)+A(24)+A(37)+A(38)+A(41) &
       +A(42)+A(43)+A(44)+A(50)+A(51)+A(62)+A(63)+A(69)+A(70))*f(5))/12._/**/REALKIND
  M2(6) = ((A(95)+A(96)+A(97)+A(98)+A(99)+A(100)+A(101)+A(102)+A(103)+A(104)+A(105)+A(106))*f(3))/12._/**/REALKIND+((A(74)+A(75) &
       +A(76)+A(77)+A(78)+A(79)+A(80)+A(81)+A(82)+A(83)+A(84)+A(85)+A(107)+A(108)+A(109)+A(110)+A(111) &
       +A(112))*f(3))/36._/**/REALKIND+((-A(31)-A(32)-A(33)-A(34)-A(35)-A(36)-A(39)-A(40)-A(48)-A(49)-A(55)-A(56)-A(57)-A(58) &
       -A(64)-A(65))*f(5))/12._/**/REALKIND+((-A(16)-A(17)-A(18)-A(19)-A(21)-A(22)-A(23)-A(24)-A(37)-A(38)-A(41)-A(42)-A(43)-A(44) &
       -A(46)-A(47)-A(50)-A(51)-A(62)-A(63)-A(67)-A(68)-A(69)-A(70))*f(5))/36._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppjjjj_uuuxuxddx_1_/**/REALKIND
