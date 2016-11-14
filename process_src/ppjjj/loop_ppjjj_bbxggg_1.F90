
module ol_colourmatrix_ppjjj_bbxggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(102,6), K2(6,11), KL(6,11)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1(  1,:) = [  384,  -48,  -48,    6,    6,   60]
  K1(  2,:) = [  -48,  384,    6,   60,  -48,    6]
  K1(  3,:) = [  -48,    6,  384,  -48,   60,    6]
  K1(  4,:) = [    6,   60,  -48,  384,    6,  -48]
  K1(  5,:) = [    6,  -48,   60,    6,  384,  -48]
  K1(  6,:) = [   60,    6,    6,  -48,  -48,  384]
  K1(  7,:) = [  512,  -64,  -64,    8,    8,   80]
  K1(  8,:) = [  -64,  512,    8,   80,  -64,    8]
  K1(  9,:) = [  -64,    8,  512,  -64,   80,    8]
  K1( 10,:) = [    8,   80,  -64,  512,    8,  -64]
  K1( 11,:) = [    8,  -64,   80,    8,  512,  -64]
  K1( 12,:) = [   80,    8,    8,  -64,  -64,  512]
  K1( 13,:) = [    1,   10,   10,  -62,  -62,   28]
  K1( 14,:) = [   10,    1,  -62,   28,   10,  -62]
  K1( 15,:) = [   10,  -62,    1,   10,   28,  -62]
  K1( 16,:) = [  -62,   28,   10,    1,  -62,   10]
  K1( 17,:) = [  -62,   10,   28,  -62,    1,   10]
  K1( 18,:) = [   28,  -62,  -62,   10,   10,    1]
  K1( 19,:) = [  512,  -64,  -64,    8,    8,   80]
  K1( 20,:) = [  -64,  512,    8,   80,  -64,    8]
  K1( 21,:) = [  -64,    8,  512,  -64,   80,    8]
  K1( 22,:) = [    8,   80,  -64,  512,    8,  -64]
  K1( 23,:) = [    8,  -64,   80,    8,  512,  -64]
  K1( 24,:) = [   80,    8,    8,  -64,  -64,  512]
  K1( 25,:) = [   -9,  -90,   -9,   -9,   72,  -90]
  K1( 26,:) = [  -90,   -9,   72,  -90,   -9,   -9]
  K1( 27,:) = [   -9,   72,   72,   72,   72,   -9]
  K1( 28,:) = [   -9,  -90,   72, -576,   -9,   72]
  K1( 29,:) = [   72,   -9,   72,   -9,   72,   72]
  K1( 30,:) = [  -90,   -9,   -9,   72,   72, -576]
  K1( 31,:) = [ -576,   72,   72,   -9,   -9,  -90]
  K1( 32,:) = [   72, -576,   -9,  -90,   72,   -9]
  K1( 33,:) = [   72,   -9,   72,   -9,   72,   72]
  K1( 34,:) = [   -9,  -90,   -9,   -9,   72,  -90]
  K1( 35,:) = [   -9,   72,   72,   72,   72,   -9]
  K1( 36,:) = [  -90,   -9,   72,  -90,   -9,   -9]
  K1( 37,:) = [ 1152, -144, -144,   18,   18,  180]
  K1( 38,:) = [ -144, 1152,   18,  180, -144,   18]
  K1( 39,:) = [ -144,   18, 1152, -144,  180,   18]
  K1( 40,:) = [   18,  180, -144, 1152,   18, -144]
  K1( 41,:) = [   18, -144,  180,   18, 1152, -144]
  K1( 42,:) = [  180,   18,   18, -144, -144, 1152]
  K1( 43,:) = [   72,   72,   -9,   72,   -9,   72]
  K1( 44,:) = [   72, -576,   -9,  -90,   72,   -9]
  K1( 45,:) = [   -9,   -9,   -9,  -90,  -90,   72]
  K1( 46,:) = [   72,  -90,  -90,   -9,   -9,   -9]
  K1( 47,:) = [   -9,   72,  -90,   -9, -576,   72]
  K1( 48,:) = [   72,   -9,   72,   -9,   72,   72]
  K1( 49,:) = [   72,   -9,   72,   -9,   72,   72]
  K1( 50,:) = [   -9,   -9,   -9,  -90,  -90,   72]
  K1( 51,:) = [   72,   -9, -576,   72,  -90,   -9]
  K1( 52,:) = [   -9,  -90,   72, -576,   -9,   72]
  K1( 53,:) = [   72,  -90,  -90,   -9,   -9,   -9]
  K1( 54,:) = [   72,   72,   -9,   72,   -9,   72]
  K1( 55,:) = [ -648,   81,    0,    0,  -81, -162]
  K1( 56,:) = [   81,   81,    0,  162,   81,    0]
  K1( 57,:) = [    0,    0, -648,   81, -162,  -81]
  K1( 58,:) = [    0,  162,   81,   81,    0,   81]
  K1( 59,:) = [  -81,   81, -162,    0, -648,    0]
  K1( 60,:) = [ -162,    0,  -81,   81,    0, -648]
  K1( 61,:) = [ 1152, -144, -144,   18,   18,  180]
  K1( 62,:) = [ -144, 1152,   18,  180, -144,   18]
  K1( 63,:) = [ -144,   18, 1152, -144,  180,   18]
  K1( 64,:) = [   18,  180, -144, 1152,   18, -144]
  K1( 65,:) = [   18, -144,  180,   18, 1152, -144]
  K1( 66,:) = [  180,   18,   18, -144, -144, 1152]
  K1( 67,:) = [ -576,   72,   72,   -9,   -9,  -90]
  K1( 68,:) = [   72,   72,   -9,   72,   -9,   72]
  K1( 69,:) = [   72,   -9, -576,   72,  -90,   -9]
  K1( 70,:) = [   -9,   72,   72,   72,   72,   -9]
  K1( 71,:) = [   -9,   -9,  -90,   72,   -9,  -90]
  K1( 72,:) = [  -90,   72,   -9,   -9,  -90,   -9]
  K1( 73,:) = [   -9,   -9,  -90,   72,   -9,  -90]
  K1( 74,:) = [   -9,   72,   72,   72,   72,   -9]
  K1( 75,:) = [  -90,   72,   -9,   -9,  -90,   -9]
  K1( 76,:) = [   72,   72,   -9,   72,   -9,   72]
  K1( 77,:) = [   -9,   72,  -90,   -9, -576,   72]
  K1( 78,:) = [  -90,   -9,   -9,   72,   72, -576]
  K1( 79,:) = [   81,   81,   81,    0,    0,  162]
  K1( 80,:) = [   81, -648,  -81, -162,    0,    0]
  K1( 81,:) = [   81,  -81, -648,    0, -162,    0]
  K1( 82,:) = [    0, -162,    0, -648,  -81,   81]
  K1( 83,:) = [    0,    0, -162,  -81, -648,   81]
  K1( 84,:) = [  162,    0,    0,   81,   81,   81]
  K1( 85,:) = [ -648,    0,   81,  -81,    0, -162]
  K1( 86,:) = [    0, -648,    0, -162,   81,  -81]
  K1( 87,:) = [   81,    0,   81,   81,  162,    0]
  K1( 88,:) = [  -81, -162,   81, -648,    0,    0]
  K1( 89,:) = [    0,   81,  162,    0,   81,   81]
  K1( 90,:) = [ -162,  -81,    0,    0,   81, -648]
  K1( 91,:) = [ 1152, -144, -144,   18,   18,  180]
  K1( 92,:) = [ -144, 1152,   18,  180, -144,   18]
  K1( 93,:) = [ -144,   18, 1152, -144,  180,   18]
  K1( 94,:) = [   18,  180, -144, 1152,   18, -144]
  K1( 95,:) = [   18, -144,  180,   18, 1152, -144]
  K1( 96,:) = [  180,   18,   18, -144, -144, 1152]
  K1( 97,:) = [    0,    0,    0,    0,    0,    0]
  K1( 98,:) = [    0,    0,    0,    0,    0,    0]
  K1( 99,:) = [    0,    0,    0,    0,    0,    0]
  K1(100,:) = [    0,    0,    0,    0,    0,    0]
  K1(101,:) = [    0,    0,    0,    0,    0,    0]
  K1(102,:) = [    0,    0,    0,    0,    0,    0]
  K1 = (1._/**/REALKIND / 54) * K1

  K2(1,:) = [ 64, -8, -8,  1,  1, 10, 24, -3, 24, 21, -6]
  K2(2,:) = [ -8, 64,  1, 10, -8,  1, -3, 24, 24, -6, 21]
  K2(3,:) = [ -8,  1, 64, -8, 10,  1, 24, 24, -3, -6, 21]
  K2(4,:) = [  1, 10, -8, 64,  1, -8, -3, 24, 24, 21, -6]
  K2(5,:) = [  1, -8, 10,  1, 64, -8, 24, 24, -3, 21, -6]
  K2(6,:) = [ 10,  1,  1, -8, -8, 64, 24, -3, 24, -6, 21]
  K2 = (1._/**/REALKIND / 9) * K2

  KL(1,:) = [ 64, -8, -8,  1,  1, 10, 24, -3, 24, 21, -6]
  KL(2,:) = [ -8, 64,  1, 10, -8,  1, -3, 24, 24, -6, 21]
  KL(3,:) = [ -8,  1, 64, -8, 10,  1, 24, 24, -3, -6, 21]
  KL(4,:) = [  1, 10, -8, 64,  1, -8, -3, 24, 24, 21, -6]
  KL(5,:) = [  1, -8, 10,  1, 64, -8, 24, 24, -3, 21, -6]
  KL(6,:) = [ 10,  1,  1, -8, -8, 64, 24, -3, 24, -6, 21]
  KL = (1._/**/REALKIND / 9) * KL

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppjjj_bbxggg_1_/**/REALKIND



module ol_forced_parameters_ppjjj_bbxggg_1_/**/REALKIND
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


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppjjj_bbxggg_1_/**/REALKIND

module ol_loop_ppjjj_bbxggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(18), c(25)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:126)
  ! denominators
  complex(REALKIND), save :: den(96)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(6,32)
  ! zero helicity identifier
  logical,           save :: zerohel(32) = .true., zerohel_ct(32) = .true.

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
    f( 1) = CI*gQCD**3
    f( 2) = gQCD**3
    f( 3) = CI*countertermnorm*gQCD**5
    f( 4) = countertermnorm*gQCD**5
    f( 5) = CI*countertermnorm*ctGbb*gQCD**5
    f( 6) = countertermnorm*ctGbb*gQCD**5
    f( 7) = CI*countertermnorm*ctVVV*gQCD**5
    f( 8) = countertermnorm*ctVVV*gQCD**5
    f( 9) = CI*countertermnorm*ctVVVV*gQCD**5
    f(10) = CI*countertermnorm*gQCD**5*R2GGGG
    f(11) = (CI*gQCD**5*integralnorm*SwB)/2._/**/REALKIND
    f(12) = CI*gQCD**5*integralnorm*SwB
    f(13) = (gQCD**5*integralnorm*SwB)/2._/**/REALKIND
    f(14) = gQCD**5*integralnorm*SwB
    f(15) = CI*gQCD**5*integralnorm*SwF
    f(16) = 2*CI*gQCD**5*integralnorm*SwF
    f(17) = gQCD**5*integralnorm*SwF
    f(18) = 2*gQCD**5*integralnorm*SwF

  c = [ 9*CI*f(11), 18*CI*f(11), CI*f(12), 3*CI*f(12), 8*CI*f(12), 9*CI*f(12), 18*CI*f(12), 3*f(13), 6*f(13), 9*f(13), 18*f(13) &
    , f(14), 3*f(14), 6*f(14), 8*f(14), 9*f(14), 18*f(14), CI*f(15), 3*CI*f(15), CI*f(16), 3*CI*f(16), f(17), 3*f(17), f(18) &
    , 3*f(18) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,5)
  integer,           intent(in)  :: H(5)
  complex(REALKIND), intent(out) :: M1(6), M2(11)
  complex(REALKIND) :: A(105)
  ! external WFs
  call wf_Q(P(:,1), rMB, H(1), wf(:,0))
  call wf_A(P(:,2), rMB, H(2), wf(:,-1))
  call wf_V(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_V(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_GGG_G(wf(:,-2),wf(:,-3),wf(:,-4),wf(:,2))
  call vert_GGG_G(wf(:,-3),wf(:,-4),wf(:,-2),wf(:,3))
  call vert_GGG_G(wf(:,-4),wf(:,-2),wf(:,-3),wf(:,4))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,5))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,6))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,7))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,-3),Q(:,8),wf(:,8))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,9))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,-2),Q(:,4),wf(:,10))
  call vert_VQ_A(wf(:,-2),wf(:,0),wf(:,11))
  call vert_AV_Q(wf(:,-1),wf(:,-3),wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,5),MB,1_intkind1,wf(:,13))
  call prop_A_Q(wf(:,12),Q(:,10),MB,1_intkind1,wf(:,14))
  call vert_VQ_A(wf(:,-4),wf(:,13),wf(:,15))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,16))
  call prop_A_Q(wf(:,16),Q(:,18),MB,1_intkind1,wf(:,17))
  call vert_VQ_A(wf(:,-3),wf(:,13),wf(:,18))
  call vert_QA_V(wf(:,13),wf(:,-1),wf(:,19))
  call vert_VQ_A(wf(:,-3),wf(:,0),wf(:,20))
  call vert_AV_Q(wf(:,-1),wf(:,-2),wf(:,21))
  call prop_Q_A(wf(:,20),Q(:,9),MB,1_intkind1,wf(:,22))
  call prop_A_Q(wf(:,21),Q(:,6),MB,1_intkind1,wf(:,23))
  call vert_VQ_A(wf(:,-4),wf(:,22),wf(:,24))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,25))
  call prop_Q_A(wf(:,25),Q(:,17),MB,1_intkind1,wf(:,26))
  call vert_VQ_A(wf(:,-3),wf(:,26),wf(:,27))
  call vert_QA_V(wf(:,0),wf(:,23),wf(:,28))
  call vert_VQ_A(wf(:,-2),wf(:,22),wf(:,29))
  call vert_QA_V(wf(:,22),wf(:,-1),wf(:,30))
  call vert_VQ_A(wf(:,-2),wf(:,26),wf(:,31))
  call vert_QA_V(wf(:,0),wf(:,14),wf(:,32))
  call vert_QA_V(wf(:,26),wf(:,-1),wf(:,33))
  call vert_QA_V(wf(:,0),wf(:,17),wf(:,34))
  call counter_GGG_G(wf(:,-2),wf(:,-3),wf(:,-4),wf(:,35))
  call counter_GGG_G(wf(:,-3),wf(:,-4),wf(:,-2),wf(:,36))
  call counter_GGG_G(wf(:,-4),wf(:,-2),wf(:,-3),wf(:,37))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,38))
  call counter_UV_W(wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,39))
  call counter_UV_W(wf(:,1),Q(:,3),wf(:,-3),Q(:,8),wf(:,40))
  call counter_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,41))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,42))
  call counter_UV_W(wf(:,1),Q(:,3),wf(:,-2),Q(:,4),wf(:,43))
  call counter_UV_W(wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,44))
  call counter_UV_W(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,45))
  call counter_VQ_A(wf(:,-4),wf(:,13),wf(:,46))
  call counter_VQ_A(wf(:,-3),wf(:,13),wf(:,47))
  call counter_VQ_A(wf(:,-4),wf(:,22),wf(:,48))
  call counter_VQ_A(wf(:,-3),wf(:,26),wf(:,49))
  call counter_VQ_A(wf(:,-2),wf(:,22),wf(:,50))
  call counter_VQ_A(wf(:,-2),wf(:,26),wf(:,51))
  call counter_QA_V(wf(:,13),wf(:,-1),wf(:,52))
  call counter_AV_Q(wf(:,-1),wf(:,-4),wf(:,53))
  call prop_A_Q(wf(:,53),Q(:,18),MB,1_intkind1,wf(:,54))
  call counter_AV_Q(wf(:,-1),wf(:,-3),wf(:,55))
  call prop_A_Q(wf(:,55),Q(:,10),MB,1_intkind1,wf(:,56))
  call counter_QA_V(wf(:,22),wf(:,-1),wf(:,57))
  call counter_QA_V(wf(:,26),wf(:,-1),wf(:,58))
  call vert_QA_V(wf(:,0),wf(:,54),wf(:,59))
  call vert_QA_V(wf(:,0),wf(:,56),wf(:,60))
  call counter_AV_Q(wf(:,-1),wf(:,-2),wf(:,61))
  call prop_A_Q(wf(:,61),Q(:,6),MB,1_intkind1,wf(:,62))
  call vert_QA_V(wf(:,0),wf(:,62),wf(:,63))
  call counter_QA_V(wf(:,0),wf(:,23),wf(:,64))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,65))
  call prop_Q_A(wf(:,65),Q(:,17),MB,1_intkind1,wf(:,66))
  call vert_VQ_A(wf(:,-3),wf(:,66),wf(:,67))
  call counter_VQ_A(wf(:,-3),wf(:,0),wf(:,68))
  call prop_Q_A(wf(:,68),Q(:,9),MB,1_intkind1,wf(:,69))
  call vert_VQ_A(wf(:,-4),wf(:,69),wf(:,70))
  call counter_QA_V(wf(:,0),wf(:,14),wf(:,71))
  call vert_VQ_A(wf(:,-2),wf(:,66),wf(:,72))
  call counter_QA_V(wf(:,0),wf(:,17),wf(:,73))
  call vert_QA_V(wf(:,66),wf(:,-1),wf(:,74))
  call vert_VQ_A(wf(:,-2),wf(:,69),wf(:,75))
  call vert_QA_V(wf(:,69),wf(:,-1),wf(:,76))
  call counter_VQ_A(wf(:,-2),wf(:,0),wf(:,77))
  call prop_Q_A(wf(:,77),Q(:,5),MB,1_intkind1,wf(:,78))
  call vert_VQ_A(wf(:,-4),wf(:,78),wf(:,79))
  call vert_VQ_A(wf(:,-3),wf(:,78),wf(:,80))
  call vert_QA_V(wf(:,78),wf(:,-1),wf(:,81))
  call vert_UV_W(wf(:,38),Q(:,3),wf(:,-4),Q(:,16),wf(:,82))
  call vert_UV_W(wf(:,38),Q(:,3),wf(:,-3),Q(:,8),wf(:,83))
  call vert_UV_W(wf(:,38),Q(:,3),wf(:,-2),Q(:,4),wf(:,84))
  call vert_UV_W(wf(:,5),Q(:,12),wf(:,-4),Q(:,16),wf(:,85))
  call counter_V_V(ctGG,wf(:,5),Q(:,12),wf(:,86))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,7),Q(:,20),wf(:,87))
  call counter_V_V(ctGG,wf(:,7),Q(:,20),wf(:,88))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,9),Q(:,24),wf(:,89))
  call counter_V_V(ctGG,wf(:,9),Q(:,24),wf(:,90))
  call vert_AV_Q(wf(:,14),wf(:,-4),wf(:,91))
  call counter_Q_A(ctbb,wf(:,13),Q(:,5),wf(:,92))
  call prop_A_Q(wf(:,91),Q(:,26),MB,1_intkind1,wf(:,93))
  call counter_A_Q(ctbb,wf(:,14),Q(:,10),wf(:,94))
  call prop_Q_A(wf(:,15),Q(:,21),MB,1_intkind1,wf(:,95))
  call vert_AV_Q(wf(:,17),wf(:,-3),wf(:,96))
  call prop_A_Q(wf(:,96),Q(:,26),MB,1_intkind1,wf(:,97))
  call counter_A_Q(ctbb,wf(:,17),Q(:,18),wf(:,98))
  call prop_Q_A(wf(:,18),Q(:,13),MB,1_intkind1,wf(:,99))
  call vert_AV_Q(wf(:,-1),wf(:,9),wf(:,100))
  call prop_A_Q(wf(:,100),Q(:,26),MB,1_intkind1,wf(:,101))
  call vert_AV_Q(wf(:,23),wf(:,-4),wf(:,102))
  call counter_Q_A(ctbb,wf(:,22),Q(:,9),wf(:,103))
  call prop_A_Q(wf(:,102),Q(:,22),MB,1_intkind1,wf(:,104))
  call counter_A_Q(ctbb,wf(:,23),Q(:,6),wf(:,105))
  call prop_Q_A(wf(:,24),Q(:,25),MB,1_intkind1,wf(:,106))
  call vert_AV_Q(wf(:,23),wf(:,-3),wf(:,107))
  call counter_Q_A(ctbb,wf(:,26),Q(:,17),wf(:,108))
  call prop_A_Q(wf(:,107),Q(:,14),MB,1_intkind1,wf(:,109))
  call prop_Q_A(wf(:,27),Q(:,25),MB,1_intkind1,wf(:,110))
  call vert_VQ_A(wf(:,9),wf(:,0),wf(:,111))
  call prop_Q_A(wf(:,111),Q(:,25),MB,1_intkind1,wf(:,112))
  call vert_AV_Q(wf(:,17),wf(:,-2),wf(:,113))
  call prop_A_Q(wf(:,113),Q(:,22),MB,1_intkind1,wf(:,114))
  call prop_Q_A(wf(:,29),Q(:,13),MB,1_intkind1,wf(:,115))
  call vert_AV_Q(wf(:,-1),wf(:,7),wf(:,116))
  call prop_A_Q(wf(:,116),Q(:,22),MB,1_intkind1,wf(:,117))
  call vert_AV_Q(wf(:,14),wf(:,-2),wf(:,118))
  call prop_A_Q(wf(:,118),Q(:,14),MB,1_intkind1,wf(:,119))
  call prop_Q_A(wf(:,31),Q(:,21),MB,1_intkind1,wf(:,120))
  call vert_VQ_A(wf(:,7),wf(:,0),wf(:,121))
  call prop_Q_A(wf(:,121),Q(:,21),MB,1_intkind1,wf(:,122))
  call vert_AV_Q(wf(:,-1),wf(:,5),wf(:,123))
  call prop_A_Q(wf(:,123),Q(:,14),MB,1_intkind1,wf(:,124))
  call vert_VQ_A(wf(:,5),wf(:,0),wf(:,125))
  call prop_Q_A(wf(:,125),Q(:,13),MB,1_intkind1,wf(:,126))

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
  den(4) = 1 / (Q(5,20))
  den(6) = 1 / (Q(5,24))
  den(8) = 1 / (Q(5,5) - MB2)
  den(9) = 1 / (Q(5,10) - MB2)
  den(11) = 1 / (Q(5,18) - MB2)
  den(14) = 1 / (Q(5,9) - MB2)
  den(15) = 1 / (Q(5,6) - MB2)
  den(17) = 1 / (Q(5,17) - MB2)
  den(26) = 1 / (Q(5,28))
  den(30) = 1 / (Q(5,19))
  den(35) = 1 / (Q(5,11))
  den(40) = 1 / (Q(5,7))
  den(43) = 1 / (Q(5,26) - MB2)
  den(46) = 1 / (Q(5,21) - MB2)
  den(51) = 1 / (Q(5,13) - MB2)
  den(58) = 1 / (Q(5,22) - MB2)
  den(61) = 1 / (Q(5,25) - MB2)
  den(64) = 1 / (Q(5,14) - MB2)

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(1)*den(6)
  den(10) = den(8)*den(9)
  den(12) = den(8)*den(11)
  den(13) = den(6)*den(8)
  den(16) = den(14)*den(15)
  den(18) = den(15)*den(17)
  den(19) = den(6)*den(15)
  den(20) = den(11)*den(14)
  den(21) = den(4)*den(14)
  den(22) = den(9)*den(17)
  den(23) = den(4)*den(9)
  den(24) = den(2)*den(17)
  den(25) = den(2)*den(11)
  den(27) = den(1)*den(26)
  den(28) = den(2)*den(26)
  den(29) = den(1)*den(28)
  den(31) = den(1)*den(30)
  den(32) = den(2)*den(31)
  den(33) = den(4)*den(26)
  den(34) = den(1)*den(33)
  den(36) = den(1)*den(35)
  den(37) = den(4)*den(36)
  den(38) = den(6)*den(26)
  den(39) = den(1)*den(38)
  den(41) = den(1)*den(40)
  den(42) = den(6)*den(41)
  den(44) = den(9)*den(43)
  den(45) = den(8)*den(44)
  den(47) = den(8)*den(46)
  den(48) = den(9)*den(47)
  den(49) = den(11)*den(43)
  den(50) = den(8)*den(49)
  den(52) = den(8)*den(51)
  den(53) = den(11)*den(52)
  den(54) = den(6)*den(43)
  den(55) = den(8)*den(54)
  den(56) = den(8)*den(40)
  den(57) = den(6)*den(56)
  den(59) = den(15)*den(58)
  den(60) = den(14)*den(59)
  den(62) = den(14)*den(61)
  den(63) = den(15)*den(62)
  den(65) = den(15)*den(64)
  den(66) = den(17)*den(65)
  den(67) = den(17)*den(61)
  den(68) = den(15)*den(67)
  den(69) = den(6)*den(61)
  den(70) = den(15)*den(69)
  den(71) = den(15)*den(40)
  den(72) = den(6)*den(71)
  den(73) = den(11)*den(58)
  den(74) = den(14)*den(73)
  den(75) = den(14)*den(51)
  den(76) = den(11)*den(75)
  den(77) = den(4)*den(58)
  den(78) = den(14)*den(77)
  den(79) = den(14)*den(35)
  den(80) = den(4)*den(79)
  den(81) = den(9)*den(64)
  den(82) = den(17)*den(81)
  den(83) = den(17)*den(46)
  den(84) = den(9)*den(83)
  den(85) = den(4)*den(46)
  den(86) = den(9)*den(85)
  den(87) = den(9)*den(35)
  den(88) = den(4)*den(87)
  den(89) = den(2)*den(64)
  den(90) = den(17)*den(89)
  den(91) = den(17)*den(30)
  den(92) = den(2)*den(91)
  den(93) = den(2)*den(51)
  den(94) = den(11)*den(93)
  den(95) = den(11)*den(30)
  den(96) = den(2)*den(95)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(105)

  A(1) = cont_VV(wf(:,1),wf(:,2)) * den(1)
  A(2) = cont_VV(wf(:,1),wf(:,3)) * den(1)
  A(3) = cont_VV(wf(:,1),wf(:,4)) * den(1)
  A(4) = cont_VV(wf(:,5),wf(:,6)) * den(3)
  A(5) = cont_VV(wf(:,7),wf(:,8)) * den(5)
  A(6) = cont_VV(wf(:,9),wf(:,10)) * den(7)
  A(7) = cont_QA(wf(:,14),wf(:,15)) * den(10)
  A(8) = cont_QA(wf(:,17),wf(:,18)) * den(12)
  A(9) = cont_VV(wf(:,9),wf(:,19)) * den(13)
  A(10) = cont_QA(wf(:,23),wf(:,24)) * den(16)
  A(11) = cont_QA(wf(:,23),wf(:,27)) * den(18)
  A(12) = cont_VV(wf(:,9),wf(:,28)) * den(19)
  A(13) = cont_QA(wf(:,17),wf(:,29)) * den(20)
  A(14) = cont_VV(wf(:,7),wf(:,30)) * den(21)
  A(15) = cont_QA(wf(:,14),wf(:,31)) * den(22)
  A(16) = cont_VV(wf(:,7),wf(:,32)) * den(23)
  A(17) = cont_VV(wf(:,5),wf(:,33)) * den(24)
  A(18) = cont_VV(wf(:,5),wf(:,34)) * den(25)

  A(19) = cont_VV(wf(:,1),wf(:,35)) * den(1)
  A(20) = cont_VV(wf(:,1),wf(:,36)) * den(1)
  A(21) = cont_VV(wf(:,1),wf(:,37)) * den(1)
  A(22) = cont_VV(wf(:,1),wf(:,35)) * den(1)
  A(23) = cont_VV(wf(:,1),wf(:,36)) * den(1)
  A(24) = cont_VV(wf(:,1),wf(:,37)) * den(1)
  A(25) = cont_VV(wf(:,2),wf(:,38)) * den(26)
  A(26) = cont_VV(wf(:,3),wf(:,38)) * den(26)
  A(27) = cont_VV(wf(:,4),wf(:,38)) * den(26)
  A(28) = cont_VV(wf(:,5),wf(:,39)) * den(3)
  A(29) = cont_VV(wf(:,7),wf(:,40)) * den(5)
  A(30) = cont_VV(wf(:,10),wf(:,41)) * den(7)
  A(31) = cont_VV(wf(:,2),wf(:,42)) * den(27)
  A(32) = cont_VV(wf(:,3),wf(:,42)) * den(27)
  A(33) = cont_VV(wf(:,4),wf(:,42)) * den(27)
  A(34) = cont_VV(wf(:,9),wf(:,43)) * den(7)
  A(35) = cont_VV(wf(:,8),wf(:,44)) * den(5)
  A(36) = cont_VV(wf(:,6),wf(:,45)) * den(3)
  A(37) = cont_QA(wf(:,14),wf(:,46)) * den(10)
  A(38) = cont_QA(wf(:,17),wf(:,47)) * den(12)
  A(39) = cont_VV(wf(:,19),wf(:,41)) * den(13)
  A(40) = cont_QA(wf(:,23),wf(:,48)) * den(16)
  A(41) = cont_QA(wf(:,23),wf(:,49)) * den(18)
  A(42) = cont_VV(wf(:,28),wf(:,41)) * den(19)
  A(43) = cont_QA(wf(:,17),wf(:,50)) * den(20)
  A(44) = cont_VV(wf(:,30),wf(:,44)) * den(21)
  A(45) = cont_QA(wf(:,14),wf(:,51)) * den(22)
  A(46) = cont_VV(wf(:,32),wf(:,44)) * den(23)
  A(47) = cont_VV(wf(:,33),wf(:,45)) * den(24)
  A(48) = cont_VV(wf(:,34),wf(:,45)) * den(25)
  A(49) = cont_VV(wf(:,9),wf(:,52)) * den(13)
  A(50) = cont_QA(wf(:,18),wf(:,54)) * den(12)
  A(51) = cont_QA(wf(:,15),wf(:,56)) * den(10)
  A(52) = cont_VV(wf(:,7),wf(:,57)) * den(21)
  A(53) = cont_QA(wf(:,29),wf(:,54)) * den(20)
  A(54) = cont_VV(wf(:,5),wf(:,58)) * den(24)
  A(55) = cont_VV(wf(:,5),wf(:,59)) * den(25)
  A(56) = cont_QA(wf(:,31),wf(:,56)) * den(22)
  A(57) = cont_VV(wf(:,7),wf(:,60)) * den(23)
  A(58) = cont_QA(wf(:,24),wf(:,62)) * den(16)
  A(59) = cont_QA(wf(:,27),wf(:,62)) * den(18)
  A(60) = cont_VV(wf(:,9),wf(:,63)) * den(19)
  A(61) = cont_VV(wf(:,9),wf(:,64)) * den(19)
  A(62) = cont_QA(wf(:,23),wf(:,67)) * den(18)
  A(63) = cont_QA(wf(:,23),wf(:,70)) * den(16)
  A(64) = cont_VV(wf(:,7),wf(:,71)) * den(23)
  A(65) = cont_QA(wf(:,14),wf(:,72)) * den(22)
  A(66) = cont_VV(wf(:,5),wf(:,73)) * den(25)
  A(67) = cont_VV(wf(:,5),wf(:,74)) * den(24)
  A(68) = cont_QA(wf(:,17),wf(:,75)) * den(20)
  A(69) = cont_VV(wf(:,7),wf(:,76)) * den(21)
  A(70) = cont_QA(wf(:,14),wf(:,79)) * den(10)
  A(71) = cont_QA(wf(:,17),wf(:,80)) * den(12)
  A(72) = cont_VV(wf(:,9),wf(:,81)) * den(13)
  A(73) = cont_VV(wf(:,5),wf(:,82)) * den(3)
  A(74) = cont_VV(wf(:,7),wf(:,83)) * den(5)
  A(75) = cont_VV(wf(:,9),wf(:,84)) * den(7)
  A(76) = cont_VV(wf(:,42),wf(:,85)) * den(29)
  A(77) = cont_VV(wf(:,6),wf(:,86)) * den(32)
  A(78) = cont_VV(wf(:,42),wf(:,87)) * den(34)
  A(79) = cont_VV(wf(:,8),wf(:,88)) * den(37)
  A(80) = cont_VV(wf(:,42),wf(:,89)) * den(39)
  A(81) = cont_VV(wf(:,10),wf(:,90)) * den(42)
  A(82) = cont_QA(wf(:,92),wf(:,93)) * den(45)
  A(83) = cont_QA(wf(:,94),wf(:,95)) * den(48)
  A(84) = cont_QA(wf(:,92),wf(:,97)) * den(50)
  A(85) = cont_QA(wf(:,98),wf(:,99)) * den(53)
  A(86) = cont_QA(wf(:,92),wf(:,101)) * den(55)
  A(87) = cont_VV(wf(:,19),wf(:,90)) * den(57)
  A(88) = cont_QA(wf(:,103),wf(:,104)) * den(60)
  A(89) = cont_QA(wf(:,105),wf(:,106)) * den(63)
  A(90) = cont_QA(wf(:,108),wf(:,109)) * den(66)
  A(91) = cont_QA(wf(:,105),wf(:,110)) * den(68)
  A(92) = cont_QA(wf(:,105),wf(:,112)) * den(70)
  A(93) = cont_VV(wf(:,28),wf(:,90)) * den(72)
  A(94) = cont_QA(wf(:,103),wf(:,114)) * den(74)
  A(95) = cont_QA(wf(:,98),wf(:,115)) * den(76)
  A(96) = cont_QA(wf(:,103),wf(:,117)) * den(78)
  A(97) = cont_VV(wf(:,30),wf(:,88)) * den(80)
  A(98) = cont_QA(wf(:,108),wf(:,119)) * den(82)
  A(99) = cont_QA(wf(:,94),wf(:,120)) * den(84)
  A(100) = cont_QA(wf(:,94),wf(:,122)) * den(86)
  A(101) = cont_VV(wf(:,32),wf(:,88)) * den(88)
  A(102) = cont_QA(wf(:,108),wf(:,124)) * den(90)
  A(103) = cont_VV(wf(:,33),wf(:,86)) * den(92)
  A(104) = cont_QA(wf(:,98),wf(:,126)) * den(94)
  A(105) = cont_VV(wf(:,34),wf(:,86)) * den(96)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(105)
  complex(REALKIND), intent(out) :: M1(6), M2(11)

  M1(1) = (A(1)-A(2)-A(4)+A(6)+A(11))*f(1)+CI*(A(12)+A(17))*f(2)
  M1(2) = (A(2)-A(3)-A(5)-A(6)+A(10))*f(1)+CI*(-A(12)+A(14))*f(2)
  M1(3) = (-A(1)+A(3)+A(4)+A(5)+A(15))*f(1)+CI*(A(16)-A(17))*f(2)
  M1(4) = (A(2)-A(3)-A(5)-A(6)+A(7))*f(1)+CI*(A(9)-A(16))*f(2)
  M1(5) = (-A(1)+A(3)+A(4)+A(5)+A(13))*f(1)+CI*(-A(14)+A(18))*f(2)
  M1(6) = (A(1)-A(2)-A(4)+A(6)+A(8))*f(1)+CI*(-A(9)-A(18))*f(2)

  M2( 1) = (-A(31)+A(32)-A(76)+A(77)-A(80)-A(81)-A(90)-A(91))*f(3)+CI*(-A(92)-A(93)-A(102)-A(103))*f(4)+(A(25)-A(26)+A(41)+A(59) &
        +A(62)-A(73)+A(75))*f(5)+CI*(A(54)+A(60)+A(61)+A(67))*f(6)+(-A(28)+A(30)+A(34)-A(36))*f(7)+CI*(A(42)+A(47))*f(8)+(-A(19) &
        -A(20))*f(9)+2*A(21)*f(9)+(19*(A(22)+A(23))*f(10))/2._/**/REALKIND-18*A(24)*f(10)
  M2( 2) = (-A(32)+A(33)+A(78)+A(79)+A(80)+A(81)-A(88)-A(89))*f(3)+CI*(A(92)+A(93)-A(96)-A(97))*f(4)+(A(26)-A(27)+A(40)+A(58) &
        +A(63)-A(74)-A(75))*f(5)+CI*(A(52)-A(60)-A(61)+A(69))*f(6)+(-A(29)-A(30)-A(34)-A(35))*f(7)+CI*(-A(42)+A(44))*f(8) &
        +2*A(19)*f(9)+(-A(20)-A(21))*f(9)-18*A(22)*f(10)+(19*(A(23)+A(24))*f(10))/2._/**/REALKIND
  M2( 3) = (A(31)-A(33)+A(76)-A(77)-A(78)-A(79)-A(98)-A(99))*f(3)+CI*(-A(100)-A(101)+A(102)+A(103))*f(4)+(-A(25)+A(27)+A(45)+A(56) &
        +A(65)+A(73)+A(74))*f(5)+CI*(-A(54)+A(57)+A(64)-A(67))*f(6)+(A(28)+A(29)+A(35)+A(36))*f(7)+CI*(A(46)-A(47))*f(8) &
        +2*A(20)*f(9)+(-A(19)-A(21))*f(9)-18*A(23)*f(10)+(19*(A(22)+A(24))*f(10))/2._/**/REALKIND
  M2( 4) = (-A(32)+A(33)+A(78)+A(79)+A(80)+A(81)-A(82)-A(83))*f(3)+CI*(-A(86)-A(87)+A(100)+A(101))*f(4)+(A(26)-A(27)+A(37)+A(51) &
        +A(70)-A(74)-A(75))*f(5)+CI*(A(49)-A(57)-A(64)+A(72))*f(6)+(-A(29)-A(30)-A(34)-A(35))*f(7)+CI*(A(39)-A(46))*f(8) &
        +2*A(19)*f(9)+(-A(20)-A(21))*f(9)-18*A(22)*f(10)+(19*(A(23)+A(24))*f(10))/2._/**/REALKIND
  M2( 5) = (A(31)-A(33)+A(76)-A(77)-A(78)-A(79)-A(94)-A(95))*f(3)+CI*(A(96)+A(97)-A(104)-A(105))*f(4)+(-A(25)+A(27)+A(43)+A(53) &
        +A(68)+A(73)+A(74))*f(5)+CI*(-A(52)+A(55)+A(66)-A(69))*f(6)+(A(28)+A(29)+A(35)+A(36))*f(7)+CI*(-A(44)+A(48))*f(8) &
        +2*A(20)*f(9)+(-A(19)-A(21))*f(9)-18*A(23)*f(10)+(19*(A(22)+A(24))*f(10))/2._/**/REALKIND
  M2( 6) = (-A(31)+A(32)-A(76)+A(77)-A(80)-A(81)-A(84)-A(85))*f(3)+CI*(A(86)+A(87)+A(104)+A(105))*f(4)+(A(25)-A(26)+A(38)+A(50) &
        +A(71)-A(73)+A(75))*f(5)+CI*(-A(49)-A(55)-A(66)-A(72))*f(6)+(-A(28)+A(30)+A(34)-A(36))*f(7)+CI*(-A(39)-A(48))*f(8)+(-A(19) &
        -A(20))*f(9)+2*A(21)*f(9)+(19*(A(22)+A(23))*f(10))/2._/**/REALKIND-18*A(24)*f(10)
  M2( 7) = (2*(-A(22)-A(23)-A(24))*f(10))/3._/**/REALKIND
  M2( 8) = (2*(-A(22)-A(23)-A(24))*f(10))/3._/**/REALKIND
  M2( 9) = (2*(-A(22)-A(23)-A(24))*f(10))/3._/**/REALKIND
  M2(10) = ((-A(22)-A(23)-A(24))*f(10))/3._/**/REALKIND
  M2(11) = ((-A(22)-A(23)-A(24))*f(10))/3._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppjjj_bbxggg_1_/**/REALKIND
