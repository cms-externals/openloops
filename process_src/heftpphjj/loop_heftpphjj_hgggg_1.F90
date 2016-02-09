
module ol_colourmatrix_heftpphjj_hgggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(102,6), K2(6,9), KL(6,9)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1(  1,:) = [  38,  -4,  -4,  -4,  -4,   8]
  K1(  2,:) = [  -4,  38,  -4,   8,  -4,  -4]
  K1(  3,:) = [  -4,  -4,  38,  -4,   8,  -4]
  K1(  4,:) = [  -4,   8,  -4,  38,  -4,  -4]
  K1(  5,:) = [  -4,  -4,   8,  -4,  38,  -4]
  K1(  6,:) = [   8,  -4,  -4,  -4,  -4,  38]
  K1(  7,:) = [   0,   0,   0,   0,   0,   0]
  K1(  8,:) = [   0,   0,   0,   0,   0,   0]
  K1(  9,:) = [   0,   0,   0,   0,   0,   0]
  K1( 10,:) = [   0,   0,   0,   0,   0,   0]
  K1( 11,:) = [   0,   0,   0,   0,   0,   0]
  K1( 12,:) = [   0,   0,   0,   0,   0,   0]
  K1( 13,:) = [   0,   0,   0,   0,   0,   0]
  K1( 14,:) = [   0,   0,   0,   0,   0,   0]
  K1( 15,:) = [   0,   0,   0,   0,   0,   0]
  K1( 16,:) = [   0,   0,   0,   0,   0,   0]
  K1( 17,:) = [   0,   0,   0,   0,   0,   0]
  K1( 18,:) = [   0,   0,   0,   0,   0,   0]
  K1( 19,:) = [ 114, -12, -12, -12, -12,  24]
  K1( 20,:) = [ -12, 114, -12,  24, -12, -12]
  K1( 21,:) = [ -12, -12, 114, -12,  24, -12]
  K1( 22,:) = [ -12,  24, -12, 114, -12, -12]
  K1( 23,:) = [ -12, -12,  24, -12, 114, -12]
  K1( 24,:) = [  24, -12, -12, -12, -12, 114]
  K1( 25,:) = [   0,   0,   0,   0,   0,   0]
  K1( 26,:) = [   0,   0,   0,   0,   0,   0]
  K1( 27,:) = [   0,   0,   0,   0,   0,   0]
  K1( 28,:) = [   0,   0,   0,   0,   0,   0]
  K1( 29,:) = [   0,   0,   0,   0,   0,   0]
  K1( 30,:) = [   0,   0,   0,   0,   0,   0]
  K1( 31,:) = [ -65,  -2,   7,  -2,   7, -20]
  K1( 32,:) = [  -2, -65,   7, -20,   7,  -2]
  K1( 33,:) = [   7,   7,  16,   7,  16,   7]
  K1( 34,:) = [  -2, -20,   7, -65,   7,  -2]
  K1( 35,:) = [   7,   7,  16,   7,  16,   7]
  K1( 36,:) = [ -20,  -2,   7,  -2,   7, -65]
  K1( 37,:) = [ 114, -12, -12, -12, -12,  24]
  K1( 38,:) = [ -12, 114, -12,  24, -12, -12]
  K1( 39,:) = [ -12, -12, 114, -12,  24, -12]
  K1( 40,:) = [ -12,  24, -12, 114, -12, -12]
  K1( 41,:) = [ -12, -12,  24, -12, 114, -12]
  K1( 42,:) = [  24, -12, -12, -12, -12, 114]
  K1( 43,:) = [   0,   0,   0,   0,   0,   0]
  K1( 44,:) = [   0,   0,   0,   0,   0,   0]
  K1( 45,:) = [   0,   0,   0,   0,   0,   0]
  K1( 46,:) = [   0,   0,   0,   0,   0,   0]
  K1( 47,:) = [   0,   0,   0,   0,   0,   0]
  K1( 48,:) = [   0,   0,   0,   0,   0,   0]
  K1( 49,:) = [  16,   7,   7,   7,   7,  16]
  K1( 50,:) = [   7, -65,  -2, -20,  -2,   7]
  K1( 51,:) = [   7,  -2, -65,  -2, -20,   7]
  K1( 52,:) = [   7, -20,  -2, -65,  -2,   7]
  K1( 53,:) = [   7,  -2, -20,  -2, -65,   7]
  K1( 54,:) = [  16,   7,   7,   7,   7,  16]
  K1( 55,:) = [ -65,   7,  -2,   7,  -2, -20]
  K1( 56,:) = [   7,  16,   7,  16,   7,   7]
  K1( 57,:) = [  -2,   7, -65,   7, -20,  -2]
  K1( 58,:) = [   7,  16,   7,  16,   7,   7]
  K1( 59,:) = [  -2,   7, -20,   7, -65,  -2]
  K1( 60,:) = [ -20,   7,  -2,   7,  -2, -65]
  K1( 61,:) = [ 114, -12, -12, -12, -12,  24]
  K1( 62,:) = [ -12, 114, -12,  24, -12, -12]
  K1( 63,:) = [ -12, -12, 114, -12,  24, -12]
  K1( 64,:) = [ -12,  24, -12, 114, -12, -12]
  K1( 65,:) = [ -12, -12,  24, -12, 114, -12]
  K1( 66,:) = [  24, -12, -12, -12, -12, 114]
  K1( 67,:) = [   0,   0,   0,   0,   0,   0]
  K1( 68,:) = [   0,   0,   0,   0,   0,   0]
  K1( 69,:) = [   0,   0,   0,   0,   0,   0]
  K1( 70,:) = [   0,   0,   0,   0,   0,   0]
  K1( 71,:) = [   0,   0,   0,   0,   0,   0]
  K1( 72,:) = [   0,   0,   0,   0,   0,   0]
  K1( 73,:) = [ -65,   7,  -2,   7,  -2, -20]
  K1( 74,:) = [   7,  16,   7,  16,   7,   7]
  K1( 75,:) = [  -2,   7, -65,   7, -20,  -2]
  K1( 76,:) = [   7,  16,   7,  16,   7,   7]
  K1( 77,:) = [  -2,   7, -20,   7, -65,  -2]
  K1( 78,:) = [ -20,   7,  -2,   7,  -2, -65]
  K1( 79,:) = [  16,   7,   7,   7,   7,  16]
  K1( 80,:) = [   7, -65,  -2, -20,  -2,   7]
  K1( 81,:) = [   7,  -2, -65,  -2, -20,   7]
  K1( 82,:) = [   7, -20,  -2, -65,  -2,   7]
  K1( 83,:) = [   7,  -2, -20,  -2, -65,   7]
  K1( 84,:) = [  16,   7,   7,   7,   7,  16]
  K1( 85,:) = [ -65,  -2,   7,  -2,   7, -20]
  K1( 86,:) = [  -2, -65,   7, -20,   7,  -2]
  K1( 87,:) = [   7,   7,  16,   7,  16,   7]
  K1( 88,:) = [  -2, -20,   7, -65,   7,  -2]
  K1( 89,:) = [   7,   7,  16,   7,  16,   7]
  K1( 90,:) = [ -20,  -2,   7,  -2,   7, -65]
  K1( 91,:) = [ 114, -12, -12, -12, -12,  24]
  K1( 92,:) = [ -12, 114, -12,  24, -12, -12]
  K1( 93,:) = [ -12, -12, 114, -12,  24, -12]
  K1( 94,:) = [ -12,  24, -12, 114, -12, -12]
  K1( 95,:) = [ -12, -12,  24, -12, 114, -12]
  K1( 96,:) = [  24, -12, -12, -12, -12, 114]
  K1( 97,:) = [   0,   0,   0,   0,   0,   0]
  K1( 98,:) = [   0,   0,   0,   0,   0,   0]
  K1( 99,:) = [   0,   0,   0,   0,   0,   0]
  K1(100,:) = [   0,   0,   0,   0,   0,   0]
  K1(101,:) = [   0,   0,   0,   0,   0,   0]
  K1(102,:) = [   0,   0,   0,   0,   0,   0]
  K1 = (1._/**/REALKIND / 12) * K1

  K2(1,:) = [  8, -1,  8, 19, -2, -2, -2, -2,  4]
  K2(2,:) = [ -1,  8,  8, -2, 19, -2,  4, -2, -2]
  K2(3,:) = [  8,  8, -1, -2, -2, 19, -2,  4, -2]
  K2(4,:) = [ -1,  8,  8, -2,  4, -2, 19, -2, -2]
  K2(5,:) = [  8,  8, -1, -2, -2,  4, -2, 19, -2]
  K2(6,:) = [  8, -1,  8,  4, -2, -2, -2, -2, 19]
  K2 = (1._/**/REALKIND / 6) * K2

  KL(1,:) = [  8, -1,  8, 19, -2, -2, -2, -2,  4]
  KL(2,:) = [ -1,  8,  8, -2, 19, -2,  4, -2, -2]
  KL(3,:) = [  8,  8, -1, -2, -2, 19, -2,  4, -2]
  KL(4,:) = [ -1,  8,  8, -2,  4, -2, 19, -2, -2]
  KL(5,:) = [  8,  8, -1, -2, -2,  4, -2, 19, -2]
  KL(6,:) = [  8, -1,  8,  4, -2, -2, -2, -2, 19]
  KL = (1._/**/REALKIND / 6) * KL

  end subroutine colourmatrix_init
end module ol_colourmatrix_heftpphjj_hgggg_1_/**/REALKIND



module ol_forced_parameters_heftpphjj_hgggg_1_/**/REALKIND
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
  if (nf /= 5) write(*,101) 'nf = 5'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_heftpphjj_hgggg_1_/**/REALKIND

module ol_loop_heftpphjj_hgggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(16), c(12)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:132)
  ! denominators
  complex(REALKIND), save :: den(105)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(6,16)
  ! zero helicity identifier
  logical,           save :: zerohel(16) = .true., zerohel_ct(16) = .true.

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
    f( 1) = (CI*eQED*gQCD**4)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 2) = (CI*countertermnorm*eQED*gQCD**6)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 3) = (CI*countertermnorm*ctHEFTggggh*eQED*gQCD**6)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 4) = (CI*countertermnorm*ctHEFTgggh*eQED*gQCD**6)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 5) = (CI*countertermnorm*ctVVV*eQED*gQCD**6)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 6) = (CI*countertermnorm*ctVVVV*eQED*gQCD**6)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 7) = (CI*countertermnorm*eQED*gQCD**6*R2GGGG)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 8) = (CI*countertermnorm*eQED*gQCD**6*R2HEFTggggh)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 9) = (eQED*gQCD**6*integralnorm*SwB)/(MW*pi**2*sw*48._/**/REALKIND)
    f(10) = (eQED*gQCD**6*integralnorm*SwB)/(MW*pi**2*sw*24._/**/REALKIND)
    f(11) = (CI*eQED*gQCD**4*integralnorm*MB*SwF)/(2._/**/REALKIND*MW*sw)
    f(12) = (eQED*gQCD**4*integralnorm*MB*SwF)/(MW*sw*2._/**/REALKIND)
    f(13) = (CI*eQED*gQCD**6*integralnorm*SwF)/(24._/**/REALKIND*MW*pi**2*sw)
    f(14) = (CI*eQED*gQCD**6*integralnorm*SwF)/(12._/**/REALKIND*MW*pi**2*sw)
    f(15) = (eQED*gQCD**6*integralnorm*SwF)/(MW*pi**2*sw*24._/**/REALKIND)
    f(16) = (eQED*gQCD**6*integralnorm*SwF)/(MW*pi**2*sw*12._/**/REALKIND)

  c = [ 2*f(9), 3*f(9), 6*f(9), 2*f(10), 3*f(10), 6*f(10), CI*f(11), f(12), CI*f(13), CI*f(14), f(15), f(16) ]
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
  complex(REALKIND), intent(out) :: M1(6), M2(9)
  complex(REALKIND) :: A(183)
  ! external WFs
  call wf_S(P(:,1), rMH, H(1), wf(:,0))
  call wf_V(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_V(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_V(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))

  ! internal WFs
  call vert_HGGG_G(wf(:,0),wf(:,-1),wf(:,-2),wf(:,-3),wf(:,1))
  call vert_HGGG_G(wf(:,0),wf(:,-2),wf(:,-3),wf(:,-1),wf(:,2))
  call vert_HGGG_G(wf(:,0),wf(:,-3),wf(:,-1),wf(:,-2),wf(:,3))
  call vert_HG_G(wf(:,0),wf(:,-1),Q(:,2),wf(:,4),Q(:,3))
  call vert_GGG_G(wf(:,-2),wf(:,-3),wf(:,-4),wf(:,5))
  call vert_GGG_G(wf(:,-3),wf(:,-4),wf(:,-2),wf(:,6))
  call vert_GGG_G(wf(:,-4),wf(:,-2),wf(:,-3),wf(:,7))
  call vert_HG_G(wf(:,0),wf(:,-2),Q(:,4),wf(:,8),Q(:,5))
  call vert_GGG_G(wf(:,-1),wf(:,-3),wf(:,-4),wf(:,9))
  call vert_GGG_G(wf(:,-3),wf(:,-4),wf(:,-1),wf(:,10))
  call vert_GGG_G(wf(:,-4),wf(:,-1),wf(:,-3),wf(:,11))
  call vert_HG_G(wf(:,0),wf(:,-3),Q(:,8),wf(:,12),Q(:,9))
  call vert_GGG_G(wf(:,-1),wf(:,-2),wf(:,-4),wf(:,13))
  call vert_GGG_G(wf(:,-2),wf(:,-4),wf(:,-1),wf(:,14))
  call vert_GGG_G(wf(:,-4),wf(:,-1),wf(:,-2),wf(:,15))
  call vert_HG_G(wf(:,0),wf(:,-4),Q(:,16),wf(:,16),Q(:,17))
  call vert_GGG_G(wf(:,-1),wf(:,-2),wf(:,-3),wf(:,17))
  call vert_GGG_G(wf(:,-2),wf(:,-3),wf(:,-1),wf(:,18))
  call vert_GGG_G(wf(:,-3),wf(:,-1),wf(:,-2),wf(:,19))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,-2),Q(:,4),wf(:,20))
  call vert_HGG_G(wf(:,0),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,21),Q(:,25))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,-3),Q(:,8),wf(:,22))
  call vert_HGG_G(wf(:,0),wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,23),Q(:,21))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,-4),Q(:,16),wf(:,24))
  call vert_HGG_G(wf(:,0),wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,25),Q(:,13))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,26))
  call vert_HGG_G(wf(:,0),wf(:,-1),Q(:,2),wf(:,-4),Q(:,16),wf(:,27),Q(:,19))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,28))
  call vert_HGG_G(wf(:,0),wf(:,-1),Q(:,2),wf(:,-3),Q(:,8),wf(:,29),Q(:,11))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,30))
  call vert_HGG_G(wf(:,0),wf(:,-1),Q(:,2),wf(:,-2),Q(:,4),wf(:,31),Q(:,7))
  call vert_UV_W(wf(:,4),Q(:,3),wf(:,-4),Q(:,16),wf(:,32))
  call vert_UV_W(wf(:,4),Q(:,3),wf(:,-3),Q(:,8),wf(:,33))
  call vert_UV_W(wf(:,4),Q(:,3),wf(:,-2),Q(:,4),wf(:,34))
  call vert_UV_W(wf(:,8),Q(:,5),wf(:,-4),Q(:,16),wf(:,35))
  call vert_UV_W(wf(:,8),Q(:,5),wf(:,-3),Q(:,8),wf(:,36))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,8),Q(:,5),wf(:,37))
  call vert_UV_W(wf(:,12),Q(:,9),wf(:,-4),Q(:,16),wf(:,38))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,16),Q(:,17),wf(:,39))
  call vert_HG_G(wf(:,0),wf(:,20),Q(:,6),wf(:,40),Q(:,7))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,12),Q(:,9),wf(:,41))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,12),Q(:,9),wf(:,42))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,16),Q(:,17),wf(:,43))
  call vert_HG_G(wf(:,0),wf(:,22),Q(:,10),wf(:,44),Q(:,11))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,16),Q(:,17),wf(:,45))
  call vert_HG_G(wf(:,0),wf(:,24),Q(:,18),wf(:,46),Q(:,19))
  call counter_HGGG_G(wf(:,0),wf(:,-1),wf(:,-2),wf(:,-3),wf(:,47))
  call counter_HGGG_G(wf(:,0),wf(:,-2),wf(:,-3),wf(:,-1),wf(:,48))
  call counter_HGGG_G(wf(:,0),wf(:,-3),wf(:,-1),wf(:,-2),wf(:,49))
  call counter_GGG_G(wf(:,-2),wf(:,-3),wf(:,-4),wf(:,50))
  call counter_GGG_G(wf(:,-3),wf(:,-4),wf(:,-2),wf(:,51))
  call counter_GGG_G(wf(:,-4),wf(:,-2),wf(:,-3),wf(:,52))
  call counter_GGG_G(wf(:,-1),wf(:,-3),wf(:,-4),wf(:,53))
  call counter_GGG_G(wf(:,-3),wf(:,-4),wf(:,-1),wf(:,54))
  call counter_GGG_G(wf(:,-4),wf(:,-1),wf(:,-3),wf(:,55))
  call counter_GGG_G(wf(:,-1),wf(:,-2),wf(:,-4),wf(:,56))
  call counter_GGG_G(wf(:,-2),wf(:,-4),wf(:,-1),wf(:,57))
  call counter_GGG_G(wf(:,-4),wf(:,-1),wf(:,-2),wf(:,58))
  call counter_GGG_G(wf(:,-1),wf(:,-2),wf(:,-3),wf(:,59))
  call counter_GGG_G(wf(:,-2),wf(:,-3),wf(:,-1),wf(:,60))
  call counter_GGG_G(wf(:,-3),wf(:,-1),wf(:,-2),wf(:,61))
  call counter_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,62))
  call counter_UV_W(wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,63))
  call counter_UV_W(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,64))
  call counter_UV_W(wf(:,-1),Q(:,2),wf(:,-4),Q(:,16),wf(:,65))
  call counter_UV_W(wf(:,-1),Q(:,2),wf(:,-3),Q(:,8),wf(:,66))
  call counter_UV_W(wf(:,-1),Q(:,2),wf(:,-2),Q(:,4),wf(:,67))
  call counter_HG_G(ctHEFTggh,wf(:,0),wf(:,-4),Q(:,16),wf(:,68),Q(:,17))
  call counter_HG_G(ctHEFTggh,wf(:,0),wf(:,-3),Q(:,8),wf(:,69),Q(:,9))
  call counter_HG_G(ctHEFTggh,wf(:,0),wf(:,-2),Q(:,4),wf(:,70),Q(:,5))
  call counter_HG_G(ctHEFTggh,wf(:,0),wf(:,-1),Q(:,2),wf(:,71),Q(:,3))
  call counter_HGG_G(wf(:,0),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,72),Q(:,25))
  call counter_HGG_G(wf(:,0),wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,73),Q(:,21))
  call counter_HGG_G(wf(:,0),wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,74),Q(:,13))
  call counter_HGG_G(wf(:,0),wf(:,-1),Q(:,2),wf(:,-4),Q(:,16),wf(:,75),Q(:,19))
  call counter_HGG_G(wf(:,0),wf(:,-1),Q(:,2),wf(:,-3),Q(:,8),wf(:,76),Q(:,11))
  call counter_HGG_G(wf(:,0),wf(:,-1),Q(:,2),wf(:,-2),Q(:,4),wf(:,77),Q(:,7))
  call counter_UV_W(wf(:,4),Q(:,3),wf(:,-4),Q(:,16),wf(:,78))
  call counter_UV_W(wf(:,4),Q(:,3),wf(:,-3),Q(:,8),wf(:,79))
  call counter_V_V(ctGG,wf(:,4),Q(:,3),wf(:,80))
  call counter_UV_W(wf(:,4),Q(:,3),wf(:,-2),Q(:,4),wf(:,81))
  call counter_UV_W(wf(:,8),Q(:,5),wf(:,-4),Q(:,16),wf(:,82))
  call counter_UV_W(wf(:,8),Q(:,5),wf(:,-3),Q(:,8),wf(:,83))
  call counter_UV_W(wf(:,12),Q(:,9),wf(:,-4),Q(:,16),wf(:,84))
  call counter_UV_W(wf(:,-3),Q(:,8),wf(:,16),Q(:,17),wf(:,85))
  call counter_UV_W(wf(:,-2),Q(:,4),wf(:,12),Q(:,9),wf(:,86))
  call counter_UV_W(wf(:,-2),Q(:,4),wf(:,16),Q(:,17),wf(:,87))
  call counter_V_V(ctGG,wf(:,8),Q(:,5),wf(:,88))
  call counter_V_V(ctGG,wf(:,12),Q(:,9),wf(:,89))
  call counter_V_V(ctGG,wf(:,16),Q(:,17),wf(:,90))
  call counter_UV_W(wf(:,-1),Q(:,2),wf(:,8),Q(:,5),wf(:,91))
  call counter_UV_W(wf(:,-1),Q(:,2),wf(:,12),Q(:,9),wf(:,92))
  call counter_UV_W(wf(:,-1),Q(:,2),wf(:,16),Q(:,17),wf(:,93))
  call vert_HG_G(wf(:,0),wf(:,65),Q(:,18),wf(:,94),Q(:,19))
  call vert_HG_G(wf(:,0),wf(:,66),Q(:,10),wf(:,95),Q(:,11))
  call vert_HG_G(wf(:,0),wf(:,67),Q(:,6),wf(:,96),Q(:,7))
  call counter_V_V(ctGG,wf(:,20),Q(:,6),wf(:,97))
  call counter_V_V(ctGG,wf(:,22),Q(:,10),wf(:,98))
  call counter_V_V(ctGG,wf(:,24),Q(:,18),wf(:,99))
  call counter_V_V(ctGG,wf(:,26),Q(:,12),wf(:,100))
  call counter_V_V(ctGG,wf(:,28),Q(:,20),wf(:,101))
  call counter_V_V(ctGG,wf(:,30),Q(:,24),wf(:,102))
  call counter_HG_G(ctHEFTggh,wf(:,0),wf(:,20),Q(:,6),wf(:,103),Q(:,7))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,68),Q(:,17),wf(:,104))
  call vert_UV_W(wf(:,69),Q(:,9),wf(:,-4),Q(:,16),wf(:,105))
  call counter_HG_G(ctHEFTggh,wf(:,0),wf(:,22),Q(:,10),wf(:,106),Q(:,11))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,68),Q(:,17),wf(:,107))
  call counter_HG_G(ctHEFTggh,wf(:,0),wf(:,24),Q(:,18),wf(:,108),Q(:,19))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,68),Q(:,17),wf(:,109))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,69),Q(:,9),wf(:,110))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,69),Q(:,9),wf(:,111))
  call vert_UV_W(wf(:,70),Q(:,5),wf(:,-4),Q(:,16),wf(:,112))
  call vert_UV_W(wf(:,70),Q(:,5),wf(:,-3),Q(:,8),wf(:,113))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,70),Q(:,5),wf(:,114))
  call vert_UV_W(wf(:,71),Q(:,3),wf(:,-4),Q(:,16),wf(:,115))
  call vert_UV_W(wf(:,71),Q(:,3),wf(:,-3),Q(:,8),wf(:,116))
  call vert_UV_W(wf(:,71),Q(:,3),wf(:,-2),Q(:,4),wf(:,117))
  call vert_UV_W(wf(:,26),Q(:,12),wf(:,-4),Q(:,16),wf(:,118))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,28),Q(:,20),wf(:,119))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,30),Q(:,24),wf(:,120))
  call vert_UV_W(wf(:,22),Q(:,10),wf(:,-4),Q(:,16),wf(:,121))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,24),Q(:,18),wf(:,122))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,30),Q(:,24),wf(:,123))
  call vert_UV_W(wf(:,20),Q(:,6),wf(:,-4),Q(:,16),wf(:,124))
  call vert_UV_W(wf(:,20),Q(:,6),wf(:,-3),Q(:,8),wf(:,125))
  call vert_HG_G(wf(:,0),wf(:,30),Q(:,24),wf(:,126),Q(:,25))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,24),Q(:,18),wf(:,127))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,28),Q(:,20),wf(:,128))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,22),Q(:,10),wf(:,129))
  call vert_HG_G(wf(:,0),wf(:,28),Q(:,20),wf(:,130),Q(:,21))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,26),Q(:,12),wf(:,131))
  call vert_HG_G(wf(:,0),wf(:,26),Q(:,12),wf(:,132),Q(:,13))

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
  den(2) = 1 / (Q(5,5))
  den(3) = 1 / (Q(5,9))
  den(4) = 1 / (Q(5,17))
  den(5) = 1 / (Q(5,6))
  den(6) = 1 / (Q(5,10))
  den(7) = 1 / (Q(5,18))
  den(8) = 1 / (Q(5,12))
  den(9) = 1 / (Q(5,20))
  den(10) = 1 / (Q(5,24))
  den(26) = 1 / (Q(5,7))
  den(27) = 1 / (Q(5,11))
  den(28) = 1 / (Q(5,19))
  den(29) = 1 / (Q(5,13))
  den(30) = 1 / (Q(5,21))
  den(31) = 1 / (Q(5,25))
  den(32) = 1 / (Q(5,14))
  den(33) = 1 / (Q(5,22))
  den(34) = 1 / (Q(5,26))
  den(35) = 1 / (Q(5,28))

  ! denominators
  den(11) = den(1)*den(8)
  den(12) = den(1)*den(9)
  den(13) = den(1)*den(10)
  den(14) = den(2)*den(6)
  den(15) = den(2)*den(7)
  den(16) = den(2)*den(10)
  den(17) = den(3)*den(5)
  den(18) = den(4)*den(5)
  den(19) = den(5)*den(10)
  den(20) = den(3)*den(7)
  den(21) = den(3)*den(9)
  den(22) = den(4)*den(6)
  den(23) = den(6)*den(9)
  den(24) = den(4)*den(8)
  den(25) = den(7)*den(8)
  den(36) = den(1)*den(35)
  den(37) = den(2)*den(34)
  den(38) = den(3)*den(33)
  den(39) = den(4)*den(32)
  den(40) = den(5)*den(31)
  den(41) = den(6)*den(30)
  den(42) = den(7)*den(29)
  den(43) = den(8)*den(28)
  den(44) = den(9)*den(27)
  den(45) = den(10)*den(26)
  den(46) = den(8)*den(35)
  den(47) = den(1)*den(46)
  den(48) = den(1)*den(28)
  den(49) = den(8)*den(48)
  den(50) = den(9)*den(35)
  den(51) = den(1)*den(50)
  den(52) = den(1)*den(27)
  den(53) = den(9)*den(52)
  den(54) = den(10)*den(35)
  den(55) = den(1)*den(54)
  den(56) = den(1)*den(26)
  den(57) = den(10)*den(56)
  den(58) = den(6)*den(34)
  den(59) = den(2)*den(58)
  den(60) = den(2)*den(30)
  den(61) = den(6)*den(60)
  den(62) = den(7)*den(34)
  den(63) = den(2)*den(62)
  den(64) = den(2)*den(29)
  den(65) = den(7)*den(64)
  den(66) = den(10)*den(34)
  den(67) = den(2)*den(66)
  den(68) = den(2)*den(26)
  den(69) = den(10)*den(68)
  den(70) = den(5)*den(33)
  den(71) = den(3)*den(70)
  den(72) = den(3)*den(31)
  den(73) = den(5)*den(72)
  den(74) = den(5)*den(32)
  den(75) = den(4)*den(74)
  den(76) = den(4)*den(31)
  den(77) = den(5)*den(76)
  den(78) = den(10)*den(31)
  den(79) = den(5)*den(78)
  den(80) = den(5)*den(26)
  den(81) = den(10)*den(80)
  den(82) = den(7)*den(33)
  den(83) = den(3)*den(82)
  den(84) = den(3)*den(29)
  den(85) = den(7)*den(84)
  den(86) = den(9)*den(33)
  den(87) = den(3)*den(86)
  den(88) = den(3)*den(27)
  den(89) = den(9)*den(88)
  den(90) = den(6)*den(32)
  den(91) = den(4)*den(90)
  den(92) = den(4)*den(30)
  den(93) = den(6)*den(92)
  den(94) = den(9)*den(30)
  den(95) = den(6)*den(94)
  den(96) = den(6)*den(27)
  den(97) = den(9)*den(96)
  den(98) = den(8)*den(32)
  den(99) = den(4)*den(98)
  den(100) = den(4)*den(28)
  den(101) = den(8)*den(100)
  den(102) = den(8)*den(29)
  den(103) = den(7)*den(102)
  den(104) = den(7)*den(28)
  den(105) = den(8)*den(104)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(183)

  A(1) = cont_VV(wf(:,-4),wf(:,1))
  A(2) = cont_VV(wf(:,-4),wf(:,2))
  A(3) = cont_VV(wf(:,-4),wf(:,3))
  A(4) = cont_VV(wf(:,4),wf(:,5)) * den(1)
  A(5) = cont_VV(wf(:,4),wf(:,6)) * den(1)
  A(6) = cont_VV(wf(:,4),wf(:,7)) * den(1)
  A(7) = cont_VV(wf(:,8),wf(:,9)) * den(2)
  A(8) = cont_VV(wf(:,8),wf(:,10)) * den(2)
  A(9) = cont_VV(wf(:,8),wf(:,11)) * den(2)
  A(10) = cont_VV(wf(:,12),wf(:,13)) * den(3)
  A(11) = cont_VV(wf(:,12),wf(:,14)) * den(3)
  A(12) = cont_VV(wf(:,12),wf(:,15)) * den(3)
  A(13) = cont_VV(wf(:,16),wf(:,17)) * den(4)
  A(14) = cont_VV(wf(:,16),wf(:,18)) * den(4)
  A(15) = cont_VV(wf(:,16),wf(:,19)) * den(4)
  A(16) = cont_VV(wf(:,20),wf(:,21)) * den(5)
  A(17) = cont_VV(wf(:,22),wf(:,23)) * den(6)
  A(18) = cont_VV(wf(:,24),wf(:,25)) * den(7)
  A(19) = cont_VV(wf(:,26),wf(:,27)) * den(8)
  A(20) = cont_VV(wf(:,28),wf(:,29)) * den(9)
  A(21) = cont_VV(wf(:,30),wf(:,31)) * den(10)
  A(22) = cont_VV(wf(:,26),wf(:,32)) * den(11)
  A(23) = cont_VV(wf(:,28),wf(:,33)) * den(12)
  A(24) = cont_VV(wf(:,30),wf(:,34)) * den(13)
  A(25) = cont_VV(wf(:,22),wf(:,35)) * den(14)
  A(26) = cont_VV(wf(:,24),wf(:,36)) * den(15)
  A(27) = cont_VV(wf(:,30),wf(:,37)) * den(16)
  A(28) = cont_VV(wf(:,20),wf(:,38)) * den(17)
  A(29) = cont_VV(wf(:,20),wf(:,39)) * den(18)
  A(30) = cont_VV(wf(:,30),wf(:,40)) * den(19)
  A(31) = cont_VV(wf(:,24),wf(:,41)) * den(20)
  A(32) = cont_VV(wf(:,28),wf(:,42)) * den(21)
  A(33) = cont_VV(wf(:,22),wf(:,43)) * den(22)
  A(34) = cont_VV(wf(:,28),wf(:,44)) * den(23)
  A(35) = cont_VV(wf(:,26),wf(:,45)) * den(24)
  A(36) = cont_VV(wf(:,26),wf(:,46)) * den(25)

  A(37) = cont_VV(wf(:,-4),wf(:,47))
  A(38) = cont_VV(wf(:,-4),wf(:,48))
  A(39) = cont_VV(wf(:,-4),wf(:,49))
  A(40) = cont_VV(wf(:,-4),wf(:,47))
  A(41) = cont_VV(wf(:,-4),wf(:,48))
  A(42) = cont_VV(wf(:,-4),wf(:,49))
  A(43) = cont_VV(wf(:,4),wf(:,50)) * den(1)
  A(44) = cont_VV(wf(:,4),wf(:,51)) * den(1)
  A(45) = cont_VV(wf(:,4),wf(:,52)) * den(1)
  A(46) = cont_VV(wf(:,4),wf(:,50)) * den(1)
  A(47) = cont_VV(wf(:,4),wf(:,51)) * den(1)
  A(48) = cont_VV(wf(:,4),wf(:,52)) * den(1)
  A(49) = cont_VV(wf(:,8),wf(:,53)) * den(2)
  A(50) = cont_VV(wf(:,8),wf(:,54)) * den(2)
  A(51) = cont_VV(wf(:,8),wf(:,55)) * den(2)
  A(52) = cont_VV(wf(:,8),wf(:,53)) * den(2)
  A(53) = cont_VV(wf(:,8),wf(:,54)) * den(2)
  A(54) = cont_VV(wf(:,8),wf(:,55)) * den(2)
  A(55) = cont_VV(wf(:,12),wf(:,56)) * den(3)
  A(56) = cont_VV(wf(:,12),wf(:,57)) * den(3)
  A(57) = cont_VV(wf(:,12),wf(:,58)) * den(3)
  A(58) = cont_VV(wf(:,12),wf(:,56)) * den(3)
  A(59) = cont_VV(wf(:,12),wf(:,57)) * den(3)
  A(60) = cont_VV(wf(:,12),wf(:,58)) * den(3)
  A(61) = cont_VV(wf(:,16),wf(:,59)) * den(4)
  A(62) = cont_VV(wf(:,16),wf(:,60)) * den(4)
  A(63) = cont_VV(wf(:,16),wf(:,61)) * den(4)
  A(64) = cont_VV(wf(:,16),wf(:,59)) * den(4)
  A(65) = cont_VV(wf(:,16),wf(:,60)) * den(4)
  A(66) = cont_VV(wf(:,16),wf(:,61)) * den(4)
  A(67) = cont_VV(wf(:,31),wf(:,62)) * den(26)
  A(68) = cont_VV(wf(:,29),wf(:,63)) * den(27)
  A(69) = cont_VV(wf(:,27),wf(:,64)) * den(28)
  A(70) = cont_VV(wf(:,25),wf(:,65)) * den(29)
  A(71) = cont_VV(wf(:,23),wf(:,66)) * den(30)
  A(72) = cont_VV(wf(:,21),wf(:,67)) * den(31)
  A(73) = cont_VV(wf(:,17),wf(:,68)) * den(32)
  A(74) = cont_VV(wf(:,18),wf(:,68)) * den(32)
  A(75) = cont_VV(wf(:,19),wf(:,68)) * den(32)
  A(76) = cont_VV(wf(:,13),wf(:,69)) * den(33)
  A(77) = cont_VV(wf(:,14),wf(:,69)) * den(33)
  A(78) = cont_VV(wf(:,15),wf(:,69)) * den(33)
  A(79) = cont_VV(wf(:,9),wf(:,70)) * den(34)
  A(80) = cont_VV(wf(:,10),wf(:,70)) * den(34)
  A(81) = cont_VV(wf(:,11),wf(:,70)) * den(34)
  A(82) = cont_VV(wf(:,5),wf(:,71)) * den(35)
  A(83) = cont_VV(wf(:,6),wf(:,71)) * den(35)
  A(84) = cont_VV(wf(:,7),wf(:,71)) * den(35)
  A(85) = cont_VV(wf(:,20),wf(:,72)) * den(5)
  A(86) = cont_VV(wf(:,22),wf(:,73)) * den(6)
  A(87) = cont_VV(wf(:,24),wf(:,74)) * den(7)
  A(88) = cont_VV(wf(:,26),wf(:,75)) * den(8)
  A(89) = cont_VV(wf(:,28),wf(:,76)) * den(9)
  A(90) = cont_VV(wf(:,30),wf(:,77)) * den(10)
  A(91) = cont_VV(wf(:,26),wf(:,78)) * den(11)
  A(92) = cont_VV(wf(:,28),wf(:,79)) * den(12)
  A(93) = cont_VV(wf(:,34),wf(:,62)) * den(13)
  A(94) = cont_VV(wf(:,5),wf(:,80)) * den(36)
  A(95) = cont_VV(wf(:,6),wf(:,80)) * den(36)
  A(96) = cont_VV(wf(:,7),wf(:,80)) * den(36)
  A(97) = cont_VV(wf(:,30),wf(:,81)) * den(13)
  A(98) = cont_VV(wf(:,33),wf(:,63)) * den(12)
  A(99) = cont_VV(wf(:,32),wf(:,64)) * den(11)
  A(100) = cont_VV(wf(:,22),wf(:,82)) * den(14)
  A(101) = cont_VV(wf(:,24),wf(:,83)) * den(15)
  A(102) = cont_VV(wf(:,37),wf(:,62)) * den(16)
  A(103) = cont_VV(wf(:,20),wf(:,84)) * den(17)
  A(104) = cont_VV(wf(:,20),wf(:,85)) * den(18)
  A(105) = cont_VV(wf(:,40),wf(:,62)) * den(19)
  A(106) = cont_VV(wf(:,24),wf(:,86)) * den(20)
  A(107) = cont_VV(wf(:,42),wf(:,63)) * den(21)
  A(108) = cont_VV(wf(:,22),wf(:,87)) * den(22)
  A(109) = cont_VV(wf(:,44),wf(:,63)) * den(23)
  A(110) = cont_VV(wf(:,45),wf(:,64)) * den(24)
  A(111) = cont_VV(wf(:,46),wf(:,64)) * den(25)
  A(112) = cont_VV(wf(:,9),wf(:,88)) * den(37)
  A(113) = cont_VV(wf(:,10),wf(:,88)) * den(37)
  A(114) = cont_VV(wf(:,11),wf(:,88)) * den(37)
  A(115) = cont_VV(wf(:,13),wf(:,89)) * den(38)
  A(116) = cont_VV(wf(:,14),wf(:,89)) * den(38)
  A(117) = cont_VV(wf(:,15),wf(:,89)) * den(38)
  A(118) = cont_VV(wf(:,17),wf(:,90)) * den(39)
  A(119) = cont_VV(wf(:,18),wf(:,90)) * den(39)
  A(120) = cont_VV(wf(:,19),wf(:,90)) * den(39)
  A(121) = cont_VV(wf(:,30),wf(:,91)) * den(16)
  A(122) = cont_VV(wf(:,36),wf(:,65)) * den(15)
  A(123) = cont_VV(wf(:,35),wf(:,66)) * den(14)
  A(124) = cont_VV(wf(:,28),wf(:,92)) * den(21)
  A(125) = cont_VV(wf(:,41),wf(:,65)) * den(20)
  A(126) = cont_VV(wf(:,26),wf(:,93)) * den(24)
  A(127) = cont_VV(wf(:,26),wf(:,94)) * den(25)
  A(128) = cont_VV(wf(:,43),wf(:,66)) * den(22)
  A(129) = cont_VV(wf(:,28),wf(:,95)) * den(23)
  A(130) = cont_VV(wf(:,38),wf(:,67)) * den(17)
  A(131) = cont_VV(wf(:,39),wf(:,67)) * den(18)
  A(132) = cont_VV(wf(:,30),wf(:,96)) * den(19)
  A(133) = cont_VV(wf(:,21),wf(:,97)) * den(40)
  A(134) = cont_VV(wf(:,23),wf(:,98)) * den(41)
  A(135) = cont_VV(wf(:,25),wf(:,99)) * den(42)
  A(136) = cont_VV(wf(:,27),wf(:,100)) * den(43)
  A(137) = cont_VV(wf(:,29),wf(:,101)) * den(44)
  A(138) = cont_VV(wf(:,31),wf(:,102)) * den(45)
  A(139) = cont_VV(wf(:,30),wf(:,103)) * den(19)
  A(140) = cont_VV(wf(:,20),wf(:,104)) * den(18)
  A(141) = cont_VV(wf(:,20),wf(:,105)) * den(17)
  A(142) = cont_VV(wf(:,28),wf(:,106)) * den(23)
  A(143) = cont_VV(wf(:,22),wf(:,107)) * den(22)
  A(144) = cont_VV(wf(:,26),wf(:,108)) * den(25)
  A(145) = cont_VV(wf(:,26),wf(:,109)) * den(24)
  A(146) = cont_VV(wf(:,24),wf(:,110)) * den(20)
  A(147) = cont_VV(wf(:,28),wf(:,111)) * den(21)
  A(148) = cont_VV(wf(:,22),wf(:,112)) * den(14)
  A(149) = cont_VV(wf(:,24),wf(:,113)) * den(15)
  A(150) = cont_VV(wf(:,30),wf(:,114)) * den(16)
  A(151) = cont_VV(wf(:,26),wf(:,115)) * den(11)
  A(152) = cont_VV(wf(:,28),wf(:,116)) * den(12)
  A(153) = cont_VV(wf(:,30),wf(:,117)) * den(13)
  A(154) = cont_VV(wf(:,80),wf(:,118)) * den(47)
  A(155) = cont_VV(wf(:,32),wf(:,100)) * den(49)
  A(156) = cont_VV(wf(:,80),wf(:,119)) * den(51)
  A(157) = cont_VV(wf(:,33),wf(:,101)) * den(53)
  A(158) = cont_VV(wf(:,80),wf(:,120)) * den(55)
  A(159) = cont_VV(wf(:,34),wf(:,102)) * den(57)
  A(160) = cont_VV(wf(:,88),wf(:,121)) * den(59)
  A(161) = cont_VV(wf(:,35),wf(:,98)) * den(61)
  A(162) = cont_VV(wf(:,88),wf(:,122)) * den(63)
  A(163) = cont_VV(wf(:,36),wf(:,99)) * den(65)
  A(164) = cont_VV(wf(:,88),wf(:,123)) * den(67)
  A(165) = cont_VV(wf(:,37),wf(:,102)) * den(69)
  A(166) = cont_VV(wf(:,89),wf(:,124)) * den(71)
  A(167) = cont_VV(wf(:,38),wf(:,97)) * den(73)
  A(168) = cont_VV(wf(:,90),wf(:,125)) * den(75)
  A(169) = cont_VV(wf(:,39),wf(:,97)) * den(77)
  A(170) = cont_VV(wf(:,97),wf(:,126)) * den(79)
  A(171) = cont_VV(wf(:,40),wf(:,102)) * den(81)
  A(172) = cont_VV(wf(:,89),wf(:,127)) * den(83)
  A(173) = cont_VV(wf(:,41),wf(:,99)) * den(85)
  A(174) = cont_VV(wf(:,89),wf(:,128)) * den(87)
  A(175) = cont_VV(wf(:,42),wf(:,101)) * den(89)
  A(176) = cont_VV(wf(:,90),wf(:,129)) * den(91)
  A(177) = cont_VV(wf(:,43),wf(:,98)) * den(93)
  A(178) = cont_VV(wf(:,98),wf(:,130)) * den(95)
  A(179) = cont_VV(wf(:,44),wf(:,101)) * den(97)
  A(180) = cont_VV(wf(:,90),wf(:,131)) * den(99)
  A(181) = cont_VV(wf(:,45),wf(:,100)) * den(101)
  A(182) = cont_VV(wf(:,99),wf(:,132)) * den(103)
  A(183) = cont_VV(wf(:,46),wf(:,100)) * den(105)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(183)
  complex(REALKIND), intent(out) :: M1(6), M2(9)

  M1(1) = 2*(A(1)-A(2)+A(4)-A(5)+A(8)-A(9)-A(10)+A(12)+A(13)-A(14)+A(16)-A(18)-A(19)+A(21)-A(22)+A(24)-A(26)+A(27)+A(28)+A(29) &
       +A(30)-A(31)-A(35)-A(36))*f(1)
  M1(2) = 2*(-A(1)+A(3)+A(5)-A(6)+A(7)-A(8)+A(10)-A(11)-A(13)+A(15)-A(16)-A(17)-A(20)-A(21)-A(23)-A(24)-A(25)-A(27)-A(28)-A(29) &
       -A(30)-A(32)-A(33)-A(34))*f(1)
  M1(3) = 2*(A(2)-A(3)-A(4)+A(6)-A(7)+A(9)+A(11)-A(12)+A(14)-A(15)+A(17)+A(18)+A(19)+A(20)+A(22)+A(23)+A(25)+A(26)+A(31)+A(32) &
       +A(33)+A(34)+A(35)+A(36))*f(1)
  M1(4) = 2*(-A(1)+A(3)+A(5)-A(6)+A(7)-A(8)+A(10)-A(11)-A(13)+A(15)-A(16)-A(17)-A(20)-A(21)-A(23)-A(24)-A(25)-A(27)-A(28)-A(29) &
       -A(30)-A(32)-A(33)-A(34))*f(1)
  M1(5) = 2*(A(2)-A(3)-A(4)+A(6)-A(7)+A(9)+A(11)-A(12)+A(14)-A(15)+A(17)+A(18)+A(19)+A(20)+A(22)+A(23)+A(25)+A(26)+A(31)+A(32) &
       +A(33)+A(34)+A(35)+A(36))*f(1)
  M1(6) = 2*(A(1)-A(2)+A(4)-A(5)+A(8)-A(9)-A(10)+A(12)+A(13)-A(14)+A(16)-A(18)-A(19)+A(21)-A(22)+A(24)-A(26)+A(27)+A(28)+A(29) &
       +A(30)-A(31)-A(35)-A(36))*f(1)

  M2(1) = (4*(-A(46)-A(47)-A(48)-A(52)-A(53)-A(54)-A(58)-A(59)-A(60)-A(64)-A(65)-A(66))*f(7))/3._/**/REALKIND+2*(-A(40)-A(41) &
       -A(42))*f(8)
  M2(2) = (4*(-A(46)-A(47)-A(48)-A(52)-A(53)-A(54)-A(58)-A(59)-A(60)-A(64)-A(65)-A(66))*f(7))/3._/**/REALKIND+2*(-A(40)-A(41) &
       -A(42))*f(8)
  M2(3) = (4*(-A(46)-A(47)-A(48)-A(52)-A(53)-A(54)-A(58)-A(59)-A(60)-A(64)-A(65)-A(66))*f(7))/3._/**/REALKIND+2*(-A(40)-A(41) &
       -A(42))*f(8)
  M2(4) = 2*(A(73)-A(74)-A(76)+A(78)+A(80)-A(81)+A(82)-A(83)-A(94)+A(95)-A(113)+A(114)+A(115)-A(117)-A(118)+A(119)-A(133)+A(135) &
       +A(136)-A(138)+A(139)+A(140)+A(141)-A(144)-A(145)-A(146)-A(149)+A(150)-A(151)+A(153)-A(154)+A(155)-A(158)-A(159)+A(162) &
       +A(163)+A(164)-A(165)+A(166)-A(167)-A(168)-A(169)-A(170)-A(171)-A(172)+A(173)-A(180)+A(181)+A(182)+A(183))*f(2)+2*(-A(37) &
       -A(38))*f(3)+4*A(39)*f(3)+2*(A(85)-A(87)-A(88)+A(90))*f(4)+2*(A(67)-A(69)-A(70)+A(72)-A(91)+A(93)+A(97)-A(99)-A(101)+A(102) &
       +A(103)+A(104)+A(105)-A(106)-A(110)-A(111)+A(121)-A(122)-A(125)-A(126)-A(127)+A(130)+A(131)+A(132))*f(5)+2*(-A(43)-A(44) &
       -A(50)-A(51)-A(55)-A(57)-A(61)-A(62))*f(6)+4*(A(45)+A(49)+A(56)+A(63))*f(6)+17*(A(46)+A(47)+A(53)+A(54)+A(58)+A(60)+A(64) &
       +A(65))*f(7)+(98*(-A(48)-A(52)-A(59)-A(66))*f(7))/3._/**/REALKIND+63*(-A(40)-A(41))*f(8)+123*A(42)*f(8)
  M2(5) = 2*(-A(73)+A(75)+A(76)-A(77)+A(79)-A(80)+A(83)-A(84)-A(95)+A(96)-A(112)+A(113)-A(115)+A(116)+A(118)-A(120)+A(133)+A(134) &
       +A(137)+A(138)-A(139)-A(140)-A(141)-A(142)-A(143)-A(147)-A(148)-A(150)-A(152)-A(153)+A(156)+A(157)+A(158)+A(159)-A(160) &
       +A(161)-A(164)+A(165)-A(166)+A(167)+A(168)+A(169)+A(170)+A(171)-A(174)+A(175)-A(176)+A(177)+A(178)+A(179))*f(2) &
       +4*A(38)*f(3)+2*(-A(37)-A(39))*f(3)+2*(-A(85)-A(86)-A(89)-A(90))*f(4)+2*(-A(67)-A(68)-A(71)-A(72)-A(92)-A(93)-A(97)-A(98) &
       -A(100)-A(102)-A(103)-A(104)-A(105)-A(107)-A(108)-A(109)-A(121)-A(123)-A(124)-A(128)-A(129)-A(130)-A(131)-A(132))*f(5) &
       +4*(A(43)+A(51)+A(57)+A(62))*f(6)+2*(-A(44)-A(45)-A(49)-A(50)-A(55)-A(56)-A(61)-A(63))*f(6)+(98*(-A(46)-A(54)-A(60) &
       -A(65))*f(7))/3._/**/REALKIND+17*(A(47)+A(48)+A(52)+A(53)+A(58)+A(59)+A(64)+A(66))*f(7)+123*A(41)*f(8)+63*(-A(40) &
       -A(42))*f(8)
  M2(6) = 2*(A(74)-A(75)+A(77)-A(78)-A(79)+A(81)-A(82)+A(84)+A(94)-A(96)+A(112)-A(114)-A(116)+A(117)-A(119)+A(120)-A(134)-A(135) &
       -A(136)-A(137)+A(142)+A(143)+A(144)+A(145)+A(146)+A(147)+A(148)+A(149)+A(151)+A(152)+A(154)-A(155)-A(156)-A(157)+A(160) &
       -A(161)-A(162)-A(163)+A(172)-A(173)+A(174)-A(175)+A(176)-A(177)-A(178)-A(179)+A(180)-A(181)-A(182)-A(183))*f(2) &
       +4*A(37)*f(3)+2*(-A(38)-A(39))*f(3)+2*(A(86)+A(87)+A(88)+A(89))*f(4)+2*(A(68)+A(69)+A(70)+A(71)+A(91)+A(92)+A(98)+A(99) &
       +A(100)+A(101)+A(106)+A(107)+A(108)+A(109)+A(110)+A(111)+A(122)+A(123)+A(124)+A(125)+A(126)+A(127)+A(128)+A(129))*f(5) &
       +4*(A(44)+A(50)+A(55)+A(61))*f(6)+2*(-A(43)-A(45)-A(49)-A(51)-A(56)-A(57)-A(62)-A(63))*f(6)+(98*(-A(47)-A(53)-A(58) &
       -A(64))*f(7))/3._/**/REALKIND+17*(A(46)+A(48)+A(52)+A(54)+A(59)+A(60)+A(65)+A(66))*f(7)+123*A(40)*f(8)+63*(-A(41) &
       -A(42))*f(8)
  M2(7) = 2*(-A(73)+A(75)+A(76)-A(77)+A(79)-A(80)+A(83)-A(84)-A(95)+A(96)-A(112)+A(113)-A(115)+A(116)+A(118)-A(120)+A(133)+A(134) &
       +A(137)+A(138)-A(139)-A(140)-A(141)-A(142)-A(143)-A(147)-A(148)-A(150)-A(152)-A(153)+A(156)+A(157)+A(158)+A(159)-A(160) &
       +A(161)-A(164)+A(165)-A(166)+A(167)+A(168)+A(169)+A(170)+A(171)-A(174)+A(175)-A(176)+A(177)+A(178)+A(179))*f(2) &
       +4*A(38)*f(3)+2*(-A(37)-A(39))*f(3)+2*(-A(85)-A(86)-A(89)-A(90))*f(4)+2*(-A(67)-A(68)-A(71)-A(72)-A(92)-A(93)-A(97)-A(98) &
       -A(100)-A(102)-A(103)-A(104)-A(105)-A(107)-A(108)-A(109)-A(121)-A(123)-A(124)-A(128)-A(129)-A(130)-A(131)-A(132))*f(5) &
       +4*(A(43)+A(51)+A(57)+A(62))*f(6)+2*(-A(44)-A(45)-A(49)-A(50)-A(55)-A(56)-A(61)-A(63))*f(6)+(98*(-A(46)-A(54)-A(60) &
       -A(65))*f(7))/3._/**/REALKIND+17*(A(47)+A(48)+A(52)+A(53)+A(58)+A(59)+A(64)+A(66))*f(7)+123*A(41)*f(8)+63*(-A(40) &
       -A(42))*f(8)
  M2(8) = 2*(A(74)-A(75)+A(77)-A(78)-A(79)+A(81)-A(82)+A(84)+A(94)-A(96)+A(112)-A(114)-A(116)+A(117)-A(119)+A(120)-A(134)-A(135) &
       -A(136)-A(137)+A(142)+A(143)+A(144)+A(145)+A(146)+A(147)+A(148)+A(149)+A(151)+A(152)+A(154)-A(155)-A(156)-A(157)+A(160) &
       -A(161)-A(162)-A(163)+A(172)-A(173)+A(174)-A(175)+A(176)-A(177)-A(178)-A(179)+A(180)-A(181)-A(182)-A(183))*f(2) &
       +4*A(37)*f(3)+2*(-A(38)-A(39))*f(3)+2*(A(86)+A(87)+A(88)+A(89))*f(4)+2*(A(68)+A(69)+A(70)+A(71)+A(91)+A(92)+A(98)+A(99) &
       +A(100)+A(101)+A(106)+A(107)+A(108)+A(109)+A(110)+A(111)+A(122)+A(123)+A(124)+A(125)+A(126)+A(127)+A(128)+A(129))*f(5) &
       +4*(A(44)+A(50)+A(55)+A(61))*f(6)+2*(-A(43)-A(45)-A(49)-A(51)-A(56)-A(57)-A(62)-A(63))*f(6)+(98*(-A(47)-A(53)-A(58) &
       -A(64))*f(7))/3._/**/REALKIND+17*(A(46)+A(48)+A(52)+A(54)+A(59)+A(60)+A(65)+A(66))*f(7)+123*A(40)*f(8)+63*(-A(41) &
       -A(42))*f(8)
  M2(9) = 2*(A(73)-A(74)-A(76)+A(78)+A(80)-A(81)+A(82)-A(83)-A(94)+A(95)-A(113)+A(114)+A(115)-A(117)-A(118)+A(119)-A(133)+A(135) &
       +A(136)-A(138)+A(139)+A(140)+A(141)-A(144)-A(145)-A(146)-A(149)+A(150)-A(151)+A(153)-A(154)+A(155)-A(158)-A(159)+A(162) &
       +A(163)+A(164)-A(165)+A(166)-A(167)-A(168)-A(169)-A(170)-A(171)-A(172)+A(173)-A(180)+A(181)+A(182)+A(183))*f(2)+2*(-A(37) &
       -A(38))*f(3)+4*A(39)*f(3)+2*(A(85)-A(87)-A(88)+A(90))*f(4)+2*(A(67)-A(69)-A(70)+A(72)-A(91)+A(93)+A(97)-A(99)-A(101)+A(102) &
       +A(103)+A(104)+A(105)-A(106)-A(110)-A(111)+A(121)-A(122)-A(125)-A(126)-A(127)+A(130)+A(131)+A(132))*f(5)+2*(-A(43)-A(44) &
       -A(50)-A(51)-A(55)-A(57)-A(61)-A(62))*f(6)+4*(A(45)+A(49)+A(56)+A(63))*f(6)+17*(A(46)+A(47)+A(53)+A(54)+A(58)+A(60)+A(64) &
       +A(65))*f(7)+(98*(-A(48)-A(52)-A(59)-A(66))*f(7))/3._/**/REALKIND+63*(-A(40)-A(41))*f(8)+123*A(42)*f(8)

end subroutine colourvectors

end module ol_loop_heftpphjj_hgggg_1_/**/REALKIND
