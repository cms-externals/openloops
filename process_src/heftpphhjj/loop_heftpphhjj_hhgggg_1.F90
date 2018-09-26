
module ol_colourmatrix_heftpphhjj_hhgggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(138,6), K2(6,9), KL(6,9)
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
  K1( 19,:) = [   0,   0,   0,   0,   0,   0]
  K1( 20,:) = [   0,   0,   0,   0,   0,   0]
  K1( 21,:) = [   0,   0,   0,   0,   0,   0]
  K1( 22,:) = [   0,   0,   0,   0,   0,   0]
  K1( 23,:) = [   0,   0,   0,   0,   0,   0]
  K1( 24,:) = [   0,   0,   0,   0,   0,   0]
  K1( 25,:) = [   0,   0,   0,   0,   0,   0]
  K1( 26,:) = [   0,   0,   0,   0,   0,   0]
  K1( 27,:) = [   0,   0,   0,   0,   0,   0]
  K1( 28,:) = [   0,   0,   0,   0,   0,   0]
  K1( 29,:) = [   0,   0,   0,   0,   0,   0]
  K1( 30,:) = [   0,   0,   0,   0,   0,   0]
  K1( 31,:) = [   0,   0,   0,   0,   0,   0]
  K1( 32,:) = [   0,   0,   0,   0,   0,   0]
  K1( 33,:) = [   0,   0,   0,   0,   0,   0]
  K1( 34,:) = [   0,   0,   0,   0,   0,   0]
  K1( 35,:) = [   0,   0,   0,   0,   0,   0]
  K1( 36,:) = [   0,   0,   0,   0,   0,   0]
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
  K1( 49,:) = [   0,   0,   0,   0,   0,   0]
  K1( 50,:) = [   0,   0,   0,   0,   0,   0]
  K1( 51,:) = [   0,   0,   0,   0,   0,   0]
  K1( 52,:) = [   0,   0,   0,   0,   0,   0]
  K1( 53,:) = [   0,   0,   0,   0,   0,   0]
  K1( 54,:) = [   0,   0,   0,   0,   0,   0]
  K1( 55,:) = [ -65,  -2,   7,  -2,   7, -20]
  K1( 56,:) = [  -2, -65,   7, -20,   7,  -2]
  K1( 57,:) = [   7,   7,  16,   7,  16,   7]
  K1( 58,:) = [  -2, -20,   7, -65,   7,  -2]
  K1( 59,:) = [   7,   7,  16,   7,  16,   7]
  K1( 60,:) = [ -20,  -2,   7,  -2,   7, -65]
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
  K1( 73,:) = [   0,   0,   0,   0,   0,   0]
  K1( 74,:) = [   0,   0,   0,   0,   0,   0]
  K1( 75,:) = [   0,   0,   0,   0,   0,   0]
  K1( 76,:) = [   0,   0,   0,   0,   0,   0]
  K1( 77,:) = [   0,   0,   0,   0,   0,   0]
  K1( 78,:) = [   0,   0,   0,   0,   0,   0]
  K1( 79,:) = [  16,   7,   7,   7,   7,  16]
  K1( 80,:) = [   7, -65,  -2, -20,  -2,   7]
  K1( 81,:) = [   7,  -2, -65,  -2, -20,   7]
  K1( 82,:) = [   7, -20,  -2, -65,  -2,   7]
  K1( 83,:) = [   7,  -2, -20,  -2, -65,   7]
  K1( 84,:) = [  16,   7,   7,   7,   7,  16]
  K1( 85,:) = [ -65,   7,  -2,   7,  -2, -20]
  K1( 86,:) = [   7,  16,   7,  16,   7,   7]
  K1( 87,:) = [  -2,   7, -65,   7, -20,  -2]
  K1( 88,:) = [   7,  16,   7,  16,   7,   7]
  K1( 89,:) = [  -2,   7, -20,   7, -65,  -2]
  K1( 90,:) = [ -20,   7,  -2,   7,  -2, -65]
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
  K1(103,:) = [   0,   0,   0,   0,   0,   0]
  K1(104,:) = [   0,   0,   0,   0,   0,   0]
  K1(105,:) = [   0,   0,   0,   0,   0,   0]
  K1(106,:) = [   0,   0,   0,   0,   0,   0]
  K1(107,:) = [   0,   0,   0,   0,   0,   0]
  K1(108,:) = [   0,   0,   0,   0,   0,   0]
  K1(109,:) = [ -65,   7,  -2,   7,  -2, -20]
  K1(110,:) = [   7,  16,   7,  16,   7,   7]
  K1(111,:) = [  -2,   7, -65,   7, -20,  -2]
  K1(112,:) = [   7,  16,   7,  16,   7,   7]
  K1(113,:) = [  -2,   7, -20,   7, -65,  -2]
  K1(114,:) = [ -20,   7,  -2,   7,  -2, -65]
  K1(115,:) = [  16,   7,   7,   7,   7,  16]
  K1(116,:) = [   7, -65,  -2, -20,  -2,   7]
  K1(117,:) = [   7,  -2, -65,  -2, -20,   7]
  K1(118,:) = [   7, -20,  -2, -65,  -2,   7]
  K1(119,:) = [   7,  -2, -20,  -2, -65,   7]
  K1(120,:) = [  16,   7,   7,   7,   7,  16]
  K1(121,:) = [ -65,  -2,   7,  -2,   7, -20]
  K1(122,:) = [  -2, -65,   7, -20,   7,  -2]
  K1(123,:) = [   7,   7,  16,   7,  16,   7]
  K1(124,:) = [  -2, -20,   7, -65,   7,  -2]
  K1(125,:) = [   7,   7,  16,   7,  16,   7]
  K1(126,:) = [ -20,  -2,   7,  -2,   7, -65]
  K1(127,:) = [ 114, -12, -12, -12, -12,  24]
  K1(128,:) = [ -12, 114, -12,  24, -12, -12]
  K1(129,:) = [ -12, -12, 114, -12,  24, -12]
  K1(130,:) = [ -12,  24, -12, 114, -12, -12]
  K1(131,:) = [ -12, -12,  24, -12, 114, -12]
  K1(132,:) = [  24, -12, -12, -12, -12, 114]
  K1(133,:) = [   0,   0,   0,   0,   0,   0]
  K1(134,:) = [   0,   0,   0,   0,   0,   0]
  K1(135,:) = [   0,   0,   0,   0,   0,   0]
  K1(136,:) = [   0,   0,   0,   0,   0,   0]
  K1(137,:) = [   0,   0,   0,   0,   0,   0]
  K1(138,:) = [   0,   0,   0,   0,   0,   0]
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
end module ol_colourmatrix_heftpphhjj_hhgggg_1_/**/REALKIND



module ol_forced_parameters_heftpphhjj_hhgggg_1_/**/REALKIND
  implicit none
  contains
  subroutine check_forced_parameters
    use ol_parameters_decl_/**/REALKIND
    use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
    use ol_loop_parameters_decl_/**/DREALKIND, only: LeadingColour, nc, nf, CKMORDER
#endif
    implicit none
    logical, save :: checks_not_written = .true.

    if (checks_not_written) then
    ! e.g.
    ! if (ME /= 0) write(*,101) 'ME = 0'
  if (MB /= 0) write(*,101) 'MB = 0'
  if (YB /= 0) write(*,101) 'YB = 0'
  if (CKMORDER /= 0) write(*,101) 'CKMORDER = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 5) write(*,101) 'nf = 5'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_heftpphhjj_hhgggg_1_/**/REALKIND

module ol_loop_heftpphhjj_hhgggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(29), c(20)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:512)
  ! denominators
  complex(REALKIND), save :: den(534)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(6,16), Mct(9,16), Mcol_loop(9,16)
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
  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*DOI*eQED**2*gQCD**6)/(1152._/**/REALKIND*MW**2*pi**4*sw**2)
    f( 2) = (CI*eQED**2*gQCD**4)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 3) = (CI*countertermnorm*eQED**2*gQCD**6)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 4) = (CI*countertermnorm*ctHEFTggggh*eQED**2*gQCD**6)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 5) = (CI*countertermnorm*ctHEFTgggh*eQED**2*gQCD**6)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 6) = (CI*countertermnorm*ctVVV*eQED**2*gQCD**6)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 7) = (CI*countertermnorm*ctVVVV*eQED**2*gQCD**6)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 8) = (CI*eQED**2*gQCD**4*lambdaHHH*MH**2)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 9) = (CI*countertermnorm*eQED**2*gQCD**6*lambdaHHH*MH**2)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(10) = (CI*countertermnorm*ctHEFTggggh*eQED**2*gQCD**6*lambdaHHH*MH**2)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(11) = (CI*countertermnorm*ctHEFTgggh*eQED**2*gQCD**6*lambdaHHH*MH**2)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(12) = (CI*countertermnorm*ctVVV*eQED**2*gQCD**6*lambdaHHH*MH**2)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(13) = (CI*countertermnorm*ctVVVV*eQED**2*gQCD**6*lambdaHHH*MH**2)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(14) = (CI*countertermnorm*eQED**2*gQCD**6*R2GGGG)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f(15) = (CI*countertermnorm*eQED**2*gQCD**6*lambdaHHH*MH**2*R2GGGG)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(16) = (CI*countertermnorm*eQED**2*gQCD**6*R2HEFTggggh)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f(17) = (CI*countertermnorm*eQED**2*gQCD**6*lambdaHHH*MH**2*R2HEFTggggh)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(18) = (eQED**2*gQCD**6*integralnorm*SwB)/(MW**2*pi**2*sw**2*96._/**/REALKIND)
    f(19) = (eQED**2*gQCD**6*integralnorm*SwB)/(MW**2*pi**2*sw**2*48._/**/REALKIND)
    f(20) = (eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwB)/(MW**2*pi**2*sw**2*32._/**/REALKIND)
    f(21) = (eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwB)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(22) = (CI*eQED**2*gQCD**6*integralnorm*SwF)/(24._/**/REALKIND*MW**2*pi**2*sw**2)
    f(23) = (CI*eQED**2*gQCD**6*integralnorm*SwF)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(24) = (eQED**2*gQCD**6*integralnorm*SwF)/(MW**2*pi**2*sw**2*24._/**/REALKIND)
    f(25) = (eQED**2*gQCD**6*integralnorm*SwF)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(26) = (CI*eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwF)/(8._/**/REALKIND*MW**2*pi**2*sw**2)
    f(27) = (3*CI*eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwF)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(28) = (eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwF)/(MW**2*pi**2*sw**2*8._/**/REALKIND)
    f(29) = (3*eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwF)/(MW**2*pi**2*sw**2*16._/**/REALKIND)

  c = [ 2*f(18), 3*f(18), 6*f(18), 2*f(19), 3*f(19), 6*f(19), 2*f(20), 3*f(20), 6*f(20), 2*f(21), 3*f(21), 6*f(21), CI*f(22) &
    , CI*f(23), f(24), f(25), CI*f(26), CI*f(27), f(28), f(29) ]
end subroutine fac_init_loop


! **********************************************************************
subroutine tree_wavefunctions(P, H, M1, M2, POLSEL)
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
  integer,           intent(in), optional  :: POLSEL(6)
  complex(REALKIND), intent(out) :: M1(6), M2(9)
  complex(REALKIND) :: A(822)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_S(P(:,1), rMH, H(1), wf(:,0), POLSEL(1))
    call pol_wf_S(P(:,2), rMH, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_S(P(:,1), rMH, H(1), wf(:,0), 0)
    call pol_wf_S(P(:,2), rMH, H(2), wf(:,-1), 0)
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_HHGGG_G(wf(:,0),wf(:,-1),wf(:,-2),wf(:,-3),wf(:,-4),wf(:,1))
  call vert_HHGGG_G(wf(:,0),wf(:,-1),wf(:,-3),wf(:,-4),wf(:,-2),wf(:,2))
  call vert_HHGGG_G(wf(:,0),wf(:,-1),wf(:,-4),wf(:,-2),wf(:,-3),wf(:,3))
  call vert_SS_S(wf(:,0),wf(:,-1),wf(:,4))
  call vert_GGGG_H(wf(:,-2),wf(:,-3),wf(:,-4),wf(:,-5),wf(:,5))
  call vert_GGGG_H(wf(:,-2),wf(:,-4),wf(:,-5),wf(:,-3),wf(:,6))
  call vert_GGGG_H(wf(:,-2),wf(:,-5),wf(:,-3),wf(:,-4),wf(:,7))
  call vert_HHG_G(wf(:,0),wf(:,-1),wf(:,-2),Q(:,4),wf(:,8),Q(:,7))
  call vert_GGG_G(wf(:,-3),wf(:,-4),wf(:,-5),wf(:,9))
  call vert_GGG_G(wf(:,-4),wf(:,-5),wf(:,-3),wf(:,10))
  call vert_GGG_G(wf(:,-5),wf(:,-3),wf(:,-4),wf(:,11))
  call vert_HHG_G(wf(:,0),wf(:,-1),wf(:,-3),Q(:,8),wf(:,12),Q(:,11))
  call vert_GGG_G(wf(:,-2),wf(:,-4),wf(:,-5),wf(:,13))
  call vert_GGG_G(wf(:,-4),wf(:,-5),wf(:,-2),wf(:,14))
  call vert_GGG_G(wf(:,-5),wf(:,-2),wf(:,-4),wf(:,15))
  call vert_HHG_G(wf(:,0),wf(:,-1),wf(:,-4),Q(:,16),wf(:,16),Q(:,19))
  call vert_GGG_G(wf(:,-2),wf(:,-3),wf(:,-5),wf(:,17))
  call vert_GGG_G(wf(:,-3),wf(:,-5),wf(:,-2),wf(:,18))
  call vert_GGG_G(wf(:,-5),wf(:,-2),wf(:,-3),wf(:,19))
  call vert_HHG_G(wf(:,0),wf(:,-1),wf(:,-5),Q(:,32),wf(:,20),Q(:,35))
  call vert_GGG_G(wf(:,-2),wf(:,-3),wf(:,-4),wf(:,21))
  call vert_GGG_G(wf(:,-3),wf(:,-4),wf(:,-2),wf(:,22))
  call vert_GGG_G(wf(:,-4),wf(:,-2),wf(:,-3),wf(:,23))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,24))
  call vert_HHGG_G(wf(:,0),wf(:,-1),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,25),Q(:,51))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,26))
  call vert_HHGG_G(wf(:,0),wf(:,-1),wf(:,-3),Q(:,8),wf(:,-5),Q(:,32),wf(:,27),Q(:,43))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-5),Q(:,32),wf(:,28))
  call vert_HHGG_G(wf(:,0),wf(:,-1),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,29),Q(:,27))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,30))
  call vert_HHGG_G(wf(:,0),wf(:,-1),wf(:,-2),Q(:,4),wf(:,-5),Q(:,32),wf(:,31),Q(:,39))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-5),Q(:,32),wf(:,32))
  call vert_HHGG_G(wf(:,0),wf(:,-1),wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,33),Q(:,23))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,34))
  call vert_HHGG_G(wf(:,0),wf(:,-1),wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,35),Q(:,15))
  call vert_HGG_G(wf(:,4),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,36),Q(:,51))
  call vert_HGG_G(wf(:,4),wf(:,-3),Q(:,8),wf(:,-5),Q(:,32),wf(:,37),Q(:,43))
  call vert_HGG_G(wf(:,4),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,38),Q(:,27))
  call vert_HG_G(wf(:,4),wf(:,-2),Q(:,4),wf(:,39),Q(:,7))
  call vert_HGG_G(wf(:,4),wf(:,-2),Q(:,4),wf(:,-5),Q(:,32),wf(:,40),Q(:,39))
  call vert_HGG_G(wf(:,4),wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,41),Q(:,23))
  call vert_HG_G(wf(:,4),wf(:,-3),Q(:,8),wf(:,42),Q(:,11))
  call vert_HGG_G(wf(:,4),wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,43),Q(:,15))
  call vert_HG_G(wf(:,4),wf(:,-4),Q(:,16),wf(:,44),Q(:,19))
  call vert_HG_G(wf(:,4),wf(:,-5),Q(:,32),wf(:,45),Q(:,35))
  call vert_HHG_G(wf(:,0),wf(:,-1),wf(:,24),Q(:,12),wf(:,46),Q(:,15))
  call vert_UV_W(wf(:,24),Q(:,12),wf(:,-4),Q(:,16),wf(:,47))
  call vert_UV_W(wf(:,24),Q(:,12),wf(:,-5),Q(:,32),wf(:,48))
  call vert_HHG_G(wf(:,0),wf(:,-1),wf(:,26),Q(:,20),wf(:,49),Q(:,23))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,26),Q(:,20),wf(:,50))
  call vert_HHG_G(wf(:,0),wf(:,-1),wf(:,28),Q(:,36),wf(:,51),Q(:,39))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,30),Q(:,24),wf(:,52))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,28),Q(:,36),wf(:,53))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,32),Q(:,40),wf(:,54))
  call vert_UV_W(wf(:,26),Q(:,20),wf(:,-5),Q(:,32),wf(:,55))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,28),Q(:,36),wf(:,56))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,34),Q(:,48),wf(:,57))
  call vert_UV_W(wf(:,30),Q(:,24),wf(:,-5),Q(:,32),wf(:,58))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,32),Q(:,40),wf(:,59))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,34),Q(:,48),wf(:,60))
  call vert_HG_G(wf(:,4),wf(:,24),Q(:,12),wf(:,61),Q(:,15))
  call vert_HG_G(wf(:,4),wf(:,26),Q(:,20),wf(:,62),Q(:,23))
  call vert_HG_G(wf(:,4),wf(:,28),Q(:,36),wf(:,63),Q(:,39))
  call counter_HHGGG_G(wf(:,0),wf(:,-1),wf(:,-2),wf(:,-3),wf(:,-4),wf(:,64))
  call counter_HHGGG_G(wf(:,0),wf(:,-1),wf(:,-3),wf(:,-4),wf(:,-2),wf(:,65))
  call counter_HHGGG_G(wf(:,0),wf(:,-1),wf(:,-4),wf(:,-2),wf(:,-3),wf(:,66))
  call counter_GGGG_H(wf(:,-2),wf(:,-3),wf(:,-4),wf(:,-5),wf(:,67))
  call counter_GGGG_H(wf(:,-2),wf(:,-4),wf(:,-5),wf(:,-3),wf(:,68))
  call counter_GGGG_H(wf(:,-2),wf(:,-5),wf(:,-3),wf(:,-4),wf(:,69))
  call counter_GGG_G(wf(:,-3),wf(:,-4),wf(:,-5),wf(:,70))
  call counter_GGG_G(wf(:,-4),wf(:,-5),wf(:,-3),wf(:,71))
  call counter_GGG_G(wf(:,-5),wf(:,-3),wf(:,-4),wf(:,72))
  call counter_GGG_G(wf(:,-2),wf(:,-4),wf(:,-5),wf(:,73))
  call counter_GGG_G(wf(:,-4),wf(:,-5),wf(:,-2),wf(:,74))
  call counter_GGG_G(wf(:,-5),wf(:,-2),wf(:,-4),wf(:,75))
  call counter_GGG_G(wf(:,-2),wf(:,-3),wf(:,-5),wf(:,76))
  call counter_GGG_G(wf(:,-3),wf(:,-5),wf(:,-2),wf(:,77))
  call counter_GGG_G(wf(:,-5),wf(:,-2),wf(:,-3),wf(:,78))
  call counter_GGG_G(wf(:,-2),wf(:,-3),wf(:,-4),wf(:,79))
  call counter_GGG_G(wf(:,-3),wf(:,-4),wf(:,-2),wf(:,80))
  call counter_GGG_G(wf(:,-4),wf(:,-2),wf(:,-3),wf(:,81))
  call vert_HGG_G(wf(:,0),wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,82),Q(:,13))
  call counter_HGG_G_vert(wf(:,-1),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,83),Q(:,50))
  call vert_HGG_G(wf(:,0),wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,84),Q(:,21))
  call counter_HGG_G_vert(wf(:,-1),wf(:,-3),Q(:,8),wf(:,-5),Q(:,32),wf(:,85),Q(:,42))
  call vert_HGG_G(wf(:,0),wf(:,-2),Q(:,4),wf(:,-5),Q(:,32),wf(:,86),Q(:,37))
  call counter_HGG_G_vert(wf(:,-1),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,87),Q(:,26))
  call vert_HGG_G(wf(:,0),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,88),Q(:,25))
  call counter_HGG_G_vert(wf(:,-1),wf(:,-2),Q(:,4),wf(:,-5),Q(:,32),wf(:,89),Q(:,38))
  call vert_HGG_G(wf(:,0),wf(:,-3),Q(:,8),wf(:,-5),Q(:,32),wf(:,90),Q(:,41))
  call counter_HGG_G_vert(wf(:,-1),wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,91),Q(:,22))
  call vert_HGG_G(wf(:,0),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,92),Q(:,49))
  call counter_HGG_G_vert(wf(:,-1),wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,93),Q(:,14))
  call counter_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,94))
  call counter_UV_W(wf(:,-3),Q(:,8),wf(:,-5),Q(:,32),wf(:,95))
  call counter_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,96))
  call counter_UV_W(wf(:,-2),Q(:,4),wf(:,-5),Q(:,32),wf(:,97))
  call counter_UV_W(wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,98))
  call counter_UV_W(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,99))
  call vert_HGGG_G(wf(:,0),wf(:,-2),wf(:,-3),wf(:,-4),wf(:,100))
  call counter_HG_G_vert(wf(:,-1),wf(:,-5),Q(:,32),wf(:,101),Q(:,34))
  call vert_HGGG_G(wf(:,0),wf(:,-3),wf(:,-4),wf(:,-2),wf(:,102))
  call vert_HGGG_G(wf(:,0),wf(:,-4),wf(:,-2),wf(:,-3),wf(:,103))
  call vert_HGGG_G(wf(:,0),wf(:,-2),wf(:,-3),wf(:,-5),wf(:,104))
  call counter_HG_G_vert(wf(:,-1),wf(:,-4),Q(:,16),wf(:,105),Q(:,18))
  call vert_HGGG_G(wf(:,0),wf(:,-3),wf(:,-5),wf(:,-2),wf(:,106))
  call vert_HGGG_G(wf(:,0),wf(:,-5),wf(:,-2),wf(:,-3),wf(:,107))
  call vert_HGGG_G(wf(:,0),wf(:,-2),wf(:,-4),wf(:,-5),wf(:,108))
  call counter_HG_G_vert(wf(:,-1),wf(:,-3),Q(:,8),wf(:,109),Q(:,10))
  call vert_HGGG_G(wf(:,0),wf(:,-4),wf(:,-5),wf(:,-2),wf(:,110))
  call vert_HGGG_G(wf(:,0),wf(:,-5),wf(:,-2),wf(:,-4),wf(:,111))
  call vert_HGGG_G(wf(:,0),wf(:,-3),wf(:,-4),wf(:,-5),wf(:,112))
  call counter_HG_G_vert(wf(:,-1),wf(:,-2),Q(:,4),wf(:,113),Q(:,6))
  call vert_HGGG_G(wf(:,0),wf(:,-4),wf(:,-5),wf(:,-3),wf(:,114))
  call vert_HGGG_G(wf(:,0),wf(:,-5),wf(:,-3),wf(:,-4),wf(:,115))
  call vert_HGGG_G(wf(:,-1),wf(:,-2),wf(:,-3),wf(:,-4),wf(:,116))
  call counter_HG_G_vert(wf(:,0),wf(:,-5),Q(:,32),wf(:,117),Q(:,33))
  call vert_HGGG_G(wf(:,-1),wf(:,-3),wf(:,-4),wf(:,-2),wf(:,118))
  call vert_HGGG_G(wf(:,-1),wf(:,-4),wf(:,-2),wf(:,-3),wf(:,119))
  call vert_HGGG_G(wf(:,-1),wf(:,-2),wf(:,-3),wf(:,-5),wf(:,120))
  call counter_HG_G_vert(wf(:,0),wf(:,-4),Q(:,16),wf(:,121),Q(:,17))
  call vert_HGGG_G(wf(:,-1),wf(:,-3),wf(:,-5),wf(:,-2),wf(:,122))
  call vert_HGGG_G(wf(:,-1),wf(:,-5),wf(:,-2),wf(:,-3),wf(:,123))
  call vert_HGGG_G(wf(:,-1),wf(:,-2),wf(:,-4),wf(:,-5),wf(:,124))
  call counter_HG_G_vert(wf(:,0),wf(:,-3),Q(:,8),wf(:,125),Q(:,9))
  call vert_HGGG_G(wf(:,-1),wf(:,-4),wf(:,-5),wf(:,-2),wf(:,126))
  call vert_HGGG_G(wf(:,-1),wf(:,-5),wf(:,-2),wf(:,-4),wf(:,127))
  call vert_HGGG_G(wf(:,-1),wf(:,-3),wf(:,-4),wf(:,-5),wf(:,128))
  call counter_HG_G_vert(wf(:,0),wf(:,-2),Q(:,4),wf(:,129),Q(:,5))
  call vert_HGGG_G(wf(:,-1),wf(:,-4),wf(:,-5),wf(:,-3),wf(:,130))
  call vert_HGGG_G(wf(:,-1),wf(:,-5),wf(:,-3),wf(:,-4),wf(:,131))
  call vert_HGG_G(wf(:,-1),wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,132),Q(:,14))
  call counter_HGG_G_vert(wf(:,0),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,133),Q(:,49))
  call vert_HGG_G(wf(:,-1),wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,134),Q(:,22))
  call counter_HGG_G_vert(wf(:,0),wf(:,-3),Q(:,8),wf(:,-5),Q(:,32),wf(:,135),Q(:,41))
  call vert_HGG_G(wf(:,-1),wf(:,-2),Q(:,4),wf(:,-5),Q(:,32),wf(:,136),Q(:,38))
  call counter_HGG_G_vert(wf(:,0),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,137),Q(:,25))
  call vert_HGG_G(wf(:,-1),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,138),Q(:,26))
  call counter_HGG_G_vert(wf(:,0),wf(:,-2),Q(:,4),wf(:,-5),Q(:,32),wf(:,139),Q(:,37))
  call vert_HGG_G(wf(:,-1),wf(:,-3),Q(:,8),wf(:,-5),Q(:,32),wf(:,140),Q(:,42))
  call counter_HGG_G_vert(wf(:,0),wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,141),Q(:,21))
  call vert_HGG_G(wf(:,-1),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,142),Q(:,50))
  call counter_HGG_G_vert(wf(:,0),wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,143),Q(:,13))
  call counter_HHG_G(ctHEFTggh,wf(:,0),wf(:,-1),wf(:,-5),Q(:,32),wf(:,144),Q(:,35))
  call counter_HHG_G(ctHEFTggh,wf(:,0),wf(:,-1),wf(:,-4),Q(:,16),wf(:,145),Q(:,19))
  call counter_HHG_G(ctHEFTggh,wf(:,0),wf(:,-1),wf(:,-3),Q(:,8),wf(:,146),Q(:,11))
  call counter_HHG_G(ctHEFTggh,wf(:,0),wf(:,-1),wf(:,-2),Q(:,4),wf(:,147),Q(:,7))
  call counter_HHGG_G(wf(:,0),wf(:,-1),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,148),Q(:,51))
  call counter_HHGG_G(wf(:,0),wf(:,-1),wf(:,-3),Q(:,8),wf(:,-5),Q(:,32),wf(:,149),Q(:,43))
  call counter_HHGG_G(wf(:,0),wf(:,-1),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,150),Q(:,27))
  call counter_HHGG_G(wf(:,0),wf(:,-1),wf(:,-2),Q(:,4),wf(:,-5),Q(:,32),wf(:,151),Q(:,39))
  call counter_HHGG_G(wf(:,0),wf(:,-1),wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,152),Q(:,23))
  call counter_HHGG_G(wf(:,0),wf(:,-1),wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,153),Q(:,15))
  call counter_HGG_G(wf(:,4),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,154),Q(:,51))
  call counter_HGG_G(wf(:,4),wf(:,-3),Q(:,8),wf(:,-5),Q(:,32),wf(:,155),Q(:,43))
  call counter_HGG_G(wf(:,4),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,156),Q(:,27))
  call counter_HG_G(ctHEFTggh,wf(:,4),wf(:,-5),Q(:,32),wf(:,157),Q(:,35))
  call counter_HG_G(ctHEFTggh,wf(:,4),wf(:,-4),Q(:,16),wf(:,158),Q(:,19))
  call counter_HG_G(ctHEFTggh,wf(:,4),wf(:,-3),Q(:,8),wf(:,159),Q(:,11))
  call counter_HG_G(ctHEFTggh,wf(:,4),wf(:,-2),Q(:,4),wf(:,160),Q(:,7))
  call counter_HGG_G(wf(:,4),wf(:,-2),Q(:,4),wf(:,-5),Q(:,32),wf(:,161),Q(:,39))
  call counter_HGG_G(wf(:,4),wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,162),Q(:,23))
  call counter_HGG_G(wf(:,4),wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,163),Q(:,15))
  call vert_HG_G(wf(:,0),wf(:,-2),Q(:,4),wf(:,164),Q(:,5))
  call counter_HG_G_vert(wf(:,-1),wf(:,164),Q(:,5),wf(:,165),Q(:,7))
  call vert_GGG_G(wf(:,164),wf(:,-3),wf(:,-4),wf(:,166))
  call vert_GGG_G(wf(:,-3),wf(:,-4),wf(:,164),wf(:,167))
  call vert_GGG_G(wf(:,-4),wf(:,164),wf(:,-3),wf(:,168))
  call vert_GGG_G(wf(:,164),wf(:,-3),wf(:,-5),wf(:,169))
  call vert_GGG_G(wf(:,-3),wf(:,-5),wf(:,164),wf(:,170))
  call vert_GGG_G(wf(:,-5),wf(:,164),wf(:,-3),wf(:,171))
  call vert_GGG_G(wf(:,164),wf(:,-4),wf(:,-5),wf(:,172))
  call vert_GGG_G(wf(:,-4),wf(:,-5),wf(:,164),wf(:,173))
  call vert_GGG_G(wf(:,-5),wf(:,164),wf(:,-4),wf(:,174))
  call vert_HG_G(wf(:,0),wf(:,-3),Q(:,8),wf(:,175),Q(:,9))
  call counter_HG_G_vert(wf(:,-1),wf(:,175),Q(:,9),wf(:,176),Q(:,11))
  call vert_GGG_G(wf(:,-2),wf(:,175),wf(:,-4),wf(:,177))
  call vert_GGG_G(wf(:,175),wf(:,-4),wf(:,-2),wf(:,178))
  call vert_GGG_G(wf(:,-4),wf(:,-2),wf(:,175),wf(:,179))
  call vert_GGG_G(wf(:,-2),wf(:,175),wf(:,-5),wf(:,180))
  call vert_GGG_G(wf(:,175),wf(:,-5),wf(:,-2),wf(:,181))
  call vert_GGG_G(wf(:,-5),wf(:,-2),wf(:,175),wf(:,182))
  call vert_HG_G(wf(:,0),wf(:,-4),Q(:,16),wf(:,183),Q(:,17))
  call counter_HG_G_vert(wf(:,-1),wf(:,183),Q(:,17),wf(:,184),Q(:,19))
  call vert_GGG_G(wf(:,-2),wf(:,-3),wf(:,183),wf(:,185))
  call vert_GGG_G(wf(:,-3),wf(:,183),wf(:,-2),wf(:,186))
  call vert_GGG_G(wf(:,183),wf(:,-2),wf(:,-3),wf(:,187))
  call vert_HG_G(wf(:,0),wf(:,-5),Q(:,32),wf(:,188),Q(:,33))
  call counter_HG_G_vert(wf(:,-1),wf(:,188),Q(:,33),wf(:,189),Q(:,35))
  call vert_HG_G(wf(:,0),wf(:,101),Q(:,34),wf(:,190),Q(:,35))
  call vert_GGG_G(wf(:,-2),wf(:,-3),wf(:,188),wf(:,191))
  call vert_GGG_G(wf(:,-3),wf(:,188),wf(:,-2),wf(:,192))
  call vert_GGG_G(wf(:,188),wf(:,-2),wf(:,-3),wf(:,193))
  call vert_HG_G(wf(:,0),wf(:,105),Q(:,18),wf(:,194),Q(:,19))
  call vert_GGG_G(wf(:,-2),wf(:,183),wf(:,-5),wf(:,195))
  call vert_GGG_G(wf(:,183),wf(:,-5),wf(:,-2),wf(:,196))
  call vert_GGG_G(wf(:,-5),wf(:,-2),wf(:,183),wf(:,197))
  call vert_GGG_G(wf(:,-2),wf(:,-4),wf(:,188),wf(:,198))
  call vert_GGG_G(wf(:,-4),wf(:,188),wf(:,-2),wf(:,199))
  call vert_GGG_G(wf(:,188),wf(:,-2),wf(:,-4),wf(:,200))
  call vert_HG_G(wf(:,0),wf(:,109),Q(:,10),wf(:,201),Q(:,11))
  call vert_GGG_G(wf(:,175),wf(:,-4),wf(:,-5),wf(:,202))
  call vert_GGG_G(wf(:,-4),wf(:,-5),wf(:,175),wf(:,203))
  call vert_GGG_G(wf(:,-5),wf(:,175),wf(:,-4),wf(:,204))
  call vert_GGG_G(wf(:,-3),wf(:,183),wf(:,-5),wf(:,205))
  call vert_GGG_G(wf(:,183),wf(:,-5),wf(:,-3),wf(:,206))
  call vert_GGG_G(wf(:,-5),wf(:,-3),wf(:,183),wf(:,207))
  call vert_GGG_G(wf(:,-3),wf(:,-4),wf(:,188),wf(:,208))
  call vert_GGG_G(wf(:,-4),wf(:,188),wf(:,-3),wf(:,209))
  call vert_GGG_G(wf(:,188),wf(:,-3),wf(:,-4),wf(:,210))
  call vert_HG_G(wf(:,0),wf(:,113),Q(:,6),wf(:,211),Q(:,7))
  call counter_HGG_G_vert(wf(:,-1),wf(:,164),Q(:,5),wf(:,-5),Q(:,32),wf(:,212),Q(:,39))
  call counter_HGG_G_vert(wf(:,-1),wf(:,164),Q(:,5),wf(:,-4),Q(:,16),wf(:,213),Q(:,23))
  call vert_UV_W(wf(:,164),Q(:,5),wf(:,-3),Q(:,8),wf(:,214))
  call counter_HGG_G_vert(wf(:,-1),wf(:,164),Q(:,5),wf(:,-3),Q(:,8),wf(:,215),Q(:,15))
  call vert_UV_W(wf(:,164),Q(:,5),wf(:,-4),Q(:,16),wf(:,216))
  call vert_UV_W(wf(:,164),Q(:,5),wf(:,-5),Q(:,32),wf(:,217))
  call counter_HGG_G_vert(wf(:,-1),wf(:,175),Q(:,9),wf(:,-5),Q(:,32),wf(:,218),Q(:,43))
  call counter_HGG_G_vert(wf(:,-1),wf(:,175),Q(:,9),wf(:,-4),Q(:,16),wf(:,219),Q(:,27))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,175),Q(:,9),wf(:,220))
  call counter_HGG_G_vert(wf(:,-1),wf(:,183),Q(:,17),wf(:,-5),Q(:,32),wf(:,221),Q(:,51))
  call counter_HGG_G_vert(wf(:,-1),wf(:,-4),Q(:,16),wf(:,188),Q(:,33),wf(:,222),Q(:,51))
  call vert_HG_G(wf(:,0),wf(:,24),Q(:,12),wf(:,223),Q(:,13))
  call counter_HGG_G_vert(wf(:,-1),wf(:,-3),Q(:,8),wf(:,183),Q(:,17),wf(:,224),Q(:,27))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,183),Q(:,17),wf(:,225))
  call counter_HGG_G_vert(wf(:,-1),wf(:,-3),Q(:,8),wf(:,188),Q(:,33),wf(:,226),Q(:,43))
  call vert_HG_G(wf(:,0),wf(:,26),Q(:,20),wf(:,227),Q(:,21))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,188),Q(:,33),wf(:,228))
  call vert_HG_G(wf(:,0),wf(:,28),Q(:,36),wf(:,229),Q(:,37))
  call counter_HGG_G_vert(wf(:,-1),wf(:,-2),Q(:,4),wf(:,175),Q(:,9),wf(:,230),Q(:,15))
  call vert_UV_W(wf(:,175),Q(:,9),wf(:,-4),Q(:,16),wf(:,231))
  call vert_UV_W(wf(:,175),Q(:,9),wf(:,-5),Q(:,32),wf(:,232))
  call counter_HGG_G_vert(wf(:,-1),wf(:,-2),Q(:,4),wf(:,183),Q(:,17),wf(:,233),Q(:,23))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,183),Q(:,17),wf(:,234))
  call counter_HGG_G_vert(wf(:,-1),wf(:,-2),Q(:,4),wf(:,188),Q(:,33),wf(:,235),Q(:,39))
  call vert_HG_G(wf(:,0),wf(:,30),Q(:,24),wf(:,236),Q(:,25))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,188),Q(:,33),wf(:,237))
  call vert_HG_G(wf(:,0),wf(:,32),Q(:,40),wf(:,238),Q(:,41))
  call vert_UV_W(wf(:,183),Q(:,17),wf(:,-5),Q(:,32),wf(:,239))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,188),Q(:,33),wf(:,240))
  call vert_HG_G(wf(:,0),wf(:,34),Q(:,48),wf(:,241),Q(:,49))
  call counter_UV_W(wf(:,24),Q(:,12),wf(:,-5),Q(:,32),wf(:,242))
  call counter_UV_W(wf(:,24),Q(:,12),wf(:,-4),Q(:,16),wf(:,243))
  call counter_UV_W(wf(:,26),Q(:,20),wf(:,-5),Q(:,32),wf(:,244))
  call counter_UV_W(wf(:,-4),Q(:,16),wf(:,28),Q(:,36),wf(:,245))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,94),Q(:,48),wf(:,246))
  call counter_UV_W(wf(:,-3),Q(:,8),wf(:,26),Q(:,20),wf(:,247))
  call counter_UV_W(wf(:,-3),Q(:,8),wf(:,28),Q(:,36),wf(:,248))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,95),Q(:,40),wf(:,249))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,96),Q(:,24),wf(:,250))
  call counter_UV_W(wf(:,30),Q(:,24),wf(:,-5),Q(:,32),wf(:,251))
  call counter_UV_W(wf(:,-4),Q(:,16),wf(:,32),Q(:,40),wf(:,252))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,94),Q(:,48),wf(:,253))
  call counter_V_V(ctGG,wf(:,8),Q(:,7),wf(:,254))
  call counter_UV_W(wf(:,-3),Q(:,8),wf(:,34),Q(:,48),wf(:,255))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,95),Q(:,40),wf(:,256))
  call vert_UV_W(wf(:,96),Q(:,24),wf(:,-5),Q(:,32),wf(:,257))
  call counter_V_V(ctGG,wf(:,12),Q(:,11),wf(:,258))
  call counter_V_V(ctGG,wf(:,16),Q(:,19),wf(:,259))
  call counter_V_V(ctGG,wf(:,20),Q(:,35),wf(:,260))
  call counter_UV_W(wf(:,-2),Q(:,4),wf(:,30),Q(:,24),wf(:,261))
  call vert_HHG_G(wf(:,0),wf(:,-1),wf(:,97),Q(:,36),wf(:,262),Q(:,39))
  call counter_UV_W(wf(:,-2),Q(:,4),wf(:,32),Q(:,40),wf(:,263))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,97),Q(:,36),wf(:,264))
  call vert_HHG_G(wf(:,0),wf(:,-1),wf(:,98),Q(:,20),wf(:,265),Q(:,23))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,98),Q(:,20),wf(:,266))
  call counter_UV_W(wf(:,-2),Q(:,4),wf(:,34),Q(:,48),wf(:,267))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,97),Q(:,36),wf(:,268))
  call vert_UV_W(wf(:,98),Q(:,20),wf(:,-5),Q(:,32),wf(:,269))
  call vert_HHG_G(wf(:,0),wf(:,-1),wf(:,99),Q(:,12),wf(:,270),Q(:,15))
  call vert_UV_W(wf(:,99),Q(:,12),wf(:,-4),Q(:,16),wf(:,271))
  call vert_UV_W(wf(:,99),Q(:,12),wf(:,-5),Q(:,32),wf(:,272))
  call counter_HG_G_vert(wf(:,-1),wf(:,24),Q(:,12),wf(:,273),Q(:,14))
  call vert_HGG_G(wf(:,0),wf(:,-4),Q(:,16),wf(:,101),Q(:,34),wf(:,274),Q(:,51))
  call vert_HGG_G(wf(:,0),wf(:,105),Q(:,18),wf(:,-5),Q(:,32),wf(:,275),Q(:,51))
  call counter_HG_G_vert(wf(:,-1),wf(:,26),Q(:,20),wf(:,276),Q(:,22))
  call vert_HGG_G(wf(:,0),wf(:,-3),Q(:,8),wf(:,101),Q(:,34),wf(:,277),Q(:,43))
  call counter_HG_G_vert(wf(:,-1),wf(:,28),Q(:,36),wf(:,278),Q(:,38))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,101),Q(:,34),wf(:,279))
  call vert_HGG_G(wf(:,0),wf(:,-3),Q(:,8),wf(:,105),Q(:,18),wf(:,280),Q(:,27))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,105),Q(:,18),wf(:,281))
  call vert_HGG_G(wf(:,0),wf(:,109),Q(:,10),wf(:,-5),Q(:,32),wf(:,282),Q(:,43))
  call vert_HGG_G(wf(:,0),wf(:,109),Q(:,10),wf(:,-4),Q(:,16),wf(:,283),Q(:,27))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,109),Q(:,10),wf(:,284))
  call counter_HG_G_vert(wf(:,-1),wf(:,30),Q(:,24),wf(:,285),Q(:,26))
  call vert_HGG_G(wf(:,0),wf(:,-2),Q(:,4),wf(:,101),Q(:,34),wf(:,286),Q(:,39))
  call counter_HG_G_vert(wf(:,-1),wf(:,32),Q(:,40),wf(:,287),Q(:,42))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,101),Q(:,34),wf(:,288))
  call vert_HGG_G(wf(:,0),wf(:,-2),Q(:,4),wf(:,105),Q(:,18),wf(:,289),Q(:,23))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,105),Q(:,18),wf(:,290))
  call counter_HG_G_vert(wf(:,-1),wf(:,34),Q(:,48),wf(:,291),Q(:,50))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,101),Q(:,34),wf(:,292))
  call vert_UV_W(wf(:,105),Q(:,18),wf(:,-5),Q(:,32),wf(:,293))
  call vert_HGG_G(wf(:,0),wf(:,-2),Q(:,4),wf(:,109),Q(:,10),wf(:,294),Q(:,15))
  call vert_UV_W(wf(:,109),Q(:,10),wf(:,-4),Q(:,16),wf(:,295))
  call vert_UV_W(wf(:,109),Q(:,10),wf(:,-5),Q(:,32),wf(:,296))
  call vert_HGG_G(wf(:,0),wf(:,113),Q(:,6),wf(:,-5),Q(:,32),wf(:,297),Q(:,39))
  call vert_HGG_G(wf(:,0),wf(:,113),Q(:,6),wf(:,-4),Q(:,16),wf(:,298),Q(:,23))
  call vert_UV_W(wf(:,113),Q(:,6),wf(:,-3),Q(:,8),wf(:,299))
  call vert_HGG_G(wf(:,0),wf(:,113),Q(:,6),wf(:,-3),Q(:,8),wf(:,300),Q(:,15))
  call vert_UV_W(wf(:,113),Q(:,6),wf(:,-4),Q(:,16),wf(:,301))
  call vert_UV_W(wf(:,113),Q(:,6),wf(:,-5),Q(:,32),wf(:,302))
  call counter_V_V(ctGG,wf(:,24),Q(:,12),wf(:,303))
  call counter_V_V(ctGG,wf(:,26),Q(:,20),wf(:,304))
  call counter_V_V(ctGG,wf(:,28),Q(:,36),wf(:,305))
  call counter_V_V(ctGG,wf(:,30),Q(:,24),wf(:,306))
  call counter_V_V(ctGG,wf(:,32),Q(:,40),wf(:,307))
  call counter_V_V(ctGG,wf(:,34),Q(:,48),wf(:,308))
  call vert_HG_G(wf(:,-1),wf(:,-2),Q(:,4),wf(:,309),Q(:,6))
  call counter_HG_G_vert(wf(:,0),wf(:,309),Q(:,6),wf(:,310),Q(:,7))
  call vert_GGG_G(wf(:,-3),wf(:,-4),wf(:,117),wf(:,311))
  call vert_GGG_G(wf(:,-4),wf(:,117),wf(:,-3),wf(:,312))
  call vert_GGG_G(wf(:,117),wf(:,-3),wf(:,-4),wf(:,313))
  call vert_GGG_G(wf(:,-3),wf(:,121),wf(:,-5),wf(:,314))
  call vert_GGG_G(wf(:,121),wf(:,-5),wf(:,-3),wf(:,315))
  call vert_GGG_G(wf(:,-5),wf(:,-3),wf(:,121),wf(:,316))
  call vert_GGG_G(wf(:,125),wf(:,-4),wf(:,-5),wf(:,317))
  call vert_GGG_G(wf(:,-4),wf(:,-5),wf(:,125),wf(:,318))
  call vert_GGG_G(wf(:,-5),wf(:,125),wf(:,-4),wf(:,319))
  call vert_HG_G(wf(:,-1),wf(:,-3),Q(:,8),wf(:,320),Q(:,10))
  call counter_HG_G_vert(wf(:,0),wf(:,320),Q(:,10),wf(:,321),Q(:,11))
  call vert_GGG_G(wf(:,-2),wf(:,-4),wf(:,117),wf(:,322))
  call vert_GGG_G(wf(:,-4),wf(:,117),wf(:,-2),wf(:,323))
  call vert_GGG_G(wf(:,117),wf(:,-2),wf(:,-4),wf(:,324))
  call vert_GGG_G(wf(:,-2),wf(:,121),wf(:,-5),wf(:,325))
  call vert_GGG_G(wf(:,121),wf(:,-5),wf(:,-2),wf(:,326))
  call vert_GGG_G(wf(:,-5),wf(:,-2),wf(:,121),wf(:,327))
  call vert_HG_G(wf(:,-1),wf(:,-4),Q(:,16),wf(:,328),Q(:,18))
  call counter_HG_G_vert(wf(:,0),wf(:,328),Q(:,18),wf(:,329),Q(:,19))
  call vert_GGG_G(wf(:,-2),wf(:,-3),wf(:,117),wf(:,330))
  call vert_GGG_G(wf(:,-3),wf(:,117),wf(:,-2),wf(:,331))
  call vert_GGG_G(wf(:,117),wf(:,-2),wf(:,-3),wf(:,332))
  call vert_HG_G(wf(:,-1),wf(:,-5),Q(:,32),wf(:,333),Q(:,34))
  call counter_HG_G_vert(wf(:,0),wf(:,333),Q(:,34),wf(:,334),Q(:,35))
  call vert_HG_G(wf(:,-1),wf(:,117),Q(:,33),wf(:,335),Q(:,35))
  call vert_GGG_G(wf(:,-2),wf(:,-3),wf(:,121),wf(:,336))
  call vert_GGG_G(wf(:,-3),wf(:,121),wf(:,-2),wf(:,337))
  call vert_GGG_G(wf(:,121),wf(:,-2),wf(:,-3),wf(:,338))
  call vert_HG_G(wf(:,-1),wf(:,121),Q(:,17),wf(:,339),Q(:,19))
  call vert_GGG_G(wf(:,-2),wf(:,125),wf(:,-5),wf(:,340))
  call vert_GGG_G(wf(:,125),wf(:,-5),wf(:,-2),wf(:,341))
  call vert_GGG_G(wf(:,-5),wf(:,-2),wf(:,125),wf(:,342))
  call vert_GGG_G(wf(:,-2),wf(:,125),wf(:,-4),wf(:,343))
  call vert_GGG_G(wf(:,125),wf(:,-4),wf(:,-2),wf(:,344))
  call vert_GGG_G(wf(:,-4),wf(:,-2),wf(:,125),wf(:,345))
  call vert_HG_G(wf(:,-1),wf(:,125),Q(:,9),wf(:,346),Q(:,11))
  call vert_GGG_G(wf(:,129),wf(:,-4),wf(:,-5),wf(:,347))
  call vert_GGG_G(wf(:,-4),wf(:,-5),wf(:,129),wf(:,348))
  call vert_GGG_G(wf(:,-5),wf(:,129),wf(:,-4),wf(:,349))
  call vert_GGG_G(wf(:,129),wf(:,-3),wf(:,-5),wf(:,350))
  call vert_GGG_G(wf(:,-3),wf(:,-5),wf(:,129),wf(:,351))
  call vert_GGG_G(wf(:,-5),wf(:,129),wf(:,-3),wf(:,352))
  call vert_GGG_G(wf(:,129),wf(:,-3),wf(:,-4),wf(:,353))
  call vert_GGG_G(wf(:,-3),wf(:,-4),wf(:,129),wf(:,354))
  call vert_GGG_G(wf(:,-4),wf(:,129),wf(:,-3),wf(:,355))
  call vert_HG_G(wf(:,-1),wf(:,129),Q(:,5),wf(:,356),Q(:,7))
  call counter_HG_G_vert(wf(:,0),wf(:,24),Q(:,12),wf(:,357),Q(:,13))
  call vert_HGG_G(wf(:,-1),wf(:,-4),Q(:,16),wf(:,117),Q(:,33),wf(:,358),Q(:,51))
  call vert_HGG_G(wf(:,-1),wf(:,121),Q(:,17),wf(:,-5),Q(:,32),wf(:,359),Q(:,51))
  call counter_HG_G_vert(wf(:,0),wf(:,26),Q(:,20),wf(:,360),Q(:,21))
  call vert_HGG_G(wf(:,-1),wf(:,-3),Q(:,8),wf(:,117),Q(:,33),wf(:,361),Q(:,43))
  call counter_HG_G_vert(wf(:,0),wf(:,28),Q(:,36),wf(:,362),Q(:,37))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,117),Q(:,33),wf(:,363))
  call vert_HGG_G(wf(:,-1),wf(:,-3),Q(:,8),wf(:,121),Q(:,17),wf(:,364),Q(:,27))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,121),Q(:,17),wf(:,365))
  call vert_HGG_G(wf(:,-1),wf(:,125),Q(:,9),wf(:,-5),Q(:,32),wf(:,366),Q(:,43))
  call vert_HGG_G(wf(:,-1),wf(:,125),Q(:,9),wf(:,-4),Q(:,16),wf(:,367),Q(:,27))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,125),Q(:,9),wf(:,368))
  call counter_HG_G_vert(wf(:,0),wf(:,30),Q(:,24),wf(:,369),Q(:,25))
  call vert_HGG_G(wf(:,-1),wf(:,-2),Q(:,4),wf(:,117),Q(:,33),wf(:,370),Q(:,39))
  call counter_HG_G_vert(wf(:,0),wf(:,32),Q(:,40),wf(:,371),Q(:,41))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,117),Q(:,33),wf(:,372))
  call vert_HGG_G(wf(:,-1),wf(:,-2),Q(:,4),wf(:,121),Q(:,17),wf(:,373),Q(:,23))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,121),Q(:,17),wf(:,374))
  call counter_HG_G_vert(wf(:,0),wf(:,34),Q(:,48),wf(:,375),Q(:,49))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,117),Q(:,33),wf(:,376))
  call vert_UV_W(wf(:,121),Q(:,17),wf(:,-5),Q(:,32),wf(:,377))
  call vert_HGG_G(wf(:,-1),wf(:,-2),Q(:,4),wf(:,125),Q(:,9),wf(:,378),Q(:,15))
  call vert_UV_W(wf(:,125),Q(:,9),wf(:,-4),Q(:,16),wf(:,379))
  call vert_UV_W(wf(:,125),Q(:,9),wf(:,-5),Q(:,32),wf(:,380))
  call vert_HGG_G(wf(:,-1),wf(:,129),Q(:,5),wf(:,-5),Q(:,32),wf(:,381),Q(:,39))
  call vert_HGG_G(wf(:,-1),wf(:,129),Q(:,5),wf(:,-4),Q(:,16),wf(:,382),Q(:,23))
  call vert_UV_W(wf(:,129),Q(:,5),wf(:,-3),Q(:,8),wf(:,383))
  call vert_HGG_G(wf(:,-1),wf(:,129),Q(:,5),wf(:,-3),Q(:,8),wf(:,384),Q(:,15))
  call vert_UV_W(wf(:,129),Q(:,5),wf(:,-4),Q(:,16),wf(:,385))
  call vert_UV_W(wf(:,129),Q(:,5),wf(:,-5),Q(:,32),wf(:,386))
  call counter_HGG_G_vert(wf(:,0),wf(:,309),Q(:,6),wf(:,-5),Q(:,32),wf(:,387),Q(:,39))
  call counter_HGG_G_vert(wf(:,0),wf(:,309),Q(:,6),wf(:,-4),Q(:,16),wf(:,388),Q(:,23))
  call vert_UV_W(wf(:,309),Q(:,6),wf(:,-3),Q(:,8),wf(:,389))
  call counter_HGG_G_vert(wf(:,0),wf(:,309),Q(:,6),wf(:,-3),Q(:,8),wf(:,390),Q(:,15))
  call vert_UV_W(wf(:,309),Q(:,6),wf(:,-4),Q(:,16),wf(:,391))
  call vert_UV_W(wf(:,309),Q(:,6),wf(:,-5),Q(:,32),wf(:,392))
  call counter_HGG_G_vert(wf(:,0),wf(:,320),Q(:,10),wf(:,-5),Q(:,32),wf(:,393),Q(:,43))
  call counter_HGG_G_vert(wf(:,0),wf(:,320),Q(:,10),wf(:,-4),Q(:,16),wf(:,394),Q(:,27))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,320),Q(:,10),wf(:,395))
  call counter_HGG_G_vert(wf(:,0),wf(:,328),Q(:,18),wf(:,-5),Q(:,32),wf(:,396),Q(:,51))
  call counter_HGG_G_vert(wf(:,0),wf(:,-4),Q(:,16),wf(:,333),Q(:,34),wf(:,397),Q(:,51))
  call vert_HG_G(wf(:,-1),wf(:,24),Q(:,12),wf(:,398),Q(:,14))
  call counter_HGG_G_vert(wf(:,0),wf(:,-3),Q(:,8),wf(:,328),Q(:,18),wf(:,399),Q(:,27))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,328),Q(:,18),wf(:,400))
  call counter_HGG_G_vert(wf(:,0),wf(:,-3),Q(:,8),wf(:,333),Q(:,34),wf(:,401),Q(:,43))
  call vert_HG_G(wf(:,-1),wf(:,26),Q(:,20),wf(:,402),Q(:,22))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,333),Q(:,34),wf(:,403))
  call vert_HG_G(wf(:,-1),wf(:,28),Q(:,36),wf(:,404),Q(:,38))
  call counter_HGG_G_vert(wf(:,0),wf(:,-2),Q(:,4),wf(:,320),Q(:,10),wf(:,405),Q(:,15))
  call vert_UV_W(wf(:,320),Q(:,10),wf(:,-4),Q(:,16),wf(:,406))
  call vert_UV_W(wf(:,320),Q(:,10),wf(:,-5),Q(:,32),wf(:,407))
  call counter_HGG_G_vert(wf(:,0),wf(:,-2),Q(:,4),wf(:,328),Q(:,18),wf(:,408),Q(:,23))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,328),Q(:,18),wf(:,409))
  call counter_HGG_G_vert(wf(:,0),wf(:,-2),Q(:,4),wf(:,333),Q(:,34),wf(:,410),Q(:,39))
  call vert_HG_G(wf(:,-1),wf(:,30),Q(:,24),wf(:,411),Q(:,26))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,333),Q(:,34),wf(:,412))
  call vert_HG_G(wf(:,-1),wf(:,32),Q(:,40),wf(:,413),Q(:,42))
  call vert_UV_W(wf(:,328),Q(:,18),wf(:,-5),Q(:,32),wf(:,414))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,333),Q(:,34),wf(:,415))
  call vert_HG_G(wf(:,-1),wf(:,34),Q(:,48),wf(:,416),Q(:,50))
  call counter_HHG_G(ctHEFTggh,wf(:,0),wf(:,-1),wf(:,24),Q(:,12),wf(:,417),Q(:,15))
  call counter_HHG_G(ctHEFTggh,wf(:,0),wf(:,-1),wf(:,26),Q(:,20),wf(:,418),Q(:,23))
  call counter_HHG_G(ctHEFTggh,wf(:,0),wf(:,-1),wf(:,28),Q(:,36),wf(:,419),Q(:,39))
  call counter_HG_G(ctHEFTggh,wf(:,4),wf(:,24),Q(:,12),wf(:,420),Q(:,15))
  call counter_HG_G(ctHEFTggh,wf(:,4),wf(:,26),Q(:,20),wf(:,421),Q(:,23))
  call counter_HG_G(ctHEFTggh,wf(:,4),wf(:,28),Q(:,36),wf(:,422),Q(:,39))
  call counter_V_V(ctGG,wf(:,9),Q(:,56),wf(:,423))
  call counter_V_V(ctGG,wf(:,10),Q(:,56),wf(:,424))
  call counter_V_V(ctGG,wf(:,11),Q(:,56),wf(:,425))
  call counter_V_V(ctGG,wf(:,13),Q(:,52),wf(:,426))
  call counter_V_V(ctGG,wf(:,14),Q(:,52),wf(:,427))
  call counter_V_V(ctGG,wf(:,15),Q(:,52),wf(:,428))
  call counter_V_V(ctGG,wf(:,17),Q(:,44),wf(:,429))
  call counter_V_V(ctGG,wf(:,18),Q(:,44),wf(:,430))
  call counter_V_V(ctGG,wf(:,19),Q(:,44),wf(:,431))
  call counter_V_V(ctGG,wf(:,21),Q(:,28),wf(:,432))
  call counter_V_V(ctGG,wf(:,22),Q(:,28),wf(:,433))
  call counter_V_V(ctGG,wf(:,23),Q(:,28),wf(:,434))
  call vert_HG_G(wf(:,4),wf(:,97),Q(:,36),wf(:,435),Q(:,39))
  call vert_HG_G(wf(:,4),wf(:,98),Q(:,20),wf(:,436),Q(:,23))
  call vert_HG_G(wf(:,4),wf(:,99),Q(:,12),wf(:,437),Q(:,15))
  call vert_UV_W(wf(:,164),Q(:,5),wf(:,101),Q(:,34),wf(:,438))
  call vert_UV_W(wf(:,164),Q(:,5),wf(:,105),Q(:,18),wf(:,439))
  call vert_UV_W(wf(:,164),Q(:,5),wf(:,109),Q(:,10),wf(:,440))
  call vert_UV_W(wf(:,175),Q(:,9),wf(:,101),Q(:,34),wf(:,441))
  call vert_UV_W(wf(:,175),Q(:,9),wf(:,105),Q(:,18),wf(:,442))
  call vert_UV_W(wf(:,183),Q(:,17),wf(:,101),Q(:,34),wf(:,443))
  call vert_UV_W(wf(:,105),Q(:,18),wf(:,188),Q(:,33),wf(:,444))
  call vert_UV_W(wf(:,109),Q(:,10),wf(:,183),Q(:,17),wf(:,445))
  call vert_UV_W(wf(:,109),Q(:,10),wf(:,188),Q(:,33),wf(:,446))
  call vert_UV_W(wf(:,113),Q(:,6),wf(:,175),Q(:,9),wf(:,447))
  call vert_UV_W(wf(:,113),Q(:,6),wf(:,183),Q(:,17),wf(:,448))
  call vert_UV_W(wf(:,113),Q(:,6),wf(:,188),Q(:,33),wf(:,449))
  call vert_HHG_G(wf(:,0),wf(:,-1),wf(:,34),Q(:,48),wf(:,450),Q(:,51))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,20),Q(:,35),wf(:,451))
  call vert_UV_W(wf(:,16),Q(:,19),wf(:,-5),Q(:,32),wf(:,452))
  call vert_HHG_G(wf(:,0),wf(:,-1),wf(:,32),Q(:,40),wf(:,453),Q(:,43))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,20),Q(:,35),wf(:,454))
  call vert_HHG_G(wf(:,0),wf(:,-1),wf(:,30),Q(:,24),wf(:,455),Q(:,27))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,20),Q(:,35),wf(:,456))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,16),Q(:,19),wf(:,457))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,16),Q(:,19),wf(:,458))
  call vert_UV_W(wf(:,12),Q(:,11),wf(:,-5),Q(:,32),wf(:,459))
  call vert_UV_W(wf(:,12),Q(:,11),wf(:,-4),Q(:,16),wf(:,460))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,12),Q(:,11),wf(:,461))
  call vert_UV_W(wf(:,8),Q(:,7),wf(:,-5),Q(:,32),wf(:,462))
  call vert_UV_W(wf(:,8),Q(:,7),wf(:,-4),Q(:,16),wf(:,463))
  call vert_UV_W(wf(:,8),Q(:,7),wf(:,-3),Q(:,8),wf(:,464))
  call vert_UV_W(wf(:,309),Q(:,6),wf(:,117),Q(:,33),wf(:,465))
  call vert_UV_W(wf(:,309),Q(:,6),wf(:,121),Q(:,17),wf(:,466))
  call vert_UV_W(wf(:,309),Q(:,6),wf(:,125),Q(:,9),wf(:,467))
  call vert_UV_W(wf(:,320),Q(:,10),wf(:,117),Q(:,33),wf(:,468))
  call vert_UV_W(wf(:,320),Q(:,10),wf(:,121),Q(:,17),wf(:,469))
  call vert_UV_W(wf(:,328),Q(:,18),wf(:,117),Q(:,33),wf(:,470))
  call vert_UV_W(wf(:,121),Q(:,17),wf(:,333),Q(:,34),wf(:,471))
  call vert_UV_W(wf(:,125),Q(:,9),wf(:,328),Q(:,18),wf(:,472))
  call vert_UV_W(wf(:,125),Q(:,9),wf(:,333),Q(:,34),wf(:,473))
  call vert_UV_W(wf(:,129),Q(:,5),wf(:,320),Q(:,10),wf(:,474))
  call vert_UV_W(wf(:,129),Q(:,5),wf(:,328),Q(:,18),wf(:,475))
  call vert_UV_W(wf(:,129),Q(:,5),wf(:,333),Q(:,34),wf(:,476))
  call vert_HG_G(wf(:,4),wf(:,34),Q(:,48),wf(:,477),Q(:,51))
  call vert_UV_W(wf(:,303),Q(:,12),wf(:,-4),Q(:,16),wf(:,478))
  call vert_UV_W(wf(:,303),Q(:,12),wf(:,-5),Q(:,32),wf(:,479))
  call counter_V_V(ctGG,wf(:,44),Q(:,19),wf(:,480))
  call counter_V_V(ctGG,wf(:,47),Q(:,28),wf(:,481))
  call vert_HG_G(wf(:,4),wf(:,32),Q(:,40),wf(:,482),Q(:,43))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,304),Q(:,20),wf(:,483))
  call vert_UV_W(wf(:,304),Q(:,20),wf(:,-5),Q(:,32),wf(:,484))
  call counter_V_V(ctGG,wf(:,42),Q(:,11),wf(:,485))
  call counter_V_V(ctGG,wf(:,50),Q(:,28),wf(:,486))
  call vert_HG_G(wf(:,4),wf(:,30),Q(:,24),wf(:,487),Q(:,27))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,306),Q(:,24),wf(:,488))
  call counter_V_V(ctGG,wf(:,39),Q(:,7),wf(:,489))
  call counter_V_V(ctGG,wf(:,52),Q(:,28),wf(:,490))
  call vert_UV_W(wf(:,306),Q(:,24),wf(:,-5),Q(:,32),wf(:,491))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,305),Q(:,36),wf(:,492))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,305),Q(:,36),wf(:,493))
  call counter_V_V(ctGG,wf(:,53),Q(:,44),wf(:,494))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,307),Q(:,40),wf(:,495))
  call counter_V_V(ctGG,wf(:,54),Q(:,44),wf(:,496))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,307),Q(:,40),wf(:,497))
  call counter_V_V(ctGG,wf(:,57),Q(:,52),wf(:,498))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,308),Q(:,48),wf(:,499))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,308),Q(:,48),wf(:,500))
  call vert_UV_W(wf(:,39),Q(:,7),wf(:,-3),Q(:,8),wf(:,501))
  call vert_UV_W(wf(:,39),Q(:,7),wf(:,-4),Q(:,16),wf(:,502))
  call vert_UV_W(wf(:,39),Q(:,7),wf(:,-5),Q(:,32),wf(:,503))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,42),Q(:,11),wf(:,504))
  call vert_UV_W(wf(:,42),Q(:,11),wf(:,-4),Q(:,16),wf(:,505))
  call vert_UV_W(wf(:,42),Q(:,11),wf(:,-5),Q(:,32),wf(:,506))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,44),Q(:,19),wf(:,507))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,44),Q(:,19),wf(:,508))
  call vert_UV_W(wf(:,44),Q(:,19),wf(:,-5),Q(:,32),wf(:,509))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,45),Q(:,35),wf(:,510))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,45),Q(:,35),wf(:,511))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,45),Q(:,35),wf(:,512))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3) - MH2)
  den(2) = 1 / (Q(5,7))
  den(3) = 1 / (Q(5,11))
  den(4) = 1 / (Q(5,19))
  den(5) = 1 / (Q(5,35))
  den(6) = 1 / (Q(5,12))
  den(7) = 1 / (Q(5,20))
  den(8) = 1 / (Q(5,36))
  den(9) = 1 / (Q(5,24))
  den(10) = 1 / (Q(5,40))
  den(11) = 1 / (Q(5,48))
  den(15) = 1 / (Q(5,56))
  den(19) = 1 / (Q(5,52))
  den(22) = 1 / (Q(5,44))
  den(24) = 1 / (Q(5,28))
  den(65) = 1 / (Q(5,13))
  den(66) = 1 / (Q(5,21))
  den(67) = 1 / (Q(5,37))
  den(68) = 1 / (Q(5,25))
  den(69) = 1 / (Q(5,41))
  den(70) = 1 / (Q(5,49))
  den(71) = 1 / (Q(5,15))
  den(72) = 1 / (Q(5,23))
  den(73) = 1 / (Q(5,39))
  den(74) = 1 / (Q(5,27))
  den(75) = 1 / (Q(5,43))
  den(76) = 1 / (Q(5,51))
  den(77) = 1 / (Q(5,29))
  den(78) = 1 / (Q(5,45))
  den(79) = 1 / (Q(5,53))
  den(80) = 1 / (Q(5,57))
  den(81) = 1 / (Q(5,30))
  den(82) = 1 / (Q(5,46))
  den(83) = 1 / (Q(5,54))
  den(84) = 1 / (Q(5,58))
  den(85) = 1 / (Q(5,14))
  den(86) = 1 / (Q(5,22))
  den(87) = 1 / (Q(5,38))
  den(88) = 1 / (Q(5,26))
  den(89) = 1 / (Q(5,42))
  den(90) = 1 / (Q(5,50))
  den(91) = 1 / (Q(5,5))
  den(93) = 1 / (Q(5,34))
  den(95) = 1 / (Q(5,18))
  den(97) = 1 / (Q(5,10))
  den(99) = 1 / (Q(5,9))
  den(103) = 1 / (Q(5,17))
  den(106) = 1 / (Q(5,33))
  den(114) = 1 / (Q(5,6))

  ! denominators
  den(12) = den(1)*den(6)
  den(13) = den(1)*den(7)
  den(14) = den(1)*den(8)
  den(16) = den(1)*den(15)
  den(17) = den(1)*den(9)
  den(18) = den(1)*den(10)
  den(20) = den(1)*den(19)
  den(21) = den(1)*den(11)
  den(23) = den(1)*den(22)
  den(25) = den(1)*den(24)
  den(26) = den(6)*den(11)
  den(27) = den(5)*den(6)
  den(28) = den(4)*den(6)
  den(29) = den(7)*den(10)
  den(30) = den(5)*den(7)
  den(31) = den(8)*den(9)
  den(32) = den(5)*den(9)
  den(33) = den(4)*den(8)
  den(34) = den(4)*den(10)
  den(35) = den(3)*den(7)
  den(36) = den(3)*den(8)
  den(37) = den(3)*den(11)
  den(38) = den(2)*den(9)
  den(39) = den(2)*den(10)
  den(40) = den(2)*den(11)
  den(41) = den(11)*den(12)
  den(42) = den(1)*den(4)
  den(43) = den(6)*den(42)
  den(44) = den(6)*den(24)
  den(45) = den(1)*den(44)
  den(46) = den(10)*den(13)
  den(47) = den(1)*den(3)
  den(48) = den(7)*den(47)
  den(49) = den(7)*den(24)
  den(50) = den(1)*den(49)
  den(51) = den(9)*den(14)
  den(52) = den(1)*den(2)
  den(53) = den(9)*den(52)
  den(54) = den(9)*den(24)
  den(55) = den(1)*den(54)
  den(56) = den(8)*den(47)
  den(57) = den(8)*den(22)
  den(58) = den(1)*den(57)
  den(59) = den(10)*den(52)
  den(60) = den(10)*den(22)
  den(61) = den(1)*den(60)
  den(62) = den(11)*den(52)
  den(63) = den(11)*den(19)
  den(64) = den(1)*den(63)
  den(92) = den(15)*den(91)
  den(94) = den(91)*den(93)
  den(96) = den(91)*den(95)
  den(98) = den(91)*den(97)
  den(100) = den(19)*den(99)
  den(101) = den(93)*den(99)
  den(102) = den(95)*den(99)
  den(104) = den(22)*den(103)
  den(105) = den(93)*den(103)
  den(107) = den(24)*den(106)
  den(108) = den(24)*den(93)
  den(109) = den(95)*den(106)
  den(110) = den(22)*den(95)
  den(111) = den(97)*den(103)
  den(112) = den(97)*den(106)
  den(113) = den(19)*den(97)
  den(115) = den(99)*den(114)
  den(116) = den(103)*den(114)
  den(117) = den(106)*den(114)
  den(118) = den(15)*den(114)
  den(119) = den(9)*den(91)
  den(120) = den(10)*den(91)
  den(121) = den(90)*den(91)
  den(122) = den(11)*den(91)
  den(123) = den(89)*den(91)
  den(124) = den(88)*den(91)
  den(125) = den(7)*den(99)
  den(126) = den(8)*den(99)
  den(127) = den(90)*den(99)
  den(128) = den(6)*den(103)
  den(129) = den(6)*den(106)
  den(130) = den(6)*den(90)
  den(131) = den(8)*den(103)
  den(132) = den(89)*den(103)
  den(133) = den(7)*den(106)
  den(134) = den(7)*den(89)
  den(135) = den(88)*den(106)
  den(136) = den(8)*den(88)
  den(137) = den(11)*den(99)
  den(138) = den(87)*den(99)
  den(139) = den(86)*den(99)
  den(140) = den(10)*den(103)
  den(141) = den(87)*den(103)
  den(142) = den(9)*den(106)
  den(143) = den(9)*den(87)
  den(144) = den(86)*den(106)
  den(145) = den(10)*den(86)
  den(146) = den(85)*den(103)
  den(147) = den(85)*den(106)
  den(148) = den(11)*den(85)
  den(149) = den(2)*den(15)
  den(150) = den(3)*den(19)
  den(151) = den(4)*den(22)
  den(152) = den(5)*den(24)
  den(153) = den(6)*den(70)
  den(154) = den(6)*den(93)
  den(155) = den(6)*den(95)
  den(156) = den(7)*den(69)
  den(157) = den(7)*den(93)
  den(158) = den(8)*den(68)
  den(159) = den(68)*den(93)
  den(160) = den(8)*den(95)
  den(161) = den(69)*den(95)
  den(162) = den(7)*den(97)
  den(163) = den(8)*den(97)
  den(164) = den(70)*den(97)
  den(165) = den(9)*den(67)
  den(166) = den(9)*den(93)
  den(167) = den(10)*den(66)
  den(168) = den(66)*den(93)
  den(169) = den(10)*den(95)
  den(170) = den(67)*den(95)
  den(171) = den(11)*den(65)
  den(172) = den(65)*den(93)
  den(173) = den(65)*den(95)
  den(174) = den(11)*den(97)
  den(175) = den(67)*den(97)
  den(176) = den(66)*den(97)
  den(177) = den(9)*den(114)
  den(178) = den(10)*den(114)
  den(179) = den(70)*den(114)
  den(180) = den(11)*den(114)
  den(181) = den(69)*den(114)
  den(182) = den(68)*den(114)
  den(183) = den(6)*den(76)
  den(184) = den(7)*den(75)
  den(185) = den(8)*den(74)
  den(186) = den(9)*den(73)
  den(187) = den(10)*den(72)
  den(188) = den(11)*den(71)
  den(189) = den(1)*den(76)
  den(190) = den(6)*den(189)
  den(191) = den(1)*den(5)
  den(192) = den(6)*den(191)
  den(193) = den(6)*den(22)
  den(194) = den(1)*den(193)
  den(195) = den(1)*den(75)
  den(196) = den(7)*den(195)
  den(197) = den(1)*den(74)
  den(198) = den(8)*den(197)
  den(199) = den(15)*den(52)
  den(200) = den(7)*den(191)
  den(201) = den(7)*den(19)
  den(202) = den(1)*den(201)
  den(203) = den(8)*den(42)
  den(204) = den(8)*den(19)
  den(205) = den(1)*den(204)
  den(206) = den(1)*den(73)
  den(207) = den(9)*den(206)
  den(208) = den(1)*den(72)
  den(209) = den(10)*den(208)
  den(210) = den(19)*den(47)
  den(211) = den(1)*den(71)
  den(212) = den(11)*den(211)
  den(213) = den(22)*den(42)
  den(214) = den(24)*den(191)
  den(215) = den(9)*den(191)
  den(216) = den(9)*den(15)
  den(217) = den(1)*den(216)
  den(218) = den(10)*den(42)
  den(219) = den(10)*den(15)
  den(220) = den(1)*den(219)
  den(221) = den(11)*den(47)
  den(222) = den(11)*den(15)
  den(223) = den(1)*den(222)
  den(224) = den(67)*den(91)
  den(225) = den(9)*den(224)
  den(226) = den(91)*den(216)
  den(227) = den(9)*den(94)
  den(228) = den(66)*den(91)
  den(229) = den(10)*den(228)
  den(230) = den(91)*den(219)
  den(231) = den(65)*den(91)
  den(232) = den(11)*den(231)
  den(233) = den(91)*den(222)
  den(234) = den(93)*den(231)
  den(235) = den(89)*den(93)
  den(236) = den(91)*den(235)
  den(237) = den(10)*den(96)
  den(238) = den(95)*den(231)
  den(239) = den(88)*den(95)
  den(240) = den(91)*den(239)
  den(241) = den(11)*den(98)
  den(242) = den(97)*den(228)
  den(243) = den(88)*den(97)
  den(244) = den(91)*den(243)
  den(245) = den(69)*den(99)
  den(246) = den(7)*den(245)
  den(247) = den(99)*den(201)
  den(248) = den(7)*den(101)
  den(249) = den(68)*den(99)
  den(250) = den(8)*den(249)
  den(251) = den(99)*den(204)
  den(252) = den(65)*den(99)
  den(253) = den(11)*den(252)
  den(254) = den(63)*den(99)
  den(255) = den(93)*den(252)
  den(256) = den(87)*den(93)
  den(257) = den(99)*den(256)
  den(258) = den(8)*den(102)
  den(259) = den(95)*den(252)
  den(260) = den(86)*den(95)
  den(261) = den(99)*den(260)
  den(262) = den(70)*den(103)
  den(263) = den(6)*den(262)
  den(264) = den(103)*den(193)
  den(265) = den(6)*den(105)
  den(266) = den(70)*den(106)
  den(267) = den(6)*den(266)
  den(268) = den(44)*den(106)
  den(269) = den(6)*den(65)
  den(270) = den(11)*den(269)
  den(271) = den(11)*den(70)
  den(272) = den(6)*den(271)
  den(273) = den(93)*den(269)
  den(274) = den(5)*den(93)
  den(275) = den(6)*den(274)
  den(276) = den(6)*den(109)
  den(277) = den(95)*den(269)
  den(278) = den(4)*den(95)
  den(279) = den(6)*den(278)
  den(280) = den(68)*den(103)
  den(281) = den(8)*den(280)
  den(282) = den(57)*den(103)
  den(283) = den(66)*den(103)
  den(284) = den(10)*den(283)
  den(285) = den(60)*den(103)
  den(286) = den(93)*den(283)
  den(287) = den(103)*den(256)
  den(288) = den(69)*den(106)
  den(289) = den(7)*den(288)
  den(290) = den(49)*den(106)
  den(291) = den(7)*den(66)
  den(292) = den(10)*den(291)
  den(293) = den(10)*den(69)
  den(294) = den(7)*den(293)
  den(295) = den(93)*den(291)
  den(296) = den(7)*den(274)
  den(297) = den(67)*den(106)
  den(298) = den(9)*den(297)
  den(299) = den(54)*den(106)
  den(300) = den(8)*den(67)
  den(301) = den(9)*den(300)
  den(302) = den(9)*den(68)
  den(303) = den(8)*den(302)
  den(304) = den(93)*den(302)
  den(305) = den(9)*den(274)
  den(306) = den(95)*den(297)
  den(307) = den(106)*den(260)
  den(308) = den(95)*den(300)
  den(309) = den(8)*den(278)
  den(310) = den(95)*den(293)
  den(311) = den(10)*den(278)
  den(312) = den(8)*den(111)
  den(313) = den(97)*den(283)
  den(314) = den(85)*den(97)
  den(315) = den(103)*den(314)
  den(316) = den(7)*den(112)
  den(317) = den(97)*den(291)
  den(318) = den(3)*den(97)
  den(319) = den(7)*den(318)
  den(320) = den(97)*den(297)
  den(321) = den(106)*den(314)
  den(322) = den(97)*den(300)
  den(323) = den(8)*den(318)
  den(324) = den(97)*den(271)
  den(325) = den(11)*den(318)
  den(326) = den(11)*den(115)
  den(327) = den(114)*den(249)
  den(328) = den(86)*den(114)
  den(329) = den(99)*den(328)
  den(330) = den(10)*den(116)
  den(331) = den(114)*den(280)
  den(332) = den(85)*den(114)
  den(333) = den(103)*den(332)
  den(334) = den(9)*den(117)
  den(335) = den(114)*den(302)
  den(336) = den(2)*den(114)
  den(337) = den(9)*den(336)
  den(338) = den(114)*den(288)
  den(339) = den(106)*den(332)
  den(340) = den(114)*den(293)
  den(341) = den(10)*den(336)
  den(342) = den(114)*den(271)
  den(343) = den(11)*den(336)
  den(344) = den(11)*den(76)
  den(345) = den(6)*den(344)
  den(346) = den(6)*den(71)
  den(347) = den(11)*den(346)
  den(348) = den(5)*den(76)
  den(349) = den(6)*den(348)
  den(350) = den(5)*den(44)
  den(351) = den(4)*den(76)
  den(352) = den(6)*den(351)
  den(353) = den(4)*den(193)
  den(354) = den(10)*den(75)
  den(355) = den(7)*den(354)
  den(356) = den(7)*den(72)
  den(357) = den(10)*den(356)
  den(358) = den(5)*den(75)
  den(359) = den(7)*den(358)
  den(360) = den(5)*den(49)
  den(361) = den(9)*den(74)
  den(362) = den(8)*den(361)
  den(363) = den(8)*den(73)
  den(364) = den(9)*den(363)
  den(365) = den(5)*den(73)
  den(366) = den(9)*den(365)
  den(367) = den(5)*den(54)
  den(368) = den(4)*den(74)
  den(369) = den(8)*den(368)
  den(370) = den(4)*den(57)
  den(371) = den(4)*den(72)
  den(372) = den(10)*den(371)
  den(373) = den(4)*den(60)
  den(374) = den(3)*den(75)
  den(375) = den(7)*den(374)
  den(376) = den(3)*den(201)
  den(377) = den(3)*den(74)
  den(378) = den(8)*den(377)
  den(379) = den(3)*den(204)
  den(380) = den(3)*den(71)
  den(381) = den(11)*den(380)
  den(382) = den(3)*den(63)
  den(383) = den(2)*den(73)
  den(384) = den(9)*den(383)
  den(385) = den(2)*den(216)
  den(386) = den(2)*den(72)
  den(387) = den(10)*den(386)
  den(388) = den(2)*den(219)
  den(389) = den(2)*den(71)
  den(390) = den(11)*den(389)
  den(391) = den(2)*den(222)
  den(392) = den(114)*den(216)
  den(393) = den(87)*den(114)
  den(394) = den(9)*den(393)
  den(395) = den(114)*den(219)
  den(396) = den(10)*den(328)
  den(397) = den(114)*den(222)
  den(398) = den(11)*den(332)
  den(399) = den(97)*den(201)
  den(400) = den(89)*den(97)
  den(401) = den(7)*den(400)
  den(402) = den(97)*den(204)
  den(403) = den(8)*den(243)
  den(404) = den(63)*den(97)
  den(405) = den(11)*den(314)
  den(406) = den(95)*den(193)
  den(407) = den(90)*den(95)
  den(408) = den(6)*den(407)
  den(409) = den(44)*den(93)
  den(410) = den(90)*den(93)
  den(411) = den(6)*den(410)
  den(412) = den(11)*den(90)
  den(413) = den(6)*den(412)
  den(414) = den(6)*den(85)
  den(415) = den(11)*den(414)
  den(416) = den(5)*den(106)
  den(417) = den(6)*den(416)
  den(418) = den(106)*den(414)
  den(419) = den(4)*den(103)
  den(420) = den(6)*den(419)
  den(421) = den(103)*den(414)
  den(422) = den(57)*den(95)
  den(423) = den(8)*den(239)
  den(424) = den(60)*den(95)
  den(425) = den(10)*den(260)
  den(426) = den(49)*den(93)
  den(427) = den(7)*den(235)
  den(428) = den(10)*den(89)
  den(429) = den(7)*den(428)
  den(430) = den(7)*den(86)
  den(431) = den(10)*den(430)
  den(432) = den(7)*den(416)
  den(433) = den(106)*den(430)
  den(434) = den(54)*den(93)
  den(435) = den(9)*den(256)
  den(436) = den(9)*den(88)
  den(437) = den(8)*den(436)
  den(438) = den(8)*den(87)
  den(439) = den(9)*den(438)
  den(440) = den(9)*den(416)
  den(441) = den(106)*den(436)
  den(442) = den(8)*den(419)
  den(443) = den(103)*den(438)
  den(444) = den(10)*den(419)
  den(445) = den(103)*den(428)
  den(446) = den(3)*den(99)
  den(447) = den(7)*den(446)
  den(448) = den(99)*den(430)
  den(449) = den(8)*den(446)
  den(450) = den(99)*den(438)
  den(451) = den(11)*den(446)
  den(452) = den(99)*den(412)
  den(453) = den(2)*den(91)
  den(454) = den(9)*den(453)
  den(455) = den(91)*den(436)
  den(456) = den(10)*den(453)
  den(457) = den(91)*den(428)
  den(458) = den(11)*den(453)
  den(459) = den(91)*den(412)
  den(460) = den(21)*den(76)
  den(461) = den(6)*den(460)
  den(462) = den(12)*den(71)
  den(463) = den(11)*den(462)
  den(464) = den(6)**2
  den(465) = den(191)*den(464)
  den(466) = den(42)*den(464)
  den(467) = den(42)*den(193)
  den(468) = den(44)*den(191)
  den(469) = den(18)*den(75)
  den(470) = den(7)*den(469)
  den(471) = den(13)*den(72)
  den(472) = den(10)*den(471)
  den(473) = den(7)**2
  den(474) = den(191)*den(473)
  den(475) = den(47)*den(473)
  den(476) = den(47)*den(201)
  den(477) = den(49)*den(191)
  den(478) = den(17)*den(74)
  den(479) = den(8)*den(478)
  den(480) = den(14)*den(73)
  den(481) = den(9)*den(480)
  den(482) = den(9)**2
  den(483) = den(191)*den(482)
  den(484) = den(52)*den(216)
  den(485) = den(54)*den(191)
  den(486) = den(52)*den(482)
  den(487) = den(8)**2
  den(488) = den(42)*den(487)
  den(489) = den(47)*den(487)
  den(490) = den(47)*den(204)
  den(491) = den(42)*den(57)
  den(492) = den(10)**2
  den(493) = den(42)*den(492)
  den(494) = den(52)*den(219)
  den(495) = den(42)*den(60)
  den(496) = den(52)*den(492)
  den(497) = den(52)*den(222)
  den(498) = den(47)*den(63)
  den(499) = den(11)**2
  den(500) = den(47)*den(499)
  den(501) = den(52)*den(499)
  den(502) = den(52)*den(71)
  den(503) = den(52)*den(72)
  den(504) = den(52)*den(73)
  den(505) = den(47)*den(71)
  den(506) = den(47)*den(74)
  den(507) = den(47)*den(75)
  den(508) = den(42)*den(72)
  den(509) = den(42)*den(74)
  den(510) = den(42)*den(76)
  den(511) = den(73)*den(191)
  den(512) = den(75)*den(191)
  den(513) = den(76)*den(191)
  den(514) = den(1)*den(6)*den(11)
  den(515) = den(1)*den(7)*den(10)
  den(516) = den(1)*den(8)*den(9)
  den(517) = den(1)*den(26)
  den(518) = den(6)*den(21)
  den(519) = den(1)*den(29)
  den(520) = den(7)*den(18)
  den(521) = den(1)*den(31)
  den(522) = den(8)*den(17)
  den(523) = den(6)*den(510)
  den(524) = den(6)*den(513)
  den(525) = den(7)*den(507)
  den(526) = den(7)*den(512)
  den(527) = den(9)*den(504)
  den(528) = den(9)*den(511)
  den(529) = den(8)*den(506)
  den(530) = den(8)*den(509)
  den(531) = den(10)*den(503)
  den(532) = den(10)*den(508)
  den(533) = den(11)*den(502)
  den(534) = den(11)*den(505)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(822)

  A(1) = cont_VV(wf(:,-5),wf(:,1))
  A(2) = cont_VV(wf(:,-5),wf(:,2))
  A(3) = cont_VV(wf(:,-5),wf(:,3))
  A(4) = cont_SS(wf(:,4),wf(:,5)) * den(1)
  A(5) = cont_SS(wf(:,4),wf(:,6)) * den(1)
  A(6) = cont_SS(wf(:,4),wf(:,7)) * den(1)
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
  A(17) = cont_VV(wf(:,20),wf(:,22)) * den(5)
  A(18) = cont_VV(wf(:,20),wf(:,23)) * den(5)
  A(19) = cont_VV(wf(:,24),wf(:,25)) * den(6)
  A(20) = cont_VV(wf(:,26),wf(:,27)) * den(7)
  A(21) = cont_VV(wf(:,28),wf(:,29)) * den(8)
  A(22) = cont_VV(wf(:,30),wf(:,31)) * den(9)
  A(23) = cont_VV(wf(:,32),wf(:,33)) * den(10)
  A(24) = cont_VV(wf(:,34),wf(:,35)) * den(11)
  A(25) = cont_VV(wf(:,24),wf(:,36)) * den(12)
  A(26) = cont_VV(wf(:,26),wf(:,37)) * den(13)
  A(27) = cont_VV(wf(:,28),wf(:,38)) * den(14)
  A(28) = cont_VV(wf(:,9),wf(:,39)) * den(16)
  A(29) = cont_VV(wf(:,10),wf(:,39)) * den(16)
  A(30) = cont_VV(wf(:,11),wf(:,39)) * den(16)
  A(31) = cont_VV(wf(:,30),wf(:,40)) * den(17)
  A(32) = cont_VV(wf(:,32),wf(:,41)) * den(18)
  A(33) = cont_VV(wf(:,13),wf(:,42)) * den(20)
  A(34) = cont_VV(wf(:,14),wf(:,42)) * den(20)
  A(35) = cont_VV(wf(:,15),wf(:,42)) * den(20)
  A(36) = cont_VV(wf(:,34),wf(:,43)) * den(21)
  A(37) = cont_VV(wf(:,17),wf(:,44)) * den(23)
  A(38) = cont_VV(wf(:,18),wf(:,44)) * den(23)
  A(39) = cont_VV(wf(:,19),wf(:,44)) * den(23)
  A(40) = cont_VV(wf(:,21),wf(:,45)) * den(25)
  A(41) = cont_VV(wf(:,22),wf(:,45)) * den(25)
  A(42) = cont_VV(wf(:,23),wf(:,45)) * den(25)
  A(43) = cont_VV(wf(:,34),wf(:,46)) * den(26)
  A(44) = cont_VV(wf(:,20),wf(:,47)) * den(27)
  A(45) = cont_VV(wf(:,16),wf(:,48)) * den(28)
  A(46) = cont_VV(wf(:,32),wf(:,49)) * den(29)
  A(47) = cont_VV(wf(:,20),wf(:,50)) * den(30)
  A(48) = cont_VV(wf(:,30),wf(:,51)) * den(31)
  A(49) = cont_VV(wf(:,20),wf(:,52)) * den(32)
  A(50) = cont_VV(wf(:,16),wf(:,53)) * den(33)
  A(51) = cont_VV(wf(:,16),wf(:,54)) * den(34)
  A(52) = cont_VV(wf(:,12),wf(:,55)) * den(35)
  A(53) = cont_VV(wf(:,12),wf(:,56)) * den(36)
  A(54) = cont_VV(wf(:,12),wf(:,57)) * den(37)
  A(55) = cont_VV(wf(:,8),wf(:,58)) * den(38)
  A(56) = cont_VV(wf(:,8),wf(:,59)) * den(39)
  A(57) = cont_VV(wf(:,8),wf(:,60)) * den(40)
  A(58) = cont_VV(wf(:,34),wf(:,61)) * den(41)
  A(59) = cont_VV(wf(:,44),wf(:,48)) * den(43)
  A(60) = cont_VV(wf(:,45),wf(:,47)) * den(45)
  A(61) = cont_VV(wf(:,32),wf(:,62)) * den(46)
  A(62) = cont_VV(wf(:,42),wf(:,55)) * den(48)
  A(63) = cont_VV(wf(:,45),wf(:,50)) * den(50)
  A(64) = cont_VV(wf(:,30),wf(:,63)) * den(51)
  A(65) = cont_VV(wf(:,39),wf(:,58)) * den(53)
  A(66) = cont_VV(wf(:,45),wf(:,52)) * den(55)
  A(67) = cont_VV(wf(:,42),wf(:,56)) * den(56)
  A(68) = cont_VV(wf(:,44),wf(:,53)) * den(58)
  A(69) = cont_VV(wf(:,39),wf(:,59)) * den(59)
  A(70) = cont_VV(wf(:,44),wf(:,54)) * den(61)
  A(71) = cont_VV(wf(:,39),wf(:,60)) * den(62)
  A(72) = cont_VV(wf(:,42),wf(:,57)) * den(64)

  A(73) = cont_VV(wf(:,-5),wf(:,64))
  A(74) = cont_VV(wf(:,-5),wf(:,65))
  A(75) = cont_VV(wf(:,-5),wf(:,66))
  A(76) = cont_VV(wf(:,-5),wf(:,64))
  A(77) = cont_VV(wf(:,-5),wf(:,65))
  A(78) = cont_VV(wf(:,-5),wf(:,66))
  A(79) = cont_SS(wf(:,4),wf(:,67)) * den(1)
  A(80) = cont_SS(wf(:,4),wf(:,68)) * den(1)
  A(81) = cont_SS(wf(:,4),wf(:,69)) * den(1)
  A(82) = cont_SS(wf(:,4),wf(:,67)) * den(1)
  A(83) = cont_SS(wf(:,4),wf(:,68)) * den(1)
  A(84) = cont_SS(wf(:,4),wf(:,69)) * den(1)
  A(85) = cont_VV(wf(:,8),wf(:,70)) * den(2)
  A(86) = cont_VV(wf(:,8),wf(:,71)) * den(2)
  A(87) = cont_VV(wf(:,8),wf(:,72)) * den(2)
  A(88) = cont_VV(wf(:,8),wf(:,70)) * den(2)
  A(89) = cont_VV(wf(:,8),wf(:,71)) * den(2)
  A(90) = cont_VV(wf(:,8),wf(:,72)) * den(2)
  A(91) = cont_VV(wf(:,12),wf(:,73)) * den(3)
  A(92) = cont_VV(wf(:,12),wf(:,74)) * den(3)
  A(93) = cont_VV(wf(:,12),wf(:,75)) * den(3)
  A(94) = cont_VV(wf(:,12),wf(:,73)) * den(3)
  A(95) = cont_VV(wf(:,12),wf(:,74)) * den(3)
  A(96) = cont_VV(wf(:,12),wf(:,75)) * den(3)
  A(97) = cont_VV(wf(:,16),wf(:,76)) * den(4)
  A(98) = cont_VV(wf(:,16),wf(:,77)) * den(4)
  A(99) = cont_VV(wf(:,16),wf(:,78)) * den(4)
  A(100) = cont_VV(wf(:,16),wf(:,76)) * den(4)
  A(101) = cont_VV(wf(:,16),wf(:,77)) * den(4)
  A(102) = cont_VV(wf(:,16),wf(:,78)) * den(4)
  A(103) = cont_VV(wf(:,20),wf(:,79)) * den(5)
  A(104) = cont_VV(wf(:,20),wf(:,80)) * den(5)
  A(105) = cont_VV(wf(:,20),wf(:,81)) * den(5)
  A(106) = cont_VV(wf(:,20),wf(:,79)) * den(5)
  A(107) = cont_VV(wf(:,20),wf(:,80)) * den(5)
  A(108) = cont_VV(wf(:,20),wf(:,81)) * den(5)
  A(109) = cont_VV(wf(:,82),wf(:,83)) * den(65)
  A(110) = cont_VV(wf(:,84),wf(:,85)) * den(66)
  A(111) = cont_VV(wf(:,86),wf(:,87)) * den(67)
  A(112) = cont_VV(wf(:,88),wf(:,89)) * den(68)
  A(113) = cont_VV(wf(:,90),wf(:,91)) * den(69)
  A(114) = cont_VV(wf(:,92),wf(:,93)) * den(70)
  A(115) = cont_VV(wf(:,35),wf(:,94)) * den(71)
  A(116) = cont_VV(wf(:,33),wf(:,95)) * den(72)
  A(117) = cont_VV(wf(:,31),wf(:,96)) * den(73)
  A(118) = cont_VV(wf(:,29),wf(:,97)) * den(74)
  A(119) = cont_VV(wf(:,27),wf(:,98)) * den(75)
  A(120) = cont_VV(wf(:,25),wf(:,99)) * den(76)
  A(121) = cont_VV(wf(:,100),wf(:,101)) * den(77)
  A(122) = cont_VV(wf(:,101),wf(:,102)) * den(77)
  A(123) = cont_VV(wf(:,101),wf(:,103)) * den(77)
  A(124) = cont_VV(wf(:,104),wf(:,105)) * den(78)
  A(125) = cont_VV(wf(:,105),wf(:,106)) * den(78)
  A(126) = cont_VV(wf(:,105),wf(:,107)) * den(78)
  A(127) = cont_VV(wf(:,108),wf(:,109)) * den(79)
  A(128) = cont_VV(wf(:,109),wf(:,110)) * den(79)
  A(129) = cont_VV(wf(:,109),wf(:,111)) * den(79)
  A(130) = cont_VV(wf(:,112),wf(:,113)) * den(80)
  A(131) = cont_VV(wf(:,113),wf(:,114)) * den(80)
  A(132) = cont_VV(wf(:,113),wf(:,115)) * den(80)
  A(133) = cont_VV(wf(:,116),wf(:,117)) * den(81)
  A(134) = cont_VV(wf(:,117),wf(:,118)) * den(81)
  A(135) = cont_VV(wf(:,117),wf(:,119)) * den(81)
  A(136) = cont_VV(wf(:,120),wf(:,121)) * den(82)
  A(137) = cont_VV(wf(:,121),wf(:,122)) * den(82)
  A(138) = cont_VV(wf(:,121),wf(:,123)) * den(82)
  A(139) = cont_VV(wf(:,124),wf(:,125)) * den(83)
  A(140) = cont_VV(wf(:,125),wf(:,126)) * den(83)
  A(141) = cont_VV(wf(:,125),wf(:,127)) * den(83)
  A(142) = cont_VV(wf(:,128),wf(:,129)) * den(84)
  A(143) = cont_VV(wf(:,129),wf(:,130)) * den(84)
  A(144) = cont_VV(wf(:,129),wf(:,131)) * den(84)
  A(145) = cont_VV(wf(:,132),wf(:,133)) * den(85)
  A(146) = cont_VV(wf(:,134),wf(:,135)) * den(86)
  A(147) = cont_VV(wf(:,136),wf(:,137)) * den(87)
  A(148) = cont_VV(wf(:,138),wf(:,139)) * den(88)
  A(149) = cont_VV(wf(:,140),wf(:,141)) * den(89)
  A(150) = cont_VV(wf(:,142),wf(:,143)) * den(90)
  A(151) = cont_VV(wf(:,21),wf(:,144)) * den(24)
  A(152) = cont_VV(wf(:,22),wf(:,144)) * den(24)
  A(153) = cont_VV(wf(:,23),wf(:,144)) * den(24)
  A(154) = cont_VV(wf(:,17),wf(:,145)) * den(22)
  A(155) = cont_VV(wf(:,18),wf(:,145)) * den(22)
  A(156) = cont_VV(wf(:,19),wf(:,145)) * den(22)
  A(157) = cont_VV(wf(:,13),wf(:,146)) * den(19)
  A(158) = cont_VV(wf(:,14),wf(:,146)) * den(19)
  A(159) = cont_VV(wf(:,15),wf(:,146)) * den(19)
  A(160) = cont_VV(wf(:,9),wf(:,147)) * den(15)
  A(161) = cont_VV(wf(:,10),wf(:,147)) * den(15)
  A(162) = cont_VV(wf(:,11),wf(:,147)) * den(15)
  A(163) = cont_VV(wf(:,24),wf(:,148)) * den(6)
  A(164) = cont_VV(wf(:,26),wf(:,149)) * den(7)
  A(165) = cont_VV(wf(:,28),wf(:,150)) * den(8)
  A(166) = cont_VV(wf(:,30),wf(:,151)) * den(9)
  A(167) = cont_VV(wf(:,32),wf(:,152)) * den(10)
  A(168) = cont_VV(wf(:,34),wf(:,153)) * den(11)
  A(169) = cont_VV(wf(:,24),wf(:,154)) * den(12)
  A(170) = cont_VV(wf(:,26),wf(:,155)) * den(13)
  A(171) = cont_VV(wf(:,28),wf(:,156)) * den(14)
  A(172) = cont_VV(wf(:,39),wf(:,70)) * den(16)
  A(173) = cont_VV(wf(:,39),wf(:,71)) * den(16)
  A(174) = cont_VV(wf(:,39),wf(:,72)) * den(16)
  A(175) = cont_VV(wf(:,39),wf(:,70)) * den(16)
  A(176) = cont_VV(wf(:,39),wf(:,71)) * den(16)
  A(177) = cont_VV(wf(:,39),wf(:,72)) * den(16)
  A(178) = cont_VV(wf(:,21),wf(:,157)) * den(25)
  A(179) = cont_VV(wf(:,22),wf(:,157)) * den(25)
  A(180) = cont_VV(wf(:,23),wf(:,157)) * den(25)
  A(181) = cont_VV(wf(:,17),wf(:,158)) * den(23)
  A(182) = cont_VV(wf(:,18),wf(:,158)) * den(23)
  A(183) = cont_VV(wf(:,19),wf(:,158)) * den(23)
  A(184) = cont_VV(wf(:,43),wf(:,94)) * den(21)
  A(185) = cont_VV(wf(:,13),wf(:,159)) * den(20)
  A(186) = cont_VV(wf(:,14),wf(:,159)) * den(20)
  A(187) = cont_VV(wf(:,15),wf(:,159)) * den(20)
  A(188) = cont_VV(wf(:,41),wf(:,95)) * den(18)
  A(189) = cont_VV(wf(:,40),wf(:,96)) * den(17)
  A(190) = cont_VV(wf(:,9),wf(:,160)) * den(16)
  A(191) = cont_VV(wf(:,10),wf(:,160)) * den(16)
  A(192) = cont_VV(wf(:,11),wf(:,160)) * den(16)
  A(193) = cont_VV(wf(:,38),wf(:,97)) * den(14)
  A(194) = cont_VV(wf(:,37),wf(:,98)) * den(13)
  A(195) = cont_VV(wf(:,36),wf(:,99)) * den(12)
  A(196) = cont_VV(wf(:,30),wf(:,161)) * den(17)
  A(197) = cont_VV(wf(:,32),wf(:,162)) * den(18)
  A(198) = cont_VV(wf(:,42),wf(:,73)) * den(20)
  A(199) = cont_VV(wf(:,42),wf(:,74)) * den(20)
  A(200) = cont_VV(wf(:,42),wf(:,75)) * den(20)
  A(201) = cont_VV(wf(:,42),wf(:,73)) * den(20)
  A(202) = cont_VV(wf(:,42),wf(:,74)) * den(20)
  A(203) = cont_VV(wf(:,42),wf(:,75)) * den(20)
  A(204) = cont_VV(wf(:,34),wf(:,163)) * den(21)
  A(205) = cont_VV(wf(:,44),wf(:,76)) * den(23)
  A(206) = cont_VV(wf(:,44),wf(:,77)) * den(23)
  A(207) = cont_VV(wf(:,44),wf(:,78)) * den(23)
  A(208) = cont_VV(wf(:,44),wf(:,76)) * den(23)
  A(209) = cont_VV(wf(:,44),wf(:,77)) * den(23)
  A(210) = cont_VV(wf(:,44),wf(:,78)) * den(23)
  A(211) = cont_VV(wf(:,45),wf(:,79)) * den(25)
  A(212) = cont_VV(wf(:,45),wf(:,80)) * den(25)
  A(213) = cont_VV(wf(:,45),wf(:,81)) * den(25)
  A(214) = cont_VV(wf(:,45),wf(:,79)) * den(25)
  A(215) = cont_VV(wf(:,45),wf(:,80)) * den(25)
  A(216) = cont_VV(wf(:,45),wf(:,81)) * den(25)
  A(217) = cont_VV(wf(:,9),wf(:,165)) * den(92)
  A(218) = cont_VV(wf(:,10),wf(:,165)) * den(92)
  A(219) = cont_VV(wf(:,11),wf(:,165)) * den(92)
  A(220) = cont_VV(wf(:,101),wf(:,166)) * den(94)
  A(221) = cont_VV(wf(:,101),wf(:,167)) * den(94)
  A(222) = cont_VV(wf(:,101),wf(:,168)) * den(94)
  A(223) = cont_VV(wf(:,105),wf(:,169)) * den(96)
  A(224) = cont_VV(wf(:,105),wf(:,170)) * den(96)
  A(225) = cont_VV(wf(:,105),wf(:,171)) * den(96)
  A(226) = cont_VV(wf(:,109),wf(:,172)) * den(98)
  A(227) = cont_VV(wf(:,109),wf(:,173)) * den(98)
  A(228) = cont_VV(wf(:,109),wf(:,174)) * den(98)
  A(229) = cont_VV(wf(:,13),wf(:,176)) * den(100)
  A(230) = cont_VV(wf(:,14),wf(:,176)) * den(100)
  A(231) = cont_VV(wf(:,15),wf(:,176)) * den(100)
  A(232) = cont_VV(wf(:,101),wf(:,177)) * den(101)
  A(233) = cont_VV(wf(:,101),wf(:,178)) * den(101)
  A(234) = cont_VV(wf(:,101),wf(:,179)) * den(101)
  A(235) = cont_VV(wf(:,105),wf(:,180)) * den(102)
  A(236) = cont_VV(wf(:,105),wf(:,181)) * den(102)
  A(237) = cont_VV(wf(:,105),wf(:,182)) * den(102)
  A(238) = cont_VV(wf(:,17),wf(:,184)) * den(104)
  A(239) = cont_VV(wf(:,18),wf(:,184)) * den(104)
  A(240) = cont_VV(wf(:,19),wf(:,184)) * den(104)
  A(241) = cont_VV(wf(:,101),wf(:,185)) * den(105)
  A(242) = cont_VV(wf(:,101),wf(:,186)) * den(105)
  A(243) = cont_VV(wf(:,101),wf(:,187)) * den(105)
  A(244) = cont_VV(wf(:,21),wf(:,189)) * den(107)
  A(245) = cont_VV(wf(:,22),wf(:,189)) * den(107)
  A(246) = cont_VV(wf(:,23),wf(:,189)) * den(107)
  A(247) = cont_VV(wf(:,21),wf(:,190)) * den(108)
  A(248) = cont_VV(wf(:,22),wf(:,190)) * den(108)
  A(249) = cont_VV(wf(:,23),wf(:,190)) * den(108)
  A(250) = cont_VV(wf(:,105),wf(:,191)) * den(109)
  A(251) = cont_VV(wf(:,105),wf(:,192)) * den(109)
  A(252) = cont_VV(wf(:,105),wf(:,193)) * den(109)
  A(253) = cont_VV(wf(:,17),wf(:,194)) * den(110)
  A(254) = cont_VV(wf(:,18),wf(:,194)) * den(110)
  A(255) = cont_VV(wf(:,19),wf(:,194)) * den(110)
  A(256) = cont_VV(wf(:,109),wf(:,195)) * den(111)
  A(257) = cont_VV(wf(:,109),wf(:,196)) * den(111)
  A(258) = cont_VV(wf(:,109),wf(:,197)) * den(111)
  A(259) = cont_VV(wf(:,109),wf(:,198)) * den(112)
  A(260) = cont_VV(wf(:,109),wf(:,199)) * den(112)
  A(261) = cont_VV(wf(:,109),wf(:,200)) * den(112)
  A(262) = cont_VV(wf(:,13),wf(:,201)) * den(113)
  A(263) = cont_VV(wf(:,14),wf(:,201)) * den(113)
  A(264) = cont_VV(wf(:,15),wf(:,201)) * den(113)
  A(265) = cont_VV(wf(:,113),wf(:,202)) * den(115)
  A(266) = cont_VV(wf(:,113),wf(:,203)) * den(115)
  A(267) = cont_VV(wf(:,113),wf(:,204)) * den(115)
  A(268) = cont_VV(wf(:,113),wf(:,205)) * den(116)
  A(269) = cont_VV(wf(:,113),wf(:,206)) * den(116)
  A(270) = cont_VV(wf(:,113),wf(:,207)) * den(116)
  A(271) = cont_VV(wf(:,113),wf(:,208)) * den(117)
  A(272) = cont_VV(wf(:,113),wf(:,209)) * den(117)
  A(273) = cont_VV(wf(:,113),wf(:,210)) * den(117)
  A(274) = cont_VV(wf(:,9),wf(:,211)) * den(118)
  A(275) = cont_VV(wf(:,10),wf(:,211)) * den(118)
  A(276) = cont_VV(wf(:,11),wf(:,211)) * den(118)
  A(277) = cont_VV(wf(:,30),wf(:,212)) * den(119)
  A(278) = cont_VV(wf(:,32),wf(:,213)) * den(120)
  A(279) = cont_VV(wf(:,83),wf(:,214)) * den(121)
  A(280) = cont_VV(wf(:,34),wf(:,215)) * den(122)
  A(281) = cont_VV(wf(:,85),wf(:,216)) * den(123)
  A(282) = cont_VV(wf(:,87),wf(:,217)) * den(124)
  A(283) = cont_VV(wf(:,26),wf(:,218)) * den(125)
  A(284) = cont_VV(wf(:,28),wf(:,219)) * den(126)
  A(285) = cont_VV(wf(:,83),wf(:,220)) * den(127)
  A(286) = cont_VV(wf(:,24),wf(:,221)) * den(128)
  A(287) = cont_VV(wf(:,24),wf(:,222)) * den(129)
  A(288) = cont_VV(wf(:,83),wf(:,223)) * den(130)
  A(289) = cont_VV(wf(:,28),wf(:,224)) * den(131)
  A(290) = cont_VV(wf(:,85),wf(:,225)) * den(132)
  A(291) = cont_VV(wf(:,26),wf(:,226)) * den(133)
  A(292) = cont_VV(wf(:,85),wf(:,227)) * den(134)
  A(293) = cont_VV(wf(:,87),wf(:,228)) * den(135)
  A(294) = cont_VV(wf(:,87),wf(:,229)) * den(136)
  A(295) = cont_VV(wf(:,34),wf(:,230)) * den(137)
  A(296) = cont_VV(wf(:,89),wf(:,231)) * den(138)
  A(297) = cont_VV(wf(:,91),wf(:,232)) * den(139)
  A(298) = cont_VV(wf(:,32),wf(:,233)) * den(140)
  A(299) = cont_VV(wf(:,89),wf(:,234)) * den(141)
  A(300) = cont_VV(wf(:,30),wf(:,235)) * den(142)
  A(301) = cont_VV(wf(:,89),wf(:,236)) * den(143)
  A(302) = cont_VV(wf(:,91),wf(:,237)) * den(144)
  A(303) = cont_VV(wf(:,91),wf(:,238)) * den(145)
  A(304) = cont_VV(wf(:,93),wf(:,239)) * den(146)
  A(305) = cont_VV(wf(:,93),wf(:,240)) * den(147)
  A(306) = cont_VV(wf(:,93),wf(:,241)) * den(148)
  A(307) = cont_VV(wf(:,16),wf(:,242)) * den(28)
  A(308) = cont_VV(wf(:,20),wf(:,243)) * den(27)
  A(309) = cont_VV(wf(:,46),wf(:,94)) * den(26)
  A(310) = cont_VV(wf(:,12),wf(:,244)) * den(35)
  A(311) = cont_VV(wf(:,12),wf(:,245)) * den(36)
  A(312) = cont_VV(wf(:,12),wf(:,246)) * den(37)
  A(313) = cont_VV(wf(:,20),wf(:,247)) * den(30)
  A(314) = cont_VV(wf(:,49),wf(:,95)) * den(29)
  A(315) = cont_VV(wf(:,16),wf(:,248)) * den(33)
  A(316) = cont_VV(wf(:,16),wf(:,249)) * den(34)
  A(317) = cont_VV(wf(:,51),wf(:,96)) * den(31)
  A(318) = cont_VV(wf(:,20),wf(:,250)) * den(32)
  A(319) = cont_VV(wf(:,8),wf(:,251)) * den(38)
  A(320) = cont_VV(wf(:,8),wf(:,252)) * den(39)
  A(321) = cont_VV(wf(:,8),wf(:,253)) * den(40)
  A(322) = cont_VV(wf(:,9),wf(:,254)) * den(149)
  A(323) = cont_VV(wf(:,10),wf(:,254)) * den(149)
  A(324) = cont_VV(wf(:,11),wf(:,254)) * den(149)
  A(325) = cont_VV(wf(:,8),wf(:,255)) * den(40)
  A(326) = cont_VV(wf(:,8),wf(:,256)) * den(39)
  A(327) = cont_VV(wf(:,8),wf(:,257)) * den(38)
  A(328) = cont_VV(wf(:,13),wf(:,258)) * den(150)
  A(329) = cont_VV(wf(:,14),wf(:,258)) * den(150)
  A(330) = cont_VV(wf(:,15),wf(:,258)) * den(150)
  A(331) = cont_VV(wf(:,17),wf(:,259)) * den(151)
  A(332) = cont_VV(wf(:,18),wf(:,259)) * den(151)
  A(333) = cont_VV(wf(:,19),wf(:,259)) * den(151)
  A(334) = cont_VV(wf(:,21),wf(:,260)) * den(152)
  A(335) = cont_VV(wf(:,22),wf(:,260)) * den(152)
  A(336) = cont_VV(wf(:,23),wf(:,260)) * den(152)
  A(337) = cont_VV(wf(:,20),wf(:,261)) * den(32)
  A(338) = cont_VV(wf(:,30),wf(:,262)) * den(31)
  A(339) = cont_VV(wf(:,16),wf(:,263)) * den(34)
  A(340) = cont_VV(wf(:,16),wf(:,264)) * den(33)
  A(341) = cont_VV(wf(:,32),wf(:,265)) * den(29)
  A(342) = cont_VV(wf(:,20),wf(:,266)) * den(30)
  A(343) = cont_VV(wf(:,12),wf(:,267)) * den(37)
  A(344) = cont_VV(wf(:,12),wf(:,268)) * den(36)
  A(345) = cont_VV(wf(:,12),wf(:,269)) * den(35)
  A(346) = cont_VV(wf(:,34),wf(:,270)) * den(26)
  A(347) = cont_VV(wf(:,20),wf(:,271)) * den(27)
  A(348) = cont_VV(wf(:,16),wf(:,272)) * den(28)
  A(349) = cont_VV(wf(:,92),wf(:,273)) * den(153)
  A(350) = cont_VV(wf(:,24),wf(:,274)) * den(154)
  A(351) = cont_VV(wf(:,24),wf(:,275)) * den(155)
  A(352) = cont_VV(wf(:,90),wf(:,276)) * den(156)
  A(353) = cont_VV(wf(:,26),wf(:,277)) * den(157)
  A(354) = cont_VV(wf(:,88),wf(:,278)) * den(158)
  A(355) = cont_VV(wf(:,88),wf(:,279)) * den(159)
  A(356) = cont_VV(wf(:,28),wf(:,280)) * den(160)
  A(357) = cont_VV(wf(:,90),wf(:,281)) * den(161)
  A(358) = cont_VV(wf(:,26),wf(:,282)) * den(162)
  A(359) = cont_VV(wf(:,28),wf(:,283)) * den(163)
  A(360) = cont_VV(wf(:,92),wf(:,284)) * den(164)
  A(361) = cont_VV(wf(:,86),wf(:,285)) * den(165)
  A(362) = cont_VV(wf(:,30),wf(:,286)) * den(166)
  A(363) = cont_VV(wf(:,84),wf(:,287)) * den(167)
  A(364) = cont_VV(wf(:,84),wf(:,288)) * den(168)
  A(365) = cont_VV(wf(:,32),wf(:,289)) * den(169)
  A(366) = cont_VV(wf(:,86),wf(:,290)) * den(170)
  A(367) = cont_VV(wf(:,82),wf(:,291)) * den(171)
  A(368) = cont_VV(wf(:,82),wf(:,292)) * den(172)
  A(369) = cont_VV(wf(:,82),wf(:,293)) * den(173)
  A(370) = cont_VV(wf(:,34),wf(:,294)) * den(174)
  A(371) = cont_VV(wf(:,86),wf(:,295)) * den(175)
  A(372) = cont_VV(wf(:,84),wf(:,296)) * den(176)
  A(373) = cont_VV(wf(:,30),wf(:,297)) * den(177)
  A(374) = cont_VV(wf(:,32),wf(:,298)) * den(178)
  A(375) = cont_VV(wf(:,92),wf(:,299)) * den(179)
  A(376) = cont_VV(wf(:,34),wf(:,300)) * den(180)
  A(377) = cont_VV(wf(:,90),wf(:,301)) * den(181)
  A(378) = cont_VV(wf(:,88),wf(:,302)) * den(182)
  A(379) = cont_VV(wf(:,25),wf(:,303)) * den(183)
  A(380) = cont_VV(wf(:,27),wf(:,304)) * den(184)
  A(381) = cont_VV(wf(:,29),wf(:,305)) * den(185)
  A(382) = cont_VV(wf(:,31),wf(:,306)) * den(186)
  A(383) = cont_VV(wf(:,33),wf(:,307)) * den(187)
  A(384) = cont_VV(wf(:,35),wf(:,308)) * den(188)
  A(385) = cont_VV(wf(:,9),wf(:,310)) * den(118)
  A(386) = cont_VV(wf(:,10),wf(:,310)) * den(118)
  A(387) = cont_VV(wf(:,11),wf(:,310)) * den(118)
  A(388) = cont_VV(wf(:,309),wf(:,311)) * den(117)
  A(389) = cont_VV(wf(:,309),wf(:,312)) * den(117)
  A(390) = cont_VV(wf(:,309),wf(:,313)) * den(117)
  A(391) = cont_VV(wf(:,309),wf(:,314)) * den(116)
  A(392) = cont_VV(wf(:,309),wf(:,315)) * den(116)
  A(393) = cont_VV(wf(:,309),wf(:,316)) * den(116)
  A(394) = cont_VV(wf(:,309),wf(:,317)) * den(115)
  A(395) = cont_VV(wf(:,309),wf(:,318)) * den(115)
  A(396) = cont_VV(wf(:,309),wf(:,319)) * den(115)
  A(397) = cont_VV(wf(:,13),wf(:,321)) * den(113)
  A(398) = cont_VV(wf(:,14),wf(:,321)) * den(113)
  A(399) = cont_VV(wf(:,15),wf(:,321)) * den(113)
  A(400) = cont_VV(wf(:,320),wf(:,322)) * den(112)
  A(401) = cont_VV(wf(:,320),wf(:,323)) * den(112)
  A(402) = cont_VV(wf(:,320),wf(:,324)) * den(112)
  A(403) = cont_VV(wf(:,320),wf(:,325)) * den(111)
  A(404) = cont_VV(wf(:,320),wf(:,326)) * den(111)
  A(405) = cont_VV(wf(:,320),wf(:,327)) * den(111)
  A(406) = cont_VV(wf(:,17),wf(:,329)) * den(110)
  A(407) = cont_VV(wf(:,18),wf(:,329)) * den(110)
  A(408) = cont_VV(wf(:,19),wf(:,329)) * den(110)
  A(409) = cont_VV(wf(:,328),wf(:,330)) * den(109)
  A(410) = cont_VV(wf(:,328),wf(:,331)) * den(109)
  A(411) = cont_VV(wf(:,328),wf(:,332)) * den(109)
  A(412) = cont_VV(wf(:,21),wf(:,334)) * den(108)
  A(413) = cont_VV(wf(:,22),wf(:,334)) * den(108)
  A(414) = cont_VV(wf(:,23),wf(:,334)) * den(108)
  A(415) = cont_VV(wf(:,21),wf(:,335)) * den(107)
  A(416) = cont_VV(wf(:,22),wf(:,335)) * den(107)
  A(417) = cont_VV(wf(:,23),wf(:,335)) * den(107)
  A(418) = cont_VV(wf(:,333),wf(:,336)) * den(105)
  A(419) = cont_VV(wf(:,333),wf(:,337)) * den(105)
  A(420) = cont_VV(wf(:,333),wf(:,338)) * den(105)
  A(421) = cont_VV(wf(:,17),wf(:,339)) * den(104)
  A(422) = cont_VV(wf(:,18),wf(:,339)) * den(104)
  A(423) = cont_VV(wf(:,19),wf(:,339)) * den(104)
  A(424) = cont_VV(wf(:,328),wf(:,340)) * den(102)
  A(425) = cont_VV(wf(:,328),wf(:,341)) * den(102)
  A(426) = cont_VV(wf(:,328),wf(:,342)) * den(102)
  A(427) = cont_VV(wf(:,333),wf(:,343)) * den(101)
  A(428) = cont_VV(wf(:,333),wf(:,344)) * den(101)
  A(429) = cont_VV(wf(:,333),wf(:,345)) * den(101)
  A(430) = cont_VV(wf(:,13),wf(:,346)) * den(100)
  A(431) = cont_VV(wf(:,14),wf(:,346)) * den(100)
  A(432) = cont_VV(wf(:,15),wf(:,346)) * den(100)
  A(433) = cont_VV(wf(:,320),wf(:,347)) * den(98)
  A(434) = cont_VV(wf(:,320),wf(:,348)) * den(98)
  A(435) = cont_VV(wf(:,320),wf(:,349)) * den(98)
  A(436) = cont_VV(wf(:,328),wf(:,350)) * den(96)
  A(437) = cont_VV(wf(:,328),wf(:,351)) * den(96)
  A(438) = cont_VV(wf(:,328),wf(:,352)) * den(96)
  A(439) = cont_VV(wf(:,333),wf(:,353)) * den(94)
  A(440) = cont_VV(wf(:,333),wf(:,354)) * den(94)
  A(441) = cont_VV(wf(:,333),wf(:,355)) * den(94)
  A(442) = cont_VV(wf(:,9),wf(:,356)) * den(92)
  A(443) = cont_VV(wf(:,10),wf(:,356)) * den(92)
  A(444) = cont_VV(wf(:,11),wf(:,356)) * den(92)
  A(445) = cont_VV(wf(:,142),wf(:,357)) * den(130)
  A(446) = cont_VV(wf(:,24),wf(:,358)) * den(129)
  A(447) = cont_VV(wf(:,24),wf(:,359)) * den(128)
  A(448) = cont_VV(wf(:,140),wf(:,360)) * den(134)
  A(449) = cont_VV(wf(:,26),wf(:,361)) * den(133)
  A(450) = cont_VV(wf(:,138),wf(:,362)) * den(136)
  A(451) = cont_VV(wf(:,138),wf(:,363)) * den(135)
  A(452) = cont_VV(wf(:,28),wf(:,364)) * den(131)
  A(453) = cont_VV(wf(:,140),wf(:,365)) * den(132)
  A(454) = cont_VV(wf(:,26),wf(:,366)) * den(125)
  A(455) = cont_VV(wf(:,28),wf(:,367)) * den(126)
  A(456) = cont_VV(wf(:,142),wf(:,368)) * den(127)
  A(457) = cont_VV(wf(:,136),wf(:,369)) * den(143)
  A(458) = cont_VV(wf(:,30),wf(:,370)) * den(142)
  A(459) = cont_VV(wf(:,134),wf(:,371)) * den(145)
  A(460) = cont_VV(wf(:,134),wf(:,372)) * den(144)
  A(461) = cont_VV(wf(:,32),wf(:,373)) * den(140)
  A(462) = cont_VV(wf(:,136),wf(:,374)) * den(141)
  A(463) = cont_VV(wf(:,132),wf(:,375)) * den(148)
  A(464) = cont_VV(wf(:,132),wf(:,376)) * den(147)
  A(465) = cont_VV(wf(:,132),wf(:,377)) * den(146)
  A(466) = cont_VV(wf(:,34),wf(:,378)) * den(137)
  A(467) = cont_VV(wf(:,136),wf(:,379)) * den(138)
  A(468) = cont_VV(wf(:,134),wf(:,380)) * den(139)
  A(469) = cont_VV(wf(:,30),wf(:,381)) * den(119)
  A(470) = cont_VV(wf(:,32),wf(:,382)) * den(120)
  A(471) = cont_VV(wf(:,142),wf(:,383)) * den(121)
  A(472) = cont_VV(wf(:,34),wf(:,384)) * den(122)
  A(473) = cont_VV(wf(:,140),wf(:,385)) * den(123)
  A(474) = cont_VV(wf(:,138),wf(:,386)) * den(124)
  A(475) = cont_VV(wf(:,30),wf(:,387)) * den(177)
  A(476) = cont_VV(wf(:,32),wf(:,388)) * den(178)
  A(477) = cont_VV(wf(:,133),wf(:,389)) * den(179)
  A(478) = cont_VV(wf(:,34),wf(:,390)) * den(180)
  A(479) = cont_VV(wf(:,135),wf(:,391)) * den(181)
  A(480) = cont_VV(wf(:,137),wf(:,392)) * den(182)
  A(481) = cont_VV(wf(:,26),wf(:,393)) * den(162)
  A(482) = cont_VV(wf(:,28),wf(:,394)) * den(163)
  A(483) = cont_VV(wf(:,133),wf(:,395)) * den(164)
  A(484) = cont_VV(wf(:,24),wf(:,396)) * den(155)
  A(485) = cont_VV(wf(:,24),wf(:,397)) * den(154)
  A(486) = cont_VV(wf(:,133),wf(:,398)) * den(153)
  A(487) = cont_VV(wf(:,28),wf(:,399)) * den(160)
  A(488) = cont_VV(wf(:,135),wf(:,400)) * den(161)
  A(489) = cont_VV(wf(:,26),wf(:,401)) * den(157)
  A(490) = cont_VV(wf(:,135),wf(:,402)) * den(156)
  A(491) = cont_VV(wf(:,137),wf(:,403)) * den(159)
  A(492) = cont_VV(wf(:,137),wf(:,404)) * den(158)
  A(493) = cont_VV(wf(:,34),wf(:,405)) * den(174)
  A(494) = cont_VV(wf(:,139),wf(:,406)) * den(175)
  A(495) = cont_VV(wf(:,141),wf(:,407)) * den(176)
  A(496) = cont_VV(wf(:,32),wf(:,408)) * den(169)
  A(497) = cont_VV(wf(:,139),wf(:,409)) * den(170)
  A(498) = cont_VV(wf(:,30),wf(:,410)) * den(166)
  A(499) = cont_VV(wf(:,139),wf(:,411)) * den(165)
  A(500) = cont_VV(wf(:,141),wf(:,412)) * den(168)
  A(501) = cont_VV(wf(:,141),wf(:,413)) * den(167)
  A(502) = cont_VV(wf(:,143),wf(:,414)) * den(173)
  A(503) = cont_VV(wf(:,143),wf(:,415)) * den(172)
  A(504) = cont_VV(wf(:,143),wf(:,416)) * den(171)
  A(505) = cont_VV(wf(:,34),wf(:,417)) * den(26)
  A(506) = cont_VV(wf(:,47),wf(:,144)) * den(27)
  A(507) = cont_VV(wf(:,48),wf(:,145)) * den(28)
  A(508) = cont_VV(wf(:,32),wf(:,418)) * den(29)
  A(509) = cont_VV(wf(:,50),wf(:,144)) * den(30)
  A(510) = cont_VV(wf(:,30),wf(:,419)) * den(31)
  A(511) = cont_VV(wf(:,52),wf(:,144)) * den(32)
  A(512) = cont_VV(wf(:,53),wf(:,145)) * den(33)
  A(513) = cont_VV(wf(:,54),wf(:,145)) * den(34)
  A(514) = cont_VV(wf(:,55),wf(:,146)) * den(35)
  A(515) = cont_VV(wf(:,56),wf(:,146)) * den(36)
  A(516) = cont_VV(wf(:,57),wf(:,146)) * den(37)
  A(517) = cont_VV(wf(:,58),wf(:,147)) * den(38)
  A(518) = cont_VV(wf(:,59),wf(:,147)) * den(39)
  A(519) = cont_VV(wf(:,60),wf(:,147)) * den(40)
  A(520) = cont_VV(wf(:,34),wf(:,420)) * den(41)
  A(521) = cont_VV(wf(:,44),wf(:,242)) * den(43)
  A(522) = cont_VV(wf(:,47),wf(:,157)) * den(45)
  A(523) = cont_VV(wf(:,36),wf(:,303)) * den(190)
  A(524) = cont_VV(wf(:,45),wf(:,243)) * den(192)
  A(525) = cont_VV(wf(:,48),wf(:,158)) * den(194)
  A(526) = cont_VV(wf(:,61),wf(:,94)) * den(41)
  A(527) = cont_VV(wf(:,32),wf(:,421)) * den(46)
  A(528) = cont_VV(wf(:,42),wf(:,244)) * den(48)
  A(529) = cont_VV(wf(:,50),wf(:,157)) * den(50)
  A(530) = cont_VV(wf(:,30),wf(:,422)) * den(51)
  A(531) = cont_VV(wf(:,39),wf(:,251)) * den(53)
  A(532) = cont_VV(wf(:,52),wf(:,157)) * den(55)
  A(533) = cont_VV(wf(:,42),wf(:,245)) * den(56)
  A(534) = cont_VV(wf(:,53),wf(:,158)) * den(58)
  A(535) = cont_VV(wf(:,39),wf(:,252)) * den(59)
  A(536) = cont_VV(wf(:,54),wf(:,158)) * den(61)
  A(537) = cont_VV(wf(:,39),wf(:,253)) * den(62)
  A(538) = cont_VV(wf(:,42),wf(:,246)) * den(64)
  A(539) = cont_VV(wf(:,37),wf(:,304)) * den(196)
  A(540) = cont_VV(wf(:,38),wf(:,305)) * den(198)
  A(541) = cont_VV(wf(:,39),wf(:,423)) * den(199)
  A(542) = cont_VV(wf(:,39),wf(:,424)) * den(199)
  A(543) = cont_VV(wf(:,39),wf(:,425)) * den(199)
  A(544) = cont_VV(wf(:,45),wf(:,247)) * den(200)
  A(545) = cont_VV(wf(:,55),wf(:,159)) * den(202)
  A(546) = cont_VV(wf(:,62),wf(:,95)) * den(46)
  A(547) = cont_VV(wf(:,44),wf(:,248)) * den(203)
  A(548) = cont_VV(wf(:,56),wf(:,159)) * den(205)
  A(549) = cont_VV(wf(:,39),wf(:,255)) * den(62)
  A(550) = cont_VV(wf(:,57),wf(:,159)) * den(64)
  A(551) = cont_VV(wf(:,39),wf(:,256)) * den(59)
  A(552) = cont_VV(wf(:,44),wf(:,249)) * den(61)
  A(553) = cont_VV(wf(:,63),wf(:,96)) * den(51)
  A(554) = cont_VV(wf(:,39),wf(:,257)) * den(53)
  A(555) = cont_VV(wf(:,45),wf(:,250)) * den(55)
  A(556) = cont_VV(wf(:,40),wf(:,306)) * den(207)
  A(557) = cont_VV(wf(:,41),wf(:,307)) * den(209)
  A(558) = cont_VV(wf(:,42),wf(:,426)) * den(210)
  A(559) = cont_VV(wf(:,42),wf(:,427)) * den(210)
  A(560) = cont_VV(wf(:,42),wf(:,428)) * den(210)
  A(561) = cont_VV(wf(:,43),wf(:,308)) * den(212)
  A(562) = cont_VV(wf(:,44),wf(:,429)) * den(213)
  A(563) = cont_VV(wf(:,44),wf(:,430)) * den(213)
  A(564) = cont_VV(wf(:,44),wf(:,431)) * den(213)
  A(565) = cont_VV(wf(:,45),wf(:,432)) * den(214)
  A(566) = cont_VV(wf(:,45),wf(:,433)) * den(214)
  A(567) = cont_VV(wf(:,45),wf(:,434)) * den(214)
  A(568) = cont_VV(wf(:,45),wf(:,261)) * den(215)
  A(569) = cont_VV(wf(:,58),wf(:,160)) * den(217)
  A(570) = cont_VV(wf(:,30),wf(:,435)) * den(51)
  A(571) = cont_VV(wf(:,44),wf(:,263)) * den(218)
  A(572) = cont_VV(wf(:,59),wf(:,160)) * den(220)
  A(573) = cont_VV(wf(:,42),wf(:,267)) * den(221)
  A(574) = cont_VV(wf(:,60),wf(:,160)) * den(223)
  A(575) = cont_VV(wf(:,42),wf(:,268)) * den(56)
  A(576) = cont_VV(wf(:,44),wf(:,264)) * den(58)
  A(577) = cont_VV(wf(:,32),wf(:,436)) * den(46)
  A(578) = cont_VV(wf(:,42),wf(:,269)) * den(48)
  A(579) = cont_VV(wf(:,45),wf(:,266)) * den(50)
  A(580) = cont_VV(wf(:,34),wf(:,437)) * den(41)
  A(581) = cont_VV(wf(:,44),wf(:,272)) * den(43)
  A(582) = cont_VV(wf(:,45),wf(:,271)) * den(45)
  A(583) = cont_VV(wf(:,217),wf(:,285)) * den(225)
  A(584) = cont_VV(wf(:,58),wf(:,165)) * den(226)
  A(585) = cont_VV(wf(:,30),wf(:,438)) * den(227)
  A(586) = cont_VV(wf(:,216),wf(:,287)) * den(229)
  A(587) = cont_VV(wf(:,59),wf(:,165)) * den(230)
  A(588) = cont_VV(wf(:,214),wf(:,291)) * den(232)
  A(589) = cont_VV(wf(:,60),wf(:,165)) * den(233)
  A(590) = cont_VV(wf(:,214),wf(:,292)) * den(234)
  A(591) = cont_VV(wf(:,216),wf(:,288)) * den(236)
  A(592) = cont_VV(wf(:,32),wf(:,439)) * den(237)
  A(593) = cont_VV(wf(:,214),wf(:,293)) * den(238)
  A(594) = cont_VV(wf(:,217),wf(:,290)) * den(240)
  A(595) = cont_VV(wf(:,34),wf(:,440)) * den(241)
  A(596) = cont_VV(wf(:,216),wf(:,296)) * den(242)
  A(597) = cont_VV(wf(:,217),wf(:,295)) * den(244)
  A(598) = cont_VV(wf(:,232),wf(:,276)) * den(246)
  A(599) = cont_VV(wf(:,55),wf(:,176)) * den(247)
  A(600) = cont_VV(wf(:,26),wf(:,441)) * den(248)
  A(601) = cont_VV(wf(:,231),wf(:,278)) * den(250)
  A(602) = cont_VV(wf(:,56),wf(:,176)) * den(251)
  A(603) = cont_VV(wf(:,220),wf(:,291)) * den(253)
  A(604) = cont_VV(wf(:,57),wf(:,176)) * den(254)
  A(605) = cont_VV(wf(:,220),wf(:,292)) * den(255)
  A(606) = cont_VV(wf(:,231),wf(:,279)) * den(257)
  A(607) = cont_VV(wf(:,28),wf(:,442)) * den(258)
  A(608) = cont_VV(wf(:,220),wf(:,293)) * den(259)
  A(609) = cont_VV(wf(:,232),wf(:,281)) * den(261)
  A(610) = cont_VV(wf(:,239),wf(:,273)) * den(263)
  A(611) = cont_VV(wf(:,48),wf(:,184)) * den(264)
  A(612) = cont_VV(wf(:,24),wf(:,443)) * den(265)
  A(613) = cont_VV(wf(:,240),wf(:,273)) * den(267)
  A(614) = cont_VV(wf(:,47),wf(:,189)) * den(268)
  A(615) = cont_VV(wf(:,223),wf(:,291)) * den(270)
  A(616) = cont_VV(wf(:,241),wf(:,273)) * den(272)
  A(617) = cont_VV(wf(:,223),wf(:,292)) * den(273)
  A(618) = cont_VV(wf(:,47),wf(:,190)) * den(275)
  A(619) = cont_VV(wf(:,24),wf(:,444)) * den(276)
  A(620) = cont_VV(wf(:,223),wf(:,293)) * den(277)
  A(621) = cont_VV(wf(:,48),wf(:,194)) * den(279)
  A(622) = cont_VV(wf(:,234),wf(:,278)) * den(281)
  A(623) = cont_VV(wf(:,53),wf(:,184)) * den(282)
  A(624) = cont_VV(wf(:,225),wf(:,287)) * den(284)
  A(625) = cont_VV(wf(:,54),wf(:,184)) * den(285)
  A(626) = cont_VV(wf(:,225),wf(:,288)) * den(286)
  A(627) = cont_VV(wf(:,234),wf(:,279)) * den(287)
  A(628) = cont_VV(wf(:,237),wf(:,276)) * den(289)
  A(629) = cont_VV(wf(:,50),wf(:,189)) * den(290)
  A(630) = cont_VV(wf(:,227),wf(:,287)) * den(292)
  A(631) = cont_VV(wf(:,238),wf(:,276)) * den(294)
  A(632) = cont_VV(wf(:,227),wf(:,288)) * den(295)
  A(633) = cont_VV(wf(:,50),wf(:,190)) * den(296)
  A(634) = cont_VV(wf(:,228),wf(:,285)) * den(298)
  A(635) = cont_VV(wf(:,52),wf(:,189)) * den(299)
  A(636) = cont_VV(wf(:,229),wf(:,285)) * den(301)
  A(637) = cont_VV(wf(:,236),wf(:,278)) * den(303)
  A(638) = cont_VV(wf(:,236),wf(:,279)) * den(304)
  A(639) = cont_VV(wf(:,52),wf(:,190)) * den(305)
  A(640) = cont_VV(wf(:,228),wf(:,290)) * den(306)
  A(641) = cont_VV(wf(:,237),wf(:,281)) * den(307)
  A(642) = cont_VV(wf(:,229),wf(:,290)) * den(308)
  A(643) = cont_VV(wf(:,53),wf(:,194)) * den(309)
  A(644) = cont_VV(wf(:,238),wf(:,281)) * den(310)
  A(645) = cont_VV(wf(:,54),wf(:,194)) * den(311)
  A(646) = cont_VV(wf(:,28),wf(:,445)) * den(312)
  A(647) = cont_VV(wf(:,225),wf(:,296)) * den(313)
  A(648) = cont_VV(wf(:,239),wf(:,284)) * den(315)
  A(649) = cont_VV(wf(:,26),wf(:,446)) * den(316)
  A(650) = cont_VV(wf(:,227),wf(:,296)) * den(317)
  A(651) = cont_VV(wf(:,55),wf(:,201)) * den(319)
  A(652) = cont_VV(wf(:,228),wf(:,295)) * den(320)
  A(653) = cont_VV(wf(:,240),wf(:,284)) * den(321)
  A(654) = cont_VV(wf(:,229),wf(:,295)) * den(322)
  A(655) = cont_VV(wf(:,56),wf(:,201)) * den(323)
  A(656) = cont_VV(wf(:,241),wf(:,284)) * den(324)
  A(657) = cont_VV(wf(:,57),wf(:,201)) * den(325)
  A(658) = cont_VV(wf(:,34),wf(:,447)) * den(326)
  A(659) = cont_VV(wf(:,231),wf(:,302)) * den(327)
  A(660) = cont_VV(wf(:,232),wf(:,301)) * den(329)
  A(661) = cont_VV(wf(:,32),wf(:,448)) * den(330)
  A(662) = cont_VV(wf(:,234),wf(:,302)) * den(331)
  A(663) = cont_VV(wf(:,239),wf(:,299)) * den(333)
  A(664) = cont_VV(wf(:,30),wf(:,449)) * den(334)
  A(665) = cont_VV(wf(:,236),wf(:,302)) * den(335)
  A(666) = cont_VV(wf(:,58),wf(:,211)) * den(337)
  A(667) = cont_VV(wf(:,237),wf(:,301)) * den(338)
  A(668) = cont_VV(wf(:,240),wf(:,299)) * den(339)
  A(669) = cont_VV(wf(:,238),wf(:,301)) * den(340)
  A(670) = cont_VV(wf(:,59),wf(:,211)) * den(341)
  A(671) = cont_VV(wf(:,241),wf(:,299)) * den(342)
  A(672) = cont_VV(wf(:,60),wf(:,211)) * den(343)
  A(673) = cont_VV(wf(:,303),wf(:,450)) * den(345)
  A(674) = cont_VV(wf(:,46),wf(:,308)) * den(347)
  A(675) = cont_VV(wf(:,303),wf(:,451)) * den(349)
  A(676) = cont_VV(wf(:,47),wf(:,260)) * den(350)
  A(677) = cont_VV(wf(:,303),wf(:,452)) * den(352)
  A(678) = cont_VV(wf(:,48),wf(:,259)) * den(353)
  A(679) = cont_VV(wf(:,304),wf(:,453)) * den(355)
  A(680) = cont_VV(wf(:,49),wf(:,307)) * den(357)
  A(681) = cont_VV(wf(:,304),wf(:,454)) * den(359)
  A(682) = cont_VV(wf(:,50),wf(:,260)) * den(360)
  A(683) = cont_VV(wf(:,305),wf(:,455)) * den(362)
  A(684) = cont_VV(wf(:,51),wf(:,306)) * den(364)
  A(685) = cont_VV(wf(:,306),wf(:,456)) * den(366)
  A(686) = cont_VV(wf(:,52),wf(:,260)) * den(367)
  A(687) = cont_VV(wf(:,305),wf(:,457)) * den(369)
  A(688) = cont_VV(wf(:,53),wf(:,259)) * den(370)
  A(689) = cont_VV(wf(:,307),wf(:,458)) * den(372)
  A(690) = cont_VV(wf(:,54),wf(:,259)) * den(373)
  A(691) = cont_VV(wf(:,304),wf(:,459)) * den(375)
  A(692) = cont_VV(wf(:,55),wf(:,258)) * den(376)
  A(693) = cont_VV(wf(:,305),wf(:,460)) * den(378)
  A(694) = cont_VV(wf(:,56),wf(:,258)) * den(379)
  A(695) = cont_VV(wf(:,308),wf(:,461)) * den(381)
  A(696) = cont_VV(wf(:,57),wf(:,258)) * den(382)
  A(697) = cont_VV(wf(:,306),wf(:,462)) * den(384)
  A(698) = cont_VV(wf(:,58),wf(:,254)) * den(385)
  A(699) = cont_VV(wf(:,307),wf(:,463)) * den(387)
  A(700) = cont_VV(wf(:,59),wf(:,254)) * den(388)
  A(701) = cont_VV(wf(:,308),wf(:,464)) * den(390)
  A(702) = cont_VV(wf(:,60),wf(:,254)) * den(391)
  A(703) = cont_VV(wf(:,58),wf(:,310)) * den(392)
  A(704) = cont_VV(wf(:,369),wf(:,392)) * den(394)
  A(705) = cont_VV(wf(:,30),wf(:,465)) * den(334)
  A(706) = cont_VV(wf(:,59),wf(:,310)) * den(395)
  A(707) = cont_VV(wf(:,371),wf(:,391)) * den(396)
  A(708) = cont_VV(wf(:,60),wf(:,310)) * den(397)
  A(709) = cont_VV(wf(:,375),wf(:,389)) * den(398)
  A(710) = cont_VV(wf(:,372),wf(:,391)) * den(338)
  A(711) = cont_VV(wf(:,376),wf(:,389)) * den(339)
  A(712) = cont_VV(wf(:,32),wf(:,466)) * den(330)
  A(713) = cont_VV(wf(:,374),wf(:,392)) * den(331)
  A(714) = cont_VV(wf(:,377),wf(:,389)) * den(333)
  A(715) = cont_VV(wf(:,34),wf(:,467)) * den(326)
  A(716) = cont_VV(wf(:,379),wf(:,392)) * den(327)
  A(717) = cont_VV(wf(:,380),wf(:,391)) * den(329)
  A(718) = cont_VV(wf(:,55),wf(:,321)) * den(399)
  A(719) = cont_VV(wf(:,360),wf(:,407)) * den(401)
  A(720) = cont_VV(wf(:,26),wf(:,468)) * den(316)
  A(721) = cont_VV(wf(:,56),wf(:,321)) * den(402)
  A(722) = cont_VV(wf(:,362),wf(:,406)) * den(403)
  A(723) = cont_VV(wf(:,57),wf(:,321)) * den(404)
  A(724) = cont_VV(wf(:,375),wf(:,395)) * den(405)
  A(725) = cont_VV(wf(:,363),wf(:,406)) * den(320)
  A(726) = cont_VV(wf(:,376),wf(:,395)) * den(321)
  A(727) = cont_VV(wf(:,28),wf(:,469)) * den(312)
  A(728) = cont_VV(wf(:,365),wf(:,407)) * den(313)
  A(729) = cont_VV(wf(:,377),wf(:,395)) * den(315)
  A(730) = cont_VV(wf(:,48),wf(:,329)) * den(406)
  A(731) = cont_VV(wf(:,357),wf(:,414)) * den(408)
  A(732) = cont_VV(wf(:,24),wf(:,470)) * den(276)
  A(733) = cont_VV(wf(:,47),wf(:,334)) * den(409)
  A(734) = cont_VV(wf(:,357),wf(:,415)) * den(411)
  A(735) = cont_VV(wf(:,357),wf(:,416)) * den(413)
  A(736) = cont_VV(wf(:,375),wf(:,398)) * den(415)
  A(737) = cont_VV(wf(:,47),wf(:,335)) * den(417)
  A(738) = cont_VV(wf(:,376),wf(:,398)) * den(418)
  A(739) = cont_VV(wf(:,24),wf(:,471)) * den(265)
  A(740) = cont_VV(wf(:,48),wf(:,339)) * den(420)
  A(741) = cont_VV(wf(:,377),wf(:,398)) * den(421)
  A(742) = cont_VV(wf(:,53),wf(:,329)) * den(422)
  A(743) = cont_VV(wf(:,362),wf(:,409)) * den(423)
  A(744) = cont_VV(wf(:,54),wf(:,329)) * den(424)
  A(745) = cont_VV(wf(:,371),wf(:,400)) * den(425)
  A(746) = cont_VV(wf(:,363),wf(:,409)) * den(306)
  A(747) = cont_VV(wf(:,372),wf(:,400)) * den(307)
  A(748) = cont_VV(wf(:,50),wf(:,334)) * den(426)
  A(749) = cont_VV(wf(:,360),wf(:,412)) * den(427)
  A(750) = cont_VV(wf(:,360),wf(:,413)) * den(429)
  A(751) = cont_VV(wf(:,371),wf(:,402)) * den(431)
  A(752) = cont_VV(wf(:,50),wf(:,335)) * den(432)
  A(753) = cont_VV(wf(:,372),wf(:,402)) * den(433)
  A(754) = cont_VV(wf(:,52),wf(:,334)) * den(434)
  A(755) = cont_VV(wf(:,369),wf(:,403)) * den(435)
  A(756) = cont_VV(wf(:,362),wf(:,411)) * den(437)
  A(757) = cont_VV(wf(:,369),wf(:,404)) * den(439)
  A(758) = cont_VV(wf(:,52),wf(:,335)) * den(440)
  A(759) = cont_VV(wf(:,363),wf(:,411)) * den(441)
  A(760) = cont_VV(wf(:,365),wf(:,412)) * den(286)
  A(761) = cont_VV(wf(:,374),wf(:,403)) * den(287)
  A(762) = cont_VV(wf(:,53),wf(:,339)) * den(442)
  A(763) = cont_VV(wf(:,374),wf(:,404)) * den(443)
  A(764) = cont_VV(wf(:,54),wf(:,339)) * den(444)
  A(765) = cont_VV(wf(:,365),wf(:,413)) * den(445)
  A(766) = cont_VV(wf(:,28),wf(:,472)) * den(258)
  A(767) = cont_VV(wf(:,368),wf(:,414)) * den(259)
  A(768) = cont_VV(wf(:,380),wf(:,400)) * den(261)
  A(769) = cont_VV(wf(:,26),wf(:,473)) * den(248)
  A(770) = cont_VV(wf(:,55),wf(:,346)) * den(447)
  A(771) = cont_VV(wf(:,380),wf(:,402)) * den(448)
  A(772) = cont_VV(wf(:,368),wf(:,415)) * den(255)
  A(773) = cont_VV(wf(:,379),wf(:,403)) * den(257)
  A(774) = cont_VV(wf(:,56),wf(:,346)) * den(449)
  A(775) = cont_VV(wf(:,379),wf(:,404)) * den(450)
  A(776) = cont_VV(wf(:,57),wf(:,346)) * den(451)
  A(777) = cont_VV(wf(:,368),wf(:,416)) * den(452)
  A(778) = cont_VV(wf(:,34),wf(:,474)) * den(241)
  A(779) = cont_VV(wf(:,385),wf(:,407)) * den(242)
  A(780) = cont_VV(wf(:,386),wf(:,406)) * den(244)
  A(781) = cont_VV(wf(:,32),wf(:,475)) * den(237)
  A(782) = cont_VV(wf(:,383),wf(:,414)) * den(238)
  A(783) = cont_VV(wf(:,386),wf(:,409)) * den(240)
  A(784) = cont_VV(wf(:,30),wf(:,476)) * den(227)
  A(785) = cont_VV(wf(:,58),wf(:,356)) * den(454)
  A(786) = cont_VV(wf(:,386),wf(:,411)) * den(455)
  A(787) = cont_VV(wf(:,383),wf(:,415)) * den(234)
  A(788) = cont_VV(wf(:,385),wf(:,412)) * den(236)
  A(789) = cont_VV(wf(:,59),wf(:,356)) * den(456)
  A(790) = cont_VV(wf(:,385),wf(:,413)) * den(457)
  A(791) = cont_VV(wf(:,60),wf(:,356)) * den(458)
  A(792) = cont_VV(wf(:,383),wf(:,416)) * den(459)
  A(793) = cont_VV(wf(:,303),wf(:,477)) * den(461)
  A(794) = cont_VV(wf(:,61),wf(:,308)) * den(463)
  A(795) = cont_VV(wf(:,45),wf(:,478)) * den(465)
  A(796) = cont_VV(wf(:,44),wf(:,479)) * den(466)
  A(797) = cont_VV(wf(:,48),wf(:,480)) * den(467)
  A(798) = cont_VV(wf(:,45),wf(:,481)) * den(468)
  A(799) = cont_VV(wf(:,304),wf(:,482)) * den(470)
  A(800) = cont_VV(wf(:,62),wf(:,307)) * den(472)
  A(801) = cont_VV(wf(:,45),wf(:,483)) * den(474)
  A(802) = cont_VV(wf(:,42),wf(:,484)) * den(475)
  A(803) = cont_VV(wf(:,55),wf(:,485)) * den(476)
  A(804) = cont_VV(wf(:,45),wf(:,486)) * den(477)
  A(805) = cont_VV(wf(:,305),wf(:,487)) * den(479)
  A(806) = cont_VV(wf(:,63),wf(:,306)) * den(481)
  A(807) = cont_VV(wf(:,45),wf(:,488)) * den(483)
  A(808) = cont_VV(wf(:,58),wf(:,489)) * den(484)
  A(809) = cont_VV(wf(:,45),wf(:,490)) * den(485)
  A(810) = cont_VV(wf(:,39),wf(:,491)) * den(486)
  A(811) = cont_VV(wf(:,44),wf(:,492)) * den(488)
  A(812) = cont_VV(wf(:,42),wf(:,493)) * den(489)
  A(813) = cont_VV(wf(:,56),wf(:,485)) * den(490)
  A(814) = cont_VV(wf(:,44),wf(:,494)) * den(491)
  A(815) = cont_VV(wf(:,44),wf(:,495)) * den(493)
  A(816) = cont_VV(wf(:,59),wf(:,489)) * den(494)
  A(817) = cont_VV(wf(:,44),wf(:,496)) * den(495)
  A(818) = cont_VV(wf(:,39),wf(:,497)) * den(496)
  A(819) = cont_VV(wf(:,60),wf(:,489)) * den(497)
  A(820) = cont_VV(wf(:,42),wf(:,498)) * den(498)
  A(821) = cont_VV(wf(:,42),wf(:,499)) * den(500)
  A(822) = cont_VV(wf(:,39),wf(:,500)) * den(501)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(822)
  complex(REALKIND), intent(out) :: M1(6), M2(9)

  M1(1) = 2*(A(1)-A(2)+A(7)-A(8)+A(11)-A(12)-A(13)+A(15)+A(16)-A(17)+A(19)-A(21)-A(22)+A(24)+A(43)+A(44)-A(45)-A(48)+A(49)+A(50) &
       -A(53)-A(54)+A(55)+A(57))*f(2)+2*(-A(4)+A(6)-A(25)+A(27)-A(28)+A(29)+A(31)-A(34)+A(35)-A(36)+A(37)-A(39)-A(40)+A(41)-A(58) &
       +A(59)-A(60)+A(64)-A(65)-A(66)+A(67)-A(68)-A(71)+A(72))*f(8)
  M1(2) = 2*(-A(1)+A(3)+A(8)-A(9)+A(10)-A(11)+A(13)-A(14)-A(16)+A(18)-A(19)-A(20)-A(23)-A(24)-A(43)-A(44)+A(45)-A(46)+A(47)+A(51) &
       +A(52)+A(54)-A(56)-A(57))*f(2)+2*(A(4)-A(5)+A(25)+A(26)-A(29)+A(30)+A(32)-A(33)+A(34)+A(36)-A(37)+A(38)+A(40)-A(42)+A(58) &
       -A(59)+A(60)+A(61)-A(62)-A(63)+A(69)-A(70)+A(71)-A(72))*f(8)
  M1(3) = 2*(A(2)-A(3)-A(7)+A(9)-A(10)+A(12)+A(14)-A(15)+A(17)-A(18)+A(20)+A(21)+A(22)+A(23)+A(46)-A(47)+A(48)-A(49)-A(50)-A(51) &
       -A(52)+A(53)-A(55)+A(56))*f(2)+2*(A(5)-A(6)-A(26)-A(27)+A(28)-A(30)-A(31)-A(32)+A(33)-A(35)-A(38)+A(39)-A(41)+A(42)-A(61) &
       +A(62)+A(63)-A(64)+A(65)+A(66)-A(67)+A(68)-A(69)+A(70))*f(8)
  M1(4) = 2*(-A(1)+A(3)+A(8)-A(9)+A(10)-A(11)+A(13)-A(14)-A(16)+A(18)-A(19)-A(20)-A(23)-A(24)-A(43)-A(44)+A(45)-A(46)+A(47)+A(51) &
       +A(52)+A(54)-A(56)-A(57))*f(2)+2*(A(4)-A(5)+A(25)+A(26)-A(29)+A(30)+A(32)-A(33)+A(34)+A(36)-A(37)+A(38)+A(40)-A(42)+A(58) &
       -A(59)+A(60)+A(61)-A(62)-A(63)+A(69)-A(70)+A(71)-A(72))*f(8)
  M1(5) = 2*(A(2)-A(3)-A(7)+A(9)-A(10)+A(12)+A(14)-A(15)+A(17)-A(18)+A(20)+A(21)+A(22)+A(23)+A(46)-A(47)+A(48)-A(49)-A(50)-A(51) &
       -A(52)+A(53)-A(55)+A(56))*f(2)+2*(A(5)-A(6)-A(26)-A(27)+A(28)-A(30)-A(31)-A(32)+A(33)-A(35)-A(38)+A(39)-A(41)+A(42)-A(61) &
       +A(62)+A(63)-A(64)+A(65)+A(66)-A(67)+A(68)-A(69)+A(70))*f(8)
  M1(6) = 2*(A(1)-A(2)+A(7)-A(8)+A(11)-A(12)-A(13)+A(15)+A(16)-A(17)+A(19)-A(21)-A(22)+A(24)+A(43)+A(44)-A(45)-A(48)+A(49)+A(50) &
       -A(53)-A(54)+A(55)+A(57))*f(2)+2*(-A(4)+A(6)-A(25)+A(27)-A(28)+A(29)+A(31)-A(34)+A(35)-A(36)+A(37)-A(39)-A(40)+A(41)-A(58) &
       +A(59)-A(60)+A(64)-A(65)-A(66)+A(67)-A(68)-A(71)+A(72))*f(8)

  M2(1) = (4*(-A(88)-A(89)-A(90)-A(94)-A(95)-A(96)-A(100)-A(101)-A(102)-A(106)-A(107)-A(108))*f(14))/3._/**/REALKIND+(4*(A(175) &
       +A(176)+A(177)+A(201)+A(202)+A(203)+A(208)+A(209)+A(210)+A(214)+A(215)+A(216))*f(15))/3._/**/REALKIND+2*(-A(76)-A(77) &
       -A(78))*f(16)+2*(A(82)+A(83)+A(84))*f(17)
  M2(2) = (4*(-A(88)-A(89)-A(90)-A(94)-A(95)-A(96)-A(100)-A(101)-A(102)-A(106)-A(107)-A(108))*f(14))/3._/**/REALKIND+(4*(A(175) &
       +A(176)+A(177)+A(201)+A(202)+A(203)+A(208)+A(209)+A(210)+A(214)+A(215)+A(216))*f(15))/3._/**/REALKIND+2*(-A(76)-A(77) &
       -A(78))*f(16)+2*(A(82)+A(83)+A(84))*f(17)
  M2(3) = (4*(-A(88)-A(89)-A(90)-A(94)-A(95)-A(96)-A(100)-A(101)-A(102)-A(106)-A(107)-A(108))*f(14))/3._/**/REALKIND+(4*(A(175) &
       +A(176)+A(177)+A(201)+A(202)+A(203)+A(208)+A(209)+A(210)+A(214)+A(215)+A(216))*f(15))/3._/**/REALKIND+2*(-A(76)-A(77) &
       -A(78))*f(16)+2*(A(82)+A(83)+A(84))*f(17)
  M2(4) = 2*(A(109)-A(111)-A(112)+A(114)+A(121)-A(122)-A(124)+A(126)+A(128)-A(129)+A(130)-A(131)+A(133)-A(134)-A(136)+A(138) &
       +A(140)-A(141)+A(142)-A(143)+A(145)-A(147)-A(148)+A(150)+A(217)-A(218)+A(220)-A(221)-A(223)+A(225)+A(227)-A(228)+A(230) &
       -A(231)+A(232)-A(233)-A(235)+A(237)-A(238)+A(240)+A(241)-A(242)+A(244)-A(245)+A(247)-A(248)-A(250)+A(252)-A(253)+A(255) &
       +A(257)-A(258)+A(260)-A(261)+A(263)-A(264)+A(265)-A(266)+A(268)-A(269)+A(271)-A(272)+A(274)-A(275)-A(277)+A(279)+A(280) &
       -A(282)-A(284)+A(285)+A(286)+A(287)+A(288)-A(289)-A(293)-A(294)+A(295)-A(296)-A(299)-A(300)-A(301)+A(304)+A(305)+A(306) &
       +A(349)+A(350)+A(351)-A(354)-A(355)-A(356)-A(359)+A(360)-A(361)-A(362)-A(366)+A(367)+A(368)+A(369)+A(370)-A(371)-A(373) &
       +A(375)+A(376)-A(378)+A(385)-A(386)+A(388)-A(389)+A(391)-A(392)+A(394)-A(395)+A(398)-A(399)+A(401)-A(402)+A(404)-A(405) &
       -A(406)+A(408)-A(409)+A(411)+A(412)-A(413)+A(415)-A(416)+A(418)-A(419)-A(421)+A(423)-A(424)+A(426)+A(427)-A(428)+A(431) &
       -A(432)+A(434)-A(435)-A(436)+A(438)+A(439)-A(440)+A(442)-A(443)+A(445)+A(446)+A(447)-A(450)-A(451)-A(452)-A(455)+A(456) &
       -A(457)-A(458)-A(462)+A(463)+A(464)+A(465)+A(466)-A(467)-A(469)+A(471)+A(472)-A(474)-A(475)+A(477)+A(478)-A(480)-A(482) &
       +A(483)+A(484)+A(485)+A(486)-A(487)-A(491)-A(492)+A(493)-A(494)-A(497)-A(498)-A(499)+A(502)+A(503)+A(504)-A(583)+A(584) &
       -A(585)+A(588)+A(589)+A(590)+A(593)-A(594)+A(595)-A(597)-A(601)-A(602)+A(603)-A(604)+A(605)-A(606)-A(607)+A(608)+A(610) &
       -A(611)+A(612)+A(613)+A(614)+A(615)+A(616)+A(617)+A(618)+A(619)+A(620)-A(621)-A(622)+A(623)-A(627)-A(634)+A(635)-A(636) &
       -A(637)-A(638)+A(639)-A(640)-A(642)+A(643)-A(646)+A(648)-A(652)+A(653)-A(654)-A(655)+A(656)-A(657)+A(658)-A(659)-A(662) &
       +A(663)-A(664)-A(665)+A(666)+A(668)+A(671)+A(672)+A(703)-A(704)-A(705)+A(708)+A(709)+A(711)-A(713)+A(714)+A(715)-A(716) &
       -A(721)-A(722)-A(723)+A(724)-A(725)+A(726)-A(727)+A(729)-A(730)+A(731)+A(732)+A(733)+A(734)+A(735)+A(736)+A(737)+A(738) &
       +A(739)-A(740)+A(741)+A(742)-A(743)-A(746)+A(754)-A(755)-A(756)-A(757)+A(758)-A(759)-A(761)+A(762)-A(763)-A(766)+A(767) &
       +A(772)-A(773)-A(774)-A(775)-A(776)+A(777)+A(778)-A(780)+A(782)-A(783)-A(784)+A(785)-A(786)+A(787)+A(791)+A(792))*f(1) &
       +2*(A(151)-A(152)-A(154)+A(156)+A(158)-A(159)+A(160)-A(161)-A(322)+A(323)-A(329)+A(330)+A(331)-A(333)-A(334)+A(335)-A(379) &
       +A(381)+A(382)-A(384)+A(505)+A(506)-A(507)-A(510)+A(511)+A(512)-A(515)-A(516)+A(517)+A(519)-A(673)-A(674)-A(675)-A(676) &
       -A(677)+A(678)+A(683)+A(684)+A(685)-A(686)+A(687)-A(688)+A(693)+A(694)-A(695)+A(696)+A(697)-A(698)-A(701)-A(702))*f(3)+2*( &
       -A(73)-A(74))*f(4)+4*A(75)*f(4)+2*(A(163)-A(165)-A(166)+A(168))*f(5)+2*(A(115)-A(117)-A(118)+A(120)-A(307)+A(308)+A(309) &
       -A(311)-A(312)+A(315)-A(317)+A(318)+A(319)+A(321)+A(325)+A(327)+A(337)-A(338)+A(340)-A(343)-A(344)+A(346)+A(347) &
       -A(348))*f(6)+2*(-A(85)-A(86)-A(92)-A(93)-A(97)-A(99)-A(103)-A(104))*f(7)+4*(A(87)+A(91)+A(98)+A(105))*f(7)+2*(-A(178) &
       +A(179)+A(181)-A(183)-A(186)+A(187)-A(190)+A(191)-A(520)-A(522)+A(523)+A(525)+A(530)-A(532)-A(534)-A(540)+A(541)-A(542) &
       +A(548)+A(550)-A(556)+A(559)-A(560)+A(561)-A(562)+A(564)+A(565)-A(566)-A(569)-A(574)+A(793)+A(794)+A(795)-A(796)-A(797) &
       +A(798)-A(805)-A(806)+A(807)+A(808)+A(809)+A(810)+A(811)-A(812)-A(813)+A(814)+A(819)-A(820)-A(821)+A(822))*f(9) &
       -4*A(80)*f(10)+2*(A(79)+A(81))*f(10)+2*(-A(169)+A(171)+A(196)-A(204))*f(11)+2*(-A(184)+A(189)+A(193)-A(195)+A(521)-A(524) &
       -A(526)-A(531)+A(533)-A(537)+A(538)-A(547)-A(549)+A(553)-A(554)-A(555)-A(568)+A(570)+A(573)+A(575)-A(576)-A(580)+A(581) &
       -A(582))*f(12)+2*(A(172)+A(173)+A(199)+A(200)+A(205)+A(207)+A(211)+A(212))*f(13)+4*(-A(174)-A(198)-A(206)-A(213))*f(13) &
       +17*(A(88)+A(89)+A(95)+A(96)+A(100)+A(102)+A(106)+A(107))*f(14)+(98*(-A(90)-A(94)-A(101)-A(108))*f(14))/3._/**/REALKIND &
       +17*(-A(175)-A(176)-A(202)-A(203)-A(208)-A(210)-A(214)-A(215))*f(15)+(98*(A(177)+A(201)+A(209) &
       +A(216))*f(15))/3._/**/REALKIND+63*(-A(76)-A(77))*f(16)+123*A(78)*f(16)-123*A(83)*f(17)+63*(A(82)+A(84))*f(17)
  M2(5) = 2*(-A(109)-A(110)-A(113)-A(114)-A(121)+A(123)+A(124)-A(125)+A(127)-A(128)+A(131)-A(132)-A(133)+A(135)+A(136)-A(137) &
       +A(139)-A(140)+A(143)-A(144)-A(145)-A(146)-A(149)-A(150)+A(218)-A(219)-A(220)+A(222)+A(223)-A(224)+A(226)-A(227)+A(229) &
       -A(230)-A(232)+A(234)+A(235)-A(236)+A(238)-A(239)-A(241)+A(243)-A(244)+A(246)-A(247)+A(249)+A(250)-A(251)+A(253)-A(254) &
       +A(256)-A(257)+A(259)-A(260)+A(262)-A(263)+A(266)-A(267)+A(269)-A(270)+A(272)-A(273)+A(275)-A(276)-A(278)-A(279)-A(280) &
       -A(281)-A(283)-A(285)-A(286)-A(287)-A(288)-A(290)-A(291)-A(292)-A(295)-A(297)-A(298)-A(302)-A(303)-A(304)-A(305)-A(306) &
       -A(349)-A(350)-A(351)-A(352)-A(353)-A(357)-A(358)-A(360)-A(363)-A(364)-A(365)-A(367)-A(368)-A(369)-A(370)-A(372)-A(374) &
       -A(375)-A(376)-A(377)+A(386)-A(387)+A(389)-A(390)+A(392)-A(393)+A(395)-A(396)+A(397)-A(398)+A(400)-A(401)+A(403)-A(404) &
       +A(406)-A(407)+A(409)-A(410)-A(412)+A(414)-A(415)+A(417)-A(418)+A(420)+A(421)-A(422)+A(424)-A(425)-A(427)+A(429)+A(430) &
       -A(431)+A(433)-A(434)+A(436)-A(437)-A(439)+A(441)+A(443)-A(444)-A(445)-A(446)-A(447)-A(448)-A(449)-A(453)-A(454)-A(456) &
       -A(459)-A(460)-A(461)-A(463)-A(464)-A(465)-A(466)-A(468)-A(470)-A(471)-A(472)-A(473)-A(476)-A(477)-A(478)-A(479)-A(481) &
       -A(483)-A(484)-A(485)-A(486)-A(488)-A(489)-A(490)-A(493)-A(495)-A(496)-A(500)-A(501)-A(502)-A(503)-A(504)-A(586)-A(587) &
       -A(588)-A(589)-A(590)-A(591)-A(592)-A(593)-A(595)-A(596)-A(598)+A(599)-A(600)-A(603)+A(604)-A(605)-A(608)-A(609)-A(610) &
       +A(611)-A(612)-A(613)-A(614)-A(615)-A(616)-A(617)-A(618)-A(619)-A(620)+A(621)-A(624)+A(625)-A(626)-A(628)+A(629)-A(630) &
       -A(631)-A(632)+A(633)-A(641)-A(644)+A(645)-A(647)-A(648)-A(649)-A(650)+A(651)-A(653)-A(656)+A(657)-A(658)-A(660)-A(661) &
       -A(663)-A(667)-A(668)-A(669)-A(670)-A(671)-A(672)-A(706)-A(707)-A(708)-A(709)-A(710)-A(711)-A(712)-A(714)-A(715)-A(717) &
       +A(718)-A(719)-A(720)+A(723)-A(724)-A(726)-A(728)-A(729)+A(730)-A(731)-A(732)-A(733)-A(734)-A(735)-A(736)-A(737)-A(738) &
       -A(739)+A(740)-A(741)+A(744)-A(745)-A(747)+A(748)-A(749)-A(750)-A(751)+A(752)-A(753)-A(760)+A(764)-A(765)-A(767)-A(768) &
       -A(769)+A(770)-A(771)-A(772)+A(776)-A(777)-A(778)-A(779)-A(781)-A(782)-A(787)-A(788)-A(789)-A(790)-A(791)-A(792))*f(1)+2*( &
       -A(151)+A(153)+A(154)-A(155)+A(157)-A(158)+A(161)-A(162)-A(323)+A(324)-A(328)+A(329)-A(331)+A(332)+A(334)-A(336)+A(379) &
       +A(380)+A(383)+A(384)-A(505)-A(506)+A(507)-A(508)+A(509)+A(513)+A(514)+A(516)-A(518)-A(519)+A(673)+A(674)+A(675)+A(676) &
       +A(677)-A(678)+A(679)+A(680)+A(681)-A(682)+A(689)-A(690)+A(691)-A(692)+A(695)-A(696)+A(699)+A(700)+A(701)+A(702))*f(3) &
       +4*A(74)*f(4)+2*(-A(73)-A(75))*f(4)+2*(-A(163)-A(164)-A(167)-A(168))*f(5)+2*(-A(115)-A(116)-A(119)-A(120)+A(307)-A(308) &
       -A(309)+A(310)+A(312)+A(313)-A(314)+A(316)-A(320)-A(321)-A(325)-A(326)+A(339)-A(341)+A(342)+A(343)+A(345)-A(346)-A(347) &
       +A(348))*f(6)+4*(A(85)+A(93)+A(99)+A(104))*f(7)+2*(-A(86)-A(87)-A(91)-A(92)-A(97)-A(98)-A(103)-A(105))*f(7)+2*(A(178) &
       -A(180)-A(181)+A(182)-A(185)+A(186)-A(191)+A(192)+A(520)+A(522)-A(523)-A(525)+A(527)-A(529)-A(536)-A(539)+A(542)-A(543) &
       -A(545)-A(550)-A(557)+A(558)-A(559)-A(561)+A(562)-A(563)-A(565)+A(567)+A(572)+A(574)-A(793)-A(794)-A(795)+A(796)+A(797) &
       -A(798)-A(799)-A(800)+A(801)+A(802)+A(803)+A(804)+A(815)-A(816)+A(817)-A(818)-A(819)+A(820)+A(821)-A(822))*f(9)+2*(A(79) &
       +A(80))*f(10)-4*A(81)*f(10)+2*(A(169)+A(170)+A(197)+A(204))*f(11)+2*(A(184)+A(188)+A(194)+A(195)-A(521)+A(524)+A(526) &
       -A(528)+A(535)+A(537)-A(538)-A(544)+A(546)+A(549)+A(551)-A(552)-A(571)-A(573)+A(577)-A(578)-A(579)+A(580)-A(581) &
       +A(582))*f(12)+4*(-A(172)-A(200)-A(207)-A(212))*f(13)+2*(A(173)+A(174)+A(198)+A(199)+A(205)+A(206)+A(211)+A(213))*f(13) &
       +(98*(-A(88)-A(96)-A(102)-A(107))*f(14))/3._/**/REALKIND+17*(A(89)+A(90)+A(94)+A(95)+A(100)+A(101)+A(106)+A(108))*f(14) &
       +(98*(A(175)+A(203)+A(210)+A(215))*f(15))/3._/**/REALKIND+17*(-A(176)-A(177)-A(201)-A(202)-A(208)-A(209)-A(214) &
       -A(216))*f(15)+123*A(77)*f(16)+63*(-A(76)-A(78))*f(16)+63*(A(82)+A(83))*f(17)-123*A(84)*f(17)
  M2(6) = 2*(A(110)+A(111)+A(112)+A(113)+A(122)-A(123)+A(125)-A(126)-A(127)+A(129)-A(130)+A(132)+A(134)-A(135)+A(137)-A(138) &
       -A(139)+A(141)-A(142)+A(144)+A(146)+A(147)+A(148)+A(149)-A(217)+A(219)+A(221)-A(222)+A(224)-A(225)-A(226)+A(228)-A(229) &
       +A(231)+A(233)-A(234)+A(236)-A(237)+A(239)-A(240)+A(242)-A(243)+A(245)-A(246)+A(248)-A(249)+A(251)-A(252)+A(254)-A(255) &
       -A(256)+A(258)-A(259)+A(261)-A(262)+A(264)-A(265)+A(267)-A(268)+A(270)-A(271)+A(273)-A(274)+A(276)+A(277)+A(278)+A(281) &
       +A(282)+A(283)+A(284)+A(289)+A(290)+A(291)+A(292)+A(293)+A(294)+A(296)+A(297)+A(298)+A(299)+A(300)+A(301)+A(302)+A(303) &
       +A(352)+A(353)+A(354)+A(355)+A(356)+A(357)+A(358)+A(359)+A(361)+A(362)+A(363)+A(364)+A(365)+A(366)+A(371)+A(372)+A(373) &
       +A(374)+A(377)+A(378)-A(385)+A(387)-A(388)+A(390)-A(391)+A(393)-A(394)+A(396)-A(397)+A(399)-A(400)+A(402)-A(403)+A(405) &
       +A(407)-A(408)+A(410)-A(411)+A(413)-A(414)+A(416)-A(417)+A(419)-A(420)+A(422)-A(423)+A(425)-A(426)+A(428)-A(429)-A(430) &
       +A(432)-A(433)+A(435)+A(437)-A(438)+A(440)-A(441)-A(442)+A(444)+A(448)+A(449)+A(450)+A(451)+A(452)+A(453)+A(454)+A(455) &
       +A(457)+A(458)+A(459)+A(460)+A(461)+A(462)+A(467)+A(468)+A(469)+A(470)+A(473)+A(474)+A(475)+A(476)+A(479)+A(480)+A(481) &
       +A(482)+A(487)+A(488)+A(489)+A(490)+A(491)+A(492)+A(494)+A(495)+A(496)+A(497)+A(498)+A(499)+A(500)+A(501)+A(583)-A(584) &
       +A(585)+A(586)+A(587)+A(591)+A(592)+A(594)+A(596)+A(597)+A(598)-A(599)+A(600)+A(601)+A(602)+A(606)+A(607)+A(609)+A(622) &
       -A(623)+A(624)-A(625)+A(626)+A(627)+A(628)-A(629)+A(630)+A(631)+A(632)-A(633)+A(634)-A(635)+A(636)+A(637)+A(638)-A(639) &
       +A(640)+A(641)+A(642)-A(643)+A(644)-A(645)+A(646)+A(647)+A(649)+A(650)-A(651)+A(652)+A(654)+A(655)+A(659)+A(660)+A(661) &
       +A(662)+A(664)+A(665)-A(666)+A(667)+A(669)+A(670)-A(703)+A(704)+A(705)+A(706)+A(707)+A(710)+A(712)+A(713)+A(716)+A(717) &
       -A(718)+A(719)+A(720)+A(721)+A(722)+A(725)+A(727)+A(728)-A(742)+A(743)-A(744)+A(745)+A(746)+A(747)-A(748)+A(749)+A(750) &
       +A(751)-A(752)+A(753)-A(754)+A(755)+A(756)+A(757)-A(758)+A(759)+A(760)+A(761)-A(762)+A(763)-A(764)+A(765)+A(766)+A(768) &
       +A(769)-A(770)+A(771)+A(773)+A(774)+A(775)+A(779)+A(780)+A(781)+A(783)+A(784)-A(785)+A(786)+A(788)+A(789)+A(790))*f(1) &
       +2*(A(152)-A(153)+A(155)-A(156)-A(157)+A(159)-A(160)+A(162)+A(322)-A(324)+A(328)-A(330)-A(332)+A(333)-A(335)+A(336)-A(380) &
       -A(381)-A(382)-A(383)+A(508)-A(509)+A(510)-A(511)-A(512)-A(513)-A(514)+A(515)-A(517)+A(518)-A(679)-A(680)-A(681)+A(682) &
       -A(683)-A(684)-A(685)+A(686)-A(687)+A(688)-A(689)+A(690)-A(691)+A(692)-A(693)-A(694)-A(697)+A(698)-A(699)-A(700))*f(3) &
       +4*A(73)*f(4)+2*(-A(74)-A(75))*f(4)+2*(A(164)+A(165)+A(166)+A(167))*f(5)+2*(A(116)+A(117)+A(118)+A(119)-A(310)+A(311) &
       -A(313)+A(314)-A(315)-A(316)+A(317)-A(318)-A(319)+A(320)+A(326)-A(327)-A(337)+A(338)-A(339)-A(340)+A(341)-A(342)+A(344) &
       -A(345))*f(6)+4*(A(86)+A(92)+A(97)+A(103))*f(7)+2*(-A(85)-A(87)-A(91)-A(93)-A(98)-A(99)-A(104)-A(105))*f(7)+2*(-A(179) &
       +A(180)-A(182)+A(183)+A(185)-A(187)+A(190)-A(192)-A(527)+A(529)-A(530)+A(532)+A(534)+A(536)+A(539)+A(540)-A(541)+A(543) &
       +A(545)-A(548)+A(556)+A(557)-A(558)+A(560)+A(563)-A(564)+A(566)-A(567)+A(569)-A(572)+A(799)+A(800)-A(801)-A(802)-A(803) &
       -A(804)+A(805)+A(806)-A(807)-A(808)-A(809)-A(810)-A(811)+A(812)+A(813)-A(814)-A(815)+A(816)-A(817)+A(818))*f(9) &
       -4*A(79)*f(10)+2*(A(80)+A(81))*f(10)+2*(-A(170)-A(171)-A(196)-A(197))*f(11)+2*(-A(188)-A(189)-A(193)-A(194)+A(528)+A(531) &
       -A(533)-A(535)+A(544)-A(546)+A(547)-A(551)+A(552)-A(553)+A(554)+A(555)+A(568)-A(570)+A(571)-A(575)+A(576)-A(577)+A(578) &
       +A(579))*f(12)+4*(-A(173)-A(199)-A(205)-A(211))*f(13)+2*(A(172)+A(174)+A(198)+A(200)+A(206)+A(207)+A(212)+A(213))*f(13) &
       +(98*(-A(89)-A(95)-A(100)-A(106))*f(14))/3._/**/REALKIND+17*(A(88)+A(90)+A(94)+A(96)+A(101)+A(102)+A(107)+A(108))*f(14) &
       +(98*(A(176)+A(202)+A(208)+A(214))*f(15))/3._/**/REALKIND+17*(-A(175)-A(177)-A(201)-A(203)-A(209)-A(210)-A(215) &
       -A(216))*f(15)+123*A(76)*f(16)+63*(-A(77)-A(78))*f(16)-123*A(82)*f(17)+63*(A(83)+A(84))*f(17)
  M2(7) = 2*(-A(109)-A(110)-A(113)-A(114)-A(121)+A(123)+A(124)-A(125)+A(127)-A(128)+A(131)-A(132)-A(133)+A(135)+A(136)-A(137) &
       +A(139)-A(140)+A(143)-A(144)-A(145)-A(146)-A(149)-A(150)+A(218)-A(219)-A(220)+A(222)+A(223)-A(224)+A(226)-A(227)+A(229) &
       -A(230)-A(232)+A(234)+A(235)-A(236)+A(238)-A(239)-A(241)+A(243)-A(244)+A(246)-A(247)+A(249)+A(250)-A(251)+A(253)-A(254) &
       +A(256)-A(257)+A(259)-A(260)+A(262)-A(263)+A(266)-A(267)+A(269)-A(270)+A(272)-A(273)+A(275)-A(276)-A(278)-A(279)-A(280) &
       -A(281)-A(283)-A(285)-A(286)-A(287)-A(288)-A(290)-A(291)-A(292)-A(295)-A(297)-A(298)-A(302)-A(303)-A(304)-A(305)-A(306) &
       -A(349)-A(350)-A(351)-A(352)-A(353)-A(357)-A(358)-A(360)-A(363)-A(364)-A(365)-A(367)-A(368)-A(369)-A(370)-A(372)-A(374) &
       -A(375)-A(376)-A(377)+A(386)-A(387)+A(389)-A(390)+A(392)-A(393)+A(395)-A(396)+A(397)-A(398)+A(400)-A(401)+A(403)-A(404) &
       +A(406)-A(407)+A(409)-A(410)-A(412)+A(414)-A(415)+A(417)-A(418)+A(420)+A(421)-A(422)+A(424)-A(425)-A(427)+A(429)+A(430) &
       -A(431)+A(433)-A(434)+A(436)-A(437)-A(439)+A(441)+A(443)-A(444)-A(445)-A(446)-A(447)-A(448)-A(449)-A(453)-A(454)-A(456) &
       -A(459)-A(460)-A(461)-A(463)-A(464)-A(465)-A(466)-A(468)-A(470)-A(471)-A(472)-A(473)-A(476)-A(477)-A(478)-A(479)-A(481) &
       -A(483)-A(484)-A(485)-A(486)-A(488)-A(489)-A(490)-A(493)-A(495)-A(496)-A(500)-A(501)-A(502)-A(503)-A(504)-A(586)-A(587) &
       -A(588)-A(589)-A(590)-A(591)-A(592)-A(593)-A(595)-A(596)-A(598)+A(599)-A(600)-A(603)+A(604)-A(605)-A(608)-A(609)-A(610) &
       +A(611)-A(612)-A(613)-A(614)-A(615)-A(616)-A(617)-A(618)-A(619)-A(620)+A(621)-A(624)+A(625)-A(626)-A(628)+A(629)-A(630) &
       -A(631)-A(632)+A(633)-A(641)-A(644)+A(645)-A(647)-A(648)-A(649)-A(650)+A(651)-A(653)-A(656)+A(657)-A(658)-A(660)-A(661) &
       -A(663)-A(667)-A(668)-A(669)-A(670)-A(671)-A(672)-A(706)-A(707)-A(708)-A(709)-A(710)-A(711)-A(712)-A(714)-A(715)-A(717) &
       +A(718)-A(719)-A(720)+A(723)-A(724)-A(726)-A(728)-A(729)+A(730)-A(731)-A(732)-A(733)-A(734)-A(735)-A(736)-A(737)-A(738) &
       -A(739)+A(740)-A(741)+A(744)-A(745)-A(747)+A(748)-A(749)-A(750)-A(751)+A(752)-A(753)-A(760)+A(764)-A(765)-A(767)-A(768) &
       -A(769)+A(770)-A(771)-A(772)+A(776)-A(777)-A(778)-A(779)-A(781)-A(782)-A(787)-A(788)-A(789)-A(790)-A(791)-A(792))*f(1)+2*( &
       -A(151)+A(153)+A(154)-A(155)+A(157)-A(158)+A(161)-A(162)-A(323)+A(324)-A(328)+A(329)-A(331)+A(332)+A(334)-A(336)+A(379) &
       +A(380)+A(383)+A(384)-A(505)-A(506)+A(507)-A(508)+A(509)+A(513)+A(514)+A(516)-A(518)-A(519)+A(673)+A(674)+A(675)+A(676) &
       +A(677)-A(678)+A(679)+A(680)+A(681)-A(682)+A(689)-A(690)+A(691)-A(692)+A(695)-A(696)+A(699)+A(700)+A(701)+A(702))*f(3) &
       +4*A(74)*f(4)+2*(-A(73)-A(75))*f(4)+2*(-A(163)-A(164)-A(167)-A(168))*f(5)+2*(-A(115)-A(116)-A(119)-A(120)+A(307)-A(308) &
       -A(309)+A(310)+A(312)+A(313)-A(314)+A(316)-A(320)-A(321)-A(325)-A(326)+A(339)-A(341)+A(342)+A(343)+A(345)-A(346)-A(347) &
       +A(348))*f(6)+4*(A(85)+A(93)+A(99)+A(104))*f(7)+2*(-A(86)-A(87)-A(91)-A(92)-A(97)-A(98)-A(103)-A(105))*f(7)+2*(A(178) &
       -A(180)-A(181)+A(182)-A(185)+A(186)-A(191)+A(192)+A(520)+A(522)-A(523)-A(525)+A(527)-A(529)-A(536)-A(539)+A(542)-A(543) &
       -A(545)-A(550)-A(557)+A(558)-A(559)-A(561)+A(562)-A(563)-A(565)+A(567)+A(572)+A(574)-A(793)-A(794)-A(795)+A(796)+A(797) &
       -A(798)-A(799)-A(800)+A(801)+A(802)+A(803)+A(804)+A(815)-A(816)+A(817)-A(818)-A(819)+A(820)+A(821)-A(822))*f(9)+2*(A(79) &
       +A(80))*f(10)-4*A(81)*f(10)+2*(A(169)+A(170)+A(197)+A(204))*f(11)+2*(A(184)+A(188)+A(194)+A(195)-A(521)+A(524)+A(526) &
       -A(528)+A(535)+A(537)-A(538)-A(544)+A(546)+A(549)+A(551)-A(552)-A(571)-A(573)+A(577)-A(578)-A(579)+A(580)-A(581) &
       +A(582))*f(12)+4*(-A(172)-A(200)-A(207)-A(212))*f(13)+2*(A(173)+A(174)+A(198)+A(199)+A(205)+A(206)+A(211)+A(213))*f(13) &
       +(98*(-A(88)-A(96)-A(102)-A(107))*f(14))/3._/**/REALKIND+17*(A(89)+A(90)+A(94)+A(95)+A(100)+A(101)+A(106)+A(108))*f(14) &
       +(98*(A(175)+A(203)+A(210)+A(215))*f(15))/3._/**/REALKIND+17*(-A(176)-A(177)-A(201)-A(202)-A(208)-A(209)-A(214) &
       -A(216))*f(15)+123*A(77)*f(16)+63*(-A(76)-A(78))*f(16)+63*(A(82)+A(83))*f(17)-123*A(84)*f(17)
  M2(8) = 2*(A(110)+A(111)+A(112)+A(113)+A(122)-A(123)+A(125)-A(126)-A(127)+A(129)-A(130)+A(132)+A(134)-A(135)+A(137)-A(138) &
       -A(139)+A(141)-A(142)+A(144)+A(146)+A(147)+A(148)+A(149)-A(217)+A(219)+A(221)-A(222)+A(224)-A(225)-A(226)+A(228)-A(229) &
       +A(231)+A(233)-A(234)+A(236)-A(237)+A(239)-A(240)+A(242)-A(243)+A(245)-A(246)+A(248)-A(249)+A(251)-A(252)+A(254)-A(255) &
       -A(256)+A(258)-A(259)+A(261)-A(262)+A(264)-A(265)+A(267)-A(268)+A(270)-A(271)+A(273)-A(274)+A(276)+A(277)+A(278)+A(281) &
       +A(282)+A(283)+A(284)+A(289)+A(290)+A(291)+A(292)+A(293)+A(294)+A(296)+A(297)+A(298)+A(299)+A(300)+A(301)+A(302)+A(303) &
       +A(352)+A(353)+A(354)+A(355)+A(356)+A(357)+A(358)+A(359)+A(361)+A(362)+A(363)+A(364)+A(365)+A(366)+A(371)+A(372)+A(373) &
       +A(374)+A(377)+A(378)-A(385)+A(387)-A(388)+A(390)-A(391)+A(393)-A(394)+A(396)-A(397)+A(399)-A(400)+A(402)-A(403)+A(405) &
       +A(407)-A(408)+A(410)-A(411)+A(413)-A(414)+A(416)-A(417)+A(419)-A(420)+A(422)-A(423)+A(425)-A(426)+A(428)-A(429)-A(430) &
       +A(432)-A(433)+A(435)+A(437)-A(438)+A(440)-A(441)-A(442)+A(444)+A(448)+A(449)+A(450)+A(451)+A(452)+A(453)+A(454)+A(455) &
       +A(457)+A(458)+A(459)+A(460)+A(461)+A(462)+A(467)+A(468)+A(469)+A(470)+A(473)+A(474)+A(475)+A(476)+A(479)+A(480)+A(481) &
       +A(482)+A(487)+A(488)+A(489)+A(490)+A(491)+A(492)+A(494)+A(495)+A(496)+A(497)+A(498)+A(499)+A(500)+A(501)+A(583)-A(584) &
       +A(585)+A(586)+A(587)+A(591)+A(592)+A(594)+A(596)+A(597)+A(598)-A(599)+A(600)+A(601)+A(602)+A(606)+A(607)+A(609)+A(622) &
       -A(623)+A(624)-A(625)+A(626)+A(627)+A(628)-A(629)+A(630)+A(631)+A(632)-A(633)+A(634)-A(635)+A(636)+A(637)+A(638)-A(639) &
       +A(640)+A(641)+A(642)-A(643)+A(644)-A(645)+A(646)+A(647)+A(649)+A(650)-A(651)+A(652)+A(654)+A(655)+A(659)+A(660)+A(661) &
       +A(662)+A(664)+A(665)-A(666)+A(667)+A(669)+A(670)-A(703)+A(704)+A(705)+A(706)+A(707)+A(710)+A(712)+A(713)+A(716)+A(717) &
       -A(718)+A(719)+A(720)+A(721)+A(722)+A(725)+A(727)+A(728)-A(742)+A(743)-A(744)+A(745)+A(746)+A(747)-A(748)+A(749)+A(750) &
       +A(751)-A(752)+A(753)-A(754)+A(755)+A(756)+A(757)-A(758)+A(759)+A(760)+A(761)-A(762)+A(763)-A(764)+A(765)+A(766)+A(768) &
       +A(769)-A(770)+A(771)+A(773)+A(774)+A(775)+A(779)+A(780)+A(781)+A(783)+A(784)-A(785)+A(786)+A(788)+A(789)+A(790))*f(1) &
       +2*(A(152)-A(153)+A(155)-A(156)-A(157)+A(159)-A(160)+A(162)+A(322)-A(324)+A(328)-A(330)-A(332)+A(333)-A(335)+A(336)-A(380) &
       -A(381)-A(382)-A(383)+A(508)-A(509)+A(510)-A(511)-A(512)-A(513)-A(514)+A(515)-A(517)+A(518)-A(679)-A(680)-A(681)+A(682) &
       -A(683)-A(684)-A(685)+A(686)-A(687)+A(688)-A(689)+A(690)-A(691)+A(692)-A(693)-A(694)-A(697)+A(698)-A(699)-A(700))*f(3) &
       +4*A(73)*f(4)+2*(-A(74)-A(75))*f(4)+2*(A(164)+A(165)+A(166)+A(167))*f(5)+2*(A(116)+A(117)+A(118)+A(119)-A(310)+A(311) &
       -A(313)+A(314)-A(315)-A(316)+A(317)-A(318)-A(319)+A(320)+A(326)-A(327)-A(337)+A(338)-A(339)-A(340)+A(341)-A(342)+A(344) &
       -A(345))*f(6)+4*(A(86)+A(92)+A(97)+A(103))*f(7)+2*(-A(85)-A(87)-A(91)-A(93)-A(98)-A(99)-A(104)-A(105))*f(7)+2*(-A(179) &
       +A(180)-A(182)+A(183)+A(185)-A(187)+A(190)-A(192)-A(527)+A(529)-A(530)+A(532)+A(534)+A(536)+A(539)+A(540)-A(541)+A(543) &
       +A(545)-A(548)+A(556)+A(557)-A(558)+A(560)+A(563)-A(564)+A(566)-A(567)+A(569)-A(572)+A(799)+A(800)-A(801)-A(802)-A(803) &
       -A(804)+A(805)+A(806)-A(807)-A(808)-A(809)-A(810)-A(811)+A(812)+A(813)-A(814)-A(815)+A(816)-A(817)+A(818))*f(9) &
       -4*A(79)*f(10)+2*(A(80)+A(81))*f(10)+2*(-A(170)-A(171)-A(196)-A(197))*f(11)+2*(-A(188)-A(189)-A(193)-A(194)+A(528)+A(531) &
       -A(533)-A(535)+A(544)-A(546)+A(547)-A(551)+A(552)-A(553)+A(554)+A(555)+A(568)-A(570)+A(571)-A(575)+A(576)-A(577)+A(578) &
       +A(579))*f(12)+4*(-A(173)-A(199)-A(205)-A(211))*f(13)+2*(A(172)+A(174)+A(198)+A(200)+A(206)+A(207)+A(212)+A(213))*f(13) &
       +(98*(-A(89)-A(95)-A(100)-A(106))*f(14))/3._/**/REALKIND+17*(A(88)+A(90)+A(94)+A(96)+A(101)+A(102)+A(107)+A(108))*f(14) &
       +(98*(A(176)+A(202)+A(208)+A(214))*f(15))/3._/**/REALKIND+17*(-A(175)-A(177)-A(201)-A(203)-A(209)-A(210)-A(215) &
       -A(216))*f(15)+123*A(76)*f(16)+63*(-A(77)-A(78))*f(16)-123*A(82)*f(17)+63*(A(83)+A(84))*f(17)
  M2(9) = 2*(A(109)-A(111)-A(112)+A(114)+A(121)-A(122)-A(124)+A(126)+A(128)-A(129)+A(130)-A(131)+A(133)-A(134)-A(136)+A(138) &
       +A(140)-A(141)+A(142)-A(143)+A(145)-A(147)-A(148)+A(150)+A(217)-A(218)+A(220)-A(221)-A(223)+A(225)+A(227)-A(228)+A(230) &
       -A(231)+A(232)-A(233)-A(235)+A(237)-A(238)+A(240)+A(241)-A(242)+A(244)-A(245)+A(247)-A(248)-A(250)+A(252)-A(253)+A(255) &
       +A(257)-A(258)+A(260)-A(261)+A(263)-A(264)+A(265)-A(266)+A(268)-A(269)+A(271)-A(272)+A(274)-A(275)-A(277)+A(279)+A(280) &
       -A(282)-A(284)+A(285)+A(286)+A(287)+A(288)-A(289)-A(293)-A(294)+A(295)-A(296)-A(299)-A(300)-A(301)+A(304)+A(305)+A(306) &
       +A(349)+A(350)+A(351)-A(354)-A(355)-A(356)-A(359)+A(360)-A(361)-A(362)-A(366)+A(367)+A(368)+A(369)+A(370)-A(371)-A(373) &
       +A(375)+A(376)-A(378)+A(385)-A(386)+A(388)-A(389)+A(391)-A(392)+A(394)-A(395)+A(398)-A(399)+A(401)-A(402)+A(404)-A(405) &
       -A(406)+A(408)-A(409)+A(411)+A(412)-A(413)+A(415)-A(416)+A(418)-A(419)-A(421)+A(423)-A(424)+A(426)+A(427)-A(428)+A(431) &
       -A(432)+A(434)-A(435)-A(436)+A(438)+A(439)-A(440)+A(442)-A(443)+A(445)+A(446)+A(447)-A(450)-A(451)-A(452)-A(455)+A(456) &
       -A(457)-A(458)-A(462)+A(463)+A(464)+A(465)+A(466)-A(467)-A(469)+A(471)+A(472)-A(474)-A(475)+A(477)+A(478)-A(480)-A(482) &
       +A(483)+A(484)+A(485)+A(486)-A(487)-A(491)-A(492)+A(493)-A(494)-A(497)-A(498)-A(499)+A(502)+A(503)+A(504)-A(583)+A(584) &
       -A(585)+A(588)+A(589)+A(590)+A(593)-A(594)+A(595)-A(597)-A(601)-A(602)+A(603)-A(604)+A(605)-A(606)-A(607)+A(608)+A(610) &
       -A(611)+A(612)+A(613)+A(614)+A(615)+A(616)+A(617)+A(618)+A(619)+A(620)-A(621)-A(622)+A(623)-A(627)-A(634)+A(635)-A(636) &
       -A(637)-A(638)+A(639)-A(640)-A(642)+A(643)-A(646)+A(648)-A(652)+A(653)-A(654)-A(655)+A(656)-A(657)+A(658)-A(659)-A(662) &
       +A(663)-A(664)-A(665)+A(666)+A(668)+A(671)+A(672)+A(703)-A(704)-A(705)+A(708)+A(709)+A(711)-A(713)+A(714)+A(715)-A(716) &
       -A(721)-A(722)-A(723)+A(724)-A(725)+A(726)-A(727)+A(729)-A(730)+A(731)+A(732)+A(733)+A(734)+A(735)+A(736)+A(737)+A(738) &
       +A(739)-A(740)+A(741)+A(742)-A(743)-A(746)+A(754)-A(755)-A(756)-A(757)+A(758)-A(759)-A(761)+A(762)-A(763)-A(766)+A(767) &
       +A(772)-A(773)-A(774)-A(775)-A(776)+A(777)+A(778)-A(780)+A(782)-A(783)-A(784)+A(785)-A(786)+A(787)+A(791)+A(792))*f(1) &
       +2*(A(151)-A(152)-A(154)+A(156)+A(158)-A(159)+A(160)-A(161)-A(322)+A(323)-A(329)+A(330)+A(331)-A(333)-A(334)+A(335)-A(379) &
       +A(381)+A(382)-A(384)+A(505)+A(506)-A(507)-A(510)+A(511)+A(512)-A(515)-A(516)+A(517)+A(519)-A(673)-A(674)-A(675)-A(676) &
       -A(677)+A(678)+A(683)+A(684)+A(685)-A(686)+A(687)-A(688)+A(693)+A(694)-A(695)+A(696)+A(697)-A(698)-A(701)-A(702))*f(3)+2*( &
       -A(73)-A(74))*f(4)+4*A(75)*f(4)+2*(A(163)-A(165)-A(166)+A(168))*f(5)+2*(A(115)-A(117)-A(118)+A(120)-A(307)+A(308)+A(309) &
       -A(311)-A(312)+A(315)-A(317)+A(318)+A(319)+A(321)+A(325)+A(327)+A(337)-A(338)+A(340)-A(343)-A(344)+A(346)+A(347) &
       -A(348))*f(6)+2*(-A(85)-A(86)-A(92)-A(93)-A(97)-A(99)-A(103)-A(104))*f(7)+4*(A(87)+A(91)+A(98)+A(105))*f(7)+2*(-A(178) &
       +A(179)+A(181)-A(183)-A(186)+A(187)-A(190)+A(191)-A(520)-A(522)+A(523)+A(525)+A(530)-A(532)-A(534)-A(540)+A(541)-A(542) &
       +A(548)+A(550)-A(556)+A(559)-A(560)+A(561)-A(562)+A(564)+A(565)-A(566)-A(569)-A(574)+A(793)+A(794)+A(795)-A(796)-A(797) &
       +A(798)-A(805)-A(806)+A(807)+A(808)+A(809)+A(810)+A(811)-A(812)-A(813)+A(814)+A(819)-A(820)-A(821)+A(822))*f(9) &
       -4*A(80)*f(10)+2*(A(79)+A(81))*f(10)+2*(-A(169)+A(171)+A(196)-A(204))*f(11)+2*(-A(184)+A(189)+A(193)-A(195)+A(521)-A(524) &
       -A(526)-A(531)+A(533)-A(537)+A(538)-A(547)-A(549)+A(553)-A(554)-A(555)-A(568)+A(570)+A(573)+A(575)-A(576)-A(580)+A(581) &
       -A(582))*f(12)+2*(A(172)+A(173)+A(199)+A(200)+A(205)+A(207)+A(211)+A(212))*f(13)+4*(-A(174)-A(198)-A(206)-A(213))*f(13) &
       +17*(A(88)+A(89)+A(95)+A(96)+A(100)+A(102)+A(106)+A(107))*f(14)+(98*(-A(90)-A(94)-A(101)-A(108))*f(14))/3._/**/REALKIND &
       +17*(-A(175)-A(176)-A(202)-A(203)-A(208)-A(210)-A(214)-A(215))*f(15)+(98*(A(177)+A(201)+A(209) &
       +A(216))*f(15))/3._/**/REALKIND+63*(-A(76)-A(77))*f(16)+123*A(78)*f(16)-123*A(83)*f(17)+63*(A(82)+A(84))*f(17)

end subroutine colourvectors

end module ol_loop_heftpphhjj_hhgggg_1_/**/REALKIND
