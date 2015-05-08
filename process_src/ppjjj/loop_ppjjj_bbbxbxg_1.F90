
module ol_colourmatrix_ppjjj_bbbxbxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(68,4), K2(4,4), KL(4,4)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  36,  12,  12,   0]
  K1( 2,:) = [  12,  36,   0,  12]
  K1( 3,:) = [  12,   0,  36,  12]
  K1( 4,:) = [   0,  12,  12,  36]
  K1( 5,:) = [  48,  16,  16,   0]
  K1( 6,:) = [  16,  48,   0,  16]
  K1( 7,:) = [  16,   0,  48,  16]
  K1( 8,:) = [   0,  16,  16,  48]
  K1( 9,:) = [   0,  16,  -2,   6]
  K1(10,:) = [  16,   0,   6,  -2]
  K1(11,:) = [  -2,   6,   0,  16]
  K1(12,:) = [   6,  -2,  16,   0]
  K1(13,:) = [  48,  16,  16,   0]
  K1(14,:) = [  16,  48,   0,  16]
  K1(15,:) = [  16,   0,  48,  16]
  K1(16,:) = [   0,  16,  16,  48]
  K1(17,:) = [   6,   2,   2,   0]
  K1(18,:) = [   2,   0,  -6, -16]
  K1(19,:) = [   2,  -6,   0, -16]
  K1(20,:) = [   0, -16, -16, -48]
  K1(21,:) = [   0,   2, -16,  -6]
  K1(22,:) = [   2,   6,   0,   2]
  K1(23,:) = [ -16,   0, -48, -16]
  K1(24,:) = [  -6,   2, -16,   0]
  K1(25,:) = [  48,  16,  16,   0]
  K1(26,:) = [  16,  48,   0,  16]
  K1(27,:) = [  16,   0,  48,  16]
  K1(28,:) = [   0,  16,  16,  48]
  K1(29,:) = [   0, -16,   2,  -6]
  K1(30,:) = [ -16, -48,   0, -16]
  K1(31,:) = [   2,   0,   6,   2]
  K1(32,:) = [  -6, -16,   2,   0]
  K1(33,:) = [ -48, -16, -16,   0]
  K1(34,:) = [ -16,   0,  -6,   2]
  K1(35,:) = [ -16,  -6,   0,   2]
  K1(36,:) = [   0,   2,   2,   6]
  K1(37,:) = [   0,  -2,  16,   6]
  K1(38,:) = [  -2,   0,   6,  16]
  K1(39,:) = [  16,   6,   0,  -2]
  K1(40,:) = [   6,  16,  -2,   0]
  K1(41,:) = [  48,  16,  16,   0]
  K1(42,:) = [  16,  48,   0,  16]
  K1(43,:) = [  16,   0,  48,  16]
  K1(44,:) = [   0,  16,  16,  48]
  K1(45,:) = [ -54, -18, -18,   0]
  K1(46,:) = [ -18,   0,   0,  18]
  K1(47,:) = [ -18,   0, -54, -18]
  K1(48,:) = [   0,  18, -18,   0]
  K1(49,:) = [   0, -18,  18,   0]
  K1(50,:) = [ -18, -54,   0, -18]
  K1(51,:) = [  18,   0,   0, -18]
  K1(52,:) = [   0, -18, -18, -54]
  K1(53,:) = [ -54, -18, -18,   0]
  K1(54,:) = [ -18, -54,   0, -18]
  K1(55,:) = [ -18,   0,   0,  18]
  K1(56,:) = [   0, -18,  18,   0]
  K1(57,:) = [   0,  18, -18,   0]
  K1(58,:) = [  18,   0,   0, -18]
  K1(59,:) = [ -18,   0, -54, -18]
  K1(60,:) = [   0, -18, -18, -54]
  K1(61,:) = [ 108,  36,  36,   0]
  K1(62,:) = [  36, 108,   0,  36]
  K1(63,:) = [  36,   0, 108,  36]
  K1(64,:) = [   0,  36,  36, 108]
  K1(65,:) = [   0,   0,   0,   0]
  K1(66,:) = [   0,   0,   0,   0]
  K1(67,:) = [   0,   0,   0,   0]
  K1(68,:) = [   0,   0,   0,   0]
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
end module ol_colourmatrix_ppjjj_bbbxbxg_1_/**/REALKIND



module ol_forced_parameters_ppjjj_bbbxbxg_1_/**/REALKIND
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


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppjjj_bbbxbxg_1_/**/REALKIND

module ol_loop_ppjjj_bbbxbxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(15), c(26)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:80)
  ! denominators
  complex(REALKIND), save :: den(66)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(4,32)
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
    f( 7) = countertermnorm*ctVVV*gQCD**5
    f( 8) = (CI*gQCD**5*integralnorm*SwB)/2._/**/REALKIND
    f( 9) = CI*gQCD**5*integralnorm*SwB
    f(10) = (gQCD**5*integralnorm*SwB)/2._/**/REALKIND
    f(11) = gQCD**5*integralnorm*SwB
    f(12) = CI*gQCD**5*integralnorm*SwF
    f(13) = 2*CI*gQCD**5*integralnorm*SwF
    f(14) = gQCD**5*integralnorm*SwF
    f(15) = 2*gQCD**5*integralnorm*SwF

  c = [ 27*CI*f(8), 54*CI*f(8), 3*CI*f(9), 9*CI*f(9), 24*CI*f(9), 27*CI*f(9), 54*CI*f(9), 18*f(10), 54*f(10), f(11), 3*f(11) &
    , 6*f(11), 8*f(11), 9*f(11), 10*f(11), 18*f(11), 21*f(11), 24*f(11), 27*f(11), 54*f(11), 9*CI*f(12), 9*CI*f(13), 3*f(14) &
    , 9*f(14), 3*f(15), 9*f(15) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,5)
  integer,           intent(in)  :: H(5)
  complex(REALKIND), intent(out) :: M1(4), M2(4)
  complex(REALKIND) :: A(60)
  ! external WFs
  call wf_Q(P(:,1), rMB, H(1), wf(:,0))
  call wf_Q(P(:,2), rMB, H(2), wf(:,-1))
  call wf_A(P(:,3), rMB, H(3), wf(:,-2))
  call wf_A(P(:,4), rMB, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,2))
  call vert_UV_W(wf(:,1),Q(:,5),wf(:,-4),Q(:,16),wf(:,3))
  call vert_VQ_A(wf(:,-4),wf(:,-1),wf(:,4))
  call prop_Q_A(wf(:,4),Q(:,18),MB,1_intkind1,wf(:,5))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,6))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,7))
  call prop_A_Q(wf(:,7),Q(:,24),MB,1_intkind1,wf(:,8))
  call vert_VQ_A(wf(:,1),wf(:,-1),wf(:,9))
  call vert_QA_V(wf(:,0),wf(:,-3),wf(:,10))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,11))
  call vert_UV_W(wf(:,10),Q(:,9),wf(:,-4),Q(:,16),wf(:,12))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,13))
  call prop_Q_A(wf(:,13),Q(:,17),MB,1_intkind1,wf(:,14))
  call vert_QA_V(wf(:,14),wf(:,-3),wf(:,15))
  call vert_VQ_A(wf(:,11),wf(:,0),wf(:,16))
  call vert_AV_Q(wf(:,-2),wf(:,10),wf(:,17))
  call vert_AV_Q(wf(:,-2),wf(:,-4),wf(:,18))
  call prop_A_Q(wf(:,18),Q(:,20),MB,1_intkind1,wf(:,19))
  call vert_VQ_A(wf(:,10),wf(:,-1),wf(:,20))
  call vert_QA_V(wf(:,14),wf(:,-2),wf(:,21))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,22))
  call counter_UV_W(wf(:,1),Q(:,5),wf(:,-4),Q(:,16),wf(:,23))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,24))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,25))
  call prop_A_Q(wf(:,25),Q(:,24),MB,1_intkind1,wf(:,26))
  call counter_UV_W(wf(:,10),Q(:,9),wf(:,-4),Q(:,16),wf(:,27))
  call counter_QA_V(wf(:,14),wf(:,-3),wf(:,28))
  call counter_AV_Q(wf(:,-2),wf(:,10),wf(:,29))
  call counter_AV_Q(wf(:,-2),wf(:,-4),wf(:,30))
  call prop_A_Q(wf(:,30),Q(:,20),MB,1_intkind1,wf(:,31))
  call counter_QA_V(wf(:,14),wf(:,-2),wf(:,32))
  call counter_VQ_A(wf(:,1),wf(:,-1),wf(:,33))
  call counter_VQ_A(wf(:,-4),wf(:,-1),wf(:,34))
  call prop_Q_A(wf(:,34),Q(:,18),MB,1_intkind1,wf(:,35))
  call counter_QA_V(wf(:,-1),wf(:,-3),wf(:,36))
  call counter_VQ_A(wf(:,10),wf(:,-1),wf(:,37))
  call vert_VQ_A(wf(:,36),wf(:,0),wf(:,38))
  call counter_QA_V(wf(:,-1),wf(:,-2),wf(:,39))
  call vert_VQ_A(wf(:,39),wf(:,0),wf(:,40))
  call counter_VQ_A(wf(:,11),wf(:,0),wf(:,41))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,42))
  call prop_Q_A(wf(:,42),Q(:,17),MB,1_intkind1,wf(:,43))
  call vert_QA_V(wf(:,43),wf(:,-3),wf(:,44))
  call counter_QA_V(wf(:,0),wf(:,-3),wf(:,45))
  call vert_UV_W(wf(:,45),Q(:,9),wf(:,-4),Q(:,16),wf(:,46))
  call counter_VQ_A(wf(:,2),wf(:,0),wf(:,47))
  call vert_QA_V(wf(:,43),wf(:,-2),wf(:,48))
  call vert_AV_Q(wf(:,-2),wf(:,45),wf(:,49))
  call vert_VQ_A(wf(:,45),wf(:,-1),wf(:,50))
  call counter_QA_V(wf(:,0),wf(:,-2),wf(:,51))
  call vert_UV_W(wf(:,51),Q(:,5),wf(:,-4),Q(:,16),wf(:,52))
  call vert_AV_Q(wf(:,-3),wf(:,51),wf(:,53))
  call vert_VQ_A(wf(:,51),wf(:,-1),wf(:,54))
  call vert_UV_W(wf(:,2),Q(:,10),wf(:,-4),Q(:,16),wf(:,55))
  call counter_V_V(ctGG,wf(:,1),Q(:,5),wf(:,56))
  call counter_V_V(ctGG,wf(:,2),Q(:,10),wf(:,57))
  call vert_QA_V(wf(:,5),wf(:,-3),wf(:,58))
  call counter_Q_A(ctbb,wf(:,5),Q(:,18),wf(:,59))
  call prop_A_Q(wf(:,6),Q(:,13),MB,1_intkind1,wf(:,60))
  call vert_QA_V(wf(:,-1),wf(:,8),wf(:,61))
  call counter_A_Q(ctbb,wf(:,8),Q(:,24),wf(:,62))
  call prop_Q_A(wf(:,9),Q(:,7),MB,1_intkind1,wf(:,63))
  call vert_UV_W(wf(:,11),Q(:,6),wf(:,-4),Q(:,16),wf(:,64))
  call counter_V_V(ctGG,wf(:,10),Q(:,9),wf(:,65))
  call counter_V_V(ctGG,wf(:,11),Q(:,6),wf(:,66))
  call vert_AV_Q(wf(:,-3),wf(:,11),wf(:,67))
  call counter_Q_A(ctbb,wf(:,14),Q(:,17),wf(:,68))
  call prop_A_Q(wf(:,67),Q(:,14),MB,1_intkind1,wf(:,69))
  call vert_QA_V(wf(:,0),wf(:,8),wf(:,70))
  call prop_Q_A(wf(:,16),Q(:,7),MB,1_intkind1,wf(:,71))
  call vert_QA_V(wf(:,5),wf(:,-2),wf(:,72))
  call prop_A_Q(wf(:,17),Q(:,13),MB,1_intkind1,wf(:,73))
  call vert_QA_V(wf(:,-1),wf(:,19),wf(:,74))
  call counter_A_Q(ctbb,wf(:,19),Q(:,20),wf(:,75))
  call prop_Q_A(wf(:,20),Q(:,11),MB,1_intkind1,wf(:,76))
  call vert_AV_Q(wf(:,-2),wf(:,2),wf(:,77))
  call prop_A_Q(wf(:,77),Q(:,14),MB,1_intkind1,wf(:,78))
  call vert_QA_V(wf(:,0),wf(:,19),wf(:,79))
  call prop_Q_A(wf(:,22),Q(:,11),MB,1_intkind1,wf(:,80))

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
  den(4) = 1 / (Q(5,18) - MB2)
  den(6) = 1 / (Q(5,24) - MB2)
  den(8) = 1 / (Q(5,9))
  den(9) = 1 / (Q(5,6))
  den(11) = 1 / (Q(5,17) - MB2)
  den(15) = 1 / (Q(5,20) - MB2)
  den(19) = 1 / (Q(5,26))
  den(22) = 1 / (Q(5,21))
  den(27) = 1 / (Q(5,13) - MB2)
  den(32) = 1 / (Q(5,7) - MB2)
  den(35) = 1 / (Q(5,22))
  den(38) = 1 / (Q(5,25))
  den(41) = 1 / (Q(5,14) - MB2)
  den(56) = 1 / (Q(5,11) - MB2)

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(1)*den(6)
  den(10) = den(8)*den(9)
  den(12) = den(9)*den(11)
  den(13) = den(6)*den(9)
  den(14) = den(4)*den(8)
  den(16) = den(8)*den(15)
  den(17) = den(2)*den(11)
  den(18) = den(2)*den(15)
  den(20) = den(2)*den(19)
  den(21) = den(1)*den(20)
  den(23) = den(1)*den(22)
  den(24) = den(2)*den(23)
  den(25) = den(4)*den(19)
  den(26) = den(1)*den(25)
  den(28) = den(1)*den(27)
  den(29) = den(4)*den(28)
  den(30) = den(6)*den(19)
  den(31) = den(1)*den(30)
  den(33) = den(1)*den(32)
  den(34) = den(6)*den(33)
  den(36) = den(9)*den(35)
  den(37) = den(8)*den(36)
  den(39) = den(8)*den(38)
  den(40) = den(9)*den(39)
  den(42) = den(9)*den(41)
  den(43) = den(11)*den(42)
  den(44) = den(11)*den(38)
  den(45) = den(9)*den(44)
  den(46) = den(6)*den(38)
  den(47) = den(9)*den(46)
  den(48) = den(9)*den(32)
  den(49) = den(6)*den(48)
  den(50) = den(4)*den(35)
  den(51) = den(8)*den(50)
  den(52) = den(8)*den(27)
  den(53) = den(4)*den(52)
  den(54) = den(15)*den(35)
  den(55) = den(8)*den(54)
  den(57) = den(8)*den(56)
  den(58) = den(15)*den(57)
  den(59) = den(2)*den(41)
  den(60) = den(11)*den(59)
  den(61) = den(11)*den(22)
  den(62) = den(2)*den(61)
  den(63) = den(15)*den(22)
  den(64) = den(2)*den(63)
  den(65) = den(2)*den(56)
  den(66) = den(15)*den(65)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(60)

  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(3) = cont_QA(wf(:,8),wf(:,9)) * den(7)
  A(4) = cont_VV(wf(:,11),wf(:,12)) * den(10)
  A(5) = cont_VV(wf(:,11),wf(:,15)) * den(12)
  A(6) = cont_QA(wf(:,8),wf(:,16)) * den(13)
  A(7) = cont_QA(wf(:,5),wf(:,17)) * den(14)
  A(8) = cont_QA(wf(:,19),wf(:,20)) * den(16)
  A(9) = cont_VV(wf(:,2),wf(:,21)) * den(17)
  A(10) = cont_QA(wf(:,19),wf(:,22)) * den(18)

  A(11) = cont_VV(wf(:,2),wf(:,23)) * den(3)
  A(12) = cont_QA(wf(:,5),wf(:,24)) * den(5)
  A(13) = cont_QA(wf(:,9),wf(:,26)) * den(7)
  A(14) = cont_VV(wf(:,11),wf(:,27)) * den(10)
  A(15) = cont_VV(wf(:,11),wf(:,28)) * den(12)
  A(16) = cont_QA(wf(:,16),wf(:,26)) * den(13)
  A(17) = cont_QA(wf(:,5),wf(:,29)) * den(14)
  A(18) = cont_QA(wf(:,20),wf(:,31)) * den(16)
  A(19) = cont_VV(wf(:,2),wf(:,32)) * den(17)
  A(20) = cont_QA(wf(:,22),wf(:,31)) * den(18)
  A(21) = cont_QA(wf(:,8),wf(:,33)) * den(7)
  A(22) = cont_QA(wf(:,6),wf(:,35)) * den(5)
  A(23) = cont_VV(wf(:,3),wf(:,36)) * den(3)
  A(24) = cont_QA(wf(:,19),wf(:,37)) * den(16)
  A(25) = cont_QA(wf(:,17),wf(:,35)) * den(14)
  A(26) = cont_VV(wf(:,21),wf(:,36)) * den(17)
  A(27) = cont_QA(wf(:,19),wf(:,38)) * den(18)
  A(28) = cont_VV(wf(:,12),wf(:,39)) * den(10)
  A(29) = cont_VV(wf(:,15),wf(:,39)) * den(12)
  A(30) = cont_QA(wf(:,8),wf(:,40)) * den(13)
  A(31) = cont_QA(wf(:,8),wf(:,41)) * den(13)
  A(32) = cont_VV(wf(:,11),wf(:,44)) * den(12)
  A(33) = cont_VV(wf(:,11),wf(:,46)) * den(10)
  A(34) = cont_QA(wf(:,19),wf(:,47)) * den(18)
  A(35) = cont_VV(wf(:,2),wf(:,48)) * den(17)
  A(36) = cont_QA(wf(:,5),wf(:,49)) * den(14)
  A(37) = cont_QA(wf(:,19),wf(:,50)) * den(16)
  A(38) = cont_VV(wf(:,2),wf(:,52)) * den(3)
  A(39) = cont_QA(wf(:,5),wf(:,53)) * den(5)
  A(40) = cont_QA(wf(:,8),wf(:,54)) * den(7)
  A(41) = cont_VV(wf(:,55),wf(:,56)) * den(21)
  A(42) = cont_VV(wf(:,3),wf(:,57)) * den(24)
  A(43) = cont_VV(wf(:,56),wf(:,58)) * den(26)
  A(44) = cont_QA(wf(:,59),wf(:,60)) * den(29)
  A(45) = cont_VV(wf(:,56),wf(:,61)) * den(31)
  A(46) = cont_QA(wf(:,62),wf(:,63)) * den(34)
  A(47) = cont_VV(wf(:,64),wf(:,65)) * den(37)
  A(48) = cont_VV(wf(:,12),wf(:,66)) * den(40)
  A(49) = cont_QA(wf(:,68),wf(:,69)) * den(43)
  A(50) = cont_VV(wf(:,15),wf(:,66)) * den(45)
  A(51) = cont_VV(wf(:,66),wf(:,70)) * den(47)
  A(52) = cont_QA(wf(:,62),wf(:,71)) * den(49)
  A(53) = cont_VV(wf(:,65),wf(:,72)) * den(51)
  A(54) = cont_QA(wf(:,59),wf(:,73)) * den(53)
  A(55) = cont_VV(wf(:,65),wf(:,74)) * den(55)
  A(56) = cont_QA(wf(:,75),wf(:,76)) * den(58)
  A(57) = cont_QA(wf(:,68),wf(:,78)) * den(60)
  A(58) = cont_VV(wf(:,21),wf(:,57)) * den(62)
  A(59) = cont_VV(wf(:,57),wf(:,79)) * den(64)
  A(60) = cont_QA(wf(:,75),wf(:,80)) * den(66)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(60)
  complex(REALKIND), intent(out) :: M1(4), M2(4)

  M1(1) = ((A(5)+A(8))*f(1))/2._/**/REALKIND+((A(9)+A(10))*f(1))/6._/**/REALKIND-(CI*A(4)*f(2))/2._/**/REALKIND
  M1(2) = ((-A(7)-A(8))*f(1))/6._/**/REALKIND+((-A(2)-A(10))*f(1))/2._/**/REALKIND-(CI*A(1)*f(2))/2._/**/REALKIND
  M1(3) = ((-A(5)-A(6))*f(1))/6._/**/REALKIND+((-A(3)-A(9))*f(1))/2._/**/REALKIND+(CI*A(1)*f(2))/2._/**/REALKIND
  M1(4) = ((A(2)+A(3))*f(1))/6._/**/REALKIND+((A(6)+A(7))*f(1))/2._/**/REALKIND+(CI*A(4)*f(2))/2._/**/REALKIND

  M2(1) = ((-A(49)-A(50)-A(55)-A(56))*f(3))/2._/**/REALKIND+((-A(57)-A(58)-A(59)-A(60))*f(3))/6._/**/REALKIND+(CI*(-A(47) &
       +A(48))*f(4))/2._/**/REALKIND+((A(19)+A(20)+A(26)+A(27)+A(34)+A(35))*f(5))/6._/**/REALKIND+((A(15)+A(18)+A(24)+A(29)+A(32) &
       +A(37))*f(5))/2._/**/REALKIND+(CI*(-A(28)-A(33))*f(6))/2._/**/REALKIND-(CI*A(14)*f(7))/2._/**/REALKIND
  M2(2) = ((A(53)+A(54)+A(55)+A(56))*f(3))/6._/**/REALKIND+((A(43)+A(44)+A(59)+A(60))*f(3))/2._/**/REALKIND+(CI*(-A(41) &
       +A(42))*f(4))/2._/**/REALKIND+((-A(17)-A(18)-A(24)-A(25)-A(36)-A(37))*f(5))/6._/**/REALKIND+((-A(12)-A(20)-A(22)-A(27) &
       -A(34)-A(39))*f(5))/2._/**/REALKIND+(CI*(-A(23)-A(38))*f(6))/2._/**/REALKIND-(CI*A(11)*f(7))/2._/**/REALKIND
  M2(3) = ((A(49)+A(50)+A(51)+A(52))*f(3))/6._/**/REALKIND+((A(45)+A(46)+A(57)+A(58))*f(3))/2._/**/REALKIND+(CI*(A(41) &
       -A(42))*f(4))/2._/**/REALKIND+((-A(15)-A(16)-A(29)-A(30)-A(31)-A(32))*f(5))/6._/**/REALKIND+((-A(13)-A(19)-A(21)-A(26) &
       -A(35)-A(40))*f(5))/2._/**/REALKIND+(CI*(A(23)+A(38))*f(6))/2._/**/REALKIND+(CI*A(11)*f(7))/2._/**/REALKIND
  M2(4) = ((-A(43)-A(44)-A(45)-A(46))*f(3))/6._/**/REALKIND+((-A(51)-A(52)-A(53)-A(54))*f(3))/2._/**/REALKIND+(CI*(A(47) &
       -A(48))*f(4))/2._/**/REALKIND+((A(16)+A(17)+A(25)+A(30)+A(31)+A(36))*f(5))/2._/**/REALKIND+((A(12)+A(13)+A(21)+A(22)+A(39) &
       +A(40))*f(5))/6._/**/REALKIND+(CI*(A(28)+A(33))*f(6))/2._/**/REALKIND+(CI*A(14)*f(7))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppjjj_bbbxbxg_1_/**/REALKIND
