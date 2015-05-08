
module ol_colourmatrix_ppttj_ttxbbxg_1_/**/REALKIND
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
  K1( 9,:) = [   6,   2,   2,   0]
  K1(10,:) = [   2,   0,  -6, -16]
  K1(11,:) = [   2,  -6,   0, -16]
  K1(12,:) = [   0, -16, -16, -48]
  K1(13,:) = [  48,  16,  16,   0]
  K1(14,:) = [  16,  48,   0,  16]
  K1(15,:) = [  16,   0,  48,  16]
  K1(16,:) = [   0,  16,  16,  48]
  K1(17,:) = [   0,  16,  -2,   6]
  K1(18,:) = [  16,   0,   6,  -2]
  K1(19,:) = [  -2,   6,   0,  16]
  K1(20,:) = [   6,  -2,  16,   0]
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
  K1(33,:) = [   0,  -2,  16,   6]
  K1(34,:) = [  -2,   0,   6,  16]
  K1(35,:) = [  16,   6,   0,  -2]
  K1(36,:) = [   6,  16,  -2,   0]
  K1(37,:) = [ -48, -16, -16,   0]
  K1(38,:) = [ -16,   0,  -6,   2]
  K1(39,:) = [ -16,  -6,   0,   2]
  K1(40,:) = [   0,   2,   2,   6]
  K1(41,:) = [  48,  16,  16,   0]
  K1(42,:) = [  16,  48,   0,  16]
  K1(43,:) = [  16,   0,  48,  16]
  K1(44,:) = [   0,  16,  16,  48]
  K1(45,:) = [ -54, -18, -18,   0]
  K1(46,:) = [ -18,   0,   0,  18]
  K1(47,:) = [ -18,   0, -54, -18]
  K1(48,:) = [   0,  18, -18,   0]
  K1(49,:) = [ -54, -18, -18,   0]
  K1(50,:) = [ -18, -54,   0, -18]
  K1(51,:) = [ -18,   0,   0,  18]
  K1(52,:) = [   0, -18,  18,   0]
  K1(53,:) = [   0, -18,  18,   0]
  K1(54,:) = [ -18, -54,   0, -18]
  K1(55,:) = [  18,   0,   0, -18]
  K1(56,:) = [   0, -18, -18, -54]
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
end module ol_colourmatrix_ppttj_ttxbbxg_1_/**/REALKIND



module ol_forced_parameters_ppttj_ttxbbxg_1_/**/REALKIND
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
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppttj_ttxbbxg_1_/**/REALKIND

module ol_loop_ppttj_ttxbbxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(17), c(26)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:50)
  ! denominators
  complex(REALKIND), save :: den(37)
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
    f( 7) = CI*countertermnorm*ctGtt*gQCD**5
    f( 8) = countertermnorm*ctGtt*gQCD**5
    f( 9) = countertermnorm*ctVVV*gQCD**5
    f(10) = (CI*gQCD**5*integralnorm*SwB)/2._/**/REALKIND
    f(11) = CI*gQCD**5*integralnorm*SwB
    f(12) = (gQCD**5*integralnorm*SwB)/2._/**/REALKIND
    f(13) = gQCD**5*integralnorm*SwB
    f(14) = CI*gQCD**5*integralnorm*SwF
    f(15) = 2*CI*gQCD**5*integralnorm*SwF
    f(16) = gQCD**5*integralnorm*SwF
    f(17) = 2*gQCD**5*integralnorm*SwF

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
  real(REALKIND),    intent(in)  :: P(0:3,5)
  integer,           intent(in)  :: H(5)
  complex(REALKIND), intent(out) :: M1(4), M2(4)
  complex(REALKIND) :: A(30)
  ! external WFs
  call wf_Q(P(:,1), rMT, H(1), wf(:,0))
  call wf_A(P(:,2), rMT, H(2), wf(:,-1))
  call wf_Q(P(:,3), rMB, H(3), wf(:,-2))
  call wf_A(P(:,4), rMB, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,3))
  call vert_VQ_A(wf(:,-4),wf(:,-2),wf(:,4))
  call prop_Q_A(wf(:,4),Q(:,20),MB,1_intkind1,wf(:,5))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,6))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,7))
  call prop_A_Q(wf(:,7),Q(:,24),MB,1_intkind1,wf(:,8))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,9))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,10))
  call prop_Q_A(wf(:,10),Q(:,17),MT,1_intkind1,wf(:,11))
  call vert_QA_V(wf(:,11),wf(:,-1),wf(:,12))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,13))
  call prop_A_Q(wf(:,13),Q(:,18),MT,1_intkind1,wf(:,14))
  call vert_QA_V(wf(:,0),wf(:,14),wf(:,15))
  call counter_UV_W(wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,16))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,17))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,18))
  call prop_A_Q(wf(:,18),Q(:,24),MB,1_intkind1,wf(:,19))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,20))
  call counter_VQ_A(wf(:,-4),wf(:,-2),wf(:,21))
  call prop_Q_A(wf(:,21),Q(:,20),MB,1_intkind1,wf(:,22))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,23))
  call counter_QA_V(wf(:,11),wf(:,-1),wf(:,24))
  call counter_AV_Q(wf(:,-1),wf(:,-4),wf(:,25))
  call prop_A_Q(wf(:,25),Q(:,18),MT,1_intkind1,wf(:,26))
  call vert_QA_V(wf(:,0),wf(:,26),wf(:,27))
  call counter_QA_V(wf(:,0),wf(:,14),wf(:,28))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,29))
  call prop_Q_A(wf(:,29),Q(:,17),MT,1_intkind1,wf(:,30))
  call vert_QA_V(wf(:,30),wf(:,-1),wf(:,31))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,32))
  call vert_UV_W(wf(:,32),Q(:,3),wf(:,-4),Q(:,16),wf(:,33))
  call vert_AV_Q(wf(:,-3),wf(:,32),wf(:,34))
  call vert_VQ_A(wf(:,32),wf(:,-2),wf(:,35))
  call vert_UV_W(wf(:,2),Q(:,12),wf(:,-4),Q(:,16),wf(:,36))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,37))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,38))
  call vert_QA_V(wf(:,5),wf(:,-3),wf(:,39))
  call counter_Q_A(ctbb,wf(:,5),Q(:,20),wf(:,40))
  call prop_A_Q(wf(:,6),Q(:,11),MB,1_intkind1,wf(:,41))
  call vert_QA_V(wf(:,-2),wf(:,8),wf(:,42))
  call counter_A_Q(ctbb,wf(:,8),Q(:,24),wf(:,43))
  call prop_Q_A(wf(:,9),Q(:,7),MB,1_intkind1,wf(:,44))
  call vert_AV_Q(wf(:,-1),wf(:,2),wf(:,45))
  call counter_Q_A(cttt,wf(:,11),Q(:,17),wf(:,46))
  call prop_A_Q(wf(:,45),Q(:,14),MT,1_intkind1,wf(:,47))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,48))
  call counter_A_Q(cttt,wf(:,14),Q(:,18),wf(:,49))
  call prop_Q_A(wf(:,48),Q(:,13),MT,1_intkind1,wf(:,50))

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
  den(4) = 1 / (Q(5,20) - MB2)
  den(6) = 1 / (Q(5,24) - MB2)
  den(8) = 1 / (Q(5,17) - MT2)
  den(10) = 1 / (Q(5,18) - MT2)
  den(12) = 1 / (Q(5,28))
  den(15) = 1 / (Q(5,19))
  den(20) = 1 / (Q(5,11) - MB2)
  den(25) = 1 / (Q(5,7) - MB2)
  den(28) = 1 / (Q(5,14) - MT2)
  den(33) = 1 / (Q(5,13) - MT2)

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(1)*den(6)
  den(9) = den(2)*den(8)
  den(11) = den(2)*den(10)
  den(13) = den(2)*den(12)
  den(14) = den(1)*den(13)
  den(16) = den(1)*den(15)
  den(17) = den(2)*den(16)
  den(18) = den(4)*den(12)
  den(19) = den(1)*den(18)
  den(21) = den(1)*den(20)
  den(22) = den(4)*den(21)
  den(23) = den(6)*den(12)
  den(24) = den(1)*den(23)
  den(26) = den(1)*den(25)
  den(27) = den(6)*den(26)
  den(29) = den(2)*den(28)
  den(30) = den(8)*den(29)
  den(31) = den(8)*den(15)
  den(32) = den(2)*den(31)
  den(34) = den(2)*den(33)
  den(35) = den(10)*den(34)
  den(36) = den(10)*den(15)
  den(37) = den(2)*den(36)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(30)

  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(3) = cont_QA(wf(:,8),wf(:,9)) * den(7)
  A(4) = cont_VV(wf(:,2),wf(:,12)) * den(9)
  A(5) = cont_VV(wf(:,2),wf(:,15)) * den(11)

  A(6) = cont_VV(wf(:,2),wf(:,16)) * den(3)
  A(7) = cont_QA(wf(:,5),wf(:,17)) * den(5)
  A(8) = cont_QA(wf(:,9),wf(:,19)) * den(7)
  A(9) = cont_QA(wf(:,8),wf(:,20)) * den(7)
  A(10) = cont_QA(wf(:,6),wf(:,22)) * den(5)
  A(11) = cont_VV(wf(:,3),wf(:,23)) * den(3)
  A(12) = cont_VV(wf(:,12),wf(:,23)) * den(9)
  A(13) = cont_VV(wf(:,15),wf(:,23)) * den(11)
  A(14) = cont_VV(wf(:,2),wf(:,24)) * den(9)
  A(15) = cont_VV(wf(:,2),wf(:,27)) * den(11)
  A(16) = cont_VV(wf(:,2),wf(:,28)) * den(11)
  A(17) = cont_VV(wf(:,2),wf(:,31)) * den(9)
  A(18) = cont_VV(wf(:,2),wf(:,33)) * den(3)
  A(19) = cont_QA(wf(:,5),wf(:,34)) * den(5)
  A(20) = cont_QA(wf(:,8),wf(:,35)) * den(7)
  A(21) = cont_VV(wf(:,36),wf(:,37)) * den(14)
  A(22) = cont_VV(wf(:,3),wf(:,38)) * den(17)
  A(23) = cont_VV(wf(:,37),wf(:,39)) * den(19)
  A(24) = cont_QA(wf(:,40),wf(:,41)) * den(22)
  A(25) = cont_VV(wf(:,37),wf(:,42)) * den(24)
  A(26) = cont_QA(wf(:,43),wf(:,44)) * den(27)
  A(27) = cont_QA(wf(:,46),wf(:,47)) * den(30)
  A(28) = cont_VV(wf(:,12),wf(:,38)) * den(32)
  A(29) = cont_QA(wf(:,49),wf(:,50)) * den(35)
  A(30) = cont_VV(wf(:,15),wf(:,38)) * den(37)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(30)
  complex(REALKIND), intent(out) :: M1(4), M2(4)

  M1(1) = ((-A(4)-A(5))*f(1))/6._/**/REALKIND
  M1(2) = ((A(2)+A(5))*f(1))/2._/**/REALKIND+(CI*A(1)*f(2))/2._/**/REALKIND
  M1(3) = ((A(3)+A(4))*f(1))/2._/**/REALKIND-(CI*A(1)*f(2))/2._/**/REALKIND
  M1(4) = ((-A(2)-A(3))*f(1))/6._/**/REALKIND

  M2(1) = ((A(27)+A(28)+A(29)+A(30))*f(3))/6._/**/REALKIND+((-A(12)-A(13))*f(5))/6._/**/REALKIND+((-A(14)-A(15)-A(16) &
       -A(17))*f(7))/6._/**/REALKIND
  M2(2) = ((-A(23)-A(24)-A(29)-A(30))*f(3))/2._/**/REALKIND+(CI*(A(21)-A(22))*f(4))/2._/**/REALKIND+((A(7)+A(10) &
       +A(13))*f(5))/2._/**/REALKIND+(CI*A(11)*f(6))/2._/**/REALKIND+((A(15)+A(16)+A(19))*f(7))/2._/**/REALKIND &
       +(CI*A(18)*f(8))/2._/**/REALKIND+(CI*A(6)*f(9))/2._/**/REALKIND
  M2(3) = ((-A(25)-A(26)-A(27)-A(28))*f(3))/2._/**/REALKIND+(CI*(-A(21)+A(22))*f(4))/2._/**/REALKIND+((A(8)+A(9) &
       +A(12))*f(5))/2._/**/REALKIND-(CI*A(11)*f(6))/2._/**/REALKIND+((A(14)+A(17)+A(20))*f(7))/2._/**/REALKIND &
       -(CI*A(18)*f(8))/2._/**/REALKIND-(CI*A(6)*f(9))/2._/**/REALKIND
  M2(4) = ((A(23)+A(24)+A(25)+A(26))*f(3))/6._/**/REALKIND+((-A(7)-A(8)-A(9)-A(10))*f(5))/6._/**/REALKIND+((-A(19) &
       -A(20))*f(7))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppttj_ttxbbxg_1_/**/REALKIND
