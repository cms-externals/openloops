
module ol_colourmatrix_ppzjj_bbxzgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(34,2), K2(2,3), KL(2,3)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  48,  -6]
  K1( 2,:) = [  -6,  48]
  K1( 3,:) = [  64,  -8]
  K1( 4,:) = [  -8,  64]
  K1( 5,:) = [  -1, -10]
  K1( 6,:) = [ -10,  -1]
  K1( 7,:) = [  64,  -8]
  K1( 8,:) = [  -8,  64]
  K1( 9,:) = [   0,   0]
  K1(10,:) = [   0,   0]
  K1(11,:) = [   0,   0]
  K1(12,:) = [   0,   0]
  K1(13,:) = [   0,   0]
  K1(14,:) = [   0,   0]
  K1(15,:) = [   9,   9]
  K1(16,:) = [   9, -72]
  K1(17,:) = [ -72,   9]
  K1(18,:) = [   9,   9]
  K1(19,:) = [   0,   0]
  K1(20,:) = [   0,   0]
  K1(21,:) = [ 144, -18]
  K1(22,:) = [ -18, 144]
  K1(23,:) = [ -72,   9]
  K1(24,:) = [   9,   9]
  K1(25,:) = [   9,   9]
  K1(26,:) = [   9, -72]
  K1(27,:) = [   0,   0]
  K1(28,:) = [   0,   0]
  K1(29,:) = [ -81,   0]
  K1(30,:) = [   0, -81]
  K1(31,:) = [ 144, -18]
  K1(32,:) = [ -18, 144]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1 = (1._/**/REALKIND / 9) * K1

  K2(1,:) = [ 16, -2,  6]
  K2(2,:) = [ -2, 16,  6]
  K2 = (1._/**/REALKIND / 3) * K2

  KL(1,:) = [ 16, -2,  6]
  KL(2,:) = [ -2, 16,  6]
  KL = (1._/**/REALKIND / 3) * KL

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppzjj_bbxzgg_1_/**/REALKIND



module ol_forced_parameters_ppzjj_bbxzgg_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppzjj_bbxzgg_1_/**/REALKIND

module ol_loop_ppzjj_bbxzgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(18), c(17)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:91)
  ! denominators
  complex(REALKIND), save :: den(72)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,48)
  ! zero helicity identifier
  logical,           save :: zerohel(48) = .true., zerohel_ct(48) = .true.

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
    f( 1) = CI*eQED*gQCD**2
    f( 2) = eQED*gQCD**2
    f( 3) = CI*countertermnorm*eQED*gQCD**4
    f( 4) = countertermnorm*eQED*gQCD**4
    f( 5) = CI*countertermnorm*ctGbb*eQED*gQCD**4
    f( 6) = countertermnorm*ctGbb*eQED*gQCD**4
    f( 7) = CI*countertermnorm*ctVbb*eQED*gQCD**4
    f( 8) = countertermnorm*ctVbb*eQED*gQCD**4
    f( 9) = countertermnorm*ctVVV*eQED*gQCD**4
    f(10) = CI*countertermnorm*ctZGG*eQED*gQCD**4
    f(11) = countertermnorm*ctZGG*eQED*gQCD**4
    f(12) = (CI*eQED*gQCD**4*integralnorm*SwB)/2._/**/REALKIND
    f(13) = CI*eQED*gQCD**4*integralnorm*SwB
    f(14) = eQED*gQCD**4*integralnorm*SwB
    f(15) = CI*eQED*gQCD**4*integralnorm*SwF
    f(16) = 2*CI*eQED*gQCD**4*integralnorm*SwF
    f(17) = eQED*gQCD**4*integralnorm*SwF
    f(18) = 2*eQED*gQCD**4*integralnorm*SwF

  c = [ 9*CI*f(12), 18*CI*f(12), CI*f(13), 3*CI*f(13), 8*CI*f(13), 9*CI*f(13), 18*CI*f(13), f(14), 3*f(14), 8*f(14), 9*f(14) &
    , 3*CI*f(15), 3*CI*f(16), f(17), 3*f(17), f(18), 3*f(18) ]
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
  complex(REALKIND), intent(out) :: M1(2), M2(3)
  complex(REALKIND) :: A(57)
  ! external WFs
  call wf_Q(P(:,1), rMB, H(1), wf(:,0))
  call wf_A(P(:,2), rMB, H(2), wf(:,-1))
  call wf_V(P(:,3), rMZ, H(3), wf(:,-2))
  call wf_V(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))

  ! internal WFs
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,0),wf(:,1))
  call vert_AV_Q(wf(:,-1),wf(:,-3),wf(:,2))
  call prop_Q_A(wf(:,1),Q(:,5),MB,1_intkind1,wf(:,3))
  call prop_A_Q(wf(:,2),Q(:,10),MB,1_intkind1,wf(:,4))
  call vert_VQ_A(wf(:,-4),wf(:,3),wf(:,5))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,6))
  call prop_A_Q(wf(:,6),Q(:,18),MB,1_intkind1,wf(:,7))
  call vert_VQ_A(wf(:,-3),wf(:,3),wf(:,8))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,9))
  call vert_QA_V(wf(:,3),wf(:,-1),wf(:,10))
  call vert_VQ_A(wf(:,-3),wf(:,0),wf(:,11))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-2),wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,9),MB,1_intkind1,wf(:,13))
  call prop_A_Q(wf(:,12),Q(:,6),MB,1_intkind1,wf(:,14))
  call vert_VQ_A(wf(:,-4),wf(:,13),wf(:,15))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,16))
  call prop_Q_A(wf(:,16),Q(:,17),MB,1_intkind1,wf(:,17))
  call vert_VQ_A(wf(:,-3),wf(:,17),wf(:,18))
  call vert_QA_V(wf(:,0),wf(:,14),wf(:,19))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,13),wf(:,20))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,17),wf(:,21))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,22))
  call counter_VGG_G(ctZGGG,wf(:,-2),wf(:,-3),wf(:,-4),wf(:,23))
  call counter_VGG_G(ctZGGG,wf(:,-2),wf(:,-4),wf(:,-3),wf(:,24))
  call counter_VG_G(wf(:,-2),wf(:,22),Q(:,3),wf(:,25),Q(:,7))
  call counter_VG_G(wf(:,-2),wf(:,-4),Q(:,16),wf(:,26),Q(:,20))
  call vert_UV_W(wf(:,22),Q(:,3),wf(:,-3),Q(:,8),wf(:,27))
  call counter_VG_G(wf(:,-2),wf(:,-3),Q(:,8),wf(:,28),Q(:,12))
  call vert_UV_W(wf(:,22),Q(:,3),wf(:,-4),Q(:,16),wf(:,29))
  call counter_VQ_A(wf(:,-4),wf(:,3),wf(:,30))
  call counter_VQ_A(wf(:,-3),wf(:,3),wf(:,31))
  call counter_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,32))
  call counter_VQ_A(wf(:,-4),wf(:,13),wf(:,33))
  call counter_VQ_A(wf(:,-3),wf(:,17),wf(:,34))
  call counter_ZQ_A(gZd,wf(:,-2),wf(:,13),wf(:,35))
  call vert_QA_V(wf(:,13),wf(:,-1),wf(:,36))
  call counter_ZQ_A(gZd,wf(:,-2),wf(:,17),wf(:,37))
  call vert_QA_V(wf(:,0),wf(:,4),wf(:,38))
  call vert_QA_V(wf(:,17),wf(:,-1),wf(:,39))
  call vert_QA_V(wf(:,0),wf(:,7),wf(:,40))
  call counter_QA_V(wf(:,3),wf(:,-1),wf(:,41))
  call counter_AV_Q(wf(:,-1),wf(:,-4),wf(:,42))
  call prop_A_Q(wf(:,42),Q(:,18),MB,1_intkind1,wf(:,43))
  call counter_AV_Q(wf(:,-1),wf(:,-3),wf(:,44))
  call prop_A_Q(wf(:,44),Q(:,10),MB,1_intkind1,wf(:,45))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,-2),wf(:,46))
  call prop_A_Q(wf(:,46),Q(:,6),MB,1_intkind1,wf(:,47))
  call vert_QA_V(wf(:,0),wf(:,47),wf(:,48))
  call counter_QA_V(wf(:,0),wf(:,14),wf(:,49))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,50))
  call prop_Q_A(wf(:,50),Q(:,17),MB,1_intkind1,wf(:,51))
  call vert_VQ_A(wf(:,-3),wf(:,51),wf(:,52))
  call counter_VQ_A(wf(:,-3),wf(:,0),wf(:,53))
  call prop_Q_A(wf(:,53),Q(:,9),MB,1_intkind1,wf(:,54))
  call vert_VQ_A(wf(:,-4),wf(:,54),wf(:,55))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,51),wf(:,56))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,54),wf(:,57))
  call counter_ZQ_A(gZd,wf(:,-2),wf(:,0),wf(:,58))
  call prop_Q_A(wf(:,58),Q(:,5),MB,1_intkind1,wf(:,59))
  call vert_VQ_A(wf(:,-4),wf(:,59),wf(:,60))
  call vert_VQ_A(wf(:,-3),wf(:,59),wf(:,61))
  call vert_QA_V(wf(:,59),wf(:,-1),wf(:,62))
  call vert_AV_Q(wf(:,4),wf(:,-4),wf(:,63))
  call counter_Q_A(ctbb,wf(:,3),Q(:,5),wf(:,64))
  call prop_A_Q(wf(:,63),Q(:,26),MB,1_intkind1,wf(:,65))
  call counter_A_Q(ctbb,wf(:,4),Q(:,10),wf(:,66))
  call prop_Q_A(wf(:,5),Q(:,21),MB,1_intkind1,wf(:,67))
  call vert_AV_Q(wf(:,7),wf(:,-3),wf(:,68))
  call prop_A_Q(wf(:,68),Q(:,26),MB,1_intkind1,wf(:,69))
  call counter_A_Q(ctbb,wf(:,7),Q(:,18),wf(:,70))
  call prop_Q_A(wf(:,8),Q(:,13),MB,1_intkind1,wf(:,71))
  call vert_AV_Q(wf(:,-1),wf(:,9),wf(:,72))
  call prop_A_Q(wf(:,72),Q(:,26),MB,1_intkind1,wf(:,73))
  call counter_V_V(ctGG,wf(:,9),Q(:,24),wf(:,74))
  call vert_AV_Q(wf(:,14),wf(:,-4),wf(:,75))
  call counter_Q_A(ctbb,wf(:,13),Q(:,9),wf(:,76))
  call prop_A_Q(wf(:,75),Q(:,22),MB,1_intkind1,wf(:,77))
  call counter_A_Q(ctbb,wf(:,14),Q(:,6),wf(:,78))
  call prop_Q_A(wf(:,15),Q(:,25),MB,1_intkind1,wf(:,79))
  call vert_AV_Q(wf(:,14),wf(:,-3),wf(:,80))
  call counter_Q_A(ctbb,wf(:,17),Q(:,17),wf(:,81))
  call prop_A_Q(wf(:,80),Q(:,14),MB,1_intkind1,wf(:,82))
  call prop_Q_A(wf(:,18),Q(:,25),MB,1_intkind1,wf(:,83))
  call vert_VQ_A(wf(:,9),wf(:,0),wf(:,84))
  call prop_Q_A(wf(:,84),Q(:,25),MB,1_intkind1,wf(:,85))
  call vert_AZ_Q(gZd,wf(:,7),wf(:,-2),wf(:,86))
  call prop_A_Q(wf(:,86),Q(:,22),MB,1_intkind1,wf(:,87))
  call prop_Q_A(wf(:,20),Q(:,13),MB,1_intkind1,wf(:,88))
  call vert_AZ_Q(gZd,wf(:,4),wf(:,-2),wf(:,89))
  call prop_A_Q(wf(:,89),Q(:,14),MB,1_intkind1,wf(:,90))
  call prop_Q_A(wf(:,21),Q(:,21),MB,1_intkind1,wf(:,91))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5) - MB2)
  den(2) = 1 / (Q(5,10) - MB2)
  den(4) = 1 / (Q(5,18) - MB2)
  den(6) = 1 / (Q(5,24))
  den(8) = 1 / (Q(5,9) - MB2)
  den(9) = 1 / (Q(5,6) - MB2)
  den(11) = 1 / (Q(5,17) - MB2)
  den(16) = 1 / (Q(5,3))
  den(18) = 1 / (Q(5,20))
  den(20) = 1 / (Q(5,12))
  den(26) = 1 / (Q(5,26) - MB2)
  den(29) = 1 / (Q(5,21) - MB2)
  den(34) = 1 / (Q(5,13) - MB2)
  den(39) = 1 / (Q(5,7))
  den(42) = 1 / (Q(5,22) - MB2)
  den(45) = 1 / (Q(5,25) - MB2)
  den(48) = 1 / (Q(5,14) - MB2)
  den(65) = 1 / (Q(5,11))
  den(67) = 1 / (Q(5,19))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(1)*den(6)
  den(10) = den(8)*den(9)
  den(12) = den(9)*den(11)
  den(13) = den(6)*den(9)
  den(14) = den(4)*den(8)
  den(15) = den(2)*den(11)
  den(17) = den(6)*den(16)
  den(19) = den(16)*den(18)
  den(21) = den(16)*den(20)
  den(22) = den(8)*den(18)
  den(23) = den(2)*den(18)
  den(24) = den(11)*den(20)
  den(25) = den(4)*den(20)
  den(27) = den(2)*den(26)
  den(28) = den(1)*den(27)
  den(30) = den(1)*den(29)
  den(31) = den(2)*den(30)
  den(32) = den(4)*den(26)
  den(33) = den(1)*den(32)
  den(35) = den(1)*den(34)
  den(36) = den(4)*den(35)
  den(37) = den(6)*den(26)
  den(38) = den(1)*den(37)
  den(40) = den(1)*den(39)
  den(41) = den(6)*den(40)
  den(43) = den(9)*den(42)
  den(44) = den(8)*den(43)
  den(46) = den(8)*den(45)
  den(47) = den(9)*den(46)
  den(49) = den(9)*den(48)
  den(50) = den(11)*den(49)
  den(51) = den(11)*den(45)
  den(52) = den(9)*den(51)
  den(53) = den(6)*den(45)
  den(54) = den(9)*den(53)
  den(55) = den(9)*den(39)
  den(56) = den(6)*den(55)
  den(57) = den(4)*den(42)
  den(58) = den(8)*den(57)
  den(59) = den(8)*den(34)
  den(60) = den(4)*den(59)
  den(61) = den(2)*den(48)
  den(62) = den(11)*den(61)
  den(63) = den(11)*den(29)
  den(64) = den(2)*den(63)
  den(66) = den(16)*den(65)
  den(68) = den(16)*den(67)
  den(69) = den(8)*den(65)
  den(70) = den(2)*den(65)
  den(71) = den(11)*den(67)
  den(72) = den(4)*den(67)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(57)

  A(1) = cont_QA(wf(:,4),wf(:,5)) * den(3)
  A(2) = cont_QA(wf(:,7),wf(:,8)) * den(5)
  A(3) = cont_VV(wf(:,9),wf(:,10)) * den(7)
  A(4) = cont_QA(wf(:,14),wf(:,15)) * den(10)
  A(5) = cont_QA(wf(:,14),wf(:,18)) * den(12)
  A(6) = cont_VV(wf(:,9),wf(:,19)) * den(13)
  A(7) = cont_QA(wf(:,7),wf(:,20)) * den(14)
  A(8) = cont_QA(wf(:,4),wf(:,21)) * den(15)

  A(9) = cont_VV(wf(:,22),wf(:,23)) * den(16)
  A(10) = cont_VV(wf(:,22),wf(:,24)) * den(16)
  A(11) = cont_VV(wf(:,9),wf(:,25)) * den(17)
  A(12) = cont_VV(wf(:,26),wf(:,27)) * den(19)
  A(13) = cont_VV(wf(:,28),wf(:,29)) * den(21)
  A(14) = cont_QA(wf(:,4),wf(:,30)) * den(3)
  A(15) = cont_QA(wf(:,7),wf(:,31)) * den(5)
  A(16) = cont_VV(wf(:,10),wf(:,32)) * den(7)
  A(17) = cont_QA(wf(:,14),wf(:,33)) * den(10)
  A(18) = cont_QA(wf(:,14),wf(:,34)) * den(12)
  A(19) = cont_VV(wf(:,19),wf(:,32)) * den(13)
  A(20) = cont_QA(wf(:,7),wf(:,35)) * den(14)
  A(21) = cont_VV(wf(:,26),wf(:,36)) * den(22)
  A(22) = cont_QA(wf(:,4),wf(:,37)) * den(15)
  A(23) = cont_VV(wf(:,26),wf(:,38)) * den(23)
  A(24) = cont_VV(wf(:,28),wf(:,39)) * den(24)
  A(25) = cont_VV(wf(:,28),wf(:,40)) * den(25)
  A(26) = cont_VV(wf(:,9),wf(:,41)) * den(7)
  A(27) = cont_QA(wf(:,8),wf(:,43)) * den(5)
  A(28) = cont_QA(wf(:,5),wf(:,45)) * den(3)
  A(29) = cont_QA(wf(:,20),wf(:,43)) * den(14)
  A(30) = cont_QA(wf(:,21),wf(:,45)) * den(15)
  A(31) = cont_QA(wf(:,15),wf(:,47)) * den(10)
  A(32) = cont_QA(wf(:,18),wf(:,47)) * den(12)
  A(33) = cont_VV(wf(:,9),wf(:,48)) * den(13)
  A(34) = cont_VV(wf(:,9),wf(:,49)) * den(13)
  A(35) = cont_QA(wf(:,14),wf(:,52)) * den(12)
  A(36) = cont_QA(wf(:,14),wf(:,55)) * den(10)
  A(37) = cont_QA(wf(:,4),wf(:,56)) * den(15)
  A(38) = cont_QA(wf(:,7),wf(:,57)) * den(14)
  A(39) = cont_QA(wf(:,4),wf(:,60)) * den(3)
  A(40) = cont_QA(wf(:,7),wf(:,61)) * den(5)
  A(41) = cont_VV(wf(:,9),wf(:,62)) * den(7)
  A(42) = cont_QA(wf(:,64),wf(:,65)) * den(28)
  A(43) = cont_QA(wf(:,66),wf(:,67)) * den(31)
  A(44) = cont_QA(wf(:,64),wf(:,69)) * den(33)
  A(45) = cont_QA(wf(:,70),wf(:,71)) * den(36)
  A(46) = cont_QA(wf(:,64),wf(:,73)) * den(38)
  A(47) = cont_VV(wf(:,10),wf(:,74)) * den(41)
  A(48) = cont_QA(wf(:,76),wf(:,77)) * den(44)
  A(49) = cont_QA(wf(:,78),wf(:,79)) * den(47)
  A(50) = cont_QA(wf(:,81),wf(:,82)) * den(50)
  A(51) = cont_QA(wf(:,78),wf(:,83)) * den(52)
  A(52) = cont_QA(wf(:,78),wf(:,85)) * den(54)
  A(53) = cont_VV(wf(:,19),wf(:,74)) * den(56)
  A(54) = cont_QA(wf(:,76),wf(:,87)) * den(58)
  A(55) = cont_QA(wf(:,70),wf(:,88)) * den(60)
  A(56) = cont_QA(wf(:,81),wf(:,90)) * den(62)
  A(57) = cont_QA(wf(:,66),wf(:,91)) * den(64)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(57)
  complex(REALKIND), intent(out) :: M1(2), M2(3)

  M1(1) = (-A(1)-A(5)-A(8))*f(1)+CI*(-A(3)-A(6))*f(2)
  M1(2) = (-A(2)-A(4)-A(7))*f(1)+CI*(A(3)+A(6))*f(2)

  M2(1) = -(A(9)*f(3))/2._/**/REALKIND+(A(42)+A(43)+A(50)+A(51)+A(56)+A(57))*f(3)+CI*(A(46)+A(47)+A(52)+A(53))*f(4)+(-A(14)-A(18) &
       -A(28)-A(30)-A(35)-A(37))*f(5)+CI*(-A(26)-A(34))*f(6)+(-A(22)-A(32)-A(39))*f(7)+CI*(-A(33)-A(41))*f(8)+CI*(-A(16) &
       -A(19))*f(9)+CI*(-A(11)-A(12)+A(13))*f(10)+(A(23)+A(24))*f(11)
  M2(2) = -(A(10)*f(3))/2._/**/REALKIND+(A(44)+A(45)+A(48)+A(49)+A(54)+A(55))*f(3)+CI*(-A(46)-A(47)-A(52)-A(53))*f(4)+(-A(15) &
       -A(17)-A(27)-A(29)-A(36)-A(38))*f(5)+CI*(A(26)+A(34))*f(6)+(-A(20)-A(31)-A(40))*f(7)+CI*(A(33)+A(41))*f(8)+CI*(A(16) &
       +A(19))*f(9)+CI*(A(11)+A(12)-A(13))*f(10)+(A(21)+A(25))*f(11)
  M2(3) = ((A(9)+A(10))*f(3))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppzjj_bbxzgg_1_/**/REALKIND
