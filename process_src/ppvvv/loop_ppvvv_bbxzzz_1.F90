
module ol_colourmatrix_ppvvv_bbxzzz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(17,1), K2(1,1), KL(1,1)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  3]
  K1( 2,:) = [  4]
  K1( 3,:) = [ -4]
  K1( 4,:) = [  4]
  K1( 5,:) = [  0]
  K1( 6,:) = [  0]
  K1( 7,:) = [  0]
  K1( 8,:) = [  0]
  K1( 9,:) = [  0]
  K1(10,:) = [  0]
  K1(11,:) = [  0]
  K1(12,:) = [  0]
  K1(13,:) = [  0]
  K1(14,:) = [  0]
  K1(15,:) = [  0]
  K1(16,:) = [  0]
  K1(17,:) = [  0]

  K2(1,:) = [ 3]

  KL(1,:) = [ 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppvvv_bbxzzz_1_/**/REALKIND



module ol_forced_parameters_ppvvv_bbxzzz_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppvvv_bbxzzz_1_/**/REALKIND

module ol_loop_ppvvv_bbxzzz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(12), c(3)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:115)
  ! denominators
  complex(REALKIND), save :: den(71)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,108)
  ! zero helicity identifier
  logical,           save :: zerohel(108) = .true., zerohel_ct(108) = .true.

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
    f( 1) = CI*eQED**3
    f( 2) = CI*countertermnorm*eQED**3*gQCD**2
    f( 3) = CI*countertermnorm*ctVbb*eQED**3*gQCD**2
    f( 4) = (CI*eQED**3*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f( 5) = (CI*countertermnorm*eQED**3*gQCD**2*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f( 6) = (CI*countertermnorm*ctSbb*eQED**3*gQCD**2*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f( 7) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**2*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f( 8) = (CI*eQED**3*MW**2)/(cw**4*sw**2)
    f( 9) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**2*MW**2)/(cw**4*sw**2)
    f(10) = eQED**3*gQCD**2*integralnorm*SwB
    f(11) = (eQED**3*gQCD**2*integralnorm*MB*SwB)/(cw**2*sw**2*2._/**/REALKIND)
    f(12) = (eQED**3*gQCD**2*integralnorm*MW**2*SwB)/(cw**4*sw**2)

  c = [ 4*f(10), 4*f(11), 4*f(12) ]
  c = (1._/**/REALKIND / 3) * c
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
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(66)
  ! external WFs
  call wf_Q(P(:,1), rMB, H(1), wf(:,0))
  call wf_A(P(:,2), rMB, H(2), wf(:,-1))
  call wf_V(P(:,3), rMZ, H(3), wf(:,-2))
  call wf_V(P(:,4), rMZ, H(4), wf(:,-3))
  call wf_V(P(:,5), rMZ, H(5), wf(:,-4))

  ! internal WFs
  call vert_QA_Z(gZd,wf(:,0),wf(:,-1),wf(:,1))
  call vert_VV_S(wf(:,-2),wf(:,-3),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,3))
  call vert_VV_S(wf(:,3),wf(:,-4),wf(:,4))
  call vert_VV_S(wf(:,-2),wf(:,-4),wf(:,5))
  call vert_VV_S(wf(:,3),wf(:,-3),wf(:,6))
  call vert_VV_S(wf(:,-3),wf(:,-4),wf(:,7))
  call vert_VV_S(wf(:,3),wf(:,-2),wf(:,8))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,0),wf(:,9))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-3),wf(:,10))
  call prop_Q_A(wf(:,9),Q(:,5),MB,1_intkind1,wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,10),MB,1_intkind1,wf(:,12))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,11),wf(:,13))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-4),wf(:,14))
  call prop_A_Q(wf(:,14),Q(:,18),MB,1_intkind1,wf(:,15))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,11),wf(:,16))
  call vert_AQ_S(gH,wf(:,-1),wf(:,11),wf(:,17))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,0),wf(:,18))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-2),wf(:,19))
  call prop_Q_A(wf(:,18),Q(:,9),MB,1_intkind1,wf(:,20))
  call prop_A_Q(wf(:,19),Q(:,6),MB,1_intkind1,wf(:,21))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,20),wf(:,22))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,0),wf(:,23))
  call prop_Q_A(wf(:,23),Q(:,17),MB,1_intkind1,wf(:,24))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,24),wf(:,25))
  call vert_AQ_S(gH,wf(:,21),wf(:,0),wf(:,26))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,20),wf(:,27))
  call vert_AQ_S(gH,wf(:,-1),wf(:,20),wf(:,28))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,24),wf(:,29))
  call vert_AQ_S(gH,wf(:,12),wf(:,0),wf(:,30))
  call vert_AQ_S(gH,wf(:,-1),wf(:,24),wf(:,31))
  call vert_AQ_S(gH,wf(:,15),wf(:,0),wf(:,32))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,11),wf(:,33))
  call counter_ZQ_A(gZd,wf(:,-3),wf(:,11),wf(:,34))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,20),wf(:,35))
  call counter_ZQ_A(gZd,wf(:,-3),wf(:,24),wf(:,36))
  call counter_ZQ_A(gZd,wf(:,-2),wf(:,20),wf(:,37))
  call counter_ZQ_A(gZd,wf(:,-2),wf(:,24),wf(:,38))
  call counter_AQ_S(gH,wf(:,-1),wf(:,11),wf(:,39))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,-4),wf(:,40))
  call prop_A_Q(wf(:,40),Q(:,18),MB,1_intkind1,wf(:,41))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,-3),wf(:,42))
  call prop_A_Q(wf(:,42),Q(:,10),MB,1_intkind1,wf(:,43))
  call counter_AQ_S(gH,wf(:,-1),wf(:,20),wf(:,44))
  call counter_AQ_S(gH,wf(:,-1),wf(:,24),wf(:,45))
  call vert_AQ_S(gH,wf(:,41),wf(:,0),wf(:,46))
  call vert_AQ_S(gH,wf(:,43),wf(:,0),wf(:,47))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,-2),wf(:,48))
  call prop_A_Q(wf(:,48),Q(:,6),MB,1_intkind1,wf(:,49))
  call vert_AQ_S(gH,wf(:,49),wf(:,0),wf(:,50))
  call counter_AQ_S(gH,wf(:,21),wf(:,0),wf(:,51))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,0),wf(:,52))
  call prop_Q_A(wf(:,52),Q(:,17),MB,1_intkind1,wf(:,53))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,53),wf(:,54))
  call counter_ZQ_A(gZd,wf(:,-3),wf(:,0),wf(:,55))
  call prop_Q_A(wf(:,55),Q(:,9),MB,1_intkind1,wf(:,56))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,56),wf(:,57))
  call counter_AQ_S(gH,wf(:,12),wf(:,0),wf(:,58))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,53),wf(:,59))
  call counter_AQ_S(gH,wf(:,15),wf(:,0),wf(:,60))
  call vert_AQ_S(gH,wf(:,-1),wf(:,53),wf(:,61))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,56),wf(:,62))
  call vert_AQ_S(gH,wf(:,-1),wf(:,56),wf(:,63))
  call counter_ZQ_A(gZd,wf(:,-2),wf(:,0),wf(:,64))
  call prop_Q_A(wf(:,64),Q(:,5),MB,1_intkind1,wf(:,65))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,65),wf(:,66))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,65),wf(:,67))
  call vert_AQ_S(gH,wf(:,-1),wf(:,65),wf(:,68))
  call counter_QA_Z(gZd,wf(:,0),wf(:,-1),wf(:,69))
  call prop_W_W(wf(:,69),Q(:,3),MZ,1_intkind1,wf(:,70))
  call vert_VV_S(wf(:,70),wf(:,-4),wf(:,71))
  call vert_VV_S(wf(:,70),wf(:,-3),wf(:,72))
  call vert_VV_S(wf(:,70),wf(:,-2),wf(:,73))
  call vert_AZ_Q(gZd,wf(:,12),wf(:,-4),wf(:,74))
  call counter_Q_A(ctbb,wf(:,11),Q(:,5),wf(:,75))
  call prop_A_Q(wf(:,74),Q(:,26),MB,1_intkind1,wf(:,76))
  call counter_A_Q(ctbb,wf(:,12),Q(:,10),wf(:,77))
  call prop_Q_A(wf(:,13),Q(:,21),MB,1_intkind1,wf(:,78))
  call vert_AZ_Q(gZd,wf(:,15),wf(:,-3),wf(:,79))
  call prop_A_Q(wf(:,79),Q(:,26),MB,1_intkind1,wf(:,80))
  call counter_A_Q(ctbb,wf(:,15),Q(:,18),wf(:,81))
  call prop_Q_A(wf(:,16),Q(:,13),MB,1_intkind1,wf(:,82))
  call vert_SA_Q(gH,wf(:,7),wf(:,-1),wf(:,83))
  call prop_A_Q(wf(:,83),Q(:,26),MB,1_intkind1,wf(:,84))
  call vert_AZ_Q(gZd,wf(:,21),wf(:,-4),wf(:,85))
  call counter_Q_A(ctbb,wf(:,20),Q(:,9),wf(:,86))
  call prop_A_Q(wf(:,85),Q(:,22),MB,1_intkind1,wf(:,87))
  call counter_A_Q(ctbb,wf(:,21),Q(:,6),wf(:,88))
  call prop_Q_A(wf(:,22),Q(:,25),MB,1_intkind1,wf(:,89))
  call vert_AZ_Q(gZd,wf(:,21),wf(:,-3),wf(:,90))
  call counter_Q_A(ctbb,wf(:,24),Q(:,17),wf(:,91))
  call prop_A_Q(wf(:,90),Q(:,14),MB,1_intkind1,wf(:,92))
  call prop_Q_A(wf(:,25),Q(:,25),MB,1_intkind1,wf(:,93))
  call vert_QS_A(gH,wf(:,0),wf(:,7),wf(:,94))
  call prop_Q_A(wf(:,94),Q(:,25),MB,1_intkind1,wf(:,95))
  call vert_AZ_Q(gZd,wf(:,15),wf(:,-2),wf(:,96))
  call prop_A_Q(wf(:,96),Q(:,22),MB,1_intkind1,wf(:,97))
  call prop_Q_A(wf(:,27),Q(:,13),MB,1_intkind1,wf(:,98))
  call vert_SA_Q(gH,wf(:,5),wf(:,-1),wf(:,99))
  call prop_A_Q(wf(:,99),Q(:,22),MB,1_intkind1,wf(:,100))
  call vert_AZ_Q(gZd,wf(:,12),wf(:,-2),wf(:,101))
  call prop_A_Q(wf(:,101),Q(:,14),MB,1_intkind1,wf(:,102))
  call prop_Q_A(wf(:,29),Q(:,21),MB,1_intkind1,wf(:,103))
  call vert_QS_A(gH,wf(:,0),wf(:,5),wf(:,104))
  call prop_Q_A(wf(:,104),Q(:,21),MB,1_intkind1,wf(:,105))
  call vert_SA_Q(gH,wf(:,2),wf(:,-1),wf(:,106))
  call prop_A_Q(wf(:,106),Q(:,14),MB,1_intkind1,wf(:,107))
  call vert_QS_A(gH,wf(:,0),wf(:,2),wf(:,108))
  call prop_Q_A(wf(:,108),Q(:,13),MB,1_intkind1,wf(:,109))
  call vert_SV_V(wf(:,2),wf(:,-4),wf(:,110))
  call prop_W_W(wf(:,110),Q(:,28),MZ,1_intkind1,wf(:,111))
  call vert_SV_V(wf(:,5),wf(:,-3),wf(:,112))
  call prop_W_W(wf(:,112),Q(:,28),MZ,1_intkind1,wf(:,113))
  call vert_SV_V(wf(:,7),wf(:,-2),wf(:,114))
  call prop_W_W(wf(:,114),Q(:,28),MZ,1_intkind1,wf(:,115))

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
  den(2) = 1 / (Q(5,12) - MH2)
  den(4) = 1 / (Q(5,20) - MH2)
  den(6) = 1 / (Q(5,24) - MH2)
  den(8) = 1 / (Q(5,5) - MB2)
  den(9) = 1 / (Q(5,10) - MB2)
  den(11) = 1 / (Q(5,18) - MB2)
  den(14) = 1 / (Q(5,9) - MB2)
  den(15) = 1 / (Q(5,6) - MB2)
  den(17) = 1 / (Q(5,17) - MB2)
  den(26) = 1 / (Q(5,26) - MB2)
  den(29) = 1 / (Q(5,21) - MB2)
  den(34) = 1 / (Q(5,13) - MB2)
  den(39) = 1 / (Q(5,22) - MB2)
  den(42) = 1 / (Q(5,25) - MB2)
  den(45) = 1 / (Q(5,14) - MB2)
  den(68) = 1 / (Q(5,28) - MZ2)

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
  den(27) = den(9)*den(26)
  den(28) = den(8)*den(27)
  den(30) = den(8)*den(29)
  den(31) = den(9)*den(30)
  den(32) = den(11)*den(26)
  den(33) = den(8)*den(32)
  den(35) = den(8)*den(34)
  den(36) = den(11)*den(35)
  den(37) = den(6)*den(26)
  den(38) = den(8)*den(37)
  den(40) = den(15)*den(39)
  den(41) = den(14)*den(40)
  den(43) = den(14)*den(42)
  den(44) = den(15)*den(43)
  den(46) = den(15)*den(45)
  den(47) = den(17)*den(46)
  den(48) = den(17)*den(42)
  den(49) = den(15)*den(48)
  den(50) = den(6)*den(42)
  den(51) = den(15)*den(50)
  den(52) = den(11)*den(39)
  den(53) = den(14)*den(52)
  den(54) = den(14)*den(34)
  den(55) = den(11)*den(54)
  den(56) = den(4)*den(39)
  den(57) = den(14)*den(56)
  den(58) = den(9)*den(45)
  den(59) = den(17)*den(58)
  den(60) = den(17)*den(29)
  den(61) = den(9)*den(60)
  den(62) = den(4)*den(29)
  den(63) = den(9)*den(62)
  den(64) = den(2)*den(45)
  den(65) = den(17)*den(64)
  den(66) = den(2)*den(34)
  den(67) = den(11)*den(66)
  den(69) = den(2)*den(68)
  den(70) = den(4)*den(68)
  den(71) = den(6)*den(68)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(66)

  A(1) = cont_SS(wf(:,2),wf(:,4)) * den(3)
  A(2) = cont_SS(wf(:,5),wf(:,6)) * den(5)
  A(3) = cont_SS(wf(:,7),wf(:,8)) * den(7)
  A(4) = cont_QA(wf(:,12),wf(:,13)) * den(10)
  A(5) = cont_QA(wf(:,15),wf(:,16)) * den(12)
  A(6) = cont_SS(wf(:,7),wf(:,17)) * den(13)
  A(7) = cont_QA(wf(:,21),wf(:,22)) * den(16)
  A(8) = cont_QA(wf(:,21),wf(:,25)) * den(18)
  A(9) = cont_SS(wf(:,7),wf(:,26)) * den(19)
  A(10) = cont_QA(wf(:,15),wf(:,27)) * den(20)
  A(11) = cont_SS(wf(:,5),wf(:,28)) * den(21)
  A(12) = cont_QA(wf(:,12),wf(:,29)) * den(22)
  A(13) = cont_SS(wf(:,5),wf(:,30)) * den(23)
  A(14) = cont_SS(wf(:,2),wf(:,31)) * den(24)
  A(15) = cont_SS(wf(:,2),wf(:,32)) * den(25)

  A(16) = cont_QA(wf(:,12),wf(:,33)) * den(10)
  A(17) = cont_QA(wf(:,15),wf(:,34)) * den(12)
  A(18) = cont_QA(wf(:,21),wf(:,35)) * den(16)
  A(19) = cont_QA(wf(:,21),wf(:,36)) * den(18)
  A(20) = cont_QA(wf(:,15),wf(:,37)) * den(20)
  A(21) = cont_QA(wf(:,12),wf(:,38)) * den(22)
  A(22) = cont_SS(wf(:,7),wf(:,39)) * den(13)
  A(23) = cont_QA(wf(:,16),wf(:,41)) * den(12)
  A(24) = cont_QA(wf(:,13),wf(:,43)) * den(10)
  A(25) = cont_SS(wf(:,5),wf(:,44)) * den(21)
  A(26) = cont_QA(wf(:,27),wf(:,41)) * den(20)
  A(27) = cont_SS(wf(:,2),wf(:,45)) * den(24)
  A(28) = cont_SS(wf(:,2),wf(:,46)) * den(25)
  A(29) = cont_QA(wf(:,29),wf(:,43)) * den(22)
  A(30) = cont_SS(wf(:,5),wf(:,47)) * den(23)
  A(31) = cont_QA(wf(:,22),wf(:,49)) * den(16)
  A(32) = cont_QA(wf(:,25),wf(:,49)) * den(18)
  A(33) = cont_SS(wf(:,7),wf(:,50)) * den(19)
  A(34) = cont_SS(wf(:,7),wf(:,51)) * den(19)
  A(35) = cont_QA(wf(:,21),wf(:,54)) * den(18)
  A(36) = cont_QA(wf(:,21),wf(:,57)) * den(16)
  A(37) = cont_SS(wf(:,5),wf(:,58)) * den(23)
  A(38) = cont_QA(wf(:,12),wf(:,59)) * den(22)
  A(39) = cont_SS(wf(:,2),wf(:,60)) * den(25)
  A(40) = cont_SS(wf(:,2),wf(:,61)) * den(24)
  A(41) = cont_QA(wf(:,15),wf(:,62)) * den(20)
  A(42) = cont_SS(wf(:,5),wf(:,63)) * den(21)
  A(43) = cont_QA(wf(:,12),wf(:,66)) * den(10)
  A(44) = cont_QA(wf(:,15),wf(:,67)) * den(12)
  A(45) = cont_SS(wf(:,7),wf(:,68)) * den(13)
  A(46) = cont_SS(wf(:,2),wf(:,71)) * den(3)
  A(47) = cont_SS(wf(:,5),wf(:,72)) * den(5)
  A(48) = cont_SS(wf(:,7),wf(:,73)) * den(7)
  A(49) = cont_QA(wf(:,75),wf(:,76)) * den(28)
  A(50) = cont_QA(wf(:,77),wf(:,78)) * den(31)
  A(51) = cont_QA(wf(:,75),wf(:,80)) * den(33)
  A(52) = cont_QA(wf(:,81),wf(:,82)) * den(36)
  A(53) = cont_QA(wf(:,75),wf(:,84)) * den(38)
  A(54) = cont_QA(wf(:,86),wf(:,87)) * den(41)
  A(55) = cont_QA(wf(:,88),wf(:,89)) * den(44)
  A(56) = cont_QA(wf(:,91),wf(:,92)) * den(47)
  A(57) = cont_QA(wf(:,88),wf(:,93)) * den(49)
  A(58) = cont_QA(wf(:,88),wf(:,95)) * den(51)
  A(59) = cont_QA(wf(:,86),wf(:,97)) * den(53)
  A(60) = cont_QA(wf(:,81),wf(:,98)) * den(55)
  A(61) = cont_QA(wf(:,86),wf(:,100)) * den(57)
  A(62) = cont_QA(wf(:,91),wf(:,102)) * den(59)
  A(63) = cont_QA(wf(:,77),wf(:,103)) * den(61)
  A(64) = cont_QA(wf(:,77),wf(:,105)) * den(63)
  A(65) = cont_QA(wf(:,91),wf(:,107)) * den(65)
  A(66) = cont_QA(wf(:,81),wf(:,109)) * den(67)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(66)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(4)-A(5)-A(7)-A(8)-A(10)-A(12))*f(1)+(A(6)+A(9)+A(11)+A(13)+A(14)+A(15))*f(4)+(A(1)+A(2)+A(3))*f(8)

  M2(1) = (A(49)+A(50)+A(51)+A(52)+A(54)+A(55)+A(56)+A(57)+A(59)+A(60)+A(62)+A(63))*f(2)+(-A(16)-A(17)-A(18)-A(19)-A(20)-A(21) &
       -A(23)-A(24)-A(26)-A(29)-A(31)-A(32)-A(35)-A(36)-A(38)-A(41)-A(43)-A(44))*f(3)+(-A(53)-A(58)-A(61)-A(64)-A(65)-A(66))*f(5) &
       +(A(22)+A(25)+A(27)+A(34)+A(37)+A(39))*f(6)+(A(28)+A(30)+A(33)+A(40)+A(42)+A(45))*f(7)+(A(46)+A(47)+A(48))*f(9)

end subroutine colourvectors

end module ol_loop_ppvvv_bbxzzz_1_/**/REALKIND
