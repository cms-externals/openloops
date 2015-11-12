
module ol_colourmatrix_ppllll_nenenexnexbbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(23,1), K2(1,1), KL(1,1)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  3]
  K1( 2,:) = [  0]
  K1( 3,:) = [  0]
  K1( 4,:) = [  0]
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
  K1(16,:) = [  4]
  K1(17,:) = [  0]
  K1(18,:) = [  0]
  K1(19,:) = [  0]
  K1(20,:) = [  0]
  K1(21,:) = [ -4]
  K1(22,:) = [  4]
  K1(23,:) = [  0]

  K2(1,:) = [ 3]

  KL(1,:) = [ 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllll_nenenexnexbbx_1_/**/REALKIND



module ol_forced_parameters_ppllll_nenenexnexbbx_1_/**/REALKIND
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
  if (ME /= 0) write(*,101) 'ME = 0'
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
end module ol_forced_parameters_ppllll_nenenexnexbbx_1_/**/REALKIND

module ol_loop_ppllll_nenenexnexbbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(7), c(2)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:88)
  ! denominators
  complex(REALKIND), save :: den(74)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,64)
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
    f(1) = CI*eQED**4
    f(2) = CI*countertermnorm*eQED**4*gQCD**2
    f(3) = CI*countertermnorm*ctVbb*eQED**4*gQCD**2
    f(4) = (CI*eQED**4*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(5) = (CI*countertermnorm*ctSbb*eQED**4*gQCD**2*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(6) = eQED**4*gQCD**2*integralnorm*SwB
    f(7) = (eQED**4*gQCD**2*integralnorm*MB*SwB)/(cw**2*sw**2*2._/**/REALKIND)

  c = [ 4*f(6), 4*f(7) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(36)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rMB, H(5), wf(:,-4))
  call wf_A(P(:,6), rMB, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,-3),wf(:,2))
  call vert_AQ_S(gH,wf(:,-5),wf(:,-4),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,5),MZ,1_intkind1,wf(:,4))
  call prop_W_W(wf(:,2),Q(:,10),MZ,1_intkind1,wf(:,5))
  call vert_VV_S(wf(:,4),wf(:,5),wf(:,6))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,7))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,5),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,21),MB,1_intkind1,wf(:,9))
  call vert_ZQ_A(gZd,wf(:,5),wf(:,-4),wf(:,10))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,11))
  call prop_Q_A(wf(:,10),Q(:,26),MB,1_intkind1,wf(:,12))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,-5),wf(:,13))
  call prop_W_W(wf(:,13),Q(:,48),MZ,1_intkind1,wf(:,14))
  call vert_ZQ_A(gZn,wf(:,4),wf(:,-1),wf(:,15))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,14),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,7),ZERO,0_intkind1,wf(:,17))
  call vert_ZQ_A(gZn,wf(:,14),wf(:,-1),wf(:,18))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,4),wf(:,19))
  call prop_Q_A(wf(:,18),Q(:,50),ZERO,0_intkind1,wf(:,20))
  call vert_QA_Z(gZn,wf(:,0),wf(:,-3),wf(:,21))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,-2),wf(:,22))
  call prop_W_W(wf(:,21),Q(:,9),MZ,1_intkind1,wf(:,23))
  call prop_W_W(wf(:,22),Q(:,6),MZ,1_intkind1,wf(:,24))
  call vert_VV_S(wf(:,24),wf(:,23),wf(:,25))
  call vert_ZQ_A(gZd,wf(:,23),wf(:,-4),wf(:,26))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,24),wf(:,27))
  call prop_Q_A(wf(:,26),Q(:,25),MB,1_intkind1,wf(:,28))
  call vert_ZQ_A(gZd,wf(:,24),wf(:,-4),wf(:,29))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,23),wf(:,30))
  call prop_Q_A(wf(:,29),Q(:,22),MB,1_intkind1,wf(:,31))
  call vert_ZQ_A(gZn,wf(:,24),wf(:,0),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,7),ZERO,0_intkind1,wf(:,33))
  call vert_ZQ_A(gZn,wf(:,14),wf(:,0),wf(:,34))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,24),wf(:,35))
  call prop_Q_A(wf(:,34),Q(:,49),ZERO,0_intkind1,wf(:,36))
  call vert_ZQ_A(gZn,wf(:,23),wf(:,-1),wf(:,37))
  call vert_AZ_Q(gZn,wf(:,-2),wf(:,14),wf(:,38))
  call prop_Q_A(wf(:,37),Q(:,11),ZERO,0_intkind1,wf(:,39))
  call vert_AZ_Q(gZn,wf(:,-2),wf(:,23),wf(:,40))
  call vert_ZQ_A(gZn,wf(:,5),wf(:,0),wf(:,41))
  call prop_Q_A(wf(:,41),Q(:,11),ZERO,0_intkind1,wf(:,42))
  call vert_AZ_Q(gZn,wf(:,-2),wf(:,5),wf(:,43))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,5),wf(:,44))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,45))
  call counter_ZQ_A(gZd,wf(:,5),wf(:,-4),wf(:,46))
  call prop_A_Q(wf(:,11),Q(:,37),MB,1_intkind1,wf(:,47))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,48))
  call prop_A_Q(wf(:,8),Q(:,42),MB,1_intkind1,wf(:,49))
  call counter_AQ_S(gH,wf(:,-5),wf(:,-4),wf(:,50))
  call counter_QA_Z(gZd,wf(:,-4),wf(:,-5),wf(:,51))
  call prop_W_W(wf(:,51),Q(:,48),MZ,1_intkind1,wf(:,52))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,52),wf(:,53))
  call vert_ZQ_A(gZn,wf(:,52),wf(:,-1),wf(:,54))
  call prop_Q_A(wf(:,54),Q(:,50),ZERO,0_intkind1,wf(:,55))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,24),wf(:,56))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,23),wf(:,57))
  call counter_ZQ_A(gZd,wf(:,24),wf(:,-4),wf(:,58))
  call prop_A_Q(wf(:,30),Q(:,41),MB,1_intkind1,wf(:,59))
  call counter_ZQ_A(gZd,wf(:,23),wf(:,-4),wf(:,60))
  call prop_A_Q(wf(:,27),Q(:,38),MB,1_intkind1,wf(:,61))
  call vert_ZQ_A(gZn,wf(:,52),wf(:,0),wf(:,62))
  call prop_Q_A(wf(:,62),Q(:,49),ZERO,0_intkind1,wf(:,63))
  call vert_AZ_Q(gZn,wf(:,-2),wf(:,52),wf(:,64))
  call counter_Q_A(ctbb,wf(:,9),Q(:,21),wf(:,65))
  call counter_Q_A(ctbb,wf(:,12),Q(:,26),wf(:,66))
  call counter_Q_A(ctbb,wf(:,28),Q(:,25),wf(:,67))
  call counter_Q_A(ctbb,wf(:,31),Q(:,22),wf(:,68))
  call vert_QA_Z(gZn,wf(:,17),wf(:,-3),wf(:,69))
  call prop_W_W(wf(:,69),Q(:,15),MZ,1_intkind1,wf(:,70))
  call prop_A_Q(wf(:,19),Q(:,13),ZERO,0_intkind1,wf(:,71))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,71),wf(:,72))
  call prop_W_W(wf(:,72),Q(:,15),MZ,1_intkind1,wf(:,73))
  call vert_QA_Z(gZn,wf(:,33),wf(:,-3),wf(:,74))
  call prop_W_W(wf(:,74),Q(:,15),MZ,1_intkind1,wf(:,75))
  call prop_A_Q(wf(:,35),Q(:,14),ZERO,0_intkind1,wf(:,76))
  call vert_QA_Z(gZn,wf(:,0),wf(:,76),wf(:,77))
  call prop_W_W(wf(:,77),Q(:,15),MZ,1_intkind1,wf(:,78))
  call vert_QA_Z(gZn,wf(:,39),wf(:,-2),wf(:,79))
  call prop_W_W(wf(:,79),Q(:,15),MZ,1_intkind1,wf(:,80))
  call prop_A_Q(wf(:,40),Q(:,13),ZERO,0_intkind1,wf(:,81))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,81),wf(:,82))
  call prop_W_W(wf(:,82),Q(:,15),MZ,1_intkind1,wf(:,83))
  call vert_QA_Z(gZn,wf(:,42),wf(:,-2),wf(:,84))
  call prop_W_W(wf(:,84),Q(:,15),MZ,1_intkind1,wf(:,85))
  call prop_A_Q(wf(:,43),Q(:,14),ZERO,0_intkind1,wf(:,86))
  call vert_QA_Z(gZn,wf(:,0),wf(:,86),wf(:,87))
  call prop_W_W(wf(:,87),Q(:,15),MZ,1_intkind1,wf(:,88))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5) - MZ2)
  den(2) = 1 / (Q(5,10) - MZ2)
  den(3) = 1 / (Q(5,48) - MH2)
  den(6) = 1 / (Q(5,21) - MB2)
  den(9) = 1 / (Q(5,26) - MB2)
  den(12) = 1 / (Q(5,48) - MZ2)
  den(13) = 1 / (Q(5,7))
  den(16) = 1 / (Q(5,50))
  den(19) = 1 / (Q(5,9) - MZ2)
  den(20) = 1 / (Q(5,6) - MZ2)
  den(23) = 1 / (Q(5,25) - MB2)
  den(26) = 1 / (Q(5,22) - MB2)
  den(31) = 1 / (Q(5,49))
  den(34) = 1 / (Q(5,11))
  den(41) = 1 / (Q(5,37) - MB2)
  den(44) = 1 / (Q(5,42) - MB2)
  den(47) = 1 / (Q(5,41) - MB2)
  den(50) = 1 / (Q(5,38) - MB2)
  den(57) = 1 / (Q(5,15) - MH2)
  den(59) = 1 / (Q(5,15) - MZ2)
  den(61) = 1 / (Q(5,13))
  den(66) = 1 / (Q(5,14))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(14) = den(1)*den(13)
  den(15) = den(12)*den(14)
  den(17) = den(12)*den(16)
  den(18) = den(1)*den(17)
  den(21) = den(19)*den(20)
  den(22) = den(3)*den(21)
  den(24) = den(19)*den(23)
  den(25) = den(20)*den(24)
  den(27) = den(20)*den(26)
  den(28) = den(19)*den(27)
  den(29) = den(13)*den(20)
  den(30) = den(12)*den(29)
  den(32) = den(12)*den(31)
  den(33) = den(20)*den(32)
  den(35) = den(19)*den(34)
  den(36) = den(12)*den(35)
  den(37) = den(17)*den(19)
  den(38) = den(2)*den(34)
  den(39) = den(12)*den(38)
  den(40) = den(2)*den(32)
  den(42) = den(1)*den(41)
  den(43) = den(2)*den(42)
  den(45) = den(2)*den(44)
  den(46) = den(1)*den(45)
  den(48) = den(19)*den(47)
  den(49) = den(20)*den(48)
  den(51) = den(20)*den(50)
  den(52) = den(19)*den(51)
  den(53) = den(7)*den(45)
  den(54) = den(10)*den(42)
  den(55) = den(24)*den(51)
  den(56) = den(27)*den(48)
  den(58) = den(4)*den(57)
  den(60) = den(14)*den(59)
  den(62) = den(1)*den(61)
  den(63) = den(59)*den(62)
  den(64) = den(21)*den(57)
  den(65) = den(29)*den(59)
  den(67) = den(20)*den(66)
  den(68) = den(59)*den(67)
  den(69) = den(35)*den(59)
  den(70) = den(19)*den(61)
  den(71) = den(59)*den(70)
  den(72) = den(38)*den(59)
  den(73) = den(2)*den(66)
  den(74) = den(59)*den(73)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(36)

  A(1) = cont_SS(wf(:,3),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,11),wf(:,12)) * den(11)
  A(4) = cont_QA(wf(:,16),wf(:,17)) * den(15)
  A(5) = cont_QA(wf(:,19),wf(:,20)) * den(18)
  A(6) = cont_SS(wf(:,3),wf(:,25)) * den(22)
  A(7) = cont_QA(wf(:,27),wf(:,28)) * den(25)
  A(8) = cont_QA(wf(:,30),wf(:,31)) * den(28)
  A(9) = cont_QA(wf(:,16),wf(:,33)) * den(30)
  A(10) = cont_QA(wf(:,35),wf(:,36)) * den(33)
  A(11) = cont_QA(wf(:,38),wf(:,39)) * den(36)
  A(12) = cont_QA(wf(:,20),wf(:,40)) * den(37)
  A(13) = cont_QA(wf(:,38),wf(:,42)) * den(39)
  A(14) = cont_QA(wf(:,36),wf(:,43)) * den(40)

  A(15) = cont_QA(wf(:,9),wf(:,44)) * den(8)
  A(16) = cont_QA(wf(:,12),wf(:,45)) * den(11)
  A(17) = cont_QA(wf(:,46),wf(:,47)) * den(43)
  A(18) = cont_QA(wf(:,48),wf(:,49)) * den(46)
  A(19) = cont_SS(wf(:,6),wf(:,50)) * den(5)
  A(20) = cont_QA(wf(:,17),wf(:,53)) * den(15)
  A(21) = cont_QA(wf(:,19),wf(:,55)) * den(18)
  A(22) = cont_QA(wf(:,28),wf(:,56)) * den(25)
  A(23) = cont_QA(wf(:,31),wf(:,57)) * den(28)
  A(24) = cont_QA(wf(:,58),wf(:,59)) * den(49)
  A(25) = cont_QA(wf(:,60),wf(:,61)) * den(52)
  A(26) = cont_SS(wf(:,25),wf(:,50)) * den(22)
  A(27) = cont_QA(wf(:,33),wf(:,53)) * den(30)
  A(28) = cont_QA(wf(:,35),wf(:,63)) * den(33)
  A(29) = cont_QA(wf(:,39),wf(:,64)) * den(36)
  A(30) = cont_QA(wf(:,40),wf(:,55)) * den(37)
  A(31) = cont_QA(wf(:,42),wf(:,64)) * den(39)
  A(32) = cont_QA(wf(:,43),wf(:,63)) * den(40)
  A(33) = cont_QA(wf(:,49),wf(:,65)) * den(53)
  A(34) = cont_QA(wf(:,47),wf(:,66)) * den(54)
  A(35) = cont_QA(wf(:,61),wf(:,67)) * den(55)
  A(36) = cont_QA(wf(:,59),wf(:,68)) * den(56)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(36)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(2)-A(3)-A(4)-A(5)+A(7)+A(8)+A(9)+A(10)+A(11)+A(12)-A(13)-A(14))*f(1)+(A(1)-A(6))*f(4)

  M2(1) = (A(33)+A(34)-A(35)-A(36))*f(2)+(-A(15)-A(16)-A(17)-A(18)-A(20)-A(21)+A(22)+A(23)+A(24)+A(25)+A(27)+A(28)+A(29)+A(30) &
       -A(31)-A(32))*f(3)+(A(19)-A(26))*f(5)

end subroutine colourvectors

end module ol_loop_ppllll_nenenexnexbbx_1_/**/REALKIND
