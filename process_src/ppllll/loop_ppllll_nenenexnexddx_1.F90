
module ol_colourmatrix_ppllll_nenenexnexddx_1_/**/REALKIND
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
end module ol_colourmatrix_ppllll_nenenexnexddx_1_/**/REALKIND



module ol_forced_parameters_ppllll_nenenexnexddx_1_/**/REALKIND
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


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllll_nenenexnexddx_1_/**/REALKIND

module ol_loop_ppllll_nenenexnexddx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(4), c(1)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:84)
  ! denominators
  complex(REALKIND), save :: den(68)
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
    f(3) = CI*countertermnorm*ctVqq*eQED**4*gQCD**2
    f(4) = eQED**4*gQCD**2*integralnorm*SwB

  c = [ 4*f(4) ]
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
  complex(REALKIND) :: A(32)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,-3),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,5),MZ,1_intkind1,wf(:,3))
  call prop_W_W(wf(:,2),Q(:,10),MZ,1_intkind1,wf(:,4))
  call vert_ZQ_A(gZd,wf(:,3),wf(:,-4),wf(:,5))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,6))
  call prop_Q_A(wf(:,5),Q(:,21),ZERO,0_intkind1,wf(:,7))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,8))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,3),wf(:,9))
  call prop_Q_A(wf(:,8),Q(:,26),ZERO,0_intkind1,wf(:,10))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,-5),wf(:,11))
  call prop_W_W(wf(:,11),Q(:,48),MZ,1_intkind1,wf(:,12))
  call vert_ZQ_A(gZn,wf(:,3),wf(:,-1),wf(:,13))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,12),wf(:,14))
  call prop_Q_A(wf(:,13),Q(:,7),ZERO,0_intkind1,wf(:,15))
  call vert_ZQ_A(gZn,wf(:,12),wf(:,-1),wf(:,16))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,3),wf(:,17))
  call prop_Q_A(wf(:,16),Q(:,50),ZERO,0_intkind1,wf(:,18))
  call vert_QA_Z(gZn,wf(:,0),wf(:,-3),wf(:,19))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,-2),wf(:,20))
  call prop_W_W(wf(:,19),Q(:,9),MZ,1_intkind1,wf(:,21))
  call prop_W_W(wf(:,20),Q(:,6),MZ,1_intkind1,wf(:,22))
  call vert_ZQ_A(gZd,wf(:,21),wf(:,-4),wf(:,23))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,22),wf(:,24))
  call prop_Q_A(wf(:,23),Q(:,25),ZERO,0_intkind1,wf(:,25))
  call vert_ZQ_A(gZd,wf(:,22),wf(:,-4),wf(:,26))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,21),wf(:,27))
  call prop_Q_A(wf(:,26),Q(:,22),ZERO,0_intkind1,wf(:,28))
  call vert_ZQ_A(gZn,wf(:,22),wf(:,0),wf(:,29))
  call prop_Q_A(wf(:,29),Q(:,7),ZERO,0_intkind1,wf(:,30))
  call vert_ZQ_A(gZn,wf(:,12),wf(:,0),wf(:,31))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,22),wf(:,32))
  call prop_Q_A(wf(:,31),Q(:,49),ZERO,0_intkind1,wf(:,33))
  call vert_ZQ_A(gZn,wf(:,21),wf(:,-1),wf(:,34))
  call vert_AZ_Q(gZn,wf(:,-2),wf(:,12),wf(:,35))
  call prop_Q_A(wf(:,34),Q(:,11),ZERO,0_intkind1,wf(:,36))
  call vert_AZ_Q(gZn,wf(:,-2),wf(:,21),wf(:,37))
  call vert_ZQ_A(gZn,wf(:,4),wf(:,0),wf(:,38))
  call prop_Q_A(wf(:,38),Q(:,11),ZERO,0_intkind1,wf(:,39))
  call vert_AZ_Q(gZn,wf(:,-2),wf(:,4),wf(:,40))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,41))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,3),wf(:,42))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,43))
  call prop_A_Q(wf(:,9),Q(:,37),ZERO,0_intkind1,wf(:,44))
  call counter_ZQ_A(gZd,wf(:,3),wf(:,-4),wf(:,45))
  call prop_A_Q(wf(:,6),Q(:,42),ZERO,0_intkind1,wf(:,46))
  call counter_QA_Z(gZd,wf(:,-4),wf(:,-5),wf(:,47))
  call prop_W_W(wf(:,47),Q(:,48),MZ,1_intkind1,wf(:,48))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,48),wf(:,49))
  call vert_ZQ_A(gZn,wf(:,48),wf(:,-1),wf(:,50))
  call prop_Q_A(wf(:,50),Q(:,50),ZERO,0_intkind1,wf(:,51))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,22),wf(:,52))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,21),wf(:,53))
  call counter_ZQ_A(gZd,wf(:,22),wf(:,-4),wf(:,54))
  call prop_A_Q(wf(:,27),Q(:,41),ZERO,0_intkind1,wf(:,55))
  call counter_ZQ_A(gZd,wf(:,21),wf(:,-4),wf(:,56))
  call prop_A_Q(wf(:,24),Q(:,38),ZERO,0_intkind1,wf(:,57))
  call vert_ZQ_A(gZn,wf(:,48),wf(:,0),wf(:,58))
  call prop_Q_A(wf(:,58),Q(:,49),ZERO,0_intkind1,wf(:,59))
  call vert_AZ_Q(gZn,wf(:,-2),wf(:,48),wf(:,60))
  call counter_Q_A(ctqq,wf(:,7),Q(:,21),wf(:,61))
  call counter_Q_A(ctqq,wf(:,10),Q(:,26),wf(:,62))
  call counter_Q_A(ctqq,wf(:,25),Q(:,25),wf(:,63))
  call counter_Q_A(ctqq,wf(:,28),Q(:,22),wf(:,64))
  call vert_QA_Z(gZn,wf(:,15),wf(:,-3),wf(:,65))
  call prop_W_W(wf(:,65),Q(:,15),MZ,1_intkind1,wf(:,66))
  call prop_A_Q(wf(:,17),Q(:,13),ZERO,0_intkind1,wf(:,67))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,67),wf(:,68))
  call prop_W_W(wf(:,68),Q(:,15),MZ,1_intkind1,wf(:,69))
  call vert_QA_Z(gZn,wf(:,30),wf(:,-3),wf(:,70))
  call prop_W_W(wf(:,70),Q(:,15),MZ,1_intkind1,wf(:,71))
  call prop_A_Q(wf(:,32),Q(:,14),ZERO,0_intkind1,wf(:,72))
  call vert_QA_Z(gZn,wf(:,0),wf(:,72),wf(:,73))
  call prop_W_W(wf(:,73),Q(:,15),MZ,1_intkind1,wf(:,74))
  call vert_QA_Z(gZn,wf(:,36),wf(:,-2),wf(:,75))
  call prop_W_W(wf(:,75),Q(:,15),MZ,1_intkind1,wf(:,76))
  call prop_A_Q(wf(:,37),Q(:,13),ZERO,0_intkind1,wf(:,77))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,77),wf(:,78))
  call prop_W_W(wf(:,78),Q(:,15),MZ,1_intkind1,wf(:,79))
  call vert_QA_Z(gZn,wf(:,39),wf(:,-2),wf(:,80))
  call prop_W_W(wf(:,80),Q(:,15),MZ,1_intkind1,wf(:,81))
  call prop_A_Q(wf(:,40),Q(:,14),ZERO,0_intkind1,wf(:,82))
  call vert_QA_Z(gZn,wf(:,0),wf(:,82),wf(:,83))
  call prop_W_W(wf(:,83),Q(:,15),MZ,1_intkind1,wf(:,84))

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
  den(3) = 1 / (Q(5,21))
  den(6) = 1 / (Q(5,26))
  den(9) = 1 / (Q(5,48) - MZ2)
  den(10) = 1 / (Q(5,7))
  den(13) = 1 / (Q(5,50))
  den(16) = 1 / (Q(5,9) - MZ2)
  den(17) = 1 / (Q(5,6) - MZ2)
  den(18) = 1 / (Q(5,25))
  den(21) = 1 / (Q(5,22))
  den(26) = 1 / (Q(5,49))
  den(29) = 1 / (Q(5,11))
  den(36) = 1 / (Q(5,37))
  den(39) = 1 / (Q(5,42))
  den(42) = 1 / (Q(5,41))
  den(45) = 1 / (Q(5,38))
  den(52) = 1 / (Q(5,15) - MZ2)
  den(54) = 1 / (Q(5,13))
  den(58) = 1 / (Q(5,14))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(1)*den(7)
  den(11) = den(1)*den(10)
  den(12) = den(9)*den(11)
  den(14) = den(9)*den(13)
  den(15) = den(1)*den(14)
  den(19) = den(16)*den(18)
  den(20) = den(17)*den(19)
  den(22) = den(17)*den(21)
  den(23) = den(16)*den(22)
  den(24) = den(10)*den(17)
  den(25) = den(9)*den(24)
  den(27) = den(9)*den(26)
  den(28) = den(17)*den(27)
  den(30) = den(16)*den(29)
  den(31) = den(9)*den(30)
  den(32) = den(14)*den(16)
  den(33) = den(2)*den(29)
  den(34) = den(9)*den(33)
  den(35) = den(2)*den(27)
  den(37) = den(1)*den(36)
  den(38) = den(2)*den(37)
  den(40) = den(2)*den(39)
  den(41) = den(1)*den(40)
  den(43) = den(16)*den(42)
  den(44) = den(17)*den(43)
  den(46) = den(17)*den(45)
  den(47) = den(16)*den(46)
  den(48) = den(4)*den(40)
  den(49) = den(7)*den(37)
  den(50) = den(19)*den(46)
  den(51) = den(22)*den(43)
  den(53) = den(11)*den(52)
  den(55) = den(1)*den(54)
  den(56) = den(52)*den(55)
  den(57) = den(24)*den(52)
  den(59) = den(17)*den(58)
  den(60) = den(52)*den(59)
  den(61) = den(30)*den(52)
  den(62) = den(16)*den(54)
  den(63) = den(52)*den(62)
  den(64) = den(33)*den(52)
  den(65) = den(2)*den(58)
  den(66) = den(52)*den(65)
  den(67) = den(1)*den(2)
  den(68) = den(16)*den(17)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(32)

  A(1) = cont_QA(wf(:,6),wf(:,7)) * den(5)
  A(2) = cont_QA(wf(:,9),wf(:,10)) * den(8)
  A(3) = cont_QA(wf(:,14),wf(:,15)) * den(12)
  A(4) = cont_QA(wf(:,17),wf(:,18)) * den(15)
  A(5) = cont_QA(wf(:,24),wf(:,25)) * den(20)
  A(6) = cont_QA(wf(:,27),wf(:,28)) * den(23)
  A(7) = cont_QA(wf(:,14),wf(:,30)) * den(25)
  A(8) = cont_QA(wf(:,32),wf(:,33)) * den(28)
  A(9) = cont_QA(wf(:,35),wf(:,36)) * den(31)
  A(10) = cont_QA(wf(:,18),wf(:,37)) * den(32)
  A(11) = cont_QA(wf(:,35),wf(:,39)) * den(34)
  A(12) = cont_QA(wf(:,33),wf(:,40)) * den(35)

  A(13) = cont_QA(wf(:,7),wf(:,41)) * den(5)
  A(14) = cont_QA(wf(:,10),wf(:,42)) * den(8)
  A(15) = cont_QA(wf(:,43),wf(:,44)) * den(38)
  A(16) = cont_QA(wf(:,45),wf(:,46)) * den(41)
  A(17) = cont_QA(wf(:,15),wf(:,49)) * den(12)
  A(18) = cont_QA(wf(:,17),wf(:,51)) * den(15)
  A(19) = cont_QA(wf(:,25),wf(:,52)) * den(20)
  A(20) = cont_QA(wf(:,28),wf(:,53)) * den(23)
  A(21) = cont_QA(wf(:,54),wf(:,55)) * den(44)
  A(22) = cont_QA(wf(:,56),wf(:,57)) * den(47)
  A(23) = cont_QA(wf(:,30),wf(:,49)) * den(25)
  A(24) = cont_QA(wf(:,32),wf(:,59)) * den(28)
  A(25) = cont_QA(wf(:,36),wf(:,60)) * den(31)
  A(26) = cont_QA(wf(:,37),wf(:,51)) * den(32)
  A(27) = cont_QA(wf(:,39),wf(:,60)) * den(34)
  A(28) = cont_QA(wf(:,40),wf(:,59)) * den(35)
  A(29) = cont_QA(wf(:,46),wf(:,61)) * den(48)
  A(30) = cont_QA(wf(:,44),wf(:,62)) * den(49)
  A(31) = cont_QA(wf(:,57),wf(:,63)) * den(50)
  A(32) = cont_QA(wf(:,55),wf(:,64)) * den(51)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(32)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(1)-A(2)-A(3)-A(4)+A(5)+A(6)+A(7)+A(8)+A(9)+A(10)-A(11)-A(12))*f(1)

  M2(1) = (A(29)+A(30)-A(31)-A(32))*f(2)+(-A(13)-A(14)-A(15)-A(16)-A(17)-A(18)+A(19)+A(20)+A(21)+A(22)+A(23)+A(24)+A(25)+A(26) &
       -A(27)-A(28))*f(3)

end subroutine colourvectors

end module ol_loop_ppllll_nenenexnexddx_1_/**/REALKIND
