
module ol_colourmatrix_ppllaj_nenexddxag_1_/**/REALKIND
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

  K1( 1,:) = [  12]
  K1( 2,:) = [   0]
  K1( 3,:) = [   0]
  K1( 4,:) = [   0]
  K1( 5,:) = [   0]
  K1( 6,:) = [   0]
  K1( 7,:) = [  16]
  K1( 8,:) = [   0]
  K1( 9,:) = [   0]
  K1(10,:) = [   2]
  K1(11,:) = [  16]
  K1(12,:) = [   0]
  K1(13,:) = [   0]
  K1(14,:) = [   0]
  K1(15,:) = [   0]
  K1(16,:) = [   0]
  K1(17,:) = [   0]
  K1(18,:) = [   0]
  K1(19,:) = [ -18]
  K1(20,:) = [ -18]
  K1(21,:) = [   0]
  K1(22,:) = [  36]
  K1(23,:) = [   0]
  K1 = (1._/**/REALKIND / 3) * K1

  K2(1,:) = [ 4]

  KL(1,:) = [ 4]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllaj_nenexddxag_1_/**/REALKIND



module ol_forced_parameters_ppllaj_nenexddxag_1_/**/REALKIND
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
end module ol_forced_parameters_ppllaj_nenexddxag_1_/**/REALKIND

module ol_loop_ppllaj_nenexddxag_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(11), c(6)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:81)
  ! denominators
  complex(REALKIND), save :: den(73)
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
    f( 1) = (CI*eQED**3*gQCD)/3._/**/REALKIND
    f( 2) = (CI*countertermnorm*eQED**3*gQCD**3)/3._/**/REALKIND
    f( 3) = CI*countertermnorm*ctAZGG*eQED**3*gQCD**3
    f( 4) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**3)/3._/**/REALKIND
    f( 5) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**3)/3._/**/REALKIND
    f( 6) = (countertermnorm*ctZGG*eQED**3*gQCD**3)/3._/**/REALKIND
    f( 7) = (CI*eQED**3*gQCD**3*integralnorm*SwB)/3._/**/REALKIND
    f( 8) = (eQED**3*gQCD**3*integralnorm*SwB)/3._/**/REALKIND
    f( 9) = (eQED**3*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(10) = (2*eQED**3*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(11) = (4*eQED**3*gQCD**3*integralnorm*SwF)/3._/**/REALKIND

  c = [ 9*CI*f(7), f(8), 8*f(8), 3*f(9), 3*f(10), 3*f(11) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(39)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_VQ_A(wf(:,-4),wf(:,-2),wf(:,2))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call prop_Q_A(wf(:,2),Q(:,20),ZERO,0_intkind1,wf(:,5))
  call prop_A_Q(wf(:,3),Q(:,40),ZERO,0_intkind1,wf(:,6))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,5),wf(:,7))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,4),wf(:,8))
  call vert_VQ_A(wf(:,-5),wf(:,5),wf(:,9))
  call prop_A_Q(wf(:,8),Q(:,11),ZERO,0_intkind1,wf(:,10))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,11))
  call vert_AV_Q(wf(:,-3),wf(:,-4),wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,36),ZERO,0_intkind1,wf(:,13))
  call prop_A_Q(wf(:,12),Q(:,24),ZERO,0_intkind1,wf(:,14))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,13),wf(:,15))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,-2),wf(:,16))
  call vert_AV_Q(wf(:,14),wf(:,-5),wf(:,17))
  call prop_Q_A(wf(:,16),Q(:,7),ZERO,0_intkind1,wf(:,18))
  call vert_VQ_A(wf(:,-4),wf(:,13),wf(:,19))
  call vert_AV_Q(wf(:,6),wf(:,-4),wf(:,20))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,21))
  call counter_VVG_G(wf(:,4),wf(:,-4),wf(:,-5),wf(:,22))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,5),wf(:,23))
  call counter_VQ_A(wf(:,-5),wf(:,5),wf(:,24))
  call vert_QA_V(wf(:,5),wf(:,-3),wf(:,25))
  call counter_VG_G(wf(:,4),wf(:,-5),Q(:,32),wf(:,26),Q(:,35))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,13),wf(:,27))
  call counter_AV_Q(wf(:,14),wf(:,-5),wf(:,28))
  call vert_QA_V(wf(:,-2),wf(:,14),wf(:,29))
  call counter_VQ_A(wf(:,-4),wf(:,13),wf(:,30))
  call counter_AV_Q(wf(:,6),wf(:,-4),wf(:,31))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,4),wf(:,32))
  call prop_Q_A(wf(:,9),Q(:,52),ZERO,0_intkind1,wf(:,33))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,34))
  call prop_A_Q(wf(:,34),Q(:,40),ZERO,0_intkind1,wf(:,35))
  call prop_Q_A(wf(:,19),Q(:,52),ZERO,0_intkind1,wf(:,36))
  call vert_AV_Q(wf(:,35),wf(:,-4),wf(:,37))
  call counter_AV_Q(wf(:,-3),wf(:,-4),wf(:,38))
  call prop_A_Q(wf(:,38),Q(:,24),ZERO,0_intkind1,wf(:,39))
  call vert_AV_Q(wf(:,39),wf(:,-5),wf(:,40))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,-2),wf(:,41))
  call prop_A_Q(wf(:,17),Q(:,56),ZERO,0_intkind1,wf(:,42))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,43))
  call prop_Q_A(wf(:,43),Q(:,36),ZERO,0_intkind1,wf(:,44))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,44),wf(:,45))
  call prop_A_Q(wf(:,20),Q(:,56),ZERO,0_intkind1,wf(:,46))
  call vert_VQ_A(wf(:,-4),wf(:,44),wf(:,47))
  call counter_VQ_A(wf(:,-4),wf(:,-2),wf(:,48))
  call prop_Q_A(wf(:,48),Q(:,20),ZERO,0_intkind1,wf(:,49))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,49),wf(:,50))
  call vert_VQ_A(wf(:,-5),wf(:,49),wf(:,51))
  call vert_AZ_Q(gZd,wf(:,6),wf(:,4),wf(:,52))
  call counter_Q_A(ctqq,wf(:,5),Q(:,20),wf(:,53))
  call prop_A_Q(wf(:,52),Q(:,43),ZERO,0_intkind1,wf(:,54))
  call counter_A_Q(ctqq,wf(:,6),Q(:,40),wf(:,55))
  call prop_Q_A(wf(:,7),Q(:,23),ZERO,0_intkind1,wf(:,56))
  call prop_Q_A(wf(:,53),Q(:,20),ZERO,0_intkind1,wf(:,57))
  call vert_VQ_A(wf(:,-5),wf(:,57),wf(:,58))
  call counter_A_Q(ctqq,wf(:,10),Q(:,11),wf(:,59))
  call vert_AZ_Q(gZd,wf(:,14),wf(:,4),wf(:,60))
  call counter_Q_A(ctqq,wf(:,13),Q(:,36),wf(:,61))
  call prop_A_Q(wf(:,60),Q(:,27),ZERO,0_intkind1,wf(:,62))
  call counter_A_Q(ctqq,wf(:,14),Q(:,24),wf(:,63))
  call prop_Q_A(wf(:,15),Q(:,39),ZERO,0_intkind1,wf(:,64))
  call counter_Q_A(ctqq,wf(:,18),Q(:,7),wf(:,65))
  call prop_A_Q(wf(:,63),Q(:,24),ZERO,0_intkind1,wf(:,66))
  call vert_AV_Q(wf(:,66),wf(:,-5),wf(:,67))
  call prop_Q_A(wf(:,61),Q(:,36),ZERO,0_intkind1,wf(:,68))
  call vert_VQ_A(wf(:,-4),wf(:,68),wf(:,69))
  call prop_A_Q(wf(:,55),Q(:,40),ZERO,0_intkind1,wf(:,70))
  call vert_AV_Q(wf(:,70),wf(:,-4),wf(:,71))
  call vert_QA_V(wf(:,18),wf(:,-3),wf(:,72))
  call vert_VQ_A(wf(:,-4),wf(:,18),wf(:,73))
  call prop_Q_A(wf(:,73),Q(:,23),ZERO,0_intkind1,wf(:,74))
  call vert_VQ_A(wf(:,-5),wf(:,18),wf(:,75))
  call prop_Q_A(wf(:,75),Q(:,39),ZERO,0_intkind1,wf(:,76))
  call vert_QA_V(wf(:,-2),wf(:,10),wf(:,77))
  call vert_AV_Q(wf(:,10),wf(:,-4),wf(:,78))
  call prop_A_Q(wf(:,78),Q(:,27),ZERO,0_intkind1,wf(:,79))
  call vert_AV_Q(wf(:,10),wf(:,-5),wf(:,80))
  call prop_A_Q(wf(:,80),Q(:,43),ZERO,0_intkind1,wf(:,81))

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
  den(2) = 1 / (Q(5,20))
  den(3) = 1 / (Q(5,40))
  den(6) = 1 / (Q(5,11))
  den(9) = 1 / (Q(5,36))
  den(10) = 1 / (Q(5,24))
  den(13) = 1 / (Q(5,7))
  den(18) = 1 / (Q(5,12))
  den(20) = 1 / (Q(5,28))
  den(25) = 1 / (Q(5,52))
  den(30) = 1 / (Q(5,56))
  den(36) = 1 / (Q(5,43))
  den(39) = 1 / (Q(5,23))
  den(46) = 1 / (Q(5,27))
  den(49) = 1 / (Q(5,39))
  den(61) = 1 / (Q(5,15))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(11) = den(1)*den(9)
  den(12) = den(10)*den(11)
  den(14) = den(1)*den(13)
  den(15) = den(10)*den(14)
  den(16) = den(7)*den(9)
  den(17) = den(3)*den(14)
  den(19) = den(1)*den(18)
  den(21) = den(2)*den(20)
  den(22) = den(1)*den(21)
  den(23) = den(10)*den(20)
  den(24) = den(1)*den(23)
  den(26) = den(2)*den(25)
  den(27) = den(1)*den(26)
  den(28) = den(9)*den(25)
  den(29) = den(1)*den(28)
  den(31) = den(10)*den(30)
  den(32) = den(1)*den(31)
  den(33) = den(3)*den(30)
  den(34) = den(1)*den(33)
  den(35) = den(1)*den(3)
  den(37) = den(35)*den(36)
  den(38) = den(2)*den(37)
  den(40) = den(4)*den(39)
  den(41) = den(3)*den(40)
  den(42) = den(2)**2
  den(43) = den(7)*den(42)
  den(44) = den(7)*den(26)
  den(45) = den(1)*den(10)
  den(47) = den(45)*den(46)
  den(48) = den(9)*den(47)
  den(50) = den(11)*den(49)
  den(51) = den(10)*den(50)
  den(52) = den(14)*den(31)
  den(53) = den(10)**2
  den(54) = den(14)*den(53)
  den(55) = den(9)**2
  den(56) = den(7)*den(55)
  den(57) = den(7)*den(28)
  den(58) = den(14)*den(33)
  den(59) = den(3)**2
  den(60) = den(14)*den(59)
  den(62) = den(14)*den(61)
  den(63) = den(14)*den(39)
  den(64) = den(14)*den(49)
  den(65) = den(7)*den(61)
  den(66) = den(7)*den(46)
  den(67) = den(7)*den(36)
  den(68) = den(1)*den(2)*den(3)
  den(69) = den(1)*den(9)*den(10)
  den(70) = den(2)*den(67)
  den(71) = den(10)*den(64)
  den(72) = den(9)*den(66)
  den(73) = den(3)*den(63)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(39)

  A(1) = cont_QA(wf(:,6),wf(:,7)) * den(5)
  A(2) = cont_QA(wf(:,9),wf(:,10)) * den(8)
  A(3) = cont_QA(wf(:,14),wf(:,15)) * den(12)
  A(4) = cont_QA(wf(:,17),wf(:,18)) * den(15)
  A(5) = cont_QA(wf(:,10),wf(:,19)) * den(16)
  A(6) = cont_QA(wf(:,18),wf(:,20)) * den(17)

  A(7) = cont_VV(wf(:,21),wf(:,22)) * den(19)
  A(8) = cont_QA(wf(:,6),wf(:,23)) * den(5)
  A(9) = cont_QA(wf(:,10),wf(:,24)) * den(8)
  A(10) = cont_VV(wf(:,25),wf(:,26)) * den(22)
  A(11) = cont_QA(wf(:,14),wf(:,27)) * den(12)
  A(12) = cont_QA(wf(:,18),wf(:,28)) * den(15)
  A(13) = cont_VV(wf(:,26),wf(:,29)) * den(24)
  A(14) = cont_QA(wf(:,10),wf(:,30)) * den(16)
  A(15) = cont_QA(wf(:,18),wf(:,31)) * den(17)
  A(16) = cont_QA(wf(:,32),wf(:,33)) * den(27)
  A(17) = cont_QA(wf(:,7),wf(:,35)) * den(5)
  A(18) = cont_QA(wf(:,32),wf(:,36)) * den(29)
  A(19) = cont_QA(wf(:,18),wf(:,37)) * den(17)
  A(20) = cont_QA(wf(:,15),wf(:,39)) * den(12)
  A(21) = cont_QA(wf(:,18),wf(:,40)) * den(15)
  A(22) = cont_QA(wf(:,41),wf(:,42)) * den(32)
  A(23) = cont_QA(wf(:,14),wf(:,45)) * den(12)
  A(24) = cont_QA(wf(:,41),wf(:,46)) * den(34)
  A(25) = cont_QA(wf(:,10),wf(:,47)) * den(16)
  A(26) = cont_QA(wf(:,6),wf(:,50)) * den(5)
  A(27) = cont_QA(wf(:,10),wf(:,51)) * den(8)
  A(28) = cont_QA(wf(:,53),wf(:,54)) * den(38)
  A(29) = cont_QA(wf(:,55),wf(:,56)) * den(41)
  A(30) = cont_QA(wf(:,10),wf(:,58)) * den(43)
  A(31) = cont_QA(wf(:,33),wf(:,59)) * den(44)
  A(32) = cont_QA(wf(:,61),wf(:,62)) * den(48)
  A(33) = cont_QA(wf(:,63),wf(:,64)) * den(51)
  A(34) = cont_QA(wf(:,42),wf(:,65)) * den(52)
  A(35) = cont_QA(wf(:,18),wf(:,67)) * den(54)
  A(36) = cont_QA(wf(:,10),wf(:,69)) * den(56)
  A(37) = cont_QA(wf(:,36),wf(:,59)) * den(57)
  A(38) = cont_QA(wf(:,46),wf(:,65)) * den(58)
  A(39) = cont_QA(wf(:,18),wf(:,71)) * den(60)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(39)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(1)-A(2)-A(3)-A(4)-A(5)-A(6))*f(1)

  M2(1) = (A(28)+A(29)+A(30)+A(31)+A(32)+A(33)+A(34)+A(35)+A(36)+A(37)+A(38)+A(39))*f(2)-A(7)*f(3)+(-A(9)-A(12)-A(17)-A(19)-A(23) &
       -A(25))*f(4)+(-A(8)-A(11)-A(14)-A(15)-A(16)-A(18)-A(20)-A(21)-A(22)-A(24)-A(26)-A(27))*f(5)+(A(10)+A(13))*f(6)

end subroutine colourvectors

end module ol_loop_ppllaj_nenexddxag_1_/**/REALKIND
