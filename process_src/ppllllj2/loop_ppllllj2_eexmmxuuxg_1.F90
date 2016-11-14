
module ol_colourmatrix_ppllllj2_eexmmxuuxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(0,0), K2(0,1), KL(0,1), KL2(1,1), KL2ct(1,1), KL2ct2(1,1)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  KL2(1,:) = [ 4]

  KL2ct(1,:) = [ 4]

  KL2ct2(1,:) = [ 4]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllllj2_eexmmxuuxg_1_/**/REALKIND



module ol_forced_parameters_ppllllj2_eexmmxuuxg_1_/**/REALKIND
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
  if (MM /= 0) write(*,101) 'MM = 0'
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
end module ol_forced_parameters_ppllllj2_eexmmxuuxg_1_/**/REALKIND

module ol_loop_ppllllj2_eexmmxuuxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(17), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:81)
  ! denominators
  complex(REALKIND), save :: den(100)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(0,128)
  ! zero helicity identifier
  logical,           save :: zerohel(128) = .true., zerohel_ct(128) = .true.

  contains

! **********************************************************************
subroutine fac_init_loop()
! Writes diagram prefactors to 'f', rsp. 'c'
! **********************************************************************
  use ol_parameters_decl_/**/REALKIND
  use ol_parameters_init_/**/REALKIND, only: parameters_init, loop_parameters_init
  use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB, DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = CI*countertermnorm*ctAAGG*eQED**4*gQCD**3
    f( 2) = CI*countertermnorm*ctAZGG*eQED**4*gQCD**3
    f( 3) = (2*countertermnorm*ctZGG*eQED**4*gQCD**3)/3._/**/REALKIND
    f( 4) = countertermnorm*ctZGG*eQED**4*gQCD**3
    f( 5) = CI*countertermnorm*ctZZGG*eQED**4*gQCD**3
    f( 6) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MW)/(cw**2*sw)
    f( 7) = (eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f( 8) = (2*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f( 9) = (eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(10) = (4*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(11) = (2*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(12) = (8*eQED**4*gQCD**3*integralnorm*SwF)/9._/**/REALKIND
    f(13) = eQED**4*gQCD**3*integralnorm*SwF
    f(14) = (4*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(15) = 2*eQED**4*gQCD**3*integralnorm*SwF
    f(16) = (eQED**4*gQCD**3*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(17) = (eQED**4*gQCD**3*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)


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
  real(REALKIND),    intent(in)  :: P(0:3,7)
  integer,           intent(in)  :: H(7)
  integer,           intent(in), optional  :: POLSEL(7)
  complex(REALKIND), intent(out) :: M1(0), M2(1)
  complex(REALKIND) :: A(21)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_Q(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_A(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))
    call pol_wf_V(P(:,7), rZERO, H(7), wf(:,-6), POLSEL(7))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_Q(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_A(P(:,6), rZERO, H(6), wf(:,-5), 0)
    call pol_wf_V(P(:,7), rZERO, H(7), wf(:,-6), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,3))
  call counter_VVG_G(wf(:,1),wf(:,2),wf(:,-6),wf(:,4))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,-3),wf(:,5))
  call prop_W_W(wf(:,5),Q(:,12),MZ,1_intkind1,wf(:,6))
  call counter_VVG_G(wf(:,1),wf(:,6),wf(:,-6),wf(:,7))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,8))
  call prop_W_W(wf(:,8),Q(:,3),MZ,1_intkind1,wf(:,9))
  call counter_VVG_G(wf(:,9),wf(:,2),wf(:,-6),wf(:,10))
  call counter_VVG_G(wf(:,9),wf(:,6),wf(:,-6),wf(:,11))
  call vert_VV_S(wf(:,9),wf(:,6),wf(:,12))
  call counter_GG_S(wf(:,3),wf(:,-6),wf(:,13))
  call vert_VQ_A(wf(:,1),wf(:,-4),wf(:,14))
  call counter_VG_G(wf(:,6),wf(:,-6),Q(:,64),wf(:,15),Q(:,76))
  call prop_Q_A(wf(:,14),Q(:,19),ZERO,0_intkind1,wf(:,16))
  call vert_QA_V(wf(:,16),wf(:,-5),wf(:,17))
  call vert_ZQ_A(gZu,wf(:,9),wf(:,-4),wf(:,18))
  call prop_Q_A(wf(:,18),Q(:,19),ZERO,0_intkind1,wf(:,19))
  call vert_QA_V(wf(:,19),wf(:,-5),wf(:,20))
  call vert_AV_Q(wf(:,-5),wf(:,1),wf(:,21))
  call prop_A_Q(wf(:,21),Q(:,35),ZERO,0_intkind1,wf(:,22))
  call vert_QA_V(wf(:,-4),wf(:,22),wf(:,23))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,9),wf(:,24))
  call prop_A_Q(wf(:,24),Q(:,35),ZERO,0_intkind1,wf(:,25))
  call vert_QA_V(wf(:,-4),wf(:,25),wf(:,26))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,27))
  call counter_VG_G(wf(:,9),wf(:,-6),Q(:,64),wf(:,28),Q(:,67))
  call prop_Q_A(wf(:,27),Q(:,28),ZERO,0_intkind1,wf(:,29))
  call vert_QA_V(wf(:,29),wf(:,-5),wf(:,30))
  call vert_ZQ_A(gZu,wf(:,6),wf(:,-4),wf(:,31))
  call prop_Q_A(wf(:,31),Q(:,28),ZERO,0_intkind1,wf(:,32))
  call vert_QA_V(wf(:,32),wf(:,-5),wf(:,33))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,34))
  call prop_A_Q(wf(:,34),Q(:,44),ZERO,0_intkind1,wf(:,35))
  call vert_QA_V(wf(:,-4),wf(:,35),wf(:,36))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,6),wf(:,37))
  call prop_A_Q(wf(:,37),Q(:,44),ZERO,0_intkind1,wf(:,38))
  call vert_QA_V(wf(:,-4),wf(:,38),wf(:,39))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,40))
  call counter_GG_V(wf(:,3),Q(:,48),wf(:,-6),Q(:,64),wf(:,41))
  call prop_Q_A(wf(:,40),Q(:,7),ZERO,0_intkind1,wf(:,42))
  call prop_W_W(wf(:,41),Q(:,112),MZ,1_intkind1,wf(:,43))
  call vert_QA_Z(gZl,wf(:,42),wf(:,-3),wf(:,44))
  call vert_ZQ_A(gZl,wf(:,9),wf(:,-2),wf(:,45))
  call prop_Q_A(wf(:,45),Q(:,7),ZERO,0_intkind1,wf(:,46))
  call vert_QA_Z(gZl,wf(:,46),wf(:,-3),wf(:,47))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,48))
  call prop_A_Q(wf(:,48),Q(:,11),ZERO,0_intkind1,wf(:,49))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,49),wf(:,50))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,9),wf(:,51))
  call prop_A_Q(wf(:,51),Q(:,11),ZERO,0_intkind1,wf(:,52))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,52),wf(:,53))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,54))
  call prop_Q_A(wf(:,54),Q(:,13),ZERO,0_intkind1,wf(:,55))
  call vert_QA_Z(gZl,wf(:,55),wf(:,-1),wf(:,56))
  call vert_ZQ_A(gZl,wf(:,6),wf(:,0),wf(:,57))
  call prop_Q_A(wf(:,57),Q(:,13),ZERO,0_intkind1,wf(:,58))
  call vert_QA_Z(gZl,wf(:,58),wf(:,-1),wf(:,59))
  call vert_AV_Q(wf(:,-1),wf(:,2),wf(:,60))
  call prop_A_Q(wf(:,60),Q(:,14),ZERO,0_intkind1,wf(:,61))
  call vert_QA_Z(gZl,wf(:,0),wf(:,61),wf(:,62))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,6),wf(:,63))
  call prop_A_Q(wf(:,63),Q(:,14),ZERO,0_intkind1,wf(:,64))
  call vert_QA_Z(gZl,wf(:,0),wf(:,64),wf(:,65))
  call vert_QA_V(wf(:,42),wf(:,-3),wf(:,66))
  call prop_W_W(wf(:,44),Q(:,15),MZ,1_intkind1,wf(:,67))
  call vert_QA_V(wf(:,46),wf(:,-3),wf(:,68))
  call prop_W_W(wf(:,47),Q(:,15),MZ,1_intkind1,wf(:,69))
  call vert_QA_V(wf(:,-2),wf(:,49),wf(:,70))
  call prop_W_W(wf(:,50),Q(:,15),MZ,1_intkind1,wf(:,71))
  call vert_QA_V(wf(:,-2),wf(:,52),wf(:,72))
  call prop_W_W(wf(:,53),Q(:,15),MZ,1_intkind1,wf(:,73))
  call vert_QA_V(wf(:,55),wf(:,-1),wf(:,74))
  call prop_W_W(wf(:,56),Q(:,15),MZ,1_intkind1,wf(:,75))
  call vert_QA_V(wf(:,58),wf(:,-1),wf(:,76))
  call prop_W_W(wf(:,59),Q(:,15),MZ,1_intkind1,wf(:,77))
  call vert_QA_V(wf(:,0),wf(:,61),wf(:,78))
  call vert_QA_V(wf(:,0),wf(:,64),wf(:,79))
  call prop_W_W(wf(:,62),Q(:,15),MZ,1_intkind1,wf(:,80))
  call prop_W_W(wf(:,65),Q(:,15),MZ,1_intkind1,wf(:,81))

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
  den(3) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,12) - MZ2)
  den(9) = 1 / (Q(5,3) - MZ2)
  den(14) = 1 / (Q(5,15) - MH2)
  den(17) = 1 / (Q(5,19))
  den(19) = 1 / (Q(5,76))
  den(24) = 1 / (Q(5,35))
  den(29) = 1 / (Q(5,28))
  den(31) = 1 / (Q(5,67))
  den(36) = 1 / (Q(5,44))
  den(41) = 1 / (Q(5,7))
  den(43) = 1 / (Q(5,112) - MZ2)
  den(48) = 1 / (Q(5,11))
  den(53) = 1 / (Q(5,13))
  den(58) = 1 / (Q(5,14))
  den(63) = 1 / (Q(5,15))
  den(65) = 1 / (Q(5,15) - MZ2)

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(3)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(3)*den(10)
  den(12) = den(6)*den(9)
  den(13) = den(3)*den(12)
  den(15) = den(12)*den(14)
  den(16) = den(3)*den(15)
  den(18) = den(1)*den(17)
  den(20) = den(6)*den(19)
  den(21) = den(18)*den(20)
  den(22) = den(9)*den(17)
  den(23) = den(20)*den(22)
  den(25) = den(1)*den(24)
  den(26) = den(20)*den(25)
  den(27) = den(9)*den(24)
  den(28) = den(20)*den(27)
  den(30) = den(2)*den(29)
  den(32) = den(9)*den(31)
  den(33) = den(30)*den(32)
  den(34) = den(6)*den(29)
  den(35) = den(32)*den(34)
  den(37) = den(2)*den(36)
  den(38) = den(32)*den(37)
  den(39) = den(6)*den(36)
  den(40) = den(32)*den(39)
  den(42) = den(1)*den(41)
  den(44) = den(3)*den(43)
  den(45) = den(42)*den(44)
  den(46) = den(9)*den(41)
  den(47) = den(44)*den(46)
  den(49) = den(1)*den(48)
  den(50) = den(44)*den(49)
  den(51) = den(9)*den(48)
  den(52) = den(44)*den(51)
  den(54) = den(2)*den(53)
  den(55) = den(44)*den(54)
  den(56) = den(6)*den(53)
  den(57) = den(44)*den(56)
  den(59) = den(2)*den(58)
  den(60) = den(44)*den(59)
  den(61) = den(6)*den(58)
  den(62) = den(44)*den(61)
  den(64) = den(42)*den(63)
  den(66) = den(42)*den(65)
  den(67) = den(46)*den(63)
  den(68) = den(46)*den(65)
  den(69) = den(49)*den(63)
  den(70) = den(49)*den(65)
  den(71) = den(51)*den(63)
  den(72) = den(51)*den(65)
  den(73) = den(54)*den(63)
  den(74) = den(54)*den(65)
  den(75) = den(56)*den(63)
  den(76) = den(56)*den(65)
  den(77) = den(59)*den(63)
  den(78) = den(61)*den(63)
  den(79) = den(59)*den(65)
  den(80) = den(61)*den(65)
  den(81) = den(1)*den(2)*den(3)
  den(82) = den(1)*den(3)*den(6)
  den(83) = den(2)*den(3)*den(9)
  den(84) = den(3)*den(6)*den(9)
  den(85) = den(3)*den(64)
  den(86) = den(3)*den(66)
  den(87) = den(3)*den(67)
  den(88) = den(3)*den(68)
  den(89) = den(3)*den(69)
  den(90) = den(3)*den(70)
  den(91) = den(3)*den(71)
  den(92) = den(3)*den(72)
  den(93) = den(3)*den(73)
  den(94) = den(3)*den(74)
  den(95) = den(3)*den(75)
  den(96) = den(3)*den(76)
  den(97) = den(3)*den(77)
  den(98) = den(3)*den(78)
  den(99) = den(3)*den(79)
  den(100) = den(3)*den(80)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(21)


  A(1) = cont_VV(wf(:,3),wf(:,4)) * den(5)
  A(2) = cont_VV(wf(:,3),wf(:,7)) * den(8)
  A(3) = cont_VV(wf(:,3),wf(:,10)) * den(11)
  A(4) = cont_VV(wf(:,3),wf(:,11)) * den(13)
  A(5) = cont_SS(wf(:,12),wf(:,13)) * den(16)
  A(6) = cont_VV(wf(:,15),wf(:,17)) * den(21)
  A(7) = cont_VV(wf(:,15),wf(:,20)) * den(23)
  A(8) = cont_VV(wf(:,15),wf(:,23)) * den(26)
  A(9) = cont_VV(wf(:,15),wf(:,26)) * den(28)
  A(10) = cont_VV(wf(:,28),wf(:,30)) * den(33)
  A(11) = cont_VV(wf(:,28),wf(:,33)) * den(35)
  A(12) = cont_VV(wf(:,28),wf(:,36)) * den(38)
  A(13) = cont_VV(wf(:,28),wf(:,39)) * den(40)
  A(14) = cont_VV(wf(:,43),wf(:,44)) * den(45)
  A(15) = cont_VV(wf(:,43),wf(:,47)) * den(47)
  A(16) = cont_VV(wf(:,43),wf(:,50)) * den(50)
  A(17) = cont_VV(wf(:,43),wf(:,53)) * den(52)
  A(18) = cont_VV(wf(:,43),wf(:,56)) * den(55)
  A(19) = cont_VV(wf(:,43),wf(:,59)) * den(57)
  A(20) = cont_VV(wf(:,43),wf(:,62)) * den(60)
  A(21) = cont_VV(wf(:,43),wf(:,65)) * den(62)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(21)
  complex(REALKIND), intent(out) :: M1(0), M2(1)


  M2(1) = A(1)*f(1)+(A(2)+A(3))*f(2)+(A(6)+A(8)+A(10)+A(12))*f(3)+(-A(7)-A(9)-A(11)-A(13)-A(14)-A(15)-A(16)-A(17)-A(18)-A(19) &
       -A(20)-A(21))*f(4)+A(4)*f(5)-A(5)*f(6)

end subroutine colourvectors

end module ol_loop_ppllllj2_eexmmxuuxg_1_/**/REALKIND
