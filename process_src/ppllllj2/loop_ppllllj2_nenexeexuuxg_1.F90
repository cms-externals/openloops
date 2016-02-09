
module ol_colourmatrix_ppllllj2_nenexeexuuxg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllllj2_nenexeexuuxg_1_/**/REALKIND



module ol_forced_parameters_ppllllj2_nenexeexuuxg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj2_nenexeexuuxg_1_/**/REALKIND

module ol_loop_ppllllj2_nenexeexuuxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(27), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:76)
  ! denominators
  complex(REALKIND), save :: den(92)
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
    f( 1) = CI*countertermnorm*ctAZGG*eQED**4*gQCD**3
    f( 2) = (2*countertermnorm*ctZGG*eQED**4*gQCD**3)/3._/**/REALKIND
    f( 3) = countertermnorm*ctZGG*eQED**4*gQCD**3
    f( 4) = CI*countertermnorm*ctZZGG*eQED**4*gQCD**3
    f( 5) = (countertermnorm*ctZGG*cw*eQED**4*gQCD**3)/(sw**3*2._/**/REALKIND)
    f( 6) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MW)/(2._/**/REALKIND*sw**3)
    f( 7) = (CI*countertermnorm*ctWWGG*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f( 8) = (countertermnorm*ctZGG*eQED**4*gQCD**3)/(sw**2*2._/**/REALKIND)
    f( 9) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MW)/(cw**2*sw)
    f(10) = (eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(11) = (2*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(12) = eQED**4*gQCD**3*integralnorm*SwF
    f(13) = (4*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(14) = 2*eQED**4*gQCD**3*integralnorm*SwF
    f(15) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**4*4._/**/REALKIND)
    f(16) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**4*2._/**/REALKIND)
    f(17) = (eQED**4*gQCD**3*integralnorm*MB*SwF)/(sw**4*4._/**/REALKIND)
    f(18) = (eQED**4*gQCD**3*integralnorm*MT*SwF)/(sw**4*4._/**/REALKIND)
    f(19) = (cw*eQED**4*gQCD**3*integralnorm*SwF)/(sw**3*2._/**/REALKIND)
    f(20) = (cw*eQED**4*gQCD**3*integralnorm*SwF)/sw**3
    f(21) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*6._/**/REALKIND)
    f(22) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(23) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(24) = (2*eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(25) = (eQED**4*gQCD**3*integralnorm*SwF)/sw**2
    f(26) = (eQED**4*gQCD**3*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(27) = (eQED**4*gQCD**3*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)


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
  complex(REALKIND) :: A(20)
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
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call counter_VVG_G(wf(:,4),wf(:,2),wf(:,-6),wf(:,5))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,-3),wf(:,6))
  call prop_W_W(wf(:,6),Q(:,12),MZ,1_intkind1,wf(:,7))
  call counter_VVG_G(wf(:,4),wf(:,7),wf(:,-6),wf(:,8))
  call vert_QA_W(wf(:,0),wf(:,-3),wf(:,9))
  call vert_QA_W(wf(:,-2),wf(:,-1),wf(:,10))
  call prop_W_W(wf(:,9),Q(:,9),MW,1_intkind1,wf(:,11))
  call prop_W_W(wf(:,10),Q(:,6),MW,1_intkind1,wf(:,12))
  call counter_VVG_G(wf(:,12),wf(:,11),wf(:,-6),wf(:,13))
  call vert_VV_S(wf(:,4),wf(:,7),wf(:,14))
  call counter_GG_S(wf(:,3),wf(:,-6),wf(:,15))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,-4),wf(:,16))
  call counter_VG_G(wf(:,7),wf(:,-6),Q(:,64),wf(:,17),Q(:,76))
  call prop_Q_A(wf(:,16),Q(:,19),ZERO,0_intkind1,wf(:,18))
  call vert_QA_V(wf(:,18),wf(:,-5),wf(:,19))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,4),wf(:,20))
  call prop_A_Q(wf(:,20),Q(:,35),ZERO,0_intkind1,wf(:,21))
  call vert_QA_V(wf(:,-4),wf(:,21),wf(:,22))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,23))
  call counter_VG_G(wf(:,4),wf(:,-6),Q(:,64),wf(:,24),Q(:,67))
  call prop_Q_A(wf(:,23),Q(:,28),ZERO,0_intkind1,wf(:,25))
  call vert_QA_V(wf(:,25),wf(:,-5),wf(:,26))
  call vert_ZQ_A(gZu,wf(:,7),wf(:,-4),wf(:,27))
  call prop_Q_A(wf(:,27),Q(:,28),ZERO,0_intkind1,wf(:,28))
  call vert_QA_V(wf(:,28),wf(:,-5),wf(:,29))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,30))
  call prop_A_Q(wf(:,30),Q(:,44),ZERO,0_intkind1,wf(:,31))
  call vert_QA_V(wf(:,-4),wf(:,31),wf(:,32))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,7),wf(:,33))
  call prop_A_Q(wf(:,33),Q(:,44),ZERO,0_intkind1,wf(:,34))
  call vert_QA_V(wf(:,-4),wf(:,34),wf(:,35))
  call vert_ZQ_A(gZl,wf(:,4),wf(:,-2),wf(:,36))
  call counter_GG_V(wf(:,3),Q(:,48),wf(:,-6),Q(:,64),wf(:,37))
  call prop_Q_A(wf(:,36),Q(:,7),ZERO,0_intkind1,wf(:,38))
  call prop_W_W(wf(:,37),Q(:,112),MZ,1_intkind1,wf(:,39))
  call vert_QA_Z(gZl,wf(:,38),wf(:,-3),wf(:,40))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,4),wf(:,41))
  call prop_A_Q(wf(:,41),Q(:,11),ZERO,0_intkind1,wf(:,42))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,42),wf(:,43))
  call vert_VV_S(wf(:,12),wf(:,11),wf(:,44))
  call vert_UV_W(wf(:,11),Q(:,9),wf(:,12),Q(:,6),wf(:,45))
  call prop_W_W(wf(:,45),Q(:,15),MZ,1_intkind1,wf(:,46))
  call vert_WQ_A(wf(:,12),wf(:,0),wf(:,47))
  call prop_Q_A(wf(:,47),Q(:,7),ZERO,0_intkind1,wf(:,48))
  call vert_QA_Z(gZl,wf(:,48),wf(:,-3),wf(:,49))
  call vert_AW_Q(wf(:,-3),wf(:,12),wf(:,50))
  call prop_A_Q(wf(:,50),Q(:,14),ZERO,0_intkind1,wf(:,51))
  call vert_QA_Z(gZn,wf(:,0),wf(:,51),wf(:,52))
  call vert_AW_Q(wf(:,-1),wf(:,11),wf(:,53))
  call prop_A_Q(wf(:,53),Q(:,11),ZERO,0_intkind1,wf(:,54))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,54),wf(:,55))
  call vert_WQ_A(wf(:,11),wf(:,-2),wf(:,56))
  call prop_Q_A(wf(:,56),Q(:,13),ZERO,0_intkind1,wf(:,57))
  call vert_QA_Z(gZn,wf(:,57),wf(:,-1),wf(:,58))
  call vert_ZQ_A(gZn,wf(:,7),wf(:,0),wf(:,59))
  call prop_Q_A(wf(:,59),Q(:,13),ZERO,0_intkind1,wf(:,60))
  call vert_QA_Z(gZn,wf(:,60),wf(:,-1),wf(:,61))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,7),wf(:,62))
  call prop_A_Q(wf(:,62),Q(:,14),ZERO,0_intkind1,wf(:,63))
  call vert_QA_Z(gZn,wf(:,0),wf(:,63),wf(:,64))
  call vert_QA_V(wf(:,38),wf(:,-3),wf(:,65))
  call prop_W_W(wf(:,40),Q(:,15),MZ,1_intkind1,wf(:,66))
  call vert_QA_V(wf(:,-2),wf(:,42),wf(:,67))
  call prop_W_W(wf(:,43),Q(:,15),MZ,1_intkind1,wf(:,68))
  call vert_QA_V(wf(:,48),wf(:,-3),wf(:,69))
  call prop_W_W(wf(:,49),Q(:,15),MZ,1_intkind1,wf(:,70))
  call prop_W_W(wf(:,52),Q(:,15),MZ,1_intkind1,wf(:,71))
  call vert_QA_V(wf(:,-2),wf(:,54),wf(:,72))
  call prop_W_W(wf(:,55),Q(:,15),MZ,1_intkind1,wf(:,73))
  call prop_W_W(wf(:,58),Q(:,15),MZ,1_intkind1,wf(:,74))
  call prop_W_W(wf(:,61),Q(:,15),MZ,1_intkind1,wf(:,75))
  call prop_W_W(wf(:,64),Q(:,15),MZ,1_intkind1,wf(:,76))

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
  den(2) = 1 / (Q(5,12))
  den(3) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,12) - MZ2)
  den(9) = 1 / (Q(5,9) - MW2)
  den(10) = 1 / (Q(5,6) - MW2)
  den(13) = 1 / (Q(5,15) - MH2)
  den(16) = 1 / (Q(5,19))
  den(18) = 1 / (Q(5,76))
  den(21) = 1 / (Q(5,35))
  den(24) = 1 / (Q(5,28))
  den(26) = 1 / (Q(5,67))
  den(31) = 1 / (Q(5,44))
  den(36) = 1 / (Q(5,7))
  den(38) = 1 / (Q(5,112) - MZ2)
  den(41) = 1 / (Q(5,11))
  den(46) = 1 / (Q(5,15) - MZ2)
  den(51) = 1 / (Q(5,14))
  den(56) = 1 / (Q(5,13))
  den(63) = 1 / (Q(5,15))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(3)*den(7)
  den(11) = den(9)*den(10)
  den(12) = den(3)*den(11)
  den(14) = den(7)*den(13)
  den(15) = den(3)*den(14)
  den(17) = den(1)*den(16)
  den(19) = den(6)*den(18)
  den(20) = den(17)*den(19)
  den(22) = den(1)*den(21)
  den(23) = den(19)*den(22)
  den(25) = den(2)*den(24)
  den(27) = den(1)*den(26)
  den(28) = den(25)*den(27)
  den(29) = den(6)*den(24)
  den(30) = den(27)*den(29)
  den(32) = den(2)*den(31)
  den(33) = den(27)*den(32)
  den(34) = den(6)*den(31)
  den(35) = den(27)*den(34)
  den(37) = den(1)*den(36)
  den(39) = den(3)*den(38)
  den(40) = den(37)*den(39)
  den(42) = den(1)*den(41)
  den(43) = den(39)*den(42)
  den(44) = den(11)*den(13)
  den(45) = den(3)*den(44)
  den(47) = den(11)*den(46)
  den(48) = den(3)*den(47)
  den(49) = den(10)*den(36)
  den(50) = den(39)*den(49)
  den(52) = den(10)*den(51)
  den(53) = den(39)*den(52)
  den(54) = den(9)*den(41)
  den(55) = den(39)*den(54)
  den(57) = den(9)*den(56)
  den(58) = den(39)*den(57)
  den(59) = den(6)*den(56)
  den(60) = den(39)*den(59)
  den(61) = den(6)*den(51)
  den(62) = den(39)*den(61)
  den(64) = den(37)*den(63)
  den(65) = den(37)*den(46)
  den(66) = den(42)*den(63)
  den(67) = den(42)*den(46)
  den(68) = den(11)*den(63)
  den(69) = den(49)*den(63)
  den(70) = den(46)*den(49)
  den(71) = den(46)*den(52)
  den(72) = den(54)*den(63)
  den(73) = den(46)*den(54)
  den(74) = den(46)*den(57)
  den(75) = den(46)*den(59)
  den(76) = den(46)*den(61)
  den(77) = den(1)*den(2)*den(3)
  den(78) = den(1)*den(3)*den(6)
  den(79) = den(3)*den(64)
  den(80) = den(3)*den(65)
  den(81) = den(3)*den(66)
  den(82) = den(3)*den(67)
  den(83) = den(3)*den(68)
  den(84) = den(3)*den(9)*den(10)
  den(85) = den(3)*den(69)
  den(86) = den(3)*den(70)
  den(87) = den(3)*den(71)
  den(88) = den(3)*den(72)
  den(89) = den(3)*den(73)
  den(90) = den(3)*den(74)
  den(91) = den(3)*den(75)
  den(92) = den(3)*den(76)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(20)


  A(1) = cont_VV(wf(:,3),wf(:,5)) * den(5)
  A(2) = cont_VV(wf(:,3),wf(:,8)) * den(8)
  A(3) = cont_VV(wf(:,3),wf(:,13)) * den(12)
  A(4) = cont_SS(wf(:,14),wf(:,15)) * den(15)
  A(5) = cont_VV(wf(:,17),wf(:,19)) * den(20)
  A(6) = cont_VV(wf(:,17),wf(:,22)) * den(23)
  A(7) = cont_VV(wf(:,24),wf(:,26)) * den(28)
  A(8) = cont_VV(wf(:,24),wf(:,29)) * den(30)
  A(9) = cont_VV(wf(:,24),wf(:,32)) * den(33)
  A(10) = cont_VV(wf(:,24),wf(:,35)) * den(35)
  A(11) = cont_VV(wf(:,39),wf(:,40)) * den(40)
  A(12) = cont_VV(wf(:,39),wf(:,43)) * den(43)
  A(13) = cont_SS(wf(:,15),wf(:,44)) * den(45)
  A(14) = cont_VV(wf(:,37),wf(:,46)) * den(48)
  A(15) = cont_VV(wf(:,39),wf(:,49)) * den(50)
  A(16) = cont_VV(wf(:,39),wf(:,52)) * den(53)
  A(17) = cont_VV(wf(:,39),wf(:,55)) * den(55)
  A(18) = cont_VV(wf(:,39),wf(:,58)) * den(58)
  A(19) = cont_VV(wf(:,39),wf(:,61)) * den(60)
  A(20) = cont_VV(wf(:,39),wf(:,64)) * den(62)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(20)
  complex(REALKIND), intent(out) :: M1(0), M2(1)


  M2(1) = A(1)*f(1)+(A(7)+A(9))*f(2)+(-A(5)-A(6)-A(8)-A(10)-A(11)-A(12)-A(19)-A(20))*f(3)+A(2)*f(4)+A(14)*f(5)+A(13)*f(6) &
       -A(3)*f(7)+(A(15)+A(16)+A(17)+A(18))*f(8)-A(4)*f(9)

end subroutine colourvectors

end module ol_loop_ppllllj2_nenexeexuuxg_1_/**/REALKIND
