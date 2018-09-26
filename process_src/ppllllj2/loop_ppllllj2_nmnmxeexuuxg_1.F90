
module ol_colourmatrix_ppllllj2_nmnmxeexuuxg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllllj2_nmnmxeexuuxg_1_/**/REALKIND



module ol_forced_parameters_ppllllj2_nmnmxeexuuxg_1_/**/REALKIND
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
  if (ME /= 0) write(*,101) 'ME = 0'
  if (MM /= 0) write(*,101) 'MM = 0'
  if (CKMORDER /= 0) write(*,101) 'CKMORDER = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (YU /= 0) write(*,101) 'YU = 0'
  if (YD /= 0) write(*,101) 'YD = 0'
  if (YS /= 0) write(*,101) 'YS = 0'
  if (YC /= 0) write(*,101) 'YC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllllj2_nmnmxeexuuxg_1_/**/REALKIND

module ol_loop_ppllllj2_nmnmxeexuuxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(20), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:56)
  ! denominators
  complex(REALKIND), save :: den(77)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(0,128), Mct(1,128), Mcol_loop(1,128)
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
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB
!  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
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
    f( 5) = (eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f( 6) = (2*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f( 7) = eQED**4*gQCD**3*integralnorm*SwF
    f( 8) = (4*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f( 9) = 2*eQED**4*gQCD**3*integralnorm*SwF
    f(10) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*lambdaHZZ*MB*MW*YB)/(cw**2*MQ2sum*sw)
    f(11) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YB)/(cw**2*sw**2*2._/**/REALKIND)
    f(12) = (countertermnorm*ctZGG*eQED**4*gQCD**3*lambdaHZZ*YE)/(cw**2*sw**2*2._/**/REALKIND)
    f(13) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YE)/(cw**2*sw**2*2._/**/REALKIND)
    f(14) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YE)/(cw**2*sw**2)
    f(15) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MB*YB*YE)/(2._/**/REALKIND*MQ2sum*MW*sw)
    f(16) = (eQED**4*gQCD**3*integralnorm*SwF*YB*YE)/(MW**2*sw**2*4._/**/REALKIND)
    f(17) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*lambdaHZZ*MT*MW*YT)/(cw**2*MQ2sum*sw)
    f(18) = (eQED**4*gQCD**3*integralnorm*lambdaHZZ*SwF*YT)/(cw**2*sw**2*2._/**/REALKIND)
    f(19) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MT*YE*YT)/(2._/**/REALKIND*MQ2sum*MW*sw)
    f(20) = (eQED**4*gQCD**3*integralnorm*SwF*YE*YT)/(MW**2*sw**2*4._/**/REALKIND)


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
  complex(REALKIND) :: A(23)
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
  call vert_AQ_S(gH,wf(:,-3),wf(:,-2),wf(:,9))
  call vert_SV_V(wf(:,9),wf(:,4),wf(:,10))
  call counter_GG_V(wf(:,3),Q(:,48),wf(:,-6),Q(:,64),wf(:,11))
  call prop_W_W(wf(:,10),Q(:,15),MZ,1_intkind1,wf(:,12))
  call vert_VV_S(wf(:,4),wf(:,7),wf(:,13))
  call counter_GG_S(wf(:,3),wf(:,-6),wf(:,14))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,-4),wf(:,15))
  call counter_SG_G(wf(:,9),wf(:,-6),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,19),ZERO,0_intkind1,wf(:,17))
  call vert_QA_V(wf(:,17),wf(:,-5),wf(:,18))
  call counter_VG_G(wf(:,7),wf(:,-6),Q(:,64),wf(:,19),Q(:,76))
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
  call prop_Q_A(wf(:,36),Q(:,7),ZERO,0_intkind1,wf(:,37))
  call vert_AQ_S(gH,wf(:,-3),wf(:,37),wf(:,38))
  call prop_W_W(wf(:,11),Q(:,112),MZ,1_intkind1,wf(:,39))
  call vert_QA_Z(gZl,wf(:,37),wf(:,-3),wf(:,40))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,4),wf(:,41))
  call prop_A_Q(wf(:,41),Q(:,11),ZERO,0_intkind1,wf(:,42))
  call vert_AQ_S(gH,wf(:,42),wf(:,-2),wf(:,43))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,42),wf(:,44))
  call vert_ZQ_A(gZn,wf(:,7),wf(:,0),wf(:,45))
  call prop_Q_A(wf(:,45),Q(:,13),ZERO,0_intkind1,wf(:,46))
  call vert_QA_Z(gZn,wf(:,46),wf(:,-1),wf(:,47))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,7),wf(:,48))
  call prop_A_Q(wf(:,48),Q(:,14),ZERO,0_intkind1,wf(:,49))
  call vert_QA_Z(gZn,wf(:,0),wf(:,49),wf(:,50))
  call vert_QA_V(wf(:,37),wf(:,-3),wf(:,51))
  call prop_W_W(wf(:,40),Q(:,15),MZ,1_intkind1,wf(:,52))
  call vert_QA_V(wf(:,-2),wf(:,42),wf(:,53))
  call prop_W_W(wf(:,44),Q(:,15),MZ,1_intkind1,wf(:,54))
  call prop_W_W(wf(:,47),Q(:,15),MZ,1_intkind1,wf(:,55))
  call prop_W_W(wf(:,50),Q(:,15),MZ,1_intkind1,wf(:,56))

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
  den(9) = 1 / (Q(5,12) - MH2)
  den(11) = 1 / (Q(5,15) - MZ2)
  den(14) = 1 / (Q(5,15) - MH2)
  den(17) = 1 / (Q(5,19))
  den(19) = 1 / (Q(5,76))
  den(24) = 1 / (Q(5,35))
  den(28) = 1 / (Q(5,28))
  den(30) = 1 / (Q(5,67))
  den(35) = 1 / (Q(5,44))
  den(40) = 1 / (Q(5,7))
  den(42) = 1 / (Q(5,112) - MH2)
  den(45) = 1 / (Q(5,112) - MZ2)
  den(48) = 1 / (Q(5,11))
  den(52) = 1 / (Q(5,13))
  den(55) = 1 / (Q(5,14))
  den(59) = 1 / (Q(5,15))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(3)*den(7)
  den(10) = den(1)*den(9)
  den(12) = den(10)*den(11)
  den(13) = den(3)*den(12)
  den(15) = den(7)*den(14)
  den(16) = den(3)*den(15)
  den(18) = den(1)*den(17)
  den(20) = den(9)*den(19)
  den(21) = den(18)*den(20)
  den(22) = den(6)*den(19)
  den(23) = den(18)*den(22)
  den(25) = den(1)*den(24)
  den(26) = den(20)*den(25)
  den(27) = den(22)*den(25)
  den(29) = den(2)*den(28)
  den(31) = den(1)*den(30)
  den(32) = den(29)*den(31)
  den(33) = den(6)*den(28)
  den(34) = den(31)*den(33)
  den(36) = den(2)*den(35)
  den(37) = den(31)*den(36)
  den(38) = den(6)*den(35)
  den(39) = den(31)*den(38)
  den(41) = den(1)*den(40)
  den(43) = den(3)*den(42)
  den(44) = den(41)*den(43)
  den(46) = den(3)*den(45)
  den(47) = den(41)*den(46)
  den(49) = den(1)*den(48)
  den(50) = den(43)*den(49)
  den(51) = den(46)*den(49)
  den(53) = den(6)*den(52)
  den(54) = den(46)*den(53)
  den(56) = den(6)*den(55)
  den(57) = den(46)*den(56)
  den(58) = den(14)*den(41)
  den(60) = den(41)*den(59)
  den(61) = den(11)*den(41)
  den(62) = den(14)*den(49)
  den(63) = den(49)*den(59)
  den(64) = den(11)*den(49)
  den(65) = den(11)*den(53)
  den(66) = den(11)*den(56)
  den(67) = den(1)*den(3)*den(9)
  den(68) = den(1)*den(2)*den(3)
  den(69) = den(1)*den(3)*den(6)
  den(70) = den(3)*den(58)
  den(71) = den(3)*den(60)
  den(72) = den(3)*den(61)
  den(73) = den(3)*den(62)
  den(74) = den(3)*den(63)
  den(75) = den(3)*den(64)
  den(76) = den(3)*den(65)
  den(77) = den(3)*den(66)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(23)


  A(1) = cont_VV(wf(:,3),wf(:,5)) * den(5)
  A(2) = cont_VV(wf(:,3),wf(:,8)) * den(8)
  A(3) = cont_VV(wf(:,11),wf(:,12)) * den(13)
  A(4) = cont_SS(wf(:,13),wf(:,14)) * den(16)
  A(5) = cont_SS(wf(:,13),wf(:,14)) * den(16)
  A(6) = cont_VV(wf(:,16),wf(:,18)) * den(21)
  A(7) = cont_VV(wf(:,16),wf(:,18)) * den(21)
  A(8) = cont_VV(wf(:,18),wf(:,19)) * den(23)
  A(9) = cont_VV(wf(:,16),wf(:,22)) * den(26)
  A(10) = cont_VV(wf(:,16),wf(:,22)) * den(26)
  A(11) = cont_VV(wf(:,19),wf(:,22)) * den(27)
  A(12) = cont_VV(wf(:,24),wf(:,26)) * den(32)
  A(13) = cont_VV(wf(:,24),wf(:,29)) * den(34)
  A(14) = cont_VV(wf(:,24),wf(:,32)) * den(37)
  A(15) = cont_VV(wf(:,24),wf(:,35)) * den(39)
  A(16) = cont_SS(wf(:,14),wf(:,38)) * den(44)
  A(17) = cont_SS(wf(:,14),wf(:,38)) * den(44)
  A(18) = cont_VV(wf(:,39),wf(:,40)) * den(47)
  A(19) = cont_SS(wf(:,14),wf(:,43)) * den(50)
  A(20) = cont_SS(wf(:,14),wf(:,43)) * den(50)
  A(21) = cont_VV(wf(:,39),wf(:,44)) * den(51)
  A(22) = cont_VV(wf(:,39),wf(:,47)) * den(54)
  A(23) = cont_VV(wf(:,39),wf(:,50)) * den(57)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(23)
  complex(REALKIND), intent(out) :: M1(0), M2(1)


  M2(1) = A(1)*f(1)+(A(12)+A(14))*f(2)+(-A(8)-A(11)-A(13)-A(15)-A(18)-A(21)-A(22)-A(23))*f(3)+A(2)*f(4)-A(5)*f(10)+A(3)*f(12)+( &
       -A(7)-A(10)-A(17)-A(20))*f(15)-A(4)*f(17)+(-A(6)-A(9)-A(16)-A(19))*f(19)

end subroutine colourvectors

end module ol_loop_ppllllj2_nmnmxeexuuxg_1_/**/REALKIND
