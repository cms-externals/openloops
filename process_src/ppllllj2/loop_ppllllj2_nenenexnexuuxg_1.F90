
module ol_colourmatrix_ppllllj2_nenenexnexuuxg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllllj2_nenenexnexuuxg_1_/**/REALKIND



module ol_forced_parameters_ppllllj2_nenenexnexuuxg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj2_nenenexnexuuxg_1_/**/REALKIND

module ol_loop_ppllllj2_nenenexnexuuxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(7), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:76)
  ! denominators
  complex(REALKIND), save :: den(87)
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
    f(1) = countertermnorm*ctZGG*eQED**4*gQCD**3
    f(2) = CI*countertermnorm*ctZZGG*eQED**4*gQCD**3
    f(3) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MW)/(cw**2*sw)
    f(4) = eQED**4*gQCD**3*integralnorm*SwF
    f(5) = 2*eQED**4*gQCD**3*integralnorm*SwF
    f(6) = (eQED**4*gQCD**3*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(7) = (eQED**4*gQCD**3*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)


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
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_A(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_Q(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_A(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))
    call pol_wf_V(P(:,7), rZERO, H(7), wf(:,-6), POLSEL(7))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_A(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_Q(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_A(P(:,6), rZERO, H(6), wf(:,-5), 0)
    call pol_wf_V(P(:,7), rZERO, H(7), wf(:,-6), 0)

  end if

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,-3),wf(:,2))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,5),MZ,1_intkind1,wf(:,4))
  call prop_W_W(wf(:,2),Q(:,10),MZ,1_intkind1,wf(:,5))
  call counter_VVG_G(wf(:,4),wf(:,5),wf(:,-6),wf(:,6))
  call vert_QA_Z(gZn,wf(:,0),wf(:,-3),wf(:,7))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,-2),wf(:,8))
  call prop_W_W(wf(:,7),Q(:,9),MZ,1_intkind1,wf(:,9))
  call prop_W_W(wf(:,8),Q(:,6),MZ,1_intkind1,wf(:,10))
  call counter_VVG_G(wf(:,10),wf(:,9),wf(:,-6),wf(:,11))
  call vert_VV_S(wf(:,4),wf(:,5),wf(:,12))
  call counter_GG_S(wf(:,3),wf(:,-6),wf(:,13))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,-4),wf(:,14))
  call counter_VG_G(wf(:,5),wf(:,-6),Q(:,64),wf(:,15),Q(:,74))
  call prop_Q_A(wf(:,14),Q(:,21),ZERO,0_intkind1,wf(:,16))
  call vert_QA_V(wf(:,16),wf(:,-5),wf(:,17))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,4),wf(:,18))
  call prop_A_Q(wf(:,18),Q(:,37),ZERO,0_intkind1,wf(:,19))
  call vert_QA_V(wf(:,-4),wf(:,19),wf(:,20))
  call vert_ZQ_A(gZu,wf(:,5),wf(:,-4),wf(:,21))
  call counter_VG_G(wf(:,4),wf(:,-6),Q(:,64),wf(:,22),Q(:,69))
  call prop_Q_A(wf(:,21),Q(:,26),ZERO,0_intkind1,wf(:,23))
  call vert_QA_V(wf(:,23),wf(:,-5),wf(:,24))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,5),wf(:,25))
  call prop_A_Q(wf(:,25),Q(:,42),ZERO,0_intkind1,wf(:,26))
  call vert_QA_V(wf(:,-4),wf(:,26),wf(:,27))
  call vert_ZQ_A(gZn,wf(:,4),wf(:,-1),wf(:,28))
  call counter_GG_V(wf(:,3),Q(:,48),wf(:,-6),Q(:,64),wf(:,29))
  call prop_Q_A(wf(:,28),Q(:,7),ZERO,0_intkind1,wf(:,30))
  call prop_W_W(wf(:,29),Q(:,112),MZ,1_intkind1,wf(:,31))
  call vert_QA_Z(gZn,wf(:,30),wf(:,-3),wf(:,32))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,4),wf(:,33))
  call prop_A_Q(wf(:,33),Q(:,13),ZERO,0_intkind1,wf(:,34))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,34),wf(:,35))
  call vert_VV_S(wf(:,10),wf(:,9),wf(:,36))
  call vert_ZQ_A(gZu,wf(:,9),wf(:,-4),wf(:,37))
  call counter_VG_G(wf(:,10),wf(:,-6),Q(:,64),wf(:,38),Q(:,70))
  call prop_Q_A(wf(:,37),Q(:,25),ZERO,0_intkind1,wf(:,39))
  call vert_QA_V(wf(:,39),wf(:,-5),wf(:,40))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,9),wf(:,41))
  call prop_A_Q(wf(:,41),Q(:,41),ZERO,0_intkind1,wf(:,42))
  call vert_QA_V(wf(:,-4),wf(:,42),wf(:,43))
  call vert_ZQ_A(gZu,wf(:,10),wf(:,-4),wf(:,44))
  call counter_VG_G(wf(:,9),wf(:,-6),Q(:,64),wf(:,45),Q(:,73))
  call prop_Q_A(wf(:,44),Q(:,22),ZERO,0_intkind1,wf(:,46))
  call vert_QA_V(wf(:,46),wf(:,-5),wf(:,47))
  call vert_AZ_Q(gZu,wf(:,-5),wf(:,10),wf(:,48))
  call prop_A_Q(wf(:,48),Q(:,38),ZERO,0_intkind1,wf(:,49))
  call vert_QA_V(wf(:,-4),wf(:,49),wf(:,50))
  call vert_ZQ_A(gZn,wf(:,10),wf(:,0),wf(:,51))
  call prop_Q_A(wf(:,51),Q(:,7),ZERO,0_intkind1,wf(:,52))
  call vert_QA_Z(gZn,wf(:,52),wf(:,-3),wf(:,53))
  call vert_AZ_Q(gZn,wf(:,-3),wf(:,10),wf(:,54))
  call prop_A_Q(wf(:,54),Q(:,14),ZERO,0_intkind1,wf(:,55))
  call vert_QA_Z(gZn,wf(:,0),wf(:,55),wf(:,56))
  call vert_ZQ_A(gZn,wf(:,9),wf(:,-1),wf(:,57))
  call prop_Q_A(wf(:,57),Q(:,11),ZERO,0_intkind1,wf(:,58))
  call vert_QA_Z(gZn,wf(:,58),wf(:,-2),wf(:,59))
  call vert_AZ_Q(gZn,wf(:,-2),wf(:,9),wf(:,60))
  call prop_A_Q(wf(:,60),Q(:,13),ZERO,0_intkind1,wf(:,61))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,61),wf(:,62))
  call vert_ZQ_A(gZn,wf(:,5),wf(:,0),wf(:,63))
  call prop_Q_A(wf(:,63),Q(:,11),ZERO,0_intkind1,wf(:,64))
  call vert_QA_Z(gZn,wf(:,64),wf(:,-2),wf(:,65))
  call vert_AZ_Q(gZn,wf(:,-2),wf(:,5),wf(:,66))
  call prop_A_Q(wf(:,66),Q(:,14),ZERO,0_intkind1,wf(:,67))
  call vert_QA_Z(gZn,wf(:,0),wf(:,67),wf(:,68))
  call prop_W_W(wf(:,32),Q(:,15),MZ,1_intkind1,wf(:,69))
  call prop_W_W(wf(:,35),Q(:,15),MZ,1_intkind1,wf(:,70))
  call prop_W_W(wf(:,53),Q(:,15),MZ,1_intkind1,wf(:,71))
  call prop_W_W(wf(:,56),Q(:,15),MZ,1_intkind1,wf(:,72))
  call prop_W_W(wf(:,59),Q(:,15),MZ,1_intkind1,wf(:,73))
  call prop_W_W(wf(:,62),Q(:,15),MZ,1_intkind1,wf(:,74))
  call prop_W_W(wf(:,65),Q(:,15),MZ,1_intkind1,wf(:,75))
  call prop_W_W(wf(:,68),Q(:,15),MZ,1_intkind1,wf(:,76))

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
  den(3) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,9) - MZ2)
  den(7) = 1 / (Q(5,6) - MZ2)
  den(10) = 1 / (Q(5,15) - MH2)
  den(13) = 1 / (Q(5,21))
  den(15) = 1 / (Q(5,74))
  den(18) = 1 / (Q(5,37))
  den(21) = 1 / (Q(5,26))
  den(23) = 1 / (Q(5,69))
  den(26) = 1 / (Q(5,42))
  den(29) = 1 / (Q(5,7))
  den(31) = 1 / (Q(5,112) - MZ2)
  den(34) = 1 / (Q(5,13))
  den(39) = 1 / (Q(5,25))
  den(41) = 1 / (Q(5,70))
  den(44) = 1 / (Q(5,41))
  den(47) = 1 / (Q(5,22))
  den(49) = 1 / (Q(5,73))
  den(52) = 1 / (Q(5,38))
  den(57) = 1 / (Q(5,14))
  den(60) = 1 / (Q(5,11))
  den(69) = 1 / (Q(5,15) - MZ2)

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(8) = den(6)*den(7)
  den(9) = den(3)*den(8)
  den(11) = den(4)*den(10)
  den(12) = den(3)*den(11)
  den(14) = den(1)*den(13)
  den(16) = den(2)*den(15)
  den(17) = den(14)*den(16)
  den(19) = den(1)*den(18)
  den(20) = den(16)*den(19)
  den(22) = den(2)*den(21)
  den(24) = den(1)*den(23)
  den(25) = den(22)*den(24)
  den(27) = den(2)*den(26)
  den(28) = den(24)*den(27)
  den(30) = den(1)*den(29)
  den(32) = den(3)*den(31)
  den(33) = den(30)*den(32)
  den(35) = den(1)*den(34)
  den(36) = den(32)*den(35)
  den(37) = den(8)*den(10)
  den(38) = den(3)*den(37)
  den(40) = den(6)*den(39)
  den(42) = den(7)*den(41)
  den(43) = den(40)*den(42)
  den(45) = den(6)*den(44)
  den(46) = den(42)*den(45)
  den(48) = den(7)*den(47)
  den(50) = den(6)*den(49)
  den(51) = den(48)*den(50)
  den(53) = den(7)*den(52)
  den(54) = den(50)*den(53)
  den(55) = den(7)*den(29)
  den(56) = den(32)*den(55)
  den(58) = den(7)*den(57)
  den(59) = den(32)*den(58)
  den(61) = den(6)*den(60)
  den(62) = den(32)*den(61)
  den(63) = den(6)*den(34)
  den(64) = den(32)*den(63)
  den(65) = den(2)*den(60)
  den(66) = den(32)*den(65)
  den(67) = den(2)*den(57)
  den(68) = den(32)*den(67)
  den(70) = den(30)*den(69)
  den(71) = den(35)*den(69)
  den(72) = den(55)*den(69)
  den(73) = den(58)*den(69)
  den(74) = den(61)*den(69)
  den(75) = den(63)*den(69)
  den(76) = den(65)*den(69)
  den(77) = den(67)*den(69)
  den(78) = den(1)*den(2)*den(3)
  den(79) = den(3)*den(70)
  den(80) = den(3)*den(71)
  den(81) = den(3)*den(6)*den(7)
  den(82) = den(3)*den(72)
  den(83) = den(3)*den(73)
  den(84) = den(3)*den(74)
  den(85) = den(3)*den(75)
  den(86) = den(3)*den(76)
  den(87) = den(3)*den(77)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(20)


  A(1) = cont_VV(wf(:,3),wf(:,6)) * den(5)
  A(2) = cont_VV(wf(:,3),wf(:,11)) * den(9)
  A(3) = cont_SS(wf(:,12),wf(:,13)) * den(12)
  A(4) = cont_VV(wf(:,15),wf(:,17)) * den(17)
  A(5) = cont_VV(wf(:,15),wf(:,20)) * den(20)
  A(6) = cont_VV(wf(:,22),wf(:,24)) * den(25)
  A(7) = cont_VV(wf(:,22),wf(:,27)) * den(28)
  A(8) = cont_VV(wf(:,31),wf(:,32)) * den(33)
  A(9) = cont_VV(wf(:,31),wf(:,35)) * den(36)
  A(10) = cont_SS(wf(:,13),wf(:,36)) * den(38)
  A(11) = cont_VV(wf(:,38),wf(:,40)) * den(43)
  A(12) = cont_VV(wf(:,38),wf(:,43)) * den(46)
  A(13) = cont_VV(wf(:,45),wf(:,47)) * den(51)
  A(14) = cont_VV(wf(:,45),wf(:,50)) * den(54)
  A(15) = cont_VV(wf(:,31),wf(:,53)) * den(56)
  A(16) = cont_VV(wf(:,31),wf(:,56)) * den(59)
  A(17) = cont_VV(wf(:,31),wf(:,59)) * den(62)
  A(18) = cont_VV(wf(:,31),wf(:,62)) * den(64)
  A(19) = cont_VV(wf(:,31),wf(:,65)) * den(66)
  A(20) = cont_VV(wf(:,31),wf(:,68)) * den(68)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(20)
  complex(REALKIND), intent(out) :: M1(0), M2(1)


  M2(1) = (A(4)+A(5)+A(6)+A(7)+A(8)+A(9)-A(11)-A(12)-A(13)-A(14)-A(15)-A(16)-A(17)-A(18)+A(19)+A(20))*f(1)+(-A(1)+A(2))*f(2)+(A(3) &
       -A(10))*f(3)

end subroutine colourvectors

end module ol_loop_ppllllj2_nenenexnexuuxg_1_/**/REALKIND
