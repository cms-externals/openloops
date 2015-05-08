
module ol_colourmatrix_ppllllj2_nmnmxeexggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(0,0), K2(0,2), KL(0,2), KL2(2,2), KL2ct(2,2), KL2ct2(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  KL2(1,:) = [  7, -2]
  KL2(2,:) = [ -2,  7]
  KL2 = (1._/**/REALKIND / 3) * KL2

  KL2ct(1,:) = [  7, -2]
  KL2ct(2,:) = [ -2,  7]
  KL2ct = (1._/**/REALKIND / 3) * KL2ct

  KL2ct2(1,:) = [  7, -2]
  KL2ct2(2,:) = [ -2,  7]
  KL2ct2 = (1._/**/REALKIND / 3) * KL2ct2

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllllj2_nmnmxeexggg_1_/**/REALKIND



module ol_forced_parameters_ppllllj2_nmnmxeexggg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj2_nmnmxeexggg_1_/**/REALKIND

module ol_loop_ppllllj2_nmnmxeexggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(19), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:66)
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
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = CI*countertermnorm*eQED**4*gQCD**3
    f( 2) = countertermnorm*ctAZGG*eQED**4*gQCD**3
    f( 3) = CI*countertermnorm*ctZGG*eQED**4*gQCD**3
    f( 4) = countertermnorm*ctZZGG*eQED**4*gQCD**3
    f( 5) = (countertermnorm*ctHGG*eQED**4*gQCD**3*MW)/(cw**2*sw)
    f( 6) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f( 7) = (2*CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f( 8) = CI*eQED**4*gQCD**3*integralnorm*SwF
    f( 9) = (4*CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(10) = 2*CI*eQED**4*gQCD**3*integralnorm*SwF
    f(11) = (eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(12) = (2*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(13) = eQED**4*gQCD**3*integralnorm*SwF
    f(14) = (4*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(15) = 2*eQED**4*gQCD**3*integralnorm*SwF
    f(16) = (CI*eQED**4*gQCD**3*integralnorm*MB*SwF)/(2._/**/REALKIND*cw**2*sw**2)
    f(17) = (eQED**4*gQCD**3*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(18) = (CI*eQED**4*gQCD**3*integralnorm*MT*SwF)/(2._/**/REALKIND*cw**2*sw**2)
    f(19) = (eQED**4*gQCD**3*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)


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
  real(REALKIND),    intent(in)  :: P(0:3,7)
  integer,           intent(in)  :: H(7)
  complex(REALKIND), intent(out) :: M1(0), M2(2)
  complex(REALKIND) :: A(33)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call counter_VVG_G(wf(:,4),wf(:,2),wf(:,-6),wf(:,5))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,-3),wf(:,6))
  call prop_W_W(wf(:,6),Q(:,12),MZ,1_intkind1,wf(:,7))
  call counter_VVG_G(wf(:,4),wf(:,7),wf(:,-6),wf(:,8))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-6),Q(:,64),wf(:,9))
  call counter_VVG_G(wf(:,4),wf(:,2),wf(:,-5),wf(:,10))
  call counter_VVG_G(wf(:,4),wf(:,7),wf(:,-5),wf(:,11))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-6),Q(:,64),wf(:,12))
  call counter_VVG_G(wf(:,4),wf(:,2),wf(:,-4),wf(:,13))
  call counter_VVG_G(wf(:,4),wf(:,7),wf(:,-4),wf(:,14))
  call counter_GGG_V(ctAGGG,wf(:,-4),wf(:,-5),wf(:,-6),wf(:,15))
  call vert_ZQ_A(gZl,wf(:,4),wf(:,-2),wf(:,16))
  call vert_AV_Q(wf(:,-3),wf(:,15),wf(:,17))
  call prop_Q_A(wf(:,16),Q(:,7),ZERO,0_intkind1,wf(:,18))
  call counter_GGG_V(ctAGGG,wf(:,-4),wf(:,-6),wf(:,-5),wf(:,19))
  call vert_AV_Q(wf(:,-3),wf(:,19),wf(:,20))
  call counter_GGG_V(ctZGGG,wf(:,-4),wf(:,-5),wf(:,-6),wf(:,21))
  call prop_W_W(wf(:,21),Q(:,112),MZ,1_intkind1,wf(:,22))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,22),wf(:,23))
  call counter_GGG_V(ctZGGG,wf(:,-4),wf(:,-6),wf(:,-5),wf(:,24))
  call prop_W_W(wf(:,24),Q(:,112),MZ,1_intkind1,wf(:,25))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,25),wf(:,26))
  call vert_VQ_A(wf(:,15),wf(:,-2),wf(:,27))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,4),wf(:,28))
  call prop_Q_A(wf(:,27),Q(:,116),ZERO,0_intkind1,wf(:,29))
  call vert_VQ_A(wf(:,19),wf(:,-2),wf(:,30))
  call prop_Q_A(wf(:,30),Q(:,116),ZERO,0_intkind1,wf(:,31))
  call vert_ZQ_A(gZl,wf(:,22),wf(:,-2),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,116),ZERO,0_intkind1,wf(:,33))
  call vert_ZQ_A(gZl,wf(:,25),wf(:,-2),wf(:,34))
  call prop_Q_A(wf(:,34),Q(:,116),ZERO,0_intkind1,wf(:,35))
  call vert_ZQ_A(gZn,wf(:,7),wf(:,0),wf(:,36))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,22),wf(:,37))
  call prop_Q_A(wf(:,36),Q(:,13),ZERO,0_intkind1,wf(:,38))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,25),wf(:,39))
  call vert_ZQ_A(gZn,wf(:,22),wf(:,0),wf(:,40))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,7),wf(:,41))
  call prop_Q_A(wf(:,40),Q(:,113),ZERO,0_intkind1,wf(:,42))
  call vert_ZQ_A(gZn,wf(:,25),wf(:,0),wf(:,43))
  call prop_Q_A(wf(:,43),Q(:,113),ZERO,0_intkind1,wf(:,44))
  call vert_VV_S(wf(:,4),wf(:,7),wf(:,45))
  call counter_GG_S(wf(:,3),wf(:,-6),wf(:,46))
  call counter_GG_S(wf(:,-5),wf(:,9),wf(:,47))
  call counter_GG_S(wf(:,-4),wf(:,12),wf(:,48))
  call counter_GG_V(wf(:,3),Q(:,48),wf(:,-6),Q(:,64),wf(:,49))
  call prop_W_W(wf(:,49),Q(:,112),MZ,1_intkind1,wf(:,50))
  call vert_QA_Z(gZl,wf(:,18),wf(:,-3),wf(:,51))
  call prop_A_Q(wf(:,28),Q(:,11),ZERO,0_intkind1,wf(:,52))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,52),wf(:,53))
  call counter_GG_V(wf(:,-5),Q(:,32),wf(:,9),Q(:,80),wf(:,54))
  call prop_W_W(wf(:,54),Q(:,112),MZ,1_intkind1,wf(:,55))
  call counter_GG_V(wf(:,-4),Q(:,16),wf(:,12),Q(:,96),wf(:,56))
  call prop_W_W(wf(:,56),Q(:,112),MZ,1_intkind1,wf(:,57))
  call vert_QA_Z(gZn,wf(:,38),wf(:,-1),wf(:,58))
  call prop_A_Q(wf(:,41),Q(:,14),ZERO,0_intkind1,wf(:,59))
  call vert_QA_Z(gZn,wf(:,0),wf(:,59),wf(:,60))
  call vert_QA_V(wf(:,18),wf(:,-3),wf(:,61))
  call prop_W_W(wf(:,51),Q(:,15),MZ,1_intkind1,wf(:,62))
  call vert_QA_V(wf(:,-2),wf(:,52),wf(:,63))
  call prop_W_W(wf(:,53),Q(:,15),MZ,1_intkind1,wf(:,64))
  call prop_W_W(wf(:,58),Q(:,15),MZ,1_intkind1,wf(:,65))
  call prop_W_W(wf(:,60),Q(:,15),MZ,1_intkind1,wf(:,66))

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
  den(9) = 1 / (Q(5,80))
  den(12) = 1 / (Q(5,96))
  den(15) = 1 / (Q(5,112))
  den(16) = 1 / (Q(5,7))
  den(19) = 1 / (Q(5,112) - MZ2)
  den(21) = 1 / (Q(5,116))
  den(26) = 1 / (Q(5,13))
  den(29) = 1 / (Q(5,113))
  den(32) = 1 / (Q(5,15) - MH2)
  den(39) = 1 / (Q(5,11))
  den(49) = 1 / (Q(5,14))
  den(56) = 1 / (Q(5,15))
  den(58) = 1 / (Q(5,15) - MZ2)

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(3)*den(7)
  den(10) = den(4)*den(9)
  den(11) = den(7)*den(9)
  den(13) = den(4)*den(12)
  den(14) = den(7)*den(12)
  den(17) = den(1)*den(16)
  den(18) = den(15)*den(17)
  den(20) = den(17)*den(19)
  den(22) = den(15)*den(21)
  den(23) = den(1)*den(22)
  den(24) = den(19)*den(21)
  den(25) = den(1)*den(24)
  den(27) = den(6)*den(26)
  den(28) = den(19)*den(27)
  den(30) = den(19)*den(29)
  den(31) = den(6)*den(30)
  den(33) = den(7)*den(32)
  den(34) = den(3)*den(33)
  den(35) = den(9)*den(33)
  den(36) = den(12)*den(33)
  den(37) = den(3)*den(19)
  den(38) = den(17)*den(37)
  den(40) = den(1)*den(39)
  den(41) = den(37)*den(40)
  den(42) = den(9)*den(19)
  den(43) = den(17)*den(42)
  den(44) = den(40)*den(42)
  den(45) = den(12)*den(19)
  den(46) = den(17)*den(45)
  den(47) = den(40)*den(45)
  den(48) = den(27)*den(37)
  den(50) = den(6)*den(49)
  den(51) = den(37)*den(50)
  den(52) = den(27)*den(42)
  den(53) = den(42)*den(50)
  den(54) = den(27)*den(45)
  den(55) = den(45)*den(50)
  den(57) = den(17)*den(56)
  den(59) = den(17)*den(58)
  den(60) = den(40)*den(56)
  den(61) = den(40)*den(58)
  den(62) = den(27)*den(58)
  den(63) = den(50)*den(58)
  den(64) = den(1)*den(2)*den(3)
  den(65) = den(1)*den(3)*den(6)
  den(66) = den(1)*den(2)*den(9)
  den(67) = den(1)*den(6)*den(9)
  den(68) = den(1)*den(2)*den(12)
  den(69) = den(1)*den(6)*den(12)
  den(70) = den(3)*den(57)
  den(71) = den(3)*den(59)
  den(72) = den(3)*den(60)
  den(73) = den(3)*den(61)
  den(74) = den(9)*den(57)
  den(75) = den(9)*den(59)
  den(76) = den(9)*den(60)
  den(77) = den(9)*den(61)
  den(78) = den(12)*den(57)
  den(79) = den(12)*den(59)
  den(80) = den(12)*den(60)
  den(81) = den(12)*den(61)
  den(82) = den(3)*den(62)
  den(83) = den(3)*den(63)
  den(84) = den(9)*den(62)
  den(85) = den(9)*den(63)
  den(86) = den(12)*den(62)
  den(87) = den(12)*den(63)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(33)


  A(1) = cont_VV(wf(:,3),wf(:,5)) * den(5)
  A(2) = cont_VV(wf(:,3),wf(:,8)) * den(8)
  A(3) = cont_VV(wf(:,9),wf(:,10)) * den(10)
  A(4) = cont_VV(wf(:,9),wf(:,11)) * den(11)
  A(5) = cont_VV(wf(:,12),wf(:,13)) * den(13)
  A(6) = cont_VV(wf(:,12),wf(:,14)) * den(14)
  A(7) = cont_QA(wf(:,17),wf(:,18)) * den(18)
  A(8) = cont_QA(wf(:,18),wf(:,20)) * den(18)
  A(9) = cont_QA(wf(:,18),wf(:,23)) * den(20)
  A(10) = cont_QA(wf(:,18),wf(:,26)) * den(20)
  A(11) = cont_QA(wf(:,28),wf(:,29)) * den(23)
  A(12) = cont_QA(wf(:,28),wf(:,31)) * den(23)
  A(13) = cont_QA(wf(:,28),wf(:,33)) * den(25)
  A(14) = cont_QA(wf(:,28),wf(:,35)) * den(25)
  A(15) = cont_QA(wf(:,37),wf(:,38)) * den(28)
  A(16) = cont_QA(wf(:,38),wf(:,39)) * den(28)
  A(17) = cont_QA(wf(:,41),wf(:,42)) * den(31)
  A(18) = cont_QA(wf(:,41),wf(:,44)) * den(31)
  A(19) = cont_SS(wf(:,45),wf(:,46)) * den(34)
  A(20) = cont_SS(wf(:,45),wf(:,47)) * den(35)
  A(21) = cont_SS(wf(:,45),wf(:,48)) * den(36)
  A(22) = cont_VV(wf(:,50),wf(:,51)) * den(38)
  A(23) = cont_VV(wf(:,50),wf(:,53)) * den(41)
  A(24) = cont_VV(wf(:,51),wf(:,55)) * den(43)
  A(25) = cont_VV(wf(:,53),wf(:,55)) * den(44)
  A(26) = cont_VV(wf(:,51),wf(:,57)) * den(46)
  A(27) = cont_VV(wf(:,53),wf(:,57)) * den(47)
  A(28) = cont_VV(wf(:,50),wf(:,58)) * den(48)
  A(29) = cont_VV(wf(:,50),wf(:,60)) * den(51)
  A(30) = cont_VV(wf(:,55),wf(:,58)) * den(52)
  A(31) = cont_VV(wf(:,55),wf(:,60)) * den(53)
  A(32) = cont_VV(wf(:,57),wf(:,58)) * den(54)
  A(33) = cont_VV(wf(:,57),wf(:,60)) * den(55)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(33)
  complex(REALKIND), intent(out) :: M1(0), M2(2)


  M2(1) = (A(7)+A(9)+A(11)+A(13)+A(15)+A(17))*f(1)+2*CI*(A(1)-A(3)+A(5))*f(2)+2*CI*(A(22)+A(23)-A(24)-A(25)+A(26)+A(27)+A(28) &
       +A(29)-A(30)-A(31)+A(32)+A(33))*f(3)+2*CI*(A(2)-A(4)+A(6))*f(4)+2*CI*(-A(19)+A(20)-A(21))*f(5)
  M2(2) = (A(8)+A(10)+A(12)+A(14)+A(16)+A(18))*f(1)+2*CI*(-A(1)+A(3)-A(5))*f(2)+2*CI*(-A(22)-A(23)+A(24)+A(25)-A(26)-A(27)-A(28) &
       -A(29)+A(30)+A(31)-A(32)-A(33))*f(3)+2*CI*(-A(2)+A(4)-A(6))*f(4)+2*CI*(A(19)-A(20)+A(21))*f(5)

end subroutine colourvectors

end module ol_loop_ppllllj2_nmnmxeexggg_1_/**/REALKIND
