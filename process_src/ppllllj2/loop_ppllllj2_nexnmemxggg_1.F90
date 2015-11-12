
module ol_colourmatrix_ppllllj2_nexnmemxggg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllllj2_nexnmemxggg_1_/**/REALKIND



module ol_forced_parameters_ppllllj2_nexnmemxggg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj2_nexnmemxggg_1_/**/REALKIND

module ol_loop_ppllllj2_nexnmemxggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(28), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:62)
  ! denominators
  complex(REALKIND), save :: den(88)
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
    f( 1) = (CI*countertermnorm*cw*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**3)
    f( 2) = (CI*countertermnorm*ctZGG*cw*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**3)
    f( 3) = (countertermnorm*ctHGG*eQED**4*gQCD**3*MW)/(sw**3*2._/**/REALKIND)
    f( 4) = (CI*countertermnorm*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f( 5) = (countertermnorm*ctWWGG*eQED**4*gQCD**3)/(sw**2*2._/**/REALKIND)
    f( 6) = (CI*countertermnorm*ctZGG*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f( 7) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/(4._/**/REALKIND*sw**4)
    f( 8) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/(2._/**/REALKIND*sw**4)
    f( 9) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**4*4._/**/REALKIND)
    f(10) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**4*2._/**/REALKIND)
    f(11) = (CI*eQED**4*gQCD**3*integralnorm*MB*SwF)/(4._/**/REALKIND*sw**4)
    f(12) = (eQED**4*gQCD**3*integralnorm*MB*SwF)/(sw**4*4._/**/REALKIND)
    f(13) = (CI*eQED**4*gQCD**3*integralnorm*MT*SwF)/(4._/**/REALKIND*sw**4)
    f(14) = (eQED**4*gQCD**3*integralnorm*MT*SwF)/(sw**4*4._/**/REALKIND)
    f(15) = (CI*cw*eQED**4*gQCD**3*integralnorm*SwF)/(2._/**/REALKIND*sw**3)
    f(16) = (CI*cw*eQED**4*gQCD**3*integralnorm*SwF)/sw**3
    f(17) = (cw*eQED**4*gQCD**3*integralnorm*SwF)/(sw**3*2._/**/REALKIND)
    f(18) = (cw*eQED**4*gQCD**3*integralnorm*SwF)/sw**3
    f(19) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/(6._/**/REALKIND*sw**2)
    f(20) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/(3._/**/REALKIND*sw**2)
    f(21) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/(2._/**/REALKIND*sw**2)
    f(22) = (2*CI*eQED**4*gQCD**3*integralnorm*SwF)/(3._/**/REALKIND*sw**2)
    f(23) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/sw**2
    f(24) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*6._/**/REALKIND)
    f(25) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(26) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(27) = (2*eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(28) = (eQED**4*gQCD**3*integralnorm*SwF)/sw**2


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
  complex(REALKIND) :: A(37)
  ! external WFs
  call wf_A(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_W(wf(:,-2),wf(:,0),wf(:,1))
  call vert_QA_W(wf(:,-1),wf(:,-3),wf(:,2))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,5),MW,1_intkind1,wf(:,4))
  call prop_W_W(wf(:,2),Q(:,10),MW,1_intkind1,wf(:,5))
  call counter_VVG_G(wf(:,4),wf(:,5),wf(:,-6),wf(:,6))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-6),Q(:,64),wf(:,7))
  call counter_VVG_G(wf(:,4),wf(:,5),wf(:,-5),wf(:,8))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-6),Q(:,64),wf(:,9))
  call counter_VVG_G(wf(:,4),wf(:,5),wf(:,-4),wf(:,10))
  call counter_GGG_V(ctAGGG,wf(:,-4),wf(:,-5),wf(:,-6),wf(:,11))
  call vert_UV_W(wf(:,5),Q(:,10),wf(:,4),Q(:,5),wf(:,12))
  call counter_GGG_V(ctAGGG,wf(:,-4),wf(:,-6),wf(:,-5),wf(:,13))
  call counter_GGG_V(ctZGGG,wf(:,-4),wf(:,-5),wf(:,-6),wf(:,14))
  call prop_W_W(wf(:,14),Q(:,112),MZ,1_intkind1,wf(:,15))
  call counter_GGG_V(ctZGGG,wf(:,-4),wf(:,-6),wf(:,-5),wf(:,16))
  call prop_W_W(wf(:,16),Q(:,112),MZ,1_intkind1,wf(:,17))
  call vert_WQ_A(wf(:,4),wf(:,-1),wf(:,18))
  call vert_AV_Q(wf(:,-3),wf(:,11),wf(:,19))
  call prop_Q_A(wf(:,18),Q(:,7),ZERO,0_intkind1,wf(:,20))
  call vert_AV_Q(wf(:,-3),wf(:,13),wf(:,21))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,15),wf(:,22))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,17),wf(:,23))
  call vert_ZQ_A(gZn,wf(:,15),wf(:,-1),wf(:,24))
  call vert_AW_Q(wf(:,-3),wf(:,4),wf(:,25))
  call prop_Q_A(wf(:,24),Q(:,114),ZERO,0_intkind1,wf(:,26))
  call vert_ZQ_A(gZn,wf(:,17),wf(:,-1),wf(:,27))
  call prop_Q_A(wf(:,27),Q(:,114),ZERO,0_intkind1,wf(:,28))
  call vert_AW_Q(wf(:,0),wf(:,5),wf(:,29))
  call vert_VQ_A(wf(:,11),wf(:,-2),wf(:,30))
  call prop_A_Q(wf(:,29),Q(:,11),ZERO,0_intkind1,wf(:,31))
  call vert_VQ_A(wf(:,13),wf(:,-2),wf(:,32))
  call vert_ZQ_A(gZl,wf(:,15),wf(:,-2),wf(:,33))
  call vert_ZQ_A(gZl,wf(:,17),wf(:,-2),wf(:,34))
  call vert_AZ_Q(gZn,wf(:,0),wf(:,15),wf(:,35))
  call vert_WQ_A(wf(:,5),wf(:,-2),wf(:,36))
  call prop_A_Q(wf(:,35),Q(:,113),ZERO,0_intkind1,wf(:,37))
  call vert_AZ_Q(gZn,wf(:,0),wf(:,17),wf(:,38))
  call prop_A_Q(wf(:,38),Q(:,113),ZERO,0_intkind1,wf(:,39))
  call vert_VV_S(wf(:,4),wf(:,5),wf(:,40))
  call counter_GG_S(wf(:,3),wf(:,-6),wf(:,41))
  call counter_GG_V(wf(:,3),Q(:,48),wf(:,-6),Q(:,64),wf(:,42))
  call prop_W_W(wf(:,12),Q(:,15),MZ,1_intkind1,wf(:,43))
  call counter_GG_S(wf(:,-5),wf(:,7),wf(:,44))
  call counter_GG_V(wf(:,-5),Q(:,32),wf(:,7),Q(:,80),wf(:,45))
  call counter_GG_S(wf(:,-4),wf(:,9),wf(:,46))
  call counter_GG_V(wf(:,-4),Q(:,16),wf(:,9),Q(:,96),wf(:,47))
  call prop_W_W(wf(:,42),Q(:,112),MZ,1_intkind1,wf(:,48))
  call vert_QA_Z(gZl,wf(:,20),wf(:,-3),wf(:,49))
  call prop_A_Q(wf(:,25),Q(:,13),ZERO,0_intkind1,wf(:,50))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,50),wf(:,51))
  call prop_W_W(wf(:,45),Q(:,112),MZ,1_intkind1,wf(:,52))
  call prop_W_W(wf(:,47),Q(:,112),MZ,1_intkind1,wf(:,53))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,31),wf(:,54))
  call prop_Q_A(wf(:,36),Q(:,14),ZERO,0_intkind1,wf(:,55))
  call vert_QA_Z(gZn,wf(:,55),wf(:,0),wf(:,56))
  call vert_QA_V(wf(:,20),wf(:,-3),wf(:,57))
  call prop_W_W(wf(:,49),Q(:,15),MZ,1_intkind1,wf(:,58))
  call prop_W_W(wf(:,51),Q(:,15),MZ,1_intkind1,wf(:,59))
  call vert_QA_V(wf(:,-2),wf(:,31),wf(:,60))
  call prop_W_W(wf(:,54),Q(:,15),MZ,1_intkind1,wf(:,61))
  call prop_W_W(wf(:,56),Q(:,15),MZ,1_intkind1,wf(:,62))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5) - MW2)
  den(2) = 1 / (Q(5,10) - MW2)
  den(3) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,80))
  den(8) = 1 / (Q(5,96))
  den(10) = 1 / (Q(5,112))
  den(12) = 1 / (Q(5,112) - MZ2)
  den(14) = 1 / (Q(5,7))
  den(18) = 1 / (Q(5,114))
  den(21) = 1 / (Q(5,11))
  den(25) = 1 / (Q(5,113))
  den(28) = 1 / (Q(5,15) - MH2)
  den(31) = 1 / (Q(5,15) - MZ2)
  den(40) = 1 / (Q(5,13))
  den(50) = 1 / (Q(5,14))
  den(57) = 1 / (Q(5,15))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(4)*den(6)
  den(9) = den(4)*den(8)
  den(11) = den(4)*den(10)
  den(13) = den(4)*den(12)
  den(15) = den(1)*den(14)
  den(16) = den(10)*den(15)
  den(17) = den(12)*den(15)
  den(19) = den(12)*den(18)
  den(20) = den(1)*den(19)
  den(22) = den(2)*den(21)
  den(23) = den(10)*den(22)
  den(24) = den(12)*den(22)
  den(26) = den(12)*den(25)
  den(27) = den(2)*den(26)
  den(29) = den(4)*den(28)
  den(30) = den(3)*den(29)
  den(32) = den(4)*den(31)
  den(33) = den(3)*den(32)
  den(34) = den(6)*den(29)
  den(35) = den(6)*den(32)
  den(36) = den(8)*den(29)
  den(37) = den(8)*den(32)
  den(38) = den(3)*den(12)
  den(39) = den(15)*den(38)
  den(41) = den(1)*den(40)
  den(42) = den(38)*den(41)
  den(43) = den(6)*den(12)
  den(44) = den(15)*den(43)
  den(45) = den(41)*den(43)
  den(46) = den(8)*den(12)
  den(47) = den(15)*den(46)
  den(48) = den(41)*den(46)
  den(49) = den(22)*den(38)
  den(51) = den(2)*den(50)
  den(52) = den(38)*den(51)
  den(53) = den(22)*den(43)
  den(54) = den(43)*den(51)
  den(55) = den(22)*den(46)
  den(56) = den(46)*den(51)
  den(58) = den(4)*den(57)
  den(59) = den(15)*den(57)
  den(60) = den(15)*den(31)
  den(61) = den(31)*den(41)
  den(62) = den(22)*den(57)
  den(63) = den(22)*den(31)
  den(64) = den(31)*den(51)
  den(65) = den(3)*den(58)
  den(66) = den(1)*den(2)*den(3)
  den(67) = den(6)*den(58)
  den(68) = den(1)*den(2)*den(6)
  den(69) = den(8)*den(58)
  den(70) = den(1)*den(2)*den(8)
  den(71) = den(3)*den(59)
  den(72) = den(3)*den(60)
  den(73) = den(3)*den(61)
  den(74) = den(6)*den(59)
  den(75) = den(6)*den(60)
  den(76) = den(6)*den(61)
  den(77) = den(8)*den(59)
  den(78) = den(8)*den(60)
  den(79) = den(8)*den(61)
  den(80) = den(3)*den(62)
  den(81) = den(3)*den(63)
  den(82) = den(3)*den(64)
  den(83) = den(6)*den(62)
  den(84) = den(6)*den(63)
  den(85) = den(6)*den(64)
  den(86) = den(8)*den(62)
  den(87) = den(8)*den(63)
  den(88) = den(8)*den(64)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(37)


  A(1) = cont_VV(wf(:,3),wf(:,6)) * den(5)
  A(2) = cont_VV(wf(:,7),wf(:,8)) * den(7)
  A(3) = cont_VV(wf(:,9),wf(:,10)) * den(9)
  A(4) = cont_VV(wf(:,11),wf(:,12)) * den(11)
  A(5) = cont_VV(wf(:,12),wf(:,13)) * den(11)
  A(6) = cont_VV(wf(:,12),wf(:,15)) * den(13)
  A(7) = cont_VV(wf(:,12),wf(:,17)) * den(13)
  A(8) = cont_QA(wf(:,19),wf(:,20)) * den(16)
  A(9) = cont_QA(wf(:,20),wf(:,21)) * den(16)
  A(10) = cont_QA(wf(:,20),wf(:,22)) * den(17)
  A(11) = cont_QA(wf(:,20),wf(:,23)) * den(17)
  A(12) = cont_QA(wf(:,25),wf(:,26)) * den(20)
  A(13) = cont_QA(wf(:,25),wf(:,28)) * den(20)
  A(14) = cont_QA(wf(:,30),wf(:,31)) * den(23)
  A(15) = cont_QA(wf(:,31),wf(:,32)) * den(23)
  A(16) = cont_QA(wf(:,31),wf(:,33)) * den(24)
  A(17) = cont_QA(wf(:,31),wf(:,34)) * den(24)
  A(18) = cont_QA(wf(:,36),wf(:,37)) * den(27)
  A(19) = cont_QA(wf(:,36),wf(:,39)) * den(27)
  A(20) = cont_SS(wf(:,40),wf(:,41)) * den(30)
  A(21) = cont_VV(wf(:,42),wf(:,43)) * den(33)
  A(22) = cont_SS(wf(:,40),wf(:,44)) * den(34)
  A(23) = cont_VV(wf(:,43),wf(:,45)) * den(35)
  A(24) = cont_SS(wf(:,40),wf(:,46)) * den(36)
  A(25) = cont_VV(wf(:,43),wf(:,47)) * den(37)
  A(26) = cont_VV(wf(:,48),wf(:,49)) * den(39)
  A(27) = cont_VV(wf(:,48),wf(:,51)) * den(42)
  A(28) = cont_VV(wf(:,49),wf(:,52)) * den(44)
  A(29) = cont_VV(wf(:,51),wf(:,52)) * den(45)
  A(30) = cont_VV(wf(:,49),wf(:,53)) * den(47)
  A(31) = cont_VV(wf(:,51),wf(:,53)) * den(48)
  A(32) = cont_VV(wf(:,48),wf(:,54)) * den(49)
  A(33) = cont_VV(wf(:,48),wf(:,56)) * den(52)
  A(34) = cont_VV(wf(:,52),wf(:,54)) * den(53)
  A(35) = cont_VV(wf(:,52),wf(:,56)) * den(54)
  A(36) = cont_VV(wf(:,53),wf(:,54)) * den(55)
  A(37) = cont_VV(wf(:,53),wf(:,56)) * den(56)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(37)
  complex(REALKIND), intent(out) :: M1(0), M2(2)


  M2(1) = A(6)*f(1)+2*CI*(A(21)-A(23)+A(25))*f(2)+2*CI*(-A(20)+A(22)-A(24))*f(3)+(-A(4)+A(8)+A(10)+A(12)+A(14)+A(16)+A(18))*f(4) &
       +2*CI*(A(1)-A(2)+A(3))*f(5)+2*CI*(A(26)+A(27)-A(28)-A(29)+A(30)+A(31)+A(32)+A(33)-A(34)-A(35)+A(36)+A(37))*f(6)
  M2(2) = A(7)*f(1)+2*CI*(-A(21)+A(23)-A(25))*f(2)+2*CI*(A(20)-A(22)+A(24))*f(3)+(-A(5)+A(9)+A(11)+A(13)+A(15)+A(17)+A(19))*f(4) &
       +2*CI*(-A(1)+A(2)-A(3))*f(5)+2*CI*(-A(26)-A(27)+A(28)+A(29)-A(30)-A(31)-A(32)-A(33)+A(34)+A(35)-A(36)-A(37))*f(6)

end subroutine colourvectors

end module ol_loop_ppllllj2_nexnmemxggg_1_/**/REALKIND
