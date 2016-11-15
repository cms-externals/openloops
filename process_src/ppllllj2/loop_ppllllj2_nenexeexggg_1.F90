
module ol_colourmatrix_ppllllj2_nenexeexggg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllllj2_nenexeexggg_1_/**/REALKIND



module ol_forced_parameters_ppllllj2_nenexeexggg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj2_nenexeexggg_1_/**/REALKIND

module ol_loop_ppllllj2_nenexeexggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(47), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:96)
  ! denominators
  complex(REALKIND), save :: den(158)
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
    f( 1) = CI*countertermnorm*eQED**4*gQCD**3
    f( 2) = countertermnorm*ctAZGG*eQED**4*gQCD**3
    f( 3) = CI*countertermnorm*ctZGG*eQED**4*gQCD**3
    f( 4) = countertermnorm*ctZZGG*eQED**4*gQCD**3
    f( 5) = (CI*countertermnorm*cw*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**3)
    f( 6) = (CI*countertermnorm*ctZGG*cw*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**3)
    f( 7) = (countertermnorm*ctHGG*eQED**4*gQCD**3*MW)/(sw**3*2._/**/REALKIND)
    f( 8) = (CI*countertermnorm*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f( 9) = (countertermnorm*ctWWGG*eQED**4*gQCD**3)/(sw**2*2._/**/REALKIND)
    f(10) = (CI*countertermnorm*ctZGG*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(11) = (countertermnorm*ctHGG*eQED**4*gQCD**3*MW)/(cw**2*sw)
    f(12) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(13) = (2*CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(14) = CI*eQED**4*gQCD**3*integralnorm*SwF
    f(15) = (4*CI*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(16) = 2*CI*eQED**4*gQCD**3*integralnorm*SwF
    f(17) = (eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(18) = (2*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(19) = eQED**4*gQCD**3*integralnorm*SwF
    f(20) = (4*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(21) = 2*eQED**4*gQCD**3*integralnorm*SwF
    f(22) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/(4._/**/REALKIND*sw**4)
    f(23) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/(2._/**/REALKIND*sw**4)
    f(24) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**4*4._/**/REALKIND)
    f(25) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**4*2._/**/REALKIND)
    f(26) = (CI*eQED**4*gQCD**3*integralnorm*MB*SwF)/(4._/**/REALKIND*sw**4)
    f(27) = (eQED**4*gQCD**3*integralnorm*MB*SwF)/(sw**4*4._/**/REALKIND)
    f(28) = (CI*eQED**4*gQCD**3*integralnorm*MT*SwF)/(4._/**/REALKIND*sw**4)
    f(29) = (eQED**4*gQCD**3*integralnorm*MT*SwF)/(sw**4*4._/**/REALKIND)
    f(30) = (CI*cw*eQED**4*gQCD**3*integralnorm*SwF)/(2._/**/REALKIND*sw**3)
    f(31) = (CI*cw*eQED**4*gQCD**3*integralnorm*SwF)/sw**3
    f(32) = (cw*eQED**4*gQCD**3*integralnorm*SwF)/(sw**3*2._/**/REALKIND)
    f(33) = (cw*eQED**4*gQCD**3*integralnorm*SwF)/sw**3
    f(34) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/(6._/**/REALKIND*sw**2)
    f(35) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/(3._/**/REALKIND*sw**2)
    f(36) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/(2._/**/REALKIND*sw**2)
    f(37) = (2*CI*eQED**4*gQCD**3*integralnorm*SwF)/(3._/**/REALKIND*sw**2)
    f(38) = (CI*eQED**4*gQCD**3*integralnorm*SwF)/sw**2
    f(39) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*6._/**/REALKIND)
    f(40) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(41) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(42) = (2*eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(43) = (eQED**4*gQCD**3*integralnorm*SwF)/sw**2
    f(44) = (CI*eQED**4*gQCD**3*integralnorm*MB*SwF)/(2._/**/REALKIND*cw**2*sw**2)
    f(45) = (eQED**4*gQCD**3*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(46) = (CI*eQED**4*gQCD**3*integralnorm*MT*SwF)/(2._/**/REALKIND*cw**2*sw**2)
    f(47) = (eQED**4*gQCD**3*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)


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
  complex(REALKIND), intent(out) :: M1(0), M2(2)
  complex(REALKIND) :: A(70)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))
    call pol_wf_V(P(:,7), rZERO, H(7), wf(:,-6), POLSEL(7))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), 0)
    call pol_wf_V(P(:,7), rZERO, H(7), wf(:,-6), 0)

  end if

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
  call vert_QA_W(wf(:,0),wf(:,-3),wf(:,36))
  call vert_QA_W(wf(:,-2),wf(:,-1),wf(:,37))
  call prop_W_W(wf(:,36),Q(:,9),MW,1_intkind1,wf(:,38))
  call prop_W_W(wf(:,37),Q(:,6),MW,1_intkind1,wf(:,39))
  call counter_VVG_G(wf(:,39),wf(:,38),wf(:,-6),wf(:,40))
  call counter_VVG_G(wf(:,39),wf(:,38),wf(:,-5),wf(:,41))
  call counter_VVG_G(wf(:,39),wf(:,38),wf(:,-4),wf(:,42))
  call vert_UV_W(wf(:,38),Q(:,9),wf(:,39),Q(:,6),wf(:,43))
  call vert_WQ_A(wf(:,39),wf(:,0),wf(:,44))
  call prop_Q_A(wf(:,44),Q(:,7),ZERO,0_intkind1,wf(:,45))
  call vert_ZQ_A(gZn,wf(:,22),wf(:,0),wf(:,46))
  call vert_AW_Q(wf(:,-3),wf(:,39),wf(:,47))
  call prop_Q_A(wf(:,46),Q(:,113),ZERO,0_intkind1,wf(:,48))
  call vert_ZQ_A(gZn,wf(:,25),wf(:,0),wf(:,49))
  call prop_Q_A(wf(:,49),Q(:,113),ZERO,0_intkind1,wf(:,50))
  call vert_AW_Q(wf(:,-1),wf(:,38),wf(:,51))
  call prop_A_Q(wf(:,51),Q(:,11),ZERO,0_intkind1,wf(:,52))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,22),wf(:,53))
  call vert_WQ_A(wf(:,38),wf(:,-2),wf(:,54))
  call prop_A_Q(wf(:,53),Q(:,114),ZERO,0_intkind1,wf(:,55))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,25),wf(:,56))
  call prop_A_Q(wf(:,56),Q(:,114),ZERO,0_intkind1,wf(:,57))
  call vert_ZQ_A(gZn,wf(:,7),wf(:,0),wf(:,58))
  call prop_Q_A(wf(:,58),Q(:,13),ZERO,0_intkind1,wf(:,59))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,7),wf(:,60))
  call vert_VV_S(wf(:,4),wf(:,7),wf(:,61))
  call counter_GG_S(wf(:,3),wf(:,-6),wf(:,62))
  call counter_GG_S(wf(:,-5),wf(:,9),wf(:,63))
  call counter_GG_S(wf(:,-4),wf(:,12),wf(:,64))
  call counter_GG_V(wf(:,3),Q(:,48),wf(:,-6),Q(:,64),wf(:,65))
  call prop_W_W(wf(:,65),Q(:,112),MZ,1_intkind1,wf(:,66))
  call vert_QA_Z(gZl,wf(:,18),wf(:,-3),wf(:,67))
  call prop_A_Q(wf(:,28),Q(:,11),ZERO,0_intkind1,wf(:,68))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,68),wf(:,69))
  call counter_GG_V(wf(:,-5),Q(:,32),wf(:,9),Q(:,80),wf(:,70))
  call prop_W_W(wf(:,70),Q(:,112),MZ,1_intkind1,wf(:,71))
  call counter_GG_V(wf(:,-4),Q(:,16),wf(:,12),Q(:,96),wf(:,72))
  call prop_W_W(wf(:,72),Q(:,112),MZ,1_intkind1,wf(:,73))
  call vert_VV_S(wf(:,39),wf(:,38),wf(:,74))
  call prop_W_W(wf(:,43),Q(:,15),MZ,1_intkind1,wf(:,75))
  call vert_QA_Z(gZl,wf(:,45),wf(:,-3),wf(:,76))
  call prop_A_Q(wf(:,47),Q(:,14),ZERO,0_intkind1,wf(:,77))
  call vert_QA_Z(gZn,wf(:,0),wf(:,77),wf(:,78))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,52),wf(:,79))
  call prop_Q_A(wf(:,54),Q(:,13),ZERO,0_intkind1,wf(:,80))
  call vert_QA_Z(gZn,wf(:,80),wf(:,-1),wf(:,81))
  call vert_QA_Z(gZn,wf(:,59),wf(:,-1),wf(:,82))
  call prop_A_Q(wf(:,60),Q(:,14),ZERO,0_intkind1,wf(:,83))
  call vert_QA_Z(gZn,wf(:,0),wf(:,83),wf(:,84))
  call vert_QA_V(wf(:,18),wf(:,-3),wf(:,85))
  call prop_W_W(wf(:,67),Q(:,15),MZ,1_intkind1,wf(:,86))
  call vert_QA_V(wf(:,-2),wf(:,68),wf(:,87))
  call prop_W_W(wf(:,69),Q(:,15),MZ,1_intkind1,wf(:,88))
  call vert_QA_V(wf(:,45),wf(:,-3),wf(:,89))
  call prop_W_W(wf(:,76),Q(:,15),MZ,1_intkind1,wf(:,90))
  call prop_W_W(wf(:,78),Q(:,15),MZ,1_intkind1,wf(:,91))
  call vert_QA_V(wf(:,-2),wf(:,52),wf(:,92))
  call prop_W_W(wf(:,79),Q(:,15),MZ,1_intkind1,wf(:,93))
  call prop_W_W(wf(:,81),Q(:,15),MZ,1_intkind1,wf(:,94))
  call prop_W_W(wf(:,82),Q(:,15),MZ,1_intkind1,wf(:,95))
  call prop_W_W(wf(:,84),Q(:,15),MZ,1_intkind1,wf(:,96))

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
  den(26) = 1 / (Q(5,9) - MW2)
  den(27) = 1 / (Q(5,6) - MW2)
  den(37) = 1 / (Q(5,113))
  den(40) = 1 / (Q(5,11))
  den(44) = 1 / (Q(5,114))
  den(47) = 1 / (Q(5,13))
  den(51) = 1 / (Q(5,15) - MH2)
  den(68) = 1 / (Q(5,15) - MZ2)
  den(76) = 1 / (Q(5,14))
  den(97) = 1 / (Q(5,15))

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
  den(28) = den(26)*den(27)
  den(29) = den(3)*den(28)
  den(30) = den(9)*den(28)
  den(31) = den(12)*den(28)
  den(32) = den(15)*den(28)
  den(33) = den(19)*den(28)
  den(34) = den(16)*den(27)
  den(35) = den(15)*den(34)
  den(36) = den(19)*den(34)
  den(38) = den(19)*den(37)
  den(39) = den(27)*den(38)
  den(41) = den(26)*den(40)
  den(42) = den(15)*den(41)
  den(43) = den(19)*den(41)
  den(45) = den(19)*den(44)
  den(46) = den(26)*den(45)
  den(48) = den(6)*den(47)
  den(49) = den(19)*den(48)
  den(50) = den(6)*den(38)
  den(52) = den(7)*den(51)
  den(53) = den(3)*den(52)
  den(54) = den(9)*den(52)
  den(55) = den(12)*den(52)
  den(56) = den(3)*den(19)
  den(57) = den(17)*den(56)
  den(58) = den(1)*den(40)
  den(59) = den(56)*den(58)
  den(60) = den(9)*den(19)
  den(61) = den(17)*den(60)
  den(62) = den(58)*den(60)
  den(63) = den(12)*den(19)
  den(64) = den(17)*den(63)
  den(65) = den(58)*den(63)
  den(66) = den(28)*den(51)
  den(67) = den(3)*den(66)
  den(69) = den(28)*den(68)
  den(70) = den(3)*den(69)
  den(71) = den(9)*den(66)
  den(72) = den(9)*den(69)
  den(73) = den(12)*den(66)
  den(74) = den(12)*den(69)
  den(75) = den(34)*den(56)
  den(77) = den(27)*den(76)
  den(78) = den(56)*den(77)
  den(79) = den(34)*den(60)
  den(80) = den(60)*den(77)
  den(81) = den(34)*den(63)
  den(82) = den(63)*den(77)
  den(83) = den(41)*den(56)
  den(84) = den(26)*den(47)
  den(85) = den(56)*den(84)
  den(86) = den(41)*den(60)
  den(87) = den(60)*den(84)
  den(88) = den(41)*den(63)
  den(89) = den(63)*den(84)
  den(90) = den(48)*den(56)
  den(91) = den(6)*den(76)
  den(92) = den(56)*den(91)
  den(93) = den(48)*den(60)
  den(94) = den(60)*den(91)
  den(95) = den(48)*den(63)
  den(96) = den(63)*den(91)
  den(98) = den(17)*den(97)
  den(99) = den(17)*den(68)
  den(100) = den(58)*den(97)
  den(101) = den(58)*den(68)
  den(102) = den(28)*den(97)
  den(103) = den(34)*den(97)
  den(104) = den(34)*den(68)
  den(105) = den(68)*den(77)
  den(106) = den(41)*den(97)
  den(107) = den(41)*den(68)
  den(108) = den(68)*den(84)
  den(109) = den(48)*den(68)
  den(110) = den(68)*den(91)
  den(111) = den(1)*den(2)*den(3)
  den(112) = den(1)*den(3)*den(6)
  den(113) = den(1)*den(2)*den(9)
  den(114) = den(1)*den(6)*den(9)
  den(115) = den(1)*den(2)*den(12)
  den(116) = den(1)*den(6)*den(12)
  den(117) = den(3)*den(98)
  den(118) = den(3)*den(99)
  den(119) = den(3)*den(100)
  den(120) = den(3)*den(101)
  den(121) = den(9)*den(98)
  den(122) = den(9)*den(99)
  den(123) = den(9)*den(100)
  den(124) = den(9)*den(101)
  den(125) = den(12)*den(98)
  den(126) = den(12)*den(99)
  den(127) = den(12)*den(100)
  den(128) = den(12)*den(101)
  den(129) = den(3)*den(102)
  den(130) = den(3)*den(26)*den(27)
  den(131) = den(9)*den(102)
  den(132) = den(9)*den(26)*den(27)
  den(133) = den(12)*den(102)
  den(134) = den(12)*den(26)*den(27)
  den(135) = den(3)*den(103)
  den(136) = den(3)*den(104)
  den(137) = den(3)*den(105)
  den(138) = den(9)*den(103)
  den(139) = den(9)*den(104)
  den(140) = den(9)*den(105)
  den(141) = den(12)*den(103)
  den(142) = den(12)*den(104)
  den(143) = den(12)*den(105)
  den(144) = den(3)*den(106)
  den(145) = den(3)*den(107)
  den(146) = den(3)*den(108)
  den(147) = den(9)*den(106)
  den(148) = den(9)*den(107)
  den(149) = den(9)*den(108)
  den(150) = den(12)*den(106)
  den(151) = den(12)*den(107)
  den(152) = den(12)*den(108)
  den(153) = den(3)*den(109)
  den(154) = den(3)*den(110)
  den(155) = den(9)*den(109)
  den(156) = den(9)*den(110)
  den(157) = den(12)*den(109)
  den(158) = den(12)*den(110)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(70)


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
  A(15) = cont_VV(wf(:,3),wf(:,40)) * den(29)
  A(16) = cont_VV(wf(:,9),wf(:,41)) * den(30)
  A(17) = cont_VV(wf(:,12),wf(:,42)) * den(31)
  A(18) = cont_VV(wf(:,15),wf(:,43)) * den(32)
  A(19) = cont_VV(wf(:,19),wf(:,43)) * den(32)
  A(20) = cont_VV(wf(:,22),wf(:,43)) * den(33)
  A(21) = cont_VV(wf(:,25),wf(:,43)) * den(33)
  A(22) = cont_QA(wf(:,17),wf(:,45)) * den(35)
  A(23) = cont_QA(wf(:,20),wf(:,45)) * den(35)
  A(24) = cont_QA(wf(:,23),wf(:,45)) * den(36)
  A(25) = cont_QA(wf(:,26),wf(:,45)) * den(36)
  A(26) = cont_QA(wf(:,47),wf(:,48)) * den(39)
  A(27) = cont_QA(wf(:,47),wf(:,50)) * den(39)
  A(28) = cont_QA(wf(:,27),wf(:,52)) * den(42)
  A(29) = cont_QA(wf(:,30),wf(:,52)) * den(42)
  A(30) = cont_QA(wf(:,32),wf(:,52)) * den(43)
  A(31) = cont_QA(wf(:,34),wf(:,52)) * den(43)
  A(32) = cont_QA(wf(:,54),wf(:,55)) * den(46)
  A(33) = cont_QA(wf(:,54),wf(:,57)) * den(46)
  A(34) = cont_QA(wf(:,53),wf(:,59)) * den(49)
  A(35) = cont_QA(wf(:,56),wf(:,59)) * den(49)
  A(36) = cont_QA(wf(:,48),wf(:,60)) * den(50)
  A(37) = cont_QA(wf(:,50),wf(:,60)) * den(50)
  A(38) = cont_SS(wf(:,61),wf(:,62)) * den(53)
  A(39) = cont_SS(wf(:,61),wf(:,63)) * den(54)
  A(40) = cont_SS(wf(:,61),wf(:,64)) * den(55)
  A(41) = cont_VV(wf(:,66),wf(:,67)) * den(57)
  A(42) = cont_VV(wf(:,66),wf(:,69)) * den(59)
  A(43) = cont_VV(wf(:,67),wf(:,71)) * den(61)
  A(44) = cont_VV(wf(:,69),wf(:,71)) * den(62)
  A(45) = cont_VV(wf(:,67),wf(:,73)) * den(64)
  A(46) = cont_VV(wf(:,69),wf(:,73)) * den(65)
  A(47) = cont_SS(wf(:,62),wf(:,74)) * den(67)
  A(48) = cont_VV(wf(:,65),wf(:,75)) * den(70)
  A(49) = cont_SS(wf(:,63),wf(:,74)) * den(71)
  A(50) = cont_VV(wf(:,70),wf(:,75)) * den(72)
  A(51) = cont_SS(wf(:,64),wf(:,74)) * den(73)
  A(52) = cont_VV(wf(:,72),wf(:,75)) * den(74)
  A(53) = cont_VV(wf(:,66),wf(:,76)) * den(75)
  A(54) = cont_VV(wf(:,66),wf(:,78)) * den(78)
  A(55) = cont_VV(wf(:,71),wf(:,76)) * den(79)
  A(56) = cont_VV(wf(:,71),wf(:,78)) * den(80)
  A(57) = cont_VV(wf(:,73),wf(:,76)) * den(81)
  A(58) = cont_VV(wf(:,73),wf(:,78)) * den(82)
  A(59) = cont_VV(wf(:,66),wf(:,79)) * den(83)
  A(60) = cont_VV(wf(:,66),wf(:,81)) * den(85)
  A(61) = cont_VV(wf(:,71),wf(:,79)) * den(86)
  A(62) = cont_VV(wf(:,71),wf(:,81)) * den(87)
  A(63) = cont_VV(wf(:,73),wf(:,79)) * den(88)
  A(64) = cont_VV(wf(:,73),wf(:,81)) * den(89)
  A(65) = cont_VV(wf(:,66),wf(:,82)) * den(90)
  A(66) = cont_VV(wf(:,66),wf(:,84)) * den(92)
  A(67) = cont_VV(wf(:,71),wf(:,82)) * den(93)
  A(68) = cont_VV(wf(:,71),wf(:,84)) * den(94)
  A(69) = cont_VV(wf(:,73),wf(:,82)) * den(95)
  A(70) = cont_VV(wf(:,73),wf(:,84)) * den(96)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(70)
  complex(REALKIND), intent(out) :: M1(0), M2(2)


  M2(1) = (A(7)+A(9)+A(11)+A(13)+A(34)+A(36))*f(1)+2*CI*(A(1)-A(3)+A(5))*f(2)+2*CI*(A(41)+A(42)-A(43)-A(44)+A(45)+A(46)+A(65) &
       +A(66)-A(67)-A(68)+A(69)+A(70))*f(3)+2*CI*(A(2)-A(4)+A(6))*f(4)-A(20)*f(5)+2*CI*(-A(48)+A(50)-A(52))*f(6)+2*CI*(A(47)-A(49) &
       +A(51))*f(7)+(A(18)-A(22)-A(24)-A(26)-A(28)-A(30)-A(32))*f(8)+2*CI*(-A(15)+A(16)-A(17))*f(9)+2*CI*(-A(53)-A(54)+A(55)+A(56) &
       -A(57)-A(58)-A(59)-A(60)+A(61)+A(62)-A(63)-A(64))*f(10)+2*CI*(-A(38)+A(39)-A(40))*f(11)
  M2(2) = (A(8)+A(10)+A(12)+A(14)+A(35)+A(37))*f(1)+2*CI*(-A(1)+A(3)-A(5))*f(2)+2*CI*(-A(41)-A(42)+A(43)+A(44)-A(45)-A(46)-A(65) &
       -A(66)+A(67)+A(68)-A(69)-A(70))*f(3)+2*CI*(-A(2)+A(4)-A(6))*f(4)-A(21)*f(5)+2*CI*(A(48)-A(50)+A(52))*f(6)+2*CI*(-A(47) &
       +A(49)-A(51))*f(7)+(A(19)-A(23)-A(25)-A(27)-A(29)-A(31)-A(33))*f(8)+2*CI*(A(15)-A(16)+A(17))*f(9)+2*CI*(A(53)+A(54)-A(55) &
       -A(56)+A(57)+A(58)+A(59)+A(60)-A(61)-A(62)+A(63)+A(64))*f(10)+2*CI*(A(38)-A(39)+A(40))*f(11)

end subroutine colourvectors

end module ol_loop_ppllllj2_nenexeexggg_1_/**/REALKIND
