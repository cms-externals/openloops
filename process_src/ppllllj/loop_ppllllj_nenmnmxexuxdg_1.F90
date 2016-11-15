
module ol_colourmatrix_ppllllj_nenmnmxexuxdg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(30,1), K2(1,1), KL(1,1)
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
  K1( 7,:) = [   0]
  K1( 8,:) = [   0]
  K1( 9,:) = [   0]
  K1(10,:) = [   0]
  K1(11,:) = [   0]
  K1(12,:) = [   0]
  K1(13,:) = [   0]
  K1(14,:) = [   0]
  K1(15,:) = [   0]
  K1(16,:) = [  16]
  K1(17,:) = [   0]
  K1(18,:) = [   0]
  K1(19,:) = [   0]
  K1(20,:) = [   0]
  K1(21,:) = [   2]
  K1(22,:) = [  16]
  K1(23,:) = [   0]
  K1(24,:) = [   0]
  K1(25,:) = [   0]
  K1(26,:) = [   0]
  K1(27,:) = [ -18]
  K1(28,:) = [ -18]
  K1(29,:) = [  36]
  K1(30,:) = [   0]
  K1 = (1._/**/REALKIND / 3) * K1

  K2(1,:) = [ 4]

  KL(1,:) = [ 4]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllllj_nenmnmxexuxdg_1_/**/REALKIND



module ol_forced_parameters_ppllllj_nenmnmxexuxdg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj_nenmnmxexuxdg_1_/**/REALKIND

module ol_loop_ppllllj_nenmnmxexuxdg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(21), c(11)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:123)
  ! denominators
  complex(REALKIND), save :: den(144)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,128)
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
    f( 1) = (CI*eQED**4*gQCD)/(4._/**/REALKIND*sw**4)
    f( 2) = (CI*countertermnorm*eQED**4*gQCD**3)/(4._/**/REALKIND*sw**4)
    f( 3) = (CI*countertermnorm*ctGqq*eQED**4*gQCD**3)/(4._/**/REALKIND*sw**4)
    f( 4) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**3)/(4._/**/REALKIND*sw**4)
    f( 5) = (CI*cw*eQED**4*gQCD)/(2._/**/REALKIND*sw**3)
    f( 6) = (CI*countertermnorm*cw*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**3)
    f( 7) = (CI*countertermnorm*ctGqq*cw*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**3)
    f( 8) = (CI*countertermnorm*ctVqq*cw*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**3)
    f( 9) = (CI*eQED**4*gQCD)/(2._/**/REALKIND*sw**2)
    f(10) = (CI*countertermnorm*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(11) = (CI*countertermnorm*ctGqq*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(12) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(13) = (countertermnorm*ctZGG*eQED**4*gQCD**3)/(sw**2*2._/**/REALKIND)
    f(14) = (CI*eQED**4*gQCD**3*integralnorm*SwB)/(4._/**/REALKIND*sw**4)
    f(15) = (eQED**4*gQCD**3*integralnorm*SwB)/(sw**4*4._/**/REALKIND)
    f(16) = (CI*cw*eQED**4*gQCD**3*integralnorm*SwB)/(2._/**/REALKIND*sw**3)
    f(17) = (cw*eQED**4*gQCD**3*integralnorm*SwB)/(sw**3*2._/**/REALKIND)
    f(18) = (CI*eQED**4*gQCD**3*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(19) = (eQED**4*gQCD**3*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(20) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(21) = (eQED**4*gQCD**3*integralnorm*SwF)/sw**2

  c = [ 9*CI*f(14), f(15), 8*f(15), 9*CI*f(16), f(17), 8*f(17), 9*CI*f(18), f(19), 8*f(19), 3*f(20), 3*f(21) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,7)
  integer,           intent(in)  :: H(7)
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(70)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_A(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_Q(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_W(wf(:,0),wf(:,-3),wf(:,1))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,-2),wf(:,2))
  call vert_AV_Q(wf(:,-4),wf(:,-6),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,9),MW,1_intkind1,wf(:,4))
  call prop_W_W(wf(:,2),Q(:,6),MZ,1_intkind1,wf(:,5))
  call prop_A_Q(wf(:,3),Q(:,80),ZERO,0_intkind1,wf(:,6))
  call vert_WQ_A(wf(:,4),wf(:,-5),wf(:,7))
  call vert_AZ_Q(gZu,wf(:,6),wf(:,5),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,41),ZERO,0_intkind1,wf(:,9))
  call vert_ZQ_A(gZd,wf(:,5),wf(:,-5),wf(:,10))
  call vert_AW_Q(wf(:,6),wf(:,4),wf(:,11))
  call prop_Q_A(wf(:,10),Q(:,38),ZERO,0_intkind1,wf(:,12))
  call vert_QA_W(wf(:,-5),wf(:,6),wf(:,13))
  call vert_UV_W(wf(:,5),Q(:,6),wf(:,4),Q(:,9),wf(:,14))
  call prop_W_W(wf(:,13),Q(:,112),MW,1_intkind1,wf(:,15))
  call vert_VQ_A(wf(:,-6),wf(:,-5),wf(:,16))
  call prop_Q_A(wf(:,16),Q(:,96),ZERO,0_intkind1,wf(:,17))
  call vert_AW_Q(wf(:,-4),wf(:,4),wf(:,18))
  call vert_ZQ_A(gZd,wf(:,5),wf(:,17),wf(:,19))
  call prop_A_Q(wf(:,18),Q(:,25),ZERO,0_intkind1,wf(:,20))
  call vert_AZ_Q(gZu,wf(:,-4),wf(:,5),wf(:,21))
  call vert_WQ_A(wf(:,4),wf(:,17),wf(:,22))
  call prop_A_Q(wf(:,21),Q(:,22),ZERO,0_intkind1,wf(:,23))
  call vert_QA_W(wf(:,17),wf(:,-4),wf(:,24))
  call prop_W_W(wf(:,24),Q(:,112),MW,1_intkind1,wf(:,25))
  call vert_AV_Q(wf(:,20),wf(:,-6),wf(:,26))
  call vert_AV_Q(wf(:,23),wf(:,-6),wf(:,27))
  call vert_ZQ_A(gZn,wf(:,5),wf(:,0),wf(:,28))
  call prop_Q_A(wf(:,28),Q(:,7),ZERO,0_intkind1,wf(:,29))
  call vert_QA_W(wf(:,29),wf(:,-3),wf(:,30))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,5),wf(:,31))
  call prop_A_Q(wf(:,31),Q(:,14),ZERO,0_intkind1,wf(:,32))
  call vert_QA_W(wf(:,0),wf(:,32),wf(:,33))
  call vert_AW_Q(wf(:,-2),wf(:,4),wf(:,34))
  call prop_A_Q(wf(:,34),Q(:,13),ZERO,0_intkind1,wf(:,35))
  call vert_QA_W(wf(:,-1),wf(:,35),wf(:,36))
  call counter_AZ_Q(gZu,wf(:,6),wf(:,5),wf(:,37))
  call counter_AW_Q(wf(:,6),wf(:,4),wf(:,38))
  call counter_ZQ_A(gZd,wf(:,5),wf(:,17),wf(:,39))
  call counter_WQ_A(wf(:,4),wf(:,17),wf(:,40))
  call counter_AV_Q(wf(:,20),wf(:,-6),wf(:,41))
  call counter_VG_G(wf(:,5),wf(:,-6),Q(:,64),wf(:,42),Q(:,70))
  call vert_QA_V(wf(:,-5),wf(:,20),wf(:,43))
  call counter_AV_Q(wf(:,23),wf(:,-6),wf(:,44))
  call vert_QA_V(wf(:,9),wf(:,-4),wf(:,45))
  call counter_QA_W(wf(:,-5),wf(:,6),wf(:,46))
  call prop_W_W(wf(:,14),Q(:,15),MW,1_intkind1,wf(:,47))
  call counter_ZQ_A(gZd,wf(:,5),wf(:,-5),wf(:,48))
  call prop_A_Q(wf(:,11),Q(:,89),ZERO,0_intkind1,wf(:,49))
  call counter_WQ_A(wf(:,4),wf(:,-5),wf(:,50))
  call prop_A_Q(wf(:,8),Q(:,86),ZERO,0_intkind1,wf(:,51))
  call prop_Q_A(wf(:,48),Q(:,38),ZERO,0_intkind1,wf(:,52))
  call prop_Q_A(wf(:,50),Q(:,41),ZERO,0_intkind1,wf(:,53))
  call counter_VQ_A(wf(:,-6),wf(:,-5),wf(:,54))
  call prop_Q_A(wf(:,54),Q(:,96),ZERO,0_intkind1,wf(:,55))
  call vert_ZQ_A(gZd,wf(:,5),wf(:,55),wf(:,56))
  call vert_WQ_A(wf(:,4),wf(:,55),wf(:,57))
  call vert_QA_W(wf(:,55),wf(:,-4),wf(:,58))
  call prop_W_W(wf(:,58),Q(:,112),MW,1_intkind1,wf(:,59))
  call counter_QA_W(wf(:,17),wf(:,-4),wf(:,60))
  call counter_AZ_Q(gZu,wf(:,-4),wf(:,5),wf(:,61))
  call prop_Q_A(wf(:,22),Q(:,105),ZERO,0_intkind1,wf(:,62))
  call counter_AW_Q(wf(:,-4),wf(:,4),wf(:,63))
  call prop_Q_A(wf(:,19),Q(:,102),ZERO,0_intkind1,wf(:,64))
  call prop_A_Q(wf(:,61),Q(:,22),ZERO,0_intkind1,wf(:,65))
  call vert_AV_Q(wf(:,65),wf(:,-6),wf(:,66))
  call prop_A_Q(wf(:,63),Q(:,25),ZERO,0_intkind1,wf(:,67))
  call vert_AV_Q(wf(:,67),wf(:,-6),wf(:,68))
  call counter_AV_Q(wf(:,-4),wf(:,-6),wf(:,69))
  call prop_A_Q(wf(:,69),Q(:,80),ZERO,0_intkind1,wf(:,70))
  call vert_AZ_Q(gZu,wf(:,70),wf(:,5),wf(:,71))
  call vert_AW_Q(wf(:,70),wf(:,4),wf(:,72))
  call vert_QA_W(wf(:,-5),wf(:,70),wf(:,73))
  call prop_W_W(wf(:,73),Q(:,112),MW,1_intkind1,wf(:,74))
  call prop_W_W(wf(:,46),Q(:,112),MW,1_intkind1,wf(:,75))
  call prop_W_W(wf(:,60),Q(:,112),MW,1_intkind1,wf(:,76))
  call counter_A_Q(ctqq,wf(:,6),Q(:,80),wf(:,77))
  call prop_A_Q(wf(:,77),Q(:,80),ZERO,0_intkind1,wf(:,78))
  call vert_QA_W(wf(:,-5),wf(:,78),wf(:,79))
  call vert_AZ_Q(gZu,wf(:,78),wf(:,5),wf(:,80))
  call vert_AW_Q(wf(:,78),wf(:,4),wf(:,81))
  call counter_Q_A(ctqq,wf(:,9),Q(:,41),wf(:,82))
  call counter_Q_A(ctqq,wf(:,12),Q(:,38),wf(:,83))
  call counter_Q_A(ctqq,wf(:,17),Q(:,96),wf(:,84))
  call prop_Q_A(wf(:,84),Q(:,96),ZERO,0_intkind1,wf(:,85))
  call vert_QA_W(wf(:,85),wf(:,-4),wf(:,86))
  call counter_A_Q(ctqq,wf(:,20),Q(:,25),wf(:,87))
  call counter_A_Q(ctqq,wf(:,23),Q(:,22),wf(:,88))
  call vert_ZQ_A(gZd,wf(:,5),wf(:,85),wf(:,89))
  call vert_WQ_A(wf(:,4),wf(:,85),wf(:,90))
  call vert_VQ_A(wf(:,-6),wf(:,12),wf(:,91))
  call prop_Q_A(wf(:,91),Q(:,102),ZERO,0_intkind1,wf(:,92))
  call vert_VQ_A(wf(:,-6),wf(:,9),wf(:,93))
  call prop_Q_A(wf(:,93),Q(:,105),ZERO,0_intkind1,wf(:,94))
  call prop_A_Q(wf(:,26),Q(:,89),ZERO,0_intkind1,wf(:,95))
  call prop_A_Q(wf(:,27),Q(:,86),ZERO,0_intkind1,wf(:,96))
  call prop_W_W(wf(:,30),Q(:,15),MW,1_intkind1,wf(:,97))
  call prop_W_W(wf(:,33),Q(:,15),MW,1_intkind1,wf(:,98))
  call prop_W_W(wf(:,36),Q(:,15),MW,1_intkind1,wf(:,99))
  call vert_AZ_Q(gZd,wf(:,20),wf(:,5),wf(:,100))
  call prop_A_Q(wf(:,100),Q(:,31),ZERO,0_intkind1,wf(:,101))
  call vert_ZQ_A(gZu,wf(:,5),wf(:,9),wf(:,102))
  call prop_Q_A(wf(:,102),Q(:,47),ZERO,0_intkind1,wf(:,103))
  call vert_AW_Q(wf(:,23),wf(:,4),wf(:,104))
  call prop_A_Q(wf(:,104),Q(:,31),ZERO,0_intkind1,wf(:,105))
  call vert_WQ_A(wf(:,4),wf(:,12),wf(:,106))
  call prop_Q_A(wf(:,106),Q(:,47),ZERO,0_intkind1,wf(:,107))
  call vert_AW_Q(wf(:,-4),wf(:,47),wf(:,108))
  call prop_A_Q(wf(:,108),Q(:,31),ZERO,0_intkind1,wf(:,109))
  call vert_WQ_A(wf(:,47),wf(:,-5),wf(:,110))
  call prop_Q_A(wf(:,110),Q(:,47),ZERO,0_intkind1,wf(:,111))
  call vert_AW_Q(wf(:,-4),wf(:,97),wf(:,112))
  call prop_A_Q(wf(:,112),Q(:,31),ZERO,0_intkind1,wf(:,113))
  call vert_WQ_A(wf(:,97),wf(:,-5),wf(:,114))
  call prop_Q_A(wf(:,114),Q(:,47),ZERO,0_intkind1,wf(:,115))
  call vert_AW_Q(wf(:,-4),wf(:,98),wf(:,116))
  call prop_A_Q(wf(:,116),Q(:,31),ZERO,0_intkind1,wf(:,117))
  call vert_WQ_A(wf(:,98),wf(:,-5),wf(:,118))
  call prop_Q_A(wf(:,118),Q(:,47),ZERO,0_intkind1,wf(:,119))
  call vert_AW_Q(wf(:,-4),wf(:,99),wf(:,120))
  call prop_A_Q(wf(:,120),Q(:,31),ZERO,0_intkind1,wf(:,121))
  call vert_WQ_A(wf(:,99),wf(:,-5),wf(:,122))
  call prop_Q_A(wf(:,122),Q(:,47),ZERO,0_intkind1,wf(:,123))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,9) - MW2)
  den(2) = 1 / (Q(5,6) - MZ2)
  den(3) = 1 / (Q(5,80))
  den(5) = 1 / (Q(5,41))
  den(9) = 1 / (Q(5,38))
  den(13) = 1 / (Q(5,112) - MW2)
  den(16) = 1 / (Q(5,96))
  den(18) = 1 / (Q(5,25))
  den(22) = 1 / (Q(5,22))
  den(29) = 1 / (Q(5,7))
  den(32) = 1 / (Q(5,14))
  den(37) = 1 / (Q(5,13))
  den(41) = 1 / (Q(5,70))
  den(45) = 1 / (Q(5,15) - MW2)
  den(48) = 1 / (Q(5,89))
  den(51) = 1 / (Q(5,86))
  den(55) = 1 / (Q(5,105))
  den(58) = 1 / (Q(5,102))
  den(94) = 1 / (Q(5,57))
  den(97) = 1 / (Q(5,31))
  den(101) = 1 / (Q(5,47))

  ! denominators
  den(4) = den(2)*den(3)
  den(6) = den(1)*den(5)
  den(7) = den(4)*den(6)
  den(8) = den(1)*den(3)
  den(10) = den(2)*den(9)
  den(11) = den(8)*den(10)
  den(12) = den(1)*den(2)
  den(14) = den(3)*den(13)
  den(15) = den(12)*den(14)
  den(17) = den(2)*den(16)
  den(19) = den(1)*den(18)
  den(20) = den(17)*den(19)
  den(21) = den(1)*den(16)
  den(23) = den(2)*den(22)
  den(24) = den(21)*den(23)
  den(25) = den(13)*den(16)
  den(26) = den(12)*den(25)
  den(27) = den(10)*den(19)
  den(28) = den(6)*den(23)
  den(30) = den(2)*den(29)
  den(31) = den(14)*den(30)
  den(33) = den(2)*den(32)
  den(34) = den(14)*den(33)
  den(35) = den(25)*den(30)
  den(36) = den(25)*den(33)
  den(38) = den(1)*den(37)
  den(39) = den(14)*den(38)
  den(40) = den(25)*den(38)
  den(42) = den(2)*den(41)
  den(43) = den(19)*den(42)
  den(44) = den(6)*den(42)
  den(46) = den(12)*den(45)
  den(47) = den(3)*den(46)
  den(49) = den(8)*den(48)
  den(50) = den(2)*den(49)
  den(52) = den(4)*den(51)
  den(53) = den(1)*den(52)
  den(54) = den(16)*den(46)
  den(56) = den(21)*den(55)
  den(57) = den(2)*den(56)
  den(59) = den(17)*den(58)
  den(60) = den(1)*den(59)
  den(61) = den(3)**2
  den(62) = den(46)*den(61)
  den(63) = den(2)*den(61)
  den(64) = den(6)*den(63)
  den(65) = den(1)*den(61)
  den(66) = den(10)*den(65)
  den(67) = den(6)*den(52)
  den(68) = den(10)*den(49)
  den(69) = den(16)**2
  den(70) = den(46)*den(69)
  den(71) = den(19)*den(59)
  den(72) = den(23)*den(56)
  den(73) = den(2)*den(69)
  den(74) = den(19)*den(73)
  den(75) = den(1)*den(69)
  den(76) = den(23)*den(75)
  den(77) = den(10)*den(58)
  den(78) = den(19)*den(77)
  den(79) = den(6)*den(55)
  den(80) = den(23)*den(79)
  den(81) = den(19)*den(48)
  den(82) = den(10)*den(81)
  den(83) = den(23)*den(51)
  den(84) = den(6)*den(83)
  den(85) = den(30)*den(45)
  den(86) = den(61)*den(85)
  den(87) = den(33)*den(45)
  den(88) = den(61)*den(87)
  den(89) = den(69)*den(85)
  den(90) = den(69)*den(87)
  den(91) = den(38)*den(45)
  den(92) = den(61)*den(91)
  den(93) = den(69)*den(91)
  den(95) = den(19)*den(94)
  den(96) = den(2)*den(19)
  den(98) = den(96)*den(97)
  den(99) = den(6)*den(94)
  den(100) = den(2)*den(6)
  den(102) = den(100)*den(101)
  den(103) = den(1)*den(23)
  den(104) = den(97)*den(103)
  den(105) = den(1)*den(10)
  den(106) = den(101)*den(105)
  den(107) = den(46)*den(97)
  den(108) = den(46)*den(101)
  den(109) = den(85)*den(97)
  den(110) = den(85)*den(101)
  den(111) = den(87)*den(97)
  den(112) = den(87)*den(101)
  den(113) = den(91)*den(97)
  den(114) = den(91)*den(101)
  den(115) = den(2)*den(3)*den(6)
  den(116) = den(1)*den(3)*den(10)
  den(117) = den(1)*den(2)*den(3)
  den(118) = den(2)*den(16)*den(19)
  den(119) = den(1)*den(16)*den(23)
  den(120) = den(1)*den(2)*den(16)
  den(121) = den(2)*den(95)
  den(122) = den(2)*den(81)
  den(123) = den(2)*den(99)
  den(124) = den(2)*den(79)
  den(125) = den(1)*den(83)
  den(126) = den(1)*den(77)
  den(127) = den(3)*den(85)
  den(128) = den(3)*den(87)
  den(129) = den(16)*den(85)
  den(130) = den(16)*den(87)
  den(131) = den(3)*den(91)
  den(132) = den(16)*den(91)
  den(133) = den(3)*den(102)
  den(134) = den(3)*den(106)
  den(135) = den(3)*den(108)
  den(136) = den(16)*den(98)
  den(137) = den(16)*den(104)
  den(138) = den(16)*den(107)
  den(139) = den(3)*den(110)
  den(140) = den(3)*den(112)
  den(141) = den(16)*den(109)
  den(142) = den(16)*den(111)
  den(143) = den(3)*den(114)
  den(144) = den(16)*den(113)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(70)

  A(1) = cont_QA(wf(:,8),wf(:,9)) * den(7)
  A(2) = cont_QA(wf(:,11),wf(:,12)) * den(11)
  A(3) = cont_VV(wf(:,14),wf(:,15)) * den(15)
  A(4) = cont_QA(wf(:,19),wf(:,20)) * den(20)
  A(5) = cont_QA(wf(:,22),wf(:,23)) * den(24)
  A(6) = cont_VV(wf(:,14),wf(:,25)) * den(26)
  A(7) = cont_QA(wf(:,12),wf(:,26)) * den(27)
  A(8) = cont_QA(wf(:,9),wf(:,27)) * den(28)
  A(9) = cont_VV(wf(:,15),wf(:,30)) * den(31)
  A(10) = cont_VV(wf(:,15),wf(:,33)) * den(34)
  A(11) = cont_VV(wf(:,25),wf(:,30)) * den(35)
  A(12) = cont_VV(wf(:,25),wf(:,33)) * den(36)
  A(13) = cont_VV(wf(:,15),wf(:,36)) * den(39)
  A(14) = cont_VV(wf(:,25),wf(:,36)) * den(40)

  A(15) = cont_QA(wf(:,9),wf(:,37)) * den(7)
  A(16) = cont_QA(wf(:,12),wf(:,38)) * den(11)
  A(17) = cont_QA(wf(:,20),wf(:,39)) * den(20)
  A(18) = cont_QA(wf(:,23),wf(:,40)) * den(24)
  A(19) = cont_QA(wf(:,12),wf(:,41)) * den(27)
  A(20) = cont_VV(wf(:,42),wf(:,43)) * den(43)
  A(21) = cont_QA(wf(:,9),wf(:,44)) * den(28)
  A(22) = cont_VV(wf(:,42),wf(:,45)) * den(44)
  A(23) = cont_VV(wf(:,46),wf(:,47)) * den(47)
  A(24) = cont_QA(wf(:,48),wf(:,49)) * den(50)
  A(25) = cont_QA(wf(:,50),wf(:,51)) * den(53)
  A(26) = cont_QA(wf(:,26),wf(:,52)) * den(27)
  A(27) = cont_QA(wf(:,27),wf(:,53)) * den(28)
  A(28) = cont_QA(wf(:,20),wf(:,56)) * den(20)
  A(29) = cont_QA(wf(:,23),wf(:,57)) * den(24)
  A(30) = cont_VV(wf(:,14),wf(:,59)) * den(26)
  A(31) = cont_VV(wf(:,47),wf(:,60)) * den(54)
  A(32) = cont_QA(wf(:,61),wf(:,62)) * den(57)
  A(33) = cont_QA(wf(:,63),wf(:,64)) * den(60)
  A(34) = cont_QA(wf(:,9),wf(:,66)) * den(28)
  A(35) = cont_QA(wf(:,12),wf(:,68)) * den(27)
  A(36) = cont_QA(wf(:,9),wf(:,71)) * den(7)
  A(37) = cont_QA(wf(:,12),wf(:,72)) * den(11)
  A(38) = cont_VV(wf(:,14),wf(:,74)) * den(15)
  A(39) = cont_VV(wf(:,30),wf(:,75)) * den(31)
  A(40) = cont_VV(wf(:,33),wf(:,75)) * den(34)
  A(41) = cont_VV(wf(:,30),wf(:,59)) * den(35)
  A(42) = cont_VV(wf(:,33),wf(:,59)) * den(36)
  A(43) = cont_VV(wf(:,30),wf(:,76)) * den(35)
  A(44) = cont_VV(wf(:,33),wf(:,76)) * den(36)
  A(45) = cont_VV(wf(:,30),wf(:,74)) * den(31)
  A(46) = cont_VV(wf(:,33),wf(:,74)) * den(34)
  A(47) = cont_VV(wf(:,36),wf(:,75)) * den(39)
  A(48) = cont_VV(wf(:,36),wf(:,59)) * den(40)
  A(49) = cont_VV(wf(:,36),wf(:,76)) * den(40)
  A(50) = cont_VV(wf(:,36),wf(:,74)) * den(39)
  A(51) = cont_VV(wf(:,47),wf(:,79)) * den(62)
  A(52) = cont_QA(wf(:,9),wf(:,80)) * den(64)
  A(53) = cont_QA(wf(:,12),wf(:,81)) * den(66)
  A(54) = cont_QA(wf(:,51),wf(:,82)) * den(67)
  A(55) = cont_QA(wf(:,49),wf(:,83)) * den(68)
  A(56) = cont_VV(wf(:,47),wf(:,86)) * den(70)
  A(57) = cont_QA(wf(:,64),wf(:,87)) * den(71)
  A(58) = cont_QA(wf(:,62),wf(:,88)) * den(72)
  A(59) = cont_QA(wf(:,20),wf(:,89)) * den(74)
  A(60) = cont_QA(wf(:,23),wf(:,90)) * den(76)
  A(61) = cont_QA(wf(:,87),wf(:,92)) * den(78)
  A(62) = cont_QA(wf(:,88),wf(:,94)) * den(80)
  A(63) = cont_QA(wf(:,83),wf(:,95)) * den(82)
  A(64) = cont_QA(wf(:,82),wf(:,96)) * den(84)
  A(65) = cont_VV(wf(:,79),wf(:,97)) * den(86)
  A(66) = cont_VV(wf(:,79),wf(:,98)) * den(88)
  A(67) = cont_VV(wf(:,86),wf(:,97)) * den(89)
  A(68) = cont_VV(wf(:,86),wf(:,98)) * den(90)
  A(69) = cont_VV(wf(:,79),wf(:,99)) * den(92)
  A(70) = cont_VV(wf(:,86),wf(:,99)) * den(93)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(70)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(13)-A(14))*f(1)+(-A(3)-A(6))*f(5)+(-A(1)-A(2)-A(4)-A(5)-A(7)-A(8)-A(9)-A(10)-A(11)-A(12))*f(9)

  M2(1) = (A(69)+A(70))*f(2)+(-A(48)-A(50))*f(3)+(-A(47)-A(49))*f(4)+(A(51)+A(56))*f(6)+(-A(30)-A(38))*f(7)+(-A(23)-A(31))*f(8) &
       +(A(52)+A(53)+A(54)+A(55)+A(57)+A(58)+A(59)+A(60)+A(61)+A(62)+A(63)+A(64)+A(65)+A(66)+A(67)+A(68))*f(10)+(-A(19)-A(21) &
       -A(28)-A(29)-A(36)-A(37)-A(41)-A(42)-A(45)-A(46))*f(11)+(-A(15)-A(16)-A(17)-A(18)-A(24)-A(25)-A(26)-A(27)-A(32)-A(33)-A(34) &
       -A(35)-A(39)-A(40)-A(43)-A(44))*f(12)+(A(20)+A(22))*f(13)

end subroutine colourvectors

end module ol_loop_ppllllj_nenmnmxexuxdg_1_/**/REALKIND
