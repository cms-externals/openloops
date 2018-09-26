
module ol_colourmatrix_ppllnnjj_vbs_nexnmxemucdxsx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(76,2), K2(2,2), KL(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9,   3]
  K1( 2,:) = [   3,   9]
  K1( 3,:) = [   0,   0]
  K1( 4,:) = [   0,   0]
  K1( 5,:) = [   0,   0]
  K1( 6,:) = [   0,   0]
  K1( 7,:) = [   0,   0]
  K1( 8,:) = [   0,   0]
  K1( 9,:) = [   0,   0]
  K1(10,:) = [   0,   0]
  K1(11,:) = [   0,   0]
  K1(12,:) = [   0,   0]
  K1(13,:) = [   0,   0]
  K1(14,:) = [   0,   0]
  K1(15,:) = [   0,   0]
  K1(16,:) = [   0,   0]
  K1(17,:) = [   0,   0]
  K1(18,:) = [   0,   0]
  K1(19,:) = [   0,   0]
  K1(20,:) = [   0,   0]
  K1(21,:) = [   0,   0]
  K1(22,:) = [   0,   0]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   0,   0]
  K1(28,:) = [   0,   0]
  K1(29,:) = [   0,   0]
  K1(30,:) = [   0,   0]
  K1(31,:) = [  12,   4]
  K1(32,:) = [   4,  12]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [   0,   4]
  K1(42,:) = [   4,   0]
  K1(43,:) = [  12,   4]
  K1(44,:) = [   4,  12]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]
  K1(47,:) = [   0,   0]
  K1(48,:) = [   0,   0]
  K1(49,:) = [   0,   0]
  K1(50,:) = [   0,   0]
  K1(51,:) = [   0,   0]
  K1(52,:) = [   0,   0]
  K1(53,:) = [   0,  -4]
  K1(54,:) = [  -4, -12]
  K1(55,:) = [ -12,  -4]
  K1(56,:) = [  -4,   0]
  K1(57,:) = [  12,   4]
  K1(58,:) = [   4,  12]
  K1(59,:) = [   0,   0]
  K1(60,:) = [   0,   0]
  K1(61,:) = [   0,   0]
  K1(62,:) = [   0,   0]
  K1(63,:) = [   0,   0]
  K1(64,:) = [   0,   0]
  K1(65,:) = [   0,   0]
  K1(66,:) = [   0,   0]
  K1(67,:) = [ -12,  -4]
  K1(68,:) = [  -4,   0]
  K1(69,:) = [   0,  -4]
  K1(70,:) = [  -4, -12]
  K1(71,:) = [   0,   4]
  K1(72,:) = [   4,   0]
  K1(73,:) = [  12,   4]
  K1(74,:) = [   4,  12]
  K1(75,:) = [   0,   0]
  K1(76,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllnnjj_vbs_nexnmxemucdxsx_1_/**/REALKIND



module ol_forced_parameters_ppllnnjj_vbs_nexnmxemucdxsx_1_/**/REALKIND
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
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllnnjj_vbs_nexnmxemucdxsx_1_/**/REALKIND

module ol_loop_ppllnnjj_vbs_nexnmxemucdxsx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(11), c(17)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-8+1:122)
  ! denominators
  complex(REALKIND), save :: den(159)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,256), Mct(2,256), Mcol_loop(2,256)
  ! zero helicity identifier
  logical,           save :: zerohel(256) = .true., zerohel_ct(256) = .true.

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
    f( 1) = (CI*eQED**4*gQCD**2)/(4._/**/REALKIND*sw**4)
    f( 2) = (CI*countertermnorm*eQED**4*gQCD**4)/(4._/**/REALKIND*sw**4)
    f( 3) = (CI*countertermnorm*ctGcc*eQED**4*gQCD**4)/(4._/**/REALKIND*sw**4)
    f( 4) = (CI*countertermnorm*ctGqq*eQED**4*gQCD**4)/(4._/**/REALKIND*sw**4)
    f( 5) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**4)/(4._/**/REALKIND*sw**4)
    f( 6) = (CI*countertermnorm*ctVsc*eQED**4*gQCD**4)/(4._/**/REALKIND*sw**4)
    f( 7) = (CI*eQED**4*gQCD**4*integralnorm*SwB)/(4._/**/REALKIND*sw**4)
    f( 8) = (eQED**4*gQCD**4*integralnorm*SwB)/(sw**4*8._/**/REALKIND)
    f( 9) = (eQED**4*gQCD**4*integralnorm*SwB)/(sw**4*4._/**/REALKIND)
    f(10) = (eQED**4*gQCD**4*integralnorm*SwF)/(sw**4*4._/**/REALKIND)
    f(11) = (eQED**4*gQCD**4*integralnorm*SwF)/(sw**4*2._/**/REALKIND)

  c = [ 9*CI*f(7), 27*CI*f(7), 18*f(8), 54*f(8), f(9), 3*f(9), 6*f(9), 8*f(9), 10*f(9), 18*f(9), 21*f(9), 24*f(9), 54*f(9) &
    , 3*f(10), 9*f(10), 3*f(11), 9*f(11) ]
  c = (1._/**/REALKIND / 36) * c
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
  real(REALKIND),    intent(in)  :: P(0:3,8)
  integer,           intent(in)  :: H(8)
  integer,           intent(in), optional  :: POLSEL(8)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(64)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_A(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_Q(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_Q(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_Q(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))
    call pol_wf_A(P(:,7), rZERO, H(7), wf(:,-6), POLSEL(7))
    call pol_wf_A(P(:,8), rZERO, H(8), wf(:,-7), POLSEL(8))

  else
    call pol_wf_A(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_Q(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_Q(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_Q(P(:,6), rZERO, H(6), wf(:,-5), 0)
    call pol_wf_A(P(:,7), rZERO, H(7), wf(:,-6), 0)
    call pol_wf_A(P(:,8), rZERO, H(8), wf(:,-7), 0)

  end if

  ! internal WFs
  call vert_QA_W(wf(:,-2),wf(:,0),wf(:,1))
  call vert_QA_W(wf(:,-3),wf(:,-1),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,5),MW,1_intkind1,wf(:,3))
  call prop_W_W(wf(:,2),Q(:,10),MW,1_intkind1,wf(:,4))
  call vert_WQ_A(wf(:,3),wf(:,-4),wf(:,5))
  call vert_WQ_A(wf(:,4),wf(:,-5),wf(:,6))
  call prop_Q_A(wf(:,5),Q(:,21),ZERO,0_intkind1,wf(:,7))
  call prop_Q_A(wf(:,6),Q(:,42),ZERO,0_intkind1,wf(:,8))
  call vert_QA_V(wf(:,7),wf(:,-6),wf(:,9))
  call vert_QA_V(wf(:,8),wf(:,-7),wf(:,10))
  call vert_AW_Q(wf(:,-7),wf(:,4),wf(:,11))
  call prop_A_Q(wf(:,11),Q(:,138),ZERO,0_intkind1,wf(:,12))
  call vert_QA_V(wf(:,-5),wf(:,12),wf(:,13))
  call vert_WQ_A(wf(:,4),wf(:,-4),wf(:,14))
  call vert_WQ_A(wf(:,3),wf(:,-5),wf(:,15))
  call prop_Q_A(wf(:,14),Q(:,26),ZERO,0_intkind1,wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,37),ZERO,0_intkind1,wf(:,17))
  call vert_QA_V(wf(:,16),wf(:,-6),wf(:,18))
  call vert_QA_V(wf(:,17),wf(:,-7),wf(:,19))
  call vert_AW_Q(wf(:,-6),wf(:,4),wf(:,20))
  call prop_A_Q(wf(:,20),Q(:,74),ZERO,0_intkind1,wf(:,21))
  call vert_QA_V(wf(:,-4),wf(:,21),wf(:,22))
  call vert_AW_Q(wf(:,-6),wf(:,3),wf(:,23))
  call prop_A_Q(wf(:,23),Q(:,69),ZERO,0_intkind1,wf(:,24))
  call vert_QA_V(wf(:,-4),wf(:,24),wf(:,25))
  call vert_AW_Q(wf(:,-7),wf(:,3),wf(:,26))
  call prop_A_Q(wf(:,26),Q(:,133),ZERO,0_intkind1,wf(:,27))
  call vert_QA_V(wf(:,-5),wf(:,27),wf(:,28))
  call counter_QA_V(wf(:,8),wf(:,-7),wf(:,29))
  call counter_AW_Q(wf(:,-7),wf(:,4),wf(:,30))
  call prop_A_Q(wf(:,30),Q(:,138),ZERO,0_intkind1,wf(:,31))
  call vert_QA_V(wf(:,-5),wf(:,31),wf(:,32))
  call counter_QA_V(wf(:,17),wf(:,-7),wf(:,33))
  call counter_AW_Q(wf(:,-7),wf(:,3),wf(:,34))
  call prop_A_Q(wf(:,34),Q(:,133),ZERO,0_intkind1,wf(:,35))
  call vert_QA_V(wf(:,-5),wf(:,35),wf(:,36))
  call counter_QA_V(wf(:,7),wf(:,-6),wf(:,37))
  call counter_QA_V(wf(:,16),wf(:,-6),wf(:,38))
  call counter_AW_Q(wf(:,-6),wf(:,4),wf(:,39))
  call prop_A_Q(wf(:,39),Q(:,74),ZERO,0_intkind1,wf(:,40))
  call vert_QA_V(wf(:,-4),wf(:,40),wf(:,41))
  call counter_AW_Q(wf(:,-6),wf(:,3),wf(:,42))
  call prop_A_Q(wf(:,42),Q(:,69),ZERO,0_intkind1,wf(:,43))
  call vert_QA_V(wf(:,-4),wf(:,43),wf(:,44))
  call counter_QA_V(wf(:,-5),wf(:,12),wf(:,45))
  call counter_WQ_A(wf(:,4),wf(:,-5),wf(:,46))
  call prop_Q_A(wf(:,46),Q(:,42),ZERO,0_intkind1,wf(:,47))
  call vert_QA_V(wf(:,47),wf(:,-7),wf(:,48))
  call counter_QA_V(wf(:,-5),wf(:,27),wf(:,49))
  call counter_WQ_A(wf(:,3),wf(:,-5),wf(:,50))
  call prop_Q_A(wf(:,50),Q(:,37),ZERO,0_intkind1,wf(:,51))
  call vert_QA_V(wf(:,51),wf(:,-7),wf(:,52))
  call counter_QA_V(wf(:,-4),wf(:,21),wf(:,53))
  call counter_WQ_A(wf(:,4),wf(:,-4),wf(:,54))
  call prop_Q_A(wf(:,54),Q(:,26),ZERO,0_intkind1,wf(:,55))
  call vert_QA_V(wf(:,55),wf(:,-6),wf(:,56))
  call counter_QA_V(wf(:,-4),wf(:,24),wf(:,57))
  call counter_WQ_A(wf(:,3),wf(:,-4),wf(:,58))
  call prop_Q_A(wf(:,58),Q(:,21),ZERO,0_intkind1,wf(:,59))
  call vert_QA_V(wf(:,59),wf(:,-6),wf(:,60))
  call counter_V_V(ctGG,wf(:,25),Q(:,85),wf(:,61))
  call counter_V_V(ctGG,wf(:,22),Q(:,90),wf(:,62))
  call counter_Q_A(ctqq,wf(:,7),Q(:,21),wf(:,63))
  call prop_Q_A(wf(:,63),Q(:,21),ZERO,0_intkind1,wf(:,64))
  call vert_QA_V(wf(:,64),wf(:,-6),wf(:,65))
  call counter_Q_A(ctqq,wf(:,16),Q(:,26),wf(:,66))
  call prop_Q_A(wf(:,66),Q(:,26),ZERO,0_intkind1,wf(:,67))
  call vert_QA_V(wf(:,67),wf(:,-6),wf(:,68))
  call counter_A_Q(ctqq,wf(:,21),Q(:,74),wf(:,69))
  call prop_A_Q(wf(:,69),Q(:,74),ZERO,0_intkind1,wf(:,70))
  call vert_QA_V(wf(:,-4),wf(:,70),wf(:,71))
  call counter_A_Q(ctqq,wf(:,24),Q(:,69),wf(:,72))
  call prop_A_Q(wf(:,72),Q(:,69),ZERO,0_intkind1,wf(:,73))
  call vert_QA_V(wf(:,-4),wf(:,73),wf(:,74))
  call counter_V_V(ctGG,wf(:,13),Q(:,170),wf(:,75))
  call counter_V_V(ctGG,wf(:,28),Q(:,165),wf(:,76))
  call counter_Q_A(ctqq,wf(:,8),Q(:,42),wf(:,77))
  call prop_Q_A(wf(:,77),Q(:,42),ZERO,0_intkind1,wf(:,78))
  call vert_QA_V(wf(:,78),wf(:,-7),wf(:,79))
  call counter_A_Q(ctcc,wf(:,12),Q(:,138),wf(:,80))
  call prop_A_Q(wf(:,80),Q(:,138),ZERO,0_intkind1,wf(:,81))
  call vert_QA_V(wf(:,-5),wf(:,81),wf(:,82))
  call counter_Q_A(ctqq,wf(:,17),Q(:,37),wf(:,83))
  call prop_Q_A(wf(:,83),Q(:,37),ZERO,0_intkind1,wf(:,84))
  call vert_QA_V(wf(:,84),wf(:,-7),wf(:,85))
  call counter_A_Q(ctcc,wf(:,27),Q(:,133),wf(:,86))
  call prop_A_Q(wf(:,86),Q(:,133),ZERO,0_intkind1,wf(:,87))
  call vert_QA_V(wf(:,-5),wf(:,87),wf(:,88))
  call counter_V_V(ctGG,wf(:,9),Q(:,85),wf(:,89))
  call counter_V_V(ctGG,wf(:,18),Q(:,90),wf(:,90))
  call vert_VQ_A(wf(:,9),wf(:,-5),wf(:,91))
  call prop_Q_A(wf(:,91),Q(:,117),ZERO,0_intkind1,wf(:,92))
  call vert_AV_Q(wf(:,-7),wf(:,9),wf(:,93))
  call prop_A_Q(wf(:,93),Q(:,213),ZERO,0_intkind1,wf(:,94))
  call vert_VQ_A(wf(:,19),wf(:,-4),wf(:,95))
  call prop_Q_A(wf(:,95),Q(:,181),ZERO,0_intkind1,wf(:,96))
  call vert_AV_Q(wf(:,-6),wf(:,19),wf(:,97))
  call prop_A_Q(wf(:,97),Q(:,229),ZERO,0_intkind1,wf(:,98))
  call vert_VQ_A(wf(:,25),wf(:,-5),wf(:,99))
  call prop_Q_A(wf(:,99),Q(:,117),ZERO,0_intkind1,wf(:,100))
  call vert_AV_Q(wf(:,-7),wf(:,25),wf(:,101))
  call prop_A_Q(wf(:,101),Q(:,213),ZERO,0_intkind1,wf(:,102))
  call vert_VQ_A(wf(:,28),wf(:,-4),wf(:,103))
  call prop_Q_A(wf(:,103),Q(:,181),ZERO,0_intkind1,wf(:,104))
  call vert_AV_Q(wf(:,-6),wf(:,28),wf(:,105))
  call prop_A_Q(wf(:,105),Q(:,229),ZERO,0_intkind1,wf(:,106))
  call vert_VQ_A(wf(:,18),wf(:,-5),wf(:,107))
  call prop_Q_A(wf(:,107),Q(:,122),ZERO,0_intkind1,wf(:,108))
  call vert_AV_Q(wf(:,-7),wf(:,18),wf(:,109))
  call prop_A_Q(wf(:,109),Q(:,218),ZERO,0_intkind1,wf(:,110))
  call vert_VQ_A(wf(:,10),wf(:,-4),wf(:,111))
  call prop_Q_A(wf(:,111),Q(:,186),ZERO,0_intkind1,wf(:,112))
  call vert_AV_Q(wf(:,-6),wf(:,10),wf(:,113))
  call prop_A_Q(wf(:,113),Q(:,234),ZERO,0_intkind1,wf(:,114))
  call vert_VQ_A(wf(:,22),wf(:,-5),wf(:,115))
  call prop_Q_A(wf(:,115),Q(:,122),ZERO,0_intkind1,wf(:,116))
  call vert_AV_Q(wf(:,-7),wf(:,22),wf(:,117))
  call prop_A_Q(wf(:,117),Q(:,218),ZERO,0_intkind1,wf(:,118))
  call vert_VQ_A(wf(:,13),wf(:,-4),wf(:,119))
  call prop_Q_A(wf(:,119),Q(:,186),ZERO,0_intkind1,wf(:,120))
  call vert_AV_Q(wf(:,-6),wf(:,13),wf(:,121))
  call prop_A_Q(wf(:,121),Q(:,234),ZERO,0_intkind1,wf(:,122))

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
  den(3) = 1 / (Q(5,21))
  den(5) = 1 / (Q(5,42))
  den(7) = 1 / (Q(5,85))
  den(10) = 1 / (Q(5,138))
  den(12) = 1 / (Q(5,170))
  den(15) = 1 / (Q(5,26))
  den(17) = 1 / (Q(5,37))
  den(19) = 1 / (Q(5,90))
  den(22) = 1 / (Q(5,74))
  den(26) = 1 / (Q(5,69))
  den(31) = 1 / (Q(5,133))
  den(33) = 1 / (Q(5,165))
  den(79) = 1 / (Q(5,117))
  den(81) = 1 / (Q(5,213))
  den(83) = 1 / (Q(5,181))
  den(85) = 1 / (Q(5,229))
  den(91) = 1 / (Q(5,122))
  den(93) = 1 / (Q(5,218))
  den(95) = 1 / (Q(5,186))
  den(97) = 1 / (Q(5,234))

  ! denominators
  den(4) = den(1)*den(3)
  den(6) = den(2)*den(5)
  den(8) = den(4)*den(7)
  den(9) = den(6)*den(8)
  den(11) = den(2)*den(10)
  den(13) = den(11)*den(12)
  den(14) = den(4)*den(13)
  den(16) = den(2)*den(15)
  den(18) = den(1)*den(17)
  den(20) = den(16)*den(19)
  den(21) = den(18)*den(20)
  den(23) = den(2)*den(22)
  den(24) = den(19)*den(23)
  den(25) = den(18)*den(24)
  den(27) = den(1)*den(26)
  den(28) = den(7)*den(27)
  den(29) = den(6)*den(28)
  den(30) = den(11)*den(28)
  den(32) = den(1)*den(31)
  den(34) = den(32)*den(33)
  den(35) = den(16)*den(34)
  den(36) = den(24)*den(32)
  den(37) = den(6)*den(12)
  den(38) = den(4)*den(37)
  den(39) = den(18)*den(33)
  den(40) = den(16)*den(39)
  den(41) = den(8)*den(11)
  den(42) = den(20)*den(32)
  den(43) = den(23)*den(39)
  den(44) = den(27)*den(37)
  den(45) = den(13)*den(27)
  den(46) = den(23)*den(34)
  den(47) = den(13)*den(28)
  den(48) = den(24)*den(34)
  den(49) = den(3)*den(4)
  den(50) = den(37)*den(49)
  den(51) = den(13)*den(49)
  den(52) = den(15)*den(16)
  den(53) = den(39)*den(52)
  den(54) = den(22)*den(23)
  den(55) = den(39)*den(54)
  den(56) = den(26)*den(27)
  den(57) = den(37)*den(56)
  den(58) = den(13)*den(56)
  den(59) = den(34)*den(52)
  den(60) = den(34)*den(54)
  den(61) = den(24)*den(39)
  den(62) = den(28)*den(37)
  den(63) = den(8)*den(13)
  den(64) = den(20)*den(34)
  den(65) = den(5)*den(6)
  den(66) = den(8)*den(65)
  den(67) = den(10)*den(11)
  den(68) = den(8)*den(67)
  den(69) = den(17)*den(18)
  den(70) = den(20)*den(69)
  den(71) = den(24)*den(69)
  den(72) = den(28)*den(65)
  den(73) = den(28)*den(67)
  den(74) = den(31)*den(32)
  den(75) = den(20)*den(74)
  den(76) = den(24)*den(74)
  den(77) = den(8)*den(37)
  den(78) = den(20)*den(39)
  den(80) = den(8)*den(79)
  den(82) = den(8)*den(81)
  den(84) = den(39)*den(83)
  den(86) = den(39)*den(85)
  den(87) = den(28)*den(79)
  den(88) = den(28)*den(81)
  den(89) = den(34)*den(83)
  den(90) = den(34)*den(85)
  den(92) = den(20)*den(91)
  den(94) = den(20)*den(93)
  den(96) = den(37)*den(95)
  den(98) = den(37)*den(97)
  den(99) = den(24)*den(91)
  den(100) = den(24)*den(93)
  den(101) = den(13)*den(95)
  den(102) = den(13)*den(97)
  den(103) = den(4)*den(6)
  den(104) = den(4)*den(11)
  den(105) = den(2)*den(80)
  den(106) = den(2)*den(82)
  den(107) = den(2)*den(8)
  den(108) = den(2)*den(4)
  den(109) = den(16)*den(18)
  den(110) = den(18)*den(23)
  den(111) = den(2)*den(84)
  den(112) = den(2)*den(86)
  den(113) = den(2)*den(39)
  den(114) = den(2)*den(18)
  den(115) = den(6)*den(27)
  den(116) = den(11)*den(27)
  den(117) = den(2)*den(87)
  den(118) = den(2)*den(88)
  den(119) = den(2)*den(28)
  den(120) = den(2)*den(27)
  den(121) = den(16)*den(32)
  den(122) = den(23)*den(32)
  den(123) = den(2)*den(89)
  den(124) = den(2)*den(90)
  den(125) = den(2)*den(34)
  den(126) = den(2)*den(32)
  den(127) = den(1)*den(92)
  den(128) = den(1)*den(94)
  den(129) = den(1)*den(20)
  den(130) = den(1)*den(16)
  den(131) = den(1)*den(96)
  den(132) = den(1)*den(98)
  den(133) = den(1)*den(37)
  den(134) = den(1)*den(6)
  den(135) = den(1)*den(99)
  den(136) = den(1)*den(100)
  den(137) = den(1)*den(24)
  den(138) = den(1)*den(23)
  den(139) = den(1)*den(101)
  den(140) = den(1)*den(102)
  den(141) = den(1)*den(13)
  den(142) = den(1)*den(11)
  den(143) = den(1)*den(2)
  den(144) = den(6)*den(82)
  den(145) = den(4)*den(98)
  den(146) = den(11)*den(80)
  den(147) = den(4)*den(102)
  den(148) = den(18)*den(94)
  den(149) = den(16)*den(86)
  den(150) = den(18)*den(100)
  den(151) = den(23)*den(84)
  den(152) = den(6)*den(88)
  den(153) = den(27)*den(96)
  den(154) = den(11)*den(87)
  den(155) = den(27)*den(101)
  den(156) = den(32)*den(92)
  den(157) = den(16)*den(90)
  den(158) = den(32)*den(99)
  den(159) = den(23)*den(89)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(64)

  A(1) = cont_VV(wf(:,9),wf(:,10)) * den(9)
  A(2) = cont_VV(wf(:,9),wf(:,13)) * den(14)
  A(3) = cont_VV(wf(:,18),wf(:,19)) * den(21)
  A(4) = cont_VV(wf(:,19),wf(:,22)) * den(25)
  A(5) = cont_VV(wf(:,10),wf(:,25)) * den(29)
  A(6) = cont_VV(wf(:,13),wf(:,25)) * den(30)
  A(7) = cont_VV(wf(:,18),wf(:,28)) * den(35)
  A(8) = cont_VV(wf(:,22),wf(:,28)) * den(36)

  A(9) = cont_VV(wf(:,9),wf(:,29)) * den(9)
  A(10) = cont_VV(wf(:,9),wf(:,32)) * den(14)
  A(11) = cont_VV(wf(:,18),wf(:,33)) * den(21)
  A(12) = cont_VV(wf(:,22),wf(:,33)) * den(25)
  A(13) = cont_VV(wf(:,25),wf(:,29)) * den(29)
  A(14) = cont_VV(wf(:,25),wf(:,32)) * den(30)
  A(15) = cont_VV(wf(:,18),wf(:,36)) * den(35)
  A(16) = cont_VV(wf(:,22),wf(:,36)) * den(36)
  A(17) = cont_VV(wf(:,10),wf(:,37)) * den(38)
  A(18) = cont_VV(wf(:,13),wf(:,37)) * den(14)
  A(19) = cont_VV(wf(:,19),wf(:,38)) * den(40)
  A(20) = cont_VV(wf(:,19),wf(:,41)) * den(25)
  A(21) = cont_VV(wf(:,28),wf(:,38)) * den(35)
  A(22) = cont_VV(wf(:,28),wf(:,41)) * den(36)
  A(23) = cont_VV(wf(:,10),wf(:,44)) * den(29)
  A(24) = cont_VV(wf(:,13),wf(:,44)) * den(30)
  A(25) = cont_VV(wf(:,9),wf(:,45)) * den(41)
  A(26) = cont_VV(wf(:,9),wf(:,48)) * den(9)
  A(27) = cont_VV(wf(:,25),wf(:,45)) * den(30)
  A(28) = cont_VV(wf(:,25),wf(:,48)) * den(29)
  A(29) = cont_VV(wf(:,18),wf(:,49)) * den(42)
  A(30) = cont_VV(wf(:,22),wf(:,49)) * den(36)
  A(31) = cont_VV(wf(:,18),wf(:,52)) * den(21)
  A(32) = cont_VV(wf(:,22),wf(:,52)) * den(25)
  A(33) = cont_VV(wf(:,19),wf(:,53)) * den(43)
  A(34) = cont_VV(wf(:,19),wf(:,56)) * den(21)
  A(35) = cont_VV(wf(:,10),wf(:,57)) * den(44)
  A(36) = cont_VV(wf(:,13),wf(:,57)) * den(45)
  A(37) = cont_VV(wf(:,28),wf(:,53)) * den(46)
  A(38) = cont_VV(wf(:,28),wf(:,56)) * den(35)
  A(39) = cont_VV(wf(:,10),wf(:,60)) * den(9)
  A(40) = cont_VV(wf(:,13),wf(:,60)) * den(14)
  A(41) = cont_VV(wf(:,13),wf(:,61)) * den(47)
  A(42) = cont_VV(wf(:,28),wf(:,62)) * den(48)
  A(43) = cont_VV(wf(:,10),wf(:,65)) * den(50)
  A(44) = cont_VV(wf(:,13),wf(:,65)) * den(51)
  A(45) = cont_VV(wf(:,19),wf(:,68)) * den(53)
  A(46) = cont_VV(wf(:,19),wf(:,71)) * den(55)
  A(47) = cont_VV(wf(:,10),wf(:,74)) * den(57)
  A(48) = cont_VV(wf(:,13),wf(:,74)) * den(58)
  A(49) = cont_VV(wf(:,28),wf(:,68)) * den(59)
  A(50) = cont_VV(wf(:,28),wf(:,71)) * den(60)
  A(51) = cont_VV(wf(:,19),wf(:,62)) * den(61)
  A(52) = cont_VV(wf(:,10),wf(:,61)) * den(62)
  A(53) = cont_VV(wf(:,9),wf(:,75)) * den(63)
  A(54) = cont_VV(wf(:,18),wf(:,76)) * den(64)
  A(55) = cont_VV(wf(:,9),wf(:,79)) * den(66)
  A(56) = cont_VV(wf(:,9),wf(:,82)) * den(68)
  A(57) = cont_VV(wf(:,18),wf(:,85)) * den(70)
  A(58) = cont_VV(wf(:,22),wf(:,85)) * den(71)
  A(59) = cont_VV(wf(:,25),wf(:,79)) * den(72)
  A(60) = cont_VV(wf(:,25),wf(:,82)) * den(73)
  A(61) = cont_VV(wf(:,18),wf(:,88)) * den(75)
  A(62) = cont_VV(wf(:,22),wf(:,88)) * den(76)
  A(63) = cont_VV(wf(:,10),wf(:,89)) * den(77)
  A(64) = cont_VV(wf(:,19),wf(:,90)) * den(78)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(64)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(1)+A(2)+A(3)+A(4)+A(5)+A(6)+A(7)+A(8))*f(1))/2._/**/REALKIND
  M1(2) = ((-A(1)-A(2)-A(3)-A(4)-A(5)-A(6)-A(7)-A(8))*f(1))/6._/**/REALKIND

  M2(1) = ((-A(41)-A(42)-A(43)-A(44)-A(45)-A(46)-A(47)-A(48)-A(49)-A(50)-A(51)-A(52)-A(53)-A(54)-A(55)-A(56)-A(57)-A(58)-A(59) &
       -A(60)-A(61)-A(62)-A(63)-A(64))*f(2))/2._/**/REALKIND+((A(25)+A(27)+A(29)+A(30))*f(3))/2._/**/REALKIND+((A(9)+A(11)+A(12) &
       +A(13)+A(17)+A(18)+A(19)+A(21)+A(33)+A(35)+A(36)+A(37))*f(4))/2._/**/REALKIND+((A(20)+A(22)+A(23)+A(24)+A(34)+A(38)+A(39) &
       +A(40))*f(5))/2._/**/REALKIND+((A(10)+A(14)+A(15)+A(16)+A(26)+A(28)+A(31)+A(32))*f(6))/2._/**/REALKIND
  M2(2) = ((A(41)+A(42)+A(43)+A(44)+A(45)+A(46)+A(47)+A(48)+A(49)+A(50)+A(51)+A(52)+A(53)+A(54)+A(55)+A(56)+A(57)+A(58)+A(59) &
       +A(60)+A(61)+A(62)+A(63)+A(64))*f(2))/6._/**/REALKIND+((-A(25)-A(27)-A(29)-A(30))*f(3))/6._/**/REALKIND+((-A(9)-A(11)-A(12) &
       -A(13)-A(17)-A(18)-A(19)-A(21)-A(33)-A(35)-A(36)-A(37))*f(4))/6._/**/REALKIND+((-A(20)-A(22)-A(23)-A(24)-A(34)-A(38)-A(39) &
       -A(40))*f(5))/6._/**/REALKIND+((-A(10)-A(14)-A(15)-A(16)-A(26)-A(28)-A(31)-A(32))*f(6))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppllnnjj_vbs_nexnmxemucdxsx_1_/**/REALKIND
