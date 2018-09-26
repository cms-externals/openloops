
module ol_colourmatrix_ppllnnjj_vbs_nexnmxemuudxdx_1_/**/REALKIND
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
end module ol_colourmatrix_ppllnnjj_vbs_nexnmxemuudxdx_1_/**/REALKIND



module ol_forced_parameters_ppllnnjj_vbs_nexnmxemuudxdx_1_/**/REALKIND
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
end module ol_forced_parameters_ppllnnjj_vbs_nexnmxemuudxdx_1_/**/REALKIND

module ol_loop_ppllnnjj_vbs_nexnmxemuudxdx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(9), c(17)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-8+1:192)
  ! denominators
  complex(REALKIND), save :: den(271)
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
    f(1) = (CI*eQED**4*gQCD**2)/(4._/**/REALKIND*sw**4)
    f(2) = (CI*countertermnorm*eQED**4*gQCD**4)/(4._/**/REALKIND*sw**4)
    f(3) = (CI*countertermnorm*ctGqq*eQED**4*gQCD**4)/(4._/**/REALKIND*sw**4)
    f(4) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**4)/(4._/**/REALKIND*sw**4)
    f(5) = (CI*eQED**4*gQCD**4*integralnorm*SwB)/(4._/**/REALKIND*sw**4)
    f(6) = (eQED**4*gQCD**4*integralnorm*SwB)/(sw**4*8._/**/REALKIND)
    f(7) = (eQED**4*gQCD**4*integralnorm*SwB)/(sw**4*4._/**/REALKIND)
    f(8) = (eQED**4*gQCD**4*integralnorm*SwF)/(sw**4*4._/**/REALKIND)
    f(9) = (eQED**4*gQCD**4*integralnorm*SwF)/(sw**4*2._/**/REALKIND)

  c = [ 9*CI*f(5), 27*CI*f(5), 18*f(6), 54*f(6), f(7), 3*f(7), 6*f(7), 8*f(7), 10*f(7), 18*f(7), 21*f(7), 24*f(7), 54*f(7), 3*f(8) &
    , 9*f(8), 3*f(9), 9*f(9) ]
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
  complex(REALKIND) :: A(128)
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
  call vert_QA_V(wf(:,8),wf(:,-6),wf(:,11))
  call vert_QA_V(wf(:,7),wf(:,-7),wf(:,12))
  call vert_AW_Q(wf(:,-6),wf(:,4),wf(:,13))
  call prop_A_Q(wf(:,13),Q(:,74),ZERO,0_intkind1,wf(:,14))
  call vert_QA_V(wf(:,-5),wf(:,14),wf(:,15))
  call vert_AW_Q(wf(:,-7),wf(:,4),wf(:,16))
  call prop_A_Q(wf(:,16),Q(:,138),ZERO,0_intkind1,wf(:,17))
  call vert_QA_V(wf(:,-5),wf(:,17),wf(:,18))
  call vert_WQ_A(wf(:,4),wf(:,-4),wf(:,19))
  call vert_WQ_A(wf(:,3),wf(:,-5),wf(:,20))
  call prop_Q_A(wf(:,19),Q(:,26),ZERO,0_intkind1,wf(:,21))
  call prop_Q_A(wf(:,20),Q(:,37),ZERO,0_intkind1,wf(:,22))
  call vert_QA_V(wf(:,21),wf(:,-6),wf(:,23))
  call vert_QA_V(wf(:,22),wf(:,-7),wf(:,24))
  call vert_QA_V(wf(:,22),wf(:,-6),wf(:,25))
  call vert_QA_V(wf(:,21),wf(:,-7),wf(:,26))
  call vert_QA_V(wf(:,-4),wf(:,14),wf(:,27))
  call vert_QA_V(wf(:,-4),wf(:,17),wf(:,28))
  call vert_AW_Q(wf(:,-6),wf(:,3),wf(:,29))
  call prop_A_Q(wf(:,29),Q(:,69),ZERO,0_intkind1,wf(:,30))
  call vert_QA_V(wf(:,-5),wf(:,30),wf(:,31))
  call vert_QA_V(wf(:,-4),wf(:,30),wf(:,32))
  call vert_AW_Q(wf(:,-7),wf(:,3),wf(:,33))
  call prop_A_Q(wf(:,33),Q(:,133),ZERO,0_intkind1,wf(:,34))
  call vert_QA_V(wf(:,-5),wf(:,34),wf(:,35))
  call vert_QA_V(wf(:,-4),wf(:,34),wf(:,36))
  call counter_QA_V(wf(:,8),wf(:,-7),wf(:,37))
  call counter_QA_V(wf(:,7),wf(:,-7),wf(:,38))
  call counter_AW_Q(wf(:,-7),wf(:,4),wf(:,39))
  call prop_A_Q(wf(:,39),Q(:,138),ZERO,0_intkind1,wf(:,40))
  call vert_QA_V(wf(:,-5),wf(:,40),wf(:,41))
  call counter_QA_V(wf(:,22),wf(:,-7),wf(:,42))
  call counter_QA_V(wf(:,21),wf(:,-7),wf(:,43))
  call vert_QA_V(wf(:,-4),wf(:,40),wf(:,44))
  call counter_AW_Q(wf(:,-7),wf(:,3),wf(:,45))
  call prop_A_Q(wf(:,45),Q(:,133),ZERO,0_intkind1,wf(:,46))
  call vert_QA_V(wf(:,-5),wf(:,46),wf(:,47))
  call vert_QA_V(wf(:,-4),wf(:,46),wf(:,48))
  call counter_QA_V(wf(:,8),wf(:,-6),wf(:,49))
  call counter_QA_V(wf(:,7),wf(:,-6),wf(:,50))
  call counter_AW_Q(wf(:,-6),wf(:,4),wf(:,51))
  call prop_A_Q(wf(:,51),Q(:,74),ZERO,0_intkind1,wf(:,52))
  call vert_QA_V(wf(:,-5),wf(:,52),wf(:,53))
  call counter_QA_V(wf(:,22),wf(:,-6),wf(:,54))
  call counter_QA_V(wf(:,21),wf(:,-6),wf(:,55))
  call vert_QA_V(wf(:,-4),wf(:,52),wf(:,56))
  call counter_AW_Q(wf(:,-6),wf(:,3),wf(:,57))
  call prop_A_Q(wf(:,57),Q(:,69),ZERO,0_intkind1,wf(:,58))
  call vert_QA_V(wf(:,-5),wf(:,58),wf(:,59))
  call vert_QA_V(wf(:,-4),wf(:,58),wf(:,60))
  call counter_QA_V(wf(:,-5),wf(:,14),wf(:,61))
  call counter_QA_V(wf(:,-5),wf(:,17),wf(:,62))
  call counter_WQ_A(wf(:,4),wf(:,-5),wf(:,63))
  call prop_Q_A(wf(:,63),Q(:,42),ZERO,0_intkind1,wf(:,64))
  call vert_QA_V(wf(:,64),wf(:,-7),wf(:,65))
  call vert_QA_V(wf(:,64),wf(:,-6),wf(:,66))
  call counter_QA_V(wf(:,-5),wf(:,30),wf(:,67))
  call counter_QA_V(wf(:,-5),wf(:,34),wf(:,68))
  call counter_WQ_A(wf(:,3),wf(:,-5),wf(:,69))
  call prop_Q_A(wf(:,69),Q(:,37),ZERO,0_intkind1,wf(:,70))
  call vert_QA_V(wf(:,70),wf(:,-7),wf(:,71))
  call vert_QA_V(wf(:,70),wf(:,-6),wf(:,72))
  call counter_QA_V(wf(:,-4),wf(:,14),wf(:,73))
  call counter_QA_V(wf(:,-4),wf(:,17),wf(:,74))
  call counter_WQ_A(wf(:,4),wf(:,-4),wf(:,75))
  call prop_Q_A(wf(:,75),Q(:,26),ZERO,0_intkind1,wf(:,76))
  call vert_QA_V(wf(:,76),wf(:,-6),wf(:,77))
  call vert_QA_V(wf(:,76),wf(:,-7),wf(:,78))
  call counter_QA_V(wf(:,-4),wf(:,30),wf(:,79))
  call counter_QA_V(wf(:,-4),wf(:,34),wf(:,80))
  call counter_WQ_A(wf(:,3),wf(:,-4),wf(:,81))
  call prop_Q_A(wf(:,81),Q(:,21),ZERO,0_intkind1,wf(:,82))
  call vert_QA_V(wf(:,82),wf(:,-6),wf(:,83))
  call vert_QA_V(wf(:,82),wf(:,-7),wf(:,84))
  call counter_V_V(ctGG,wf(:,32),Q(:,85),wf(:,85))
  call counter_V_V(ctGG,wf(:,28),Q(:,154),wf(:,86))
  call counter_V_V(ctGG,wf(:,27),Q(:,90),wf(:,87))
  call counter_V_V(ctGG,wf(:,36),Q(:,149),wf(:,88))
  call counter_Q_A(ctqq,wf(:,7),Q(:,21),wf(:,89))
  call prop_Q_A(wf(:,89),Q(:,21),ZERO,0_intkind1,wf(:,90))
  call vert_QA_V(wf(:,90),wf(:,-6),wf(:,91))
  call counter_Q_A(ctqq,wf(:,21),Q(:,26),wf(:,92))
  call prop_Q_A(wf(:,92),Q(:,26),ZERO,0_intkind1,wf(:,93))
  call vert_QA_V(wf(:,93),wf(:,-6),wf(:,94))
  call counter_A_Q(ctqq,wf(:,14),Q(:,74),wf(:,95))
  call prop_A_Q(wf(:,95),Q(:,74),ZERO,0_intkind1,wf(:,96))
  call vert_QA_V(wf(:,-4),wf(:,96),wf(:,97))
  call counter_A_Q(ctqq,wf(:,30),Q(:,69),wf(:,98))
  call prop_A_Q(wf(:,98),Q(:,69),ZERO,0_intkind1,wf(:,99))
  call vert_QA_V(wf(:,-4),wf(:,99),wf(:,100))
  call vert_QA_V(wf(:,90),wf(:,-7),wf(:,101))
  call vert_QA_V(wf(:,93),wf(:,-7),wf(:,102))
  call counter_A_Q(ctqq,wf(:,17),Q(:,138),wf(:,103))
  call prop_A_Q(wf(:,103),Q(:,138),ZERO,0_intkind1,wf(:,104))
  call vert_QA_V(wf(:,-4),wf(:,104),wf(:,105))
  call counter_A_Q(ctqq,wf(:,34),Q(:,133),wf(:,106))
  call prop_A_Q(wf(:,106),Q(:,133),ZERO,0_intkind1,wf(:,107))
  call vert_QA_V(wf(:,-4),wf(:,107),wf(:,108))
  call counter_Q_A(ctqq,wf(:,8),Q(:,42),wf(:,109))
  call prop_Q_A(wf(:,109),Q(:,42),ZERO,0_intkind1,wf(:,110))
  call vert_QA_V(wf(:,110),wf(:,-6),wf(:,111))
  call vert_QA_V(wf(:,-5),wf(:,96),wf(:,112))
  call counter_V_V(ctGG,wf(:,18),Q(:,170),wf(:,113))
  call counter_Q_A(ctqq,wf(:,22),Q(:,37),wf(:,114))
  call prop_Q_A(wf(:,114),Q(:,37),ZERO,0_intkind1,wf(:,115))
  call vert_QA_V(wf(:,115),wf(:,-6),wf(:,116))
  call vert_QA_V(wf(:,-5),wf(:,99),wf(:,117))
  call counter_V_V(ctGG,wf(:,35),Q(:,165),wf(:,118))
  call vert_QA_V(wf(:,110),wf(:,-7),wf(:,119))
  call counter_V_V(ctGG,wf(:,15),Q(:,106),wf(:,120))
  call vert_QA_V(wf(:,-5),wf(:,104),wf(:,121))
  call vert_QA_V(wf(:,115),wf(:,-7),wf(:,122))
  call counter_V_V(ctGG,wf(:,31),Q(:,101),wf(:,123))
  call vert_QA_V(wf(:,-5),wf(:,107),wf(:,124))
  call counter_V_V(ctGG,wf(:,9),Q(:,85),wf(:,125))
  call counter_V_V(ctGG,wf(:,11),Q(:,106),wf(:,126))
  call counter_V_V(ctGG,wf(:,23),Q(:,90),wf(:,127))
  call counter_V_V(ctGG,wf(:,25),Q(:,101),wf(:,128))
  call vert_VQ_A(wf(:,9),wf(:,-5),wf(:,129))
  call prop_Q_A(wf(:,129),Q(:,117),ZERO,0_intkind1,wf(:,130))
  call vert_AV_Q(wf(:,-7),wf(:,9),wf(:,131))
  call prop_A_Q(wf(:,131),Q(:,213),ZERO,0_intkind1,wf(:,132))
  call vert_VQ_A(wf(:,12),wf(:,-5),wf(:,133))
  call prop_Q_A(wf(:,133),Q(:,181),ZERO,0_intkind1,wf(:,134))
  call vert_AV_Q(wf(:,-6),wf(:,12),wf(:,135))
  call prop_A_Q(wf(:,135),Q(:,213),ZERO,0_intkind1,wf(:,136))
  call vert_VQ_A(wf(:,25),wf(:,-4),wf(:,137))
  call prop_Q_A(wf(:,137),Q(:,117),ZERO,0_intkind1,wf(:,138))
  call vert_VQ_A(wf(:,24),wf(:,-4),wf(:,139))
  call prop_Q_A(wf(:,139),Q(:,181),ZERO,0_intkind1,wf(:,140))
  call vert_AV_Q(wf(:,-7),wf(:,25),wf(:,141))
  call prop_A_Q(wf(:,141),Q(:,229),ZERO,0_intkind1,wf(:,142))
  call vert_AV_Q(wf(:,-6),wf(:,24),wf(:,143))
  call prop_A_Q(wf(:,143),Q(:,229),ZERO,0_intkind1,wf(:,144))
  call vert_VQ_A(wf(:,32),wf(:,-5),wf(:,145))
  call prop_Q_A(wf(:,145),Q(:,117),ZERO,0_intkind1,wf(:,146))
  call vert_VQ_A(wf(:,31),wf(:,-4),wf(:,147))
  call prop_Q_A(wf(:,147),Q(:,117),ZERO,0_intkind1,wf(:,148))
  call vert_AV_Q(wf(:,-7),wf(:,32),wf(:,149))
  call prop_A_Q(wf(:,149),Q(:,213),ZERO,0_intkind1,wf(:,150))
  call vert_AV_Q(wf(:,-7),wf(:,31),wf(:,151))
  call prop_A_Q(wf(:,151),Q(:,229),ZERO,0_intkind1,wf(:,152))
  call vert_VQ_A(wf(:,36),wf(:,-5),wf(:,153))
  call prop_Q_A(wf(:,153),Q(:,181),ZERO,0_intkind1,wf(:,154))
  call vert_VQ_A(wf(:,35),wf(:,-4),wf(:,155))
  call prop_Q_A(wf(:,155),Q(:,181),ZERO,0_intkind1,wf(:,156))
  call vert_AV_Q(wf(:,-6),wf(:,36),wf(:,157))
  call prop_A_Q(wf(:,157),Q(:,213),ZERO,0_intkind1,wf(:,158))
  call vert_AV_Q(wf(:,-6),wf(:,35),wf(:,159))
  call prop_A_Q(wf(:,159),Q(:,229),ZERO,0_intkind1,wf(:,160))
  call vert_VQ_A(wf(:,23),wf(:,-5),wf(:,161))
  call prop_Q_A(wf(:,161),Q(:,122),ZERO,0_intkind1,wf(:,162))
  call vert_AV_Q(wf(:,-7),wf(:,23),wf(:,163))
  call prop_A_Q(wf(:,163),Q(:,218),ZERO,0_intkind1,wf(:,164))
  call vert_VQ_A(wf(:,26),wf(:,-5),wf(:,165))
  call prop_Q_A(wf(:,165),Q(:,186),ZERO,0_intkind1,wf(:,166))
  call vert_AV_Q(wf(:,-6),wf(:,26),wf(:,167))
  call prop_A_Q(wf(:,167),Q(:,218),ZERO,0_intkind1,wf(:,168))
  call vert_VQ_A(wf(:,11),wf(:,-4),wf(:,169))
  call prop_Q_A(wf(:,169),Q(:,122),ZERO,0_intkind1,wf(:,170))
  call vert_VQ_A(wf(:,10),wf(:,-4),wf(:,171))
  call prop_Q_A(wf(:,171),Q(:,186),ZERO,0_intkind1,wf(:,172))
  call vert_AV_Q(wf(:,-7),wf(:,11),wf(:,173))
  call prop_A_Q(wf(:,173),Q(:,234),ZERO,0_intkind1,wf(:,174))
  call vert_AV_Q(wf(:,-6),wf(:,10),wf(:,175))
  call prop_A_Q(wf(:,175),Q(:,234),ZERO,0_intkind1,wf(:,176))
  call vert_VQ_A(wf(:,27),wf(:,-5),wf(:,177))
  call prop_Q_A(wf(:,177),Q(:,122),ZERO,0_intkind1,wf(:,178))
  call vert_VQ_A(wf(:,15),wf(:,-4),wf(:,179))
  call prop_Q_A(wf(:,179),Q(:,122),ZERO,0_intkind1,wf(:,180))
  call vert_AV_Q(wf(:,-7),wf(:,27),wf(:,181))
  call prop_A_Q(wf(:,181),Q(:,218),ZERO,0_intkind1,wf(:,182))
  call vert_AV_Q(wf(:,-7),wf(:,15),wf(:,183))
  call prop_A_Q(wf(:,183),Q(:,234),ZERO,0_intkind1,wf(:,184))
  call vert_VQ_A(wf(:,28),wf(:,-5),wf(:,185))
  call prop_Q_A(wf(:,185),Q(:,186),ZERO,0_intkind1,wf(:,186))
  call vert_VQ_A(wf(:,18),wf(:,-4),wf(:,187))
  call prop_Q_A(wf(:,187),Q(:,186),ZERO,0_intkind1,wf(:,188))
  call vert_AV_Q(wf(:,-6),wf(:,28),wf(:,189))
  call prop_A_Q(wf(:,189),Q(:,218),ZERO,0_intkind1,wf(:,190))
  call vert_AV_Q(wf(:,-6),wf(:,18),wf(:,191))
  call prop_A_Q(wf(:,191),Q(:,234),ZERO,0_intkind1,wf(:,192))

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
  den(10) = 1 / (Q(5,106))
  den(13) = 1 / (Q(5,74))
  den(17) = 1 / (Q(5,138))
  den(19) = 1 / (Q(5,170))
  den(22) = 1 / (Q(5,26))
  den(24) = 1 / (Q(5,37))
  den(26) = 1 / (Q(5,90))
  den(29) = 1 / (Q(5,101))
  den(34) = 1 / (Q(5,154))
  den(37) = 1 / (Q(5,69))
  den(45) = 1 / (Q(5,133))
  den(47) = 1 / (Q(5,165))
  den(50) = 1 / (Q(5,149))
  den(131) = 1 / (Q(5,117))
  den(133) = 1 / (Q(5,213))
  den(135) = 1 / (Q(5,181))
  den(140) = 1 / (Q(5,229))
  den(151) = 1 / (Q(5,122))
  den(153) = 1 / (Q(5,218))
  den(155) = 1 / (Q(5,186))
  den(160) = 1 / (Q(5,234))

  ! denominators
  den(4) = den(1)*den(3)
  den(6) = den(2)*den(5)
  den(8) = den(4)*den(7)
  den(9) = den(6)*den(8)
  den(11) = den(6)*den(10)
  den(12) = den(4)*den(11)
  den(14) = den(2)*den(13)
  den(15) = den(10)*den(14)
  den(16) = den(4)*den(15)
  den(18) = den(2)*den(17)
  den(20) = den(18)*den(19)
  den(21) = den(4)*den(20)
  den(23) = den(2)*den(22)
  den(25) = den(1)*den(24)
  den(27) = den(23)*den(26)
  den(28) = den(25)*den(27)
  den(30) = den(25)*den(29)
  den(31) = den(23)*den(30)
  den(32) = den(14)*den(26)
  den(33) = den(25)*den(32)
  den(35) = den(18)*den(34)
  den(36) = den(25)*den(35)
  den(38) = den(1)*den(37)
  den(39) = den(29)*den(38)
  den(40) = den(23)*den(39)
  den(41) = den(7)*den(38)
  den(42) = den(6)*den(41)
  den(43) = den(18)*den(41)
  den(44) = den(35)*den(38)
  den(46) = den(1)*den(45)
  den(48) = den(46)*den(47)
  den(49) = den(23)*den(48)
  den(51) = den(46)*den(50)
  den(52) = den(6)*den(51)
  den(53) = den(32)*den(46)
  den(54) = den(14)*den(51)
  den(55) = den(4)*den(50)
  den(56) = den(6)*den(55)
  den(57) = den(6)*den(19)
  den(58) = den(4)*den(57)
  den(59) = den(23)*den(34)
  den(60) = den(25)*den(59)
  den(61) = den(25)*den(47)
  den(62) = den(23)*den(61)
  den(63) = den(14)*den(55)
  den(64) = den(8)*den(18)
  den(65) = den(38)*den(59)
  den(66) = den(27)*den(46)
  den(67) = den(14)*den(61)
  den(68) = den(18)*den(30)
  den(69) = den(38)*den(57)
  den(70) = den(20)*den(38)
  den(71) = den(18)*den(39)
  den(72) = den(11)*den(46)
  den(73) = den(14)*den(48)
  den(74) = den(15)*den(46)
  den(75) = den(20)*den(41)
  den(76) = den(35)*den(39)
  den(77) = den(32)*den(48)
  den(78) = den(15)*den(51)
  den(79) = den(3)*den(4)
  den(80) = den(57)*den(79)
  den(81) = den(20)*den(79)
  den(82) = den(22)*den(23)
  den(83) = den(61)*den(82)
  den(84) = den(13)*den(14)
  den(85) = den(61)*den(84)
  den(86) = den(30)*den(35)
  den(87) = den(37)*den(38)
  den(88) = den(57)*den(87)
  den(89) = den(20)*den(87)
  den(90) = den(48)*den(82)
  den(91) = den(11)*den(51)
  den(92) = den(48)*den(84)
  den(93) = den(11)*den(79)
  den(94) = den(15)*den(79)
  den(95) = den(30)*den(82)
  den(96) = den(32)*den(61)
  den(97) = den(17)*den(18)
  den(98) = den(30)*den(97)
  den(99) = den(39)*den(82)
  den(100) = den(41)*den(57)
  den(101) = den(39)*den(97)
  den(102) = den(45)*den(46)
  den(103) = den(11)*den(102)
  den(104) = den(15)*den(102)
  den(105) = den(5)*den(6)
  den(106) = den(55)*den(105)
  den(107) = den(55)*den(84)
  den(108) = den(8)*den(20)
  den(109) = den(24)*den(25)
  den(110) = den(59)*den(109)
  den(111) = den(35)*den(109)
  den(112) = den(59)*den(87)
  den(113) = den(35)*den(87)
  den(114) = den(27)*den(48)
  den(115) = den(51)*den(105)
  den(116) = den(51)*den(84)
  den(117) = den(8)*den(105)
  den(118) = den(15)*den(55)
  den(119) = den(8)*den(97)
  den(120) = den(27)*den(109)
  den(121) = den(32)*den(109)
  den(122) = den(39)*den(59)
  den(123) = den(41)*den(105)
  den(124) = den(41)*den(97)
  den(125) = den(27)*den(102)
  den(126) = den(32)*den(102)
  den(127) = den(8)*den(57)
  den(128) = den(11)*den(55)
  den(129) = den(27)*den(61)
  den(130) = den(30)*den(59)
  den(132) = den(8)*den(131)
  den(134) = den(8)*den(133)
  den(136) = den(55)*den(135)
  den(137) = den(55)*den(133)
  den(138) = den(30)*den(131)
  den(139) = den(61)*den(135)
  den(141) = den(30)*den(140)
  den(142) = den(61)*den(140)
  den(143) = den(41)*den(131)
  den(144) = den(39)*den(131)
  den(145) = den(41)*den(133)
  den(146) = den(39)*den(140)
  den(147) = den(51)*den(135)
  den(148) = den(48)*den(135)
  den(149) = den(51)*den(133)
  den(150) = den(48)*den(140)
  den(152) = den(27)*den(151)
  den(154) = den(27)*den(153)
  den(156) = den(59)*den(155)
  den(157) = den(59)*den(153)
  den(158) = den(11)*den(151)
  den(159) = den(57)*den(155)
  den(161) = den(11)*den(160)
  den(162) = den(57)*den(160)
  den(163) = den(32)*den(151)
  den(164) = den(15)*den(151)
  den(165) = den(32)*den(153)
  den(166) = den(15)*den(160)
  den(167) = den(35)*den(155)
  den(168) = den(20)*den(155)
  den(169) = den(35)*den(153)
  den(170) = den(20)*den(160)
  den(171) = den(4)*den(6)
  den(172) = den(4)*den(14)
  den(173) = den(4)*den(18)
  den(174) = den(2)*den(132)
  den(175) = den(2)*den(134)
  den(176) = den(2)*den(8)
  den(177) = den(2)*den(136)
  den(178) = den(2)*den(137)
  den(179) = den(2)*den(55)
  den(180) = den(2)*den(4)
  den(181) = den(23)*den(25)
  den(182) = den(14)*den(25)
  den(183) = den(18)*den(25)
  den(184) = den(2)*den(138)
  den(185) = den(2)*den(139)
  den(186) = den(2)*den(141)
  den(187) = den(2)*den(30)
  den(188) = den(2)*den(142)
  den(189) = den(2)*den(61)
  den(190) = den(2)*den(25)
  den(191) = den(23)*den(38)
  den(192) = den(6)*den(38)
  den(193) = den(18)*den(38)
  den(194) = den(2)*den(143)
  den(195) = den(2)*den(144)
  den(196) = den(2)*den(145)
  den(197) = den(2)*den(41)
  den(198) = den(2)*den(146)
  den(199) = den(2)*den(39)
  den(200) = den(2)*den(38)
  den(201) = den(23)*den(46)
  den(202) = den(6)*den(46)
  den(203) = den(14)*den(46)
  den(204) = den(2)*den(147)
  den(205) = den(2)*den(148)
  den(206) = den(2)*den(149)
  den(207) = den(2)*den(51)
  den(208) = den(2)*den(150)
  den(209) = den(2)*den(48)
  den(210) = den(2)*den(46)
  den(211) = den(1)*den(152)
  den(212) = den(1)*den(154)
  den(213) = den(1)*den(27)
  den(214) = den(1)*den(156)
  den(215) = den(1)*den(157)
  den(216) = den(1)*den(59)
  den(217) = den(1)*den(23)
  den(218) = den(1)*den(158)
  den(219) = den(1)*den(159)
  den(220) = den(1)*den(161)
  den(221) = den(1)*den(11)
  den(222) = den(1)*den(162)
  den(223) = den(1)*den(57)
  den(224) = den(1)*den(6)
  den(225) = den(1)*den(163)
  den(226) = den(1)*den(164)
  den(227) = den(1)*den(165)
  den(228) = den(1)*den(32)
  den(229) = den(1)*den(166)
  den(230) = den(1)*den(15)
  den(231) = den(1)*den(14)
  den(232) = den(1)*den(167)
  den(233) = den(1)*den(168)
  den(234) = den(1)*den(169)
  den(235) = den(1)*den(35)
  den(236) = den(1)*den(170)
  den(237) = den(1)*den(20)
  den(238) = den(1)*den(18)
  den(239) = den(1)*den(2)
  den(240) = den(6)*den(134)
  den(241) = den(6)*den(137)
  den(242) = den(4)*den(161)
  den(243) = den(4)*den(162)
  den(244) = den(14)*den(136)
  den(245) = den(4)*den(166)
  den(246) = den(18)*den(132)
  den(247) = den(4)*den(170)
  den(248) = den(25)*den(154)
  den(249) = den(25)*den(157)
  den(250) = den(23)*den(141)
  den(251) = den(23)*den(142)
  den(252) = den(25)*den(165)
  den(253) = den(14)*den(139)
  den(254) = den(25)*den(169)
  den(255) = den(18)*den(138)
  den(256) = den(38)*den(156)
  den(257) = den(23)*den(146)
  den(258) = den(6)*den(145)
  den(259) = den(38)*den(159)
  den(260) = den(18)*den(143)
  den(261) = den(38)*den(167)
  den(262) = den(18)*den(144)
  den(263) = den(38)*den(168)
  den(264) = den(46)*den(152)
  den(265) = den(23)*den(150)
  den(266) = den(6)*den(149)
  den(267) = den(46)*den(158)
  den(268) = den(46)*den(163)
  den(269) = den(14)*den(147)
  den(270) = den(46)*den(164)
  den(271) = den(14)*den(148)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(128)

  A(1) = cont_VV(wf(:,9),wf(:,10)) * den(9)
  A(2) = cont_VV(wf(:,11),wf(:,12)) * den(12)
  A(3) = cont_VV(wf(:,12),wf(:,15)) * den(16)
  A(4) = cont_VV(wf(:,9),wf(:,18)) * den(21)
  A(5) = cont_VV(wf(:,23),wf(:,24)) * den(28)
  A(6) = cont_VV(wf(:,25),wf(:,26)) * den(31)
  A(7) = cont_VV(wf(:,24),wf(:,27)) * den(33)
  A(8) = cont_VV(wf(:,25),wf(:,28)) * den(36)
  A(9) = cont_VV(wf(:,26),wf(:,31)) * den(40)
  A(10) = cont_VV(wf(:,10),wf(:,32)) * den(42)
  A(11) = cont_VV(wf(:,18),wf(:,32)) * den(43)
  A(12) = cont_VV(wf(:,28),wf(:,31)) * den(44)
  A(13) = cont_VV(wf(:,23),wf(:,35)) * den(49)
  A(14) = cont_VV(wf(:,11),wf(:,36)) * den(52)
  A(15) = cont_VV(wf(:,27),wf(:,35)) * den(53)
  A(16) = cont_VV(wf(:,15),wf(:,36)) * den(54)

  A(17) = cont_VV(wf(:,9),wf(:,37)) * den(9)
  A(18) = cont_VV(wf(:,11),wf(:,38)) * den(12)
  A(19) = cont_VV(wf(:,15),wf(:,38)) * den(16)
  A(20) = cont_VV(wf(:,9),wf(:,41)) * den(21)
  A(21) = cont_VV(wf(:,23),wf(:,42)) * den(28)
  A(22) = cont_VV(wf(:,25),wf(:,43)) * den(31)
  A(23) = cont_VV(wf(:,27),wf(:,42)) * den(33)
  A(24) = cont_VV(wf(:,25),wf(:,44)) * den(36)
  A(25) = cont_VV(wf(:,31),wf(:,43)) * den(40)
  A(26) = cont_VV(wf(:,32),wf(:,37)) * den(42)
  A(27) = cont_VV(wf(:,32),wf(:,41)) * den(43)
  A(28) = cont_VV(wf(:,31),wf(:,44)) * den(44)
  A(29) = cont_VV(wf(:,23),wf(:,47)) * den(49)
  A(30) = cont_VV(wf(:,11),wf(:,48)) * den(52)
  A(31) = cont_VV(wf(:,27),wf(:,47)) * den(53)
  A(32) = cont_VV(wf(:,15),wf(:,48)) * den(54)
  A(33) = cont_VV(wf(:,12),wf(:,49)) * den(56)
  A(34) = cont_VV(wf(:,10),wf(:,50)) * den(58)
  A(35) = cont_VV(wf(:,18),wf(:,50)) * den(21)
  A(36) = cont_VV(wf(:,12),wf(:,53)) * den(16)
  A(37) = cont_VV(wf(:,26),wf(:,54)) * den(60)
  A(38) = cont_VV(wf(:,24),wf(:,55)) * den(62)
  A(39) = cont_VV(wf(:,28),wf(:,54)) * den(36)
  A(40) = cont_VV(wf(:,24),wf(:,56)) * den(33)
  A(41) = cont_VV(wf(:,35),wf(:,55)) * den(49)
  A(42) = cont_VV(wf(:,36),wf(:,49)) * den(52)
  A(43) = cont_VV(wf(:,36),wf(:,53)) * den(54)
  A(44) = cont_VV(wf(:,35),wf(:,56)) * den(53)
  A(45) = cont_VV(wf(:,26),wf(:,59)) * den(40)
  A(46) = cont_VV(wf(:,10),wf(:,60)) * den(42)
  A(47) = cont_VV(wf(:,28),wf(:,59)) * den(44)
  A(48) = cont_VV(wf(:,18),wf(:,60)) * den(43)
  A(49) = cont_VV(wf(:,12),wf(:,61)) * den(63)
  A(50) = cont_VV(wf(:,9),wf(:,62)) * den(64)
  A(51) = cont_VV(wf(:,9),wf(:,65)) * den(9)
  A(52) = cont_VV(wf(:,12),wf(:,66)) * den(12)
  A(53) = cont_VV(wf(:,26),wf(:,67)) * den(65)
  A(54) = cont_VV(wf(:,32),wf(:,62)) * den(43)
  A(55) = cont_VV(wf(:,28),wf(:,67)) * den(44)
  A(56) = cont_VV(wf(:,32),wf(:,65)) * den(42)
  A(57) = cont_VV(wf(:,23),wf(:,68)) * den(66)
  A(58) = cont_VV(wf(:,27),wf(:,68)) * den(53)
  A(59) = cont_VV(wf(:,36),wf(:,61)) * den(54)
  A(60) = cont_VV(wf(:,36),wf(:,66)) * den(52)
  A(61) = cont_VV(wf(:,23),wf(:,71)) * den(28)
  A(62) = cont_VV(wf(:,26),wf(:,72)) * den(31)
  A(63) = cont_VV(wf(:,27),wf(:,71)) * den(33)
  A(64) = cont_VV(wf(:,28),wf(:,72)) * den(36)
  A(65) = cont_VV(wf(:,24),wf(:,73)) * den(67)
  A(66) = cont_VV(wf(:,25),wf(:,74)) * den(68)
  A(67) = cont_VV(wf(:,24),wf(:,77)) * den(28)
  A(68) = cont_VV(wf(:,25),wf(:,78)) * den(31)
  A(69) = cont_VV(wf(:,10),wf(:,79)) * den(69)
  A(70) = cont_VV(wf(:,18),wf(:,79)) * den(70)
  A(71) = cont_VV(wf(:,31),wf(:,74)) * den(71)
  A(72) = cont_VV(wf(:,31),wf(:,78)) * den(40)
  A(73) = cont_VV(wf(:,11),wf(:,80)) * den(72)
  A(74) = cont_VV(wf(:,35),wf(:,73)) * den(73)
  A(75) = cont_VV(wf(:,15),wf(:,80)) * den(74)
  A(76) = cont_VV(wf(:,35),wf(:,77)) * den(49)
  A(77) = cont_VV(wf(:,10),wf(:,83)) * den(9)
  A(78) = cont_VV(wf(:,11),wf(:,84)) * den(12)
  A(79) = cont_VV(wf(:,15),wf(:,84)) * den(16)
  A(80) = cont_VV(wf(:,18),wf(:,83)) * den(21)
  A(81) = cont_VV(wf(:,18),wf(:,85)) * den(75)
  A(82) = cont_VV(wf(:,31),wf(:,86)) * den(76)
  A(83) = cont_VV(wf(:,35),wf(:,87)) * den(77)
  A(84) = cont_VV(wf(:,15),wf(:,88)) * den(78)
  A(85) = cont_VV(wf(:,10),wf(:,91)) * den(80)
  A(86) = cont_VV(wf(:,18),wf(:,91)) * den(81)
  A(87) = cont_VV(wf(:,24),wf(:,94)) * den(83)
  A(88) = cont_VV(wf(:,24),wf(:,97)) * den(85)
  A(89) = cont_VV(wf(:,25),wf(:,86)) * den(86)
  A(90) = cont_VV(wf(:,10),wf(:,100)) * den(88)
  A(91) = cont_VV(wf(:,18),wf(:,100)) * den(89)
  A(92) = cont_VV(wf(:,35),wf(:,94)) * den(90)
  A(93) = cont_VV(wf(:,11),wf(:,88)) * den(91)
  A(94) = cont_VV(wf(:,35),wf(:,97)) * den(92)
  A(95) = cont_VV(wf(:,11),wf(:,101)) * den(93)
  A(96) = cont_VV(wf(:,15),wf(:,101)) * den(94)
  A(97) = cont_VV(wf(:,25),wf(:,102)) * den(95)
  A(98) = cont_VV(wf(:,24),wf(:,87)) * den(96)
  A(99) = cont_VV(wf(:,25),wf(:,105)) * den(98)
  A(100) = cont_VV(wf(:,31),wf(:,102)) * den(99)
  A(101) = cont_VV(wf(:,10),wf(:,85)) * den(100)
  A(102) = cont_VV(wf(:,31),wf(:,105)) * den(101)
  A(103) = cont_VV(wf(:,11),wf(:,108)) * den(103)
  A(104) = cont_VV(wf(:,15),wf(:,108)) * den(104)
  A(105) = cont_VV(wf(:,12),wf(:,111)) * den(106)
  A(106) = cont_VV(wf(:,12),wf(:,112)) * den(107)
  A(107) = cont_VV(wf(:,9),wf(:,113)) * den(108)
  A(108) = cont_VV(wf(:,26),wf(:,116)) * den(110)
  A(109) = cont_VV(wf(:,28),wf(:,116)) * den(111)
  A(110) = cont_VV(wf(:,26),wf(:,117)) * den(112)
  A(111) = cont_VV(wf(:,28),wf(:,117)) * den(113)
  A(112) = cont_VV(wf(:,23),wf(:,118)) * den(114)
  A(113) = cont_VV(wf(:,36),wf(:,111)) * den(115)
  A(114) = cont_VV(wf(:,36),wf(:,112)) * den(116)
  A(115) = cont_VV(wf(:,9),wf(:,119)) * den(117)
  A(116) = cont_VV(wf(:,12),wf(:,120)) * den(118)
  A(117) = cont_VV(wf(:,9),wf(:,121)) * den(119)
  A(118) = cont_VV(wf(:,23),wf(:,122)) * den(120)
  A(119) = cont_VV(wf(:,27),wf(:,122)) * den(121)
  A(120) = cont_VV(wf(:,26),wf(:,123)) * den(122)
  A(121) = cont_VV(wf(:,32),wf(:,119)) * den(123)
  A(122) = cont_VV(wf(:,32),wf(:,121)) * den(124)
  A(123) = cont_VV(wf(:,23),wf(:,124)) * den(125)
  A(124) = cont_VV(wf(:,27),wf(:,124)) * den(126)
  A(125) = cont_VV(wf(:,10),wf(:,125)) * den(127)
  A(126) = cont_VV(wf(:,12),wf(:,126)) * den(128)
  A(127) = cont_VV(wf(:,24),wf(:,127)) * den(129)
  A(128) = cont_VV(wf(:,26),wf(:,128)) * den(130)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(128)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(1)+A(4)+A(5)+A(7)+A(10)+A(11)+A(13)+A(15))*f(1))/2._/**/REALKIND+((A(2)+A(3)+A(6)+A(8)+A(9)+A(12)+A(14) &
       +A(16))*f(1))/6._/**/REALKIND
  M1(2) = ((-A(1)-A(4)-A(5)-A(7)-A(10)-A(11)-A(13)-A(15))*f(1))/6._/**/REALKIND+((-A(2)-A(3)-A(6)-A(8)-A(9)-A(12)-A(14) &
       -A(16))*f(1))/2._/**/REALKIND

  M2(1) = ((-A(81)-A(83)-A(85)-A(86)-A(87)-A(88)-A(90)-A(91)-A(92)-A(94)-A(98)-A(101)-A(107)-A(112)-A(115)-A(117)-A(118)-A(119) &
       -A(121)-A(122)-A(123)-A(124)-A(125)-A(127))*f(2))/2._/**/REALKIND+((-A(82)-A(84)-A(89)-A(93)-A(95)-A(96)-A(97)-A(99)-A(100) &
       -A(102)-A(103)-A(104)-A(105)-A(106)-A(108)-A(109)-A(110)-A(111)-A(113)-A(114)-A(116)-A(120)-A(126) &
       -A(128))*f(2))/6._/**/REALKIND+((A(17)+A(21)+A(23)+A(26)+A(34)+A(35)+A(38)+A(41)+A(50)+A(54)+A(57)+A(58)+A(65)+A(69)+A(70) &
       +A(74))*f(3))/2._/**/REALKIND+((A(18)+A(19)+A(22)+A(25)+A(33)+A(37)+A(39)+A(42)+A(49)+A(53)+A(55)+A(59)+A(66)+A(71)+A(73) &
       +A(75))*f(3))/6._/**/REALKIND+((A(24)+A(28)+A(30)+A(32)+A(36)+A(43)+A(45)+A(47)+A(52)+A(60)+A(62)+A(64)+A(68)+A(72)+A(78) &
       +A(79))*f(4))/6._/**/REALKIND+((A(20)+A(27)+A(29)+A(31)+A(40)+A(44)+A(46)+A(48)+A(51)+A(56)+A(61)+A(63)+A(67)+A(76)+A(77) &
       +A(80))*f(4))/2._/**/REALKIND
  M2(2) = ((A(81)+A(83)+A(85)+A(86)+A(87)+A(88)+A(90)+A(91)+A(92)+A(94)+A(98)+A(101)+A(107)+A(112)+A(115)+A(117)+A(118)+A(119) &
       +A(121)+A(122)+A(123)+A(124)+A(125)+A(127))*f(2))/6._/**/REALKIND+((A(82)+A(84)+A(89)+A(93)+A(95)+A(96)+A(97)+A(99)+A(100) &
       +A(102)+A(103)+A(104)+A(105)+A(106)+A(108)+A(109)+A(110)+A(111)+A(113)+A(114)+A(116)+A(120)+A(126) &
       +A(128))*f(2))/2._/**/REALKIND+((-A(17)-A(21)-A(23)-A(26)-A(34)-A(35)-A(38)-A(41)-A(50)-A(54)-A(57)-A(58)-A(65)-A(69)-A(70) &
       -A(74))*f(3))/6._/**/REALKIND+((-A(18)-A(19)-A(22)-A(25)-A(33)-A(37)-A(39)-A(42)-A(49)-A(53)-A(55)-A(59)-A(66)-A(71)-A(73) &
       -A(75))*f(3))/2._/**/REALKIND+((-A(24)-A(28)-A(30)-A(32)-A(36)-A(43)-A(45)-A(47)-A(52)-A(60)-A(62)-A(64)-A(68)-A(72)-A(78) &
       -A(79))*f(4))/2._/**/REALKIND+((-A(20)-A(27)-A(29)-A(31)-A(40)-A(44)-A(46)-A(48)-A(51)-A(56)-A(61)-A(63)-A(67)-A(76)-A(77) &
       -A(80))*f(4))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppllnnjj_vbs_nexnmxemuudxdx_1_/**/REALKIND
