
module ol_colourmatrix_ppllllj_nenexnexeudxg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllllj_nenexnexeudxg_1_/**/REALKIND



module ol_forced_parameters_ppllllj_nenexnexeudxg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj_nenexnexeudxg_1_/**/REALKIND

module ol_loop_ppllllj_nenexnexeudxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(21), c(11)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:220)
  ! denominators
  complex(REALKIND), save :: den(275)
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
  complex(REALKIND) :: A(140)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_Q(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_W(wf(:,-3),wf(:,-2),wf(:,2))
  call vert_VQ_A(wf(:,-6),wf(:,-4),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call prop_W_W(wf(:,2),Q(:,12),MW,1_intkind1,wf(:,5))
  call prop_Q_A(wf(:,3),Q(:,80),ZERO,0_intkind1,wf(:,6))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,7))
  call vert_WQ_A(wf(:,5),wf(:,6),wf(:,8))
  call prop_A_Q(wf(:,7),Q(:,35),ZERO,0_intkind1,wf(:,9))
  call vert_AW_Q(wf(:,-5),wf(:,5),wf(:,10))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,6),wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,44),ZERO,0_intkind1,wf(:,12))
  call vert_QA_W(wf(:,6),wf(:,-5),wf(:,13))
  call vert_UV_W(wf(:,5),Q(:,12),wf(:,4),Q(:,3),wf(:,14))
  call prop_W_W(wf(:,13),Q(:,112),MW,1_intkind1,wf(:,15))
  call vert_AV_Q(wf(:,-5),wf(:,-6),wf(:,16))
  call prop_A_Q(wf(:,16),Q(:,96),ZERO,0_intkind1,wf(:,17))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,-4),wf(:,18))
  call vert_AW_Q(wf(:,17),wf(:,5),wf(:,19))
  call prop_Q_A(wf(:,18),Q(:,19),ZERO,0_intkind1,wf(:,20))
  call vert_WQ_A(wf(:,5),wf(:,-4),wf(:,21))
  call vert_AZ_Q(gZd,wf(:,17),wf(:,4),wf(:,22))
  call prop_Q_A(wf(:,21),Q(:,28),ZERO,0_intkind1,wf(:,23))
  call vert_QA_W(wf(:,-4),wf(:,17),wf(:,24))
  call prop_W_W(wf(:,24),Q(:,112),MW,1_intkind1,wf(:,25))
  call vert_VQ_A(wf(:,-6),wf(:,20),wf(:,26))
  call vert_VQ_A(wf(:,-6),wf(:,23),wf(:,27))
  call vert_AZ_Q(gZn,wf(:,-2),wf(:,4),wf(:,28))
  call prop_A_Q(wf(:,28),Q(:,7),ZERO,0_intkind1,wf(:,29))
  call vert_QA_W(wf(:,-3),wf(:,29),wf(:,30))
  call vert_ZQ_A(gZl,wf(:,4),wf(:,-3),wf(:,31))
  call prop_Q_A(wf(:,31),Q(:,11),ZERO,0_intkind1,wf(:,32))
  call vert_QA_W(wf(:,32),wf(:,-2),wf(:,33))
  call vert_QA_Z(gZn,wf(:,0),wf(:,-2),wf(:,34))
  call vert_QA_W(wf(:,-3),wf(:,-1),wf(:,35))
  call prop_W_W(wf(:,34),Q(:,5),MZ,1_intkind1,wf(:,36))
  call prop_W_W(wf(:,35),Q(:,10),MW,1_intkind1,wf(:,37))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,36),wf(:,38))
  call vert_WQ_A(wf(:,37),wf(:,6),wf(:,39))
  call prop_A_Q(wf(:,38),Q(:,37),ZERO,0_intkind1,wf(:,40))
  call vert_AW_Q(wf(:,-5),wf(:,37),wf(:,41))
  call vert_ZQ_A(gZu,wf(:,36),wf(:,6),wf(:,42))
  call prop_A_Q(wf(:,41),Q(:,42),ZERO,0_intkind1,wf(:,43))
  call vert_UV_W(wf(:,37),Q(:,10),wf(:,36),Q(:,5),wf(:,44))
  call vert_ZQ_A(gZu,wf(:,36),wf(:,-4),wf(:,45))
  call vert_AW_Q(wf(:,17),wf(:,37),wf(:,46))
  call prop_Q_A(wf(:,45),Q(:,21),ZERO,0_intkind1,wf(:,47))
  call vert_WQ_A(wf(:,37),wf(:,-4),wf(:,48))
  call vert_AZ_Q(gZd,wf(:,17),wf(:,36),wf(:,49))
  call prop_Q_A(wf(:,48),Q(:,26),ZERO,0_intkind1,wf(:,50))
  call vert_VQ_A(wf(:,-6),wf(:,47),wf(:,51))
  call vert_VQ_A(wf(:,-6),wf(:,50),wf(:,52))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,36),wf(:,53))
  call prop_A_Q(wf(:,53),Q(:,7),ZERO,0_intkind1,wf(:,54))
  call vert_QA_W(wf(:,-3),wf(:,54),wf(:,55))
  call vert_ZQ_A(gZl,wf(:,36),wf(:,-3),wf(:,56))
  call prop_Q_A(wf(:,56),Q(:,13),ZERO,0_intkind1,wf(:,57))
  call vert_QA_W(wf(:,57),wf(:,-1),wf(:,58))
  call vert_WQ_A(wf(:,37),wf(:,0),wf(:,59))
  call prop_Q_A(wf(:,59),Q(:,11),ZERO,0_intkind1,wf(:,60))
  call vert_QA_W(wf(:,60),wf(:,-2),wf(:,61))
  call vert_WQ_A(wf(:,5),wf(:,0),wf(:,62))
  call prop_Q_A(wf(:,62),Q(:,13),ZERO,0_intkind1,wf(:,63))
  call vert_QA_W(wf(:,63),wf(:,-1),wf(:,64))
  call counter_WQ_A(wf(:,5),wf(:,6),wf(:,65))
  call counter_ZQ_A(gZu,wf(:,4),wf(:,6),wf(:,66))
  call counter_AW_Q(wf(:,17),wf(:,5),wf(:,67))
  call counter_AZ_Q(gZd,wf(:,17),wf(:,4),wf(:,68))
  call counter_VQ_A(wf(:,-6),wf(:,20),wf(:,69))
  call counter_VQ_A(wf(:,-6),wf(:,23),wf(:,70))
  call counter_VG_G(wf(:,4),wf(:,-6),Q(:,64),wf(:,71),Q(:,67))
  call vert_QA_V(wf(:,23),wf(:,-5),wf(:,72))
  call vert_QA_V(wf(:,-4),wf(:,12),wf(:,73))
  call counter_QA_W(wf(:,6),wf(:,-5),wf(:,74))
  call prop_W_W(wf(:,14),Q(:,15),MW,1_intkind1,wf(:,75))
  call counter_AW_Q(wf(:,-5),wf(:,5),wf(:,76))
  call prop_Q_A(wf(:,11),Q(:,83),ZERO,0_intkind1,wf(:,77))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,78))
  call prop_Q_A(wf(:,8),Q(:,92),ZERO,0_intkind1,wf(:,79))
  call prop_A_Q(wf(:,76),Q(:,44),ZERO,0_intkind1,wf(:,80))
  call prop_A_Q(wf(:,78),Q(:,35),ZERO,0_intkind1,wf(:,81))
  call counter_AV_Q(wf(:,-5),wf(:,-6),wf(:,82))
  call prop_A_Q(wf(:,82),Q(:,96),ZERO,0_intkind1,wf(:,83))
  call vert_AW_Q(wf(:,83),wf(:,5),wf(:,84))
  call vert_AZ_Q(gZd,wf(:,83),wf(:,4),wf(:,85))
  call vert_QA_W(wf(:,-4),wf(:,83),wf(:,86))
  call prop_W_W(wf(:,86),Q(:,112),MW,1_intkind1,wf(:,87))
  call counter_QA_W(wf(:,-4),wf(:,17),wf(:,88))
  call counter_WQ_A(wf(:,5),wf(:,-4),wf(:,89))
  call prop_A_Q(wf(:,22),Q(:,99),ZERO,0_intkind1,wf(:,90))
  call counter_ZQ_A(gZu,wf(:,4),wf(:,-4),wf(:,91))
  call prop_A_Q(wf(:,19),Q(:,108),ZERO,0_intkind1,wf(:,92))
  call prop_Q_A(wf(:,89),Q(:,28),ZERO,0_intkind1,wf(:,93))
  call vert_VQ_A(wf(:,-6),wf(:,93),wf(:,94))
  call prop_Q_A(wf(:,91),Q(:,19),ZERO,0_intkind1,wf(:,95))
  call vert_VQ_A(wf(:,-6),wf(:,95),wf(:,96))
  call counter_VQ_A(wf(:,-6),wf(:,-4),wf(:,97))
  call prop_Q_A(wf(:,97),Q(:,80),ZERO,0_intkind1,wf(:,98))
  call vert_WQ_A(wf(:,5),wf(:,98),wf(:,99))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,98),wf(:,100))
  call vert_QA_W(wf(:,98),wf(:,-5),wf(:,101))
  call prop_W_W(wf(:,101),Q(:,112),MW,1_intkind1,wf(:,102))
  call prop_W_W(wf(:,74),Q(:,112),MW,1_intkind1,wf(:,103))
  call prop_W_W(wf(:,88),Q(:,112),MW,1_intkind1,wf(:,104))
  call counter_WQ_A(wf(:,37),wf(:,6),wf(:,105))
  call counter_ZQ_A(gZu,wf(:,36),wf(:,6),wf(:,106))
  call counter_AW_Q(wf(:,17),wf(:,37),wf(:,107))
  call counter_AZ_Q(gZd,wf(:,17),wf(:,36),wf(:,108))
  call counter_VQ_A(wf(:,-6),wf(:,47),wf(:,109))
  call counter_VQ_A(wf(:,-6),wf(:,50),wf(:,110))
  call counter_VG_G(wf(:,36),wf(:,-6),Q(:,64),wf(:,111),Q(:,69))
  call vert_QA_V(wf(:,50),wf(:,-5),wf(:,112))
  call vert_QA_V(wf(:,-4),wf(:,43),wf(:,113))
  call prop_W_W(wf(:,44),Q(:,15),MW,1_intkind1,wf(:,114))
  call counter_AW_Q(wf(:,-5),wf(:,37),wf(:,115))
  call prop_Q_A(wf(:,42),Q(:,85),ZERO,0_intkind1,wf(:,116))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,36),wf(:,117))
  call prop_Q_A(wf(:,39),Q(:,90),ZERO,0_intkind1,wf(:,118))
  call prop_A_Q(wf(:,115),Q(:,42),ZERO,0_intkind1,wf(:,119))
  call prop_A_Q(wf(:,117),Q(:,37),ZERO,0_intkind1,wf(:,120))
  call vert_AW_Q(wf(:,83),wf(:,37),wf(:,121))
  call vert_AZ_Q(gZd,wf(:,83),wf(:,36),wf(:,122))
  call counter_WQ_A(wf(:,37),wf(:,-4),wf(:,123))
  call prop_A_Q(wf(:,49),Q(:,101),ZERO,0_intkind1,wf(:,124))
  call counter_ZQ_A(gZu,wf(:,36),wf(:,-4),wf(:,125))
  call prop_A_Q(wf(:,46),Q(:,106),ZERO,0_intkind1,wf(:,126))
  call prop_Q_A(wf(:,123),Q(:,26),ZERO,0_intkind1,wf(:,127))
  call vert_VQ_A(wf(:,-6),wf(:,127),wf(:,128))
  call prop_Q_A(wf(:,125),Q(:,21),ZERO,0_intkind1,wf(:,129))
  call vert_VQ_A(wf(:,-6),wf(:,129),wf(:,130))
  call vert_WQ_A(wf(:,37),wf(:,98),wf(:,131))
  call vert_ZQ_A(gZu,wf(:,36),wf(:,98),wf(:,132))
  call counter_Q_A(ctqq,wf(:,6),Q(:,80),wf(:,133))
  call prop_Q_A(wf(:,133),Q(:,80),ZERO,0_intkind1,wf(:,134))
  call vert_QA_W(wf(:,134),wf(:,-5),wf(:,135))
  call vert_WQ_A(wf(:,5),wf(:,134),wf(:,136))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,134),wf(:,137))
  call counter_A_Q(ctqq,wf(:,9),Q(:,35),wf(:,138))
  call counter_A_Q(ctqq,wf(:,12),Q(:,44),wf(:,139))
  call counter_A_Q(ctqq,wf(:,17),Q(:,96),wf(:,140))
  call prop_A_Q(wf(:,140),Q(:,96),ZERO,0_intkind1,wf(:,141))
  call vert_QA_W(wf(:,-4),wf(:,141),wf(:,142))
  call counter_Q_A(ctqq,wf(:,20),Q(:,19),wf(:,143))
  call counter_Q_A(ctqq,wf(:,23),Q(:,28),wf(:,144))
  call vert_AW_Q(wf(:,141),wf(:,5),wf(:,145))
  call vert_AZ_Q(gZd,wf(:,141),wf(:,4),wf(:,146))
  call vert_AV_Q(wf(:,12),wf(:,-6),wf(:,147))
  call prop_A_Q(wf(:,147),Q(:,108),ZERO,0_intkind1,wf(:,148))
  call vert_AV_Q(wf(:,9),wf(:,-6),wf(:,149))
  call prop_A_Q(wf(:,149),Q(:,99),ZERO,0_intkind1,wf(:,150))
  call prop_Q_A(wf(:,26),Q(:,83),ZERO,0_intkind1,wf(:,151))
  call prop_Q_A(wf(:,27),Q(:,92),ZERO,0_intkind1,wf(:,152))
  call prop_W_W(wf(:,30),Q(:,15),MW,1_intkind1,wf(:,153))
  call prop_W_W(wf(:,33),Q(:,15),MW,1_intkind1,wf(:,154))
  call vert_WQ_A(wf(:,37),wf(:,134),wf(:,155))
  call vert_ZQ_A(gZu,wf(:,36),wf(:,134),wf(:,156))
  call counter_A_Q(ctqq,wf(:,40),Q(:,37),wf(:,157))
  call counter_A_Q(ctqq,wf(:,43),Q(:,42),wf(:,158))
  call counter_Q_A(ctqq,wf(:,47),Q(:,21),wf(:,159))
  call counter_Q_A(ctqq,wf(:,50),Q(:,26),wf(:,160))
  call vert_AW_Q(wf(:,141),wf(:,37),wf(:,161))
  call vert_AZ_Q(gZd,wf(:,141),wf(:,36),wf(:,162))
  call vert_AV_Q(wf(:,43),wf(:,-6),wf(:,163))
  call prop_A_Q(wf(:,163),Q(:,106),ZERO,0_intkind1,wf(:,164))
  call vert_AV_Q(wf(:,40),wf(:,-6),wf(:,165))
  call prop_A_Q(wf(:,165),Q(:,101),ZERO,0_intkind1,wf(:,166))
  call prop_Q_A(wf(:,51),Q(:,85),ZERO,0_intkind1,wf(:,167))
  call prop_Q_A(wf(:,52),Q(:,90),ZERO,0_intkind1,wf(:,168))
  call prop_W_W(wf(:,55),Q(:,15),MW,1_intkind1,wf(:,169))
  call prop_W_W(wf(:,58),Q(:,15),MW,1_intkind1,wf(:,170))
  call prop_W_W(wf(:,61),Q(:,15),MW,1_intkind1,wf(:,171))
  call prop_W_W(wf(:,64),Q(:,15),MW,1_intkind1,wf(:,172))
  call vert_WQ_A(wf(:,5),wf(:,20),wf(:,173))
  call prop_Q_A(wf(:,173),Q(:,31),ZERO,0_intkind1,wf(:,174))
  call vert_AW_Q(wf(:,9),wf(:,5),wf(:,175))
  call prop_A_Q(wf(:,175),Q(:,47),ZERO,0_intkind1,wf(:,176))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,23),wf(:,177))
  call prop_Q_A(wf(:,177),Q(:,31),ZERO,0_intkind1,wf(:,178))
  call vert_AZ_Q(gZu,wf(:,12),wf(:,4),wf(:,179))
  call prop_A_Q(wf(:,179),Q(:,47),ZERO,0_intkind1,wf(:,180))
  call vert_WQ_A(wf(:,75),wf(:,-4),wf(:,181))
  call prop_Q_A(wf(:,181),Q(:,31),ZERO,0_intkind1,wf(:,182))
  call vert_AW_Q(wf(:,-5),wf(:,75),wf(:,183))
  call prop_A_Q(wf(:,183),Q(:,47),ZERO,0_intkind1,wf(:,184))
  call vert_WQ_A(wf(:,153),wf(:,-4),wf(:,185))
  call prop_Q_A(wf(:,185),Q(:,31),ZERO,0_intkind1,wf(:,186))
  call vert_AW_Q(wf(:,-5),wf(:,153),wf(:,187))
  call prop_A_Q(wf(:,187),Q(:,47),ZERO,0_intkind1,wf(:,188))
  call vert_WQ_A(wf(:,154),wf(:,-4),wf(:,189))
  call prop_Q_A(wf(:,189),Q(:,31),ZERO,0_intkind1,wf(:,190))
  call vert_AW_Q(wf(:,-5),wf(:,154),wf(:,191))
  call prop_A_Q(wf(:,191),Q(:,47),ZERO,0_intkind1,wf(:,192))
  call vert_WQ_A(wf(:,37),wf(:,47),wf(:,193))
  call prop_Q_A(wf(:,193),Q(:,31),ZERO,0_intkind1,wf(:,194))
  call vert_AW_Q(wf(:,40),wf(:,37),wf(:,195))
  call prop_A_Q(wf(:,195),Q(:,47),ZERO,0_intkind1,wf(:,196))
  call vert_ZQ_A(gZd,wf(:,36),wf(:,50),wf(:,197))
  call prop_Q_A(wf(:,197),Q(:,31),ZERO,0_intkind1,wf(:,198))
  call vert_AZ_Q(gZu,wf(:,43),wf(:,36),wf(:,199))
  call prop_A_Q(wf(:,199),Q(:,47),ZERO,0_intkind1,wf(:,200))
  call vert_WQ_A(wf(:,114),wf(:,-4),wf(:,201))
  call prop_Q_A(wf(:,201),Q(:,31),ZERO,0_intkind1,wf(:,202))
  call vert_AW_Q(wf(:,-5),wf(:,114),wf(:,203))
  call prop_A_Q(wf(:,203),Q(:,47),ZERO,0_intkind1,wf(:,204))
  call vert_WQ_A(wf(:,169),wf(:,-4),wf(:,205))
  call prop_Q_A(wf(:,205),Q(:,31),ZERO,0_intkind1,wf(:,206))
  call vert_AW_Q(wf(:,-5),wf(:,169),wf(:,207))
  call prop_A_Q(wf(:,207),Q(:,47),ZERO,0_intkind1,wf(:,208))
  call vert_WQ_A(wf(:,170),wf(:,-4),wf(:,209))
  call prop_Q_A(wf(:,209),Q(:,31),ZERO,0_intkind1,wf(:,210))
  call vert_AW_Q(wf(:,-5),wf(:,170),wf(:,211))
  call prop_A_Q(wf(:,211),Q(:,47),ZERO,0_intkind1,wf(:,212))
  call vert_WQ_A(wf(:,171),wf(:,-4),wf(:,213))
  call prop_Q_A(wf(:,213),Q(:,31),ZERO,0_intkind1,wf(:,214))
  call vert_AW_Q(wf(:,-5),wf(:,171),wf(:,215))
  call prop_A_Q(wf(:,215),Q(:,47),ZERO,0_intkind1,wf(:,216))
  call vert_WQ_A(wf(:,172),wf(:,-4),wf(:,217))
  call prop_Q_A(wf(:,217),Q(:,31),ZERO,0_intkind1,wf(:,218))
  call vert_AW_Q(wf(:,-5),wf(:,172),wf(:,219))
  call prop_A_Q(wf(:,219),Q(:,47),ZERO,0_intkind1,wf(:,220))

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
  den(2) = 1 / (Q(5,12) - MW2)
  den(3) = 1 / (Q(5,80))
  den(5) = 1 / (Q(5,35))
  den(9) = 1 / (Q(5,44))
  den(13) = 1 / (Q(5,112) - MW2)
  den(16) = 1 / (Q(5,96))
  den(18) = 1 / (Q(5,19))
  den(22) = 1 / (Q(5,28))
  den(29) = 1 / (Q(5,7))
  den(32) = 1 / (Q(5,11))
  den(37) = 1 / (Q(5,5) - MZ2)
  den(38) = 1 / (Q(5,10) - MW2)
  den(40) = 1 / (Q(5,37))
  den(44) = 1 / (Q(5,42))
  den(50) = 1 / (Q(5,21))
  den(54) = 1 / (Q(5,26))
  den(62) = 1 / (Q(5,13))
  den(73) = 1 / (Q(5,67))
  den(77) = 1 / (Q(5,15) - MW2)
  den(80) = 1 / (Q(5,83))
  den(83) = 1 / (Q(5,92))
  den(87) = 1 / (Q(5,99))
  den(90) = 1 / (Q(5,108))
  den(93) = 1 / (Q(5,69))
  den(99) = 1 / (Q(5,85))
  den(102) = 1 / (Q(5,90))
  den(106) = 1 / (Q(5,101))
  den(109) = 1 / (Q(5,106))
  den(177) = 1 / (Q(5,31))
  den(180) = 1 / (Q(5,47))
  den(182) = 1 / (Q(5,60))
  den(199) = 1 / (Q(5,58))

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
  den(30) = den(1)*den(29)
  den(31) = den(14)*den(30)
  den(33) = den(1)*den(32)
  den(34) = den(14)*den(33)
  den(35) = den(25)*den(30)
  den(36) = den(25)*den(33)
  den(39) = den(3)*den(38)
  den(41) = den(37)*den(40)
  den(42) = den(39)*den(41)
  den(43) = den(3)*den(37)
  den(45) = den(38)*den(44)
  den(46) = den(43)*den(45)
  den(47) = den(37)*den(38)
  den(48) = den(14)*den(47)
  den(49) = den(16)*den(38)
  den(51) = den(37)*den(50)
  den(52) = den(49)*den(51)
  den(53) = den(16)*den(37)
  den(55) = den(38)*den(54)
  den(56) = den(53)*den(55)
  den(57) = den(25)*den(47)
  den(58) = den(45)*den(51)
  den(59) = den(41)*den(55)
  den(60) = den(29)*den(37)
  den(61) = den(14)*den(60)
  den(63) = den(37)*den(62)
  den(64) = den(14)*den(63)
  den(65) = den(25)*den(60)
  den(66) = den(25)*den(63)
  den(67) = den(32)*den(38)
  den(68) = den(14)*den(67)
  den(69) = den(25)*den(67)
  den(70) = den(2)*den(62)
  den(71) = den(14)*den(70)
  den(72) = den(25)*den(70)
  den(74) = den(1)*den(73)
  den(75) = den(23)*den(74)
  den(76) = den(10)*den(74)
  den(78) = den(12)*den(77)
  den(79) = den(3)*den(78)
  den(81) = den(8)*den(80)
  den(82) = den(2)*den(81)
  den(84) = den(4)*den(83)
  den(85) = den(1)*den(84)
  den(86) = den(16)*den(78)
  den(88) = den(21)*den(87)
  den(89) = den(2)*den(88)
  den(91) = den(17)*den(90)
  den(92) = den(1)*den(91)
  den(94) = den(37)*den(93)
  den(95) = den(55)*den(94)
  den(96) = den(45)*den(94)
  den(97) = den(47)*den(77)
  den(98) = den(3)*den(97)
  den(100) = den(43)*den(99)
  den(101) = den(38)*den(100)
  den(103) = den(39)*den(102)
  den(104) = den(37)*den(103)
  den(105) = den(16)*den(97)
  den(107) = den(53)*den(106)
  den(108) = den(38)*den(107)
  den(110) = den(49)*den(109)
  den(111) = den(37)*den(110)
  den(112) = den(3)**2
  den(113) = den(78)*den(112)
  den(114) = den(2)*den(112)
  den(115) = den(6)*den(114)
  den(116) = den(1)*den(112)
  den(117) = den(10)*den(116)
  den(118) = den(6)*den(84)
  den(119) = den(10)*den(81)
  den(120) = den(16)**2
  den(121) = den(78)*den(120)
  den(122) = den(19)*den(91)
  den(123) = den(23)*den(88)
  den(124) = den(2)*den(120)
  den(125) = den(19)*den(124)
  den(126) = den(1)*den(120)
  den(127) = den(23)*den(126)
  den(128) = den(10)*den(90)
  den(129) = den(19)*den(128)
  den(130) = den(6)*den(87)
  den(131) = den(23)*den(130)
  den(132) = den(19)*den(80)
  den(133) = den(10)*den(132)
  den(134) = den(23)*den(83)
  den(135) = den(6)*den(134)
  den(136) = den(30)*den(77)
  den(137) = den(112)*den(136)
  den(138) = den(33)*den(77)
  den(139) = den(112)*den(138)
  den(140) = den(120)*den(136)
  den(141) = den(120)*den(138)
  den(142) = den(97)*den(112)
  den(143) = den(38)*den(112)
  den(144) = den(41)*den(143)
  den(145) = den(37)*den(112)
  den(146) = den(45)*den(145)
  den(147) = den(41)*den(103)
  den(148) = den(45)*den(100)
  den(149) = den(97)*den(120)
  den(150) = den(51)*den(110)
  den(151) = den(55)*den(107)
  den(152) = den(38)*den(120)
  den(153) = den(51)*den(152)
  den(154) = den(37)*den(120)
  den(155) = den(55)*den(154)
  den(156) = den(45)*den(109)
  den(157) = den(51)*den(156)
  den(158) = den(41)*den(106)
  den(159) = den(55)*den(158)
  den(160) = den(51)*den(99)
  den(161) = den(45)*den(160)
  den(162) = den(55)*den(102)
  den(163) = den(41)*den(162)
  den(164) = den(60)*den(77)
  den(165) = den(112)*den(164)
  den(166) = den(63)*den(77)
  den(167) = den(112)*den(166)
  den(168) = den(120)*den(164)
  den(169) = den(120)*den(166)
  den(170) = den(67)*den(77)
  den(171) = den(112)*den(170)
  den(172) = den(120)*den(170)
  den(173) = den(70)*den(77)
  den(174) = den(112)*den(173)
  den(175) = den(120)*den(173)
  den(176) = den(2)*den(19)
  den(178) = den(176)*den(177)
  den(179) = den(2)*den(6)
  den(181) = den(179)*den(180)
  den(183) = den(23)*den(182)
  den(184) = den(1)*den(23)
  den(185) = den(177)*den(184)
  den(186) = den(10)*den(182)
  den(187) = den(1)*den(10)
  den(188) = den(180)*den(187)
  den(189) = den(78)*den(177)
  den(190) = den(78)*den(180)
  den(191) = den(136)*den(177)
  den(192) = den(136)*den(180)
  den(193) = den(138)*den(177)
  den(194) = den(138)*den(180)
  den(195) = den(38)*den(51)
  den(196) = den(177)*den(195)
  den(197) = den(38)*den(41)
  den(198) = den(180)*den(197)
  den(200) = den(55)*den(199)
  den(201) = den(37)*den(55)
  den(202) = den(177)*den(201)
  den(203) = den(45)*den(199)
  den(204) = den(37)*den(45)
  den(205) = den(180)*den(204)
  den(206) = den(97)*den(177)
  den(207) = den(97)*den(180)
  den(208) = den(164)*den(177)
  den(209) = den(164)*den(180)
  den(210) = den(166)*den(177)
  den(211) = den(166)*den(180)
  den(212) = den(170)*den(177)
  den(213) = den(170)*den(180)
  den(214) = den(173)*den(177)
  den(215) = den(173)*den(180)
  den(216) = den(2)*den(3)*den(6)
  den(217) = den(1)*den(3)*den(10)
  den(218) = den(1)*den(2)*den(3)
  den(219) = den(2)*den(16)*den(19)
  den(220) = den(1)*den(16)*den(23)
  den(221) = den(1)*den(2)*den(16)
  den(222) = den(2)*den(132)
  den(223) = den(2)*den(130)
  den(224) = den(1)*den(183)
  den(225) = den(1)*den(134)
  den(226) = den(1)*den(186)
  den(227) = den(1)*den(128)
  den(228) = den(3)*den(136)
  den(229) = den(3)*den(138)
  den(230) = den(16)*den(136)
  den(231) = den(16)*den(138)
  den(232) = den(3)*den(38)*den(41)
  den(233) = den(3)*den(37)*den(45)
  den(234) = den(3)*den(37)*den(38)
  den(235) = den(16)*den(38)*den(51)
  den(236) = den(16)*den(37)*den(55)
  den(237) = den(16)*den(37)*den(38)
  den(238) = den(38)*den(160)
  den(239) = den(38)*den(158)
  den(240) = den(37)*den(200)
  den(241) = den(37)*den(162)
  den(242) = den(37)*den(203)
  den(243) = den(37)*den(156)
  den(244) = den(3)*den(164)
  den(245) = den(3)*den(166)
  den(246) = den(16)*den(164)
  den(247) = den(16)*den(166)
  den(248) = den(3)*den(170)
  den(249) = den(16)*den(170)
  den(250) = den(3)*den(173)
  den(251) = den(16)*den(173)
  den(252) = den(3)*den(181)
  den(253) = den(3)*den(188)
  den(254) = den(3)*den(190)
  den(255) = den(16)*den(178)
  den(256) = den(16)*den(185)
  den(257) = den(16)*den(189)
  den(258) = den(3)*den(192)
  den(259) = den(3)*den(194)
  den(260) = den(16)*den(191)
  den(261) = den(16)*den(193)
  den(262) = den(3)*den(198)
  den(263) = den(3)*den(205)
  den(264) = den(3)*den(207)
  den(265) = den(16)*den(196)
  den(266) = den(16)*den(202)
  den(267) = den(16)*den(206)
  den(268) = den(3)*den(209)
  den(269) = den(3)*den(211)
  den(270) = den(16)*den(208)
  den(271) = den(16)*den(210)
  den(272) = den(3)*den(213)
  den(273) = den(16)*den(212)
  den(274) = den(3)*den(215)
  den(275) = den(16)*den(214)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(140)

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
  A(13) = cont_QA(wf(:,39),wf(:,40)) * den(42)
  A(14) = cont_QA(wf(:,42),wf(:,43)) * den(46)
  A(15) = cont_VV(wf(:,15),wf(:,44)) * den(48)
  A(16) = cont_QA(wf(:,46),wf(:,47)) * den(52)
  A(17) = cont_QA(wf(:,49),wf(:,50)) * den(56)
  A(18) = cont_VV(wf(:,25),wf(:,44)) * den(57)
  A(19) = cont_QA(wf(:,43),wf(:,51)) * den(58)
  A(20) = cont_QA(wf(:,40),wf(:,52)) * den(59)
  A(21) = cont_VV(wf(:,15),wf(:,55)) * den(61)
  A(22) = cont_VV(wf(:,15),wf(:,58)) * den(64)
  A(23) = cont_VV(wf(:,25),wf(:,55)) * den(65)
  A(24) = cont_VV(wf(:,25),wf(:,58)) * den(66)
  A(25) = cont_VV(wf(:,15),wf(:,61)) * den(68)
  A(26) = cont_VV(wf(:,25),wf(:,61)) * den(69)
  A(27) = cont_VV(wf(:,15),wf(:,64)) * den(71)
  A(28) = cont_VV(wf(:,25),wf(:,64)) * den(72)

  A(29) = cont_QA(wf(:,9),wf(:,65)) * den(7)
  A(30) = cont_QA(wf(:,12),wf(:,66)) * den(11)
  A(31) = cont_QA(wf(:,20),wf(:,67)) * den(20)
  A(32) = cont_QA(wf(:,23),wf(:,68)) * den(24)
  A(33) = cont_QA(wf(:,12),wf(:,69)) * den(27)
  A(34) = cont_QA(wf(:,9),wf(:,70)) * den(28)
  A(35) = cont_VV(wf(:,71),wf(:,72)) * den(75)
  A(36) = cont_VV(wf(:,71),wf(:,73)) * den(76)
  A(37) = cont_VV(wf(:,74),wf(:,75)) * den(79)
  A(38) = cont_QA(wf(:,76),wf(:,77)) * den(82)
  A(39) = cont_QA(wf(:,78),wf(:,79)) * den(85)
  A(40) = cont_QA(wf(:,26),wf(:,80)) * den(27)
  A(41) = cont_QA(wf(:,27),wf(:,81)) * den(28)
  A(42) = cont_QA(wf(:,20),wf(:,84)) * den(20)
  A(43) = cont_QA(wf(:,23),wf(:,85)) * den(24)
  A(44) = cont_VV(wf(:,14),wf(:,87)) * den(26)
  A(45) = cont_VV(wf(:,75),wf(:,88)) * den(86)
  A(46) = cont_QA(wf(:,89),wf(:,90)) * den(89)
  A(47) = cont_QA(wf(:,91),wf(:,92)) * den(92)
  A(48) = cont_QA(wf(:,9),wf(:,94)) * den(28)
  A(49) = cont_QA(wf(:,12),wf(:,96)) * den(27)
  A(50) = cont_QA(wf(:,9),wf(:,99)) * den(7)
  A(51) = cont_QA(wf(:,12),wf(:,100)) * den(11)
  A(52) = cont_VV(wf(:,14),wf(:,102)) * den(15)
  A(53) = cont_VV(wf(:,30),wf(:,103)) * den(31)
  A(54) = cont_VV(wf(:,33),wf(:,103)) * den(34)
  A(55) = cont_VV(wf(:,30),wf(:,87)) * den(35)
  A(56) = cont_VV(wf(:,33),wf(:,87)) * den(36)
  A(57) = cont_VV(wf(:,30),wf(:,104)) * den(35)
  A(58) = cont_VV(wf(:,33),wf(:,104)) * den(36)
  A(59) = cont_VV(wf(:,30),wf(:,102)) * den(31)
  A(60) = cont_VV(wf(:,33),wf(:,102)) * den(34)
  A(61) = cont_QA(wf(:,40),wf(:,105)) * den(42)
  A(62) = cont_QA(wf(:,43),wf(:,106)) * den(46)
  A(63) = cont_QA(wf(:,47),wf(:,107)) * den(52)
  A(64) = cont_QA(wf(:,50),wf(:,108)) * den(56)
  A(65) = cont_QA(wf(:,43),wf(:,109)) * den(58)
  A(66) = cont_QA(wf(:,40),wf(:,110)) * den(59)
  A(67) = cont_VV(wf(:,111),wf(:,112)) * den(95)
  A(68) = cont_VV(wf(:,111),wf(:,113)) * den(96)
  A(69) = cont_VV(wf(:,74),wf(:,114)) * den(98)
  A(70) = cont_QA(wf(:,115),wf(:,116)) * den(101)
  A(71) = cont_QA(wf(:,117),wf(:,118)) * den(104)
  A(72) = cont_QA(wf(:,51),wf(:,119)) * den(58)
  A(73) = cont_QA(wf(:,52),wf(:,120)) * den(59)
  A(74) = cont_QA(wf(:,47),wf(:,121)) * den(52)
  A(75) = cont_QA(wf(:,50),wf(:,122)) * den(56)
  A(76) = cont_VV(wf(:,44),wf(:,87)) * den(57)
  A(77) = cont_VV(wf(:,88),wf(:,114)) * den(105)
  A(78) = cont_QA(wf(:,123),wf(:,124)) * den(108)
  A(79) = cont_QA(wf(:,125),wf(:,126)) * den(111)
  A(80) = cont_QA(wf(:,40),wf(:,128)) * den(59)
  A(81) = cont_QA(wf(:,43),wf(:,130)) * den(58)
  A(82) = cont_QA(wf(:,40),wf(:,131)) * den(42)
  A(83) = cont_QA(wf(:,43),wf(:,132)) * den(46)
  A(84) = cont_VV(wf(:,44),wf(:,102)) * den(48)
  A(85) = cont_VV(wf(:,55),wf(:,103)) * den(61)
  A(86) = cont_VV(wf(:,58),wf(:,103)) * den(64)
  A(87) = cont_VV(wf(:,55),wf(:,87)) * den(65)
  A(88) = cont_VV(wf(:,58),wf(:,87)) * den(66)
  A(89) = cont_VV(wf(:,55),wf(:,104)) * den(65)
  A(90) = cont_VV(wf(:,58),wf(:,104)) * den(66)
  A(91) = cont_VV(wf(:,55),wf(:,102)) * den(61)
  A(92) = cont_VV(wf(:,58),wf(:,102)) * den(64)
  A(93) = cont_VV(wf(:,61),wf(:,103)) * den(68)
  A(94) = cont_VV(wf(:,61),wf(:,87)) * den(69)
  A(95) = cont_VV(wf(:,61),wf(:,104)) * den(69)
  A(96) = cont_VV(wf(:,61),wf(:,102)) * den(68)
  A(97) = cont_VV(wf(:,64),wf(:,103)) * den(71)
  A(98) = cont_VV(wf(:,64),wf(:,87)) * den(72)
  A(99) = cont_VV(wf(:,64),wf(:,104)) * den(72)
  A(100) = cont_VV(wf(:,64),wf(:,102)) * den(71)
  A(101) = cont_VV(wf(:,75),wf(:,135)) * den(113)
  A(102) = cont_QA(wf(:,9),wf(:,136)) * den(115)
  A(103) = cont_QA(wf(:,12),wf(:,137)) * den(117)
  A(104) = cont_QA(wf(:,79),wf(:,138)) * den(118)
  A(105) = cont_QA(wf(:,77),wf(:,139)) * den(119)
  A(106) = cont_VV(wf(:,75),wf(:,142)) * den(121)
  A(107) = cont_QA(wf(:,92),wf(:,143)) * den(122)
  A(108) = cont_QA(wf(:,90),wf(:,144)) * den(123)
  A(109) = cont_QA(wf(:,20),wf(:,145)) * den(125)
  A(110) = cont_QA(wf(:,23),wf(:,146)) * den(127)
  A(111) = cont_QA(wf(:,143),wf(:,148)) * den(129)
  A(112) = cont_QA(wf(:,144),wf(:,150)) * den(131)
  A(113) = cont_QA(wf(:,139),wf(:,151)) * den(133)
  A(114) = cont_QA(wf(:,138),wf(:,152)) * den(135)
  A(115) = cont_VV(wf(:,135),wf(:,153)) * den(137)
  A(116) = cont_VV(wf(:,135),wf(:,154)) * den(139)
  A(117) = cont_VV(wf(:,142),wf(:,153)) * den(140)
  A(118) = cont_VV(wf(:,142),wf(:,154)) * den(141)
  A(119) = cont_VV(wf(:,114),wf(:,135)) * den(142)
  A(120) = cont_QA(wf(:,40),wf(:,155)) * den(144)
  A(121) = cont_QA(wf(:,43),wf(:,156)) * den(146)
  A(122) = cont_QA(wf(:,118),wf(:,157)) * den(147)
  A(123) = cont_QA(wf(:,116),wf(:,158)) * den(148)
  A(124) = cont_VV(wf(:,114),wf(:,142)) * den(149)
  A(125) = cont_QA(wf(:,126),wf(:,159)) * den(150)
  A(126) = cont_QA(wf(:,124),wf(:,160)) * den(151)
  A(127) = cont_QA(wf(:,47),wf(:,161)) * den(153)
  A(128) = cont_QA(wf(:,50),wf(:,162)) * den(155)
  A(129) = cont_QA(wf(:,159),wf(:,164)) * den(157)
  A(130) = cont_QA(wf(:,160),wf(:,166)) * den(159)
  A(131) = cont_QA(wf(:,158),wf(:,167)) * den(161)
  A(132) = cont_QA(wf(:,157),wf(:,168)) * den(163)
  A(133) = cont_VV(wf(:,135),wf(:,169)) * den(165)
  A(134) = cont_VV(wf(:,135),wf(:,170)) * den(167)
  A(135) = cont_VV(wf(:,142),wf(:,169)) * den(168)
  A(136) = cont_VV(wf(:,142),wf(:,170)) * den(169)
  A(137) = cont_VV(wf(:,135),wf(:,171)) * den(171)
  A(138) = cont_VV(wf(:,142),wf(:,171)) * den(172)
  A(139) = cont_VV(wf(:,135),wf(:,172)) * den(174)
  A(140) = cont_VV(wf(:,142),wf(:,172)) * den(175)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(140)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(25)+A(26)-A(27)-A(28))*f(1)+(-A(3)-A(6)+A(15)+A(18))*f(5)+(-A(1)-A(2)-A(4)-A(5)-A(7)-A(8)-A(9)-A(10)-A(11)-A(12) &
       +A(13)+A(14)+A(16)+A(17)+A(19)+A(20)+A(21)+A(22)+A(23)+A(24))*f(9)

  M2(1) = (-A(137)-A(138)+A(139)+A(140))*f(2)+(A(94)+A(96)-A(98)-A(100))*f(3)+(A(93)+A(95)-A(97)-A(99))*f(4)+(A(101)+A(106)-A(119) &
       -A(124))*f(6)+(-A(44)-A(52)+A(76)+A(84))*f(7)+(-A(37)-A(45)+A(69)+A(77))*f(8)+(A(102)+A(103)+A(104)+A(105)+A(107)+A(108) &
       +A(109)+A(110)+A(111)+A(112)+A(113)+A(114)+A(115)+A(116)+A(117)+A(118)-A(120)-A(121)-A(122)-A(123)-A(125)-A(126)-A(127) &
       -A(128)-A(129)-A(130)-A(131)-A(132)-A(133)-A(134)-A(135)-A(136))*f(10)+(-A(33)-A(34)-A(42)-A(43)-A(50)-A(51)-A(55)-A(56) &
       -A(59)-A(60)+A(65)+A(66)+A(74)+A(75)+A(82)+A(83)+A(87)+A(88)+A(91)+A(92))*f(11)+(-A(29)-A(30)-A(31)-A(32)-A(38)-A(39)-A(40) &
       -A(41)-A(46)-A(47)-A(48)-A(49)-A(53)-A(54)-A(57)-A(58)+A(61)+A(62)+A(63)+A(64)+A(70)+A(71)+A(72)+A(73)+A(78)+A(79)+A(80) &
       +A(81)+A(85)+A(86)+A(89)+A(90))*f(12)+(A(35)+A(36)-A(67)-A(68))*f(13)

end subroutine colourvectors

end module ol_loop_ppllllj_nenexnexeudxg_1_/**/REALKIND
