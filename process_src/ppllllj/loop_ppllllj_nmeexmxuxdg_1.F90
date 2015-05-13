
module ol_colourmatrix_ppllllj_nmeexmxuxdg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllllj_nmeexmxuxdg_1_/**/REALKIND



module ol_forced_parameters_ppllllj_nmeexmxuxdg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj_nmeexmxuxdg_1_/**/REALKIND

module ol_loop_ppllllj_nmeexmxuxdg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(36), c(20)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:172)
  ! denominators
  complex(REALKIND), save :: den(225)
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
    f( 9) = (CI*eQED**4*gQCD)/(6._/**/REALKIND*sw**2)
    f(10) = (CI*eQED**4*gQCD)/(3._/**/REALKIND*sw**2)
    f(11) = (CI*eQED**4*gQCD)/(2._/**/REALKIND*sw**2)
    f(12) = (CI*countertermnorm*eQED**4*gQCD**3)/(6._/**/REALKIND*sw**2)
    f(13) = (CI*countertermnorm*eQED**4*gQCD**3)/(3._/**/REALKIND*sw**2)
    f(14) = (CI*countertermnorm*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(15) = (CI*countertermnorm*ctGqq*eQED**4*gQCD**3)/(6._/**/REALKIND*sw**2)
    f(16) = (CI*countertermnorm*ctGqq*eQED**4*gQCD**3)/(3._/**/REALKIND*sw**2)
    f(17) = (CI*countertermnorm*ctGqq*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(18) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**3)/(6._/**/REALKIND*sw**2)
    f(19) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**3)/(3._/**/REALKIND*sw**2)
    f(20) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(21) = (countertermnorm*ctZGG*eQED**4*gQCD**3)/(sw**2*2._/**/REALKIND)
    f(22) = (CI*eQED**4*gQCD**3*integralnorm*SwB)/(4._/**/REALKIND*sw**4)
    f(23) = (eQED**4*gQCD**3*integralnorm*SwB)/(sw**4*4._/**/REALKIND)
    f(24) = (CI*cw*eQED**4*gQCD**3*integralnorm*SwB)/(2._/**/REALKIND*sw**3)
    f(25) = (cw*eQED**4*gQCD**3*integralnorm*SwB)/(sw**3*2._/**/REALKIND)
    f(26) = (CI*eQED**4*gQCD**3*integralnorm*SwB)/(6._/**/REALKIND*sw**2)
    f(27) = (CI*eQED**4*gQCD**3*integralnorm*SwB)/(3._/**/REALKIND*sw**2)
    f(28) = (CI*eQED**4*gQCD**3*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(29) = (eQED**4*gQCD**3*integralnorm*SwB)/(sw**2*6._/**/REALKIND)
    f(30) = (eQED**4*gQCD**3*integralnorm*SwB)/(sw**2*3._/**/REALKIND)
    f(31) = (eQED**4*gQCD**3*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(32) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*6._/**/REALKIND)
    f(33) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(34) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(35) = (2*eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(36) = (eQED**4*gQCD**3*integralnorm*SwF)/sw**2

  c = [ 9*CI*f(22), f(23), 8*f(23), 9*CI*f(24), f(25), 8*f(25), 9*CI*f(26), 9*CI*f(27), 9*CI*f(28), f(29), 8*f(29), f(30), 8*f(30) &
    , f(31), 8*f(31), 3*f(32), 3*f(33), 3*f(34), 3*f(35), 3*f(36) ]
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
  complex(REALKIND) :: A(122)
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
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,2))
  call vert_AV_Q(wf(:,-4),wf(:,-6),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,9),MW,1_intkind1,wf(:,4))
  call prop_A_Q(wf(:,3),Q(:,80),ZERO,0_intkind1,wf(:,5))
  call vert_WQ_A(wf(:,4),wf(:,-5),wf(:,6))
  call vert_AV_Q(wf(:,5),wf(:,2),wf(:,7))
  call prop_Q_A(wf(:,6),Q(:,41),ZERO,0_intkind1,wf(:,8))
  call vert_QA_Z(gZl,wf(:,-1),wf(:,-2),wf(:,9))
  call prop_W_W(wf(:,9),Q(:,6),MZ,1_intkind1,wf(:,10))
  call vert_AZ_Q(gZu,wf(:,5),wf(:,10),wf(:,11))
  call vert_VQ_A(wf(:,2),wf(:,-5),wf(:,12))
  call vert_AW_Q(wf(:,5),wf(:,4),wf(:,13))
  call prop_Q_A(wf(:,12),Q(:,38),ZERO,0_intkind1,wf(:,14))
  call vert_ZQ_A(gZd,wf(:,10),wf(:,-5),wf(:,15))
  call prop_Q_A(wf(:,15),Q(:,38),ZERO,0_intkind1,wf(:,16))
  call vert_QA_W(wf(:,-5),wf(:,5),wf(:,17))
  call vert_UV_W(wf(:,2),Q(:,6),wf(:,4),Q(:,9),wf(:,18))
  call prop_W_W(wf(:,17),Q(:,112),MW,1_intkind1,wf(:,19))
  call vert_UV_W(wf(:,10),Q(:,6),wf(:,4),Q(:,9),wf(:,20))
  call vert_VQ_A(wf(:,-6),wf(:,-5),wf(:,21))
  call prop_Q_A(wf(:,21),Q(:,96),ZERO,0_intkind1,wf(:,22))
  call vert_AW_Q(wf(:,-4),wf(:,4),wf(:,23))
  call vert_VQ_A(wf(:,2),wf(:,22),wf(:,24))
  call prop_A_Q(wf(:,23),Q(:,25),ZERO,0_intkind1,wf(:,25))
  call vert_ZQ_A(gZd,wf(:,10),wf(:,22),wf(:,26))
  call vert_AV_Q(wf(:,-4),wf(:,2),wf(:,27))
  call vert_WQ_A(wf(:,4),wf(:,22),wf(:,28))
  call prop_A_Q(wf(:,27),Q(:,22),ZERO,0_intkind1,wf(:,29))
  call vert_AZ_Q(gZu,wf(:,-4),wf(:,10),wf(:,30))
  call prop_A_Q(wf(:,30),Q(:,22),ZERO,0_intkind1,wf(:,31))
  call vert_QA_W(wf(:,22),wf(:,-4),wf(:,32))
  call prop_W_W(wf(:,32),Q(:,112),MW,1_intkind1,wf(:,33))
  call vert_AV_Q(wf(:,25),wf(:,-6),wf(:,34))
  call vert_AV_Q(wf(:,29),wf(:,-6),wf(:,35))
  call vert_AV_Q(wf(:,31),wf(:,-6),wf(:,36))
  call vert_ZQ_A(gZn,wf(:,10),wf(:,0),wf(:,37))
  call prop_Q_A(wf(:,37),Q(:,7),ZERO,0_intkind1,wf(:,38))
  call vert_QA_W(wf(:,38),wf(:,-3),wf(:,39))
  call vert_AV_Q(wf(:,-3),wf(:,2),wf(:,40))
  call prop_A_Q(wf(:,40),Q(:,14),ZERO,0_intkind1,wf(:,41))
  call vert_QA_W(wf(:,0),wf(:,41),wf(:,42))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,10),wf(:,43))
  call prop_A_Q(wf(:,43),Q(:,14),ZERO,0_intkind1,wf(:,44))
  call vert_QA_W(wf(:,0),wf(:,44),wf(:,45))
  call vert_WQ_A(wf(:,4),wf(:,-1),wf(:,46))
  call prop_Q_A(wf(:,46),Q(:,11),ZERO,0_intkind1,wf(:,47))
  call vert_QA_W(wf(:,47),wf(:,-2),wf(:,48))
  call counter_AV_Q(wf(:,5),wf(:,2),wf(:,49))
  call counter_AZ_Q(gZu,wf(:,5),wf(:,10),wf(:,50))
  call counter_AW_Q(wf(:,5),wf(:,4),wf(:,51))
  call counter_VQ_A(wf(:,2),wf(:,22),wf(:,52))
  call counter_ZQ_A(gZd,wf(:,10),wf(:,22),wf(:,53))
  call counter_WQ_A(wf(:,4),wf(:,22),wf(:,54))
  call counter_AV_Q(wf(:,25),wf(:,-6),wf(:,55))
  call counter_VG_G(wf(:,10),wf(:,-6),Q(:,64),wf(:,56),Q(:,70))
  call vert_QA_V(wf(:,-5),wf(:,25),wf(:,57))
  call counter_AV_Q(wf(:,29),wf(:,-6),wf(:,58))
  call counter_AV_Q(wf(:,31),wf(:,-6),wf(:,59))
  call vert_QA_V(wf(:,8),wf(:,-4),wf(:,60))
  call counter_QA_W(wf(:,-5),wf(:,5),wf(:,61))
  call prop_W_W(wf(:,18),Q(:,15),MW,1_intkind1,wf(:,62))
  call prop_W_W(wf(:,20),Q(:,15),MW,1_intkind1,wf(:,63))
  call counter_VQ_A(wf(:,2),wf(:,-5),wf(:,64))
  call prop_A_Q(wf(:,13),Q(:,89),ZERO,0_intkind1,wf(:,65))
  call counter_ZQ_A(gZd,wf(:,10),wf(:,-5),wf(:,66))
  call counter_WQ_A(wf(:,4),wf(:,-5),wf(:,67))
  call prop_A_Q(wf(:,7),Q(:,86),ZERO,0_intkind1,wf(:,68))
  call prop_A_Q(wf(:,11),Q(:,86),ZERO,0_intkind1,wf(:,69))
  call prop_Q_A(wf(:,64),Q(:,38),ZERO,0_intkind1,wf(:,70))
  call prop_Q_A(wf(:,66),Q(:,38),ZERO,0_intkind1,wf(:,71))
  call prop_Q_A(wf(:,67),Q(:,41),ZERO,0_intkind1,wf(:,72))
  call counter_VQ_A(wf(:,-6),wf(:,-5),wf(:,73))
  call prop_Q_A(wf(:,73),Q(:,96),ZERO,0_intkind1,wf(:,74))
  call vert_VQ_A(wf(:,2),wf(:,74),wf(:,75))
  call vert_ZQ_A(gZd,wf(:,10),wf(:,74),wf(:,76))
  call vert_WQ_A(wf(:,4),wf(:,74),wf(:,77))
  call vert_QA_W(wf(:,74),wf(:,-4),wf(:,78))
  call prop_W_W(wf(:,78),Q(:,112),MW,1_intkind1,wf(:,79))
  call counter_QA_W(wf(:,22),wf(:,-4),wf(:,80))
  call counter_AV_Q(wf(:,-4),wf(:,2),wf(:,81))
  call prop_Q_A(wf(:,28),Q(:,105),ZERO,0_intkind1,wf(:,82))
  call counter_AZ_Q(gZu,wf(:,-4),wf(:,10),wf(:,83))
  call counter_AW_Q(wf(:,-4),wf(:,4),wf(:,84))
  call prop_Q_A(wf(:,24),Q(:,102),ZERO,0_intkind1,wf(:,85))
  call prop_Q_A(wf(:,26),Q(:,102),ZERO,0_intkind1,wf(:,86))
  call prop_A_Q(wf(:,81),Q(:,22),ZERO,0_intkind1,wf(:,87))
  call vert_AV_Q(wf(:,87),wf(:,-6),wf(:,88))
  call prop_A_Q(wf(:,83),Q(:,22),ZERO,0_intkind1,wf(:,89))
  call vert_AV_Q(wf(:,89),wf(:,-6),wf(:,90))
  call prop_A_Q(wf(:,84),Q(:,25),ZERO,0_intkind1,wf(:,91))
  call vert_AV_Q(wf(:,91),wf(:,-6),wf(:,92))
  call counter_AV_Q(wf(:,-4),wf(:,-6),wf(:,93))
  call prop_A_Q(wf(:,93),Q(:,80),ZERO,0_intkind1,wf(:,94))
  call vert_AV_Q(wf(:,94),wf(:,2),wf(:,95))
  call vert_AZ_Q(gZu,wf(:,94),wf(:,10),wf(:,96))
  call vert_AW_Q(wf(:,94),wf(:,4),wf(:,97))
  call vert_QA_W(wf(:,-5),wf(:,94),wf(:,98))
  call prop_W_W(wf(:,98),Q(:,112),MW,1_intkind1,wf(:,99))
  call prop_W_W(wf(:,61),Q(:,112),MW,1_intkind1,wf(:,100))
  call prop_W_W(wf(:,80),Q(:,112),MW,1_intkind1,wf(:,101))
  call counter_A_Q(ctqq,wf(:,5),Q(:,80),wf(:,102))
  call prop_A_Q(wf(:,102),Q(:,80),ZERO,0_intkind1,wf(:,103))
  call vert_QA_W(wf(:,-5),wf(:,103),wf(:,104))
  call vert_AV_Q(wf(:,103),wf(:,2),wf(:,105))
  call vert_AZ_Q(gZu,wf(:,103),wf(:,10),wf(:,106))
  call vert_AW_Q(wf(:,103),wf(:,4),wf(:,107))
  call counter_Q_A(ctqq,wf(:,8),Q(:,41),wf(:,108))
  call counter_Q_A(ctqq,wf(:,14),Q(:,38),wf(:,109))
  call counter_Q_A(ctqq,wf(:,16),Q(:,38),wf(:,110))
  call counter_Q_A(ctqq,wf(:,22),Q(:,96),wf(:,111))
  call prop_Q_A(wf(:,111),Q(:,96),ZERO,0_intkind1,wf(:,112))
  call vert_QA_W(wf(:,112),wf(:,-4),wf(:,113))
  call counter_A_Q(ctqq,wf(:,25),Q(:,25),wf(:,114))
  call counter_A_Q(ctqq,wf(:,29),Q(:,22),wf(:,115))
  call counter_A_Q(ctqq,wf(:,31),Q(:,22),wf(:,116))
  call vert_VQ_A(wf(:,2),wf(:,112),wf(:,117))
  call vert_ZQ_A(gZd,wf(:,10),wf(:,112),wf(:,118))
  call vert_WQ_A(wf(:,4),wf(:,112),wf(:,119))
  call vert_VQ_A(wf(:,-6),wf(:,14),wf(:,120))
  call prop_Q_A(wf(:,120),Q(:,102),ZERO,0_intkind1,wf(:,121))
  call vert_VQ_A(wf(:,-6),wf(:,16),wf(:,122))
  call prop_Q_A(wf(:,122),Q(:,102),ZERO,0_intkind1,wf(:,123))
  call vert_VQ_A(wf(:,-6),wf(:,8),wf(:,124))
  call prop_Q_A(wf(:,124),Q(:,105),ZERO,0_intkind1,wf(:,125))
  call prop_A_Q(wf(:,34),Q(:,89),ZERO,0_intkind1,wf(:,126))
  call prop_A_Q(wf(:,35),Q(:,86),ZERO,0_intkind1,wf(:,127))
  call prop_A_Q(wf(:,36),Q(:,86),ZERO,0_intkind1,wf(:,128))
  call prop_W_W(wf(:,39),Q(:,15),MW,1_intkind1,wf(:,129))
  call prop_W_W(wf(:,42),Q(:,15),MW,1_intkind1,wf(:,130))
  call prop_W_W(wf(:,45),Q(:,15),MW,1_intkind1,wf(:,131))
  call prop_W_W(wf(:,48),Q(:,15),MW,1_intkind1,wf(:,132))
  call vert_AV_Q(wf(:,25),wf(:,2),wf(:,133))
  call prop_A_Q(wf(:,133),Q(:,31),ZERO,0_intkind1,wf(:,134))
  call vert_AZ_Q(gZd,wf(:,25),wf(:,10),wf(:,135))
  call prop_A_Q(wf(:,135),Q(:,31),ZERO,0_intkind1,wf(:,136))
  call vert_VQ_A(wf(:,2),wf(:,8),wf(:,137))
  call prop_Q_A(wf(:,137),Q(:,47),ZERO,0_intkind1,wf(:,138))
  call vert_ZQ_A(gZu,wf(:,10),wf(:,8),wf(:,139))
  call prop_Q_A(wf(:,139),Q(:,47),ZERO,0_intkind1,wf(:,140))
  call vert_AW_Q(wf(:,29),wf(:,4),wf(:,141))
  call prop_A_Q(wf(:,141),Q(:,31),ZERO,0_intkind1,wf(:,142))
  call vert_AW_Q(wf(:,31),wf(:,4),wf(:,143))
  call prop_A_Q(wf(:,143),Q(:,31),ZERO,0_intkind1,wf(:,144))
  call vert_WQ_A(wf(:,4),wf(:,14),wf(:,145))
  call prop_Q_A(wf(:,145),Q(:,47),ZERO,0_intkind1,wf(:,146))
  call vert_WQ_A(wf(:,4),wf(:,16),wf(:,147))
  call prop_Q_A(wf(:,147),Q(:,47),ZERO,0_intkind1,wf(:,148))
  call vert_AW_Q(wf(:,-4),wf(:,62),wf(:,149))
  call prop_A_Q(wf(:,149),Q(:,31),ZERO,0_intkind1,wf(:,150))
  call vert_AW_Q(wf(:,-4),wf(:,63),wf(:,151))
  call prop_A_Q(wf(:,151),Q(:,31),ZERO,0_intkind1,wf(:,152))
  call vert_WQ_A(wf(:,62),wf(:,-5),wf(:,153))
  call prop_Q_A(wf(:,153),Q(:,47),ZERO,0_intkind1,wf(:,154))
  call vert_WQ_A(wf(:,63),wf(:,-5),wf(:,155))
  call prop_Q_A(wf(:,155),Q(:,47),ZERO,0_intkind1,wf(:,156))
  call vert_AW_Q(wf(:,-4),wf(:,129),wf(:,157))
  call prop_A_Q(wf(:,157),Q(:,31),ZERO,0_intkind1,wf(:,158))
  call vert_WQ_A(wf(:,129),wf(:,-5),wf(:,159))
  call prop_Q_A(wf(:,159),Q(:,47),ZERO,0_intkind1,wf(:,160))
  call vert_AW_Q(wf(:,-4),wf(:,130),wf(:,161))
  call prop_A_Q(wf(:,161),Q(:,31),ZERO,0_intkind1,wf(:,162))
  call vert_AW_Q(wf(:,-4),wf(:,131),wf(:,163))
  call prop_A_Q(wf(:,163),Q(:,31),ZERO,0_intkind1,wf(:,164))
  call vert_WQ_A(wf(:,130),wf(:,-5),wf(:,165))
  call prop_Q_A(wf(:,165),Q(:,47),ZERO,0_intkind1,wf(:,166))
  call vert_WQ_A(wf(:,131),wf(:,-5),wf(:,167))
  call prop_Q_A(wf(:,167),Q(:,47),ZERO,0_intkind1,wf(:,168))
  call vert_AW_Q(wf(:,-4),wf(:,132),wf(:,169))
  call prop_A_Q(wf(:,169),Q(:,31),ZERO,0_intkind1,wf(:,170))
  call vert_WQ_A(wf(:,132),wf(:,-5),wf(:,171))
  call prop_Q_A(wf(:,171),Q(:,47),ZERO,0_intkind1,wf(:,172))

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
  den(2) = 1 / (Q(5,6))
  den(3) = 1 / (Q(5,80))
  den(5) = 1 / (Q(5,41))
  den(8) = 1 / (Q(5,6) - MZ2)
  den(12) = 1 / (Q(5,38))
  den(18) = 1 / (Q(5,112) - MW2)
  den(23) = 1 / (Q(5,96))
  den(25) = 1 / (Q(5,25))
  den(31) = 1 / (Q(5,22))
  den(43) = 1 / (Q(5,7))
  den(46) = 1 / (Q(5,14))
  den(54) = 1 / (Q(5,11))
  den(58) = 1 / (Q(5,70))
  den(62) = 1 / (Q(5,15) - MW2)
  den(67) = 1 / (Q(5,89))
  den(71) = 1 / (Q(5,86))
  den(78) = 1 / (Q(5,105))
  den(82) = 1 / (Q(5,102))
  den(141) = 1 / (Q(5,57))
  den(144) = 1 / (Q(5,31))
  den(150) = 1 / (Q(5,47))

  ! denominators
  den(4) = den(2)*den(3)
  den(6) = den(1)*den(5)
  den(7) = den(4)*den(6)
  den(9) = den(3)*den(8)
  den(10) = den(6)*den(9)
  den(11) = den(1)*den(3)
  den(13) = den(2)*den(12)
  den(14) = den(11)*den(13)
  den(15) = den(8)*den(12)
  den(16) = den(11)*den(15)
  den(17) = den(1)*den(2)
  den(19) = den(3)*den(18)
  den(20) = den(17)*den(19)
  den(21) = den(1)*den(8)
  den(22) = den(19)*den(21)
  den(24) = den(2)*den(23)
  den(26) = den(1)*den(25)
  den(27) = den(24)*den(26)
  den(28) = den(8)*den(23)
  den(29) = den(26)*den(28)
  den(30) = den(1)*den(23)
  den(32) = den(2)*den(31)
  den(33) = den(30)*den(32)
  den(34) = den(8)*den(31)
  den(35) = den(30)*den(34)
  den(36) = den(18)*den(23)
  den(37) = den(17)*den(36)
  den(38) = den(21)*den(36)
  den(39) = den(13)*den(26)
  den(40) = den(15)*den(26)
  den(41) = den(6)*den(32)
  den(42) = den(6)*den(34)
  den(44) = den(8)*den(43)
  den(45) = den(19)*den(44)
  den(47) = den(2)*den(46)
  den(48) = den(19)*den(47)
  den(49) = den(8)*den(46)
  den(50) = den(19)*den(49)
  den(51) = den(36)*den(44)
  den(52) = den(36)*den(47)
  den(53) = den(36)*den(49)
  den(55) = den(1)*den(54)
  den(56) = den(19)*den(55)
  den(57) = den(36)*den(55)
  den(59) = den(8)*den(58)
  den(60) = den(26)*den(59)
  den(61) = den(6)*den(59)
  den(63) = den(17)*den(62)
  den(64) = den(3)*den(63)
  den(65) = den(21)*den(62)
  den(66) = den(3)*den(65)
  den(68) = den(11)*den(67)
  den(69) = den(2)*den(68)
  den(70) = den(8)*den(68)
  den(72) = den(4)*den(71)
  den(73) = den(1)*den(72)
  den(74) = den(9)*den(71)
  den(75) = den(1)*den(74)
  den(76) = den(23)*den(63)
  den(77) = den(23)*den(65)
  den(79) = den(30)*den(78)
  den(80) = den(2)*den(79)
  den(81) = den(8)*den(79)
  den(83) = den(24)*den(82)
  den(84) = den(1)*den(83)
  den(85) = den(28)*den(82)
  den(86) = den(1)*den(85)
  den(87) = den(3)**2
  den(88) = den(63)*den(87)
  den(89) = den(65)*den(87)
  den(90) = den(2)*den(87)
  den(91) = den(6)*den(90)
  den(92) = den(8)*den(87)
  den(93) = den(6)*den(92)
  den(94) = den(1)*den(87)
  den(95) = den(13)*den(94)
  den(96) = den(15)*den(94)
  den(97) = den(6)*den(72)
  den(98) = den(6)*den(74)
  den(99) = den(13)*den(68)
  den(100) = den(15)*den(68)
  den(101) = den(23)**2
  den(102) = den(63)*den(101)
  den(103) = den(65)*den(101)
  den(104) = den(26)*den(83)
  den(105) = den(26)*den(85)
  den(106) = den(32)*den(79)
  den(107) = den(34)*den(79)
  den(108) = den(2)*den(101)
  den(109) = den(26)*den(108)
  den(110) = den(8)*den(101)
  den(111) = den(26)*den(110)
  den(112) = den(1)*den(101)
  den(113) = den(32)*den(112)
  den(114) = den(34)*den(112)
  den(115) = den(13)*den(82)
  den(116) = den(26)*den(115)
  den(117) = den(15)*den(82)
  den(118) = den(26)*den(117)
  den(119) = den(6)*den(78)
  den(120) = den(32)*den(119)
  den(121) = den(34)*den(119)
  den(122) = den(26)*den(67)
  den(123) = den(13)*den(122)
  den(124) = den(15)*den(122)
  den(125) = den(32)*den(71)
  den(126) = den(6)*den(125)
  den(127) = den(34)*den(71)
  den(128) = den(6)*den(127)
  den(129) = den(44)*den(62)
  den(130) = den(87)*den(129)
  den(131) = den(47)*den(62)
  den(132) = den(87)*den(131)
  den(133) = den(49)*den(62)
  den(134) = den(87)*den(133)
  den(135) = den(101)*den(129)
  den(136) = den(101)*den(131)
  den(137) = den(101)*den(133)
  den(138) = den(55)*den(62)
  den(139) = den(87)*den(138)
  den(140) = den(101)*den(138)
  den(142) = den(26)*den(141)
  den(143) = den(2)*den(26)
  den(145) = den(143)*den(144)
  den(146) = den(8)*den(26)
  den(147) = den(144)*den(146)
  den(148) = den(6)*den(141)
  den(149) = den(2)*den(6)
  den(151) = den(149)*den(150)
  den(152) = den(6)*den(8)
  den(153) = den(150)*den(152)
  den(154) = den(1)*den(32)
  den(155) = den(144)*den(154)
  den(156) = den(1)*den(34)
  den(157) = den(144)*den(156)
  den(158) = den(1)*den(13)
  den(159) = den(150)*den(158)
  den(160) = den(1)*den(15)
  den(161) = den(150)*den(160)
  den(162) = den(63)*den(144)
  den(163) = den(65)*den(144)
  den(164) = den(63)*den(150)
  den(165) = den(65)*den(150)
  den(166) = den(129)*den(144)
  den(167) = den(129)*den(150)
  den(168) = den(131)*den(144)
  den(169) = den(133)*den(144)
  den(170) = den(131)*den(150)
  den(171) = den(133)*den(150)
  den(172) = den(138)*den(144)
  den(173) = den(138)*den(150)
  den(174) = den(2)*den(3)*den(6)
  den(175) = den(3)*den(6)*den(8)
  den(176) = den(1)*den(3)*den(13)
  den(177) = den(1)*den(3)*den(15)
  den(178) = den(1)*den(2)*den(3)
  den(179) = den(1)*den(3)*den(8)
  den(180) = den(2)*den(23)*den(26)
  den(181) = den(8)*den(23)*den(26)
  den(182) = den(1)*den(23)*den(32)
  den(183) = den(1)*den(23)*den(34)
  den(184) = den(1)*den(2)*den(23)
  den(185) = den(1)*den(8)*den(23)
  den(186) = den(2)*den(142)
  den(187) = den(8)*den(142)
  den(188) = den(2)*den(122)
  den(189) = den(8)*den(122)
  den(190) = den(2)*den(148)
  den(191) = den(8)*den(148)
  den(192) = den(2)*den(119)
  den(193) = den(8)*den(119)
  den(194) = den(1)*den(125)
  den(195) = den(1)*den(127)
  den(196) = den(1)*den(115)
  den(197) = den(1)*den(117)
  den(198) = den(3)*den(129)
  den(199) = den(3)*den(131)
  den(200) = den(3)*den(133)
  den(201) = den(23)*den(129)
  den(202) = den(23)*den(131)
  den(203) = den(23)*den(133)
  den(204) = den(3)*den(138)
  den(205) = den(23)*den(138)
  den(206) = den(3)*den(151)
  den(207) = den(3)*den(153)
  den(208) = den(3)*den(159)
  den(209) = den(3)*den(161)
  den(210) = den(3)*den(164)
  den(211) = den(3)*den(165)
  den(212) = den(23)*den(145)
  den(213) = den(23)*den(147)
  den(214) = den(23)*den(155)
  den(215) = den(23)*den(157)
  den(216) = den(23)*den(162)
  den(217) = den(23)*den(163)
  den(218) = den(3)*den(167)
  den(219) = den(3)*den(170)
  den(220) = den(3)*den(171)
  den(221) = den(23)*den(166)
  den(222) = den(23)*den(168)
  den(223) = den(23)*den(169)
  den(224) = den(3)*den(173)
  den(225) = den(23)*den(172)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(122)

  A(1) = cont_QA(wf(:,7),wf(:,8)) * den(7)
  A(2) = cont_QA(wf(:,8),wf(:,11)) * den(10)
  A(3) = cont_QA(wf(:,13),wf(:,14)) * den(14)
  A(4) = cont_QA(wf(:,13),wf(:,16)) * den(16)
  A(5) = cont_VV(wf(:,18),wf(:,19)) * den(20)
  A(6) = cont_VV(wf(:,19),wf(:,20)) * den(22)
  A(7) = cont_QA(wf(:,24),wf(:,25)) * den(27)
  A(8) = cont_QA(wf(:,25),wf(:,26)) * den(29)
  A(9) = cont_QA(wf(:,28),wf(:,29)) * den(33)
  A(10) = cont_QA(wf(:,28),wf(:,31)) * den(35)
  A(11) = cont_VV(wf(:,18),wf(:,33)) * den(37)
  A(12) = cont_VV(wf(:,20),wf(:,33)) * den(38)
  A(13) = cont_QA(wf(:,14),wf(:,34)) * den(39)
  A(14) = cont_QA(wf(:,16),wf(:,34)) * den(40)
  A(15) = cont_QA(wf(:,8),wf(:,35)) * den(41)
  A(16) = cont_QA(wf(:,8),wf(:,36)) * den(42)
  A(17) = cont_VV(wf(:,19),wf(:,39)) * den(45)
  A(18) = cont_VV(wf(:,19),wf(:,42)) * den(48)
  A(19) = cont_VV(wf(:,19),wf(:,45)) * den(50)
  A(20) = cont_VV(wf(:,33),wf(:,39)) * den(51)
  A(21) = cont_VV(wf(:,33),wf(:,42)) * den(52)
  A(22) = cont_VV(wf(:,33),wf(:,45)) * den(53)
  A(23) = cont_VV(wf(:,19),wf(:,48)) * den(56)
  A(24) = cont_VV(wf(:,33),wf(:,48)) * den(57)

  A(25) = cont_QA(wf(:,8),wf(:,49)) * den(7)
  A(26) = cont_QA(wf(:,8),wf(:,50)) * den(10)
  A(27) = cont_QA(wf(:,14),wf(:,51)) * den(14)
  A(28) = cont_QA(wf(:,16),wf(:,51)) * den(16)
  A(29) = cont_QA(wf(:,25),wf(:,52)) * den(27)
  A(30) = cont_QA(wf(:,25),wf(:,53)) * den(29)
  A(31) = cont_QA(wf(:,29),wf(:,54)) * den(33)
  A(32) = cont_QA(wf(:,31),wf(:,54)) * den(35)
  A(33) = cont_QA(wf(:,14),wf(:,55)) * den(39)
  A(34) = cont_QA(wf(:,16),wf(:,55)) * den(40)
  A(35) = cont_VV(wf(:,56),wf(:,57)) * den(60)
  A(36) = cont_QA(wf(:,8),wf(:,58)) * den(41)
  A(37) = cont_QA(wf(:,8),wf(:,59)) * den(42)
  A(38) = cont_VV(wf(:,56),wf(:,60)) * den(61)
  A(39) = cont_VV(wf(:,61),wf(:,62)) * den(64)
  A(40) = cont_VV(wf(:,61),wf(:,63)) * den(66)
  A(41) = cont_QA(wf(:,64),wf(:,65)) * den(69)
  A(42) = cont_QA(wf(:,65),wf(:,66)) * den(70)
  A(43) = cont_QA(wf(:,67),wf(:,68)) * den(73)
  A(44) = cont_QA(wf(:,67),wf(:,69)) * den(75)
  A(45) = cont_QA(wf(:,34),wf(:,70)) * den(39)
  A(46) = cont_QA(wf(:,34),wf(:,71)) * den(40)
  A(47) = cont_QA(wf(:,35),wf(:,72)) * den(41)
  A(48) = cont_QA(wf(:,36),wf(:,72)) * den(42)
  A(49) = cont_QA(wf(:,25),wf(:,75)) * den(27)
  A(50) = cont_QA(wf(:,25),wf(:,76)) * den(29)
  A(51) = cont_QA(wf(:,29),wf(:,77)) * den(33)
  A(52) = cont_QA(wf(:,31),wf(:,77)) * den(35)
  A(53) = cont_VV(wf(:,18),wf(:,79)) * den(37)
  A(54) = cont_VV(wf(:,20),wf(:,79)) * den(38)
  A(55) = cont_VV(wf(:,62),wf(:,80)) * den(76)
  A(56) = cont_VV(wf(:,63),wf(:,80)) * den(77)
  A(57) = cont_QA(wf(:,81),wf(:,82)) * den(80)
  A(58) = cont_QA(wf(:,82),wf(:,83)) * den(81)
  A(59) = cont_QA(wf(:,84),wf(:,85)) * den(84)
  A(60) = cont_QA(wf(:,84),wf(:,86)) * den(86)
  A(61) = cont_QA(wf(:,8),wf(:,88)) * den(41)
  A(62) = cont_QA(wf(:,8),wf(:,90)) * den(42)
  A(63) = cont_QA(wf(:,14),wf(:,92)) * den(39)
  A(64) = cont_QA(wf(:,16),wf(:,92)) * den(40)
  A(65) = cont_QA(wf(:,8),wf(:,95)) * den(7)
  A(66) = cont_QA(wf(:,8),wf(:,96)) * den(10)
  A(67) = cont_QA(wf(:,14),wf(:,97)) * den(14)
  A(68) = cont_QA(wf(:,16),wf(:,97)) * den(16)
  A(69) = cont_VV(wf(:,18),wf(:,99)) * den(20)
  A(70) = cont_VV(wf(:,20),wf(:,99)) * den(22)
  A(71) = cont_VV(wf(:,39),wf(:,100)) * den(45)
  A(72) = cont_VV(wf(:,42),wf(:,100)) * den(48)
  A(73) = cont_VV(wf(:,45),wf(:,100)) * den(50)
  A(74) = cont_VV(wf(:,39),wf(:,79)) * den(51)
  A(75) = cont_VV(wf(:,42),wf(:,79)) * den(52)
  A(76) = cont_VV(wf(:,45),wf(:,79)) * den(53)
  A(77) = cont_VV(wf(:,39),wf(:,101)) * den(51)
  A(78) = cont_VV(wf(:,42),wf(:,101)) * den(52)
  A(79) = cont_VV(wf(:,45),wf(:,101)) * den(53)
  A(80) = cont_VV(wf(:,39),wf(:,99)) * den(45)
  A(81) = cont_VV(wf(:,42),wf(:,99)) * den(48)
  A(82) = cont_VV(wf(:,45),wf(:,99)) * den(50)
  A(83) = cont_VV(wf(:,48),wf(:,100)) * den(56)
  A(84) = cont_VV(wf(:,48),wf(:,79)) * den(57)
  A(85) = cont_VV(wf(:,48),wf(:,101)) * den(57)
  A(86) = cont_VV(wf(:,48),wf(:,99)) * den(56)
  A(87) = cont_VV(wf(:,62),wf(:,104)) * den(88)
  A(88) = cont_VV(wf(:,63),wf(:,104)) * den(89)
  A(89) = cont_QA(wf(:,8),wf(:,105)) * den(91)
  A(90) = cont_QA(wf(:,8),wf(:,106)) * den(93)
  A(91) = cont_QA(wf(:,14),wf(:,107)) * den(95)
  A(92) = cont_QA(wf(:,16),wf(:,107)) * den(96)
  A(93) = cont_QA(wf(:,68),wf(:,108)) * den(97)
  A(94) = cont_QA(wf(:,69),wf(:,108)) * den(98)
  A(95) = cont_QA(wf(:,65),wf(:,109)) * den(99)
  A(96) = cont_QA(wf(:,65),wf(:,110)) * den(100)
  A(97) = cont_VV(wf(:,62),wf(:,113)) * den(102)
  A(98) = cont_VV(wf(:,63),wf(:,113)) * den(103)
  A(99) = cont_QA(wf(:,85),wf(:,114)) * den(104)
  A(100) = cont_QA(wf(:,86),wf(:,114)) * den(105)
  A(101) = cont_QA(wf(:,82),wf(:,115)) * den(106)
  A(102) = cont_QA(wf(:,82),wf(:,116)) * den(107)
  A(103) = cont_QA(wf(:,25),wf(:,117)) * den(109)
  A(104) = cont_QA(wf(:,25),wf(:,118)) * den(111)
  A(105) = cont_QA(wf(:,29),wf(:,119)) * den(113)
  A(106) = cont_QA(wf(:,31),wf(:,119)) * den(114)
  A(107) = cont_QA(wf(:,114),wf(:,121)) * den(116)
  A(108) = cont_QA(wf(:,114),wf(:,123)) * den(118)
  A(109) = cont_QA(wf(:,115),wf(:,125)) * den(120)
  A(110) = cont_QA(wf(:,116),wf(:,125)) * den(121)
  A(111) = cont_QA(wf(:,109),wf(:,126)) * den(123)
  A(112) = cont_QA(wf(:,110),wf(:,126)) * den(124)
  A(113) = cont_QA(wf(:,108),wf(:,127)) * den(126)
  A(114) = cont_QA(wf(:,108),wf(:,128)) * den(128)
  A(115) = cont_VV(wf(:,104),wf(:,129)) * den(130)
  A(116) = cont_VV(wf(:,104),wf(:,130)) * den(132)
  A(117) = cont_VV(wf(:,104),wf(:,131)) * den(134)
  A(118) = cont_VV(wf(:,113),wf(:,129)) * den(135)
  A(119) = cont_VV(wf(:,113),wf(:,130)) * den(136)
  A(120) = cont_VV(wf(:,113),wf(:,131)) * den(137)
  A(121) = cont_VV(wf(:,104),wf(:,132)) * den(139)
  A(122) = cont_VV(wf(:,113),wf(:,132)) * den(140)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(122)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(23)-A(24))*f(1)+(-A(6)-A(12))*f(5)+(-A(3)-A(7)-A(13))*f(9)+(A(1)+A(9)+A(15))*f(10)+(-A(2)-A(4)+A(5)-A(8)-A(10)+A(11) &
       -A(14)-A(16)-A(17)-A(18)-A(19)-A(20)-A(21)-A(22))*f(11)

  M2(1) = (A(121)+A(122))*f(2)+(-A(84)-A(86))*f(3)+(-A(83)-A(85))*f(4)+(A(88)+A(98))*f(6)+(-A(54)-A(70))*f(7)+(-A(40)-A(56))*f(8) &
       +(A(91)+A(95)+A(99)+A(103)+A(107)+A(111))*f(12)+(-A(89)-A(93)-A(101)-A(105)-A(109)-A(113))*f(13)+(-A(87)+A(90)+A(92)+A(94) &
       +A(96)-A(97)+A(100)+A(102)+A(104)+A(106)+A(108)+A(110)+A(112)+A(114)+A(115)+A(116)+A(117)+A(118)+A(119)+A(120))*f(14)+( &
       -A(33)-A(49)-A(67))*f(15)+(A(36)+A(51)+A(65))*f(16)+(-A(34)-A(37)-A(50)-A(52)+A(53)-A(66)-A(68)+A(69)-A(74)-A(75)-A(76) &
       -A(80)-A(81)-A(82))*f(17)+(-A(27)-A(29)-A(41)-A(45)-A(59)-A(63))*f(18)+(A(25)+A(31)+A(43)+A(47)+A(57)+A(61))*f(19)+(-A(26) &
       -A(28)-A(30)-A(32)+A(39)-A(42)-A(44)-A(46)-A(48)+A(55)-A(58)-A(60)-A(62)-A(64)-A(71)-A(72)-A(73)-A(77)-A(78)-A(79))*f(20) &
       +(A(35)+A(38))*f(21)

end subroutine colourvectors

end module ol_loop_ppllllj_nmeexmxuxdg_1_/**/REALKIND
