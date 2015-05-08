
module ol_colourmatrix_ppllllj_nexnmemxddxg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllllj_nexnmemxddxg_1_/**/REALKIND



module ol_forced_parameters_ppllllj_nexnmemxddxg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj_nexnmemxddxg_1_/**/REALKIND

module ol_loop_ppllllj_nexnmemxddxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(39), c(23)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:126)
  ! denominators
  complex(REALKIND), save :: den(169)
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
    f( 9) = (countertermnorm*ctZGG*cw*eQED**4*gQCD**3)/(sw**3*2._/**/REALKIND)
    f(10) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MW)/(2._/**/REALKIND*sw**3)
    f(11) = (CI*eQED**4*gQCD)/(6._/**/REALKIND*sw**2)
    f(12) = (CI*eQED**4*gQCD)/(2._/**/REALKIND*sw**2)
    f(13) = (CI*countertermnorm*eQED**4*gQCD**3)/(6._/**/REALKIND*sw**2)
    f(14) = (CI*countertermnorm*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(15) = (CI*countertermnorm*ctGqq*eQED**4*gQCD**3)/(6._/**/REALKIND*sw**2)
    f(16) = (CI*countertermnorm*ctGqq*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(17) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**3)/(6._/**/REALKIND*sw**2)
    f(18) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(19) = (CI*countertermnorm*ctWWGG*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(20) = (countertermnorm*ctZGG*eQED**4*gQCD**3)/(sw**2*2._/**/REALKIND)
    f(21) = (CI*eQED**4*gQCD**3*integralnorm*SwB)/(4._/**/REALKIND*sw**4)
    f(22) = (eQED**4*gQCD**3*integralnorm*SwB)/(sw**4*4._/**/REALKIND)
    f(23) = (CI*cw*eQED**4*gQCD**3*integralnorm*SwB)/(2._/**/REALKIND*sw**3)
    f(24) = (cw*eQED**4*gQCD**3*integralnorm*SwB)/(sw**3*2._/**/REALKIND)
    f(25) = (CI*eQED**4*gQCD**3*integralnorm*SwB)/(6._/**/REALKIND*sw**2)
    f(26) = (CI*eQED**4*gQCD**3*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(27) = (eQED**4*gQCD**3*integralnorm*SwB)/(sw**2*6._/**/REALKIND)
    f(28) = (eQED**4*gQCD**3*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(29) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**4*4._/**/REALKIND)
    f(30) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**4*2._/**/REALKIND)
    f(31) = (eQED**4*gQCD**3*integralnorm*MB*SwF)/(sw**4*4._/**/REALKIND)
    f(32) = (eQED**4*gQCD**3*integralnorm*MT*SwF)/(sw**4*4._/**/REALKIND)
    f(33) = (cw*eQED**4*gQCD**3*integralnorm*SwF)/(sw**3*2._/**/REALKIND)
    f(34) = (cw*eQED**4*gQCD**3*integralnorm*SwF)/sw**3
    f(35) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*6._/**/REALKIND)
    f(36) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(37) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(38) = (2*eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(39) = (eQED**4*gQCD**3*integralnorm*SwF)/sw**2

  c = [ 9*CI*f(21), f(22), 8*f(22), 9*CI*f(23), f(24), 8*f(24), 9*CI*f(25), 9*CI*f(26), f(27), 8*f(27), f(28), 8*f(28), 3*f(29) &
    , 3*f(30), 3*f(31), 3*f(32), 3*f(33), 3*f(34), 3*f(35), 3*f(36), 3*f(37), 3*f(38), 3*f(39) ]
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
  complex(REALKIND) :: A(89)
  ! external WFs
  call wf_A(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_W(wf(:,-2),wf(:,0),wf(:,1))
  call vert_QA_W(wf(:,-1),wf(:,-3),wf(:,2))
  call vert_VQ_A(wf(:,-6),wf(:,-4),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,5),MW,1_intkind1,wf(:,4))
  call prop_W_W(wf(:,2),Q(:,10),MW,1_intkind1,wf(:,5))
  call prop_Q_A(wf(:,3),Q(:,80),ZERO,0_intkind1,wf(:,6))
  call vert_AW_Q(wf(:,-5),wf(:,4),wf(:,7))
  call vert_WQ_A(wf(:,5),wf(:,6),wf(:,8))
  call prop_A_Q(wf(:,7),Q(:,37),ZERO,0_intkind1,wf(:,9))
  call vert_QA_V(wf(:,6),wf(:,-5),wf(:,10))
  call vert_UV_W(wf(:,5),Q(:,10),wf(:,4),Q(:,5),wf(:,11))
  call vert_QA_Z(gZd,wf(:,6),wf(:,-5),wf(:,12))
  call prop_W_W(wf(:,12),Q(:,112),MZ,1_intkind1,wf(:,13))
  call vert_AV_Q(wf(:,-5),wf(:,-6),wf(:,14))
  call prop_A_Q(wf(:,14),Q(:,96),ZERO,0_intkind1,wf(:,15))
  call vert_WQ_A(wf(:,5),wf(:,-4),wf(:,16))
  call vert_AW_Q(wf(:,15),wf(:,4),wf(:,17))
  call prop_Q_A(wf(:,16),Q(:,26),ZERO,0_intkind1,wf(:,18))
  call vert_QA_V(wf(:,-4),wf(:,15),wf(:,19))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,15),wf(:,20))
  call prop_W_W(wf(:,20),Q(:,112),MZ,1_intkind1,wf(:,21))
  call vert_VQ_A(wf(:,-6),wf(:,18),wf(:,22))
  call vert_WQ_A(wf(:,4),wf(:,-1),wf(:,23))
  call prop_Q_A(wf(:,23),Q(:,7),ZERO,0_intkind1,wf(:,24))
  call vert_QA_V(wf(:,24),wf(:,-3),wf(:,25))
  call vert_QA_Z(gZl,wf(:,24),wf(:,-3),wf(:,26))
  call vert_AW_Q(wf(:,-3),wf(:,4),wf(:,27))
  call prop_A_Q(wf(:,27),Q(:,13),ZERO,0_intkind1,wf(:,28))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,28),wf(:,29))
  call vert_AW_Q(wf(:,0),wf(:,5),wf(:,30))
  call prop_A_Q(wf(:,30),Q(:,11),ZERO,0_intkind1,wf(:,31))
  call vert_QA_V(wf(:,-2),wf(:,31),wf(:,32))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,31),wf(:,33))
  call vert_WQ_A(wf(:,5),wf(:,-2),wf(:,34))
  call prop_Q_A(wf(:,34),Q(:,14),ZERO,0_intkind1,wf(:,35))
  call vert_QA_Z(gZn,wf(:,35),wf(:,0),wf(:,36))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,37))
  call counter_VVG_G(wf(:,4),wf(:,5),wf(:,-6),wf(:,38))
  call vert_VV_S(wf(:,4),wf(:,5),wf(:,39))
  call counter_GG_S(wf(:,37),wf(:,-6),wf(:,40))
  call counter_GG_V(wf(:,37),Q(:,48),wf(:,-6),Q(:,64),wf(:,41))
  call prop_W_W(wf(:,11),Q(:,15),MZ,1_intkind1,wf(:,42))
  call counter_WQ_A(wf(:,5),wf(:,6),wf(:,43))
  call counter_AW_Q(wf(:,15),wf(:,4),wf(:,44))
  call counter_VQ_A(wf(:,-6),wf(:,18),wf(:,45))
  call counter_QA_V(wf(:,6),wf(:,-5),wf(:,46))
  call counter_QA_Z(gZd,wf(:,6),wf(:,-5),wf(:,47))
  call counter_AW_Q(wf(:,-5),wf(:,4),wf(:,48))
  call prop_Q_A(wf(:,8),Q(:,90),ZERO,0_intkind1,wf(:,49))
  call prop_A_Q(wf(:,48),Q(:,37),ZERO,0_intkind1,wf(:,50))
  call counter_AV_Q(wf(:,-5),wf(:,-6),wf(:,51))
  call prop_A_Q(wf(:,51),Q(:,96),ZERO,0_intkind1,wf(:,52))
  call vert_AW_Q(wf(:,52),wf(:,4),wf(:,53))
  call vert_QA_V(wf(:,-4),wf(:,52),wf(:,54))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,52),wf(:,55))
  call prop_W_W(wf(:,55),Q(:,112),MZ,1_intkind1,wf(:,56))
  call counter_QA_V(wf(:,-4),wf(:,15),wf(:,57))
  call counter_QA_Z(gZd,wf(:,-4),wf(:,15),wf(:,58))
  call counter_WQ_A(wf(:,5),wf(:,-4),wf(:,59))
  call prop_A_Q(wf(:,17),Q(:,101),ZERO,0_intkind1,wf(:,60))
  call prop_Q_A(wf(:,59),Q(:,26),ZERO,0_intkind1,wf(:,61))
  call vert_VQ_A(wf(:,-6),wf(:,61),wf(:,62))
  call counter_VQ_A(wf(:,-6),wf(:,-4),wf(:,63))
  call prop_Q_A(wf(:,63),Q(:,80),ZERO,0_intkind1,wf(:,64))
  call vert_WQ_A(wf(:,5),wf(:,64),wf(:,65))
  call vert_QA_V(wf(:,64),wf(:,-5),wf(:,66))
  call vert_QA_Z(gZd,wf(:,64),wf(:,-5),wf(:,67))
  call prop_W_W(wf(:,67),Q(:,112),MZ,1_intkind1,wf(:,68))
  call prop_W_W(wf(:,41),Q(:,112),MZ,1_intkind1,wf(:,69))
  call prop_W_W(wf(:,47),Q(:,112),MZ,1_intkind1,wf(:,70))
  call prop_W_W(wf(:,58),Q(:,112),MZ,1_intkind1,wf(:,71))
  call counter_Q_A(ctqq,wf(:,6),Q(:,80),wf(:,72))
  call prop_Q_A(wf(:,72),Q(:,80),ZERO,0_intkind1,wf(:,73))
  call vert_QA_V(wf(:,73),wf(:,-5),wf(:,74))
  call vert_QA_Z(gZd,wf(:,73),wf(:,-5),wf(:,75))
  call vert_WQ_A(wf(:,5),wf(:,73),wf(:,76))
  call counter_A_Q(ctqq,wf(:,9),Q(:,37),wf(:,77))
  call counter_A_Q(ctqq,wf(:,15),Q(:,96),wf(:,78))
  call prop_A_Q(wf(:,78),Q(:,96),ZERO,0_intkind1,wf(:,79))
  call vert_QA_V(wf(:,-4),wf(:,79),wf(:,80))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,79),wf(:,81))
  call counter_Q_A(ctqq,wf(:,18),Q(:,26),wf(:,82))
  call vert_AW_Q(wf(:,79),wf(:,4),wf(:,83))
  call vert_AV_Q(wf(:,9),wf(:,-6),wf(:,84))
  call prop_A_Q(wf(:,84),Q(:,101),ZERO,0_intkind1,wf(:,85))
  call prop_Q_A(wf(:,22),Q(:,90),ZERO,0_intkind1,wf(:,86))
  call prop_W_W(wf(:,26),Q(:,15),MZ,1_intkind1,wf(:,87))
  call prop_W_W(wf(:,29),Q(:,15),MZ,1_intkind1,wf(:,88))
  call prop_W_W(wf(:,33),Q(:,15),MZ,1_intkind1,wf(:,89))
  call prop_W_W(wf(:,36),Q(:,15),MZ,1_intkind1,wf(:,90))
  call vert_AW_Q(wf(:,9),wf(:,5),wf(:,91))
  call prop_A_Q(wf(:,91),Q(:,47),ZERO,0_intkind1,wf(:,92))
  call vert_WQ_A(wf(:,4),wf(:,18),wf(:,93))
  call prop_Q_A(wf(:,93),Q(:,31),ZERO,0_intkind1,wf(:,94))
  call vert_VQ_A(wf(:,11),wf(:,-4),wf(:,95))
  call prop_Q_A(wf(:,95),Q(:,31),ZERO,0_intkind1,wf(:,96))
  call vert_ZQ_A(gZd,wf(:,42),wf(:,-4),wf(:,97))
  call prop_Q_A(wf(:,97),Q(:,31),ZERO,0_intkind1,wf(:,98))
  call vert_AV_Q(wf(:,-5),wf(:,11),wf(:,99))
  call prop_A_Q(wf(:,99),Q(:,47),ZERO,0_intkind1,wf(:,100))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,42),wf(:,101))
  call prop_A_Q(wf(:,101),Q(:,47),ZERO,0_intkind1,wf(:,102))
  call vert_VQ_A(wf(:,25),wf(:,-4),wf(:,103))
  call prop_Q_A(wf(:,103),Q(:,31),ZERO,0_intkind1,wf(:,104))
  call vert_ZQ_A(gZd,wf(:,87),wf(:,-4),wf(:,105))
  call prop_Q_A(wf(:,105),Q(:,31),ZERO,0_intkind1,wf(:,106))
  call vert_AV_Q(wf(:,-5),wf(:,25),wf(:,107))
  call prop_A_Q(wf(:,107),Q(:,47),ZERO,0_intkind1,wf(:,108))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,87),wf(:,109))
  call prop_A_Q(wf(:,109),Q(:,47),ZERO,0_intkind1,wf(:,110))
  call vert_ZQ_A(gZd,wf(:,88),wf(:,-4),wf(:,111))
  call prop_Q_A(wf(:,111),Q(:,31),ZERO,0_intkind1,wf(:,112))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,88),wf(:,113))
  call prop_A_Q(wf(:,113),Q(:,47),ZERO,0_intkind1,wf(:,114))
  call vert_VQ_A(wf(:,32),wf(:,-4),wf(:,115))
  call prop_Q_A(wf(:,115),Q(:,31),ZERO,0_intkind1,wf(:,116))
  call vert_ZQ_A(gZd,wf(:,89),wf(:,-4),wf(:,117))
  call prop_Q_A(wf(:,117),Q(:,31),ZERO,0_intkind1,wf(:,118))
  call vert_AV_Q(wf(:,-5),wf(:,32),wf(:,119))
  call prop_A_Q(wf(:,119),Q(:,47),ZERO,0_intkind1,wf(:,120))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,89),wf(:,121))
  call prop_A_Q(wf(:,121),Q(:,47),ZERO,0_intkind1,wf(:,122))
  call vert_ZQ_A(gZd,wf(:,90),wf(:,-4),wf(:,123))
  call prop_Q_A(wf(:,123),Q(:,31),ZERO,0_intkind1,wf(:,124))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,90),wf(:,125))
  call prop_A_Q(wf(:,125),Q(:,47),ZERO,0_intkind1,wf(:,126))

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
  den(3) = 1 / (Q(5,80))
  den(5) = 1 / (Q(5,37))
  den(9) = 1 / (Q(5,112))
  den(12) = 1 / (Q(5,112) - MZ2)
  den(15) = 1 / (Q(5,96))
  den(17) = 1 / (Q(5,26))
  den(25) = 1 / (Q(5,7))
  den(29) = 1 / (Q(5,13))
  den(35) = 1 / (Q(5,11))
  den(39) = 1 / (Q(5,14))
  den(45) = 1 / (Q(5,48))
  den(47) = 1 / (Q(5,15) - MH2)
  den(50) = 1 / (Q(5,15) - MZ2)
  den(53) = 1 / (Q(5,15))
  den(57) = 1 / (Q(5,90))
  den(62) = 1 / (Q(5,101))
  den(105) = 1 / (Q(5,47))
  den(108) = 1 / (Q(5,31))

  ! denominators
  den(4) = den(2)*den(3)
  den(6) = den(1)*den(5)
  den(7) = den(4)*den(6)
  den(8) = den(1)*den(2)
  den(10) = den(3)*den(9)
  den(11) = den(8)*den(10)
  den(13) = den(3)*den(12)
  den(14) = den(8)*den(13)
  den(16) = den(1)*den(15)
  den(18) = den(2)*den(17)
  den(19) = den(16)*den(18)
  den(20) = den(9)*den(15)
  den(21) = den(8)*den(20)
  den(22) = den(12)*den(15)
  den(23) = den(8)*den(22)
  den(24) = den(6)*den(18)
  den(26) = den(1)*den(25)
  den(27) = den(10)*den(26)
  den(28) = den(13)*den(26)
  den(30) = den(1)*den(29)
  den(31) = den(13)*den(30)
  den(32) = den(20)*den(26)
  den(33) = den(22)*den(26)
  den(34) = den(22)*den(30)
  den(36) = den(2)*den(35)
  den(37) = den(10)*den(36)
  den(38) = den(13)*den(36)
  den(40) = den(2)*den(39)
  den(41) = den(13)*den(40)
  den(42) = den(20)*den(36)
  den(43) = den(22)*den(36)
  den(44) = den(22)*den(40)
  den(46) = den(8)*den(45)
  den(48) = den(8)*den(47)
  den(49) = den(45)*den(48)
  den(51) = den(8)*den(50)
  den(52) = den(45)*den(51)
  den(54) = den(8)*den(53)
  den(55) = den(3)*den(54)
  den(56) = den(3)*den(51)
  den(58) = den(4)*den(57)
  den(59) = den(1)*den(58)
  den(60) = den(15)*den(54)
  den(61) = den(15)*den(51)
  den(63) = den(16)*den(62)
  den(64) = den(2)*den(63)
  den(65) = den(12)*den(45)
  den(66) = den(26)*den(65)
  den(67) = den(30)*den(65)
  den(68) = den(36)*den(65)
  den(69) = den(40)*den(65)
  den(70) = den(3)**2
  den(71) = den(54)*den(70)
  den(72) = den(51)*den(70)
  den(73) = den(2)*den(70)
  den(74) = den(6)*den(73)
  den(75) = den(6)*den(58)
  den(76) = den(15)**2
  den(77) = den(54)*den(76)
  den(78) = den(51)*den(76)
  den(79) = den(18)*den(63)
  den(80) = den(1)*den(76)
  den(81) = den(18)*den(80)
  den(82) = den(6)*den(62)
  den(83) = den(18)*den(82)
  den(84) = den(18)*den(57)
  den(85) = den(6)*den(84)
  den(86) = den(26)*den(53)
  den(87) = den(70)*den(86)
  den(88) = den(26)*den(50)
  den(89) = den(70)*den(88)
  den(90) = den(30)*den(50)
  den(91) = den(70)*den(90)
  den(92) = den(76)*den(86)
  den(93) = den(76)*den(88)
  den(94) = den(76)*den(90)
  den(95) = den(36)*den(53)
  den(96) = den(70)*den(95)
  den(97) = den(36)*den(50)
  den(98) = den(70)*den(97)
  den(99) = den(40)*den(50)
  den(100) = den(70)*den(99)
  den(101) = den(76)*den(95)
  den(102) = den(76)*den(97)
  den(103) = den(76)*den(99)
  den(104) = den(2)*den(6)
  den(106) = den(104)*den(105)
  den(107) = den(1)*den(18)
  den(109) = den(107)*den(108)
  den(110) = den(54)*den(108)
  den(111) = den(51)*den(108)
  den(112) = den(54)*den(105)
  den(113) = den(51)*den(105)
  den(114) = den(86)*den(108)
  den(115) = den(88)*den(108)
  den(116) = den(86)*den(105)
  den(117) = den(88)*den(105)
  den(118) = den(90)*den(108)
  den(119) = den(90)*den(105)
  den(120) = den(95)*den(108)
  den(121) = den(97)*den(108)
  den(122) = den(95)*den(105)
  den(123) = den(97)*den(105)
  den(124) = den(99)*den(108)
  den(125) = den(99)*den(105)
  den(126) = den(45)*den(54)
  den(127) = den(1)*den(2)*den(45)
  den(128) = den(2)*den(3)*den(6)
  den(129) = den(1)*den(2)*den(3)
  den(130) = den(1)*den(15)*den(18)
  den(131) = den(1)*den(2)*den(15)
  den(132) = den(2)*den(82)
  den(133) = den(1)*den(84)
  den(134) = den(45)*den(86)
  den(135) = den(45)*den(88)
  den(136) = den(45)*den(90)
  den(137) = den(3)*den(86)
  den(138) = den(3)*den(88)
  den(139) = den(3)*den(90)
  den(140) = den(15)*den(86)
  den(141) = den(15)*den(88)
  den(142) = den(15)*den(90)
  den(143) = den(45)*den(95)
  den(144) = den(45)*den(97)
  den(145) = den(45)*den(99)
  den(146) = den(3)*den(95)
  den(147) = den(3)*den(97)
  den(148) = den(3)*den(99)
  den(149) = den(15)*den(95)
  den(150) = den(15)*den(97)
  den(151) = den(15)*den(99)
  den(152) = den(3)*den(106)
  den(153) = den(3)*den(112)
  den(154) = den(3)*den(113)
  den(155) = den(15)*den(109)
  den(156) = den(15)*den(110)
  den(157) = den(15)*den(111)
  den(158) = den(3)*den(116)
  den(159) = den(3)*den(117)
  den(160) = den(3)*den(119)
  den(161) = den(15)*den(114)
  den(162) = den(15)*den(115)
  den(163) = den(15)*den(118)
  den(164) = den(3)*den(122)
  den(165) = den(3)*den(123)
  den(166) = den(3)*den(125)
  den(167) = den(15)*den(120)
  den(168) = den(15)*den(121)
  den(169) = den(15)*den(124)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(89)

  A(1) = cont_QA(wf(:,8),wf(:,9)) * den(7)
  A(2) = cont_VV(wf(:,10),wf(:,11)) * den(11)
  A(3) = cont_VV(wf(:,11),wf(:,13)) * den(14)
  A(4) = cont_QA(wf(:,17),wf(:,18)) * den(19)
  A(5) = cont_VV(wf(:,11),wf(:,19)) * den(21)
  A(6) = cont_VV(wf(:,11),wf(:,21)) * den(23)
  A(7) = cont_QA(wf(:,9),wf(:,22)) * den(24)
  A(8) = cont_VV(wf(:,10),wf(:,25)) * den(27)
  A(9) = cont_VV(wf(:,13),wf(:,26)) * den(28)
  A(10) = cont_VV(wf(:,13),wf(:,29)) * den(31)
  A(11) = cont_VV(wf(:,19),wf(:,25)) * den(32)
  A(12) = cont_VV(wf(:,21),wf(:,26)) * den(33)
  A(13) = cont_VV(wf(:,21),wf(:,29)) * den(34)
  A(14) = cont_VV(wf(:,10),wf(:,32)) * den(37)
  A(15) = cont_VV(wf(:,13),wf(:,33)) * den(38)
  A(16) = cont_VV(wf(:,13),wf(:,36)) * den(41)
  A(17) = cont_VV(wf(:,19),wf(:,32)) * den(42)
  A(18) = cont_VV(wf(:,21),wf(:,33)) * den(43)
  A(19) = cont_VV(wf(:,21),wf(:,36)) * den(44)

  A(20) = cont_VV(wf(:,37),wf(:,38)) * den(46)
  A(21) = cont_SS(wf(:,39),wf(:,40)) * den(49)
  A(22) = cont_VV(wf(:,41),wf(:,42)) * den(52)
  A(23) = cont_QA(wf(:,9),wf(:,43)) * den(7)
  A(24) = cont_QA(wf(:,18),wf(:,44)) * den(19)
  A(25) = cont_QA(wf(:,9),wf(:,45)) * den(24)
  A(26) = cont_VV(wf(:,11),wf(:,46)) * den(55)
  A(27) = cont_VV(wf(:,42),wf(:,47)) * den(56)
  A(28) = cont_QA(wf(:,48),wf(:,49)) * den(59)
  A(29) = cont_QA(wf(:,22),wf(:,50)) * den(24)
  A(30) = cont_QA(wf(:,18),wf(:,53)) * den(19)
  A(31) = cont_VV(wf(:,11),wf(:,54)) * den(21)
  A(32) = cont_VV(wf(:,11),wf(:,56)) * den(23)
  A(33) = cont_VV(wf(:,11),wf(:,57)) * den(60)
  A(34) = cont_VV(wf(:,42),wf(:,58)) * den(61)
  A(35) = cont_QA(wf(:,59),wf(:,60)) * den(64)
  A(36) = cont_QA(wf(:,9),wf(:,62)) * den(24)
  A(37) = cont_QA(wf(:,9),wf(:,65)) * den(7)
  A(38) = cont_VV(wf(:,11),wf(:,66)) * den(11)
  A(39) = cont_VV(wf(:,11),wf(:,68)) * den(14)
  A(40) = cont_VV(wf(:,26),wf(:,69)) * den(66)
  A(41) = cont_VV(wf(:,29),wf(:,69)) * den(67)
  A(42) = cont_VV(wf(:,25),wf(:,46)) * den(27)
  A(43) = cont_VV(wf(:,26),wf(:,70)) * den(28)
  A(44) = cont_VV(wf(:,29),wf(:,70)) * den(31)
  A(45) = cont_VV(wf(:,25),wf(:,54)) * den(32)
  A(46) = cont_VV(wf(:,26),wf(:,56)) * den(33)
  A(47) = cont_VV(wf(:,29),wf(:,56)) * den(34)
  A(48) = cont_VV(wf(:,25),wf(:,57)) * den(32)
  A(49) = cont_VV(wf(:,26),wf(:,71)) * den(33)
  A(50) = cont_VV(wf(:,29),wf(:,71)) * den(34)
  A(51) = cont_VV(wf(:,25),wf(:,66)) * den(27)
  A(52) = cont_VV(wf(:,26),wf(:,68)) * den(28)
  A(53) = cont_VV(wf(:,29),wf(:,68)) * den(31)
  A(54) = cont_VV(wf(:,33),wf(:,69)) * den(68)
  A(55) = cont_VV(wf(:,36),wf(:,69)) * den(69)
  A(56) = cont_VV(wf(:,32),wf(:,46)) * den(37)
  A(57) = cont_VV(wf(:,33),wf(:,70)) * den(38)
  A(58) = cont_VV(wf(:,36),wf(:,70)) * den(41)
  A(59) = cont_VV(wf(:,32),wf(:,54)) * den(42)
  A(60) = cont_VV(wf(:,33),wf(:,56)) * den(43)
  A(61) = cont_VV(wf(:,36),wf(:,56)) * den(44)
  A(62) = cont_VV(wf(:,32),wf(:,57)) * den(42)
  A(63) = cont_VV(wf(:,33),wf(:,71)) * den(43)
  A(64) = cont_VV(wf(:,36),wf(:,71)) * den(44)
  A(65) = cont_VV(wf(:,32),wf(:,66)) * den(37)
  A(66) = cont_VV(wf(:,33),wf(:,68)) * den(38)
  A(67) = cont_VV(wf(:,36),wf(:,68)) * den(41)
  A(68) = cont_VV(wf(:,11),wf(:,74)) * den(71)
  A(69) = cont_VV(wf(:,42),wf(:,75)) * den(72)
  A(70) = cont_QA(wf(:,9),wf(:,76)) * den(74)
  A(71) = cont_QA(wf(:,49),wf(:,77)) * den(75)
  A(72) = cont_VV(wf(:,11),wf(:,80)) * den(77)
  A(73) = cont_VV(wf(:,42),wf(:,81)) * den(78)
  A(74) = cont_QA(wf(:,60),wf(:,82)) * den(79)
  A(75) = cont_QA(wf(:,18),wf(:,83)) * den(81)
  A(76) = cont_QA(wf(:,82),wf(:,85)) * den(83)
  A(77) = cont_QA(wf(:,77),wf(:,86)) * den(85)
  A(78) = cont_VV(wf(:,25),wf(:,74)) * den(87)
  A(79) = cont_VV(wf(:,75),wf(:,87)) * den(89)
  A(80) = cont_VV(wf(:,75),wf(:,88)) * den(91)
  A(81) = cont_VV(wf(:,25),wf(:,80)) * den(92)
  A(82) = cont_VV(wf(:,81),wf(:,87)) * den(93)
  A(83) = cont_VV(wf(:,81),wf(:,88)) * den(94)
  A(84) = cont_VV(wf(:,32),wf(:,74)) * den(96)
  A(85) = cont_VV(wf(:,75),wf(:,89)) * den(98)
  A(86) = cont_VV(wf(:,75),wf(:,90)) * den(100)
  A(87) = cont_VV(wf(:,32),wf(:,80)) * den(101)
  A(88) = cont_VV(wf(:,81),wf(:,89)) * den(102)
  A(89) = cont_VV(wf(:,81),wf(:,90)) * den(103)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(89)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(1)+A(4)+A(7))*f(1)+(A(3)+A(6))*f(5)+(-A(2)-A(5)+A(8)+A(11)+A(14)+A(17))*f(11)+(A(9)+A(10)+A(12)+A(13)+A(15)+A(16) &
       +A(18)+A(19))*f(12)

  M2(1) = (-A(70)-A(71)-A(74)-A(75)-A(76)-A(77))*f(2)+(A(25)+A(30)+A(37))*f(3)+(A(23)+A(24)+A(28)+A(29)+A(35)+A(36))*f(4)+(-A(69) &
       -A(73))*f(6)+(A(32)+A(39))*f(7)+(A(27)+A(34))*f(8)-A(22)*f(9)-A(21)*f(10)+(A(68)+A(72)-A(78)-A(81)-A(84)-A(87))*f(13)+( &
       -A(79)-A(80)-A(82)-A(83)-A(85)-A(86)-A(88)-A(89))*f(14)+(-A(31)-A(38)+A(45)+A(51)+A(59)+A(65))*f(15)+(A(46)+A(47)+A(52) &
       +A(53)+A(60)+A(61)+A(66)+A(67))*f(16)+(-A(26)-A(33)+A(42)+A(48)+A(56)+A(62))*f(17)+(A(43)+A(44)+A(49)+A(50)+A(57)+A(58) &
       +A(63)+A(64))*f(18)+A(20)*f(19)+(-A(40)-A(41)-A(54)-A(55))*f(20)

end subroutine colourvectors

end module ol_loop_ppllllj_nexnmemxddxg_1_/**/REALKIND
