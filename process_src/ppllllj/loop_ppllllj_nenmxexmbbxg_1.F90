
module ol_colourmatrix_ppllllj_nenmxexmbbxg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllllj_nenmxexmbbxg_1_/**/REALKIND



module ol_forced_parameters_ppllllj_nenmxexmbbxg_1_/**/REALKIND
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
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllllj_nenmxexmbbxg_1_/**/REALKIND

module ol_loop_ppllllj_nenmxexmbbxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(46), c(26)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:138)
  ! denominators
  complex(REALKIND), save :: den(182)
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
    f( 3) = (CI*countertermnorm*ctGbb*eQED**4*gQCD**3)/(4._/**/REALKIND*sw**4)
    f( 4) = (CI*countertermnorm*ctGtt*eQED**4*gQCD**3)/(4._/**/REALKIND*sw**4)
    f( 5) = (CI*countertermnorm*ctVbt*eQED**4*gQCD**3)/(4._/**/REALKIND*sw**4)
    f( 6) = (CI*eQED**4*gQCD*MB)/(4._/**/REALKIND*sw**4)
    f( 7) = (CI*countertermnorm*eQED**4*gQCD**3*MB)/(4._/**/REALKIND*sw**4)
    f( 8) = (CI*countertermnorm*ctGbb*eQED**4*gQCD**3*MB)/(4._/**/REALKIND*sw**4)
    f( 9) = (CI*countertermnorm*ctSbb*eQED**4*gQCD**3*MB)/(4._/**/REALKIND*sw**4)
    f(10) = (CI*cw*eQED**4*gQCD)/(2._/**/REALKIND*sw**3)
    f(11) = (CI*countertermnorm*cw*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**3)
    f(12) = (CI*countertermnorm*ctGbb*cw*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**3)
    f(13) = (CI*countertermnorm*ctVbb*cw*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**3)
    f(14) = (countertermnorm*ctZGG*cw*eQED**4*gQCD**3)/(sw**3*2._/**/REALKIND)
    f(15) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MW)/(2._/**/REALKIND*sw**3)
    f(16) = (CI*eQED**4*gQCD)/(6._/**/REALKIND*sw**2)
    f(17) = (CI*eQED**4*gQCD)/(2._/**/REALKIND*sw**2)
    f(18) = (CI*countertermnorm*eQED**4*gQCD**3)/(6._/**/REALKIND*sw**2)
    f(19) = (CI*countertermnorm*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(20) = (CI*countertermnorm*ctGbb*eQED**4*gQCD**3)/(6._/**/REALKIND*sw**2)
    f(21) = (CI*countertermnorm*ctGbb*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(22) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**3)/(6._/**/REALKIND*sw**2)
    f(23) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(24) = (CI*countertermnorm*ctWWGG*eQED**4*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(25) = (countertermnorm*ctZGG*eQED**4*gQCD**3)/(sw**2*2._/**/REALKIND)
    f(26) = (CI*eQED**4*gQCD**3*integralnorm*SwB)/(4._/**/REALKIND*sw**4)
    f(27) = (eQED**4*gQCD**3*integralnorm*SwB)/(sw**4*4._/**/REALKIND)
    f(28) = (CI*eQED**4*gQCD**3*integralnorm*MB*SwB)/(4._/**/REALKIND*sw**4)
    f(29) = (eQED**4*gQCD**3*integralnorm*MB*SwB)/(sw**4*4._/**/REALKIND)
    f(30) = (CI*cw*eQED**4*gQCD**3*integralnorm*SwB)/(2._/**/REALKIND*sw**3)
    f(31) = (cw*eQED**4*gQCD**3*integralnorm*SwB)/(sw**3*2._/**/REALKIND)
    f(32) = (CI*eQED**4*gQCD**3*integralnorm*SwB)/(6._/**/REALKIND*sw**2)
    f(33) = (CI*eQED**4*gQCD**3*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(34) = (eQED**4*gQCD**3*integralnorm*SwB)/(sw**2*6._/**/REALKIND)
    f(35) = (eQED**4*gQCD**3*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(36) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**4*4._/**/REALKIND)
    f(37) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**4*2._/**/REALKIND)
    f(38) = (eQED**4*gQCD**3*integralnorm*MB*SwF)/(sw**4*4._/**/REALKIND)
    f(39) = (eQED**4*gQCD**3*integralnorm*MT*SwF)/(sw**4*4._/**/REALKIND)
    f(40) = (cw*eQED**4*gQCD**3*integralnorm*SwF)/(sw**3*2._/**/REALKIND)
    f(41) = (cw*eQED**4*gQCD**3*integralnorm*SwF)/sw**3
    f(42) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*6._/**/REALKIND)
    f(43) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(44) = (eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(45) = (2*eQED**4*gQCD**3*integralnorm*SwF)/(sw**2*3._/**/REALKIND)
    f(46) = (eQED**4*gQCD**3*integralnorm*SwF)/sw**2

  c = [ 9*CI*f(26), f(27), 8*f(27), 9*CI*f(28), f(29), 8*f(29), 9*CI*f(30), f(31), 8*f(31), 9*CI*f(32), 9*CI*f(33), f(34), 8*f(34) &
    , f(35), 8*f(35), 3*f(36), 3*f(37), 3*f(38), 3*f(39), 3*f(40), 3*f(41), 3*f(42), 3*f(43), 3*f(44), 3*f(45), 3*f(46) ]
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
  complex(REALKIND) :: A(97)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_Q(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rMB, H(5), wf(:,-4))
  call wf_A(P(:,6), rMB, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_W(wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_W(wf(:,-3),wf(:,-1),wf(:,2))
  call vert_VQ_A(wf(:,-6),wf(:,-4),wf(:,3))
  call prop_W_W(wf(:,2),Q(:,10),MW,1_intkind1,wf(:,4))
  call prop_W_W(wf(:,1),Q(:,5),MW,1_intkind1,wf(:,5))
  call prop_Q_A(wf(:,3),Q(:,80),MB,1_intkind1,wf(:,6))
  call vert_AW_Q(wf(:,-5),wf(:,4),wf(:,7))
  call vert_WQ_A(wf(:,5),wf(:,6),wf(:,8))
  call prop_A_Q(wf(:,7),Q(:,42),MT,1_intkind1,wf(:,9))
  call vert_AQ_S(gH,wf(:,-5),wf(:,6),wf(:,10))
  call vert_VV_S(wf(:,5),wf(:,4),wf(:,11))
  call vert_QA_V(wf(:,6),wf(:,-5),wf(:,12))
  call vert_UV_W(wf(:,5),Q(:,5),wf(:,4),Q(:,10),wf(:,13))
  call vert_QA_Z(gZd,wf(:,6),wf(:,-5),wf(:,14))
  call prop_W_W(wf(:,14),Q(:,112),MZ,1_intkind1,wf(:,15))
  call vert_AV_Q(wf(:,-5),wf(:,-6),wf(:,16))
  call prop_A_Q(wf(:,16),Q(:,96),MB,1_intkind1,wf(:,17))
  call vert_WQ_A(wf(:,5),wf(:,-4),wf(:,18))
  call vert_AW_Q(wf(:,17),wf(:,4),wf(:,19))
  call prop_Q_A(wf(:,18),Q(:,21),MT,1_intkind1,wf(:,20))
  call vert_AQ_S(gH,wf(:,17),wf(:,-4),wf(:,21))
  call vert_QA_V(wf(:,-4),wf(:,17),wf(:,22))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,17),wf(:,23))
  call prop_W_W(wf(:,23),Q(:,112),MZ,1_intkind1,wf(:,24))
  call vert_VQ_A(wf(:,-6),wf(:,20),wf(:,25))
  call vert_AW_Q(wf(:,-1),wf(:,5),wf(:,26))
  call prop_A_Q(wf(:,26),Q(:,7),ZERO,0_intkind1,wf(:,27))
  call vert_QA_V(wf(:,-3),wf(:,27),wf(:,28))
  call vert_QA_Z(gZl,wf(:,-3),wf(:,27),wf(:,29))
  call vert_WQ_A(wf(:,5),wf(:,-3),wf(:,30))
  call prop_Q_A(wf(:,30),Q(:,13),ZERO,0_intkind1,wf(:,31))
  call vert_QA_Z(gZn,wf(:,31),wf(:,-1),wf(:,32))
  call vert_WQ_A(wf(:,4),wf(:,0),wf(:,33))
  call prop_Q_A(wf(:,33),Q(:,11),ZERO,0_intkind1,wf(:,34))
  call vert_QA_V(wf(:,34),wf(:,-2),wf(:,35))
  call vert_QA_Z(gZl,wf(:,34),wf(:,-2),wf(:,36))
  call vert_AW_Q(wf(:,-2),wf(:,4),wf(:,37))
  call prop_A_Q(wf(:,37),Q(:,14),ZERO,0_intkind1,wf(:,38))
  call vert_QA_Z(gZn,wf(:,0),wf(:,38),wf(:,39))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,40))
  call counter_VVG_G(wf(:,5),wf(:,4),wf(:,-6),wf(:,41))
  call counter_GG_S(wf(:,40),wf(:,-6),wf(:,42))
  call counter_GG_V(wf(:,40),Q(:,48),wf(:,-6),Q(:,64),wf(:,43))
  call prop_W_W(wf(:,13),Q(:,15),MZ,1_intkind1,wf(:,44))
  call counter_WQ_A(wf(:,5),wf(:,6),wf(:,45))
  call counter_AW_Q(wf(:,17),wf(:,4),wf(:,46))
  call counter_VQ_A(wf(:,-6),wf(:,20),wf(:,47))
  call counter_AQ_S(gH,wf(:,-5),wf(:,6),wf(:,48))
  call counter_QA_V(wf(:,6),wf(:,-5),wf(:,49))
  call counter_QA_Z(gZd,wf(:,6),wf(:,-5),wf(:,50))
  call counter_AW_Q(wf(:,-5),wf(:,4),wf(:,51))
  call prop_Q_A(wf(:,8),Q(:,85),MT,1_intkind1,wf(:,52))
  call prop_A_Q(wf(:,51),Q(:,42),MT,1_intkind1,wf(:,53))
  call counter_AV_Q(wf(:,-5),wf(:,-6),wf(:,54))
  call prop_A_Q(wf(:,54),Q(:,96),MB,1_intkind1,wf(:,55))
  call vert_AW_Q(wf(:,55),wf(:,4),wf(:,56))
  call vert_AQ_S(gH,wf(:,55),wf(:,-4),wf(:,57))
  call vert_QA_V(wf(:,-4),wf(:,55),wf(:,58))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,55),wf(:,59))
  call prop_W_W(wf(:,59),Q(:,112),MZ,1_intkind1,wf(:,60))
  call counter_AQ_S(gH,wf(:,17),wf(:,-4),wf(:,61))
  call counter_QA_V(wf(:,-4),wf(:,17),wf(:,62))
  call counter_QA_Z(gZd,wf(:,-4),wf(:,17),wf(:,63))
  call counter_WQ_A(wf(:,5),wf(:,-4),wf(:,64))
  call prop_A_Q(wf(:,19),Q(:,106),MT,1_intkind1,wf(:,65))
  call prop_Q_A(wf(:,64),Q(:,21),MT,1_intkind1,wf(:,66))
  call vert_VQ_A(wf(:,-6),wf(:,66),wf(:,67))
  call counter_VQ_A(wf(:,-6),wf(:,-4),wf(:,68))
  call prop_Q_A(wf(:,68),Q(:,80),MB,1_intkind1,wf(:,69))
  call vert_WQ_A(wf(:,5),wf(:,69),wf(:,70))
  call vert_AQ_S(gH,wf(:,-5),wf(:,69),wf(:,71))
  call vert_QA_V(wf(:,69),wf(:,-5),wf(:,72))
  call vert_QA_Z(gZd,wf(:,69),wf(:,-5),wf(:,73))
  call prop_W_W(wf(:,73),Q(:,112),MZ,1_intkind1,wf(:,74))
  call prop_W_W(wf(:,43),Q(:,112),MZ,1_intkind1,wf(:,75))
  call prop_W_W(wf(:,50),Q(:,112),MZ,1_intkind1,wf(:,76))
  call prop_W_W(wf(:,63),Q(:,112),MZ,1_intkind1,wf(:,77))
  call counter_Q_A(ctbb,wf(:,6),Q(:,80),wf(:,78))
  call prop_Q_A(wf(:,78),Q(:,80),MB,1_intkind1,wf(:,79))
  call vert_AQ_S(gH,wf(:,-5),wf(:,79),wf(:,80))
  call vert_QA_V(wf(:,79),wf(:,-5),wf(:,81))
  call vert_QA_Z(gZd,wf(:,79),wf(:,-5),wf(:,82))
  call vert_WQ_A(wf(:,5),wf(:,79),wf(:,83))
  call counter_A_Q(cttt,wf(:,9),Q(:,42),wf(:,84))
  call counter_A_Q(ctbb,wf(:,17),Q(:,96),wf(:,85))
  call prop_A_Q(wf(:,85),Q(:,96),MB,1_intkind1,wf(:,86))
  call vert_AQ_S(gH,wf(:,86),wf(:,-4),wf(:,87))
  call vert_QA_V(wf(:,-4),wf(:,86),wf(:,88))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,86),wf(:,89))
  call counter_Q_A(cttt,wf(:,20),Q(:,21),wf(:,90))
  call vert_AW_Q(wf(:,86),wf(:,4),wf(:,91))
  call vert_AV_Q(wf(:,9),wf(:,-6),wf(:,92))
  call prop_A_Q(wf(:,92),Q(:,106),MT,1_intkind1,wf(:,93))
  call prop_Q_A(wf(:,25),Q(:,85),MT,1_intkind1,wf(:,94))
  call prop_W_W(wf(:,29),Q(:,15),MZ,1_intkind1,wf(:,95))
  call prop_W_W(wf(:,32),Q(:,15),MZ,1_intkind1,wf(:,96))
  call prop_W_W(wf(:,36),Q(:,15),MZ,1_intkind1,wf(:,97))
  call prop_W_W(wf(:,39),Q(:,15),MZ,1_intkind1,wf(:,98))
  call vert_WQ_A(wf(:,4),wf(:,20),wf(:,99))
  call prop_Q_A(wf(:,99),Q(:,31),MB,1_intkind1,wf(:,100))
  call vert_AW_Q(wf(:,9),wf(:,5),wf(:,101))
  call prop_A_Q(wf(:,101),Q(:,47),MB,1_intkind1,wf(:,102))
  call vert_QS_A(gH,wf(:,-4),wf(:,11),wf(:,103))
  call prop_Q_A(wf(:,103),Q(:,31),MB,1_intkind1,wf(:,104))
  call vert_VQ_A(wf(:,13),wf(:,-4),wf(:,105))
  call prop_Q_A(wf(:,105),Q(:,31),MB,1_intkind1,wf(:,106))
  call vert_ZQ_A(gZd,wf(:,44),wf(:,-4),wf(:,107))
  call prop_Q_A(wf(:,107),Q(:,31),MB,1_intkind1,wf(:,108))
  call vert_SA_Q(gH,wf(:,11),wf(:,-5),wf(:,109))
  call prop_A_Q(wf(:,109),Q(:,47),MB,1_intkind1,wf(:,110))
  call vert_AV_Q(wf(:,-5),wf(:,13),wf(:,111))
  call prop_A_Q(wf(:,111),Q(:,47),MB,1_intkind1,wf(:,112))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,44),wf(:,113))
  call prop_A_Q(wf(:,113),Q(:,47),MB,1_intkind1,wf(:,114))
  call vert_VQ_A(wf(:,28),wf(:,-4),wf(:,115))
  call prop_Q_A(wf(:,115),Q(:,31),MB,1_intkind1,wf(:,116))
  call vert_ZQ_A(gZd,wf(:,95),wf(:,-4),wf(:,117))
  call prop_Q_A(wf(:,117),Q(:,31),MB,1_intkind1,wf(:,118))
  call vert_AV_Q(wf(:,-5),wf(:,28),wf(:,119))
  call prop_A_Q(wf(:,119),Q(:,47),MB,1_intkind1,wf(:,120))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,95),wf(:,121))
  call prop_A_Q(wf(:,121),Q(:,47),MB,1_intkind1,wf(:,122))
  call vert_ZQ_A(gZd,wf(:,96),wf(:,-4),wf(:,123))
  call prop_Q_A(wf(:,123),Q(:,31),MB,1_intkind1,wf(:,124))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,96),wf(:,125))
  call prop_A_Q(wf(:,125),Q(:,47),MB,1_intkind1,wf(:,126))
  call vert_VQ_A(wf(:,35),wf(:,-4),wf(:,127))
  call prop_Q_A(wf(:,127),Q(:,31),MB,1_intkind1,wf(:,128))
  call vert_ZQ_A(gZd,wf(:,97),wf(:,-4),wf(:,129))
  call prop_Q_A(wf(:,129),Q(:,31),MB,1_intkind1,wf(:,130))
  call vert_AV_Q(wf(:,-5),wf(:,35),wf(:,131))
  call prop_A_Q(wf(:,131),Q(:,47),MB,1_intkind1,wf(:,132))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,97),wf(:,133))
  call prop_A_Q(wf(:,133),Q(:,47),MB,1_intkind1,wf(:,134))
  call vert_ZQ_A(gZd,wf(:,98),wf(:,-4),wf(:,135))
  call prop_Q_A(wf(:,135),Q(:,31),MB,1_intkind1,wf(:,136))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,98),wf(:,137))
  call prop_A_Q(wf(:,137),Q(:,47),MB,1_intkind1,wf(:,138))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,10) - MW2)
  den(2) = 1 / (Q(5,5) - MW2)
  den(3) = 1 / (Q(5,80) - MB2)
  den(5) = 1 / (Q(5,42) - MT2)
  den(9) = 1 / (Q(5,112) - MH2)
  den(12) = 1 / (Q(5,112))
  den(15) = 1 / (Q(5,112) - MZ2)
  den(18) = 1 / (Q(5,96) - MB2)
  den(20) = 1 / (Q(5,21) - MT2)
  den(30) = 1 / (Q(5,7))
  den(34) = 1 / (Q(5,13))
  den(40) = 1 / (Q(5,11))
  den(44) = 1 / (Q(5,14))
  den(50) = 1 / (Q(5,48))
  den(52) = 1 / (Q(5,15) - MH2)
  den(55) = 1 / (Q(5,15) - MZ2)
  den(59) = 1 / (Q(5,15))
  den(63) = 1 / (Q(5,85) - MT2)
  den(69) = 1 / (Q(5,106) - MT2)
  den(114) = 1 / (Q(5,31) - MB2)
  den(117) = 1 / (Q(5,47) - MB2)

  ! denominators
  den(4) = den(2)*den(3)
  den(6) = den(1)*den(5)
  den(7) = den(4)*den(6)
  den(8) = den(1)*den(2)
  den(10) = den(3)*den(9)
  den(11) = den(8)*den(10)
  den(13) = den(3)*den(12)
  den(14) = den(8)*den(13)
  den(16) = den(3)*den(15)
  den(17) = den(8)*den(16)
  den(19) = den(1)*den(18)
  den(21) = den(2)*den(20)
  den(22) = den(19)*den(21)
  den(23) = den(9)*den(18)
  den(24) = den(8)*den(23)
  den(25) = den(12)*den(18)
  den(26) = den(8)*den(25)
  den(27) = den(15)*den(18)
  den(28) = den(8)*den(27)
  den(29) = den(6)*den(21)
  den(31) = den(2)*den(30)
  den(32) = den(13)*den(31)
  den(33) = den(16)*den(31)
  den(35) = den(2)*den(34)
  den(36) = den(16)*den(35)
  den(37) = den(25)*den(31)
  den(38) = den(27)*den(31)
  den(39) = den(27)*den(35)
  den(41) = den(1)*den(40)
  den(42) = den(13)*den(41)
  den(43) = den(16)*den(41)
  den(45) = den(1)*den(44)
  den(46) = den(16)*den(45)
  den(47) = den(25)*den(41)
  den(48) = den(27)*den(41)
  den(49) = den(27)*den(45)
  den(51) = den(8)*den(50)
  den(53) = den(8)*den(52)
  den(54) = den(50)*den(53)
  den(56) = den(8)*den(55)
  den(57) = den(50)*den(56)
  den(58) = den(3)*den(53)
  den(60) = den(8)*den(59)
  den(61) = den(3)*den(60)
  den(62) = den(3)*den(56)
  den(64) = den(4)*den(63)
  den(65) = den(1)*den(64)
  den(66) = den(18)*den(53)
  den(67) = den(18)*den(60)
  den(68) = den(18)*den(56)
  den(70) = den(19)*den(69)
  den(71) = den(2)*den(70)
  den(72) = den(15)*den(50)
  den(73) = den(31)*den(72)
  den(74) = den(35)*den(72)
  den(75) = den(41)*den(72)
  den(76) = den(45)*den(72)
  den(77) = den(3)**2
  den(78) = den(53)*den(77)
  den(79) = den(60)*den(77)
  den(80) = den(56)*den(77)
  den(81) = den(2)*den(77)
  den(82) = den(6)*den(81)
  den(83) = den(6)*den(64)
  den(84) = den(18)**2
  den(85) = den(53)*den(84)
  den(86) = den(60)*den(84)
  den(87) = den(56)*den(84)
  den(88) = den(21)*den(70)
  den(89) = den(1)*den(84)
  den(90) = den(21)*den(89)
  den(91) = den(6)*den(69)
  den(92) = den(21)*den(91)
  den(93) = den(21)*den(63)
  den(94) = den(6)*den(93)
  den(95) = den(31)*den(59)
  den(96) = den(77)*den(95)
  den(97) = den(31)*den(55)
  den(98) = den(77)*den(97)
  den(99) = den(35)*den(55)
  den(100) = den(77)*den(99)
  den(101) = den(84)*den(95)
  den(102) = den(84)*den(97)
  den(103) = den(84)*den(99)
  den(104) = den(41)*den(59)
  den(105) = den(77)*den(104)
  den(106) = den(41)*den(55)
  den(107) = den(77)*den(106)
  den(108) = den(45)*den(55)
  den(109) = den(77)*den(108)
  den(110) = den(84)*den(104)
  den(111) = den(84)*den(106)
  den(112) = den(84)*den(108)
  den(113) = den(1)*den(21)
  den(115) = den(113)*den(114)
  den(116) = den(2)*den(6)
  den(118) = den(116)*den(117)
  den(119) = den(53)*den(114)
  den(120) = den(60)*den(114)
  den(121) = den(56)*den(114)
  den(122) = den(53)*den(117)
  den(123) = den(60)*den(117)
  den(124) = den(56)*den(117)
  den(125) = den(95)*den(114)
  den(126) = den(97)*den(114)
  den(127) = den(95)*den(117)
  den(128) = den(97)*den(117)
  den(129) = den(99)*den(114)
  den(130) = den(99)*den(117)
  den(131) = den(104)*den(114)
  den(132) = den(106)*den(114)
  den(133) = den(104)*den(117)
  den(134) = den(106)*den(117)
  den(135) = den(108)*den(114)
  den(136) = den(108)*den(117)
  den(137) = den(50)*den(60)
  den(138) = den(1)*den(2)*den(50)
  den(139) = den(2)*den(3)*den(6)
  den(140) = den(1)*den(2)*den(3)
  den(141) = den(1)*den(18)*den(21)
  den(142) = den(1)*den(2)*den(18)
  den(143) = den(1)*den(93)
  den(144) = den(2)*den(91)
  den(145) = den(50)*den(95)
  den(146) = den(50)*den(97)
  den(147) = den(50)*den(99)
  den(148) = den(3)*den(95)
  den(149) = den(3)*den(97)
  den(150) = den(3)*den(99)
  den(151) = den(18)*den(95)
  den(152) = den(18)*den(97)
  den(153) = den(18)*den(99)
  den(154) = den(50)*den(104)
  den(155) = den(50)*den(106)
  den(156) = den(50)*den(108)
  den(157) = den(3)*den(104)
  den(158) = den(3)*den(106)
  den(159) = den(3)*den(108)
  den(160) = den(18)*den(104)
  den(161) = den(18)*den(106)
  den(162) = den(18)*den(108)
  den(163) = den(3)*den(118)
  den(164) = den(3)*den(122)
  den(165) = den(3)*den(123)
  den(166) = den(3)*den(124)
  den(167) = den(18)*den(115)
  den(168) = den(18)*den(119)
  den(169) = den(18)*den(120)
  den(170) = den(18)*den(121)
  den(171) = den(3)*den(127)
  den(172) = den(3)*den(128)
  den(173) = den(3)*den(130)
  den(174) = den(18)*den(125)
  den(175) = den(18)*den(126)
  den(176) = den(18)*den(129)
  den(177) = den(3)*den(133)
  den(178) = den(3)*den(134)
  den(179) = den(3)*den(136)
  den(180) = den(18)*den(131)
  den(181) = den(18)*den(132)
  den(182) = den(18)*den(135)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(97)

  A(1) = cont_QA(wf(:,8),wf(:,9)) * den(7)
  A(2) = cont_SS(wf(:,10),wf(:,11)) * den(11)
  A(3) = cont_VV(wf(:,12),wf(:,13)) * den(14)
  A(4) = cont_VV(wf(:,13),wf(:,15)) * den(17)
  A(5) = cont_QA(wf(:,19),wf(:,20)) * den(22)
  A(6) = cont_SS(wf(:,11),wf(:,21)) * den(24)
  A(7) = cont_VV(wf(:,13),wf(:,22)) * den(26)
  A(8) = cont_VV(wf(:,13),wf(:,24)) * den(28)
  A(9) = cont_QA(wf(:,9),wf(:,25)) * den(29)
  A(10) = cont_VV(wf(:,12),wf(:,28)) * den(32)
  A(11) = cont_VV(wf(:,15),wf(:,29)) * den(33)
  A(12) = cont_VV(wf(:,15),wf(:,32)) * den(36)
  A(13) = cont_VV(wf(:,22),wf(:,28)) * den(37)
  A(14) = cont_VV(wf(:,24),wf(:,29)) * den(38)
  A(15) = cont_VV(wf(:,24),wf(:,32)) * den(39)
  A(16) = cont_VV(wf(:,12),wf(:,35)) * den(42)
  A(17) = cont_VV(wf(:,15),wf(:,36)) * den(43)
  A(18) = cont_VV(wf(:,15),wf(:,39)) * den(46)
  A(19) = cont_VV(wf(:,22),wf(:,35)) * den(47)
  A(20) = cont_VV(wf(:,24),wf(:,36)) * den(48)
  A(21) = cont_VV(wf(:,24),wf(:,39)) * den(49)

  A(22) = cont_VV(wf(:,40),wf(:,41)) * den(51)
  A(23) = cont_SS(wf(:,11),wf(:,42)) * den(54)
  A(24) = cont_VV(wf(:,43),wf(:,44)) * den(57)
  A(25) = cont_QA(wf(:,9),wf(:,45)) * den(7)
  A(26) = cont_QA(wf(:,20),wf(:,46)) * den(22)
  A(27) = cont_QA(wf(:,9),wf(:,47)) * den(29)
  A(28) = cont_SS(wf(:,11),wf(:,48)) * den(58)
  A(29) = cont_VV(wf(:,13),wf(:,49)) * den(61)
  A(30) = cont_VV(wf(:,44),wf(:,50)) * den(62)
  A(31) = cont_QA(wf(:,51),wf(:,52)) * den(65)
  A(32) = cont_QA(wf(:,25),wf(:,53)) * den(29)
  A(33) = cont_QA(wf(:,20),wf(:,56)) * den(22)
  A(34) = cont_SS(wf(:,11),wf(:,57)) * den(24)
  A(35) = cont_VV(wf(:,13),wf(:,58)) * den(26)
  A(36) = cont_VV(wf(:,13),wf(:,60)) * den(28)
  A(37) = cont_SS(wf(:,11),wf(:,61)) * den(66)
  A(38) = cont_VV(wf(:,13),wf(:,62)) * den(67)
  A(39) = cont_VV(wf(:,44),wf(:,63)) * den(68)
  A(40) = cont_QA(wf(:,64),wf(:,65)) * den(71)
  A(41) = cont_QA(wf(:,9),wf(:,67)) * den(29)
  A(42) = cont_QA(wf(:,9),wf(:,70)) * den(7)
  A(43) = cont_SS(wf(:,11),wf(:,71)) * den(11)
  A(44) = cont_VV(wf(:,13),wf(:,72)) * den(14)
  A(45) = cont_VV(wf(:,13),wf(:,74)) * den(17)
  A(46) = cont_VV(wf(:,29),wf(:,75)) * den(73)
  A(47) = cont_VV(wf(:,32),wf(:,75)) * den(74)
  A(48) = cont_VV(wf(:,28),wf(:,49)) * den(32)
  A(49) = cont_VV(wf(:,29),wf(:,76)) * den(33)
  A(50) = cont_VV(wf(:,32),wf(:,76)) * den(36)
  A(51) = cont_VV(wf(:,28),wf(:,58)) * den(37)
  A(52) = cont_VV(wf(:,29),wf(:,60)) * den(38)
  A(53) = cont_VV(wf(:,32),wf(:,60)) * den(39)
  A(54) = cont_VV(wf(:,28),wf(:,62)) * den(37)
  A(55) = cont_VV(wf(:,29),wf(:,77)) * den(38)
  A(56) = cont_VV(wf(:,32),wf(:,77)) * den(39)
  A(57) = cont_VV(wf(:,28),wf(:,72)) * den(32)
  A(58) = cont_VV(wf(:,29),wf(:,74)) * den(33)
  A(59) = cont_VV(wf(:,32),wf(:,74)) * den(36)
  A(60) = cont_VV(wf(:,36),wf(:,75)) * den(75)
  A(61) = cont_VV(wf(:,39),wf(:,75)) * den(76)
  A(62) = cont_VV(wf(:,35),wf(:,49)) * den(42)
  A(63) = cont_VV(wf(:,36),wf(:,76)) * den(43)
  A(64) = cont_VV(wf(:,39),wf(:,76)) * den(46)
  A(65) = cont_VV(wf(:,35),wf(:,58)) * den(47)
  A(66) = cont_VV(wf(:,36),wf(:,60)) * den(48)
  A(67) = cont_VV(wf(:,39),wf(:,60)) * den(49)
  A(68) = cont_VV(wf(:,35),wf(:,62)) * den(47)
  A(69) = cont_VV(wf(:,36),wf(:,77)) * den(48)
  A(70) = cont_VV(wf(:,39),wf(:,77)) * den(49)
  A(71) = cont_VV(wf(:,35),wf(:,72)) * den(42)
  A(72) = cont_VV(wf(:,36),wf(:,74)) * den(43)
  A(73) = cont_VV(wf(:,39),wf(:,74)) * den(46)
  A(74) = cont_SS(wf(:,11),wf(:,80)) * den(78)
  A(75) = cont_VV(wf(:,13),wf(:,81)) * den(79)
  A(76) = cont_VV(wf(:,44),wf(:,82)) * den(80)
  A(77) = cont_QA(wf(:,9),wf(:,83)) * den(82)
  A(78) = cont_QA(wf(:,52),wf(:,84)) * den(83)
  A(79) = cont_SS(wf(:,11),wf(:,87)) * den(85)
  A(80) = cont_VV(wf(:,13),wf(:,88)) * den(86)
  A(81) = cont_VV(wf(:,44),wf(:,89)) * den(87)
  A(82) = cont_QA(wf(:,65),wf(:,90)) * den(88)
  A(83) = cont_QA(wf(:,20),wf(:,91)) * den(90)
  A(84) = cont_QA(wf(:,90),wf(:,93)) * den(92)
  A(85) = cont_QA(wf(:,84),wf(:,94)) * den(94)
  A(86) = cont_VV(wf(:,28),wf(:,81)) * den(96)
  A(87) = cont_VV(wf(:,82),wf(:,95)) * den(98)
  A(88) = cont_VV(wf(:,82),wf(:,96)) * den(100)
  A(89) = cont_VV(wf(:,28),wf(:,88)) * den(101)
  A(90) = cont_VV(wf(:,89),wf(:,95)) * den(102)
  A(91) = cont_VV(wf(:,89),wf(:,96)) * den(103)
  A(92) = cont_VV(wf(:,35),wf(:,81)) * den(105)
  A(93) = cont_VV(wf(:,82),wf(:,97)) * den(107)
  A(94) = cont_VV(wf(:,82),wf(:,98)) * den(109)
  A(95) = cont_VV(wf(:,35),wf(:,88)) * den(110)
  A(96) = cont_VV(wf(:,89),wf(:,97)) * den(111)
  A(97) = cont_VV(wf(:,89),wf(:,98)) * den(112)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(97)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(1)+A(5)+A(9))*f(1)+(-A(2)-A(6))*f(6)+(A(4)+A(8))*f(10)+(-A(3)-A(7)+A(10)+A(13)+A(16)+A(19))*f(16)+(A(11)+A(12)+A(14) &
       +A(15)+A(17)+A(18)+A(20)+A(21))*f(17)

  M2(1) = (-A(77)-A(78)-A(82)-A(83)-A(84)-A(85))*f(2)+(A(33)+A(42))*f(3)+A(27)*f(4)+(A(25)+A(26)+A(31)+A(32)+A(40)+A(41))*f(5) &
       +(A(74)+A(79))*f(7)+(-A(34)-A(43))*f(8)+(-A(28)-A(37))*f(9)+(-A(76)-A(81))*f(11)+(A(36)+A(45))*f(12)+(A(30)+A(39))*f(13) &
       -A(24)*f(14)-A(23)*f(15)+(A(75)+A(80)-A(86)-A(89)-A(92)-A(95))*f(18)+(-A(87)-A(88)-A(90)-A(91)-A(93)-A(94)-A(96) &
       -A(97))*f(19)+(-A(35)-A(44)+A(51)+A(57)+A(65)+A(71))*f(20)+(A(52)+A(53)+A(58)+A(59)+A(66)+A(67)+A(72)+A(73))*f(21)+(-A(29) &
       -A(38)+A(48)+A(54)+A(62)+A(68))*f(22)+(A(49)+A(50)+A(55)+A(56)+A(63)+A(64)+A(69)+A(70))*f(23)+A(22)*f(24)+(-A(46)-A(47) &
       -A(60)-A(61))*f(25)

end subroutine colourvectors

end module ol_loop_ppllllj_nenmxexmbbxg_1_/**/REALKIND
