
module ol_colourmatrix_ppthjj_utxdxbhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(46,2), K2(2,2), KL(2,4)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  12,   0]
  K1( 2,:) = [   0,  12]
  K1( 3,:) = [  16,   0]
  K1( 4,:) = [   0,  16]
  K1( 5,:) = [   0,  -2]
  K1( 6,:) = [  -2,   0]
  K1( 7,:) = [  16,   0]
  K1( 8,:) = [   0,  16]
  K1( 9,:) = [ -16,   0]
  K1(10,:) = [   0,   2]
  K1(11,:) = [   0,   2]
  K1(12,:) = [   2,   0]
  K1(13,:) = [  16,   0]
  K1(14,:) = [   0,  16]
  K1(15,:) = [   0,   2]
  K1(16,:) = [   2,   0]
  K1(17,:) = [   2,   0]
  K1(18,:) = [   0, -16]
  K1(19,:) = [   0,  -2]
  K1(20,:) = [  -2,   0]
  K1(21,:) = [  16,   0]
  K1(22,:) = [   0,  16]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   0,   0]
  K1(28,:) = [   0,   0]
  K1(29,:) = [   0,   0]
  K1(30,:) = [   0,   0]
  K1(31,:) = [   0,   0]
  K1(32,:) = [   0,   0]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0, -18]
  K1(35,:) = [ -18,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0, -18]
  K1(39,:) = [ -18,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [   0,   0]
  K1(42,:) = [   0,   0]
  K1(43,:) = [  36,   0]
  K1(44,:) = [   0,  36]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]

  K2(1,:) = [ 12,  0]
  K2(2,:) = [  0, 12]

  KL(1,:) = [  4, 12,  0,  4]
  KL(2,:) = [  4,  0, 12,  4]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppthjj_utxdxbhg_1_/**/REALKIND



module ol_forced_parameters_ppthjj_utxdxbhg_1_/**/REALKIND
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
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (YU /= 0) write(*,101) 'YU = 0'
  if (YD /= 0) write(*,101) 'YD = 0'
  if (YS /= 0) write(*,101) 'YS = 0'
  if (YC /= 0) write(*,101) 'YC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppthjj_utxdxbhg_1_/**/REALKIND

module ol_loop_ppthjj_utxdxbhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(45), c(22)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:212)
  ! denominators
  complex(REALKIND), save :: den(169)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,32)
  ! zero helicity identifier
  logical,           save :: zerohel(32) = .true., zerohel_ct(32) = .true.

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
    f( 1) = (CI*countertermnorm*ctSqq*eQED**3*gQCD**3*MH**2)/(4._/**/REALKIND*MW**3*sw**3)
    f( 2) = (CI*eQED**3*gQCD)/(4._/**/REALKIND*MW*sw**3)
    f( 3) = (CI*countertermnorm*eQED**3*gQCD**3)/(4._/**/REALKIND*MW*sw**3)
    f( 4) = (CI*countertermnorm*ctGbb*eQED**3*gQCD**3)/(4._/**/REALKIND*MW*sw**3)
    f( 5) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**3)/(4._/**/REALKIND*MW*sw**3)
    f( 6) = (CI*countertermnorm*ctGtt*eQED**3*gQCD**3)/(4._/**/REALKIND*MW*sw**3)
    f( 7) = (CI*countertermnorm*ctSqq*eQED**3*gQCD**3)/(4._/**/REALKIND*MW*sw**3)
    f( 8) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**3)/(4._/**/REALKIND*MW*sw**3)
    f( 9) = (CI*eQED**3*gQCD*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f(10) = (CI*countertermnorm*eQED**3*gQCD**3*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f(11) = (CI*countertermnorm*ctGbb*eQED**3*gQCD**3*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f(12) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**3*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f(13) = (CI*countertermnorm*ctGtt*eQED**3*gQCD**3*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f(14) = (CI*countertermnorm*ctVbt*eQED**3*gQCD**3*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f(15) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**3*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f(16) = (CI*eQED**3*gQCD**3*integralnorm*SwB)/(4._/**/REALKIND*MW*sw**3)
    f(17) = (eQED**3*gQCD**3*integralnorm*SwB)/(MW*sw**3*4._/**/REALKIND)
    f(18) = (CI*eQED**3*gQCD**3*integralnorm*lambdaHWW*MW*SwB)/(2._/**/REALKIND*sw**3)
    f(19) = (eQED**3*gQCD**3*integralnorm*lambdaHWW*MW*SwB)/(sw**3*2._/**/REALKIND)
    f(20) = (CI*countertermnorm*ctSqq*eQED**3*gQCD**3*YB)/(4._/**/REALKIND*MW**3*sw**3)
    f(21) = (CI*eQED**3*gQCD*YB)/(4._/**/REALKIND*MW*sw**3)
    f(22) = (CI*countertermnorm*eQED**3*gQCD**3*YB)/(4._/**/REALKIND*MW*sw**3)
    f(23) = (CI*countertermnorm*ctGbb*eQED**3*gQCD**3*YB)/(4._/**/REALKIND*MW*sw**3)
    f(24) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**3*YB)/(4._/**/REALKIND*MW*sw**3)
    f(25) = (CI*countertermnorm*ctGtt*eQED**3*gQCD**3*YB)/(4._/**/REALKIND*MW*sw**3)
    f(26) = (CI*countertermnorm*ctSbb*eQED**3*gQCD**3*YB)/(4._/**/REALKIND*MW*sw**3)
    f(27) = (CI*countertermnorm*ctVbt*eQED**3*gQCD**3*YB)/(4._/**/REALKIND*MW*sw**3)
    f(28) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**3*YB)/(4._/**/REALKIND*MW*sw**3)
    f(29) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**3*MB*YB)/(2._/**/REALKIND*MQ2sum*sw**2)
    f(30) = (CI*eQED**3*gQCD**3*integralnorm*SwB*YB)/(4._/**/REALKIND*MW*sw**3)
    f(31) = (eQED**3*gQCD**3*integralnorm*SwB*YB)/(MW*sw**3*4._/**/REALKIND)
    f(32) = (eQED**3*gQCD**3*integralnorm*SwF*YB)/(MW*sw**3*4._/**/REALKIND)
    f(33) = (CI*countertermnorm*ctSqq*eQED**3*gQCD**3*YT)/(4._/**/REALKIND*MW**3*sw**3)
    f(34) = (CI*eQED**3*gQCD*YT)/(4._/**/REALKIND*MW*sw**3)
    f(35) = (CI*countertermnorm*eQED**3*gQCD**3*YT)/(4._/**/REALKIND*MW*sw**3)
    f(36) = (CI*countertermnorm*ctGbb*eQED**3*gQCD**3*YT)/(4._/**/REALKIND*MW*sw**3)
    f(37) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**3*YT)/(4._/**/REALKIND*MW*sw**3)
    f(38) = (CI*countertermnorm*ctGtt*eQED**3*gQCD**3*YT)/(4._/**/REALKIND*MW*sw**3)
    f(39) = (CI*countertermnorm*ctStt*eQED**3*gQCD**3*YT)/(4._/**/REALKIND*MW*sw**3)
    f(40) = (CI*countertermnorm*ctVbt*eQED**3*gQCD**3*YT)/(4._/**/REALKIND*MW*sw**3)
    f(41) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**3*YT)/(4._/**/REALKIND*MW*sw**3)
    f(42) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**3*MT*YT)/(2._/**/REALKIND*MQ2sum*sw**2)
    f(43) = (CI*eQED**3*gQCD**3*integralnorm*SwB*YT)/(4._/**/REALKIND*MW*sw**3)
    f(44) = (eQED**3*gQCD**3*integralnorm*SwB*YT)/(MW*sw**3*4._/**/REALKIND)
    f(45) = (eQED**3*gQCD**3*integralnorm*SwF*YT)/(MW*sw**3*4._/**/REALKIND)

  c = [ 3*CI*f(16), 9*CI*f(16), f(17), 3*f(17), 8*f(17), 3*CI*f(18), 9*CI*f(18), f(19), 3*f(19), 8*f(19), 3*CI*f(30), 9*CI*f(30) &
    , f(31), 3*f(31), 8*f(31), 3*f(32), 3*CI*f(43), 9*CI*f(43), f(44), 3*f(44), 8*f(44), 3*f(45) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(136)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rMT, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_Q(P(:,4), rMB, H(4), wf(:,-3))
  call wf_S(P(:,5), rMH, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_W(wf(:,0),wf(:,-2),wf(:,1))
  call vert_SA_Q(gH,wf(:,-4),wf(:,-1),wf(:,2))
  call vert_VQ_A(wf(:,-5),wf(:,-3),wf(:,3))
  call prop_A_Q(wf(:,2),Q(:,18),MT,1_intkind1,wf(:,4))
  call prop_Q_A(wf(:,3),Q(:,40),MB,1_intkind1,wf(:,5))
  call vert_AW_Q(wf(:,4),wf(:,1),wf(:,6))
  call vert_WQ_A(wf(:,1),wf(:,-3),wf(:,7))
  call vert_AV_Q(wf(:,4),wf(:,-5),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,13),MT,1_intkind1,wf(:,9))
  call vert_AV_Q(wf(:,-1),wf(:,-5),wf(:,10))
  call vert_QS_A(gH,wf(:,-3),wf(:,-4),wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,34),MT,1_intkind1,wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,24),MB,1_intkind1,wf(:,13))
  call vert_AW_Q(wf(:,12),wf(:,1),wf(:,14))
  call vert_AW_Q(wf(:,-1),wf(:,1),wf(:,15))
  call vert_VQ_A(wf(:,-5),wf(:,13),wf(:,16))
  call prop_A_Q(wf(:,15),Q(:,7),MB,1_intkind1,wf(:,17))
  call vert_SA_Q(gH,wf(:,-4),wf(:,12),wf(:,18))
  call vert_AQ_S(gPtb,wf(:,12),wf(:,-3),wf(:,19))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,1),Q(:,5),wf(:,20))
  call vert_QA_W(wf(:,-3),wf(:,12),wf(:,21))
  call vert_SV_V(wf(:,-4),wf(:,1),wf(:,22))
  call vert_QS_A(gH,wf(:,5),wf(:,-4),wf(:,23))
  call vert_AQ_S(gPtb,wf(:,-1),wf(:,5),wf(:,24))
  call vert_QA_W(wf(:,5),wf(:,-1),wf(:,25))
  call vert_VQ_A(wf(:,-5),wf(:,0),wf(:,26))
  call vert_AQ_S(gPtb,wf(:,-1),wf(:,-3),wf(:,27))
  call prop_Q_A(wf(:,26),Q(:,33),ZERO,0_intkind1,wf(:,28))
  call vert_QA_W(wf(:,28),wf(:,-2),wf(:,29))
  call vert_ST_V(wf(:,27),Q(:,10),wf(:,-4),Q(:,16),wf(:,30))
  call vert_QA_W(wf(:,-3),wf(:,-1),wf(:,31))
  call vert_SV_V(wf(:,-4),wf(:,31),wf(:,32))
  call vert_AV_Q(wf(:,-2),wf(:,-5),wf(:,33))
  call prop_A_Q(wf(:,33),Q(:,36),ZERO,0_intkind1,wf(:,34))
  call vert_QA_W(wf(:,0),wf(:,34),wf(:,35))
  call vert_QA_W(wf(:,-3),wf(:,4),wf(:,36))
  call vert_QA_W(wf(:,13),wf(:,-1),wf(:,37))
  call counter_AW_Q(wf(:,4),wf(:,1),wf(:,38))
  call counter_AV_Q(wf(:,4),wf(:,-5),wf(:,39))
  call counter_AW_Q(wf(:,12),wf(:,1),wf(:,40))
  call counter_VQ_A(wf(:,-5),wf(:,13),wf(:,41))
  call counter_SA_Q(gH,wf(:,-4),wf(:,12),wf(:,42))
  call counter_QS_A(gH,wf(:,5),wf(:,-4),wf(:,43))
  call counter_SG_G(wf(:,-4),wf(:,-5),wf(:,44))
  call vert_VQ_A(wf(:,44),wf(:,-3),wf(:,45))
  call vert_AV_Q(wf(:,-1),wf(:,44),wf(:,46))
  call prop_A_Q(wf(:,46),Q(:,50),MT,1_intkind1,wf(:,47))
  call counter_WQ_A(wf(:,1),wf(:,-3),wf(:,48))
  call prop_A_Q(wf(:,8),Q(:,50),MT,1_intkind1,wf(:,49))
  call counter_VQ_A(wf(:,-5),wf(:,-3),wf(:,50))
  call prop_Q_A(wf(:,50),Q(:,40),MB,1_intkind1,wf(:,51))
  call counter_AQ_S(ctStb,wf(:,12),wf(:,-3),wf(:,52))
  call counter_QA_W(wf(:,-3),wf(:,12),wf(:,53))
  call prop_A_Q(wf(:,18),Q(:,50),MT,1_intkind1,wf(:,54))
  call vert_QS_A(gH,wf(:,51),wf(:,-4),wf(:,55))
  call vert_AQ_S(gPtb,wf(:,-1),wf(:,51),wf(:,56))
  call vert_QA_W(wf(:,51),wf(:,-1),wf(:,57))
  call counter_QS_A(gH,wf(:,-3),wf(:,-4),wf(:,58))
  call prop_Q_A(wf(:,58),Q(:,24),MB,1_intkind1,wf(:,59))
  call vert_VQ_A(wf(:,-5),wf(:,59),wf(:,60))
  call vert_WQ_A(wf(:,31),wf(:,0),wf(:,61))
  call vert_AV_Q(wf(:,-2),wf(:,44),wf(:,62))
  call prop_Q_A(wf(:,61),Q(:,11),ZERO,0_intkind1,wf(:,63))
  call vert_VQ_A(wf(:,44),wf(:,0),wf(:,64))
  call vert_AW_Q(wf(:,-2),wf(:,31),wf(:,65))
  call prop_Q_A(wf(:,64),Q(:,49),ZERO,0_intkind1,wf(:,66))
  call counter_QA_W(wf(:,-3),wf(:,4),wf(:,67))
  call vert_QA_W(wf(:,59),wf(:,-1),wf(:,68))
  call vert_SS_S(wf(:,27),wf(:,-4),wf(:,69))
  call counter_AQ_S(gPdu,wf(:,-2),wf(:,28),wf(:,70))
  call counter_QA_W(wf(:,28),wf(:,-2),wf(:,71))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,31),Q(:,10),wf(:,72))
  call counter_AV_Q(wf(:,-2),wf(:,-5),wf(:,73))
  call prop_A_Q(wf(:,73),Q(:,36),ZERO,0_intkind1,wf(:,74))
  call vert_QA_W(wf(:,0),wf(:,74),wf(:,75))
  call vert_AQ_S(gPtb,wf(:,4),wf(:,-3),wf(:,76))
  call vert_AQ_S(gPtb,wf(:,-1),wf(:,13),wf(:,77))
  call counter_AW_Q(wf(:,-1),wf(:,1),wf(:,78))
  call prop_Q_A(wf(:,16),Q(:,56),MB,1_intkind1,wf(:,79))
  call counter_AV_Q(wf(:,-1),wf(:,-5),wf(:,80))
  call prop_A_Q(wf(:,80),Q(:,34),MT,1_intkind1,wf(:,81))
  call vert_AW_Q(wf(:,81),wf(:,1),wf(:,82))
  call counter_AQ_S(ctStb,wf(:,-1),wf(:,5),wf(:,83))
  call counter_QA_W(wf(:,5),wf(:,-1),wf(:,84))
  call prop_Q_A(wf(:,23),Q(:,56),MB,1_intkind1,wf(:,85))
  call vert_SA_Q(gH,wf(:,-4),wf(:,81),wf(:,86))
  call vert_AQ_S(gPtb,wf(:,81),wf(:,-3),wf(:,87))
  call vert_QA_W(wf(:,-3),wf(:,81),wf(:,88))
  call counter_SA_Q(gH,wf(:,-4),wf(:,-1),wf(:,89))
  call prop_A_Q(wf(:,89),Q(:,18),MT,1_intkind1,wf(:,90))
  call vert_AW_Q(wf(:,90),wf(:,1),wf(:,91))
  call vert_AV_Q(wf(:,90),wf(:,-5),wf(:,92))
  call counter_QA_W(wf(:,13),wf(:,-1),wf(:,93))
  call vert_QA_W(wf(:,-3),wf(:,90),wf(:,94))
  call counter_AQ_S(ctStb,wf(:,-1),wf(:,-3),wf(:,95))
  call vert_ST_V(wf(:,95),Q(:,10),wf(:,-4),Q(:,16),wf(:,96))
  call counter_QA_W(wf(:,-3),wf(:,-1),wf(:,97))
  call vert_SV_V(wf(:,-4),wf(:,97),wf(:,98))
  call counter_AQ_S(gPdu,wf(:,34),wf(:,0),wf(:,99))
  call counter_QA_W(wf(:,0),wf(:,34),wf(:,100))
  call counter_VQ_A(wf(:,-5),wf(:,0),wf(:,101))
  call prop_Q_A(wf(:,101),Q(:,33),ZERO,0_intkind1,wf(:,102))
  call vert_QA_W(wf(:,102),wf(:,-2),wf(:,103))
  call counter_AQ_S(gPdu,wf(:,-2),wf(:,0),wf(:,104))
  call vert_SA_Q(gPtb,wf(:,104),wf(:,4),wf(:,105))
  call counter_QA_W(wf(:,0),wf(:,-2),wf(:,106))
  call vert_AW_Q(wf(:,4),wf(:,106),wf(:,107))
  call vert_QS_A(gPtb,wf(:,-3),wf(:,104),wf(:,108))
  call prop_Q_A(wf(:,108),Q(:,13),MT,1_intkind1,wf(:,109))
  call vert_WQ_A(wf(:,106),wf(:,-3),wf(:,110))
  call prop_Q_A(wf(:,110),Q(:,13),MT,1_intkind1,wf(:,111))
  call vert_SA_Q(gPtb,wf(:,104),wf(:,12),wf(:,112))
  call vert_AW_Q(wf(:,12),wf(:,106),wf(:,113))
  call vert_SA_Q(gPtb,wf(:,104),wf(:,-1),wf(:,114))
  call prop_A_Q(wf(:,114),Q(:,7),MB,1_intkind1,wf(:,115))
  call vert_AW_Q(wf(:,-1),wf(:,106),wf(:,116))
  call prop_A_Q(wf(:,116),Q(:,7),MB,1_intkind1,wf(:,117))
  call vert_SS_S(wf(:,104),wf(:,-4),wf(:,118))
  call vert_ST_V(wf(:,104),Q(:,5),wf(:,-4),Q(:,16),wf(:,119))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,106),Q(:,5),wf(:,120))
  call vert_SV_V(wf(:,-4),wf(:,106),wf(:,121))
  call vert_WQ_A(wf(:,1),wf(:,5),wf(:,122))
  call counter_A_Q(cttt,wf(:,4),Q(:,18),wf(:,123))
  call prop_Q_A(wf(:,122),Q(:,45),MT,1_intkind1,wf(:,124))
  call counter_Q_A(ctbb,wf(:,5),Q(:,40),wf(:,125))
  call prop_A_Q(wf(:,6),Q(:,23),MB,1_intkind1,wf(:,126))
  call prop_A_Q(wf(:,123),Q(:,18),MT,1_intkind1,wf(:,127))
  call vert_AV_Q(wf(:,127),wf(:,-5),wf(:,128))
  call counter_Q_A(cttt,wf(:,9),Q(:,13),wf(:,129))
  call vert_WQ_A(wf(:,1),wf(:,13),wf(:,130))
  call counter_A_Q(cttt,wf(:,12),Q(:,34),wf(:,131))
  call prop_Q_A(wf(:,130),Q(:,29),MT,1_intkind1,wf(:,132))
  call counter_Q_A(ctbb,wf(:,13),Q(:,24),wf(:,133))
  call prop_A_Q(wf(:,14),Q(:,39),MB,1_intkind1,wf(:,134))
  call counter_A_Q(ctbb,wf(:,17),Q(:,7),wf(:,135))
  call prop_Q_A(wf(:,133),Q(:,24),MB,1_intkind1,wf(:,136))
  call vert_VQ_A(wf(:,-5),wf(:,136),wf(:,137))
  call prop_A_Q(wf(:,131),Q(:,34),MT,1_intkind1,wf(:,138))
  call vert_AQ_S(gPtb,wf(:,138),wf(:,-3),wf(:,139))
  call vert_QA_W(wf(:,-3),wf(:,138),wf(:,140))
  call vert_SA_Q(gH,wf(:,-4),wf(:,138),wf(:,141))
  call prop_Q_A(wf(:,125),Q(:,40),MB,1_intkind1,wf(:,142))
  call vert_AQ_S(gPtb,wf(:,-1),wf(:,142),wf(:,143))
  call vert_QA_W(wf(:,142),wf(:,-1),wf(:,144))
  call vert_QS_A(gH,wf(:,142),wf(:,-4),wf(:,145))
  call counter_Q_A(ctqq,wf(:,28),Q(:,33),wf(:,146))
  call prop_Q_A(wf(:,146),Q(:,33),ZERO,0_intkind1,wf(:,147))
  call vert_QA_W(wf(:,147),wf(:,-2),wf(:,148))
  call counter_A_Q(ctqq,wf(:,34),Q(:,36),wf(:,149))
  call prop_A_Q(wf(:,149),Q(:,36),ZERO,0_intkind1,wf(:,150))
  call vert_QA_W(wf(:,0),wf(:,150),wf(:,151))
  call vert_QA_W(wf(:,-3),wf(:,127),wf(:,152))
  call vert_QA_W(wf(:,136),wf(:,-1),wf(:,153))
  call vert_QA_V(wf(:,-3),wf(:,17),wf(:,154))
  call vert_SA_Q(gH,wf(:,-4),wf(:,17),wf(:,155))
  call prop_A_Q(wf(:,155),Q(:,23),MB,1_intkind1,wf(:,156))
  call vert_AV_Q(wf(:,17),wf(:,-5),wf(:,157))
  call prop_A_Q(wf(:,157),Q(:,39),MB,1_intkind1,wf(:,158))
  call vert_QA_V(wf(:,9),wf(:,-1),wf(:,159))
  call vert_QS_A(gH,wf(:,9),wf(:,-4),wf(:,160))
  call prop_Q_A(wf(:,160),Q(:,29),MT,1_intkind1,wf(:,161))
  call vert_VQ_A(wf(:,-5),wf(:,9),wf(:,162))
  call prop_Q_A(wf(:,162),Q(:,45),MT,1_intkind1,wf(:,163))
  call vert_SA_Q(gPtb,wf(:,20),wf(:,-1),wf(:,164))
  call prop_A_Q(wf(:,164),Q(:,23),MB,1_intkind1,wf(:,165))
  call vert_AW_Q(wf(:,-1),wf(:,22),wf(:,166))
  call prop_A_Q(wf(:,166),Q(:,23),MB,1_intkind1,wf(:,167))
  call vert_QS_A(gPtb,wf(:,-3),wf(:,20),wf(:,168))
  call prop_Q_A(wf(:,168),Q(:,29),MT,1_intkind1,wf(:,169))
  call vert_WQ_A(wf(:,22),wf(:,-3),wf(:,170))
  call prop_Q_A(wf(:,170),Q(:,29),MT,1_intkind1,wf(:,171))
  call vert_QA_V(wf(:,63),wf(:,-2),wf(:,172))
  call prop_A_Q(wf(:,65),Q(:,14),ZERO,0_intkind1,wf(:,173))
  call vert_QA_V(wf(:,0),wf(:,173),wf(:,174))
  call vert_WQ_A(wf(:,30),wf(:,0),wf(:,175))
  call prop_Q_A(wf(:,175),Q(:,27),ZERO,0_intkind1,wf(:,176))
  call vert_WQ_A(wf(:,32),wf(:,0),wf(:,177))
  call prop_Q_A(wf(:,177),Q(:,27),ZERO,0_intkind1,wf(:,178))
  call vert_AW_Q(wf(:,-2),wf(:,30),wf(:,179))
  call prop_A_Q(wf(:,179),Q(:,30),ZERO,0_intkind1,wf(:,180))
  call vert_AW_Q(wf(:,-2),wf(:,32),wf(:,181))
  call prop_A_Q(wf(:,181),Q(:,30),ZERO,0_intkind1,wf(:,182))
  call vert_QA_W(wf(:,5),wf(:,4),wf(:,183))
  call vert_WQ_A(wf(:,36),wf(:,0),wf(:,184))
  call prop_Q_A(wf(:,184),Q(:,27),ZERO,0_intkind1,wf(:,185))
  call vert_AW_Q(wf(:,-2),wf(:,36),wf(:,186))
  call prop_A_Q(wf(:,186),Q(:,30),ZERO,0_intkind1,wf(:,187))
  call vert_QA_W(wf(:,-3),wf(:,49),wf(:,188))
  call vert_QA_W(wf(:,13),wf(:,12),wf(:,189))
  call vert_WQ_A(wf(:,37),wf(:,0),wf(:,190))
  call prop_Q_A(wf(:,190),Q(:,27),ZERO,0_intkind1,wf(:,191))
  call vert_AW_Q(wf(:,-2),wf(:,37),wf(:,192))
  call prop_A_Q(wf(:,192),Q(:,30),ZERO,0_intkind1,wf(:,193))
  call vert_QA_W(wf(:,79),wf(:,-1),wf(:,194))
  call vert_AW_Q(wf(:,-1),wf(:,29),wf(:,195))
  call prop_A_Q(wf(:,195),Q(:,39),MB,1_intkind1,wf(:,196))
  call vert_WQ_A(wf(:,29),wf(:,-3),wf(:,197))
  call prop_Q_A(wf(:,197),Q(:,45),MT,1_intkind1,wf(:,198))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,29),Q(:,37),wf(:,199))
  call vert_SV_V(wf(:,-4),wf(:,29),wf(:,200))
  call vert_ST_V(wf(:,19),Q(:,42),wf(:,-4),Q(:,16),wf(:,201))
  call vert_SV_V(wf(:,-4),wf(:,21),wf(:,202))
  call vert_QA_W(wf(:,-3),wf(:,54),wf(:,203))
  call vert_AW_Q(wf(:,-1),wf(:,35),wf(:,204))
  call prop_A_Q(wf(:,204),Q(:,39),MB,1_intkind1,wf(:,205))
  call vert_WQ_A(wf(:,35),wf(:,-3),wf(:,206))
  call prop_Q_A(wf(:,206),Q(:,45),MT,1_intkind1,wf(:,207))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,35),Q(:,37),wf(:,208))
  call vert_SV_V(wf(:,-4),wf(:,35),wf(:,209))
  call vert_ST_V(wf(:,24),Q(:,42),wf(:,-4),Q(:,16),wf(:,210))
  call vert_SV_V(wf(:,-4),wf(:,25),wf(:,211))
  call vert_QA_W(wf(:,85),wf(:,-1),wf(:,212))

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
  den(2) = 1 / (Q(5,18) - MT2)
  den(3) = 1 / (Q(5,40) - MB2)
  den(6) = 1 / (Q(5,13) - MT2)
  den(9) = 1 / (Q(5,34) - MT2)
  den(10) = 1 / (Q(5,24) - MB2)
  den(13) = 1 / (Q(5,7) - MB2)
  den(17) = 1 / (Q(5,42) - MW2)
  den(23) = 1 / (Q(5,33))
  den(24) = 1 / (Q(5,10) - MW2)
  den(25) = 1 / (Q(5,37) - MW2)
  den(28) = 1 / (Q(5,36))
  den(33) = 1 / (Q(5,26) - MW2)
  den(37) = 1 / (Q(5,48))
  den(39) = 1 / (Q(5,50) - MT2)
  den(44) = 1 / (Q(5,21) - MW2)
  den(49) = 1 / (Q(5,11))
  den(52) = 1 / (Q(5,49))
  den(59) = 1 / (Q(5,56) - MB2)
  den(70) = 1 / (Q(5,45) - MT2)
  den(73) = 1 / (Q(5,23) - MB2)
  den(80) = 1 / (Q(5,29) - MT2)
  den(83) = 1 / (Q(5,39) - MB2)
  den(109) = 1 / (Q(5,15))
  den(119) = 1 / (Q(5,14))
  den(122) = 1 / (Q(5,27))
  den(124) = 1 / (Q(5,30))
  den(127) = 1 / (Q(5,58) - MW2)
  den(139) = 1 / (Q(5,53) - MW2)

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(11) = den(1)*den(9)
  den(12) = den(10)*den(11)
  den(14) = den(1)*den(13)
  den(15) = den(10)*den(14)
  den(16) = den(7)*den(9)
  den(18) = den(9)*den(17)
  den(19) = den(1)*den(18)
  den(20) = den(3)*den(14)
  den(21) = den(3)*den(17)
  den(22) = den(1)*den(21)
  den(26) = den(23)*den(25)
  den(27) = den(24)*den(26)
  den(29) = den(25)*den(28)
  den(30) = den(24)*den(29)
  den(31) = den(2)*den(26)
  den(32) = den(2)*den(29)
  den(34) = den(10)*den(33)
  den(35) = den(23)*den(34)
  den(36) = den(10)*den(29)
  den(38) = den(14)*den(37)
  den(40) = den(37)*den(39)
  den(41) = den(1)*den(40)
  den(42) = den(2)*den(39)
  den(43) = den(1)*den(42)
  den(45) = den(1)*den(44)
  den(46) = den(9)*den(45)
  den(47) = den(9)*den(39)
  den(48) = den(1)*den(47)
  den(50) = den(24)*den(49)
  den(51) = den(37)*den(50)
  den(53) = den(37)*den(52)
  den(54) = den(24)*den(53)
  den(55) = den(24)*den(33)
  den(56) = den(23)*den(55)
  den(57) = den(2)*den(33)
  den(58) = den(23)*den(57)
  den(60) = den(10)*den(59)
  den(61) = den(1)*den(60)
  den(62) = den(3)*den(45)
  den(63) = den(3)*den(59)
  den(64) = den(1)*den(63)
  den(65) = den(10)*den(26)
  den(66) = den(28)*den(55)
  den(67) = den(28)*den(57)
  den(68) = den(28)*den(34)
  den(69) = den(1)*den(3)
  den(71) = den(69)*den(70)
  den(72) = den(2)*den(71)
  den(74) = den(4)*den(73)
  den(75) = den(3)*den(74)
  den(76) = den(2)**2
  den(77) = den(7)*den(76)
  den(78) = den(7)*den(42)
  den(79) = den(1)*den(10)
  den(81) = den(79)*den(80)
  den(82) = den(9)*den(81)
  den(84) = den(11)*den(83)
  den(85) = den(10)*den(84)
  den(86) = den(14)*den(60)
  den(87) = den(10)**2
  den(88) = den(14)*den(87)
  den(89) = den(9)**2
  den(90) = den(45)*den(89)
  den(91) = den(7)*den(89)
  den(92) = den(7)*den(47)
  den(93) = den(3)**2
  den(94) = den(45)*den(93)
  den(95) = den(14)*den(63)
  den(96) = den(14)*den(93)
  den(97) = den(23)**2
  den(98) = den(55)*den(97)
  den(99) = den(28)**2
  den(100) = den(55)*den(99)
  den(101) = den(57)*den(97)
  den(102) = den(26)*den(76)
  den(103) = den(57)*den(99)
  den(104) = den(29)*den(76)
  den(105) = den(34)*den(97)
  den(106) = den(26)*den(87)
  den(107) = den(34)*den(99)
  den(108) = den(29)*den(87)
  den(110) = den(14)*den(109)
  den(111) = den(14)*den(73)
  den(112) = den(14)*den(83)
  den(113) = den(7)*den(109)
  den(114) = den(7)*den(80)
  den(115) = den(7)*den(70)
  den(116) = den(45)*den(73)
  den(117) = den(45)*den(80)
  den(118) = den(50)*den(109)
  den(120) = den(24)*den(119)
  den(121) = den(109)*den(120)
  den(123) = den(55)*den(122)
  den(125) = den(55)*den(124)
  den(126) = den(2)*den(3)
  den(128) = den(126)*den(127)
  den(129) = den(57)*den(122)
  den(130) = den(57)*den(124)
  den(131) = den(42)*den(127)
  den(132) = den(9)*den(10)
  den(133) = den(127)*den(132)
  den(134) = den(34)*den(122)
  den(135) = den(34)*den(124)
  den(136) = den(60)*den(127)
  den(137) = den(26)*den(83)
  den(138) = den(26)*den(70)
  den(140) = den(26)*den(139)
  den(141) = den(18)*den(127)
  den(142) = den(47)*den(127)
  den(143) = den(29)*den(83)
  den(144) = den(29)*den(70)
  den(145) = den(29)*den(139)
  den(146) = den(21)*den(127)
  den(147) = den(63)*den(127)
  den(148) = den(1)*den(2)*den(3)
  den(149) = den(1)*den(9)*den(10)
  den(150) = den(2)*den(23)
  den(151) = den(2)*den(28)
  den(152) = den(10)*den(23)
  den(153) = den(10)*den(28)
  den(154) = den(2)*den(115)
  den(155) = den(10)*den(112)
  den(156) = den(9)*den(114)
  den(157) = den(9)*den(117)
  den(158) = den(3)*den(111)
  den(159) = den(3)*den(116)
  den(160) = den(23)*den(125)
  den(161) = den(28)*den(123)
  den(162) = den(2)*den(138)
  den(163) = den(23)*den(130)
  den(164) = den(2)*den(144)
  den(165) = den(28)*den(129)
  den(166) = den(10)*den(137)
  den(167) = den(23)*den(135)
  den(168) = den(10)*den(143)
  den(169) = den(28)*den(134)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(136)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,13),wf(:,14)) * den(12)
  A(4) = cont_QA(wf(:,16),wf(:,17)) * den(15)
  A(5) = cont_QA(wf(:,9),wf(:,18)) * den(16)
  A(6) = cont_SS(wf(:,19),wf(:,20)) * den(19)
  A(7) = cont_VV(wf(:,21),wf(:,22)) * den(19)
  A(8) = cont_QA(wf(:,17),wf(:,23)) * den(20)
  A(9) = cont_SS(wf(:,20),wf(:,24)) * den(22)
  A(10) = cont_VV(wf(:,22),wf(:,25)) * den(22)
  A(11) = cont_VV(wf(:,29),wf(:,30)) * den(27)
  A(12) = cont_VV(wf(:,29),wf(:,32)) * den(27)
  A(13) = cont_VV(wf(:,30),wf(:,35)) * den(30)
  A(14) = cont_VV(wf(:,32),wf(:,35)) * den(30)
  A(15) = cont_VV(wf(:,29),wf(:,36)) * den(31)
  A(16) = cont_VV(wf(:,35),wf(:,36)) * den(32)
  A(17) = cont_VV(wf(:,29),wf(:,37)) * den(35)
  A(18) = cont_VV(wf(:,35),wf(:,37)) * den(36)

  A(19) = cont_QA(wf(:,5),wf(:,38)) * den(5)
  A(20) = cont_QA(wf(:,9),wf(:,39)) * den(8)
  A(21) = cont_QA(wf(:,13),wf(:,40)) * den(12)
  A(22) = cont_QA(wf(:,17),wf(:,41)) * den(15)
  A(23) = cont_QA(wf(:,9),wf(:,42)) * den(16)
  A(24) = cont_QA(wf(:,17),wf(:,43)) * den(20)
  A(25) = cont_QA(wf(:,17),wf(:,45)) * den(38)
  A(26) = cont_QA(wf(:,17),wf(:,45)) * den(38)
  A(27) = cont_QA(wf(:,7),wf(:,47)) * den(41)
  A(28) = cont_QA(wf(:,7),wf(:,47)) * den(41)
  A(29) = cont_QA(wf(:,48),wf(:,49)) * den(43)
  A(30) = cont_QA(wf(:,6),wf(:,51)) * den(5)
  A(31) = cont_SS(wf(:,20),wf(:,52)) * den(46)
  A(32) = cont_VV(wf(:,22),wf(:,53)) * den(46)
  A(33) = cont_QA(wf(:,48),wf(:,54)) * den(48)
  A(34) = cont_QA(wf(:,17),wf(:,55)) * den(20)
  A(35) = cont_SS(wf(:,20),wf(:,56)) * den(22)
  A(36) = cont_VV(wf(:,22),wf(:,57)) * den(22)
  A(37) = cont_QA(wf(:,14),wf(:,59)) * den(12)
  A(38) = cont_QA(wf(:,17),wf(:,60)) * den(15)
  A(39) = cont_QA(wf(:,62),wf(:,63)) * den(51)
  A(40) = cont_QA(wf(:,62),wf(:,63)) * den(51)
  A(41) = cont_QA(wf(:,65),wf(:,66)) * den(54)
  A(42) = cont_QA(wf(:,65),wf(:,66)) * den(54)
  A(43) = cont_VV(wf(:,29),wf(:,67)) * den(31)
  A(44) = cont_VV(wf(:,35),wf(:,67)) * den(32)
  A(45) = cont_VV(wf(:,29),wf(:,68)) * den(35)
  A(46) = cont_VV(wf(:,35),wf(:,68)) * den(36)
  A(47) = cont_SS(wf(:,69),wf(:,70)) * den(56)
  A(48) = cont_VV(wf(:,30),wf(:,71)) * den(56)
  A(49) = cont_SS(wf(:,70),wf(:,72)) * den(56)
  A(50) = cont_VV(wf(:,32),wf(:,71)) * den(56)
  A(51) = cont_VV(wf(:,30),wf(:,75)) * den(30)
  A(52) = cont_VV(wf(:,32),wf(:,75)) * den(30)
  A(53) = cont_SS(wf(:,70),wf(:,76)) * den(58)
  A(54) = cont_VV(wf(:,36),wf(:,71)) * den(58)
  A(55) = cont_VV(wf(:,36),wf(:,75)) * den(32)
  A(56) = cont_SS(wf(:,70),wf(:,77)) * den(35)
  A(57) = cont_VV(wf(:,37),wf(:,71)) * den(35)
  A(58) = cont_VV(wf(:,37),wf(:,75)) * den(36)
  A(59) = cont_QA(wf(:,78),wf(:,79)) * den(61)
  A(60) = cont_QA(wf(:,13),wf(:,82)) * den(12)
  A(61) = cont_SS(wf(:,20),wf(:,83)) * den(62)
  A(62) = cont_VV(wf(:,22),wf(:,84)) * den(62)
  A(63) = cont_QA(wf(:,78),wf(:,85)) * den(64)
  A(64) = cont_QA(wf(:,9),wf(:,86)) * den(16)
  A(65) = cont_SS(wf(:,20),wf(:,87)) * den(19)
  A(66) = cont_VV(wf(:,22),wf(:,88)) * den(19)
  A(67) = cont_QA(wf(:,5),wf(:,91)) * den(5)
  A(68) = cont_QA(wf(:,9),wf(:,92)) * den(8)
  A(69) = cont_VV(wf(:,29),wf(:,93)) * den(65)
  A(70) = cont_VV(wf(:,35),wf(:,93)) * den(36)
  A(71) = cont_VV(wf(:,29),wf(:,94)) * den(31)
  A(72) = cont_VV(wf(:,35),wf(:,94)) * den(32)
  A(73) = cont_VV(wf(:,29),wf(:,96)) * den(27)
  A(74) = cont_VV(wf(:,29),wf(:,98)) * den(27)
  A(75) = cont_VV(wf(:,35),wf(:,96)) * den(30)
  A(76) = cont_VV(wf(:,35),wf(:,98)) * den(30)
  A(77) = cont_SS(wf(:,69),wf(:,99)) * den(66)
  A(78) = cont_SS(wf(:,72),wf(:,99)) * den(66)
  A(79) = cont_VV(wf(:,30),wf(:,100)) * den(66)
  A(80) = cont_VV(wf(:,32),wf(:,100)) * den(66)
  A(81) = cont_VV(wf(:,30),wf(:,103)) * den(27)
  A(82) = cont_VV(wf(:,32),wf(:,103)) * den(27)
  A(83) = cont_SS(wf(:,76),wf(:,99)) * den(67)
  A(84) = cont_VV(wf(:,36),wf(:,100)) * den(67)
  A(85) = cont_VV(wf(:,36),wf(:,103)) * den(31)
  A(86) = cont_SS(wf(:,77),wf(:,99)) * den(68)
  A(87) = cont_VV(wf(:,37),wf(:,100)) * den(68)
  A(88) = cont_VV(wf(:,37),wf(:,103)) * den(35)
  A(89) = cont_QA(wf(:,5),wf(:,105)) * den(5)
  A(90) = cont_QA(wf(:,5),wf(:,107)) * den(5)
  A(91) = cont_QA(wf(:,8),wf(:,109)) * den(8)
  A(92) = cont_QA(wf(:,8),wf(:,111)) * den(8)
  A(93) = cont_QA(wf(:,13),wf(:,112)) * den(12)
  A(94) = cont_QA(wf(:,13),wf(:,113)) * den(12)
  A(95) = cont_QA(wf(:,16),wf(:,115)) * den(15)
  A(96) = cont_QA(wf(:,16),wf(:,117)) * den(15)
  A(97) = cont_QA(wf(:,18),wf(:,109)) * den(16)
  A(98) = cont_QA(wf(:,18),wf(:,111)) * den(16)
  A(99) = cont_SS(wf(:,19),wf(:,118)) * den(19)
  A(100) = cont_VV(wf(:,21),wf(:,119)) * den(19)
  A(101) = cont_SS(wf(:,19),wf(:,120)) * den(19)
  A(102) = cont_VV(wf(:,21),wf(:,121)) * den(19)
  A(103) = cont_QA(wf(:,23),wf(:,115)) * den(20)
  A(104) = cont_QA(wf(:,23),wf(:,117)) * den(20)
  A(105) = cont_SS(wf(:,24),wf(:,118)) * den(22)
  A(106) = cont_VV(wf(:,25),wf(:,119)) * den(22)
  A(107) = cont_SS(wf(:,24),wf(:,120)) * den(22)
  A(108) = cont_VV(wf(:,25),wf(:,121)) * den(22)
  A(109) = cont_QA(wf(:,123),wf(:,124)) * den(72)
  A(110) = cont_QA(wf(:,125),wf(:,126)) * den(75)
  A(111) = cont_QA(wf(:,9),wf(:,128)) * den(77)
  A(112) = cont_QA(wf(:,49),wf(:,129)) * den(78)
  A(113) = cont_QA(wf(:,131),wf(:,132)) * den(82)
  A(114) = cont_QA(wf(:,133),wf(:,134)) * den(85)
  A(115) = cont_QA(wf(:,79),wf(:,135)) * den(86)
  A(116) = cont_QA(wf(:,17),wf(:,137)) * den(88)
  A(117) = cont_SS(wf(:,20),wf(:,139)) * den(90)
  A(118) = cont_VV(wf(:,22),wf(:,140)) * den(90)
  A(119) = cont_QA(wf(:,9),wf(:,141)) * den(91)
  A(120) = cont_QA(wf(:,54),wf(:,129)) * den(92)
  A(121) = cont_SS(wf(:,20),wf(:,143)) * den(94)
  A(122) = cont_VV(wf(:,22),wf(:,144)) * den(94)
  A(123) = cont_QA(wf(:,85),wf(:,135)) * den(95)
  A(124) = cont_QA(wf(:,17),wf(:,145)) * den(96)
  A(125) = cont_VV(wf(:,30),wf(:,148)) * den(98)
  A(126) = cont_VV(wf(:,32),wf(:,148)) * den(98)
  A(127) = cont_VV(wf(:,30),wf(:,151)) * den(100)
  A(128) = cont_VV(wf(:,32),wf(:,151)) * den(100)
  A(129) = cont_VV(wf(:,36),wf(:,148)) * den(101)
  A(130) = cont_VV(wf(:,29),wf(:,152)) * den(102)
  A(131) = cont_VV(wf(:,36),wf(:,151)) * den(103)
  A(132) = cont_VV(wf(:,35),wf(:,152)) * den(104)
  A(133) = cont_VV(wf(:,37),wf(:,148)) * den(105)
  A(134) = cont_VV(wf(:,29),wf(:,153)) * den(106)
  A(135) = cont_VV(wf(:,37),wf(:,151)) * den(107)
  A(136) = cont_VV(wf(:,35),wf(:,153)) * den(108)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(136)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = (A(6)+A(9))*f(2)+(A(7)+A(10))*f(9)+(A(3)+A(4)+A(8))*f(21)+(A(1)+A(2)+A(5))*f(34)
  M1(2) = (A(11)+A(13))*f(2)+(A(12)+A(14))*f(9)+(A(17)+A(18))*f(21)+(A(15)+A(16))*f(34)

  M2(1) = (A(99)+A(105))*f(1)+(A(31)+A(61)-A(117)-A(121))*f(3)+A(35)*f(4)+A(65)*f(6)+(A(100)+A(106))*f(7)+(A(101)+A(107))*f(8)+( &
       -A(118)-A(122))*f(10)+A(36)*f(11)+A(66)*f(13)+(A(32)+A(62))*f(14)+(A(102)+A(108))*f(15)+(A(93)+A(95)+A(103))*f(20)+(-A(113) &
       -A(114)-A(115)-A(116)-A(123)-A(124))*f(22)+(A(22)+A(34))*f(23)+A(60)*f(25)+(A(24)+A(37)+A(38))*f(26)+(A(21)+A(59) &
       +A(63))*f(27)+(A(94)+A(96)+A(104))*f(28)+(A(26)+A(28))*f(29)+(A(89)+A(91)+A(97))*f(33)+(-A(109)-A(110)-A(111)-A(112)-A(119) &
       -A(120))*f(35)+A(30)*f(36)+(A(20)+A(64))*f(38)+(A(23)+A(67)+A(68))*f(39)+(A(19)+A(29)+A(33))*f(40)+(A(90)+A(92) &
       +A(98))*f(41)+(A(25)+A(27))*f(42)
  M2(2) = (A(47)+A(77))*f(1)+(A(73)+A(75)-A(125)-A(127))*f(3)+(A(51)+A(81))*f(5)+(A(49)+A(78))*f(7)+(A(48)+A(79))*f(8)+(-A(126) &
       -A(128))*f(10)+(A(52)+A(82))*f(12)+(A(74)+A(76))*f(14)+(A(50)+A(80))*f(15)+(A(56)+A(86))*f(20)+(-A(133)-A(134)-A(135) &
       -A(136))*f(22)+(A(58)+A(88))*f(24)+(A(45)+A(46))*f(26)+(A(69)+A(70))*f(27)+(A(57)+A(87))*f(28)+(A(40)+A(42))*f(29)+(A(53) &
       +A(83))*f(33)+(-A(129)-A(130)-A(131)-A(132))*f(35)+(A(55)+A(85))*f(37)+(A(71)+A(72))*f(39)+(A(43)+A(44))*f(40)+(A(54) &
       +A(84))*f(41)+(A(39)+A(41))*f(42)

end subroutine colourvectors

end module ol_loop_ppthjj_utxdxbhg_1_/**/REALKIND
