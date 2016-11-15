
module ol_colourmatrix_ppvvv_bbxawwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(17,1), K2(1,1), KL(1,1)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  3]
  K1( 2,:) = [  4]
  K1( 3,:) = [ -4]
  K1( 4,:) = [  4]
  K1( 5,:) = [  0]
  K1( 6,:) = [  0]
  K1( 7,:) = [  0]
  K1( 8,:) = [  0]
  K1( 9,:) = [  0]
  K1(10,:) = [  0]
  K1(11,:) = [  0]
  K1(12,:) = [  0]
  K1(13,:) = [  0]
  K1(14,:) = [  0]
  K1(15,:) = [  0]
  K1(16,:) = [  0]
  K1(17,:) = [  0]

  K2(1,:) = [ 3]

  KL(1,:) = [ 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppvvv_bbxawwx_1_/**/REALKIND



module ol_forced_parameters_ppvvv_bbxawwx_1_/**/REALKIND
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
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppvvv_bbxawwx_1_/**/REALKIND

module ol_loop_ppvvv_bbxawwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(36), c(9)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:113)
  ! denominators
  complex(REALKIND), save :: den(70)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,72)
  ! zero helicity identifier
  logical,           save :: zerohel(72) = .true., zerohel_ct(72) = .true.

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
    f( 1) = (CI*eQED**3)/9._/**/REALKIND
    f( 2) = (CI*eQED**3)/3._/**/REALKIND
    f( 3) = (CI*countertermnorm*eQED**3*gQCD**2)/9._/**/REALKIND
    f( 4) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**2)/9._/**/REALKIND
    f( 5) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**2)/3._/**/REALKIND
    f( 6) = (CI*eQED**3)/(6._/**/REALKIND*sw**2)
    f( 7) = (CI*eQED**3)/(3._/**/REALKIND*sw**2)
    f( 8) = (CI*eQED**3)/(2._/**/REALKIND*sw**2)
    f( 9) = (CI*countertermnorm*eQED**3*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(10) = (CI*countertermnorm*eQED**3*gQCD**2)/(3._/**/REALKIND*sw**2)
    f(11) = (CI*countertermnorm*eQED**3*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(12) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(13) = (CI*countertermnorm*ctVbt*eQED**3*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(14) = (CI*countertermnorm*ctVbt*eQED**3*gQCD**2)/(3._/**/REALKIND*sw**2)
    f(15) = (CI*countertermnorm*ctVbt*eQED**3*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(16) = (CI*countertermnorm*ctVtt*eQED**3*gQCD**2)/(3._/**/REALKIND*sw**2)
    f(17) = (CI*eQED**3*MB)/(6._/**/REALKIND*sw**2)
    f(18) = (CI*eQED**3*MB)/(2._/**/REALKIND*sw**2)
    f(19) = (CI*countertermnorm*eQED**3*gQCD**2*MB)/(6._/**/REALKIND*sw**2)
    f(20) = (CI*countertermnorm*ctSbb*eQED**3*gQCD**2*MB)/(6._/**/REALKIND*sw**2)
    f(21) = (CI*countertermnorm*ctSbb*eQED**3*gQCD**2*MB)/(2._/**/REALKIND*sw**2)
    f(22) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**2*MB)/(6._/**/REALKIND*sw**2)
    f(23) = (CI*cw*eQED**3)/(3._/**/REALKIND*sw)
    f(24) = (CI*cw*eQED**3)/sw
    f(25) = (CI*countertermnorm*cw*eQED**3*gQCD**2)/(3._/**/REALKIND*sw)
    f(26) = (CI*countertermnorm*ctVbb*cw*eQED**3*gQCD**2)/(3._/**/REALKIND*sw)
    f(27) = (CI*countertermnorm*ctVbb*cw*eQED**3*gQCD**2)/sw
    f(28) = (eQED**3*gQCD**2*integralnorm*SwB)/9._/**/REALKIND
    f(29) = (eQED**3*gQCD**2*integralnorm*SwB)/3._/**/REALKIND
    f(30) = (eQED**3*gQCD**2*integralnorm*SwB)/(sw**2*6._/**/REALKIND)
    f(31) = (eQED**3*gQCD**2*integralnorm*SwB)/(sw**2*3._/**/REALKIND)
    f(32) = (eQED**3*gQCD**2*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(33) = (eQED**3*gQCD**2*integralnorm*MB*SwB)/(sw**2*6._/**/REALKIND)
    f(34) = (eQED**3*gQCD**2*integralnorm*MB*SwB)/(sw**2*2._/**/REALKIND)
    f(35) = (cw*eQED**3*gQCD**2*integralnorm*SwB)/(sw*3._/**/REALKIND)
    f(36) = (cw*eQED**3*gQCD**2*integralnorm*SwB)/sw

  c = [ 4*f(28), 4*f(29), 4*f(30), 4*f(31), 4*f(32), 4*f(33), 4*f(34), 4*f(35), 4*f(36) ]
  c = (1._/**/REALKIND / 3) * c
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
  real(REALKIND),    intent(in)  :: P(0:3,5)
  integer,           intent(in)  :: H(5)
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(66)
  ! external WFs
  call wf_Q(P(:,1), rMB, H(1), wf(:,0))
  call wf_A(P(:,2), rMB, H(2), wf(:,-1))
  call wf_V(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_V(P(:,4), rMW, H(4), wf(:,-3))
  call wf_V(P(:,5), rMW, H(5), wf(:,-4))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_WWV_V(wf(:,-3),wf(:,-4),wf(:,-2),wf(:,2))
  call vert_QA_Z(gZd,wf(:,0),wf(:,-1),wf(:,3))
  call prop_W_W(wf(:,3),Q(:,3),MZ,1_intkind1,wf(:,4))
  call vert_AQ_S(gH,wf(:,-1),wf(:,0),wf(:,5))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-2),Q(:,4),wf(:,6))
  call prop_W_W(wf(:,6),Q(:,12),MW,1_intkind1,wf(:,7))
  call vert_SV_V(wf(:,5),wf(:,-4),wf(:,8))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,9))
  call vert_UV_W(wf(:,4),Q(:,3),wf(:,-4),Q(:,16),wf(:,10))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,11))
  call prop_W_W(wf(:,11),Q(:,20),MW,1_intkind1,wf(:,12))
  call vert_SV_V(wf(:,5),wf(:,-3),wf(:,13))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,1),Q(:,3),wf(:,14))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,4),Q(:,3),wf(:,15))
  call vert_VQ_A(wf(:,-2),wf(:,0),wf(:,16))
  call vert_AW_Q(wf(:,-1),wf(:,-3),wf(:,17))
  call prop_Q_A(wf(:,16),Q(:,5),MB,1_intkind1,wf(:,18))
  call prop_A_Q(wf(:,17),Q(:,10),MT,1_intkind1,wf(:,19))
  call vert_WQ_A(wf(:,-4),wf(:,18),wf(:,20))
  call vert_VV_S(wf(:,-3),wf(:,-4),wf(:,21))
  call vert_AQ_S(gH,wf(:,-1),wf(:,18),wf(:,22))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-3),Q(:,8),wf(:,23))
  call vert_QA_V(wf(:,18),wf(:,-1),wf(:,24))
  call prop_W_W(wf(:,23),Q(:,24),MZ,1_intkind1,wf(:,25))
  call vert_QA_Z(gZd,wf(:,18),wf(:,-1),wf(:,26))
  call vert_WQ_A(wf(:,-4),wf(:,0),wf(:,27))
  call vert_AV_Q(wf(:,-1),wf(:,-2),wf(:,28))
  call prop_Q_A(wf(:,27),Q(:,17),MT,1_intkind1,wf(:,29))
  call prop_A_Q(wf(:,28),Q(:,6),MB,1_intkind1,wf(:,30))
  call vert_WQ_A(wf(:,-3),wf(:,29),wf(:,31))
  call vert_AQ_S(gH,wf(:,30),wf(:,0),wf(:,32))
  call vert_QA_V(wf(:,0),wf(:,30),wf(:,33))
  call vert_QA_Z(gZd,wf(:,0),wf(:,30),wf(:,34))
  call vert_VQ_A(wf(:,-2),wf(:,29),wf(:,35))
  call vert_QA_W(wf(:,0),wf(:,19),wf(:,36))
  call vert_QA_W(wf(:,29),wf(:,-1),wf(:,37))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,38))
  call counter_QA_Z(gZd,wf(:,0),wf(:,-1),wf(:,39))
  call prop_W_W(wf(:,2),Q(:,28),MZ,1_intkind1,wf(:,40))
  call counter_WQ_A(wf(:,-4),wf(:,18),wf(:,41))
  call counter_WQ_A(wf(:,-3),wf(:,29),wf(:,42))
  call counter_VQ_A(wf(:,-2),wf(:,29),wf(:,43))
  call counter_AQ_S(gH,wf(:,-1),wf(:,18),wf(:,44))
  call counter_QA_V(wf(:,18),wf(:,-1),wf(:,45))
  call counter_QA_Z(gZd,wf(:,18),wf(:,-1),wf(:,46))
  call counter_AW_Q(wf(:,-1),wf(:,-3),wf(:,47))
  call prop_A_Q(wf(:,47),Q(:,10),MT,1_intkind1,wf(:,48))
  call counter_QA_W(wf(:,29),wf(:,-1),wf(:,49))
  call vert_QA_W(wf(:,0),wf(:,48),wf(:,50))
  call counter_AV_Q(wf(:,-1),wf(:,-2),wf(:,51))
  call prop_A_Q(wf(:,51),Q(:,6),MB,1_intkind1,wf(:,52))
  call vert_AQ_S(gH,wf(:,52),wf(:,0),wf(:,53))
  call vert_QA_V(wf(:,0),wf(:,52),wf(:,54))
  call vert_QA_Z(gZd,wf(:,0),wf(:,52),wf(:,55))
  call counter_AQ_S(gH,wf(:,30),wf(:,0),wf(:,56))
  call counter_QA_V(wf(:,0),wf(:,30),wf(:,57))
  call counter_QA_Z(gZd,wf(:,0),wf(:,30),wf(:,58))
  call counter_WQ_A(wf(:,-4),wf(:,0),wf(:,59))
  call prop_Q_A(wf(:,59),Q(:,17),MT,1_intkind1,wf(:,60))
  call vert_WQ_A(wf(:,-3),wf(:,60),wf(:,61))
  call counter_QA_W(wf(:,0),wf(:,19),wf(:,62))
  call vert_VQ_A(wf(:,-2),wf(:,60),wf(:,63))
  call vert_QA_W(wf(:,60),wf(:,-1),wf(:,64))
  call counter_VQ_A(wf(:,-2),wf(:,0),wf(:,65))
  call prop_Q_A(wf(:,65),Q(:,5),MB,1_intkind1,wf(:,66))
  call vert_WQ_A(wf(:,-4),wf(:,66),wf(:,67))
  call vert_AQ_S(gH,wf(:,-1),wf(:,66),wf(:,68))
  call vert_QA_V(wf(:,66),wf(:,-1),wf(:,69))
  call vert_QA_Z(gZd,wf(:,66),wf(:,-1),wf(:,70))
  call counter_AQ_S(gH,wf(:,-1),wf(:,0),wf(:,71))
  call vert_SV_V(wf(:,71),wf(:,-4),wf(:,72))
  call vert_UV_W(wf(:,38),Q(:,3),wf(:,-4),Q(:,16),wf(:,73))
  call prop_W_W(wf(:,39),Q(:,3),MZ,1_intkind1,wf(:,74))
  call vert_UV_W(wf(:,74),Q(:,3),wf(:,-4),Q(:,16),wf(:,75))
  call vert_SV_V(wf(:,71),wf(:,-3),wf(:,76))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,38),Q(:,3),wf(:,77))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,74),Q(:,3),wf(:,78))
  call vert_AW_Q(wf(:,19),wf(:,-4),wf(:,79))
  call counter_Q_A(ctbb,wf(:,18),Q(:,5),wf(:,80))
  call prop_A_Q(wf(:,79),Q(:,26),MB,1_intkind1,wf(:,81))
  call counter_A_Q(cttt,wf(:,19),Q(:,10),wf(:,82))
  call prop_Q_A(wf(:,20),Q(:,21),MT,1_intkind1,wf(:,83))
  call vert_SA_Q(gH,wf(:,21),wf(:,-1),wf(:,84))
  call prop_A_Q(wf(:,84),Q(:,26),MB,1_intkind1,wf(:,85))
  call vert_AV_Q(wf(:,-1),wf(:,23),wf(:,86))
  call prop_A_Q(wf(:,86),Q(:,26),MB,1_intkind1,wf(:,87))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,25),wf(:,88))
  call prop_A_Q(wf(:,88),Q(:,26),MB,1_intkind1,wf(:,89))
  call vert_AW_Q(wf(:,30),wf(:,-3),wf(:,90))
  call counter_Q_A(cttt,wf(:,29),Q(:,17),wf(:,91))
  call prop_A_Q(wf(:,90),Q(:,14),MT,1_intkind1,wf(:,92))
  call counter_A_Q(ctbb,wf(:,30),Q(:,6),wf(:,93))
  call prop_Q_A(wf(:,31),Q(:,25),MB,1_intkind1,wf(:,94))
  call vert_QS_A(gH,wf(:,0),wf(:,21),wf(:,95))
  call prop_Q_A(wf(:,95),Q(:,25),MB,1_intkind1,wf(:,96))
  call vert_VQ_A(wf(:,23),wf(:,0),wf(:,97))
  call prop_Q_A(wf(:,97),Q(:,25),MB,1_intkind1,wf(:,98))
  call vert_ZQ_A(gZd,wf(:,25),wf(:,0),wf(:,99))
  call prop_Q_A(wf(:,99),Q(:,25),MB,1_intkind1,wf(:,100))
  call vert_AV_Q(wf(:,19),wf(:,-2),wf(:,101))
  call prop_A_Q(wf(:,101),Q(:,14),MT,1_intkind1,wf(:,102))
  call prop_Q_A(wf(:,35),Q(:,21),MT,1_intkind1,wf(:,103))
  call vert_WQ_A(wf(:,12),wf(:,0),wf(:,104))
  call prop_Q_A(wf(:,104),Q(:,21),MT,1_intkind1,wf(:,105))
  call vert_AW_Q(wf(:,-1),wf(:,7),wf(:,106))
  call prop_A_Q(wf(:,106),Q(:,14),MT,1_intkind1,wf(:,107))
  call vert_VV_S(wf(:,7),wf(:,-4),wf(:,108))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,7),Q(:,12),wf(:,109))
  call prop_W_W(wf(:,109),Q(:,28),MZ,1_intkind1,wf(:,110))
  call vert_VV_S(wf(:,-3),wf(:,12),wf(:,111))
  call vert_UV_W(wf(:,12),Q(:,20),wf(:,-3),Q(:,8),wf(:,112))
  call prop_W_W(wf(:,112),Q(:,28),MZ,1_intkind1,wf(:,113))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3))
  den(2) = 1 / (Q(5,3) - MZ2)
  den(3) = 1 / (Q(5,3) - MH2)
  den(4) = 1 / (Q(5,12) - MW2)
  den(8) = 1 / (Q(5,20) - MW2)
  den(12) = 1 / (Q(5,5) - MB2)
  den(13) = 1 / (Q(5,10) - MT2)
  den(15) = 1 / (Q(5,24) - MH2)
  den(17) = 1 / (Q(5,24))
  den(19) = 1 / (Q(5,24) - MZ2)
  den(21) = 1 / (Q(5,17) - MT2)
  den(22) = 1 / (Q(5,6) - MB2)
  den(30) = 1 / (Q(5,28))
  den(31) = 1 / (Q(5,28) - MZ2)
  den(32) = 1 / (Q(5,26) - MB2)
  den(35) = 1 / (Q(5,21) - MT2)
  den(44) = 1 / (Q(5,14) - MT2)
  den(47) = 1 / (Q(5,25) - MB2)
  den(64) = 1 / (Q(5,28) - MH2)

  ! denominators
  den(5) = den(3)*den(4)
  den(6) = den(1)*den(4)
  den(7) = den(2)*den(4)
  den(9) = den(3)*den(8)
  den(10) = den(1)*den(8)
  den(11) = den(2)*den(8)
  den(14) = den(12)*den(13)
  den(16) = den(12)*den(15)
  den(18) = den(12)*den(17)
  den(20) = den(12)*den(19)
  den(23) = den(21)*den(22)
  den(24) = den(15)*den(22)
  den(25) = den(17)*den(22)
  den(26) = den(19)*den(22)
  den(27) = den(13)*den(21)
  den(28) = den(8)*den(13)
  den(29) = den(4)*den(21)
  den(33) = den(13)*den(32)
  den(34) = den(12)*den(33)
  den(36) = den(12)*den(35)
  den(37) = den(13)*den(36)
  den(38) = den(15)*den(32)
  den(39) = den(12)*den(38)
  den(40) = den(17)*den(32)
  den(41) = den(12)*den(40)
  den(42) = den(19)*den(32)
  den(43) = den(12)*den(42)
  den(45) = den(22)*den(44)
  den(46) = den(21)*den(45)
  den(48) = den(21)*den(47)
  den(49) = den(22)*den(48)
  den(50) = den(15)*den(47)
  den(51) = den(22)*den(50)
  den(52) = den(17)*den(47)
  den(53) = den(22)*den(52)
  den(54) = den(19)*den(47)
  den(55) = den(22)*den(54)
  den(56) = den(13)*den(44)
  den(57) = den(21)*den(56)
  den(58) = den(21)*den(35)
  den(59) = den(13)*den(58)
  den(60) = den(8)*den(35)
  den(61) = den(13)*den(60)
  den(62) = den(4)*den(44)
  den(63) = den(21)*den(62)
  den(65) = den(4)*den(64)
  den(66) = den(4)*den(30)
  den(67) = den(4)*den(31)
  den(68) = den(8)*den(64)
  den(69) = den(8)*den(30)
  den(70) = den(8)*den(31)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(66)

  A(1) = cont_VV(wf(:,1),wf(:,2)) * den(1)
  A(2) = cont_VV(wf(:,2),wf(:,4)) * den(2)
  A(3) = cont_VV(wf(:,7),wf(:,8)) * den(5)
  A(4) = cont_VV(wf(:,7),wf(:,9)) * den(6)
  A(5) = cont_VV(wf(:,7),wf(:,10)) * den(7)
  A(6) = cont_VV(wf(:,12),wf(:,13)) * den(9)
  A(7) = cont_VV(wf(:,12),wf(:,14)) * den(10)
  A(8) = cont_VV(wf(:,12),wf(:,15)) * den(11)
  A(9) = cont_QA(wf(:,19),wf(:,20)) * den(14)
  A(10) = cont_SS(wf(:,21),wf(:,22)) * den(16)
  A(11) = cont_VV(wf(:,23),wf(:,24)) * den(18)
  A(12) = cont_VV(wf(:,25),wf(:,26)) * den(20)
  A(13) = cont_QA(wf(:,30),wf(:,31)) * den(23)
  A(14) = cont_SS(wf(:,21),wf(:,32)) * den(24)
  A(15) = cont_VV(wf(:,23),wf(:,33)) * den(25)
  A(16) = cont_VV(wf(:,25),wf(:,34)) * den(26)
  A(17) = cont_QA(wf(:,19),wf(:,35)) * den(27)
  A(18) = cont_VV(wf(:,12),wf(:,36)) * den(28)
  A(19) = cont_VV(wf(:,7),wf(:,37)) * den(29)

  A(20) = cont_VV(wf(:,2),wf(:,38)) * den(30)
  A(21) = cont_VV(wf(:,39),wf(:,40)) * den(31)
  A(22) = cont_QA(wf(:,19),wf(:,41)) * den(14)
  A(23) = cont_QA(wf(:,30),wf(:,42)) * den(23)
  A(24) = cont_QA(wf(:,19),wf(:,43)) * den(27)
  A(25) = cont_SS(wf(:,21),wf(:,44)) * den(16)
  A(26) = cont_VV(wf(:,23),wf(:,45)) * den(18)
  A(27) = cont_VV(wf(:,25),wf(:,46)) * den(20)
  A(28) = cont_QA(wf(:,20),wf(:,48)) * den(14)
  A(29) = cont_VV(wf(:,7),wf(:,49)) * den(29)
  A(30) = cont_QA(wf(:,35),wf(:,48)) * den(27)
  A(31) = cont_VV(wf(:,12),wf(:,50)) * den(28)
  A(32) = cont_QA(wf(:,31),wf(:,52)) * den(23)
  A(33) = cont_SS(wf(:,21),wf(:,53)) * den(24)
  A(34) = cont_VV(wf(:,23),wf(:,54)) * den(25)
  A(35) = cont_VV(wf(:,25),wf(:,55)) * den(26)
  A(36) = cont_SS(wf(:,21),wf(:,56)) * den(24)
  A(37) = cont_VV(wf(:,23),wf(:,57)) * den(25)
  A(38) = cont_VV(wf(:,25),wf(:,58)) * den(26)
  A(39) = cont_QA(wf(:,30),wf(:,61)) * den(23)
  A(40) = cont_VV(wf(:,12),wf(:,62)) * den(28)
  A(41) = cont_QA(wf(:,19),wf(:,63)) * den(27)
  A(42) = cont_VV(wf(:,7),wf(:,64)) * den(29)
  A(43) = cont_QA(wf(:,19),wf(:,67)) * den(14)
  A(44) = cont_SS(wf(:,21),wf(:,68)) * den(16)
  A(45) = cont_VV(wf(:,23),wf(:,69)) * den(18)
  A(46) = cont_VV(wf(:,25),wf(:,70)) * den(20)
  A(47) = cont_VV(wf(:,7),wf(:,72)) * den(5)
  A(48) = cont_VV(wf(:,7),wf(:,73)) * den(6)
  A(49) = cont_VV(wf(:,7),wf(:,75)) * den(7)
  A(50) = cont_VV(wf(:,12),wf(:,76)) * den(9)
  A(51) = cont_VV(wf(:,12),wf(:,77)) * den(10)
  A(52) = cont_VV(wf(:,12),wf(:,78)) * den(11)
  A(53) = cont_QA(wf(:,80),wf(:,81)) * den(34)
  A(54) = cont_QA(wf(:,82),wf(:,83)) * den(37)
  A(55) = cont_QA(wf(:,80),wf(:,85)) * den(39)
  A(56) = cont_QA(wf(:,80),wf(:,87)) * den(41)
  A(57) = cont_QA(wf(:,80),wf(:,89)) * den(43)
  A(58) = cont_QA(wf(:,91),wf(:,92)) * den(46)
  A(59) = cont_QA(wf(:,93),wf(:,94)) * den(49)
  A(60) = cont_QA(wf(:,93),wf(:,96)) * den(51)
  A(61) = cont_QA(wf(:,93),wf(:,98)) * den(53)
  A(62) = cont_QA(wf(:,93),wf(:,100)) * den(55)
  A(63) = cont_QA(wf(:,91),wf(:,102)) * den(57)
  A(64) = cont_QA(wf(:,82),wf(:,103)) * den(59)
  A(65) = cont_QA(wf(:,82),wf(:,105)) * den(61)
  A(66) = cont_QA(wf(:,91),wf(:,107)) * den(63)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(66)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(11)+A(15))*f(1)+(A(1)-A(4)-A(7))*f(2)+(-A(9)-A(13))*f(6)+A(17)*f(7)+(A(18)+A(19))*f(8)+(A(10)+A(14))*f(17)+(-A(3) &
       -A(6))*f(18)+(-A(12)-A(16))*f(23)+(-A(2)+A(5)+A(8))*f(24)

  M2(1) = (-A(56)-A(61))*f(3)+(A(26)+A(34)+A(37)+A(45))*f(4)+(A(20)-A(48)-A(51))*f(5)+(A(53)+A(54)+A(58)+A(59))*f(9)+(-A(63) &
       -A(64))*f(10)+(-A(65)-A(66))*f(11)+(-A(32)-A(43))*f(12)+(-A(22)-A(23)-A(28)-A(39))*f(13)+(A(30)+A(41))*f(14)+(A(29)+A(31) &
       +A(40)+A(42))*f(15)+A(24)*f(16)+(-A(55)-A(60))*f(19)+(A(25)+A(36))*f(20)+(-A(47)-A(50))*f(21)+(A(33)+A(44))*f(22)+(A(57) &
       +A(62))*f(25)+(-A(27)-A(35)-A(38)-A(46))*f(26)+(-A(21)+A(49)+A(52))*f(27)

end subroutine colourvectors

end module ol_loop_ppvvv_bbxawwx_1_/**/REALKIND
