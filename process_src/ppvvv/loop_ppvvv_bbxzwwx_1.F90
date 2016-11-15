
module ol_colourmatrix_ppvvv_bbxzwwx_1_/**/REALKIND
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
end module ol_colourmatrix_ppvvv_bbxzwwx_1_/**/REALKIND



module ol_forced_parameters_ppvvv_bbxzwwx_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppvvv_bbxzwwx_1_/**/REALKIND

module ol_loop_ppvvv_bbxzwwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(38), c(10)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:120)
  ! denominators
  complex(REALKIND), save :: den(74)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,108)
  ! zero helicity identifier
  logical,           save :: zerohel(108) = .true., zerohel_ct(108) = .true.

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
    f( 1) = (CI*eQED**3)/3._/**/REALKIND
    f( 2) = (CI*countertermnorm*eQED**3*gQCD**2)/3._/**/REALKIND
    f( 3) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**2)/3._/**/REALKIND
    f( 4) = (CI*cw*eQED**3)/(2._/**/REALKIND*sw**3)
    f( 5) = (CI*countertermnorm*cw*eQED**3*gQCD**2)/(2._/**/REALKIND*sw**3)
    f( 6) = (CI*countertermnorm*ctVbt*cw*eQED**3*gQCD**2)/(2._/**/REALKIND*sw**3)
    f( 7) = (CI*eQED**3*MB)/(2._/**/REALKIND*cw*sw**3)
    f( 8) = (CI*cw*eQED**3*MB)/(2._/**/REALKIND*sw**3)
    f( 9) = (CI*countertermnorm*ctSbb*eQED**3*gQCD**2*MB)/(2._/**/REALKIND*cw*sw**3)
    f(10) = (CI*countertermnorm*ctSbb*cw*eQED**3*gQCD**2*MB)/(2._/**/REALKIND*sw**3)
    f(11) = (CI*eQED**3)/(2._/**/REALKIND*sw**2)
    f(12) = (CI*cw**2*eQED**3)/sw**2
    f(13) = (CI*countertermnorm*eQED**3*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(14) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(15) = (CI*countertermnorm*ctVbt*eQED**3*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(16) = (CI*countertermnorm*ctVtt*eQED**3*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(17) = (CI*countertermnorm*ctVbb*cw**2*eQED**3*gQCD**2)/sw**2
    f(18) = (CI*eQED**3*MB)/(2._/**/REALKIND*sw**2)
    f(19) = (CI*countertermnorm*eQED**3*gQCD**2*MB)/(2._/**/REALKIND*sw**2)
    f(20) = (CI*countertermnorm*ctSbb*eQED**3*gQCD**2*MB)/(2._/**/REALKIND*sw**2)
    f(21) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**2*MB)/(2._/**/REALKIND*sw**2)
    f(22) = (CI*eQED**3*MW**2)/(cw**2*sw**2)
    f(23) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**2*MW**2)/(cw**2*sw**2)
    f(24) = (CI*cw*eQED**3)/(3._/**/REALKIND*sw)
    f(25) = (CI*cw*eQED**3)/sw
    f(26) = (CI*countertermnorm*cw*eQED**3*gQCD**2)/sw
    f(27) = (CI*countertermnorm*ctVbb*cw*eQED**3*gQCD**2)/(3._/**/REALKIND*sw)
    f(28) = (CI*countertermnorm*ctVbb*cw*eQED**3*gQCD**2)/sw
    f(29) = (eQED**3*gQCD**2*integralnorm*SwB)/3._/**/REALKIND
    f(30) = (cw*eQED**3*gQCD**2*integralnorm*SwB)/(sw**3*2._/**/REALKIND)
    f(31) = (eQED**3*gQCD**2*integralnorm*MB*SwB)/(cw*sw**3*2._/**/REALKIND)
    f(32) = (cw*eQED**3*gQCD**2*integralnorm*MB*SwB)/(sw**3*2._/**/REALKIND)
    f(33) = (eQED**3*gQCD**2*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(34) = (cw**2*eQED**3*gQCD**2*integralnorm*SwB)/sw**2
    f(35) = (eQED**3*gQCD**2*integralnorm*MB*SwB)/(sw**2*2._/**/REALKIND)
    f(36) = (eQED**3*gQCD**2*integralnorm*MW**2*SwB)/(cw**2*sw**2)
    f(37) = (cw*eQED**3*gQCD**2*integralnorm*SwB)/(sw*3._/**/REALKIND)
    f(38) = (cw*eQED**3*gQCD**2*integralnorm*SwB)/sw

  c = [ 4*f(29), 4*f(30), 4*f(31), 4*f(32), 4*f(33), 4*f(34), 4*f(35), 4*f(36), 4*f(37), 4*f(38) ]
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
  complex(REALKIND) :: A(70)
  ! external WFs
  call wf_Q(P(:,1), rMB, H(1), wf(:,0))
  call wf_A(P(:,2), rMB, H(2), wf(:,-1))
  call wf_V(P(:,3), rMZ, H(3), wf(:,-2))
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
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-3),Q(:,8),wf(:,16))
  call prop_W_W(wf(:,16),Q(:,24),MZ,1_intkind1,wf(:,17))
  call vert_SV_V(wf(:,5),wf(:,-2),wf(:,18))
  call vert_VV_S(wf(:,-3),wf(:,-4),wf(:,19))
  call vert_VV_S(wf(:,4),wf(:,-2),wf(:,20))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,0),wf(:,21))
  call vert_AW_Q(wf(:,-1),wf(:,-3),wf(:,22))
  call prop_Q_A(wf(:,21),Q(:,5),MB,1_intkind1,wf(:,23))
  call prop_A_Q(wf(:,22),Q(:,10),MT,1_intkind1,wf(:,24))
  call vert_WQ_A(wf(:,-4),wf(:,23),wf(:,25))
  call vert_AQ_S(gH,wf(:,-1),wf(:,23),wf(:,26))
  call vert_QA_V(wf(:,23),wf(:,-1),wf(:,27))
  call vert_QA_Z(gZd,wf(:,23),wf(:,-1),wf(:,28))
  call vert_WQ_A(wf(:,-4),wf(:,0),wf(:,29))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-2),wf(:,30))
  call prop_Q_A(wf(:,29),Q(:,17),MT,1_intkind1,wf(:,31))
  call prop_A_Q(wf(:,30),Q(:,6),MB,1_intkind1,wf(:,32))
  call vert_WQ_A(wf(:,-3),wf(:,31),wf(:,33))
  call vert_AQ_S(gH,wf(:,32),wf(:,0),wf(:,34))
  call vert_QA_V(wf(:,0),wf(:,32),wf(:,35))
  call vert_QA_Z(gZd,wf(:,0),wf(:,32),wf(:,36))
  call vert_ZQ_A(gZu,wf(:,-2),wf(:,31),wf(:,37))
  call vert_QA_W(wf(:,0),wf(:,24),wf(:,38))
  call vert_QA_W(wf(:,31),wf(:,-1),wf(:,39))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,40))
  call counter_QA_Z(gZd,wf(:,0),wf(:,-1),wf(:,41))
  call prop_W_W(wf(:,2),Q(:,28),MZ,1_intkind1,wf(:,42))
  call counter_WQ_A(wf(:,-4),wf(:,23),wf(:,43))
  call counter_WQ_A(wf(:,-3),wf(:,31),wf(:,44))
  call counter_ZQ_A(gZu,wf(:,-2),wf(:,31),wf(:,45))
  call counter_AQ_S(gH,wf(:,-1),wf(:,23),wf(:,46))
  call counter_QA_V(wf(:,23),wf(:,-1),wf(:,47))
  call counter_QA_Z(gZd,wf(:,23),wf(:,-1),wf(:,48))
  call counter_AW_Q(wf(:,-1),wf(:,-3),wf(:,49))
  call prop_A_Q(wf(:,49),Q(:,10),MT,1_intkind1,wf(:,50))
  call counter_QA_W(wf(:,31),wf(:,-1),wf(:,51))
  call vert_QA_W(wf(:,0),wf(:,50),wf(:,52))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,-2),wf(:,53))
  call prop_A_Q(wf(:,53),Q(:,6),MB,1_intkind1,wf(:,54))
  call vert_AQ_S(gH,wf(:,54),wf(:,0),wf(:,55))
  call vert_QA_V(wf(:,0),wf(:,54),wf(:,56))
  call vert_QA_Z(gZd,wf(:,0),wf(:,54),wf(:,57))
  call counter_AQ_S(gH,wf(:,32),wf(:,0),wf(:,58))
  call counter_QA_V(wf(:,0),wf(:,32),wf(:,59))
  call counter_QA_Z(gZd,wf(:,0),wf(:,32),wf(:,60))
  call counter_WQ_A(wf(:,-4),wf(:,0),wf(:,61))
  call prop_Q_A(wf(:,61),Q(:,17),MT,1_intkind1,wf(:,62))
  call vert_WQ_A(wf(:,-3),wf(:,62),wf(:,63))
  call counter_QA_W(wf(:,0),wf(:,24),wf(:,64))
  call vert_ZQ_A(gZu,wf(:,-2),wf(:,62),wf(:,65))
  call vert_QA_W(wf(:,62),wf(:,-1),wf(:,66))
  call counter_ZQ_A(gZd,wf(:,-2),wf(:,0),wf(:,67))
  call prop_Q_A(wf(:,67),Q(:,5),MB,1_intkind1,wf(:,68))
  call vert_WQ_A(wf(:,-4),wf(:,68),wf(:,69))
  call vert_AQ_S(gH,wf(:,-1),wf(:,68),wf(:,70))
  call vert_QA_V(wf(:,68),wf(:,-1),wf(:,71))
  call vert_QA_Z(gZd,wf(:,68),wf(:,-1),wf(:,72))
  call counter_AQ_S(gH,wf(:,-1),wf(:,0),wf(:,73))
  call vert_SV_V(wf(:,73),wf(:,-4),wf(:,74))
  call vert_UV_W(wf(:,40),Q(:,3),wf(:,-4),Q(:,16),wf(:,75))
  call prop_W_W(wf(:,41),Q(:,3),MZ,1_intkind1,wf(:,76))
  call vert_UV_W(wf(:,76),Q(:,3),wf(:,-4),Q(:,16),wf(:,77))
  call vert_SV_V(wf(:,73),wf(:,-3),wf(:,78))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,40),Q(:,3),wf(:,79))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,76),Q(:,3),wf(:,80))
  call vert_SV_V(wf(:,73),wf(:,-2),wf(:,81))
  call vert_VV_S(wf(:,76),wf(:,-2),wf(:,82))
  call vert_AW_Q(wf(:,24),wf(:,-4),wf(:,83))
  call counter_Q_A(ctbb,wf(:,23),Q(:,5),wf(:,84))
  call prop_A_Q(wf(:,83),Q(:,26),MB,1_intkind1,wf(:,85))
  call counter_A_Q(cttt,wf(:,24),Q(:,10),wf(:,86))
  call prop_Q_A(wf(:,25),Q(:,21),MT,1_intkind1,wf(:,87))
  call vert_SA_Q(gH,wf(:,19),wf(:,-1),wf(:,88))
  call prop_A_Q(wf(:,88),Q(:,26),MB,1_intkind1,wf(:,89))
  call vert_AV_Q(wf(:,-1),wf(:,16),wf(:,90))
  call prop_A_Q(wf(:,90),Q(:,26),MB,1_intkind1,wf(:,91))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,17),wf(:,92))
  call prop_A_Q(wf(:,92),Q(:,26),MB,1_intkind1,wf(:,93))
  call vert_AW_Q(wf(:,32),wf(:,-3),wf(:,94))
  call counter_Q_A(cttt,wf(:,31),Q(:,17),wf(:,95))
  call prop_A_Q(wf(:,94),Q(:,14),MT,1_intkind1,wf(:,96))
  call counter_A_Q(ctbb,wf(:,32),Q(:,6),wf(:,97))
  call prop_Q_A(wf(:,33),Q(:,25),MB,1_intkind1,wf(:,98))
  call vert_QS_A(gH,wf(:,0),wf(:,19),wf(:,99))
  call prop_Q_A(wf(:,99),Q(:,25),MB,1_intkind1,wf(:,100))
  call vert_VQ_A(wf(:,16),wf(:,0),wf(:,101))
  call prop_Q_A(wf(:,101),Q(:,25),MB,1_intkind1,wf(:,102))
  call vert_ZQ_A(gZd,wf(:,17),wf(:,0),wf(:,103))
  call prop_Q_A(wf(:,103),Q(:,25),MB,1_intkind1,wf(:,104))
  call vert_AZ_Q(gZu,wf(:,24),wf(:,-2),wf(:,105))
  call prop_A_Q(wf(:,105),Q(:,14),MT,1_intkind1,wf(:,106))
  call prop_Q_A(wf(:,37),Q(:,21),MT,1_intkind1,wf(:,107))
  call vert_WQ_A(wf(:,12),wf(:,0),wf(:,108))
  call prop_Q_A(wf(:,108),Q(:,21),MT,1_intkind1,wf(:,109))
  call vert_AW_Q(wf(:,-1),wf(:,7),wf(:,110))
  call prop_A_Q(wf(:,110),Q(:,14),MT,1_intkind1,wf(:,111))
  call vert_VV_S(wf(:,7),wf(:,-4),wf(:,112))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,7),Q(:,12),wf(:,113))
  call prop_W_W(wf(:,113),Q(:,28),MZ,1_intkind1,wf(:,114))
  call vert_VV_S(wf(:,-3),wf(:,12),wf(:,115))
  call vert_UV_W(wf(:,12),Q(:,20),wf(:,-3),Q(:,8),wf(:,116))
  call prop_W_W(wf(:,116),Q(:,28),MZ,1_intkind1,wf(:,117))
  call vert_SV_V(wf(:,19),wf(:,-2),wf(:,118))
  call prop_W_W(wf(:,118),Q(:,28),MZ,1_intkind1,wf(:,119))
  call vert_VV_S(wf(:,-2),wf(:,17),wf(:,120))

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
  den(12) = 1 / (Q(5,24) - MZ2)
  den(14) = 1 / (Q(5,24) - MH2)
  den(16) = 1 / (Q(5,5) - MB2)
  den(17) = 1 / (Q(5,10) - MT2)
  den(20) = 1 / (Q(5,24))
  den(23) = 1 / (Q(5,17) - MT2)
  den(24) = 1 / (Q(5,6) - MB2)
  den(32) = 1 / (Q(5,28))
  den(33) = 1 / (Q(5,28) - MZ2)
  den(34) = 1 / (Q(5,26) - MB2)
  den(37) = 1 / (Q(5,21) - MT2)
  den(46) = 1 / (Q(5,14) - MT2)
  den(49) = 1 / (Q(5,25) - MB2)
  den(66) = 1 / (Q(5,28) - MH2)

  ! denominators
  den(5) = den(3)*den(4)
  den(6) = den(1)*den(4)
  den(7) = den(2)*den(4)
  den(9) = den(3)*den(8)
  den(10) = den(1)*den(8)
  den(11) = den(2)*den(8)
  den(13) = den(3)*den(12)
  den(15) = den(2)*den(14)
  den(18) = den(16)*den(17)
  den(19) = den(14)*den(16)
  den(21) = den(16)*den(20)
  den(22) = den(12)*den(16)
  den(25) = den(23)*den(24)
  den(26) = den(14)*den(24)
  den(27) = den(20)*den(24)
  den(28) = den(12)*den(24)
  den(29) = den(17)*den(23)
  den(30) = den(8)*den(17)
  den(31) = den(4)*den(23)
  den(35) = den(17)*den(34)
  den(36) = den(16)*den(35)
  den(38) = den(16)*den(37)
  den(39) = den(17)*den(38)
  den(40) = den(14)*den(34)
  den(41) = den(16)*den(40)
  den(42) = den(20)*den(34)
  den(43) = den(16)*den(42)
  den(44) = den(12)*den(34)
  den(45) = den(16)*den(44)
  den(47) = den(24)*den(46)
  den(48) = den(23)*den(47)
  den(50) = den(23)*den(49)
  den(51) = den(24)*den(50)
  den(52) = den(14)*den(49)
  den(53) = den(24)*den(52)
  den(54) = den(20)*den(49)
  den(55) = den(24)*den(54)
  den(56) = den(12)*den(49)
  den(57) = den(24)*den(56)
  den(58) = den(17)*den(46)
  den(59) = den(23)*den(58)
  den(60) = den(23)*den(37)
  den(61) = den(17)*den(60)
  den(62) = den(8)*den(37)
  den(63) = den(17)*den(62)
  den(64) = den(4)*den(46)
  den(65) = den(23)*den(64)
  den(67) = den(4)*den(66)
  den(68) = den(4)*den(32)
  den(69) = den(4)*den(33)
  den(70) = den(8)*den(66)
  den(71) = den(8)*den(32)
  den(72) = den(8)*den(33)
  den(73) = den(14)*den(33)
  den(74) = den(12)*den(66)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(70)

  A(1) = cont_VV(wf(:,1),wf(:,2)) * den(1)
  A(2) = cont_VV(wf(:,2),wf(:,4)) * den(2)
  A(3) = cont_VV(wf(:,7),wf(:,8)) * den(5)
  A(4) = cont_VV(wf(:,7),wf(:,9)) * den(6)
  A(5) = cont_VV(wf(:,7),wf(:,10)) * den(7)
  A(6) = cont_VV(wf(:,12),wf(:,13)) * den(9)
  A(7) = cont_VV(wf(:,12),wf(:,14)) * den(10)
  A(8) = cont_VV(wf(:,12),wf(:,15)) * den(11)
  A(9) = cont_VV(wf(:,17),wf(:,18)) * den(13)
  A(10) = cont_SS(wf(:,19),wf(:,20)) * den(15)
  A(11) = cont_QA(wf(:,24),wf(:,25)) * den(18)
  A(12) = cont_SS(wf(:,19),wf(:,26)) * den(19)
  A(13) = cont_VV(wf(:,16),wf(:,27)) * den(21)
  A(14) = cont_VV(wf(:,17),wf(:,28)) * den(22)
  A(15) = cont_QA(wf(:,32),wf(:,33)) * den(25)
  A(16) = cont_SS(wf(:,19),wf(:,34)) * den(26)
  A(17) = cont_VV(wf(:,16),wf(:,35)) * den(27)
  A(18) = cont_VV(wf(:,17),wf(:,36)) * den(28)
  A(19) = cont_QA(wf(:,24),wf(:,37)) * den(29)
  A(20) = cont_VV(wf(:,12),wf(:,38)) * den(30)
  A(21) = cont_VV(wf(:,7),wf(:,39)) * den(31)

  A(22) = cont_VV(wf(:,2),wf(:,40)) * den(32)
  A(23) = cont_VV(wf(:,41),wf(:,42)) * den(33)
  A(24) = cont_QA(wf(:,24),wf(:,43)) * den(18)
  A(25) = cont_QA(wf(:,32),wf(:,44)) * den(25)
  A(26) = cont_QA(wf(:,24),wf(:,45)) * den(29)
  A(27) = cont_SS(wf(:,19),wf(:,46)) * den(19)
  A(28) = cont_VV(wf(:,16),wf(:,47)) * den(21)
  A(29) = cont_VV(wf(:,17),wf(:,48)) * den(22)
  A(30) = cont_QA(wf(:,25),wf(:,50)) * den(18)
  A(31) = cont_VV(wf(:,7),wf(:,51)) * den(31)
  A(32) = cont_QA(wf(:,37),wf(:,50)) * den(29)
  A(33) = cont_VV(wf(:,12),wf(:,52)) * den(30)
  A(34) = cont_QA(wf(:,33),wf(:,54)) * den(25)
  A(35) = cont_SS(wf(:,19),wf(:,55)) * den(26)
  A(36) = cont_VV(wf(:,16),wf(:,56)) * den(27)
  A(37) = cont_VV(wf(:,17),wf(:,57)) * den(28)
  A(38) = cont_SS(wf(:,19),wf(:,58)) * den(26)
  A(39) = cont_VV(wf(:,16),wf(:,59)) * den(27)
  A(40) = cont_VV(wf(:,17),wf(:,60)) * den(28)
  A(41) = cont_QA(wf(:,32),wf(:,63)) * den(25)
  A(42) = cont_VV(wf(:,12),wf(:,64)) * den(30)
  A(43) = cont_QA(wf(:,24),wf(:,65)) * den(29)
  A(44) = cont_VV(wf(:,7),wf(:,66)) * den(31)
  A(45) = cont_QA(wf(:,24),wf(:,69)) * den(18)
  A(46) = cont_SS(wf(:,19),wf(:,70)) * den(19)
  A(47) = cont_VV(wf(:,16),wf(:,71)) * den(21)
  A(48) = cont_VV(wf(:,17),wf(:,72)) * den(22)
  A(49) = cont_VV(wf(:,7),wf(:,74)) * den(5)
  A(50) = cont_VV(wf(:,7),wf(:,75)) * den(6)
  A(51) = cont_VV(wf(:,7),wf(:,77)) * den(7)
  A(52) = cont_VV(wf(:,12),wf(:,78)) * den(9)
  A(53) = cont_VV(wf(:,12),wf(:,79)) * den(10)
  A(54) = cont_VV(wf(:,12),wf(:,80)) * den(11)
  A(55) = cont_VV(wf(:,17),wf(:,81)) * den(13)
  A(56) = cont_SS(wf(:,19),wf(:,82)) * den(15)
  A(57) = cont_QA(wf(:,84),wf(:,85)) * den(36)
  A(58) = cont_QA(wf(:,86),wf(:,87)) * den(39)
  A(59) = cont_QA(wf(:,84),wf(:,89)) * den(41)
  A(60) = cont_QA(wf(:,84),wf(:,91)) * den(43)
  A(61) = cont_QA(wf(:,84),wf(:,93)) * den(45)
  A(62) = cont_QA(wf(:,95),wf(:,96)) * den(48)
  A(63) = cont_QA(wf(:,97),wf(:,98)) * den(51)
  A(64) = cont_QA(wf(:,97),wf(:,100)) * den(53)
  A(65) = cont_QA(wf(:,97),wf(:,102)) * den(55)
  A(66) = cont_QA(wf(:,97),wf(:,104)) * den(57)
  A(67) = cont_QA(wf(:,95),wf(:,106)) * den(59)
  A(68) = cont_QA(wf(:,86),wf(:,107)) * den(61)
  A(69) = cont_QA(wf(:,86),wf(:,109)) * den(63)
  A(70) = cont_QA(wf(:,95),wf(:,111)) * den(65)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(70)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(13)+A(17))*f(1)+(-A(20)-A(21))*f(4)+A(9)*f(7)+(A(3)+A(6))*f(8)+(-A(11)-A(15)-A(19))*f(11)+(A(2)-A(5)-A(8))*f(12) &
       +(A(12)+A(16))*f(18)+A(10)*f(22)+(-A(1)+A(4)+A(7))*f(24)+(-A(14)-A(18))*f(25)

  M2(1) = (-A(60)-A(65))*f(2)+(A(28)+A(36)+A(39)+A(47))*f(3)+(A(69)+A(70))*f(5)+(-A(31)-A(33)-A(42)-A(44))*f(6)+A(55)*f(9)+(A(49) &
       +A(52))*f(10)+(A(57)+A(58)+A(62)+A(63)+A(67)+A(68))*f(13)+(-A(34)-A(45))*f(14)+(-A(24)-A(25)-A(30)-A(32)-A(41)-A(43))*f(15) &
       -A(26)*f(16)+(A(23)-A(51)-A(54))*f(17)+(-A(59)-A(64))*f(19)+(A(27)+A(38))*f(20)+(A(35)+A(46))*f(21)+A(56)*f(23)+(A(61) &
       +A(66))*f(26)+(-A(22)+A(50)+A(53))*f(27)+(-A(29)-A(37)-A(40)-A(48))*f(28)

end subroutine colourvectors

end module ol_loop_ppvvv_bbxzwwx_1_/**/REALKIND
