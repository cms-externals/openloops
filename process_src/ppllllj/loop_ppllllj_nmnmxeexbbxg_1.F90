
module ol_colourmatrix_ppllllj_nmnmxeexbbxg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllllj_nmnmxeexbbxg_1_/**/REALKIND



module ol_forced_parameters_ppllllj_nmnmxeexbbxg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj_nmnmxeexbbxg_1_/**/REALKIND

module ol_loop_ppllllj_nmnmxeexbbxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(30), c(16)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:202)
  ! denominators
  complex(REALKIND), save :: den(277)
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
    f( 1) = (CI*eQED**4*gQCD)/3._/**/REALKIND
    f( 2) = CI*eQED**4*gQCD
    f( 3) = (CI*countertermnorm*eQED**4*gQCD**3)/3._/**/REALKIND
    f( 4) = CI*countertermnorm*eQED**4*gQCD**3
    f( 5) = CI*countertermnorm*ctAZGG*eQED**4*gQCD**3
    f( 6) = (CI*countertermnorm*ctGbb*eQED**4*gQCD**3)/3._/**/REALKIND
    f( 7) = CI*countertermnorm*ctGbb*eQED**4*gQCD**3
    f( 8) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**3)/3._/**/REALKIND
    f( 9) = CI*countertermnorm*ctVbb*eQED**4*gQCD**3
    f(10) = (countertermnorm*ctZGG*eQED**4*gQCD**3)/3._/**/REALKIND
    f(11) = countertermnorm*ctZGG*eQED**4*gQCD**3
    f(12) = CI*countertermnorm*ctZZGG*eQED**4*gQCD**3
    f(13) = (CI*eQED**4*gQCD*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(14) = (CI*countertermnorm*eQED**4*gQCD**3*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(15) = (CI*countertermnorm*ctGbb*eQED**4*gQCD**3*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(16) = (CI*countertermnorm*ctSbb*eQED**4*gQCD**3*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(17) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MW)/(cw**2*sw)
    f(18) = (CI*eQED**4*gQCD**3*integralnorm*SwB)/3._/**/REALKIND
    f(19) = CI*eQED**4*gQCD**3*integralnorm*SwB
    f(20) = (eQED**4*gQCD**3*integralnorm*SwB)/3._/**/REALKIND
    f(21) = eQED**4*gQCD**3*integralnorm*SwB
    f(22) = (CI*eQED**4*gQCD**3*integralnorm*MB*SwB)/(2._/**/REALKIND*cw**2*sw**2)
    f(23) = (eQED**4*gQCD**3*integralnorm*MB*SwB)/(cw**2*sw**2*2._/**/REALKIND)
    f(24) = (eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(25) = (2*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(26) = eQED**4*gQCD**3*integralnorm*SwF
    f(27) = (4*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(28) = 2*eQED**4*gQCD**3*integralnorm*SwF
    f(29) = (eQED**4*gQCD**3*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(30) = (eQED**4*gQCD**3*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)

  c = [ 9*CI*f(18), 9*CI*f(19), f(20), 8*f(20), f(21), 8*f(21), 9*CI*f(22), f(23), 8*f(23), 3*f(24), 3*f(25), 3*f(26), 3*f(27) &
    , 3*f(28), 3*f(29), 3*f(30) ]
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
  complex(REALKIND) :: A(141)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rMB, H(5), wf(:,-4))
  call wf_A(P(:,6), rMB, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_VQ_A(wf(:,-6),wf(:,-4),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call prop_Q_A(wf(:,3),Q(:,80),MB,1_intkind1,wf(:,5))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,6))
  call vert_VQ_A(wf(:,2),wf(:,5),wf(:,7))
  call prop_A_Q(wf(:,6),Q(:,35),MB,1_intkind1,wf(:,8))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,-3),wf(:,9))
  call prop_W_W(wf(:,9),Q(:,12),MZ,1_intkind1,wf(:,10))
  call vert_ZQ_A(gZd,wf(:,10),wf(:,5),wf(:,11))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,12))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,5),wf(:,13))
  call prop_A_Q(wf(:,12),Q(:,44),MB,1_intkind1,wf(:,14))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,10),wf(:,15))
  call prop_A_Q(wf(:,15),Q(:,44),MB,1_intkind1,wf(:,16))
  call vert_AQ_S(gH,wf(:,-5),wf(:,5),wf(:,17))
  call vert_VV_S(wf(:,4),wf(:,10),wf(:,18))
  call vert_AV_Q(wf(:,-5),wf(:,-6),wf(:,19))
  call prop_A_Q(wf(:,19),Q(:,96),MB,1_intkind1,wf(:,20))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,21))
  call vert_AV_Q(wf(:,20),wf(:,2),wf(:,22))
  call prop_Q_A(wf(:,21),Q(:,19),MB,1_intkind1,wf(:,23))
  call vert_AZ_Q(gZd,wf(:,20),wf(:,10),wf(:,24))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,25))
  call vert_AZ_Q(gZd,wf(:,20),wf(:,4),wf(:,26))
  call prop_Q_A(wf(:,25),Q(:,28),MB,1_intkind1,wf(:,27))
  call vert_ZQ_A(gZd,wf(:,10),wf(:,-4),wf(:,28))
  call prop_Q_A(wf(:,28),Q(:,28),MB,1_intkind1,wf(:,29))
  call vert_AQ_S(gH,wf(:,20),wf(:,-4),wf(:,30))
  call vert_VQ_A(wf(:,-6),wf(:,23),wf(:,31))
  call vert_VQ_A(wf(:,-6),wf(:,27),wf(:,32))
  call vert_VQ_A(wf(:,-6),wf(:,29),wf(:,33))
  call vert_ZQ_A(gZl,wf(:,4),wf(:,-2),wf(:,34))
  call vert_QA_V(wf(:,5),wf(:,-5),wf(:,35))
  call prop_Q_A(wf(:,34),Q(:,7),ZERO,0_intkind1,wf(:,36))
  call vert_QA_V(wf(:,36),wf(:,-3),wf(:,37))
  call vert_QA_Z(gZd,wf(:,5),wf(:,-5),wf(:,38))
  call prop_W_W(wf(:,38),Q(:,112),MZ,1_intkind1,wf(:,39))
  call vert_QA_Z(gZl,wf(:,36),wf(:,-3),wf(:,40))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,4),wf(:,41))
  call prop_A_Q(wf(:,41),Q(:,11),ZERO,0_intkind1,wf(:,42))
  call vert_QA_V(wf(:,-2),wf(:,42),wf(:,43))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,42),wf(:,44))
  call vert_QA_V(wf(:,-4),wf(:,20),wf(:,45))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,20),wf(:,46))
  call prop_W_W(wf(:,46),Q(:,112),MZ,1_intkind1,wf(:,47))
  call vert_ZQ_A(gZn,wf(:,10),wf(:,0),wf(:,48))
  call prop_Q_A(wf(:,48),Q(:,13),ZERO,0_intkind1,wf(:,49))
  call vert_QA_Z(gZn,wf(:,49),wf(:,-1),wf(:,50))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,10),wf(:,51))
  call prop_A_Q(wf(:,51),Q(:,14),ZERO,0_intkind1,wf(:,52))
  call vert_QA_Z(gZn,wf(:,0),wf(:,52),wf(:,53))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,54))
  call counter_VVG_G(wf(:,4),wf(:,2),wf(:,-6),wf(:,55))
  call counter_VVG_G(wf(:,4),wf(:,10),wf(:,-6),wf(:,56))
  call counter_GG_S(wf(:,54),wf(:,-6),wf(:,57))
  call counter_VQ_A(wf(:,2),wf(:,5),wf(:,58))
  call counter_ZQ_A(gZd,wf(:,10),wf(:,5),wf(:,59))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,5),wf(:,60))
  call counter_AV_Q(wf(:,20),wf(:,2),wf(:,61))
  call counter_AZ_Q(gZd,wf(:,20),wf(:,10),wf(:,62))
  call counter_AZ_Q(gZd,wf(:,20),wf(:,4),wf(:,63))
  call counter_VQ_A(wf(:,-6),wf(:,23),wf(:,64))
  call counter_VG_G(wf(:,10),wf(:,-6),Q(:,64),wf(:,65),Q(:,76))
  call vert_QA_V(wf(:,23),wf(:,-5),wf(:,66))
  call counter_VQ_A(wf(:,-6),wf(:,27),wf(:,67))
  call counter_VQ_A(wf(:,-6),wf(:,29),wf(:,68))
  call vert_QA_V(wf(:,-4),wf(:,8),wf(:,69))
  call counter_VG_G(wf(:,4),wf(:,-6),Q(:,64),wf(:,70),Q(:,67))
  call vert_QA_V(wf(:,27),wf(:,-5),wf(:,71))
  call vert_QA_V(wf(:,29),wf(:,-5),wf(:,72))
  call vert_QA_V(wf(:,-4),wf(:,14),wf(:,73))
  call vert_QA_V(wf(:,-4),wf(:,16),wf(:,74))
  call counter_AQ_S(gH,wf(:,-5),wf(:,5),wf(:,75))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,76))
  call prop_Q_A(wf(:,13),Q(:,83),MB,1_intkind1,wf(:,77))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,10),wf(:,78))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,79))
  call prop_Q_A(wf(:,7),Q(:,92),MB,1_intkind1,wf(:,80))
  call prop_Q_A(wf(:,11),Q(:,92),MB,1_intkind1,wf(:,81))
  call prop_A_Q(wf(:,76),Q(:,44),MB,1_intkind1,wf(:,82))
  call prop_A_Q(wf(:,78),Q(:,44),MB,1_intkind1,wf(:,83))
  call prop_A_Q(wf(:,79),Q(:,35),MB,1_intkind1,wf(:,84))
  call counter_AV_Q(wf(:,-5),wf(:,-6),wf(:,85))
  call prop_A_Q(wf(:,85),Q(:,96),MB,1_intkind1,wf(:,86))
  call vert_AV_Q(wf(:,86),wf(:,2),wf(:,87))
  call vert_AZ_Q(gZd,wf(:,86),wf(:,10),wf(:,88))
  call vert_AZ_Q(gZd,wf(:,86),wf(:,4),wf(:,89))
  call vert_AQ_S(gH,wf(:,86),wf(:,-4),wf(:,90))
  call counter_AQ_S(gH,wf(:,20),wf(:,-4),wf(:,91))
  call counter_VQ_A(wf(:,2),wf(:,-4),wf(:,92))
  call prop_A_Q(wf(:,26),Q(:,99),MB,1_intkind1,wf(:,93))
  call counter_ZQ_A(gZd,wf(:,10),wf(:,-4),wf(:,94))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,95))
  call prop_A_Q(wf(:,22),Q(:,108),MB,1_intkind1,wf(:,96))
  call prop_A_Q(wf(:,24),Q(:,108),MB,1_intkind1,wf(:,97))
  call prop_Q_A(wf(:,92),Q(:,28),MB,1_intkind1,wf(:,98))
  call vert_VQ_A(wf(:,-6),wf(:,98),wf(:,99))
  call prop_Q_A(wf(:,94),Q(:,28),MB,1_intkind1,wf(:,100))
  call vert_VQ_A(wf(:,-6),wf(:,100),wf(:,101))
  call prop_Q_A(wf(:,95),Q(:,19),MB,1_intkind1,wf(:,102))
  call vert_VQ_A(wf(:,-6),wf(:,102),wf(:,103))
  call counter_VQ_A(wf(:,-6),wf(:,-4),wf(:,104))
  call prop_Q_A(wf(:,104),Q(:,80),MB,1_intkind1,wf(:,105))
  call vert_VQ_A(wf(:,2),wf(:,105),wf(:,106))
  call vert_ZQ_A(gZd,wf(:,10),wf(:,105),wf(:,107))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,105),wf(:,108))
  call vert_AQ_S(gH,wf(:,-5),wf(:,105),wf(:,109))
  call counter_GG_V(wf(:,54),Q(:,48),wf(:,-6),Q(:,64),wf(:,110))
  call prop_W_W(wf(:,110),Q(:,112),MZ,1_intkind1,wf(:,111))
  call counter_QA_V(wf(:,5),wf(:,-5),wf(:,112))
  call counter_QA_Z(gZd,wf(:,5),wf(:,-5),wf(:,113))
  call prop_W_W(wf(:,113),Q(:,112),MZ,1_intkind1,wf(:,114))
  call vert_QA_V(wf(:,-4),wf(:,86),wf(:,115))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,86),wf(:,116))
  call prop_W_W(wf(:,116),Q(:,112),MZ,1_intkind1,wf(:,117))
  call counter_QA_V(wf(:,-4),wf(:,20),wf(:,118))
  call counter_QA_Z(gZd,wf(:,-4),wf(:,20),wf(:,119))
  call prop_W_W(wf(:,119),Q(:,112),MZ,1_intkind1,wf(:,120))
  call vert_QA_V(wf(:,105),wf(:,-5),wf(:,121))
  call vert_QA_Z(gZd,wf(:,105),wf(:,-5),wf(:,122))
  call prop_W_W(wf(:,122),Q(:,112),MZ,1_intkind1,wf(:,123))
  call counter_Q_A(ctbb,wf(:,5),Q(:,80),wf(:,124))
  call prop_Q_A(wf(:,124),Q(:,80),MB,1_intkind1,wf(:,125))
  call vert_AQ_S(gH,wf(:,-5),wf(:,125),wf(:,126))
  call vert_VQ_A(wf(:,2),wf(:,125),wf(:,127))
  call vert_ZQ_A(gZd,wf(:,10),wf(:,125),wf(:,128))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,125),wf(:,129))
  call counter_A_Q(ctbb,wf(:,8),Q(:,35),wf(:,130))
  call counter_A_Q(ctbb,wf(:,14),Q(:,44),wf(:,131))
  call counter_A_Q(ctbb,wf(:,16),Q(:,44),wf(:,132))
  call counter_A_Q(ctbb,wf(:,20),Q(:,96),wf(:,133))
  call prop_A_Q(wf(:,133),Q(:,96),MB,1_intkind1,wf(:,134))
  call vert_AQ_S(gH,wf(:,134),wf(:,-4),wf(:,135))
  call counter_Q_A(ctbb,wf(:,23),Q(:,19),wf(:,136))
  call counter_Q_A(ctbb,wf(:,27),Q(:,28),wf(:,137))
  call counter_Q_A(ctbb,wf(:,29),Q(:,28),wf(:,138))
  call vert_AV_Q(wf(:,134),wf(:,2),wf(:,139))
  call vert_AZ_Q(gZd,wf(:,134),wf(:,10),wf(:,140))
  call vert_AZ_Q(gZd,wf(:,134),wf(:,4),wf(:,141))
  call vert_AV_Q(wf(:,14),wf(:,-6),wf(:,142))
  call prop_A_Q(wf(:,142),Q(:,108),MB,1_intkind1,wf(:,143))
  call vert_AV_Q(wf(:,16),wf(:,-6),wf(:,144))
  call prop_A_Q(wf(:,144),Q(:,108),MB,1_intkind1,wf(:,145))
  call vert_AV_Q(wf(:,8),wf(:,-6),wf(:,146))
  call prop_A_Q(wf(:,146),Q(:,99),MB,1_intkind1,wf(:,147))
  call prop_Q_A(wf(:,31),Q(:,83),MB,1_intkind1,wf(:,148))
  call prop_Q_A(wf(:,32),Q(:,92),MB,1_intkind1,wf(:,149))
  call prop_Q_A(wf(:,33),Q(:,92),MB,1_intkind1,wf(:,150))
  call vert_QA_V(wf(:,125),wf(:,-5),wf(:,151))
  call vert_QA_Z(gZd,wf(:,125),wf(:,-5),wf(:,152))
  call prop_W_W(wf(:,40),Q(:,15),MZ,1_intkind1,wf(:,153))
  call prop_W_W(wf(:,44),Q(:,15),MZ,1_intkind1,wf(:,154))
  call vert_QA_V(wf(:,-4),wf(:,134),wf(:,155))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,134),wf(:,156))
  call prop_W_W(wf(:,50),Q(:,15),MZ,1_intkind1,wf(:,157))
  call prop_W_W(wf(:,53),Q(:,15),MZ,1_intkind1,wf(:,158))
  call vert_VQ_A(wf(:,2),wf(:,23),wf(:,159))
  call prop_Q_A(wf(:,159),Q(:,31),MB,1_intkind1,wf(:,160))
  call vert_ZQ_A(gZd,wf(:,10),wf(:,23),wf(:,161))
  call prop_Q_A(wf(:,161),Q(:,31),MB,1_intkind1,wf(:,162))
  call vert_AV_Q(wf(:,8),wf(:,2),wf(:,163))
  call prop_A_Q(wf(:,163),Q(:,47),MB,1_intkind1,wf(:,164))
  call vert_AZ_Q(gZd,wf(:,8),wf(:,10),wf(:,165))
  call prop_A_Q(wf(:,165),Q(:,47),MB,1_intkind1,wf(:,166))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,27),wf(:,167))
  call prop_Q_A(wf(:,167),Q(:,31),MB,1_intkind1,wf(:,168))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,29),wf(:,169))
  call prop_Q_A(wf(:,169),Q(:,31),MB,1_intkind1,wf(:,170))
  call vert_AZ_Q(gZd,wf(:,14),wf(:,4),wf(:,171))
  call prop_A_Q(wf(:,171),Q(:,47),MB,1_intkind1,wf(:,172))
  call vert_AZ_Q(gZd,wf(:,16),wf(:,4),wf(:,173))
  call prop_A_Q(wf(:,173),Q(:,47),MB,1_intkind1,wf(:,174))
  call vert_QS_A(gH,wf(:,-4),wf(:,18),wf(:,175))
  call prop_Q_A(wf(:,175),Q(:,31),MB,1_intkind1,wf(:,176))
  call vert_SA_Q(gH,wf(:,18),wf(:,-5),wf(:,177))
  call prop_A_Q(wf(:,177),Q(:,47),MB,1_intkind1,wf(:,178))
  call vert_VQ_A(wf(:,37),wf(:,-4),wf(:,179))
  call prop_Q_A(wf(:,179),Q(:,31),MB,1_intkind1,wf(:,180))
  call vert_ZQ_A(gZd,wf(:,153),wf(:,-4),wf(:,181))
  call prop_Q_A(wf(:,181),Q(:,31),MB,1_intkind1,wf(:,182))
  call vert_AV_Q(wf(:,-5),wf(:,37),wf(:,183))
  call prop_A_Q(wf(:,183),Q(:,47),MB,1_intkind1,wf(:,184))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,153),wf(:,185))
  call prop_A_Q(wf(:,185),Q(:,47),MB,1_intkind1,wf(:,186))
  call vert_VQ_A(wf(:,43),wf(:,-4),wf(:,187))
  call prop_Q_A(wf(:,187),Q(:,31),MB,1_intkind1,wf(:,188))
  call vert_ZQ_A(gZd,wf(:,154),wf(:,-4),wf(:,189))
  call prop_Q_A(wf(:,189),Q(:,31),MB,1_intkind1,wf(:,190))
  call vert_AV_Q(wf(:,-5),wf(:,43),wf(:,191))
  call prop_A_Q(wf(:,191),Q(:,47),MB,1_intkind1,wf(:,192))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,154),wf(:,193))
  call prop_A_Q(wf(:,193),Q(:,47),MB,1_intkind1,wf(:,194))
  call vert_ZQ_A(gZd,wf(:,157),wf(:,-4),wf(:,195))
  call prop_Q_A(wf(:,195),Q(:,31),MB,1_intkind1,wf(:,196))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,157),wf(:,197))
  call prop_A_Q(wf(:,197),Q(:,47),MB,1_intkind1,wf(:,198))
  call vert_ZQ_A(gZd,wf(:,158),wf(:,-4),wf(:,199))
  call prop_Q_A(wf(:,199),Q(:,31),MB,1_intkind1,wf(:,200))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,158),wf(:,201))
  call prop_A_Q(wf(:,201),Q(:,47),MB,1_intkind1,wf(:,202))

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
  den(2) = 1 / (Q(5,12))
  den(3) = 1 / (Q(5,80) - MB2)
  den(5) = 1 / (Q(5,35) - MB2)
  den(8) = 1 / (Q(5,12) - MZ2)
  den(12) = 1 / (Q(5,44) - MB2)
  den(18) = 1 / (Q(5,112) - MH2)
  den(21) = 1 / (Q(5,96) - MB2)
  den(23) = 1 / (Q(5,19) - MB2)
  den(29) = 1 / (Q(5,28) - MB2)
  den(40) = 1 / (Q(5,7))
  den(42) = 1 / (Q(5,112))
  den(45) = 1 / (Q(5,112) - MZ2)
  den(48) = 1 / (Q(5,11))
  den(58) = 1 / (Q(5,13))
  den(61) = 1 / (Q(5,14))
  den(66) = 1 / (Q(5,48))
  den(70) = 1 / (Q(5,15) - MH2)
  den(73) = 1 / (Q(5,76))
  den(77) = 1 / (Q(5,67))
  den(84) = 1 / (Q(5,83) - MB2)
  den(88) = 1 / (Q(5,92) - MB2)
  den(94) = 1 / (Q(5,99) - MB2)
  den(98) = 1 / (Q(5,108) - MB2)
  den(148) = 1 / (Q(5,15))
  den(151) = 1 / (Q(5,15) - MZ2)
  den(168) = 1 / (Q(5,51))
  den(171) = 1 / (Q(5,31) - MB2)
  den(177) = 1 / (Q(5,47) - MB2)
  den(181) = 1 / (Q(5,60))

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
  den(17) = den(1)*den(8)
  den(19) = den(3)*den(18)
  den(20) = den(17)*den(19)
  den(22) = den(2)*den(21)
  den(24) = den(1)*den(23)
  den(25) = den(22)*den(24)
  den(26) = den(8)*den(21)
  den(27) = den(24)*den(26)
  den(28) = den(1)*den(21)
  den(30) = den(2)*den(29)
  den(31) = den(28)*den(30)
  den(32) = den(8)*den(29)
  den(33) = den(28)*den(32)
  den(34) = den(18)*den(21)
  den(35) = den(17)*den(34)
  den(36) = den(13)*den(24)
  den(37) = den(15)*den(24)
  den(38) = den(6)*den(30)
  den(39) = den(6)*den(32)
  den(41) = den(1)*den(40)
  den(43) = den(3)*den(42)
  den(44) = den(41)*den(43)
  den(46) = den(3)*den(45)
  den(47) = den(41)*den(46)
  den(49) = den(1)*den(48)
  den(50) = den(43)*den(49)
  den(51) = den(46)*den(49)
  den(52) = den(21)*den(42)
  den(53) = den(41)*den(52)
  den(54) = den(21)*den(45)
  den(55) = den(41)*den(54)
  den(56) = den(49)*den(52)
  den(57) = den(49)*den(54)
  den(59) = den(8)*den(58)
  den(60) = den(46)*den(59)
  den(62) = den(8)*den(61)
  den(63) = den(46)*den(62)
  den(64) = den(54)*den(59)
  den(65) = den(54)*den(62)
  den(67) = den(1)*den(2)
  den(68) = den(66)*den(67)
  den(69) = den(17)*den(66)
  den(71) = den(17)*den(70)
  den(72) = den(66)*den(71)
  den(74) = den(8)*den(73)
  den(75) = den(24)*den(74)
  den(76) = den(6)*den(74)
  den(78) = den(1)*den(77)
  den(79) = den(30)*den(78)
  den(80) = den(32)*den(78)
  den(81) = den(13)*den(78)
  den(82) = den(15)*den(78)
  den(83) = den(3)*den(71)
  den(85) = den(11)*den(84)
  den(86) = den(2)*den(85)
  den(87) = den(8)*den(85)
  den(89) = den(4)*den(88)
  den(90) = den(1)*den(89)
  den(91) = den(9)*den(88)
  den(92) = den(1)*den(91)
  den(93) = den(21)*den(71)
  den(95) = den(28)*den(94)
  den(96) = den(2)*den(95)
  den(97) = den(8)*den(95)
  den(99) = den(22)*den(98)
  den(100) = den(1)*den(99)
  den(101) = den(26)*den(98)
  den(102) = den(1)*den(101)
  den(103) = den(45)*den(66)
  den(104) = den(41)*den(103)
  den(105) = den(49)*den(103)
  den(106) = den(59)*den(103)
  den(107) = den(62)*den(103)
  den(108) = den(3)**2
  den(109) = den(71)*den(108)
  den(110) = den(2)*den(108)
  den(111) = den(6)*den(110)
  den(112) = den(8)*den(108)
  den(113) = den(6)*den(112)
  den(114) = den(1)*den(108)
  den(115) = den(13)*den(114)
  den(116) = den(15)*den(114)
  den(117) = den(6)*den(89)
  den(118) = den(6)*den(91)
  den(119) = den(13)*den(85)
  den(120) = den(15)*den(85)
  den(121) = den(21)**2
  den(122) = den(71)*den(121)
  den(123) = den(24)*den(99)
  den(124) = den(24)*den(101)
  den(125) = den(30)*den(95)
  den(126) = den(32)*den(95)
  den(127) = den(2)*den(121)
  den(128) = den(24)*den(127)
  den(129) = den(8)*den(121)
  den(130) = den(24)*den(129)
  den(131) = den(1)*den(121)
  den(132) = den(30)*den(131)
  den(133) = den(32)*den(131)
  den(134) = den(13)*den(98)
  den(135) = den(24)*den(134)
  den(136) = den(15)*den(98)
  den(137) = den(24)*den(136)
  den(138) = den(6)*den(94)
  den(139) = den(30)*den(138)
  den(140) = den(32)*den(138)
  den(141) = den(24)*den(84)
  den(142) = den(13)*den(141)
  den(143) = den(15)*den(141)
  den(144) = den(30)*den(88)
  den(145) = den(6)*den(144)
  den(146) = den(32)*den(88)
  den(147) = den(6)*den(146)
  den(149) = den(41)*den(148)
  den(150) = den(108)*den(149)
  den(152) = den(41)*den(151)
  den(153) = den(108)*den(152)
  den(154) = den(49)*den(148)
  den(155) = den(108)*den(154)
  den(156) = den(49)*den(151)
  den(157) = den(108)*den(156)
  den(158) = den(121)*den(149)
  den(159) = den(121)*den(152)
  den(160) = den(121)*den(154)
  den(161) = den(121)*den(156)
  den(162) = den(59)*den(151)
  den(163) = den(108)*den(162)
  den(164) = den(62)*den(151)
  den(165) = den(108)*den(164)
  den(166) = den(121)*den(162)
  den(167) = den(121)*den(164)
  den(169) = den(24)*den(168)
  den(170) = den(2)*den(24)
  den(172) = den(170)*den(171)
  den(173) = den(8)*den(24)
  den(174) = den(171)*den(173)
  den(175) = den(6)*den(168)
  den(176) = den(2)*den(6)
  den(178) = den(176)*den(177)
  den(179) = den(6)*den(8)
  den(180) = den(177)*den(179)
  den(182) = den(30)*den(181)
  den(183) = den(32)*den(181)
  den(184) = den(1)*den(30)
  den(185) = den(171)*den(184)
  den(186) = den(1)*den(32)
  den(187) = den(171)*den(186)
  den(188) = den(13)*den(181)
  den(189) = den(15)*den(181)
  den(190) = den(1)*den(13)
  den(191) = den(177)*den(190)
  den(192) = den(1)*den(15)
  den(193) = den(177)*den(192)
  den(194) = den(71)*den(171)
  den(195) = den(71)*den(177)
  den(196) = den(149)*den(171)
  den(197) = den(152)*den(171)
  den(198) = den(149)*den(177)
  den(199) = den(152)*den(177)
  den(200) = den(154)*den(171)
  den(201) = den(156)*den(171)
  den(202) = den(154)*den(177)
  den(203) = den(156)*den(177)
  den(204) = den(162)*den(171)
  den(205) = den(162)*den(177)
  den(206) = den(164)*den(171)
  den(207) = den(164)*den(177)
  den(208) = den(1)*den(2)*den(66)
  den(209) = den(1)*den(8)*den(66)
  den(210) = den(2)*den(3)*den(6)
  den(211) = den(3)*den(6)*den(8)
  den(212) = den(1)*den(3)*den(13)
  den(213) = den(1)*den(3)*den(15)
  den(214) = den(1)*den(2)*den(3)
  den(215) = den(1)*den(3)*den(8)
  den(216) = den(2)*den(21)*den(24)
  den(217) = den(8)*den(21)*den(24)
  den(218) = den(1)*den(21)*den(30)
  den(219) = den(1)*den(21)*den(32)
  den(220) = den(1)*den(2)*den(21)
  den(221) = den(1)*den(8)*den(21)
  den(222) = den(2)*den(169)
  den(223) = den(8)*den(169)
  den(224) = den(2)*den(141)
  den(225) = den(8)*den(141)
  den(226) = den(2)*den(175)
  den(227) = den(8)*den(175)
  den(228) = den(2)*den(138)
  den(229) = den(8)*den(138)
  den(230) = den(1)*den(182)
  den(231) = den(1)*den(183)
  den(232) = den(1)*den(144)
  den(233) = den(1)*den(146)
  den(234) = den(1)*den(188)
  den(235) = den(1)*den(189)
  den(236) = den(1)*den(134)
  den(237) = den(1)*den(136)
  den(238) = den(66)*den(149)
  den(239) = den(66)*den(152)
  den(240) = den(66)*den(154)
  den(241) = den(66)*den(156)
  den(242) = den(3)*den(149)
  den(243) = den(3)*den(152)
  den(244) = den(3)*den(154)
  den(245) = den(3)*den(156)
  den(246) = den(21)*den(149)
  den(247) = den(21)*den(152)
  den(248) = den(21)*den(154)
  den(249) = den(21)*den(156)
  den(250) = den(66)*den(162)
  den(251) = den(66)*den(164)
  den(252) = den(3)*den(162)
  den(253) = den(3)*den(164)
  den(254) = den(21)*den(162)
  den(255) = den(21)*den(164)
  den(256) = den(3)*den(178)
  den(257) = den(3)*den(180)
  den(258) = den(3)*den(191)
  den(259) = den(3)*den(193)
  den(260) = den(3)*den(195)
  den(261) = den(21)*den(172)
  den(262) = den(21)*den(174)
  den(263) = den(21)*den(185)
  den(264) = den(21)*den(187)
  den(265) = den(21)*den(194)
  den(266) = den(3)*den(198)
  den(267) = den(3)*den(199)
  den(268) = den(3)*den(202)
  den(269) = den(3)*den(203)
  den(270) = den(21)*den(196)
  den(271) = den(21)*den(197)
  den(272) = den(21)*den(200)
  den(273) = den(21)*den(201)
  den(274) = den(3)*den(205)
  den(275) = den(3)*den(207)
  den(276) = den(21)*den(204)
  den(277) = den(21)*den(206)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(141)

  A(1) = cont_QA(wf(:,7),wf(:,8)) * den(7)
  A(2) = cont_QA(wf(:,8),wf(:,11)) * den(10)
  A(3) = cont_QA(wf(:,13),wf(:,14)) * den(14)
  A(4) = cont_QA(wf(:,13),wf(:,16)) * den(16)
  A(5) = cont_SS(wf(:,17),wf(:,18)) * den(20)
  A(6) = cont_QA(wf(:,22),wf(:,23)) * den(25)
  A(7) = cont_QA(wf(:,23),wf(:,24)) * den(27)
  A(8) = cont_QA(wf(:,26),wf(:,27)) * den(31)
  A(9) = cont_QA(wf(:,26),wf(:,29)) * den(33)
  A(10) = cont_SS(wf(:,18),wf(:,30)) * den(35)
  A(11) = cont_QA(wf(:,14),wf(:,31)) * den(36)
  A(12) = cont_QA(wf(:,16),wf(:,31)) * den(37)
  A(13) = cont_QA(wf(:,8),wf(:,32)) * den(38)
  A(14) = cont_QA(wf(:,8),wf(:,33)) * den(39)
  A(15) = cont_VV(wf(:,35),wf(:,37)) * den(44)
  A(16) = cont_VV(wf(:,39),wf(:,40)) * den(47)
  A(17) = cont_VV(wf(:,35),wf(:,43)) * den(50)
  A(18) = cont_VV(wf(:,39),wf(:,44)) * den(51)
  A(19) = cont_VV(wf(:,37),wf(:,45)) * den(53)
  A(20) = cont_VV(wf(:,40),wf(:,47)) * den(55)
  A(21) = cont_VV(wf(:,43),wf(:,45)) * den(56)
  A(22) = cont_VV(wf(:,44),wf(:,47)) * den(57)
  A(23) = cont_VV(wf(:,39),wf(:,50)) * den(60)
  A(24) = cont_VV(wf(:,39),wf(:,53)) * den(63)
  A(25) = cont_VV(wf(:,47),wf(:,50)) * den(64)
  A(26) = cont_VV(wf(:,47),wf(:,53)) * den(65)

  A(27) = cont_VV(wf(:,54),wf(:,55)) * den(68)
  A(28) = cont_VV(wf(:,54),wf(:,56)) * den(69)
  A(29) = cont_SS(wf(:,18),wf(:,57)) * den(72)
  A(30) = cont_QA(wf(:,8),wf(:,58)) * den(7)
  A(31) = cont_QA(wf(:,8),wf(:,59)) * den(10)
  A(32) = cont_QA(wf(:,14),wf(:,60)) * den(14)
  A(33) = cont_QA(wf(:,16),wf(:,60)) * den(16)
  A(34) = cont_QA(wf(:,23),wf(:,61)) * den(25)
  A(35) = cont_QA(wf(:,23),wf(:,62)) * den(27)
  A(36) = cont_QA(wf(:,27),wf(:,63)) * den(31)
  A(37) = cont_QA(wf(:,29),wf(:,63)) * den(33)
  A(38) = cont_QA(wf(:,14),wf(:,64)) * den(36)
  A(39) = cont_QA(wf(:,16),wf(:,64)) * den(37)
  A(40) = cont_VV(wf(:,65),wf(:,66)) * den(75)
  A(41) = cont_QA(wf(:,8),wf(:,67)) * den(38)
  A(42) = cont_QA(wf(:,8),wf(:,68)) * den(39)
  A(43) = cont_VV(wf(:,65),wf(:,69)) * den(76)
  A(44) = cont_VV(wf(:,70),wf(:,71)) * den(79)
  A(45) = cont_VV(wf(:,70),wf(:,72)) * den(80)
  A(46) = cont_VV(wf(:,70),wf(:,73)) * den(81)
  A(47) = cont_VV(wf(:,70),wf(:,74)) * den(82)
  A(48) = cont_SS(wf(:,18),wf(:,75)) * den(83)
  A(49) = cont_QA(wf(:,76),wf(:,77)) * den(86)
  A(50) = cont_QA(wf(:,77),wf(:,78)) * den(87)
  A(51) = cont_QA(wf(:,79),wf(:,80)) * den(90)
  A(52) = cont_QA(wf(:,79),wf(:,81)) * den(92)
  A(53) = cont_QA(wf(:,31),wf(:,82)) * den(36)
  A(54) = cont_QA(wf(:,31),wf(:,83)) * den(37)
  A(55) = cont_QA(wf(:,32),wf(:,84)) * den(38)
  A(56) = cont_QA(wf(:,33),wf(:,84)) * den(39)
  A(57) = cont_QA(wf(:,23),wf(:,87)) * den(25)
  A(58) = cont_QA(wf(:,23),wf(:,88)) * den(27)
  A(59) = cont_QA(wf(:,27),wf(:,89)) * den(31)
  A(60) = cont_QA(wf(:,29),wf(:,89)) * den(33)
  A(61) = cont_SS(wf(:,18),wf(:,90)) * den(35)
  A(62) = cont_SS(wf(:,18),wf(:,91)) * den(93)
  A(63) = cont_QA(wf(:,92),wf(:,93)) * den(96)
  A(64) = cont_QA(wf(:,93),wf(:,94)) * den(97)
  A(65) = cont_QA(wf(:,95),wf(:,96)) * den(100)
  A(66) = cont_QA(wf(:,95),wf(:,97)) * den(102)
  A(67) = cont_QA(wf(:,8),wf(:,99)) * den(38)
  A(68) = cont_QA(wf(:,8),wf(:,101)) * den(39)
  A(69) = cont_QA(wf(:,14),wf(:,103)) * den(36)
  A(70) = cont_QA(wf(:,16),wf(:,103)) * den(37)
  A(71) = cont_QA(wf(:,8),wf(:,106)) * den(7)
  A(72) = cont_QA(wf(:,8),wf(:,107)) * den(10)
  A(73) = cont_QA(wf(:,14),wf(:,108)) * den(14)
  A(74) = cont_QA(wf(:,16),wf(:,108)) * den(16)
  A(75) = cont_SS(wf(:,18),wf(:,109)) * den(20)
  A(76) = cont_VV(wf(:,40),wf(:,111)) * den(104)
  A(77) = cont_VV(wf(:,44),wf(:,111)) * den(105)
  A(78) = cont_VV(wf(:,37),wf(:,112)) * den(44)
  A(79) = cont_VV(wf(:,40),wf(:,114)) * den(47)
  A(80) = cont_VV(wf(:,43),wf(:,112)) * den(50)
  A(81) = cont_VV(wf(:,44),wf(:,114)) * den(51)
  A(82) = cont_VV(wf(:,37),wf(:,115)) * den(53)
  A(83) = cont_VV(wf(:,40),wf(:,117)) * den(55)
  A(84) = cont_VV(wf(:,43),wf(:,115)) * den(56)
  A(85) = cont_VV(wf(:,44),wf(:,117)) * den(57)
  A(86) = cont_VV(wf(:,37),wf(:,118)) * den(53)
  A(87) = cont_VV(wf(:,40),wf(:,120)) * den(55)
  A(88) = cont_VV(wf(:,43),wf(:,118)) * den(56)
  A(89) = cont_VV(wf(:,44),wf(:,120)) * den(57)
  A(90) = cont_VV(wf(:,37),wf(:,121)) * den(44)
  A(91) = cont_VV(wf(:,40),wf(:,123)) * den(47)
  A(92) = cont_VV(wf(:,43),wf(:,121)) * den(50)
  A(93) = cont_VV(wf(:,44),wf(:,123)) * den(51)
  A(94) = cont_VV(wf(:,50),wf(:,111)) * den(106)
  A(95) = cont_VV(wf(:,53),wf(:,111)) * den(107)
  A(96) = cont_VV(wf(:,50),wf(:,114)) * den(60)
  A(97) = cont_VV(wf(:,53),wf(:,114)) * den(63)
  A(98) = cont_VV(wf(:,50),wf(:,117)) * den(64)
  A(99) = cont_VV(wf(:,53),wf(:,117)) * den(65)
  A(100) = cont_VV(wf(:,50),wf(:,120)) * den(64)
  A(101) = cont_VV(wf(:,53),wf(:,120)) * den(65)
  A(102) = cont_VV(wf(:,50),wf(:,123)) * den(60)
  A(103) = cont_VV(wf(:,53),wf(:,123)) * den(63)
  A(104) = cont_SS(wf(:,18),wf(:,126)) * den(109)
  A(105) = cont_QA(wf(:,8),wf(:,127)) * den(111)
  A(106) = cont_QA(wf(:,8),wf(:,128)) * den(113)
  A(107) = cont_QA(wf(:,14),wf(:,129)) * den(115)
  A(108) = cont_QA(wf(:,16),wf(:,129)) * den(116)
  A(109) = cont_QA(wf(:,80),wf(:,130)) * den(117)
  A(110) = cont_QA(wf(:,81),wf(:,130)) * den(118)
  A(111) = cont_QA(wf(:,77),wf(:,131)) * den(119)
  A(112) = cont_QA(wf(:,77),wf(:,132)) * den(120)
  A(113) = cont_SS(wf(:,18),wf(:,135)) * den(122)
  A(114) = cont_QA(wf(:,96),wf(:,136)) * den(123)
  A(115) = cont_QA(wf(:,97),wf(:,136)) * den(124)
  A(116) = cont_QA(wf(:,93),wf(:,137)) * den(125)
  A(117) = cont_QA(wf(:,93),wf(:,138)) * den(126)
  A(118) = cont_QA(wf(:,23),wf(:,139)) * den(128)
  A(119) = cont_QA(wf(:,23),wf(:,140)) * den(130)
  A(120) = cont_QA(wf(:,27),wf(:,141)) * den(132)
  A(121) = cont_QA(wf(:,29),wf(:,141)) * den(133)
  A(122) = cont_QA(wf(:,136),wf(:,143)) * den(135)
  A(123) = cont_QA(wf(:,136),wf(:,145)) * den(137)
  A(124) = cont_QA(wf(:,137),wf(:,147)) * den(139)
  A(125) = cont_QA(wf(:,138),wf(:,147)) * den(140)
  A(126) = cont_QA(wf(:,131),wf(:,148)) * den(142)
  A(127) = cont_QA(wf(:,132),wf(:,148)) * den(143)
  A(128) = cont_QA(wf(:,130),wf(:,149)) * den(145)
  A(129) = cont_QA(wf(:,130),wf(:,150)) * den(147)
  A(130) = cont_VV(wf(:,37),wf(:,151)) * den(150)
  A(131) = cont_VV(wf(:,152),wf(:,153)) * den(153)
  A(132) = cont_VV(wf(:,43),wf(:,151)) * den(155)
  A(133) = cont_VV(wf(:,152),wf(:,154)) * den(157)
  A(134) = cont_VV(wf(:,37),wf(:,155)) * den(158)
  A(135) = cont_VV(wf(:,153),wf(:,156)) * den(159)
  A(136) = cont_VV(wf(:,43),wf(:,155)) * den(160)
  A(137) = cont_VV(wf(:,154),wf(:,156)) * den(161)
  A(138) = cont_VV(wf(:,152),wf(:,157)) * den(163)
  A(139) = cont_VV(wf(:,152),wf(:,158)) * den(165)
  A(140) = cont_VV(wf(:,156),wf(:,157)) * den(166)
  A(141) = cont_VV(wf(:,156),wf(:,158)) * den(167)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(141)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(1)+A(3)+A(6)+A(8)+A(11)+A(13)+A(15)+A(17)+A(19)+A(21))*f(1)+(A(2)+A(4)+A(7)+A(9)+A(12)+A(14)+A(16)+A(18)+A(20)+A(22) &
       +A(23)+A(24)+A(25)+A(26))*f(2)+(-A(5)-A(10))*f(13)

  M2(1) = (-A(105)-A(107)-A(109)-A(111)-A(114)-A(116)-A(118)-A(120)-A(122)-A(124)-A(126)-A(128)-A(130)-A(132)-A(134)-A(136))*f(3) &
       +(-A(106)-A(108)-A(110)-A(112)-A(115)-A(117)-A(119)-A(121)-A(123)-A(125)-A(127)-A(129)-A(131)-A(133)-A(135)-A(137)-A(138) &
       -A(139)-A(140)-A(141))*f(4)+A(27)*f(5)+(A(38)+A(41)+A(57)+A(59)+A(71)+A(73)+A(82)+A(84)+A(90)+A(92))*f(6)+(A(39)+A(42) &
       +A(58)+A(60)+A(72)+A(74)+A(83)+A(85)+A(91)+A(93)+A(98)+A(99)+A(102)+A(103))*f(7)+(A(30)+A(32)+A(34)+A(36)+A(49)+A(51)+A(53) &
       +A(55)+A(63)+A(65)+A(67)+A(69)+A(78)+A(80)+A(86)+A(88))*f(8)+(A(31)+A(33)+A(35)+A(37)+A(50)+A(52)+A(54)+A(56)+A(64)+A(66) &
       +A(68)+A(70)+A(79)+A(81)+A(87)+A(89)+A(96)+A(97)+A(100)+A(101))*f(9)+(-A(44)-A(46))*f(10)+(-A(40)-A(43)-A(45)-A(47)-A(76) &
       -A(77)-A(94)-A(95))*f(11)+A(28)*f(12)+(A(104)+A(113))*f(14)+(-A(61)-A(75))*f(15)+(-A(48)-A(62))*f(16)-A(29)*f(17)

end subroutine colourvectors

end module ol_loop_ppllllj_nmnmxeexbbxg_1_/**/REALKIND
