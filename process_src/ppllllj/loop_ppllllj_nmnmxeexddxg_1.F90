
module ol_colourmatrix_ppllllj_nmnmxeexddxg_1_/**/REALKIND
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
end module ol_colourmatrix_ppllllj_nmnmxeexddxg_1_/**/REALKIND



module ol_forced_parameters_ppllllj_nmnmxeexddxg_1_/**/REALKIND
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
end module ol_forced_parameters_ppllllj_nmnmxeexddxg_1_/**/REALKIND

module ol_loop_ppllllj_nmnmxeexddxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(24), c(13)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:190)
  ! denominators
  complex(REALKIND), save :: den(264)
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
    f( 6) = (CI*countertermnorm*ctGqq*eQED**4*gQCD**3)/3._/**/REALKIND
    f( 7) = CI*countertermnorm*ctGqq*eQED**4*gQCD**3
    f( 8) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**3)/3._/**/REALKIND
    f( 9) = CI*countertermnorm*ctVqq*eQED**4*gQCD**3
    f(10) = (countertermnorm*ctZGG*eQED**4*gQCD**3)/3._/**/REALKIND
    f(11) = countertermnorm*ctZGG*eQED**4*gQCD**3
    f(12) = CI*countertermnorm*ctZZGG*eQED**4*gQCD**3
    f(13) = (CI*countertermnorm*ctHGG*eQED**4*gQCD**3*MW)/(cw**2*sw)
    f(14) = (CI*eQED**4*gQCD**3*integralnorm*SwB)/3._/**/REALKIND
    f(15) = CI*eQED**4*gQCD**3*integralnorm*SwB
    f(16) = (eQED**4*gQCD**3*integralnorm*SwB)/3._/**/REALKIND
    f(17) = eQED**4*gQCD**3*integralnorm*SwB
    f(18) = (eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(19) = (2*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(20) = eQED**4*gQCD**3*integralnorm*SwF
    f(21) = (4*eQED**4*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(22) = 2*eQED**4*gQCD**3*integralnorm*SwF
    f(23) = (eQED**4*gQCD**3*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(24) = (eQED**4*gQCD**3*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)

  c = [ 9*CI*f(14), 9*CI*f(15), f(16), 8*f(16), f(17), 8*f(17), 3*f(18), 3*f(19), 3*f(20), 3*f(21), 3*f(22), 3*f(23), 3*f(24) ]
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
  complex(REALKIND) :: A(133)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_V(P(:,7), rZERO, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_VQ_A(wf(:,-6),wf(:,-4),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call prop_Q_A(wf(:,3),Q(:,80),ZERO,0_intkind1,wf(:,5))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,6))
  call vert_VQ_A(wf(:,2),wf(:,5),wf(:,7))
  call prop_A_Q(wf(:,6),Q(:,35),ZERO,0_intkind1,wf(:,8))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,-3),wf(:,9))
  call prop_W_W(wf(:,9),Q(:,12),MZ,1_intkind1,wf(:,10))
  call vert_ZQ_A(gZd,wf(:,10),wf(:,5),wf(:,11))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,12))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,5),wf(:,13))
  call prop_A_Q(wf(:,12),Q(:,44),ZERO,0_intkind1,wf(:,14))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,10),wf(:,15))
  call prop_A_Q(wf(:,15),Q(:,44),ZERO,0_intkind1,wf(:,16))
  call vert_AV_Q(wf(:,-5),wf(:,-6),wf(:,17))
  call prop_A_Q(wf(:,17),Q(:,96),ZERO,0_intkind1,wf(:,18))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,19))
  call vert_AV_Q(wf(:,18),wf(:,2),wf(:,20))
  call prop_Q_A(wf(:,19),Q(:,19),ZERO,0_intkind1,wf(:,21))
  call vert_AZ_Q(gZd,wf(:,18),wf(:,10),wf(:,22))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,23))
  call vert_AZ_Q(gZd,wf(:,18),wf(:,4),wf(:,24))
  call prop_Q_A(wf(:,23),Q(:,28),ZERO,0_intkind1,wf(:,25))
  call vert_ZQ_A(gZd,wf(:,10),wf(:,-4),wf(:,26))
  call prop_Q_A(wf(:,26),Q(:,28),ZERO,0_intkind1,wf(:,27))
  call vert_VQ_A(wf(:,-6),wf(:,21),wf(:,28))
  call vert_VQ_A(wf(:,-6),wf(:,25),wf(:,29))
  call vert_VQ_A(wf(:,-6),wf(:,27),wf(:,30))
  call vert_ZQ_A(gZl,wf(:,4),wf(:,-2),wf(:,31))
  call vert_QA_V(wf(:,5),wf(:,-5),wf(:,32))
  call prop_Q_A(wf(:,31),Q(:,7),ZERO,0_intkind1,wf(:,33))
  call vert_QA_V(wf(:,33),wf(:,-3),wf(:,34))
  call vert_QA_Z(gZd,wf(:,5),wf(:,-5),wf(:,35))
  call prop_W_W(wf(:,35),Q(:,112),MZ,1_intkind1,wf(:,36))
  call vert_QA_Z(gZl,wf(:,33),wf(:,-3),wf(:,37))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,4),wf(:,38))
  call prop_A_Q(wf(:,38),Q(:,11),ZERO,0_intkind1,wf(:,39))
  call vert_QA_V(wf(:,-2),wf(:,39),wf(:,40))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,39),wf(:,41))
  call vert_QA_V(wf(:,-4),wf(:,18),wf(:,42))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,18),wf(:,43))
  call prop_W_W(wf(:,43),Q(:,112),MZ,1_intkind1,wf(:,44))
  call vert_ZQ_A(gZn,wf(:,10),wf(:,0),wf(:,45))
  call prop_Q_A(wf(:,45),Q(:,13),ZERO,0_intkind1,wf(:,46))
  call vert_QA_Z(gZn,wf(:,46),wf(:,-1),wf(:,47))
  call vert_AZ_Q(gZn,wf(:,-1),wf(:,10),wf(:,48))
  call prop_A_Q(wf(:,48),Q(:,14),ZERO,0_intkind1,wf(:,49))
  call vert_QA_Z(gZn,wf(:,0),wf(:,49),wf(:,50))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,51))
  call counter_VVG_G(wf(:,4),wf(:,2),wf(:,-6),wf(:,52))
  call counter_VVG_G(wf(:,4),wf(:,10),wf(:,-6),wf(:,53))
  call vert_VV_S(wf(:,4),wf(:,10),wf(:,54))
  call counter_GG_S(wf(:,51),wf(:,-6),wf(:,55))
  call counter_VQ_A(wf(:,2),wf(:,5),wf(:,56))
  call counter_ZQ_A(gZd,wf(:,10),wf(:,5),wf(:,57))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,5),wf(:,58))
  call counter_AV_Q(wf(:,18),wf(:,2),wf(:,59))
  call counter_AZ_Q(gZd,wf(:,18),wf(:,10),wf(:,60))
  call counter_AZ_Q(gZd,wf(:,18),wf(:,4),wf(:,61))
  call counter_VQ_A(wf(:,-6),wf(:,21),wf(:,62))
  call counter_VG_G(wf(:,10),wf(:,-6),Q(:,64),wf(:,63),Q(:,76))
  call vert_QA_V(wf(:,21),wf(:,-5),wf(:,64))
  call counter_VQ_A(wf(:,-6),wf(:,25),wf(:,65))
  call counter_VQ_A(wf(:,-6),wf(:,27),wf(:,66))
  call vert_QA_V(wf(:,-4),wf(:,8),wf(:,67))
  call counter_VG_G(wf(:,4),wf(:,-6),Q(:,64),wf(:,68),Q(:,67))
  call vert_QA_V(wf(:,25),wf(:,-5),wf(:,69))
  call vert_QA_V(wf(:,27),wf(:,-5),wf(:,70))
  call vert_QA_V(wf(:,-4),wf(:,14),wf(:,71))
  call vert_QA_V(wf(:,-4),wf(:,16),wf(:,72))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,73))
  call prop_Q_A(wf(:,13),Q(:,83),ZERO,0_intkind1,wf(:,74))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,10),wf(:,75))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,4),wf(:,76))
  call prop_Q_A(wf(:,7),Q(:,92),ZERO,0_intkind1,wf(:,77))
  call prop_Q_A(wf(:,11),Q(:,92),ZERO,0_intkind1,wf(:,78))
  call prop_A_Q(wf(:,73),Q(:,44),ZERO,0_intkind1,wf(:,79))
  call prop_A_Q(wf(:,75),Q(:,44),ZERO,0_intkind1,wf(:,80))
  call prop_A_Q(wf(:,76),Q(:,35),ZERO,0_intkind1,wf(:,81))
  call counter_AV_Q(wf(:,-5),wf(:,-6),wf(:,82))
  call prop_A_Q(wf(:,82),Q(:,96),ZERO,0_intkind1,wf(:,83))
  call vert_AV_Q(wf(:,83),wf(:,2),wf(:,84))
  call vert_AZ_Q(gZd,wf(:,83),wf(:,10),wf(:,85))
  call vert_AZ_Q(gZd,wf(:,83),wf(:,4),wf(:,86))
  call counter_VQ_A(wf(:,2),wf(:,-4),wf(:,87))
  call prop_A_Q(wf(:,24),Q(:,99),ZERO,0_intkind1,wf(:,88))
  call counter_ZQ_A(gZd,wf(:,10),wf(:,-4),wf(:,89))
  call counter_ZQ_A(gZd,wf(:,4),wf(:,-4),wf(:,90))
  call prop_A_Q(wf(:,20),Q(:,108),ZERO,0_intkind1,wf(:,91))
  call prop_A_Q(wf(:,22),Q(:,108),ZERO,0_intkind1,wf(:,92))
  call prop_Q_A(wf(:,87),Q(:,28),ZERO,0_intkind1,wf(:,93))
  call vert_VQ_A(wf(:,-6),wf(:,93),wf(:,94))
  call prop_Q_A(wf(:,89),Q(:,28),ZERO,0_intkind1,wf(:,95))
  call vert_VQ_A(wf(:,-6),wf(:,95),wf(:,96))
  call prop_Q_A(wf(:,90),Q(:,19),ZERO,0_intkind1,wf(:,97))
  call vert_VQ_A(wf(:,-6),wf(:,97),wf(:,98))
  call counter_VQ_A(wf(:,-6),wf(:,-4),wf(:,99))
  call prop_Q_A(wf(:,99),Q(:,80),ZERO,0_intkind1,wf(:,100))
  call vert_VQ_A(wf(:,2),wf(:,100),wf(:,101))
  call vert_ZQ_A(gZd,wf(:,10),wf(:,100),wf(:,102))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,100),wf(:,103))
  call counter_GG_V(wf(:,51),Q(:,48),wf(:,-6),Q(:,64),wf(:,104))
  call prop_W_W(wf(:,104),Q(:,112),MZ,1_intkind1,wf(:,105))
  call counter_QA_V(wf(:,5),wf(:,-5),wf(:,106))
  call counter_QA_Z(gZd,wf(:,5),wf(:,-5),wf(:,107))
  call prop_W_W(wf(:,107),Q(:,112),MZ,1_intkind1,wf(:,108))
  call vert_QA_V(wf(:,-4),wf(:,83),wf(:,109))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,83),wf(:,110))
  call prop_W_W(wf(:,110),Q(:,112),MZ,1_intkind1,wf(:,111))
  call counter_QA_V(wf(:,-4),wf(:,18),wf(:,112))
  call counter_QA_Z(gZd,wf(:,-4),wf(:,18),wf(:,113))
  call prop_W_W(wf(:,113),Q(:,112),MZ,1_intkind1,wf(:,114))
  call vert_QA_V(wf(:,100),wf(:,-5),wf(:,115))
  call vert_QA_Z(gZd,wf(:,100),wf(:,-5),wf(:,116))
  call prop_W_W(wf(:,116),Q(:,112),MZ,1_intkind1,wf(:,117))
  call counter_Q_A(ctqq,wf(:,5),Q(:,80),wf(:,118))
  call prop_Q_A(wf(:,118),Q(:,80),ZERO,0_intkind1,wf(:,119))
  call vert_VQ_A(wf(:,2),wf(:,119),wf(:,120))
  call vert_ZQ_A(gZd,wf(:,10),wf(:,119),wf(:,121))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,119),wf(:,122))
  call counter_A_Q(ctqq,wf(:,8),Q(:,35),wf(:,123))
  call counter_A_Q(ctqq,wf(:,14),Q(:,44),wf(:,124))
  call counter_A_Q(ctqq,wf(:,16),Q(:,44),wf(:,125))
  call counter_Q_A(ctqq,wf(:,21),Q(:,19),wf(:,126))
  call counter_Q_A(ctqq,wf(:,25),Q(:,28),wf(:,127))
  call counter_Q_A(ctqq,wf(:,27),Q(:,28),wf(:,128))
  call counter_A_Q(ctqq,wf(:,18),Q(:,96),wf(:,129))
  call prop_A_Q(wf(:,129),Q(:,96),ZERO,0_intkind1,wf(:,130))
  call vert_AV_Q(wf(:,130),wf(:,2),wf(:,131))
  call vert_AZ_Q(gZd,wf(:,130),wf(:,10),wf(:,132))
  call vert_AZ_Q(gZd,wf(:,130),wf(:,4),wf(:,133))
  call vert_AV_Q(wf(:,14),wf(:,-6),wf(:,134))
  call prop_A_Q(wf(:,134),Q(:,108),ZERO,0_intkind1,wf(:,135))
  call vert_AV_Q(wf(:,16),wf(:,-6),wf(:,136))
  call prop_A_Q(wf(:,136),Q(:,108),ZERO,0_intkind1,wf(:,137))
  call vert_AV_Q(wf(:,8),wf(:,-6),wf(:,138))
  call prop_A_Q(wf(:,138),Q(:,99),ZERO,0_intkind1,wf(:,139))
  call prop_Q_A(wf(:,28),Q(:,83),ZERO,0_intkind1,wf(:,140))
  call prop_Q_A(wf(:,29),Q(:,92),ZERO,0_intkind1,wf(:,141))
  call prop_Q_A(wf(:,30),Q(:,92),ZERO,0_intkind1,wf(:,142))
  call vert_QA_V(wf(:,119),wf(:,-5),wf(:,143))
  call vert_QA_Z(gZd,wf(:,119),wf(:,-5),wf(:,144))
  call prop_W_W(wf(:,37),Q(:,15),MZ,1_intkind1,wf(:,145))
  call prop_W_W(wf(:,41),Q(:,15),MZ,1_intkind1,wf(:,146))
  call vert_QA_V(wf(:,-4),wf(:,130),wf(:,147))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,130),wf(:,148))
  call prop_W_W(wf(:,47),Q(:,15),MZ,1_intkind1,wf(:,149))
  call prop_W_W(wf(:,50),Q(:,15),MZ,1_intkind1,wf(:,150))
  call vert_VQ_A(wf(:,2),wf(:,21),wf(:,151))
  call prop_Q_A(wf(:,151),Q(:,31),ZERO,0_intkind1,wf(:,152))
  call vert_ZQ_A(gZd,wf(:,10),wf(:,21),wf(:,153))
  call prop_Q_A(wf(:,153),Q(:,31),ZERO,0_intkind1,wf(:,154))
  call vert_AV_Q(wf(:,8),wf(:,2),wf(:,155))
  call prop_A_Q(wf(:,155),Q(:,47),ZERO,0_intkind1,wf(:,156))
  call vert_AZ_Q(gZd,wf(:,8),wf(:,10),wf(:,157))
  call prop_A_Q(wf(:,157),Q(:,47),ZERO,0_intkind1,wf(:,158))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,25),wf(:,159))
  call prop_Q_A(wf(:,159),Q(:,31),ZERO,0_intkind1,wf(:,160))
  call vert_ZQ_A(gZd,wf(:,4),wf(:,27),wf(:,161))
  call prop_Q_A(wf(:,161),Q(:,31),ZERO,0_intkind1,wf(:,162))
  call vert_AZ_Q(gZd,wf(:,14),wf(:,4),wf(:,163))
  call prop_A_Q(wf(:,163),Q(:,47),ZERO,0_intkind1,wf(:,164))
  call vert_AZ_Q(gZd,wf(:,16),wf(:,4),wf(:,165))
  call prop_A_Q(wf(:,165),Q(:,47),ZERO,0_intkind1,wf(:,166))
  call vert_VQ_A(wf(:,34),wf(:,-4),wf(:,167))
  call prop_Q_A(wf(:,167),Q(:,31),ZERO,0_intkind1,wf(:,168))
  call vert_ZQ_A(gZd,wf(:,145),wf(:,-4),wf(:,169))
  call prop_Q_A(wf(:,169),Q(:,31),ZERO,0_intkind1,wf(:,170))
  call vert_AV_Q(wf(:,-5),wf(:,34),wf(:,171))
  call prop_A_Q(wf(:,171),Q(:,47),ZERO,0_intkind1,wf(:,172))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,145),wf(:,173))
  call prop_A_Q(wf(:,173),Q(:,47),ZERO,0_intkind1,wf(:,174))
  call vert_VQ_A(wf(:,40),wf(:,-4),wf(:,175))
  call prop_Q_A(wf(:,175),Q(:,31),ZERO,0_intkind1,wf(:,176))
  call vert_ZQ_A(gZd,wf(:,146),wf(:,-4),wf(:,177))
  call prop_Q_A(wf(:,177),Q(:,31),ZERO,0_intkind1,wf(:,178))
  call vert_AV_Q(wf(:,-5),wf(:,40),wf(:,179))
  call prop_A_Q(wf(:,179),Q(:,47),ZERO,0_intkind1,wf(:,180))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,146),wf(:,181))
  call prop_A_Q(wf(:,181),Q(:,47),ZERO,0_intkind1,wf(:,182))
  call vert_ZQ_A(gZd,wf(:,149),wf(:,-4),wf(:,183))
  call prop_Q_A(wf(:,183),Q(:,31),ZERO,0_intkind1,wf(:,184))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,149),wf(:,185))
  call prop_A_Q(wf(:,185),Q(:,47),ZERO,0_intkind1,wf(:,186))
  call vert_ZQ_A(gZd,wf(:,150),wf(:,-4),wf(:,187))
  call prop_Q_A(wf(:,187),Q(:,31),ZERO,0_intkind1,wf(:,188))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,150),wf(:,189))
  call prop_A_Q(wf(:,189),Q(:,47),ZERO,0_intkind1,wf(:,190))

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
  den(3) = 1 / (Q(5,80))
  den(5) = 1 / (Q(5,35))
  den(8) = 1 / (Q(5,12) - MZ2)
  den(12) = 1 / (Q(5,44))
  den(17) = 1 / (Q(5,96))
  den(19) = 1 / (Q(5,19))
  den(25) = 1 / (Q(5,28))
  den(34) = 1 / (Q(5,7))
  den(36) = 1 / (Q(5,112))
  den(39) = 1 / (Q(5,112) - MZ2)
  den(42) = 1 / (Q(5,11))
  den(52) = 1 / (Q(5,13))
  den(55) = 1 / (Q(5,14))
  den(60) = 1 / (Q(5,48))
  den(65) = 1 / (Q(5,15) - MH2)
  den(68) = 1 / (Q(5,76))
  den(72) = 1 / (Q(5,67))
  den(78) = 1 / (Q(5,83))
  den(82) = 1 / (Q(5,92))
  den(87) = 1 / (Q(5,99))
  den(91) = 1 / (Q(5,108))
  den(139) = 1 / (Q(5,15))
  den(142) = 1 / (Q(5,15) - MZ2)
  den(159) = 1 / (Q(5,51))
  den(162) = 1 / (Q(5,31))
  den(168) = 1 / (Q(5,47))
  den(172) = 1 / (Q(5,60))

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
  den(18) = den(2)*den(17)
  den(20) = den(1)*den(19)
  den(21) = den(18)*den(20)
  den(22) = den(8)*den(17)
  den(23) = den(20)*den(22)
  den(24) = den(1)*den(17)
  den(26) = den(2)*den(25)
  den(27) = den(24)*den(26)
  den(28) = den(8)*den(25)
  den(29) = den(24)*den(28)
  den(30) = den(13)*den(20)
  den(31) = den(15)*den(20)
  den(32) = den(6)*den(26)
  den(33) = den(6)*den(28)
  den(35) = den(1)*den(34)
  den(37) = den(3)*den(36)
  den(38) = den(35)*den(37)
  den(40) = den(3)*den(39)
  den(41) = den(35)*den(40)
  den(43) = den(1)*den(42)
  den(44) = den(37)*den(43)
  den(45) = den(40)*den(43)
  den(46) = den(17)*den(36)
  den(47) = den(35)*den(46)
  den(48) = den(17)*den(39)
  den(49) = den(35)*den(48)
  den(50) = den(43)*den(46)
  den(51) = den(43)*den(48)
  den(53) = den(8)*den(52)
  den(54) = den(40)*den(53)
  den(56) = den(8)*den(55)
  den(57) = den(40)*den(56)
  den(58) = den(48)*den(53)
  den(59) = den(48)*den(56)
  den(61) = den(1)*den(2)
  den(62) = den(60)*den(61)
  den(63) = den(1)*den(8)
  den(64) = den(60)*den(63)
  den(66) = den(63)*den(65)
  den(67) = den(60)*den(66)
  den(69) = den(8)*den(68)
  den(70) = den(20)*den(69)
  den(71) = den(6)*den(69)
  den(73) = den(1)*den(72)
  den(74) = den(26)*den(73)
  den(75) = den(28)*den(73)
  den(76) = den(13)*den(73)
  den(77) = den(15)*den(73)
  den(79) = den(11)*den(78)
  den(80) = den(2)*den(79)
  den(81) = den(8)*den(79)
  den(83) = den(4)*den(82)
  den(84) = den(1)*den(83)
  den(85) = den(9)*den(82)
  den(86) = den(1)*den(85)
  den(88) = den(24)*den(87)
  den(89) = den(2)*den(88)
  den(90) = den(8)*den(88)
  den(92) = den(18)*den(91)
  den(93) = den(1)*den(92)
  den(94) = den(22)*den(91)
  den(95) = den(1)*den(94)
  den(96) = den(39)*den(60)
  den(97) = den(35)*den(96)
  den(98) = den(43)*den(96)
  den(99) = den(53)*den(96)
  den(100) = den(56)*den(96)
  den(101) = den(3)**2
  den(102) = den(2)*den(101)
  den(103) = den(6)*den(102)
  den(104) = den(8)*den(101)
  den(105) = den(6)*den(104)
  den(106) = den(1)*den(101)
  den(107) = den(13)*den(106)
  den(108) = den(15)*den(106)
  den(109) = den(6)*den(83)
  den(110) = den(6)*den(85)
  den(111) = den(13)*den(79)
  den(112) = den(15)*den(79)
  den(113) = den(20)*den(92)
  den(114) = den(20)*den(94)
  den(115) = den(26)*den(88)
  den(116) = den(28)*den(88)
  den(117) = den(17)**2
  den(118) = den(2)*den(117)
  den(119) = den(20)*den(118)
  den(120) = den(8)*den(117)
  den(121) = den(20)*den(120)
  den(122) = den(1)*den(117)
  den(123) = den(26)*den(122)
  den(124) = den(28)*den(122)
  den(125) = den(13)*den(91)
  den(126) = den(20)*den(125)
  den(127) = den(15)*den(91)
  den(128) = den(20)*den(127)
  den(129) = den(6)*den(87)
  den(130) = den(26)*den(129)
  den(131) = den(28)*den(129)
  den(132) = den(20)*den(78)
  den(133) = den(13)*den(132)
  den(134) = den(15)*den(132)
  den(135) = den(26)*den(82)
  den(136) = den(6)*den(135)
  den(137) = den(28)*den(82)
  den(138) = den(6)*den(137)
  den(140) = den(35)*den(139)
  den(141) = den(101)*den(140)
  den(143) = den(35)*den(142)
  den(144) = den(101)*den(143)
  den(145) = den(43)*den(139)
  den(146) = den(101)*den(145)
  den(147) = den(43)*den(142)
  den(148) = den(101)*den(147)
  den(149) = den(117)*den(140)
  den(150) = den(117)*den(143)
  den(151) = den(117)*den(145)
  den(152) = den(117)*den(147)
  den(153) = den(53)*den(142)
  den(154) = den(101)*den(153)
  den(155) = den(56)*den(142)
  den(156) = den(101)*den(155)
  den(157) = den(117)*den(153)
  den(158) = den(117)*den(155)
  den(160) = den(20)*den(159)
  den(161) = den(2)*den(20)
  den(163) = den(161)*den(162)
  den(164) = den(8)*den(20)
  den(165) = den(162)*den(164)
  den(166) = den(6)*den(159)
  den(167) = den(2)*den(6)
  den(169) = den(167)*den(168)
  den(170) = den(6)*den(8)
  den(171) = den(168)*den(170)
  den(173) = den(26)*den(172)
  den(174) = den(28)*den(172)
  den(175) = den(1)*den(26)
  den(176) = den(162)*den(175)
  den(177) = den(1)*den(28)
  den(178) = den(162)*den(177)
  den(179) = den(13)*den(172)
  den(180) = den(15)*den(172)
  den(181) = den(1)*den(13)
  den(182) = den(168)*den(181)
  den(183) = den(1)*den(15)
  den(184) = den(168)*den(183)
  den(185) = den(140)*den(162)
  den(186) = den(143)*den(162)
  den(187) = den(140)*den(168)
  den(188) = den(143)*den(168)
  den(189) = den(145)*den(162)
  den(190) = den(147)*den(162)
  den(191) = den(145)*den(168)
  den(192) = den(147)*den(168)
  den(193) = den(153)*den(162)
  den(194) = den(153)*den(168)
  den(195) = den(155)*den(162)
  den(196) = den(155)*den(168)
  den(197) = den(1)*den(2)*den(60)
  den(198) = den(1)*den(8)*den(60)
  den(199) = den(2)*den(3)*den(6)
  den(200) = den(3)*den(6)*den(8)
  den(201) = den(1)*den(3)*den(13)
  den(202) = den(1)*den(3)*den(15)
  den(203) = den(1)*den(2)*den(3)
  den(204) = den(1)*den(3)*den(8)
  den(205) = den(2)*den(17)*den(20)
  den(206) = den(8)*den(17)*den(20)
  den(207) = den(1)*den(17)*den(26)
  den(208) = den(1)*den(17)*den(28)
  den(209) = den(1)*den(2)*den(17)
  den(210) = den(1)*den(8)*den(17)
  den(211) = den(2)*den(160)
  den(212) = den(8)*den(160)
  den(213) = den(2)*den(132)
  den(214) = den(8)*den(132)
  den(215) = den(2)*den(166)
  den(216) = den(8)*den(166)
  den(217) = den(2)*den(129)
  den(218) = den(8)*den(129)
  den(219) = den(1)*den(173)
  den(220) = den(1)*den(174)
  den(221) = den(1)*den(135)
  den(222) = den(1)*den(137)
  den(223) = den(1)*den(179)
  den(224) = den(1)*den(180)
  den(225) = den(1)*den(125)
  den(226) = den(1)*den(127)
  den(227) = den(60)*den(140)
  den(228) = den(60)*den(143)
  den(229) = den(60)*den(145)
  den(230) = den(60)*den(147)
  den(231) = den(3)*den(140)
  den(232) = den(3)*den(143)
  den(233) = den(3)*den(145)
  den(234) = den(3)*den(147)
  den(235) = den(17)*den(140)
  den(236) = den(17)*den(143)
  den(237) = den(17)*den(145)
  den(238) = den(17)*den(147)
  den(239) = den(60)*den(153)
  den(240) = den(60)*den(155)
  den(241) = den(3)*den(153)
  den(242) = den(3)*den(155)
  den(243) = den(17)*den(153)
  den(244) = den(17)*den(155)
  den(245) = den(3)*den(169)
  den(246) = den(3)*den(171)
  den(247) = den(3)*den(182)
  den(248) = den(3)*den(184)
  den(249) = den(17)*den(163)
  den(250) = den(17)*den(165)
  den(251) = den(17)*den(176)
  den(252) = den(17)*den(178)
  den(253) = den(3)*den(187)
  den(254) = den(3)*den(188)
  den(255) = den(3)*den(191)
  den(256) = den(3)*den(192)
  den(257) = den(17)*den(185)
  den(258) = den(17)*den(186)
  den(259) = den(17)*den(189)
  den(260) = den(17)*den(190)
  den(261) = den(3)*den(194)
  den(262) = den(3)*den(196)
  den(263) = den(17)*den(193)
  den(264) = den(17)*den(195)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(133)

  A(1) = cont_QA(wf(:,7),wf(:,8)) * den(7)
  A(2) = cont_QA(wf(:,8),wf(:,11)) * den(10)
  A(3) = cont_QA(wf(:,13),wf(:,14)) * den(14)
  A(4) = cont_QA(wf(:,13),wf(:,16)) * den(16)
  A(5) = cont_QA(wf(:,20),wf(:,21)) * den(21)
  A(6) = cont_QA(wf(:,21),wf(:,22)) * den(23)
  A(7) = cont_QA(wf(:,24),wf(:,25)) * den(27)
  A(8) = cont_QA(wf(:,24),wf(:,27)) * den(29)
  A(9) = cont_QA(wf(:,14),wf(:,28)) * den(30)
  A(10) = cont_QA(wf(:,16),wf(:,28)) * den(31)
  A(11) = cont_QA(wf(:,8),wf(:,29)) * den(32)
  A(12) = cont_QA(wf(:,8),wf(:,30)) * den(33)
  A(13) = cont_VV(wf(:,32),wf(:,34)) * den(38)
  A(14) = cont_VV(wf(:,36),wf(:,37)) * den(41)
  A(15) = cont_VV(wf(:,32),wf(:,40)) * den(44)
  A(16) = cont_VV(wf(:,36),wf(:,41)) * den(45)
  A(17) = cont_VV(wf(:,34),wf(:,42)) * den(47)
  A(18) = cont_VV(wf(:,37),wf(:,44)) * den(49)
  A(19) = cont_VV(wf(:,40),wf(:,42)) * den(50)
  A(20) = cont_VV(wf(:,41),wf(:,44)) * den(51)
  A(21) = cont_VV(wf(:,36),wf(:,47)) * den(54)
  A(22) = cont_VV(wf(:,36),wf(:,50)) * den(57)
  A(23) = cont_VV(wf(:,44),wf(:,47)) * den(58)
  A(24) = cont_VV(wf(:,44),wf(:,50)) * den(59)

  A(25) = cont_VV(wf(:,51),wf(:,52)) * den(62)
  A(26) = cont_VV(wf(:,51),wf(:,53)) * den(64)
  A(27) = cont_SS(wf(:,54),wf(:,55)) * den(67)
  A(28) = cont_QA(wf(:,8),wf(:,56)) * den(7)
  A(29) = cont_QA(wf(:,8),wf(:,57)) * den(10)
  A(30) = cont_QA(wf(:,14),wf(:,58)) * den(14)
  A(31) = cont_QA(wf(:,16),wf(:,58)) * den(16)
  A(32) = cont_QA(wf(:,21),wf(:,59)) * den(21)
  A(33) = cont_QA(wf(:,21),wf(:,60)) * den(23)
  A(34) = cont_QA(wf(:,25),wf(:,61)) * den(27)
  A(35) = cont_QA(wf(:,27),wf(:,61)) * den(29)
  A(36) = cont_QA(wf(:,14),wf(:,62)) * den(30)
  A(37) = cont_QA(wf(:,16),wf(:,62)) * den(31)
  A(38) = cont_VV(wf(:,63),wf(:,64)) * den(70)
  A(39) = cont_QA(wf(:,8),wf(:,65)) * den(32)
  A(40) = cont_QA(wf(:,8),wf(:,66)) * den(33)
  A(41) = cont_VV(wf(:,63),wf(:,67)) * den(71)
  A(42) = cont_VV(wf(:,68),wf(:,69)) * den(74)
  A(43) = cont_VV(wf(:,68),wf(:,70)) * den(75)
  A(44) = cont_VV(wf(:,68),wf(:,71)) * den(76)
  A(45) = cont_VV(wf(:,68),wf(:,72)) * den(77)
  A(46) = cont_QA(wf(:,73),wf(:,74)) * den(80)
  A(47) = cont_QA(wf(:,74),wf(:,75)) * den(81)
  A(48) = cont_QA(wf(:,76),wf(:,77)) * den(84)
  A(49) = cont_QA(wf(:,76),wf(:,78)) * den(86)
  A(50) = cont_QA(wf(:,28),wf(:,79)) * den(30)
  A(51) = cont_QA(wf(:,28),wf(:,80)) * den(31)
  A(52) = cont_QA(wf(:,29),wf(:,81)) * den(32)
  A(53) = cont_QA(wf(:,30),wf(:,81)) * den(33)
  A(54) = cont_QA(wf(:,21),wf(:,84)) * den(21)
  A(55) = cont_QA(wf(:,21),wf(:,85)) * den(23)
  A(56) = cont_QA(wf(:,25),wf(:,86)) * den(27)
  A(57) = cont_QA(wf(:,27),wf(:,86)) * den(29)
  A(58) = cont_QA(wf(:,87),wf(:,88)) * den(89)
  A(59) = cont_QA(wf(:,88),wf(:,89)) * den(90)
  A(60) = cont_QA(wf(:,90),wf(:,91)) * den(93)
  A(61) = cont_QA(wf(:,90),wf(:,92)) * den(95)
  A(62) = cont_QA(wf(:,8),wf(:,94)) * den(32)
  A(63) = cont_QA(wf(:,8),wf(:,96)) * den(33)
  A(64) = cont_QA(wf(:,14),wf(:,98)) * den(30)
  A(65) = cont_QA(wf(:,16),wf(:,98)) * den(31)
  A(66) = cont_QA(wf(:,8),wf(:,101)) * den(7)
  A(67) = cont_QA(wf(:,8),wf(:,102)) * den(10)
  A(68) = cont_QA(wf(:,14),wf(:,103)) * den(14)
  A(69) = cont_QA(wf(:,16),wf(:,103)) * den(16)
  A(70) = cont_VV(wf(:,37),wf(:,105)) * den(97)
  A(71) = cont_VV(wf(:,41),wf(:,105)) * den(98)
  A(72) = cont_VV(wf(:,34),wf(:,106)) * den(38)
  A(73) = cont_VV(wf(:,37),wf(:,108)) * den(41)
  A(74) = cont_VV(wf(:,40),wf(:,106)) * den(44)
  A(75) = cont_VV(wf(:,41),wf(:,108)) * den(45)
  A(76) = cont_VV(wf(:,34),wf(:,109)) * den(47)
  A(77) = cont_VV(wf(:,37),wf(:,111)) * den(49)
  A(78) = cont_VV(wf(:,40),wf(:,109)) * den(50)
  A(79) = cont_VV(wf(:,41),wf(:,111)) * den(51)
  A(80) = cont_VV(wf(:,34),wf(:,112)) * den(47)
  A(81) = cont_VV(wf(:,37),wf(:,114)) * den(49)
  A(82) = cont_VV(wf(:,40),wf(:,112)) * den(50)
  A(83) = cont_VV(wf(:,41),wf(:,114)) * den(51)
  A(84) = cont_VV(wf(:,34),wf(:,115)) * den(38)
  A(85) = cont_VV(wf(:,37),wf(:,117)) * den(41)
  A(86) = cont_VV(wf(:,40),wf(:,115)) * den(44)
  A(87) = cont_VV(wf(:,41),wf(:,117)) * den(45)
  A(88) = cont_VV(wf(:,47),wf(:,105)) * den(99)
  A(89) = cont_VV(wf(:,50),wf(:,105)) * den(100)
  A(90) = cont_VV(wf(:,47),wf(:,108)) * den(54)
  A(91) = cont_VV(wf(:,50),wf(:,108)) * den(57)
  A(92) = cont_VV(wf(:,47),wf(:,111)) * den(58)
  A(93) = cont_VV(wf(:,50),wf(:,111)) * den(59)
  A(94) = cont_VV(wf(:,47),wf(:,114)) * den(58)
  A(95) = cont_VV(wf(:,50),wf(:,114)) * den(59)
  A(96) = cont_VV(wf(:,47),wf(:,117)) * den(54)
  A(97) = cont_VV(wf(:,50),wf(:,117)) * den(57)
  A(98) = cont_QA(wf(:,8),wf(:,120)) * den(103)
  A(99) = cont_QA(wf(:,8),wf(:,121)) * den(105)
  A(100) = cont_QA(wf(:,14),wf(:,122)) * den(107)
  A(101) = cont_QA(wf(:,16),wf(:,122)) * den(108)
  A(102) = cont_QA(wf(:,77),wf(:,123)) * den(109)
  A(103) = cont_QA(wf(:,78),wf(:,123)) * den(110)
  A(104) = cont_QA(wf(:,74),wf(:,124)) * den(111)
  A(105) = cont_QA(wf(:,74),wf(:,125)) * den(112)
  A(106) = cont_QA(wf(:,91),wf(:,126)) * den(113)
  A(107) = cont_QA(wf(:,92),wf(:,126)) * den(114)
  A(108) = cont_QA(wf(:,88),wf(:,127)) * den(115)
  A(109) = cont_QA(wf(:,88),wf(:,128)) * den(116)
  A(110) = cont_QA(wf(:,21),wf(:,131)) * den(119)
  A(111) = cont_QA(wf(:,21),wf(:,132)) * den(121)
  A(112) = cont_QA(wf(:,25),wf(:,133)) * den(123)
  A(113) = cont_QA(wf(:,27),wf(:,133)) * den(124)
  A(114) = cont_QA(wf(:,126),wf(:,135)) * den(126)
  A(115) = cont_QA(wf(:,126),wf(:,137)) * den(128)
  A(116) = cont_QA(wf(:,127),wf(:,139)) * den(130)
  A(117) = cont_QA(wf(:,128),wf(:,139)) * den(131)
  A(118) = cont_QA(wf(:,124),wf(:,140)) * den(133)
  A(119) = cont_QA(wf(:,125),wf(:,140)) * den(134)
  A(120) = cont_QA(wf(:,123),wf(:,141)) * den(136)
  A(121) = cont_QA(wf(:,123),wf(:,142)) * den(138)
  A(122) = cont_VV(wf(:,34),wf(:,143)) * den(141)
  A(123) = cont_VV(wf(:,144),wf(:,145)) * den(144)
  A(124) = cont_VV(wf(:,40),wf(:,143)) * den(146)
  A(125) = cont_VV(wf(:,144),wf(:,146)) * den(148)
  A(126) = cont_VV(wf(:,34),wf(:,147)) * den(149)
  A(127) = cont_VV(wf(:,145),wf(:,148)) * den(150)
  A(128) = cont_VV(wf(:,40),wf(:,147)) * den(151)
  A(129) = cont_VV(wf(:,146),wf(:,148)) * den(152)
  A(130) = cont_VV(wf(:,144),wf(:,149)) * den(154)
  A(131) = cont_VV(wf(:,144),wf(:,150)) * den(156)
  A(132) = cont_VV(wf(:,148),wf(:,149)) * den(157)
  A(133) = cont_VV(wf(:,148),wf(:,150)) * den(158)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(133)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(1)+A(3)+A(5)+A(7)+A(9)+A(11)+A(13)+A(15)+A(17)+A(19))*f(1)+(A(2)+A(4)+A(6)+A(8)+A(10)+A(12)+A(14)+A(16)+A(18)+A(20) &
       +A(21)+A(22)+A(23)+A(24))*f(2)

  M2(1) = (-A(98)-A(100)-A(102)-A(104)-A(106)-A(108)-A(110)-A(112)-A(114)-A(116)-A(118)-A(120)-A(122)-A(124)-A(126)-A(128))*f(3)+( &
       -A(99)-A(101)-A(103)-A(105)-A(107)-A(109)-A(111)-A(113)-A(115)-A(117)-A(119)-A(121)-A(123)-A(125)-A(127)-A(129)-A(130) &
       -A(131)-A(132)-A(133))*f(4)+A(25)*f(5)+(A(36)+A(39)+A(54)+A(56)+A(66)+A(68)+A(76)+A(78)+A(84)+A(86))*f(6)+(A(37)+A(40) &
       +A(55)+A(57)+A(67)+A(69)+A(77)+A(79)+A(85)+A(87)+A(92)+A(93)+A(96)+A(97))*f(7)+(A(28)+A(30)+A(32)+A(34)+A(46)+A(48)+A(50) &
       +A(52)+A(58)+A(60)+A(62)+A(64)+A(72)+A(74)+A(80)+A(82))*f(8)+(A(29)+A(31)+A(33)+A(35)+A(47)+A(49)+A(51)+A(53)+A(59)+A(61) &
       +A(63)+A(65)+A(73)+A(75)+A(81)+A(83)+A(90)+A(91)+A(94)+A(95))*f(9)+(-A(42)-A(44))*f(10)+(-A(38)-A(41)-A(43)-A(45)-A(70) &
       -A(71)-A(88)-A(89))*f(11)+A(26)*f(12)-A(27)*f(13)

end subroutine colourvectors

end module ol_loop_ppllllj_nmnmxeexddxg_1_/**/REALKIND
