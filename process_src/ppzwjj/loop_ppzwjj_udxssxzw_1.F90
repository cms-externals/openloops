
module ol_colourmatrix_ppzwjj_udxssxzw_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(46,2), K2(2,2), KL(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9,   3]
  K1( 2,:) = [   3,   9]
  K1( 3,:) = [  12,   4]
  K1( 4,:) = [   4,  12]
  K1( 5,:) = [   0,  -4]
  K1( 6,:) = [  -4, -12]
  K1( 7,:) = [  12,   4]
  K1( 8,:) = [   4,  12]
  K1( 9,:) = [   0,   4]
  K1(10,:) = [   4,   0]
  K1(11,:) = [ -12,  -4]
  K1(12,:) = [  -4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [ -12,  -4]
  K1(16,:) = [  -4,   0]
  K1(17,:) = [   0,   4]
  K1(18,:) = [   4,   0]
  K1(19,:) = [   0,  -4]
  K1(20,:) = [  -4, -12]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
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
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [   0,   0]
  K1(42,:) = [   0,   0]
  K1(43,:) = [   0,   0]
  K1(44,:) = [   0,   0]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppzwjj_udxssxzw_1_/**/REALKIND



module ol_forced_parameters_ppzwjj_udxssxzw_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppzwjj_udxssxzw_1_/**/REALKIND

module ol_loop_ppzwjj_udxssxzw_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(19), c(34)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:147)
  ! denominators
  complex(REALKIND), save :: den(153)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,144)
  ! zero helicity identifier
  logical,           save :: zerohel(144) = .true., zerohel_ct(144) = .true.

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
    f( 1) = (CI*cw*eQED**2*gQCD**2)/(sqrt2*sw**2)
    f( 2) = (CI*countertermnorm*cw*eQED**2*gQCD**4)/(sqrt2*sw**2)
    f( 3) = (CI*countertermnorm*ctGqq*cw*eQED**2*gQCD**4)/(sqrt2*sw**2)
    f( 4) = (CI*countertermnorm*ctVqq*cw*eQED**2*gQCD**4)/(sqrt2*sw**2)
    f( 5) = (CI*eQED**2*gQCD**2)/(sqrt2*sw)
    f( 6) = (CI*countertermnorm*eQED**2*gQCD**4)/(sqrt2*sw)
    f( 7) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4)/(sqrt2*sw)
    f( 8) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4)/(sqrt2*sw)
    f( 9) = (countertermnorm*ctZGG*eQED**2*gQCD**4)/(sqrt2*sw)
    f(10) = (CI*cw*eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw**2)
    f(11) = (cw*eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw**2*2._/**/REALKIND)
    f(12) = (cw*eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw**2)
    f(13) = (CI*eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw)
    f(14) = (eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw*2._/**/REALKIND)
    f(15) = (eQED**2*gQCD**4*integralnorm*SwB)/(sqrt2*sw)
    f(16) = (cw*eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw**2)
    f(17) = (2*cw*eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw**2)
    f(18) = (eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw)
    f(19) = (2*eQED**2*gQCD**4*integralnorm*SwF)/(sqrt2*sw)

  c = [ 9*CI*f(10), 27*CI*f(10), 18*f(11), 54*f(11), f(12), 3*f(12), 6*f(12), 8*f(12), 10*f(12), 18*f(12), 21*f(12), 24*f(12) &
    , 54*f(12), 9*CI*f(13), 27*CI*f(13), 18*f(14), 54*f(14), f(15), 3*f(15), 6*f(15), 8*f(15), 10*f(15), 18*f(15), 21*f(15) &
    , 24*f(15), 54*f(15), 3*f(16), 9*f(16), 3*f(17), 9*f(17), 3*f(18), 9*f(18), 3*f(19), 9*f(19) ]
  c = (1._/**/REALKIND / 36) * c
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
  complex(REALKIND) :: A(94)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_V(P(:,5), rMZ, H(5), wf(:,-4))
  call wf_V(P(:,6), rMW, H(6), wf(:,-5))

  ! internal WFs
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,0),wf(:,1))
  call vert_AW_Q(wf(:,-1),wf(:,-5),wf(:,2))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,3))
  call prop_Q_A(wf(:,1),Q(:,17),ZERO,0_intkind1,wf(:,4))
  call prop_A_Q(wf(:,2),Q(:,34),ZERO,0_intkind1,wf(:,5))
  call vert_QA_V(wf(:,4),wf(:,5),wf(:,6))
  call vert_AV_Q(wf(:,-1),wf(:,3),wf(:,7))
  call vert_WQ_A(wf(:,-5),wf(:,4),wf(:,8))
  call prop_A_Q(wf(:,7),Q(:,14),ZERO,0_intkind1,wf(:,9))
  call vert_WQ_A(wf(:,-5),wf(:,0),wf(:,10))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-4),wf(:,11))
  call prop_Q_A(wf(:,10),Q(:,33),ZERO,0_intkind1,wf(:,12))
  call prop_A_Q(wf(:,11),Q(:,18),ZERO,0_intkind1,wf(:,13))
  call vert_QA_V(wf(:,12),wf(:,13),wf(:,14))
  call vert_VQ_A(wf(:,3),wf(:,0),wf(:,15))
  call vert_AW_Q(wf(:,13),wf(:,-5),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,13),ZERO,0_intkind1,wf(:,17))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,12),wf(:,18))
  call vert_AZ_Q(gZu,wf(:,5),wf(:,-4),wf(:,19))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-4),Q(:,16),wf(:,20))
  call prop_W_W(wf(:,20),Q(:,48),MW,1_intkind1,wf(:,21))
  call vert_AW_Q(wf(:,-1),wf(:,21),wf(:,22))
  call vert_WQ_A(wf(:,21),wf(:,0),wf(:,23))
  call prop_Q_A(wf(:,23),Q(:,49),ZERO,0_intkind1,wf(:,24))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,-2),wf(:,25))
  call prop_Q_A(wf(:,25),Q(:,20),ZERO,0_intkind1,wf(:,26))
  call vert_QA_V(wf(:,12),wf(:,-1),wf(:,27))
  call vert_QA_V(wf(:,26),wf(:,-3),wf(:,28))
  call vert_QA_V(wf(:,0),wf(:,5),wf(:,29))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,-4),wf(:,30))
  call prop_A_Q(wf(:,30),Q(:,24),ZERO,0_intkind1,wf(:,31))
  call vert_QA_V(wf(:,-2),wf(:,31),wf(:,32))
  call counter_QA_V(wf(:,4),wf(:,5),wf(:,33))
  call counter_WQ_A(wf(:,-5),wf(:,4),wf(:,34))
  call counter_QA_V(wf(:,12),wf(:,13),wf(:,35))
  call counter_AW_Q(wf(:,13),wf(:,-5),wf(:,36))
  call counter_VG_G(wf(:,-4),wf(:,3),Q(:,12),wf(:,37),Q(:,28))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,12),wf(:,38))
  call counter_AZ_Q(gZu,wf(:,5),wf(:,-4),wf(:,39))
  call counter_QA_V(wf(:,26),wf(:,-3),wf(:,40))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,-4),wf(:,41))
  call prop_A_Q(wf(:,41),Q(:,24),ZERO,0_intkind1,wf(:,42))
  call vert_QA_V(wf(:,-2),wf(:,42),wf(:,43))
  call counter_QA_V(wf(:,-2),wf(:,31),wf(:,44))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,-2),wf(:,45))
  call prop_Q_A(wf(:,45),Q(:,20),ZERO,0_intkind1,wf(:,46))
  call vert_QA_V(wf(:,46),wf(:,-3),wf(:,47))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,48))
  call vert_AV_Q(wf(:,-1),wf(:,48),wf(:,49))
  call prop_A_Q(wf(:,49),Q(:,14),ZERO,0_intkind1,wf(:,50))
  call vert_VQ_A(wf(:,48),wf(:,0),wf(:,51))
  call prop_Q_A(wf(:,51),Q(:,13),ZERO,0_intkind1,wf(:,52))
  call counter_AV_Q(wf(:,-1),wf(:,3),wf(:,53))
  call prop_Q_A(wf(:,8),Q(:,49),ZERO,0_intkind1,wf(:,54))
  call counter_AW_Q(wf(:,-1),wf(:,-5),wf(:,55))
  call prop_A_Q(wf(:,55),Q(:,34),ZERO,0_intkind1,wf(:,56))
  call vert_QA_V(wf(:,4),wf(:,56),wf(:,57))
  call prop_Q_A(wf(:,18),Q(:,49),ZERO,0_intkind1,wf(:,58))
  call counter_AW_Q(wf(:,-1),wf(:,21),wf(:,59))
  call vert_AZ_Q(gZu,wf(:,56),wf(:,-4),wf(:,60))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,-4),wf(:,61))
  call prop_A_Q(wf(:,61),Q(:,18),ZERO,0_intkind1,wf(:,62))
  call vert_QA_V(wf(:,12),wf(:,62),wf(:,63))
  call vert_AW_Q(wf(:,62),wf(:,-5),wf(:,64))
  call counter_QA_V(wf(:,12),wf(:,-1),wf(:,65))
  call vert_QA_V(wf(:,0),wf(:,56),wf(:,66))
  call counter_VQ_A(wf(:,3),wf(:,0),wf(:,67))
  call prop_A_Q(wf(:,16),Q(:,50),ZERO,0_intkind1,wf(:,68))
  call counter_WQ_A(wf(:,-5),wf(:,0),wf(:,69))
  call prop_Q_A(wf(:,69),Q(:,33),ZERO,0_intkind1,wf(:,70))
  call vert_QA_V(wf(:,70),wf(:,13),wf(:,71))
  call prop_A_Q(wf(:,19),Q(:,50),ZERO,0_intkind1,wf(:,72))
  call prop_A_Q(wf(:,22),Q(:,50),ZERO,0_intkind1,wf(:,73))
  call counter_WQ_A(wf(:,21),wf(:,0),wf(:,74))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,70),wf(:,75))
  call counter_ZQ_A(gZu,wf(:,-4),wf(:,0),wf(:,76))
  call prop_Q_A(wf(:,76),Q(:,17),ZERO,0_intkind1,wf(:,77))
  call vert_QA_V(wf(:,77),wf(:,5),wf(:,78))
  call vert_WQ_A(wf(:,-5),wf(:,77),wf(:,79))
  call counter_QA_V(wf(:,0),wf(:,5),wf(:,80))
  call vert_QA_V(wf(:,70),wf(:,-1),wf(:,81))
  call vert_AV_Q(wf(:,5),wf(:,3),wf(:,82))
  call counter_Q_A(ctqq,wf(:,4),Q(:,17),wf(:,83))
  call prop_A_Q(wf(:,82),Q(:,46),ZERO,0_intkind1,wf(:,84))
  call vert_VQ_A(wf(:,3),wf(:,4),wf(:,85))
  call counter_A_Q(ctqq,wf(:,5),Q(:,34),wf(:,86))
  call prop_Q_A(wf(:,85),Q(:,29),ZERO,0_intkind1,wf(:,87))
  call counter_V_V(ctGG,wf(:,3),Q(:,12),wf(:,88))
  call prop_Q_A(wf(:,83),Q(:,17),ZERO,0_intkind1,wf(:,89))
  call vert_WQ_A(wf(:,-5),wf(:,89),wf(:,90))
  call vert_AV_Q(wf(:,-1),wf(:,88),wf(:,91))
  call counter_A_Q(ctqq,wf(:,9),Q(:,14),wf(:,92))
  call vert_AV_Q(wf(:,13),wf(:,3),wf(:,93))
  call counter_Q_A(ctqq,wf(:,12),Q(:,33),wf(:,94))
  call prop_A_Q(wf(:,93),Q(:,30),ZERO,0_intkind1,wf(:,95))
  call vert_VQ_A(wf(:,3),wf(:,12),wf(:,96))
  call counter_A_Q(ctqq,wf(:,13),Q(:,18),wf(:,97))
  call prop_Q_A(wf(:,96),Q(:,45),ZERO,0_intkind1,wf(:,98))
  call vert_VQ_A(wf(:,88),wf(:,0),wf(:,99))
  call counter_Q_A(ctqq,wf(:,17),Q(:,13),wf(:,100))
  call prop_A_Q(wf(:,97),Q(:,18),ZERO,0_intkind1,wf(:,101))
  call vert_AW_Q(wf(:,101),wf(:,-5),wf(:,102))
  call prop_Q_A(wf(:,94),Q(:,33),ZERO,0_intkind1,wf(:,103))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,103),wf(:,104))
  call prop_A_Q(wf(:,86),Q(:,34),ZERO,0_intkind1,wf(:,105))
  call vert_AZ_Q(gZu,wf(:,105),wf(:,-4),wf(:,106))
  call counter_Q_A(ctqq,wf(:,24),Q(:,49),wf(:,107))
  call vert_QA_V(wf(:,103),wf(:,-1),wf(:,108))
  call counter_V_V(ctGG,wf(:,27),Q(:,35),wf(:,109))
  call counter_Q_A(ctqq,wf(:,26),Q(:,20),wf(:,110))
  call prop_Q_A(wf(:,110),Q(:,20),ZERO,0_intkind1,wf(:,111))
  call vert_QA_V(wf(:,111),wf(:,-3),wf(:,112))
  call vert_QA_V(wf(:,0),wf(:,105),wf(:,113))
  call counter_V_V(ctGG,wf(:,29),Q(:,35),wf(:,114))
  call counter_A_Q(ctqq,wf(:,31),Q(:,24),wf(:,115))
  call prop_A_Q(wf(:,115),Q(:,24),ZERO,0_intkind1,wf(:,116))
  call vert_QA_V(wf(:,-2),wf(:,116),wf(:,117))
  call vert_ZQ_A(gZu,wf(:,-4),wf(:,17),wf(:,118))
  call prop_Q_A(wf(:,118),Q(:,29),ZERO,0_intkind1,wf(:,119))
  call vert_WQ_A(wf(:,-5),wf(:,17),wf(:,120))
  call prop_Q_A(wf(:,120),Q(:,45),ZERO,0_intkind1,wf(:,121))
  call vert_AZ_Q(gZd,wf(:,9),wf(:,-4),wf(:,122))
  call prop_A_Q(wf(:,122),Q(:,30),ZERO,0_intkind1,wf(:,123))
  call vert_AW_Q(wf(:,9),wf(:,-5),wf(:,124))
  call prop_A_Q(wf(:,124),Q(:,46),ZERO,0_intkind1,wf(:,125))
  call vert_QA_V(wf(:,54),wf(:,-1),wf(:,126))
  call vert_QA_V(wf(:,0),wf(:,68),wf(:,127))
  call vert_VQ_A(wf(:,28),wf(:,0),wf(:,128))
  call prop_Q_A(wf(:,128),Q(:,29),ZERO,0_intkind1,wf(:,129))
  call vert_AV_Q(wf(:,-1),wf(:,28),wf(:,130))
  call prop_A_Q(wf(:,130),Q(:,30),ZERO,0_intkind1,wf(:,131))
  call vert_VQ_A(wf(:,32),wf(:,0),wf(:,132))
  call prop_Q_A(wf(:,132),Q(:,29),ZERO,0_intkind1,wf(:,133))
  call vert_AV_Q(wf(:,-1),wf(:,32),wf(:,134))
  call prop_A_Q(wf(:,134),Q(:,30),ZERO,0_intkind1,wf(:,135))
  call vert_VQ_A(wf(:,27),wf(:,-2),wf(:,136))
  call prop_Q_A(wf(:,136),Q(:,39),ZERO,0_intkind1,wf(:,137))
  call vert_AV_Q(wf(:,-3),wf(:,27),wf(:,138))
  call prop_A_Q(wf(:,138),Q(:,43),ZERO,0_intkind1,wf(:,139))
  call vert_QA_V(wf(:,58),wf(:,-1),wf(:,140))
  call vert_VQ_A(wf(:,29),wf(:,-2),wf(:,141))
  call prop_Q_A(wf(:,141),Q(:,39),ZERO,0_intkind1,wf(:,142))
  call vert_AV_Q(wf(:,-3),wf(:,29),wf(:,143))
  call prop_A_Q(wf(:,143),Q(:,43),ZERO,0_intkind1,wf(:,144))
  call vert_QA_V(wf(:,0),wf(:,72),wf(:,145))
  call vert_QA_V(wf(:,24),wf(:,-1),wf(:,146))
  call vert_QA_V(wf(:,0),wf(:,73),wf(:,147))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,17))
  den(2) = 1 / (Q(5,34))
  den(3) = 1 / (Q(5,12))
  den(6) = 1 / (Q(5,14))
  den(9) = 1 / (Q(5,33))
  den(10) = 1 / (Q(5,18))
  den(13) = 1 / (Q(5,13))
  den(18) = 1 / (Q(5,48) - MW2)
  den(20) = 1 / (Q(5,49))
  den(23) = 1 / (Q(5,20))
  den(24) = 1 / (Q(5,35))
  den(29) = 1 / (Q(5,24))
  den(38) = 1 / (Q(5,28))
  den(43) = 1 / (Q(5,50))
  den(54) = 1 / (Q(5,46))
  den(58) = 1 / (Q(5,29))
  den(61) = 1 / (Q(5,51))
  den(70) = 1 / (Q(5,30))
  den(74) = 1 / (Q(5,45))
  den(119) = 1 / (Q(5,39))
  den(121) = 1 / (Q(5,43))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(3)*den(6)
  den(8) = den(1)*den(7)
  den(11) = den(9)*den(10)
  den(12) = den(3)*den(11)
  den(14) = den(3)*den(13)
  den(15) = den(10)*den(14)
  den(16) = den(7)*den(9)
  den(17) = den(2)*den(14)
  den(19) = den(14)*den(18)
  den(21) = den(18)*den(20)
  den(22) = den(3)*den(21)
  den(25) = den(9)*den(24)
  den(26) = den(23)*den(25)
  den(27) = den(2)*den(24)
  den(28) = den(23)*den(27)
  den(30) = den(25)*den(29)
  den(31) = den(27)*den(29)
  den(32) = den(3)*den(25)
  den(33) = den(3)*den(27)
  den(34) = den(1)*den(20)
  den(35) = den(3)*den(34)
  den(36) = den(9)*den(20)
  den(37) = den(3)*den(36)
  den(39) = den(23)*den(38)
  den(40) = den(9)*den(39)
  den(41) = den(29)*den(38)
  den(42) = den(9)*den(41)
  den(44) = den(10)*den(43)
  den(45) = den(3)*den(44)
  den(46) = den(2)*den(43)
  den(47) = den(3)*den(46)
  den(48) = den(18)*den(43)
  den(49) = den(3)*den(48)
  den(50) = den(7)*den(18)
  den(51) = den(2)*den(39)
  den(52) = den(2)*den(41)
  den(53) = den(2)*den(3)
  den(55) = den(53)*den(54)
  den(56) = den(1)*den(55)
  den(57) = den(1)*den(3)
  den(59) = den(57)*den(58)
  den(60) = den(2)*den(59)
  den(62) = den(4)*den(61)
  den(63) = den(3)*den(62)
  den(64) = den(1)**2
  den(65) = den(7)*den(64)
  den(66) = den(3)**2
  den(67) = den(34)*den(66)
  den(68) = den(7)*den(34)
  den(69) = den(3)*den(10)
  den(71) = den(69)*den(70)
  den(72) = den(9)*den(71)
  den(73) = den(3)*den(9)
  den(75) = den(73)*den(74)
  den(76) = den(10)*den(75)
  den(77) = den(11)*den(61)
  den(78) = den(3)*den(77)
  den(79) = den(44)*den(66)
  den(80) = den(14)*den(44)
  den(81) = den(10)**2
  den(82) = den(14)*den(81)
  den(83) = den(9)**2
  den(84) = den(7)*den(83)
  den(85) = den(36)*den(66)
  den(86) = den(7)*den(36)
  den(87) = den(46)*den(66)
  den(88) = den(14)*den(46)
  den(89) = den(2)**2
  den(90) = den(14)*den(89)
  den(91) = den(14)*den(48)
  den(92) = den(7)*den(21)
  den(93) = den(48)*den(66)
  den(94) = den(21)*den(66)
  den(95) = den(39)*den(83)
  den(96) = den(25)*den(39)
  den(97) = den(23)**2
  den(98) = den(25)*den(97)
  den(99) = den(39)*den(89)
  den(100) = den(27)*den(39)
  den(101) = den(27)*den(97)
  den(102) = den(41)*den(83)
  den(103) = den(25)*den(41)
  den(104) = den(29)**2
  den(105) = den(25)*den(104)
  den(106) = den(41)*den(89)
  den(107) = den(27)*den(41)
  den(108) = den(27)*den(104)
  den(109) = den(14)*den(58)
  den(110) = den(14)*den(74)
  den(111) = den(7)*den(70)
  den(112) = den(7)*den(54)
  den(113) = den(34)*den(61)
  den(114) = den(44)*den(61)
  den(115) = den(39)*den(58)
  den(116) = den(39)*den(70)
  den(117) = den(41)*den(58)
  den(118) = den(41)*den(70)
  den(120) = den(25)*den(119)
  den(122) = den(25)*den(121)
  den(123) = den(36)*den(61)
  den(124) = den(27)*den(119)
  den(125) = den(27)*den(121)
  den(126) = den(46)*den(61)
  den(127) = den(21)*den(61)
  den(128) = den(48)*den(61)
  den(129) = den(1)*den(2)*den(3)
  den(130) = den(3)*den(9)*den(10)
  den(131) = den(3)*den(18)
  den(132) = den(9)*den(23)
  den(133) = den(2)*den(23)
  den(134) = den(9)*den(29)
  den(135) = den(2)*den(29)
  den(136) = den(3)*den(113)
  den(137) = den(1)*den(112)
  den(138) = den(10)*den(110)
  den(139) = den(3)*den(114)
  den(140) = den(3)*den(123)
  den(141) = den(9)*den(111)
  den(142) = den(2)*den(109)
  den(143) = den(3)*den(126)
  den(144) = den(3)*den(127)
  den(145) = den(3)*den(128)
  den(146) = den(23)*den(122)
  den(147) = den(9)*den(116)
  den(148) = den(23)*den(125)
  den(149) = den(2)*den(115)
  den(150) = den(29)*den(120)
  den(151) = den(9)*den(118)
  den(152) = den(29)*den(124)
  den(153) = den(2)*den(117)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(94)

  A(1) = cont_VV(wf(:,3),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_VV(wf(:,3),wf(:,14)) * den(12)
  A(4) = cont_QA(wf(:,16),wf(:,17)) * den(15)
  A(5) = cont_QA(wf(:,9),wf(:,18)) * den(16)
  A(6) = cont_QA(wf(:,17),wf(:,19)) * den(17)
  A(7) = cont_QA(wf(:,17),wf(:,22)) * den(19)
  A(8) = cont_QA(wf(:,7),wf(:,24)) * den(22)
  A(9) = cont_VV(wf(:,27),wf(:,28)) * den(26)
  A(10) = cont_VV(wf(:,28),wf(:,29)) * den(28)
  A(11) = cont_VV(wf(:,27),wf(:,32)) * den(30)
  A(12) = cont_VV(wf(:,29),wf(:,32)) * den(31)

  A(13) = cont_VV(wf(:,3),wf(:,33)) * den(5)
  A(14) = cont_QA(wf(:,9),wf(:,34)) * den(8)
  A(15) = cont_VV(wf(:,3),wf(:,35)) * den(12)
  A(16) = cont_QA(wf(:,17),wf(:,36)) * den(15)
  A(17) = cont_VV(wf(:,27),wf(:,37)) * den(32)
  A(18) = cont_QA(wf(:,9),wf(:,38)) * den(16)
  A(19) = cont_VV(wf(:,29),wf(:,37)) * den(33)
  A(20) = cont_QA(wf(:,17),wf(:,39)) * den(17)
  A(21) = cont_VV(wf(:,27),wf(:,40)) * den(26)
  A(22) = cont_VV(wf(:,29),wf(:,40)) * den(28)
  A(23) = cont_VV(wf(:,27),wf(:,43)) * den(30)
  A(24) = cont_VV(wf(:,29),wf(:,43)) * den(31)
  A(25) = cont_VV(wf(:,27),wf(:,44)) * den(30)
  A(26) = cont_VV(wf(:,29),wf(:,44)) * den(31)
  A(27) = cont_VV(wf(:,27),wf(:,47)) * den(26)
  A(28) = cont_VV(wf(:,29),wf(:,47)) * den(28)
  A(29) = cont_VV(wf(:,6),wf(:,48)) * den(5)
  A(30) = cont_QA(wf(:,8),wf(:,50)) * den(8)
  A(31) = cont_VV(wf(:,14),wf(:,48)) * den(12)
  A(32) = cont_QA(wf(:,16),wf(:,52)) * den(15)
  A(33) = cont_QA(wf(:,18),wf(:,50)) * den(16)
  A(34) = cont_QA(wf(:,19),wf(:,52)) * den(17)
  A(35) = cont_QA(wf(:,24),wf(:,49)) * den(22)
  A(36) = cont_QA(wf(:,22),wf(:,52)) * den(19)
  A(37) = cont_QA(wf(:,53),wf(:,54)) * den(35)
  A(38) = cont_VV(wf(:,3),wf(:,57)) * den(5)
  A(39) = cont_QA(wf(:,53),wf(:,58)) * den(37)
  A(40) = cont_QA(wf(:,17),wf(:,59)) * den(19)
  A(41) = cont_QA(wf(:,24),wf(:,53)) * den(22)
  A(42) = cont_QA(wf(:,17),wf(:,60)) * den(17)
  A(43) = cont_VV(wf(:,3),wf(:,63)) * den(12)
  A(44) = cont_QA(wf(:,17),wf(:,64)) * den(15)
  A(45) = cont_VV(wf(:,28),wf(:,65)) * den(40)
  A(46) = cont_VV(wf(:,28),wf(:,66)) * den(28)
  A(47) = cont_VV(wf(:,32),wf(:,65)) * den(42)
  A(48) = cont_VV(wf(:,32),wf(:,66)) * den(31)
  A(49) = cont_QA(wf(:,67),wf(:,68)) * den(45)
  A(50) = cont_VV(wf(:,3),wf(:,71)) * den(12)
  A(51) = cont_QA(wf(:,67),wf(:,72)) * den(47)
  A(52) = cont_QA(wf(:,67),wf(:,73)) * den(49)
  A(53) = cont_QA(wf(:,9),wf(:,74)) * den(50)
  A(54) = cont_QA(wf(:,9),wf(:,75)) * den(16)
  A(55) = cont_VV(wf(:,3),wf(:,78)) * den(5)
  A(56) = cont_QA(wf(:,9),wf(:,79)) * den(8)
  A(57) = cont_VV(wf(:,28),wf(:,80)) * den(51)
  A(58) = cont_VV(wf(:,28),wf(:,81)) * den(26)
  A(59) = cont_VV(wf(:,32),wf(:,80)) * den(52)
  A(60) = cont_VV(wf(:,32),wf(:,81)) * den(30)
  A(61) = cont_QA(wf(:,83),wf(:,84)) * den(56)
  A(62) = cont_QA(wf(:,86),wf(:,87)) * den(60)
  A(63) = cont_VV(wf(:,6),wf(:,88)) * den(63)
  A(64) = cont_QA(wf(:,9),wf(:,90)) * den(65)
  A(65) = cont_QA(wf(:,54),wf(:,91)) * den(67)
  A(66) = cont_QA(wf(:,54),wf(:,92)) * den(68)
  A(67) = cont_QA(wf(:,94),wf(:,95)) * den(72)
  A(68) = cont_QA(wf(:,97),wf(:,98)) * den(76)
  A(69) = cont_VV(wf(:,14),wf(:,88)) * den(78)
  A(70) = cont_QA(wf(:,68),wf(:,99)) * den(79)
  A(71) = cont_QA(wf(:,68),wf(:,100)) * den(80)
  A(72) = cont_QA(wf(:,17),wf(:,102)) * den(82)
  A(73) = cont_QA(wf(:,9),wf(:,104)) * den(84)
  A(74) = cont_QA(wf(:,58),wf(:,91)) * den(85)
  A(75) = cont_QA(wf(:,58),wf(:,92)) * den(86)
  A(76) = cont_QA(wf(:,72),wf(:,99)) * den(87)
  A(77) = cont_QA(wf(:,72),wf(:,100)) * den(88)
  A(78) = cont_QA(wf(:,17),wf(:,106)) * den(90)
  A(79) = cont_QA(wf(:,73),wf(:,100)) * den(91)
  A(80) = cont_QA(wf(:,9),wf(:,107)) * den(92)
  A(81) = cont_QA(wf(:,73),wf(:,99)) * den(93)
  A(82) = cont_QA(wf(:,24),wf(:,91)) * den(94)
  A(83) = cont_VV(wf(:,28),wf(:,108)) * den(95)
  A(84) = cont_VV(wf(:,28),wf(:,109)) * den(96)
  A(85) = cont_VV(wf(:,27),wf(:,112)) * den(98)
  A(86) = cont_VV(wf(:,28),wf(:,113)) * den(99)
  A(87) = cont_VV(wf(:,28),wf(:,114)) * den(100)
  A(88) = cont_VV(wf(:,29),wf(:,112)) * den(101)
  A(89) = cont_VV(wf(:,32),wf(:,108)) * den(102)
  A(90) = cont_VV(wf(:,32),wf(:,109)) * den(103)
  A(91) = cont_VV(wf(:,27),wf(:,117)) * den(105)
  A(92) = cont_VV(wf(:,32),wf(:,113)) * den(106)
  A(93) = cont_VV(wf(:,32),wf(:,114)) * den(107)
  A(94) = cont_VV(wf(:,29),wf(:,117)) * den(108)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(94)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(7)+A(8))*f(1))/2._/**/REALKIND+((A(1)+A(2)+A(3)+A(4)+A(5)+A(6)+A(9)+A(10)+A(11)+A(12))*f(5))/2._/**/REALKIND
  M1(2) = ((-A(7)-A(8))*f(1))/6._/**/REALKIND+((-A(1)-A(2)-A(3)-A(4)-A(5)-A(6)-A(9)-A(10)-A(11)-A(12))*f(5))/6._/**/REALKIND

  M2(1) = ((-A(79)-A(80)-A(81)-A(82))*f(2))/2._/**/REALKIND+((A(35)+A(36)+A(41)+A(52))*f(3))/2._/**/REALKIND+((A(40) &
       +A(53))*f(4))/2._/**/REALKIND+((-A(61)-A(62)-A(63)-A(64)-A(65)-A(66)-A(67)-A(68)-A(69)-A(70)-A(71)-A(72)-A(73)-A(74)-A(75) &
       -A(76)-A(77)-A(78)-A(83)-A(84)-A(85)-A(86)-A(87)-A(88)-A(89)-A(90)-A(91)-A(92)-A(93)-A(94))*f(6))/2._/**/REALKIND+((A(13) &
       +A(15)+A(21)+A(22)+A(25)+A(26)+A(29)+A(30)+A(31)+A(32)+A(33)+A(34)+A(37)+A(39)+A(45)+A(47)+A(49)+A(51)+A(57) &
       +A(59))*f(7))/2._/**/REALKIND+((A(14)+A(16)+A(18)+A(20)+A(23)+A(24)+A(27)+A(28)+A(38)+A(42)+A(43)+A(44)+A(46)+A(48)+A(50) &
       +A(54)+A(55)+A(56)+A(58)+A(60))*f(8))/2._/**/REALKIND+((-A(17)-A(19))*f(9))/2._/**/REALKIND
  M2(2) = ((A(79)+A(80)+A(81)+A(82))*f(2))/6._/**/REALKIND+((-A(35)-A(36)-A(41)-A(52))*f(3))/6._/**/REALKIND+((-A(40) &
       -A(53))*f(4))/6._/**/REALKIND+((A(61)+A(62)+A(63)+A(64)+A(65)+A(66)+A(67)+A(68)+A(69)+A(70)+A(71)+A(72)+A(73)+A(74)+A(75) &
       +A(76)+A(77)+A(78)+A(83)+A(84)+A(85)+A(86)+A(87)+A(88)+A(89)+A(90)+A(91)+A(92)+A(93)+A(94))*f(6))/6._/**/REALKIND+((-A(13) &
       -A(15)-A(21)-A(22)-A(25)-A(26)-A(29)-A(30)-A(31)-A(32)-A(33)-A(34)-A(37)-A(39)-A(45)-A(47)-A(49)-A(51)-A(57) &
       -A(59))*f(7))/6._/**/REALKIND+((-A(14)-A(16)-A(18)-A(20)-A(23)-A(24)-A(27)-A(28)-A(38)-A(42)-A(43)-A(44)-A(46)-A(48)-A(50) &
       -A(54)-A(55)-A(56)-A(58)-A(60))*f(8))/6._/**/REALKIND+((A(17)+A(19))*f(9))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_ppzwjj_udxssxzw_1_/**/REALKIND
