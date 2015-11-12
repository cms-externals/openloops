
module ol_colourmatrix_pphlnjj_neexuxdddxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(60,2), K2(2,2), KL(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9,   3]
  K1( 2,:) = [   3,   9]
  K1( 3,:) = [   0,   0]
  K1( 4,:) = [   0,   0]
  K1( 5,:) = [   0,   0]
  K1( 6,:) = [   0,   0]
  K1( 7,:) = [   0,   0]
  K1( 8,:) = [   0,   0]
  K1( 9,:) = [   0,   0]
  K1(10,:) = [   0,   0]
  K1(11,:) = [   0,   0]
  K1(12,:) = [   0,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [   0,   0]
  K1(16,:) = [   0,   0]
  K1(17,:) = [   0,   0]
  K1(18,:) = [   0,   0]
  K1(19,:) = [   0,  -4]
  K1(20,:) = [  -4, -12]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [ -12,  -4]
  K1(28,:) = [  -4,   0]
  K1(29,:) = [   0,   4]
  K1(30,:) = [   4,   0]
  K1(31,:) = [  12,   4]
  K1(32,:) = [   4,  12]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   4]
  K1(38,:) = [   4,   0]
  K1(39,:) = [ -12,  -4]
  K1(40,:) = [  -4,   0]
  K1(41,:) = [   0,  -4]
  K1(42,:) = [  -4, -12]
  K1(43,:) = [  12,   4]
  K1(44,:) = [   4,  12]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]
  K1(47,:) = [   0,   0]
  K1(48,:) = [   0,   0]
  K1(49,:) = [   0,   0]
  K1(50,:) = [   0,   0]
  K1(51,:) = [   0,   0]
  K1(52,:) = [   0,   0]
  K1(53,:) = [   0,   0]
  K1(54,:) = [   0,   0]
  K1(55,:) = [   0,   0]
  K1(56,:) = [   0,   0]
  K1(57,:) = [   0,   0]
  K1(58,:) = [   0,   0]
  K1(59,:) = [   0,   0]
  K1(60,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphlnjj_neexuxdddxh_1_/**/REALKIND



module ol_forced_parameters_pphlnjj_neexuxdddxh_1_/**/REALKIND
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
  if (YE /= 0) write(*,101) 'YE = 0'
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
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphlnjj_neexuxdddxh_1_/**/REALKIND

module ol_loop_pphlnjj_neexuxdddxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(13), c(21)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-7+1:82)
  ! denominators
  complex(REALKIND), save :: den(74)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,64)
  ! zero helicity identifier
  logical,           save :: zerohel(64) = .true., zerohel_ct(64) = .true.

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
    f( 1) = (CI*eQED**3*gQCD**2*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f( 2) = (CI*countertermnorm*eQED**3*gQCD**4*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f( 3) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**4*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f( 4) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**4*lambdaHWW*MW)/(2._/**/REALKIND*sw**3)
    f( 5) = (CI*eQED**3*gQCD**4*integralnorm*lambdaHWW*MW*SwB)/(2._/**/REALKIND*sw**3)
    f( 6) = (eQED**3*gQCD**4*integralnorm*lambdaHWW*MW*SwB)/(sw**3*4._/**/REALKIND)
    f( 7) = (eQED**3*gQCD**4*integralnorm*lambdaHWW*MW*SwB)/(sw**3*2._/**/REALKIND)
    f( 8) = (eQED**3*gQCD**4*integralnorm*lambdaHWW*MW*SwF)/(sw**3*2._/**/REALKIND)
    f( 9) = (eQED**3*gQCD**4*integralnorm*lambdaHWW*MW*SwF)/sw**3
    f(10) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MB*YB)/(2._/**/REALKIND*MQ2sum*sw**2)
    f(11) = (eQED**3*gQCD**4*integralnorm*SwF*YB)/(MW*sw**3*4._/**/REALKIND)
    f(12) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**4*MT*YT)/(2._/**/REALKIND*MQ2sum*sw**2)
    f(13) = (eQED**3*gQCD**4*integralnorm*SwF*YT)/(MW*sw**3*4._/**/REALKIND)

  c = [ 9*CI*f(5), 27*CI*f(5), 18*f(6), 54*f(6), f(7), 3*f(7), 6*f(7), 8*f(7), 10*f(7), 18*f(7), 21*f(7), 24*f(7), 54*f(7), 3*f(8) &
    , 9*f(8), 3*f(9), 9*f(9), 3*f(11), 9*f(11), 3*f(13), 9*f(13) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,7)
  integer,           intent(in)  :: H(7)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(32)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_Q(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rZERO, H(6), wf(:,-5))
  call wf_S(P(:,7), rMH, H(7), wf(:,-6))

  ! internal WFs
  call vert_QA_W(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-3),wf(:,-5),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MW,1_intkind1,wf(:,3))
  call vert_AV_Q(wf(:,-2),wf(:,2),wf(:,4))
  call vert_SV_V(wf(:,-6),wf(:,3),wf(:,5))
  call prop_A_Q(wf(:,4),Q(:,44),ZERO,0_intkind1,wf(:,6))
  call prop_W_W(wf(:,5),Q(:,67),MW,1_intkind1,wf(:,7))
  call vert_QA_W(wf(:,-4),wf(:,6),wf(:,8))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,9))
  call prop_Q_A(wf(:,9),Q(:,56),ZERO,0_intkind1,wf(:,10))
  call vert_QA_W(wf(:,10),wf(:,-2),wf(:,11))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,12))
  call vert_AV_Q(wf(:,-2),wf(:,12),wf(:,13))
  call prop_A_Q(wf(:,13),Q(:,52),ZERO,0_intkind1,wf(:,14))
  call vert_QA_W(wf(:,-3),wf(:,14),wf(:,15))
  call vert_VQ_A(wf(:,12),wf(:,-3),wf(:,16))
  call prop_Q_A(wf(:,16),Q(:,56),ZERO,0_intkind1,wf(:,17))
  call vert_QA_W(wf(:,17),wf(:,-2),wf(:,18))
  call vert_AW_Q(wf(:,-2),wf(:,3),wf(:,19))
  call counter_SG_G(wf(:,-6),wf(:,2),wf(:,20))
  call prop_A_Q(wf(:,19),Q(:,7),ZERO,0_intkind1,wf(:,21))
  call vert_QA_V(wf(:,-4),wf(:,21),wf(:,22))
  call vert_WQ_A(wf(:,3),wf(:,-4),wf(:,23))
  call prop_Q_A(wf(:,23),Q(:,19),ZERO,0_intkind1,wf(:,24))
  call vert_QA_V(wf(:,24),wf(:,-2),wf(:,25))
  call counter_SG_G(wf(:,-6),wf(:,12),wf(:,26))
  call vert_QA_V(wf(:,-3),wf(:,21),wf(:,27))
  call vert_WQ_A(wf(:,3),wf(:,-3),wf(:,28))
  call prop_Q_A(wf(:,28),Q(:,11),ZERO,0_intkind1,wf(:,29))
  call vert_QA_V(wf(:,29),wf(:,-2),wf(:,30))
  call counter_QA_W(wf(:,-4),wf(:,6),wf(:,31))
  call counter_VQ_A(wf(:,2),wf(:,-4),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,56),ZERO,0_intkind1,wf(:,33))
  call vert_QA_W(wf(:,33),wf(:,-2),wf(:,34))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,35))
  call vert_VQ_A(wf(:,35),wf(:,-3),wf(:,36))
  call prop_Q_A(wf(:,36),Q(:,56),ZERO,0_intkind1,wf(:,37))
  call vert_QA_W(wf(:,37),wf(:,-2),wf(:,38))
  call vert_AV_Q(wf(:,-2),wf(:,35),wf(:,39))
  call prop_A_Q(wf(:,39),Q(:,52),ZERO,0_intkind1,wf(:,40))
  call vert_QA_W(wf(:,-3),wf(:,40),wf(:,41))
  call counter_QA_W(wf(:,-3),wf(:,14),wf(:,42))
  call counter_VQ_A(wf(:,12),wf(:,-3),wf(:,43))
  call prop_Q_A(wf(:,43),Q(:,56),ZERO,0_intkind1,wf(:,44))
  call vert_QA_W(wf(:,44),wf(:,-2),wf(:,45))
  call counter_QA_V(wf(:,-3),wf(:,-5),wf(:,46))
  call vert_VQ_A(wf(:,46),wf(:,-4),wf(:,47))
  call prop_Q_A(wf(:,47),Q(:,56),ZERO,0_intkind1,wf(:,48))
  call vert_QA_W(wf(:,48),wf(:,-2),wf(:,49))
  call vert_AV_Q(wf(:,-2),wf(:,46),wf(:,50))
  call prop_A_Q(wf(:,50),Q(:,44),ZERO,0_intkind1,wf(:,51))
  call vert_QA_W(wf(:,-4),wf(:,51),wf(:,52))
  call counter_AV_Q(wf(:,-2),wf(:,2),wf(:,53))
  call prop_A_Q(wf(:,53),Q(:,44),ZERO,0_intkind1,wf(:,54))
  call vert_QA_W(wf(:,-4),wf(:,54),wf(:,55))
  call counter_QA_W(wf(:,10),wf(:,-2),wf(:,56))
  call counter_AV_Q(wf(:,-2),wf(:,12),wf(:,57))
  call prop_A_Q(wf(:,57),Q(:,52),ZERO,0_intkind1,wf(:,58))
  call vert_QA_W(wf(:,-3),wf(:,58),wf(:,59))
  call counter_QA_W(wf(:,17),wf(:,-2),wf(:,60))
  call counter_V_V(ctGG,wf(:,2),Q(:,40),wf(:,61))
  call vert_AV_Q(wf(:,-2),wf(:,61),wf(:,62))
  call vert_WQ_A(wf(:,7),wf(:,-4),wf(:,63))
  call prop_A_Q(wf(:,62),Q(:,44),ZERO,0_intkind1,wf(:,64))
  call counter_A_Q(ctqq,wf(:,6),Q(:,44),wf(:,65))
  call prop_Q_A(wf(:,63),Q(:,83),ZERO,0_intkind1,wf(:,66))
  call vert_AW_Q(wf(:,-2),wf(:,7),wf(:,67))
  call counter_Q_A(ctqq,wf(:,10),Q(:,56),wf(:,68))
  call prop_A_Q(wf(:,67),Q(:,71),ZERO,0_intkind1,wf(:,69))
  call vert_VQ_A(wf(:,61),wf(:,-4),wf(:,70))
  call vert_WQ_A(wf(:,7),wf(:,-3),wf(:,71))
  call counter_A_Q(ctqq,wf(:,14),Q(:,52),wf(:,72))
  call prop_Q_A(wf(:,71),Q(:,75),ZERO,0_intkind1,wf(:,73))
  call counter_Q_A(ctqq,wf(:,17),Q(:,56),wf(:,74))
  call counter_V_V(ctGG,wf(:,12),Q(:,48),wf(:,75))
  call vert_AV_Q(wf(:,-2),wf(:,75),wf(:,76))
  call prop_A_Q(wf(:,76),Q(:,52),ZERO,0_intkind1,wf(:,77))
  call vert_VQ_A(wf(:,75),wf(:,-3),wf(:,78))
  call vert_QA_V(wf(:,-3),wf(:,69),wf(:,79))
  call vert_QA_V(wf(:,73),wf(:,-2),wf(:,80))
  call vert_QA_V(wf(:,-4),wf(:,69),wf(:,81))
  call vert_QA_V(wf(:,66),wf(:,-2),wf(:,82))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,40))
  den(2) = 1 / (Q(5,3) - MW2)
  den(3) = 1 / (Q(5,44))
  den(5) = 1 / (Q(5,67) - MW2)
  den(8) = 1 / (Q(5,56))
  den(11) = 1 / (Q(5,48))
  den(12) = 1 / (Q(5,52))
  den(17) = 1 / (Q(5,7))
  den(19) = 1 / (Q(5,104))
  den(22) = 1 / (Q(5,19))
  den(25) = 1 / (Q(5,112))
  den(28) = 1 / (Q(5,11))
  den(34) = 1 / (Q(5,83))
  den(37) = 1 / (Q(5,71))
  den(41) = 1 / (Q(5,75))
  den(49) = 1 / (Q(5,23))
  den(52) = 1 / (Q(5,15))
  den(55) = 1 / (Q(5,79))
  den(58) = 1 / (Q(5,87))

  ! denominators
  den(4) = den(1)*den(3)
  den(6) = den(2)*den(5)
  den(7) = den(4)*den(6)
  den(9) = den(1)*den(8)
  den(10) = den(6)*den(9)
  den(13) = den(11)*den(12)
  den(14) = den(6)*den(13)
  den(15) = den(8)*den(11)
  den(16) = den(6)*den(15)
  den(18) = den(2)*den(17)
  den(20) = den(1)*den(19)
  den(21) = den(18)*den(20)
  den(23) = den(2)*den(22)
  den(24) = den(20)*den(23)
  den(26) = den(11)*den(25)
  den(27) = den(18)*den(26)
  den(29) = den(2)*den(28)
  den(30) = den(26)*den(29)
  den(31) = den(1)**2
  den(32) = den(3)*den(31)
  den(33) = den(6)*den(32)
  den(35) = den(6)*den(34)
  den(36) = den(4)*den(35)
  den(38) = den(6)*den(37)
  den(39) = den(9)*den(38)
  den(40) = den(31)*den(38)
  den(42) = den(6)*den(41)
  den(43) = den(13)*den(42)
  den(44) = den(15)*den(38)
  den(45) = den(11)**2
  den(46) = den(12)*den(45)
  den(47) = den(6)*den(46)
  den(48) = den(38)*den(45)
  den(50) = den(18)*den(49)
  den(51) = den(23)*den(49)
  den(53) = den(18)*den(52)
  den(54) = den(29)*den(52)
  den(56) = den(38)*den(55)
  den(57) = den(42)*den(55)
  den(59) = den(38)*den(58)
  den(60) = den(35)*den(58)
  den(61) = den(1)*den(50)
  den(62) = den(1)*den(51)
  den(63) = den(1)*den(38)
  den(64) = den(1)*den(35)
  den(65) = den(1)*den(6)
  den(66) = den(11)*den(53)
  den(67) = den(11)*den(54)
  den(68) = den(11)*den(38)
  den(69) = den(11)*den(42)
  den(70) = den(6)*den(11)
  den(71) = den(1)*den(59)
  den(72) = den(1)*den(60)
  den(73) = den(11)*den(56)
  den(74) = den(11)*den(57)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(32)

  A(1) = cont_VV(wf(:,7),wf(:,8)) * den(7)
  A(2) = cont_VV(wf(:,7),wf(:,11)) * den(10)
  A(3) = cont_VV(wf(:,7),wf(:,15)) * den(14)
  A(4) = cont_VV(wf(:,7),wf(:,18)) * den(16)

  A(5) = cont_VV(wf(:,20),wf(:,22)) * den(21)
  A(6) = cont_VV(wf(:,20),wf(:,22)) * den(21)
  A(7) = cont_VV(wf(:,20),wf(:,25)) * den(24)
  A(8) = cont_VV(wf(:,20),wf(:,25)) * den(24)
  A(9) = cont_VV(wf(:,26),wf(:,27)) * den(27)
  A(10) = cont_VV(wf(:,26),wf(:,27)) * den(27)
  A(11) = cont_VV(wf(:,26),wf(:,30)) * den(30)
  A(12) = cont_VV(wf(:,26),wf(:,30)) * den(30)
  A(13) = cont_VV(wf(:,7),wf(:,31)) * den(7)
  A(14) = cont_VV(wf(:,7),wf(:,34)) * den(10)
  A(15) = cont_VV(wf(:,7),wf(:,38)) * den(16)
  A(16) = cont_VV(wf(:,7),wf(:,41)) * den(14)
  A(17) = cont_VV(wf(:,7),wf(:,42)) * den(14)
  A(18) = cont_VV(wf(:,7),wf(:,45)) * den(16)
  A(19) = cont_VV(wf(:,7),wf(:,49)) * den(10)
  A(20) = cont_VV(wf(:,7),wf(:,52)) * den(7)
  A(21) = cont_VV(wf(:,7),wf(:,55)) * den(7)
  A(22) = cont_VV(wf(:,7),wf(:,56)) * den(10)
  A(23) = cont_VV(wf(:,7),wf(:,59)) * den(14)
  A(24) = cont_VV(wf(:,7),wf(:,60)) * den(16)
  A(25) = cont_QA(wf(:,63),wf(:,64)) * den(33)
  A(26) = cont_QA(wf(:,65),wf(:,66)) * den(36)
  A(27) = cont_QA(wf(:,68),wf(:,69)) * den(39)
  A(28) = cont_QA(wf(:,69),wf(:,70)) * den(40)
  A(29) = cont_QA(wf(:,72),wf(:,73)) * den(43)
  A(30) = cont_QA(wf(:,69),wf(:,74)) * den(44)
  A(31) = cont_QA(wf(:,71),wf(:,77)) * den(47)
  A(32) = cont_QA(wf(:,69),wf(:,78)) * den(48)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(32)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(1)-A(2))*f(1))/6._/**/REALKIND+((-A(3)-A(4))*f(1))/2._/**/REALKIND
  M1(2) = ((A(1)+A(2))*f(1))/2._/**/REALKIND+((A(3)+A(4))*f(1))/6._/**/REALKIND

  M2(1) = ((A(25)+A(26)+A(27)+A(28))*f(2))/6._/**/REALKIND+((A(29)+A(30)+A(31)+A(32))*f(2))/2._/**/REALKIND+((-A(14)-A(19)-A(20) &
       -A(21))*f(3))/6._/**/REALKIND+((-A(15)-A(16)-A(18)-A(23))*f(3))/2._/**/REALKIND+((-A(13)-A(22))*f(4))/6._/**/REALKIND+(( &
       -A(17)-A(24))*f(4))/2._/**/REALKIND+((-A(6)-A(8))*f(10))/6._/**/REALKIND+((-A(10)-A(12))*f(10))/2._/**/REALKIND+((-A(5) &
       -A(7))*f(12))/6._/**/REALKIND+((-A(9)-A(11))*f(12))/2._/**/REALKIND
  M2(2) = ((-A(25)-A(26)-A(27)-A(28))*f(2))/2._/**/REALKIND+((-A(29)-A(30)-A(31)-A(32))*f(2))/6._/**/REALKIND+((A(14)+A(19)+A(20) &
       +A(21))*f(3))/2._/**/REALKIND+((A(15)+A(16)+A(18)+A(23))*f(3))/6._/**/REALKIND+((A(13)+A(22))*f(4))/2._/**/REALKIND+((A(17) &
       +A(24))*f(4))/6._/**/REALKIND+((A(6)+A(8))*f(10))/2._/**/REALKIND+((A(10)+A(12))*f(10))/6._/**/REALKIND+((A(5) &
       +A(7))*f(12))/2._/**/REALKIND+((A(9)+A(11))*f(12))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pphlnjj_neexuxdddxh_1_/**/REALKIND
