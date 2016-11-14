
module ol_colourmatrix_ppvvv_uxdazwx_1_/**/REALKIND
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
end module ol_colourmatrix_ppvvv_uxdazwx_1_/**/REALKIND



module ol_forced_parameters_ppvvv_uxdazwx_1_/**/REALKIND
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
end module ol_forced_parameters_ppvvv_uxdazwx_1_/**/REALKIND

module ol_loop_ppvvv_uxdazwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(23), c(6)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:104)
  ! denominators
  complex(REALKIND), save :: den(62)
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
    f( 1) = (CI*cw*eQED**3)/(3._/**/REALKIND*sqrt2*sw**2)
    f( 2) = (2*CI*cw*eQED**3)/(3._/**/REALKIND*sqrt2*sw**2)
    f( 3) = (CI*cw*eQED**3)/(sqrt2*sw**2)
    f( 4) = (CI*countertermnorm*cw*eQED**3*gQCD**2)/(3._/**/REALKIND*sqrt2*sw**2)
    f( 5) = (2*CI*countertermnorm*cw*eQED**3*gQCD**2)/(3._/**/REALKIND*sqrt2*sw**2)
    f( 6) = (CI*countertermnorm*ctVqq*cw*eQED**3*gQCD**2)/(3._/**/REALKIND*sqrt2*sw**2)
    f( 7) = (2*CI*countertermnorm*ctVqq*cw*eQED**3*gQCD**2)/(3._/**/REALKIND*sqrt2*sw**2)
    f( 8) = (CI*countertermnorm*ctVqq*cw*eQED**3*gQCD**2)/(sqrt2*sw**2)
    f( 9) = (CI*eQED**3)/(3._/**/REALKIND*sqrt2*sw)
    f(10) = (2*CI*eQED**3)/(3._/**/REALKIND*sqrt2*sw)
    f(11) = (CI*eQED**3)/(sqrt2*sw)
    f(12) = (CI*countertermnorm*eQED**3*gQCD**2)/(3._/**/REALKIND*sqrt2*sw)
    f(13) = (2*CI*countertermnorm*eQED**3*gQCD**2)/(3._/**/REALKIND*sqrt2*sw)
    f(14) = (CI*countertermnorm*eQED**3*gQCD**2)/(sqrt2*sw)
    f(15) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2)/(3._/**/REALKIND*sqrt2*sw)
    f(16) = (2*CI*countertermnorm*ctVqq*eQED**3*gQCD**2)/(3._/**/REALKIND*sqrt2*sw)
    f(17) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2)/(sqrt2*sw)
    f(18) = (cw*eQED**3*gQCD**2*integralnorm*SwB)/(sqrt2*sw**2*3._/**/REALKIND)
    f(19) = (2*cw*eQED**3*gQCD**2*integralnorm*SwB)/(sqrt2*sw**2*3._/**/REALKIND)
    f(20) = (cw*eQED**3*gQCD**2*integralnorm*SwB)/(sqrt2*sw**2)
    f(21) = (eQED**3*gQCD**2*integralnorm*SwB)/(sqrt2*sw*3._/**/REALKIND)
    f(22) = (2*eQED**3*gQCD**2*integralnorm*SwB)/(sqrt2*sw*3._/**/REALKIND)
    f(23) = (eQED**3*gQCD**2*integralnorm*SwB)/(sqrt2*sw)

  c = [ 4*f(18), 4*f(19), 4*f(20), 4*f(21), 4*f(22), 4*f(23) ]
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
  complex(REALKIND) :: A(58)
  ! external WFs
  call wf_A(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_V(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_V(P(:,4), rMZ, H(4), wf(:,-3))
  call wf_V(P(:,5), rMW, H(5), wf(:,-4))

  ! internal WFs
  call vert_QA_W(wf(:,-1),wf(:,0),wf(:,1))
  call vert_WWV_V(wf(:,-2),wf(:,-3),wf(:,-4),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MW,1_intkind1,wf(:,3))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,4))
  call prop_W_W(wf(:,4),Q(:,20),MW,1_intkind1,wf(:,5))
  call vert_UV_W(wf(:,3),Q(:,3),wf(:,-3),Q(:,8),wf(:,6))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,7))
  call prop_W_W(wf(:,7),Q(:,24),MW,1_intkind1,wf(:,8))
  call vert_UV_W(wf(:,3),Q(:,3),wf(:,-2),Q(:,4),wf(:,9))
  call vert_AV_Q(wf(:,0),wf(:,-2),wf(:,10))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,-1),wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,5),ZERO,0_intkind1,wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,10),ZERO,0_intkind1,wf(:,13))
  call vert_AW_Q(wf(:,12),wf(:,-4),wf(:,14))
  call vert_WQ_A(wf(:,-4),wf(:,-1),wf(:,15))
  call prop_Q_A(wf(:,15),Q(:,18),ZERO,0_intkind1,wf(:,16))
  call vert_AZ_Q(gZu,wf(:,12),wf(:,-3),wf(:,17))
  call vert_QA_W(wf(:,-1),wf(:,12),wf(:,18))
  call vert_AZ_Q(gZu,wf(:,0),wf(:,-3),wf(:,19))
  call vert_VQ_A(wf(:,-2),wf(:,-1),wf(:,20))
  call prop_A_Q(wf(:,19),Q(:,9),ZERO,0_intkind1,wf(:,21))
  call prop_Q_A(wf(:,20),Q(:,6),ZERO,0_intkind1,wf(:,22))
  call vert_AW_Q(wf(:,21),wf(:,-4),wf(:,23))
  call vert_AW_Q(wf(:,0),wf(:,-4),wf(:,24))
  call prop_A_Q(wf(:,24),Q(:,17),ZERO,0_intkind1,wf(:,25))
  call vert_AZ_Q(gZd,wf(:,25),wf(:,-3),wf(:,26))
  call vert_QA_W(wf(:,22),wf(:,0),wf(:,27))
  call vert_AV_Q(wf(:,21),wf(:,-2),wf(:,28))
  call vert_QA_W(wf(:,-1),wf(:,21),wf(:,29))
  call vert_AV_Q(wf(:,25),wf(:,-2),wf(:,30))
  call vert_QA_W(wf(:,13),wf(:,0),wf(:,31))
  call counter_QA_W(wf(:,-1),wf(:,0),wf(:,32))
  call prop_W_W(wf(:,2),Q(:,28),MW,1_intkind1,wf(:,33))
  call counter_AW_Q(wf(:,12),wf(:,-4),wf(:,34))
  call counter_AZ_Q(gZu,wf(:,12),wf(:,-3),wf(:,35))
  call counter_AW_Q(wf(:,21),wf(:,-4),wf(:,36))
  call counter_AZ_Q(gZd,wf(:,25),wf(:,-3),wf(:,37))
  call counter_AV_Q(wf(:,21),wf(:,-2),wf(:,38))
  call counter_AV_Q(wf(:,25),wf(:,-2),wf(:,39))
  call counter_QA_W(wf(:,-1),wf(:,12),wf(:,40))
  call counter_WQ_A(wf(:,-4),wf(:,-1),wf(:,41))
  call prop_Q_A(wf(:,41),Q(:,18),ZERO,0_intkind1,wf(:,42))
  call counter_ZQ_A(gZd,wf(:,-3),wf(:,-1),wf(:,43))
  call prop_Q_A(wf(:,43),Q(:,10),ZERO,0_intkind1,wf(:,44))
  call counter_QA_W(wf(:,-1),wf(:,21),wf(:,45))
  call vert_QA_W(wf(:,44),wf(:,0),wf(:,46))
  call counter_VQ_A(wf(:,-2),wf(:,-1),wf(:,47))
  call prop_Q_A(wf(:,47),Q(:,6),ZERO,0_intkind1,wf(:,48))
  call vert_QA_W(wf(:,48),wf(:,0),wf(:,49))
  call counter_QA_W(wf(:,22),wf(:,0),wf(:,50))
  call counter_AW_Q(wf(:,0),wf(:,-4),wf(:,51))
  call prop_A_Q(wf(:,51),Q(:,17),ZERO,0_intkind1,wf(:,52))
  call vert_AZ_Q(gZd,wf(:,52),wf(:,-3),wf(:,53))
  call counter_AZ_Q(gZu,wf(:,0),wf(:,-3),wf(:,54))
  call prop_A_Q(wf(:,54),Q(:,9),ZERO,0_intkind1,wf(:,55))
  call vert_AW_Q(wf(:,55),wf(:,-4),wf(:,56))
  call counter_QA_W(wf(:,13),wf(:,0),wf(:,57))
  call vert_AV_Q(wf(:,52),wf(:,-2),wf(:,58))
  call vert_AV_Q(wf(:,55),wf(:,-2),wf(:,59))
  call vert_QA_W(wf(:,-1),wf(:,55),wf(:,60))
  call counter_AV_Q(wf(:,0),wf(:,-2),wf(:,61))
  call prop_A_Q(wf(:,61),Q(:,5),ZERO,0_intkind1,wf(:,62))
  call vert_AW_Q(wf(:,62),wf(:,-4),wf(:,63))
  call vert_AZ_Q(gZu,wf(:,62),wf(:,-3),wf(:,64))
  call vert_QA_W(wf(:,-1),wf(:,62),wf(:,65))
  call prop_W_W(wf(:,32),Q(:,3),MW,1_intkind1,wf(:,66))
  call vert_UV_W(wf(:,66),Q(:,3),wf(:,-3),Q(:,8),wf(:,67))
  call vert_UV_W(wf(:,66),Q(:,3),wf(:,-2),Q(:,4),wf(:,68))
  call vert_WQ_A(wf(:,-4),wf(:,13),wf(:,69))
  call counter_A_Q(ctqq,wf(:,12),Q(:,5),wf(:,70))
  call prop_Q_A(wf(:,69),Q(:,26),ZERO,0_intkind1,wf(:,71))
  call counter_Q_A(ctqq,wf(:,13),Q(:,10),wf(:,72))
  call prop_A_Q(wf(:,14),Q(:,21),ZERO,0_intkind1,wf(:,73))
  call vert_ZQ_A(gZu,wf(:,-3),wf(:,16),wf(:,74))
  call prop_Q_A(wf(:,74),Q(:,26),ZERO,0_intkind1,wf(:,75))
  call counter_Q_A(ctqq,wf(:,16),Q(:,18),wf(:,76))
  call prop_A_Q(wf(:,17),Q(:,13),ZERO,0_intkind1,wf(:,77))
  call vert_WQ_A(wf(:,8),wf(:,-1),wf(:,78))
  call prop_Q_A(wf(:,78),Q(:,26),ZERO,0_intkind1,wf(:,79))
  call vert_WQ_A(wf(:,-4),wf(:,22),wf(:,80))
  call counter_A_Q(ctqq,wf(:,21),Q(:,9),wf(:,81))
  call prop_Q_A(wf(:,80),Q(:,22),ZERO,0_intkind1,wf(:,82))
  call counter_Q_A(ctqq,wf(:,22),Q(:,6),wf(:,83))
  call prop_A_Q(wf(:,23),Q(:,25),ZERO,0_intkind1,wf(:,84))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,22),wf(:,85))
  call counter_A_Q(ctqq,wf(:,25),Q(:,17),wf(:,86))
  call prop_Q_A(wf(:,85),Q(:,14),ZERO,0_intkind1,wf(:,87))
  call prop_A_Q(wf(:,26),Q(:,25),ZERO,0_intkind1,wf(:,88))
  call vert_AW_Q(wf(:,0),wf(:,8),wf(:,89))
  call prop_A_Q(wf(:,89),Q(:,25),ZERO,0_intkind1,wf(:,90))
  call vert_VQ_A(wf(:,-2),wf(:,16),wf(:,91))
  call prop_Q_A(wf(:,91),Q(:,22),ZERO,0_intkind1,wf(:,92))
  call prop_A_Q(wf(:,28),Q(:,13),ZERO,0_intkind1,wf(:,93))
  call vert_WQ_A(wf(:,5),wf(:,-1),wf(:,94))
  call prop_Q_A(wf(:,94),Q(:,22),ZERO,0_intkind1,wf(:,95))
  call vert_VQ_A(wf(:,-2),wf(:,13),wf(:,96))
  call prop_Q_A(wf(:,96),Q(:,14),ZERO,0_intkind1,wf(:,97))
  call prop_A_Q(wf(:,30),Q(:,21),ZERO,0_intkind1,wf(:,98))
  call vert_AW_Q(wf(:,0),wf(:,5),wf(:,99))
  call prop_A_Q(wf(:,99),Q(:,21),ZERO,0_intkind1,wf(:,100))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,5),Q(:,20),wf(:,101))
  call prop_W_W(wf(:,101),Q(:,28),MW,1_intkind1,wf(:,102))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,8),Q(:,24),wf(:,103))
  call prop_W_W(wf(:,103),Q(:,28),MW,1_intkind1,wf(:,104))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3) - MW2)
  den(2) = 1 / (Q(5,20) - MW2)
  den(4) = 1 / (Q(5,24) - MW2)
  den(6) = 1 / (Q(5,5))
  den(7) = 1 / (Q(5,10))
  den(9) = 1 / (Q(5,18))
  den(12) = 1 / (Q(5,9))
  den(13) = 1 / (Q(5,6))
  den(15) = 1 / (Q(5,17))
  den(22) = 1 / (Q(5,28) - MW2)
  den(23) = 1 / (Q(5,26))
  den(26) = 1 / (Q(5,21))
  den(31) = 1 / (Q(5,13))
  den(36) = 1 / (Q(5,22))
  den(39) = 1 / (Q(5,25))
  den(42) = 1 / (Q(5,14))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(8) = den(6)*den(7)
  den(10) = den(6)*den(9)
  den(11) = den(4)*den(6)
  den(14) = den(12)*den(13)
  den(16) = den(13)*den(15)
  den(17) = den(4)*den(13)
  den(18) = den(9)*den(12)
  den(19) = den(2)*den(12)
  den(20) = den(7)*den(15)
  den(21) = den(2)*den(7)
  den(24) = den(7)*den(23)
  den(25) = den(6)*den(24)
  den(27) = den(6)*den(26)
  den(28) = den(7)*den(27)
  den(29) = den(9)*den(23)
  den(30) = den(6)*den(29)
  den(32) = den(6)*den(31)
  den(33) = den(9)*den(32)
  den(34) = den(4)*den(23)
  den(35) = den(6)*den(34)
  den(37) = den(13)*den(36)
  den(38) = den(12)*den(37)
  den(40) = den(12)*den(39)
  den(41) = den(13)*den(40)
  den(43) = den(13)*den(42)
  den(44) = den(15)*den(43)
  den(45) = den(15)*den(39)
  den(46) = den(13)*den(45)
  den(47) = den(4)*den(39)
  den(48) = den(13)*den(47)
  den(49) = den(9)*den(36)
  den(50) = den(12)*den(49)
  den(51) = den(12)*den(31)
  den(52) = den(9)*den(51)
  den(53) = den(2)*den(36)
  den(54) = den(12)*den(53)
  den(55) = den(7)*den(42)
  den(56) = den(15)*den(55)
  den(57) = den(15)*den(26)
  den(58) = den(7)*den(57)
  den(59) = den(2)*den(26)
  den(60) = den(7)*den(59)
  den(61) = den(2)*den(22)
  den(62) = den(4)*den(22)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(58)

  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(1)
  A(2) = cont_VV(wf(:,5),wf(:,6)) * den(3)
  A(3) = cont_VV(wf(:,8),wf(:,9)) * den(5)
  A(4) = cont_QA(wf(:,13),wf(:,14)) * den(8)
  A(5) = cont_QA(wf(:,16),wf(:,17)) * den(10)
  A(6) = cont_VV(wf(:,8),wf(:,18)) * den(11)
  A(7) = cont_QA(wf(:,22),wf(:,23)) * den(14)
  A(8) = cont_QA(wf(:,22),wf(:,26)) * den(16)
  A(9) = cont_VV(wf(:,8),wf(:,27)) * den(17)
  A(10) = cont_QA(wf(:,16),wf(:,28)) * den(18)
  A(11) = cont_VV(wf(:,5),wf(:,29)) * den(19)
  A(12) = cont_QA(wf(:,13),wf(:,30)) * den(20)
  A(13) = cont_VV(wf(:,5),wf(:,31)) * den(21)

  A(14) = cont_VV(wf(:,32),wf(:,33)) * den(22)
  A(15) = cont_QA(wf(:,13),wf(:,34)) * den(8)
  A(16) = cont_QA(wf(:,16),wf(:,35)) * den(10)
  A(17) = cont_QA(wf(:,22),wf(:,36)) * den(14)
  A(18) = cont_QA(wf(:,22),wf(:,37)) * den(16)
  A(19) = cont_QA(wf(:,16),wf(:,38)) * den(18)
  A(20) = cont_QA(wf(:,13),wf(:,39)) * den(20)
  A(21) = cont_VV(wf(:,8),wf(:,40)) * den(11)
  A(22) = cont_QA(wf(:,17),wf(:,42)) * den(10)
  A(23) = cont_QA(wf(:,14),wf(:,44)) * den(8)
  A(24) = cont_VV(wf(:,5),wf(:,45)) * den(19)
  A(25) = cont_QA(wf(:,28),wf(:,42)) * den(18)
  A(26) = cont_QA(wf(:,30),wf(:,44)) * den(20)
  A(27) = cont_VV(wf(:,5),wf(:,46)) * den(21)
  A(28) = cont_QA(wf(:,23),wf(:,48)) * den(14)
  A(29) = cont_QA(wf(:,26),wf(:,48)) * den(16)
  A(30) = cont_VV(wf(:,8),wf(:,49)) * den(17)
  A(31) = cont_VV(wf(:,8),wf(:,50)) * den(17)
  A(32) = cont_QA(wf(:,22),wf(:,53)) * den(16)
  A(33) = cont_QA(wf(:,22),wf(:,56)) * den(14)
  A(34) = cont_VV(wf(:,5),wf(:,57)) * den(21)
  A(35) = cont_QA(wf(:,13),wf(:,58)) * den(20)
  A(36) = cont_QA(wf(:,16),wf(:,59)) * den(18)
  A(37) = cont_VV(wf(:,5),wf(:,60)) * den(19)
  A(38) = cont_QA(wf(:,13),wf(:,63)) * den(8)
  A(39) = cont_QA(wf(:,16),wf(:,64)) * den(10)
  A(40) = cont_VV(wf(:,8),wf(:,65)) * den(11)
  A(41) = cont_VV(wf(:,5),wf(:,67)) * den(3)
  A(42) = cont_VV(wf(:,8),wf(:,68)) * den(5)
  A(43) = cont_QA(wf(:,70),wf(:,71)) * den(25)
  A(44) = cont_QA(wf(:,72),wf(:,73)) * den(28)
  A(45) = cont_QA(wf(:,70),wf(:,75)) * den(30)
  A(46) = cont_QA(wf(:,76),wf(:,77)) * den(33)
  A(47) = cont_QA(wf(:,70),wf(:,79)) * den(35)
  A(48) = cont_QA(wf(:,81),wf(:,82)) * den(38)
  A(49) = cont_QA(wf(:,83),wf(:,84)) * den(41)
  A(50) = cont_QA(wf(:,86),wf(:,87)) * den(44)
  A(51) = cont_QA(wf(:,83),wf(:,88)) * den(46)
  A(52) = cont_QA(wf(:,83),wf(:,90)) * den(48)
  A(53) = cont_QA(wf(:,81),wf(:,92)) * den(50)
  A(54) = cont_QA(wf(:,76),wf(:,93)) * den(52)
  A(55) = cont_QA(wf(:,81),wf(:,95)) * den(54)
  A(56) = cont_QA(wf(:,86),wf(:,97)) * den(56)
  A(57) = cont_QA(wf(:,72),wf(:,98)) * den(58)
  A(58) = cont_QA(wf(:,72),wf(:,100)) * den(60)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(58)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = A(9)*f(1)-A(6)*f(2)+(A(1)-A(2)-A(3))*f(3)+(A(7)+A(8)+A(12))*f(9)+(-A(4)-A(5)-A(10))*f(10)+(-A(11)-A(13))*f(11)

  M2(1) = -(A(52)*f(4))+A(47)*f(5)+(A(30)+A(31))*f(6)+(-A(21)-A(40))*f(7)+(A(14)-A(41)-A(42))*f(8)+(-A(48)-A(49)-A(50)-A(51)-A(56) &
       -A(57))*f(12)+(A(43)+A(44)+A(45)+A(46)+A(53)+A(54))*f(13)+(A(55)+A(58))*f(14)+(A(17)+A(18)+A(20)+A(26)+A(28)+A(29)+A(32) &
       +A(33)+A(35))*f(15)+(-A(15)-A(16)-A(19)-A(22)-A(23)-A(25)-A(36)-A(38)-A(39))*f(16)+(-A(24)-A(27)-A(34)-A(37))*f(17)

end subroutine colourvectors

end module ol_loop_ppvvv_uxdazwx_1_/**/REALKIND
