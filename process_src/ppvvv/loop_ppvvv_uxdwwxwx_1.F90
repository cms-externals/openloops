
module ol_colourmatrix_ppvvv_uxdwwxwx_1_/**/REALKIND
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
end module ol_colourmatrix_ppvvv_uxdwwxwx_1_/**/REALKIND



module ol_forced_parameters_ppvvv_uxdwwxwx_1_/**/REALKIND
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
  if (wMW /= 0) write(*,101) 'wMW = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppvvv_uxdwwxwx_1_/**/REALKIND

module ol_loop_ppvvv_uxdwwxwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(28), c(8)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:104)
  ! denominators
  complex(REALKIND), save :: den(62)
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
    f( 1) = (CI*eQED**3)/(sqrt2*sw**3)
    f( 2) = (CI*cw**2*eQED**3)/(sqrt2*sw**3)
    f( 3) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2)/(sqrt2*sw**3)
    f( 4) = (CI*countertermnorm*ctVqq*cw**2*eQED**3*gQCD**2)/(sqrt2*sw**3)
    f( 5) = (CI*eQED**3*MW**2)/(sqrt2*sw**3)
    f( 6) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2*MW**2)/(sqrt2*sw**3)
    f( 7) = (CI*eQED**3*sqrt2)/(4._/**/REALKIND*sw**3)
    f( 8) = (CI*countertermnorm*eQED**3*gQCD**2*sqrt2)/(4._/**/REALKIND*sw**3)
    f( 9) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2*sqrt2)/(4._/**/REALKIND*sw**3)
    f(10) = (CI*cw*eQED**3)/(sqrt2*sw**2)
    f(11) = (CI*countertermnorm*cw*eQED**3*gQCD**2)/(sqrt2*sw**2)
    f(12) = (CI*countertermnorm*ctVqq*cw*eQED**3*gQCD**2)/(sqrt2*sw**2)
    f(13) = (CI*eQED**3)/(3._/**/REALKIND*sqrt2*sw)
    f(14) = (2*CI*eQED**3)/(3._/**/REALKIND*sqrt2*sw)
    f(15) = (CI*eQED**3)/(sqrt2*sw)
    f(16) = (CI*countertermnorm*eQED**3*gQCD**2)/(3._/**/REALKIND*sqrt2*sw)
    f(17) = (2*CI*countertermnorm*eQED**3*gQCD**2)/(3._/**/REALKIND*sqrt2*sw)
    f(18) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2)/(3._/**/REALKIND*sqrt2*sw)
    f(19) = (2*CI*countertermnorm*ctVqq*eQED**3*gQCD**2)/(3._/**/REALKIND*sqrt2*sw)
    f(20) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2)/(sqrt2*sw)
    f(21) = (eQED**3*gQCD**2*integralnorm*SwB)/(sqrt2*sw**3)
    f(22) = (cw**2*eQED**3*gQCD**2*integralnorm*SwB)/(sqrt2*sw**3)
    f(23) = (eQED**3*gQCD**2*integralnorm*MW**2*SwB)/(sqrt2*sw**3)
    f(24) = (eQED**3*gQCD**2*integralnorm*sqrt2*SwB)/(sw**3*4._/**/REALKIND)
    f(25) = (cw*eQED**3*gQCD**2*integralnorm*SwB)/(sqrt2*sw**2)
    f(26) = (eQED**3*gQCD**2*integralnorm*SwB)/(sqrt2*sw*3._/**/REALKIND)
    f(27) = (2*eQED**3*gQCD**2*integralnorm*SwB)/(sqrt2*sw*3._/**/REALKIND)
    f(28) = (eQED**3*gQCD**2*integralnorm*SwB)/(sqrt2*sw)

  c = [ 4*f(21), 4*f(22), 4*f(23), 4*f(24), 4*f(25), 4*f(26), 4*f(27), 4*f(28) ]
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
  call wf_V(P(:,3), rMW, H(3), wf(:,-2))
  call wf_V(P(:,4), rMW, H(4), wf(:,-3))
  call wf_V(P(:,5), rMW, H(5), wf(:,-4))

  ! internal WFs
  call vert_QA_W(wf(:,-1),wf(:,0),wf(:,1))
  call vert_WWV_V(wf(:,-3),wf(:,-4),wf(:,-2),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MW,1_intkind1,wf(:,3))
  call vert_VV_S(wf(:,-2),wf(:,-3),wf(:,4))
  call vert_VV_S(wf(:,3),wf(:,-4),wf(:,5))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-2),Q(:,4),wf(:,6))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,3),Q(:,3),wf(:,7))
  call prop_W_W(wf(:,6),Q(:,12),MZ,1_intkind1,wf(:,8))
  call vert_VV_S(wf(:,-2),wf(:,-4),wf(:,9))
  call vert_VV_S(wf(:,3),wf(:,-3),wf(:,10))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-2),Q(:,4),wf(:,11))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,3),Q(:,3),wf(:,12))
  call prop_W_W(wf(:,11),Q(:,20),MZ,1_intkind1,wf(:,13))
  call vert_AW_Q(wf(:,0),wf(:,-3),wf(:,14))
  call vert_WQ_A(wf(:,-4),wf(:,-1),wf(:,15))
  call prop_A_Q(wf(:,14),Q(:,9),ZERO,0_intkind1,wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,18),ZERO,0_intkind1,wf(:,17))
  call vert_AW_Q(wf(:,16),wf(:,-2),wf(:,18))
  call vert_QA_V(wf(:,-1),wf(:,16),wf(:,19))
  call vert_QA_Z(gZd,wf(:,-1),wf(:,16),wf(:,20))
  call vert_AW_Q(wf(:,0),wf(:,-4),wf(:,21))
  call vert_WQ_A(wf(:,-3),wf(:,-1),wf(:,22))
  call prop_A_Q(wf(:,21),Q(:,17),ZERO,0_intkind1,wf(:,23))
  call prop_Q_A(wf(:,22),Q(:,10),ZERO,0_intkind1,wf(:,24))
  call vert_AW_Q(wf(:,23),wf(:,-2),wf(:,25))
  call vert_QA_V(wf(:,24),wf(:,0),wf(:,26))
  call vert_QA_Z(gZu,wf(:,24),wf(:,0),wf(:,27))
  call vert_QA_V(wf(:,-1),wf(:,23),wf(:,28))
  call vert_QA_Z(gZd,wf(:,-1),wf(:,23),wf(:,29))
  call vert_QA_V(wf(:,17),wf(:,0),wf(:,30))
  call vert_QA_Z(gZu,wf(:,17),wf(:,0),wf(:,31))
  call counter_QA_W(wf(:,-1),wf(:,0),wf(:,32))
  call prop_W_W(wf(:,2),Q(:,28),MW,1_intkind1,wf(:,33))
  call counter_AW_Q(wf(:,16),wf(:,-2),wf(:,34))
  call counter_AW_Q(wf(:,23),wf(:,-2),wf(:,35))
  call counter_QA_V(wf(:,-1),wf(:,16),wf(:,36))
  call counter_QA_Z(gZd,wf(:,-1),wf(:,16),wf(:,37))
  call counter_WQ_A(wf(:,-4),wf(:,-1),wf(:,38))
  call prop_Q_A(wf(:,38),Q(:,18),ZERO,0_intkind1,wf(:,39))
  call counter_QA_V(wf(:,-1),wf(:,23),wf(:,40))
  call counter_QA_Z(gZd,wf(:,-1),wf(:,23),wf(:,41))
  call vert_QA_V(wf(:,39),wf(:,0),wf(:,42))
  call vert_QA_Z(gZu,wf(:,39),wf(:,0),wf(:,43))
  call counter_WQ_A(wf(:,-3),wf(:,-1),wf(:,44))
  call prop_Q_A(wf(:,44),Q(:,10),ZERO,0_intkind1,wf(:,45))
  call vert_QA_V(wf(:,45),wf(:,0),wf(:,46))
  call vert_QA_Z(gZu,wf(:,45),wf(:,0),wf(:,47))
  call counter_QA_V(wf(:,24),wf(:,0),wf(:,48))
  call counter_QA_Z(gZu,wf(:,24),wf(:,0),wf(:,49))
  call counter_AW_Q(wf(:,0),wf(:,-4),wf(:,50))
  call prop_A_Q(wf(:,50),Q(:,17),ZERO,0_intkind1,wf(:,51))
  call vert_AW_Q(wf(:,51),wf(:,-2),wf(:,52))
  call counter_QA_V(wf(:,17),wf(:,0),wf(:,53))
  call counter_QA_Z(gZu,wf(:,17),wf(:,0),wf(:,54))
  call vert_QA_V(wf(:,-1),wf(:,51),wf(:,55))
  call vert_QA_Z(gZd,wf(:,-1),wf(:,51),wf(:,56))
  call counter_AW_Q(wf(:,0),wf(:,-3),wf(:,57))
  call prop_A_Q(wf(:,57),Q(:,9),ZERO,0_intkind1,wf(:,58))
  call vert_AW_Q(wf(:,58),wf(:,-2),wf(:,59))
  call vert_QA_V(wf(:,-1),wf(:,58),wf(:,60))
  call vert_QA_Z(gZd,wf(:,-1),wf(:,58),wf(:,61))
  call prop_W_W(wf(:,32),Q(:,3),MW,1_intkind1,wf(:,62))
  call vert_VV_S(wf(:,62),wf(:,-4),wf(:,63))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,62),Q(:,3),wf(:,64))
  call vert_VV_S(wf(:,62),wf(:,-3),wf(:,65))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,62),Q(:,3),wf(:,66))
  call vert_WQ_A(wf(:,-2),wf(:,17),wf(:,67))
  call counter_A_Q(ctqq,wf(:,16),Q(:,9),wf(:,68))
  call prop_Q_A(wf(:,67),Q(:,22),ZERO,0_intkind1,wf(:,69))
  call counter_Q_A(ctqq,wf(:,17),Q(:,18),wf(:,70))
  call prop_A_Q(wf(:,18),Q(:,13),ZERO,0_intkind1,wf(:,71))
  call vert_VQ_A(wf(:,11),wf(:,-1),wf(:,72))
  call prop_Q_A(wf(:,72),Q(:,22),ZERO,0_intkind1,wf(:,73))
  call vert_ZQ_A(gZd,wf(:,13),wf(:,-1),wf(:,74))
  call prop_Q_A(wf(:,74),Q(:,22),ZERO,0_intkind1,wf(:,75))
  call vert_WQ_A(wf(:,-2),wf(:,24),wf(:,76))
  call counter_A_Q(ctqq,wf(:,23),Q(:,17),wf(:,77))
  call prop_Q_A(wf(:,76),Q(:,14),ZERO,0_intkind1,wf(:,78))
  call counter_Q_A(ctqq,wf(:,24),Q(:,10),wf(:,79))
  call prop_A_Q(wf(:,25),Q(:,21),ZERO,0_intkind1,wf(:,80))
  call vert_AV_Q(wf(:,0),wf(:,11),wf(:,81))
  call prop_A_Q(wf(:,81),Q(:,21),ZERO,0_intkind1,wf(:,82))
  call vert_AZ_Q(gZu,wf(:,0),wf(:,13),wf(:,83))
  call prop_A_Q(wf(:,83),Q(:,21),ZERO,0_intkind1,wf(:,84))
  call vert_VQ_A(wf(:,6),wf(:,-1),wf(:,85))
  call prop_Q_A(wf(:,85),Q(:,14),ZERO,0_intkind1,wf(:,86))
  call vert_ZQ_A(gZd,wf(:,8),wf(:,-1),wf(:,87))
  call prop_Q_A(wf(:,87),Q(:,14),ZERO,0_intkind1,wf(:,88))
  call vert_AV_Q(wf(:,0),wf(:,6),wf(:,89))
  call prop_A_Q(wf(:,89),Q(:,13),ZERO,0_intkind1,wf(:,90))
  call vert_AZ_Q(gZu,wf(:,0),wf(:,8),wf(:,91))
  call prop_A_Q(wf(:,91),Q(:,13),ZERO,0_intkind1,wf(:,92))
  call vert_SV_V(wf(:,4),wf(:,-4),wf(:,93))
  call prop_W_W(wf(:,93),Q(:,28),MW,1_intkind1,wf(:,94))
  call vert_UV_W(wf(:,6),Q(:,12),wf(:,-4),Q(:,16),wf(:,95))
  call prop_W_W(wf(:,95),Q(:,28),MW,1_intkind1,wf(:,96))
  call vert_UV_W(wf(:,8),Q(:,12),wf(:,-4),Q(:,16),wf(:,97))
  call prop_W_W(wf(:,97),Q(:,28),MW,1_intkind1,wf(:,98))
  call vert_SV_V(wf(:,9),wf(:,-3),wf(:,99))
  call prop_W_W(wf(:,99),Q(:,28),MW,1_intkind1,wf(:,100))
  call vert_UV_W(wf(:,11),Q(:,20),wf(:,-3),Q(:,8),wf(:,101))
  call prop_W_W(wf(:,101),Q(:,28),MW,1_intkind1,wf(:,102))
  call vert_UV_W(wf(:,13),Q(:,20),wf(:,-3),Q(:,8),wf(:,103))
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
  den(2) = 1 / (Q(5,12) - MH2)
  den(4) = 1 / (Q(5,12))
  den(6) = 1 / (Q(5,12) - MZ2)
  den(8) = 1 / (Q(5,20) - MH2)
  den(10) = 1 / (Q(5,20))
  den(12) = 1 / (Q(5,20) - MZ2)
  den(14) = 1 / (Q(5,9))
  den(15) = 1 / (Q(5,18))
  den(19) = 1 / (Q(5,17))
  den(20) = 1 / (Q(5,10))
  den(28) = 1 / (Q(5,28) - MW2)
  den(29) = 1 / (Q(5,22))
  den(32) = 1 / (Q(5,13))
  den(39) = 1 / (Q(5,14))
  den(42) = 1 / (Q(5,21))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(1)*den(6)
  den(9) = den(1)*den(8)
  den(11) = den(1)*den(10)
  den(13) = den(1)*den(12)
  den(16) = den(14)*den(15)
  den(17) = den(10)*den(14)
  den(18) = den(12)*den(14)
  den(21) = den(19)*den(20)
  den(22) = den(10)*den(20)
  den(23) = den(12)*den(20)
  den(24) = den(4)*den(19)
  den(25) = den(6)*den(19)
  den(26) = den(4)*den(15)
  den(27) = den(6)*den(15)
  den(30) = den(15)*den(29)
  den(31) = den(14)*den(30)
  den(33) = den(14)*den(32)
  den(34) = den(15)*den(33)
  den(35) = den(10)*den(29)
  den(36) = den(14)*den(35)
  den(37) = den(12)*den(29)
  den(38) = den(14)*den(37)
  den(40) = den(20)*den(39)
  den(41) = den(19)*den(40)
  den(43) = den(19)*den(42)
  den(44) = den(20)*den(43)
  den(45) = den(10)*den(42)
  den(46) = den(20)*den(45)
  den(47) = den(12)*den(42)
  den(48) = den(20)*den(47)
  den(49) = den(4)*den(39)
  den(50) = den(19)*den(49)
  den(51) = den(6)*den(39)
  den(52) = den(19)*den(51)
  den(53) = den(4)*den(32)
  den(54) = den(15)*den(53)
  den(55) = den(6)*den(32)
  den(56) = den(15)*den(55)
  den(57) = den(2)*den(28)
  den(58) = den(4)*den(28)
  den(59) = den(6)*den(28)
  den(60) = den(8)*den(28)
  den(61) = den(10)*den(28)
  den(62) = den(12)*den(28)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(58)

  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(1)
  A(2) = cont_SS(wf(:,4),wf(:,5)) * den(3)
  A(3) = cont_VV(wf(:,6),wf(:,7)) * den(5)
  A(4) = cont_VV(wf(:,7),wf(:,8)) * den(7)
  A(5) = cont_SS(wf(:,9),wf(:,10)) * den(9)
  A(6) = cont_VV(wf(:,11),wf(:,12)) * den(11)
  A(7) = cont_VV(wf(:,12),wf(:,13)) * den(13)
  A(8) = cont_QA(wf(:,17),wf(:,18)) * den(16)
  A(9) = cont_VV(wf(:,11),wf(:,19)) * den(17)
  A(10) = cont_VV(wf(:,13),wf(:,20)) * den(18)
  A(11) = cont_QA(wf(:,24),wf(:,25)) * den(21)
  A(12) = cont_VV(wf(:,11),wf(:,26)) * den(22)
  A(13) = cont_VV(wf(:,13),wf(:,27)) * den(23)
  A(14) = cont_VV(wf(:,6),wf(:,28)) * den(24)
  A(15) = cont_VV(wf(:,8),wf(:,29)) * den(25)
  A(16) = cont_VV(wf(:,6),wf(:,30)) * den(26)
  A(17) = cont_VV(wf(:,8),wf(:,31)) * den(27)

  A(18) = cont_VV(wf(:,32),wf(:,33)) * den(28)
  A(19) = cont_QA(wf(:,17),wf(:,34)) * den(16)
  A(20) = cont_QA(wf(:,24),wf(:,35)) * den(21)
  A(21) = cont_VV(wf(:,11),wf(:,36)) * den(17)
  A(22) = cont_VV(wf(:,13),wf(:,37)) * den(18)
  A(23) = cont_QA(wf(:,18),wf(:,39)) * den(16)
  A(24) = cont_VV(wf(:,6),wf(:,40)) * den(24)
  A(25) = cont_VV(wf(:,8),wf(:,41)) * den(25)
  A(26) = cont_VV(wf(:,6),wf(:,42)) * den(26)
  A(27) = cont_VV(wf(:,8),wf(:,43)) * den(27)
  A(28) = cont_QA(wf(:,25),wf(:,45)) * den(21)
  A(29) = cont_VV(wf(:,11),wf(:,46)) * den(22)
  A(30) = cont_VV(wf(:,13),wf(:,47)) * den(23)
  A(31) = cont_VV(wf(:,11),wf(:,48)) * den(22)
  A(32) = cont_VV(wf(:,13),wf(:,49)) * den(23)
  A(33) = cont_QA(wf(:,24),wf(:,52)) * den(21)
  A(34) = cont_VV(wf(:,6),wf(:,53)) * den(26)
  A(35) = cont_VV(wf(:,8),wf(:,54)) * den(27)
  A(36) = cont_VV(wf(:,6),wf(:,55)) * den(24)
  A(37) = cont_VV(wf(:,8),wf(:,56)) * den(25)
  A(38) = cont_QA(wf(:,17),wf(:,59)) * den(16)
  A(39) = cont_VV(wf(:,11),wf(:,60)) * den(17)
  A(40) = cont_VV(wf(:,13),wf(:,61)) * den(18)
  A(41) = cont_SS(wf(:,4),wf(:,63)) * den(3)
  A(42) = cont_VV(wf(:,6),wf(:,64)) * den(5)
  A(43) = cont_VV(wf(:,8),wf(:,64)) * den(7)
  A(44) = cont_SS(wf(:,9),wf(:,65)) * den(9)
  A(45) = cont_VV(wf(:,11),wf(:,66)) * den(11)
  A(46) = cont_VV(wf(:,13),wf(:,66)) * den(13)
  A(47) = cont_QA(wf(:,68),wf(:,69)) * den(31)
  A(48) = cont_QA(wf(:,70),wf(:,71)) * den(34)
  A(49) = cont_QA(wf(:,68),wf(:,73)) * den(36)
  A(50) = cont_QA(wf(:,68),wf(:,75)) * den(38)
  A(51) = cont_QA(wf(:,77),wf(:,78)) * den(41)
  A(52) = cont_QA(wf(:,79),wf(:,80)) * den(44)
  A(53) = cont_QA(wf(:,79),wf(:,82)) * den(46)
  A(54) = cont_QA(wf(:,79),wf(:,84)) * den(48)
  A(55) = cont_QA(wf(:,77),wf(:,86)) * den(50)
  A(56) = cont_QA(wf(:,77),wf(:,88)) * den(52)
  A(57) = cont_QA(wf(:,70),wf(:,90)) * den(54)
  A(58) = cont_QA(wf(:,70),wf(:,92)) * den(56)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(58)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = A(1)*f(1)+(A(4)+A(7))*f(2)+(-A(2)-A(5))*f(5)+(A(8)+A(11))*f(7)+(A(10)+A(13)+A(15)+A(17))*f(10)+(-A(9)-A(14))*f(13) &
       +(A(12)+A(16))*f(14)+(A(3)+A(6))*f(15)

  M2(1) = A(18)*f(3)+(A(43)+A(46))*f(4)+(-A(41)-A(44))*f(6)+(-A(47)-A(48)-A(51)-A(52))*f(8)+(A(19)+A(20)+A(23)+A(28)+A(33) &
       +A(38))*f(9)+(-A(50)-A(54)-A(56)-A(58))*f(11)+(A(22)+A(25)+A(27)+A(30)+A(32)+A(35)+A(37)+A(40))*f(12)+(A(49)+A(55))*f(16)+( &
       -A(53)-A(57))*f(17)+(-A(21)-A(24)-A(36)-A(39))*f(18)+(A(26)+A(29)+A(31)+A(34))*f(19)+(A(42)+A(45))*f(20)

end subroutine colourvectors

end module ol_loop_ppvvv_uxdwwxwx_1_/**/REALKIND
