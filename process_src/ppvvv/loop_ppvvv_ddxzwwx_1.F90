
module ol_colourmatrix_ppvvv_ddxzwwx_1_/**/REALKIND
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
end module ol_colourmatrix_ppvvv_ddxzwwx_1_/**/REALKIND



module ol_forced_parameters_ppvvv_ddxzwwx_1_/**/REALKIND
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
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppvvv_ddxzwwx_1_/**/REALKIND

module ol_loop_ppvvv_ddxzwwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(25), c(7)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:99)
  ! denominators
  complex(REALKIND), save :: den(60)
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
    f( 3) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2)/3._/**/REALKIND
    f( 4) = (CI*cw*eQED**3)/(2._/**/REALKIND*sw**3)
    f( 5) = (CI*countertermnorm*cw*eQED**3*gQCD**2)/(2._/**/REALKIND*sw**3)
    f( 6) = (CI*countertermnorm*ctVqq*cw*eQED**3*gQCD**2)/(2._/**/REALKIND*sw**3)
    f( 7) = (CI*eQED**3)/(2._/**/REALKIND*sw**2)
    f( 8) = (CI*cw**2*eQED**3)/sw**2
    f( 9) = (CI*countertermnorm*eQED**3*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(10) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(11) = (CI*countertermnorm*ctVqq*cw**2*eQED**3*gQCD**2)/sw**2
    f(12) = (CI*eQED**3*MW**2)/(cw**2*sw**2)
    f(13) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2*MW**2)/(cw**2*sw**2)
    f(14) = (CI*cw*eQED**3)/(3._/**/REALKIND*sw)
    f(15) = (CI*cw*eQED**3)/sw
    f(16) = (CI*countertermnorm*cw*eQED**3*gQCD**2)/sw
    f(17) = (CI*countertermnorm*ctVqq*cw*eQED**3*gQCD**2)/(3._/**/REALKIND*sw)
    f(18) = (CI*countertermnorm*ctVqq*cw*eQED**3*gQCD**2)/sw
    f(19) = (eQED**3*gQCD**2*integralnorm*SwB)/3._/**/REALKIND
    f(20) = (cw*eQED**3*gQCD**2*integralnorm*SwB)/(sw**3*2._/**/REALKIND)
    f(21) = (eQED**3*gQCD**2*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(22) = (cw**2*eQED**3*gQCD**2*integralnorm*SwB)/sw**2
    f(23) = (eQED**3*gQCD**2*integralnorm*MW**2*SwB)/(cw**2*sw**2)
    f(24) = (cw*eQED**3*gQCD**2*integralnorm*SwB)/(sw*3._/**/REALKIND)
    f(25) = (cw*eQED**3*gQCD**2*integralnorm*SwB)/sw

  c = [ 4*f(19), 4*f(20), 4*f(21), 4*f(22), 4*f(23), 4*f(24), 4*f(25) ]
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
  complex(REALKIND) :: A(56)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_V(P(:,3), rMZ, H(3), wf(:,-2))
  call wf_V(P(:,4), rMW, H(4), wf(:,-3))
  call wf_V(P(:,5), rMW, H(5), wf(:,-4))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_WWV_V(wf(:,-3),wf(:,-4),wf(:,-2),wf(:,2))
  call vert_QA_Z(gZd,wf(:,0),wf(:,-1),wf(:,3))
  call prop_W_W(wf(:,3),Q(:,3),MZ,1_intkind1,wf(:,4))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-2),Q(:,4),wf(:,5))
  call prop_W_W(wf(:,5),Q(:,12),MW,1_intkind1,wf(:,6))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,7))
  call vert_UV_W(wf(:,4),Q(:,3),wf(:,-4),Q(:,16),wf(:,8))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,9))
  call prop_W_W(wf(:,9),Q(:,20),MW,1_intkind1,wf(:,10))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,1),Q(:,3),wf(:,11))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,4),Q(:,3),wf(:,12))
  call vert_VV_S(wf(:,-3),wf(:,-4),wf(:,13))
  call vert_VV_S(wf(:,4),wf(:,-2),wf(:,14))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,0),wf(:,15))
  call vert_AW_Q(wf(:,-1),wf(:,-3),wf(:,16))
  call prop_Q_A(wf(:,15),Q(:,5),ZERO,0_intkind1,wf(:,17))
  call prop_A_Q(wf(:,16),Q(:,10),ZERO,0_intkind1,wf(:,18))
  call vert_WQ_A(wf(:,-4),wf(:,17),wf(:,19))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-3),Q(:,8),wf(:,20))
  call vert_QA_V(wf(:,17),wf(:,-1),wf(:,21))
  call prop_W_W(wf(:,20),Q(:,24),MZ,1_intkind1,wf(:,22))
  call vert_QA_Z(gZd,wf(:,17),wf(:,-1),wf(:,23))
  call vert_WQ_A(wf(:,-4),wf(:,0),wf(:,24))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-2),wf(:,25))
  call prop_Q_A(wf(:,24),Q(:,17),ZERO,0_intkind1,wf(:,26))
  call prop_A_Q(wf(:,25),Q(:,6),ZERO,0_intkind1,wf(:,27))
  call vert_WQ_A(wf(:,-3),wf(:,26),wf(:,28))
  call vert_QA_V(wf(:,0),wf(:,27),wf(:,29))
  call vert_QA_Z(gZd,wf(:,0),wf(:,27),wf(:,30))
  call vert_ZQ_A(gZu,wf(:,-2),wf(:,26),wf(:,31))
  call vert_QA_W(wf(:,0),wf(:,18),wf(:,32))
  call vert_QA_W(wf(:,26),wf(:,-1),wf(:,33))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,34))
  call counter_QA_Z(gZd,wf(:,0),wf(:,-1),wf(:,35))
  call prop_W_W(wf(:,2),Q(:,28),MZ,1_intkind1,wf(:,36))
  call counter_WQ_A(wf(:,-4),wf(:,17),wf(:,37))
  call counter_WQ_A(wf(:,-3),wf(:,26),wf(:,38))
  call counter_ZQ_A(gZu,wf(:,-2),wf(:,26),wf(:,39))
  call counter_QA_V(wf(:,17),wf(:,-1),wf(:,40))
  call counter_QA_Z(gZd,wf(:,17),wf(:,-1),wf(:,41))
  call counter_AW_Q(wf(:,-1),wf(:,-3),wf(:,42))
  call prop_A_Q(wf(:,42),Q(:,10),ZERO,0_intkind1,wf(:,43))
  call counter_QA_W(wf(:,26),wf(:,-1),wf(:,44))
  call vert_QA_W(wf(:,0),wf(:,43),wf(:,45))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,-2),wf(:,46))
  call prop_A_Q(wf(:,46),Q(:,6),ZERO,0_intkind1,wf(:,47))
  call vert_QA_V(wf(:,0),wf(:,47),wf(:,48))
  call vert_QA_Z(gZd,wf(:,0),wf(:,47),wf(:,49))
  call counter_QA_V(wf(:,0),wf(:,27),wf(:,50))
  call counter_QA_Z(gZd,wf(:,0),wf(:,27),wf(:,51))
  call counter_WQ_A(wf(:,-4),wf(:,0),wf(:,52))
  call prop_Q_A(wf(:,52),Q(:,17),ZERO,0_intkind1,wf(:,53))
  call vert_WQ_A(wf(:,-3),wf(:,53),wf(:,54))
  call counter_QA_W(wf(:,0),wf(:,18),wf(:,55))
  call vert_ZQ_A(gZu,wf(:,-2),wf(:,53),wf(:,56))
  call vert_QA_W(wf(:,53),wf(:,-1),wf(:,57))
  call counter_ZQ_A(gZd,wf(:,-2),wf(:,0),wf(:,58))
  call prop_Q_A(wf(:,58),Q(:,5),ZERO,0_intkind1,wf(:,59))
  call vert_WQ_A(wf(:,-4),wf(:,59),wf(:,60))
  call vert_QA_V(wf(:,59),wf(:,-1),wf(:,61))
  call vert_QA_Z(gZd,wf(:,59),wf(:,-1),wf(:,62))
  call vert_UV_W(wf(:,34),Q(:,3),wf(:,-4),Q(:,16),wf(:,63))
  call prop_W_W(wf(:,35),Q(:,3),MZ,1_intkind1,wf(:,64))
  call vert_UV_W(wf(:,64),Q(:,3),wf(:,-4),Q(:,16),wf(:,65))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,34),Q(:,3),wf(:,66))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,64),Q(:,3),wf(:,67))
  call vert_VV_S(wf(:,64),wf(:,-2),wf(:,68))
  call vert_AW_Q(wf(:,18),wf(:,-4),wf(:,69))
  call counter_Q_A(ctqq,wf(:,17),Q(:,5),wf(:,70))
  call prop_A_Q(wf(:,69),Q(:,26),ZERO,0_intkind1,wf(:,71))
  call counter_A_Q(ctqq,wf(:,18),Q(:,10),wf(:,72))
  call prop_Q_A(wf(:,19),Q(:,21),ZERO,0_intkind1,wf(:,73))
  call vert_AV_Q(wf(:,-1),wf(:,20),wf(:,74))
  call prop_A_Q(wf(:,74),Q(:,26),ZERO,0_intkind1,wf(:,75))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,22),wf(:,76))
  call prop_A_Q(wf(:,76),Q(:,26),ZERO,0_intkind1,wf(:,77))
  call vert_AW_Q(wf(:,27),wf(:,-3),wf(:,78))
  call counter_Q_A(ctqq,wf(:,26),Q(:,17),wf(:,79))
  call prop_A_Q(wf(:,78),Q(:,14),ZERO,0_intkind1,wf(:,80))
  call counter_A_Q(ctqq,wf(:,27),Q(:,6),wf(:,81))
  call prop_Q_A(wf(:,28),Q(:,25),ZERO,0_intkind1,wf(:,82))
  call vert_VQ_A(wf(:,20),wf(:,0),wf(:,83))
  call prop_Q_A(wf(:,83),Q(:,25),ZERO,0_intkind1,wf(:,84))
  call vert_ZQ_A(gZd,wf(:,22),wf(:,0),wf(:,85))
  call prop_Q_A(wf(:,85),Q(:,25),ZERO,0_intkind1,wf(:,86))
  call vert_AZ_Q(gZu,wf(:,18),wf(:,-2),wf(:,87))
  call prop_A_Q(wf(:,87),Q(:,14),ZERO,0_intkind1,wf(:,88))
  call prop_Q_A(wf(:,31),Q(:,21),ZERO,0_intkind1,wf(:,89))
  call vert_WQ_A(wf(:,10),wf(:,0),wf(:,90))
  call prop_Q_A(wf(:,90),Q(:,21),ZERO,0_intkind1,wf(:,91))
  call vert_AW_Q(wf(:,-1),wf(:,6),wf(:,92))
  call prop_A_Q(wf(:,92),Q(:,14),ZERO,0_intkind1,wf(:,93))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,6),Q(:,12),wf(:,94))
  call prop_W_W(wf(:,94),Q(:,28),MZ,1_intkind1,wf(:,95))
  call vert_UV_W(wf(:,10),Q(:,20),wf(:,-3),Q(:,8),wf(:,96))
  call prop_W_W(wf(:,96),Q(:,28),MZ,1_intkind1,wf(:,97))
  call vert_SV_V(wf(:,13),wf(:,-2),wf(:,98))
  call prop_W_W(wf(:,98),Q(:,28),MZ,1_intkind1,wf(:,99))

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
  den(3) = 1 / (Q(5,12) - MW2)
  den(6) = 1 / (Q(5,20) - MW2)
  den(9) = 1 / (Q(5,24) - MH2)
  den(11) = 1 / (Q(5,5))
  den(12) = 1 / (Q(5,10))
  den(14) = 1 / (Q(5,24))
  den(16) = 1 / (Q(5,24) - MZ2)
  den(18) = 1 / (Q(5,17))
  den(19) = 1 / (Q(5,6))
  den(26) = 1 / (Q(5,28))
  den(27) = 1 / (Q(5,28) - MZ2)
  den(28) = 1 / (Q(5,26))
  den(31) = 1 / (Q(5,21))
  den(38) = 1 / (Q(5,14))
  den(41) = 1 / (Q(5,25))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(3)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(6)
  den(10) = den(2)*den(9)
  den(13) = den(11)*den(12)
  den(15) = den(11)*den(14)
  den(17) = den(11)*den(16)
  den(20) = den(18)*den(19)
  den(21) = den(14)*den(19)
  den(22) = den(16)*den(19)
  den(23) = den(12)*den(18)
  den(24) = den(6)*den(12)
  den(25) = den(3)*den(18)
  den(29) = den(12)*den(28)
  den(30) = den(11)*den(29)
  den(32) = den(11)*den(31)
  den(33) = den(12)*den(32)
  den(34) = den(14)*den(28)
  den(35) = den(11)*den(34)
  den(36) = den(16)*den(28)
  den(37) = den(11)*den(36)
  den(39) = den(19)*den(38)
  den(40) = den(18)*den(39)
  den(42) = den(18)*den(41)
  den(43) = den(19)*den(42)
  den(44) = den(14)*den(41)
  den(45) = den(19)*den(44)
  den(46) = den(16)*den(41)
  den(47) = den(19)*den(46)
  den(48) = den(12)*den(38)
  den(49) = den(18)*den(48)
  den(50) = den(18)*den(31)
  den(51) = den(12)*den(50)
  den(52) = den(6)*den(31)
  den(53) = den(12)*den(52)
  den(54) = den(3)*den(38)
  den(55) = den(18)*den(54)
  den(56) = den(3)*den(26)
  den(57) = den(3)*den(27)
  den(58) = den(6)*den(26)
  den(59) = den(6)*den(27)
  den(60) = den(9)*den(27)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(56)

  A(1) = cont_VV(wf(:,1),wf(:,2)) * den(1)
  A(2) = cont_VV(wf(:,2),wf(:,4)) * den(2)
  A(3) = cont_VV(wf(:,6),wf(:,7)) * den(4)
  A(4) = cont_VV(wf(:,6),wf(:,8)) * den(5)
  A(5) = cont_VV(wf(:,10),wf(:,11)) * den(7)
  A(6) = cont_VV(wf(:,10),wf(:,12)) * den(8)
  A(7) = cont_SS(wf(:,13),wf(:,14)) * den(10)
  A(8) = cont_QA(wf(:,18),wf(:,19)) * den(13)
  A(9) = cont_VV(wf(:,20),wf(:,21)) * den(15)
  A(10) = cont_VV(wf(:,22),wf(:,23)) * den(17)
  A(11) = cont_QA(wf(:,27),wf(:,28)) * den(20)
  A(12) = cont_VV(wf(:,20),wf(:,29)) * den(21)
  A(13) = cont_VV(wf(:,22),wf(:,30)) * den(22)
  A(14) = cont_QA(wf(:,18),wf(:,31)) * den(23)
  A(15) = cont_VV(wf(:,10),wf(:,32)) * den(24)
  A(16) = cont_VV(wf(:,6),wf(:,33)) * den(25)

  A(17) = cont_VV(wf(:,2),wf(:,34)) * den(26)
  A(18) = cont_VV(wf(:,35),wf(:,36)) * den(27)
  A(19) = cont_QA(wf(:,18),wf(:,37)) * den(13)
  A(20) = cont_QA(wf(:,27),wf(:,38)) * den(20)
  A(21) = cont_QA(wf(:,18),wf(:,39)) * den(23)
  A(22) = cont_VV(wf(:,20),wf(:,40)) * den(15)
  A(23) = cont_VV(wf(:,22),wf(:,41)) * den(17)
  A(24) = cont_QA(wf(:,19),wf(:,43)) * den(13)
  A(25) = cont_VV(wf(:,6),wf(:,44)) * den(25)
  A(26) = cont_QA(wf(:,31),wf(:,43)) * den(23)
  A(27) = cont_VV(wf(:,10),wf(:,45)) * den(24)
  A(28) = cont_QA(wf(:,28),wf(:,47)) * den(20)
  A(29) = cont_VV(wf(:,20),wf(:,48)) * den(21)
  A(30) = cont_VV(wf(:,22),wf(:,49)) * den(22)
  A(31) = cont_VV(wf(:,20),wf(:,50)) * den(21)
  A(32) = cont_VV(wf(:,22),wf(:,51)) * den(22)
  A(33) = cont_QA(wf(:,27),wf(:,54)) * den(20)
  A(34) = cont_VV(wf(:,10),wf(:,55)) * den(24)
  A(35) = cont_QA(wf(:,18),wf(:,56)) * den(23)
  A(36) = cont_VV(wf(:,6),wf(:,57)) * den(25)
  A(37) = cont_QA(wf(:,18),wf(:,60)) * den(13)
  A(38) = cont_VV(wf(:,20),wf(:,61)) * den(15)
  A(39) = cont_VV(wf(:,22),wf(:,62)) * den(17)
  A(40) = cont_VV(wf(:,6),wf(:,63)) * den(4)
  A(41) = cont_VV(wf(:,6),wf(:,65)) * den(5)
  A(42) = cont_VV(wf(:,10),wf(:,66)) * den(7)
  A(43) = cont_VV(wf(:,10),wf(:,67)) * den(8)
  A(44) = cont_SS(wf(:,13),wf(:,68)) * den(10)
  A(45) = cont_QA(wf(:,70),wf(:,71)) * den(30)
  A(46) = cont_QA(wf(:,72),wf(:,73)) * den(33)
  A(47) = cont_QA(wf(:,70),wf(:,75)) * den(35)
  A(48) = cont_QA(wf(:,70),wf(:,77)) * den(37)
  A(49) = cont_QA(wf(:,79),wf(:,80)) * den(40)
  A(50) = cont_QA(wf(:,81),wf(:,82)) * den(43)
  A(51) = cont_QA(wf(:,81),wf(:,84)) * den(45)
  A(52) = cont_QA(wf(:,81),wf(:,86)) * den(47)
  A(53) = cont_QA(wf(:,79),wf(:,88)) * den(49)
  A(54) = cont_QA(wf(:,72),wf(:,89)) * den(51)
  A(55) = cont_QA(wf(:,72),wf(:,91)) * den(53)
  A(56) = cont_QA(wf(:,79),wf(:,93)) * den(55)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(56)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(9)+A(12))*f(1)+(-A(15)-A(16))*f(4)+(-A(8)-A(11)-A(14))*f(7)+(A(2)-A(4)-A(6))*f(8)+A(7)*f(12)+(-A(1)+A(3)+A(5))*f(14) &
       +(-A(10)-A(13))*f(15)

  M2(1) = (-A(47)-A(51))*f(2)+(A(22)+A(29)+A(31)+A(38))*f(3)+(A(55)+A(56))*f(5)+(-A(25)-A(27)-A(34)-A(36))*f(6)+(A(45)+A(46)+A(49) &
       +A(50)+A(53)+A(54))*f(9)+(-A(19)-A(20)-A(21)-A(24)-A(26)-A(28)-A(33)-A(35)-A(37))*f(10)+(A(18)-A(41)-A(43))*f(11) &
       +A(44)*f(13)+(A(48)+A(52))*f(16)+(-A(17)+A(40)+A(42))*f(17)+(-A(23)-A(30)-A(32)-A(39))*f(18)

end subroutine colourvectors

end module ol_loop_ppvvv_ddxzwwx_1_/**/REALKIND
