
module ol_colourmatrix_heftpphjj_ddxhgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(34,2), K2(2,2), KL(2,3)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  48,  -6]
  K1( 2,:) = [  -6,  48]
  K1( 3,:) = [  64,  -8]
  K1( 4,:) = [  -8,  64]
  K1( 5,:) = [  -1, -10]
  K1( 6,:) = [ -10,  -1]
  K1( 7,:) = [  64,  -8]
  K1( 8,:) = [  -8,  64]
  K1( 9,:) = [   0,   0]
  K1(10,:) = [   0,   0]
  K1(11,:) = [   0,   0]
  K1(12,:) = [   0,   0]
  K1(13,:) = [   0,   0]
  K1(14,:) = [   0,   0]
  K1(15,:) = [   9,   9]
  K1(16,:) = [   9, -72]
  K1(17,:) = [ -72,   9]
  K1(18,:) = [   9,   9]
  K1(19,:) = [   0,   0]
  K1(20,:) = [   0,   0]
  K1(21,:) = [ 144, -18]
  K1(22,:) = [ -18, 144]
  K1(23,:) = [ -72,   9]
  K1(24,:) = [   9,   9]
  K1(25,:) = [   9,   9]
  K1(26,:) = [   9, -72]
  K1(27,:) = [   0,   0]
  K1(28,:) = [   0,   0]
  K1(29,:) = [ -81,   0]
  K1(30,:) = [   0, -81]
  K1(31,:) = [ 144, -18]
  K1(32,:) = [ -18, 144]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1 = (1._/**/REALKIND / 9) * K1

  K2(1,:) = [ 16, -2]
  K2(2,:) = [ -2, 16]
  K2 = (1._/**/REALKIND / 3) * K2

  KL(1,:) = [ 16, -2,  6]
  KL(2,:) = [ -2, 16,  6]
  KL = (1._/**/REALKIND / 3) * KL

  end subroutine colourmatrix_init
end module ol_colourmatrix_heftpphjj_ddxhgg_1_/**/REALKIND



module ol_forced_parameters_heftpphjj_ddxhgg_1_/**/REALKIND
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
  if (nf /= 5) write(*,101) 'nf = 5'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_heftpphjj_ddxhgg_1_/**/REALKIND

module ol_loop_heftpphjj_ddxhgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(22), c(22)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:93)
  ! denominators
  complex(REALKIND), save :: den(70)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,16)
  ! zero helicity identifier
  logical,           save :: zerohel(16) = .true., zerohel_ct(16) = .true.

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
!  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED*gQCD**4)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 2) = (eQED*gQCD**4)/(MW*pi**2*sw*24._/**/REALKIND)
    f( 3) = (CI*countertermnorm*eQED*gQCD**6)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 4) = (countertermnorm*eQED*gQCD**6)/(MW*pi**2*sw*24._/**/REALKIND)
    f( 5) = (CI*countertermnorm*ctGqq*eQED*gQCD**6)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 6) = (countertermnorm*ctGqq*eQED*gQCD**6)/(MW*pi**2*sw*24._/**/REALKIND)
    f( 7) = (countertermnorm*ctHEFTgggh*eQED*gQCD**6)/(MW*pi**2*sw*24._/**/REALKIND)
    f( 8) = (countertermnorm*ctVVV*eQED*gQCD**6)/(MW*pi**2*sw*24._/**/REALKIND)
    f( 9) = (CI*countertermnorm*eQED*gQCD**6*R2HEFTghqq)/(24._/**/REALKIND*MW*pi**2*sw)
    f(10) = (countertermnorm*eQED*gQCD**6*R2HEFTghqq)/(MW*pi**2*sw*24._/**/REALKIND)
    f(11) = (CI*countertermnorm*eQED*gQCD**6*R2HEFThqq)/(24._/**/REALKIND*MW*pi**2*sw)
    f(12) = (countertermnorm*eQED*gQCD**6*R2HEFThqq)/(MW*pi**2*sw*24._/**/REALKIND)
    f(13) = (CI*eQED*gQCD**6*integralnorm*SwB)/(48._/**/REALKIND*MW*pi**2*sw)
    f(14) = (CI*eQED*gQCD**6*integralnorm*SwB)/(24._/**/REALKIND*MW*pi**2*sw)
    f(15) = (eQED*gQCD**6*integralnorm*SwB)/(MW*pi**2*sw*48._/**/REALKIND)
    f(16) = (eQED*gQCD**6*integralnorm*SwB)/(MW*pi**2*sw*24._/**/REALKIND)
    f(17) = (CI*eQED*gQCD**4*integralnorm*MB*SwF)/(2._/**/REALKIND*MW*sw)
    f(18) = (eQED*gQCD**4*integralnorm*MB*SwF)/(MW*sw*2._/**/REALKIND)
    f(19) = (CI*eQED*gQCD**6*integralnorm*SwF)/(24._/**/REALKIND*MW*pi**2*sw)
    f(20) = (CI*eQED*gQCD**6*integralnorm*SwF)/(12._/**/REALKIND*MW*pi**2*sw)
    f(21) = (eQED*gQCD**6*integralnorm*SwF)/(MW*pi**2*sw*24._/**/REALKIND)
    f(22) = (eQED*gQCD**6*integralnorm*SwF)/(MW*pi**2*sw*12._/**/REALKIND)

  c = [ 9*CI*f(13), 18*CI*f(13), CI*f(14), 3*CI*f(14), 8*CI*f(14), 9*CI*f(14), 18*CI*f(14), 18*f(15), f(16), 3*f(16), 8*f(16) &
    , 9*f(16), 18*f(16), 3*CI*f(17), f(18), 3*f(18), 3*CI*f(19), 3*CI*f(20), f(21), 3*f(21), f(22), 3*f(22) ]
  c = (1._/**/REALKIND / 6) * c
end subroutine fac_init_loop


! **********************************************************************
subroutine tree_wavefunctions(P, H, M1, M2, POLSEL)
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
  integer,           intent(in), optional  :: POLSEL(5)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(59)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_S(P(:,3), rMH, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_S(P(:,3), rMH, H(3), wf(:,-2), 0)
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_HGG_G(wf(:,-2),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,2),Q(:,28))
  call vert_HG_G(wf(:,-2),wf(:,-3),Q(:,8),wf(:,3),Q(:,12))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,4))
  call vert_HG_G(wf(:,-2),wf(:,-4),Q(:,16),wf(:,5),Q(:,20))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,-3),Q(:,8),wf(:,6))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,7))
  call vert_HG_G(wf(:,-2),wf(:,1),Q(:,3),wf(:,8),Q(:,7))
  call vert_VQ_A(wf(:,-3),wf(:,0),wf(:,9))
  call prop_Q_A(wf(:,9),Q(:,9),ZERO,0_intkind1,wf(:,10))
  call vert_QA_V(wf(:,10),wf(:,-1),wf(:,11))
  call vert_AV_Q(wf(:,-1),wf(:,-3),wf(:,12))
  call prop_A_Q(wf(:,12),Q(:,10),ZERO,0_intkind1,wf(:,13))
  call vert_QA_V(wf(:,0),wf(:,13),wf(:,14))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,15))
  call prop_Q_A(wf(:,15),Q(:,17),ZERO,0_intkind1,wf(:,16))
  call vert_QA_V(wf(:,16),wf(:,-1),wf(:,17))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,18))
  call prop_A_Q(wf(:,18),Q(:,18),ZERO,0_intkind1,wf(:,19))
  call vert_QA_V(wf(:,0),wf(:,19),wf(:,20))
  call counter_HGG_G(wf(:,-2),wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,21),Q(:,28))
  call counter_AVH_Q(wf(:,-1),wf(:,-4),wf(:,-2),wf(:,22))
  call counter_AVH_Q(wf(:,-1),wf(:,-3),wf(:,-2),wf(:,23))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,24))
  call counter_VHQ_A(wf(:,-4),wf(:,-2),wf(:,0),wf(:,25))
  call counter_VHQ_A(wf(:,-3),wf(:,-2),wf(:,0),wf(:,26))
  call counter_HQA_V(wf(:,-2),wf(:,0),wf(:,-1),wf(:,27))
  call counter_UV_W(wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,28))
  call counter_UV_W(wf(:,1),Q(:,3),wf(:,-3),Q(:,8),wf(:,29))
  call counter_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,30))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,31))
  call counter_HG_G(ctHEFTggh,wf(:,-2),wf(:,1),Q(:,3),wf(:,32),Q(:,7))
  call counter_HG_G(ctHEFTggh,wf(:,-2),wf(:,-4),Q(:,16),wf(:,33),Q(:,20))
  call counter_HG_G(ctHEFTggh,wf(:,-2),wf(:,-3),Q(:,8),wf(:,34),Q(:,12))
  call counter_QH_A(wf(:,10),Q(:,9),wf(:,-2),wf(:,35),Q(:,13))
  call counter_QH_A(wf(:,16),Q(:,17),wf(:,-2),wf(:,36),Q(:,21))
  call counter_QA_V(wf(:,10),wf(:,-1),wf(:,37))
  call counter_QA_V(wf(:,16),wf(:,-1),wf(:,38))
  call counter_AV_Q(wf(:,-1),wf(:,-4),wf(:,39))
  call prop_A_Q(wf(:,39),Q(:,18),ZERO,0_intkind1,wf(:,40))
  call vert_QA_V(wf(:,0),wf(:,40),wf(:,41))
  call counter_AV_Q(wf(:,-1),wf(:,-3),wf(:,42))
  call prop_A_Q(wf(:,42),Q(:,10),ZERO,0_intkind1,wf(:,43))
  call vert_QA_V(wf(:,0),wf(:,43),wf(:,44))
  call counter_HA_Q(wf(:,-2),wf(:,-1),Q(:,2),wf(:,45),Q(:,6))
  call prop_A_Q(wf(:,45),Q(:,6),ZERO,0_intkind1,wf(:,46))
  call vert_VQ_A(wf(:,-4),wf(:,10),wf(:,47))
  call vert_VQ_A(wf(:,-3),wf(:,16),wf(:,48))
  call vert_QA_V(wf(:,0),wf(:,46),wf(:,49))
  call counter_QA_V(wf(:,0),wf(:,13),wf(:,50))
  call counter_QA_V(wf(:,0),wf(:,19),wf(:,51))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,52))
  call prop_Q_A(wf(:,52),Q(:,17),ZERO,0_intkind1,wf(:,53))
  call vert_QA_V(wf(:,53),wf(:,-1),wf(:,54))
  call counter_VQ_A(wf(:,-3),wf(:,0),wf(:,55))
  call prop_Q_A(wf(:,55),Q(:,9),ZERO,0_intkind1,wf(:,56))
  call vert_QA_V(wf(:,56),wf(:,-1),wf(:,57))
  call counter_QH_A(wf(:,0),Q(:,1),wf(:,-2),wf(:,58),Q(:,5))
  call prop_Q_A(wf(:,58),Q(:,5),ZERO,0_intkind1,wf(:,59))
  call vert_VQ_A(wf(:,-4),wf(:,59),wf(:,60))
  call vert_VQ_A(wf(:,-3),wf(:,59),wf(:,61))
  call vert_QA_V(wf(:,59),wf(:,-1),wf(:,62))
  call vert_UV_W(wf(:,24),Q(:,3),wf(:,-4),Q(:,16),wf(:,63))
  call vert_UV_W(wf(:,24),Q(:,3),wf(:,-3),Q(:,8),wf(:,64))
  call vert_HG_G(wf(:,-2),wf(:,24),Q(:,3),wf(:,65),Q(:,7))
  call vert_UV_W(wf(:,3),Q(:,12),wf(:,-4),Q(:,16),wf(:,66))
  call counter_V_V(ctGG,wf(:,3),Q(:,12),wf(:,67))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,5),Q(:,20),wf(:,68))
  call counter_V_V(ctGG,wf(:,5),Q(:,20),wf(:,69))
  call vert_HG_G(wf(:,-2),wf(:,7),Q(:,24),wf(:,70),Q(:,28))
  call counter_V_V(ctGG,wf(:,7),Q(:,24),wf(:,71))
  call vert_AV_Q(wf(:,-1),wf(:,5),wf(:,72))
  call counter_Q_A(ctqq,wf(:,10),Q(:,9),wf(:,73))
  call prop_A_Q(wf(:,72),Q(:,22),ZERO,0_intkind1,wf(:,74))
  call vert_VQ_A(wf(:,5),wf(:,0),wf(:,75))
  call counter_A_Q(ctqq,wf(:,13),Q(:,10),wf(:,76))
  call prop_Q_A(wf(:,75),Q(:,21),ZERO,0_intkind1,wf(:,77))
  call vert_AV_Q(wf(:,-1),wf(:,3),wf(:,78))
  call counter_Q_A(ctqq,wf(:,16),Q(:,17),wf(:,79))
  call prop_A_Q(wf(:,78),Q(:,14),ZERO,0_intkind1,wf(:,80))
  call vert_VQ_A(wf(:,3),wf(:,0),wf(:,81))
  call counter_A_Q(ctqq,wf(:,19),Q(:,18),wf(:,82))
  call prop_Q_A(wf(:,81),Q(:,13),ZERO,0_intkind1,wf(:,83))
  call prop_Q_A(wf(:,47),Q(:,25),ZERO,0_intkind1,wf(:,84))
  call vert_AV_Q(wf(:,13),wf(:,-4),wf(:,85))
  call prop_A_Q(wf(:,85),Q(:,26),ZERO,0_intkind1,wf(:,86))
  call prop_Q_A(wf(:,48),Q(:,25),ZERO,0_intkind1,wf(:,87))
  call vert_AV_Q(wf(:,19),wf(:,-3),wf(:,88))
  call prop_A_Q(wf(:,88),Q(:,26),ZERO,0_intkind1,wf(:,89))
  call vert_VQ_A(wf(:,7),wf(:,0),wf(:,90))
  call prop_Q_A(wf(:,90),Q(:,25),ZERO,0_intkind1,wf(:,91))
  call vert_AV_Q(wf(:,-1),wf(:,7),wf(:,92))
  call prop_A_Q(wf(:,92),Q(:,26),ZERO,0_intkind1,wf(:,93))

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
  den(2) = 1 / (Q(5,12))
  den(4) = 1 / (Q(5,20))
  den(6) = 1 / (Q(5,24))
  den(8) = 1 / (Q(5,9))
  den(10) = 1 / (Q(5,10))
  den(12) = 1 / (Q(5,17))
  den(14) = 1 / (Q(5,18))
  den(16) = 1 / (Q(5,28))
  den(20) = 1 / (Q(5,6))
  den(24) = 1 / (Q(5,5))
  den(30) = 1 / (Q(5,19))
  den(35) = 1 / (Q(5,11))
  den(40) = 1 / (Q(5,7))
  den(43) = 1 / (Q(5,22))
  den(48) = 1 / (Q(5,21))
  den(53) = 1 / (Q(5,14))
  den(58) = 1 / (Q(5,13))
  den(63) = 1 / (Q(5,25))
  den(65) = 1 / (Q(5,26))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(1)*den(6)
  den(9) = den(4)*den(8)
  den(11) = den(4)*den(10)
  den(13) = den(2)*den(12)
  den(15) = den(2)*den(14)
  den(17) = den(1)*den(16)
  den(18) = den(8)*den(14)
  den(19) = den(10)*den(12)
  den(21) = den(8)*den(20)
  den(22) = den(12)*den(20)
  den(23) = den(6)*den(20)
  den(25) = den(10)*den(24)
  den(26) = den(14)*den(24)
  den(27) = den(6)*den(24)
  den(28) = den(2)*den(16)
  den(29) = den(1)*den(28)
  den(31) = den(1)*den(30)
  den(32) = den(2)*den(31)
  den(33) = den(4)*den(16)
  den(34) = den(1)*den(33)
  den(36) = den(1)*den(35)
  den(37) = den(4)*den(36)
  den(38) = den(6)*den(16)
  den(39) = den(1)*den(38)
  den(41) = den(1)*den(40)
  den(42) = den(6)*den(41)
  den(44) = den(4)*den(43)
  den(45) = den(8)*den(44)
  den(46) = den(8)*den(35)
  den(47) = den(4)*den(46)
  den(49) = den(4)*den(48)
  den(50) = den(10)*den(49)
  den(51) = den(10)*den(35)
  den(52) = den(4)*den(51)
  den(54) = den(2)*den(53)
  den(55) = den(12)*den(54)
  den(56) = den(12)*den(30)
  den(57) = den(2)*den(56)
  den(59) = den(2)*den(58)
  den(60) = den(14)*den(59)
  den(61) = den(14)*den(30)
  den(62) = den(2)*den(61)
  den(64) = den(8)*den(63)
  den(66) = den(10)*den(65)
  den(67) = den(12)*den(63)
  den(68) = den(14)*den(65)
  den(69) = den(6)*den(63)
  den(70) = den(6)*den(65)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(59)

  A(1) = cont_VV(wf(:,1),wf(:,2)) * den(1)
  A(2) = cont_VV(wf(:,3),wf(:,4)) * den(3)
  A(3) = cont_VV(wf(:,5),wf(:,6)) * den(5)
  A(4) = cont_VV(wf(:,7),wf(:,8)) * den(7)
  A(5) = cont_VV(wf(:,5),wf(:,11)) * den(9)
  A(6) = cont_VV(wf(:,5),wf(:,14)) * den(11)
  A(7) = cont_VV(wf(:,3),wf(:,17)) * den(13)
  A(8) = cont_VV(wf(:,3),wf(:,20)) * den(15)

  A(9) = cont_VV(wf(:,1),wf(:,21)) * den(1)
  A(10) = cont_QA(wf(:,10),wf(:,22)) * den(8)
  A(11) = cont_QA(wf(:,16),wf(:,23)) * den(12)
  A(12) = cont_VV(wf(:,2),wf(:,24)) * den(16)
  A(13) = cont_QA(wf(:,13),wf(:,25)) * den(10)
  A(14) = cont_QA(wf(:,19),wf(:,26)) * den(14)
  A(15) = cont_VV(wf(:,7),wf(:,27)) * den(6)
  A(16) = cont_VV(wf(:,3),wf(:,28)) * den(3)
  A(17) = cont_VV(wf(:,5),wf(:,29)) * den(5)
  A(18) = cont_VV(wf(:,8),wf(:,30)) * den(7)
  A(19) = cont_VV(wf(:,2),wf(:,31)) * den(17)
  A(20) = cont_VV(wf(:,7),wf(:,32)) * den(7)
  A(21) = cont_VV(wf(:,6),wf(:,33)) * den(5)
  A(22) = cont_VV(wf(:,4),wf(:,34)) * den(3)
  A(23) = cont_QA(wf(:,19),wf(:,35)) * den(18)
  A(24) = cont_VV(wf(:,11),wf(:,33)) * den(9)
  A(25) = cont_QA(wf(:,13),wf(:,36)) * den(19)
  A(26) = cont_VV(wf(:,14),wf(:,33)) * den(11)
  A(27) = cont_VV(wf(:,17),wf(:,34)) * den(13)
  A(28) = cont_VV(wf(:,20),wf(:,34)) * den(15)
  A(29) = cont_VV(wf(:,5),wf(:,37)) * den(9)
  A(30) = cont_VV(wf(:,3),wf(:,38)) * den(13)
  A(31) = cont_VV(wf(:,3),wf(:,41)) * den(15)
  A(32) = cont_VV(wf(:,5),wf(:,44)) * den(11)
  A(33) = cont_QA(wf(:,46),wf(:,47)) * den(21)
  A(34) = cont_QA(wf(:,46),wf(:,48)) * den(22)
  A(35) = cont_VV(wf(:,7),wf(:,49)) * den(23)
  A(36) = cont_VV(wf(:,5),wf(:,50)) * den(11)
  A(37) = cont_VV(wf(:,3),wf(:,51)) * den(15)
  A(38) = cont_VV(wf(:,3),wf(:,54)) * den(13)
  A(39) = cont_VV(wf(:,5),wf(:,57)) * den(9)
  A(40) = cont_QA(wf(:,13),wf(:,60)) * den(25)
  A(41) = cont_QA(wf(:,19),wf(:,61)) * den(26)
  A(42) = cont_VV(wf(:,7),wf(:,62)) * den(27)
  A(43) = cont_VV(wf(:,3),wf(:,63)) * den(3)
  A(44) = cont_VV(wf(:,5),wf(:,64)) * den(5)
  A(45) = cont_VV(wf(:,7),wf(:,65)) * den(7)
  A(46) = cont_VV(wf(:,31),wf(:,66)) * den(29)
  A(47) = cont_VV(wf(:,4),wf(:,67)) * den(32)
  A(48) = cont_VV(wf(:,31),wf(:,68)) * den(34)
  A(49) = cont_VV(wf(:,6),wf(:,69)) * den(37)
  A(50) = cont_VV(wf(:,31),wf(:,70)) * den(39)
  A(51) = cont_VV(wf(:,8),wf(:,71)) * den(42)
  A(52) = cont_QA(wf(:,73),wf(:,74)) * den(45)
  A(53) = cont_VV(wf(:,11),wf(:,69)) * den(47)
  A(54) = cont_QA(wf(:,76),wf(:,77)) * den(50)
  A(55) = cont_VV(wf(:,14),wf(:,69)) * den(52)
  A(56) = cont_QA(wf(:,79),wf(:,80)) * den(55)
  A(57) = cont_VV(wf(:,17),wf(:,67)) * den(57)
  A(58) = cont_QA(wf(:,82),wf(:,83)) * den(60)
  A(59) = cont_VV(wf(:,20),wf(:,67)) * den(62)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(59)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = (-A(6)-A(7))*f(1)+CI*(-A(1)+A(2)-A(3)-A(4))*f(2)
  M1(2) = (-A(5)-A(8))*f(1)+CI*(A(1)-A(2)+A(3)+A(4))*f(2)

  M2(1) = (-A(26)-A(27)+A(54)+A(55)+A(56)+A(57))*f(3)+CI*(A(19)-A(20)-A(21)+A(22)+A(46)-A(47)+A(48)+A(49)+A(50)+A(51))*f(4)+( &
       -A(30)-A(32)-A(36)-A(38))*f(5)+CI*(-A(12)+A(43)-A(44)-A(45))*f(6)-CI*A(9)*f(7)+CI*(A(16)-A(17)-A(18))*f(8)+(A(11) &
       +A(13))*f(9)+CI*A(15)*f(10)+(A(25)+A(34)+A(40))*f(11)+CI*(A(35)+A(42))*f(12)
  M2(2) = (-A(24)-A(28)+A(52)+A(53)+A(58)+A(59))*f(3)+CI*(-A(19)+A(20)+A(21)-A(22)-A(46)+A(47)-A(48)-A(49)-A(50)-A(51))*f(4)+( &
       -A(29)-A(31)-A(37)-A(39))*f(5)+CI*(A(12)-A(43)+A(44)+A(45))*f(6)+CI*A(9)*f(7)+CI*(-A(16)+A(17)+A(18))*f(8)+(A(10) &
       +A(14))*f(9)-CI*A(15)*f(10)+(A(23)+A(33)+A(41))*f(11)+CI*(-A(35)-A(42))*f(12)

end subroutine colourvectors

end module ol_loop_heftpphjj_ddxhgg_1_/**/REALKIND
