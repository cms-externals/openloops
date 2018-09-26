
module ol_colourmatrix_ppwjj_ckm_cxdwxgg_1_/**/REALKIND
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
  K1(15,:) = [ -72,   9]
  K1(16,:) = [   9,   9]
  K1(17,:) = [   9,   9]
  K1(18,:) = [   9, -72]
  K1(19,:) = [   0,   0]
  K1(20,:) = [   0,   0]
  K1(21,:) = [ 144, -18]
  K1(22,:) = [ -18, 144]
  K1(23,:) = [   9,   9]
  K1(24,:) = [   9, -72]
  K1(25,:) = [ -72,   9]
  K1(26,:) = [   9,   9]
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
end module ol_colourmatrix_ppwjj_ckm_cxdwxgg_1_/**/REALKIND



module ol_forced_parameters_ppwjj_ckm_cxdwxgg_1_/**/REALKIND
  implicit none
  contains
  subroutine check_forced_parameters
    use ol_parameters_decl_/**/REALKIND
    use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
    use ol_loop_parameters_decl_/**/DREALKIND, only: LeadingColour, nc, nf, CKMORDER
#endif
    implicit none
    logical, save :: checks_not_written = .true.

    if (checks_not_written) then
    ! e.g.
    ! if (ME /= 0) write(*,101) 'ME = 0'
  if (ME /= 0) write(*,101) 'ME = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppwjj_ckm_cxdwxgg_1_/**/REALKIND

module ol_loop_ppwjj_ckm_cxdwxgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(18), c(17)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:79)
  ! denominators
  complex(REALKIND), save :: den(54)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,48), Mct(2,48), Mcol_loop(3,48)
  ! zero helicity identifier
  logical,           save :: zerohel(48) = .true., zerohel_ct(48) = .true.

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
    f( 1) = (CI*eQED*gQCD**2*VCKMdc)/(sqrt2*sw)
    f( 2) = (eQED*gQCD**2*VCKMdc)/(sqrt2*sw)
    f( 3) = (CI*countertermnorm*eQED*gQCD**4*VCKMdc)/(sqrt2*sw)
    f( 4) = (countertermnorm*eQED*gQCD**4*VCKMdc)/(sqrt2*sw)
    f( 5) = (CI*countertermnorm*ctGcc*eQED*gQCD**4*VCKMdc)/(sqrt2*sw)
    f( 6) = (countertermnorm*ctGcc*eQED*gQCD**4*VCKMdc)/(sqrt2*sw)
    f( 7) = (CI*countertermnorm*ctGqq*eQED*gQCD**4*VCKMdc)/(sqrt2*sw)
    f( 8) = (countertermnorm*ctGqq*eQED*gQCD**4*VCKMdc)/(sqrt2*sw)
    f( 9) = (CI*countertermnorm*ctVqq*eQED*gQCD**4*VCKMdc)/(sqrt2*sw)
    f(10) = (countertermnorm*ctVqq*eQED*gQCD**4*VCKMdc)/(sqrt2*sw)
    f(11) = (countertermnorm*ctVVV*eQED*gQCD**4*VCKMdc)/(sqrt2*sw)
    f(12) = (CI*eQED*gQCD**4*integralnorm*SwB*VCKMdc)/(2._/**/REALKIND*sqrt2*sw)
    f(13) = (CI*eQED*gQCD**4*integralnorm*SwB*VCKMdc)/(sqrt2*sw)
    f(14) = (eQED*gQCD**4*integralnorm*SwB*VCKMdc)/(sqrt2*sw)
    f(15) = (CI*eQED*gQCD**4*integralnorm*SwF*VCKMdc)/(sqrt2*sw)
    f(16) = (2*CI*eQED*gQCD**4*integralnorm*SwF*VCKMdc)/(sqrt2*sw)
    f(17) = (eQED*gQCD**4*integralnorm*SwF*VCKMdc)/(sqrt2*sw)
    f(18) = (2*eQED*gQCD**4*integralnorm*SwF*VCKMdc)/(sqrt2*sw)

  c = [ 9*CI*f(12), 18*CI*f(12), CI*f(13), 3*CI*f(13), 8*CI*f(13), 9*CI*f(13), 18*CI*f(13), f(14), 3*f(14), 8*f(14), 9*f(14) &
    , 3*CI*f(15), 3*CI*f(16), f(17), 3*f(17), f(18), 3*f(18) ]
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
  complex(REALKIND) :: A(48)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_A(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_V(P(:,3), rMW, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_A(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_V(P(:,3), rMW, H(3), wf(:,-2), 0)
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_AW_Q(wf(:,0),wf(:,-2),wf(:,1))
  call vert_VQ_A(wf(:,-3),wf(:,-1),wf(:,2))
  call prop_A_Q(wf(:,1),Q(:,5),ZERO,0_intkind1,wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,10),ZERO,0_intkind1,wf(:,4))
  call vert_AV_Q(wf(:,3),wf(:,-4),wf(:,5))
  call vert_VQ_A(wf(:,-4),wf(:,-1),wf(:,6))
  call prop_Q_A(wf(:,6),Q(:,18),ZERO,0_intkind1,wf(:,7))
  call vert_AV_Q(wf(:,3),wf(:,-3),wf(:,8))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,9))
  call vert_QA_V(wf(:,-1),wf(:,3),wf(:,10))
  call vert_AV_Q(wf(:,0),wf(:,-3),wf(:,11))
  call vert_WQ_A(wf(:,-2),wf(:,-1),wf(:,12))
  call prop_A_Q(wf(:,11),Q(:,9),ZERO,0_intkind1,wf(:,13))
  call prop_Q_A(wf(:,12),Q(:,6),ZERO,0_intkind1,wf(:,14))
  call vert_AV_Q(wf(:,13),wf(:,-4),wf(:,15))
  call vert_AV_Q(wf(:,0),wf(:,-4),wf(:,16))
  call prop_A_Q(wf(:,16),Q(:,17),ZERO,0_intkind1,wf(:,17))
  call vert_AV_Q(wf(:,17),wf(:,-3),wf(:,18))
  call vert_QA_V(wf(:,14),wf(:,0),wf(:,19))
  call vert_AW_Q(wf(:,13),wf(:,-2),wf(:,20))
  call vert_AW_Q(wf(:,17),wf(:,-2),wf(:,21))
  call counter_AV_Q(wf(:,3),wf(:,-4),wf(:,22))
  call counter_AV_Q(wf(:,3),wf(:,-3),wf(:,23))
  call counter_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,24))
  call counter_AV_Q(wf(:,13),wf(:,-4),wf(:,25))
  call counter_AV_Q(wf(:,17),wf(:,-3),wf(:,26))
  call counter_AW_Q(wf(:,13),wf(:,-2),wf(:,27))
  call counter_AW_Q(wf(:,17),wf(:,-2),wf(:,28))
  call counter_QA_V(wf(:,-1),wf(:,3),wf(:,29))
  call counter_VQ_A(wf(:,-4),wf(:,-1),wf(:,30))
  call prop_Q_A(wf(:,30),Q(:,18),ZERO,0_intkind1,wf(:,31))
  call counter_VQ_A(wf(:,-3),wf(:,-1),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,10),ZERO,0_intkind1,wf(:,33))
  call counter_WQ_A(wf(:,-2),wf(:,-1),wf(:,34))
  call prop_Q_A(wf(:,34),Q(:,6),ZERO,0_intkind1,wf(:,35))
  call vert_QA_V(wf(:,35),wf(:,0),wf(:,36))
  call counter_QA_V(wf(:,14),wf(:,0),wf(:,37))
  call counter_AV_Q(wf(:,0),wf(:,-4),wf(:,38))
  call prop_A_Q(wf(:,38),Q(:,17),ZERO,0_intkind1,wf(:,39))
  call vert_AV_Q(wf(:,39),wf(:,-3),wf(:,40))
  call counter_AV_Q(wf(:,0),wf(:,-3),wf(:,41))
  call prop_A_Q(wf(:,41),Q(:,9),ZERO,0_intkind1,wf(:,42))
  call vert_AV_Q(wf(:,42),wf(:,-4),wf(:,43))
  call vert_AW_Q(wf(:,39),wf(:,-2),wf(:,44))
  call vert_AW_Q(wf(:,42),wf(:,-2),wf(:,45))
  call counter_AW_Q(wf(:,0),wf(:,-2),wf(:,46))
  call prop_A_Q(wf(:,46),Q(:,5),ZERO,0_intkind1,wf(:,47))
  call vert_AV_Q(wf(:,47),wf(:,-4),wf(:,48))
  call vert_AV_Q(wf(:,47),wf(:,-3),wf(:,49))
  call vert_QA_V(wf(:,-1),wf(:,47),wf(:,50))
  call vert_VQ_A(wf(:,-4),wf(:,4),wf(:,51))
  call counter_A_Q(ctqq,wf(:,3),Q(:,5),wf(:,52))
  call prop_Q_A(wf(:,51),Q(:,26),ZERO,0_intkind1,wf(:,53))
  call counter_Q_A(ctqq,wf(:,4),Q(:,10),wf(:,54))
  call prop_A_Q(wf(:,5),Q(:,21),ZERO,0_intkind1,wf(:,55))
  call vert_VQ_A(wf(:,-3),wf(:,7),wf(:,56))
  call prop_Q_A(wf(:,56),Q(:,26),ZERO,0_intkind1,wf(:,57))
  call counter_Q_A(ctqq,wf(:,7),Q(:,18),wf(:,58))
  call prop_A_Q(wf(:,8),Q(:,13),ZERO,0_intkind1,wf(:,59))
  call vert_VQ_A(wf(:,9),wf(:,-1),wf(:,60))
  call prop_Q_A(wf(:,60),Q(:,26),ZERO,0_intkind1,wf(:,61))
  call counter_V_V(ctGG,wf(:,9),Q(:,24),wf(:,62))
  call vert_VQ_A(wf(:,-4),wf(:,14),wf(:,63))
  call counter_A_Q(ctcc,wf(:,13),Q(:,9),wf(:,64))
  call prop_Q_A(wf(:,63),Q(:,22),ZERO,0_intkind1,wf(:,65))
  call counter_Q_A(ctcc,wf(:,14),Q(:,6),wf(:,66))
  call prop_A_Q(wf(:,15),Q(:,25),ZERO,0_intkind1,wf(:,67))
  call vert_VQ_A(wf(:,-3),wf(:,14),wf(:,68))
  call counter_A_Q(ctcc,wf(:,17),Q(:,17),wf(:,69))
  call prop_Q_A(wf(:,68),Q(:,14),ZERO,0_intkind1,wf(:,70))
  call prop_A_Q(wf(:,18),Q(:,25),ZERO,0_intkind1,wf(:,71))
  call vert_AV_Q(wf(:,0),wf(:,9),wf(:,72))
  call prop_A_Q(wf(:,72),Q(:,25),ZERO,0_intkind1,wf(:,73))
  call vert_WQ_A(wf(:,-2),wf(:,7),wf(:,74))
  call prop_Q_A(wf(:,74),Q(:,22),ZERO,0_intkind1,wf(:,75))
  call prop_A_Q(wf(:,20),Q(:,13),ZERO,0_intkind1,wf(:,76))
  call vert_WQ_A(wf(:,-2),wf(:,4),wf(:,77))
  call prop_Q_A(wf(:,77),Q(:,14),ZERO,0_intkind1,wf(:,78))
  call prop_A_Q(wf(:,21),Q(:,21),ZERO,0_intkind1,wf(:,79))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5))
  den(2) = 1 / (Q(5,10))
  den(4) = 1 / (Q(5,18))
  den(6) = 1 / (Q(5,24))
  den(8) = 1 / (Q(5,9))
  den(9) = 1 / (Q(5,6))
  den(11) = 1 / (Q(5,17))
  den(16) = 1 / (Q(5,26))
  den(19) = 1 / (Q(5,21))
  den(24) = 1 / (Q(5,13))
  den(29) = 1 / (Q(5,7))
  den(32) = 1 / (Q(5,22))
  den(35) = 1 / (Q(5,25))
  den(38) = 1 / (Q(5,14))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(1)*den(6)
  den(10) = den(8)*den(9)
  den(12) = den(9)*den(11)
  den(13) = den(6)*den(9)
  den(14) = den(4)*den(8)
  den(15) = den(2)*den(11)
  den(17) = den(2)*den(16)
  den(18) = den(1)*den(17)
  den(20) = den(1)*den(19)
  den(21) = den(2)*den(20)
  den(22) = den(4)*den(16)
  den(23) = den(1)*den(22)
  den(25) = den(1)*den(24)
  den(26) = den(4)*den(25)
  den(27) = den(6)*den(16)
  den(28) = den(1)*den(27)
  den(30) = den(1)*den(29)
  den(31) = den(6)*den(30)
  den(33) = den(9)*den(32)
  den(34) = den(8)*den(33)
  den(36) = den(8)*den(35)
  den(37) = den(9)*den(36)
  den(39) = den(9)*den(38)
  den(40) = den(11)*den(39)
  den(41) = den(11)*den(35)
  den(42) = den(9)*den(41)
  den(43) = den(6)*den(35)
  den(44) = den(9)*den(43)
  den(45) = den(9)*den(29)
  den(46) = den(6)*den(45)
  den(47) = den(4)*den(32)
  den(48) = den(8)*den(47)
  den(49) = den(8)*den(24)
  den(50) = den(4)*den(49)
  den(51) = den(2)*den(38)
  den(52) = den(11)*den(51)
  den(53) = den(11)*den(19)
  den(54) = den(2)*den(53)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(48)

  A(1) = cont_QA(wf(:,4),wf(:,5)) * den(3)
  A(2) = cont_QA(wf(:,7),wf(:,8)) * den(5)
  A(3) = cont_VV(wf(:,9),wf(:,10)) * den(7)
  A(4) = cont_QA(wf(:,14),wf(:,15)) * den(10)
  A(5) = cont_QA(wf(:,14),wf(:,18)) * den(12)
  A(6) = cont_VV(wf(:,9),wf(:,19)) * den(13)
  A(7) = cont_QA(wf(:,7),wf(:,20)) * den(14)
  A(8) = cont_QA(wf(:,4),wf(:,21)) * den(15)

  A(9) = cont_QA(wf(:,4),wf(:,22)) * den(3)
  A(10) = cont_QA(wf(:,7),wf(:,23)) * den(5)
  A(11) = cont_VV(wf(:,10),wf(:,24)) * den(7)
  A(12) = cont_QA(wf(:,14),wf(:,25)) * den(10)
  A(13) = cont_QA(wf(:,14),wf(:,26)) * den(12)
  A(14) = cont_VV(wf(:,19),wf(:,24)) * den(13)
  A(15) = cont_QA(wf(:,7),wf(:,27)) * den(14)
  A(16) = cont_QA(wf(:,4),wf(:,28)) * den(15)
  A(17) = cont_VV(wf(:,9),wf(:,29)) * den(7)
  A(18) = cont_QA(wf(:,8),wf(:,31)) * den(5)
  A(19) = cont_QA(wf(:,5),wf(:,33)) * den(3)
  A(20) = cont_QA(wf(:,20),wf(:,31)) * den(14)
  A(21) = cont_QA(wf(:,21),wf(:,33)) * den(15)
  A(22) = cont_QA(wf(:,15),wf(:,35)) * den(10)
  A(23) = cont_QA(wf(:,18),wf(:,35)) * den(12)
  A(24) = cont_VV(wf(:,9),wf(:,36)) * den(13)
  A(25) = cont_VV(wf(:,9),wf(:,37)) * den(13)
  A(26) = cont_QA(wf(:,14),wf(:,40)) * den(12)
  A(27) = cont_QA(wf(:,14),wf(:,43)) * den(10)
  A(28) = cont_QA(wf(:,4),wf(:,44)) * den(15)
  A(29) = cont_QA(wf(:,7),wf(:,45)) * den(14)
  A(30) = cont_QA(wf(:,4),wf(:,48)) * den(3)
  A(31) = cont_QA(wf(:,7),wf(:,49)) * den(5)
  A(32) = cont_VV(wf(:,9),wf(:,50)) * den(7)
  A(33) = cont_QA(wf(:,52),wf(:,53)) * den(18)
  A(34) = cont_QA(wf(:,54),wf(:,55)) * den(21)
  A(35) = cont_QA(wf(:,52),wf(:,57)) * den(23)
  A(36) = cont_QA(wf(:,58),wf(:,59)) * den(26)
  A(37) = cont_QA(wf(:,52),wf(:,61)) * den(28)
  A(38) = cont_VV(wf(:,10),wf(:,62)) * den(31)
  A(39) = cont_QA(wf(:,64),wf(:,65)) * den(34)
  A(40) = cont_QA(wf(:,66),wf(:,67)) * den(37)
  A(41) = cont_QA(wf(:,69),wf(:,70)) * den(40)
  A(42) = cont_QA(wf(:,66),wf(:,71)) * den(42)
  A(43) = cont_QA(wf(:,66),wf(:,73)) * den(44)
  A(44) = cont_VV(wf(:,19),wf(:,62)) * den(46)
  A(45) = cont_QA(wf(:,64),wf(:,75)) * den(48)
  A(46) = cont_QA(wf(:,58),wf(:,76)) * den(50)
  A(47) = cont_QA(wf(:,69),wf(:,78)) * den(52)
  A(48) = cont_QA(wf(:,54),wf(:,79)) * den(54)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(48)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = (A(2)+A(4)+A(7))*f(1)+CI*(A(3)+A(6))*f(2)
  M1(2) = (A(1)+A(5)+A(8))*f(1)+CI*(-A(3)-A(6))*f(2)

  M2(1) = (-A(35)-A(36)-A(39)-A(40)-A(45)-A(46))*f(3)+CI*(-A(37)-A(38)-A(43)-A(44))*f(4)+(A(12)+A(27)+A(29))*f(5)+CI*A(25)*f(6) &
       +(A(10)+A(18)+A(20))*f(7)+CI*A(17)*f(8)+(A(15)+A(22)+A(31))*f(9)+CI*(A(24)+A(32))*f(10)+CI*(A(11)+A(14))*f(11)
  M2(2) = (-A(33)-A(34)-A(41)-A(42)-A(47)-A(48))*f(3)+CI*(A(37)+A(38)+A(43)+A(44))*f(4)+(A(13)+A(26)+A(28))*f(5)-CI*A(25)*f(6) &
       +(A(9)+A(19)+A(21))*f(7)-CI*A(17)*f(8)+(A(16)+A(23)+A(30))*f(9)+CI*(-A(24)-A(32))*f(10)+CI*(-A(11)-A(14))*f(11)

end subroutine colourvectors

end module ol_loop_ppwjj_ckm_cxdwxgg_1_/**/REALKIND
