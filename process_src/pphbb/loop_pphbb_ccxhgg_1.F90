
module ol_colourmatrix_pphbb_ccxhgg_1_/**/REALKIND
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
end module ol_colourmatrix_pphbb_ccxhgg_1_/**/REALKIND



module ol_forced_parameters_pphbb_ccxhgg_1_/**/REALKIND
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
  if (CKMORDER /= 0) write(*,101) 'CKMORDER = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (YU /= 0) write(*,101) 'YU = 0'
  if (YD /= 0) write(*,101) 'YD = 0'
  if (YS /= 0) write(*,101) 'YS = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMC /= 0) write(*,101) 'wMC = 0'
  if (wMC /= 0) write(*,101) 'wMC = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphbb_ccxhgg_1_/**/REALKIND

module ol_loop_pphbb_ccxhgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(26), c(23)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:89)
  ! denominators
  complex(REALKIND), save :: den(72)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,16), Mct(2,16), Mcol_loop(3,16)
  ! zero helicity identifier
  logical,           save :: zerohel(16) = .true., zerohel_ct(16) = .true.

  contains

! **********************************************************************
subroutine fac_init_loop()
! Writes diagram prefactors to 'f', rsp. 'c'
! **********************************************************************
  use ol_parameters_decl_/**/REALKIND
  use ol_parameters_decl_/**/DREALKIND, only: do_ew_renorm
  use ol_parameters_init_/**/REALKIND, only: parameters_init, loop_parameters_init
  use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB
!  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
#endif
  implicit none
  logical :: force_ew_renorm
  if (do_ew_renorm <= 0 .and. 0 >= 1) then
    ! If do_ew_renorm is needed, but not already enabled,
    ! force calling loop_parameters_init().
    force_ew_renorm = .true.
  else
    force_ew_renorm = .false.
  end if
  do_ew_renorm = 0
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0 .or. force_ew_renorm) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*countertermnorm*ctHGG*eQED*gQCD**4*MB*YB)/MQ2sum
    f( 2) = (countertermnorm*ctHGG*eQED*gQCD**4*MB*YB)/MQ2sum
    f( 3) = (CI*eQED*gQCD**4*integralnorm*SwF*YB)/(2._/**/REALKIND*MW*sw)
    f( 4) = (eQED*gQCD**4*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f( 5) = (CI*countertermnorm*ctHGG*eQED*gQCD**4*MC*YC)/MQ2sum
    f( 6) = (countertermnorm*ctHGG*eQED*gQCD**4*MC*YC)/MQ2sum
    f( 7) = (CI*eQED*gQCD**2*YC)/(2._/**/REALKIND*MW*sw)
    f( 8) = (eQED*gQCD**2*YC)/(MW*sw*2._/**/REALKIND)
    f( 9) = (CI*countertermnorm*eQED*gQCD**4*YC)/(2._/**/REALKIND*MW*sw)
    f(10) = (countertermnorm*eQED*gQCD**4*YC)/(MW*sw*2._/**/REALKIND)
    f(11) = (CI*countertermnorm*ctGcc*eQED*gQCD**4*YC)/(2._/**/REALKIND*MW*sw)
    f(12) = (countertermnorm*ctGcc*eQED*gQCD**4*YC)/(MW*sw*2._/**/REALKIND)
    f(13) = (CI*countertermnorm*ctScc*eQED*gQCD**4*YC)/(2._/**/REALKIND*MW*sw)
    f(14) = (countertermnorm*ctScc*eQED*gQCD**4*YC)/(MW*sw*2._/**/REALKIND)
    f(15) = (countertermnorm*ctVVV*eQED*gQCD**4*YC)/(MW*sw*2._/**/REALKIND)
    f(16) = (CI*eQED*gQCD**4*integralnorm*SwB*YC)/(4._/**/REALKIND*MW*sw)
    f(17) = (CI*eQED*gQCD**4*integralnorm*SwB*YC)/(2._/**/REALKIND*MW*sw)
    f(18) = (eQED*gQCD**4*integralnorm*SwB*YC)/(MW*sw*2._/**/REALKIND)
    f(19) = (CI*eQED*gQCD**4*integralnorm*SwF*YC)/(2._/**/REALKIND*MW*sw)
    f(20) = (CI*eQED*gQCD**4*integralnorm*SwF*YC)/(MW*sw)
    f(21) = (eQED*gQCD**4*integralnorm*SwF*YC)/(MW*sw*2._/**/REALKIND)
    f(22) = (eQED*gQCD**4*integralnorm*SwF*YC)/(MW*sw)
    f(23) = (CI*countertermnorm*ctHGG*eQED*gQCD**4*MT*YT)/MQ2sum
    f(24) = (countertermnorm*ctHGG*eQED*gQCD**4*MT*YT)/MQ2sum
    f(25) = (CI*eQED*gQCD**4*integralnorm*SwF*YT)/(2._/**/REALKIND*MW*sw)
    f(26) = (eQED*gQCD**4*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

  c = [ 3*CI*f(3), f(4), 3*f(4), 9*CI*f(16), 18*CI*f(16), CI*f(17), 3*CI*f(17), 8*CI*f(17), 9*CI*f(17), 18*CI*f(17), f(18) &
    , 3*f(18), 8*f(18), 9*f(18), 3*CI*f(19), 3*CI*f(20), f(21), 3*f(21), f(22), 3*f(22), 3*CI*f(25), f(26), 3*f(26) ]
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
  complex(REALKIND) :: A(69)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rMC, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rMC, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_S(P(:,3), rMH, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_Q(P(:,1), rMC, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rMC, H(2), wf(:,-1), 0)
    call pol_wf_S(P(:,3), rMH, H(3), wf(:,-2), 0)
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_QS_A(gH,wf(:,0),wf(:,-2),wf(:,1))
  call vert_AV_Q(wf(:,-1),wf(:,-3),wf(:,2))
  call prop_Q_A(wf(:,1),Q(:,5),MC,1_intkind1,wf(:,3))
  call prop_A_Q(wf(:,2),Q(:,10),MC,1_intkind1,wf(:,4))
  call vert_VQ_A(wf(:,-4),wf(:,3),wf(:,5))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,6))
  call prop_A_Q(wf(:,6),Q(:,18),MC,1_intkind1,wf(:,7))
  call vert_VQ_A(wf(:,-3),wf(:,3),wf(:,8))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,9))
  call vert_QA_V(wf(:,3),wf(:,-1),wf(:,10))
  call vert_VQ_A(wf(:,-3),wf(:,0),wf(:,11))
  call vert_SA_Q(gH,wf(:,-2),wf(:,-1),wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,9),MC,1_intkind1,wf(:,13))
  call prop_A_Q(wf(:,12),Q(:,6),MC,1_intkind1,wf(:,14))
  call vert_VQ_A(wf(:,-4),wf(:,13),wf(:,15))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,16))
  call prop_Q_A(wf(:,16),Q(:,17),MC,1_intkind1,wf(:,17))
  call vert_VQ_A(wf(:,-3),wf(:,17),wf(:,18))
  call vert_QA_V(wf(:,0),wf(:,14),wf(:,19))
  call vert_QS_A(gH,wf(:,13),wf(:,-2),wf(:,20))
  call vert_QS_A(gH,wf(:,17),wf(:,-2),wf(:,21))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,22))
  call counter_SG_G(wf(:,-2),wf(:,22),wf(:,23))
  call counter_SG_G(wf(:,-2),wf(:,-4),wf(:,24))
  call vert_UV_W(wf(:,22),Q(:,3),wf(:,-3),Q(:,8),wf(:,25))
  call counter_SG_G(wf(:,-2),wf(:,-3),wf(:,26))
  call vert_UV_W(wf(:,22),Q(:,3),wf(:,-4),Q(:,16),wf(:,27))
  call counter_VQ_A(wf(:,-4),wf(:,3),wf(:,28))
  call counter_VQ_A(wf(:,-3),wf(:,3),wf(:,29))
  call counter_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,30))
  call counter_VQ_A(wf(:,-4),wf(:,13),wf(:,31))
  call counter_VQ_A(wf(:,-3),wf(:,17),wf(:,32))
  call counter_QS_A(gH,wf(:,13),wf(:,-2),wf(:,33))
  call vert_QA_V(wf(:,13),wf(:,-1),wf(:,34))
  call counter_QS_A(gH,wf(:,17),wf(:,-2),wf(:,35))
  call vert_QA_V(wf(:,0),wf(:,4),wf(:,36))
  call vert_QA_V(wf(:,17),wf(:,-1),wf(:,37))
  call vert_QA_V(wf(:,0),wf(:,7),wf(:,38))
  call counter_QA_V(wf(:,3),wf(:,-1),wf(:,39))
  call counter_AV_Q(wf(:,-1),wf(:,-4),wf(:,40))
  call prop_A_Q(wf(:,40),Q(:,18),MC,1_intkind1,wf(:,41))
  call counter_AV_Q(wf(:,-1),wf(:,-3),wf(:,42))
  call prop_A_Q(wf(:,42),Q(:,10),MC,1_intkind1,wf(:,43))
  call counter_SA_Q(gH,wf(:,-2),wf(:,-1),wf(:,44))
  call prop_A_Q(wf(:,44),Q(:,6),MC,1_intkind1,wf(:,45))
  call vert_QA_V(wf(:,0),wf(:,45),wf(:,46))
  call counter_QA_V(wf(:,0),wf(:,14),wf(:,47))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,48))
  call prop_Q_A(wf(:,48),Q(:,17),MC,1_intkind1,wf(:,49))
  call vert_VQ_A(wf(:,-3),wf(:,49),wf(:,50))
  call counter_VQ_A(wf(:,-3),wf(:,0),wf(:,51))
  call prop_Q_A(wf(:,51),Q(:,9),MC,1_intkind1,wf(:,52))
  call vert_VQ_A(wf(:,-4),wf(:,52),wf(:,53))
  call vert_QS_A(gH,wf(:,49),wf(:,-2),wf(:,54))
  call vert_QS_A(gH,wf(:,52),wf(:,-2),wf(:,55))
  call counter_QS_A(gH,wf(:,0),wf(:,-2),wf(:,56))
  call prop_Q_A(wf(:,56),Q(:,5),MC,1_intkind1,wf(:,57))
  call vert_VQ_A(wf(:,-4),wf(:,57),wf(:,58))
  call vert_VQ_A(wf(:,-3),wf(:,57),wf(:,59))
  call vert_QA_V(wf(:,57),wf(:,-1),wf(:,60))
  call vert_AV_Q(wf(:,4),wf(:,-4),wf(:,61))
  call counter_Q_A(ctcc,wf(:,3),Q(:,5),wf(:,62))
  call prop_A_Q(wf(:,61),Q(:,26),MC,1_intkind1,wf(:,63))
  call counter_A_Q(ctcc,wf(:,4),Q(:,10),wf(:,64))
  call prop_Q_A(wf(:,5),Q(:,21),MC,1_intkind1,wf(:,65))
  call vert_AV_Q(wf(:,7),wf(:,-3),wf(:,66))
  call prop_A_Q(wf(:,66),Q(:,26),MC,1_intkind1,wf(:,67))
  call counter_A_Q(ctcc,wf(:,7),Q(:,18),wf(:,68))
  call prop_Q_A(wf(:,8),Q(:,13),MC,1_intkind1,wf(:,69))
  call vert_AV_Q(wf(:,-1),wf(:,9),wf(:,70))
  call prop_A_Q(wf(:,70),Q(:,26),MC,1_intkind1,wf(:,71))
  call counter_V_V(ctGG,wf(:,9),Q(:,24),wf(:,72))
  call vert_AV_Q(wf(:,14),wf(:,-4),wf(:,73))
  call counter_Q_A(ctcc,wf(:,13),Q(:,9),wf(:,74))
  call prop_A_Q(wf(:,73),Q(:,22),MC,1_intkind1,wf(:,75))
  call counter_A_Q(ctcc,wf(:,14),Q(:,6),wf(:,76))
  call prop_Q_A(wf(:,15),Q(:,25),MC,1_intkind1,wf(:,77))
  call vert_AV_Q(wf(:,14),wf(:,-3),wf(:,78))
  call counter_Q_A(ctcc,wf(:,17),Q(:,17),wf(:,79))
  call prop_A_Q(wf(:,78),Q(:,14),MC,1_intkind1,wf(:,80))
  call prop_Q_A(wf(:,18),Q(:,25),MC,1_intkind1,wf(:,81))
  call vert_VQ_A(wf(:,9),wf(:,0),wf(:,82))
  call prop_Q_A(wf(:,82),Q(:,25),MC,1_intkind1,wf(:,83))
  call vert_SA_Q(gH,wf(:,-2),wf(:,7),wf(:,84))
  call prop_A_Q(wf(:,84),Q(:,22),MC,1_intkind1,wf(:,85))
  call prop_Q_A(wf(:,20),Q(:,13),MC,1_intkind1,wf(:,86))
  call vert_SA_Q(gH,wf(:,-2),wf(:,4),wf(:,87))
  call prop_A_Q(wf(:,87),Q(:,14),MC,1_intkind1,wf(:,88))
  call prop_Q_A(wf(:,21),Q(:,21),MC,1_intkind1,wf(:,89))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5) - MC2)
  den(2) = 1 / (Q(5,10) - MC2)
  den(4) = 1 / (Q(5,18) - MC2)
  den(6) = 1 / (Q(5,24))
  den(8) = 1 / (Q(5,9) - MC2)
  den(9) = 1 / (Q(5,6) - MC2)
  den(11) = 1 / (Q(5,17) - MC2)
  den(16) = 1 / (Q(5,3))
  den(18) = 1 / (Q(5,20))
  den(20) = 1 / (Q(5,12))
  den(26) = 1 / (Q(5,26) - MC2)
  den(29) = 1 / (Q(5,21) - MC2)
  den(34) = 1 / (Q(5,13) - MC2)
  den(39) = 1 / (Q(5,7))
  den(42) = 1 / (Q(5,22) - MC2)
  den(45) = 1 / (Q(5,25) - MC2)
  den(48) = 1 / (Q(5,14) - MC2)
  den(65) = 1 / (Q(5,11))
  den(67) = 1 / (Q(5,19))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(1)*den(6)
  den(10) = den(8)*den(9)
  den(12) = den(9)*den(11)
  den(13) = den(6)*den(9)
  den(14) = den(4)*den(8)
  den(15) = den(2)*den(11)
  den(17) = den(6)*den(16)
  den(19) = den(16)*den(18)
  den(21) = den(16)*den(20)
  den(22) = den(8)*den(18)
  den(23) = den(2)*den(18)
  den(24) = den(11)*den(20)
  den(25) = den(4)*den(20)
  den(27) = den(2)*den(26)
  den(28) = den(1)*den(27)
  den(30) = den(1)*den(29)
  den(31) = den(2)*den(30)
  den(32) = den(4)*den(26)
  den(33) = den(1)*den(32)
  den(35) = den(1)*den(34)
  den(36) = den(4)*den(35)
  den(37) = den(6)*den(26)
  den(38) = den(1)*den(37)
  den(40) = den(1)*den(39)
  den(41) = den(6)*den(40)
  den(43) = den(9)*den(42)
  den(44) = den(8)*den(43)
  den(46) = den(8)*den(45)
  den(47) = den(9)*den(46)
  den(49) = den(9)*den(48)
  den(50) = den(11)*den(49)
  den(51) = den(11)*den(45)
  den(52) = den(9)*den(51)
  den(53) = den(6)*den(45)
  den(54) = den(9)*den(53)
  den(55) = den(9)*den(39)
  den(56) = den(6)*den(55)
  den(57) = den(4)*den(42)
  den(58) = den(8)*den(57)
  den(59) = den(8)*den(34)
  den(60) = den(4)*den(59)
  den(61) = den(2)*den(48)
  den(62) = den(11)*den(61)
  den(63) = den(11)*den(29)
  den(64) = den(2)*den(63)
  den(66) = den(16)*den(65)
  den(68) = den(16)*den(67)
  den(69) = den(8)*den(65)
  den(70) = den(2)*den(65)
  den(71) = den(11)*den(67)
  den(72) = den(4)*den(67)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(69)

  A(1) = cont_QA(wf(:,4),wf(:,5)) * den(3)
  A(2) = cont_QA(wf(:,7),wf(:,8)) * den(5)
  A(3) = cont_VV(wf(:,9),wf(:,10)) * den(7)
  A(4) = cont_QA(wf(:,14),wf(:,15)) * den(10)
  A(5) = cont_QA(wf(:,14),wf(:,18)) * den(12)
  A(6) = cont_VV(wf(:,9),wf(:,19)) * den(13)
  A(7) = cont_QA(wf(:,7),wf(:,20)) * den(14)
  A(8) = cont_QA(wf(:,4),wf(:,21)) * den(15)

  A(9) = cont_VV(wf(:,9),wf(:,23)) * den(17)
  A(10) = cont_VV(wf(:,9),wf(:,23)) * den(17)
  A(11) = cont_VV(wf(:,9),wf(:,23)) * den(17)
  A(12) = cont_VV(wf(:,24),wf(:,25)) * den(19)
  A(13) = cont_VV(wf(:,24),wf(:,25)) * den(19)
  A(14) = cont_VV(wf(:,24),wf(:,25)) * den(19)
  A(15) = cont_VV(wf(:,26),wf(:,27)) * den(21)
  A(16) = cont_VV(wf(:,26),wf(:,27)) * den(21)
  A(17) = cont_VV(wf(:,26),wf(:,27)) * den(21)
  A(18) = cont_QA(wf(:,4),wf(:,28)) * den(3)
  A(19) = cont_QA(wf(:,7),wf(:,29)) * den(5)
  A(20) = cont_VV(wf(:,10),wf(:,30)) * den(7)
  A(21) = cont_QA(wf(:,14),wf(:,31)) * den(10)
  A(22) = cont_QA(wf(:,14),wf(:,32)) * den(12)
  A(23) = cont_VV(wf(:,19),wf(:,30)) * den(13)
  A(24) = cont_QA(wf(:,7),wf(:,33)) * den(14)
  A(25) = cont_VV(wf(:,24),wf(:,34)) * den(22)
  A(26) = cont_VV(wf(:,24),wf(:,34)) * den(22)
  A(27) = cont_VV(wf(:,24),wf(:,34)) * den(22)
  A(28) = cont_QA(wf(:,4),wf(:,35)) * den(15)
  A(29) = cont_VV(wf(:,24),wf(:,36)) * den(23)
  A(30) = cont_VV(wf(:,24),wf(:,36)) * den(23)
  A(31) = cont_VV(wf(:,24),wf(:,36)) * den(23)
  A(32) = cont_VV(wf(:,26),wf(:,37)) * den(24)
  A(33) = cont_VV(wf(:,26),wf(:,37)) * den(24)
  A(34) = cont_VV(wf(:,26),wf(:,37)) * den(24)
  A(35) = cont_VV(wf(:,26),wf(:,38)) * den(25)
  A(36) = cont_VV(wf(:,26),wf(:,38)) * den(25)
  A(37) = cont_VV(wf(:,26),wf(:,38)) * den(25)
  A(38) = cont_VV(wf(:,9),wf(:,39)) * den(7)
  A(39) = cont_QA(wf(:,8),wf(:,41)) * den(5)
  A(40) = cont_QA(wf(:,5),wf(:,43)) * den(3)
  A(41) = cont_QA(wf(:,20),wf(:,41)) * den(14)
  A(42) = cont_QA(wf(:,21),wf(:,43)) * den(15)
  A(43) = cont_QA(wf(:,15),wf(:,45)) * den(10)
  A(44) = cont_QA(wf(:,18),wf(:,45)) * den(12)
  A(45) = cont_VV(wf(:,9),wf(:,46)) * den(13)
  A(46) = cont_VV(wf(:,9),wf(:,47)) * den(13)
  A(47) = cont_QA(wf(:,14),wf(:,50)) * den(12)
  A(48) = cont_QA(wf(:,14),wf(:,53)) * den(10)
  A(49) = cont_QA(wf(:,4),wf(:,54)) * den(15)
  A(50) = cont_QA(wf(:,7),wf(:,55)) * den(14)
  A(51) = cont_QA(wf(:,4),wf(:,58)) * den(3)
  A(52) = cont_QA(wf(:,7),wf(:,59)) * den(5)
  A(53) = cont_VV(wf(:,9),wf(:,60)) * den(7)
  A(54) = cont_QA(wf(:,62),wf(:,63)) * den(28)
  A(55) = cont_QA(wf(:,64),wf(:,65)) * den(31)
  A(56) = cont_QA(wf(:,62),wf(:,67)) * den(33)
  A(57) = cont_QA(wf(:,68),wf(:,69)) * den(36)
  A(58) = cont_QA(wf(:,62),wf(:,71)) * den(38)
  A(59) = cont_VV(wf(:,10),wf(:,72)) * den(41)
  A(60) = cont_QA(wf(:,74),wf(:,75)) * den(44)
  A(61) = cont_QA(wf(:,76),wf(:,77)) * den(47)
  A(62) = cont_QA(wf(:,79),wf(:,80)) * den(50)
  A(63) = cont_QA(wf(:,76),wf(:,81)) * den(52)
  A(64) = cont_QA(wf(:,76),wf(:,83)) * den(54)
  A(65) = cont_VV(wf(:,19),wf(:,72)) * den(56)
  A(66) = cont_QA(wf(:,74),wf(:,85)) * den(58)
  A(67) = cont_QA(wf(:,68),wf(:,86)) * den(60)
  A(68) = cont_QA(wf(:,79),wf(:,88)) * den(62)
  A(69) = cont_QA(wf(:,64),wf(:,89)) * den(64)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(69)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = (A(1)+A(5)+A(8))*f(7)+CI*(A(3)+A(6))*f(8)
  M1(2) = (A(2)+A(4)+A(7))*f(7)+CI*(-A(3)-A(6))*f(8)

  M2(1) = (A(30)+A(33))*f(1)+CI*(A(10)+A(13)-A(16))*f(2)+(A(31)+A(34))*f(5)+CI*(A(11)+A(14)-A(17))*f(6)+(-A(54)-A(55)-A(62)-A(63) &
       -A(68)-A(69))*f(9)+CI*(-A(58)-A(59)-A(64)-A(65))*f(10)+(A(18)+A(22)+A(40)+A(42)+A(47)+A(49))*f(11)+CI*(A(38)+A(46))*f(12) &
       +(A(28)+A(44)+A(51))*f(13)+CI*(A(45)+A(53))*f(14)+CI*(A(20)+A(23))*f(15)+(A(29)+A(32))*f(23)+CI*(A(9)+A(12)-A(15))*f(24)
  M2(2) = (A(26)+A(36))*f(1)+CI*(-A(10)-A(13)+A(16))*f(2)+(A(27)+A(37))*f(5)+CI*(-A(11)-A(14)+A(17))*f(6)+(-A(56)-A(57)-A(60) &
       -A(61)-A(66)-A(67))*f(9)+CI*(A(58)+A(59)+A(64)+A(65))*f(10)+(A(19)+A(21)+A(39)+A(41)+A(48)+A(50))*f(11)+CI*(-A(38) &
       -A(46))*f(12)+(A(24)+A(43)+A(52))*f(13)+CI*(-A(45)-A(53))*f(14)+CI*(-A(20)-A(23))*f(15)+(A(25)+A(35))*f(23)+CI*(-A(9)-A(12) &
       +A(15))*f(24)

end subroutine colourvectors

end module ol_loop_pphbb_ccxhgg_1_/**/REALKIND
