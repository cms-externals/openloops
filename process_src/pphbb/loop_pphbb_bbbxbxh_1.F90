
module ol_colourmatrix_pphbb_bbbxbxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(34,2), K2(2,2), KL(2,2)
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
  K1( 5,:) = [   0,   4]
  K1( 6,:) = [   4,   0]
  K1( 7,:) = [  12,   4]
  K1( 8,:) = [   4,  12]
  K1( 9,:) = [   0,  -4]
  K1(10,:) = [  -4, -12]
  K1(11,:) = [ -12,  -4]
  K1(12,:) = [  -4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [ -12,  -4]
  K1(16,:) = [  -4,   0]
  K1(17,:) = [   0,  -4]
  K1(18,:) = [  -4, -12]
  K1(19,:) = [   0,   4]
  K1(20,:) = [   4,   0]
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

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphbb_bbbxbxh_1_/**/REALKIND



module ol_forced_parameters_pphbb_bbbxbxh_1_/**/REALKIND
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
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphbb_bbbxbxh_1_/**/REALKIND

module ol_loop_pphbb_bbbxbxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(14), c(21)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:74)
  ! denominators
  complex(REALKIND), save :: den(58)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,16), Mct(2,16), Mcol_loop(2,16)
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
    f( 2) = (CI*eQED*gQCD**2*YB)/(2._/**/REALKIND*MW*sw)
    f( 3) = (CI*countertermnorm*eQED*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f( 4) = (CI*countertermnorm*ctGbb*eQED*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f( 5) = (CI*countertermnorm*ctSbb*eQED*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f( 6) = (CI*eQED*gQCD**4*integralnorm*SwB*YB)/(2._/**/REALKIND*MW*sw)
    f( 7) = (eQED*gQCD**4*integralnorm*SwB*YB)/(MW*sw*4._/**/REALKIND)
    f( 8) = (eQED*gQCD**4*integralnorm*SwB*YB)/(MW*sw*2._/**/REALKIND)
    f( 9) = (eQED*gQCD**4*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(10) = (eQED*gQCD**4*integralnorm*SwF*YB)/(MW*sw)
    f(11) = (CI*countertermnorm*ctHGG*eQED*gQCD**4*MC*YC)/MQ2sum
    f(12) = (eQED*gQCD**4*integralnorm*SwF*YC)/(MW*sw*2._/**/REALKIND)
    f(13) = (CI*countertermnorm*ctHGG*eQED*gQCD**4*MT*YT)/MQ2sum
    f(14) = (eQED*gQCD**4*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

  c = [ 9*CI*f(6), 27*CI*f(6), 18*f(7), 54*f(7), f(8), 3*f(8), 6*f(8), 8*f(8), 10*f(8), 18*f(8), 21*f(8), 24*f(8), 54*f(8), 3*f(9) &
    , 9*f(9), 3*f(10), 9*f(10), 3*f(12), 9*f(12), 3*f(14), 9*f(14) ]
  c = (1._/**/REALKIND / 36) * c
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
  complex(REALKIND) :: A(54)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rMB, H(1), wf(:,0), POLSEL(1))
    call pol_wf_Q(P(:,2), rMB, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_A(P(:,3), rMB, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rMB, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_Q(P(:,1), rMB, H(1), wf(:,0), 0)
    call pol_wf_Q(P(:,2), rMB, H(2), wf(:,-1), 0)
    call pol_wf_A(P(:,3), rMB, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rMB, H(4), wf(:,-3), 0)
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,1))
  call vert_QS_A(gH,wf(:,-1),wf(:,-4),wf(:,2))
  call prop_Q_A(wf(:,2),Q(:,18),MB,1_intkind1,wf(:,3))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,4))
  call vert_SA_Q(gH,wf(:,-4),wf(:,-3),wf(:,5))
  call prop_A_Q(wf(:,5),Q(:,24),MB,1_intkind1,wf(:,6))
  call vert_VQ_A(wf(:,1),wf(:,-1),wf(:,7))
  call vert_QS_A(gH,wf(:,0),wf(:,-4),wf(:,8))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,9))
  call prop_Q_A(wf(:,8),Q(:,17),MB,1_intkind1,wf(:,10))
  call vert_QA_V(wf(:,10),wf(:,-3),wf(:,11))
  call vert_VQ_A(wf(:,9),wf(:,0),wf(:,12))
  call vert_QA_V(wf(:,0),wf(:,-3),wf(:,13))
  call vert_AV_Q(wf(:,-2),wf(:,13),wf(:,14))
  call vert_SA_Q(gH,wf(:,-4),wf(:,-2),wf(:,15))
  call prop_A_Q(wf(:,15),Q(:,20),MB,1_intkind1,wf(:,16))
  call vert_VQ_A(wf(:,13),wf(:,-1),wf(:,17))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,18))
  call vert_QA_V(wf(:,10),wf(:,-2),wf(:,19))
  call vert_VQ_A(wf(:,18),wf(:,0),wf(:,20))
  call counter_SG_G(wf(:,-4),wf(:,1),wf(:,21))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,22))
  call counter_SA_Q(gH,wf(:,-4),wf(:,-3),wf(:,23))
  call prop_A_Q(wf(:,23),Q(:,24),MB,1_intkind1,wf(:,24))
  call counter_SG_G(wf(:,-4),wf(:,13),wf(:,25))
  call counter_QA_V(wf(:,10),wf(:,-3),wf(:,26))
  call counter_AV_Q(wf(:,-2),wf(:,13),wf(:,27))
  call counter_SA_Q(gH,wf(:,-4),wf(:,-2),wf(:,28))
  call prop_A_Q(wf(:,28),Q(:,20),MB,1_intkind1,wf(:,29))
  call counter_QA_V(wf(:,10),wf(:,-2),wf(:,30))
  call counter_VQ_A(wf(:,1),wf(:,-1),wf(:,31))
  call counter_QS_A(gH,wf(:,-1),wf(:,-4),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,18),MB,1_intkind1,wf(:,33))
  call counter_VQ_A(wf(:,13),wf(:,-1),wf(:,34))
  call counter_QA_V(wf(:,-1),wf(:,-3),wf(:,35))
  call vert_VQ_A(wf(:,35),wf(:,0),wf(:,36))
  call counter_QA_V(wf(:,-1),wf(:,-2),wf(:,37))
  call vert_VQ_A(wf(:,37),wf(:,0),wf(:,38))
  call counter_VQ_A(wf(:,9),wf(:,0),wf(:,39))
  call counter_QS_A(gH,wf(:,0),wf(:,-4),wf(:,40))
  call prop_Q_A(wf(:,40),Q(:,17),MB,1_intkind1,wf(:,41))
  call vert_QA_V(wf(:,41),wf(:,-3),wf(:,42))
  call counter_VQ_A(wf(:,18),wf(:,0),wf(:,43))
  call vert_QA_V(wf(:,41),wf(:,-2),wf(:,44))
  call counter_QA_V(wf(:,0),wf(:,-3),wf(:,45))
  call vert_AV_Q(wf(:,-2),wf(:,45),wf(:,46))
  call vert_VQ_A(wf(:,45),wf(:,-1),wf(:,47))
  call counter_QA_V(wf(:,0),wf(:,-2),wf(:,48))
  call vert_AV_Q(wf(:,-3),wf(:,48),wf(:,49))
  call vert_VQ_A(wf(:,48),wf(:,-1),wf(:,50))
  call vert_QA_V(wf(:,3),wf(:,-3),wf(:,51))
  call counter_V_V(ctGG,wf(:,1),Q(:,5),wf(:,52))
  call counter_Q_A(ctbb,wf(:,3),Q(:,18),wf(:,53))
  call prop_A_Q(wf(:,4),Q(:,13),MB,1_intkind1,wf(:,54))
  call vert_QA_V(wf(:,-1),wf(:,6),wf(:,55))
  call counter_A_Q(ctbb,wf(:,6),Q(:,24),wf(:,56))
  call prop_Q_A(wf(:,7),Q(:,7),MB,1_intkind1,wf(:,57))
  call vert_AV_Q(wf(:,-3),wf(:,9),wf(:,58))
  call counter_Q_A(ctbb,wf(:,10),Q(:,17),wf(:,59))
  call prop_A_Q(wf(:,58),Q(:,14),MB,1_intkind1,wf(:,60))
  call counter_V_V(ctGG,wf(:,9),Q(:,6),wf(:,61))
  call vert_QA_V(wf(:,0),wf(:,6),wf(:,62))
  call prop_Q_A(wf(:,12),Q(:,7),MB,1_intkind1,wf(:,63))
  call vert_QA_V(wf(:,3),wf(:,-2),wf(:,64))
  call counter_V_V(ctGG,wf(:,13),Q(:,9),wf(:,65))
  call prop_A_Q(wf(:,14),Q(:,13),MB,1_intkind1,wf(:,66))
  call vert_QA_V(wf(:,-1),wf(:,16),wf(:,67))
  call counter_A_Q(ctbb,wf(:,16),Q(:,20),wf(:,68))
  call prop_Q_A(wf(:,17),Q(:,11),MB,1_intkind1,wf(:,69))
  call vert_AV_Q(wf(:,-2),wf(:,18),wf(:,70))
  call prop_A_Q(wf(:,70),Q(:,14),MB,1_intkind1,wf(:,71))
  call counter_V_V(ctGG,wf(:,18),Q(:,10),wf(:,72))
  call vert_QA_V(wf(:,0),wf(:,16),wf(:,73))
  call prop_Q_A(wf(:,20),Q(:,11),MB,1_intkind1,wf(:,74))

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
  den(2) = 1 / (Q(5,18) - MB2)
  den(4) = 1 / (Q(5,24) - MB2)
  den(6) = 1 / (Q(5,17) - MB2)
  den(7) = 1 / (Q(5,6))
  den(10) = 1 / (Q(5,9))
  den(12) = 1 / (Q(5,20) - MB2)
  den(14) = 1 / (Q(5,10))
  den(19) = 1 / (Q(5,26))
  den(22) = 1 / (Q(5,13) - MB2)
  den(27) = 1 / (Q(5,7) - MB2)
  den(30) = 1 / (Q(5,14) - MB2)
  den(33) = 1 / (Q(5,25))
  den(40) = 1 / (Q(5,22))
  den(47) = 1 / (Q(5,11) - MB2)
  den(52) = 1 / (Q(5,21))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(8) = den(6)*den(7)
  den(9) = den(4)*den(7)
  den(11) = den(2)*den(10)
  den(13) = den(10)*den(12)
  den(15) = den(6)*den(14)
  den(16) = den(12)*den(14)
  den(17) = den(1)*den(14)
  den(18) = den(7)*den(10)
  den(20) = den(2)*den(19)
  den(21) = den(1)*den(20)
  den(23) = den(1)*den(22)
  den(24) = den(2)*den(23)
  den(25) = den(4)*den(19)
  den(26) = den(1)*den(25)
  den(28) = den(1)*den(27)
  den(29) = den(4)*den(28)
  den(31) = den(7)*den(30)
  den(32) = den(6)*den(31)
  den(34) = den(6)*den(33)
  den(35) = den(7)*den(34)
  den(36) = den(4)*den(33)
  den(37) = den(7)*den(36)
  den(38) = den(7)*den(27)
  den(39) = den(4)*den(38)
  den(41) = den(2)*den(40)
  den(42) = den(10)*den(41)
  den(43) = den(10)*den(22)
  den(44) = den(2)*den(43)
  den(45) = den(12)*den(40)
  den(46) = den(10)*den(45)
  den(48) = den(10)*den(47)
  den(49) = den(12)*den(48)
  den(50) = den(14)*den(30)
  den(51) = den(6)*den(50)
  den(53) = den(6)*den(52)
  den(54) = den(14)*den(53)
  den(55) = den(12)*den(52)
  den(56) = den(14)*den(55)
  den(57) = den(14)*den(47)
  den(58) = den(12)*den(57)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(54)

  A(1) = cont_QA(wf(:,3),wf(:,4)) * den(3)
  A(2) = cont_QA(wf(:,6),wf(:,7)) * den(5)
  A(3) = cont_VV(wf(:,9),wf(:,11)) * den(8)
  A(4) = cont_QA(wf(:,6),wf(:,12)) * den(9)
  A(5) = cont_QA(wf(:,3),wf(:,14)) * den(11)
  A(6) = cont_QA(wf(:,16),wf(:,17)) * den(13)
  A(7) = cont_VV(wf(:,18),wf(:,19)) * den(15)
  A(8) = cont_QA(wf(:,16),wf(:,20)) * den(16)

  A(9) = cont_VV(wf(:,18),wf(:,21)) * den(17)
  A(10) = cont_VV(wf(:,18),wf(:,21)) * den(17)
  A(11) = cont_VV(wf(:,18),wf(:,21)) * den(17)
  A(12) = cont_QA(wf(:,3),wf(:,22)) * den(3)
  A(13) = cont_QA(wf(:,7),wf(:,24)) * den(5)
  A(14) = cont_VV(wf(:,9),wf(:,25)) * den(18)
  A(15) = cont_VV(wf(:,9),wf(:,25)) * den(18)
  A(16) = cont_VV(wf(:,9),wf(:,25)) * den(18)
  A(17) = cont_VV(wf(:,9),wf(:,26)) * den(8)
  A(18) = cont_QA(wf(:,12),wf(:,24)) * den(9)
  A(19) = cont_QA(wf(:,3),wf(:,27)) * den(11)
  A(20) = cont_QA(wf(:,17),wf(:,29)) * den(13)
  A(21) = cont_VV(wf(:,18),wf(:,30)) * den(15)
  A(22) = cont_QA(wf(:,20),wf(:,29)) * den(16)
  A(23) = cont_QA(wf(:,6),wf(:,31)) * den(5)
  A(24) = cont_QA(wf(:,4),wf(:,33)) * den(3)
  A(25) = cont_QA(wf(:,16),wf(:,34)) * den(13)
  A(26) = cont_QA(wf(:,14),wf(:,33)) * den(11)
  A(27) = cont_VV(wf(:,19),wf(:,35)) * den(15)
  A(28) = cont_QA(wf(:,16),wf(:,36)) * den(16)
  A(29) = cont_VV(wf(:,11),wf(:,37)) * den(8)
  A(30) = cont_QA(wf(:,6),wf(:,38)) * den(9)
  A(31) = cont_QA(wf(:,6),wf(:,39)) * den(9)
  A(32) = cont_VV(wf(:,9),wf(:,42)) * den(8)
  A(33) = cont_QA(wf(:,16),wf(:,43)) * den(16)
  A(34) = cont_VV(wf(:,18),wf(:,44)) * den(15)
  A(35) = cont_QA(wf(:,3),wf(:,46)) * den(11)
  A(36) = cont_QA(wf(:,16),wf(:,47)) * den(13)
  A(37) = cont_QA(wf(:,3),wf(:,49)) * den(3)
  A(38) = cont_QA(wf(:,6),wf(:,50)) * den(5)
  A(39) = cont_VV(wf(:,51),wf(:,52)) * den(21)
  A(40) = cont_QA(wf(:,53),wf(:,54)) * den(24)
  A(41) = cont_VV(wf(:,52),wf(:,55)) * den(26)
  A(42) = cont_QA(wf(:,56),wf(:,57)) * den(29)
  A(43) = cont_QA(wf(:,59),wf(:,60)) * den(32)
  A(44) = cont_VV(wf(:,11),wf(:,61)) * den(35)
  A(45) = cont_VV(wf(:,61),wf(:,62)) * den(37)
  A(46) = cont_QA(wf(:,56),wf(:,63)) * den(39)
  A(47) = cont_VV(wf(:,64),wf(:,65)) * den(42)
  A(48) = cont_QA(wf(:,53),wf(:,66)) * den(44)
  A(49) = cont_VV(wf(:,65),wf(:,67)) * den(46)
  A(50) = cont_QA(wf(:,68),wf(:,69)) * den(49)
  A(51) = cont_QA(wf(:,59),wf(:,71)) * den(51)
  A(52) = cont_VV(wf(:,19),wf(:,72)) * den(54)
  A(53) = cont_VV(wf(:,72),wf(:,73)) * den(56)
  A(54) = cont_QA(wf(:,68),wf(:,74)) * den(58)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(54)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(3)-A(4)-A(5)-A(6))*f(2))/6._/**/REALKIND+((-A(1)-A(2)-A(7)-A(8))*f(2))/2._/**/REALKIND
  M1(2) = ((A(3)+A(4)+A(5)+A(6))*f(2))/2._/**/REALKIND+((A(1)+A(2)+A(7)+A(8))*f(2))/6._/**/REALKIND

  M2(1) = -(A(10)*f(1))/2._/**/REALKIND-(A(15)*f(1))/6._/**/REALKIND+((A(43)+A(44)+A(45)+A(46)+A(47)+A(48)+A(49) &
       +A(50))*f(3))/6._/**/REALKIND+((A(39)+A(40)+A(41)+A(42)+A(51)+A(52)+A(53)+A(54))*f(3))/2._/**/REALKIND+((-A(17)-A(19)-A(25) &
       -A(29)-A(30)-A(31)-A(35)-A(36))*f(4))/6._/**/REALKIND+((-A(12)-A(21)-A(23)-A(27)-A(28)-A(33)-A(37) &
       -A(38))*f(4))/2._/**/REALKIND+((-A(18)-A(20)-A(26)-A(32))*f(5))/6._/**/REALKIND+((-A(13)-A(22)-A(24) &
       -A(34))*f(5))/2._/**/REALKIND-(A(11)*f(11))/2._/**/REALKIND-(A(16)*f(11))/6._/**/REALKIND-(A(9)*f(13))/2._/**/REALKIND &
       -(A(14)*f(13))/6._/**/REALKIND
  M2(2) = (A(10)*f(1))/6._/**/REALKIND+(A(15)*f(1))/2._/**/REALKIND+((-A(43)-A(44)-A(45)-A(46)-A(47)-A(48)-A(49) &
       -A(50))*f(3))/2._/**/REALKIND+((-A(39)-A(40)-A(41)-A(42)-A(51)-A(52)-A(53)-A(54))*f(3))/6._/**/REALKIND+((A(17)+A(19)+A(25) &
       +A(29)+A(30)+A(31)+A(35)+A(36))*f(4))/2._/**/REALKIND+((A(12)+A(21)+A(23)+A(27)+A(28)+A(33)+A(37) &
       +A(38))*f(4))/6._/**/REALKIND+((A(18)+A(20)+A(26)+A(32))*f(5))/2._/**/REALKIND+((A(13)+A(22)+A(24) &
       +A(34))*f(5))/6._/**/REALKIND+(A(11)*f(11))/6._/**/REALKIND+(A(16)*f(11))/2._/**/REALKIND+(A(9)*f(13))/6._/**/REALKIND &
       +(A(14)*f(13))/2._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pphbb_bbbxbxh_1_/**/REALKIND
