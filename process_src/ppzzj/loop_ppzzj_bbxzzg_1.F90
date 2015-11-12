
module ol_colourmatrix_ppzzj_bbxzzg_1_/**/REALKIND
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

  K1( 1,:) = [  12]
  K1( 2,:) = [  16]
  K1( 3,:) = [   2]
  K1( 4,:) = [  16]
  K1( 5,:) = [   0]
  K1( 6,:) = [   0]
  K1( 7,:) = [   0]
  K1( 8,:) = [   0]
  K1( 9,:) = [   0]
  K1(10,:) = [   0]
  K1(11,:) = [   0]
  K1(12,:) = [ -18]
  K1(13,:) = [ -18]
  K1(14,:) = [   0]
  K1(15,:) = [   0]
  K1(16,:) = [  36]
  K1(17,:) = [   0]
  K1 = (1._/**/REALKIND / 3) * K1

  K2(1,:) = [ 4]

  KL(1,:) = [ 4]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppzzj_bbxzzg_1_/**/REALKIND



module ol_forced_parameters_ppzzj_bbxzzg_1_/**/REALKIND
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
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppzzj_bbxzzg_1_/**/REALKIND

module ol_loop_ppzzj_bbxzzg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(19), c(10)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:86)
  ! denominators
  complex(REALKIND), save :: den(63)
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
    f( 1) = CI*eQED**2*gQCD
    f( 2) = CI*countertermnorm*eQED**2*gQCD**3
    f( 3) = CI*countertermnorm*ctGbb*eQED**2*gQCD**3
    f( 4) = CI*countertermnorm*ctVbb*eQED**2*gQCD**3
    f( 5) = countertermnorm*ctZGG*eQED**2*gQCD**3
    f( 6) = CI*countertermnorm*ctZZGG*eQED**2*gQCD**3
    f( 7) = (CI*eQED**2*gQCD*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f( 8) = (CI*countertermnorm*eQED**2*gQCD**3*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f( 9) = (CI*countertermnorm*ctGbb*eQED**2*gQCD**3*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(10) = (CI*countertermnorm*ctSbb*eQED**2*gQCD**3*MB)/(2._/**/REALKIND*cw**2*sw**2)
    f(11) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**3*MW)/(cw**2*sw)
    f(12) = CI*eQED**2*gQCD**3*integralnorm*SwB
    f(13) = eQED**2*gQCD**3*integralnorm*SwB
    f(14) = (CI*eQED**2*gQCD**3*integralnorm*MB*SwB)/(2._/**/REALKIND*cw**2*sw**2)
    f(15) = (eQED**2*gQCD**3*integralnorm*MB*SwB)/(cw**2*sw**2*2._/**/REALKIND)
    f(16) = eQED**2*gQCD**3*integralnorm*SwF
    f(17) = 2*eQED**2*gQCD**3*integralnorm*SwF
    f(18) = (eQED**2*gQCD**3*integralnorm*MB*SwF)/(cw**2*sw**2*2._/**/REALKIND)
    f(19) = (eQED**2*gQCD**3*integralnorm*MT*SwF)/(cw**2*sw**2*2._/**/REALKIND)

  c = [ 9*CI*f(12), f(13), 8*f(13), 9*CI*f(14), f(15), 8*f(15), 3*f(16), 3*f(17), 3*f(18), 3*f(19) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,5)
  integer,           intent(in)  :: H(5)
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(50)
  ! external WFs
  call wf_Q(P(:,1), rMB, H(1), wf(:,0))
  call wf_A(P(:,2), rMB, H(2), wf(:,-1))
  call wf_V(P(:,3), rMZ, H(3), wf(:,-2))
  call wf_V(P(:,4), rMZ, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))

  ! internal WFs
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,0),wf(:,1))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-3),wf(:,2))
  call prop_Q_A(wf(:,1),Q(:,5),MB,1_intkind1,wf(:,3))
  call prop_A_Q(wf(:,2),Q(:,10),MB,1_intkind1,wf(:,4))
  call vert_VQ_A(wf(:,-4),wf(:,3),wf(:,5))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,6))
  call prop_A_Q(wf(:,6),Q(:,18),MB,1_intkind1,wf(:,7))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,3),wf(:,8))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,0),wf(:,9))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-2),wf(:,10))
  call prop_Q_A(wf(:,9),Q(:,9),MB,1_intkind1,wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,6),MB,1_intkind1,wf(:,12))
  call vert_VQ_A(wf(:,-4),wf(:,11),wf(:,13))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,14))
  call prop_Q_A(wf(:,14),Q(:,17),MB,1_intkind1,wf(:,15))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,15),wf(:,16))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,11),wf(:,17))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,15),wf(:,18))
  call vert_VV_S(wf(:,-2),wf(:,-3),wf(:,19))
  call vert_AQ_S(gH,wf(:,-1),wf(:,15),wf(:,20))
  call vert_AQ_S(gH,wf(:,7),wf(:,0),wf(:,21))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,22))
  call counter_VVG_G(wf(:,-2),wf(:,-3),wf(:,-4),wf(:,23))
  call counter_GG_S(wf(:,22),wf(:,-4),wf(:,24))
  call counter_VQ_A(wf(:,-4),wf(:,3),wf(:,25))
  call counter_ZQ_A(gZd,wf(:,-3),wf(:,3),wf(:,26))
  call counter_VG_G(wf(:,-3),wf(:,-4),Q(:,16),wf(:,27),Q(:,24))
  call vert_QA_V(wf(:,3),wf(:,-1),wf(:,28))
  call counter_VQ_A(wf(:,-4),wf(:,11),wf(:,29))
  call counter_ZQ_A(gZd,wf(:,-3),wf(:,15),wf(:,30))
  call vert_QA_V(wf(:,0),wf(:,12),wf(:,31))
  call counter_ZQ_A(gZd,wf(:,-2),wf(:,11),wf(:,32))
  call counter_VG_G(wf(:,-2),wf(:,-4),Q(:,16),wf(:,33),Q(:,20))
  call vert_QA_V(wf(:,11),wf(:,-1),wf(:,34))
  call counter_ZQ_A(gZd,wf(:,-2),wf(:,15),wf(:,35))
  call vert_QA_V(wf(:,0),wf(:,4),wf(:,36))
  call counter_AV_Q(wf(:,-1),wf(:,-4),wf(:,37))
  call prop_A_Q(wf(:,37),Q(:,18),MB,1_intkind1,wf(:,38))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,-3),wf(:,39))
  call prop_A_Q(wf(:,39),Q(:,10),MB,1_intkind1,wf(:,40))
  call counter_AQ_S(gH,wf(:,-1),wf(:,15),wf(:,41))
  call vert_AQ_S(gH,wf(:,38),wf(:,0),wf(:,42))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,-2),wf(:,43))
  call prop_A_Q(wf(:,43),Q(:,6),MB,1_intkind1,wf(:,44))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,45))
  call prop_Q_A(wf(:,45),Q(:,17),MB,1_intkind1,wf(:,46))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,46),wf(:,47))
  call counter_ZQ_A(gZd,wf(:,-3),wf(:,0),wf(:,48))
  call prop_Q_A(wf(:,48),Q(:,9),MB,1_intkind1,wf(:,49))
  call vert_VQ_A(wf(:,-4),wf(:,49),wf(:,50))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,46),wf(:,51))
  call counter_AQ_S(gH,wf(:,7),wf(:,0),wf(:,52))
  call vert_AQ_S(gH,wf(:,-1),wf(:,46),wf(:,53))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,49),wf(:,54))
  call counter_ZQ_A(gZd,wf(:,-2),wf(:,0),wf(:,55))
  call prop_Q_A(wf(:,55),Q(:,5),MB,1_intkind1,wf(:,56))
  call vert_VQ_A(wf(:,-4),wf(:,56),wf(:,57))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,56),wf(:,58))
  call vert_AV_Q(wf(:,4),wf(:,-4),wf(:,59))
  call counter_Q_A(ctbb,wf(:,3),Q(:,5),wf(:,60))
  call prop_A_Q(wf(:,59),Q(:,26),MB,1_intkind1,wf(:,61))
  call counter_A_Q(ctbb,wf(:,4),Q(:,10),wf(:,62))
  call prop_Q_A(wf(:,5),Q(:,21),MB,1_intkind1,wf(:,63))
  call vert_AZ_Q(gZd,wf(:,7),wf(:,-3),wf(:,64))
  call prop_A_Q(wf(:,64),Q(:,26),MB,1_intkind1,wf(:,65))
  call counter_A_Q(ctbb,wf(:,7),Q(:,18),wf(:,66))
  call prop_Q_A(wf(:,8),Q(:,13),MB,1_intkind1,wf(:,67))
  call vert_AV_Q(wf(:,12),wf(:,-4),wf(:,68))
  call counter_Q_A(ctbb,wf(:,11),Q(:,9),wf(:,69))
  call prop_A_Q(wf(:,68),Q(:,22),MB,1_intkind1,wf(:,70))
  call counter_A_Q(ctbb,wf(:,12),Q(:,6),wf(:,71))
  call prop_Q_A(wf(:,13),Q(:,25),MB,1_intkind1,wf(:,72))
  call vert_AZ_Q(gZd,wf(:,12),wf(:,-3),wf(:,73))
  call counter_Q_A(ctbb,wf(:,15),Q(:,17),wf(:,74))
  call prop_A_Q(wf(:,73),Q(:,14),MB,1_intkind1,wf(:,75))
  call prop_Q_A(wf(:,16),Q(:,25),MB,1_intkind1,wf(:,76))
  call vert_AZ_Q(gZd,wf(:,7),wf(:,-2),wf(:,77))
  call prop_A_Q(wf(:,77),Q(:,22),MB,1_intkind1,wf(:,78))
  call prop_Q_A(wf(:,17),Q(:,13),MB,1_intkind1,wf(:,79))
  call vert_AZ_Q(gZd,wf(:,4),wf(:,-2),wf(:,80))
  call prop_A_Q(wf(:,80),Q(:,14),MB,1_intkind1,wf(:,81))
  call prop_Q_A(wf(:,18),Q(:,21),MB,1_intkind1,wf(:,82))
  call vert_SA_Q(gH,wf(:,19),wf(:,-1),wf(:,83))
  call prop_A_Q(wf(:,83),Q(:,14),MB,1_intkind1,wf(:,84))
  call vert_QS_A(gH,wf(:,0),wf(:,19),wf(:,85))
  call prop_Q_A(wf(:,85),Q(:,13),MB,1_intkind1,wf(:,86))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5) - MB2)
  den(2) = 1 / (Q(5,10) - MB2)
  den(4) = 1 / (Q(5,18) - MB2)
  den(6) = 1 / (Q(5,9) - MB2)
  den(7) = 1 / (Q(5,6) - MB2)
  den(9) = 1 / (Q(5,17) - MB2)
  den(13) = 1 / (Q(5,12) - MH2)
  den(16) = 1 / (Q(5,3))
  den(18) = 1 / (Q(5,24))
  den(21) = 1 / (Q(5,20))
  den(24) = 1 / (Q(5,26) - MB2)
  den(27) = 1 / (Q(5,21) - MB2)
  den(32) = 1 / (Q(5,13) - MB2)
  den(35) = 1 / (Q(5,22) - MB2)
  den(38) = 1 / (Q(5,25) - MB2)
  den(41) = 1 / (Q(5,14) - MB2)
  den(58) = 1 / (Q(5,7))
  den(61) = 1 / (Q(5,11))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(8) = den(6)*den(7)
  den(10) = den(7)*den(9)
  den(11) = den(4)*den(6)
  den(12) = den(2)*den(9)
  den(14) = den(9)*den(13)
  den(15) = den(4)*den(13)
  den(17) = den(13)*den(16)
  den(19) = den(1)*den(18)
  den(20) = den(7)*den(18)
  den(22) = den(6)*den(21)
  den(23) = den(2)*den(21)
  den(25) = den(2)*den(24)
  den(26) = den(1)*den(25)
  den(28) = den(1)*den(27)
  den(29) = den(2)*den(28)
  den(30) = den(4)*den(24)
  den(31) = den(1)*den(30)
  den(33) = den(1)*den(32)
  den(34) = den(4)*den(33)
  den(36) = den(7)*den(35)
  den(37) = den(6)*den(36)
  den(39) = den(6)*den(38)
  den(40) = den(7)*den(39)
  den(42) = den(7)*den(41)
  den(43) = den(9)*den(42)
  den(44) = den(9)*den(38)
  den(45) = den(7)*den(44)
  den(46) = den(4)*den(35)
  den(47) = den(6)*den(46)
  den(48) = den(6)*den(32)
  den(49) = den(4)*den(48)
  den(50) = den(2)*den(41)
  den(51) = den(9)*den(50)
  den(52) = den(9)*den(27)
  den(53) = den(2)*den(52)
  den(54) = den(13)*den(41)
  den(55) = den(9)*den(54)
  den(56) = den(13)*den(32)
  den(57) = den(4)*den(56)
  den(59) = den(1)*den(58)
  den(60) = den(7)*den(58)
  den(62) = den(6)*den(61)
  den(63) = den(2)*den(61)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(50)

  A(1) = cont_QA(wf(:,4),wf(:,5)) * den(3)
  A(2) = cont_QA(wf(:,7),wf(:,8)) * den(5)
  A(3) = cont_QA(wf(:,12),wf(:,13)) * den(8)
  A(4) = cont_QA(wf(:,12),wf(:,16)) * den(10)
  A(5) = cont_QA(wf(:,7),wf(:,17)) * den(11)
  A(6) = cont_QA(wf(:,4),wf(:,18)) * den(12)
  A(7) = cont_SS(wf(:,19),wf(:,20)) * den(14)
  A(8) = cont_SS(wf(:,19),wf(:,21)) * den(15)

  A(9) = cont_VV(wf(:,22),wf(:,23)) * den(16)
  A(10) = cont_SS(wf(:,19),wf(:,24)) * den(17)
  A(11) = cont_QA(wf(:,4),wf(:,25)) * den(3)
  A(12) = cont_QA(wf(:,7),wf(:,26)) * den(5)
  A(13) = cont_VV(wf(:,27),wf(:,28)) * den(19)
  A(14) = cont_QA(wf(:,12),wf(:,29)) * den(8)
  A(15) = cont_QA(wf(:,12),wf(:,30)) * den(10)
  A(16) = cont_VV(wf(:,27),wf(:,31)) * den(20)
  A(17) = cont_QA(wf(:,7),wf(:,32)) * den(11)
  A(18) = cont_VV(wf(:,33),wf(:,34)) * den(22)
  A(19) = cont_QA(wf(:,4),wf(:,35)) * den(12)
  A(20) = cont_VV(wf(:,33),wf(:,36)) * den(23)
  A(21) = cont_QA(wf(:,8),wf(:,38)) * den(5)
  A(22) = cont_QA(wf(:,5),wf(:,40)) * den(3)
  A(23) = cont_QA(wf(:,17),wf(:,38)) * den(11)
  A(24) = cont_SS(wf(:,19),wf(:,41)) * den(14)
  A(25) = cont_SS(wf(:,19),wf(:,42)) * den(15)
  A(26) = cont_QA(wf(:,18),wf(:,40)) * den(12)
  A(27) = cont_QA(wf(:,13),wf(:,44)) * den(8)
  A(28) = cont_QA(wf(:,16),wf(:,44)) * den(10)
  A(29) = cont_QA(wf(:,12),wf(:,47)) * den(10)
  A(30) = cont_QA(wf(:,12),wf(:,50)) * den(8)
  A(31) = cont_QA(wf(:,4),wf(:,51)) * den(12)
  A(32) = cont_SS(wf(:,19),wf(:,52)) * den(15)
  A(33) = cont_SS(wf(:,19),wf(:,53)) * den(14)
  A(34) = cont_QA(wf(:,7),wf(:,54)) * den(11)
  A(35) = cont_QA(wf(:,4),wf(:,57)) * den(3)
  A(36) = cont_QA(wf(:,7),wf(:,58)) * den(5)
  A(37) = cont_QA(wf(:,60),wf(:,61)) * den(26)
  A(38) = cont_QA(wf(:,62),wf(:,63)) * den(29)
  A(39) = cont_QA(wf(:,60),wf(:,65)) * den(31)
  A(40) = cont_QA(wf(:,66),wf(:,67)) * den(34)
  A(41) = cont_QA(wf(:,69),wf(:,70)) * den(37)
  A(42) = cont_QA(wf(:,71),wf(:,72)) * den(40)
  A(43) = cont_QA(wf(:,74),wf(:,75)) * den(43)
  A(44) = cont_QA(wf(:,71),wf(:,76)) * den(45)
  A(45) = cont_QA(wf(:,69),wf(:,78)) * den(47)
  A(46) = cont_QA(wf(:,66),wf(:,79)) * den(49)
  A(47) = cont_QA(wf(:,74),wf(:,81)) * den(51)
  A(48) = cont_QA(wf(:,62),wf(:,82)) * den(53)
  A(49) = cont_QA(wf(:,74),wf(:,84)) * den(55)
  A(50) = cont_QA(wf(:,66),wf(:,86)) * den(57)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(50)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(1)+A(2)+A(3)+A(4)+A(5)+A(6))*f(1)+(-A(7)-A(8))*f(7)

  M2(1) = (-A(37)-A(38)-A(39)-A(40)-A(41)-A(42)-A(43)-A(44)-A(45)-A(46)-A(47)-A(48))*f(2)+(A(11)+A(14)+A(21)+A(23)+A(29) &
       +A(31))*f(3)+(A(12)+A(15)+A(17)+A(19)+A(22)+A(26)+A(27)+A(28)+A(30)+A(34)+A(35)+A(36))*f(4)+(-A(13)-A(16)-A(18)-A(20))*f(5) &
       +A(9)*f(6)+(A(49)+A(50))*f(8)+(-A(25)-A(33))*f(9)+(-A(24)-A(32))*f(10)-A(10)*f(11)

end subroutine colourvectors

end module ol_loop_ppzzj_bbxzzg_1_/**/REALKIND
