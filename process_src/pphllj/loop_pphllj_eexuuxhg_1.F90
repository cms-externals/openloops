
module ol_colourmatrix_pphllj_eexuuxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(23,1), K2(1,1), KL(1,1)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  12]
  K1( 2,:) = [   0]
  K1( 3,:) = [   0]
  K1( 4,:) = [   0]
  K1( 5,:) = [   0]
  K1( 6,:) = [   0]
  K1( 7,:) = [  16]
  K1( 8,:) = [   0]
  K1( 9,:) = [   0]
  K1(10,:) = [   2]
  K1(11,:) = [  16]
  K1(12,:) = [   0]
  K1(13,:) = [   0]
  K1(14,:) = [   0]
  K1(15,:) = [   0]
  K1(16,:) = [   0]
  K1(17,:) = [   0]
  K1(18,:) = [   0]
  K1(19,:) = [ -18]
  K1(20,:) = [ -18]
  K1(21,:) = [   0]
  K1(22,:) = [  36]
  K1(23,:) = [   0]
  K1 = (1._/**/REALKIND / 3) * K1

  K2(1,:) = [ 4]

  KL(1,:) = [ 4]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphllj_eexuuxhg_1_/**/REALKIND



module ol_forced_parameters_pphllj_eexuuxhg_1_/**/REALKIND
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
end module ol_forced_parameters_pphllj_eexuuxhg_1_/**/REALKIND

module ol_loop_pphllj_eexuuxhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(18), c(10)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:51)
  ! denominators
  complex(REALKIND), save :: den(45)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,32)
  ! zero helicity identifier
  logical,           save :: zerohel(32) = .true., zerohel_ct(32) = .true.

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
    f( 1) = (CI*eQED**3*gQCD*lambdaHZZ*MW)/(cw**2*sw)
    f( 2) = (CI*countertermnorm*eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f( 3) = (CI*countertermnorm*ctGqq*eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f( 4) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f( 5) = (countertermnorm*ctZGG*eQED**3*gQCD**3*lambdaHZZ*MW)/(cw**2*sw)
    f( 6) = (CI*eQED**3*gQCD**3*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f( 7) = (eQED**3*gQCD**3*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f( 8) = (eQED**3*gQCD**3*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f( 9) = (2*eQED**3*gQCD**3*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(10) = (2*CI*countertermnorm*ctHGG*eQED**3*gQCD**3*MB*YB)/(3._/**/REALKIND*MQ2sum)
    f(11) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**3*MB*YB)/MQ2sum
    f(12) = (eQED**3*gQCD**3*integralnorm*SwF*YB)/(MW*sw*6._/**/REALKIND)
    f(13) = (eQED**3*gQCD**3*integralnorm*SwF*YB)/(MW*sw*3._/**/REALKIND)
    f(14) = (eQED**3*gQCD**3*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(15) = (2*CI*countertermnorm*ctHGG*eQED**3*gQCD**3*MT*YT)/(3._/**/REALKIND*MQ2sum)
    f(16) = (CI*countertermnorm*ctHGG*eQED**3*gQCD**3*MT*YT)/MQ2sum
    f(17) = (eQED**3*gQCD**3*integralnorm*SwF*YT)/(MW*sw*3._/**/REALKIND)
    f(18) = (eQED**3*gQCD**3*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

  c = [ 9*CI*f(6), f(7), 8*f(7), 3*f(8), 3*f(9), 3*f(12), 3*f(13), 3*f(14), 3*f(17), 3*f(18) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(17)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_S(P(:,5), rMH, H(5), wf(:,-4))
  call wf_V(P(:,6), rZERO, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,1))
  call vert_VQ_A(wf(:,-5),wf(:,-2),wf(:,2))
  call prop_Q_A(wf(:,2),Q(:,36),ZERO,0_intkind1,wf(:,3))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,4))
  call vert_QA_Z(gZu,wf(:,3),wf(:,-3),wf(:,5))
  call vert_SV_V(wf(:,-4),wf(:,4),wf(:,6))
  call prop_W_W(wf(:,5),Q(:,44),MZ,1_intkind1,wf(:,7))
  call vert_AV_Q(wf(:,-3),wf(:,-5),wf(:,8))
  call prop_A_Q(wf(:,8),Q(:,40),ZERO,0_intkind1,wf(:,9))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,9),wf(:,10))
  call prop_W_W(wf(:,10),Q(:,44),MZ,1_intkind1,wf(:,11))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,12))
  call counter_GG_V(wf(:,12),Q(:,12),wf(:,-5),Q(:,32),wf(:,13))
  call prop_W_W(wf(:,6),Q(:,19),MZ,1_intkind1,wf(:,14))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,15))
  call counter_SG_G(wf(:,-4),wf(:,-5),wf(:,16))
  call vert_VQ_A(wf(:,15),wf(:,-2),wf(:,17))
  call vert_AV_Q(wf(:,-3),wf(:,16),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,7),ZERO,0_intkind1,wf(:,19))
  call vert_ZQ_A(gZu,wf(:,4),wf(:,-2),wf(:,20))
  call prop_Q_A(wf(:,20),Q(:,7),ZERO,0_intkind1,wf(:,21))
  call vert_VQ_A(wf(:,16),wf(:,-2),wf(:,22))
  call vert_AV_Q(wf(:,-3),wf(:,15),wf(:,23))
  call prop_Q_A(wf(:,22),Q(:,52),ZERO,0_intkind1,wf(:,24))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,4),wf(:,25))
  call counter_QA_Z(gZu,wf(:,3),wf(:,-3),wf(:,26))
  call counter_AV_Q(wf(:,-3),wf(:,-5),wf(:,27))
  call prop_A_Q(wf(:,27),Q(:,40),ZERO,0_intkind1,wf(:,28))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,28),wf(:,29))
  call prop_W_W(wf(:,29),Q(:,44),MZ,1_intkind1,wf(:,30))
  call counter_QA_Z(gZu,wf(:,-2),wf(:,9),wf(:,31))
  call counter_VQ_A(wf(:,-5),wf(:,-2),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,36),ZERO,0_intkind1,wf(:,33))
  call vert_QA_Z(gZu,wf(:,33),wf(:,-3),wf(:,34))
  call prop_W_W(wf(:,34),Q(:,44),MZ,1_intkind1,wf(:,35))
  call counter_Q_A(ctqq,wf(:,3),Q(:,36),wf(:,36))
  call prop_Q_A(wf(:,36),Q(:,36),ZERO,0_intkind1,wf(:,37))
  call vert_QA_Z(gZu,wf(:,37),wf(:,-3),wf(:,38))
  call counter_A_Q(ctqq,wf(:,9),Q(:,40),wf(:,39))
  call prop_A_Q(wf(:,39),Q(:,40),ZERO,0_intkind1,wf(:,40))
  call vert_QA_Z(gZu,wf(:,-2),wf(:,40),wf(:,41))
  call vert_QA_V(wf(:,19),wf(:,-3),wf(:,42))
  call vert_QA_V(wf(:,21),wf(:,-3),wf(:,43))
  call prop_A_Q(wf(:,23),Q(:,11),ZERO,0_intkind1,wf(:,44))
  call vert_QA_V(wf(:,-2),wf(:,44),wf(:,45))
  call prop_A_Q(wf(:,25),Q(:,11),ZERO,0_intkind1,wf(:,46))
  call vert_QA_V(wf(:,-2),wf(:,46),wf(:,47))
  call vert_ZQ_A(gZu,wf(:,14),wf(:,-2),wf(:,48))
  call prop_Q_A(wf(:,48),Q(:,23),ZERO,0_intkind1,wf(:,49))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,14),wf(:,50))
  call prop_A_Q(wf(:,50),Q(:,27),ZERO,0_intkind1,wf(:,51))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,36))
  den(2) = 1 / (Q(5,3) - MZ2)
  den(3) = 1 / (Q(5,44) - MZ2)
  den(6) = 1 / (Q(5,40))
  den(9) = 1 / (Q(5,12))
  den(10) = 1 / (Q(5,19) - MZ2)
  den(13) = 1 / (Q(5,3))
  den(14) = 1 / (Q(5,48))
  den(15) = 1 / (Q(5,7))
  den(20) = 1 / (Q(5,52))
  den(30) = 1 / (Q(5,15))
  den(33) = 1 / (Q(5,11))
  den(38) = 1 / (Q(5,23))
  den(40) = 1 / (Q(5,27))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(3)*den(6)
  den(8) = den(2)*den(7)
  den(11) = den(2)*den(10)
  den(12) = den(9)*den(11)
  den(16) = den(13)*den(15)
  den(17) = den(14)*den(16)
  den(18) = den(2)*den(15)
  den(19) = den(14)*den(18)
  den(21) = den(14)*den(20)
  den(22) = den(13)*den(21)
  den(23) = den(2)*den(21)
  den(24) = den(1)*den(11)
  den(25) = den(6)*den(11)
  den(26) = den(1)**2
  den(27) = den(11)*den(26)
  den(28) = den(6)**2
  den(29) = den(11)*den(28)
  den(31) = den(16)*den(30)
  den(32) = den(18)*den(30)
  den(34) = den(13)*den(33)
  den(35) = den(30)*den(34)
  den(36) = den(2)*den(33)
  den(37) = den(30)*den(36)
  den(39) = den(11)*den(38)
  den(41) = den(11)*den(40)
  den(42) = den(9)*den(13)
  den(43) = den(2)*den(9)
  den(44) = den(1)*den(41)
  den(45) = den(6)*den(39)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(17)

  A(1) = cont_VV(wf(:,6),wf(:,7)) * den(5)
  A(2) = cont_VV(wf(:,6),wf(:,11)) * den(8)

  A(3) = cont_VV(wf(:,13),wf(:,14)) * den(12)
  A(4) = cont_QA(wf(:,18),wf(:,19)) * den(17)
  A(5) = cont_QA(wf(:,18),wf(:,19)) * den(17)
  A(6) = cont_QA(wf(:,18),wf(:,21)) * den(19)
  A(7) = cont_QA(wf(:,18),wf(:,21)) * den(19)
  A(8) = cont_QA(wf(:,23),wf(:,24)) * den(22)
  A(9) = cont_QA(wf(:,23),wf(:,24)) * den(22)
  A(10) = cont_QA(wf(:,24),wf(:,25)) * den(23)
  A(11) = cont_QA(wf(:,24),wf(:,25)) * den(23)
  A(12) = cont_VV(wf(:,14),wf(:,26)) * den(24)
  A(13) = cont_VV(wf(:,6),wf(:,30)) * den(8)
  A(14) = cont_VV(wf(:,14),wf(:,31)) * den(25)
  A(15) = cont_VV(wf(:,6),wf(:,35)) * den(5)
  A(16) = cont_VV(wf(:,14),wf(:,38)) * den(27)
  A(17) = cont_VV(wf(:,14),wf(:,41)) * den(29)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(17)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(1)+A(2))*f(1)

  M2(1) = (-A(16)-A(17))*f(2)+(A(13)+A(15))*f(3)+(A(12)+A(14))*f(4)-A(3)*f(5)+(-A(5)-A(9))*f(10)+(A(7)+A(11))*f(11)+(-A(4) &
       -A(8))*f(15)+(A(6)+A(10))*f(16)

end subroutine colourvectors

end module ol_loop_pphllj_eexuuxhg_1_/**/REALKIND
