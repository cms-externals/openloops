
module ol_colourmatrix_pphtt_ttxbbxh_1_/**/REALKIND
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
  K1( 5,:) = [   0,  -4]
  K1( 6,:) = [  -4, -12]
  K1( 7,:) = [  12,   4]
  K1( 8,:) = [   4,  12]
  K1( 9,:) = [   0,   4]
  K1(10,:) = [   4,   0]
  K1(11,:) = [ -12,  -4]
  K1(12,:) = [  -4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [ -12,  -4]
  K1(16,:) = [  -4,   0]
  K1(17,:) = [   0,   4]
  K1(18,:) = [   4,   0]
  K1(19,:) = [   0,  -4]
  K1(20,:) = [  -4, -12]
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
end module ol_colourmatrix_pphtt_ttxbbxh_1_/**/REALKIND



module ol_forced_parameters_pphtt_ttxbbxh_1_/**/REALKIND
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
  if (YU /= 0) write(*,101) 'YU = 0'
  if (YD /= 0) write(*,101) 'YD = 0'
  if (YS /= 0) write(*,101) 'YS = 0'
  if (YC /= 0) write(*,101) 'YC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphtt_ttxbbxh_1_/**/REALKIND

module ol_loop_pphtt_ttxbbxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(22), c(34)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:47)
  ! denominators
  complex(REALKIND), save :: den(33)
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
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*countertermnorm*ctHGG*eQED*gQCD**4*MB*YB)/MQ2sum
    f( 2) = (CI*eQED*gQCD**2*YB)/(2._/**/REALKIND*MW*sw)
    f( 3) = (CI*countertermnorm*eQED*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f( 4) = (CI*countertermnorm*ctGbb*eQED*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f( 5) = (CI*countertermnorm*ctGtt*eQED*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f( 6) = (CI*countertermnorm*ctSbb*eQED*gQCD**4*YB)/(2._/**/REALKIND*MW*sw)
    f( 7) = (CI*eQED*gQCD**4*integralnorm*SwB*YB)/(2._/**/REALKIND*MW*sw)
    f( 8) = (eQED*gQCD**4*integralnorm*SwB*YB)/(MW*sw*4._/**/REALKIND)
    f( 9) = (eQED*gQCD**4*integralnorm*SwB*YB)/(MW*sw*2._/**/REALKIND)
    f(10) = (eQED*gQCD**4*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(11) = (eQED*gQCD**4*integralnorm*SwF*YB)/(MW*sw)
    f(12) = (CI*countertermnorm*ctHGG*eQED*gQCD**4*MT*YT)/MQ2sum
    f(13) = (CI*eQED*gQCD**2*YT)/(2._/**/REALKIND*MW*sw)
    f(14) = (CI*countertermnorm*eQED*gQCD**4*YT)/(2._/**/REALKIND*MW*sw)
    f(15) = (CI*countertermnorm*ctGbb*eQED*gQCD**4*YT)/(2._/**/REALKIND*MW*sw)
    f(16) = (CI*countertermnorm*ctGtt*eQED*gQCD**4*YT)/(2._/**/REALKIND*MW*sw)
    f(17) = (CI*countertermnorm*ctStt*eQED*gQCD**4*YT)/(2._/**/REALKIND*MW*sw)
    f(18) = (CI*eQED*gQCD**4*integralnorm*SwB*YT)/(2._/**/REALKIND*MW*sw)
    f(19) = (eQED*gQCD**4*integralnorm*SwB*YT)/(MW*sw*4._/**/REALKIND)
    f(20) = (eQED*gQCD**4*integralnorm*SwB*YT)/(MW*sw*2._/**/REALKIND)
    f(21) = (eQED*gQCD**4*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)
    f(22) = (eQED*gQCD**4*integralnorm*SwF*YT)/(MW*sw)

  c = [ 9*CI*f(7), 27*CI*f(7), 18*f(8), 54*f(8), f(9), 3*f(9), 6*f(9), 8*f(9), 10*f(9), 18*f(9), 21*f(9), 24*f(9), 54*f(9) &
    , 3*f(10), 9*f(10), 3*f(11), 9*f(11), 9*CI*f(18), 27*CI*f(18), 18*f(19), 54*f(19), f(20), 3*f(20), 6*f(20), 8*f(20), 10*f(20) &
    , 18*f(20), 21*f(20), 24*f(20), 54*f(20), 3*f(21), 9*f(21), 3*f(22), 9*f(22) ]
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
  complex(REALKIND) :: A(26)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rMT, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rMT, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rMB, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rMB, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_Q(P(:,1), rMT, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rMT, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rMB, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rMB, H(4), wf(:,-3), 0)
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QS_A(gH,wf(:,-2),wf(:,-4),wf(:,2))
  call prop_Q_A(wf(:,2),Q(:,20),MB,1_intkind1,wf(:,3))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,4))
  call vert_SA_Q(gH,wf(:,-4),wf(:,-3),wf(:,5))
  call prop_A_Q(wf(:,5),Q(:,24),MB,1_intkind1,wf(:,6))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,7))
  call vert_QS_A(gH,wf(:,0),wf(:,-4),wf(:,8))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,9))
  call prop_Q_A(wf(:,8),Q(:,17),MT,1_intkind1,wf(:,10))
  call vert_QA_V(wf(:,10),wf(:,-1),wf(:,11))
  call vert_SA_Q(gH,wf(:,-4),wf(:,-1),wf(:,12))
  call prop_A_Q(wf(:,12),Q(:,18),MT,1_intkind1,wf(:,13))
  call vert_QA_V(wf(:,0),wf(:,13),wf(:,14))
  call counter_SG_G(wf(:,-4),wf(:,1),wf(:,15))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,16))
  call counter_SA_Q(gH,wf(:,-4),wf(:,-3),wf(:,17))
  call prop_A_Q(wf(:,17),Q(:,24),MB,1_intkind1,wf(:,18))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,19))
  call counter_QS_A(gH,wf(:,-2),wf(:,-4),wf(:,20))
  call prop_Q_A(wf(:,20),Q(:,20),MB,1_intkind1,wf(:,21))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,22))
  call counter_QA_V(wf(:,10),wf(:,-1),wf(:,23))
  call counter_SA_Q(gH,wf(:,-4),wf(:,-1),wf(:,24))
  call prop_A_Q(wf(:,24),Q(:,18),MT,1_intkind1,wf(:,25))
  call vert_QA_V(wf(:,0),wf(:,25),wf(:,26))
  call counter_QA_V(wf(:,0),wf(:,13),wf(:,27))
  call counter_QS_A(gH,wf(:,0),wf(:,-4),wf(:,28))
  call prop_Q_A(wf(:,28),Q(:,17),MT,1_intkind1,wf(:,29))
  call vert_QA_V(wf(:,29),wf(:,-1),wf(:,30))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,31))
  call vert_AV_Q(wf(:,-3),wf(:,31),wf(:,32))
  call vert_VQ_A(wf(:,31),wf(:,-2),wf(:,33))
  call vert_QA_V(wf(:,3),wf(:,-3),wf(:,34))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,35))
  call counter_Q_A(ctbb,wf(:,3),Q(:,20),wf(:,36))
  call prop_A_Q(wf(:,4),Q(:,11),MB,1_intkind1,wf(:,37))
  call vert_QA_V(wf(:,-2),wf(:,6),wf(:,38))
  call counter_A_Q(ctbb,wf(:,6),Q(:,24),wf(:,39))
  call prop_Q_A(wf(:,7),Q(:,7),MB,1_intkind1,wf(:,40))
  call vert_AV_Q(wf(:,-1),wf(:,9),wf(:,41))
  call counter_Q_A(cttt,wf(:,10),Q(:,17),wf(:,42))
  call prop_A_Q(wf(:,41),Q(:,14),MT,1_intkind1,wf(:,43))
  call counter_V_V(ctGG,wf(:,9),Q(:,12),wf(:,44))
  call vert_VQ_A(wf(:,9),wf(:,0),wf(:,45))
  call counter_A_Q(cttt,wf(:,13),Q(:,18),wf(:,46))
  call prop_Q_A(wf(:,45),Q(:,13),MT,1_intkind1,wf(:,47))

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
  den(2) = 1 / (Q(5,20) - MB2)
  den(4) = 1 / (Q(5,24) - MB2)
  den(6) = 1 / (Q(5,17) - MT2)
  den(7) = 1 / (Q(5,12))
  den(9) = 1 / (Q(5,18) - MT2)
  den(12) = 1 / (Q(5,28))
  den(15) = 1 / (Q(5,11) - MB2)
  den(20) = 1 / (Q(5,7) - MB2)
  den(23) = 1 / (Q(5,14) - MT2)
  den(26) = 1 / (Q(5,19))
  den(29) = 1 / (Q(5,13) - MT2)

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(8) = den(6)*den(7)
  den(10) = den(7)*den(9)
  den(11) = den(1)*den(7)
  den(13) = den(2)*den(12)
  den(14) = den(1)*den(13)
  den(16) = den(1)*den(15)
  den(17) = den(2)*den(16)
  den(18) = den(4)*den(12)
  den(19) = den(1)*den(18)
  den(21) = den(1)*den(20)
  den(22) = den(4)*den(21)
  den(24) = den(7)*den(23)
  den(25) = den(6)*den(24)
  den(27) = den(6)*den(26)
  den(28) = den(7)*den(27)
  den(30) = den(7)*den(29)
  den(31) = den(9)*den(30)
  den(32) = den(9)*den(26)
  den(33) = den(7)*den(32)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(26)

  A(1) = cont_QA(wf(:,3),wf(:,4)) * den(3)
  A(2) = cont_QA(wf(:,6),wf(:,7)) * den(5)
  A(3) = cont_VV(wf(:,9),wf(:,11)) * den(8)
  A(4) = cont_VV(wf(:,9),wf(:,14)) * den(10)

  A(5) = cont_VV(wf(:,9),wf(:,15)) * den(11)
  A(6) = cont_VV(wf(:,9),wf(:,15)) * den(11)
  A(7) = cont_QA(wf(:,3),wf(:,16)) * den(3)
  A(8) = cont_QA(wf(:,7),wf(:,18)) * den(5)
  A(9) = cont_QA(wf(:,6),wf(:,19)) * den(5)
  A(10) = cont_QA(wf(:,4),wf(:,21)) * den(3)
  A(11) = cont_VV(wf(:,11),wf(:,22)) * den(8)
  A(12) = cont_VV(wf(:,14),wf(:,22)) * den(10)
  A(13) = cont_VV(wf(:,9),wf(:,23)) * den(8)
  A(14) = cont_VV(wf(:,9),wf(:,26)) * den(10)
  A(15) = cont_VV(wf(:,9),wf(:,27)) * den(10)
  A(16) = cont_VV(wf(:,9),wf(:,30)) * den(8)
  A(17) = cont_QA(wf(:,3),wf(:,32)) * den(3)
  A(18) = cont_QA(wf(:,6),wf(:,33)) * den(5)
  A(19) = cont_VV(wf(:,34),wf(:,35)) * den(14)
  A(20) = cont_QA(wf(:,36),wf(:,37)) * den(17)
  A(21) = cont_VV(wf(:,35),wf(:,38)) * den(19)
  A(22) = cont_QA(wf(:,39),wf(:,40)) * den(22)
  A(23) = cont_QA(wf(:,42),wf(:,43)) * den(25)
  A(24) = cont_VV(wf(:,11),wf(:,44)) * den(28)
  A(25) = cont_QA(wf(:,46),wf(:,47)) * den(31)
  A(26) = cont_VV(wf(:,14),wf(:,44)) * den(33)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(26)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(1)+A(2))*f(2))/2._/**/REALKIND+((A(3)+A(4))*f(13))/2._/**/REALKIND
  M1(2) = ((-A(1)-A(2))*f(2))/6._/**/REALKIND+((-A(3)-A(4))*f(13))/6._/**/REALKIND

  M2(1) = (A(6)*f(1))/2._/**/REALKIND+((-A(19)-A(20)-A(21)-A(22))*f(3))/2._/**/REALKIND+((A(7)+A(9))*f(4))/2._/**/REALKIND+((A(17) &
       +A(18))*f(5))/2._/**/REALKIND+((A(8)+A(10))*f(6))/2._/**/REALKIND+(A(5)*f(12))/2._/**/REALKIND+((-A(23)-A(24)-A(25) &
       -A(26))*f(14))/2._/**/REALKIND+((A(11)+A(12))*f(15))/2._/**/REALKIND+((A(13)+A(15))*f(16))/2._/**/REALKIND+((A(14) &
       +A(16))*f(17))/2._/**/REALKIND
  M2(2) = -(A(6)*f(1))/6._/**/REALKIND+((A(19)+A(20)+A(21)+A(22))*f(3))/6._/**/REALKIND+((-A(7)-A(9))*f(4))/6._/**/REALKIND+(( &
       -A(17)-A(18))*f(5))/6._/**/REALKIND+((-A(8)-A(10))*f(6))/6._/**/REALKIND-(A(5)*f(12))/6._/**/REALKIND+((A(23)+A(24)+A(25) &
       +A(26))*f(14))/6._/**/REALKIND+((-A(11)-A(12))*f(15))/6._/**/REALKIND+((-A(13)-A(15))*f(16))/6._/**/REALKIND+((-A(14) &
       -A(16))*f(17))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pphtt_ttxbbxh_1_/**/REALKIND
