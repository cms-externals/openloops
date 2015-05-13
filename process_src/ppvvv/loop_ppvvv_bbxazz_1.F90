
module ol_colourmatrix_ppvvv_bbxazz_1_/**/REALKIND
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
end module ol_colourmatrix_ppvvv_bbxazz_1_/**/REALKIND



module ol_forced_parameters_ppvvv_bbxazz_1_/**/REALKIND
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
end module ol_forced_parameters_ppvvv_bbxazz_1_/**/REALKIND

module ol_loop_ppvvv_bbxazz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(9), c(2)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:77)
  ! denominators
  complex(REALKIND), save :: den(49)
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
    f(1) = (CI*eQED**3)/3._/**/REALKIND
    f(2) = (CI*countertermnorm*eQED**3*gQCD**2)/3._/**/REALKIND
    f(3) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**2)/3._/**/REALKIND
    f(4) = (CI*eQED**3*MB)/(6._/**/REALKIND*cw**2*sw**2)
    f(5) = (CI*countertermnorm*eQED**3*gQCD**2*MB)/(6._/**/REALKIND*cw**2*sw**2)
    f(6) = (CI*countertermnorm*ctSbb*eQED**3*gQCD**2*MB)/(6._/**/REALKIND*cw**2*sw**2)
    f(7) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**2*MB)/(6._/**/REALKIND*cw**2*sw**2)
    f(8) = (eQED**3*gQCD**2*integralnorm*SwB)/3._/**/REALKIND
    f(9) = (eQED**3*gQCD**2*integralnorm*MB*SwB)/(cw**2*sw**2*6._/**/REALKIND)

  c = [ 4*f(8), 4*f(9) ]
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
  complex(REALKIND) :: A(44)
  ! external WFs
  call wf_Q(P(:,1), rMB, H(1), wf(:,0))
  call wf_A(P(:,2), rMB, H(2), wf(:,-1))
  call wf_V(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_V(P(:,4), rMZ, H(4), wf(:,-3))
  call wf_V(P(:,5), rMZ, H(5), wf(:,-4))

  ! internal WFs
  call vert_VQ_A(wf(:,-2),wf(:,0),wf(:,1))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-3),wf(:,2))
  call prop_Q_A(wf(:,1),Q(:,5),MB,1_intkind1,wf(:,3))
  call prop_A_Q(wf(:,2),Q(:,10),MB,1_intkind1,wf(:,4))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,3),wf(:,5))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-4),wf(:,6))
  call prop_A_Q(wf(:,6),Q(:,18),MB,1_intkind1,wf(:,7))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,3),wf(:,8))
  call vert_VV_S(wf(:,-3),wf(:,-4),wf(:,9))
  call vert_AQ_S(gH,wf(:,-1),wf(:,3),wf(:,10))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,0),wf(:,11))
  call vert_AV_Q(wf(:,-1),wf(:,-2),wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,9),MB,1_intkind1,wf(:,13))
  call prop_A_Q(wf(:,12),Q(:,6),MB,1_intkind1,wf(:,14))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,13),wf(:,15))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,0),wf(:,16))
  call prop_Q_A(wf(:,16),Q(:,17),MB,1_intkind1,wf(:,17))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,17),wf(:,18))
  call vert_AQ_S(gH,wf(:,14),wf(:,0),wf(:,19))
  call vert_VQ_A(wf(:,-2),wf(:,13),wf(:,20))
  call vert_VQ_A(wf(:,-2),wf(:,17),wf(:,21))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,3),wf(:,22))
  call counter_ZQ_A(gZd,wf(:,-3),wf(:,3),wf(:,23))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,13),wf(:,24))
  call counter_ZQ_A(gZd,wf(:,-3),wf(:,17),wf(:,25))
  call counter_VQ_A(wf(:,-2),wf(:,13),wf(:,26))
  call counter_VQ_A(wf(:,-2),wf(:,17),wf(:,27))
  call counter_AQ_S(gH,wf(:,-1),wf(:,3),wf(:,28))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,-4),wf(:,29))
  call prop_A_Q(wf(:,29),Q(:,18),MB,1_intkind1,wf(:,30))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,-3),wf(:,31))
  call prop_A_Q(wf(:,31),Q(:,10),MB,1_intkind1,wf(:,32))
  call counter_AV_Q(wf(:,-1),wf(:,-2),wf(:,33))
  call prop_A_Q(wf(:,33),Q(:,6),MB,1_intkind1,wf(:,34))
  call vert_AQ_S(gH,wf(:,34),wf(:,0),wf(:,35))
  call counter_AQ_S(gH,wf(:,14),wf(:,0),wf(:,36))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,0),wf(:,37))
  call prop_Q_A(wf(:,37),Q(:,17),MB,1_intkind1,wf(:,38))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,38),wf(:,39))
  call counter_ZQ_A(gZd,wf(:,-3),wf(:,0),wf(:,40))
  call prop_Q_A(wf(:,40),Q(:,9),MB,1_intkind1,wf(:,41))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,41),wf(:,42))
  call vert_VQ_A(wf(:,-2),wf(:,38),wf(:,43))
  call vert_VQ_A(wf(:,-2),wf(:,41),wf(:,44))
  call counter_VQ_A(wf(:,-2),wf(:,0),wf(:,45))
  call prop_Q_A(wf(:,45),Q(:,5),MB,1_intkind1,wf(:,46))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,46),wf(:,47))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,46),wf(:,48))
  call vert_AQ_S(gH,wf(:,-1),wf(:,46),wf(:,49))
  call vert_AZ_Q(gZd,wf(:,4),wf(:,-4),wf(:,50))
  call counter_Q_A(ctbb,wf(:,3),Q(:,5),wf(:,51))
  call prop_A_Q(wf(:,50),Q(:,26),MB,1_intkind1,wf(:,52))
  call counter_A_Q(ctbb,wf(:,4),Q(:,10),wf(:,53))
  call prop_Q_A(wf(:,5),Q(:,21),MB,1_intkind1,wf(:,54))
  call vert_AZ_Q(gZd,wf(:,7),wf(:,-3),wf(:,55))
  call prop_A_Q(wf(:,55),Q(:,26),MB,1_intkind1,wf(:,56))
  call counter_A_Q(ctbb,wf(:,7),Q(:,18),wf(:,57))
  call prop_Q_A(wf(:,8),Q(:,13),MB,1_intkind1,wf(:,58))
  call vert_SA_Q(gH,wf(:,9),wf(:,-1),wf(:,59))
  call prop_A_Q(wf(:,59),Q(:,26),MB,1_intkind1,wf(:,60))
  call vert_AZ_Q(gZd,wf(:,14),wf(:,-4),wf(:,61))
  call counter_Q_A(ctbb,wf(:,13),Q(:,9),wf(:,62))
  call prop_A_Q(wf(:,61),Q(:,22),MB,1_intkind1,wf(:,63))
  call counter_A_Q(ctbb,wf(:,14),Q(:,6),wf(:,64))
  call prop_Q_A(wf(:,15),Q(:,25),MB,1_intkind1,wf(:,65))
  call vert_AZ_Q(gZd,wf(:,14),wf(:,-3),wf(:,66))
  call counter_Q_A(ctbb,wf(:,17),Q(:,17),wf(:,67))
  call prop_A_Q(wf(:,66),Q(:,14),MB,1_intkind1,wf(:,68))
  call prop_Q_A(wf(:,18),Q(:,25),MB,1_intkind1,wf(:,69))
  call vert_QS_A(gH,wf(:,0),wf(:,9),wf(:,70))
  call prop_Q_A(wf(:,70),Q(:,25),MB,1_intkind1,wf(:,71))
  call vert_AV_Q(wf(:,7),wf(:,-2),wf(:,72))
  call prop_A_Q(wf(:,72),Q(:,22),MB,1_intkind1,wf(:,73))
  call prop_Q_A(wf(:,20),Q(:,13),MB,1_intkind1,wf(:,74))
  call vert_AV_Q(wf(:,4),wf(:,-2),wf(:,75))
  call prop_A_Q(wf(:,75),Q(:,14),MB,1_intkind1,wf(:,76))
  call prop_Q_A(wf(:,21),Q(:,21),MB,1_intkind1,wf(:,77))

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
  den(6) = 1 / (Q(5,24) - MH2)
  den(8) = 1 / (Q(5,9) - MB2)
  den(9) = 1 / (Q(5,6) - MB2)
  den(11) = 1 / (Q(5,17) - MB2)
  den(16) = 1 / (Q(5,26) - MB2)
  den(19) = 1 / (Q(5,21) - MB2)
  den(24) = 1 / (Q(5,13) - MB2)
  den(29) = 1 / (Q(5,22) - MB2)
  den(32) = 1 / (Q(5,25) - MB2)
  den(35) = 1 / (Q(5,14) - MB2)

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
  den(30) = den(9)*den(29)
  den(31) = den(8)*den(30)
  den(33) = den(8)*den(32)
  den(34) = den(9)*den(33)
  den(36) = den(9)*den(35)
  den(37) = den(11)*den(36)
  den(38) = den(11)*den(32)
  den(39) = den(9)*den(38)
  den(40) = den(6)*den(32)
  den(41) = den(9)*den(40)
  den(42) = den(4)*den(29)
  den(43) = den(8)*den(42)
  den(44) = den(8)*den(24)
  den(45) = den(4)*den(44)
  den(46) = den(2)*den(35)
  den(47) = den(11)*den(46)
  den(48) = den(11)*den(19)
  den(49) = den(2)*den(48)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(44)

  A(1) = cont_QA(wf(:,4),wf(:,5)) * den(3)
  A(2) = cont_QA(wf(:,7),wf(:,8)) * den(5)
  A(3) = cont_SS(wf(:,9),wf(:,10)) * den(7)
  A(4) = cont_QA(wf(:,14),wf(:,15)) * den(10)
  A(5) = cont_QA(wf(:,14),wf(:,18)) * den(12)
  A(6) = cont_SS(wf(:,9),wf(:,19)) * den(13)
  A(7) = cont_QA(wf(:,7),wf(:,20)) * den(14)
  A(8) = cont_QA(wf(:,4),wf(:,21)) * den(15)

  A(9) = cont_QA(wf(:,4),wf(:,22)) * den(3)
  A(10) = cont_QA(wf(:,7),wf(:,23)) * den(5)
  A(11) = cont_QA(wf(:,14),wf(:,24)) * den(10)
  A(12) = cont_QA(wf(:,14),wf(:,25)) * den(12)
  A(13) = cont_QA(wf(:,7),wf(:,26)) * den(14)
  A(14) = cont_QA(wf(:,4),wf(:,27)) * den(15)
  A(15) = cont_SS(wf(:,9),wf(:,28)) * den(7)
  A(16) = cont_QA(wf(:,8),wf(:,30)) * den(5)
  A(17) = cont_QA(wf(:,5),wf(:,32)) * den(3)
  A(18) = cont_QA(wf(:,20),wf(:,30)) * den(14)
  A(19) = cont_QA(wf(:,21),wf(:,32)) * den(15)
  A(20) = cont_QA(wf(:,15),wf(:,34)) * den(10)
  A(21) = cont_QA(wf(:,18),wf(:,34)) * den(12)
  A(22) = cont_SS(wf(:,9),wf(:,35)) * den(13)
  A(23) = cont_SS(wf(:,9),wf(:,36)) * den(13)
  A(24) = cont_QA(wf(:,14),wf(:,39)) * den(12)
  A(25) = cont_QA(wf(:,14),wf(:,42)) * den(10)
  A(26) = cont_QA(wf(:,4),wf(:,43)) * den(15)
  A(27) = cont_QA(wf(:,7),wf(:,44)) * den(14)
  A(28) = cont_QA(wf(:,4),wf(:,47)) * den(3)
  A(29) = cont_QA(wf(:,7),wf(:,48)) * den(5)
  A(30) = cont_SS(wf(:,9),wf(:,49)) * den(7)
  A(31) = cont_QA(wf(:,51),wf(:,52)) * den(18)
  A(32) = cont_QA(wf(:,53),wf(:,54)) * den(21)
  A(33) = cont_QA(wf(:,51),wf(:,56)) * den(23)
  A(34) = cont_QA(wf(:,57),wf(:,58)) * den(26)
  A(35) = cont_QA(wf(:,51),wf(:,60)) * den(28)
  A(36) = cont_QA(wf(:,62),wf(:,63)) * den(31)
  A(37) = cont_QA(wf(:,64),wf(:,65)) * den(34)
  A(38) = cont_QA(wf(:,67),wf(:,68)) * den(37)
  A(39) = cont_QA(wf(:,64),wf(:,69)) * den(39)
  A(40) = cont_QA(wf(:,64),wf(:,71)) * den(41)
  A(41) = cont_QA(wf(:,62),wf(:,73)) * den(43)
  A(42) = cont_QA(wf(:,57),wf(:,74)) * den(45)
  A(43) = cont_QA(wf(:,67),wf(:,76)) * den(47)
  A(44) = cont_QA(wf(:,53),wf(:,77)) * den(49)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(44)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(1)-A(2)-A(4)-A(5)-A(7)-A(8))*f(1)+(A(3)+A(6))*f(4)

  M2(1) = (A(31)+A(32)+A(33)+A(34)+A(36)+A(37)+A(38)+A(39)+A(41)+A(42)+A(43)+A(44))*f(2)+(-A(9)-A(10)-A(11)-A(12)-A(13)-A(14) &
       -A(16)-A(17)-A(18)-A(19)-A(20)-A(21)-A(24)-A(25)-A(26)-A(27)-A(28)-A(29))*f(3)+(-A(35)-A(40))*f(5)+(A(15)+A(23))*f(6) &
       +(A(22)+A(30))*f(7)

end subroutine colourvectors

end module ol_loop_ppvvv_bbxazz_1_/**/REALKIND
