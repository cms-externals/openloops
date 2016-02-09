
module ol_colourmatrix_ppvvv_ddxzzz_1_/**/REALKIND
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
end module ol_colourmatrix_ppvvv_ddxzzz_1_/**/REALKIND



module ol_forced_parameters_ppvvv_ddxzzz_1_/**/REALKIND
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
  if (wMZ /= 0) write(*,101) 'wMZ = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppvvv_ddxzzz_1_/**/REALKIND

module ol_loop_ppvvv_ddxzzz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(7), c(2)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:85)
  ! denominators
  complex(REALKIND), save :: den(53)
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
    f(1) = CI*eQED**3
    f(2) = CI*countertermnorm*eQED**3*gQCD**2
    f(3) = CI*countertermnorm*ctVqq*eQED**3*gQCD**2
    f(4) = (CI*eQED**3*MW**2)/(cw**4*sw**2)
    f(5) = (CI*countertermnorm*ctVqq*eQED**3*gQCD**2*MW**2)/(cw**4*sw**2)
    f(6) = eQED**3*gQCD**2*integralnorm*SwB
    f(7) = (eQED**3*gQCD**2*integralnorm*MW**2*SwB)/(cw**4*sw**2)

  c = [ 4*f(6), 4*f(7) ]
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
  complex(REALKIND) :: A(42)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_V(P(:,3), rMZ, H(3), wf(:,-2))
  call wf_V(P(:,4), rMZ, H(4), wf(:,-3))
  call wf_V(P(:,5), rMZ, H(5), wf(:,-4))

  ! internal WFs
  call vert_QA_Z(gZd,wf(:,0),wf(:,-1),wf(:,1))
  call vert_VV_S(wf(:,-2),wf(:,-3),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,3))
  call vert_VV_S(wf(:,3),wf(:,-4),wf(:,4))
  call vert_VV_S(wf(:,-2),wf(:,-4),wf(:,5))
  call vert_VV_S(wf(:,3),wf(:,-3),wf(:,6))
  call vert_VV_S(wf(:,-3),wf(:,-4),wf(:,7))
  call vert_VV_S(wf(:,3),wf(:,-2),wf(:,8))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,0),wf(:,9))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-3),wf(:,10))
  call prop_Q_A(wf(:,9),Q(:,5),ZERO,0_intkind1,wf(:,11))
  call prop_A_Q(wf(:,10),Q(:,10),ZERO,0_intkind1,wf(:,12))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,11),wf(:,13))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-4),wf(:,14))
  call prop_A_Q(wf(:,14),Q(:,18),ZERO,0_intkind1,wf(:,15))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,11),wf(:,16))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,0),wf(:,17))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,-2),wf(:,18))
  call prop_Q_A(wf(:,17),Q(:,9),ZERO,0_intkind1,wf(:,19))
  call prop_A_Q(wf(:,18),Q(:,6),ZERO,0_intkind1,wf(:,20))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,19),wf(:,21))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,0),wf(:,22))
  call prop_Q_A(wf(:,22),Q(:,17),ZERO,0_intkind1,wf(:,23))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,23),wf(:,24))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,19),wf(:,25))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,23),wf(:,26))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,11),wf(:,27))
  call counter_ZQ_A(gZd,wf(:,-3),wf(:,11),wf(:,28))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,19),wf(:,29))
  call counter_ZQ_A(gZd,wf(:,-3),wf(:,23),wf(:,30))
  call counter_ZQ_A(gZd,wf(:,-2),wf(:,19),wf(:,31))
  call counter_ZQ_A(gZd,wf(:,-2),wf(:,23),wf(:,32))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,-4),wf(:,33))
  call prop_A_Q(wf(:,33),Q(:,18),ZERO,0_intkind1,wf(:,34))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,-3),wf(:,35))
  call prop_A_Q(wf(:,35),Q(:,10),ZERO,0_intkind1,wf(:,36))
  call counter_AZ_Q(gZd,wf(:,-1),wf(:,-2),wf(:,37))
  call prop_A_Q(wf(:,37),Q(:,6),ZERO,0_intkind1,wf(:,38))
  call counter_ZQ_A(gZd,wf(:,-4),wf(:,0),wf(:,39))
  call prop_Q_A(wf(:,39),Q(:,17),ZERO,0_intkind1,wf(:,40))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,40),wf(:,41))
  call counter_ZQ_A(gZd,wf(:,-3),wf(:,0),wf(:,42))
  call prop_Q_A(wf(:,42),Q(:,9),ZERO,0_intkind1,wf(:,43))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,43),wf(:,44))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,40),wf(:,45))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,43),wf(:,46))
  call counter_ZQ_A(gZd,wf(:,-2),wf(:,0),wf(:,47))
  call prop_Q_A(wf(:,47),Q(:,5),ZERO,0_intkind1,wf(:,48))
  call vert_ZQ_A(gZd,wf(:,-4),wf(:,48),wf(:,49))
  call vert_ZQ_A(gZd,wf(:,-3),wf(:,48),wf(:,50))
  call counter_QA_Z(gZd,wf(:,0),wf(:,-1),wf(:,51))
  call prop_W_W(wf(:,51),Q(:,3),MZ,1_intkind1,wf(:,52))
  call vert_VV_S(wf(:,52),wf(:,-4),wf(:,53))
  call vert_VV_S(wf(:,52),wf(:,-3),wf(:,54))
  call vert_VV_S(wf(:,52),wf(:,-2),wf(:,55))
  call vert_AZ_Q(gZd,wf(:,12),wf(:,-4),wf(:,56))
  call counter_Q_A(ctqq,wf(:,11),Q(:,5),wf(:,57))
  call prop_A_Q(wf(:,56),Q(:,26),ZERO,0_intkind1,wf(:,58))
  call counter_A_Q(ctqq,wf(:,12),Q(:,10),wf(:,59))
  call prop_Q_A(wf(:,13),Q(:,21),ZERO,0_intkind1,wf(:,60))
  call vert_AZ_Q(gZd,wf(:,15),wf(:,-3),wf(:,61))
  call prop_A_Q(wf(:,61),Q(:,26),ZERO,0_intkind1,wf(:,62))
  call counter_A_Q(ctqq,wf(:,15),Q(:,18),wf(:,63))
  call prop_Q_A(wf(:,16),Q(:,13),ZERO,0_intkind1,wf(:,64))
  call vert_AZ_Q(gZd,wf(:,20),wf(:,-4),wf(:,65))
  call counter_Q_A(ctqq,wf(:,19),Q(:,9),wf(:,66))
  call prop_A_Q(wf(:,65),Q(:,22),ZERO,0_intkind1,wf(:,67))
  call counter_A_Q(ctqq,wf(:,20),Q(:,6),wf(:,68))
  call prop_Q_A(wf(:,21),Q(:,25),ZERO,0_intkind1,wf(:,69))
  call vert_AZ_Q(gZd,wf(:,20),wf(:,-3),wf(:,70))
  call counter_Q_A(ctqq,wf(:,23),Q(:,17),wf(:,71))
  call prop_A_Q(wf(:,70),Q(:,14),ZERO,0_intkind1,wf(:,72))
  call prop_Q_A(wf(:,24),Q(:,25),ZERO,0_intkind1,wf(:,73))
  call vert_AZ_Q(gZd,wf(:,15),wf(:,-2),wf(:,74))
  call prop_A_Q(wf(:,74),Q(:,22),ZERO,0_intkind1,wf(:,75))
  call prop_Q_A(wf(:,25),Q(:,13),ZERO,0_intkind1,wf(:,76))
  call vert_AZ_Q(gZd,wf(:,12),wf(:,-2),wf(:,77))
  call prop_A_Q(wf(:,77),Q(:,14),ZERO,0_intkind1,wf(:,78))
  call prop_Q_A(wf(:,26),Q(:,21),ZERO,0_intkind1,wf(:,79))
  call vert_SV_V(wf(:,2),wf(:,-4),wf(:,80))
  call prop_W_W(wf(:,80),Q(:,28),MZ,1_intkind1,wf(:,81))
  call vert_SV_V(wf(:,5),wf(:,-3),wf(:,82))
  call prop_W_W(wf(:,82),Q(:,28),MZ,1_intkind1,wf(:,83))
  call vert_SV_V(wf(:,7),wf(:,-2),wf(:,84))
  call prop_W_W(wf(:,84),Q(:,28),MZ,1_intkind1,wf(:,85))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,3) - MZ2)
  den(2) = 1 / (Q(5,12) - MH2)
  den(4) = 1 / (Q(5,20) - MH2)
  den(6) = 1 / (Q(5,24) - MH2)
  den(8) = 1 / (Q(5,5))
  den(9) = 1 / (Q(5,10))
  den(11) = 1 / (Q(5,18))
  den(13) = 1 / (Q(5,9))
  den(14) = 1 / (Q(5,6))
  den(16) = 1 / (Q(5,17))
  den(20) = 1 / (Q(5,26))
  den(23) = 1 / (Q(5,21))
  den(28) = 1 / (Q(5,13))
  den(31) = 1 / (Q(5,22))
  den(34) = 1 / (Q(5,25))
  den(37) = 1 / (Q(5,14))
  den(50) = 1 / (Q(5,28) - MZ2)

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(1)*den(6)
  den(10) = den(8)*den(9)
  den(12) = den(8)*den(11)
  den(15) = den(13)*den(14)
  den(17) = den(14)*den(16)
  den(18) = den(11)*den(13)
  den(19) = den(9)*den(16)
  den(21) = den(9)*den(20)
  den(22) = den(8)*den(21)
  den(24) = den(8)*den(23)
  den(25) = den(9)*den(24)
  den(26) = den(11)*den(20)
  den(27) = den(8)*den(26)
  den(29) = den(8)*den(28)
  den(30) = den(11)*den(29)
  den(32) = den(14)*den(31)
  den(33) = den(13)*den(32)
  den(35) = den(13)*den(34)
  den(36) = den(14)*den(35)
  den(38) = den(14)*den(37)
  den(39) = den(16)*den(38)
  den(40) = den(16)*den(34)
  den(41) = den(14)*den(40)
  den(42) = den(11)*den(31)
  den(43) = den(13)*den(42)
  den(44) = den(13)*den(28)
  den(45) = den(11)*den(44)
  den(46) = den(9)*den(37)
  den(47) = den(16)*den(46)
  den(48) = den(16)*den(23)
  den(49) = den(9)*den(48)
  den(51) = den(2)*den(50)
  den(52) = den(4)*den(50)
  den(53) = den(6)*den(50)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(42)

  A(1) = cont_SS(wf(:,2),wf(:,4)) * den(3)
  A(2) = cont_SS(wf(:,5),wf(:,6)) * den(5)
  A(3) = cont_SS(wf(:,7),wf(:,8)) * den(7)
  A(4) = cont_QA(wf(:,12),wf(:,13)) * den(10)
  A(5) = cont_QA(wf(:,15),wf(:,16)) * den(12)
  A(6) = cont_QA(wf(:,20),wf(:,21)) * den(15)
  A(7) = cont_QA(wf(:,20),wf(:,24)) * den(17)
  A(8) = cont_QA(wf(:,15),wf(:,25)) * den(18)
  A(9) = cont_QA(wf(:,12),wf(:,26)) * den(19)

  A(10) = cont_QA(wf(:,12),wf(:,27)) * den(10)
  A(11) = cont_QA(wf(:,15),wf(:,28)) * den(12)
  A(12) = cont_QA(wf(:,20),wf(:,29)) * den(15)
  A(13) = cont_QA(wf(:,20),wf(:,30)) * den(17)
  A(14) = cont_QA(wf(:,15),wf(:,31)) * den(18)
  A(15) = cont_QA(wf(:,12),wf(:,32)) * den(19)
  A(16) = cont_QA(wf(:,16),wf(:,34)) * den(12)
  A(17) = cont_QA(wf(:,13),wf(:,36)) * den(10)
  A(18) = cont_QA(wf(:,25),wf(:,34)) * den(18)
  A(19) = cont_QA(wf(:,26),wf(:,36)) * den(19)
  A(20) = cont_QA(wf(:,21),wf(:,38)) * den(15)
  A(21) = cont_QA(wf(:,24),wf(:,38)) * den(17)
  A(22) = cont_QA(wf(:,20),wf(:,41)) * den(17)
  A(23) = cont_QA(wf(:,20),wf(:,44)) * den(15)
  A(24) = cont_QA(wf(:,12),wf(:,45)) * den(19)
  A(25) = cont_QA(wf(:,15),wf(:,46)) * den(18)
  A(26) = cont_QA(wf(:,12),wf(:,49)) * den(10)
  A(27) = cont_QA(wf(:,15),wf(:,50)) * den(12)
  A(28) = cont_SS(wf(:,2),wf(:,53)) * den(3)
  A(29) = cont_SS(wf(:,5),wf(:,54)) * den(5)
  A(30) = cont_SS(wf(:,7),wf(:,55)) * den(7)
  A(31) = cont_QA(wf(:,57),wf(:,58)) * den(22)
  A(32) = cont_QA(wf(:,59),wf(:,60)) * den(25)
  A(33) = cont_QA(wf(:,57),wf(:,62)) * den(27)
  A(34) = cont_QA(wf(:,63),wf(:,64)) * den(30)
  A(35) = cont_QA(wf(:,66),wf(:,67)) * den(33)
  A(36) = cont_QA(wf(:,68),wf(:,69)) * den(36)
  A(37) = cont_QA(wf(:,71),wf(:,72)) * den(39)
  A(38) = cont_QA(wf(:,68),wf(:,73)) * den(41)
  A(39) = cont_QA(wf(:,66),wf(:,75)) * den(43)
  A(40) = cont_QA(wf(:,63),wf(:,76)) * den(45)
  A(41) = cont_QA(wf(:,71),wf(:,78)) * den(47)
  A(42) = cont_QA(wf(:,59),wf(:,79)) * den(49)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(42)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(4)-A(5)-A(6)-A(7)-A(8)-A(9))*f(1)+(A(1)+A(2)+A(3))*f(4)

  M2(1) = (A(31)+A(32)+A(33)+A(34)+A(35)+A(36)+A(37)+A(38)+A(39)+A(40)+A(41)+A(42))*f(2)+(-A(10)-A(11)-A(12)-A(13)-A(14)-A(15) &
       -A(16)-A(17)-A(18)-A(19)-A(20)-A(21)-A(22)-A(23)-A(24)-A(25)-A(26)-A(27))*f(3)+(A(28)+A(29)+A(30))*f(5)

end subroutine colourvectors

end module ol_loop_ppvvv_ddxzzz_1_/**/REALKIND
