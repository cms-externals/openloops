
module ol_colourmatrix_ppzwj_uxdzwxg_1_/**/REALKIND
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
end module ol_colourmatrix_ppzwj_uxdzwxg_1_/**/REALKIND



module ol_forced_parameters_ppzwj_uxdzwxg_1_/**/REALKIND
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


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppzwj_uxdzwxg_1_/**/REALKIND

module ol_loop_ppzwj_uxdzwxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(15), c(8)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:81)
  ! denominators
  complex(REALKIND), save :: den(55)
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
    f( 1) = (CI*cw*eQED**2*gQCD)/(sqrt2*sw**2)
    f( 2) = (CI*countertermnorm*cw*eQED**2*gQCD**3)/(sqrt2*sw**2)
    f( 3) = (CI*countertermnorm*ctGqq*cw*eQED**2*gQCD**3)/(sqrt2*sw**2)
    f( 4) = (CI*countertermnorm*ctVqq*cw*eQED**2*gQCD**3)/(sqrt2*sw**2)
    f( 5) = (CI*eQED**2*gQCD)/(sqrt2*sw)
    f( 6) = (CI*countertermnorm*eQED**2*gQCD**3)/(sqrt2*sw)
    f( 7) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**3)/(sqrt2*sw)
    f( 8) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**3)/(sqrt2*sw)
    f( 9) = (countertermnorm*ctZGG*eQED**2*gQCD**3)/(sqrt2*sw)
    f(10) = (CI*cw*eQED**2*gQCD**3*integralnorm*SwB)/(sqrt2*sw**2)
    f(11) = (cw*eQED**2*gQCD**3*integralnorm*SwB)/(sqrt2*sw**2)
    f(12) = (CI*eQED**2*gQCD**3*integralnorm*SwB)/(sqrt2*sw)
    f(13) = (eQED**2*gQCD**3*integralnorm*SwB)/(sqrt2*sw)
    f(14) = (eQED**2*gQCD**3*integralnorm*SwF)/(sqrt2*sw)
    f(15) = (2*eQED**2*gQCD**3*integralnorm*SwF)/(sqrt2*sw)

  c = [ 9*CI*f(10), f(11), 8*f(11), 9*CI*f(12), f(13), 8*f(13), 3*f(14), 3*f(15) ]
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
  complex(REALKIND) :: A(46)
  ! external WFs
  call wf_A(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_V(P(:,3), rMZ, H(3), wf(:,-2))
  call wf_V(P(:,4), rMW, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))

  ! internal WFs
  call vert_AZ_Q(gZu,wf(:,0),wf(:,-2),wf(:,1))
  call vert_WQ_A(wf(:,-3),wf(:,-1),wf(:,2))
  call prop_A_Q(wf(:,1),Q(:,5),ZERO,0_intkind1,wf(:,3))
  call prop_Q_A(wf(:,2),Q(:,10),ZERO,0_intkind1,wf(:,4))
  call vert_AV_Q(wf(:,3),wf(:,-4),wf(:,5))
  call vert_VQ_A(wf(:,-4),wf(:,-1),wf(:,6))
  call prop_Q_A(wf(:,6),Q(:,18),ZERO,0_intkind1,wf(:,7))
  call vert_AW_Q(wf(:,3),wf(:,-3),wf(:,8))
  call vert_AW_Q(wf(:,0),wf(:,-3),wf(:,9))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,-1),wf(:,10))
  call prop_A_Q(wf(:,9),Q(:,9),ZERO,0_intkind1,wf(:,11))
  call prop_Q_A(wf(:,10),Q(:,6),ZERO,0_intkind1,wf(:,12))
  call vert_AV_Q(wf(:,11),wf(:,-4),wf(:,13))
  call vert_AV_Q(wf(:,0),wf(:,-4),wf(:,14))
  call prop_A_Q(wf(:,14),Q(:,17),ZERO,0_intkind1,wf(:,15))
  call vert_AW_Q(wf(:,15),wf(:,-3),wf(:,16))
  call vert_AZ_Q(gZd,wf(:,11),wf(:,-2),wf(:,17))
  call vert_AZ_Q(gZu,wf(:,15),wf(:,-2),wf(:,18))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,19))
  call prop_W_W(wf(:,19),Q(:,12),MW,1_intkind1,wf(:,20))
  call vert_QA_W(wf(:,-1),wf(:,15),wf(:,21))
  call vert_QA_W(wf(:,7),wf(:,0),wf(:,22))
  call counter_AV_Q(wf(:,3),wf(:,-4),wf(:,23))
  call counter_AW_Q(wf(:,3),wf(:,-3),wf(:,24))
  call counter_AV_Q(wf(:,11),wf(:,-4),wf(:,25))
  call counter_AW_Q(wf(:,15),wf(:,-3),wf(:,26))
  call counter_AZ_Q(gZd,wf(:,11),wf(:,-2),wf(:,27))
  call counter_VG_G(wf(:,-2),wf(:,-4),Q(:,16),wf(:,28),Q(:,20))
  call vert_QA_V(wf(:,-1),wf(:,11),wf(:,29))
  call counter_AZ_Q(gZu,wf(:,15),wf(:,-2),wf(:,30))
  call vert_QA_V(wf(:,4),wf(:,0),wf(:,31))
  call counter_VQ_A(wf(:,-4),wf(:,-1),wf(:,32))
  call prop_Q_A(wf(:,32),Q(:,18),ZERO,0_intkind1,wf(:,33))
  call counter_WQ_A(wf(:,-3),wf(:,-1),wf(:,34))
  call prop_Q_A(wf(:,34),Q(:,10),ZERO,0_intkind1,wf(:,35))
  call counter_QA_W(wf(:,-1),wf(:,15),wf(:,36))
  call vert_QA_W(wf(:,33),wf(:,0),wf(:,37))
  call counter_ZQ_A(gZd,wf(:,-2),wf(:,-1),wf(:,38))
  call prop_Q_A(wf(:,38),Q(:,6),ZERO,0_intkind1,wf(:,39))
  call counter_AV_Q(wf(:,0),wf(:,-4),wf(:,40))
  call prop_A_Q(wf(:,40),Q(:,17),ZERO,0_intkind1,wf(:,41))
  call vert_AW_Q(wf(:,41),wf(:,-3),wf(:,42))
  call counter_AW_Q(wf(:,0),wf(:,-3),wf(:,43))
  call prop_A_Q(wf(:,43),Q(:,9),ZERO,0_intkind1,wf(:,44))
  call vert_AV_Q(wf(:,44),wf(:,-4),wf(:,45))
  call vert_AZ_Q(gZu,wf(:,41),wf(:,-2),wf(:,46))
  call counter_QA_W(wf(:,7),wf(:,0),wf(:,47))
  call vert_QA_W(wf(:,-1),wf(:,41),wf(:,48))
  call vert_AZ_Q(gZd,wf(:,44),wf(:,-2),wf(:,49))
  call counter_AZ_Q(gZu,wf(:,0),wf(:,-2),wf(:,50))
  call prop_A_Q(wf(:,50),Q(:,5),ZERO,0_intkind1,wf(:,51))
  call vert_AV_Q(wf(:,51),wf(:,-4),wf(:,52))
  call vert_AW_Q(wf(:,51),wf(:,-3),wf(:,53))
  call vert_VQ_A(wf(:,-4),wf(:,4),wf(:,54))
  call counter_A_Q(ctqq,wf(:,3),Q(:,5),wf(:,55))
  call prop_Q_A(wf(:,54),Q(:,26),ZERO,0_intkind1,wf(:,56))
  call counter_Q_A(ctqq,wf(:,4),Q(:,10),wf(:,57))
  call prop_A_Q(wf(:,5),Q(:,21),ZERO,0_intkind1,wf(:,58))
  call vert_WQ_A(wf(:,-3),wf(:,7),wf(:,59))
  call prop_Q_A(wf(:,59),Q(:,26),ZERO,0_intkind1,wf(:,60))
  call counter_Q_A(ctqq,wf(:,7),Q(:,18),wf(:,61))
  call prop_A_Q(wf(:,8),Q(:,13),ZERO,0_intkind1,wf(:,62))
  call vert_VQ_A(wf(:,-4),wf(:,12),wf(:,63))
  call counter_A_Q(ctqq,wf(:,11),Q(:,9),wf(:,64))
  call prop_Q_A(wf(:,63),Q(:,22),ZERO,0_intkind1,wf(:,65))
  call counter_Q_A(ctqq,wf(:,12),Q(:,6),wf(:,66))
  call prop_A_Q(wf(:,13),Q(:,25),ZERO,0_intkind1,wf(:,67))
  call vert_WQ_A(wf(:,-3),wf(:,12),wf(:,68))
  call counter_A_Q(ctqq,wf(:,15),Q(:,17),wf(:,69))
  call prop_Q_A(wf(:,68),Q(:,14),ZERO,0_intkind1,wf(:,70))
  call prop_A_Q(wf(:,16),Q(:,25),ZERO,0_intkind1,wf(:,71))
  call vert_ZQ_A(gZd,wf(:,-2),wf(:,7),wf(:,72))
  call prop_Q_A(wf(:,72),Q(:,22),ZERO,0_intkind1,wf(:,73))
  call prop_A_Q(wf(:,17),Q(:,13),ZERO,0_intkind1,wf(:,74))
  call vert_ZQ_A(gZu,wf(:,-2),wf(:,4),wf(:,75))
  call prop_Q_A(wf(:,75),Q(:,14),ZERO,0_intkind1,wf(:,76))
  call prop_A_Q(wf(:,18),Q(:,21),ZERO,0_intkind1,wf(:,77))
  call vert_WQ_A(wf(:,20),wf(:,-1),wf(:,78))
  call prop_Q_A(wf(:,78),Q(:,14),ZERO,0_intkind1,wf(:,79))
  call vert_AW_Q(wf(:,0),wf(:,20),wf(:,80))
  call prop_A_Q(wf(:,80),Q(:,13),ZERO,0_intkind1,wf(:,81))

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
  den(6) = 1 / (Q(5,9))
  den(7) = 1 / (Q(5,6))
  den(9) = 1 / (Q(5,17))
  den(13) = 1 / (Q(5,12) - MW2)
  den(16) = 1 / (Q(5,20))
  den(19) = 1 / (Q(5,26))
  den(22) = 1 / (Q(5,21))
  den(27) = 1 / (Q(5,13))
  den(30) = 1 / (Q(5,22))
  den(33) = 1 / (Q(5,25))
  den(36) = 1 / (Q(5,14))
  den(53) = 1 / (Q(5,11))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(8) = den(6)*den(7)
  den(10) = den(7)*den(9)
  den(11) = den(4)*den(6)
  den(12) = den(2)*den(9)
  den(14) = den(9)*den(13)
  den(15) = den(4)*den(13)
  den(17) = den(6)*den(16)
  den(18) = den(2)*den(16)
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
  den(37) = den(7)*den(36)
  den(38) = den(9)*den(37)
  den(39) = den(9)*den(33)
  den(40) = den(7)*den(39)
  den(41) = den(4)*den(30)
  den(42) = den(6)*den(41)
  den(43) = den(6)*den(27)
  den(44) = den(4)*den(43)
  den(45) = den(2)*den(36)
  den(46) = den(9)*den(45)
  den(47) = den(9)*den(22)
  den(48) = den(2)*den(47)
  den(49) = den(13)*den(36)
  den(50) = den(9)*den(49)
  den(51) = den(13)*den(27)
  den(52) = den(4)*den(51)
  den(54) = den(6)*den(53)
  den(55) = den(2)*den(53)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(46)

  A(1) = cont_QA(wf(:,4),wf(:,5)) * den(3)
  A(2) = cont_QA(wf(:,7),wf(:,8)) * den(5)
  A(3) = cont_QA(wf(:,12),wf(:,13)) * den(8)
  A(4) = cont_QA(wf(:,12),wf(:,16)) * den(10)
  A(5) = cont_QA(wf(:,7),wf(:,17)) * den(11)
  A(6) = cont_QA(wf(:,4),wf(:,18)) * den(12)
  A(7) = cont_VV(wf(:,20),wf(:,21)) * den(14)
  A(8) = cont_VV(wf(:,20),wf(:,22)) * den(15)

  A(9) = cont_QA(wf(:,4),wf(:,23)) * den(3)
  A(10) = cont_QA(wf(:,7),wf(:,24)) * den(5)
  A(11) = cont_QA(wf(:,12),wf(:,25)) * den(8)
  A(12) = cont_QA(wf(:,12),wf(:,26)) * den(10)
  A(13) = cont_QA(wf(:,7),wf(:,27)) * den(11)
  A(14) = cont_VV(wf(:,28),wf(:,29)) * den(17)
  A(15) = cont_QA(wf(:,4),wf(:,30)) * den(12)
  A(16) = cont_VV(wf(:,28),wf(:,31)) * den(18)
  A(17) = cont_QA(wf(:,8),wf(:,33)) * den(5)
  A(18) = cont_QA(wf(:,5),wf(:,35)) * den(3)
  A(19) = cont_QA(wf(:,17),wf(:,33)) * den(11)
  A(20) = cont_VV(wf(:,20),wf(:,36)) * den(14)
  A(21) = cont_VV(wf(:,20),wf(:,37)) * den(15)
  A(22) = cont_QA(wf(:,18),wf(:,35)) * den(12)
  A(23) = cont_QA(wf(:,13),wf(:,39)) * den(8)
  A(24) = cont_QA(wf(:,16),wf(:,39)) * den(10)
  A(25) = cont_QA(wf(:,12),wf(:,42)) * den(10)
  A(26) = cont_QA(wf(:,12),wf(:,45)) * den(8)
  A(27) = cont_QA(wf(:,4),wf(:,46)) * den(12)
  A(28) = cont_VV(wf(:,20),wf(:,47)) * den(15)
  A(29) = cont_VV(wf(:,20),wf(:,48)) * den(14)
  A(30) = cont_QA(wf(:,7),wf(:,49)) * den(11)
  A(31) = cont_QA(wf(:,4),wf(:,52)) * den(3)
  A(32) = cont_QA(wf(:,7),wf(:,53)) * den(5)
  A(33) = cont_QA(wf(:,55),wf(:,56)) * den(21)
  A(34) = cont_QA(wf(:,57),wf(:,58)) * den(24)
  A(35) = cont_QA(wf(:,55),wf(:,60)) * den(26)
  A(36) = cont_QA(wf(:,61),wf(:,62)) * den(29)
  A(37) = cont_QA(wf(:,64),wf(:,65)) * den(32)
  A(38) = cont_QA(wf(:,66),wf(:,67)) * den(35)
  A(39) = cont_QA(wf(:,69),wf(:,70)) * den(38)
  A(40) = cont_QA(wf(:,66),wf(:,71)) * den(40)
  A(41) = cont_QA(wf(:,64),wf(:,73)) * den(42)
  A(42) = cont_QA(wf(:,61),wf(:,74)) * den(44)
  A(43) = cont_QA(wf(:,69),wf(:,76)) * den(46)
  A(44) = cont_QA(wf(:,57),wf(:,77)) * den(48)
  A(45) = cont_QA(wf(:,69),wf(:,79)) * den(50)
  A(46) = cont_QA(wf(:,61),wf(:,81)) * den(52)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(46)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(7)-A(8))*f(1)+(-A(1)-A(2)-A(3)-A(4)-A(5)-A(6))*f(5)

  M2(1) = (A(45)+A(46))*f(2)+(-A(21)-A(29))*f(3)+(-A(20)-A(28))*f(4)+(A(33)+A(34)+A(35)+A(36)+A(37)+A(38)+A(39)+A(40)+A(41)+A(42) &
       +A(43)+A(44))*f(6)+(-A(9)-A(11)-A(17)-A(19)-A(25)-A(27))*f(7)+(-A(10)-A(12)-A(13)-A(15)-A(18)-A(22)-A(23)-A(24)-A(26)-A(30) &
       -A(31)-A(32))*f(8)+(A(14)+A(16))*f(9)

end subroutine colourvectors

end module ol_loop_ppzwj_uxdzwxg_1_/**/REALKIND
