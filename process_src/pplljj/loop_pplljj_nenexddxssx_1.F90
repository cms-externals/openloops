
module ol_colourmatrix_pplljj_nenexddxssx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(46,2), K2(2,2), KL(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9,   3]
  K1( 2,:) = [   3,   9]
  K1( 3,:) = [   0,   0]
  K1( 4,:) = [   0,   0]
  K1( 5,:) = [   0,   0]
  K1( 6,:) = [   0,   0]
  K1( 7,:) = [   0,   0]
  K1( 8,:) = [   0,   0]
  K1( 9,:) = [   0,   0]
  K1(10,:) = [   0,   0]
  K1(11,:) = [   0,   0]
  K1(12,:) = [   0,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [   0,   0]
  K1(16,:) = [   0,   0]
  K1(17,:) = [   0,   0]
  K1(18,:) = [   0,   0]
  K1(19,:) = [   0,  -4]
  K1(20,:) = [  -4, -12]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1(25,:) = [   0,   0]
  K1(26,:) = [   0,   0]
  K1(27,:) = [   0,   4]
  K1(28,:) = [   4,   0]
  K1(29,:) = [ -12,  -4]
  K1(30,:) = [  -4,   0]
  K1(31,:) = [  12,   4]
  K1(32,:) = [   4,  12]
  K1(33,:) = [   0,   0]
  K1(34,:) = [   0,   0]
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [ -12,  -4]
  K1(38,:) = [  -4,   0]
  K1(39,:) = [   0,   4]
  K1(40,:) = [   4,   0]
  K1(41,:) = [   0,  -4]
  K1(42,:) = [  -4, -12]
  K1(43,:) = [  12,   4]
  K1(44,:) = [   4,  12]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pplljj_nenexddxssx_1_/**/REALKIND



module ol_forced_parameters_pplljj_nenexddxssx_1_/**/REALKIND
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
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pplljj_nenexddxssx_1_/**/REALKIND

module ol_loop_pplljj_nenexddxssx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(10), c(17)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:51)
  ! denominators
  complex(REALKIND), save :: den(51)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,64)
  ! zero helicity identifier
  logical,           save :: zerohel(64) = .true., zerohel_ct(64) = .true.

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
    f( 1) = CI*eQED**2*gQCD**2
    f( 2) = CI*countertermnorm*eQED**2*gQCD**4
    f( 3) = CI*countertermnorm*ctGqq*eQED**2*gQCD**4
    f( 4) = CI*countertermnorm*ctVqq*eQED**2*gQCD**4
    f( 5) = countertermnorm*ctZGG*eQED**2*gQCD**4
    f( 6) = CI*eQED**2*gQCD**4*integralnorm*SwB
    f( 7) = (eQED**2*gQCD**4*integralnorm*SwB)/2._/**/REALKIND
    f( 8) = eQED**2*gQCD**4*integralnorm*SwB
    f( 9) = eQED**2*gQCD**4*integralnorm*SwF
    f(10) = 2*eQED**2*gQCD**4*integralnorm*SwF

  c = [ 9*CI*f(6), 27*CI*f(6), 18*f(7), 54*f(7), f(8), 3*f(8), 6*f(8), 8*f(8), 10*f(8), 18*f(8), 21*f(8), 24*f(8), 54*f(8), 3*f(9) &
    , 9*f(9), 3*f(10), 9*f(10) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  integer,           intent(in), optional  :: POLSEL(6)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(25)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_Q(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_A(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_Q(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_A(P(:,6), rZERO, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,3))
  call vert_ZQ_A(gZd,wf(:,3),wf(:,-4),wf(:,4))
  call vert_AV_Q(wf(:,-5),wf(:,2),wf(:,5))
  call prop_Q_A(wf(:,4),Q(:,19),ZERO,0_intkind1,wf(:,6))
  call vert_VQ_A(wf(:,2),wf(:,-4),wf(:,7))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,3),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,28),ZERO,0_intkind1,wf(:,9))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,10))
  call vert_ZQ_A(gZd,wf(:,3),wf(:,-2),wf(:,11))
  call vert_AV_Q(wf(:,-3),wf(:,10),wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,7),ZERO,0_intkind1,wf(:,13))
  call vert_VQ_A(wf(:,10),wf(:,-2),wf(:,14))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,3),wf(:,15))
  call prop_Q_A(wf(:,14),Q(:,52),ZERO,0_intkind1,wf(:,16))
  call counter_VG_G(wf(:,3),wf(:,2),Q(:,12),wf(:,17),Q(:,15))
  call counter_AV_Q(wf(:,-5),wf(:,2),wf(:,18))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,3),wf(:,19))
  call counter_VQ_A(wf(:,2),wf(:,-4),wf(:,20))
  call prop_A_Q(wf(:,8),Q(:,35),ZERO,0_intkind1,wf(:,21))
  call counter_ZQ_A(gZd,wf(:,3),wf(:,-4),wf(:,22))
  call prop_A_Q(wf(:,5),Q(:,44),ZERO,0_intkind1,wf(:,23))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,24))
  call vert_AV_Q(wf(:,-3),wf(:,24),wf(:,25))
  call vert_VQ_A(wf(:,24),wf(:,-2),wf(:,26))
  call prop_Q_A(wf(:,26),Q(:,52),ZERO,0_intkind1,wf(:,27))
  call counter_AV_Q(wf(:,-3),wf(:,10),wf(:,28))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,3),wf(:,29))
  call counter_VQ_A(wf(:,10),wf(:,-2),wf(:,30))
  call prop_A_Q(wf(:,15),Q(:,11),ZERO,0_intkind1,wf(:,31))
  call counter_ZQ_A(gZd,wf(:,3),wf(:,-2),wf(:,32))
  call prop_A_Q(wf(:,12),Q(:,56),ZERO,0_intkind1,wf(:,33))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,34))
  call vert_AV_Q(wf(:,-5),wf(:,34),wf(:,35))
  call vert_VQ_A(wf(:,34),wf(:,-4),wf(:,36))
  call prop_Q_A(wf(:,36),Q(:,28),ZERO,0_intkind1,wf(:,37))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,38))
  call vert_VQ_A(wf(:,38),wf(:,-4),wf(:,39))
  call vert_AV_Q(wf(:,-5),wf(:,38),wf(:,40))
  call counter_Q_A(ctqq,wf(:,6),Q(:,19),wf(:,41))
  call counter_Q_A(ctqq,wf(:,9),Q(:,28),wf(:,42))
  call counter_Q_A(ctqq,wf(:,13),Q(:,7),wf(:,43))
  call counter_Q_A(ctqq,wf(:,16),Q(:,52),wf(:,44))
  call counter_V_V(ctGG,wf(:,10),Q(:,48),wf(:,45))
  call vert_VQ_A(wf(:,45),wf(:,-2),wf(:,46))
  call vert_AV_Q(wf(:,-3),wf(:,45),wf(:,47))
  call vert_QA_V(wf(:,13),wf(:,-3),wf(:,48))
  call vert_QA_V(wf(:,-2),wf(:,31),wf(:,49))
  call vert_QA_V(wf(:,6),wf(:,-5),wf(:,50))
  call vert_QA_V(wf(:,-4),wf(:,21),wf(:,51))

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
  den(2) = 1 / (Q(5,12))
  den(3) = 1 / (Q(5,19))
  den(6) = 1 / (Q(5,28))
  den(9) = 1 / (Q(5,48))
  den(10) = 1 / (Q(5,7))
  den(13) = 1 / (Q(5,52))
  den(18) = 1 / (Q(5,35))
  den(21) = 1 / (Q(5,44))
  den(24) = 1 / (Q(5,11))
  den(27) = 1 / (Q(5,56))
  den(40) = 1 / (Q(5,15))
  den(43) = 1 / (Q(5,51))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(1)*den(7)
  den(11) = den(1)*den(10)
  den(12) = den(9)*den(11)
  den(14) = den(9)*den(13)
  den(15) = den(1)*den(14)
  den(16) = den(1)*den(2)
  den(17) = den(9)*den(16)
  den(19) = den(1)*den(18)
  den(20) = den(2)*den(19)
  den(22) = den(2)*den(21)
  den(23) = den(1)*den(22)
  den(25) = den(1)*den(24)
  den(26) = den(9)*den(25)
  den(28) = den(9)*den(27)
  den(29) = den(1)*den(28)
  den(30) = den(2)**2
  den(31) = den(19)*den(30)
  den(32) = den(4)*den(30)
  den(33) = den(4)*den(22)
  den(34) = den(7)*den(19)
  den(35) = den(11)*den(28)
  den(36) = den(14)*den(25)
  den(37) = den(9)**2
  den(38) = den(25)*den(37)
  den(39) = den(11)*den(37)
  den(41) = den(11)*den(40)
  den(42) = den(25)*den(40)
  den(44) = den(4)*den(43)
  den(45) = den(19)*den(43)
  den(46) = den(1)*den(2)*den(9)
  den(47) = den(1)*den(9)
  den(48) = den(2)*den(44)
  den(49) = den(2)*den(45)
  den(50) = den(9)*den(41)
  den(51) = den(9)*den(42)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(25)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,12),wf(:,13)) * den(12)
  A(4) = cont_QA(wf(:,15),wf(:,16)) * den(15)

  A(5) = cont_VV(wf(:,10),wf(:,17)) * den(17)
  A(6) = cont_QA(wf(:,6),wf(:,18)) * den(5)
  A(7) = cont_QA(wf(:,9),wf(:,19)) * den(8)
  A(8) = cont_QA(wf(:,20),wf(:,21)) * den(20)
  A(9) = cont_QA(wf(:,22),wf(:,23)) * den(23)
  A(10) = cont_QA(wf(:,13),wf(:,25)) * den(12)
  A(11) = cont_QA(wf(:,15),wf(:,27)) * den(15)
  A(12) = cont_QA(wf(:,13),wf(:,28)) * den(12)
  A(13) = cont_QA(wf(:,16),wf(:,29)) * den(15)
  A(14) = cont_QA(wf(:,30),wf(:,31)) * den(26)
  A(15) = cont_QA(wf(:,32),wf(:,33)) * den(29)
  A(16) = cont_QA(wf(:,6),wf(:,35)) * den(5)
  A(17) = cont_QA(wf(:,8),wf(:,37)) * den(8)
  A(18) = cont_QA(wf(:,21),wf(:,39)) * den(31)
  A(19) = cont_QA(wf(:,6),wf(:,40)) * den(32)
  A(20) = cont_QA(wf(:,23),wf(:,41)) * den(33)
  A(21) = cont_QA(wf(:,21),wf(:,42)) * den(34)
  A(22) = cont_QA(wf(:,33),wf(:,43)) * den(35)
  A(23) = cont_QA(wf(:,31),wf(:,44)) * den(36)
  A(24) = cont_QA(wf(:,31),wf(:,46)) * den(38)
  A(25) = cont_QA(wf(:,13),wf(:,47)) * den(39)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(25)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((A(1)+A(2)+A(3)+A(4))*f(1))/2._/**/REALKIND
  M1(2) = ((-A(1)-A(2)-A(3)-A(4))*f(1))/6._/**/REALKIND

  M2(1) = ((-A(18)-A(19)-A(20)-A(21)-A(22)-A(23)-A(24)-A(25))*f(2))/2._/**/REALKIND+((A(6)+A(8)+A(10)+A(11)+A(12)+A(14)+A(16) &
       +A(17))*f(3))/2._/**/REALKIND+((A(7)+A(9)+A(13)+A(15))*f(4))/2._/**/REALKIND-(A(5)*f(5))/2._/**/REALKIND
  M2(2) = ((A(18)+A(19)+A(20)+A(21)+A(22)+A(23)+A(24)+A(25))*f(2))/6._/**/REALKIND+((-A(6)-A(8)-A(10)-A(11)-A(12)-A(14)-A(16) &
       -A(17))*f(3))/6._/**/REALKIND+((-A(7)-A(9)-A(13)-A(15))*f(4))/6._/**/REALKIND+(A(5)*f(5))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pplljj_nenexddxssx_1_/**/REALKIND
