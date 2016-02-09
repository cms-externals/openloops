
module ol_colourmatrix_pphzjj_uuxccxhz_1_/**/REALKIND
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
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [   0,   0]
  K1(42,:) = [   0,   0]
  K1(43,:) = [   0,   0]
  K1(44,:) = [   0,   0]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphzjj_uuxccxhz_1_/**/REALKIND



module ol_forced_parameters_pphzjj_uuxccxhz_1_/**/REALKIND
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
  if (wMH /= 0) write(*,101) 'wMH = 0'
  if (wMZ /= 0) write(*,101) 'wMZ = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphzjj_uuxccxhz_1_/**/REALKIND

module ol_loop_pphzjj_uuxccxhz_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(16), c(21)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:65)
  ! denominators
  complex(REALKIND), save :: den(66)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,48)
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
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED**2*gQCD**2*lambdaHZZ*MW)/(cw**2*sw)
    f( 2) = (CI*countertermnorm*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 3) = (CI*countertermnorm*ctGcc*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 4) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 5) = (CI*countertermnorm*ctVcc*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 6) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 7) = (countertermnorm*ctZGG*eQED**2*gQCD**4*lambdaHZZ*MW)/(cw**2*sw)
    f( 8) = (CI*eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f( 9) = (eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw*2._/**/REALKIND)
    f(10) = (eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f(11) = (eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(12) = (2*eQED**2*gQCD**4*integralnorm*lambdaHZZ*MW*SwF)/(cw**2*sw)
    f(13) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MB*YB)/MQ2sum
    f(14) = (eQED**2*gQCD**4*integralnorm*SwF*YB)/(MW*sw*2._/**/REALKIND)
    f(15) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**4*MT*YT)/MQ2sum
    f(16) = (eQED**2*gQCD**4*integralnorm*SwF*YT)/(MW*sw*2._/**/REALKIND)

  c = [ 9*CI*f(8), 27*CI*f(8), 18*f(9), 54*f(9), f(10), 3*f(10), 6*f(10), 8*f(10), 10*f(10), 18*f(10), 21*f(10), 24*f(10) &
    , 54*f(10), 3*f(11), 9*f(11), 3*f(12), 9*f(12), 3*f(14), 9*f(14), 3*f(16), 9*f(16) ]
  c = (1._/**/REALKIND / 36) * c
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
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(33)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_S(P(:,5), rMH, H(5), wf(:,-4))
  call wf_V(P(:,6), rMZ, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_SV_V(wf(:,-4),wf(:,-5),wf(:,2))
  call prop_W_W(wf(:,2),Q(:,48),MZ,1_intkind1,wf(:,3))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,4))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,3),wf(:,5))
  call prop_Q_A(wf(:,4),Q(:,7),ZERO,0_intkind1,wf(:,6))
  call vert_ZQ_A(gZu,wf(:,3),wf(:,-2),wf(:,7))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,52),ZERO,0_intkind1,wf(:,9))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,10))
  call vert_VQ_A(wf(:,10),wf(:,0),wf(:,11))
  call vert_AZ_Q(gZu,wf(:,-1),wf(:,3),wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,13),ZERO,0_intkind1,wf(:,13))
  call vert_ZQ_A(gZu,wf(:,3),wf(:,0),wf(:,14))
  call vert_AV_Q(wf(:,-1),wf(:,10),wf(:,15))
  call prop_Q_A(wf(:,14),Q(:,49),ZERO,0_intkind1,wf(:,16))
  call counter_GG_V(wf(:,1),Q(:,3),wf(:,10),Q(:,12),wf(:,17))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,-2),wf(:,18))
  call prop_Q_A(wf(:,18),Q(:,36),ZERO,0_intkind1,wf(:,19))
  call vert_QA_V(wf(:,19),wf(:,-3),wf(:,20))
  call counter_SG_G(wf(:,-4),wf(:,1),wf(:,21))
  call vert_AZ_Q(gZu,wf(:,-3),wf(:,-5),wf(:,22))
  call prop_A_Q(wf(:,22),Q(:,40),ZERO,0_intkind1,wf(:,23))
  call vert_QA_V(wf(:,-2),wf(:,23),wf(:,24))
  call counter_AZ_Q(gZu,wf(:,-3),wf(:,3),wf(:,25))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,26))
  call counter_ZQ_A(gZu,wf(:,3),wf(:,-2),wf(:,27))
  call prop_A_Q(wf(:,8),Q(:,11),ZERO,0_intkind1,wf(:,28))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,29))
  call prop_A_Q(wf(:,5),Q(:,56),ZERO,0_intkind1,wf(:,30))
  call vert_ZQ_A(gZu,wf(:,-5),wf(:,0),wf(:,31))
  call prop_Q_A(wf(:,31),Q(:,33),ZERO,0_intkind1,wf(:,32))
  call vert_QA_V(wf(:,32),wf(:,-1),wf(:,33))
  call counter_SG_G(wf(:,-4),wf(:,10),wf(:,34))
  call vert_AZ_Q(gZu,wf(:,-1),wf(:,-5),wf(:,35))
  call prop_A_Q(wf(:,35),Q(:,34),ZERO,0_intkind1,wf(:,36))
  call vert_QA_V(wf(:,0),wf(:,36),wf(:,37))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,38))
  call vert_AV_Q(wf(:,-1),wf(:,38),wf(:,39))
  call vert_VQ_A(wf(:,38),wf(:,0),wf(:,40))
  call prop_Q_A(wf(:,40),Q(:,13),ZERO,0_intkind1,wf(:,41))
  call counter_AZ_Q(gZu,wf(:,-1),wf(:,3),wf(:,42))
  call counter_AV_Q(wf(:,-1),wf(:,10),wf(:,43))
  call counter_VQ_A(wf(:,10),wf(:,0),wf(:,44))
  call prop_A_Q(wf(:,12),Q(:,50),ZERO,0_intkind1,wf(:,45))
  call counter_ZQ_A(gZu,wf(:,3),wf(:,0),wf(:,46))
  call prop_A_Q(wf(:,15),Q(:,14),ZERO,0_intkind1,wf(:,47))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,48))
  call vert_VQ_A(wf(:,48),wf(:,-2),wf(:,49))
  call prop_Q_A(wf(:,49),Q(:,7),ZERO,0_intkind1,wf(:,50))
  call vert_AV_Q(wf(:,-3),wf(:,48),wf(:,51))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,52))
  call vert_VQ_A(wf(:,52),wf(:,-2),wf(:,53))
  call vert_AV_Q(wf(:,-3),wf(:,52),wf(:,54))
  call counter_Q_A(ctcc,wf(:,6),Q(:,7),wf(:,55))
  call counter_Q_A(ctcc,wf(:,9),Q(:,52),wf(:,56))
  call counter_Q_A(ctqq,wf(:,13),Q(:,13),wf(:,57))
  call counter_Q_A(ctqq,wf(:,16),Q(:,49),wf(:,58))
  call counter_V_V(ctGG,wf(:,10),Q(:,12),wf(:,59))
  call vert_VQ_A(wf(:,59),wf(:,0),wf(:,60))
  call vert_AV_Q(wf(:,-1),wf(:,59),wf(:,61))
  call vert_QA_V(wf(:,16),wf(:,-1),wf(:,62))
  call vert_QA_V(wf(:,0),wf(:,45),wf(:,63))
  call vert_QA_V(wf(:,9),wf(:,-3),wf(:,64))
  call vert_QA_V(wf(:,-2),wf(:,30),wf(:,65))

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
  den(2) = 1 / (Q(5,48) - MZ2)
  den(3) = 1 / (Q(5,7))
  den(6) = 1 / (Q(5,52))
  den(9) = 1 / (Q(5,12))
  den(10) = 1 / (Q(5,13))
  den(13) = 1 / (Q(5,49))
  den(18) = 1 / (Q(5,36))
  den(19) = 1 / (Q(5,44))
  den(22) = 1 / (Q(5,40))
  den(25) = 1 / (Q(5,11))
  den(28) = 1 / (Q(5,56))
  den(31) = 1 / (Q(5,33))
  den(32) = 1 / (Q(5,35))
  den(35) = 1 / (Q(5,34))
  den(38) = 1 / (Q(5,50))
  den(41) = 1 / (Q(5,14))
  den(54) = 1 / (Q(5,51))
  den(57) = 1 / (Q(5,60))

  ! denominators
  den(4) = den(1)*den(3)
  den(5) = den(2)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(1)*den(7)
  den(11) = den(9)*den(10)
  den(12) = den(2)*den(11)
  den(14) = den(2)*den(13)
  den(15) = den(9)*den(14)
  den(16) = den(1)*den(9)
  den(17) = den(2)*den(16)
  den(20) = den(18)*den(19)
  den(21) = den(1)*den(20)
  den(23) = den(19)*den(22)
  den(24) = den(1)*den(23)
  den(26) = den(1)*den(25)
  den(27) = den(2)*den(26)
  den(29) = den(2)*den(28)
  den(30) = den(1)*den(29)
  den(33) = den(31)*den(32)
  den(34) = den(9)*den(33)
  den(36) = den(32)*den(35)
  den(37) = den(9)*den(36)
  den(39) = den(2)*den(38)
  den(40) = den(9)*den(39)
  den(42) = den(9)*den(41)
  den(43) = den(2)*den(42)
  den(44) = den(1)**2
  den(45) = den(29)*den(44)
  den(46) = den(7)*den(44)
  den(47) = den(4)*den(29)
  den(48) = den(7)*den(26)
  den(49) = den(11)*den(39)
  den(50) = den(14)*den(42)
  den(51) = den(9)**2
  den(52) = den(39)*den(51)
  den(53) = den(14)*den(51)
  den(55) = den(14)*den(54)
  den(56) = den(39)*den(54)
  den(58) = den(7)*den(57)
  den(59) = den(29)*den(57)
  den(60) = den(1)*den(2)*den(9)
  den(61) = den(1)*den(2)
  den(62) = den(2)*den(9)
  den(63) = den(1)*den(58)
  den(64) = den(1)*den(59)
  den(65) = den(9)*den(55)
  den(66) = den(9)*den(56)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(33)

  A(1) = cont_QA(wf(:,5),wf(:,6)) * den(5)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(3) = cont_QA(wf(:,12),wf(:,13)) * den(12)
  A(4) = cont_QA(wf(:,15),wf(:,16)) * den(15)

  A(5) = cont_VV(wf(:,3),wf(:,17)) * den(17)
  A(6) = cont_VV(wf(:,20),wf(:,21)) * den(21)
  A(7) = cont_VV(wf(:,20),wf(:,21)) * den(21)
  A(8) = cont_VV(wf(:,21),wf(:,24)) * den(24)
  A(9) = cont_VV(wf(:,21),wf(:,24)) * den(24)
  A(10) = cont_QA(wf(:,6),wf(:,25)) * den(5)
  A(11) = cont_QA(wf(:,9),wf(:,26)) * den(8)
  A(12) = cont_QA(wf(:,27),wf(:,28)) * den(27)
  A(13) = cont_QA(wf(:,29),wf(:,30)) * den(30)
  A(14) = cont_VV(wf(:,33),wf(:,34)) * den(34)
  A(15) = cont_VV(wf(:,33),wf(:,34)) * den(34)
  A(16) = cont_VV(wf(:,34),wf(:,37)) * den(37)
  A(17) = cont_VV(wf(:,34),wf(:,37)) * den(37)
  A(18) = cont_QA(wf(:,16),wf(:,39)) * den(15)
  A(19) = cont_QA(wf(:,12),wf(:,41)) * den(12)
  A(20) = cont_QA(wf(:,13),wf(:,42)) * den(12)
  A(21) = cont_QA(wf(:,16),wf(:,43)) * den(15)
  A(22) = cont_QA(wf(:,44),wf(:,45)) * den(40)
  A(23) = cont_QA(wf(:,46),wf(:,47)) * den(43)
  A(24) = cont_QA(wf(:,5),wf(:,50)) * den(5)
  A(25) = cont_QA(wf(:,9),wf(:,51)) * den(8)
  A(26) = cont_QA(wf(:,30),wf(:,53)) * den(45)
  A(27) = cont_QA(wf(:,9),wf(:,54)) * den(46)
  A(28) = cont_QA(wf(:,30),wf(:,55)) * den(47)
  A(29) = cont_QA(wf(:,28),wf(:,56)) * den(48)
  A(30) = cont_QA(wf(:,45),wf(:,57)) * den(49)
  A(31) = cont_QA(wf(:,47),wf(:,58)) * den(50)
  A(32) = cont_QA(wf(:,45),wf(:,60)) * den(52)
  A(33) = cont_QA(wf(:,16),wf(:,61)) * den(53)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(33)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = ((-A(1)-A(2)-A(3)-A(4))*f(1))/2._/**/REALKIND
  M1(2) = ((A(1)+A(2)+A(3)+A(4))*f(1))/6._/**/REALKIND

  M2(1) = ((A(26)+A(27)+A(28)+A(29)+A(30)+A(31)+A(32)+A(33))*f(2))/2._/**/REALKIND+((-A(11)-A(13)-A(18) &
       -A(19))*f(3))/2._/**/REALKIND+((-A(21)-A(22)-A(24)-A(25))*f(4))/2._/**/REALKIND+((-A(10)-A(12))*f(5))/2._/**/REALKIND+(( &
       -A(20)-A(23))*f(6))/2._/**/REALKIND+(A(5)*f(7))/2._/**/REALKIND+((-A(7)-A(9)-A(15)-A(17))*f(13))/2._/**/REALKIND+((-A(6) &
       -A(8)-A(14)-A(16))*f(15))/2._/**/REALKIND
  M2(2) = ((-A(26)-A(27)-A(28)-A(29)-A(30)-A(31)-A(32)-A(33))*f(2))/6._/**/REALKIND+((A(11)+A(13)+A(18) &
       +A(19))*f(3))/6._/**/REALKIND+((A(21)+A(22)+A(24)+A(25))*f(4))/6._/**/REALKIND+((A(10)+A(12))*f(5))/6._/**/REALKIND+((A(20) &
       +A(23))*f(6))/6._/**/REALKIND-(A(5)*f(7))/6._/**/REALKIND+((A(7)+A(9)+A(15)+A(17))*f(13))/6._/**/REALKIND+((A(6)+A(8)+A(14) &
       +A(16))*f(15))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_pphzjj_uuxccxhz_1_/**/REALKIND
