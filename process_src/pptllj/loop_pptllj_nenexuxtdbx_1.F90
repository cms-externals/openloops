
module ol_colourmatrix_pptllj_nenexuxtdbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(23,1), K2(1,1), KL(1,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9]
  K1( 2,:) = [   0]
  K1( 3,:) = [   0]
  K1( 4,:) = [   0]
  K1( 5,:) = [   0]
  K1( 6,:) = [   0]
  K1( 7,:) = [  12]
  K1( 8,:) = [   0]
  K1( 9,:) = [   0]
  K1(10,:) = [   0]
  K1(11,:) = [  12]
  K1(12,:) = [   0]
  K1(13,:) = [   0]
  K1(14,:) = [ -12]
  K1(15,:) = [   0]
  K1(16,:) = [  12]
  K1(17,:) = [   0]
  K1(18,:) = [   0]
  K1(19,:) = [   0]
  K1(20,:) = [ -12]
  K1(21,:) = [   0]
  K1(22,:) = [  12]
  K1(23,:) = [   0]

  K2(1,:) = [ 9]

  KL(1,:) = [ 9, 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pptllj_nenexuxtdbx_1_/**/REALKIND



module ol_forced_parameters_pptllj_nenexuxtdbx_1_/**/REALKIND
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
  if (wMT /= 0) write(*,101) 'wMT = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pptllj_nenexuxtdbx_1_/**/REALKIND

module ol_loop_pptllj_nenexuxtdbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(28), c(11)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:75)
  ! denominators
  complex(REALKIND), save :: den(49)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,64)
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
    f( 1) = -(CI*countertermnorm*ctSqq*eQED**4*gQCD**2*(cw - sw)*(cw + sw))/(4._/**/REALKIND*cw*MW**2*sw**3)
    f( 2) = (CI*eQED**4)/(4._/**/REALKIND*sw**4)
    f( 3) = (CI*countertermnorm*ctVbt*eQED**4*gQCD**2)/(4._/**/REALKIND*sw**4)
    f( 4) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(4._/**/REALKIND*sw**4)
    f( 5) = (CI*countertermnorm*ctSqq*eQED**4*gQCD**2*ME**2)/(4._/**/REALKIND*MW**4*sw**4)
    f( 6) = (CI*eQED**4*ME)/(4._/**/REALKIND*MW**2*sw**4)
    f( 7) = (CI*countertermnorm*eQED**4*gQCD**2*ME)/(4._/**/REALKIND*MW**2*sw**4)
    f( 8) = (CI*countertermnorm*ctSqq*eQED**4*gQCD**2*ME)/(4._/**/REALKIND*MW**2*sw**4)
    f( 9) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2*ME)/(4._/**/REALKIND*MW**2*sw**4)
    f(10) = (CI*cw*eQED**4)/(2._/**/REALKIND*sw**3)
    f(11) = (CI*countertermnorm*ctVbt*cw*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**3)
    f(12) = (CI*countertermnorm*ctVqq*cw*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**3)
    f(13) = (CI*eQED**4)/(2._/**/REALKIND*sw**2)
    f(14) = (CI*countertermnorm*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(15) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(16) = (CI*countertermnorm*ctVbt*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(17) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(18) = (CI*countertermnorm*ctVtt*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(19) = (CI*countertermnorm*ctSqq*eQED**4*gQCD**2)/(2._/**/REALKIND*MW**2*sw**2)
    f(20) = (CI*eQED**4)/(2._/**/REALKIND*cw*sw)
    f(21) = (CI*countertermnorm*eQED**4*gQCD**2)/(2._/**/REALKIND*cw*sw)
    f(22) = (CI*countertermnorm*ctSqq*eQED**4*gQCD**2)/(2._/**/REALKIND*cw*sw)
    f(23) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(2._/**/REALKIND*cw*sw)
    f(24) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**4*4._/**/REALKIND)
    f(25) = (eQED**4*gQCD**2*integralnorm*ME*SwB)/(MW**2*sw**4*4._/**/REALKIND)
    f(26) = (cw*eQED**4*gQCD**2*integralnorm*SwB)/(sw**3*2._/**/REALKIND)
    f(27) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(28) = (eQED**4*gQCD**2*integralnorm*SwB)/(cw*sw*2._/**/REALKIND)

  c = [ 8*f(24), 8*f(25), f(26), 3*f(26), 8*f(26), f(27), 3*f(27), 8*f(27), f(28), 3*f(28), 8*f(28) ]
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
  complex(REALKIND) :: A(40)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_A(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_Q(P(:,4), rMT, H(4), wf(:,-3))
  call wf_Q(P(:,5), rZERO, H(5), wf(:,-4))
  call wf_A(P(:,6), rMB, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_Z(gZn,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_W(wf(:,-4),wf(:,-2),wf(:,2))
  call vert_AQ_S(gPbt,wf(:,-5),wf(:,-3),wf(:,3))
  call vert_VV_S(wf(:,1),wf(:,2),wf(:,4))
  call vert_QA_W(wf(:,-3),wf(:,-5),wf(:,5))
  call vert_UV_W(wf(:,2),Q(:,20),wf(:,1),Q(:,3),wf(:,6))
  call vert_ZQ_A(gZu,wf(:,1),wf(:,-3),wf(:,7))
  call vert_AW_Q(wf(:,-5),wf(:,2),wf(:,8))
  call prop_Q_A(wf(:,7),Q(:,11),MT,1_intkind1,wf(:,9))
  call vert_WQ_A(wf(:,2),wf(:,-3),wf(:,10))
  call vert_AZ_Q(gZd,wf(:,-5),wf(:,1),wf(:,11))
  call prop_Q_A(wf(:,10),Q(:,28),MB,1_intkind1,wf(:,12))
  call vert_AZ_Q(gZu,wf(:,-2),wf(:,1),wf(:,13))
  call vert_WQ_A(wf(:,5),wf(:,-4),wf(:,14))
  call prop_A_Q(wf(:,13),Q(:,7),ZERO,0_intkind1,wf(:,15))
  call vert_AW_Q(wf(:,-2),wf(:,5),wf(:,16))
  call vert_ZQ_A(gZd,wf(:,1),wf(:,-4),wf(:,17))
  call prop_A_Q(wf(:,16),Q(:,44),ZERO,0_intkind1,wf(:,18))
  call vert_WQ_A(wf(:,2),wf(:,0),wf(:,19))
  call vert_SA_Q(gPnl,wf(:,3),wf(:,-1),wf(:,20))
  call prop_Q_A(wf(:,19),Q(:,21),ME,1_intkind1,wf(:,21))
  call vert_AW_Q(wf(:,-1),wf(:,5),wf(:,22))
  call counter_AW_Q(wf(:,-5),wf(:,2),wf(:,23))
  call counter_AZ_Q(gZd,wf(:,-5),wf(:,1),wf(:,24))
  call counter_QS_A(gPud,wf(:,-4),wf(:,3),wf(:,25))
  call counter_WQ_A(wf(:,5),wf(:,-4),wf(:,26))
  call counter_ZQ_A(gZd,wf(:,1),wf(:,-4),wf(:,27))
  call counter_WQ_A(wf(:,2),wf(:,-3),wf(:,28))
  call prop_A_Q(wf(:,11),Q(:,35),MB,1_intkind1,wf(:,29))
  call counter_ZQ_A(gZu,wf(:,1),wf(:,-3),wf(:,30))
  call prop_A_Q(wf(:,8),Q(:,52),MT,1_intkind1,wf(:,31))
  call counter_AQ_S(ctSbt,wf(:,-5),wf(:,-3),wf(:,32))
  call counter_QA_W(wf(:,-3),wf(:,-5),wf(:,33))
  call vert_WQ_A(wf(:,33),wf(:,-4),wf(:,34))
  call vert_AW_Q(wf(:,-2),wf(:,33),wf(:,35))
  call prop_A_Q(wf(:,35),Q(:,44),ZERO,0_intkind1,wf(:,36))
  call counter_SA_Q(gPud,wf(:,3),wf(:,-2),wf(:,37))
  call prop_Q_A(wf(:,17),Q(:,19),ZERO,0_intkind1,wf(:,38))
  call counter_AW_Q(wf(:,-2),wf(:,5),wf(:,39))
  call counter_AZ_Q(gZu,wf(:,-2),wf(:,1),wf(:,40))
  call prop_Q_A(wf(:,14),Q(:,56),ZERO,0_intkind1,wf(:,41))
  call counter_AQ_S(gPud,wf(:,-2),wf(:,-4),wf(:,42))
  call vert_TV_S(wf(:,42),Q(:,20),wf(:,1),Q(:,3),wf(:,43))
  call vert_SV_V(wf(:,42),wf(:,1),wf(:,44))
  call counter_QA_W(wf(:,-4),wf(:,-2),wf(:,45))
  call vert_VV_S(wf(:,1),wf(:,45),wf(:,46))
  call vert_UV_W(wf(:,45),Q(:,20),wf(:,1),Q(:,3),wf(:,47))
  call vert_SA_Q(gPbt,wf(:,42),wf(:,-5),wf(:,48))
  call vert_AW_Q(wf(:,-5),wf(:,45),wf(:,49))
  call vert_QS_A(gPbt,wf(:,-3),wf(:,42),wf(:,50))
  call prop_Q_A(wf(:,50),Q(:,28),MB,1_intkind1,wf(:,51))
  call vert_WQ_A(wf(:,45),wf(:,-3),wf(:,52))
  call prop_Q_A(wf(:,52),Q(:,28),MB,1_intkind1,wf(:,53))
  call vert_SA_Q(gPnl,wf(:,32),wf(:,-1),wf(:,54))
  call vert_AW_Q(wf(:,-1),wf(:,33),wf(:,55))
  call vert_QS_A(gPln,wf(:,0),wf(:,42),wf(:,56))
  call prop_Q_A(wf(:,56),Q(:,21),ME,1_intkind1,wf(:,57))
  call vert_WQ_A(wf(:,45),wf(:,0),wf(:,58))
  call prop_Q_A(wf(:,58),Q(:,21),ME,1_intkind1,wf(:,59))
  call counter_Q_A(cttt,wf(:,9),Q(:,11),wf(:,60))
  call counter_Q_A(ctbb,wf(:,12),Q(:,28),wf(:,61))
  call counter_A_Q(ctqq,wf(:,15),Q(:,7),wf(:,62))
  call counter_A_Q(ctqq,wf(:,18),Q(:,44),wf(:,63))
  call vert_SV_V(wf(:,3),wf(:,1),wf(:,64))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,5),Q(:,40),wf(:,65))
  call vert_QA_W(wf(:,-4),wf(:,15),wf(:,66))
  call vert_QA_W(wf(:,9),wf(:,-5),wf(:,67))
  call vert_QA_W(wf(:,38),wf(:,-2),wf(:,68))
  call vert_QA_W(wf(:,-3),wf(:,29),wf(:,69))
  call vert_AQ_S(gPnl,wf(:,-1),wf(:,21),wf(:,70))
  call vert_QA_W(wf(:,21),wf(:,-1),wf(:,71))
  call prop_A_Q(wf(:,20),Q(:,42),ME,1_intkind1,wf(:,72))
  call vert_QA_W(wf(:,0),wf(:,72),wf(:,73))
  call prop_A_Q(wf(:,22),Q(:,42),ME,1_intkind1,wf(:,74))
  call vert_QA_W(wf(:,0),wf(:,74),wf(:,75))

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
  den(2) = 1 / (Q(5,20) - MW2)
  den(3) = 1 / (Q(5,40) - MW2)
  den(6) = 1 / (Q(5,11) - MT2)
  den(9) = 1 / (Q(5,28) - MB2)
  den(12) = 1 / (Q(5,7))
  den(15) = 1 / (Q(5,44))
  den(18) = 1 / (Q(5,21) - ME2)
  den(21) = 1 / (Q(5,35) - MB2)
  den(24) = 1 / (Q(5,52) - MT2)
  den(27) = 1 / (Q(5,19))
  den(30) = 1 / (Q(5,56))
  den(37) = 1 / (Q(5,23) - MW2)
  den(40) = 1 / (Q(5,43) - MW2)
  den(47) = 1 / (Q(5,42) - ME2)

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(13) = den(1)*den(12)
  den(14) = den(3)*den(13)
  den(16) = den(3)*den(15)
  den(17) = den(1)*den(16)
  den(19) = den(2)*den(18)
  den(20) = den(3)*den(19)
  den(22) = den(1)*den(21)
  den(23) = den(2)*den(22)
  den(25) = den(2)*den(24)
  den(26) = den(1)*den(25)
  den(28) = den(1)*den(27)
  den(29) = den(3)*den(28)
  den(31) = den(3)*den(30)
  den(32) = den(1)*den(31)
  den(33) = den(7)*den(25)
  den(34) = den(10)*den(22)
  den(35) = den(13)*den(31)
  den(36) = den(16)*den(28)
  den(38) = den(4)*den(37)
  den(39) = den(1)*den(3)
  den(41) = den(39)*den(40)
  den(42) = den(13)*den(37)
  den(43) = den(7)*den(40)
  den(44) = den(28)*den(37)
  den(45) = den(22)*den(40)
  den(46) = den(19)*den(37)
  den(48) = den(3)*den(47)
  den(49) = den(40)*den(48)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(40)

  A(1) = cont_SS(wf(:,3),wf(:,4)) * den(5)
  A(2) = cont_VV(wf(:,5),wf(:,6)) * den(5)
  A(3) = cont_QA(wf(:,8),wf(:,9)) * den(8)
  A(4) = cont_QA(wf(:,11),wf(:,12)) * den(11)
  A(5) = cont_QA(wf(:,14),wf(:,15)) * den(14)
  A(6) = cont_QA(wf(:,17),wf(:,18)) * den(17)
  A(7) = cont_QA(wf(:,20),wf(:,21)) * den(20)
  A(8) = cont_QA(wf(:,21),wf(:,22)) * den(20)

  A(9) = cont_QA(wf(:,9),wf(:,23)) * den(8)
  A(10) = cont_QA(wf(:,12),wf(:,24)) * den(11)
  A(11) = cont_QA(wf(:,15),wf(:,25)) * den(14)
  A(12) = cont_QA(wf(:,15),wf(:,26)) * den(14)
  A(13) = cont_QA(wf(:,18),wf(:,27)) * den(17)
  A(14) = cont_QA(wf(:,28),wf(:,29)) * den(23)
  A(15) = cont_QA(wf(:,30),wf(:,31)) * den(26)
  A(16) = cont_SS(wf(:,4),wf(:,32)) * den(5)
  A(17) = cont_VV(wf(:,6),wf(:,33)) * den(5)
  A(18) = cont_QA(wf(:,15),wf(:,34)) * den(14)
  A(19) = cont_QA(wf(:,17),wf(:,36)) * den(17)
  A(20) = cont_QA(wf(:,37),wf(:,38)) * den(29)
  A(21) = cont_QA(wf(:,38),wf(:,39)) * den(29)
  A(22) = cont_QA(wf(:,40),wf(:,41)) * den(32)
  A(23) = cont_SS(wf(:,3),wf(:,43)) * den(5)
  A(24) = cont_VV(wf(:,5),wf(:,44)) * den(5)
  A(25) = cont_SS(wf(:,3),wf(:,46)) * den(5)
  A(26) = cont_VV(wf(:,5),wf(:,47)) * den(5)
  A(27) = cont_QA(wf(:,9),wf(:,48)) * den(8)
  A(28) = cont_QA(wf(:,9),wf(:,49)) * den(8)
  A(29) = cont_QA(wf(:,11),wf(:,51)) * den(11)
  A(30) = cont_QA(wf(:,11),wf(:,53)) * den(11)
  A(31) = cont_QA(wf(:,21),wf(:,54)) * den(20)
  A(32) = cont_QA(wf(:,21),wf(:,55)) * den(20)
  A(33) = cont_QA(wf(:,20),wf(:,57)) * den(20)
  A(34) = cont_QA(wf(:,22),wf(:,57)) * den(20)
  A(35) = cont_QA(wf(:,20),wf(:,59)) * den(20)
  A(36) = cont_QA(wf(:,22),wf(:,59)) * den(20)
  A(37) = cont_QA(wf(:,31),wf(:,60)) * den(33)
  A(38) = cont_QA(wf(:,29),wf(:,61)) * den(34)
  A(39) = cont_QA(wf(:,41),wf(:,62)) * den(35)
  A(40) = cont_QA(wf(:,38),wf(:,63)) * den(36)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(40)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = A(8)*f(2)-A(7)*f(6)+A(2)*f(10)+(A(3)+A(4)+A(5)+A(6))*f(13)+A(1)*f(20)

  M2(1) = -(A(23)*f(1))+A(32)*f(3)+A(36)*f(4)-A(33)*f(5)-A(31)*f(7)+A(34)*f(8)-A(35)*f(9)+A(17)*f(11)+A(26)*f(12)+(-A(37)-A(38) &
       -A(39)-A(40))*f(14)+A(10)*f(15)+(A(9)+A(14)+A(18)+A(19))*f(16)+(A(12)+A(13)+A(21)+A(22)+A(28)+A(30))*f(17)+A(15)*f(18) &
       +(A(11)+A(20)+A(27)+A(29))*f(19)+A(16)*f(21)-A(24)*f(22)+A(25)*f(23)

end subroutine colourvectors

end module ol_loop_pptllj_nenexuxtdbx_1_/**/REALKIND
