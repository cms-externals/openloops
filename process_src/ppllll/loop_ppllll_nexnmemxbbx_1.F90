
module ol_colourmatrix_ppllll_nexnmemxbbx_1_/**/REALKIND
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

  K1( 1,:) = [  3]
  K1( 2,:) = [  0]
  K1( 3,:) = [  0]
  K1( 4,:) = [  0]
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
  K1(16,:) = [  4]
  K1(17,:) = [  0]
  K1(18,:) = [  0]
  K1(19,:) = [  0]
  K1(20,:) = [  0]
  K1(21,:) = [ -4]
  K1(22,:) = [  4]
  K1(23,:) = [  0]

  K2(1,:) = [ 3]

  KL(1,:) = [ 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppllll_nexnmemxbbx_1_/**/REALKIND



module ol_forced_parameters_ppllll_nexnmemxbbx_1_/**/REALKIND
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
  if (MM /= 0) write(*,101) 'MM = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppllll_nexnmemxbbx_1_/**/REALKIND

module ol_loop_ppllll_nexnmemxbbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(16), c(5)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:56)
  ! denominators
  complex(REALKIND), save :: den(46)
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
    f( 1) = (CI*eQED**4)/(4._/**/REALKIND*sw**4)
    f( 2) = (CI*countertermnorm*eQED**4*gQCD**2)/(4._/**/REALKIND*sw**4)
    f( 3) = (CI*countertermnorm*ctVbt*eQED**4*gQCD**2)/(4._/**/REALKIND*sw**4)
    f( 4) = (CI*eQED**4*MB)/(4._/**/REALKIND*sw**4)
    f( 5) = (CI*countertermnorm*ctSbb*eQED**4*gQCD**2*MB)/(4._/**/REALKIND*sw**4)
    f( 6) = (CI*cw*eQED**4)/(2._/**/REALKIND*sw**3)
    f( 7) = (CI*countertermnorm*ctVbb*cw*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**3)
    f( 8) = (CI*eQED**4)/(6._/**/REALKIND*sw**2)
    f( 9) = (CI*eQED**4)/(2._/**/REALKIND*sw**2)
    f(10) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(11) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(12) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**4*4._/**/REALKIND)
    f(13) = (eQED**4*gQCD**2*integralnorm*MB*SwB)/(sw**4*4._/**/REALKIND)
    f(14) = (cw*eQED**4*gQCD**2*integralnorm*SwB)/(sw**3*2._/**/REALKIND)
    f(15) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*6._/**/REALKIND)
    f(16) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*2._/**/REALKIND)

  c = [ 4*f(12), 4*f(13), 4*f(14), 4*f(15), 4*f(16) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(22)
  ! external WFs
  call wf_A(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rZERO, H(3), wf(:,-2))
  call wf_A(P(:,4), rZERO, H(4), wf(:,-3))
  call wf_Q(P(:,5), rMB, H(5), wf(:,-4))
  call wf_A(P(:,6), rMB, H(6), wf(:,-5))

  ! internal WFs
  call vert_QA_W(wf(:,-2),wf(:,0),wf(:,1))
  call vert_QA_W(wf(:,-1),wf(:,-3),wf(:,2))
  call vert_AQ_S(gH,wf(:,-5),wf(:,-4),wf(:,3))
  call prop_W_W(wf(:,1),Q(:,5),MW,1_intkind1,wf(:,4))
  call prop_W_W(wf(:,2),Q(:,10),MW,1_intkind1,wf(:,5))
  call vert_VV_S(wf(:,4),wf(:,5),wf(:,6))
  call vert_QA_V(wf(:,-4),wf(:,-5),wf(:,7))
  call vert_UV_W(wf(:,5),Q(:,10),wf(:,4),Q(:,5),wf(:,8))
  call vert_QA_Z(gZd,wf(:,-4),wf(:,-5),wf(:,9))
  call prop_W_W(wf(:,9),Q(:,48),MZ,1_intkind1,wf(:,10))
  call vert_WQ_A(wf(:,5),wf(:,-4),wf(:,11))
  call vert_AW_Q(wf(:,-5),wf(:,4),wf(:,12))
  call prop_Q_A(wf(:,11),Q(:,26),MT,1_intkind1,wf(:,13))
  call vert_WQ_A(wf(:,4),wf(:,-1),wf(:,14))
  call vert_AV_Q(wf(:,-3),wf(:,7),wf(:,15))
  call prop_Q_A(wf(:,14),Q(:,7),ZERO,0_intkind1,wf(:,16))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,10),wf(:,17))
  call vert_ZQ_A(gZn,wf(:,10),wf(:,-1),wf(:,18))
  call vert_AW_Q(wf(:,-3),wf(:,4),wf(:,19))
  call prop_Q_A(wf(:,18),Q(:,50),ZERO,0_intkind1,wf(:,20))
  call vert_AW_Q(wf(:,0),wf(:,5),wf(:,21))
  call vert_VQ_A(wf(:,7),wf(:,-2),wf(:,22))
  call prop_A_Q(wf(:,21),Q(:,11),ZERO,0_intkind1,wf(:,23))
  call vert_ZQ_A(gZl,wf(:,10),wf(:,-2),wf(:,24))
  call vert_AZ_Q(gZn,wf(:,0),wf(:,10),wf(:,25))
  call vert_WQ_A(wf(:,5),wf(:,-2),wf(:,26))
  call prop_A_Q(wf(:,25),Q(:,49),ZERO,0_intkind1,wf(:,27))
  call counter_AW_Q(wf(:,-5),wf(:,4),wf(:,28))
  call counter_WQ_A(wf(:,5),wf(:,-4),wf(:,29))
  call prop_A_Q(wf(:,12),Q(:,37),MT,1_intkind1,wf(:,30))
  call counter_AQ_S(gH,wf(:,-5),wf(:,-4),wf(:,31))
  call counter_QA_V(wf(:,-4),wf(:,-5),wf(:,32))
  call counter_QA_Z(gZd,wf(:,-4),wf(:,-5),wf(:,33))
  call prop_W_W(wf(:,33),Q(:,48),MZ,1_intkind1,wf(:,34))
  call vert_AV_Q(wf(:,-3),wf(:,32),wf(:,35))
  call vert_AZ_Q(gZl,wf(:,-3),wf(:,34),wf(:,36))
  call vert_ZQ_A(gZn,wf(:,34),wf(:,-1),wf(:,37))
  call prop_Q_A(wf(:,37),Q(:,50),ZERO,0_intkind1,wf(:,38))
  call vert_VQ_A(wf(:,32),wf(:,-2),wf(:,39))
  call vert_ZQ_A(gZl,wf(:,34),wf(:,-2),wf(:,40))
  call vert_AZ_Q(gZn,wf(:,0),wf(:,34),wf(:,41))
  call prop_A_Q(wf(:,41),Q(:,49),ZERO,0_intkind1,wf(:,42))
  call counter_Q_A(cttt,wf(:,13),Q(:,26),wf(:,43))
  call prop_W_W(wf(:,8),Q(:,15),MZ,1_intkind1,wf(:,44))
  call vert_QA_V(wf(:,16),wf(:,-3),wf(:,45))
  call vert_QA_Z(gZl,wf(:,16),wf(:,-3),wf(:,46))
  call prop_W_W(wf(:,46),Q(:,15),MZ,1_intkind1,wf(:,47))
  call prop_A_Q(wf(:,19),Q(:,13),ZERO,0_intkind1,wf(:,48))
  call vert_QA_Z(gZn,wf(:,-1),wf(:,48),wf(:,49))
  call prop_W_W(wf(:,49),Q(:,15),MZ,1_intkind1,wf(:,50))
  call vert_QA_V(wf(:,-2),wf(:,23),wf(:,51))
  call vert_QA_Z(gZl,wf(:,-2),wf(:,23),wf(:,52))
  call prop_W_W(wf(:,52),Q(:,15),MZ,1_intkind1,wf(:,53))
  call prop_Q_A(wf(:,26),Q(:,14),ZERO,0_intkind1,wf(:,54))
  call vert_QA_Z(gZn,wf(:,54),wf(:,0),wf(:,55))
  call prop_W_W(wf(:,55),Q(:,15),MZ,1_intkind1,wf(:,56))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5) - MW2)
  den(2) = 1 / (Q(5,10) - MW2)
  den(3) = 1 / (Q(5,48) - MH2)
  den(6) = 1 / (Q(5,48))
  den(8) = 1 / (Q(5,48) - MZ2)
  den(10) = 1 / (Q(5,26) - MT2)
  den(13) = 1 / (Q(5,7))
  den(17) = 1 / (Q(5,50))
  den(20) = 1 / (Q(5,11))
  den(24) = 1 / (Q(5,49))
  den(27) = 1 / (Q(5,37) - MT2)
  den(31) = 1 / (Q(5,15) - MH2)
  den(33) = 1 / (Q(5,15))
  den(35) = 1 / (Q(5,15) - MZ2)
  den(39) = 1 / (Q(5,13))
  den(44) = 1 / (Q(5,14))

  ! denominators
  den(4) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(4)*den(6)
  den(9) = den(4)*den(8)
  den(11) = den(2)*den(10)
  den(12) = den(1)*den(11)
  den(14) = den(1)*den(13)
  den(15) = den(6)*den(14)
  den(16) = den(8)*den(14)
  den(18) = den(8)*den(17)
  den(19) = den(1)*den(18)
  den(21) = den(2)*den(20)
  den(22) = den(6)*den(21)
  den(23) = den(8)*den(21)
  den(25) = den(8)*den(24)
  den(26) = den(2)*den(25)
  den(28) = den(1)*den(27)
  den(29) = den(2)*den(28)
  den(30) = den(11)*den(28)
  den(32) = den(4)*den(31)
  den(34) = den(4)*den(33)
  den(36) = den(4)*den(35)
  den(37) = den(14)*den(33)
  den(38) = den(14)*den(35)
  den(40) = den(1)*den(39)
  den(41) = den(35)*den(40)
  den(42) = den(21)*den(33)
  den(43) = den(21)*den(35)
  den(45) = den(2)*den(44)
  den(46) = den(35)*den(45)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(22)

  A(1) = cont_SS(wf(:,3),wf(:,6)) * den(5)
  A(2) = cont_VV(wf(:,7),wf(:,8)) * den(7)
  A(3) = cont_VV(wf(:,8),wf(:,10)) * den(9)
  A(4) = cont_QA(wf(:,12),wf(:,13)) * den(12)
  A(5) = cont_QA(wf(:,15),wf(:,16)) * den(15)
  A(6) = cont_QA(wf(:,16),wf(:,17)) * den(16)
  A(7) = cont_QA(wf(:,19),wf(:,20)) * den(19)
  A(8) = cont_QA(wf(:,22),wf(:,23)) * den(22)
  A(9) = cont_QA(wf(:,23),wf(:,24)) * den(23)
  A(10) = cont_QA(wf(:,26),wf(:,27)) * den(26)

  A(11) = cont_QA(wf(:,13),wf(:,28)) * den(12)
  A(12) = cont_QA(wf(:,29),wf(:,30)) * den(29)
  A(13) = cont_SS(wf(:,6),wf(:,31)) * den(5)
  A(14) = cont_VV(wf(:,8),wf(:,32)) * den(7)
  A(15) = cont_VV(wf(:,8),wf(:,34)) * den(9)
  A(16) = cont_QA(wf(:,16),wf(:,35)) * den(15)
  A(17) = cont_QA(wf(:,16),wf(:,36)) * den(16)
  A(18) = cont_QA(wf(:,19),wf(:,38)) * den(19)
  A(19) = cont_QA(wf(:,23),wf(:,39)) * den(22)
  A(20) = cont_QA(wf(:,23),wf(:,40)) * den(23)
  A(21) = cont_QA(wf(:,26),wf(:,42)) * den(26)
  A(22) = cont_QA(wf(:,30),wf(:,43)) * den(30)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(22)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = A(4)*f(1)-A(1)*f(4)+A(3)*f(6)+(-A(2)+A(5)+A(8))*f(8)+(A(6)+A(7)+A(9)+A(10))*f(9)

  M2(1) = -(A(22)*f(2))+(A(11)+A(12))*f(3)-A(13)*f(5)+A(15)*f(7)+(-A(14)+A(16)+A(19))*f(10)+(A(17)+A(18)+A(20)+A(21))*f(11)

end subroutine colourvectors

end module ol_loop_ppllll_nexnmemxbbx_1_/**/REALKIND
