
module ol_colourmatrix_heftpphjj_uuxddxh_1_/**/REALKIND
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
end module ol_colourmatrix_heftpphjj_uuxddxh_1_/**/REALKIND



module ol_forced_parameters_heftpphjj_uuxddxh_1_/**/REALKIND
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
  if (nf /= 5) write(*,101) 'nf = 5'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_heftpphjj_uuxddxh_1_/**/REALKIND

module ol_loop_heftpphjj_uuxddxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(11), c(19)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:30)
  ! denominators
  complex(REALKIND), save :: den(25)
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
!  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED*gQCD**4)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 2) = (CI*countertermnorm*eQED*gQCD**6)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 3) = (CI*countertermnorm*ctGqq*eQED*gQCD**6)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 4) = (CI*countertermnorm*eQED*gQCD**6*R2HEFTghqq)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 5) = (CI*countertermnorm*eQED*gQCD**6*R2HEFThqq)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 6) = (CI*eQED*gQCD**6*integralnorm*SwB)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 7) = (eQED*gQCD**6*integralnorm*SwB)/(MW*pi**2*sw*48._/**/REALKIND)
    f( 8) = (eQED*gQCD**6*integralnorm*SwB)/(MW*pi**2*sw*24._/**/REALKIND)
    f( 9) = (eQED*gQCD**4*integralnorm*MB*SwF)/(MW*sw*2._/**/REALKIND)
    f(10) = (eQED*gQCD**6*integralnorm*SwF)/(MW*pi**2*sw*24._/**/REALKIND)
    f(11) = (eQED*gQCD**6*integralnorm*SwF)/(MW*pi**2*sw*12._/**/REALKIND)

  c = [ 9*CI*f(6), 27*CI*f(6), 18*f(7), 54*f(7), f(8), 3*f(8), 6*f(8), 8*f(8), 10*f(8), 18*f(8), 21*f(8), 24*f(8), 54*f(8), 3*f(9) &
    , 9*f(9), 3*f(10), 9*f(10), 3*f(11), 9*f(11) ]
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
  complex(REALKIND) :: A(12)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_HG_G(wf(:,-4),wf(:,1),Q(:,3),wf(:,3),Q(:,19))
  call counter_HQA_V(wf(:,-4),wf(:,-2),wf(:,-3),wf(:,4))
  call counter_HQA_V(wf(:,-4),wf(:,0),wf(:,-1),wf(:,5))
  call counter_HG_G(ctHEFTggh,wf(:,-4),wf(:,1),Q(:,3),wf(:,6),Q(:,19))
  call counter_HA_Q(wf(:,-4),wf(:,-3),Q(:,8),wf(:,7),Q(:,24))
  call prop_A_Q(wf(:,7),Q(:,24),ZERO,0_intkind1,wf(:,8))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,9))
  call counter_QH_A(wf(:,-2),Q(:,4),wf(:,-4),wf(:,10),Q(:,20))
  call prop_Q_A(wf(:,10),Q(:,20),ZERO,0_intkind1,wf(:,11))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,12))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,13))
  call counter_HA_Q(wf(:,-4),wf(:,-1),Q(:,2),wf(:,14),Q(:,18))
  call prop_A_Q(wf(:,14),Q(:,18),ZERO,0_intkind1,wf(:,15))
  call vert_QA_V(wf(:,0),wf(:,15),wf(:,16))
  call counter_QH_A(wf(:,0),Q(:,1),wf(:,-4),wf(:,17),Q(:,17))
  call prop_Q_A(wf(:,17),Q(:,17),ZERO,0_intkind1,wf(:,18))
  call vert_QA_V(wf(:,18),wf(:,-1),wf(:,19))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,20))
  call vert_HG_G(wf(:,-4),wf(:,20),Q(:,3),wf(:,21),Q(:,19))
  call vert_HG_G(wf(:,-4),wf(:,2),Q(:,12),wf(:,22),Q(:,28))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,23))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,24))
  call prop_Q_A(wf(:,9),Q(:,7),ZERO,0_intkind1,wf(:,25))
  call prop_A_Q(wf(:,12),Q(:,11),ZERO,0_intkind1,wf(:,26))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,27))
  call prop_Q_A(wf(:,27),Q(:,13),ZERO,0_intkind1,wf(:,28))
  call vert_AV_Q(wf(:,-1),wf(:,2),wf(:,29))
  call prop_A_Q(wf(:,29),Q(:,14),ZERO,0_intkind1,wf(:,30))

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
  den(2) = 1 / (Q(5,12))
  den(4) = 1 / (Q(5,24))
  den(6) = 1 / (Q(5,20))
  den(8) = 1 / (Q(5,18))
  den(10) = 1 / (Q(5,17))
  den(12) = 1 / (Q(5,28))
  den(15) = 1 / (Q(5,19))
  den(18) = 1 / (Q(5,7))
  den(20) = 1 / (Q(5,11))
  den(22) = 1 / (Q(5,13))
  den(24) = 1 / (Q(5,14))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(1)*den(6)
  den(9) = den(2)*den(8)
  den(11) = den(2)*den(10)
  den(13) = den(2)*den(12)
  den(14) = den(1)*den(13)
  den(16) = den(1)*den(15)
  den(17) = den(2)*den(16)
  den(19) = den(1)*den(18)
  den(21) = den(1)*den(20)
  den(23) = den(2)*den(22)
  den(25) = den(2)*den(24)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(12)

  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(3)

  A(2) = cont_VV(wf(:,1),wf(:,4)) * den(1)
  A(3) = cont_VV(wf(:,2),wf(:,5)) * den(2)
  A(4) = cont_VV(wf(:,2),wf(:,6)) * den(3)
  A(5) = cont_QA(wf(:,8),wf(:,9)) * den(5)
  A(6) = cont_QA(wf(:,11),wf(:,12)) * den(7)
  A(7) = cont_VV(wf(:,3),wf(:,13)) * den(3)
  A(8) = cont_VV(wf(:,2),wf(:,16)) * den(9)
  A(9) = cont_VV(wf(:,2),wf(:,19)) * den(11)
  A(10) = cont_VV(wf(:,2),wf(:,21)) * den(3)
  A(11) = cont_VV(wf(:,22),wf(:,23)) * den(14)
  A(12) = cont_VV(wf(:,3),wf(:,24)) * den(17)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(12)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = -(A(1)*f(1))/2._/**/REALKIND
  M1(2) = (A(1)*f(1))/6._/**/REALKIND

  M2(1) = ((-A(4)+A(11)+A(12))*f(2))/2._/**/REALKIND+((-A(7)-A(10))*f(3))/2._/**/REALKIND+((A(2)+A(3))*f(4))/2._/**/REALKIND &
       +((A(5)+A(6)+A(8)+A(9))*f(5))/2._/**/REALKIND
  M2(2) = ((A(4)-A(11)-A(12))*f(2))/6._/**/REALKIND+((A(7)+A(10))*f(3))/6._/**/REALKIND+((-A(2)-A(3))*f(4))/6._/**/REALKIND+(( &
       -A(5)-A(6)-A(8)-A(9))*f(5))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_heftpphjj_uuxddxh_1_/**/REALKIND
