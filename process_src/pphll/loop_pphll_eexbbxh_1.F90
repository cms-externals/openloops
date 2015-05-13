
module ol_colourmatrix_pphll_eexbbxh_1_/**/REALKIND
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
  K1( 2,:) = [  0]
  K1( 3,:) = [  0]
  K1( 4,:) = [  0]
  K1( 5,:) = [  0]
  K1( 6,:) = [  0]
  K1( 7,:) = [  4]
  K1( 8,:) = [  0]
  K1( 9,:) = [  0]
  K1(10,:) = [ -4]
  K1(11,:) = [  4]
  K1(12,:) = [  0]
  K1(13,:) = [  0]
  K1(14,:) = [  0]
  K1(15,:) = [  0]
  K1(16,:) = [  0]
  K1(17,:) = [  0]

  K2(1,:) = [ 3]

  KL(1,:) = [ 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphll_eexbbxh_1_/**/REALKIND



module ol_forced_parameters_pphll_eexbbxh_1_/**/REALKIND
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
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphll_eexbbxh_1_/**/REALKIND

module ol_loop_pphll_eexbbxh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(13), c(3)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:31)
  ! denominators
  complex(REALKIND), save :: den(22)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,16)
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
    f( 1) = (CI*eQED**3*lambdaHZZ*MW)/(cw**2*sw)
    f( 2) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**2*lambdaHZZ*MW)/(cw**2*sw)
    f( 3) = (eQED**3*gQCD**2*integralnorm*lambdaHZZ*MW*SwB)/(cw**2*sw)
    f( 4) = (CI*eQED**3*YB)/(6._/**/REALKIND*MW*sw)
    f( 5) = (CI*eQED**3*YB)/(2._/**/REALKIND*MW*sw)
    f( 6) = (CI*countertermnorm*eQED**3*gQCD**2*YB)/(6._/**/REALKIND*MW*sw)
    f( 7) = (CI*countertermnorm*eQED**3*gQCD**2*YB)/(2._/**/REALKIND*MW*sw)
    f( 8) = (CI*countertermnorm*ctSbb*eQED**3*gQCD**2*YB)/(6._/**/REALKIND*MW*sw)
    f( 9) = (CI*countertermnorm*ctSbb*eQED**3*gQCD**2*YB)/(2._/**/REALKIND*MW*sw)
    f(10) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**2*YB)/(6._/**/REALKIND*MW*sw)
    f(11) = (CI*countertermnorm*ctVbb*eQED**3*gQCD**2*YB)/(2._/**/REALKIND*MW*sw)
    f(12) = (eQED**3*gQCD**2*integralnorm*SwB*YB)/(MW*sw*6._/**/REALKIND)
    f(13) = (eQED**3*gQCD**2*integralnorm*SwB*YB)/(MW*sw*2._/**/REALKIND)

  c = [ 4*f(3), 4*f(12), 4*f(13) ]
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
  complex(REALKIND) :: A(18)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rMB, H(3), wf(:,-2))
  call wf_A(P(:,4), rMB, H(4), wf(:,-3))
  call wf_S(P(:,5), rMH, H(5), wf(:,-4))

  ! internal WFs
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,-3),wf(:,2))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,3))
  call prop_W_W(wf(:,2),Q(:,12),MZ,1_intkind1,wf(:,4))
  call vert_SV_V(wf(:,-4),wf(:,3),wf(:,5))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,6))
  call vert_QS_A(gH,wf(:,-2),wf(:,-4),wf(:,7))
  call prop_Q_A(wf(:,7),Q(:,20),MB,1_intkind1,wf(:,8))
  call vert_AV_Q(wf(:,-3),wf(:,6),wf(:,9))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,3),wf(:,10))
  call vert_SA_Q(gH,wf(:,-4),wf(:,-3),wf(:,11))
  call prop_A_Q(wf(:,11),Q(:,24),MB,1_intkind1,wf(:,12))
  call vert_VQ_A(wf(:,6),wf(:,-2),wf(:,13))
  call vert_ZQ_A(gZd,wf(:,3),wf(:,-2),wf(:,14))
  call counter_AV_Q(wf(:,-3),wf(:,6),wf(:,15))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,3),wf(:,16))
  call counter_SA_Q(gH,wf(:,-4),wf(:,-3),wf(:,17))
  call prop_A_Q(wf(:,17),Q(:,24),MB,1_intkind1,wf(:,18))
  call counter_VQ_A(wf(:,6),wf(:,-2),wf(:,19))
  call counter_ZQ_A(gZd,wf(:,3),wf(:,-2),wf(:,20))
  call counter_QS_A(gH,wf(:,-2),wf(:,-4),wf(:,21))
  call prop_Q_A(wf(:,21),Q(:,20),MB,1_intkind1,wf(:,22))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,-3),wf(:,23))
  call prop_W_W(wf(:,23),Q(:,12),MZ,1_intkind1,wf(:,24))
  call counter_Q_A(ctbb,wf(:,8),Q(:,20),wf(:,25))
  call prop_A_Q(wf(:,9),Q(:,11),MB,1_intkind1,wf(:,26))
  call prop_A_Q(wf(:,10),Q(:,11),MB,1_intkind1,wf(:,27))
  call counter_A_Q(ctbb,wf(:,12),Q(:,24),wf(:,28))
  call prop_Q_A(wf(:,13),Q(:,7),MB,1_intkind1,wf(:,29))
  call prop_Q_A(wf(:,14),Q(:,7),MB,1_intkind1,wf(:,30))
  call prop_W_W(wf(:,5),Q(:,19),MZ,1_intkind1,wf(:,31))

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
  den(2) = 1 / (Q(5,12) - MZ2)
  den(4) = 1 / (Q(5,3))
  den(5) = 1 / (Q(5,20) - MB2)
  den(8) = 1 / (Q(5,24) - MB2)
  den(11) = 1 / (Q(5,11) - MB2)
  den(16) = 1 / (Q(5,7) - MB2)
  den(21) = 1 / (Q(5,19) - MZ2)

  ! denominators
  den(3) = den(1)*den(2)
  den(6) = den(4)*den(5)
  den(7) = den(1)*den(5)
  den(9) = den(4)*den(8)
  den(10) = den(1)*den(8)
  den(12) = den(4)*den(11)
  den(13) = den(5)*den(12)
  den(14) = den(1)*den(11)
  den(15) = den(5)*den(14)
  den(17) = den(4)*den(16)
  den(18) = den(8)*den(17)
  den(19) = den(1)*den(16)
  den(20) = den(8)*den(19)
  den(22) = den(1)*den(21)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(18)

  A(1) = cont_VV(wf(:,4),wf(:,5)) * den(3)
  A(2) = cont_QA(wf(:,8),wf(:,9)) * den(6)
  A(3) = cont_QA(wf(:,8),wf(:,10)) * den(7)
  A(4) = cont_QA(wf(:,12),wf(:,13)) * den(9)
  A(5) = cont_QA(wf(:,12),wf(:,14)) * den(10)

  A(6) = cont_QA(wf(:,8),wf(:,15)) * den(6)
  A(7) = cont_QA(wf(:,8),wf(:,16)) * den(7)
  A(8) = cont_QA(wf(:,13),wf(:,18)) * den(9)
  A(9) = cont_QA(wf(:,14),wf(:,18)) * den(10)
  A(10) = cont_QA(wf(:,12),wf(:,19)) * den(9)
  A(11) = cont_QA(wf(:,12),wf(:,20)) * den(10)
  A(12) = cont_QA(wf(:,9),wf(:,22)) * den(6)
  A(13) = cont_QA(wf(:,10),wf(:,22)) * den(7)
  A(14) = cont_VV(wf(:,5),wf(:,24)) * den(3)
  A(15) = cont_QA(wf(:,25),wf(:,26)) * den(13)
  A(16) = cont_QA(wf(:,25),wf(:,27)) * den(15)
  A(17) = cont_QA(wf(:,28),wf(:,29)) * den(18)
  A(18) = cont_QA(wf(:,28),wf(:,30)) * den(20)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(18)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = A(1)*f(1)+(A(2)+A(4))*f(4)+(A(3)+A(5))*f(5)

  M2(1) = A(14)*f(2)+(-A(15)-A(17))*f(6)+(-A(16)-A(18))*f(7)+(A(8)+A(12))*f(8)+(A(9)+A(13))*f(9)+(A(6)+A(10))*f(10)+(A(7) &
       +A(11))*f(11)

end subroutine colourvectors

end module ol_loop_pphll_eexbbxh_1_/**/REALKIND
