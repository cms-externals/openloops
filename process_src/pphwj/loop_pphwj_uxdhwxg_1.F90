
module ol_colourmatrix_pphwj_uxdhwxg_1_/**/REALKIND
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
end module ol_colourmatrix_pphwj_uxdhwxg_1_/**/REALKIND



module ol_forced_parameters_pphwj_uxdhwxg_1_/**/REALKIND
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
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphwj_uxdhwxg_1_/**/REALKIND

module ol_loop_pphwj_uxdhwxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(10), c(5)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:29)
  ! denominators
  complex(REALKIND), save :: den(19)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,24)
  ! zero helicity identifier
  logical,           save :: zerohel(24) = .true., zerohel_ct(24) = .true.

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
    f( 1) = (CI*eQED**2*gQCD*lambdaHWW*MW)/(sqrt2*sw**2)
    f( 2) = (CI*countertermnorm*eQED**2*gQCD**3*lambdaHWW*MW)/(sqrt2*sw**2)
    f( 3) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**3*lambdaHWW*MW)/(sqrt2*sw**2)
    f( 4) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**3*lambdaHWW*MW)/(sqrt2*sw**2)
    f( 5) = (CI*eQED**2*gQCD**3*integralnorm*lambdaHWW*MW*SwB)/(sqrt2*sw**2)
    f( 6) = (eQED**2*gQCD**3*integralnorm*lambdaHWW*MW*SwB)/(sqrt2*sw**2)
    f( 7) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**3*MB*YB)/(MQ2sum*sqrt2*sw)
    f( 8) = (eQED**2*gQCD**3*integralnorm*SwF*YB)/(MW*sqrt2*sw**2*2._/**/REALKIND)
    f( 9) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**3*MT*YT)/(MQ2sum*sqrt2*sw)
    f(10) = (eQED**2*gQCD**3*integralnorm*SwF*YT)/(MW*sqrt2*sw**2*2._/**/REALKIND)

  c = [ 9*CI*f(5), f(6), 8*f(6), 3*f(8), 3*f(10) ]
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
  complex(REALKIND) :: A(12)
  ! external WFs
  call wf_A(P(:,1), rZERO, H(1), wf(:,0))
  call wf_Q(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_S(P(:,3), rMH, H(3), wf(:,-2))
  call wf_V(P(:,4), rMW, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))

  ! internal WFs
  call vert_AV_Q(wf(:,0),wf(:,-4),wf(:,1))
  call vert_SV_V(wf(:,-2),wf(:,-3),wf(:,2))
  call prop_A_Q(wf(:,1),Q(:,17),ZERO,0_intkind1,wf(:,3))
  call prop_W_W(wf(:,2),Q(:,12),MW,1_intkind1,wf(:,4))
  call vert_QA_W(wf(:,-1),wf(:,3),wf(:,5))
  call vert_VQ_A(wf(:,-4),wf(:,-1),wf(:,6))
  call prop_Q_A(wf(:,6),Q(:,18),ZERO,0_intkind1,wf(:,7))
  call vert_QA_W(wf(:,7),wf(:,0),wf(:,8))
  call vert_AW_Q(wf(:,0),wf(:,-3),wf(:,9))
  call counter_SG_G(wf(:,-2),wf(:,-4),wf(:,10))
  call prop_A_Q(wf(:,9),Q(:,9),ZERO,0_intkind1,wf(:,11))
  call vert_QA_V(wf(:,-1),wf(:,11),wf(:,12))
  call vert_WQ_A(wf(:,-3),wf(:,-1),wf(:,13))
  call prop_Q_A(wf(:,13),Q(:,10),ZERO,0_intkind1,wf(:,14))
  call vert_QA_V(wf(:,14),wf(:,0),wf(:,15))
  call counter_QA_W(wf(:,-1),wf(:,3),wf(:,16))
  call counter_VQ_A(wf(:,-4),wf(:,-1),wf(:,17))
  call prop_Q_A(wf(:,17),Q(:,18),ZERO,0_intkind1,wf(:,18))
  call vert_QA_W(wf(:,18),wf(:,0),wf(:,19))
  call counter_QA_W(wf(:,7),wf(:,0),wf(:,20))
  call counter_AV_Q(wf(:,0),wf(:,-4),wf(:,21))
  call prop_A_Q(wf(:,21),Q(:,17),ZERO,0_intkind1,wf(:,22))
  call vert_QA_W(wf(:,-1),wf(:,22),wf(:,23))
  call vert_WQ_A(wf(:,4),wf(:,-1),wf(:,24))
  call counter_A_Q(ctqq,wf(:,3),Q(:,17),wf(:,25))
  call prop_Q_A(wf(:,24),Q(:,14),ZERO,0_intkind1,wf(:,26))
  call vert_AW_Q(wf(:,0),wf(:,4),wf(:,27))
  call counter_Q_A(ctqq,wf(:,7),Q(:,18),wf(:,28))
  call prop_A_Q(wf(:,27),Q(:,13),ZERO,0_intkind1,wf(:,29))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,17))
  den(2) = 1 / (Q(5,12) - MW2)
  den(4) = 1 / (Q(5,18))
  den(6) = 1 / (Q(5,9))
  den(7) = 1 / (Q(5,20))
  den(9) = 1 / (Q(5,10))
  den(11) = 1 / (Q(5,14))
  den(14) = 1 / (Q(5,13))
  den(17) = 1 / (Q(5,11))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(2)*den(4)
  den(8) = den(6)*den(7)
  den(10) = den(7)*den(9)
  den(12) = den(2)*den(11)
  den(13) = den(1)*den(12)
  den(15) = den(2)*den(14)
  den(16) = den(4)*den(15)
  den(18) = den(6)*den(17)
  den(19) = den(9)*den(17)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(12)

  A(1) = cont_VV(wf(:,4),wf(:,5)) * den(3)
  A(2) = cont_VV(wf(:,4),wf(:,8)) * den(5)

  A(3) = cont_VV(wf(:,10),wf(:,12)) * den(8)
  A(4) = cont_VV(wf(:,10),wf(:,12)) * den(8)
  A(5) = cont_VV(wf(:,10),wf(:,15)) * den(10)
  A(6) = cont_VV(wf(:,10),wf(:,15)) * den(10)
  A(7) = cont_VV(wf(:,4),wf(:,16)) * den(3)
  A(8) = cont_VV(wf(:,4),wf(:,19)) * den(5)
  A(9) = cont_VV(wf(:,4),wf(:,20)) * den(5)
  A(10) = cont_VV(wf(:,4),wf(:,23)) * den(3)
  A(11) = cont_QA(wf(:,25),wf(:,26)) * den(13)
  A(12) = cont_QA(wf(:,28),wf(:,29)) * den(16)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(12)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(1)+A(2))*f(1)

  M2(1) = (-A(11)-A(12))*f(2)+(A(8)+A(10))*f(3)+(A(7)+A(9))*f(4)+(A(4)+A(6))*f(7)+(A(3)+A(5))*f(9)

end subroutine colourvectors

end module ol_loop_pphwj_uxdhwxg_1_/**/REALKIND
