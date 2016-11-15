
module ol_colourmatrix_heftpphhj_ddxhhg_1_/**/REALKIND
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
end module ol_colourmatrix_heftpphhj_ddxhhg_1_/**/REALKIND



module ol_forced_parameters_heftpphhj_ddxhhg_1_/**/REALKIND
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
  if (MB /= 0) write(*,101) 'MB = 0'
  if (YB /= 0) write(*,101) 'YB = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 5) write(*,101) 'nf = 5'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_heftpphhj_ddxhhg_1_/**/REALKIND

module ol_loop_heftpphhj_ddxhhg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(21), c(14)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:28)
  ! denominators
  complex(REALKIND), save :: den(15)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,8)
  ! zero helicity identifier
  logical,           save :: zerohel(8) = .true., zerohel_ct(8) = .true.

  contains

! **********************************************************************
subroutine fac_init_loop()
! Writes diagram prefactors to 'f', rsp. 'c'
! **********************************************************************
  use ol_parameters_decl_/**/REALKIND
  use ol_parameters_init_/**/REALKIND, only: parameters_init, loop_parameters_init
  use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB, DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*DOI*eQED**2*gQCD**5)/(1152._/**/REALKIND*MW**2*pi**4*sw**2)
    f( 2) = (CI*eQED**2*gQCD**3)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 3) = (CI*countertermnorm*eQED**2*gQCD**5)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 4) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**5)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 5) = (CI*eQED**2*gQCD**3*lambdaHHH*MH**2)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 6) = (CI*countertermnorm*eQED**2*gQCD**5*lambdaHHH*MH**2)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 7) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**5*lambdaHHH*MH**2)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 8) = (CI*countertermnorm*eQED**2*gQCD**5*R2HEFTghqq)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 9) = (CI*countertermnorm*eQED**2*gQCD**5*lambdaHHH*MH**2*R2HEFTghqq)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(10) = (CI*countertermnorm*eQED**2*gQCD**5*R2HEFThqq)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f(11) = (CI*countertermnorm*eQED**2*gQCD**5*lambdaHHH*MH**2*R2HEFThqq)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(12) = (CI*eQED**2*gQCD**5*integralnorm*SwB)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f(13) = (eQED**2*gQCD**5*integralnorm*SwB)/(MW**2*pi**2*sw**2*96._/**/REALKIND)
    f(14) = (eQED**2*gQCD**5*integralnorm*SwB)/(MW**2*pi**2*sw**2*48._/**/REALKIND)
    f(15) = (CI*eQED**2*gQCD**5*integralnorm*lambdaHHH*MH**2*SwB)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(16) = (eQED**2*gQCD**5*integralnorm*lambdaHHH*MH**2*SwB)/(MW**2*pi**2*sw**2*32._/**/REALKIND)
    f(17) = (eQED**2*gQCD**5*integralnorm*lambdaHHH*MH**2*SwB)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(18) = (eQED**2*gQCD**5*integralnorm*SwF)/(MW**2*pi**2*sw**2*24._/**/REALKIND)
    f(19) = (eQED**2*gQCD**5*integralnorm*SwF)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(20) = (eQED**2*gQCD**5*integralnorm*lambdaHHH*MH**2*SwF)/(MW**2*pi**2*sw**2*8._/**/REALKIND)
    f(21) = (3*eQED**2*gQCD**5*integralnorm*lambdaHHH*MH**2*SwF)/(MW**2*pi**2*sw**2*16._/**/REALKIND)

  c = [ 9*CI*f(12), 18*f(13), f(14), 8*f(14), 18*f(14), 9*CI*f(15), 18*f(16), f(17), 8*f(17), 18*f(17), 3*f(18), 3*f(19), 3*f(20) &
    , 3*f(21) ]
  c = (1._/**/REALKIND / 6) * c
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
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(18)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_S(P(:,3), rMH, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_S(P(:,4), rMH, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_S(P(:,3), rMH, H(3), wf(:,-2), 0)
    call pol_wf_S(P(:,4), rMH, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_HHG_G(wf(:,-2),wf(:,-3),wf(:,-4),Q(:,16),wf(:,2),Q(:,28))
  call vert_SS_S(wf(:,-2),wf(:,-3),wf(:,3))
  call vert_GG_H(wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,4))
  call counter_HHQA_V(wf(:,-2),wf(:,-3),wf(:,0),wf(:,-1),wf(:,5))
  call counter_HHG_G(ctHEFTggh,wf(:,-2),wf(:,-3),wf(:,-4),Q(:,16),wf(:,6),Q(:,28))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,7))
  call counter_HHA_Q(wf(:,-2),wf(:,-3),wf(:,-1),Q(:,2),wf(:,8),Q(:,14))
  call prop_Q_A(wf(:,7),Q(:,17),ZERO,0_intkind1,wf(:,9))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,10))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,11))
  call counter_QHH_A(wf(:,0),Q(:,1),wf(:,-2),wf(:,-3),wf(:,12),Q(:,13))
  call prop_A_Q(wf(:,11),Q(:,18),ZERO,0_intkind1,wf(:,13))
  call counter_QAV_H(wf(:,0),wf(:,-1),wf(:,-4),wf(:,14))
  call counter_GG_H(ctHEFTggh,wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,15))
  call vert_HG_G(wf(:,-2),wf(:,-4),Q(:,16),wf(:,16),Q(:,20))
  call counter_HG_G_vert(wf(:,-3),wf(:,1),Q(:,3),wf(:,17),Q(:,11))
  call counter_HG_G_vert(wf(:,-3),wf(:,-4),Q(:,16),wf(:,18),Q(:,24))
  call vert_HG_G(wf(:,-2),wf(:,1),Q(:,3),wf(:,19),Q(:,7))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,20))
  call vert_HG_G(wf(:,-3),wf(:,-4),Q(:,16),wf(:,21),Q(:,24))
  call counter_HG_G_vert(wf(:,-2),wf(:,1),Q(:,3),wf(:,22),Q(:,7))
  call counter_HG_G_vert(wf(:,-2),wf(:,-4),Q(:,16),wf(:,23),Q(:,20))
  call vert_HG_G(wf(:,-3),wf(:,1),Q(:,3),wf(:,24),Q(:,11))
  call counter_AQ_H(wf(:,-1),Q(:,2),wf(:,9),Q(:,17),wf(:,25))
  call counter_AQ_H(wf(:,13),Q(:,18),wf(:,0),Q(:,1),wf(:,26))
  call vert_GG_H(wf(:,10),Q(:,3),wf(:,-4),Q(:,16),wf(:,27))
  call vert_HG_G(wf(:,3),wf(:,-4),Q(:,16),wf(:,28),Q(:,28))

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
  den(2) = 1 / (Q(5,12) - MH2)
  den(4) = 1 / (Q(5,17))
  den(5) = 1 / (Q(5,28))
  den(6) = 1 / (Q(5,18))
  den(7) = 1 / (Q(5,20))
  den(9) = 1 / (Q(5,24))

  ! denominators
  den(3) = den(1)*den(2)
  den(8) = den(1)*den(7)
  den(10) = den(1)*den(9)
  den(11) = den(1)*den(5)
  den(12) = den(2)*den(4)
  den(13) = den(2)*den(6)
  den(14) = den(2)*den(5)
  den(15) = den(1)*den(14)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(18)

  A(1) = cont_VV(wf(:,1),wf(:,2)) * den(1)
  A(2) = cont_SS(wf(:,3),wf(:,4)) * den(3)

  A(3) = cont_VV(wf(:,-4),wf(:,5))
  A(4) = cont_VV(wf(:,1),wf(:,6)) * den(1)
  A(5) = cont_QA(wf(:,8),wf(:,9)) * den(4)
  A(6) = cont_VV(wf(:,2),wf(:,10)) * den(5)
  A(7) = cont_QA(wf(:,12),wf(:,13)) * den(6)
  A(8) = cont_SS(wf(:,3),wf(:,14)) * den(2)
  A(9) = cont_SS(wf(:,3),wf(:,15)) * den(3)
  A(10) = cont_VV(wf(:,16),wf(:,17)) * den(8)
  A(11) = cont_VV(wf(:,18),wf(:,19)) * den(10)
  A(12) = cont_VV(wf(:,2),wf(:,20)) * den(11)
  A(13) = cont_VV(wf(:,21),wf(:,22)) * den(10)
  A(14) = cont_VV(wf(:,23),wf(:,24)) * den(8)
  A(15) = cont_SS(wf(:,3),wf(:,25)) * den(12)
  A(16) = cont_SS(wf(:,3),wf(:,26)) * den(13)
  A(17) = cont_SS(wf(:,3),wf(:,27)) * den(3)
  A(18) = cont_VV(wf(:,20),wf(:,28)) * den(15)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(18)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = A(1)*f(2)-A(2)*f(5)

  M2(1) = (A(10)+A(11)+A(13)+A(14))*f(1)+(A(4)-A(12))*f(3)+A(6)*f(4)+(-A(9)+A(18))*f(6)-A(17)*f(7)-A(3)*f(8)+A(8)*f(9)+(-A(5) &
       -A(7))*f(10)+(A(15)+A(16))*f(11)

end subroutine colourvectors

end module ol_loop_heftpphhj_ddxhhg_1_/**/REALKIND
