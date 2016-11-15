
module ol_colourmatrix_pphhjj_uxcdsxhh_1_/**/REALKIND
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
  K1( 2,:) = [  12]
  K1( 3,:) = [   0]
  K1( 4,:) = [  12]
  K1( 5,:) = [ -12]
  K1( 6,:) = [   0]
  K1( 7,:) = [  12]
  K1( 8,:) = [   0]
  K1( 9,:) = [ -12]
  K1(10,:) = [   0]
  K1(11,:) = [  12]
  K1(12,:) = [   0]
  K1(13,:) = [   0]
  K1(14,:) = [   0]
  K1(15,:) = [   0]
  K1(16,:) = [   0]
  K1(17,:) = [   0]
  K1(18,:) = [   0]
  K1(19,:) = [   0]
  K1(20,:) = [   0]
  K1(21,:) = [   0]
  K1(22,:) = [   0]
  K1(23,:) = [   0]

  K2(1,:) = [ 9]

  KL(1,:) = [ 9, 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_pphhjj_uxcdsxhh_1_/**/REALKIND



module ol_forced_parameters_pphhjj_uxcdsxhh_1_/**/REALKIND
  implicit none
  contains
  subroutine check_forced_parameters
    use ol_parameters_decl_/**/REALKIND
    use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
    use ol_loop_parameters_decl_/**/DREALKIND, only: LeadingColour, nc, nf, CKMORDER
#endif
    implicit none
    logical, save :: checks_not_written = .true.

    if (checks_not_written) then
    ! e.g.
    ! if (ME /= 0) write(*,101) 'ME = 0'
  if (CKMORDER /= 0) write(*,101) 'CKMORDER = 0'
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
  if (wMH /= 0) write(*,101) 'wMH = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_pphhjj_uxcdsxhh_1_/**/REALKIND

module ol_loop_pphhjj_uxcdsxhh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(22), c(12)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:49)
  ! denominators
  complex(REALKIND), save :: den(27)
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
!  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED**4)/(8._/**/REALKIND*sw**4)
    f( 2) = (CI*eQED**4)/(4._/**/REALKIND*sw**4)
    f( 3) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(8._/**/REALKIND*sw**4)
    f( 4) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2)/(4._/**/REALKIND*sw**4)
    f( 5) = (CI*countertermnorm*ctVsc*eQED**4*gQCD**2)/(8._/**/REALKIND*sw**4)
    f( 6) = (CI*countertermnorm*ctVsc*eQED**4*gQCD**2)/(4._/**/REALKIND*sw**4)
    f( 7) = (CI*countertermnorm*eQED**4*gQCD**2*lambdaHWW)/(4._/**/REALKIND*sw**4)
    f( 8) = (CI*countertermnorm*ctSqq*eQED**4*gQCD**2*lambdaHWW)/(4._/**/REALKIND*sw**4)
    f( 9) = (3*CI*eQED**4*lambdaHHH*lambdaHWW*MH**2)/(4._/**/REALKIND*sw**4)
    f(10) = (3*CI*countertermnorm*ctVqq*eQED**4*gQCD**2*lambdaHHH*lambdaHWW*MH**2)/(4._/**/REALKIND*sw**4)
    f(11) = (3*CI*countertermnorm*ctVsc*eQED**4*gQCD**2*lambdaHHH*lambdaHWW*MH**2)/(4._/**/REALKIND*sw**4)
    f(12) = (CI*countertermnorm*eQED**4*gQCD**2*MH**2)/(8._/**/REALKIND*MW**2*sw**4)
    f(13) = (CI*countertermnorm*ctSqq*eQED**4*gQCD**2*MH**2)/(8._/**/REALKIND*MW**2*sw**4)
    f(14) = (3*CI*countertermnorm*eQED**4*gQCD**2*lambdaHHH*MH**2)/(8._/**/REALKIND*MW**2*sw**4)
    f(15) = (3*CI*countertermnorm*ctSqq*eQED**4*gQCD**2*lambdaHHH*MH**2)/(8._/**/REALKIND*MW**2*sw**4)
    f(16) = (CI*eQED**4*lambdaHWW**2*MW**2)/(2._/**/REALKIND*sw**4)
    f(17) = (CI*countertermnorm*ctVqq*eQED**4*gQCD**2*lambdaHWW**2*MW**2)/(2._/**/REALKIND*sw**4)
    f(18) = (CI*countertermnorm*ctVsc*eQED**4*gQCD**2*lambdaHWW**2*MW**2)/(2._/**/REALKIND*sw**4)
    f(19) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**4*8._/**/REALKIND)
    f(20) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**4*4._/**/REALKIND)
    f(21) = (3*eQED**4*gQCD**2*integralnorm*lambdaHHH*lambdaHWW*MH**2*SwB)/(sw**4*4._/**/REALKIND)
    f(22) = (eQED**4*gQCD**2*integralnorm*lambdaHWW**2*MW**2*SwB)/(sw**4*2._/**/REALKIND)

  c = [ f(19), 3*f(19), 8*f(19), f(20), 3*f(20), 8*f(20), f(21), 3*f(21), 8*f(21), f(22), 3*f(22), 8*f(22) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  integer,           intent(in), optional  :: POLSEL(6)
  complex(REALKIND), intent(out) :: M1(1), M2(1)
  complex(REALKIND) :: A(28)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_A(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_S(P(:,6), rMH, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_A(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), 0)
    call pol_wf_S(P(:,6), rMH, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_W(wf(:,-2),wf(:,0),wf(:,1))
  call vert_QA_W(wf(:,-1),wf(:,-3),wf(:,2))
  call vert_SSV_V(wf(:,-4),wf(:,-5),wf(:,1),wf(:,3))
  call vert_SS_S(wf(:,-4),wf(:,-5),wf(:,4))
  call vert_VV_S(wf(:,1),wf(:,2),wf(:,5))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,1),Q(:,5),wf(:,6))
  call vert_TV_S(wf(:,-5),Q(:,32),wf(:,2),Q(:,10),wf(:,7))
  call vert_SV_V(wf(:,-4),wf(:,1),wf(:,8))
  call vert_SV_V(wf(:,-5),wf(:,2),wf(:,9))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,2),Q(:,10),wf(:,10))
  call vert_TV_S(wf(:,-5),Q(:,32),wf(:,1),Q(:,5),wf(:,11))
  call vert_SV_V(wf(:,-4),wf(:,2),wf(:,12))
  call vert_SV_V(wf(:,-5),wf(:,1),wf(:,13))
  call counter_QA_W(wf(:,-1),wf(:,-3),wf(:,14))
  call counter_QA_W(wf(:,-2),wf(:,0),wf(:,15))
  call vert_SSV_V(wf(:,-4),wf(:,-5),wf(:,15),wf(:,16))
  call counter_AQ_S(ctSsc,wf(:,-3),wf(:,-1),wf(:,17))
  call vert_VS_T(wf(:,1),Q(:,5),wf(:,17),Q(:,10),wf(:,18))
  call vert_VV_S(wf(:,1),wf(:,14),wf(:,19))
  call vert_SS_S(wf(:,17),wf(:,-5),wf(:,20))
  call vert_ST_V(wf(:,17),Q(:,10),wf(:,-5),Q(:,32),wf(:,21))
  call vert_TV_S(wf(:,-5),Q(:,32),wf(:,14),Q(:,10),wf(:,22))
  call vert_SV_V(wf(:,-5),wf(:,14),wf(:,23))
  call vert_SS_S(wf(:,17),wf(:,-4),wf(:,24))
  call vert_ST_V(wf(:,17),Q(:,10),wf(:,-4),Q(:,16),wf(:,25))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,14),Q(:,10),wf(:,26))
  call vert_SV_V(wf(:,-4),wf(:,14),wf(:,27))
  call counter_AQ_S(gPud,wf(:,0),wf(:,-2),wf(:,28))
  call vert_VS_T(wf(:,2),Q(:,10),wf(:,28),Q(:,5),wf(:,29))
  call vert_VV_S(wf(:,15),wf(:,2),wf(:,30))
  call vert_SS_S(wf(:,28),wf(:,-4),wf(:,31))
  call vert_ST_V(wf(:,28),Q(:,5),wf(:,-4),Q(:,16),wf(:,32))
  call vert_TV_S(wf(:,-4),Q(:,16),wf(:,15),Q(:,5),wf(:,33))
  call vert_SV_V(wf(:,-4),wf(:,15),wf(:,34))
  call vert_SS_S(wf(:,28),wf(:,-5),wf(:,35))
  call vert_ST_V(wf(:,28),Q(:,5),wf(:,-5),Q(:,32),wf(:,36))
  call vert_TV_S(wf(:,-5),Q(:,32),wf(:,15),Q(:,5),wf(:,37))
  call vert_SV_V(wf(:,-5),wf(:,15),wf(:,38))
  call vert_SSV_V(wf(:,-4),wf(:,-5),wf(:,2),wf(:,39))
  call vert_SV_V(wf(:,4),wf(:,1),wf(:,40))
  call vert_ST_V(wf(:,6),Q(:,21),wf(:,-5),Q(:,32),wf(:,41))
  call vert_SV_V(wf(:,-5),wf(:,8),wf(:,42))
  call vert_ST_V(wf(:,11),Q(:,37),wf(:,-4),Q(:,16),wf(:,43))
  call vert_SV_V(wf(:,-4),wf(:,13),wf(:,44))
  call vert_SV_V(wf(:,4),wf(:,2),wf(:,45))
  call vert_ST_V(wf(:,10),Q(:,26),wf(:,-5),Q(:,32),wf(:,46))
  call vert_SV_V(wf(:,-5),wf(:,12),wf(:,47))
  call vert_ST_V(wf(:,7),Q(:,42),wf(:,-4),Q(:,16),wf(:,48))
  call vert_SV_V(wf(:,-4),wf(:,9),wf(:,49))

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
  den(4) = 1 / (Q(5,48) - MH2)
  den(6) = 1 / (Q(5,21) - MW2)
  den(9) = 1 / (Q(5,26) - MW2)
  den(12) = 1 / (Q(5,53) - MW2)
  den(14) = 1 / (Q(5,58) - MW2)
  den(19) = 1 / (Q(5,37) - MW2)
  den(25) = 1 / (Q(5,42) - MW2)

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(1)*den(10)
  den(13) = den(1)*den(12)
  den(15) = den(2)*den(14)
  den(16) = den(1)*den(4)
  den(17) = den(12)*den(16)
  den(18) = den(7)*den(12)
  den(20) = den(1)*den(19)
  den(21) = den(12)*den(20)
  den(22) = den(2)*den(4)
  den(23) = den(14)*den(22)
  den(24) = den(10)*den(14)
  den(26) = den(2)*den(25)
  den(27) = den(14)*den(26)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(28)

  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_SS(wf(:,4),wf(:,5)) * den(5)
  A(3) = cont_SS(wf(:,6),wf(:,7)) * den(8)
  A(4) = cont_VV(wf(:,8),wf(:,9)) * den(8)
  A(5) = cont_SS(wf(:,10),wf(:,11)) * den(11)
  A(6) = cont_VV(wf(:,12),wf(:,13)) * den(11)

  A(7) = cont_VV(wf(:,3),wf(:,14)) * den(3)
  A(8) = cont_VV(wf(:,2),wf(:,16)) * den(3)
  A(9) = cont_SS(wf(:,4),wf(:,18)) * den(5)
  A(10) = cont_SS(wf(:,4),wf(:,19)) * den(5)
  A(11) = cont_SS(wf(:,6),wf(:,20)) * den(8)
  A(12) = cont_VV(wf(:,8),wf(:,21)) * den(8)
  A(13) = cont_SS(wf(:,6),wf(:,22)) * den(8)
  A(14) = cont_VV(wf(:,8),wf(:,23)) * den(8)
  A(15) = cont_SS(wf(:,11),wf(:,24)) * den(11)
  A(16) = cont_VV(wf(:,13),wf(:,25)) * den(11)
  A(17) = cont_SS(wf(:,11),wf(:,26)) * den(11)
  A(18) = cont_VV(wf(:,13),wf(:,27)) * den(11)
  A(19) = cont_SS(wf(:,4),wf(:,29)) * den(5)
  A(20) = cont_SS(wf(:,4),wf(:,30)) * den(5)
  A(21) = cont_SS(wf(:,7),wf(:,31)) * den(8)
  A(22) = cont_VV(wf(:,9),wf(:,32)) * den(8)
  A(23) = cont_SS(wf(:,7),wf(:,33)) * den(8)
  A(24) = cont_VV(wf(:,9),wf(:,34)) * den(8)
  A(25) = cont_SS(wf(:,10),wf(:,35)) * den(11)
  A(26) = cont_VV(wf(:,12),wf(:,36)) * den(11)
  A(27) = cont_SS(wf(:,10),wf(:,37)) * den(11)
  A(28) = cont_VV(wf(:,12),wf(:,38)) * den(11)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(28)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (A(3)+A(5))*f(1)+A(1)*f(2)+A(2)*f(9)+(A(4)+A(6))*f(16)

  M2(1) = (A(23)+A(27))*f(3)+A(8)*f(4)+(A(13)+A(17))*f(5)+A(7)*f(6)+(A(12)+A(16))*f(7)+(A(22)+A(26))*f(8)+A(20)*f(10)+A(10)*f(11) &
       +(A(11)+A(15))*f(12)+(A(21)+A(25))*f(13)+A(9)*f(14)+A(19)*f(15)+(A(24)+A(28))*f(17)+(A(14)+A(18))*f(18)

end subroutine colourvectors

end module ol_loop_pphhjj_uxcdsxhh_1_/**/REALKIND
