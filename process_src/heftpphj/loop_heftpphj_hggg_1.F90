
module ol_colourmatrix_heftpphj_hggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(24,2), K2(2,2), KL(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  14,  -4]
  K1( 2,:) = [  -4,  14]
  K1( 3,:) = [   0,   0]
  K1( 4,:) = [   0,   0]
  K1( 5,:) = [   0,   0]
  K1( 6,:) = [   0,   0]
  K1( 7,:) = [  42, -12]
  K1( 8,:) = [ -12,  42]
  K1( 9,:) = [   0,   0]
  K1(10,:) = [   0,   0]
  K1(11,:) = [ -21,   6]
  K1(12,:) = [   6, -21]
  K1(13,:) = [  42, -12]
  K1(14,:) = [ -12,  42]
  K1(15,:) = [   0,   0]
  K1(16,:) = [   0,   0]
  K1(17,:) = [ -21,   6]
  K1(18,:) = [   6, -21]
  K1(19,:) = [ -21,   6]
  K1(20,:) = [   6, -21]
  K1(21,:) = [  42, -12]
  K1(22,:) = [ -12,  42]
  K1(23,:) = [   0,   0]
  K1(24,:) = [   0,   0]
  K1 = (1._/**/REALKIND / 6) * K1

  K2(1,:) = [  7, -2]
  K2(2,:) = [ -2,  7]
  K2 = (1._/**/REALKIND / 3) * K2

  KL(1,:) = [  7, -2]
  KL(2,:) = [ -2,  7]
  KL = (1._/**/REALKIND / 3) * KL

  end subroutine colourmatrix_init
end module ol_colourmatrix_heftpphj_hggg_1_/**/REALKIND



module ol_forced_parameters_heftpphj_hggg_1_/**/REALKIND
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
end module ol_forced_parameters_heftpphj_hggg_1_/**/REALKIND

module ol_loop_heftpphj_hggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(12), c(10)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-4+1:17)
  ! denominators
  complex(REALKIND), save :: den(9)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,8)
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
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB
!  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (eQED*gQCD**3)/(MW*pi**2*sw*24._/**/REALKIND)
    f( 2) = (countertermnorm*eQED*gQCD**5)/(MW*pi**2*sw*24._/**/REALKIND)
    f( 3) = (countertermnorm*ctHEFTgggh*eQED*gQCD**5)/(MW*pi**2*sw*24._/**/REALKIND)
    f( 4) = (countertermnorm*ctVVV*eQED*gQCD**5)/(MW*pi**2*sw*24._/**/REALKIND)
    f( 5) = (CI*eQED*gQCD**5*integralnorm*SwB)/(48._/**/REALKIND*MW*pi**2*sw)
    f( 6) = (CI*eQED*gQCD**5*integralnorm*SwB)/(24._/**/REALKIND*MW*pi**2*sw)
    f( 7) = (CI*eQED*gQCD**3*integralnorm*MB*SwF)/(2._/**/REALKIND*MW*sw)
    f( 8) = (eQED*gQCD**3*integralnorm*MB*SwF)/(MW*sw*2._/**/REALKIND)
    f( 9) = (CI*eQED*gQCD**5*integralnorm*SwF)/(24._/**/REALKIND*MW*pi**2*sw)
    f(10) = (CI*eQED*gQCD**5*integralnorm*SwF)/(12._/**/REALKIND*MW*pi**2*sw)
    f(11) = (eQED*gQCD**5*integralnorm*SwF)/(MW*pi**2*sw*24._/**/REALKIND)
    f(12) = (eQED*gQCD**5*integralnorm*SwF)/(MW*pi**2*sw*12._/**/REALKIND)

  c = [ 3*CI*f(5), 6*CI*f(5), 3*CI*f(6), 6*CI*f(6), CI*f(7), f(8), CI*f(9), CI*f(10), f(11), f(12) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,4)
  integer,           intent(in)  :: H(4)
  integer,           intent(in), optional  :: POLSEL(4)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(14)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_S(P(:,1), rMH, H(1), wf(:,0), POLSEL(1))
    call pol_wf_V(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))

  else
    call pol_wf_S(P(:,1), rMH, H(1), wf(:,0), 0)
    call pol_wf_V(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), 0)

  end if

  ! internal WFs
  call vert_HGG_G(wf(:,0),wf(:,-1),Q(:,2),wf(:,-2),Q(:,4),wf(:,1),Q(:,7))
  call vert_HG_G(wf(:,0),wf(:,-1),Q(:,2),wf(:,2),Q(:,3))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,3))
  call vert_HG_G(wf(:,0),wf(:,-2),Q(:,4),wf(:,4),Q(:,5))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,-3),Q(:,8),wf(:,5))
  call vert_HG_G(wf(:,0),wf(:,-3),Q(:,8),wf(:,6),Q(:,9))
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,-2),Q(:,4),wf(:,7))
  call counter_HGG_G(wf(:,0),wf(:,-1),Q(:,2),wf(:,-2),Q(:,4),wf(:,8),Q(:,7))
  call counter_UV_W(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,9))
  call counter_UV_W(wf(:,-1),Q(:,2),wf(:,-3),Q(:,8),wf(:,10))
  call counter_UV_W(wf(:,-1),Q(:,2),wf(:,-2),Q(:,4),wf(:,11))
  call counter_HG_G(ctHEFTggh,wf(:,0),wf(:,-3),Q(:,8),wf(:,12),Q(:,9))
  call counter_HG_G(ctHEFTggh,wf(:,0),wf(:,-2),Q(:,4),wf(:,13),Q(:,5))
  call counter_HG_G(ctHEFTggh,wf(:,0),wf(:,-1),Q(:,2),wf(:,14),Q(:,3))
  call counter_V_V(ctGG,wf(:,2),Q(:,3),wf(:,15))
  call counter_V_V(ctGG,wf(:,4),Q(:,5),wf(:,16))
  call counter_V_V(ctGG,wf(:,6),Q(:,9),wf(:,17))

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
  den(2) = 1 / (Q(5,5))
  den(3) = 1 / (Q(5,9))
  den(4) = 1 / (Q(5,6))
  den(5) = 1 / (Q(5,10))
  den(6) = 1 / (Q(5,12))

  ! denominators
  den(7) = den(1)*den(6)
  den(8) = den(2)*den(5)
  den(9) = den(3)*den(4)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(14)

  A(1) = cont_VV(wf(:,-3),wf(:,1))
  A(2) = cont_VV(wf(:,2),wf(:,3)) * den(1)
  A(3) = cont_VV(wf(:,4),wf(:,5)) * den(2)
  A(4) = cont_VV(wf(:,6),wf(:,7)) * den(3)

  A(5) = cont_VV(wf(:,-3),wf(:,8))
  A(6) = cont_VV(wf(:,2),wf(:,9)) * den(1)
  A(7) = cont_VV(wf(:,4),wf(:,10)) * den(2)
  A(8) = cont_VV(wf(:,6),wf(:,11)) * den(3)
  A(9) = cont_VV(wf(:,7),wf(:,12)) * den(4)
  A(10) = cont_VV(wf(:,5),wf(:,13)) * den(5)
  A(11) = cont_VV(wf(:,3),wf(:,14)) * den(6)
  A(12) = cont_VV(wf(:,3),wf(:,15)) * den(7)
  A(13) = cont_VV(wf(:,5),wf(:,16)) * den(8)
  A(14) = cont_VV(wf(:,7),wf(:,17)) * den(9)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(14)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = 2*CI*(-A(1)-A(2)+A(3)-A(4))*f(1)
  M1(2) = 2*CI*(A(1)+A(2)-A(3)+A(4))*f(1)

  M2(1) = 2*CI*(-A(9)+A(10)-A(11)+A(12)-A(13)+A(14))*f(2)-2*CI*A(5)*f(3)+2*CI*(-A(6)+A(7)-A(8))*f(4)
  M2(2) = 2*CI*(A(9)-A(10)+A(11)-A(12)+A(13)-A(14))*f(2)+2*CI*A(5)*f(3)+2*CI*(A(6)-A(7)+A(8))*f(4)

end subroutine colourvectors

end module ol_loop_heftpphj_hggg_1_/**/REALKIND
