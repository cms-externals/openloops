
module ol_colourmatrix_ppvvj2_wwxggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(0,0), K2(0,2), KL(0,2), KL2(2,2), KL2ct(2,2), KL2ct2(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  KL2(1,:) = [  7, -2]
  KL2(2,:) = [ -2,  7]
  KL2 = (1._/**/REALKIND / 3) * KL2

  KL2ct(1,:) = [  7, -2]
  KL2ct(2,:) = [ -2,  7]
  KL2ct = (1._/**/REALKIND / 3) * KL2ct

  KL2ct2(1,:) = [  7, -2]
  KL2ct2(2,:) = [ -2,  7]
  KL2ct2 = (1._/**/REALKIND / 3) * KL2ct2

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppvvj2_wwxggg_1_/**/REALKIND



module ol_forced_parameters_ppvvj2_wwxggg_1_/**/REALKIND
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
  if (wMW /= 0) write(*,101) 'wMW = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppvvj2_wwxggg_1_/**/REALKIND

module ol_loop_ppvvj2_wwxggg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(24), c(0)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:19)
  ! denominators
  complex(REALKIND), save :: den(15)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(0,72)
  ! zero helicity identifier
  logical,           save :: zerohel(72) = .true., zerohel_ct(72) = .true.

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
    f( 1) = CI*countertermnorm*eQED**2*gQCD**3
    f( 2) = countertermnorm*ctWWGG*eQED**2*gQCD**3
    f( 3) = (CI*countertermnorm*cw*eQED**2*gQCD**3)/sw
    f( 4) = (CI*countertermnorm*ctZGG*cw*eQED**2*gQCD**3)/sw
    f( 5) = (CI*eQED**2*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f( 6) = (2*CI*eQED**2*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f( 7) = (4*CI*eQED**2*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f( 8) = (eQED**2*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f( 9) = (2*eQED**2*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(10) = (4*eQED**2*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(11) = (CI*eQED**2*gQCD**3*integralnorm*SwF)/(2._/**/REALKIND*sw**2)
    f(12) = (CI*eQED**2*gQCD**3*integralnorm*SwF)/sw**2
    f(13) = (eQED**2*gQCD**3*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(14) = (eQED**2*gQCD**3*integralnorm*SwF)/sw**2
    f(15) = (CI*cw*eQED**2*gQCD**3*integralnorm*SwF)/sw
    f(16) = (2*CI*cw*eQED**2*gQCD**3*integralnorm*SwF)/sw
    f(17) = (cw*eQED**2*gQCD**3*integralnorm*SwF)/sw
    f(18) = (2*cw*eQED**2*gQCD**3*integralnorm*SwF)/sw
    f(19) = (countertermnorm*ctHGG*eQED**2*gQCD**3*lambdaHWW*MB*MW*YB)/(MQ2sum*sw)
    f(20) = (CI*eQED**2*gQCD**3*integralnorm*lambdaHWW*SwF*YB)/(2._/**/REALKIND*sw**2)
    f(21) = (eQED**2*gQCD**3*integralnorm*lambdaHWW*SwF*YB)/(sw**2*2._/**/REALKIND)
    f(22) = (countertermnorm*ctHGG*eQED**2*gQCD**3*lambdaHWW*MT*MW*YT)/(MQ2sum*sw)
    f(23) = (CI*eQED**2*gQCD**3*integralnorm*lambdaHWW*SwF*YT)/(2._/**/REALKIND*sw**2)
    f(24) = (eQED**2*gQCD**3*integralnorm*lambdaHWW*SwF*YT)/(sw**2*2._/**/REALKIND)


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
  complex(REALKIND), intent(out) :: M1(0), M2(2)
  complex(REALKIND) :: A(16)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_V(P(:,1), rMW, H(1), wf(:,0), POLSEL(1))
    call pol_wf_V(P(:,2), rMW, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))

  else
    call pol_wf_V(P(:,1), rMW, H(1), wf(:,0), 0)
    call pol_wf_V(P(:,2), rMW, H(2), wf(:,-1), 0)
    call pol_wf_V(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_V(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)

  end if

  ! internal WFs
  call vert_UV_W(wf(:,-1),Q(:,2),wf(:,0),Q(:,1),wf(:,1))
  call counter_GGG_V(ctAGGG,wf(:,-2),wf(:,-3),wf(:,-4),wf(:,2))
  call counter_GGG_V(ctAGGG,wf(:,-2),wf(:,-4),wf(:,-3),wf(:,3))
  call counter_GGG_V(ctZGGG,wf(:,-2),wf(:,-3),wf(:,-4),wf(:,4))
  call prop_W_W(wf(:,1),Q(:,3),MZ,1_intkind1,wf(:,5))
  call counter_GGG_V(ctZGGG,wf(:,-2),wf(:,-4),wf(:,-3),wf(:,6))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-3),Q(:,8),wf(:,7))
  call counter_VVG_G(wf(:,0),wf(:,-1),wf(:,-4),wf(:,8))
  call vert_UV_W(wf(:,-2),Q(:,4),wf(:,-4),Q(:,16),wf(:,9))
  call counter_VVG_G(wf(:,0),wf(:,-1),wf(:,-3),wf(:,10))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-4),Q(:,16),wf(:,11))
  call counter_VVG_G(wf(:,0),wf(:,-1),wf(:,-2),wf(:,12))
  call vert_VV_S(wf(:,0),wf(:,-1),wf(:,13))
  call counter_SG_G(wf(:,13),wf(:,-4),wf(:,14))
  call counter_VG_G(wf(:,5),wf(:,-4),Q(:,16),wf(:,15),Q(:,19))
  call counter_SG_G(wf(:,13),wf(:,-3),wf(:,16))
  call counter_VG_G(wf(:,5),wf(:,-3),Q(:,8),wf(:,17),Q(:,11))
  call counter_SG_G(wf(:,13),wf(:,-2),wf(:,18))
  call counter_VG_G(wf(:,5),wf(:,-2),Q(:,4),wf(:,19),Q(:,7))

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
  den(2) = 1 / (Q(5,3) - MZ2)
  den(3) = 1 / (Q(5,12))
  den(4) = 1 / (Q(5,20))
  den(5) = 1 / (Q(5,24))
  den(6) = 1 / (Q(5,3) - MH2)

  ! denominators
  den(7) = den(3)*den(6)
  den(8) = den(2)*den(3)
  den(9) = den(4)*den(6)
  den(10) = den(2)*den(4)
  den(11) = den(5)*den(6)
  den(12) = den(2)*den(5)
  den(13) = den(1)*den(3)
  den(14) = den(1)*den(4)
  den(15) = den(1)*den(5)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(16)


  A(1) = cont_VV(wf(:,1),wf(:,2)) * den(1)
  A(2) = cont_VV(wf(:,1),wf(:,3)) * den(1)
  A(3) = cont_VV(wf(:,4),wf(:,5)) * den(2)
  A(4) = cont_VV(wf(:,5),wf(:,6)) * den(2)
  A(5) = cont_VV(wf(:,7),wf(:,8)) * den(3)
  A(6) = cont_VV(wf(:,9),wf(:,10)) * den(4)
  A(7) = cont_VV(wf(:,11),wf(:,12)) * den(5)
  A(8) = cont_VV(wf(:,7),wf(:,14)) * den(7)
  A(9) = cont_VV(wf(:,7),wf(:,14)) * den(7)
  A(10) = cont_VV(wf(:,7),wf(:,15)) * den(8)
  A(11) = cont_VV(wf(:,9),wf(:,16)) * den(9)
  A(12) = cont_VV(wf(:,9),wf(:,16)) * den(9)
  A(13) = cont_VV(wf(:,9),wf(:,17)) * den(10)
  A(14) = cont_VV(wf(:,11),wf(:,18)) * den(11)
  A(15) = cont_VV(wf(:,11),wf(:,18)) * den(11)
  A(16) = cont_VV(wf(:,11),wf(:,19)) * den(12)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(16)
  complex(REALKIND), intent(out) :: M1(0), M2(2)


  M2(1) = -(A(1)*f(1))+2*CI*(A(5)-A(6)+A(7))*f(2)+A(3)*f(3)+2*CI*(A(10)-A(13)+A(16))*f(4)+2*CI*(-A(9)+A(12)-A(15))*f(19)+2*CI*( &
       -A(8)+A(11)-A(14))*f(22)
  M2(2) = -(A(2)*f(1))+2*CI*(-A(5)+A(6)-A(7))*f(2)+A(4)*f(3)+2*CI*(-A(10)+A(13)-A(16))*f(4)+2*CI*(A(9)-A(12)+A(15))*f(19) &
       +2*CI*(A(8)-A(11)+A(14))*f(22)

end subroutine colourvectors

end module ol_loop_ppvvj2_wwxggg_1_/**/REALKIND
