
module ol_colourmatrix_ppwwj_ddxwwxg_1_/**/REALKIND
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
end module ol_colourmatrix_ppwwj_ddxwwxg_1_/**/REALKIND



module ol_forced_parameters_ppwwj_ddxwwxg_1_/**/REALKIND
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
  if (wMW /= 0) write(*,101) 'wMW = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppwwj_ddxwwxg_1_/**/REALKIND

module ol_loop_ppwwj_ddxwwxg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(30), c(18)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-5+1:65)
  ! denominators
  complex(REALKIND), save :: den(42)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,72)
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
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*eQED**2*gQCD)/3._/**/REALKIND
    f( 2) = (CI*countertermnorm*eQED**2*gQCD**3)/3._/**/REALKIND
    f( 3) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**3)/3._/**/REALKIND
    f( 4) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**3)/3._/**/REALKIND
    f( 5) = CI*countertermnorm*ctWWGG*eQED**2*gQCD**3
    f( 6) = (CI*eQED**2*gQCD)/(2._/**/REALKIND*sw**2)
    f( 7) = (CI*countertermnorm*eQED**2*gQCD**3)/(2._/**/REALKIND*sw**2)
    f( 8) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**3)/(2._/**/REALKIND*sw**2)
    f( 9) = (CI*countertermnorm*ctVqq*eQED**2*gQCD**3)/(2._/**/REALKIND*sw**2)
    f(10) = (CI*cw*eQED**2*gQCD)/sw
    f(11) = (CI*countertermnorm*cw*eQED**2*gQCD**3)/sw
    f(12) = (CI*countertermnorm*ctGqq*cw*eQED**2*gQCD**3)/sw
    f(13) = (CI*countertermnorm*ctVqq*cw*eQED**2*gQCD**3)/sw
    f(14) = (countertermnorm*ctZGG*cw*eQED**2*gQCD**3)/sw
    f(15) = (CI*countertermnorm*ctHGG*eQED**2*gQCD**3*MW)/sw
    f(16) = (CI*eQED**2*gQCD**3*integralnorm*SwB)/3._/**/REALKIND
    f(17) = (eQED**2*gQCD**3*integralnorm*SwB)/3._/**/REALKIND
    f(18) = (CI*eQED**2*gQCD**3*integralnorm*SwB)/(2._/**/REALKIND*sw**2)
    f(19) = (eQED**2*gQCD**3*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(20) = (CI*cw*eQED**2*gQCD**3*integralnorm*SwB)/sw
    f(21) = (cw*eQED**2*gQCD**3*integralnorm*SwB)/sw
    f(22) = (eQED**2*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(23) = (2*eQED**2*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(24) = (4*eQED**2*gQCD**3*integralnorm*SwF)/3._/**/REALKIND
    f(25) = (eQED**2*gQCD**3*integralnorm*SwF)/(sw**2*2._/**/REALKIND)
    f(26) = (eQED**2*gQCD**3*integralnorm*SwF)/sw**2
    f(27) = (eQED**2*gQCD**3*integralnorm*MB*SwF)/(sw**2*2._/**/REALKIND)
    f(28) = (eQED**2*gQCD**3*integralnorm*MT*SwF)/(sw**2*2._/**/REALKIND)
    f(29) = (cw*eQED**2*gQCD**3*integralnorm*SwF)/sw
    f(30) = (2*cw*eQED**2*gQCD**3*integralnorm*SwF)/sw

  c = [ 9*CI*f(16), f(17), 8*f(17), 9*CI*f(18), f(19), 8*f(19), 9*CI*f(20), f(21), 8*f(21), 3*f(22), 3*f(23), 3*f(24), 3*f(25) &
    , 3*f(26), 3*f(27), 3*f(28), 3*f(29), 3*f(30) ]
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
  complex(REALKIND) :: A(37)
  ! external WFs
  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_V(P(:,3), rMW, H(3), wf(:,-2))
  call wf_V(P(:,4), rMW, H(4), wf(:,-3))
  call wf_V(P(:,5), rZERO, H(5), wf(:,-4))

  ! internal WFs
  call vert_WQ_A(wf(:,-3),wf(:,0),wf(:,1))
  call vert_AW_Q(wf(:,-1),wf(:,-2),wf(:,2))
  call prop_Q_A(wf(:,1),Q(:,9),ZERO,0_intkind1,wf(:,3))
  call prop_A_Q(wf(:,2),Q(:,6),ZERO,0_intkind1,wf(:,4))
  call vert_VQ_A(wf(:,-4),wf(:,3),wf(:,5))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,6))
  call prop_Q_A(wf(:,6),Q(:,17),ZERO,0_intkind1,wf(:,7))
  call vert_WQ_A(wf(:,-3),wf(:,7),wf(:,8))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,9))
  call prop_A_Q(wf(:,9),Q(:,18),ZERO,0_intkind1,wf(:,10))
  call vert_WQ_A(wf(:,-2),wf(:,3),wf(:,11))
  call vert_UV_W(wf(:,-3),Q(:,8),wf(:,-2),Q(:,4),wf(:,12))
  call vert_QA_V(wf(:,7),wf(:,-1),wf(:,13))
  call prop_W_W(wf(:,12),Q(:,12),MZ,1_intkind1,wf(:,14))
  call vert_QA_Z(gZd,wf(:,7),wf(:,-1),wf(:,15))
  call vert_QA_V(wf(:,0),wf(:,10),wf(:,16))
  call vert_QA_Z(gZd,wf(:,0),wf(:,10),wf(:,17))
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,18))
  call counter_VVG_G(wf(:,-2),wf(:,-3),wf(:,-4),wf(:,19))
  call vert_VV_S(wf(:,-2),wf(:,-3),wf(:,20))
  call counter_GG_S(wf(:,18),wf(:,-4),wf(:,21))
  call counter_GG_V(wf(:,18),Q(:,3),wf(:,-4),Q(:,16),wf(:,22))
  call counter_VQ_A(wf(:,-4),wf(:,3),wf(:,23))
  call counter_WQ_A(wf(:,-3),wf(:,7),wf(:,24))
  call counter_WQ_A(wf(:,-2),wf(:,3),wf(:,25))
  call counter_AV_Q(wf(:,-1),wf(:,-4),wf(:,26))
  call prop_A_Q(wf(:,26),Q(:,18),ZERO,0_intkind1,wf(:,27))
  call counter_QA_V(wf(:,7),wf(:,-1),wf(:,28))
  call counter_QA_Z(gZd,wf(:,7),wf(:,-1),wf(:,29))
  call vert_QA_V(wf(:,0),wf(:,27),wf(:,30))
  call vert_QA_Z(gZd,wf(:,0),wf(:,27),wf(:,31))
  call counter_AW_Q(wf(:,-1),wf(:,-2),wf(:,32))
  call prop_A_Q(wf(:,32),Q(:,6),ZERO,0_intkind1,wf(:,33))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,34))
  call prop_Q_A(wf(:,34),Q(:,17),ZERO,0_intkind1,wf(:,35))
  call vert_WQ_A(wf(:,-3),wf(:,35),wf(:,36))
  call counter_WQ_A(wf(:,-3),wf(:,0),wf(:,37))
  call prop_Q_A(wf(:,37),Q(:,9),ZERO,0_intkind1,wf(:,38))
  call vert_VQ_A(wf(:,-4),wf(:,38),wf(:,39))
  call counter_QA_V(wf(:,0),wf(:,10),wf(:,40))
  call counter_QA_Z(gZd,wf(:,0),wf(:,10),wf(:,41))
  call vert_QA_V(wf(:,35),wf(:,-1),wf(:,42))
  call vert_QA_Z(gZd,wf(:,35),wf(:,-1),wf(:,43))
  call vert_WQ_A(wf(:,-2),wf(:,38),wf(:,44))
  call vert_AV_Q(wf(:,4),wf(:,-4),wf(:,45))
  call counter_Q_A(ctqq,wf(:,3),Q(:,9),wf(:,46))
  call prop_A_Q(wf(:,45),Q(:,22),ZERO,0_intkind1,wf(:,47))
  call counter_A_Q(ctqq,wf(:,4),Q(:,6),wf(:,48))
  call prop_Q_A(wf(:,5),Q(:,25),ZERO,0_intkind1,wf(:,49))
  call vert_AW_Q(wf(:,4),wf(:,-3),wf(:,50))
  call counter_Q_A(ctqq,wf(:,7),Q(:,17),wf(:,51))
  call prop_A_Q(wf(:,50),Q(:,14),ZERO,0_intkind1,wf(:,52))
  call prop_Q_A(wf(:,8),Q(:,25),ZERO,0_intkind1,wf(:,53))
  call vert_AW_Q(wf(:,10),wf(:,-2),wf(:,54))
  call prop_A_Q(wf(:,54),Q(:,22),ZERO,0_intkind1,wf(:,55))
  call counter_A_Q(ctqq,wf(:,10),Q(:,18),wf(:,56))
  call prop_Q_A(wf(:,11),Q(:,13),ZERO,0_intkind1,wf(:,57))
  call vert_AV_Q(wf(:,-1),wf(:,12),wf(:,58))
  call prop_A_Q(wf(:,58),Q(:,14),ZERO,0_intkind1,wf(:,59))
  call vert_AZ_Q(gZd,wf(:,-1),wf(:,14),wf(:,60))
  call prop_A_Q(wf(:,60),Q(:,14),ZERO,0_intkind1,wf(:,61))
  call vert_VQ_A(wf(:,12),wf(:,0),wf(:,62))
  call prop_Q_A(wf(:,62),Q(:,13),ZERO,0_intkind1,wf(:,63))
  call vert_ZQ_A(gZd,wf(:,14),wf(:,0),wf(:,64))
  call prop_Q_A(wf(:,64),Q(:,13),ZERO,0_intkind1,wf(:,65))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,9))
  den(2) = 1 / (Q(5,6))
  den(4) = 1 / (Q(5,17))
  den(6) = 1 / (Q(5,18))
  den(8) = 1 / (Q(5,12))
  den(10) = 1 / (Q(5,12) - MZ2)
  den(14) = 1 / (Q(5,3))
  den(15) = 1 / (Q(5,12) - MH2)
  den(18) = 1 / (Q(5,22))
  den(21) = 1 / (Q(5,25))
  den(24) = 1 / (Q(5,14))
  den(31) = 1 / (Q(5,13))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(2)*den(4)
  den(7) = den(1)*den(6)
  den(9) = den(4)*den(8)
  den(11) = den(4)*den(10)
  den(12) = den(6)*den(8)
  den(13) = den(6)*den(10)
  den(16) = den(14)*den(15)
  den(17) = den(10)*den(14)
  den(19) = den(2)*den(18)
  den(20) = den(1)*den(19)
  den(22) = den(1)*den(21)
  den(23) = den(2)*den(22)
  den(25) = den(2)*den(24)
  den(26) = den(4)*den(25)
  den(27) = den(4)*den(21)
  den(28) = den(2)*den(27)
  den(29) = den(6)*den(18)
  den(30) = den(1)*den(29)
  den(32) = den(1)*den(31)
  den(33) = den(6)*den(32)
  den(34) = den(8)*den(24)
  den(35) = den(4)*den(34)
  den(36) = den(10)*den(24)
  den(37) = den(4)*den(36)
  den(38) = den(8)*den(31)
  den(39) = den(6)*den(38)
  den(40) = den(10)*den(31)
  den(41) = den(6)*den(40)
  den(42) = den(8)*den(14)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(37)

  A(1) = cont_QA(wf(:,4),wf(:,5)) * den(3)
  A(2) = cont_QA(wf(:,4),wf(:,8)) * den(5)
  A(3) = cont_QA(wf(:,10),wf(:,11)) * den(7)
  A(4) = cont_VV(wf(:,12),wf(:,13)) * den(9)
  A(5) = cont_VV(wf(:,14),wf(:,15)) * den(11)
  A(6) = cont_VV(wf(:,12),wf(:,16)) * den(12)
  A(7) = cont_VV(wf(:,14),wf(:,17)) * den(13)

  A(8) = cont_VV(wf(:,18),wf(:,19)) * den(14)
  A(9) = cont_SS(wf(:,20),wf(:,21)) * den(16)
  A(10) = cont_VV(wf(:,14),wf(:,22)) * den(17)
  A(11) = cont_QA(wf(:,4),wf(:,23)) * den(3)
  A(12) = cont_QA(wf(:,4),wf(:,24)) * den(5)
  A(13) = cont_QA(wf(:,10),wf(:,25)) * den(7)
  A(14) = cont_QA(wf(:,11),wf(:,27)) * den(7)
  A(15) = cont_VV(wf(:,12),wf(:,28)) * den(9)
  A(16) = cont_VV(wf(:,14),wf(:,29)) * den(11)
  A(17) = cont_VV(wf(:,12),wf(:,30)) * den(12)
  A(18) = cont_VV(wf(:,14),wf(:,31)) * den(13)
  A(19) = cont_QA(wf(:,5),wf(:,33)) * den(3)
  A(20) = cont_QA(wf(:,8),wf(:,33)) * den(5)
  A(21) = cont_QA(wf(:,4),wf(:,36)) * den(5)
  A(22) = cont_QA(wf(:,4),wf(:,39)) * den(3)
  A(23) = cont_VV(wf(:,12),wf(:,40)) * den(12)
  A(24) = cont_VV(wf(:,14),wf(:,41)) * den(13)
  A(25) = cont_VV(wf(:,12),wf(:,42)) * den(9)
  A(26) = cont_VV(wf(:,14),wf(:,43)) * den(11)
  A(27) = cont_QA(wf(:,10),wf(:,44)) * den(7)
  A(28) = cont_QA(wf(:,46),wf(:,47)) * den(20)
  A(29) = cont_QA(wf(:,48),wf(:,49)) * den(23)
  A(30) = cont_QA(wf(:,51),wf(:,52)) * den(26)
  A(31) = cont_QA(wf(:,48),wf(:,53)) * den(28)
  A(32) = cont_QA(wf(:,46),wf(:,55)) * den(30)
  A(33) = cont_QA(wf(:,56),wf(:,57)) * den(33)
  A(34) = cont_QA(wf(:,51),wf(:,59)) * den(35)
  A(35) = cont_QA(wf(:,51),wf(:,61)) * den(37)
  A(36) = cont_QA(wf(:,56),wf(:,63)) * den(39)
  A(37) = cont_QA(wf(:,56),wf(:,65)) * den(41)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(37)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(4)-A(6))*f(1)+(A(1)+A(2)+A(3))*f(6)+(A(5)+A(7))*f(10)

  M2(1) = (A(34)+A(36))*f(2)+(-A(17)-A(25))*f(3)+(-A(15)-A(23))*f(4)+A(8)*f(5)+(-A(28)-A(29)-A(30)-A(31)-A(32)-A(33))*f(7)+(A(11) &
       +A(14)+A(21))*f(8)+(A(12)+A(13)+A(19)+A(20)+A(22)+A(27))*f(9)+(-A(35)-A(37))*f(11)+(A(18)+A(26))*f(12)+(A(16)+A(24))*f(13) &
       -A(10)*f(14)-A(9)*f(15)

end subroutine colourvectors

end module ol_loop_ppwwj_ddxwwxg_1_/**/REALKIND
