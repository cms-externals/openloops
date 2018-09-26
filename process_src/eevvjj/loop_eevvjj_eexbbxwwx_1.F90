
module ol_colourmatrix_eevvjj_eexbbxwwx_1_/**/REALKIND
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
  K1(18,:) = [  0]
  K1(19,:) = [  0]
  K1(20,:) = [  0]
  K1(21,:) = [  0]
  K1(22,:) = [  0]
  K1(23,:) = [  0]

  K2(1,:) = [ 3]

  KL(1,:) = [ 3]

  end subroutine colourmatrix_init
end module ol_colourmatrix_eevvjj_eexbbxwwx_1_/**/REALKIND



module ol_forced_parameters_eevvjj_eexbbxwwx_1_/**/REALKIND
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
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'
  if (wMW /= 0) write(*,101) 'wMW = 0'


    checks_not_written = .false.
    end if

    101 format('[OpenLoops] === WARNING ===',/,'[OpenLoops] code was generated with ',A,/,'[OpenLoops] ===============')
  end subroutine check_forced_parameters
end module ol_forced_parameters_eevvjj_eexbbxwwx_1_/**/REALKIND

module ol_loop_eevvjj_eexbbxwwx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(69), c(17)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:228)
  ! denominators
  complex(REALKIND), save :: den(250)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(1,144)
  ! zero helicity identifier
  logical,           save :: zerohel(144) = .true., zerohel_ct(144) = .true.

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
    f( 1) = (CI*eQED**4)/9._/**/REALKIND
    f( 2) = (CI*eQED**4)/3._/**/REALKIND
    f( 3) = CI*eQED**4
    f( 4) = (CI*countertermnorm*eQED**4*gQCD**2)/9._/**/REALKIND
    f( 5) = (CI*countertermnorm*eQED**4*gQCD**2)/3._/**/REALKIND
    f( 6) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2)/9._/**/REALKIND
    f( 7) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2)/3._/**/REALKIND
    f( 8) = CI*countertermnorm*ctVbb*eQED**4*gQCD**2
    f( 9) = (CI*eQED**4)/(4._/**/REALKIND*sw**4)
    f(10) = (CI*countertermnorm*eQED**4*gQCD**2)/(4._/**/REALKIND*sw**4)
    f(11) = (CI*countertermnorm*ctVbt*eQED**4*gQCD**2)/(4._/**/REALKIND*sw**4)
    f(12) = (CI*eQED**4*MB)/(4._/**/REALKIND*sw**4)
    f(13) = (CI*countertermnorm*ctSbb*eQED**4*gQCD**2*MB)/(4._/**/REALKIND*sw**4)
    f(14) = (CI*cw*eQED**4)/(2._/**/REALKIND*sw**3)
    f(15) = (CI*countertermnorm*cw*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**3)
    f(16) = (CI*countertermnorm*ctVbb*cw*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**3)
    f(17) = (CI*countertermnorm*ctVbt*cw*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**3)
    f(18) = (CI*eQED**4*MB)/(2._/**/REALKIND*cw*sw**3)
    f(19) = (CI*cw*eQED**4*MB)/(2._/**/REALKIND*sw**3)
    f(20) = (CI*countertermnorm*ctSbb*eQED**4*gQCD**2*MB)/(2._/**/REALKIND*cw*sw**3)
    f(21) = (CI*countertermnorm*ctSbb*cw*eQED**4*gQCD**2*MB)/(2._/**/REALKIND*sw**3)
    f(22) = (CI*eQED**4)/(6._/**/REALKIND*sw**2)
    f(23) = (CI*eQED**4)/(3._/**/REALKIND*sw**2)
    f(24) = (CI*eQED**4)/(2._/**/REALKIND*sw**2)
    f(25) = (CI*cw**2*eQED**4)/sw**2
    f(26) = (CI*countertermnorm*eQED**4*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(27) = (CI*countertermnorm*eQED**4*gQCD**2)/(3._/**/REALKIND*sw**2)
    f(28) = (CI*countertermnorm*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(29) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(30) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(31) = (CI*countertermnorm*ctVbt*eQED**4*gQCD**2)/(6._/**/REALKIND*sw**2)
    f(32) = (CI*countertermnorm*ctVbt*eQED**4*gQCD**2)/(3._/**/REALKIND*sw**2)
    f(33) = (CI*countertermnorm*ctVbt*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(34) = (CI*countertermnorm*ctVtt*eQED**4*gQCD**2)/(3._/**/REALKIND*sw**2)
    f(35) = (CI*countertermnorm*ctVtt*eQED**4*gQCD**2)/(2._/**/REALKIND*sw**2)
    f(36) = (CI*countertermnorm*ctVbb*cw**2*eQED**4*gQCD**2)/sw**2
    f(37) = (CI*eQED**4*MB)/(6._/**/REALKIND*sw**2)
    f(38) = (CI*eQED**4*MB)/(2._/**/REALKIND*sw**2)
    f(39) = (CI*countertermnorm*eQED**4*gQCD**2*MB)/(6._/**/REALKIND*sw**2)
    f(40) = (CI*countertermnorm*eQED**4*gQCD**2*MB)/(2._/**/REALKIND*sw**2)
    f(41) = (CI*countertermnorm*ctSbb*eQED**4*gQCD**2*MB)/(6._/**/REALKIND*sw**2)
    f(42) = (CI*countertermnorm*ctSbb*eQED**4*gQCD**2*MB)/(2._/**/REALKIND*sw**2)
    f(43) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2*MB)/(6._/**/REALKIND*sw**2)
    f(44) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2*MB)/(2._/**/REALKIND*sw**2)
    f(45) = (CI*eQED**4*MW**2)/(cw**2*sw**2)
    f(46) = (CI*countertermnorm*ctVbb*eQED**4*gQCD**2*MW**2)/(cw**2*sw**2)
    f(47) = (CI*cw*eQED**4)/(3._/**/REALKIND*sw)
    f(48) = (CI*cw*eQED**4)/sw
    f(49) = (CI*countertermnorm*cw*eQED**4*gQCD**2)/(3._/**/REALKIND*sw)
    f(50) = (CI*countertermnorm*cw*eQED**4*gQCD**2)/sw
    f(51) = (CI*countertermnorm*ctVbb*cw*eQED**4*gQCD**2)/(3._/**/REALKIND*sw)
    f(52) = (CI*countertermnorm*ctVbb*cw*eQED**4*gQCD**2)/sw
    f(53) = (eQED**4*gQCD**2*integralnorm*SwB)/9._/**/REALKIND
    f(54) = (eQED**4*gQCD**2*integralnorm*SwB)/3._/**/REALKIND
    f(55) = eQED**4*gQCD**2*integralnorm*SwB
    f(56) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**4*4._/**/REALKIND)
    f(57) = (eQED**4*gQCD**2*integralnorm*MB*SwB)/(sw**4*4._/**/REALKIND)
    f(58) = (cw*eQED**4*gQCD**2*integralnorm*SwB)/(sw**3*2._/**/REALKIND)
    f(59) = (eQED**4*gQCD**2*integralnorm*MB*SwB)/(cw*sw**3*2._/**/REALKIND)
    f(60) = (cw*eQED**4*gQCD**2*integralnorm*MB*SwB)/(sw**3*2._/**/REALKIND)
    f(61) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*6._/**/REALKIND)
    f(62) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*3._/**/REALKIND)
    f(63) = (eQED**4*gQCD**2*integralnorm*SwB)/(sw**2*2._/**/REALKIND)
    f(64) = (cw**2*eQED**4*gQCD**2*integralnorm*SwB)/sw**2
    f(65) = (eQED**4*gQCD**2*integralnorm*MB*SwB)/(sw**2*6._/**/REALKIND)
    f(66) = (eQED**4*gQCD**2*integralnorm*MB*SwB)/(sw**2*2._/**/REALKIND)
    f(67) = (eQED**4*gQCD**2*integralnorm*MW**2*SwB)/(cw**2*sw**2)
    f(68) = (cw*eQED**4*gQCD**2*integralnorm*SwB)/(sw*3._/**/REALKIND)
    f(69) = (cw*eQED**4*gQCD**2*integralnorm*SwB)/sw

  c = [ 4*f(53), 4*f(54), 4*f(55), 4*f(56), 4*f(57), 4*f(58), 4*f(59), 4*f(60), 4*f(61), 4*f(62), 4*f(63), 4*f(64), 4*f(65) &
    , 4*f(66), 4*f(67), 4*f(68), 4*f(69) ]
  c = (1._/**/REALKIND / 3) * c
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
  complex(REALKIND) :: A(182)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rMB, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rMB, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rMW, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_V(P(:,6), rMW, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rMB, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rMB, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rMW, H(5), wf(:,-4), 0)
    call pol_wf_V(P(:,6), rMW, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_WWV_V(wf(:,-4),wf(:,-5),wf(:,1),wf(:,3))
  call vert_QA_Z(gZd,wf(:,-2),wf(:,-3),wf(:,4))
  call prop_W_W(wf(:,4),Q(:,12),MZ,1_intkind1,wf(:,5))
  call vert_QA_Z(gZl,wf(:,0),wf(:,-1),wf(:,6))
  call prop_W_W(wf(:,6),Q(:,3),MZ,1_intkind1,wf(:,7))
  call vert_WWV_V(wf(:,-4),wf(:,-5),wf(:,7),wf(:,8))
  call vert_AQ_S(gH,wf(:,-3),wf(:,-2),wf(:,9))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,-4),Q(:,16),wf(:,10))
  call prop_W_W(wf(:,10),Q(:,48),MZ,1_intkind1,wf(:,11))
  call vert_SV_V(wf(:,9),wf(:,7),wf(:,12))
  call vert_VV_S(wf(:,-4),wf(:,-5),wf(:,13))
  call vert_VV_S(wf(:,7),wf(:,5),wf(:,14))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,1),Q(:,3),wf(:,15))
  call vert_SV_V(wf(:,9),wf(:,-5),wf(:,16))
  call prop_W_W(wf(:,15),Q(:,19),MW,1_intkind1,wf(:,17))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,7),Q(:,3),wf(:,18))
  call prop_W_W(wf(:,18),Q(:,19),MW,1_intkind1,wf(:,19))
  call vert_UV_W(wf(:,2),Q(:,12),wf(:,-5),Q(:,32),wf(:,20))
  call vert_UV_W(wf(:,5),Q(:,12),wf(:,-5),Q(:,32),wf(:,21))
  call vert_SV_V(wf(:,9),wf(:,-4),wf(:,22))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,23))
  call prop_W_W(wf(:,22),Q(:,28),MW,1_intkind1,wf(:,24))
  call vert_UV_W(wf(:,7),Q(:,3),wf(:,-5),Q(:,32),wf(:,25))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,2),Q(:,12),wf(:,26))
  call prop_W_W(wf(:,26),Q(:,28),MW,1_intkind1,wf(:,27))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,5),Q(:,12),wf(:,28))
  call prop_W_W(wf(:,28),Q(:,28),MW,1_intkind1,wf(:,29))
  call vert_WQ_A(wf(:,-5),wf(:,-2),wf(:,30))
  call vert_AW_Q(wf(:,-3),wf(:,-4),wf(:,31))
  call prop_Q_A(wf(:,30),Q(:,36),MT,1_intkind1,wf(:,32))
  call prop_A_Q(wf(:,31),Q(:,24),MT,1_intkind1,wf(:,33))
  call vert_VQ_A(wf(:,1),wf(:,32),wf(:,34))
  call vert_ZQ_A(gZu,wf(:,7),wf(:,32),wf(:,35))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,36))
  call vert_AW_Q(wf(:,33),wf(:,-5),wf(:,37))
  call prop_Q_A(wf(:,36),Q(:,7),MB,1_intkind1,wf(:,38))
  call vert_ZQ_A(gZd,wf(:,7),wf(:,-2),wf(:,39))
  call prop_Q_A(wf(:,39),Q(:,7),MB,1_intkind1,wf(:,40))
  call vert_QA_W(wf(:,-2),wf(:,33),wf(:,41))
  call prop_W_W(wf(:,41),Q(:,28),MW,1_intkind1,wf(:,42))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,43))
  call vert_WQ_A(wf(:,-4),wf(:,32),wf(:,44))
  call prop_A_Q(wf(:,43),Q(:,11),MB,1_intkind1,wf(:,45))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,7),wf(:,46))
  call prop_A_Q(wf(:,46),Q(:,11),MB,1_intkind1,wf(:,47))
  call vert_QA_W(wf(:,32),wf(:,-3),wf(:,48))
  call prop_W_W(wf(:,48),Q(:,44),MW,1_intkind1,wf(:,49))
  call vert_SA_Q(gH,wf(:,13),wf(:,-3),wf(:,50))
  call vert_AV_Q(wf(:,-3),wf(:,10),wf(:,51))
  call vert_AZ_Q(gZd,wf(:,-3),wf(:,11),wf(:,52))
  call vert_QS_A(gH,wf(:,-2),wf(:,13),wf(:,53))
  call prop_Q_A(wf(:,53),Q(:,52),MB,1_intkind1,wf(:,54))
  call vert_VQ_A(wf(:,10),wf(:,-2),wf(:,55))
  call prop_Q_A(wf(:,55),Q(:,52),MB,1_intkind1,wf(:,56))
  call vert_ZQ_A(gZd,wf(:,11),wf(:,-2),wf(:,57))
  call prop_Q_A(wf(:,57),Q(:,52),MB,1_intkind1,wf(:,58))
  call vert_WQ_A(wf(:,-5),wf(:,0),wf(:,59))
  call vert_AW_Q(wf(:,-1),wf(:,-4),wf(:,60))
  call prop_Q_A(wf(:,59),Q(:,33),ZERO,0_intkind1,wf(:,61))
  call prop_A_Q(wf(:,60),Q(:,18),ZERO,0_intkind1,wf(:,62))
  call vert_QA_Z(gZn,wf(:,61),wf(:,62),wf(:,63))
  call vert_QA_W(wf(:,0),wf(:,62),wf(:,64))
  call prop_W_W(wf(:,64),Q(:,19),MW,1_intkind1,wf(:,65))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,66))
  call vert_AW_Q(wf(:,62),wf(:,-5),wf(:,67))
  call prop_Q_A(wf(:,66),Q(:,13),ZERO,0_intkind1,wf(:,68))
  call vert_ZQ_A(gZl,wf(:,5),wf(:,0),wf(:,69))
  call prop_Q_A(wf(:,69),Q(:,13),ZERO,0_intkind1,wf(:,70))
  call vert_QA_W(wf(:,61),wf(:,-1),wf(:,71))
  call prop_W_W(wf(:,71),Q(:,35),MW,1_intkind1,wf(:,72))
  call vert_AV_Q(wf(:,-1),wf(:,2),wf(:,73))
  call vert_WQ_A(wf(:,-4),wf(:,61),wf(:,74))
  call prop_A_Q(wf(:,73),Q(:,14),ZERO,0_intkind1,wf(:,75))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,5),wf(:,76))
  call prop_A_Q(wf(:,76),Q(:,14),ZERO,0_intkind1,wf(:,77))
  call vert_AV_Q(wf(:,-1),wf(:,10),wf(:,78))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,11),wf(:,79))
  call vert_VQ_A(wf(:,10),wf(:,0),wf(:,80))
  call prop_Q_A(wf(:,80),Q(:,49),ZERO,0_intkind1,wf(:,81))
  call vert_ZQ_A(gZl,wf(:,11),wf(:,0),wf(:,82))
  call prop_Q_A(wf(:,82),Q(:,49),ZERO,0_intkind1,wf(:,83))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,84))
  call counter_QA_Z(gZd,wf(:,-2),wf(:,-3),wf(:,85))
  call prop_W_W(wf(:,85),Q(:,12),MZ,1_intkind1,wf(:,86))
  call counter_VQ_A(wf(:,1),wf(:,32),wf(:,87))
  call counter_ZQ_A(gZu,wf(:,7),wf(:,32),wf(:,88))
  call counter_AW_Q(wf(:,33),wf(:,-5),wf(:,89))
  call counter_WQ_A(wf(:,-4),wf(:,32),wf(:,90))
  call counter_QA_W(wf(:,32),wf(:,-3),wf(:,91))
  call counter_AV_Q(wf(:,-3),wf(:,1),wf(:,92))
  call prop_Q_A(wf(:,44),Q(:,52),MB,1_intkind1,wf(:,93))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,7),wf(:,94))
  call counter_SA_Q(gH,wf(:,13),wf(:,-3),wf(:,95))
  call counter_AV_Q(wf(:,-3),wf(:,10),wf(:,96))
  call counter_AZ_Q(gZd,wf(:,-3),wf(:,11),wf(:,97))
  call counter_AW_Q(wf(:,-3),wf(:,-4),wf(:,98))
  call prop_A_Q(wf(:,98),Q(:,24),MT,1_intkind1,wf(:,99))
  call vert_AW_Q(wf(:,99),wf(:,-5),wf(:,100))
  call vert_QA_W(wf(:,-2),wf(:,99),wf(:,101))
  call prop_W_W(wf(:,101),Q(:,28),MW,1_intkind1,wf(:,102))
  call counter_QA_W(wf(:,-2),wf(:,33),wf(:,103))
  call prop_W_W(wf(:,23),Q(:,35),MW,1_intkind1,wf(:,104))
  call prop_W_W(wf(:,25),Q(:,35),MW,1_intkind1,wf(:,105))
  call counter_VQ_A(wf(:,1),wf(:,-2),wf(:,106))
  call prop_A_Q(wf(:,37),Q(:,56),MB,1_intkind1,wf(:,107))
  call counter_ZQ_A(gZd,wf(:,7),wf(:,-2),wf(:,108))
  call counter_WQ_A(wf(:,-5),wf(:,-2),wf(:,109))
  call prop_Q_A(wf(:,109),Q(:,36),MT,1_intkind1,wf(:,110))
  call vert_VQ_A(wf(:,1),wf(:,110),wf(:,111))
  call vert_ZQ_A(gZu,wf(:,7),wf(:,110),wf(:,112))
  call counter_QS_A(gH,wf(:,-2),wf(:,13),wf(:,113))
  call counter_VQ_A(wf(:,10),wf(:,-2),wf(:,114))
  call counter_ZQ_A(gZd,wf(:,11),wf(:,-2),wf(:,115))
  call prop_A_Q(wf(:,50),Q(:,56),MB,1_intkind1,wf(:,116))
  call prop_A_Q(wf(:,51),Q(:,56),MB,1_intkind1,wf(:,117))
  call prop_A_Q(wf(:,52),Q(:,56),MB,1_intkind1,wf(:,118))
  call vert_WQ_A(wf(:,-4),wf(:,110),wf(:,119))
  call vert_QA_W(wf(:,110),wf(:,-3),wf(:,120))
  call prop_W_W(wf(:,120),Q(:,44),MW,1_intkind1,wf(:,121))
  call counter_AQ_S(gH,wf(:,-3),wf(:,-2),wf(:,122))
  call vert_SV_V(wf(:,122),wf(:,7),wf(:,123))
  call vert_VV_S(wf(:,7),wf(:,86),wf(:,124))
  call vert_SV_V(wf(:,122),wf(:,-5),wf(:,125))
  call vert_UV_W(wf(:,84),Q(:,12),wf(:,-5),Q(:,32),wf(:,126))
  call vert_UV_W(wf(:,86),Q(:,12),wf(:,-5),Q(:,32),wf(:,127))
  call vert_SV_V(wf(:,122),wf(:,-4),wf(:,128))
  call prop_W_W(wf(:,128),Q(:,28),MW,1_intkind1,wf(:,129))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,84),Q(:,12),wf(:,130))
  call prop_W_W(wf(:,130),Q(:,28),MW,1_intkind1,wf(:,131))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,86),Q(:,12),wf(:,132))
  call prop_W_W(wf(:,132),Q(:,28),MW,1_intkind1,wf(:,133))
  call vert_VQ_A(wf(:,84),wf(:,0),wf(:,134))
  call prop_Q_A(wf(:,134),Q(:,13),ZERO,0_intkind1,wf(:,135))
  call vert_ZQ_A(gZl,wf(:,86),wf(:,0),wf(:,136))
  call prop_Q_A(wf(:,136),Q(:,13),ZERO,0_intkind1,wf(:,137))
  call vert_AV_Q(wf(:,-1),wf(:,84),wf(:,138))
  call prop_A_Q(wf(:,138),Q(:,14),ZERO,0_intkind1,wf(:,139))
  call vert_AZ_Q(gZl,wf(:,-1),wf(:,86),wf(:,140))
  call prop_A_Q(wf(:,140),Q(:,14),ZERO,0_intkind1,wf(:,141))
  call vert_AV_Q(wf(:,33),wf(:,1),wf(:,142))
  call counter_Q_A(cttt,wf(:,32),Q(:,36),wf(:,143))
  call prop_A_Q(wf(:,142),Q(:,27),MT,1_intkind1,wf(:,144))
  call vert_AZ_Q(gZu,wf(:,33),wf(:,7),wf(:,145))
  call prop_A_Q(wf(:,145),Q(:,27),MT,1_intkind1,wf(:,146))
  call counter_A_Q(cttt,wf(:,33),Q(:,24),wf(:,147))
  call prop_Q_A(wf(:,34),Q(:,39),MT,1_intkind1,wf(:,148))
  call prop_Q_A(wf(:,35),Q(:,39),MT,1_intkind1,wf(:,149))
  call prop_A_Q(wf(:,147),Q(:,24),MT,1_intkind1,wf(:,150))
  call vert_QA_W(wf(:,-2),wf(:,150),wf(:,151))
  call counter_Q_A(ctbb,wf(:,38),Q(:,7),wf(:,152))
  call counter_Q_A(ctbb,wf(:,40),Q(:,7),wf(:,153))
  call vert_AW_Q(wf(:,150),wf(:,-5),wf(:,154))
  call prop_Q_A(wf(:,143),Q(:,36),MT,1_intkind1,wf(:,155))
  call vert_QA_W(wf(:,155),wf(:,-3),wf(:,156))
  call vert_WQ_A(wf(:,-4),wf(:,155),wf(:,157))
  call counter_A_Q(ctbb,wf(:,45),Q(:,11),wf(:,158))
  call counter_A_Q(ctbb,wf(:,47),Q(:,11),wf(:,159))
  call counter_Q_A(ctbb,wf(:,54),Q(:,52),wf(:,160))
  call counter_Q_A(ctbb,wf(:,56),Q(:,52),wf(:,161))
  call counter_Q_A(ctbb,wf(:,58),Q(:,52),wf(:,162))
  call prop_W_W(wf(:,3),Q(:,51),MZ,1_intkind1,wf(:,163))
  call prop_W_W(wf(:,8),Q(:,51),MZ,1_intkind1,wf(:,164))
  call vert_SV_V(wf(:,13),wf(:,7),wf(:,165))
  call prop_W_W(wf(:,165),Q(:,51),MZ,1_intkind1,wf(:,166))
  call vert_VV_S(wf(:,7),wf(:,11),wf(:,167))
  call vert_WQ_A(wf(:,-5),wf(:,38),wf(:,168))
  call prop_Q_A(wf(:,168),Q(:,39),MT,1_intkind1,wf(:,169))
  call vert_WQ_A(wf(:,-5),wf(:,40),wf(:,170))
  call prop_Q_A(wf(:,170),Q(:,39),MT,1_intkind1,wf(:,171))
  call vert_AW_Q(wf(:,45),wf(:,-4),wf(:,172))
  call prop_A_Q(wf(:,172),Q(:,27),MT,1_intkind1,wf(:,173))
  call vert_AW_Q(wf(:,47),wf(:,-4),wf(:,174))
  call prop_A_Q(wf(:,174),Q(:,27),MT,1_intkind1,wf(:,175))
  call vert_AW_Q(wf(:,-3),wf(:,17),wf(:,176))
  call prop_A_Q(wf(:,176),Q(:,27),MT,1_intkind1,wf(:,177))
  call vert_AW_Q(wf(:,-3),wf(:,19),wf(:,178))
  call prop_A_Q(wf(:,178),Q(:,27),MT,1_intkind1,wf(:,179))
  call vert_VV_S(wf(:,17),wf(:,-5),wf(:,180))
  call vert_VV_S(wf(:,19),wf(:,-5),wf(:,181))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,17),Q(:,19),wf(:,182))
  call prop_W_W(wf(:,182),Q(:,51),MZ,1_intkind1,wf(:,183))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,19),Q(:,19),wf(:,184))
  call prop_W_W(wf(:,184),Q(:,51),MZ,1_intkind1,wf(:,185))
  call vert_WQ_A(wf(:,104),wf(:,-2),wf(:,186))
  call prop_Q_A(wf(:,186),Q(:,39),MT,1_intkind1,wf(:,187))
  call vert_WQ_A(wf(:,105),wf(:,-2),wf(:,188))
  call prop_Q_A(wf(:,188),Q(:,39),MT,1_intkind1,wf(:,189))
  call vert_VV_S(wf(:,-4),wf(:,104),wf(:,190))
  call vert_VV_S(wf(:,-4),wf(:,105),wf(:,191))
  call vert_UV_W(wf(:,104),Q(:,35),wf(:,-4),Q(:,16),wf(:,192))
  call prop_W_W(wf(:,192),Q(:,51),MZ,1_intkind1,wf(:,193))
  call vert_UV_W(wf(:,105),Q(:,35),wf(:,-4),Q(:,16),wf(:,194))
  call prop_W_W(wf(:,194),Q(:,51),MZ,1_intkind1,wf(:,195))
  call prop_W_W(wf(:,63),Q(:,51),MZ,1_intkind1,wf(:,196))
  call vert_AW_Q(wf(:,-3),wf(:,65),wf(:,197))
  call prop_A_Q(wf(:,197),Q(:,27),MT,1_intkind1,wf(:,198))
  call vert_VV_S(wf(:,65),wf(:,-5),wf(:,199))
  call vert_UV_W(wf(:,-5),Q(:,32),wf(:,65),Q(:,19),wf(:,200))
  call prop_W_W(wf(:,200),Q(:,51),MZ,1_intkind1,wf(:,201))
  call prop_A_Q(wf(:,67),Q(:,50),ZERO,0_intkind1,wf(:,202))
  call vert_QA_V(wf(:,0),wf(:,202),wf(:,203))
  call vert_QA_Z(gZl,wf(:,0),wf(:,202),wf(:,204))
  call prop_W_W(wf(:,204),Q(:,51),MZ,1_intkind1,wf(:,205))
  call vert_WQ_A(wf(:,72),wf(:,-2),wf(:,206))
  call prop_Q_A(wf(:,206),Q(:,39),MT,1_intkind1,wf(:,207))
  call vert_VV_S(wf(:,-4),wf(:,72),wf(:,208))
  call vert_UV_W(wf(:,72),Q(:,35),wf(:,-4),Q(:,16),wf(:,209))
  call prop_W_W(wf(:,209),Q(:,51),MZ,1_intkind1,wf(:,210))
  call prop_Q_A(wf(:,74),Q(:,49),ZERO,0_intkind1,wf(:,211))
  call vert_QA_V(wf(:,211),wf(:,-1),wf(:,212))
  call vert_QA_Z(gZl,wf(:,211),wf(:,-1),wf(:,213))
  call prop_W_W(wf(:,213),Q(:,51),MZ,1_intkind1,wf(:,214))
  call vert_QA_V(wf(:,81),wf(:,-1),wf(:,215))
  call vert_QA_Z(gZl,wf(:,81),wf(:,-1),wf(:,216))
  call prop_W_W(wf(:,216),Q(:,51),MZ,1_intkind1,wf(:,217))
  call vert_QA_V(wf(:,83),wf(:,-1),wf(:,218))
  call vert_QA_Z(gZl,wf(:,83),wf(:,-1),wf(:,219))
  call prop_W_W(wf(:,219),Q(:,51),MZ,1_intkind1,wf(:,220))
  call prop_A_Q(wf(:,78),Q(:,50),ZERO,0_intkind1,wf(:,221))
  call vert_QA_V(wf(:,0),wf(:,221),wf(:,222))
  call prop_A_Q(wf(:,79),Q(:,50),ZERO,0_intkind1,wf(:,223))
  call vert_QA_V(wf(:,0),wf(:,223),wf(:,224))
  call vert_QA_Z(gZl,wf(:,0),wf(:,221),wf(:,225))
  call prop_W_W(wf(:,225),Q(:,51),MZ,1_intkind1,wf(:,226))
  call vert_QA_Z(gZl,wf(:,0),wf(:,223),wf(:,227))
  call prop_W_W(wf(:,227),Q(:,51),MZ,1_intkind1,wf(:,228))

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
  den(4) = 1 / (Q(5,12) - MZ2)
  den(6) = 1 / (Q(5,3) - MZ2)
  den(9) = 1 / (Q(5,12) - MH2)
  den(10) = 1 / (Q(5,48) - MZ2)
  den(13) = 1 / (Q(5,48) - MH2)
  den(15) = 1 / (Q(5,19) - MW2)
  den(24) = 1 / (Q(5,28) - MW2)
  den(34) = 1 / (Q(5,36) - MT2)
  den(35) = 1 / (Q(5,24) - MT2)
  den(40) = 1 / (Q(5,7) - MB2)
  den(48) = 1 / (Q(5,11) - MB2)
  den(53) = 1 / (Q(5,44) - MW2)
  den(59) = 1 / (Q(5,48))
  den(64) = 1 / (Q(5,52) - MB2)
  den(74) = 1 / (Q(5,33))
  den(75) = 1 / (Q(5,18))
  den(82) = 1 / (Q(5,13))
  den(87) = 1 / (Q(5,35) - MW2)
  den(92) = 1 / (Q(5,14))
  den(101) = 1 / (Q(5,49))
  den(119) = 1 / (Q(5,56) - MB2)
  den(139) = 1 / (Q(5,27) - MT2)
  den(145) = 1 / (Q(5,39) - MT2)
  den(178) = 1 / (Q(5,51))
  den(180) = 1 / (Q(5,51) - MZ2)
  den(187) = 1 / (Q(5,51) - MH2)
  den(214) = 1 / (Q(5,50))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(2)*den(6)
  den(8) = den(4)*den(6)
  den(11) = den(6)*den(9)
  den(12) = den(10)*den(11)
  den(14) = den(8)*den(13)
  den(16) = den(1)*den(15)
  den(17) = den(9)*den(16)
  den(18) = den(6)*den(15)
  den(19) = den(9)*den(18)
  den(20) = den(2)*den(16)
  den(21) = den(4)*den(16)
  den(22) = den(2)*den(18)
  den(23) = den(4)*den(18)
  den(25) = den(9)*den(24)
  den(26) = den(1)*den(25)
  den(27) = den(6)*den(25)
  den(28) = den(2)*den(24)
  den(29) = den(1)*den(28)
  den(30) = den(4)*den(24)
  den(31) = den(1)*den(30)
  den(32) = den(6)*den(28)
  den(33) = den(6)*den(30)
  den(36) = den(1)*den(34)
  den(37) = den(35)*den(36)
  den(38) = den(6)*den(34)
  den(39) = den(35)*den(38)
  den(41) = den(1)*den(40)
  den(42) = den(35)*den(41)
  den(43) = den(6)*den(40)
  den(44) = den(35)*den(43)
  den(45) = den(24)*den(35)
  den(46) = den(1)*den(45)
  den(47) = den(6)*den(45)
  den(49) = den(1)*den(48)
  den(50) = den(34)*den(49)
  den(51) = den(6)*den(48)
  den(52) = den(34)*den(51)
  den(54) = den(34)*den(53)
  den(55) = den(1)*den(54)
  den(56) = den(6)*den(54)
  den(57) = den(13)*den(41)
  den(58) = den(13)*den(43)
  den(60) = den(41)*den(59)
  den(61) = den(10)*den(41)
  den(62) = den(43)*den(59)
  den(63) = den(10)*den(43)
  den(65) = den(13)*den(64)
  den(66) = den(1)*den(65)
  den(67) = den(6)*den(65)
  den(68) = den(59)*den(64)
  den(69) = den(1)*den(68)
  den(70) = den(10)*den(64)
  den(71) = den(1)*den(70)
  den(72) = den(6)*den(68)
  den(73) = den(6)*den(70)
  den(76) = den(74)*den(75)
  den(77) = den(4)*den(76)
  den(78) = den(15)*den(75)
  den(79) = den(9)*den(78)
  den(80) = den(2)*den(78)
  den(81) = den(4)*den(78)
  den(83) = den(2)*den(82)
  den(84) = den(75)*den(83)
  den(85) = den(4)*den(82)
  den(86) = den(75)*den(85)
  den(88) = den(74)*den(87)
  den(89) = den(9)*den(88)
  den(90) = den(2)*den(88)
  den(91) = den(4)*den(88)
  den(93) = den(2)*den(92)
  den(94) = den(74)*den(93)
  den(95) = den(4)*den(92)
  den(96) = den(74)*den(95)
  den(97) = den(59)*den(83)
  den(98) = den(10)*den(83)
  den(99) = den(59)*den(85)
  den(100) = den(10)*den(85)
  den(102) = den(59)*den(101)
  den(103) = den(2)*den(102)
  den(104) = den(4)*den(102)
  den(105) = den(10)*den(101)
  den(106) = den(2)*den(105)
  den(107) = den(4)*den(105)
  den(108) = den(34)*den(78)
  den(109) = den(35)*den(88)
  den(110) = den(16)*den(34)
  den(111) = den(18)*den(34)
  den(112) = den(34)*den(64)
  den(113) = den(1)*den(112)
  den(114) = den(6)*den(112)
  den(115) = den(1)*den(87)
  den(116) = den(35)*den(115)
  den(117) = den(6)*den(87)
  den(118) = den(35)*den(117)
  den(120) = den(35)*den(119)
  den(121) = den(1)*den(120)
  den(122) = den(6)*den(120)
  den(123) = den(13)*den(49)
  den(124) = den(13)*den(51)
  den(125) = den(49)*den(59)
  den(126) = den(10)*den(49)
  den(127) = den(51)*den(59)
  den(128) = den(10)*den(51)
  den(129) = den(13)*den(119)
  den(130) = den(1)*den(129)
  den(131) = den(6)*den(129)
  den(132) = den(59)*den(119)
  den(133) = den(1)*den(132)
  den(134) = den(10)*den(119)
  den(135) = den(1)*den(134)
  den(136) = den(6)*den(132)
  den(137) = den(6)*den(134)
  den(138) = den(1)*den(35)
  den(140) = den(138)*den(139)
  den(141) = den(34)*den(140)
  den(142) = den(6)*den(35)
  den(143) = den(139)*den(142)
  den(144) = den(34)*den(143)
  den(146) = den(36)*den(145)
  den(147) = den(35)*den(146)
  den(148) = den(38)*den(145)
  den(149) = den(35)*den(148)
  den(150) = den(35)**2
  den(151) = den(115)*den(150)
  den(152) = den(117)*den(150)
  den(153) = den(41)*den(120)
  den(154) = den(43)*den(120)
  den(155) = den(41)*den(150)
  den(156) = den(43)*den(150)
  den(157) = den(34)**2
  den(158) = den(16)*den(157)
  den(159) = den(18)*den(157)
  den(160) = den(49)*den(157)
  den(161) = den(51)*den(157)
  den(162) = den(49)*den(112)
  den(163) = den(51)*den(112)
  den(164) = den(41)*den(129)
  den(165) = den(43)*den(129)
  den(166) = den(41)*den(132)
  den(167) = den(41)*den(134)
  den(168) = den(43)*den(132)
  den(169) = den(43)*den(134)
  den(170) = den(49)*den(65)
  den(171) = den(51)*den(65)
  den(172) = den(49)*den(68)
  den(173) = den(49)*den(70)
  den(174) = den(51)*den(68)
  den(175) = den(51)*den(70)
  den(176) = den(78)*den(157)
  den(177) = den(88)*den(150)
  den(179) = den(1)*den(178)
  den(181) = den(1)*den(180)
  den(182) = den(6)*den(178)
  den(183) = den(6)*den(180)
  den(184) = den(6)*den(13)
  den(185) = den(180)*den(184)
  den(186) = den(6)*den(10)
  den(188) = den(186)*den(187)
  den(189) = den(41)*den(145)
  den(190) = den(43)*den(145)
  den(191) = den(49)*den(139)
  den(192) = den(51)*den(139)
  den(193) = den(16)*den(139)
  den(194) = den(18)*den(139)
  den(195) = den(16)*den(187)
  den(196) = den(18)*den(187)
  den(197) = den(16)*den(178)
  den(198) = den(16)*den(180)
  den(199) = den(18)*den(178)
  den(200) = den(18)*den(180)
  den(201) = den(115)*den(145)
  den(202) = den(117)*den(145)
  den(203) = den(115)*den(187)
  den(204) = den(117)*den(187)
  den(205) = den(115)*den(178)
  den(206) = den(115)*den(180)
  den(207) = den(117)*den(178)
  den(208) = den(117)*den(180)
  den(209) = den(76)*den(180)
  den(210) = den(78)*den(139)
  den(211) = den(78)*den(187)
  den(212) = den(78)*den(178)
  den(213) = den(78)*den(180)
  den(215) = den(75)*den(214)
  den(216) = den(178)*den(215)
  den(217) = den(180)*den(215)
  den(218) = den(88)*den(145)
  den(219) = den(88)*den(187)
  den(220) = den(88)*den(178)
  den(221) = den(88)*den(180)
  den(222) = den(74)*den(101)
  den(223) = den(178)*den(222)
  den(224) = den(180)*den(222)
  den(225) = den(102)*den(178)
  den(226) = den(102)*den(180)
  den(227) = den(105)*den(178)
  den(228) = den(105)*den(180)
  den(229) = den(59)*den(214)
  den(230) = den(178)*den(229)
  den(231) = den(10)*den(214)
  den(232) = den(178)*den(231)
  den(233) = den(180)*den(229)
  den(234) = den(180)*den(231)
  den(235) = den(1)*den(34)*den(35)
  den(236) = den(6)*den(34)*den(35)
  den(237) = den(1)*den(13)
  den(238) = den(1)*den(59)
  den(239) = den(1)*den(10)
  den(240) = den(6)*den(59)
  den(241) = den(35)*den(189)
  den(242) = den(35)*den(190)
  den(243) = den(35)*den(201)
  den(244) = den(35)*den(202)
  den(245) = den(34)*den(191)
  den(246) = den(34)*den(192)
  den(247) = den(34)*den(193)
  den(248) = den(34)*den(194)
  den(249) = den(34)*den(210)
  den(250) = den(35)*den(218)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(182)

  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_VV(wf(:,3),wf(:,5)) * den(5)
  A(3) = cont_VV(wf(:,2),wf(:,8)) * den(7)
  A(4) = cont_VV(wf(:,5),wf(:,8)) * den(8)
  A(5) = cont_VV(wf(:,11),wf(:,12)) * den(12)
  A(6) = cont_SS(wf(:,13),wf(:,14)) * den(14)
  A(7) = cont_VV(wf(:,16),wf(:,17)) * den(17)
  A(8) = cont_VV(wf(:,16),wf(:,19)) * den(19)
  A(9) = cont_VV(wf(:,17),wf(:,20)) * den(20)
  A(10) = cont_VV(wf(:,17),wf(:,21)) * den(21)
  A(11) = cont_VV(wf(:,19),wf(:,20)) * den(22)
  A(12) = cont_VV(wf(:,19),wf(:,21)) * den(23)
  A(13) = cont_VV(wf(:,23),wf(:,24)) * den(26)
  A(14) = cont_VV(wf(:,24),wf(:,25)) * den(27)
  A(15) = cont_VV(wf(:,23),wf(:,27)) * den(29)
  A(16) = cont_VV(wf(:,23),wf(:,29)) * den(31)
  A(17) = cont_VV(wf(:,25),wf(:,27)) * den(32)
  A(18) = cont_VV(wf(:,25),wf(:,29)) * den(33)
  A(19) = cont_QA(wf(:,33),wf(:,34)) * den(37)
  A(20) = cont_QA(wf(:,33),wf(:,35)) * den(39)
  A(21) = cont_QA(wf(:,37),wf(:,38)) * den(42)
  A(22) = cont_QA(wf(:,37),wf(:,40)) * den(44)
  A(23) = cont_VV(wf(:,23),wf(:,42)) * den(46)
  A(24) = cont_VV(wf(:,25),wf(:,42)) * den(47)
  A(25) = cont_QA(wf(:,44),wf(:,45)) * den(50)
  A(26) = cont_QA(wf(:,44),wf(:,47)) * den(52)
  A(27) = cont_VV(wf(:,15),wf(:,49)) * den(55)
  A(28) = cont_VV(wf(:,18),wf(:,49)) * den(56)
  A(29) = cont_QA(wf(:,38),wf(:,50)) * den(57)
  A(30) = cont_QA(wf(:,40),wf(:,50)) * den(58)
  A(31) = cont_QA(wf(:,38),wf(:,51)) * den(60)
  A(32) = cont_QA(wf(:,38),wf(:,52)) * den(61)
  A(33) = cont_QA(wf(:,40),wf(:,51)) * den(62)
  A(34) = cont_QA(wf(:,40),wf(:,52)) * den(63)
  A(35) = cont_QA(wf(:,43),wf(:,54)) * den(66)
  A(36) = cont_QA(wf(:,46),wf(:,54)) * den(67)
  A(37) = cont_QA(wf(:,43),wf(:,56)) * den(69)
  A(38) = cont_QA(wf(:,43),wf(:,58)) * den(71)
  A(39) = cont_QA(wf(:,46),wf(:,56)) * den(72)
  A(40) = cont_QA(wf(:,46),wf(:,58)) * den(73)
  A(41) = cont_VV(wf(:,5),wf(:,63)) * den(77)
  A(42) = cont_VV(wf(:,16),wf(:,65)) * den(79)
  A(43) = cont_VV(wf(:,20),wf(:,65)) * den(80)
  A(44) = cont_VV(wf(:,21),wf(:,65)) * den(81)
  A(45) = cont_QA(wf(:,67),wf(:,68)) * den(84)
  A(46) = cont_QA(wf(:,67),wf(:,70)) * den(86)
  A(47) = cont_VV(wf(:,22),wf(:,72)) * den(89)
  A(48) = cont_VV(wf(:,26),wf(:,72)) * den(90)
  A(49) = cont_VV(wf(:,28),wf(:,72)) * den(91)
  A(50) = cont_QA(wf(:,74),wf(:,75)) * den(94)
  A(51) = cont_QA(wf(:,74),wf(:,77)) * den(96)
  A(52) = cont_QA(wf(:,68),wf(:,78)) * den(97)
  A(53) = cont_QA(wf(:,68),wf(:,79)) * den(98)
  A(54) = cont_QA(wf(:,70),wf(:,78)) * den(99)
  A(55) = cont_QA(wf(:,70),wf(:,79)) * den(100)
  A(56) = cont_QA(wf(:,73),wf(:,81)) * den(103)
  A(57) = cont_QA(wf(:,76),wf(:,81)) * den(104)
  A(58) = cont_QA(wf(:,73),wf(:,83)) * den(106)
  A(59) = cont_QA(wf(:,76),wf(:,83)) * den(107)
  A(60) = cont_VV(wf(:,48),wf(:,65)) * den(108)
  A(61) = cont_VV(wf(:,41),wf(:,72)) * den(109)

  A(62) = cont_VV(wf(:,3),wf(:,84)) * den(3)
  A(63) = cont_VV(wf(:,3),wf(:,86)) * den(5)
  A(64) = cont_VV(wf(:,8),wf(:,84)) * den(7)
  A(65) = cont_VV(wf(:,8),wf(:,86)) * den(8)
  A(66) = cont_QA(wf(:,33),wf(:,87)) * den(37)
  A(67) = cont_QA(wf(:,33),wf(:,88)) * den(39)
  A(68) = cont_QA(wf(:,38),wf(:,89)) * den(42)
  A(69) = cont_QA(wf(:,40),wf(:,89)) * den(44)
  A(70) = cont_QA(wf(:,45),wf(:,90)) * den(50)
  A(71) = cont_QA(wf(:,47),wf(:,90)) * den(52)
  A(72) = cont_VV(wf(:,17),wf(:,91)) * den(110)
  A(73) = cont_VV(wf(:,19),wf(:,91)) * den(111)
  A(74) = cont_QA(wf(:,92),wf(:,93)) * den(113)
  A(75) = cont_QA(wf(:,93),wf(:,94)) * den(114)
  A(76) = cont_QA(wf(:,38),wf(:,95)) * den(57)
  A(77) = cont_QA(wf(:,40),wf(:,95)) * den(58)
  A(78) = cont_QA(wf(:,38),wf(:,96)) * den(60)
  A(79) = cont_QA(wf(:,38),wf(:,97)) * den(61)
  A(80) = cont_QA(wf(:,40),wf(:,96)) * den(62)
  A(81) = cont_QA(wf(:,40),wf(:,97)) * den(63)
  A(82) = cont_QA(wf(:,54),wf(:,92)) * den(66)
  A(83) = cont_QA(wf(:,54),wf(:,94)) * den(67)
  A(84) = cont_QA(wf(:,56),wf(:,92)) * den(69)
  A(85) = cont_QA(wf(:,58),wf(:,92)) * den(71)
  A(86) = cont_QA(wf(:,56),wf(:,94)) * den(72)
  A(87) = cont_QA(wf(:,58),wf(:,94)) * den(73)
  A(88) = cont_QA(wf(:,34),wf(:,99)) * den(37)
  A(89) = cont_QA(wf(:,35),wf(:,99)) * den(39)
  A(90) = cont_QA(wf(:,38),wf(:,100)) * den(42)
  A(91) = cont_QA(wf(:,40),wf(:,100)) * den(44)
  A(92) = cont_VV(wf(:,23),wf(:,102)) * den(46)
  A(93) = cont_VV(wf(:,25),wf(:,102)) * den(47)
  A(94) = cont_VV(wf(:,103),wf(:,104)) * den(116)
  A(95) = cont_VV(wf(:,103),wf(:,105)) * den(118)
  A(96) = cont_QA(wf(:,106),wf(:,107)) * den(121)
  A(97) = cont_QA(wf(:,107),wf(:,108)) * den(122)
  A(98) = cont_QA(wf(:,33),wf(:,111)) * den(37)
  A(99) = cont_QA(wf(:,33),wf(:,112)) * den(39)
  A(100) = cont_QA(wf(:,45),wf(:,113)) * den(123)
  A(101) = cont_QA(wf(:,47),wf(:,113)) * den(124)
  A(102) = cont_QA(wf(:,45),wf(:,114)) * den(125)
  A(103) = cont_QA(wf(:,45),wf(:,115)) * den(126)
  A(104) = cont_QA(wf(:,47),wf(:,114)) * den(127)
  A(105) = cont_QA(wf(:,47),wf(:,115)) * den(128)
  A(106) = cont_QA(wf(:,106),wf(:,116)) * den(130)
  A(107) = cont_QA(wf(:,108),wf(:,116)) * den(131)
  A(108) = cont_QA(wf(:,106),wf(:,117)) * den(133)
  A(109) = cont_QA(wf(:,106),wf(:,118)) * den(135)
  A(110) = cont_QA(wf(:,108),wf(:,117)) * den(136)
  A(111) = cont_QA(wf(:,108),wf(:,118)) * den(137)
  A(112) = cont_QA(wf(:,45),wf(:,119)) * den(50)
  A(113) = cont_QA(wf(:,47),wf(:,119)) * den(52)
  A(114) = cont_VV(wf(:,15),wf(:,121)) * den(55)
  A(115) = cont_VV(wf(:,18),wf(:,121)) * den(56)
  A(116) = cont_VV(wf(:,11),wf(:,123)) * den(12)
  A(117) = cont_SS(wf(:,13),wf(:,124)) * den(14)
  A(118) = cont_VV(wf(:,17),wf(:,125)) * den(17)
  A(119) = cont_VV(wf(:,19),wf(:,125)) * den(19)
  A(120) = cont_VV(wf(:,17),wf(:,126)) * den(20)
  A(121) = cont_VV(wf(:,17),wf(:,127)) * den(21)
  A(122) = cont_VV(wf(:,19),wf(:,126)) * den(22)
  A(123) = cont_VV(wf(:,19),wf(:,127)) * den(23)
  A(124) = cont_VV(wf(:,23),wf(:,129)) * den(26)
  A(125) = cont_VV(wf(:,25),wf(:,129)) * den(27)
  A(126) = cont_VV(wf(:,23),wf(:,131)) * den(29)
  A(127) = cont_VV(wf(:,23),wf(:,133)) * den(31)
  A(128) = cont_VV(wf(:,25),wf(:,131)) * den(32)
  A(129) = cont_VV(wf(:,25),wf(:,133)) * den(33)
  A(130) = cont_VV(wf(:,65),wf(:,91)) * den(108)
  A(131) = cont_VV(wf(:,72),wf(:,101)) * den(109)
  A(132) = cont_VV(wf(:,65),wf(:,120)) * den(108)
  A(133) = cont_VV(wf(:,72),wf(:,103)) * den(109)
  A(134) = cont_VV(wf(:,63),wf(:,86)) * den(77)
  A(135) = cont_VV(wf(:,65),wf(:,125)) * den(79)
  A(136) = cont_VV(wf(:,65),wf(:,126)) * den(80)
  A(137) = cont_VV(wf(:,65),wf(:,127)) * den(81)
  A(138) = cont_QA(wf(:,67),wf(:,135)) * den(84)
  A(139) = cont_QA(wf(:,67),wf(:,137)) * den(86)
  A(140) = cont_VV(wf(:,72),wf(:,128)) * den(89)
  A(141) = cont_VV(wf(:,72),wf(:,130)) * den(90)
  A(142) = cont_VV(wf(:,72),wf(:,132)) * den(91)
  A(143) = cont_QA(wf(:,74),wf(:,139)) * den(94)
  A(144) = cont_QA(wf(:,74),wf(:,141)) * den(96)
  A(145) = cont_QA(wf(:,81),wf(:,138)) * den(103)
  A(146) = cont_QA(wf(:,81),wf(:,140)) * den(104)
  A(147) = cont_QA(wf(:,83),wf(:,138)) * den(106)
  A(148) = cont_QA(wf(:,83),wf(:,140)) * den(107)
  A(149) = cont_QA(wf(:,78),wf(:,135)) * den(97)
  A(150) = cont_QA(wf(:,79),wf(:,135)) * den(98)
  A(151) = cont_QA(wf(:,78),wf(:,137)) * den(99)
  A(152) = cont_QA(wf(:,79),wf(:,137)) * den(100)
  A(153) = cont_QA(wf(:,143),wf(:,144)) * den(141)
  A(154) = cont_QA(wf(:,143),wf(:,146)) * den(144)
  A(155) = cont_QA(wf(:,147),wf(:,148)) * den(147)
  A(156) = cont_QA(wf(:,147),wf(:,149)) * den(149)
  A(157) = cont_VV(wf(:,104),wf(:,151)) * den(151)
  A(158) = cont_VV(wf(:,105),wf(:,151)) * den(152)
  A(159) = cont_QA(wf(:,107),wf(:,152)) * den(153)
  A(160) = cont_QA(wf(:,107),wf(:,153)) * den(154)
  A(161) = cont_QA(wf(:,38),wf(:,154)) * den(155)
  A(162) = cont_QA(wf(:,40),wf(:,154)) * den(156)
  A(163) = cont_VV(wf(:,17),wf(:,156)) * den(158)
  A(164) = cont_VV(wf(:,19),wf(:,156)) * den(159)
  A(165) = cont_QA(wf(:,45),wf(:,157)) * den(160)
  A(166) = cont_QA(wf(:,47),wf(:,157)) * den(161)
  A(167) = cont_QA(wf(:,93),wf(:,158)) * den(162)
  A(168) = cont_QA(wf(:,93),wf(:,159)) * den(163)
  A(169) = cont_QA(wf(:,116),wf(:,152)) * den(164)
  A(170) = cont_QA(wf(:,116),wf(:,153)) * den(165)
  A(171) = cont_QA(wf(:,117),wf(:,152)) * den(166)
  A(172) = cont_QA(wf(:,118),wf(:,152)) * den(167)
  A(173) = cont_QA(wf(:,117),wf(:,153)) * den(168)
  A(174) = cont_QA(wf(:,118),wf(:,153)) * den(169)
  A(175) = cont_QA(wf(:,45),wf(:,160)) * den(170)
  A(176) = cont_QA(wf(:,47),wf(:,160)) * den(171)
  A(177) = cont_QA(wf(:,45),wf(:,161)) * den(172)
  A(178) = cont_QA(wf(:,45),wf(:,162)) * den(173)
  A(179) = cont_QA(wf(:,47),wf(:,161)) * den(174)
  A(180) = cont_QA(wf(:,47),wf(:,162)) * den(175)
  A(181) = cont_VV(wf(:,65),wf(:,156)) * den(176)
  A(182) = cont_VV(wf(:,72),wf(:,151)) * den(177)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(182)
  complex(REALKIND), intent(out) :: M1(1), M2(1)

  M1(1) = (-A(31)-A(37))*f(1)+(-A(1)+A(9)+A(15)-A(33)-A(39)-A(52)-A(56))*f(2)+(-A(54)-A(57))*f(3)+(A(60)+A(61))*f(9)+(-A(42) &
       -A(47))*f(12)+(A(24)+A(28)+A(44)+A(49))*f(14)-A(5)*f(18)+(-A(8)-A(14))*f(19)+(A(21)+A(25)-A(43)+A(45)-A(48)+A(50))*f(22) &
       -A(19)*f(23)+(A(20)+A(22)-A(23)+A(26)-A(27)+A(41)+A(46)+A(51))*f(24)+(-A(4)+A(12)+A(18))*f(25)+(-A(29)-A(35))*f(37)+(A(7) &
       +A(13)-A(30)-A(36))*f(38)-A(6)*f(45)+(A(3)-A(11)-A(17)+A(32)+A(38)+A(53)+A(58))*f(47)+(A(2)-A(10)-A(16)+A(34)+A(40)+A(55) &
       +A(59))*f(48)

  M2(1) = (A(171)+A(177))*f(4)+(A(173)+A(179))*f(5)+(-A(78)-A(84)-A(102)-A(108))*f(6)+(-A(62)-A(80)-A(86)-A(104)-A(110)+A(120) &
       +A(126)-A(145)-A(149))*f(7)+(-A(146)-A(151))*f(8)+(-A(181)-A(182))*f(10)+(A(130)+A(131)+A(132)+A(133))*f(11)+(-A(135) &
       -A(140))*f(13)+(-A(158)-A(164))*f(15)+(A(137)+A(142))*f(16)+(A(73)+A(93)+A(95)+A(115))*f(17)-A(116)*f(20)+(-A(119) &
       -A(125))*f(21)+(-A(159)-A(161)-A(165)-A(167))*f(26)+(A(153)+A(155))*f(27)+(-A(154)-A(156)+A(157)-A(160)-A(162)+A(163) &
       -A(166)-A(168))*f(28)+(A(74)+A(96)-A(136)+A(138)-A(141)+A(143))*f(29)+(A(75)+A(97)+A(134)+A(139)+A(144))*f(30)+(A(68)+A(70) &
       +A(90)+A(112))*f(31)+(-A(88)-A(98))*f(32)+(A(69)+A(71)-A(72)+A(89)+A(91)-A(92)-A(94)+A(99)+A(113)-A(114))*f(33)-A(66)*f(34) &
       +A(67)*f(35)+(-A(65)+A(123)+A(129))*f(36)+(A(169)+A(175))*f(39)+(A(170)+A(176))*f(40)+(-A(76)-A(100))*f(41)+(-A(77)-A(101) &
       +A(118)+A(124))*f(42)+(-A(82)-A(106))*f(43)+(-A(83)-A(107))*f(44)-A(117)*f(46)+(-A(172)-A(178))*f(49)+(-A(174) &
       -A(180))*f(50)+(A(64)+A(79)+A(85)+A(103)+A(109)-A(122)-A(128)+A(147)+A(150))*f(51)+(A(63)+A(81)+A(87)+A(105)+A(111)-A(121) &
       -A(127)+A(148)+A(152))*f(52)

end subroutine colourvectors

end module ol_loop_eevvjj_eexbbxwwx_1_/**/REALKIND
