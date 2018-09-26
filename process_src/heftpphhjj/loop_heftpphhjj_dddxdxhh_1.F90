
module ol_colourmatrix_heftpphhjj_dddxdxhh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(46,2), K2(2,2), KL(2,2)
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
  K1( 5,:) = [   0,   4]
  K1( 6,:) = [   4,   0]
  K1( 7,:) = [  12,   4]
  K1( 8,:) = [   4,  12]
  K1( 9,:) = [   0,  -4]
  K1(10,:) = [  -4, -12]
  K1(11,:) = [ -12,  -4]
  K1(12,:) = [  -4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [ -12,  -4]
  K1(16,:) = [  -4,   0]
  K1(17,:) = [   0,  -4]
  K1(18,:) = [  -4, -12]
  K1(19,:) = [   0,   4]
  K1(20,:) = [   4,   0]
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
  K1(35,:) = [   0,   0]
  K1(36,:) = [   0,   0]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [   0,   0]
  K1(42,:) = [   0,   0]
  K1(43,:) = [   0,   0]
  K1(44,:) = [   0,   0]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_heftpphhjj_dddxdxhh_1_/**/REALKIND



module ol_forced_parameters_heftpphhjj_dddxdxhh_1_/**/REALKIND
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
  if (MB /= 0) write(*,101) 'MB = 0'
  if (YB /= 0) write(*,101) 'YB = 0'
  if (CKMORDER /= 0) write(*,101) 'CKMORDER = 0'
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
end module ol_forced_parameters_heftpphhjj_dddxdxhh_1_/**/REALKIND

module ol_loop_heftpphhjj_dddxdxhh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(20), c(34)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:77)
  ! denominators
  complex(REALKIND), save :: den(83)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,16), Mct(2,16), Mcol_loop(2,16)
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
  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f( 1) = (CI*DOI*eQED**2*gQCD**6)/(1152._/**/REALKIND*MW**2*pi**4*sw**2)
    f( 2) = (CI*eQED**2*gQCD**4)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 3) = (CI*countertermnorm*eQED**2*gQCD**6)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 4) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**6)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 5) = (CI*eQED**2*gQCD**4*lambdaHHH*MH**2)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 6) = (CI*countertermnorm*eQED**2*gQCD**6*lambdaHHH*MH**2)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 7) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**6*lambdaHHH*MH**2)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 8) = (CI*countertermnorm*eQED**2*gQCD**6*R2HEFTghqq)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 9) = (CI*countertermnorm*eQED**2*gQCD**6*lambdaHHH*MH**2*R2HEFTghqq)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(10) = (CI*countertermnorm*eQED**2*gQCD**6*lambdaHHH*MH**2*R2HEFThqq)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(11) = (CI*eQED**2*gQCD**6*integralnorm*SwB)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f(12) = (eQED**2*gQCD**6*integralnorm*SwB)/(MW**2*pi**2*sw**2*96._/**/REALKIND)
    f(13) = (eQED**2*gQCD**6*integralnorm*SwB)/(MW**2*pi**2*sw**2*48._/**/REALKIND)
    f(14) = (CI*eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwB)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(15) = (eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwB)/(MW**2*pi**2*sw**2*32._/**/REALKIND)
    f(16) = (eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwB)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(17) = (eQED**2*gQCD**6*integralnorm*SwF)/(MW**2*pi**2*sw**2*24._/**/REALKIND)
    f(18) = (eQED**2*gQCD**6*integralnorm*SwF)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(19) = (eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwF)/(MW**2*pi**2*sw**2*8._/**/REALKIND)
    f(20) = (3*eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwF)/(MW**2*pi**2*sw**2*16._/**/REALKIND)

  c = [ 9*CI*f(11), 27*CI*f(11), 18*f(12), 54*f(12), f(13), 3*f(13), 6*f(13), 8*f(13), 10*f(13), 18*f(13), 21*f(13), 24*f(13) &
    , 54*f(13), 9*CI*f(14), 27*CI*f(14), 18*f(15), 54*f(15), f(16), 3*f(16), 6*f(16), 8*f(16), 10*f(16), 18*f(16), 21*f(16) &
    , 24*f(16), 54*f(16), 3*f(17), 9*f(17), 3*f(18), 9*f(18), 3*f(19), 9*f(19), 3*f(20), 9*f(20) ]
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
  real(REALKIND),    intent(in)  :: P(0:3,6)
  integer,           intent(in)  :: H(6)
  integer,           intent(in), optional  :: POLSEL(6)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(48)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_A(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_S(P(:,6), rMH, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_Q(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_A(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), 0)
    call pol_wf_S(P(:,6), rMH, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-2),wf(:,1))
  call vert_QA_V(wf(:,-1),wf(:,-3),wf(:,2))
  call vert_HHG_G(wf(:,-4),wf(:,-5),wf(:,1),Q(:,5),wf(:,3),Q(:,53))
  call vert_QA_V(wf(:,0),wf(:,-3),wf(:,4))
  call vert_QA_V(wf(:,-1),wf(:,-2),wf(:,5))
  call vert_HHG_G(wf(:,-4),wf(:,-5),wf(:,4),Q(:,9),wf(:,6),Q(:,57))
  call vert_SS_S(wf(:,-4),wf(:,-5),wf(:,7))
  call vert_GG_H(wf(:,1),Q(:,5),wf(:,2),Q(:,10),wf(:,8))
  call vert_GG_H(wf(:,5),Q(:,6),wf(:,4),Q(:,9),wf(:,9))
  call counter_HHQA_V(wf(:,-4),wf(:,-5),wf(:,-1),wf(:,-3),wf(:,10))
  call counter_HHQA_V(wf(:,-4),wf(:,-5),wf(:,-1),wf(:,-2),wf(:,11))
  call counter_HHQA_V(wf(:,-4),wf(:,-5),wf(:,0),wf(:,-3),wf(:,12))
  call counter_HHQA_V(wf(:,-4),wf(:,-5),wf(:,0),wf(:,-2),wf(:,13))
  call counter_HHG_G(ctHEFTggh,wf(:,-4),wf(:,-5),wf(:,1),Q(:,5),wf(:,14),Q(:,53))
  call counter_HHG_G(ctHEFTggh,wf(:,-4),wf(:,-5),wf(:,4),Q(:,9),wf(:,15),Q(:,57))
  call counter_QA_V(wf(:,-1),wf(:,-3),wf(:,16))
  call counter_QA_V(wf(:,-1),wf(:,-2),wf(:,17))
  call counter_QAV_H(wf(:,-1),wf(:,-3),wf(:,1),wf(:,18))
  call counter_QAV_H(wf(:,-1),wf(:,-2),wf(:,4),wf(:,19))
  call counter_QA_V(wf(:,0),wf(:,-3),wf(:,20))
  call vert_HHG_G(wf(:,-4),wf(:,-5),wf(:,20),Q(:,9),wf(:,21),Q(:,57))
  call counter_QA_V(wf(:,0),wf(:,-2),wf(:,22))
  call vert_HHG_G(wf(:,-4),wf(:,-5),wf(:,22),Q(:,5),wf(:,23),Q(:,53))
  call counter_QAV_H(wf(:,0),wf(:,-3),wf(:,5),wf(:,24))
  call counter_QAV_H(wf(:,0),wf(:,-2),wf(:,2),wf(:,25))
  call counter_GG_H(ctHEFTggh,wf(:,1),Q(:,5),wf(:,2),Q(:,10),wf(:,26))
  call vert_HG_G(wf(:,-4),wf(:,1),Q(:,5),wf(:,27),Q(:,21))
  call counter_HG_G_vert(wf(:,-5),wf(:,2),Q(:,10),wf(:,28),Q(:,42))
  call vert_HG_G(wf(:,-4),wf(:,2),Q(:,10),wf(:,29),Q(:,26))
  call counter_HG_G_vert(wf(:,-5),wf(:,1),Q(:,5),wf(:,30),Q(:,37))
  call vert_HHG_G(wf(:,-4),wf(:,-5),wf(:,2),Q(:,10),wf(:,31),Q(:,58))
  call counter_V_V(ctGG,wf(:,1),Q(:,5),wf(:,32))
  call counter_V_V(ctGG,wf(:,2),Q(:,10),wf(:,33))
  call vert_HG_G(wf(:,-5),wf(:,1),Q(:,5),wf(:,34),Q(:,37))
  call counter_HG_G_vert(wf(:,-4),wf(:,2),Q(:,10),wf(:,35),Q(:,26))
  call vert_HG_G(wf(:,-5),wf(:,2),Q(:,10),wf(:,36),Q(:,42))
  call counter_HG_G_vert(wf(:,-4),wf(:,1),Q(:,5),wf(:,37),Q(:,21))
  call vert_VQ_A(wf(:,1),wf(:,-1),wf(:,38))
  call counter_HA_Q(wf(:,7),wf(:,-3),Q(:,8),wf(:,39),Q(:,56))
  call prop_Q_A(wf(:,38),Q(:,7),ZERO,0_intkind1,wf(:,40))
  call counter_GG_H(ctHEFTggh,wf(:,5),Q(:,6),wf(:,4),Q(:,9),wf(:,41))
  call vert_HG_G(wf(:,-4),wf(:,4),Q(:,9),wf(:,42),Q(:,25))
  call counter_HG_G_vert(wf(:,-5),wf(:,5),Q(:,6),wf(:,43),Q(:,38))
  call vert_HG_G(wf(:,-4),wf(:,5),Q(:,6),wf(:,44),Q(:,22))
  call counter_HG_G_vert(wf(:,-5),wf(:,4),Q(:,9),wf(:,45),Q(:,41))
  call vert_HHG_G(wf(:,-4),wf(:,-5),wf(:,5),Q(:,6),wf(:,46),Q(:,54))
  call counter_V_V(ctGG,wf(:,4),Q(:,9),wf(:,47))
  call counter_V_V(ctGG,wf(:,5),Q(:,6),wf(:,48))
  call vert_HG_G(wf(:,-5),wf(:,4),Q(:,9),wf(:,49),Q(:,41))
  call counter_HG_G_vert(wf(:,-4),wf(:,5),Q(:,6),wf(:,50),Q(:,22))
  call vert_HG_G(wf(:,-5),wf(:,5),Q(:,6),wf(:,51),Q(:,38))
  call counter_HG_G_vert(wf(:,-4),wf(:,4),Q(:,9),wf(:,52),Q(:,25))
  call vert_VQ_A(wf(:,5),wf(:,0),wf(:,53))
  call prop_Q_A(wf(:,53),Q(:,7),ZERO,0_intkind1,wf(:,54))
  call vert_VQ_A(wf(:,4),wf(:,-1),wf(:,55))
  call counter_HA_Q(wf(:,7),wf(:,-2),Q(:,4),wf(:,56),Q(:,52))
  call prop_Q_A(wf(:,55),Q(:,11),ZERO,0_intkind1,wf(:,57))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,58))
  call prop_Q_A(wf(:,58),Q(:,11),ZERO,0_intkind1,wf(:,59))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,60))
  call counter_QH_A(wf(:,-1),Q(:,2),wf(:,7),wf(:,61),Q(:,50))
  call prop_A_Q(wf(:,60),Q(:,13),ZERO,0_intkind1,wf(:,62))
  call vert_GG_H(wf(:,1),Q(:,5),wf(:,16),Q(:,10),wf(:,63))
  call vert_AV_Q(wf(:,-2),wf(:,4),wf(:,64))
  call prop_A_Q(wf(:,64),Q(:,13),ZERO,0_intkind1,wf(:,65))
  call vert_GG_H(wf(:,17),Q(:,6),wf(:,4),Q(:,9),wf(:,66))
  call vert_AV_Q(wf(:,-3),wf(:,5),wf(:,67))
  call counter_QH_A(wf(:,0),Q(:,1),wf(:,7),wf(:,68),Q(:,49))
  call prop_A_Q(wf(:,67),Q(:,14),ZERO,0_intkind1,wf(:,69))
  call vert_GG_H(wf(:,5),Q(:,6),wf(:,20),Q(:,9),wf(:,70))
  call vert_AV_Q(wf(:,-2),wf(:,2),wf(:,71))
  call prop_A_Q(wf(:,71),Q(:,14),ZERO,0_intkind1,wf(:,72))
  call vert_GG_H(wf(:,22),Q(:,5),wf(:,2),Q(:,10),wf(:,73))
  call vert_HG_G(wf(:,7),wf(:,2),Q(:,10),wf(:,74),Q(:,58))
  call vert_HG_G(wf(:,7),wf(:,1),Q(:,5),wf(:,75),Q(:,53))
  call vert_HG_G(wf(:,7),wf(:,5),Q(:,6),wf(:,76),Q(:,54))
  call vert_HG_G(wf(:,7),wf(:,4),Q(:,9),wf(:,77),Q(:,57))

  call denominators()
  call diagrams(A)
  call colourvectors(A, M1, M2)
end subroutine tree_wavefunctions


subroutine denominators()
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none
  ! propagators
  den(1) = 1 / (Q(5,5))
  den(2) = 1 / (Q(5,10))
  den(4) = 1 / (Q(5,9))
  den(5) = 1 / (Q(5,6))
  den(7) = 1 / (Q(5,48) - MH2)
  den(14) = 1 / (Q(5,21))
  den(17) = 1 / (Q(5,26))
  den(20) = 1 / (Q(5,58))
  den(23) = 1 / (Q(5,53))
  den(26) = 1 / (Q(5,37))
  den(29) = 1 / (Q(5,42))
  den(32) = 1 / (Q(5,7))
  den(35) = 1 / (Q(5,25))
  den(38) = 1 / (Q(5,22))
  den(41) = 1 / (Q(5,54))
  den(44) = 1 / (Q(5,57))
  den(47) = 1 / (Q(5,41))
  den(50) = 1 / (Q(5,38))
  den(55) = 1 / (Q(5,11))
  den(60) = 1 / (Q(5,13))
  den(65) = 1 / (Q(5,14))

  ! denominators
  den(3) = den(1)*den(2)
  den(6) = den(4)*den(5)
  den(8) = den(3)*den(7)
  den(9) = den(6)*den(7)
  den(10) = den(1)*den(7)
  den(11) = den(4)*den(7)
  den(12) = den(5)*den(7)
  den(13) = den(2)*den(7)
  den(15) = den(1)*den(14)
  den(16) = den(2)*den(15)
  den(18) = den(2)*den(17)
  den(19) = den(1)*den(18)
  den(21) = den(2)*den(20)
  den(22) = den(1)*den(21)
  den(24) = den(1)*den(23)
  den(25) = den(2)*den(24)
  den(27) = den(1)*den(26)
  den(28) = den(2)*den(27)
  den(30) = den(2)*den(29)
  den(31) = den(1)*den(30)
  den(33) = den(1)*den(32)
  den(34) = den(7)*den(33)
  den(36) = den(4)*den(35)
  den(37) = den(5)*den(36)
  den(39) = den(5)*den(38)
  den(40) = den(4)*den(39)
  den(42) = den(5)*den(41)
  den(43) = den(4)*den(42)
  den(45) = den(4)*den(44)
  den(46) = den(5)*den(45)
  den(48) = den(4)*den(47)
  den(49) = den(5)*den(48)
  den(51) = den(5)*den(50)
  den(52) = den(4)*den(51)
  den(53) = den(5)*den(32)
  den(54) = den(7)*den(53)
  den(56) = den(4)*den(55)
  den(57) = den(7)*den(56)
  den(58) = den(2)*den(55)
  den(59) = den(7)*den(58)
  den(61) = den(1)*den(60)
  den(62) = den(7)*den(61)
  den(63) = den(4)*den(60)
  den(64) = den(7)*den(63)
  den(66) = den(5)*den(65)
  den(67) = den(7)*den(66)
  den(68) = den(2)*den(65)
  den(69) = den(7)*den(68)
  den(70) = den(13)*den(20)
  den(71) = den(1)*den(70)
  den(72) = den(10)*den(23)
  den(73) = den(2)*den(72)
  den(74) = den(12)*den(41)
  den(75) = den(4)*den(74)
  den(76) = den(11)*den(44)
  den(77) = den(5)*den(76)
  den(78) = den(1)*den(2)*den(7)
  den(79) = den(4)*den(5)*den(7)
  den(80) = den(1)*den(13)
  den(81) = den(2)*den(10)
  den(82) = den(4)*den(12)
  den(83) = den(5)*den(11)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(48)

  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_VV(wf(:,5),wf(:,6)) * den(6)
  A(3) = cont_SS(wf(:,7),wf(:,8)) * den(8)
  A(4) = cont_SS(wf(:,7),wf(:,9)) * den(9)

  A(5) = cont_VV(wf(:,1),wf(:,10)) * den(1)
  A(6) = cont_VV(wf(:,4),wf(:,11)) * den(4)
  A(7) = cont_VV(wf(:,5),wf(:,12)) * den(5)
  A(8) = cont_VV(wf(:,2),wf(:,13)) * den(2)
  A(9) = cont_VV(wf(:,2),wf(:,14)) * den(3)
  A(10) = cont_VV(wf(:,5),wf(:,15)) * den(6)
  A(11) = cont_VV(wf(:,3),wf(:,16)) * den(3)
  A(12) = cont_VV(wf(:,6),wf(:,17)) * den(6)
  A(13) = cont_SS(wf(:,7),wf(:,18)) * den(10)
  A(14) = cont_SS(wf(:,7),wf(:,19)) * den(11)
  A(15) = cont_VV(wf(:,5),wf(:,21)) * den(6)
  A(16) = cont_VV(wf(:,2),wf(:,23)) * den(3)
  A(17) = cont_SS(wf(:,7),wf(:,24)) * den(12)
  A(18) = cont_SS(wf(:,7),wf(:,25)) * den(13)
  A(19) = cont_SS(wf(:,7),wf(:,26)) * den(8)
  A(20) = cont_VV(wf(:,27),wf(:,28)) * den(16)
  A(21) = cont_VV(wf(:,29),wf(:,30)) * den(19)
  A(22) = cont_VV(wf(:,31),wf(:,32)) * den(22)
  A(23) = cont_VV(wf(:,3),wf(:,33)) * den(25)
  A(24) = cont_VV(wf(:,34),wf(:,35)) * den(28)
  A(25) = cont_VV(wf(:,36),wf(:,37)) * den(31)
  A(26) = cont_QA(wf(:,39),wf(:,40)) * den(34)
  A(27) = cont_SS(wf(:,7),wf(:,41)) * den(9)
  A(28) = cont_VV(wf(:,42),wf(:,43)) * den(37)
  A(29) = cont_VV(wf(:,44),wf(:,45)) * den(40)
  A(30) = cont_VV(wf(:,46),wf(:,47)) * den(43)
  A(31) = cont_VV(wf(:,6),wf(:,48)) * den(46)
  A(32) = cont_VV(wf(:,49),wf(:,50)) * den(49)
  A(33) = cont_VV(wf(:,51),wf(:,52)) * den(52)
  A(34) = cont_QA(wf(:,39),wf(:,54)) * den(54)
  A(35) = cont_QA(wf(:,56),wf(:,57)) * den(57)
  A(36) = cont_QA(wf(:,56),wf(:,59)) * den(59)
  A(37) = cont_QA(wf(:,61),wf(:,62)) * den(62)
  A(38) = cont_SS(wf(:,7),wf(:,63)) * den(8)
  A(39) = cont_QA(wf(:,61),wf(:,65)) * den(64)
  A(40) = cont_SS(wf(:,7),wf(:,66)) * den(9)
  A(41) = cont_QA(wf(:,68),wf(:,69)) * den(67)
  A(42) = cont_SS(wf(:,7),wf(:,70)) * den(9)
  A(43) = cont_QA(wf(:,68),wf(:,72)) * den(69)
  A(44) = cont_SS(wf(:,7),wf(:,73)) * den(8)
  A(45) = cont_VV(wf(:,32),wf(:,74)) * den(71)
  A(46) = cont_VV(wf(:,33),wf(:,75)) * den(73)
  A(47) = cont_VV(wf(:,47),wf(:,76)) * den(75)
  A(48) = cont_VV(wf(:,48),wf(:,77)) * den(77)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(48)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = -(A(1)*f(2))/2._/**/REALKIND-(A(2)*f(2))/6._/**/REALKIND+(A(3)*f(5))/2._/**/REALKIND+(A(4)*f(5))/6._/**/REALKIND
  M1(2) = (A(1)*f(2))/6._/**/REALKIND+(A(2)*f(2))/2._/**/REALKIND-(A(3)*f(5))/6._/**/REALKIND-(A(4)*f(5))/2._/**/REALKIND

  M2(1) = ((-A(20)-A(21)-A(24)-A(25))*f(1))/2._/**/REALKIND+((-A(28)-A(29)-A(32)-A(33))*f(1))/6._/**/REALKIND+((-A(9)+A(22) &
       +A(23))*f(3))/2._/**/REALKIND+((-A(10)+A(30)+A(31))*f(3))/6._/**/REALKIND+((-A(12)-A(15))*f(4))/6._/**/REALKIND+((-A(11) &
       -A(16))*f(4))/2._/**/REALKIND+((A(19)-A(45)-A(46))*f(6))/2._/**/REALKIND+((A(27)-A(47)-A(48))*f(6))/6._/**/REALKIND+((A(40) &
       +A(42))*f(7))/6._/**/REALKIND+((A(38)+A(44))*f(7))/2._/**/REALKIND+((A(6)+A(7))*f(8))/6._/**/REALKIND+((A(5) &
       +A(8))*f(8))/2._/**/REALKIND+((-A(14)-A(17))*f(9))/6._/**/REALKIND+((-A(13)-A(18))*f(9))/2._/**/REALKIND+((-A(34)-A(35) &
       -A(39)-A(41))*f(10))/6._/**/REALKIND+((-A(26)-A(36)-A(37)-A(43))*f(10))/2._/**/REALKIND
  M2(2) = ((A(20)+A(21)+A(24)+A(25))*f(1))/6._/**/REALKIND+((A(28)+A(29)+A(32)+A(33))*f(1))/2._/**/REALKIND+((A(9)-A(22) &
       -A(23))*f(3))/6._/**/REALKIND+((A(10)-A(30)-A(31))*f(3))/2._/**/REALKIND+((A(12)+A(15))*f(4))/2._/**/REALKIND+((A(11) &
       +A(16))*f(4))/6._/**/REALKIND+((-A(19)+A(45)+A(46))*f(6))/6._/**/REALKIND+((-A(27)+A(47)+A(48))*f(6))/2._/**/REALKIND+(( &
       -A(40)-A(42))*f(7))/2._/**/REALKIND+((-A(38)-A(44))*f(7))/6._/**/REALKIND+((-A(6)-A(7))*f(8))/2._/**/REALKIND+((-A(5) &
       -A(8))*f(8))/6._/**/REALKIND+((A(14)+A(17))*f(9))/2._/**/REALKIND+((A(13)+A(18))*f(9))/6._/**/REALKIND+((A(34)+A(35)+A(39) &
       +A(41))*f(10))/2._/**/REALKIND+((A(26)+A(36)+A(37)+A(43))*f(10))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_heftpphhjj_dddxdxhh_1_/**/REALKIND
