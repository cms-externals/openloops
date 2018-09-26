
module ol_colourmatrix_heftpphhjj_ddxhhgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(46,2), K2(2,2), KL(2,3)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [  48,  -6]
  K1( 2,:) = [  -6,  48]
  K1( 3,:) = [  64,  -8]
  K1( 4,:) = [  -8,  64]
  K1( 5,:) = [  -1, -10]
  K1( 6,:) = [ -10,  -1]
  K1( 7,:) = [  64,  -8]
  K1( 8,:) = [  -8,  64]
  K1( 9,:) = [   0,   0]
  K1(10,:) = [   0,   0]
  K1(11,:) = [   0,   0]
  K1(12,:) = [   0,   0]
  K1(13,:) = [   0,   0]
  K1(14,:) = [   0,   0]
  K1(15,:) = [   0,   0]
  K1(16,:) = [   0,   0]
  K1(17,:) = [   0,   0]
  K1(18,:) = [   0,   0]
  K1(19,:) = [   0,   0]
  K1(20,:) = [   0,   0]
  K1(21,:) = [   0,   0]
  K1(22,:) = [   0,   0]
  K1(23,:) = [   9,   9]
  K1(24,:) = [   9, -72]
  K1(25,:) = [ -72,   9]
  K1(26,:) = [   9,   9]
  K1(27,:) = [   0,   0]
  K1(28,:) = [   0,   0]
  K1(29,:) = [   0,   0]
  K1(30,:) = [   0,   0]
  K1(31,:) = [ 144, -18]
  K1(32,:) = [ -18, 144]
  K1(33,:) = [ -72,   9]
  K1(34,:) = [   9,   9]
  K1(35,:) = [   9,   9]
  K1(36,:) = [   9, -72]
  K1(37,:) = [   0,   0]
  K1(38,:) = [   0,   0]
  K1(39,:) = [   0,   0]
  K1(40,:) = [   0,   0]
  K1(41,:) = [ -81,   0]
  K1(42,:) = [   0, -81]
  K1(43,:) = [ 144, -18]
  K1(44,:) = [ -18, 144]
  K1(45,:) = [   0,   0]
  K1(46,:) = [   0,   0]
  K1 = (1._/**/REALKIND / 9) * K1

  K2(1,:) = [ 16, -2]
  K2(2,:) = [ -2, 16]
  K2 = (1._/**/REALKIND / 3) * K2

  KL(1,:) = [ 16, -2,  6]
  KL(2,:) = [ -2, 16,  6]
  KL = (1._/**/REALKIND / 3) * KL

  end subroutine colourmatrix_init
end module ol_colourmatrix_heftpphhjj_ddxhhgg_1_/**/REALKIND



module ol_forced_parameters_heftpphhjj_ddxhhgg_1_/**/REALKIND
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
end module ol_forced_parameters_heftpphhjj_ddxhhgg_1_/**/REALKIND

module ol_loop_heftpphhjj_ddxhhgg_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(40), c(38)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:203)
  ! denominators
  complex(REALKIND), save :: den(200)
  ! Born, CT and Loop colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,16), Mct(2,16), Mcol_loop(3,16)
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
    f( 2) = (DOI*eQED**2*gQCD**6)/(MW**2*pi**4*sw**2*1152._/**/REALKIND)
    f( 3) = (CI*eQED**2*gQCD**4)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 4) = (eQED**2*gQCD**4)/(MW**2*pi**2*sw**2*48._/**/REALKIND)
    f( 5) = (CI*countertermnorm*eQED**2*gQCD**6)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 6) = (countertermnorm*eQED**2*gQCD**6)/(MW**2*pi**2*sw**2*48._/**/REALKIND)
    f( 7) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**6)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f( 8) = (countertermnorm*ctGqq*eQED**2*gQCD**6)/(MW**2*pi**2*sw**2*48._/**/REALKIND)
    f( 9) = (countertermnorm*ctHEFTgggh*eQED**2*gQCD**6)/(MW**2*pi**2*sw**2*48._/**/REALKIND)
    f(10) = (countertermnorm*ctVVV*eQED**2*gQCD**6)/(MW**2*pi**2*sw**2*48._/**/REALKIND)
    f(11) = (CI*eQED**2*gQCD**4*lambdaHHH*MH**2)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(12) = (eQED**2*gQCD**4*lambdaHHH*MH**2)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(13) = (CI*countertermnorm*eQED**2*gQCD**6*lambdaHHH*MH**2)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(14) = (countertermnorm*eQED**2*gQCD**6*lambdaHHH*MH**2)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(15) = (CI*countertermnorm*ctGqq*eQED**2*gQCD**6*lambdaHHH*MH**2)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(16) = (countertermnorm*ctGqq*eQED**2*gQCD**6*lambdaHHH*MH**2)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(17) = (countertermnorm*ctHEFTgggh*eQED**2*gQCD**6*lambdaHHH*MH**2)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(18) = (countertermnorm*ctVVV*eQED**2*gQCD**6*lambdaHHH*MH**2)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(19) = (CI*countertermnorm*eQED**2*gQCD**6*R2HEFTghqq)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f(20) = (countertermnorm*eQED**2*gQCD**6*R2HEFTghqq)/(MW**2*pi**2*sw**2*48._/**/REALKIND)
    f(21) = (CI*countertermnorm*eQED**2*gQCD**6*lambdaHHH*MH**2*R2HEFTghqq)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(22) = (countertermnorm*eQED**2*gQCD**6*lambdaHHH*MH**2*R2HEFTghqq)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(23) = (CI*countertermnorm*eQED**2*gQCD**6*lambdaHHH*MH**2*R2HEFThqq)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(24) = (countertermnorm*eQED**2*gQCD**6*lambdaHHH*MH**2*R2HEFThqq)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(25) = (CI*eQED**2*gQCD**6*integralnorm*SwB)/(96._/**/REALKIND*MW**2*pi**2*sw**2)
    f(26) = (CI*eQED**2*gQCD**6*integralnorm*SwB)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f(27) = (eQED**2*gQCD**6*integralnorm*SwB)/(MW**2*pi**2*sw**2*96._/**/REALKIND)
    f(28) = (eQED**2*gQCD**6*integralnorm*SwB)/(MW**2*pi**2*sw**2*48._/**/REALKIND)
    f(29) = (CI*eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwB)/(32._/**/REALKIND*MW**2*pi**2*sw**2)
    f(30) = (CI*eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwB)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(31) = (eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwB)/(MW**2*pi**2*sw**2*32._/**/REALKIND)
    f(32) = (eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwB)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(33) = (CI*eQED**2*gQCD**6*integralnorm*SwF)/(24._/**/REALKIND*MW**2*pi**2*sw**2)
    f(34) = (CI*eQED**2*gQCD**6*integralnorm*SwF)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(35) = (eQED**2*gQCD**6*integralnorm*SwF)/(MW**2*pi**2*sw**2*24._/**/REALKIND)
    f(36) = (eQED**2*gQCD**6*integralnorm*SwF)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(37) = (CI*eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwF)/(8._/**/REALKIND*MW**2*pi**2*sw**2)
    f(38) = (3*CI*eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwF)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(39) = (eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwF)/(MW**2*pi**2*sw**2*8._/**/REALKIND)
    f(40) = (3*eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwF)/(MW**2*pi**2*sw**2*16._/**/REALKIND)

  c = [ 9*CI*f(25), 18*CI*f(25), CI*f(26), 3*CI*f(26), 8*CI*f(26), 9*CI*f(26), 18*CI*f(26), 18*f(27), f(28), 3*f(28), 8*f(28) &
    , 9*f(28), 18*f(28), 9*CI*f(29), 18*CI*f(29), CI*f(30), 3*CI*f(30), 8*CI*f(30), 9*CI*f(30), 18*CI*f(30), 18*f(31), f(32) &
    , 3*f(32), 8*f(32), 9*f(32), 18*f(32), 3*CI*f(33), 3*CI*f(34), f(35), 3*f(35), f(36), 3*f(36), 3*CI*f(37), 3*CI*f(38), f(39) &
    , 3*f(39), f(40), 3*f(40) ]
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
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(170)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_S(P(:,3), rMH, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_S(P(:,4), rMH, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_S(P(:,3), rMH, H(3), wf(:,-2), 0)
    call pol_wf_S(P(:,4), rMH, H(4), wf(:,-3), 0)
    call pol_wf_V(P(:,5), rZERO, H(5), wf(:,-4), 0)
    call pol_wf_V(P(:,6), rZERO, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_HHGG_G(wf(:,-2),wf(:,-3),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,2),Q(:,60))
  call vert_SS_S(wf(:,-2),wf(:,-3),wf(:,3))
  call vert_GGG_H(wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,4))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,5))
  call vert_HHG_G(wf(:,-2),wf(:,-3),wf(:,1),Q(:,3),wf(:,6),Q(:,15))
  call vert_HHG_G(wf(:,-2),wf(:,-3),wf(:,-5),Q(:,32),wf(:,7),Q(:,44))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,8))
  call vert_HHG_G(wf(:,-2),wf(:,-3),wf(:,-4),Q(:,16),wf(:,9),Q(:,28))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,10))
  call vert_VQ_A(wf(:,-4),wf(:,0),wf(:,11))
  call prop_Q_A(wf(:,11),Q(:,17),ZERO,0_intkind1,wf(:,12))
  call vert_QA_V(wf(:,12),wf(:,-1),wf(:,13))
  call vert_AV_Q(wf(:,-1),wf(:,-4),wf(:,14))
  call prop_A_Q(wf(:,14),Q(:,18),ZERO,0_intkind1,wf(:,15))
  call vert_QA_V(wf(:,0),wf(:,15),wf(:,16))
  call vert_VQ_A(wf(:,-5),wf(:,0),wf(:,17))
  call prop_Q_A(wf(:,17),Q(:,33),ZERO,0_intkind1,wf(:,18))
  call vert_QA_V(wf(:,18),wf(:,-1),wf(:,19))
  call vert_AV_Q(wf(:,-1),wf(:,-5),wf(:,20))
  call prop_A_Q(wf(:,20),Q(:,34),ZERO,0_intkind1,wf(:,21))
  call vert_QA_V(wf(:,0),wf(:,21),wf(:,22))
  call vert_HG_G(wf(:,3),wf(:,1),Q(:,3),wf(:,23),Q(:,15))
  call vert_HG_G(wf(:,3),wf(:,-5),Q(:,32),wf(:,24),Q(:,44))
  call vert_HG_G(wf(:,3),wf(:,-4),Q(:,16),wf(:,25),Q(:,28))
  call counter_HHGG_G(wf(:,-2),wf(:,-3),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,26),Q(:,60))
  call counter_AVHH_Q(wf(:,-1),wf(:,-5),wf(:,-2),wf(:,-3),wf(:,27))
  call counter_AVHH_Q(wf(:,-1),wf(:,-4),wf(:,-2),wf(:,-3),wf(:,28))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,29))
  call counter_VHHQ_A(wf(:,-5),wf(:,-2),wf(:,-3),wf(:,0),wf(:,30))
  call counter_VHHQ_A(wf(:,-4),wf(:,-2),wf(:,-3),wf(:,0),wf(:,31))
  call counter_HHQA_V(wf(:,-2),wf(:,-3),wf(:,0),wf(:,-1),wf(:,32))
  call counter_GGG_H(wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,33))
  call vert_HG_G(wf(:,-2),wf(:,-4),Q(:,16),wf(:,34),Q(:,20))
  call counter_HGG_G_vert(wf(:,-3),wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,35),Q(:,43))
  call vert_HG_G(wf(:,-2),wf(:,-5),Q(:,32),wf(:,36),Q(:,36))
  call counter_HGG_G_vert(wf(:,-3),wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,37),Q(:,27))
  call counter_HGG_G_vert(wf(:,-3),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,38),Q(:,56))
  call vert_HG_G(wf(:,-2),wf(:,1),Q(:,3),wf(:,39),Q(:,7))
  call counter_UV_W(wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,40))
  call counter_UV_W(wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,41))
  call counter_UV_W(wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,42))
  call vert_HGG_G(wf(:,-2),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,43),Q(:,52))
  call counter_HG_G_vert(wf(:,-3),wf(:,1),Q(:,3),wf(:,44),Q(:,11))
  call counter_HG_G_vert(wf(:,-3),wf(:,-5),Q(:,32),wf(:,45),Q(:,40))
  call vert_HGG_G(wf(:,-2),wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,46),Q(:,23))
  call counter_HG_G_vert(wf(:,-3),wf(:,-4),Q(:,16),wf(:,47),Q(:,24))
  call vert_HGG_G(wf(:,-2),wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,48),Q(:,39))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,49))
  call vert_HGG_G(wf(:,-3),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,50),Q(:,56))
  call counter_HG_G_vert(wf(:,-2),wf(:,1),Q(:,3),wf(:,51),Q(:,7))
  call counter_HG_G_vert(wf(:,-2),wf(:,-5),Q(:,32),wf(:,52),Q(:,36))
  call vert_HGG_G(wf(:,-3),wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,53),Q(:,27))
  call counter_HG_G_vert(wf(:,-2),wf(:,-4),Q(:,16),wf(:,54),Q(:,20))
  call vert_HGG_G(wf(:,-3),wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,55),Q(:,43))
  call vert_HG_G(wf(:,-3),wf(:,-4),Q(:,16),wf(:,56),Q(:,24))
  call counter_HGG_G_vert(wf(:,-2),wf(:,1),Q(:,3),wf(:,-5),Q(:,32),wf(:,57),Q(:,39))
  call vert_HG_G(wf(:,-3),wf(:,-5),Q(:,32),wf(:,58),Q(:,40))
  call counter_HGG_G_vert(wf(:,-2),wf(:,1),Q(:,3),wf(:,-4),Q(:,16),wf(:,59),Q(:,23))
  call counter_HGG_G_vert(wf(:,-2),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,60),Q(:,52))
  call vert_HG_G(wf(:,-3),wf(:,1),Q(:,3),wf(:,61),Q(:,11))
  call counter_HHG_G(ctHEFTggh,wf(:,-2),wf(:,-3),wf(:,1),Q(:,3),wf(:,62),Q(:,15))
  call counter_HHG_G(ctHEFTggh,wf(:,-2),wf(:,-3),wf(:,-5),Q(:,32),wf(:,63),Q(:,44))
  call counter_HHG_G(ctHEFTggh,wf(:,-2),wf(:,-3),wf(:,-4),Q(:,16),wf(:,64),Q(:,28))
  call counter_QA_V(wf(:,12),wf(:,-1),wf(:,65))
  call counter_QA_V(wf(:,18),wf(:,-1),wf(:,66))
  call counter_AV_Q(wf(:,-1),wf(:,-5),wf(:,67))
  call prop_A_Q(wf(:,67),Q(:,34),ZERO,0_intkind1,wf(:,68))
  call vert_QA_V(wf(:,0),wf(:,68),wf(:,69))
  call counter_AV_Q(wf(:,-1),wf(:,-4),wf(:,70))
  call prop_A_Q(wf(:,70),Q(:,18),ZERO,0_intkind1,wf(:,71))
  call vert_QA_V(wf(:,0),wf(:,71),wf(:,72))
  call counter_QAV_H(wf(:,12),wf(:,-1),wf(:,-5),wf(:,73))
  call counter_QAV_H(wf(:,18),wf(:,-1),wf(:,-4),wf(:,74))
  call counter_QA_V(wf(:,0),wf(:,15),wf(:,75))
  call counter_QA_V(wf(:,0),wf(:,21),wf(:,76))
  call counter_VQ_A(wf(:,-5),wf(:,0),wf(:,77))
  call prop_Q_A(wf(:,77),Q(:,33),ZERO,0_intkind1,wf(:,78))
  call vert_QA_V(wf(:,78),wf(:,-1),wf(:,79))
  call counter_VQ_A(wf(:,-4),wf(:,0),wf(:,80))
  call prop_Q_A(wf(:,80),Q(:,17),ZERO,0_intkind1,wf(:,81))
  call vert_QA_V(wf(:,81),wf(:,-1),wf(:,82))
  call vert_GGG_H(wf(:,29),Q(:,3),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,83))
  call vert_HHG_G(wf(:,-2),wf(:,-3),wf(:,29),Q(:,3),wf(:,84),Q(:,15))
  call vert_UV_W(wf(:,29),Q(:,3),wf(:,-4),Q(:,16),wf(:,85))
  call vert_UV_W(wf(:,29),Q(:,3),wf(:,-5),Q(:,32),wf(:,86))
  call counter_QAV_H(wf(:,0),wf(:,15),wf(:,-5),wf(:,87))
  call counter_QAV_H(wf(:,0),wf(:,21),wf(:,-4),wf(:,88))
  call counter_HQA_V(wf(:,3),wf(:,0),wf(:,-1),wf(:,89))
  call counter_HG_G(ctHEFTggh,wf(:,3),wf(:,1),Q(:,3),wf(:,90),Q(:,15))
  call counter_HG_G(ctHEFTggh,wf(:,3),wf(:,-5),Q(:,32),wf(:,91),Q(:,44))
  call vert_HGG_G(wf(:,3),wf(:,-4),Q(:,16),wf(:,-5),Q(:,32),wf(:,92),Q(:,60))
  call counter_HG_G(ctHEFTggh,wf(:,3),wf(:,-4),Q(:,16),wf(:,93),Q(:,28))
  call counter_HG_G_vert(wf(:,-3),wf(:,34),Q(:,20),wf(:,94),Q(:,28))
  call vert_UV_W(wf(:,34),Q(:,20),wf(:,-5),Q(:,32),wf(:,95))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,34),Q(:,20),wf(:,96))
  call counter_HG_G_vert(wf(:,-3),wf(:,36),Q(:,36),wf(:,97),Q(:,44))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,36),Q(:,36),wf(:,98))
  call counter_HG_G_vert(wf(:,-3),wf(:,5),Q(:,48),wf(:,99),Q(:,56))
  call vert_HG_G(wf(:,-2),wf(:,5),Q(:,48),wf(:,100),Q(:,52))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,45),Q(:,40),wf(:,101))
  call vert_HG_G(wf(:,-2),wf(:,45),Q(:,40),wf(:,102),Q(:,44))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,36),Q(:,36),wf(:,103))
  call vert_UV_W(wf(:,47),Q(:,24),wf(:,-5),Q(:,32),wf(:,104))
  call vert_HG_G(wf(:,-2),wf(:,47),Q(:,24),wf(:,105),Q(:,28))
  call vert_HHG_G(wf(:,-2),wf(:,-3),wf(:,5),Q(:,48),wf(:,106),Q(:,60))
  call counter_V_V(ctGG,wf(:,5),Q(:,48),wf(:,107))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,7),Q(:,44),wf(:,108))
  call counter_V_V(ctGG,wf(:,7),Q(:,44),wf(:,109))
  call vert_UV_W(wf(:,9),Q(:,28),wf(:,-5),Q(:,32),wf(:,110))
  call counter_V_V(ctGG,wf(:,9),Q(:,28),wf(:,111))
  call counter_HG_G_vert(wf(:,-2),wf(:,56),Q(:,24),wf(:,112),Q(:,28))
  call vert_UV_W(wf(:,56),Q(:,24),wf(:,-5),Q(:,32),wf(:,113))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,52),Q(:,36),wf(:,114))
  call counter_HG_G_vert(wf(:,-2),wf(:,58),Q(:,40),wf(:,115),Q(:,44))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,58),Q(:,40),wf(:,116))
  call counter_HG_G_vert(wf(:,-2),wf(:,5),Q(:,48),wf(:,117),Q(:,52))
  call vert_HG_G(wf(:,-3),wf(:,5),Q(:,48),wf(:,118),Q(:,56))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,52),Q(:,36),wf(:,119))
  call vert_HG_G(wf(:,-3),wf(:,52),Q(:,36),wf(:,120),Q(:,44))
  call vert_UV_W(wf(:,1),Q(:,3),wf(:,54),Q(:,20),wf(:,121))
  call vert_UV_W(wf(:,54),Q(:,20),wf(:,-5),Q(:,32),wf(:,122))
  call vert_HG_G(wf(:,-3),wf(:,54),Q(:,20),wf(:,123),Q(:,28))
  call counter_AQ_H(wf(:,21),Q(:,34),wf(:,12),Q(:,17),wf(:,124))
  call counter_AQ_H(wf(:,15),Q(:,18),wf(:,18),Q(:,33),wf(:,125))
  call vert_VQ_A(wf(:,34),wf(:,0),wf(:,126))
  call vert_AV_Q(wf(:,-1),wf(:,45),wf(:,127))
  call prop_Q_A(wf(:,126),Q(:,21),ZERO,0_intkind1,wf(:,128))
  call vert_VQ_A(wf(:,45),wf(:,0),wf(:,129))
  call vert_AV_Q(wf(:,-1),wf(:,34),wf(:,130))
  call prop_Q_A(wf(:,129),Q(:,41),ZERO,0_intkind1,wf(:,131))
  call vert_VQ_A(wf(:,36),wf(:,0),wf(:,132))
  call vert_AV_Q(wf(:,-1),wf(:,47),wf(:,133))
  call prop_Q_A(wf(:,132),Q(:,37),ZERO,0_intkind1,wf(:,134))
  call vert_VQ_A(wf(:,47),wf(:,0),wf(:,135))
  call vert_AV_Q(wf(:,-1),wf(:,36),wf(:,136))
  call prop_Q_A(wf(:,135),Q(:,25),ZERO,0_intkind1,wf(:,137))
  call vert_AV_Q(wf(:,-1),wf(:,7),wf(:,138))
  call counter_Q_A(ctqq,wf(:,12),Q(:,17),wf(:,139))
  call prop_A_Q(wf(:,138),Q(:,46),ZERO,0_intkind1,wf(:,140))
  call vert_VQ_A(wf(:,7),wf(:,0),wf(:,141))
  call counter_A_Q(ctqq,wf(:,15),Q(:,18),wf(:,142))
  call prop_Q_A(wf(:,141),Q(:,45),ZERO,0_intkind1,wf(:,143))
  call vert_AV_Q(wf(:,-1),wf(:,9),wf(:,144))
  call counter_Q_A(ctqq,wf(:,18),Q(:,33),wf(:,145))
  call prop_A_Q(wf(:,144),Q(:,30),ZERO,0_intkind1,wf(:,146))
  call vert_VQ_A(wf(:,9),wf(:,0),wf(:,147))
  call counter_A_Q(ctqq,wf(:,21),Q(:,34),wf(:,148))
  call prop_Q_A(wf(:,147),Q(:,29),ZERO,0_intkind1,wf(:,149))
  call vert_VQ_A(wf(:,56),wf(:,0),wf(:,150))
  call vert_AV_Q(wf(:,-1),wf(:,52),wf(:,151))
  call prop_Q_A(wf(:,150),Q(:,25),ZERO,0_intkind1,wf(:,152))
  call vert_VQ_A(wf(:,52),wf(:,0),wf(:,153))
  call vert_AV_Q(wf(:,-1),wf(:,56),wf(:,154))
  call prop_Q_A(wf(:,153),Q(:,37),ZERO,0_intkind1,wf(:,155))
  call vert_VQ_A(wf(:,58),wf(:,0),wf(:,156))
  call vert_AV_Q(wf(:,-1),wf(:,54),wf(:,157))
  call prop_Q_A(wf(:,156),Q(:,41),ZERO,0_intkind1,wf(:,158))
  call vert_VQ_A(wf(:,54),wf(:,0),wf(:,159))
  call vert_AV_Q(wf(:,-1),wf(:,58),wf(:,160))
  call prop_Q_A(wf(:,159),Q(:,21),ZERO,0_intkind1,wf(:,161))
  call vert_VQ_A(wf(:,-5),wf(:,12),wf(:,162))
  call counter_HA_Q(wf(:,3),wf(:,-1),Q(:,2),wf(:,163),Q(:,14))
  call prop_Q_A(wf(:,162),Q(:,49),ZERO,0_intkind1,wf(:,164))
  call vert_VQ_A(wf(:,-4),wf(:,18),wf(:,165))
  call prop_Q_A(wf(:,165),Q(:,49),ZERO,0_intkind1,wf(:,166))
  call vert_VQ_A(wf(:,5),wf(:,0),wf(:,167))
  call prop_Q_A(wf(:,167),Q(:,49),ZERO,0_intkind1,wf(:,168))
  call vert_AV_Q(wf(:,15),wf(:,-5),wf(:,169))
  call counter_QH_A(wf(:,0),Q(:,1),wf(:,3),wf(:,170),Q(:,13))
  call prop_A_Q(wf(:,169),Q(:,50),ZERO,0_intkind1,wf(:,171))
  call vert_AV_Q(wf(:,21),wf(:,-4),wf(:,172))
  call prop_A_Q(wf(:,172),Q(:,50),ZERO,0_intkind1,wf(:,173))
  call vert_AV_Q(wf(:,-1),wf(:,5),wf(:,174))
  call prop_A_Q(wf(:,174),Q(:,50),ZERO,0_intkind1,wf(:,175))
  call vert_HG_G(wf(:,3),wf(:,29),Q(:,3),wf(:,176),Q(:,15))
  call vert_HG_G(wf(:,3),wf(:,5),Q(:,48),wf(:,177),Q(:,60))
  call vert_UV_W(wf(:,49),Q(:,3),wf(:,-4),Q(:,16),wf(:,178))
  call vert_UV_W(wf(:,49),Q(:,3),wf(:,-5),Q(:,32),wf(:,179))
  call counter_V_V(ctGG,wf(:,8),Q(:,19),wf(:,180))
  call counter_V_V(ctGG,wf(:,25),Q(:,28),wf(:,181))
  call prop_Q_A(wf(:,139),Q(:,17),ZERO,0_intkind1,wf(:,182))
  call vert_QA_V(wf(:,182),wf(:,-1),wf(:,183))
  call counter_V_V(ctGG,wf(:,13),Q(:,19),wf(:,184))
  call prop_A_Q(wf(:,142),Q(:,18),ZERO,0_intkind1,wf(:,185))
  call vert_QA_V(wf(:,0),wf(:,185),wf(:,186))
  call counter_V_V(ctGG,wf(:,16),Q(:,19),wf(:,187))
  call prop_Q_A(wf(:,145),Q(:,33),ZERO,0_intkind1,wf(:,188))
  call vert_QA_V(wf(:,188),wf(:,-1),wf(:,189))
  call counter_V_V(ctGG,wf(:,19),Q(:,35),wf(:,190))
  call prop_A_Q(wf(:,148),Q(:,34),ZERO,0_intkind1,wf(:,191))
  call vert_QA_V(wf(:,0),wf(:,191),wf(:,192))
  call counter_V_V(ctGG,wf(:,22),Q(:,35),wf(:,193))
  call vert_VQ_A(wf(:,25),wf(:,0),wf(:,194))
  call prop_Q_A(wf(:,194),Q(:,29),ZERO,0_intkind1,wf(:,195))
  call vert_VQ_A(wf(:,24),wf(:,0),wf(:,196))
  call prop_Q_A(wf(:,196),Q(:,45),ZERO,0_intkind1,wf(:,197))
  call vert_AV_Q(wf(:,-1),wf(:,25),wf(:,198))
  call prop_A_Q(wf(:,198),Q(:,30),ZERO,0_intkind1,wf(:,199))
  call vert_AV_Q(wf(:,-1),wf(:,24),wf(:,200))
  call prop_A_Q(wf(:,200),Q(:,46),ZERO,0_intkind1,wf(:,201))
  call vert_UV_W(wf(:,25),Q(:,28),wf(:,-5),Q(:,32),wf(:,202))
  call vert_UV_W(wf(:,-4),Q(:,16),wf(:,24),Q(:,44),wf(:,203))

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
  den(4) = 1 / (Q(5,48))
  den(6) = 1 / (Q(5,44))
  den(8) = 1 / (Q(5,28))
  den(10) = 1 / (Q(5,17))
  den(12) = 1 / (Q(5,18))
  den(14) = 1 / (Q(5,33))
  den(16) = 1 / (Q(5,34))
  den(19) = 1 / (Q(5,19))
  den(28) = 1 / (Q(5,35))
  den(33) = 1 / (Q(5,60))
  den(34) = 1 / (Q(5,20))
  den(36) = 1 / (Q(5,36))
  den(38) = 1 / (Q(5,56))
  den(40) = 1 / (Q(5,52))
  den(42) = 1 / (Q(5,40))
  den(44) = 1 / (Q(5,24))
  den(65) = 1 / (Q(5,7))
  den(79) = 1 / (Q(5,15))
  den(94) = 1 / (Q(5,11))
  den(115) = 1 / (Q(5,21))
  den(118) = 1 / (Q(5,41))
  den(123) = 1 / (Q(5,37))
  den(126) = 1 / (Q(5,25))
  den(129) = 1 / (Q(5,46))
  den(133) = 1 / (Q(5,45))
  den(137) = 1 / (Q(5,30))
  den(141) = 1 / (Q(5,29))
  den(145) = 1 / (Q(5,49))
  den(155) = 1 / (Q(5,50))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(1)*den(4)
  den(7) = den(1)*den(6)
  den(9) = den(1)*den(8)
  den(11) = den(6)*den(10)
  den(13) = den(6)*den(12)
  den(15) = den(8)*den(14)
  den(17) = den(8)*den(16)
  den(18) = den(3)*den(4)
  den(20) = den(1)*den(19)
  den(21) = den(2)*den(20)
  den(22) = den(2)*den(8)
  den(23) = den(1)*den(22)
  den(24) = den(10)*den(19)
  den(25) = den(2)*den(24)
  den(26) = den(12)*den(19)
  den(27) = den(2)*den(26)
  den(29) = den(14)*den(28)
  den(30) = den(2)*den(29)
  den(31) = den(16)*den(28)
  den(32) = den(2)*den(31)
  den(35) = den(1)*den(34)
  den(37) = den(1)*den(36)
  den(39) = den(1)*den(38)
  den(41) = den(1)*den(40)
  den(43) = den(1)*den(42)
  den(45) = den(1)*den(44)
  den(46) = den(1)*den(33)
  den(47) = den(2)*den(10)
  den(48) = den(2)*den(14)
  den(49) = den(2)*den(12)
  den(50) = den(2)*den(16)
  den(51) = den(2)*den(4)
  den(52) = den(2)*den(33)
  den(53) = den(1)*den(52)
  den(54) = den(1)*den(28)
  den(55) = den(2)*den(54)
  den(56) = den(2)*den(6)
  den(57) = den(1)*den(56)
  den(58) = den(34)*den(54)
  den(59) = den(34)*den(40)
  den(60) = den(1)*den(59)
  den(61) = den(35)*den(42)
  den(62) = den(20)*den(36)
  den(63) = den(36)*den(40)
  den(64) = den(1)*den(63)
  den(66) = den(1)*den(65)
  den(67) = den(4)*den(66)
  den(68) = den(4)*den(40)
  den(69) = den(1)*den(68)
  den(70) = den(42)*den(66)
  den(71) = den(6)*den(42)
  den(72) = den(1)*den(71)
  den(73) = den(37)*den(44)
  den(74) = den(44)*den(66)
  den(75) = den(8)*den(44)
  den(76) = den(1)*den(75)
  den(77) = den(4)*den(33)
  den(78) = den(1)*den(77)
  den(80) = den(1)*den(79)
  den(81) = den(4)*den(80)
  den(82) = den(6)*den(33)
  den(83) = den(1)*den(82)
  den(84) = den(6)*den(20)
  den(85) = den(8)*den(33)
  den(86) = den(1)*den(85)
  den(87) = den(8)*den(54)
  den(88) = den(44)*den(54)
  den(89) = den(38)*den(44)
  den(90) = den(1)*den(89)
  den(91) = den(20)*den(42)
  den(92) = den(38)*den(42)
  den(93) = den(1)*den(92)
  den(95) = den(1)*den(94)
  den(96) = den(4)*den(95)
  den(97) = den(4)*den(38)
  den(98) = den(1)*den(97)
  den(99) = den(36)*den(95)
  den(100) = den(6)*den(36)
  den(101) = den(1)*den(100)
  den(102) = den(34)*den(95)
  den(103) = den(8)*den(34)
  den(104) = den(1)*den(103)
  den(105) = den(10)*den(16)
  den(106) = den(2)*den(105)
  den(107) = den(12)*den(14)
  den(108) = den(2)*den(107)
  den(109) = den(24)*den(36)
  den(110) = den(24)*den(42)
  den(111) = den(26)*den(36)
  den(112) = den(26)*den(42)
  den(113) = den(29)*den(34)
  den(114) = den(31)*den(34)
  den(116) = den(34)*den(115)
  den(117) = den(42)*den(116)
  den(119) = den(42)*den(118)
  den(120) = den(34)*den(119)
  den(121) = den(29)*den(44)
  den(122) = den(31)*den(44)
  den(124) = den(36)*den(123)
  den(125) = den(44)*den(124)
  den(127) = den(44)*den(126)
  den(128) = den(36)*den(127)
  den(130) = den(6)*den(129)
  den(131) = den(10)*den(130)
  den(132) = den(6)*den(24)
  den(134) = den(6)*den(133)
  den(135) = den(12)*den(134)
  den(136) = den(6)*den(26)
  den(138) = den(8)*den(137)
  den(139) = den(14)*den(138)
  den(140) = den(8)*den(29)
  den(142) = den(8)*den(141)
  den(143) = den(16)*den(142)
  den(144) = den(8)*den(31)
  den(146) = den(10)*den(145)
  den(147) = den(2)*den(146)
  den(148) = den(10)*den(56)
  den(149) = den(14)*den(145)
  den(150) = den(2)*den(149)
  den(151) = den(14)*den(22)
  den(152) = den(4)*den(145)
  den(153) = den(2)*den(152)
  den(154) = den(12)*den(56)
  den(156) = den(12)*den(155)
  den(157) = den(2)*den(156)
  den(158) = den(16)*den(22)
  den(159) = den(16)*den(155)
  den(160) = den(2)*den(159)
  den(161) = den(4)*den(155)
  den(162) = den(2)*den(161)
  den(163) = den(33)*den(51)
  den(164) = den(1)*den(163)
  den(165) = den(3)*den(79)
  den(166) = den(4)*den(165)
  den(167) = den(1)**2
  den(168) = den(56)*den(167)
  den(169) = den(22)*den(167)
  den(170) = den(20)*den(56)
  den(171) = den(22)*den(54)
  den(172) = den(10)**2
  den(173) = den(56)*den(172)
  den(174) = den(24)*den(56)
  den(175) = den(12)**2
  den(176) = den(56)*den(175)
  den(177) = den(26)*den(56)
  den(178) = den(14)**2
  den(179) = den(22)*den(178)
  den(180) = den(22)*den(29)
  den(181) = den(16)**2
  den(182) = den(22)*den(181)
  den(183) = den(22)*den(31)
  den(184) = den(22)*den(141)
  den(185) = den(56)*den(133)
  den(186) = den(22)*den(137)
  den(187) = den(56)*den(129)
  den(188) = den(22)*den(33)
  den(189) = den(33)*den(56)
  den(190) = den(1)*den(2)*den(4)
  den(191) = den(2)*den(10)*den(16)
  den(192) = den(2)*den(12)*den(14)
  den(193) = den(1)*den(51)
  den(194) = den(2)*den(5)
  den(195) = den(1)*den(188)
  den(196) = den(1)*den(189)
  den(197) = den(10)*den(187)
  den(198) = den(12)*den(185)
  den(199) = den(14)*den(186)
  den(200) = den(16)*den(184)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(170)

  A(1) = cont_VV(wf(:,1),wf(:,2)) * den(1)
  A(2) = cont_SS(wf(:,3),wf(:,4)) * den(3)
  A(3) = cont_VV(wf(:,5),wf(:,6)) * den(5)
  A(4) = cont_VV(wf(:,7),wf(:,8)) * den(7)
  A(5) = cont_VV(wf(:,9),wf(:,10)) * den(9)
  A(6) = cont_VV(wf(:,7),wf(:,13)) * den(11)
  A(7) = cont_VV(wf(:,7),wf(:,16)) * den(13)
  A(8) = cont_VV(wf(:,9),wf(:,19)) * den(15)
  A(9) = cont_VV(wf(:,9),wf(:,22)) * den(17)
  A(10) = cont_VV(wf(:,5),wf(:,23)) * den(18)
  A(11) = cont_VV(wf(:,8),wf(:,24)) * den(21)
  A(12) = cont_VV(wf(:,10),wf(:,25)) * den(23)
  A(13) = cont_VV(wf(:,13),wf(:,24)) * den(25)
  A(14) = cont_VV(wf(:,16),wf(:,24)) * den(27)
  A(15) = cont_VV(wf(:,19),wf(:,25)) * den(30)
  A(16) = cont_VV(wf(:,22),wf(:,25)) * den(32)

  A(17) = cont_VV(wf(:,1),wf(:,26)) * den(1)
  A(18) = cont_QA(wf(:,12),wf(:,27)) * den(10)
  A(19) = cont_QA(wf(:,18),wf(:,28)) * den(14)
  A(20) = cont_VV(wf(:,2),wf(:,29)) * den(33)
  A(21) = cont_QA(wf(:,15),wf(:,30)) * den(12)
  A(22) = cont_QA(wf(:,21),wf(:,31)) * den(16)
  A(23) = cont_VV(wf(:,5),wf(:,32)) * den(4)
  A(24) = cont_SS(wf(:,3),wf(:,33)) * den(3)
  A(25) = cont_VV(wf(:,34),wf(:,35)) * den(35)
  A(26) = cont_VV(wf(:,36),wf(:,37)) * den(37)
  A(27) = cont_VV(wf(:,38),wf(:,39)) * den(39)
  A(28) = cont_VV(wf(:,9),wf(:,40)) * den(9)
  A(29) = cont_VV(wf(:,7),wf(:,41)) * den(7)
  A(30) = cont_VV(wf(:,6),wf(:,42)) * den(5)
  A(31) = cont_VV(wf(:,43),wf(:,44)) * den(41)
  A(32) = cont_VV(wf(:,45),wf(:,46)) * den(43)
  A(33) = cont_VV(wf(:,47),wf(:,48)) * den(45)
  A(34) = cont_VV(wf(:,2),wf(:,49)) * den(46)
  A(35) = cont_VV(wf(:,50),wf(:,51)) * den(39)
  A(36) = cont_VV(wf(:,52),wf(:,53)) * den(37)
  A(37) = cont_VV(wf(:,54),wf(:,55)) * den(35)
  A(38) = cont_VV(wf(:,56),wf(:,57)) * den(45)
  A(39) = cont_VV(wf(:,58),wf(:,59)) * den(43)
  A(40) = cont_VV(wf(:,60),wf(:,61)) * den(41)
  A(41) = cont_VV(wf(:,5),wf(:,62)) * den(5)
  A(42) = cont_VV(wf(:,8),wf(:,63)) * den(7)
  A(43) = cont_VV(wf(:,10),wf(:,64)) * den(9)
  A(44) = cont_VV(wf(:,13),wf(:,63)) * den(11)
  A(45) = cont_VV(wf(:,16),wf(:,63)) * den(13)
  A(46) = cont_VV(wf(:,19),wf(:,64)) * den(15)
  A(47) = cont_VV(wf(:,22),wf(:,64)) * den(17)
  A(48) = cont_VV(wf(:,7),wf(:,65)) * den(11)
  A(49) = cont_VV(wf(:,9),wf(:,66)) * den(15)
  A(50) = cont_VV(wf(:,9),wf(:,69)) * den(17)
  A(51) = cont_VV(wf(:,7),wf(:,72)) * den(13)
  A(52) = cont_SS(wf(:,3),wf(:,73)) * den(47)
  A(53) = cont_SS(wf(:,3),wf(:,74)) * den(48)
  A(54) = cont_VV(wf(:,7),wf(:,75)) * den(13)
  A(55) = cont_VV(wf(:,9),wf(:,76)) * den(17)
  A(56) = cont_VV(wf(:,9),wf(:,79)) * den(15)
  A(57) = cont_VV(wf(:,7),wf(:,82)) * den(11)
  A(58) = cont_SS(wf(:,3),wf(:,83)) * den(3)
  A(59) = cont_VV(wf(:,5),wf(:,84)) * den(5)
  A(60) = cont_VV(wf(:,7),wf(:,85)) * den(7)
  A(61) = cont_VV(wf(:,9),wf(:,86)) * den(9)
  A(62) = cont_SS(wf(:,3),wf(:,87)) * den(49)
  A(63) = cont_SS(wf(:,3),wf(:,88)) * den(50)
  A(64) = cont_VV(wf(:,5),wf(:,89)) * den(51)
  A(65) = cont_VV(wf(:,5),wf(:,90)) * den(18)
  A(66) = cont_VV(wf(:,8),wf(:,91)) * den(21)
  A(67) = cont_VV(wf(:,25),wf(:,40)) * den(23)
  A(68) = cont_VV(wf(:,49),wf(:,92)) * den(53)
  A(69) = cont_VV(wf(:,10),wf(:,93)) * den(55)
  A(70) = cont_VV(wf(:,24),wf(:,41)) * den(57)
  A(71) = cont_VV(wf(:,23),wf(:,42)) * den(18)
  A(72) = cont_VV(wf(:,10),wf(:,94)) * den(58)
  A(73) = cont_VV(wf(:,44),wf(:,95)) * den(60)
  A(74) = cont_VV(wf(:,45),wf(:,96)) * den(61)
  A(75) = cont_VV(wf(:,8),wf(:,97)) * den(62)
  A(76) = cont_VV(wf(:,44),wf(:,98)) * den(64)
  A(77) = cont_VV(wf(:,39),wf(:,99)) * den(67)
  A(78) = cont_VV(wf(:,44),wf(:,100)) * den(69)
  A(79) = cont_VV(wf(:,39),wf(:,101)) * den(70)
  A(80) = cont_VV(wf(:,8),wf(:,102)) * den(72)
  A(81) = cont_VV(wf(:,47),wf(:,103)) * den(73)
  A(82) = cont_VV(wf(:,39),wf(:,104)) * den(74)
  A(83) = cont_VV(wf(:,10),wf(:,105)) * den(76)
  A(84) = cont_VV(wf(:,49),wf(:,106)) * den(78)
  A(85) = cont_VV(wf(:,6),wf(:,107)) * den(81)
  A(86) = cont_VV(wf(:,49),wf(:,108)) * den(83)
  A(87) = cont_VV(wf(:,8),wf(:,109)) * den(84)
  A(88) = cont_VV(wf(:,49),wf(:,110)) * den(86)
  A(89) = cont_VV(wf(:,10),wf(:,111)) * den(87)
  A(90) = cont_VV(wf(:,10),wf(:,112)) * den(88)
  A(91) = cont_VV(wf(:,51),wf(:,113)) * den(90)
  A(92) = cont_VV(wf(:,56),wf(:,114)) * den(73)
  A(93) = cont_VV(wf(:,8),wf(:,115)) * den(91)
  A(94) = cont_VV(wf(:,51),wf(:,116)) * den(93)
  A(95) = cont_VV(wf(:,61),wf(:,117)) * den(96)
  A(96) = cont_VV(wf(:,51),wf(:,118)) * den(98)
  A(97) = cont_VV(wf(:,61),wf(:,119)) * den(99)
  A(98) = cont_VV(wf(:,8),wf(:,120)) * den(101)
  A(99) = cont_VV(wf(:,58),wf(:,121)) * den(61)
  A(100) = cont_VV(wf(:,61),wf(:,122)) * den(102)
  A(101) = cont_VV(wf(:,10),wf(:,123)) * den(104)
  A(102) = cont_SS(wf(:,3),wf(:,124)) * den(106)
  A(103) = cont_VV(wf(:,13),wf(:,91)) * den(25)
  A(104) = cont_SS(wf(:,3),wf(:,125)) * den(108)
  A(105) = cont_VV(wf(:,16),wf(:,91)) * den(27)
  A(106) = cont_VV(wf(:,19),wf(:,93)) * den(30)
  A(107) = cont_VV(wf(:,22),wf(:,93)) * den(32)
  A(108) = cont_VV(wf(:,13),wf(:,97)) * den(109)
  A(109) = cont_VV(wf(:,13),wf(:,102)) * den(110)
  A(110) = cont_VV(wf(:,16),wf(:,97)) * den(111)
  A(111) = cont_VV(wf(:,16),wf(:,102)) * den(112)
  A(112) = cont_VV(wf(:,19),wf(:,94)) * den(113)
  A(113) = cont_VV(wf(:,22),wf(:,94)) * den(114)
  A(114) = cont_QA(wf(:,127),wf(:,128)) * den(117)
  A(115) = cont_QA(wf(:,130),wf(:,131)) * den(120)
  A(116) = cont_VV(wf(:,19),wf(:,105)) * den(121)
  A(117) = cont_VV(wf(:,22),wf(:,105)) * den(122)
  A(118) = cont_QA(wf(:,133),wf(:,134)) * den(125)
  A(119) = cont_QA(wf(:,136),wf(:,137)) * den(128)
  A(120) = cont_QA(wf(:,139),wf(:,140)) * den(131)
  A(121) = cont_VV(wf(:,13),wf(:,109)) * den(132)
  A(122) = cont_QA(wf(:,142),wf(:,143)) * den(135)
  A(123) = cont_VV(wf(:,16),wf(:,109)) * den(136)
  A(124) = cont_QA(wf(:,145),wf(:,146)) * den(139)
  A(125) = cont_VV(wf(:,19),wf(:,111)) * den(140)
  A(126) = cont_QA(wf(:,148),wf(:,149)) * den(143)
  A(127) = cont_VV(wf(:,22),wf(:,111)) * den(144)
  A(128) = cont_VV(wf(:,13),wf(:,115)) * den(110)
  A(129) = cont_VV(wf(:,13),wf(:,120)) * den(109)
  A(130) = cont_VV(wf(:,16),wf(:,115)) * den(112)
  A(131) = cont_VV(wf(:,16),wf(:,120)) * den(111)
  A(132) = cont_VV(wf(:,19),wf(:,112)) * den(121)
  A(133) = cont_VV(wf(:,22),wf(:,112)) * den(122)
  A(134) = cont_QA(wf(:,151),wf(:,152)) * den(128)
  A(135) = cont_QA(wf(:,154),wf(:,155)) * den(125)
  A(136) = cont_VV(wf(:,19),wf(:,123)) * den(113)
  A(137) = cont_VV(wf(:,22),wf(:,123)) * den(114)
  A(138) = cont_QA(wf(:,157),wf(:,158)) * den(120)
  A(139) = cont_QA(wf(:,160),wf(:,161)) * den(117)
  A(140) = cont_QA(wf(:,163),wf(:,164)) * den(147)
  A(141) = cont_VV(wf(:,24),wf(:,65)) * den(148)
  A(142) = cont_QA(wf(:,163),wf(:,166)) * den(150)
  A(143) = cont_VV(wf(:,25),wf(:,66)) * den(151)
  A(144) = cont_QA(wf(:,163),wf(:,168)) * den(153)
  A(145) = cont_VV(wf(:,25),wf(:,69)) * den(32)
  A(146) = cont_VV(wf(:,24),wf(:,72)) * den(27)
  A(147) = cont_VV(wf(:,24),wf(:,75)) * den(154)
  A(148) = cont_QA(wf(:,170),wf(:,171)) * den(157)
  A(149) = cont_VV(wf(:,25),wf(:,76)) * den(158)
  A(150) = cont_QA(wf(:,170),wf(:,173)) * den(160)
  A(151) = cont_QA(wf(:,170),wf(:,175)) * den(162)
  A(152) = cont_VV(wf(:,25),wf(:,79)) * den(30)
  A(153) = cont_VV(wf(:,24),wf(:,82)) * den(25)
  A(154) = cont_VV(wf(:,5),wf(:,176)) * den(18)
  A(155) = cont_VV(wf(:,24),wf(:,85)) * den(21)
  A(156) = cont_VV(wf(:,25),wf(:,86)) * den(23)
  A(157) = cont_VV(wf(:,49),wf(:,177)) * den(164)
  A(158) = cont_VV(wf(:,23),wf(:,107)) * den(166)
  A(159) = cont_VV(wf(:,24),wf(:,178)) * den(168)
  A(160) = cont_VV(wf(:,25),wf(:,179)) * den(169)
  A(161) = cont_VV(wf(:,24),wf(:,180)) * den(170)
  A(162) = cont_VV(wf(:,10),wf(:,181)) * den(171)
  A(163) = cont_VV(wf(:,24),wf(:,183)) * den(173)
  A(164) = cont_VV(wf(:,24),wf(:,184)) * den(174)
  A(165) = cont_VV(wf(:,24),wf(:,186)) * den(176)
  A(166) = cont_VV(wf(:,24),wf(:,187)) * den(177)
  A(167) = cont_VV(wf(:,25),wf(:,189)) * den(179)
  A(168) = cont_VV(wf(:,25),wf(:,190)) * den(180)
  A(169) = cont_VV(wf(:,25),wf(:,192)) * den(182)
  A(170) = cont_VV(wf(:,25),wf(:,193)) * den(183)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(170)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = (A(7)+A(8))*f(3)+CI*(A(1)+A(3)+A(4)-A(5))*f(4)+(-A(14)-A(15))*f(11)+CI*(-A(2)-A(10)-A(11)+A(12))*f(12)
  M1(2) = (A(6)+A(9))*f(3)+CI*(-A(1)-A(3)-A(4)+A(5))*f(4)+(-A(13)-A(16))*f(11)+CI*(A(2)+A(10)+A(11)-A(12))*f(12)

  M2(1) = (A(110)+A(111)+A(112)+A(115)+A(116)+A(118)+A(130)+A(131)+A(132)+A(135)+A(136)+A(138))*f(1)+CI*(-A(25)+A(26)+A(27)+A(31) &
       +A(32)-A(33)+A(35)+A(36)-A(37)-A(38)+A(39)+A(40)-A(72)+A(73)+A(74)+A(75)+A(76)+A(77)+A(78)+A(79)+A(80)-A(81)+A(82)-A(83) &
       -A(90)+A(91)-A(92)+A(93)+A(94)+A(95)+A(96)+A(97)+A(98)+A(99)+A(100)-A(101))*f(2)+(A(45)+A(46)-A(122)-A(123)-A(124) &
       -A(125))*f(5)+CI*(-A(34)+A(41)+A(42)-A(43)-A(84)-A(85)-A(86)-A(87)-A(88)+A(89))*f(6)+(A(49)+A(51)+A(54)+A(56))*f(7) &
       +CI*(A(20)+A(59)+A(60)-A(61))*f(8)+CI*A(17)*f(9)+CI*(-A(28)+A(29)+A(30))*f(10)+(-A(105)-A(106)+A(165)+A(166)+A(167) &
       +A(168))*f(13)+CI*(-A(65)-A(66)+A(68)+A(69)+A(157)+A(158)+A(159)-A(160)+A(161)-A(162))*f(14)+(-A(143)-A(146)-A(147) &
       -A(152))*f(15)+CI*(-A(58)-A(154)-A(155)+A(156))*f(16)-CI*A(24)*f(17)+CI*(A(67)-A(70)-A(71))*f(18)+(-A(19)-A(21))*f(19) &
       -CI*A(23)*f(20)+(A(53)+A(62))*f(21)+CI*A(64)*f(22)+(A(104)+A(142)+A(148))*f(23)+CI*(A(144)+A(151))*f(24)
  M2(2) = (A(108)+A(109)+A(113)+A(114)+A(117)+A(119)+A(128)+A(129)+A(133)+A(134)+A(137)+A(139))*f(1)+CI*(A(25)-A(26)-A(27)-A(31) &
       -A(32)+A(33)-A(35)-A(36)+A(37)+A(38)-A(39)-A(40)+A(72)-A(73)-A(74)-A(75)-A(76)-A(77)-A(78)-A(79)-A(80)+A(81)-A(82)+A(83) &
       +A(90)-A(91)+A(92)-A(93)-A(94)-A(95)-A(96)-A(97)-A(98)-A(99)-A(100)+A(101))*f(2)+(A(44)+A(47)-A(120)-A(121)-A(126) &
       -A(127))*f(5)+CI*(A(34)-A(41)-A(42)+A(43)+A(84)+A(85)+A(86)+A(87)+A(88)-A(89))*f(6)+(A(48)+A(50)+A(55)+A(57))*f(7)+CI*( &
       -A(20)-A(59)-A(60)+A(61))*f(8)-CI*A(17)*f(9)+CI*(A(28)-A(29)-A(30))*f(10)+(-A(103)-A(107)+A(163)+A(164)+A(169) &
       +A(170))*f(13)+CI*(A(65)+A(66)-A(68)-A(69)-A(157)-A(158)-A(159)+A(160)-A(161)+A(162))*f(14)+(-A(141)-A(145)-A(149) &
       -A(153))*f(15)+CI*(A(58)+A(154)+A(155)-A(156))*f(16)+CI*A(24)*f(17)+CI*(-A(67)+A(70)+A(71))*f(18)+(-A(18)-A(22))*f(19) &
       +CI*A(23)*f(20)+(A(52)+A(63))*f(21)-CI*A(64)*f(22)+(A(102)+A(140)+A(150))*f(23)+CI*(-A(144)-A(151))*f(24)

end subroutine colourvectors

end module ol_loop_heftpphhjj_ddxhhgg_1_/**/REALKIND
