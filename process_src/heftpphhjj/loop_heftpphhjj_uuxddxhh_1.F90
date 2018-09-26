
module ol_colourmatrix_heftpphhjj_uuxddxhh_1_/**/REALKIND
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
  K1( 5,:) = [   0,  -4]
  K1( 6,:) = [  -4, -12]
  K1( 7,:) = [  12,   4]
  K1( 8,:) = [   4,  12]
  K1( 9,:) = [   0,   4]
  K1(10,:) = [   4,   0]
  K1(11,:) = [ -12,  -4]
  K1(12,:) = [  -4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [ -12,  -4]
  K1(16,:) = [  -4,   0]
  K1(17,:) = [   0,   4]
  K1(18,:) = [   4,   0]
  K1(19,:) = [   0,  -4]
  K1(20,:) = [  -4, -12]
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
end module ol_colourmatrix_heftpphhjj_uuxddxhh_1_/**/REALKIND



module ol_forced_parameters_heftpphhjj_uuxddxhh_1_/**/REALKIND
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
end module ol_forced_parameters_heftpphhjj_uuxddxhh_1_/**/REALKIND

module ol_loop_heftpphhjj_uuxddxhh_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(21), c(34)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-6+1:45)
  ! denominators
  complex(REALKIND), save :: den(48)
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
    f(10) = (CI*countertermnorm*eQED**2*gQCD**6*R2HEFThqq)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f(11) = (CI*countertermnorm*eQED**2*gQCD**6*lambdaHHH*MH**2*R2HEFThqq)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(12) = (CI*eQED**2*gQCD**6*integralnorm*SwB)/(48._/**/REALKIND*MW**2*pi**2*sw**2)
    f(13) = (eQED**2*gQCD**6*integralnorm*SwB)/(MW**2*pi**2*sw**2*96._/**/REALKIND)
    f(14) = (eQED**2*gQCD**6*integralnorm*SwB)/(MW**2*pi**2*sw**2*48._/**/REALKIND)
    f(15) = (CI*eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwB)/(16._/**/REALKIND*MW**2*pi**2*sw**2)
    f(16) = (eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwB)/(MW**2*pi**2*sw**2*32._/**/REALKIND)
    f(17) = (eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwB)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(18) = (eQED**2*gQCD**6*integralnorm*SwF)/(MW**2*pi**2*sw**2*24._/**/REALKIND)
    f(19) = (eQED**2*gQCD**6*integralnorm*SwF)/(MW**2*pi**2*sw**2*16._/**/REALKIND)
    f(20) = (eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwF)/(MW**2*pi**2*sw**2*8._/**/REALKIND)
    f(21) = (3*eQED**2*gQCD**6*integralnorm*lambdaHHH*MH**2*SwF)/(MW**2*pi**2*sw**2*16._/**/REALKIND)

  c = [ 9*CI*f(12), 27*CI*f(12), 18*f(13), 54*f(13), f(14), 3*f(14), 6*f(14), 8*f(14), 10*f(14), 18*f(14), 21*f(14), 24*f(14) &
    , 54*f(14), 9*CI*f(15), 27*CI*f(15), 18*f(16), 54*f(16), f(17), 3*f(17), 6*f(17), 8*f(17), 10*f(17), 18*f(17), 21*f(17) &
    , 24*f(17), 54*f(17), 3*f(18), 9*f(18), 3*f(19), 9*f(19), 3*f(20), 9*f(20), 3*f(21), 9*f(21) ]
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
  complex(REALKIND) :: A(26)
  ! external WFs
  if (present(POLSEL)) then
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), POLSEL(1))
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), POLSEL(2))
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), POLSEL(3))
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), POLSEL(4))
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), POLSEL(5))
    call pol_wf_S(P(:,6), rMH, H(6), wf(:,-5), POLSEL(6))

  else
    call pol_wf_Q(P(:,1), rZERO, H(1), wf(:,0), 0)
    call pol_wf_A(P(:,2), rZERO, H(2), wf(:,-1), 0)
    call pol_wf_Q(P(:,3), rZERO, H(3), wf(:,-2), 0)
    call pol_wf_A(P(:,4), rZERO, H(4), wf(:,-3), 0)
    call pol_wf_S(P(:,5), rMH, H(5), wf(:,-4), 0)
    call pol_wf_S(P(:,6), rMH, H(6), wf(:,-5), 0)

  end if

  ! internal WFs
  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call vert_HHG_G(wf(:,-4),wf(:,-5),wf(:,1),Q(:,3),wf(:,3),Q(:,51))
  call vert_SS_S(wf(:,-4),wf(:,-5),wf(:,4))
  call vert_GG_H(wf(:,1),Q(:,3),wf(:,2),Q(:,12),wf(:,5))
  call counter_HHQA_V(wf(:,-4),wf(:,-5),wf(:,-2),wf(:,-3),wf(:,6))
  call counter_HHQA_V(wf(:,-4),wf(:,-5),wf(:,0),wf(:,-1),wf(:,7))
  call counter_HHG_G(ctHEFTggh,wf(:,-4),wf(:,-5),wf(:,1),Q(:,3),wf(:,8),Q(:,51))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,9))
  call counter_QAV_H(wf(:,-2),wf(:,-3),wf(:,1),wf(:,10))
  call counter_HHA_Q(wf(:,-4),wf(:,-5),wf(:,-1),Q(:,2),wf(:,11),Q(:,50))
  call prop_A_Q(wf(:,11),Q(:,50),ZERO,0_intkind1,wf(:,12))
  call vert_VQ_A(wf(:,2),wf(:,0),wf(:,13))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,14))
  call vert_HHG_G(wf(:,-4),wf(:,-5),wf(:,14),Q(:,3),wf(:,15),Q(:,51))
  call counter_QHH_A(wf(:,0),Q(:,1),wf(:,-4),wf(:,-5),wf(:,16),Q(:,49))
  call prop_Q_A(wf(:,16),Q(:,49),ZERO,0_intkind1,wf(:,17))
  call vert_AV_Q(wf(:,-1),wf(:,2),wf(:,18))
  call counter_QAV_H(wf(:,0),wf(:,-1),wf(:,2),wf(:,19))
  call counter_GG_H(ctHEFTggh,wf(:,1),Q(:,3),wf(:,2),Q(:,12),wf(:,20))
  call vert_HG_G(wf(:,-4),wf(:,1),Q(:,3),wf(:,21),Q(:,19))
  call counter_HG_G_vert(wf(:,-5),wf(:,2),Q(:,12),wf(:,22),Q(:,44))
  call vert_HG_G(wf(:,-4),wf(:,2),Q(:,12),wf(:,23),Q(:,28))
  call counter_HG_G_vert(wf(:,-5),wf(:,1),Q(:,3),wf(:,24),Q(:,35))
  call vert_HHG_G(wf(:,-4),wf(:,-5),wf(:,2),Q(:,12),wf(:,25),Q(:,60))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,26))
  call counter_V_V(ctGG,wf(:,2),Q(:,12),wf(:,27))
  call vert_HG_G(wf(:,-5),wf(:,1),Q(:,3),wf(:,28),Q(:,35))
  call counter_HG_G_vert(wf(:,-4),wf(:,2),Q(:,12),wf(:,29),Q(:,28))
  call vert_HG_G(wf(:,-5),wf(:,2),Q(:,12),wf(:,30),Q(:,44))
  call counter_HG_G_vert(wf(:,-4),wf(:,1),Q(:,3),wf(:,31),Q(:,19))
  call vert_VQ_A(wf(:,1),wf(:,-2),wf(:,32))
  call counter_HA_Q(wf(:,4),wf(:,-3),Q(:,8),wf(:,33),Q(:,56))
  call prop_Q_A(wf(:,32),Q(:,7),ZERO,0_intkind1,wf(:,34))
  call vert_AV_Q(wf(:,-3),wf(:,1),wf(:,35))
  call counter_QH_A(wf(:,-2),Q(:,4),wf(:,4),wf(:,36),Q(:,52))
  call prop_A_Q(wf(:,35),Q(:,11),ZERO,0_intkind1,wf(:,37))
  call vert_GG_H(wf(:,1),Q(:,3),wf(:,9),Q(:,12),wf(:,38))
  call counter_HA_Q(wf(:,4),wf(:,-1),Q(:,2),wf(:,39),Q(:,50))
  call prop_Q_A(wf(:,13),Q(:,13),ZERO,0_intkind1,wf(:,40))
  call counter_QH_A(wf(:,0),Q(:,1),wf(:,4),wf(:,41),Q(:,49))
  call prop_A_Q(wf(:,18),Q(:,14),ZERO,0_intkind1,wf(:,42))
  call vert_GG_H(wf(:,14),Q(:,3),wf(:,2),Q(:,12),wf(:,43))
  call vert_HG_G(wf(:,4),wf(:,2),Q(:,12),wf(:,44),Q(:,60))
  call vert_HG_G(wf(:,4),wf(:,1),Q(:,3),wf(:,45),Q(:,51))

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
  den(4) = 1 / (Q(5,48) - MH2)
  den(7) = 1 / (Q(5,50))
  den(9) = 1 / (Q(5,49))
  den(12) = 1 / (Q(5,19))
  den(15) = 1 / (Q(5,28))
  den(18) = 1 / (Q(5,60))
  den(21) = 1 / (Q(5,51))
  den(24) = 1 / (Q(5,35))
  den(27) = 1 / (Q(5,44))
  den(30) = 1 / (Q(5,7))
  den(33) = 1 / (Q(5,11))
  den(36) = 1 / (Q(5,13))
  den(39) = 1 / (Q(5,14))

  ! denominators
  den(3) = den(1)*den(2)
  den(5) = den(3)*den(4)
  den(6) = den(1)*den(4)
  den(8) = den(2)*den(7)
  den(10) = den(2)*den(9)
  den(11) = den(2)*den(4)
  den(13) = den(1)*den(12)
  den(14) = den(2)*den(13)
  den(16) = den(2)*den(15)
  den(17) = den(1)*den(16)
  den(19) = den(2)*den(18)
  den(20) = den(1)*den(19)
  den(22) = den(1)*den(21)
  den(23) = den(2)*den(22)
  den(25) = den(1)*den(24)
  den(26) = den(2)*den(25)
  den(28) = den(2)*den(27)
  den(29) = den(1)*den(28)
  den(31) = den(1)*den(30)
  den(32) = den(4)*den(31)
  den(34) = den(1)*den(33)
  den(35) = den(4)*den(34)
  den(37) = den(2)*den(36)
  den(38) = den(4)*den(37)
  den(40) = den(2)*den(39)
  den(41) = den(4)*den(40)
  den(42) = den(11)*den(18)
  den(43) = den(1)*den(42)
  den(44) = den(6)*den(21)
  den(45) = den(2)*den(44)
  den(46) = den(1)*den(2)*den(4)
  den(47) = den(1)*den(11)
  den(48) = den(2)*den(6)

end subroutine denominators


subroutine diagrams(A)
  ! colour stripped tree amplitudes
  use ol_contractions_/**/REALKIND
  implicit none
  complex(REALKIND), intent(out) :: A(26)

  A(1) = cont_VV(wf(:,2),wf(:,3)) * den(3)
  A(2) = cont_SS(wf(:,4),wf(:,5)) * den(5)

  A(3) = cont_VV(wf(:,1),wf(:,6)) * den(1)
  A(4) = cont_VV(wf(:,2),wf(:,7)) * den(2)
  A(5) = cont_VV(wf(:,2),wf(:,8)) * den(3)
  A(6) = cont_VV(wf(:,3),wf(:,9)) * den(3)
  A(7) = cont_SS(wf(:,4),wf(:,10)) * den(6)
  A(8) = cont_QA(wf(:,12),wf(:,13)) * den(8)
  A(9) = cont_VV(wf(:,2),wf(:,15)) * den(3)
  A(10) = cont_QA(wf(:,17),wf(:,18)) * den(10)
  A(11) = cont_SS(wf(:,4),wf(:,19)) * den(11)
  A(12) = cont_SS(wf(:,4),wf(:,20)) * den(5)
  A(13) = cont_VV(wf(:,21),wf(:,22)) * den(14)
  A(14) = cont_VV(wf(:,23),wf(:,24)) * den(17)
  A(15) = cont_VV(wf(:,25),wf(:,26)) * den(20)
  A(16) = cont_VV(wf(:,3),wf(:,27)) * den(23)
  A(17) = cont_VV(wf(:,28),wf(:,29)) * den(26)
  A(18) = cont_VV(wf(:,30),wf(:,31)) * den(29)
  A(19) = cont_QA(wf(:,33),wf(:,34)) * den(32)
  A(20) = cont_QA(wf(:,36),wf(:,37)) * den(35)
  A(21) = cont_SS(wf(:,4),wf(:,38)) * den(5)
  A(22) = cont_QA(wf(:,39),wf(:,40)) * den(38)
  A(23) = cont_QA(wf(:,41),wf(:,42)) * den(41)
  A(24) = cont_SS(wf(:,4),wf(:,43)) * den(5)
  A(25) = cont_VV(wf(:,26),wf(:,44)) * den(43)
  A(26) = cont_VV(wf(:,27),wf(:,45)) * den(45)

end subroutine diagrams


subroutine colourvectors(A, M1, M2)
  ! Born and counterterm colour vectors
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in) :: A(26)
  complex(REALKIND), intent(out) :: M1(2), M2(2)

  M1(1) = (A(1)*f(2))/2._/**/REALKIND-(A(2)*f(5))/2._/**/REALKIND
  M1(2) = -(A(1)*f(2))/6._/**/REALKIND+(A(2)*f(5))/6._/**/REALKIND

  M2(1) = ((A(13)+A(14)+A(17)+A(18))*f(1))/2._/**/REALKIND+((A(5)-A(15)-A(16))*f(3))/2._/**/REALKIND+((A(6) &
       +A(9))*f(4))/2._/**/REALKIND+((-A(12)+A(25)+A(26))*f(6))/2._/**/REALKIND+((-A(21)-A(24))*f(7))/2._/**/REALKIND+((-A(3) &
       -A(4))*f(8))/2._/**/REALKIND+((A(7)+A(11))*f(9))/2._/**/REALKIND+((-A(8)-A(10))*f(10))/2._/**/REALKIND+((A(19)+A(20)+A(22) &
       +A(23))*f(11))/2._/**/REALKIND
  M2(2) = ((-A(13)-A(14)-A(17)-A(18))*f(1))/6._/**/REALKIND+((-A(5)+A(15)+A(16))*f(3))/6._/**/REALKIND+((-A(6) &
       -A(9))*f(4))/6._/**/REALKIND+((A(12)-A(25)-A(26))*f(6))/6._/**/REALKIND+((A(21)+A(24))*f(7))/6._/**/REALKIND+((A(3) &
       +A(4))*f(8))/6._/**/REALKIND+((-A(7)-A(11))*f(9))/6._/**/REALKIND+((A(8)+A(10))*f(10))/6._/**/REALKIND+((-A(19)-A(20)-A(22) &
       -A(23))*f(11))/6._/**/REALKIND

end subroutine colourvectors

end module ol_loop_heftpphhjj_uuxddxhh_1_/**/REALKIND
