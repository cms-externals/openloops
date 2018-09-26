! 1
! The number above is the number of vamp routines for this process.
! This is needed by the build system.

module ol_vamp_ppwjj_ckm_cxdddxwx_1_/**/REALKIND
  contains

! **********************************************************************
subroutine vamp(M, mode, hel)
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_vamp_1_ppwjj_ckm_cxdddxwx_1_/**/REALKIND, only: vamp_1

  implicit none
  complex(REALKIND), intent(in) :: M(2)
  integer, intent(in) :: mode, hel ! recycle mode & hel config number

#ifdef LOOPSQUARED
  M = 0
#endif

  ! Call subroutines for all branches to calculate loop coefficient tensors
  call vamp_1(M,mode,hel)

end subroutine vamp



! **********************************************************************
subroutine vamp2base(P_scatt, M2L0, M2L1, IRL1, M2L2, IRL2, mode)
! P_scatt(0:3,Npart) = external momenta
! M2tree = helicity-summed squared tree matrix element for anti-charm down down anti-down W+ -> 0
! M2loop0 = helicity-summed squared loop matrix element for anti-charm down down anti-down W+ -> 0
! M2loop1 = IR1, M2loop2 = IR2 are dummy values for the single and double poles
! IR0, IR1, IR2 = finite, single pole, and double pole IR contribution
! mode = 1 (default): full matrix element;
!        2: reuse and scale coefficients from the last call;
!        note that scalings cannot be reset
! **********************************************************************
  use KIND_TYPES, only: REALKIND, DREALKIND
  use ol_debug, only: ol_fatal
  use ol_generic, only: to_string
  use ol_external_ppwjj_ckm_cxdddxwx_1, only: &
    & external_perm_inv_ppwjj_ckm_cxdddxwx_1, &
    & average_factor_ppwjj_ckm_cxdddxwx_1, &
    external_perm_ppwjj_ckm_cxdddxwx_1
  use ol_external_ppwjj_ckm_cxdddxwx_1, only: &
    & hel_not_initialised, hel_init, H, H_HC, POLSEL
  use ol_colourmatrix_ppwjj_ckm_cxdddxwx_1_/**/REALKIND ! colmat_not_initialised, colourmatrix_init, K1, K2, KL, KL2, KL2ct, KL2ct2
  use ol_kinematics_/**/REALKIND, only: conv_mom_scatt2in, internal_momenta
  use ol_parameters_decl_/**/REALKIND ! parameters_status, scalefactor, <masses>
  ! tensorrankuse: for compatibility with old OL versions only insert if rank > 6
  use ol_parameters_init_/**/REALKIND, only: ensure_mp_loop_init
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  use ol_loop_parameters_decl_/**/DREALKIND, only: IR_is_on, ioperator_mode
  use ol_forced_parameters_ppwjj_ckm_cxdddxwx_1_/**/REALKIND, only: &
    & check_forced_parameters
  use ol_momenta_decl_/**/DREALKIND, only: momenta_nan_check
  use ol_momenta_decl_/**/REALKIND, only: QInvariantsMatrix
  use ol_tensor_sum_storage_ppwjj_ckm_cxdddxwx_1_/**/REALKIND, only: &
    & reset_tensor_sum, integrate_tensor_sum, scale_tensor_sum
  use ol_loop_ppwjj_ckm_cxdddxwx_1_/**/REALKIND, only: &
    & fac_status_loop1, fac_status_loop2, zerohel, M0, Mct, Mcol_loop, &
    & fac_init_loop, tree_wavefunctions
  use ol_i_operator_/**/REALKIND, only: intdip
  use ol_wavefunctions_/**/REALKIND, only: wf_V_Std
  use ol_kinematics_/**/REALKIND, only: LC2Std_Rep_cmplx
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,5)
  real(REALKIND),  intent(out) :: M2L0, M2L1, IRL1(0:2), M2L2, IRL2(0:4)
  integer, intent(in), optional :: mode
  real(REALKIND)       :: P(0:3,5)
  integer              :: la, i, j, colmatpos, recycle_mode
  complex(REALKIND)    :: M0_col1(2)
!  complex(REALKIND)    :: Mcol_loop(2), Mct(2)
  real(REALKIND)       :: M2colint(16), M2CC(5,5), M2CC_EW ! colour correlations
  integer,        save :: ntry = 1
  real(REALKIND)       :: extmasses2(5)
  real(REALKIND)       :: M2hel, M2ct, M2L2ct, M2L2ct2, vdip, c_dip(0:2)
  real(REALKIND)       :: scalebackfactor
  integer              :: cr, k, m, n, l, hels(2,4)
  real(REALKIND)       :: P_scatt_intern(0:3,5)
  complex(REALKIND)    :: epLC(1:4), epStd(1:4,2)
  real(REALKIND)       :: M2hels(4)


  if (present(mode)) then
    recycle_mode = mode
  else
    recycle_mode = 1
  end if

  call ensure_mp_loop_init()
  if (fac_status_loop1 /= parameters_status .or. fac_status_loop2 /= loop_parameters_status) then
    ! Note: if fac_init would only be called when parameters changed which are relevant for the factors,
    ! a different 'status' would have to be used, because forced_parameters should be called after every parameter change.
    call check_forced_parameters()
    call fac_init_loop()
  end if
  if (hel_not_initialised) call hel_init()
  if (colmat_not_initialised) call colourmatrix_init()


  M2L0      = 0
  M2L1      = 0
  M2ct      = 0
  Mcol_loop = 0
  M2colint  = 0
  M2L2      = 0
  M2L2ct    = 0
  M2L2ct2   = 0
  IRL1      = 0
  IRL2      = 0

  if (momenta_nan_check(P_scatt) /= 0) return

  ! Convert 2 -> n-2 PS-point to n -> 0 (so that P(1) + ... + P(n) = 0)
  extmasses2 = [ rZERO2, rZERO2, rZERO2, rZERO2, rMW2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, &
     & external_perm_inv_ppwjj_ckm_cxdddxwx_1, 5)
  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P, 5)

#ifdef LOOPSQUARED
  call integrate_tensor_sum(M2L1)
#else
  if (recycle_mode == 1) call reset_tensor_sum()
#endif

  la = 0
  do
    la = la + 1
    if (la > 48) then
      exit
    end if

    if (ntry > 1 .and. zerohel(la)) then
      cycle
    else if (ntry == 1) then
      call tree_wavefunctions(P, H(:,la), M0(:,la), Mct(:,la))
    else
      call tree_wavefunctions(P, H(:,la), M0(:,la), Mct(:,la), POLSEL)
    end if
#if 2 > 0
    do j = 1, 2
      M2L0 = M2L0 + real(sum(conjg(M0(:,la))*K1(1:2,j))*M0(j,la))
    end do
#endif
#if 2 > 0
    do j = 1, 2
      M2ct = M2ct + real(sum(conjg(M0(:,la))*K2(:,j))*Mct(j,la))
    end do
#endif
    do j = 1, 2
      M0_col1(j) = sum(conjg(M0(:,la))*KL(:,j))
    end do
#if 17 > 1
    if (IR_is_on > 0) then
      do j = 1, 17-1
        colmatpos = 2*j
        do i = 1, 2
          M2colint(j) = M2colint(j) + real(conjg(M0(i,la))*sum(K1(i+colmatpos,:)*M0(:,la)))
        end do
      end do
    end if
#endif

#ifdef LOOPSQUARED
    call vamp(Mcol_loop(:,la), recycle_mode, la)
    ! tree-loop interference
    M2L1 = M2L1 + real(sum(M0_col1*Mcol_loop(:,la)))
    ! loop^2
    M2hel = 0
    do j = 1, 2
      M2hel = M2hel + real(sum(conjg(Mcol_loop(:,la))*KL2(1:2,j))*Mcol_loop(j,la))
    end do
    M2L2 = M2L2 + M2hel
#if 2 > 0
    do j = 1, 2
      M2L2ct  = M2L2ct  + real(sum(conjg(Mcol_loop(:,la))*KL2ct(1:2,j))*Mct(j,la))
      M2L2ct2 = M2L2ct2 + real(sum(conjg(Mct(:,la))*KL2ct2(1: 2,j))*Mct(j,la))
    end do
#endif
! #ifdef LOOPSQUARED
#else
    if (recycle_mode == 1) then
      call vamp(M0_col1, recycle_mode, la)
    end if
! #ifdef LOOPSQUARED
#endif
    if (ntry == 1) then
#ifdef LOOPSQUARED
      if (M2hel /= 0) zerohel(la) = .false.
#else
      if (count(M0(:,la) /= 0) > 0) zerohel(la) = .false.
#endif
      if (la == 48) then
        ntry = ntry + 1
        if (any(POLSEL /= 0)) then
          M2L0            = 0
          M2L1            = 0
          Mct(:,la)      = 0
          Mcol_loop(:,la) = 0
          M2colint        = 0
          M2L2            = 0
          M2L2ct          = 0
          M2L2ct2         = 0
          la = 0
          cycle
        end if
      end if
    end if
  end do

#ifdef LOOPSQUARED
  M2L2 = (M2L2 + 2*M2L2ct + M2L2ct2) / average_factor_ppwjj_ckm_cxdddxwx_1
#else
  if (recycle_mode == 2) then
    call scale_tensor_sum()
  end if
  call integrate_tensor_sum(M2L1)
#endif

  if (IR_is_on > 0) then
    do i = 1, 5
      do j = 1, i
        ! Why does this work without permuting the colour correlation matrices?
        ! M2CC(i,j) = M2colint(extcomb_perm_uxuexe_uxuexe(i*(i-1)/2+j))
        M2CC(i,j) = M2colint(i*(i-1)/2+j)
      end do
    end do
    do j = 2, 5
      do i = 1, j-1
        M2CC(i,j) = M2CC(j,i)
      end do
    end do
    M2CC_EW = M2colint(5*(5+1)/2+1)
    call intdip(ioperator_mode, M2L0, M2CC, M2CC_EW, [2,2,2,2,3], [-2,-1,-1,1,3]/3._/**/REALKIND, &
      & 5, extmasses2, QInvariantsMatrix, vdip, c_dip)
    IRL1(0) = c_dip(0) / average_factor_ppwjj_ckm_cxdddxwx_1
    IRL1(1) = c_dip(1) / average_factor_ppwjj_ckm_cxdddxwx_1
    IRL1(2) = c_dip(2) / average_factor_ppwjj_ckm_cxdddxwx_1
  else
    vdip = 0
    IRL1 = 0
  end if

  ! loop^2 IR contribution: not implemented
  IRL2 = 0

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2L0 = M2L0 / average_factor_ppwjj_ckm_cxdddxwx_1
  M2L1 = 2*(M2L1 + M2ct)
  if (IR_is_on > 1) then
    M2L1 = M2L1 + vdip
  end if
  M2L1 = M2L1 / average_factor_ppwjj_ckm_cxdddxwx_1

  ! check for NaN result
  if (M2L0 /= M2L0) then
    M2L0 = 0
  end if
  if (M2L1 /= M2L1) then
    M2L1 = 0
    M2L2 = 0
    IRL1 = 0
    IRL2 = 0
  end if

  scalebackfactor = scalefactor**(2*5-8)
  M2L0 = scalebackfactor * M2L0
  M2L1 = scalebackfactor * M2L1
  IRL1 = scalebackfactor * IRL1
  M2L2 = scalebackfactor * M2L2
  IRL2 = scalebackfactor * IRL2

end subroutine vamp2base


#ifdef LOOPSQUARED
! **********************************************************************
subroutine vamp2cache(P_scatt, M2L0, M2L1, M2L2, corr)
! P_scatt(0:3,Npart) = external momenta
! M2L0 = helicity-summed squared tree matrix element for anti-charm down down anti-down W+ -> 0
! M2L2 = helicity-summed tree x loop matrix element for anti-charm down down anti-down W+ -> 0
! M2L2 = helicity-summed squared loop matrix element for anti-charm down down anti-down W+ -> 0
! corr = loop-correlators with
! corr%type = 1: cc
! corr%type = 2: sc
! corr%type = 3: powhegsc
! a calculated from cached matrix elements
! **********************************************************************
  use KIND_TYPES, only: REALKIND, DREALKIND
  use ol_debug, only: ol_fatal
  use ol_external_ppwjj_ckm_cxdddxwx_1, only: average_factor_ppwjj_ckm_cxdddxwx_1, H, H_HC, &
   external_perm_ppwjj_ckm_cxdddxwx_1, extcomb_perm_ppwjj_ckm_cxdddxwx_1
  use ol_colourmatrix_ppwjj_ckm_cxdddxwx_1_/**/REALKIND ! colmat_not_initialised, colourmatrix_init, K1, K2, KL, KL2, KL2ct, KL2ct2
  use ol_parameters_decl_/**/REALKIND ! parameters_status, scalefactor, <masses>
  use ol_momenta_decl_/**/DREALKIND, only: momenta_nan_check
  use ol_loop_ppwjj_ckm_cxdddxwx_1_/**/REALKIND, only: zerohel, M0, Mct, Mcol_loop
  use ol_wavefunctions_/**/REALKIND, only: wf_V_Std
  use ol_kinematics_/**/REALKIND, only: LC2Std_Rep_cmplx
  use ol_helicity_bookkeeping_/**/REALKIND, only: flip_phase
  use ol_data_types_/**/DREALKIND, only: correlator
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,5)
  real(REALKIND),  intent(out) :: M2L0, M2L1, M2L2
  real(REALKIND)       :: P_scatt_intern(0:3,5)
  real(REALKIND)       :: mom(0:3)
  type(correlator), target, intent(inout), optional :: corr
  integer              :: la, i, j, colmatpos, recycle_mode
  real(REALKIND)       :: M2colint(16)
  integer,        save :: ntry = 1
  real(REALKIND)       :: M2hel, M2ct
  real(REALKIND)       :: M2L2cc(0:16)
  integer              :: cr, k, m, n, l, hels(2,4)
  integer              :: extcombs(16), extcombs_permuted(16)
  integer              :: nextcombs, emitter
  complex(REALKIND)    :: epLC(1:4), epStd(1:4,2)
  real(REALKIND)       :: M2hels(4)
  complex(REALKIND)    :: omega(2), om ! phases for helicity correlations

  cr = 0
  if (present(corr)) cr = corr%type
  if (cr == 1) then
    extcombs = [(i, i = 0, 16-1)]
    nextcombs = 5*(5+1)/2
  else if (cr == 2) then
    extcombs = corr%extcombs
    nextcombs = corr%nextcombs
  else
    nextcombs = 0
  end if

  emitter = external_perm_ppwjj_ckm_cxdddxwx_1(corr%emitter)
  do k = 1, nextcombs
    extcombs_permuted(k) = extcomb_perm_ppwjj_ckm_cxdddxwx_1(extcombs(k))
  end do

  M2L0     = 0
  M2L1     = 0
  M2ct     = 0
  M2colint = 0
  M2L2cc   = 0

  if (momenta_nan_check(P_scatt) /= 0) return

#ifdef LOOPSQUARED
  ! LOOPSQUARED correlators
  if (cr == 1) then

    do la=1,48
#if 2 > 0
        call colint(Mcol_loop(:,la), Mcol_loop(:,la), 2, KL2, extcombs_permuted(1:nextcombs), M2L2cc)  ! <M1|M1>
        call colint(2*Mcol_loop(:,la), Mct(:,la), 2, KL2ct, extcombs_permuted(1:nextcombs), M2L2cc)    ! 2*<M1|Mct>
#endif
#if 2 > 0
        call colint(Mct(:,la), Mct(:,la), 2, KL2ct2, extcombs_permuted(1:nextcombs), M2L2cc)            ! <Mct|Mct>
#endif
    end do
    do k = 0, 16
      corr%rescc(k) = M2L2cc(extcomb_perm_ppwjj_ckm_cxdddxwx_1(k)) / average_factor_ppwjj_ckm_cxdddxwx_1
    end do
    M2L2 = corr%rescc(0)

  else if (cr == 2) then
    P_scatt_intern = P_scatt
    mom = corr%mom
    corr%rescc=0
    call flip_phase(P_scatt_intern(:,corr%emitter), -1, mom, omega)
    do k=1,48,2
      do m=0,1
        do n=0,1
        hels(:,2*m+n+1)=[ H_HC(k+n,emitter) , H_HC(k+m,emitter) ]
        end do
      end do
      do l=1,size(hels,2)
        if (hels(1,l) == hels(2,l)) then
          om = 1
        else
          om = omega((H(emitter,hels(1,l))+1)/2+1)
        end if
#if 2 > 0
          call colint(Mcol_loop(:,hels(1,l)), om*Mcol_loop(:,hels(2,l)), 2, KL2, extcombs_permuted(1:nextcombs), M2L2cc) ! <M1|M1>
          call colint(Mcol_loop(:,hels(1,l)), om*Mct(:,hels(2,l)), 2, KL2ct, extcombs_permuted(1:nextcombs), M2L2cc)     ! <M1|Mct>
          call colint(om*Mcol_loop(:,hels(1,l)), Mct(:,hels(2,l)), 2, KL2ct, extcombs_permuted(1:nextcombs), M2L2cc)     ! <Mct|M1>
#endif
#if 2 > 0
          call colint(Mct(:,hels(1,l)), om*Mct(:,hels(2,l)), 2, KL2ct2, extcombs_permuted(1:nextcombs), M2L2cc)           ! <Mct|Mct>
#endif
      end do
    end do

    do k = 0, 16
      corr%rescc(k) = 0.5_/**/REALKIND*M2L2cc(extcomb_perm_ppwjj_ckm_cxdddxwx_1(k)) / average_factor_ppwjj_ckm_cxdddxwx_1
    end do

  else if (cr == 3) then ! L^mu_nu
  ! get wfs
    P_scatt_intern = P_scatt
    call wf_V_Std(P_scatt_intern(:,corr%emitter), rZERO, -1, epLC)
    call LC2Std_Rep_cmplx(epLC, epStd(:,1))
    call wf_V_Std(P_scatt_intern(:,corr%emitter), rZERO, 1, epLC)
    call LC2Std_Rep_cmplx(epLC, epStd(:,2))

    corr%resmunu = 0
    do k=1,48,2
      M2hels = 0
      do m=0,1
        do n=0,1
        hels(:,2*m+n+1)=[ H_HC(k+n,emitter) , H_HC(k+m,emitter) ]
        end do
      end do
      do l=1,size(hels,2)
#if 2 > 0
        call colint(Mcol_loop(:,hels(1,l)), Mcol_loop(:,hels(2,l)), 2, KL2(1:2,:), [0], M2hels(l:l))  ! <M1|M1>
#endif
#if 2 > 0
        call colint(2*Mcol_loop(:,hels(1,l)), Mct(:,hels(2,l)), 2, KL2ct(1:2,:), [0], M2hels(l:l))     ! <M1|Mct>
        call colint(Mct(:,hels(1,l)), Mct(:,hels(2,l)), 2, KL2ct2(1:2,:), [0], M2hels(l:l))             ! <Mct|Mct>
#endif
      ! B^(mu,nu) = sum_(k) sum_(l,-l) M^*_(k) M_(k) (eps^(mu)_(l))^* eps^(nu)_(-l)
        do m = 1,4
          do n = 1,4
            corr%resmunu(m,n) = corr%resmunu(m,n) + M2hels(l)*conjg(epStd(m,(H(emitter,hels(1,l))+1)/2+1))* &
                                                 & epStd(n,(H(emitter,hels(2,l))+1)/2+1)
          end do
        end do
      end do
    end do
    if (any(corr%resmunu(:,:) /= corr%resmunu(:,:))) then
      corr%resmunu(m,n) = 0
    else
      corr%resmunu = corr%resmunu / average_factor_ppwjj_ckm_cxdddxwx_1
      M2L2 = -corr%resmunu(1,1)+corr%resmunu(2,2)+corr%resmunu(3,3)+corr%resmunu(4,4) ! |M1|^2 = -B^mu_mu
    end if
  end if
#else
  ! LOOPxBORN correlators. Todo
#endif

  ! check for NaN result
  if (M2L2 /= M2L2) then
    M2L2 = 0
  end if

end subroutine vamp2cache
#endif


! **********************************************************************
subroutine colint(M1, M2, ncolourbasis, K, extcombs, M2sum)
! M1(i)   = <M|Ci> colour component of matrix element1
! M2(i)   = <M|Cj> colour component of matrix element2
! M2sum   = <M1|M2>
!         = Sum_{i,j} <M1|Ci> * <Ci|Cj> * <Cj|M2>
!         = colour-summed squared matrix element
! K(i,j) = <Ci|Cj>
! ncolourbasis = K(ncolourbasis*extcombs,:)
! M2sum is an array which contains the colour interference for each colour matrix
! The elements of the array extcombs specifies for which external particle
! combinations the colour correlations will be calculated. For particles i,j: i*(i-1)/2+j
! i=j=0 -> 0 means no colour insertion.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), intent(in) :: M1(:)
  complex(REALKIND), intent(in) :: M2(:)
  complex(REALKIND), intent(in) :: K(:,:)
  integer, intent(in)           :: ncolourbasis, extcombs(:)
  real(REALKIND), intent(inout) :: M2sum(:)
  integer :: i, j, l, colmatpos

  do j = 1, size(extcombs)
    l = extcombs(j)
    colmatpos = ncolourbasis*l
    do i = 1, ncolourbasis
        M2sum(l+1) = M2sum(l+1) + real(conjg(M1(i))*sum(K(i+colmatpos,:)*M2(:)))
    end do
  end do
end subroutine colint



! **********************************************************************
subroutine ctamp2base(P_scatt, M2tree, M2ct)
! The part of vamp2 which calculates tree and counter-term
! matrix elements, but not the loop. R2 is deactivated.
! Does not calculate loop^2 counterterms/R2.
! P_scatt(0:3,Npart) = external momenta
! M2tree = helicity-summed squared tree matrix element for anti-charm down down anti-down W+ -> 0
! M2ct   = helicity-summed counterterm matrix element
! **********************************************************************
  use KIND_TYPES, only: REALKIND, DREALKIND
  use ol_external_ppwjj_ckm_cxdddxwx_1, only: hel_not_initialised, hel_init, H
  use ol_colourmatrix_ppwjj_ckm_cxdddxwx_1_/**/REALKIND, only: &
    & colmat_not_initialised, colourmatrix_init, K1, K2
  use ol_parameters_decl_/**/REALKIND ! parameters_status, scalefactor, <masses>
  use ol_loop_parameters_decl_/**/DREALKIND, only: &
    & loop_parameters_status, CT_is_on, R2_is_on, TP_is_on
!  use ol_loop_parameters_decl_/**/DREALKIND, only: DOI
  use ol_init, only: parameters_flush
  use ol_parameters_init_/**/REALKIND, only: ensure_mp_loop_init, loop_parameters_init
  use ol_init, only: set_parameter
  use ol_external_ppwjj_ckm_cxdddxwx_1, only: &
    & external_perm_inv_ppwjj_ckm_cxdddxwx_1, average_factor_ppwjj_ckm_cxdddxwx_1
  use ol_momenta_decl_/**/DREALKIND, only: momenta_nan_check
  use ol_kinematics_/**/REALKIND, only: conv_mom_scatt2in, internal_momenta
  use ol_forced_parameters_ppwjj_ckm_cxdddxwx_1_/**/REALKIND, only: check_forced_parameters
  use ol_loop_ppwjj_ckm_cxdddxwx_1_/**/REALKIND, only: &
    & fac_status_loop1, fac_status_loop2, zerohel_ct, M0, fac_init_loop, tree_wavefunctions
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,5)
  real(REALKIND),  intent(out) :: M2tree, M2ct
  real(REALKIND)    :: P(0:3,5)
  integer           :: la, j
  complex(REALKIND) :: Mct(2)
  integer,  save    :: ntry = 1
  real(REALKIND)    :: extmasses2(5)
  integer           :: CT_on_bak, R2_on_bak, TP_on_bak
!  integer           :: DOI_bak
  real(REALKIND)    :: scalebackfactor

  CT_on_bak = CT_is_on
  R2_on_bak = R2_is_on
  TP_on_bak = TP_is_on
!  DOI_bak   = DOI
  CT_is_on = 1
  R2_is_on = 0
  TP_is_on = 0
!  DOI      = 0

  call loop_parameters_init()
  call ensure_mp_loop_init()

  extmasses2 = [ rZERO2, rZERO2, rZERO2, rZERO2, rMW2 ]
  if (hel_not_initialised) call hel_init()
  if (colmat_not_initialised) call colourmatrix_init()
  if (fac_status_loop1 /= parameters_status .or. fac_status_loop2 /= loop_parameters_status) then
    ! Note: if fac_init would only be called when parameters changed which are relevant for the factors,
    ! a different 'status' would have to be used, because forced_parameters should be called after every parameter change.
    call check_forced_parameters()
    call fac_init_loop()
  end if

  if (momenta_nan_check(P_scatt) /= 0) then
    M2tree = 0
    M2ct   = 0
    return
  end if

  ! Convert 2 -> n-2 PS-point to n -> 0 (so that P(1) + ... + P(n) = 0)
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppwjj_ckm_cxdddxwx_1, 5)
  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P, 5)

  M2tree = 0
  M2ct   = 0

  do la = 1, 48
    if (ntry <= 2) then
      call tree_wavefunctions(P, H(:,la), M0(:,la), Mct)
      if (count(M0(:,la) /= 0) > 0) zerohel_ct(la) = .false.
      if (la == 48) ntry = ntry + 1
    else
      if (zerohel_ct(la)) cycle
      call tree_wavefunctions(P, H(:,la), M0(:,la), Mct)
    end if
#if 2 > 0
    do j = 1, 2
      M2tree = M2tree + real(sum(conjg(M0(:,la))*K1(1:2,j))*M0(j,la))
    end do
#endif
#if 2 > 0
    do j = 1, 2
      M2ct = M2ct + real(sum(conjg(M0(:,la))*K2(1:2,j))*Mct(j))
    end do
#endif
  end do

  CT_is_on = CT_on_bak
  R2_is_on = R2_on_bak
  TP_is_on = TP_on_bak
!  DOI      = DOI_bak

  call loop_parameters_init()
  call ensure_mp_loop_init()

  scalebackfactor = scalefactor**(2*5-8)
  M2tree = scalebackfactor * M2tree / average_factor_ppwjj_ckm_cxdddxwx_1
  M2ct   = scalebackfactor * 2*M2ct / average_factor_ppwjj_ckm_cxdddxwx_1

end subroutine ctamp2base



subroutine vamp2pc(P_scatt, M2L0, M2L1, IRL1, M2L2, IRL2, mode)
  ! polecheck routine: perform multiple calls of vamp2
  ! with different values of the UV/IR poles
  ! to determine the coefficients of the IR poles.
  ! Note: loop correlators are not calculated when polecheck is activated.
  use KIND_TYPES, only: REALKIND, DREALKIND
  use ol_parameters_decl_/**/DREALKIND, only: rZERO_dp => rZERO
  use ol_parameters_init_/**/REALKIND, only: ensure_mp_loop_init
  use ol_loop_parameters_decl_/**/REALKIND, only: &
    & de1_UV, de1_IR, de2_i_IR
  use ol_loop_parameters_decl_/**/DREALKIND, only: &
    & polescale_dp => polescale, polecheck_is, CT_is_on, IR_is_on
  use ol_parameters_init_/**/DREALKIND, only: loop_parameters_init
  implicit none
  real(DREALKIND), intent(in)  :: P_scatt(:,:)
  real(REALKIND),  intent(out) :: M2L0, M2L1(0:2), IRL1(0:2), M2L2(0:4), IRL2(0:4)
  integer, intent(in), optional :: mode
  real(DREALKIND) :: poleUV1bak, poleIR1bak, poleIR2bak
  integer         :: CT_on_bak, IR_on_bak
  real(REALKIND)  :: M2L1_00, M2L1_10, M2L1_01, M2L1_20, M2L1_02, M2L1_11
  real(REALKIND)  :: M2L2_00, M2L2_10, M2L2_01, M2L2_20, M2L2_02, M2L2_11
  real(REALKIND)  :: IRL1x(0:2), IRL2x(0:4)

  if (polecheck_is == 0) then
    call vamp2base(P_scatt, M2L0, M2L1(0), IRL1, M2L2(0), IRL2, mode=mode)
    if (IR_is_on == 2) then
      M2L1(1:2) = 0
      M2L2(1:4) = 0
    else
      M2L1(1:2) = -IRL1(1:2)
      M2L2(1:4) = -IRL2(1:4)
    end if
  else
    ! remember original parameters
    poleUV1bak = de1_UV
    poleIR1bak = de1_IR
    poleIR2bak = de2_i_IR
    CT_on_bak  = CT_is_on
    IR_on_bak  = IR_is_on
    ! three calls with different IR poles; IR subtractions are calculated but not added
    call loop_parameters_init(pole1_UV=rZERO_dp, pole1_IR=rZERO_dp, pole2_IR=rZERO_dp, CT_on=1, IR_on=1)
    call ensure_mp_loop_init()
    call vamp2base(P_scatt, M2L0, M2L1_00, IRL1, M2L2_00, IRL2, mode)
    call loop_parameters_init(pole1_UV=polescale_dp, pole1_IR=polescale_dp, pole2_IR=rZERO_dp, IR_on=0)
    call ensure_mp_loop_init()
    call vamp2base(P_scatt, M2L0, M2L1_10, IRL1x, M2L2_10, IRL2x, mode)
    call loop_parameters_init(pole1_UV=rZERO_dp, pole1_IR=rZERO_dp, pole2_IR=polescale_dp)
    call ensure_mp_loop_init()
    call vamp2base(P_scatt, M2L0, M2L1_01, IRL1x, M2L2_01, IRL2x, mode)

    M2L1(0) = M2L1_00
    M2L1(1) = (M2L1_10 - M2L1_00)/polescale_dp
    M2L1(2) = (M2L1_01 - M2L1_00)/polescale_dp

#ifdef LOOPSQUARED
    call loop_parameters_init(pole1_UV=polescale_dp, pole1_IR=2*polescale_dp, pole2_IR=rZERO_dp)
    call ensure_mp_loop_init()
    call vamp2base(P_scatt, M2L0, M2L1_20, IRL1x, M2L2_20, IRL2x)
    call loop_parameters_init(pole1_UV=rZERO_dp, pole1_IR=rZERO_dp, pole2_IR=2*polescale_dp)
    call ensure_mp_loop_init()
    call vamp2base(P_scatt, M2L0, M2L1_02, IRL1x, M2L2_02, IRL2x)
    call loop_parameters_init(pole1_UV=rZERO_dp, pole1_IR=polescale_dp, pole2_IR=polescale_dp)
    call ensure_mp_loop_init()
    call vamp2base(P_scatt, M2L0, M2L1_11, IRL1x, M2L2_11, IRL2x)

    M2L2(0) = M2L2_00
    M2L2(1) = (-3*M2L2_00 - M2L2_20 + 4*M2L2_10)/(2*polescale_dp)
    M2L2(2) = (-3*M2L2_00 + 4*M2L2_01 - M2L2_02)/(2*polescale_dp) + (M2L2_00 - 2*M2L2_10 + M2L2_20)/(2*polescale_dp**2)
    M2L2(3) = (M2L2_00 - M2L2_01 - M2L2_10 + M2L2_11)/polescale_dp**2
    M2L2(4) = (M2L2_00 + M2L2_02 - 2*M2L2_01)/(2*polescale_dp**2)
#endif
    ! restore original parameters
    call loop_parameters_init(pole1_UV=poleUV1bak, pole1_IR=poleIR1bak, pole2_IR=poleIR2bak, &
      & CT_on=CT_on_bak, IR_on=IR_on_bak)
    call ensure_mp_loop_init()
    if (IR_on_bak == 2) then
      M2L1 = M2L1 + IRL1
      M2L2 = M2L2 + IRL2
    end if
  end if

end subroutine vamp2pc

end module ol_vamp_ppwjj_ckm_cxdddxwx_1_/**/REALKIND



#ifdef PRECISION_dp

module ol_vamp_ppwjj_ckm_cxdddxwx_1
  use ol_parameters_decl_/**/DREALKIND, only: procname_length
  use ol_data_types_/**/DREALKIND, only: me_cache
  implicit none
  character(procname_length) :: processname = 'ppwjj_ckm_cxdddxwx_1'
  integer, save :: qp_eval = 0, killed = 0
  integer, save :: npoints(8) = 0
  integer, save :: stability_histogram(20) = 0, stability_histogram_qp(20) = 0
  type(me_cache), allocatable, target, save :: me_caches(:)
  logical, save :: first_call = .true.
  contains

  subroutine finish_ppwjj_ckm_cxdddxwx_1()
    ! final update of the stability histogram
    use KIND_TYPES, only: DREALKIND
    use ol_stability, only: finish_histograms
    use ol_vamp_ppwjj_ckm_cxdddxwx_1_/**/DREALKIND, only: vamp
    implicit none
    complex(DREALKIND) :: Mdummy(2)
    integer :: k
    call vamp(Mdummy, -1, -1)
    if (allocated(me_caches)) then
      do k = 1, size(me_caches)
        if (allocated(me_caches(k)%psp)) deallocate(me_caches(k)%psp)
        if (allocated(me_caches(k)%me)) deallocate(me_caches(k)%me)
      end do
      deallocate(me_caches)
    end if
    call finish_histograms(processname, stability_histogram, stability_histogram_qp, npoints, qp_eval, killed)
  end subroutine finish_ppwjj_ckm_cxdddxwx_1


  subroutine vamp2(P_scatt, M2L0, M2L1, IRL1, M2L2, IRL2) &
      & bind(c,name="ol_f_vamp2_ppwjj_ckm_cxdddxwx_1")
    use KIND_TYPES, only: DREALKIND
    use ol_init, only: register_cleanup
    use ol_external_ppwjj_ckm_cxdddxwx_1, only: external_perm_ppwjj_ckm_cxdddxwx_1
    use ol_vamp_ppwjj_ckm_cxdddxwx_1_/**/DREALKIND, only: vamp2dp => vamp2pc
#ifdef USE_qp
    use ol_vamp_ppwjj_ckm_cxdddxwx_1_/**/QREALKIND, only: vamp2qp => vamp2pc
#endif
    use ol_stability, only: vamp2generic
    implicit none
    real(DREALKIND), intent(in)  :: P_scatt(0:3,5)
    real(DREALKIND), intent(out) :: M2L0, M2L1(0:2), IRL1(0:2), M2L2(0:4), IRL2(0:4)
    real(DREALKIND), save :: abs_kfactor_threshold = 1, trigeff_local = 0, sum_M2tree = 0
#ifndef USE_qp
    logical :: vamp2qp = .false.
#endif
    if (first_call) then
      call register_cleanup(finish_ppwjj_ckm_cxdddxwx_1)
      first_call = .false.
    end if
    call vamp2generic(vamp2dp, vamp2qp, processname, P_scatt, M2L0, M2L1, IRL1, M2L2, IRL2, &
                    & abs_kfactor_threshold, trigeff_local, sum_M2tree, &
                    & npoints, qp_eval, killed, stability_histogram, stability_histogram_qp, &
                    & external_perm_ppwjj_ckm_cxdddxwx_1, me_caches)
  end subroutine vamp2



#ifdef LOOPSQUARED
! **********************************************************************
  subroutine vampcr2(P_scatt, M2L0, M2L1, M2L2, crtype, &
      & emitter, nextcombs, extcombs, mom, M2cc, M2munu) &
      & bind(c,name="ol_f_vampcr2_ppwjj_ckm_cxdddxwx_1")
! Interface for loop^2 correlators.
! crtype = 1: cc
! crtype = 2: sc
! crtype = 3: powhegsc
! **********************************************************************
    use KIND_TYPES, only: DREALKIND
    use ol_external_ppwjj_ckm_cxdddxwx_1, only: external_perm_ppwjj_ckm_cxdddxwx_1
    use ol_vamp_ppwjj_ckm_cxdddxwx_1_/**/DREALKIND, only: vamp2cache
    use ol_data_types_/**/DREALKIND, only: correlator
    implicit none
    real(DREALKIND), intent(in)  :: P_scatt(0:3,5)
    real(DREALKIND), intent(out) :: M2L0, M2L1, M2L2
    integer, intent(in) :: crtype
    integer, intent(in) :: emitter
    integer, intent(in) :: nextcombs
    integer, intent(in) :: extcombs(nextcombs)
    real(DREALKIND), intent(in) :: mom(4)
    real(REALKIND),  intent(out) :: M2cc(0:5*(5+1)/2+1)
    real(DREALKIND), intent(out) :: M2munu(4,4)
    integer :: n_cc = 5*(5+1)/2
    type(correlator) :: corr
    corr%type=crtype
    M2L0 = 0
    M2L1 = 0
    M2L2 = 0
    if (crtype == 0) then
      M2cc = 0
      M2munu = 0
      return
    else if (crtype == 1) then
      allocate(corr%rescc(0:5*(5+1)/2+1))
      call vamp2cache(P_scatt, M2L0, M2L1, M2L2, corr=corr)
      M2cc = corr%rescc
      deallocate(corr%rescc)
    else if (crtype == 2) then
      corr%emitter=emitter
      corr%mom=mom
      corr%nextcombs=nextcombs
      allocate(corr%extcombs(nextcombs))
      corr%extcombs=extcombs
      allocate(corr%rescc(0:5*(5+1)/2+1))
      call vamp2cache(P_scatt, M2L0, M2L1, M2L2, corr=corr)
      M2cc = corr%rescc
      deallocate(corr%extcombs)
      deallocate(corr%rescc)
    else if (crtype == 3) then
      corr%emitter=emitter
      call vamp2cache(P_scatt, M2L0, M2L1, M2L2, corr=corr)
      M2munu=corr%resmunu
    end if
  end subroutine vampcr2
#endif

  subroutine ctamp2(P_scatt, M2tree, M2ct) &
      & bind(c,name="ol_f_ctamp2_ppwjj_ckm_cxdddxwx_1")
    use KIND_TYPES, only: DREALKIND
    use ol_vamp_ppwjj_ckm_cxdddxwx_1_/**/DREALKIND, only: ctamp2base
    implicit none
    real(DREALKIND), intent(in)  :: P_scatt(0:3,5)
    real(DREALKIND), intent(out) :: M2tree, M2ct
    call ctamp2base(P_scatt, M2tree, M2ct)
  end subroutine ctamp2


  subroutine vamp2_c(P_scatt, M2L0, M2L1, IRL1, M2L2, IRL2) &
      & bind(c,name="ol_vamp2_ppwjj_ckm_cxdddxwx_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(in)  :: P_scatt(0:3,5)
    real(c_double), intent(out) :: M2L0, M2L1(0:2), IRL1(0:2), M2L2(0:4), IRL2(0:4)
    real(DREALKIND) :: f_p_scatt(0:3,5)
    real(DREALKIND) :: f_m2l0, f_m2l1(0:2), f_irl1(0:2), f_m2l2(0:4), f_irl2(0:4)
    f_p_scatt = P_scatt
    call vamp2(f_p_scatt, f_m2l0, f_m2l1, f_irl1, f_m2l2, f_irl2)
    M2L0 = f_m2l0
    M2L1 = f_m2l1
    IRL1 = f_irl1
    M2L2 = f_m2l2
    IRL2 = f_irl2
  end subroutine vamp2_c


  subroutine ctamp2_c(P_scatt, M2tree, M2ct) &
      & bind(c,name="ol_ctamp2_ppwjj_ckm_cxdddxwx_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(in)  :: P_scatt(0:3,5)
    real(c_double), intent(out) :: M2tree, M2ct
    real(DREALKIND) :: f_p_scatt(0:3,5)
    real(DREALKIND) :: f_m2tree, f_m2ct
    f_p_scatt = P_scatt
    call ctamp2(f_p_scatt, f_m2tree, f_m2ct)
    M2tree = f_m2tree
    M2ct = f_m2ct
  end subroutine ctamp2_c



  ! Only for compatibility with the Sherpa (rsp. old) interface
  subroutine vamp2_legacy(P_scatt, M2L0, M2L1, IRL1, M2L2, IRL2) &
      & bind(c,name="vamp2_ppwjj_ckm_cxdddxwx_1_")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(in)  :: P_scatt(0:3,5)
    real(c_double), intent(out) :: M2L0, M2L1(0:2), IRL1(0:2), M2L2(0:4), IRL2(0:4)
    real(DREALKIND) :: f_p_scatt(0:3,5)
    real(DREALKIND) :: f_m2l0, f_m2l1(0:2), f_irl1(0:2), f_m2l2(0:4), f_irl2(0:4)
    f_p_scatt = P_scatt
    call vamp2(f_p_scatt, f_m2l0, f_m2l1, f_irl1, f_m2l2, f_irl2)
    M2L0 = f_m2l0
    M2L1 = f_m2l1
    IRL1 = f_irl1
    M2L2 = f_m2l2
    IRL2 = f_irl2
  end subroutine vamp2_legacy

  subroutine ctamp2_legacy(P_scatt, M2tree, M2ct) &
      & bind(c,name="ctamp2_ppwjj_ckm_cxdddxwx_1_")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(in)  :: P_scatt(0:3,5)
    real(c_double), intent(out) :: M2tree, M2ct
    real(DREALKIND) :: f_p_scatt(0:3,5)
    real(DREALKIND) :: f_m2tree, f_m2ct
    f_p_scatt = P_scatt
    call ctamp2(f_p_scatt, f_m2tree, f_m2ct)
    M2tree = f_m2tree
    M2ct = f_m2ct
  end subroutine ctamp2_legacy

end module ol_vamp_ppwjj_ckm_cxdddxwx_1

! #ifdef PRECISION_dp
#endif
