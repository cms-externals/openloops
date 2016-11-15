! 3
! The number above is the number of vamp routines for this process.
! This is needed by the build system.

module ol_vamp_ppllttj_nenexttxbbxg_1_/**/REALKIND
  contains

! **********************************************************************
subroutine vamp(M)
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_vamp_1_ppllttj_nenexttxbbxg_1_/**/REALKIND, only: vamp_1
  use ol_vamp_2_ppllttj_nenexttxbbxg_1_/**/REALKIND, only: vamp_2
  use ol_vamp_3_ppllttj_nenexttxbbxg_1_/**/REALKIND, only: vamp_3

  implicit none

  complex(REALKIND), intent(in) :: M(4)

#ifdef LOOPSQUARED
  M = 0
#endif

  ! Call subroutines for all branches to calculate loop coefficient tensors
  call vamp_1(M)
  call vamp_2(M)
  call vamp_3(M)

end subroutine vamp



! **********************************************************************
subroutine vamp2base(P_scatt, M2L0, M2L1, IRL1, M2L2, IRL2, mode)
! P_scatt(0:3,Npart) = external momenta
! M2tree = helicity-summed squared tree matrix element for nu_e anti-nu_e top anti-top bottom anti-bottom glue -> 0
! M2loop0 = helicity-summed squared loop matrix element for nu_e anti-nu_e top anti-top bottom anti-bottom glue -> 0
! M2loop1 = IR1, M2loop2 = IR2 are dummy values for the single and double poles
! IR0, IR1, IR2 = finite, single pole, and double pole IR contribution
! mode = 1 (default): full matrix element;
!        2: reuse and scale coefficients from the last call;
!        note that scalings cannot be reset
! **********************************************************************
  use KIND_TYPES, only: REALKIND, DREALKIND
  use ol_external_ppllttj_nenexttxbbxg_1, only: &
    & external_perm_inv_ppllttj_nenexttxbbxg_1, average_factor_ppllttj_nenexttxbbxg_1
  use ol_external_ppllttj_nenexttxbbxg_1, only: hel_not_initialised, hel_init, H
  use ol_colourmatrix_ppllttj_nenexttxbbxg_1_/**/REALKIND ! colmat_not_initialised, colourmatrix_init, K1, K2, KL, KL2, KL2ct, KL2ct2
  use ol_kinematics_/**/REALKIND, only: conv_mom_scatt2in, internal_momenta
  use ol_parameters_decl_/**/REALKIND ! parameters_status, scalefactor, <masses>
  ! tensorrankuse: for compatibility with old OL versions only insert if rank > 6
  use ol_parameters_init_/**/REALKIND, only: ensure_mp_loop_init
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  use ol_loop_parameters_decl_/**/DREALKIND, only: IR_is_on, ioperator_mode
  use ol_forced_parameters_ppllttj_nenexttxbbxg_1_/**/REALKIND, only: check_forced_parameters
  use ol_momenta_decl_/**/DREALKIND, only: momenta_nan_check
  use ol_momenta_decl_/**/REALKIND, only: QInvariantsMatrix
  use ol_tensor_sum_storage_ppllttj_nenexttxbbxg_1_/**/REALKIND, only: &
    & reset_tensor_sum, integrate_tensor_sum, scale_tensor_sum
  use ol_loop_ppllttj_nenexttxbbxg_1_/**/REALKIND, only: &
    & fac_status_loop1, fac_status_loop2, zerohel, M0, fac_init_loop, tree_wavefunctions
  use ol_i_operator_/**/REALKIND, only: intdip
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,7)
  real(REALKIND),  intent(out) :: M2L0, M2L1, IRL1(0:2), M2L2, IRL2(0:4)
  integer, intent(in), optional :: mode
  real(REALKIND)       :: P(0:3,7)
  integer              :: la, i, j, colmatpos, recycle_mode
  complex(REALKIND)    :: M0_col1(4), Mct(4)
  complex(REALKIND)    :: Mcol_loop(4)
  real(REALKIND)       :: M2colint(29), M2CC(7,7), M2CC_EW ! colour correlations
  integer,        save :: ntry = 1
  real(REALKIND)       :: extmasses2(7)
  real(REALKIND)       :: M2hel, M2ct, M2L2ct, M2L2ct2, vdip, c_dip(0:2)
  real(REALKIND)       :: scalebackfactor

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


  M2L0     = 0
  M2L1     = 0
  M2ct     = 0
  M2colint = 0
  M2L2     = 0
  M2L2ct   = 0
  M2L2ct2  = 0
  IRL1     = 0
  IRL2     = 0

  if (momenta_nan_check(P_scatt) /= 0) return

  ! Convert 2 -> n-2 PS-point to n -> 0 (so that P(1) + ... + P(n) = 0)
  extmasses2 = [ rZERO2, rZERO2, rMT2, rMT2, rMB2, rMB2, rZERO2 ]
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppllttj_nenexttxbbxg_1, 7)
  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P, 7)

#ifdef LOOPSQUARED
  call integrate_tensor_sum(M2L1)
#else
  if (recycle_mode == 1) call reset_tensor_sum()
#endif

  do la = 1, 128

    if (ntry > 2 .and. zerohel(la)) cycle
    call tree_wavefunctions(P, H(:,la), M0(:,la), Mct)
#if 4 > 0
    do j = 1, 4
      M2L0 = M2L0 + real(sum(conjg(M0(:,la))*K1(1:4,j))*M0(j,la))
    end do
#endif
#if 4 > 0
    do j = 1, 4
      M2ct = M2ct + real(sum(conjg(M0(:,la))*K2(:,j))*Mct(j))
    end do
#endif
    do j = 1, 4
      M0_col1(j) = sum(conjg(M0(:,la))*KL(:,j))
    end do
#if 30 > 1
    if (IR_is_on > 0) then
      do j = 1, 30-1
        colmatpos = 4*j
        do i = 1, 4
          M2colint(j) = M2colint(j) + real(conjg(M0(i,la))*sum(K1(i+colmatpos,:)*M0(:,la)))
        end do
      end do
    end if
#endif

#ifdef LOOPSQUARED
    call vamp(Mcol_loop)
    ! tree-loop interference
    M2L1 = M2L1 + real(sum(M0_col1*Mcol_loop))
    ! loop^2
    M2hel = 0
    do j = 1, 4
      M2hel = M2hel + real(sum(conjg(Mcol_loop(:))*KL2(:,j))*Mcol_loop(j))
    end do
    M2L2 = M2L2 + M2hel
#if 4 > 0
    do j = 1, 4
      M2L2ct  = M2L2ct  + real(sum(conjg(Mcol_loop(:))*KL2ct(:,j))*Mct(j))
      M2L2ct2 = M2L2ct2 + real(sum(conjg(Mct(:))*KL2ct2(:,j))*Mct(j))
    end do
#endif
! #ifdef LOOPSQUARED
#else
    if (recycle_mode == 1) then
      call vamp(M0_col1)
    end if
! #ifdef LOOPSQUARED
#endif
    if (ntry <= 2) then
#ifdef LOOPSQUARED
      if (M2hel /= 0) zerohel(la) = .false.
#else
      if (count(M0(:,la) /= 0) > 0) zerohel(la) = .false.
#endif
      if (la == 128) ntry = ntry + 1
    end if

  end do

#ifdef LOOPSQUARED
  M2L2 = (M2L2 + 2*M2L2ct + M2L2ct2) / average_factor_ppllttj_nenexttxbbxg_1
#else
  if (recycle_mode == 2) then
    call scale_tensor_sum()
  else if (recycle_mode /= 1) then
    print *, "[OpenLoops] ERROR in vamp2base: unrecognised recycle_mode:", recycle_mode
    stop
  end if
  call integrate_tensor_sum(M2L1)
#endif

  if (IR_is_on > 0) then
    do i = 1, 7
      do j = 1, i
        ! Why does this work without permuting the colour correlation matrices?
        ! M2CC(i,j) = M2colint(extcomb_perm_uxuexe_uxuexe(i*(i-1)/2+j))
        M2CC(i,j) = M2colint(i*(i-1)/2+j)
      end do
    end do
    do j = 2, 7
      do i = 1, j-1
        M2CC(i,j) = M2CC(j,i)
      end do
    end do
    M2CC_EW = M2colint(7*(7+1)/2+1)
    call intdip(ioperator_mode, M2L0, M2CC, M2CC_EW, [3,3,2,2,2,2,1], [0,0,2,-2,-1,1,0]/3._/**/REALKIND, &
      & 7, extmasses2, QInvariantsMatrix, vdip, c_dip)
    IRL1(0) = c_dip(0) / average_factor_ppllttj_nenexttxbbxg_1
    IRL1(1) = c_dip(1) / average_factor_ppllttj_nenexttxbbxg_1
    IRL1(2) = c_dip(2) / average_factor_ppllttj_nenexttxbbxg_1
  else
    vdip = 0
    IRL1 = 0
  end if

  ! loop^2 IR contribution: not implemented
  IRL2 = 0

  ! Colour and helicity average and symmetry factor of outgoing particles
  M2L0 = M2L0 / average_factor_ppllttj_nenexttxbbxg_1
  M2L1 = 2*(M2L1 + M2ct)
  if (IR_is_on > 1) then
    M2L1 = M2L1 + vdip
  end if
  M2L1 = M2L1 / average_factor_ppllttj_nenexttxbbxg_1

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

  scalebackfactor = scalefactor**(2*7-8)
  M2L0 = scalebackfactor * M2L0
  M2L1 = scalebackfactor * M2L1
  IRL1 = scalebackfactor * IRL1
  M2L2 = scalebackfactor * M2L2
  IRL2 = scalebackfactor * IRL2

end subroutine vamp2base



! **********************************************************************
subroutine ctamp2base(P_scatt, M2tree, M2ct)
! The part of vamp2 which calculates tree and counter-term
! matrix elements, but not the loop. R2 is deactivated.
! Does not calculate loop^2 counterterms/R2.
! P_scatt(0:3,Npart) = external momenta
! M2tree = helicity-summed squared tree matrix element for nu_e anti-nu_e top anti-top bottom anti-bottom glue -> 0
! M2ct   = helicity-summed counterterm matrix element
! **********************************************************************
  use KIND_TYPES, only: REALKIND, DREALKIND
  use ol_external_ppllttj_nenexttxbbxg_1, only: hel_not_initialised, hel_init, H
  use ol_colourmatrix_ppllttj_nenexttxbbxg_1_/**/REALKIND, only: &
    & colmat_not_initialised, colourmatrix_init, K1, K2
  use ol_parameters_decl_/**/REALKIND ! parameters_status, scalefactor, <masses>
  use ol_loop_parameters_decl_/**/DREALKIND, only: &
    & loop_parameters_status, CT_is_on, R2_is_on, TP_is_on
  use ol_init, only: parameters_flush
  use ol_parameters_init_/**/REALKIND, only: ensure_mp_loop_init, loop_parameters_init
  use ol_init, only: set_parameter
  use ol_external_ppllttj_nenexttxbbxg_1, only: &
    & external_perm_inv_ppllttj_nenexttxbbxg_1, average_factor_ppllttj_nenexttxbbxg_1
  use ol_momenta_decl_/**/DREALKIND, only: momenta_nan_check
  use ol_kinematics_/**/REALKIND, only: conv_mom_scatt2in, internal_momenta
  use ol_forced_parameters_ppllttj_nenexttxbbxg_1_/**/REALKIND, only: check_forced_parameters
  use ol_loop_ppllttj_nenexttxbbxg_1_/**/REALKIND, only: &
    & fac_status_loop1, fac_status_loop2, zerohel_ct, M0, fac_init_loop, tree_wavefunctions
  implicit none

  real(DREALKIND), intent(in)  :: P_scatt(0:3,7)
  real(REALKIND),  intent(out) :: M2tree, M2ct
  real(REALKIND)    :: P(0:3,7)
  integer           :: la, j
  complex(REALKIND) :: Mct(4)
  integer,  save    :: ntry = 1
  real(REALKIND)    :: extmasses2(7)
  integer           :: CT_on_bak, R2_on_bak, TP_on_bak
  real(REALKIND)    :: scalebackfactor

  CT_on_bak = CT_is_on
  R2_on_bak = R2_is_on
  TP_on_bak = TP_is_on
  CT_is_on = 1
  R2_is_on = 0
  TP_is_on = 0

  call loop_parameters_init()
  call ensure_mp_loop_init()

  extmasses2 = [ rZERO2, rZERO2, rMT2, rMT2, rMB2, rMB2, rZERO2 ]
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
  call conv_mom_scatt2in(P_scatt, extmasses2, P, external_perm_inv_ppllttj_nenexttxbbxg_1, 7)
  ! internal-propagator momenta in light-cone representation
  call internal_momenta(P, 7)

  M2tree = 0
  M2ct   = 0

  do la = 1, 128
    if (ntry <= 2) then
      call tree_wavefunctions(P, H(:,la), M0(:,la), Mct)
      if (count(M0(:,la) /= 0) > 0) zerohel_ct(la) = .false.
      if (la == 128) ntry = ntry + 1
    else
      if (zerohel_ct(la)) cycle
      call tree_wavefunctions(P, H(:,la), M0(:,la), Mct)
    end if
#if 4 > 0
    do j = 1, 4
      M2tree = M2tree + real(sum(conjg(M0(:,la))*K1(1:4,j))*M0(j,la))
    end do
#endif
#if 4 > 0
    do j = 1, 4
      M2ct = M2ct + real(sum(conjg(M0(:,la))*K2(1:4,j))*Mct(j))
    end do
#endif
  end do

  CT_is_on = CT_on_bak
  R2_is_on = R2_on_bak
  TP_is_on = TP_on_bak

  call loop_parameters_init()
  call ensure_mp_loop_init()

  scalebackfactor = scalefactor**(2*7-8)
  M2tree = scalebackfactor * M2tree / average_factor_ppllttj_nenexttxbbxg_1
  M2ct   = scalebackfactor * 2*M2ct / average_factor_ppllttj_nenexttxbbxg_1

end subroutine ctamp2base



subroutine vamp2pc(P_scatt, M2L0, M2L1, IRL1, M2L2, IRL2, mode)
  ! polecheck routine: perform multiple calls of vamp2
  ! with different values of the UV/IR poles
  ! to determine the coefficients of the IR poles.
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
    call vamp2base(P_scatt, M2L0, M2L1(0), IRL1, M2L2(0), IRL2, mode)
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

end module ol_vamp_ppllttj_nenexttxbbxg_1_/**/REALKIND



#ifdef PRECISION_dp

module ol_vamp_ppllttj_nenexttxbbxg_1
  use ol_parameters_decl_/**/DREALKIND, only: procname_length
  use ol_data_types_/**/DREALKIND, only: me_cache
  implicit none
  character(procname_length) :: processname = 'ppllttj_nenexttxbbxg_1'
  integer, save :: qp_eval = 0, killed = 0
  integer, save :: npoints(8) = 0
  integer, save :: stability_histogram(20) = 0, stability_histogram_qp(20) = 0
  type(me_cache), allocatable, target, save :: me_caches(:)
  contains

  subroutine finish_ppllttj_nenexttxbbxg_1()
    ! final update of the stability histogram
    use ol_stability, only: finish_histograms
    implicit none
    integer :: k
    if (allocated(me_caches)) then
      do k = 1, size(me_caches)
        if (allocated(me_caches(k)%psp)) deallocate(me_caches(k)%psp)
        if (allocated(me_caches(k)%me)) deallocate(me_caches(k)%me)
      end do
      deallocate(me_caches)
    end if
    call finish_histograms(processname, stability_histogram, stability_histogram_qp, npoints, qp_eval, killed)
  end subroutine finish_ppllttj_nenexttxbbxg_1


  subroutine vamp2(P_scatt, M2L0, M2L1, IRL1, M2L2, IRL2) &
      & bind(c,name="ol_f_vamp2_ppllttj_nenexttxbbxg_1")
    use KIND_TYPES, only: DREALKIND
    use ol_init, only: register_cleanup
    use ol_external_ppllttj_nenexttxbbxg_1, only: external_perm_ppllttj_nenexttxbbxg_1
    use ol_vamp_ppllttj_nenexttxbbxg_1_/**/DREALKIND, only: vamp2dp => vamp2pc
#ifdef USE_qp
    use ol_vamp_ppllttj_nenexttxbbxg_1_/**/QREALKIND, only: vamp2qp => vamp2pc
#endif
    use ol_stability, only: vamp2generic
    implicit none
    real(DREALKIND), intent(in)  :: P_scatt(0:3,7)
    real(DREALKIND), intent(out) :: M2L0, M2L1(0:2), IRL1(0:2), M2L2(0:4), IRL2(0:4)
    real(DREALKIND), save :: abs_kfactor_threshold = 1, trigeff_local = 0, sum_M2tree = 0
    logical :: first_call = .true.
#ifndef USE_qp
    logical :: vamp2qp = .false.
#endif
    if (first_call) then
      call register_cleanup(finish_ppllttj_nenexttxbbxg_1)
      first_call = .false.
    end if
    call vamp2generic(vamp2dp, vamp2qp, processname, P_scatt, M2L0, M2L1, IRL1, M2L2, IRL2, &
                    & abs_kfactor_threshold, trigeff_local, sum_M2tree, &
                    & npoints, qp_eval, killed, stability_histogram, stability_histogram_qp, &
                    & external_perm_ppllttj_nenexttxbbxg_1, me_caches)
  end subroutine vamp2


  subroutine ctamp2(P_scatt, M2tree, M2ct) &
      & bind(c,name="ol_f_ctamp2_ppllttj_nenexttxbbxg_1")
    use KIND_TYPES, only: DREALKIND
    use ol_vamp_ppllttj_nenexttxbbxg_1_/**/DREALKIND, only: ctamp2base
    implicit none
    real(DREALKIND), intent(in)  :: P_scatt(0:3,7)
    real(DREALKIND), intent(out) :: M2tree, M2ct
    call ctamp2base(P_scatt, M2tree, M2ct)
  end subroutine ctamp2


  subroutine vamp2_c(P_scatt, M2L0, M2L1, IRL1, M2L2, IRL2) &
      & bind(c,name="ol_vamp2_ppllttj_nenexttxbbxg_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(in)  :: P_scatt(0:3,7)
    real(c_double), intent(out) :: M2L0, M2L1(0:2), IRL1(0:2), M2L2(0:4), IRL2(0:4)
    real(DREALKIND) :: f_p_scatt(0:3,7)
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
      & bind(c,name="ol_ctamp2_ppllttj_nenexttxbbxg_1")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(in)  :: P_scatt(0:3,7)
    real(c_double), intent(out) :: M2tree, M2ct
    real(DREALKIND) :: f_p_scatt(0:3,7)
    real(DREALKIND) :: f_m2tree, f_m2ct
    f_p_scatt = P_scatt
    call ctamp2(f_p_scatt, f_m2tree, f_m2ct)
    M2tree = f_m2tree
    M2ct = f_m2ct
  end subroutine ctamp2_c



  ! Only for compatibility with the Sherpa (rsp. old) interface
  subroutine vamp2_legacy(P_scatt, M2L0, M2L1, IRL1, M2L2, IRL2) &
      & bind(c,name="vamp2_ppllttj_nenexttxbbxg_1_")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(in)  :: P_scatt(0:3,7)
    real(c_double), intent(out) :: M2L0, M2L1(0:2), IRL1(0:2), M2L2(0:4), IRL2(0:4)
    real(DREALKIND) :: f_p_scatt(0:3,7)
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
      & bind(c,name="ctamp2_ppllttj_nenexttxbbxg_1_")
    use KIND_TYPES, only: DREALKIND
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(in)  :: P_scatt(0:3,7)
    real(c_double), intent(out) :: M2tree, M2ct
    real(DREALKIND) :: f_p_scatt(0:3,7)
    real(DREALKIND) :: f_m2tree, f_m2ct
    f_p_scatt = P_scatt
    call ctamp2(f_p_scatt, f_m2tree, f_m2ct)
    M2tree = f_m2tree
    M2ct = f_m2ct
  end subroutine ctamp2_legacy

end module ol_vamp_ppllttj_nenexttxbbxg_1

! #ifdef PRECISION_dp
#endif
