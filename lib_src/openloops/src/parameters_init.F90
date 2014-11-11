
! Copyright 2014 Fabio Cascioli, Jonas Lindert, Philipp Maierhoefer, Stefano Pozzorini
!
! This file is part of OpenLoops.
!
! OpenLoops is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! OpenLoops is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with OpenLoops.  If not, see <http://www.gnu.org/licenses/>.


module ol_parameters_init_/**/REALKIND
  implicit none
  contains

subroutine masspowers(rM, Ga, M, M2, rM2)
  use KIND_TYPES, only: REALKIND
  use ol_parameters_decl_/**/REALKIND, only: CI
  use ol_parameters_decl_/**/DREALKIND, only: cms_on
  implicit none
  real(REALKIND),    intent(in)  :: rM, Ga
  complex(REALKIND), intent(out) :: M,  M2
  real(REALKIND),    intent(out) :: rM2
  M2  = rM*rM - CI*rM*Ga
  if ( cms_on == 0 ) then
    M   = rM
    rM2 = rM*rM
  else
    M  = sqrt(M2)
    rM2 = real(M2)
  end if
end subroutine masspowers



#ifdef PRECISION_dp

subroutine parameters_init(Mass_E, Mass_M, Mass_L, Mass_U, Mass_D, Mass_S, Mass_C, Width_C, Mass_B, Width_B, Mass_T, Width_T, &
                           Mass_W, Width_W, Mass_Z, Width_Z, Mass_H, Width_H, Coupl_Alpha_QED, Coupl_Alpha_QCD, &
                           last_switch, amp_switch, amp_switch_rescue, &
                           use_coli_cache, check_Ward_tree, check_Ward_loop, out_symmetry, leading_colour)
  ! Assign values of physical input patameters.
  ! Use this subroutine with named arguments, e.g. parameters_init(Mass_B=0._/**/REALKIND) to set
  ! the bottom-quark mass to zero without changing other parameters.
  ! Always use this routine to change parameters, otherwise factors which contain these
  ! parameters will not be recalculated (see parameters_status).
  use KIND_TYPES, only: REALKIND
  use ol_generic, only: to_string, random_string
  use ol_parameters_decl_/**/REALKIND
#if defined(COLLIER_LEGACY) && defined(USE_COLLIER)
  use bt_TI_lib_switch, only: TI_library ! from COLI: module to switch between COLI and DD libraries
#endif
#ifdef USE_IFORT
  use ifport, only: getpid, system
#endif
  use ol_version, only: splash_todo, print_welcome
  implicit none
  real(REALKIND), intent(in), optional :: Mass_E, Mass_M,  Mass_L ! physical (real) lepton masses
  real(REALKIND), intent(in), optional :: Mass_U, Mass_D,  Mass_S ! physical (real) light-quark masses
  real(REALKIND), intent(in), optional :: Mass_C, Width_C, Mass_B, Width_B, Mass_T, Width_T ! physical (real) heavy-quark masses and widths
  real(REALKIND), intent(in), optional :: Mass_W, Width_W, Mass_Z, Width_Z, Mass_H, Width_H ! physical (real) boson masses and widths
  real(REALKIND), intent(in), optional :: Coupl_Alpha_QED, Coupl_Alpha_QCD ! Coupling constants
  integer,  intent(in), optional :: last_switch, amp_switch, amp_switch_rescue ! set mode for check_last_[...] and loop_amp
  integer,  intent(in), optional :: use_coli_cache
  integer,  intent(in), optional :: check_Ward_tree, check_Ward_loop
  integer,  intent(in), optional :: out_symmetry
  integer,  intent(in), optional :: leading_colour
  integer :: dummy

  if (parameters_status == 0) then
    pid_string = trim(to_string(getpid())) // "-" // random_string(4)
  end if

  if (splash_todo) then
    call print_welcome()
  end if

  if (stability_logdir_not_created .and. stability_log > 0) then
    stability_logdir_not_created = .false.
    dummy = system("mkdir -p " // trim(stability_logdir))
  end if

  ! Mode switches
  if (present(last_switch))       l_switch        = last_switch
  if (present(amp_switch))        a_switch        = amp_switch
  if (present(amp_switch_rescue)) a_switch_rescue = amp_switch_rescue

#if defined(COLLIER_LEGACY) && defined(USE_COLLIER)
  if (a_switch == 1) TI_library = 1 ! use COLI
  if (a_switch == 7) TI_library = 2 ! use DD
#endif

  if (present(use_coli_cache)) coli_cache_use = use_coli_cache
  if (present(check_Ward_tree)) Ward_tree = check_Ward_tree
  if (present(check_Ward_loop)) Ward_loop = check_Ward_loop
  if (present(out_symmetry)) out_symmetry_on = out_symmetry
  if (present(leading_colour)) LeadingColour = leading_colour
  ! Check for optional arguments
  if (present(Coupl_Alpha_QED)) alpha_QED = Coupl_Alpha_QED
  if (present(Coupl_Alpha_QCD)) alpha_QCD = Coupl_Alpha_QCD
  if (present(Mass_E))  rME_unscaled = Mass_E
  if (present(Mass_M))  rMM_unscaled = Mass_M
  if (present(Mass_L))  rML_unscaled = Mass_L
  if (present(Mass_U))  rMU_unscaled = Mass_U
  if (present(Mass_D))  rMD_unscaled = Mass_D
  if (present(Mass_S))  rMS_unscaled = Mass_S
  if (present(Mass_C))  rMC_unscaled = Mass_C
  if (present(Mass_B))  rMB_unscaled = Mass_B
  if (present(Mass_T))  rMT_unscaled = Mass_T
  if (present(Mass_W))  rMW_unscaled = Mass_W
  if (present(Mass_Z))  rMZ_unscaled = Mass_Z
  if (present(Mass_H))  rMH_unscaled = Mass_H
  if (present(Width_C)) wMC_unscaled = Width_C
  if (present(Width_B)) wMB_unscaled = Width_B
  if (present(Width_T)) wMT_unscaled = Width_T
  if (present(Width_W)) wMW_unscaled = Width_W
  if (present(Width_W)) wMY_unscaled = Width_W
  if (present(Width_Z)) wMZ_unscaled = Width_Z
  if (present(Width_Z)) wMX_unscaled = Width_Z
  if (present(Width_H)) wMH_unscaled = Width_H

  ! set mass of V-auxiliary fields
  rMX_unscaled = rMZ_unscaled
  rMY_unscaled = rMW_unscaled

  rME = scalefactor * rME_unscaled
  wME = scalefactor * wME_unscaled
  rMM = scalefactor * rMM_unscaled
  wMM = scalefactor * wMM_unscaled
  rML = scalefactor * rML_unscaled
  wML = scalefactor * wML_unscaled
  rMU = scalefactor * rMU_unscaled
  wMU = scalefactor * wMU_unscaled
  rMD = scalefactor * rMD_unscaled
  wMD = scalefactor * wMD_unscaled
  rMS = scalefactor * rMS_unscaled
  wMS = scalefactor * wMS_unscaled
  rMC = scalefactor * rMC_unscaled
  wMC = scalefactor * wMC_unscaled
  rMB = scalefactor * rMB_unscaled
  wMB = scalefactor * wMB_unscaled
  rMT = scalefactor * rMT_unscaled
  wMT = scalefactor * wMT_unscaled
  rMW = scalefactor * rMW_unscaled
  wMW = scalefactor * wMW_unscaled
  rMZ = scalefactor * rMZ_unscaled
  wMZ = scalefactor * wMZ_unscaled
  rMX = scalefactor * rMX_unscaled
  wMX = scalefactor * wMX_unscaled
  rMY = scalefactor * rMY_unscaled
  wMY = scalefactor * wMY_unscaled
  rMH = scalefactor * rMH_unscaled
  wMH = scalefactor * wMH_unscaled
  MREG= scalefactor * MREG_unscaled

! ifdef PRECISION_dp
#else

subroutine parameters_init()
  ! non-dp initialisation: synchronise with dp parameters
  use KIND_TYPES, only: REALKIND
  use ol_parameters_decl_/**/REALKIND
  use ol_parameters_decl_/**/DREALKIND, only: scalefactor_dp => scalefactor, cms_on => cms_on, &
    & parameters_status_dp => parameters_status, alpha_QED_dp => alpha_QED, alpha_QCD_dp => alpha_QCD, &
    & rME_dp => rME, wME_dp => wME, rMM_dp => rMM, wMM_dp => wMM, rML_dp => rML, wML_dp => wML, &
    & rMU_dp => rMU, wMU_dp => wMU, rMD_dp => rMD, wMD_dp => wMD, rMS_dp => rMS, wMS_dp => wMS, &
    & rMC_dp => rMC, wMC_dp => wMC, rMB_dp => rMB, wMB_dp => wMB, rMT_dp => rMT, wMT_dp => wMT, &
    & rMW_dp => rMW, wMW_dp => wMW, rMZ_dp => rMZ, wMZ_dp => wMZ, rMH_dp => rMH, wMH_dp => wMH, &
    & rMX_dp => rMX, wMX_dp => wMX, rMY_dp => rMY, wMY_dp => wMY
  implicit none

  scalefactor = scalefactor_dp

  alpha_QED = alpha_QED_dp
  alpha_QCD = alpha_QCD_dp

  rME = rME_dp
  wME = wME_dp
  rMM = rMM_dp
  wMM = wMM_dp
  rML = rML_dp
  wML = wML_dp
  rMU = rMU_dp
  wMU = wMU_dp
  rMD = rMD_dp
  wMD = wMD_dp
  rMS = rMS_dp
  wMS = wMS_dp
  rMC = rMC_dp
  wMC = wMC_dp
  rMB = rMB_dp
  wMB = wMB_dp
  rMT = rMT_dp
  wMT = wMT_dp
  rMW = rMW_dp
  wMW = wMW_dp
  rMZ = rMZ_dp
  wMZ = wMZ_dp
  rMX = rMX_dp
  wMX = wMX_dp
  rMY = rMY_dp
  wMY = wMY_dp
  rMH = rMH_dp
  wMH = wMH_dp

! ifdef PRECISION_dp
#endif

  ! Complex masses and squared masses
  call masspowers(rME, wME, ME, ME2, rME2)
  call masspowers(rMM, wMM, MM, MM2, rMM2)
  call masspowers(rML, wML, ML, ML2, rML2)
  call masspowers(rMU, wMU, MU, MU2, rMU2)
  call masspowers(rMD, wMD, MD, MD2, rMD2)
  call masspowers(rMS, wMS, MS, MS2, rMS2)
  call masspowers(rMC, wMC, MC, MC2, rMC2)
  call masspowers(rMB, wMB, MB, MB2, rMB2)
  call masspowers(rMT, wMT, MT, MT2, rMT2)
  call masspowers(rMW, wMW, MW, MW2, rMW2)
  call masspowers(rMZ, wMZ, MZ, MZ2, rMZ2)
  call masspowers(rMX, wMX, MX, MX2, rMX2)
  call masspowers(rMY, wMY, MY, MY2, rMY2)
  call masspowers(rMH, wMH, MH, MH2, rMH2)
  ! Dependent couplings

  !QCD
  G2_QCD = 4*pi*alpha_QCD
  gQCD   = sqrt(G2_QCD)

  !EW
  E2_QED = 4*pi*alpha_QED
  eQED   = sqrt(E2_QED)
  if ( cms_on == 0 ) then
    cw   = rMW/rMZ
  else
    cw   = MW/MZ
  end if
  cw2    = cw**2
  cw3    = cw**3
  cw4    = cw2**2
  sw2    = 1. - cw2
  sw     = sqrt(sw2)
  sw3    = sw**3
  sw4    = sw2**2
  sw6    = sw2**3

  ! (1) Right-handed Z-fermion couplings = gf^+ = gZRH*Qf in Denner's FRs
  ! (2) Left-handed  Z-fermion couplings = gf^- = gZLH*(I3f-sw2*Qf) in Denner's FRs
  gZRH = -sw/cw
  gZLH = 1/(sw*cw)
  gZn  = [    ZERO   , gZLH*( 0.5_/**/REALKIND            ) ] ! neutrino
  gZl  = [   -gZRH   , gZLH*(-0.5_/**/REALKIND +    sw2   ) ] ! lepton
  gZu  = [ (2*gZRH)/3, gZLH*( 0.5_/**/REALKIND - (2*sw2)/3) ] ! up
  gZd  = [   -gZRH /3, gZLH*(-0.5_/**/REALKIND +    sw2 /3) ] ! down
  ! Right- (1) and left-handed (2) couplings of scalars to fermions
  ! gPud = P+ u~ d; gPdu = P- d~ u; gPnl = P+ n~ l; gPln = P- l~ n (all incoming)
  gH   = [  cONE, cONE ]
  gX   = [ -cONE, cONE ]
  gPud = [   -MD,   MU ]
  gPcs = [   -MS,   MC ]
  gPtb = [   -MB,   MT ]
  gPdu = [   -MU,   MD ]
  gPsc = [   -MC,   MS ]
  gPbt = [   -MT,   MB ]
  gPnl = [  cONE, ZERO ]
  gPln = [  ZERO, cONE ]

  ! Number of time this function has been called:
#ifdef PRECISION_dp
  parameters_status = parameters_status + 1
#else
  parameters_status = parameters_status_dp
#endif

  ! write parameters
  if (parameters_verbose == 1 ) then
    call parameters_write
  end if

end subroutine parameters_init



subroutine ensure_mp_init()
  ! synchronise non-dp parameters with dp if they are not up to date
  ! should be called after parameters_init()
  ! and in tree matrix element routines before anything is done
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/REALKIND, only: parameters_status
  use ol_parameters_decl_/**/DREALKIND, only: &
    & parameters_status_dp => parameters_status
  implicit none
  if (parameters_status_dp /= parameters_status) call parameters_init()
#endif
end subroutine ensure_mp_init



#ifdef PRECISION_dp
! **********************************************************************
subroutine channel_on(ch)
! If ch = -1 generate new channel number ch > 0 and switch channel ch on.
! Otherwise initialise the existing channel ch to compute a new phase space point.
! **********************************************************************
  use ol_parameters_decl_/**/DREALKIND, only: &
    next_channel_number, coli_cache_use, a_switch
#ifndef COLLIER_LEGACY
  use collier, only: initevent_cll
#endif
  implicit none
  integer, intent(inout) :: ch
#ifdef USE_COLLIER
#ifdef COLLIER_LEGACY
  integer :: maxcache
  external :: cacheon, cachereonch, cacheinit ! from COLI
  if (coli_cache_use /= 0 .and. (a_switch == 1 .or. a_switch == 2 .or. a_switch == 3)) then
    call getmaxcache(maxcache)
    if (ch == -1) then
      ch = next_channel_number
      next_channel_number = next_channel_number + 1
      if (ch > maxcache) then
        ! maximum number of channels exceeded
        write(*,*) 'subroutine channel_on: stop'
        write(*,*) 'next channel =', next_channel_number,'/',maxcache
        write(*,*) 'to handle more channels increase maxcache'
        write(*,*) 'in collier/src/coli_params_cache.h'
        stop
      else
        call cacheon(ch)
      end if
    end if

    call cachereonch(ch)  ! cache channel reactivated
    call cacheinit(0,ch) ! cache channel initialised for new PS point
  end if
#else
  if (coli_cache_use /= 0 .and. (a_switch == 1 .or. a_switch == 2 .or. a_switch == 3)) then
    if (ch == -1) then
      ch = next_channel_number
      next_channel_number = next_channel_number + 1
    end if
    call initevent_cll(ch)
  end if
#endif
#endif
end subroutine channel_on



! **********************************************************************
subroutine channel_off(ch)
! switch cache for channel ch temporarily off
! **********************************************************************
  use ol_parameters_decl_/**/DREALKIND, only: coli_cache_use, a_switch
  implicit none
  integer, intent(in) :: ch
#if defined(USE_COLLIER) && defined(COLLIER_LEGACY)
  external :: cachetempoffch
  if (coli_cache_use /= 0 .and. (a_switch == 1 .or. a_switch == 2 .or. a_switch == 3)) then
    call cachetempoffch(ch)
  end if
#endif
end subroutine channel_off
! #ifdef PRECISION_dp
#endif



#if defined(USE_COLLIER) && defined(COLLIER_LEGACY)
! *************************
subroutine tensor_ints_init()
! *************************
  use bt_BuildTensors_/**/REALKIND, only: init_tables
  implicit none
  call init_tables(6,6)
end subroutine tensor_ints_init
#endif



#ifdef PRECISION_dp
! **********************************************************************
subroutine loop_parameters_init(renscale, fact_UV, fact_IR, pole1_UV, pole1_IR, pole2_IR, polenorm_swi, &
                                N_quarks, nq_nondecoupled, &
                                opp_rootsvalue, opp_limitvalue, opp_thrs, opp_idig, opp_scaloop, &
                                sam_isca, sam_verbosity, sam_itest, fermion_loops, nonfermion_loops, &
                                CT_on, R2_on, IR_on, polecheck, set_C_PV_threshold, &
                                set_D_PV_threshold, set_dd_red_mode)
! **********************************************************************
! Assign values of dimensional regularisation parameters. Use this subroutine
! with named arguments, e.g. parameters_init(pole1_UV=0._/**/REALKIND) to set the
! single UV pole to zero without changing other parameters. Always use this
! routine to change parameters, otherwise factors which contain these
! parameters will not be recalculated.
! **********************************************************************
! renscale = renormalisation scale
! ----------------------------------------------------------------------
! mu2_UV = (fact_UV*renscale)^2 = UV dim-reg scale (squared)
! mu2_IR = (fact_IR*renscale)^2 = IR dim-reg scale (squared)
! ----------------------------------------------------------------------
! numerical values of poles in D=4-2*eps dimensions
! pole1_UV -> de1_i_UV= K_i(eps_UV)/eps_UV   = de1_UV
! pole1_IR -> de1_i_IR= K_i(eps_IR)/eps_IR   = de1_IR
! pole2_IR -> de2_i_IR= K_i(eps_IR)/eps_IR^2   (depends on K_i)
! ----------------------------------------------------------------------
! results of loop/dipole routines based on generic normalisation
! K_i(eps) = (4Pi)^eps/Gamma(1-eps) + de2_i_shift*eps^2 +  O(esp^3)
! ----------------------------------------------------------------------
! polenorm_swi = 0 <=> Binoth-Les-Houches accord normalisation (default)
! de2_i_shift  = de2_0_shift = 0
! K_i(eps)     = K_0(eps)    = (4Pi)^eps/Gamma(1-eps)
! ----------------------------------------------------------------------
! polenorm_swi = 1 <=> normalisation employed by COLI library
! de2_i_shift  = de2_1_shift = Pi^2/6
! K_i(eps)     = K_1(eps)    = (4Pi)^eps*Gamma(1+eps)
! ----------------------------------------------------------------------
! Normalisation dependence of IR-divergent Laurent series
! in D=4-2*eps dimensions (i=0,1,...)
!
! F        = K_i(eps)*[F_i(0) + F(1)/eps       + F(2)/eps**2]
!          =           F_i(0) + F(1)*de1_IR    + F(2)*de2_i_IR
!          = independent of de2_i_shift normalisation convention
!
! de2_i_IR = de2_0_IR + de2_i_shift
!          = de2_j_IR + de2_i_shift - de2_j_shift
!
! F_i(0)   = F_0(0) - F(2)*de2_i_shift
!          = F_j(0) - F(2)*[de2_i_shift-de2_j_shift]
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND
  use ol_qcd_renormalisation_/**/REALKIND, only: qcd_renormalisation
!   use ol_ew_renormalisation_/**/REALKIND, only: ew_renormalisation
  use ol_tensor_bookkeeping, only: initialised_rank, init_tensorbookkeeping
#ifdef USE_COLLIER
#ifdef COLLIER_LEGACY
  use dd_init_/**/REALKIND, only: dd_setmode, dd_setparam
#else
  use collier, only: init_cll, initcachesystem_cll, setmode_cll, setmuuv2_cll, &
    & setmuir2_cll, setdeltauv_cll, setdeltair_cll, settenred_cll, setaccuracy_cll
#endif
#endif
#ifdef USE_ONELOOP
  use avh_olo, only: olo_scale, olo_onshell
#endif
#ifdef USE_SAMURAI
  use msamurai, only: initsamurai
#endif
  implicit none

  real(REALKIND), intent(in), optional :: renscale, fact_UV, fact_IR, pole1_UV, pole1_IR, pole2_IR
  integer,        intent(in), optional :: polenorm_swi, N_quarks, nq_nondecoupled

  ! DD parameters
  real(REALKIND), intent(in), optional :: set_C_PV_threshold, set_D_PV_threshold
  integer,        intent(in), optional :: set_dd_red_mode

  ! CutTools parameters
  real(REALKIND), intent(in), optional :: opp_rootsvalue, opp_limitvalue, opp_thrs
  integer,        intent(in), optional :: opp_idig, opp_scaloop

  ! Samurai parameters
  integer,        intent(in), optional :: sam_isca, sam_verbosity, sam_itest
  ! Switches for counter terms and R2
  integer,        intent(in), optional :: fermion_loops, nonfermion_loops, polecheck
  integer,        intent(in), optional :: CT_on, R2_on, IR_on

  real(REALKIND) :: mp2(10)

  if (present(renscale)) then
    if (mureg_unscaled /= renscale) reset_mureg = .true.
    mureg_unscaled = renscale
  end if
  if (present(fact_UV))         x_UV          = fact_UV
  if (present(fact_IR))         x_IR          = fact_IR
  if (present(pole1_UV))        de1_UV        = pole1_UV
  if (present(pole1_IR))        de1_IR        = pole1_IR
  if (present(pole2_IR))        de2_i_IR      = pole2_IR
  if (present(polenorm_swi))    norm_swi      = polenorm_swi
  if (present(N_quarks))        nf            = N_quarks
  if (present(nq_nondecoupled)) nq_nondecoupl = nq_nondecoupled

  if (present(set_C_PV_threshold)) C_PV_threshold = set_C_PV_threshold
  if (present(set_D_PV_threshold)) D_PV_threshold = set_D_PV_threshold
  if (present(set_dd_red_mode))    dd_red_mode    = set_dd_red_mode

  if (present(opp_rootsvalue)) then
    if (opprootsvalue_unscaled /= opp_rootsvalue) cuttools_not_init = .true.
    opprootsvalue_unscaled = opp_rootsvalue
  end if
  if (present(opp_limitvalue)) then
    if (opplimitvalue /= opp_limitvalue) cuttools_not_init = .true.
    opplimitvalue = opp_limitvalue
  end if
  if (present(opp_thrs)) then
    if (oppthrs /= opp_thrs) reset_oppthrs = .true.
    oppthrs = opp_thrs
  end if
  if (present(opp_idig)) then
    if (oppidig /= opp_idig) cuttools_not_init = .true.
    oppidig = opp_idig
  end if
  if (present(opp_scaloop)) then
    if (oppscaloop /= opp_scaloop) cuttools_not_init = .true.
    oppscaloop = opp_scaloop
  end if
  if (present(sam_isca)) then
    if (set_isca /= sam_isca) samurai_not_init = .true.
    set_isca = sam_isca
  end if
  if (present(sam_verbosity)) then
    if (set_verbosity /= sam_verbosity) samurai_not_init = .true.
    set_verbosity = sam_verbosity
  end if
  if (present(sam_itest)) then
    if (set_itest /= sam_itest) samurai_not_init = .true.
    set_itest = sam_itest
  end if

  if (present(fermion_loops))    SwF = fermion_loops
  if (present(nonfermion_loops)) SwB = nonfermion_loops


  if (present(CT_on)) CT_is_on = CT_on
  if (present(R2_on)) R2_is_on = R2_on
  if (present(IR_on)) IR_is_on = IR_on
  if (present(polecheck)) polecheck_is = polecheck

  if (maxrank > initialised_rank) call init_tensorbookkeeping(maxrank)

  if (reset_scalefactor) then
    reset_mureg = .true.
    reset_oppthrs = .true.
    reset_scalefactor = .false.
  end if

  opprootsvalue = scalefactor * opprootsvalue_unscaled
  mureg = scalefactor * mureg_unscaled

  ! convention for dim-reg Poles K_i(eps)/eps^N
  if (norm_swi == 0) then ! Les-Houches Accord normalisation (default)
    de2_i_shift = 0
    norm_name   = 'LH-accord '
  else if (norm_swi == 1) then ! COLI normalisation
    de2_i_shift = pi2_6
    norm_name   = 'COLI      '
  else
    write(*,*) 'routine loop_parameters_init: stop'
    write(*,*) 'norm_swi =', norm_swi, 'not allowed.'
    stop
  end if

! ifdef PRECISION_dp
#else

subroutine loop_parameters_init
  ! non-dp initialisation: synchronise with dp parameters
  use ol_parameters_decl_/**/REALKIND, only: pi2_6
  use ol_loop_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/DREALKIND, only: &
    & loop_parameters_status_dp => loop_parameters_status, norm_swi, a_switch, a_switch_rescue, redlib_qp, &
    & dd_qp_not_init, tensorlib_qp_not_init, renscale_dp => mureg, fact_UV_dp => x_UV, fact_IR_dp => x_IR, &
    & pole1_UV_dp => de1_UV, pole1_IR_dp => de1_IR, pole2_IR_dp => de2_i_IR, do_ew_renorm
#if defined(USE_COLLIER) && defined(COLLIER_LEGACY)
  use dd_init_/**/REALKIND, only: dd_setmode, dd_setparam
#endif
  use ol_qcd_renormalisation_/**/REALKIND, only: qcd_renormalisation
!   use ol_ew_renormalisation_/**/REALKIND, only: ew_renormalisation
  implicit none

  if (norm_swi == 0) then
    de2_i_shift = 0
  else if (norm_swi == 1) then
    de2_i_shift = pi2_6
  end if

  mureg    = renscale_dp
  x_UV     = fact_UV_dp
  x_IR     = fact_IR_dp
  de1_UV   = pole1_UV_dp
  de1_IR   = pole1_IR_dp
  de2_i_IR = pole2_IR_dp

! ifdef PRECISION_dp
#endif

  de2_0_IR = de2_i_IR - de2_i_shift         ! LH-norm double pole
  de2_1_IR = de2_i_IR - de2_i_shift + pi2_6 ! COLI-norm double pole
  ! renormalisation scale
  mureg2 = mureg**2
  ! dim reg scale in UV-div loops
  mu2_UV = (x_UV**2)*mureg2
  mu2_IR = (x_IR**2)*mureg2

#ifdef PRECISION_dp
  ! initialise reduction libraries only in double precision
  ! (quad precision initialisation is handles within these libraries if applicable)

#ifdef COLLIER_LEGACY
  ! COLI initialisation
  if (a_switch == 1 .or. a_switch_rescue == 1 .or. a_switch == 2 .or. a_switch == 3) then
#ifdef USE_COLLIER
    if (coli_not_init) then
      call defcoli ! assign default values to parameters of loop library
      coli_not_init = .false.
      call unsetinfo_coli ! turn off printed information for coli parameters setting
    end if
    call setdeltauv_coli(de1_UV)
    call setdeltair_coli(de1_IR,de2_1_IR)
    call setmuuv2_coli(mu2_UV)
    call setmuir2_coli(mu2_IR)
! #else
!     write(*,*) 'ERROR: Collier (Coli) is deactivated.'
#endif
  end if

  ! DD initialisation
  if (a_switch == 7 .or. a_switch_rescue == 7) then
#ifdef USE_COLLIER
    if (dd_not_init) then
      mp2 = 0
!     cacc         threshold precision to activate 3-point alternative reductions
!     dacc         threshold precision to activate 4-point alternative reductions
!     mode34 = 2   PV or alternative 3/4-point reductions
!     mode34 = 0   PV 3/4-point reduction only
!     mode5  = 0   best 5-point reduction
!     mode6  = 0   best 6-point reduction
!     outlevel = 0 no output
      call dd_setmode(C_PV_threshold,D_PV_threshold,dd_red_mode,0,0,0) ! cacc,dacc,mode34,mode5,mode6,outlevel
      dd_not_init = .false.
    end if
    call dd_setparam(de1_UV,mu2_UV,de2_1_IR,de1_IR,mu2_IR,mp2)
! #else
!     write(*,*) 'ERROR: Collier (DD) is deactivated.'
#endif
  end if
! #ifdef COLLIER_LEGACY
#else
  if (a_switch == 1 .or. a_switch_rescue == 1 .or. a_switch == 2 .or. a_switch == 3 .or. &
    & a_switch == 7 .or. a_switch_rescue == 7) then
    call init_cll(maxpoint) ! TODO: max number of legs on the loop
    call initcachesystem_cll(cll_channels,maxpoint) ! TODO
    if (a_switch == 1) call setmode_cll(1)
    if (a_switch == 7) call setmode_cll(2)
    call setmuuv2_cll(mu2_UV)
    call setmuir2_cll(mu2_IR)
    call setdeltauv_cll(de1_UV)
    call setdeltair_cll(de1_IR,de2_1_IR)
    call settenred_cll(cll_tenred)
    call setaccuracy_cll(cll_pvthr,cll_accthr,cll_mode3thr)
  end if
! #ifdef COLLIER_LEGACY
#endif

  ! Initialisation of CutTools
  if ((a_switch == 5 .or. a_switch_rescue == 5) .and. cuttools_not_init) then
#ifdef USE_CUTTOOLS
    call ctsinit(opplimitvalue, oppscaloop, .true.)
    cuttools_not_init = .false.
! #else
!     write(*,*) 'ERROR: CutTools is deactivated.'
#endif
  end if
  ! Initialisation of Samurai
  if ((a_switch == 6 .or. a_switch_rescue == 6) .and. samurai_not_init) then
#if defined(USE_SAMURAI) && defined(PRECISION_dp)
    call initsamurai(set_imeth, set_isca, set_verbosity, set_itest)
    samurai_not_init = .false.
! #else
!     write(*,*) 'ERROR: Samurai is deactivated.'
#endif
  end if
  ! Set AvH OneLOop parameters
  if (a_switch == 5 .or. a_switch == 6 .or. a_switch_rescue == 5 .or. a_switch_rescue == 6 .or. redlib_qp == 5) then
#ifdef USE_ONELOOP
    if (reset_oppthrs) then
      call olo_onshell(oppthrs)
      reset_oppthrs = .false.
    end if
    if (reset_mureg) then
      call olo_scale(mureg)
      reset_mureg = .false.
    end if
! #else
!     write(*,*) 'ERROR: CutTools and Samurai are both deactivated.'
#endif
  end if

  ! Initialisation of BuildTensors library
  if (tensorlib_not_init .and. (a_switch == 1 .or. a_switch == 2 .or. a_switch == 3 .or. a_switch == 7 &
  & .or. a_switch_rescue == 1 .or. a_switch_rescue == 7)) then
#if defined(USE_COLLIER) && defined(COLLIER_LEGACY)
    call tensor_ints_init()
    tensorlib_not_init = .false.
! #else
!     write(*,*) 'ERROR: Collier (BuildTensors) is deactivated.'
#endif
  end if

! ifdef PRECISION_dp
#else

  ! DD qp initialisation
!  if (a_switch == 7 .or. a_switch_rescue == 7) then
  if ( redlib_qp == 7 ) then
#if defined(USE_COLLIER) && defined(COLLIER_LEGACY)
    if (dd_qp_not_init) then
      call dd_setmode(1._qp, 1._qp, 0, 0, 0, 0)
      dd_qp_not_init = .false.
    end if
    if (tensorlib_qp_not_init) then
      call tensor_ints_init()
      tensorlib_qp_not_init = .false.
    end if
    call dd_setparam(de1_UV, mu2_UV, de2_1_IR, de1_IR, mu2_IR, [0,0,0,0,0,0,0,0,0,0]*0._qp)
#endif
  end if

! ifdef PRECISION_dp
#endif

  call qcd_renormalisation
!   if (ew_renorm_switch /= 0) then
!     call ew_renormalisation
!   end if

  ! Increment number of time this function has been called:
#ifdef PRECISION_dp
  loop_parameters_status = loop_parameters_status + 1
#else
  loop_parameters_status = loop_parameters_status_dp
#endif

end subroutine loop_parameters_init



subroutine ensure_mp_loop_init()
  ! synchronise non-dp parameters with dp if they are not up to date
  ! should be called after loop_parameters_init()
  ! and in loop matrix element routines before anything is done
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/REALKIND, only: parameters_status
  use ol_loop_parameters_decl_/**/REALKIND, only: loop_parameters_status
  use ol_parameters_decl_/**/DREALKIND, only: &
    & parameters_status_dp => parameters_status
  use ol_loop_parameters_decl_/**/DREALKIND, only: &
    & loop_parameters_status_dp => loop_parameters_status
  implicit none
  if (parameters_status_dp /= parameters_status) &
    & call parameters_init()
  if (loop_parameters_status_dp /= loop_parameters_status) &
    & call loop_parameters_init()
#endif
end subroutine ensure_mp_loop_init



#ifdef PRECISION_dp
subroutine parameters_write() bind(c,name="ol_parameters_write")
#else
subroutine parameters_write()
#endif
  use KIND_TYPES, only: REALKIND
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND
  implicit none
  write(*,*) 'coupling constants'
  write(*,*) 'alpha_s   =', alpha_QCD
  write(*,*) 'alpha_qed =', alpha_QED
  write(*,*) 'sw2       =', sw2
  write(*,*)
  write(*,*) 'particle masses and widths'
  write(*,*) 'ME = ', MU, 'rME =', rMU, 'wME =', wMU
  write(*,*) 'MM = ', MU, 'rMM =', rMU, 'wMM =', wMU
  write(*,*) 'ML = ', MU, 'rML =', rMU, 'wML =', wMU
  write(*,*) 'MU = ', MU, 'rMU =', rMU, 'wMU =', wMU
  write(*,*) 'MD = ', MD, 'rMD =', rMD, 'wMD =', wMD
  write(*,*) 'MS = ', MS, 'rMS =', rMS, 'wMS =', wMS
  write(*,*) 'MC = ', MC, 'rMC =', rMC, 'wMC =', wMC
  write(*,*) 'MB = ', MB, 'rMB =', rMB, 'wMB =', wMB
  write(*,*) 'MT = ', MT, 'rMT =', rMT, 'wMT =', wMT
  write(*,*) 'MW = ', MW, 'rMW =', rMW, 'wMW =', wMW
  write(*,*) 'MZ = ', MZ, 'rMZ =', rMZ, 'wMZ =', wMZ
  write(*,*) 'MH = ', MH, 'rMH =', rMH, 'wMH =', wMH
  write(*,*) 'MX = ', MX, 'rMX =', rMX, 'wMX =', wMX
  write(*,*) 'MY = ', MY, 'rMY =', rMY, 'wMY =', wMY
  write(*,*)
  write(*,*) 'renscale          =', mureg
  write(*,*) 'pole1_UV          =', de1_UV
  write(*,*) 'pole1_IR          =', de1_IR
  write(*,*) 'pole2_IR          =', de2_i_IR
  write(*,*) 'fact_UV           =', x_UV
  write(*,*) 'fact_IR           =', x_IR
#ifdef PRECISION_dp
  write(*,*) 'N_quarks          =', nf
  write(*,*) 'light quarks      =', N_lf
  write(*,*) 'fermion_loops     =', SwF
  write(*,*) 'nonfermion_loops  =', SwB
  write(*,*) 'CT_on             =', CT_is_on
  write(*,*) 'R2_on             =', R2_is_on
  write(*,*) 'IR_on             =', IR_is_on
  write(*,*) 'polecheck         =', polecheck_is
  write(*,*) 'polenorm_swi      =', norm_swi
  write(*,*) 'i-operator mode   =', ioperator_mode
  write(*,*) 'last_switch       =', l_switch
  write(*,*) 'amp_switch        =', a_switch
  write(*,*) 'amp_switch_rescue =', a_switch_rescue
  write(*,*) 'ew_renorm_switch  =', ew_renorm_switch
  write(*,*) 'use_coli_cache    =', coli_cache_use
  write(*,*) 'check_Ward_tree   =', Ward_tree
  write(*,*) 'check_Ward_loop   =', Ward_loop
  write(*,*) 'out_symmetry      =', out_symmetry_on
#endif
! opp_rootsvalue, opp_limitvalue, opp_thrs, opp_idig, opp_scaloop
! sam_isca, sam_verbosity, sam_itest
! set_C_PV_threshold, set_D_PV_threshold, set_dd_red_mode
end subroutine parameters_write

end module ol_parameters_init_/**/REALKIND
