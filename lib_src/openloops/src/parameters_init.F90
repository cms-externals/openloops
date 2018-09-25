!******************************************************************************!
! Copyright (C) 2014-2018 OpenLoops Collaboration. For authors see authors.txt !
!                                                                              !
! This file is part of OpenLoops.                                              !
!                                                                              !
! OpenLoops is free software: you can redistribute it and/or modify            !
! it under the terms of the GNU General Public License as published by         !
! the Free Software Foundation, either version 3 of the License, or            !
! (at your option) any later version.                                          !
!                                                                              !
! OpenLoops is distributed in the hope that it will be useful,                 !
! but WITHOUT ANY WARRANTY; without even the implied warranty of               !
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                !
! GNU General Public License for more details.                                 !
!                                                                              !
! You should have received a copy of the GNU General Public License            !
! along with OpenLoops.  If not, see <http://www.gnu.org/licenses/>.           !
!******************************************************************************!


module ol_parameters_init_/**/REALKIND
  use ol_debug
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
  if (cms_on == 0) then
    M   = rM
    rM2 = rM*rM
  else
    M = sqrt(M2)
    rM2 = real(M2)
    if (rM < 0) M = -M
  end if
end subroutine masspowers



#ifdef PRECISION_dp

subroutine parameters_init()
  ! Initialise parameters for tree amplitudes.
#ifdef USE_IFORT
  use IFPORT
#endif
  use KIND_TYPES, only: REALKIND
  use ol_generic, only: to_string, random_string
  use ol_parameters_decl_/**/REALKIND
#if defined(COLLIER_LEGACY) && defined(USE_COLLIER)
  use bt_TI_lib_switch, only: TI_library ! from COLI: module to switch between COLI and DD libraries
#endif
  use ol_version, only: splash_todo, print_welcome
  implicit none

  if (parameters_status == 0) then
    pid_string = trim(to_string(getpid())) // "-" // random_string(4)
  end if

  if (splash_todo .and. .not. nosplash) then
    call print_welcome()
  end if

#if defined(COLLIER_LEGACY) && defined(USE_COLLIER)
  if (a_switch == 1) TI_library = 1 ! use COLI
  if (a_switch == 7) TI_library = 2 ! use DD
#endif

  ! set mass of V-auxiliary fields
  rMX_unscaled = rMZ_unscaled
  rMY_unscaled = rMW_unscaled

  rME = scalefactor * rME_unscaled
  wME = scalefactor * wME_unscaled
  rYE = scalefactor * rYE_unscaled
  rMM = scalefactor * rMM_unscaled
  wMM = scalefactor * wMM_unscaled
  rYM = scalefactor * rYM_unscaled
  rML = scalefactor * rML_unscaled
  wML = scalefactor * wML_unscaled
  rYL = scalefactor * rYL_unscaled
  rMU = scalefactor * rMU_unscaled
  wMU = scalefactor * wMU_unscaled
  rYU = scalefactor * rYU_unscaled
  rMD = scalefactor * rMD_unscaled
  wMD = scalefactor * wMD_unscaled
  rYD = scalefactor * rYD_unscaled
  rMS = scalefactor * rMS_unscaled
  wMS = scalefactor * wMS_unscaled
  rYS = scalefactor * rYS_unscaled
  rMC = scalefactor * rMC_unscaled
  wMC = scalefactor * wMC_unscaled
  rYC = scalefactor * rYC_unscaled
  rMB = scalefactor * rMB_unscaled
  wMB = scalefactor * wMB_unscaled
  rYB = scalefactor * rYB_unscaled
  wYB = scalefactor * wYB_unscaled
  rMT = scalefactor * rMT_unscaled
  wMT = scalefactor * wMT_unscaled
  rYT = scalefactor * rYT_unscaled
  wYT = scalefactor * wYT_unscaled
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

  rMA0 = scalefactor * rMA0_unscaled
  wMA0 = scalefactor * wMA0_unscaled
  rMHH = scalefactor * rMHH_unscaled
  wMHH = scalefactor * wMHH_unscaled
  rMHp = scalefactor * rMHp_unscaled
  wMHp = scalefactor * wMHp_unscaled

  MREG = scalefactor * MREG_unscaled

  Gmu  = Gmu_unscaled / scalefactor**2

! ifdef PRECISION_dp
#else

subroutine parameters_init()
  ! non-dp initialisation: synchronise with dp parameters
  use KIND_TYPES, only: REALKIND
  use ol_parameters_decl_/**/REALKIND
  use ol_parameters_decl_/**/DREALKIND, only: &
    & model, parameters_verbose, cms_on => cms_on, ew_scheme => ew_scheme, &
    & parameters_status_dp => parameters_status,  alpha_QCD_dp => alpha_QCD, &
    & alpha_QED_0_dp => alpha_QED_0, alpha_QED_MZ_dp => alpha_QED_MZ, Gmu_dp => Gmu, &
    & rME_dp => rME, wME_dp => wME, rMM_dp => rMM, wMM_dp => wMM, rML_dp => rML, wML_dp => wML, &
    & rMU_dp => rMU, wMU_dp => wMU, rMD_dp => rMD, wMD_dp => wMD, rMS_dp => rMS, wMS_dp => wMS, &
    & rMC_dp => rMC, wMC_dp => wMC, rMB_dp => rMB, wMB_dp => wMB, rMT_dp => rMT, wMT_dp => wMT, &
    & rMW_dp => rMW, wMW_dp => wMW, rMZ_dp => rMZ, wMZ_dp => wMZ, rMH_dp => rMH, wMH_dp => wMH, &
    & rMX_dp => rMX, wMX_dp => wMX, rMY_dp => rMY, wMY_dp => wMY, &
    & rYE_dp => rYE, rYM_dp => rYM, rYL_dp => rYL, rYU_dp => rYU, rYD_dp => rYD, rYS_dp => rYS, &
    & rYC_dp => rYC, rYB_dp => rYB, wYB_dp => wYB, rYT_dp => rYT, wYT_dp => wYT, &
    & rMA0_dp => rMA0, wMA0_dp => wMA0, rMHH_dp => rMHH, wMHH_dp => wMHH, rMHp_dp => rMHp, wMHp_dp => wMHp, &
    & thdmTB_dp => thdmTB, thdmSBA_dp => thdmSBA, thdmL5_dp => thdmL5
  use ol_parameters_decl_/**/QREALKIND, only: scalefactor
  implicit none

  rMX_unscaled = rMZ_unscaled
  rMY_unscaled = rMW_unscaled

  rME = scalefactor * rME_unscaled
  wME = scalefactor * wME_unscaled
  rYE = scalefactor * rYE_unscaled
  rMM = scalefactor * rMM_unscaled
  wMM = scalefactor * wMM_unscaled
  rYM = scalefactor * rYM_unscaled
  rML = scalefactor * rML_unscaled
  wML = scalefactor * wML_unscaled
  rYL = scalefactor * rYL_unscaled
  rMU = scalefactor * rMU_unscaled
  wMU = scalefactor * wMU_unscaled
  rYU = scalefactor * rYU_unscaled
  rMD = scalefactor * rMD_unscaled
  wMD = scalefactor * wMD_unscaled
  rYD = scalefactor * rYD_unscaled
  rMS = scalefactor * rMS_unscaled
  wMS = scalefactor * wMS_unscaled
  rYS = scalefactor * rYS_unscaled
  rMC = scalefactor * rMC_unscaled
  wMC = scalefactor * wMC_unscaled
  rYC = scalefactor * rYC_unscaled
  rMB = scalefactor * rMB_unscaled
  wMB = scalefactor * wMB_unscaled
  rYB = scalefactor * rYB_unscaled
  wYB = scalefactor * wYB_unscaled
  rMT = scalefactor * rMT_unscaled
  wMT = scalefactor * wMT_unscaled
  rYT = scalefactor * rYT_unscaled
  wYT = scalefactor * wYT_unscaled
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

  rMA0 = scalefactor * rMA0_unscaled
  wMA0 = scalefactor * wMA0_unscaled
  rMHH = scalefactor * rMHH_unscaled
  wMHH = scalefactor * wMHH_unscaled
  rMHp = scalefactor * rMHp_unscaled
  wMHp = scalefactor * wMHp_unscaled

  MREG = scalefactor * MREG_unscaled

  Gmu  = Gmu_unscaled / scalefactor**2

  alpha_QED_0  = alpha_QED_0_dp
  alpha_QED_MZ = alpha_QED_MZ_dp
  alpha_QCD    = alpha_QCD_dp

  thdmTB = thdmTB_dp
  thdmSBA = thdmSBA_dp
  thdmL5 = thdmL5_dp



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

  call masspowers(rYE, rZERO, YE, YE2, rYE2)
  call masspowers(rYM, rZERO, YM, YM2, rYM2)
  call masspowers(rYL, rZERO, YL, YL2, rYL2)
  call masspowers(rYU, rZERO, YU, YU2, rYU2)
  call masspowers(rYD, rZERO, YD, YD2, rYD2)
  call masspowers(rYS, rZERO, YS, YS2, rYS2)
  call masspowers(rYC, rZERO, YC, YC2, rYC2)
  call masspowers(rYB, wYB, YB, YB2, rYB2)
  call masspowers(rYT, wYT, YT, YT2, rYT2)

  call masspowers(rMA0, wMA0, MA0, MA02, rMA02)
  call masspowers(rMHH, wMHH, MHH, MHH2, rMHH2)
  call masspowers(rMHp, wMHp, MHp, MHp2, rMHp2)

  ! Dependent couplings

  !QCD
  G2_QCD = 4*pi*alpha_QCD
  gQCD   = sqrt(G2_QCD)

  !EW
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

  if (ew_scheme == 0) then ! alpha(0) OS scheme
    alpha_QED = alpha_QED_0
  else if (ew_scheme == 1) then ! Gmu scheme
    if (cms_on == 1 ) then
      alpha_QED = sqrt2/pi*Gmu*abs(MW2*sw2)
    else if (cms_on == 2) then
      alpha_QED = sqrt2/pi*Gmu*rMW2*(1.-rMW2/rMZ2)
    else
      alpha_QED = sqrt2/pi*Gmu*rMW2*sw2
    end if
  else if (ew_scheme == 2) then ! alpha(MZ) scheme
    alpha_QED = alpha_QED_MZ
  end if

  E2_QED = 4*pi*alpha_QED
  eQED   = sqrt(E2_QED)

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
  gPud = [   -YD,   YU ]
  gPus = [   -YS,   YU ]
  gPub = [   -YB,   YU ]
  gPcd = [   -YD,   YC ]
  gPcs = [   -YS,   YC ]
  gPcb = [   -YB,   YC ]
  gPtd = [   -YD,   YT ]
  gPts = [   -YS,   YT ]
  gPtb = [   -YB,   YT ]
  gPdu = [   -YU,   YD ]
  gPdc = [   -YC,   YD ]
  gPdt = [   -YT,   YD ]
  gPsu = [   -YU,   YS ]
  gPsc = [   -YC,   YS ]
  gPst = [   -YT,   YS ]
  gPbu = [   -YU,   YB ]
  gPbc = [   -YC,   YB ]
  gPbt = [   -YT,   YB ]

  if (trim(model) == "2hdm") call thdm_parameters_init()

  if (trim(model) == "higgspo") call model_higgspo_parameters_init()


  ! Number of time this function has been called:
#ifdef PRECISION_dp
  parameters_status = parameters_status + 1
#else
  parameters_status = parameters_status_dp
#endif

  ! write parameters
  if (parameters_verbose == 1 ) then
    call parameters_write()
  end if

   call ol_msg(4, "Parameters initialized")
end subroutine parameters_init



subroutine thdm_parameters_init()
  use ol_debug, only: ol_fatal
  use ol_parameters_decl_/**/REALKIND
  implicit none
  ! mixing angles
  thdm_b = atan(thdmTB)
  thdm_a = thdm_b - asin(thdmSBA)
  thdmCA  = cos(thdm_a)
  thdmSA  = sin(thdm_a)
  thdmCB  = cos(thdm_b)
  thdmSB  = sin(thdm_b)
  thdmC2A = cos(2*thdm_a)
  thdmS2A = sin(2*thdm_a)
  thdmC2B = cos(2*thdm_b)
  thdmS2B = sin(2*thdm_b)
  thdmCAB = cos(thdm_a+thdm_b)
  thdmSAB = sin(thdm_a+thdm_b)
  thdmCBA = cos(thdm_b-thdm_a)
  if (thdmTB == 0) then
    call ol_fatal("2HDM model parameter ill defined: tan(beta) = 0")
  end if
  ! 2HDM Type I or Type II
  if (thdm_type == 1) then
    if (thdmSB == 0) then
      call ol_fatal("2HDM-Type-I model parameter ill defined: sin(beta) = 0")
    end if
    thdmYuk1 = thdmCA/thdmSB
    thdmYuk2 = thdmSA/thdmSB
    thdmYuk3 = -1/thdmTB
  else if (thdm_type == 2) then
    if (thdmCB == 0) then
      call ol_fatal("2HDM-Type-II model parameter ill defined: cos(beta) = 0")
    end if
    thdmYuk1 = -thdmSA/thdmCB
    thdmYuk2 = thdmCA/thdmCB
    thdmYuk3 = thdmTB
  end if
  ! 2HDM charged higgs quark couplings
  thdmHpud = [-YD*(-thdmYuk3), YU/thdmTB]
  thdmHpcs = [-YS*(-thdmYuk3), YC/thdmTB]
  thdmHptb = [-YB*(-thdmYuk3), YT/thdmTB]
  thdmHpdu = [-YU/thdmTB, YD*(-thdmYuk3)]
  thdmHpsc = [-YC/thdmTB, YS*(-thdmYuk3)]
  thdmHpbt = [-YT/thdmTB, YB*(-thdmYuk3)]
end subroutine thdm_parameters_init


! *********************************************************************
! parameters_init for HiggsPO model
! *********************************************************************
#ifdef PRECISION_dp
subroutine model_higgspo_parameters_init()
  use ol_parameters_decl_/**/REALKIND
  implicit none
  complex(REALKIND) :: gRL
  HPOvev = scalefactor * HPOvev_unscaled
#else
subroutine model_higgspo_parameters_init()
  use ol_parameters_decl_/**/REALKIND
  use ol_parameters_decl_/**/DREALKIND, only: scalefactor_dp => scalefactor, &
    &  HPOvev_dp => HPOvev, ThetaCabi_dp => ThetaCabi, &
    &  HPOgZeL_dp => HPOgZeL, HPOgZeR_dp => HPOgZeR, HPOgZv_dp => HPOgZv, HPOgZuL_dp => HPOgZuL, &
    &  HPOgZuR_dp => HPOgZuR, HPOgZdL_dp  => HPOgZdL, HPOgZdR_dp => HPOgZdR, &
    &  HPOgWeL_dp => HPOgWeL, HPOgWmL_dp => HPOgWmL, HPOgWlL_dp => HPOgWlL, HPOgWqL_dp => HPOgWqL, &
    &  HPOkapWW_dp  => HPOkapWW, HPOkapZZ_dp  => HPOkapZZ, HPOepsWW_dp  => HPOepsWW, &
    &  HPOaepsWW_dp => HPOaepsWW, HPOepsZZ_dp  => HPOepsZZ, HPOaepsZZ_dp => HPOaepsZZ, &
    &  HPOepsZA_dp  => HPOepsZA, HPOaepsZA_dp => HPOaepsZA, HPOepsAA_dp  => HPOepsAA, &
    &  HPOaepsAA_dp => HPOaepsAA, HPOepsZnn_dp => HPOepsZnn, HPOepsZll_dp => HPOepsZll, &
    &  HPOepsZdd_dp => HPOepsZdd, HPOepsZuu_dp => HPOepsZuu, HPOepsWqq_dp => HPOepsWqq, HPOepsWln_dp => HPOepsWln, &
    &  HPOphiWeL_dp => HPOphiWeL, HPOphiWmL_dp => HPOphiWmL, HPOphiWlL_dp => HPOphiWlL, HPOphiWqL_dp => HPOphiWqL
  implicit none
  complex(REALKIND) :: gRL
  scalefactor = scalefactor_dp
  ThetaCabi = ThetaCabi_dp
  HPOvev    = HPOvev_dp
  HPOgZeL   = HPOgZeL_dp
  HPOgZeR   = HPOgZeR_dp
  HPOgZv    = HPOgZv_dp
  HPOgZuL   = HPOgZuL_dp
  HPOgZuR   = HPOgZuR_dp
  HPOgZdL   = HPOgZdL_dp
  HPOgZdR   = HPOgZdR_dp
  HPOgWeL   = HPOgWeL_dp
  HPOgWmL   = HPOgWmL_dp
  HPOgWlL   = HPOgWlL_dp
  HPOgWqL   = HPOgWqL_dp
  HPOkapWW  = HPOkapWW_dp
  HPOkapZZ  = HPOkapZZ_dp
  HPOepsWW  = HPOepsWW_dp
  HPOaepsWW = HPOaepsWW_dp
  HPOepsZZ  = HPOepsZZ_dp
  HPOaepsZZ = HPOaepsZZ_dp
  HPOepsZA  = HPOepsZA_dp
  HPOaepsZA = HPOaepsZA_dp
  HPOepsAA  = HPOepsAA_dp
  HPOaepsAA = HPOaepsAA_dp
  HPOepsZnn = HPOepsZnn_dp
  HPOepsZll = HPOepsZll_dp
  HPOepsZdd = HPOepsZdd_dp
  HPOepsZuu = HPOepsZuu_dp
  HPOepsWqq = HPOepsWqq_dp
  HPOepsWln = HPOepsWln_dp
  HPOphiWeL = HPOphiWeL_dp
  HPOphiWmL = HPOphiWmL_dp
  HPOphiWlL = HPOphiWlL_dp
  HPOphiWqL = HPOphiWqL_dp
#endif
  cCabi = cos(ThetaCabi)
  sCabi = sin(ThetaCabi)
!  gRL = 1/(cw*sw)
  gRL = 2*MZ/HPOvev
  gZn  = [  ZERO      , gRL*HPOgZv ] ! neutrino
  gZl  = gRL*[ HPOgZeR, HPOgZeL    ] ! lepton
  gZu  = gRL*[ HPOgZuR, HPOgZuL    ] ! up
  gZd  = gRL*[ HPOgZdR, HPOgZdL    ] ! down

  HPOcpWeL = cos(HPOphiWeL)
  HPOspWeL = sin(HPOphiWeL)
  HPOcpWmL = cos(HPOphiWmL)
  HPOspWmL = sin(HPOphiWmL)
  HPOcpWlL = cos(HPOphiWlL)
  HPOspWlL = sin(HPOphiWlL)
  HPOcpWqL = cos(HPOphiWqL)
  HPOspWqL = sin(HPOphiWqL)
end subroutine model_higgspo_parameters_init




subroutine ensure_mp_init()
  ! synchronise non-dp parameters with dp if they are not up to date
  ! should be called after tree_parameters_flush()
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
  use ol_generic, only: to_string
  use ol_debug, only: ol_error, ol_fatal
#ifdef USE_COLLIER
#ifndef COLLIER_LEGACY
  use collier, only: initevent_cll
#endif
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
        call ol_error(2, 'subroutine channel_on:')
        call ol_error(2, 'next channel = ' // to_string(next_channel_number) // '/' // to_string(maxcache))
        call ol_error(2, 'to handle more channels increase maxcache')
        call ol_error(2,  'in collier/src/coli_params_cache.h')
        call ol_fatal()
      else
        call cacheon(ch)
      end if
    end if

    call cachereonch(ch)  ! cache channel reactivated
    call cacheinit(0,ch) ! cache channel initialised for new PS point
  end if
#else
  if (coli_cache_use /= 0 .and. (a_switch == 1 .or. a_switch == 2 .or. a_switch == 3 .or. a_switch == 7)) then
    if (ch == -1) then
      ch = next_channel_number
      next_channel_number = next_channel_number + 1
    end if
    if (a_switch == 7) then
      call initevent_cll(2*ch)
    else
      call initevent_cll(2*ch-1)
    end if
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


subroutine init_kin_arrays(Npart)
  use ol_momenta_decl_/**/REALKIND, only: Q, L, QInvariantsMatrix
  use ol_external_decl_/**/REALKIND, only: P_ex, binom2, crossing, inverse_crossing, gf_array, Ward_array
  use ol_external_decl_/**/REALKIND, only: allocatedNpart
  implicit none
  integer, intent(in) :: Npart
  integer :: n_
  if (Npart > allocatedNpart) then
    if (allocatedNpart /= 0) then
      call clean_kin_arrays()
    end if
    allocate(Q(5,0:2**Npart-1))
    Q = 0
    allocate(L(6,0:2**Npart-1))
    L = 0
    allocate(QInvariantsMatrix(Npart,Npart))
    QInvariantsMatrix = 0

    allocate(binom2(Npart))
    binom2 = [((n_*(n_-1))/2, n_=1, size(binom2))]

    allocate(P_ex(0:3,Npart))  ! uncleaned external 2->n-2 momenta, set by conv_mom_scatt2in
    allocate(crossing(Npart))
    crossing = 0        ! only used if a reduction error occurs
    allocate(inverse_crossing(Npart))
    inverse_crossing = 0 ! set by conv_mom_scatt2in
    !
    allocate(gf_array(Npart))
    gf_array = 0
    allocate(Ward_array(Npart))
    Ward_array = 0

    allocatedNpart = Npart
  end if
end subroutine init_kin_arrays

subroutine clean_kin_arrays
  use ol_external_decl_/**/REALKIND, only: allocatedNpart
  use ol_momenta_decl_/**/REALKIND, only: Q, L, QInvariantsMatrix
  use ol_external_decl_/**/REALKIND, only: P_ex, binom2, crossing, inverse_crossing, gf_array, Ward_array
  implicit none
  if (allocated(Q)) deallocate(Q)
  if (allocated(L)) deallocate(L)
  if (allocated(QInvariantsMatrix)) deallocate(QInvariantsMatrix)
  if (allocated(binom2)) deallocate(binom2)
  if (allocated(P_ex)) deallocate(P_ex)
  if (allocated(crossing)) deallocate(crossing)
  if (allocated(inverse_crossing)) deallocate(inverse_crossing)
  if (allocated(gf_array)) deallocate(gf_array)
  if (allocated(Ward_array)) deallocate(Ward_array)
  allocatedNpart = 0
end subroutine

#if defined(USE_COLLIER) && defined(COLLIER_LEGACY)
! *************************
subroutine tensor_ints_init()
! *************************
  use bt_BuildTensors_/**/REALKIND, only: init_tables
  implicit none
  call init_tables(6,6)
end subroutine tensor_ints_init
#endif



subroutine tensorrank_init(rank)
  use ol_generic, only: binomial
  use ol_tensor_storage_/**/REALKIND, only: tensor_stored, tensor_storage_maxrank
  use ol_tensor_bookkeeping, only: initialised_rank, init_tensorbookkeeping
  implicit none
  integer, intent(in) :: rank
  if (rank > initialised_rank) call init_tensorbookkeeping(rank)
  if (allocated(tensor_stored)) deallocate(tensor_stored)
  allocate(tensor_stored(binomial(rank+4,4)))
  tensor_storage_maxrank = rank
end subroutine tensorrank_init



#ifdef PRECISION_dp
! **********************************************************************
subroutine loop_parameters_init(pole1_UV, pole1_IR, pole2_IR, CT_on, IR_on)
! **********************************************************************
! Initialise parameters for loop amplitudes.
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
  use ol_generic, only: to_string
  use ol_debug, only: ol_error, ol_fatal, olodebug_unit
  use ol_cwrappers, only: stdout_off, stdout_on
  use ol_tensor_storage_/**/REALKIND, only: tensor_storage_maxrank
  use ol_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/REALKIND
  use ol_qcd_renormalisation_/**/REALKIND, only: qcd_renormalisation
  use ol_ew_renormalisation_/**/REALKIND, only: ew_renormalisation
#ifdef USE_COLLIER
#ifdef COLLIER_LEGACY
  use dd_init_/**/REALKIND, only: dd_setmode, dd_setparam
#else
  use collier, only: init_cll, initcachesystem_cll, setmode_cll, setmuuv2_cll, &
    & setmuir2_cll, setdeltauv_cll, setdeltair_cll, settenred_cll, setaccuracy_cll, &
    & initmonitoring_cll, SwitchOffErrStop_cll
#endif
#endif
#ifdef USE_ONELOOP
  use avh_olo, only: olo_scale_prec, olo_onshell, olo_unit
#endif
  implicit none
#ifdef USE_QCDLOOP
  interface
    subroutine qlshowsplash()
      implicit none
    end subroutine qlshowsplash
    subroutine qlcachesize(sz)
      implicit none
      integer, intent(in) :: sz
    end subroutine qlcachesize
  end interface
#endif
  ! keep these optional arguments for compatibility with old process libraries
  real(REALKIND), intent(in), optional :: pole1_UV, pole1_IR, pole2_IR
  integer,        intent(in), optional :: CT_on, IR_on

  real(REALKIND) :: mp2(10)

  if (present(pole1_UV)) de1_UV   = pole1_UV
  if (present(pole1_IR)) de1_IR   = pole1_IR
  if (present(pole2_IR)) de2_i_IR = pole2_IR
  if (present(CT_on)) CT_is_on = CT_on
  if (present(IR_on)) IR_is_on = IR_on

  if (reset_scalefactor) then
    reset_mureg = .true.
    reset_olo = .true.
    reset_scalefactor = .false.
  end if

  opprootsvalue = scalefactor * opprootsvalue_unscaled
  mureg = scalefactor * mureg_unscaled
  muren = scalefactor * muren_unscaled
  muyc = scalefactor * muyc_unscaled
  muyb = scalefactor * muyb_unscaled
  muyt = scalefactor * muyt_unscaled

  ! convention for dim-reg Poles K_i(eps)/eps^N
  if (norm_swi == 0) then ! Les-Houches Accord normalisation (default)
    de2_i_shift = 0
    norm_name   = 'LH-accord '
  else if (norm_swi == 1) then ! COLI normalisation
    de2_i_shift = pi2_6
    norm_name   = 'COLI      '
  else
    call ol_error(2,'routine loop_parameters_init: ')
    call ol_error(2,'norm_swi = ' // to_string(norm_swi) // ' not allowed.')
    call ol_fatal()
  end if

! ifdef PRECISION_dp
#else

subroutine loop_parameters_init
  ! non-dp initialisation: synchronise with dp parameters
  use ol_tensor_storage_/**/REALKIND, only: tensor_storage_maxrank
  use ol_parameters_decl_/**/REALKIND, only: pi2_6
  use ol_parameters_decl_/**/REALKIND, only: reset_scalefactor
  use ol_loop_parameters_decl_/**/REALKIND
  use ol_loop_parameters_decl_/**/QREALKIND, only: scalefactor, mureg
  use ol_loop_parameters_decl_/**/DREALKIND, only: reset_mureg, reset_olo
  use ol_loop_parameters_decl_/**/DREALKIND, only: &
    & loop_parameters_status_dp => loop_parameters_status, norm_swi, a_switch, a_switch_rescue, redlib_qp, &
    & dd_qp_not_init, tensorlib_qp_not_init, mureg_dp => mureg, muren_dp => muren, fact_UV_dp => x_UV, fact_IR_dp => x_IR, &
    & pole1_UV_dp => de1_UV, pole1_IR_dp => de1_IR, pole2_IR_dp => de2_i_IR, do_ew_renorm, do_qcd_renorm, maxrank
#if defined(USE_COLLIER) && defined(COLLIER_LEGACY)
  use dd_init_/**/REALKIND, only: dd_setmode, dd_setparam
#endif
  use ol_qcd_renormalisation_/**/REALKIND, only: qcd_renormalisation
  use ol_ew_renormalisation_/**/REALKIND, only: ew_renormalisation
  use ol_loop_parameters_decl_/**/DREALKIND, only: opprootsvalue, opprootsvalue_unscaled
#ifdef USE_ONELOOP
  use avh_olo, only: olo_scale_prec, olo_onshell, olo_unit
#endif
  implicit none

  if (norm_swi == 0) then
    de2_i_shift = 0
  else if (norm_swi == 1) then
    de2_i_shift = pi2_6
  end if

  if (reset_scalefactor) then
    reset_mureg = .true.
    reset_olo = .true.
    reset_scalefactor = .false.
  end if

  opprootsvalue = scalefactor * opprootsvalue_unscaled
  mureg = scalefactor * mureg_unscaled
  muren = scalefactor * muren_unscaled
  muyc = scalefactor * muyc_unscaled
  muyb = scalefactor * muyb_unscaled
  muyt = scalefactor * muyt_unscaled

  x_UV     = fact_UV_dp
  x_IR     = fact_IR_dp
  de1_UV   = pole1_UV_dp
  de1_IR   = pole1_IR_dp
  de2_i_IR = pole2_IR_dp

! ifdef PRECISION_dp
#endif

  if (maxrank > tensor_storage_maxrank) call tensorrank_init(maxrank)

  de2_0_IR = de2_i_IR - de2_i_shift         ! LH-norm double pole
  de2_1_IR = de2_i_IR - de2_i_shift + pi2_6 ! COLI-norm double pole
  ! renormalisation & regularization scale
  mureg2 = mureg**2
  muren2 = muren**2

  if (muyc /= 0) then
    muyc2 = muyc**2
  else
    muyc2 = YC2
  end if
  if (muyb /= 0) then
    muyb2 = muyb**2
  else
    muyb2 = YB2
  end if
  if (muyt /= 0) then
    muyt2 = muyt**2
  else
    muyt2 = YT2
  end if

  ! dim reg scale in UV-div loops
  mu2_UV = (x_UV**2)*mureg2
  mu2_IR = (x_IR**2)*mureg2

#ifdef PRECISION_dp
  ! initialise reduction libraries only in double precision
  ! (quad precision initialisation is handled within these libraries if applicable)

#ifdef USE_COLLIER
#ifdef COLLIER_LEGACY
  ! COLI initialisation
  if (a_switch == 1 .or. a_switch_rescue == 1 .or. a_switch == 2 .or. a_switch == 3) then
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
  end if

  ! DD initialisation
  if (a_switch == 7 .or. a_switch_rescue == 7) then
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
  end if
! #ifdef COLLIER_LEGACY
#else
  if (a_switch == 1 .or. a_switch_rescue == 1 .or. a_switch == 2 .or. a_switch == 3 .or. &
    & a_switch == 7 .or. a_switch_rescue == 7) then
    if (maxpoint > maxpoint_active .or. maxrank > maxrank_active .or. cll_channels > cll_channels_active) then
      if (maxpoint > maxpoint_active .or. maxrank > maxrank_active) then
        if (nosplash) call stdout_off()
        if (cll_log == 0) then
          call init_cll(maxpoint, rmax=maxrank, folder_name="", noreset=.true.)
        else
          call init_cll(maxpoint, rmax=maxrank, noreset=.true.)
        end if
        if (nosplash) call stdout_on()
      end if
      if (coli_cache_use /= 0) then
        ! cache channel number in OL is per process; cll requires caches per reduction library (coli/dd)
        call initcachesystem_cll(2*cll_channels,maxpoint)
        cll_channels_active = cll_channels
      end if
      maxpoint_active = maxpoint
      maxrank_active = maxrank
    end if
    if (a_switch == 1) call setmode_cll(1)
    if (a_switch == 7) call setmode_cll(2)
    call setmuuv2_cll(mu2_UV)
    call setmuir2_cll(mu2_IR)
    call setdeltauv_cll(de1_UV)
    call setdeltair_cll(de1_IR,de2_1_IR)
    call settenred_cll(cll_tenred)
    call setaccuracy_cll(cll_pvthr,cll_accthr,cll_mode3thr)
    if (no_collier_stop) call SwitchOffErrStop_cll
    if (cll_log == 2) call initmonitoring_cll()
  end if
! #ifdef COLLIER_LEGACY
#endif
! #ifdef USE_COLLIER
#endif

  ! Initialisation of CutTools
  if ((a_switch == 5 .or. a_switch_rescue == 5) .and. cuttools_not_init) then
#ifdef USE_CUTTOOLS
    if (nosplash) call stdout_off()
    call ctsinit(opplimitvalue, oppscaloop, .true.)
    if (nosplash) call stdout_on()
    cuttools_not_init = .false.
! #else
!     write(*,*) 'ERROR: CutTools is deactivated.'
#endif
  end if
  ! Set AvH OneLOop parameters
  if (a_switch == 5 .or. a_switch == 6 .or. a_switch_rescue == 5 .or. a_switch_rescue == 6) then
#ifdef USE_ONELOOP
    ! Silencing with stdout_off() not necessary, because CutTools already called olo routines.
    if (reset_olo) then
      if (nosplash) call stdout_off()
      call olo_unit(-1)
      if (nosplash) call stdout_on()
      olodebug_unit = -1
      if (olo_verbose > 0) call olo_unit(olo_outunit, "error")
      if (olo_verbose > 1) call olo_unit(olo_outunit, "warning")
      if (olo_verbose > 2) then
        call olo_unit(olo_outunit, "message")
        olodebug_unit = olo_outunit
      end if
      if (olo_verbose > 3) call olo_unit(olo_outunit, "printall")
      call olo_onshell(oppthrs)
      reset_olo = .false.
    end if
    if (reset_mureg) then
      call olo_scale_prec(mureg)
#ifndef USE_qp
      reset_mureg = .false.
#endif
    end if
! #else
!     write(*,*) 'ERROR: CutTools is deactivated.'
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
#ifdef USE_ONELOOP
    if (reset_mureg) then
      call olo_scale_prec(mureg)
      reset_mureg = .false.
    end if
#endif
! ifdef PRECISION_dp
#endif

  if (do_qcd_renorm /= 0) then
    call qcd_renormalisation
  end if
  if (do_ew_renorm /= 0) then
    call ew_renormalisation
  end if

  ! Increment number of time this function has been called:
#ifdef PRECISION_dp
  loop_parameters_status = loop_parameters_status + 1
#else
  loop_parameters_status = loop_parameters_status_dp
#endif

  call ol_msg(4, "Loop parameters initialized")
end subroutine loop_parameters_init



subroutine ensure_mp_loop_init()
  ! synchronise non-dp parameters with dp if they are not up to date
  ! should be called after parameters_flush()
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



!#ifdef PRECISION_dp
!subroutine parameters_write(filename) bind(c,name="ol_parameters_write")
!#else
subroutine parameters_write(filename)
!#endif
  use, intrinsic :: iso_fortran_env, only : stdout=>output_unit
  use KIND_TYPES, only: REALKIND
  use ol_generic, only: to_string
  use ol_debug, only: ol_error, ol_msg
  use ol_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: model, ew_scheme, ew_renorm_scheme
#endif
  use ol_loop_parameters_decl_/**/REALKIND
  implicit none
  character(len=*), optional :: filename
  integer :: outid, ios
  outid = stdout
  if (present(filename)) then
    if (len_trim(filename) > 0) then
      outid = 10
      open(outid, file=filename, status="replace", iostat=ios)
      if (ios /= 0) then
        call ol_error("ol_printparameter: error opening file " // trim(filename))
        call ol_msg("iostat =" // to_string(ios))
        return
      end if
    end if
  end if

  write(outid,*) '===================================================='
  write(outid,*) '================OpenLoops Parameters================'
  write(outid,*) '===================================================='
  write(outid,*) 'model =', trim(model)
  write(outid,*)
  write(outid,*) 'coupling constants'
  write(outid,*) 'alpha_s      =', alpha_QCD
  write(outid,*) 'alpha_qed    =', alpha_QED,    '  1/alpha_qed    =', 1/alpha_QED
  write(outid,*)
  write(outid,*) 'ew_scheme    =', ew_scheme
  write(outid,*) 'alpha_qed_0  =', alpha_QED_0,  '  1/alpha_qed_0  =', 1/alpha_QED_0
  write(outid,*) 'alpha_qed_MZ =', alpha_QED_MZ, '  1/alpha_qed_MZ =', 1/alpha_QED_MZ
  write(outid,*) 'Gmu          =', Gmu
  write(outid,*)
  write(outid,*) 'derived couplings'
  write(outid,*) 'sw           =', sw,           '  sw2            =', sw2
  write(outid,*) 'vev          =', 2*MW*sw/eQED
  if (trim(model) == "2hdm") then
    write(outid,*)
    write(outid,*) '2HDM tan(beta) =', thdmTB
    write(outid,*) '2HDM sin(beta-alpha) =', thdmSBA
  end if
  write(outid,*)
  write(outid,*) 'particle masses and widths'
  write(outid,*) 'ME = ', ME, 'rME =', rME, 'wME =', wMU, 'YE =', YE
  write(outid,*) 'MM = ', MM, 'rMM =', rMM, 'wMM =', wMU, 'YM =', YM
  write(outid,*) 'ML = ', ML, 'rML =', rML, 'wML =', wMU, 'YL =', YL
  write(outid,*) 'MU = ', MU, 'rMU =', rMU, 'wMU =', wMU, 'YU =', YU
  write(outid,*) 'MD = ', MD, 'rMD =', rMD, 'wMD =', wMD, 'YD =', YD
  write(outid,*) 'MS = ', MS, 'rMS =', rMS, 'wMS =', wMS, 'YS =', YS
  write(outid,*) 'MC = ', MC, 'rMC =', rMC, 'wMC =', wMC, 'YC =', YC
  write(outid,*) 'MB = ', MB, 'rMB =', rMB, 'wMB =', wMB, 'YB =', YB
  write(outid,*) 'MT = ', MT, 'rMT =', rMT, 'wMT =', wMT, 'YT =', YT
  write(outid,*) 'MW = ', MW, 'rMW =', rMW, 'wMW =', wMW
  write(outid,*) 'MZ = ', MZ, 'rMZ =', rMZ, 'wMZ =', wMZ
  write(outid,*) 'MH = ', MH, 'rMH =', rMH, 'wMH =', wMH
  write(outid,*) 'MX = ', MX, 'rMX =', rMX, 'wMX =', wMX
  write(outid,*) 'MY = ', MY, 'rMY =', rMY, 'wMY =', wMY
  if (trim(model) == "2hdm") then
    write(outid,*) 'MA0 = ', MA0, 'rMA0 =', rMA0, 'wMA0 =', wMA0
    write(outid,*) 'MHH = ', MHH, 'rMHH =', rMHH, 'wMHH =', wMHH
    write(outid,*) 'MHp = ', MHp, 'rMHp =', rMHp, 'wMHp =', wMHp
  end if
  if (trim(model) == "higgspo") then
    write(outid,*)
    write(outid,*) '==Higgs PO effective couplings=='
    write(outid,*) 'vev     = ', HPOvev
    write(outid,*) 'kapZZ   = ', HPOkapZZ,             'kapWW   = ', HPOkapWW
    write(outid,*) 'epsZZ   = ', HPOepsZZ,             'aepsZZ  = ',HPOaepsZZ
    write(outid,*) 'epsWW   = ', HPOepsWW,             'aepsWW  = ', HPOaepsWW
    write(outid,*) 'epsAA   = ', HPOepsAA,             'aepsAA  = ', HPOaepsAA
    write(outid,*) 'epsZA   = ', HPOepsZA,             'aepsZA  = ', HPOaepsZA
    write(outid,*) 'epsZuR  = ', real(HPOepsZuu(1,1)), 'epsZuL  = ', real(HPOepsZuu(1,2))
    write(outid,*) 'epsZdR  = ', real(HPOepsZdd(1,1)), 'epsZdL  = ', real(HPOepsZdd(1,2))
    write(outid,*) 'epsZcR  = ', real(HPOepsZuu(2,1)), 'epsZcL  = ', real(HPOepsZuu(2,2))
    write(outid,*) 'epsZsR  = ', real(HPOepsZdd(2,1)), 'epsZsL  = ', real(HPOepsZdd(2,2))
    write(outid,*) 'epsZtR  = ', real(HPOepsZuu(3,1)), 'epsZtL  = ', real(HPOepsZuu(3,2))
    write(outid,*) 'epsZbR  = ', real(HPOepsZdd(3,1)), 'epsZbL  = ', real(HPOepsZdd(3,2))
    write(outid,*) 'epsZneR = ', real(HPOepsZnn(1,1)), 'epsZneL = ', real(HPOepsZnn(1,2))
    write(outid,*) 'epsZeR  = ', real(HPOepsZll(1,1)), 'epsZeL  = ', real(HPOepsZll(1,2))
    write(outid,*) 'epsZnmR = ', real(HPOepsZnn(2,1)), 'epsZnmL = ', real(HPOepsZnn(2,2))
    write(outid,*) 'epsZmR  = ', real(HPOepsZll(2,1)), 'epsZmL  = ', real(HPOepsZll(2,2))
    write(outid,*) 'epsZnlR = ', real(HPOepsZnn(3,1)), 'epsZnlL = ', real(HPOepsZnn(3,2))
    write(outid,*) 'epsZlR  = ', real(HPOepsZll(3,1)), 'epsZlL  = ', real(HPOepsZll(3,2))
    write(outid,*) 'epsWlne = ', HPOepsWln(1),         'epsWdu  = ', HPOepsWqq(1)
    write(outid,*) 'epsWlnm = ', HPOepsWln(2),         'epsWsc  = ', HPOepsWqq(2)
    write(outid,*) 'epsWlnl = ', HPOepsWln(3),         'epsWbt  = ', HPOepsWqq(3)
  end if

  write(outid,*)
    write(outid,*) '==Technical Parameters=='
  write(outid,*) 'muren             =', muren
  write(outid,*) 'mureg             =', mureg
  write(outid,*) 'pole1_UV          =', de1_UV
  write(outid,*) 'pole1_IR          =', de1_IR
  write(outid,*) 'pole2_IR          =', de2_i_IR
  write(outid,*) 'fact_UV           =', x_UV
  write(outid,*) 'fact_IR           =', x_IR
  write(outid,*) 'ew_renorm_scheme  =', ew_renorm_scheme
#ifdef PRECISION_dp
  write(outid,*) 'N_quarks          =', nf
  write(outid,*) 'light quarks      =', N_lf
  write(outid,*) 'nq_nondecoupled   =', nq_nondecoupl
  write(outid,*) 'fermion_loops     =', SwF
  write(outid,*) 'nonfermion_loops  =', SwB
  write(outid,*) 'CT_on             =', CT_is_on
  write(outid,*) 'R2_on             =', R2_is_on
  write(outid,*) 'IR_on             =', IR_is_on
  write(outid,*) 'polecheck         =', polecheck_is
  write(outid,*) 'polenorm_swi      =', norm_swi
  write(outid,*) 'i-operator mode   =', ioperator_mode
  write(outid,*) 'last_switch       =', l_switch
  write(outid,*) 'ew_renorm_switch  =', ew_renorm_switch
  write(outid,*) 'use_coli_cache    =', coli_cache_use
  write(outid,*) 'use_me_cache      =', use_me_cache
  write(outid,*) 'check_Ward_tree   =', Ward_tree
  write(outid,*) 'check_Ward_loop   =', Ward_loop
  write(outid,*) 'out_symmetry      =', out_symmetry_on
  write(outid,*) 'stability_mode         =', stability_mode
  write(outid,*) 'deviation_mode         =', deviation_mode
  write(outid,*) 'stability_triggerratio =', trigeff_targ
  write(outid,*) 'stability_unstable     =', abscorr_unst
  write(outid,*) 'stability_kill         =', ratcorr_bad
  write(outid,*) 'stability_kill2        =', ratcorr_bad_L2
  write(outid,*) 'redlib1                =', a_switch
  write(outid,*) 'redlib2                =', a_switch_rescue
  write(outid,*) 'redlib_qp              =', redlib_qp
  write(outid,*)
  write(outid,*) '===================================================='
#endif
! opp_rootsvalue, opp_limitvalue, opp_thrs, opp_idig, opp_scaloop
! set_C_PV_threshold, set_D_PV_threshold, set_dd_red_mode

  if (outid == 10) then
    call ol_msg("Parameters written to file " // trim(filename))
    close(outid, iostat=ios)
    if (ios /= 0) then
      call ol_error("Error writing parameters to file " // trim(filename))
      call ol_msg("iostat =" // to_string(ios))
      return
    end if
  end if
end subroutine parameters_write

end module ol_parameters_init_/**/REALKIND
