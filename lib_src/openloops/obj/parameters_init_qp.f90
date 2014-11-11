
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


module ol_parameters_init_qp
  implicit none
  contains

subroutine masspowers(rM, Ga, M, M2, rM2)
  use kind_types, only: qp
  use ol_parameters_decl_qp, only: CI
  use ol_parameters_decl_dp, only: cms_on
  implicit none
  real(qp),    intent(in)  :: rM, Ga
  complex(qp), intent(out) :: M,  M2
  real(qp),    intent(out) :: rM2
  M2  = rM*rM - CI*rM*Ga
  if ( cms_on == 0 ) then
    M   = rM
    rM2 = rM*rM
  else
    M  = sqrt(M2)
    rM2 = real(M2)
  end if
end subroutine masspowers





subroutine parameters_init()
  ! non-dp initialisation: synchronise with dp parameters
  use kind_types, only: qp
  use ol_parameters_decl_qp
  use ol_parameters_decl_dp, only: scalefactor_dp => scalefactor, cms_on => cms_on, &
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
  gZn  = [    ZERO   , gZLH*( 0.5_qp            ) ] ! neutrino
  gZl  = [   -gZRH   , gZLH*(-0.5_qp +    sw2   ) ] ! lepton
  gZu  = [ (2*gZRH)/3, gZLH*( 0.5_qp - (2*sw2)/3) ] ! up
  gZd  = [   -gZRH /3, gZLH*(-0.5_qp +    sw2 /3) ] ! down
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



  parameters_status = parameters_status_dp


  ! write parameters
  if (parameters_verbose == 1 ) then
    call parameters_write
  end if

end subroutine parameters_init



subroutine ensure_mp_init()
  ! synchronise non-dp parameters with dp if they are not up to date
  ! should be called after parameters_init()
  ! and in tree matrix element routines before anything is done

  use ol_parameters_decl_qp, only: parameters_status
  use ol_parameters_decl_dp, only: &
    & parameters_status_dp => parameters_status
  implicit none
  if (parameters_status_dp /= parameters_status) call parameters_init()

end subroutine ensure_mp_init













subroutine loop_parameters_init
  ! non-dp initialisation: synchronise with dp parameters
  use ol_parameters_decl_qp, only: pi2_6
  use ol_loop_parameters_decl_qp
  use ol_loop_parameters_decl_dp, only: &
    & loop_parameters_status_dp => loop_parameters_status, norm_swi, a_switch, a_switch_rescue, redlib_qp, &
    & dd_qp_not_init, tensorlib_qp_not_init, renscale_dp => mureg, fact_UV_dp => x_UV, fact_IR_dp => x_IR, &
    & pole1_UV_dp => de1_UV, pole1_IR_dp => de1_IR, pole2_IR_dp => de2_i_IR, do_ew_renorm



  use ol_qcd_renormalisation_qp, only: qcd_renormalisation
!   use ol_ew_renormalisation_qp, only: ew_renormalisation
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


  de2_0_IR = de2_i_IR - de2_i_shift         ! LH-norm double pole
  de2_1_IR = de2_i_IR - de2_i_shift + pi2_6 ! COLI-norm double pole
  ! renormalisation scale
  mureg2 = mureg**2
  ! dim reg scale in UV-div loops
  mu2_UV = (x_UV**2)*mureg2
  mu2_IR = (x_IR**2)*mureg2



  ! DD qp initialisation
!  if (a_switch == 7 .or. a_switch_rescue == 7) then
  if ( redlib_qp == 7 ) then

  end if

! ifdef PRECISION_dp


  call qcd_renormalisation
!   if (ew_renorm_switch /= 0) then
!     call ew_renormalisation
!   end if

  ! Increment number of time this function has been called:



  loop_parameters_status = loop_parameters_status_dp


end subroutine loop_parameters_init



subroutine ensure_mp_loop_init()
  ! synchronise non-dp parameters with dp if they are not up to date
  ! should be called after loop_parameters_init()
  ! and in loop matrix element routines before anything is done

  use ol_parameters_decl_qp, only: parameters_status
  use ol_loop_parameters_decl_qp, only: loop_parameters_status
  use ol_parameters_decl_dp, only: &
    & parameters_status_dp => parameters_status
  use ol_loop_parameters_decl_dp, only: &
    & loop_parameters_status_dp => loop_parameters_status
  implicit none
  if (parameters_status_dp /= parameters_status) &
    & call parameters_init()
  if (loop_parameters_status_dp /= loop_parameters_status) &
    & call loop_parameters_init()

end subroutine ensure_mp_loop_init






subroutine parameters_write()

  use kind_types, only: qp
  use ol_parameters_decl_qp
  use ol_loop_parameters_decl_qp
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

! opp_rootsvalue, opp_limitvalue, opp_thrs, opp_idig, opp_scaloop
! sam_isca, sam_verbosity, sam_itest
! set_C_PV_threshold, set_D_PV_threshold, set_dd_red_mode
end subroutine parameters_write

end module ol_parameters_init_qp

