
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


module ol_qcd_renormalisation_dp
  implicit none
  contains

! **********************************************************************
subroutine qcd_renormalisation
! **********************************************************************
! QCD renormalisation constants (in physical GeV units);
! conventions for UV/IR div. see subroutine loop_parameters_init.
! This subroutine is automatically called by loop_parameters_init.
! **********************************************************************
! dZMB  = bottom-quark mass RC       : MB_bare = MB*(1+dZMB)
! dZMT  = top-quark mass RC          : MT_bare = MT*(1+dZMT)
! dZg   = gluon-field RC             : A_bare  = (1+1/2*dZg)*A_ren
! dZq   = massless-quark field RC    : Q_bare  = (1+1/2*dZq)*Q_ren
! dZb   = bottom-quark field RC      : idem
! dZt   = top-quark field RC         : idem
! dgQCD = strong coupling RC         : g_bare  = (1+dgQCD)*g_ren
! **********************************************************************
  use kind_types, only: dp
  use ol_parameters_decl_dp
  use ol_loop_parameters_decl_dp





  implicit none

  real(dp)    :: deC_UV, deC_IR, deB_UV, deB_IR, deT_UV, deT_IR
  complex(dp) :: dummy_complex

  logical :: zeromasses(6) ! maximal possible number of quarks; only to determine N_lf

  zeromasses = [MU==0,MD==0,MS==0,MC==0,MB==0,MT==0]
  N_lf = count(zeromasses(:nf))

  if (N_lf /= 3 .and. N_lf /= 4 .and. N_lf /= 5) then
    write(*,*) '[OpenLoops] ERROR in qcd_renormalisation:'
    write(*,*) 'N_lf = ', N_lf, 'is not supported.'
    stop
  end if


  if (LeadingColour == 0) then
    cf = (nc*nc-1)/(2._dp*nc)
    ca = nc
    tf = 0.5_dp
  else
    cf = 0.5_dp*nc
    ca = nc
    tf = 0
  end if

  ! modified poles ((mu^2/MT^2)^eps)/eps
  if (MC /= 0) then
    deC_UV = de1_UV + real(log(mu2_UV/MC2))
    deC_IR = de1_IR + real(log(mu2_IR/MC2))
  end if
  if (MB /= 0) then
    deB_UV = de1_UV + real(log(mu2_UV/MB2))
    deB_IR = de1_IR + real(log(mu2_IR/MB2))
  end if
  deT_UV   = de1_UV + real(log(mu2_UV/MT2))
  deT_IR   = de1_IR + real(log(mu2_IR/MT2))

  dZq   = 0
  dZc   = 0
  dZb   = 0
  dZt   = 0
  dZMC  = 0
  dZMB  = 0
  dZMT  = 0
  dZg   = 0
  dgQCD = 0

  if (SwB /= 0) then
    ! non-fermionic
    ! On-shell renormalisation constant for gluon wave functions
    dZg   = (5*ca)/3 * (deT_UV - deT_IR)
    ! On-shell renormalisation constants for quark wave functions
    dZq   = -cf * (deT_UV - deT_IR)       ! massless quarks
    dZt   = -cf * (deT_UV + 2*deT_IR + 4) ! massive top-quark
    dZc   = dZq
    dZb   = dZq
    if (MC /= 0) then
      dZc  = -cf * (deC_UV + 2*deC_IR + 4) ! massive charm-quark
      dZMC = -cf * (4 + 3*(de1_UV+log(mu2_UV/MC2)))
    end if
    if (MB /= 0) then
      dZb  = -cf * (deB_UV + 2*deB_IR + 4) ! massive bottom-quark
      dZMB = -cf * (4 + 3*(de1_UV+log(mu2_UV/MB2)))
    end if
    ! On-shell top-mass renormalisation at complex pole p^2 = MT^2
    ! dMT = MT * (-cf * (4 + 3*(de1_UV+log(mu2_UV/MT2))) + dZt)
    dZMT  =       -cf * (4 + 3*(de1_UV+log(mu2_UV/MT2)))
    ! MS-bar renormalization constant for gQCD, YM-contribution
    dgQCD = -(11*ca)/6 * (de1_UV + log(mu2_UV/mureg2))
  end if
  if (SwF /= 0) then
    ! fermionic
    dZg   = dZg - (4*tf)/3 * N_lf * (deT_UV - deT_IR)
    ! MS-bar renormalization constant for gQCD; contribution for nf quarks
    dgQCD = dgQCD + (2*tf*nf)/3 * (de1_UV + log(mu2_UV/mureg2))
    if (nf > 3) then
      if (MC /= 0) then
        dZg = dZg - (4*tf)/3 * deC_UV
        if (nq_nondecoupl < 4 .or. mureg <= rMC) then
          dgQCD = dgQCD + (2*tf)*real(log(mureg2/MC2))/3
        end if
      end if
    end if
    if (nf > 4) then
      if (MB /= 0) then
        dZg = dZg - (4*tf)/3 * deB_UV
        if (nq_nondecoupl < 5 .or. mureg <= rMB) then
          dgQCD = dgQCD + (2*tf)*real(log(mureg2/MB2))/3
        end if
      end if
    end if
    if (nf > 5) then
      dZg = dZg - (4*tf)/3 * deT_UV
      ! top-quark decoupling term
      if (nq_nondecoupl < 6 .or. mureg <= rMT) then
        dgQCD = dgQCD + (2*tf)*real(log(mureg2/MT2))/3
      end if
    end if
  end if

!   ! MS-bar renormalisation constants
!   if (SwB /= 0) then
!     dZq  = -cf * de1_UV
!     dZb  = dZq
!     dZt  = dZq
!     dZg  = (5*ca)/3 * de1_UV
!     dZMB = -3*cf * de1_UV
!     dZMT = dZMB
!     dgQCD = -(11*ca)/6 * de1_UV
!   end if
!   if (SwF /= 0) then
!     ! only fermionic contribution
!     dgQCD = dgQCD + nf * (de1_UV/3)
!     dZg   = dZg - 2*nf * (de1_UV/3)
!   end if

  ! Sum of squared quark masses
  MQ2sum = MU2 + MD2 + MS2
  MQ2sum_pairs = MU2 + MD2
  nf_up = 1
  nf_down = 2
  if (nf > 3) then
    MQ2sum = MQ2sum + MC2
    MQ2sum_pairs = MQ2sum_pairs + MS2 + MC2
    nf_up = nf_up + 1
  end if
  if (nf > 4) then
    MQ2sum = MQ2sum + MB2
    nf_down = nf_down + 1
  end if
  if (nf > 5) then
    MQ2sum = MQ2sum + MT2
    MQ2sum_pairs = MQ2sum_pairs + MB2 + MT2
    nf_up = nf_up + 1
  end if

  ! set all counterterms to zero
  ctqq   = 0
  ctcc   = 0
  ctbb   = 0
  cttt   = 0
  ctGG   = 0
  ctGqq  = 0
  ctGcc  = 0
  ctGbb  = 0
  ctGtt  = 0
  ctVVV  = 0
  ctVVVV = 0
  ctVsc  = 0
  ctVbt  = 0
  ctVtt  = 0
  ctVcc  = 0
  ctVbb  = 0
  ctVqq  = 0
  ctScs  = 0
  ctSsc  = 0
  ctStb  = 0
  ctSbt  = 0
  ctSqq  = 0
  ctScc  = 0
  ctSbb  = 0
  ctStt  = 0
  ! set pure R2 terms to zero
  ctZGG  = 0
  ctHGG  = 0
  ctAAGG = 0
  ctAZGG = 0
  ctZZGG = 0
  ctWWGG = 0
  ctHHGG = 0
  ctHXGG = 0
  ctXXGG = 0
  ctPPGG = 0
  ctAGGG = [ 0, 0 ]
  ctZGGG = [ 0, 0 ]
  R2GGGG = 0

  if (CT_is_on /= 0) then
    ! Only UV counterterms
    dummy_complex = dZq
    ctqq   = [ dummy_complex, ZERO ]
    dummy_complex = dZc
    ctcc   = [ dummy_complex, MC * (dZMC + dZc) ]
    dummy_complex = dZb
    ctbb   = [ dummy_complex, MB * (dZMB + dZb) ]
    dummy_complex = dZt
    cttt   = [ dummy_complex, MT * (dZMT + dZt) ]
    dummy_complex = dZg
    ctGG   = [ dummy_complex, ZERO , ZERO ]
    ctGqq = (dgQCD + dZq + dZg/2)
    ctGcc = (dgQCD + dZc + dZg/2)
    ctGbb = (dgQCD + dZb + dZg/2)
    ctGtt = (dgQCD + dZt + dZg/2)
    ctVVV  = (dgQCD + 1.5_dp * dZg)
    ctVVVV = (2 * (dgQCD + dZg))
    ctVsc  = (dZc/2 + dZq/2)
    ctVbt  = (dZt/2 + dZb/2)
    ctVtt  = (dZt)
    ctVcc  = (dZc)
    ctVbb  = (dZb)
    ctVqq  = (dZq)
    ctScs  = [ -MS * (dZc/2 + dZq/2       ), MC * (dZc/2 + dZq/2 + dZMC) ]
    ctSsc  = [ -MC * (dZc/2 + dZq/2 + dZMC), MS * (dZc/2 + dZq/2       ) ]
    ctStb  = [ -MB * (dZt/2 + dZb/2 + dZMB), MT * (dZt/2 + dZb/2 + dZMT) ]
    ctSbt  = [ -MT * (dZt/2 + dZb/2 + dZMT), MB * (dZt/2 + dZb/2 + dZMB) ]
    ctSqq  = (dZq)
    ctScc  = (dZc + dZMC)
    ctSbb  = (dZb + dZMB)
    ctStt  = (dZt + dZMT)

  end if

  if (R2_is_on /= 0) then
    ! Add R2 contribution
    ! ctff = [ dZf - cf , mf * (dZmf + dZf - 2*cf) ]
    ! ctVV = [ dZV - (ca/3*(1/2+LHV) + 2*tf*nf/3) , - m2*(dZm2+dZV) + 4*tf*MQ2sum , LHV ]
    if (SwB /= 0) then
      ! non-fermionic
      dummy_complex = -cf
      ctqq   = ctqq + [ dummy_complex, ZERO ]
      ctcc   = ctcc + [ dummy_complex, -2*MC*cf ]
      ctbb   = ctbb + [ dummy_complex, -2*MB*cf ]
      cttt   = cttt + [ dummy_complex, -2*MT*cf ]
      dummy_complex = -0.5_dp*ca
      ctGG   = ctGG + [ dummy_complex, ZERO , cONE ]
      ctGqq = ctGqq - 2*cf
      ctGcc = ctGcc - 2*cf
      ctGbb = ctGbb - 2*cf
      ctGtt = ctGtt - 2*cf
      ctVVV  = ctVVV - (11*ca)/12
      ! ctVVVV = ctVVVV; handled in the Feynman rule
      ctVsc  = ctVsc - 2*cf
      ctVbt  = ctVbt - 2*cf
      ctVtt  = ctVtt - 2*cf
      ctVcc  = ctVcc - 2*cf
      ctVbb  = ctVbb - 2*cf
      ctVqq  = ctVqq - 2*cf
      ctScs  = ctScs - [4*cf,4*cf]
      ctSsc  = ctSsc - [4*cf,4*cf]
      ctStb  = ctStb - [4*cf,4*cf]
      ctSbt  = ctSbt - [4*cf,4*cf]
      ctSqq  = ctSqq - 4*cf
      ctScc  = ctScc - 4*cf
      ctSbb  = ctSbb - 4*cf
      ctStt  = ctStt - 4*cf
    end if
    if (SwF /= 0) then
      ! fermionic
      dummy_complex = -(2*tf*nf)/3
      ctGG   = ctGG + [ dummy_complex, 4*tf*MQ2sum , ZERO ]
      ctVVV  = ctVVV - (4*tf*nf)/3
      ! pure R2 terms
      ! ZGG R2 coupling: 4/3*sum_q(a_q)
      ctZGG = (nf_down-nf_up)*(-2*tf)/(3*cw*sw)
      ! HGG R2 coupling: 2*sum_q(m_q*v_q)
      ctHGG = -2*tf*MQ2sum/(sw*MW)
      ! VVGG R2 coupling: 2/3*sum(v1*v2+a1*a2)
      ctAAGG = (nf_up*4+nf_down)*(tf*4)/27
      ctAZGG = (nf_up*(-6+16*sw2)+nf_down*(-3+4*sw2)) * tf/(27*cw*sw)
      ctZZGG = (nf_up*(9-24*sw2+32*sw2**2)+nf_down*(9-12*sw2+ 8*sw2**2)) * tf/(54*cw2*sw2)
      ctWWGG = int(nf/2)*tf/(3*sw2) ! tf/(3*sw2) per SU(2) doublet
      ! SSGG R2 coupling: 2*sum(v1*v2-a1*a2)
      ctHHGG = tf*MQ2sum/(sw2*MW2)
      ctHXGG = 0
      ctXXGG = tf*MQ2sum/(sw2*MW2)
      ctPPGG = tf*MQ2sum_pairs/(sw2*MW2)
      ! VGGG R2 coupling: [ 4/3*tf*sum_q(v_q), -12*tf*CI*sum_q(a_q) ]
      dummy_complex = -(4*tf)/9*(2*nf_up-nf_down)
      ctAGGG = [ dummy_complex     , ZERO ]
      ctZGGG = [ (nf_up*(3-8*sw2)+nf_down*(-3+4*sw2)) * tf/(9*cw*sw) , (nf_up-nf_down)*3*tf*CI/(sw*cw) ]
      R2GGGG = int(2*tf) ! switch on the 4-gluon R2
    end if

  end if

end subroutine qcd_renormalisation

end module ol_qcd_renormalisation_dp

