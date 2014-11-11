
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


module ol_data_types_qp
  use kind_types, only: qp, intkind1, intkind2
  type wfun
    ! four complex components for the wave function
     complex(qp) :: j(4)
    ! indicator if left- or right components of of-sheel line vanish
    !                      j= (0,0,0,0) (0,0,j3,j4) (j1,j2,0,0) (j1,j2,j3,j4)
    integer(intkind1) :: h !    B"00"      B"01"       B"10"        B"11"
    integer(intkind2) :: e ! helicities of external on-shell lines
  end type wfun

  type polcont
    complex(qp) :: j
    integer(intkind2) :: e ! helicities of external on-shell lines
    integer(intkind2) :: s ! table for final helicity syncronisation
  end type polcont







end module ol_data_types_qp


module ol_momenta_decl_qp
  use kind_types, only: qp, MaxParticles
  implicit none
  ! Internal momenta for up to 'MaxParticles' external particles
  ! Components 1:4 = light cone representation; component 5 = squared momentum
  complex(qp), save :: Q(5,0:2**MaxParticles-1) = 0 ! 2^Nmax - 1 = 511
  complex(qp), allocatable, save :: QInvariantsMatrix(:,:)
  contains

  function momenta_nan_check(P)
    implicit none
    real(qp), intent(in) :: P(:,:)
    integer :: momenta_nan_check
    integer :: i
    if (all(P == P)) then
      ! ok
      momenta_nan_check = 0
    else
      ! contains NaN
      write(*,*) "WARNING: corrupted phase space point"
      do i = 1, size(P,2)
        write(*,*) P(:,i)
      end do
      momenta_nan_check = 1
    end if
  end function momenta_nan_check

end module ol_momenta_decl_qp



module ol_external_decl_qp
  use kind_types, only: qp, MaxParticles
  implicit none
  ! phase space point cache; used to print the ps-point if tensor reduction fails
  integer,        save :: nParticles = 0 ! set by conv_mom_scatt2in
  real(qp), save :: M_ex(MaxParticles) ! unused
  real(qp), save :: P_ex(0:3,MaxParticles) ! uncleaned external 2->n-2 momenta, set by conv_mom_scatt2in
  integer,        save :: crossing(MaxParticles) = 0 ! only used if a reduction error occurs
  integer,        save :: inverse_crossing(MaxParticles) = 0 ! set by conv_mom_scatt2in
  integer,  save :: gf_array(MaxParticles) = 0
  ! lists for each external particle the external momentum
  ! used for the gauge fixing of the vector polarization. Used in subroutine wf_gf_V.
  ! A zero entry means that it's not a massless vector particle.
  integer,  save :: Ward_array(MaxParticles) = 0 ! select particle "i" for the Ward identity -> Ward_array(i) = 1
end module ol_external_decl_qp



module ol_pseudotree_qp
  use kind_types, only: qp
  implicit none
  ! loop momentum in pseudo tree (standard representation)
  real(qp), save :: pseudotree_momentum(0:3) = [ 314.1592653589793_qp, 271.8281828459045_qp, 100._qp, 57.72156649015328_qp ]
  ! Wave functions for pseudo tree
  complex(qp), save :: exloop(4,2) = reshape([ 2.718_qp, 3.141_qp,  0.9159_qp, 1._qp,  &
                                               1._qp,    0.5772_qp, 1.618_qp,  1.282_qp], [ 4, 2 ])
end module ol_pseudotree_qp



module ol_parameters_decl_qp
  ! Declarations and initial values for numerical and physical parameters like masses, widths, and couplings.
  ! Loading the module for the first time initialises the parameters with their default values (given in this module).
  ! Note that when a value has been changed using parameters_init(val=...) loading the module
  ! will not reset the value to its default.
  use kind_types, only: qp
  use ol_version, only: splash_todo
!   use TI_call_interface
  implicit none
  ! Counted up by 1 each time parameters_init() is called
  integer, save :: parameters_status = 0
  integer, save :: parameters_verbose = 0
  integer, save :: verbose = 0


  ! Numerical constants
  real(qp),    parameter :: rONE   = 1
  real(qp),    parameter :: rZERO  = 0
  real(qp),    parameter :: rZERO2 = 0
  real(qp),    parameter :: pi     = acos(-1._qp)
  real(qp),    parameter :: pi2_6  = (pi**2)/6
  real(qp),    parameter :: sqrt2  = sqrt(2._qp)
  real(qp),    parameter :: sqrt05 = sqrt(0.5_qp)
  complex(qp), parameter :: cONE   = 1
  complex(qp), parameter :: ZERO   = 0
  complex(qp), parameter :: ZERO2  = 0
  complex(qp), parameter :: CI     = (0._qp, 1._qp)
  complex(qp) :: integralnorm = CI/(16*pi**2)
  complex(qp) :: countertermnorm = 1._qp/(16._qp*pi**2)

  ! scale factor for dimensionful parameters
  real(qp), save :: scalefactor = 1
  logical,        save :: reset_scalefactor = .false.
  integer,        save :: scaling_mode = 1 ! 1: reduction only, 3: everything
  ! Particle masses and widths
  real(qp), save :: rME_unscaled = 0,                    wME_unscaled = 0 ! electron mass and width
  real(qp), save :: rMM_unscaled = 0,                    wMM_unscaled = 0 ! muon mass and width
  real(qp), save :: rML_unscaled = 0,                    wML_unscaled = 0 ! tau mass and width
  real(qp), save :: rMU_unscaled = 0,                    wMU_unscaled = 0 ! up-quark mass and width
  real(qp), save :: rMD_unscaled = 0,                    wMD_unscaled = 0 ! down-quark mass and width
  real(qp), save :: rMS_unscaled = 0,                    wMS_unscaled = 0 ! strange-quark mass and width
  real(qp), save :: rMC_unscaled = 0,                    wMC_unscaled = 0 ! charm-quark mass and width
  real(qp), save :: rMB_unscaled = 0._qp,      wMB_unscaled = 0 ! bottom-quark mass and width
  real(qp), save :: rMT_unscaled = 172._qp,    wMT_unscaled = 0 ! top-quark mass and width
  real(qp), save :: rMW_unscaled = 80.399_qp,  wMW_unscaled = 0 ! W boson mass LEP PDG 2008/2009 and width
  real(qp), save :: rMZ_unscaled = 91.1876_qp, wMZ_unscaled = 0 ! Z boson mass LEP PDG 2008/2009 and width
  real(qp), save :: rMX_unscaled = 0._qp,  wMX_unscaled = 0._qp ! auxiliary field for Z
  real(qp), save :: rMY_unscaled = 0._qp,  wMY_unscaled = 0.0000_qp ! auxiliary field for W
  real(qp), save :: rMH_unscaled = 125._qp,    wMH_unscaled = 0 ! higgs boson mass and width
  real(qp), save :: MREG_unscaled = 1._qp                       ! collinear mass regulator for photon WF CT
  ! Coupling constants
  real(qp), save :: alpha_QCD = 0.1258086856923967_qp ! LO MRST
  real(qp), save :: alpha_QED = 1/128._qp
  ! Everything beyond this line is derived from the values given above and initialised by parameters_init().
  real(qp), save :: rescalefactor = 1.1
  ! scaled masses and widths
  real(qp), save :: rME, wME
  real(qp), save :: rMM, wMM
  real(qp), save :: rML, wML
  real(qp), save :: rMU, wMU
  real(qp), save :: rMD, wMD
  real(qp), save :: rMS, wMS
  real(qp), save :: rMC, wMC
  real(qp), save :: rMB, wMB
  real(qp), save :: rMT, wMT
  real(qp), save :: rMW, wMW
  real(qp), save :: rMZ, wMZ
  real(qp), save :: rMH, wMH
  real(qp), save :: rMX, wMX
  real(qp), save :: rMY, wMY
  ! Complex masses, complex and real squared masses
  complex(qp), save ::  ME,   MM,   ML,   MU,   MD,   MS,   MC,   MB,   MT,   MW,   MZ,   MH,  MX,    MY
  complex(qp), save ::  ME2,  MM2,  ML2,  MU2,  MD2,  MS2,  MC2,  MB2,  MT2,  MW2,  MZ2,  MH2, MX2,   MY2
  real(qp),    save :: rME2, rMM2, rML2, rMU2, rMD2, rMS2, rMC2, rMB2, rMT2, rMW2, rMZ2, rMH2, rMX2, rMY2
  ! collinear mass regulator for photon WF CT
  real(qp),    save :: MREG
  ! Coupling constants
  complex(qp), save :: eQED, E2_QED, gQCD, G2_QCD
  ! Weak mixing angle
  complex(qp), save :: cw, cw2, cw3, cw4, sw, sw2, sw3 ,sw4, sw6
  ! Right/left couplings of a Z boson to neutrinos, leptons, up- and down-type quarks
  complex(qp), save :: gZn(2), gZl(2), gZu(2), gZd(2)
  ! Right/left coupling for Higgs(H), Chi(X) = Z-Goldstone, Phi(P) = W-Goldstone
  complex(qp), save :: gH(2), gX(2), gPud(2), gPcs(2), gPtb(2), gPdu(2), gPsc(2), gPbt(2), gPnl(2), gPln(2)
  complex(qp) :: gZRH, gZLH
  ! Vertex scale factors for naive deviations from the Standard Model (changes don't affect CT/R2)
  real(qp), save :: lambdaHHH = 1, lambdaHWW = 1, lambdaHZZ = 1

end module ol_parameters_decl_qp



! **********************************************************************
module ol_loop_parameters_decl_qp
! Declarations and initial values for renormalisation constants and parameters of
! dimensional regularisation, dipole subtraction, tensor-integral libraries.
! Loading the module for the first time initialises the parameters with
! their default values (given in this module). Note that when a value has
! been changed using loop_parameters_init(val=...) loading the module will not
! reset the value to its default.
! **********************************************************************
  use kind_types, only: qp
  use ol_parameters_decl_qp

  use avh_olo_version, only: olo_splash_done => done


  use countdigits, only: cts_splash_todo

  implicit none
  integer,        save :: loop_parameters_status = 0



  real(qp), save      :: ca    = 3          ! adjoint Casimir
  real(qp), save      :: cf    = 4._qp/3 ! fundamental Casimir
  real(qp), save      :: tf    = 0.5_qp  ! generator normalisation

  real(qp), save      :: polescale   = 1 ! used as pole values in VAMP2chk to determine the true poles
  real(qp), save      :: de1_UV      = 0 ! numerical value of single UV pole (independent of norm-convention)
  real(qp), save      :: de1_IR      = 0 ! numerical value of single IR pole (independent of norm-convention)
  real(qp), save      :: de2_i_IR    = 0 ! numerical value of double IR pole using actual norm-convention
  real(qp), save      :: de2_i_shift = 0 ! double pole shift defining actual norm convention
  real(qp), save      :: mureg_unscaled = 100    ! renormalisation scale
  real(qp), save      :: mureg
  real(qp), save      :: x_UV  = 1       ! rescaling factor for dim-reg scale in UV-divergent quantities
  real(qp), save      :: x_IR  = 1       ! rescaling factor for dim-reg scale in IR-divergent quantities
  real(qp), parameter :: kappa = 2/3._qp ! kappa parameter used in dipole subtraction

  ! the following derived parameters are initilised by subroutine loop_parameters_init
  real(qp), save :: de2_0_IR  ! numerical value of double IR pole using LH-accord convention (i=0)
  real(qp), save :: de2_1_IR  ! numerical value of double IR pole using COLI convention (i=1)
  real(qp), save :: mureg2    ! squared renormalisation scale
  real(qp), save :: mu2_UV    ! dim-reg scale for UV-divergent quantities
  real(qp), save :: mu2_IR    ! dim-reg scale for IR-divergent quantities

  ! the following renormalisation constants are initilised by subroutine QCD_renormalisation
  complex(qp), save :: dZMC     = 0 ! charm-quark mass RC        : MC_bare = MC*(1+dZMC)
  complex(qp), save :: dZMB     = 0 ! bottom-quark mass RC       : MB_bare = MB*(1+dZMB)
  complex(qp), save :: dZMT     = 0 ! top-quark mass RC          : MT_bare = MT*(1+dZMT)
  real(qp),    save :: dZg      = 0 ! gluon-field RC             : G_bare  = (1+dZg/2)*G_ren
  real(qp),    save :: dZq      = 0 ! massless-quark field RC    : Q_bare  = (1+dZq/2)*Q_ren
  real(qp),    save :: dZc      = 0 ! charm-quark field RC       : idem
  real(qp),    save :: dZb      = 0 ! bottom-quark field RC      : idem
  real(qp),    save :: dZt      = 0 ! top-quark field RC         : idem
  real(qp),    save :: dgQCD    = 0 ! strong coupling RC         : g_bare  = (1+dgQCD)*g_ren
  real(qp),    save :: dgQCDym  = 0 ! YM-contribution to delnG
  real(qp),    save :: dgQCDfer = 0 ! fermionic-contribution to delnG

  ! Counter terms for QCD corrections
  complex(qp), save :: ctqq(2) ! massless quark propagator counter term
  complex(qp), save :: ctcc(2) ! charm quark propagator counter term
  complex(qp), save :: ctbb(2) ! bottom quark propagator counter term
  complex(qp), save :: cttt(2) ! top quark propagator counter term
  complex(qp), save :: ctGG(3) ! gluon propagator counter term
  real(qp),    save :: ctGqq   ! massless quark-gluon vertex counter term
  real(qp),    save :: ctGcc   ! charm quark-gluon vertex counter term (massive or massless c)
  real(qp),    save :: ctGbb   ! bottom quark-gluon vertex counter term (massive or massless b)
  real(qp),    save :: ctGtt   ! top quark-gluon vertex counter term
  real(qp),    save :: ctVVV   ! three gluon vertex counter term
  real(qp),    save :: ctVVVV  ! four gluon vertex counter term (times 1/2)
  real(qp),    save :: ctVsc   ! Wcs (massive or massless c) vertex counter term
  real(qp),    save :: ctVbt   ! Wtb (massive or massless b) vertex counter term
  real(qp),    save :: ctVtt   ! Att and Ztt vertex counter term
  real(qp),    save :: ctVcc   ! Acc and Zcc vertex counter term (massive or massless c)
  real(qp),    save :: ctVbb   ! Abb and Zbb vertex counter term (massive or massless b)
  real(qp),    save :: ctVqq   ! Aqq and Zqq (massless q) vertex counter term
  complex(qp), save :: ctScs(2)
  complex(qp), save :: ctSsc(2)
  complex(qp), save :: ctStb(2)
  complex(qp), save :: ctSbt(2)
  complex(qp), save :: ctSqq
  complex(qp), save :: ctScc
  complex(qp), save :: ctSbb
  complex(qp), save :: ctStt

  ! Additional parameters for R2
  complex(qp), save :: MQ2sum, MQ2sum_pairs

  ! Additional counterterms for R2 QCD
  complex(qp), save :: ctZGG
  complex(qp), save :: ctHGG
  complex(qp), save :: ctAAGG
  complex(qp), save :: ctAZGG
  complex(qp), save :: ctZZGG
  complex(qp), save :: ctWWGG
  complex(qp), save :: ctHHGG
  complex(qp), save :: ctHXGG
  complex(qp), save :: ctXXGG
  complex(qp), save :: ctPPGG
  complex(qp), save :: ctAGGG(2)
  complex(qp), save :: ctZGGG(2)
  integer,           save :: R2GGGG

  ! EW_renormalisation renormalisation constants
  complex(qp), save :: dZMBEW     = 0 ! bottom-quark mass RC       : MB_bare = MB+dZMBEW)
  complex(qp), save :: dZMTEW     = 0 ! top-quark mass RC          : MT_bare = MT+dZMTEW)
  complex(qp), save :: dZMLEW     = 0 ! tau-lepton mass RC         : ML_bare = ML+dZMLEW)
  complex(qp), save :: dZMW2EW    = 0 ! W mass RC                  : MW^2_bare = MW^2+dZMW2EW^2
  complex(qp), save :: dZMZ2EW    = 0 ! Z mass RC                  : MZ^2_bare = MZ^2+dZMZ2EW^2
  complex(qp), save :: dZMH2EW    = 0 ! H mass RC                  : MH^2_bare = MH^2+dZMH2EW^2
  complex(qp), save :: dswEW      = 0 ! sin EW mixing angle RC         : sw_bare = sw + dswEW  i.e. dswEW/swEW = - c^2/s^2 dcwEW/c ; dcEW/c=1/2(dZMW2/MW^2-dZMZ2/MZ^2)
  complex(qp), save :: dcwEW      = 0 ! cos EW mixing angle RC         : dcwEW/c=1/2(dZMW2/MW^2-dZMZ2/MZ^2) defined for convinience

!   complex(rp), save      :: dZqLEW     = 0 ! L-massless-quark field RC : Q_bare  = (1+1/2*dZqLEW)*Q_ren
!   complex(rp), save      :: dZqREW     = 0 ! R-massless-quark field RC : Q_bare  = (1+1/2*dZqREW)*Q_ren
  complex(qp), save :: dZuLEW     = 0 ! L-massless-u-quark field RC : Q_bare  = (1+1/2*dZuLEW)*Q_ren
  complex(qp), save :: dZuREW     = 0 ! R-massless-u-quark field RC : Q_bare  = (1+1/2*dZuREW)*Q_ren
  complex(qp), save :: dZdLEW     = 0 ! L-massless-d-quark field RC : Q_bare  = (1+1/2*dZdLEW)*Q_ren
  complex(qp), save :: dZdREW     = 0 ! R-massless-d-quark field RC : Q_bare  = (1+1/2*dZdREW)*Q_ren
  complex(qp), save :: dZbLEW     = 0 ! L-bottom-quark field RC     : idem
  complex(qp), save :: dZbREW     = 0 ! R-bottom-quark field RC     : idem
  complex(qp), save :: dZtLEW     = 0 ! L-top-quark field RC        : idem
  complex(qp), save :: dZtREW     = 0 ! R-top-quark field RC        : idem
  complex(qp), save :: dZeLEW     = 0 ! L-lepton field RC           : idem
  complex(qp), save :: dZeREW     = 0 ! R-lepton field RC           : idem
  complex(qp), save :: dZnLEW     = 0 ! L-neutrino field RC         : idem
  complex(qp), save :: dZLLEW     = 0 ! L-tau-lepton field RC       : idem
  complex(qp), save :: dZLREW     = 0 ! R-tau-lepton field RC       : idem
  complex(qp), save :: dZuLEWcc   = 0 ! L-massless-u-quark field RC : Q_bare  = (1+1/2*dZuLEW)*Q_ren
  complex(qp), save :: dZuREWcc   = 0 ! R-massless-u-quark field RC : Q_bare  = (1+1/2*dZuREW)*Q_ren
  complex(qp), save :: dZdLEWcc   = 0 ! L-massless-d-quark field RC : Q_bare  = (1+1/2*dZdLEW)*Q_ren
  complex(qp), save :: dZdREWcc   = 0 ! R-massless-d-quark field RC : Q_bare  = (1+1/2*dZdREW)*Q_ren
  complex(qp), save :: dZbLEWcc   = 0 ! L-bottom-quark field RC     : idem
  complex(qp), save :: dZbREWcc   = 0 ! R-bottom-quark field RC     : idem
  complex(qp), save :: dZtLEWcc   = 0 ! L-top-quark field RC        : idem
  complex(qp), save :: dZtREWcc   = 0 ! R-top-quark field RC        : idem
  complex(qp), save :: dZeLEWcc   = 0 ! L-lepton field RC           : idem
  complex(qp), save :: dZeREWcc   = 0 ! R-lepton field RC           : idem
  complex(qp), save :: dZnLEWcc   = 0 ! L-neutrino field RC         : idem
  complex(qp), save :: dZLLEWcc   = 0 ! L-tau-lepton field RC       : idem
  complex(qp), save :: dZLREWcc    = 0 ! R-tau-lepton field RC       : idem

  complex(qp), save :: dZWEW      = 0 ! W field RC                 : idem
  complex(qp), save :: dZZZEW     = 0 ! ZZ field RC                : idem
  complex(qp), save :: dZAZEW     = 0 ! AZ field RC                : idem
  complex(qp), save :: dZZAEW     = 0 ! AZ field RC                : idem
  complex(qp), save :: dZAAEW     = 0  ! AA field RC                : idem
  complex(qp), save :: dZHEW      = 0 ! H field RC                 : idem

  complex(qp), save :: dtEW       = 0 ! tadpole-RC                 :
  complex(qp), save :: dZeQEDEW   = 0 ! EW coupling RC         : e_bare  = (1+dZeEW)*e_ren

  ! Counter terms for EW corrections
  ! VV Vector propagators
  complex(qp), save :: EWctWW(3)
  complex(qp), save :: EWctZZ(3)
  complex(qp), save :: EWctAZ(3)
  complex(qp), save :: EWctAA(3)
  ! SS scalar propagators
  complex(qp), save :: EWctHH(2)
  complex(qp), save :: EWctXX(2)
  complex(qp), save :: EWctPP(2)
  ! SV scalar-vector mixing
  complex(qp), save :: EWctXA
  complex(qp), save :: EWctXZ
  complex(qp), save :: EWctPW
  ! FF fermionic propagators
  complex(qp), save :: EWctuu(4)
  complex(qp), save :: EWctdd(4)
  complex(qp), save :: EWcttt(4)
  complex(qp), save :: EWctbb(4)
  complex(qp), save :: EWctee(4)
  complex(qp), save :: EWctLL(4)
  complex(qp), save :: EWctnn(4)
  !VVVV
  complex(qp), save :: EWctWWWW(2)
  complex(qp), save :: EWctWWZZ(2)
  complex(qp), save :: EWctWWAZ(2)
  complex(qp), save :: EWctWWAA(2)
  !VVVV pure R2
  complex(qp), save :: EWctR2AAAA
  complex(qp), save :: EWctR2AAAZ
  complex(qp), save :: EWctR2AAZZ
  complex(qp), save :: EWctR2AZZZ
  complex(qp), save :: EWctR2ZZZZ
  !VVV
  complex(qp), save :: EWctAWW
  complex(qp), save :: EWctZWW
  !SSSS
  complex(qp), save :: EWctSSSS1
  complex(qp), save :: EWctSSSS2
  complex(qp), save :: EWctSSSS3
  complex(qp), save :: EWctHHHH
  complex(qp), save :: EWctHHXX
  complex(qp), save :: EWctHHPP
  complex(qp), save :: EWctXXXX
  complex(qp), save :: EWctXXPP
  complex(qp), save :: EWctPPPP
  !SSS
  complex(qp), save :: EWctHHH
  complex(qp), save :: EWctHXX
  complex(qp), save :: EWctHPP
  !VVSS
  complex(qp), save :: EWctWWXX
  complex(qp), save :: EWctWWHH
  complex(qp), save :: EWctWWPP
  complex(qp), save :: EWctZZPP
  complex(qp), save :: EWctZAPP
  complex(qp), save :: EWctAAPP
  complex(qp), save :: EWctZZHH
  complex(qp), save :: EWctZZXX
  complex(qp), save :: EWctZAHH
  complex(qp), save :: EWctWZPH
  complex(qp), save :: EWctWAPH
  complex(qp), save :: EWctWZPX
  complex(qp), save :: EWctWAPX
  !VVSS R2
  complex(qp), save :: EWctAAHH
  complex(qp), save :: EWctAAXX
  !VSS
  complex(qp), save :: EWctAXH
  complex(qp), save :: EWctZXH
  complex(qp), save :: EWctAPP
  complex(qp), save :: EWctZPP
  complex(qp), save :: EWctWPH
  complex(qp), save :: EWctWPX
  !SVV
  complex(qp), save :: EWctHWW
  complex(qp), save :: EWctHZZ
  complex(qp), save :: EWctHZA
  complex(qp), save :: EWctPWZ
  complex(qp), save :: EWctPWA
  ! pure R2 SVV
  complex(qp), save :: EWctHAA
  !VFF
  ! Aff
  complex(qp), save :: EWctAuu(2)
  complex(qp), save :: EWctAdd(2)
  complex(qp), save :: EWctAtt(2)
  complex(qp), save :: EWctAbb(2)
  complex(qp), save :: EWctAee(2)
  complex(qp), save :: EWctALL(2)
  complex(qp), save :: EWctAnn(2)
  ! Zff
  complex(qp), save :: dgZu(2)
  complex(qp), save :: dgZd(2)
  complex(qp), save :: dgZl(2)
  complex(qp), save :: dgZn(2)
  complex(qp), save :: EWctVuu(2)
  complex(qp), save :: EWctVdd(2)
  complex(qp), save :: EWctVtt(2)
  complex(qp), save :: EWctVbb(2)
  complex(qp), save :: EWctVee(2)
  complex(qp), save :: EWctVLL(2)
  complex(qp), save :: EWctVnn(2)
  ! Wff
  complex(qp), save :: EWctVdu
  complex(qp), save :: EWctVbt
  complex(qp), save :: EWctVen
  complex(qp), save :: EWctVLn
  complex(qp), save :: EWctVud
  complex(qp), save :: EWctVtb
  complex(qp), save :: EWctVne
  complex(qp), save :: EWctVnL
  ! Gff mixed EW/QCD
  complex(qp), save ::  EWctGuu(2)
  complex(qp), save ::  EWctGdd(2)
  complex(qp), save ::  EWctGtt(2)
  complex(qp), save ::  EWctGbb(2)
  !SFF
  complex(qp), save :: EWctHtt(2)
  complex(qp), save :: EWctHbb(2)
  complex(qp), save :: EWctHLL(2)
  complex(qp), save :: EWctXtt(2)
  complex(qp), save :: EWctXbb(2)
  complex(qp), save :: EWctXLL(2)
  complex(qp), save :: EWctPtb(2)
  complex(qp), save :: EWctPbt(2)
  complex(qp), save :: EWctPnL(2)
  complex(qp), save :: EWctPLn(2)
  ! VUU
  complex(qp), save :: EWctAUWUW
  complex(qp), save :: EWctZUWUW
  complex(qp), save :: EWctWUWUZ
  complex(qp), save :: EWctWUZUW
  complex(qp), save :: EWctWUWUA
  complex(qp), save :: EWctWUAUW
  ! SUU
  complex(qp), save :: EWctHUZUZ
  complex(qp), save :: EWctHUWUW
  complex(qp), save :: EWctXUWUW
  complex(qp), save :: EWctPUZUW
  complex(qp), save :: EWctPUWUZ
  complex(qp), save :: EWctPUWUA

  ! Additional parameters for R2 EW
  complex(qp), save :: sumMQ2
  complex(qp), save :: sumMQ2Q2
  complex(qp), save :: sumMQ2QI
  complex(qp), save :: sumMQ4
  complex(qp), save :: sumMUD
  complex(qp), save :: sumMUD2
  complex(qp), save :: sumMU2
  complex(qp), save :: sumMD2
  complex(qp), save :: sumMU4
  complex(qp), save :: sumMD4
  complex(qp), save :: sumMQ2QUD

end module ol_loop_parameters_decl_qp

