
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


module ol_data_types_dp
  use kind_types, only: dp, intkind1, intkind2
  type wfun
    ! four complex components for the wave function
     complex(dp) :: j(4)
    ! indicator if left- or right components of of-sheel line vanish
    !                      j= (0,0,0,0) (0,0,j3,j4) (j1,j2,0,0) (j1,j2,j3,j4)
    integer(intkind1) :: h !    B"00"      B"01"       B"10"        B"11"
    integer(intkind2) :: e ! helicities of external on-shell lines
  end type wfun

  type polcont
    complex(dp) :: j
    integer(intkind2) :: e ! helicities of external on-shell lines
    integer(intkind2) :: s ! table for final helicity syncronisation
  end type polcont


  type me_cache
    real(dp), allocatable :: psp(:,:), me(:)
  end type me_cache


end module ol_data_types_dp


module ol_momenta_decl_dp
  use kind_types, only: dp, MaxParticles
  implicit none
  ! Internal momenta for up to 'MaxParticles' external particles
  ! Components 1:4 = light cone representation; component 5 = squared momentum
  complex(dp), save :: Q(5,0:2**MaxParticles-1) = 0 ! 2^Nmax - 1 = 511
  complex(dp), allocatable, save :: QInvariantsMatrix(:,:)
  contains

  function momenta_nan_check(P)
    implicit none
    real(dp), intent(in) :: P(:,:)
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

end module ol_momenta_decl_dp



module ol_external_decl_dp
  use kind_types, only: dp, MaxParticles
  implicit none
  ! phase space point cache; used to print the ps-point if tensor reduction fails
  integer,        save :: nParticles = 0 ! set by conv_mom_scatt2in
  real(dp), save :: M_ex(MaxParticles) ! unused
  real(dp), save :: P_ex(0:3,MaxParticles) ! uncleaned external 2->n-2 momenta, set by conv_mom_scatt2in
  integer,        save :: crossing(MaxParticles) = 0 ! only used if a reduction error occurs
  integer,        save :: inverse_crossing(MaxParticles) = 0 ! set by conv_mom_scatt2in
  integer,  save :: gf_array(MaxParticles) = 0
  ! lists for each external particle the external momentum
  ! used for the gauge fixing of the vector polarization. Used in subroutine wf_gf_V.
  ! A zero entry means that it's not a massless vector particle.
  integer,  save :: Ward_array(MaxParticles) = 0 ! select particle "i" for the Ward identity -> Ward_array(i) = 1
end module ol_external_decl_dp



module ol_pseudotree_dp
  use kind_types, only: dp
  implicit none
  ! loop momentum in pseudo tree (standard representation)
  real(dp), save :: pseudotree_momentum(0:3) = [ 314.1592653589793_dp, 271.8281828459045_dp, 100._dp, 57.72156649015328_dp ]
  ! Wave functions for pseudo tree
  complex(dp), save :: exloop(4,2) = reshape([ 2.718_dp, 3.141_dp,  0.9159_dp, 1._dp,  &
                                               1._dp,    0.5772_dp, 1.618_dp,  1.282_dp], [ 4, 2 ])
end module ol_pseudotree_dp



module ol_parameters_decl_dp
  ! Declarations and initial values for numerical and physical parameters like masses, widths, and couplings.
  ! Loading the module for the first time initialises the parameters with their default values (given in this module).
  ! Note that when a value has been changed using parameters_init(val=...) loading the module
  ! will not reset the value to its default.
  use kind_types, only: dp
  use ol_version, only: splash_todo
!   use TI_call_interface
  implicit none
  ! Counted up by 1 each time parameters_init() is called
  integer, save :: parameters_status = 0
  integer, save :: parameters_verbose = 0
  integer, save :: verbose = 0

  integer, parameter :: procname_length = 80
  character(procname_length) :: current_processname = 'none' ! set by vamp2generic()
  integer, parameter :: max_parameter_length = 255 ! used for stability_logdir, install_path,
                                                   ! contract_file, printparameter file
  integer, parameter :: max_parameter_name_length = 30 ! maximal length of parameter names in init routines
  ! 0: never, 1: on finish() call, 2: adaptive, 3: always
  integer, save :: stability_log = 2
  integer, save :: write_psp = 0 ! write out phase space points from vamp2generic is called
  integer, save :: use_me_cache = 1
  integer, save :: parameters_changed = 0
  character(len=max_parameter_length) :: stability_logdir = "stability_log"
  character(len=max_parameter_length) :: tmp_dir = "."
  character(len=max_parameter_length) :: allowed_libs = ""
  character(len=max_parameter_length) :: approximation = ""
  character(len=max_parameter_length) :: shopping_list = "OL_shopping.m"
  logical, save :: stability_logdir_not_created = .true.
  character(16) :: pid_string ! 11 for pid, "-", 4 random characters
  ! OpenLoops installation path; used to locate info files and process libraries
  ! character(len=:), allocatable :: install_path ! gfortran 4.7: doesn't work in modules and type (though it does in subroutines)
  character(len=255) :: &
    & install_path = "/Users/eulisse/Downloads/OpenLoops"
  ! Mode for check_last_[...] in laststep and tensor integral routine in looproutines
  integer, save :: l_switch = 1, a_switch = 5, a_switch_rescue = 5, redlib_qp = 5
  ! switchers for checking Ward identities at tree/loop level
  integer, save :: Ward_tree = 0
  integer, save :: Ward_loop = 0
  ! 0 = full colour, 1 = leading colour
  integer, save :: LeadingColour = 0
  ! divide by the symmetry factor of identical outgoing particles
  integer, save :: out_symmetry_on = 1
  ! use flavour mappings
  integer, save :: flavour_mapping_on = 1
  ! Running number of the next partonic channel which is initialised (by get_externel_<proc>)
  ! in the tensor library cache system
  integer, save :: next_channel_number = 1
  integer, save :: coli_cache_use = 1
  ! select alpha_QED renormalization scheme: 0 = on-shell scheme, 1 = G_mu scheme
  integer, save :: ew_scheme = 1
  ! coupling order
  integer :: coupling_QCD(0:1) = -1
  integer :: coupling_EW(0:1) = -1
  integer :: order_ew = -1
  integer :: order_qcd = -1
  integer :: ckmorder = 0
  ! select tensor library for EW renormalization: 0 = none, 1 = coli, 7 = DD TODO: auto set to amp_switch
  integer, save :: ew_renorm_switch = 3
  integer, save :: do_ew_renorm = 0
  integer, save :: cms_on = 1


  ! Numerical constants
  real(dp),    parameter :: rONE   = 1
  real(dp),    parameter :: rZERO  = 0
  real(dp),    parameter :: rZERO2 = 0
  real(dp),    parameter :: pi     = acos(-1._dp)
  real(dp),    parameter :: pi2_6  = (pi**2)/6
  real(dp),    parameter :: sqrt2  = sqrt(2._dp)
  real(dp),    parameter :: sqrt05 = sqrt(0.5_dp)
  complex(dp), parameter :: cONE   = 1
  complex(dp), parameter :: ZERO   = 0
  complex(dp), parameter :: ZERO2  = 0
  complex(dp), parameter :: CI     = (0._dp, 1._dp)
  complex(dp) :: integralnorm = CI/(16*pi**2)
  complex(dp) :: countertermnorm = 1._dp/(16._dp*pi**2)

  ! scale factor for dimensionful parameters
  real(dp), save :: scalefactor = 1
  logical,        save :: reset_scalefactor = .false.
  integer,        save :: scaling_mode = 1 ! 1: reduction only, 3: everything
  ! Particle masses and widths
  real(dp), save :: rME_unscaled = 0,                    wME_unscaled = 0 ! electron mass and width
  real(dp), save :: rMM_unscaled = 0,                    wMM_unscaled = 0 ! muon mass and width
  real(dp), save :: rML_unscaled = 0,                    wML_unscaled = 0 ! tau mass and width
  real(dp), save :: rMU_unscaled = 0,                    wMU_unscaled = 0 ! up-quark mass and width
  real(dp), save :: rMD_unscaled = 0,                    wMD_unscaled = 0 ! down-quark mass and width
  real(dp), save :: rMS_unscaled = 0,                    wMS_unscaled = 0 ! strange-quark mass and width
  real(dp), save :: rMC_unscaled = 0,                    wMC_unscaled = 0 ! charm-quark mass and width
  real(dp), save :: rMB_unscaled = 0._dp,      wMB_unscaled = 0 ! bottom-quark mass and width
  real(dp), save :: rMT_unscaled = 172._dp,    wMT_unscaled = 0 ! top-quark mass and width
  real(dp), save :: rMW_unscaled = 80.399_dp,  wMW_unscaled = 0 ! W boson mass LEP PDG 2008/2009 and width
  real(dp), save :: rMZ_unscaled = 91.1876_dp, wMZ_unscaled = 0 ! Z boson mass LEP PDG 2008/2009 and width
  real(dp), save :: rMX_unscaled = 0._dp,  wMX_unscaled = 0._dp ! auxiliary field for Z
  real(dp), save :: rMY_unscaled = 0._dp,  wMY_unscaled = 0.0000_dp ! auxiliary field for W
  real(dp), save :: rMH_unscaled = 125._dp,    wMH_unscaled = 0 ! higgs boson mass and width
  real(dp), save :: MREG_unscaled = 1._dp                       ! collinear mass regulator for photon WF CT
  ! Coupling constants
  real(dp), save :: alpha_QCD = 0.1258086856923967_dp ! LO MRST
  real(dp), save :: alpha_QED = 1/128._dp
  ! Everything beyond this line is derived from the values given above and initialised by parameters_init().
  real(dp), save :: rescalefactor = 1.1
  ! scaled masses and widths
  real(dp), save :: rME, wME
  real(dp), save :: rMM, wMM
  real(dp), save :: rML, wML
  real(dp), save :: rMU, wMU
  real(dp), save :: rMD, wMD
  real(dp), save :: rMS, wMS
  real(dp), save :: rMC, wMC
  real(dp), save :: rMB, wMB
  real(dp), save :: rMT, wMT
  real(dp), save :: rMW, wMW
  real(dp), save :: rMZ, wMZ
  real(dp), save :: rMH, wMH
  real(dp), save :: rMX, wMX
  real(dp), save :: rMY, wMY
  ! Complex masses, complex and real squared masses
  complex(dp), save ::  ME,   MM,   ML,   MU,   MD,   MS,   MC,   MB,   MT,   MW,   MZ,   MH,  MX,    MY
  complex(dp), save ::  ME2,  MM2,  ML2,  MU2,  MD2,  MS2,  MC2,  MB2,  MT2,  MW2,  MZ2,  MH2, MX2,   MY2
  real(dp),    save :: rME2, rMM2, rML2, rMU2, rMD2, rMS2, rMC2, rMB2, rMT2, rMW2, rMZ2, rMH2, rMX2, rMY2
  ! collinear mass regulator for photon WF CT
  real(dp),    save :: MREG
  ! Coupling constants
  complex(dp), save :: eQED, E2_QED, gQCD, G2_QCD
  ! Weak mixing angle
  complex(dp), save :: cw, cw2, cw3, cw4, sw, sw2, sw3 ,sw4, sw6
  ! Right/left couplings of a Z boson to neutrinos, leptons, up- and down-type quarks
  complex(dp), save :: gZn(2), gZl(2), gZu(2), gZd(2)
  ! Right/left coupling for Higgs(H), Chi(X) = Z-Goldstone, Phi(P) = W-Goldstone
  complex(dp), save :: gH(2), gX(2), gPud(2), gPcs(2), gPtb(2), gPdu(2), gPsc(2), gPbt(2), gPnl(2), gPln(2)
  complex(dp) :: gZRH, gZLH
  ! Vertex scale factors for naive deviations from the Standard Model (changes don't affect CT/R2)
  real(dp), save :: lambdaHHH = 1, lambdaHWW = 1, lambdaHZZ = 1

end module ol_parameters_decl_dp



! **********************************************************************
module ol_loop_parameters_decl_dp
! Declarations and initial values for renormalisation constants and parameters of
! dimensional regularisation, dipole subtraction, tensor-integral libraries.
! Loading the module for the first time initialises the parameters with
! their default values (given in this module). Note that when a value has
! been changed using loop_parameters_init(val=...) loading the module will not
! reset the value to its default.
! **********************************************************************
  use kind_types, only: dp
  use ol_parameters_decl_dp

  use avh_olo_version, only: olo_splash_done => done


  use countdigits, only: cts_splash_todo

  implicit none
  integer,        save :: loop_parameters_status = 0


  integer,        save :: maxpoint = 6
  integer,        save :: maxrank = 6
  integer,        save :: norm_swi = 0     ! switch controlling normalisation of UV/IR poles
  character(10),  save :: norm_name
  ! switch on UV counterterms, R2 terms, IR dipoles
  integer,        save :: SwF = 1 ! factors to multiply diagrams with fermion loops
  integer,        save :: SwB = 1 ! factors to multiply diagrams with non-fermion loops
  integer,        save :: CT_is_on = 1 ! switch on/off UV CT contributions
  integer,        save :: R2_is_on = 1 ! switch on/off R2 contributions
  integer,        save :: TP_is_on = 1 ! switch on/off tadpole-like contributions
  integer,        save :: IR_is_on = 1 ! 0 = off, 1 = return poles, 2 = add I operator
  ! i-operator mode: 1 = QCD, 2 = EM, 3 = QCD+EM, none otherwise;
  ! TODO: remove this as soon as the mode is set automatically per process
  integer,        save :: ioperator_mode = 1
  integer,        save :: polecheck_is = 0

  integer,        save :: stability_mode = 14 ! 11: no trigger, default: 14
  integer,        save :: deviation_mode = 1  ! deviation measure in vamp scaling based on
                                              ! (1) k-factor (2) virtual matrix element

  real(dp), save :: trigeff_targ = .2   ! target efficiency of K-factor based stability trigger (should not be << 0.1)
  real(dp), save :: abscorr_unst = 0.01 ! absolute correction discrepancy above which a point is considered "unstable"
  real(dp), save :: ratcorr_bad  = 1    ! relative deviation to two virtual matrix elements above which
                                              ! an unstable point is considered "bad" and possibly "killed"
                                              ! (i.e. the finite part of the virtual correcton is set to zero)
  real(dp), save :: ratcorr_bad_L2 = 10 ! relative deviation to two virtual matrix elements above which
                                              ! an unstable point is killed in loop induced amplitudes

  ! Collier parameters
  integer,           save :: cll_channels = 50      ! number of cache channels
  real(dp),    save :: C_PV_threshold = 1.e-6 ! threshold precision to activate 3-point alternative reductions
  real(dp),    save :: D_PV_threshold = 1.e-6 ! threshold precision to activate 4-point alternative reductions
  integer,           save :: dd_red_mode    = 2     ! PV or alternative 3/4-point reductions
  ! setaccuracy_cll() arguments
  real(dp),    save :: cll_pvthr = 1.e-6, cll_accthr = 1.e-4, cll_mode3thr = 1.e-8
  integer,           save :: cll_tenred = 7 ! settenred_cll(): # of legs from which on component reduction is used
  real(dp),    save :: ti_os_thresh = 1.e-10

  ! CutTools parameters
  real(dp),    save :: opprootsvalue_unscaled = 1000
  real(dp),    save :: opprootsvalue
  real(dp),    save :: opplimitvalue = 0.01
  real(dp),    save :: oppthrs       = 1.e-6
  integer,           save :: oppidig       = 0
  integer,           save :: oppscaloop    = 2







  ! Samurai parameters
  character(4),      save :: set_imeth     = 'diag'
  integer,           save :: set_isca      = 2
  integer,           save :: set_verbosity = 0
  integer,           save :: set_itest     = 0

  logical,           save :: samurai_not_init      = .true.
  logical,           save :: cuttools_not_init     = .true.
  logical,           save :: coli_not_init         = .true.
  logical,           save :: dd_not_init           = .true.
  logical,           save :: dd_qp_not_init        = .true.
  logical,           save :: tensorlib_not_init    = .true.
  logical,           save :: tensorlib_qp_not_init = .true.

  ! Generic parameters related to tensor reduction
  integer, save :: tensor_reduction_error = 0

  logical, save :: reset_mureg = .true.
  logical, save :: reset_oppthrs = .true.

  integer,        save      :: nc    = 3          ! number of colours
  integer,        save      :: nf = 6, nf_up = 3, nf_down =3 ! number of quarks (total, up-type, down-type)
  integer,        save      :: nq_nondecoupl = 5  ! number of quarks which don't decouple above threshold
  integer,        save      :: N_lf  = 5          ! number of massless quark flavours
! ifdef 1


  real(dp), save      :: ca    = 3          ! adjoint Casimir
  real(dp), save      :: cf    = 4._dp/3 ! fundamental Casimir
  real(dp), save      :: tf    = 0.5_dp  ! generator normalisation

  real(dp), save      :: polescale   = 1 ! used as pole values in VAMP2chk to determine the true poles
  real(dp), save      :: de1_UV      = 0 ! numerical value of single UV pole (independent of norm-convention)
  real(dp), save      :: de1_IR      = 0 ! numerical value of single IR pole (independent of norm-convention)
  real(dp), save      :: de2_i_IR    = 0 ! numerical value of double IR pole using actual norm-convention
  real(dp), save      :: de2_i_shift = 0 ! double pole shift defining actual norm convention
  real(dp), save      :: mureg_unscaled = 100    ! renormalisation scale
  real(dp), save      :: mureg
  real(dp), save      :: x_UV  = 1       ! rescaling factor for dim-reg scale in UV-divergent quantities
  real(dp), save      :: x_IR  = 1       ! rescaling factor for dim-reg scale in IR-divergent quantities
  real(dp), parameter :: kappa = 2/3._dp ! kappa parameter used in dipole subtraction

  ! the following derived parameters are initilised by subroutine loop_parameters_init
  real(dp), save :: de2_0_IR  ! numerical value of double IR pole using LH-accord convention (i=0)
  real(dp), save :: de2_1_IR  ! numerical value of double IR pole using COLI convention (i=1)
  real(dp), save :: mureg2    ! squared renormalisation scale
  real(dp), save :: mu2_UV    ! dim-reg scale for UV-divergent quantities
  real(dp), save :: mu2_IR    ! dim-reg scale for IR-divergent quantities

  ! the following renormalisation constants are initilised by subroutine QCD_renormalisation
  complex(dp), save :: dZMC     = 0 ! charm-quark mass RC        : MC_bare = MC*(1+dZMC)
  complex(dp), save :: dZMB     = 0 ! bottom-quark mass RC       : MB_bare = MB*(1+dZMB)
  complex(dp), save :: dZMT     = 0 ! top-quark mass RC          : MT_bare = MT*(1+dZMT)
  real(dp),    save :: dZg      = 0 ! gluon-field RC             : G_bare  = (1+dZg/2)*G_ren
  real(dp),    save :: dZq      = 0 ! massless-quark field RC    : Q_bare  = (1+dZq/2)*Q_ren
  real(dp),    save :: dZc      = 0 ! charm-quark field RC       : idem
  real(dp),    save :: dZb      = 0 ! bottom-quark field RC      : idem
  real(dp),    save :: dZt      = 0 ! top-quark field RC         : idem
  real(dp),    save :: dgQCD    = 0 ! strong coupling RC         : g_bare  = (1+dgQCD)*g_ren
  real(dp),    save :: dgQCDym  = 0 ! YM-contribution to delnG
  real(dp),    save :: dgQCDfer = 0 ! fermionic-contribution to delnG

  ! Counter terms for QCD corrections
  complex(dp), save :: ctqq(2) ! massless quark propagator counter term
  complex(dp), save :: ctcc(2) ! charm quark propagator counter term
  complex(dp), save :: ctbb(2) ! bottom quark propagator counter term
  complex(dp), save :: cttt(2) ! top quark propagator counter term
  complex(dp), save :: ctGG(3) ! gluon propagator counter term
  real(dp),    save :: ctGqq   ! massless quark-gluon vertex counter term
  real(dp),    save :: ctGcc   ! charm quark-gluon vertex counter term (massive or massless c)
  real(dp),    save :: ctGbb   ! bottom quark-gluon vertex counter term (massive or massless b)
  real(dp),    save :: ctGtt   ! top quark-gluon vertex counter term
  real(dp),    save :: ctVVV   ! three gluon vertex counter term
  real(dp),    save :: ctVVVV  ! four gluon vertex counter term (times 1/2)
  real(dp),    save :: ctVsc   ! Wcs (massive or massless c) vertex counter term
  real(dp),    save :: ctVbt   ! Wtb (massive or massless b) vertex counter term
  real(dp),    save :: ctVtt   ! Att and Ztt vertex counter term
  real(dp),    save :: ctVcc   ! Acc and Zcc vertex counter term (massive or massless c)
  real(dp),    save :: ctVbb   ! Abb and Zbb vertex counter term (massive or massless b)
  real(dp),    save :: ctVqq   ! Aqq and Zqq (massless q) vertex counter term
  complex(dp), save :: ctScs(2)
  complex(dp), save :: ctSsc(2)
  complex(dp), save :: ctStb(2)
  complex(dp), save :: ctSbt(2)
  complex(dp), save :: ctSqq
  complex(dp), save :: ctScc
  complex(dp), save :: ctSbb
  complex(dp), save :: ctStt

  ! Additional parameters for R2
  complex(dp), save :: MQ2sum, MQ2sum_pairs

  ! Additional counterterms for R2 QCD
  complex(dp), save :: ctZGG
  complex(dp), save :: ctHGG
  complex(dp), save :: ctAAGG
  complex(dp), save :: ctAZGG
  complex(dp), save :: ctZZGG
  complex(dp), save :: ctWWGG
  complex(dp), save :: ctHHGG
  complex(dp), save :: ctHXGG
  complex(dp), save :: ctXXGG
  complex(dp), save :: ctPPGG
  complex(dp), save :: ctAGGG(2)
  complex(dp), save :: ctZGGG(2)
  integer,           save :: R2GGGG

  ! EW_renormalisation renormalisation constants
  complex(dp), save :: dZMBEW     = 0 ! bottom-quark mass RC       : MB_bare = MB+dZMBEW)
  complex(dp), save :: dZMTEW     = 0 ! top-quark mass RC          : MT_bare = MT+dZMTEW)
  complex(dp), save :: dZMLEW     = 0 ! tau-lepton mass RC         : ML_bare = ML+dZMLEW)
  complex(dp), save :: dZMW2EW    = 0 ! W mass RC                  : MW^2_bare = MW^2+dZMW2EW^2
  complex(dp), save :: dZMZ2EW    = 0 ! Z mass RC                  : MZ^2_bare = MZ^2+dZMZ2EW^2
  complex(dp), save :: dZMH2EW    = 0 ! H mass RC                  : MH^2_bare = MH^2+dZMH2EW^2
  complex(dp), save :: dswEW      = 0 ! sin EW mixing angle RC         : sw_bare = sw + dswEW  i.e. dswEW/swEW = - c^2/s^2 dcwEW/c ; dcEW/c=1/2(dZMW2/MW^2-dZMZ2/MZ^2)
  complex(dp), save :: dcwEW      = 0 ! cos EW mixing angle RC         : dcwEW/c=1/2(dZMW2/MW^2-dZMZ2/MZ^2) defined for convinience

!   complex(rp), save      :: dZqLEW     = 0 ! L-massless-quark field RC : Q_bare  = (1+1/2*dZqLEW)*Q_ren
!   complex(rp), save      :: dZqREW     = 0 ! R-massless-quark field RC : Q_bare  = (1+1/2*dZqREW)*Q_ren
  complex(dp), save :: dZuLEW     = 0 ! L-massless-u-quark field RC : Q_bare  = (1+1/2*dZuLEW)*Q_ren
  complex(dp), save :: dZuREW     = 0 ! R-massless-u-quark field RC : Q_bare  = (1+1/2*dZuREW)*Q_ren
  complex(dp), save :: dZdLEW     = 0 ! L-massless-d-quark field RC : Q_bare  = (1+1/2*dZdLEW)*Q_ren
  complex(dp), save :: dZdREW     = 0 ! R-massless-d-quark field RC : Q_bare  = (1+1/2*dZdREW)*Q_ren
  complex(dp), save :: dZbLEW     = 0 ! L-bottom-quark field RC     : idem
  complex(dp), save :: dZbREW     = 0 ! R-bottom-quark field RC     : idem
  complex(dp), save :: dZtLEW     = 0 ! L-top-quark field RC        : idem
  complex(dp), save :: dZtREW     = 0 ! R-top-quark field RC        : idem
  complex(dp), save :: dZeLEW     = 0 ! L-lepton field RC           : idem
  complex(dp), save :: dZeREW     = 0 ! R-lepton field RC           : idem
  complex(dp), save :: dZnLEW     = 0 ! L-neutrino field RC         : idem
  complex(dp), save :: dZLLEW     = 0 ! L-tau-lepton field RC       : idem
  complex(dp), save :: dZLREW     = 0 ! R-tau-lepton field RC       : idem
  complex(dp), save :: dZuLEWcc   = 0 ! L-massless-u-quark field RC : Q_bare  = (1+1/2*dZuLEW)*Q_ren
  complex(dp), save :: dZuREWcc   = 0 ! R-massless-u-quark field RC : Q_bare  = (1+1/2*dZuREW)*Q_ren
  complex(dp), save :: dZdLEWcc   = 0 ! L-massless-d-quark field RC : Q_bare  = (1+1/2*dZdLEW)*Q_ren
  complex(dp), save :: dZdREWcc   = 0 ! R-massless-d-quark field RC : Q_bare  = (1+1/2*dZdREW)*Q_ren
  complex(dp), save :: dZbLEWcc   = 0 ! L-bottom-quark field RC     : idem
  complex(dp), save :: dZbREWcc   = 0 ! R-bottom-quark field RC     : idem
  complex(dp), save :: dZtLEWcc   = 0 ! L-top-quark field RC        : idem
  complex(dp), save :: dZtREWcc   = 0 ! R-top-quark field RC        : idem
  complex(dp), save :: dZeLEWcc   = 0 ! L-lepton field RC           : idem
  complex(dp), save :: dZeREWcc   = 0 ! R-lepton field RC           : idem
  complex(dp), save :: dZnLEWcc   = 0 ! L-neutrino field RC         : idem
  complex(dp), save :: dZLLEWcc   = 0 ! L-tau-lepton field RC       : idem
  complex(dp), save :: dZLREWcc    = 0 ! R-tau-lepton field RC       : idem

  complex(dp), save :: dZWEW      = 0 ! W field RC                 : idem
  complex(dp), save :: dZZZEW     = 0 ! ZZ field RC                : idem
  complex(dp), save :: dZAZEW     = 0 ! AZ field RC                : idem
  complex(dp), save :: dZZAEW     = 0 ! AZ field RC                : idem
  complex(dp), save :: dZAAEW     = 0  ! AA field RC                : idem
  complex(dp), save :: dZHEW      = 0 ! H field RC                 : idem

  complex(dp), save :: dtEW       = 0 ! tadpole-RC                 :
  complex(dp), save :: dZeQEDEW   = 0 ! EW coupling RC         : e_bare  = (1+dZeEW)*e_ren

  ! Counter terms for EW corrections
  ! VV Vector propagators
  complex(dp), save :: EWctWW(3)
  complex(dp), save :: EWctZZ(3)
  complex(dp), save :: EWctAZ(3)
  complex(dp), save :: EWctAA(3)
  ! SS scalar propagators
  complex(dp), save :: EWctHH(2)
  complex(dp), save :: EWctXX(2)
  complex(dp), save :: EWctPP(2)
  ! SV scalar-vector mixing
  complex(dp), save :: EWctXA
  complex(dp), save :: EWctXZ
  complex(dp), save :: EWctPW
  ! FF fermionic propagators
  complex(dp), save :: EWctuu(4)
  complex(dp), save :: EWctdd(4)
  complex(dp), save :: EWcttt(4)
  complex(dp), save :: EWctbb(4)
  complex(dp), save :: EWctee(4)
  complex(dp), save :: EWctLL(4)
  complex(dp), save :: EWctnn(4)
  !VVVV
  complex(dp), save :: EWctWWWW(2)
  complex(dp), save :: EWctWWZZ(2)
  complex(dp), save :: EWctWWAZ(2)
  complex(dp), save :: EWctWWAA(2)
  !VVVV pure R2
  complex(dp), save :: EWctR2AAAA
  complex(dp), save :: EWctR2AAAZ
  complex(dp), save :: EWctR2AAZZ
  complex(dp), save :: EWctR2AZZZ
  complex(dp), save :: EWctR2ZZZZ
  !VVV
  complex(dp), save :: EWctAWW
  complex(dp), save :: EWctZWW
  !SSSS
  complex(dp), save :: EWctSSSS1
  complex(dp), save :: EWctSSSS2
  complex(dp), save :: EWctSSSS3
  complex(dp), save :: EWctHHHH
  complex(dp), save :: EWctHHXX
  complex(dp), save :: EWctHHPP
  complex(dp), save :: EWctXXXX
  complex(dp), save :: EWctXXPP
  complex(dp), save :: EWctPPPP
  !SSS
  complex(dp), save :: EWctHHH
  complex(dp), save :: EWctHXX
  complex(dp), save :: EWctHPP
  !VVSS
  complex(dp), save :: EWctWWXX
  complex(dp), save :: EWctWWHH
  complex(dp), save :: EWctWWPP
  complex(dp), save :: EWctZZPP
  complex(dp), save :: EWctZAPP
  complex(dp), save :: EWctAAPP
  complex(dp), save :: EWctZZHH
  complex(dp), save :: EWctZZXX
  complex(dp), save :: EWctZAHH
  complex(dp), save :: EWctWZPH
  complex(dp), save :: EWctWAPH
  complex(dp), save :: EWctWZPX
  complex(dp), save :: EWctWAPX
  !VVSS R2
  complex(dp), save :: EWctAAHH
  complex(dp), save :: EWctAAXX
  !VSS
  complex(dp), save :: EWctAXH
  complex(dp), save :: EWctZXH
  complex(dp), save :: EWctAPP
  complex(dp), save :: EWctZPP
  complex(dp), save :: EWctWPH
  complex(dp), save :: EWctWPX
  !SVV
  complex(dp), save :: EWctHWW
  complex(dp), save :: EWctHZZ
  complex(dp), save :: EWctHZA
  complex(dp), save :: EWctPWZ
  complex(dp), save :: EWctPWA
  ! pure R2 SVV
  complex(dp), save :: EWctHAA
  !VFF
  ! Aff
  complex(dp), save :: EWctAuu(2)
  complex(dp), save :: EWctAdd(2)
  complex(dp), save :: EWctAtt(2)
  complex(dp), save :: EWctAbb(2)
  complex(dp), save :: EWctAee(2)
  complex(dp), save :: EWctALL(2)
  complex(dp), save :: EWctAnn(2)
  ! Zff
  complex(dp), save :: dgZu(2)
  complex(dp), save :: dgZd(2)
  complex(dp), save :: dgZl(2)
  complex(dp), save :: dgZn(2)
  complex(dp), save :: EWctVuu(2)
  complex(dp), save :: EWctVdd(2)
  complex(dp), save :: EWctVtt(2)
  complex(dp), save :: EWctVbb(2)
  complex(dp), save :: EWctVee(2)
  complex(dp), save :: EWctVLL(2)
  complex(dp), save :: EWctVnn(2)
  ! Wff
  complex(dp), save :: EWctVdu
  complex(dp), save :: EWctVbt
  complex(dp), save :: EWctVen
  complex(dp), save :: EWctVLn
  complex(dp), save :: EWctVud
  complex(dp), save :: EWctVtb
  complex(dp), save :: EWctVne
  complex(dp), save :: EWctVnL
  ! Gff mixed EW/QCD
  complex(dp), save ::  EWctGuu(2)
  complex(dp), save ::  EWctGdd(2)
  complex(dp), save ::  EWctGtt(2)
  complex(dp), save ::  EWctGbb(2)
  !SFF
  complex(dp), save :: EWctHtt(2)
  complex(dp), save :: EWctHbb(2)
  complex(dp), save :: EWctHLL(2)
  complex(dp), save :: EWctXtt(2)
  complex(dp), save :: EWctXbb(2)
  complex(dp), save :: EWctXLL(2)
  complex(dp), save :: EWctPtb(2)
  complex(dp), save :: EWctPbt(2)
  complex(dp), save :: EWctPnL(2)
  complex(dp), save :: EWctPLn(2)
  ! VUU
  complex(dp), save :: EWctAUWUW
  complex(dp), save :: EWctZUWUW
  complex(dp), save :: EWctWUWUZ
  complex(dp), save :: EWctWUZUW
  complex(dp), save :: EWctWUWUA
  complex(dp), save :: EWctWUAUW
  ! SUU
  complex(dp), save :: EWctHUZUZ
  complex(dp), save :: EWctHUWUW
  complex(dp), save :: EWctXUWUW
  complex(dp), save :: EWctPUZUW
  complex(dp), save :: EWctPUWUZ
  complex(dp), save :: EWctPUWUA

  ! Additional parameters for R2 EW
  complex(dp), save :: sumMQ2
  complex(dp), save :: sumMQ2Q2
  complex(dp), save :: sumMQ2QI
  complex(dp), save :: sumMQ4
  complex(dp), save :: sumMUD
  complex(dp), save :: sumMUD2
  complex(dp), save :: sumMU2
  complex(dp), save :: sumMD2
  complex(dp), save :: sumMU4
  complex(dp), save :: sumMD4
  complex(dp), save :: sumMQ2QUD

end module ol_loop_parameters_decl_dp

