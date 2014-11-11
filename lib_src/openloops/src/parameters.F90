
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


module ol_data_types_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  type wfun
    ! four complex components for the wave function
     complex(REALKIND) :: j(4)
    ! indicator if left- or right components of of-sheel line vanish
    !                      j= (0,0,0,0) (0,0,j3,j4) (j1,j2,0,0) (j1,j2,j3,j4)
    integer(intkind1) :: h !    B"00"      B"01"       B"10"        B"11"
    integer(intkind2) :: e ! helicities of external on-shell lines
  end type wfun

  type polcont
    complex(REALKIND) :: j
    integer(intkind2) :: e ! helicities of external on-shell lines
    integer(intkind2) :: s ! table for final helicity syncronisation
  end type polcont

#ifdef PRECISION_dp
  type me_cache
    real(REALKIND), allocatable :: psp(:,:), me(:)
  end type me_cache
#endif

end module ol_data_types_/**/REALKIND


module ol_momenta_decl_/**/REALKIND
  use KIND_TYPES, only: REALKIND, MaxParticles
  implicit none
  ! Internal momenta for up to 'MaxParticles' external particles
  ! Components 1:4 = light cone representation; component 5 = squared momentum
  complex(REALKIND), save :: Q(5,0:2**MaxParticles-1) = 0 ! 2^Nmax - 1 = 511
  complex(REALKIND), allocatable, save :: QInvariantsMatrix(:,:)
  contains

  function momenta_nan_check(P)
    implicit none
    real(REALKIND), intent(in) :: P(:,:)
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

end module ol_momenta_decl_/**/REALKIND



module ol_external_decl_/**/REALKIND
  use KIND_TYPES, only: REALKIND, MaxParticles
  implicit none
  ! phase space point cache; used to print the ps-point if tensor reduction fails
  integer,        save :: nParticles = 0 ! set by conv_mom_scatt2in
  real(REALKIND), save :: M_ex(MaxParticles) ! unused
  real(REALKIND), save :: P_ex(0:3,MaxParticles) ! uncleaned external 2->n-2 momenta, set by conv_mom_scatt2in
  integer,        save :: crossing(MaxParticles) = 0 ! only used if a reduction error occurs
  integer,        save :: inverse_crossing(MaxParticles) = 0 ! set by conv_mom_scatt2in
  integer,  save :: gf_array(MaxParticles) = 0
  ! lists for each external particle the external momentum
  ! used for the gauge fixing of the vector polarization. Used in subroutine wf_gf_V.
  ! A zero entry means that it's not a massless vector particle.
  integer,  save :: Ward_array(MaxParticles) = 0 ! select particle "i" for the Ward identity -> Ward_array(i) = 1
end module ol_external_decl_/**/REALKIND



module ol_pseudotree_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  ! loop momentum in pseudo tree (standard representation)
  real(REALKIND), save :: pseudotree_momentum(0:3) = [ 314.1592653589793_/**/REALKIND, 271.8281828459045_/**/REALKIND, 100._/**/REALKIND, 57.72156649015328_/**/REALKIND ]
  ! Wave functions for pseudo tree
  complex(REALKIND), save :: exloop(4,2) = reshape([ 2.718_/**/REALKIND, 3.141_/**/REALKIND,  0.9159_/**/REALKIND, 1._/**/REALKIND,  &
                                               1._/**/REALKIND,    0.5772_/**/REALKIND, 1.618_/**/REALKIND,  1.282_/**/REALKIND], [ 4, 2 ])
end module ol_pseudotree_/**/REALKIND



module ol_parameters_decl_/**/REALKIND
  ! Declarations and initial values for numerical and physical parameters like masses, widths, and couplings.
  ! Loading the module for the first time initialises the parameters with their default values (given in this module).
  ! Note that when a value has been changed using parameters_init(val=...) loading the module
  ! will not reset the value to its default.
  use KIND_TYPES, only: REALKIND
  use ol_version, only: splash_todo
!   use TI_call_interface
  implicit none
  ! Counted up by 1 each time parameters_init() is called
  integer, save :: parameters_status = 0
  integer, save :: parameters_verbose = 0
  integer, save :: verbose = 0
#ifdef PRECISION_dp
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
    & install_path = OL_INSTALL_PATH
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
#endif

  ! Numerical constants
  real(REALKIND),    parameter :: rONE   = 1
  real(REALKIND),    parameter :: rZERO  = 0
  real(REALKIND),    parameter :: rZERO2 = 0
  real(REALKIND),    parameter :: pi     = acos(-1._/**/REALKIND)
  real(REALKIND),    parameter :: pi2_6  = (pi**2)/6
  real(REALKIND),    parameter :: sqrt2  = sqrt(2._/**/REALKIND)
  real(REALKIND),    parameter :: sqrt05 = sqrt(0.5_/**/REALKIND)
  complex(REALKIND), parameter :: cONE   = 1
  complex(REALKIND), parameter :: ZERO   = 0
  complex(REALKIND), parameter :: ZERO2  = 0
  complex(REALKIND), parameter :: CI     = (0._/**/REALKIND, 1._/**/REALKIND)
  complex(REALKIND) :: integralnorm = CI/(16*pi**2)
  complex(REALKIND) :: countertermnorm = 1._/**/REALKIND/(16._/**/REALKIND*pi**2)

  ! scale factor for dimensionful parameters
  real(REALKIND), save :: scalefactor = 1
  logical,        save :: reset_scalefactor = .false.
  integer,        save :: scaling_mode = 1 ! 1: reduction only, 3: everything
  ! Particle masses and widths
  real(REALKIND), save :: rME_unscaled = 0,                    wME_unscaled = 0 ! electron mass and width
  real(REALKIND), save :: rMM_unscaled = 0,                    wMM_unscaled = 0 ! muon mass and width
  real(REALKIND), save :: rML_unscaled = 0,                    wML_unscaled = 0 ! tau mass and width
  real(REALKIND), save :: rMU_unscaled = 0,                    wMU_unscaled = 0 ! up-quark mass and width
  real(REALKIND), save :: rMD_unscaled = 0,                    wMD_unscaled = 0 ! down-quark mass and width
  real(REALKIND), save :: rMS_unscaled = 0,                    wMS_unscaled = 0 ! strange-quark mass and width
  real(REALKIND), save :: rMC_unscaled = 0,                    wMC_unscaled = 0 ! charm-quark mass and width
  real(REALKIND), save :: rMB_unscaled = 0._/**/REALKIND,      wMB_unscaled = 0 ! bottom-quark mass and width
  real(REALKIND), save :: rMT_unscaled = 172._/**/REALKIND,    wMT_unscaled = 0 ! top-quark mass and width
  real(REALKIND), save :: rMW_unscaled = 80.399_/**/REALKIND,  wMW_unscaled = 0 ! W boson mass LEP PDG 2008/2009 and width
  real(REALKIND), save :: rMZ_unscaled = 91.1876_/**/REALKIND, wMZ_unscaled = 0 ! Z boson mass LEP PDG 2008/2009 and width
  real(REALKIND), save :: rMX_unscaled = 0._/**/REALKIND,  wMX_unscaled = 0._/**/REALKIND ! auxiliary field for Z
  real(REALKIND), save :: rMY_unscaled = 0._/**/REALKIND,  wMY_unscaled = 0.0000_/**/REALKIND ! auxiliary field for W
  real(REALKIND), save :: rMH_unscaled = 125._/**/REALKIND,    wMH_unscaled = 0 ! higgs boson mass and width
  real(REALKIND), save :: MREG_unscaled = 1._/**/REALKIND                       ! collinear mass regulator for photon WF CT
  ! Coupling constants
  real(REALKIND), save :: alpha_QCD = 0.1258086856923967_/**/REALKIND ! LO MRST
  real(REALKIND), save :: alpha_QED = 1/128._/**/REALKIND
  ! Everything beyond this line is derived from the values given above and initialised by parameters_init().
  real(REALKIND), save :: rescalefactor = 1.1
  ! scaled masses and widths
  real(REALKIND), save :: rME, wME
  real(REALKIND), save :: rMM, wMM
  real(REALKIND), save :: rML, wML
  real(REALKIND), save :: rMU, wMU
  real(REALKIND), save :: rMD, wMD
  real(REALKIND), save :: rMS, wMS
  real(REALKIND), save :: rMC, wMC
  real(REALKIND), save :: rMB, wMB
  real(REALKIND), save :: rMT, wMT
  real(REALKIND), save :: rMW, wMW
  real(REALKIND), save :: rMZ, wMZ
  real(REALKIND), save :: rMH, wMH
  real(REALKIND), save :: rMX, wMX
  real(REALKIND), save :: rMY, wMY
  ! Complex masses, complex and real squared masses
  complex(REALKIND), save ::  ME,   MM,   ML,   MU,   MD,   MS,   MC,   MB,   MT,   MW,   MZ,   MH,  MX,    MY
  complex(REALKIND), save ::  ME2,  MM2,  ML2,  MU2,  MD2,  MS2,  MC2,  MB2,  MT2,  MW2,  MZ2,  MH2, MX2,   MY2
  real(REALKIND),    save :: rME2, rMM2, rML2, rMU2, rMD2, rMS2, rMC2, rMB2, rMT2, rMW2, rMZ2, rMH2, rMX2, rMY2
  ! collinear mass regulator for photon WF CT
  real(REALKIND),    save :: MREG
  ! Coupling constants
  complex(REALKIND), save :: eQED, E2_QED, gQCD, G2_QCD
  ! Weak mixing angle
  complex(REALKIND), save :: cw, cw2, cw3, cw4, sw, sw2, sw3 ,sw4, sw6
  ! Right/left couplings of a Z boson to neutrinos, leptons, up- and down-type quarks
  complex(REALKIND), save :: gZn(2), gZl(2), gZu(2), gZd(2)
  ! Right/left coupling for Higgs(H), Chi(X) = Z-Goldstone, Phi(P) = W-Goldstone
  complex(REALKIND), save :: gH(2), gX(2), gPud(2), gPcs(2), gPtb(2), gPdu(2), gPsc(2), gPbt(2), gPnl(2), gPln(2)
  complex(REALKIND) :: gZRH, gZLH
  ! Vertex scale factors for naive deviations from the Standard Model (changes don't affect CT/R2)
  real(REALKIND), save :: lambdaHHH = 1, lambdaHWW = 1, lambdaHZZ = 1

end module ol_parameters_decl_/**/REALKIND



! **********************************************************************
module ol_loop_parameters_decl_/**/REALKIND
! Declarations and initial values for renormalisation constants and parameters of
! dimensional regularisation, dipole subtraction, tensor-integral libraries.
! Loading the module for the first time initialises the parameters with
! their default values (given in this module). Note that when a value has
! been changed using loop_parameters_init(val=...) loading the module will not
! reset the value to its default.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_parameters_decl_/**/REALKIND
#ifdef USE_ONELOOP
  use avh_olo_version, only: olo_splash_done => done
#endif
#ifdef USE_CUTTOOLS
  use countdigits, only: cts_splash_todo
#endif
  implicit none
  integer,        save :: loop_parameters_status = 0

#ifdef PRECISION_dp
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

  real(REALKIND), save :: trigeff_targ = .2   ! target efficiency of K-factor based stability trigger (should not be << 0.1)
  real(REALKIND), save :: abscorr_unst = 0.01 ! absolute correction discrepancy above which a point is considered "unstable"
  real(REALKIND), save :: ratcorr_bad  = 1    ! relative deviation to two virtual matrix elements above which
                                              ! an unstable point is considered "bad" and possibly "killed"
                                              ! (i.e. the finite part of the virtual correcton is set to zero)
  real(REALKIND), save :: ratcorr_bad_L2 = 10 ! relative deviation to two virtual matrix elements above which
                                              ! an unstable point is killed in loop induced amplitudes

  ! Collier parameters
  integer,           save :: cll_channels = 50      ! number of cache channels
  real(REALKIND),    save :: C_PV_threshold = 1.e-6 ! threshold precision to activate 3-point alternative reductions
  real(REALKIND),    save :: D_PV_threshold = 1.e-6 ! threshold precision to activate 4-point alternative reductions
  integer,           save :: dd_red_mode    = 2     ! PV or alternative 3/4-point reductions
  ! setaccuracy_cll() arguments
  real(REALKIND),    save :: cll_pvthr = 1.e-6, cll_accthr = 1.e-4, cll_mode3thr = 1.e-8
  integer,           save :: cll_tenred = 7 ! settenred_cll(): # of legs from which on component reduction is used
  real(REALKIND),    save :: ti_os_thresh = 1.e-10

  ! CutTools parameters
  real(REALKIND),    save :: opprootsvalue_unscaled = 1000
  real(REALKIND),    save :: opprootsvalue
  real(REALKIND),    save :: opplimitvalue = 0.01
  real(REALKIND),    save :: oppthrs       = 1.e-6
  integer,           save :: oppidig       = 0
  integer,           save :: oppscaloop    = 2
#ifndef USE_ONELOOP
  logical,           save :: olo_splash_done = .false.
#endif
#ifndef USE_CUTTOOLS
  logical,           save :: cts_splash_todo = .true.
#endif

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
! ifdef PRECISION_dp
#endif

  real(REALKIND), save      :: ca    = 3          ! adjoint Casimir
  real(REALKIND), save      :: cf    = 4._/**/REALKIND/3 ! fundamental Casimir
  real(REALKIND), save      :: tf    = 0.5_/**/REALKIND  ! generator normalisation

  real(REALKIND), save      :: polescale   = 1 ! used as pole values in VAMP2chk to determine the true poles
  real(REALKIND), save      :: de1_UV      = 0 ! numerical value of single UV pole (independent of norm-convention)
  real(REALKIND), save      :: de1_IR      = 0 ! numerical value of single IR pole (independent of norm-convention)
  real(REALKIND), save      :: de2_i_IR    = 0 ! numerical value of double IR pole using actual norm-convention
  real(REALKIND), save      :: de2_i_shift = 0 ! double pole shift defining actual norm convention
  real(REALKIND), save      :: mureg_unscaled = 100    ! renormalisation scale
  real(REALKIND), save      :: mureg
  real(REALKIND), save      :: x_UV  = 1       ! rescaling factor for dim-reg scale in UV-divergent quantities
  real(REALKIND), save      :: x_IR  = 1       ! rescaling factor for dim-reg scale in IR-divergent quantities
  real(REALKIND), parameter :: kappa = 2/3._/**/REALKIND ! kappa parameter used in dipole subtraction

  ! the following derived parameters are initilised by subroutine loop_parameters_init
  real(REALKIND), save :: de2_0_IR  ! numerical value of double IR pole using LH-accord convention (i=0)
  real(REALKIND), save :: de2_1_IR  ! numerical value of double IR pole using COLI convention (i=1)
  real(REALKIND), save :: mureg2    ! squared renormalisation scale
  real(REALKIND), save :: mu2_UV    ! dim-reg scale for UV-divergent quantities
  real(REALKIND), save :: mu2_IR    ! dim-reg scale for IR-divergent quantities

  ! the following renormalisation constants are initilised by subroutine QCD_renormalisation
  complex(REALKIND), save :: dZMC     = 0 ! charm-quark mass RC        : MC_bare = MC*(1+dZMC)
  complex(REALKIND), save :: dZMB     = 0 ! bottom-quark mass RC       : MB_bare = MB*(1+dZMB)
  complex(REALKIND), save :: dZMT     = 0 ! top-quark mass RC          : MT_bare = MT*(1+dZMT)
  real(REALKIND),    save :: dZg      = 0 ! gluon-field RC             : G_bare  = (1+dZg/2)*G_ren
  real(REALKIND),    save :: dZq      = 0 ! massless-quark field RC    : Q_bare  = (1+dZq/2)*Q_ren
  real(REALKIND),    save :: dZc      = 0 ! charm-quark field RC       : idem
  real(REALKIND),    save :: dZb      = 0 ! bottom-quark field RC      : idem
  real(REALKIND),    save :: dZt      = 0 ! top-quark field RC         : idem
  real(REALKIND),    save :: dgQCD    = 0 ! strong coupling RC         : g_bare  = (1+dgQCD)*g_ren
  real(REALKIND),    save :: dgQCDym  = 0 ! YM-contribution to delnG
  real(REALKIND),    save :: dgQCDfer = 0 ! fermionic-contribution to delnG

  ! Counter terms for QCD corrections
  complex(REALKIND), save :: ctqq(2) ! massless quark propagator counter term
  complex(REALKIND), save :: ctcc(2) ! charm quark propagator counter term
  complex(REALKIND), save :: ctbb(2) ! bottom quark propagator counter term
  complex(REALKIND), save :: cttt(2) ! top quark propagator counter term
  complex(REALKIND), save :: ctGG(3) ! gluon propagator counter term
  real(REALKIND),    save :: ctGqq   ! massless quark-gluon vertex counter term
  real(REALKIND),    save :: ctGcc   ! charm quark-gluon vertex counter term (massive or massless c)
  real(REALKIND),    save :: ctGbb   ! bottom quark-gluon vertex counter term (massive or massless b)
  real(REALKIND),    save :: ctGtt   ! top quark-gluon vertex counter term
  real(REALKIND),    save :: ctVVV   ! three gluon vertex counter term
  real(REALKIND),    save :: ctVVVV  ! four gluon vertex counter term (times 1/2)
  real(REALKIND),    save :: ctVsc   ! Wcs (massive or massless c) vertex counter term
  real(REALKIND),    save :: ctVbt   ! Wtb (massive or massless b) vertex counter term
  real(REALKIND),    save :: ctVtt   ! Att and Ztt vertex counter term
  real(REALKIND),    save :: ctVcc   ! Acc and Zcc vertex counter term (massive or massless c)
  real(REALKIND),    save :: ctVbb   ! Abb and Zbb vertex counter term (massive or massless b)
  real(REALKIND),    save :: ctVqq   ! Aqq and Zqq (massless q) vertex counter term
  complex(REALKIND), save :: ctScs(2)
  complex(REALKIND), save :: ctSsc(2)
  complex(REALKIND), save :: ctStb(2)
  complex(REALKIND), save :: ctSbt(2)
  complex(REALKIND), save :: ctSqq
  complex(REALKIND), save :: ctScc
  complex(REALKIND), save :: ctSbb
  complex(REALKIND), save :: ctStt

  ! Additional parameters for R2
  complex(REALKIND), save :: MQ2sum, MQ2sum_pairs

  ! Additional counterterms for R2 QCD
  complex(REALKIND), save :: ctZGG
  complex(REALKIND), save :: ctHGG
  complex(REALKIND), save :: ctAAGG
  complex(REALKIND), save :: ctAZGG
  complex(REALKIND), save :: ctZZGG
  complex(REALKIND), save :: ctWWGG
  complex(REALKIND), save :: ctHHGG
  complex(REALKIND), save :: ctHXGG
  complex(REALKIND), save :: ctXXGG
  complex(REALKIND), save :: ctPPGG
  complex(REALKIND), save :: ctAGGG(2)
  complex(REALKIND), save :: ctZGGG(2)
  integer,           save :: R2GGGG

  ! EW_renormalisation renormalisation constants
  complex(REALKIND), save :: dZMBEW     = 0 ! bottom-quark mass RC       : MB_bare = MB+dZMBEW)
  complex(REALKIND), save :: dZMTEW     = 0 ! top-quark mass RC          : MT_bare = MT+dZMTEW)
  complex(REALKIND), save :: dZMLEW     = 0 ! tau-lepton mass RC         : ML_bare = ML+dZMLEW)
  complex(REALKIND), save :: dZMW2EW    = 0 ! W mass RC                  : MW^2_bare = MW^2+dZMW2EW^2
  complex(REALKIND), save :: dZMZ2EW    = 0 ! Z mass RC                  : MZ^2_bare = MZ^2+dZMZ2EW^2
  complex(REALKIND), save :: dZMH2EW    = 0 ! H mass RC                  : MH^2_bare = MH^2+dZMH2EW^2
  complex(REALKIND), save :: dswEW      = 0 ! sin EW mixing angle RC         : sw_bare = sw + dswEW  i.e. dswEW/swEW = - c^2/s^2 dcwEW/c ; dcEW/c=1/2(dZMW2/MW^2-dZMZ2/MZ^2)
  complex(REALKIND), save :: dcwEW      = 0 ! cos EW mixing angle RC         : dcwEW/c=1/2(dZMW2/MW^2-dZMZ2/MZ^2) defined for convinience

!   complex(rp), save      :: dZqLEW     = 0 ! L-massless-quark field RC : Q_bare  = (1+1/2*dZqLEW)*Q_ren
!   complex(rp), save      :: dZqREW     = 0 ! R-massless-quark field RC : Q_bare  = (1+1/2*dZqREW)*Q_ren
  complex(REALKIND), save :: dZuLEW     = 0 ! L-massless-u-quark field RC : Q_bare  = (1+1/2*dZuLEW)*Q_ren
  complex(REALKIND), save :: dZuREW     = 0 ! R-massless-u-quark field RC : Q_bare  = (1+1/2*dZuREW)*Q_ren
  complex(REALKIND), save :: dZdLEW     = 0 ! L-massless-d-quark field RC : Q_bare  = (1+1/2*dZdLEW)*Q_ren
  complex(REALKIND), save :: dZdREW     = 0 ! R-massless-d-quark field RC : Q_bare  = (1+1/2*dZdREW)*Q_ren
  complex(REALKIND), save :: dZbLEW     = 0 ! L-bottom-quark field RC     : idem
  complex(REALKIND), save :: dZbREW     = 0 ! R-bottom-quark field RC     : idem
  complex(REALKIND), save :: dZtLEW     = 0 ! L-top-quark field RC        : idem
  complex(REALKIND), save :: dZtREW     = 0 ! R-top-quark field RC        : idem
  complex(REALKIND), save :: dZeLEW     = 0 ! L-lepton field RC           : idem
  complex(REALKIND), save :: dZeREW     = 0 ! R-lepton field RC           : idem
  complex(REALKIND), save :: dZnLEW     = 0 ! L-neutrino field RC         : idem
  complex(REALKIND), save :: dZLLEW     = 0 ! L-tau-lepton field RC       : idem
  complex(REALKIND), save :: dZLREW     = 0 ! R-tau-lepton field RC       : idem
  complex(REALKIND), save :: dZuLEWcc   = 0 ! L-massless-u-quark field RC : Q_bare  = (1+1/2*dZuLEW)*Q_ren
  complex(REALKIND), save :: dZuREWcc   = 0 ! R-massless-u-quark field RC : Q_bare  = (1+1/2*dZuREW)*Q_ren
  complex(REALKIND), save :: dZdLEWcc   = 0 ! L-massless-d-quark field RC : Q_bare  = (1+1/2*dZdLEW)*Q_ren
  complex(REALKIND), save :: dZdREWcc   = 0 ! R-massless-d-quark field RC : Q_bare  = (1+1/2*dZdREW)*Q_ren
  complex(REALKIND), save :: dZbLEWcc   = 0 ! L-bottom-quark field RC     : idem
  complex(REALKIND), save :: dZbREWcc   = 0 ! R-bottom-quark field RC     : idem
  complex(REALKIND), save :: dZtLEWcc   = 0 ! L-top-quark field RC        : idem
  complex(REALKIND), save :: dZtREWcc   = 0 ! R-top-quark field RC        : idem
  complex(REALKIND), save :: dZeLEWcc   = 0 ! L-lepton field RC           : idem
  complex(REALKIND), save :: dZeREWcc   = 0 ! R-lepton field RC           : idem
  complex(REALKIND), save :: dZnLEWcc   = 0 ! L-neutrino field RC         : idem
  complex(REALKIND), save :: dZLLEWcc   = 0 ! L-tau-lepton field RC       : idem
  complex(REALKIND), save :: dZLREWcc    = 0 ! R-tau-lepton field RC       : idem

  complex(REALKIND), save :: dZWEW      = 0 ! W field RC                 : idem
  complex(REALKIND), save :: dZZZEW     = 0 ! ZZ field RC                : idem
  complex(REALKIND), save :: dZAZEW     = 0 ! AZ field RC                : idem
  complex(REALKIND), save :: dZZAEW     = 0 ! AZ field RC                : idem
  complex(REALKIND), save :: dZAAEW     = 0  ! AA field RC                : idem
  complex(REALKIND), save :: dZHEW      = 0 ! H field RC                 : idem

  complex(REALKIND), save :: dtEW       = 0 ! tadpole-RC                 :
  complex(REALKIND), save :: dZeQEDEW   = 0 ! EW coupling RC         : e_bare  = (1+dZeEW)*e_ren

  ! Counter terms for EW corrections
  ! VV Vector propagators
  complex(REALKIND), save :: EWctWW(3)
  complex(REALKIND), save :: EWctZZ(3)
  complex(REALKIND), save :: EWctAZ(3)
  complex(REALKIND), save :: EWctAA(3)
  ! SS scalar propagators
  complex(REALKIND), save :: EWctHH(2)
  complex(REALKIND), save :: EWctXX(2)
  complex(REALKIND), save :: EWctPP(2)
  ! SV scalar-vector mixing
  complex(REALKIND), save :: EWctXA
  complex(REALKIND), save :: EWctXZ
  complex(REALKIND), save :: EWctPW
  ! FF fermionic propagators
  complex(REALKIND), save :: EWctuu(4)
  complex(REALKIND), save :: EWctdd(4)
  complex(REALKIND), save :: EWcttt(4)
  complex(REALKIND), save :: EWctbb(4)
  complex(REALKIND), save :: EWctee(4)
  complex(REALKIND), save :: EWctLL(4)
  complex(REALKIND), save :: EWctnn(4)
  !VVVV
  complex(REALKIND), save :: EWctWWWW(2)
  complex(REALKIND), save :: EWctWWZZ(2)
  complex(REALKIND), save :: EWctWWAZ(2)
  complex(REALKIND), save :: EWctWWAA(2)
  !VVVV pure R2
  complex(REALKIND), save :: EWctR2AAAA
  complex(REALKIND), save :: EWctR2AAAZ
  complex(REALKIND), save :: EWctR2AAZZ
  complex(REALKIND), save :: EWctR2AZZZ
  complex(REALKIND), save :: EWctR2ZZZZ
  !VVV
  complex(REALKIND), save :: EWctAWW
  complex(REALKIND), save :: EWctZWW
  !SSSS
  complex(REALKIND), save :: EWctSSSS1
  complex(REALKIND), save :: EWctSSSS2
  complex(REALKIND), save :: EWctSSSS3
  complex(REALKIND), save :: EWctHHHH
  complex(REALKIND), save :: EWctHHXX
  complex(REALKIND), save :: EWctHHPP
  complex(REALKIND), save :: EWctXXXX
  complex(REALKIND), save :: EWctXXPP
  complex(REALKIND), save :: EWctPPPP
  !SSS
  complex(REALKIND), save :: EWctHHH
  complex(REALKIND), save :: EWctHXX
  complex(REALKIND), save :: EWctHPP
  !VVSS
  complex(REALKIND), save :: EWctWWXX
  complex(REALKIND), save :: EWctWWHH
  complex(REALKIND), save :: EWctWWPP
  complex(REALKIND), save :: EWctZZPP
  complex(REALKIND), save :: EWctZAPP
  complex(REALKIND), save :: EWctAAPP
  complex(REALKIND), save :: EWctZZHH
  complex(REALKIND), save :: EWctZZXX
  complex(REALKIND), save :: EWctZAHH
  complex(REALKIND), save :: EWctWZPH
  complex(REALKIND), save :: EWctWAPH
  complex(REALKIND), save :: EWctWZPX
  complex(REALKIND), save :: EWctWAPX
  !VVSS R2
  complex(REALKIND), save :: EWctAAHH
  complex(REALKIND), save :: EWctAAXX
  !VSS
  complex(REALKIND), save :: EWctAXH
  complex(REALKIND), save :: EWctZXH
  complex(REALKIND), save :: EWctAPP
  complex(REALKIND), save :: EWctZPP
  complex(REALKIND), save :: EWctWPH
  complex(REALKIND), save :: EWctWPX
  !SVV
  complex(REALKIND), save :: EWctHWW
  complex(REALKIND), save :: EWctHZZ
  complex(REALKIND), save :: EWctHZA
  complex(REALKIND), save :: EWctPWZ
  complex(REALKIND), save :: EWctPWA
  ! pure R2 SVV
  complex(REALKIND), save :: EWctHAA
  !VFF
  ! Aff
  complex(REALKIND), save :: EWctAuu(2)
  complex(REALKIND), save :: EWctAdd(2)
  complex(REALKIND), save :: EWctAtt(2)
  complex(REALKIND), save :: EWctAbb(2)
  complex(REALKIND), save :: EWctAee(2)
  complex(REALKIND), save :: EWctALL(2)
  complex(REALKIND), save :: EWctAnn(2)
  ! Zff
  complex(REALKIND), save :: dgZu(2)
  complex(REALKIND), save :: dgZd(2)
  complex(REALKIND), save :: dgZl(2)
  complex(REALKIND), save :: dgZn(2)
  complex(REALKIND), save :: EWctVuu(2)
  complex(REALKIND), save :: EWctVdd(2)
  complex(REALKIND), save :: EWctVtt(2)
  complex(REALKIND), save :: EWctVbb(2)
  complex(REALKIND), save :: EWctVee(2)
  complex(REALKIND), save :: EWctVLL(2)
  complex(REALKIND), save :: EWctVnn(2)
  ! Wff
  complex(REALKIND), save :: EWctVdu
  complex(REALKIND), save :: EWctVbt
  complex(REALKIND), save :: EWctVen
  complex(REALKIND), save :: EWctVLn
  complex(REALKIND), save :: EWctVud
  complex(REALKIND), save :: EWctVtb
  complex(REALKIND), save :: EWctVne
  complex(REALKIND), save :: EWctVnL
  ! Gff mixed EW/QCD
  complex(REALKIND), save ::  EWctGuu(2)
  complex(REALKIND), save ::  EWctGdd(2)
  complex(REALKIND), save ::  EWctGtt(2)
  complex(REALKIND), save ::  EWctGbb(2)
  !SFF
  complex(REALKIND), save :: EWctHtt(2)
  complex(REALKIND), save :: EWctHbb(2)
  complex(REALKIND), save :: EWctHLL(2)
  complex(REALKIND), save :: EWctXtt(2)
  complex(REALKIND), save :: EWctXbb(2)
  complex(REALKIND), save :: EWctXLL(2)
  complex(REALKIND), save :: EWctPtb(2)
  complex(REALKIND), save :: EWctPbt(2)
  complex(REALKIND), save :: EWctPnL(2)
  complex(REALKIND), save :: EWctPLn(2)
  ! VUU
  complex(REALKIND), save :: EWctAUWUW
  complex(REALKIND), save :: EWctZUWUW
  complex(REALKIND), save :: EWctWUWUZ
  complex(REALKIND), save :: EWctWUZUW
  complex(REALKIND), save :: EWctWUWUA
  complex(REALKIND), save :: EWctWUAUW
  ! SUU
  complex(REALKIND), save :: EWctHUZUZ
  complex(REALKIND), save :: EWctHUWUW
  complex(REALKIND), save :: EWctXUWUW
  complex(REALKIND), save :: EWctPUZUW
  complex(REALKIND), save :: EWctPUWUZ
  complex(REALKIND), save :: EWctPUWUA

  ! Additional parameters for R2 EW
  complex(REALKIND), save :: sumMQ2
  complex(REALKIND), save :: sumMQ2Q2
  complex(REALKIND), save :: sumMQ2QI
  complex(REALKIND), save :: sumMQ4
  complex(REALKIND), save :: sumMUD
  complex(REALKIND), save :: sumMUD2
  complex(REALKIND), save :: sumMU2
  complex(REALKIND), save :: sumMD2
  complex(REALKIND), save :: sumMU4
  complex(REALKIND), save :: sumMD4
  complex(REALKIND), save :: sumMQ2QUD

end module ol_loop_parameters_decl_/**/REALKIND
