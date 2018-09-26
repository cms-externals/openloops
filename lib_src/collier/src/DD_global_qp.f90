












!!
!!  File DD_global.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!
!!----------------------------------------------------------------------





module dd_global_qp
  integer, parameter            :: dp = selected_real_kind(15)
  integer, parameter            :: qp = selected_real_kind(33)
  integer, parameter            :: rk = qp
  integer                       :: rmax2,rmax3,rmax4,rmax5,rmax6,nmax,rmax
  integer                       :: r2max2,r2max3,r2max4,r2max5,r2max6
  integer (kind=8)              :: Ncoefmax2,Ncoefmax3,Ncoefmax4,Ncoefmax5,Ncoefmax6
  integer (kind=8)              :: Ncoefmax3_int,Ncoefmax4_int
  integer,allocatable,dimension(:) :: tid,ntid
  integer (kind=8),allocatable,dimension(:,:) :: BinC

  real(rk)                      :: cacc,dacc
  integer                       :: mode34,mode5,mode6,ritmax
  integer                       :: outlevel,outchannel,cout,coutmax
  logical                       :: cout_on

  real(rk)        , allocatable, dimension(:)         :: aimacc,erracc
  real(rk)        , allocatable, dimension(:,:)       :: resaccabs,resaccrel,resaccabs2,resaccrel2
  integer                       :: nmaster,r2master,accflag,errflag,stopflag

  real(rk)                      :: deltauv,muv2,delta2ir,delta1ir,mir2,mx2(100)

  complex(rk)     , allocatable, dimension(:)         :: scalint,scalintnew
  real(rk)        , allocatable, dimension(:)         :: scalint_err
  complex(rk)     , allocatable, dimension(:,:)       :: auxc
  real(rk)        , allocatable, dimension(:,:)       :: auxr
  real(rk)        , allocatable, dimension(:)         :: acc_pave,acc_new
  real(rk)        , allocatable, dimension(:,:)       :: accr2_aux,accr2_newprelim,accr2_new_aux
  real(rk)        , allocatable, dimension(:)         :: maxtxij,maxttx0klm
  real(rk)        , allocatable, dimension(:)         :: maxttx0ijm,maxtz_nj,maxttz_knlm,ttzff_kl
  integer, allocatable,dimension(:,:) :: auxi
  integer, allocatable,dimension(:)   :: r2_aux,r2_new_aux,r2_newprelim,qmethod,qmethod_new
  integer                             :: nc_DDin,nr_DDin,ni_DDin,i_DDin(100)
  complex(rk)                         :: c_DDin(100)
  real(rk)                            :: r_DDin(100)
  character(len=20)                   :: s_DDin

  real(rk)                            :: dprec_dd

  interface acmplx
    module procedure acmplx1, acmplx2, acmplxc
  end interface acmplx

  contains

function acmplx1(r)
  implicit none
  complex(rk) :: acmplx1
  real(rk), intent(in) :: r
  acmplx1 = r
end function acmplx1

function acmplx2(r,i)
  implicit none
  complex(rk) :: acmplx2
  real(rk), intent(in) :: r, i
  acmplx2 = cmplx(r, i, kind=rk)
end function acmplx2

function acmplxc(c)
  implicit none
  complex(rk) :: acmplxc
  complex(rk), intent(in) :: c
  acmplxc = c
end function acmplxc

end module dd_global_qp


! Li2 function with quadruple precision support (#define 1)
! from OpenLoops to replace the ancient cspen_dd function.
! Added 02.03.2018, Philipp Maierhoefer
module dd_dilog_qp
  use dd_global_qp, only: rk
  implicit none
  real(rk), parameter :: pi2_6 = 8/3._rk*atan(1._rk)**2
  integer, parameter :: max_n = 29
  real(rk), parameter :: G4  = 6,         G6  = G4 * 4* 5, G8  = G6 * 6* 7, G10 = G8 * 8* 9, G12 = G10*10*11
  real(rk), parameter :: G14 = G12*12*13, G16 = G14*14*15, G18 = G16*16*17, G20 = G18*18*19, G22 = G20*20*21
  real(rk), parameter :: G24 = G22*22*23, G26 = G24*24*25, G28 = G26*26*27, G30 = G28*28*29, G32 = G30*30*31
  real(rk), parameter :: G34 = G32*32*33, G36 = G34*34*35, G38 = G36*36*37, G40 = G38*38*39, G42 = G40*40*41
  real(rk), parameter :: G44 = G42*42*43, G46 = G44*44*45, G48 = G46*46*47, G50 = G48*48*49, G52 = G50*50*51
  real(rk), parameter :: G54 = G52*52*53, G56 = G54*54*55, G58 = G56*56*57, G60 = G58*58*59, G62 = G60*60*61
  real(rk), parameter :: B2n(max_n) = [ & ! BernoulliB[2*n]/Gamma[2*n+2]
    &                                     1._rk / (      6 * G4 ) & !  1
    & ,                                  -1._rk / (     30 * G6 ) & !  2
    & ,                                   1._rk / (     42 * G8 ) & !  3
    & ,                                  -1._rk / (     30 * G10) & !  4
    & ,                                   5._rk / (     66 * G12) & !  5
    & ,                                -691._rk / (   2730 * G14) & !  6
    & ,                                   7._rk / (      6 * G16) & !  7
    & ,                               -3617._rk / (    510 * G18) & !  8
    & ,                               43867._rk / (    798 * G20) & !  9
    & ,                             -174611._rk / (    330 * G22) & ! 10
    & ,                              854513._rk / (    138 * G24) & ! 11
    & ,                          -236364091._rk / (   2730 * G26) & ! 12
    & ,                             8553103._rk / (      6 * G28) & ! 13
    & ,                        -23749461029._rk / (    870 * G30) & ! 14
    & ,                       8615841276005._rk / (  14322 * G32) & ! 15
    & ,                      -7709321041217._rk / (    510 * G34) & ! 16
    & ,                       2577687858367._rk / (      6 * G36) & ! 17
    & ,               -26315271553053477373._rk / (1919190 * G38) & ! 18
    & ,                    2929993913841559._rk / (      6 * G40) & ! 19
    & ,              -261082718496449122051._rk / (  13530 * G42) & ! 20
    & ,              1520097643918070802691._rk / (   1806 * G44) & ! 21
    & ,            -27833269579301024235023._rk / (    690 * G46) & ! 22
    & ,            596451111593912163277961._rk / (    282 * G48) & ! 23
    & ,       -5609403368997817686249127547._rk / (  46410 * G50) & ! 24
    & ,         495057205241079648212477525._rk / (     66 * G52) & ! 25
    & ,     -801165718135489957347924991853._rk / (   1590 * G54) & ! 26
    & ,    29149963634884862421418123812691._rk / (    798 * G56) & ! 27
    & , -2479392929313226753685415739663229._rk / (    870 * G58) & ! 28
    & , 84483613348880041862046775994036021._rk / (    354 * G60) & ! 29
  & ]

  contains

  ! **********************************************************************
  function Li2conv(z)
  ! Complex dilogarithm Li2(z) calculated from a series expansion in terms of Bernoulli numbers.
  ! This is supposed to be used in the complex region |z| < 1 && Re(z) < 1/2.
  ! In this region the expansion (max_n = 29) is deep enough to achieve quadruple precision.
  ! Required expansion depths: sp:9, dp:13, ep:15, qp:24
  ! **********************************************************************
    implicit none
    complex(rk), intent(in) :: z
    complex(rk) :: Li2conv
    complex(rk) :: Lz2, Lz2n1, Li2conv_next
    integer           :: n
    Lz2n1   = -log(1-z)
    Lz2     = Lz2n1*Lz2n1
    Li2conv = Lz2n1 - 0.25_rk*Lz2
    do n = 1, max_n
      Lz2n1 = Lz2n1 * Lz2
      Li2conv_next = Li2conv + Lz2n1 * B2n(n)
      Li2conv = Li2conv_next
    end do
  end function Li2conv


  ! **********************************************************************
  function Li2(z)
  ! Complex dilogarithm Li2(z)
  ! Map z to the complex region |z| < 1 && Re(z) < 1/2 and call the series expansion.
  ! **********************************************************************
    implicit none
    complex(rk), intent(in) :: z
    complex(rk) :: Li2
    if (real(z) <= 0.5) then
      if (abs(z) <= 1) then
        Li2 =   Li2conv(z)
      else
        Li2 = - Li2conv(1/z)     -   pi2_6                   - .5_rk * log( -z)**2
      end if
    else
      if (abs(1-z) <= 1) then
        Li2 = - Li2conv(1-z)     +   pi2_6 - log(z)*log(1-z)
      else
        Li2 =   Li2conv(1/(1-z)) + 2*pi2_6 - log(z)*log(1-z) + .5_rk * log(z-1)**2
      end if
    end if
  end function Li2

end module dd_dilog_qp


module MODULE_DD_STATISTICS
  integer :: dpv_calc_dd,dpv_ok_dd
  integer :: dapv_calc_dd,dapv_ok_dd,dg_calc_dd,dg_ok_dd
  integer :: dg2_calc_dd,dg2_ok_dd,dgc_calc_dd,dgc_ok_dd
  integer :: d_bad_dd
end module MODULE_DD_STATISTICS
