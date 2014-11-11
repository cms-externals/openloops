
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


! ol_dilog configuration
! stop series expansion when the result doesn't change anymore

! check expansion depth required to achieve full precision;
! store the number of points which required n terms in bin(n)
!#CHECK_DEPTH






module ol_dilog_qp
  use kind_types, only: qp
  implicit none
  real(qp), parameter :: pi2_6 = 8/3._qp*atan(1._qp)**2



  integer, parameter :: max_n = 29




  real(qp), parameter :: G4  = 6,         G6  = G4 * 4* 5, G8  = G6 * 6* 7, G10 = G8 * 8* 9, G12 = G10*10*11
  real(qp), parameter :: G14 = G12*12*13, G16 = G14*14*15, G18 = G16*16*17, G20 = G18*18*19, G22 = G20*20*21
  real(qp), parameter :: G24 = G22*22*23, G26 = G24*24*25, G28 = G26*26*27, G30 = G28*28*29, G32 = G30*30*31

  real(qp), parameter :: G34 = G32*32*33, G36 = G34*34*35, G38 = G36*36*37, G40 = G38*38*39, G42 = G40*40*41
  real(qp), parameter :: G44 = G42*42*43, G46 = G44*44*45, G48 = G46*46*47, G50 = G48*48*49, G52 = G50*50*51
  real(qp), parameter :: G54 = G52*52*53, G56 = G54*54*55, G58 = G56*56*57, G60 = G58*58*59, G62 = G60*60*61

  real(qp), parameter :: B2n(max_n) = [ & ! BernoulliB[2*n]/Gamma[2*n+2]
    &                                     1._qp / (      6 * G4 ) & !  1
    & ,                                  -1._qp / (     30 * G6 ) & !  2
    & ,                                   1._qp / (     42 * G8 ) & !  3
    & ,                                  -1._qp / (     30 * G10) & !  4
    & ,                                   5._qp / (     66 * G12) & !  5
    & ,                                -691._qp / (   2730 * G14) & !  6
    & ,                                   7._qp / (      6 * G16) & !  7
    & ,                               -3617._qp / (    510 * G18) & !  8
    & ,                               43867._qp / (    798 * G20) & !  9
    & ,                             -174611._qp / (    330 * G22) & ! 10
    & ,                              854513._qp / (    138 * G24) & ! 11
    & ,                          -236364091._qp / (   2730 * G26) & ! 12
    & ,                             8553103._qp / (      6 * G28) & ! 13
    & ,                        -23749461029._qp / (    870 * G30) & ! 14
    & ,                       8615841276005._qp / (  14322 * G32) & ! 15

    & ,                      -7709321041217._qp / (    510 * G34) & ! 16
    & ,                       2577687858367._qp / (      6 * G36) & ! 17
    & ,               -26315271553053477373._qp / (1919190 * G38) & ! 18
    & ,                    2929993913841559._qp / (      6 * G40) & ! 19
    & ,              -261082718496449122051._qp / (  13530 * G42) & ! 20
    & ,              1520097643918070802691._qp / (   1806 * G44) & ! 21
    & ,            -27833269579301024235023._qp / (    690 * G46) & ! 22
    & ,            596451111593912163277961._qp / (    282 * G48) & ! 23
    & ,       -5609403368997817686249127547._qp / (  46410 * G50) & ! 24
    & ,         495057205241079648212477525._qp / (     66 * G52) & ! 25
    & ,     -801165718135489957347924991853._qp / (   1590 * G54) & ! 26
    & ,    29149963634884862421418123812691._qp / (    798 * G56) & ! 27
    & , -2479392929313226753685415739663229._qp / (    870 * G58) & ! 28
    & , 84483613348880041862046775994036021._qp / (    354 * G60) & ! 29

  & ]

  contains

! **********************************************************************
function Li2conv(z)
! Complex dilogarithm Li2(z) calculated from a series expansion in terms of Bernoulli numbers.
! This is supposed to be used in the complex region |z| < 1 && Re(z) < 1/2.
! In this region the expansion (max_n = 29) is deep enough to achieve quadruple precision.
! Required expansion depths: sp:9, dp:13, ep:15, qp:24
! **********************************************************************
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in) :: z
  complex(qp) :: Li2conv
  complex(qp) :: Lz2, Lz2n1, Li2conv_next
  integer           :: n
  Lz2n1   = -log(1-z)
  Lz2     = Lz2n1*Lz2n1
  Li2conv = Lz2n1 - 0.25_qp*Lz2
  do n = 1, max_n
    Lz2n1 = Lz2n1 * Lz2
    Li2conv_next = Li2conv + Lz2n1 * B2n(n)

    if (Li2conv_next == Li2conv) exit

    Li2conv = Li2conv_next
  end do



end function Li2conv


! **********************************************************************
function Li2(z)
! Complex dilogarithm Li2(z)
! Map z to the complex region |z| < 1 && Re(z) < 1/2 and call the series expansion.
! **********************************************************************
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in) :: z
  complex(qp) :: Li2
  if (real(z) <= 0.5) then
    if (abs(z) <= 1) then
      Li2 =   Li2conv(z)
    else
      Li2 = - Li2conv(1/z)     -   pi2_6                   - .5_qp * log( -z)**2
    end if
  else
    if (abs(1-z) <= 1) then
      Li2 = - Li2conv(1-z)     +   pi2_6 - log(z)*log(1-z)
    else
      Li2 =   Li2conv(1/(1-z)) + 2*pi2_6 - log(z)*log(1-z) + .5_qp * log(z-1)**2
    end if
  end if
end function Li2

end module ol_dilog_qp

