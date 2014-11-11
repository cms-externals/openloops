
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


module kind_types
  implicit none

  !!! THIS DOES NOT BELONG HERE !!!
  integer, parameter :: MaxParticles = 9
  !!! THIS DOES NOT BELONG HERE !!!

  integer, parameter :: intkind1 = selected_int_kind(2)
  integer, parameter :: intkind2 = selected_int_kind(4)




! #ifdef 1
  integer, parameter :: dp = selected_real_kind(15)
  complex(dp), parameter :: cI_dp = (0._dp, 1._dp)
! #endif




! #ifdef 1
  integer, parameter :: qp = selected_real_kind(33)
  complex(qp), parameter :: cI_qp = (0._qp, 1._qp)
! #endif

  interface cmplx_wp




    module procedure cmplx_dp





    module procedure cmplx_qp

  end interface cmplx_wp
  contains

  function cmplx_dp(re, im)
    implicit none
    real(dp), intent(in) :: re
    real(dp), intent(in), optional :: im
    complex(dp) :: cmplx_dp
    cmplx_dp = re
    if (present(im)) cmplx_dp = cmplx(re,im,dp)
  end function cmplx_dp

  function cmplx_qp(re, im)
    implicit none
    real(qp), intent(in) :: re
    real(qp), intent(in), optional :: im
    complex(qp) :: cmplx_qp
    cmplx_qp = re
    if (present(im)) cmplx_qp = cmplx_qp + im * cI_qp
  end function cmplx_qp

end module kind_types

