
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


module ol_Std2LC_converter_dp
  implicit none
  contains

! **********************************************************************
subroutine Std_Contr2LC_Tensor(T_Lor,T_LC)
! convert tensor integrals
! T_Lor = Lorentz contravariant -> T_LC = Light-cone contravariant
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: T_Lor(:)
  complex(dp), intent(out) :: T_LC(:)
  integer :: length

  length = size(T_Lor)
  select case (length)

    case (1)
      call Std2LC_Tensor0(T_Lor,T_LC)

    case (5)
      call Std2LC_Tensor1(T_Lor,T_LC)

    case (15)
      call Std2LC_Tensor2(T_Lor,T_LC)

    case (35)
      call Std2LC_Tensor3(T_Lor,T_LC)

    case (70)
      call Std2LC_Tensor4(T_Lor,T_LC)

    case (126)
      call Std2LC_Tensor5(T_Lor,T_LC)

    case (210)
      call Std2LC_Tensor6(T_Lor,T_LC)

  end select
end subroutine Std_Contr2LC_Tensor


! **********************************************************************
subroutine Std2LC_Tensor0(T_Lor,T_LC)
! Lorentz contravariant -> Light-cone contravariant
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: T_Lor(1)
  complex(dp), intent(out) :: T_LC(1)

  T_LC = T_Lor

end subroutine Std2LC_Tensor0


! **********************************************************************
subroutine Std2LC_Tensor1(T_Lor,T_LC)
! Lorentz contravariant -> Light-cone contravariant
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: T_Lor(5)
  complex(dp), intent(out) :: T_LC(5)

  T_LC(1) = T_Lor(1)
  T_LC(2) = T_Lor(2) - T_Lor(5)
  T_LC(3) = T_Lor(2) + T_Lor(5)
  T_LC(4) = -T_Lor(3) - (0._dp,1._dp)* T_Lor(4)
  T_LC(5) = -T_Lor(3) + (0._dp,1._dp)* T_Lor(4)

end subroutine Std2LC_Tensor1
! **********************************************************************


subroutine Std2LC_Tensor2(T_Lor,T_LC)
! Lorentz contravariant -> Light-cone contravariant
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: T_Lor(15)
  complex(dp), intent(out) :: T_LC(15)

  T_LC(1) = T_Lor(1)
  T_LC(2) = T_Lor(2) - T_Lor(5)
  T_LC(3) = T_Lor(2) + T_Lor(5)
  T_LC(4) = -T_Lor(3) - (0._dp,1._dp)* T_Lor(4)
  T_LC(5) = -T_Lor(3) + (0._dp,1._dp)* T_Lor(4)
  T_LC(6) = T_Lor(15) + T_Lor(6) - 2._dp * T_Lor(9)
  T_LC(7) = -T_Lor(15) + T_Lor(6)
  T_LC(8) = T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (T_Lor(14) - T_Lor(8))
  T_LC(9) = T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (-T_Lor(14) + T_Lor(8))
  T_LC(10) = T_Lor(15) + T_Lor(6) + 2._dp * T_Lor(9)
  T_LC(11) = -T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (-T_Lor(14) - T_Lor(8))
  T_LC(12) = -T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (T_Lor(14) + T_Lor(8))
  T_LC(13) = T_Lor(10) + (0._dp,2._dp)* T_Lor(11) - T_Lor(13)
  T_LC(14) = T_Lor(10) + T_Lor(13)
  T_LC(15) = T_Lor(10) - (0._dp,2._dp)* T_Lor(11) - T_Lor(13)

end subroutine Std2LC_Tensor2


! **********************************************************************
subroutine Std2LC_Tensor3(T_Lor,T_LC)
! Lorentz contravariant -> Light-cone contravariant
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: T_Lor(35)
  complex(dp), intent(out) :: T_LC(35)

  T_LC(1) = T_Lor(1)
  T_LC(2) = T_Lor(2) - T_Lor(5)
  T_LC(3) = T_Lor(2) + T_Lor(5)
  T_LC(4) = -T_Lor(3) - (0._dp,1._dp)* T_Lor(4)
  T_LC(5) = -T_Lor(3) + (0._dp,1._dp)* T_Lor(4)
  T_LC(6) = T_Lor(15) + T_Lor(6) - 2._dp * T_Lor(9)
  T_LC(7) = -T_Lor(15) + T_Lor(6)
  T_LC(8) = T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (T_Lor(14) - T_Lor(8))
  T_LC(9) = T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (-T_Lor(14) + T_Lor(8))
  T_LC(10) = T_Lor(15) + T_Lor(6) + 2._dp * T_Lor(9)
  T_LC(11) = -T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (-T_Lor(14) - T_Lor(8))
  T_LC(12) = -T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (T_Lor(14) + T_Lor(8))
  T_LC(13) = T_Lor(10) + (0._dp,2._dp)* T_Lor(11) - T_Lor(13)
  T_LC(14) = T_Lor(10) + T_Lor(13)
  T_LC(15) = T_Lor(10) - (0._dp,2._dp)* T_Lor(11) - T_Lor(13)
  T_LC(16) = T_Lor(16) + 3._dp * (-T_Lor(19) + T_Lor(25)) - T_Lor(35)
  T_LC(17) = T_Lor(16) - T_Lor(19) - T_Lor(25) + T_Lor(35)
  T_LC(18) = -T_Lor(17) + 2._dp * T_Lor(22) + (0._dp,2._dp)* T_Lor(24) - T_Lor(31) + &
             (0._dp,1._dp)* (-T_Lor(18) - T_Lor(34))
  T_LC(19) = -T_Lor(17) + 2._dp * T_Lor(22) - (0._dp,2._dp)* T_Lor(24) - T_Lor(31) + &
             (0._dp,1._dp)* (T_Lor(18) + T_Lor(34))
  T_LC(20) = T_Lor(16) + T_Lor(19) - T_Lor(25) - T_Lor(35)
  T_LC(21) = -T_Lor(17) + T_Lor(31) + (0._dp,1._dp)* (-T_Lor(18) + T_Lor(34))
  T_LC(22) = -T_Lor(17) + T_Lor(31) + (0._dp,1._dp)* (T_Lor(18) - T_Lor(34))
  T_LC(23) = T_Lor(20) - T_Lor(23) - T_Lor(28) + (0._dp,2._dp)* (T_Lor(21) - T_Lor(30)) + T_Lor(33)
  T_LC(24) = T_Lor(20) + T_Lor(23) - T_Lor(28) - T_Lor(33)
  T_LC(25) = T_Lor(20) - T_Lor(23) - T_Lor(28) + (0._dp,2._dp)* (-T_Lor(21) + T_Lor(30)) + T_Lor(33)
  T_LC(26) = T_Lor(16) + 3._dp * (T_Lor(19) + T_Lor(25)) + T_Lor(35)
  T_LC(27) = -T_Lor(17) - 2._dp * T_Lor(22) - (0._dp,2._dp)* T_Lor(24) - T_Lor(31) + &
             (0._dp,1._dp)* (-T_Lor(18) - T_Lor(34))
  T_LC(28) = -T_Lor(17) - 2._dp * T_Lor(22) + (0._dp,2._dp)* T_Lor(24) - T_Lor(31) + &
             (0._dp,1._dp)* (T_Lor(18) + T_Lor(34))
  T_LC(29) = T_Lor(20) - T_Lor(23) + T_Lor(28) + (0._dp,2._dp)* (T_Lor(21) + T_Lor(30)) - T_Lor(33)
  T_LC(30) = T_Lor(20) + T_Lor(23) + T_Lor(28) + T_Lor(33)
  T_LC(31) = T_Lor(20) - T_Lor(23) + T_Lor(28) + (0._dp,2._dp)* (-T_Lor(21) - T_Lor(30)) - T_Lor(33)
  T_LC(32) = -T_Lor(26) - (0._dp,3._dp)* T_Lor(27) + 3._dp * T_Lor(29) + (0._dp,1._dp)* T_Lor(32)
  T_LC(33) = -T_Lor(26) - T_Lor(29) + (0._dp,1._dp)* (-T_Lor(27) - T_Lor(32))
  T_LC(34) = -T_Lor(26) - T_Lor(29) + (0._dp,1._dp)* (T_Lor(27) + T_Lor(32))
  T_LC(35) = -T_Lor(26) + (0._dp,3._dp)* T_Lor(27) + 3._dp * T_Lor(29) - (0._dp,1._dp)* T_Lor(32)

end subroutine Std2LC_Tensor3


! **********************************************************************
subroutine Std2LC_Tensor4(T_Lor,T_LC)
! Lorentz contravariant -> Light-cone contravariant
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: T_Lor(70)
  complex(dp), intent(out) :: T_LC(70)

  T_LC(1) = T_Lor(1)
  T_LC(2) = T_Lor(2) - T_Lor(5)
  T_LC(3) = T_Lor(2) + T_Lor(5)
  T_LC(4) = -T_Lor(3) - (0._dp,1._dp)* T_Lor(4)
  T_LC(5) = -T_Lor(3) + (0._dp,1._dp)* T_Lor(4)
  T_LC(6) = T_Lor(15) + T_Lor(6) - 2._dp * T_Lor(9)
  T_LC(7) = -T_Lor(15) + T_Lor(6)
  T_LC(8) = T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (T_Lor(14) - T_Lor(8))
  T_LC(9) = T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (-T_Lor(14) + T_Lor(8))
  T_LC(10) = T_Lor(15) + T_Lor(6) + 2._dp * T_Lor(9)
  T_LC(11) = -T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (-T_Lor(14) - T_Lor(8))
  T_LC(12) = -T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (T_Lor(14) + T_Lor(8))
  T_LC(13) = T_Lor(10) + (0._dp,2._dp)* T_Lor(11) - T_Lor(13)
  T_LC(14) = T_Lor(10) + T_Lor(13)
  T_LC(15) = T_Lor(10) - (0._dp,2._dp)* T_Lor(11) - T_Lor(13)
  T_LC(16) = T_Lor(16) + 3._dp * (-T_Lor(19) + T_Lor(25)) - T_Lor(35)
  T_LC(17) = T_Lor(16) - T_Lor(19) - T_Lor(25) + T_Lor(35)
  T_LC(18) = -T_Lor(17) + 2._dp * T_Lor(22) + (0._dp,2._dp)* T_Lor(24) - T_Lor(31) + &
             (0._dp,1._dp)* (-T_Lor(18) - T_Lor(34))
  T_LC(19) = -T_Lor(17) + 2._dp * T_Lor(22) - (0._dp,2._dp)* T_Lor(24) - T_Lor(31) + &
             (0._dp,1._dp)* (T_Lor(18) + T_Lor(34))
  T_LC(20) = T_Lor(16) + T_Lor(19) - T_Lor(25) - T_Lor(35)
  T_LC(21) = -T_Lor(17) + T_Lor(31) + (0._dp,1._dp)* (-T_Lor(18) + T_Lor(34))
  T_LC(22) = -T_Lor(17) + T_Lor(31) + (0._dp,1._dp)* (T_Lor(18) - T_Lor(34))
  T_LC(23) = T_Lor(20) - T_Lor(23) - T_Lor(28) + (0._dp,2._dp)* (T_Lor(21) - T_Lor(30)) + T_Lor(33)
  T_LC(24) = T_Lor(20) + T_Lor(23) - T_Lor(28) - T_Lor(33)
  T_LC(25) = T_Lor(20) - T_Lor(23) - T_Lor(28) + (0._dp,2._dp)* (-T_Lor(21) + T_Lor(30)) + T_Lor(33)
  T_LC(26) = T_Lor(16) + 3._dp * (T_Lor(19) + T_Lor(25)) + T_Lor(35)
  T_LC(27) = -T_Lor(17) - 2._dp * T_Lor(22) - (0._dp,2._dp)* T_Lor(24) - T_Lor(31) + &
             (0._dp,1._dp)* (-T_Lor(18) - T_Lor(34))
  T_LC(28) = -T_Lor(17) - 2._dp * T_Lor(22) + (0._dp,2._dp)* T_Lor(24) - T_Lor(31) + &
             (0._dp,1._dp)* (T_Lor(18) + T_Lor(34))
  T_LC(29) = T_Lor(20) - T_Lor(23) + T_Lor(28) + (0._dp,2._dp)* (T_Lor(21) + T_Lor(30)) - T_Lor(33)
  T_LC(30) = T_Lor(20) + T_Lor(23) + T_Lor(28) + T_Lor(33)
  T_LC(31) = T_Lor(20) - T_Lor(23) + T_Lor(28) + (0._dp,2._dp)* (-T_Lor(21) - T_Lor(30)) - T_Lor(33)
  T_LC(32) = -T_Lor(26) - (0._dp,3._dp)* T_Lor(27) + 3._dp * T_Lor(29) + (0._dp,1._dp)* T_Lor(32)
  T_LC(33) = -T_Lor(26) - T_Lor(29) + (0._dp,1._dp)* (-T_Lor(27) - T_Lor(32))
  T_LC(34) = -T_Lor(26) - T_Lor(29) + (0._dp,1._dp)* (T_Lor(27) + T_Lor(32))
  T_LC(35) = -T_Lor(26) + (0._dp,3._dp)* T_Lor(27) + 3._dp * T_Lor(29) - (0._dp,1._dp)* T_Lor(32)
  T_LC(36) = T_Lor(36) + 6._dp * T_Lor(45) + 4._dp * (-T_Lor(39) - T_Lor(55)) + T_Lor(70)
  T_LC(37) = T_Lor(36) + 2._dp * (-T_Lor(39) + T_Lor(55)) - T_Lor(70)
  T_LC(38) = -T_Lor(37) + 3._dp * (T_Lor(42) - T_Lor(51)) + (0._dp,3._dp)* (T_Lor(44) - T_Lor(54)) + &
             T_Lor(65) + (0._dp,1._dp)* (-T_Lor(38) + T_Lor(69))
  T_LC(39) = -T_Lor(37) + 3._dp * (T_Lor(42) - T_Lor(51)) + (0._dp,3._dp)* (-T_Lor(44) + T_Lor(54)) + &
             T_Lor(65) + (0._dp,1._dp)* (T_Lor(38) - T_Lor(69))
  T_LC(40) = T_Lor(36) - 2._dp * T_Lor(45) + T_Lor(70)
  T_LC(41) = -T_Lor(37) + T_Lor(42) + T_Lor(51) - T_Lor(65) + (0._dp,1._dp)* (-T_Lor(38) + T_Lor(44) + T_Lor(54) - T_Lor(69))
  T_LC(42) = -T_Lor(37) + T_Lor(42) + T_Lor(51) - T_Lor(65) + (0._dp,1._dp)* (T_Lor(38) - T_Lor(44) - T_Lor(54) + T_Lor(69))
  T_LC(43) = T_Lor(40) - T_Lor(43) - (0._dp,4._dp)* T_Lor(50) + 2._dp * (-T_Lor(48) + T_Lor(53)) + T_Lor(61) + &
             (0._dp,2._dp)* (T_Lor(41) + T_Lor(64)) - T_Lor(68)
  T_LC(44) = T_Lor(40) + T_Lor(43) + 2._dp * (-T_Lor(48) - T_Lor(53)) + T_Lor(61) + T_Lor(68)
  T_LC(45) = T_Lor(40) - T_Lor(43) + (0._dp,4._dp)* T_Lor(50) + 2._dp * (-T_Lor(48) + T_Lor(53)) + T_Lor(61) + &
             (0._dp,2._dp)* (-T_Lor(41) - T_Lor(64)) - T_Lor(68)
  T_LC(46) = T_Lor(36) + 2._dp * (T_Lor(39) - T_Lor(55)) - T_Lor(70)
  T_LC(47) = -T_Lor(37) - T_Lor(42) + T_Lor(51) + T_Lor(65) + (0._dp,1._dp)* (-T_Lor(38) - T_Lor(44) + T_Lor(54) + T_Lor(69))
  T_LC(48) = -T_Lor(37) - T_Lor(42) + T_Lor(51) + T_Lor(65) + (0._dp,1._dp)* (T_Lor(38) + T_Lor(44) - T_Lor(54) - T_Lor(69))
  T_LC(49) = T_Lor(40) - T_Lor(43) - T_Lor(61) + (0._dp,2._dp)* (T_Lor(41) - T_Lor(64)) + T_Lor(68)
  T_LC(50) = T_Lor(40) + T_Lor(43) - T_Lor(61) - T_Lor(68)
  T_LC(51) = T_Lor(40) - T_Lor(43) - T_Lor(61) + (0._dp,2._dp)* (-T_Lor(41) + T_Lor(64)) + T_Lor(68)
  T_LC(52) = -T_Lor(46) + T_Lor(58) + (0._dp,3._dp)* (-T_Lor(47) + T_Lor(60)) + 3._dp * (T_Lor(49) - T_Lor(63)) + &
             (0._dp,1._dp)* (T_Lor(52) - T_Lor(67))
  T_LC(53) = -T_Lor(46) - T_Lor(49) + T_Lor(58) + T_Lor(63) + (0._dp,1._dp)* (-T_Lor(47) - T_Lor(52) + T_Lor(60) + T_Lor(67))
  T_LC(54) = -T_Lor(46) - T_Lor(49) + T_Lor(58) + T_Lor(63) + (0._dp,1._dp)* (T_Lor(47) + T_Lor(52) - T_Lor(60) - T_Lor(67))
  T_LC(55) = -T_Lor(46) + T_Lor(58) + (0._dp,3._dp)* (T_Lor(47) - T_Lor(60)) + 3._dp * (T_Lor(49) - T_Lor(63)) + &
             (0._dp,1._dp)* (-T_Lor(52) + T_Lor(67))
  T_LC(56) = T_Lor(36) + 6._dp * T_Lor(45) + 4._dp * (T_Lor(39) + T_Lor(55)) + T_Lor(70)
  T_LC(57) = -T_Lor(37) + 3._dp * (-T_Lor(42) - T_Lor(51)) + (0._dp,3._dp)* (-T_Lor(44) - T_Lor(54)) - T_Lor(65) + &
             (0._dp,1._dp)* (-T_Lor(38) - T_Lor(69))
  T_LC(58) = -T_Lor(37) + 3._dp * (-T_Lor(42) - T_Lor(51)) + (0._dp,3._dp)* (T_Lor(44) + T_Lor(54)) - T_Lor(65) + &
             (0._dp,1._dp)* (T_Lor(38) + T_Lor(69))
  T_LC(59) = T_Lor(40) - T_Lor(43) + (0._dp,4._dp)* T_Lor(50) + 2._dp * (T_Lor(48) - T_Lor(53)) + T_Lor(61) + &
             (0._dp,2._dp)* (T_Lor(41) + T_Lor(64)) - T_Lor(68)
  T_LC(60) = T_Lor(40) + T_Lor(43) + 2._dp * (T_Lor(48) + T_Lor(53)) + T_Lor(61) + T_Lor(68)
  T_LC(61) = T_Lor(40) - T_Lor(43) - (0._dp,4._dp)* T_Lor(50) + 2._dp * (T_Lor(48) - T_Lor(53)) + T_Lor(61) + &
             (0._dp,2._dp)* (-T_Lor(41) - T_Lor(64)) - T_Lor(68)
  T_LC(62) = -T_Lor(46) - T_Lor(58) + (0._dp,3._dp)* (-T_Lor(47) - T_Lor(60)) + 3._dp * (T_Lor(49) + T_Lor(63)) + &
             (0._dp,1._dp)* (T_Lor(52) + T_Lor(67))
  T_LC(63) = -T_Lor(46) - T_Lor(49) - T_Lor(58) - T_Lor(63) + (0._dp,1._dp)* (-T_Lor(47) - T_Lor(52) - T_Lor(60) - T_Lor(67))
  T_LC(64) = -T_Lor(46) - T_Lor(49) - T_Lor(58) - T_Lor(63) + (0._dp,1._dp)* (T_Lor(47) + T_Lor(52) + T_Lor(60) + T_Lor(67))
  T_LC(65) = -T_Lor(46) - T_Lor(58) + (0._dp,3._dp)* (T_Lor(47) + T_Lor(60)) + 3._dp * (T_Lor(49) + T_Lor(63)) + &
             (0._dp,1._dp)* (-T_Lor(52) - T_Lor(67))
  T_LC(66) = T_Lor(56) - 6._dp * T_Lor(59) + (0._dp,4._dp)* (T_Lor(57) - T_Lor(62)) + T_Lor(66)
  T_LC(67) = T_Lor(56) + (0._dp,2._dp)* (T_Lor(57) + T_Lor(62)) - T_Lor(66)
  T_LC(68) = T_Lor(56) + 2._dp * T_Lor(59) + T_Lor(66)
  T_LC(69) = T_Lor(56) + (0._dp,2._dp)* (-T_Lor(57) - T_Lor(62)) - T_Lor(66)
  T_LC(70) = T_Lor(56) - 6._dp * T_Lor(59) + (0._dp,4._dp)* (-T_Lor(57) + T_Lor(62)) + T_Lor(66)

end subroutine Std2LC_Tensor4


! **********************************************************************
subroutine Std2LC_Tensor5(T_Lor,T_LC)
! Lorentz contravariant -> Light-cone contravariant
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: T_Lor(126)
  complex(dp), intent(out) :: T_LC(126)

  T_LC(1) = T_Lor(1)
  T_LC(2) = T_Lor(2) - T_Lor(5)
  T_LC(3) = T_Lor(2) + T_Lor(5)
  T_LC(4) = -T_Lor(3) - (0._dp,1._dp)* T_Lor(4)
  T_LC(5) = -T_Lor(3) + (0._dp,1._dp)* T_Lor(4)
  T_LC(6) = T_Lor(15) + T_Lor(6) - 2._dp * T_Lor(9)
  T_LC(7) = -T_Lor(15) + T_Lor(6)
  T_LC(8) = T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (T_Lor(14) - T_Lor(8))
  T_LC(9) = T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (-T_Lor(14) + T_Lor(8))
  T_LC(10) = T_Lor(15) + T_Lor(6) + 2._dp * T_Lor(9)
  T_LC(11) = -T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (-T_Lor(14) - T_Lor(8))
  T_LC(12) = -T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (T_Lor(14) + T_Lor(8))
  T_LC(13) = T_Lor(10) + (0._dp,2._dp)* T_Lor(11) - T_Lor(13)
  T_LC(14) = T_Lor(10) + T_Lor(13)
  T_LC(15) = T_Lor(10) - (0._dp,2._dp)* T_Lor(11) - T_Lor(13)
  T_LC(16) = T_Lor(16) + 3._dp * (-T_Lor(19) + T_Lor(25)) - T_Lor(35)
  T_LC(17) = T_Lor(16) - T_Lor(19) - T_Lor(25) + T_Lor(35)
  T_LC(18) = -T_Lor(17) + 2._dp * T_Lor(22) + (0._dp,2._dp)* T_Lor(24) - T_Lor(31) + &
             (0._dp,1._dp)* (-T_Lor(18) - T_Lor(34))
  T_LC(19) = -T_Lor(17) + 2._dp * T_Lor(22) - (0._dp,2._dp)* T_Lor(24) - T_Lor(31) + &
             (0._dp,1._dp)* (T_Lor(18) + T_Lor(34))
  T_LC(20) = T_Lor(16) + T_Lor(19) - T_Lor(25) - T_Lor(35)
  T_LC(21) = -T_Lor(17) + T_Lor(31) + (0._dp,1._dp)* (-T_Lor(18) + T_Lor(34))
  T_LC(22) = -T_Lor(17) + T_Lor(31) + (0._dp,1._dp)* (T_Lor(18) - T_Lor(34))
  T_LC(23) = T_Lor(20) - T_Lor(23) - T_Lor(28) + (0._dp,2._dp)* (T_Lor(21) - T_Lor(30)) + T_Lor(33)
  T_LC(24) = T_Lor(20) + T_Lor(23) - T_Lor(28) - T_Lor(33)
  T_LC(25) = T_Lor(20) - T_Lor(23) - T_Lor(28) + (0._dp,2._dp)* (-T_Lor(21) + T_Lor(30)) + T_Lor(33)
  T_LC(26) = T_Lor(16) + 3._dp * (T_Lor(19) + T_Lor(25)) + T_Lor(35)
  T_LC(27) = -T_Lor(17) - 2._dp * T_Lor(22) - (0._dp,2._dp)* T_Lor(24) - T_Lor(31) + &
             (0._dp,1._dp)* (-T_Lor(18) - T_Lor(34))
  T_LC(28) = -T_Lor(17) - 2._dp * T_Lor(22) + (0._dp,2._dp)* T_Lor(24) - T_Lor(31) + &
             (0._dp,1._dp)* (T_Lor(18) + T_Lor(34))
  T_LC(29) = T_Lor(20) - T_Lor(23) + T_Lor(28) + (0._dp,2._dp)* (T_Lor(21) + T_Lor(30)) - T_Lor(33)
  T_LC(30) = T_Lor(20) + T_Lor(23) + T_Lor(28) + T_Lor(33)
  T_LC(31) = T_Lor(20) - T_Lor(23) + T_Lor(28) + (0._dp,2._dp)* (-T_Lor(21) - T_Lor(30)) - T_Lor(33)
  T_LC(32) = -T_Lor(26) - (0._dp,3._dp)* T_Lor(27) + 3._dp * T_Lor(29) + (0._dp,1._dp)* T_Lor(32)
  T_LC(33) = -T_Lor(26) - T_Lor(29) + (0._dp,1._dp)* (-T_Lor(27) - T_Lor(32))
  T_LC(34) = -T_Lor(26) - T_Lor(29) + (0._dp,1._dp)* (T_Lor(27) + T_Lor(32))
  T_LC(35) = -T_Lor(26) + (0._dp,3._dp)* T_Lor(27) + 3._dp * T_Lor(29) - (0._dp,1._dp)* T_Lor(32)
  T_LC(36) = T_Lor(36) + 6._dp * T_Lor(45) + 4._dp * (-T_Lor(39) - T_Lor(55)) + T_Lor(70)
  T_LC(37) = T_Lor(36) + 2._dp * (-T_Lor(39) + T_Lor(55)) - T_Lor(70)
  T_LC(38) = -T_Lor(37) + 3._dp * (T_Lor(42) - T_Lor(51)) + (0._dp,3._dp)* (T_Lor(44) - T_Lor(54)) + &
             T_Lor(65) + (0._dp,1._dp)* (-T_Lor(38) + T_Lor(69))
  T_LC(39) = -T_Lor(37) + 3._dp * (T_Lor(42) - T_Lor(51)) + (0._dp,3._dp)* (-T_Lor(44) + T_Lor(54)) + &
             T_Lor(65) + (0._dp,1._dp)* (T_Lor(38) - T_Lor(69))
  T_LC(40) = T_Lor(36) - 2._dp * T_Lor(45) + T_Lor(70)
  T_LC(41) = -T_Lor(37) + T_Lor(42) + T_Lor(51) - T_Lor(65) + (0._dp,1._dp)* (-T_Lor(38) + T_Lor(44) + T_Lor(54) - T_Lor(69))
  T_LC(42) = -T_Lor(37) + T_Lor(42) + T_Lor(51) - T_Lor(65) + (0._dp,1._dp)* (T_Lor(38) - T_Lor(44) - T_Lor(54) + T_Lor(69))
  T_LC(43) = T_Lor(40) - T_Lor(43) - (0._dp,4._dp)* T_Lor(50) + 2._dp * (-T_Lor(48) + T_Lor(53)) + T_Lor(61) + &
             (0._dp,2._dp)* (T_Lor(41) + T_Lor(64)) - T_Lor(68)
  T_LC(44) = T_Lor(40) + T_Lor(43) + 2._dp * (-T_Lor(48) - T_Lor(53)) + T_Lor(61) + T_Lor(68)
  T_LC(45) = T_Lor(40) - T_Lor(43) + (0._dp,4._dp)* T_Lor(50) + 2._dp * (-T_Lor(48) + T_Lor(53)) + T_Lor(61) + &
             (0._dp,2._dp)* (-T_Lor(41) - T_Lor(64)) - T_Lor(68)
  T_LC(46) = T_Lor(36) + 2._dp * (T_Lor(39) - T_Lor(55)) - T_Lor(70)
  T_LC(47) = -T_Lor(37) - T_Lor(42) + T_Lor(51) + T_Lor(65) + (0._dp,1._dp)* (-T_Lor(38) - T_Lor(44) + T_Lor(54) + T_Lor(69))
  T_LC(48) = -T_Lor(37) - T_Lor(42) + T_Lor(51) + T_Lor(65) + (0._dp,1._dp)* (T_Lor(38) + T_Lor(44) - T_Lor(54) - T_Lor(69))
  T_LC(49) = T_Lor(40) - T_Lor(43) - T_Lor(61) + (0._dp,2._dp)* (T_Lor(41) - T_Lor(64)) + T_Lor(68)
  T_LC(50) = T_Lor(40) + T_Lor(43) - T_Lor(61) - T_Lor(68)
  T_LC(51) = T_Lor(40) - T_Lor(43) - T_Lor(61) + (0._dp,2._dp)* (-T_Lor(41) + T_Lor(64)) + T_Lor(68)
  T_LC(52) = -T_Lor(46) + T_Lor(58) + (0._dp,3._dp)* (-T_Lor(47) + T_Lor(60)) + 3._dp * (T_Lor(49) - T_Lor(63)) + &
             (0._dp,1._dp)* (T_Lor(52) - T_Lor(67))
  T_LC(53) = -T_Lor(46) - T_Lor(49) + T_Lor(58) + T_Lor(63) + (0._dp,1._dp)* (-T_Lor(47) - T_Lor(52) + T_Lor(60) + T_Lor(67))
  T_LC(54) = -T_Lor(46) - T_Lor(49) + T_Lor(58) + T_Lor(63) + (0._dp,1._dp)* (T_Lor(47) + T_Lor(52) - T_Lor(60) - T_Lor(67))
  T_LC(55) = -T_Lor(46) + T_Lor(58) + (0._dp,3._dp)* (T_Lor(47) - T_Lor(60)) + 3._dp * (T_Lor(49) - T_Lor(63)) + &
             (0._dp,1._dp)* (-T_Lor(52) + T_Lor(67))
  T_LC(56) = T_Lor(36) + 6._dp * T_Lor(45) + 4._dp * (T_Lor(39) + T_Lor(55)) + T_Lor(70)
  T_LC(57) = -T_Lor(37) + 3._dp * (-T_Lor(42) - T_Lor(51)) + (0._dp,3._dp)* (-T_Lor(44) - T_Lor(54)) - T_Lor(65) + &
             (0._dp,1._dp)* (-T_Lor(38) - T_Lor(69))
  T_LC(58) = -T_Lor(37) + 3._dp * (-T_Lor(42) - T_Lor(51)) + (0._dp,3._dp)* (T_Lor(44) + T_Lor(54)) - T_Lor(65) + &
             (0._dp,1._dp)* (T_Lor(38) + T_Lor(69))
  T_LC(59) = T_Lor(40) - T_Lor(43) + (0._dp,4._dp)* T_Lor(50) + 2._dp * (T_Lor(48) - T_Lor(53)) + T_Lor(61) + &
             (0._dp,2._dp)* (T_Lor(41) + T_Lor(64)) - T_Lor(68)
  T_LC(60) = T_Lor(40) + T_Lor(43) + 2._dp * (T_Lor(48) + T_Lor(53)) + T_Lor(61) + T_Lor(68)
  T_LC(61) = T_Lor(40) - T_Lor(43) - (0._dp,4._dp)* T_Lor(50) + 2._dp * (T_Lor(48) - T_Lor(53)) + T_Lor(61) + &
             (0._dp,2._dp)* (-T_Lor(41) - T_Lor(64)) - T_Lor(68)
  T_LC(62) = -T_Lor(46) - T_Lor(58) + (0._dp,3._dp)* (-T_Lor(47) - T_Lor(60)) + 3._dp * (T_Lor(49) + T_Lor(63)) + &
             (0._dp,1._dp)* (T_Lor(52) + T_Lor(67))
  T_LC(63) = -T_Lor(46) - T_Lor(49) - T_Lor(58) - T_Lor(63) + (0._dp,1._dp)* (-T_Lor(47) - T_Lor(52) - T_Lor(60) - T_Lor(67))
  T_LC(64) = -T_Lor(46) - T_Lor(49) - T_Lor(58) - T_Lor(63) + (0._dp,1._dp)* (T_Lor(47) + T_Lor(52) + T_Lor(60) + T_Lor(67))
  T_LC(65) = -T_Lor(46) - T_Lor(58) + (0._dp,3._dp)* (T_Lor(47) + T_Lor(60)) + 3._dp * (T_Lor(49) + T_Lor(63)) + &
             (0._dp,1._dp)* (-T_Lor(52) - T_Lor(67))
  T_LC(66) = T_Lor(56) - 6._dp * T_Lor(59) + (0._dp,4._dp)* (T_Lor(57) - T_Lor(62)) + T_Lor(66)
  T_LC(67) = T_Lor(56) + (0._dp,2._dp)* (T_Lor(57) + T_Lor(62)) - T_Lor(66)
  T_LC(68) = T_Lor(56) + 2._dp * T_Lor(59) + T_Lor(66)
  T_LC(69) = T_Lor(56) + (0._dp,2._dp)* (-T_Lor(57) - T_Lor(62)) - T_Lor(66)
  T_LC(70) = T_Lor(56) - 6._dp * T_Lor(59) + (0._dp,4._dp)* (-T_Lor(57) + T_Lor(62)) + T_Lor(66)
  T_LC(71) = -T_Lor(126) + T_Lor(71) + 5._dp * (T_Lor(105) - T_Lor(74)) + 10._dp * (T_Lor(80) - T_Lor(90))
  T_LC(72) = T_Lor(126) + T_Lor(71) + 3._dp * (-T_Lor(105) - T_Lor(74)) + 2._dp * (T_Lor(80) + T_Lor(90))
  T_LC(73) = -T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (-T_Lor(125) - T_Lor(73)) + 4._dp * (T_Lor(100) + T_Lor(77)) + &
             (0._dp,4._dp)* (T_Lor(104) + T_Lor(79)) - 6._dp * T_Lor(86) - (0._dp,6._dp)* T_Lor(89)
  T_LC(74) = -T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (T_Lor(125) + T_Lor(73)) + 4._dp * (T_Lor(100) + T_Lor(77)) + &
             (0._dp,4._dp)* (-T_Lor(104) - T_Lor(79)) - 6._dp * T_Lor(86) + (0._dp,6._dp)* T_Lor(89)
  T_LC(75) = T_Lor(105) - T_Lor(126) + T_Lor(71) - T_Lor(74) + 2._dp * (-T_Lor(80) + T_Lor(90))
  T_LC(76) = T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (T_Lor(125) - T_Lor(73)) + 2._dp * (-T_Lor(100) + T_Lor(77)) + &
             (0._dp,2._dp)* (-T_Lor(104) + T_Lor(79))
  T_LC(77) = T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (-T_Lor(125) + T_Lor(73)) + 2._dp * (-T_Lor(100) + T_Lor(77)) + &
             (0._dp,2._dp)* (T_Lor(104) - T_Lor(79))
  T_LC(78) = -T_Lor(115) + T_Lor(124) + T_Lor(75) + (0._dp,2._dp)* (-T_Lor(119) + T_Lor(76)) - T_Lor(78) + &
             3._dp * (-T_Lor(103) - T_Lor(83) + T_Lor(88) + T_Lor(96)) + (0._dp,6._dp)* (-T_Lor(85) + T_Lor(99))
  T_LC(79) = -T_Lor(115) - T_Lor(124) + T_Lor(75) + T_Lor(78) + 3._dp * (T_Lor(103) - T_Lor(83) - T_Lor(88) + T_Lor(96))
  T_LC(80) = -T_Lor(115) + T_Lor(124) + T_Lor(75) + (0._dp,2._dp)* (T_Lor(119) - T_Lor(76)) - T_Lor(78) + &
             3._dp * (-T_Lor(103) - T_Lor(83) + T_Lor(88) + T_Lor(96)) + (0._dp,6._dp)* (T_Lor(85) - T_Lor(99))
  T_LC(81) = T_Lor(105) + T_Lor(126) + T_Lor(71) + T_Lor(74) + 2._dp * (-T_Lor(80) - T_Lor(90))
  T_LC(82) = -T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (-T_Lor(125) - T_Lor(73)) + 2._dp * T_Lor(86) + &
             (0._dp,2._dp)* T_Lor(89)
  T_LC(83) = -T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (T_Lor(125) + T_Lor(73)) + 2._dp * T_Lor(86) - &
             (0._dp,2._dp)* T_Lor(89)
  T_LC(84) = T_Lor(103) + T_Lor(115) - T_Lor(124) + T_Lor(75) - T_Lor(78) - T_Lor(83) + T_Lor(88) - T_Lor(96) + &
             (0._dp,2._dp)* (T_Lor(119) + T_Lor(76) - T_Lor(85) - T_Lor(99))
  T_LC(85) = -T_Lor(103) + T_Lor(115) + T_Lor(124) + T_Lor(75) + T_Lor(78) - T_Lor(83) - T_Lor(88) - T_Lor(96)
  T_LC(86) = T_Lor(103) + T_Lor(115) - T_Lor(124) + T_Lor(75) - T_Lor(78) - T_Lor(83) + T_Lor(88) - T_Lor(96) + &
             (0._dp,2._dp)* (-T_Lor(119) - T_Lor(76) + T_Lor(85) + T_Lor(99))
  T_LC(87) = -((0._dp,2._dp)* T_Lor(102)) - T_Lor(111) - T_Lor(81) + (0._dp,3._dp)* (-T_Lor(114) - T_Lor(82)) + &
             3._dp * (T_Lor(118) + T_Lor(84)) + (0._dp,1._dp)* (T_Lor(123) + T_Lor(87)) + 2._dp * T_Lor(93) + &
             (0._dp,6._dp)* T_Lor(95) - 6._dp * T_Lor(98)
  T_LC(88) = -T_Lor(111) - T_Lor(118) - T_Lor(81) - T_Lor(84) + (0._dp,1._dp)* (-T_Lor(114) - T_Lor(123) - T_Lor(82) - &
             T_Lor(87)) + (0._dp,2._dp)* (T_Lor(102) + T_Lor(95)) + 2._dp * (T_Lor(93) + T_Lor(98))
  T_LC(89) = -T_Lor(111) - T_Lor(118) - T_Lor(81) - T_Lor(84) + (0._dp,1._dp)* (T_Lor(114) + T_Lor(123) + T_Lor(82) + &
             T_Lor(87)) + (0._dp,2._dp)* (-T_Lor(102) - T_Lor(95)) + 2._dp * (T_Lor(93) + T_Lor(98))
  T_LC(90) = (0._dp,2._dp)* T_Lor(102) - T_Lor(111) - T_Lor(81) + (0._dp,3._dp)* (T_Lor(114) + T_Lor(82)) + &
             3._dp * (T_Lor(118) + T_Lor(84)) + (0._dp,1._dp)* (-T_Lor(123) - T_Lor(87)) + 2._dp * T_Lor(93) - &
             (0._dp,6._dp)* T_Lor(95) - 6._dp * T_Lor(98)
  T_LC(91) = -T_Lor(126) + T_Lor(71) + 3._dp * (-T_Lor(105) + T_Lor(74)) + 2._dp * (T_Lor(80) - T_Lor(90))
  T_LC(92) = T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (T_Lor(125) - T_Lor(73)) + 2._dp * (T_Lor(100) - T_Lor(77)) + &
             (0._dp,2._dp)* (T_Lor(104) - T_Lor(79))
  T_LC(93) = T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (-T_Lor(125) + T_Lor(73)) + 2._dp * (T_Lor(100) - T_Lor(77)) + &
             (0._dp,2._dp)* (-T_Lor(104) + T_Lor(79))
  T_LC(94) = T_Lor(103) - T_Lor(115) + T_Lor(124) + T_Lor(75) - T_Lor(78) + T_Lor(83) - T_Lor(88) - T_Lor(96) + &
             (0._dp,2._dp)* (-T_Lor(119) + T_Lor(76) + T_Lor(85) - T_Lor(99))
  T_LC(95) = -T_Lor(103) - T_Lor(115) - T_Lor(124) + T_Lor(75) + T_Lor(78) + T_Lor(83) + T_Lor(88) - T_Lor(96)
  T_LC(96) = T_Lor(103) - T_Lor(115) + T_Lor(124) + T_Lor(75) - T_Lor(78) + T_Lor(83) - T_Lor(88) - T_Lor(96) + &
             (0._dp,2._dp)* (T_Lor(119) - T_Lor(76) - T_Lor(85) + T_Lor(99))
  T_LC(97) = T_Lor(111) - T_Lor(81) + (0._dp,3._dp)* (T_Lor(114) - T_Lor(82)) + 3._dp * (-T_Lor(118) + T_Lor(84)) + &
             (0._dp,1._dp)* (-T_Lor(123) + T_Lor(87))
  T_LC(98) = T_Lor(111) + T_Lor(118) - T_Lor(81) - T_Lor(84) + (0._dp,1._dp)* (T_Lor(114) + T_Lor(123) -T_Lor(82)-T_Lor(87))
  T_LC(99) = T_Lor(111) + T_Lor(118) - T_Lor(81) - T_Lor(84) + (0._dp,1._dp)*(-T_Lor(114) - T_Lor(123) +T_Lor(82)+T_Lor(87))
  T_LC(100) = T_Lor(111) - T_Lor(81) + (0._dp,3._dp)* (-T_Lor(114) + T_Lor(82)) + 3._dp * (-T_Lor(118) + T_Lor(84)) + &
              (0._dp,1._dp)* (T_Lor(123) - T_Lor(87))
  T_LC(101) = T_Lor(101) - T_Lor(108) - T_Lor(122) + T_Lor(91) + 6._dp * (T_Lor(113) - T_Lor(94)) + &
              (0._dp,4._dp)* (-T_Lor(110) + T_Lor(117) + T_Lor(92) - T_Lor(97))
  T_LC(102) = -T_Lor(101) - T_Lor(108) + T_Lor(122) + T_Lor(91) + (0._dp,2._dp)* (-T_Lor(110) - T_Lor(117) + &
               T_Lor(92) + T_Lor(97))
  T_LC(103) = T_Lor(101) - T_Lor(108) - T_Lor(122) + T_Lor(91) + 2._dp * (-T_Lor(113) + T_Lor(94))
  T_LC(104) = -T_Lor(101) - T_Lor(108) + T_Lor(122) + T_Lor(91) + (0._dp,2._dp)* (T_Lor(110) + T_Lor(117) - &
              T_Lor(92) - T_Lor(97))
  T_LC(105) = T_Lor(101) - T_Lor(108) - T_Lor(122) + T_Lor(91) + 6._dp * (T_Lor(113) - T_Lor(94)) + &
              (0._dp,4._dp)* (T_Lor(110) - T_Lor(117) - T_Lor(92) + T_Lor(97))
  T_LC(106) = T_Lor(126) + T_Lor(71) + 5._dp * (T_Lor(105) + T_Lor(74)) + 10._dp * (T_Lor(80) + T_Lor(90))
  T_LC(107) = -T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (-T_Lor(125) - T_Lor(73)) + 4._dp * (-T_Lor(100) - T_Lor(77)) + &
              (0._dp,4._dp)* (-T_Lor(104) - T_Lor(79)) - 6._dp * T_Lor(86) - (0._dp,6._dp)* T_Lor(89)
  T_LC(108) = -T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (T_Lor(125) + T_Lor(73)) + 4._dp * (-T_Lor(100) - T_Lor(77)) + &
              (0._dp,4._dp)* (T_Lor(104) + T_Lor(79)) - 6._dp * T_Lor(86) + (0._dp,6._dp)* T_Lor(89)
  T_LC(109) = T_Lor(115) - T_Lor(124) + T_Lor(75) + (0._dp,2._dp)* (T_Lor(119) + T_Lor(76)) - T_Lor(78) + &
              3._dp * (-T_Lor(103) + T_Lor(83) - T_Lor(88) + T_Lor(96)) + (0._dp,6._dp)* (T_Lor(85) + T_Lor(99))
  T_LC(110) = T_Lor(115) + T_Lor(124) + T_Lor(75) + T_Lor(78) + 3._dp * (T_Lor(103) + T_Lor(83) + T_Lor(88) + T_Lor(96))
  T_LC(111) = T_Lor(115) - T_Lor(124) + T_Lor(75) + (0._dp,2._dp)* (-T_Lor(119) - T_Lor(76)) - T_Lor(78) + &
              3._dp * (-T_Lor(103) + T_Lor(83) - T_Lor(88) + T_Lor(96)) + (0._dp,6._dp)* (-T_Lor(85) - T_Lor(99))
  T_LC(112) = (0._dp,2._dp)* T_Lor(102) - T_Lor(111) - T_Lor(81) + (0._dp,3._dp)* (-T_Lor(114) - T_Lor(82)) + &
              3._dp * (T_Lor(118) + T_Lor(84)) + (0._dp,1._dp)* (T_Lor(123) + T_Lor(87)) - 2._dp * T_Lor(93) - &
              (0._dp,6._dp)* T_Lor(95) + 6._dp * T_Lor(98)
  T_LC(113) = -T_Lor(111) - T_Lor(118) - T_Lor(81) - T_Lor(84) + (0._dp,1._dp)* (-T_Lor(114) - T_Lor(123) - T_Lor(82) - &
              T_Lor(87)) + (0._dp,2._dp)* (-T_Lor(102) - T_Lor(95)) + 2._dp * (-T_Lor(93) - T_Lor(98))
  T_LC(114) = -T_Lor(111) - T_Lor(118) - T_Lor(81) - T_Lor(84) + (0._dp,1._dp)* (T_Lor(114) + T_Lor(123) + T_Lor(82) + &
              T_Lor(87)) + (0._dp,2._dp)* (T_Lor(102) + T_Lor(95)) + 2._dp * (-T_Lor(93) - T_Lor(98))
  T_LC(115) = -((0._dp,2._dp)* T_Lor(102)) - T_Lor(111) - T_Lor(81) + (0._dp,3._dp)* (T_Lor(114) + T_Lor(82)) + &
              3._dp * (T_Lor(118) + T_Lor(84)) + (0._dp,1._dp)* (-T_Lor(123) - T_Lor(87)) - 2._dp * T_Lor(93) + &
              (0._dp,6._dp)* T_Lor(95) + 6._dp * T_Lor(98)
  T_LC(116) = T_Lor(101) + T_Lor(108) + T_Lor(122) + T_Lor(91) + 6._dp * (-T_Lor(113) - T_Lor(94)) + &
              (0._dp,4._dp)* (T_Lor(110) - T_Lor(117) + T_Lor(92) - T_Lor(97))
  T_LC(117) = -T_Lor(101) + T_Lor(108) - T_Lor(122) + T_Lor(91) + (0._dp,2._dp)* (T_Lor(110) + &
              T_Lor(117) + T_Lor(92) + T_Lor(97))
  T_LC(118) = T_Lor(101) + T_Lor(108) + T_Lor(122) + T_Lor(91) + 2._dp * (T_Lor(113) + T_Lor(94))
  T_LC(119) = -T_Lor(101) + T_Lor(108) - T_Lor(122) + T_Lor(91) + (0._dp,2._dp)* (-T_Lor(110) - &
              T_Lor(117) - T_Lor(92) - T_Lor(97))
  T_LC(120) = T_Lor(101) + T_Lor(108) + T_Lor(122) + T_Lor(91) + 6._dp * (-T_Lor(113) - T_Lor(94)) + &
              (0._dp,4._dp)* (-T_Lor(110) + T_Lor(117) - T_Lor(92) + T_Lor(97))
  T_LC(121) = -T_Lor(106) - (0._dp,5._dp)* T_Lor(107) + 10._dp * T_Lor(109) + (0._dp,10._dp)* T_Lor(112) - &
              5._dp * T_Lor(116) - (0._dp,1._dp)* T_Lor(121)
  T_LC(122) = -T_Lor(106) - (0._dp,3._dp)* T_Lor(107) + 2._dp * T_Lor(109) - (0._dp,2._dp)* T_Lor(112) + &
              3._dp * T_Lor(116) + (0._dp,1._dp)* T_Lor(121)
  T_LC(123) = -T_Lor(106) - 2._dp * T_Lor(109) - (0._dp,2._dp)* T_Lor(112) - T_Lor(116) + &
              (0._dp,1._dp)* (-T_Lor(107) - T_Lor(121))
  T_LC(124) = -T_Lor(106) - 2._dp * T_Lor(109) + (0._dp,2._dp)* T_Lor(112) - T_Lor(116) + &
              (0._dp,1._dp)* (T_Lor(107) + T_Lor(121))
  T_LC(125) = -T_Lor(106) + (0._dp,3._dp)* T_Lor(107) + 2._dp * T_Lor(109) + (0._dp,2._dp)* T_Lor(112) + &
              3._dp * T_Lor(116) - (0._dp,1._dp)* T_Lor(121)
  T_LC(126) = -T_Lor(106) + (0._dp,5._dp)* T_Lor(107) + 10._dp * T_Lor(109) - (0._dp,10._dp)* T_Lor(112) - &
              5._dp * T_Lor(116) + (0._dp,1._dp)* T_Lor(121)

end subroutine Std2LC_Tensor5


! **********************************************************************
subroutine Std2LC_Tensor6(T_Lor,T_LC)
! Lorentz contravariant -> Light-cone contravariant
! **********************************************************************
  use kind_types, only: dp
  implicit none
  complex(dp), intent(in)  :: T_Lor(210)
  complex(dp), intent(out) :: T_LC(210)

  T_LC(1) = T_Lor(1)
  T_LC(2) = T_Lor(2) - T_Lor(5)
  T_LC(3) = T_Lor(2) + T_Lor(5)
  T_LC(4) = -T_Lor(3) - (0._dp,1._dp)* T_Lor(4)
  T_LC(5) = -T_Lor(3) + (0._dp,1._dp)* T_Lor(4)
  T_LC(6) = T_Lor(15) + T_Lor(6) - 2._dp * T_Lor(9)
  T_LC(7) = -T_Lor(15) + T_Lor(6)
  T_LC(8) = T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (T_Lor(14) - T_Lor(8))
  T_LC(9) = T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (-T_Lor(14) + T_Lor(8))
  T_LC(10) = T_Lor(15) + T_Lor(6) + 2._dp * T_Lor(9)
  T_LC(11) = -T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (-T_Lor(14) - T_Lor(8))
  T_LC(12) = -T_Lor(12) - T_Lor(7) + (0._dp,1._dp)* (T_Lor(14) + T_Lor(8))
  T_LC(13) = T_Lor(10) + (0._dp,2._dp)* T_Lor(11) - T_Lor(13)
  T_LC(14) = T_Lor(10) + T_Lor(13)
  T_LC(15) = T_Lor(10) - (0._dp,2._dp)* T_Lor(11) - T_Lor(13)
  T_LC(16) = T_Lor(16) + 3._dp * (-T_Lor(19) + T_Lor(25)) - T_Lor(35)
  T_LC(17) = T_Lor(16) - T_Lor(19) - T_Lor(25) + T_Lor(35)
  T_LC(18) = -T_Lor(17) + 2._dp * T_Lor(22) + (0._dp,2._dp)* T_Lor(24) - T_Lor(31) + &
             (0._dp,1._dp)* (-T_Lor(18) - T_Lor(34))
  T_LC(19) = -T_Lor(17) + 2._dp * T_Lor(22) - (0._dp,2._dp)* T_Lor(24) - T_Lor(31) + &
             (0._dp,1._dp)* (T_Lor(18) + T_Lor(34))
  T_LC(20) = T_Lor(16) + T_Lor(19) - T_Lor(25) - T_Lor(35)
  T_LC(21) = -T_Lor(17) + T_Lor(31) + (0._dp,1._dp)* (-T_Lor(18) + T_Lor(34))
  T_LC(22) = -T_Lor(17) + T_Lor(31) + (0._dp,1._dp)* (T_Lor(18) - T_Lor(34))
  T_LC(23) = T_Lor(20) - T_Lor(23) - T_Lor(28) + (0._dp,2._dp)* (T_Lor(21) - T_Lor(30)) + T_Lor(33)
  T_LC(24) = T_Lor(20) + T_Lor(23) - T_Lor(28) - T_Lor(33)
  T_LC(25) = T_Lor(20) - T_Lor(23) - T_Lor(28) + (0._dp,2._dp)* (-T_Lor(21) + T_Lor(30)) + T_Lor(33)
  T_LC(26) = T_Lor(16) + 3._dp * (T_Lor(19) + T_Lor(25)) + T_Lor(35)
  T_LC(27) = -T_Lor(17) - 2._dp * T_Lor(22) - (0._dp,2._dp)* T_Lor(24) - T_Lor(31) + &
             (0._dp,1._dp)* (-T_Lor(18) - T_Lor(34))
  T_LC(28) = -T_Lor(17) - 2._dp * T_Lor(22) + (0._dp,2._dp)* T_Lor(24) - T_Lor(31) + &
             (0._dp,1._dp)* (T_Lor(18) + T_Lor(34))
  T_LC(29) = T_Lor(20) - T_Lor(23) + T_Lor(28) + (0._dp,2._dp)* (T_Lor(21) + T_Lor(30)) - T_Lor(33)
  T_LC(30) = T_Lor(20) + T_Lor(23) + T_Lor(28) + T_Lor(33)
  T_LC(31) = T_Lor(20) - T_Lor(23) + T_Lor(28) + (0._dp,2._dp)* (-T_Lor(21) - T_Lor(30)) - T_Lor(33)
  T_LC(32) = -T_Lor(26) - (0._dp,3._dp)* T_Lor(27) + 3._dp * T_Lor(29) + (0._dp,1._dp)* T_Lor(32)
  T_LC(33) = -T_Lor(26) - T_Lor(29) + (0._dp,1._dp)* (-T_Lor(27) - T_Lor(32))
  T_LC(34) = -T_Lor(26) - T_Lor(29) + (0._dp,1._dp)* (T_Lor(27) + T_Lor(32))
  T_LC(35) = -T_Lor(26) + (0._dp,3._dp)* T_Lor(27) + 3._dp * T_Lor(29) - (0._dp,1._dp)* T_Lor(32)
  T_LC(36) = T_Lor(36) + 6._dp * T_Lor(45) + 4._dp * (-T_Lor(39) - T_Lor(55)) + T_Lor(70)
  T_LC(37) = T_Lor(36) + 2._dp * (-T_Lor(39) + T_Lor(55)) - T_Lor(70)
  T_LC(38) = -T_Lor(37) + 3._dp * (T_Lor(42) - T_Lor(51)) + (0._dp,3._dp)* (T_Lor(44) - T_Lor(54)) + &
             T_Lor(65) + (0._dp,1._dp)* (-T_Lor(38) + T_Lor(69))
  T_LC(39) = -T_Lor(37) + 3._dp * (T_Lor(42) - T_Lor(51)) + (0._dp,3._dp)* (-T_Lor(44) + T_Lor(54)) + &
             T_Lor(65) + (0._dp,1._dp)* (T_Lor(38) - T_Lor(69))
  T_LC(40) = T_Lor(36) - 2._dp * T_Lor(45) + T_Lor(70)
  T_LC(41) = -T_Lor(37) + T_Lor(42) + T_Lor(51) - T_Lor(65) + (0._dp,1._dp)* (-T_Lor(38) + T_Lor(44) + T_Lor(54) - T_Lor(69))
  T_LC(42) = -T_Lor(37) + T_Lor(42) + T_Lor(51) - T_Lor(65) + (0._dp,1._dp)* (T_Lor(38) - T_Lor(44) - T_Lor(54) + T_Lor(69))
  T_LC(43) = T_Lor(40) - T_Lor(43) - (0._dp,4._dp)* T_Lor(50) + 2._dp * (-T_Lor(48) + T_Lor(53)) + T_Lor(61) + &
             (0._dp,2._dp)* (T_Lor(41) + T_Lor(64)) - T_Lor(68)
  T_LC(44) = T_Lor(40) + T_Lor(43) + 2._dp * (-T_Lor(48) - T_Lor(53)) + T_Lor(61) + T_Lor(68)
  T_LC(45) = T_Lor(40) - T_Lor(43) + (0._dp,4._dp)* T_Lor(50) + 2._dp * (-T_Lor(48) + T_Lor(53)) + T_Lor(61) + &
             (0._dp,2._dp)* (-T_Lor(41) - T_Lor(64)) - T_Lor(68)
  T_LC(46) = T_Lor(36) + 2._dp * (T_Lor(39) - T_Lor(55)) - T_Lor(70)
  T_LC(47) = -T_Lor(37) - T_Lor(42) + T_Lor(51) + T_Lor(65) + (0._dp,1._dp)* (-T_Lor(38) - T_Lor(44) + T_Lor(54) + T_Lor(69))
  T_LC(48) = -T_Lor(37) - T_Lor(42) + T_Lor(51) + T_Lor(65) + (0._dp,1._dp)* (T_Lor(38) + T_Lor(44) - T_Lor(54) - T_Lor(69))
  T_LC(49) = T_Lor(40) - T_Lor(43) - T_Lor(61) + (0._dp,2._dp)* (T_Lor(41) - T_Lor(64)) + T_Lor(68)
  T_LC(50) = T_Lor(40) + T_Lor(43) - T_Lor(61) - T_Lor(68)
  T_LC(51) = T_Lor(40) - T_Lor(43) - T_Lor(61) + (0._dp,2._dp)* (-T_Lor(41) + T_Lor(64)) + T_Lor(68)
  T_LC(52) = -T_Lor(46) + T_Lor(58) + (0._dp,3._dp)* (-T_Lor(47) + T_Lor(60)) + 3._dp * (T_Lor(49) - T_Lor(63)) + &
             (0._dp,1._dp)* (T_Lor(52) - T_Lor(67))
  T_LC(53) = -T_Lor(46) - T_Lor(49) + T_Lor(58) + T_Lor(63) + (0._dp,1._dp)* (-T_Lor(47) - T_Lor(52) + T_Lor(60) + T_Lor(67))
  T_LC(54) = -T_Lor(46) - T_Lor(49) + T_Lor(58) + T_Lor(63) + (0._dp,1._dp)* (T_Lor(47) + T_Lor(52) - T_Lor(60) - T_Lor(67))
  T_LC(55) = -T_Lor(46) + T_Lor(58) + (0._dp,3._dp)* (T_Lor(47) - T_Lor(60)) + 3._dp * (T_Lor(49) - T_Lor(63)) + &
             (0._dp,1._dp)* (-T_Lor(52) + T_Lor(67))
  T_LC(56) = T_Lor(36) + 6._dp * T_Lor(45) + 4._dp * (T_Lor(39) + T_Lor(55)) + T_Lor(70)
  T_LC(57) = -T_Lor(37) + 3._dp * (-T_Lor(42) - T_Lor(51)) + (0._dp,3._dp)* (-T_Lor(44) - T_Lor(54)) - T_Lor(65) + &
             (0._dp,1._dp)* (-T_Lor(38) - T_Lor(69))
  T_LC(58) = -T_Lor(37) + 3._dp * (-T_Lor(42) - T_Lor(51)) + (0._dp,3._dp)* (T_Lor(44) + T_Lor(54)) - T_Lor(65) + &
             (0._dp,1._dp)* (T_Lor(38) + T_Lor(69))
  T_LC(59) = T_Lor(40) - T_Lor(43) + (0._dp,4._dp)* T_Lor(50) + 2._dp * (T_Lor(48) - T_Lor(53)) + T_Lor(61) + &
             (0._dp,2._dp)* (T_Lor(41) + T_Lor(64)) - T_Lor(68)
  T_LC(60) = T_Lor(40) + T_Lor(43) + 2._dp * (T_Lor(48) + T_Lor(53)) + T_Lor(61) + T_Lor(68)
  T_LC(61) = T_Lor(40) - T_Lor(43) - (0._dp,4._dp)* T_Lor(50) + 2._dp * (T_Lor(48) - T_Lor(53)) + T_Lor(61) + &
             (0._dp,2._dp)* (-T_Lor(41) - T_Lor(64)) - T_Lor(68)
  T_LC(62) = -T_Lor(46) - T_Lor(58) + (0._dp,3._dp)* (-T_Lor(47) - T_Lor(60)) + 3._dp * (T_Lor(49) + T_Lor(63)) + &
             (0._dp,1._dp)* (T_Lor(52) + T_Lor(67))
  T_LC(63) = -T_Lor(46) - T_Lor(49) - T_Lor(58) - T_Lor(63) + (0._dp,1._dp)* (-T_Lor(47) - T_Lor(52) - T_Lor(60) - T_Lor(67))
  T_LC(64) = -T_Lor(46) - T_Lor(49) - T_Lor(58) - T_Lor(63) + (0._dp,1._dp)* (T_Lor(47) + T_Lor(52) + T_Lor(60) + T_Lor(67))
  T_LC(65) = -T_Lor(46) - T_Lor(58) + (0._dp,3._dp)* (T_Lor(47) + T_Lor(60)) + 3._dp * (T_Lor(49) + T_Lor(63)) + &
             (0._dp,1._dp)* (-T_Lor(52) - T_Lor(67))
  T_LC(66) = T_Lor(56) - 6._dp * T_Lor(59) + (0._dp,4._dp)* (T_Lor(57) - T_Lor(62)) + T_Lor(66)
  T_LC(67) = T_Lor(56) + (0._dp,2._dp)* (T_Lor(57) + T_Lor(62)) - T_Lor(66)
  T_LC(68) = T_Lor(56) + 2._dp * T_Lor(59) + T_Lor(66)
  T_LC(69) = T_Lor(56) + (0._dp,2._dp)* (-T_Lor(57) - T_Lor(62)) - T_Lor(66)
  T_LC(70) = T_Lor(56) - 6._dp * T_Lor(59) + (0._dp,4._dp)* (-T_Lor(57) + T_Lor(62)) + T_Lor(66)
  T_LC(71) = -T_Lor(126) + T_Lor(71) + 5._dp * (T_Lor(105) - T_Lor(74)) + 10._dp * (T_Lor(80) - T_Lor(90))
  T_LC(72) = T_Lor(126) + T_Lor(71) + 3._dp * (-T_Lor(105) - T_Lor(74)) + 2._dp * (T_Lor(80) + T_Lor(90))
  T_LC(73) = -T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (-T_Lor(125) - T_Lor(73)) + 4._dp * (T_Lor(100) + T_Lor(77)) + &
             (0._dp,4._dp)* (T_Lor(104) + T_Lor(79)) - 6._dp * T_Lor(86) - (0._dp,6._dp)* T_Lor(89)
  T_LC(74) = -T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (T_Lor(125) + T_Lor(73)) + 4._dp * (T_Lor(100) + T_Lor(77)) + &
             (0._dp,4._dp)* (-T_Lor(104) - T_Lor(79)) - 6._dp * T_Lor(86) + (0._dp,6._dp)* T_Lor(89)
  T_LC(75) = T_Lor(105) - T_Lor(126) + T_Lor(71) - T_Lor(74) + 2._dp * (-T_Lor(80) + T_Lor(90))
  T_LC(76) = T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (T_Lor(125) - T_Lor(73)) + 2._dp * (-T_Lor(100) + T_Lor(77)) + &
             (0._dp,2._dp)* (-T_Lor(104) + T_Lor(79))
  T_LC(77) = T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (-T_Lor(125) + T_Lor(73)) + 2._dp * (-T_Lor(100) + T_Lor(77)) + &
             (0._dp,2._dp)* (T_Lor(104) - T_Lor(79))
  T_LC(78) = -T_Lor(115) + T_Lor(124) + T_Lor(75) + (0._dp,2._dp)* (-T_Lor(119) + T_Lor(76)) - T_Lor(78) + &
             3._dp * (-T_Lor(103) - T_Lor(83) + T_Lor(88) + T_Lor(96)) + (0._dp,6._dp)* (-T_Lor(85) + T_Lor(99))
  T_LC(79) = -T_Lor(115) - T_Lor(124) + T_Lor(75) + T_Lor(78) + 3._dp * (T_Lor(103) - T_Lor(83) - T_Lor(88) + T_Lor(96))
  T_LC(80) = -T_Lor(115) + T_Lor(124) + T_Lor(75) + (0._dp,2._dp)* (T_Lor(119) - T_Lor(76)) - T_Lor(78) + &
             3._dp * (-T_Lor(103) - T_Lor(83) + T_Lor(88) + T_Lor(96)) + (0._dp,6._dp)* (T_Lor(85) - T_Lor(99))
  T_LC(81) = T_Lor(105) + T_Lor(126) + T_Lor(71) + T_Lor(74) + 2._dp * (-T_Lor(80) - T_Lor(90))
  T_LC(82) = -T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (-T_Lor(125) - T_Lor(73)) + 2._dp * T_Lor(86) + &
             (0._dp,2._dp)* T_Lor(89)
  T_LC(83) = -T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (T_Lor(125) + T_Lor(73)) + 2._dp * T_Lor(86) - &
             (0._dp,2._dp)* T_Lor(89)
  T_LC(84) = T_Lor(103) + T_Lor(115) - T_Lor(124) + T_Lor(75) - T_Lor(78) - T_Lor(83) + T_Lor(88) - T_Lor(96) + &
             (0._dp,2._dp)* (T_Lor(119) + T_Lor(76) - T_Lor(85) - T_Lor(99))
  T_LC(85) = -T_Lor(103) + T_Lor(115) + T_Lor(124) + T_Lor(75) + T_Lor(78) - T_Lor(83) - T_Lor(88) - T_Lor(96)
  T_LC(86) = T_Lor(103) + T_Lor(115) - T_Lor(124) + T_Lor(75) - T_Lor(78) - T_Lor(83) + T_Lor(88) - T_Lor(96) + &
             (0._dp,2._dp)* (-T_Lor(119) - T_Lor(76) + T_Lor(85) + T_Lor(99))
  T_LC(87) = -((0._dp,2._dp)* T_Lor(102)) - T_Lor(111) - T_Lor(81) + (0._dp,3._dp)* (-T_Lor(114) - T_Lor(82)) + &
             3._dp * (T_Lor(118) + T_Lor(84)) + (0._dp,1._dp)* (T_Lor(123) + T_Lor(87)) + 2._dp * T_Lor(93) + &
             (0._dp,6._dp)* T_Lor(95) - 6._dp * T_Lor(98)
  T_LC(88) = -T_Lor(111) - T_Lor(118) - T_Lor(81) - T_Lor(84) + (0._dp,1._dp)* (-T_Lor(114) - T_Lor(123) - T_Lor(82) - &
             T_Lor(87)) + (0._dp,2._dp)* (T_Lor(102) + T_Lor(95)) + 2._dp * (T_Lor(93) + T_Lor(98))
  T_LC(89) = -T_Lor(111) - T_Lor(118) - T_Lor(81) - T_Lor(84) + (0._dp,1._dp)* (T_Lor(114) + T_Lor(123) + T_Lor(82) + &
             T_Lor(87)) + (0._dp,2._dp)* (-T_Lor(102) - T_Lor(95)) + 2._dp * (T_Lor(93) + T_Lor(98))
  T_LC(90) = (0._dp,2._dp)* T_Lor(102) - T_Lor(111) - T_Lor(81) + (0._dp,3._dp)* (T_Lor(114) + T_Lor(82)) + &
             3._dp * (T_Lor(118) + T_Lor(84)) + (0._dp,1._dp)* (-T_Lor(123) - T_Lor(87)) + 2._dp * T_Lor(93) - &
             (0._dp,6._dp)* T_Lor(95) - 6._dp * T_Lor(98)
  T_LC(91) = -T_Lor(126) + T_Lor(71) + 3._dp * (-T_Lor(105) + T_Lor(74)) + 2._dp * (T_Lor(80) - T_Lor(90))
  T_LC(92) = T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (T_Lor(125) - T_Lor(73)) + 2._dp * (T_Lor(100) - T_Lor(77)) + &
             (0._dp,2._dp)* (T_Lor(104) - T_Lor(79))
  T_LC(93) = T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (-T_Lor(125) + T_Lor(73)) + 2._dp * (T_Lor(100) - T_Lor(77)) + &
             (0._dp,2._dp)* (-T_Lor(104) + T_Lor(79))
  T_LC(94) = T_Lor(103) - T_Lor(115) + T_Lor(124) + T_Lor(75) - T_Lor(78) + T_Lor(83) - T_Lor(88) - T_Lor(96) + &
             (0._dp,2._dp)* (-T_Lor(119) + T_Lor(76) + T_Lor(85) - T_Lor(99))
  T_LC(95) = -T_Lor(103) - T_Lor(115) - T_Lor(124) + T_Lor(75) + T_Lor(78) + T_Lor(83) + T_Lor(88) - T_Lor(96)
  T_LC(96) = T_Lor(103) - T_Lor(115) + T_Lor(124) + T_Lor(75) - T_Lor(78) + T_Lor(83) - T_Lor(88) - T_Lor(96) + &
             (0._dp,2._dp)* (T_Lor(119) - T_Lor(76) - T_Lor(85) + T_Lor(99))
  T_LC(97) = T_Lor(111) - T_Lor(81) + (0._dp,3._dp)* (T_Lor(114) - T_Lor(82)) + 3._dp * (-T_Lor(118) + T_Lor(84)) + &
             (0._dp,1._dp)* (-T_Lor(123) + T_Lor(87))
  T_LC(98) = T_Lor(111) + T_Lor(118) - T_Lor(81) - T_Lor(84) + (0._dp,1._dp)* (T_Lor(114) + T_Lor(123) -T_Lor(82)-T_Lor(87))
  T_LC(99) = T_Lor(111) + T_Lor(118) - T_Lor(81) - T_Lor(84) + (0._dp,1._dp)*(-T_Lor(114) - T_Lor(123) +T_Lor(82)+T_Lor(87))
  T_LC(100) = T_Lor(111) - T_Lor(81) + (0._dp,3._dp)* (-T_Lor(114) + T_Lor(82)) + 3._dp * (-T_Lor(118) + T_Lor(84)) + &
              (0._dp,1._dp)* (T_Lor(123) - T_Lor(87))
  T_LC(101) = T_Lor(101) - T_Lor(108) - T_Lor(122) + T_Lor(91) + 6._dp * (T_Lor(113) - T_Lor(94)) + &
              (0._dp,4._dp)* (-T_Lor(110) + T_Lor(117) + T_Lor(92) - T_Lor(97))
  T_LC(102) = -T_Lor(101) - T_Lor(108) + T_Lor(122) + T_Lor(91) + (0._dp,2._dp)* (-T_Lor(110) - T_Lor(117) + &
               T_Lor(92) + T_Lor(97))
  T_LC(103) = T_Lor(101) - T_Lor(108) - T_Lor(122) + T_Lor(91) + 2._dp * (-T_Lor(113) + T_Lor(94))
  T_LC(104) = -T_Lor(101) - T_Lor(108) + T_Lor(122) + T_Lor(91) + (0._dp,2._dp)* (T_Lor(110) + T_Lor(117) - &
              T_Lor(92) - T_Lor(97))
  T_LC(105) = T_Lor(101) - T_Lor(108) - T_Lor(122) + T_Lor(91) + 6._dp * (T_Lor(113) - T_Lor(94)) + &
              (0._dp,4._dp)* (T_Lor(110) - T_Lor(117) - T_Lor(92) + T_Lor(97))
  T_LC(106) = T_Lor(126) + T_Lor(71) + 5._dp * (T_Lor(105) + T_Lor(74)) + 10._dp * (T_Lor(80) + T_Lor(90))
  T_LC(107) = -T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (-T_Lor(125) - T_Lor(73)) + 4._dp * (-T_Lor(100) - T_Lor(77)) + &
              (0._dp,4._dp)* (-T_Lor(104) - T_Lor(79)) - 6._dp * T_Lor(86) - (0._dp,6._dp)* T_Lor(89)
  T_LC(108) = -T_Lor(120) - T_Lor(72) + (0._dp,1._dp)* (T_Lor(125) + T_Lor(73)) + 4._dp * (-T_Lor(100) - T_Lor(77)) + &
              (0._dp,4._dp)* (T_Lor(104) + T_Lor(79)) - 6._dp * T_Lor(86) + (0._dp,6._dp)* T_Lor(89)
  T_LC(109) = T_Lor(115) - T_Lor(124) + T_Lor(75) + (0._dp,2._dp)* (T_Lor(119) + T_Lor(76)) - T_Lor(78) + &
              3._dp * (-T_Lor(103) + T_Lor(83) - T_Lor(88) + T_Lor(96)) + (0._dp,6._dp)* (T_Lor(85) + T_Lor(99))
  T_LC(110) = T_Lor(115) + T_Lor(124) + T_Lor(75) + T_Lor(78) + 3._dp * (T_Lor(103) + T_Lor(83) + T_Lor(88) + T_Lor(96))
  T_LC(111) = T_Lor(115) - T_Lor(124) + T_Lor(75) + (0._dp,2._dp)* (-T_Lor(119) - T_Lor(76)) - T_Lor(78) + &
              3._dp * (-T_Lor(103) + T_Lor(83) - T_Lor(88) + T_Lor(96)) + (0._dp,6._dp)* (-T_Lor(85) - T_Lor(99))
  T_LC(112) = (0._dp,2._dp)* T_Lor(102) - T_Lor(111) - T_Lor(81) + (0._dp,3._dp)* (-T_Lor(114) - T_Lor(82)) + &
              3._dp * (T_Lor(118) + T_Lor(84)) + (0._dp,1._dp)* (T_Lor(123) + T_Lor(87)) - 2._dp * T_Lor(93) - &
              (0._dp,6._dp)* T_Lor(95) + 6._dp * T_Lor(98)
  T_LC(113) = -T_Lor(111) - T_Lor(118) - T_Lor(81) - T_Lor(84) + (0._dp,1._dp)* (-T_Lor(114) - T_Lor(123) - T_Lor(82) - &
              T_Lor(87)) + (0._dp,2._dp)* (-T_Lor(102) - T_Lor(95)) + 2._dp * (-T_Lor(93) - T_Lor(98))
  T_LC(114) = -T_Lor(111) - T_Lor(118) - T_Lor(81) - T_Lor(84) + (0._dp,1._dp)* (T_Lor(114) + T_Lor(123) + T_Lor(82) + &
              T_Lor(87)) + (0._dp,2._dp)* (T_Lor(102) + T_Lor(95)) + 2._dp * (-T_Lor(93) - T_Lor(98))
  T_LC(115) = -((0._dp,2._dp)* T_Lor(102)) - T_Lor(111) - T_Lor(81) + (0._dp,3._dp)* (T_Lor(114) + T_Lor(82)) + &
              3._dp * (T_Lor(118) + T_Lor(84)) + (0._dp,1._dp)* (-T_Lor(123) - T_Lor(87)) - 2._dp * T_Lor(93) + &
              (0._dp,6._dp)* T_Lor(95) + 6._dp * T_Lor(98)
  T_LC(116) = T_Lor(101) + T_Lor(108) + T_Lor(122) + T_Lor(91) + 6._dp * (-T_Lor(113) - T_Lor(94)) + &
              (0._dp,4._dp)* (T_Lor(110) - T_Lor(117) + T_Lor(92) - T_Lor(97))
  T_LC(117) = -T_Lor(101) + T_Lor(108) - T_Lor(122) + T_Lor(91) + (0._dp,2._dp)* (T_Lor(110) + &
              T_Lor(117) + T_Lor(92) + T_Lor(97))
  T_LC(118) = T_Lor(101) + T_Lor(108) + T_Lor(122) + T_Lor(91) + 2._dp * (T_Lor(113) + T_Lor(94))
  T_LC(119) = -T_Lor(101) + T_Lor(108) - T_Lor(122) + T_Lor(91) + (0._dp,2._dp)* (-T_Lor(110) - &
              T_Lor(117) - T_Lor(92) - T_Lor(97))
  T_LC(120) = T_Lor(101) + T_Lor(108) + T_Lor(122) + T_Lor(91) + 6._dp * (-T_Lor(113) - T_Lor(94)) + &
              (0._dp,4._dp)* (-T_Lor(110) + T_Lor(117) - T_Lor(92) + T_Lor(97))
  T_LC(121) = -T_Lor(106) - (0._dp,5._dp)* T_Lor(107) + 10._dp * T_Lor(109) + (0._dp,10._dp)* T_Lor(112) - &
              5._dp * T_Lor(116) - (0._dp,1._dp)* T_Lor(121)
  T_LC(122) = -T_Lor(106) - (0._dp,3._dp)* T_Lor(107) + 2._dp * T_Lor(109) - (0._dp,2._dp)* T_Lor(112) + &
              3._dp * T_Lor(116) + (0._dp,1._dp)* T_Lor(121)
  T_LC(123) = -T_Lor(106) - 2._dp * T_Lor(109) - (0._dp,2._dp)* T_Lor(112) - T_Lor(116) + &
              (0._dp,1._dp)* (-T_Lor(107) - T_Lor(121))
  T_LC(124) = -T_Lor(106) - 2._dp * T_Lor(109) + (0._dp,2._dp)* T_Lor(112) - T_Lor(116) + &
              (0._dp,1._dp)* (T_Lor(107) + T_Lor(121))
  T_LC(125) = -T_Lor(106) + (0._dp,3._dp)* T_Lor(107) + 2._dp * T_Lor(109) + (0._dp,2._dp)* T_Lor(112) + &
              3._dp * T_Lor(116) - (0._dp,1._dp)* T_Lor(121)
  T_LC(126) = -T_Lor(106) + (0._dp,5._dp)* T_Lor(107) + 10._dp * T_Lor(109) - (0._dp,10._dp)* T_Lor(112) - &
              5._dp * T_Lor(116) + (0._dp,1._dp)* T_Lor(121)
  T_LC(127) = T_Lor(127) - 20._dp * T_Lor(146) + 15._dp * (T_Lor(136) + T_Lor(161)) + 6._dp * (-T_Lor(130) - T_Lor(182))+T_Lor(210)
  T_LC(128) = T_Lor(127) + 5._dp * (T_Lor(136) - T_Lor(161)) + 4._dp * (-T_Lor(130) + T_Lor(182)) - T_Lor(210)
  T_LC(129) = -T_Lor(128) + 10._dp * (-T_Lor(142) + T_Lor(156)) + (0._dp,10._dp)* (-T_Lor(145) + T_Lor(160)) + &
              5._dp * (T_Lor(133) - T_Lor(176)) + (0._dp,5._dp)* (T_Lor(135) - T_Lor(181)) + T_Lor(203) + &
              (0._dp,1._dp)* (-T_Lor(129) + T_Lor(209))
  T_LC(130) = -T_Lor(128) + 10._dp * (-T_Lor(142) + T_Lor(156)) + (0._dp,10._dp)* (T_Lor(145) - T_Lor(160)) + &
              5._dp * (T_Lor(133) - T_Lor(176)) + (0._dp,5._dp)* (-T_Lor(135) + T_Lor(181)) + T_Lor(203) + &
              (0._dp,1._dp)* (T_Lor(129) - T_Lor(209))
  T_LC(131) = T_Lor(127) - T_Lor(136) + 4._dp * T_Lor(146) - T_Lor(161) + 2._dp * (-T_Lor(130) - T_Lor(182)) + T_Lor(210)
  T_LC(132) = -T_Lor(128) + 2._dp * (-T_Lor(142) - T_Lor(156)) + (0._dp,2._dp)* (-T_Lor(145) - T_Lor(160)) + &
              3._dp * (T_Lor(133) + T_Lor(176)) + (0._dp,3._dp)* (T_Lor(135) + T_Lor(181)) - T_Lor(203) + &
              (0._dp,1._dp)* (-T_Lor(129) - T_Lor(209))
  T_LC(133) = -T_Lor(128) + 2._dp * (-T_Lor(142) - T_Lor(156)) + (0._dp,2._dp)* (T_Lor(145) + T_Lor(160)) + &
              3._dp * (T_Lor(133) + T_Lor(176)) + (0._dp,3._dp)* (-T_Lor(135) - T_Lor(181)) - T_Lor(203) + &
              (0._dp,1._dp)* (T_Lor(129) + T_Lor(209))
  T_LC(134) = T_Lor(131) - T_Lor(134) + (0._dp,12._dp)* T_Lor(155) + 6._dp * (T_Lor(152) - T_Lor(159)) + &
              (0._dp,8._dp)* (-T_Lor(141) - T_Lor(175)) + 4._dp * (-T_Lor(139) + T_Lor(144) - T_Lor(171) + T_Lor(180)) + &
              T_Lor(197) + (0._dp,2._dp)* (T_Lor(132) + T_Lor(202)) - T_Lor(208)
  T_LC(135) = T_Lor(131) + T_Lor(134) + 6._dp * (T_Lor(152) + T_Lor(159)) + 4._dp * (-T_Lor(139) - T_Lor(144) - T_Lor(171) - &
              T_Lor(180)) + T_Lor(197) + T_Lor(208)
  T_LC(136) = T_Lor(131) - T_Lor(134) - (0._dp,12._dp)* T_Lor(155) + 6._dp * (T_Lor(152) - T_Lor(159)) + &
              (0._dp,8._dp)* (T_Lor(141) + T_Lor(175)) + 4._dp * (-T_Lor(139) + T_Lor(144) - T_Lor(171) + T_Lor(180)) + &
              T_Lor(197) + (0._dp,2._dp)* (-T_Lor(132) - T_Lor(202)) - T_Lor(208)
  T_LC(137) = T_Lor(127) + 3._dp * (-T_Lor(136) + T_Lor(161)) - T_Lor(210)
  T_LC(138) = -T_Lor(128) + T_Lor(133) + 2._dp * (T_Lor(142) - T_Lor(156)) + (0._dp,2._dp)* (T_Lor(145) - T_Lor(160)) - &
              T_Lor(176) + T_Lor(203) + (0._dp,1._dp)* (-T_Lor(129) + T_Lor(135) - T_Lor(181) + T_Lor(209))
  T_LC(139) = -T_Lor(128) + T_Lor(133) + 2._dp * (T_Lor(142) - T_Lor(156)) + (0._dp,2._dp)* (-T_Lor(145) + T_Lor(160)) - &
              T_Lor(176) + T_Lor(203) + (0._dp,1._dp)* (T_Lor(129) - T_Lor(135) + T_Lor(181) - T_Lor(209))
  T_LC(140) = T_Lor(131) - T_Lor(134) + (0._dp,4._dp)* (-T_Lor(141) + T_Lor(175)) + 2._dp * (-T_Lor(139) + T_Lor(144) + &
              T_Lor(171) - T_Lor(180)) - T_Lor(197) + (0._dp,2._dp)* (T_Lor(132) - T_Lor(202)) + T_Lor(208)
  T_LC(141) = T_Lor(131) + T_Lor(134) + 2._dp * (-T_Lor(139) - T_Lor(144) + T_Lor(171) + T_Lor(180)) - T_Lor(197) - T_Lor(208)
  T_LC(142) = T_Lor(131) - T_Lor(134) + (0._dp,4._dp)* (T_Lor(141) - T_Lor(175)) + 2._dp * (-T_Lor(139) + T_Lor(144) + &
              T_Lor(171) - T_Lor(180)) - T_Lor(197) + (0._dp,2._dp)* (-T_Lor(132) + T_Lor(202)) + T_Lor(208)
  T_LC(143) = -T_Lor(137) + (0._dp,9._dp)* (T_Lor(151) - T_Lor(170)) + 9._dp * (-T_Lor(154) + T_Lor(174)) + T_Lor(192) + &
              (0._dp,3._dp)* (-T_Lor(138) - T_Lor(158) + T_Lor(179) + T_Lor(196)) + 3._dp * (T_Lor(140) + T_Lor(149) - &
              T_Lor(167) - T_Lor(201)) + (0._dp,1._dp)* (T_Lor(143) - T_Lor(207))
  T_LC(144) = -T_Lor(137) - T_Lor(140) + 3._dp * (T_Lor(149) + T_Lor(154) - T_Lor(167) - T_Lor(174)) + &
              (0._dp,3._dp)* (T_Lor(151) + T_Lor(158) - T_Lor(170) - T_Lor(179)) + T_Lor(192) + T_Lor(201) + &
              (0._dp,1._dp)* (-T_Lor(138) - T_Lor(143) + T_Lor(196) + T_Lor(207))
  T_LC(145) = -T_Lor(137) - T_Lor(140) + 3._dp * (T_Lor(149) + T_Lor(154) - T_Lor(167) - T_Lor(174)) + &
              (0._dp,3._dp)* (-T_Lor(151) - T_Lor(158) + T_Lor(170) + T_Lor(179)) + T_Lor(192) + T_Lor(201) + &
              (0._dp,1._dp)* (T_Lor(138) + T_Lor(143) - T_Lor(196) - T_Lor(207))
  T_LC(146) = -T_Lor(137) + (0._dp,9._dp)* (-T_Lor(151) + T_Lor(170)) + 9._dp * (-T_Lor(154) + T_Lor(174)) + T_Lor(192) + &
              (0._dp,3._dp)* (T_Lor(138) + T_Lor(158) - T_Lor(179) - T_Lor(196)) + 3._dp * (T_Lor(140) + T_Lor(149) - &
              T_Lor(167) - T_Lor(201)) + (0._dp,1._dp)* (-T_Lor(143) + T_Lor(207))
  T_LC(147) = T_Lor(127) - T_Lor(136) - 4._dp * T_Lor(146) - T_Lor(161) + 2._dp * (T_Lor(130) + T_Lor(182)) + T_Lor(210)
  T_LC(148) = -T_Lor(128) - T_Lor(133) + 2._dp * (T_Lor(142) + T_Lor(156)) + (0._dp,2._dp)* (T_Lor(145) + T_Lor(160)) - &
              T_Lor(176) - T_Lor(203) + (0._dp,1._dp)* (-T_Lor(129) - T_Lor(135) - T_Lor(181) - T_Lor(209))
  T_LC(149) = -T_Lor(128) - T_Lor(133) + 2._dp * (T_Lor(142) + T_Lor(156)) + (0._dp,2._dp)* (-T_Lor(145) - T_Lor(160)) - &
              T_Lor(176) - T_Lor(203) + (0._dp,1._dp)* (T_Lor(129) + T_Lor(135) + T_Lor(181) + T_Lor(209))
  T_LC(150) = T_Lor(131) - T_Lor(134) - (0._dp,4._dp)* T_Lor(155) + 2._dp * (-T_Lor(152) + T_Lor(159)) + T_Lor(197) + &
              (0._dp,2._dp)* (T_Lor(132) + T_Lor(202)) - T_Lor(208)
  T_LC(151) = T_Lor(131) + T_Lor(134) + 2._dp * (-T_Lor(152) - T_Lor(159)) + T_Lor(197) + T_Lor(208)
  T_LC(152) = T_Lor(131) - T_Lor(134) + (0._dp,4._dp)* T_Lor(155) + 2._dp * (-T_Lor(152) + T_Lor(159)) + T_Lor(197) + &
              (0._dp,2._dp)* (-T_Lor(132) - T_Lor(202)) - T_Lor(208)
  T_LC(153) = -T_Lor(137) + T_Lor(149) + T_Lor(167) - T_Lor(192) + (0._dp,3._dp)* (-T_Lor(138) + T_Lor(151) + T_Lor(170) &
              - T_Lor(196)) + 3._dp * (T_Lor(140) - T_Lor(154) - T_Lor(174) + T_Lor(201)) + (0._dp,1._dp)* (T_Lor(143) - &
              T_Lor(158) - T_Lor(179) + T_Lor(207))
  T_LC(154) = -T_Lor(137) - T_Lor(140) + T_Lor(149) + T_Lor(154) + T_Lor(167) + T_Lor(174) - T_Lor(192) - T_Lor(201) + &
              (0._dp,1._dp)* (-T_Lor(138) - T_Lor(143) + T_Lor(151) + T_Lor(158)+T_Lor(170)+T_Lor(179)-T_Lor(196)-T_Lor(207))
  T_LC(155) = -T_Lor(137) - T_Lor(140) + T_Lor(149) + T_Lor(154) + T_Lor(167) + T_Lor(174) - T_Lor(192) - T_Lor(201) + &
              (0._dp,1._dp)* (T_Lor(138) + T_Lor(143) - T_Lor(151) - T_Lor(158) - T_Lor(170) - T_Lor(179) + &
              T_Lor(196) + T_Lor(207))
  T_LC(156) = -T_Lor(137) + T_Lor(149) + T_Lor(167) - T_Lor(192) + (0._dp,3._dp)* (T_Lor(138) - T_Lor(151) - T_Lor(170) + &
              T_Lor(196)) + 3._dp * (T_Lor(140) - T_Lor(154) - T_Lor(174) + T_Lor(201)) + (0._dp,1._dp)* (-T_Lor(143) + &
              T_Lor(158) + T_Lor(179) - T_Lor(207))
  T_LC(157) = T_Lor(147) + T_Lor(157) + 12._dp * T_Lor(169) + (0._dp,8._dp)* (-T_Lor(166) + T_Lor(173)) + &
              2._dp * (-T_Lor(164) - T_Lor(178)) + T_Lor(188) + 6._dp * (-T_Lor(150) - T_Lor(195)) + &
              (0._dp,4._dp)* (T_Lor(148) - T_Lor(153) + T_Lor(191) - T_Lor(200)) + T_Lor(206)
  T_LC(158) = T_Lor(147) - T_Lor(157) + (0._dp,4._dp)* (-T_Lor(166) - T_Lor(173)) + 2._dp * (-T_Lor(164) + T_Lor(178)) + &
              T_Lor(188) + (0._dp,2._dp)* (T_Lor(148) + T_Lor(153) + T_Lor(191) + T_Lor(200)) - T_Lor(206)
  T_LC(159) = T_Lor(147) + T_Lor(157) - 4._dp * T_Lor(169) + T_Lor(188) + 2._dp * (T_Lor(150) - T_Lor(164) - T_Lor(178) + &
              T_Lor(195)) + T_Lor(206)
  T_LC(160) = T_Lor(147) - T_Lor(157) + (0._dp,4._dp)* (T_Lor(166) + T_Lor(173)) + 2._dp * (-T_Lor(164) + T_Lor(178)) + &
              T_Lor(188) + (0._dp,2._dp)* (-T_Lor(148) - T_Lor(153) - T_Lor(191) - T_Lor(200)) - T_Lor(206)
  T_LC(161) = T_Lor(147) + T_Lor(157) + 12._dp * T_Lor(169) + (0._dp,8._dp)* (T_Lor(166) - T_Lor(173)) + &
              2._dp * (-T_Lor(164) - T_Lor(178)) + T_Lor(188) + 6._dp * (-T_Lor(150) - T_Lor(195)) + &
              (0._dp,4._dp)* (-T_Lor(148) + T_Lor(153) - T_Lor(191) + T_Lor(200)) + T_Lor(206)
  T_LC(162) = T_Lor(127) + 5._dp * (T_Lor(136) - T_Lor(161)) + 4._dp * (T_Lor(130) - T_Lor(182)) - T_Lor(210)
  T_LC(163) = -T_Lor(128) + 2._dp * (-T_Lor(142) + T_Lor(156)) + (0._dp,2._dp)* (-T_Lor(145) + T_Lor(160)) + &
              3._dp * (-T_Lor(133) + T_Lor(176)) + (0._dp,3._dp)* (-T_Lor(135) + T_Lor(181)) + T_Lor(203) + &
              (0._dp,1._dp)* (-T_Lor(129) + T_Lor(209))
  T_LC(164) = -T_Lor(128) + 2._dp * (-T_Lor(142) + T_Lor(156)) + (0._dp,2._dp)* (T_Lor(145) - T_Lor(160)) + &
              3._dp * (-T_Lor(133) + T_Lor(176)) + (0._dp,3._dp)* (T_Lor(135) - T_Lor(181)) + T_Lor(203) + &
              (0._dp,1._dp)* (T_Lor(129) - T_Lor(209))
  T_LC(165) = T_Lor(131) - T_Lor(134) + (0._dp,4._dp)* (T_Lor(141) - T_Lor(175)) + 2._dp * (T_Lor(139) - T_Lor(144) - &
              T_Lor(171) + T_Lor(180)) - T_Lor(197) + (0._dp,2._dp)* (T_Lor(132) - T_Lor(202)) + T_Lor(208)
  T_LC(166) = T_Lor(131) + T_Lor(134) + 2._dp * (T_Lor(139) + T_Lor(144) - T_Lor(171) - T_Lor(180)) - T_Lor(197) - T_Lor(208)
  T_LC(167) = T_Lor(131) - T_Lor(134) + (0._dp,4._dp)* (-T_Lor(141) + T_Lor(175)) + 2._dp * (T_Lor(139) - T_Lor(144) - &
              T_Lor(171) + T_Lor(180)) - T_Lor(197) + (0._dp,2._dp)* (-T_Lor(132) + T_Lor(202)) + T_Lor(208)
  T_LC(168) = -T_Lor(137) - T_Lor(149) + T_Lor(167) + T_Lor(192) + (0._dp,3._dp)* (-T_Lor(138) - T_Lor(151) + T_Lor(170) + &
              T_Lor(196)) + 3._dp * (T_Lor(140) + T_Lor(154) - T_Lor(174) - T_Lor(201)) + (0._dp,1._dp)* (T_Lor(143) + &
              T_Lor(158) - T_Lor(179) - T_Lor(207))
  T_LC(169) = -T_Lor(137) - T_Lor(140) - T_Lor(149) - T_Lor(154) + T_Lor(167) + T_Lor(174) + T_Lor(192) + T_Lor(201) + &
              (0._dp,1._dp)* (-T_Lor(138) - T_Lor(143) - T_Lor(151) - T_Lor(158) + T_Lor(170) + T_Lor(179) + &
              T_Lor(196) + T_Lor(207))
  T_LC(170) = -T_Lor(137) - T_Lor(140) - T_Lor(149) - T_Lor(154) + T_Lor(167) + T_Lor(174) + T_Lor(192) + T_Lor(201) + &
              (0._dp,1._dp)* (T_Lor(138) + T_Lor(143) + T_Lor(151) + T_Lor(158) - T_Lor(170) - T_Lor(179) - &
              T_Lor(196) - T_Lor(207))
  T_LC(171) = -T_Lor(137) - T_Lor(149) + T_Lor(167) + T_Lor(192) + (0._dp,3._dp)* (T_Lor(138) + T_Lor(151) - T_Lor(170) - &
              T_Lor(196)) + 3._dp * (T_Lor(140) + T_Lor(154) - T_Lor(174) - T_Lor(201)) + (0._dp,1._dp)* (-T_Lor(143) - &
              T_Lor(158) + T_Lor(179) + T_Lor(207))
  T_LC(172) = T_Lor(147) + T_Lor(157) - T_Lor(188) + 6._dp * (-T_Lor(150) + T_Lor(195)) + (0._dp,4._dp)* (T_Lor(148) - &
              T_Lor(153) - T_Lor(191) + T_Lor(200)) - T_Lor(206)
  T_LC(173) = T_Lor(147) - T_Lor(157) - T_Lor(188) + (0._dp,2._dp)* (T_Lor(148) + T_Lor(153) - T_Lor(191) - &
              T_Lor(200)) + T_Lor(206)
  T_LC(174) = T_Lor(147) + T_Lor(157) - T_Lor(188) + 2._dp * (T_Lor(150) - T_Lor(195)) - T_Lor(206)
  T_LC(175) = T_Lor(147) - T_Lor(157) - T_Lor(188) + (0._dp,2._dp)* (-T_Lor(148) - T_Lor(153) + T_Lor(191) + T_Lor(200)) + &
              T_Lor(206)
  T_LC(176) = T_Lor(147) + T_Lor(157) - T_Lor(188) + 6._dp * (-T_Lor(150) + T_Lor(195)) + (0._dp,4._dp)* (-T_Lor(148) + &
              T_Lor(153) + T_Lor(191) - T_Lor(200)) - T_Lor(206)
  T_LC(177) = -T_Lor(162) + T_Lor(185) + (0._dp,5._dp)* (-T_Lor(163) + T_Lor(187)) + 10._dp * (T_Lor(165) - T_Lor(190)) + &
              (0._dp,10._dp)* (T_Lor(168) - T_Lor(194)) + 5._dp * (-T_Lor(172) + T_Lor(199)) + &
              (0._dp,1._dp)* (-T_Lor(177) + T_Lor(205))
  T_LC(178) = -T_Lor(162) + T_Lor(185) + (0._dp,3._dp)* (-T_Lor(163) + T_Lor(187)) + 2._dp * (T_Lor(165) - T_Lor(190)) + &
              (0._dp,2._dp)* (-T_Lor(168) + T_Lor(194)) + 3._dp * (T_Lor(172) - T_Lor(199)) + &
              (0._dp,1._dp)* (T_Lor(177) - T_Lor(205))
  T_LC(179) = -T_Lor(162) - T_Lor(172) + T_Lor(185) + 2._dp * (-T_Lor(165) + T_Lor(190)) + (0._dp,2._dp)* (-T_Lor(168) + &
              T_Lor(194)) + T_Lor(199) + (0._dp,1._dp)* (-T_Lor(163) - T_Lor(177) + T_Lor(187) + T_Lor(205))
  T_LC(180) = -T_Lor(162) - T_Lor(172) + T_Lor(185) + 2._dp * (-T_Lor(165) + T_Lor(190)) + (0._dp,2._dp)* (T_Lor(168) - &
              T_Lor(194)) + T_Lor(199) + (0._dp,1._dp)* (T_Lor(163) + T_Lor(177) - T_Lor(187) - T_Lor(205))
  T_LC(181) = -T_Lor(162) + T_Lor(185) + (0._dp,3._dp)* (T_Lor(163) - T_Lor(187)) + 2._dp * (T_Lor(165) - T_Lor(190)) + &
              (0._dp,2._dp)* (T_Lor(168) - T_Lor(194)) + 3._dp * (T_Lor(172) - T_Lor(199)) + &
              (0._dp,1._dp)* (-T_Lor(177) + T_Lor(205))
  T_LC(182) = -T_Lor(162) + T_Lor(185) + (0._dp,5._dp)* (T_Lor(163) - T_Lor(187)) + 10._dp * (T_Lor(165) - T_Lor(190)) + &
              (0._dp,10._dp)* (-T_Lor(168) + T_Lor(194)) + 5._dp * (-T_Lor(172) + T_Lor(199)) + &
              (0._dp,1._dp)* (T_Lor(177) - T_Lor(205))
  T_LC(183) = T_Lor(127) + 20._dp * T_Lor(146) + 15._dp * (T_Lor(136) + T_Lor(161)) + 6._dp * (T_Lor(130) + T_Lor(182))+T_Lor(210)
  T_LC(184) = -T_Lor(128) + 10._dp * (-T_Lor(142) - T_Lor(156)) + (0._dp,10._dp)* (-T_Lor(145) - T_Lor(160)) + &
              5._dp * (-T_Lor(133) - T_Lor(176)) + (0._dp,5._dp)* (-T_Lor(135) - T_Lor(181)) - T_Lor(203) + &
              (0._dp,1._dp)* (-T_Lor(129) - T_Lor(209))
  T_LC(185) = -T_Lor(128) + 10._dp * (-T_Lor(142) - T_Lor(156)) + (0._dp,10._dp)* (T_Lor(145) + T_Lor(160)) + &
              5._dp * (-T_Lor(133) - T_Lor(176)) + (0._dp,5._dp)* (T_Lor(135) + T_Lor(181)) - T_Lor(203) + &
              (0._dp,1._dp)* (T_Lor(129) + T_Lor(209))
  T_LC(186) = T_Lor(131) - T_Lor(134) + (0._dp,12._dp)* T_Lor(155) + 6._dp * (T_Lor(152) - T_Lor(159)) + &
              (0._dp,8._dp)* (T_Lor(141) + T_Lor(175)) + 4._dp * (T_Lor(139) - T_Lor(144) + T_Lor(171) - T_Lor(180)) + &
              T_Lor(197) + (0._dp,2._dp)* (T_Lor(132) + T_Lor(202)) - T_Lor(208)
  T_LC(187) = T_Lor(131) + T_Lor(134) + 6._dp * (T_Lor(152) + T_Lor(159)) + 4._dp * (T_Lor(139) + T_Lor(144) + T_Lor(171) + &
              T_Lor(180)) + T_Lor(197) + T_Lor(208)
  T_LC(188) = T_Lor(131) - T_Lor(134) - (0._dp,12._dp)* T_Lor(155) + 6._dp * (T_Lor(152) - T_Lor(159)) + &
              (0._dp,8._dp)* (-T_Lor(141) - T_Lor(175)) + 4._dp * (T_Lor(139) - T_Lor(144) + T_Lor(171) - T_Lor(180)) + &
              T_Lor(197) + (0._dp,2._dp)* (-T_Lor(132) - T_Lor(202)) - T_Lor(208)
  T_LC(189) = -T_Lor(137) + (0._dp,9._dp)* (-T_Lor(151) - T_Lor(170)) + 9._dp * (T_Lor(154) + T_Lor(174)) - T_Lor(192) + &
              (0._dp,3._dp)* (-T_Lor(138) + T_Lor(158) + T_Lor(179) - T_Lor(196)) + 3._dp * (T_Lor(140) - T_Lor(149) - &
              T_Lor(167) + T_Lor(201)) + (0._dp,1._dp)* (T_Lor(143) + T_Lor(207))
  T_LC(190) = -T_Lor(137) - T_Lor(140) + 3._dp * (-T_Lor(149) - T_Lor(154) - T_Lor(167) - T_Lor(174)) + &
              (0._dp,3._dp)* (-T_Lor(151) - T_Lor(158) - T_Lor(170) - T_Lor(179)) - T_Lor(192) - T_Lor(201) + &
              (0._dp,1._dp)* (-T_Lor(138) - T_Lor(143) - T_Lor(196) - T_Lor(207))
  T_LC(191) = -T_Lor(137) - T_Lor(140) + 3._dp * (-T_Lor(149) - T_Lor(154) - T_Lor(167) - T_Lor(174)) + &
              (0._dp,3._dp)* (T_Lor(151) + T_Lor(158) + T_Lor(170) + T_Lor(179)) - T_Lor(192) - T_Lor(201) + &
              (0._dp,1._dp)* (T_Lor(138) + T_Lor(143) + T_Lor(196) + T_Lor(207))
  T_LC(192) = -T_Lor(137) + (0._dp,9._dp)* (T_Lor(151) + T_Lor(170)) + 9._dp * (T_Lor(154) + T_Lor(174)) - T_Lor(192) + &
              (0._dp,3._dp)* (T_Lor(138) - T_Lor(158) - T_Lor(179) + T_Lor(196)) + 3._dp * (T_Lor(140) - T_Lor(149) - &
              T_Lor(167) + T_Lor(201)) + (0._dp,1._dp)* (-T_Lor(143) - T_Lor(207))
  T_LC(193) = T_Lor(147) + T_Lor(157) - 12._dp * T_Lor(169) + (0._dp,8._dp)* (T_Lor(166) - T_Lor(173)) + &
              2._dp * (T_Lor(164) + T_Lor(178)) + T_Lor(188) + 6._dp * (-T_Lor(150) - T_Lor(195)) + &
              (0._dp,4._dp)* (T_Lor(148) - T_Lor(153) + T_Lor(191) - T_Lor(200)) + T_Lor(206)
  T_LC(194) = T_Lor(147) - T_Lor(157) + (0._dp,4._dp)* (T_Lor(166) + T_Lor(173)) + 2._dp * (T_Lor(164) - T_Lor(178)) + &
              T_Lor(188) + (0._dp,2._dp)* (T_Lor(148) + T_Lor(153) + T_Lor(191) + T_Lor(200)) - T_Lor(206)
  T_LC(195) = T_Lor(147) + T_Lor(157) + 4._dp * T_Lor(169) + T_Lor(188) + 2._dp * (T_Lor(150) + T_Lor(164) + T_Lor(178) + &
              T_Lor(195)) + T_Lor(206)
  T_LC(196) = T_Lor(147) - T_Lor(157) + (0._dp,4._dp)* (-T_Lor(166) - T_Lor(173)) + 2._dp * (T_Lor(164) - T_Lor(178)) + &
              T_Lor(188) + (0._dp,2._dp)* (-T_Lor(148) - T_Lor(153) - T_Lor(191) - T_Lor(200)) - T_Lor(206)
  T_LC(197) = T_Lor(147) + T_Lor(157) - 12._dp * T_Lor(169) + (0._dp,8._dp)* (-T_Lor(166) + T_Lor(173)) + &
              2._dp * (T_Lor(164) + T_Lor(178)) + T_Lor(188) + 6._dp * (-T_Lor(150) - T_Lor(195)) + &
              (0._dp,4._dp)* (-T_Lor(148) + T_Lor(153) - T_Lor(191) + T_Lor(200)) + T_Lor(206)
  T_LC(198) = -T_Lor(162) - T_Lor(185) + (0._dp,5._dp)* (-T_Lor(163) - T_Lor(187)) + 10._dp * (T_Lor(165) + T_Lor(190)) + &
              (0._dp,10._dp)* (T_Lor(168) + T_Lor(194)) + 5._dp * (-T_Lor(172) - T_Lor(199)) + &
              (0._dp,1._dp)* (-T_Lor(177) - T_Lor(205))
  T_LC(199) = -T_Lor(162) - T_Lor(185) + (0._dp,3._dp)* (-T_Lor(163) - T_Lor(187)) + 2._dp * (T_Lor(165) + T_Lor(190)) + &
              (0._dp,2._dp)* (-T_Lor(168) - T_Lor(194)) + 3._dp * (T_Lor(172) + T_Lor(199)) + &
              (0._dp,1._dp)* (T_Lor(177) + T_Lor(205))
  T_LC(200) = -T_Lor(162) - T_Lor(172) - T_Lor(185) + 2._dp * (-T_Lor(165) - T_Lor(190)) + (0._dp,2._dp)* (-T_Lor(168) - &
              T_Lor(194)) - T_Lor(199) + (0._dp,1._dp)* (-T_Lor(163) - T_Lor(177) - T_Lor(187) - T_Lor(205))
  T_LC(201) = -T_Lor(162) - T_Lor(172) - T_Lor(185) + 2._dp * (-T_Lor(165) - T_Lor(190)) + (0._dp,2._dp)* (T_Lor(168) + &
               T_Lor(194)) - T_Lor(199) + (0._dp,1._dp)* (T_Lor(163) + T_Lor(177) + T_Lor(187) + T_Lor(205))
  T_LC(202) = -T_Lor(162) - T_Lor(185) + (0._dp,3._dp)* (T_Lor(163) + T_Lor(187)) + 2._dp * (T_Lor(165) + T_Lor(190)) + &
              (0._dp,2._dp)* (T_Lor(168) + T_Lor(194)) + 3._dp * (T_Lor(172) + T_Lor(199)) + &
              (0._dp,1._dp)* (-T_Lor(177) - T_Lor(205))
  T_LC(203) = -T_Lor(162) - T_Lor(185) + (0._dp,5._dp)* (T_Lor(163) + T_Lor(187)) + 10._dp * (T_Lor(165) + T_Lor(190)) + &
              (0._dp,10._dp)* (-T_Lor(168) - T_Lor(194)) + 5._dp * (-T_Lor(172) - T_Lor(199)) + &
              (0._dp,1._dp)* (T_Lor(177) + T_Lor(205))
  T_LC(204) = T_Lor(183) - (0._dp,20._dp)* T_Lor(189) + 15._dp * (-T_Lor(186) + T_Lor(193)) + &
              (0._dp,6._dp)* (T_Lor(184) + T_Lor(198)) - T_Lor(204)
  T_LC(205) = T_Lor(183) + 5._dp * (-T_Lor(186) - T_Lor(193)) + (0._dp,4._dp)* (T_Lor(184) - T_Lor(198)) + T_Lor(204)
  T_LC(206) = T_Lor(183) + T_Lor(186) + (0._dp,4._dp)* T_Lor(189) - T_Lor(193) + (0._dp,2._dp)* (T_Lor(184) + &
              T_Lor(198)) - T_Lor(204)
  T_LC(207) = T_Lor(183) + 3._dp * (T_Lor(186) + T_Lor(193)) + T_Lor(204)
  T_LC(208) = T_Lor(183) + T_Lor(186) - (0._dp,4._dp)* T_Lor(189) - T_Lor(193) + (0._dp,2._dp)* (-T_Lor(184) - &
              T_Lor(198)) - T_Lor(204)
  T_LC(209) = T_Lor(183) + 5._dp * (-T_Lor(186) - T_Lor(193)) + (0._dp,4._dp)* (-T_Lor(184) + T_Lor(198)) + T_Lor(204)
  T_LC(210) = T_Lor(183) + (0._dp,20._dp)* T_Lor(189) + 15._dp * (-T_Lor(186) + T_Lor(193)) + &
              (0._dp,6._dp)* (-T_Lor(184) - T_Lor(198)) - T_Lor(204)

end subroutine Std2LC_Tensor6

end module ol_Std2LC_converter_dp

