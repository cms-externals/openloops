
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


module ol_tensor_handling
  ! For compatibility with old process code only.
  ! TODO: remove this module as soon as all processes are regenerated.
  implicit none
  logical, save :: HR_not_initialised = .false.
  contains
  subroutine raise_rank_init()
    implicit none
  end subroutine raise_rank_init
end module ol_tensor_handling


module ol_tensor_bookkeeping
  use ol_generic, only: binomial, compositions
  implicit none
  integer, save :: initialised_rank = -1
  integer, allocatable, save :: rank_to_size(:), tensor_size(:), hr(:,:)
  contains

  function tensor_pos(a, b, c, d)
    implicit none
    integer :: tensor_pos
    integer, intent(in) :: a, b, c, d
    tensor_pos = (24*(1+d) &
              & + 12*(c+d)*(1+c+d) &
              & + 4*(b+c+d)*(1+b+c+d)*(2+b+c+d) &
              & + (a+b+c+d)*(1+a+b+c+d)*(2+a+b+c+d)*(3+a+b+c+d))/24
  end function tensor_pos

  subroutine init_tensorbookkeeping(maxrank)
    implicit none
    integer, intent(in) :: maxrank
    integer :: pos, r, i, compos(1:4,1:binomial(maxrank+3,3)), compo(4)
    if (allocated(rank_to_size)) then
      deallocate(rank_to_size)
      deallocate(tensor_size)
      deallocate(hr)
    end if
    allocate(rank_to_size(0:maxrank))
    allocate(tensor_size(-1:maxrank))
    allocate(hr(4,binomial(maxrank+3,4)))
    initialised_rank = maxrank
    pos = 0
    rank_to_size = [(binomial(r+3,3), r=0, maxrank)]
    tensor_size = [(binomial(r+4,4), r=-1, maxrank)]
    do r = 0, maxrank - 1
      compos(:,1:rank_to_size(r)) = compositions(r,4)
      do i = rank_to_size(r), 1, -1
        pos = pos + 1
        compo = compos(:,i)
        hr(1,pos) = tensor_pos(compo(1)+1,compo(2),compo(3),compo(4))
        hr(2,pos) = tensor_pos(compo(1),compo(2)+1,compo(3),compo(4))
        hr(3,pos) = tensor_pos(compo(1),compo(2),compo(3)+1,compo(4))
        hr(4,pos) = tensor_pos(compo(1),compo(2),compo(3),compo(4)+1)
      end do
    end do
  end subroutine init_tensorbookkeeping

end module ol_tensor_bookkeeping

