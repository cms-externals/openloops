!******************************************************************************!
! Copyright (C) 2014-2018 OpenLoops Collaboration. For authors see authors.txt !
!                                                                              !
! This file is part of OpenLoops.                                              !
!                                                                              !
! OpenLoops is free software: you can redistribute it and/or modify            !
! it under the terms of the GNU General Public License as published by         !
! the Free Software Foundation, either version 3 of the License, or            !
! (at your option) any later version.                                          !
!                                                                              !
! OpenLoops is distributed in the hope that it will be useful,                 !
! but WITHOUT ANY WARRANTY; without even the implied warranty of               !
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                !
! GNU General Public License for more details.                                 !
!                                                                              !
! You should have received a copy of the GNU General Public License            !
! along with OpenLoops.  If not, see <http://www.gnu.org/licenses/>.           !
!******************************************************************************!


module hol_initialisation_/**/REALKIND
  implicit none
  contains

!************************************************************************
subroutine hol_allocation(alpha, rank, beta, hel_states,ol_coeff, m)
!************************************************************************
! Allocation of the OpenLoops coefficient of type hol
!************************************************************************
! alpha      = dimension of the alpha-index array
! rank       = rank dimensionality (N=1 r=0, N=5 r=1,...)
! beta       = dimension of the beta-index array
! hel_states = number of helicity states
! ol_coeff   = type(hol) OpenLoops coefficient to be allocated in memory
! m          = number of ol_coeff with the same number of hel_stases to
!              be allocated
!************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: hol
  implicit none

  integer, intent(in) :: alpha, rank, beta, hel_states, m
  type(hol), intent(inout) :: ol_coeff(:)

  integer :: i

  do i = 1, m
    allocate(ol_coeff(i)%hf(hel_states))
    allocate(ol_coeff(i)%j(alpha, rank, beta, hel_states))
  end do

end subroutine hol_allocation

!************************************************************************
subroutine hol_deallocation(ol_coeff, m)
!************************************************************************
! Allocation of the OpenLoops coefficient of type hol
!************************************************************************
! alpha      = dimension of the alpha-index array
! rank       = rank dimensionality (N=1 r=0, N=5 r=1,...)
! beta       = dimension of the beta-index array
! hel_states = number of helicity states
! ol_coeff   = type(hol) OpenLoops coefficient to be allocated in memory
! m          = number of ol_coeff with the same number of hel_stases to
!              be allocated
!************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: hol
  implicit none

  integer, intent(in) :: m
  type(hol), intent(inout) :: ol_coeff(:)

  integer :: i

  do i = 1, m
    deallocate(ol_coeff(i)%hf)
    deallocate(ol_coeff(i)%j)
  end do

end subroutine hol_deallocation

!***********************************************************************
subroutine G0_hol_initialisation(ntry,G0coeff,ol_coeff,nhel_in,h0t,n_wf,w1,w2,w3,w4,w5)
!***********************************************************************
! Initialisation of the OpenLoops coefficient
!***********************************************************************
! G0coeff    = array of size given by the global number of helicity states
!              with the coefficients needed for the tree-level 1-loop interference
! hel_states = array with the helicity configurations
! ol_coeff   = OpenLoops coefficient to be initialised
! m0         = number of non-vanishing helicity states in this diagram
! h0t        = list of non-vanishing helicity states in this diagram
! n_wf       = number of subtrees attached to the full loop consisting of more than 1 external particles
! w1,...,w5  = arrays of hf (helicity) integers for the subtrees consisting of more than 1 external particles
!***********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: hol, Hpolcont, wfun
  use ol_loop_routines_/**/REALKIND, only: G0initialisationOLR
  use ol_debug, only: ol_fatal, ol_msg
  use ind_bookkeeping_/**/REALKIND, only : ProjHind
  implicit none

  integer(intkind1), intent(in)  :: ntry
  integer(intkind2), intent(inout) :: h0t(:), nhel_in
  integer, intent(in)  :: n_wf
  type(Hpolcont), intent(in)     :: G0coeff(:)
  type(hol), intent(inout)       :: ol_coeff
  type(wfun), optional, intent(in) :: w1(:), w2(:), w3(:), w4(:), w5(:)
  integer :: l, htot, sw, n
  integer(intkind2)  :: n_expart
  integer(intkind2)  :: G0_new_hf(size(G0coeff))
  integer(intkind2)  :: h1, h3
  integer(intkind2)  :: nhel_wf
  integer(intkind2)  :: hel3, hel1, subset
  logical :: check_hel_ol(size(G0coeff))

!***********************************************************************************
  if (ntry == 1) then

    htot = size(G0coeff)
    do l = 1, htot
      ol_coeff%hf(l) = G0coeff(l)%hf
      h0t(l)=l
    end do

    ! check number of non-vanishing helicity configurations of the input OL coefficient
    nhel_in = 0
    do h3 = 1, htot
      if (ol_coeff%hf(h3) /= -1_intkind2) nhel_in = nhel_in + 1
    end do

    !!!============================= 1 or more external subtrees
    if (n_wf >= 1) then
      sw=size(w1)
      ! Check number of non-vanishing helicity configurations of the external wavefunction
      nhel_wf = 0
      do h1 = 1, sw
        if (w1(h1)%hf /= -1_intkind2) nhel_wf = nhel_wf + 1
      end do

      subset = w1(1)%t
      n_expart = w1(1)%n_part
      check_hel_ol = .true.
      do h3=1, nhel_in
        hel3=ol_coeff%hf(h3)
        do h1=1, nhel_wf
          hel1 = w1(h1)%hf
          if (check_hel_ol(h3)) then
            if (ProjHind(subset,hel3,n_expart)==hel1) then
              if(check_hel_ol(h3)) check_hel_ol(h3)=.false.
            end if
          end if
        end do
      end do

      h3=1
      do while (h3 <= nhel_in)
        if(check_hel_ol(h3)) then
          if (h3 > 1) G0_new_hf(1:h3-1)=ol_coeff%hf(1:h3-1)
          G0_new_hf(h3:nhel_in-1)=ol_coeff%hf(h3+1:nhel_in)
          G0_new_hf(nhel_in)=-1_intkind2
          ol_coeff%hf(1:nhel_in)=G0_new_hf(1:nhel_in)
          h0t(h3:nhel_in-1)=h0t(h3+1:nhel_in)
          check_hel_ol(1:nhel_in-1)=check_hel_ol(2:nhel_in)
          nhel_in=nhel_in-1
        else
          h3=h3+1
        end if
      end do
    end if

  !!!============================= 2 or more external subtrees
    if (n_wf >= 2) then
      sw=size(w2)
      ! Check number of non-vanishing helicity configurations of the external wavefunction
      nhel_wf = 0
      do h1 = 1, sw
        if (w2(h1)%hf /= -1_intkind2) nhel_wf = nhel_wf + 1
      end do

      subset = w2(1)%t
      n_expart = w2(1)%n_part
      check_hel_ol = .true.

      do h3=1, nhel_in
        hel3=ol_coeff%hf(h3)
        do h1=1, nhel_wf
          hel1 = w2(h1)%hf
          if (check_hel_ol(h3)) then
            if (ProjHind(subset,hel3,n_expart)==hel1) then
              if(check_hel_ol(h3)) check_hel_ol(h3)=.false.
            end if
          end if
        end do
      end do

      h3=1
      do while (h3 <= nhel_in)
        if(check_hel_ol(h3)) then
          if (h3 > 1) G0_new_hf(1:h3-1)=ol_coeff%hf(1:h3-1)
          G0_new_hf(h3:nhel_in-1)=ol_coeff%hf(h3+1:nhel_in)
          G0_new_hf(nhel_in)=-1_intkind2
          ol_coeff%hf(1:nhel_in)=G0_new_hf(1:nhel_in)
          h0t(h3:nhel_in-1)=h0t(h3+1:nhel_in)
          check_hel_ol(1:nhel_in-1)=check_hel_ol(2:nhel_in)
          nhel_in=nhel_in-1
        else
          h3=h3+1
        end if
      end do
    end if
  !!!============================= 3 or more external subtrees
    if (n_wf >= 3) then
      sw=size(w3)
      ! Check number of non-vanishing helicity configurations of the external wavefunction
      nhel_wf = 0
      do h1 = 1, sw
        if (w3(h1)%hf /= -1_intkind2) nhel_wf = nhel_wf + 1
      end do

      subset = w3(1)%t
      n_expart = w3(1)%n_part
      check_hel_ol = .true.

      do h3=1, nhel_in
      hel3=ol_coeff%hf(h3)
        do h1=1, nhel_wf
          hel1 = w3(h1)%hf
          if (check_hel_ol(h3)) then
            if (ProjHind(subset,hel3,n_expart)==hel1) then
              if(check_hel_ol(h3)) check_hel_ol(h3)=.false.
            end if
          end if
        end do
      end do

      h3=1
      do while (h3 <= nhel_in)
        if(check_hel_ol(h3)) then
          if (h3 > 1) G0_new_hf(1:h3-1)=ol_coeff%hf(1:h3-1)
          G0_new_hf(h3:nhel_in-1)=ol_coeff%hf(h3+1:nhel_in)
          G0_new_hf(nhel_in)=-1_intkind2
          ol_coeff%hf(1:nhel_in)=G0_new_hf(1:nhel_in)
          h0t(h3:nhel_in-1)=h0t(h3+1:nhel_in)
          check_hel_ol(1:nhel_in-1)=check_hel_ol(2:nhel_in)
          nhel_in=nhel_in-1
        else
          h3=h3+1
        end if
      end do
    end if
  !!!============================= 4 or more external subtrees
    if (n_wf >= 4) then
      sw=size(w4)
      ! Check number of non-vanishing helicity configurations of the external wavefunction
      nhel_wf = 0
      do h1 = 1, sw
        if (w4(h1)%hf /= -1_intkind2) nhel_wf = nhel_wf + 1
      end do

      subset = w4(1)%t
      n_expart = w4(1)%n_part
      check_hel_ol = .true.

      do h3=1, nhel_in
      hel3=ol_coeff%hf(h3)
        do h1=1, nhel_wf
          hel1 = w4(h1)%hf
          if (check_hel_ol(h3)) then
            if (ProjHind(subset,hel3,n_expart)==hel1) then
              if(check_hel_ol(h3)) check_hel_ol(h3)=.false.
            end if
          end if
        end do
      end do

      h3=1
      do while (h3 <= nhel_in)
        if(check_hel_ol(h3)) then
          if (h3 > 1) G0_new_hf(1:h3-1)=ol_coeff%hf(1:h3-1)
          G0_new_hf(h3:nhel_in-1)=ol_coeff%hf(h3+1:nhel_in)
          G0_new_hf(nhel_in)=-1_intkind2
          ol_coeff%hf(1:nhel_in)=G0_new_hf(1:nhel_in)
          h0t(h3:nhel_in-1)=h0t(h3+1:nhel_in)
          check_hel_ol(1:nhel_in-1)=check_hel_ol(2:nhel_in)
          nhel_in=nhel_in-1
        else
          h3=h3+1
        end if
      end do
    end if
  !!!============================= 5 or more external subtrees
    if (n_wf >= 5) then
      sw=size(w5)
      ! Check number of non-vanishing helicity configurations of the external wavefunction
      nhel_wf = 0
      do h1 = 1, sw
        if (w5(h1)%hf /= -1_intkind2) nhel_wf = nhel_wf + 1
      end do

      subset = w5(1)%t
      n_expart = w5(1)%n_part
      check_hel_ol = .true.

      do h3=1, nhel_in
        hel3=ol_coeff%hf(h3)
        do h1=1, nhel_wf
          hel1 = w5(h1)%hf
          if (check_hel_ol(h3)) then
            if (ProjHind(subset,hel3,n_expart)==hel1) then
              if(check_hel_ol(h3)) check_hel_ol(h3)=.false.
            end if
          end if
        end do
      end do

      h3=1
      do while (h3 <= nhel_in)
        if(check_hel_ol(h3)) then
          if (h3 > 1) G0_new_hf(1:h3-1)=ol_coeff%hf(1:h3-1)
          G0_new_hf(h3:nhel_in-1)=ol_coeff%hf(h3+1:nhel_in)
          G0_new_hf(nhel_in)=-1_intkind2
          ol_coeff%hf(1:nhel_in)=G0_new_hf(1:nhel_in)
          h0t(h3:nhel_in-1)=h0t(h3+1:nhel_in)
          check_hel_ol(1:nhel_in-1)=check_hel_ol(2:nhel_in)
          nhel_in=nhel_in-1
        else
          h3=h3+1
        end if
      end do
    end if
!!!============================= 6 or more external subtrees
    if (n_wf >= 6) then
      call ol_msg("More than 5 external subtrees consisting of >=2 particles")
      call ol_fatal()
    end if
!!!=============================
  end if
!***********************************************************************************

  ol_coeff%j(:,:,:,nhel_in+1:size(ol_coeff%hf)) = 0._/**/REALKIND

  do l = 1, nhel_in
    call G0initialisationOLR(G0coeff(h0t(l))%j,ol_coeff%j(:,:,:,l))
  end do

end subroutine G0_hol_initialisation


end module hol_initialisation_/**/REALKIND





module ol_h_vert_interface_/**/REALKIND

  implicit none
  contains

!***********************************************************************
! OpenLoops dressing steps with summation over the helicities of the
! external wave functions.
! ----------------------------------------------------------------------
! ntry          = 1 (2) for 1st (subsequent) PS points
! G_X(1:n(3))   = input open-loop with n(3) helicity states
!                 corresponding to "unattached" external legs
! J_Y(1:n(1))   = external wave function with n(1) helicity states
! Gout_Z(1:n(2))= output open loop with n(2) helicity states
!                 corresponding to "unattached" external legs
! n(1:3) = n(1) is the number of helicity states of the external subtree
!          n(2) is the number of helicity states of the output open-loop
!          n(3) is number of non-vanishing helicity states of the input
! t(1:2,n(3)) = helicity table. It links the helicity states of G_X(h)
! with J_Y(t(1,h)) and Gout_Z(t(2,h))
! **********************************************************************

!***********************************************************************
subroutine Hloop_AZ_Q(ntry, G_A, J_Z, Gout_A, g_RL, n, t)
!-----------------------------------------------------------------------
! bare AZ -> A Z-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_AZ_Q
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:), n(3)
  type(wfun),  intent(in)    :: J_Z(:)
  type(hol),   intent(in)    :: G_A
  type(hol),   intent(out)   :: Gout_A
  complex(REALKIND), intent(in)  :: g_RL(2)
  complex(REALKIND)  :: G_add(size(Gout_A%j,1),size(Gout_A%j,2),size(Gout_A%j,3))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_Z, G_A, Gout_A, n, t)

  Gout_A%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_AZ_Q(G_A%j(:,:,:,h), J_Z(t(1,h))%j, G_add, g_RL)
    Gout_A%j(:,:,:,t(2,h)) = Gout_A%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_AZ_Q


!***********************************************************************
subroutine Hloop_AQ_Z(ntry, G_A, J_Q, Gout_Z, g_RL, n, t)
!-----------------------------------------------------------------------
! bare AQ -> Z Z-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_AQ_Z
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:), n(3)
  type(wfun),  intent(in)    :: J_Q(:)
  type(hol),   intent(in)    :: G_A
  type(hol),   intent(out)   :: Gout_Z
  complex(REALKIND), intent(in)  :: g_RL(2)
  complex(REALKIND)  :: G_add(size(Gout_Z%j,1),size(Gout_Z%j,2),size(Gout_Z%j,3))
  integer(intkind2)  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_Q, G_A, Gout_Z, n, t)

  Gout_Z%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_AQ_Z(G_A%j(:,:,:,h), J_Q(t(1,h))%j, G_add, g_RL)
    Gout_Z%j(:,:,:,t(2,h)) = Gout_Z%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_AQ_Z


!***********************************************************************
subroutine Hloop_ZA_Q(ntry, G_Z, J_A, Gout_A, g_RL, n, t)
!-----------------------------------------------------------------------
! bare ZA -> Q Z-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_ZA_Q
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:), n(3)
  type(wfun),  intent(in)    :: J_A(:)
  type(hol),   intent(in)    :: G_Z
  type(hol),   intent(out)   :: Gout_A
  complex(REALKIND), intent(in)  :: g_RL(2)
  complex(REALKIND)  :: G_add(size(Gout_A%j,1),size(Gout_A%j,2),size(Gout_A%j,3))
  integer  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_A, G_Z, Gout_A, n, t)

  Gout_A%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_ZA_Q(G_Z%j(:,:,:,h), J_A(t(1,h))%j, G_add, g_RL)
    Gout_A%j(:,:,:,t(2,h)) = Gout_A%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_ZA_Q


!***********************************************************************
subroutine Hloop_QZ_A(ntry, G_Q, J_Z, Gout_Q, g_RL, n, t)
!-----------------------------------------------------------------------
! bare QZ -> A Z-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_QZ_A
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:), n(3)
  type(wfun),  intent(in)    :: J_Z(:)
  type(hol),   intent(in)    :: G_Q
  type(hol),   intent(out)   :: Gout_Q
  complex(REALKIND), intent(in)  :: g_RL(2)
  complex(REALKIND)  :: G_add(size(Gout_Q%j,1),size(Gout_Q%j,2),size(Gout_Q%j,3))
  integer  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_Z, G_Q, Gout_Q, n, t)

  Gout_Q%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_QZ_A(G_Q%j(:,:,:,h), J_Z(t(1,h))%j, G_add, g_RL)
    Gout_Q%j(:,:,:,t(2,h)) = Gout_Q%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_QZ_A


!***********************************************************************
subroutine Hloop_QA_Z(ntry, G_Q, J_A, Gout_Z, g_RL, n, t)
!-----------------------------------------------------------------------
! bare QA -> Z Z-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_QA_Z
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:), n(3)
  type(wfun),  intent(in)    :: J_A(:)
  type(hol),   intent(in)    :: G_Q
  type(hol),   intent(out)   :: Gout_Z
  complex(REALKIND), intent(in)  :: g_RL(2)
  complex(REALKIND)  :: G_add(size(Gout_Z%j,1),size(Gout_Z%j,2),size(Gout_Z%j,3))
  integer(intkind2)  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_A, G_Q, Gout_Z, n, t)

  Gout_Z%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
      call loop_QA_Z(G_Q%j(:,:,:,h), J_A(t(1,h))%j, G_add, g_RL)
      Gout_Z%j(:,:,:,t(2,h)) = Gout_Z%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_QA_Z


!***********************************************************************
subroutine Hloop_ZQ_A(ntry, G_Z, J_Q, Gout_Q, g_RL, n, t)
!-----------------------------------------------------------------------
! bare ZQ -> A Z-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_ZQ_A
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:), n(3)
  type(wfun),  intent(in)    :: J_Q(:)
  type(hol),   intent(in)    :: G_Z
  type(hol),   intent(out)   :: Gout_Q
  complex(REALKIND), intent(in)  :: g_RL(2)
  complex(REALKIND)  :: G_add(size(Gout_Q%j,1),size(Gout_Q%j,2),size(Gout_Q%j,3))
  integer(intkind2)  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_Q, G_Z, Gout_Q, n, t)

  Gout_Q%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_ZQ_A(G_Z%j(:,:,:,h), J_Q(t(1,h))%j, G_add, g_RL)
    Gout_Q%j(:,:,:,t(2,h)) = Gout_Q%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_ZQ_A


!***********************************************************************
subroutine Hloop_AW_Q(ntry, G_A, J_W, Gout_A, n, t)
!-----------------------------------------------------------------------
! bare AW -> A W-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_AW_Q
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(3)
  type(wfun),  intent(in)    :: J_W(:)
  type(hol),   intent(in)    :: G_A
  type(hol),   intent(out)   :: Gout_A
  complex(REALKIND)  :: G_add(size(Gout_A%j,1),size(Gout_A%j,2),size(Gout_A%j,3))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_W, G_A, Gout_A, n, t)

  Gout_A%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_AW_Q(G_A%j(:,:,:,h), J_W(t(1,h))%j, G_add)
    Gout_A%j(:,:,:,t(2,h)) = Gout_A%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_AW_Q


!***********************************************************************
subroutine Hloop_AQ_W(ntry, G_A, J_Q, Gout_W, n, t)
!-----------------------------------------------------------------------
! bare AQ -> W W-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_AQ_W
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(3)
  type(wfun),  intent(in)    :: J_Q(:)
  type(hol),   intent(in)    :: G_A
  type(hol),   intent(out)   :: Gout_W
  complex(REALKIND)  :: G_add(size(Gout_W%j,1),size(Gout_W%j,2),size(Gout_W%j,3))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_Q, G_A, Gout_W, n, t)

  Gout_W%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_AQ_W(G_A%j(:,:,:,h), J_Q(t(1,h))%j, G_add)
    Gout_W%j(:,:,:,t(2,h)) = Gout_W%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_AQ_W


!***********************************************************************
subroutine Hloop_WA_Q(ntry, G_W, J_A, Gout_A, n, t)
!-----------------------------------------------------------------------
! bare WA -> Q W-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_WA_Q
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(3)
  type(wfun),  intent(in)    :: J_A(:)
  type(hol),   intent(in)    :: G_W
  type(hol),   intent(out)   :: Gout_A
  complex(REALKIND)  :: G_add(size(Gout_A%j,1),size(Gout_A%j,2),size(Gout_A%j,3))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_A, G_W, Gout_A, n, t)

  Gout_A%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_WA_Q(G_W%j(:,:,:,h), J_A(t(1,h))%j, G_add)
    Gout_A%j(:,:,:,t(2,h)) = Gout_A%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_WA_Q


!***********************************************************************
subroutine Hloop_QW_A(ntry, G_Q, J_W, Gout_Q, n, t)
!-----------------------------------------------------------------------
! bare QW -> A W-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_QW_A
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(3)
  type(wfun),  intent(in)    :: J_W(:)
  type(hol),   intent(in)    :: G_Q
  type(hol),   intent(out)   :: Gout_Q
  complex(REALKIND)  :: G_add(size(Gout_Q%j,1),size(Gout_Q%j,2),size(Gout_Q%j,3))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_W, G_Q, Gout_Q, n, t)

  Gout_Q%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_QW_A(G_Q%j(:,:,:,h), J_W(t(1,h))%j, G_add)
    Gout_Q%j(:,:,:,t(2,h)) = Gout_Q%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_QW_A


!***********************************************************************
subroutine Hloop_QA_W(ntry, G_Q, J_A, Gout_W, n, t)
!-----------------------------------------------------------------------
! bare QA -> W W-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_QA_W
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(3)
  type(wfun),  intent(in)    :: J_A(:)
  type(hol),   intent(in)    :: G_Q
  type(hol),   intent(out)   :: Gout_W
  complex(REALKIND)  :: G_add(size(Gout_W%j,1),size(Gout_W%j,2),size(Gout_W%j,3))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_A, G_Q, Gout_W, n, t)

  Gout_W%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_QA_W(G_Q%j(:,:,:,h), J_A(t(1,h))%j, G_add)
    Gout_W%j(:,:,:,t(2,h)) = Gout_W%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_QA_W


!***********************************************************************
subroutine Hloop_WQ_A(ntry, G_W, J_Q, Gout_Q, n, t)
!-----------------------------------------------------------------------
! bare WQ -> A W-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_WQ_A
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(3)
  type(wfun),  intent(in)    :: J_Q(:)
  type(hol),   intent(in)    :: G_W
  type(hol),   intent(out)   :: Gout_Q
  complex(REALKIND)  :: G_add(size(Gout_Q%j,1),size(Gout_Q%j,2),size(Gout_Q%j,3))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_Q, G_W, Gout_Q, n, t)

  Gout_Q%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_WQ_A(G_W%j(:,:,:,h), J_Q(t(1,h))%j, G_add)
    Gout_Q%j(:,:,:,t(2,h)) = Gout_Q%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_WQ_A


!***********************************************************************
subroutine Hloop_AV_Q(ntry, G_A, J_V, Gout_A, n, t)
!-----------------------------------------------------------------------
! bare AV -> Q gluon-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_AV_Q
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(3)
  type(wfun),  intent(in)    :: J_V(:)
  type(hol),   intent(in)    :: G_A
  type(hol),   intent(out)   :: Gout_A
  complex(REALKIND)  :: G_add(size(Gout_A%j,1),size(Gout_A%j,2),size(Gout_A%j,3))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_V, G_A, Gout_A, n, t)  ! STILL TO BE ADAPTED

  Gout_A%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_AV_Q(G_A%j(:,:,:,h), J_V(t(1,h))%j, G_add)
    Gout_A%j(:,:,:,t(2,h)) = Gout_A%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_AV_Q

!***********************************************************************
subroutine Hloop_AQ_V(ntry, G_A, J_Q, Gout_V, n, t)
!-----------------------------------------------------------------------
! bare AQ -> V gluon-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_AQ_V
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(3)
  type(wfun),  intent(in)    :: J_Q(:)
  type(hol),   intent(in)    :: G_A
  type(hol),   intent(out)   :: Gout_V
  complex(REALKIND)  :: G_add(size(Gout_V%j,1),size(Gout_V%j,2),size(Gout_V%j,3))
  integer  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_Q, G_A, Gout_V, n, t)

  Gout_V%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_AQ_V(G_A%j(:,:,:,h), J_Q(t(1,h))%j, G_add)
    Gout_V%j(:,:,:,t(2,h)) = Gout_V%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_AQ_V


!***********************************************************************
subroutine Hloop_VA_Q(ntry, G_V, J_A, Gout_A, n, t)
!-----------------------------------------------------------------------
! bare VA -> Q gluon-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_VA_Q
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(3)
  type(wfun),  intent(in)    :: J_A(:)
  type(hol),   intent(in)    :: G_V
  type(hol),   intent(out)   :: Gout_A

  complex(REALKIND)  :: G_add(size(Gout_A%j,1),size(Gout_A%j,2),size(Gout_A%j,3))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_A, G_V, Gout_A, n, t)

  Gout_A%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step + helicity summation
    call loop_VA_Q(G_V%j(:,:,:,h), J_A(t(1,h))%j, G_add)
    Gout_A%j(:,:,:,t(2,h)) = Gout_A%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_VA_Q


!***********************************************************************
subroutine Hloop_QV_A(ntry, G_Q, J_V, Gout_Q, n, t)
!-----------------------------------------------------------------------
! bare QV -> A gluon-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_QV_A
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(3)
  type(wfun),  intent(in)    :: J_V(:)
  type(hol),   intent(in)    :: G_Q
  type(hol),   intent(out)   :: Gout_Q
  complex(REALKIND)  :: G_add(size(Gout_Q%j,1),size(Gout_Q%j,2),size(Gout_Q%j,3))
  integer  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_V, G_Q, Gout_Q, n, t)

  Gout_Q%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_QV_A(G_Q%j(:,:,:,h), J_V(t(1,h))%j, G_add)
    Gout_Q%j(:,:,:,t(2,h)) = Gout_Q%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_QV_A


!***********************************************************************
subroutine Hloop_QA_V(ntry, G_Q, J_A, Gout_V, n, t)
!-----------------------------------------------------------------------
! bare QA -> V gluon-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_QA_V
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(3)
  type(wfun),  intent(in)    :: J_A(:)
  type(hol),   intent(in)    :: G_Q
  type(hol),   intent(out)   :: Gout_V
  complex(REALKIND)  :: G_add(size(Gout_V%j,1),size(Gout_V%j,2),size(Gout_V%j,3))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_A, G_Q, Gout_V, n, t)  ! STILL TO BE ADAPTED

  Gout_V%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_QA_V(G_Q%j(:,:,:,h), J_A(t(1,h))%j, G_add)
    Gout_V%j(:,:,:,t(2,h)) = Gout_V%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_QA_V


!***********************************************************************
subroutine Hloop_VQ_A(ntry, G_V, J_Q, Gout_A, n, t)
!-----------------------------------------------------------------------
! bare VQ -> A gluon-like interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_VQ_A
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2),     intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(3)
  type(wfun),  intent(in)    :: J_Q(:)
  type(hol),   intent(in)    :: G_V
  type(hol),   intent(out)   :: Gout_A
  complex(REALKIND)  :: G_add(size(Gout_A%j,1),size(Gout_A%j,2),size(Gout_A%j,3))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_Q, G_V, Gout_A, n, t)  ! STILL TO BE ADAPTED

  Gout_A%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_VQ_A(G_V%j(:,:,:,h), J_Q(t(1,h))%j, G_add)
    Gout_A%j(:,:,:,t(2,h)) = Gout_A%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_VQ_A


!***********************************************************************
subroutine Hloop_UV_W(ntry, Gin_V, Ploop, J_V, Ptree, Gout_V, n, t)
!-----------------------------------------------------------------------
! bare VV -> V vertex
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_UV_W
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(3)
  type(wfun),  intent(in)    :: J_V(:)
  type(hol),   intent(in)    :: Gin_V
  type(hol),   intent(out)   :: Gout_V
  complex(REALKIND), intent(in)  :: Ploop(4), Ptree(4)
  complex(REALKIND)  :: G_add(size(Gout_V%j,1),size(Gout_V%j,2),size(Gout_V%j,3))
  integer  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_V, Gin_V, Gout_V, n, t)  ! STILL TO BE ADAPTED

  Gout_V%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_UV_W(Gin_V%j(:,:,:,h), Ploop, J_V(t(1,h))%j, Ptree, G_add)
    Gout_V%j(:,:,:,t(2,h)) = Gout_V%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_UV_W


!***********************************************************************
subroutine Hloop_UW_V(ntry, Gin_V, Ploop, J_V, Ptree, Gout_V, n, t)
!-----------------------------------------------------------------------
! bare VV -> V vertex
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_UW_V
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2),     intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(3)
  type(wfun),  intent(in)    :: J_V(:)
  type(hol),   intent(in)    :: Gin_V
  type(hol),   intent(out)   :: Gout_V
  complex(REALKIND), intent(in)  :: Ploop(4), Ptree(4)
  complex(REALKIND)  :: G_add(size(Gout_V%j,1),size(Gout_V%j,2),size(Gout_V%j,3))
  integer  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_V, Gin_V, Gout_V, n, t)  ! STILL TO BE ADAPTED

  Gout_V%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_UW_V(Gin_V%j(:,:,:,h), Ploop, J_V(t(1,h))%j, Ptree, G_add)
    Gout_V%j(:,:,:,t(2,h)) = Gout_V%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_UW_V


!***********************************************************************
subroutine Hloop_GGG_G_23(ntry, Gin_V, J1, J2, Gout_V, n, t)
!-----------------------------------------------------------------------
! 4-gluon vertex interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert4
  use ol_vert_interface_/**/REALKIND, only: loop_GGG_G_23
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(4)
  type(wfun),  intent(in)    :: J1(:), J2(:)
  type(hol),   intent(in)    :: Gin_V
  type(hol),   intent(out)   :: Gout_V
  complex(REALKIND)  :: G_add(size(Gout_V%j,1),size(Gout_V%j,2),size(Gout_V%j,3))
  integer  :: h

  if (ntry == 1) call helbookkeeping_ol_vert4(ntry, J1, J2, Gin_V, Gout_V, n, t)  ! STILL TO BE ADAPTED

  Gout_V%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(4)  ! recursion step
    call loop_GGG_G_23(Gin_V%j(:,:,:,h), J1(t(1,h))%j, J2(t(2,h))%j, G_add)
    Gout_V%j(:,:,:,t(3,h)) = Gout_V%j(:,:,:,t(3,h)) + G_add
  end do

end subroutine Hloop_GGG_G_23


!***********************************************************************
subroutine Hloop_GGG_G_12(ntry, Gin_V, J1, J2, Gout_V, n, t)
!-----------------------------------------------------------------------
! 4-gluon vertex interaction
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert4
  use ol_vert_interface_/**/REALKIND, only: loop_GGG_G_12
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(4)
  type(wfun),  intent(in)    :: J1(:), J2(:)
  type(hol),   intent(in)    :: Gin_V
  type(hol),   intent(out)   :: Gout_V
  complex(REALKIND)  :: G_add(size(Gout_V%j,1),size(Gout_V%j,2),size(Gout_V%j,3))
  integer  :: h

  if (ntry == 1) call helbookkeeping_ol_vert4(ntry, J1, J2, Gin_V, Gout_V, n, t)  ! STILL TO BE ADAPTED

  Gout_V%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(4)  ! recursion step
    call loop_GGG_G_12(Gin_V%j(:,:,:,h), J1(t(1,h))%j, J2(t(2,h))%j, G_add)
    Gout_V%j(:,:,:,t(3,h)) = Gout_V%j(:,:,:,t(3,h)) + G_add
  end do

end subroutine Hloop_GGG_G_12


!***********************************************************************
subroutine Hloop_CV_D(ntry, Gin_C, Ploop, J_V, Ptree, Gout_C, n, t)
!-----------------------------------------------------------------------
! bare ghost gluon -> ghost interaction
! always comes in a closed ghost loop
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_CV_D
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(3)
  complex(REALKIND), intent(in)  :: Ploop(4), Ptree(4)
  type(wfun),  intent(in)    :: J_V(:)
  type(hol),   intent(in)    :: Gin_C
  type(hol),   intent(out)   :: Gout_C
  complex(REALKIND)  :: G_add(size(Gout_C%j,1),size(Gout_C%j,2),size(Gout_C%j,3))
  integer  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_V, Gin_C, Gout_C, n, t)  ! STILL TO BE ADAPTED

  Gout_C%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_CV_D(Gin_C%j(:,:,:,h), Ploop, J_V(t(1,h))%j, Ptree, G_add)
    Gout_C%j(:,:,:,t(2,h)) = Gout_C%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_CV_D


!***********************************************************************
subroutine Hloop_DV_C(ntry, Gin_D, Ploop, J_V, Gout_D, n, t)
!-----------------------------------------------------------------------
! bare anti-ghost gluon -> anti-ghost interaction
! always comes in a closed ghost loop
! **********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_DV_C
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: t(:,:)
  integer(intkind2), intent(inout) :: n(3)
  complex(REALKIND), intent(in)  :: Ploop(4)
  type(wfun),  intent(in)    :: J_V(:)
  type(hol),   intent(in)    :: Gin_D
  type(hol),   intent(out)   :: Gout_D
  complex(REALKIND)  :: G_add(size(Gout_D%j,1),size(Gout_D%j,2),size(Gout_D%j,3))
  integer :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_V, Gin_D, Gout_D, n, t)

  Gout_D%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_DV_C(Gin_D%j(:,:,:,h), Ploop, J_V(t(1,h))%j, G_add)
    Gout_D%j(:,:,:,t(2,h)) = Gout_D%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_DV_C


!***********************************************************************
subroutine Hloop_AS_Q(ntry, G_A, J_S, Gout_A, g_RL, n, t)
!-----------------------------------------------------------------------
! bare anti-quark scalar -> quark interaction
!***********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_AS_Q
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  complex(REALKIND), intent(in)  :: g_RL(2)
  type(wfun),  intent(in)    :: J_S(:)
  type(hol),   intent(in)    :: G_A
  type(hol),   intent(out)   :: Gout_A
  complex(REALKIND)  :: G_add(size(Gout_A%j,1),size(Gout_A%j,2),size(Gout_A%j,3))
  integer(intkind2)  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_S, G_A, Gout_A, n, t)

  Gout_A%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_AS_Q(G_A%j(:,:,:,h), J_S(t(1,h))%j, G_add, g_RL)
    Gout_A%j(:,:,:,t(2,h)) = Gout_A%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_AS_Q


!***********************************************************************
subroutine Hloop_SA_Q(ntry, G_S, J_A, Gout_A, g_RL, n, t)
!-----------------------------------------------------------------------
! bare scalar anti-quark -> quark interaction
!***********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_SA_Q
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  complex(REALKIND), intent(in)  :: g_RL(2)
  type(wfun),  intent(in)    :: J_A(:)
  type(hol),   intent(in)    :: G_S
  type(hol),   intent(out)   :: Gout_A
  complex(REALKIND)  :: G_add(size(Gout_A%j,1),size(Gout_A%j,2),size(Gout_A%j,3))
  integer(intkind2)  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_A, G_S, Gout_A, n, t)

  Gout_A%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_SA_Q(G_S%j(:,:,:,h), J_A(t(1,h))%j, G_add, g_RL)
    Gout_A%j(:,:,:,t(2,h)) = Gout_A%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_SA_Q


!***********************************************************************
subroutine Hloop_QS_A(ntry, G_Q, J_S, Gout_Q, g_RL, n, t)
!-----------------------------------------------------------------------
! bare quark scalar -> anti-quark interaction
! ----------------------------------------------------------------------
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_QS_A
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  complex(REALKIND), intent(in)  :: g_RL(2)
  type(wfun),  intent(in)    :: J_S(:)
  type(hol),   intent(in)    :: G_Q
  type(hol),   intent(out)   :: Gout_Q
  complex(REALKIND)  :: G_add(size(Gout_Q%j,1),size(Gout_Q%j,2),size(Gout_Q%j,3))
  integer  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_S, G_Q, Gout_Q, n, t)

  Gout_Q%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_QS_A(G_Q%j(:,:,:,h), J_S(t(1,h))%j, G_add, g_RL)
    Gout_Q%j(:,:,:,t(2,h)) = Gout_Q%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_QS_A


!***********************************************************************
subroutine Hloop_SQ_A(ntry, G_S, J_Q, Gout_Q, g_RL, n, t)
!-----------------------------------------------------------------------
! bare scalar quark -> anti-quark interaction
! ----------------------------------------------------------------------
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_SQ_A
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  complex(REALKIND), intent(in)  :: g_RL(2)
  type(wfun),  intent(in)    :: J_Q(:)
  type(hol),   intent(in)    :: G_S
  type(hol),   intent(out)   :: Gout_Q
  complex(REALKIND)  :: G_add(size(Gout_Q%j,1),size(Gout_Q%j,2),size(Gout_Q%j,3))
  integer  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_Q, G_S, Gout_Q, n, t)

  Gout_Q%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_SQ_A(G_S%j(:,:,:,h), J_Q(t(1,h))%j, G_add, g_RL)
    Gout_Q%j(:,:,:,t(2,h)) = Gout_Q%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_SQ_A


!***********************************************************************
subroutine Hloop_QA_S(ntry, G_Q, J_A, Gout_S, g_RL, n, t)
!-----------------------------------------------------------------------
! bare quar anti-quark -> scalar interaction
!***********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_QA_S
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  complex(REALKIND), intent(in)  :: g_RL(2)
  type(wfun),  intent(in)    :: J_A(:)
  type(hol),   intent(in)    :: G_Q
  type(hol),   intent(out)   :: Gout_S
  complex(REALKIND)  :: G_add(size(Gout_S%j,1),size(Gout_S%j,2),size(Gout_S%j,3))
  integer(intkind2)  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_A, G_Q, Gout_S, n, t)

  Gout_S%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_QA_S(G_Q%j(:,:,:,h), J_A(t(1,h))%j, G_add, g_RL)
    Gout_S%j(:,:,:,t(2,h)) = Gout_S%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_QA_S


!***********************************************************************
subroutine Hloop_AQ_S(ntry, G_A, J_Q, Gout_S, g_RL, n, t)
!-----------------------------------------------------------------------
! bare quar anti-quark -> scalar interaction
!***********************************************************************
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_AQ_S
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  complex(REALKIND), intent(in)  :: g_RL(2)
  type(wfun),  intent(in)    :: J_Q(:)
  type(hol),   intent(in)    :: G_A
  type(hol),   intent(out)   :: Gout_S
  complex(REALKIND)  :: G_add(size(Gout_S%j,1),size(Gout_S%j,2),size(Gout_S%j,3))
  integer(intkind2)  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_Q, G_A, Gout_S, n, t)

  Gout_S%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_AQ_S(G_A%j(:,:,:,h), J_Q(t(1,h))%j, G_add, g_RL)
    Gout_S%j(:,:,:,t(2,h)) = Gout_S%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_AQ_S


!***********************************************************************
subroutine Hloop_VV_S(ntry, G_V, J_V, Gout_S, n, t)
!-----------------------------------------------------------------------
! bare vector vector -> scalar interaction
! ----------------------------------------------------------------------
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_VV_S
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  type(wfun),  intent(in)    :: J_V(:)
  type(hol),   intent(in)    :: G_V
  type(hol),   intent(out)   :: Gout_S
  complex(REALKIND)  :: G_add(size(Gout_S%j,1),size(Gout_S%j,2),size(Gout_S%j,3))
  integer(intkind2)  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_V, G_V, Gout_S, n, t)

  Gout_S%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_VV_S(G_V%j(:,:,:,h), J_V(t(1,h))%j, G_add)
    Gout_S%j(:,:,:,t(2,h)) = Gout_S%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_VV_S


!***********************************************************************
subroutine Hloop_VS_V(ntry, G_V, J_S, Gout_V, n, t)
!-----------------------------------------------------------------------
! bare vector scalar -> vector interaction
! ----------------------------------------------------------------------
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_VS_V
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  type(wfun),  intent(in)    :: J_S(:)
  type(hol),   intent(in)    :: G_V
  type(hol),   intent(out)   :: Gout_V
  complex(REALKIND)  :: G_add(size(Gout_V%j,1),size(Gout_V%j,2),size(Gout_V%j,3))
  integer(intkind2)  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_S, G_V, Gout_V, n, t)

  Gout_V%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_VS_V(G_V%j(:,:,:,h), J_S(t(1,h))%j, G_add)
    Gout_V%j(:,:,:,t(2,h)) = Gout_V%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_VS_V


!***********************************************************************
subroutine Hloop_SV_V(ntry, G_S, J_V, Gout_V, n, t)
!-----------------------------------------------------------------------
! bare scalar vector -> vector interaction
! ----------------------------------------------------------------------
  use KIND_TYPES, only: REALKIND, intkind1, intkind2
  use ol_data_types_/**/REALKIND, only: wfun, hol
  use hel_bookkeeping_/**/REALKIND, only: helbookkeeping_ol_vert3
  use ol_vert_interface_/**/REALKIND, only: loop_SV_V
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(:,:)
  type(wfun),  intent(in)    :: J_V(:)
  type(hol),   intent(in)    :: G_S
  type(hol),   intent(out)   :: Gout_V
  complex(REALKIND)  :: G_add(size(Gout_V%j,1),size(Gout_V%j,2),size(Gout_V%j,3))
  integer(intkind2)  :: h

  if (ntry == 1) call helbookkeeping_ol_vert3(ntry, J_V, G_S, Gout_V, n, t)

  Gout_V%j = 0._/**/REALKIND
  G_add    = 0._/**/REALKIND

  do h = 1, n(3)  ! recursion step
    call loop_SV_V(G_S%j(:,:,:,h), J_V(t(1,h))%j, G_add)
    Gout_V%j(:,:,:,t(2,h)) = Gout_V%j(:,:,:,t(2,h)) + G_add
  end do

end subroutine Hloop_SV_V


end module ol_h_vert_interface_/**/REALKIND
