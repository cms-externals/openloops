
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


module ol_helicity_bookkeeping_dp
  implicit none
  contains

! **********************************************************************
subroutine checkzero_scalar(S)
! ----------------------------------------------------------------------
! sets to zero irrelevant components of scalar wfun in order to
! find vanishing scalar subtrees via generic S(h)%j == 0 test
! **********************************************************************
  use ol_data_types_dp, only: wfun
  implicit none
  type(wfun), intent(inout) :: S(:)
  integer :: h
  do h = 1, size(S)
    S(h)%j(2:4) = 0
  end do
end subroutine checkzero_scalar



! **********************************************************************
subroutine helbookkeeping_wf(hel, ex, shift)
! ----------------------------------------------------------------------
! attributes additive global labels (0, 1,..., n_k-1)*shift_k
! to helicity states (1,2,...,n_k) of kth external particle, where
! shift_1 = 1 and shift_k+1 = n_k*shift_k
! shift = shift_k + 1 is returned as output to fix next particle's labels
! **********************************************************************
  use ol_data_types_dp, only: wfun
  implicit none
  integer,           intent(in)    :: hel(:)
  type(wfun),        intent(inout) :: ex(size(hel))
  integer,           intent(inout) :: shift
  integer                          :: h

  do h = 1, size(hel)
    if (all(ex(h)%j == 0)) then
      ex(h)%e = -1 ! marks vanishing helicity configurations
    else
      ex(h)%e = (h-1) * shift
    end if
  end do
  shift = shift * size(Hel)

end subroutine helbookkeeping_wf



! **********************************************************************
subroutine helbookkeeping_prop(ntry, WF1, WF2, n)
! ----------------------------------------------------------------------
! WF1(1:n) = input  wfun array
! WF2(1:n) = output wfun array
! **********************************************************************
  use kind_types, only: intkind1, intkind2
  use ol_data_types_dp, only: wfun
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n
  type(wfun),        intent(in)    :: WF1(n)
  type(wfun),        intent(out)   :: WF2(n)
  integer(intkind2) :: h1, i

  if (ntry > 1) then ! the following operations input table t in initialisation form
    write(*,*) 'subroutine helbookkeeping_prop: stop'
    write(*,*) 'ntry =', ntry,'not allowed'
    stop
  end if

  ! sets n = # of non-zero WF1 components and check that all zeros are at the end
  h1 = n
  do i = 1, n
    if (WF1(i)%e == -1_intkind2) then
      h1 = i - 1
      exit
    end if
    WF2(i)%e = WF1(i)%e ! sets WF2 helicity label
  end do

  do i = h1 + 1, n
    if (WF1(i)%e /= -1_intkind2) then
      write(*,*) 'subroutine helbookkeeping_prop: stop'
      write(*,*) 'i, h1, n, WF1(i)%e =', i, h1, n, WF1(i)%e
      stop
    end if
    WF2(i)%e = -1_intkind2  ! mark vanishing helicity configurations
  end do
  n = h1

end subroutine helbookkeeping_prop



! **********************************************************************
subroutine helbookkeeping_vert3(ntry, WF1, WF2, WF3, n, t)
! ----------------------------------------------------------------------
! WF1(1:n(1)), WF(1:n(2)) = input wfun arrays
! WF3(1:n(3))             = output wfun array
! vanishing components moved at the end of WF3 array and marked via %e=-1
! non-zero WF3 components ordered according to global helicity label
! WF1(h1), WF2(h2) <-> WF3(h3) connection stored in table hi= t(i,h3) with i=1,2
! array sizes n(1),n(2),n(3) restricted to non-vanishing components
! **********************************************************************
  use kind_types, only: intkind1, intkind2
  use ol_data_types_dp, only: wfun
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(3), t(2,n(3))
  type(wfun),        intent(in)    :: WF1(n(1)), WF2(n(2))
  type(wfun),        intent(out)   :: WF3(n(3))
  integer(intkind2) :: h1, h2, h3, i, n2_in
  integer(intkind2) :: iq, imin, emin
  integer(intkind2) :: taux(2)
  type(wfun)        :: WFaux

  if (ntry /= 1) then ! the following operations input table t in initialisation form
    write(*,*) 'subroutine helbookkeeping_vert3: stop'
    write(*,*) 'ntry =', ntry, 'not allowed'
    stop
  end if

  ! sets n(1) = # of non-zero WF1 components and check that all zeros are at the end
  h1 = n(1)
  do i = 1, n(1)
    if (WF1(i)%e == -1_intkind2) then
      h1 = i-1
      exit
    end if
  end do
  do i = h1+1, n(1)
    if (WF1(i)%e /= -1_intkind2) then
      write(*,*) 'subroutine helbookkeeping_vert3: stop'
      write(*,*) 'i, h1, n(1), WF1(i)%e =', i, h1, n(1), WF1(i)%e
      stop
    end if
  end do
  n(1) = h1

  ! sets n(2) = # of non-zero WF2 components and check that all zeros are at the end
  h2 = n(2)
  do i = 1, n(2)
    if (WF2(i)%e == -1_intkind2) then
      h2 = i - 1
      exit
    end if
  end do
  do i = h2+1, n(2)
    if (WF2(i)%e /= -1_intkind2) then
      write(*,*) 'subroutine helbookkeeping_vert3: stop'
      write(*,*) 'i, h2, n(2), WF2(i)%e =', i, h2, n(2), WF2(i)%e
      stop
    end if
  end do
  n2_in = n(2)
  n(2) = h2

  ! restrict I/O table to non-vanishing helicity configurations
  i  = 0 ! index of WF3 states
  h3 = 0 ! index of non-zero WF3 states
  do h1 = 1, n(1)
    do h2 = 1, n(2)
      i = (h1-1)*n2_in + h2         ! assumes input table in standard inititalisation form
      if (all(WF3(i)%j == 0)) cycle ! skips vanishing WF3 components
      h3 = h3 + 1
      t(1,h3) = h1
      t(2,h3) = h2
      WF3(h3)%e = WF1(h1)%e + WF2(h2)%e ! determines additive helicity label for outgoing states
      if (h3 == i) cycle
      WF3(h3)%j = WF3(i)%j              ! shifts non-zero components to first part of WF3 array
      WF3(h3)%h = WF3(i)%h              ! shifts non-zero components to first part of WF3 array
    end do
  end do

  ! put vanishing components at the end of WF3 array
  do i = h3 + 1, n(3)
    WF3(i)%j = 0
    WF3(i)%h = B"00"
    WF3(i)%e = -1_intkind2 ! marker for vanising helicity states
  end do

  ! sets n(3) = # of non-vanishing I/O helicity configurations
  n(3) = h3

  ! sort non-vanishing WF3(1:h3) components and adapt table accordingly
  do i = 1, n(3)-1
    emin = WF3(i)%e
    imin = i
    do iq = i+1, n(3)             ! look for component with helicity < emin
      if (WF3(iq)%e >= emin) cycle
      emin = WF3(iq)%e
      imin = iq
    end do
    if (imin > i) then
       WFaux       = WF3(i)          ! flip WF3(i) <-> WF3(imin) components
       WF3(i)      = WF3(imin)
       WF3(imin)   = WFaux
       taux        = t(1:2,i)      ! same for table
       t(1:2,i)    = t(1:2,imin)
       t(1:2,imin) = taux
    end if
  end do

end subroutine helbookkeeping_vert3



! **********************************************************************
subroutine helbookkeeping_vert4(ntry, WF1, WF2, WF3, WF4, n, t)
! ----------------------------------------------------------------------
! WF1(1:n(1)),WF2(1:n(2)),WF3(1:n(3)) = input wfun arrays
! WF4(1:n(4))                         = output wfun array
! vanishing components moved at the end of WF4 array and marked via %e = -1
! non-zero WF4 components ordered according to global helicity label
! WF1(h1), WF2(h2), WF3(h3) <-> WF4(h4) connection stored in table hi = t(i,h3) with i=1,2
! array sizes n(1), n(2), n(3), n(4) restricted to non-vanishing components
! **********************************************************************
  use kind_types, only: intkind1, intkind2
  use ol_data_types_dp, only: wfun
  implicit none
  integer(intkind1), intent(in)    :: ntry
  integer(intkind2), intent(inout) :: n(4), t(3,n(4))
  type(wfun),        intent(in)    :: WF1(n(1)), WF2(n(2)), WF3(n(3))
  type(wfun),        intent(out)   :: WF4(n(4))
  integer(intkind2) :: h1, h2, h3, h4, i, n2_in, n3_in
  integer(intkind2) :: iq, imin, emin
  integer(intkind2) :: taux(3)
  type(wfun)        :: WFaux

  if(ntry /= 1) then ! the following operations input table t in initialisation form
    write(*,*) 'subroutine helbookkeeping_vert4: stop'
    write(*,*) 'ntry =', ntry, 'not allowed'
    stop
  end if

  ! sets n(1) = # of non-zero WF1 components and check that all zeros are at the end
  h1 = n(1)
  do i = 1, n(1)
    if (WF1(i)%e == -1_intkind2) then
      h1 = i - 1
      exit
    end if
  end do
  do i = h1 + 1, n(1)
    if (WF1(i)%e /= -1_intkind2) then
      write(*,*) 'subroutine helbookkeeping_vert4: stop'
      write(*,*) 'i, h1, n(1), WF1(i)%e =', i, h1, n(1), WF1(i)%e
      stop
    end if
  end do
  n(1) = h1

  ! sets n(2) = # of non-zero WF2 components and check that all zeros are at the end
  h2 = n(2)
  do i = 1, n(2)
    if (WF2(i)%e == -1_intkind2) then
      h2 = i - 1
      exit
    end if
  end do
  do i = h2 + 1, n(2)
    if (WF2(i)%e /= -1_intkind2) then
      write(*,*) 'subroutine helbookkeeping_vert4: stop'
      write(*,*) 'i, h2, n(2), WF2(i)%e =', i, h2, n(2), WF2(i)%e
      stop
    end if
  end do
  n2_in = n(2)
  n(2) = h2

  ! sets n(3) = # of non-zero WF3 components and check that all zeros are at the end
  h3 = n(3)
  do i = 1, n(3)
    if (WF3(i)%e == -1_intkind2) then
      h3 = i - 1
      exit
    end if
  end do
  do i = h3 + 1, n(3)
    if (WF3(i)%e /= -1_intkind2) then
      write(*,*) 'subroutine helbookkeeping_vert4: stop'
      write(*,*) 'i, h3, n(3), WF3(i)%e =', i, h3, n(3), WF3(i)%e
      stop
    end if
  end do
  n3_in = n(3)
  n(3) = h3


  ! restrict I/O table to non-vanishing helicity configurations
  i  = 0 ! index of WF4 states
  h4 = 0 ! index of non-zero WF4 states
  do h1 = 1, n(1)
    do h2 = 1, n(2)
      do h3 = 1, n(3)
        i  = h3 + n3_in * (h2-1 + n2_in * (h1-1)) ! assumes input table in standard inititalisation form
        if (all(WF4(i)%j == 0)) cycle ! skips vanishing WF4 components
        h4 = h4 + 1
        t(1,h4) = h1
        t(2,h4) = h2
        t(3,h4) = h3
        WF4(h4)%e = WF1(h1)%e + WF2(h2)%e + WF3(h3)%e ! additive helicity label for outgoing states
        if (h4 == i) cycle
        WF4(h4)%j = WF4(i)%j ! shifts non-zero components to first part of WF4 array
        WF4(h4)%h = WF4(i)%h ! shifts non-zero components to first part of WF4 array
      end do
    end do
  end do

  ! put vanishing components at the end of WF4 array
  do i = h4 + 1, n(4)
    WF4(i)%j = 0
    WF4(i)%h = B"00"
    WF4(i)%e = -1_intkind2 ! marker for vanising helicity states
  end do

  ! sets n(4) = # of non-vanishing I/O helicity configurations
  n(4) = h4

  ! sort non-vanishing WF4(1:h4) components and adapt table accordingly
  do i = 1, n(4) - 1
    emin = WF4(i)%e
    imin = i
    do iq = i + 1, n(4)              ! look for component with helicity < emin
      if (WF4(iq)%e >= emin) cycle
      emin = WF4(iq)%e
      imin = iq
    end do
    if (imin > i) then
       WFaux     = WF4(i) ! flip WF4(i) <-> WF4(imin) components
       WF4(i)    = WF4(imin)
       WF4(imin) = WFaux
       taux        = t(1:3,i) ! same for table
       t(1:3,i)    = t(1:3,imin)
       t(1:3,imin) = taux
    end if
  end do

end subroutine helbookkeeping_vert4



! **********************************************************************
subroutine helbookkeeping_cont(nsync, WF1, WF2, cont, n, t, nhel)
! ----------------------------------------------------------------------
! WF1(1:n(1)), WF(1:n(2))  = input wfun arrays
! cont(1:nhel)             = contraction array
! i3(i1,i2) helicity table stored in  ik= t(k,i3) with k=1,2
! nsync = 1 :  filter and order n ,t, and cont according to WF3 entries
!     (n3 = max # hels  -> n3 = # nonvanishing hels for actual diag)
! nsync = 2 :  syncronise n, t, and cont using global helsync table cont(i)%s
!     (n3 = # nonvanishing hels for actual diag => n3 = global # nonvanishing hels)
! **********************************************************************
  use kind_types, only: intkind1, intkind2
  use ol_data_types_dp, only: wfun, polcont
  implicit none
  integer(intkind1), intent(in)    :: nsync
  integer(intkind2), intent(inout) :: n(3), nhel, t(2,nhel)
  type(wfun),        intent(in)    :: WF1(n(1)), WF2(n(2))
  type(polcont),     intent(inout) :: cont(nhel)
  integer(intkind2) :: h1, h2, h3, i, n2_in
  integer(intkind2) :: iq, imin, emin
  integer(intkind2) :: taux(2)
  type(polcont)     :: contaux

  if (n(3) > nhel) then
    write(*,*) 'subroutine helbookkeeping_cont: stop'
    write(*,*) 'n(3) =', n(3), '> nhel =', nhel, 'not allowed'
    stop
  end if

  select case (nsync)

  case (1_intkind1) ! filter and order n, t, and cont according to WF3 entries
    ! sets n(1) = # of non-zero WF1 components and check that all zeros are at the end
    h1 = n(1)
    do i = 1, n(1)
      if (WF1(i)%e == -1_intkind2) then
        h1 = i-1
        exit
      end if
    end do
    do i = h1+1, n(1)
      if (WF1(i)%e /= -1_intkind2) then
        write(*,*)'subroutine helbookkeeping_cont: stop'
        write(*,*)'i, h1, n(1), WF1(i)%e =', i, h1, n(1), WF1(i)%e
        stop
      end if
    end do
    n(1) = h1

    ! sets n(2) = # of non-zero WF2 components and check that all zeros are at the end
    h2 = n(2)
    do i = 1, n(2)
      if (WF2(i)%e == -1_intkind2) then
        h2 = i-1
        exit
      end if
    end do
    do i = h2+1, n(2)
      if (WF2(i)%e /= -1_intkind2) then
        write(*,*) 'subroutine helbookkeeping_cont: stop'
        write(*,*) 'i, h2, n(2), WF2(i)%e =', i, h2, n(2), WF2(i)%e
        stop
      end if
    end do
    n2_in = n(2)
    n(2) = h2

    ! restrict I/O table to non-vanishing helicity configurations
    i  = 0 ! index of cont states
    h3 = 0 ! index of non-zero cont states
    do h1 = 1, n(1)
      do h2 = 1, n(2)
        i = (h1-1)*n2_in + h2              ! assumes input table in standard inititalisation form
        if (cont(i)%j == 0) cycle          ! skips vanishing cont components
        h3 = h3 + 1
        t(1,h3) = h1
        t(2,h3) = h2
        cont(h3)%e = WF1(h1)%e + WF2(h2)%e ! determines additive helicity label for outgoing states
        if (h3 == i) cycle
        cont(h3)%j = cont(i)%j             ! shifts non-zero components to first part of cont array
      end do
    end do

    ! put vanishing components at the end of cont array
    do i = h3+1, n(3)
      cont(i)%j = 0
      cont(i)%e = -1_intkind2      ! marker for vanising helicity states
    end do

    ! sets n(3) = # of non-vanishing I/O helicity configurations
    n(3) = h3

    ! sort non-vanishing cont(1:h3) components and adapt table accordingly
    do i = 1, n(3)-1
      emin = cont(i)%e
      imin = i
      do iq = i+1, n(3)            ! look for component with helicity < emin
        if (cont(iq)%e >= emin) cycle
        emin = cont(iq)%e
        imin = iq
      end do
      if (imin > i) then
        contaux     = cont(i)      ! flip cont(i) <-> cont(imin) components
        cont(i)     = cont(imin)
        cont(imin)  = contaux
        taux        = t(1:2,i)     ! same for table
        t(1:2,i)    = t(1:2,imin)
        t(1:2,imin) = taux
      end if
    end do

  case (2_intkind1) ! syncronise n, t, and cont using global helsync table cont(i)%s
    n(3) = nhel
    do i = Nhel, 1 , -1
      if (cont(i)%s == 0) then
        cont(i)%j = 0
        t(1:2,i)  = 0_intkind2
      else
        cont(i)%j = cont(cont(i)%s)%j
        t(1:2,i)  = t(1:2,cont(i)%s)
      end if
    end do

  case default
    write(*,*) 'subroutine helbookkeeping_cont: stop'
    write(*,*) 'nsync =', nsync, 'not allowed'
    stop

  end select

end subroutine helbookkeeping_cont



! **********************************************************************
subroutine helsync(nsync, cont, Nhel, Hel)
! ----------------------------------------------------------------------
! cont(1:Nhelmax,1:Ndiag) = amplitudes of all individual polarised diagrams
! Nhelmax                 = maximal number of helicity configurations
! **********************************************************************
  use kind_types, only: intkind1, intkind2
  use ol_data_types_dp, only: polcont
  implicit none
  integer(intkind1), intent(in)    :: nsync
  type(polcont),     intent(inout) :: cont(:,:)
  integer(intkind2), intent(out)   :: Hel(size(cont,1))
  integer(intkind2), intent(inout) :: NHel
  logical :: nonvanishing(size(cont,1))
  integer :: Nhelmax, h, h0, i, ishift, Ndiag, n

  if (nsync /= 1) then
    write(*,*) 'subroutine helsync: stop'
    write(*,*) 'nsync =', nsync, 'not allowed'
    stop
  end if

  Ndiag = size(cont,2)   ! number of diagrams
  Nhelmax = size(cont,1) ! maximal number of helicity configurations
  nonvanishing(1:Nhelmax) = .false.

  ! sets nonvanishing(h) = .true. if configuration with helicity Hel = h-1 non vanishing
  ! Hel = physical helicty    :   0 <= Hel <= Nhelmax - 1
  ! h = Hel+1 = helicty index :   1 <=  h  <= Nhelmax

  nexthel: do h = 1, Nhelmax
    do h0 = 1, Nhelmax
      do n = 1, Ndiag
        if (cont(h0,n)%e == h-1) then
          nonvanishing(h) = .true.
          cycle nexthel
        end if
      end do
    end do
  end do nexthel

  ! extract subset Hel(1:Nhel) of non-vanishing helicity configurations
  Nhel = 0
  do h = 1, Nhelmax
    if (nonvanishing(h)) then
      NHel      = Nhel + 1
      Hel(NHel) = h - 1
    end if
  end do
  Hel(Nhel+1:Nhelmax) = -1 ! put vanishing helicity configurations at the end

  ! helicity-index table cont(i,n)%s to determine position of helicity Hel(i) in diagram n
  !   cont(cont(i,n)%s,n)%e = Hel(i)   when diagram n non-vanisging for helicity state Hel(i)
  !        cont(i,n)%s      = 0        when diagram n vanishes for helicity state Hel(i)
  ! NB: construction below exploits pre-ordering of cont array, i.e. cont(h2,n)%e > cont(h1,n)%e for h2 > h1

  do n = 1, Ndiag
    ishift = 0        ! incremental counter of absent Hel(:) elements in cont(:,n)%e
    do i = 1, Nhel    ! index to scan cont(1:Nhel,n)%e (sub)row
      if (cont(i-ishift,n)%e == Hel(i)) then
        cont(i,n)%s = i - ishift    ! position of Hel(i) in n-th row
      else
        cont(i,n)%s = 0             ! mark absence of Hel(i) in n-th row
        ishift = ishift + 1         ! avoids incrementing i - ishift
      end if
    end do
    cont(Nhel+1:Nhelmax,n)%s = -1  ! irrelevant positions <-> helicity states that never contribute
  end do

end subroutine helsync



! **********************************************************************
subroutine flip_phase(P, pol, MOM, omega)
! ----------------------------------------------------------------------
! P     = emitter 4-momentum (contravariant standard representation, real)
! pol   = +1/-1 gluon-emitter helicity
! MOM   = auxiliary momentum (contravariant standard representation, real)
! omega = (<epsilon(pol),MOM>/|<epsilon(pol),MOM>|)^2
! **********************************************************************
  use kind_types, only: dp
  use ol_wavefunctions_dp, only: wf_V_Std
  use ol_kinematics_dp, only: Std2LC_Rep
  implicit none
  real(dp),    intent(in)  :: P(0:3)
  integer,           intent(in)  :: pol
  real(dp),    intent(in)  :: MOM(0:3)
  complex(dp), intent(out) :: omega(2)
  complex(dp) :: eps(4), MOM_LC(4)
  call wf_V_Std(P, 0._dp, pol, eps) ! light-cone polarisation vector
  call Std2LC_Rep(MOM, MOM_LC)

  ! Don't use h_contractions::cont_PP(eps,MOM_LC) to avoid cyclic dependencies
  omega(1) = eps(1)*MOM_LC(2) + eps(2)*MOM_LC(1) - eps(3)*MOM_LC(4) - eps(4)*MOM_LC(3)
  omega(1) = omega(1)/abs(omega(1))
  omega(1) = omega(1)*omega(1)
  omega(2) = conjg(omega(1))

end subroutine flip_phase

end module ol_helicity_bookkeeping_dp

