
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


module ol_kinematics_qp
  use kind_types, only: MaxParticles
  implicit none
  integer :: n_
  integer :: binom2(MaxParticles) = [((n_*(n_-1))/2, n_=1, MaxParticles)]

contains

! **********************************************************************
subroutine Std2LC_Rep(P,L)
! **********************************************************************
! Lorentz -> light-cone representation
! P(0:3)   = Lorentz momentum P^mu (contravariant)
! L(1:4)   = light-cone representation L^A (contravariant)
! **********************************************************************
  use kind_types, only: qp
  use ol_parameters_decl_qp, only: CI
  implicit none
  real(qp),    intent(in)  :: P(0:3)
  complex(qp), intent(out) :: L(1:4)
  L(1) =  P(0) - P(3)
  L(2) =  P(0) + P(3)
  L(3) = -P(1) - CI*P(2)
  L(4) = -P(1) + CI*P(2)
end subroutine Std2LC_Rep


! **********************************************************************
subroutine Std2LC_cmplx(P,L)
! **********************************************************************
! complex version needed for OPP Reduction
! Lorentz -> light-cone representation
! P(0:3)   = Lorentz momentum P^mu (ALWAYS CONTRAVARIANT)
! L(1:4)   = light-cone representation L^A (ALWAYS CONTRAVARIANT)
! **********************************************************************
  use kind_types, only: qp
  use ol_parameters_decl_qp, only: CI
  implicit none
  complex(qp), intent(in)  :: P(0:3)
  complex(qp), intent(out) :: L(1:4)
  L(1) =  P(0) -    P(3)
  L(2) =  P(0) +    P(3)
  L(3) = -P(1) - CI*P(2)
  L(4) = -P(1) + CI*P(2)
end subroutine Std2LC_cmplx


! **********************************************************************
subroutine LC2Std_Rep(L,P)
! **********************************************************************
! light-cone -> Lorentz representation
! L(1:4)   = light-cone representation L^A (ALWAYS CONTRAVARIANT)
! P(0:3)   = Lorentz momentum P^mu (ALWAYS CONTRAVARIANT)
! **********************************************************************
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: L(1:4)
  real(qp),    intent(out) :: P(0:3)
  P(0) =  real(L(1)+L(2))*0.5_qp
  P(1) = -real(L(3)+L(4))*0.5_qp
  P(2) = aimag(L(4)-L(3))*0.5_qp
  P(3) =  real(L(2)-L(1))*0.5_qp
end subroutine LC2Std_Rep


! **********************************************************************
subroutine LC2Std_Rep_cmplx(L,P)
! **********************************************************************
! complex version
! light-cone -> Lorentz representation
! L(1:4)   = light-cone representation L^A (ALWAYS CONTRAVARIANT)
! P(0:3)   = Lorentz momentum P^mu (ALWAYS CONTRAVARIANT)
! **********************************************************************
  use kind_types, only: qp
  use ol_parameters_decl_qp, only: CI
  implicit none
  complex(qp), intent(in)  :: L(1:4)
  complex(qp), intent(out) :: P(0:3)
  P(0) =  (L(1)+L(2))*0.5_qp
  P(1) = -(L(3)+L(4))*0.5_qp
  P(2) = -CI*(L(4)-L(3))*0.5_qp
  P(3) =  (L(2)-L(1))*0.5_qp
end subroutine LC2Std_Rep_cmplx


! **********************************************************************
function cont_L_cmplx(A)
! Contraction of a complex Lorentz vector in standard representation with itself
! **********************************************************************
  use kind_types, only: qp
  implicit none
  complex(qp) :: cont_L_cmplx
  complex(qp), intent(in) :: A(0:3)
  cont_L_cmplx = A(0)*A(0) - A(1)*A(1) - A(2)*A(2) - A(3)*A(3)
end function cont_L_cmplx





! **********************************************************************
subroutine clean_mom_in(P_in, m_ext2, P, n)
! remove numerical inaccuracies in n -> 0 point (momenta all incoming)
! P_in(0:3,n) = original PS point in double precision
! m_ext2      = squared external masses
! P(0:3,n)    = improved PS point in working precision
! 3-momentum of n-th particle recomputed with momentum conservation;
! all energies recomputed with on-shell condition;
! 3-momenta (and energies) of incoming particles shifted by +/- eps*p1,
! so that energy conservation is fulfilled up to terms of O(eps^3)
! **********************************************************************
  use kind_types, only: qp, dp
  implicit none
  real(dp), intent(in)  :: P_in(0:3,n)
  integer,         intent(in)  :: n
  real(qp),  intent(in)  :: m_ext2(n)
  real(qp),  intent(out) :: P(0:3,n)
  real(qp)  :: E_ref, P0(n), P2(n)
  real(qp)  :: E0(2), E1(2), E2(2)
  real(qp)  :: E0_tot, E1_tot, E2_tot
  real(qp)  :: eps1, eps
  integer         :: nex, i, pout_max_pos, pout_max_pos_arr(1)
  real            :: prec
  real, parameter :: tolerance = 1.e-9

  P = P_in

!   call dirty_mom(P_in, P, n, 9)

  ! print momena before cleaning
!   do i = 1, n
!     write(*,*) P(:,i), (P(0,i)**2-P(1,i)**2-P(2,i)**2-P(3,i)**2)-m_ext2(i)
!   end do
!   write(*,*) sum(P(0,:)), sum(P(1,:)), sum(P(2,:)), sum(P(3,:))
!   write(*,*)

  E_ref = 0.5_qp * sum(abs(P(0,:)))
  ! check momentum conservation
  do i = 0, 3
    prec = abs(sum(P(i,:)))/E_ref
    if (prec > tolerance) then
      write(*,*) "*** WARNING ***"
      write(*,*) "OpenLoops subroutine clean_mom: inconsistent phase space point."
      write(*,*) "Momentum conservation is only satisfied to", -log10(prec), "digits."
    end if
  end do

  ! check on-shell conditions
  do nex = 1, n
    P2(nex) = P(1,nex)*P(1,nex) + P(2,nex)*P(2,nex) + P(3,nex)*P(3,nex)
    P0(nex) = sign(sqrt(P2(nex) + m_ext2(nex)), P(0,nex))
    prec = abs(P(0,nex)-P0(nex))/E_ref
    if(prec > tolerance) then
      write(*,*) "*** WARNING ***"
      write(*,*) "OpenLoops subroutine clean_mom: inconsistent phase space point."
      write(*,*) "On-shell condition is only satisfied to", -log10(prec), "digits."
    end if
  end do

  ! position of the outgoing momentum with the largest energy
  pout_max_pos_arr = maxloc(abs(P(0,3:)))
  pout_max_pos = 2 + pout_max_pos_arr(1)
  ! fix 3-momentum by momentum conservation
  do i = 1, 3
    P(i,pout_max_pos) = P(i,pout_max_pos) - sum(P(i,:))
  end do

  ! enforce on-shell conditions
  P0(pout_max_pos) = sign(sqrt(P(1,pout_max_pos)**2 + P(2,pout_max_pos)**2 + P(3,pout_max_pos)**2 + m_ext2(pout_max_pos)), &
                        & P(0,pout_max_pos))
  P(0,:) = P0

  E0_tot = sum(P(0,:))

  ! lowest-order energy terms
  E0(1)  = P(0,1)
  E0(2)  = P(0,2)

  ! 1st order energy coefficients
  E1(1)  = P2(1)/E0(1)
  E1(2)  = -(P(1,1)*P(1,2)+P(2,1)*P(2,2)+P(3,1)*P(3,2))/E0(2)
  E1_tot = E1(1)+E1(2)

  ! 2nd order energy coefficients
  ! for quad-precision applications it is recommended to use the formulas w.o.
  ! beam-alignement instabilities both for E2(1) and E2(2)
!   E2(1)  = (P2(1)-E1(1)**2)/(2*E0(1))   ! direct formula
  E2(1)  = P2(1)*m_ext2(1)/(2*E0(1)**3) ! default = equivalent fomula w.o. beam-alignment instabilities
  E2(2)  = (P2(1)-E1(2)**2)/(2*E0(2))   ! default = direct formula
!   E2(2)  = 0.5_qp/E0(2)**3*(P2(1)*(m_ext2(2) + P(1,2)**2 + P(2,2)**2) & ! equivalent formula w.o. beam-alignment instabilities
!          + (P(1,1)**2+P(2,1)**2)*P(3,2)**2 &
!          - (P(1,1)*P(1,2)+P(2,1)*P(2,2))*(P(1,1)*P(1,2)+P(2,1)*P(2,2)+2*P(3,1)*P(3,2)))
  E2_tot = E2(1) + E2(2)

  ! 1st order shift
  eps1 = -E0_tot/E1_tot
  ! 2nd order shift
  eps  = eps1*(1-eps1*E2_tot/E1_tot)

  ! shifted IS momenta
  do i = 1, 3
    P(i,2) = P(i,2) - P(i,1)*eps
    P(i,1) = P(i,1) + P(i,1)*eps
  end do

  ! shifted IS energies
  do nex = 1, 2
    if(m_ext2(nex) == 0 .and. P(1,nex) == 0 .and. P(2,nex) == 0) then
      ! exact formula for m = 0 along beam-axis
      P(0,nex) = sign(abs(P(3,nex)), real(P_in(0,nex), qp))
    else
      P(0,nex) = E0(nex) + E1(nex)*eps + E2(nex)*eps**2
    end if
  end do

  ! print momena after cleaning
!   do i = 1, n
!     write(*,*) P(:,i), (P(0,i)**2-P(1,i)**2-P(2,i)**2-P(3,i)**2)-m_ext2(i)
!   end do
!   write(*,*) sum(P(0,:)), sum(P(1,:)), sum(P(2,:)), sum(P(3,:))
!   write(*,*)

end subroutine clean_mom_in


! **********************************************************************
subroutine clean_mom_scatt(P_in, m_ext2, P, n)
! same as clean_mom_in but for 2-> n-2 PS point
! This routine is not used internally.
! **********************************************************************
  use kind_types, only: qp, dp
  implicit none
  real(dp), intent(in)  :: P_in(0:3,n)
  integer,         intent(in)  :: n
  real(qp),  intent(in)  :: m_ext2(n)
  real(qp),  intent(out) :: P(0:3,n)
  real(dp) :: Q_in(0:3,n)
  real(qp)  :: Q(0:3,n)

  Q_in(:,1:2) = P_in(:,1:2)
  Q_in(:,3:) = -P_in(:,3:)

  call clean_mom_in(Q_in, m_ext2, Q, n)

  P(:,1:2) = Q(:,1:2)
  P(:,3:) = -Q(:,3:)

end subroutine clean_mom_scatt



! **********************************************************************
subroutine dirty_mom(P_in, P, n, DIG)
! introduces random noise in every PS component after DIG digits
! (to test PS-point cleaning).
! Needs RAMBO subroutine rans().
! **********************************************************************
  use kind_types, only: qp, dp
  use ol_rambo, only: rans
  implicit none
  integer,        intent(in)  :: n, DIG
  real(qp), intent(in)  :: P_in(0:3,n)
  real(qp), intent(out) :: P(0:3,n)
  real(dp) :: x, shift
  integer        :: nex, i

  shift = 10._dp**(-DIG)

  do nex = 1, n
    do i = 0, 3
      call rans(x)
      P(i,nex) = P_in(i,nex)*(1+(x-0.5_qp)*shift)
    end do
  end do

end subroutine dirty_mom
! #ifdef 1



! **********************************************************************
subroutine conv_mom_scatt2in(P_scatt, m_ext2, P_in_clean, perm_inv, n)
! Keep two incoming momenta and reverse outgoing momenta.
! Apply phase space cleaning and crossing.
! **********************************************************************
  use kind_types, only: qp, dp
  use ol_external_decl_qp, only: nParticles, P_ex, inverse_crossing
  use ol_parameters_decl_dp, only: scalefactor
  implicit none
  integer,         intent(in)  :: n
  real(dp), intent(in)  :: P_scatt(0:3,n)
  real(qp),  intent(in)  :: m_ext2(n)
  real(qp),  intent(out) :: P_in_clean(0:3,n)
  integer,         intent(in)  :: perm_inv(n)
  real(dp) :: P_in(0:3,n)
  real(qp)  :: P_clean(0:3,n), m_ext2_perm(n)
  integer         :: k
  nParticles = n
  P_ex(:,1:n) = P_scatt
  inverse_crossing(1:n) = perm_inv
  do k = 1, n
    m_ext2_perm(perm_inv(k)) = m_ext2(k)
  end do
  P_in(:,1:2) =   scalefactor * P_scatt(:,1:2)
  P_in(:,3:)  = - scalefactor * P_scatt(:,3:)
  ! Clean momenta to get full numerical precision.
  ! Do the cleaning in the original permutation where the first two momenta are incoming.
  ! Otherwise the beam alignment (zero components) might be spoiled by the cleaning.
  call clean_mom_in(P_in, m_ext2_perm, P_clean, n)
  do k = 1, n
    P_in_clean(:,k) = P_clean(:,perm_inv(k))
  end do
end subroutine conv_mom_scatt2in


! **********************************************************************
subroutine conv_mom_os(P_decay, P_in, n)
! reverse decay products
! ToDo: Apply phase space cleaning
! **********************************************************************
  use kind_types, only: qp, dp
  use ol_parameters_decl_dp, only: scalefactor
  implicit none
  integer,         intent(in)  :: n
  real(dp), intent(in)  :: P_decay(0:3,n)
  real(qp),  intent(out) :: P_in(0:3,n)
  integer         :: k

  P_in  = - scalefactor * P_decay

end subroutine conv_mom_os







! **********************************************************************
subroutine internal_momenta(P, Npart)
! **********************************************************************
! P(0:3,Npart) = external real-valued four-momenta (stantdard representation)
! Npart        = total (in & out) external particle number
! Q(1:5,1:Npart^2-2) = internal four-momenta in light-cone representation;
!                      the fifth component is the C-valued squared momentum.
! Numbering of internal momenta:
!   Sum_i s(i)*P(i) => Q(Sum_i s(i)*2^(i-1)), s(i) = 0, 1
!   so that Q(J1) + Q(J2) = Q(J1+J2)
! QInvariantsMatrix(i,j) = (p_i+p_j)^2 for i /= j, otherwise undefined.
! **********************************************************************
  use kind_types, only: qp
  use ol_momenta_decl_qp, only: Q, QInvariantsMatrix
  implicit none

  real(qp), intent(in) :: P(0:3,Npart)
  integer,        intent(in)  :: Npart
  integer :: i, j
  integer, save :: allocatedNpart = 0
  integer :: Jmax
  integer :: i1, i2 ! conventional particle numbers
  integer :: l1, l2 ! individual 2^(i-1) particles numbers
  integer :: s1, s2 ! sums of 2^(i-1) particle numbers
  integer :: r1, r2 ! inverse of s1, s2, ...


  i = 2**Npart - 2

  Q(:,i+1) = 0

  if (Npart == 2) then
    call Std2LC_Rep(P(:,1),Q(1:4,1))
    Q(1:4,2) = - Q(1:4,1)
    Q(5,1) = Q(1,1)*Q(2,1) - Q(3,1)*Q(4,1)
    Q(5,2) = Q(5,1)
  else if (Npart == 3) then
    call Std2LC_Rep(P(:,1),Q(1:4,1))
    call Std2LC_Rep(P(:,2),Q(1:4,2))
    Q(1:4,3) = Q(1:4,1) + Q(1:4,2)
    Q(1:4,4) = - Q(1:4,3)
    Q(1:4,5) = - Q(1:4,2)
    Q(1:4,6) = - Q(1:4,1)
    Q(5,1) = Q(1,1)*Q(2,1) - Q(3,1)*Q(4,1)
    Q(5,2) = Q(1,2)*Q(2,2) - Q(3,2)*Q(4,2)
    Q(5,3) = Q(1,3)*Q(2,3) - Q(3,3)*Q(4,3)
    Q(5,4) = Q(5,3)
    Q(5,5) = Q(5,2)
    Q(5,6) = Q(5,1)
  else
    call intmom(P, Npart, i)
  end if

  if (allocatedNpart /= Npart) then
    if (allocatedNpart /= 0) deallocate(QInvariantsMatrix)
    allocate(QInvariantsMatrix(Npart,Npart))
    allocatedNpart = Npart
  end if

  do i = 1, Npart
    ! QInvariantsMatrix(i,i) = m_ex2(i)
    do j = i + 1, Npart
      QInvariantsMatrix(i,j) = Q(5,2**(i-1)+2**(j-1))
      QInvariantsMatrix(j,i) = QInvariantsMatrix(i,j)
    end do
  end do


end subroutine internal_momenta


! **********************************************************************
subroutine intmom(P_ex,Npart,Jmax)
! **********************************************************************
  use kind_types, only: qp
  use ol_momenta_decl_qp, only: Q
  implicit none

  real(qp), intent(in) :: P_ex(0:3,Npart)
  integer,        intent(in) :: Npart, Jmax
  integer :: A
  integer :: i1 ! conventional particle numbers
  integer :: l1 ! individual 2^(i-1) particles numbers
  integer :: s1 ! sums of 2^(i-1) particle numbers
  integer :: r1 ! inverse of s1, s2, ...

  l1 = 1 ! ext mom 1 <= i1 <= Npart
  do i1 = 1, Npart
    s1 = l1
    r1 = Jmax + 1 - s1
    call Std2LC_Rep(P_ex(0,i1),Q(1,l1))
    l1 = 2*l1
    do A = 1, 4
      Q(A,r1) = -Q(A,s1)
    end do
    call intmom_rec(Npart, Jmax, i1, s1, 1)
  end do !i1

  do s1 = 1, Jmax/2 ! squared momenta
    Q(5,s1) = Q(1,s1)*Q(2,s1) - Q(3,s1)*Q(4,s1)
    Q(5,Jmax+1-s1) = Q(5,s1)
  end do

end subroutine intmom


! **********************************************************************
recursive subroutine intmom_rec(Npart, Jmax, i1, s1, x)
! **********************************************************************
  use ol_momenta_decl_qp, only: Q
  implicit none
  integer,        intent(in) :: Npart, Jmax, i1, s1, x
  integer :: A
  integer :: ix ! conventional particle numbers
  integer :: lx ! individual 2^(i-1) particles numbers
  integer :: sx ! sums of 2^(i-1) particle numbers
  integer :: rx ! inverse of sx
  logical :: last

  last = .false.
  if (2*x+2 == Npart .or. 2*x+3 == Npart) then
    last = .true.
  end if

  lx = 1 ! adding ext mom 1 <= ix < i1
  do ix = 1, i1 - 1
    sx = s1 + lx
    rx = Jmax + 1 - sx
    if ( (last .eqv. .false.) .or. (mod(Npart,2) == 1 .or. (sx < rx)) ) then  ! avoid double determination for even Npart
      do A = 1, 4
        Q(A,sx) = Q(A,s1) + Q(A,lx)
        Q(A,rx) = -Q(A,sx)
      end do
    end if
    lx = 2*lx
    if ( last .eqv. .false. ) then ! recursion
      call intmom_rec(Npart, Jmax, ix, sx, x+1)
    end if
  end do  !ix

end subroutine intmom_rec


! **********************************************************************
function squeeze_onshell(pinv, masses)
! **********************************************************************
! If 'pinv' is "close" to an element of 'masses', return the mass (positive or negative).
! Otherwise return pinv.
! **********************************************************************
  use kind_types, only: qp
  use ol_loop_parameters_decl_dp, only: ti_os_thresh, mureg
  implicit none
  complex(qp) :: squeeze_onshell
  complex(qp), intent(inout) :: pinv
  real(qp) :: masses(:), mass
  integer :: k
  squeeze_onshell = pinv
  do k = 1, size(masses)
    mass = masses(k)
    if (k /= 1 .and. mass == 0) cycle
    if (abs(abs(pinv)-mass**2)/mureg**2 < ti_os_thresh) then
      squeeze_onshell = sign(mass*mass, real(pinv))
    end if
  end do
end function squeeze_onshell


! **********************************************************************
function momenta_invariants(moms) result(invs)
! **********************************************************************
! Calculate the list of invariants from the momenta 'moms' (complex standard rep.)
! as used by Collier. Apply 'squeeze_onshell' to each invariant with the masses in the theory.
! **********************************************************************
  use kind_types, only: qp
  use ol_parameters_decl_qp, only: &
    & wMW, rMW, wMZ, rMZ, wMH, rMH, wMC, rMC, wMB, rMB, wMT, rMT
  implicit none
  complex(qp), intent(in) :: moms(:,:)
  complex(qp) :: invs(binom2(size(moms,2)+1))
  complex(qp) :: moms0(0:3,0:size(moms,2))
  real(qp) :: masses(0:6)
  integer :: n, k, a, b
  n = size(moms,2) + 1
  moms0(:,0) = 0
  moms0(:,1:n-1) = moms

  k = 0
  do a = 1, n-1
    do b = a, n-1
      k = k + 1
      if (b == a) then
        invs(k) = cont_L_cmplx(moms(:,b))
      else
        invs(k) = cont_L_cmplx(moms(:,b) - moms(:,a))
      end if
    end do
  end do





  masses = 0
  if (wMW == 0) masses(1) = rMW
  if (wMZ == 0) masses(2) = rMZ
  if (wMH == 0) masses(3) = rMH
  if (wMC == 0 .and. rMC /= 0) masses(4) = rMC
  if (wMB == 0 .and. rMB /= 0) masses(5) = rMB
  if (wMT == 0) masses(6) = rMT
  do k = 1, size(invs)
    invs(k) = squeeze_onshell(invs(k), masses)
  end do
end function momenta_invariants

end module ol_kinematics_qp

