
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


module ol_kinematics_/**/REALKIND
  use KIND_TYPES
  use ol_global_decl, only: MaxParticles
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
  use KIND_TYPES, only: REALKIND
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  real(REALKIND),    intent(in)  :: P(0:3)
  complex(REALKIND), intent(out) :: L(1:4)
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
  use KIND_TYPES, only: REALKIND
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in)  :: P(0:3)
  complex(REALKIND), intent(out) :: L(1:4)
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
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), intent(in)  :: L(1:4)
  real(REALKIND),    intent(out) :: P(0:3)
  P(0) =  real(L(1)+L(2))*0.5_/**/REALKIND
  P(1) = -real(L(3)+L(4))*0.5_/**/REALKIND
  P(2) = aimag(L(4)-L(3))*0.5_/**/REALKIND
  P(3) =  real(L(2)-L(1))*0.5_/**/REALKIND
end subroutine LC2Std_Rep


! **********************************************************************
subroutine LC2Std_Rep_cmplx(L,P)
! **********************************************************************
! complex version
! light-cone -> Lorentz representation
! L(1:4)   = light-cone representation L^A (ALWAYS CONTRAVARIANT)
! P(0:3)   = Lorentz momentum P^mu (ALWAYS CONTRAVARIANT)
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in)  :: L(1:4)
  complex(REALKIND), intent(out) :: P(0:3)
  P(0) =  (L(1)+L(2))*0.5_/**/REALKIND
  P(1) = -(L(3)+L(4))*0.5_/**/REALKIND
  P(2) = -CI*(L(4)-L(3))*0.5_/**/REALKIND
  P(3) =  (L(2)-L(1))*0.5_/**/REALKIND
end subroutine LC2Std_Rep_cmplx


! **********************************************************************
function cont_L_cmplx(A)
! Contraction of a complex Lorentz vector in standard representation with itself
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND) :: cont_L_cmplx
  complex(REALKIND), intent(in) :: A(0:3)
  cont_L_cmplx = A(0)*A(0) - A(1)*A(1) - A(2)*A(2) - A(3)*A(3)
end function cont_L_cmplx


#ifdef PRECISION_dp
#ifdef USE_RAMBO
! *********************************************************************
subroutine rambo(sqrt_s, m_ex, p_rambo)
! *********************************************************************
! Calls Rambo for 2 -> n-2 or 1 -> n-1 PS
! *********************************************************************
! sqrt_s         = total cms energy
! m_ex(n)        = external particle masses
! p_rambo(0:3,n) = momenta, n = 1,2 incoming; n = 3,..,n outgoing
! *********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_external_decl_/**/DREALKIND, only: n_scatt
  implicit none
  real(REALKIND), intent(in)  :: sqrt_s, m_ex(:)
  real(REALKIND), intent(out) :: p_rambo(0:3,size(m_ex))

  if (n_scatt == 2) then
    call rambo_2scatt(sqrt_s, m_ex, p_rambo)
  else if (n_scatt == 1) then
    call rambo_decay(sqrt_s, m_ex, p_rambo)
  else
    print*, "[OpenLoops] ERROR: Phase-space not available for scattering of: ",  n_scatt , " -> ", size(m_ex)-n_scatt
    stop
  end if

end subroutine rambo


! *********************************************************************
subroutine rambo_2scatt(sqrt_s, m_ex, p_rambo)
! *********************************************************************
! Generate 2 -> n-2 PS-point with P(1) + P(2) = P(3) + ... + P(n)
! Apply cleaning procedure to get full numerical precision
! *********************************************************************
! sqrt_s         = total cms energy
! m_ex(n)        = external particle masses
! p_rambo(0:3,n) = momenta, n = 1,2 incoming; n = 3,..,n outgoing
! beam momenta:
!   p_rambo(0:3,1) = (EA,0,0,kA)
!   p_rambo(0:3,2) = (EB,0,0,kB)
!   EA + EB        = sqrt_s
!   kA + kB        = 0
! *********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_ramboX, only: rambo0 => rambo
  implicit none
  real(REALKIND), intent(in)  :: sqrt_s, m_ex(:)
  real(REALKIND), intent(out) :: p_rambo(0:3,size(m_ex))
  real(REALKIND) :: E, MA2, MB2, dEAB
  real(REALKIND) :: p_scatt(4,size(m_ex)-2), wgt
  integer :: n
  n = size(m_ex)
  E = sqrt_s*0.5_/**/REALKIND
  ! beam momenta
  if((m_ex(1) == 0) .and. (m_ex(2) == 0)) then
    p_rambo(0,1) =  E
    p_rambo(1,1) =  0
    p_rambo(2,1) =  0
    p_rambo(3,1) =  E
    p_rambo(0,2) =  E
    p_rambo(1,2) =  0
    p_rambo(2,2) =  0
    p_rambo(3,2) = -E
  else
    MA2  = m_ex(1)*m_ex(1)
    MB2  = m_ex(2)*m_ex(2)
    dEAB = (MA2 - MB2) / (2*sqrt_s)
    p_rambo(0,1) =  E + dEAB
    p_rambo(1,1) =  0
    p_rambo(2,1) =  0
    p_rambo(3,1) =  sqrt(p_rambo(0,1)**2-MA2)
    p_rambo(0,2) =  E - dEAB
    p_rambo(1,2) =  0
    p_rambo(2,2) =  0
    p_rambo(3,2) = -p_rambo(3,1)
  end if
  call rambo0(n-2, sqrt_s, m_ex(3:n), p_scatt, wgt)
  p_rambo(  0,3:n) = p_scatt(  4,1:n-2)
  p_rambo(1:3,3:n) = p_scatt(1:3,1:n-2)
end subroutine rambo_2scatt


! *********************************************************************
subroutine rambo_decay(sqrt_s, m_ex, p_rambo)
! *********************************************************************
! Generate 1 -> n-1 PS-point with P(1) = P(2) + ... + P(n)
! Apply cleaning procedure to get full numerical precision
! *********************************************************************
! sqrt_s         = total cms energy
! m_ex(n)        = external particle masses
! p_rambo(0:3,n) = momenta, n = 2,..,n outgoing
! *********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_ramboX, only: rambo0 => rambo
  implicit none
  real(REALKIND), intent(in)  :: sqrt_s, m_ex(:)
  real(REALKIND), intent(out) :: p_rambo(0:3,size(m_ex))
  real(REALKIND) :: p_scatt(4,size(m_ex)-1), wgt
  integer :: n, k
  n = size(m_ex)
  if( m_ex(1) == 0 ) then
    print*, "[OpenLoops] Warning: decay of massless particle!"
  else
    p_rambo(0,1) =  sqrt_s
    p_rambo(1,1) =  0
    p_rambo(2,1) =  0
    p_rambo(3,1) =  0
  end if
  call rambo0(n-1, sqrt_s, m_ex(2:n), p_scatt, wgt)
  p_rambo(  0,2:n) = p_scatt(  4,1:n-1)
  p_rambo(1:3,2:n) = p_scatt(1:3,1:n-1)
end subroutine rambo_decay


subroutine rambo_c(sqrt_s, m_ex, n, p_rambo) bind(c,name="ol_rambo")
  use KIND_TYPES, only: REALKIND
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  integer(c_int), intent(in)  :: n
  real(c_double), intent(out) :: p_rambo(0:3,n)
  real(c_double), intent(in)  :: sqrt_s, m_ex(n)
  real(REALKIND) :: f_p_rambo(0:3,n)
  real(REALKIND) :: f_sqrt_s, f_m_ex(n)
  f_m_ex = m_ex
  f_sqrt_s = sqrt_s
  call rambo(f_sqrt_s, f_m_ex, f_p_rambo)
  p_rambo = f_p_rambo
end subroutine rambo_c

! #ifdef USE_RAMBO
#else

subroutine rambo(sqrt_s, m_ex, p_rambo)
  use KIND_TYPES, only: REALKIND
  implicit none
  real(REALKIND), intent(in)  :: sqrt_s, m_ex(:)
  real(REALKIND), intent(out) :: p_rambo(0:3,size(m_ex))
  p_rambo = 0 ! prevent compiler warning
  write(*,*) '[OpenLoops] ERROR: Rambo is not available.'
  stop
end subroutine rambo

subroutine rambo_c(sqrt_s, m_ex, n, p_rambo) bind(c,name="ol_rambo")
  use KIND_TYPES, only: REALKIND
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  integer(c_int), intent(in)  :: n
  real(c_double), intent(out) :: p_rambo(0:3,n)
  real(c_double), intent(in)  :: sqrt_s, m_ex(n)
  p_rambo = 0 ! prevent compiler warning
  write(*,*) '[OpenLoops] ERROR: Rambo is not available.'
  stop
end subroutine rambo_c

! #ifdef USE_RAMBO
#endif
! #ifdef PRECISION_dp
#endif


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
  use KIND_TYPES, only: REALKIND, DREALKIND
  use ol_parameters_decl_/**/REALKIND, only: psp_tolerance
  implicit none
  real(DREALKIND), intent(in)  :: P_in(0:3,n)
  integer,         intent(in)  :: n
  real(REALKIND),  intent(in)  :: m_ext2(n)
  real(REALKIND),  intent(out) :: P(0:3,n)
  real(REALKIND)  :: E_ref, P0(n), P2(n)
  real(REALKIND)  :: E0(2), E1(2), E2(2)
  real(REALKIND)  :: E0_tot, E1_tot, E2_tot
  real(REALKIND)  :: eps1, eps
  integer         :: nex, i, pout_max_pos, pout_max_pos_arr(1)
  real            :: prec

  P = P_in

!   call dirty_mom(P_in, P, n, 9)

  ! print momena before cleaning
!   do i = 1, n
!     write(*,*) P(:,i), (P(0,i)**2-P(1,i)**2-P(2,i)**2-P(3,i)**2)-m_ext2(i)
!   end do
!   write(*,*) sum(P(0,:)), sum(P(1,:)), sum(P(2,:)), sum(P(3,:))
!   write(*,*)

  E_ref = 0.5_/**/REALKIND * sum(abs(P(0,:)))
  ! check momentum conservation
  do i = 0, 3
    prec = abs(sum(P(i,:)))/E_ref
    if (prec > psp_tolerance) then
      write(*,*) "[OpenLoops] === WARNING ==="
      write(*,*) "[OpenLoops] OpenLoops subroutine clean_mom: inconsistent phase space point."
      write(*,*) "[OpenLoops] Momentum conservation is only satisfied to", -log10(prec), "digits."
      write(*,*) "[OpenLoops] ==============="
    end if
  end do

  ! check on-shell conditions
  do nex = 1, n
    P2(nex) = P(1,nex)*P(1,nex) + P(2,nex)*P(2,nex) + P(3,nex)*P(3,nex)
    P0(nex) = sign(sqrt(P2(nex) + m_ext2(nex)), P(0,nex))
    prec = abs(P(0,nex)-P0(nex))/E_ref
    if(prec > psp_tolerance) then
      write(*,*) "[OpenLoops] === WARNING ==="
      write(*,*) "[OpenLoops] OpenLoops subroutine clean_mom: inconsistent phase space point."
      write(*,*) "[OpenLoops] On-shell condition is only satisfied to", -log10(prec), "digits."
      write(*,*) "[OpenLoops] ==============="
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
!   E2(2)  = 0.5_/**/REALKIND/E0(2)**3*(P2(1)*(m_ext2(2) + P(1,2)**2 + P(2,2)**2) & ! equivalent formula w.o. beam-alignment instabilities
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
      P(0,nex) = sign(abs(P(3,nex)), real(P_in(0,nex), REALKIND))
    else
      P(0,nex) = E0(nex) + E1(nex)*eps + E2(nex)*eps**2
    end if
  end do

  ! print momenta after cleaning
!    do i = 1, n
!      write(*,*) P(:,i), (P(0,i)**2-P(1,i)**2-P(2,i)**2-P(3,i)**2)-m_ext2(i)
!    end do
!    write(*,*) sum(P(0,:)), sum(P(1,:)), sum(P(2,:)), sum(P(3,:))
!   write(*,*)

end subroutine clean_mom_in


! **********************************************************************
subroutine clean_mom_scatt(P_in, m_ext2, P, n)
! same as clean_mom_in but for 2-> n-2 PS point
! This routine is not used internally.
! **********************************************************************
  use KIND_TYPES, only: REALKIND, DREALKIND
  implicit none
  real(DREALKIND), intent(in)  :: P_in(0:3,n)
  integer,         intent(in)  :: n
  real(REALKIND),  intent(in)  :: m_ext2(n)
  real(REALKIND),  intent(out) :: P(0:3,n)
  real(DREALKIND) :: Q_in(0:3,n)
  real(REALKIND)  :: Q(0:3,n)

  Q_in(:,1:2) = P_in(:,1:2)
  Q_in(:,3:) = -P_in(:,3:)

  call clean_mom_in(Q_in, m_ext2, Q, n)

  P(:,1:2) = Q(:,1:2)
  P(:,3:) = -Q(:,3:)

end subroutine clean_mom_scatt


#ifdef USE_RAMBO
! **********************************************************************
subroutine dirty_mom(P_in, P, n, DIG)
! introduces random noise in every PS component after DIG digits
! (to test PS-point cleaning).
! Needs RAMBO subroutine rans().
! **********************************************************************
  use KIND_TYPES, only: REALKIND, DREALKIND
  use ol_ramboX, only: rans
  implicit none
  integer,        intent(in)  :: n, DIG
  real(REALKIND), intent(in)  :: P_in(0:3,n)
  real(REALKIND), intent(out) :: P(0:3,n)
  real(DREALKIND) :: x, shift
  integer        :: nex, i

  shift = 10._/**/DREALKIND**(-DIG)

  do nex = 1, n
    do i = 0, 3
      call rans(x)
      P(i,nex) = P_in(i,nex)*(1+(x-0.5_/**/REALKIND)*shift)
    end do
  end do

end subroutine dirty_mom
! #ifdef USE_RAMBO
#else
subroutine dirty_mom(P_in,P, n, DIG)
  use KIND_TYPES, only: REALKIND
  implicit none
  integer,        intent(in)  :: n, DIG
  real(REALKIND), intent(in)  :: P_in(0:3,n)
  real(REALKIND), intent(out) :: P(0:3,n)
  write(*,*) '[OpenLoops] ERROR: dirty_mom() requires Rambo.'
  stop
end subroutine dirty_mom
! #ifdef USE_RAMBO
#endif


! **********************************************************************
subroutine conv_mom_scatt2in(P_scatt, m_ext2, P_in_clean, perm_inv, n)
! Keep two incoming momenta and reverse outgoing momenta.
! Apply phase space cleaning and crossing.
! ToDo: cleaning for n_scatt /= 2
! **********************************************************************
  use KIND_TYPES, only: REALKIND, DREALKIND
  use ol_external_decl_/**/REALKIND, only: nParticles, P_ex, inverse_crossing
  use ol_external_decl_/**/DREALKIND, only: n_scatt
  use ol_parameters_decl_/**/DREALKIND, only: scalefactor
  implicit none
  integer,           intent(in)  :: n
  real(DREALKIND),   intent(in)  :: P_scatt(0:3,n)
  real(REALKIND),    intent(in)  :: m_ext2(n)
  real(REALKIND),    intent(out) :: P_in_clean(0:3,n)
  integer,           intent(in)  :: perm_inv(n)
  real(DREALKIND) :: P_in(0:3,n)
  real(REALKIND)  :: P_clean(0:3,n), m_ext2_perm(n)
  integer         :: k
  nParticles = n
  P_ex(:,1:n) = P_scatt
  inverse_crossing(1:n) = perm_inv
  do k = 1, n
    m_ext2_perm(perm_inv(k)) = m_ext2(k)
  end do
  P_in(:,1:n_scatt) =   scalefactor * P_scatt(:,1:n_scatt)
  P_in(:,n_scatt+1:)  = - scalefactor * P_scatt(:,n_scatt+1:)
  if (n_scatt == 2) then
    ! Clean momenta to get full numerical precision.
    ! Do the cleaning in the original permutation where the first two momenta are incoming.
    ! Otherwise the beam alignment (zero components) might be spoiled by the cleaning.
    call clean_mom_in(P_in, m_ext2_perm, P_clean, n)
  else
    P_clean = P_in
  end if
  do k = 1, n
    P_in_clean(:,k) = P_clean(:,perm_inv(k))
  end do
end subroutine conv_mom_scatt2in


! **********************************************************************
subroutine conv_mom_os(P_decay, P_in, n)
! reverse decay products
! ToDo: Apply phase space cleaning
! **********************************************************************
  use KIND_TYPES, only: REALKIND, DREALKIND
  use ol_parameters_decl_/**/DREALKIND, only: scalefactor
  implicit none
  integer,         intent(in)  :: n
  real(DREALKIND), intent(in)  :: P_decay(0:3,n)
  real(REALKIND),  intent(out) :: P_in(0:3,n)
  integer         :: k

  P_in  = - scalefactor * P_decay

end subroutine conv_mom_os



#ifdef PRECISION_dp

! **********************************************************************
subroutine write_INmom(P_ex, n, unit)
! **********************************************************************
! Write the information on the four momenta.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none

  real(REALKIND), intent(in) :: P_ex(0:3,n)
  integer,        intent(in) :: n, unit
  integer        :: i, k
  real(REALKIND) :: Ptot(0:3), Pabs(0:3), PR(0:3)

  do i = 0, 3
    Ptot(i) = P_ex(i,1)
    Pabs(i) = abs(P_ex(i,1))
    do k = 2, n
      Ptot(i) = Ptot(i) + P_ex(i,K)
      Pabs(i) = Pabs(i)+abs(P_ex(i,K))
    end do
    PR(i) = Ptot(i)/Pabs(i)
  end do

  write (unit,*) "------------------------------------", "-----------------------------------------"
  write (unit,*) " ", n, " -> 0  Phase space point:"
  write (unit,*) "------------------------------------", "-----------------------------------------"
  write (unit,*) "n        E             px             ", "py              pz               m "
  do i = 1, n
    write (unit,'(i2,1x,5e15.7)') i, P_ex(0,i), P_ex(1,i), P_ex(2,i), P_ex(3,i), sqrt(abs(cont_LL(P_ex(0,i),P_ex(0,i))))
  end do
  write (unit,*) "------------------------------------", "-----------------------------------------"
  write (unit,'(A2,1x,4e15.7)') 'T', PR(0), PR(1), PR(2), PR(3)
  write (unit,*) "------------------------------------", "-----------------------------------------"
  write (unit,*)
  contains
  function cont_LL(A, B)
    ! Contraction of (real) Lorentz vectors in standard representation.
    ! Don't use the contractions module to avoid dependency cycles.
    use KIND_TYPES, only: REALKIND
    implicit none
    real(REALKIND) :: cont_LL
    real(REALKIND), intent(in) :: A(0:3), B(0:3)
    cont_LL = A(0)*B(0) - A(1)*B(1) - A(2)*B(2) - A(3)*B(3)
  end function cont_LL
end subroutine write_INmom

! #ifdef PRECISION_dp
#endif



! **********************************************************************
subroutine internal_momenta(P, Npart)
! **********************************************************************
! P(0:3,Npart) = external real-valued four-momenta (standard representation)
! Npart        = total (in & out) external particle number
! Q(1:5,1:Npart^2-2) = internal four-momenta in light-cone representation;
!                      the fifth component is the C-valued squared momentum.
! Numbering of internal momenta:
!   Sum_i s(i)*P(i) => Q(Sum_i s(i)*2^(i-1)), s(i) = 0, 1
!   so that Q(J1) + Q(J2) = Q(J1+J2)
! QInvariantsMatrix(i,j) = (p_i+p_j)^2 for i /= j, otherwise undefined.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q, QInvariantsMatrix
  implicit none

  real(REALKIND), intent(in) :: P(0:3,Npart)
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
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  implicit none

  real(REALKIND), intent(in) :: P_ex(0:3,Npart)
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
  use ol_momenta_decl_/**/REALKIND, only: Q
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
  use KIND_TYPES, only: REALKIND
  use ol_loop_parameters_decl_/**/DREALKIND, only: ti_os_thresh, mureg
  implicit none
  complex(REALKIND) :: squeeze_onshell
  complex(REALKIND), intent(inout) :: pinv
  real(REALKIND) :: masses(:), mass
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
  use KIND_TYPES, only: REALKIND
  use ol_parameters_decl_/**/REALKIND, only: &
    & wMW, rMW, wMZ, rMZ, wMH, rMH, wMC, rMC, wMB, rMB, wMT, rMT
  implicit none
  complex(REALKIND), intent(in) :: moms(:,:)
  complex(REALKIND) :: invs(binom2(size(moms,2)+1))
  complex(REALKIND) :: moms0(0:3,0:size(moms,2))
  real(REALKIND) :: masses(0:6)
  integer :: n, k, a, b
  n = size(moms,2) + 1
  moms0(:,0) = 0
  moms0(:,1:n-1) = moms
#ifdef COLLIER_LEGACY
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
#else
  do k = 1, size(invs)
    invs(k) = cont_L_cmplx(moms0(:,mod(k-1,n)) - moms0(:,mod(k+((k-1)/n),n)))
  end do
#endif
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

end module ol_kinematics_/**/REALKIND
