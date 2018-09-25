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


module ol_loop_reduction_/**/REALKIND
  use ol_debug, only: ol_fatal, ol_msg, ol_error
  use KIND_TYPES, only: REALKIND
  implicit none

  ! Thresholds used for the expansions
  real(REALKIND) :: m_thres = 1.E-9
  real(REALKIND) :: delta_thres = 1.E-2

#ifdef PRECISION_dp
  ! In order to switch off the expansions, set DeltaExp = .false.
  logical :: DeltaExp = .true.
#else
  ! Expansions are switched on in quad-precision by default. 
  ! Set DeltaExp = .false. in the following to siwtch them off in quad-precision
  logical :: DeltaExp = .true.
#endif

  interface TI_reduction
    module procedure TI_reduction_1
  end interface


contains


! ******************************************************************************
subroutine TI_bubble_red(Gin_A,p,msq,Gout_A,M2R1,A0_0,A0_1)
! Reduction of tensor bubbles
! ------------------------------------------------------------------------------
! Gin_A   = input closed-loop two-point integral
! msq     = array containing the squared masses m0^2, m1^2
! Gout_A  = scalar output bubble
! M2R1    = R1 rational part of the amplitude
! A0_i    = tadpole with mass m_i
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ofred_reduction_/**/REALKIND, only: twopoint_reduction
  implicit none
  complex(REALKIND), intent(in)  :: Gin_A(:)
  complex(REALKIND), intent(in)  :: p(1:5), msq(0:1)
  complex(REALKIND), intent(out) :: Gout_A(1)
  complex(REALKIND), intent(inout) :: M2R1
  complex(REALKIND), optional :: A0_0(1), A0_1(1)
  complex(REALKIND) :: redcoeff(1:4)

  redcoeff = 0._/**/REALKIND

  call twopoint_reduction(Gin_A,p,msq,redcoeff)

  Gout_A(1) = redcoeff(1)
  M2R1 = M2R1 + redcoeff(4)

  if (present(A0_0) .AND. present(A0_1)) then
    A0_0 = msq(0)*redcoeff(2)
    A0_1 = msq(1)*redcoeff(3)
  else if(present(A0_0)) then
    if (msq(0) == msq(1)) then
      A0_0 = msq(0)*(redcoeff(2) + redcoeff(3))
    else if (msq(0) == 0._/**/REALKIND) then
      A0_0 = msq(1)*redcoeff(3)
    else
      A0_0 = msq(0)*redcoeff(2)
    end if
  end if

end subroutine TI_bubble_red


! ******************************************************************************
subroutine TI_triangle_red(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,M2R1,&
A0msq,A0_0,A0_1,A0_2)
! Reduction of tensor triangles
! ------------------------------------------------------------------------------
! Gin_A    = input closed-loop three-point integral
! RedBasis = Reduction basis
! msq      = array of the squared masses m_0^2, m_1^2, m_2^2
! Gout_A   = output scalar triangle
! Gout_Ai  = output scalar bubbles originating from Di pinch
! M2R1     = R1 rational part of the amplitude
! A0msq    = squared masses in the tadpoles
! A0_i     = tadpole of mass m_i
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis
  use ofred_reduction_/**/REALKIND, only: otf_3pt_reduction_last
  implicit none
  complex(REALKIND), intent(inout)  :: Gin_A(:)
  type(basis), intent(in) :: RedBasis
  complex(REALKIND), intent(in) :: msq(0:2)
  complex(REALKIND), intent(out):: Gout_A(1), Gout_A0(1), Gout_A1(1), Gout_A2(1)
  complex(REALKIND), intent(inout):: M2R1
  complex(REALKIND), optional, intent(in) :: A0msq(:)
  complex(REALKIND), optional, intent(out) :: A0_0(1), A0_1(1), A0_2(1)
  complex(REALKIND) :: Gout_R1, momenta(5,3)
  real(REALKIND) :: sdlt
  integer :: perm(3), m_ind
  logical :: tch_top

  !! Check for a t-channel triangle topology with a massless external leg
  call tch_triangle_check(RedBasis%mom1, RedBasis%mom2, tch_top, perm, sdlt, momenta)

  Gout_R1 = 0._/**/REALKIND

  if(tch_top) then
  !! Exact reduction formulas or Gram-\Delta expansions
    if (present(A0_2)) then
      call t_channel_triangle_reduction(perm,sdlt,momenta,msq,Gin_A,Gout_A,Gout_A0,Gout_A1,&
                                      Gout_A2,Gout_R1,A0msq,A0_0,A0_1,A0_2)
    else if(present(A0_1)) then
      call t_channel_triangle_reduction(perm,sdlt,momenta,msq,Gin_A,Gout_A,Gout_A0,Gout_A1,&
                                      Gout_A2,Gout_R1,A0msq,A0_0,A0_1)
    else if(present(A0_0)) then
      call t_channel_triangle_reduction(perm,sdlt,momenta,msq,Gin_A,Gout_A,Gout_A0,Gout_A1,&
                                      Gout_A2,Gout_R1,A0msq,A0_0)
    else
      call t_channel_triangle_reduction(perm,sdlt,momenta,msq,Gin_A,Gout_A,Gout_A0,Gout_A1,&
                                      Gout_A2,Gout_R1)
    end if

  else

    !! On-the-fly reduction-like
    if(present(A0_2)) then
      call otf_3pt_reduction_last(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_R1,&
                                A0msq,A0_0,A0_1,A0_2)
    else if(present(A0_1)) then
      call otf_3pt_reduction_last(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_R1,&
                                A0msq,A0_0,A0_1)
    else if(present(A0_0)) then
      call otf_3pt_reduction_last(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_R1,&
                                A0msq,A0_0)
    else
      call otf_3pt_reduction_last(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_R1)
    end if

  end if

  M2R1 = M2R1 + Gout_R1

end subroutine TI_triangle_red


! ********************************************************************************
!                            t-channel triangle topology
! ********************************************************************************
!               -->                      <--
!               p1 _____________________ -p2
!                     \             /
!                      \           /
!   p1^2= -p^2          \         /
!                        \       /
!   p2^2= -p^2(1+dlt)     \     /
!                          \   /
!                           \ /
!                            |
!                            |
!                         (p2-p1)^2 = 0
!
! ********************************************************************************


! ******************************************************************************
subroutine tch_triangle_check(mom1, mom2, tr_t_top, perm, sdlt, mom)
! ------------------------------------------------------------------------------
! Check for a t-channel triangle topology with one external massless leg
! TODO: this check might be cached
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: L
  use ol_loop_parameters_decl_/**/REALKIND, only: mureg
  implicit none
  integer, intent(in) :: mom1, mom2
  logical, intent(out) :: tr_t_top
  integer, intent(out) :: perm(3)
  complex(REALKIND), intent(out) :: mom(5,3)
  real(REALKIND), intent(out) :: sdlt
  logical :: zero_mass(3)
  complex(REALKIND) :: p1(5), p2(5)
  complex(REALKIND) :: k1(5), k2(5), k12(5)
  real(REALKIND) :: rone

  rone = 1._/**/REALKIND

  !! k1 and k2 are the external momenta flowing into the triangle
  k1(1:4)  = L(1:4,mom1)
  k1(5)    = L(5,mom1) + L(6,mom1)
  k2(1:4)  = L(1:4,mom2-mom1)
  k2(5)    = L(5,mom2-mom1) + L(6,mom2-mom1)
  k12(1:4) = L(1:4,mom2)
  k12(5)   = L(5,mom2) + L(6,mom2)

  tr_t_top  = .FALSE.
  zero_mass = .FALSE.

  !! Check for an external massless leg.
  !! TODO: is mureg the appropriate scale to choose for normalization?
  zero_mass(1) = abs(k1(5) )/mureg**2 < m_thres
  zero_mass(2) = abs(k2(5) )/mureg**2 < m_thres
  zero_mass(3) = abs(k12(5))/mureg**2 < m_thres

  if((zero_mass(1) .and. zero_mass(2)) .or. &
     (zero_mass(1) .and. zero_mass(3)) .or. &
     (zero_mass(2) .and. zero_mass(3)) ) return

  if(zero_mass(1) .AND. REAL(k12(5)*k2(5)) > 0) then
    tr_t_top = .TRUE.
    mom(1:5,1) =   k1(1:5)
    mom(1:4,2) = - k2(1:4)
    mom(5,2)   =   k2(5)
    mom(1:4,3) = - k12(1:4)
    mom(5,3)   =   k12(5)
    if(abs(k2(5)) > abs(k12(5))) then
      perm = [2,0,1]
      sdlt = ABS(k2(5)/k12(5)) - rone
    else
      perm = [2,1,0]
      sdlt = ABS(k12(5)/k2(5)) - rone
    end if

  else if(zero_mass(2) .AND. REAL(k1(5)*k12(5)) > 0) then
    tr_t_top = .TRUE.
    mom(1:5,1) = k1(1:5)
    mom(1:5,2) = k2(1:5)
    mom(1:5,3) = k12(1:5)
    if(abs(k1(5)) > abs(k12(5))) then
      perm = [0,2,1]
      sdlt = ABS(k1(5)/k12(5)) - rone
    else
      perm = [0,1,2]
      sdlt = ABS(k12(5)/k1(5)) - rone
    end if

  else if(zero_mass(3) .AND. REAL(k1(5)*k2(5)) > 0) then
    tr_t_top = .TRUE.
    mom(1:4,1) = - k1(1:4)
    mom(5,1)   =   k1(5)
    mom(1:5,2) =   k2(1:5)
    mom(1:5,3) =   k12(1:5)
    if(abs(k1(5)) > abs(k2(5))) then
      perm = [1,2,0]
      sdlt = ABS(k1(5)/k2(5)) - rone
    else
      perm = [1,0,2]
      sdlt = ABS(k2(5)/k1(5)) - rone
    end if
  else
    tr_t_top = .FALSE.
  end if

end subroutine tch_triangle_check


! ******************************************************************************
subroutine tch_triangle_exact(r,G_in,msq,masses_ind,dlt,psq,p1,p2,&
& redcoeff)
! ******************************************************************************
! OpenLoops Reduction. Reduction of three point integrals with one
! external massless leg. Exact reduction formulae
! ------------------------------------------------------------------------------
! G_in     = input OL coefficient
! r        = tensor integral rank
! msq      = array of internal masses squared
! dlt      = \delta parameter
! p1, p2   = external momenta in the triangle
! redcoeff = (1) scalar triangle
!            (2) D0-pinch
!            (3) D1-pinch
!            (4) D2-pinch
!            (5) m0-tadpole
!            (6) m1-tadpole
!            (7) rational part
! masses_ind = integer for the internal masses configuration
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), intent(in)  :: G_in(:)
  complex(REALKIND), intent(in) :: msq(0:2), p1(1:5), p2(1:5)
  integer, intent(in) :: r, masses_ind
  real(REALKIND), intent(in) :: dlt, psq
  complex(REALKIND), intent(out) :: redcoeff(1:7)

  complex(REALKIND) :: p12(1:4), psq_delta, psq_delta2
  complex(REALKIND) :: contr_p12, contr_p1, contr_p2

  complex(REALKIND) :: p1p1(6:15), p2p2(6:15), p1p2(6:15)
  complex(REALKIND) :: p1p1p1(16:35), p2p2p2(16:35), p1_p2(16:35), kp2(16:35)
  complex(REALKIND) :: P11, P22, T, U, Gmn, W0, P111, P222, V1, Gp1, Gp2, Gm, Gp
  complex(REALKIND) :: T3, T6, U3, V16, P1116, P111z3, P16
  complex(REALKIND) :: dlt2, dlt3, dlt4, zero, one, two, five
  complex(REALKIND) :: z, z0, z01, z1, ct1, ct2, z12, z2, zd
  integer :: i, j, k, n

  zero = 0._/**/REALKIND
  one  = 1._/**/REALKIND
  two  = 2._/**/REALKIND
  five = 5._/**/REALKIND

  redcoeff = zero
  psq_delta = psq*dlt
  psq_delta2 = psq_delta*dlt

  !------------------------------------------------------------------------*
  !--------------------- Formulae for the rank-1 part ---------------------*
  !------------------------------------------------------------------------*
  p12(1:4) = p1(1:4) - p2(1:4)

  if(r == 1) then

    contr_p1  = SUM(G_in(2:5)*p1(1:4) )  !! G_\mu p_1^{\mu}
    contr_p2  = SUM(G_in(2:5)*p2(1:4) )  !! G_\mu p_2^{\mu}
    contr_p12 = SUM(G_in(2:5)*p12(1:4))  !! G_\mu (p_1^{\mu} - p_2^{\mu})

    !! masses: (0,0,0)
    if (masses_ind == 0) then
      redcoeff(1) = -contr_p12/dlt - contr_p1
      redcoeff(2) = - contr_p12/psq_delta !scaleless bubble
      redcoeff(3) = (2*contr_p12)/psq_delta2 + (contr_p1 + contr_p12)/psq_delta
      redcoeff(4) = -(2*contr_p12)/psq_delta2 - contr_p1/psq_delta

    !! masses: (m0,0,0)
    else if (masses_ind == 1) then
      z0  = msq(0)/psq
      z01 = one + z0

      redcoeff(1) = -z01*contr_p12/dlt - contr_p1
      redcoeff(2) = - contr_p12/psq_delta !scaleless bubble
      redcoeff(3) = (2*contr_p12)/psq_delta2 + (contr_p1 + contr_p12)/psq_delta
      redcoeff(4) = -(2*contr_p12)/psq_delta2 - contr_p1/psq_delta

    !! masses: (m,m,m)
    else if(masses_ind == 3 ) then
      redcoeff(1) = -contr_p12/dlt - contr_p1
      redcoeff(2) = -contr_p12/psq_delta
      redcoeff(3) = 2*contr_p12/psq_delta2 + (contr_p1 + contr_p12)/psq_delta
      redcoeff(4) = -2*contr_p12/psq_delta2 - contr_p1/psq_delta

    !! masses: (0,m,m)
    else if (masses_ind == 6) then
      redcoeff(1) = contr_p12*(-1/dlt + msq(1)/psq_delta) - contr_p1
      redcoeff(2) = -contr_p12/psq_delta
      redcoeff(3) = 2*contr_p12/psq_delta2 + (contr_p1 + contr_p12)/psq_delta
      redcoeff(4) = -(2*contr_p12)/psq_delta2 - contr_p1/psq_delta

    end if

  !------------------------------------------------------------------------*
  !--------------------- Formulae for the rank-2 part ---------------------*
  !------------------------------------------------------------------------*
  else if (r == 2) then

  dlt2 = dlt**2
  dlt3 = dlt**3

  n = 6
  do i = 1, 4
    do j = i, 4
      p1p1(n) = p1(i)*p1(j)
      p2p2(n) = p2(i)*p2(j)
      p1p2(n) = p12(i)*p12(j)
      n = n + 1
    end do
  end do

  P11 = SUM(p1p1*G_in(6:15))/psq  !! G_{\mu\nu} p_1^{\mu} p_1^{\nu}
  P22 = SUM(p2p2*G_in(6:15))/psq  !! G_{\mu\nu} p_2^{\mu} p_2^{\nu}
  T   = SUM(p1p2*G_in(6:15))/psq  !! G_{\mu\nu} (p_1^{\mu} - p_2^{\mu})(p_1^{\nu} - p_2^{\nu})
  Gmn = (G_in(7)-G_in(14))/2      !! g^{\mu\nu}/4
  U   = P11 - P22

    !! masses: (0,0,0)
    if (masses_ind == 0) then
      redcoeff(1) = (T/dlt2 + (T+U)/dlt + P11)*psq
      redcoeff(2) = (T/dlt2 + (T/2 + U)/dlt)
      redcoeff(3) = (-3*T/dlt3 + (one + 1/dlt)*Gmn - (7*T+5*U)/(2*dlt2) - &
      (1.5*P11 + 0.5*T+U)/dlt)

      redcoeff(4) = (3*T/dlt3 + 2.5*(T+U)/dlt2 - Gmn/dlt + 1.5*P11/dlt)
      redcoeff(7) = (-T/dlt2 - (T+U)/(2*dlt) + Gmn)

    !! masses: (m0,0,0)
    else if (masses_ind == 1) then ! m0!=0, m1=m2=0
      z0  = msq(0)/psq
      z01 = one + z0

      redcoeff(1) = (T*(z01**2)/dlt2 + (T+U)*z01/dlt + P11)*psq
      redcoeff(2) = z01*T/dlt2 + (T/2 + U)/dlt

      redcoeff(3) = -3*z01*T/dlt3 - ((z0+five)*U + (two+11*z01)*T)/(2*dlt2) - &
      ((5*z0+8)*T +(z0+10*one)*P11 - (2*z0+7*one)*P22)/(2*dlt) - &
      (T/2 + U + 3*P11/2) + Gmn*(z01/dlt + z01 + one + dlt)
      redcoeff(3) = redcoeff(3)/(one + dlt)

      redcoeff(4) = 3*z01*T/dlt3 + (z0+five)*(T+U)/(2*dlt2) + &
      (3*one-z0)*P11/(2*dlt) -Gmn*z01/dlt

      redcoeff(5) = (T/dlt2 + (T+U/2)/dlt + P11/2)*z0
      redcoeff(5) = redcoeff(5)/(one + dlt)

      redcoeff(7) = (-T/dlt2 - (3*T+U)/(2*dlt) - (T+U)/2 + (one + dlt)*Gmn)
      redcoeff(7) = redcoeff(7)/(one + dlt)

    !! masses: (m,m,m)
    else if(masses_ind == 3 ) then
      z = msq(0)/psq

      redcoeff(1) = psq*((one-2*z)*T/dlt2 + (one-z)*(T+U)/dlt + 2*z*Gmn + P11)
      redcoeff(2) = T/dlt2 + (U+T/2)/dlt
      redcoeff(3) =(-3*T/dlt3 - (5*U+7*T)/(2*dlt2) + Gmn/dlt - (3*P11+T+2*U)/(2*dlt) + Gmn)
      redcoeff(4) =(3*T/dlt3 + 2.5*(U+T)/dlt2 + (1.5*P11-Gmn)/dlt)
      redcoeff(7) = (-T/dlt2 - (T+U)/(2*dlt) + Gmn)

    !! masses: (0,m1,m1)
    else if(masses_ind == 6) then
      z1 = msq(1)/psq

      redcoeff(1) = psq*((one+z1**2-4*z1)*T/dlt2 + (one-2*z1)*(T+U)/dlt + 2*z1*Gmn + P11)
      redcoeff(2) = (one-2*z1)*T/dlt2 + (T+(two-z1)*U)/(2*dlt) + &
      z1*(U-P11)/(2*(one+dlt))
      redcoeff(3) = -3*(one-z1)*T/dlt3 + ((5*z1-7*one)*T+(z1-five)*U)/(2*dlt2) + &
      (one-z1)*Gmn/dlt - ((3*one+z1)*P11-(z1-two)*U+T)/(2*dlt) + &
      Gmn + (z1*P11-z1*U)/(2*(one+dlt))
      redcoeff(4) = 3*(one-z1)*T/dlt3 + (five-z1)*(T+U)/(2*dlt2)-&
      Gmn*(one-z1)/dlt + (3*one+z1)*P11/(2*dlt)
      redcoeff(7) = -(one+z1)*T/dlt2 - (T+(one+z1)*U)/(2*dlt) + Gmn + &
      z1*(U-P11)/(two+2*dlt)
    end if

  !------------------------------------------------------------------------*
  !--------------------- Formulae for the rank-3 part ---------------------*
  !------------------------------------------------------------------------*
  else if (r == 3) then
  dlt2 = dlt**2
  dlt3 = dlt**3
  dlt4 = dlt**4

  n = 16
  do i = 1, 4
    do j = i, 4
      do k = j, 4
        p1p1p1(n) = p1(i)*p1(j)*p1(k)
        p2p2p2(n) = p2(i)*p2(j)*p2(k)
        p1_p2(n)  = p12(i)*p12(j)*p12(k)
        kp2(n) = p12(i)*p12(j)*p2(k) + p12(i)*p12(k)*p2(j) + p12(k)*p12(j)*p2(i)

        n = n + 1
      end do
    end do
  end do

  P111 = SUM(p1p1p1(16:35)*G_in(16:35))/psq
  P222 = SUM(p2p2p2(16:35)*G_in(16:35))/psq

  T  = SUM(p1_p2(16:35)*G_in(16:35))/psq
  W0 = -2*T - SUM(kp2(16:35)*G_in(16:35))/psq
  U  = W0 - T
  V1 = W0 - P111 + P222

  !! g^{\mu\nu} p^{\rho}_1 + g^{\rho\mu} p^{\nu}_1 + g^{\nu\rho} p^{\mu}_1
  Gp1 = 2*((2*G_in(17) - G_in(24))*p1(1) + (2*G_in(20) - G_in(30))*p1(2) + &
        (G_in(21) - 2*G_in(33))*p1(3) + (G_in(22) - 2*G_in(34))*p1(4))

  !! g^{\mu\nu} p^{\rho}_2 + g^{\rho\mu} p^{\nu}_2 + g^{\nu\rho} p^{\mu}_2
  Gp2 = 2*((2*G_in(17) - G_in(24))*p2(1) + (2*G_in(20) - G_in(30))*p2(2) + &
        (G_in(21) - 2*G_in(33))*p2(3) + (G_in(22) - 2*G_in(34))*p2(4))

  Gp = (Gp1+Gp2)/12
  Gm = (Gp1-Gp2)/12

    !! masses: (0,0,0)
    if (masses_ind == 0) then
      redcoeff(1) = psq*(-T/dlt3 + U/dlt2 + V1/dlt - P111)
      redcoeff(3) = (11*T/3)/dlt4 + (T-10*U/3)/dlt3 - (Gm + T/2 + U + 17*V1/6)/dlt2 + &
      (2*T + 3*U - 6*V1 + 11*P111 - Gp1)/(6*dlt) - Gp

      redcoeff(4) = -(11*T/3)/dlt4 + (10*U/3)/dlt3 + (Gm + 17*V1/6)/dlt2 + (Gp1-11*P111)/(6*dlt)

      redcoeff(7) = (5*T/3)/dlt3 - (T+4*U)/(3*dlt2) + (T/18 + (U - 5*V1)/6 -Gm)/dlt - (5*Gp1+2*Gp2)/36

    !! masses: (m0,0,0)
    else if (masses_ind == 1) then
      z   = msq(0)/psq
      z2  = z**2
      z1  = one + z
      zd  = z1/dlt
      z12 = z1**2
      T3  = T/3
      T6  = T/6
      U3  = U/3
      V16 = V1/6
      P16 = P111/6

      redcoeff(1) = (-T*zd**3 + U*zd**2 + V1*zd - P111)*psq

      redcoeff(2) = -T*z12/dlt3 + (T/2 + U)*z1/dlt2 - (T3 + U/2 - V1)/dlt

      redcoeff(3) =(-dlt2*Gp + (dlt*(11*P111 + 2*T + 3*U - 6*V1 - &
      3*Gp*(6*one + z) - 3*Gm*(two + 3*z)))/6 + ((-(U*(10*one + z)) + &
      5*T*(5*one + 6*z))*z1)/(3*dlt3) + (11*T*z12)/(3*dlt4) + (23*P111 &
      - P222 - U - 28*V1 - 10*P111*z + 5*P222*z - 4*U*z - 14*V1*z + &
      3*Gp*(-6*one - 2*z + z2) - 3*Gm*(6*one + 10*z + 3*z2))/6 + &
      (11*P111 + 2*T - 29*U - 40*V1 - 5*P111*z + 7*T*z - 45*U*z - &
      13*V1*z + 2*P111*z2 + 11*T*z2 - 6*U*z2 + 3*V1*z2 + 3*Gp*(-two - z +&
      z2) - 3*Gm*(6*one + 11*z + 5*z2))/(6*dlt) + (-46*U - 17*V1 - 58*U*z -&
      4*V1*z - 6*Gm*z12 - 6*U*z2 + V1*z2 + T*(31 + 78*z + &
      51*z2))/(6*dlt2))/(one + dlt)**2

      redcoeff(4) = ((Gm + Gp)*(2*one - z)*z1)/(2*dlt) + (U*(10*one + &
      z)*z1)/(3*dlt3) + (Gm*z12)/dlt2 - (11*T*z12)/(3*dlt4) - &
      (V1*(-17*one - 4*z + z2))/(6*dlt2) - (P111*(11*one - 5*z + 2*z2))/(6*dlt)

      redcoeff(5) = -5*z1*T3/dlt3 + ((z+4*one)*U3-(17*z+13*one)*T6)/dlt2 + &
      ((5.5*one + 2*z)*U3 - (z - five)*V16 + Gm*z1 - T6*(5*z + one))/dlt + &
      (z*Gp)/2+P16*(4*z-five)+Gm*(1.5*z + two) + T3 + U/2 + (five + z)*V16 + &
      dlt*((z*Gp)/2 + (2*z-five)*P16 + Gm*(z/2 +one))
      redcoeff(5) = z*redcoeff(5)/((one+ dlt)**2)

      redcoeff(7) = 5*z1*T3/dlt3 + ((6*z+4*one)*T3 -(z+4*one)*U3)/dlt2 + &
      ((z-five/3)*T6-(1.5*z + 3.5*one)*U3 + (z-five)*V16 - Gm*z1)/dlt + &
      (T3 + U -7*Gp-z*P111-Gm*(6*z + 9*one) - 5*V1)/6 - (5*Gp1 + 2*Gp2)*dlt/36
      redcoeff(7) = redcoeff(7)/(one + dlt)

    !! masses: (m,m,m)
    else if(masses_ind == 3 ) then
      z = msq(0)/psq
      ct1 = one - 2*z
      ct2 = 4*z
      T6  = T/6
      U3  = U/3
      V16 = V1/6
      P1116 = (11*P111)/6
      P111z3 = (z*P111)/3

      redcoeff(1) = psq*((T*(6*z - one))/dlt3 - U*(ct2 - one)/dlt2 + &
      6*(ct1*V16 - z*Gm)/dlt - (z*Gp1)/2 - P111)

      redcoeff(3) = -(16*z - 11*one)*T/(3*dlt4) + &
      (4*(7*one - 12*z)*T6 + 2*(ct2-five)*U3)/dlt3 + &
      ((12*z - 13*one)*U3 + (ct2 - 17*one)*V16 + Gm*(8*z - one) + 3*T6*(one-ct2))/dlt2 + &
      (P1116-ct1*Gp+P111z3+Gm*(14*z - two) - ct1*(T6 + (3*U3)/2) + (6*z - 23*one)*V16)/dlt + &
      P1116 + 2*Gp*(z-one) - Gp*dlt + T/3 + U/2 - V1 + Gm*(6*z-one)
      redcoeff(3) = redcoeff(3)/(one + dlt)

      redcoeff(4) =(16*z-11*one)*T/(3*dlt4) + 2*(five-ct2)*U3/dlt3 + &
      ((one-8*z)*Gm-(ct2-17*one)*V16)/dlt2 + (ct1*(Gp+Gm)-P1116-P111z3)/dlt

      redcoeff(5) = ((8*z - 3*one)*T)/(3*dlt3) + &
      ((3*one-ct2)*U3 + 3*T6*(ct2 - one))/dlt2 - &
      (ct2*Gm - ct1*(T6 + 1.5*U3) + 2*(z - 3*one)*V16)/dlt - &
      ct2*Gm + P111z3 - 2*T6 - 1.5*U3 + V1
      redcoeff(5) = redcoeff(5)/(one + dlt)

      redcoeff(7) = 16*T6*(one-z)/dlt3 + ((11*one-12*z)*T6 + (ct2 - 7*one)*U3)/dlt2 + &
      ((3*z - 4*one)*T/9 + (z - five/3)*U - (ct1 + 10*one)*V16 + Gm*(ct2 - one))/dlt + &
      (Gm*(8*z - 3*one))/2 - 7*one*Gp/6 - P111z3  + &
      ((-Gm - (7*Gp)/3)*dlt)/2 + (7*T6)/3 + 2*U3 - 11*V16
      redcoeff(7) = redcoeff(7)/(one + dlt)

    !! masses: (0,m1,m1)
    else if(masses_ind == 6) then
      z   = msq(1)/psq
      z2  = z**2
      ct1 = one - z
      T3  = T/3
      T6  = T/6
      U3  = U/3
      V16 = V1/6
      P16 = P111/6

      redcoeff(1) = psq*(-(ct1*(ct1**2-6*z)*T)/dlt3 + &
      (U*(3*z*(z-two)+one))/dlt2 + (-6*ct1*z*Gm+(ct1-2*z)*V1)/dlt -(z*Gp1)/2-P111)

      redcoeff(3) = ((11*(ct1**2) - 16*z)*T3)/dlt4 + &
      (5*(6*z2-19*z+five)*T3+(19*z-z2-10*one)*U3)/dlt3 + &
      ((8*z-ct1**2)*Gm + (-138*z + 51*z2 + 31*one)*T6 + (49*z-3*z2-23*one)*U3 + &
      (8*z-17*one + z2)*V16)/dlt2 + &
      ((Gp2*(5*z+z2-two))/12-2*Gm*(z2-15*z+two)+P16*(7*z+11*one+2*z2) + &
      (11*z2-17*z+two)*T6+(37.5*z-3*z2-14.5*one)*U3+(23*z+3*z2-40*one)*V16)/dlt + &
      Gm*(25*z-1.5*z2-3*one)+Gp*(5*z+z2/2-3*one)+ P16*(7*z+22*one)+2.5*z*U+(15*z-29*one)*V16 +&
      T6*(7*z + one)+ dlt*(20*P16+ Gp*(2.5*z-3*one) + T3 - (1.5*P111 - U/2 + V1) + &
      Gm*(7.5*z-one))-Gp*dlt2
      redcoeff(3) = redcoeff(3)/((one + dlt)**2)

      redcoeff(4) =-((11*ct1**2-16*z)*T3)/dlt4 + (z2-19*z+10*one)*U3/dlt3 + &
      ((ct1**2-8*z)*Gm-(z2+8*z-17*one)*V16)/dlt2 - &
      ((Gm+Gp)*(2.5*z+z2/2-one) + P16*(7*z+2*z2+11*one))/dlt

      redcoeff(5) = (19*z-8*z2-3*one)*T3/dlt3 + &
      ((62*z-29*z2-9*one)*T6 + (z2-11*z +3*one)*U3)/dlt2 + &
      (z*Gm*(z-five)+(17*z-11*z2-two)*T6 + (2*z2-18.5*z+4.5*one)*U3 + &
      (6*one-7*z-z2)*V16)/dlt + &
      ((7*z + 4*z2)*P16 + z*((z*Gp)/2+Gm*(1.5*z-10*one))-&
      2.5*z*U+(z2-7*z+12*one)*V16-T6*(7*z+one)) +&
      dlt*(-5*z*Gm+(z2*Gp1)/12+z*P16*(2*z+7*one)-T3-U/2+V1)
      redcoeff(5) = redcoeff(5)/(one + dlt)**2

      redcoeff(7) = ((3*z2-19*z+8*one)*T3)/dlt3 + &
      ((6*z2-33*z+11*one)*T6 + (8*z-7*one)*U3)/dlt2 + &
      ((2*z-4*one/3)*T3 + (z-11*one)*V16 + 5*U3*(1.5*z-one)+Gm*(5*z - one))/dlt +&
      ((-7*Gp)/6-z*P16+Gm*(5*z-1.5*one) +&
      ((-5*Gp1 - 2*Gp2)*dlt)/36 + (7*T6)/3 + 2*U3 - 11*V16)
      redcoeff(7) = redcoeff(7)/(one + dlt)

    end if

  end if

end subroutine tch_triangle_exact

! ******************************************************************************
subroutine tch_triangle_expand(r,G_in,msq,masses_ind,dlt,psq,p1,p2,&
& redcoeff)
! ******************************************************************************
! OpenLoops Reduction. Reduction of three point integrals with one
! external massless leg. Reduction formulae expanded in \delta
! ------------------------------------------------------------------------------
! G_in       = input coefficient
! r          = tensor integral rank
! msq        = array of squared mass
! dlt        = \delta parameter
! p1, p2     = external momenta in the triangle
! redcoeff   = (1-6) zero
!              (7) contracted coefficients
! masses_ind = integer for the internal masses configuration
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use trred,only: C_m00_p1,C_m00_P12,C_0mm_p1,C_0mm_P12,               &
                  C_m00_P12P12,C_m00_p1P12,C_m00_p1p1,C_m00_g,         &
                  C_0mm_P12P12,C_0mm_p1P12,C_0mm_p1p1,C_0mm_g,         &
                  C_000_p1,C_000_P12,C_000_g,C_000_p1p1,C_000_p1P12,   &
                  C_000_P12P12,C_mmm_P12,C_mmm_p1,C_mmm_g,C_mmm_p1p1,  &
                  C_mmm_p1P12,C_mmm_P12P12,C_000_P12P12P12,            &
                  C_000_p1P12P12,C_000_p1p1P12,C_000_p1p1p1,C_000_gp1, &
                  C_000_gP12,C_m00_P12P12P12,C_m00_p1P12P12,           &
                  C_m00_p1p1P12,C_m00_p1p1p1,C_m00_gp1,C_m00_gP12,     &
                  C_0mm_P12P12P12,C_0mm_p1P12P12,C_0mm_p1p1P12,        &
                  C_0mm_p1p1p1,C_0mm_gp1,C_0mm_gP12,C_mmm_P12P12P12,   &
                  C_mmm_p1P12P12,C_mmm_p1p1P12,C_mmm_p1p1p1,C_mmm_gp1, &
                  C_mmm_gP12
  use ol_loop_parameters_decl_/**/REALKIND, only: mu2_UV, mu2_IR
  implicit none
  complex(REALKIND), intent(in)  :: G_in(:)
  complex(REALKIND), intent(in)  :: msq(0:2),p1(1:5),p2(1:5)
  integer,           intent(in)  :: r,masses_ind
  real(REALKIND),    intent(in)  :: dlt,psq
  complex(REALKIND), intent(out) :: redcoeff(1:7)

  complex(REALKIND) :: psq_delta,psq_delta2,dlt2,dlt3,dlt4
  complex(REALKIND) :: contr_p1,contr_p2,contr_P12
  complex(REALKIND) :: contr_P12P12,contr_p1p1,contr_p1P12,contr_g,contr_gp1,contr_gP12
  complex(REALKIND) :: contr_P12P12P12,contr_p1p1p1,contr_p1P12P12,contr_p1p1P12
  complex(REALKIND) :: p1p1(6:15),p2p2(6:15),p1p2(6:15),p1P12(6:15),P12P12(6:15),P12_vec(1:4)
  complex(REALKIND) :: p1p1p1(16:35),p1p1P12(16:35),p1P12P12(16:35),P12P12P12(16:35)
  integer :: i,j,n,k

  redcoeff = 0._/**/REALKIND
  psq_delta = psq*dlt
  psq_delta2 = psq_delta*dlt
  dlt2 = dlt**2
  dlt3 = dlt**3
  dlt4 = dlt**4

  !------------------------------------------------------------------------*
  !--------------------- Formulae for the rank-1 part ---------------------*
  !------------------------------------------------------------------------*
  if(r == 1) then
    contr_p1  = SUM(G_in(2:5)*p1(1:4))
    contr_p2  = SUM(G_in(2:5)*p2(1:4))
    contr_P12 = contr_p1 - contr_p2

    !! masses: (0,0,0)
    if (masses_ind == 0) then
      redcoeff(:6) = 0._/**/REALKIND
      redcoeff(7) = contr_P12*C_000_P12(psq,mu2_UV,mu2_IR,dlt) &
                  + contr_p1*C_000_p1(psq,mu2_UV,mu2_IR,dlt)
    !! masses: (m,0,0)
    else if (masses_ind == 1) then
      redcoeff(:6) = 0._/**/REALKIND
      redcoeff(7) = contr_P12*C_m00_P12(psq,msq(0),mu2_UV,mu2_IR,dlt) &
                  + contr_p1*C_m00_p1(psq,msq(0),mu2_UV,mu2_IR,dlt)

    !! masses: (m,m,m)
    else if(masses_ind == 3 ) then
      redcoeff(:6) = 0._/**/REALKIND
      redcoeff(7) = contr_P12*C_mmm_P12(psq,msq(0),mu2_UV,dlt)  &
                  + contr_p1*C_mmm_p1(psq,msq(0),mu2_UV,dlt)

    !! masses: (0,m1,m1)
    else if (masses_ind == 6) then
      redcoeff(:6) = 0._/**/REALKIND
      redcoeff(7) = contr_P12*C_0mm_P12(psq,msq(1),mu2_UV,dlt)  &
                  + contr_p1*C_0mm_p1(psq,msq(1),mu2_UV,dlt)
  end if
  !------------------------------------------------------------------------*
  !--------------------- Formulae for the rank-2 part ---------------------*
  !------------------------------------------------------------------------*
  else if(r == 2) then
    n = 6
    do i = 1, 4
      do j = i, 4
        p1p1(n) = p1(i)*p1(j)
        p2p2(n) = p2(i)*p2(j)
        p1p2(n) = p1(i)*p2(j) + p1(j)*p2(i)
        p1P12(n) = p1(i)*(p1(j)-p2(j)) + p1(j)*(p1(i)-p2(i))
        P12P12(n) = (p1(i)-p2(i))*(p1(j)-p2(j))
        n = n + 1
      end do
    end do

    contr_p1p1 = sum(p1p1*G_in(6:15))
    contr_p1P12 = sum(p1P12*G_in(6:15))
    contr_P12P12 = sum(P12P12*G_in(6:15))
    contr_g = 2*(G_in(7)-G_in(14))

    !! masses: (0,0,0)
    if(masses_ind == 0) then
        redcoeff(:6) = 0._/**/REALKIND
        redcoeff(7) = + contr_P12P12*C_000_P12P12(psq,mu2_UV,mu2_IR,dlt) &
                      + contr_p1p1*C_000_p1p1(psq,mu2_UV,mu2_IR,dlt)     &
                      + contr_p1P12*C_000_p1P12(psq,mu2_UV,mu2_IR,dlt)   &
                      + contr_g*C_000_g(psq,mu2_UV,mu2_IR,dlt)

    !! masses: (m0,0,0)
    else if(masses_ind == 1) then
      redcoeff(:6) = 0._/**/REALKIND
      redcoeff(7) = + contr_P12P12*C_m00_P12P12(psq,msq(0),mu2_UV,mu2_IR,dlt) &
                    + contr_p1p1*C_m00_p1p1(psq,msq(0),mu2_UV,mu2_IR,dlt)     &
                    + contr_p1P12*C_m00_p1P12(psq,msq(0),mu2_UV,mu2_IR,dlt)   &
                    + contr_g*C_m00_g(psq,msq(0),mu2_UV,mu2_IR,dlt)

    !! masses: (m,m,m)
    else if(masses_ind == 3) then
      redcoeff(:6) = 0._/**/REALKIND
      redcoeff(7) = + contr_P12P12*C_mmm_P12P12(psq,msq(0),mu2_UV,dlt) &
                    + contr_p1p1*C_mmm_p1p1(psq,msq(0),mu2_UV,dlt)     &
                    + contr_p1P12*C_mmm_p1P12(psq,msq(0),mu2_UV,dlt)   &
                    + contr_g*C_mmm_g(psq,msq(0),mu2_UV,dlt)

    !! masses: (0,m1,m1)
    else if(masses_ind == 6) then
      redcoeff(:6) = 0._/**/REALKIND
      redcoeff(7) = + contr_P12P12*C_0mm_P12P12(psq,msq(1),mu2_UV,dlt) &
                    + contr_p1p1*C_0mm_p1p1(psq,msq(1),mu2_UV,dlt)     &
                    + contr_p1P12*C_0mm_p1P12(psq,msq(1),mu2_UV,dlt)   &
                    + contr_g*C_0mm_g(psq,msq(1),mu2_UV,dlt)
    end if

  !------------------------------------------------------------------------*
  !--------------------- Formulae for the rank-3 part ---------------------*
  !------------------------------------------------------------------------*
  else if(r == 3) then
    n = 16
    do i = 1, 4
      P12_vec(i) = p1(i) - p2(i)
    end do

    do i = 1, 4
      do j = i, 4
        do k = j, 4
          p1p1p1(n) = p1(i)*p1(j)*p1(k)
          P12P12P12(n) = P12_vec(i)*P12_vec(j)*P12_vec(k)
          p1p1P12(n) = p1(i)*p1(j)*P12_vec(k) + &
                       p1(i)*p1(k)*P12_vec(j) + &
                       p1(k)*p1(j)*P12_vec(i)
          p1P12P12(n) = P12_vec(i)*P12_vec(j)*p1(k) + &
                        P12_vec(i)*P12_vec(k)*p1(j) + &
                        P12_vec(k)*P12_vec(j)*p1(i)
          n = n + 1
        end do
      end do
    end do

    contr_p1p1p1 = sum(p1p1p1(16:35)*G_in(16:35))
    contr_p1p1P12 = sum(p1p1P12(16:35)*G_in(16:35))
    contr_p1P12P12 = sum(p1P12P12(16:35)*G_in(16:35))
    contr_P12P12P12 = sum(P12P12P12(16:35)*G_in(16:35))

    contr_gp1 = 2*((2*G_in(17) - G_in(24))*p1(1) + &
               (2*G_in(20) - G_in(30))*p1(2) +     &
               (G_in(21) - 2*G_in(33))*p1(3) +     &
               (G_in(22) - 2*G_in(34))*p1(4))
    contr_gP12 = 2*((2*G_in(17) - G_in(24))*P12_vec(1) + &
                (2*G_in(20) - G_in(30))*P12_vec(2) +     &
                (G_in(21) - 2*G_in(33))*P12_vec(3) +     &
                (G_in(22) - 2*G_in(34))*P12_vec(4))

    !! masses: (0,0,0)
    if(masses_ind == 0) then
      redcoeff(:6) = 0._/**/REALKIND
      redcoeff(7) = + contr_P12P12P12*C_000_P12P12P12(psq,mu2_UV,mu2_IR,dlt) &
                    + contr_p1P12P12*C_000_p1P12P12(psq,mu2_UV,mu2_IR,dlt)   &
                    + contr_p1p1P12*C_000_p1p1P12(psq,mu2_UV,mu2_IR,dlt)     &
                    + contr_p1p1p1*C_000_p1p1p1(psq,mu2_UV,mu2_IR,dlt)       &
                    + contr_gp1*C_000_gp1(psq,mu2_UV,mu2_IR,dlt)             &
                    + contr_gP12*C_000_gP12(psq,mu2_UV,mu2_IR,dlt)

    !! masses: (m0,0,0)
    else if(masses_ind == 1) then
      redcoeff(:6) = 0._/**/REALKIND
      redcoeff(7) = + contr_P12P12P12*C_m00_P12P12P12(psq,msq(0),mu2_UV,mu2_IR,dlt) &
                    + contr_p1P12P12*C_m00_p1P12P12(psq,msq(0),mu2_UV,mu2_IR,dlt)   &
                    + contr_p1p1P12*C_m00_p1p1P12(psq,msq(0),mu2_UV,mu2_IR,dlt)     &
                    + contr_p1p1p1*C_m00_p1p1p1(psq,msq(0),mu2_UV,mu2_IR,dlt)       &
                    + contr_gp1*C_m00_gp1(psq,msq(0),mu2_UV,mu2_IR,dlt)             &
                    + contr_gP12*C_m00_gP12(psq,msq(0),mu2_UV,mu2_IR,dlt)

    !! masses: (m,m,m)
    else if(masses_ind == 3) then
      redcoeff(:6) = 0._/**/REALKIND
      redcoeff(7) = + contr_P12P12P12*C_mmm_P12P12P12(psq,msq(0),mu2_UV,dlt) &
                    + contr_p1P12P12*C_mmm_p1P12P12(psq,msq(0),mu2_UV,dlt)   &
                    + contr_p1p1P12*C_mmm_p1p1P12(psq,msq(0),mu2_UV,dlt)     &
                    + contr_p1p1p1*C_mmm_p1p1p1(psq,msq(0),mu2_UV,dlt)       &
                    + contr_gp1*C_mmm_gp1(psq,msq(0),mu2_UV,dlt)             &
                    + contr_gP12*C_mmm_gP12(psq,msq(0),mu2_UV,dlt)
    ! masses: (0,m1,m1)
    else if(masses_ind == 6) then
      redcoeff(:6) = 0._/**/REALKIND
      redcoeff(7) = + contr_P12P12P12*C_0mm_P12P12P12(psq,msq(1),mu2_UV,dlt) &
                    + contr_p1P12P12*C_0mm_p1P12P12(psq,msq(1),mu2_UV,dlt)   &
                    + contr_p1p1P12*C_0mm_p1p1P12(psq,msq(1),mu2_UV,dlt)     &
                    + contr_p1p1p1*C_0mm_p1p1p1(psq,msq(1),mu2_UV,dlt)       &
                    + contr_gp1*C_0mm_gp1(psq,msq(1),mu2_UV,dlt)             &
                    + contr_gP12*C_0mm_gP12(psq,msq(1),mu2_UV,dlt)
    end if

  end if

end subroutine tch_triangle_expand

! ********************************************************************************
subroutine t_channel_triangle_reduction(perm,sdlt,mom,msq,Gin,Gout_A,Gout_A0,Gout_A1,&
Gout_A2, Gout_R1,A0msq,A0_0,A0_1,A0_2)
! --------------------------------------------------------------------------------
! Reduction of a t-channel triangle topology with one external massless leg
! --------------------------------------------------------------------------------
! perm: permutation. It gives the permutation order for the pinched subtopologies
! Gin: input openloops coefficient. It can be rank-1,2,3
! mom: external momenta of the triangle
! msq: array of squared internal masses
! Gout_A : coefficient of the scalar triangle
! Gout_Ai: coefficient of the scalar bubble, Di-pinch, i=0,1,2
! Gout_R1: rational terms
! ********************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_loop_handling_/**/REALKIND, only: G_TensorShift
  implicit none
  integer, intent(in) :: perm(3)
  real(REALKIND), intent(in) :: sdlt
  complex(REALKIND), intent(inout)  :: Gin(:)
  complex(REALKIND), intent(in) :: msq(0:2), mom(5,3)
  complex(REALKIND), intent(out) :: Gout_A(1), Gout_A0(1), Gout_A1(1), Gout_A2(1)
  complex(REALKIND), intent(out) :: Gout_R1
  complex(REALKIND), optional, intent(in) :: A0msq(:)
  complex(REALKIND), optional, intent(out) :: A0_0(1), A0_1(1), A0_2(1)
  complex(REALKIND) :: k1(5), k2(5), k12(5), masses(0:2), tad_aux(1)

  k1  = mom(:,1)
  k2  = mom(:,2)
  k12 = mom(:,3)

  if(present(A0_0)) A0_0 = 0._/**/REALKIND
  if(present(A0_1)) A0_1 = 0._/**/REALKIND
  if(present(A0_2)) A0_2 = 0._/**/REALKIND

  !! If needed a loop momentum shift is performed in order to set the D0 propagator in
  !! the opposite position to the external massless leg.

  if(perm(1) == 2) then

    call G_TensorShift(Gin,k12(1:4))

    if(perm(2) == 0) then
      masses = (/msq(2),msq(0),msq(1)/)
      call triangle_zero_leg(k12,k2,sdlt,masses,Gin,Gout_A,Gout_A2,Gout_A0,Gout_A1,&
                             Gout_R1,tad_aux)

    else
      masses = (/msq(2),msq(1),msq(0)/)
      call triangle_zero_leg(k2,k12,sdlt,masses,Gin,Gout_A,Gout_A2,Gout_A1,Gout_A0,&
                             Gout_R1,tad_aux)

    end if

  else if(perm(1) == 0) then

    if(perm(2) == 2) then
      masses = (/msq(0),msq(2),msq(1)/)
      call triangle_zero_leg(k12,k1,sdlt,masses,Gin,Gout_A,Gout_A0,Gout_A2,Gout_A1,&
                             Gout_R1,tad_aux)

    else
      masses = (/msq(0),msq(1),msq(2)/)
      call triangle_zero_leg(k1,k12,sdlt,masses,Gin,Gout_A,Gout_A0,Gout_A1,Gout_A2,&
                             Gout_R1,tad_aux)

    end if

  else if(perm(1) == 1) then

    call G_TensorShift(Gin,k1(1:4))

    if(perm(2) == 2) then
      masses = (/msq(1),msq(2),msq(0)/)
      call triangle_zero_leg(k2,k1,sdlt,masses,Gin,Gout_A,Gout_A1,Gout_A2,Gout_A0,&
                             Gout_R1,tad_aux)

    else
      masses = (/msq(1),msq(0),msq(2)/)
      call triangle_zero_leg(k1,k2,sdlt,masses,Gin,Gout_A,Gout_A1,Gout_A0,Gout_A2,&
                             Gout_R1,tad_aux)

    end if
  end if

  if (present(A0msq)) then
    if(size(A0msq)==2) then
      if(A0msq(2) == 0._/**/REALKIND) A0_0 = A0_0 + tad_aux
      if(A0msq(1) == 0._/**/REALKIND) A0_1 = A0_1 + tad_aux
      if(A0msq(1) /= 0._/**/REALKIND .AND. A0msq(2)/=0._/**/REALKIND) then
        write(*,*) 'Tadpoles with 2 masses non-zero'
      end if
    else if(size(A0msq)==1) then
      A0_0 = A0_0 + tad_aux
    end if
  end if

end subroutine t_channel_triangle_reduction


!******************************************************************************
subroutine triangle_zero_leg(p1,p2,dlt,msq,Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_R1,A0_0)
!******************************************************************************
! Reduction of 3-point integrals with one massless external leg.
! ------------------------------------------------------------------------------
! p1, p2  = external momenta of the 3-point function
! dlt     = \delta parameter
! msq     = array of internal masses squared. (m0^2, m1^2, m2^2)
! Gin_A   = input  OpenLoops coefficient of the rank-2   3-point function
! Gout_A  = output OpenLoops coefficient of the scalar 3-point function
! Gout_Ai = output OpenLoops coefficient of the scalar bubble. i-th pinch
! Gout_R1 = rational part
! A0_0    = coefficient of the tadpole. For the moment we allow only one for QCD
! ------------------------------------------------------------------------------
  use KIND_TYPES, only: REALKIND
  use ol_debug, only: ol_error
 use ol_loop_parameters_decl_/**/DREALKIND, only: polecheck_is
  implicit none
  complex(REALKIND), intent(in)  :: p1(1:5), p2(1:5), msq(0:2)
  real(REALKIND),    intent(in)  :: dlt
  complex(REALKIND), intent(in)  :: Gin_A(:)
  complex(REALKIND), intent(out) :: Gout_A(1), Gout_A0(1), Gout_A1(1), Gout_A2(1)
  complex(REALKIND), intent(out) :: Gout_R1
  complex(REALKIND), optional, intent(out) :: A0_0(1) !, A0_1, A0_2

  real(REALKIND) :: psq
  integer :: m_ind
  logical :: dlt_exp
  complex(REALKIND) :: redcoeff1(1:7), redcoeff2(1:7), redcoeff3(1:7), zero

  zero = 0._/**/REALKIND
  Gout_A  = zero
  Gout_A0 = zero
  Gout_A1 = zero
  Gout_A2 = zero

  if(present(A0_0)) A0_0 = zero

  redcoeff1 = zero
  redcoeff2 = zero
  redcoeff3 = zero

  if(REAL(p1(5)) < 0) then
    psq = ABS(p1(5))
  else
    psq = - ABS(p1(5))
  end if

  if (msq(1)==msq(2) .AND. msq(1)==msq(0) .AND. msq(0)==zero) then
    !! (0,0,0) masses configuration
    m_ind = 0
  else if (msq(1)==msq(2) .AND. msq(1)==msq(0)) then
    !! (m,m,m) masses configuration
    m_ind = 3
  else if (msq(1)==msq(2) .AND. msq(0)==zero) then
    !! (0,m,m) masses configuration
    m_ind = 6
  else if (msq(1)==msq(2) .AND. msq(1)==zero) then
    !! (m,0,0) masses configuration
    m_ind = 1
  end if

  ! local switcher to activate the expansions in the Gram-Determinant
  dlt_exp = (dlt < delta_thres) .AND. DeltaExp .AND. (REAL(p1(5)) < 0)

  ! TEMPORARY: expansions are switched off in case polecheck = 1
  dlt_exp = dlt_exp .and. (polecheck_is == 0)

  if(.NOT. dlt_exp) then
    !! Exact reduction formulae
    if(size(Gin_A) == 5) then
      call tch_triangle_exact( 1,Gin_A(1:5) ,msq,m_ind,dlt,psq, p1,p2,redcoeff1)

    else if(size(Gin_A) == 15) then
      call tch_triangle_exact( 2,Gin_A(1:15),msq,m_ind,dlt,psq, p1,p2,redcoeff2)
      call tch_triangle_exact( 1,Gin_A(1:5) ,msq,m_ind,dlt,psq, p1,p2,redcoeff1)

    else if(size(Gin_A) == 35) then
      call tch_triangle_exact( 3,Gin_A(1:35),msq,m_ind,dlt,psq, p1,p2,redcoeff3)
      call tch_triangle_exact( 2,Gin_A(1:15),msq,m_ind,dlt,psq, p1,p2,redcoeff2)
      call tch_triangle_exact( 1,Gin_A(1:5) ,msq,m_ind,dlt,psq, p1,p2,redcoeff1)
    end if

  else
    !! Expansions, computed via trred library
    if(size(Gin_A) == 5) then
      call tch_triangle_expand( 1,Gin_A(1:5) ,msq,m_ind,dlt,psq, p1,p2,redcoeff1)

    else if(size(Gin_A) == 15) then
      call tch_triangle_expand( 2,Gin_A(1:15),msq,m_ind,dlt,psq, p1,p2,redcoeff2)
      call tch_triangle_expand( 1,Gin_A(1:5) ,msq,m_ind,dlt,psq, p1,p2,redcoeff1)

    else if(size(Gin_A) == 35) then
      call tch_triangle_expand( 3,Gin_A(1:35),msq,m_ind,dlt,psq, p1,p2,redcoeff3)
      call tch_triangle_expand( 2,Gin_A(1:15),msq,m_ind,dlt,psq, p1,p2,redcoeff2)
      call tch_triangle_expand( 1,Gin_A(1:5) ,msq,m_ind,dlt,psq, p1,p2,redcoeff1)
    end if
  end if

  Gout_A(1)  = Gin_A(1)     + redcoeff1(1) + redcoeff2(1) + redcoeff3(1)
  Gout_A0(1) = Gout_A0(1)   + redcoeff1(2) + redcoeff2(2) + redcoeff3(2)
  Gout_A1(1) = Gout_A1(1)   + redcoeff1(3) + redcoeff2(3) + redcoeff3(3)
  Gout_A2(1) = Gout_A2(1)   + redcoeff1(4) + redcoeff2(4) + redcoeff3(4)
  Gout_R1    = redcoeff1(7) + redcoeff2(7) + redcoeff3(7)

  !! Tadpole contribution
  if(present(A0_0)) A0_0 = (redcoeff1(5) + redcoeff2(5) + redcoeff3(5))

end subroutine triangle_zero_leg


!************************************************************************************
subroutine scalar_MIs(momenta, masses2, Gsum, M2add)
!************************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_parameters_decl_/**/DREALKIND, only: a_switch
  implicit none
  integer,           intent(in)    :: momenta(:)
  complex(REALKIND), intent(in)    :: masses2(:), Gsum(:)
  complex(REALKIND), intent(out)   :: M2add

#ifdef PRECISION_dp
  if(a_switch==1 .or. a_switch==7) then
    call collier_scalars_interface(momenta, masses2, Gsum, M2add)

  else if(a_switch==5) then
    call avh_olo_interface(momenta, masses2, Gsum, M2add)

!  else if(sc_switch = 5) then
!    call QCDLOOPSCALARS
  end if
#else
  call avh_olo_interface(momenta, masses2, Gsum, M2add)
#endif
end subroutine scalar_MIs

#ifdef PRECISION_dp
!************************************************************************************
subroutine collier_scalars_interface(momenta, masses2, Gsum, M2add)
!************************************************************************************
  use KIND_TYPES, only: REALKIND, DREALKIND
  use ol_momenta_decl_/**/REALKIND, only: L
  use ol_kinematics_/**/REALKIND, only: LC2Std_Rep_cmplx, collier_invariants
#ifdef USE_COLLIER
  use collier, only: tnten_cll
#endif
  implicit none
  integer,           intent(in)    :: momenta(:)
  complex(REALKIND), intent(in)    :: masses2(:), Gsum(:)
  complex(REALKIND), intent(out)   :: M2add
  complex(REALKIND) :: TI(size(Gsum)), p(0:3,1:size(momenta)-1)
  complex(REALKIND) :: momenta_TI(0:3,size(momenta)-1)
  complex(REALKIND) :: sc_int(1), UV_part(1)
  integer :: i, k, int_mom(1:size(momenta)-1)
#ifdef USE_COLLIER
  i = momenta(1)
  int_mom(1) = i
  p(0:3,1) = L(1:4,i)
  do k = 2, size(momenta) - 1
    i = i + momenta(k)
    int_mom(k) = i
    p(0:3,k) = L(1:4,i)
  end do

  do k = 1, size(p,2)
    call LC2Std_Rep_cmplx(p(:,k), momenta_TI(:,k))
  end do

  call tnten_cll(sc_int, UV_part, momenta_TI, collier_invariants(int_mom), masses2, size(masses2), 0)
  M2add = Gsum(1)*sc_int(1)
#else
  call ol_error("COLLIER library not compiled")
  M2add = 0
#endif
end subroutine collier_scalars_interface
#endif

!************************************************************************************
subroutine avh_olo_interface(momenta, masses2, Gsum, M2add)
!************************************************************************************
  use KIND_TYPES, only: REALKIND, DREALKIND
  use ol_momenta_decl_/**/REALKIND, only: L
  use avh_olo, only: olo_scale_prec, olo
  use ol_loop_parameters_decl_/**/REALKIND, only: mureg
  use ol_loop_parameters_decl_/**/DREALKIND, only: de1_IR, de2_i_IR
  implicit none
  complex(REALKIND), intent(in)  :: masses2(:), Gsum(:)
  integer, intent(in) :: momenta(:)
  complex(REALKIND), intent(out) :: M2add
  logical           :: tadpole_check
  complex(REALKIND) :: tadpole_mass, p1_2, p2_2, p3_2, p4_2, p12_2, p23_2
  complex(REALKIND) :: rslt(0:2), zero = 0._/**/REALKIND
  integer :: i, j, k

  tadpole_check = .FALSE.
  call olo_scale_prec(mureg)

  ! Check if the bubble given in input is a representation of a Tadpole.
  ! A Tadpole can be represented through a bubble with zero momentum:
  ! A(m) = m^2 B(p=0,m0=0,m1=m)
  if(size(momenta)==2) then
    tadpole_check = (momenta(1) == 0) .AND. (momenta(2) == 0)
  end if

  ! -- TADPOLES
  if(tadpole_check) then
    if(masses2(1)/=zero .AND. masses2(2)==zero) then
      tadpole_mass = masses2(1)
    else if(masses2(1)==zero .AND. masses2(2)/=zero) then
      tadpole_mass = masses2(2)
    else if(masses2(1)==zero .AND. masses2(2)==zero) then
      ! Tadpole with zero mass. An exact 0 is returned
      M2add = zero
      return
    end if

    call olo(rslt, tadpole_mass)
    M2add = (Gsum(1)/tadpole_mass)*(rslt(0) + rslt(1)*de1_IR + rslt(2)*de2_i_IR)
    return
  end if

  ! -- BUBBLES
  if(size(masses2) == 2) then
    i = momenta(1)
    p1_2 = L(5,i) + L(6,i)
    call olo(rslt,p1_2,masses2(1),masses2(2))

  ! -- TRIANGLES
  else if(size(masses2) == 3) then

    i = momenta(1)
    j = momenta(2)
    p1_2 = L(5,i)   + L(6,i)
    p2_2 = L(5,j)   + L(6,j)
    p3_2 = L(5,i+j) + L(6,i+j)
    call olo(rslt,p1_2,p2_2,p3_2,masses2(1),masses2(2),masses2(3))

  ! -- BOXES
  else if(size(masses2) == 4) then
    i = momenta(1)
    j = momenta(2)
    k = momenta(3)
    p1_2  = L(5,i)     + L(6,i)
    p2_2  = L(5,j)     + L(6,j)
    p3_2  = L(5,k)     + L(6,k)
    p4_2  = L(5,i+j+k) + L(6,i+j+k)
    p12_2 = L(5,i+j)   + L(6,i+j)
    p23_2 = L(5,j+k)   + L(6,j+k)
    call olo(rslt, p1_2,p2_2,p3_2,p4_2,p12_2,p23_2,&
      masses2(1),masses2(2),masses2(3),masses2(4))
  else
    call ol_error('avh_olo_interface: integration called for a non-MI')
  end if

  M2add = Gsum(1)*(rslt(0) + rslt(1)*de1_IR + rslt(2)*de2_i_IR)

end subroutine avh_olo_interface


#ifdef PRECISION_dp
!************************************************************************************
subroutine collier_scalar_box(momenta, masses2, rslt)
!************************************************************************************
  use KIND_TYPES, only: REALKIND, DREALKIND
  use ol_momenta_decl_/**/REALKIND, only: L
  use ol_kinematics_/**/REALKIND, only: LC2Std_Rep_cmplx, collier_invariants
#ifdef USE_COLLIER
  use collier, only: tnten_cll
#endif
  implicit none
  integer,           intent(in)    :: momenta(3)
  complex(REALKIND), intent(in)    :: masses2(4)
  complex(REALKIND), intent(out) :: rslt(0:2)
  complex(REALKIND) :: p(0:3,3), momenta_TI(0:3,3), sc_box(1), UV_part(1)
  integer :: k

  rslt = 0._/**/REALKIND
#ifdef USE_COLLIER
  p(0:3,1) = L(1:4,momenta(1))
  p(0:3,2) = L(1:4,momenta(2))
  p(0:3,3) = L(1:4,momenta(3))

  do k = 1, 3
    call LC2Std_Rep_cmplx(p(:,k), momenta_TI(:,k))
  end do

  call tnten_cll(sc_box, UV_part, momenta_TI, collier_invariants(momenta), masses2, 4, 0)
  rslt(0) = sc_box(1)
#else
  call ol_error("COLLIER library not compiled")
#endif
end subroutine collier_scalar_box
#endif


!************************************************************************************
subroutine avh_olo_box(momenta, masses2, rslt)
!************************************************************************************
  use KIND_TYPES, only: REALKIND, DREALKIND
  use ol_momenta_decl_/**/REALKIND, only: L
  use avh_olo, only: olo_scale_prec, olo
  use ol_loop_parameters_decl_/**/REALKIND, only: mureg
  implicit none
  complex(REALKIND), intent(in)  :: masses2(4)
  integer,           intent(in)  :: momenta(3)
  complex(REALKIND), intent(out) :: rslt(0:2)
  complex(REALKIND) :: p1_2, p2_2, p3_2, p4_2, p12_2, p23_2
  integer :: i, j, k

  call olo_scale_prec(mureg)

  ! -- BOXES
  i = momenta(1)
  j = momenta(2)
  k = momenta(3)
  p1_2  = L(5,i)     + L(6,i)
  p2_2  = L(5,j)     + L(6,j)
  p3_2  = L(5,k)     + L(6,k)
  p4_2  = L(5,i+j+k) + L(6,i+j+k)
  p12_2 = L(5,i+j)   + L(6,i+j)
  p23_2 = L(5,j+k)   + L(6,j+k)
  call olo(rslt, p1_2,p2_2,p3_2,p4_2,p12_2,p23_2,&
  masses2(1),masses2(2),masses2(3),masses2(4))

end subroutine avh_olo_box


!******************************************************************************
subroutine compute_scalar_box(mom_ind, masses2, RedSet, box)
!******************************************************************************
! Evaluation of a scalar box needed for the OPP reduction
! ------------------------------------------------------------------------------
! mom_ind = indices of internal momenta
! masses2 = array of internal masses squared. (m0^2, m1^2, m2^2, m3^2)
! RedSet  = reduction set used to find the cut solution
! box     = scalar box data type. It contains the value of the box and the cut
!           solutions
! ------------------------------------------------------------------------------
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: L
  use ol_data_types_/**/REALKIND, only: redset4, scalarbox
  use ol_parameters_decl_/**/DREALKIND, only: a_switch
  implicit none
  integer, intent(in) :: mom_ind(3)
  complex(REALKIND), intent(in) :: masses2(0:3)
  type(redset4), intent(in)  :: RedSet
  type(scalarbox), intent(out) :: box
  complex(REALKIND) :: p(1:5,3), q0_p(5), q0_m(5), q0_cuts(2,5), sc_box(0:2)
  integer :: mom_box(3)

  p(1:4,1) = L(1:4,mom_ind(1))
  p(5,1)   = L(5,mom_ind(1)) + L(6,mom_ind(1))
  p(1:4,2) = L(1:4,mom_ind(2))
  p(5,2)   = L(5,mom_ind(2)) + L(6,mom_ind(2))
  p(1:4,3) = L(1:4,mom_ind(3))
  p(5,3)   = L(5,mom_ind(3)) + L(6,mom_ind(3))

  call box_onshell_cut(p, masses2, RedSet, q0_p, q0_m)

  q0_cuts(1,:) = q0_p(:)
  q0_cuts(2,:) = q0_m(:)

#ifdef PRECISION_dp
  if (a_switch == 5) then
  !!! OneLoop used for the scalar boxes
    mom_box(1) = mom_ind(1)
    mom_box(2) = mom_ind(2)-mom_ind(1)
    mom_box(3) = mom_ind(3)-mom_ind(2)
    call avh_olo_box(mom_box, masses2, sc_box)

  else if (a_switch == 1 .or. a_switch == 7) then
  !!! Collier/DD call for scalar boxes
    call collier_scalar_box(mom_ind, masses2, sc_box)
  end if
#else
  !!! OneLoop used for the scalar boxes
  mom_box(1) = mom_ind(1)
  mom_box(2) = mom_ind(2)-mom_ind(1)
  mom_box(3) = mom_ind(3)-mom_ind(2)
  call avh_olo_box(mom_box, masses2, sc_box)
#endif
  box = scalarbox(sc_box,q0_cuts)

end subroutine compute_scalar_box


! ********************************************************************
subroutine box_onshell_cut(p, m2, RedSet, q0_p, q0_m)
!---------------------------------------------------------------------
! Calculation of q0+, q0- for the quadruple cut
! ********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_kinematics_/**/REALKIND, only: cont_LC_cntrv
  use ol_data_types_/**/REALKIND, only: basis, redset4
  implicit none
  complex(REALKIND), intent(in)  :: p(1:5,1:3), m2(0:3)
  complex(REALKIND), intent(out) :: q0_p(1:5), q0_m(1:5)
  type(redset4), intent(in) :: RedSet

  complex(REALKIND) :: l(1:4,4), k1(1:5), k2(1:5), k3(1:5)
  complex(REALKIND) :: gamma, a1, a2
  complex(REALKIND) :: x1, x2, x3p, x3m, x4p, x4m, k3scalars(1:4)
  complex(REALKIND) :: dd0, dd1, dd2, dd3
  complex(REALKIND) :: c0, b0, A, B, R
  integer :: i, perm(3)
  type(basis) :: bas

  bas = RedSet%redbasis
  perm = RedSet%perm

  l = bas%li
  a1 = bas%alpha(1)
  a2 = bas%alpha(2)

  if(perm(1)+perm(2)==3) then ! [1,2,3]
    do i = 1, 4
      k3scalars(i) = cont_LC_cntrv(p(1:4,3),l(:,i))
    end do
    dd0 = m2(0)
    dd1 = m2(1) - p(5,1)
    dd2 = m2(2) - p(5,2)
    dd3 = m2(3) - p(5,3)
  else if(perm(1)+perm(2)==4) then ! [1,3,2]
    do i = 1, 4
      k3scalars(i) = cont_LC_cntrv(p(1:4,2),l(:,i))
    end do
    dd0 = m2(0)
    dd1 = m2(1) - p(5,1)
    dd2 = m2(3) - p(5,3)
    dd3 = m2(2) - p(5,2)
  else if(perm(1)+perm(2)==5) then ! [2,3,1]
    do i = 1, 4
      k3scalars(i) = cont_LC_cntrv(p(1:4,1),l(:,i))
    end do
    dd0 = m2(0)
    dd1 = m2(2) - p(5,2)
    dd2 = m2(3) - p(5,3)
    dd3 = m2(1) - p(5,1)
  end if

  x1 = (dd2 - dd0*(1._/**/REALKIND-a2) - dd1*a2)/bas%gamma
  x2 = (dd1 - dd0*(1._/**/REALKIND-a1) - dd2*a1)/bas%gamma

  A = k3scalars(3)/k3scalars(4)

  B = (dd0-dd3)/2 + k3scalars(1)*x1 + k3scalars(2)*x2
  B = B/k3scalars(4)

  R = (x1*x2 - dd0/bas%gamma)
  c0 = SQRT(B**2 - A*R)

  x4m = (- B + c0)/2
  x4p = (- B - c0)/2

  x3m = x4p/A
  x3p = x4m/A

  ! The assumption is that the p_0 momentum is zero
  q0_p(1:4) = x1*l(1:4,1) + x2*l(1:4,2)
  q0_m(1:4) = q0_p(1:4)
  q0_p(1:4) = q0_p(1:4) + x3p*l(1:4,3) + x4p*l(1:4,4)
  q0_m(1:4) = q0_m(1:4) + x3m*l(1:4,3) + x4m*l(1:4,4)
  q0_p(5) = m2(0)
  q0_m(5) = m2(0)

end subroutine box_onshell_cut

! --- Calculation of the coefficients of the OPP reduction ---
! ********************************************************************
subroutine opp_numerator(Gtensor,q0,N0)
! ********************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), intent(in)  :: Gtensor(:), q0(4)
  complex(REALKIND), intent(out) :: N0

  if(size(Gtensor)==1) then
    N0 = Gtensor(1)
  else if(size(Gtensor)==5) then
    N0 = Gtensor(1) + SUM(Gtensor(2:5)*q0(1:4))
  else
    call ol_error("opp_numerator: rank > 1 ")
  end if

end subroutine opp_numerator


! ********************************************************************
subroutine box_coefficient(p_offshell, m_offshell, q0_pm, Gtensor, box_coeff)
! ********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_kinematics_/**/REALKIND, only: cont_LC_cntrv
  implicit none
  complex(REALKIND), intent(in)  :: Gtensor(:)
  complex(REALKIND), intent(in)  :: p_offshell(:,:), m_offshell(:), q0_pm(2,5)
  complex(REALKIND), intent(out) :: box_coeff
  complex(REALKIND) :: q0_p(1:4), q0_m(1:4)
  complex(REALKIND) :: N0p, N0m, Gsum(size(Gtensor))
  complex(REALKIND) :: mom(1:4), prop_m, prop_p, Di_p, Di_m
  integer :: i

  !q0+, q0- for the quadruple cut
  q0_p(1:4) = q0_pm(1,1:4)
  q0_m(1:4) = q0_pm(2,1:4)

  ! Calculation of the numerator at the multiple cut
  call opp_numerator(Gtensor,q0_p,N0p)
  call opp_numerator(Gtensor,q0_m,N0m)

  Di_p = 1._/**/REALKIND
  Di_m = 1._/**/REALKIND

  do i = 1, size(m_offshell)
    prop_p = p_offshell(5,i) + q0_pm(1,5) + &
             2*cont_LC_cntrv(p_offshell(1:4,i),q0_p(1:4))
    prop_p = prop_p - m_offshell(i)

    prop_m = p_offshell(5,i) + q0_pm(2,5) + &
             2*cont_LC_cntrv(p_offshell(1:4,i),q0_m(1:4))
    prop_m = prop_m - m_offshell(i)

    Di_p = Di_p*prop_p
    Di_m = Di_m*prop_m
  end do

  box_coeff = (N0p/Di_p + N0m/Di_m)/2

end subroutine box_coefficient


!*************************************************************************************
subroutine TI_reduction_1(rank, momenta, masses2, Gtensor, M2add, scboxes, all_scboxes)
!-------------------------------------------------------------------------------------
! Tensor integral manager. Momenta passed as 4-dimensional vectors in light-cone
!*************************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: scalarbox
  implicit none
  integer,           intent(in)  :: rank
  complex(REALKIND), intent(in)  :: momenta(:,:), masses2(:), Gtensor(:)
  complex(REALKIND),    intent(out) :: M2add
  integer,         intent(in), optional :: scboxes(:)
  type(scalarbox), intent(in), optional :: all_scboxes(:)

  if(size(masses2) == 7) then
    call reduction_7points(rank, momenta, masses2, Gtensor, M2add, scboxes, all_scboxes)
  else if(size(masses2) == 6) then
    call reduction_6points(rank, momenta, masses2, Gtensor, M2add, scboxes, all_scboxes)
  else if(size(masses2) == 5) then
    call reduction_5points(rank, momenta, masses2, Gtensor, M2add, scboxes, all_scboxes)
  end if

end subroutine TI_reduction_1


! --- PENTAGONS ---
! ********************************************************************
subroutine reduction_5points(rank, momenta, masses2, Gtensor, M2add, scboxes, all_scboxes)
! ********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_loop_handling_/**/REALKIND, only: G_TensorShift
  use ol_data_types_/**/REALKIND, only: scalarbox
  use ol_loop_parameters_decl_/**/DREALKIND, only: de1_IR, de2_i_IR
  implicit none
  integer, intent(in) :: rank
  complex(REALKIND), intent(in)  :: momenta(:,:), masses2(0:4), Gtensor(:)
  complex(REALKIND), intent(out) :: M2add
  integer,         intent(in) :: scboxes(:)
  type(scalarbox), intent(in) :: all_scboxes(:)

  complex(REALKIND) :: box_p(1:4,0:3,1:5), box_m2(0:3,5)
  complex(REALKIND) :: offshell_p(1:5,1,5), offshell_m2(1,5)
  complex(REALKIND) :: Gsum(size(Gtensor))

  complex(REALKIND) :: box_coeff(5), box(5), scalar_box(0:2), q0p_q0m(2,5)
  integer :: i, j, k

  if(rank > 1) call ol_error("TI_reduction: reduction of a rank > 1 pentagon")

  M2add = 0._/**/REALKIND
  scalar_box = 0._/**/REALKIND

  ! Assignment of the off-shell momenta for all the sub-boxes
  offshell_p(1:4,1,1:4) = momenta(1:4,1:4)
  offshell_p(5,1,1:4)   = momenta(5,1:4)
  offshell_m2(1,1:4)    = masses2(1:4)

  offshell_p(1:4,1,5) = - momenta(1:4,1)
  offshell_p(5,1,5)   = momenta(5,1)
  offshell_m2(1,5)    = masses2(0)

  do i = 1, 5

    q0p_q0m = all_scboxes(scboxes(i))%onshell_cuts
    scalar_box = all_scboxes(scboxes(i))%poles
    Gsum = Gtensor

    if(i == 5) call G_TensorShift(Gsum,-momenta(1:4,1))

    call box_coefficient(offshell_p(:,:,i),&
    offshell_m2(:,i),q0p_q0m,Gsum,box_coeff(i))

    box(i) = box_coeff(i)*(scalar_box(0) + scalar_box(1)*de1_IR + scalar_box(2)*de2_i_IR)
    M2add = M2add + box(i)
  end do

end subroutine reduction_5points


! --- HEXAGONS ---
! ********************************************************************
subroutine reduction_6points(rank, momenta, masses2, Gtensor, M2add, &
 scboxes, all_scboxes)
! ********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_kinematics_/**/REALKIND, only: cont_LC_cntrv
  use ol_loop_handling_/**/REALKIND, only: G_TensorShift
  use ol_data_types_/**/REALKIND, only: scalarbox
  use ol_loop_parameters_decl_/**/DREALKIND, only: de1_IR, de2_i_IR
  implicit none
  integer,           intent(in)  :: rank
  complex(REALKIND), intent(in)  :: momenta(:,:), masses2(0:5), Gtensor(:)
  complex(REALKIND), intent(out) :: M2add
  integer,         intent(in) :: scboxes(:)
  type(scalarbox), intent(in) :: all_scboxes(:)

  complex(REALKIND) :: offshell_p(1:5,2), offshell_m2(2)

  complex(REALKIND) :: box_coeff(size(scboxes)), box(size(scboxes)), scalar_box(0:2)
  complex(REALKIND) :: q0p_q0m(2,5), momshift(5), Gsum(size(Gtensor))
  integer :: i, j, k

  if(rank > 1) call ol_error("TI_reduction: reduction of a rank > 1 hexagon")

  M2add = 0._/**/REALKIND

  k = 0
  do i = 1, 5
    do j = i, 5
      k = k + 1

      q0p_q0m = all_scboxes(scboxes(k))%onshell_cuts
      scalar_box = all_scboxes(scboxes(k))%poles
      Gsum = Gtensor

      if(j == 5) then
        if (i == 1) then
          momshift(1:5) = momenta(1:5,2)
        else
          momshift(1:5) = momenta(1:5,1)
        end if

        ! Assignment of the off-shell momenta and masses for all the sub-boxes
        ! in the case when D0 is pinched
        offshell_p(1:4,1) = - momshift(1:4)
        offshell_p(5,1)   = momshift(5)
        offshell_p(1:4,2) = momenta(1:4,i) - momshift(1:4)
        offshell_p(5,2)   = momenta(5,i) + momshift(5) - &
                            2*cont_LC_cntrv(momenta(1:4,i),momshift(1:4))

        offshell_m2(1) = masses2(0)
        offshell_m2(2) = masses2(i)

        call G_TensorShift(Gsum,-momshift(1:4))
      else

        ! Assignment of the off-shell momenta and masses for all the sub-boxes
        offshell_p(1:5,1) = momenta(1:5,i)
        offshell_p(1:5,2) = momenta(1:5,j+1)

        offshell_m2(1) = masses2(i)
        offshell_m2(2) = masses2(j+1)
      end if

      call box_coefficient(offshell_p(:,:),&
      offshell_m2(:),q0p_q0m,Gsum,box_coeff(k))

      box(k) = box_coeff(k)*(scalar_box(0) + scalar_box(1)*de1_IR + scalar_box(2)*de2_i_IR)
      M2add = M2add + box(k)
    end do
  end do

end subroutine reduction_6points


! --- HEPTAGONS ---
! ********************************************************************
subroutine reduction_7points(rank, momenta, masses2, Gtensor, M2add, &
scboxes, all_scboxes)
! ********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_kinematics_/**/REALKIND, only: cont_LC_cntrv
  use ol_loop_handling_/**/REALKIND, only: G_TensorShift
  use ol_data_types_/**/REALKIND, only: scalarbox
  use ol_loop_parameters_decl_/**/DREALKIND, only: de1_IR, de2_i_IR
  implicit none
  integer,           intent(in)  :: rank
  complex(REALKIND), intent(in)  :: momenta(:,:), masses2(0:6), Gtensor(:)
  complex(REALKIND), intent(out) :: M2add
  integer,         intent(in) :: scboxes(:)
  type(scalarbox), intent(in) :: all_scboxes(:)

  complex(REALKIND) :: offshell_p(1:5,3), offshell_m2(3)

  complex(REALKIND) :: box_coeff(size(scboxes)), box(size(scboxes)), scalar_box(0:2)
  complex(REALKIND) :: q0p_q0m(2,5), momshift(5), Gsum(size(Gtensor))
  integer :: i1, i2, i3, k

  if(rank > 1) call ol_error("TI_reduction: reduction of a rank > 1 heptagon")

  M2add = 0._/**/REALKIND

  k = 0
  do i1 = 1, 5
    do i2 = i1, 5
      do i3 = i2, 5
        k = k + 1

        q0p_q0m = all_scboxes(scboxes(k))%onshell_cuts
        scalar_box = all_scboxes(scboxes(k))%poles
        Gsum = Gtensor

        if(i3 == 5) then
          if(i1 == 1) then
            if(i2 == 1) then
              momshift(1:5) = momenta(1:5,3)
            else
              momshift(1:5) = momenta(1:5,2)
            end if
          else
            momshift(1:5) = momenta(1:5,1)
          end if

          ! Assignment of the off-shell momenta and masses for all the sub-boxes
          ! in the case when D0 is pinched
          offshell_p(1:4,1) = - momshift(1:4)
          offshell_p(5,1) = momshift(5)

          offshell_p(1:4,2) = momenta(1:4,i1) - momshift(1:4)
          offshell_p(5,2) = momenta(5,i1) + momshift(5) - &
                            2*cont_LC_cntrv(momenta(1:4,i1),momshift(1:4))

          offshell_p(1:4,3) = momenta(1:4,i2+1) - momshift(1:4)
          offshell_p(5,3) = momenta(5,i2+1) + momshift(5) - &
                            2*cont_LC_cntrv(momenta(1:4,i2+1),momshift(1:4))

          offshell_m2(1) = masses2(0)
          offshell_m2(2) = masses2(i1)
          offshell_m2(3) = masses2(i2+1)

          call G_TensorShift(Gsum,-momshift(1:4))
        else

          ! Assignment of the off-shell momenta and masses for all the sub-boxes
          offshell_p(1:5,1) = momenta(1:5,i1)
          offshell_p(1:5,2) = momenta(1:5,i2+1)
          offshell_p(1:5,3) = momenta(1:5,i3+2)

          offshell_m2(1) = masses2(i1)
          offshell_m2(2) = masses2(i2+1)
          offshell_m2(3) = masses2(i3+2)
        end if

      call box_coefficient(offshell_p(:,:),&
      offshell_m2(:),q0p_q0m,Gsum,box_coeff(k))

      box(k) = box_coeff(k)*(scalar_box(0) + scalar_box(1)*de1_IR + scalar_box(2)*de2_i_IR)
      M2add = M2add + box(k)
      end do
    end do
  end do

end subroutine reduction_7points


end module ol_loop_reduction_/**/REALKIND
