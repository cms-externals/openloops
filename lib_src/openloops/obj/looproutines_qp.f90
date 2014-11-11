
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


module ol_OPP_storing_qp
  use kind_types, only: qp
  implicit none
  complex(qp), save :: Gtensor_stored(210) ! stored as an array of rank 6
  integer,           save :: rank_stored
  integer,           save :: array_length_stored ! length of the array associated with rank_stored
end module ol_OPP_storing_qp



module ol_loop_routines_qp
  implicit none
  contains

! ****************************************************
subroutine tensor_integral(rank, momenta, masses_2, TI)
! ****************************************************
  use kind_types, only: qp

  implicit none
  integer,           intent(in)  :: rank
  complex(qp), intent(in)  :: momenta(:,:), masses_2(:)
  complex(qp), intent(out) :: TI(:)

  TI = 0 ! prevent compiler warning
  print *, '[OpenLoops] ERROR in tensor_integral: Collier is not available'
  stop

end subroutine tensor_integral



! ****************************************************
subroutine tensor_integral_contract(rank, momenta, masses_2, Gtensor, M2)
! ****************************************************
  use kind_types, only: qp
  implicit none
  integer,           intent(in)  :: rank
  complex(qp), intent(in)  :: momenta(:,:), masses_2(:), Gtensor(:)
  complex(qp), intent(out) :: M2
  complex(qp) :: TI(size(Gtensor))
  call tensor_integral(rank, momenta, masses_2, TI)
  M2 = tensor_contract(Gtensor, TI)
end subroutine tensor_integral_contract



! ****************************************************
subroutine scalar_integral(momenta, masses_2)
! ****************************************************
  use kind_types, only: qp








  implicit none
  complex(qp), intent(in)  :: momenta(:,:), masses_2(:)

  print *, '[OpenLoops] ERROR in scalar_integral: Collier is not available'
  stop

end subroutine scalar_integral



! ****************************************************
subroutine covariant_coefficients(rank, momenta, masses_2)
! ****************************************************
  use kind_types, only: qp





  implicit none
  integer,           intent(in)  :: rank
  complex(qp), intent(in)  :: momenta(:,:), masses_2(:)

  print *, '[OpenLoops] ERROR in covariant_coefficients: Collier (legacy) is not available'
  stop

end subroutine covariant_coefficients



! ***************************************************
subroutine fake_tensor_integral(rank, momenta, masses_2, Gtensor, M2)
! Build a tensor as the direct product of the loop momentum with itself up to rank 'rank',
! divided by the denominator of the corresponding tensor integral.
! ***************************************************
! rank     = highest rank in the loop
! momenta  = list of the N-1 momenta flowing inside the loop without the loop momentum.
!            momenta(N) = 0 is not included.
! masses_2 = list of the N squared masses of the loop propagators
! TI       = array containing all independent tensor components up to highest rank (contravariant light-cone);
!            divided by the denominator 'den' corresponding to the loop with 'momenta' and 'masses_2'.
! den      = product((momenta(i)+Qloop)^2 - masses_2(i), i=1..N)
! The loop momentum 'pseudotree_momentum' is taken from the module pseudotree
! QloopLC  = loop momentum in light-cone representation
! ***************************************************
  use kind_types, only: qp
  use ol_tensor_bookkeeping, only: HR, tensor_size
  use ol_pseudotree_qp, only: pseudotree_momentum
  use ol_contractions_qp, only: cont_V
  use ol_kinematics_qp, only: Std2LC_Rep
  implicit none
  integer,           intent(in)  :: rank
  complex(qp), intent(in)  :: momenta(:,:), masses_2(:), Gtensor(:)
  complex(qp), intent(out) :: M2
  complex(qp) :: TI(size(Gtensor)), QloopLC(4), den
  integer           :: i, l

  call Std2LC_Rep(pseudotree_momentum, QloopLC)
  den = cont_V(QloopLC) - masses_2(1)
  do i = 1, size(masses_2)-1
    den = den * (cont_V(momenta(:,i) + QloopLC) - masses_2(i+1))
  end do
  TI(1) = 1 / den
  do l = 1, tensor_size(rank-1)
    do i = 1,4
      TI(HR(i,l)) = QloopLC(i)*TI(l)
    end do
  end do

  M2 = tensor_contract(Gtensor, TI)

end subroutine fake_tensor_integral



! ***************************************************
subroutine TI_call(rank, momenta, masses_2, Gsum, M2)
! ***************************************************
  use kind_types, only: qp
  use ol_parameters_decl_dp, only: a_switch
  implicit none
  integer,           intent(in)    :: rank
  complex(qp), intent(in)    :: momenta(:,:), masses_2(:), Gsum(:)
  real(qp),    intent(inout) :: M2
  complex(qp) :: M2add
  if (a_switch == 0) then
    call fake_tensor_integral(rank, momenta, masses_2, Gsum, M2add)
  else if (a_switch == 1 .or. a_switch == 7) then
    ! COLI/DD: full tensor integral
    call tensor_integral_contract(rank, momenta, masses_2, Gsum, M2add)
  else if (a_switch == 2) then
    ! COLI/DD: only covariant coefficients, no tensor contruction
    call covariant_coefficients(rank, momenta, masses_2)
    M2add = sum(Gsum)
  else if (a_switch == 3) then
    ! COLI/DD: only scalar integrals
    call scalar_integral(momenta, masses_2)
    M2add = sum(Gsum)
  else if (a_switch == 4) then
    ! Do nothing
    M2add = sum(Gsum)
  else if (a_switch == 5) then
    ! CutTools
    call cuttools_interface(rank, momenta, masses_2, Gsum, M2add)
  else if (a_switch == 6) then
    ! Samurai
    call samurai_interface(rank, momenta, masses_2, Gsum, M2add)
  else
    write(*,*) '[OpenLoops] ERROR in TI_call: amp_switch out of range: ', a_switch
    stop
  end if
  M2 = M2 + real(M2add)
end subroutine TI_call



! ****************************************************
function TI2_call(rank, momenta, masses_2, Gsum, TI)
! TI_call with precalculated tensor integrals.
! Used for loop^2 processes.
! Returns the contribution to the amplitude
! ****************************************************
  use kind_types, only: qp
  use ol_parameters_decl_dp, only: a_switch
  implicit none
  integer,           intent(in)    :: rank
  complex(qp), intent(in)    :: momenta(:,:), masses_2(:), Gsum(:), TI(:)
  complex(qp) :: TI2_call
  if (a_switch == 0) then
    call fake_tensor_integral(rank, momenta, masses_2, Gsum, TI2_call)
  else if (a_switch == 1 .or. a_switch == 7) then
    ! COLI/DD: contract with precalculated tensor integral
    TI2_call = tensor_contract(Gsum, TI)
  else if (a_switch == 4) then
    ! Do nothing
    TI2_call = sum(Gsum)
  else if (a_switch == 5) then
    ! CutTools
    call cuttools_interface(rank, momenta, masses_2, Gsum, TI2_call)
  else if (a_switch == 6) then
    ! Samurai
    call samurai_interface(rank, momenta, masses_2, Gsum, TI2_call)
  else
    write(*,*) '[OpenLoops] ERROR in TI2_call: amp_switch out of range: ', a_switch
    write(*,*) 'note that modes 2 and 3 are not supported in loop^2.'
    stop
  end if
end function TI2_call



! **********************************************************************
function tensor_contract(G, TI)
! Contract two tensors from rank 0 to the highest rank of G.
! TI must be of equal or higher rank as G.
! **********************************************************************
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in) :: G(:), TI(:)
  complex(qp) :: tensor_contract
  tensor_contract = sum(G*TI(1:size(G)))
end function tensor_contract



! ****************************************
subroutine loop_trace(G_in, Gtensor)
! ****************************************
! Close spinor or vector loop line after all vertex insertions
! Gtensor(l) = Tr[G_in(beta,l,alpha)] = Sum(G_in(alpha,l,alpha), alpha=1..4)
! alpha = covariant (light-cone)
! beta  = contravariant (light-cone)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_in(:,:,:)
  complex(qp), intent(out) :: Gtensor(:)
  Gtensor = G_in(1,:,1) + G_in(2,:,2) + G_in(3,:,3) + G_in(4,:,4)
end subroutine loop_trace



! *********************************************
subroutine loop_cont_VV(G_in, J_V2, J_V1, G_out)
! *********************************************
! contraction of tensor coefficients with vector currents for pseudo-tree
! G_out(l) =  J_V1(alpha) * G_in(beta,l,alpha) * J_V2(beta)
! J_Vi  = CONTRAVARIANT current
! alpha = COVARIANT index
! beta  = CONTRAVARIANT index
  use kind_types, only: qp
  use ol_contractions_qp, only: cont_VV
  implicit none
  complex(qp), intent(in)  :: G_in(:,:,:), J_V1(4), J_V2(4)
  complex(qp), intent(out) :: G_out(:)
  integer :: l

  do l = 1, size(G_in,2)
    G_out(l) = J_V1(1) * cont_VV(G_in(:,l,1), J_V2) + J_V1(2) * cont_VV(G_in(:,l,2), J_V2) &
             + J_V1(3) * cont_VV(G_in(:,l,3), J_V2) + J_V1(4) * cont_VV(G_in(:,l,4), J_V2)
  end do

end subroutine loop_cont_VV



! *************************************
subroutine loop_cont_QA(G_in, Q, A, G_out)
! *************************************
! Contraction of tensor coefficients with quark-antiquark currents (attach loop line)
! G_out(l) = Q(alpha) * G_in(beta,l,alpha) * A(beta)
  use kind_types, only: qp
  implicit none
  complex(qp), intent(in)  :: G_in(:,:,:), Q(4), A(4)
  complex(qp), intent(out) :: G_out(:)

  G_out = Q(1)*A(1)*G_in(1,:,1) + Q(2)*A(1)*G_in(2,:,1) + Q(3)*A(1)*G_in(3,:,1) + Q(4)*A(1)*G_in(4,:,1) &
        + Q(1)*A(2)*G_in(1,:,2) + Q(2)*A(2)*G_in(2,:,2) + Q(3)*A(2)*G_in(3,:,2) + Q(4)*A(2)*G_in(4,:,2) &
        + Q(1)*A(3)*G_in(1,:,3) + Q(2)*A(3)*G_in(2,:,3) + Q(3)*A(3)*G_in(3,:,3) + Q(4)*A(3)*G_in(4,:,3) &
        + Q(1)*A(4)*G_in(1,:,4) + Q(2)*A(4)*G_in(2,:,4) + Q(3)*A(4)*G_in(3,:,4) + Q(4)*A(4)*G_in(4,:,4)

end subroutine loop_cont_QA



! *****************************
subroutine G0initialisation(G0)
! *****************************
! Initialise the rank 0 tensor coefficient of the cut spinor or vector loop line;
! parameterised only by loop momentum
! G0(beta,1,alpha) = delta(alpha,beta)
! index notation: G0(beta,l,alpha)
! l = tensor index
! alpha = covariant (light-cone) "frozen" open index, is untouched till the last contraction in the loop
! beta  = contravariant (light-cone) "active" index contracted with vertices/props to build the loop
  use kind_types, only: qp
  implicit none
  complex(qp), intent(out) :: G0(4,1,4)
  G0 = 0
  G0(1,1,1) = 1
  G0(2,1,2) = 1
  G0(3,1,3) = 1
  G0(4,1,4) = 1
end subroutine G0initialisation



subroutine cts_numerator(q, amp)
  use kind_types, only: qp
  use ol_loop_momentum_qp, only: loop_mom_tens
  use ol_OPP_storing_qp
  implicit none
  complex(qp), intent(in)  :: q(0:3)
  complex(qp), intent(out) :: amp
  complex(qp) :: Qtensor(array_length_stored), QloopLC(1:4)

  call loop_mom_tens(q, rank_stored, Qtensor)
  amp = tensor_contract(Qtensor, Gtensor_stored) ! contract up to the length of Qtensor

end subroutine cts_numerator



function samurai_num(ncut, q, mu2)
  use kind_types, only: qp
  use ol_loop_momentum_qp, only: loop_mom_tens
  use ol_OPP_storing_qp
  implicit none
  complex(qp), intent(in) :: q(4) ! q = [p_x,p_y,p_z,E]
  complex(qp), intent(in) :: mu2
  integer,           intent(in) :: ncut
  complex(qp) :: samurai_num
  complex(qp) :: Qtensor(array_length_stored), q_tmp(0:3)

  q_tmp(0)   = q(4)
  q_tmp(1:3) = q(1:3)

  call loop_mom_tens(q_tmp, rank_stored, Qtensor)
  samurai_num = tensor_contract(Qtensor, Gtensor_stored) ! contract up to the length of Qtensor

end function samurai_num



subroutine cuttools_interface(rank, momenta, masses2, Gtensor, M2)
  use kind_types, only: qp, dp
  use ol_loop_parameters_decl_dp, only: opprootsvalue, mureg, de1_UV, de1_IR, de2_i_IR
  use ol_kinematics_qp, only: LC2Std_Rep
  use ol_OPP_storing_qp
  use ol_tensor_bookkeeping, only: tensor_size

  use cts_numdummies, only: dpnumdummy, mpnumdummy

  implicit none

  integer,           intent(in)  :: rank
  complex(qp), intent(in)  :: momenta(:,:), masses2(:), Gtensor(:)
  complex(qp), intent(out) :: M2

  complex(dp) :: cts_amp_array(0:2), cts_ampcc, cts_ar1
  real(qp)     :: mom(0:3)
  complex(dp) :: masses2_dp(size(masses2))
  real(dp)    :: cts_pp(0:3,0:size(masses2)-1)
  integer            :: l
  logical            :: cts_stable

  if (de1_UV /= de1_IR) then
    write(*,*) '========'
    write(*,*) 'ERROR in subroutine cuttools_interface:'
    write(*,*) 'pole1_UV != pole1_IR is not allowed with CutTools.'
    write(*,*) '========'
    stop
  end if

  Gtensor_stored(:size(Gtensor)) = Gtensor
  rank_stored = rank
  array_length_stored = tensor_size(rank)

  masses2_dp = masses2

  cts_pp(:,0) = 0
  do l = 1, size(momenta,2)
    call LC2Std_Rep(momenta(:,l), mom)
    cts_pp(:,l) = mom
  end do





  call ctsxcut(6, opprootsvalue, mureg, size(masses2), dpnumdummy, cts_numerator, rank, &
             & cts_pp, masses2_dp, cts_amp_array, cts_ampcc, cts_ar1, cts_stable)


  M2 = cts_amp_array(0) + cts_amp_array(1)*de1_IR + cts_amp_array(2)*de2_i_IR






end subroutine cuttools_interface



subroutine samurai_interface(rank, momenta, masses2, Gtensor, M2)
  use kind_types, only: qp
  use ol_loop_parameters_decl_qp, only: &
    & mureg2, de1_UV, de1_IR, de2_i_IR
  use ol_kinematics_qp, only: LC2Std_Rep
  use ol_OPP_storing_qp
  use ol_tensor_bookkeeping, only: tensor_size



  implicit none

  integer,           intent(in)  :: rank
  complex(qp), intent(in)  :: momenta(:,:), masses2(:), Gtensor(:)
  complex(qp), intent(out) :: M2

  M2 = 0 ! prevent compiler warning







  print *, '[OpenLoops] ERROR in samurai_interface: Samurai is not available'
  stop

end subroutine samurai_interface

end module ol_loop_routines_qp

