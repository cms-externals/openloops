
! **********************************************************************
module ol_tensor_sum_storage_pphlnjj_neexcxddxsh_1_/**/REALKIND
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none

  complex(REALKIND) :: momenta_1(4,1)
  complex(REALKIND) :: momenta_2(4,1)
  complex(REALKIND) :: momenta_3(4,1)
  complex(REALKIND) :: momenta_4(4,2)
  complex(REALKIND) :: momenta_5(4,2)
  complex(REALKIND) :: momenta_6(4,2)
  complex(REALKIND) :: momenta_7(4,2)
  complex(REALKIND) :: momenta_8(4,2)
  complex(REALKIND) :: momenta_9(4,2)
  complex(REALKIND) :: momenta_10(4,2)
  complex(REALKIND) :: momenta_11(4,2)
  complex(REALKIND) :: momenta_12(4,2)
  complex(REALKIND) :: momenta_13(4,3)
  complex(REALKIND) :: momenta_14(4,3)
  complex(REALKIND) :: momenta_15(4,3)
  complex(REALKIND) :: momenta_16(4,3)
  complex(REALKIND) :: momenta_17(4,3)
  complex(REALKIND) :: momenta_18(4,3)
  complex(REALKIND) :: momenta_19(4,3)
  complex(REALKIND) :: momenta_20(4,4)
  complex(REALKIND) :: momenta_21(4,4)

  complex(REALKIND), save :: masses2_1(2)
  complex(REALKIND), save :: masses2_2(2)
  complex(REALKIND), save :: masses2_3(2)
  complex(REALKIND), save :: masses2_4(3)
  complex(REALKIND), save :: masses2_5(3)
  complex(REALKIND), save :: masses2_6(3)
  complex(REALKIND), save :: masses2_7(4)
  complex(REALKIND), save :: masses2_8(5)

  complex(REALKIND), save :: T1sum(5,2) = 0
  complex(REALKIND), save :: T2sum(15,15) = 0
  complex(REALKIND), save :: T3sum(35,7) = 0

  contains

#ifdef PRECISION_dp
subroutine max_point(r) &
    & bind(c,name="ol_f_max_point_pphlnjj_neexcxddxsh_1")
  ! Return the number maximal tensor rank
  implicit none
  integer, intent(out) :: r
  r = 5
end subroutine max_point

subroutine tensor_rank(r) &
    & bind(c,name="ol_f_tensor_rank_pphlnjj_neexcxddxsh_1")
  ! Return the number maximal tensor rank
  implicit none
  integer, intent(out) :: r
  r = 3
end subroutine tensor_rank
#endif

subroutine reset_tensor_sum()
  implicit none
  T1sum = 0
  T2sum = 0
  T3sum = 0

end subroutine reset_tensor_sum


subroutine scale_one_tsum(tsum, spow)
  use ol_parameters_decl_/**/REALKIND, only: scalefactor
  implicit none
  complex(REALKIND), intent(inout) :: tsum(:)
  integer, intent(in) :: spow ! rank 0 scale power
  real(REALKIND) :: sfinv, sfac
  integer :: sz
  sfinv = 1/scalefactor
  sfac = scalefactor**spow
  sz = size(tsum)
  tsum(1) = sfac*tsum(1)
  if (sz > 1) then ! rank 1
    sfac = sfac*sfinv
    tsum(2:5) = sfac*tsum(2:5)
  end if
  if (sz > 5) then ! rank 2
    sfac = sfac*sfinv
    tsum(6:15) = sfac*tsum(6:15)
  end if
  if (sz > 15) then ! rank 3
    sfac = sfac*sfinv
    tsum(16:35) = sfac*tsum(16:35)
  end if
  if (sz > 35) then ! rank 4
    sfac = sfac*sfinv
    tsum(36:70) = sfac*tsum(36:70)
  end if
  if (sz > 70) then ! rank 5
    sfac = sfac*sfinv
    tsum(71:126) = sfac*tsum(71:126)
  end if
  if (sz > 126) then ! rank 6
    sfac = sfac*sfinv
    tsum(127:210) = sfac*tsum(127:210)
  end if
  if (sz > 210) then ! rank 7
    sfac = sfac*sfinv
    tsum(211:330) = sfac*tsum(211:330)
  end if
end subroutine scale_one_tsum


subroutine scale_tensor_sum()
  implicit none
  call scale_one_tsum(T1sum(:,1), -6)
  call scale_one_tsum(T1sum(:,2), -6)
  call scale_one_tsum(T2sum(:,1), -4)
  call scale_one_tsum(T2sum(:,2), -4)
  call scale_one_tsum(T2sum(:,3), -4)
  call scale_one_tsum(T2sum(:,4), -4)
  call scale_one_tsum(T2sum(:,5), -4)
  call scale_one_tsum(T2sum(:,6), -4)
  call scale_one_tsum(T2sum(:,7), -4)
  call scale_one_tsum(T2sum(:,8), -4)
  call scale_one_tsum(T2sum(:,9), -2)
  call scale_one_tsum(T2sum(:,10), -2)
  call scale_one_tsum(T2sum(:,11), -2)
  call scale_one_tsum(T2sum(:,12), -2)
  call scale_one_tsum(T2sum(:,13), -6)
  call scale_one_tsum(T2sum(:,14), -6)
  call scale_one_tsum(T2sum(:,15), -6)
  call scale_one_tsum(T3sum(:,1), -4)
  call scale_one_tsum(T3sum(:,2), -4)
  call scale_one_tsum(T3sum(:,3), -2)
  call scale_one_tsum(T3sum(:,4), -2)
  call scale_one_tsum(T3sum(:,5), -2)
  call scale_one_tsum(T3sum(:,6), 0)
  call scale_one_tsum(T3sum(:,7), 0)

end subroutine scale_tensor_sum


! **********************************************************************
subroutine integrate_tensor_sum(M2)
! **********************************************************************
  use ol_external_pphlnjj_neexcxddxsh_1, only: channel_number_pphlnjj_neexcxddxsh_1
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! only: ZERO, masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: a_switch
#endif
  use ol_parameters_init_/**/DREALKIND, only: channel_on, channel_off
  use ol_loop_routines_/**/REALKIND, only: TI_call, tensor_integral
  implicit none
  real(REALKIND), intent(out) :: M2
  call channel_on(channel_number_pphlnjj_neexcxddxsh_1)
  M2 = 0

  momenta_1(:,1) = Q(1:4,71)
  momenta_2(:,1) = Q(1:4,99)
  momenta_3(:,1) = Q(1:4,103)
  momenta_4(:,1) = Q(1:4,16)
  momenta_4(:,2) = Q(1:4,119)
  momenta_5(:,1) = Q(1:4,24)
  momenta_5(:,2) = Q(1:4,123)
  momenta_6(:,1) = Q(1:4,32)
  momenta_6(:,2) = Q(1:4,99)
  momenta_7(:,1) = Q(1:4,32)
  momenta_7(:,2) = Q(1:4,103)
  momenta_8(:,1) = Q(1:4,39)
  momenta_8(:,2) = Q(1:4,103)
  momenta_9(:,1) = Q(1:4,56)
  momenta_9(:,2) = Q(1:4,123)
  momenta_10(:,1) = Q(1:4,71)
  momenta_10(:,2) = Q(1:4,95)
  momenta_11(:,1) = Q(1:4,99)
  momenta_11(:,2) = Q(1:4,123)
  momenta_12(:,1) = Q(1:4,103)
  momenta_12(:,2) = Q(1:4,119)
  momenta_13(:,1) = Q(1:4,8)
  momenta_13(:,2) = Q(1:4,24)
  momenta_13(:,3) = Q(1:4,123)
  momenta_14(:,1) = Q(1:4,16)
  momenta_14(:,2) = Q(1:4,24)
  momenta_14(:,3) = Q(1:4,123)
  momenta_15(:,1) = Q(1:4,24)
  momenta_15(:,2) = Q(1:4,56)
  momenta_15(:,3) = Q(1:4,123)
  momenta_16(:,1) = Q(1:4,32)
  momenta_16(:,2) = Q(1:4,56)
  momenta_16(:,3) = Q(1:4,123)
  momenta_17(:,1) = Q(1:4,32)
  momenta_17(:,2) = Q(1:4,99)
  momenta_17(:,3) = Q(1:4,123)
  momenta_18(:,1) = Q(1:4,32)
  momenta_18(:,2) = Q(1:4,103)
  momenta_18(:,3) = Q(1:4,119)
  momenta_19(:,1) = Q(1:4,71)
  momenta_19(:,2) = Q(1:4,103)
  momenta_19(:,3) = Q(1:4,119)
  momenta_20(:,1) = Q(1:4,8)
  momenta_20(:,2) = Q(1:4,24)
  momenta_20(:,3) = Q(1:4,56)
  momenta_20(:,4) = Q(1:4,123)
  momenta_21(:,1) = Q(1:4,16)
  momenta_21(:,2) = Q(1:4,24)
  momenta_21(:,3) = Q(1:4,56)
  momenta_21(:,4) = Q(1:4,123)

  masses2_1 = [ MB2, MB2 ]
  masses2_2 = [ MT2, MT2 ]
  masses2_3 = [ ZERO2, ZERO2 ]
  masses2_4 = [ MB2, MB2, MB2 ]
  masses2_5 = [ MT2, MT2, MT2 ]
  masses2_6 = [ ZERO2, ZERO2, ZERO2 ]
  masses2_7 = [ ZERO2, ZERO2, ZERO2, ZERO2 ]
  masses2_8 = [ ZERO2, ZERO2, ZERO2, ZERO2, ZERO2 ]

#ifdef LOOPSQUARED
  if (a_switch == 1 .or. a_switch == 7) then
#endif

  call TI_call(1, momenta_2, masses2_3, T1sum(:,1), M2)
  call TI_call(1, momenta_1, masses2_3, T1sum(:,2), M2)

  call TI_call(2, momenta_6, masses2_6, T2sum(:,1), M2)
  call TI_call(2, momenta_10, masses2_6, T2sum(:,2), M2)
  call TI_call(2, momenta_7, masses2_6, T2sum(:,3), M2)
  call TI_call(2, momenta_9, masses2_6, T2sum(:,4), M2)
  call TI_call(2, momenta_11, masses2_6, T2sum(:,5), M2)
  call TI_call(2, momenta_5, masses2_6, T2sum(:,6), M2)
  call TI_call(2, momenta_4, masses2_6, T2sum(:,7), M2)
  call TI_call(2, momenta_12, masses2_6, T2sum(:,8), M2)
  call TI_call(2, momenta_18, masses2_7, T2sum(:,9), M2)
  call TI_call(2, momenta_19, masses2_7, T2sum(:,10), M2)
  call TI_call(2, momenta_13, masses2_7, T2sum(:,11), M2)
  call TI_call(2, momenta_14, masses2_7, T2sum(:,12), M2)
  call TI_call(2, momenta_3, masses2_3, T2sum(:,13), M2)
  call TI_call(2, momenta_3, masses2_2, T2sum(:,14), M2)
  call TI_call(2, momenta_3, masses2_1, T2sum(:,15), M2)

  call TI_call(3, momenta_8, masses2_5, T3sum(:,1), M2)
  call TI_call(3, momenta_8, masses2_4, T3sum(:,2), M2)
  call TI_call(3, momenta_17, masses2_7, T3sum(:,3), M2)
  call TI_call(3, momenta_16, masses2_7, T3sum(:,4), M2)
  call TI_call(3, momenta_15, masses2_7, T3sum(:,5), M2)
  call TI_call(3, momenta_20, masses2_8, T3sum(:,6), M2)
  call TI_call(3, momenta_21, masses2_8, T3sum(:,7), M2)

#ifdef LOOPSQUARED
  end if
#endif

  call channel_off(channel_number_pphlnjj_neexcxddxsh_1)
end subroutine integrate_tensor_sum

end module ol_tensor_sum_storage_pphlnjj_neexcxddxsh_1_/**/REALKIND
