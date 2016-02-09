
! **********************************************************************
module ol_tensor_sum_storage_ppllllj2_onlyh_eexmmxggg_1_/**/REALKIND
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none

  complex(REALKIND) :: momenta_1(4,2)
  complex(REALKIND) :: momenta_2(4,2)
  complex(REALKIND) :: momenta_3(4,2)
  complex(REALKIND) :: momenta_4(4,3)
  complex(REALKIND) :: momenta_5(4,3)
  complex(REALKIND) :: momenta_6(4,3)

  complex(REALKIND), save :: masses2_1(3)
  complex(REALKIND), save :: masses2_2(3)
  complex(REALKIND), save :: masses2_3(4)
  complex(REALKIND), save :: masses2_4(4)

  complex(REALKIND), save :: T3sum(35,6) = 0
  complex(REALKIND), save :: T4sum(70,6) = 0

  contains

#ifdef PRECISION_dp
subroutine max_point(r) &
    & bind(c,name="ol_f_max_point_ppllllj2_onlyh_eexmmxggg_1")
  ! Return the number maximal tensor rank
  implicit none
  integer, intent(out) :: r
  r = 4
end subroutine max_point

subroutine tensor_rank(r) &
    & bind(c,name="ol_f_tensor_rank_ppllllj2_onlyh_eexmmxggg_1")
  ! Return the number maximal tensor rank
  implicit none
  integer, intent(out) :: r
  r = 4
end subroutine tensor_rank
#endif

subroutine reset_tensor_sum()
  implicit none

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
  call scale_one_tsum(T3sum(:,1), -4)
  call scale_one_tsum(T3sum(:,2), -4)
  call scale_one_tsum(T3sum(:,3), -4)
  call scale_one_tsum(T3sum(:,4), -4)
  call scale_one_tsum(T3sum(:,5), -4)
  call scale_one_tsum(T3sum(:,6), -4)
  call scale_one_tsum(T4sum(:,1), -2)
  call scale_one_tsum(T4sum(:,2), -2)
  call scale_one_tsum(T4sum(:,3), -2)
  call scale_one_tsum(T4sum(:,4), -2)
  call scale_one_tsum(T4sum(:,5), -2)
  call scale_one_tsum(T4sum(:,6), -2)

end subroutine scale_tensor_sum


! **********************************************************************
subroutine integrate_tensor_sum(M2)
! **********************************************************************
  use ol_external_ppllllj2_onlyh_eexmmxggg_1, only: channel_number_ppllllj2_onlyh_eexmmxggg_1
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! only: ZERO, masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: a_switch
#endif
  use ol_parameters_init_/**/DREALKIND, only: channel_on, channel_off
  use ol_loop_routines_/**/REALKIND, only: TI_call, tensor_integral
  implicit none
  real(REALKIND), intent(out) :: M2
  call channel_on(channel_number_ppllllj2_onlyh_eexmmxggg_1)
  M2 = 0

  momenta_1(:,1) = Q(1:4,16)
  momenta_1(:,2) = Q(1:4,112)
  momenta_2(:,1) = Q(1:4,32)
  momenta_2(:,2) = Q(1:4,112)
  momenta_3(:,1) = Q(1:4,48)
  momenta_3(:,2) = Q(1:4,112)
  momenta_4(:,1) = Q(1:4,16)
  momenta_4(:,2) = Q(1:4,48)
  momenta_4(:,3) = Q(1:4,112)
  momenta_5(:,1) = Q(1:4,16)
  momenta_5(:,2) = Q(1:4,80)
  momenta_5(:,3) = Q(1:4,112)
  momenta_6(:,1) = Q(1:4,32)
  momenta_6(:,2) = Q(1:4,48)
  momenta_6(:,3) = Q(1:4,112)

  masses2_1 = [ MB2, MB2, MB2 ]
  masses2_2 = [ MT2, MT2, MT2 ]
  masses2_3 = [ MB2, MB2, MB2, MB2 ]
  masses2_4 = [ MT2, MT2, MT2, MT2 ]

#ifdef LOOPSQUARED
  if (a_switch == 1 .or. a_switch == 7) then
#endif

  call tensor_integral(3, momenta_3, masses2_2, T3sum(:,1))
  call tensor_integral(3, momenta_3, masses2_1, T3sum(:,2))
  call tensor_integral(3, momenta_2, masses2_2, T3sum(:,3))
  call tensor_integral(3, momenta_2, masses2_1, T3sum(:,4))
  call tensor_integral(3, momenta_1, masses2_2, T3sum(:,5))
  call tensor_integral(3, momenta_1, masses2_1, T3sum(:,6))

  call tensor_integral(4, momenta_6, masses2_4, T4sum(:,1))
  call tensor_integral(4, momenta_6, masses2_3, T4sum(:,2))
  call tensor_integral(4, momenta_4, masses2_4, T4sum(:,3))
  call tensor_integral(4, momenta_4, masses2_3, T4sum(:,4))
  call tensor_integral(4, momenta_5, masses2_4, T4sum(:,5))
  call tensor_integral(4, momenta_5, masses2_3, T4sum(:,6))

#ifdef LOOPSQUARED
  end if
#endif

  call channel_off(channel_number_ppllllj2_onlyh_eexmmxggg_1)
end subroutine integrate_tensor_sum

end module ol_tensor_sum_storage_ppllllj2_onlyh_eexmmxggg_1_/**/REALKIND
