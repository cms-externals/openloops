
module ol_vamp_5_ppzzjj_bbxzzgg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_5(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_ppzzjj_bbxzzgg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_ppzzjj_bbxzzgg_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_ppzzjj_bbxzzgg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_ppzzjj_bbxzzgg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(3)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,1) :: G0
  complex(REALKIND), dimension(4,5,4,1) :: G1
  complex(REALKIND), dimension(15,8) :: G2tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_CV_D(G0(:,:,:,1),Q(:,0),wf(:,3),Q(:,48),G1(:,:,:,1))
  call check_last_CV_D(l_switch,G1(:,:,:,1),Q(:,48),wf(:,6),Q(:,15),G2tensor(:,1))
  call check_last_CV_D(l_switch,G1(:,:,:,1),Q(:,48),wf(:,316),Q(:,15),G2tensor(:,2))
  call check_last_CV_D(l_switch,G1(:,:,:,1),Q(:,48),wf(:,31),Q(:,15),G2tensor(:,3))
  call check_last_CV_D(l_switch,G1(:,:,:,1),Q(:,48),wf(:,333),Q(:,15),G2tensor(:,4))
  call check_last_CV_D(l_switch,G1(:,:,:,1),Q(:,48),wf(:,350),Q(:,15),G2tensor(:,5))
  call check_last_CV_D(l_switch,G1(:,:,:,1),Q(:,48),wf(:,367),Q(:,15),G2tensor(:,6))
  call check_last_CV_D(l_switch,G1(:,:,:,1),Q(:,48),wf(:,382),Q(:,15),G2tensor(:,7))
  call check_last_CV_D(l_switch,G1(:,:,:,1),Q(:,48),wf(:,383),Q(:,15),G2tensor(:,8))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (c(7)*(M(1)-M(2))) * den(231)
  T2sum(1:15,61) = T2sum(1:15,61) + Gcoeff * G2tensor(:,1)
  Gcoeff = (c(7)*(M(1)-M(2))) * den(479)
  T2sum(1:15,61) = T2sum(1:15,61) + Gcoeff * G2tensor(:,2)
  Gcoeff = (c(7)*(M(1)-M(2))) * den(267)
  T2sum(1:15,61) = T2sum(1:15,61) + Gcoeff * G2tensor(:,3)
  Gcoeff = (c(7)*(M(1)-M(2))) * den(494)
  T2sum(1:15,61) = T2sum(1:15,61) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(7)*(M(1)-M(2))) * den(503)
  T2sum(1:15,61) = T2sum(1:15,61) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(7)*(M(1)-M(2))) * den(514)
  T2sum(1:15,61) = T2sum(1:15,61) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(18)*(-M(1)+M(2))) * den(519)
  T2sum(1:15,61) = T2sum(1:15,61) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(18)*(-M(1)+M(2))) * den(520)
  T2sum(1:15,61) = T2sum(1:15,61) + Gcoeff * G2tensor(:,8)

end subroutine vamp_5

end module ol_vamp_5_ppzzjj_bbxzzgg_1_/**/REALKIND
