
module ol_vamp_1_pptln_neextxbg_1_/**/REALKIND
contains

! **********************************************************************
subroutine vamp_1(M)
! P(0:3,nlegs) = incoming external momenta
! Uses tree structures 'wf', factors 'c', and denominators 'den' from loop_pptln_neextxbg_1.
! Sets colour stripped amplitudes A from the module loop_amplitudes_pptln_neextxbg_1.
! **********************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_parameters_decl_/**/REALKIND ! masses
#ifndef PRECISION_dp
  use ol_parameters_decl_/**/DREALKIND, only: l_switch
#endif
  use ol_loop_pptln_neextxbg_1_/**/REALKIND, only: c, f, wf, den
  use ol_vert_interface_/**/REALKIND
  use ol_prop_interface_/**/REALKIND
  use ol_last_step_/**/REALKIND
  use ol_tensor_sum_storage_pptln_neextxbg_1_/**/REALKIND
  use ol_loop_routines_/**/REALKIND, only: G0initialisation, TI2_call
  implicit none

  complex(REALKIND) :: Gcoeff
  complex(REALKIND), intent(in) :: M(1)

  ! Declarations of loop wave function tensors
  complex(REALKIND), dimension(4,1,4,6) :: G0
  complex(REALKIND), dimension(4,5,4,18) :: G1
  complex(REALKIND), dimension(4,15,4,18) :: G2
  complex(REALKIND), dimension(4,35,4,6) :: G3
  complex(REALKIND), dimension(5,4) :: G1tensor
  complex(REALKIND), dimension(15,12) :: G2tensor
  complex(REALKIND), dimension(35,6) :: G3tensor


  call G0initialisation(G0(:,:,:,1))

  ! Vertex and propagator calls to build loop structures
  call loop_VA_Q(G0(:,:,:,1),wf(:,3),G0(:,:,:,2))
  call loop_A_Q(G0(:,:,:,2),Q(:,20),MT,G1(:,:,:,1))
  call loop_AS_Q(G1(:,:,:,1),wf(:,1),G1(:,:,:,2),gPtb)
  call loop_A_Q(G1(:,:,:,2),Q(:,23),MB,G2(:,:,:,1))
  call check_last_AQ_V(l_switch,G2(:,:,:,1),wf(:,-3),G2tensor(:,1))
  call loop_AW_Q(G1(:,:,:,1),wf(:,5),G1(:,:,:,3))
  call loop_A_Q(G1(:,:,:,3),Q(:,23),MB,G2(:,:,:,2))
  call check_last_AQ_V(l_switch,G2(:,:,:,2),wf(:,-3),G2tensor(:,2))
  call check_last_AQ_V(l_switch,G1(:,:,:,1),wf(:,20),G1tensor(:,1))
  call check_last_AQ_V(l_switch,G1(:,:,:,1),wf(:,21),G1tensor(:,2))
  call loop_VQ_A(G0(:,:,:,1),wf(:,8),G0(:,:,:,3))
  call loop_Q_A(G0(:,:,:,3),Q(:,24),MB,G1(:,:,:,4))
  call loop_QS_A(G1(:,:,:,4),wf(:,1),G1(:,:,:,5),gPtb)
  call loop_Q_A(G1(:,:,:,5),Q(:,27),MT,G2(:,:,:,3))
  call check_last_QA_V(l_switch,G2(:,:,:,3),wf(:,-2),G2tensor(:,3))
  call loop_QW_A(G1(:,:,:,4),wf(:,5),G1(:,:,:,6))
  call loop_Q_A(G1(:,:,:,6),Q(:,27),MT,G2(:,:,:,4))
  call check_last_QA_V(l_switch,G2(:,:,:,4),wf(:,-2),G2tensor(:,4))
  call check_last_QA_V(l_switch,G1(:,:,:,4),wf(:,23),G1tensor(:,3))
  call check_last_QA_V(l_switch,G1(:,:,:,4),wf(:,24),G1tensor(:,4))
  call loop_VQ_A(G0(:,:,:,1),wf(:,-3),G0(:,:,:,4))
  call loop_Q_A(G0(:,:,:,4),Q(:,8),MB,G1(:,:,:,7))
  call loop_QV_A(G1(:,:,:,7),wf(:,-4),G1(:,:,:,8))
  call loop_Q_A(G1(:,:,:,8),Q(:,24),MB,G2(:,:,:,5))
  call check_last_QA_V(l_switch,G2(:,:,:,5),wf(:,23),G2tensor(:,5))
  call check_last_QA_V(l_switch,G2(:,:,:,5),wf(:,24),G2tensor(:,6))
  call loop_QS_A(G2(:,:,:,5),wf(:,1),G2(:,:,:,6),gPtb)
  call loop_Q_A(G2(:,:,:,6),Q(:,27),MT,G3(:,:,:,1))
  call check_last_QA_V(l_switch,G3(:,:,:,1),wf(:,-2),G3tensor(:,1))
  call loop_QW_A(G2(:,:,:,5),wf(:,5),G2(:,:,:,7))
  call loop_Q_A(G2(:,:,:,7),Q(:,27),MT,G3(:,:,:,2))
  call check_last_QA_V(l_switch,G3(:,:,:,2),wf(:,-2),G3tensor(:,2))
  call loop_QS_A(G1(:,:,:,7),wf(:,1),G1(:,:,:,9),gPtb)
  call loop_Q_A(G1(:,:,:,9),Q(:,11),MT,G2(:,:,:,8))
  call loop_QV_A(G2(:,:,:,8),wf(:,-4),G2(:,:,:,9))
  call loop_Q_A(G2(:,:,:,9),Q(:,27),MT,G3(:,:,:,3))
  call check_last_QA_V(l_switch,G3(:,:,:,3),wf(:,-2),G3tensor(:,3))
  call loop_QW_A(G1(:,:,:,7),wf(:,5),G1(:,:,:,10))
  call loop_Q_A(G1(:,:,:,10),Q(:,11),MT,G2(:,:,:,10))
  call loop_QV_A(G2(:,:,:,10),wf(:,-4),G2(:,:,:,11))
  call loop_Q_A(G2(:,:,:,11),Q(:,27),MT,G3(:,:,:,4))
  call check_last_QA_V(l_switch,G3(:,:,:,4),wf(:,-2),G3tensor(:,4))
  call loop_UV_W(G0(:,:,:,1),Q(:,0),wf(:,-4),Q(:,16),G1(:,:,:,11))
  call loop_VQ_A(G1(:,:,:,11),wf(:,-3),G1(:,:,:,12))
  call loop_Q_A(G1(:,:,:,12),Q(:,24),MB,G2(:,:,:,12))
  call check_last_QA_V(l_switch,G2(:,:,:,12),wf(:,23),G2tensor(:,7))
  call check_last_QA_V(l_switch,G2(:,:,:,12),wf(:,24),G2tensor(:,8))
  call loop_QS_A(G2(:,:,:,12),wf(:,1),G2(:,:,:,13),gPtb)
  call loop_Q_A(G2(:,:,:,13),Q(:,27),MT,G3(:,:,:,5))
  call check_last_QA_V(l_switch,G3(:,:,:,5),wf(:,-2),G3tensor(:,5))
  call loop_QW_A(G2(:,:,:,12),wf(:,5),G2(:,:,:,14))
  call loop_Q_A(G2(:,:,:,14),Q(:,27),MT,G3(:,:,:,6))
  call check_last_QA_V(l_switch,G3(:,:,:,6),wf(:,-2),G3tensor(:,6))
  call loop_VQ_A(G1(:,:,:,11),wf(:,20),G1(:,:,:,13))
  call loop_Q_A(G1(:,:,:,13),Q(:,27),MT,G2(:,:,:,15))
  call check_last_QA_V(l_switch,G2(:,:,:,15),wf(:,-2),G2tensor(:,9))
  call loop_VQ_A(G1(:,:,:,11),wf(:,21),G1(:,:,:,14))
  call loop_Q_A(G1(:,:,:,14),Q(:,27),MT,G2(:,:,:,16))
  call check_last_QA_V(l_switch,G2(:,:,:,16),wf(:,-2),G2tensor(:,10))
  call loop_VQ_A(G0(:,:,:,1),wf(:,20),G0(:,:,:,5))
  call loop_Q_A(G0(:,:,:,5),Q(:,11),MT,G1(:,:,:,15))
  call loop_QV_A(G1(:,:,:,15),wf(:,-4),G1(:,:,:,16))
  call loop_Q_A(G1(:,:,:,16),Q(:,27),MT,G2(:,:,:,17))
  call check_last_QA_V(l_switch,G2(:,:,:,17),wf(:,-2),G2tensor(:,11))
  call loop_VQ_A(G0(:,:,:,1),wf(:,21),G0(:,:,:,6))
  call loop_Q_A(G0(:,:,:,6),Q(:,11),MT,G1(:,:,:,17))
  call loop_QV_A(G1(:,:,:,17),wf(:,-4),G1(:,:,:,18))
  call loop_Q_A(G1(:,:,:,18),Q(:,27),MT,G2(:,:,:,18))
  call check_last_QA_V(l_switch,G2(:,:,:,18),wf(:,-2),G2tensor(:,12))


  ! add colour interference with born to the sum of coefficient tensors for the proper tensor integral
  Gcoeff = (-(c(6)*M(1))) * den(3)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,1)
  Gcoeff = (-(c(3)*M(1))) * den(3)
  T2sum(1:15,1) = T2sum(1:15,1) + Gcoeff * G2tensor(:,2)
  Gcoeff = (-(c(6)*M(1))) * den(5)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,3)
  Gcoeff = (-(c(3)*M(1))) * den(5)
  T2sum(1:15,2) = T2sum(1:15,2) + Gcoeff * G2tensor(:,4)
  Gcoeff = (c(5)*M(1)) * den(10)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,5)
  Gcoeff = (c(4)*M(1)) * den(10)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,7)
  Gcoeff = (c(2)*M(1)) * den(10)
  T2sum(1:15,3) = T2sum(1:15,3) + Gcoeff * G2tensor(:,6)
  Gcoeff = (c(1)*M(1)) * den(10)
  T2sum(1:15,4) = T2sum(1:15,4) + Gcoeff * G2tensor(:,8)
  Gcoeff = (c(5)*M(1)) * den(7)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,11)
  Gcoeff = (c(4)*M(1)) * den(7)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,9)
  Gcoeff = (c(2)*M(1)) * den(7)
  T2sum(1:15,5) = T2sum(1:15,5) + Gcoeff * G2tensor(:,12)
  Gcoeff = (c(1)*M(1)) * den(7)
  T2sum(1:15,6) = T2sum(1:15,6) + Gcoeff * G2tensor(:,10)
  Gcoeff = (c(5)*M(1)) * den(1)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,3)
  Gcoeff = (c(2)*M(1)) * den(1)
  T3sum(1:35,1) = T3sum(1:35,1) + Gcoeff * G3tensor(:,4)
  Gcoeff = (c(5)*M(1)) * den(1)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,1)
  Gcoeff = (c(2)*M(1)) * den(1)
  T3sum(1:35,2) = T3sum(1:35,2) + Gcoeff * G3tensor(:,2)
  Gcoeff = (c(4)*M(1)) * den(1)
  T3sum(1:35,3) = T3sum(1:35,3) + Gcoeff * G3tensor(:,5)
  Gcoeff = (c(1)*M(1)) * den(1)
  T3sum(1:35,3) = T3sum(1:35,3) + Gcoeff * G3tensor(:,6)
  Gcoeff = (-(c(6)*M(1))) * den(8)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,1)
  Gcoeff = (-(c(3)*M(1))) * den(8)
  T1sum(1:5,1) = T1sum(1:5,1) + Gcoeff * G1tensor(:,2)
  Gcoeff = (-(c(6)*M(1))) * den(11)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,3)
  Gcoeff = (-(c(3)*M(1))) * den(11)
  T1sum(1:5,2) = T1sum(1:5,2) + Gcoeff * G1tensor(:,4)

end subroutine vamp_1

end module ol_vamp_1_pptln_neextxbg_1_/**/REALKIND
