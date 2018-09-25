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


module ol_loop_handling_/**/REALKIND
  implicit none
  contains


!!*****************************************************************************
!! Functions for transposing an open loop (change of dressing direction)     !!
!!*****************************************************************************


!******************************************************************************
subroutine HGT_OLR(Gin,r1,r2,hel)
!------------------------------------------------------------------------------
! Transpose G(beta,l,alpha) for fixed l = tensor index
! alpha = covariant (light-cone) "frozen" open index, is untouched till the
!         last contraction in the loop
! beta  = contravariant (light-cone) "active" index contracted with vertices/
!         props to build the loop
!******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  integer, intent(in) :: r1, r2, hel
  complex(REALKIND), intent(inout) :: Gin(4,r1:r2,4,hel)
  complex(REALKIND) :: Gout(4,r1:r2,4,hel)
  integer :: al ,be, h

  do h=1,hel
    do al=1,4
      do be=1,4
        Gout(al,r1:r2,be,h)=Gin(be,r1:r2,al,h)
      end do
    end do
  end do

  Gin=Gout

end subroutine HGT_OLR


!******************************************************************************
subroutine HGT_invQ_OLR(Gin,r1,r2,hel)
!------------------------------------------------------------------------------
! Transpose G(beta,l,alpha) for fixed l = tensor index
! change sign because of inverted direction of q
! alpha = covariant (light-cone) "frozen" open index, is untouched till the
!         last contraction in the loop
! beta  = contravariant (light-cone) "active" index contracted with vertices/
!         props to build the loop
!******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  integer, intent(in) :: r1, r2, hel
  complex(REALKIND), intent(inout) :: Gin(4,r1:r2,4,hel)
  complex(REALKIND) :: Gout(4,r1:r2,4,hel)
  integer :: al, be, h

  do h=1,hel
    do al=1,4
        do be=1,4
          Gout(al,r1:r2,be,h)=-Gin(be,r1:r2,al,h)
        end do
    end do
  end do

  Gin=Gout

end subroutine HGT_invQ_OLR


!******************************************************************************
subroutine HGT_w2_OLR(Gin,r1,r2,hel)
!------------------------------------------------------------------------------
! Transpose G(beta,l,alpha) for fixed l = tensor index
! alpha = covariant (light-cone) "frozen" open index, is untouched till the
!         last contraction in the loop
! beta  = contravariant (light-cone) "active" index contracted with vertices/
!         props to build the loop
! NOTE: The factor 2 stemming from the metric when raising alpha before is
!       multiplied here
!******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  integer, intent(in) :: r1, r2, hel
  complex(REALKIND), intent(inout) :: Gin(4,r1:r2,4,hel)
  complex(REALKIND) :: Gout(4,r1:r2,4,hel)
  integer :: al ,be, h

  do h=1,hel
    do al=1,4
      do be=1,4
        Gout(al,r1:r2,be,h)=Gin(be,r1:r2,al,h)
      end do
    end do
  end do

  !! Factor 2 from raising alpha before
  Gin=Gout+Gout

end subroutine HGT_w2_OLR


!******************************************************************************
subroutine HGT_w2_invQ_OLR(Gin,r1,r2,hel)
!------------------------------------------------------------------------------
! Transpose G(beta,l,alpha) for fixed l = tensor index
! change sign because of inverted direction of q
! alpha = covariant (light-cone) "frozen" open index, is untouched till the
!         last contraction in the loop
! beta  = contravariant (light-cone) "active" index contracted with vertices/
!         props to build the loop
! NOTE: The factor 2 stemming from the metric when raising alpha before is
!       multiplied here
!******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  integer, intent(in) :: r1, r2, hel
  complex(REALKIND), intent(inout) :: Gin(4,r1:r2,4,hel)
  complex(REALKIND) :: Gout(4,r1:r2,4,hel)
  integer :: al ,be, h

  do h=1,hel
    do al=1,4
      do be=1,4
        Gout(al,r1:r2,be,h)=-Gin(be,r1:r2,al,h)
      end do
    end do
  end do

  !! Factor 2 from raising alpha before
  Gin = Gout + Gout

end subroutine HGT_w2_invQ_OLR


!******************************************************************************
subroutine HGT_raise_alpha_OLR(Gin,r1,r2,hel)
!------------------------------------------------------------------------------
! Transpose G(beta,l,alpha) for fixed l = tensor index
! alpha = covariant (light-cone) "frozen" open index, is untouched till the
!         last contraction in the loop is raised to become a contravariant
!         (light-cone) "active" index contracted with vertices/props to build
!         the loop against the usual direction
! beta  = contravariant (light-cone) "active" index contracted with vertices/
!         props to build the loop in usual direction (dir=0)
! NOTE: A factor 2 stemming from the metric is suppressed (to be multiplied
!       later)
!******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  integer, intent(in) :: r1, r2, hel
  complex(REALKIND), intent(inout) :: Gin(4,r1:r2,4,hel)
  complex(REALKIND) :: Gout(4,r1:r2,4,hel)
  integer :: be

  do be=1,4
    Gout(2,r1:r2,be,1:hel)= Gin(be,r1:r2,1,1:hel)
    Gout(1,r1:r2,be,1:hel)= Gin(be,r1:r2,2,1:hel)
    Gout(4,r1:r2,be,1:hel)=-Gin(be,r1:r2,3,1:hel)
    Gout(3,r1:r2,be,1:hel)=-Gin(be,r1:r2,4,1:hel)
  end do

  Gin = Gout

end subroutine HGT_raise_alpha_OLR

!******************************************************************************
subroutine HGT_raise_alpha_OLR_test(Gin,r1,r2,hel)
!------------------------------------------------------------------------------
! Transpose G(beta,l,alpha) for fixed l = tensor index
! alpha = covariant (light-cone) "frozen" open index, is untouched till the
!         last contraction in the loop is raised to become a contravariant
!         (light-cone) "active" index contracted with vertices/props to build
!         the loop against the usual direction
! beta  = contravariant (light-cone) "active" index contracted with vertices/
!         props to build the loop in usual direction (dir=0)
! NOTE: A factor 2 stemming from the metric is suppressed (to be multiplied
!       later)
!******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  integer, intent(in) :: r1, r2, hel
  complex(REALKIND), intent(inout) :: Gin(:,:,:,:)
  complex(REALKIND) :: Gout(4,size(Gin,2),4,hel)
  integer :: be

  do be=1,4
    Gout(2,r1:r2,be,1:hel)= Gin(be,r1:r2,1,1:hel)
    Gout(1,r1:r2,be,1:hel)= Gin(be,r1:r2,2,1:hel)
    Gout(4,r1:r2,be,1:hel)=-Gin(be,r1:r2,3,1:hel)
    Gout(3,r1:r2,be,1:hel)=-Gin(be,r1:r2,4,1:hel)
  end do

  Gin(:,r1:r2,1:4,1:hel) = Gout(:,r1:r2,1:4,1:hel)

end subroutine HGT_raise_alpha_OLR_test


!******************************************************************************
subroutine HGT_raise_alpha_invQ_OLR(Gin,r1,r2,hel)
!------------------------------------------------------------------------------
! Transpose G(beta,l,alpha) for fixed l = tensor index
! change sign because of inverted direction of q
! alpha = covariant (light-cone) "frozen" open index, is untouched till the
!         last contraction in the loop is raised to become a contravariant
!         (light-cone) "active" index contracted with vertices/props to build
!         the loop against the usual direction
! beta  = contravariant (light-cone) "active" index contracted with vertices/
!         props to build the loop in usual direction
! NOTE: A factor 2 stemming from the metric is suppressed (to be multiplied
!       later)
!******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  integer, intent(in) :: r1, r2, hel
  complex(REALKIND), intent(inout) :: Gin(4,r1:r2,4,hel)
  complex(REALKIND) :: Gout(4,r1:r2,4,hel)
  integer :: be, h

  do be=1,4
    Gout(2,r1:r2,be,1:hel)= -Gin(be,r1:r2,1,1:hel)
    Gout(1,r1:r2,be,1:hel)= -Gin(be,r1:r2,2,1:hel)
    Gout(4,r1:r2,be,1:hel)=  Gin(be,r1:r2,3,1:hel)
    Gout(3,r1:r2,be,1:hel)=  Gin(be,r1:r2,4,1:hel)
  end do

  Gin = Gout

end subroutine HGT_raise_alpha_invQ_OLR


!******************************************************************************
subroutine HGT_lower_alpha_OLR(Gin,r1,r2,hel)
!------------------------------------------------------------------------------
! Transpose G(alpha,l,beta) for fixed l = tensor index lowering
! alpha (contravariant in lightcone)
! NOTE: A factor 1/2 stemming from the metric is suppressed
!       (cancelling a factor 2 from raising alpha before)
!******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  integer,           intent(in)    :: r1, r2, hel
  complex(REALKIND), intent(inout) :: Gin(4,r1:r2,4,hel)
  complex(REALKIND)                :: Gout(4,r1:r2,4,hel)
  integer :: be, h

  do be=1,4
    Gout(be,r1:r2,2,1:hel)= Gin(1,r1:r2,be,1:hel)
    Gout(be,r1:r2,1,1:hel)= Gin(2,r1:r2,be,1:hel)
    Gout(be,r1:r2,4,1:hel)=-Gin(3,r1:r2,be,1:hel)
    Gout(be,r1:r2,3,1:hel)=-Gin(4,r1:r2,be,1:hel)
  end do

 Gin = Gout

end subroutine HGT_lower_alpha_OLR


!******************************************************************************
subroutine HGT_lower_alpha_invQ_OLR(Gin,r1,r2,hel)
!------------------------------------------------------------------------------
! Transpose G(alpha,l,beta) for fixed l = tensor index lowering
! change sign because of inverted direction of q
! alpha (contravariant in lightcone)
! NOTE: A factor 1/2 stemming from the metric is suppressed
!       (cancelling a factor 2 from raising alpha before)
!******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  integer, intent(in) :: r1, r2, hel
  complex(REALKIND), intent(inout) :: Gin(4,r1:r2,4,hel)
  complex(REALKIND) :: Gout(4,r1:r2,4,hel)
  integer :: be, h

  do be=1,4
    Gout(be,r1:r2,2,1:hel)= -Gin(1,r1:r2,be,1:hel)
    Gout(be,r1:r2,1,1:hel)= -Gin(2,r1:r2,be,1:hel)
    Gout(be,r1:r2,4,1:hel)=  Gin(3,r1:r2,be,1:hel)
    Gout(be,r1:r2,3,1:hel)=  Gin(4,r1:r2,be,1:hel)
  end do

  Gin = Gout

end subroutine HGT_lower_alpha_invQ_OLR


!******************************************************************************
subroutine HGT_lower_alpha_w2_OLR(Gin,r1,r2,hel)
!------------------------------------------------------------------------------
! Transpose G(alpha,l,beta) for fixed l = tensor index lowering
! alpha (contravariant in lightcone)
! NOTE: The factor 1/2 stemming from the metric is multiplied here
!******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  integer, intent(in) :: r1, r2, hel
  complex(REALKIND), intent(inout) :: Gin(4,r1:r2,4,hel)
  complex(REALKIND) :: Gout(4,r1:r2,4,hel)
  integer :: be, h

  do be=1,4
    Gout(be,r1:r2,2,1:hel)= Gin(1,r1:r2,be,1:hel)
    Gout(be,r1:r2,1,1:hel)= Gin(2,r1:r2,be,1:hel)
    Gout(be,r1:r2,4,1:hel)=-Gin(3,r1:r2,be,1:hel)
    Gout(be,r1:r2,3,1:hel)=-Gin(4,r1:r2,be,1:hel)
  end do

  Gin = Gout/2

end subroutine HGT_lower_alpha_w2_OLR


!******************************************************************************
subroutine HGT_lower_alpha_w2_invQ_OLR(Gin,r1,r2,hel)
!------------------------------------------------------------------------------
! Transpose G(alpha,l,beta) for fixed l = tensor index lowering
! change sign because of inverted direction of q
! alpha (contravariant in lightcone)
! NOTE: The factor 1/2 stemming from the metric is multiplied here
!******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  integer, intent(in) :: r1, r2, hel
  complex(REALKIND), intent(inout) :: Gin(4,r1:r2,4,hel)
  complex(REALKIND) :: Gout(4,r1:r2,4,hel)
  integer :: be, h

  do be = 1, 4
    Gout(be,r1:r2,2,1:hel)= -Gin(1,r1:r2,be,1:hel)
    Gout(be,r1:r2,1,1:hel)= -Gin(2,r1:r2,be,1:hel)
    Gout(be,r1:r2,4,1:hel)=  Gin(3,r1:r2,be,1:hel)
    Gout(be,r1:r2,3,1:hel)=  Gin(4,r1:r2,be,1:hel)
  end do

  Gin = Gout/2

end subroutine HGT_lower_alpha_w2_invQ_OLR


!!*****************************************************************************
!!            Functions for shifting the loop momentum                       !!
!!*****************************************************************************

!******************************************************************************
subroutine HG1shiftOLR(HG1in,dQ,htot)
!------------------------------------------------------------------------------
! Shift the momentum q -> q + dQ for the tensor G1(beta,l,alpha), i.e.
! the scalar part of G1 is shifted by the vector part contracted with dQ
! (all in light-cone rep)
!******************************************************************************
  use KIND_TYPES, only: REALKIND, intkind1
  use ol_data_types_/**/REALKIND, only: hol
  implicit none

  integer, intent(in):: htot
  type(hol), intent(inout)      :: HG1in
  complex(REALKIND), intent(in) :: dQ(4)
  complex(REALKIND) :: G1shift(4,1,4,htot)

  G1shift(:,1,:,:) = HG1in%j(:,2,:,:)*dQ(1) + HG1in%j(:,3,:,:)*dQ(2) &
                   + HG1in%j(:,4,:,:)*dQ(3) + HG1in%j(:,5,:,:)*dQ(4)

  HG1in%j(:,1,:,:) = HG1in%j(:,1,:,:) + G1shift(:,1,:,:)

end subroutine HG1shiftOLR


!******************************************************************************
subroutine G_TensorShift(Gin,dQ)
! -----------------------------------------------------------------------------
! It performs a loop-momentum shift, i.e. q -> q + dQ, for the tensor Gin(l).
! rank-1, rank-2 and rank-3 supported
!******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), intent(in) :: dQ(4)
  complex(REALKIND), intent(inout) :: Gin(:)

  if (size(Gin) == 5) then
    call G1tensorshiftOLR(Gin,dQ)
  else if (size(Gin) == 15) then
    call G2tensorshiftOLR(Gin,dQ)
  else if (size(Gin) == 35) then
    call G3tensorshiftOLR(Gin,dQ)
  end if

end subroutine G_TensorShift


!******************************************************************************
subroutine G1tensorshiftOLR(G1in,dQ)
! -----------------------------------------------------------------------------
! Shift the momentum q -> q + dQ for the tensor G1tensor(l), i.e. the scalar
! part of G1 is shifted by the vector part contracted with dQ (all in
! light-cone rep)
! l = tensor index
!******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), intent(in) :: dQ(4)
  complex(REALKIND), intent(inout) :: G1in(5)
  complex(REALKIND) :: G1shift

  G1shift = G1in(2)*dQ(1) + G1in(3)*dQ(2) + G1in(4)*dQ(3) + G1in(5)*dQ(4)
  G1in(1) = G1in(1) + G1shift
end subroutine G1tensorshiftOLR


!******************************************************************************
subroutine G2tensorshiftOLR(Gin,dQ)
! -----------------------------------------------------------------------------
! Shift the momentum q -> q + dQ for the tensor G2tensor(l), i.e. the vector
! and the scalar part  by the tensor and vector part contracted with dQ 
! (all in light-cone rep) 
! l = tensor index
!******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), intent(in) :: dQ(4)
  complex(REALKIND), intent(inout) :: Gin(15)
  complex(REALKIND) :: g2q0, g3q1,  g4q2,  g5q3
  complex(REALKIND) :: g6q0, g7q1,  g8q2,  g9q3
  complex(REALKIND) :: g7q0, g10q1, g11q2, g12q3
  complex(REALKIND) :: g8q0, g11q1, g13q2, g14q3
  complex(REALKIND) :: g9q0, g12q1, g14q2, g15q3
  complex(REALKIND) :: g2, g3, g4

  g2q0=Gin(2)*dQ(1)
  g3q1=Gin(3)*dQ(2)
  g4q2=Gin(4)*dQ(3)
  g5q3=Gin(5)*dQ(4)

  g6q0=Gin(6)*dQ(1)
  g7q1=Gin(7)*dQ(2)
  g8q2=Gin(8)*dQ(3)
  g9q3=Gin(9)*dQ(4)

  g7q0 =Gin(7)*dQ(1)
  g10q1=Gin(10)*dQ(2)
  g11q2=Gin(11)*dQ(3)
  g12q3=Gin(12)*dQ(4)

  g8q0 =Gin(8)*dQ(1)
  g11q1=Gin(11)*dQ(2)
  g13q2=Gin(13)*dQ(3)
  g14q3=Gin(14)*dQ(4)

  g9q0 =Gin(9)*dQ(1)
  g12q1=Gin(12)*dQ(2)
  g14q2=Gin(14)*dQ(3)
  g15q3=Gin(15)*dQ(4)

  g2= g6q0  + g7q1  + g8q2  + g9q3
  g3= g10q1 + g11q2 + g12q3
  g4= g13q2 + g14q3

  Gin(1) = Gin(1) + (g2q0 + g3q1 + g4q2 + g5q3) + dQ(1)*g2 + dQ(2)*g3 + &
           dQ(3)*g4 + dQ(4)*g15q3
  Gin(2) = Gin(2) + g2 + g6q0
  Gin(3) = Gin(3) + g3 + g10q1 + g7q0
  Gin(4) = Gin(4) + g4 + g13q2 + g11q1 + g8q0
  Gin(5) = Gin(5) + g15q3 + g15q3 + g14q2 + g12q1 + g9q0

end subroutine G2tensorshiftOLR

!******************************************************************************
subroutine G3tensorshiftOLR(Gin,dQ)
! -----------------------------------------------------------------------------
! Shift the momentum q -> q + dQ for the tensor G3tensor(l), i.e. the vector 
! and the scalar part by the tensor and vector part contracted with dQ 
! (all in light-cone rep)
! l = tensor index
!******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), intent(in) :: dQ(4)
  complex(REALKIND), intent(inout) :: Gin(35)
  complex(REALKIND) :: g16q1, g17q2, g18q3, g19q4
  complex(REALKIND) :: g17q1, g20q2, g21q3, g22q4
  complex(REALKIND) :: g18q1, g21q2, g23q3, g24q4
  complex(REALKIND) :: g19q1, g22q2, g24q3, g25q4
  complex(REALKIND) :: g20q1, g26q2, g27q3, g28q4
  complex(REALKIND) :: g21q1, g27q2, g29q3, g30q4
  complex(REALKIND) :: g22q1, g28q2, g30q3, g31q4
  complex(REALKIND) :: g23q1, g29q2, g32q3, g33q4
  complex(REALKIND) :: g24q1, g30q2, g33q3, g34q4
  complex(REALKIND) :: g25q1, g31q2, g34q3, g35q4
  complex(REALKIND) :: g6, g7, g8, g9, g10, g11, g12, g13, g14, g15
  complex(REALKIND) :: Gtmp_r2(1:15)

  g16q1 =Gin(16)*dQ(1)
  g17q2 =Gin(17)*dQ(2)
  g18q3 =Gin(18)*dQ(3)
  g19q4 =Gin(19)*dQ(4)

  g17q1 =Gin(17)*dQ(1)
  g20q2 =Gin(20)*dQ(2)
  g21q3 =Gin(21)*dQ(3)
  g22q4 =Gin(22)*dQ(4)

  g18q1 =Gin(18)*dQ(1)
  g21q2 =Gin(21)*dQ(2)
  g23q3 =Gin(23)*dQ(3)
  g24q4 =Gin(24)*dQ(4)

  g19q1 =Gin(19)*dQ(1)
  g22q2 =Gin(22)*dQ(2)
  g24q3 =Gin(24)*dQ(3)
  g25q4 =Gin(25)*dQ(4)

  g20q1 =Gin(20)*dQ(1)
  g26q2 =Gin(26)*dQ(2)
  g27q3 =Gin(27)*dQ(3)
  g28q4 =Gin(28)*dQ(4)

  g21q1 =Gin(21)*dQ(1)
  g27q2 =Gin(27)*dQ(2)
  g29q3 =Gin(29)*dQ(3)
  g30q4 =Gin(30)*dQ(4)

  g22q1 =Gin(22)*dQ(1)
  g28q2 =Gin(28)*dQ(2)
  g30q3 =Gin(30)*dQ(3)
  g31q4 =Gin(31)*dQ(4)

  g23q1 =Gin(23)*dQ(1)
  g29q2 =Gin(29)*dQ(2)
  g32q3 =Gin(32)*dQ(3)
  g33q4 =Gin(33)*dQ(4)

  g24q1 =Gin(24)*dQ(1)
  g30q2 =Gin(30)*dQ(2)
  g33q3 =Gin(33)*dQ(3)
  g34q4 =Gin(34)*dQ(4)

  g25q1 =Gin(25)*dQ(1)
  g31q2 =Gin(31)*dQ(2)
  g34q3 =Gin(34)*dQ(3)
  g35q4 =Gin(35)*dQ(4)

  g6  = g16q1 + g17q2 + g18q3 + g19q4
  g7  = g17q1 + g20q2 + g21q3 + g22q4
  g8  = g18q1 + g21q2 + g23q3 + g24q4
  g9  = g19q1 + g22q2 + g24q3 + g25q4
  g10 = g20q1 + g26q2 + g27q3 + g28q4
  g11 = g21q1 + g27q2 + g29q3 + g30q4
  g12 = g22q1 + g28q2 + g30q3 + g31q4
  g13 = g23q1 + g29q2 + g32q3 + g33q4
  g14 = g24q1 + g30q2 + g33q3 + g34q4
  g15 = g25q1 + g31q2 + g34q3 + g35q4

  Gtmp_r2(6)  = 2*g16q1       + g6
  Gtmp_r2(7)  = g17q1 + g20q2 + g7
  Gtmp_r2(8)  = g18q1 + g23q3 + g8
  Gtmp_r2(9)  = g19q1 + g25q4 + g9
  Gtmp_r2(10) = 2*g26q2       + g10
  Gtmp_r2(11) = g27q2 + g29q3 + g11
  Gtmp_r2(12) = g28q2 + g31q4 + g12
  Gtmp_r2(13) = 2*g32q3       + g13
  Gtmp_r2(14) = g33q3 + g34q4 + g14
  Gtmp_r2(15) = 2*g35q4       + g15

  Gtmp_r2(2) = g6*dQ(1) + g7*dQ(2)  + g8*dQ(3)  + g9*dQ(4)  +&
  2*g16q1*dQ(1) - (g21q3+g22q4)*dQ(2) - g24q3*dQ(4)

  Gtmp_r2(3) = g7*dQ(1) + g10*dQ(2) + g11*dQ(3) + g12*dQ(4) +&
  2*g26q2*dQ(2) - (g21q3+g22q4)*dQ(1) - g30q3*dQ(4)

  Gtmp_r2(4) = g8*dQ(1) + g11*dQ(2) + g13*dQ(3) + g14*dQ(4) +&
  2*g32q3*dQ(3) - (g21q2+g24q4)*dQ(1) - g30q2*dQ(4)

  Gtmp_r2(5) = g9*dQ(1) + g12*dQ(2) + g14*dQ(3) + g15*dQ(4) +&
  2*g35q4*dQ(4) - (g22q2+g24q3)*dQ(1) - g30q2*dQ(3)

  Gtmp_r2(1) = g6*(dQ(1)**2) + g10*(dQ(2)**2) + g13*(dQ(3)**2) + g15*(dQ(4)**2) + &
  (g24q3*dQ(1) + g30q3*dQ(2))*dQ(4) + (g21q1*dQ(3) + g22q1*dQ(4))*dQ(2)

  call G2tensorshiftOLR(Gin(1:15),dQ)

  Gin(1)    = Gin(1)    + Gtmp_r2(1)
  Gin(2:15) = Gin(2:15) + Gtmp_r2(2:15)

end subroutine G3tensorshiftOLR

end module ol_loop_handling_/**/REALKIND
