!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  *******************
!  *  module Checks  *
!  *  by Lars Hofer  *
!  *******************
!
!  functions and subroutines:
!  CheckTensors, CalcSignum
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



module Checks

  use TensorManipulations
  use GramCayley
  use TensorReduction

  implicit none

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CheckTensors(N,r,MomVec,masses2,MomInv,TNr) 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CheckTensors(N,r,MomVec,masses2,MomInv,TNr)
  
    integer, intent(in) :: N, r
    complex*16, intent(in) :: MomVec(0:3,N-1), masses2(0:N-1), MomInv(BinomTable(2,N))
    complex*16, intent(in) :: TNr(RankToSize(4,r),0:r)
    complex*16 :: Gram(N-1,N-1), check1, check2
    complex*16, allocatable :: Coefs(:,:,:)
    integer :: inds2(2), inds3(3), inds4(4), inds5(5), inds6(6)
    integer :: rts, bino, kmax, Nm1, signum, signum1, signum2, signum3, signum4
    integer :: i,j,k,m,cnt,a,b,c,d,e,f

    write(*,*) ''
    write(*,*) ''
    write(*,*) ''
    write(*,*) '*********************************************************'
    write(*,*) '*                check tensor integrals:                *' 
    write(*,*) '* calculate contractions with various tensor structures *' 
    write(*,*) '* and compare with direct expressions in terms of       *'
    write(*,*) '* tensor coefficients                                   *'
    write(*,*) '*********************************************************'
    write(*,*) ''
    write(*,*) ''
    if (r.le.0) return    

    Nm1 = N-1
    rts = RankToSize(4,r)
    bino = BinomTable(r,r+N-2)
    kmax = r/2

    allocate(Coefs(bino,0:kmax,0:r))
    Coefs = CalcTNrPVco(N,r,masses2,MomInv)

    Gram = CalcGramMat2(Nm1,MomInv)

    write(*,*) '*********************'
    write(*,*) '*     rank = 1      *'
    write(*,*) '*********************'

    ! contraction with pi
    do i=1,Nm1

      check1 = MomVec(0,i)*TNr(1,1)
      do j=1,3
        check1 = check1 - MomVec(j,i)*TNr(j+1,1)
      end do

      check2 = 0
      do j=1,Nm1
        check2 = check2 + Coefs(j,0,1)*Gram(j,i)/2d0
      end do
      
      write(*,*) i
      write(*,*) check1, check2      
    end do                              
    write(*,*) ''
    write(*,*) ''

    if (r.le.1) return
   

    write(*,*) '*********************'
    write(*,*) '*     rank = 2      *'
    write(*,*) '*********************'

    ! contraction with g
    check1 = TNr(1,2) - TNr(5,2) - TNr(8,2) - TNr(10,2)
    
    check2 = 4d0*Coefs(1,1,2)
    cnt = 1
    do i=1,N-1
      check2 = check2 + Gram(i,i)*Coefs(cnt,0,2)/2d0
      cnt = cnt+1
      do j=i+1,N-1
        inds2(2) = j-1
        check2 = check2 + Gram(j,i)*Coefs(cnt,0,2)
        cnt = cnt+1 
      end do 
    end do
    
    write(*,*) 0, 0
    write(*,*) check1, check2
    
    ! contraction with pk*pm
    do k=1,N-1
      do m=k,N-1

        check1 = 0d0
        do i=0,3
          signum1 = CalcSignum(i)
          inds2(1) = i
  
          do j=0,3
            signum2 = CalcSignum(j)
            inds2(2) = j
  
            check1 = check1 + signum1*signum2*MomVec(i,k)*MomVec(j,m)* &
                & GetTensorElement(2,TNr(1:10,2),1,inds2)
   
          end do
        end do
        
        check2 = Gram(k,m)*Coefs(1,1,2)/2d0
        cnt = 1
        do i=1,N-1
          check2 = check2 + Gram(k,i)*Gram(m,i)* &
                &  Coefs(cnt,0,2)/4d0
          cnt = cnt+1
          do j=i+1,N-1
            check2 = check2 + (Gram(k,i)*Gram(m,j)+Gram(k,j)*Gram(m,i))* &
                &  Coefs(cnt,0,2)/4d0
            cnt = cnt+1
          end do
        end do
        
        write(*,*) k, m
        write(*,*) check1, check2
      end do
    end do
    write(*,*) ''
    write(*,*) ''
    
    if (r.le.2) return
    
    
    write(*,*) '*********************'
    write(*,*) '*     rank = 3      *'
    write(*,*) '*********************'

    ! contraction with g*pk
    do k=1,N-1
      
      check1 = 0
      do i=0,3
        signum = CalcSignum(i)
        inds3(1) = i
        inds3(2) = 0
        inds3(3) = 0 
        check1 = check1 + signum*MomVec(i,k)* &
                & GetTensorElement(3,TNr(1:20,3),1,inds3)         
        do j=1,3
          inds3(2) = j
          inds3(3) = j
          check1 = check1 - signum*MomVec(i,k)* &
                & GetTensorElement(3,TNr(1:20,3),1,inds3)
        end do
      end do

      check2 = 0
      cnt = 1
      do i=1,N-1
        check2 = check2 + 3*Coefs(i,1,3)*Gram(i,k) + Coefs(cnt,0,3)*Gram(i,i)*Gram(i,k)/4d0
        cnt = cnt+1
        do m=i+1,N-1
          check2 = check2 + Coefs(cnt,0,3)*(Gram(i,i)*Gram(m,k)+2d0*Gram(i,m)*Gram(i,k))/4d0
          cnt = cnt+1
        end do 
        do j=i+1,N-1
          check2 = check2 + Coefs(cnt,0,3)*(Gram(j,j)*Gram(i,k)+2d0*Gram(i,j)*Gram(j,k))/4d0
          cnt = cnt+1
          do m=j+1,N-1
            check2 = check2 + Coefs(cnt,0,3)*(Gram(i,j)*Gram(m,k)+Gram(i,m)*Gram(j,k)+ &
                     & Gram(j,m)*Gram(i,k))/2d0
            cnt = cnt+1
          end do
        end do
      end do
         
      write(*,*) 0, 0, k
      write(*,*) check1, check2
          
    end do

    ! contraction with pi*pj*pk
    do i=1,N-1
      do j=i,N-1
        do k=j,N-1
          
          check1 = 0
          do a=0,3
            signum1 = CalcSignum(a)
            inds3(1) = a

            do b=0,3
              signum2 = CalcSignum(b)
              inds3(2) = b

              do c=0,3
                signum3 = CalcSignum(c)
                inds3(3) = c

                check1 = check1 + signum1*signum2*signum3*MomVec(a,i)*MomVec(b,j)* &
                  & MomVec(c,k)*GetTensorElement(3,TNr(1:20,3),1,inds3)

              end do
            end do
          end do

          check2 = 0
          cnt = 1
          do a=1,N-1
            check2 = check2 + Coefs(a,1,3)*(Gram(a,i)*Gram(j,k)+Gram(a,j)*Gram(i,k) &
                      & +Gram(a,k)*Gram(i,j))/4d0 &
                      & + Coefs(cnt,0,3)*Gram(a,i)*Gram(a,j)*Gram(a,k)/8d0
            cnt = cnt+1
            do c=a+1,N-1
              check2 = check2 + Coefs(cnt,0,3)*(Gram(a,i)*Gram(a,j)*Gram(c,k)+ &
                      & Gram(a,i)*Gram(c,j)*Gram(a,k)+Gram(c,i)*Gram(a,j)*Gram(a,k))/8d0
              cnt = cnt+1
            end do 
            do b=a+1,N-1
              check2 = check2 + Coefs(cnt,0,3)*(Gram(a,i)*Gram(b,j)*Gram(b,k)+ &
                      & Gram(b,i)*Gram(a,j)*Gram(b,k)+Gram(b,i)*Gram(b,j)*Gram(a,k))/8d0
              cnt = cnt+1
              do c=b+1,N-1
                check2 = check2 + Coefs(cnt,0,3)*(Gram(a,i)*Gram(b,j)*Gram(c,k)+ &
                      & Gram(a,i)*Gram(c,j)*Gram(b,k)+Gram(b,i)*Gram(a,j)*Gram(c,k)+ &
                      & Gram(b,i)*Gram(c,j)*Gram(a,k)+Gram(c,i)*Gram(a,j)*Gram(b,k)+ &
                      & Gram(c,i)*Gram(b,j)*Gram(a,k))/8d0
                cnt = cnt+1
              end do
            end do
          end do

          write(*,*) i,j,k
          write(*,*) check1, check2
        end do
      end do
    end do
    write(*,*) ''
    write(*,*) ''
        
    if (r.le.3) return
    
    
    write(*,*) '*********************'
    write(*,*) '*     rank = 4      *'
    write(*,*) '*********************'

    ! contraction with g*g
    check1 = 0
    do i=0,3
      signum1 = CalcSignum(i)
      inds4(1) = i
      inds4(2) = i

      do j=0,3
        signum2 = CalcSignum(j)
        inds4(3) = j
        inds4(4) = j

        check1 = check1 + signum1*signum2* &
                  & GetTensorElement(4,TNr(1:35,4),1,inds4)

      end do
    end do

    check2 = 24d0*Coefs(1,2,4)
    cnt = 1
    do i=1,N-1
      check2 = check2 + 6d0*Coefs(cnt,1,4)*Gram(i,i)
      cnt = cnt+1
      do j=i+1,N-1
        check2 = check2 + 12d0*Coefs(cnt,1,4)*Gram(i,j)
        cnt = cnt+1
      end do
    end do
    cnt = 1
    do i=1,N-1
      check2 = check2 + Coefs(cnt,0,4)*Gram(i,i)*Gram(i,i)/4d0
      cnt = cnt+1
      do m=i+1,N-1
        check2 = check2 + Coefs(cnt,0,4)*Gram(i,i)*Gram(i,m)
        cnt = cnt+1
      end do
      do k=i+1,N-1
        check2 = check2 + Coefs(cnt,0,4)*(Gram(i,i)*Gram(k,k)+ &
                 & 2d0*Gram(i,k)*Gram(i,k))/2d0
        cnt = cnt+1
        do m=k+1,N-1
          check2 = check2 + Coefs(cnt,0,4)*(Gram(i,i)*Gram(k,m)+ &
                 & 2d0*Gram(i,k)*Gram(i,m))
          cnt = cnt+1
        end do
      end do
      do j=i+1,N-1
        check2 = check2 + Coefs(cnt,0,4)*Gram(i,j)*Gram(j,j)
        cnt = cnt+1
        do m=j+1,N-1
          check2 = check2 + Coefs(cnt,0,4)*(Gram(i,m)*Gram(j,j)+ &
                 & 2d0*Gram(i,j)*Gram(j,m))
          cnt = cnt+1
        end do
        do k=j+1,N-1
          check2 = check2 + Coefs(cnt,0,4)*(Gram(i,j)*Gram(k,k)+ &
                 & 2d0*Gram(i,k)*Gram(j,k))
          cnt = cnt+1
          do m=k+1,N-1
            check2 = check2 + Coefs(cnt,0,4)*(Gram(i,j)*Gram(k,m)+ &
                   & Gram(i,k)*Gram(j,m)+Gram(i,m)*Gram(j,k))*2d0
            cnt = cnt+1
          end do
        end do
      end do
    end do

    write(*,*) 0,0,0,0
    write(*,*) check1, check2

    ! contraction with g*pi*pj
    do i=1,N-1
      do j=i,N-1

      check1 = 0
      do a=0,3
        signum1 = CalcSignum(a)
        inds4(1) = a
        inds4(2) = a
        do b=0,3
          signum2 = CalcSignum(b)
          inds4(3) = b
          do c=0,3
            signum3 = CalcSignum(c)
            inds4(4) = c

            check1 = check1 + signum1*signum2*signum3* &
                   & MomVec(b,i)*MomVec(c,j)*GetTensorElement(4,TNr(1:35,4),1,inds4)
          end do
        end do
      end do

      check2 = 3d0*Coefs(1,2,4)*Gram(i,j)
      cnt = 1
      do a=1,N-1
        check2 = check2 + Coefs(cnt,1,4)*(8d0*Gram(a,i)*Gram(a,j)+ &
               & Gram(a,a)*Gram(i,j))/4d0
        cnt = cnt+1
        do b=a+1,N-1
          check2 = check2 + Coefs(cnt,1,4)*(4d0*Gram(a,i)*Gram(b,j)+ &
                 & 4d0*Gram(b,i)*Gram(a,j)+Gram(a,b)*Gram(i,j))/2d0
          cnt = cnt+1
        end do
      end do
      cnt = 1
      do a=1,N-1
        check2 = check2 + Coefs(cnt,0,4)*Gram(a,a)*Gram(a,i)*Gram(a,j)/8d0
        cnt = cnt+1
        do d=a+1,N-1
          check2 = check2 + Coefs(cnt,0,4)*(2d0*Gram(a,d)*Gram(a,i)*Gram(a,j)+ &
                 & Gram(a,a)*Gram(a,i)*Gram(d,j)+Gram(a,a)*Gram(d,i)*Gram(a,j))/8d0
          cnt = cnt+1
        end do
        do c=a+1,N-1
          check2 = check2 + Coefs(cnt,0,4)*(2d0*Gram(a,c)*Gram(a,i)*Gram(c,j)+ &
                 & 2d0*Gram(a,c)*Gram(c,i)*Gram(a,j)+Gram(a,a)*Gram(c,i)*Gram(c,j)+ &
                 & Gram(c,c)*Gram(a,i)*Gram(a,j))/8d0
          cnt = cnt+1
          do d=c+1,N-1
            check2 = check2 + Coefs(cnt,0,4)*(2d0*Gram(c,d)*Gram(a,i)*Gram(a,j)+ &
                   & 2d0*Gram(a,c)*Gram(a,i)*Gram(d,j)+2d0*Gram(a,c)*Gram(d,i)*Gram(a,j)+ &
                   & 2d0*Gram(a,d)*Gram(a,i)*Gram(c,j)+2d0*Gram(a,d)*Gram(c,i)*Gram(a,j)+ &
                   & Gram(a,a)*Gram(c,i)*Gram(d,j)+Gram(a,a)*Gram(d,i)*Gram(c,j))/8d0
            cnt = cnt+1
          end do
        end do
        do b=a+1,N-1
          check2 = check2 + Coefs(cnt,0,4)*(2d0*Gram(b,a)*Gram(b,i)*Gram(b,j)+ &
                 & Gram(b,b)*Gram(b,i)*Gram(a,j)+Gram(b,b)*Gram(a,i)*Gram(b,j))/8d0
          cnt = cnt+1
          do d=b+1,N-1
            check2 = check2 + Coefs(cnt,0,4)*(2d0*Gram(a,d)*Gram(b,i)*Gram(b,j)+ &
                   & 2d0*Gram(b,a)*Gram(b,i)*Gram(d,j)+2d0*Gram(b,a)*Gram(d,i)*Gram(b,j)+ &
                   & 2d0*Gram(b,d)*Gram(b,i)*Gram(a,j)+2d0*Gram(b,d)*Gram(a,i)*Gram(b,j)+ &
                   & Gram(b,b)*Gram(a,i)*Gram(d,j)+Gram(b,b)*Gram(d,i)*Gram(a,j))/8d0
            cnt = cnt+1
          end do
          do c=b+1,N-1
            check2 = check2 + Coefs(cnt,0,4)*(2d0*Gram(a,b)*Gram(c,i)*Gram(c,j)+ &
                   & 2d0*Gram(c,a)*Gram(c,i)*Gram(b,j)+2d0*Gram(c,a)*Gram(b,i)*Gram(c,j)+ &
                   & 2d0*Gram(c,b)*Gram(c,i)*Gram(a,j)+2d0*Gram(c,b)*Gram(a,i)*Gram(c,j)+ &
                   & Gram(c,c)*Gram(a,i)*Gram(b,j)+Gram(c,c)*Gram(b,i)*Gram(a,j))/8d0
            cnt = cnt+1
            do d=c+1,N-1
              check2 = check2 + Coefs(cnt,0,4)*(Gram(a,b)*Gram(c,i)*Gram(d,j)+ &
                     & Gram(a,b)*Gram(d,i)*Gram(c,j)+Gram(a,c)*Gram(b,i)*Gram(d,j)+ &
                     & Gram(a,c)*Gram(d,i)*Gram(b,j)+Gram(a,d)*Gram(b,i)*Gram(c,j)+ &
                     & Gram(a,d)*Gram(c,i)*Gram(b,j)+Gram(b,c)*Gram(a,i)*Gram(d,j)+ &
                     & Gram(b,c)*Gram(d,i)*Gram(a,j)+Gram(b,d)*Gram(a,i)*Gram(c,j)+ &
                     & Gram(b,d)*Gram(c,i)*Gram(a,j)+Gram(c,d)*Gram(a,i)*Gram(b,j)+ &
                     & Gram(c,d)*Gram(b,i)*Gram(a,j))/4d0
              cnt = cnt+1
            end do
          end do
        end do        
      end do

      write(*,*) 0,0,i,j
      write(*,*) check1, check2
      end do
    end do
    
    ! contraction with pi*pj*pk*pl
    do i=1,N-1
      do j=i,N-1
        do k=j,N-1
          do m=k,N-1

            check1 = 0
            do a=0,3
              signum1 = CalcSignum(a)
              inds4(1) = a
              do b=0,3
                signum2 = CalcSignum(b)
                inds4(2) = b
                do c=0,3
                  signum3 = CalcSignum(c)
                  inds4(3) = c
                  do d=0,3
                    signum4 = CalcSignum(d)
                    inds4(4) = d

                    check1 = check1 + signum1*signum2*signum3*signum4* &
                         & MomVec(a,i)*MomVec(b,j)*MomVec(c,k)*MomVec(d,m)* &
                         & GetTensorElement(4,TNr(1:35,4),1,inds4)
                  
                  end do
                end do
              end do
            end do


            check2 = Coefs(1,2,4)*(Gram(i,j)*Gram(k,m)+Gram(i,k)*Gram(j,m)+Gram(i,m)*Gram(j,k))/4d0
            cnt = 1
            do a=1,N-1
              check2 = check2 + Coefs(cnt,1,4)*(Gram(i,j)*Gram(a,k)*Gram(a,m)+ &
                 & Gram(i,k)*Gram(a,j)*Gram(a,m)+Gram(i,m)*Gram(a,j)*Gram(a,k)+ &
                 & Gram(j,k)*Gram(a,i)*Gram(a,m)+Gram(j,m)*Gram(a,i)*Gram(a,k)+ &
                 & Gram(k,m)*Gram(a,i)*Gram(a,j))/8d0
              cnt = cnt+1
              do b=a+1,N-1
                check2 = check2 + Coefs(cnt,1,4)*(Gram(i,j)*Gram(a,k)*Gram(b,m)+ &
                   & Gram(i,k)*Gram(a,j)*Gram(b,m)+Gram(i,m)*Gram(a,j)*Gram(b,k)+ &
                   & Gram(j,k)*Gram(a,i)*Gram(b,m)+Gram(j,m)*Gram(a,i)*Gram(b,k)+ &
                   & Gram(k,m)*Gram(a,i)*Gram(b,j)+Gram(i,j)*Gram(b,k)*Gram(a,m)+ &
                   & Gram(i,k)*Gram(b,j)*Gram(a,m)+Gram(i,m)*Gram(b,j)*Gram(a,k)+ &
                   & Gram(j,k)*Gram(b,i)*Gram(a,m)+Gram(j,m)*Gram(b,i)*Gram(a,k)+ &
                   & Gram(k,m)*Gram(b,i)*Gram(a,j))/8d0
                cnt = cnt+1
              end do
            end do
            cnt = 1
            do a=1,N-1
              check2 = check2 + Coefs(cnt,0,4)*Gram(a,i)*Gram(a,j)*Gram(a,k)*Gram(a,m)/16d0
              cnt = cnt+1
              do d=a+1,N-1
                check2 = check2 + Coefs(cnt,0,4)*(Gram(a,i)*Gram(a,j)*Gram(a,k)*Gram(d,m)+ &
                   & Gram(a,i)*Gram(a,j)*Gram(d,k)*Gram(a,m)+Gram(a,i)*Gram(d,j)*Gram(a,k)*Gram(a,m)+ &
                   & Gram(d,i)*Gram(a,j)*Gram(a,k)*Gram(a,m))/16d0
                cnt = cnt+1
              end do
              do c=a+1,N-1
                check2 = check2 + Coefs(cnt,0,4)*(Gram(a,i)*Gram(a,j)*Gram(c,k)*Gram(c,m)+ &
                   & Gram(a,i)*Gram(c,j)*Gram(a,k)*Gram(c,m)+Gram(a,i)*Gram(c,j)*Gram(c,k)*Gram(a,m)+ &
                   & Gram(c,i)*Gram(a,j)*Gram(a,k)*Gram(c,m)+Gram(c,i)*Gram(a,j)*Gram(c,k)*Gram(a,m)+ &
                   & Gram(c,i)*Gram(c,j)*Gram(a,k)*Gram(a,m))/16d0
                cnt = cnt+1
                do d=c+1,N-1
                  check2 = check2 + Coefs(cnt,0,4)*(Gram(a,i)*Gram(a,j)*Gram(c,k)*Gram(d,m)+ &
                     & Gram(a,i)*Gram(a,j)*Gram(c,m)*Gram(d,k)+Gram(a,i)*Gram(c,j)*Gram(a,k)*Gram(d,m)+ &
                     & Gram(a,i)*Gram(d,j)*Gram(a,k)*Gram(c,m)+Gram(a,i)*Gram(c,j)*Gram(d,k)*Gram(a,m)+ &
                     & Gram(a,i)*Gram(d,j)*Gram(c,k)*Gram(a,m)+Gram(c,i)*Gram(a,j)*Gram(d,k)*Gram(a,m)+ &
                     & Gram(c,i)*Gram(a,j)*Gram(a,k)*Gram(d,m)+Gram(c,i)*Gram(d,j)*Gram(a,k)*Gram(a,m)+ &
                     & Gram(d,i)*Gram(a,j)*Gram(c,k)*Gram(a,m)+Gram(d,i)*Gram(a,j)*Gram(a,k)*Gram(c,m)+ &
                     & Gram(d,i)*Gram(c,j)*Gram(a,k)*Gram(a,m))/16d0
                  cnt = cnt+1
                end do
              end do
              do b=a+1,N-1
                check2 = check2 + Coefs(cnt,0,4)*(Gram(a,i)*Gram(b,j)*Gram(b,k)*Gram(b,m)+ &
                   & Gram(b,i)*Gram(a,j)*Gram(b,k)*Gram(b,m)+Gram(b,i)*Gram(b,j)*Gram(a,k)*Gram(b,m)+ &
                   & Gram(b,i)*Gram(b,j)*Gram(b,k)*Gram(a,m))/16d0
                cnt = cnt+1
                do d=b+1,N-1
                  check2 = check2 + Coefs(cnt,0,4)*(Gram(a,i)*Gram(b,j)*Gram(b,k)*Gram(d,m)+ &
                     & Gram(a,i)*Gram(b,j)*Gram(d,k)*Gram(b,m)+Gram(a,i)*Gram(d,j)*Gram(b,k)*Gram(b,m)+ &
                     & Gram(b,i)*Gram(a,j)*Gram(b,k)*Gram(d,m)+Gram(b,i)*Gram(a,j)*Gram(d,k)*Gram(b,m)+ &
                     & Gram(b,i)*Gram(b,j)*Gram(a,k)*Gram(d,m)+Gram(b,i)*Gram(b,j)*Gram(d,k)*Gram(a,m)+ &
                     & Gram(b,i)*Gram(d,j)*Gram(a,k)*Gram(b,m)+Gram(b,i)*Gram(d,j)*Gram(b,k)*Gram(a,m)+ &
                     & Gram(d,i)*Gram(a,j)*Gram(b,k)*Gram(b,m)+Gram(d,i)*Gram(b,j)*Gram(a,k)*Gram(b,m)+ &
                     & Gram(d,i)*Gram(b,j)*Gram(b,k)*Gram(a,m))/16d0
                  cnt = cnt+1
                end do
                do c=b+1,N-1
                  check2 = check2 + Coefs(cnt,0,4)*(Gram(a,i)*Gram(c,j)*Gram(c,k)*Gram(b,m)+ &
                     & Gram(a,i)*Gram(c,j)*Gram(b,k)*Gram(c,m)+Gram(a,i)*Gram(b,j)*Gram(c,k)*Gram(c,m)+ &
                     & Gram(c,i)*Gram(a,j)*Gram(c,k)*Gram(b,m)+Gram(c,i)*Gram(a,j)*Gram(b,k)*Gram(c,m)+ &
                     & Gram(c,i)*Gram(c,j)*Gram(a,k)*Gram(b,m)+Gram(c,i)*Gram(c,j)*Gram(b,k)*Gram(a,m)+ &
                     & Gram(c,i)*Gram(b,j)*Gram(a,k)*Gram(c,m)+Gram(c,i)*Gram(b,j)*Gram(c,k)*Gram(a,m)+ &
                     & Gram(b,i)*Gram(a,j)*Gram(c,k)*Gram(c,m)+Gram(b,i)*Gram(c,j)*Gram(a,k)*Gram(c,m)+ &
                     & Gram(b,i)*Gram(c,j)*Gram(c,k)*Gram(a,m))/16d0
                  cnt = cnt+1
                  do d=c+1,N-1
                    check2 = check2 + Coefs(cnt,0,4)*(Gram(a,i)*Gram(b,j)*Gram(c,k)*Gram(d,m)+ &
                       & Gram(a,i)*Gram(b,j)*Gram(d,k)*Gram(c,m)+Gram(a,i)*Gram(c,j)*Gram(b,k)*Gram(d,m)+ &
                       & Gram(a,i)*Gram(c,j)*Gram(d,k)*Gram(b,m)+Gram(a,i)*Gram(d,j)*Gram(b,k)*Gram(c,m)+ &
                       & Gram(a,i)*Gram(d,j)*Gram(c,k)*Gram(b,m)+Gram(b,i)*Gram(a,j)*Gram(c,k)*Gram(d,m)+ &
                       & Gram(b,i)*Gram(a,j)*Gram(d,k)*Gram(c,m)+Gram(b,i)*Gram(c,j)*Gram(a,k)*Gram(d,m)+ &
                       & Gram(b,i)*Gram(c,j)*Gram(d,k)*Gram(a,m)+Gram(b,i)*Gram(d,j)*Gram(a,k)*Gram(c,m)+ &
                       & Gram(b,i)*Gram(d,j)*Gram(c,k)*Gram(a,m)+Gram(c,i)*Gram(a,j)*Gram(b,k)*Gram(d,m)+ &
                       & Gram(c,i)*Gram(a,j)*Gram(d,k)*Gram(b,m)+Gram(c,i)*Gram(b,j)*Gram(a,k)*Gram(d,m)+ &
                       & Gram(c,i)*Gram(b,j)*Gram(d,k)*Gram(a,m)+Gram(c,i)*Gram(d,j)*Gram(a,k)*Gram(b,m)+ &
                       & Gram(c,i)*Gram(d,j)*Gram(b,k)*Gram(a,m)+Gram(d,i)*Gram(a,j)*Gram(b,k)*Gram(c,m)+ &
                       & Gram(d,i)*Gram(a,j)*Gram(c,k)*Gram(b,m)+Gram(d,i)*Gram(b,j)*Gram(a,k)*Gram(c,m)+ &
                       & Gram(d,i)*Gram(b,j)*Gram(c,k)*Gram(a,m)+Gram(d,i)*Gram(c,j)*Gram(a,k)*Gram(b,m)+ &
                       & Gram(d,i)*Gram(c,j)*Gram(b,k)*Gram(a,m))/16d0
                    cnt = cnt+1
                  end do
                end do
              end do        
            end do

            write(*,*) i,j,k,m
            write(*,*) check1, check2
          end do
        end do
      end do
    end do
    

  end subroutine CheckTensors



  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcSignum(mu)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcSignum(mu)

    integer, intent(in) :: mu
    integer :: CalcSignum

    select case (mu)
      case (:-1)
        write(*,*) 'CalcSignum: Lorentz index out of bounds'
        Stop
      case (0)
        CalcSignum = 1
      case (1:3)
        CalcSignum = -1
      case (4:)
        write(*,*) 'CalcSignum: Lorentz index out of bounds'
        Stop
    end select

  end function CalcSignum


end module Checks
  