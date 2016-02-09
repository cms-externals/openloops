!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ****************************
!  *  module TensorReduction  *
!  *      by Lars Hofer       *
!  ****************************
!
!  functions and subroutines:
!  init_tables, CalcTensorNr, CalcTNrPVco, CalcTNrPVco_nd, CalcTtilde, CalcTgtilde,
!  CalcTensorEr, EreduD0, EreduD1, EreduDr, CalcTensorFr, FreduE0, FreduEr
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



module TensorReduction

  use collier_coefs
  use BuildTensors

  implicit none


contains





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorFr(TF,TFuv,TFerr,MomVec,MomInv,masses2,rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcTensorFr(TF,TFuv,TFerr,MomVec,MomInv,masses2,rmax)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,5), masses2(0:5), MomInv(15)
    double complex, intent(out) :: TF(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TFuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: TFerr(0:rmax)
    double complex, allocatable :: TFaux(:,:,:,:)
    double complex, allocatable :: TFuvaux(:,:,:,:)
    double precision, allocatable :: TFerraux(:)
    double complex :: x(41)
    double complex, allocatable :: fct(:)
    integer :: r,n0,n1,n2,n3,n4,i,rank,bino,cnt
    logical :: nocalc,wrica


    if (tenred_cll.gt.6) then
      if (erroutlev_cll.ge.1) then
        write(nerrout_cll,*) 'inconsistent call of CalcTensorFr'
      end if
      stop 
    endif

    if (use_cache_system) then
      if ((ncache.gt.0).and.(ncache.le.ncache_max)) then
!        if (use_cache(ncache).ge.6) then
          x(1:15) = MomInv
          x(16:21) = masses2
          do i=1,5
            x(22+(i-1)*4:21+i*4) = MomVec(0:3,i)
          end do   
          rank = rmax

          allocate(fct(2*RtS(rmax)))
          call ReadCache(fct,2*RtS(rmax)+rmax+1,x,41,10+mode_cll,0,6,rank,nocalc,wrica)
    
          if(nocalc)then
            cnt = 1
            do r=0,rmax
              do n0=0,r
                do n1=0,r-n0
                  do n2=0,r-n0-n1
                    n3 = r-n0-n1-n2
                    TF(n0,n1,n2,n3) = fct(cnt)
                    cnt = cnt+1
                    TFuv(n0,n1,n2,n3) = fct(cnt)
                    cnt = cnt+1
                  end do
                end do
              end do
              TFerr(r) = real(fct(cnt))
              cnt = cnt+1
            end do     
            return
          endif


          allocate(TFaux(0:rank,0:rank,0:rank,0:rank))
          allocate(TFuvaux(0:rank,0:rank,0:rank,0:rank))
          allocate(TFerraux(0:rank))

          call CalcTensorFrRed(TFaux,TFuvaux,TFerraux,MomVec,MomInv,masses2,rank)
          

          if (wrica) then
            cnt = 0
            if (rank.ne.rmax) then
              deallocate(fct)
              allocate(fct(2*RtS(rank)))
            end if
            do r=0,rank
              do n0=0,r
                do n1=0,r-n0
                  do n2=0,r-n0-n1
                    n3 = r-n0-n1-n2
 
                    cnt = cnt+1
                    fct(cnt) = TFaux(n0,n1,n2,n3)
                    cnt = cnt+1
                    fct(cnt) = TFuvaux(n0,n1,n2,n3)

                  end do
                end do
              end do
              cnt = cnt+1
              fct(cnt) = TFerraux(r)
            end do
   
            call WriteCache(fct,2*RtS(rank)+rank+1,0,6,rank)

          end if

          TF = TFaux(0:rmax,0:rmax,0:rmax,0:rmax)
          TFuv = TFuvaux(0:rmax,0:rmax,0:rmax,0:rmax)
          TFerr = TFerraux(0:rmax)
          return

!        end if
      end if
    end if


    call CalcTensorFrRed(TF,TFuv,TFerr,MomVec,MomInv,masses2,rmax)


  end subroutine CalcTensorFr




  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorFrRed(TF,TFuv,TFerr,MomVec,MomInv,masses2,rmax)
  !
  !  calculate tensor integral TFr for 6-point functions by direct reduction 
  !  from the tensor integrals TEr for 5-point functions.
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcTensorFrRed(TF,TFuv,TFerr,MomVec,MomInv,masses2,rmax)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,5), masses2(0:5), MomInv(15)
    double complex, intent(out) :: TF(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TFuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: TFerr(0:rmax)
    double complex, allocatable :: TE(:,:,:,:,:), TE_0aux(:,:,:,:)
    double complex, allocatable :: TEuv(:,:,:,:,:), TEuv_0aux(:,:,:,:)
    double precision, allocatable :: TEerr(:,:)
    double precision :: p1max
    double complex, allocatable :: CE(:,:,:,:,:), CEuv(:,:,:,:,:)
    double precision, allocatable :: CEerr(:) 
    double complex :: MomVecE(0:3,4), masses2E(0:4), MomInvE(10)
    double complex :: q10,q21,q32,q43,q54,q50,q20,q31,q42,q53,q40
    double complex :: q51,q30,q41,q52,mm02,mm12,mm22,mm32,mm42,mm52
    double complex :: mx(0:5,0:5), mxinv(0:5,0:5),mx0k(5,5),mx0kinv(5,5),mxinvP(5,0:3)
    double complex :: mxinvs,mxinvPs(0:3)
    double complex :: det,newdet
    double complex :: Smod, Faux(5), elimminf2_coli,chdet,P1ten
    integer :: r,n0,n1,n2,n3,n4,np0,np1,np2,np3,k,i,j,kbest,rmaxE,mu,combi,nid(0:4),bin


    ! determine inverse modified Caley matrix
    mm02 = elimminf2_coli(masses2(0))
    mm12 = elimminf2_coli(masses2(1))
    mm22 = elimminf2_coli(masses2(2))
    mm32 = elimminf2_coli(masses2(3))
    mm42 = elimminf2_coli(masses2(4))
    mm52 = elimminf2_coli(masses2(5))
    q10  = elimminf2_coli(MomInv(1))
    q21  = elimminf2_coli(MomInv(2))
    q32  = elimminf2_coli(MomInv(3))
    q43  = elimminf2_coli(MomInv(4))
    q54  = elimminf2_coli(MomInv(5))
    q50  = elimminf2_coli(MomInv(6))
    q20  = elimminf2_coli(MomInv(7))
    q31  = elimminf2_coli(MomInv(8))
    q42  = elimminf2_coli(MomInv(9))
    q53  = elimminf2_coli(MomInv(10))
    q40  = elimminf2_coli(MomInv(11))
    q51  = elimminf2_coli(MomInv(12))
    q30  = elimminf2_coli(MomInv(13))
    q41  = elimminf2_coli(MomInv(14))
    q52  = elimminf2_coli(MomInv(15))

 
    mx(0,0) = 2d0*mm02
    mx(1,0) = q10 - mm12 + mm02
    mx(2,0) = q20 - mm22 + mm02
    mx(3,0) = q30 - mm32 + mm02
    mx(4,0) = q40 - mm42 + mm02
    mx(5,0) = q50 - mm52 + mm02
    mx(0,1) = mx(1,0)
    mx(1,1) = 2d0*q10
    mx(2,1) = q10+q20-q21
    mx(3,1) = q10+q30-q31
    mx(4,1) = q10+q40-q41
    mx(5,1) = q10+q50-q51
    mx(0,2) = mx(2,0)
    mx(1,2) = mx(2,1)
    mx(2,2) = 2d0*q20
    mx(3,2) = q20+q30-q32
    mx(4,2) = q20+q40-q42
    mx(5,2) = q20+q50-q52
    mx(0,3) = mx(3,0)
    mx(1,3) = mx(3,1)
    mx(2,3) = mx(3,2)
    mx(3,3) = 2d0*q30
    mx(4,3) = q30+q40-q43
    mx(5,3) = q30+q50-q53
    mx(0,4) = mx(4,0)
    mx(1,4) = mx(4,1)
    mx(2,4) = mx(4,2)
    mx(3,4) = mx(4,3)
    mx(4,4) = 2d0*q40
    mx(5,4) = q40+q50-q54
    mx(0,5) = mx(5,0)
    mx(1,5) = mx(5,1)
    mx(2,5) = mx(5,2)
    mx(3,5) = mx(5,3)
    mx(4,5) = mx(5,4)
    mx(5,5) = 2d0*q50

    call chinv(6,mx,mxinv)


    ! determine X_(0,5)
    do j=1,5
      do i=1,5
        mx0k(i,j) = mx(i,j-1)
      end do
    end do

    det = chdet(5,mx0k)
    kbest = 5

    do j=5,2,-1
      do i=1,5
        mx0k(i,j) = mx(i,j)
      end do

      newdet =  chdet(5,mx0k)
      if (abs(newdet).gt.abs(det)) then          
        kbest = j-1
        det = newdet
      end if
    
    end do
    
    do i=1,5
      mx0k(i,1) = mx(i,1)
      mx0k(i,kbest) = mx(i,0)
    end do

    call chinv(5,mx0k,mx0kinv)
    do i=1,5
      mx0kinv(kbest,i) = 0d0
    end do
    
    mxinvs = sum(mxinv(0:5,0))

    ! build rank5 tensors
    rmaxE = max(rmax-1,0)
    allocate(CE(0:rmaxE/2,0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE))
    allocate(CEuv(0:rmaxE/2,0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE))
    allocate(CEerr(0:rmaxE))
    allocate(TE(0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE,0:5))
    allocate(TE_0aux(0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE))
    allocate(TEuv(0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE,0:5))
    allocate(TEuv_0aux(0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE))
    allocate(TEerr(0:5,0:rmaxE))
    
    MomInvE = SubMomInv(6,0,MomInv)
    masses2E = SubMasses(6,0,masses2)
    call E_main_cll(CE,CEuv,MomInvE(1),MomInvE(2),MomInvE(3),MomInvE(4),MomInvE(5),MomInvE(6), &
                            MomInvE(7),MomInvE(8),MomInvE(9),MomInvE(10),masses2E(0),masses2E(1), &
                            masses2E(2),masses2E(3),masses2E(4),rmaxE,Eerr2=CEerr,id_in=1)                           
    call CalcTensorE(TE_0aux(:,:,:,:),TEuv_0aux(:,:,:,:),TEerr(0,0:),  &
                     CE,CEuv,CEerr,SubMomVec(6,0,MomVec),rmaxE)

    ! shift of integration momentum in TE\{0}
    TE(:,:,:,:,0) = 0d0
    TEuv(:,:,:,:,0) = 0d0

    do r=0,rmaxE

      do np0=0,r
        do np1=0,r-np0
          do np2=0,r-np0-np1
            do np3=0,r-np0-np1-np2

              P1ten = (-MomVec(0,1))**np0*(-MomVec(1,1))**np1*  &
                      (-MomVec(2,1))**np2*(-MomVec(3,1))**np3

              do n0=0,r-np0-np1-np2-np3
                do n1=0,r-np0-np1-np2-np3-n0
                  do n2=0,r-np0-np1-np2-np3-n0-n1
                    n3 = r-np0-np1-np2-np3-n0-n1-n2

                    combi = BinomTable(n0,n0+np0)*BinomTable(n1,n1+np1)*  &
                            BinomTable(n2,n2+np2)*BinomTable(n3,n3+np3)

                    TE(np0+n0,np1+n1,np2+n2,np3+n3,0) = TE(np0+n0,np1+n1,np2+n2,np3+n3,0)  &
                        + combi*TE_0aux(n0,n1,n2,n3)*P1ten

                  end do
                end do
              end do
            
            end do
          end do 
        end do
      end do

    end do

    p1max = maxval(abs(MomVec(0:3,1)))
    do r=1,rmaxE
      do i=1,r
        TEerr(0,r) = max(TEerr(0,i),TEerr(0,r-i)*p1max**i)
      end do
    end do


    do i=1,5
      MomInvE = SubMomInv(6,i,MomInv)
      masses2E = SubMasses(6,i,masses2)
      call E_main_cll(CE,CEuv,MomInvE(1),MomInvE(2),MomInvE(3),MomInvE(4),MomInvE(5),MomInvE(6), &
                              MomInvE(7),MomInvE(8),MomInvE(9),MomInvE(10),masses2E(0),masses2E(1), &
                              masses2E(2),masses2E(3),masses2E(4),rmaxE,Eerr2=CEerr,id_in=2**i)
      call CalcTensorE(TE(:,:,:,:,i),TEuv(:,:,:,:,i),TEerr(i,0:),  &
                       CE,CEuv,CEerr,SubMomVec(6,i,MomVec),rmaxE)
    end do

    
    TF = 0d0
    TF(0,0,0,0) = -mxinv(0,0)*TE(0,0,0,0,0)
    do i=1,5
      TF(0,0,0,0) = TF(0,0,0,0) + mxinv(i,0)*(TE(0,0,0,0,i)-TE(0,0,0,0,0))
    end do
    TFerr(0) = max( abs(mxinvs)*TEerr(0,0), &
                    abs(mxinv(1,0))*TEerr(1,0) , &
                    abs(mxinv(2,0))*TEerr(2,0) , &
                    abs(mxinv(3,0))*TEerr(3,0) , &
                    abs(mxinv(4,0))*TEerr(4,0) , &
                    abs(mxinv(5,0))*TEerr(5,0) )

    do mu=0,3
      do i=1,5
        mxinvP(i,mu)=0d0
        do j=1,5
          mxinvP(i,mu) = mxinvP(i,mu) + mx0kinv(j,i)*MomVec(mu,j)
        end do
      end do
      mxinvPs(mu) = sum(mxinvP(1:5,mu))
    end do

    do r=1,rmax    
      do n0=0,r
        do n1=0,r-n0
          do n2=0,r-n0-n1
            n3 = r-n0-n1-n2

            if (n0.ge.1) then
              do i=1,5
                Smod = TE(n0-1,n1,n2,n3,i) - TE(n0-1,n1,n2,n3,0)
                TF(n0,n1,n2,n3) = TF(n0,n1,n2,n3) + mxinvP(i,0)*Smod
              end do
            else if (n1.ge.1) then
              do i=1,5
                Smod = TE(n0,n1-1,n2,n3,i) - TE(n0,n1-1,n2,n3,0)
                TF(n0,n1,n2,n3) = TF(n0,n1,n2,n3) + mxinvP(i,1)*Smod
              end do
            else if (n2.ge.1) then
              do i=1,5
                Smod = TE(n0,n1,n2-1,n3,i) - TE(n0,n1,n2-1,n3,0)
                TF(n0,n1,n2,n3) = TF(n0,n1,n2,n3) + mxinvP(i,2)*Smod
              end do
            else
              do i=1,5
                Smod = TE(n0,n1,n2,n3-1,i) - TE(n0,n1,n2,n3-1,0)
                TF(n0,n1,n2,n3) = TF(n0,n1,n2,n3) + mxinvP(i,3)*Smod
              end do
            end if

          end do
        end do
      end do

      TFerr(r) = max(maxval(abs(mxinvPs(0:3)))*TEerr(0,r-1), &
                     maxval(abs(mxinvP(1,0:3)))*TEerr(1,r-1), &
                     maxval(abs(mxinvP(2,0:3)))*TEerr(2,r-1), &
                     maxval(abs(mxinvP(3,0:3)))*TEerr(3,r-1))

    end do
  
  end subroutine CalcTensorFrRed





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorFr_list(TF,TFuv,TFerr,MomVec,MomInv,masses2,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorFr_list(TF,TFuv,TFerr,MomVec,MomInv,masses2,rmax)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,5), masses2(0:5), MomInv(15)
    double complex, intent(out) :: TF(RtS(rmax)), TFuv(RtS(rmax))
    double precision, intent(out) :: TFerr(0:rmax)
    double complex :: TF_aux(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: TFuv_aux(0:rmax,0:rmax,0:rmax,0:rmax)
    integer :: mu

    call CalcTensorFr(TF_aux,TFuv_aux,TFerr,MomVec,MomInv,masses2,rmax)

    do mu=1,RtS(rmax)
      TF(mu) = TF_aux(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu))
    end do

    if (calcUV_cll) then
      do mu=1,RtS(rmax)
        TFuv(mu) = TFuv_aux(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu))
      end do
    end if

  end subroutine CalcTensorFr_list





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorTNr(TN,TNuv,TNerr,MomVec,MomInv,masses2,rmax,id)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  recursive subroutine CalcTensorTNr(TN,TNuv,TNerr,MomVec,MomInv,masses2,N,rmax,id)
  
    integer, intent(in) :: N,rmax,id
    double complex, intent(in) :: MomVec(0:3,N-1), masses2(0:N-1)
    double complex, intent(in) :: MomInv(BinomTable(2,N))
    double complex, intent(out) :: TN(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TNuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: TNerr(0:rmax)
    double complex, allocatable :: TNaux(:,:,:,:)
    double complex, allocatable :: TNuvaux(:,:,:,:)
    double precision, allocatable :: TNerraux(:),CNerr(:)
    double complex :: x(BinomTable(2,N)+N+4*(N-1))
    double complex, allocatable :: fct(:),CN(:),CNuv(:)
    integer :: r,n0,n1,n2,n3,n4,i,rank,bino,cnt
    logical :: nocalc,wrica

    if ((use_cache_system).and.(N.ge.tencache)) then
      if ((ncache.gt.0).and.(ncache.le.ncache_max)) then
!        if (use_cache(ncache).ge.N) then
          do i=1,N-1
            x((i-1)*4+1:i*4) = MomVec(0:3,i)
          end do 
          bino = BinomTable(2,N)
          x(4*N-3:bino+4*N-4) = MomInv
          x(4*N-3+bino:bino+5*N-4) = masses2  
          rank = rmax

          allocate(fct(2*RtS(rmax)+rmax+1))
          call ReadCache(fct,2*RtS(rmax)+rmax+1,x,bino+5*N-4,mode_cll+10,id,N,rank,nocalc,wrica)
  
          if(nocalc)then
            cnt = 1
            do r=0,rmax
              do n0=0,r
                do n1=0,r-n0
                  do n2=0,r-n0-n1
                    n3 = r-n0-n1-n2
                    TN(n0,n1,n2,n3) = fct(cnt)
                    cnt = cnt+1
                    TNuv(n0,n1,n2,n3) = fct(cnt)
                    cnt = cnt+1
                  end do
                end do
              end do
              TNerr(r) = real(fct(cnt))
              cnt = cnt+1
            end do     
            return
          endif


          allocate(TNaux(0:rank,0:rank,0:rank,0:rank))
          allocate(TNuvaux(0:rank,0:rank,0:rank,0:rank))
          allocate(TNerraux(0:rank))

          if (N.eq.tenred_cll-1) then
            allocate(CN(NCoefs(rank,N)))
            allocate(CNuv(NCoefs(rank,N)))
            allocate(CNerr(0:rank))
            call TN_cll(CN,CNuv,MomInv,masses2,N,rank,TNerr2=CNerr,id_in=id)
            call CalcTensorTN(TNaux,TNuvaux,TNerraux,CN,CNuv,CNerr,MomVec,N,rank)
          else
            call CalcTensorTNrRed(TNaux,TNuvaux,TNerraux,MomVec,MomInv,masses2,N,rank,id)
          end if

          if (wrica) then
            cnt = 0
            if (rank.ne.rmax) then
              deallocate(fct)
              allocate(fct(2*RtS(rank)+rank+1))
            end if
            do r=0,rank
              do n0=0,r
                do n1=0,r-n0
                  do n2=0,r-n0-n1
                    n3 = r-n0-n1-n2
 
                    cnt = cnt+1
                    fct(cnt) = TNaux(n0,n1,n2,n3)
                    cnt = cnt+1
                    fct(cnt) = TNuvaux(n0,n1,n2,n3)

                  end do
                end do
              end do

              cnt = cnt+1
              fct(cnt) = TNerraux(r)

            end do
   
            call WriteCache(fct,2*RtS(rank)+rank+1,id,N,rank)

          end if

          TN = TNaux(0:rmax,0:rmax,0:rmax,0:rmax)
          TNuv = TNuvaux(0:rmax,0:rmax,0:rmax,0:rmax)
          TNerr = TNerraux(0:rmax)
          return

!        end if
      end if
    end if

    if (N.le.tenred_cll-1) then
      allocate(CN(NCoefs(rmax,N)))
      allocate(CNuv(NCoefs(rmax,N)))
      allocate(CNerr(0:rmax))
      call TN_cll(CN,CNuv,MomInv,masses2,N,rmax,TNerr2=CNerr,id_in=id)
      call CalcTensorTN(TN,TNuv,TNerr,CN,CNuv,CNerr,MomVec,N,rmax)
    else
      call CalcTensorTNrRed(TN,TNuv,TNerr,MomVec,MomInv,masses2,N,rmax,id)
    end if


  end subroutine CalcTensorTNr





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorTNrRed(TN,TNuv,TNerr,MomVec,MomInv,masses2,rmax,id)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  recursive subroutine CalcTensorTNrRed(TN,TNuv,TNerr,MomVec,MomInv,masses2,N,rmax,id)
  
    integer, intent(in) :: N,rmax,id
    double complex, intent(in) :: MomVec(0:3,N-1), masses2(0:N-1)
    double complex, intent(in) :: MomInv(BinomTable(2,N))
    double complex, intent(out) :: TN(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TNuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: TNerr(0:rmax)
    double complex, allocatable :: TNm1(:,:,:,:,:),TNm1_0aux(:,:,:,:)
    double complex, allocatable :: TNm1uv(:,:,:,:,:),TNm1uv_0aux(:,:,:,:)
    double precision, allocatable :: TNm1err(:,:)
    double precision :: p1max
    double complex :: q10,q21,q32,q43,q54,q50,q20,q31,q42,q53,q40
    double complex :: q51,q30,q41,q52,mm02,mm12,mm22,mm32,mm42,mm52
    double complex :: mx(0:5,0:5), mxinv(0:5,0:5),mx0k(5,5),mx0kinv(5,5)
    double complex :: det,newdet
    double complex :: Smod, elimminf2_coli,chdet,P1ten,mxinvP(5,0:3)
    double complex :: mxinvs,mxinvPs(0:3)
    integer :: r,n0,n1,n2,n3,n4,np0,np1,np2,np3,k,i,j,kbest,rmax_m1,combi,mu
    integer :: bin,nid(0:5),r0,rBCD,mia


    ! determine inverse modified Caley matrix
    mm02 = elimminf2_coli(masses2(0))
    mm12 = elimminf2_coli(masses2(1))
    mm22 = elimminf2_coli(masses2(2))
    mm32 = elimminf2_coli(masses2(3))
    mm42 = elimminf2_coli(masses2(4))
    mm52 = elimminf2_coli(masses2(5))
    q10  = elimminf2_coli(MomInv(1))
    q21  = elimminf2_coli(MomInv(2))
    q32  = elimminf2_coli(MomInv(3))
    q43  = elimminf2_coli(MomInv(4))
    q54  = elimminf2_coli(MomInv(5))
    if (N.le.9) then
      q50  = elimminf2_coli(MomInv((N-6)*N+6))
    else
      q50 = elimminf2_coli(MomInv(4*N+1))
    end if
    q20  = elimminf2_coli(MomInv(N+1))
    q31  = elimminf2_coli(MomInv(N+2))
    q42  = elimminf2_coli(MomInv(N+3))
    q53  = elimminf2_coli(MomInv(N+4))
    if (N.le.7) then
      q40 = elimminf2_coli(MomInv((N-5)*N+5))
      q51 = elimminf2_coli(MomInv((N-5)*N+6))
    else
      q40 = elimminf2_coli(MomInv(3*N+1))
      q51 = elimminf2_coli(MomInv(3*N+2))
    end if
    q30  = elimminf2_coli(MomInv(2*N+1))
    q41  = elimminf2_coli(MomInv(2*N+2))
    q52  = elimminf2_coli(MomInv(2*N+3))

 
    mx(0,0) = 2d0*mm02
    mx(1,0) = q10 - mm12 + mm02
    mx(2,0) = q20 - mm22 + mm02
    mx(3,0) = q30 - mm32 + mm02
    mx(4,0) = q40 - mm42 + mm02
    mx(5,0) = q50 - mm52 + mm02
    mx(0,1) = mx(1,0)
    mx(1,1) = 2d0*q10
    mx(2,1) = q10+q20-q21
    mx(3,1) = q10+q30-q31
    mx(4,1) = q10+q40-q41
    mx(5,1) = q10+q50-q51
    mx(0,2) = mx(2,0)
    mx(1,2) = mx(2,1)
    mx(2,2) = 2d0*q20
    mx(3,2) = q20+q30-q32
    mx(4,2) = q20+q40-q42
    mx(5,2) = q20+q50-q52
    mx(0,3) = mx(3,0)
    mx(1,3) = mx(3,1)
    mx(2,3) = mx(3,2)
    mx(3,3) = 2d0*q30
    mx(4,3) = q30+q40-q43
    mx(5,3) = q30+q50-q53
    mx(0,4) = mx(4,0)
    mx(1,4) = mx(4,1)
    mx(2,4) = mx(4,2)
    mx(3,4) = mx(4,3)
    mx(4,4) = 2d0*q40
    mx(5,4) = q40+q50-q54
    mx(0,5) = mx(5,0)
    mx(1,5) = mx(5,1)
    mx(2,5) = mx(5,2)
    mx(3,5) = mx(5,3)
    mx(4,5) = mx(5,4)
    mx(5,5) = 2d0*q50

    call chinv(6,mx,mxinv)

    ! determine X_(0,5)
    do j=1,5
      do i=1,5
        mx0k(i,j) = mx(i,j-1)
      end do
    end do

    det = chdet(5,mx0k)
    kbest = 5

    do j=5,2,-1
      do i=1,5
        mx0k(i,j) = mx(i,j)
      end do

      newdet =  chdet(5,mx0k)
      if (abs(newdet).gt.abs(det)) then          
        kbest = j-1
        det = newdet
      end if
    
    end do
    
    do i=1,5
      mx0k(i,1) = mx(i,1)
      mx0k(i,kbest) = mx(i,0)
    end do

    call chinv(5,mx0k,mx0kinv)
    do i=1,5
      mx0kinv(kbest,i) = 0d0
    end do
    
    mxinvs = sum(mxinv(0:5,0))

    ! determine binaries for F-coefficients
    k=0
    bin = 1
    do while (k.le.5)
      if (mod(id/bin,2).eq.0) then
        nid(k) = id+bin
        k = k+1
      end if
      bin = 2*bin
    end do

    rmax_m1 = max(rmax-1,0)
    allocate(TNm1(0:rmax_m1,0:rmax_m1,0:rmax_m1,0:rmax_m1,0:5))
    allocate(TNm1_0aux(0:rmax_m1,0:rmax_m1,0:rmax_m1,0:rmax_m1))
    allocate(TNm1uv(0:rmax_m1,0:rmax_m1,0:rmax_m1,0:rmax_m1,0:5))
    allocate(TNm1uv_0aux(0:rmax_m1,0:rmax_m1,0:rmax_m1,0:rmax_m1))
    allocate(TNm1err(0:5,0:rmax_m1))
    
    call CalcTensorTNr(TNm1_0aux(:,:,:,:),TNm1uv_0aux(:,:,:,:),TNm1err(0,0:),SubMomVec(N,0,MomVec),  &
                       SubMomInv(N,0,MomInv),SubMasses(N,0,masses2),N-1,rmax_m1,nid(0))
      
    ! shift of integration momentum in TE\{0}
    TNm1(:,:,:,:,0) = 0d0

    do r=0,rmax_m1

      do np0=0,r
        do np1=0,r-np0
          do np2=0,r-np0-np1
            do np3=0,r-np0-np1-np2

              P1ten = (-MomVec(0,1))**np0*(-MomVec(1,1))**np1*  &
                      (-MomVec(2,1))**np2*(-MomVec(3,1))**np3

              do n0=0,r-np0-np1-np2-np3
                do n1=0,r-np0-np1-np2-np3-n0
                  do n2=0,r-np0-np1-np2-np3-n0-n1
                    n3 = r-np0-np1-np2-np3-n0-n1-n2

                    combi = BinomTable(n0,n0+np0)*BinomTable(n1,n1+np1)*  &
                            BinomTable(n2,n2+np2)*BinomTable(n3,n3+np3)

                    TNm1(np0+n0,np1+n1,np2+n2,np3+n3,0) = TNm1(np0+n0,np1+n1,np2+n2,np3+n3,0)  &
                        + combi*TNm1_0aux(n0,n1,n2,n3)*P1ten

                  end do
                end do
              end do

            end do
          end do 
        end do
      end do

    end do


    p1max = maxval(abs(MomVec(0:3,1)))
    do r=1,rmax_m1
      do i=1,r
        TNm1err(0,r) = max(TNm1err(0,i),TNm1err(0,r-i)*p1max**i)
      end do
    end do

    do i=1,5    
      call CalcTensorTNr(TNm1(:,:,:,:,i),TNm1uv(:,:,:,:,i),TNm1err(i,0:),SubMomVec(N,i,MomVec),  &
                         SubMomInv(N,i,MomInv),SubMasses(N,i,masses2),N-1,rmax_m1,nid(i))
    end do    

    
    TN = 0d0
    TN(0,0,0,0) = -mxinv(0,0)*TNm1(0,0,0,0,0)
    do i=1,5
      TN(0,0,0,0) = TN(0,0,0,0) + mxinv(i,0)*(TNm1(0,0,0,0,i)-TNm1(0,0,0,0,0))
    end do
    TNerr(0) = max( abs(mxinvs)*TNm1err(0,0), &
                    abs(mxinv(1,0))*TNm1err(1,0) , &
                    abs(mxinv(2,0))*TNm1err(2,0) , &
                    abs(mxinv(3,0))*TNm1err(3,0) , &
                    abs(mxinv(4,0))*TNm1err(4,0) , &
                    abs(mxinv(5,0))*TNm1err(5,0) )

    do mu=0,3
      do i=1,5
        mxinvP(i,mu)=0d0
        do j=1,5
          mxinvP(i,mu) = mxinvP(i,mu) + mx0kinv(j,i)*MomVec(mu,j)
        end do
      end do
      mxinvPs(mu) = sum(mxinvP(1:5,mu))
    end do

    do r=1,rmax    
      do n0=0,r
        do n1=0,r-n0
          do n2=0,r-n0-n1
            n3 = r-n0-n1-n2

            if (n0.ge.1) then
              do i=1,5
                Smod = TNm1(n0-1,n1,n2,n3,i) - TNm1(n0-1,n1,n2,n3,0)
                TN(n0,n1,n2,n3) = TN(n0,n1,n2,n3) + mxinvP(i,0)*Smod
              end do
            else if (n1.ge.1) then
              do i=1,5
                Smod = TNm1(n0,n1-1,n2,n3,i) - TNm1(n0,n1-1,n2,n3,0)
                TN(n0,n1,n2,n3) = TN(n0,n1,n2,n3) + mxinvP(i,1)*Smod
              end do
            else if (n2.ge.1) then
              do i=1,5
                Smod = TNm1(n0,n1,n2-1,n3,i) - TNm1(n0,n1,n2-1,n3,0)
                TN(n0,n1,n2,n3) = TN(n0,n1,n2,n3) + mxinvP(i,2)*Smod
              end do
            else
              do i=1,5
                Smod = TNm1(n0,n1,n2,n3-1,i) - TNm1(n0,n1,n2,n3-1,0)
                TN(n0,n1,n2,n3) = TN(n0,n1,n2,n3) + mxinvP(i,3)*Smod
              end do
            end if

          end do
        end do
      end do

      TNerr(r) = max(maxval(abs(mxinvPs(0:3)))*TNm1err(0,r-1), &
                     maxval(abs(mxinvP(1,0:3)))*TNm1err(1,r-1), &
                     maxval(abs(mxinvP(2,0:3)))*TNm1err(2,r-1), &
                     maxval(abs(mxinvP(3,0:3)))*TNm1err(3,r-1))

    end do

  
  end subroutine CalcTensorTNrRed





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorTNr_list(TN,TNuv,TNerr,MomVec,MomInv,masses2,rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcTensorTNr_list(TN,TNuv,TNerr,MomVec,MomInv,masses2,N,rmax)
  
    integer, intent(in) :: N,rmax
    double complex, intent(in) :: MomVec(0:3,N-1), masses2(0:N-1)
    double complex, intent(in) :: MomInv(BinomTable(2,N))
    double complex, intent(out) :: TN(RtS(rmax)),TNuv(RtS(rmax))
    double precision, intent(out) :: TNerr(0:rmax)
    double complex :: TN_aux(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: TNuv_aux(0:rmax,0:rmax,0:rmax,0:rmax)
    integer :: mu

    call CalcTensorTNr(TN_aux,TNuv_aux,TNerr,MomVec,MomInv,masses2,N,rmax,0)

    do mu=1,RtS(rmax)
      TN(mu) = TN_aux(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu))
    end do

    if (calcUV_cll) then
      do mu=1,RtS(rmax)
        TNuv(mu) = TNuv_aux(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu))
      end do
    end if

  end subroutine CalcTensorTNr_list





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CheckTensors(TN,MomInv,N,rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CheckTensors(TN,MomVec,MomInv,masses2,N,rmax,writeout,acc)

    integer, intent(in) :: N,rmax,writeout
    double precision, intent(in) :: acc
    double complex, intent(in) :: TN(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: MomVec(0:3,N-1), MomInv(BinomTable(2,N)), masses2(0:N-1)
    double complex :: CoefsN(BinomTable(rmax,N+rmax-2),0:rmax/2,0:rmax)
    double complex :: CoefsNuv(BinomTable(rmax,N+rmax-2),0:rmax/2,0:rmax)
    double complex :: check1, check2, fac, Gram(N-1,N-1)
    integer, allocatable :: pinds(:),loinds(:)
    integer :: struc1(0:N-1), struc2(0:N-1)
    integer :: r,n0,i,j,mu,mu0,sgn,tinds(0:3),cnt,nn0,ii
    double precision :: TNerr(0:rmax),TNerr2(0:rmax)

    call CalcTN(CoefsN,CoefsNuv,MomInv,masses2,N,rmax,0,TNerr,TNerr2)


    ! determine Gram matrix
    cnt = 1
    do i=1,N-1
      Gram(i,i) = MomInv(cnt)
      cnt = cnt+N-i
    end do

    cnt = 2
    do i=1,N-1
      do j=i+1,N-1
        Gram(i,j) = (Gram(i,i)+Gram(j,j)-MomInv(cnt))/2d0
        Gram(j,i) = Gram(i,j)
        cnt = cnt+1
      end do
      cnt = cnt+1
    end do


    do r=1,rmax
        
      if (allocated(loinds)) then
        deallocate(loinds)
      end if
      allocate(loinds(r))

      do n0=r/2,0,-1
        
        if (allocated(pinds)) then
          deallocate(pinds)
        end if
        allocate(pinds(r-2*n0))

        do i=1,BinomTable(r-2*n0,r-2*n0+N-2)
          if (r.gt.2*n0) then
            pinds = IndCombisEq(1:r-2*n0,i,r-2*n0,N-1)
          end if

          struc1(1:N-1) = CalcCIndArr(N-1,r-2*n0,i)
          ! contract TN(r) with g(n0)*MomVec(pinds)
          ! --> check1
          check1 = 0d0

          loinds = 0
          do mu=0,4**r-1

            mu0 = mu
            tinds = 0
            do j=1,r
              loinds(j) = mod(mu0,4)
              tinds(loinds(j)) = tinds(loinds(j))+1
              mu0 = mu0/4
            end do

            ! sign of contraction from Minkowski metric
            sgn=(-1)**(r-tinds(0))
            do j=1,n0
              select case (loinds(2*j)-loinds(2*j-1))
                case (0)
                  if (loinds(2*j).ne.0) then
                    sgn = -sgn
                  end if
                case default
                  sgn = 0
              end select
            end do

            if (sgn.ne.0) then

              fac = 1d0
              do j=1,r-2*n0
                fac = fac*MomVec(loinds(2*n0+j),pinds(j))
              end do

              check1 = check1 + sgn*fac*TN(tinds(0),tinds(1),tinds(2),tinds(3))

            end if      
          
          end do

          
          ! calculate contraction directly from coefficients
          ! --> check2
          check2 = 0d0

          struc1(0) = n0
          struc1(1:N-1) = CalcCIndArr(N-1,r-2*n0,i)

          do nn0=r/2,0,-1
            do ii=1,BinomTable(r-2*nn0,r-2*nn0+N-2)

              struc2(0) = nn0
              struc2(1:N-1) = CalcCIndArr(N-1,r-2*nn0,ii)

              check2 = check2 + CoefsN(ii,nn0,r)*  &
                       ContractLoStruc(N-1,struc1,struc2,Gram)

            end do
          end do

          select case (writeout)

            case(1)
              if (abs(check1-check2)/abs(check2).gt.acc) then
                write(ninfout_cll,*) struc1
                write(ninfout_cll,*) 'check1:', check1
                write(ninfout_cll,*) 'check2:', check2
              end if

            case(2)
                write(ninfout_cll,*) struc1
                write(ninfout_cll,*) 'check1:', check1
                write(ninfout_cll,*) 'check2:', check2

          end select

        end do
      end do
    end do

  end subroutine CheckTensors





!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  !  function ContractLoStruc(Nm1,struc1,struc2,Gram)
!  !
!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  recursive function ContractLoStruc(Nm1,struc1,struc2,Gram) result(res)
!
!    integer, intent(in) :: Nm1
!    integer, intent(in) :: struc1(0:Nm1), Struc2(0:Nm1)
!    integer :: struc1aux(0:Nm1), struc2aux(0:Nm1), struc2aux2(0:Nm1)
!    double complex, intent(in) :: Gram(Nm1,Nm1)
!    double complex :: res
!    integer :: i,j,con,sum1,sum2,fac
!
!    sum1 = 2*struc1(0)
!    sum2 = 2*struc2(0)
!    do i=1,Nm1
!      sum1 = sum1 + struc1(i)
!      sum2 = sum2 + struc2(i)
!    end do
!    
!    if (sum1.ne.sum2) then
!      write(*,*) 'error in ContractLoStruc:'
!      write(*,*) 'struc1 and struc2 must be of equal rank!'
!      stop
!    end if
!
!    if (sum1.eq.0) then
!      res = 1d0
!      return
!    end if
!    
!
!    do i=0,Nm1
!      if (struc1(i).ge.1) then
!        con = i
!      end if
!    end do
!
!    res = 0d0
!    struc1aux = struc1
!    struc1aux(con) = struc1aux(con)-1
!
!    if (con.ge.1) then
!
!      ! contract p_con from T1 with g from T2
!      if (struc2(0).ge.1) then
!        struc2aux = struc2
!        struc2aux(0) = struc2aux(0)-1
!        struc2aux(con) = struc2aux(con)+1
!        ! go on contracting recursively
!        ! (factor struc2aux(con) because of symmetrization wrt. g and pi)
!        res = res + struc2aux(con)*ContractLoStruc(Nm1,struc1aux,struc2aux,Gram)
!      end if
!
!      ! contract p_con from T1 with all the pi from T2
!      do i=1,Nm1
!        if (struc2(i).ge.1) then
!          struc2aux = struc2
!          struc2aux(i) = struc2aux(i)-1
!          ! go on contracting recursively
!          res = res + Gram(i,con)*ContractLoStruc(Nm1,struc1aux,struc2aux,Gram)
!        end if
!      end do
!
!
!    else
!        
!      ! contract g from T1 with g from T2
!      if (struc2(0).ge.1) then
!        struc2aux = struc2
!        struc2aux(0) = struc2aux(0)-1
!        ! full contraction g^{mu,nu}.g_{mu,nu}
!        ! tensor in D=4 dimensions g*g=4
!        fac = 4
!        do i=0,Nm1
!          ! partial contration g^{mu,nu}.(pi_mu g_{nu,rho})
!          ! or g^{mu,nu}.(g_{mu,rho}g_{nu,sigma})
!          ! factor 2 for mu <--> nu
!          fac = fac + 2*struc2aux(i)
!        end do
!        ! go on contracting recursively
!        res = res + fac*ContractLoStruc(Nm1,struc1aux,struc2aux,Gram)
!      end if
!
!      ! contract g^{mu,nu} from T1 with pi,pj from T2
!      do i=1,Nm1
!        if (struc2(i).ge.1) then
!          struc2aux = struc2
!          struc2aux(i) = struc2aux(i)-1
!          do j=1,Nm1
!            if (struc2aux(j).ge.1) then
!              struc2aux2 = struc2aux
!              struc2aux2(j) = struc2aux2(j)-1
!              ! go on contracting recursively
!              res = res + Gram(i,j)*ContractLoStruc(Nm1,struc1aux,struc2aux2,Gram)
!            end if
!          end do
!        end if
!      end do
!
!    end if
!
!
!  end function ContractLoStruc





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CompareTensors
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CompareTensors(T1,T2,rmax,writeout,acc)

    integer, intent(in) :: rmax, writeout
    double complex, intent(in) :: T1(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: T2(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(in) :: acc
    integer :: r,n0,n1,n2,n3

    do r=0,rmax
      do n0=0,r
        do n1=0,r-n0
          do n2=0,r-n0-n1
            n3 = r-n0-n1-n2

            if (writeout.eq.1) then
              if (abs(T1(n0,n1,n2,n3)-T2(n0,n1,n2,n3)).gt.  &
                  acc*abs(T1(n0,n1,n2,n3)+T2(n0,n1,n2,n3))) then
                write(ninfout_cll,*) n0,n1,n2,n3
                write(ninfout_cll,*) T1(n0,n1,n2,n3), T2(n0,n1,n2,n3)
              end if
            end if

          end do
        end do
      end do
    end do

  end subroutine CompareTensors

     
end module TensorReduction

