program test_gf
   USE mt19937
   USE gf_utils
   USE cu_gf_driver
   USE machine, only: kind_phys
#define MPI
#ifdef _OPENMP
   USE omp_lib
#endif
#ifdef _OPENACC
   USE accel_lib
#endif

   IMPLICIT NONE
#ifdef MPI
   INCLUDE 'mpif.h'
#endif

   !--For init
   integer  :: imfshalcnv, imfshalcnv_gf
   integer  :: imfdeepcnv, imfdeepcnv_gf
   character(len=512)  :: errmsg
   integer             :: errflg
   integer             :: i,j,k

   !---For run
   integer :: ix, im, km, ntracer
   logical :: flag_for_scnv_generic_tend, flag_for_dcnv_generic_tend
   real (kind=kind_phys) :: g,cp, xlv, r_v
   logical :: ldiag3d, flag_init, flag_restart
   real(kind=kind_phys) :: dt
   integer :: index_of_x_wind, index_of_y_wind, index_of_temperature,            &
        index_of_process_scnv, index_of_process_dcnv, ntqv, ntcw, ntiw    !in

   !allocatables
   real(kind=kind_phys), allocatable :: dtend(:,:,:) !inout
   integer, allocatable  :: dtidx(:,:) !in
   real(kind=kind_phys),  dimension( : , : ), allocatable :: forcet,forceqv_spechum,w,phil !in
   real(kind=kind_phys),  dimension( : , : ), allocatable :: t,us,vs !inout
   real(kind=kind_phys),  dimension( : , : ), allocatable :: qci_conv !inout
   real(kind=kind_phys),  dimension( : , : ), allocatable :: cnvw_moist,cnvc !out
   real(kind=kind_phys),  dimension( : , : ), allocatable :: cliw, clcw !inout
   integer, dimension (:),  allocatable :: hbot,htop,kcnv !out
   integer, dimension (:),  allocatable :: xland !in
   real(kind=kind_phys),    dimension (:), allocatable :: pbl !in
   real(kind=kind_phys), dimension (:), allocatable    :: hfx2,qfx2,psuri !in
   real(kind=kind_phys), dimension (:,:), allocatable  :: ud_mf,dd_mf,dt_mf !out
   real(kind=kind_phys), dimension (:), allocatable    :: raincv,cld1d !out
   real(kind=kind_phys), dimension (:,:), allocatable  :: t2di,p2di !in
   real(kind=kind_phys), dimension (:,:), allocatable :: qv2di_spechum !in
   real(kind=kind_phys), dimension (:,:), allocatable :: qv_spechum    !inout
   real(kind=kind_phys), dimension(:), allocatable :: garea !in
   integer, dimension(:), allocatable :: cactiv,cactiv_m !inout
   real(kind=kind_phys),    dimension (:), allocatable :: aod_gf !inout

   logical :: do_cap_suppress
   integer :: dfi_radar_max_intervals
   real(kind=kind_phys):: fhour
   integer :: num_dfi_radar
   integer, dimension(:), allocatable :: ix_dfi_radar
   real(kind=kind_phys), dimension(:), allocatable :: fh_dfi_radar
   real(kind=kind_phys), dimension(:,:), allocatable :: cap_suppress

   integer :: count_rate, count_start, count_end
   real :: elapsed

   integer :: alloc_stat
   integer :: n_omp_threads, s, e, tid
   integer :: N_GPUS, gpuid
   integer, parameter :: DTEND_DIM = 12

   integer :: M, N_STEPS
   integer ncol,nlev,ierror
   character(64) :: str

   ! MPI information
   integer                    :: mpicomm
   integer                    :: mpirank
   integer                    :: mpiroot
   integer                    :: mpisize

   !===============================
#ifdef MPI
   mpicomm = MPI_COMM_WORLD
   mpiroot = 0
   CALL MPI_INIT(ierror)
   CALL MPI_COMM_SIZE(mpicomm, mpisize, ierror)
   CALL MPI_COMM_RANK(mpicomm, mpirank, ierror)

   gpuid = mpirank
#else
   gpuid = 0
#endif
   PRINT*, 'MPI rank', mpirank

#ifdef _OPENACC
   CALL acc_set_device_num(gpuid,acc_device_nvidia)
#endif

#ifdef _OPENMP
!$omp parallel
!$omp single
   n_omp_threads = omp_get_num_threads()
!$omp end single
!$omp end parallel
#endif

#ifdef _OPENACC
   N_GPUS = 1
   n_omp_threads = N_GPUS
   CALL omp_set_num_threads(n_omp_threads)
#else
   N_GPUS = 0
#endif

#ifdef _OPENMP
   WRITE(6,'(" Using ",i3," threads")') n_omp_threads
#endif

   !===============================
   if (COMMAND_ARGUMENT_COUNT().GE.1) THEN
      CALL GET_COMMAND_ARGUMENT(1, str)
      READ(str,*) ncol
   else
      ncol = 512
   endif
   if (COMMAND_ARGUMENT_COUNT().GE.2) THEN
      CALL GET_COMMAND_ARGUMENT(2, str)
      READ(str,*) nlev
   else
      nlev = 64
   endif
   if (COMMAND_ARGUMENT_COUNT().GE.3) THEN
      CALL GET_COMMAND_ARGUMENT(3, str)
      READ(str,*) N_STEPS
   else
      N_STEPS = 1
   endif
   print*, ncol, nlev, N_STEPS
   !===============================
   !===============================
   ntracer = 13
   im = ncol
   km = nlev
   ix = im
   dt = 600.0
   flag_init = .FALSE.
   flag_restart = .FALSE.
   g = 9.806649999
   cp = 1004.6
   xlv = 2500000.0
   r_v = 461.5
   imfshalcnv = 3
   imfshalcnv_gf = 3
   imfdeepcnv = 1
   imfdeepcnv_gf = 1
   flag_for_scnv_generic_tend = .FALSE.
   flag_for_dcnv_generic_tend = .FALSE.
   ntqv = 1
   ntiw = 3
   ntcw = 2
   index_of_x_wind = 11
   index_of_y_wind = 12
   index_of_temperature = 10
   index_of_process_scnv = 3
   index_of_process_dcnv = 2
   fhour = 72.0 !1.0
   num_dfi_radar = 10
   dfi_radar_max_intervals = 4
   ldiag3d = .TRUE.
   do_cap_suppress = .TRUE.

   WRITE(6,'(" (im,km) = (",i5,",",i4,")")') im,km

   !!===============================
   !open(unit=10, file="input.dat", status='old')
   !read(10,*) ntracer,im,km,dt,flag_init,flag_restart,g,cp,xlv,r_v, &
   !            imfshalcnv,flag_for_scnv_generic_tend, &
   !            flag_for_dcnv_generic_tend,ntqv,ntiw,ntcw, &
   !            index_of_x_wind, index_of_y_wind, index_of_temperature, &
   !            index_of_process_scnv, index_of_process_dcnv, fhour, &
   !            num_dfi_radar, dfi_radar_max_intervals, ldiag3d, do_cap_suppress
   !ix = im
   !flag_for_scnv_generic_tend = .TRUE.
   !flag_for_dcnv_generic_tend = .TRUE.
   !num_dfi_radar = 10
   !!===============================

#ifdef MPI
   CALL MPI_Barrier(MPI_COMM_WORLD,ierror)
#endif

   PRINT*, "Allocating arrays"
   ALLOCATE(                        &
       garea(im),                   &
       cactiv(im),                  & !integer
       cactiv_m(im),                  & !integer
       forcet(ix, km),              &
       forceqv_spechum(ix, km),     &
       phil(ix, km),                &
       raincv(im),                  &
       qv_spechum(ix, km),          &
       t(ix, km),                   &
       cld1d(im),                   &
       us(ix, km),                  &
       vs(ix, km),                  &
       t2di(ix, km),                &
       w(ix, km),                   &
       qv2di_spechum(ix, km),       &
       p2di(ix, km),                &
       psuri(im),                   &
       hbot(im),                    & !integer
       htop(im),                    & !integer
       kcnv(im),                    & !integer
       xland(im),                   & !integer
       hfx2(im),                    &
       qfx2(im),                    &
       aod_gf(im),                  &
       cliw(ix, km),                &
       clcw(ix, km),                &
       pbl(im),                     &
       ud_mf(im, km),               &
       dd_mf(im, km),               &
       dt_mf(im, km),               &
       cnvw_moist(ix, km),          &
       cnvc(ix, km),                &
       dtend(im, km, DTEND_DIM),           & !confirm
       dtidx(113, 18),               & !integer
       qci_conv(im, km),            & !confirm
       ix_dfi_radar(num_dfi_radar), &
       fh_dfi_radar(num_dfi_radar+1), &
       cap_suppress(im, num_dfi_radar), &
       STAT=alloc_stat)
   IF (alloc_stat /= 0) STOP "Error allocating arrays"

   !=============================================================
   PRINT*, "Initializing arrays"
   s = 1
   e = im

   CALL mt19937_real1d(garea(s:e))
   cactiv(s:e) = 1
   cactiv_m(s:e) = 1
   do i=s,e
     cactiv  (i) = 1 + mod(i,2)
     cactiv_m(i) = 1 + mod(i,3)
   enddo
   CALL mt19937_real2d(forcet(s:e,:))
   CALL mt19937_real2d(forceqv_spechum(s:e,:))
   CALL mt19937_real2d(phil(s:e,:))
   CALL mt19937_real1d(raincv(s:e))
   CALL mt19937_real2d(qv_spechum(s:e,:))
   CALL mt19937_real2d(t(s:e,:))
   t(s:e,:) = t(s:e,:) + 510
   CALL mt19937_real1d(cld1d(s:e))
   CALL mt19937_real2d(us(s:e,:))
   CALL mt19937_real2d(vs(s:e,:))
   CALL mt19937_real2d(t2di(s:e,:))
   t2di(s:e,:) = t2di(s:e,:) + 500
   CALL mt19937_real2d(w(s:e,:))
   CALL mt19937_real2d(qv2di_spechum(s:e,:))
   CALL mt19937_real2d(p2di(s:e,:))
   CALL mt19937_real1d(psuri(s:e))
   hbot(s:e) = 1
   htop(s:e) = 4
   kcnv(s:e) = 1
   xland(s:e) = 1
   do i=s,e
     kcnv (i) = 1 + mod(i,2)
     xland(i) = 1 + mod(i,3)
   enddo
   CALL mt19937_real1d(hfx2(s:e))
   CALL mt19937_real1d(qfx2(s:e))
   CALL mt19937_real2d(cliw(s:e,:))
   CALL mt19937_real2d(clcw(s:e,:))
   CALL mt19937_real1d(pbl(s:e))
   CALL mt19937_real2d(ud_mf(s:e,:))
   CALL mt19937_real2d(dd_mf(s:e,:))
   CALL mt19937_real2d(dt_mf(s:e,:))
   CALL mt19937_real2d(cnvw_moist(s:e,:))
   CALL mt19937_real2d(cnvc(s:e,:))
   CALL mt19937_real3d(dtend(s:e,:,:))
   dtidx(:,:) = 1
   CALL mt19937_real2d(qci_conv(s:e,:))
   CALL mt19937_real1d(aod_gf(s:e))
   ix_dfi_radar(:) = 1
   do i=1,113
     do j=1,18
        dtidx(i,j) = 1 + mod(j,4) + mod(i,4)
     enddo
   enddo
   do i=1,num_dfi_radar
     ix_dfi_radar(i) = 1 + mod(i,3)
   enddo
   CALL mt19937_real1d(fh_dfi_radar(:))
   CALL mt19937_real2d(cap_suppress(s:e,:))
   !=============================================================
   
   !=============================================================
   !print*,"=================================="
   !print*,"    Reading input data            "
   !print*,"=================================="
   !read(10,*) (garea(i), i=1,im)
   !read(10,*) (cactiv(i), i=1,im)
   !read(10,*) ((forcet(i,j), j=1,km), i=1,im)
   !read(10,*) ((forceqv_spechum(i,j), j=1,km), i=1,im)
   !read(10,*) ((phil(i,j), j=1,km), i=1,im)
   !read(10,*) (raincv(i), i=1,im)
   !read(10,*) ((qv_spechum(i,j), j=1,km), i=1,im)
   !read(10,*) ((t(i,j), j=1,km), i=1,im)
   !read(10,*) (cld1d(i), i=1,im)
   !read(10,*) ((us(i,j), j=1,km), i=1,im)
   !read(10,*) ((vs(i,j), j=1,km), i=1,im)
   !read(10,*) ((t2di(i,j), j=1,km), i=1,im)
   !read(10,*) ((w(i,j), j=1,km), i=1,im)
   !read(10,*) ((qv2di_spechum(i,j), j=1,km), i=1,im)
   !read(10,*) ((p2di(i,j), j=1,km), i=1,im)
   !read(10,*) (psuri(i), i=1,im)
   !read(10,*) (hbot(i), i=1,im)
   !read(10,*) (htop(i), i=1,im)
   !read(10,*) (kcnv(i), i=1,im)
   !read(10,*) (xland(i), i=1,im)
   !read(10,*) (hfx2(i), i=1,im)
   !read(10,*) (qfx2(i), i=1,im)
   !read(10,*) (aod_gf(i), i=1,im)
   !read(10,*) ((cliw(i,j), j=1,km), i=1,im)
   !read(10,*) ((clcw(i,j), j=1,km), i=1,im)
   !read(10,*) (pbl(i), i=1,im)
   !read(10,*) ((ud_mf(i,j), j=1,km), i=1,im)
   !read(10,*) ((dd_mf(i,j), j=1,km), i=1,im)
   !read(10,*) ((dt_mf(i,j), j=1,km), i=1,im)
   !read(10,*) ((cnvw_moist(i,j), j=1,km), i=1,im)
   !read(10,*) ((cnvc(i,j), j=1,km), i=1,im)
   !read(10,*) (((dtend(i,j,k), k=1,DTEND_DIM), j=1,km), i=1,im)
   !read(10,*) ((dtidx(i,j), j=1,18), i=1,113)
   !read(10,*) ((qci_conv(i,j), j=1,km), i=1,im)
   !read(10,*) (ix_dfi_radar(i), i=1,num_dfi_radar)
   !read(10,*) (fh_dfi_radar(i), i=1,num_dfi_radar+1)
   !read(10,*) ((cap_suppress(i,j), j=1,num_dfi_radar), i=1,im)
   !print*,"=================================="
   !close(10)
   !=============================================================

#ifdef MPI
   CALL MPI_Barrier(MPI_COMM_WORLD,ierror)
#endif

   CALL SYSTEM_CLOCK (count_rate=count_rate)
   CALL SYSTEM_CLOCK (count=count_start)

#ifdef _OPENACC
   PRINT*, "Copying arrays to GPU"
   s = 1
   e = im
!$acc enter data copyin( garea(s:e) )               
!$acc enter data copyin( cactiv(s:e) )              
!$acc enter data copyin( cactiv_m(s:e) )            
!$acc enter data copyin( forcet(s:e,:) )              
!$acc enter data copyin( forceqv_spechum(s:e,:) )     
!$acc enter data copyin( phil(s:e,:) )                
!$acc enter data copyin( raincv(s:e) )              
!$acc enter data copyin( qv_spechum(s:e,:) )          
!$acc enter data copyin( t(s:e,:) )                   
!$acc enter data copyin( cld1d(s:e) )               
!$acc enter data copyin( us(s:e,:) )                  
!$acc enter data copyin( vs(s:e,:) )                  
!$acc enter data copyin( t2di(s:e,:) )                
!$acc enter data copyin( w(s:e,:) )                   
!$acc enter data copyin( qv2di_spechum(s:e,:) )       
!$acc enter data copyin( p2di(s:e,:) )                
!$acc enter data copyin( psuri(s:e) )               
!$acc enter data copyin( hbot(s:e) )                
!$acc enter data copyin( htop(s:e) )                
!$acc enter data copyin( kcnv(s:e) )                
!$acc enter data copyin( xland(s:e) )               
!$acc enter data copyin( hfx2(s:e) )                
!$acc enter data copyin( qfx2(s:e) )                
!$acc enter data copyin( aod_gf(s:e) )              
!$acc enter data copyin( cliw(s:e,:) )                
!$acc enter data copyin( clcw(s:e,:) )                
!$acc enter data copyin( pbl(s:e) )                 
!$acc enter data copyin( ud_mf(s:e,:) )               
!$acc enter data copyin( dd_mf(s:e,:) )               
!$acc enter data copyin( dt_mf(s:e,:) )               
!$acc enter data copyin( cnvw_moist(s:e,:) )          
!$acc enter data copyin( cnvc(s:e,:) )                
!$acc enter data copyin( dtend(s:e,:,:) )               
!$acc enter data copyin( dtidx(:,:) )               
!$acc enter data copyin( qci_conv(s:e,:))
!$acc enter data copyin( ix_dfi_radar(:) )        
!$acc enter data copyin( fh_dfi_radar(:) )        
!$acc enter data copyin( cap_suppress(s:e,:) )

#endif

   CALL SYSTEM_CLOCK (count=count_end)
   elapsed = REAL (count_end - count_start) / REAL (count_rate)
   PRINT*, "Finished copying data in =", elapsed  
   PRINT*

#ifdef MPI
   CALL MPI_Barrier(MPI_COMM_WORLD,ierror)
#endif

   !--- Print state
   CALL print_state("Input state",   &
       garea,                   &
       cactiv,                  &
       cactiv_m,                &
       forcet,                  &
       forceqv_spechum,         &
       phil,                    &
       raincv,                  &
       qv_spechum,              &
       t,                       &
       cld1d,                   &
       us,                      &
       vs,                      &
       t2di,                    &
       w,                       &
       qv2di_spechum,           &
       p2di,                    &
       psuri,                   &
       hbot,                    &
       htop,                    &
       kcnv,                    &
       xland,                   &
       hfx2,                    &
       qfx2,                    &
       aod_gf,                  &
       cliw,                    &
       clcw,                    &
       pbl,                     &
       ud_mf,                   &
       dd_mf,                   &
       dt_mf,                   &
       cnvw_moist,              &
       cnvc,                    &
       dtend,                   &
       dtidx,                   &
       qci_conv,                &
       ix_dfi_radar,            &
       fh_dfi_radar,            &
       cap_suppress             &
       )
   !-------------

   PRINT*, "Calling init"
   CALL cu_gf_driver_init(imfshalcnv, imfshalcnv_gf, imfdeepcnv, &
                          imfdeepcnv_gf,mpirank, mpiroot, errmsg, errflg)

#ifdef MPI
   CALL MPI_Barrier(MPI_COMM_WORLD,ierror)
#endif

   PRINT*, "Calling run"
   CALL SYSTEM_CLOCK (count_rate=count_rate)
   CALL SYSTEM_CLOCK (count=count_start)

#ifndef _OPENACC
!$omp parallel do private(tid,s,e)
#endif
   DO tid = 0, n_omp_threads - 1
       s = tid * (im / n_omp_threads) + 1
       e = (tid + 1) * (im / n_omp_threads)
       e = MIN(e, im)

       DO M = 1, N_STEPS
          CALL cu_gf_driver_run(ntracer,garea(s:e),e-s+1,km,dt,flag_init,flag_restart,&
               cactiv(s:e),cactiv_m(s:e),g,cp,xlv,r_v,forcet(s:e,:),forceqv_spechum(s:e,:),phil(s:e,:),raincv(s:e), &
               qv_spechum(s:e,:),t(s:e,:),cld1d(s:e),us(s:e,:),vs(s:e,:),t2di(s:e,:),w(s:e,:), &
               qv2di_spechum(s:e,:),p2di(s:e,:),psuri(s:e),        &
               hbot(s:e),htop(s:e),kcnv(s:e),xland(s:e),hfx2(s:e),qfx2(s:e),aod_gf(s:e),cliw(s:e,:),clcw(s:e,:),                 &
               pbl(s:e),ud_mf(s:e,:),dd_mf(s:e,:),dt_mf(s:e,:),cnvw_moist(s:e,:),cnvc(s:e,:),imfshalcnv,                &
               flag_for_scnv_generic_tend,flag_for_dcnv_generic_tend,           &
               dtend(s:e,:,:),dtidx(:,:),ntqv,ntiw,ntcw,index_of_temperature,index_of_x_wind, &
               index_of_y_wind,index_of_process_scnv,index_of_process_dcnv,     &
               fhour,fh_dfi_radar(:),ix_dfi_radar(:),num_dfi_radar,cap_suppress(s:e,:),      &
               dfi_radar_max_intervals,ldiag3d,qci_conv(s:e,:),do_cap_suppress,        &
               errmsg,errflg)
       ENDDO

   ENDDO
#ifndef _OPENACC
!$omp end parallel do
#endif

   CALL SYSTEM_CLOCK (count=count_end)
   elapsed = REAL (count_end - count_start) / REAL (count_rate)
   PRINT*
   PRINT*, "Finished executing kernel in =", elapsed  
   PRINT*

#ifdef MPI
   CALL MPI_Barrier(MPI_COMM_WORLD,ierror)
#endif

   PRINT*, "Calling finalize"
   CALL cu_gf_driver_finalize()

#ifdef _OPENACC
   s = 1
   e = im
!$acc update self( garea(s:e) )               
!$acc update self( cactiv(s:e) )              
!$acc update self( cactiv_m(s:e) )            
!$acc update self( forcet(s:e,:) )              
!$acc update self( forceqv_spechum(s:e,:) )     
!$acc update self( phil(s:e,:) )                
!$acc update self( raincv(s:e) )              
!$acc update self( qv_spechum(s:e,:) )          
!$acc update self( t(s:e,:) )                   
!$acc update self( cld1d(s:e) )               
!$acc update self( us(s:e,:) )                  
!$acc update self( vs(s:e,:) )                  
!$acc update self( t2di(s:e,:) )                
!$acc update self( w(s:e,:) )                   
!$acc update self( qv2di_spechum(s:e,:) )       
!$acc update self( p2di(s:e,:) )                
!$acc update self( psuri(s:e) )               
!$acc update self( hbot(s:e) )                
!$acc update self( htop(s:e) )                
!$acc update self( kcnv(s:e) )                
!$acc update self( xland(s:e) )               
!$acc update self( hfx2(s:e) )                
!$acc update self( qfx2(s:e) )                
!$acc update self( aod_gf(s:e) )              
!$acc update self( cliw(s:e,:) )                
!$acc update self( clcw(s:e,:) )                
!$acc update self( pbl(s:e) )                 
!$acc update self( ud_mf(s:e,:) )               
!$acc update self( dd_mf(s:e,:) )               
!$acc update self( dt_mf(s:e,:) )               
!$acc update self( cnvw_moist(s:e,:) )          
!$acc update self( cnvc(s:e,:) )                
!$acc update self( dtend(s:e,:,:) )               
!$acc update self( dtidx(:,:) )               
!$acc update self( qci_conv(s:e,:))
!$acc update self( ix_dfi_radar(:) )        
!$acc update self( fh_dfi_radar(:) )        
!$acc update self( cap_suppress(s:e,:) )
#endif

   !--- Print state
   CALL print_state("Output state",   &
       garea,                   &
       cactiv,                  &
       cactiv_m,                &
       forcet,                  &
       forceqv_spechum,         &
       phil,                    &
       raincv,                  &
       qv_spechum,              &
       t,                       &
       cld1d,                   &
       us,                      &
       vs,                      &
       t2di,                    &
       w,                       &
       qv2di_spechum,           &
       p2di,                    &
       psuri,                   &
       hbot,                    &
       htop,                    &
       kcnv,                    &
       xland,                   &
       hfx2,                    &
       qfx2,                    &
       aod_gf,                  &
       cliw,                    &
       clcw,                    &
       pbl,                     &
       ud_mf,                   &
       dd_mf,                   &
       dt_mf,                   &
       cnvw_moist,              &
       cnvc,                    &
       dtend,                   &
       dtidx,                   &
       qci_conv,                &
       ix_dfi_radar,            &
       fh_dfi_radar,            &
       cap_suppress             &
       )
   !-------------

#ifdef _OPENACC
   s = 1
   e = im
!$acc exit data delete( garea(s:e) )               
!$acc exit data delete( cactiv(s:e) )              
!$acc exit data delete( cactiv_m(s:e) )            
!$acc exit data delete( forcet(s:e,:) )              
!$acc exit data delete( forceqv_spechum(s:e,:) )     
!$acc exit data delete( phil(s:e,:) )                
!$acc exit data delete( raincv(s:e) )              
!$acc exit data delete( qv_spechum(s:e,:) )          
!$acc exit data delete( t(s:e,:) )                   
!$acc exit data delete( cld1d(s:e) )               
!$acc exit data delete( us(s:e,:) )                  
!$acc exit data delete( vs(s:e,:) )                  
!$acc exit data delete( t2di(s:e,:) )                
!$acc exit data delete( w(s:e,:) )                   
!$acc exit data delete( qv2di_spechum(s:e,:) )       
!$acc exit data delete( p2di(s:e,:) )                
!$acc exit data delete( psuri(s:e) )               
!$acc exit data delete( hbot(s:e) )                
!$acc exit data delete( htop(s:e) )                
!$acc exit data delete( kcnv(s:e) )                
!$acc exit data delete( xland(s:e) )               
!$acc exit data delete( hfx2(s:e) )                
!$acc exit data delete( qfx2(s:e) )                
!$acc exit data delete( aod_gf(s:e) )              
!$acc exit data delete( cliw(s:e,:) )                
!$acc exit data delete( clcw(s:e,:) )                
!$acc exit data delete( pbl(s:e) )                 
!$acc exit data delete( ud_mf(s:e,:) )               
!$acc exit data delete( dd_mf(s:e,:) )               
!$acc exit data delete( dt_mf(s:e,:) )               
!$acc exit data delete( cnvw_moist(s:e,:) )          
!$acc exit data delete( cnvc(s:e,:) )                
!$acc exit data delete( dtend(s:e,:,:) )               
!$acc exit data delete( dtidx(:,:) )               
!$acc exit data delete( qci_conv(s:e,:))
!$acc exit data delete( ix_dfi_radar(:) )        
!$acc exit data delete( fh_dfi_radar(:) )        
!$acc exit data delete( cap_suppress(s:e,:) )
#endif

#ifdef MPI
   CALL MPI_FINALIZE(ierror)
#endif

end program test_gf
