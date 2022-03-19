program test_gf
   USE mt19937
   USE gf_utils
   USE cu_gf_driver
   USE machine, only: kind_phys

   IMPLICIT NONE

   !--For init
   integer  :: imfshalcnv, imfshalcnv_gf
   integer  :: imfdeepcnv, imfdeepcnv_gf
   integer, parameter  :: mpirank = 0
   integer, parameter  :: mpiroot = 0
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

   !---Allocating arrays
   integer :: alloc_stat

   !===============================
   ntracer = 13 !10
   ix = 10240 !1
   im = 10240 !1
   km = 128 !64
   dt = 600.0 !0.1
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
   dfi_radar_max_intervals = 4 !1
   ldiag3d = .TRUE.
   do_cap_suppress = .TRUE.

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
       dtend(im, km, 105),           & !confirm
       dtidx(113, 18),               & !integer
       qci_conv(im, km),            & !confirm
       ix_dfi_radar(num_dfi_radar), &
       fh_dfi_radar(num_dfi_radar), &
       cap_suppress(im, num_dfi_radar), &
       STAT=alloc_stat)
   IF (alloc_stat /= 0) STOP "Error allocating arrays"

   !=============================================================
   !---Initialize arrays with random values
   PRINT*, "Initialize arrays"
   CALL mt19937_real1d(garea)
   cactiv(:) = 1
   cactiv_m(:) = 1
   do i=1,im
     cactiv  (i) = 1 + mod(i,2)
     cactiv_m(i) = 1 + mod(i,3)
   enddo
   CALL mt19937_real2d(forcet)
   CALL mt19937_real2d(forceqv_spechum)
   CALL mt19937_real2d(phil)
   CALL mt19937_real1d(raincv)
   CALL mt19937_real2d(qv_spechum)
   CALL mt19937_real2d(t)
   t(:,:) = t(:,:) + 510
   CALL mt19937_real1d(cld1d)
   CALL mt19937_real2d(us)
   CALL mt19937_real2d(vs)
   CALL mt19937_real2d(t2di)
   t2di(:,:) = t2di(:,:) + 500
   CALL mt19937_real2d(w)
   CALL mt19937_real2d(qv2di_spechum)
   CALL mt19937_real2d(p2di)
   CALL mt19937_real1d(psuri)
   hbot(:) = 1
   htop(:) = 4
   kcnv(:) = 1
   xland(:) = 1
   do i=1,im
     kcnv (i) = 1 + mod(i,2)
     xland(i) = 1 + mod(i,3)
   enddo
   CALL mt19937_real1d(hfx2)
   CALL mt19937_real1d(qfx2)
   CALL mt19937_real2d(cliw)
   CALL mt19937_real2d(clcw)
   CALL mt19937_real1d(pbl)
   CALL mt19937_real2d(ud_mf)
   CALL mt19937_real2d(dd_mf)
   CALL mt19937_real2d(dt_mf)
   CALL mt19937_real2d(cnvw_moist)
   CALL mt19937_real2d(cnvc)
   CALL mt19937_real3d(dtend)
   dtidx(:,:) = 1
   CALL mt19937_real2d(qci_conv)
   CALL mt19937_real1d(aod_gf)
   ix_dfi_radar(:) = 1
   do i=1,113
     do j=1,18
        dtidx(i,j) = 1 + mod(j,8) + mod(i,8)
     enddo
   enddo
   do i=1,im
     ix_dfi_radar(i) = 1 + mod(i,3)
   enddo
   CALL mt19937_real1d(fh_dfi_radar)
   CALL mt19937_real2d(cap_suppress)
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
   !read(10,*) (((dtend(i,j,k), k=1,105), j=1,km), i=1,im)
   !read(10,*) ((dtidx(i,j), j=1,18), i=1,113)
   !read(10,*) ((qci_conv(i,j), j=1,km), i=1,im)
   !read(10,*) (ix_dfi_radar(i), i=1,num_dfi_radar)
   !read(10,*) (fh_dfi_radar(i), i=1,num_dfi_radar)
   !read(10,*) ((cap_suppress(i,j), j=1,num_dfi_radar), i=1,im)
   !print*,"=================================="
   !close(10)
   !=============================================================

!$acc enter data copyin( garea )               
!$acc enter data copyin( cactiv )              
!$acc enter data copyin( cactiv_m )            
!$acc enter data copyin( forcet )              
!$acc enter data copyin( forceqv_spechum )     
!$acc enter data copyin( phil )                
!$acc enter data copyin( raincv )              
!$acc enter data copyin( qv_spechum )          
!$acc enter data copyin( t )                   
!$acc enter data copyin( cld1d )               
!$acc enter data copyin( us )                  
!$acc enter data copyin( vs )                  
!$acc enter data copyin( t2di )                
!$acc enter data copyin( w )                   
!$acc enter data copyin( qv2di_spechum )       
!$acc enter data copyin( p2di )                
!$acc enter data copyin( psuri )               
!$acc enter data copyin( hbot )                
!$acc enter data copyin( htop )                
!$acc enter data copyin( kcnv )                
!$acc enter data copyin( xland )               
!$acc enter data copyin( hfx2 )                
!$acc enter data copyin( qfx2 )                
!$acc enter data copyin( aod_gf )              
!$acc enter data copyin( cliw )                
!$acc enter data copyin( clcw )                
!$acc enter data copyin( pbl )                 
!$acc enter data copyin( ud_mf )               
!$acc enter data copyin( dd_mf )               
!$acc enter data copyin( dt_mf )               
!$acc enter data copyin( cnvw_moist )          
!$acc enter data copyin( cnvc )                
!$acc enter data copyin( dtend )               
!$acc enter data copyin( dtidx )               
!$acc enter data copyin( qci_conv)
!$acc enter data copyin( ix_dfi_radar )        
!$acc enter data copyin( fh_dfi_radar )        
!$acc enter data copyin( cap_suppress)

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

   PRINT*, "Calling run"
   CALL SYSTEM_CLOCK (count_rate=count_rate)
   CALL SYSTEM_CLOCK (count=count_start)

   CALL cu_gf_driver_run(ntracer,garea,im,km,dt,flag_init,flag_restart,&
               cactiv,cactiv_m,g,cp,xlv,r_v,forcet,forceqv_spechum,phil,raincv, &
               qv_spechum,t,cld1d,us,vs,t2di,w,qv2di_spechum,p2di,psuri,        &
               hbot,htop,kcnv,xland,hfx2,qfx2,aod_gf,cliw,clcw,                 &
               pbl,ud_mf,dd_mf,dt_mf,cnvw_moist,cnvc,imfshalcnv,                &
               flag_for_scnv_generic_tend,flag_for_dcnv_generic_tend,           &
               dtend,dtidx,ntqv,ntiw,ntcw,index_of_temperature,index_of_x_wind, &
               index_of_y_wind,index_of_process_scnv,index_of_process_dcnv,     &
               fhour,fh_dfi_radar,ix_dfi_radar,num_dfi_radar,cap_suppress,      &
               dfi_radar_max_intervals,ldiag3d,qci_conv,do_cap_suppress,        &
               errmsg,errflg)

   CALL SYSTEM_CLOCK (count=count_end)
   elapsed = REAL (count_end - count_start) / REAL (count_rate)
   PRINT*
   PRINT*, "Finished executing kernel in =", elapsed  
   PRINT*

   PRINT*, "Calling finalize"
   CALL cu_gf_driver_finalize()

!$acc update self( garea )               
!$acc update self( cactiv )              
!$acc update self( cactiv_m )            
!$acc update self( forcet )              
!$acc update self( forceqv_spechum )     
!$acc update self( phil )                
!$acc update self( raincv )              
!$acc update self( qv_spechum )          
!$acc update self( t )                   
!$acc update self( cld1d )               
!$acc update self( us )                  
!$acc update self( vs )                  
!$acc update self( t2di )                
!$acc update self( w )                   
!$acc update self( qv2di_spechum )       
!$acc update self( p2di )                
!$acc update self( psuri )               
!$acc update self( hbot )                
!$acc update self( htop )                
!$acc update self( kcnv )                
!$acc update self( xland )               
!$acc update self( hfx2 )                
!$acc update self( qfx2 )                
!$acc update self( aod_gf )              
!$acc update self( cliw )                
!$acc update self( clcw )                
!$acc update self( pbl )                 
!$acc update self( ud_mf )               
!$acc update self( dd_mf )               
!$acc update self( dt_mf )               
!$acc update self( cnvw_moist )          
!$acc update self( cnvc )                
!$acc update self( dtend )               
!$acc update self( dtidx )               
!$acc update self( qci_conv)
!$acc update self( ix_dfi_radar )        
!$acc update self( fh_dfi_radar )        
!$acc update self( cap_suppress)

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
!$acc exit data delete( garea )               
!$acc exit data delete( cactiv )              
!$acc exit data delete( cactiv_m )            
!$acc exit data delete( forcet )              
!$acc exit data delete( forceqv_spechum )     
!$acc exit data delete( phil )                
!$acc exit data delete( raincv )              
!$acc exit data delete( qv_spechum )          
!$acc exit data delete( t )                   
!$acc exit data delete( cld1d )               
!$acc exit data delete( us )                  
!$acc exit data delete( vs )                  
!$acc exit data delete( t2di )                
!$acc exit data delete( w )                   
!$acc exit data delete( qv2di_spechum )       
!$acc exit data delete( p2di )                
!$acc exit data delete( psuri )               
!$acc exit data delete( hbot )                
!$acc exit data delete( htop )                
!$acc exit data delete( kcnv )                
!$acc exit data delete( xland )               
!$acc exit data delete( hfx2 )                
!$acc exit data delete( qfx2 )                
!$acc exit data delete( aod_gf )              
!$acc exit data delete( cliw )                
!$acc exit data delete( clcw )                
!$acc exit data delete( pbl )                 
!$acc exit data delete( ud_mf )               
!$acc exit data delete( dd_mf )               
!$acc exit data delete( dt_mf )               
!$acc exit data delete( cnvw_moist )          
!$acc exit data delete( cnvc )                
!$acc exit data delete( dtend )               
!$acc exit data delete( dtidx )               
!$acc exit data delete( qci_conv(1:im,1:km))
!$acc exit data delete( ix_dfi_radar )        
!$acc exit data delete( fh_dfi_radar )        
!$acc exit data delete( cap_suppress)

end program test_gf
