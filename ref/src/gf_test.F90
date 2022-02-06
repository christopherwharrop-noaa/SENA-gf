program test_gf
   USE mt19937
   USE gf_utils
   USE cu_gf_driver
   USE machine, only: kind_phys

   IMPLICIT NONE

   !--For init
   integer, parameter  :: imfshalcnv = 2, imfshalcnv_gf = 2
   integer, parameter  :: imfdeepcnv = 1, imfdeepcnv_gf = 1
   integer, parameter  :: mpirank = 0
   integer, parameter  :: mpiroot = 0
   character(len=512)  :: errmsg
   integer             :: errflg

   !---For run
   integer, parameter :: ix = 10000, im = 10000, km = 100, ntracer = 10
   logical, parameter :: flag_for_scnv_generic_tend = .TRUE., flag_for_dcnv_generic_tend = .TRUE.
   real (kind=kind_phys), parameter :: g = 1.0 ,cp = 2.0, xlv = 3.0, r_v = 2.3
   logical, parameter :: ldiag3d = .TRUE., flag_init = .FALSE., flag_restart = .FALSE.
   real(kind=kind_phys) :: dt = 0.1
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

   logical, parameter :: do_cap_suppress = .FALSE.
   integer, parameter :: dfi_radar_max_intervals = 1
   real(kind=kind_phys), parameter:: fhour = 1.0
   integer, parameter :: num_dfi_radar = 10
   integer, dimension(:), allocatable :: ix_dfi_radar
   real(kind=kind_phys), dimension(:), allocatable :: fh_dfi_radar
   real(kind=kind_phys), dimension(:,:), allocatable :: cap_suppress

   integer :: count_rate, count_start, count_end
   real :: elapsed

   !---Allocating arrays
   integer :: alloc_stat

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
       dtend(im, km, 20),           & !confirm
       dtidx(im, km),               & !integer
       qci_conv(im, km),            & !confirm
       ix_dfi_radar(num_dfi_radar), &
       fh_dfi_radar(num_dfi_radar), &
       cap_suppress(im, num_dfi_radar), &
       STAT=alloc_stat)
   IF (alloc_stat /= 0) STOP "Error allocating arrays"


   !---Initialize arrays with random values
   PRINT*, "Initialize arrays"
   CALL mt19937_real1d(garea)
   cactiv(:) = 1
   cactiv_m(:) = 1
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
   CALL mt19937_real1d(fh_dfi_radar)
   CALL mt19937_real2d(cap_suppress)
   

!$acc enter data copyin(         &
!$acc       garea,               &
!$acc       cactiv,              &
!$acc       cactiv_m,            &
!$acc       forcet,              &
!$acc       forceqv_spechum,     &
!$acc       phil,                &
!$acc       raincv,              &
!$acc       qv_spechum,          &
!$acc       t,                   &
!$acc       cld1d,               &
!$acc       us,                  &
!$acc       vs,                  &
!$acc       t2di,                &
!$acc       w,                   &
!$acc       qv2di_spechum,       &
!$acc       p2di,                &
!$acc       psuri,               &
!$acc       hbot,                &
!$acc       htop,                &
!$acc       kcnv,                &
!$acc       xland,               &
!$acc       hfx2,                &
!$acc       qfx2,                &
!$acc       aod_gf,              &
!$acc       cliw,                &
!$acc       clcw,                &
!$acc       pbl,                 &
!$acc       ud_mf,               &
!$acc       dd_mf,               &
!$acc       dt_mf,               &
!$acc       cnvw_moist,          &
!$acc       cnvc,                &
!$acc       dtend,               &
!$acc       dtidx,               &
!$acc       qci_conv,            &
!$acc       ix_dfi_radar,        &
!$acc       fh_dfi_radar,        &
!$acc       cap_suppress)

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

!$acc update self(               &
!$acc       garea,               &
!$acc       cactiv,              &
!$acc       cactiv_m,            &
!$acc       forcet,              &
!$acc       forceqv_spechum,     &
!$acc       phil,                &
!$acc       raincv,              &
!$acc       qv_spechum,          &
!$acc       t,                   &
!$acc       cld1d,               &
!$acc       us,                  &
!$acc       vs,                  &
!$acc       t2di,                &
!$acc       w,                   &
!$acc       qv2di_spechum,       &
!$acc       p2di,                &
!$acc       psuri,               &
!$acc       hbot,                &
!$acc       htop,                &
!$acc       kcnv,                &
!$acc       xland,               &
!$acc       hfx2,                &
!$acc       qfx2,                &
!$acc       aod_gf,              &
!$acc       cliw,                &
!$acc       clcw,                &
!$acc       pbl,                 &
!$acc       ud_mf,               &
!$acc       dd_mf,               &
!$acc       dt_mf,               &
!$acc       cnvw_moist,          &
!$acc       cnvc,                &
!$acc       dtend,               &
!$acc       dtidx,               &
!$acc       qci_conv,            &
!$acc       ix_dfi_radar,        &
!$acc       fh_dfi_radar,        &
!$acc       cap_suppress)

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

end program test_gf
