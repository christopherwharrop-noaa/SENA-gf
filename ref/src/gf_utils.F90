MODULE gf_utils

  use machine , only : kind_phys

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: print_state

CONTAINS

  !------------------------------------------------------------------
  ! print_state
  !
  ! Prints statistics for the kernel state variables
  !------------------------------------------------------------------
  SUBROUTINE print_state(msg,   &
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

    CHARACTER(LEN=*) :: msg

    REAL(kind_phys), INTENT(IN) ::       garea(:)
    INTEGER, INTENT(IN) :: cactiv(:), cactiv_m(:)
    REAL(kind_phys), INTENT(IN) :: forcet(:, :)
    REAL(kind_phys), INTENT(IN) :: forceqv_spechum(:, :)
    REAL(kind_phys), INTENT(IN) :: phil(:, :)
    REAL(kind_phys), INTENT(IN) :: raincv(:)
    REAL(kind_phys), INTENT(IN) :: qv_spechum(:, :)
    REAL(kind_phys), INTENT(IN) :: t(:, :)
    REAL(kind_phys), INTENT(IN) :: cld1d(:)
    REAL(kind_phys), INTENT(IN) :: us(:, :)
    REAL(kind_phys), INTENT(IN) :: vs(:, :)
    REAL(kind_phys), INTENT(IN) :: t2di(:, :)
    REAL(kind_phys), INTENT(IN) :: w(:, :)
    REAL(kind_phys), INTENT(IN) :: qv2di_spechum(:, :)
    REAL(kind_phys), INTENT(IN) :: p2di(:, :)
    REAL(kind_phys), INTENT(IN) :: psuri(:)
    INTEGER, INTENT(IN) :: hbot(:)
    INTEGER, INTENT(IN) :: htop(:)
    INTEGER, INTENT(IN) :: kcnv(:)
    INTEGER, INTENT(IN) :: xland(:)
    REAL(kind_phys), INTENT(IN) :: hfx2(:)
    REAL(kind_phys), INTENT(IN) :: qfx2(:)
    REAL(kind_phys), INTENT(IN) :: aod_gf(:)
    REAL(kind_phys), INTENT(IN) :: cliw(:, :)
    REAL(kind_phys), INTENT(IN) :: clcw(:, :)
    REAL(kind_phys), INTENT(IN) :: pbl(:)
    REAL(kind_phys), INTENT(IN) :: ud_mf(:, :)
    REAL(kind_phys), INTENT(IN) :: dd_mf(:, :)
    REAL(kind_phys), INTENT(IN) :: dt_mf(:, :)
    REAL(kind_phys), INTENT(IN) :: cnvw_moist(:, :)
    REAL(kind_phys), INTENT(IN) :: cnvc(:, :)
    REAL(kind_phys), INTENT(IN) :: dtend(:, :, :)
    INTEGER, INTENT(IN) :: dtidx(:, :)
    REAL(kind_phys), INTENT(IN) :: qci_conv(:, :)
    INTEGER, INTENT(IN) :: ix_dfi_radar(:)
    REAL(kind_phys), INTENT(IN) :: fh_dfi_radar(:)
    REAL(kind_phys), INTENT(IN) :: cap_suppress(:, :)

    WRITE(*,'(A4)') "TEST"
    WRITE(*,'(A5,A117)') "TEST ", REPEAT("=",117)
    WRITE(*,'(A5,A32)') "TEST ", msg
    WRITE(*,'(A5,A117)') "TEST ", REPEAT("=",117)
    WRITE(*,'(A5,A17,5A20)') "TEST ", "Variable", "Min", "Max", "First", "Last", "RMS"
    WRITE(*,'(A5,A117)') "TEST ", REPEAT("-",117)

    CALL print_1d_variable("garea", garea)
    CALL print_1d_variable_int("cactiv", cactiv)
    CALL print_1d_variable_int("cactiv_m", cactiv_m)
    CALL print_2d_variable("forcet", forcet)
    CALL print_2d_variable("forceqv_spechum", forceqv_spechum)
    CALL print_2d_variable("phil", phil)
    CALL print_1d_variable("raincv", raincv)
    CALL print_2d_variable("qv_spechum", qv_spechum)
    CALL print_2d_variable("t", t)
    CALL print_1d_variable("cld1d", cld1d)
    CALL print_2d_variable("us", us)
    CALL print_2d_variable("vs", vs)
    CALL print_2d_variable("t2di", t2di)
    CALL print_2d_variable("w", w)
    CALL print_2d_variable("qv2di_spechum", qv2di_spechum)
    CALL print_2d_variable("p2di", p2di)
    CALL print_1d_variable("psuri", psuri)
    CALL print_1d_variable_int("hbot", hbot)
    CALL print_1d_variable_int("htop", htop)
    CALL print_1d_variable_int("kcnv", kcnv)
    CALL print_1d_variable_int("xland", xland)
    CALL print_1d_variable("hfx2", hfx2)
    CALL print_1d_variable("qfx2", qfx2)
    CALL print_1d_variable("aod_gf", aod_gf)
    CALL print_2d_variable("cliw", cliw)
    CALL print_2d_variable("clcw", clcw)
    CALL print_1d_variable("pbl", pbl)
    CALL print_2d_variable("ud_mf", ud_mf)
    CALL print_2d_variable("dd_mf", dd_mf)
    CALL print_2d_variable("dt_mf", dt_mf)
    CALL print_2d_variable("cnvw_moist", cnvw_moist)
    CALL print_2d_variable("cnvc", cnvc)
    CALL print_3d_variable("dtend", dtend)
    CALL print_2d_variable_int("dtidx", dtidx)
    CALL print_2d_variable("qci_conv", qci_conv)
    CALL print_1d_variable_int("ix_dfi_radar", ix_dfi_radar)
    CALL print_1d_variable("fh_dfi_radar", fh_dfi_radar)
    CALL print_2d_variable("cap_suppress", cap_suppress)

    WRITE(*,'(A5,A117)') "TEST ", REPEAT("-",117)
    WRITE(*,'(A4)') "TEST"

  END SUBROUTINE print_state

  !------------------------------------------------------------------
  ! print_1d_variable
  !
  ! Prints statistics for a 1d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_1d_variable(name, data)

    CHARACTER(LEN=*) :: name
    REAL(kind_phys)         :: data(:)

    ! Note: Assumed shape array sections always start with index=1 for all
    ! dimensions
    !       So we don't have to know start/end indices here
    WRITE(*,'(A5, A17,5ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1), &
                            data(SIZE(data,1)),            &
                            SQRT(SUM(data**2) / SIZE(data))

  END SUBROUTINE print_1d_variable

  !------------------------------------------------------------------
  ! print_2d_variable
  !
  ! Prints statistics for a 2d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_2d_variable(name, data)

    CHARACTER(LEN=*) :: name
    REAL(kind_phys)         :: data(:,:)

    ! Note: Assumed shape array sections always start with index=1 for all
    ! dimensions
    !       So we don't have to know start/end indices here
    WRITE(*,'(A5, A17,5ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1), &
                            data(SIZE(data,1), SIZE(data,2)),            &
                            SQRT(SUM(data**2) / SIZE(data))

  END SUBROUTINE print_2d_variable

  !------------------------------------------------------------------
  ! print_3d_variable
  !
  ! Prints statistics for a 3d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_3d_variable(name, data)

    CHARACTER(LEN=*) :: name
    REAL(kind_phys)         :: data(:,:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we do not have to know start/end indices here
    WRITE(*,'(A5,A17,5ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1,1),  &
                            data(SIZE(data,1), SIZE(data,2), SIZE(data,3)), &
                            SQRT(SUM(data**2) / SIZE(data))

  END SUBROUTINE print_3d_variable

  !------------------------------------------------------------------
  ! print_4d_variable
  !
  ! Prints statistics for a 4d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_4d_variable(name, data)

    CHARACTER(LEN=*) :: name
    REAL(kind_phys)         :: data(:,:,:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we do not have to know start/end indices here
    WRITE(*,'(A5,A17,5ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1,1,1),  &
                            data(SIZE(data,1), SIZE(data,2), SIZE(data,3), SIZE(data,4)), &
                            SQRT(SUM(data**2) / SIZE(data))

  END SUBROUTINE print_4d_variable


  !------------------------------------------------------------------
  ! print_5d_variable
  !
  ! Prints statistics for a 5d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_5d_variable(name, data)

    CHARACTER(LEN=*) :: name
    REAL(kind_phys)         :: data(:,:,:,:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we do not have to know start/end indices here
    WRITE(*,'(A5,A17,5ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1,1,1,1),  &
                            data(SIZE(data,1), SIZE(data,2), SIZE(data,3), SIZE(data,4), SIZE(data,5)), &
                            SQRT(SUM(data**2) / SIZE(data))

  END SUBROUTINE print_5d_variable

  !------------------------------------------------------------------
  ! print_1d_variable
  !
  ! Prints statistics for a 1d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_1d_variable_int(name, data)

    CHARACTER(LEN=*) :: name
    INTEGER         :: data(:)

    ! Note: Assumed shape array sections always start with index=1 for all
    ! dimensions
    !       So we don't have to know start/end indices here
    WRITE(*,'(A5, A17,4I20,ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1), &
                            data(SIZE(data,1)),            &
                            SQRT(REAL(SUM(data**2) / SIZE(data)))

  END SUBROUTINE print_1d_variable_int

  !------------------------------------------------------------------
  ! print_2d_variable
  !
  ! Prints statistics for a 2d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_2d_variable_int(name, data)

    CHARACTER(LEN=*) :: name
    INTEGER         :: data(:,:)

    ! Note: Assumed shape array sections always start with index=1 for all
    ! dimensions
    !       So we don't have to know start/end indices here
    WRITE(*,'(A5, A17,4I20,ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1), &
                            data(SIZE(data,1), SIZE(data,2)),            &
                            SQRT(REAL(SUM(data**2) / SIZE(data)))

  END SUBROUTINE print_2d_variable_int

  !------------------------------------------------------------------
  ! print_3d_variable
  !
  ! Prints statistics for a 3d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_3d_variable_int(name, data)

    CHARACTER(LEN=*) :: name
    REAL(kind_phys)         :: data(:,:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we do not have to know start/end indices here
    WRITE(*,'(A5,A17,4I20,ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1,1),  &
                            data(SIZE(data,1), SIZE(data,2), SIZE(data,3)), &
                            SQRT(REAL(SUM(data**2) / SIZE(data)))

  END SUBROUTINE print_3d_variable_int

  !------------------------------------------------------------------
  ! print_4d_variable
  !
  ! Prints statistics for a 4d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_4d_variable_int(name, data)

    CHARACTER(LEN=*) :: name
    REAL(kind_phys)         :: data(:,:,:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we do not have to know start/end indices here
    WRITE(*,'(A5,A17,4I20,ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1,1,1),  &
                            data(SIZE(data,1), SIZE(data,2), SIZE(data,3), SIZE(data,4)), &
                            SQRT(REAL(SUM(data**2) / SIZE(data)))

  END SUBROUTINE print_4d_variable_int


  !------------------------------------------------------------------
  ! print_5d_variable
  !
  ! Prints statistics for a 5d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_5d_variable_int(name, data)

    CHARACTER(LEN=*) :: name
    REAL(kind_phys)         :: data(:,:,:,:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we do not have to know start/end indices here
    WRITE(*,'(A5,A17,4I20,ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1,1,1,1),  &
                            data(SIZE(data,1), SIZE(data,2), SIZE(data,3), SIZE(data,4), SIZE(data,5)), &
                            SQRT(REAL(SUM(data**2) / SIZE(data)))

  END SUBROUTINE print_5d_variable_int


END MODULE gf_utils
