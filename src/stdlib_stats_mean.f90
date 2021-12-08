submodule (stdlib_stats) stdlib_stats_mean

  use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
  use stdlib_error, only: error_stop
  use stdlib_optval, only: optval
  implicit none

contains

      module function mean_all_1_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_1_rdp_rdp
      module function mean_all_2_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_2_rdp_rdp
      module function mean_all_3_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_3_rdp_rdp
      module function mean_all_4_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_4_rdp_rdp
      module function mean_all_1_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_1_cdp_cdp
      module function mean_all_2_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_2_cdp_cdp
      module function mean_all_3_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_3_cdp_cdp
      module function mean_all_4_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_4_cdp_cdp

      module function mean_all_1_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_1_iint32_dp
      module function mean_all_2_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_2_iint32_dp
      module function mean_all_3_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_3_iint32_dp
      module function mean_all_4_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_4_iint32_dp


      module function mean_1_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 1) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_1_rdp_rdp
      module function mean_2_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_2_rdp_rdp
      module function mean_3_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_3_rdp_rdp
      module function mean_4_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_4_rdp_rdp
      module function mean_1_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 1) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_1_cdp_cdp
      module function mean_2_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_2_cdp_cdp
      module function mean_3_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_3_cdp_cdp
      module function mean_4_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_4_cdp_cdp


      module function mean_1_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 1) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_1_iint32_dp
      module function mean_2_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_2_iint32_dp
      module function mean_3_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_3_iint32_dp
      module function mean_4_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_4_iint32_dp

      module function mean_mask_all_1_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_1_rdp_rdp
      module function mean_mask_all_2_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_2_rdp_rdp
      module function mean_mask_all_3_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_3_rdp_rdp
      module function mean_mask_all_4_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_4_rdp_rdp
      module function mean_mask_all_1_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_1_cdp_cdp
      module function mean_mask_all_2_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_2_cdp_cdp
      module function mean_mask_all_3_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_3_cdp_cdp
      module function mean_mask_all_4_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_4_cdp_cdp


      module function mean_mask_all_1_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_1_iint32_dp
      module function mean_mask_all_2_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_2_iint32_dp
      module function mean_mask_all_3_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_3_iint32_dp
      module function mean_mask_all_4_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_4_iint32_dp

      module function  mean_mask_1_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res

        if (dim >= 1 .and. dim <= 1) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_1_rdp_rdp
      module function  mean_mask_2_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_2_rdp_rdp
      module function  mean_mask_3_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_3_rdp_rdp
      module function  mean_mask_4_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_4_rdp_rdp
      module function  mean_mask_1_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        complex(dp) :: res

        if (dim >= 1 .and. dim <= 1) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_1_cdp_cdp
      module function  mean_mask_2_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_2_cdp_cdp
      module function  mean_mask_3_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_3_cdp_cdp
      module function  mean_mask_4_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_4_cdp_cdp


      module function mean_mask_1_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res

        if (dim >= 1 .and. dim <= 1) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_1_iint32_dp
      module function mean_mask_2_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_2_iint32_dp
      module function mean_mask_3_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_3_iint32_dp
      module function mean_mask_4_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_4_iint32_dp

end submodule
