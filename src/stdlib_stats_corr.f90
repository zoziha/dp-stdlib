submodule (stdlib_stats) stdlib_stats_corr

  use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
  use stdlib_error, only: error_stop
  use stdlib_linalg, only: diag
  use stdlib_optval, only: optval
  implicit none

contains

    module function corr_1_rdp_rdp(x, dim, mask) result(res)
      real(dp), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      real(dp) :: res

      if (.not.optval(mask, .true.) .or. size(x) < 2) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      res = 1

    end function corr_1_rdp_rdp
    module function corr_1_cdp_cdp(x, dim, mask) result(res)
      complex(dp), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      real(dp) :: res

      if (.not.optval(mask, .true.) .or. size(x) < 2) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      res = 1

    end function corr_1_cdp_cdp


    module function corr_1_iint32_dp(x, dim, mask) result(res)
      integer(int32), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      real(dp) :: res

      if (.not.optval(mask, .true.) .or. size(x) < 2) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      res = 1

    end function corr_1_iint32_dp


    module function corr_mask_1_rdp_rdp(x, dim, mask) result(res)
      real(dp), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:)
      real(dp) :: res

      if (count(mask) < 2) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      res = 1

    end function corr_mask_1_rdp_rdp
    module function corr_mask_1_cdp_cdp(x, dim, mask) result(res)
      complex(dp), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:)
      real(dp) :: res

      if (count(mask) < 2) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      res = 1

    end function corr_mask_1_cdp_cdp


    module function corr_mask_1_iint32_dp(x, dim, mask) result(res)
      integer(int32), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:)
      real(dp) :: res

      if (count(mask) < 2) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      res = 1

    end function corr_mask_1_iint32_dp


    module function corr_2_rdp_rdp(x, dim, mask) result(res)
      real(dp), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                          , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i, j
      real(dp) :: mean_(merge(size(x, 1), size(x, 2), mask = 1<dim))
      real(dp) :: center(size(x, 1),size(x, 2))

      if (.not.optval(mask, .true.) .or. size(x) < 2) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      mean_ = mean(x, dim)
      select case(dim)
        case(1)
          do i = 1, size(x, 1)
            center(i, :) = x(i, :) - mean_
          end do
            res = matmul( transpose(center), center)
        case(2)
          do i = 1, size(x, 2)
            center(:, i) = x(:, i) - mean_
          end do
            res = matmul( center, transpose(center))
        case default
          call error_stop("ERROR (corr): wrong dimension")
      end select

      mean_ = 1 / sqrt(diag(res))
      do i = 1, size(res, 1)
        do j = 1, size(res, 2)
          res(j, i) = res(j, i) * mean_(i) * mean_(j)
        end do
      end do

    end function corr_2_rdp_rdp
    module function corr_2_cdp_cdp(x, dim, mask) result(res)
      complex(dp), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      complex(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                          , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i, j
      complex(dp) :: mean_(merge(size(x, 1), size(x, 2), mask = 1<dim))
      complex(dp) :: center(size(x, 1),size(x, 2))

      if (.not.optval(mask, .true.) .or. size(x) < 2) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      mean_ = mean(x, dim)
      select case(dim)
        case(1)
          do i = 1, size(x, 1)
            center(i, :) = x(i, :) - mean_
          end do
            res = matmul( transpose(conjg(center)), center)
        case(2)
          do i = 1, size(x, 2)
            center(:, i) = x(:, i) - mean_
          end do
            res = matmul( center, transpose(conjg(center)))
        case default
          call error_stop("ERROR (corr): wrong dimension")
      end select

      mean_ = 1 / sqrt(diag(res))
      do i = 1, size(res, 1)
        do j = 1, size(res, 2)
          res(j, i) = res(j, i) * mean_(i) * mean_(j)
        end do
      end do

    end function corr_2_cdp_cdp


    module function corr_2_iint32_dp(x, dim, mask) result(res)
      integer(int32), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                      , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i, j
      real(dp) :: mean_(merge(size(x, 1), size(x, 2), mask = 1<dim))
      real(dp) :: center(size(x, 1),size(x, 2))

      if (.not.optval(mask, .true.) .or. size(x) < 2) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      mean_ = mean(x, dim)
      select case(dim)
        case(1)
          do i = 1, size(x, 1)
            center(i, :) = real(x(i, :), dp) - mean_
          end do
          res = matmul( transpose(center), center)
        case(2)
          do i = 1, size(x, 2)
            center(:, i) = real(x(:, i), dp) - mean_
          end do
          res = matmul( center, transpose(center))
        case default
          call error_stop("ERROR (corr): wrong dimension")
      end select

      mean_ = 1 / sqrt(diag(res))
      do i = 1, size(res, 1)
        do j = 1, size(res, 2)
          res(j, i) = res(j, i) * mean_(i) * mean_(j)
        end do
      end do

    end function corr_2_iint32_dp


    module function corr_mask_2_rdp_rdp(x, dim, mask) result(res)
      real(dp), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:,:)
      real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                          , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i, j
      real(dp) :: centeri_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      real(dp) :: centerj_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      logical :: mask_(merge(size(x, 2), size(x, 1), mask = 1<dim))

      select case(dim)
        case(1)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
             mask_ = merge(.true., .false., mask(:, i) .and. mask(:, j))
             centeri_ = merge( x(:, i) - mean(x(:, i), mask = mask_),&
                0._dp,&
                mask_)
             centerj_ = merge( x(:, j) - mean(x(:, j), mask = mask_),&
                0._dp,&
                mask_)

              res(j, i) = dot_product( centerj_, centeri_)&
               /sqrt(dot_product( centeri_, centeri_)*&
                     dot_product( centerj_, centerj_))

            end do
          end do
        case(2)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
             mask_ = merge(.true., .false., mask(i, :) .and. mask(j, :))
             centeri_ = merge( x(i, :) - mean(x(i, :), mask = mask_),&
                0._dp,&
                mask_)
             centerj_ = merge( x(j, :) - mean(x(j, :), mask = mask_),&
                0._dp,&
                mask_)

              res(j, i) = dot_product( centeri_, centerj_)&
               /sqrt(dot_product( centeri_, centeri_)*&
                     dot_product( centerj_, centerj_))
            end do
          end do
        case default
          call error_stop("ERROR (corr): wrong dimension")
      end select

    end function corr_mask_2_rdp_rdp
    module function corr_mask_2_cdp_cdp(x, dim, mask) result(res)
      complex(dp), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:,:)
      complex(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                          , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i, j
      complex(dp) :: centeri_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      complex(dp) :: centerj_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      logical :: mask_(merge(size(x, 2), size(x, 1), mask = 1<dim))

      select case(dim)
        case(1)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
             mask_ = merge(.true., .false., mask(:, i) .and. mask(:, j))
             centeri_ = merge( x(:, i) - mean(x(:, i), mask = mask_),&
                cmplx(0,0,kind=dp),&
                mask_)
             centerj_ = merge( x(:, j) - mean(x(:, j), mask = mask_),&
                cmplx(0,0,kind=dp),&
                mask_)

              res(j, i) = dot_product( centerj_, centeri_)&
               /sqrt(dot_product( centeri_, centeri_)*&
                     dot_product( centerj_, centerj_))

            end do
          end do
        case(2)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
             mask_ = merge(.true., .false., mask(i, :) .and. mask(j, :))
             centeri_ = merge( x(i, :) - mean(x(i, :), mask = mask_),&
                cmplx(0,0,kind=dp),&
                mask_)
             centerj_ = merge( x(j, :) - mean(x(j, :), mask = mask_),&
                cmplx(0,0,kind=dp),&
                mask_)

              res(j, i) = dot_product( centeri_, centerj_)&
               /sqrt(dot_product( centeri_, centeri_)*&
                     dot_product( centerj_, centerj_))
            end do
          end do
        case default
          call error_stop("ERROR (corr): wrong dimension")
      end select

    end function corr_mask_2_cdp_cdp


    module function corr_mask_2_iint32_dp(x, dim, mask) result(res)
      integer(int32), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:,:)
      real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                          , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i, j
      real(dp) :: centeri_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      real(dp) :: centerj_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      logical :: mask_(merge(size(x, 2), size(x, 1), mask = 1<dim))

      select case(dim)
        case(1)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
             mask_ = merge(.true., .false., mask(:, i) .and. mask(:, j))
             centeri_ = merge( x(:, i) - mean(x(:, i), mask = mask_),&
                0._dp, mask_)
             centerj_ = merge( x(:, j) - mean(x(:, j), mask = mask_),&
                0._dp, mask_)

             res(j, i) = dot_product( centerj_, centeri_)&
                 /sqrt(dot_product( centeri_, centeri_)*&
                       dot_product( centerj_, centerj_))

            end do
          end do
        case(2)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
             mask_ = merge(.true., .false., mask(i, :) .and. mask(j, :))
             centeri_ = merge( x(i, :) - mean(x(i, :), mask = mask_),&
                0._dp, mask_)
             centerj_ = merge( x(j, :) - mean(x(j, :), mask = mask_),&
                0._dp, mask_)

             res(j, i) = dot_product( centeri_, centerj_)&
                 /sqrt(dot_product( centeri_, centeri_)*&
                       dot_product( centerj_, centerj_))
            end do
          end do
        case default
          call error_stop("ERROR (corr): wrong dimension")
      end select

    end function corr_mask_2_iint32_dp


end submodule
