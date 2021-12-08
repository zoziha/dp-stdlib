submodule (stdlib_linalg) stdlib_linalg_outer_product

  implicit none

contains

    pure module function outer_product_rdp(u, v) result(res)
      real(dp), intent(in) :: u(:), v(:)
      real(dp) :: res(size(u),size(v))
      integer :: col
      do col = 1, size(v)
        res(:,col) = v(col) * u
      end do
    end function outer_product_rdp
    pure module function outer_product_cdp(u, v) result(res)
      complex(dp), intent(in) :: u(:), v(:)
      complex(dp) :: res(size(u),size(v))
      integer :: col
      do col = 1, size(v)
        res(:,col) = v(col) * u
      end do
    end function outer_product_cdp
    pure module function outer_product_iint32(u, v) result(res)
      integer(int32), intent(in) :: u(:), v(:)
      integer(int32) :: res(size(u),size(v))
      integer :: col
      do col = 1, size(v)
        res(:,col) = v(col) * u
      end do
    end function outer_product_iint32

end submodule
