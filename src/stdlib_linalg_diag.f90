submodule (stdlib_linalg) stdlib_linalg_diag

  implicit none

contains

      module function diag_rdp(v) result(res)
        real(dp), intent(in) :: v(:)
        real(dp) :: res(size(v),size(v))
        integer :: i
        res = 0
        do i = 1, size(v)
          res(i,i) = v(i)
        end do
      end function diag_rdp
      module function diag_cdp(v) result(res)
        complex(dp), intent(in) :: v(:)
        complex(dp) :: res(size(v),size(v))
        integer :: i
        res = 0
        do i = 1, size(v)
          res(i,i) = v(i)
        end do
      end function diag_cdp
      module function diag_iint32(v) result(res)
        integer(int32), intent(in) :: v(:)
        integer(int32) :: res(size(v),size(v))
        integer :: i
        res = 0
        do i = 1, size(v)
          res(i,i) = v(i)
        end do
      end function diag_iint32


      module function diag_rdp_k(v,k) result(res)
        real(dp), intent(in) :: v(:)
        integer, intent(in) :: k
        real(dp) :: res(size(v)+abs(k),size(v)+abs(k))
        integer :: i, sz
        sz = size(v)
        res = 0
        if (k > 0) then
          do i = 1, sz
              res(i,k+i) = v(i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i+abs(k),i) = v(i)
          end do
        else
          do i = 1, sz
              res(i,i) = v(i)
          end do
        end if
      end function diag_rdp_k
      module function diag_cdp_k(v,k) result(res)
        complex(dp), intent(in) :: v(:)
        integer, intent(in) :: k
        complex(dp) :: res(size(v)+abs(k),size(v)+abs(k))
        integer :: i, sz
        sz = size(v)
        res = 0
        if (k > 0) then
          do i = 1, sz
              res(i,k+i) = v(i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i+abs(k),i) = v(i)
          end do
        else
          do i = 1, sz
              res(i,i) = v(i)
          end do
        end if
      end function diag_cdp_k
      module function diag_iint32_k(v,k) result(res)
        integer(int32), intent(in) :: v(:)
        integer, intent(in) :: k
        integer(int32) :: res(size(v)+abs(k),size(v)+abs(k))
        integer :: i, sz
        sz = size(v)
        res = 0
        if (k > 0) then
          do i = 1, sz
              res(i,k+i) = v(i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i+abs(k),i) = v(i)
          end do
        else
          do i = 1, sz
              res(i,i) = v(i)
          end do
        end if
      end function diag_iint32_k

      module function diag_rdp_mat(A) result(res)
        real(dp), intent(in) :: A(:,:)
        real(dp) :: res(minval(shape(A)))
        integer :: i
        do i = 1, minval(shape(A))
          res(i) = A(i,i)
        end do
      end function diag_rdp_mat
      module function diag_cdp_mat(A) result(res)
        complex(dp), intent(in) :: A(:,:)
        complex(dp) :: res(minval(shape(A)))
        integer :: i
        do i = 1, minval(shape(A))
          res(i) = A(i,i)
        end do
      end function diag_cdp_mat
      module function diag_iint32_mat(A) result(res)
        integer(int32), intent(in) :: A(:,:)
        integer(int32) :: res(minval(shape(A)))
        integer :: i
        do i = 1, minval(shape(A))
          res(i) = A(i,i)
        end do
      end function diag_iint32_mat

      module function diag_rdp_mat_k(A,k) result(res)
        real(dp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        real(dp) :: res(minval(shape(A))-abs(k))
        integer :: i, sz
        sz = minval(shape(A))-abs(k)
        if (k > 0) then
          do i = 1, sz
              res(i) = A(i,k+i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i) = A(i+abs(k),i)
          end do
        else
          do i = 1, sz
              res(i) = A(i,i)
          end do
        end if
      end function diag_rdp_mat_k
      module function diag_cdp_mat_k(A,k) result(res)
        complex(dp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        complex(dp) :: res(minval(shape(A))-abs(k))
        integer :: i, sz
        sz = minval(shape(A))-abs(k)
        if (k > 0) then
          do i = 1, sz
              res(i) = A(i,k+i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i) = A(i+abs(k),i)
          end do
        else
          do i = 1, sz
              res(i) = A(i,i)
          end do
        end if
      end function diag_cdp_mat_k
      module function diag_iint32_mat_k(A,k) result(res)
        integer(int32), intent(in) :: A(:,:)
        integer, intent(in) :: k
        integer(int32) :: res(minval(shape(A))-abs(k))
        integer :: i, sz
        sz = minval(shape(A))-abs(k)
        if (k > 0) then
          do i = 1, sz
              res(i) = A(i,k+i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i) = A(i+abs(k),i)
          end do
        else
          do i = 1, sz
              res(i) = A(i,i)
          end do
        end if
      end function diag_iint32_mat_k

end submodule
