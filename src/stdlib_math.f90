
module stdlib_math
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp, xdp, qp
    use stdlib_optval, only: optval

    implicit none
    private
    public :: clip, gcd, linspace, logspace
    public :: EULERS_NUMBER_SP, EULERS_NUMBER_DP
    public :: DEFAULT_LINSPACE_LENGTH, DEFAULT_LOGSPACE_BASE, DEFAULT_LOGSPACE_LENGTH
    public :: arange

    integer, parameter :: DEFAULT_LINSPACE_LENGTH = 100
    integer, parameter :: DEFAULT_LOGSPACE_LENGTH = 50
    integer, parameter :: DEFAULT_LOGSPACE_BASE = 10

    ! Useful constants for lnspace
    real(sp), parameter :: EULERS_NUMBER_SP = exp(1.0_sp)
    real(dp), parameter :: EULERS_NUMBER_DP = exp(1.0_dp)

    interface clip
        module procedure clip_int32
        module procedure clip_dp
    end interface clip

    !> Returns the greatest common divisor of two integers
    !> ([Specification](../page/specs/stdlib_math.html#gcd))
    !>
    !> Version: experimental
    interface gcd
        module procedure gcd_int32
    end interface gcd

    interface linspace
    !! Version: Experimental
    !!
    !! Create rank 1 array of linearly spaced elements
    !! If the number of elements is not specified, create an array with size 100. If n is a negative value,
    !! return an array with size 0. If n = 1, return an array whose only element is end
    !!([Specification](../page/specs/stdlib_math.html#linspace-create-a-linearly-spaced-rank-one-array))
      pure module function linspace_default_1_rdp_rdp(start, end) result(res)
        real(dp), intent(in) :: start
        real(dp), intent(in) :: end

        real(dp) :: res(DEFAULT_LINSPACE_LENGTH)
      end function linspace_default_1_rdp_rdp
      pure module function linspace_default_1_cdp_cdp(start, end) result(res)
        complex(dp), intent(in) :: start
        complex(dp), intent(in) :: end

        complex(dp) :: res(DEFAULT_LINSPACE_LENGTH)
      end function linspace_default_1_cdp_cdp

      pure module function linspace_n_1_rdp_rdp(start, end, n) result(res)
        real(dp), intent(in) :: start
        real(dp), intent(in) :: end
        integer, intent(in) :: n

        real(dp) :: res(max(n, 0))
      end function linspace_n_1_rdp_rdp
      pure module function linspace_n_1_cdp_cdp(start, end, n) result(res)
        complex(dp), intent(in) :: start
        complex(dp), intent(in) :: end
        integer, intent(in) :: n

        complex(dp) :: res(max(n, 0))
      end function linspace_n_1_cdp_cdp


    ! Add support for integer linspace
    !!
    !! When dealing with integers as the `start` and `end` parameters, the return type is always a `real(dp)`.
      pure module function linspace_default_1_iint32_iint32(start, end) result(res)
        integer(int32), intent(in) :: start
        integer(int32), intent(in) :: end

        real(dp) :: res(DEFAULT_LINSPACE_LENGTH)
      end function linspace_default_1_iint32_iint32

      pure module function linspace_n_1_iint32_iint32(start, end, n) result(res)
        integer(int32), intent(in) :: start
        integer(int32), intent(in) :: end
        integer, intent(in) :: n

        real(dp) :: res(max(n, 0))
      end function linspace_n_1_iint32_iint32

  end interface

  interface logspace
  !! Version: Experimental
  !!
  !! Create rank 1 array of logarithmically spaced elements from base**start to base**end.
  !! If the number of elements is not specified, create an array with size 50. If n is a negative value,
  !! return an array with size 0. If n = 1, return an array whose only element is base**end. If no base
  !! is specified, logspace will default to using a base of 10
  !!
  !!([Specification](../page/specs/stdlib_math.html#logspace-create-a-logarithmically-spaced-rank-one-array))
    pure module function logspace_1_rdp_default(start, end) result(res)

      real(dp), intent(in) :: start
      real(dp), intent(in) :: end

      real(dp) :: res(DEFAULT_LOGSPACE_LENGTH)

    end function logspace_1_rdp_default
    pure module function logspace_1_cdp_default(start, end) result(res)

      complex(dp), intent(in) :: start
      complex(dp), intent(in) :: end

      complex(dp) :: res(DEFAULT_LOGSPACE_LENGTH)

    end function logspace_1_cdp_default
    pure module function logspace_1_iint32_default(start, end) result(res)

      integer, intent(in) :: start
      integer, intent(in) :: end

      real(dp) :: res(DEFAULT_LOGSPACE_LENGTH)

  end function logspace_1_iint32_default

    pure module function logspace_1_rdp_n(start, end, n) result(res)
      real(dp), intent(in) :: start
      real(dp), intent(in) :: end
      integer, intent(in) :: n

      real(dp) :: res(max(n, 0))
    end function logspace_1_rdp_n
    pure module function logspace_1_cdp_n(start, end, n) result(res)
      complex(dp), intent(in) :: start
      complex(dp), intent(in) :: end
      integer, intent(in) :: n

      complex(dp) :: res(max(n, 0))
    end function logspace_1_cdp_n
    pure module function logspace_1_iint32_n(start, end, n) result(res)
      integer, intent(in) :: start
      integer, intent(in) :: end
      integer, intent(in) :: n

      real(dp) :: res(n)
    end function logspace_1_iint32_n

    ! Generate logarithmically spaced sequence from dp base to the powers
    ! of dp start and end. [base^start, ... , base^end]
    ! Different combinations of parameter types will lead to different result types.
    ! Those combinations are indicated in the body of each function.
    pure module function logspace_1_rdp_n_rbase(start, end, n, base) result(res)
      real(dp), intent(in) :: start
      real(dp), intent(in) :: end
      integer, intent(in) :: n
      real(dp), intent(in) :: base
      ! real(dp) endpoints + real(dp) base = real(dp) result
      real(dp) :: res(max(n, 0))
    end function logspace_1_rdp_n_rbase

    pure module function logspace_1_rdp_n_cbase(start, end, n, base) result(res)
      real(dp), intent(in) :: start
      real(dp), intent(in) :: end
      integer, intent(in) :: n
      complex(dp), intent(in) :: base
      ! real(dp) endpoints + complex(dp) base = complex(dp) result
      real(dp) :: res(max(n, 0))
    end function logspace_1_rdp_n_cbase

    pure module function logspace_1_rdp_n_ibase(start, end, n, base) result(res)
      real(dp), intent(in) :: start
      real(dp), intent(in) :: end
      integer, intent(in) :: n
      integer, intent(in) :: base
      ! real(dp) endpoints + integer base = real(dp) result
      real(dp) :: res(max(n, 0))
    end function logspace_1_rdp_n_ibase
    ! Generate logarithmically spaced sequence from dp base to the powers
    ! of dp start and end. [base^start, ... , base^end]
    ! Different combinations of parameter types will lead to different result types.
    ! Those combinations are indicated in the body of each function.
    pure module function logspace_1_cdp_n_rbase(start, end, n, base) result(res)
      complex(dp), intent(in) :: start
      complex(dp), intent(in) :: end
      integer, intent(in) :: n
      real(dp), intent(in) :: base
      ! complex(dp) endpoints + real(dp) base = complex(dp) result
      complex(dp) :: res(max(n, 0))
    end function logspace_1_cdp_n_rbase

    pure module function logspace_1_cdp_n_cbase(start, end, n, base) result(res)
      complex(dp), intent(in) :: start
      complex(dp), intent(in) :: end
      integer, intent(in) :: n
      complex(dp), intent(in) :: base
      ! complex(dp) endpoints + complex(dp) base = complex(dp) result
      complex(dp) :: res(max(n, 0))
    end function logspace_1_cdp_n_cbase

    pure module function logspace_1_cdp_n_ibase(start, end, n, base) result(res)
      complex(dp), intent(in) :: start
      complex(dp), intent(in) :: end
      integer, intent(in) :: n
      integer, intent(in) :: base
      ! complex(dp) endpoints + integer base = complex(dp) result
      complex(dp) :: res(max(n, 0))
    end function logspace_1_cdp_n_ibase
    ! Generate logarithmically spaced sequence from dp base to the powers
    ! of dp start and end. [base^start, ... , base^end]
    ! Different combinations of parameter types will lead to different result types.
    ! Those combinations are indicated in the body of each function.
    pure module function logspace_1_iint32_n_rdpbase(start, end, n, base) result(res)
      integer, intent(in) :: start
      integer, intent(in) :: end
      integer, intent(in) :: n
      real(dp), intent(in) :: base
      ! integer endpoints + real(dp) base = real(dp) result
      real(dp) :: res(max(n, 0))
    end function logspace_1_iint32_n_rdpbase

    pure module function logspace_1_iint32_n_cdpbase(start, end, n, base) result(res)
      integer, intent(in) :: start
      integer, intent(in) :: end
      integer, intent(in) :: n
      complex(dp), intent(in) :: base
      ! integer endpoints + complex(dp) base = complex(dp) result
      complex(dp) :: res(max(n, 0))
    end function logspace_1_iint32_n_cdpbase

    pure module function logspace_1_iint32_n_ibase(start, end, n, base) result(res)
      integer, intent(in) :: start
      integer, intent(in) :: end
      integer, intent(in) :: n
      integer, intent(in) :: base
      ! integer endpoints + integer base = integer result
      integer :: res(max(n, 0))
    end function logspace_1_iint32_n_ibase


  end interface

    !> Version: experimental
    !>
    !> `arange` creates a one-dimensional `array` of the `integer/real` type 
    !>  with fixed-spaced values of given spacing, within a given interval.
    !> ([Specification](../page/specs/stdlib_math.html#arange))
    interface arange
        pure module function arange_r_dp(start, end, step) result(result)
            real(dp), intent(in) :: start
            real(dp), intent(in), optional :: end, step
            real(dp), allocatable :: result(:)
        end function arange_r_dp
        pure module function arange_i_int32(start, end, step) result(result)
            integer(int32), intent(in) :: start
            integer(int32), intent(in), optional :: end, step
            integer(int32), allocatable :: result(:)
        end function arange_i_int32
    end interface arange

contains

    elemental function clip_int32(x, xmin, xmax) result(res)
        integer(int32), intent(in) :: x
        integer(int32), intent(in) :: xmin
        integer(int32), intent(in) :: xmax
        integer(int32) :: res

        res = max(min(x, xmax), xmin)
    end function clip_int32

    elemental function clip_dp(x, xmin, xmax) result(res)
        real(dp), intent(in) :: x
        real(dp), intent(in) :: xmin
        real(dp), intent(in) :: xmax
        real(dp) :: res

        res = max(min(x, xmax), xmin)
    end function clip_dp


    !> Returns the greatest common divisor of two integers of kind int32
    !> using the Euclidean algorithm.
    elemental function gcd_int32(a, b) result(res)
        integer(int32), intent(in) :: a
        integer(int32), intent(in) :: b
        integer(int32) :: res

        integer(int32) :: rem, tmp

        rem = min(abs(a), abs(b))
        res = max(abs(a), abs(b))
        do while (rem /= 0_int32)
          tmp = rem
          rem = mod(res, rem)
          res = tmp
        end do
    end function gcd_int32

end module stdlib_math
