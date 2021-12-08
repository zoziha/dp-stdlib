module stdlib_stats_distribution_uniform
    use stdlib_kinds
    use stdlib_error, only : error_stop
    use stdlib_random, only : dist_rand

    implicit none
    private

    real(dp), parameter  :: MESENNE_NUMBER = 1.0_dp / (2.0_dp ** 53 - 1.0_dp)
    integer(int64), parameter :: INT_ONE = 1_int64

    public :: rvs_uniform
    public :: pdf_uniform
    public :: cdf_uniform
    public :: shuffle


    interface rvs_uniform
    !! version: experimental
    !!
    !! Get uniformly distributed random variate for integer, real and complex
    !! variables.
    !! ([Specification](../page/specs/stdlib_stats_distribution_uniform.html#
    !! rvs_uniform-uniform-distribution-random-variates))

        module procedure rvs_unif_0_rsp                 ! 0 dummy variable

        module procedure rvs_unif_1_iint32     ! 1 dummy variable
        module procedure rvs_unif_1_rdp     ! 1 dummy variable
        module procedure rvs_unif_1_cdp     ! 1 dummy variable

        module procedure rvs_unif_iint32       ! 2 dummy variables
        module procedure rvs_unif_rdp       ! 2 dummy variables
        module procedure rvs_unif_cdp       ! 2 dummy variables

        module procedure rvs_unif_array_iint32 ! 3 dummy variables
        module procedure rvs_unif_array_rdp ! 3 dummy variables
        module procedure rvs_unif_array_cdp ! 3 dummy variables
    end interface rvs_uniform


    interface pdf_uniform
    !! version: experimental
    !!
    !! Get uniform distribution probability density (pdf) for integer, real and
    !! complex variables.
    !! ([Specification](../page/specs/stdlib_stats_distribution_uniform.html#
    !! pdf_uniform-uniform-probability-density-function))

        module procedure pdf_unif_iint32
        module procedure pdf_unif_rdp
        module procedure pdf_unif_cdp
    end interface pdf_uniform


    interface cdf_uniform
    !! version: experimental
    !!
    !! Get uniform distribution cumulative distribution function (cdf) for integer,
    !! real and complex variables.
    !! ([Specification](../page/specs/stdlib_stats_distribution_uniform.html#
    !! cdf_uniform-uniform-cumulative-distribution-function))
    !!
        module procedure cdf_unif_iint32
        module procedure cdf_unif_rdp
        module procedure cdf_unif_cdp
    end interface cdf_uniform


    interface shuffle
    !! version: experimental
    !!
    !! Fisher-Yates shuffle algorithm for a rank one array of integer, real and
    !! complex variables.
    !! ([Specification](../page/specs/stdlib_stats_distribution_uniform.html#
    !! shuffle-using-fisher-yates-algorithm-to-generate-a-random-permutation-of-a-list))
    !!
        module procedure shuffle_iint32
        module procedure shuffle_rdp
        module procedure shuffle_cdp
    end interface shuffle






contains


    impure elemental function rvs_unif_1_iint32(scale) result(res)
    !
    ! Uniformly distributed integer in [0, scale]
    ! Bitmask with rejection
    ! https://www.pcg-random.org/posts/bounded-rands.html
    !
    ! Fortran 90 translated from c by Jim-215-fisher
    !
        integer(int32), intent(in) :: scale
        integer(int32) ::  res, u, mask
        integer :: zeros, bits_left, bits

        if(scale <= 0_int32) call error_stop("Error(rvs_unif_1): Uniform"     &
            //" distribution scale parameter must be positive")
        zeros = leadz(scale)
        bits = bit_size(scale) - zeros
        mask = shiftr(not(0_int32), zeros)
        L1 : do
            u = dist_rand(scale)
            res = iand(u, mask)
            if(res <= scale) exit L1
            bits_left = zeros
            L2 : do
                if(bits_left < bits) exit L2
                u = shiftr(u, bits)
                res = iand(u, mask)
                if(res <= scale) exit L1
                bits_left = bits_left - bits
            end do L2
        end do L1
    end function rvs_unif_1_iint32




    impure elemental function rvs_unif_iint32(loc, scale) result(res)
    !
    ! Uniformly distributed integer in [loc, loc + scale]
    !
        integer(int32), intent(in) :: loc, scale
        integer(int32)  ::  res

        if(scale <= 0_int32) call error_stop("Error(rvs_unif): Uniform"       &
            //" distribution scale parameter must be positive")
        res = loc + rvs_unif_1_iint32(scale)
    end function rvs_unif_iint32




    impure elemental function rvs_unif_0_rdp( ) result(res)
    !
    ! Uniformly distributed float in [0,1]
    ! Based on the paper by Frederic Goualard, "Generating Random Floating-
    ! Point Numbers By Dividing Integers: a Case Study", Proceedings of
    ! ICCS 2020, June 2020, Amsterdam, Netherlands
    !
        real(dp)  ::  res
        integer(int64) :: tmp

        tmp = shiftr(dist_rand(INT_ONE), 11)        ! Get random from [0,2^53-1]
        res = real(tmp * MESENNE_NUMBER, kind = dp) ! convert to [0,1]
    end function rvs_unif_0_rdp




    impure elemental function rvs_unif_1_rdp(scale) result(res)
    !
    ! Uniformly distributed float in [0, scale]
    !
        real(dp), intent(in) :: scale
        real(dp)  ::  res

        if(scale == 0._dp) call error_stop("Error(rvs_unif_1): "           &
            //"Uniform distribution scale parameter must be non-zero")
        res = scale * rvs_unif_0_rdp( )
    end function rvs_unif_1_rdp




    impure elemental function rvs_unif_rdp(loc, scale) result(res)
    !
    ! Uniformly distributed float in [loc, loc + scale]
    !
        real(dp), intent(in) :: loc, scale
        real(dp)  ::  res

        if(scale == 0._dp) call error_stop("Error(rvs_unif): "             &
           //"Uniform distribution scale parameter must be non-zero")
        res = loc + scale * rvs_unif_0_rdp( )
    end function rvs_unif_rdp




    impure elemental function rvs_unif_1_cdp(scale) result(res)
    !
    ! Uniformly distributed complex in [(0,0i), (scale, i(scale))]
    ! The real part and imaginary part are independent of each other, so that
    ! the joint distribution is on an unit square [(0,0i), (scale,i(scale))]
    !
        complex(dp), intent(in) :: scale
        complex(dp) :: res
        real(dp) :: r1, tr, ti

        if(scale == (0.0_dp, 0.0_dp)) call error_stop("Error(rvs_uni_" &
           //"1): Uniform distribution scale parameter must be non-zero")
        r1 = rvs_unif_0_rdp( )
        if(scale % re == 0.0_dp) then
            ti = scale % im * r1
            tr = 0.0_dp
        else if(scale % im == 0.0_dp) then
            tr = scale % re * r1
            ti = 0.0_dp
        else
            tr = scale % re * r1
            r1 = rvs_unif_0_rdp( )
            ti = scale % im * r1
        end if
        res = cmplx(tr, ti, kind=dp)
    end function rvs_unif_1_cdp




    impure elemental function rvs_unif_cdp(loc, scale) result(res)
    !
    ! Uniformly distributed complex in [(loc,iloc), (loc + scale, i(loc +
    ! scale))].
    ! The real part and imaginary part are independent of each other, so that
    ! the joint distribution is on an unit square [(loc,iloc), (loc + scale,
    ! i(loc + scale))]
    !
        complex(dp), intent(in) :: loc, scale
        complex(dp) :: res
        real(dp) :: r1, tr, ti

        if(scale == (0.0_dp, 0.0_dp)) call error_stop("Error(rvs_uni_" &
            //"): Uniform distribution scale parameter must be non-zero")
        r1 = rvs_unif_0_rdp( )
        if(scale % re == 0.0_dp) then
            tr = loc % re
            ti = loc % im + scale % im * r1
        else if(scale % im == 0.0_dp) then
            tr = loc % re + scale % re * r1
            ti = loc % im
        else
            tr = loc % re + scale % re * r1
            r1 = rvs_unif_0_rdp( )
            ti = loc % im + scale % im * r1
        end if
        res = cmplx(tr, ti, kind=dp)
    end function rvs_unif_cdp




    function rvs_unif_array_iint32(loc, scale, array_size) result(res)

        integer, intent(in) :: array_size
        integer(int32), intent(in) :: loc, scale
        integer(int32) :: res(array_size)
        integer(int32) :: u, mask, nn
        integer :: i, zeros, bits_left, bits

        if(scale == 0_int32) call error_stop("Error(rvs_unif_array): "        &
            //"Uniform distribution scale parameter must be non-zero")
        zeros = leadz(scale)
        bits = bit_size(scale) - zeros
        mask = shiftr(not(0_int32), zeros)
        do i = 1, array_size
            L1 : do
                u = dist_rand(scale)
                nn = iand(u, mask)
                if(nn <= scale) exit L1
                bits_left = zeros
                L2 : do
                    if(bits_left < bits) exit L2
                    u = shiftr(u, bits)
                    nn = iand(u, mask)
                    if(nn <= scale) exit L1
                    bits_left = bits_left - bits
                end do L2
            end do L1
            res(i) = loc + nn
        end do
    end function rvs_unif_array_iint32




    function rvs_unif_array_rdp(loc, scale, array_size) result(res)

        integer, intent(in) :: array_size
        real(dp), intent(in) :: loc, scale
        real(dp) :: res(array_size)
        real(dp) :: t
        integer(int64) :: tmp
        integer :: i


        if(scale == 0._dp) call error_stop("Error(rvs_unif_array):"        &
           //" Uniform distribution scale parameter must be non-zero")
        do i = 1, array_size
            tmp = shiftr(dist_rand(INT_ONE), 11)
            t = real(tmp * MESENNE_NUMBER, kind = dp)
            res(i) = loc + scale * t
        end do
    end function rvs_unif_array_rdp




    function rvs_unif_array_cdp(loc, scale, array_size) result(res)

        integer, intent(in) :: array_size
        complex(dp), intent(in) :: loc, scale
        complex(dp) :: res(array_size)
        real(dp) :: r1, tr, ti
        integer(int64) :: tmp
        integer :: i


        if(scale == (0.0_dp, 0.0_dp)) call error_stop("Error(rvs_unif" &
           //"_array): Uniform distribution scale parameter must be non-zero")
        do i = 1, array_size
            tmp = shiftr(dist_rand(INT_ONE), 11)
            r1 = real(tmp * MESENNE_NUMBER, kind = dp)
            if(scale % re == 0.0_dp) then
                tr = loc % re
                ti = loc % im + scale % im * r1
            else if(scale % im == 0.0_dp) then
                tr = loc % re + scale % re * r1
                ti = loc % im
            else
                tr = loc % re + scale % re * r1
                tmp = shiftr(dist_rand(INT_ONE), 11)
                r1 = real(tmp * MESENNE_NUMBER, kind = dp)
                ti = loc % im + scale % im * r1
            end if
            res(i) = cmplx(tr, ti, kind=dp)
        end do
    end function rvs_unif_array_cdp




    elemental function pdf_unif_iint32(x, loc, scale) result(res)

        integer(int32), intent(in) :: x, loc, scale
        real :: res

        if(scale == 0_int32) then
            res = 0.0
        else if(x < loc .or. x > (loc + scale)) then
            res = 0.0
        else
            res = 1. / (scale + 1_int32)
        end if
    end function pdf_unif_iint32




    elemental function pdf_unif_rdp(x, loc, scale) result(res)

        real(dp), intent(in) :: x, loc, scale
        real :: res

        if(scale == 0.0_dp) then
            res = 0.0
        else if(x < loc .or. x > (loc + scale)) then
            res = 0.0
        else
            res = 1.0 / scale
        end if
    end function pdf_unif_rdp




    elemental function pdf_unif_cdp(x, loc, scale) result(res)

        complex(dp), intent(in) :: x, loc, scale
        real :: res
        real(dp) :: tr, ti

        tr = loc % re + scale % re; ti = loc % im + scale % im
        if(scale == (0.0_dp,0.0_dp)) then
            res = 0.0
        else if((x % re >= loc % re .and. x % re <= tr) .and.                  &
            (x % im >= loc % im .and. x % im <= ti)) then
            res = 1.0 / (scale % re * scale % im)
        else
            res = 0.0
        end if
    end function pdf_unif_cdp




    elemental function cdf_unif_iint32(x, loc, scale) result(res)

        integer(int32), intent(in) :: x, loc, scale
        real :: res

        if(scale == 0_int32) then
            res = 0.0
        else if(x < loc) then
            res = 0.0
        else if(x >= loc .and. x <= (loc + scale)) then
            res = real((x - loc + 1_int32)) / real((scale + 1_int32))
        else
            res = 1.0
        end if
    end function cdf_unif_iint32




    elemental function cdf_unif_rdp(x, loc, scale) result(res)

        real(dp), intent(in) :: x, loc, scale
        real :: res

        if(scale == 0.0_dp) then
            res = 0.0
        else if(x < loc) then
            res = 0.0
        else if(x >= loc .and. x <= (loc + scale)) then
            res = (x - loc) / scale
        else
            res = 1.0
        end if
    end function cdf_unif_rdp




    elemental function cdf_unif_cdp(x, loc, scale) result(res)

        complex(dp), intent(in) :: x, loc, scale
        real :: res
        logical :: r1, r2, i1, i2

        if(scale == (0.0_dp,0.0_dp)) then
            res = 0.0
            return
        end if
        r1 = x % re < loc % re
        r2 = x % re > (loc % re + scale % re)
        i1 = x % im < loc % im
        i2 = x % im > (loc % im + scale % im)
        if(r1 .or. i1) then
            res = 0.0
        else if((.not. r1) .and. (.not. r2) .and. i2) then
            res = (x % re - loc % re) / scale % re
        else if((.not. i1) .and. (.not. i2) .and. r2) then
            res = (x % im - loc % im) / scale % im
        else if((.not. r1) .and. (.not. r2) .and. (.not. i1) .and. (.not. i2)) &
            then
            res = (x % re - loc % re) * (x % im - loc % im) /                  &
                   (scale % re * scale % im)
        else if(r2 .and. i2)then
             res = 1.0
        end if
    end function cdf_unif_cdp




    function shuffle_iint32( list ) result(res)

        integer(int32), intent(in) :: list(:)
        integer(int32) :: res(size(list))
        integer(int32) :: tmp
        integer :: n, i, j

        n = size(list)
        res = list
        do i = 1, n - 1
            j = rvs_uniform(n - i) + i
            tmp = res(i)
            res(i) = res(j)
            res(j) = tmp
        end do
    end function shuffle_iint32

    function shuffle_rdp( list ) result(res)

        real(dp), intent(in) :: list(:)
        real(dp) :: res(size(list))
        real(dp) :: tmp
        integer :: n, i, j

        n = size(list)
        res = list
        do i = 1, n - 1
            j = rvs_uniform(n - i) + i
            tmp = res(i)
            res(i) = res(j)
            res(j) = tmp
        end do
    end function shuffle_rdp

    function shuffle_cdp( list ) result(res)

        complex(dp), intent(in) :: list(:)
        complex(dp) :: res(size(list))
        complex(dp) :: tmp
        integer :: n, i, j

        n = size(list)
        res = list
        do i = 1, n - 1
            j = rvs_uniform(n - i) + i
            tmp = res(i)
            res(i) = res(j)
            res(j) = tmp
        end do
    end function shuffle_cdp

end module stdlib_stats_distribution_uniform
