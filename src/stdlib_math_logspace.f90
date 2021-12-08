
submodule (stdlib_math) stdlib_math_logspace

implicit none

contains

    module procedure logspace_1_rdp_default
      res = logspace(start, end, DEFAULT_LOGSPACE_LENGTH, real(DEFAULT_LOGSPACE_BASE, dp))
    end procedure
    module procedure logspace_1_cdp_default
      res = logspace(start, end, DEFAULT_LOGSPACE_LENGTH, real(DEFAULT_LOGSPACE_BASE, dp))
    end procedure
    module procedure logspace_1_iint32_default
      res = logspace(start, end, DEFAULT_LOGSPACE_LENGTH, DEFAULT_LOGSPACE_BASE)
    end procedure

    module procedure logspace_1_rdp_n
      res = logspace(start, end, n, real(DEFAULT_LOGSPACE_BASE, dp))
    end procedure
    module procedure logspace_1_cdp_n
      res = logspace(start, end, n, real(DEFAULT_LOGSPACE_BASE, dp))
    end procedure
    module procedure logspace_1_iint32_n
      res = logspace(start, end, n, DEFAULT_LOGSPACE_BASE)
    end procedure

    module procedure logspace_1_rdp_n_rbase
      real(dp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_rdp_n_cbase
      real(dp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_rdp_n_ibase
      real(dp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure
    module procedure logspace_1_cdp_n_rbase
      complex(dp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_cdp_n_cbase
      complex(dp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_cdp_n_ibase
      complex(dp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure
    ! Generate logarithmically spaced sequence from dp base to the powers
    ! of dp start and end. [base^start, ... , base^end]
    ! RName = logspace_1_cdp_n_ibase
    module procedure logspace_1_iint32_n_rdpbase
      integer :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_iint32_n_cdpbase
      integer :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_iint32_n_ibase
      integer :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure


end submodule
