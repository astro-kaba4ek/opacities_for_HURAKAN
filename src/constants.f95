module contains

use regime
implicit none

real(knd), parameter :: PI = 2 * asin(1q0)

! https://physics.nist.gov/cuu/Constants/index.html
real(knd), parameter :: c_light	= 2.99792458q10		!Speed of light            [cm s-1]
real(knd), parameter :: k_B		= 1.380649q-16		!Boltzmann constant        [erg K-1]
real(knd), parameter :: h_Pl	= 6.62607015q-27	!Planck constant           [erg s]
real(knd), parameter :: SBc		= 2 * PI**5 * k_B**4 / 15 / h_Pl**3 / c_light**2 !Stefan-Boltzmann constant [erg cm-2 s-1 K-4]
! = 5.670374419...q-5

integer, parameter	:: crutch_max_lambda = 216

! real(WP), parameter :: c_light = 2.99792458e10_WP            !Speed of light            [cm s-1]
! real(WP), parameter :: hP     = 6.626070040e-27_WP          !Planck constant           [erg s]
! real(WP), parameter :: kB     = 1.38064852e-16_WP           !Boltzmann constant        [erg K-1]
! real(WP), parameter :: Gg     = 6.67408e-8_WP               !Graviatational constant   [cm3 g-1 s-2]
! real(WP), parameter :: ep     = 4.80320425e-10_WP           !Elementary charge         [statC]
! real(WP), parameter :: amu    = 1.660539040e-24_WP          !Atomic mass unit          [g]
! real(WP), parameter :: mp     = 1.672621898e-24_WP          !Proton mass               [g]
! real(WP), parameter :: me     = 9.10938356e-28_WP           !Electron mass             [g]
! real(WP), parameter :: eV     = 1.6021766208e-12_WP         !Electron volt             [erg]
! real(WP), parameter :: SBc    = 5.670367e-5_WP              !Stefan-Boltzmann constant [erg cm-2 s-1 K-4]

contains
	
end module contains