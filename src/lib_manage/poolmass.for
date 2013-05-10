!$Author$
!$Date$
!$Revision$
!$HeadURL$
      real function poolmass( nslay,                                    &
     &           mstandstem, mstandleaf, mstandstore,                   &
     &           mflatstem, mflatleaf, mflatstore,                      &
     &           mflatrootstore, mflatrootfiber,                        &
     &           mbgstemz, mbgleafz, mbgstorez,                         &
     &           mbgrootstorez, mbgrootfiberz )

!     + + + VARIABLE DECLARATIONS + + +

      integer nslay          ! number of soil layers
      ! state variables
      real mstandstem
      real mstandleaf
      real mstandstore

      real mflatstem
      real mflatleaf
      real mflatstore

      real mflatrootstore
      real mflatrootfiber

      real mbgstemz(nslay)
      real mbgleafz(nslay)
      real mbgstorez(nslay)

      real :: mbgrootstorez(nslay)
      real :: mbgrootfiberz(nslay)

!     + + + PURPOSE + + +
      ! sums the total biomass contained in a pool from some of the
      ! state variables and some of the derived variables

!     + + + VARIABLE DEFINITIONS + + +

!     mstandstem  - standing stem mass (kg/m^2)
!     mstandleaf  - standing leaf mass (kg/m^2)
!     mstandstore - standing storage mass (kg/m^2)b

!     mflatstem  - flat stem mass (kg/m^2)
!     mflatleaf  - flat leaf mass (kg/m^2)
!     mflatstore - flat storage mass (kg/m^2)

!     mflatstore - flat storage root mass (kg/m^2)
!     mflatfiber - flat fibrous root mass (kg/m^2)

!     mbgstemz  - buried stem mass by layer (kg/m^2)
!     mbgleafz  - buried leaf mass by layer (kg/m^2)
!     mbgstorez - buried (from above ground) storage mass by layer (kg/m^2)

!     mbgrootstorez - buried storage root mass by layer (kg/m^2)
!     mbgrootfiberz - buried fibrous root mass by layer (kg/m^2)

!     + + + LOCAL VARIABLES + + +
      integer idx
      real mass

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     idx     - layer counter
!     mass    - summation variable for poolmass

      ! sum all above ground biomass pools
      mass = mstandstem + mstandleaf + mstandstore                      &
     &     + mflatstem + mflatleaf + mflatstore                         &
     &     + mflatrootstore + mflatrootfiber

      ! add in below ground biomass pools
      do idx = 1, nslay
          mass = mass + mbgstemz(idx) + mbgleafz(idx) + mbgstorez(idx)  &
     &         + mbgrootstorez(idx) + mbgrootfiberz(idx)
      end do
      poolmass = mass

      return
      end