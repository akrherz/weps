!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!

!     decini.for

      subroutine decoinit(residue, decompfac)

      use biomaterial, only: biomatter, decomp_factors

!     + + +  PURPOSE + + +
!     This subroutine initalizes values needed in the decomposiiton
!     submodel. The values are read from a file that indicate the previous
!     crop harvested and quantities of biomass remaining in the field, for
!     standing, surface, buried, and root residues.

!     The subroutine also sets all other age pools and decompdays to 0.

!     + + +   ARGUMENT DECLARATIONS + + +     type(decomp_factors) :: decompfac

      type(biomatter), intent(inout) :: residue
      type(decomp_factors), intent(inout) :: decompfac

!      + + + LOCAL VARIABLE DECLARATION + + +
      integer idx     ! loop index

!     + + + END SPECIFICATION + + +

!     default initalization values for crop type, stem no. stem biomass
!     surface biomass, below ground biomass, and root biomass. ie. nothing

      residue%bname = "No Crop"
!     water coefficent parameters
      decompfac%iwcsy = 0.0
      decompfac%weti = 0

      ! calendar days from residue initiation
      residue%decomp%resday = 0
      ! index for each residue initiation
      residue%decomp%resyear = 0

!     cummulative ddays for standing residues
      residue%decomp%cumdds = 0.0
!     flat biomass, cummddays, and covfact for surface residues
      residue%decomp%cumddf = 0.0

!     cumulative ddays and biomass for all layers below ground
      do idx = 1, size(residue%decomp%bg)
         residue%decomp%bg(idx)%cumddg = 0.0
      end do

      residue%mass%standstem = 0.0
      residue%mass%standleaf = 0.0
      residue%mass%standstore = 0.0

      residue%mass%flatstem = 0.0
      residue%mass%flatleaf = 0.0
      residue%mass%flatstore = 0.0

      residue%mass%flatrootstore = 0.0
      residue%mass%flatrootfiber = 0.0

      do idx = 1, size(residue%mass%bg)
         residue%mass%bg(idx)%stemz = 0.0
         residue%mass%bg(idx)%leafz = 0.0
         residue%mass%bg(idx)%storez = 0.0

         residue%mass%bg(idx)%rootstorez = 0.0
         residue%mass%bg(idx)%rootfiberz = 0.0
      end do

      residue%geometry%dstm = 0.0
      residue%geometry%xstmrep = 0.0

      residue%geometry%grainf = 1.0
      residue%geometry%hyfg = 0

      residue%geometry%dstm = 0.0
      residue%geometry%zht = 0.0

      residue%deriv%m = 0.0
      residue%deriv%mst = 0.0
      residue%deriv%mf = 0.0
      residue%deriv%mbg = 0.0
      residue%deriv%mrt = 0.0

      residue%deriv%rsai = 0.0
      residue%deriv%rlai = 0.0
      residue%deriv%ffcv = 0.0
      residue%deriv%fscv = 0.0
      residue%deriv%ftcv = 0.0

      do idx = 1, size(residue%deriv%bg)
         residue%deriv%bg(idx)%mbgz = 0.0
         residue%deriv%bg(idx)%mrtz = 0.0
      end do

      !  canopy layer
      do idx = 1, size(residue%deriv%can)
         residue%deriv%can(idx)%rsaz = 0.0
         residue%deriv%can(idx)%rlaz = 0.0
      end do

!     set biomass decomposition rates to 0.0 for all pools
!     residue type counter
      do idx = 1, 5
         residue%database%dkrate(idx) = 0.0
      end do

      ! set biomass surface evaporation suppression coefficients
      residue%database%resevapa = 0.0
      residue%database%resevapb = 1.0

      residue%database%ddsthrsh = 0.0
      residue%database%xstm = 0.0
      residue%database%sla = 0.0
      residue%database%ck = 0.0
      residue%database%rbc = 1
      residue%database%covfact = 0.0

      return
      end
