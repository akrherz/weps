!$Author$
!$Date$
!$Revision$
!$HeadURL$

module biomaterial
  implicit none

  private

  public :: biomatter
  public :: create_biomatter
  public :: destroy_biomatter
 
  ! start c1glob.inc and d1glob.inc
  ! defines mass of plant parts that are below ground by soil layer
  type biostate_mass_below_ground_layers
     real :: stemz          ! buried stem mass by layer (kg/m^2)
     real :: leafz          ! buried leaf mass by layer (kg/m^2)
     real :: storez         ! buried (from above ground) storage mass by layer (kg/m^2)
     real :: rootstorez     ! buried storage root mass by layer (kg/m^2)
     real :: rootfiberz     ! buried fibrous root mass by layer (kg/m^2)
  end type biostate_mass_below_ground_layers

  ! defines mass of all plant parts
  type biostate_mass
     real :: standstem      ! standing stem mass (kg/m^2)
     real :: standleaf      ! standing leaf mass (kg/m^2)
     real :: standstore     ! standing storage mass (kg/m^2)
     real :: flatstem       ! flat stem mass (kg/m^2)
     real :: flatleaf       ! flat leaf mass (kg/m^2)
     real :: flatstore      ! flat storage mass (kg/m^2)
     real :: flatrootstore  ! flat storage root mass (kg/m^2)
     real :: flatrootfiber  ! flat fibrous root mass (kg/m^2)
     type(biostate_mass_below_ground_layers), dimension(:), allocatable :: bg
  end type biostate_mass

  type biostate_geometry
     real :: zht            ! "stem" height (m)
     real :: dstm           ! Number of stems per unit area (#/m^2)
     real :: xstmrep        ! a representative diameter so that dstm*xstmrep*zht=rsai
     real :: grainf         ! internally computed grain fraction of reproductive mass
     integer :: hyfg        ! flag indicating the part of plant to which the "grain fraction" GRF is applied
                            ! when removing that plant part for yield
                            ! 0     GRF applied to above ground storage (seeds, reproductive)
                            ! 1     GRF times growth stage factor (see growth.for) applied to above ground storage (seeds, reproductive)
                            ! 2     GRF applied to all aboveground biomass (forage)
                            ! 3     GRF applied to leaf mass (tobacco)
                            ! 4     GRF applied to stem mass (sugarcane)
                            ! 5     GRF applied to below ground storage mass (potatoes, peanuts)
     real :: zshoot         ! length of actively growing shoot from root biomass (m)
     real :: zrtd           ! root depth (m)
  end type biostate_geometry

  type biostate_growth
     real :: thucum         ! crop accumulated heat units
     real :: trthucum       ! accumulated root growth heat units (degree-days)

     real :: zgrowpt        ! depth in the soil of the growing point (m)
     real :: fliveleaf      ! fraction of standing plant leaf which is living (transpiring)
     real :: leafareatrend  ! direction in which leaf area is trending.
                            ! Saves trend even if leaf area is static for long periods.
     real :: stemmasstrend  ! direction in which stem mass is trending.
                            ! Saves trend even if stem mass is static for long periods.

     real :: twarmdays      ! number of consecutive days that the temperature has been above the minimum growth temperature
     real :: tchillucum     ! accumulated chilling units (days)
     real :: thardnx        ! hardening index for winter annuals (range from 0 t0 2)

     real :: thu_shoot_beg  ! heat unit total for beginning of shoot grow from root storage period
     real :: thu_shoot_end  ! heat unit total for end of shoot grow from root storage period

     integer :: dayap       ! number of days of growth completed since crop planted
     integer :: dayam       ! number of days since crop matured
     integer :: dayspring   ! day of year in which a winter annual released stored growth
  end type biostate_growth

  type biostate_decomp_below_ground_layers
     real :: cumddg       ! cumm. decomp days below ground res by pool and layer (days)
  end type biostate_decomp_below_ground_layers

  type biostate_decomp    ! from decomp/decomp.inc
     integer :: resday    ! calendar days after residue initiation
     integer :: resyear   ! index counting each new residue initiation
     integer :: weti     ! days since anticedent moisture (4 to 0) index
     real :: iwcsy       ! water coefficient from previous day standing res.  (0 to 1)
     real :: cumdds       ! cumulative decomp days for standing res. by pool (days)
     real :: cumddf       ! cummlative decomp days for surface res. by pool (days)
     type(biostate_decomp_below_ground_layers), dimension(:), allocatable :: bg
  end type biostate_decomp

  type bioderived_below_ground_layers
     real :: mrtz           ! Buried root mass by soil layer (kg/m^2)
     real :: mbgz           ! Buried mass by soil layer (kg/m^2)
  end type bioderived_below_ground_layers

  type bioderived_canopy_layers
     real :: rsaz           ! stem area index by height (1/m)
     real :: rlaz           ! leaf area index by height (1/m)
  end type bioderived_canopy_layers

  type bioderived
     real :: m            ! Total mass (standing + flat + roots + buried) (kg/m^2)
     real :: mst          ! Standing mass (standstem + standleaf + standstore) (kg/m^2)
     real :: mf           ! Flat mass (flatstem + flatleaf + flatstore) (kg/m^2)
     real :: mrt          ! Buried root mass (rootfiber + rootstore)(kg/m^2)
     real :: mbg          ! Buried mass (kg/m^2) Excludes root mass below the surface.
     type(bioderived_below_ground_layers), dimension(:), allocatable :: bg

     real :: rsai         ! Residue stem area index (m^2/m^2)
     real :: rlai         ! Residue leaf area index (m^2/m^2)
     type(bioderived_canopy_layers), dimension(:), allocatable :: can

     real :: rcd          ! effective Biomass silhouette area (SAI+LAI) (m^2/m^2)
                          ! (combination of leaf area and stem area indices)
     real :: ffcv         ! biomass cover - flat (m^2/m^2)
     real :: fscv         ! biomass cover - standing (m^2/m^2)
     real :: ftcv         ! biomass cover - total (m^2/m^2)
                          ! (ffcv + fscv)
     real :: fcancov      ! fraction of soil surface covered by canopy (m^2/m^2)
  end type bioderived

  type biodatabase_decomp ! from c1db1.inc
     real, dimension(1:5) :: dkrate ! array of decomposition rate parameters
                                    ! acdkrate(1) - standing residue mass decomposition rate (d<1) (g/g/day)
                                    ! acdkrate(2) - flat residue mass decomposition rate (d<1) (g/g/day)
                                    ! acdkrate(3) - buried residue mass decomposition rate (d<1) (g/g/day)
                                    ! acdkrate(4) - root residue mass decomposition rate (d<1) (g/g/day)
                                    ! acdkrate(5) - stem residue number decline rate (d<1) (#/m^2/day)? (fall rate)
     real :: ddsthrsh     ! threshhold number of decomp. days before stems begin to fall
     real :: xstm         ! mature crop stem diameter (m)
     real :: covfact      ! flat residue cover factor (m^2/kg)
     real :: resevapa     ! coefficient a in relation ea/ep = exp(resevapa * (flat mass kg/m^2)**resevapb)
     real :: resevapb     ! coefficient b in relation ea/ep = exp(resevapa * (flat mass kg/m^2)**resevapb)
     real :: sla          ! residue specific leaf area
     real :: ck           ! residue light extinction coeffficient (fraction)
     integer :: rbc       ! residue burial class
                          ! 1   o Fragile-very small (soybeans) residue
                          ! 2   o Moderately tough-short (wheat) residue
                          ! 3   o Non fragile-med (corn) residue
                          ! 4   o Woody-large residue
                          ! 5   o Gravel-rock
  end type biodatabase_decomp

  type bio_output_units
     integer :: dec
  end type bio_output_units

  type biomatter
     character*(80) :: bname       ! the name of the biomaterial
     type(bio_output_units) :: luo
     type(biostate_mass) :: mass
     type(biostate_geometry) :: geometry
     type(biostate_growth) :: growth
     type(biostate_decomp) :: decomp
     type(bioderived) :: deriv
     type(biodatabase_decomp) :: database
     type(bio_output_units) :: luo
  end type biomatter

contains

  function create_biomatter(nsoillay, ncanlay) result(biomat)
     integer, intent(in) :: nsoillay
     integer, intent(in) :: ncanlay
     type(biomatter) :: biomat

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     ! allocate below and above ground arrays
     allocate(biomat%mass%bg(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(biomat%decomp%bg(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(biomat%deriv%bg(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(biomat%deriv%can(ncanlay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for biomatter'
        stop(1)
     end if
  end function create_biomatter

  subroutine destroy_biomatter(biomat)
     type(biomatter), intent(inout) :: biomat

     ! local variable
     integer :: dealloc_stat
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     ! allocate below and above ground arrays
     deallocate(biomat%mass%bg, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(biomat%decomp%bg, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(biomat%deriv%bg, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(biomat%deriv%can, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to deallocate memory for biomatter'
     end if
  end subroutine destroy_biomatter

end module biomaterial



