!$Author$
!$Date$
!$Revision$
!$HeadURL$

subroutine erodsubr_update( sr, soil, crop, restot, croptot, biotot, h1et, subrsurf )

!     +++ PURPOSE +++
!     print out input file for stand alone erosion

!     + + + Modules Used + + +
    use subregions_mod
    use soil_data_struct_defs, only: soil_def
    use biomaterial, only: biototal, biomatter
    use hydro_data_struct_defs, only: hydro_derived_et
    use erosion_data_struct_defs, only: subregionsurfacestate
    use sberod_mod, only: sbsfdall

!     +++ ARGUMENT DECLARATIONS +++
    integer sr                               ! subregion index (eventually obsolete)
    type(soil_def), intent(in) :: soil  ! soil for this subregion
    type(biomatter), intent(in) :: crop
    type(biototal), intent(in) :: restot
    type(biototal), intent(in) :: croptot
    type(biototal), intent(in) :: biotot
    type(hydro_derived_et), intent(in) :: h1et
    type(subregionsurfacestate), intent(inout) :: subrsurf  ! subregion surface conditions (erosion specific set)

!     +++ ARGUMENT DEFINITIONS +++

!     + + + GLOBAL COMMON BLOCKS + + +
      include  'p1werm.inc'
      include  'h1db1.inc'

!     +++ LOCAL VARIABLES +++
      integer :: idx

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     idx - loop index

!     +++ END SPECIFICATIONS +++

    subrsurf%adzht_ave = restot%zht_ave
    subrsurf%aczht = croptot%zht_ave

    subrsurf%acrsai = croptot%rsaitot
    subrsurf%acrlai = croptot%rlaitot

    subrsurf%adrsaitot = restot%rsaitot
    subrsurf%adrlaitot = restot%rlaitot

    subrsurf%acxrow = crop%geometry%xrow
    subrsurf%ac0rg = crop%geometry%rg

    subrsurf%abffcv = biotot%ffcvtot

    subrsurf%asfcr = soil%asfcr
    subrsurf%aszcr = soil%aszcr
    subrsurf%asflos = soil%asflos
    subrsurf%asmlos = soil%asmlos
    subrsurf%asdcr = soil%asdcr
    subrsurf%asecr = soil%asecr
    subrsurf%aslrr = soil%aslrr
    subrsurf%aszrgh = soil%aszrgh
    subrsurf%asxrgs = soil%asxrgs
    subrsurf%asxrgw = soil%asxrgw
    subrsurf%asargo = soil%asargo

    do idx = 1, soil%nslay

        subrsurf%bsl(idx)%aszlyt = soil%aszlyt(idx)
        subrsurf%bsl(idx)%asdblk = soil%asdblk(idx)
        subrsurf%bsl(idx)%asfsan = soil%asfsan(idx)
        subrsurf%bsl(idx)%asfvfs = soil%asfvfs(idx)
        subrsurf%bsl(idx)%asfsil = soil%asfsil(idx)
        subrsurf%bsl(idx)%asfcla = soil%asfcla(idx)
        subrsurf%bsl(idx)%asvroc = soil%asvroc(idx)
        subrsurf%bsl(idx)%asdagd = soil%asdagd(idx)
        subrsurf%bsl(idx)%aseags = soil%aseags(idx)
        subrsurf%bsl(idx)%aslagm = soil%aslagm(idx)
        subrsurf%bsl(idx)%aslagn = soil%aslagn(idx)
        subrsurf%bsl(idx)%aslagx = soil%aslagx(idx)
        subrsurf%bsl(idx)%as0ags = soil%as0ags(idx)

        subrsurf%bsl(idx)%ahrwcw = soil%ahrwcw(idx)
        subrsurf%bsl(idx)%ahrwca = soil%ahrwca(idx)

    end do

    subrsurf%ahzsnd = h1et%zsnd

    do idx = 1, 24
        subrsurf%ahrwc0(idx) = ahrwc0(idx,sr)
    end do

    ! derived
    subrsurf%abrsai = biotot%rsaitot
    subrsurf%abrlai = biotot%rlaitot
    subrsurf%abzht = biotot%zht_ave
!     real :: sxprg      ! sxprg  - ridge spacing parallel the wind direction(mm)

    ! updates acanag, acancr, asf10an, asf10en, asf10bk, 
    ! sfd1, sfd10, sfd84, sfd200, sf1ic, sf10ic, sf84ic, sf200ic
    ! for reporting in plot.out
    call sbsfdall( subrsurf )

    return
    end

