!$Author$
!$Date$
!$Revision$
!$HeadURL$

! NOTE:  all global decomp pools have been modified to have an array size of
!        mnbpls (3) even though some pools are not currently used/defined
!        within the DECOMP model.  This change is being made to make coding
!        easier for all other submodels needing to manipulate the residue pools
!        (ie. will only need one index value for loops on all residue pool classes).
!        Eventually it is envisioned that the DECOMP model will be expanded to
!        truly contain the same number of age groups (3) for all residue pools
!        (standing, flat, buried, and roots). Currently only the flat residue pool
!        contains 3 age groups, the others contain only 2 age groups.
!
!        This include file contains only local variables for the DECOMP routines.
!        Thus, the local variables will not have the array sizes modified at
!        this time.  However, if and when the decomp routines are expanded to
!        work with 3 age groups for all pools, they will need to be modified then.
!
!        All array dimensions are updated by defining them via the parameter
!        mnbpls or a function of that parameter value.
!
!        Sat Jul 29 18:08:25 CDT 1995
!
! LEW
!
!     DECOMP.INC
!
!     These local variables are used by
!     the decomposition submodel DECOMP, DECINI, DECHRV, AND DECOUT
!     subroutines
!     Variables that need to be maintained by subregion are
!     arrayed be mnsub
!
!     requires p1werm.inc to be included prior to inclusion of this file
!
!     + + + VARIABLE DECLARATIONS + + +

      integer iage
      integer idtype
      integer iht
      integer isz

      integer iwets
      integer iwetf
      integer iwetg(mnsz)

      real aqua
      real diddf
      real diddg(mnsz)
      real didds
      real ditca
      real ditcg(mnsz)
      real diwcf
      real diwcg(mnsz)
      real diwcs
      real diwcsy(mnsub)

! + + + VARIABLE DEFINITIONS + + +

!     iage   - residue pool age index
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           1                    3
!         typical                         1                    3
!
!         - counter for the number of residue pools for standing, flat, buried  and
!           below ground residues

!     idtype  - index of residue type
!
!         1 = standing
!         2 = flat or surface
!         3 = buried (non root)
!         4 = root
!         5 = stem number
!
!          - used for assigning residue decomposition rates

!     iht  - index for standing residues vertical distribution
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           1                    5
!         typical                         1                    5

!     isz     - soil layer indexing variable
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           1                    max no. soil layers
!         typical                         1                    ?

!     aqua    - sum of precip, irrigation and snow melt (mm)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           fmin                 fmax
!         typical                         tmin                 tmax

!     diddf   - decomposition day for surface residue (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

!     diddg() - decomp. day for below ground residue by soil layer (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

!     didds   - decomposition day for standing residue (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

!     ditca   - temperature coef. for above ground res. (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

!     ditcg() - temperature coef. below ground res. by soil layer (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

!     diwcf   - daily water coefficient for surface residues (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

!     diwcg - water coef. for below ground res. by soil layer (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

!     diwcs - water coefficient for standing residues (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

!     diwcsy  - water coefficient from previous day standing res.  (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

      common / decom /                                                  &
     &    iwets,iwetf,iwetg,                                            &
     &    iage, idtype, iht,                                            &
     &    isz,                                                          &
     &    aqua,                                                         &
     &    diddf, diddg, didds,                                          &
     &    ditca, ditcg, diwcf, diwcg, diwcs, diwcsy

      save :: /decom/
