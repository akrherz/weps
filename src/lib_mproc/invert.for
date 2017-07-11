!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine invert                                                 &
     &              (nlay,density,laythk,                               &
     &               sand,silt,clay, rock_vol,                          &
     &               c_sand, m_sand, f_sand, vf_sand,                   &
     &               w_bd,                                              &
     &               organic, ph, calcarb, cation,                      &
     &               lin_ext,                                           &
     &               aggden, drystab,                                   &
     &               soilwatr,                                          &
     &               satwatr, thrdbar, ftnbar,                          &
     &               avawatr,                                           &
     &               soilcb,soilair,satcond,                            &
     &               residue, massf)


!     + + + PURPOSE + + +
!     
!     This subroutine reads in the array(s) containing the components 
!     that need to be inverted.  It then calls the subroutine invproc 
!     and the actual inversion process is performed.

!     + + + KEYWORDS + + +
!     inversion, tillage 

      use asd_mod, only: msieve
      use weps_interface_defs, ignore_me=>invert
      use biomaterial, only: biomatter

!     + + + ARGUMENT DECLARATIONS + + +
      integer nlay
      real density(*),laythk(*)
      real sand(*),silt(*),clay(*), rock_vol(*)
      real c_sand(*), m_sand(*), f_sand(*), vf_sand(*)
      real w_bd(*)
      real organic(*), ph(*), calcarb(*), cation(*)
      real lin_ext(*)
      real aggden(*), drystab(*)
      real soilwatr(*)
      real satwatr(*), thrdbar(*), ftnbar(*)
      real avawatr(*)
      real soilcb(*), soilair(*), satcond(*)
      type(biomatter), dimension(:), intent(inout) :: residue
      real, dimension(msieve+1,*) :: massf

!     + + + ARGUMENT DEFINITIONS + + +

!     density     - soil density 
!     laythk      - layer thickness

!     sand        - fraction of sand
!     silt        - fraction of silt
!     clay        - fraction of clay
!     rock_vol    - volume fraction of rock
!     c_sand      - fraction of course sand
!     m_sand      - fraction of medium sand
!     f_sand      - fraction of fine sand
!     vf_sand     - fraction of very fine sand

!     w_bd        - wet (1/3 bar) soil density 

!     organic     - fraction of organic matter
!     ph          - soil Ph
!     calcarb     - fraction of calcium carbonate
!     cation      - cation exchange capcity

!     lin_ext     - linear extensibility

!     aggden      - aggregrate density
!     drystab     - dry aggregrate stability

!     soilwatr    - soil water content (mass bases)
!     satwatr     - saturation soil water content
!     thrdbar     - 1/3 bar soil water content
!     ftnbar      - 15 bar soil water content
!     avawatr     - available soil water content

!     soilcbr     - soil CB value
!     soilair     - soil air entery potential
!     satcond     - saturated hydraulic conductivity

!     residue     - structure containing residue by soil layer
!     massf       - mass fractions for sieve cuts

!     nlay        - number of soil layers used

!     + + + LOCAL VARIABLES + + +
      integer i,j,k
      real dum2(nlay)

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     dum2    - dummy variable containing a variable array to
!               be passed to the inversion process routine
!     i       - loop variable on decomposition pools
!     j       - loop variable on asd sieves 
!     k       - loop variable on the number of layers 

!     + + + END SPECIFICATIONS + + + 

!  Make calls to the inversion process for all variables that need 
!  to be inverted. 

!************************SOIL VARIABLES********************	
      call invproc(nlay,laythk,sand)
      call invproc(nlay,laythk,silt)
      call invproc(nlay,laythk,clay)
      call invproc(nlay,laythk,rock_vol)

      call invproc(nlay,laythk,c_sand)
      call invproc(nlay,laythk,m_sand)
      call invproc(nlay,laythk,f_sand)
      call invproc(nlay,laythk,vf_sand)

      call invproc(nlay,laythk,w_bd)

      call invproc(nlay,laythk,organic)
      call invproc(nlay,laythk,ph)
      call invproc(nlay,laythk,calcarb)
      call invproc(nlay,laythk,cation)

      call invproc(nlay,laythk,lin_ext)

      call invproc(nlay,laythk,aggden)
      call invproc(nlay,laythk,drystab)
!************************SOIL VARIABLES********************	
!
!************************HYDROLOGY VARIABLES********************	
      call invproc(nlay,laythk,soilwatr)
      call invproc(nlay,laythk,satwatr)
      call invproc(nlay,laythk,thrdbar)
      call invproc(nlay,laythk,ftnbar)
      call invproc(nlay,laythk,avawatr)

      call invproc(nlay,laythk,soilcb)
      call invproc(nlay,laythk,soilair)
      call invproc(nlay,laythk,satcond)
!************************HYDROLOGY VARIABLES********************	
! 
!************************ASD MASS FRACTIONS********************	
!   need to invert mass fractions for all sieve cuts and layers 
!  
      do 170 j=1,msieve
         do 200 k=1,nlay
            dum2(k)=massf(j,k)
200      continue
         call invproc(nlay,laythk,dum2(1))
         do 201 k=1,nlay
            massf(j,k)=dum2(k)
201      continue
170   continue
!************************ASD MASS FRACTIONS********************	
! 
!************************DECOMPOSITION VARIABLES********************	
!   need to invert each pool for these

      do i=1,size(residue)

         do k=1,nlay
            dum2(k) = residue(i)%mass%stemz(k)
         end do
         call invproc(nlay,laythk,dum2(1))
         do k=1,nlay
           residue(i)%mass%stemz(k) = dum2(k)
         end do

         do k=1,nlay
            dum2(k) = residue(i)%mass%leafz(k)
         end do
         call invproc(nlay,laythk,dum2(1))
         do k=1,nlay
           residue(i)%mass%leafz(k) = dum2(k)
         end do

         do k=1,nlay
            dum2(k) = residue(i)%mass%storez(k)
         end do
         call invproc(nlay,laythk,dum2(1))
         do k=1,nlay
           residue(i)%mass%storez(k) = dum2(k)
         end do

         do k=1,nlay
            dum2(k) = residue(i)%mass%rootstorez(k)
         end do
         call invproc(nlay,laythk,dum2(1))
         do k=1,nlay
           residue(i)%mass%rootstorez(k) = dum2(k)
         end do

         do k=1,nlay
            dum2(k) = residue(i)%mass%rootfiberz(k)
         end do
         call invproc(nlay,laythk,dum2(1))
         do k=1,nlay
           residue(i)%mass%rootfiberz(k) = dum2(k)
         end do

      end do
!************************DECOMPOSITION VARIABLES********************
!
		  
      end
