! $Author$
! $Date$
! $Revision$
! $HeadURL$
!     file: decopen.for

      subroutine decopen(isr)

! + + + Purpose  + + +
!   Write headers for output files
!       dabove.out
!       dbelow.out

      use file_io_mod, only: luod_above, luod_below
      use decomp_data_struct_defs, only: am0dfl

      integer :: isr

! + + + FORMATS + + +
 2030 format( 29x,'Standing',9x,'Flat',9x,'Surface Cover    Silhouett ' &
     &,'Area     Total Residue Amounts')
 2035 format (14x,'Pool',1x,'Stem',1x,2(2x,'decomp',3x,'bio-',1x),2x,   &
     &       15('-'),3x,14('-'),4x,24('-'))
 2040 format ('sr day/mo/yr   no.  no.    days    mass    days    mass',&
     &        '   Stems      Flat    Total      /5     Stand    Flat  ',&
     &'  Buried    ')

!    + + + END SPECIFICATIONS + + +

!     write headers for above ground residues file if requested
      if ((am0dfl(isr) .eq. 1) .or. (am0dfl(isr) .eq. 3)) then
         write (luod_above(isr),*)                                      &
     &         'Above Ground Residue Decomposition Output File'
         write (luod_above(isr),*) 'Standing and Surface Residues'
         write (luod_above(isr),*) '  '
         write (luod_above(isr),2030)
         write (luod_above(isr),2035)
         write (luod_above(isr),2040)
         write (luod_above(isr),*) '  '
      end if

!     write headers for below ground residues file if requested
      if ((am0dfl(isr) .eq. 2) .or. (am0dfl(isr) .eq. 3)) then
         write (luod_below(isr),*)                                      &
     &         'Below Ground Residue Decomposition Output File'
         write (luod_below(isr),*)                                      &
     &         'Data by soil layer for age pools 1 and 2'
         write (luod_below(isr),*) '  '
         write (luod_below(isr),*) '     day/mo/year '
      end if

      return
      end
