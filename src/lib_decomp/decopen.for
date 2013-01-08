! $Author$
! $Date$
! $Revision$
! $HeadURL$
!     file: decopen.for

      subroutine decopen

! + + + Purpose  + + +
!     Write the headers into some files for decomp output

      use file_io_mod, only: luod_above, luod_below
      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1flag.inc'
      include 'd1gen.inc'
      include 'h1temp.inc'

! + + + LOCAL VARIABLES + + +

! + + + FORMATS + + +
 2030 format( 29x,'Standing',9x,'Flat',9x,'Surface Cover    Silhouett ' &
     &,'Area     Total Residue Amounts')
 2035 format (14x,'Pool',1x,'Stem',1x,2(2x,'decomp',3x,'bio-',1x),2x,   &
     &       15('-'),3x,14('-'),4x,24('-'))
 2040 format ('sr day/mo/yr   no.  no.    days    mass    days    mass',&
     &        '   Stems      Flat    Total      /5     Stand    Flat  ',&
     &'  Buried    ')

!    + + + DATA + + +

!    + + + END SPECIFICATIONS + + +

!   Write headers for output files
!       dabove.out
!       dbelow.out

!     write headers for above ground residues file if requested
      if ((am0dfl .eq. 1) .or. (am0dfl .eq. 3)) then
         write (luod_above,*)                                           &
     &         'Above Ground Residue Decomposition Output File'
         write (luod_above,*) 'Standing and Surface Residues'
         write (luod_above,*) '  '
         write (luod_above,2030)
         write (luod_above,2035)
         write (luod_above,2040)
         write (luod_above,*) '  '
      end if

!     write headers for below ground residues file if requested
      if ((am0dfl .eq. 2) .or. (am0dfl .eq. 3)) then
         write (luod_below,*)                                           &
     &         'Below Ground Residue Decomposition Output File'
         write (luod_below,*) 'Data by soil layer for age pools 1 and 2'
         write (luod_below,*) '  '
         write (luod_below,*) '     day/mo/year '
      end if

      return
      end
