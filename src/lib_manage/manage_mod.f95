!$Author$
!$Date$
!$Revision$
!$HeadURL$

module manage_mod

  contains

    subroutine manage( sr, startyr, soil, crop, cropprev, residue, biotot, mandate, h1et, manFile)

!     + + + PURPOSE + + +
!     This is the main routine of the MANAGEMENT submodel. The date passed
!     to this routine is checked with the next operation date in the
!     management file. If the dates match, then an operation is to be
!     performed today on the given subregion.
!     The date of last operation (op*) is also passed for output purposes.jt

!     Edit History
!     19-Feb-99   wjr   rewrote
!     20-Feb-99   wjr   made date return

!     + + + KEYWORDS + + +
!     tillage, management

      use weps_interface_defs
      use datetime_mod, only: difdat, get_simdate
      use file_io_mod, only: luomanage
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter, biototal, bio_prevday
      use mandate_mod, only: opercrop_date
      use stir_report_mod, only: stir_report
      use hydro_data_struct_defs, only: hydro_derived_et
      use manage_data_struct_defs, only: man_file_struct, lastoper

!     + + + PARAMETERS AND COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'manage/asd.inc'
      include 'manage/mproc.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr, startyr
      type(soil_def), intent(inout) :: soil  ! soil for this subregion
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(bio_prevday), intent(inout) :: cropprev    ! structure containing crop previous day values
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(in) :: biotot
      type(opercrop_date), dimension(:), intent(inout) :: mandate
      type(hydro_derived_et), intent(inout) :: h1et
      type(man_file_struct), intent(inout) :: manFile

!     + + + ARGUMENT DEFINITIONS + + +
!        sr - the subregion number
!     startyr - starting year of the simulation run

!     + + + LOCAL VARIABLES + + +

      integer simdd, simmm, simyr, mansimyr
      character*256   line

!        simdd - current simulation day
!        simmm - current simulation month
!        simyr - current simulation year
!     mansimyr - the simulation year which corresponds to the year from the management file

!     + + + SUBROUTINES CALLED + + +
!     dooper - DO OPERation is called when dates match
!     dogroup - DO GROUP is called when G code encountered
!     doproc - DO PROCess is called when P code encountered

!     + + + OUTPUT FORMATS + + +
2015     format ('Op Date ', i2,1x,i2,1x,i4,' Rot yr ',i2,' sr #',i2)
!2015     format ('Operation Date ',i2,1x,i2,1x,i4,', subregion #',i2)

!     + + + END SPECIFICATIONS + + +

      ! get current simulation day, month, year
      call get_simdate( simdd, simmm, simyr )

      ! reset any global variables whose setting should only be valid
      ! for one day
      call mgdreset(h1et%zirr)

      ! find simulation year to which management year corresponds
      mansimyr = simyr - mod (simyr-startyr, manFile%mperod) + manFile%oper%operDate%year - 1
      if (difdat (simdd,simmm,simyr,manFile%oper%operDate%day,manFile%oper%operDate%month,mansimyr).ne.0) then
        ! simulation date precedes management date 
        return
      end if

      if (manFile%am0tfl .eq. 1) then
        write (luomanage(sr),*)
        write (luomanage(sr),2015) simdd,simmm,simyr,manFile%oper%operDate%year,sr
      endif

      ! pass date of operation to MAIN for output purposes, used by STIR also
      lastoper(0)%day = manFile%oper%operDate%day
      lastoper(0)%mon = manFile%oper%operDate%month
      lastoper(0)%yr = manFile%oper%operDate%year
      lastoper(sr)%day = manFile%oper%operDate%day
      lastoper(sr)%mon = manFile%oper%operDate%month
      lastoper(sr)%yr = manFile%oper%operDate%year

      ! perform all operations that occur on this date
      do while ( associated(manFile%oper) )
        lastoper(sr)%skip = 0
        call dooper(manFile)
        ! do groups
        manFile%grp => manFile%oper%grpFirst
        do while ( associated(manFile%grp) )
          if(lastoper(sr)%skip.eq.0) then
            call dogroup(soil, manFile)
            ! do processes
            manFile%proc => manFile%grp%procFirst
            do while ( associated(manFile%proc) )
              call doproc(soil, crop, cropprev, residue, biotot, mandate, h1et, manFile)
              ! next process
              manFile%proc => manFile%proc%procNext
            end do
            ! next group
            manFile%grp => manFile%grp%grpNext
          end if
        end do
        ! next operation
        manFile%oper => manFile%oper%operNext
        if( associated(manFile%oper) ) then
          ! find simulation year to which management year corresponds
          mansimyr = simyr - mod (simyr-startyr, manFile%mperod) + manFile%oper%operDate%year - 1
          if( difdat (simdd,simmm,simyr,manFile%oper%operDate%day,manFile%oper%operDate%month,mansimyr) .ne. 0) then
            ! this is a future operation
            ! initialize end of season / hydrobal reporting flag to true to generate a report
            rpt_season_flg(sr) = .true.
            exit
          end if
        else  ! not associated
          ! end of rotation
          manFile%mcount = manFile%mcount + 1
          manFile%oper => manFile%operFirst
          exit
        end if
      end do

      return

    end subroutine manage

    subroutine mfinit (sr, manFile)
!
!     + + + PURPOSE + + +
!     Mfinit should be called during the initialization stage of the the
!     main weps program. Mfinit searches the management data file; marking
!     the start sections of each subregion, while storing the number of
!     years in each subregion's management cycle.
!
!
!       Edit History
!       19-Feb-99       wjr     rewrote
!
!     + + + KEYWORDS + + +
!     tillage, management file, initialization
!
!     + + + PARAMETERS AND COMMON BLOCKS + + +

      use weps_interface_defs
      use file_io_mod, only: fopenk
      use manage_data_struct_defs, only: lastoper, man_file_struct, operation_date
      use flib_sax
      use manage_xml_mod, only: init_man_xml, read_old_manfile
      use manage_xml_mod, only: manfile_complete
      use manage_xml_mod, only: begin_man_element_handler, end_man_element_handler, pcdata_man_chunk_handler

      include 'p1werm.inc'
      include 'm1flag.inc'
      include 'manage/asd.inc'
      include 'manage/tcrop.inc'
      include 'manage/mproc.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr                        ! current subregion
      type(man_file_struct) :: manFile  ! management file data structure

!     + + + LOCAL VARIABLES + + +
      integer :: idx
      integer :: luimandate   ! unit number for reading in management file
      character*256 :: line

      type(xml_t) :: fxml   ! xml file handle structure
      integer :: read_stat  ! reading file status

!     + + + DATA INITIALIZATIONS + + +

      ! initialize values for crop effect flags
      am0kilfl = 0
      am0cropupfl = 0
      am0defoliatefl = 0

      ! initialize the manage/tcrop.inc variables

      atmstandstem(sr) = 0.0
      atmstandleaf(sr) = 0.0
      atmstandstore(sr) = 0.0

      atmflatstem(sr) = 0.0
      atmflatleaf(sr) = 0.0
      atmflatstore(sr) = 0.0

      atmflatrootstore(sr) = 0.0
      atmflatrootfiber(sr) = 0.0

      atzht(sr) = 0.0
      atdstm(sr) = 0.0
      atxstmrep(sr) = 0.0
      atzrtd(sr) = 0.0
      atgrainf(sr) = 0.0

      do idx = 1,mnsz
         atmbgstemz(idx,sr) = 0.0
         atmbgleafz(idx,sr) = 0.0
         atmbgstorez(idx,sr) = 0.0

         atmbgrootstorez(idx,sr) = 0.0
         atmbgrootfiberz(idx,sr) = 0.0
      end do
      rpt_season_flg(sr) = .true.

!     + + + END SPECIFICATIONS + + +

!     read in management file

      call fopenk(luimandate, trim(manFile%tinfil), 'old')
      read(luimandate, '(a)', iostat=read_stat) line
      if (read_stat /= 0) then
        stop "Cannot read input file"
      end if

      call init_man_xml( manFile%isub )
      if ( (line (1:8).ne.'Version: ') .and. (index(line, 'xml') .gt. 0) ) then
        close(luimandate)
        ! open input file
        call open_xmlfile(trim(manFile%tinfil),fxml,read_stat)
        if (read_stat /= 0) stop "Cannot open xml input file"
        ! read in xml based input file
        call xml_parse(fxml, &
           begin_element_handler = begin_man_element_handler, &
           end_element_handler = end_man_element_handler, &
           pcdata_chunk_handler = pcdata_man_chunk_handler, &
           verbose = .false.)
        if (.not. manfile_complete) then
          write(*,*) 'Management file incomplete: ', trim(manFile%tinfil)
          call exit(1)
        end if
      else
        call read_old_manfile ( manFile%isub, luimandate )
      end if

      ! init flag calibration of crops with multiple harvests.
      manFile%harv_calib_not_selected = .true.
      ! init rotation counter
      manFile%mcount = 0

      return

    end subroutine mfinit

end module manage_mod

