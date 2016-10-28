!$Author$
!$Date$
!$Revision$
!$HeadURL$

program test_soil_density

  ! test of bulk density routines

  use file_io_mod, only: fopenk
  use f2kcli, only: COMMAND_ARGUMENT_COUNT

  real testbdref
  real testsetbdref
  real testoptimalwat

  include 'p1unconv.inc'
  include 'precision.inc'
  include 'hydro/vapprop.inc'

  ! variable definitions
  character     line*256                      ! character buffer for input line
  integer       lui1                          ! input file logical unit number
  integer       luo1                          ! output file logical unit number
  integer       ios                           ! input output status
  integer       rds                           ! line read status
  character*20  tmpchar1(1)                   ! temporary string array for test reading 1 string values
  character*20  tmpchar2(2)                   ! temporary string array for test reading 2 string values
  real          tmpreal7(7)                   ! temporary real array for test reading 7 values
  real          tmpreal6(6)                   ! temporary real array for test reading 6 values
  real          tmpreal5(5)                   ! temporary real array for test reading 5 values
  real          tmpreal2(2)                   ! temporary real array for test reading 2 values
  integer       class                         ! soil class index

  integer :: idx                              ! generic index
  integer :: jdx                              ! generic index
  integer :: nsoil                            ! number of soils
  character(LEN=20), dimension(:), allocatable :: dataname  ! name from data file
  character(LEN=7), dimension(:), allocatable :: soilname  ! soil texture class names
  real, dimension(:), allocatable :: bszlyd   ! Depth to bottom of each soil layer for each subregion (mm)
  real, dimension(:), allocatable :: bsdblk   ! bulk density (Mg/m^3) = (g/cm^3)
  real, dimension(:), allocatable :: bsdsblk  ! settled bulk density (Mg/m^3) = (g/cm^3)
  real, dimension(:), allocatable :: bsdprocblk  ! proctor bulk density (Mg/m^3) = (g/cm^3)
  real, dimension(:), allocatable :: bsdpart  ! particle density (Mg/m^3)
  real, dimension(:), allocatable :: bsfcla   ! fraction of soil mineral portion which is clay
  real, dimension(:), allocatable :: bsfsil   ! fraction of soil mineral portion which is silt
  real, dimension(:), allocatable :: bsfsan   ! fraction of soil mineral portion which is sand
  real, dimension(:), allocatable :: bsfom    ! fraction of total soil mass which is organic matter
  real, dimension(:), allocatable :: bsfcec   ! Soil layer cation exchange capacity (cmol/kg) (meq/100g)
  real, dimension(:), allocatable :: bhrwcs   ! gravimetric saturated water
  real, dimension(:), allocatable :: bhrwcf   ! gravimetric 1/3 bar water
  real, dimension(:), allocatable :: bhrwcw   ! gravimetric 15 bar water
  real, dimension(:), allocatable :: bhrwcr   ! gravimetric residual water
  real, dimension(:), allocatable :: bhrwca   ! gravimetric plant available water
  real, dimension(:), allocatable :: bh0cb    ! Brooks and Corey pore size interation exponent b
  real, dimension(:), allocatable :: bheaep   ! Brooks and Corey air entry potential (J/kg)
  real, dimension(:), allocatable :: bhrsk    ! saturated hydraulic conductivity (m/s)
  real, dimension(:), allocatable :: bhfredsat! fraction of soil porosity that will be filled with water
                                              ! while wetting under normal field conditions due to entrapped air

  ! Declarations for command line arguments
  character*512 argv    ! For Fortran 2k commandline parsing
  integer       icmd    ! index through numarg commands
  integer       numarg  ! number of command line arguments
  integer       ll,ss   ! length and status return
  logical       diff    ! if TRUE calculate difference between reference bulk density and settled bulk density
  logical       ratio   ! if TRUE calculate ratio (reference bulk density / settled bulk density)
  logical       drat    ! if TRUE calculate ratio ((reference-settled) / settled bulk density)
  logical       odat    ! if TRUE output original data
  logical       tfit    ! if TRUE output test of fitted equation
  logical       value   ! if TRUE output test of calculated value
  logical       proc_wepp   ! if TRUE output settled, reference, proctor calculations BD for WEPP proctor density data set
  logical       proc_serdp  ! if TRUE output settled, reference, proctor calculations BD for SERDP proctor density data set
  logical       proc_pref   ! if TRUE output settled, reference, proctor calculations BD for REFerence density data set

  ! default value for command line arguments
  diff = .FALSE.
  ratio = .FALSE.
  drat = .FALSE.
  odat = .FALSE.
  tfit = .FALSE.
  value = .FALSE.
  proc_wepp = .FALSE.
  proc_serdp = .FALSE.
  proc_pref = .FALSE.

  ! check command line arguments
  numarg = COMMAND_ARGUMENT_COUNT()  !Fortran 2k compatible call

  if (numarg .gt. 0) then
    do icmd = 1, numarg
      call GET_COMMAND_ARGUMENT(icmd,argv,ll,ss)  !Fortran 2k compatible call
      select case (argv(1:4))
        case ('diff')
          diff = .TRUE.
        case ('rati')
          ratio = .TRUE.
        case ('drat')
          drat = .TRUE.
        case ('odat')
          odat = .TRUE.
        case ('tfit')
          tfit = .TRUE.
        case ('valu')
          value = .TRUE.
        case ('wepp')
          proc_wepp = .TRUE.
        case ('serd')
          proc_serdp = .TRUE.
        case ('pref')
          proc_pref = .TRUE.
      end select
    end do
  else
    write(*,*) 'No command line arguments, No operation performed.'
    write(*,*) 'Options are:'
    write(*,*) '  diff (calculate difference between reference bulk density and settled bulk density)'
    write(*,*) '  ratio (calculate ratio: reference bulk density / settled bulk density)'
    write(*,*) '  odat (output original data)'
    write(*,*) '  tfit (output test of fitted equation)'
    write(*,*) '  value (output the calculated value)'
    write(*,*) 'test_soil_density (diff | ratio | drat) (odat | tfit | value)'
    write(*,*) 'test_soil_density (wepp | serdp | pref)'
  end if

  if( proc_wepp ) then
    ! open test data set file
    call fopenk (lui1, 'proctor-wepp.txt', 'old')

    ! count valid data points
    ! initial count
    nsoil = 0
    ! read first line
    read (lui1,'(a)',iostat=ios) line
    do while( ios .eq. 0 )
      ! parse line for data points
      read (line,*, iostat=rds) tmpchar2, tmpreal7
      if( rds .eq. 0 ) then
        ! seven real values on line
        nsoil = nsoil + 1
      end if
      ! read next line
      read (lui1,'(a)',iostat=ios) line
    end do

    write(*,*) '# Number of Values to be output: ', nsoil

  else if( proc_serdp ) then
    ! open test data set file
    call fopenk (lui1, 'proctor-serdp.txt', 'old')
    ! count valid data points
    ! initial count
    nsoil = 0
    ! read first line
    read (lui1,'(a)',iostat=ios) line
    do while( ios .eq. 0 )
      ! parse line for data points
      read (line,*, iostat=rds) tmpchar1, tmpreal6
      if( rds .eq. 0 ) then
        ! six real values on line
        nsoil = nsoil + 1
      end if
      ! read next line
      read (lui1,'(a)',iostat=ios) line
    end do

    write(*,*) '# Number of Values to be output: ', nsoil

  else
    ! open test data set file
    call fopenk (lui1, 'refdata.txt', 'old')

    ! count valid data points
    ! initial count
    nsoil = 0
    ! read first line
    read (lui1,'(a)',iostat=ios) line
    do while( ios .eq. 0 )
      ! parse line for data points
      read (line,*, iostat=rds) tmpchar1, tmpreal5
      if( rds .eq. 0 ) then
        ! five real values on line
        nsoil = nsoil + 1
      else
        read (line,*, iostat=rds) tmpchar1, tmpreal2
        if( rds .eq. 0 ) then
          nsoil = nsoil + 12
        end if
      end if
      ! read next line
      read (lui1,'(a)',iostat=ios) line
    end do

    write(*,*) '# Number of Values to be output: ', nsoil

  end if

  ! index file back to beginning
  rewind( lui1 )

  ! allocate arrays
  allocate( dataname(nsoil) )
  allocate( soilname(nsoil) )
  allocate( bszlyd(nsoil) )
  allocate( bsdblk(nsoil) )
  allocate( bsdsblk(nsoil) )
  allocate( bsdprocblk(nsoil) )
  allocate( bsdpart(nsoil) )
  allocate( bsfcla(nsoil) )
  allocate( bsfsil(nsoil) )
  allocate( bsfsan(nsoil) )
  allocate( bsfom(nsoil) )
  allocate( bsfcec(nsoil) )
  allocate( bhrwcs(nsoil) )
  allocate( bhrwcf(nsoil) )
  allocate( bhrwcw(nsoil) )
  allocate( bhrwcr(nsoil) )
  allocate( bhrwca(nsoil) )
  allocate( bh0cb(nsoil) )
  allocate( bheaep(nsoil) )
  allocate( bhrsk(nsoil) )
  allocate( bhfredsat(nsoil) )

  if( proc_wepp ) then
    ! set initial index
    idx = 0
    ! read first line
    read (lui1,'(a)',iostat=ios) line
    do while( ios .eq. 0 )
      ! parse line for data points
      read (line,*, iostat=rds) tmpchar2, tmpreal7
      if( rds .eq. 0 ) then
        ! set index for this input value
        idx = idx + 1
        ! assign data name
        dataname(idx) = tmpchar2(1)
        ! assign data value
        bsdblk(idx) = tmpreal7(5)
        ! convert from % to fraction
        bsfcla(idx) = tmpreal7(1) / 100.0
        bsfsil(idx) = 1.0 - (tmpreal7(1)+tmpreal7(2)) / 100.0
        bsfsan(idx) = tmpreal7(2) / 100.0
        bsfom(idx) = tmpreal7(3) / 100.0
        ! estimate cec from input data
        bszlyd(idx) = 100.0
        bsfcec(idx) = bsfcla(idx)*100*0.5 + bsfom(idx)*100.*2.0
        call usdatx( bsfsan(idx), bsfcla(idx), class)
        call usda_tx_name_frm_class( class, soilname(idx) )
      end if
      ! read next line
      read (lui1,'(a)',iostat=ios) line
    end do
  else if( proc_serdp ) then
    ! set initial index
    idx = 0
    ! read first line
    read (lui1,'(a)',iostat=ios) line
    do while( ios .eq. 0 )
      ! parse line for data points
      read (line,*, iostat=rds) tmpchar1, tmpreal6
      if( rds .eq. 0 ) then
        ! set index for this input value
        idx = idx + 1
        ! assign data name
        dataname(idx) = tmpchar1(1)
        ! assign data value
        bsdblk(idx) = tmpreal6(6)
        ! convert from % to fraction
        bsfcla(idx) = tmpreal6(1) / 100.0
        bsfsil(idx) = tmpreal6(2) / 100.0
        bsfsan(idx) = tmpreal6(3) / 100.0
        bsfom(idx) = tmpreal6(4) / 100.0
        ! estimate cec from input data
        bszlyd(idx) = 100.0
        bsfcec(idx) = bsfcla(idx)*100*0.5 + bsfom(idx)*100.*2.0
        call usdatx( bsfsan(idx), bsfcla(idx), class)
        call usda_tx_name_frm_class( class, soilname(idx) )
      end if
      ! read next line
      read (lui1,'(a)',iostat=ios) line
    end do
  else
    ! set initial index
    idx = 0
    ! read first line
    read (lui1,'(a)',iostat=ios) line
    do while( ios .eq. 0 )
      ! check for valid input line
      read (line,*, iostat=rds) tmpchar1, tmpreal5
      if( rds .eq. 0 ) then
        ! set index for this input value
        idx = idx + 1
        ! assign data name
        dataname(idx) = tmpchar1(1)
        ! assign data value
        bsdblk(idx) = tmpreal5(1)
        ! convert from % to fraction
        bsfcla(idx) = tmpreal5(2) / 100.0
        bsfsil(idx) = tmpreal5(3) / 100.0
        bsfsan(idx) = tmpreal5(4) / 100.0
        bsfom(idx) = tmpreal5(5) / 100.0
        ! estimate cec from input data
        bszlyd(idx) = 100.0
        bsfcec(idx) = bsfcla(idx)*100*0.5 + bsfom(idx)*100.*2.0
        call usdatx( bsfsan(idx), bsfcla(idx), class)
        call usda_tx_name_frm_class( class, soilname(idx) )
      else
        ! check for valid input line
        read (line,*, iostat=rds) tmpchar1, tmpreal2
        if( rds .eq. 0 ) then
          ! set index for this input value
          idx = idx + 1
          ! input provided two values, create various mineral compositions for comparison
          do jdx = 1, 12
            ! assign data name
            dataname(idx) = tmpchar1(1)
            ! assign data value
            bsdblk(idx) = tmpreal2(1)
            ! convert from % to fraction
            bsfom(idx) = tmpreal2(2) / 100.0
            select case (jdx)
            case (1)
              ! sand
              soilname(idx) = 'S*'
              bsfcla(idx) = 0.03
              bsfsan(idx) = 0.93
              bsfsil(idx) = 1.0 - bsfcla(idx) - bsfsan(idx)
            case (2)
              ! loamy sand
              soilname(idx) = 'LS*'
              bsfcla(idx) = 0.05
              bsfsan(idx) = 0.83
              bsfsil(idx) = 1.0 - bsfcla(idx) - bsfsan(idx)
            case (3)
              ! sandy loam
              soilname(idx) = 'SL*'
              bsfcla(idx) = 0.11
              bsfsan(idx) = 0.65
              bsfsil(idx) = 1.0 - bsfcla(idx) - bsfsan(idx)
            case (4)
              ! loam
              soilname(idx) = 'L*'
              bsfcla(idx) = 0.18
              bsfsan(idx) = 0.41
              bsfsil(idx) = 1.0 - bsfcla(idx) - bsfsan(idx)
            case (5)
              ! silt loam
              soilname(idx) = 'SiL*'
              bsfcla(idx) = 0.13
              bsfsan(idx) = 0.21
              bsfsil(idx) = 1.0 - bsfcla(idx) - bsfsan(idx)
            case (6)
              ! sandy clay loam
              soilname(idx) = 'Si*'
              bsfcla(idx) = 0.06
              bsfsan(idx) = 0.07
              bsfsil(idx) = 1.0 - bsfcla(idx) - bsfsan(idx)
            case (7)
              ! sandy clay loam
              soilname(idx) = 'SCL*'
              bsfcla(idx) = 0.27
              bsfsan(idx) = 0.61
              bsfsil(idx) = 1.0 - bsfcla(idx) - bsfsan(idx)
            case (8)
              ! clay loam
              soilname(idx) = 'CL*'
              bsfcla(idx) = 0.33
              bsfsan(idx) = 0.33
              bsfsil(idx) = 1.0 - bsfcla(idx) - bsfsan(idx)
            case (9)
              ! silty clay loam
              soilname(idx) = 'SiCL*'
              bsfcla(idx) = 0.33
              bsfsan(idx) = 0.11
              bsfsil(idx) = 1.0 - bsfcla(idx) - bsfsan(idx)
            case (10)
              ! sandy clay
              soilname(idx) = 'SC*'
              bsfcla(idx) = 0.40
              bsfsan(idx) = 0.53
              bsfsil(idx) = 1.0 - bsfcla(idx) - bsfsan(idx)
            case (11)
              ! silty clay
              soilname(idx) = 'SiC*'
              bsfcla(idx) = 0.45
              bsfsan(idx) = 0.07
              bsfsil(idx) = 1.0 - bsfcla(idx) - bsfsan(idx)
            case (12)
              ! clay
              soilname(idx) = 'C*'
              bsfcla(idx) = 0.65
              bsfsan(idx) = 0.18
              bsfsil(idx) = 1.0 - bsfcla(idx) - bsfsan(idx)
            end select
            ! estimate cec from input data
            bszlyd(idx) = 100.0
            bsfcec(idx) = bsfcla(idx)*100*0.5 + bsfom(idx)*100.*2.0
            idx = idx + 1
          end do
          idx = idx - 1
        end if
      end if
      ! read next line
      read (lui1,'(a)',iostat=ios) line
    end do
  end if
  close(lui1)

  ! find densities adjusted for organic matter
  call proptext( nsoil, bsfcla, bsfsan, bsfom, bsdsblk, bsdprocblk, bsdpart )
    
  if( proc_wepp ) then
    ! open output file
    call fopenk (luo1, 'proc-wepp-ref.txt', 'replace')
    write(luo1,*) 'Name Texture proc_WEPP Settled_BD Ref_BD Proc_BD PartDen C_frac Si_frac S_frac OM_frac OWC'
    do idx = 1, nsoil
      write(luo1,*) dataname(idx), soilname(idx), bsdblk(idx), bsdsblk(idx), &
                    testsetbdref( bsfcla(idx), bsfsan(idx), bsfom(idx) ), bsdprocblk(idx), &
                    bsdpart(idx), bsfcla(idx), bsfsil(idx), bsfsan(idx), bsfom(idx), &
                    testoptimalwat( bsfcla(idx), bsfsan(idx), bsfom(idx) )
    end do
    close(luo1)
  else if( proc_serdp ) then
    ! open output file
    call fopenk (luo1, 'proc-serdp-ref.txt', 'replace')
    write(luo1,*) 'Name Texture proc_SERDP Settled_BD Ref_BD Proc_BD PartDen C_frac Si_frac S_frac OM_frac OWC'
    do idx = 1, nsoil
      write(luo1,*) dataname(idx), soilname(idx), bsdblk(idx), bsdsblk(idx), &
                    testsetbdref( bsfcla(idx), bsfsan(idx), bsfom(idx) ), bsdprocblk(idx), &
                    bsdpart(idx), bsfcla(idx), bsfsil(idx), bsfsan(idx), bsfom(idx), &
                    testoptimalwat( bsfcla(idx), bsfsan(idx), bsfom(idx) )
    end do
    close(luo1)
  else if( proc_pref ) then
    ! open output file
    call fopenk (luo1, 'proc-pref-ref.txt', 'replace')
    write(luo1,*) 'Name Texture ref_data_BD Settled_BD Ref_BD Proc_BD PartDen C_frac Si_frac S_frac OM_frac OWC'
    do idx = 1, nsoil
      write(luo1,*) dataname(idx), soilname(idx), bsdblk(idx), bsdsblk(idx), &
                    testsetbdref( bsfcla(idx), bsfsan(idx), bsfom(idx) ), bsdprocblk(idx), &
                    bsdpart(idx), bsfcla(idx), bsfsil(idx), bsfsan(idx), bsfom(idx), &
                    testoptimalwat( bsfcla(idx), bsfsan(idx), bsfom(idx) )
    end do
    close(luo1)
  else
    if( diff ) then
      if( odat ) then
        ! open output file
        call fopenk (luo1, 'odat_diff.txt', 'replace')
        write(luo1,*) '#Texture Settled_Bulk_Den Ref_Data_Bulk_Den data-set C_frac Si_frac S_frac OM_frac'
        do idx = 1, nsoil
          write(luo1,*) soilname(idx), bsdsblk(idx), bsdblk(idx), bsdblk(idx)-bsdsblk(idx), &
                     bsfcla(idx), bsfsil(idx), bsfsan(idx), bsfom(idx)
        end do
        close(luo1)
      end if
      if( tfit ) then
        ! open output file
        call fopenk (luo1, 'tfit_diff.txt', 'replace')
        write(luo1,*) '#Texture Settled_Bulk_Den Ref_Data_Bulk_Den fit-set C_frac Si_frac S_frac OM_frac'
        do idx = 1, nsoil
          write(luo1,*) soilname(idx), bsdsblk(idx), bsdblk(idx), &
                     bsdblk(idx) - testbdref(.true.,.false.,.false.,.true.,bsfcla(idx),bsfsan(idx),bsfom(idx)), &
                     bsfcla(idx), bsfsil(idx), bsfsan(idx), bsfom(idx)
        end do
        close(luo1)
      end if
      if( value ) then
        ! open output file
        call fopenk (luo1, 'value_diff.txt', 'replace')
        write(luo1,*) '#Texture Settled_Bulk_Den Ref_Data_Bulk_Den Ref_fit C_frac Si_frac S_frac OM_frac'
        do idx = 1, nsoil
          write(luo1,*) soilname(idx), bsdsblk(idx), bsdblk(idx), &
                     testbdref(.true.,.false.,.false.,.false.,bsfcla(idx),bsfsan(idx),bsfom(idx)), &
                     bsfcla(idx), bsfsil(idx), bsfsan(idx), bsfom(idx)
        end do
        close(luo1)
      end if
    end if
    if( ratio ) then
      if( odat ) then
        ! open output file
        call fopenk (luo1, 'odat_ratio.txt', 'replace')
        write(luo1,*) '#Texture Settled_Bulk_Den Ref_Data_Bulk_Den data/set C_frac Si_frac S_frac OM_frac'
        do idx = 1, nsoil
          write(luo1,*) soilname(idx), bsdsblk(idx), bsdblk(idx), bsdblk(idx)/bsdsblk(idx), &
                     bsfcla(idx), bsfsil(idx), bsfsan(idx), bsfom(idx)
        end do
        close(luo1)
      end if
      if( tfit ) then
        ! open output file
        call fopenk (luo1, 'tfit_ratio.txt', 'replace')
        write(luo1,*) '#Texture Settled_Bulk_Den Ref_Data_Bulk_Den fit/set C_frac Si_frac S_frac OM_frac'
        do idx = 1, nsoil
          write(luo1,*) soilname(idx), bsdsblk(idx), bsdblk(idx), &
                     bsdblk(idx)/testbdref(.false.,.true.,.false.,.true.,bsfcla(idx),bsfsan(idx),bsfom(idx)), &
                     bsfcla(idx), bsfsil(idx), bsfsan(idx), bsfom(idx)
        end do
        close(luo1)
      end if
      if( value ) then
        ! open output file
        call fopenk (luo1, 'value_ratio.txt', 'replace')
        write(luo1,*) '#Texture Settled_Bulk_Den Ref_Data_Bulk_Den fit C_frac Si_frac S_frac OM_frac'
        do idx = 1, nsoil
          write(luo1,*) soilname(idx), bsdsblk(idx), bsdblk(idx), &
                     testbdref(.false.,.true.,.false.,.false.,bsfcla(idx),bsfsan(idx),bsfom(idx)), &
                     bsfcla(idx), bsfsil(idx), bsfsan(idx), bsfom(idx)
        end do
        close(luo1)
      end if
    end if
    if( drat ) then
      if( odat ) then
        ! open output file
        call fopenk (luo1, 'odat_drat.txt', 'replace')
        write(luo1,*) '#Texture Settled_Bulk_Den Ref_Data_Bulk_Den (data-set)/set C_frac Si_frac S_frac OM_frac'
        do idx = 1, nsoil
          write(luo1,*) soilname(idx), bsdsblk(idx), bsdblk(idx), (bsdblk(idx)-bsdsblk(idx))/bsdsblk(idx), &
                     bsfcla(idx), bsfsil(idx), bsfsan(idx), bsfom(idx)
        end do
        close(luo1)
      end if
      if( tfit ) then
        ! open output file
        call fopenk (luo1, 'tfit_drat.txt', 'replace')
        write(luo1,*) '#Texture Settled_Bulk_Den Ref_Data_Bulk_Den (fit-set)/set C_frac Si_frac S_frac OM_frac'
        do idx = 1, nsoil
          write(luo1,*) soilname(idx), bsdsblk(idx), bsdblk(idx), &
                     (bsdblk(idx)-testbdref(.false.,.false.,.true.,.true.,bsfcla(idx),bsfsan(idx),bsfom(idx))) &
                     /testbdref(.false.,.false.,.true.,.true.,bsfcla(idx),bsfsan(idx),bsfom(idx)), &
                     bsfcla(idx), bsfsil(idx), bsfsan(idx), bsfom(idx)
        end do
        close(luo1)
      end if
      if( value ) then
        ! open output file
        call fopenk (luo1, 'value_drat.txt', 'replace')
        write(luo1,*) '#Texture Settled_Bulk_Den Ref_Data_Bulk_Den fit C_frac Si_frac S_frac OM_frac'
        do idx = 1, nsoil
          write(luo1,*) soilname(idx), bsdsblk(idx), bsdblk(idx), &
                     testbdref(.false.,.false.,.true.,.false.,bsfcla(idx),bsfsan(idx),bsfom(idx)), &
                     bsfcla(idx), bsfsil(idx), bsfsan(idx), bsfom(idx)
        end do
        close(luo1)
      end if
    end if
  end if
  ! find particle density adjusted for organic matter
  !call proptext( nsoil, bsfcla, bsfsil, bsfsan, bsfom, bsdsblk, bsdprocblk, bsdpart )

  !do idx = 1, nsoil
  !  bsdblk(idx) = bsdsblk(idx)
  !end do
  !call param_prop_bc( nsoil, bszlyd, bsdblk, bsdpart, bsfcla, bsfsan, bsfom, bsfcec, &
  !                    bhrwcs, bhrwcf, bhrwcw, bhrwcr, bhrwca, bh0cb, bheaep, bhrsk, bhfredsat )

  ! print out results
  ! to match Rawls table, potential converted from m to cm and conductivity converted from m/s to cm/hr
  !write(*,*) 'Texture tot_porosity residual eff_porosity bub_press pore_size_dist -0.33bar -15bar k_sat'
  !do idx = 1, 11
  !  write(*,*) soilname(idx), bhrwcs(idx)*bsdblk(idx), bhrwcr(idx)*bsdblk(idx), bhrwcs(idx)*bsdblk(idx)-bhrwcr(idx)*bsdblk(idx), &
  !             100*bheaep(idx)/gravconst, 1.0/bh0cb(idx), bhrwcf(idx)*bsdblk(idx), bhrwcw(idx)*bsdblk(idx), 100*3600*bhrsk(idx)
  !end do

end program test_soil_density

subroutine usda_tx_name_frm_class( class, soilname )
  integer class
  character(LEN=7) :: soilname  ! soil texture class name

  select case (class)
  case (1)
    soilname = 'S'
  case (2)
    soilname = 'LS'
  case (3)
    soilname = 'SL'
  case (4)
    soilname = 'L'
  case (5)
    soilname = 'SiL'
  case (6)
    soilname = 'Si'
  case (7)
    soilname = 'SCL'
  case (8)
    soilname = 'CL'
  case (9)
    soilname = 'SiCL'
  case (10)
    soilname = 'SC'
  case (11)
    soilname = 'SiC'
  case (12)
    soilname = 'C'
  end select
end subroutine usda_tx_name_frm_class

function testbdref ( diff, ratio, drat, part, clay, sand, om ) result( bdref )

  ! + + + PURPOSE + + +
  ! Calculation to test the curve fits being used in reference bulk density

  use soilden_mod, only: setbds

  ! + + + ARGUMENTS + + +
  logical, intent(in) :: diff    ! if TRUE calculate difference between reference bulk density and settled bulk density
  logical, intent(in) :: ratio   ! if TRUE calculate ratio (reference bulk density / settled bulk density)
  logical, intent(in) :: drat    ! if TRUE calculate ratio ((reference-settled) / settled bulk density)
  logical, intent(in) :: part    ! if TRUE output the partially adjusted value (first fit)
  real, intent(in) :: clay       ! fraction of soil clay content (mineral fraction)
  real, intent(in) :: sand       ! fraction of soil sand content (mineral fraction)
  real, intent(in) :: om         ! fraction of soil organic matter
  real :: bdref     ! reference bulk density (compressed to 200Kpa for 1 week)

  ! + + + LOCAL VARIABLES + + +
  real :: bds                    ! settled bulk density
  real :: bd_adjustment          ! difference between settled and reference bulk density
  real :: a,b,c,d                ! curve fit for low OM data
  real, parameter :: diff_mean = 0.0666647  ! difference mean value for high OM data
  real, parameter :: ratio_mean = 1.19378   ! ratio mean value for high OM data
  real, parameter :: drat_mean = 0.193782   ! difference ration mean value for high OM data

  ! + + + END SPECIFICATIONS + + +

  bds = setbds(clay, sand, om)
  if( diff ) then
    a = -1.89943
    b = 2.92681
    c = 2.01621
    d = -0.568659
    bd_adjustment = a*cos(sand) + b*sin(sand) + c*cos(2*sand) + d*sin(2*sand)
    if( part ) then
      bdref = bds + bd_adjustment
    else
      bdref = bds + 1.0/((om/diff_mean)+(1.0-om)/bd_adjustment)
    end if
  else if( ratio ) then
    a = -0.516472
    b = 3.46117
    c = 1.64331
    d = -0.965802
    bd_adjustment = a*cos(sand) + b*sin(sand) + c*cos(2*sand) + d*sin(2*sand)
    if( part ) then
      bdref = bds * bd_adjustment
    else
      bdref = bds * 1.0/((om/ratio_mean)+(1.0-om)/bd_adjustment)
    end if
  else if( drat ) then
    a = -1.75229
    b = 2.92099
    c = 1.88065
    d = -0.709907
    bd_adjustment = a*cos(sand) + b*sin(sand) + c*cos(2*sand) + d*sin(2*sand)
    if( part ) then
      bdref = bds * bd_adjustment + bds
    else
      bdref = bds * 1.0/((om/drat_mean)+(1.0-om)/bd_adjustment) + bds
    end if
  end if

  return

end function testbdref

function testsetbdref ( clay, sand, om ) result( bdref )

  ! + + + PURPOSE + + +
  ! The following function estimates a reference soil bulk density from
  ! intrinsic properties. see Thomas Keller, Inge Håkansson. 2010. Estimation
  ! of reference bulk density from soil particle size distribution and soil
  ! organic matter content. Geoderma 154:398–406.

  ! The multilinear fit reported in the paper

  ! The article references: Heinonen, R., 1960. Das Volumengewicht als Kennzeichen
  ! der “normalen” Bodenstruktur. Zeitschrift der Landwirtschafts-wissenschaftlichen
  ! Gesellschaft in Finnland, vol. 32, pp. 81–87. A regression equation for "natural"
  ! or settled bulk density is provided. When tested, it matches RAWLS settled bulk
  ! density closely for OM less than 0.1
  ! bd_normal = 1.40 - 7.2*om - 0.13*clay + 0.14*sand

  use soilden_mod, only: setbds

  ! + + + ARGUMENTS + + +
  real, intent(in) :: clay       ! fraction of soil clay content (mineral fraction)
  real, intent(in) :: sand       ! fraction of soil sand content (mineral fraction)
  real, intent(in) :: om         ! fraction of soil organic matter
  real :: bdref     ! reference bulk density (compressed to 200Kpa for 1 week)

  ! + + + PARAMETERS + + +
  real, parameter :: a = -1.89943
  real, parameter :: b = 2.92681
  real, parameter :: c = 2.01621
  real, parameter :: d = -0.568659

  ! + + + LOCAL VARIABLES + + +
  real :: bd_adjustment              ! difference between settled and reference bulk density

  ! + + + END SPECIFICATIONS + + +

  bd_adjustment = a*cos(sand) + b*sin(sand) + c*cos(2*sand) + d*sin(2*sand)

  bdref = setbds(clay, sand, om) + 1.0/((om/0.1)+(1.0-om)/bd_adjustment)

  return

end function testsetbdref

function testoptimalwat( clay, sand, om ) result( owc )

  ! + + + PURPOSE + + +
  ! Calculation to test proctor density calculation

  real, intent(in) :: clay       ! fraction of soil clay content (mineral fraction)
  real, intent(in) :: sand       ! fraction of soil sand content (mineral fraction)
  real, intent(in) :: om         ! fraction of soil organic matter
  real :: owc          ! optimal water content for compaction (%)

  ! + + + LOCAL VARIABLES + + +
  real, parameter :: owc_a = 16.0932
  real, parameter :: owc_b = 0.129032*100
  real, parameter :: owc_c = -0.0883454*100
  real, parameter :: owc_d = 1.06305*100

  ! find optimal water content
  owc = owc_a + owc_b*clay + owc_c*sand + owc_d*om

end function testoptimalwat
