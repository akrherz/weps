      Program tstasd4

      use soil_data_struct_defs, only: soil_def, allocate_soil

      include 'p1werm.inc'
      include 'm1subr.inc'
      include 's1layr.inc'
      include 's1agg.inc'
      include 'manage/asd.inc' !msieve = 26 (allocation for maximum number of sieves) and sdia(msieve) defined here

      type(soil_def), dimension(:), allocatable :: soil, soil2             ! structure with soil state and parameters as updated suring simulation

      integer :: alloc_stat = 0, sum_stat = 0
      integer :: nsubr = 1
      integer :: nsl = 1

      real    :: total = 0.0

      integer :: sr,l,i
      real    :: massf(msieve+1,mnsz) ! allocate space for the maximum number of sieve "cuts" (msieve+1)
      real    :: gmd_prime, gsd_prime, gmd2_prime, gsd2_prime

      real    :: initgmd = 3.75 , initgsd = 39.55
      real    :: m_not = 0.005, m_inf = 1000.0

      logcas = 3
      nsieve = msieve - 1 ! number of sieves set to 25 here therefore (nsieve+1) is the number of sieve "cuts" used
      mnsize = 0.005
      mxsize = 1000.0

! allocate space for "soil" structures
      allocate(soil(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(soil2(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      if( sum_stat .gt. 0 ) then
        write(0,*) "ERROR: unable to allocate enough memory for weps main data arrays."
      end if

! allocate space for soil layer data
      do sr=1, nsubr
        soil(sr)%nslay = nsl
        soil2(sr)%nslay = nsl
        call allocate_soil(soil(sr))  ! allocate layer arrays
        call allocate_soil(soil2(sr)) ! allocate layer arrays
        write (0,*) 'sr: ', sr, 'nsl: ',nsl, 'soil(sr)%nslay: ',soil(sr)%nslay, 'soil2(sr)%nslay: ', soil2(sr)%nslay
      end do

!     compute geometric mean (lognormal) distribution of sieve sizes (dia.) for each sieve cut
      write(0,*) "sieve sizes - dia. in (mm): sdia(i) values"
      do i = 1, nsieve
          sdia(i) = exp(log(mnsize) + i*(log(mxsize)-log(mnsize))/(nsieve+1))
      end do
      write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieve)
      write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (sdia(i), i=1, nsieve)
      write(0,*)

!     compute geometric mean value - dia. size in (mm) for each sieve "cut"
      write(0,*) "compute geometric mean value for each sieve cut"
      mdia(1) = sqrt(mnsize*sdia(1))
      do i = 2, nsieve
           mdia(i) = sqrt(sdia(i)*sdia(i-1))
      end do
      mdia(nsieve+1) = sqrt(mxsize*sdia(nsieve))

      write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieve+1)
      write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (mdia(i), i=1, nsieve+1)
      write(0,*)

!     Initialize m_not, gmd, gsd and m_inf values
      do sr=1, nsubr
        do l=1, soil(sr)%nslay
          soil(sr)%aslagn(l) = m_not
          soil(sr)%aslagm(l) = initgmd
          soil(sr)%as0ags(l) = initgsd
          soil(sr)%aslagx(l) = m_inf
        end do
      end do
      soil2 = soil

      write(UNIT=6,FMT="(2(A), 6(A))",ADVANCE="YES") '  sr', ' lay', '     m_not', '   initgmd', '   initgsd', &
                                                                     '     m_inf', ' gmd_prime', ' gsd_prime'
      do sr=1, nsubr
        do l=1, soil(sr)%nslay
          write(UNIT=6,FMT="(2(i4), 4(f10.4))",ADVANCE="NO") sr, l, &
               soil(sr)%aslagn(l), soil(sr)%aslagm(l), soil(sr)%as0ags(l), soil(sr)%aslagx(l)

          gmd_prime = (soil(sr)%aslagm(l)-soil(sr)%aslagn(l)) * (soil(sr)%aslagx(l)-soil(sr)%aslagn(l)) / &
               (soil(sr)%aslagx(l)-soil(sr)%aslagm(l))
          gsd_prime = (soil(sr)%as0ags(l)-soil(sr)%aslagn(l)) * (soil(sr)%aslagx(l)-soil(sr)%aslagn(l)) / &
               (soil(sr)%aslagx(l)-soil(sr)%as0ags(l))
          write(UNIT=6,FMT="(2(f10.4))",ADVANCE="YES") gmd_prime, gsd_prime

        end do
        write(0,*)
      end do


! Convert to massf
      do sr=1, nsubr
        do l=1, soil(sr)%nslay
          call asd2m(soil(sr)%aslagn(l), soil(sr)%aslagx(l),                &
     &          soil(sr)%aslagm(l), soil(sr)%as0ags(l), &
!     &         gmd_prime, gsd_prime,                      &
     &         soil(sr)%nslay, massf)

          write(0,*) 'sr lay:',sr, l, 'nsieve+1', nsieve+1
          write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieve+1)
          write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (massf(i,sr), i=1, nsieve+1)

          total = 0.0
          do i=1, msieve
             total = total + massf(i,sr)
          end do
          write(0,*) 'total: ', total
          write(0,*)
        end do
      end do

!      subroutine m2asd (mf, nlay, mnot, minf, gmd, gsd)
      do sr=1, nsubr
        do l=1, soil(sr)%nslay
          call m2asd(massf, soil2(sr)%nslay,                                 &
     &         soil2(sr)%aslagn(l), soil2(sr)%aslagx(l),                      &
     &         soil2(sr)%aslagm(l), soil2(sr)%as0ags(l))

          write(UNIT=6,FMT="(2(i4), 4(f10.4))",ADVANCE="NO") sr, l, &
               soil2(sr)%aslagn(l), soil2(sr)%aslagm(l), soil2(sr)%as0ags(l), soil2(sr)%aslagx(l)

          gmd2_prime = (soil2(sr)%aslagm(l)-soil2(sr)%aslagn(l)) * (soil2(sr)%aslagx(l)-soil2(sr)%aslagn(l)) / &
               (soil2(sr)%aslagx(l)-soil2(sr)%aslagm(l))
          gsd2_prime = (soil2(sr)%as0ags(l)-soil2(sr)%aslagn(l)) * (soil2(sr)%aslagx(l)-soil2(sr)%aslagn(l)) / &
               (soil2(sr)%aslagx(l)-soil2(sr)%as0ags(l))
          write(UNIT=6,FMT="(2(f10.4))",ADVANCE="YES") gmd2_prime, gsd2_prime

          write(0,*)

        end do
      end do

      do sr=1, nsubr
        do l=1, soil(sr)%nslay
          call m2asd(massf, soil2(sr)%nslay,                                 &
     &         soil2(sr)%aslagn(l), soil2(sr)%aslagx(l),                      &
     &         soil2(sr)%aslagm(l), soil2(sr)%as0ags(l))

          write(UNIT=6,FMT="(2(i4), 4(f10.4))",ADVANCE="NO") sr, l, &
               soil2(sr)%aslagn(l), soil2(sr)%aslagm(l), soil2(sr)%as0ags(l), soil2(sr)%aslagx(l)

          gmd_prime = (gmd_prime-soil(sr)%aslagn(l)) * (soil(sr)%aslagx(l)-soil(sr)%aslagn(l)) / &
               (soil(sr)%aslagx(l)- gmd_prime)
          gsd_prime = (gsd_prime-soil(sr)%aslagn(l)) * (soil(sr)%aslagx(l)-soil(sr)%aslagn(l)) / &
               (soil(sr)%aslagx(l)-gsd_prime)
          write(UNIT=6,FMT="(2(f10.4))",ADVANCE="YES") gmd_prime, gsd_prime

          write(0,*)

        end do
      end do


      stop
      end program