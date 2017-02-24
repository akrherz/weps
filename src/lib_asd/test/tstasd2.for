!$Author: wagner $
!$Date: 2017-02-14 17:09:03 -0700 (Tue, 14 Feb 2017) $
!$Revision: 14989 $
!$HeadURL: https://infosys.ars.usda.gov/svn/code/weps1/branches/weps.src.subregion/src/lib_asd/test/tstasd.for $

      Program tstasd

      use soil_data_struct_defs, only: soil_def, allocate_soil

      include 'p1werm.inc'
      include 'm1subr.inc'
      include 's1layr.inc'
      include 's1agg.inc'
      include 'manage/asd.inc'

      type(soil_def), dimension(:), allocatable :: soil, soil2             ! structure with soil state and parameters as updated suring simulation

      integer :: alloc_stat, sum_stat
      integer :: nsubr

      integer :: sr,lay,l,i,iter
      real    :: massf(msieve+1,mnsz)
      real    :: initgmd, initgsd

      nsubr = 1
      sum_stat = 0
      allocate(soil(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(soil2(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      if( sum_stat .gt. 0 ) then
!        write(0,*) "ERROR: unable to allocate enough memory for weps main data arrays."
      end if

      do sr=1, nsubr
        soil(sr)%nslay = 29
        soil2(sr)%nslay = 29
        call allocate_soil(soil(sr))  ! allocate layer arrays
        call allocate_soil(soil2(sr)) ! allocate layer arrays
      end do

      write (0,*) 'soil(1)%nslay: ', soil(1)%nslay

      logcas = 3

      call asdini()


! conversions from distribution to bin and back are tested and compared.
! conversions are perfect if gsd is set equal to e
! error is decreased if the number of bins is increased (msieve in asd.inc)
! experimentation with adaptive binning yielded moderate improvements
! but would require maintaining separate sieve arrays for every soil layer.
! A middle approach of keeping gsd above 2.0 (mingsd in asd.inc)
! keeps errors on the large end within reason. High values of GSD
! result in reduction of GMD by the conversion in all ranges, but more so
! with larger GMD. The cure is to go to bins in all phases.


! Convert to massf and back again, then print
      do sr=1, nsubr
        do l=1, soil(sr)%nslay
          call asd2m(soil(sr)%aslagn(l), soil(sr)%aslagx(l),                &
     &         soil(sr)%aslagm(l), soil(sr)%as0ags(l),                      &
     &         soil(sr)%nslay, massf)


         call m2asd(massf, soil(1)%nslay,                                   &
     &   soil(1)%aslagn(1), soil(1)%aslagx(1),                              &
     &   soil(1)%aslagm(1), soil(1)%as0ags(1))


!         print*,'subregion',sr,' after m2asd, iteration',iter
!         do lay=1, soil(1)%nslay
!            print*, iter, initgmd, soil(1)%aslagm(lay), initgsd,
!     &              soil(1)%as0ags(lay),
!     &              soil(1)%aslagn(lay), soil(1)%aslagx(lay)
!            do i=1,nsieve+1
!                print*, mdia(i),'massf(',i,',',lay,')',massf(i,lay)
!            end do
!         end do

         initgmd = 0.0
         initgsd = 0.0
         do lay=1, soil(1)%nslay
           write(*,*) lay, initgmd, soil(1)%aslagm(lay),                        &
     &                initgsd, soil(1)%as0ags(lay)
         end do
!      end do
      write(0,*)

! New data test code here - 2/14/2017 - LEW

!     Initialize "soil2" data to "soil" data
      do sr=1, nsubr
         soil2(sr)%aslagn = soil(sr)%aslagn
         soil2(sr)%aslagm = soil(sr)%aslagm
         soil2(sr)%as0ags = soil(sr)%as0ags
         soil2(sr)%aslagx = soil(sr)%aslagx
      end do

      write(UNIT=6,FMT="(2(A), 4(A))",ADVANCE="YES") '  sr', ' lay',            &
     &        '     m_not', '       gsd', '       gsd', '     m_inf'

      do sr=1, nsubr
        do lay=1, soil(sr)%nslay
          write(UNIT=6,FMT="(2(i4), 4(f10.4))",ADVANCE="YES") sr, lay,          &
     &              soil(sr)%aslagn(lay), soil(sr)%aslagm(lay),                 &
     &              soil(sr)%as0ags(lay), soil(sr)%aslagx(lay)
        end do
      end do
      write(0,*)

! Convert to massf and back again, then print
      do sr=1, nsubr
        do l=1, soil(sr)%nslay
          call asd2m(soil(sr)%aslagn(l), soil(sr)%aslagx(l),                &
     &         soil(sr)%aslagm(l), soil(sr)%as0ags(l),                      &
     &         soil(sr)%nslay, massf)

          call m2asd(massf, soil2(sr)%nslay,                                    &
     &         soil2(sr)%aslagn(l), soil2(sr)%aslagx(l),                    &
     &         soil2(sr)%aslagm(l), soil2(sr)%as0ags(l))
 
         write(UNIT=6,FMT="(2(i4), 8(f10.4))",ADVANCE="YES") sr, lay,           &
     &              soil2(sr)%aslagn(l), soil2(sr)%aslagm(l),               &
     &              soil2(sr)%as0ags(l), soil2(sr)%aslagx(l),               &
     &    (soil(sr)%aslagn(l)-soil2(sr)%aslagn(l))/soil(sr)%aslagn(l),    &
     &    (soil(sr)%aslagm(l)-soil2(sr)%aslagm(l))/soil(sr)%aslagm(l),    &
     &    (soil(sr)%as0ags(l)-soil2(sr)%as0ags(l))/soil(sr)%as0ags(l),    &
     &    (soil(sr)%aslagx(l)-soil2(sr)%aslagx(l))/soil(sr)%aslagx(l)


        end do
      end do
      write(0,*)

      stop
      end
