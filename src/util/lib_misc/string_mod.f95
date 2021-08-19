!$Author$
!$Date$
!$Revision$
!$HeadURL$
module string_mod

  contains

    function space2unders( input_string ) result ( output_string )

      ! input string
      character(len=*), intent(in) :: input_string
      ! result string
      character(len=len(input_string)) :: output_string

      ! local vars
      integer :: idx    ! loop index
      integer :: iachar_char ! character expressed as integer
      integer, parameter :: iachar_space = 32
      integer, parameter :: iachar_tab = 9
      
      ! initialize output_string
      output_string = ' '
      ! loop over all string characters
      do idx = 1, len(input_string)
        ! -- Convert the current character to its position
        ! -- in the ASCII collating sequence
        iachar_char = IACHAR( input_string( idx:idx ) )

        if( iachar_char /= iachar_space .and. iachar_char /= iachar_tab ) then
          ! not a space or tab, copy to output string
          output_string( idx:idx ) = input_string( idx:idx )
        else
          ! space or tab, replace with underscore
          output_string( idx:idx ) = '_'
        end if

      end do
     
    end function space2unders
    
    function space2hyphen( input_string ) result ( output_string )

      ! input string
      character(len=*), intent(in) :: input_string
      ! result string
      character(len=len(input_string)) :: output_string

      ! local vars
      integer :: idx    ! loop index
      integer :: iachar_char ! character expressed as integer
      integer, parameter :: iachar_space = 32
      integer, parameter :: iachar_tab = 9
      
      ! initialize output_string
      output_string = ' '
      ! loop over all string characters
      do idx = 1, len(input_string)
        ! -- Convert the current character to its position
        ! -- in the ASCII collating sequence
        iachar_char = IACHAR( input_string( idx:idx ) )

        if( iachar_char /= iachar_space .and. iachar_char /= iachar_tab ) then
          ! not a space or tab, copy to output string
          output_string( idx:idx ) = input_string( idx:idx )
        else
          ! space or tab, replace with underscore
          output_string( idx:idx ) = '-'
        end if

      end do
     
    end function space2hyphen

end module string_mod
