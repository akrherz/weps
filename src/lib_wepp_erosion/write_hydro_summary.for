!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine write_hydro_summary(sumfile,totalPrecip,precipEvents,  &
     & totalRunoff,runoffEvents,totalSnowrunoff, snowmeltEvents,years)
      
      
      integer, intent(in) :: sumfile
      real, intent(in) :: totalPrecip,totalRunoff, totalSnowrunoff
      integer, intent(in):: precipEvents, runoffEvents,snowmeltEvents
      integer, intent(in) :: years
      
      write(sumfile,1050)
      write(sumfile,1200) 1,years
      write(sumfile,1350) precipEvents,totalPrecip,runoffEvents,        &
     &    totalRunoff,snowmeltEvents,totalSnowrunoff
     
      write(sumfile,1650) years,totalPrecip/years,totalRunoff/years,    &
     &  totalSnowrunoff/years
      
      return 
      
 1050 format(//'AVERAGE ANNUAL SUMMARIES',/,72('-'))
 1200 format (//'I.   RAINFALL AND RUNOFF SUMMARY',/,5x,8('-'),1x,3('-' &
     &    ),1x,6('-'),1x,7('-'),//,6x,'total summary: ',' years ',i4,   &
     &    ' - ',i4)
 1350 format(/5x,i5,                                                    &
     &    ' storms produced                       ',f9.2,               &
     &    ' mm of precipitation',/,5x,i5,                               &
     &    ' rain storm runoff events produced     ',f9.2,               &
     &    ' mm of runoff',/,5x,i5,                                      &
     &    ' snow melts and/or',/,10x,                                   &
     &    '   events during winter produced       ',f9.2,               &
     &    ' mm of runoff',/)  
 1650 format (6x,'annual averages'/6x,'---------------'//6x,            &
     &    '  Number of years                              ',            &
     &    3x,i4,/,6x,                                                   &
     &    '  Mean annual precipitation                    ',            &
     &    f7.2,1x,'mm',/,6x,                                            &
     &    '  Mean annual runoff from rainfall             ',            &
     &    f7.2,1x,'mm',/,6x,                                            &
     &    '  Mean annual runoff from snow melt',/,6x,                   &
     &    '    and/or rain storm during winter            ',            &
     &    f7.2,1x,'mm',/)
      
      
      end
