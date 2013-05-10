#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
# include <strings.h>
#endif

#include "protos.h"
#include "registry.h"
#include "data.h"
#include "sym.h"

int gen_streams(  char * dirname ) 
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn ;
  if ( dirname == NULL ) return(1) ;

  fn = "module_io_domain_defs.inc" ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_io_domain_defs( fp ) ;
  close_the_file( fp ) ;

  fn = "set_timekeeping_defs.inc" ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_set_timekeeping_defs( fp ) ;
  close_the_file( fp ) ;

  fn = "set_timekeeping_alarms.inc" ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_set_timekeeping_alarms( fp ) ;
  close_the_file( fp ) ;

  fn = "io_form_for_dataset.inc" ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_io_form_for_dataset( fp ) ;
  close_the_file( fp ) ;

  fn = "io_form_for_stream.inc" ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_io_form_for_stream( fp ) ;
  close_the_file( fp ) ;

  fn = "switches_and_alarms.inc" ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_switches_and_alarms( fp ) ;
  close_the_file( fp ) ;

  fn = "check_auxstream_alarms.inc" ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_check_auxstream_alarms( fp ) ;
  close_the_file( fp ) ;

  fn = "fine_stream_input.inc" ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_fine_stream_input( fp ) ;
  close_the_file( fp ) ;

  fn = "med_auxinput_in.inc" ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_med_auxinput_in( fp ) ;
  close_the_file( fp ) ;

  fn = "med_hist_out_opens.inc" ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_med_hist_out_opens( fp ) ;
  close_the_file( fp ) ;

  fn = "med_hist_out_closes.inc" ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_med_hist_out_closes( fp ) ;
  close_the_file( fp ) ;

  fn = "med_auxinput_in_closes.inc" ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_med_auxinput_in_closes( fp ) ;
  close_the_file( fp ) ;

  fn = "med_last_solve_io.inc" ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_med_last_solve_io( fp ) ;
  close_the_file( fp ) ;

  fn = "med_open_esmf_calls.inc" ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_med_open_esmf_calls( fp ) ;
  close_the_file( fp ) ;

  fn = "med_find_esmf_coupling.inc" ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_med_find_esmf_coupling( fp ) ;
  close_the_file( fp ) ;

  fn = "shutdown_closes.inc" ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_shutdown_closes( fp ) ;
  close_the_file( fp ) ;

  return(0) ;
}

int
gen_io_domain_defs ( FILE * fp )
{
  char * dir , * aux , *streamtype , streamno[5]  ;
  int i, j ;

  for ( j = 0 ; j < 2 ; j++ ) {
    if ( j == 0 ) dir = "output" ;
    else          dir = "input"  ;
    for ( i = 0 ; i < 2*MAX_HISTORY ; i++ ) 
    {
      if ( i % MAX_HISTORY == 0 ) { aux = ""  ; streamno[0] = '\0' ; }
      else                        { aux="aux" ; sprintf(streamno,"%d",i%MAX_HISTORY) ; }
      if ( i < MAX_HISTORY )      { streamtype = "input" ; }
      else                        { streamtype = ( i%MAX_HISTORY == 0 )?"history":"hist" ; }

      fprintf(fp,"SUBROUTINE %s_%s%s%s ( fid , grid , config_flags , ierr )\n",dir,aux,streamtype,streamno) ;
      fprintf(fp," IMPLICIT NONE\n") ;
      fprintf(fp," TYPE(domain) :: grid\n") ;
      fprintf(fp," TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags\n") ;
      fprintf(fp," INTEGER, INTENT(IN) :: fid\n") ;
      fprintf(fp," INTEGER, INTENT(INOUT) :: ierr\n") ;
      fprintf(fp," IF ( config_flags%%io_form_%s%s%s .GT. 0 ) THEN\n", aux,streamtype,streamno) ;
      fprintf(fp,"   CALL %s_wrf( fid, grid, config_flags, %s%s%s_only, ierr ) ;\n",dir,aux,streamtype,streamno) ;
      fprintf(fp," ENDIF\n") ;
      fprintf(fp," RETURN\n") ;
      fprintf(fp,"END SUBROUTINE %s_%s%s%s\n",dir,aux,streamtype,streamno) ;
    }
  }
  return 0; /* SamT: bug fix: return a value */
}

int
gen_set_timekeeping_defs ( FILE *fp )
{
  char * aux , *streamtype , streamno[5]  ;
  int i ;
  for ( i = 0 ; i < 2*MAX_HISTORY ; i++ ) 
  {
    if ( i % MAX_HISTORY == 0 ) { aux = ""  ; streamno[0] = '\0' ; }
    else                        { aux="aux" ; sprintf(streamno,"%d",i%MAX_HISTORY) ; }
    if ( i < MAX_HISTORY )      { streamtype = "input" ; }
    else                        { streamtype = ( i%MAX_HISTORY == 0 )?"history":"hist" ; }

    fprintf(fp," INTEGER :: %s%s%s_interval  , &\n",aux,streamtype,streamno) ;
    fprintf(fp,"            %s%s%s_interval_d, &\n",aux,streamtype,streamno) ;
    fprintf(fp,"            %s%s%s_interval_h, &\n",aux,streamtype,streamno) ;
    fprintf(fp,"            %s%s%s_interval_m, &\n",aux,streamtype,streamno) ;
    fprintf(fp,"            %s%s%s_interval_s   \n",aux,streamtype,streamno) ;
    fprintf(fp," INTEGER :: %s%s%s_begin  ,    &\n",aux,streamtype,streamno) ;
    fprintf(fp,"            %s%s%s_begin_y,    &\n",aux,streamtype,streamno) ;
    fprintf(fp,"            %s%s%s_begin_d,    &\n",aux,streamtype,streamno) ;
    fprintf(fp,"            %s%s%s_begin_h,    &\n",aux,streamtype,streamno) ;
    fprintf(fp,"            %s%s%s_begin_m,    &\n",aux,streamtype,streamno) ;
    fprintf(fp,"            %s%s%s_begin_s      \n",aux,streamtype,streamno) ;
    fprintf(fp," INTEGER :: %s%s%s_end  ,      &\n",aux,streamtype,streamno) ;
    fprintf(fp,"            %s%s%s_end_y,      &\n",aux,streamtype,streamno) ;
    fprintf(fp,"            %s%s%s_end_d,      &\n",aux,streamtype,streamno) ;
    fprintf(fp,"            %s%s%s_end_h,      &\n",aux,streamtype,streamno) ;
    fprintf(fp,"            %s%s%s_end_m,      &\n",aux,streamtype,streamno) ;
    fprintf(fp,"            %s%s%s_end_s        \n",aux,streamtype,streamno) ;
  }
  return 0; /* SamT: bug fix: return a value */
}

int
gen_set_timekeeping_alarms ( FILE * fp )
{
  char * dir , * aux , *streamtype , streamno[5]  ;
  int i, j ;

  for ( i = 0 ; i < 2*MAX_HISTORY ; i++ )
  {
    if ( i % MAX_HISTORY == 0 ) { aux = ""  ; streamno[0] = '\0' ; }
    else                        { aux="aux" ; sprintf(streamno,"%d",i%MAX_HISTORY) ; }
    if ( i < MAX_HISTORY )      { streamtype = "input" ; }
    else                        { streamtype = ( i%MAX_HISTORY == 0 )?"history":"hist" ; }
    if ( i == 0 ) continue ;  /* skip just input */

    fprintf(fp,"! %s%s%s INTERVAL\n",aux,streamtype,streamno) ;
    fprintf(fp,"   CALL nl_get_%s%s%s_interval( grid%%id, %s%s%s_interval )   ! same as minutes\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   CALL nl_get_%s%s%s_interval_d( grid%%id, %s%s%s_interval_d )\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   CALL nl_get_%s%s%s_interval_h( grid%%id, %s%s%s_interval_h )\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   CALL nl_get_%s%s%s_interval_m( grid%%id, %s%s%s_interval_m )\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   CALL nl_get_%s%s%s_interval_s( grid%%id, %s%s%s_interval_s )\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   IF ( %s%s%s_interval_m .EQ. 0 ) %s%s%s_interval_m = %s%s%s_interval\n",aux,streamtype,streamno,aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   IF ( MAX( %s%s%s_interval_d,   &\n",aux,streamtype,streamno) ;
    fprintf(fp,"             %s%s%s_interval_h, %s%s%s_interval_m , %s%s%s_interval_s   ) .GT. 0 ) THEN\n",aux,streamtype,streamno,aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"     CALL WRFU_TimeIntervalSet( interval, D=%s%s%s_interval_d, &\n",aux,streamtype,streamno) ;
    fprintf(fp,"                                        H=%s%s%s_interval_h, M=%s%s%s_interval_m, S=%s%s%s_interval_s, rc=rc )\n",aux,streamtype,streamno,aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"     CALL wrf_check_error( WRFU_SUCCESS, rc, &\n") ;
    fprintf(fp,"                           'WRFU_TimeIntervalSet(%s%s%s_interval) FAILED', &\n",aux,streamtype,streamno) ;
    fprintf(fp,"                           __FILE__ , &\n") ;
    fprintf(fp,"                           __LINE__  )\n") ;
    fprintf(fp,"   ELSE\n") ;
#if 0
    fprintf(fp,"     interval = run_length + padding_interval\n") ;
#else
    fprintf(fp,"     interval =  padding_interval\n") ;
#endif
    fprintf(fp,"   ENDIF\n") ;
    fprintf(fp,"   CALL nl_get_%s%s%s_begin  ( grid%%id, %s%s%s_begin   )\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   CALL nl_get_%s%s%s_begin_y( grid%%id, %s%s%s_begin_y )\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   CALL nl_get_%s%s%s_begin_d( grid%%id, %s%s%s_begin_d )\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   CALL nl_get_%s%s%s_begin_h( grid%%id, %s%s%s_begin_h )\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   CALL nl_get_%s%s%s_begin_m( grid%%id, %s%s%s_begin_m )\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   CALL nl_get_%s%s%s_begin_s( grid%%id, %s%s%s_begin_s )\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   IF ( %s%s%s_begin_m .EQ. 0 ) %s%s%s_begin_m = %s%s%s_begin\n",aux,streamtype,streamno,aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   IF ( MAX( %s%s%s_begin_y, %s%s%s_begin_d,   &\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"             %s%s%s_begin_h, %s%s%s_begin_m , %s%s%s_begin_s   ) .GT. 0 ) THEN\n",aux,streamtype,streamno,aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"      CALL WRFU_TimeIntervalSet( begin_time , D=%s%s%s_begin_d, &\n",aux,streamtype,streamno) ;
    fprintf(fp,"                                      H=%s%s%s_begin_h, M=%s%s%s_begin_m, S=%s%s%s_begin_s, rc=rc )\n",aux,streamtype,streamno,aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"      CALL wrf_check_error( WRFU_SUCCESS, rc, &\n") ;
    fprintf(fp,"                            'WRFU_TimeIntervalSet(%s%s%s_begin) FAILED', &\n",aux,streamtype,streamno) ;
    fprintf(fp,"                            __FILE__ , &\n") ;
    fprintf(fp,"                            __LINE__  )\n") ;
    fprintf(fp,"   ELSE\n") ;
    fprintf(fp,"      begin_time = zero_time\n") ;
    fprintf(fp,"   ENDIF\n") ;
    fprintf(fp,"   CALL nl_get_%s%s%s_end( grid%%id, %s%s%s_end )\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   CALL nl_get_%s%s%s_end_y( grid%%id, %s%s%s_end_y )\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   CALL nl_get_%s%s%s_end_d( grid%%id, %s%s%s_end_d )\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   CALL nl_get_%s%s%s_end_h( grid%%id, %s%s%s_end_h )\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   CALL nl_get_%s%s%s_end_m( grid%%id, %s%s%s_end_m )\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   CALL nl_get_%s%s%s_end_s( grid%%id, %s%s%s_end_s )\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   IF ( %s%s%s_end_m .EQ. 0 ) %s%s%s_end_m = %s%s%s_end\n",aux,streamtype,streamno,aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"   IF ( MAX( %s%s%s_end_y, %s%s%s_end_d,   &\n",aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"             %s%s%s_end_h, %s%s%s_end_m , %s%s%s_end_s   ) .GT. 0 ) THEN\n",aux,streamtype,streamno,aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"      CALL WRFU_TimeIntervalSet( end_time , D=%s%s%s_end_d, &\n",aux,streamtype,streamno) ;
    fprintf(fp,"                                     H=%s%s%s_end_h, M=%s%s%s_end_m, S=%s%s%s_end_s, rc=rc )\n",aux,streamtype,streamno,aux,streamtype,streamno,aux,streamtype,streamno) ;
    fprintf(fp,"      CALL wrf_check_error( WRFU_SUCCESS, rc, &\n") ;
    fprintf(fp,"                            'WRFU_TimeIntervalSet(%s%s%s_end) FAILED', &\n",aux,streamtype,streamno) ;
    fprintf(fp,"                            __FILE__ , &\n") ;
    fprintf(fp,"                            __LINE__  )\n") ;
    fprintf(fp,"   ELSE\n") ;
    fprintf(fp,"      end_time = run_length + padding_interval\n") ;
    fprintf(fp,"   ENDIF\n") ;
    fprintf(fp,"   CALL domain_alarm_create( grid, %s%s%s_ALARM, interval, begin_time, end_time )\n",aux,streamtype,streamno) ;
#if 0
    fprintf(fp,"   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN\n") ;
#else
    fprintf(fp,"   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN\n") ;
#endif
    fprintf(fp,"     CALL WRFU_AlarmRingerOn( grid%%alarms( %s%s%s_ALARM ),  rc=rc )\n",aux,streamtype,streamno) ;
    fprintf(fp,"     CALL wrf_check_error( WRFU_SUCCESS, rc, &\n") ;
    fprintf(fp,"                           'WRFU_AlarmRingerOn(%s%s%s_ALARM) FAILED', &\n",aux,streamtype,streamno) ;
    fprintf(fp,"                           __FILE__ , &\n") ;
    fprintf(fp,"                           __LINE__  )\n") ;
    fprintf(fp,"   ENDIF\n") ;
  }
  return 0; /* SamT: bug fix: return a value */
}

int
gen_io_form_for_dataset ( FILE *fp )
{
  char * aux , *streamtype , streamno[5]  ;
  int i ;

  fprintf(fp,"    IF      ( DataSet .eq. 'RESTART' ) THEN\n") ;
  fprintf(fp,"      CALL nl_get_io_form_restart( 1, io_form )\n") ;
  fprintf(fp,"    ELSE IF ( DataSet .eq. 'INPUT' ) THEN\n") ;
  fprintf(fp,"      CALL nl_get_io_form_input( 1, io_form )\n") ;
  fprintf(fp,"    ELSE IF ( DataSet .eq. 'HISTORY' ) THEN\n") ;
  fprintf(fp,"      CALL nl_get_io_form_history( 1, io_form )\n") ;
  fprintf(fp,"    ELSE IF ( DataSet .eq. 'BOUNDARY' ) THEN\n") ;
  fprintf(fp,"      CALL nl_get_io_form_boundary( 1, io_form )\n") ;
  for ( i = 1 ; i < MAX_HISTORY ; i++ )
  {
    sprintf(streamno,"%d",i) ;
    fprintf(fp,"    ELSE IF ( DataSet .eq. 'AUXINPUT%s' ) THEN\n",     streamno) ;
    fprintf(fp,"      CALL nl_get_io_form_auxinput%s( 1, io_form )\n", streamno) ;
    fprintf(fp,"    ELSE IF ( DataSet .eq. 'AUXHIST%s' ) THEN\n",      streamno) ;
    fprintf(fp,"      CALL nl_get_io_form_auxhist%s( 1, io_form )\n", streamno) ;
  }
  fprintf(fp,"    ELSE  ! default if nothing is set in SysDepInfo; use history\n") ;
  fprintf(fp,"      CALL nl_get_io_form_history( 1, io_form )\n") ;
  fprintf(fp,"    ENDIF\n") ;
  return 0; /* SamT: bug fix: return a value */
}

int
gen_io_form_for_stream ( FILE *fp )
{
  char * aux , *streamtype , streamno[5]  ;
  int i ;

  fprintf(fp,"    IF      ( stream .eq. restart_only ) THEN\n") ;
  fprintf(fp,"      CALL nl_get_io_form_restart( 1, io_form )\n") ;
  fprintf(fp,"    ELSE IF ( stream .eq. input_only ) THEN\n") ;
  fprintf(fp,"      CALL nl_get_io_form_input( 1, io_form )\n") ;
  fprintf(fp,"    ELSE IF ( stream .eq. history_only ) THEN\n") ;
  fprintf(fp,"      CALL nl_get_io_form_history( 1, io_form )\n") ;
  fprintf(fp,"    ELSE IF ( stream .eq. boundary_only ) THEN\n") ;
  fprintf(fp,"      CALL nl_get_io_form_boundary( 1, io_form )\n") ;
  for ( i = 1 ; i < MAX_HISTORY ; i++ )
  {
    sprintf(streamno,"%d",i) ;
    fprintf(fp,"    ELSE IF ( stream .eq. auxinput%s_only ) THEN\n",     streamno) ;
    fprintf(fp,"      CALL nl_get_io_form_auxinput%s( 1, io_form )\n", streamno) ;
    fprintf(fp,"    ELSE IF ( stream .eq. auxhist%s_only ) THEN\n",      streamno) ;
    fprintf(fp,"      CALL nl_get_io_form_auxhist%s( 1, io_form )\n", streamno) ;
  }
  fprintf(fp,"    ELSE  ! if no match then do the old service representative schtick\n") ;
  fprintf(fp,"      CALL wrf_error_fatal('internal error: please contact wrfhelp@ucar.edu: io_form_for_stream.inc -- invalid stream number')\n") ;
  fprintf(fp,"    ENDIF\n") ;
  return 0; /* SamT: bug fix: return a value */
}

int
gen_switches_and_alarms ( FILE *fp )
{
  char * aux , *streamtype , streamno[5]  ;
  int i ;

  fprintf(fp,"INTEGER, PARAMETER :: history_only      = 1\n") ;
  fprintf(fp,"INTEGER, PARAMETER :: HISTORY_ALARM     = history_only\n") ;
  fprintf(fp,"INTEGER, PARAMETER :: input_only        = %d\n",MAX_HISTORY+1) ;
  fprintf(fp,"INTEGER, PARAMETER :: INPUT_ALARM       = input_only         ! not used\n") ;
  for ( i = 1 ; i < MAX_HISTORY ; i++ )
  {
    fprintf(fp,"INTEGER, PARAMETER :: auxhist%d_only     = %d\n",i,i+1) ;
    fprintf(fp,"INTEGER, PARAMETER :: AUXHIST%d_ALARM    = %d\n",i,i+1) ;
    fprintf(fp,"INTEGER, PARAMETER :: auxinput%d_only     = %d\n",i,MAX_HISTORY+i+1) ;
    fprintf(fp,"INTEGER, PARAMETER :: AUXINPUT%d_ALARM    = %d\n",i,MAX_HISTORY+i+1) ;
  }
  return 0; /* SamT: bug fix: return a value */
}

int
gen_check_auxstream_alarms ( FILE *fp )
{
  char * aux , *streamtype , streamno[5]  ;
  int i ;

  fprintf(fp,"! - AUX HISTORY OUTPUT\n") ;
  for ( i = 1 ; i < MAX_HISTORY ; i++ )
  {
    fprintf(fp,"#ifndef  DISABLE_ALARM_AUXHIST%d\n",i) ;
    fprintf(fp,"   IF( WRFU_AlarmIsRinging( grid%%alarms( AUXHIST%d_ALARM ), rc=rc ) ) THEN\n",i) ;
    fprintf(fp,"     CALL med_hist_out ( grid , %d, config_flags )\n",i) ;
    fprintf(fp,"     CALL WRFU_AlarmRingerOff( grid%%alarms( AUXHIST%d_ALARM ), rc=rc )\n",i) ;
    fprintf(fp,"   ENDIF\n") ;
    fprintf(fp,"#endif\n") ;
  }
  fprintf(fp,"! - AUX INPUT INPUT\n") ;
  for ( i = 1 ; i < MAX_HISTORY ; i++ )
  {
    fprintf(fp,"#ifndef  DISABLE_ALARM_AUXINPUT%d\n",i) ;
    fprintf(fp,"   IF( WRFU_AlarmIsRinging( grid%%alarms( AUXINPUT%d_ALARM ), rc=rc ) ) THEN\n",i) ;
    fprintf(fp,"     CALL med_auxinput%d_in ( grid , config_flags )\n",i) ;
    fprintf(fp,"     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &\n") ;
    fprintf(fp,"        TRIM(config_flags%%auxinput%d_inname) , ' for domain ',grid%%id\n",i) ;
    fprintf(fp,"     CALL wrf_debug ( 0 , message )\n") ;
    fprintf(fp,"     CALL WRFU_AlarmRingerOff( grid%%alarms( AUXINPUT%d_ALARM ), rc=rc )\n",i) ;
    fprintf(fp,"   ENDIF\n") ;
    fprintf(fp,"#endif\n") ;
  }
  return 0; /* SamT: bug fix: return a value */
}

int
gen_fine_stream_input ( FILE *fp )
{
  char * aux , *streamtype , streamno[5]  ;
  int i ;
  fprintf(fp,"IF      ( ( grid%%id .EQ. 1 ) .OR. ( config_flags%%fine_input_stream .EQ. 0 ) ) THEN\n") ;
  fprintf(fp,"   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_input' )\n") ;
  fprintf(fp,"   CALL input_input      ( fid ,  grid , config_flags , ierr )\n") ;
  fprintf(fp,"   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_input' )\n") ;
  for ( i = 1 ; i < MAX_HISTORY ; i++ )
  {
    fprintf(fp,"ELSE IF   ( config_flags%%fine_input_stream .EQ. %d ) THEN\n",i) ;
    fprintf(fp,"   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput%d' )\n",i) ;
    fprintf(fp,"   CALL input_auxinput%d ( fid ,   grid , config_flags , ierr )\n",i) ;
    fprintf(fp,"   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput%d' )\n",i) ;
  }
  fprintf(fp,"ELSE\n") ;
  fprintf(fp,"  WRITE( message , '(\"med_initialdata_input: bad fine_input_stream = \",I4)') config_flags%%fine_input_stream\n") ;
  fprintf(fp,"  CALL WRF_ERROR_FATAL ( message )\n") ;
  fprintf(fp,"END IF\n") ;
  return 0; /* SamT: bug fix: return a value */
}

int
gen_med_auxinput_in ( FILE *fp )
{
  char * aux , *streamtype , streamno[5]  ;
  int i ;
  for ( i = 1 ; i < MAX_HISTORY ; i++ )
  {
    fprintf(fp," CASE ( AUXINPUT%d_ALARM )\n",i) ;
    fprintf(fp,"   CALL open_aux_u( grid, config_flags, stream, AUXINPUT%d_ALARM,       &\n",i) ;
    fprintf(fp,"                    config_flags%%auxinput%d_inname, grid%%auxinput%d_oid, &\n",i,i) ;
    fprintf(fp,"                    input_auxinput%d, ierr )\n",i) ;
    fprintf(fp,"   CALL input_auxinput%d ( grid%%auxinput%d_oid, grid , config_flags , ierr )\n",i,i) ;
  }
  return 0; /* SamT: bug fix: return a value */
}

int
gen_med_hist_out_opens ( FILE *fp )
{
  char * aux , *streamtype , streamno[5]  ;
  int i ;
  for ( i = 1 ; i < MAX_HISTORY ; i++ )
  {
    fprintf(fp," CASE ( AUXHIST%d_ALARM )\n",i) ;
    fprintf(fp,"   CALL open_hist_w( grid, config_flags, stream, AUXHIST%d_ALARM,       &\n",i) ;
    fprintf(fp,"                     config_flags%%auxhist%d_outname, grid%%auxhist%d_oid, &\n",i,i) ;
    fprintf(fp,"                     output_auxhist%d, fname, n2, ierr )\n",i) ;
    fprintf(fp,"   CALL output_auxhist%d ( grid%%auxhist%d_oid, grid , config_flags , ierr )\n",i,i) ;
  }
  return 0; /* SamT: bug fix: return a value */
}

int
gen_med_hist_out_closes ( FILE *fp )
{
  char * aux , *streamtype , streamno[5]  ;
  int i ;
  for ( i = 1 ; i < MAX_HISTORY ; i++ )
  {
    fprintf(fp," CASE ( AUXHIST%d_ALARM )\n",i) ;
    fprintf(fp,"     IF ( grid%%nframes(stream) >= config_flags%%frames_per_auxhist%d ) THEN\n",i) ;
    fprintf(fp,"       CALL close_dataset ( grid%%auxhist%d_oid , config_flags , n2 )\n",i) ;
    fprintf(fp,"       grid%%auxhist%d_oid = 0\n",i) ;
    fprintf(fp,"       grid%%nframes(stream) = 0\n") ;
    fprintf(fp,"     ENDIF\n") ; 
  }
  return 0; /* SamT: bug fix: return a value */
}

int
gen_med_auxinput_in_closes ( FILE *fp )
{
  char * aux , *streamtype , streamno[5]  ;
  int i ;
  for ( i = 1 ; i < MAX_HISTORY ; i++ )  /* the number of history is the same as the number of input and MAX_INPUT collides with system definitions */
  {
    fprintf(fp," CASE ( AUXINPUT%d_ALARM )\n",i) ;
    fprintf(fp,"     IF ( grid%%nframes(stream) >= config_flags%%frames_per_auxinput%d ) THEN\n",i) ;
    fprintf(fp,"       CALL close_dataset ( grid%%auxinput%d_oid , config_flags , \"DATASET=AUXINPUT%d\" )\n",i,i) ;
    fprintf(fp,"       grid%%auxinput%d_oid = 0\n",i) ;
    fprintf(fp,"       grid%%nframes(stream) = 0\n") ;
    fprintf(fp,"     ENDIF\n") ;
  }
  return 0; /* SamT: bug fix: return a value */
}

int
gen_med_last_solve_io ( FILE *fp )
{
  char * aux , *streamtype , streamno[5]  ;
  int i ;
  for ( i = 1 ; i < MAX_HISTORY ; i++ )
  {
    fprintf(fp," IF( WRFU_AlarmIsRinging( grid%%alarms( AUXHIST%d_ALARM ), rc=rc ) ) THEN\n",i) ;
    fprintf(fp,"   CALL med_hist_out ( grid , AUXHIST%d_ALARM , config_flags )\n",i) ;
    fprintf(fp," ENDIF\n") ;
  }
  return 0; /* SamT: bug fix: return a value */
}

int
gen_shutdown_closes ( FILE *fp )
{
  char * aux , *streamtype , streamno[5]  ;
  int i ;
  for ( i = 1 ; i < MAX_HISTORY ; i++ )
  {
    fprintf(fp,"IF( grid%%auxhist%d_oid > 0 ) CALL close_dataset ( grid%%auxhist%d_oid, config_flags, 'DATASET=AUXHIST%d' )\n",i,i,i)  ;
  }
  return 0; /* SamT: bug fix: return a value */
}

/* generate the calls that main/wrf_ESMFMod.F uses in wrf_state_populate() */
gen_med_open_esmf_calls ( FILE *fp )
{
  int i ;
  for ( i = 1 ; i < MAX_HISTORY ; i++ )
  {
     fprintf(fp,"CALL nl_get_io_form_auxinput%d( 1, io_form )\n",i) ;
     fprintf(fp,"IF ( use_package( io_form ) == IO_ESMF ) THEN\n") ;
     fprintf(fp,"  stream = first_auxinput + %d\n",i-1) ;
     fprintf(fp,"  CALL open_aux_u( grid, config_flags, stream, AUXINPUT%d_ALARM,       &\n",i) ;
     fprintf(fp,"                   config_flags%%auxinput%d_inname, grid%%auxinput%d_oid, &\n",i,i) ;
     fprintf(fp,"                   input_auxinput%d, ierr )\n",i) ;
     fprintf(fp,"  IF ( ierr /= 0 ) RETURN\n") ;
     fprintf(fp,"ENDIF\n") ;
  }

  for ( i = 1 ; i < MAX_HISTORY ; i++ )
  {
     fprintf(fp,"CALL nl_get_io_form_auxhist%d( 1, io_form )\n",i) ;
     fprintf(fp,"IF ( use_package( io_form ) == IO_ESMF ) THEN\n") ;
     fprintf(fp,"  stream = first_auxhist + %d\n",i-1) ;
     fprintf(fp,"  CALL open_hist_w( grid, config_flags, stream, AUXHIST%d_ALARM,       &\n",i) ;
     fprintf(fp,"                    config_flags%%auxhist%d_outname, grid%%auxhist%d_oid, &\n",i,i) ;
     fprintf(fp,"                    output_auxhist%d, fname, n2, ierr )\n",i) ;
     fprintf(fp,"  IF ( ierr /= 0 ) RETURN\n") ;
     fprintf(fp,"ENDIF\n") ;
  }
  return 0; /* SamT: bug fix: return a value */
}

/* generate the calls that main/wrf_ESMFMod.F uses in wrf_state_populate() */
gen_med_find_esmf_coupling ( FILE *fp )
{
  int i ;
  for ( i = 1 ; i < MAX_HISTORY ; i++ )
  {
     fprintf(fp,"IF ( .NOT. foundcoupling ) THEN\n") ;
     fprintf(fp,"  CALL nl_get_io_form_auxinput%d( 1, io_form )\n",i) ;
     fprintf(fp,"  IF ( use_package( io_form ) == IO_ESMF ) THEN\n") ;
     fprintf(fp,"    CALL ESMF_AlarmGet( head_grid%%alarms( AUXINPUT%d_ALARM ), &\n",i) ;
     fprintf(fp,"                        RingInterval=couplingInterval, rc=rc )\n") ;
     fprintf(fp,"    IF ( rc /= ESMF_SUCCESS ) THEN\n") ;
     fprintf(fp,"      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT%d_ALARM) failed' )\n",i) ;
     fprintf(fp,"    ENDIF\n") ;
     fprintf(fp,"    foundcoupling = .TRUE.\n") ;
     fprintf(fp,"  ENDIF\n") ;
     fprintf(fp,"ENDIF\n") ;
     fprintf(fp,"IF ( .NOT. foundcoupling ) THEN\n") ;
     fprintf(fp,"  CALL nl_get_io_form_auxhist%d( 1, io_form )\n",i) ;
     fprintf(fp,"  IF ( use_package( io_form ) == IO_ESMF ) THEN\n") ;
     fprintf(fp,"    CALL ESMF_AlarmGet( head_grid%%alarms( AUXHIST%d_ALARM ), &\n",i) ;
     fprintf(fp,"                        RingInterval=couplingInterval, rc=rc )\n") ;
     fprintf(fp,"    IF ( rc /= ESMF_SUCCESS ) THEN\n") ;
     fprintf(fp,"      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST%d_ALARM) failed' )\n",i) ;
     fprintf(fp,"    ENDIF\n") ;
     fprintf(fp,"    foundcoupling = .TRUE.\n") ;
     fprintf(fp,"  ENDIF\n") ;
     fprintf(fp,"ENDIF\n") ;
  }
  return 0; /* SamT: bug fix: return a value */
}


/* 
   This one is special; it gets called before the registry actually runs and produces a file
   that defines a lot of per-stream variables, mostly rconfig but also the oid state variables
   for each stream.  This file is then included by the registry.io_boilerplate file when the
   registry actually runs.  As with the other mods above, this allows a variable, compile-time
   number of io streams. Note that this one is self contained and dirname is hard-coded.
*/
int
gen_io_boilerplate ()
{
  FILE * fp ;
  char * dirname = "Registry" ;
  char  fname[NAMELEN] ;
  char * fn ;
  char * aux , *streamtype , streamno[5]  ;
  char * howset = "namelist,time_control" ;
  char * maxd   = "max_domains" ;
  int i, j ;

  fn = "io_boilerplate_temporary.inc" ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;

  fprintf(fp,"rconfig logical override_restart_timers namelist,time_control 1 .false.\n") ;
  for ( j = 0 ; j < 2 ; j++ ) {  /* j=0 is hist, j=1 is input */
    streamtype = (j==0)?"hist":"input" ;
    for ( i = 1 ; i < MAX_HISTORY ; i++ )
    {
      fprintf(fp,"state integer aux%s%d_oid         - - - - - \"\" \"\" \"\"\n",streamtype,i) ;
      fprintf(fp,"rconfig character aux%s%d_inname %s %s \"aux%s%d_d<domain>_<date>\"\n",streamtype,i,howset,"1",streamtype,i) ;
      fprintf(fp,"rconfig character aux%s%d_outname %s %s \"aux%s%d_d<domain>_<date>\"\n",streamtype,i,howset,"1",streamtype,i) ;
      fprintf(fp,"rconfig integer aux%s%d_interval_y %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer aux%s%d_interval_d %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer aux%s%d_interval_h %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer aux%s%d_interval_m %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer aux%s%d_interval_s %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer aux%s%d_interval   %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer aux%s%d_begin_y %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer aux%s%d_begin_d %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer aux%s%d_begin_h %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer aux%s%d_begin_m %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer aux%s%d_begin_s %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer aux%s%d_begin   %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer aux%s%d_end_y %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer aux%s%d_end_d %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer aux%s%d_end_h %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer aux%s%d_end_m %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer aux%s%d_end_s %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer aux%s%d_end   %s %s 0\n",streamtype,i,howset,maxd) ;
      fprintf(fp,"rconfig integer io_form_aux%s%d %s %s 0\n",streamtype,i,howset,"1") ;
      fprintf(fp,"rconfig integer frames_per_aux%s%d %s %s 999999\n",streamtype,i,howset,maxd) ;
    }
  }

  close_the_file( fp ) ;
  return 0; /* SamT: bug fix: return a value */
}



