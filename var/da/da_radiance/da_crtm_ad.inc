#ifdef CRTM
subroutine da_crtm_ad( nsensor, nchan, nprof, Atmosphere,   &
                            Surface,      &
                            RTSolution_AD, &
                            GeometryInfo, &
                            ChannelInfo,  &
                            Atmosphere_AD,   &
                            Surface_AD,   &
                            RTSolution,   &
                            Options)

   integer, intent(in)            :: nsensor, nchan, nprof
   type (CRTM_RTSolution_type ),  intent(inout)  :: RTSolution(nchan,nprof)
   type (CRTM_RTSolution_type ),  intent(inout)  :: RTSolution_AD(nchan,nprof)
   type (CRTM_ChannelInfo_type),  intent(in)  :: ChannelInfo(nsensor)
   type( CRTM_Atmosphere_type ),  intent(in)  :: Atmosphere(nprof)
   type( CRTM_Atmosphere_type ),  intent(inout)  :: Atmosphere_AD(nprof)
   type( CRTM_Surface_type ),     intent(in)  :: Surface(nprof)
   type( CRTM_Surface_type ),     intent(inout)  :: Surface_AD(nprof)
   type( CRTM_Geometry_type ),    intent(inout)  :: GeometryInfo(nprof)
   type (CRTM_Options_type),      intent(in)     :: Options(nprof)

   integer :: Error_Status

   if (trace_use) call da_trace_entry("da_crtm_ad")

         Error_Status = CRTM_Adjoint(Atmosphere,   &
                            Surface,      &
                            RTSolution_AD,&
                            GeometryInfo, &
                            ChannelInfo,  &
                            Atmosphere_AD,&
                            Surface_AD,   &
                            RTSolution,   &
                            Options)
         if ( Error_Status /= 0 ) then
              call da_error(__FILE__,__LINE__, &
                 (/"Error in calling CRTM_Adjoint"/))
         end if

   if (trace_use) call da_trace_exit("da_crtm_ad")

end subroutine da_crtm_ad
#endif
