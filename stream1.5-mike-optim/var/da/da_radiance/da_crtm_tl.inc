#ifdef CRTM
subroutine da_crtm_tl(  nsensor, nchan, nprof, Atmosphere,   &
                            Surface,      &
                            Atmosphere_TL,&
                            Surface_TL,   &
                            GeometryInfo, &
                            ChannelInfo,  &
                            RTSolution,   &
                            RTSolution_TL,&
                            Options)

   integer, intent(in)            :: nsensor, nchan, nprof
   type (CRTM_RTSolution_type ),  intent(inout)  :: RTSolution(nchan,nprof),RTSolution_TL(nchan,nprof)
   type (CRTM_ChannelInfo_type),  intent(in)  :: ChannelInfo(nsensor)
   type( CRTM_Atmosphere_type ),  intent(in)  :: Atmosphere(nprof), Atmosphere_TL(nprof)
   type( CRTM_Surface_type ),     intent(in)  :: Surface(nprof),Surface_TL(nprof)
   type( CRTM_Geometry_type ),    intent(inout)  :: GeometryInfo(nprof)
   type (CRTM_Options_type),      intent(in)     :: Options(nprof)

   integer :: Error_Status

   if (trace_use) call da_trace_entry("da_crtm_tl")

         Error_Status = CRTM_Tangent_Linear(Atmosphere,   &
                            Surface,      &
                            Atmosphere_TL,&
                            Surface_TL,   &
                            GeometryInfo, &
                            ChannelInfo,  &
                            RTSolution,   &
                            RTSolution_TL,&
                            Options) 

         if ( Error_Status /= 0 ) then
              call da_error(__FILE__,__LINE__, &
                 (/"Error in calling CRTM_Tangent_Linear"/))
         end if


   if (trace_use) call da_trace_exit("da_crtm_tl")

end subroutine da_crtm_tl
#endif
