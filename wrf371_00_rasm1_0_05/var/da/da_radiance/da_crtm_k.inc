#ifdef CRTM
subroutine da_crtm_k(  nsensor, nchan, nprof, &
                            Atmosphere,   &
                            Surface,      &
                            RTSolution_K, &
                            GeometryInfo, &
                            ChannelInfo,  &
                            Atmosphere_K,   &
                            Surface_K,   &
                            RTSolution,   &
                            Options)

   integer, intent(in)            :: nsensor, nchan, nprof
   type (CRTM_RTSolution_type ),  intent(inout)  :: RTSolution(nchan,nprof)
   type (CRTM_RTSolution_type ),  intent(inout)  :: RTSolution_K(nchan,nprof)
   type (CRTM_ChannelInfo_type),  intent(in)     :: ChannelInfo(nsensor)
   type( CRTM_Atmosphere_type ),  intent(in)     :: Atmosphere(nprof)
   type( CRTM_Atmosphere_type ),  intent(inout)  :: Atmosphere_K(nchan,nprof)
   type( CRTM_Surface_type ),     intent(in)     :: Surface(nprof)
   type( CRTM_Surface_type ),     intent(inout)  :: Surface_K(nchan,nprof)
   type( CRTM_Geometry_type ),    intent(inout)  :: GeometryInfo(nprof)
   type (CRTM_Options_type),      intent(in)     :: Options(nprof)

   integer :: Error_Status

   if (trace_use) call da_trace_entry("da_crtm_k")

         Error_Status = CRTM_K_Matrix(Atmosphere,   &
                            Surface,      &
                            RTSolution_K,&
                            GeometryInfo, &
                            ChannelInfo,  &
                            Atmosphere_K,&
                            Surface_K,   &
                            RTSolution,   &
                            Options)
         if ( Error_Status /= 0 ) then
              call da_error(__FILE__,__LINE__, &
                 (/"Error in calling CRTM_K_Matrix"/))
         end if

   if (trace_use) call da_trace_exit("da_crtm_k")

end subroutine da_crtm_k
#endif
