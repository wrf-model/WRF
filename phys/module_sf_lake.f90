MODULE module_sf_lake

















 USE module_wrf_error
 USE module_model_constants, ONLY : rcp

    implicit none 
    integer, parameter ::      r8 = selected_real_kind(12) 

    integer, parameter :: nlevsoil     =  10   
    integer, parameter :: nlevlake     =  10   
    integer, parameter :: nlevsnow     =   5   

    integer,parameter  ::     lbp = 1                        
    integer,parameter  ::     ubp = 1
    integer,parameter  ::     lbc = 1                        
    integer,parameter  ::     ubc = 1
    integer,parameter  ::     num_shlakec       = 1          
    integer,parameter  ::     filter_shlakec(1) = 1          
    integer,parameter  ::     num_shlakep       = 1          
    integer,parameter  ::     filter_shlakep(1) = 1          
    integer,parameter  ::     pcolumn(1)        = 1  
    integer,parameter  ::     pgridcell(1)      = 1  
    integer,parameter  ::     cgridcell(1)      = 1          
    integer,parameter  ::     clandunit(1)      = 1          
  
    integer,parameter  ::     begg = 1
    integer,parameter  ::     endg = 1
    integer,parameter  ::     begl = 1
    integer,parameter  ::     endl = 1
    integer,parameter  ::     begc = 1
    integer,parameter  ::     endc = 1
    integer,parameter  ::     begp = 1
    integer,parameter  ::     endp = 1

    integer,parameter  ::     column    =1
    logical,parameter  ::     lakpoi(1) = .true.
   




    real(r8), parameter :: vkc    = 0.4_r8       
    real(r8), parameter :: pie    = 3.141592653589793_r8 
    real(r8), parameter :: grav   = 9.80616_r8   
    real(r8), parameter :: sb     = 5.67e-8_r8   
    real(r8), parameter :: tfrz   = 273.16_r8    
    real(r8), parameter :: denh2o = 1.000e3_r8   
    real(r8), parameter :: denice = 0.917e3_r8   
    real(r8), parameter :: cpice  = 2.11727e3_r8 
    real(r8), parameter :: cpliq  = 4.188e3_r8   
    real(r8), parameter :: hfus   = 3.337e5_r8   
    real(r8), parameter :: hvap   = 2.501e6_r8   
    real(r8), parameter :: hsub   = 2.501e6_r8+3.337e5_r8 
    real(r8), parameter :: rair   = 287.0423_r8  
    real(r8), parameter :: cpair  = 1.00464e3_r8 
    real(r8), parameter :: tcrit  = 2.5          
    real(r8), parameter :: tkwat  = 0.6          
    real(r8), parameter :: tkice  = 2.290        
    real(r8), parameter :: tkairc = 0.023        
    real(r8), parameter :: bdsno = 250.            
    
    real(r8), public, parameter :: spval = 1.e36  

    real, parameter  ::     depth_c = 50.          

    
   
    real(r8), parameter :: wimp   = 0.05    
    real(r8), parameter :: ssi    = 0.033   
    real(r8), parameter :: cnfac  = 0.5     


   
    integer,parameter :: istsoil = 1  
    integer, private  :: i  
    real(r8) :: dtime                                    

    real(r8) :: zlak(1:nlevlake)     
    real(r8) :: dzlak(1:nlevlake)    
    real(r8) :: zsoi(1:nlevsoil)     
    real(r8) :: dzsoi(1:nlevsoil)    
    real(r8) :: zisoi(0:nlevsoil)    


    real(r8) :: sand(19)                           
    real(r8) :: clay(19)                           

    data(sand(i), i=1,19)/92.,80.,66.,20.,5.,43.,60.,&
      10.,32.,51., 6.,22.,39.7,0.,100.,54.,17.,100.,92./

    data(clay(i), i=1,19)/ 3., 5.,10.,15.,5.,18.,27.,&
      33.,33.,41.,47.,58.,14.7,0., 0., 8.5,54.,  0., 3./


  
    real(r8) :: watsat(1,nlevsoil)      
    real(r8) :: tksatu(1,nlevsoil)      
    real(r8) :: tkmg(1,nlevsoil)        
    real(r8) :: tkdry(1,nlevsoil)       
    real(r8) :: csol(1,nlevsoil)        
    CONTAINS
 

    SUBROUTINE Lake( t_phy        ,p8w            ,dz8w         ,qvcurr          ,&  
                     u_phy        ,v_phy          , glw         ,emiss           ,&
                     rainbl       ,dtbl           ,swdown       ,albedo          ,&
                     xlat_urb2d   ,z_lake3d       ,dz_lake3d    ,lakedepth2d     ,&
                     watsat3d     ,csol3d         ,tkmg3d       ,tkdry3d         ,&
                     tksatu3d     ,ivgtyp         ,ht           ,xland           ,& 
                     iswater, xice, xice_threshold, lake_min_elev                ,&
                     ids          ,ide            ,jds          ,jde             ,&
                     kds          ,kde            ,ims          ,ime             ,&
                     jms          ,jme            ,kms          ,kme             ,&
                     its          ,ite            ,jts          ,jte             ,&
                     kts          ,kte                                           ,&
                     h2osno2d     ,snowdp2d       ,snl2d        ,z3d             ,&  
                     dz3d         ,zi3d           ,h2osoi_vol3d ,h2osoi_liq3d    ,&
                     h2osoi_ice3d ,t_grnd2d       ,t_soisno3d   ,t_lake3d        ,&
                     savedtke12d  ,lake_icefrac3d                                ,& 
             
                     lakemask                                          ,&
                     hfx          ,lh             ,grdflx       ,tsk             ,&  
                     qfx          ,t2             ,th2          ,q2 )





    IMPLICIT NONE
    

    
    INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                     ims,ime, jms,jme, kms,kme,  &
                                     its,ite, jts,jte, kts,kte
    INTEGER , INTENT (IN) :: iswater
    REAL,     INTENT(IN)  :: xice_threshold
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   XICE
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   LAKEMASK
 
    
    REAL,           DIMENSION( ims:ime, kms:kme, jms:jme ),INTENT(IN)  :: t_phy  
    REAL,           DIMENSION( ims:ime, kms:kme, jms:jme ),INTENT(IN)  :: p8w    
    REAL,           DIMENSION( ims:ime, kms:kme, jms:jme ),INTENT(IN)  :: dz8w
    REAL,           DIMENSION( ims:ime, kms:kme, jms:jme ),INTENT(IN)  :: qvcurr
    REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),INTENT(IN)  :: U_PHY
    REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),INTENT(IN)  :: V_PHY
    REAL,           DIMENSION( ims:ime, jms:jme )         ,INTENT(IN)  :: glw
    REAL,           DIMENSION( ims:ime, jms:jme )         ,INTENT(IN)  :: emiss
    REAL,           DIMENSION( ims:ime, jms:jme )         ,INTENT(IN)  :: rainbl
    REAL,           DIMENSION( ims:ime, jms:jme )         ,INTENT(IN)  :: swdown
    REAL,           DIMENSION( ims:ime, jms:jme )         ,INTENT(INOUT)  :: albedo
    REAL,           DIMENSION( ims:ime, jms:jme )         ,INTENT(IN)  :: XLAND
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme )         ,INTENT(IN)  :: XLAT_URB2D
    INTEGER,        DIMENSION( ims:ime, jms:jme )         ,INTENT(INOUT)  :: IVGTYP
    REAL,                                                  INTENT(IN)  :: dtbl
    
    REAL,           DIMENSION( ims:ime,1:nlevlake,jms:jme ),INTENT(IN)  :: z_lake3d
    REAL,           DIMENSION( ims:ime,1:nlevlake,jms:jme ),INTENT(IN)  :: dz_lake3d
    REAL,           DIMENSION( ims:ime,1:nlevsoil,jms:jme ),INTENT(IN)  :: watsat3d
    REAL,           DIMENSION( ims:ime,1:nlevsoil,jms:jme ),INTENT(IN)  :: csol3d
    REAL,           DIMENSION( ims:ime,1:nlevsoil,jms:jme ),INTENT(IN)  :: tkmg3d
    REAL,           DIMENSION( ims:ime,1:nlevsoil,jms:jme ),INTENT(IN)  :: tkdry3d
    REAL,           DIMENSION( ims:ime,1:nlevsoil,jms:jme ),INTENT(IN)  :: tksatu3d
    REAL,           DIMENSION( ims:ime, jms:jme )         ,INTENT(IN)  :: lakedepth2d    
    REAL,           DIMENSION( ims:ime, jms:jme )         ,INTENT(IN)  :: ht
    REAL                                                  ,INTENT(IN)  :: lake_min_elev


    REAL,           DIMENSION( ims:ime, jms:jme )         ,INTENT(OUT) :: HFX
    REAL,           DIMENSION( ims:ime, jms:jme )         ,INTENT(OUT) :: LH
    REAL,           DIMENSION( ims:ime, jms:jme )         ,INTENT(OUT) :: GRDFLX
    REAL,           DIMENSION( ims:ime, jms:jme )         ,INTENT(OUT) :: TSK
    REAL,           DIMENSION( ims:ime, jms:jme )         ,INTENT(OUT) :: QFX   
    REAL,           DIMENSION( ims:ime, jms:jme )         ,INTENT(OUT) :: T2
    REAL,           DIMENSION( ims:ime, jms:jme )         ,INTENT(OUT) :: TH2
    REAL,           DIMENSION( ims:ime, jms:jme )         ,INTENT(OUT) :: Q2



    real,           dimension(ims:ime,jms:jme )                ,intent(inout)  :: savedtke12d 
    real,           dimension(ims:ime,jms:jme )                ,intent(inout)  :: snowdp2d,       &    
                                                                                  h2osno2d,       &    
                                                                                  snl2d,          &    
                                                                                  t_grnd2d
    
    real,    dimension( ims:ime,1:nlevlake, jms:jme )           ,INTENT(inout)  :: t_lake3d,       &    
                                                                                  lake_icefrac3d
    real,    dimension( ims:ime,-nlevsnow+1:nlevsoil, jms:jme )  ,INTENT(inout)  :: t_soisno3d,     &    
                                                                                  h2osoi_ice3d,   &    
                                                                                  h2osoi_liq3d,   &    
                                                                                  h2osoi_vol3d,   &    
                                                                                  z3d,            &    
                                                                                  dz3d 
    real,    dimension( ims:ime,-nlevsnow+0:nlevsoil, jms:jme )  ,INTENT(inout)  :: zi3d    
       



    REAL     :: SFCTMP,PBOT,PSFC,ZLVL,Q2K,EMISSI,LWDN,PRCP,SOLDN,SOLNET
    INTEGER  :: C,i,j,k


      
      real(r8)  :: forc_t(1)          
      real(r8)  :: forc_pbot(1)       
      real(r8)  :: forc_psrf(1)       
      real(r8)  :: forc_hgt(1)        
      real(r8)  :: forc_hgt_q(1)      
      real(r8)  :: forc_hgt_t(1)      
      real(r8)  :: forc_hgt_u(1)      
      real(r8)  :: forc_q(1)          
      real(r8)  :: forc_u(1)          
      real(r8)  :: forc_v(1)          
     
      real(r8)  :: forc_lwrad(1)      
      real(r8)  :: prec(1)               
      real(r8)  :: sabg(1)            
      real(r8)  :: lat(1)             
      real(r8)  :: z_lake(1,nlevlake)  
      real(r8)  :: dz_lake(1,nlevlake)                  

      real(r8)  :: lakedepth(1)       
      logical   :: do_capsnow(1)     

      
      real(r8)  :: h2osoi_vol(1,-nlevsnow+1:nlevsoil)  
      real(r8)  :: t_grnd(1)          
      real(r8)  :: h2osno(1)          
      real(r8)  :: snowdp(1)          
      real(r8)  :: z(1,-nlevsnow+1:nlevsoil)             
      real(r8)  :: dz(1,-nlevsnow+1:nlevsoil)            
      real(r8)  :: t_soisno(1,-nlevsnow+1:nlevsoil)      
      real(r8)  :: t_lake(1,nlevlake)                   
      integer   :: snl(1)                              
      real(r8)  :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)    
      real(r8)  :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)    
      real(r8)  :: savedtke1(1)       
      real(r8)  :: zi(1,-nlevsnow+0:nlevsoil)            
      real(r8)  :: lake_icefrac(1,nlevlake)  


      
      real(r8)  :: eflx_gnet(1)       
      real(r8)  :: eflx_lwrad_net(1)  
      real(r8)  :: eflx_sh_tot(1)     
      real(r8)  :: eflx_lh_tot(1)     
      real(r8)  :: t_ref2m(1)         
      real(r8)  :: q_ref2m(1)         
      real(r8)  :: taux(1)            
      real(r8)  :: tauy(1)            
      real(r8)  :: ram1(1)            
                                               
                                               
      real(r8)  :: z0mg(1)            


      dtime = dtbl

        DO J = jts,jte
        DO I = its,ite

           SFCTMP  = t_phy(i,1,j)
           PBOT    = p8w(i,2,j)
           PSFC    = P8w(i,1,j) 
           ZLVL    = 0.5 * dz8w(i,1,j) 
           Q2K     = qvcurr(i,1,j)/(1.0 + qvcurr(i,1,j))
           EMISSI  = EMISS(I,J) 
           LWDN    = GLW(I,J)*EMISSI 
           PRCP    = RAINBL(i,j)/dtbl
           SOLDN   = SWDOWN(I,J)                        
           SOLNET  = SOLDN*(1.-ALBEDO(I,J))             
                                                        


       
       
       
       
       

        if (lakemask(i,j).eq.1) THEN
    
           do c = 1,column
     
            forc_t(c)          = SFCTMP           
            forc_pbot(c)       = PBOT 
            forc_psrf(c)       = PSFC
            forc_hgt(c)        = ZLVL             
            forc_hgt_q(c)      = ZLVL             
            forc_hgt_t(c)      = ZLVL             
            forc_hgt_u(c)      = ZLVL             
            forc_q(c)          = Q2K              
            forc_u(c)          = U_PHY(I,1,J)
            forc_v(c)          = V_PHY(I,1,J)
           
            forc_lwrad(c)      = LWDN             
            prec(c)            = PRCP             
            sabg(c)            = SOLNET
            lat(c)             = XLAT_URB2D(I,J)*pie/180  
            do_capsnow(c)      = .false.

            lakedepth(c)           = lakedepth2d(i,j)
            savedtke1(c)           = savedtke12d(i,j)
            snowdp(c)              = snowdp2d(i,j)
            h2osno(c)              = h2osno2d(i,j)
            snl(c)                 = snl2d(i,j)
            t_grnd(c)              = t_grnd2d(i,j)
            do k = 1,nlevlake
               t_lake(c,k)        = t_lake3d(i,k,j)
               lake_icefrac(c,k)  = lake_icefrac3d(i,k,j)
               z_lake(c,k)        = z_lake3d(i,k,j)
               dz_lake(c,k)       = dz_lake3d(i,k,j)
            enddo
            do k = -nlevsnow+1,nlevsoil
               t_soisno(c,k)      = t_soisno3d(i,k,j)
	       h2osoi_ice(c,k)    = h2osoi_ice3d(i,k,j)
               h2osoi_liq(c,k)    = h2osoi_liq3d(i,k,j)
               h2osoi_vol(c,k)    = h2osoi_vol3d(i,k,j)
               z(c,k)             = z3d(i,k,j)
               dz(c,k)            = dz3d(i,k,j)
            enddo   
            do k = -nlevsnow+0,nlevsoil
               zi(c,k)            = zi3d(i,k,j)
            enddo
            do k = 1,nlevsoil
               watsat(c,k)        = watsat3d(i,k,j)
               csol(c,k)          = csol3d(i,k,j)
               tkmg(c,k)          = tkmg3d(i,k,j)
               tkdry(c,k)         = tkdry3d(i,k,j)
               tksatu(c,k)        = tksatu3d(i,k,j)
            enddo
            
          enddo
            CALL LakeMain(forc_t,forc_pbot,forc_psrf,forc_hgt,forc_hgt_q,   & 
                          forc_hgt_t,forc_hgt_u,forc_q, forc_u,         &
                          forc_v,forc_lwrad,prec, sabg,lat,             &
                          z_lake,dz_lake,lakedepth,do_capsnow,          &
                          h2osno,snowdp,snl,z,dz,zi,                    & 
                          h2osoi_vol,h2osoi_liq,h2osoi_ice,             &
                          t_grnd,t_soisno,t_lake,                       &
                          savedtke1,lake_icefrac,                       &
                          eflx_lwrad_net,eflx_gnet,                     & 
                          eflx_sh_tot,eflx_lh_tot,                      &
                          t_ref2m,q_ref2m,                              &
                          taux,tauy,ram1,z0mg)


           do c = 1,column
            HFX(I,J)          = eflx_sh_tot(c)            
            LH(I,J)           = eflx_lh_tot(c)            
            GRDFLX(I,J)       = eflx_gnet(c)              
            TSK(I,J)          = t_grnd(c)                 
            T2(I,J)           = t_ref2m(c)
            TH2(I,J)          = T2(I,J)*(1.E5/PSFC)**RCP
            Q2(I,J)           = q_ref2m(c) 
            albedo(i,j)       = ( 0.6 * lake_icefrac(c,1) ) + ( (1.0-lake_icefrac(c,1)) * 0.08)  

            if( tsk(i,j) >= tfrz ) then
                qfx(i,j)      = eflx_lh_tot(c)/hvap
            else
                qfx(i,j)      = eflx_lh_tot(c)/hsub       
            endif
           enddo


           do c = 1,column

            savedtke12d(i,j)         = savedtke1(c)
            snowdp2d(i,j)            = snowdp(c)
            h2osno2d(i,j)            = h2osno(c)
	    snl2d(i,j)               = snl(c)
            t_grnd2d(i,j)            = t_grnd(c)
            do k = 1,nlevlake
               t_lake3d(i,k,j)       = t_lake(c,k)
	       lake_icefrac3d(i,k,j) = lake_icefrac(c,k)
            enddo
	    do k = -nlevsnow+1,nlevsoil
	       z3d(i,k,j)            = z(c,k)
	       dz3d(i,k,j)           = dz(c,k) 
	       t_soisno3d(i,k,j)     = t_soisno(c,k)
	       h2osoi_liq3d(i,k,j)   = h2osoi_liq(c,k)
	       h2osoi_ice3d(i,k,j)   = h2osoi_ice(c,k)
               h2osoi_vol3d(i,k,j)   = h2osoi_vol(c,k)
	   enddo
           do k = -nlevsnow+0,nlevsoil
               zi3d(i,k,j)           = zi(c,k)
           enddo
        
         enddo

        endif

        ENDDO
        ENDDO

    END SUBROUTINE Lake


    SUBROUTINE LakeMain(forc_t,forc_pbot,forc_psrf,forc_hgt,forc_hgt_q,     & 
                          forc_hgt_t,forc_hgt_u,forc_q, forc_u,         &   
                          forc_v,forc_lwrad,prec, sabg,lat,             &   
                          z_lake,dz_lake,lakedepth,do_capsnow,          &
                          h2osno,snowdp,snl,z,dz,zi,                    & 
                          h2osoi_vol,h2osoi_liq,h2osoi_ice,             &
                          t_grnd,t_soisno,t_lake,                       &  
                          savedtke1,lake_icefrac,                       &
                          eflx_lwrad_net,eflx_gnet,                     & 
                          eflx_sh_tot,eflx_lh_tot,                      &
                          t_ref2m,q_ref2m,                              &
                          taux,tauy,ram1,z0mg)
    implicit none


    real(r8),intent(in) :: forc_t(1)          
    real(r8),intent(in) :: forc_pbot(1)       
    real(r8),intent(in) :: forc_psrf(1)       
    real(r8),intent(in) :: forc_hgt(1)        
    real(r8),intent(in) :: forc_hgt_q(1)      
    real(r8),intent(in) :: forc_hgt_t(1)      
    real(r8),intent(in) :: forc_hgt_u(1)      
    real(r8),intent(in) :: forc_q(1)          
    real(r8),intent(in) :: forc_u(1)          
    real(r8),intent(in) :: forc_v(1)          
   
    real(r8),intent(in) :: forc_lwrad(1)      
    real(r8),intent(in) :: prec(1)               
    real(r8),intent(in) :: sabg(1)            
    real(r8),intent(in) :: lat(1)             
    real(r8),intent(in) :: z_lake(1,nlevlake)  
    real(r8),intent(in) :: dz_lake(1,nlevlake)                  

    real(r8), intent(in) :: lakedepth(1)       
    
   
    
    logical , intent(in) :: do_capsnow(1)     
   



    real(r8),intent(inout) :: h2osoi_vol(1,-nlevsnow+1:nlevsoil)  
    real(r8),intent(inout) :: t_grnd(1)          
    real(r8),intent(inout) :: h2osno(1)          
    real(r8),intent(inout) :: snowdp(1)          
    real(r8),intent(inout) :: z(1,-nlevsnow+1:nlevsoil)             
    real(r8),intent(inout) :: dz(1,-nlevsnow+1:nlevsoil)            
    real(r8),intent(inout) :: t_soisno(1,-nlevsnow+1:nlevsoil)      
    real(r8),intent(inout) :: t_lake(1,nlevlake)                   
    integer ,intent(inout) :: snl(1)                              
    real(r8),intent(inout) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)    
    real(r8),intent(inout) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)    
    real(r8),intent(inout) :: savedtke1(1)       
    real(r8),intent(inout) :: zi(1,-nlevsnow+0:nlevsoil)            
    real(r8),intent(inout) :: lake_icefrac(1,nlevlake)  



    real(r8),intent(out) :: eflx_gnet(1)       
    real(r8),intent(out) :: eflx_lwrad_net(1)  
    real(r8),intent(out) :: eflx_sh_tot(1)     
    real(r8),intent(out) :: eflx_lh_tot(1)     
    real(r8),intent(out) :: t_ref2m(1)         
    real(r8),intent(out) :: q_ref2m(1)         
    real(r8),intent(out) :: taux(1)            
    real(r8),intent(out) :: tauy(1)            
    real(r8),intent(out) :: ram1(1)            
                                               
                                               
    real(r8),intent(out) :: z0mg(1)            



    
    real(r8) :: begwb(1)           
    real(r8) :: t_veg(1)           
    real(r8) :: eflx_soil_grnd(1)  
    real(r8) :: eflx_lh_grnd(1)    
    real(r8) :: eflx_sh_grnd(1)    
    real(r8) :: eflx_lwrad_out(1)  
    real(r8) :: qflx_evap_tot(1)   
    real(r8) :: qflx_evap_soi(1)   
    real(r8) :: qflx_prec_grnd(1)  
    real(r8) :: forc_snow(1)       
    real(r8) :: forc_rain(1)       
    real(r8) :: ws(1)              
    real(r8) :: ks(1)              
    real(r8) :: qflx_snomelt(1)    
    integer  :: imelt(1,-nlevsnow+1:nlevsoil)      
    real(r8) :: endwb(1)         
    real(r8) :: snowage(1)       
    real(r8) :: snowice(1)       
    real(r8) :: snowliq(1)       
    real(r8) :: t_snow(1)        
    real(r8) :: qflx_drain(1)    
    real(r8) :: qflx_surf(1)     
    real(r8) :: qflx_infl(1)     
    real(r8) :: qflx_qrgwl(1)    
    real(r8) :: qcharge(1)       
    real(r8) :: qflx_snowcap(1)       
    real(r8) :: qflx_snowcap_col(1)   
    real(r8) :: qflx_snow_grnd_pft(1) 
    real(r8) :: qflx_snow_grnd_col(1) 
    real(r8) :: qflx_rain_grnd(1)     
    real(r8) :: frac_iceold(1,-nlevsnow+1:nlevsoil)      
    real(r8) :: qflx_evap_tot_col(1) 
    real(r8) :: soilalpha(1)     
    real(r8) :: zwt(1)           
    real(r8) :: fcov(1)          
    real(r8) :: rootr_column(1,1:nlevsoil) 
    real(r8) :: qflx_evap_grnd(1)  
    real(r8) :: qflx_sub_snow(1)   
    real(r8) :: qflx_dew_snow(1)   
    real(r8) :: qflx_dew_grnd(1)   
    real(r8) :: qflx_rain_grnd_col(1)   
    



    if (prec(1)> 0.) then
        if ( forc_t(1) > (tfrz + tcrit)) then
            forc_rain(1) = prec(1)
            forc_snow(1) = 0.
          
         else
            forc_rain(1) = 0.
            forc_snow(1) = prec(1)

          
          
          
          
          
          
         endif
    else
         forc_rain(1) = 0.
         forc_snow(1) = 0.
       
    endif

    CALL ShalLakeFluxes(forc_t,forc_pbot,forc_psrf,forc_hgt,forc_hgt_q,   &  
                          forc_hgt_t,forc_hgt_u,forc_q,                   &
                          forc_u,forc_v,forc_lwrad,forc_snow,             &
                          forc_rain,t_grnd,h2osno,snowdp,sabg,lat,        &
                          dz,dz_lake,t_soisno,t_lake,snl,h2osoi_liq,      &
                          h2osoi_ice,savedtke1,                           &
                          qflx_prec_grnd,qflx_evap_soi,qflx_evap_tot,     &  
                          eflx_sh_grnd,eflx_lwrad_out,eflx_lwrad_net,     &
                          eflx_soil_grnd,eflx_sh_tot,eflx_lh_tot,         &
                          eflx_lh_grnd,t_veg,t_ref2m,q_ref2m,taux,tauy,   &
                          ram1,ws,ks,eflx_gnet,z0mg)
 

    CALL ShalLakeTemperature(t_grnd,h2osno,sabg,dz,dz_lake,z,zi,             & 
                                 z_lake,ws,ks,snl,eflx_gnet,lakedepth,       &
                                 lake_icefrac,snowdp,                        & 
                                 eflx_sh_grnd,eflx_sh_tot,eflx_soil_grnd,    & 
                                 t_lake,t_soisno,h2osoi_liq,                 &
                                 h2osoi_ice,savedtke1,                       &
                                 frac_iceold,qflx_snomelt,imelt)



    CALL ShalLakeHydrology(dz_lake,forc_rain,forc_snow,                          & 
                               begwb,qflx_evap_tot,forc_t,do_capsnow,            &
                               t_grnd,qflx_evap_soi,                             &
                               qflx_snomelt,imelt,frac_iceold,                   & 
                               z,dz,zi,snl,h2osno,snowdp,lake_icefrac,t_lake,      & 
                               endwb,snowage,snowice,snowliq,t_snow,             & 
                               t_soisno,h2osoi_ice,h2osoi_liq,h2osoi_vol,        &
                               qflx_drain,qflx_surf,qflx_infl,qflx_qrgwl,        &
                               qcharge,qflx_prec_grnd,qflx_snowcap,              &
                               qflx_snowcap_col,qflx_snow_grnd_pft,              &
                               qflx_snow_grnd_col,qflx_rain_grnd,                &
                               qflx_evap_tot_col,soilalpha,zwt,fcov,             &
                               rootr_column,qflx_evap_grnd,qflx_sub_snow,        &
                               qflx_dew_snow,qflx_dew_grnd,qflx_rain_grnd_col)
                       




                       
   END SUBROUTINE LakeMain


SUBROUTINE ShalLakeFluxes(forc_t,forc_pbot,forc_psrf,forc_hgt,forc_hgt_q,           &  
                          forc_hgt_t,forc_hgt_u,forc_q,                   &
                          forc_u,forc_v,forc_lwrad,forc_snow,             &
                          forc_rain,t_grnd,h2osno,snowdp,sabg,lat,        &
                          dz,dz_lake,t_soisno,t_lake,snl,h2osoi_liq,      &
                          h2osoi_ice,savedtke1,                           &
                          qflx_prec_grnd,qflx_evap_soi,qflx_evap_tot,     &  
                          eflx_sh_grnd,eflx_lwrad_out,eflx_lwrad_net,     &
                          eflx_soil_grnd,eflx_sh_tot,eflx_lh_tot,         &
                          eflx_lh_grnd,t_veg,t_ref2m,q_ref2m,taux,tauy,   &
                          ram1,ws,ks,eflx_gnet,z0mg)            














   
 
    implicit none



    real(r8),intent(in) :: forc_t(1)          
    real(r8),intent(in) :: forc_pbot(1)       
    real(r8),intent(in) :: forc_psrf(1)       
    real(r8),intent(in) :: forc_hgt(1)        
    real(r8),intent(in) :: forc_hgt_q(1)      
    real(r8),intent(in) :: forc_hgt_t(1)      
    real(r8),intent(in) :: forc_hgt_u(1)      
    real(r8),intent(in) :: forc_q(1)          
    real(r8),intent(in) :: forc_u(1)          
    real(r8),intent(in) :: forc_v(1)          
    real(r8),intent(in) :: forc_lwrad(1)      
   
    real(r8),intent(in) :: forc_snow(1)       
    real(r8),intent(in) :: forc_rain(1)       
    real(r8),intent(in) :: h2osno(1)          
    real(r8),intent(in) :: snowdp(1)          
    real(r8),intent(in) :: sabg(1)            
    real(r8),intent(in) :: lat(1)             
    real(r8),intent(in) :: dz(1,-nlevsnow+1:nlevsoil)            
    real(r8),intent(in) :: dz_lake(1,nlevlake)                  
    real(r8),intent(in) :: t_soisno(1,-nlevsnow+1:nlevsoil)      
    real(r8),intent(in) :: t_lake(1,nlevlake)                   
    integer ,intent(in) :: snl(1)                              
    real(r8),intent(in) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)    
    real(r8),intent(in) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)    
    real(r8),intent(in) :: savedtke1(1)       


    real(r8),intent(inout) :: t_grnd(1)          

    real(r8),intent(out):: qflx_prec_grnd(1)  
    real(r8),intent(out):: qflx_evap_soi(1)   
    real(r8),intent(out):: qflx_evap_tot(1)   
    real(r8),intent(out):: eflx_sh_grnd(1)    
    real(r8),intent(out):: eflx_lwrad_out(1)  
    real(r8),intent(out):: eflx_lwrad_net(1)  
    real(r8),intent(out):: eflx_soil_grnd(1)  
    real(r8),intent(out):: eflx_sh_tot(1)     
    real(r8),intent(out):: eflx_lh_tot(1)     
    real(r8),intent(out):: eflx_lh_grnd(1)    
    real(r8),intent(out):: t_veg(1)           
    real(r8),intent(out):: t_ref2m(1)         
    real(r8),intent(out):: q_ref2m(1)         
    real(r8),intent(out):: taux(1)            
    real(r8),intent(out):: tauy(1)            
    real(r8),intent(out):: ram1(1)            
    real(r8),intent(out):: ws(1)              
    real(r8),intent(out):: ks(1)              
                                               
    real(r8),intent(out):: eflx_gnet(1)       
                                               
    real(r8),intent(out):: z0mg(1)            





    integer , parameter :: islak  = 2       
    integer , parameter :: niters = 3       
    real(r8), parameter :: beta1  = 1._r8   
    real(r8), parameter :: emg    = 0.97_r8 
    real(r8), parameter :: zii    = 1000._r8
    real(r8), parameter :: tdmax  = 277._r8 
    real(r8) :: forc_th(1)         
    real(r8) :: forc_vp(1)         
    real(r8) :: forc_rho(1)        
    integer  :: i,fc,fp,g,c,p           
    integer  :: fncopy                  
    integer  :: fnold                   
    integer  :: fpcopy(num_shlakep)     
    integer  :: iter                    
    integer  :: nmozsgn(lbp:ubp)        
    integer  :: jtop(lbc:ubc)           

    real(r8) :: ax                      
    real(r8) :: bx                      
    real(r8) :: degdT                   
    real(r8) :: dqh(lbp:ubp)            
    real(r8) :: dth(lbp:ubp)            
    real(r8) :: dthv                    
    real(r8) :: dzsur(lbc:ubc)          
    real(r8) :: eg                      
    real(r8) :: htvp(lbc:ubc)           
    real(r8) :: obu(lbp:ubp)            
    real(r8) :: obuold(lbp:ubp)         
    real(r8) :: qsatg(lbc:ubc)          
    real(r8) :: qsatgdT(lbc:ubc)        
    real(r8) :: qstar                   
    real(r8) :: ram(lbp:ubp)            
    real(r8) :: rah(lbp:ubp)            
    real(r8) :: raw(lbp:ubp)            
    real(r8) :: stftg3(lbp:ubp)         
    real(r8) :: temp1(lbp:ubp)          
    real(r8) :: temp12m(lbp:ubp)        
    real(r8) :: temp2(lbp:ubp)          
    real(r8) :: temp22m(lbp:ubp)        
    real(r8) :: tgbef(lbc:ubc)          
    real(r8) :: thm(lbc:ubc)            
    real(r8) :: thv(lbc:ubc)            
    real(r8) :: thvstar                 
    real(r8) :: tksur                   
    real(r8) :: tsur                    
    real(r8) :: tstar                   
    real(r8) :: um(lbp:ubp)             
    real(r8) :: ur(lbp:ubp)             
    real(r8) :: ustar(lbp:ubp)          
    real(r8) :: wc                      
    real(r8) :: zeta                    
    real(r8) :: zldis(lbp:ubp)          
    real(r8) :: displa(lbp:ubp)         

    real(r8) :: z0hg(lbp:ubp)           
    real(r8) :: z0qg(lbp:ubp)           
    real(r8) :: beta(2)                 
    real(r8) :: u2m                     
    real(r8) :: u10(1)         
    real(r8) :: fv(1)          

    real(r8) :: fm(lbp:ubp)             
    real(r8) :: bw                       
    real(r8) :: t_grnd_temp              
    real(r8) :: betaprime(lbc:ubc)       
    character*256 :: message 
      
      



    data beta/0.4_r8, 0.4_r8/  
    









!dir$ concurrent

    forc_th(1)  = forc_t(1) * (forc_psrf(1)/ forc_pbot(1))**(rair/cpair)
    forc_vp(1)  = forc_q(1) * forc_pbot(1)/ (0.622 + 0.378 * forc_q(1))
    forc_rho(1) = (forc_pbot(1) - 0.378 * forc_vp(1)) / (rair * forc_t(1))

    do fc = 1, num_shlakec
       c = filter_shlakec(fc)
       g = cgridcell(c)

       

       
       if (snl(c) > 0 .or. snl(c) < -5) then
         WRITE(message,*)  'snl is not defined in ShalLakeFluxesMod'
         CALL wrf_message(message)
         CALL wrf_error_fatal3("<stdin>",790,&
"snl: out of range value")
       end if




       jtop(c) = snl(c) + 1


       if (snl(c) < 0) then
           betaprime(c) = 1._r8  
           dzsur(c) = dz(c,jtop(c))/2._r8
       else
           betaprime(c) = beta(islak)
           dzsur(c) = dz_lake(c,1)/2._r8
       end if


       
       

       call QSat(t_grnd(c), forc_pbot(g), eg, degdT, qsatg(c), qsatgdT(c))

       
       

       thm(c) = forc_t(g) + 0.0098_r8*forc_hgt_t(g)   
       thv(c) = forc_th(g)*(1._r8+0.61_r8*forc_q(g))     
    end do

!dir$ concurrent

    do fp = 1, num_shlakep
       p = filter_shlakep(fp)
       c = pcolumn(p)
       g = pgridcell(p)

       nmozsgn(p) = 0
       obuold(p) = 0._r8
       displa(p) = 0._r8

       
 


    
    
    
    
    
    
    
 
       if (t_grnd(c) >= tfrz) then   
          z0mg(p) = 0.001_r8        
       else if(snl(c) == 0 ) then                         
       
       
          z0mg(p) = 0.005_r8          
       else                          
          z0mg(p) = 0.0024_r8
       end if
 
 


       z0hg(p) = z0mg(p)
       z0qg(p) = z0mg(p)

       

       if (t_grnd(c) > tfrz) then
          htvp(c) = hvap
       else
          htvp(c) = hsub
       end if



       

       ur(p)    = max(1.0_r8,sqrt(forc_u(g)*forc_u(g)+forc_v(g)*forc_v(g)))
       dth(p)   = thm(c)-t_grnd(c)
       dqh(p)   = forc_q(g)-qsatg(c)
       dthv     = dth(p)*(1._r8+0.61_r8*forc_q(g))+0.61_r8*forc_th(g)*dqh(p)
       zldis(p) = forc_hgt_u(g) - 0._r8

       

       call MoninObukIni(ur(p), thv(c), dthv, zldis(p), z0mg(p), um(p), obu(p))

    end do

    iter = 1
    fncopy = num_shlakep
    fpcopy(1:num_shlakep) = filter_shlakep(1:num_shlakep)

    

    ITERATION : do while (iter <= niters .and. fncopy > 0)

       
       

       call FrictionVelocity(pgridcell,forc_hgt,forc_hgt_u,          & 
                             forc_hgt_t,forc_hgt_q,                  & 
                             lbp, ubp, fncopy, fpcopy,               & 
                             displa, z0mg, z0hg, z0qg,               & 
                             obu, iter, ur, um,                      & 
                             ustar,temp1, temp2, temp12m, temp22m,   & 
                             u10,fv,                                 & 
                             fm)  

!dir$ concurrent

       do fp = 1, fncopy
          p = fpcopy(fp)
          c = pcolumn(p)
          g = pgridcell(p)

          tgbef(c) = t_grnd(c)
          if (t_grnd(c) > tfrz .and. t_lake(c,1) > tfrz .and. snl(c) == 0) then
             tksur = savedtke1(c)
             
             
             
             tsur = t_lake(c,1)
          else if (snl(c) == 0) then  
             tksur = tkice
             tsur = t_lake(c,1)
          else
          
             bw = (h2osoi_ice(c,jtop(c))+h2osoi_liq(c,jtop(c)))/dz(c,jtop(c))
             tksur = tkairc + (7.75e-5_r8 *bw + 1.105e-6_r8*bw*bw)*(tkice-tkairc)
             tsur = t_soisno(c,jtop(c))
          end if

          

          ram(p)  = 1._r8/(ustar(p)*ustar(p)/um(p))
          rah(p)  = 1._r8/(temp1(p)*ustar(p))
          raw(p)  = 1._r8/(temp2(p)*ustar(p))
          ram1(p) = ram(p)   

          

          stftg3(p) = emg*sb*tgbef(c)*tgbef(c)*tgbef(c)

          
          
          ax  = betaprime(c)*sabg(p) + emg*forc_lwrad(g) + 3._r8*stftg3(p)*tgbef(c) &
               + forc_rho(g)*cpair/rah(p)*thm(c) &
               - htvp(c)*forc_rho(g)/raw(p)*(qsatg(c)-qsatgdT(c)*tgbef(c) - forc_q(g)) &
               + tksur*tsur/dzsur(c)
          
          bx  = 4._r8*stftg3(p) + forc_rho(g)*cpair/rah(p) &
               + htvp(c)*forc_rho(g)/raw(p)*qsatgdT(c) + tksur/dzsur(c)

          t_grnd(c) = ax/bx

          
       if (t_grnd(c) > tfrz) then
          htvp(c) = hvap
       else
          htvp(c) = hsub
       end if

          
          

          eflx_sh_grnd(p) = forc_rho(g)*cpair*(t_grnd(c)-thm(c))/rah(p)
          qflx_evap_soi(p) = forc_rho(g)*(qsatg(c)+qsatgdT(c)*(t_grnd(c)-tgbef(c))-forc_q(g))/raw(p)

          
          

          call QSat(t_grnd(c), forc_pbot(g), eg, degdT, qsatg(c), qsatgdT(c))

          dth(p)=thm(c)-t_grnd(c)
          dqh(p)=forc_q(g)-qsatg(c)

          tstar = temp1(p)*dth(p)
          qstar = temp2(p)*dqh(p)

          thvstar=tstar*(1._r8+0.61_r8*forc_q(g)) + 0.61_r8*forc_th(g)*qstar
          zeta=zldis(p)*vkc * grav*thvstar/(ustar(p)**2*thv(c))

          if (zeta >= 0._r8) then     
             zeta = min(2._r8,max(zeta,0.01_r8))
             um(p) = max(ur(p),0.1_r8)
          else                     
             zeta = max(-100._r8,min(zeta,-0.01_r8))
             wc = beta1*(-grav*ustar(p)*thvstar*zii/thv(c))**0.333_r8
             um(p) = sqrt(ur(p)*ur(p)+wc*wc)
          end if
          obu(p) = zldis(p)/zeta

          if (obuold(p)*obu(p) < 0._r8) nmozsgn(p) = nmozsgn(p)+1

          obuold(p) = obu(p)

       end do   

       iter = iter + 1
       if (iter <= niters ) then
          

          fnold = fncopy
          fncopy = 0
          do fp = 1, fnold
             p = fpcopy(fp)
             if (nmozsgn(p) < 3) then
                fncopy = fncopy + 1
                fpcopy(fncopy) = p
             end if
          end do   
       end if

    end do ITERATION   

!dir$ concurrent

    do fp = 1, num_shlakep
       p = filter_shlakep(fp)
       c = pcolumn(p)
       g = pgridcell(p)

       
       
       
       
       
       
       

       
       

       if ( (h2osno(c) > 0.5_r8 .or. t_lake(c,1) <= tfrz) .and. t_grnd(c) > tfrz) then



          t_grnd_temp = t_grnd(c)
          t_grnd(c) = tfrz
          eflx_sh_grnd(p) = forc_rho(g)*cpair*(t_grnd(c)-thm(c))/rah(p)
          qflx_evap_soi(p) = forc_rho(g)*(qsatg(c)+qsatgdT(c)*(t_grnd(c)-t_grnd_temp) - forc_q(g))/raw(p)
       else if ( (t_lake(c,1) > t_grnd(c) .and. t_grnd(c) > tdmax) .or. &
                 (t_lake(c,1) < t_grnd(c) .and. t_lake(c,1) > tfrz .and. t_grnd(c) < tdmax) ) then
                 
          t_grnd_temp = t_grnd(c)
          t_grnd(c) = t_lake(c,1)
          eflx_sh_grnd(p) = forc_rho(g)*cpair*(t_grnd(c)-thm(c))/rah(p)
          qflx_evap_soi(p) = forc_rho(g)*(qsatg(c)+qsatgdT(c)*(t_grnd(c)-t_grnd_temp) - forc_q(g))/raw(p)
       end if

          
       if (t_grnd(c) > tfrz) then
          htvp(c) = hvap
       else
          htvp(c) = hsub
       end if

       



       eflx_lwrad_out(p) = (1._r8-emg)*forc_lwrad(g) + emg*sb*t_grnd(c)**4

       

       eflx_soil_grnd(p) = sabg(p) + forc_lwrad(g) - eflx_lwrad_out(p) - &
            eflx_sh_grnd(p) - htvp(c)*qflx_evap_soi(p)
       

       
       
       

       taux(p) = -forc_rho(g)*forc_u(g)/ram(p)
       tauy(p) = -forc_rho(g)*forc_v(g)/ram(p)

       eflx_sh_tot(p)   = eflx_sh_grnd(p)
       qflx_evap_tot(p) = qflx_evap_soi(p)
       eflx_lh_tot(p)   = htvp(c)*qflx_evap_soi(p)
       eflx_lh_grnd(p)  = htvp(c)*qflx_evap_soi(p)
       
       t_ref2m(p) = thm(c) + temp1(p)*dth(p)*(1._r8/temp12m(p) - 1._r8/temp1(p))

       
       q_ref2m(p) = forc_q(g) + temp2(p)*dqh(p)*(1._r8/temp22m(p) - 1._r8/temp2(p))

       
       

       
       
       
       

       eflx_gnet(p) = betaprime(c) * sabg(p) + forc_lwrad(g) - (eflx_lwrad_out(p) + &
            eflx_sh_tot(p) + eflx_lh_tot(p))
       
       


       
       
       
       u2m = max(0.1_r8,ustar(p)/vkc*log(2._r8/z0mg(p)))

       ws(c) = 1.2e-03_r8 * u2m
       ks(c) = 6.6_r8*sqrt(abs(sin(lat(g))))*(u2m**(-1.84_r8))

    end do


    

    

!dir$ concurrent

    do fp = 1, num_shlakep
       p = filter_shlakep(fp)
       c = pcolumn(p)
       g = pgridcell(p)

        
        
       t_veg(p) = t_grnd(c)
       eflx_lwrad_net(p)  = eflx_lwrad_out(p) - forc_lwrad(g)
       qflx_prec_grnd(p) = forc_rain(g) + forc_snow(g)
    end do

END SUBROUTINE ShalLakeFluxes
 
SUBROUTINE ShalLakeTemperature(t_grnd,h2osno,sabg,dz,dz_lake,z,zi,           & 
                                 z_lake,ws,ks,snl,eflx_gnet,lakedepth,       &
                                 lake_icefrac,snowdp,                        & 
                                 eflx_sh_grnd,eflx_sh_tot,eflx_soil_grnd,    & 
                                 t_lake,t_soisno,h2osoi_liq,                 &
                                 h2osoi_ice,savedtke1,                       &
                                 frac_iceold,qflx_snomelt,imelt)




















































































    
    implicit none


    real(r8), intent(in) :: t_grnd(1)          
    real(r8), intent(inout) :: h2osno(1)          
    real(r8), intent(in) :: sabg(1)            
    real(r8), intent(in) :: dz(1,-nlevsnow + 1:nlevsoil)          
    real(r8), intent(in) :: dz_lake(1,nlevlake)                  
    real(r8), intent(in) :: z(1,-nlevsnow+1:nlevsoil)             
    real(r8), intent(in) :: zi(1,-nlevsnow+0:nlevsoil)            
                                                                
    real(r8), intent(in) :: z_lake(1,nlevlake)  
    real(r8), intent(in) :: ws(1)              
    real(r8), intent(in) :: ks(1)              
                                               
    integer , intent(in) :: snl(1)             
    real(r8), intent(inout) :: eflx_gnet(1)       
    real(r8), intent(in) :: lakedepth(1)       
    
   
    real(r8), intent(inout) :: snowdp(1)        


    real(r8), intent(out) :: eflx_sh_grnd(1)    
    real(r8), intent(out) :: eflx_sh_tot(1)     
    real(r8), intent(out) :: eflx_soil_grnd(1)  
                                               
    real(r8), intent(inout) :: t_lake(1,nlevlake)                 
    real(r8), intent(inout) :: t_soisno(1,-nlevsnow+1:nlevsoil)    
    real(r8), intent(inout) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)  
    real(r8), intent(inout) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)  
    real(r8), intent(inout) :: lake_icefrac(1,nlevlake)           
    real(r8), intent(out) :: savedtke1(1)                      
    real(r8), intent(out) :: frac_iceold(1,-nlevsnow+1:nlevsoil) 
    real(r8), intent(out) :: qflx_snomelt(1)  
    integer, intent(out)  :: imelt(1,-nlevsnow+1:nlevsoil)        




    integer , parameter  :: islak = 2     
    real(r8), parameter  :: p0 = 1._r8     
    integer  :: i,j,fc,fp,g,c,p         

    real(r8) :: beta(2)                 
    real(r8) :: za(2)                   
    real(r8) :: eta(2)                  
    real(r8) :: cwat                    
    real(r8) :: cice_eff                
                                          
    real(r8) :: cfus                    
                                          
    real(r8) :: km                      
    real(r8) :: tkice_eff               
    real(r8) :: a(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil)      
    real(r8) :: b(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil)      
    real(r8) :: c1(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil)     
    real(r8) :: r(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil)      
    real(r8) :: rhow(lbc:ubc,nlevlake)   
    real(r8) :: phi(lbc:ubc,nlevlake)    
    real(r8) :: kme(lbc:ubc,nlevlake)    
    real(r8) :: rsfin                   
    real(r8) :: rsfout                  
    real(r8) :: phi_soil(lbc:ubc)       
    real(r8) :: ri                      
    real(r8) :: fin(lbc:ubc)            
    real(r8) :: ocvts(lbc:ubc)          
    real(r8) :: ncvts(lbc:ubc)          
    real(r8) :: ke                      
    real(r8) :: zin                     
    real(r8) :: zout                    
    real(r8) :: drhodz                  
    real(r8) :: n2                      
    real(r8) :: num                     
    real(r8) :: den                     
    real(r8) :: tav_froz(lbc:ubc)       
    real(r8) :: tav_unfr(lbc:ubc)       
    real(r8) :: nav(lbc:ubc)            
    real(r8) :: phidum                  
    real(r8) :: iceav(lbc:ubc)          
    real(r8) :: qav(lbc:ubc)            
    integer  :: jtop(lbc:ubc)           
    real(r8) :: cv (lbc:ubc,-nlevsnow+1:nlevsoil)  
    real(r8) :: tk (lbc:ubc,-nlevsnow+1:nlevsoil)  
                                                 
    real(r8) :: cv_lake (lbc:ubc,1:nlevlake)      
    real(r8) :: tk_lake (lbc:ubc,1:nlevlake)  
    real(r8) :: cvx (lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil) 
    real(r8) :: tkix(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil) 
                                                         
    real(r8) :: tx(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil) 
    real(r8) :: tktopsoillay(lbc:ubc)          
    real(r8) :: fnx(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil)  
    real(r8) :: phix(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil) 
    real(r8) :: zx(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil)   
    real(r8) :: dzm                              
    real(r8) :: dzp                              
    integer  :: jprime                   
    real(r8) :: factx(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil) 
    real(r8) :: t_lake_bef(lbc:ubc,1:nlevlake)    
    real(r8) :: t_soisno_bef(lbc:ubc,-nlevsnow+1:nlevsoil) 
    real(r8) :: lhabs(lbc:ubc)       
    real(r8) :: esum1(lbc:ubc)        
    real(r8) :: esum2(lbc:ubc)        
    real(r8) :: zsum(lbc:ubc)        
    real(r8) :: wsum(lbc:ubc)        
    real(r8) :: wsum_end(lbc:ubc)    
    real(r8) :: errsoi(1)                         
    real(r8) :: eflx_snomelt(1)  
    CHARACTER*256 :: message



    data beta/0.4_r8, 0.4_r8/  
    data za  /0.6_r8, 0.6_r8/








    
    



    
    cwat = cpliq*denh2o 
    cice_eff = cpice*denh2o 
                              
    cfus = hfus*denh2o  
    tkice_eff = tkice * denice/denh2o 
    km = tkwat/cwat     

    

!dir$ concurrent

    do fc = 1, num_shlakec
       c = filter_shlakec(fc)

       

       ocvts(c) = 0._r8
       ncvts(c) = 0._r8
       esum1(c) = 0._r8
       esum2(c) = 0._r8

    end do

    
    
    
    

    do j = -nlevsnow+1,0
!dir$ concurrent

      do fc = 1, num_shlakec
         c = filter_shlakec(fc)
         if (j >= snl(c) + 1) then
            frac_iceold(c,j) = h2osoi_ice(c,j)/(h2osoi_liq(c,j)+h2osoi_ice(c,j))
         end if
      end do
    end do

    
    do j = 1, nlevsoil
!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)
          if (j == 1) wsum(c) = 0._r8
          wsum(c) = wsum(c) + h2osoi_ice(c,j) + h2osoi_liq(c,j)
       end do
    end do

!dir$ concurrent

    do fp = 1, num_shlakep
       p = filter_shlakep(fp)
       c = pcolumn(p)


       

       
       
       
       fin(c) = eflx_gnet(p)

    end do

    

    do j = 1, nlevlake
!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)
          rhow(c,j) = (1._r8 - lake_icefrac(c,j)) * & 
                      1000._r8*( 1.0_r8 - 1.9549e-05_r8*(abs(t_lake(c,j)-277._r8))**1.68_r8 ) &
                    + lake_icefrac(c,j)*denice
                    
                    
                    
                    
       end do
    end do

    
    do j = 1, nlevlake-1
!dir$ prefervector
!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)
          drhodz = (rhow(c,j+1)-rhow(c,j)) / (z_lake(c,j+1)-z_lake(c,j))
          n2 = grav / rhow(c,j) * drhodz
          
          
          num = 40._r8 * n2 * (vkc*z_lake(c,j))**2
          den = max( (ws(c)**2) * exp(-2._r8*ks(c)*z_lake(c,j)), 1.e-10_r8 )
          ri = ( -1._r8 + sqrt( max(1._r8+num/den, 0._r8) ) ) / 20._r8
          if (t_grnd(c) > tfrz .and. t_lake(c,1) > tfrz .and. snl(c) == 0) then
            

             if( t_lake(c,1) > 277.15_r8 ) then 
                if (lakedepth(c) > 15.0 ) then 
                   ke = 1.e+2_r8*vkc*ws(c)*z_lake(c,j)/p0 * exp(-ks(c)*z_lake(c,j)) / (1._r8+37._r8*ri*ri)
                else 
                   ke = vkc*ws(c)*z_lake(c,j)/p0 * exp(-ks(c)*z_lake(c,j)) / (1._r8+37._r8*ri*ri)
                endif
             else 
                if (lakedepth(c) > 15.0 ) then 
                  if (lakedepth(c) > 150.0 ) then 
                    ke = 1.e+5_r8*vkc*ws(c)*z_lake(c,j)/p0 * exp(-ks(c)*z_lake(c,j)) / (1._r8+37._r8*ri*ri)
                  else 
                    ke =1.e+4_r8*vkc*ws(c)*z_lake(c,j)/p0 * exp(-ks(c)*z_lake(c,j)) / (1._r8+37._r8*ri*ri)
                  end if
                else 
                  ke = vkc*ws(c)*z_lake(c,j)/p0 * exp(-ks(c)*z_lake(c,j)) / (1._r8+37._r8*ri*ri)
                endif 
             end if

             kme(c,j) = km + ke
             tk_lake(c,j) = kme(c,j)*cwat
             
             
             
          else
             kme(c,j) = km
             tk_lake(c,j) = tkwat*tkice_eff / ( (1._r8-lake_icefrac(c,j))*tkice_eff &
                            + tkwat*lake_icefrac(c,j) )
             
          end if
       end do
    end do

!dir$ concurrent

    do fc = 1, num_shlakec
       c = filter_shlakec(fc)

       j = nlevlake
       kme(c,nlevlake) = kme(c,nlevlake-1)

       if (t_grnd(c) > tfrz .and. t_lake(c,1) > tfrz .and. snl(c) == 0) then
          tk_lake(c,j) = tk_lake(c,j-1)
       else
          tk_lake(c,j) = tkwat*tkice_eff / ( (1._r8-lake_icefrac(c,j))*tkice_eff &
                            + tkwat*lake_icefrac(c,j) )
       end if

       
       savedtke1(c) = kme(c,1)*cwat 
       
       jtop(c) = snl(c) + 1
    end do

    
    do j = 1, nlevlake
!dir$ concurrent

       do fp = 1, num_shlakep
          p = filter_shlakep(fp)
          c = pcolumn(p)

          
          
          
          eta(:) = 1.1925_r8*lakedepth(c)**(-0.424)

          zin  = z_lake(c,j) - 0.5_r8*dz_lake(c,j)
          zout = z_lake(c,j) + 0.5_r8*dz_lake(c,j)
          rsfin  = exp( -eta(islak)*max(  zin-za(islak),0._r8 ) )
          rsfout = exp( -eta(islak)*max( zout-za(islak),0._r8 ) )

          
          
            
          if (t_grnd(c) > tfrz .and. t_lake(c,1) > tfrz .and. snl(c) == 0) then
             phidum = (rsfin-rsfout) * sabg(p) * (1._r8-beta(islak))
             if (j == nlevlake) then
                phi_soil(c) = rsfout * sabg(p) * (1._r8-beta(islak))
             end if
          else if (j == 1 .and. snl(c) == 0) then 
             phidum = sabg(p) * (1._r8-beta(islak))
          else 
             phidum = 0._r8
             if (j == nlevlake) phi_soil(c) = 0._r8
          end if
          phi(c,j) = phidum

       end do
    end do

    

    
    do j = 1, nlevlake
!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          cv_lake(c,j) = dz_lake(c,j) * (cwat*(1._r8-lake_icefrac(c,j)) + cice_eff*lake_icefrac(c,j))
       end do
    end do

    
  call SoilThermProp_Lake (snl,dz,zi,z,t_soisno,h2osoi_liq,h2osoi_ice,    &
                           tk, cv, tktopsoillay)

    
    

    
    do j = 1, nlevlake
!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)


          ocvts(c) = ocvts(c) + cv_lake(c,j)*(t_lake(c,j)-tfrz) &
                   + cfus*dz_lake(c,j)*(1._r8-lake_icefrac(c,j)) 

          t_lake_bef(c,j) = t_lake(c,j)
       end do
    end do

    
    do j = -nlevsnow + 1, nlevsoil
!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          if (j >= jtop(c)) then

             ocvts(c) = ocvts(c) + cv(c,j)*(t_soisno(c,j)-tfrz) &
                      + hfus*h2osoi_liq(c,j) 

             if (j == 1 .and. h2osno(c) > 0._r8 .and. j == jtop(c)) then
                ocvts(c) = ocvts(c) - h2osno(c)*hfus
             end if
             t_soisno_bef(c,j) = t_soisno(c,j)
             if(abs(t_soisno(c,j)-288) > 150)   then 
                WRITE( message,* ) 'WARNING: Extreme t_soisno at c, level',c, j
                CALL wrf_error_fatal3("<stdin>",1591,&
message )
             endif
          end if
       end do
    end do


    

    
    

    
    do j = -nlevsnow+1, nlevlake+nlevsoil
!dir$ prefervector
!dir$ concurrent

       do fc = 1,num_shlakec
          c = filter_shlakec(fc)

          jprime = j - nlevlake

          if (j >= jtop(c)) then
             if (j < 1) then 
                zx(c,j) = z(c,j)
                cvx(c,j) = cv(c,j)
                phix(c,j) = 0._r8
                tx(c,j) = t_soisno(c,j)
             else if (j <= nlevlake) then 
                zx(c,j) = z_lake(c,j)
                cvx(c,j) = cv_lake(c,j)
                phix(c,j) = phi(c,j)
                tx(c,j) = t_lake(c,j)
             else 
                zx(c,j) = zx(c,nlevlake) + dz_lake(c,nlevlake)/2._r8 + z(c,jprime)
                cvx(c,j) = cv(c,jprime)
                if (j == nlevlake + 1) then 
                   phix(c,j) = phi_soil(c)
                else 
                   phix(c,j) = 0._r8
                end if
                tx(c,j) = t_soisno(c,jprime)
             end if
          end if

       end do
    end do

    

    do j = -nlevsnow+1, nlevlake+nlevsoil
!dir$ prefervector
!dir$ concurrent

       do fc = 1,num_shlakec
          c = filter_shlakec(fc)

          jprime = j - nlevlake

          if (j >= jtop(c)) then
             if (j < 0) then 
                tkix(c,j) = tk(c,j)
             else if (j == 0) then 
                dzp = zx(c,j+1) - zx(c,j)
                tkix(c,j) = tk_lake(c,1)*tk(c,j)*dzp / &
                      (tk(c,j)*z_lake(c,1) + tk_lake(c,1)*(-z(c,j)) )
                
             else if (j < nlevlake) then 
                tkix(c,j) = ( tk_lake(c,j)*tk_lake(c,j+1) * (dz_lake(c,j+1)+dz_lake(c,j)) ) &
                           / ( tk_lake(c,j)*dz_lake(c,j+1) + tk_lake(c,j+1)*dz_lake(c,j) )
             else if (j == nlevlake) then 
                dzp = zx(c,j+1) - zx(c,j)
                tkix(c,j) = (tktopsoillay(c)*tk_lake(c,j)*dzp / &
                    (tktopsoillay(c)*dz_lake(c,j)/2._r8 + tk_lake(c,j)*z(c,1) ) )
                    
             else 
                tkix(c,j) = tk(c,jprime)
             end if
         end if

      end do 
   end do


    
    
    

    do j = -nlevsnow+1, nlevlake+nlevsoil
!dir$ prefervector
!dir$ concurrent

       do fc = 1,num_shlakec
          c = filter_shlakec(fc)
          if (j >= jtop(c)) then
             if (j < nlevlake+nlevsoil) then 
                factx(c,j) = dtime/cvx(c,j)
                fnx(c,j) = tkix(c,j)*(tx(c,j+1)-tx(c,j))/(zx(c,j+1)-zx(c,j))
             else 
                factx(c,j) = dtime/cvx(c,j)
                fnx(c,j) = 0._r8 
             end if
          end if
       enddo
    end do

    do j = -nlevsnow+1,nlevlake+nlevsoil
!dir$ prefervector
!dir$ concurrent

       do fc = 1,num_shlakec
          c = filter_shlakec(fc)
          if (j >= jtop(c)) then
             if (j == jtop(c)) then 
                dzp    = zx(c,j+1)-zx(c,j)
                a(c,j) = 0._r8
                b(c,j) = 1+(1._r8-cnfac)*factx(c,j)*tkix(c,j)/dzp
                c1(c,j) =  -(1._r8-cnfac)*factx(c,j)*tkix(c,j)/dzp
                r(c,j) = tx(c,j) + factx(c,j)*( fin(c) + phix(c,j) + cnfac*fnx(c,j) )
             else if (j < nlevlake+nlevsoil) then 
                dzm    = (zx(c,j)-zx(c,j-1))
                dzp    = (zx(c,j+1)-zx(c,j))
                a(c,j) =   - (1._r8-cnfac)*factx(c,j)* tkix(c,j-1)/dzm
                b(c,j) = 1._r8+ (1._r8-cnfac)*factx(c,j)*(tkix(c,j)/dzp + tkix(c,j-1)/dzm)
                c1(c,j) =   - (1._r8-cnfac)*factx(c,j)* tkix(c,j)/dzp
                r(c,j) = tx(c,j) + cnfac*factx(c,j)*( fnx(c,j) - fnx(c,j-1) ) + factx(c,j)*phix(c,j)
             else  
                dzm     = (zx(c,j)-zx(c,j-1))
                a(c,j) =   - (1._r8-cnfac)*factx(c,j)*tkix(c,j-1)/dzm
                b(c,j) = 1._r8+ (1._r8-cnfac)*factx(c,j)*tkix(c,j-1)/dzm
                c1(c,j) = 0._r8
                r(c,j) = tx(c,j) - cnfac*factx(c,j)*fnx(c,j-1)
             end if
          end if
       enddo
    end do



    

    call Tridiagonal(lbc, ubc, -nlevsnow + 1, nlevlake + nlevsoil, jtop, num_shlakec, filter_shlakec, &
                     a, b, c1, r, tx)
 
    
    do j = -nlevsnow+1, nlevlake + nlevsoil
!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          jprime = j - nlevlake


          if (j >= jtop(c)) then
             if (j < 1) then 
             t_soisno(c,j) = tx(c,j)
             else if (j <= nlevlake) then 
             t_lake(c,j)   = tx(c,j)
             else 
             t_soisno(c,jprime) = tx(c,j)
             end if
          end if
       end do
    end do



    
    




    
    call PhaseChange_Lake (snl,h2osno,dz,dz_lake,                            & 
                               t_soisno,h2osoi_liq,h2osoi_ice,               & 
                               lake_icefrac,t_lake, snowdp,                  & 
                               qflx_snomelt,eflx_snomelt,imelt,              & 
                               cv, cv_lake,                                  & 
                               lhabs)                                          



    
    
    
    
    



    
    
    

    
    do j = 1, nlevlake
!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)
          rhow(c,j) = (1._r8 - lake_icefrac(c,j)) * &
                      1000._r8*( 1.0_r8 - 1.9549e-05_r8*(abs(t_lake(c,j)-277._r8))**1.68_r8 ) &
                    + lake_icefrac(c,j)*denice
       end do
    end do

    do j = 1, nlevlake-1
!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)
          qav(c) = 0._r8
          nav(c) = 0._r8
          iceav(c) = 0._r8
       end do

       do i = 1, j+1
!dir$ concurrent

          do fc = 1, num_shlakec
             c = filter_shlakec(fc)
             if (rhow(c,j) > rhow(c,j+1) .or. &
                (lake_icefrac(c,j) < 1._r8 .and. lake_icefrac(c,j+1) > 0._r8) ) then
                qav(c) = qav(c) + dz_lake(c,i)*(t_lake(c,i)-tfrz) * & 
                        ((1._r8 - lake_icefrac(c,i))*cwat + lake_icefrac(c,i)*cice_eff)

                iceav(c) = iceav(c) + lake_icefrac(c,i)*dz_lake(c,i)
                nav(c) = nav(c) + dz_lake(c,i)
             end if
          end do
       end do

!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)
          if (rhow(c,j) > rhow(c,j+1) .or. &
             (lake_icefrac(c,j) < 1._r8 .and. lake_icefrac(c,j+1) > 0._r8) ) then
             qav(c) = qav(c)/nav(c)
             iceav(c) = iceav(c)/nav(c)
             
             
             if (qav(c) > 0._r8) then
                tav_froz(c) = 0._r8 
                tav_unfr(c) = qav(c) / ((1._r8 - iceav(c))*cwat)
             else if (qav(c) < 0._r8) then
                tav_froz(c) = qav(c) / (iceav(c)*cice_eff)
                tav_unfr(c) = 0._r8 
             else
                tav_froz(c) = 0._r8
                tav_unfr(c) = 0._r8
             end if
          end if
       end do

       do i = 1, j+1
!dir$ concurrent

          do fc = 1, num_shlakec
             c = filter_shlakec(fc)
             if (nav(c) > 0._r8) then


                
                
                
                
                
                
                if (i == 1) zsum(c) = 0._r8
                if ((zsum(c)+dz_lake(c,i))/nav(c) <= iceav(c)) then
                   lake_icefrac(c,i) = 1._r8
                   t_lake(c,i) = tav_froz(c) + tfrz
                else if (zsum(c)/nav(c) < iceav(c)) then
                   lake_icefrac(c,i) = (iceav(c)*nav(c) - zsum(c)) / dz_lake(c,i)
                   
                   t_lake(c,i) = ( lake_icefrac(c,i)*tav_froz(c)*cice_eff &
                               + (1._r8 - lake_icefrac(c,i))*tav_unfr(c)*cwat ) &
                               / ( lake_icefrac(c,i)*cice_eff + (1-lake_icefrac(c,i))*cwat ) + tfrz
                else
                   lake_icefrac(c,i) = 0._r8
                   t_lake(c,i) = tav_unfr(c) + tfrz
                end if
                zsum(c) = zsum(c) + dz_lake(c,i)

                rhow(c,i) = (1._r8 - lake_icefrac(c,i)) * & 
                            1000._r8*( 1.0_r8 - 1.9549e-05_r8*(abs(t_lake(c,i)-277._r8))**1.68_r8 ) &
                          + lake_icefrac(c,i)*denice
             end if
          end do
       end do
    end do


    
    
    do j = 1, nlevlake
!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          cv_lake(c,j) = dz_lake(c,j) * (cwat*(1._r8-lake_icefrac(c,j)) + cice_eff*lake_icefrac(c,j))
       end do
    end do
    
  
  call SoilThermProp_Lake (snl,dz,zi,z,t_soisno,h2osoi_liq,h2osoi_ice,    &
                           tk, cv, tktopsoillay)


    
    do j = 1, nlevlake
!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)


          ncvts(c) = ncvts(c) + cv_lake(c,j)*(t_lake(c,j)-tfrz) &
                   + cfus*dz_lake(c,j)*(1._r8-lake_icefrac(c,j)) 

          fin(c) = fin(c) + phi(c,j)
       end do
    end do

    do j = -nlevsnow + 1, nlevsoil
!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          if (j >= jtop(c)) then

             ncvts(c) = ncvts(c) + cv(c,j)*(t_soisno(c,j)-tfrz) &
                      + hfus*h2osoi_liq(c,j) 

             if (j == 1 .and. h2osno(c) > 0._r8 .and. j == jtop(c)) then
                ncvts(c) = ncvts(c) - h2osno(c)*hfus
             end if
          end if
          if (j == 1) fin(c) = fin(c) + phi_soil(c)
       end do
    end do


    

    do fp = 1, num_shlakep
       p = filter_shlakep(fp)
       c = pcolumn(p)
       errsoi(c) = (ncvts(c)-ocvts(c)) / dtime - fin(c)

       if (abs(errsoi(c)) < 10._r8) then 
          eflx_sh_tot(p) = eflx_sh_tot(p) - errsoi(c)
          eflx_sh_grnd(p) = eflx_sh_grnd(p) - errsoi(c)
          eflx_soil_grnd(p) = eflx_soil_grnd(p) + errsoi(c)
          eflx_gnet(p) = eflx_gnet(p) + errsoi(c)

          if (abs(errsoi(c)) > 1.e-1_r8) then
             write(message,*)'errsoi incorporated into sensible heat in ShalLakeTemperature: c, (W/m^2):', c, errsoi(c)
             CALL wrf_message(message)
          end if
          errsoi(c) = 0._r8
       end if
    end do
    

  end subroutine ShalLakeTemperature








  subroutine SoilThermProp_Lake (snl,dz,zi,z,t_soisno,h2osoi_liq,h2osoi_ice,    &
                           tk, cv, tktopsoillay)




















    implicit none


    integer , intent(in) :: snl(1)           

   
   
   
   
   
    real(r8), intent(in) :: dz(1,-nlevsnow+1:nlevsoil)          
    real(r8), intent(in) :: zi(1,-nlevsnow+0:nlevsoil)          
    real(r8), intent(in) :: z(1,-nlevsnow+1:nlevsoil)           
    real(r8), intent(in) :: t_soisno(1,-nlevsnow+1:nlevsoil)    
    real(r8), intent(in) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)  
    real(r8), intent(in) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)  


    real(r8), intent(out) :: cv(lbc:ubc,-nlevsnow+1:nlevsoil) 
    real(r8), intent(out) :: tk(lbc:ubc,-nlevsnow+1:nlevsoil) 
    real(r8), intent(out) :: tktopsoillay(lbc:ubc)          























    integer  :: l,c,j                     
    integer  :: fc                        
    real(r8) :: bw                        
    real(r8) :: dksat                     
    real(r8) :: dke                       
    real(r8) :: fl                        
    real(r8) :: satw                      
    real(r8) :: thk(lbc:ubc,-nlevsnow+1:nlevsoil) 
    character*256 :: message 



    do j = -nlevsnow+1,nlevsoil
!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          
          if (j >= 1) then


              

          
                satw = 1._r8
                   fl = h2osoi_liq(c,j)/(h2osoi_ice(c,j)+h2osoi_liq(c,j))
                   if (t_soisno(c,j) >= tfrz) then       
                      dke = max(0._r8, log10(satw) + 1.0_r8)
                      dksat = tksatu(c,j)
                   else                               
                      dke = satw
                      dksat = tkmg(c,j)*0.249_r8**(fl*watsat(c,j))*2.29_r8**watsat(c,j)
                   endif
                   thk(c,j) = dke*dksat + (1._r8-dke)*tkdry(c,j)




          endif

          
          
          if (snl(c)+1 < 1 .AND. (j >= snl(c)+1) .AND. (j <= 0)) then
             bw = (h2osoi_ice(c,j)+h2osoi_liq(c,j))/dz(c,j)
             thk(c,j) = tkairc + (7.75e-5_r8 *bw + 1.105e-6_r8*bw*bw)*(tkice-tkairc)
          end if

       end do
    end do

    

    
    
    
    
    do j = -nlevsnow+1,nlevsoil
!dir$ concurrent

       do fc = 1,num_shlakec
          c = filter_shlakec(fc)
          if (j >= snl(c)+1 .AND. j <= nlevsoil-1 .AND. j /= 0) then
             tk(c,j) = thk(c,j)*thk(c,j+1)*(z(c,j+1)-z(c,j)) &
                  /(thk(c,j)*(z(c,j+1)-zi(c,j))+thk(c,j+1)*(zi(c,j)-z(c,j)))
          else if (j == 0) then
             tk(c,j) = thk(c,j)
          else if (j == nlevsoil) then
             tk(c,j) = 0._r8
          end if
          
          if (j == 1) tktopsoillay(c) = thk(c,j)
       end do
    end do

    

    do j = 1, nlevsoil
!dir$ concurrent

       do fc = 1,num_shlakec
          c = filter_shlakec(fc)


             cv(c,j) = csol(c,j)*(1-watsat(c,j))*dz(c,j) +   &
               (h2osoi_ice(c,j)*cpice + h2osoi_liq(c,j)*cpliq)









       enddo
    end do

    

    do j = -nlevsnow+1,0
!dir$ concurrent

       do fc = 1,num_shlakec
          c = filter_shlakec(fc)
          if (snl(c)+1 < 1 .and. j >= snl(c)+1) then
             cv(c,j) = cpliq*h2osoi_liq(c,j) + cpice*h2osoi_ice(c,j)
          end if
       end do
    end do

  end subroutine SoilThermProp_Lake








  subroutine PhaseChange_Lake (snl,h2osno,dz,dz_lake,                        & 
                               t_soisno,h2osoi_liq,h2osoi_ice,               & 
                               lake_icefrac,t_lake, snowdp,                  & 
                               qflx_snomelt,eflx_snomelt,imelt,              & 
                               cv, cv_lake,                                  & 
                               lhabs)                                          


























    implicit none


    integer , intent(in) :: snl(1)           
    real(r8), intent(inout) :: h2osno(1)        
    real(r8), intent(in) :: dz(1,-nlevsnow+1:nlevsoil)          
    real(r8), intent(in) :: dz_lake(1,nlevlake)     
    



    real(r8), intent(inout) :: snowdp(1)        
    real(r8), intent(inout) :: t_soisno(1,-nlevsnow+1:nlevsoil)     
    real(r8), intent(inout) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)   
    real(r8), intent(inout) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)   
    real(r8), intent(inout) :: lake_icefrac(1,nlevlake) 
    real(r8), intent(inout) :: t_lake(1,nlevlake)       


    real(r8), intent(out) :: qflx_snomelt(1)  
    real(r8), intent(out) :: eflx_snomelt(1)  
    integer, intent(out)  :: imelt(1,-nlevsnow+1:nlevsoil)        

    real(r8), intent(inout) :: cv(lbc:ubc,-nlevsnow+1:nlevsoil)       
    real(r8), intent(inout) :: cv_lake (lbc:ubc,1:nlevlake)          
    real(r8), intent(out):: lhabs(lbc:ubc)                       




    integer  :: j,c,g                              
    integer  :: fc                                 

    real(r8) :: heatavail                          
    real(r8) :: heatrem                            
    real(r8) :: melt                               
    real(r8), parameter :: smallnumber = 1.e-7_r8  
    logical  :: dophasechangeflag




    

!dir$ concurrent

    do fc = 1,num_shlakec
       c = filter_shlakec(fc)

       qflx_snomelt(c) = 0._r8
       eflx_snomelt(c) = 0._r8
       lhabs(c)        = 0._r8
    end do

    do j = -nlevsnow+1,0
!dir$ concurrent

       do fc = 1,num_shlakec
          c = filter_shlakec(fc)

          if (j >= snl(c) + 1) imelt(c,j) = 0
       end do
    end do

    

!dir$ concurrent

    do fc = 1,num_shlakec
       c = filter_shlakec(fc)

       if (snl(c) == 0 .and. h2osno(c) > 0._r8 .and. t_lake(c,1) > tfrz) then
          heatavail = (t_lake(c,1) - tfrz) * cv_lake(c,1)
          melt = min(h2osno(c), heatavail/hfus)
          heatrem = max(heatavail - melt*hfus, 0._r8)
                       
          t_lake(c,1) = tfrz + heatrem/(cv_lake(c,1))
          snowdp(c) = snowdp(c)*(1._r8 - melt/h2osno(c))
          h2osno(c) = h2osno(c) - melt
          lhabs(c) = lhabs(c) + melt*hfus
          qflx_snomelt(c) = qflx_snomelt(c) + melt
          
          if (h2osno(c) < smallnumber) h2osno(c) = 0._r8
          if (snowdp(c) < smallnumber) snowdp(c) = 0._r8
       end if
    end do

    

    do j = 1,nlevlake
!dir$ concurrent

       do fc = 1,num_shlakec
          c = filter_shlakec(fc)

          dophasechangeflag = .false.
          if (t_lake(c,j) > tfrz .and. lake_icefrac(c,j) > 0._r8) then 
             dophasechangeflag = .true.
             heatavail = (t_lake(c,j) - tfrz) * cv_lake(c,j)
             melt = min(lake_icefrac(c,j)*denh2o*dz_lake(c,j), heatavail/hfus)
                        
             heatrem = max(heatavail - melt*hfus, 0._r8)
                       
          else if (t_lake(c,j) < tfrz .and. lake_icefrac(c,j) < 1._r8) then 
             dophasechangeflag = .true.
             heatavail = (t_lake(c,j) - tfrz) * cv_lake(c,j)
             melt = max(-(1._r8-lake_icefrac(c,j))*denh2o*dz_lake(c,j), heatavail/hfus)
                        
             heatrem = min(heatavail - melt*hfus, 0._r8)
                       
          end if
          
          if (dophasechangeflag) then
             lake_icefrac(c,j) = lake_icefrac(c,j) - melt/(denh2o*dz_lake(c,j))
             lhabs(c) = lhabs(c) + melt*hfus
          
             cv_lake(c,j) = cv_lake(c,j) + melt*(cpliq-cpice)
             t_lake(c,j) = tfrz + heatrem/cv_lake(c,j)
             
             if (lake_icefrac(c,j) > 1._r8 - smallnumber) lake_icefrac(c,j) = 1._r8
             if (lake_icefrac(c,j) < smallnumber)         lake_icefrac(c,j) = 0._r8
          end if
       end do
    end do

    

    do j = -nlevsnow+1,nlevsoil
!dir$ concurrent

       do fc = 1,num_shlakec
          c = filter_shlakec(fc)
          dophasechangeflag = .false.

          if (j >= snl(c) + 1) then

             if (t_soisno(c,j) > tfrz .and. h2osoi_ice(c,j) > 0._r8) then 
                dophasechangeflag = .true.
                heatavail = (t_soisno(c,j) - tfrz) * cv(c,j)
                melt = min(h2osoi_ice(c,j), heatavail/hfus)
                heatrem = max(heatavail - melt*hfus, 0._r8)
                          
                if (j <= 0) then 
                   imelt(c,j) = 1
                   qflx_snomelt(c) = qflx_snomelt(c) + melt
                end if
             else if (t_soisno(c,j) < tfrz .and. h2osoi_liq(c,j) > 0._r8) then 
                dophasechangeflag = .true.
                heatavail = (t_soisno(c,j) - tfrz) * cv(c,j)
                melt = max(-h2osoi_liq(c,j), heatavail/hfus)
                heatrem = min(heatavail - melt*hfus, 0._r8)
                          
                if (j <= 0) then 
                   imelt(c,j) = 2
                   qflx_snomelt(c) = qflx_snomelt(c) + melt
                   
                   
                end if
             end if

             
             if (dophasechangeflag) then
                h2osoi_ice(c,j) = h2osoi_ice(c,j) - melt
                h2osoi_liq(c,j) = h2osoi_liq(c,j) + melt
                lhabs(c) = lhabs(c) + melt*hfus
             
                cv(c,j) = cv(c,j) + melt*(cpliq-cpice)
                t_soisno(c,j) = tfrz + heatrem/cv(c,j)
                
                if (h2osoi_ice(c,j) < smallnumber) h2osoi_ice(c,j) = 0._r8
                if (h2osoi_liq(c,j) < smallnumber) h2osoi_liq(c,j) = 0._r8
             end if

         end if
      end do
   end do

   
!dir$ concurrent

    do fc = 1,num_shlakec
       c = filter_shlakec(fc)
       eflx_snomelt(c) = qflx_snomelt(c)*hfus
    end do


   end subroutine PhaseChange_Lake


  subroutine ShalLakeHydrology(dz_lake,forc_rain,forc_snow,                      & 
                               begwb,qflx_evap_tot,forc_t,do_capsnow,            &
                               t_grnd,qflx_evap_soi,                             &
                               qflx_snomelt,imelt,frac_iceold,                   & 
                               z,dz,zi,snl,h2osno,snowdp,lake_icefrac,t_lake,      & 
                               endwb,snowage,snowice,snowliq,t_snow,             & 
                               t_soisno,h2osoi_ice,h2osoi_liq,h2osoi_vol,        &
                               qflx_drain,qflx_surf,qflx_infl,qflx_qrgwl,        &
                               qcharge,qflx_prec_grnd,qflx_snowcap,              &
                               qflx_snowcap_col,qflx_snow_grnd_pft,              &
                               qflx_snow_grnd_col,qflx_rain_grnd,                &
                               qflx_evap_tot_col,soilalpha,zwt,fcov,             &
                               rootr_column,qflx_evap_grnd,qflx_sub_snow,        &
                               qflx_dew_snow,qflx_dew_grnd,qflx_rain_grnd_col)
                       





































    implicit none




   
   
    real(r8), intent(in) :: dz_lake(1,nlevlake)     
    real(r8), intent(in) :: forc_rain(1)     
    real(r8), intent(in) :: forc_snow(1)     
    real(r8), intent(in) :: qflx_evap_tot(1) 
    real(r8), intent(in) :: forc_t(1)        
    logical , intent(in) :: do_capsnow(1)     
    real(r8), intent(in) :: t_grnd(1)          
    real(r8), intent(in) :: qflx_evap_soi(1)   
    real(r8), intent(in) :: qflx_snomelt(1)     
    integer,  intent(in) :: imelt(1,-nlevsnow+1:nlevsoil)        



    real(r8), intent(inout) :: begwb(1)         



    
    real(r8), intent(inout) :: z(1,-nlevsnow+1:nlevsoil)           
    real(r8), intent(inout) :: dz(1,-nlevsnow+1:nlevsoil)          
    real(r8), intent(inout) :: zi(1,-nlevsnow+0:nlevsoil)          
    integer , intent(inout) :: snl(1)           
    real(r8), intent(inout) :: h2osno(1)        
    real(r8), intent(inout) :: snowdp(1)        
    real(r8), intent(inout) :: lake_icefrac(1,nlevlake)  
    real(r8), intent(inout) :: t_lake(1,nlevlake)        

    real(r8), intent(inout) :: frac_iceold(1,-nlevsnow+1:nlevsoil)      



    real(r8), intent(out) :: endwb(1)         
    real(r8), intent(out) :: snowage(1)       
    real(r8), intent(out) :: snowice(1)       
    real(r8), intent(out) :: snowliq(1)       
    real(r8), intent(out) :: t_snow(1)        
    real(r8), intent(out) :: t_soisno(1,-nlevsnow+1:nlevsoil)    
    real(r8), intent(out) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)  
    real(r8), intent(out) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)  
    real(r8), intent(out) :: h2osoi_vol(1,-nlevsnow+1:nlevsoil)  
    real(r8), intent(out) :: qflx_drain(1)    
    real(r8), intent(out) :: qflx_surf(1)     
    real(r8), intent(out) :: qflx_infl(1)     
    real(r8), intent(out) :: qflx_qrgwl(1)    
    real(r8), intent(out) :: qcharge(1)       
    real(r8), intent(out) :: qflx_prec_grnd(1)     
    real(r8), intent(out) :: qflx_snowcap(1)       
    real(r8), intent(out) :: qflx_snowcap_col(1)   
    real(r8), intent(out) :: qflx_snow_grnd_pft(1) 
    real(r8), intent(out) :: qflx_snow_grnd_col(1) 
    real(r8), intent(out) :: qflx_rain_grnd(1)     
    real(r8), intent(out) :: qflx_evap_tot_col(1) 
    real(r8) ,intent(out) :: soilalpha(1)     
    real(r8), intent(out) :: zwt(1)           
    real(r8), intent(out) :: fcov(1)          
    real(r8), intent(out) :: rootr_column(1,1:nlevsoil) 
    real(r8), intent(out) :: qflx_evap_grnd(1)  
    real(r8), intent(out) :: qflx_sub_snow(1)   
    real(r8), intent(out) :: qflx_dew_snow(1)   
    real(r8), intent(out) :: qflx_dew_grnd(1)   
    real(r8), intent(out) :: qflx_rain_grnd_col(1)   


    real(r8), pointer :: sucsat(:,:)      
    real(r8), pointer :: bsw(:,:)         
    real(r8), pointer :: bsw2(:,:)        
    real(r8), pointer :: psisat(:,:)      
    real(r8), pointer :: vwcsat(:,:)      
    real(r8), pointer :: wf(:)            
    real(r8), pointer :: soilpsi(:,:)     
    real(r8) :: psi,vwc,fsat               




    integer  :: p,fp,g,l,c,j,fc,jtop             
    integer  :: num_shlakesnowc                  
    integer  :: filter_shlakesnowc(ubc-lbc+1)    
    integer  :: num_shlakenosnowc                
    integer  :: filter_shlakenosnowc(ubc-lbc+1)  

    integer  :: newnode                      
    real(r8) :: dz_snowf                     
    real(r8) :: bifall                       
    real(r8) :: fracsnow(lbp:ubp)            
    real(r8) :: fracrain(lbp:ubp)            
    real(r8) :: qflx_prec_grnd_snow(lbp:ubp) 
    real(r8) :: qflx_prec_grnd_rain(lbp:ubp) 
    real(r8) :: qflx_evap_soi_lim            
    real(r8) :: h2osno_temp                  
    real(r8), parameter :: snow_bd = 250._r8  
    real(r8) :: sumsnowice(lbc:ubc)             
    logical  :: unfrozen(lbc:ubc)            
    real(r8) :: heatrem                      
    real(r8) :: heatsum(lbc:ubc)             
    real(r8) :: qflx_top_soil(1)     
    character*256 :: message 




    



    
    do j = 1, nlevsoil
!dir$ concurrent

      do fc = 1, num_shlakec
         c = filter_shlakec(fc)
         begwb(c) = begwb(c) + h2osoi_ice(c,j) + h2osoi_liq(c,j)
      end do
    end do



    

!dir$ concurrent

    do fp = 1, num_shlakep
       p = filter_shlakep(fp)
       g = pgridcell(p)

       c = pcolumn(p)

       





       

          qflx_prec_grnd_snow(p) = forc_snow(g)
          qflx_prec_grnd_rain(p) = forc_rain(g) 




       qflx_prec_grnd(p) = qflx_prec_grnd_snow(p) + qflx_prec_grnd_rain(p)

       if (do_capsnow(c)) then
          qflx_snowcap(p) = qflx_prec_grnd_snow(p) + qflx_prec_grnd_rain(p)
          qflx_snow_grnd_pft(p) = 0._r8
          qflx_rain_grnd(p) = 0._r8
       else
          qflx_snowcap(p) = 0._r8
          qflx_snow_grnd_pft(p) = qflx_prec_grnd_snow(p)           
          qflx_rain_grnd(p)     = qflx_prec_grnd_rain(p)           
       end if
       
       qflx_snow_grnd_col(c) = qflx_snow_grnd_pft(p)
       qflx_rain_grnd_col(c) = qflx_rain_grnd(p)

    end do 

    

!dir$ concurrent

    do fc = 1, num_shlakec
       c = filter_shlakec(fc)

       g = cgridcell(c)

       
       
       

       if (do_capsnow(c)) then
          dz_snowf = 0._r8
       else
          if (forc_t(g) > tfrz + 2._r8) then
             bifall=50._r8 + 1.7_r8*(17.0_r8)**1.5_r8
          else if (forc_t(g) > tfrz - 15._r8) then
             bifall=50._r8 + 1.7_r8*(forc_t(g) - tfrz + 15._r8)**1.5_r8
          else
             bifall=50._r8
          end if
          dz_snowf = qflx_snow_grnd_col(c)/bifall
          snowdp(c) = snowdp(c) + dz_snowf*dtime
          h2osno(c) = h2osno(c) + qflx_snow_grnd_col(c)*dtime  
       end if






       

       
       
       

       newnode = 0    
       if (snl(c) == 0 .and. qflx_snow_grnd_col(c) > 0.0_r8 .and. snowdp(c) >= 0.01_r8) then
          newnode = 1
          snl(c) = -1
          dz(c,0) = snowdp(c)                       
          z(c,0) = -0.5_r8*dz(c,0)
          zi(c,-1) = -dz(c,0)
          snowage(c) = 0._r8                        
          t_soisno(c,0) = min(tfrz, forc_t(g))      
          h2osoi_ice(c,0) = h2osno(c)               
          h2osoi_liq(c,0) = 0._r8                   
          frac_iceold(c,0) = 1._r8
       end if

       
       
       

       if (snl(c) < 0 .and. newnode == 0) then
          h2osoi_ice(c,snl(c)+1) = h2osoi_ice(c,snl(c)+1)+dtime*qflx_snow_grnd_col(c)
          dz(c,snl(c)+1) = dz(c,snl(c)+1)+dz_snowf*dtime
       end if

    end do

    

!dir$ concurrent

    do fp = 1,num_shlakep
       p = filter_shlakep(fp)
       c = pcolumn(p)
       jtop = snl(c)+1

       
       qflx_evap_grnd(c) = 0._r8
       qflx_sub_snow(c) = 0._r8
       qflx_dew_snow(c) = 0._r8
       qflx_dew_grnd(c) = 0._r8

       if (jtop <= 0) then 
          j = jtop
          
          

          if (qflx_evap_soi(p) >= 0._r8) then
          
          

          
             qflx_evap_soi_lim = min(qflx_evap_soi(p), (h2osoi_liq(c,j)+h2osoi_ice(c,j))/dtime)
             if ((h2osoi_liq(c,j)+h2osoi_ice(c,j)) > 0._r8) then
                qflx_evap_grnd(c) = max(qflx_evap_soi_lim*(h2osoi_liq(c,j)/(h2osoi_liq(c,j)+h2osoi_ice(c,j))), 0._r8)
             else
                qflx_evap_grnd(c) = 0._r8
             end if
             qflx_sub_snow(c) = qflx_evap_soi_lim - qflx_evap_grnd(c)     
          else
             if (t_grnd(c) < tfrz) then
                qflx_dew_snow(c) = abs(qflx_evap_soi(p))
             else
                qflx_dew_grnd(c) = abs(qflx_evap_soi(p))
             end if
          end if
          
          
          
          if (do_capsnow(c)) qflx_snowcap(p) = qflx_snowcap(p) + qflx_dew_snow(c) + qflx_dew_grnd(c)

       else 
          if (qflx_evap_soi(p) >= 0._r8) then
             
             
             qflx_sub_snow(c) = min(qflx_evap_soi(p), h2osno(c)/dtime)
             qflx_evap_grnd(c) = qflx_evap_soi(p) - qflx_sub_snow(c)
          else
             if (t_grnd(c) < tfrz-0.1_r8) then
                qflx_dew_snow(c) = abs(qflx_evap_soi(p))
             else
                qflx_dew_grnd(c) = abs(qflx_evap_soi(p))
             end if
          end if

          
          h2osno_temp = h2osno(c)
          if (do_capsnow(c)) then
             h2osno(c) = h2osno(c) - qflx_sub_snow(c)*dtime
             qflx_snowcap(p) = qflx_snowcap(p) + qflx_dew_snow(c) + qflx_dew_grnd(c)
          else
             h2osno(c) = h2osno(c) + (-qflx_sub_snow(c)+qflx_dew_snow(c))*dtime
          end if
          if (h2osno_temp > 0._r8) then
             snowdp(c) = snowdp(c) * h2osno(c) / h2osno_temp
          else
             snowdp(c) = h2osno(c)/snow_bd 
          end if

          h2osno(c) = max(h2osno(c), 0._r8)

       end if

    qflx_snowcap_col(c) = qflx_snowcap(p)

    end do



    
    

    call BuildSnowFilter(lbc, ubc, num_shlakec, filter_shlakec,snl,       &            
         num_shlakesnowc, filter_shlakesnowc, num_shlakenosnowc, filter_shlakenosnowc) 

    

    call SnowWater(lbc, ubc, num_shlakesnowc, filter_shlakesnowc,         & 
                   num_shlakenosnowc, filter_shlakenosnowc,               & 
                   snl,do_capsnow,qflx_snomelt,qflx_rain_grnd,            & 
                   qflx_sub_snow,qflx_evap_grnd,                          & 
                   qflx_dew_snow,qflx_dew_grnd,dz,                        & 
                   h2osoi_ice,h2osoi_liq,                                 & 
                   qflx_top_soil)                                           


    
    
    
    

    do j = 1,nlevsoil
!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          if (h2osoi_vol(c,j) < watsat(c,j)) then
             h2osoi_liq(c,j) = (watsat(c,j)*dz(c,j) - h2osoi_ice(c,j)/denice)*denh2o
          
          else if (h2osoi_liq(c,j) > watsat(c,j)*denh2o*dz(c,j)) then
             h2osoi_liq(c,j) = watsat(c,j)*denh2o*dz(c,j)
          end if

       end do
    end do



    if (1==1) then

       

       call SnowCompaction(lbc, ubc, num_shlakesnowc, filter_shlakesnowc,   &
                           snl,imelt,frac_iceold,t_soisno,                  &
                           h2osoi_ice,h2osoi_liq,                           &
                           dz)                                               

       

       call CombineSnowLayers(lbc, ubc,                            & 
                              num_shlakesnowc, filter_shlakesnowc, & 
                              snl,h2osno,snowdp,dz,zi,             & 
                              t_soisno,h2osoi_ice,h2osoi_liq,      & 
                              z)  


       

       call DivideSnowLayers(lbc, ubc,                             & 
                             num_shlakesnowc, filter_shlakesnowc,  & 
                             snl,dz,zi,t_soisno,                   & 
                             h2osoi_ice,h2osoi_liq,                & 
                             z)  


    else

       do fc = 1, num_shlakesnowc
          c = filter_shlakesnowc(fc)
          h2osno(c) = 0._r8
       end do
       do j = -nlevsnow+1,0
          do fc = 1, num_shlakesnowc
             c = filter_shlakesnowc(fc)
             if (j >= snl(c)+1) then
                h2osno(c) = h2osno(c) + h2osoi_ice(c,j) + h2osoi_liq(c,j)
             end if
          end do
       end do

    end if

    
    
    
    
    
!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          if (t_lake(c,1) > tfrz .and. lake_icefrac(c,1) == 0._r8 .and. snl(c) < 0) then
             unfrozen(c) = .true.
          else
             unfrozen(c) = .false.
          end if
       end do

    do j = -nlevsnow+1,0
!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          if (unfrozen(c)) then
             if (j == -nlevsnow+1) then
                sumsnowice(c) = 0._r8
                heatsum(c) = 0._r8
             end if
             if (j >= snl(c)+1) then
                sumsnowice(c) = sumsnowice(c) + h2osoi_ice(c,j)
                heatsum(c) = heatsum(c) + h2osoi_ice(c,j)*cpice*(tfrz - t_soisno(c,j)) &
                           + h2osoi_liq(c,j)*cpliq*(tfrz - t_soisno(c,j))
             end if
          end if
       end do
    end do

!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          if (unfrozen(c)) then
             heatsum(c) = heatsum(c) + sumsnowice(c)*hfus
             heatrem = (t_lake(c,1) - tfrz)*cpliq*denh2o*dz_lake(c,1) - heatsum(c)

             if (heatrem + denh2o*dz_lake(c,1)*hfus > 0._r8) then            
                
                h2osno(c) = 0._r8
                snl(c) = 0
                
                if (heatrem > 0._r8) then 
                   t_lake(c,1) = t_lake(c,1) - heatrem/(cpliq*denh2o*dz_lake(c,1))
                else 
                   t_lake(c,1) = tfrz
                   lake_icefrac(c,1) = -heatrem/(denh2o*dz_lake(c,1)*hfus)
                end if
             end if
          end if
       end do


    

!dir$ concurrent

    do fc = 1, num_shlakesnowc
       c = filter_shlakesnowc(fc)
       if (snl(c) == 0) then
          snowage(c) = 0._r8
       end if
    end do

    

    do j = -nlevsnow+1,0
!dir$ concurrent

       do fc = 1, num_shlakesnowc
          c = filter_shlakesnowc(fc)
          if (j <= snl(c) .and. snl(c) > -nlevsnow) then
             h2osoi_ice(c,j) = 0._r8
             h2osoi_liq(c,j) = 0._r8
             t_soisno(c,j) = 0._r8
             dz(c,j) = 0._r8
             z(c,j) = 0._r8
             zi(c,j-1) = 0._r8
          end if
       end do
    end do

    

    call BuildSnowFilter(lbc, ubc, num_shlakec, filter_shlakec, snl,&   
         num_shlakesnowc, filter_shlakesnowc, num_shlakenosnowc, filter_shlakenosnowc) 

    
    

!dir$ concurrent

    do fc = 1, num_shlakesnowc
       c = filter_shlakesnowc(fc)
       t_snow(c)  = 0._r8
       snowice(c) = 0._r8
       snowliq(c) = 0._r8
    end do
!dir$ concurrent

    do fc = 1, num_shlakenosnowc
       c = filter_shlakenosnowc(fc)
       t_snow(c)  = spval
       snowice(c) = spval
       snowliq(c) = spval
    end do

    do j = -nlevsnow+1, 0
!dir$ concurrent

       do fc = 1, num_shlakesnowc
          c = filter_shlakesnowc(fc)
          if (j >= snl(c)+1) then
             t_snow(c)  = t_snow(c) + t_soisno(c,j)
             snowice(c) = snowice(c) + h2osoi_ice(c,j)
             snowliq(c) = snowliq(c) + h2osoi_liq(c,j)
          end if
       end do
    end do

    

!dir$ concurrent

    do fc = 1, num_shlakec
       
       c = filter_shlakec(fc)
       if (snl(c) < 0) t_snow(c) = t_snow(c)/abs(snl(c))
       endwb(c) = h2osno(c)
    end do

    do j = 1, nlevsoil
!dir$ concurrent

       do fc = 1, num_shlakec
          c = filter_shlakec(fc)
          endwb(c) = endwb(c) + h2osoi_ice(c,j) + h2osoi_liq(c,j)
          h2osoi_vol(c,j) = h2osoi_liq(c,j)/(dz(c,j)*denh2o) + h2osoi_ice(c,j)/(dz(c,j)*denice)
       end do
    end do



    
!dir$ concurrent

    do fp = 1,num_shlakep
       p = filter_shlakep(fp)
       c = pcolumn(p)
       g = pgridcell(p)

       qflx_infl(c)      = 0._r8
       qflx_surf(c)      = 0._r8
       qflx_drain(c)     = 0._r8
       rootr_column(c,:) = spval
       soilalpha(c)      = spval
       zwt(c)            = spval
       fcov(c)           = spval
       qcharge(c)        = spval


       
       qflx_qrgwl(c)     = forc_rain(g) + forc_snow(g) - qflx_evap_tot(p) - (endwb(c)-begwb(c))/dtime

       
       qflx_evap_tot_col(c) = qflx_evap_tot(p)
    end do






  end subroutine ShalLakeHydrology

  subroutine QSat (T, p, es, esdT, qs, qsdT)











    implicit none
    real(r8), intent(in)  :: T        
    real(r8), intent(in)  :: p        
    real(r8), intent(out) :: es       
    real(r8), intent(out) :: esdT     
    real(r8), intent(out) :: qs       
    real(r8), intent(out) :: qsdT     














    real(r8) :: T_limit
    real(r8) :: td,vp,vp1,vp2



    real(r8), parameter :: a0 =  6.11213476
    real(r8), parameter :: a1 =  0.444007856
    real(r8), parameter :: a2 =  0.143064234e-01
    real(r8), parameter :: a3 =  0.264461437e-03
    real(r8), parameter :: a4 =  0.305903558e-05
    real(r8), parameter :: a5 =  0.196237241e-07
    real(r8), parameter :: a6 =  0.892344772e-10
    real(r8), parameter :: a7 = -0.373208410e-12
    real(r8), parameter :: a8 =  0.209339997e-15



    real(r8), parameter :: b0 =  0.444017302
    real(r8), parameter :: b1 =  0.286064092e-01
    real(r8), parameter :: b2 =  0.794683137e-03
    real(r8), parameter :: b3 =  0.121211669e-04
    real(r8), parameter :: b4 =  0.103354611e-06
    real(r8), parameter :: b5 =  0.404125005e-09
    real(r8), parameter :: b6 = -0.788037859e-12
    real(r8), parameter :: b7 = -0.114596802e-13
    real(r8), parameter :: b8 =  0.381294516e-16



    real(r8), parameter :: c0 =  6.11123516
    real(r8), parameter :: c1 =  0.503109514
    real(r8), parameter :: c2 =  0.188369801e-01
    real(r8), parameter :: c3 =  0.420547422e-03
    real(r8), parameter :: c4 =  0.614396778e-05
    real(r8), parameter :: c5 =  0.602780717e-07
    real(r8), parameter :: c6 =  0.387940929e-09
    real(r8), parameter :: c7 =  0.149436277e-11
    real(r8), parameter :: c8 =  0.262655803e-14



    real(r8), parameter :: d0 =  0.503277922
    real(r8), parameter :: d1 =  0.377289173e-01
    real(r8), parameter :: d2 =  0.126801703e-02
    real(r8), parameter :: d3 =  0.249468427e-04
    real(r8), parameter :: d4 =  0.313703411e-06
    real(r8), parameter :: d5 =  0.257180651e-08
    real(r8), parameter :: d6 =  0.133268878e-10
    real(r8), parameter :: d7 =  0.394116744e-13
    real(r8), parameter :: d8 =  0.498070196e-16


    T_limit = T - tfrz
    if (T_limit > 100.0) T_limit=100.0
    if (T_limit < -75.0) T_limit=-75.0

    td       = T_limit
    if (td >= 0.0) then
       es   = a0 + td*(a1 + td*(a2 + td*(a3 + td*(a4 &
            + td*(a5 + td*(a6 + td*(a7 + td*a8)))))))
       esdT = b0 + td*(b1 + td*(b2 + td*(b3 + td*(b4 &
            + td*(b5 + td*(b6 + td*(b7 + td*b8)))))))
    else
       es   = c0 + td*(c1 + td*(c2 + td*(c3 + td*(c4 &
            + td*(c5 + td*(c6 + td*(c7 + td*c8)))))))
       esdT = d0 + td*(d1 + td*(d2 + td*(d3 + td*(d4 &
            + td*(d5 + td*(d6 + td*(d7 + td*d8)))))))
    endif

    es    = es    * 100.            
    esdT  = esdT  * 100.            

    vp    = 1.0   / (p - 0.378*es)
    vp1   = 0.622 * vp
    vp2   = vp1   * vp

    qs    = es    * vp1             
    qsdT  = esdT  * vp2 * p         

  end subroutine QSat


  subroutine Tridiagonal (lbc, ubc, lbj, ubj, jtop, numf, filter, &
                          a, b, c, r, u)





  


    implicit none
    integer , intent(in)    :: lbc, ubc               
    integer , intent(in)    :: lbj, ubj               
    integer , intent(in)    :: jtop(lbc:ubc)          
    integer , intent(in)    :: numf                   
    integer , intent(in)    :: filter(1:numf)         
    real(r8), intent(in)    :: a(lbc:ubc, lbj:ubj)    
    real(r8), intent(in)    :: b(lbc:ubc, lbj:ubj)    
    real(r8), intent(in)    :: c(lbc:ubc, lbj:ubj)    
    real(r8), intent(in)    :: r(lbc:ubc, lbj:ubj)    
    real(r8), intent(inout) :: u(lbc:ubc, lbj:ubj)    















    integer  :: j,ci,fc                   
    real(r8) :: gam(lbc:ubc,lbj:ubj)      
    real(r8) :: bet(lbc:ubc)              


    

!dir$ concurrent

    do fc = 1,numf
       ci = filter(fc)
       bet(ci) = b(ci,jtop(ci))
    end do

    do j = lbj, ubj
!dir$ prefervector
!dir$ concurrent

       do fc = 1,numf
          ci = filter(fc)
          if (j >= jtop(ci)) then
             if (j == jtop(ci)) then
                u(ci,j) = r(ci,j) / bet(ci)
             else
                gam(ci,j) = c(ci,j-1) / bet(ci)
                bet(ci) = b(ci,j) - a(ci,j) * gam(ci,j)
                u(ci,j) = (r(ci,j) - a(ci,j)*u(ci,j-1)) / bet(ci)
             end if
          end if
       end do
    end do


!dir$ unroll 0
    do j = ubj-1,lbj,-1
!dir$ prefervector
!dir$ concurrent

       do fc = 1,numf
          ci = filter(fc)
          if (j >= jtop(ci)) then
             u(ci,j) = u(ci,j) - gam(ci,j+1) * u(ci,j+1)
          end if
       end do
    end do

  end subroutine Tridiagonal


  subroutine SnowWater(lbc, ubc, num_snowc, filter_snowc,         & 
                   num_nosnowc, filter_nosnowc,               & 
                   snl,do_capsnow,qflx_snomelt,qflx_rain_grnd,            & 
                   qflx_sub_snow,qflx_evap_grnd,                          & 
                   qflx_dew_snow,qflx_dew_grnd,dz,                        & 
                   h2osoi_ice,h2osoi_liq,                                 & 
                   qflx_top_soil)                                           




















  

    implicit none


    integer, intent(in) :: lbc, ubc                    
    integer, intent(in) :: num_snowc                   
    integer, intent(in) :: filter_snowc(ubc-lbc+1)     
    integer, intent(in) :: num_nosnowc                 
    integer, intent(in) :: filter_nosnowc(ubc-lbc+1)   

    integer , intent(in) :: snl(1)              
    logical , intent(in) :: do_capsnow(1)       
    real(r8), intent(in) :: qflx_snomelt(1)     
    real(r8), intent(in) :: qflx_rain_grnd(1)   
    real(r8), intent(in) :: qflx_sub_snow(1)    
    real(r8), intent(in) :: qflx_evap_grnd(1)   
    real(r8), intent(in) :: qflx_dew_snow(1)    
    real(r8), intent(in) :: qflx_dew_grnd(1)    
    real(r8), intent(in) :: dz(1,-nlevsnow+1:nlevsoil)             




    real(r8), intent(inout) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)     
    real(r8), intent(inout) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)     



    real(r8), intent(out) :: qflx_top_soil(1)     




    integer  :: c, j, fc                           
    real(r8) :: qin(lbc:ubc)                       
    real(r8) :: qout(lbc:ubc)                      
    real(r8) :: wgdif                              
    real(r8) :: vol_liq(lbc:ubc,-nlevsnow+1:0)      
    real(r8) :: vol_ice(lbc:ubc,-nlevsnow+1:0)      
    real(r8) :: eff_porosity(lbc:ubc,-nlevsnow+1:0) 

    
    

!dir$ concurrent

    do fc = 1,num_snowc
       c = filter_snowc(fc)
       if (do_capsnow(c)) then
          wgdif = h2osoi_ice(c,snl(c)+1) - qflx_sub_snow(c)*dtime
          h2osoi_ice(c,snl(c)+1) = wgdif
          if (wgdif < 0.) then
             h2osoi_ice(c,snl(c)+1) = 0.
             h2osoi_liq(c,snl(c)+1) = h2osoi_liq(c,snl(c)+1) + wgdif
          end if
          h2osoi_liq(c,snl(c)+1) = h2osoi_liq(c,snl(c)+1) - qflx_evap_grnd(c) * dtime
       else
          wgdif = h2osoi_ice(c,snl(c)+1) + (qflx_dew_snow(c) - qflx_sub_snow(c)) * dtime
          h2osoi_ice(c,snl(c)+1) = wgdif
          if (wgdif < 0.) then
             h2osoi_ice(c,snl(c)+1) = 0.
             h2osoi_liq(c,snl(c)+1) = h2osoi_liq(c,snl(c)+1) + wgdif
          end if
          h2osoi_liq(c,snl(c)+1) = h2osoi_liq(c,snl(c)+1) +  &
               (qflx_rain_grnd(c) + qflx_dew_grnd(c) - qflx_evap_grnd(c)) * dtime
       end if
       h2osoi_liq(c,snl(c)+1) = max(0._r8, h2osoi_liq(c,snl(c)+1))
    end do

    

    do j = -nlevsnow+1, 0
!dir$ concurrent

       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then
             vol_ice(c,j) = min(1._r8, h2osoi_ice(c,j)/(dz(c,j)*denice))
             eff_porosity(c,j) = 1. - vol_ice(c,j)
             vol_liq(c,j) = min(eff_porosity(c,j),h2osoi_liq(c,j)/(dz(c,j)*denh2o))
          end if
       end do
    end do

    
    
    
    
    

    

    qin(:) = 0._r8

    do j = -nlevsnow+1, 0
!dir$ concurrent

       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then
             h2osoi_liq(c,j) = h2osoi_liq(c,j) + qin(c)
             if (j <= -1) then
                
                if (eff_porosity(c,j) < wimp .OR. eff_porosity(c,j+1) < wimp) then
                   qout(c) = 0._r8
                else
                   qout(c) = max(0._r8,(vol_liq(c,j)-ssi*eff_porosity(c,j))*dz(c,j))
                   qout(c) = min(qout(c),(1.-vol_ice(c,j+1)-vol_liq(c,j+1))*dz(c,j+1))
                end if
             else
                qout(c) = max(0._r8,(vol_liq(c,j) - ssi*eff_porosity(c,j))*dz(c,j))
             end if
             qout(c) = qout(c)*1000.
             h2osoi_liq(c,j) = h2osoi_liq(c,j) - qout(c)
             qin(c) = qout(c)
          end if
       end do
    end do

!dir$ concurrent

    do fc = 1, num_snowc
       c = filter_snowc(fc)
       
       qflx_top_soil(c) = qout(c) / dtime
    end do

!dir$ concurrent

    do fc = 1, num_nosnowc
       c = filter_nosnowc(fc)
       qflx_top_soil(c) = qflx_rain_grnd(c) + qflx_snomelt(c)
    end do

  end subroutine SnowWater

  subroutine SnowCompaction(lbc, ubc, num_snowc, filter_snowc,   &
                           snl,imelt,frac_iceold,t_soisno,                  &
                           h2osoi_ice,h2osoi_liq,                           &
                           dz)                                               





















  


    implicit none


    integer, intent(in) :: lbc, ubc                
    integer, intent(in) :: num_snowc               
    integer, intent(in) :: filter_snowc(ubc-lbc+1) 
    integer,  intent(in) :: snl(1)             
    integer,  intent(in) :: imelt(1,-nlevsnow+1:nlevsoil)        
    real(r8), intent(in) :: frac_iceold(1,-nlevsnow+1:nlevsoil)  
    real(r8), intent(in) :: t_soisno(1,-nlevsnow+1:nlevsoil)     
    real(r8), intent(in) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)   
    real(r8), intent(in) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)   



    real(r8), intent(inout) :: dz(1,-nlevsnow+1:nlevsoil)           



    integer :: j, c, fc                   
    real(r8), parameter :: c2 = 23.e-3    
    real(r8), parameter :: c3 = 2.777e-6  
    real(r8), parameter :: c4 = 0.04      
    real(r8), parameter :: c5 = 2.0       
    real(r8), parameter :: dm = 100.0     
    real(r8), parameter :: eta0 = 9.e+5   
    real(r8) :: burden(lbc:ubc) 
    real(r8) :: ddz1   
    real(r8) :: ddz2   
    real(r8) :: ddz3   
    real(r8) :: dexpf  
    real(r8) :: fi     
    real(r8) :: td     
    real(r8) :: pdzdtc 
    real(r8) :: void   
    real(r8) :: wx     
    real(r8) :: bi     




    

    burden(:) = 0._r8

    do j = -nlevsnow+1, 0
!dir$ concurrent

       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then

             wx = h2osoi_ice(c,j) + h2osoi_liq(c,j)
             void = 1. - (h2osoi_ice(c,j)/denice + h2osoi_liq(c,j)/denh2o) / dz(c,j)

             
             if (void > 0.001 .and. h2osoi_ice(c,j) > .1) then
                bi = h2osoi_ice(c,j) / dz(c,j)
                fi = h2osoi_ice(c,j) / wx
                td = tfrz-t_soisno(c,j)
                dexpf = exp(-c4*td)

                

                ddz1 = -c3*dexpf
                if (bi > dm) ddz1 = ddz1*exp(-46.0e-3*(bi-dm))

                

                if (h2osoi_liq(c,j) > 0.01*dz(c,j)) ddz1=ddz1*c5

                

                ddz2 = -burden(c)*exp(-0.08*td - c2*bi)/eta0

                

                if (imelt(c,j) == 1) then
                   ddz3 = - 1./dtime * max(0._r8,(frac_iceold(c,j) - fi)/frac_iceold(c,j))
                else
                   ddz3 = 0._r8
                end if

                

                pdzdtc = ddz1 + ddz2 + ddz3

                

                dz(c,j) = dz(c,j) * (1.+pdzdtc*dtime)
             end if

             

             burden(c) = burden(c) + wx

          end if
       end do
    end do

  end subroutine SnowCompaction

  subroutine CombineSnowLayers(lbc, ubc,                            & 
                              num_snowc, filter_snowc, & 
                              snl,h2osno,snowdp,dz,zi,             & 
                              t_soisno,h2osoi_ice,h2osoi_liq,      & 
                              z)  















  


    implicit none

    integer, intent(in)    :: lbc, ubc                    
   
   


    integer, intent(inout) :: num_snowc                   
    integer, intent(inout) :: filter_snowc(ubc-lbc+1)     
    integer , intent(inout) :: snl(1)            
    real(r8), intent(inout) :: h2osno(1)         
    real(r8), intent(inout) :: snowdp(1)         
    real(r8), intent(inout) :: dz(1,-nlevsnow+1:nlevsoil)           
    real(r8), intent(inout) :: zi(1,-nlevsnow+0:nlevsoil)           
    real(r8), intent(inout) :: t_soisno(1,-nlevsnow+1:nlevsoil)     
    real(r8), intent(inout) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)   
    real(r8), intent(inout) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)   



    real(r8), intent(out) :: z(1,-nlevsnow+1:nlevsoil)            





    integer :: c, fc                 
    integer :: i,k                   
    integer :: j,l                   
    integer :: msn_old(lbc:ubc)      
    integer :: mssi(lbc:ubc)         
    integer :: neibor                
    real(r8):: zwice(lbc:ubc)        
    real(r8):: zwliq (lbc:ubc)       
    real(r8):: dzmin(5)              

    data dzmin /0.010, 0.015, 0.025, 0.055, 0.115/


    
    

!dir$ concurrent

    do fc = 1, num_snowc
       c = filter_snowc(fc)
       msn_old(c) = snl(c)
    end do

    

    do fc = 1, num_snowc
       c = filter_snowc(fc)
   
       do j = msn_old(c)+1,0
          if (h2osoi_ice(c,j) <= .1) then
           
           
           
           
             h2osoi_liq(c,j+1) = h2osoi_liq(c,j+1) + h2osoi_liq(c,j)
             h2osoi_ice(c,j+1) = h2osoi_ice(c,j+1) + h2osoi_ice(c,j)
           

             
             if (j > snl(c)+1 .and. snl(c) < -1) then
                do i = j, snl(c)+2, -1
                   t_soisno(c,i)   = t_soisno(c,i-1)
                   h2osoi_liq(c,i) = h2osoi_liq(c,i-1)
                   h2osoi_ice(c,i) = h2osoi_ice(c,i-1)
                   dz(c,i)         = dz(c,i-1)
                end do
             end if
             snl(c) = snl(c) + 1
          end if
       end do
    end do

!dir$ concurrent

    do fc = 1, num_snowc
       c = filter_snowc(fc)
       h2osno(c) = 0._r8
       snowdp(c) = 0._r8
       zwice(c)  = 0._r8
       zwliq(c)  = 0._r8
    end do

    do j = -nlevsnow+1,0
!dir$ concurrent

       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then
             h2osno(c) = h2osno(c) + h2osoi_ice(c,j) + h2osoi_liq(c,j)
             snowdp(c) = snowdp(c) + dz(c,j)
             zwice(c)  = zwice(c) + h2osoi_ice(c,j)
             zwliq(c)  = zwliq(c) + h2osoi_liq(c,j)
          end if
       end do
    end do

    
    

!dir$ concurrent

    do fc = 1, num_snowc
       c = filter_snowc(fc)
      
       if (snowdp(c) < 0.01 .and. snowdp(c) > 0.) then
          snl(c) = 0
          h2osno(c) = zwice(c)
          if (h2osno(c) <= 0.) snowdp(c) = 0._r8
      
       end if
    end do

    
    

    do fc = 1, num_snowc
       c = filter_snowc(fc)

       

       if (snl(c) < -1) then

          msn_old(c) = snl(c)
          mssi(c) = 1

          do i = msn_old(c)+1,0
             if (dz(c,i) < dzmin(mssi(c))) then

                if (i == snl(c)+1) then
                   
                   neibor = i + 1
                else if (i == 0) then
                   
                   neibor = i - 1
                else
                   
                   neibor = i + 1
                   if ((dz(c,i-1)+dz(c,i)) < (dz(c,i+1)+dz(c,i))) neibor = i-1
                end if

                
                if (neibor > i) then
                   j = neibor
                   l = i
                else
                   j = i
                   l = neibor
                end if

                call Combo (dz(c,j), h2osoi_liq(c,j), h2osoi_ice(c,j), &
                   t_soisno(c,j), dz(c,l), h2osoi_liq(c,l), h2osoi_ice(c,l), t_soisno(c,l) )

                
                if (j-1 > snl(c)+1) then
                   do k = j-1, snl(c)+2, -1
                      t_soisno(c,k) = t_soisno(c,k-1)
                      h2osoi_ice(c,k) = h2osoi_ice(c,k-1)
                      h2osoi_liq(c,k) = h2osoi_liq(c,k-1)
                      dz(c,k) = dz(c,k-1)
                   end do
                end if

                
                snl(c) = snl(c) + 1
                if (snl(c) >= -1) EXIT

             else

                
                mssi(c) = mssi(c) + 1

             end if
          end do

       end if

    end do

    

    do j = 0, -nlevsnow+1, -1
!dir$ concurrent

       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c) + 1) then
             z(c,j) = zi(c,j) - 0.5*dz(c,j)
             zi(c,j-1) = zi(c,j) - dz(c,j)
          end if
       end do
    end do

  end subroutine CombineSnowLayers

  subroutine DivideSnowLayers(lbc, ubc,                             & 
                             num_snowc, filter_snowc,  & 
                             snl,dz,zi,t_soisno,                   & 
                             h2osoi_ice,h2osoi_liq,                & 
                             z)  














 


    implicit none


    integer, intent(in)    :: lbc, ubc                    



    integer, intent(inout) :: num_snowc                   
    integer, intent(inout) :: filter_snowc(ubc-lbc+1)     
    integer , intent(inout) :: snl(1)            
    real(r8), intent(inout) :: dz(1,-nlevsnow+1:nlevsoil)           
    real(r8), intent(inout) :: zi(1,-nlevsnow+0:nlevsoil)           
    real(r8), intent(inout) :: t_soisno(1,-nlevsnow+1:nlevsoil)     
    real(r8), intent(inout) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)   
    real(r8), intent(inout) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)   



    real(r8), intent(out) :: z(1,-nlevsnow+1:nlevsoil)            





    integer  :: j, c, fc               
    real(r8) :: drr                    
    integer  :: msno                   
    real(r8) :: dzsno(lbc:ubc,nlevsnow) 
    real(r8) :: swice(lbc:ubc,nlevsnow) 
    real(r8) :: swliq(lbc:ubc,nlevsnow) 
    real(r8) :: tsno(lbc:ubc ,nlevsnow) 
    real(r8) :: zwice                  
    real(r8) :: zwliq                  
    real(r8) :: propor                 


    
    

    do j = 1,nlevsnow
!dir$ concurrent

       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j <= abs(snl(c))) then
             dzsno(c,j) = dz(c,j+snl(c))
             swice(c,j) = h2osoi_ice(c,j+snl(c))
             swliq(c,j) = h2osoi_liq(c,j+snl(c))
             tsno(c,j)  = t_soisno(c,j+snl(c))
          end if
       end do
    end do

!dir$ concurrent

    do fc = 1, num_snowc
       c = filter_snowc(fc)

       msno = abs(snl(c))

       if (msno == 1) then
          
          if (dzsno(c,1) > 0.03) then
             msno = 2
             dzsno(c,1) = dzsno(c,1)/2.
             swice(c,1) = swice(c,1)/2.
             swliq(c,1) = swliq(c,1)/2.
             dzsno(c,2) = dzsno(c,1)
             swice(c,2) = swice(c,1)
             swliq(c,2) = swliq(c,1)
             tsno(c,2)  = tsno(c,1)
          end if
       end if

       if (msno > 1) then
          if (dzsno(c,1) > 0.02) then
             drr = dzsno(c,1) - 0.02
             propor = drr/dzsno(c,1)
             zwice = propor*swice(c,1)
             zwliq = propor*swliq(c,1)
             propor = 0.02/dzsno(c,1)
             swice(c,1) = propor*swice(c,1)
             swliq(c,1) = propor*swliq(c,1)
             dzsno(c,1) = 0.02

             call Combo (dzsno(c,2), swliq(c,2), swice(c,2), tsno(c,2), drr, &
                  zwliq, zwice, tsno(c,1))

             
             if (msno <= 2 .and. dzsno(c,2) > 0.07) then
                msno = 3
                dzsno(c,2) = dzsno(c,2)/2.
                swice(c,2) = swice(c,2)/2.
                swliq(c,2) = swliq(c,2)/2.
                dzsno(c,3) = dzsno(c,2)
                swice(c,3) = swice(c,2)
                swliq(c,3) = swliq(c,2)
                tsno(c,3)  = tsno(c,2)
             end if
          end if
       end if

       if (msno > 2) then
          if (dzsno(c,2) > 0.05) then
             drr = dzsno(c,2) - 0.05
             propor = drr/dzsno(c,2)
             zwice = propor*swice(c,2)
             zwliq = propor*swliq(c,2)
             propor = 0.05/dzsno(c,2)
             swice(c,2) = propor*swice(c,2)
             swliq(c,2) = propor*swliq(c,2)
             dzsno(c,2) = 0.05

             call Combo (dzsno(c,3), swliq(c,3), swice(c,3), tsno(c,3), drr, &
                  zwliq, zwice, tsno(c,2))

             
             if (msno <= 3 .and. dzsno(c,3) > 0.18) then
                msno =  4
                dzsno(c,3) = dzsno(c,3)/2.
                swice(c,3) = swice(c,3)/2.
                swliq(c,3) = swliq(c,3)/2.
                dzsno(c,4) = dzsno(c,3)
                swice(c,4) = swice(c,3)
                swliq(c,4) = swliq(c,3)
                tsno(c,4)  = tsno(c,3)
             end if
          end if
       end if

       if (msno > 3) then
          if (dzsno(c,3) > 0.11) then
             drr = dzsno(c,3) - 0.11
             propor = drr/dzsno(c,3)
             zwice = propor*swice(c,3)
             zwliq = propor*swliq(c,3)
             propor = 0.11/dzsno(c,3)
             swice(c,3) = propor*swice(c,3)
             swliq(c,3) = propor*swliq(c,3)
             dzsno(c,3) = 0.11

             call Combo (dzsno(c,4), swliq(c,4), swice(c,4), tsno(c,4), drr, &
                  zwliq, zwice, tsno(c,3))

             
             if (msno <= 4 .and. dzsno(c,4) > 0.41) then
                msno = 5
                dzsno(c,4) = dzsno(c,4)/2.
                swice(c,4) = swice(c,4)/2.
                swliq(c,4) = swliq(c,4)/2.
                dzsno(c,5) = dzsno(c,4)
                swice(c,5) = swice(c,4)
                swliq(c,5) = swliq(c,4)
                tsno(c,5)  = tsno(c,4)
             end if
          end if
       end if

       if (msno > 4) then
          if (dzsno(c,4) > 0.23) then
             drr = dzsno(c,4) - 0.23
             propor = drr/dzsno(c,4)
             zwice = propor*swice(c,4)
             zwliq = propor*swliq(c,4)
             propor = 0.23/dzsno(c,4)
             swice(c,4) = propor*swice(c,4)
             swliq(c,4) = propor*swliq(c,4)
             dzsno(c,4) = 0.23

             call Combo (dzsno(c,5), swliq(c,5), swice(c,5), tsno(c,5), drr, &
                  zwliq, zwice, tsno(c,4))
          end if
       end if

       snl(c) = -msno

    end do

    do j = -nlevsnow+1,0
!dir$ concurrent

       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then
             dz(c,j)         = dzsno(c,j-snl(c))
             h2osoi_ice(c,j) = swice(c,j-snl(c))
             h2osoi_liq(c,j) = swliq(c,j-snl(c))
             t_soisno(c,j)   = tsno(c,j-snl(c))
          end if
       end do
    end do

    do j = 0, -nlevsnow+1, -1
!dir$ concurrent

       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then
             z(c,j)    = zi(c,j) - 0.5*dz(c,j)
             zi(c,j-1) = zi(c,j) - dz(c,j)
          end if
       end do
    end do

  end subroutine DivideSnowLayers

  subroutine Combo(dz,  wliq,  wice, t, dz2, wliq2, wice2, t2)











    implicit none
    real(r8), intent(in)    :: dz2   
    real(r8), intent(in)    :: wliq2 
    real(r8), intent(in)    :: wice2 
    real(r8), intent(in)    :: t2    
    real(r8), intent(inout) :: dz    
    real(r8), intent(inout) :: wliq  
    real(r8), intent(inout) :: wice  
    real(r8), intent(inout) :: t     













    real(r8) :: dzc   
    real(r8) :: wliqc 
    real(r8) :: wicec 
    real(r8) :: tc    
    real(r8) :: h     
    real(r8) :: h2    
    real(r8) :: hc    


    dzc = dz+dz2
    wicec = (wice+wice2)
    wliqc = (wliq+wliq2)
    h = (cpice*wice+cpliq*wliq) * (t-tfrz)+hfus*wliq
    h2= (cpice*wice2+cpliq*wliq2) * (t2-tfrz)+hfus*wliq2

    hc = h + h2
    if(hc < 0.)then
       tc = tfrz + hc/(cpice*wicec + cpliq*wliqc)
    else if (hc.le.hfus*wliqc) then
       tc = tfrz
    else
       tc = tfrz + (hc - hfus*wliqc) / (cpice*wicec + cpliq*wliqc)
    end if

    dz = dzc
    wice = wicec
    wliq = wliqc
    t = tc

  end subroutine Combo

  subroutine BuildSnowFilter(lbc, ubc, num_nolakec, filter_nolakec,snl, & 
                             num_snowc, filter_snowc, &                   
                             num_nosnowc, filter_nosnowc)                 








    implicit none
    integer, intent(in)  :: lbc, ubc                    
    integer, intent(in)  :: num_nolakec                 
    integer, intent(in)  :: filter_nolakec(ubc-lbc+1)   
    integer, intent(in)  :: snl(1)                        
    integer, intent(out) :: num_snowc                   
    integer, intent(out) :: filter_snowc(ubc-lbc+1)     
    integer, intent(out) :: num_nosnowc                 
    integer, intent(out) :: filter_nosnowc(ubc-lbc+1)   














    integer  :: fc, c



    

    num_snowc = 0
    num_nosnowc = 0
    do fc = 1, num_nolakec
       c = filter_nolakec(fc)
       if (snl(c) < 0) then
          num_snowc = num_snowc + 1
          filter_snowc(num_snowc) = c
       else
          num_nosnowc = num_nosnowc + 1
          filter_nosnowc(num_nosnowc) = c
       end if
    end do

  end subroutine BuildSnowFilter



subroutine FrictionVelocity(pgridcell,forc_hgt,forc_hgt_u,        & 
                             forc_hgt_t,forc_hgt_q,                  & 
                             lbp, ubp, fn, filterp,                  & 
                             displa, z0m, z0h, z0q,                  & 
                             obu, iter, ur, um,                      & 
                             ustar,temp1, temp2, temp12m, temp22m,   & 
                             u10,fv,                                 & 
                             fm)  


















  
   


   implicit none



   integer , intent(in) :: pgridcell(1)   
   real(r8), intent(in) :: forc_hgt(1)    
   real(r8), intent(in) :: forc_hgt_u(1)  
   real(r8), intent(in) :: forc_hgt_t(1)  
   real(r8), intent(in) :: forc_hgt_q(1)  
   integer , intent(in)  :: lbp, ubp         
   integer , intent(in)  :: fn               
   integer , intent(in)  :: filterp(fn)      
   real(r8), intent(in)  :: displa(lbp:ubp)  
   real(r8), intent(in)  :: z0m(lbp:ubp)     
   real(r8), intent(in)  :: z0h(lbp:ubp)     
   real(r8), intent(in)  :: z0q(lbp:ubp)     
   real(r8), intent(in)  :: obu(lbp:ubp)     
   integer,  intent(in)  :: iter             
   real(r8), intent(in)  :: ur(lbp:ubp)      
   real(r8), intent(in)  :: um(lbp:ubp)      



   real(r8), intent(out) :: ustar(lbp:ubp)   
   real(r8), intent(out) :: temp1(lbp:ubp)   
   real(r8), intent(out) :: temp12m(lbp:ubp) 
   real(r8), intent(out) :: temp2(lbp:ubp)   
   real(r8), intent(out) :: temp22m(lbp:ubp) 
   real(r8), intent(out) :: u10(1)         
   real(r8), intent(out) :: fv(1)          


   real(r8), intent(inout) :: fm(lbp:ubp)    



   real(r8), parameter :: zetam = 1.574_r8 
   real(r8), parameter :: zetat = 0.465_r8 
   integer :: f                         
   integer :: p                         
   integer :: g                         
   real(r8):: zldis(lbp:ubp)            
   real(r8):: zeta(lbp:ubp)             



   


!dir$ concurrent

   do f = 1, fn
      p = filterp(f)
      g = pgridcell(p)

      

      zldis(p) = forc_hgt_u(g)-displa(p)
      zeta(p) = zldis(p)/obu(p)
      if (zeta(p) < -zetam) then
         ustar(p) = vkc*um(p)/(log(-zetam*obu(p)/z0m(p))&
              - StabilityFunc1(-zetam) &
              + StabilityFunc1(z0m(p)/obu(p)) &
              + 1.14_r8*((-zeta(p))**0.333_r8-(zetam)**0.333_r8))
      else if (zeta(p) < 0._r8) then
         ustar(p) = vkc*um(p)/(log(zldis(p)/z0m(p))&
              - StabilityFunc1(zeta(p))&
              + StabilityFunc1(z0m(p)/obu(p)))
      else if (zeta(p) <=  1._r8) then
         ustar(p) = vkc*um(p)/(log(zldis(p)/z0m(p)) + 5._r8*zeta(p) -5._r8*z0m(p)/obu(p))
      else
         ustar(p) = vkc*um(p)/(log(obu(p)/z0m(p))+5._r8-5._r8*z0m(p)/obu(p) &
              +(5._r8*log(zeta(p))+zeta(p)-1._r8))
      end if

      

      zldis(p) = forc_hgt_t(g)-displa(p)
      zeta(p) = zldis(p)/obu(p)
      if (zeta(p) < -zetat) then
         temp1(p) = vkc/(log(-zetat*obu(p)/z0h(p))&
              - StabilityFunc2(-zetat) &
              + StabilityFunc2(z0h(p)/obu(p)) &
              + 0.8_r8*((zetat)**(-0.333_r8)-(-zeta(p))**(-0.333_r8)))
      else if (zeta(p) < 0._r8) then
         temp1(p) = vkc/(log(zldis(p)/z0h(p)) &
              - StabilityFunc2(zeta(p)) &
              + StabilityFunc2(z0h(p)/obu(p)))
      else if (zeta(p) <=  1._r8) then
         temp1(p) = vkc/(log(zldis(p)/z0h(p)) + 5._r8*zeta(p) - 5._r8*z0h(p)/obu(p))
      else
         temp1(p) = vkc/(log(obu(p)/z0h(p)) + 5._r8 - 5._r8*z0h(p)/obu(p) &
              + (5._r8*log(zeta(p))+zeta(p)-1._r8))
      end if

      

      if (forc_hgt_q(g) == forc_hgt_t(g) .and. z0q(p) == z0h(p)) then
         temp2(p) = temp1(p)
      else
         zldis(p) = forc_hgt_q(g)-displa(p)
         zeta(p) = zldis(p)/obu(p)
         if (zeta(p) < -zetat) then
            temp2(p) = vkc/(log(-zetat*obu(p)/z0q(p)) &
                 - StabilityFunc2(-zetat) &
                 + StabilityFunc2(z0q(p)/obu(p)) &
                 + 0.8_r8*((zetat)**(-0.333_r8)-(-zeta(p))**(-0.333_r8)))
         else if (zeta(p) < 0._r8) then
            temp2(p) = vkc/(log(zldis(p)/z0q(p)) &
                 - StabilityFunc2(zeta(p)) &
                 + StabilityFunc2(z0q(p)/obu(p)))
         else if (zeta(p) <=  1._r8) then
            temp2(p) = vkc/(log(zldis(p)/z0q(p)) + 5._r8*zeta(p)-5._r8*z0q(p)/obu(p))
         else
            temp2(p) = vkc/(log(obu(p)/z0q(p)) + 5._r8 - 5._r8*z0q(p)/obu(p) &
                 + (5._r8*log(zeta(p))+zeta(p)-1._r8))
         end if
      endif

      

      zldis(p) = 2.0_r8 + z0h(p)
      zeta(p) = zldis(p)/obu(p)
      if (zeta(p) < -zetat) then
         temp12m(p) = vkc/(log(-zetat*obu(p)/z0h(p))&
              - StabilityFunc2(-zetat) &
              + StabilityFunc2(z0h(p)/obu(p)) &
              + 0.8_r8*((zetat)**(-0.333_r8)-(-zeta(p))**(-0.333_r8)))
      else if (zeta(p) < 0._r8) then
         temp12m(p) = vkc/(log(zldis(p)/z0h(p)) &
              - StabilityFunc2(zeta(p))  &
              + StabilityFunc2(z0h(p)/obu(p)))
      else if (zeta(p) <=  1._r8) then
         temp12m(p) = vkc/(log(zldis(p)/z0h(p)) + 5._r8*zeta(p) - 5._r8*z0h(p)/obu(p))
      else
         temp12m(p) = vkc/(log(obu(p)/z0h(p)) + 5._r8 - 5._r8*z0h(p)/obu(p) &
              + (5._r8*log(zeta(p))+zeta(p)-1._r8))
      end if

      

      if (z0q(p) == z0h(p)) then
         temp22m(p) = temp12m(p)
      else
         zldis(p) = 2.0_r8 + z0q(p)
         zeta(p) = zldis(p)/obu(p)
         if (zeta(p) < -zetat) then
            temp22m(p) = vkc/(log(-zetat*obu(p)/z0q(p)) - &
                 StabilityFunc2(-zetat) + StabilityFunc2(z0q(p)/obu(p)) &
                 + 0.8_r8*((zetat)**(-0.333_r8)-(-zeta(p))**(-0.333_r8)))
         else if (zeta(p) < 0._r8) then
            temp22m(p) = vkc/(log(zldis(p)/z0q(p)) - &
                 StabilityFunc2(zeta(p))+StabilityFunc2(z0q(p)/obu(p)))
         else if (zeta(p) <=  1._r8) then
            temp22m(p) = vkc/(log(zldis(p)/z0q(p)) + 5._r8*zeta(p)-5._r8*z0q(p)/obu(p))
         else
            temp22m(p) = vkc/(log(obu(p)/z0q(p)) + 5._r8 - 5._r8*z0q(p)/obu(p) &
                 + (5._r8*log(zeta(p))+zeta(p)-1._r8))
         end if
      end if


   end do



   end subroutine FrictionVelocity




   real(r8) function StabilityFunc1(zeta)









      implicit none
      real(r8), intent(in) :: zeta  











      real(r8) :: chik, chik2


      chik2 = sqrt(1._r8-16._r8*zeta)
      chik = sqrt(chik2)
      StabilityFunc1 = 2._r8*log((1._r8+chik)*0.5_r8) &

           + log((1._r8+chik2)*0.5_r8)-2._r8*atan(chik)+pie*0.5_r8

    end function StabilityFunc1







   real(r8) function StabilityFunc2(zeta)









     implicit none
     real(r8), intent(in) :: zeta  











     real(r8) :: chik2


     chik2 = sqrt(1._r8-16._r8*zeta)
     StabilityFunc2 = 2._r8*log((1._r8+chik2)*0.5_r8)

   end function StabilityFunc2







  subroutine MoninObukIni (ur, thv, dthv, zldis, z0m, um, obu)











    implicit none
    real(r8), intent(in)  :: ur    
    real(r8), intent(in)  :: thv   
    real(r8), intent(in)  :: dthv  
    real(r8), intent(in)  :: zldis 
    real(r8), intent(in)  :: z0m   
    real(r8), intent(out) :: um    
    real(r8), intent(out) :: obu   














    real(r8) :: wc    
    real(r8) :: rib   
    real(r8) :: zeta  
    real(r8) :: ustar 


    

    ustar=0.06_r8
    wc=0.5_r8
    if (dthv >= 0._r8) then
       um=max(ur,0.1_r8)
    else
       um=sqrt(ur*ur+wc*wc)
    endif

    rib=grav*zldis*dthv/(thv*um*um)

    if (rib >= 0._r8) then      
       zeta = rib*log(zldis/z0m)/(1._r8-5._r8*min(rib,0.19_r8))
       zeta = min(2._r8,max(zeta,0.01_r8 ))
    else                     
       zeta=rib*log(zldis/z0m)
       zeta = max(-100._r8,min(zeta,-0.01_r8 ))
    endif

    obu=zldis/zeta

  end subroutine MoninObukIni

subroutine LakeDebug( str ) 
 
  IMPLICIT NONE
  CHARACTER*(*), str
 
  CALL wrf_debug( 0 , TRIM(str) )
 
end subroutine LakeDebug

 SUBROUTINE lakeini(IVGTYP,         ISLTYP,          HT,              SNOW,           & 
                    lake_min_elev,     restart,        lakedepth_default, lake_depth,     &
                    lakedepth2d,    savedtke12d,     snowdp2d,        h2osno2d,       & 
                    snl2d,          t_grnd2d,        t_lake3d,        lake_icefrac3d, &
                    z_lake3d,       dz_lake3d,       t_soisno3d,      h2osoi_ice3d,   &
                    h2osoi_liq3d,   h2osoi_vol3d,    z3d,             dz3d,           &
                    zi3d,           watsat3d,        csol3d,          tkmg3d,         &
                    iswater,        xice,            xice_threshold,  xland,   tsk,   &
                    lakemask,       lakeflag,                                         &
                    lake_depth_flag, use_lakedepth,                                   &
                    tkdry3d,        tksatu3d,        lake,            its, ite, jts, jte, &
                    ims,ime, jms,jme)






  USE module_wrf_error
  implicit none

  INTEGER , INTENT (IN) :: iswater
  REAL,     INTENT(IN)  :: xice_threshold
  REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   XICE
  REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN)::      TSK
  REAL, DIMENSION( ims:ime, jms:jme )  ,INTENT(INOUT)  :: XLAND

  REAL, DIMENSION( ims:ime , jms:jme ) ::   LAKEMASK
  INTEGER , INTENT (IN) :: lakeflag
  INTEGER , INTENT (INOUT) :: lake_depth_flag
  INTEGER , INTENT (IN) ::   use_lakedepth

  LOGICAL , INTENT(IN)      ::     restart
  INTEGER,  INTENT(IN   )   ::     ims,ime, jms,jme
  INTEGER,  INTENT(IN   )   ::     its,ite, jts,jte
  INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)       :: IVGTYP,       &
                                                              ISLTYP
  REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN)       :: HT
  REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)    :: SNOW
  real,    intent(in)                                      :: lakedepth_default,lake_min_elev

  real,    dimension(ims:ime,jms:jme ),intent(out)                        :: lakedepth2d,    &
                                                                             savedtke12d
  real,    dimension(ims:ime,jms:jme ),intent(out)                        :: snowdp2d,       &
                                                                             h2osno2d,       &
                                                                             snl2d,          &
                                                                             t_grnd2d
                                                                              
  real,    dimension( ims:ime,1:nlevlake, jms:jme ),INTENT(out)            :: t_lake3d,       &
                                                                             lake_icefrac3d, &
                                                                             z_lake3d,       &
                                                                             dz_lake3d
  real,    dimension( ims:ime,-nlevsnow+1:nlevsoil, jms:jme ),INTENT(out)   :: t_soisno3d,     &
                                                                             h2osoi_ice3d,   &
                                                                             h2osoi_liq3d,   &
                                                                             h2osoi_vol3d,   &
                                                                             z3d,            &
                                                                             dz3d
  real,    dimension( ims:ime,1:nlevsoil, jms:jme ),INTENT(out)            :: watsat3d,       &
                                                                             csol3d,         &
                                                                             tkmg3d,         &
                                                                             tkdry3d,        &
                                                                             tksatu3d
  real,    dimension( ims:ime,-nlevsnow+0:nlevsoil, jms:jme ),INTENT(out)   :: zi3d            

  LOGICAL, DIMENSION( ims:ime, jms:jme ),intent(out)                      :: lake
  REAL, OPTIONAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN)    ::  lake_depth

  real,    dimension( ims:ime,1:nlevsoil, jms:jme )   :: bsw3d,    &
                                                        bsw23d,   &
                                                        psisat3d, &
                                                        vwcsat3d, &
                                                        watdry3d, &
                                                        watopt3d, &
                                                        hksat3d,  &
                                                        sucsat3d, &
                                                        clay3d,   &
                                                        sand3d   
  integer  :: n,i,j,k,ib,lev,bottom      
  real(r8),dimension(ims:ime,jms:jme )    :: bd2d               
  real(r8),dimension(ims:ime,jms:jme )    :: tkm2d              
  real(r8),dimension(ims:ime,jms:jme )    :: xksat2d            
  real(r8),dimension(ims:ime,jms:jme )    :: depthratio2d       
  real(r8),dimension(ims:ime,jms:jme )    :: clay2d             
  real(r8),dimension(ims:ime,jms:jme )    :: sand2d             

  real(r8)                 :: scalez  = 0.025_r8   
  logical,parameter        :: arbinit = .true.
  real,parameter           :: defval  = -999.0
  integer                  :: isl
  integer                  :: numb_lak    
  character*256 :: message

  IF ( RESTART ) RETURN 

  DO j = jts,jte
  DO i = its,ite
        snowdp2d(i,j)         = snow(i,j)*0.005               
	h2osno2d(i,j)         = snow(i,j) 
  ENDDO
  ENDDO


  DO j = jts,jte
  DO i = its,ite

    lakedepth2d(i,j)             = defval
    snl2d(i,j)                   = defval
    do k = -nlevsnow+1,nlevsoil
        h2osoi_liq3d(i,k,j)      = defval
        h2osoi_ice3d(i,k,j)      = defval
	t_soisno3d(i,k,j)        = defval
        z3d(i,k,j)               = defval 
        dz3d(i,k,j)              = defval                           
    enddo
    do k = 1,nlevlake 
	t_lake3d(i,k,j)          = defval
        lake_icefrac3d(i,k,j)    = defval
        z_lake3d(i,k,j)          = defval
        dz_lake3d(i,k,j)         = defval
    enddo

  ENDDO
  ENDDO


   numb_lak = 0
       do i=its,ite
         do j=jts,jte
         IF (lakeflag.eq.0) THEN    
            if(ht(i,j)>=lake_min_elev) then 
              if ( xice(i,j).gt.xice_threshold) then   
                   ivgtyp(i,j) = iswater
                   xland(i,j) = 2. 
                   lake_icefrac3d(i,1,j) = xice(i,j)
                   xice(i,j)=0.0
               endif
            endif

            if(ivgtyp(i,j)==iswater.and.ht(i,j)>=lake_min_elev) then 
                lake(i,j)  = .true.
                lakemask(i,j) = 1
                numb_lak   = numb_lak + 1
            else 
                lake(i,j)  = .false.
                lakemask(i,j) = 0
            end if
          ELSE
            if(lakemask(i,j).eq.1) then 
                lake(i,j)  = .true.
                numb_lak   = numb_lak + 1
                if ( xice(i,j).gt.xice_threshold) then   
                   ivgtyp(i,j) = iswater
                   xland(i,j) = 2. 
                   lake_icefrac3d(i,1,j) = xice(i,j)
                   xice(i,j)=0.0
                endif
             else  
                lake(i,j)  = .false.
             endif
         ENDIF   
        end do
       end do
    write(message,*) "the total number of lake grid is :", numb_lak
    CALL wrf_message(message)



  DO j = jts,jte
  DO i = its,ite

     if ( lake(i,j) ) then





        z3d(i,:,j)             = 0.0
        dz3d(i,:,j)            = 0.0
        zi3d(i,:,j)            = 0.0
        h2osoi_liq3d(i,:,j)    = 0.0
        h2osoi_ice3d(i,:,j)    = 0.0
        lake_icefrac3d(i,:,j)  = 0.0
        h2osoi_vol3d(i,:,j)    = 0.0
        snl2d(i,j)             = 0.0
          if ( use_lakedepth.eq.1 .and.lake_depth_flag.eq.0 ) then 
          call wrf_error_fatal3("<stdin>",4613,&
'STOP: You need lake-depth information. Rerun WPS or set use_lakedepth = 0')
          end if
          if ( use_lakedepth.eq.0 .and.lake_depth_flag.eq.1 ) then 
          lake_depth_flag = 0 
          end if
        if ( lake_depth_flag.eq.1 ) then

          if (lake_depth(i,j) > 0.0) then 
            lakedepth2d(i,j)   = lake_depth(i,j)
          else
            if ( lakedepth_default  > 0.0 ) then
               lakedepth2d(i,j)   = lakedepth_default
            else 
               lakedepth2d(i,j)   = spval
            endif
          endif

        else
          if ( lakedepth_default  > 0.0 ) then
             lakedepth2d(i,j)   = lakedepth_default
          else 
             lakedepth2d(i,j)   = spval
          endif
        endif
     endif

  ENDDO
  ENDDO 

  





















  dzlak(1) = 0.1_r8
  dzlak(2) = 0.1_r8
  dzlak(3) = 0.1_r8
  dzlak(4) = 0.1_r8
  dzlak(5) = 0.1_r8
  dzlak(6) = 0.1_r8
  dzlak(7) = 0.1_r8
  dzlak(8) = 0.1_r8
  dzlak(9) = 0.1_r8
  dzlak(10)= 0.1_r8
 
  zlak(1) =  0.05_r8
  zlak(2) =  0.15_r8
  zlak(3) =  0.25_r8
  zlak(4) =  0.35_r8
  zlak(5) =  0.45_r8
  zlak(6) = 0.55_r8
  zlak(7) = 0.65_r8
  zlak(8) = 0.75_r8
  zlak(9) = 0.85_r8
  zlak(10)= 0.95_r8

   

   do j = 1, nlevsoil
      zsoi(j) = scalez*(exp(0.5_r8*(j-0.5_r8))-1._r8)    
   enddo

   dzsoi(1) = 0.5_r8*(zsoi(1)+zsoi(2))             
   do j = 2,nlevsoil-1
      dzsoi(j)= 0.5_r8*(zsoi(j+1)-zsoi(j-1))
   enddo
   dzsoi(nlevsoil) = zsoi(nlevsoil)-zsoi(nlevsoil-1)

   zisoi(0) = 0._r8
   do j = 1, nlevsoil-1
      zisoi(j) = 0.5_r8*(zsoi(j)+zsoi(j+1))         
   enddo
   zisoi(nlevsoil) = zsoi(nlevsoil) + 0.5_r8*dzsoi(nlevsoil)




  DO j = jts,jte
  DO i = its,ite
      
     if ( lake(i,j) ) then

                             
         isl = ISLTYP(i,j)   
         if (isl == 14 ) isl = isl + 1 
         do k = 1,nlevsoil
            sand3d(i,k,j)  = sand(isl)
            clay3d(i,k,j)  = clay(isl)
         enddo

         do k = 1,nlevsoil
            clay2d(i,j) = clay3d(i,k,j)
            sand2d(i,j) = sand3d(i,k,j)
            watsat3d(i,k,j) = 0.489_r8 - 0.00126_r8*sand2d(i,j)
            bd2d(i,j)    = (1._r8-watsat3d(i,k,j))*2.7e3_r8
            xksat2d(i,j) = 0.0070556_r8 *( 10._r8**(-0.884_r8+0.0153_r8*sand2d(i,j)) ) 
            tkm2d(i,j) = (8.80_r8*sand2d(i,j)+2.92_r8*clay2d(i,j))/(sand2d(i,j)+clay2d(i,j))          

            bsw3d(i,k,j) = 2.91_r8 + 0.159_r8*clay2d(i,j)
            bsw23d(i,k,j) = -(3.10_r8 + 0.157_r8*clay2d(i,j) - 0.003_r8*sand2d(i,j))
            psisat3d(i,k,j) = -(exp((1.54_r8 - 0.0095_r8*sand2d(i,j) + 0.0063_r8*(100.0_r8-sand2d(i,j)  &
                              -clay2d(i,j)))*log(10.0_r8))*9.8e-5_r8)
            vwcsat3d(i,k,j) = (50.5_r8 - 0.142_r8*sand2d(i,j) - 0.037_r8*clay2d(i,j))/100.0_r8
            hksat3d(i,k,j) = xksat2d(i,j)
            sucsat3d(i,k,j) = 10._r8 * ( 10._r8**(1.88_r8-0.0131_r8*sand2d(i,j)) )
            tkmg3d(i,k,j) = tkm2d(i,j) ** (1._r8- watsat3d(i,k,j))
            tksatu3d(i,k,j) = tkmg3d(i,k,j)*0.57_r8**watsat3d(i,k,j)
            tkdry3d(i,k,j) = (0.135_r8*bd2d(i,j) + 64.7_r8) / (2.7e3_r8 - 0.947_r8*bd2d(i,j))
            csol3d(i,k,j) = (2.128_r8*sand2d(i,j)+2.385_r8*clay2d(i,j)) / (sand2d(i,j)+clay2d(i,j))*1.e6_r8  
            watdry3d(i,k,j) = watsat3d(i,k,j) * (316230._r8/sucsat3d(i,k,j)) ** (-1._r8/bsw3d(i,k,j))
            watopt3d(i,k,j) = watsat3d(i,k,j) * (158490._r8/sucsat3d(i,k,j)) ** (-1._r8/bsw3d(i,k,j))
         end do
         if (lakedepth2d(i,j) == spval) then
            lakedepth2d(i,j) = zlak(nlevlake) + 0.5_r8*dzlak(nlevlake)
            z_lake3d(i,1:nlevlake,j) = zlak(1:nlevlake)
            dz_lake3d(i,1:nlevlake,j) = dzlak(1:nlevlake)
         else
            depthratio2d(i,j) = lakedepth2d(i,j) / (zlak(nlevlake) + 0.5_r8*dzlak(nlevlake)) 
            z_lake3d(i,1,j) = zlak(1)
            dz_lake3d(i,1,j) = dzlak(1)
            dz_lake3d(i,2:nlevlake,j) = dzlak(2:nlevlake)*depthratio2d(i,j)
            z_lake3d(i,2:nlevlake,j) = zlak(2:nlevlake)*depthratio2d(i,j) + dz_lake3d(i,1,j)*(1._r8 - depthratio2d(i,j))
         end if

	t_soisno3d(i,1,j)      = tsk(i,j)
        t_lake3d(i,1,j)        = tsk(i,j)
        t_grnd2d(i,j)          = 277.0
        do k = 2, nlevlake
        if(z_lake3d(i,k,j).le.depth_c) then 
         t_soisno3d(i,k,j)=tsk(i,j)+(277.0-tsk(i,j))/depth_c*z_lake3d(i,k,j)
         t_lake3d(i,k,j)=tsk(i,j)+(277.0-tsk(i,j))/depth_c*z_lake3d(i,k,j)
        else
	t_soisno3d(i,k,j)      = 277.0
        t_lake3d(i,k,j)        = 277.0
        end if 
        enddo

         z3d(i,1:nlevsoil,j) = zsoi(1:nlevsoil)
         zi3d(i,0:nlevsoil,j) = zisoi(0:nlevsoil)
         dz3d(i,1:nlevsoil,j) = dzsoi(1:nlevsoil)
         savedtke12d(i,j) = tkwat 
   

        if (snowdp2d(i,j) < 0.01_r8) then
           snl2d(i,j) = 0
           dz3d(i,-nlevsnow+1:0,j) = 0._r8
           z3d (i,-nlevsnow+1:0,j) = 0._r8
           zi3d(i,-nlevsnow+0:0,j) = 0._r8
        else
           if ((snowdp2d(i,j) >= 0.01_r8) .and. (snowdp2d(i,j) <= 0.03_r8)) then
              snl2d(i,j) = -1
              dz3d(i,0,j)  = snowdp2d(i,j)
           else if ((snowdp2d(i,j) > 0.03_r8) .and. (snowdp2d(i,j) <= 0.04_r8)) then
              snl2d(i,j) = -2
              dz3d(i,-1,j) = snowdp2d(i,j)/2._r8
              dz3d(i, 0,j) = dz3d(i,-1,j)
           else if ((snowdp2d(i,j) > 0.04_r8) .and. (snowdp2d(i,j) <= 0.07_r8)) then
              snl2d(i,j) = -2
              dz3d(i,-1,j) = 0.02_r8
              dz3d(i, 0,j) = snowdp2d(i,j) - dz3d(i,-1,j)
           else if ((snowdp2d(i,j) > 0.07_r8) .and. (snowdp2d(i,j) <= 0.12_r8)) then
              snl2d(i,j) = -3
              dz3d(i,-2,j) = 0.02_r8
              dz3d(i,-1,j) = (snowdp2d(i,j) - 0.02_r8)/2._r8
              dz3d(i, 0,j) = dz3d(i,-1,j)
           else if ((snowdp2d(i,j) > 0.12_r8) .and. (snowdp2d(i,j) <= 0.18_r8)) then
              snl2d(i,j) = -3
              dz3d(i,-2,j) = 0.02_r8
              dz3d(i,-1,j) = 0.05_r8
              dz3d(i, 0,j) = snowdp2d(i,j) - dz3d(i,-2,j) - dz3d(i,-1,j)
           else if ((snowdp2d(i,j) > 0.18_r8) .and. (snowdp2d(i,j) <= 0.29_r8)) then
              snl2d(i,j) = -4
              dz3d(i,-3,j) = 0.02_r8
              dz3d(i,-2,j) = 0.05_r8
              dz3d(i,-1,j) = (snowdp2d(i,j) - dz3d(i,-3,j) - dz3d(i,-2,j))/2._r8
              dz3d(i, 0,j) = dz3d(i,-1,j)
           else if ((snowdp2d(i,j) > 0.29_r8) .and. (snowdp2d(i,j) <= 0.41_r8)) then
              snl2d(i,j) = -4
              dz3d(i,-3,j) = 0.02_r8
              dz3d(i,-2,j) = 0.05_r8
              dz3d(i,-1,j) = 0.11_r8
              dz3d(i, 0,j) = snowdp2d(i,j) - dz3d(i,-3,j) - dz3d(i,-2,j) - dz3d(i,-1,j)
           else if ((snowdp2d(i,j) > 0.41_r8) .and. (snowdp2d(i,j) <= 0.64_r8)) then
              snl2d(i,j) = -5
              dz3d(i,-4,j) = 0.02_r8
              dz3d(i,-3,j) = 0.05_r8
              dz3d(i,-2,j) = 0.11_r8
              dz3d(i,-1,j) = (snowdp2d(i,j) - dz3d(i,-4,j) - dz3d(i,-3,j) - dz3d(i,-2,j))/2._r8
              dz3d(i, 0,j) = dz3d(i,-1,j)
           else if (snowdp2d(i,j) > 0.64_r8) then
              snl2d(i,j) = -5
              dz3d(i,-4,j) = 0.02_r8
              dz3d(i,-3,j) = 0.05_r8
              dz3d(i,-2,j) = 0.11_r8
              dz3d(i,-1,j) = 0.23_r8
              dz3d(i, 0,j)=snowdp2d(i,j)-dz3d(i,-4,j)-dz3d(i,-3,j)-dz3d(i,-2,j)-dz3d(i,-1,j)
           endif
        end if
 
        do k = 0, snl2d(i,j)+1, -1
           z3d(i,k,j)    = zi3d(i,k,j) - 0.5_r8*dz3d(i,k,j)
           zi3d(i,k-1,j) = zi3d(i,k,j) - dz3d(i,k,j)
        end do



        if (snl2d(i,j) < 0) then
           do k = snl2d(i,j)+1, 0
                
                
              if(arbinit .or. t_soisno3d(i,k,j) > 300 .or. t_soisno3d(i,k,j) < 200) t_soisno3d(i,k,j) = 250._r8
           enddo
        end if

        do k = 1, nlevsoil
           if(arbinit .or. t_soisno3d(i,k,j) > 1000 .or. t_soisno3d(i,k,j) < 0) t_soisno3d(i,k,j) = t_lake3d(i,nlevlake,j)
        end do

        do k = 1, nlevlake
           if(arbinit .or. lake_icefrac3d(i,k,j) > 1._r8 .or. lake_icefrac3d(i,k,j) < 0._r8) then
              if(t_lake3d(i,k,j) >= tfrz) then
                 lake_icefrac3d(i,k,j) = 0._r8
              else
                 lake_icefrac3d(i,k,j) = 1._r8
              end if
           end if
        end do
        
        do k = 1,nlevsoil
           if (arbinit .or. h2osoi_vol3d(i,k,j) > 10._r8 .or. h2osoi_vol3d(i,k,j) < 0._r8) h2osoi_vol3d(i,k,j) = 1.0_r8
           h2osoi_vol3d(i,k,j) = min(h2osoi_vol3d(i,k,j),watsat3d(i,k,j))

             
           if (t_soisno3d(i,k,j) <= tfrz) then
              h2osoi_ice3d(i,k,j)  = dz3d(i,k,j)*denice*h2osoi_vol3d(i,k,j)
              h2osoi_liq3d(i,k,j) = 0._r8
           else
              h2osoi_ice3d(i,k,j) = 0._r8
              h2osoi_liq3d(i,k,j) = dz3d(i,k,j)*denh2o*h2osoi_vol3d(i,k,j)
           endif
        enddo

        do k = -nlevsnow+1, 0
           if (k > snl2d(i,j)) then
              h2osoi_ice3d(i,k,j) = dz3d(i,k,j)*bdsno
              h2osoi_liq3d(i,k,j) = 0._r8
           end if
        end do

    end if   
  ENDDO
  ENDDO

  END SUBROUTINE lakeini

END MODULE module_sf_lake
