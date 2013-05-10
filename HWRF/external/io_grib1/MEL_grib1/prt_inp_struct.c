/* File:  prt_inp_struct.c		Alice Nakajima, SAIC, 10/96
   Func to print content of all of the Internal structs
Revisions:
09/10/98 atn:  display whether or not GRIB extensions are used.
10/22/98 atn:  +typecast for compiler;
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "grib_lookup.h"	/* for Parm/Ctr/Levl/Mdl_defn */
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */

/* 
   Defines following Global var:
   int UseTables;   flag set if using lookuptbl
 
   Uses the external tables, defined in ld_enc_inputs.c:
   PARM_DEFN  db_parm_tbl[256*MAX_PARM_TBLS]  holds Parameter lookup info;
   LVL_DEFN   db_lvl_tbl[NLEVS]  holds Level lookup info;
   MODEL_DEFN db_mdl_tbl[NMODEL] hold Model lookup info;
   GEOM_DEFN  db_geom_tbl[NGEOM]   holds Geom lookup info;
               
*/
int UseTables=0;       /* GLOBALVAR:   default is no lookup table used */

extern PARM_DEFN  db_parm_tbl[];  	/* parameter conversion info */
extern MODEL_DEFN db_mdl_tbl[]; 	/* model conversion info */
extern LVL_DEFN   db_lvl_tbl[]; 	/* level conversion info */
extern GEOM_DEFN  db_geom_tbl[];	/* Geom conversion info  */
extern CTR_DEFN   db_ctr_tbl[];  	/* Ctr conversion info  */

/*
*
************************************************************************
* A. FUNCTION:  prt_inp_input
*      to print content of the Internal Grib structures
*
*    INTERFACE:
*      void  prt_inp_struct (pds, gds, bms_input, bds_head_input, ppfarr)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I) PDS_INPUT *pds;                
*          internal Product Defn Section struct to print
*      (I) grid_desc_sec  *gds;  
*          internal Grid Defn struct to print
*      (I) BMS_INPUT *bms_input;          
*          internal Bitmap Section  to print
*      (I) struct BDS_HEAD_INPUT *bds_head_input; 
*          internal 11-byte hdr of Binary Data Section to print
*      (I) float **ppfarr;    
*          unpacked & restored float data array to print
*
*    RETURN CODE: none
************************************************************************
*/
#if PROTOTYPE_NEEDED
void	prt_inp_struct (PDS_INPUT 	*pds,
			grid_desc_sec   *gds,
			BMS_INPUT 	*bms_input,
			struct BDS_HEAD_INPUT *bds_head_input,
			float 		**ppfarr)
#else
void	prt_inp_struct (pds, gds, bms_input, bds_head_input, ppfarr)
	PDS_INPUT 	*pds;
	grid_desc_sec   *gds;
	BMS_INPUT 	*bms_input;
	struct BDS_HEAD_INPUT *bds_head_input;
	float 		**ppfarr;
#endif
{
  char *func= "prt_inp_struct"; /* name of function */
  float *farr;			/* ptr to float data aray */
  double pwr10toD;		/* 10 to the Decimal Scl fctr */
  int D;			/* Decimal Scl Fctr */
  int indx,i;			/* index */

   fprintf(stdout,"\n--- Entering %s ---\n", func);
/*
*
* A.1       IF (PDS_INPUT struct is null) THEN
*               PRINT message
*           ELSE
*               PRINT out all of its fields, values and type
*           ENDIF
*/
   if (pds == NULL) fprintf(stdout,"**** PDS_INPUT is null****\n");
   else
   {             
     fprintf(stdout,"***  PDS_INPUT ****\n");
     p_ushort (pds->uslength);        
     p_ushort (pds->usEd_num);       
     p_ushort (pds->usParm_tbl);    
     p_ushort (pds->usCenter_id);  
     if (UseTables)
        if ( db_ctr_tbl[pds->usCenter_id].ctr_dsc[0] )
        fprintf(stdout,"\tCtr_dsc=%s\n", db_ctr_tbl[pds->usCenter_id].ctr_dsc);
        else 
	fprintf(stdout,
	"\tOriginating Center ID %d not defined in current table.\n",
	pds->usCenter_id);

     p_ushort (pds->usProc_id);   
     if (UseTables)
     if ( db_mdl_tbl[pds->usProc_id].grib_dsc[0] )
        fprintf(stdout,"\tModel Description = %s\n",
	db_mdl_tbl[pds->usProc_id].grib_dsc);
     	else
        fprintf(stdout,"\tModel ID %d not defined in current table.\n",
	pds->usProc_id);

     p_ushort (pds->usGrid_id);  
     if (UseTables) 
     if ( db_geom_tbl[pds->usGrid_id].grib_dsc [0] )
        fprintf(stdout,"\tGrid Description = %s\n",
	db_geom_tbl[pds->usGrid_id].grib_dsc);
     else
        fprintf(stdout,"\tGrid ID %d not defined in current table.\n",
	pds->usGrid_id);

     p_ushort (pds->usGds_bms_id); 
     p_ushort (pds->usParm_id);   

     /*
      * The sub-tables cannot have GribCode Zero since it's Reserved
      * if the center doesn't use sub tables then they must set ParmSub to 0
      * when ParmId is between 250 (Tbl-A) and 254 (Tbl-E);
      */
     if (pds->usParm_id>=250 && pds->usParm_id<=254 && pds->usParm_sub!=0)
        {
          indx = 256 * (pds->usParm_id - 249) + pds->usParm_sub;
          p_ushort (pds->usParm_sub);
	  fprintf(stdout,"  (pid=%d, psub=%d) is Sub-ParmTbl Indx = %d\n",
	  pds->usParm_id, pds->usParm_sub, indx);
        }
      else  {
	  indx = pds->usParm_id;
	  fprintf(stdout,
	  "  usParm_sub not used (%ld), default to ParmTbl Indx= %d\n",
	  pds->usParm_sub, indx);
	}

     if(UseTables) {
      if ( db_parm_tbl[indx].grib_dsc[0] ) {
         fprintf(stdout,"\tParameter = %s\n",db_parm_tbl[indx].grib_dsc);
         fprintf(stdout,"\tUnits = %s\n",db_parm_tbl[indx].grib_unit_dsc);
      }else
         fprintf(stdout,"\tParameter ID %d not defined in current table.\n",
	 pds->usParm_id);
      }

     p_ushort (pds->usLevel_id); 
     p_ushort (pds->usLevel_octets); 
     p_ushort (pds->usHeight1);     
     p_ushort (pds->usHeight2);    
    if(UseTables) {
       if ( db_lvl_tbl[pds->usLevel_id].grib_dsc[0] ) {
          fprintf(stdout,"\tLevel description = %s\n", 
   		db_lvl_tbl[pds->usLevel_id].grib_dsc);
          switch(db_lvl_tbl[pds->usLevel_id].num_octets){
            case 2: fprintf(stdout,"\t%s = %u\n",
     	   	db_lvl_tbl[pds->usLevel_id].lvl_name_1, pds->usHeight1);
              	break;
            case 1: fprintf(stdout,"\t%s = %u\n\t%s = %u\n",
     	   	db_lvl_tbl[pds->usLevel_id].lvl_name_1, pds->usHeight1,
     	   	db_lvl_tbl[pds->usLevel_id].lvl_name_2, pds->usHeight2);
              	break;
            case 0: break;
            default:
              fprintf(stdout,"***Number of octets for table 3 undefined - possibly "
              "corrupt dataset.***\n");
          }
       }
     else fprintf(stdout,"Level ID %d not defined in current table.\n",
   	pds->usLevel_id);
    } /* end UseTables 'if' statement */

     p_ushort (pds->usYear);      
     p_ushort (pds->usMonth);    
     p_ushort (pds->usDay);     
     p_ushort (pds->usHour);   
     p_ushort (pds->usMinute);
     p_ushort (pds->usFcst_unit_id);   
     p_ushort (pds->usP1);            
     p_ushort (pds->usP2);           
     p_ushort (pds->usTime_range);  
     p_ushort (pds->usTime_range_avg);
     p_ushort (pds->usTime_range_mis);
     p_ushort (pds->usCentury); 
     p_ushort (pds->usCenter_sub);   
     p_short  (pds->sDec_sc_fctr);
     /* p_ushort (pds->ausZero[0]);
     p_ushort (pds->ausZero[1]);
     p_ushort (pds->ausZero[2]);
     p_ushort (pds->ausZero[3]);
     p_ushort (pds->ausZero[4]);
     p_ushort (pds->ausZero[5]);
     p_ushort (pds->ausZero[6]);
     p_ushort (pds->ausZero[7]);
     p_ushort (pds->ausZero[8]);
     p_ushort (pds->ausZero[9]);
     p_ushort (pds->ausZero[10]);
     p_ushort (pds->ausZero[11]); */

     if (pds->usExt_flag == (unsigned short)EXTENSION_FLAG) {
       fprintf(stdout,
       " (uns. short) pds->usExt_flag  = %u (Extensions used)\n",
       pds->usExt_flag);
       p_ushort (pds->usSecond);
       p_ushort (pds->usTrack_num);
       p_ushort (pds->usParm_sub);	
       p_ushort (pds->usSub_tbl); 
      }
     else fprintf(stdout,
 	" (uns. short) pds->usExt_flag  = %u (not used)\n" \
 	" (uns. short) pds->usSecond    = not avail\n" \
 	" (uns. short) pds->usTrack_num = not avail\n" \
 	" (uns. short) pds->usParm_sub  = not avail\n" \
 	" (uns. short) pds->usSub_tbl   = not avail\n", pds->usExt_flag );

   }

/*
*
* A.2       IF (GDS struct is null) THEN
*               PRINT message
*           ELSE
*               TEST the Projection type, skip if not a supported one;
*               PRINT out all of its fields, values and type
*               SWITCH (type of projection)
*                 > LatLon (0), Gaussian (4), 
*                 > Rotated LatLon (10), Rotated Gaussian (14), 
*                 > Stretched LatLon (20), Stretched Gaussian (24), 
*                 > Stretched Rotated Latlon(30), Stretched Rotated Gauss(34):
*                      CAST the projection block to type GDS_LATLON_INPUT
*                      and print its fields, value and type;
*                 > Lambert (3), Albers (8), Oblique Lambert Conf (13):
*                      CAST the projection block to type GDS_LAM_INPUT
*                      and print its fields, value and type;
*                 > Polar (5):  
*                      CAST the projection block to type GDS_PS_INPUT
*                      and print its fields, value and type;
*                 default: PRINT error  !unsupported projection
*               ENDSWITCH
*           ENDIF
*/
  if (gds == NULL) {
	fprintf(stdout,"\n*** GDS is null ***\n");
	goto CHECK_BMS;
	}

  fprintf(stdout,"\n*** GDS_HEAD_INPUT ***\n"); 
  fprintf(stdout,"sizeof(GDS_GRIB + projblk)= ");
  p_ushort (gds->head.uslength);
  p_ushort (gds->head.usNum_v);
  p_ushort (gds->head.usPl_Pv);

  /* test to see if the Data type is out of bound, or if it's
 	currently not supported;  if so, print msg got print next section.
  */
  if (gds->head.usData_type >= (unsigned short) PRJ_COUNT  ||
     strstr((char*)prjn_name[gds->head.usData_type],(char*)"Unsupported Grid")) 
    {
	fprintf(stdout,
	"Warning:  Projection #%d is not supported, skip this section\n",
	gds->head.usData_type);
	goto CHECK_BMS;
    }
     
  p_string (prjn_name[gds->head.usData_type]);
  switch (gds->head.usData_type) 
       	 {
           case LATLON_PRJ:
           case GAUSS_PRJ:
           case ROT_LATLON_PRJ:
           case ROT_GAUSS_PRJ:
           case STR_LATLON_PRJ:
           case STR_GAUSS_PRJ :
           case STR_ROT_LATLON_PRJ :
           case STR_ROT_GAUSS_PRJ :
		   p_int (gds->llg.usNi);      
		   p_int (gds->llg.usNj);     
		   p_long (gds->llg.lLat1);   
		   p_long (gds->llg.lLon1);  
		   p_ushort (gds->llg.usRes_flag);
		   p_long (gds->llg.lLat2);    
		   p_long (gds->llg.lLon2);   
		   p_int (gds->llg.iDi);    
		   p_int (gds->llg.iDj);   
		   p_ushort (gds->llg.usScan_mode);

		   if (gds->head.usData_type == LATLON_PRJ ||
		       gds->head.usData_type == GAUSS_PRJ) 
		   break;

		   /* 	02/98
		   remaining code is for the Stretch/Rotate parameters 
		   */
		   p_long (gds->llg.lLat_southpole);
		   p_long (gds->llg.lLon_southpole);
		   fprintf(stdout,"gds->llg.lRotate (%ld) tofloat= %.3f\n",
		   gds->llg.lRotate , 
		   grib_ibm_local((unsigned long)gds->llg.lRotate));
		   p_long (gds->llg.lPole_lat);    
		   p_long (gds->llg.lPole_lon);   
		   fprintf(stdout,"gds->llg.lStretch (%ld) tofloat= %.3f\n"
		   ,gds->llg.lStretch, 
		   grib_ibm_local((unsigned long)gds->llg.lStretch));
		break;

	   case MERC_PRJ:
		   p_int (gds->merc.cols); 
		   p_int (gds->merc.rows);
		   p_long (gds->merc.first_lat); 
		   p_long (gds->merc.first_lon);
		   p_ushort (gds->merc.usRes_flag);          
		   p_long (gds->merc.La2); 
		   p_long (gds->merc.Lo2);
		   p_long (gds->merc.latin);
		   p_ushort (gds->merc.usScan_mode);          
		   p_float (gds->merc.lon_inc); 
		   p_float (gds->merc.lat_inc);
		break;

           case LAMB_PRJ:  
           case ALBERS_PRJ:  
           case OBLIQ_LAMB_PRJ:  
		   p_int (gds->lam.iNx);                 
		   p_int (gds->lam.iNy);                 
		   p_long (gds->lam.lLat1);               
		   p_long (gds->lam.lLon1);               
		   p_ushort (gds->lam.usRes_flag);          
		   p_long (gds->lam.lLon_orient);         
		   p_ulong (gds->lam.ulDx);                
		   p_ulong (gds->lam.ulDy);                
		   p_ushort (gds->lam.usProj_flag);         
		   p_ushort (gds->lam.usScan_mode);         
		   p_long (gds->lam.lLat_cut1);          
		   p_long (gds->lam.lLat_cut2);           

		   if (gds->head.usData_type == LAMB_PRJ) break;
		   p_long (gds->lam.lLat_southpole);      
		   p_long (gds->lam.lLon_southpole);      
		 break;

           case POLAR_PRJ:  
		   p_ushort (gds->pol.usNx);                
		   p_ushort (gds->pol.usNy);                
		   p_long (gds->pol.lLat1);               
		   p_long (gds->pol.lLon1);               
		   p_ushort (gds->pol.usRes_flag);          
		   p_long (gds->pol.lLon_orient);         
		   p_ulong (gds->pol.ulDx);                
		   p_ulong (gds->pol.ulDy);                
		   p_ushort (gds->pol.usProj_flag);         
		   p_ushort (gds->pol.usScan_mode);         
		 break;
		
	   default:  fprintf(stdout,
		"Warning:  print code not available for Projection %d\n",
		gds->head.usData_type);

          } /* SWitch */


CHECK_BMS:
/*
*
* A.3       IF (BMS_INPUT struct is null) THEN
*               PRINT message
*           ELSE
*               PRINT out all of its fields, values and type
*           ENDIF
*/
   
  if (bms_input==NULL || bms_input->uslength <= 0) 
      fprintf(stdout,"\n*** BMS_INPUT is Null (no bms)  ***\n"); 
  else {
      fprintf(stdout,"\n*** BMS_INPUT  ***\n"); 
      p_ushort (bms_input->uslength);
      p_ushort (bms_input->usUnused_bits);
      p_ushort (bms_input->usBMS_id);
      p_ulong (bms_input->ulbits_set);
      fprintf(stdout,"(not going to print CHAR *bit_map)\n");
    } 

/*
*
* A.4       IF (BDS_INPUT struct is null) THEN
*               PRINT message
*           ELSE
*               PRINT out all of its fields, values and type
*           ENDIF
*/
  if (bds_head_input == NULL) 
	fprintf(stdout,"\n*** BDS_HEAD_INPUT is null ***\n");
  else {
    fprintf(stdout,"\n*** BDS_HEAD_INPUT ***\n");
    p_ulong (bds_head_input->length);
    p_ushort (bds_head_input->usBDS_flag);
    p_int (bds_head_input->Bin_sc_fctr);
    p_float (bds_head_input->fReference); 
    p_ushort (bds_head_input->usBit_pack_num);
    p_ulong (bds_head_input->ulGrid_size);  
    p_float (bds_head_input->fPack_null);  
  }

/*
*
* A.5       IF (Float array is null) THEN
*               PRINT message
*           ELSE
*               PRINT out up to 100 of its values (after taking out
*               the Decimal Scale Factor)
*           ENDIF
*/
  if (ppfarr == 0) fprintf(stdout,"\n*** FLOAT array is null ***\n");
  else {
     farr= *ppfarr;
     fprintf(stdout,
	"\n*** FLOAT ARRAY has %ld elements (print upto first 100) ***\n",
	bds_head_input->ulGrid_size);

     if (pds != NULL) {
	D= pds->sDec_sc_fctr;
	pwr10toD=  pow ( 10., (double) D );
        fprintf(stdout,"Dec_sc_fctr=%d\n", D );
	}
    else { 
	D= 0; pwr10toD = 1.; 
	fprintf(stdout,"No PDS avail, assume Dec Scl Fctr to be 0\n");
	}
	
     /* Data was scaled up in Pack_Spatial in order to get the INT values
	to pack, so now need to change it back to its true value by
 	taking out the Decimal Scale Factor 
     */
     if (D >= 0)
        for (i=0; i<bds_head_input->ulGrid_size && i < 100; i=i+5)
        fprintf(stdout,"%3d-  %.*f  %.*f  %.*f  %.*f  %.*f\n", 
	i+1, 
	D, farr[i], D, farr[i+1], D, farr[i+2], D, farr[i+3], D, farr[i+4] );
     else 
        for (i=0; i<bds_head_input->ulGrid_size && i < 100; i=i+5)
        fprintf(stdout,"%3d-  %.0f  %.0f  %.0f  %.0f  %.0f\n", 
	i+1, farr[i], farr[i+1], farr[i+2], farr[i+3], farr[i+4] );
   }
/*
*
* A.6       RETURN w/nothing
*
*/
  fprintf(stdout,"--- Leaving %s ---\n\n", func);

/*
*
* END OF FUNCTION
*
*/
}
