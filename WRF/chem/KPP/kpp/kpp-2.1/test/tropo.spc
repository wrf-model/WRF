#include atoms

#DEFVAR                  
    NO		= N + O;     
    NO2		= N + 2O;   
    HNO3	= H + N + 3O;   
    NH3		= N + 3H;    
    SO2		= S + 2O;    
    SO4		= S + 4O;            
    O3		= 3O;     
    CH4		= C + 4H;   
    C2H6	= 2C + 6H;   
    C3H8	= 3C + 8H;   
    ALKA	= 			IGNORE;   
    ETHE	= 			IGNORE;             
    ALKE	= 			IGNORE;   
    AROM	= 			IGNORE;  
    ACO2	= 2H + C + 2O; 			{ HCOOH }   
    ACTA	= 2C + 4H + 2O; 		{ CH3COOH }
    HCHO	= 2H + C + O;   
    ALD2	= 			IGNORE;  
    H2O2	= 2H + 2O;   
    ROOH	= 			IGNORE;   
    HONO	= H + 2O + N;   
    PAN		= 2C + 3H + 5O + N;		{ CH3CO3NO2 }       
    TPAN	= 4C + 3H + 6O + N;		{ CHOCH=CHCO3NO2 }   
    KET		= 			IGNORE;   
    CRES	= 			IGNORE;   
    DIAL	= 			IGNORE;   
    GLYX	= 2C + 2H + 2O;			{ CHOCHO }   
    MGLY	= 3C + 4H + 2O; 	 	{ CH3COCHO }            
    NH4NO3	= 2N + 4H + 3O;         	{ or AMNT }
    HCL		= H + Cl;   
    R3N2	= N + IGNORE;   
    RAN1	= N + IGNORE;   
    RAN2	= N + IGNORE;   
    N2O5	= 2N + 5O;             
    HNO4	= H + N + 4O;   
    NO3		= N + 3O;   
    ISOP	= 			IGNORE;   
    MVK		= 			IGNORE;    
    MACR	= 			IGNORE;   
    HAC		= 			IGNORE;              
    MGGY	= 			IGNORE;   
    MPAN	= N + IGNORE;  
    IPAN	= N + IGNORE;   
    INO2	= N + IGNORE;   
    MAN2	= N + IGNORE;   
    MVN2	= N + IGNORE;             
    MACA	= 			IGNORE;   
    PYVA	= 			IGNORE;  
    DOL6	= 			IGNORE;   
    DOL7	= 			IGNORE;   
    DOL8	= 			IGNORE;   
    CPET	= 			IGNORE;             
    CHEX	= 			IGNORE;   
    SUCA	= 			IGNORE;  
    GLUA	= 			IGNORE;   
    ADIA	= 			IGNORE;                                   
    PRN2	= 2N + IGNORE;   
    PRPN	= N + IGNORE;  
    OZID	= 			IGNORE;   
    DMS		= 2C + 6H + S; 			{ CH3-S-CH3 }   
    MSA		= S + IGNORE; 
    CO		= C + O;      

#DEFRAD                                                  
    OH		= O + H;      
    HO2		= H + 2O;                        
    AHO2	= 3H + 3O + C; 			{ HOCH2O2 }    
    MCO3	= 2C + 3H + 3O; 		{ CH3CO2 }   
    MO2		= C + 3H + 2O;			{ CH3O2 }     
    ETO2	= 2C + 5H + 2O;			{ C2H5O2 }   
    KO2		= 			IGNORE;                        
    R3O2	= 			IGNORE;    
    RAO2	= 			IGNORE;   
    TO2		= 			IGNORE;     
    TCO3	= 4C + 3H + 4O; 		{ CHOCH=CHCO3 }  
    ZO2		= 			IGNORE;                        
    EO2		= 			IGNORE;     
    PO2		= 			IGNORE;    
    CHO2	= C + 2H + 2O;			{ CH2O2 }    
    CRO2	= 2C + 4H + 2O;			{ CH3CHO2 }   
    PRN1	= N + IGNORE;                       
    RIO2	= 			IGNORE;    
    VRO2	= 			IGNORE;   
    MAO3	= 			IGNORE;    
    MRO2	= 			IGNORE;   
    HACO	= 			IGNORE;                       
    MAOO	= 			IGNORE;    
    MCRG	= 			IGNORE;   
    MVKO	= 			IGNORE;                                             

#DEFFIX
    O2		= 2O;      
    H2O		= H + 2O;    
    CO2		= C + 2O;    
    H2		= 2H;

