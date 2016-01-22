#include atoms

#DEFVAR
    NO          = N + O ;       {nitric oxide}
    NO2         = N + 2O ;      {nitrogen dioxide}
    NO3         = N + 3O ;      {nitrogen trioxide}
    N2O5        = 2N + 5O ;     {dinitrogen pentoxide}
    HONO        = H + 2O + N ;  {nitrous acid}
    HNO3        = H + N + 3O ;  { nitric acid }
    PNA         = H + 4 O + N ; {HO2NO2 peroxynitric acid}
    O1D         = O ;           {oxygen atomic first singlet state}
    O           = O ;           {oxygen atomic ground state (3P)}
    OH          = O + H ;       {hydroxyl radical}
    O3          = 3O ;          {ozone}
    HO2         = H + 2O ;      {perhydroxyl radical}
    H2O2        = 2H + 2O ;     {hydrogen peroxide}
    HCHO        = C + 2H + O ;  {formalydehyde}
    ALD2        = IGNORE ;      {high molecular weight aldehides}
    C2O3        = 2C + 3H + 3O ; {CH3CO(O)OO peroxyacyl radical}
    PAN         = 2C + 3H + 5O + N ; {CH3C(O)OONO2, peroxyacyl nitrate}
    PAR         = IGNORE ;      {parafin carbon bond}
    ROR         = IGNORE ;      {secondary organic oxy radical}
    OLE         = IGNORE ;      {olefinic carbon bond}
    ETH         = 2C + 4H ;     {CH2=CH2 ethene}
    TOL         = 7C + 8H ;     {C6H5-CH3 toluene}
    CRES        = IGNORE ;      {cresol and h.m.w. phenols}
    TO2         = IGNORE ;      {toluene-hydroxyl radical adduct}
    CRO         = IGNORE ;      {methylphenoxy radical}
    OPEN        = IGNORE ;      {h.m.w. aromatic oxidation ring fragment}
    XYL         = 8C + 10H ;    {C6H4-(CH3)2 xylene}
    MGLY        = 3C + 4H + 2O ; {CH3C(O)C(O)H methylglyoxal}
    ISOP        = IGNORE ;      {isoprene}
    XO2         = IGNORE ;      {NO-to-NO2 operation}
    XO2N        = IGNORE ;      {NO-to-nitrate operation}          
    CO          = C + O ;       {carbon monoxide}
    HNO2        = H + N + 2O ;
    ROOH        = IGNORE ;
    SO2         = S + 2O ;
    NH3         = N + 3H ;
    CH3O2       = C + 3H + 2O ;
    CH3O        = C + 3H + O ;
    CH3OOH      = 4H + C + 2O ;
    CH3OH       = 4H + C + O ;
    HCOOH       = 2H + C + 2O ;


HCHOaq = IGNORE ;
HMSAaq = IGNORE ;
OHMINaq  = IGNORE ;
SO32MINaq  = IGNORE ;
CH3O2aq = IGNORE ;
O2MINaq = IGNORE ;
O3aq = IGNORE ;
HSO3MINaq = IGNORE ;
SO42MINaq = IGNORE ;
HPLUSaq = IGNORE ;
H2O2aq  = IGNORE ;
HO2aq  = IGNORE ;
OHaq = IGNORE ;
CH3O2Haq = IGNORE ;
CUPLUSaq = IGNORE ;
CU2PLUSaq = IGNORE ;
FE2PLUSaq = IGNORE ;
FE3PLUSaq = IGNORE ;
HCOOHaq  = IGNORE ;
HNO3aq = IGNORE ;
NO3aq  = IGNORE ;
HCOOMINaq  = IGNORE ;
SO2aq  = IGNORE ;
NH3aq = IGNORE ;
NH4aq = IGNORE ;
NO3MINaq = IGNORE ;
HCO3MINaq = IGNORE ; 
CO32MINUSaq = IGNORE ;
H2CO3aq     = IGNORE ;


#DEFFIX
    H2O         = H + 2O ;      {water}
    CO2aq       = C + 2O ;
    H2Oaq       = H + 2O ;      {water}
    H2          = 2H ;          {molecular hydrogen}
    O2          = 2O ;          {molecular oxygen}
    N2          = 2N ;          {molecular nitrogen}
    CH4         = C + 4H ;      {methane}
    CO2         = C + 2O ;      {carbon dioxide}
    M           = IGNORE ;      {third body}