#DEFVAR
{1}   O3          = 3O ;               {A: ozone}
{2}   CH4         = C + 4H ;           {A: methane}
{3}   CO          = C + O ;            {A: carbon monoxide}
{4}   H2O2        = 2H + 2O ;          {A: hydrogen peroxide}
{5}   HNO3        = H + N + 3O ;       {A: nitric acid} 
{6}   MeOOH       = C + 4H + 2O ;      {A: methyl hydro peroxide}
{7}   HCHO        = C + 2H + O ;       {A: formalydehyde}
{8}   MeOH        = C + 4H + O ;       {A: CH3OH methanol }
{8b}  MeO2NO2     = C + 3H + 4O + N ;  {A: methyl peroxy nitrate}
{c  --- additionals to ch4 chemistry --- }
{9}   ISOP        = 5C + 8H;           {A: isoprene}
{10}   ISOOH      = 5C + 10H + 3O;     {A: isoprene (hydro) peroxides}
{11}  MVK         = 4C + 6H + O;       {A: methyl vinyl ketone + methacrolein}
{12}  MVKOOH      = 4C + 8H + 4O;      {A: MVK (hydro) peroxides}
{13}  MGLO        = 3C + 4H + 2O;      {A: CH3C(O)C(O)H  methylglyoxal}
{14}  PAA         = 2C + 4H + 3O;      {A: peroxy acetylic acid}
{15}  PAN         = 2C + 3H + 5O + N;  {A: peroxyacetylnitrate}
{16}  MPAN        = 3C + 5H + 5O + N;  {A: peroxymethacryloyl nitrate or peroxymethacrylic nitric anhydride}
{17}  ISON        = IGNORE + N;        {A: organic nitrates from ISO2 and ISOP+NO3}    
{18}  HCOOH       = C + 2H + 2O;       {A: formic acid}
{19}  CH3COOH     = 2C + 4H + 2O;      {A: acetic acid}
{20}  ACETOL      = 3C + 6H + 2O;      {A: hydroxy acetone, HACET in output}
{21}  NACA        = 2C + 3H + 4O + N;  {A: nito-oxy acetaldehyde} 
{22}  C2H6        = 2C + 6H ;          {A: ethan}
{23}  EtOOH       = 2C + 6H + 2O;      {A: ethyl hydro peroxide}
{24}  ALD         = 2C + 4H + O;       {A: acetaldehyde}
{25}  C3H8        = 3C + 8H;           {A: propane}
{26}  PrOOH       = 3C + 8H + 2O;      {A: hydrogenperoxide from PrO2}
{27}  ACET        = 3C + 6H + O;       {A: acetone}
{28}  ACETP       = 3C + 6H + 3O;      {A: Hydrogenperoxide from ACETO2}
{29}  PrONO2      = IGNORE + N;        {Ax: i-Propyl Nitrate, transported with ONIT}
{30}  C3H6        = 3C + 6H;           {A: propene ~ propylene }
{31}  C3H6OOH     = 3C + 8H + 3O;      {A: hydoperoxides from C3H6 }
{32}  C2H4        = 2C + 4H;           {A: ethene }
{33}  C4H10       = 4C + 10H;          {A: n-butan, representative of higher alkanes }
{34}  C4H9OOH     = 4C + 10H + 3O;     {A: hydoperoxides from C4H10 }
{35}  ONIT        = IGNORE + N;        {A: organic nitrates from higher alkyl nitrates, +C3H6+NO3}
{36}  MEK         = 4C + 8H + O;       {A: CH3CH2C(O)CH3, methyl ethyl ketone, represents all higher ketones}
{37}  MEKOOH      = 4C + 8H + O;       {A:  hydoperoxides from MEK}
{38}  MeCOCO      = 4C + 6H + 2O;      {CH3COCOCH3, represents multiply oxigenated C>3 compounds}


{c  ******************non advected species *******************************}

{37}  O1D         = O ;                {N: oxygen atomic first singlet state}
{38}  OH          = O + H ;            {N: hydroxyl radical}
{39}  HO2         = H + 2O ;           {N: perhydroxyl radical}
{40}  NO          = N + O ;            {N:1 nitric oxide}
{41}  NO2         = N + 2O ;           {N:1 nitrogen dioxide}
{42}  NO3         = N + 3O ;           {N: nitrogen trioxide}
{43}  N2O5        = 2N + 5O ;          {N: dinitrogen pentoxide}
{44}  HNO4        = H + 4O + N ;       {N: HO2NO2 peroxynitric acid}
{45}  MeO2        = C + 3H + 2O;       {N: methylperoxy radical}
{c ---- additional to ch4-chemistry ---------------------}
{46}  PA          = 2C + 3H + 3O;      {N: peroxy acetyl radical}
{47}  ISO2        = 5C + 9H + 3O;      {N: isoprene (hydroxy) peroxy radicals}
{48}  MVKO2       = 4C + 7H + 4O;      {N: MVK/MACR peroxy radicals}
{49}  EtO2        = 2C + 5H + 2O;      {N: ethylperoxy radical}
{50}  PrO2        = 3C + 7H + 2O;      {N: peroxyradical von propane, secondary only}
{51}  ACETO2      = 3C + 5H + 3O;      {N: peroxyradical from acetone}
{52}  C3H6O2      = 3C + 7H + 3O;      {N: peroxyradical from propene ~ propylene + OH (+O2)}
{53}  C4H9O2      = 4C + 10H + 3O;     {N: peroxyradical from C4H10}
{54}  MEKO2       = 4C + 9H + 4O;      {N: peroxyradical from MEK, also for multiply oxigenated peroxy radicals }
{???? }
{55  C2H4O2      = 2C + 5H + 3O;      N: peroxyradical from C2H4 + OH (+O2)}
{ethene chemistry stuff, only for future expansions, see separate file "ethene.eqn" }
{EO  = IGNORE ;} 
{EO2  = IGNORE ;}
{EOOH  = IGNORE ;}
{GLOALD   = IGNORE ;}
{GCO3 = IGNORE ;}
{+2 species}

#DEFFIX
{1}   H2O         = H + 2O ;      {water}
{2}   M           = IGNORE ;      {third body}
