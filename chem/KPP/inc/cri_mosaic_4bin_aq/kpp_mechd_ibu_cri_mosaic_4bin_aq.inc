!
! calculate summation for RO2 species

    RO2 = & 
        REAL(var(ind_CH3OO) + var(ind_C2H5O2) + var(ind_RN10O2) + var(ind_IC3H7O2) &
        + var(ind_RN13O2) + var(ind_RN13AO2) + var(ind_RN16AO2)  + var(ind_RN16O2) &
        + var(ind_RN19O2) + var(ind_HOCH2CH2O2) + var(ind_RN9O2) + var(ind_RN12O2) &
        + var(ind_RN15O2)+ var(ind_RN18O2)+ var(ind_RN15AO2) + var(ind_RN18AO2) &
        + var(ind_CH3CO3) + var(ind_C2H5CO3)+ var(ind_RN11O2)+ var(ind_RN14O2) &
        + var(ind_RN17O2) + var(ind_HOCH2CO3)+ var(ind_RU14O2)+ var(ind_RU12O2) &
        + var(ind_RU10O2)+ var(ind_NRN6O2)+ var(ind_NRN9O2)+ var(ind_NRN12O2) &
        + var(ind_RTN28O2) + var(ind_NRU14O2)+ var(ind_NRU12O2) + var(ind_RA13O2) &
        + var(ind_RA16O2) + var(ind_RA19AO2) + var(ind_RA19CO2)+ var(ind_RN8O2) &
        + var(ind_RTN26O2)  + var(ind_NRTN28O2) + var(ind_RTN25O2) + var(ind_RTN24O2) &
        + var(ind_RTN23O2)+ var(ind_RTN14O2)+ var(ind_RTN10O2)+ var(ind_RTX28O2) &
        + var(ind_RTX24O2)+ var(ind_RTX22O2)+ var(ind_NRTX28O2), KIND=dp)
!
