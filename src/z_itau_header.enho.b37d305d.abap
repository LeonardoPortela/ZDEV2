"Name: \PR:RFFOBR_D\FO:FILL_HEADER_ITAU\SE:END\EI
ENHANCEMENT 0 Z_ITAU_HEADER.

*     Convert the control digit REGUH-UBKON(char2) to numc1.
    CONDENSE reguh-ubkon NO-GAPS.
    CLEAR j_1bdmeyh-h06.
***    CONCATENATE  agency '00' reguh-ubknt(5) reguh-ubkon INTO j_1bdmeyh-h06.
    CONCATENATE  agency '00' reguh-ubknt(5) reguh-ubknt+6(1) INTO j_1bdmeyh-h06.
    CONDENSE j_1bdmeyh-h06 NO-GAPS.

ENDENHANCEMENT.
