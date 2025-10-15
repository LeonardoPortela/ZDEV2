"Name: \PR:SAPLJ1BN\FO:SAVE_TAX_FIELDS\SE:BEGIN\EI
ENHANCEMENT 0 Z_MM_MBSU.
*
   IF     p_i_mseg-umwrk  IS NOT INITIAL AND
          p_xtaxcom-lifnr IS INITIAL AND
          p_xtaxcom-kunnr IS INITIAL AND
          p_mwart         EQ 'V'.
       CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = p_i_mseg-umwrk
        IMPORTING
          OUTPUT = p_xtaxcom-lifnr.
   ENDIF.

ENDENHANCEMENT.
