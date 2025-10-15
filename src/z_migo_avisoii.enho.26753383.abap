"Name: \FU:LE_DELIVERY_GET_BUFFERED\SE:END\EI
ENHANCEMENT 0 Z_MIGO_AVISOII.
*
  DATA lv_charg_migo TYPE mseg-charg.
  IMPORT   lv_charg_migo   TO lv_charg_migo FROM MEMORY ID 'ZIV_CHARG_MIGO'.
  IF lv_charg_migo IS NOT INITIAL.
    READ TABLE cx_deliveries ASSIGNING FIELD-SYMBOL(<ls_delivery>) INDEX 1.
    IF <ls_delivery> IS ASSIGNED.
      READ TABLE <ls_delivery>-item ASSIGNING FIELD-SYMBOL(<fs_delivery>) INDEX 1.
      IF <fs_delivery> IS ASSIGNED.
        <fs_delivery>-charg = lv_charg_migo.
      ENDIF.
    ENDIF.
  ENDIF.
  FREE MEMORY ID 'ZIV_CHARG_MIGO'.

ENDENHANCEMENT.
