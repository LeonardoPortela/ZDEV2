"Name: \FU:J_1B_NF_IV_CHECK_SELECT_ITEMS\SE:BEGIN\EI
ENHANCEMENT 0 Z_PEDIDO_ZDBP.
*
   data:  VBSART type ekko-BSART,
          VEBELN type ekko-ebeln.

   clear: VBSART, VEBELN.
   LOOP AT it_rseg WHERE ebeln IS NOT INITIAL.
      VEBELN = it_rseg-ebeln.
      exit.
   ENDLOOP.

   if VEBELN is not INITIAL.
     SELECT SINGLE bsart into VBSART from ekko where ebeln = VEBELN.
     if VBSART = 'ZDBP' or VBSART = 'YDBP'.
        LOOP AT it_rseg.
           if it_rseg-ebeln is  INITIAL.
              it_rseg-ebeln = VEBELN.
              it_rseg-ebelp = 10.
              MODIFY it_rseg INDEX sy-tabix TRANSPORTING ebeln ebelp.
           endif.
        ENDLOOP.
     endif.
   endif.
ENDENHANCEMENT.
