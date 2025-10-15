"Name: \FU:MB_CREATE_GOODS_MOVEMENT\SE:END\EI
ENHANCEMENT 0 Z_PEDIDO_CLASSV.
*
  LOOP at XMSEG.
    CALL FUNCTION 'Z_MM_INDEA_LOTE'
      EXPORTING
        I_EBELN       = xmseg-ebeln
        I_EBELP       = xmseg-ebelp
        I_MBLNR       = xmseg-mblnr
        I_MJAHR       = xmseg-mjahr
        I_MATNR       = xmseg-matnr
        I_CHARG       = xmseg-charg
        I_MENGE       = xmseg-menge
        I_BTN         = ' '
        I_tcode       = sy-tcode.
 ENDLOOP.


ENDENHANCEMENT.
