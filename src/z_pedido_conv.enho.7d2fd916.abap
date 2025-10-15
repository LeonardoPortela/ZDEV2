"Name: \PR:SAPLV50S\FO:GN_LIPS_FUELLEN_SERNR\SE:BEGIN\EI
ENHANCEMENT 0 Z_PEDIDO_CONV.
*
  if sy-tcode = 'ZMM0110'.
      READ TABLE XEKPO ASSIGNING FIELD-SYMBOL(<fs_ekko>) with key ebeln = XKOMDLGN-VGBEL
                                                                  ebelp = XKOMDLGN-VGPOS.
      IF sy-subrc = 0 and XKOMDLGN-NTGEW gt 0 and XKOMDLGN-MEINS = 'BAG'.
         <fs_ekko>-NTGEW = XKOMDLGN-NTGEW .
         <fs_ekko>-BRGEW = XKOMDLGN-BRGEW .
      ENDIF.
  ENDIF.

ENDENHANCEMENT.
