"Name: \PR:SAPFV50W\FO:XIMSEG_FUELLEN\SE:END\EI
ENHANCEMENT 0 Z_FILL_LIFNR_REMESSA_ZARM.

  IF XLIKP-LFART EQ 'ZLO'. "Fornecimento sem referencia - Processo de Remessa de Armazenagem - US 66690
    read table xvbpa with key vbeln = xlips-vbeln
                               posnr = posnr_low
                               parvw = 'WL'         "goods supplier
      into DATA(lwa_vbpa_wl) transporting lifnr.

    if sy-subrc = 0.
      LOOP AT ximseg ASSIGNING FIELD-SYMBOL(<fs_ximseg>).
        <fs_ximseg>-lifnr = lwa_vbpa_wl-lifnr.
      ENDLOOP.
    endif.
  ENDIF.

ENDENHANCEMENT.
