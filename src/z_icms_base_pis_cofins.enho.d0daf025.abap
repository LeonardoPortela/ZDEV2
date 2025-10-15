"Name: \PR:SAPLV61A\FO:USEREXIT_XKOMV_BEWERTEN_END\SE:BEGIN\EI
ENHANCEMENT 0 Z_ICMS_BASE_PIS_COFINS.
* CSB - 28.09.2021 - Comentado - Inicio
*IF xkomv[] IS INITIAL.
*  EXIT.
*ENDIF.
*
*IF  ( sy-tcode  EQ 'VA01' AND  t001-bukrs <> '0100' )
*    OR sy-tcode EQ 'VA02'
*    OR ( sy-tcode EQ 'VF01' AND t001-bukrs <> '0100' )
*    OR ( sy-tcode EQ 'VF02' AND t001-bukrs <> '0100' )
*    OR sy-tcode EQ 'ZLES0136'
*    OR sy-tcode EQ 'ZLES0077'
*    OR sy-tcode EQ 'ZSDT0062'
*    OR sy-tcode EQ 'ZSDT0041'.
*
*  DATA: lva_total_icmi TYPE komv-kwert,
*        lva_total_bx13 TYPE komv-kwert,
*        lva_total      TYPE komv-kwert,
*        lva_kwert      TYPE komv-kwert,
*        lva_value      TYPE komv-kwert,
*        lva_total_bco1 TYPE komv-kwert,
*        lva_total_bpi1 TYPE komv-kwert,
*        lva_perc       TYPE p DECIMALS 4,
*        lva_total_bx70 TYPE komv-kwert,
*        lva_total_bx80 TYPE komv-kwert,
*        lva_valor(15)  TYPE p DECIMALS 5,
*        lva_mwsbp      TYPE komp-mwsbp,
*        lva_bx70_base  TYPE komv-kawrt,
*        lva_bx80_base  TYPE komv-kawrt,
*        lva_bx72       TYPE komv-kwert,
*        lva_bx82       TYPE komv-kwert.
*
*  DATA: xkomv_val   TYPE STANDARD TABLE OF komv_index WITH HEADER LINE.
*
*  xkomv_val[] = xkomv[].
*
*  READ TABLE xkomv_val WITH KEY kschl = 'ICMI' .
*  IF sy-subrc = 0.
*    lva_total_icmi = xkomv_val-kwert.
*  ENDIF.
*
*  READ TABLE xkomv_val WITH KEY kschl = 'BX13' .
*  IF sy-subrc = 0.
*    lva_total_bx13 = xkomv_val-kwert.
*    lva_mwsbp = lva_mwsbp + lva_total_bx13 .
*  ENDIF.
*
*  READ TABLE xkomv_val WITH KEY kschl = 'BCO1'.
*  IF sy-subrc = 0.
*    lva_total_bco1 = xkomv_val-kwert.
*  ENDIF.
*
*  READ TABLE xkomv_val WITH KEY kschl = 'BPI1'.
*  IF sy-subrc = 0.
*    lva_total_bpi1 = xkomv_val-kwert.
*    lva_perc       = xkomv_val-kbetr.
*    lva_perc  = lva_perc  / 10.
*  ENDIF.
*
*  lva_total = lva_total_icmi - lva_total_bx13.
*
*  LOOP AT xkomv.
*
*    IF xkomv-kschl = 'BX70' AND xkomv-krech = 'B'.
*      xkomv-kwert  =  lva_total.
*      lva_bx70_base = xkomv-kawrt.
*      MODIFY xkomv.
*    ENDIF.
*
*    IF xkomv-kschl = 'BX71'.
*      xkomv-kawrt  =  lva_bx70_base + lva_total.
*      MODIFY xkomv.
*    ENDIF.
*
*
*    IF xkomv-kschl = 'BX80' AND xkomv-krech = 'B'.
*      xkomv-kwert  =  lva_total.
*      lva_bx80_base = xkomv-kawrt.
*      MODIFY xkomv.
*    ENDIF.
*
*    IF xkomv-kschl = 'BX81'.
*      xkomv-kawrt  =  lva_bx80_base + lva_total.
*      MODIFY xkomv.
*    ENDIF.
*  ENDLOOP.
*
*  LOOP AT xkomv.
*    IF xkomv-kschl = 'BX72' AND xkomv-krech = 'B'.
*      IF xkomv-kwert IS NOT INITIAL.
*        xkomv-kwert  = ( ( lva_total  * lva_total_bco1 ) / 1000 ) .
*        xkomv-kawrt  =  lva_bx70_base + lva_total.
*        lva_bx72 = xkomv-kwert.
*        MODIFY xkomv.
*        lva_mwsbp = lva_mwsbp + xkomv-kwert .
*      ENDIF.
*    ENDIF.
*
*    IF xkomv-kschl = 'BX7O' AND xkomv-krech = 'A'.
*      IF xkomv-kwert IS NOT INITIAL.
*        xkomv-kwert  = ( ( lva_total  * lva_total_bco1 ) / 1000 ) * -1.
*        xkomv-kawrt  = ( ( lva_total  * lva_total_bco1 ) / 1000 ) .
*        MODIFY xkomv.
*      ENDIF.
*    ENDIF.
*
*    IF xkomv-kschl = 'BX8O' AND xkomv-krech = 'A'.
*      IF xkomv-kwert IS NOT INITIAL.
*        lva_valor = ( lva_total * lva_perc ) / 10000.
*        xkomv-kawrt = lva_valor.
*        xkomv-kwert = lva_valor * -1.
*        MODIFY xkomv.
*      ENDIF.
*    ENDIF.
*
*    IF xkomv-kschl = 'BX82' AND xkomv-krech = 'B'.
*      IF xkomv-kwert IS NOT INITIAL.
*        lva_valor = ( lva_total * lva_perc ) / 10000.
*        xkomv-kwert = lva_valor.
*        xkomv-kawrt  =  lva_bx80_base + lva_total.
*        lva_bx82 = xkomv-kwert .
*
*        MODIFY xkomv.
*        lva_mwsbp = lva_mwsbp + xkomv-kwert .
*      ENDIF.
*    ENDIF.
*
*    IF xkomv-kschl = 'BX7P'.
*      xkomv-kawrt  =  lva_bx70_base + lva_total + lva_bx72.
*      MODIFY xkomv.
*    ENDIF.
*
*    IF xkomv-kschl = 'BX8P'.
*      xkomv-kwert = lva_valor.
*      xkomv-kawrt  =  lva_bx80_base + lva_total + lva_bx82.
*      MODIFY xkomv.
*    ENDIF.
*
*   ENDLOOP.
*
*  IF ( sy-tcode EQ 'VF01' )
*    OR ( sy-tcode EQ  'VF02' )
*    OR ( sy-tcode EQ  'ZLES0077' )
*    OR ( sy-tcode EQ  'ZLES0136' )
*    OR ( sy-tcode EQ  'ZLES0077' )
*    OR ( sy-tcode EQ  'ZSDT0062' )
*    OR ( sy-tcode EQ  'ZSDT0041' ).
*
*    IF komp-mwsbp <> lva_mwsbp.
*      komp-mwsbp = lva_mwsbp.
*    ENDIF.
*    LOOP AT xkomv.
*      IF xkomv-kschl = 'ICMI'.
*        xkomv-kwert = komp-mwsbp + komp-netwr.
*        MODIFY xkomv.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*ENDIF.
* CSB - 28.09.2021 - Comentado - Fim
ENDENHANCEMENT.
