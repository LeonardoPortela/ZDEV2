"Name: \PR:SAPLV50E\FO:RELATION_TABLE_FILL\SE:END\EI
ENHANCEMENT 0 ZSD_FATURAMENTO_ARGENTINA3.
*
*-#130592-10.01.2024-JT- inicio
*------- Zuordnen der Basisbelegnummer
  CASE h_ahbas.
    WHEN con_ahbas-2fa.
      LOOP AT h_int_tab_relation.
        IF h_xvbrk-exnum EQ h_int_tab_relation-exnum.
          CLEAR: sy-subrc.
        ELSE.
          READ TABLE h_xvbrk
                     WITH KEY exnum = h_int_tab_relation-exnum.
        ENDIF.
        IF sy-subrc IS INITIAL.
          h_int_tab_relation-ahbas = h_ahbas.
          h_int_tab_relation-belnr = h_xvbrk-vbeln.
          MODIFY h_int_tab_relation.
        ELSE.
          DELETE h_int_tab_relation.
        ENDIF.
      ENDLOOP.
  ENDCASE.

*------- Sortieren der Tabelle
  SORT h_int_tab_relation BY exnum belnr ahbas.
*-#130592-10.01.2024-JT- fim
*
ENDENHANCEMENT.
