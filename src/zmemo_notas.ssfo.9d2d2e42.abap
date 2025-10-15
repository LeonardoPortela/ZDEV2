data: wl_spell type spell,
      wl_qtd_total(20),
      wl_count type i.

wl_qtd_total = WG_QTD_TOTAL.
wl_count = strlen( Wl_QTD_TOTAL ).
SUBTRACT 4 from wl_count.


CALL FUNCTION 'SPELL_AMOUNT'
 EXPORTING
   AMOUNT          = wl_qtd_total(wl_count)
 IMPORTING
   IN_WORDS        = wl_spell.

move: wl_spell-word to wg_qtd_ext.
CONCATENATE wl_spell-word WA_T006A-msehl into wg_qtd_ext SEPARATED BY space.






















