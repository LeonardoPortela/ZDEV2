data: ls_spell type spell.

CALL FUNCTION 'SPELL_AMOUNT'
  EXPORTING
    amount    = WK_FRETE-ADIANTAMENTO
    currency  = 'BRL'
    language  = 'P'
  IMPORTING
    in_words  = ls_spell.

CONCATENATE '(' ls_spell-word 'REAIS' ')'
into vg_word SEPARATED BY space.






















