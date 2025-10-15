"Name: \PR:SAPMM07M\FO:BWART_WE\SE:BEGIN\EI
ENHANCEMENT 0 Z_MB0A_MOV.
*
  IF sy-tcode = 'MB0A'.
    IF RM07M-vbeln is not INITIAL and not ( 'ZX3_861' CS RM07M-BWARTWE ).
        MESSAGE E000(Z01) WITH  'Tipo de movimento inválido'
                                'para entrada via remessa! (861/ZX3)'.
    ENDIF.

  ENDIF.
ENDENHANCEMENT.
