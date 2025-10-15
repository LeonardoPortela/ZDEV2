"Name: \PR:SAPMM07R\FO:OK-CODE_PRUEFEN\SE:BEGIN\EI
ENHANCEMENT 0 Z_CHECA_CC_311_4.
*
** Ini - RJF - 15-04-2024 - Adicionar campos na ZMM0077 - 137599
*IF SY-TCODE EQ 'MB21' OR SY-TCODE EQ 'MB22'.
*
*  LOOP AT YRSEG INTO DATA(lw_YRSEG).
*    IF lw_YRSEG-LGORT IS INITIAL.
*      DATA(LV_SAIR) = ABAP_TRUE.
*    ENDIF.
*  ENDLOOP.
*
*  IF LV_SAIR IS NOT INITIAL.
*    MESSAGE E897(SD) WITH   'Informe o depósito para '
*                            lw_YRSEG-MATNR '/'
*                            lw_YRSEG-WERKS DISPLAY LIKE 'E'.
**    EXIT.
*  ENDIF.
*ENDIF.
** Fim - RJF - 15-04-2024 - Adicionar campos na ZMM0077 - 137599
ENDENHANCEMENT.
