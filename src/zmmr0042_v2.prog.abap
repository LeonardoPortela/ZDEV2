
REPORT zmmr0042_v2.

**********************************************************************
* INCLUDES
**********************************************************************
INCLUDE ZMMR0042_V2_TOP.
INCLUDE ZMMR0042_V2_CLASS.
INCLUDE ZMMR0042_V2_SCREEN.
INCLUDE ZMMR0042_V2_FORMS.

**********************************************************************
* START-OF-SELECTION
**********************************************************************
START-OF-SELECTION.

  PERFORM f_selecao_dados.

  IF git_zppt0002[] IS INITIAL.
    MESSAGE s024(sd) WITH text-100 DISPLAY LIKE 'W'.
    STOP.
  ENDIF.

  PERFORM f_processa_dados.

  PERFORM f_alv_saida.

**********************************************************************
**********************************************************************
