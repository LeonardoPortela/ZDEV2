*&---------------------------------------------------------------------*
*& Report  ZGL033
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zgl033.

INCLUDE zgl033_top.
INCLUDE zgl033_form.



AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

  variante-report = sy-repid.

  IF ( p_varia IS NOT INITIAL ).
    vg_variant-variant = p_varia.
  ENDIF.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = variante
      i_save        = 'A'
    IMPORTING
      es_variant    = variante
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE s000(z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE variante-variant TO p_varia.
    MOVE variante-variant TO gs_variant_c-variant.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_stipo-low.

  PERFORM help_search USING 'S_STIPO-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_slcto-low.

  PERFORM help_search USING 'S_SLCTO-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_slcto-high.

  PERFORM help_search USING 'S_SLCTO-HIGH'.


*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM iniciar_variaveis.
  PERFORM seleciona_dados.

  IF vg_not_found IS NOT INITIAL.
    EXIT.
  ENDIF.

  PERFORM processa_dados.
  PERFORM imprime_dados.
