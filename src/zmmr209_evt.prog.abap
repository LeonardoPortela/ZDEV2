*&---------------------------------------------------------------------*
*& Include          ZMMR209_EVT
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.

  PERFORM f_trata_exibicao_campos.

START-OF-SELECTION.

  PERFORM f_valida_obrigatoriedade.

  IF gv_obrig IS NOT INITIAL.
    CLEAR gv_obrig.
    RETURN.
  ENDIF.

  PERFORM f_limpa_variaveis.
  PERFORM f_seleciona_dados.
  PERFORM f_monta_dados.
  PERFORM f_exibe_dados.
