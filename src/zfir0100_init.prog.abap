*&---------------------------------------------------------------------*
*& Include ZFIR0100_INIT
*&---------------------------------------------------------------------*

TABLES: acdoca,zib_contabil,sscrfields.

SELECTION-SCREEN:
BEGIN OF BLOCK a WITH FRAME TITLE TEXT-001.
PARAMETERS:       p_emp  TYPE bukrs OBLIGATORY.
PARAMETERS:       p_ano  TYPE gjahr OBLIGATORY.
PARAMETERS:       p_mes(2)  TYPE c OBLIGATORY.
SELECTION-SCREEN END OF BLOCK a.


START-OF-SELECTION.

  PERFORM grava_check_contas.

  PERFORM pega_dados.

  IF it_saida IS NOT INITIAL.

    PERFORM container.
    CALL SCREEN 0100.

  ELSE.

    CLEAR: lv_message,it_acdoca.
    lv_message =  |Não Existem dados para esta Empresa/Período!|.
    MESSAGE lv_message TYPE 'I'.

    STOP.

  ENDIF.
