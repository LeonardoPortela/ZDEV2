*&---------------------------------------------------------------------*
*& Include ZFIR0100_INIT
*&---------------------------------------------------------------------*

TABLES: ekpo,zsdt0225.

SELECTION-SCREEN:
BEGIN OF BLOCK a WITH FRAME TITLE TEXT-001.
PARAMETERS:       s_lote  TYPE zsdt0225-id_seq OBLIGATORY.
PARAMETERS:       s_bukrs  TYPE ekpo-bukrs OBLIGATORY.
PARAMETERS:       s_werks  TYPE ekpo-werks OBLIGATORY.
SELECTION-SCREEN END OF BLOCK a.

START-OF-SELECTION.

  PERFORM pega_dados.

  "IF t_saida1[] IS NOT INITIAL.

    PERFORM container.
    CALL SCREEN 0100.

  "ELSE.

*    CLEAR: lv_message,t_saida1[].
*    lv_message =  |Não Existem dados para esta Empresa!|.
*    MESSAGE lv_message TYPE 'I'.
*
*    STOP.

  "ENDIF.
