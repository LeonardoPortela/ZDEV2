*&---------------------------------------------------------------------*
*& Report  ZFIR0066
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFIR0066.

TABLES: ZFIT0079.



*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS FOR ZFIT0079-bukrs OBLIGATORY,
                S_ZFBDT FOR ZFIT0079-zfbdt NO-DISPLAY.
SELECTION-SCREEN: END OF BLOCK B1.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*
INCLUDE ZFIR0066_TOP.
INCLUDE ZFIR0066_FORM.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM SELECIONA_DADOS.

  IF VG_ERR_CONSULTA IS INITIAL.

    PERFORM: PROCESSA_DADOS,
             GRAVA_DADOS.

  ENDIF.
