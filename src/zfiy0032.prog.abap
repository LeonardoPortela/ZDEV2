*&---------------------------------------------------------------------*
*& Report  ZFIY0032
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZFIY0032.

TABLES: REGUP.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS FOR REGUP-ZBUKR OBLIGATORY DEFAULT '0100',
                S_LAUFD FOR REGUP-LAUFD OBLIGATORY NO INTERVALS NO-EXTENSION,
                S_LAUFI FOR REGUP-LAUFI,
                S_LIFNR FOR REGUP-LIFNR.
SELECTION-SCREEN: END OF BLOCK B1.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*
INCLUDE ZFIY0032_TOP.
INCLUDE ZFIY0032_CLASS.
INCLUDE ZFIY0032_FORM.
INCLUDE ZFIY0032_PBO.
INCLUDE ZFIY0032_PAI.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM INICIAR_VARIAVEIS.
  PERFORM SELECIONA_DADOS.

  IF VG_NOT_FOUND IS NOT INITIAL.
    EXIT.
  ENDIF.

  PERFORM PROCESSA_DADOS.
  PERFORM IMPRIME_DADOS.
