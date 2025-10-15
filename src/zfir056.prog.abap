*&---------------------------------------------------------------------*
*& Report  ZFIR056
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFIR056.

TABLES: ZFIT0098, ZFIT0099, ZFIT0100, ZFIT0104, BSAD.

"Variaveis

DATA: R_ST1  TYPE C LENGTH 1 VALUE 'X',
      R_ST2  TYPE C LENGTH 1.

RANGES:  P_EMP     FOR ZFIT0099-EMPRESA, " Empresa.
         P_LOTE    FOR ZFIT0099-LOTEAMENTO  , " Loteamento
         P_QUADRA  FOR ZFIT0099-NRO_QUADRA  , " Quadra
         P_TERR    FOR ZFIT0099-NRO_TERRENO . " Terreno


*SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS:
*    P_EMP     FOR ZFIT0099-EMPRESA     NO INTERVALS NO-EXTENSION OBLIGATORY, " Empresa
*    P_LOTE    FOR ZFIT0099-LOTEAMENTO  , " Loteamento
*    P_QUADRA  FOR ZFIT0099-NRO_QUADRA  , " Quadra
*    P_TERR    FOR ZFIT0099-NRO_TERRENO . " Terreno
*
*SELECTION-SCREEN: END OF BLOCK B1.
*
*SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
*PARAMETERS:
*   R_ST1       RADIOBUTTON GROUP TP DEFAULT 'X',
*   R_ST2       RADIOBUTTON GROUP TP.
*SELECTION-SCREEN END   OF BLOCK B2.


"START-OF-SELECTION.

INITIALIZATION.

CALL SCREEN 0100.


INCLUDE ZFIR056_TOP.

INCLUDE ZFIR056_FORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'SEA'.

      PERFORM CONFIG_RANGES.

      PERFORM INICIAR_VARIAVEIS.
      PERFORM SELECIONA_DADOS.
      PERFORM PROCESSA_DADOS.
      PERFORM IMPRIMIR_DADOS.
    WHEN 'PSQ_QD'.
      PERFORM PSQ_QD_LOW.
    WHEN 'PSQ_QD_HIGH'.
      PERFORM PSQ_QD_HIGH.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
