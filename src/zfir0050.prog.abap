*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski Tavares                              &*
*& Data.....: 01/06/2014                                              &*
*& Descrição: Fluxo Financeiro – Conta Contábil X Fluxo Financeiro    &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT  ZFIR0050.

TYPES: BEGIN OF TY_ZFIT0077,
         COD_FLX  TYPE ZFIT0077-COD_FLX,
       END OF TY_ZFIT0077,

       BEGIN OF TY_ZFIT0078,
         SAKNR    TYPE ZFIT0078-SAKNR,
         COD_FLX  TYPE ZFIT0078-COD_FLX,
         MATKL    TYPE ZFIT0078-MATKL,
         PRCTR    TYPE ZFIT0078-PRCTR,
         USNAM    TYPE ZFIT0078-USNAM,
         DT_ATUAL TYPE ZFIT0078-DT_ATUAL,
         HR_ATUAL TYPE ZFIT0078-HR_ATUAL,
       END OF TY_ZFIT0078,

       BEGIN OF TY_SKAT,
         KTOPL TYPE SKAT-KTOPL,
         SAKNR TYPE SKAT-SAKNR,
         TXT50 TYPE SKAT-TXT50,
       END OF TY_SKAT.

DATA: T_0077 TYPE TABLE OF TY_ZFIT0077,
      T_0078 TYPE TABLE OF TY_ZFIT0078,
      T_SKAT TYPE TABLE OF TY_SKAT.

DATA: WA_0077 TYPE TY_ZFIT0077,
      WA_0078 TYPE TY_ZFIT0078,
      WA_SKAT TYPE TY_SKAT.

CALL SCREEN 100.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_HEADER' ITSELF
CONTROLS: TC_HEADER TYPE TABLEVIEW USING SCREEN 0100.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_HEADER'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TC_HEADER_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE T_0078 LINES TC_HEADER-LINES.
ENDMODULE.                    "TC_HEADER_CHANGE_TC_ATTR OUTPUT
*&---------------------------------------------------------------------*
*&      Module  T_DADOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE T_DADOS OUTPUT.
  SELECT COD_FLX
    FROM ZFIT0077
      INTO TABLE T_0077.

  SELECT SAKNR COD_FLX MATKL PRCTR USNAM DT_ATUAL HR_ATUAL
    FROM ZFIT0078
      INTO TABLE T_0078.

    SELECT KTOPL SAKNR TXT50
      FROM SKAT
       INTO TABLE T_SKAT
         WHERE KTOPL EQ '0050'
           AND SPRAS EQ SY-LANGU.

ENDMODULE.                 " T_DADOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'Z001'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
