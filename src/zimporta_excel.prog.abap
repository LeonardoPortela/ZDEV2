*&---------------------------------------------------------------------*
*& Report  ZIMPORTA_EXCEL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZIMPORTA_EXCEL.

TYPES: BEGIN OF TY_0041.
        INCLUDE TYPE ZGLT041.
TYPES: END OF TY_0041.

TYPES: BEGIN OF TY_ERRO,
         MSG(100),
       END OF TY_ERRO.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: TI_BDCDATA TYPE STANDARD TABLE OF BDCDATA,   "Guarda o mapeamento
      IT_ERRO    TYPE TABLE OF TY_ERRO.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WA_BDCDATA LIKE LINE OF TI_BDCDATA,
      WA_0041    TYPE TY_0041,
      WA_ERRO    TYPE TY_ERRO.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MSG.
DATA: WL_MODE(1),
      WG_DOCUMENTO(10),
      VMSG(50),
      WL_ERRO(1),
      WL_SEQ           TYPE I,
      WL_SEQ1          TYPE C LENGTH 11,
      WL_SEQ2(5),
      WA_SKB1          TYPE SKB1.



PARAMETER P_FILE TYPE RLGRAP-FILENAME DEFAULT 'C:\maggi\planilha.xlsx'.

DATA: T_EXCEL  LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE,
      T_EXCEL2 LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.


CLEAR T_EXCEL.
REFRESH T_EXCEL.

CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
  EXPORTING
    FILENAME                = P_FILE
    I_BEGIN_COL             = 1
    I_BEGIN_ROW             = 3
    I_END_COL               = 11
    I_END_ROW               = 10000
  TABLES
    INTERN                  = T_EXCEL
  EXCEPTIONS
    INCONSISTENT_PARAMETERS = 1
    UPLOAD_OLE              = 2
    OTHERS                  = 3.

CASE SY-SUBRC.
  WHEN 1.
    MESSAGE 'Parâmetros inválidos.' TYPE 'E'.
  WHEN 2.
    MESSAGE 'Não foi possível ler o arquivo, verifique o caminho e tente novamente.' TYPE 'E'.
  WHEN 3.
    MESSAGE 'Erro ao importar o arquivo.' TYPE 'E'.
ENDCASE.

CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
  EXPORTING
    TEXT = 'Atualizando Dados'.

T_EXCEL2[] = T_EXCEL[].

SORT T_EXCEL2 BY ROW COL.

CLEAR: T_EXCEL2, WL_SEQ.

LOOP AT T_EXCEL.
  IF T_EXCEL-ROW = T_EXCEL2-ROW.
    CONTINUE.
  ENDIF.
  LOOP AT T_EXCEL2 WHERE ROW = T_EXCEL-ROW.
    CASE T_EXCEL2-COL.
      WHEN 1.
        "Empresa
        WA_0041-BUKRS            = T_EXCEL2-VALUE.
*      WHEN 2.
*        "Ano
*        WA_0041-GJAHR            = T_EXCEL2-VALUE.
      WHEN 2.
        "Conta
        WA_0041-SAKNR            = T_EXCEL2-VALUE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_0041-SAKNR
          IMPORTING
            OUTPUT = WA_0041-SAKNR.
      WHEN 4.
        "cod.Balanço
        WA_0041-COD_CLAS_BAL     = T_EXCEL2-VALUE.
      WHEN 5.
        "cod.Nota
        WA_0041-COD_CLAS_NOT2    = T_EXCEL2-VALUE.
      WHEN 6.
        "Cta.Monetaria (S/N)
        WA_0041-CTA_MONET        = T_EXCEL2-VALUE.
      WHEN 7.
        "Cta Intercompany (S/N)
        WA_0041-CTA_INTERCOMPANY = T_EXCEL2-VALUE.
      WHEN 8.
        "Cod.Departamento
        WA_0041-DEP_RESP2        = T_EXCEL2-VALUE.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_0041-DEP_RESP2
          IMPORTING
            OUTPUT = WA_0041-DEP_RESP2.
      WHEN 9.
        "Usuario responsavel
        WA_0041-BNAME2           = T_EXCEL2-VALUE.
      WHEN 10.
        "Prazo Entrega
        WA_0041-PRAZO_ENTR       = T_EXCEL2-VALUE.
      WHEN 11.
        "Criterio Vcto Partidas em aberto
        WA_0041-CRIT_VECTO       = T_EXCEL2-VALUE.
    ENDCASE.
  ENDLOOP.

  ADD 1 TO WL_SEQ.

  WL_SEQ1 = WL_SEQ.

  CONCATENATE 'Gravando linha:' WL_SEQ1 INTO VMSG.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = VMSG.

  SELECT SINGLE * INTO WA_SKB1 "#EC CI_DB_OPERATION_OK[2431747]
    FROM SKB1
   WHERE BUKRS EQ WA_0041-BUKRS
     AND SAKNR EQ WA_0041-SAKNR.

  IF SY-SUBRC IS INITIAL.
    INSERT INTO ZGLT041 VALUES WA_0041.
  ENDIF.

ENDLOOP.

MESSAGE 'Fim atualização' TYPE 'I'.
