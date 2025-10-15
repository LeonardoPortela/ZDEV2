*&---------------------------------------------------------------------*
*& Report  ZFIR053
*&
*&---------------------------------------------------------------------*
*&
*& ATUALIZA BUKRS IS INITIAL NA TABELA ZFIT0083
*& ATUALIZA CAMPOS ERRADOS ZFIT0083
*&---------------------------------------------------------------------*

REPORT  ZFIR053.


DATA: IT_0083 TYPE TABLE OF ZFIT0083,
      WA_0083 TYPE ZFIT0083,
      P_STRING TYPE CHAR10.

*SELECT * FROM ZFIT0083
*  INTO TABLE IT_0083
*  WHERE BUKRS EQ '' .
*
*CLEAR WA_0083.
*
*LOOP AT IT_0083 INTO WA_0083.
*  CLEAR P_STRING.
*  P_STRING = WA_0083-ALLOC_ACCOUNT.
*
*  IF P_STRING(6) EQ 'AMAGGI' OR P_STRING EQ ''.
*    WA_0083-BUKRS = '0001'.
*  ELSEIF P_STRING(7) CS 'HERMASA'.
*    WA_0083-BUKRS = '0010'.
*  ELSEIF P_STRING(10) CS 'UNITAPAJOS'.
*    WA_0083-BUKRS = '0039'.
*  ELSEIF P_STRING(4) CS 'AGRO'.
*    WA_0083-BUKRS = '0015'.
*  ENDIF.
*
*  UPDATE ZFIT0083 SET BUKRS = WA_0083-BUKRS
*                    WHERE BUKRS EQ '' AND
*                          MSG_TYPE    EQ  WA_0083-MSG_TYPE
*                      AND DEAL_TYPE   EQ  WA_0083-DEAL_TYPE
*                      AND SIDE        EQ  WA_0083-SIDE
*                      AND PRODUCT     EQ  WA_0083-PRODUCT
*                      AND SOURCE_REF  EQ  WA_0083-SOURCE_REF
*                      AND TRANS_TYPE  EQ  WA_0083-TRANS_TYPE
*                      AND REV_TRADE   EQ  WA_0083-REV_TRADE
*                      AND TRADE_ID    EQ  WA_0083-TRADE_ID
*                      AND BLOCK_ID    EQ  WA_0083-BLOCK_ID
*                      AND TRADER_ID   EQ  WA_0083-TRADER_ID
*                      AND TRADER_NAME EQ  WA_0083-TRADER_NAME
*                      AND COUNTERPARTY_ID EQ  WA_0083-COUNTERPARTY_ID
*                      AND COUNTERPARTY_NAM EQ  WA_0083-COUNTERPARTY_NAM
*                      AND DATE_OF_DEAL    EQ  WA_0083-DATE_OF_DEAL
*                      AND TIME_OF_DEAL    EQ  WA_0083-TIME_OF_DEAL.
*
*
*ENDLOOP.

*COMMIT WORK.



*-----------------------------------------------------------------------
*  Tabelas internas e Work areas
*-----------------------------------------------------------------------
*
TYPES: BEGIN OF TY_ARQUIVO,
          SOURCE_REF  TYPE ZFIT0083-SOURCE_REF ,     " Empresa
          TRADE_ID    TYPE ZFIT0083-TRADE_ID   ,     " Empresa
          NOTE_TEXT_1 TYPE ZFIT0083-NOTE_TEXT_1,     " Empresa
          NOTE_TEXT_2 TYPE ZFIT0083-NOTE_TEXT_2,     " Empresa
        END   OF TY_ARQUIVO.

DATA: VL_FILENAME      TYPE STRING,
      WA_ARQUIVO       TYPE TY_ARQUIVO,
      IT_ARQUIVO       LIKE STANDARD TABLE OF WA_ARQUIVO,
      T_ALSMEX_TABLINE LIKE STANDARD TABLE OF ALSMEX_TABLINE WITH HEADER LINE.

DATA:
  BEGIN OF TEST OCCURS 1,
   SOURCE_REF  TYPE ZFIT0083-SOURCE_REF ,     " Empresa
   TRADE_ID    TYPE ZFIT0083-TRADE_ID   ,     " Empresa
   NOTE_TEXT_1 TYPE ZFIT0083-NOTE_TEXT_1,     " Empresa
   NOTE_TEXT_2 TYPE ZFIT0083-NOTE_TEXT_2,     " Empresa
  END OF TEST.


DATA:
  W_ROW        TYPE I,
  W_LINE(50)   TYPE C,
  W_VALUES     TYPE I,
  W_TYPE       TYPE C,
  W_TIMES      TYPE I,
  WA_TEST      TYPE TY_ARQUIVO.



DESCRIBE FIELD TEST TYPE W_TYPE COMPONENTS W_VALUES.

CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
  EXPORTING
    FILENAME                = 'C:\Users\camila.brand.MAGGI\Desktop\Ajuste Blooberg\ajuste_BBG.xlsx'
    I_BEGIN_COL             = 1
    I_BEGIN_ROW             = 1
    I_END_COL               = W_VALUES
    I_END_ROW               = 1115
  TABLES
    INTERN                  = T_ALSMEX_TABLINE
  EXCEPTIONS
    INCONSISTENT_PARAMETERS = 1
    UPLOAD_OLE              = 2
    OTHERS                  = 3.

IF SY-SUBRC <> 0.
*  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.


DESCRIBE TABLE T_ALSMEX_TABLINE.

W_TIMES = SY-TFILL DIV W_VALUES.

W_ROW = 0.

DO W_TIMES TIMES.
  W_ROW = W_ROW + 1.

  LOOP AT T_ALSMEX_TABLINE WHERE ROW = W_ROW .
    CONCATENATE W_LINE T_ALSMEX_TABLINE-VALUE
           INTO W_LINE SEPARATED BY SPACE.

  ENDLOOP.

  SHIFT W_LINE LEFT.

  SPLIT W_LINE AT SPACE INTO  TEST-SOURCE_REF
                              TEST-TRADE_ID
                              TEST-NOTE_TEXT_1
                              TEST-NOTE_TEXT_2.
  APPEND TEST.
  WRITE:
  /  TEST-SOURCE_REF ,
     TEST-TRADE_ID   ,
     TEST-NOTE_TEXT_1,
     TEST-NOTE_TEXT_2.


  CLEAR: TEST,
         W_LINE.
ENDDO.

LOOP AT TEST INTO WA_TEST.

  UPDATE ZFIT0083 SET NOTE_TEXT_1 = WA_TEST-NOTE_TEXT_1  NOTE_TEXT_2 = WA_TEST-NOTE_TEXT_2
                      WHERE SOURCE_REF  EQ  WA_TEST-SOURCE_REF.
  CLEAR: WA_TEST.
ENDLOOP.

COMMIT WORK.
