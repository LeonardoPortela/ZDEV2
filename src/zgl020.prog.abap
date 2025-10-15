*&---------------------------------------------------------------------*
*& Report  ZGL020
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZGL020.

TABLES: SKB1, BSIS.
INCLUDE <CL_ALV_CONTROL>.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS, KKBLO.
INCLUDE: <ICON>.

DATA: BEGIN OF TG_SKA1 OCCURS 0,
        KTOPL LIKE SKA1-KTOPL,
        SAKNR LIKE SKA1-SAKNR,
        KTOKS LIKE SKA1-KTOKS,
        TXT50 LIKE SKAT-TXT50,
      END OF TG_SKA1.

DATA: BEGIN OF TG_SKB1 OCCURS 0,
        BUKRS LIKE SKB1-BUKRS,
        SAKNR LIKE SKB1-SAKNR,
        MITKZ LIKE SKB1-MITKZ,
        XSPEB LIKE SKB1-XSPEB,
      END OF TG_SKB1.

DATA: BEGIN OF TG_ZGLT039 OCCURS 0.
        INCLUDE STRUCTURE ZGLT039.
DATA:  CHAVE1(40),
       CHAVE2(40).
DATA: END OF TG_ZGLT039.

DATA: BEGIN OF TG_ZIMP_CAD_DEPTO OCCURS 0.
        INCLUDE STRUCTURE ZIMP_CAD_DEPTO.
DATA: END OF TG_ZIMP_CAD_DEPTO.

DATA: BEGIN OF TG_USREFUS OCCURS 0.
        INCLUDE STRUCTURE USREFUS.
DATA: END OF TG_USREFUS.

DATA: BEGIN OF TG_ZGLT041 OCCURS 0.
        INCLUDE STRUCTURE ZGLT041.
DATA: END OF TG_ZGLT041.

TYPES: BEGIN OF TY_SAIDA.
TYPES:  MARK,
        STATUS              TYPE ICONNAME,
        BUKRS               LIKE SKB1-BUKRS,
        SAKNR               LIKE SKB1-SAKNR,
        TXT50               LIKE SKAT-TXT50,
        KTOKS               LIKE SKA1-KTOKS,
        MITKZ               LIKE SKB1-MITKZ,
        XSPEB               LIKE SKB1-XSPEB,
        COD_CLAS_BAL(40),
        COD_NOTA            LIKE ZGLT039-COD_NOTA,
        COD_CLAS_NOT2(40),
        CTA_MONET(7),  "          LIKE ZGLT041-CTA_MONET,
        CTA_INTERCOMPANY(7),  "   LIKE ZGLT041-CTA_INTERCOMPANY,
        DEP_RESP2           LIKE ZGLT041-DEP_RESP2,
        DEP_RESP_DESC       LIKE ZIMP_CAD_DEPTO-DEP_RESP_DESC,
        BNAME2              LIKE ZGLT041-BNAME2,
        USERALIAS           LIKE USREFUS-USERALIAS,
        CRIT_VECTO          LIKE ZGLT041-CRIT_VECTO,
        PRAZO_ENTR          LIKE ZGLT041-PRAZO_ENTR,
        CRITERIO            TYPE ICONNAME.
*        GJAHR               TYPE BSIS-GJAHR. "/Modificação CS2017000372
TYPES: END OF TY_SAIDA.

DATA: TG_SAIDA     TYPE TABLE OF TY_SAIDA WITH HEADER LINE,
      TG_SAIDA_DEL TYPE TABLE OF TY_SAIDA WITH HEADER LINE,

      BEGIN OF WG_T001,
        BUKRS TYPE T001-BUKRS,
        BUTXT TYPE T001-BUTXT,
      END OF WG_T001.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

TYPES: BEGIN OF TY_AUX,
         BUKRS             LIKE SKB1-BUKRS,
*         GJAHR             LIKE BSIS-GJAHR, "/Modificação CS2017000372
         SAKNR             LIKE SKB1-SAKNR,
         TXT50             LIKE SKAT-TXT50,
         KTOKS             LIKE SKA1-KTOKS,
         MITKZ             LIKE SKB1-MITKZ,
         XSPEB             LIKE SKB1-XSPEB,
         COD_CLAS_BAL(40),
         COD_CLAS_NOT2(40),
         TDFORMAT          TYPE TLINE-TDFORMAT,
         TDLINE            TYPE TLINE-TDLINE,
       END OF TY_AUX.

*TYPES: BEGIN OF TY_GJAHR. "/Modificação CS2017000372
*TYPES: GJAHR TYPE BSIS-GJAHR.
*TYPES: END OF TY_GJAHR.

DATA: BEGIN OF GT_VALUES OCCURS 0,
        DOMVALUE_L TYPE DOMVALUE_L,
        DDTEXT     TYPE VAL_TEXT,
      END OF GT_VALUES.

DATA: TG_AUX    TYPE TABLE OF TY_AUX,
      WA_AUX    TYPE TY_AUX,
      WA_SAIDA  LIKE LINE OF TG_SAIDA,
      TG_TEXTO  TYPE TABLE OF TLINE WITH HEADER LINE,
      VG_CONT   TYPE SY-TABIX,
      VG_CONT_A TYPE SY-TABIX.

*DATA: IT_GJAHR TYPE STANDARD TABLE OF TY_GJAHR, "/Modificação CS2017000372
*      WG_GJAHR TYPE BSIS-GJAHR. "/Modificação CS2017000372

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE          SLIS_ALV_EVENT,
      EVENTS       TYPE          SLIS_T_EVENT,
      T_PRINT      TYPE          SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE          TY_ESTRUTURA,
      V_REPORT     LIKE          SY-REPID,
      T_TOP        TYPE          SLIS_T_LISTHEADER,
      WL_LAYOUT    TYPE          SLIS_LAYOUT_ALV,
      INIT.

*----------------------------------------------------------------------*
* ESTRUTURA TEXTO
*----------------------------------------------------------------------*

DATA :  IT_FLINES TYPE TABLE OF TLINE,
        ST_FLINES TYPE TLINE.
*----------------------------------------------------------------------*
* OBJETOS/CLASSES
*----------------------------------------------------------------------*
DATA: REF1 TYPE REF TO CL_GUI_ALV_GRID,
      REF2 TYPE REF TO CL_GUI_ALV_GRID.
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS: HANDLE_ON_BUTTON_CLICK FOR EVENT BUTTON_CLICK OF CL_GUI_ALV_GRID
      IMPORTING ES_COL_ID
                  ES_ROW_NO.

    CLASS-METHODS: ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
      IMPORTING ER_DATA_CHANGED.

  PRIVATE SECTION.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD HANDLE_ON_BUTTON_CLICK.

    DATA: TL_TEXTO TYPE CATSXT_LONGTEXT_ITAB,
          WL_TEXTO TYPE LINE OF CATSXT_LONGTEXT_ITAB,
          WL_FIELD TYPE LVC_S_COL,
          V_CONT   TYPE I.

    DATA: LS_SEL_HIDE TYPE SLIS_SEL_HIDE_ALV,
          IS_TABLE    TYPE LVC_S_STBL.

    REFRESH TL_TEXTO.
    CLEAR:WL_TEXTO.

    READ TABLE TG_SAIDA INTO WA_SAIDA INDEX ES_ROW_NO-ROW_ID.
    LOOP AT TG_AUX INTO WA_AUX WHERE BUKRS         EQ WA_SAIDA-BUKRS
*                                 AND GJAHR         EQ WA_SAIDA-GJAHR "/Modificação CS2017000372
                                 AND SAKNR         EQ WA_SAIDA-SAKNR
                                 AND TXT50         EQ WA_SAIDA-TXT50
                                 AND KTOKS         EQ WA_SAIDA-KTOKS
                                 AND MITKZ         EQ WA_SAIDA-MITKZ
                                 AND XSPEB         EQ WA_SAIDA-XSPEB
                                 AND COD_CLAS_BAL  EQ WA_SAIDA-COD_CLAS_BAL
                                 AND COD_CLAS_NOT2 EQ WA_SAIDA-COD_CLAS_NOT2.

      MOVE: WA_AUX-TDLINE   TO WL_TEXTO.

      APPEND WL_TEXTO TO TL_TEXTO.
      CLEAR: WL_TEXTO.
    ENDLOOP.

    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        IM_TITLE = 'Critérios Reconciliação'           "" Título
      CHANGING
        CH_TEXT  = TL_TEXTO.

    IF SY-UCOMM EQ 'CX_CONT'.
      IF TL_TEXTO[] IS NOT INITIAL.
        READ TABLE TG_SAIDA INTO WA_SAIDA INDEX ES_ROW_NO-ROW_ID.
        MOVE ICON_TEXT_ACT TO WA_SAIDA-CRITERIO.
        MODIFY  TG_SAIDA FROM WA_SAIDA INDEX ES_ROW_NO-ROW_ID TRANSPORTING CRITERIO.

        READ TABLE TG_SAIDA INTO WA_SAIDA INDEX ES_ROW_NO-ROW_ID.
        DELETE TG_AUX WHERE BUKRS EQ WA_SAIDA-BUKRS
*                       AND GJAHR EQ WA_SAIDA-GJAHR "/Modificação CS2017000372
                       AND SAKNR EQ WA_SAIDA-SAKNR
                       AND TXT50 EQ WA_SAIDA-TXT50
                       AND KTOKS EQ WA_SAIDA-KTOKS
                       AND MITKZ EQ WA_SAIDA-MITKZ
                       AND XSPEB EQ WA_SAIDA-XSPEB
                       AND COD_CLAS_BAL  EQ WA_SAIDA-COD_CLAS_BAL
                       AND COD_CLAS_NOT2 EQ WA_SAIDA-COD_CLAS_NOT2.


        LOOP AT TL_TEXTO INTO WL_TEXTO.
          MOVE: WA_SAIDA-BUKRS         TO WA_AUX-BUKRS,
*                WA_SAIDA-GJAHR         TO WA_AUX-GJAHR, "/Modificação CS2017000372
                WA_SAIDA-SAKNR         TO WA_AUX-SAKNR,
                WA_SAIDA-TXT50         TO WA_AUX-TXT50,
                WA_SAIDA-KTOKS         TO WA_AUX-KTOKS,
                WA_SAIDA-MITKZ         TO WA_AUX-MITKZ,
                WA_SAIDA-XSPEB         TO WA_AUX-XSPEB,
                WA_SAIDA-COD_CLAS_BAL  TO WA_AUX-COD_CLAS_BAL,
                WA_SAIDA-COD_CLAS_NOT2 TO WA_AUX-COD_CLAS_NOT2,
                '*'                    TO WA_AUX-TDFORMAT,
                WL_TEXTO               TO WA_AUX-TDLINE.

          APPEND WA_AUX TO TG_AUX.
          CLEAR:WA_AUX.
        ENDLOOP.
      ELSE.
        DELETE TG_AUX WHERE BUKRS EQ WA_SAIDA-BUKRS
*                       AND GJAHR EQ WA_SAIDA-GJAHR "/Modificação CS2017000372
                       AND SAKNR EQ WA_SAIDA-SAKNR
                       AND TXT50 EQ WA_SAIDA-TXT50
                       AND KTOKS EQ WA_SAIDA-KTOKS
                       AND MITKZ EQ WA_SAIDA-MITKZ
                       AND XSPEB EQ WA_SAIDA-XSPEB
                       AND COD_CLAS_BAL  EQ WA_SAIDA-COD_CLAS_BAL
                       AND COD_CLAS_NOT2 EQ WA_SAIDA-COD_CLAS_NOT2.
        MOVE ICON_TEXT_INA TO WA_SAIDA-CRITERIO.
        MODIFY  TG_SAIDA FROM WA_SAIDA INDEX ES_ROW_NO-ROW_ID TRANSPORTING CRITERIO.
      ENDIF.
    ENDIF.


    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        ES_SEL_HIDE = LS_SEL_HIDE
        E_GRID      = REF1.

    CALL METHOD REF1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = IS_TABLE.

    CALL METHOD CL_GUI_CFW=>DISPATCH.
    CALL METHOD CL_GUI_CFW=>FLUSH.

  ENDMETHOD.                    "HANDLE_ON_BUTTON_CLICK.

  METHOD ON_DATA_CHANGED.
    DATA: LS_GOOD      TYPE LVC_S_MODI,
          LV_VALUE     TYPE LVC_VALUE,
          VL_VALUE     TYPE LVC_VALUE,
          WL_USREFUS   TYPE USREFUS,
          WL_CAD_DEPTO TYPE ZIMP_CAD_DEPTO.


    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'BNAME2'.


      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      WL_USREFUS-BNAME = LV_VALUE.

      SELECT SINGLE *
        FROM USREFUS
        INTO WL_USREFUS
         WHERE BNAME EQ WL_USREFUS-BNAME.
      IF SY-SUBRC IS NOT INITIAL.
        CLEAR WL_USREFUS.
      ENDIF.

      LV_VALUE = WL_USREFUS-USERALIAS.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'USERALIAS'
          I_VALUE     = LV_VALUE.

      LV_VALUE = WL_USREFUS-BNAME.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'BNAME2'
          I_VALUE     = LV_VALUE.
*
    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'DEP_RESP2'.


      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      WL_CAD_DEPTO-DEP_RESP = LV_VALUE.

      SELECT SINGLE *
        FROM ZIMP_CAD_DEPTO
        INTO WL_CAD_DEPTO
         WHERE DEP_RESP EQ WL_CAD_DEPTO-DEP_RESP.
      IF SY-SUBRC IS NOT INITIAL.
        CLEAR WL_CAD_DEPTO.
      ENDIF.

      LV_VALUE = WL_CAD_DEPTO-DEP_RESP_DESC.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'DEP_RESP_DESC'
          I_VALUE     = LV_VALUE.

      LV_VALUE = WL_CAD_DEPTO-DEP_RESP.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'DEP_RESP2'
          I_VALUE     = LV_VALUE.
*
    ENDLOOP.
  ENDMETHOD.                    "on_data_chaged
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-H01.
SELECT-OPTIONS: S_BUKRS  FOR SKB1-BUKRS,
                S_SKB1   FOR SKB1-SAKNR.
*                S_GJAHR  FOR BSIS-GJAHR OBLIGATORY NO INTERVALS. "/Modificação CS2017000372
SELECTION-SCREEN: END OF BLOCK B1.

START-OF-SELECTION.
  PERFORM VERIFICAR_AUTORIZACAO. " Modificação 08.11.2016
  PERFORM: SELECIONA_DADOS,
           PROCESSA_DADOS.
  PERFORM INICIAR_VARIAVEIS.
  PERFORM IMPRIMIR_DADOS.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS .

* 1 – Busca de dados Conta Contábil
  SELECT SKA1~KTOPL SKA1~SAKNR SKA1~KTOKS "#EC CI_DB_OPERATION_OK[2389136]
         SKAT~TXT50 "#EC CI_DB_OPERATION_OK[2431747]
    FROM SKA1
   INNER JOIN SKAT ON ( SKA1~KTOPL = SKAT~KTOPL AND SKA1~SAKNR = SKAT~SAKNR )
    INTO TABLE TG_SKA1
   WHERE SKA1~KTOPL EQ '0050'
     AND SKAT~SPRAS EQ SY-LANGU
     AND SKA1~SAKNR IN S_SKB1.

  CHECK SY-SUBRC IS INITIAL.


  SELECT BUKRS SAKNR MITKZ XSPEB "#EC CI_DB_OPERATION_OK[2431747]
    FROM SKB1
    INTO TABLE TG_SKB1
    FOR ALL ENTRIES IN TG_SKA1
    WHERE BUKRS IN S_BUKRS
      AND SAKNR EQ TG_SKA1-SAKNR.

  CHECK SY-SUBRC IS INITIAL.

* 4 – Busca de dados Classificação de Balanço
  SELECT * FROM ZGLT039 INTO TABLE TG_ZGLT039.
  LOOP AT TG_ZGLT039.
    IF TG_ZGLT039-CODIGO IS NOT INITIAL.
      CONCATENATE TG_ZGLT039-CODIGO '-' TG_ZGLT039-DESCR INTO TG_ZGLT039-CHAVE1.
    ENDIF.
    IF TG_ZGLT039-COD_NOTA IS NOT INITIAL.
      CONCATENATE TG_ZGLT039-COD_NOTA '-' TG_ZGLT039-DESCR_NOTA INTO TG_ZGLT039-CHAVE2.
    ENDIF.
    MODIFY TG_ZGLT039.
  ENDLOOP.

** 5 – Busca de dados Classificação da Nota
*  SELECT * FROM ZGLT040 INTO TABLE TG_ZGLT040.

* 8 – Busca dados ZGLT041
  SELECT * FROM ZGLT041 INTO TABLE TG_ZGLT041
    FOR ALL ENTRIES IN TG_SKB1
    WHERE BUKRS EQ TG_SKB1-BUKRS
      AND SAKNR EQ TG_SKB1-SAKNR.
*         AND GJAHR IN S_GJAHR. "/Modificação CS2017000372

  IF SY-SUBRC IS INITIAL.
*  Busca de Dados Departamento
    SELECT *
      FROM ZIMP_CAD_DEPTO
      INTO TABLE TG_ZIMP_CAD_DEPTO
       FOR ALL ENTRIES IN TG_ZGLT041
       WHERE DEP_RESP EQ TG_ZGLT041-DEP_RESP2.

*   Busca dos usuários
    SELECT *
      FROM USREFUS
      INTO TABLE TG_USREFUS
      FOR ALL ENTRIES IN TG_ZGLT041
       WHERE BNAME EQ TG_ZGLT041-BNAME2.

  ENDIF.

*  PERFORM SELECIONA_EXERCICIO.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  PROCESSA_DADOS
*&---------------------------------------------------------------------*
FORM PROCESSA_DADOS .
  DATA: WL_NAME  TYPE THEAD-TDNAME,
        CK_REGIS TYPE C LENGTH 1.

  SORT: TG_SKA1 BY KTOPL SAKNR,
        TG_SKB1 BY BUKRS SAKNR,
        TG_ZGLT041 BY BUKRS SAKNR,
        TG_ZIMP_CAD_DEPTO BY DEP_RESP,
        TG_USREFUS BY BNAME.

*  DATA: WA_GJAHR TYPE TY_GJAHR. "/Modificação CS2017000372

  CLEAR: TG_SAIDA.
  REFRESH TG_SAIDA.

*  LOOP AT IT_GJAHR INTO WA_GJAHR. "/Modificação CS2017000372

  LOOP AT TG_SKB1.

    CK_REGIS = ABAP_TRUE.
* Movendo SKB1

    LOOP AT TG_ZGLT041 WHERE BUKRS     =  TG_SKB1-BUKRS
                         AND SAKNR     =  TG_SKB1-SAKNR.
*                           AND GJAHR     =  WA_GJAHR-GJAHR. "/Modificação CS2017000372

      CK_REGIS = ABAP_FALSE.

      MOVE: TG_SKB1-BUKRS  TO TG_SAIDA-BUKRS,
            TG_SKB1-SAKNR  TO TG_SAIDA-SAKNR,
            TG_SKB1-MITKZ  TO TG_SAIDA-MITKZ,
            TG_SKB1-XSPEB  TO TG_SAIDA-XSPEB.
*              WA_GJAHR-GJAHR TO TG_SAIDA-GJAHR. "/Modificação CS2017000372

*   Movendo SKA1.
      READ TABLE TG_SKA1 WITH KEY SAKNR = TG_SKB1-SAKNR
                                  BINARY SEARCH.
      MOVE: TG_SKA1-TXT50 TO TG_SAIDA-TXT50,
            TG_SKA1-KTOKS TO TG_SAIDA-KTOKS.

      TG_SAIDA-STATUS = ICON_CHECKED.
      MOVE-CORRESPONDING TG_ZGLT041 TO TG_SAIDA.

      READ TABLE TG_ZGLT039
        WITH KEY CODIGO = TG_ZGLT041-COD_CLAS_BAL.

      TG_SAIDA-COD_CLAS_BAL = TG_ZGLT039-CHAVE1.

      READ TABLE TG_ZGLT039
        WITH KEY COD_NOTA = TG_ZGLT041-COD_CLAS_NOT2.

      TG_SAIDA-COD_CLAS_NOT2 = TG_ZGLT039-CHAVE2.

      READ TABLE TG_ZIMP_CAD_DEPTO
        WITH KEY DEP_RESP = TG_ZGLT041-DEP_RESP2
                 BINARY SEARCH.

      READ TABLE TG_USREFUS
        WITH KEY BNAME = TG_ZGLT041-BNAME2
                 BINARY SEARCH.

      MOVE: TG_ZGLT041-CTA_MONET             TO TG_SAIDA-CTA_MONET,
            TG_ZGLT041-CTA_INTERCOMPANY      TO TG_SAIDA-CTA_INTERCOMPANY,
            TG_ZGLT041-DEP_RESP2             TO TG_SAIDA-DEP_RESP2,
            TG_ZIMP_CAD_DEPTO-DEP_RESP_DESC  TO TG_SAIDA-DEP_RESP_DESC,
            TG_ZGLT041-BNAME2                TO TG_SAIDA-BNAME2,
            TG_USREFUS-USERALIAS             TO TG_SAIDA-USERALIAS,
            TG_ZGLT041-PRAZO_ENTR            TO TG_SAIDA-PRAZO_ENTR,
            TG_ZGLT041-CRIT_VECTO            TO TG_SAIDA-CRIT_VECTO.

      IF TG_ZGLT041-CTA_MONET EQ 'S'.
        TG_SAIDA-CTA_MONET = 'S - SIM'.
      ELSEIF TG_ZGLT041-CTA_MONET EQ 'N'.
        TG_SAIDA-CTA_MONET = 'N - NÃO'.
      ENDIF.

      IF TG_ZGLT041-CTA_INTERCOMPANY EQ 'S'.
        TG_SAIDA-CTA_INTERCOMPANY = 'S - SIM'.
      ELSEIF TG_ZGLT041-CTA_INTERCOMPANY EQ 'N'.
        TG_SAIDA-CTA_INTERCOMPANY = 'N - NÃO'.
      ENDIF.

      REFRESH: TG_TEXTO.
      CLEAR: WL_NAME.
      CONCATENATE TG_SAIDA-BUKRS TG_SAIDA-SAKNR TG_SAIDA-COD_CLAS_BAL TG_SAIDA-COD_CLAS_NOT2 INTO WL_NAME."/ Modificação CS2017000372 --> TG_SAIDA-GJAHR
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = 'ZCRI'
          LANGUAGE                = SY-LANGU
          NAME                    = WL_NAME
          OBJECT                  = 'ZCRITERIO'
        TABLES
          LINES                   = TG_TEXTO
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.

      IF SY-SUBRC IS INITIAL.
        TG_SAIDA-CRITERIO = ICON_TEXT_ACT.
        LOOP AT TG_TEXTO.
          MOVE: TG_SAIDA-BUKRS    TO WA_AUX-BUKRS,
*                  TG_SAIDA-GJAHR    TO WA_AUX-GJAHR, "/ Modificação CS2017000372
                TG_SAIDA-SAKNR    TO WA_AUX-SAKNR,
                TG_SAIDA-TXT50    TO WA_AUX-TXT50,
                TG_SAIDA-KTOKS    TO WA_AUX-KTOKS,
                TG_SAIDA-MITKZ    TO WA_AUX-MITKZ,
                TG_SAIDA-XSPEB    TO WA_AUX-XSPEB,
                TG_SAIDA-COD_CLAS_BAL  TO WA_AUX-COD_CLAS_BAL,
                TG_SAIDA-COD_CLAS_NOT2 TO WA_AUX-COD_CLAS_NOT2,
                TG_TEXTO-TDFORMAT TO WA_AUX-TDFORMAT,
                TG_TEXTO-TDLINE   TO WA_AUX-TDLINE.

          APPEND WA_AUX TO TG_AUX.
          CLEAR: WA_AUX.

        ENDLOOP.
      ELSE.
        TG_SAIDA-CRITERIO = ICON_TEXT_INA.
      ENDIF.

      APPEND TG_SAIDA.
      CLEAR: TG_SAIDA, TG_ZGLT041, TG_SKA1, TG_USREFUS, TG_ZIMP_CAD_DEPTO.

    ENDLOOP.

    IF CK_REGIS EQ ABAP_TRUE.
      MOVE: TG_SKB1-BUKRS  TO TG_SAIDA-BUKRS,
            TG_SKB1-SAKNR  TO TG_SAIDA-SAKNR,
            TG_SKB1-MITKZ  TO TG_SAIDA-MITKZ,
            TG_SKB1-XSPEB  TO TG_SAIDA-XSPEB.
*              WA_GJAHR-GJAHR TO TG_SAIDA-GJAHR. "/ Modificação CS2017000372

*   Movendo SKA1.
      READ TABLE TG_SKA1 WITH KEY SAKNR = TG_SKB1-SAKNR
                                  BINARY SEARCH.
      MOVE: TG_SKA1-TXT50 TO TG_SAIDA-TXT50,
            TG_SKA1-KTOKS TO TG_SAIDA-KTOKS.
      TG_SAIDA-STATUS = ICON_INCOMPLETE.
      CLEAR: TG_SAIDA-COD_CLAS_BAL.
      CLEAR TG_SAIDA-COD_CLAS_NOT2.
      APPEND TG_SAIDA.
      CLEAR: TG_SAIDA, TG_ZGLT041, TG_SKA1, TG_USREFUS, TG_ZIMP_CAD_DEPTO.
    ENDIF.

  ENDLOOP.

*  ENDLOOP.

ENDFORM.                    " PROCESSA_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DADOS .
  PERFORM DEFINIR_EVENTOS.
  PERFORM MONTAR_LAYOUT.
  WL_LAYOUT-BOX_FIELDNAME = 'MARK'.
  WL_LAYOUT-BOX_TABNAME  = 'TG_SAIDA'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = V_REPORT
      IT_FIELDCAT        = ESTRUTURA[]
      IS_LAYOUT          = WL_LAYOUT
      IT_EVENTS          = EVENTS
    TABLES
      T_OUTTAB           = TG_SAIDA.

ENDFORM.                    "imprimir_dados
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFINIR_EVENTOS.
  PERFORM F_CARREGAR_EVENTOS USING: SLIS_EV_USER_COMMAND  'XUSER_COMMAND',
                                    SLIS_EV_PF_STATUS_SET 'XPF_STATUS_SET'.
ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT.
  PERFORM MONTAR_ESTRUTURA USING:
    1  ' '                 ''                 'TG_SAIDA' 'STATUS'                    'Status'                   ' ' ' ',
    2  'SKB1'             'BUKRS'             'TG_SAIDA' 'BUKRS'                     ' '                        ' ' ' ',
*    2  'BSIS'             'GJAHR'             'TG_SAIDA' 'GJAHR'                     ' '                        ' ' ' ', "/ Modificação CS2017000372
    3  'SKB1'             'SAKNR'             'TG_SAIDA' 'SAKNR'                     ' '                        ' ' ' ',
    4  'SKAT'             'TXT50'             'TG_SAIDA' 'TXT50'                     ' '                        '15' ' ',
    5  'SKA1'             'KTOKS'             'TG_SAIDA' 'KTOKS'                     'Gpo.Ctas'                 '5' ' ',
    6  'SKB1'             'MITKZ'             'TG_SAIDA' 'MITKZ'                     'Tp.Cta'                   '7' ' ',
    7  'SKB1'             'XSPEB'             'TG_SAIDA' 'XSPEB'                     'Status/conta'             '10' ' ',
    8  ' '                ' '                 'TG_SAIDA' 'COD_CLAS_BAL'              'Classif.Balanço'          '14' 'X',
   10  ' '                ' '                 'TG_SAIDA' 'COD_CLAS_NOT2'             'Classif.Nota  '           '14' 'X',
   11  ' '                ' '                 'TG_SAIDA' 'CTA_MONET'                 'Monetária'                '13' 'X',
   12  ' '                ' '                 'TG_SAIDA' 'CTA_INTERCOMPANY'          'Intercompany'             '10' 'X',
   13  'ZGLT041'          'DEP_RESP2'         'TG_SAIDA' 'DEP_RESP2'                 'Departamento'             '14' 'X',
   13  'ZIMP_CAD_DEPTO'   'DEP_RESP_DESC'     'TG_SAIDA' 'DEP_RESP_DESC'             'Desc.Departamento'        '24' ' ',
   14  'ZGLT041'          'BNAME2'            'TG_SAIDA' 'BNAME2'                    'Responsável'              '14' 'X',
   14  'USREFUS'          'USERALIAS'         'TG_SAIDA' 'USERALIAS'                 'Desc.Responsável'         '24' ' ',
   15  'ZGLT041'          'CRIT_VECTO'        'TG_SAIDA' 'CRIT_VECTO'                'Criterio Vcto'            '14' 'X',
   16  'ZGLT041'          'PRAZO_ENTR'        'TG_SAIDA' 'PRAZO_ENTR'                'Prazo Entrega'            '14' 'X',
   17  ' '                ' '                 'TG_SAIDA' 'CRITERIO'                  'Critérios Reconciliação'  '10' ' '.


ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0321   text
*      -->P_0322   text
*      -->P_0323   text
*      -->P_0324   text
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_EDIT).

  CLEAR WA_ESTRUTURA.

  WA_ESTRUTURA-EDIT          = P_EDIT.
  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.

  IF P_OUTPUTLEN IS NOT INITIAL.
    WA_ESTRUTURA-OUTPUTLEN = P_OUTPUTLEN.
  ENDIF.

*  IF P_FIELD EQ 'QTE_LIBERAR'.
*    WA_ESTRUTURA-HOTSPOT = 'X'.
*  ENDIF.
*
*  IF P_FIELD EQ 'CHECKBOX'.
*    WA_ESTRUTURA-CHECKBOX = 'X'.
*  ENDIF.


  IF P_SCRTEXT_L IS NOT INITIAL.
    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  ENDIF.

  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP
      I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0181   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INICIAR_VARIAVEIS.
  V_REPORT = SY-REPID.
  PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-002.

ENDFORM.                    " INICIAR_VARIAVES
*---------------------------------------------------------------------*
*       FORM XPF_STATUS_SET                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XPF_STATUS_SET USING UCOMM TYPE KKBLO_T_EXTAB.         "#EC CALLED
  DATA: TL_FCODE TYPE TABLE OF SY-UCOMM,
        WL_FCODE TYPE SY-UCOMM.

  DATA: GR_EVENTS       TYPE REF TO LCL_EVENT_RECEIVER,
        LS_SEL_HIDE     TYPE SLIS_SEL_HIDE_ALV,
*        REF1              TYPE REF TO CL_GUI_ALV_GRID,
        IT_FIELDCATALOG TYPE LVC_T_FCAT,
        WA_FIELDCATALOG TYPE LVC_S_FCAT,
        IS_TABLE        TYPE LVC_S_STBL.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      ES_SEL_HIDE = LS_SEL_HIDE
      E_GRID      = REF1.

  CALL METHOD REF1->GET_FRONTEND_FIELDCATALOG
    IMPORTING
      ET_FIELDCATALOG = IT_FIELDCATALOG.

  LOOP AT IT_FIELDCATALOG INTO WA_FIELDCATALOG
    WHERE FIELDNAME EQ 'CRITERIO'
       OR FIELDNAME EQ 'COD_CLAS_BAL'
       OR FIELDNAME EQ 'COD_CLAS_NOT2'
       OR FIELDNAME EQ 'DEP_RESP2'
       OR FIELDNAME EQ 'BNAME2'
       OR FIELDNAME EQ 'CTA_INTERCOMPANY'
       OR FIELDNAME EQ 'CTA_MONET'.

    IF WA_FIELDCATALOG-FIELDNAME EQ 'CRITERIO'.
      WA_FIELDCATALOG-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.

    ELSEIF WA_FIELDCATALOG-FIELDNAME EQ 'COD_CLAS_BAL'.
      WA_FIELDCATALOG-DRDN_HNDL  = 1.
      WA_FIELDCATALOG-DRDN_ALIAS = 'X'.

    ELSEIF  WA_FIELDCATALOG-FIELDNAME EQ 'COD_CLAS_NOT2'.
      WA_FIELDCATALOG-DRDN_HNDL  = 2.
      WA_FIELDCATALOG-DRDN_ALIAS = 'X'.

    ELSEIF WA_FIELDCATALOG-FIELDNAME EQ 'CTA_INTERCOMPANY'
        OR WA_FIELDCATALOG-FIELDNAME EQ 'CTA_MONET'.
      WA_FIELDCATALOG-DRDN_HNDL  = 3.
      WA_FIELDCATALOG-DRDN_ALIAS = 'X'.

    ELSEIF WA_FIELDCATALOG-FIELDNAME EQ 'DEP_RESP2'
        OR WA_FIELDCATALOG-FIELDNAME EQ 'BNAME2'.
      WA_FIELDCATALOG-F4AVAILABL    = 'X'.

    ENDIF.

    MODIFY IT_FIELDCATALOG FROM WA_FIELDCATALOG.
  ENDLOOP.

  PERFORM BUILD_DROPDOWN USING REF1.

  CALL METHOD REF1->SET_FRONTEND_FIELDCATALOG
    EXPORTING
      IT_FIELDCATALOG = IT_FIELDCATALOG.

  IS_TABLE-ROW = 'X'.
  IS_TABLE-COL = 'X'.

  CALL METHOD REF1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = IS_TABLE
      I_SOFT_REFRESH = 'X'.

  IF INIT IS INITIAL.
    CALL METHOD REF1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD REF1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    CREATE OBJECT GR_EVENTS.
    SET HANDLER: GR_EVENTS->HANDLE_ON_BUTTON_CLICK FOR REF1,
                 GR_EVENTS->ON_DATA_CHANGED FOR REF1.
    INIT = 'X'.
  ENDIF.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING TL_FCODE.
ENDFORM. "XPF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM XUSER_COMMAND                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XUSER_COMMAND USING UCOMM    LIKE SY-UCOMM
                         SELFIELD TYPE KKBLO_SELFIELD..     "#EC CALLED

  DATA: TL_INPUT_041 TYPE TABLE OF ZGLT041 WITH HEADER LINE,
        TL_SAIDA     LIKE TABLE OF TG_SAIDA WITH HEADER LINE,
        WA_SAI       LIKE LINE OF TG_SAIDA,
        VG_TABIX     LIKE SY-TABIX,
        WL_HEADER    TYPE THEAD.

  DATA: GR_EVENTS   TYPE REF TO LCL_EVENT_RECEIVER,
        LS_SEL_HIDE TYPE SLIS_SEL_HIDE_ALV.
*        VL_ERRO,
*        WL_INDEX(3) TYPE N,
*        TL_TLINES LIKE TLINE OCCURS 0 WITH HEADER LINE,
*        WG_INDEX,
*        XSDO TYPE ZSDT0082-QTE_SOL,
*        XQTSOL TYPE ZSDT0082-QTE_SOL,
*        TQTSOL(20)  TYPE C,
*        XQLIB TYPE ZSDT0082-QTE_LIB,
*        TQLIB(20) TYPE C,
*        NROSOL TYPE ZSDT0082-NRO_SOL.

  CLEAR: WG_T001, VG_CONT. "/ Modificação CS2017000372 --> WG_GJAHR.
  REFRESH: TL_SAIDA.
*           ESTRUTURA,
*           T_AUX,
*           IT_SAIDA2.
*
*  WG_UCOMM = UCOMM.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      ES_SEL_HIDE = LS_SEL_HIDE
      E_GRID      = REF1.


  CALL METHOD REF1->CHECK_CHANGED_DATA.
  CASE UCOMM.
    WHEN 'APAGAR'.
      LOOP AT TG_SAIDA INTO WA_SAIDA WHERE MARK = ABAP_TRUE.
        MOVE-CORRESPONDING WA_SAIDA TO TG_SAIDA_DEL.
        APPEND TG_SAIDA_DEL.
      ENDLOOP.
      DELETE TG_SAIDA WHERE MARK = ABAP_TRUE.
    WHEN 'DUPLI'.
      CLEAR: WA_SAI.
      READ TABLE TG_SAIDA INTO WA_SAIDA WITH KEY MARK = ABAP_TRUE.
      IF SY-SUBRC IS INITIAL.
        VG_TABIX = SY-TABIX.
        WA_SAI-STATUS = ICON_INCOMPLETE.
        WA_SAI-BUKRS  = WA_SAIDA-BUKRS.
*        WA_SAI-GJAHR  = WA_SAIDA-GJAHR. "/ Modificação CS2017000372
        WA_SAI-SAKNR  = WA_SAIDA-SAKNR.
        WA_SAI-TXT50  = WA_SAIDA-TXT50.
        WA_SAI-KTOKS  = WA_SAIDA-KTOKS.
        WA_SAI-MITKZ  = WA_SAIDA-MITKZ.
        WA_SAI-XSPEB  = WA_SAIDA-XSPEB.
        ADD 1 TO VG_TABIX.
        INSERT WA_SAI INTO TG_SAIDA INDEX VG_TABIX.
      ENDIF.
    WHEN '&SAVE'.
      REFRESH: TL_INPUT_041.

      LOOP AT TG_SAIDA_DEL
        WHERE COD_CLAS_BAL         IS NOT INITIAL
           OR COD_CLAS_NOT2        IS NOT INITIAL
           OR CTA_MONET            IS NOT INITIAL
           OR CTA_INTERCOMPANY     IS NOT INITIAL
           OR DEP_RESP2            IS NOT INITIAL
           OR CRIT_VECTO           IS NOT INITIAL
           OR PRAZO_ENTR           IS NOT INITIAL.

        IF TG_SAIDA_DEL-COD_CLAS_BAL IS NOT INITIAL.
          READ TABLE TG_ZGLT039
            WITH KEY CHAVE1 = TG_SAIDA_DEL-COD_CLAS_BAL.
          TL_INPUT_041-COD_CLAS_BAL = TG_ZGLT039-CODIGO.
        ELSE.
          CLEAR:  TL_INPUT_041-COD_CLAS_BAL.
        ENDIF.

        IF TG_SAIDA_DEL-COD_CLAS_NOT2 IS NOT INITIAL.
          READ TABLE TG_ZGLT039
            WITH KEY CHAVE2 = TG_SAIDA_DEL-COD_CLAS_NOT2.
          TL_INPUT_041-COD_CLAS_NOT2 = TG_ZGLT039-COD_NOTA.
        ELSE.
          CLEAR:  TL_INPUT_041-COD_CLAS_NOT2.
        ENDIF.


        MOVE: TG_SAIDA_DEL-BUKRS                TO TL_INPUT_041-BUKRS,
*              TG_SAIDA_DEL-GJAHR                TO TL_INPUT_041-GJAHR, "/ Modificação CS2017000372
              TG_SAIDA_DEL-SAKNR                TO TL_INPUT_041-SAKNR,
              TG_SAIDA_DEL-CTA_MONET(1)         TO TL_INPUT_041-CTA_MONET,
              TG_SAIDA_DEL-CTA_INTERCOMPANY(1)  TO TL_INPUT_041-CTA_INTERCOMPANY,
              TG_SAIDA_DEL-DEP_RESP2            TO TL_INPUT_041-DEP_RESP2,
              TG_SAIDA_DEL-BNAME2               TO TL_INPUT_041-BNAME2,
              TG_SAIDA_DEL-PRAZO_ENTR           TO TL_INPUT_041-PRAZO_ENTR,
              TG_SAIDA_DEL-CRIT_VECTO           TO TL_INPUT_041-CRIT_VECTO.

        APPEND TL_INPUT_041.
        CLEAR: TL_INPUT_041.

      ENDLOOP.

      LOOP AT TL_INPUT_041.
        DELETE FROM ZGLT041 WHERE BUKRS         EQ TL_INPUT_041-BUKRS
                              AND SAKNR         EQ TL_INPUT_041-SAKNR
*                              AND GJAHR         EQ TL_INPUT_041-GJAHR "/ Modificação CS2017000372
                              AND COD_CLAS_BAL  EQ TL_INPUT_041-COD_CLAS_BAL
                              AND COD_CLAS_NOT2 EQ TL_INPUT_041-COD_CLAS_NOT2.
      ENDLOOP.

      CLEAR: TL_INPUT_041[].

      LOOP AT TG_SAIDA
        WHERE COD_CLAS_BAL         IS NOT INITIAL
           OR COD_CLAS_NOT2        IS NOT INITIAL
           OR CTA_MONET            IS NOT INITIAL
           OR CTA_INTERCOMPANY     IS NOT INITIAL
           OR DEP_RESP2            IS NOT INITIAL
           OR CRIT_VECTO           IS NOT INITIAL
           OR PRAZO_ENTR           IS NOT INITIAL.

        IF TG_SAIDA-COD_CLAS_BAL IS NOT INITIAL.
          READ TABLE TG_ZGLT039
            WITH KEY CHAVE1 = TG_SAIDA-COD_CLAS_BAL.
          TL_INPUT_041-COD_CLAS_BAL = TG_ZGLT039-CODIGO.
        ELSE.
          CLEAR:  TL_INPUT_041-COD_CLAS_BAL.
        ENDIF.

        IF TG_SAIDA-COD_CLAS_NOT2 IS NOT INITIAL.
          READ TABLE TG_ZGLT039
            WITH KEY CHAVE2 = TG_SAIDA-COD_CLAS_NOT2.
          TL_INPUT_041-COD_CLAS_NOT2 = TG_ZGLT039-COD_NOTA.
        ELSE.
          CLEAR:  TL_INPUT_041-COD_CLAS_NOT2.
        ENDIF.


        MOVE: TG_SAIDA-BUKRS                TO TL_INPUT_041-BUKRS,
*              TG_SAIDA-GJAHR                TO TL_INPUT_041-GJAHR, "/ Modificação CS2017000372
              TG_SAIDA-SAKNR                TO TL_INPUT_041-SAKNR,
              TG_SAIDA-CTA_MONET(1)         TO TL_INPUT_041-CTA_MONET,
              TG_SAIDA-CTA_INTERCOMPANY(1)  TO TL_INPUT_041-CTA_INTERCOMPANY,
              TG_SAIDA-DEP_RESP2            TO TL_INPUT_041-DEP_RESP2,
              TG_SAIDA-BNAME2               TO TL_INPUT_041-BNAME2,
              TG_SAIDA-PRAZO_ENTR           TO TL_INPUT_041-PRAZO_ENTR,
              TG_SAIDA-CRIT_VECTO           TO TL_INPUT_041-CRIT_VECTO.

        APPEND TL_INPUT_041.
        CLEAR: TL_INPUT_041.

        CONCATENATE TG_SAIDA-BUKRS TG_SAIDA-SAKNR TG_SAIDA-COD_CLAS_BAL TG_SAIDA-COD_CLAS_NOT2 INTO WL_HEADER-TDNAME. "/ Modificação CS2017000372 --> TG_SAIDA-GJAHR.
        WL_HEADER-TDOBJECT = 'ZCRITERIO'.
        WL_HEADER-TDID     = 'ZCRI'.
        WL_HEADER-TDSPRAS  = SY-LANGU.
        PERFORM SAVE_TEXT USING WL_HEADER.

      ENDLOOP.
      IF TL_INPUT_041[] IS NOT INITIAL.
        MODIFY ZGLT041 FROM TABLE TL_INPUT_041.
        "Modificação 03.11.2016 - Início -----------------------------------------
        DATA: IT_ZGLT041 TYPE STANDARD TABLE OF ZGLT041,
              WA_ZGLT041 TYPE ZGLT041.

        SELECT *
          FROM ZGLT041
          INTO TABLE IT_ZGLT041
          WHERE BUKRS IN S_BUKRS
            AND SAKNR IN S_SKB1.
*            AND GJAHR IN S_GJAHR. "/ Modificação CS2017000372

        LOOP AT IT_ZGLT041 INTO WA_ZGLT041.
          READ TABLE TL_INPUT_041 WITH KEY BUKRS          = WA_ZGLT041-BUKRS
*                                           GJAHR          = WA_ZGLT041-GJAHR "/ Modificação CS2017000372
                                           SAKNR          = WA_ZGLT041-SAKNR
                                           COD_CLAS_BAL   = WA_ZGLT041-COD_CLAS_BAL
                                           COD_CLAS_NOT2  = WA_ZGLT041-COD_CLAS_NOT2
                                           TRANSPORTING NO FIELDS.
          IF SY-SUBRC NE 0.
            DELETE ZGLT041 FROM WA_ZGLT041.
          ENDIF.
          CLEAR WA_ZGLT041.
        ENDLOOP.
        "Modificação 03.11.2016 - Fim --------------------------------------------
        MESSAGE S836(SD) WITH 'Os dados foram salvos com sucesso'.
      ENDIF.
    WHEN 'COPY'.
      TL_SAIDA[] = TG_SAIDA[].
      DELETE TL_SAIDA WHERE MARK IS INITIAL.
      VG_CONT = LINES( TL_SAIDA ).
      IF VG_CONT IS NOT INITIAL.
        "LOOP AT TL_SAIDA.
        "IF WG_T001 IS INITIAL.
        CALL SCREEN 100 STARTING AT 3 3 ENDING AT  57 10.
        "ENDIF.
        "ENDLOOP.
      ENDIF.
    WHEN 'COPY_A'.
      TL_SAIDA[] = TG_SAIDA[].
      DELETE TL_SAIDA WHERE MARK IS INITIAL.
      VG_CONT_A = LINES( TL_SAIDA ).
      IF VG_CONT_A IS NOT INITIAL.
        CALL SCREEN 200 STARTING AT 3 3 ENDING AT  57 10.
      ENDIF.
  ENDCASE.
ENDFORM. "XUSER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  SAVE_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2350   text
*      -->P_WL_HEADER  text
*----------------------------------------------------------------------*
FORM SAVE_TEXT  USING    WL_HEADER TYPE THEAD.

  DATA: TL_TLINES LIKE TLINE OCCURS 0 WITH HEADER LINE.

  REFRESH: TL_TLINES.

  LOOP AT TG_AUX INTO WA_AUX
    WHERE BUKRS EQ TG_SAIDA-BUKRS
*      AND GJAHR EQ TG_SAIDA-GJAHR "/ Modificação CS2017000372
      AND SAKNR EQ TG_SAIDA-SAKNR
      AND TXT50 EQ TG_SAIDA-TXT50
      AND KTOKS EQ TG_SAIDA-KTOKS
      AND MITKZ EQ TG_SAIDA-MITKZ
      AND XSPEB EQ TG_SAIDA-XSPEB
      AND COD_CLAS_BAL  EQ TG_SAIDA-COD_CLAS_BAL
      AND COD_CLAS_NOT2 EQ TG_SAIDA-COD_CLAS_NOT2.

    MOVE: '*'           TO TL_TLINES-TDFORMAT,
          WA_AUX-TDLINE TO TL_TLINES-TDLINE.

    APPEND TL_TLINES.
    CLEAR TL_TLINES.

  ENDLOOP.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      HEADER          = WL_HEADER
      SAVEMODE_DIRECT = 'X'
    TABLES
      LINES           = TL_TLINES
    EXCEPTIONS
      ID              = 1
      LANGUAGE        = 2
      NAME            = 3
      OBJECT          = 4
      OTHERS          = 5.

ENDFORM.                    " SAVE_TEXT
*&---------------------------------------------------------------------*
*&      Form  BUILD_DROPDOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_DROPDOWN USING WL_REF TYPE REF TO CL_GUI_ALV_GRID.

  DATA:   LS_DROPDOWN  TYPE LVC_S_DRAL,
          LT_DROPDOWN  TYPE LVC_T_DRAL,
          LT_DROPDOWN2 TYPE LVC_T_DRAL,
          LT_DROPDOWN3 TYPE LVC_T_DRAL,
          E_ROW	       TYPE I,
          VG_CODIGO    LIKE ZGLT039-CODIGO.

  REFRESH: LT_DROPDOWN, LT_DROPDOWN2, LT_DROPDOWN3.

  CALL METHOD REF1->GET_CURRENT_CELL
    IMPORTING
      E_ROW = E_ROW.

  CLEAR: VG_CODIGO.

  READ TABLE TG_SAIDA INTO WA_SAIDA INDEX E_ROW.
  IF ( SY-SUBRC IS INITIAL ) AND ( WA_SAIDA-COD_CLAS_BAL IS NOT INITIAL ).
    READ TABLE TG_ZGLT039 WITH KEY CHAVE1 = WA_SAIDA-COD_CLAS_BAL.
    IF SY-SUBRC IS INITIAL.
      VG_CODIGO = TG_ZGLT039-CODIGO.
    ENDIF.
  ENDIF.

  LOOP AT TG_ZGLT039.
    IF TG_ZGLT039-CODIGO IS NOT INITIAL.
      LS_DROPDOWN-HANDLE = '1'.

      LS_DROPDOWN-INT_VALUE = TG_ZGLT039-CHAVE1.
      LS_DROPDOWN-VALUE = TG_ZGLT039-CHAVE1.
      APPEND: LS_DROPDOWN TO LT_DROPDOWN.
    ENDIF.

    IF ( TG_ZGLT039-COD_NOTA IS NOT INITIAL ) AND ( VG_CODIGO EQ TG_ZGLT039-CODIGO OR VG_CODIGO IS INITIAL ).
      LS_DROPDOWN-HANDLE = '2'.
      LS_DROPDOWN-INT_VALUE = TG_ZGLT039-CHAVE2.
      LS_DROPDOWN-VALUE = TG_ZGLT039-CHAVE2.
      APPEND: LS_DROPDOWN TO LT_DROPDOWN2.
    ENDIF.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM LT_DROPDOWN.
  DELETE ADJACENT DUPLICATES FROM LT_DROPDOWN2.


  IF LT_DROPDOWN[] IS NOT INITIAL.
* Übergabe der Dropdown-Tabelle an ALV-Grid-Control
    CALL METHOD WL_REF->SET_DROP_DOWN_TABLE
      EXPORTING
        IT_DROP_DOWN_ALIAS = LT_DROPDOWN.
  ENDIF.

  IF LT_DROPDOWN2[] IS NOT INITIAL.
* Übergabe der Dropdown-Tabelle an ALV-Grid-Control
    CALL METHOD WL_REF->SET_DROP_DOWN_TABLE
      EXPORTING
        IT_DROP_DOWN_ALIAS = LT_DROPDOWN2.
  ENDIF.

  LS_DROPDOWN-HANDLE = '3'.
  LS_DROPDOWN-INT_VALUE = 'S - SIM'.
  LS_DROPDOWN-VALUE = 'S - SIM'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN3.

  LS_DROPDOWN-INT_VALUE = 'N - NÃO'.
  LS_DROPDOWN-VALUE = 'N - NÃO'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN3.


  IF LT_DROPDOWN3[] IS NOT INITIAL.
* Übergabe der Dropdown-Tabelle an ALV-Grid-Control
    CALL METHOD WL_REF->SET_DROP_DOWN_TABLE
      EXPORTING
        IT_DROP_DOWN_ALIAS = LT_DROPDOWN3.
  ENDIF.
ENDFORM.                    " BUILD_DROPDOWN
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'Z002'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'ENTER'.
      CLEAR: WG_T001-BUTXT.

      SELECT SINGLE BUTXT
        FROM T001
        INTO WG_T001-BUTXT
        WHERE BUKRS EQ WG_T001-BUKRS.
    WHEN 'OK'.
      IF WG_T001-BUKRS IS NOT INITIAL." OR WG_GJAHR IS NOT INITIAL.
        PERFORM VERIFICA_BUKRS.
        PERFORM COPIA_LINHAS.

        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VERIFICA_BUKRS .
  DATA: TL_SKB1 TYPE TABLE OF SKB1 WITH HEADER LINE,
        WL_T001 TYPE T001.

  SELECT SINGLE *
    FROM T001
    INTO WL_T001
     WHERE  BUKRS EQ WG_T001-BUKRS.

  IF SY-SUBRC IS INITIAL.

    SELECT  * "#EC CI_DB_OPERATION_OK[2431747]
      FROM SKB1
      INTO TABLE TL_SKB1
       FOR ALL ENTRIES IN TG_SAIDA
       WHERE BUKRS EQ WG_T001-BUKRS
         AND SAKNR EQ TG_SAIDA-SAKNR.

    LOOP AT TG_SAIDA
      WHERE MARK IS NOT INITIAL.

      READ TABLE TL_SKB1
        WITH KEY "BUKRS = TG_SAIDA-BUKRS
                 SAKNR = TG_SAIDA-SAKNR.

      IF SY-SUBRC IS NOT INITIAL.
        SUBTRACT 1 FROM VG_CONT.
        TG_SAIDA-MARK = SPACE.
        MODIFY TG_SAIDA.
      ENDIF.
    ENDLOOP.

  ELSE.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Empresa não existe!'.
  ENDIF.

ENDFORM.                    " VERIFICA_BUKRS
*&---------------------------------------------------------------------*
*&      Form  COPIA_LINHAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COPIA_LINHAS .
  DATA: TL_INPUT_041      TYPE TABLE OF ZGLT041 WITH HEADER LINE,
        TL_INPUT_041_FAIL TYPE TABLE OF ZGLT041 WITH HEADER LINE,
        WL_HEADER         TYPE THEAD,
        TL_TLINES         LIKE TLINE OCCURS 0 WITH HEADER LINE.

  DATA: IT_ZGLT041_COPIA  TYPE STANDARD TABLE OF ZGLT041,
        IT_ZGLT043A_COPIA TYPE STANDARD TABLE OF ZGLT043A,
        TG_SAIDA_COPIA    TYPE TABLE OF TY_SAIDA WITH HEADER LINE,
        VG_CONT_B         TYPE SY-TABIX.

  TG_SAIDA_COPIA[] = TG_SAIDA[].

  DELETE TG_SAIDA_COPIA
    WHERE MARK IS INITIAL
       OR STATUS NE ICON_CHECKED.

  SELECT *
    FROM ZGLT041
    INTO TABLE IT_ZGLT041_COPIA
*    FOR ALL ENTRIES IN TG_SAIDA_COPIA
      WHERE BUKRS EQ WG_T001-BUKRS.
*        AND GJAHR EQ TG_SAIDA_COPIA-GJAHR. "/ Modificação CS2017000372

  SELECT *
    FROM ZGLT043A
    INTO TABLE IT_ZGLT043A_COPIA
    FOR ALL ENTRIES IN IT_ZGLT041_COPIA
      WHERE BUKRS EQ IT_ZGLT041_COPIA-BUKRS
        AND KTOPL EQ '0050'.

  REFRESH: TL_TLINES.
  REFRESH: TL_INPUT_041.

  LOOP AT TG_SAIDA
     WHERE MARK   IS NOT INITIAL
       AND STATUS EQ ICON_CHECKED.

    LOOP AT TG_AUX INTO WA_AUX
     WHERE BUKRS EQ TG_SAIDA-BUKRS
*       AND GJAHR EQ TG_SAIDA-GJAHR "/ Modificação CS2017000372
       AND SAKNR EQ TG_SAIDA-SAKNR
       AND TXT50 EQ TG_SAIDA-TXT50
       AND KTOKS EQ TG_SAIDA-KTOKS
       AND MITKZ EQ TG_SAIDA-MITKZ
       AND XSPEB EQ TG_SAIDA-XSPEB.

      MOVE: '*'           TO TL_TLINES-TDFORMAT,
            WA_AUX-TDLINE TO TL_TLINES-TDLINE.

      APPEND TL_TLINES.
      CLEAR TL_TLINES.

    ENDLOOP.

    TG_SAIDA-BUKRS = WG_T001-BUKRS.

    IF TG_SAIDA-COD_CLAS_BAL IS NOT INITIAL.
      READ TABLE TG_ZGLT039 WITH KEY CHAVE1 = TG_SAIDA-COD_CLAS_BAL.
      TL_INPUT_041-COD_CLAS_BAL = TG_ZGLT039-CODIGO.
    ELSE.
      CLEAR: TL_INPUT_041-COD_CLAS_BAL.
    ENDIF.

    IF TG_SAIDA-COD_CLAS_NOT2 IS NOT INITIAL.
      READ TABLE TG_ZGLT039 WITH KEY CHAVE2 = TG_SAIDA-COD_CLAS_NOT2.
      TL_INPUT_041-COD_CLAS_NOT2 = TG_ZGLT039-COD_NOTA.
    ELSE.
      CLEAR: TL_INPUT_041-COD_CLAS_NOT2.
    ENDIF.

    MOVE: TG_SAIDA-BUKRS                TO TL_INPUT_041-BUKRS,
*          TG_SAIDA-GJAHR                TO TL_INPUT_041-GJAHR, "/ Modificação CS2017000372
          TG_SAIDA-SAKNR                TO TL_INPUT_041-SAKNR,
*              tg_saida-cod_clas_bal         TO tl_input_041-cod_clas_bal,
          TG_SAIDA-CTA_MONET(1)         TO TL_INPUT_041-CTA_MONET,
          TG_SAIDA-CTA_INTERCOMPANY(1)  TO TL_INPUT_041-CTA_INTERCOMPANY,
          TG_SAIDA-DEP_RESP2            TO TL_INPUT_041-DEP_RESP2,
          TG_SAIDA-BNAME2               TO TL_INPUT_041-BNAME2,
          TG_SAIDA-PRAZO_ENTR           TO TL_INPUT_041-PRAZO_ENTR,
          TG_SAIDA-CRIT_VECTO           TO TL_INPUT_041-CRIT_VECTO.
*              tg_saida-cod_clas_not2        TO tl_input_041-cod_clas_not2.

    READ TABLE IT_ZGLT041_COPIA WITH KEY BUKRS = TL_INPUT_041-BUKRS
*                                         GJAHR = TL_INPUT_041-GJAHR "/ Modificação CS2017000372
                                         SAKNR = TL_INPUT_041-SAKNR TRANSPORTING NO FIELDS.
    IF SY-SUBRC IS INITIAL.
      READ TABLE IT_ZGLT043A_COPIA WITH KEY BUKRS = TL_INPUT_041-BUKRS
                                            KTOPL = '0050'
                                            KTOKS = TG_SAIDA-KTOKS TRANSPORTING NO FIELDS.
      IF SY-SUBRC IS NOT INITIAL.
        APPEND TL_INPUT_041 TO TL_INPUT_041_FAIL.
        CLEAR: TL_INPUT_041.
        ADD 1 TO VG_CONT_B.
        CONTINUE.
      ENDIF.
    ENDIF.

    APPEND TL_INPUT_041.
    CLEAR: TL_INPUT_041.

    CONCATENATE TG_SAIDA-BUKRS TG_SAIDA-SAKNR TG_SAIDA-COD_CLAS_BAL TG_SAIDA-COD_CLAS_NOT2 INTO WL_HEADER-TDNAME. "/ Modificação CS2017000372 --> TG_SAIDA-GJAHR
    WL_HEADER-TDOBJECT = 'ZCRITERIO'.
    WL_HEADER-TDID     = 'ZCRI'.
    WL_HEADER-TDSPRAS  = SY-LANGU.
    PERFORM SAVE_TEXT USING WL_HEADER.

    IF TL_TLINES[] IS NOT INITIAL.
      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
          HEADER          = WL_HEADER
          SAVEMODE_DIRECT = 'X'
        TABLES
          LINES           = TL_TLINES
        EXCEPTIONS
          ID              = 1
          LANGUAGE        = 2
          NAME            = 3
          OBJECT          = 4
          OTHERS          = 5.
    ENDIF.
  ENDLOOP.

  IF TL_INPUT_041[] IS NOT INITIAL.
    MODIFY ZGLT041 FROM TABLE TL_INPUT_041.
  ENDIF.

  DESCRIBE TABLE TL_INPUT_041[] LINES VG_CONT.
  MESSAGE S836(SD) WITH 'Total Copiados'
                          VG_CONT
                          'Total Não Copiados'
                          VG_CONT_B.

  IF VG_CONT_B IS NOT INITIAL.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_GRID_TITLE          = 'Não Copiados'
        I_STRUCTURE_NAME      = 'ZGLT041'
        I_SCREEN_START_COLUMN = 5
        I_SCREEN_START_LINE   = 5
        I_SCREEN_END_COLUMN   = 80
        I_SCREEN_END_LINE     = 20
      TABLES
        T_OUTTAB              = TL_INPUT_041_FAIL.

  ENDIF.

ENDFORM.                    " COPIA_LINHAS
*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_AUTORIZACAO
*&---------------------------------------------------------------------*
*       Formulário para autorização de usuário no campo BUKRS para
*       Reconciliações.
*----------------------------------------------------------------------*
*  -->  p1        text
*----------------------------------------------------------------------*
FORM VERIFICAR_AUTORIZACAO .

  DATA: IT_T001      TYPE TABLE OF T001 WITH HEADER LINE,
        BUKRS_RECONC TYPE CHAR250.

  RANGES: S_BUKRS_2 FOR T001-BUKRS.

* Lista todas as empresas selecionadas pelo usuário
  SELECT DISTINCT BUKRS
    FROM T001
    INTO CORRESPONDING FIELDS OF TABLE IT_T001
    WHERE BUKRS IN S_BUKRS.

  SORT IT_T001 BY BUKRS.

* Busca e trata parâmetro de Z_BUKRS_RECONC
  GET PARAMETER ID 'Z_BUKRS_RECONC' FIELD BUKRS_RECONC.
  IF BUKRS_RECONC IS NOT INITIAL.
    CONDENSE BUKRS_RECONC.
    IF BUKRS_RECONC CS '*'..
      S_BUKRS_2-SIGN    = 'I'.
      S_BUKRS_2-OPTION  = 'CP'.
      S_BUKRS_2-LOW     = '*'.
      APPEND S_BUKRS_2.
    ELSE.
      WHILE BUKRS_RECONC IS NOT INITIAL.
        IF BUKRS_RECONC+4(1) EQ ','.
          S_BUKRS_2-SIGN    = 'I'.
          S_BUKRS_2-OPTION  = 'EQ'.
          S_BUKRS_2-LOW     = BUKRS_RECONC(4).
          APPEND S_BUKRS_2.
          SHIFT BUKRS_RECONC BY 5 PLACES LEFT.
        ELSEIF BUKRS_RECONC+4(1) EQ '-'.
          S_BUKRS_2-SIGN    = 'I'.
          S_BUKRS_2-OPTION  = 'BT'.
          S_BUKRS_2-LOW     = BUKRS_RECONC(4).
          S_BUKRS_2-HIGH    = BUKRS_RECONC+5(4).
          APPEND S_BUKRS_2.
          SHIFT BUKRS_RECONC BY 9 PLACES LEFT.
          IF BUKRS_RECONC(1) IS NOT INITIAL.
            SHIFT BUKRS_RECONC BY 1 PLACES LEFT.
          ENDIF.
        ELSEIF BUKRS_RECONC+4(1) IS INITIAL.
          S_BUKRS_2-SIGN    = 'I'.
          S_BUKRS_2-OPTION  = 'EQ'.
          S_BUKRS_2-LOW     = BUKRS_RECONC(4).
          APPEND S_BUKRS_2.
          SHIFT BUKRS_RECONC LEFT DELETING LEADING BUKRS_RECONC(4).
        ELSE.
          MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
          STOP.
        ENDIF.
        CLEAR S_BUKRS_2.
      ENDWHILE.
    ENDIF.
    "Verificação da lista de Empresas solicitadas
    LOOP AT IT_T001 WHERE BUKRS NOT IN S_BUKRS_2.
      MESSAGE S091(8B) WITH IT_T001-BUKRS DISPLAY LIKE 'E'.
      STOP.
    ENDLOOP.
  ELSE.
    MESSAGE S091(8B) WITH IT_T001-BUKRS DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  SELECIONA_EXERCICIO
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM SELECIONA_EXERCICIO .
*
*  DATA: WA_GJAHR TYPE TY_GJAHR.
*
*  LOOP AT S_GJAHR.
*    IF S_GJAHR-OPTION EQ 'EQ'.
*      WA_GJAHR-GJAHR = S_GJAHR-LOW.
*      APPEND WA_GJAHR TO IT_GJAHR.
*      CLEAR: WA_GJAHR.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Module  STATUS_0200  OUTPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE STATUS_0200 OUTPUT.
*  SET PF-STATUS 'Z002'.
*ENDMODULE.
**&---------------------------------------------------------------------*
**&      Module  USER_COMMAND_0200  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE USER_COMMAND_0200 INPUT.
*  CASE SY-UCOMM.
*    WHEN 'ENTER'.
*      "DO NOTHING
*    WHEN 'OK'.
*      IF WG_GJAHR IS NOT INITIAL.
*        PERFORM COPIA_LINHAS_ANO.
*        LEAVE TO SCREEN 0.
*      ENDIF.
*    WHEN 'CANCEL'.
*      LEAVE TO SCREEN 0.
*  ENDCASE.
*ENDMODULE.
**&---------------------------------------------------------------------*
**&      Form  COPIA_LINHAS_ANO
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM COPIA_LINHAS_ANO .
*
*  DATA: TL_INPUT_041      TYPE TABLE OF ZGLT041 WITH HEADER LINE,
*        TL_INPUT_041_FAIL TYPE TABLE OF ZGLT041 WITH HEADER LINE,
*        WL_HEADER         TYPE THEAD,
*        TL_TLINES         LIKE TLINE OCCURS 0 WITH HEADER LINE.
*
*  DATA: IT_ZGLT041_COPIA  TYPE STANDARD TABLE OF ZGLT041,
*        IT_ZGLT043A_COPIA TYPE STANDARD TABLE OF ZGLT043A,
*        TG_SAIDA_COPIA    TYPE TABLE OF TY_SAIDA WITH HEADER LINE,
*        VG_CONT_B         TYPE SY-TABIX.
*
*  TG_SAIDA_COPIA[] = TG_SAIDA[].
*
*  DELETE TG_SAIDA_COPIA
*    WHERE MARK IS INITIAL
*       OR STATUS NE ICON_CHECKED.
*
*  SELECT *
*    FROM ZGLT041
*    INTO TABLE IT_ZGLT041_COPIA
*    FOR ALL ENTRIES IN TG_SAIDA_COPIA
*      WHERE BUKRS EQ TG_SAIDA_COPIA-BUKRS
*        AND GJAHR EQ WG_GJAHR.
*
*  SELECT *
*    FROM ZGLT043A
*    INTO TABLE IT_ZGLT043A_COPIA
*    FOR ALL ENTRIES IN IT_ZGLT041_COPIA
*      WHERE BUKRS EQ IT_ZGLT041_COPIA-BUKRS
*        AND KTOPL EQ '0050'.
*
*  REFRESH: TL_TLINES.
*  REFRESH: TL_INPUT_041.
*
*  LOOP AT TG_SAIDA
*    WHERE MARK   IS NOT INITIAL
*      AND STATUS EQ ICON_CHECKED.
*
*    LOOP AT TG_AUX INTO WA_AUX
*     WHERE BUKRS EQ TG_SAIDA-BUKRS
*       AND GJAHR EQ TG_SAIDA-GJAHR
*       AND SAKNR EQ TG_SAIDA-SAKNR
*       AND TXT50 EQ TG_SAIDA-TXT50
*       AND KTOKS EQ TG_SAIDA-KTOKS
*       AND MITKZ EQ TG_SAIDA-MITKZ
*       AND XSPEB EQ TG_SAIDA-XSPEB.
*
*      MOVE: '*'           TO TL_TLINES-TDFORMAT,
*            WA_AUX-TDLINE TO TL_TLINES-TDLINE.
*
*      APPEND TL_TLINES.
*      CLEAR TL_TLINES.
*    ENDLOOP.
*
*    TG_SAIDA-GJAHR = WG_GJAHR.
*
*    IF TG_SAIDA-COD_CLAS_BAL IS NOT INITIAL.
*      READ TABLE TG_ZGLT039 WITH KEY CHAVE1 = TG_SAIDA-COD_CLAS_BAL.
*      TL_INPUT_041-COD_CLAS_BAL = TG_ZGLT039-CODIGO.
*    ELSE.
*      CLEAR: TL_INPUT_041-COD_CLAS_BAL.
*    ENDIF.
*
*    IF TG_SAIDA-COD_CLAS_NOT2 IS NOT INITIAL.
*      READ TABLE TG_ZGLT039 WITH KEY CHAVE2 = TG_SAIDA-COD_CLAS_NOT2.
*      TL_INPUT_041-COD_CLAS_NOT2 = TG_ZGLT039-COD_NOTA.
*    ELSE.
*      CLEAR:  TL_INPUT_041-COD_CLAS_NOT2.
*    ENDIF.
*
*    MOVE: TG_SAIDA-BUKRS                TO TL_INPUT_041-BUKRS,
*          TG_SAIDA-GJAHR                TO TL_INPUT_041-GJAHR,
*          TG_SAIDA-SAKNR                TO TL_INPUT_041-SAKNR,
**              tg_saida-cod_clas_bal         TO tl_input_041-cod_clas_bal,
*          TG_SAIDA-CTA_MONET(1)         TO TL_INPUT_041-CTA_MONET,
*          TG_SAIDA-CTA_INTERCOMPANY(1)  TO TL_INPUT_041-CTA_INTERCOMPANY,
*          TG_SAIDA-DEP_RESP2            TO TL_INPUT_041-DEP_RESP2,
*          TG_SAIDA-BNAME2               TO TL_INPUT_041-BNAME2,
*          TG_SAIDA-PRAZO_ENTR           TO TL_INPUT_041-PRAZO_ENTR,
*          TG_SAIDA-CRIT_VECTO           TO TL_INPUT_041-CRIT_VECTO.
**              tg_saida-cod_clas_not2        TO tl_input_041-cod_clas_not2.
*
*    READ TABLE IT_ZGLT041_COPIA WITH KEY BUKRS = TL_INPUT_041-BUKRS
*                                         GJAHR = TL_INPUT_041-GJAHR
*                                         SAKNR = TL_INPUT_041-SAKNR TRANSPORTING NO FIELDS.
*    IF SY-SUBRC IS INITIAL.
*      READ TABLE IT_ZGLT043A_COPIA WITH KEY BUKRS = TL_INPUT_041-BUKRS
*                                            KTOPL = '0050'
*                                            KTOKS = TG_SAIDA-KTOKS TRANSPORTING NO FIELDS.
*      IF SY-SUBRC IS NOT INITIAL.
*        APPEND TL_INPUT_041 TO TL_INPUT_041_FAIL.
*        CLEAR: TL_INPUT_041.
*        ADD 1 TO VG_CONT_B.
*        CONTINUE.
*      ENDIF.
*    ENDIF.
*
*    APPEND TL_INPUT_041.
*    CLEAR: TL_INPUT_041.
*
*    CONCATENATE TG_SAIDA-BUKRS TG_SAIDA-GJAHR TG_SAIDA-SAKNR TG_SAIDA-COD_CLAS_BAL TG_SAIDA-COD_CLAS_NOT2 INTO WL_HEADER-TDNAME.
*    WL_HEADER-TDOBJECT = 'ZCRITERIO'.
*    WL_HEADER-TDID     = 'ZCRI'.
*    WL_HEADER-TDSPRAS  = SY-LANGU.
*    PERFORM SAVE_TEXT USING WL_HEADER.
*
*    IF TL_TLINES[] IS NOT INITIAL.
*      CALL FUNCTION 'SAVE_TEXT'
*        EXPORTING
*          HEADER          = WL_HEADER
*          SAVEMODE_DIRECT = 'X'
*        TABLES
*          LINES           = TL_TLINES
*        EXCEPTIONS
*          ID              = 1
*          LANGUAGE        = 2
*          NAME            = 3
*          OBJECT          = 4
*          OTHERS          = 5.
*    ENDIF.
*  ENDLOOP.
*
*  IF TL_INPUT_041[] IS NOT INITIAL.
*    MODIFY ZGLT041 FROM TABLE TL_INPUT_041.
*  ENDIF.
*
*  DESCRIBE TABLE TL_INPUT_041[] LINES VG_CONT_A.
*  MESSAGE S836(SD) WITH 'Total Copiados'
*                        VG_CONT_A
*                        'Total Não Copiados'
*                        VG_CONT_B.
*
*  IF VG_CONT_B IS NOT INITIAL.
*
*    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*      EXPORTING
*        I_GRID_TITLE          = 'Não Copiados'
*        I_STRUCTURE_NAME      = 'ZGLT041'
*        I_SCREEN_START_COLUMN = 5
*        I_SCREEN_START_LINE   = 5
*        I_SCREEN_END_COLUMN   = 80
*        I_SCREEN_END_LINE     = 20
*      TABLES
*        T_OUTTAB              = TL_INPUT_041_FAIL.
*
*  ENDIF.
*
*ENDFORM.
