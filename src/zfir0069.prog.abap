************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*----------------------------------------------------------------------*
* Programa   : ZFIR0069                                                *
* Descrição  : Transferências de Saldos de PIS e COFINS (Entre filiais)*
* Módulo     : FI                                Transação: ZFI0083    *
*----------------------------------------------------------------------*
* Autor       : Camila Brand                          Data: 28/06/2016 *
* Observações : Desenvolvimento inicial do Programa                    *
*----------------------------------------------------------------------*
* Data Modif    Autor         Descriçao      Hora           Request    *
************************************************************************
*                                                                      *
************************************************************************
REPORT ZFIR0069.


*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON, SLIS.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:FAGLFLEXA, BSIS, ZDE_MOV_LCT_BANCO.
*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

TYPES: ESTILO TYPE LVC_T_STYL.
TYPES: SINAL_AUX TYPE CHAR1.
TYPES: CONTA_AUX TYPE CHAR1.

TYPES:

  BEGIN OF TY_SKAT,
    SAKNR TYPE SKAT-SAKNR,
    TXT50 TYPE SKAT-TXT50,
  END   OF TY_SKAT,

  BEGIN OF TY_SAIDA,
    BUKRS        TYPE ZFIT0114-BUKRS,
    GJAHR        TYPE ZFIT0114-GJAHR,
    NOPER        TYPE ZFIT0114-NOPER,
    HKONT        TYPE ZFIT0114-HKONT,
    RBUSA        TYPE ZFIT0114-RBUSA,
    NAME1        TYPE T001W-NAME1,
    TP_LCTO      TYPE ZFIT0114-TP_LCTO,
    TXT50        TYPE SKAT-TXT50,
    SALDO_R      TYPE ZFIT0114-SALDO_R,
    SALDO_D      TYPE ZFIT0114-SALDO_D,
    LOTE         TYPE ZFIT0114-LOTE,
    DOC_LCTO     TYPE ZFIT0114-DOC_LCTO,
    DOC_CONTABIL TYPE ZFIT0114-DOC_CONTABIL,
    USNAM        TYPE ZFIT0114-USNAM,
    DT_ATUAL     TYPE ZFIT0114-DT_ATUAL,
    HR_ATUAL     TYPE ZFIT0114-HR_ATUAL,
    ROWCOLOR(4)  TYPE C,
    DT_VENC      TYPE DATS,
  END OF TY_SAIDA.


*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------**
DATA:
  IT_ZFIT0114       TYPE TABLE OF ZFIT0114,
  IT_CONTAS         TYPE ZCT_EMP_CONTAS,
  IT_SALDO_CONTAS   TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
  IT_SALDO_CONTAS_S TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
  IT_SALDO_CONTAS_2 TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
  IT_SALDO_CONTAS_3 TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
  IT_SKAT           TYPE TABLE OF TY_SKAT,
  IT_SAIDA          TYPE TABLE OF TY_SAIDA,

  "Tables para tela
  IT_SEL_ROWS       TYPE LVC_T_ROW,
  IT_FCAT           TYPE LVC_T_FCAT,
  IT_FCAT_PG        TYPE LVC_T_FCAT,
  IT_DTA            TYPE STANDARD TABLE OF BDCDATA WITH HEADER LINE.


*&---------------------------------------------------------------------*
*& Ranges
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
DATA:
  WA_ZFIT0114       TYPE ZFIT0114,
  WA_CONTAS         TYPE ZLC_EMP_CONTAS,
  WA_SALDO_CONTAS   TYPE ZDE_FI_GL_SALDO_FAGLFLEXT,
  WA_SALDO_CONTAS_S TYPE ZDE_FI_GL_SALDO_FAGLFLEXT,
  WA_SALDO_CONTAS_2 TYPE ZDE_FI_GL_SALDO_FAGLFLEXT,
  WA_SALDO_CONTAS_3 TYPE ZDE_FI_GL_SALDO_FAGLFLEXT,
  WA_SKAT           TYPE TY_SKAT,
  WA_SAIDA          TYPE TY_SAIDA,

  "WorkAreas para ALV e Tela
  C_ALV_TM          TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
  WA_ALV            TYPE REF TO CL_GUI_ALV_GRID,
  WA_ALV_PG         TYPE REF TO CL_GUI_ALV_GRID,
  WA_SEL_ROWS       TYPE LVC_S_ROW,
  WA_FCAT           TYPE LVC_S_FCAT,
  WA_FCAT_PG        TYPE LVC_S_FCAT,
  WA_LAYOUT         TYPE LVC_S_LAYO,
  WA_VARIANTE       TYPE DISVARIANT,
  WA_STABLE         TYPE LVC_S_STBL,
  TY_TOOLBAR        TYPE STB_BUTTON,
  WA_CONT           TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  WA_CONT_PG        TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  WA_DTA            TYPE BDCDATA,
  OPT               TYPE CTU_PARAMS.


DATA: X_SCREEN TYPE SY-DYNNR VALUE '0101'.

*&---------------------------------------------------------------------*
*& FIELD-SYMBOLS
*&---------------------------------------------------------------------*
FIELD-SYMBOLS: <SAIDA> TYPE TY_SAIDA,
               <FCAT>  TYPE LVC_S_FCAT.

DATA: OBJ_ZCL_UTIL_SD TYPE REF TO ZCL_UTIL_SD.

SELECTION-SCREEN: BEGIN OF SCREEN 0102 AS SUBSCREEN.
SELECT-OPTIONS: P_RBUKRS FOR FAGLFLEXA-RBUKRS            NO-EXTENSION NO INTERVALS , "Empresa           OBLIGATORY
                P_RACCT  FOR FAGLFLEXA-RACCT             NO INTERVALS              , "Contas            OBLIGATORY
                P_TPLANC FOR ZDE_MOV_LCT_BANCO-TP_LCTO   NO INTERVALS NO-EXTENSION , "Tipo de Documento OBLIGATORY
                P_POPER  FOR FAGLFLEXA-POPER             NO-EXTENSION NO INTERVALS , "Mês               OBLIGATORY
                P_YEAR   FOR FAGLFLEXA-RYEAR             NO-EXTENSION NO INTERVALS . "Ano               OBLIGATORY

SELECTION-SCREEN: END OF SCREEN 0102.

START-OF-SELECTION.
  CALL SCREEN 0100.

END-OF-SELECTION.

CLASS ZCL_EVENTS DEFINITION.
  PUBLIC SECTION.
    METHODS: CONSTRUCTOR IMPORTING IO_ALV_GRID  TYPE REF TO CL_GUI_ALV_GRID,
      ON_TOOLBAR  FOR EVENT TOOLBAR  OF CL_GUI_ALV_GRID IMPORTING E_OBJECT E_INTERACTIVE SENDER,
      ON_HANDLE   FOR EVENT USER_COMMAND          OF CL_GUI_ALV_GRID IMPORTING E_UCOMM,
      ON_CLICK    FOR EVENT HOTSPOT_CLICK         OF CL_GUI_ALV_GRID IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.


ENDCLASS.                    "ZCL_EVENTS DEFINITION

*       CLASS ZCL_CONTROL_NDF DEFINITION
CLASS ZCL_CONTROL_ZFIR0069 DEFINITION.
  PUBLIC SECTION.
    METHODS:
      CHECK_CAMPOS,
      SELECIONA_DADOS,
      MONTAR_ALV,
      MONTAR_LAY,
      CRIA_ALV,
      SALVAR_DADOS,
      GERAR_DOC_CONTABIL,
      REFRESH_DOC_CONTABIL,
      ESTORNO_DOC_CONTABIL,
      REFRESH_DADOS,
      Z_RETORNA_STATUS_ZIB IMPORTING I_DOC_LCTO TYPE NUM10 I_ANO_LCTO TYPE NUM4
                           EXPORTING E_ZIBCHV   TYPE ZIB_CONTABIL_CHV E_ZIBERR TYPE ZIB_CONTABIL_ERR.

  PRIVATE SECTION.
    DATA: C_CHECK TYPE C.

ENDCLASS.                    "ZCL_CONTROL_NDF DEFINITION

DATA: OBG_EVENTS  TYPE REF TO ZCL_EVENTS,
      OBG_CONTROL TYPE REF TO ZCL_CONTROL_ZFIR0069.

*       CLASS ZCL_EVENTS IMPLEMENTATION
CLASS ZCL_EVENTS IMPLEMENTATION.

  METHOD CONSTRUCTOR.
    CREATE OBJECT C_ALV_TM
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.

  METHOD ON_HANDLE.

    CASE E_UCOMM.
      WHEN 'GERAR'.
        OBG_CONTROL->GERAR_DOC_CONTABIL( ).
      WHEN 'REFRESH'.
        OBG_CONTROL->REFRESH_DOC_CONTABIL( ).
      WHEN 'ESTORNO'.
        OBG_CONTROL->ESTORNO_DOC_CONTABIL( ).
    ENDCASE.

  ENDMETHOD.

  METHOD ON_CLICK.

    DATA WA_0096_DEL TYPE ZFIT0096.

    UNASSIGN <SAIDA>.

    READ TABLE IT_SAIDA ASSIGNING <SAIDA> INDEX E_ROW_ID.

    CASE E_COLUMN_ID.
      WHEN 'DOC_LCTO'.

        CHECK ( <SAIDA>-DOC_LCTO IS NOT INITIAL ).
        FREE: IT_DTA.
        DEFINE SHDB.
          CLEAR IT_DTA.
          WA_DTA-PROGRAM   = &1.
          WA_DTA-DYNPRO    = &2.
          WA_DTA-DYNBEGIN  = &3.
          WA_DTA-FNAM      = &4.
          WA_DTA-FVAL      = &5.
          APPEND WA_DTA TO IT_DTA.
        END-OF-DEFINITION.

        SHDB:
        'ZGL015'   '0050' 'X'  ' '                   ' ',
        ' '        ' '    ' '  'BDC_CURSOR'          'WG_ZGLT034-LOTE',
        ' '        ' '    ' '  'BDC_OKCODE'          '=DISPLA',
        'ZGL015'   '0050' 'X'  ' '                   ' ',
        ' '        ' '    ' '  'BDC_CURSOR'          'WG_ZGLT035-DOC_LCTO',
        ' '        ' '    ' '  'BDC_OKCODE'          '=SEARCH',
        ' '        ' '    ' '  'WG_ZGLT035-DOC_LCTO' <SAIDA>-DOC_LCTO.

        OPT-DISMODE = 'E'.
        CALL TRANSACTION 'ZGL016' USING IT_DTA OPTIONS FROM OPT.


        SELECT SINGLE DOC_LCTO FROM ZGLT035 INTO <SAIDA>-DOC_LCTO
          WHERE DOC_LCTO EQ <SAIDA>-DOC_LCTO
            AND LOEKZ    EQ ''.

        IF NOT SY-SUBRC IS INITIAL.
          MOVE: ''     TO <SAIDA>-LOTE,
                ''     TO <SAIDA>-DOC_LCTO.

*          SELECT SINGLE * FROM ZFIT0104
*              INTO WA_0096_DEL
*                WHERE BUKRS EQ <SAIDA>-BUKRS AND
*                      TRADE_ID EQ <SAIDA>-TRADE_ID.
*
*          IF SY-SUBRC IS INITIAL.
*            DELETE FROM ZFIT0096 WHERE BUKRS EQ WA_0096_DEL-BUKRS
*                                AND TRADE_ID EQ WA_0096_DEL-TRADE_ID
*                            AND DOC_CONTABIL EQ 0.
*          ENDIF.

        ENDIF.

      WHEN 'DOC_CONTABIL'.

        CHECK ( <SAIDA>-DOC_CONTABIL IS NOT INITIAL ).
        SET PARAMETER ID 'BLN' FIELD <SAIDA>-DOC_CONTABIL.
        SET PARAMETER ID 'BUK' FIELD <SAIDA>-BUKRS.
        SET PARAMETER ID 'GJR' FIELD <SAIDA>-DT_VENC.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      WHEN 'LOTE'.

        CHECK ( <SAIDA>-LOTE IS NOT INITIAL ).

        FREE: IT_DTA.
        DEFINE SHDB.
          CLEAR IT_DTA.
          WA_DTA-PROGRAM   = &1.
          WA_DTA-DYNPRO    = &2.
          WA_DTA-DYNBEGIN  = &3.
          WA_DTA-FNAM      = &4.
          WA_DTA-FVAL      = &5.
          APPEND WA_DTA TO IT_DTA.
        END-OF-DEFINITION.

*        SHDB:
*          'ZGL017'   '0100' 'X'  ' '                   ' ',
*          ' '        ' '    ' '  'BDC_CURSOR'          'WG_CABECALHO-LOTE',
*          ' '        ' '    ' '  'BDC_OKCODE'          '=SEARCH',
*          ' '        ' '    ' '  'WG_CABECALHO-LOTE'  <SAIDA>-LOTE.
*
*        OPT-DISMODE = 'E'.
*
*        CALL TRANSACTION 'ZGL017' USING IT_DTA OPTIONS FROM OPT.


        SET PARAMETER ID 'LOT' FIELD  <SAIDA>-LOTE.
        CALL TRANSACTION 'ZGL017' AND SKIP FIRST SCREEN.




    ENDCASE.

    OBG_CONTROL->SELECIONA_DADOS( ).
    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.

  ENDMETHOD.                    "ON_CLICK

  METHOD ON_TOOLBAR.

    FIELD-SYMBOLS: <LS_TOOLBAR>  TYPE STB_BUTTON.

* Incluir novos Botoes
    FREE: TY_TOOLBAR.
    DEFINE TOOBAR.
      TY_TOOLBAR-ICON      = &1.
      TY_TOOLBAR-FUNCTION  = &2.
      TY_TOOLBAR-QUICKINFO = &3.
      TY_TOOLBAR-TEXT      = &4.
      TY_TOOLBAR-BUTN_TYPE = &5.
      APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
      CLEAR TY_TOOLBAR.
    END-OF-DEFINITION.
    TOOBAR:
            '@39@' 'GERAR'   'Gerar'     'Gerar Doc. Contabil' 0,
            ''     ''        ''          ''                    3,
            '@42@' 'REFRESH' 'Atualizar' 'Atualizar'           0,
            ''     ''        ''          ''                    3,
            '@F1@' 'ESTORNO' 'Estorno'   'Estorno'             0,
            ''     ''        ''          ''                    3.

    LOOP AT E_OBJECT->MT_TOOLBAR ASSIGNING <LS_TOOLBAR>.
      CASE <LS_TOOLBAR>-FUNCTION.
        WHEN '&CHECK' OR '&REFRESH' OR '&LOCAL&CUT' OR '&LOCAL&COPY' OR '&LOCAL&PASTE' OR '&LOCAL&UNDO'.
          DELETE E_OBJECT->MT_TOOLBAR INDEX SY-TABIX.
      ENDCASE.
    ENDLOOP.

    CALL METHOD C_ALV_TM->REORGANIZE( IO_ALV_TOOLBAR = E_OBJECT ).


  ENDMETHOD.                    "ON_TOOLBAR

  "CONTRUCTOR
ENDCLASS.


*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TI0100'.

  CREATE OBJECT: OBG_CONTROL.

  OBG_CONTROL->MONTAR_ALV( ).
  OBG_CONTROL->MONTAR_LAY( ).
  OBG_CONTROL->CRIA_ALV( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&       Class (Implementation)  ZCL_CONTROL_ZFIR0069
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS ZCL_CONTROL_ZFIR0069 IMPLEMENTATION.

  METHOD CHECK_CAMPOS.

    IF P_RBUKRS-LOW IS INITIAL.
      C_CHECK = ABAP_TRUE.
      MESSAGE S836(SD) WITH TEXT-009.
      EXIT.
    ENDIF.

    IF P_RACCT-LOW IS INITIAL.
      C_CHECK = ABAP_TRUE.
      MESSAGE S836(SD) WITH TEXT-010.
      EXIT.
    ENDIF.

    IF P_TPLANC-LOW IS INITIAL.
      C_CHECK = ABAP_TRUE.
      MESSAGE S836(SD) WITH TEXT-011.
      EXIT.
    ENDIF.

    IF P_POPER-LOW IS INITIAL.
      C_CHECK = ABAP_TRUE.
      MESSAGE S836(SD) WITH TEXT-012.
      EXIT.
    ENDIF.

    IF P_YEAR-LOW IS INITIAL.
      C_CHECK = ABAP_TRUE.
      MESSAGE S836(SD) WITH TEXT-013.
      EXIT.
    ENDIF.




  ENDMETHOD.
  METHOD SELECIONA_DADOS.

    CHECK C_CHECK IS INITIAL.

    DATA: E_X001         TYPE X001,
          W_T001         TYPE T001,
          WA_RANGE_RACCT TYPE FAGL_RANGE_RACCT,
          V_NAME1        TYPE T001W-NAME1,
          V_LOTE         TYPE ZFIT0114-LOTE,
          V_DOC_LCTO     TYPE ZFIT0114-DOC_LCTO,
          V_DOC_CONTABIL TYPE ZFIT0114-DOC_CONTABIL,
          V_HKONT        TYPE ZFIT0114-HKONT,
          V_USNAM        TYPE ZFIT0114-USNAM,
          V_DT_ATUAL     TYPE ZFIT0114-DT_ATUAL,
          V_HR_ATUAL     TYPE ZFIT0114-HR_ATUAL.

    CLEAR: IT_CONTAS, IT_SAIDA, IT_SKAT , IT_SALDO_CONTAS[] ,WA_SAIDA, IT_SALDO_CONTAS,
    V_LOTE,V_DOC_LCTO,V_DOC_CONTABIL.

    LOOP AT P_RACCT INTO WA_RANGE_RACCT.
      WA_CONTAS-BUKRS = P_RBUKRS-LOW.
      WA_CONTAS-SAKNR = WA_RANGE_RACCT-LOW.
      APPEND WA_CONTAS TO IT_CONTAS.
      CLEAR: WA_CONTAS.
    ENDLOOP.


    SELECT SAKNR
           TXT50
        FROM SKAT
          INTO TABLE IT_SKAT
        FOR ALL ENTRIES IN IT_CONTAS
        WHERE SAKNR   EQ IT_CONTAS-SAKNR
        AND KTOPL EQ '0050'
         AND SPRAS EQ 'PT'.



    CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
      EXPORTING
        RYEAR          = P_YEAR-LOW
        "WAERS          = 'BRL'
        CONTAS         = IT_CONTAS
        P_GERAR_TODAS  = ABAP_TRUE
        RLDNR          = '50'
        P_GERAR_FILIAL = ABAP_TRUE
      TABLES
        IT_SALDOS      = IT_SALDO_CONTAS
        IT_SALDOS_2    = IT_SALDO_CONTAS_2
        IT_SALDOS_3    = IT_SALDO_CONTAS_3
      EXCEPTIONS
        MOEDA_NAO_ADM  = 1
        ERRO_LEDGER    = 2
        OTHERS         = 3.


    SORT:  IT_SALDO_CONTAS BY RACCT,
           IT_SALDO_CONTAS_2 BY RBUSA RACCT,
           IT_SKAT         BY SAKNR,
           IT_ZFIT0114     BY HKONT.

    LOOP AT IT_SALDO_CONTAS INTO WA_SALDO_CONTAS.



      IF WA_SALDO_CONTAS IS NOT INITIAL.

        CLEAR: WA_SALDO_CONTAS_2,WA_SKAT.

        READ TABLE IT_SKAT INTO WA_SKAT  WITH KEY SAKNR = WA_SALDO_CONTAS-RACCT BINARY SEARCH .

        READ TABLE IT_SALDO_CONTAS_2 INTO WA_SALDO_CONTAS_2  WITH KEY RBUSA  = WA_SALDO_CONTAS-RBUSA RACCT = WA_SALDO_CONTAS-RACCT.

        WA_SAIDA-BUKRS        = P_RBUKRS-LOW.
        WA_SAIDA-GJAHR        = P_YEAR-LOW.
        WA_SAIDA-NOPER        = P_POPER-LOW.
        WA_SAIDA-HKONT        = WA_SALDO_CONTAS-RACCT.


        CONCATENATE   P_YEAR-LOW  P_POPER-LOW+1(2) '01' INTO WA_SAIDA-DT_VENC.

        " Pego o Final do mes
        CALL FUNCTION 'LAST_DAY_OF_MONTHS'
          EXPORTING
            DAY_IN            = WA_SAIDA-DT_VENC
          IMPORTING
            LAST_DAY_OF_MONTH = WA_SAIDA-DT_VENC.


        WA_SAIDA-TP_LCTO      = P_TPLANC-LOW.
        WA_SAIDA-TXT50        = WA_SKAT-TXT50.
        WA_SAIDA-RBUSA        = WA_SALDO_CONTAS-RBUSA.


        " Verificar se esse mês já foi processado para essa filial
        SELECT SINGLE LOTE DOC_LCTO DOC_CONTABIL USNAM DT_ATUAL HR_ATUAL
             FROM ZFIT0114
          INTO  (V_LOTE, V_DOC_LCTO, V_DOC_CONTABIL, V_USNAM, V_DT_ATUAL, V_HR_ATUAL  )
        WHERE BUKRS EQ WA_SAIDA-BUKRS
          AND GJAHR EQ WA_SAIDA-GJAHR
          AND NOPER EQ WA_SAIDA-NOPER
          AND HKONT EQ WA_SAIDA-HKONT
          AND RBUSA EQ WA_SAIDA-RBUSA.


        WA_SAIDA-LOTE         = V_LOTE.
        WA_SAIDA-DOC_LCTO     = V_DOC_LCTO.
        WA_SAIDA-DOC_CONTABIL = V_DOC_CONTABIL.
        WA_SAIDA-USNAM        = V_USNAM.
        WA_SAIDA-DT_ATUAL     = V_DT_ATUAL.
        WA_SAIDA-HR_ATUAL     = V_HR_ATUAL.

        CLEAR V_NAME1.
        SELECT SINGLE NAME1 INTO WA_SAIDA-NAME1 FROM T001W WHERE WERKS = WA_SALDO_CONTAS-RBUSA.



        CASE P_POPER-LOW.

          WHEN '001'. "Janeiro

            WA_SAIDA-SALDO_R    = WA_SALDO_CONTAS-SL01.

            WA_SAIDA-SALDO_D    = WA_SALDO_CONTAS_2-SL01.

          WHEN '002'. "Fevereiro

            WA_SAIDA-SALDO_R    = WA_SALDO_CONTAS-SL01   + WA_SALDO_CONTAS-SL02.

            WA_SAIDA-SALDO_D    = WA_SALDO_CONTAS_2-SL01 + WA_SALDO_CONTAS_2-SL02.

          WHEN '003'. "Março

            WA_SAIDA-SALDO_R    = WA_SALDO_CONTAS-SL01   + WA_SALDO_CONTAS-SL02   + WA_SALDO_CONTAS-SL03.

            WA_SAIDA-SALDO_D    = WA_SALDO_CONTAS_2-SL01 + WA_SALDO_CONTAS_2-SL02 + WA_SALDO_CONTAS_2-SL03.

          WHEN '004'. "Abril

            WA_SAIDA-SALDO_R    = WA_SALDO_CONTAS-SL01   + WA_SALDO_CONTAS-SL02   + WA_SALDO_CONTAS-SL03   + WA_SALDO_CONTAS-SL04.

            WA_SAIDA-SALDO_D    = WA_SALDO_CONTAS_2-SL01 + WA_SALDO_CONTAS_2-SL02 + WA_SALDO_CONTAS_2-SL03 + WA_SALDO_CONTAS_2-SL04.

          WHEN '005'. "Maio

            WA_SAIDA-SALDO_R    = WA_SALDO_CONTAS-SL01  + WA_SALDO_CONTAS-SL02   + WA_SALDO_CONTAS-SL03   + WA_SALDO_CONTAS-SL04 +
                                  WA_SALDO_CONTAS-SL05.

            WA_SAIDA-SALDO_D    = WA_SALDO_CONTAS_2-SL01 + WA_SALDO_CONTAS_2-SL02 + WA_SALDO_CONTAS_2-SL03 + WA_SALDO_CONTAS_2-SL04 +
                                  WA_SALDO_CONTAS_2-SL05.

          WHEN '006'. "Junho

            WA_SAIDA-SALDO_R    = WA_SALDO_CONTAS-SL01 + WA_SALDO_CONTAS-SL02 + WA_SALDO_CONTAS-SL03 + WA_SALDO_CONTAS-SL04 +
                                  WA_SALDO_CONTAS-SL05 + WA_SALDO_CONTAS-SL06.

            WA_SAIDA-SALDO_D    = WA_SALDO_CONTAS_2-SL01 + WA_SALDO_CONTAS_2-SL02 + WA_SALDO_CONTAS_2-SL03 + WA_SALDO_CONTAS_2-SL04 +
                                  WA_SALDO_CONTAS_2-SL05 +  WA_SALDO_CONTAS_2-SL06.


          WHEN '007'. "Julho
            WA_SAIDA-SALDO_R    = WA_SALDO_CONTAS-SL01 + WA_SALDO_CONTAS-SL02 + WA_SALDO_CONTAS-SL03 + WA_SALDO_CONTAS-SL04 +
                                  WA_SALDO_CONTAS-SL05 + WA_SALDO_CONTAS-SL06 + WA_SALDO_CONTAS-SL07.

            WA_SAIDA-SALDO_D    = WA_SALDO_CONTAS_2-SL01 + WA_SALDO_CONTAS_2-SL02 + WA_SALDO_CONTAS_2-SL03 + WA_SALDO_CONTAS_2-SL04 +
                                  WA_SALDO_CONTAS_2-SL05 + WA_SALDO_CONTAS_2-SL06 + WA_SALDO_CONTAS_2-SL07.

          WHEN '008'. "Agosto
            WA_SAIDA-SALDO_R    = WA_SALDO_CONTAS-SL01 + WA_SALDO_CONTAS-SL02 + WA_SALDO_CONTAS-SL03 + WA_SALDO_CONTAS-SL04 +
                                  WA_SALDO_CONTAS-SL05 + WA_SALDO_CONTAS-SL06 + WA_SALDO_CONTAS-SL07 + WA_SALDO_CONTAS-SL08.

            WA_SAIDA-SALDO_D    = WA_SALDO_CONTAS_2-SL01 + WA_SALDO_CONTAS_2-SL02 + WA_SALDO_CONTAS_2-SL03 + WA_SALDO_CONTAS_2-SL04 +
                                  WA_SALDO_CONTAS_2-SL05 + WA_SALDO_CONTAS_2-SL06 + WA_SALDO_CONTAS_2-SL07 + WA_SALDO_CONTAS_2-SL08.

          WHEN '009'. "Setembro

            WA_SAIDA-SALDO_R    = WA_SALDO_CONTAS-SL01 + WA_SALDO_CONTAS-SL02 + WA_SALDO_CONTAS-SL03 + WA_SALDO_CONTAS-SL04 +
                                  WA_SALDO_CONTAS-SL05 + WA_SALDO_CONTAS-SL06 + WA_SALDO_CONTAS-SL07 + WA_SALDO_CONTAS-SL08 +
                                  WA_SALDO_CONTAS-SL09.

            WA_SAIDA-SALDO_D    = WA_SALDO_CONTAS_2-SL01 + WA_SALDO_CONTAS_2-SL02 + WA_SALDO_CONTAS_2-SL03 + WA_SALDO_CONTAS_2-SL04 +
                                  WA_SALDO_CONTAS_2-SL05 + WA_SALDO_CONTAS_2-SL06 + WA_SALDO_CONTAS_2-SL07 + WA_SALDO_CONTAS_2-SL08 +
                                  WA_SALDO_CONTAS_2-SL09.

          WHEN '010'. "Outubro

            WA_SAIDA-SALDO_R    = WA_SALDO_CONTAS-SL01 + WA_SALDO_CONTAS-SL02 + WA_SALDO_CONTAS-SL03 + WA_SALDO_CONTAS-SL04 +
                                  WA_SALDO_CONTAS-SL05 + WA_SALDO_CONTAS-SL06 + WA_SALDO_CONTAS-SL07 + WA_SALDO_CONTAS-SL08 +
                                  WA_SALDO_CONTAS-SL09 + WA_SALDO_CONTAS-SL10.

            WA_SAIDA-SALDO_D    = WA_SALDO_CONTAS_2-SL01 + WA_SALDO_CONTAS_2-SL02 + WA_SALDO_CONTAS_2-SL03 + WA_SALDO_CONTAS_2-SL04 +
                                  WA_SALDO_CONTAS_2-SL05 + WA_SALDO_CONTAS_2-SL06 + WA_SALDO_CONTAS_2-SL07 + WA_SALDO_CONTAS_2-SL08 +
                                  WA_SALDO_CONTAS_2-SL09 + WA_SALDO_CONTAS_2-SL10.

          WHEN '011'. "Novembro

            WA_SAIDA-SALDO_R    = WA_SALDO_CONTAS-SL01 + WA_SALDO_CONTAS-SL02 + WA_SALDO_CONTAS-SL03 + WA_SALDO_CONTAS-SL04 +
                                  WA_SALDO_CONTAS-SL05 + WA_SALDO_CONTAS-SL06 + WA_SALDO_CONTAS-SL07 + WA_SALDO_CONTAS-SL08 +
                                  WA_SALDO_CONTAS-SL09 + WA_SALDO_CONTAS-SL10 + WA_SALDO_CONTAS-SL11.

            WA_SAIDA-SALDO_D    = WA_SALDO_CONTAS_2-SL01 + WA_SALDO_CONTAS_2-SL02 + WA_SALDO_CONTAS_2-SL03 + WA_SALDO_CONTAS_2-SL04 +
                                  WA_SALDO_CONTAS_2-SL05 + WA_SALDO_CONTAS_2-SL06 + WA_SALDO_CONTAS_2-SL07 + WA_SALDO_CONTAS_2-SL08 +
                                  WA_SALDO_CONTAS_2-SL09 + WA_SALDO_CONTAS_2-SL10 + WA_SALDO_CONTAS_2-SL11.

          WHEN '012'. "Dezembro e não usa os complementares

            WA_SAIDA-SALDO_R    = WA_SALDO_CONTAS-SL01 + WA_SALDO_CONTAS-SL02 + WA_SALDO_CONTAS-SL03  + WA_SALDO_CONTAS-SL04 +
                                  WA_SALDO_CONTAS-SL05 + WA_SALDO_CONTAS-SL06 + WA_SALDO_CONTAS-SL07  + WA_SALDO_CONTAS-SL08 +
                                  WA_SALDO_CONTAS-SL09 + WA_SALDO_CONTAS-SL10 + WA_SALDO_CONTAS-SL11  + WA_SALDO_CONTAS-SL12.


            WA_SAIDA-SALDO_D    = WA_SALDO_CONTAS_2-SL01 + WA_SALDO_CONTAS_2-SL02 + WA_SALDO_CONTAS_2-SL03 + WA_SALDO_CONTAS_2-SL04 +
                                  WA_SALDO_CONTAS_2-SL05 + WA_SALDO_CONTAS_2-SL06 + WA_SALDO_CONTAS_2-SL07 + WA_SALDO_CONTAS_2-SL08 +
                                  WA_SALDO_CONTAS_2-SL09 + WA_SALDO_CONTAS_2-SL10 + WA_SALDO_CONTAS_2-SL11 + WA_SALDO_CONTAS_2-SL12.


        ENDCASE.

        CLEAR : V_HKONT.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = WA_SAIDA-HKONT
          IMPORTING
            OUTPUT = V_HKONT.

        "Verificar os campos que podem ser calulados.
        "Contas que começam com 1 e são negativas não processa
        IF V_HKONT(1) = 1 AND WA_SAIDA-SALDO_R < 0 .
          WA_SAIDA-ROWCOLOR  = 'C600'.
        ENDIF.

        "Contas que começam com 2 e são positivas não processa
        IF V_HKONT(1) = 2 AND WA_SAIDA-SALDO_R > 0 .
          WA_SAIDA-ROWCOLOR  = 'C600'.
        ENDIF.

        "Não processa filial 0101.
        IF WA_SAIDA-RBUSA = '0101'.
          WA_SAIDA-ROWCOLOR  = 'C600'.
        ENDIF.

        "Não processa filial 0101.
        IF WA_SAIDA-LOTE IS NOT INITIAL.
          WA_SAIDA-ROWCOLOR  = 'C600'.
        ENDIF.

        APPEND WA_SAIDA TO IT_SAIDA.
        CLEAR: WA_SAIDA, WA_SALDO_CONTAS_2, WA_SALDO_CONTAS, WA_SKAT,
          V_LOTE, V_DOC_LCTO, V_DOC_CONTABIL,  V_USNAM , V_DT_ATUAL,V_HR_ATUAL.

      ELSE.
        MESSAGE S836(SD) WITH TEXT-014.
      ENDIF.

    ENDLOOP.

    DELETE IT_SAIDA WHERE SALDO_R = 0 AND LOTE IS INITIAL.
    DELETE IT_SAIDA WHERE RBUSA   = '0101'.

  ENDMETHOD.

  METHOD CRIA_ALV.

    CASE X_SCREEN.
      WHEN '0101'.

        IF WA_CONT IS INITIAL.

          CREATE OBJECT WA_CONT
            EXPORTING
              CONTAINER_NAME = 'C_01'.

          CREATE OBJECT WA_ALV
            EXPORTING
              I_SHELLSTYLE    = 0
              I_PARENT        = WA_CONT
              I_APPL_EVENTS   = SPACE
              I_FCAT_COMPLETE = SPACE.

          CREATE OBJECT OBG_EVENTS
            EXPORTING
              IO_ALV_GRID = WA_ALV.

          SET HANDLER:
                       OBG_EVENTS->ON_TOOLBAR  FOR WA_ALV,
                       OBG_EVENTS->ON_HANDLE   FOR WA_ALV,
                       OBG_EVENTS->ON_CLICK    FOR WA_ALV.


          CALL METHOD WA_ALV->SET_TABLE_FOR_FIRST_DISPLAY
            EXPORTING
              IS_LAYOUT       = WA_LAYOUT
              IS_VARIANT      = WA_VARIANTE
              I_SAVE          = 'X'
            CHANGING
              IT_OUTTAB       = IT_SAIDA
              IT_FIELDCATALOG = IT_FCAT.

          CALL METHOD WA_ALV->REGISTER_EDIT_EVENT( I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED ).
          CALL METHOD WA_ALV->REGISTER_EDIT_EVENT( I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER ).

        ELSE.
          CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.
        ENDIF.

      WHEN '0103'.

        IF WA_CONT_PG IS INITIAL.

          CREATE OBJECT WA_CONT_PG
            EXPORTING
              CONTAINER_NAME = 'C_02'.

          CREATE OBJECT WA_ALV_PG
            EXPORTING
              I_SHELLSTYLE    = 0
              I_PARENT        = WA_CONT_PG
              I_APPL_EVENTS   = SPACE
              I_FCAT_COMPLETE = SPACE.

          CALL METHOD WA_ALV_PG->SET_TABLE_FOR_FIRST_DISPLAY
            EXPORTING
              IS_LAYOUT       = WA_LAYOUT
              IS_VARIANT      = WA_VARIANTE
              I_SAVE          = 'X'
            CHANGING
              IT_OUTTAB       = IT_SAIDA
              IT_FIELDCATALOG = IT_FCAT_PG.

        ELSE.
          CALL METHOD WA_ALV_PG->REFRESH_TABLE_DISPLAY.
        ENDIF.

    ENDCASE.
  ENDMETHOD.

  METHOD GERAR_DOC_CONTABIL.

    DATA: E_NUM_LOTE    TYPE ZLOTE_NUM,
          WL_ZGLT031    TYPE ZGLT031,
          GT_ZGL032_AUX TYPE TABLE OF ZGLT032,
          GT_ZGLT032    TYPE TABLE OF ZGLT032,
          WL_ZGLT032    TYPE ZGLT032,
          GT_ZGLT036    TYPE TABLE OF ZGLT036,
          WL_ZGLT036    TYPE ZGLT036,
          DP_RESP       TYPE CHAR2,
          DESCRICAO     TYPE ZDESCR_LOTE,
          WL_ZGLT035    TYPE ZGLT035,
          V_GSBER       TYPE ZFIT0114-RBUSA,
          V_HKONT       TYPE ZFIT0114-HKONT.



    CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

    CALL METHOD WA_ALV->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SEL_ROWS.

    CHECK NOT IT_SEL_ROWS IS INITIAL.

    CLEAR: WL_ZGLT031, WL_ZGLT035.

    "Parametro da tela inicial

    SELECT SINGLE * FROM ZGLT031 INTO WL_ZGLT031 WHERE TP_LCTO EQ P_TPLANC-LOW.

    IF NOT WL_ZGLT031 IS INITIAL.

      FREE: GT_ZGL032_AUX, GT_ZGLT032.

      SELECT * FROM ZGLT032 INTO TABLE GT_ZGL032_AUX WHERE TP_LCTO EQ P_TPLANC-LOW.

      LOOP AT GT_ZGL032_AUX INTO WL_ZGLT032 WHERE TP_LCTO = P_TPLANC-LOW.

        IF WL_ZGLT032-HKONT IN P_RACCT.
          APPEND WL_ZGLT032 TO GT_ZGLT032.
        ENDIF.

      ENDLOOP.

      IF NOT GT_ZGLT032 IS INITIAL.

        MOVE WL_ZGLT031-DESCRICAO TO DESCRICAO.
        MOVE WL_ZGLT031-DPTO_RESP TO DP_RESP.


        " Gera número do lote
        CALL METHOD ZCL_GERAR_LOTE=>CREATE_LOTE
          EXPORTING
            I_BUKRS      = P_RBUKRS-LOW
            I_DESCR_LOTE = DESCRICAO
            I_DEP_RESP   = DP_RESP
            I_USER_RESP  = SY-UNAME
          IMPORTING
            E_NUM_LOTE   = WL_ZGLT035-LOTE.


      ELSE.
        "Tipo de Lançamento não Encontrado!
        MESSAGE S836(SD) WITH TEXT-005.
      ENDIF.

*    ELSE.
*      "Contas já processada!
*      MESSAGE S836(SD) WITH TEXT-006.

    ENDIF.



    LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.

      READ TABLE IT_SAIDA ASSIGNING <SAIDA> INDEX WA_SEL_ROWS-INDEX.

      IF SY-SUBRC IS NOT INITIAL OR <SAIDA>-ROWCOLOR <> 'C600'. "Não existe esse lançamento então gera a ZGL

        MOVE: WL_ZGLT035-LOTE           TO <SAIDA>-LOTE,
              P_RBUKRS-LOW              TO WL_ZGLT035-BUKRS,
              P_TPLANC-LOW              TO WL_ZGLT035-TP_LCTO,
              DP_RESP                   TO WL_ZGLT035-DPTO_RESP,
              'BRL'                     TO WL_ZGLT035-MOEDA_DOC,
              WL_ZGLT031-ST_LC_MOEDA    TO WL_ZGLT035-ST_LC_MOEDA,
              WL_ZGLT031-MOEDA_INT_HIST TO WL_ZGLT035-MOEDA_INT_HIST,
              WL_ZGLT031-MOEDA_FT_HIST  TO WL_ZGLT035-MOEDA_FT_HIST,
              WL_ZGLT031-MOEDA_GP_HIST  TO WL_ZGLT035-MOEDA_GP_HIST,
              WL_ZGLT031-BLART          TO WL_ZGLT035-BLART,
              WL_ZGLT031-DESCRICAO      TO WL_ZGLT035-XBLNR,
              WL_ZGLT031-BKTXT          TO WL_ZGLT035-BKTXT,
              <SAIDA>-DT_VENC           TO WL_ZGLT035-BUDAT,
              <SAIDA>-DT_VENC           TO WL_ZGLT035-BLDAT,
              <SAIDA>-DT_VENC           TO WL_ZGLT035-DT_LCTO,
              WL_ZGLT031-PROV_EST       TO WL_ZGLT035-PROV_EST,
              WL_ZGLT031-ST_AP_FISCAL   TO WL_ZGLT035-ST_AP_FISCAL,
              P_POPER-LOW               TO WL_ZGLT035-MONAT,
              P_YEAR-LOW                TO WL_ZGLT035-GJAHR,
              SY-UNAME                  TO WL_ZGLT035-USNAM,
              SY-DATUM                  TO WL_ZGLT035-DT_ENTRADA,
              SY-UZEIT                  TO WL_ZGLT035-HR_ENTRADA.



        FREE: GT_ZGLT036.

        LOOP AT GT_ZGLT032 INTO WL_ZGLT032.

          CLEAR : V_HKONT.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              INPUT  = <SAIDA>-HKONT
            IMPORTING
              OUTPUT = V_HKONT.

          CASE V_HKONT(1).
            WHEN 1.
              IF WL_ZGLT032-BSCHL = 50.
                MOVE  <SAIDA>-RBUSA   TO  WL_ZGLT036-GSBER.
              ELSEIF WL_ZGLT032-BSCHL = 40.
                CLEAR : V_GSBER.

                "CONCATENATE  <SAIDA>-BUKRS+2(2) <SAIDA>-RBUSA(2) INTO V_GSBER.
                CONCATENATE  <SAIDA>-BUKRS+2(2) '01' INTO V_GSBER.
                MOVE  V_GSBER  TO  WL_ZGLT036-GSBER.
              ENDIF.

            WHEN 2.
              IF WL_ZGLT032-BSCHL = 50.
                CLEAR : V_GSBER.
                CONCATENATE  <SAIDA>-BUKRS+2(2) '01' INTO V_GSBER.
                MOVE  V_GSBER  TO  WL_ZGLT036-GSBER.
              ELSEIF  WL_ZGLT032-BSCHL = 40.
                MOVE  <SAIDA>-RBUSA   TO  WL_ZGLT036-GSBER.
              ENDIF.

          ENDCASE.

          MOVE: SY-TABIX              TO WL_ZGLT036-SEQITEM,
                WL_ZGLT032-TP_LCTO    TO WL_ZGLT036-TP_LCTO,
                WL_ZGLT032-BSCHL      TO WL_ZGLT036-BSCHL,

                WL_ZGLT032-HKONT      TO WL_ZGLT036-HKONT,
                WL_ZGLT032-SGTXT      TO WL_ZGLT036-SGTXT,
                <SAIDA>-DT_VENC       TO WL_ZGLT036-DT_VCT, " Ultimo dia do mês que esta processando no parametro
                WL_ZGLT032-UMSKZ      TO WL_ZGLT036-UMSKZ.


          MOVE:  ABS( <SAIDA>-SALDO_R ) TO WL_ZGLT036-VLR_MOEDA_DOC,
                 ABS( <SAIDA>-SALDO_R ) TO WL_ZGLT036-VLR_MOEDA_INT,
                 ABS( <SAIDA>-SALDO_D ) TO WL_ZGLT036-VLR_MOEDA_FORTE.


          APPEND WL_ZGLT036 TO GT_ZGLT036.

          CLEAR: WL_ZGLT036, WL_ZGLT032.

        ENDLOOP.

        CALL METHOD ZCL_GERAR_LOTE=>CONTABILIZAR_LOTE(
          CHANGING
            I_ZGLT036 = GT_ZGLT036
            I_ZGLT035 = WL_ZGLT035 ).

        MOVE: WL_ZGLT035-LOTE     TO <SAIDA>-LOTE,
              WL_ZGLT035-DOC_LCTO TO <SAIDA>-DOC_LCTO.


        MOVE-CORRESPONDING <SAIDA> TO WA_ZFIT0114.

        WA_ZFIT0114-BUKRS    = <SAIDA>-BUKRS.
        WA_ZFIT0114-GJAHR    = <SAIDA>-GJAHR.
        WA_ZFIT0114-NOPER    = <SAIDA>-NOPER.
        WA_ZFIT0114-HKONT    = <SAIDA>-HKONT.
        WA_ZFIT0114-RBUSA    = <SAIDA>-RBUSA.
        WA_ZFIT0114-TP_LCTO  = <SAIDA>-TP_LCTO.
        WA_ZFIT0114-SALDO_R  = <SAIDA>-SALDO_R.
        WA_ZFIT0114-SALDO_D  = <SAIDA>-SALDO_D.
        WA_ZFIT0114-LOTE     = <SAIDA>-LOTE.
        WA_ZFIT0114-DOC_LCTO = <SAIDA>-DOC_LCTO.
        WA_ZFIT0114-USNAM    = SY-UNAME.
        WA_ZFIT0114-DT_ATUAL = SY-DATUM.
        WA_ZFIT0114-HR_ATUAL = SY-UZEIT.

        MODIFY ZFIT0114 FROM WA_ZFIT0114.

        <SAIDA>-ROWCOLOR = 'C600'.

      ELSE.
        "Contas já processada!
        MESSAGE S836(SD) WITH TEXT-006.

      ENDIF.

    ENDLOOP.

    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.

  ENDMETHOD.

  METHOD MONTAR_ALV.

    FREE: IT_FCAT, WA_FCAT.

    CASE X_SCREEN.
      WHEN '0101'.
        DEFINE ALV.
          WA_FCAT-HOTSPOT   = &1.
          WA_FCAT-REF_TABLE = &2.
          WA_FCAT-REF_FIELD = &3.
          WA_FCAT-TABNAME   = &4.
          WA_FCAT-FIELDNAME = &5.
          WA_FCAT-SCRTEXT_L = &6.
          WA_FCAT-SCRTEXT_M = &6.
          WA_FCAT-NO_ZERO   = &7.
          WA_FCAT-OUTPUTLEN = &8.
          WA_FCAT-EDIT      = &9.

          APPEND WA_FCAT TO IT_FCAT.
          CLEAR WA_FCAT.
        END-OF-DEFINITION.
        ALV:
             ''  ''         ''                 'IT_SAIDA' 'RBUSA'          'Filial'       ' '  ' '  ' ',
             ''  ''         ''                 'IT_SAIDA' 'NAME1'          'Descrição'    ' '  ' '  ' ',
             ''  ''         ''                 'IT_SAIDA' 'HKONT'          'Conta'        ' '  ' '  ' ',
             ''  ''         ''                 'IT_SAIDA' 'TXT50'          'Descrição'    ' '  ' '  ' ',
            'X'  ''         ''                 'IT_SAIDA' 'LOTE'           'Lote'         ' '  ' '  ' ',
            'X'  ''         ''                 'IT_SAIDA' 'DOC_LCTO'       'Documento'    ' '  ' '  ' ',
            'X'  ''         ''                 'IT_SAIDA' 'DOC_CONTABIL'   'Doc.Contabil' ' '  ' '  ' ',
             ''  ''         ''                 'IT_SAIDA' 'SALDO_R'        'Saldo R$'     ' '  ' '  ' ',
             ''  ''         ''                 'IT_SAIDA' 'SALDO_D'        'Saldo U$'     ' '  ' '  ' '.

    ENDCASE.
  ENDMETHOD.

  METHOD MONTAR_LAY.

    CLEAR: WA_LAYOUT, WA_VARIANTE.

    WA_LAYOUT-ZEBRA      = ABAP_TRUE.
    WA_LAYOUT-NO_ROWINS  = ABAP_TRUE.
    WA_LAYOUT-STYLEFNAME = 'ESTILO'.
    WA_LAYOUT-INFO_FNAME = 'ROWCOLOR'.
    WA_LAYOUT-SEL_MODE   = 'C'.
    WA_STABLE-ROW        = ABAP_TRUE.

    WA_VARIANTE-REPORT  = SY-REPID.

  ENDMETHOD.

  METHOD REFRESH_DADOS.

    ME->SELECIONA_DADOS( ).

  ENDMETHOD.                    "REFRESH_DADOS

  METHOD SALVAR_DADOS.

    CHECK NOT IT_SAIDA IS INITIAL.

    "UNASSIGN <SAIDA>.
    "CLEAR: WA_0096_AUX.
    "FREE IT_0096_AUX.

    "LOOP AT IT_SAIDA ASSIGNING <SAIDA>.

*      IF <SAIDA>-STATUS EQ ''.
*        MOVE 'S'    TO <SAIDA>-STATUS.
*        MOVE '@08@' TO <SAIDA>-ICON.
*      ENDIF.

*      MOVE-CORRESPONDING <SAIDA> TO WA_0096_AUX.
*      APPEND WA_0096_AUX TO IT_0096_AUX.

    "ENDLOOP.

    "MODIFY ZFIT0096 FROM TABLE IT_0096_AUX.

    "ME->SELECIONA_DADOS( ).
    "ME->AGRUPA_DADOS( ).

    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.

  ENDMETHOD.

  METHOD REFRESH_DOC_CONTABIL.

    DATA: WL_ZGLT034   TYPE ZGLT034,
          WL_ZIB_CHAVE TYPE ZIB_CONTABIL_CHV,
          WL_ZIB_ERRO  TYPE ZIB_CONTABIL_ERR.


    UNASSIGN <SAIDA>.
    CHECK NOT IT_SAIDA IS INITIAL.

    LOOP AT IT_SAIDA ASSIGNING <SAIDA> WHERE LOTE IS NOT INITIAL.

      SELECT SINGLE *
        FROM ZGLT034
        INTO WL_ZGLT034
       WHERE BUKRS = <SAIDA>-BUKRS
         AND LOTE  = <SAIDA>-LOTE.

      ME->Z_RETORNA_STATUS_ZIB( EXPORTING
                               I_DOC_LCTO = <SAIDA>-DOC_LCTO
                               I_ANO_LCTO = <SAIDA>-DT_VENC(4)
                               IMPORTING
                               E_ZIBCHV   = WL_ZIB_CHAVE
                               E_ZIBERR   = WL_ZIB_ERRO ).

      " Se gerou o documento e ele não tinha então atualiza tela e tabela
      IF ( WL_ZIB_CHAVE IS NOT INITIAL ) AND ( <SAIDA>-DOC_CONTABIL IS INITIAL ).
        MOVE:  WL_ZIB_CHAVE-BELNR TO <SAIDA>-DOC_CONTABIL.

        "Atualiza Doc contabil se enontrar.
        CLEAR WA_ZFIT0114.
        MOVE-CORRESPONDING <SAIDA> TO WA_ZFIT0114.
        MODIFY ZFIT0114 FROM WA_ZFIT0114.

        MESSAGE S836(SD) WITH TEXT-001.

      ELSEIF ( WL_ZIB_ERRO IS NOT INITIAL ).
        MESSAGE S836(SD) WITH TEXT-002.
      ENDIF.

    ENDLOOP.

    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.

  ENDMETHOD.                    "REFRESH_DOC_CONTABIL

  METHOD Z_RETORNA_STATUS_ZIB.

    DATA V_OBJKEY    TYPE CHAR20.
    CLEAR: E_ZIBCHV, E_ZIBERR.

    CONCATENATE 'ZGL17' I_DOC_LCTO I_ANO_LCTO INTO V_OBJKEY.

    SELECT SINGLE *
      FROM ZIB_CONTABIL_CHV
      INTO E_ZIBCHV
     WHERE OBJ_KEY = V_OBJKEY.

    IF ( SY-SUBRC IS NOT INITIAL ).
      SELECT SINGLE *
        FROM ZIB_CONTABIL_ERR
        INTO E_ZIBERR
       WHERE OBJ_KEY = V_OBJKEY.
    ENDIF.

  ENDMETHOD.                    "z_retorna_status_zib

  METHOD ESTORNO_DOC_CONTABIL.

    CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.
    UNASSIGN <SAIDA>.

    DATA: IT_MSG TYPE TABLE OF BDCMSGCOLL, " WITH HEADER LINE,
          WA_MSG TYPE BDCMSGCOLL.

    CALL METHOD WA_ALV->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SEL_ROWS.

    CHECK NOT IT_SEL_ROWS IS INITIAL.

    IF LINES( IT_SEL_ROWS ) EQ 1.
      READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.
      READ TABLE IT_SAIDA ASSIGNING <SAIDA> INDEX WA_SEL_ROWS-INDEX.

      " Documento contabil
      IF <SAIDA>-DOC_CONTABIL IS NOT INITIAL .

        FREE: IT_DTA.
        DEFINE SHDB.
          CLEAR IT_DTA.
          WA_DTA-PROGRAM   = &1.
          WA_DTA-DYNPRO    = &2.
          WA_DTA-DYNBEGIN  = &3.
          WA_DTA-FNAM      = &4.
          WA_DTA-FVAL      = &5.
          APPEND WA_DTA TO IT_DTA.
        END-OF-DEFINITION.

        SHDB:
        'SAPMF05A' '0105' 'X'  ' '           ' ',
        ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
        ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
        ' '        ' '    ' '  'RF05A-BELNS' <SAIDA>-DOC_CONTABIL,
        ' '        ' '    ' '  'BKPF-BUKRS'  <SAIDA>-BUKRS,
        ' '        ' '    ' '  'RF05A-GJAHS' <SAIDA>-DT_VENC(4),
        ' '        ' '    ' '  'UF05A-STGRD' '01'.

        OPT-DISMODE = 'E'.
        CALL TRANSACTION 'FB08' USING IT_DTA OPTIONS FROM OPT.

        IF SY-SUBRC IS INITIAL.

          MOVE: ABAP_FALSE TO <SAIDA>-LOTE,
                ABAP_FALSE TO <SAIDA>-DOC_LCTO,
                ABAP_FALSE TO <SAIDA>-DOC_CONTABIL.

          CLEAR WA_ZFIT0114.

          MOVE-CORRESPONDING <SAIDA> TO WA_ZFIT0114.

          MODIFY ZFIT0114 FROM WA_ZFIT0114.

        ENDIF.

      ENDIF.

    ELSE.
      MESSAGE S836(SD) WITH TEXT-008.
    ENDIF.
    OBG_CONTROL->REFRESH_DADOS( ).
    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.
  ENDMETHOD.                    "ESTORNO_DOC_CONTABIL




ENDCLASS.               "ZCL_CONTROL_ZFIR0069
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXE'.
      FREE:  IT_SAIDA.
      OBG_CONTROL->CHECK_CAMPOS( ).
      OBG_CONTROL->SELECIONA_DADOS( ).
    WHEN 'SAVE'.
      OBG_CONTROL->SALVAR_DADOS( ).
    WHEN 'BT_ALV'.

      IF X_SCREEN EQ '0101'.
        X_SCREEN = '0103'.
      ELSE.
        X_SCREEN = '0101'.
      ENDIF.

  ENDCASE.
  "

ENDMODULE.
