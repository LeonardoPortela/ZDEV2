

*&---------------------------------------------------------------------*
*& Report  ZIM04
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZIM04.

************************************************************************
* Tabelas de investimentos e Detalhes
************************************************************************
TYPES: BEGIN OF TY_INVESTIMENTO,
         MARK(1),
         COD_GPO2    TYPE ZIM01_SOL_AP_INV-COD_GPO,
         DES_GPO(15),
         DESC_EMP    TYPE NAME1,
         DESC_EMP2   TYPE NAME1,
         DESC_FIL    TYPE NAME1,
         DESC_FASE   TYPE NAME1,
         DESC_CUST   TYPE NAME1,
         VALOR_USD   TYPE ZVALOR,
         COR         TYPE LVC_T_SCOL.
         INCLUDE     TYPE ZIM01_SOL_AP_INV.
         TYPES COLOR(4)    TYPE C.
TYPES LINE_COLOR(4) TYPE C. "Used to store row color attributes
TYPES END OF TY_INVESTIMENTO.

TYPES: BEGIN OF TY_DETALHES,
         DESC_FIL(30),
         DESC_CUSTO(30),
         DESC_FASE(30),
         DESC_FINAL(30),
         DESC_CATEG(30),
         DESC_CLASS(30),
         VLR_TOTAL_USD  TYPE ZVLR_TOTAL.
         INCLUDE TYPE ZIM01_SOL_AP_INV.
       TYPES END OF TY_DETALHES.

TYPES: BEGIN OF TY_TELA2,
         EMPRESA(40),
         CENTRO_CUSTO(40),
         USUARIO(25),
         VLR_TOTAL        TYPE ZVALOR,
         LINHA            TYPE I,
       END OF TY_TELA2,

       BEGIN OF TY_LOTE,
         LOTE TYPE ZIM01_SOL_AP_INV-LOTE,
       END OF TY_LOTE.

DATA: MANAGER        TYPE REF TO CL_GOS_MANAGER.

DATA: LT_COLTAB TYPE LVC_T_SCOL,
      LS_COL    TYPE LVC_S_SCOL.
************************************************************************
* tabelas internas e estruturas
************************************************************************
DATA: IT_ZIM01   TYPE TABLE OF ZIM01_SOL_AP_INV,
      WA_ZIM01   TYPE ZIM01_SOL_AP_INV,
      WA_APROVAR TYPE ZIM01_SOL_AP_INV,
      IT_INVES   TYPE TABLE OF TY_INVESTIMENTO,
      WA_INVES   TYPE TY_INVESTIMENTO,
      IT_DOCS    TYPE TABLE OF TY_DETALHES,
      WA_DOCS    TYPE TY_DETALHES,
      IT_ESTRA   TYPE TABLE OF ZFI_ESTRATEGIA_ZIM,
      WA_ESTRA   TYPE ZFI_ESTRATEGIA_ZIM,
      TG_ESTRA   TYPE TABLE OF ZFI_ESTRATEGIA_ZIM,
      WG_ESTRA   TYPE ZFI_ESTRATEGIA_ZIM,
      TG_DOCS    TYPE TABLE OF TY_DETALHES,
      WG_DOCS    TYPE          TY_DETALHES,
      IT_LOTE    TYPE TABLE OF TY_LOTE WITH HEADER LINE,
      IT_001     TYPE TABLE OF T001,
      WA_001     TYPE T001,
      WA_TELA2   TYPE TY_TELA2,
      IT_CSKT    TYPE TABLE OF CSKT,
      WA_CSKT    TYPE CSKT,
      P_INDEX    TYPE SY-TABIX,
      INICIO     TYPE C.

DATA: IT_FCAT     TYPE LVC_T_FCAT,
      WA_FCAT     TYPE LVC_S_FCAT,
      I_SORT      TYPE LVC_T_SORT,
      WA_CONT     TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV      TYPE REF TO CL_GUI_ALV_GRID,
      IT_FCAT2    TYPE LVC_T_FCAT,
      WA_FCAT2    TYPE LVC_S_FCAT,
      WA_CONT2    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV2     TYPE REF TO CL_GUI_ALV_GRID,
      IT_FCAT3    TYPE LVC_T_FCAT,
      WA_FCAT3    TYPE LVC_S_FCAT,
      WA_CONT3    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV3     TYPE REF TO CL_GUI_ALV_GRID,
      WA_LAYOUT   TYPE LVC_S_LAYO,
      WA_STABLE   TYPE LVC_S_STBL,
      WA_VARIANTE TYPE DISVARIANT,
      TL_FUNCTION TYPE UI_FUNCTIONS,
      WL_FUNCTION LIKE TL_FUNCTION WITH HEADER LINE.

DATA LINE TYPE SY-TABIX.

DATA: V_MSG       TYPE CHAR50,
      BTN_REJ(30),
      T_LOTES     TYPE TABLE OF ZFI_GRU_INV,
      W_LOTES     TYPE          ZFI_GRU_INV,
      T_ESTRA     TYPE TABLE OF ZFI_ESTRATEGIA_ZIM,
      W_ESTRA     TYPE          ZFI_ESTRATEGIA_ZIM,
      T_DOCS      TYPE TABLE OF ZIM01_SOL_AP_INV,
      W_DOCS      TYPE          ZIM01_SOL_AP_INV,
      VDATA(10),
      VG_CANCEL(1),
      TABIX       TYPE SY-TABIX.

CLASS LCL_EVENT_HANDLER DEFINITION DEFERRED.
DATA: WA_EVENT TYPE REF TO LCL_EVENT_HANDLER.
DATA: TL_INDEX_ROWS          TYPE LVC_T_ROW,
      WL_INDEX_ROWS          TYPE LVC_S_ROW,
      VSEQ(10)               TYPE P,
      GF_AUTHORIZATION_FT_09 TYPE C. "Workflow de Documentos
************************************************************************
* Classes
************************************************************************
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    METHODS:
      ON_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN.
    METHODS:
      ON_CLICKE FOR EVENT HOTSPOT_CLICK  OF CL_GUI_ALV_GRID IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
ENDCLASS.

CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
  METHOD ON_CLICK.

    FIELD-SYMBOLS <WA_INVES> TYPE TY_INVESTIMENTO.

    FREE: INICIO, WA_APROVAR, LINE.

    CHECK E_ROW GT 0.

    LINE = E_ROW.

    LOOP AT IT_INVES ASSIGNING <WA_INVES>.
      <WA_INVES>-COLOR = ' '.
    ENDLOOP.

    READ TABLE IT_INVES ASSIGNING <WA_INVES> INDEX E_ROW.

    MOVE-CORRESPONDING <WA_INVES> TO WA_APROVAR.

    WA_TELA2-EMPRESA      = <WA_INVES>-DESC_EMP.
    WA_TELA2-CENTRO_CUSTO = <WA_INVES>-DESC_CUST.
    WA_TELA2-USUARIO      = SY-UNAME.
    WA_TELA2-VLR_TOTAL    = <WA_INVES>-VLR_TOTAL.
    WA_TELA2-LINHA        = E_ROW.
    <WA_INVES>-COLOR      = 'C500'.

    REFRESH TG_ESTRA.
    REFRESH TG_DOCS.
    IF <WA_INVES>-COD_GPO NE 3.
      LOOP AT IT_ESTRA INTO WA_ESTRA WHERE LOTE    = <WA_INVES>-LOTE
                                     AND   KOSTL   = <WA_INVES>-KOSTL
                                     AND   ANO     = <WA_INVES>-ANO
                                     AND   SAFRA   = <WA_INVES>-SAFRA
                                     AND   SAFRA2  = <WA_INVES>-SAFRA2
                                     AND   BUZEI   = <WA_INVES>-BUZEI
                                     AND   ( FASE    = <WA_INVES>-FASE OR FASE = 99 )
                                     AND   GPO_CMP = 0.
        APPEND WA_ESTRA TO TG_ESTRA.
      ENDLOOP.
      LOOP AT IT_DOCS INTO WA_DOCS WHERE LOTE  = <WA_INVES>-LOTE
                                   AND   KOSTL = <WA_INVES>-KOSTL
                                   AND   ANO     = <WA_INVES>-ANO
                                   AND   SAFRA   = <WA_INVES>-SAFRA
                                   AND   SAFRA2  = <WA_INVES>-SAFRA2
                                   AND   BUZEI   = <WA_INVES>-BUZEI
                                   AND   ( FASE    = <WA_INVES>-FASE OR FASE = 99 ).
        IF WA_DOCS-COD_GPO NE 3  OR WA_DOCS-STATUS_APROV = 9.
          APPEND WA_DOCS TO TG_DOCS.
        ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT IT_ESTRA INTO WA_ESTRA WHERE LOTE  = <WA_INVES>-LOTE
                                     AND   BUKRS = <WA_INVES>-BUKRS
                                     AND   KOSTL = <WA_INVES>-KOSTL
                                     AND   ANO   = <WA_INVES>-ANO
                                     AND   BUZEI = <WA_INVES>-BUZEI
                                     AND   FASE  = <WA_INVES>-FASE
                                     AND   GPO_CMP = 3.
        APPEND WA_ESTRA TO TG_ESTRA.
      ENDLOOP.
      LOOP AT IT_DOCS INTO WA_DOCS WHERE LOTE3 = <WA_INVES>-LOTE
                                   AND   BUKRS = <WA_INVES>-BUKRS
                                   AND   ANO   = <WA_INVES>-ANO
                                   AND   KOSTL = <WA_INVES>-KOSTL
                                   AND   BUZEI = <WA_INVES>-BUZEI
                                   AND   FASE  = <WA_INVES>-FASE.
        IF WA_DOCS-COD_GPO EQ 3.
          APPEND WA_DOCS TO TG_DOCS.
        ENDIF.
      ENDLOOP.
    ENDIF.
    SORT TG_ESTRA BY NIVEL DESCENDING.
    "
    CALL SCREEN 0100.

  ENDMETHOD.

  METHOD ON_CLICKE.
    DATA:  V_MSG    TYPE CHAR50.
    REFRESH: T_LOTES, T_ESTRA.

    IF E_ROW_ID GT 0.
      READ TABLE TG_ESTRA INTO WG_ESTRA INDEX E_ROW_ID.

      CLEAR W_LOTES.
      READ TABLE IT_INVES INTO WA_INVES INDEX WA_TELA2-LINHA.
      MOVE-CORRESPONDING WA_INVES TO W_LOTES.
      W_LOTES-EMPRESA   = WA_INVES-DESC_EMP.
      W_LOTES-ANO       = WA_INVES-ANO.
      W_LOTES-WERKS     = WA_INVES-GSBER.
      W_LOTES-FASE      = WA_INVES-FASE.
      W_LOTES-LOTE      = WA_INVES-LOTE.
      W_LOTES-GPO_CMP   = WA_INVES-COD_GPO.
      W_LOTES-TOTAL     = WA_INVES-VLR_TOTAL.
      W_LOTES-TOTAL_USD = WA_INVES-VALOR_USD.
      W_LOTES-GPO_CMP   = WA_INVES-COD_GPO.
      APPEND W_LOTES  TO T_LOTES.

      LOOP AT IT_ESTRA INTO WA_ESTRA WHERE LOTE   = WG_ESTRA-LOTE
                                     AND   KOSTL  = WG_ESTRA-KOSTL
                                     AND   ANO    = WG_ESTRA-ANO
                                     AND   SAFRA  = WG_ESTRA-SAFRA
                                     AND   SAFRA2 = WG_ESTRA-SAFRA2
                                     AND   BUZEI  = WG_ESTRA-BUZEI
                                     AND   ( FASE   = WA_INVES-FASE OR FASE = 99 )
                                     AND   GPO_CMP = WG_ESTRA-GPO_CMP.
        MOVE-CORRESPONDING WA_ESTRA TO W_ESTRA.
        W_ESTRA-NIVELC = WG_ESTRA-NIVEL.
        APPEND W_ESTRA TO T_ESTRA.
      ENDLOOP.

      CALL FUNCTION 'Z_IM_ESTRATEGIA_EXECUTAR'
        EXPORTING
          V_USUARIO = SY-UNAME
        IMPORTING
          MSG       = V_MSG
        TABLES
          T_LOTES   = T_LOTES
          T_ESTRA   = T_ESTRA.

      READ TABLE T_LOTES INTO W_LOTES INDEX 1.
      IF W_LOTES-LOTE IS NOT INITIAL.
        READ TABLE IT_INVES INTO WA_INVES INDEX WA_TELA2-LINHA.
        WA_INVES-LOTE = W_LOTES-LOTE.
        MODIFY IT_INVES FROM WA_INVES INDEX WA_TELA2-LINHA TRANSPORTING LOTE.
      ENDIF.
      LOOP AT T_ESTRA INTO W_ESTRA
        WHERE APROVADOR EQ SY-UNAME
        AND   GPO_CMP = W_LOTES-GPO_CMP.

        READ TABLE TG_ESTRA INTO WA_ESTRA WITH KEY NIVEL = W_ESTRA-NIVELC.
        CHECK SY-SUBRC = 0.
        MOVE: W_ESTRA-OPCOES TO WA_ESTRA-OPCOES,
              W_ESTRA-ESTADO TO WA_ESTRA-ESTADO.
        MODIFY TG_ESTRA FROM WA_ESTRA INDEX SY-TABIX TRANSPORTING OPCOES ESTADO.
        READ TABLE IT_ESTRA INTO WA_ESTRA WITH KEY KOSTL      = W_ESTRA-KOSTL
                                                   ANO        = W_ESTRA-ANO
                                                   SAFRA      = W_ESTRA-SAFRA
                                                   SAFRA2     = W_ESTRA-SAFRA2
                                                   BUZEI      = W_ESTRA-BUZEI
                                                   FASE       = W_ESTRA-FASE
                                                   APROVADOR  = W_ESTRA-APROVADOR
                                                   NIVEL      = W_ESTRA-NIVEL
                                                   GPO_CMP    = W_ESTRA-GPO_CMP.
        MOVE: W_ESTRA-OPCOES TO WA_ESTRA-OPCOES,
              W_ESTRA-ESTADO TO WA_ESTRA-ESTADO.
        MODIFY IT_ESTRA FROM WA_ESTRA INDEX SY-TABIX TRANSPORTING OPCOES ESTADO.

      ENDLOOP.


      MESSAGE S836(SD) DISPLAY LIKE 'W' WITH V_MSG .

*      REFRESH: IT_INVES,TG_ESTRA,TG_DOCS.
      REFRESH: IT_INVES,TG_DOCS.
      PERFORM SELECIONA_DADOS.
      PERFORM AGRUPA_DADOS.


      CALL METHOD WA_ALV3->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.

      CALL METHOD WA_ALV2->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.

      CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.

    ENDIF.
  ENDMETHOD.
ENDCLASS.


************************************************************************
* start-of-selection
************************************************************************
START-OF-SELECTION.
  INICIO = ABAP_TRUE.
  CALL SCREEN 0100.

END-OF-SELECTION.

FORM SELECIONA_DADOS.
  REFRESH: T_LOTES,T_ESTRA,T_DOCS.
  CLEAR VG_CANCEL.
  CALL FUNCTION 'Z_IM_ESTRATEGIA_LISTA'
    EXPORTING
      V_USUARIO = SY-UNAME
    IMPORTING
      MSG       = V_MSG
    TABLES
      T_LOTES   = T_LOTES
      T_ESTRA   = T_ESTRA
      T_DOCS    = T_DOCS.

ENDFORM.

FORM AGRUPA_DADOS.
  FREE: IT_INVES.
  DATA: DESC_CUST TYPE CSKT-LTEXT.

  REFRESH: IT_INVES,IT_ESTRA, IT_DOCS.
  LOOP AT T_LOTES INTO W_LOTES.
    P_INDEX = SY-TABIX.
    WA_INVES-LOTE      = W_LOTES-LOTE.
    WA_INVES-LOTE_ANEX = W_LOTES-LOTE_ANEX.
    WA_INVES-DESC_EMP  = W_LOTES-EMPRESA.
    WA_INVES-DESC_EMP2 = W_LOTES-EMPRESA+5(25).
    CASE W_LOTES-FASE.
      WHEN 1. WA_INVES-DESC_FASE = 'PLANEJADO'.
      WHEN 2. WA_INVES-DESC_FASE = 'EXTRA'.
      WHEN 3. WA_INVES-DESC_FASE = 'COMPLEMENTO ORÇADO'.
    ENDCASE.

    SELECT SINGLE LTEXT
      FROM CSKT
            INTO DESC_CUST
      WHERE KOSTL = W_LOTES-KOSTL.

    "
    WA_INVES-BUKRS     = W_LOTES-EMPRESA+0(4).
    WA_INVES-ANO       = W_LOTES-ANO.
    WA_INVES-GSBER     = W_LOTES-WERKS.
    "
    SELECT SINGLE NAME1
      INTO WA_INVES-DESC_FIL
      FROM T001W
      WHERE WERKS = W_LOTES-WERKS.
    "
    CONCATENATE W_LOTES-WERKS '-' WA_INVES-DESC_FIL INTO WA_INVES-DESC_FIL.
    WA_INVES-SAFRA     = W_LOTES-SAFRA.
    WA_INVES-SAFRA2    = W_LOTES-SAFRA2.
    WA_INVES-BUZEI     = W_LOTES-BUZEI.
    WA_INVES-FASE      = W_LOTES-FASE.
    WA_INVES-KOSTL     = W_LOTES-KOSTL.
    WA_INVES-DESC_CUST = DESC_CUST.
    WA_INVES-VLR_TOTAL = W_LOTES-TOTAL.
*---> 10/06/2023 - Migração S4 - JS
*        WA_INVES-VALOR_USD = W_LOTES-TOTAL_USD.
       WA_INVES-VALOR_USD = CONV #( W_LOTES-TOTAL_USD ).
*<--- 10/06/2023 - Migração S4 - JS
    WA_INVES-COD_GPO   = W_LOTES-GPO_CMP.
    WA_INVES-COD_GPO2  = W_LOTES-COD_GPO.
    WA_INVES-OBJETIVO	 = W_LOTES-OBJETIVO.
    WA_INVES-DESCR_ITEM	= W_LOTES-DESCR_ITEM.
    "WA_INVES-MENGE
    IF W_LOTES-GPO_CMP = 3 .
      WA_INVES-DES_GPO = 'Informática'.
    ENDIF.
    SELECT SINGLE DESCR_GRUPO FROM ZIM010 INTO WA_INVES-DES_GPO
     WHERE COD_GPO = WA_INVES-COD_GPO2.

*    IF WA_APROVAR IS INITIAL.
*      WA_INVES-COLOR = ''.
*    ELSE.
*      IF LINE EQ P_INDEX.
*        WA_INVES-COLOR = 'C500'.
*      ELSE.
*        WA_INVES-COLOR = ''.
*      ENDIF.
*    ENDIF.
    CLEAR WA_INVES-LINE_COLOR.
    IF WA_INVES-FASE NE '01'.
*      LS_COL-FNAME     = 'DESC_FASE'.
*      LS_COL-COLOR-COL = '5'.
*      APPEND LS_COL TO LT_COLTAB.
*      WA_INVES-COR = LT_COLTAB.
*      CLEAR LS_COL.
*      REFRESH LT_COLTAB.
      WA_INVES-LINE_COLOR = 'C510'.
    ENDIF.


    APPEND WA_INVES TO IT_INVES.
    FREE: WA_INVES.

  ENDLOOP.

  LOOP AT T_ESTRA INTO W_ESTRA.
    MOVE-CORRESPONDING W_ESTRA TO WA_ESTRA.
    APPEND WA_ESTRA TO IT_ESTRA.
  ENDLOOP.
  "
  SORT IT_ESTRA BY LOTE KOSTL FASE NIVEL.

  LOOP AT T_DOCS INTO W_DOCS.
    MOVE-CORRESPONDING W_DOCS TO WA_DOCS.
    IF W_DOCS-MOEDA = 'BRL' AND WA_DOCS-TX_USD GT 0.
      WA_DOCS-VLR_TOTAL_USD = WA_DOCS-VLR_TOTAL / WA_DOCS-TX_USD.
    ELSE.
      WA_DOCS-VLR_TOTAL_USD = WA_DOCS-VLR_TOTAL.
    ENDIF.
    SELECT SINGLE NAME1 INTO WA_DOCS-DESC_FIL   FROM T001W WHERE WERKS = WA_DOCS-GSBER.
    SELECT SINGLE KTEXT INTO WA_DOCS-DESC_CUSTO FROM CSKT  WHERE SPRAS = SY-LANGU AND KOSTL = WA_DOCS-KOSTL.
    CASE WA_DOCS-FINALIDADE.
      WHEN '01'. WA_DOCS-DESC_FINAL   = '01-Meio ambiente'.
      WHEN '02'. WA_DOCS-DESC_FINAL   = '02-Obrigação Legal'.
      WHEN '03'. WA_DOCS-DESC_FINAL   = '03-Sinistro'.
      WHEN '04'. WA_DOCS-DESC_FINAL   = '04-Reserva'.
      WHEN '05'. WA_DOCS-DESC_FINAL   = '05-Outros'.
    ENDCASE.
    CASE WA_DOCS-IZWEK.
      WHEN '10'. WA_DOCS-DESC_CATEG   = '10-Substituição'.
      WHEN '20'. WA_DOCS-DESC_CATEG   = '20-Expansão'.
      WHEN '50'. WA_DOCS-DESC_CATEG   = '50-Novas Aquisições'.
    ENDCASE.
    SELECT SINGLE KNTTX INTO  WA_DOCS-DESC_CLASS  FROM T163I WHERE SPRAS = SY-LANGU AND KNTTP = WA_DOCS-KNTTP.
    CONCATENATE WA_DOCS-KNTTP '-' WA_DOCS-DESC_CLASS INTO WA_DOCS-DESC_CLASS.
    CASE WA_DOCS-FASE.
      WHEN 1. WA_DOCS-DESC_FASE = '01-Planejado'.
      WHEN 2. WA_DOCS-DESC_FASE = '02-Extra'.
      WHEN 3. WA_DOCS-DESC_FASE = '03-Complemento Orçado'.
    ENDCASE.

    APPEND WA_DOCS TO IT_DOCS.
  ENDLOOP.



  LOOP AT  IT_INVES INTO WA_INVES.
    LOOP AT IT_DOCS INTO WA_DOCS WHERE    LOTE    = WA_INVES-LOTE
                                    AND   KOSTL   = WA_INVES-KOSTL
                                    AND   ANO     = WA_INVES-ANO
                                    AND   SAFRA   = WA_INVES-SAFRA
                                    AND   SAFRA2  = WA_INVES-SAFRA2
                                    AND   BUZEI   = WA_INVES-BUZEI
                                    AND   FASE    = WA_INVES-FASE.
      WA_INVES-MENGE = WA_DOCS-MENGE.
    ENDLOOP.
    MODIFY  IT_INVES  FROM WA_INVES INDEX SY-TABIX.
    CLEAR WA_INVES.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TI0100'.

  IF WA_CONT IS INITIAL.
    PERFORM SELECIONA_DADOS.
    PERFORM AGRUPA_DADOS.
  ENDIF.
  PERFORM MONTA_ALV USING '1'.
  PERFORM CRIA_ALV.
  PERFORM CRIA_ALV2.
  PERFORM CRIA_ALV3.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_0100 INPUT.
  DATA: W_LINHAS    TYPE I,
        VLOTE_ANEX  TYPE ZIM01_SOL_AP_INV-LOTE_ANEX,
        W_MENSA(50),
        V_LOTE      TYPE  ZIM01_SOL_AP_INV-LOTE,
        V_KOSTL     TYPE  ZIM01_SOL_AP_INV-KOSTL,
        V_ANO       TYPE  ZIM01_SOL_AP_INV-ANO,

        W_ANSWER(1).

  DATA: TL_FIELDS TYPE TABLE OF SVAL WITH HEADER LINE,
        LV_RETURN TYPE ZIM01_SOL_AP_INV-LOTE.

  CASE SY-UCOMM.
    WHEN 'CANCELAR'.
      CLEAR: TL_FIELDS, TL_FIELDS[].
      TL_FIELDS-TABNAME    = 'ZPFE_LOTE_ITEM'.
      TL_FIELDS-FIELDNAME  = 'NM_LOTE'.
      TL_FIELDS-FIELD_OBL  = 'X'.
      TL_FIELDS-FIELD_ATTR = '01'.
      APPEND TL_FIELDS.
      "
      TL_FIELDS-TABNAME    = 'CSKS'.
      TL_FIELDS-FIELDNAME  = 'KOSTL'.
      TL_FIELDS-FIELD_OBL  = 'X'.
      TL_FIELDS-FIELD_ATTR = '01'.
      APPEND TL_FIELDS.

      TL_FIELDS-TABNAME    = 'BKPF'.
      TL_FIELDS-FIELDNAME  = 'GJAHR'.
      TL_FIELDS-FIELD_OBL  = 'X'.
      TL_FIELDS-FIELD_ATTR = '01'.
      APPEND TL_FIELDS.

      CALL FUNCTION 'POPUP_GET_VALUES'
        EXPORTING
          POPUP_TITLE     = 'Informe o Lote/Custo/Ano para reprovar'
        IMPORTING
          RETURNCODE      = LV_RETURN
        TABLES
          FIELDS          = TL_FIELDS
        EXCEPTIONS
          ERROR_IN_FIELDS = 1
          OTHERS          = 2.

      CLEAR: V_LOTE, V_KOSTL, V_ANO.
      LOOP AT TL_FIELDS.
        IF TL_FIELDS-FIELDNAME = 'NM_LOTE'.
          V_LOTE = TL_FIELDS-VALUE+0(10).
        ENDIF.
        IF TL_FIELDS-FIELDNAME = 'KOSTL'.
          V_KOSTL = TL_FIELDS-VALUE+0(10).
        ENDIF.
        IF TL_FIELDS-FIELDNAME = 'NM_ANO'.
          V_ANO = TL_FIELDS-VALUE+0(4).
        ENDIF.
      ENDLOOP.

      IF V_LOTE IS NOT INITIAL AND V_ANO IS NOT INITIAL AND V_KOSTL IS NOT INITIAL.
        REFRESH: IT_INVES,TG_ESTRA,TG_DOCS.
        REFRESH: T_LOTES,T_ESTRA,T_DOCS.
        CALL FUNCTION 'Z_IM_ESTRATEGIA_LISTA'
          EXPORTING
            V_USUARIO = SY-UNAME
            V_LOTE    = V_LOTE
            V_KOSTL   = V_KOSTL
            V_ANO     = V_ANO
            V_BUZEI   = 999
          IMPORTING
            MSG       = V_MSG
          TABLES
            T_LOTES   = T_LOTES
            T_ESTRA   = T_ESTRA
            T_DOCS    = T_DOCS.
        IF T_ESTRA[] IS NOT INITIAL.
          PERFORM AGRUPA_DADOS.
        ELSE.
          MESSAGE 'Não encontrado lote aprovado por você!' TYPE 'I'.
        ENDIF.
      ENDIF.


    WHEN 'REJ'.
      LOOP AT TG_ESTRA INTO WA_ESTRA WHERE  APROVADOR = SY-UNAME.

        BTN_REJ = TEXT-B01.
        IF  WA_ESTRA-OPCOES = ICON_REJECT.
          WA_ESTRA-OPCOES = ICON_SET_STATE.
        ELSEIF  WA_ESTRA-OPCOES = ICON_SET_STATE.
          WA_ESTRA-OPCOES = ICON_REJECT.
        ENDIF.
        MODIFY TG_ESTRA FROM WA_ESTRA INDEX SY-TABIX TRANSPORTING OPCOES.
        "
        READ TABLE IT_ESTRA INTO W_ESTRA  WITH KEY KOSTL      = WA_ESTRA-KOSTL
                                                   ANO        = WA_ESTRA-ANO
                                                   SAFRA      = WA_ESTRA-SAFRA
                                                   SAFRA2     = WA_ESTRA-SAFRA2
                                                   BUZEI      = WA_ESTRA-BUZEI
                                                   FASE       = WA_ESTRA-FASE
                                                   APROVADOR  = WA_ESTRA-APROVADOR
                                                   NIVEL      = WA_ESTRA-NIVEL
                                                   GPO_CMP    = WA_ESTRA-GPO_CMP.
        IF SY-SUBRC = 0.
          MOVE: WA_ESTRA-OPCOES TO W_ESTRA-OPCOES,
                WA_ESTRA-ESTADO TO W_ESTRA-ESTADO.
          MODIFY IT_ESTRA FROM W_ESTRA INDEX SY-TABIX TRANSPORTING OPCOES.
        ENDIF.
      ENDLOOP.
    WHEN 'DESAPROVAR'.
      CALL METHOD WA_ALV->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = TL_INDEX_ROWS.

      W_LINHAS = LINES( TL_INDEX_ROWS ).
      IF W_LINHAS LT 2.
        MESSAGE 'Selecione ao menos 2 linhas para aprovar!' TYPE 'I'.
        EXIT.
      ENDIF.
      LOOP AT TL_INDEX_ROWS INTO WL_INDEX_ROWS.
        READ TABLE IT_INVES INTO WA_INVES INDEX WL_INDEX_ROWS-INDEX.
        "
        IF WA_INVES-COD_GPO EQ 3.
          W_LINHAS = 98.
        ENDIF.
      ENDLOOP.
      IF W_LINHAS EQ 98.
        MESSAGE 'Avaliação não permite desaprovação em Massa!' TYPE 'I'.
        EXIT.
      ENDIF.
      "
      CONCATENATE 'Confirma a desaprovação em Massa?' W_MENSA INTO W_MENSA SEPARATED BY SPACE.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TEXT_QUESTION         = W_MENSA
          TEXT_BUTTON_1         = 'Sim'(100)
          ICON_BUTTON_1         = 'ICON_OKAY '
          TEXT_BUTTON_2         = 'Não'(101)
          ICON_BUTTON_2         = 'ICON_CANCEL'
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ' '
          START_COLUMN          = 25
          START_ROW             = 6
        IMPORTING
          ANSWER                = W_ANSWER
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.

      IF W_ANSWER = '2'. "não
        EXIT.
      ENDIF.
      "
      LOOP AT TL_INDEX_ROWS INTO WL_INDEX_ROWS.
        READ TABLE IT_INVES INTO WA_INVES INDEX WL_INDEX_ROWS-INDEX.
        "
        IF WA_INVES-COD_GPO EQ 3.
          CONTINUE.
        ENDIF.

        REFRESH: T_LOTES, T_ESTRA.
        MOVE-CORRESPONDING WA_INVES TO W_LOTES.
        W_LOTES-EMPRESA   = WA_INVES-DESC_EMP.
        W_LOTES-WERKS     = WA_INVES-GSBER.
        W_LOTES-LOTE      = WA_INVES-LOTE.
        W_LOTES-ANO       = WA_INVES-ANO.
        W_LOTES-GPO_CMP   = WA_INVES-COD_GPO.
        W_LOTES-TOTAL     = WA_INVES-VLR_TOTAL.
        W_LOTES-TOTAL_USD = WA_INVES-VALOR_USD.
        W_LOTES-GPO_CMP   = WA_INVES-COD_GPO.
        APPEND W_LOTES  TO T_LOTES.

        LOOP AT IT_ESTRA INTO WA_ESTRA WHERE LOTE  = WA_INVES-LOTE
                                     AND   KOSTL   = WA_INVES-KOSTL
                                     AND   ANO     = WA_INVES-ANO
                                     AND   SAFRA   = WA_INVES-SAFRA
                                     AND   SAFRA2  = WA_INVES-SAFRA2
                                     AND   BUZEI   = WA_INVES-BUZEI
                                     AND   ( FASE  = WA_INVES-FASE OR FASE = 99 )
                                     AND   GPO_CMP = WA_INVES-COD_GPO.

          MOVE-CORRESPONDING WA_ESTRA TO W_ESTRA.
          IF  W_ESTRA-OPCOES = ICON_SET_STATE.
            W_ESTRA-OPCOES = ICON_REJECT.
          ENDIF.
          CLEAR W_ESTRA-NIVELC.
          APPEND W_ESTRA TO T_ESTRA.
        ENDLOOP.
        CALL FUNCTION 'Z_IM_ESTRATEGIA_EXECUTAR'
          EXPORTING
            V_USUARIO = SY-UNAME
          IMPORTING
            MSG       = V_MSG
          TABLES
            T_LOTES   = T_LOTES
            T_ESTRA   = T_ESTRA.

        READ TABLE T_LOTES INTO W_LOTES INDEX 1.

        WA_INVES-LOTE = W_LOTES-LOTE.
        MODIFY IT_INVES FROM WA_INVES INDEX WL_INDEX_ROWS-INDEX TRANSPORTING LOTE.

        LOOP AT T_ESTRA INTO W_ESTRA
            WHERE APROVADOR EQ SY-UNAME
            AND   GPO_CMP = W_LOTES-GPO_CMP.

          READ TABLE IT_ESTRA INTO WA_ESTRA WITH KEY KOSTL      = W_ESTRA-KOSTL
                                                     ANO        = W_ESTRA-ANO
                                                     SAFRA      = W_ESTRA-SAFRA
                                                     SAFRA2     = W_ESTRA-SAFRA2
                                                     BUZEI      = W_ESTRA-BUZEI
                                                     FASE       = W_ESTRA-FASE
                                                     APROVADOR  = W_ESTRA-APROVADOR
                                                     NIVEL      = W_ESTRA-NIVEL
                                                     GPO_CMP    = W_ESTRA-GPO_CMP.
          MOVE: W_ESTRA-OPCOES TO WA_ESTRA-OPCOES,
                W_ESTRA-ESTADO TO WA_ESTRA-ESTADO.
          MODIFY IT_ESTRA FROM WA_ESTRA INDEX SY-TABIX TRANSPORTING OPCOES ESTADO.
        ENDLOOP.
        MESSAGE S836(SD) DISPLAY LIKE 'W' WITH 'Fim Desaprovação'.
      ENDLOOP.
    WHEN 'APROVAR'.
      CALL METHOD WA_ALV->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = TL_INDEX_ROWS.

      W_LINHAS = LINES( TL_INDEX_ROWS ).
      IF W_LINHAS LT 2.
        MESSAGE 'Selecione ao menos 2 linhas para aprovar!' TYPE 'I'.
        EXIT.
      ENDIF.
      LOOP AT TL_INDEX_ROWS INTO WL_INDEX_ROWS.
        READ TABLE IT_INVES INTO WA_INVES INDEX WL_INDEX_ROWS-INDEX.
        "
        IF WA_INVES-COD_GPO EQ 3.
          W_LINHAS = 98.
        ENDIF.
      ENDLOOP.
      IF W_LINHAS EQ 98.
        MESSAGE 'Avaliação não permite aprovação em Massa!' TYPE 'I'.
        EXIT.
      ENDIF.
      "
      CONCATENATE 'Confirma a aprovação em Massa?' W_MENSA INTO W_MENSA SEPARATED BY SPACE.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TEXT_QUESTION         = W_MENSA
          TEXT_BUTTON_1         = 'Sim'(100)
          ICON_BUTTON_1         = 'ICON_OKAY '
          TEXT_BUTTON_2         = 'Não'(101)
          ICON_BUTTON_2         = 'ICON_CANCEL'
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ' '
          START_COLUMN          = 25
          START_ROW             = 6
        IMPORTING
          ANSWER                = W_ANSWER
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.

      IF W_ANSWER = '2'. "não
        EXIT.
      ENDIF.
      "
      LOOP AT TL_INDEX_ROWS INTO WL_INDEX_ROWS.
        READ TABLE IT_INVES INTO WA_INVES INDEX WL_INDEX_ROWS-INDEX.
        "
        IF WA_INVES-COD_GPO EQ 3.
          CONTINUE.
        ENDIF.

        REFRESH: T_LOTES, T_ESTRA.
        MOVE-CORRESPONDING WA_INVES TO W_LOTES.
        W_LOTES-EMPRESA   = WA_INVES-DESC_EMP.
        W_LOTES-WERKS     = WA_INVES-GSBER.
        W_LOTES-LOTE      = WA_INVES-LOTE.
        W_LOTES-ANO       = WA_INVES-ANO.
        W_LOTES-GPO_CMP   = WA_INVES-COD_GPO.
        W_LOTES-TOTAL     = WA_INVES-VLR_TOTAL.
        W_LOTES-TOTAL_USD = WA_INVES-VALOR_USD.
        W_LOTES-GPO_CMP   = WA_INVES-COD_GPO.
        APPEND W_LOTES  TO T_LOTES.

        LOOP AT IT_ESTRA INTO WA_ESTRA WHERE LOTE  = WA_INVES-LOTE
                                     AND   KOSTL   = WA_INVES-KOSTL
                                     AND   ANO     = WA_INVES-ANO
                                     AND   SAFRA   = WA_INVES-SAFRA
                                     AND   SAFRA2  = WA_INVES-SAFRA2
                                     AND   BUZEI   = WA_INVES-BUZEI
                                     AND   ( FASE  = WA_INVES-FASE OR FASE = 99 )
                                     AND   GPO_CMP = WA_INVES-COD_GPO.

          MOVE-CORRESPONDING WA_ESTRA TO W_ESTRA.
          IF  W_ESTRA-OPCOES = ICON_REJECT.
            W_ESTRA-OPCOES = ICON_SET_STATE.
          ENDIF.
          CLEAR W_ESTRA-NIVELC.
          APPEND W_ESTRA TO T_ESTRA.
        ENDLOOP.
        CALL FUNCTION 'Z_IM_ESTRATEGIA_EXECUTAR'
          EXPORTING
            V_USUARIO = SY-UNAME
          IMPORTING
            MSG       = V_MSG
          TABLES
            T_LOTES   = T_LOTES
            T_ESTRA   = T_ESTRA.

        READ TABLE T_LOTES INTO W_LOTES INDEX 1.

        WA_INVES-LOTE = W_LOTES-LOTE.
        MODIFY IT_INVES FROM WA_INVES INDEX WL_INDEX_ROWS-INDEX TRANSPORTING LOTE.

        LOOP AT T_ESTRA INTO W_ESTRA
            WHERE APROVADOR EQ SY-UNAME
            AND   GPO_CMP = W_LOTES-GPO_CMP.

          READ TABLE IT_ESTRA INTO WA_ESTRA WITH KEY KOSTL      = W_ESTRA-KOSTL
                                                     ANO        = W_ESTRA-ANO
                                                     SAFRA      = W_ESTRA-SAFRA
                                                     SAFRA2     = W_ESTRA-SAFRA2
                                                     BUZEI      = W_ESTRA-BUZEI
                                                     FASE       = W_ESTRA-FASE
                                                     APROVADOR  = W_ESTRA-APROVADOR
                                                     NIVEL      = W_ESTRA-NIVEL
                                                     GPO_CMP    = W_ESTRA-GPO_CMP.
          MOVE: W_ESTRA-OPCOES TO WA_ESTRA-OPCOES,
                W_ESTRA-ESTADO TO WA_ESTRA-ESTADO.
          MODIFY IT_ESTRA FROM WA_ESTRA INDEX SY-TABIX TRANSPORTING OPCOES ESTADO.
        ENDLOOP.
        MESSAGE S836(SD) DISPLAY LIKE 'W' WITH 'Fim aprovação'.
      ENDLOOP.
    WHEN 'ANEXAR'.
      REFRESH: IT_LOTE.
      CALL METHOD WA_ALV->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = TL_INDEX_ROWS.

      W_LINHAS = LINES( TL_INDEX_ROWS ).
      IF W_LINHAS EQ 0.
        MESSAGE 'Selecione linhas para anexar!' TYPE 'I'.
        EXIT.
      ENDIF.

      CLEAR: VLOTE_ANEX, WA_INVES, W_LINHAS.
      READ TABLE TL_INDEX_ROWS INTO WL_INDEX_ROWS INDEX 1.
      READ TABLE IT_INVES INTO WA_INVES INDEX WL_INDEX_ROWS-INDEX.
      VLOTE_ANEX = WA_INVES-LOTE_ANEX.
      LOOP AT TL_INDEX_ROWS INTO WL_INDEX_ROWS.
        READ TABLE IT_INVES INTO WA_INVES INDEX WL_INDEX_ROWS-INDEX.
        IF VLOTE_ANEX NE WA_INVES-LOTE_ANEX.
          W_LINHAS = 99.
          EXIT.
        ENDIF.
        IF WA_INVES-COD_GPO EQ 3.
          W_LINHAS = 98.
          EXIT.
        ENDIF.
        VLOTE_ANEX = WA_INVES-LOTE_ANEX.
      ENDLOOP.
      IF W_LINHAS EQ 98.
        MESSAGE 'Avaliação não permite anexo!' TYPE 'I'.
        EXIT.
      ENDIF.
      IF W_LINHAS EQ 99.
        MESSAGE 'Lotes de anexo diferentes!' TYPE 'I'.
        EXIT.
      ENDIF.

      W_LINHAS = LINES( TL_INDEX_ROWS ).
      W_MENSA = W_LINHAS.
      CONDENSE W_MENSA NO-GAPS.
      CONCATENATE 'O Anexo sera para as linhas selecionadas' W_MENSA INTO W_MENSA SEPARATED BY SPACE.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TEXT_QUESTION         = W_MENSA
          TEXT_BUTTON_1         = 'Sim'(100)
          ICON_BUTTON_1         = 'ICON_OKAY '
          TEXT_BUTTON_2         = 'Não'(101)
          ICON_BUTTON_2         = 'ICON_CANCEL'
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ' '
          START_COLUMN          = 25
          START_ROW             = 6
        IMPORTING
          ANSWER                = W_ANSWER
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.

      IF W_ANSWER = '2'. "não
        EXIT.
      ENDIF.

      CLEAR VSEQ.
      LOOP AT TL_INDEX_ROWS INTO WL_INDEX_ROWS.
        READ TABLE IT_INVES INTO WA_INVES INDEX WL_INDEX_ROWS-INDEX.
        IF WA_INVES-LOTE_ANEX IS INITIAL AND VSEQ = 0.
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              NR_RANGE_NR = '01'
              OBJECT      = 'ZID_LOTEIV'
            IMPORTING
              NUMBER      = VSEQ.
        ENDIF.
        IF VSEQ = 0.
          VSEQ = WA_INVES-LOTE_ANEX.
        ENDIF.
        WA_INVES-LOTE_ANEX = VSEQ.
        MODIFY IT_INVES FROM WA_INVES INDEX WL_INDEX_ROWS-INDEX TRANSPORTING LOTE_ANEX.
        "Atualiza lote
        UPDATE ZIM01_SOL_AP_INV SET LOTE_ANEX = VSEQ
         WHERE BUKRS  = WA_INVES-BUKRS
         AND   GSBER  = WA_INVES-GSBER
         AND   KOSTL  = WA_INVES-KOSTL
         AND   ANO    = WA_INVES-ANO
         AND   SAFRA  = WA_INVES-SAFRA
         AND   SAFRA2 = WA_INVES-SAFRA2
         AND   BUZEI  = WA_INVES-BUZEI
         AND   FASE   = WA_INVES-FASE
         AND   LOTE_ANEX  = 0
         AND   ( ( STATUS_APROV = ' ' AND COD_GPO NE 3 ) OR STATUS_APROV = '9' ).

        PERFORM HABILITAR_WORKFLOW_DOCUMENTOS.
      ENDLOOP.
      SORT IT_LOTE BY LOTE.
      DELETE ADJACENT DUPLICATES FROM IT_LOTE.
    WHEN 'REFRESH'.
      REFRESH: IT_INVES,TG_ESTRA,TG_DOCS.
      PERFORM SELECIONA_DADOS.
      PERFORM AGRUPA_DADOS.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'APR'.
      PERFORM APROVAR_INVESTIMENTO.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  MONTA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTA_ALV USING P_DIR.

  FREE: IT_FCAT, WA_FCAT.

  CASE P_DIR.

    WHEN '1'.
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
      '' '' ''          'IT_INVES' 'BUKRS'      'Emp.'                  ' '  '05'  ' ',
      '' '' ''          'IT_INVES' 'DESC_EMP2'  'Desc.Empresa'          ' '  '15'  ' ',
      '' '' ''          'IT_INVES' 'ANO'        'Ano'                   ' '  '04'  ' ',
      '' '' ''          'IT_INVES' 'DESC_FIL'   'Filial'                ' '  '15'  ' ',
      '' '' ''          'IT_INVES' 'FASE'       'Fase'                  ' '  '04'  ' ',
      '' '' ''          'IT_INVES' 'DESC_FASE'  'Desc.Fase'             ' '  '08'  ' ',
      '' 'CSKS' 'KOSTL' 'IT_INVES' 'KOSTL'      'Centro de Custo'       ' '  '10'  ' ',
      '' '' ''          'IT_INVES' 'DESC_CUST'  'Desc. CCusto'          ' '  '15'  ' ',
      '' '' ''          'IT_INVES' 'OBJETIVO'   'Objetivo Investimento' ' '  '20'  ' ',
      '' '' ''          'IT_INVES' 'DESCR_ITEM' 'Descrição do Item'     ' '  '20'  ' ',
      '' '' ''          'IT_INVES' 'VLR_TOTAL'  'Total R$'              ' '  '15'  ' ',
      '' '' ''          'IT_INVES' 'VALOR_USD'  'Total US$'             ' '  '15'  ' ',
      '' '' ''          'TG_DOCS'  'MENGE'      'Quantidade'            ' '  '13'  ' ',
      '' '' ''          'IT_INVES' 'COD_GPO2'   'Grupo'                 ' '  '05'  ' ',
      '' '' ''          'IT_INVES' 'DES_GPO'    'Descrição'             ' '  '10'  ' ',
      '' '' ''          'IT_INVES' 'LOTE'       'Lote Aprov'            ' '  '08'  ' ',
      '' '' ''          'IT_INVES' 'LOTE_ANEX'  'Lote Anex.'            ' '  '08'  ' '.

    WHEN '2'.
      DEFINE ALV2.
        WA_FCAT2-HOTSPOT   = &1.
        WA_FCAT2-REF_TABLE = &2.
        WA_FCAT2-REF_FIELD = &3.
        WA_FCAT2-TABNAME   = &4.
        WA_FCAT2-FIELDNAME = &5.
        WA_FCAT2-SCRTEXT_L = &6.
        WA_FCAT2-SCRTEXT_M = &6.
        WA_FCAT2-NO_ZERO   = &7.
        WA_FCAT2-OUTPUTLEN = &8.
        WA_FCAT2-DO_SUM    = &9.

        APPEND WA_FCAT2 TO IT_FCAT2.
        CLEAR WA_FCAT.

      END-OF-DEFINITION.
      ALV2:
*        '' '' '' 'TG_DOCS' 'BUKRS'         'Empresa'                ' '  '06'  ' ',
*        '' '' '' 'TG_DOCS' 'GSBER'         'Filial'                 ' '  '06'  ' ',
*        '' '' '' 'TG_DOCS' 'DESC_FIL'      'Descr.Filial'           ' '  '20'  ' ',
        '' '' '' 'TG_DOCS' 'KOSTL'         'Centro Custo'           ' '  '10'  ' ',
        '' '' '' 'TG_DOCS' 'DESC_CUSTO'    'Desc. C.Custo'          ' '  '20'  ' ',
        '' '' '' 'TG_DOCS' 'OBSERVACOES'   'Obs.Contábil'           ' '  '15'  ' ',
*        '' '' '' 'TG_DOCS' 'OBJETIVO'      'Objetivo Investimento'  ' '  '25'  ' ',
*        '' '' '' 'TG_DOCS' 'DESCR_ITEM'    'Descrição do Item'      ' '  '25'  ' ',
        '' '' '' 'TG_DOCS' 'ANO'           'Ano'                    ' '  '05'  ' ',
        '' '' '' 'TG_DOCS' 'SAFRA'         'Safra'                  ' '  '05'  ' ',
        '' '' '' 'TG_DOCS' 'DESC_FASE'     'Fase'                   ' '  '12'  ' ',
        "'' '' '' 'TG_DOCS' 'MENGE'         'Quantidade'             ' '  '13'  ' ',
        '' '' '' 'TG_DOCS' 'VLR_UNITARIO'  'Vlr.Un.R$'              ' '  '15'  ' ',
        '' '' '' 'TG_DOCS' 'VLR_TOTAL'     'Total R$'               ' '  '15'  'X',
        '' '' '' 'TG_DOCS' 'VLR_TOTAL_USD' 'Total US$'              ' '  '15'  'X',
        '' '' '' 'TG_DOCS' 'DESC_FINAL'    'Finalidade'             ' '  '12'  ' ',
        '' '' '' 'TG_DOCS' 'DESC_CATEG'    'Ctg.Investimento'       ' '  '12'  ' ',
        '' '' '' 'TG_DOCS' 'DESC_CLASS'    'Class.Contabil'         ' '  '12'  ' ',

        '' '' '' 'TG_DOCS' 'SAKNR'         'Nr.Conta-ctb'           ' '  '12'  ' ',
        '' '' '' 'TG_DOCS' 'DT_INICIO'     'Dt. Inicio'             ' '  '10'  ' ',
        '' '' '' 'TG_DOCS' 'DT_FIM'        'Dt. Fim'                ' '  '10'  ' ',
        '' '' '' 'TG_DOCS' 'COD_GPO'       'Grupo'                  ' '  '05'  ' '.


    WHEN '3'.
      DEFINE ALV3.
        WA_FCAT3-HOTSPOT   = &1.
        WA_FCAT3-REF_TABLE = &2.
        WA_FCAT3-REF_FIELD = &3.
        WA_FCAT3-TABNAME   = &4.
        WA_FCAT3-FIELDNAME = &5.
        WA_FCAT3-SCRTEXT_L = &6.
        WA_FCAT3-SCRTEXT_M = &6.
        WA_FCAT3-NO_ZERO   = &7.
        WA_FCAT3-OUTPUTLEN = &8.
        WA_FCAT3-EDIT      = &9.

        APPEND WA_FCAT3 TO IT_FCAT3.
        CLEAR WA_FCAT3.

      END-OF-DEFINITION.
      ALV3:
      ''  '' '' 'TG_ESTRA' 'NIVEL'        'Nível'             ' '  '05'  ' ',
      ''  '' '' 'TG_ESTRA' 'APROVADOR'    'Aprovador'         ' '  '20'  ' ',
      ''  '' '' 'TG_ESTRA' 'ESTADO'       'Estado'            ' '  '05'  ' ',
      'X' '' '' 'TG_ESTRA' 'OPCOES'       'Opções'            ' '  '08'  ' '.

  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CRIA_ALV .

  DATA: WA_OBJ  TYPE BORIDENT,
        IP_MODE TYPE SGS_RWMOD.

  "workflow documentos
  IF MANAGER IS NOT INITIAL.
    CALL METHOD MANAGER->UNPUBLISH.
    CLEAR: MANAGER.
  ENDIF.
  IF IT_LOTE[] IS NOT INITIAL.
    READ TABLE IT_LOTE INDEX 1.
    WA_OBJ-OBJTYPE = 'ZIM06'.
    CONCATENATE SY-MANDT IT_LOTE-LOTE INTO WA_OBJ-OBJKEY.

    IF GF_AUTHORIZATION_FT_09 EQ ABAP_TRUE.
      IP_MODE = 'E'.
    ELSE.
      IP_MODE = 'D'.
    ENDIF.

    CREATE OBJECT MANAGER
      EXPORTING
        IS_OBJECT        = WA_OBJ
        IP_NO_COMMIT     = 'R'
        IP_MODE          = IP_MODE
      EXCEPTIONS
        OBJECT_INVALID   = 1
        CALLBACK_INVALID = 2
        OTHERS           = 3.

    SET TITLEBAR 'TLWORK' WITH IT_LOTE-LOTE.

  ELSE.

  ENDIF.

  IF WA_CONT IS INITIAL.
    PERFORM FUNCTION_LAYOUT.

    WA_LAYOUT-GRID_TITLE = TEXT-T01.
    WA_LAYOUT-BOX_FNAME  = 'MARK'.
    WA_LAYOUT-SEL_MODE   = 'A'.
    WA_LAYOUT-CWIDTH_OPT = ' '.
    WA_LAYOUT-CTAB_FNAME = 'COR'.
    WA_LAYOUT-ZEBRA      = 'X'.
    WA_LAYOUT-NO_ROWMOVE = 'X'.
    WA_LAYOUT-NO_ROWINS  = 'X'.
    WA_LAYOUT-NO_ROWMARK = SPACE.
    WA_LAYOUT-NO_TOOLBAR = SPACE.
    WA_LAYOUT-INFO_FNAME = 'LINE_COLOR'.

    CREATE OBJECT WA_CONT
      EXPORTING
        CONTAINER_NAME = 'C_01'.

    CREATE OBJECT WA_ALV
      EXPORTING
*       I_SHELLSTYLE = 0
        I_PARENT = WA_CONT.
*        I_APPL_EVENTS   = SPACE
*        I_FCAT_COMPLETE = SPACE.

    CREATE OBJECT WA_EVENT.


    WA_VARIANTE-REPORT = SY-REPID. "Enable users save own LAYOUTs

    CALL METHOD WA_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT      = WA_VARIANTE
        IS_LAYOUT       = WA_LAYOUT
*       I_SAVE          = 'X'
        I_DEFAULT       = 'X'
      CHANGING
        IT_FIELDCATALOG = IT_FCAT
        IT_SORT         = I_SORT[]
        IT_OUTTAB       = IT_INVES.

    SET HANDLER: WA_EVENT->ON_CLICK   FOR WA_ALV.
  ELSE.
    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDIF.

ENDFORM.

FORM CRIA_ALV2.

  IF WA_CONT2 IS INITIAL.
    PERFORM FUNCTION_LAYOUT.
    PERFORM MONTA_ALV USING '2'.

    WA_LAYOUT-GRID_TITLE = TEXT-T02 .

    CREATE OBJECT WA_CONT2
      EXPORTING
        CONTAINER_NAME = 'C_03'.

    CREATE OBJECT WA_ALV2
      EXPORTING
        I_SHELLSTYLE    = 0
        I_PARENT        = WA_CONT2
        I_APPL_EVENTS   = SPACE
        I_FCAT_COMPLETE = SPACE.

    CALL METHOD WA_ALV2->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
        IS_VARIANT           = WA_VARIANTE
        I_SAVE               = 'X'
      CHANGING
        IT_OUTTAB            = TG_DOCS
        IT_FIELDCATALOG      = IT_FCAT2.

  ELSE.
    CALL METHOD WA_ALV2->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDFORM.

FORM FUNCTION_LAYOUT.

  FREE: WA_LAYOUT, WA_STABLE, TL_FUNCTION, WL_FUNCTION.

  WA_LAYOUT-ZEBRA      = ABAP_TRUE.
  WA_LAYOUT-NO_ROWMARK = ABAP_TRUE.
  WA_STABLE-ROW        = ABAP_TRUE.
*  WA_LAYOUT-STYLEFNAME = 'ESTILO'.
*  WA_LAYOUT-INFO_FNAME = 'COLOR'.
  WA_LAYOUT-SEL_MODE   = 'C'.
  WA_LAYOUT-NO_TOOLBAR = ABAP_TRUE.
  WA_VARIANTE-REPORT  = SY-REPID.


  REFRESH TL_FUNCTION.
  WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
  APPEND WL_FUNCTION TO TL_FUNCTION.
  WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
  APPEND WL_FUNCTION TO TL_FUNCTION.
  WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
  APPEND WL_FUNCTION TO TL_FUNCTION.
  WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
  APPEND WL_FUNCTION TO TL_FUNCTION.
  WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
  APPEND WL_FUNCTION TO TL_FUNCTION.
  WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
  APPEND WL_FUNCTION TO TL_FUNCTION.
  WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
  APPEND WL_FUNCTION TO TL_FUNCTION.
  WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
  APPEND WL_FUNCTION TO TL_FUNCTION.
  WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
  APPEND WL_FUNCTION TO TL_FUNCTION.
  WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
  APPEND WL_FUNCTION TO TL_FUNCTION.
  WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
  APPEND WL_FUNCTION TO TL_FUNCTION.
  WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
  APPEND WL_FUNCTION TO TL_FUNCTION.
  WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
  APPEND WL_FUNCTION TO TL_FUNCTION.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  APROVAR_INVESTIMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APROVAR_INVESTIMENTO .
  DATA: PRCTR TYPE PRCTR.

  DATA: APPROPRIATIONREQUEST_IN        LIKE  BAPI_APPREQ_ID-APPREQ,
        APPREQUEST_TYPE                LIKE  BAPI_APPREQ_ID-APPREQTYPE,
        APPROPRIATIONREQUESTVARIANT_IN LIKE  BAPI_APPREQ_ID-APPREQVRNT,
        CONTROLLING_AREA               LIKE  BAPI_APPREQ_ID-CNTRL_AREA,
        MASTER_DATA                    LIKE  BAPIAPPREQMASTER,
        TORG_UNITS                     TYPE TABLE OF BAPIAPPREQORGUNIT,
        WORG_UNITS                     TYPE BAPIAPPREQORGUNIT,
        TINVESTMENT_REASON             TYPE TABLE OF BAPIAPPREQINVREASON,
        WINVESTMENT_REASON             TYPE  BAPIAPPREQINVREASON,
        TPARTNER                       TYPE TABLE OF BAPIAPPREQPARTNER,
        WPARTNER                       TYPE BAPIAPPREQPARTNER,
        TASSIGNMENT_TO_POS             TYPE TABLE OF  BAPIAPPREQEXPPROGASSGN,
        WASSIGNMENT_TO_POS             TYPE  BAPIAPPREQEXPPROGASSGN,
        TASSIGNMENT_TO_BUDG_CATEG      TYPE TABLE OF BAPIAPPREQEXPPROGASSGNBUDGCATG,
        WASSIGNMENT_TO_BUDG_CATEG      TYPE  BAPIAPPREQEXPPROGASSGNBUDGCATG,
        APPREQ                         TYPE IMA_POSID,
        APPREQVRNT                     TYPE IMA_VARNT,
        RETURN                         TYPE TABLE OF BAPIRET2 WITH HEADER LINE.

  DATA: MSG_INVES(50).


  MOVE: ''   TO APPROPRIATIONREQUEST_IN,
        '01'  TO APPREQUEST_TYPE,
        '00' TO APPROPRIATIONREQUESTVARIANT_IN,
        'MAGI'  TO CONTROLLING_AREA.

  SELECT SINGLE PRCTR
    FROM CSKS
      INTO PRCTR
  WHERE KOSTL EQ WA_APROVAR-KOSTL.

  MOVE: WA_APROVAR-OBJETIVO  TO MASTER_DATA-REQ_TXT,
        WA_APROVAR-ANO       TO MASTER_DATA-ORIG_APPR_YEAR,
        WA_APROVAR-BUKRS     TO MASTER_DATA-REQ_COMP_CODE,
        WA_APROVAR-BUKRS     TO MASTER_DATA-RSP_COMP_CODE,
        WA_APROVAR-GSBER     TO MASTER_DATA-RSP_BUS_AREA,
        WA_APROVAR-KOSTL     TO MASTER_DATA-RSP_COST_CENTER,
        PRCTR                TO MASTER_DATA-RSP_PROF_CENTER.

  MOVE: WA_APROVAR-KOSTL     TO WORG_UNITS-REQ_COST_CENTER,
        WA_APROVAR-GSBER     TO WORG_UNITS-REQ_BUSINESS_AREA,
                 PRCTR       TO WORG_UNITS-REQ_PROFIT_CENTER,
                  '100'      TO WORG_UNITS-PERCENTAGE.
  APPEND WORG_UNITS TO TORG_UNITS.

  MOVE: WA_APROVAR-IZWEK     TO WINVESTMENT_REASON-INV_REASON,
                    '100'    TO WINVESTMENT_REASON-PERCENTAGE.
  APPEND WINVESTMENT_REASON  TO TINVESTMENT_REASON.


  MOVE: 'I1'                 TO WPARTNER-PARTNER_FUNCTION,
        WA_APROVAR-USUARIO   TO WPARTNER-PARTNER.
  APPEND WPARTNER TO TPARTNER.

  MOVE: 'I4'                 TO WPARTNER-PARTNER_FUNCTION,
        WA_APROVAR-APROVADOR TO WPARTNER-PARTNER.
  APPEND WPARTNER TO TPARTNER.


  CALL FUNCTION 'BAPI_APPREQUEST_CREATE'
    EXPORTING
      APPROPRIATIONREQUEST_IN        = APPROPRIATIONREQUEST_IN
      APPREQUEST_TYPE                = APPREQUEST_TYPE
      APPROPRIATIONREQUESTVARIANT_IN = APPROPRIATIONREQUESTVARIANT_IN
      CONTROLLING_AREA               = CONTROLLING_AREA
      MASTER_DATA                    = MASTER_DATA
      LANGUAGE                       = SY-LANGU
      TEST_RUN                       = ' '
    IMPORTING
      EXTERNALNUMBER                 = APPREQ
      APPROPRIATIONREQUESTVARIANTOUT = APPREQVRNT
    TABLES
      ORG_UNITS                      = TORG_UNITS
      INVESTMENT_REASON              = TINVESTMENT_REASON
      PARTNER                        = TPARTNER
      ASSIGNMENT_TO_POS              = TASSIGNMENT_TO_POS
      ASSIGNMENT_TO_BUDG_CATEG       = TASSIGNMENT_TO_BUDG_CATEG
      RETURN                         = RETURN.

  READ TABLE RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = ABAP_TRUE.

    UPDATE ZIM01_SOL_AP_INV SET STATUS_APROV = 1
                                POSNR        = APPREQ
                                DT_APROVACAO = SY-DATUM
                      WHERE BUKRS EQ WA_APROVAR-BUKRS
                        AND GSBER EQ WA_APROVAR-GSBER
                        AND ANO   EQ WA_APROVAR-ANO
                        AND SAFRA EQ WA_APROVAR-SAFRA
                       AND SAFRA2 EQ WA_APROVAR-SAFRA2
                        AND KOSTL EQ WA_APROVAR-KOSTL
                        AND BUZEI EQ WA_APROVAR-BUZEI.

    FREE: IT_DOCS, WA_TELA2.

    CALL SCREEN 0100.
    SHIFT APPREQ LEFT DELETING LEADING '0'.
    CONCATENATE  'Investimento nº' APPREQ 'criado!' INTO MSG_INVES SEPARATED BY SPACE.
    MESSAGE  MSG_INVES TYPE 'S'.
  ELSE.
    MESSAGE RETURN-MESSAGE TYPE 'E'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  PBO_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO_0102 OUTPUT.

  LOOP AT SCREEN.

    CASE SCREEN-NAME.
      WHEN 'APROVAR' OR 'TXT_APRO'.

        IF WA_APROVAR IS INITIAL.
          SCREEN-INPUT = 0.
        ELSE.
          SCREEN-INPUT = 1.
        ENDIF.
        MODIFY SCREEN.

    ENDCASE.

  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM   CRIA_ALV3 .
  IF WA_CONT3 IS INITIAL.
    PERFORM FUNCTION_LAYOUT.
    PERFORM MONTA_ALV USING '3'.

    BTN_REJ = TEXT-B01.

    WA_LAYOUT-GRID_TITLE = TEXT-T03 .

    CREATE OBJECT WA_CONT3
      EXPORTING
        CONTAINER_NAME = 'CC_ESTRA'.

    CREATE OBJECT WA_ALV3
      EXPORTING
        I_SHELLSTYLE    = 0
        I_PARENT        = WA_CONT3
        I_APPL_EVENTS   = SPACE
        I_FCAT_COMPLETE = SPACE.

    CALL METHOD WA_ALV3->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
        IS_VARIANT           = WA_VARIANTE
        I_SAVE               = 'X'
      CHANGING
        IT_OUTTAB            = TG_ESTRA
        IT_FIELDCATALOG      = IT_FCAT3.

    SET HANDLER: WA_EVENT->ON_CLICKE   FOR WA_ALV3.

  ELSE.
    CALL METHOD WA_ALV3->REFRESH_TABLE_DISPLAY.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HABILITAR_WORKFLOW_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM HABILITAR_WORKFLOW_DOCUMENTOS .
  GF_AUTHORIZATION_FT_09 = ABAP_TRUE.
  CLEAR: IT_LOTE.
  "Somente validar acesso para modificar
  IT_LOTE = WA_INVES-LOTE_ANEX.
  APPEND IT_LOTE.
ENDFORM.
