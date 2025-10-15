*&---------------------------------------------------------------------*
*& Report  ZSDR0224
*----------------------------------------------------------------------*

REPORT ZSDR0224.

TABLES: ZSDT0166, LFA1, ZSDE0091, ZSDE0094.

**********************************************************************
* TYPES
**********************************************************************

TYPES: BEGIN OF TY_PROVISIONAL,
         BURKS          TYPE BUKRS,
         NR_PROVISIONAL TYPE ZSDT0166-NR_PROVISIONAL,
         KUNNR          TYPE KUNNR,
         Desc_cliente   TYPE STRING,
         CONTRATO       TYPE ZSDT0166-CONTRATO,
         DT_TAKEUP      TYPE ZSDT0166-DATA_TAKEUP,
         MES_TAKEUP     TYPE I,
         DT_EMISSAO     TYPE ZSDT0166-DATA_ATUAL,
         DT_TAKEUP_E    TYPE ZSDT0166-DATA_TAKEUP,
         RET_CORRETORA  TYPE ZSDT0166-DATA_ATUAL,
         DT_EMISSAO_RET TYPE ZSDT0166-DATA_ATUAL,
         QTD_TON        TYPE I,
         VAL_TOTAL      TYPE ZSDT0166-VALOR_TOTAL,
         PER_ADIANT     TYPE I,
         QTD_RECEBER    TYPE ZSDT0166-VALOR_TOTAL,
         VAL_RECEBER    TYPE ZSDT0166-VALOR_TOTAL,
         DOC_CONT_PI    TYPE I,
         DT_PGto_pi     TYPE ZSDT0166-DATA_ATUAL,
         ANO            TYPE GJAHR,
         DT_RET_PGTO    TYPE ZSDT0166-DATA_ATUAL,
         QTD_TONS_REC   TYPE ZSDT0166-VALOR_TOTAL,
         RECEBIFO_USD   TYPE ZSDT0166-VALOR_TOTAL,
         A_reCeBER_USD  TYPE ZSDT0166-VALOR_TOTAL,

       END   OF TY_PROVISIONAL.

*----------------------------------------------------------------------*
* Tabela Interna
*----------------------------------------------------------------------*
DATA: it_PROVISIONAL           TYPE TABLE OF TY_PROVISIONAL,
      It_sort                  TYPE LVC_T_SORT,
      IT_ZSDT0166              TYPE TABLE OF ZSDT0166,
      WA_PROVISIONAL           TYPE TY_PROVISIONAL,
      WA_ZSDT0166              TYPE ZSDT0166,
      IT_SAIDA_PROVISIONAL     TYPE ZSDE0089_T,
      IT_SAIDA_INVOICE         TYPE ZSDE0098_T, "zsde0091_t,
      IT_SAIDA_NOTA_INVOICE    TYPE ZSDE0091_T,
      IT_SAIDA_ABAT_PI         TYPE ZSDE0101_T,
      IT_SAIDA_LOTE            TYPE ZSDE0093_T,
      IT_DADOS_LOTE            TYPE ZSDE0093_T,
      IT_SAIDA_ALV_INVOICE     TYPE ZSDE0093_T,
      IT_SAIDA_ALV_SALDO_AVAR  TYPE ZSDE0099_T,
      IT_DADOS_LOTE_AV         TYPE ZSDE0099_T,
      IT_CONTRATO              TYPE ZSDE0093_T,
      IT_TOTAL_PROVISIONAL     TYPE ZSDE0093_T,
      IT_SAIDA_ALV_INVOICE_AUX TYPE ZSDE0093_T,
      WS_HEADER_IVOICE         TYPE ZSDE0094,
      VAR_TOTAL_GERAL_DIFE     TYPE DMBTR,
      VAR_TOTAL_GERAL          TYPE DMBTR,
      VAR_TOTAL_GERAL_PESO_B   TYPE DMBTR,
      VAR_TOTAL_GERAL_PESO_L   TYPE DMBTR,
      VAR_TOTAL_GERAL_TARE     TYPE DMBTR,
      VAR_TOTAL_GERAL_VOLUM    TYPE ZSDED029,
      VAR_TOTAL_GERAL_ANTEC    TYPE DMBTR,
      IT_PARAM                 TYPE RS_T_SELECT,
      IT_KNA1                  TYPE TABLE OF KNA1,
      IT_ADRC                  TYPE TABLE OF ADRC,
      VAR_DATE                 TYPE CHAR30,
      VAR_TEXTO_CREDITO        TYPE CHAR20 VALUE 'Crédito:',
      V_TEXT_TO                TYPE CHAR45, "value ' TO: ADM INTERNATIONAL SARL', " Ajustes Impressão Invoice #189775 - BG
      V_TEXT_NR_INVOICE        TYPE CHAR45,
      V_TEXT_SAFRA             TYPE CHAR45,
      V_NR_INVOICE             TYPE CHAR45 VALUE 'INVOICE: NR.150/24',
      V_DATE_TITLE             TYPE CHAR45 VALUE 'CUIABÁ, DECEMBER 09TH, 2024'.

DATA: VG_UCOMM_AVAR       TYPE CHAR01,
      VAR_TOTAL_CAD       TYPE DMBTR,
      VAR_TOTAL_UTIL      TYPE DMBTR,
      VAR_TOTAL_PROV_UTIL TYPE DMBTR,
      WA_SELECTED_ROWS    TYPE LVC_S_ROW,
      IT_SELECTED_ROWS    TYPE LVC_T_ROW,
      LINES               TYPE SY-TABIX,
      _PARAM              TYPE  USTYP_T_PARAMETERS,
      ZVAR_USER           TYPE CHAR01,
      VAR_BLOCK           TYPE CHAR01.


DATA: OBJ_IVOICE TYPE REF TO ZCL_INVOICE.


DATA: V_GRID            TYPE REF TO CL_GUI_ALV_GRID.        "#EC NEEDED
"DATA: V_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER.     "#EC NEEDED
DATA: V_CONTAINER       TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

"Declaração fieldcatalog
DATA: IT_FIELDCATALOG TYPE LVC_T_FCAT,
      WA_FIELDCATALOG TYPE LVC_S_FCAT.

DATA: IT_FIELDCAT TYPE LVC_T_FCAT,
      WA_FIELDCAT TYPE LVC_S_FCAT.

DATA: it_fieldcat_Av TYPE LVC_T_FCAT,
      wa_fieldcat_Av TYPE LVC_S_FCAT.

DATA: LT_SORT TYPE LVC_T_SORT,
      LS_SORT TYPE LVC_S_SORT.

DATA: DG_SPLITTER_1        TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      G_GRID               TYPE REF TO CL_GUI_ALV_GRID,
      G_CUSTOM_CONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      CONTAINER_1          TYPE REF TO CL_GUI_CONTAINER,
      CL_CONTAINER_95      TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      OBJ_DYNDOC_ID        TYPE REF TO CL_DD_DOCUMENT,
      TL_FUNCTION          TYPE UI_FUNCTIONS,
      WL_FUNCTION          TYPE UI_FUNC,
      T_COLORCELL          TYPE TABLE OF LVC_S_SCOL,
      W_COLORCELL          TYPE LVC_S_SCOL,
      T_EXCTAB             TYPE SLIS_T_EXTAB,
      W_EXCTAB             TYPE SLIS_EXTAB,
      W_LAYOUT             TYPE LVC_S_LAYO,
      W_STABLE             TYPE LVC_S_STBL,
      T_STYLE              TYPE LVC_T_STYL,
      W_STYLE              TYPE LVC_S_STYL,
      T_ROWS               TYPE LVC_T_ROW,
      W_ROWS               TYPE LVC_S_ROW,
      OK_CODE              TYPE SY-UCOMM,
      IT_EXCLUDE           TYPE UI_FUNCTIONS,
      IT_EXCLUDE_AV        TYPE UI_FUNCTIONS.

DATA: _G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      _DG_SPLITTER_1      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_1         TYPE REF TO CL_GUI_CONTAINER,
      DG_SPLITTER_2       TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_2         TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_2A        TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_ALV       TYPE REF TO CL_GUI_CONTAINER,
      PICTURE             TYPE REF TO CL_GUI_PICTURE,
      _G_GRID             TYPE REF TO CL_GUI_ALV_GRID,
      DG_DYNDOC_ID        TYPE REF TO CL_DD_DOCUMENT,
      TABLE_ELEMENT       TYPE REF TO CL_DD_TABLE_ELEMENT,
      COLUMN              TYPE REF TO CL_DD_AREA,
      TABLE_ELEMENT2      TYPE REF TO CL_DD_TABLE_ELEMENT,
      COLUMN_1            TYPE REF TO CL_DD_AREA,
      DG_HTML_CNTRL       TYPE REF TO CL_GUI_HTML_VIEWER,
      IT_EXCLUDE_FCODE    TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE    LIKE LINE OF IT_EXCLUDE_FCODE,
      GS_LAYOUT           TYPE LVC_S_LAYO,
      GS_VARIANT          TYPE DISVARIANT.

DATA: G_CUSTOM_CONTAINER_ TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      _DG_SPLITTER_1_     TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_1_        TYPE REF TO CL_GUI_CONTAINER,
      DG_SPLITTER_2_      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_2_        TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_2A_       TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_ALV_      TYPE REF TO CL_GUI_CONTAINER,
      PICTURE_            TYPE REF TO CL_GUI_PICTURE,
      G_GRID_             TYPE REF TO CL_GUI_ALV_GRID,
      DG_DYNDOC_ID_       TYPE REF TO CL_DD_DOCUMENT,
      TABLE_ELEMENT_      TYPE REF TO CL_DD_TABLE_ELEMENT,
      COLUMN_             TYPE REF TO CL_DD_AREA,
      TABLE_ELEMENT2_     TYPE REF TO CL_DD_TABLE_ELEMENT,
      COLUMN_1_           TYPE REF TO CL_DD_AREA,
      DG_HTML_CNTRL_      TYPE REF TO CL_GUI_HTML_VIEWER,
      IT_EXCLUDE_FCODE_   TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE_   LIKE LINE OF IT_EXCLUDE_FCODE,
      GS_LAYOUT_          TYPE LVC_S_LAYO,
      W_STABLE_           TYPE LVC_S_STBL,
      GS_VARIANT_         TYPE DISVARIANT.

DATA: G_CUSTOM_CONTAINER_AV TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      _DG_SPLITTER_1_AV     TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_1_AV        TYPE REF TO CL_GUI_CONTAINER,
      DG_SPLITTER_2_AV      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_2_AV        TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_2A_AV       TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_ALV_AV      TYPE REF TO CL_GUI_CONTAINER,
      PICTURE_AV            TYPE REF TO CL_GUI_PICTURE,
      G_GRID_AV             TYPE REF TO CL_GUI_ALV_GRID,
      DG_DYNDOC_ID_AV       TYPE REF TO CL_DD_DOCUMENT,
      TABLE_ELEMENT_AV      TYPE REF TO CL_DD_TABLE_ELEMENT,
      COLUMN_AV             TYPE REF TO CL_DD_AREA,
      TABLE_ELEMENT2_AV     TYPE REF TO CL_DD_TABLE_ELEMENT,
      COLUMN_1_AV           TYPE REF TO CL_DD_AREA,
      DG_HTML_CNTRL_AV      TYPE REF TO CL_GUI_HTML_VIEWER,
      IT_EXCLUDE_FCODE_AV   TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE_AV   LIKE LINE OF IT_EXCLUDE_FCODE,
      GS_LAYOUT_AV          TYPE LVC_S_LAYO,
      W_STABLE_AV           TYPE LVC_S_STBL,
      GS_VARIANT_AV         TYPE DISVARIANT,
      GT_F4                 TYPE LVC_T_F4 WITH HEADER LINE.


DATA: GS_VARIANT_C TYPE DISVARIANT,
      WL_TOOLBAR   TYPE STB_BUTTON.


*----------------------------------------------------------------------*
* Parâmetros de Seleção
*----------------------------------------------------------------------*
SELECTION-SCREEN:    BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: S_BUKRS  FOR ZSDT0166-EMPRESA MODIF ID 1,
                  S_SAFRA  FOR ZSDT0166-SAFRA   MODIF ID 1,
                  S_CONTR  FOR ZSDT0166-CONTRATO MODIF ID 2,
                  S_KUNNR  FOR  LFA1-LIFNR MODIF ID 1,
                  S_PROV   FOR ZSDT0166-NR_PROVISIONAL MODIF ID 2,
                  S_LOTE   FOR ZSDT0166-LOTE MODIF ID 2,
                  "s_fazenda   FOR ZSDT0166-,
                  S_DTTKUP FOR ZSDT0166-DATA_TAKEUP MODIF ID 2,
                  S_DTPROV FOR ZSDT0166-DATA_TAKEUP MODIF ID 2,
                  S_STAT   FOR ZSDE0094-STATUS MODIF ID 1,
                  S_INVO FOR ZSDE0094-ID_INVOICE MODIF ID 1,
                  S_DTINV FOR ZSDE0091-DATA_INVOICE MODIF ID 1.
SELECTION-SCREEN: END OF BLOCK  BL1.

SELECTION-SCREEN BEGIN OF BLOCK BL2 WITH FRAME TITLE TEXT-009.
  PARAMETERS: P_PROV TYPE C DEFAULT 'X' RADIOBUTTON GROUP G1 USER-COMMAND ACESS,
              P_INVO TYPE C RADIOBUTTON GROUP G1,
              P_NOTA TYPE C RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK BL2.


AT SELECTION-SCREEN OUTPUT.
  PERFORM FM_MODIFY_SCREEN.

*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar DEFINITION ALV TELA 0300
*----------------------------------------------------------------------*
CLASS LCL_EVENTOS_0300_AV DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      SET_TOOLBAR  FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT.

    CLASS-METHODS:
      ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID.

    CLASS-METHODS:
      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.

    CLASS-METHODS:
      ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS.

    CLASS-METHODS: ON_F4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID
      IMPORTING E_FIELDNAME ES_ROW_NO ER_EVENT_DATA ET_BAD_CELLS.

    CLASS-METHODS:
      GET_UCOMM   FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.

    CLASS-METHODS:
      HANDLE_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING
          ER_DATA_CHANGED
          E_ONF4
          E_ONF4_BEFORE
          E_ONF4_AFTER
          E_UCOMM.

ENDCLASS.

CLASS LCL_EVENTOS_0300_AV IMPLEMENTATION.
  METHOD HANDLE_USER_COMMAND.

    CASE E_UCOMM.
      WHEN ''.
      WHEN ''.
      WHEN OTHERS.
    ENDCASE.


  ENDMETHOD.                    "zm_handle_user_command

  METHOD HANDLE_DATA_CHANGED.
    DATA: P_RESP TYPE CHAR01.


    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO DATA(_ITEM_CHANGED).

      CONDENSE _ITEM_CHANGED-VALUE.

      READ TABLE IT_SAIDA_ALV_SALDO_AVAR ASSIGNING FIELD-SYMBOL(<FS_GET_INDEX>) INDEX _ITEM_CHANGED-ROW_ID.

      IF SY-SUBRC <> 0.
*        append new_line to it_saida.
      ENDIF.

      READ TABLE IT_SAIDA_ALV_SALDO_AVAR ASSIGNING FIELD-SYMBOL(<FS_SET_INFO>) INDEX _ITEM_CHANGED-ROW_ID.

      CASE _ITEM_CHANGED-FIELDNAME.
        WHEN 'BTN_EXCLUIR'.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING        "TITLEBAR = 'Confirmar'
              TEXT_QUESTION         = 'Deseja realmente excluir?'
              TEXT_BUTTON_1         = 'Sim'
*             text_button_2         = 'Não'
              DISPLAY_CANCEL_BUTTON = 'X' "VALUE(DISPLAY_CANCEL_BUTTON) DEFAULT 'X'
            IMPORTING
              ANSWER                = P_RESP.

          IF P_RESP EQ 1.
            PERFORM FM_EXCLUIR.
          ENDIF.
      ENDCASE.

    ENDLOOP.




  ENDMETHOD.
  METHOD SET_TOOLBAR.
    IF WS_HEADER_IVOICE-DATA_RECEB IS INITIAL AND WS_HEADER_IVOICE-VALOR_RECEB IS INITIAL.
      WL_TOOLBAR-FUNCTION     = 'BTN_EXCLUIR'.
      WL_TOOLBAR-ICON         = ICON_DELETE.
*      wl_toolbar-butn_type    = 1.
      WL_TOOLBAR-TEXT         = 'Excluir'.
      APPEND WL_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
      CLEAR WL_TOOLBAR.

      WL_TOOLBAR-FUNCTION     = 'BTN_INSERT'.
      WL_TOOLBAR-ICON         = ICON_INSERT_ROW.
*      wl_toolbar-butn_type    = 1.
      WL_TOOLBAR-TEXT         = 'Inserir linha'.
      APPEND WL_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
      CLEAR WL_TOOLBAR.
    ENDIF.
  ENDMETHOD.                    "SET_TOOLBAR

  METHOD GET_UCOMM.
    DATA: P_RESP, CHECK, P_ERRO(1),
          RG_PROVISIONAL TYPE RANGE OF ZDE_NR_PROVISIONAL,
          IT_RESULT      TYPE ZSDE0099_T,
          IT_RETURN      TYPE TABLE OF DDSHRETVAL,
          IT_DSELC       TYPE TABLE OF DSELC,
          WA_DSELC       TYPE DSELC,
          VAR_LOTE       TYPE CHAR10,
          VARG_PI        TYPE CHAR15,
          LS_GOOD        TYPE LVC_S_MODI,
          RG_LOTE        TYPE RANGE OF CHARG_D.

    DATA: LT_LOTES TYPE STANDARD TABLE OF CHARG_D WITH EMPTY KEY,
          LT_PROV  TYPE STANDARD TABLE OF ZDE_NR_PROVISIONAL WITH EMPTY KEY.

    CASE E_UCOMM.
      WHEN 'BTN_EXCLUIR'.

*        FREE: t_rows[].
*
*        call method g_grid_av->get_selected_rows
*          importing
*            et_index_rows = t_rows. "DATA(lt_sel_rows). *-CS2022000332-#79430-02.08.2022-JT-inicio

*        data(it_rows) = t_rows[].
*
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING        "TITLEBAR = 'Confirmar'
            TEXT_QUESTION         = 'Deseja realmente excluir?'
            TEXT_BUTTON_1         = 'Sim'
*           text_button_2         = 'Não'
            DISPLAY_CANCEL_BUTTON = 'X' "VALUE(DISPLAY_CANCEL_BUTTON) DEFAULT 'X'
          IMPORTING
            ANSWER                = P_RESP.

        IF P_RESP EQ 1.
          PERFORM FM_EXCLUIR.
        ENDIF.
      WHEN 'BTN_INSERT'.



        IF IT_SAIDA_ALV_SALDO_AVAR IS NOT INITIAL.
          OBJ_IVOICE->AT_RG_PROV = VALUE #( FOR I IN IT_SAIDA_ALV_SALDO_AVAR ( SIGN = 'i' OPTION = 'eq' LOW = I-NR_PROVISIONAL ) ).
          OBJ_IVOICE->AT_RG_LOTE = VALUE #( FOR E IN IT_SAIDA_ALV_SALDO_AVAR ( SIGN = 'i' OPTION = 'eq' LOW = E-CHARG ) ).
        ENDIF.
*         loop at rg_lote into data(ls_lote).
*          append ls_lote-low to lt_lotes.
*        endloop.
*
*        loop at rg_provisional into data(ls_prov).
*          append ls_prov-low to lt_prov.
*        endloop.

        DATA: ZVAR_DIF TYPE DMBTR.

        FREE: IT_RESULT.

        DATA(IT_RESP) = OBJ_IVOICE->GET_SALDO_LOTE_CLIENTE( EXPORTING I_KUNNR = WS_HEADER_IVOICE-KUNNR I_SAFRA = WS_HEADER_IVOICE-SAFRA ).
        IF IT_RESP IS NOT INITIAL.

          SELECT * FROM ZSDT0404 INTO TABLE
          @DATA(IT_ZSDT0404)
          FOR ALL ENTRIES IN @IT_RESP
          WHERE CONTRATO EQ @IT_RESP-CONTRATO
            AND CHARG    EQ @IT_RESP-CHARG
            AND NR_PROVISIONAL EQ @IT_RESP-NR_PROVISIONAL.

          LOOP AT IT_RESP ASSIGNING FIELD-SYMBOL(<LS_RESP>).
*            clear: <ls_resp>-valor_cad.
            LOOP AT IT_ZSDT0404 INTO DATA(LS_ZSDT0404) WHERE CONTRATO    = <LS_RESP>-CONTRATO
                                                     AND       CHARG     = <LS_RESP>-CHARG
                                                     AND NR_PROVISIONAL  = <LS_RESP>-NR_PROVISIONAL.

              <LS_RESP>-VALOR_ANTEC =  <LS_RESP>-VALOR_ANTEC - LS_ZSDT0404-VALOR_CAD .
            ENDLOOP.


            LOOP AT IT_SAIDA_ALV_SALDO_AVAR INTO DATA(LS_SAIDA_ALV_SALDO_AVAR) WHERE CONTRATO       = <LS_RESP>-CONTRATO
                                                                                AND CHARG           = <LS_RESP>-CHARG
                                                                                AND NR_PROVISIONAL  = <LS_RESP>-NR_PROVISIONAL.

              <LS_RESP>-VALOR_ANTEC =  <LS_RESP>-VALOR_ANTEC - LS_SAIDA_ALV_SALDO_AVAR-VALOR_CAD .
            ENDLOOP.

            IF <LS_RESP>-VALOR_ANTEC > 0.
              APPEND VALUE #(
              CONTRATO        = <LS_RESP>-CONTRATO
              NR_PROVISIONAL  = <LS_RESP>-NR_PROVISIONAL
              CHARG           = <LS_RESP>-CHARG
              VALOR_ANTEC     = <LS_RESP>-VALOR_ANTEC
              VALOR_CAD       = <LS_RESP>-VALOR_CAD
              OBSERVACAO      = <LS_RESP>-OBSERVACAO
              ) TO IT_RESULT.
            ENDIF.
          ENDLOOP.
        ENDIF.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
*           DDIC_STRUCTURE  = ' '
            RETFIELD        = 'OBSERVACAO'   "field of internal table
            VALUE_ORG       = 'S'
            DYNPPROG        = SY-REPID
            DYNPNR          = SY-DYNNR
          TABLES
            VALUE_TAB       = IT_RESULT
*           FIELD_TAB       =
            RETURN_TAB      = IT_RETURN
            DYNPFLD_MAPPING = IT_DSELC.


        READ TABLE IT_RETURN INTO DATA(WA_RETURN) INDEX 1.
        IF SY-SUBRC EQ 0 AND WA_RETURN-FIELDVAL <> ''.
          SPLIT WA_RETURN-FIELDVAL AT '/' INTO: VARG_PI VAR_LOTE.
          CONDENSE: VAR_LOTE, VARG_PI NO-GAPS.
          LOOP AT IT_RESULT INTO DATA(WA_RESULT) WHERE NR_PROVISIONAL = VARG_PI AND CHARG EQ VAR_LOTE.
            WA_RESULT-OBSERVACAO = ''.
            APPEND WA_RESULT TO IT_SAIDA_ALV_SALDO_AVAR.
          ENDLOOP.
        ENDIF.

        CALL METHOD G_GRID_AV->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = W_STABLE_AV.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "GET_UCOMM

  METHOD ON_DATA_CHANGED_FINISHED.
    TYPES: BEGIN OF T_F4_STRUCTURE,
             FIELDTEXT TYPE DFIES-FIELDTEXT,
             FIELDNAME TYPE DFIES-FIELDNAME,
           END OF T_F4_STRUCTURE.

    FIELD-SYMBOLS: <ITAB> TYPE LVC_T_MODI.

    DATA: LS_MODI TYPE LVC_S_MODI.
    DATA: IT_RETURN      TYPE TABLE OF DDSHRETVAL,
          IT_DSELC       TYPE TABLE OF DSELC,
          WA_DSELC       TYPE DSELC,
          VAR_LOTE       TYPE CHAR10,
          VARG_PI        TYPE CHAR15,
          IT_RESULT      TYPE ZSDE0099_T,
          LS_GOOD        TYPE LVC_S_MODI,
          RG_PROVISIONAL TYPE RANGE OF ZDE_NR_PROVISIONAL,
          RG_LOTE        TYPE RANGE OF CHARG_D.
    DATA:  IT_SAIDA_ALV_INVOICE_aux     TYPE ZSDE0093_T.


    RG_PROVISIONAL = VALUE #( FOR I IN IT_SAIDA_ALV_SALDO_AVAR ( SIGN = 'i' OPTION = 'eq' LOW = I-NR_PROVISIONAL ) ).
    RG_LOTE = VALUE #( FOR E IN IT_SAIDA_ALV_SALDO_AVAR ( SIGN = 'i' OPTION = 'eq' LOW = E-CHARG ) ).

    DATA: ZVAR_DIF TYPE DMBTR.

    FREE: IT_RESULT.

    CALL METHOD G_GRID_AV->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = T_ROWS. "DATA(lt_sel_rows). *-CS2022000332-#79430-02.08.2022-JT-inicio

    DATA(IT_ROWS) = T_ROWS[].

*    if et_good_cells is not initial.
*      describe table et_good_cells lines data(v_lines).
*    endif.

*    if v_lines > 1.

*      obj_ivoice->get_saldo_lote_cliente( exporting i_kunnr = ws_header_ivoice-kunnr i_safra = ws_header_ivoice-safra receiving it_result = it_result ).
*      if rg_provisional is not initial and rg_lote is not initial and it_result is not initial.
*        delete it_result where charg in rg_lote and nr_provisional in rg_provisional.
*      endif.

*      call function 'F4IF_INT_TABLE_VALUE_REQUEST'
*        exporting
**         DDIC_STRUCTURE  = ' '
*          retfield        = 'OBSERVACAO'   "field of internal table
*          value_org       = 'S'
*          dynpprog        = sy-repid
*          dynpnr          = sy-dynnr
*        tables
*          value_tab       = it_result
**         FIELD_TAB       =
*          return_tab      = it_return
*          dynpfld_mapping = it_dselc.


*      read table it_return into data(wa_return) index 1.
*      if sy-subrc eq 0 and wa_return-fieldval <> ''.
*        split wa_return-fieldval at '/' into: varg_pi var_lote.
*        condense: var_lote, varg_pi no-gaps.
*        loop at it_result into data(wa_result) where nr_provisional = varg_pi and charg eq var_lote.
*          wa_result-observacao = ''.
*          append wa_result to it_saida_alv_saldo_avar.
*        endloop.
*      endif.

*    else.
    LOOP AT ET_GOOD_CELLS INTO LS_GOOD WHERE TABIX GT 0.
      CASE LS_GOOD-FIELDNAME.
        WHEN 'VALOR_ANTEC'.
          "191628 - Ajuste instrução com lote avariado - INICIO
*          CLEAR: VAR_TOTAL_CAD, VAR_TOTAL_UTIL, ZVAR_DIF.
*          LOOP AT IT_SAIDA_ALV_INVOICE ASSIGNING FIELD-SYMBOL(<WA_alv_invoice>).
*            ADD <WA_alv_invoice>-VALOR_ANTEC TO VAR_TOTAL_CAD.
*          ENDLOOP.
*
*          LOOP AT IT_SAIDA_ALV_SALDO_AVAR ASSIGNING FIELD-SYMBOL(<WA_alv_saldo_avar>).
*            ADD <WA_alv_saldo_avar>-VALOR_ANTEC TO VAR_TOTAL_UTIL.
*          ENDLOOP.
*
*          IF VAR_TOTAL_UTIL > VAR_TOTAL_CAD.
*            ZVAR_DIF = VAR_TOTAL_CAD - VAR_TOTAL_UTIL.
*            MESSAGE I024(SD) WITH |Valor utilizado não pode ser maior| |que o valor CAD| |saldo: { ZVAR_DIF }|.
*          ENDIF.
        WHEN 'VALOR_CAD'.
          CLEAR: VAR_TOTAL_CAD, VAR_TOTAL_UTIL, ZVAR_DIF.
          READ TABLE IT_SAIDA_ALV_SALDO_AVAR ASSIGNING FIELD-SYMBOL(<W_AVARIADO>) INDEX LS_GOOD-ROW_ID.

          IT_SAIDA_ALV_INVOICE_AUX = IT_SAIDA_ALV_INVOICE[].

          DELETE ADJACENT DUPLICATES FROM IT_SAIDA_ALV_INVOICE_AUX  COMPARING CONTRATO WERKS.

          LOOP AT IT_SAIDA_ALV_INVOICE ASSIGNING FIELD-SYMBOL(<WA_alv_invoice>).
            IF <WA_alv_invoice>-CHARG EQ <W_AVARIADO>-CHARG.
              ADD <WA_alv_invoice>-VALOR_ANTEC TO VAR_TOTAL_CAD.
            ENDIF.

          ENDLOOP.
"verificar se o lote informado ja foi usado em outra invoice com mesmo contrato  filial
          LOOP AT IT_SAIDA_ALV_INVOICE_AUX INTO DATA(WA_INVOICE_AUX).
            SELECT SUM( VALOR_ANTEC ) AS VAL_ANT_LOT
                           INTO @DATA(V_VALOR_ANTEC)
                           FROM ZSDT0400
                           WHERE WERKS EQ @WA_INVOICE_AUX-WERKS AND
                           CONTRATO EQ @WA_INVOICE_AUX-CONTRATO AND
                           CHARG = @<W_AVARIADO>-CHARG
                           and ID_INVOICE ne @WS_HEADER_IVOICE-ID_INVOICE.
            IF SY-SUBRC IS INITIAL.
              ADD  V_VALOR_ANTEC TO VAR_TOTAL_CAD.
            ENDIF.
          ENDLOOP.

          LOOP AT IT_SAIDA_ALV_SALDO_AVAR ASSIGNING FIELD-SYMBOL(<WA_alv_saldo_avar>).
            IF <WA_alv_saldo_avar>-CHARG EQ <W_AVARIADO>-CHARG.
              ADD <WA_alv_saldo_avar>-VALOR_ANTEC TO VAR_TOTAL_UTIL.
              ADD <WA_alv_saldo_avar>-VALOR_CAD TO VAR_TOTAL_CAD.
            ENDIF.
          ENDLOOP.

          IF VAR_TOTAL_UTIL < VAR_TOTAL_CAD.
            ZVAR_DIF = <W_AVARIADO>-VALOR_CAD - ( VAR_TOTAL_CAD - VAR_TOTAL_UTIL ).
            <W_AVARIADO>-VALOR_CAD = 0.
            MESSAGE I024(SD) WITH |Valor utilizado não pode ser maior| |que o valor CAD| |saldo: { ZVAR_DIF }|.
          ENDIF.
          "191628 - Ajuste instrução com lote avariado - INICIO
      ENDCASE.
    ENDLOOP.
*    endif.

    SORT IT_SAIDA_ALV_SALDO_AVAR BY NR_PROVISIONAL CHARG.
    DELETE IT_SAIDA_ALV_SALDO_AVAR WHERE NR_PROVISIONAL EQ SPACE.

    CALL METHOD G_GRID_AV->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = W_STABLE_AV.
  ENDMETHOD.                    "zm_handle_user_command


  METHOD ON_HOTSPOT_CLICK.


  ENDMETHOD.

  METHOD ON_F4.
    TYPES: BEGIN OF T_F4_STRUCTURE,
             FIELDTEXT TYPE DFIES-FIELDTEXT,
             FIELDNAME TYPE DFIES-FIELDNAME,
           END OF T_F4_STRUCTURE.

    FIELD-SYMBOLS: <ITAB> TYPE LVC_T_MODI.

    DATA: LS_MODI TYPE LVC_S_MODI.
    DATA: IT_RETURN      TYPE TABLE OF DDSHRETVAL,
          IT_DSELC       TYPE TABLE OF DSELC,
          WA_DSELC       TYPE DSELC,
          VAR_LOTE       TYPE CHAR10,
          VARG_PI        TYPE CHAR15,
          IT_RESULT      TYPE ZSDE0099_T,
          RG_PROVISIONAL TYPE RANGE OF ZDE_NR_PROVISIONAL,
          RG_LOTE        TYPE RANGE OF CHARG_D.


    RG_PROVISIONAL = VALUE #( FOR I IN IT_SAIDA_ALV_SALDO_AVAR ( SIGN = 'i' OPTION = 'eq' LOW = I-NR_PROVISIONAL ) ).
    RG_LOTE = VALUE #( FOR E IN IT_SAIDA_ALV_SALDO_AVAR ( SIGN = 'i' OPTION = 'eq' LOW = E-CHARG ) ).




    CASE E_FIELDNAME.
      WHEN 'NR_PROVISIONAL'.

        WA_DSELC-FLDNAME   = 'NR_PROVISIONAL'.
        WA_DSELC-DYFLDNAME = 'IT_RESULT-PROVISIONAL'.
        APPEND  WA_DSELC TO IT_DSELC.

        WA_DSELC-FLDNAME   = 'CHARG'.
        WA_DSELC-DYFLDNAME = 'IT_RESULT-CHARG'.
        APPEND  WA_DSELC TO IT_DSELC.

        WA_DSELC-FLDNAME   = 'VALOR_ANTEC'.
        WA_DSELC-DYFLDNAME = 'IT_RESULT-VALOR_ANTEC'.
        APPEND  WA_DSELC TO IT_DSELC.

        WA_DSELC-FLDNAME   = 'VALOR_CAD'.
        WA_DSELC-DYFLDNAME = 'IT_RESULT-VALOR_CAD'.
        APPEND  WA_DSELC TO IT_DSELC.

        OBJ_IVOICE->GET_SALDO_LOTE_CLIENTE( EXPORTING I_KUNNR = WS_HEADER_IVOICE-KUNNR I_SAFRA = WS_HEADER_IVOICE-SAFRA RECEIVING IT_RESULT = IT_RESULT ).
        IF RG_PROVISIONAL IS NOT INITIAL AND RG_LOTE IS NOT INITIAL AND IT_RESULT IS NOT INITIAL.
          DELETE IT_RESULT WHERE CHARG IN RG_LOTE AND NR_PROVISIONAL IN RG_PROVISIONAL.
        ENDIF.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
*           DDIC_STRUCTURE  = ' '
            RETFIELD        = 'OBSERVACAO'   "field of internal table
            VALUE_ORG       = 'S'
            DYNPPROG        = SY-REPID
            DYNPNR          = SY-DYNNR
          TABLES
            VALUE_TAB       = IT_RESULT
*           FIELD_TAB       =
            RETURN_TAB      = IT_RETURN
            DYNPFLD_MAPPING = IT_DSELC.


        READ TABLE IT_RETURN INTO DATA(WA_RETURN) INDEX 1.
        IF SY-SUBRC EQ 0 AND WA_RETURN-FIELDVAL <> ''.
          SPLIT WA_RETURN-FIELDVAL AT '/' INTO: VARG_PI VAR_LOTE.
          CONDENSE: VAR_LOTE, VARG_PI NO-GAPS.
          ASSIGN ER_EVENT_DATA->M_DATA->* TO <ITAB>.
          LOOP AT IT_RESULT INTO DATA(WA_RESULT) WHERE NR_PROVISIONAL = VARG_PI AND CHARG EQ VAR_LOTE.
            APPEND WA_RESULT TO IT_SAIDA_ALV_SALDO_AVAR.
          ENDLOOP.
          ER_EVENT_DATA->M_EVENT_HANDLED = 'X'.
        ENDIF.

        DELETE IT_SAIDA_ALV_SALDO_AVAR WHERE NR_PROVISIONAL EQ SPACE.

        CALL METHOD G_GRID_AV->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = W_STABLE_AV.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS LCL_EVENTOS DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID.

    CLASS-METHODS:
      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.

    CLASS-METHODS:
      ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS.

    CLASS-METHODS:
      HANDLE_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING
          ER_DATA_CHANGED
          E_ONF4
          E_ONF4_BEFORE
          E_ONF4_AFTER
          E_UCOMM.

ENDCLASS.

CLASS LCL_EVENTOS IMPLEMENTATION.
  METHOD HANDLE_USER_COMMAND.


  ENDMETHOD.                    "zm_handle_user_command
  METHOD HANDLE_DATA_CHANGED.
    DATA: P_RESP TYPE CHAR01.


    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO DATA(_ITEM_CHANGED).

      CONDENSE _ITEM_CHANGED-VALUE.

      READ TABLE IT_SAIDA_ALV_SALDO_AVAR ASSIGNING FIELD-SYMBOL(<FS_GET_INDEX>) INDEX _ITEM_CHANGED-ROW_ID.

      IF SY-SUBRC <> 0.
*        append new_line to it_saida.
      ENDIF.

      READ TABLE IT_SAIDA_ALV_SALDO_AVAR ASSIGNING FIELD-SYMBOL(<FS_SET_INFO>) INDEX _ITEM_CHANGED-ROW_ID.

      CASE _ITEM_CHANGED-FIELDNAME.
        WHEN 'BTN_EXCLUIR'.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING        "TITLEBAR = 'Confirmar'
              TEXT_QUESTION         = 'Deseja realmente excluir?'
              TEXT_BUTTON_1         = 'Sim'
*             text_button_2         = 'Não'
              DISPLAY_CANCEL_BUTTON = 'X' "VALUE(DISPLAY_CANCEL_BUTTON) DEFAULT 'X'
            IMPORTING
              ANSWER                = P_RESP.

          IF P_RESP EQ 1.
            PERFORM FM_EXCLUIR.
          ENDIF.
      ENDCASE.

    ENDLOOP.




  ENDMETHOD.
  METHOD ON_DATA_CHANGED_FINISHED.
    DATA: LS_GOOD         TYPE LVC_S_MODI,
          QTD_PESO_BRUTO  TYPE DMBTR,
          QTD_PESO_LIQ    TYPE DMBTR,
          QTD_PESO_TARA   TYPE DMBTR,
          vlr_VALOR_ANTEC TYPE DMBTR,
          vlr_VALOR_total TYPE DMBTR,
          VLR_TOTAL       TYPE DMBTR.

    DATA: ZVAR_QUANT_PESO_TOTAL TYPE P,
          ZVAR_QUANT_PESO_LIQ   TYPE P.
*          zvar_quant_peso_liq   type p.


    CHECK ET_GOOD_CELLS IS NOT INITIAL.

    CLEAR: WS_HEADER_IVOICE-NET_WEGHT, WS_HEADER_IVOICE-TOTAL_USS, WS_HEADER_IVOICE-TARE, WS_HEADER_IVOICE-GROSS_WEIGHT, WS_HEADER_IVOICE-BALES.

    LOOP AT ET_GOOD_CELLS INTO LS_GOOD.
      READ TABLE IT_SAIDA_ALV_INVOICE ASSIGNING FIELD-SYMBOL(<WA_SAIDA_ALV_INVOICE>) INDEX LS_GOOD-ROW_ID.
      IF IT_DADOS_LOTE IS INITIAL AND <WA_SAIDA_ALV_INVOICE>-INSTRUCAO IS NOT INITIAL.
        IT_DADOS_LOTE = OBJ_IVOICE->GET_DADOS_LOTE_INSTRUCAO( I_INSTRUCAO = CONV #( <WA_SAIDA_ALV_INVOICE>-INSTRUCAO ) ).
      ENDIF.

*      it_saida_alv_invoice_aux = obj_ivoice->get_dados_lote_instrucao( i_instrucao = conv #( <wa_saida_alv_invoice>-instrucao ) ).
      READ TABLE IT_DADOS_LOTE ASSIGNING FIELD-SYMBOL(<WA_SAIDA_ALV_INVOICE_AUX>) WITH KEY NRO_SOL_OV = <WA_SAIDA_ALV_INVOICE>-NRO_SOL_OV
                                                                                           CONTRATO = <WA_SAIDA_ALV_INVOICE>-CONTRATO
                                                                                     NR_PROVISIONAL = <WA_SAIDA_ALV_INVOICE>-NR_PROVISIONAL
                                                                                     CHARG          = <WA_SAIDA_ALV_INVOICE>-CHARG
                                                                                     POSNR          = <WA_SAIDA_ALV_INVOICE>-POSNR.
      IF  SY-SUBRC EQ 0.
        CASE LS_GOOD-FIELDNAME.
          WHEN  'VOLUM'.
            IF <WA_SAIDA_ALV_INVOICE>-GROSS_WEIGHT > 0 AND <WA_SAIDA_ALV_INVOICE>-NET_WEGHT > 0.
              <WA_SAIDA_ALV_INVOICE>-TARE = ( <WA_SAIDA_ALV_INVOICE>-GROSS_WEIGHT - <WA_SAIDA_ALV_INVOICE>-NET_WEGHT ).
            ENDIF.

            CLEAR: QTD_PESO_BRUTO, QTD_PESO_LIQ, QTD_PESO_TARA, VLR_VALOR_ANTEC, ZVAR_QUANT_PESO_TOTAL, ZVAR_QUANT_PESO_LIQ, VLR_TOTAL.
            QTD_PESO_BRUTO  = ( <WA_SAIDA_ALV_INVOICE_AUX>-GROSS_WEIGHT / <WA_SAIDA_ALV_INVOICE_AUX>-VOLUM ).
            QTD_PESO_LIQ    = ( <WA_SAIDA_ALV_INVOICE_AUX>-NET_WEGHT / <WA_SAIDA_ALV_INVOICE_AUX>-VOLUM ).
            VLR_VALOR_ANTEC = ( <WA_SAIDA_ALV_INVOICE_AUX>-VALOR_ANTEC / <WA_SAIDA_ALV_INVOICE_AUX>-VOLUM ).
            VLR_VALOR_TOTAL = ( <WA_SAIDA_ALV_INVOICE_AUX>-TOTAL_USS / <WA_SAIDA_ALV_INVOICE_AUX>-VOLUM ).

            ZVAR_QUANT_PESO_TOTAL  = ( QTD_PESO_BRUTO * <WA_SAIDA_ALV_INVOICE>-VOLUM ).
            ZVAR_QUANT_PESO_LIQ    = ( QTD_PESO_LIQ * <WA_SAIDA_ALV_INVOICE>-VOLUM ).

            <WA_SAIDA_ALV_INVOICE>-GROSS_WEIGHT = ZVAR_QUANT_PESO_TOTAL.
            <WA_SAIDA_ALV_INVOICE>-NET_WEGHT    = ZVAR_QUANT_PESO_LIQ.
            <WA_SAIDA_ALV_INVOICE>-VALOR_ANTEC  = ( vlr_VALOR_ANTEC * <WA_SAIDA_ALV_INVOICE>-VOLUM ).
            <WA_SAIDA_ALV_INVOICE>-TOTAL_USS    = ( <WA_SAIDA_ALV_INVOICE>-NET_WEGHT * <WA_SAIDA_ALV_INVOICE>-PRICE_USS ).

            IF <WA_SAIDA_ALV_INVOICE>-GROSS_WEIGHT > 0 AND <WA_SAIDA_ALV_INVOICE>-NET_WEGHT > 0.
              <WA_SAIDA_ALV_INVOICE>-TARE = ( <WA_SAIDA_ALV_INVOICE>-GROSS_WEIGHT - <WA_SAIDA_ALV_INVOICE>-NET_WEGHT ).
            ENDIF.

            <WA_SAIDA_ALV_INVOICE>-VALOR_CAD = ( <WA_SAIDA_ALV_INVOICE>-TOTAL_USS - <WA_SAIDA_ALV_INVOICE>-VALOR_ANTEC ).


          WHEN 'GROSS_WEIGHT' OR 'NET_WEGHT'.

            CLEAR: QTD_PESO_BRUTO, QTD_PESO_LIQ, QTD_PESO_TARA, VLR_VALOR_ANTEC, ZVAR_QUANT_PESO_TOTAL, ZVAR_QUANT_PESO_LIQ, VLR_TOTAL.

            IF <WA_SAIDA_ALV_INVOICE>-GROSS_WEIGHT > 0 AND <WA_SAIDA_ALV_INVOICE>-NET_WEGHT > 0.
              <WA_SAIDA_ALV_INVOICE>-TARE = ( <WA_SAIDA_ALV_INVOICE>-GROSS_WEIGHT - <WA_SAIDA_ALV_INVOICE>-NET_WEGHT ).
            ENDIF.

            <WA_SAIDA_ALV_INVOICE>-TOTAL_USS    = ( <WA_SAIDA_ALV_INVOICE>-NET_WEGHT * <WA_SAIDA_ALV_INVOICE>-PRICE_USS ).
            "valor antecipado só vai ser alterado se mudar o volume   - INICIO
            "VLR_VALOR_ANTEC = ( <WA_SAIDA_ALV_INVOICE_AUX>-VALOR_ANTEC / <WA_SAIDA_ALV_INVOICE_AUX>-TARE ).
            "<WA_SAIDA_ALV_INVOICE>-VALOR_ANTEC  = ( VLR_VALOR_ANTEC * <WA_SAIDA_ALV_INVOICE>-TARE ).
            "valor antecipado só vai ser alterado se mudar o volume - FIM
            <WA_SAIDA_ALV_INVOICE>-VALOR_CAD = ( <WA_SAIDA_ALV_INVOICE>-TOTAL_USS - <WA_SAIDA_ALV_INVOICE>-VALOR_ANTEC ).

          WHEN OTHERS.
        ENDCASE.
      ENDIF.
    ENDLOOP.


    LOOP AT IT_SAIDA_ALV_INVOICE ASSIGNING FIELD-SYMBOL(<WS_ALV_INVOICE>).
      IF <WS_ALV_INVOICE>-VOLUM > 0.
        ADD <WS_ALV_INVOICE>-VOLUM TO WS_HEADER_IVOICE-BALES.
      ENDIF.


      IF <WS_ALV_INVOICE>-GROSS_WEIGHT > 0.
        ADD <WS_ALV_INVOICE>-GROSS_WEIGHT TO WS_HEADER_IVOICE-GROSS_WEIGHT.
      ENDIF.

      IF <WS_ALV_INVOICE>-NET_WEGHT > 0.
        ADD <WS_ALV_INVOICE>-NET_WEGHT TO WS_HEADER_IVOICE-NET_WEGHT.
      ENDIF.

      IF <WS_ALV_INVOICE>-TARE > 0.
        ADD <WS_ALV_INVOICE>-TARE TO WS_HEADER_IVOICE-TARE.
      ENDIF.

      IF <WS_ALV_INVOICE>-TOTAL_USS > 0.
        ADD <WS_ALV_INVOICE>-TOTAL_USS TO WS_HEADER_IVOICE-TOTAL_USS.
      ENDIF.
    ENDLOOP.

    CALL METHOD G_GRID_->REFRESH_TABLE_DISPLAY( IS_STABLE = W_STABLE ).


  ENDMETHOD.                    "zm_handle_user_command


  METHOD ON_HOTSPOT_CLICK.

    READ TABLE IT_SAIDA_ALV_INVOICE INTO DATA(WA_ALV_INVOICE) INDEX E_ROW_ID-INDEX.
    IF SY-SUBRC EQ 0.

    ENDIF.
*
    CASE E_COLUMN_ID-FIELDNAME.
      WHEN:'INSTRUCAO'.

    ENDCASE.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar DEFINITION ALV TELA 0100
*----------------------------------------------------------------------*

CLASS LCL_EVENT_TOOLBAR DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      SET_TOOLBAR  FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT.

    CLASS-METHODS:
      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_COLUMN E_ROW ES_ROW_NO SENDER.

    CLASS-METHODS:
      ON_DATA_CHANGED_FINISHED2 FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS.

    CLASS-METHODS:
      GET_UCOMM   FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.
ENDCLASS.                    "LCL_EVENT_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_TOOLBAR IMPLEMENTATION.

  METHOD ON_DOUBLE_CLICK.
    FREE: IT_SAIDA_ALV_INVOICE.
    CLEAR: WS_HEADER_IVOICE.
    READ TABLE IT_SAIDA_INVOICE INTO DATA(WA_ALV_INVOICE) INDEX E_ROW-INDEX.
    IF SY-SUBRC EQ 0.
      OBJ_IVOICE->GET_INVOICE_GERADAS(
        EXPORTING
          I_ID_INVOICE    = WA_ALV_INVOICE-ID_INVOICE
        IMPORTING
          E_ITENS         = IT_SAIDA_ALV_INVOICE
          E_HEADER        = WS_HEADER_IVOICE
          E_LOTE_AVARIADO = IT_SAIDA_ALV_SALDO_AVAR ).
      IF IT_SAIDA_ALV_INVOICE IS NOT INITIAL AND WS_HEADER_IVOICE IS NOT INITIAL.
        CALL SCREEN 0300.
      ENDIF.

    ENDIF.
  ENDMETHOD.

  METHOD SET_TOOLBAR.
    CLEAR: WL_TOOLBAR.
    IF P_INVO IS NOT INITIAL.
      WL_TOOLBAR-FUNCTION     = 'BTN_EXIBIR_INSTRUCAO'.
      WL_TOOLBAR-ICON         = ICON_DESELECT_BLOCK.
*      wl_toolbar-butn_type    = 1.
      WL_TOOLBAR-TEXT         = 'Selecionar Instrução'.
      APPEND WL_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
      CLEAR WL_TOOLBAR.

      WL_TOOLBAR-FUNCTION     = 'BTN_EXCLUIR_INV'.
      WL_TOOLBAR-ICON         = ICON_DELETE.
*      wl_toolbar-butn_type    = 1.
      WL_TOOLBAR-TEXT         = 'Excluir Invoice'.
      APPEND WL_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
      CLEAR WL_TOOLBAR.
    ENDIF.

*    clear: wl_toolbar.
*    if p_invo is not initial.
*      wl_toolbar-function     = 'BTN_GERAR_IVOICE'.
*      wl_toolbar-icon         = icon_generate.
**      wl_toolbar-butn_type    = 1.
*      wl_toolbar-text         = 'Gerar invoice'.
*      append wl_toolbar to e_object->mt_toolbar.
*      clear wl_toolbar.
*    endif.


  ENDMETHOD.                    "SET_TOOLBAR
  METHOD ON_DATA_CHANGED_FINISHED2.
    DATA: LS_GOOD TYPE LVC_S_MODI.

    LOOP AT IT_SAIDA_PROVISIONAL ASSIGNING FIELD-SYMBOL(<WA_SAIDA_provisional>)." index ls_good-row_id.
      IF <WA_SAIDA_provisional>-DT_RET_CORRETORA IS NOT INITIAL.
        <WA_SAIDA_provisional>-DIA_EMISSAO_RET = ( <WA_SAIDA_provisional>-DT_RET_CORRETORA - <WA_SAIDA_provisional>-DT_EMISSAO ).
        <WA_SAIDA_provisional>-DIA_RET_PGTO = ( <WA_SAIDA_provisional>-DT_PGTO_PI - <WA_SAIDA_provisional>-DT_RET_CORRETORA ).
      ENDIF.
    ENDLOOP.

    CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY( IS_STABLE = W_STABLE ).

  ENDMETHOD.
  METHOD GET_UCOMM.
    DATA: P_RESP, CHECK, P_ERRO(1).

    CASE E_UCOMM.
      WHEN 'BTN_EXIBIR_INSTRUCAO'.
        DATA(IT_RESULT) = OBJ_IVOICE->GET_DADOS_INSTRUCAO( ).
        PERFORM FM_EXIBIR_DADOS_INSTRUCAO USING IT_RESULT.

      WHEN 'BTN_EXCLUIR_INV'.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING        "TITLEBAR = 'Confirmar'
            TEXT_QUESTION         = 'Deseja realmente excluir invoice?'
            TEXT_BUTTON_1         = 'Sim'
*           text_button_2         = 'Não'
            DISPLAY_CANCEL_BUTTON = 'X' "VALUE(DISPLAY_CANCEL_BUTTON) DEFAULT 'X'
          IMPORTING
            ANSWER                = P_RESP.

        IF P_RESP EQ 1.
          PERFORM FM_EXCLUIR_INVOICE.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "GET_UCOMM
ENDCLASS.                    "LCL_EVENT_TOOLBAR IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar DEFINITION ALV TELA 0200
*----------------------------------------------------------------------*
CLASS LCL_EVENT DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      SET_TOOLBAR  FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT.

    CLASS-METHODS:
      ON_DATA_CHANGED_FINISHED2 FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS.

    CLASS-METHODS:
      GET_UCOMM   FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.
ENDCLASS.

CLASS LCL_EVENT IMPLEMENTATION.

  METHOD SET_TOOLBAR.
    CLEAR: WL_TOOLBAR.
    IF P_INVO IS NOT INITIAL.
      WL_TOOLBAR-FUNCTION     = 'BTN_GERAR_IVOICE'.
      WL_TOOLBAR-ICON         = ICON_GENERATE.
*      wl_toolbar-butn_type    = 1.
      WL_TOOLBAR-TEXT         = 'Gerar invoice'.
      APPEND WL_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
      CLEAR WL_TOOLBAR.
    ENDIF.


  ENDMETHOD.                    "SET_TOOLBAR
  METHOD ON_DATA_CHANGED_FINISHED2.

  ENDMETHOD.
  METHOD GET_UCOMM.
    FREE: IT_SAIDA_ALV_INVOICE.
    CASE E_UCOMM.
      WHEN 'BTN_GERAR_IVOICE'.

        CALL METHOD _G_GRID->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = T_ROWS. "DATA(lt_sel_rows). *-CS2022000332-#79430-02.08.2022-JT-inicio

        DATA(LT_SEL_ROWS) = T_ROWS[].

        CHECK LT_SEL_ROWS IS NOT INITIAL.

        CLEAR:
        WS_HEADER_IVOICE-SHIPPERS,
        WS_HEADER_IVOICE-TOTAL_USS,
        WS_HEADER_IVOICE-GROSS_WEIGHT,
        WS_HEADER_IVOICE-NET_WEGHT,
        WS_HEADER_IVOICE-TARE,
        WS_HEADER_IVOICE-TOTAL_USS,
        WS_HEADER_IVOICE-BALES.

        LOOP AT LT_SEL_ROWS INTO DATA(W_SEL_ROWS).
          READ TABLE IT_SAIDA_LOTE INTO  DATA(WS_SAIDA_LOTE) INDEX W_SEL_ROWS-INDEX.
          IF SY-SUBRC EQ 0 AND WS_SAIDA_LOTE-VALOR_CAD > 0.

            if WS_SAIDA_LOTE-VOLUM > 0.

            WS_HEADER_IVOICE-INSTRUCAO        = WS_SAIDA_LOTE-INSTRUCAO.
            WS_HEADER_IVOICE-INSTRUCAO_ORIG   = WS_SAIDA_LOTE-INSTRUCAO.
            WS_HEADER_IVOICE-STATUS = '1'.
            WS_HEADER_IVOICE-KUNNR  = WS_SAIDA_LOTE-KUNNR.
            WS_HEADER_IVOICE-SAFRA  = WS_SAIDA_LOTE-SAFRA.


            IF WS_HEADER_IVOICE-STATUS IS NOT INITIAL.
              SELECT SINGLE DDTEXT FROM DD07T
                INTO WS_HEADER_IVOICE-DESC_STATUS
                WHERE DOMNAME EQ 'ZDE_STATUS_INVOICE'
                AND DOMVALUE_L EQ WS_HEADER_IVOICE-STATUS.
            ENDIF.

            SELECT SINGLE *
            FROM J_1BBRANCH INTO @DATA(WS_BRANCH)
            WHERE BRANCH EQ @WS_SAIDA_LOTE-WERKS.
            IF SY-SUBRC EQ 0.
              SELECT SINGLE NAME, STCD1
              FROM J_1BBRANCH INTO (@DATA(VG_NAME), @DATA(VG_CNPJ))
              WHERE BUKRS EQ @WS_BRANCH-BUKRS
               AND  BRANCH EQ '0001'.
              IF SY-SUBRC EQ 0.
                WS_HEADER_IVOICE-BUKRS  = WS_BRANCH-BUKRS.
                WS_HEADER_IVOICE-SHIPPERS = |{ VG_NAME } - { VG_CNPJ }|.
                REPLACE '.' IN WS_HEADER_IVOICE-SHIPPERS WITH SPACE.
              ENDIF.
            ENDIF.

            SELECT SINGLE A~NUMERO_RUC, C~NUMERO_DUE, B~NR_CONHEC, B~DT_DATA, D~DS_NOME_TRANSPOR
            FROM ZSDT0053 AS A
            LEFT JOIN ZNOM_CONHEC AS B ON B~ID_NOMEACAO_TRAN EQ A~ID_NOMEACAO_TRAN
            LEFT JOIN ZSDT0170 AS C ON C~NUMERO_RUC EQ A~NUMERO_RUC
            LEFT JOIN ZNOM_TRANSPORTE AS D ON D~ID_NOMEACAO_TRAN EQ A~ID_NOMEACAO_TRAN
            INTO ( @WS_HEADER_IVOICE-NUMERO_RUC, @WS_HEADER_IVOICE-NUMERO_DUE, @WS_HEADER_IVOICE-NR_CONHEC, @WS_HEADER_IVOICE-BL_DATE, @WS_HEADER_IVOICE-VESSEL )
              WHERE A~INSTRUCAO EQ @WS_SAIDA_LOTE-INSTRUCAO.

            IF WS_SAIDA_LOTE-VOLUM > 0.
              ADD WS_SAIDA_LOTE-VOLUM TO WS_HEADER_IVOICE-BALES.
            ENDIF.

            IF WS_SAIDA_LOTE-GROSS_WEIGHT > 0.
              ADD WS_SAIDA_LOTE-GROSS_WEIGHT TO WS_HEADER_IVOICE-GROSS_WEIGHT.
            ENDIF.

            IF WS_SAIDA_LOTE-NET_WEGHT > 0.
              ADD WS_SAIDA_LOTE-NET_WEGHT TO WS_HEADER_IVOICE-NET_WEGHT.
            ENDIF.

            IF WS_SAIDA_LOTE-TARE > 0.
              ADD WS_SAIDA_LOTE-TARE TO WS_HEADER_IVOICE-TARE.
            ENDIF.

            IF WS_SAIDA_LOTE-TOTAL_USS > 0.
              ADD WS_SAIDA_LOTE-TOTAL_USS TO WS_HEADER_IVOICE-TOTAL_USS.
            ENDIF.

            WS_SAIDA_LOTE-VALOR_CAD = ( WS_SAIDA_LOTE-TOTAL_USS - WS_SAIDA_LOTE-VALOR_ANTEC ).

            APPEND WS_SAIDA_LOTE TO IT_SAIDA_ALV_INVOICE.
            CLEAR: WS_SAIDA_LOTE.
            else.
               MESSAGE I024(SD) WITH 'Lote' WS_SAIDA_LOTE-CHARG 'não possue volume Disponível'.
              endif.
          ELSE.
            IF WS_SAIDA_LOTE-CHARG IS NOT INITIAL.
              MESSAGE I024(SD) WITH 'Lote' WS_SAIDA_LOTE-CHARG 'não possue saldo'.
            ENDIF.
          ENDIF.
        ENDLOOP.

        CLEAR: IT_SAIDA_ALV_INVOICE_AUX.
        IT_SAIDA_ALV_INVOICE_AUX = IT_SAIDA_ALV_INVOICE.
        IF IT_SAIDA_ALV_INVOICE IS NOT INITIAL.
          CALL SCREEN 0300.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "GET_UCOMM
ENDCLASS.                    "LCL_EVENT_TOOLBAR IMPLEMENTATION


*----------------------------------------------------------------------*
* Processamento
*----------------------------------------------------------------------*
START-OF-SELECTION.
  CREATE OBJECT OBJ_IVOICE.

  PERFORM FM_SET_PARAMETRO.

  IF S_BUKRS IS INITIAL.
    MESSAGE I024(SD) WITH 'Informe a empresa' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF S_SAFRA IS INITIAL.
    MESSAGE I024(SD) WITH 'Informe a safra' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM FM_CHECK_USER.


  OBJ_IVOICE->SET_PARAMETRO_FILTER( IT_PARAM ).

  CASE ABAP_TRUE.
    WHEN P_PROV.
      IT_SAIDA_PROVISIONAL = OBJ_IVOICE->GET_DADOS_PROVISIONAL( IT_PARAM ).
    WHEN P_INVO.
      IT_SAIDA_INVOICE = OBJ_IVOICE->GET_DADOS_INVOICE( IT_PARAM ).
    WHEN P_NOTA.
      IT_SAIDA_ABAT_PI = OBJ_IVOICE->GET_ABATIMENTO_PI( ).
    WHEN OTHERS.
  ENDCASE.

  PERFORM ZF_ALV.


*&---------------------------------------------------------------------*
*&      Form  ZF_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_ALV .
  CALL SCREEN 100.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form fm_set_parametro
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_SET_PARAMETRO .
  IF S_BUKRS IS NOT INITIAL.
    LOOP AT S_BUKRS.
      APPEND VALUE #( FIELDNM = 'BUKRS' SIGN = S_BUKRS-SIGN OPTION = S_BUKRS-OPTION LOW = S_BUKRS-LOW  HIGH = S_BUKRS-HIGH ) TO IT_PARAM.
    ENDLOOP.
  ENDIF.

  IF S_KUNNR IS NOT INITIAL.
    LOOP AT S_KUNNR.
      APPEND VALUE #( FIELDNM = 'KUNNR' SIGN = S_KUNNR-SIGN OPTION = S_KUNNR-OPTION LOW = S_KUNNR-LOW  HIGH = S_KUNNR-HIGH ) TO IT_PARAM.
    ENDLOOP.
  ENDIF.

  IF s_SAFRA IS NOT INITIAL.
    LOOP AT s_SAFRA.
      APPEND VALUE #( FIELDNM = 'SAFRA' SIGN = s_SAFRA-SIGN OPTION = s_SAFRA-OPTION LOW = s_SAFRA-LOW  HIGH = s_SAFRA-HIGH ) TO IT_PARAM.
    ENDLOOP.
  ENDIF.

  IF S_CONTR IS NOT INITIAL.
    LOOP AT S_CONTR.
      APPEND VALUE #( FIELDNM = 'CONTR' SIGN = S_CONTR-SIGN OPTION = S_CONTR-OPTION LOW = S_CONTR-LOW  HIGH = S_CONTR-HIGH ) TO IT_PARAM.
    ENDLOOP.
  ENDIF.

  IF S_PROV IS NOT INITIAL.
    LOOP AT S_PROV.
      APPEND VALUE #( FIELDNM = 'PROV' SIGN = S_PROV-SIGN OPTION = S_PROV-OPTION LOW = S_PROV-LOW  HIGH = S_PROV-HIGH ) TO IT_PARAM.
    ENDLOOP.
  ENDIF.

  IF S_LOTE IS NOT INITIAL.
    LOOP AT S_LOTE.
      APPEND VALUE #( FIELDNM = 'LOTE' SIGN = S_LOTE-SIGN OPTION = S_LOTE-OPTION LOW = S_LOTE-LOW  HIGH = S_LOTE-HIGH ) TO IT_PARAM.
    ENDLOOP.
  ENDIF.

  IF S_DTTKUP IS NOT INITIAL.
    LOOP AT S_DTTKUP.
      APPEND VALUE #( FIELDNM = 'DTTKUP' SIGN = S_DTTKUP-SIGN OPTION = S_DTTKUP-OPTION LOW = S_DTTKUP-LOW  HIGH = S_DTTKUP-HIGH ) TO IT_PARAM.
    ENDLOOP.
  ENDIF.

  IF S_DTPROV IS NOT INITIAL.
    LOOP AT S_DTPROV.
      APPEND VALUE #( FIELDNM = 'DTPROV' SIGN = S_DTPROV-SIGN OPTION = S_DTPROV-OPTION LOW = S_DTPROV-LOW  HIGH = S_DTPROV-HIGH ) TO IT_PARAM.
    ENDLOOP.
  ENDIF.

  IF S_DTINV IS NOT INITIAL.
    LOOP AT S_DTINV.
      APPEND VALUE #( FIELDNM = 'DTINV' SIGN = S_DTINV-SIGN OPTION = S_DTINV-OPTION LOW = S_DTINV-LOW  HIGH = S_DTINV-HIGH ) TO IT_PARAM.
    ENDLOOP.
  ENDIF.

  IF S_STAT IS NOT INITIAL.
    LOOP AT S_STAT.
      APPEND VALUE #( FIELDNM = 'STATUS' SIGN = S_STAT-SIGN OPTION = S_STAT-OPTION LOW = S_STAT-LOW  HIGH = S_STAT-HIGH ) TO IT_PARAM.
    ENDLOOP.
  ENDIF.

  IF S_INVO IS NOT INITIAL.
    LOOP AT S_INVO.
      APPEND VALUE #( FIELDNM = 'ID_INVOICE' SIGN = S_INVO-SIGN OPTION = S_INVO-OPTION LOW = S_INVO-LOW  HIGH = S_INVO-HIGH ) TO IT_PARAM.
    ENDLOOP.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'ST0100'.
  SET TITLEBAR 'SET0100'.

  CASE ABAP_TRUE.
    WHEN P_PROV.
      PERFORM F_INIT_ALV_PROVISIONAL.
    WHEN P_INVO.
      PERFORM F_INIT_ALV_IVOICE.
    WHEN P_NOTA.
      PERFORM F_INIT_ALV_NOTA_IVOICE.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'SAVE'.
      CASE ABAP_TRUE.
        WHEN P_PROV.
          DATA(ZV_RETURN) = OBJ_IVOICE->SAVE_MEMOR_DATA_PROVISIONAL( I_DATA = IT_SAIDA_PROVISIONAL ).
          IF ZV_RETURN IS INITIAL.
            MESSAGE I024(SD) WITH 'Não foi possivél salvar os dados'.
          ELSE.
            MESSAGE I024(SD) WITH 'Dados salvo com sucesso'.
          ENDIF.

          IT_SAIDA_PROVISIONAL = OBJ_IVOICE->GET_DADOS_PROVISIONAL( IT_PARAM ).

          CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY( IS_STABLE = W_STABLE ).

        WHEN P_NOTA.

        WHEN OTHERS.
      ENDCASE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form fm_alv_0100_invoice
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_ALV_0100_INVOICE .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_init_alv_provisional
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_INIT_ALV_PROVISIONAL .

  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.
  DATA:
    P_TEXT      TYPE SDYDO_TEXT_ELEMENT,
    FILTROS	    TYPE ZIF_SCREEN_LINHA_FILTRO,
    V_DATUM(10) TYPE C,
    V_UZEIT(10) TYPE C,
    I_FILTROS	  TYPE ZIF_SCREEN_LINHA_FILTRO_T.

  DATA: VARIANTE         LIKE DISVARIANT.
  VARIANTE = VALUE #( REPORT = SY-REPID ).

  DATA: ZV_PERIODO     TYPE CHAR30.

  "Motagem fieldcatalog
  PERFORM FM_FIELDCATALOG_PROVISIONAL.

  CLEAR: I_FILTROS.
  CONCATENATE SY-DATUM+06(02) '/' SY-DATUM+04(02) '/' SY-DATUM(04) INTO V_DATUM.
  CONCATENATE SY-UZEIT(02) ':' SY-UZEIT+02(02) ':' SY-UZEIT+04(02) INTO V_UZEIT.
  DESCRIBE TABLE IT_SAIDA_PROVISIONAL LINES DATA(V_LINES).
  APPEND VALUE #( PARAMETRO = 'Data:' VALOR = V_DATUM ) TO I_FILTROS.
  APPEND VALUE #( PARAMETRO = 'Hora:' VALOR = V_UZEIT ) TO I_FILTROS.
  APPEND VALUE #( PARAMETRO = 'Qtde.Registros:' VALOR = V_LINES ) TO I_FILTROS.

  P_TEXT = 'Exibir dados provisional'.

  IF ZCL_SCREEN=>ZIF_SCREEN~SET_CRIAR_TELA_PADRAO_REPORT(
      EXPORTING
        I_TITULO  = CONV #( P_TEXT )
        I_FILTROS = I_FILTROS
      CHANGING
        SPLIT     = DG_SPLITTER_1
        ALV       = G_GRID ) = ABAP_TRUE.


    W_LAYOUT-SEL_MODE    = 'A'.
    W_LAYOUT-COL_OPT     = ABAP_TRUE.
    W_LAYOUT-CWIDTH_OPT  = ABAP_TRUE.

    W_STABLE-ROW          = ABAP_TRUE.
    W_STABLE-COL          = ABAP_TRUE.

*    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_check.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
*    append wl_function to tl_function.

    SET HANDLER:
    LCL_EVENT_TOOLBAR=>SET_TOOLBAR     FOR G_GRID,
    LCL_EVENT_TOOLBAR=>GET_UCOMM       FOR G_GRID,
    LCL_EVENT_TOOLBAR=>ON_DATA_CHANGED_FINISHED2 FOR G_GRID.

    CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = W_LAYOUT
        I_SAVE                        = 'A'
        IT_TOOLBAR_EXCLUDING          = TL_FUNCTION
        IS_VARIANT                    = VARIANTE
      CHANGING
        IT_OUTTAB                     = IT_SAIDA_PROVISIONAL[]
        IT_FIELDCATALOG               = IT_FIELDCATALOG
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.


    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.


*    if lines( t_rows ) > 0.
*      call method g_grid->set_selected_rows
*        exporting
*          it_index_rows = t_rows.
*    endif.

  ELSE.
    CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY( IS_STABLE = W_STABLE ).
  ENDIF.

  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_init_alv_ivoice
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_INIT_ALV_IVOICE .

  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.
  DATA:
    P_TEXT      TYPE SDYDO_TEXT_ELEMENT,
    FILTROS	    TYPE ZIF_SCREEN_LINHA_FILTRO,
    V_DATUM(10) TYPE C,
    V_UZEIT(10) TYPE C,
    I_FILTROS	  TYPE ZIF_SCREEN_LINHA_FILTRO_T.

  DATA: VARIANTE         LIKE DISVARIANT.
  VARIANTE = VALUE #( REPORT = SY-REPID ).

  DATA: ZV_PERIODO     TYPE CHAR30.

  "Motagem fieldcatalog
  PERFORM FM_FIELDCATALOG_IVOICE.

  CLEAR: I_FILTROS.
  CONCATENATE SY-DATUM+06(02) '/' SY-DATUM+04(02) '/' SY-DATUM(04) INTO V_DATUM.
  CONCATENATE SY-UZEIT(02) ':' SY-UZEIT+02(02) ':' SY-UZEIT+04(02) INTO V_UZEIT.
  DESCRIBE TABLE IT_SAIDA_INVOICE LINES DATA(V_LINES).
  APPEND VALUE #( PARAMETRO = 'Data:' VALOR = V_DATUM ) TO I_FILTROS.
  APPEND VALUE #( PARAMETRO = 'Hora:' VALOR = V_UZEIT ) TO I_FILTROS.
  APPEND VALUE #( PARAMETRO = 'Qtde.Registros:' VALOR = V_LINES ) TO I_FILTROS.

  P_TEXT = 'Exibir dados invoice'.

  IF ZCL_SCREEN=>ZIF_SCREEN~SET_CRIAR_TELA_PADRAO_REPORT(
      EXPORTING
        I_TITULO  = CONV #( P_TEXT )
        I_FILTROS = I_FILTROS
      CHANGING
        SPLIT     = DG_SPLITTER_1
        ALV       = G_GRID ) = ABAP_TRUE.

    W_LAYOUT-SEL_MODE    = 'A'.
    W_LAYOUT-COL_OPT     = ABAP_TRUE.
    W_LAYOUT-CWIDTH_OPT  = ABAP_TRUE.

    W_STABLE-ROW          = ABAP_TRUE.
    W_STABLE-COL          = ABAP_TRUE.

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

    SET HANDLER:
    LCL_EVENT_TOOLBAR=>SET_TOOLBAR      FOR G_GRID,
    LCL_EVENT_TOOLBAR=>ON_DOUBLE_CLICK FOR G_GRID,
    LCL_EVENT_TOOLBAR=>GET_UCOMM        FOR G_GRID.

    CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = W_LAYOUT
        I_SAVE                        = 'A'
        IT_TOOLBAR_EXCLUDING          = TL_FUNCTION
        IS_VARIANT                    = VARIANTE
      CHANGING
        IT_OUTTAB                     = IT_SAIDA_INVOICE[]
        IT_FIELDCATALOG               = IT_FIELDCATALOG
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.


    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.


    IF LINES( T_ROWS ) > 0.
      CALL METHOD G_GRID->SET_SELECTED_ROWS
        EXPORTING
          IT_INDEX_ROWS = T_ROWS.
    ENDIF.

  ELSE.
    CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY( IS_STABLE = W_STABLE ).
  ENDIF.

  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

  CALL METHOD G_GRID->CHECK_CHANGED_DATA
*  importing
*    e_valid   =
*  changing
*    c_refresh = 'X'
    .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_init_alv_nota_ivoice
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_INIT_ALV_NOTA_IVOICE .
  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.
  DATA:
    P_TEXT      TYPE SDYDO_TEXT_ELEMENT,
    FILTROS	    TYPE ZIF_SCREEN_LINHA_FILTRO,
    V_DATUM(10) TYPE C,
    V_UZEIT(10) TYPE C,
    I_FILTROS	  TYPE ZIF_SCREEN_LINHA_FILTRO_T.

  DATA: VARIANTE         LIKE DISVARIANT.
  VARIANTE = VALUE #( REPORT = SY-REPID ).

  DATA: ZV_PERIODO     TYPE CHAR30.

  "Motagem fieldcatalog
  PERFORM FM_FIELDCATALOG_ABAT_PI.
  PERFORM FM_SORT_ABAT_PI.

  CLEAR: I_FILTROS.
  CONCATENATE SY-DATUM+06(02) '/' SY-DATUM+04(02) '/' SY-DATUM(04) INTO V_DATUM.
  CONCATENATE SY-UZEIT(02) ':' SY-UZEIT+02(02) ':' SY-UZEIT+04(02) INTO V_UZEIT.
  DESCRIBE TABLE IT_SAIDA_INVOICE LINES DATA(V_LINES).
  APPEND VALUE #( PARAMETRO = 'Data:' VALOR = V_DATUM ) TO I_FILTROS.
  APPEND VALUE #( PARAMETRO = 'Hora:' VALOR = V_UZEIT ) TO I_FILTROS.
  APPEND VALUE #( PARAMETRO = 'Qtde.Registros:' VALOR = V_LINES ) TO I_FILTROS.

  P_TEXT = 'Exibir Dados Abatimento PI'.

  IF ZCL_SCREEN=>ZIF_SCREEN~SET_CRIAR_TELA_PADRAO_REPORT(
      EXPORTING
        I_TITULO  = CONV #( P_TEXT )
        I_FILTROS = I_FILTROS
      CHANGING
        SPLIT     = DG_SPLITTER_1
        ALV       = G_GRID ) = ABAP_TRUE.

    W_LAYOUT-SEL_MODE    = 'A'.
    W_LAYOUT-COL_OPT     = ABAP_TRUE.
    W_LAYOUT-CWIDTH_OPT  = ABAP_TRUE.

    W_STABLE-ROW          = ABAP_TRUE.
    W_STABLE-COL          = ABAP_TRUE.

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

    SET HANDLER:
    LCL_EVENT_TOOLBAR=>SET_TOOLBAR      FOR G_GRID,
    LCL_EVENT_TOOLBAR=>ON_DOUBLE_CLICK FOR G_GRID,
    LCL_EVENT_TOOLBAR=>GET_UCOMM        FOR G_GRID.

    CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = W_LAYOUT
        I_SAVE                        = 'A'
        IT_TOOLBAR_EXCLUDING          = TL_FUNCTION
        IS_VARIANT                    = VARIANTE
      CHANGING
        IT_OUTTAB                     = IT_SAIDA_ABAT_PI[]
        IT_FIELDCATALOG               = IT_FIELDCATALOG
        IT_SORT                       = LT_SORT " Aqui define-se o agrupamento
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.


    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.


    IF LINES( T_ROWS ) > 0.
      CALL METHOD G_GRID->SET_SELECTED_ROWS
        EXPORTING
          IT_INDEX_ROWS = T_ROWS.
    ENDIF.

  ELSE.
    CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY( IS_STABLE = W_STABLE ).
  ENDIF.

  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

  CALL METHOD G_GRID->CHECK_CHANGED_DATA
*  importing
*    e_valid   =
*  changing
*    c_refresh = 'X'
    .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_fieldcatalog_ivoice
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_FIELDCATALOG_IVOICE .

  DATA: LC_COL_POS  TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  FREE: IT_FIELDCATALOG.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZSDE0098'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG.

  LOOP AT IT_FIELDCATALOG ASSIGNING FIELD-SYMBOL(<FS_FCAT>).
    CLEAR: <FS_FCAT>-SCRTEXT_L, <FS_FCAT>-SCRTEXT_M, <FS_FCAT>-SCRTEXT_S.
    CASE <FS_FCAT>-FIELDNAME.
      WHEN 'NAME1'.
        <FS_FCAT>-COLTEXT = 'Nome do Cliente'.
      WHEN 'STATUS_PROC'.
        <FS_FCAT>-COLTEXT = 'Status processamento'.
*        <fs_fcat>- = 'Status processamento'.
      WHEN 'ID_INVOICE' .
        <FS_FCAT>-COLTEXT = 'Nr Invoice'.
      WHEN 'SAFRA'.
        <FS_FCAT>-COLTEXT = 'Safra'.
      WHEN 'BUKRS'.
        <FS_FCAT>-COLTEXT = 'Empresa'.
      WHEN 'SHIPPERS'.
        <FS_FCAT>-COLTEXT = 'Desc.Empresa'.
      WHEN 'NR_PROVISIONAL'.
        <FS_FCAT>-COLTEXT = 'Nº Provisional'.
      WHEN 'INSTRUCAO'.
        <FS_FCAT>-COLTEXT = 'Instrução'.
      WHEN 'INSTRUCAO_ORIG'.
        <FS_FCAT>-COLTEXT = 'Instrução Origem'.
      WHEN 'CONTRATO'.
        <FS_FCAT>-COLTEXT = 'Contrato'.
      WHEN 'LOADING'.
        <FS_FCAT>-COLTEXT = 'Loading'.
      WHEN 'DESTINATION'.
        <FS_FCAT>-COLTEXT = 'Destination'.
      WHEN 'VESSEL'.
        <FS_FCAT>-COLTEXT = 'Vessel'.
      WHEN 'DESCRIPTION_OF_GOODS'.
        <FS_FCAT>-COLTEXT = 'Desc Of Goods'.
      WHEN 'DESCRIPTION_OF_GOODS'.
        <FS_FCAT>-COLTEXT = 'Desc Of Goods'.
      WHEN 'DESCRIPTION_OF_GOODS'.
        <FS_FCAT>-COLTEXT = 'Desc Of Goods'.
      WHEN 'BL_DATE'.
        <FS_FCAT>-COLTEXT = 'Data da BL'.
      WHEN 'TOTAL_USS'.
        <FS_FCAT>-COLTEXT = 'Total US$'.
*        <fs_fcat>-do_sum  = abap_true.
      WHEN 'VALOR_ANTEC'.
        <FS_FCAT>-COLTEXT = 'Antecipação'.
*        <fs_fcat>-do_sum  = abap_true.
      WHEN 'VALOR_CAD'.
        <FS_FCAT>-COLTEXT = 'Valor CAD'.
*         <fs_fcat>-do_sum  = abap_true.
      WHEN 'UTIL_PI'.
        <FS_FCAT>-COLTEXT = 'Utilização PI'.
      WHEN 'DESC_STATUS'.
        <FS_FCAT>-COLTEXT = 'Desc.Status'.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_fieldcatalog_provisional
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_FIELDCATALOG_PROVISIONAL .

  DATA: LC_COL_POS  TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  FREE: IT_FIELDCATALOG.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZSDE0089'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG.

  LOOP AT IT_FIELDCATALOG ASSIGNING FIELD-SYMBOL(<FS_FCAT>).
    CLEAR: <FS_FCAT>-SCRTEXT_L, <FS_FCAT>-SCRTEXT_M, <FS_FCAT>-SCRTEXT_S.
    CASE <FS_FCAT>-FIELDNAME.
      WHEN 'BUKRS'.
        <FS_FCAT>-COLTEXT = 'Empresa'.
        <FS_FCAT>-REF_TABLE = 'ZSDT0399'.
        <FS_FCAT>-REF_FIELD = 'BUKRS'.
      WHEN 'NR_PROVISIONAL' .
        <FS_FCAT>-COLTEXT = 'N° Provisional'.
        <FS_FCAT>-REF_TABLE = 'ZSDT0400'.
        <FS_FCAT>-REF_FIELD = 'NR_PROVISIONAL'.
      WHEN 'KUNNR'.
        <FS_FCAT>-COLTEXT = 'Cliente'.
        <FS_FCAT>-REF_TABLE = 'ZSDT0400'.
        <FS_FCAT>-REF_FIELD = 'KUNNR'.
      WHEN 'DESC_CLIENTE'.
        <FS_FCAT>-COLTEXT = 'Descrição do Cliente'.
      WHEN 'CONTRATO'.
        <FS_FCAT>-COLTEXT = 'Contrato'.
        <FS_FCAT>-REF_TABLE = 'ZSDT0400'.
        <FS_FCAT>-REF_FIELD = 'CONTRATO'.
      WHEN 'DT_TAKEUP'.
        <FS_FCAT>-COLTEXT = 'Data do Take-UP'.
      WHEN 'MES_TAKEUP'.
        <FS_FCAT>-COLTEXT = 'Mês do Take-UP'.
      WHEN 'DT_EMISSAO'.
        <FS_FCAT>-COLTEXT = 'Data Emissão'.
      WHEN 'QDT_DIA_TAKEUP_E'.
        <FS_FCAT>-COLTEXT = 'Dias Emissão - TAKE-UP'.
      WHEN 'DT_RET_CORRETORA'.
        <FS_FCAT>-COLTEXT = 'Data Retorno Corretora'.
        <FS_FCAT>-EDIT    = ABAP_TRUE.
      WHEN 'DIA_EMISSAO_RET'.
        <FS_FCAT>-COLTEXT = 'Dias retorno - emissão'. "Calcular qtde dias ( Data retorno corretora - data emissão)
*        <fs_fcat>-edit    = abap_true.
      WHEN 'QTD_TON'.
        <FS_FCAT>-COLTEXT = 'Qtde(TO)'.
      WHEN 'VAL_TOTAL'.
        <FS_FCAT>-COLTEXT = 'Valor Total'.
      WHEN 'VAL_ADIANT'.
        <FS_FCAT>-COLTEXT = 'Valor Adiantado'.
      WHEN 'PER_ADIANT'.
        <FS_FCAT>-COLTEXT = '% Adiantamento'.
      WHEN 'QTD_RECEBER'.
        <FS_FCAT>-COLTEXT = 'Qtde a Receber (TO)'.
      WHEN 'VAL_RECEBER'.
        <FS_FCAT>-COLTEXT = 'Valor a Receber'.
      WHEN 'DOC_CONT_PI'.
        <FS_FCAT>-COLTEXT = 'Doc Contabil Antecipação PI'.
      WHEN 'DT_PGTO_PI'.
        <FS_FCAT>-COLTEXT = 'Data de Pagamento'.
      WHEN 'ANO'.
        <FS_FCAT>-COLTEXT = 'Ano'.
      WHEN 'DIA_RET_PGTO'.
        <FS_FCAT>-COLTEXT = 'Dias Retorno Pagamento'. "Calcular qtde dias ( Data pagamento - data retorno corretora )
      WHEN 'QTD_TONS_REC'.
        <FS_FCAT>-COLTEXT = 'Qtde (tons) Recebido'.
      WHEN 'RECEBIFO_USD'.
        <FS_FCAT>-COLTEXT = 'Recebido U$'.
      WHEN 'A_RECEBER_USD'.
        <FS_FCAT>-COLTEXT = 'A Receber U$'.
      WHEN 'OBSERVACAO'.
        <FS_FCAT>-EDIT = ABAP_TRUE.
      WHEN 'PREVISAO_CAD'.
        <FS_FCAT>-COLTEXT = 'Previsão CAD'.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_exibir_dados_instrucao
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_EXIBIR_DADOS_INSTRUCAO USING IT_RESULT TYPE ZSDE0092_T.
  FREE: IT_SAIDA_ALV_SALDO_AVAR.
  DATA: LINHA_SELECIONADA TYPE SLIS_SELFIELD.
  DATA: _EXIT             TYPE C.
  CLEAR: WS_HEADER_IVOICE.

  DATA(TL_FIELDCAT) = VALUE SLIS_T_FIELDCAT_ALV(

          ( FIELDNAME = 'BUKRS     '        SELTEXT_M = 'Empresa     '  OUTPUTLEN = '10' )
          ( FIELDNAME = 'INSTRUCAO '        SELTEXT_M = 'Instrução   '  OUTPUTLEN = '25' )
*          ( fieldname = 'CONTRATO  '        seltext_m = 'Contrato    '  outputlen = '20' )
          ( FIELDNAME = 'SAFRA     '        SELTEXT_M = 'SAFRA       '  OUTPUTLEN = '10' )
          ( FIELDNAME = 'KUNNR     '        SELTEXT_M = 'Cód.Cliente '  OUTPUTLEN = '15' )
          ( FIELDNAME = 'NAME1     '        SELTEXT_M = 'Desc.Cliente'  OUTPUTLEN = '40' ) ).


  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      I_TITLE     = 'Dados da instrução'
      I_SELECTION = 'X'
      I_TABNAME   = 'IT_RESULT'
      I_ZEBRA     = 'X'
      IT_FIELDCAT = TL_FIELDCAT
    IMPORTING
      ES_SELFIELD = LINHA_SELECIONADA
      E_EXIT      = _EXIT
    TABLES
      T_OUTTAB    = IT_RESULT.


  CASE LINHA_SELECIONADA-FIELDNAME.
    WHEN 'INSTRUCAO'.
      FREE: IT_SAIDA_LOTE, IT_DADOS_LOTE.
      IT_DADOS_LOTE = OBJ_IVOICE->GET_DADOS_LOTE_INSTRUCAO( I_INSTRUCAO = CONV #( LINHA_SELECIONADA-VALUE ) ).
      IT_SAIDA_LOTE = IT_DADOS_LOTE.
      SELECT * FROM ZSDT0400 INTO TABLE @DATA(IT_ZSDT0400) FOR ALL ENTRIES IN @IT_DADOS_LOTE
      WHERE CONTRATO EQ @IT_DADOS_LOTE-CONTRATO
       AND  CHARG    EQ @IT_DADOS_LOTE-CHARG
       AND  INSTRUCAO EQ @IT_DADOS_LOTE-INSTRUCAO.
      LOOP AT IT_SAIDA_LOTE ASSIGNING FIELD-SYMBOL(<WA_SAIDA_LOTE>).
        LOOP AT IT_ZSDT0400 ASSIGNING FIELD-SYMBOL(<WA_ZSDT0400>) WHERE NRO_SOL_OV EQ <WA_SAIDA_LOTE>-NRO_SOL_OV
                                                                    AND POSNR    EQ <WA_SAIDA_LOTE>-POSNR
                                                                    AND CONTRATO EQ <WA_SAIDA_LOTE>-CONTRATO
                                                                    AND CHARG    EQ <WA_SAIDA_LOTE>-CHARG
                                                                   AND INSTRUCAO EQ <WA_SAIDA_LOTE>-INSTRUCAO.


          <WA_SAIDA_LOTE>-VOLUM        = ( <WA_SAIDA_LOTE>-VOLUM - <WA_ZSDT0400>-VOLUM ).
          <WA_SAIDA_LOTE>-NET_WEGHT    = ( <WA_SAIDA_LOTE>-NET_WEGHT - <WA_ZSDT0400>-NET_WEGHT ).
          <WA_SAIDA_LOTE>-GROSS_WEIGHT = ( <WA_SAIDA_LOTE>-GROSS_WEIGHT - <WA_ZSDT0400>-GROSS_WEIGHT ).
          <WA_SAIDA_LOTE>-TARE         = ( <WA_SAIDA_LOTE>-TARE - <WA_ZSDT0400>-TARE ).
          <WA_SAIDA_LOTE>-TOTAL_USS    = ( <WA_SAIDA_LOTE>-TOTAL_USS - <WA_ZSDT0400>-TOTAL_USS ).
          <WA_SAIDA_LOTE>-VALOR_ANTEC  = ( <WA_SAIDA_LOTE>-VALOR_ANTEC - <WA_ZSDT0400>-VALOR_ANTEC ).
        ENDLOOP.
        <WA_SAIDA_LOTE>-VALOR_CAD    = ( <WA_SAIDA_LOTE>-TOTAL_USS - <WA_SAIDA_LOTE>-VALOR_ANTEC ).
      ENDLOOP.


*      it_saida_lote
      PERFORM FM_EXIBIR_DADOS_LOTE USING IT_SAIDA_LOTE.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_exibir_dados_lote
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_RESULT
*&---------------------------------------------------------------------*
FORM FM_EXIBIR_DADOS_LOTE  USING  P_IT_RESULT TYPE ZSDE0093_T.
  CALL SCREEN 200.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'ST0200'.
  SET TITLEBAR 'SET0200'.

  PERFORM FM_EXIB_ALV.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.


  CASE SY-UCOMM.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form fm_exib_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_EXIB_ALV .

  "Motagem fieldcatalog
  PERFORM FM_FIELDCATALOG_LOTE.

  IF _G_CUSTOM_CONTAINER IS INITIAL.

    CREATE OBJECT _G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME = 'CC_'.

    CREATE OBJECT _G_GRID
      EXPORTING
        I_PARENT = _G_CUSTOM_CONTAINER.

    GS_LAYOUT-ZEBRA       = ABAP_TRUE.
    GS_LAYOUT-CWIDTH_OPT  = ABAP_TRUE.
    GS_LAYOUT-SEL_MODE    = 'A'.

    SET HANDLER:
    LCL_EVENT=>SET_TOOLBAR     FOR _G_GRID,
    LCL_EVENT=>GET_UCOMM       FOR _G_GRID.

    CALL METHOD _G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT      = GS_VARIANT
        IS_LAYOUT       = GS_LAYOUT
        I_SAVE          = 'A'
        I_DEFAULT       = ABAP_TRUE
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCAT
        IT_OUTTAB       = IT_SAIDA_LOTE.

  ELSE.

    CALL METHOD _G_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = W_STABLE.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_fieldcatalog_lote
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_FIELDCATALOG_LOTE .

  DATA: LC_COL_POS  TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  FREE: IT_FIELDCATALOG.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZSDE0093'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCAT.

  LOOP AT IT_FIELDCAT ASSIGNING FIELD-SYMBOL(<FS_FCAT>).
    CLEAR: <FS_FCAT>-SCRTEXT_L, <FS_FCAT>-SCRTEXT_M, <FS_FCAT>-SCRTEXT_S.
    CASE <FS_FCAT>-FIELDNAME.
      WHEN 'VOLUM_CONTRATO'.
        <FS_FCAT>-COLTEXT  = 'Volume contratado'.
      WHEN 'VALOR_ANTEC'.
        <FS_FCAT>-COLTEXT = 'Antecipação'.
      WHEN 'CONTRATO'.
        <FS_FCAT>-COLTEXT = 'Contrato'.
      WHEN 'CONTRATO_CLIENTE'.
        <FS_FCAT>-COLTEXT  = 'Contrato de cliente'.
      WHEN 'NAME1'.
        <FS_FCAT>-COLTEXT = 'Cliente'.
      WHEN 'DESC_PONT_COLE'.
        <FS_FCAT>-COLTEXT = 'Descrição fazenda'.
      WHEN 'TIPO' .
        <FS_FCAT>-COLTEXT = 'Tipo'.
      WHEN 'TARE'.
        <FS_FCAT>-COLTEXT = 'TARE'.
      WHEN 'GROSS_WEIGHT'.
        <FS_FCAT>-COLTEXT = 'Gross weight'.
      WHEN 'SAFRA'.
        <FS_FCAT>-COLTEXT = 'Safra'.
      WHEN 'NET_WEGHT'.
        <FS_FCAT>-COLTEXT = 'Net weght'.
      WHEN 'PRICE_LBS'.
        <FS_FCAT>-COLTEXT = 'Price lbs'.
      WHEN 'PRICE_USS'.
        <FS_FCAT>-COLTEXT = 'Price US$'.
      WHEN 'TOTAL_USS'.
        <FS_FCAT>-COLTEXT = 'Total US$'.
      WHEN 'VALOR_CAD'.
        <FS_FCAT>-COLTEXT = 'Valor CAD'.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_modify_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_MODIFY_SCREEN .

  " Suponha que você controla isso com uma variável de condição
  IF P_INVO = ABAP_TRUE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = '2'.
        SCREEN-ACTIVE = 0.  " Esconde o campo
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_exib_alv_0300
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_EXIB_ALV_0300 .

  "Motagem fieldcatalog
  PERFORM FM_FIELDCATALOG_GERAR_INVOICE.
  PERFORM FM_SORT_300.


  IF G_CUSTOM_CONTAINER_ IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER_
      EXPORTING
        CONTAINER_NAME = 'CC_300'.

    CREATE OBJECT G_GRID_
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER_.

    GS_LAYOUT_-SEL_MODE    = 'A'.
    GS_LAYOUT_-COL_OPT     = ABAP_TRUE.
    GS_LAYOUT_-CWIDTH_OPT  = ABAP_TRUE.

    W_STABLE_-ROW          = ABAP_TRUE.
    W_STABLE_-COL          = ABAP_TRUE.

*    create object event_receiver.
*    set handler: event_receiver->on_data_changed_finished for g_grid_.
*    lcl_event_toolbar=>on_data_changed_finished2  for g_grid_300.

    SET HANDLER: LCL_EVENTOS=>ON_HOTSPOT_CLICK FOR G_GRID_.
    SET HANDLER: LCL_EVENTOS=>ON_DATA_CHANGED_FINISHED FOR G_GRID_.
    SET HANDLER: LCL_EVENTOS=>HANDLE_USER_COMMAND FOR G_GRID_.

    PERFORM EXCLUIR_BOTOES CHANGING IT_EXCLUDE.

    CALL METHOD G_GRID_->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
*       is_variant           = gs_variant_300
        IS_LAYOUT            = GS_LAYOUT_
        I_SAVE               = 'A'
        I_DEFAULT            = ABAP_TRUE
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCAT
        IT_OUTTAB            = IT_SAIDA_ALV_INVOICE
        IT_SORT              = LT_SORT. " Aqui define-se o agrupamento

    CALL METHOD G_GRID_->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD G_GRID_->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  ELSE.

    CALL METHOD G_GRID_->REFRESH_TABLE_DISPLAY.
  ENDIF.

*==============================================================

  "Motagem fieldcatalog
  PERFORM FM_FIELDCATALOG_SALDO_AV.


  "Criar alv saldo avariado.
  IF G_CUSTOM_CONTAINER_AV IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER_AV
      EXPORTING
        CONTAINER_NAME = 'CC_300_2'.

    CREATE OBJECT G_GRID_AV
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER_AV.

    GS_LAYOUT_AV-SEL_MODE    = 'A'.
    GS_LAYOUT_AV-COL_OPT     = ABAP_TRUE.
    GS_LAYOUT_AV-CWIDTH_OPT  = ABAP_TRUE.

    W_STABLE_AV-ROW          = ABAP_TRUE.
    W_STABLE_AV-COL          = ABAP_TRUE.

*    create object event_receiver.
*    set handler: event_receiver->on_data_changed_finished for g_grid_.
*    lcl_event_toolbar=>on_data_changed_finished2  for g_grid_300.

*    set handler: lcl_eventos=>on_hotspot_click for g_grid_.
    SET HANDLER: LCL_EVENTOS_0300_AV=>ON_DATA_CHANGED_FINISHED FOR G_GRID_AV.
    SET HANDLER: LCL_EVENTOS_0300_AV=>HANDLE_USER_COMMAND FOR G_GRID_AV.
    SET HANDLER: LCL_EVENTOS_0300_AV=>GET_UCOMM FOR G_GRID_AV.
    SET HANDLER: LCL_EVENTOS_0300_AV=>SET_TOOLBAR FOR G_GRID_AV.
    SET HANDLER: LCL_EVENTOS_0300_AV=>HANDLE_DATA_CHANGED FOR G_GRID_AV.
*    set handler: lcl_eventos_0300_av=>on_f4 for g_grid_av.

    PERFORM EXCLUIR_BOTOES_AV CHANGING IT_EXCLUDE_AV.

    CALL METHOD G_GRID_AV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
*       is_variant           = gs_variant_300
        IS_LAYOUT            = GS_LAYOUT_AV
        I_SAVE               = 'A'
        I_DEFAULT            = ABAP_TRUE
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_AV
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCAT_AV
        IT_OUTTAB            = IT_SAIDA_ALV_SALDO_AVAR.
*        it_sort         = lt_sort. " Aqui define-se o agrupamento

    CALL METHOD G_GRID_AV->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD G_GRID_AV->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.


    FREE: GT_F4.
    APPEND VALUE #( FIELDNAME = 'NR_PROVISIONAL'
                     REGISTER = ABAP_TRUE
                    GETBEFORE = ABAP_TRUE
                   CHNGEAFTER = ABAP_TRUE
           ) TO  GT_F4.

*    call method g_grid_av->register_f4_for_fields
*      exporting
*        it_f4 = gt_f4[].


    SET HANDLER: LCL_EVENTOS_0300_AV=>ON_F4 FOR G_GRID_AV.
* set handler: lcl_eventos=>on_hotspot_click for g_grid_.
*    set handler: lcl_eventos_0300_av=>on_data_changed_finished for g_grid_av.
*    set handler: lcl_eventos_0300_av=>handle_user_command for g_grid_av.

  ELSE.

    CALL METHOD G_GRID_AV->REFRESH_TABLE_DISPLAY.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  DATA: TG_FCODE TYPE TABLE OF SY-UCOMM WITH HEADER LINE.

  IF VAR_BLOCK IS INITIAL.
    TG_FCODE = 'SAVE'.
    APPEND TG_FCODE.
  ENDIF.

  SET PF-STATUS 'ST0300' EXCLUDING TG_FCODE.
  SET TITLEBAR 'SET0300'.

  PERFORM FM_EXIB_ALV_0300.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.
  DATA: ZVAR_DIF TYPE DMBTR.


  CASE SY-UCOMM.
    WHEN 'EXIT'.
      FREE: IT_SAIDA_INVOICE.
      IT_SAIDA_INVOICE = OBJ_IVOICE->GET_DADOS_INVOICE( IT_PARAM ).
      CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY( IS_STABLE = W_STABLE ).

      LEAVE TO SCREEN 0.
    WHEN 'BACK'.
      FREE: IT_SAIDA_INVOICE.
      IT_SAIDA_INVOICE = OBJ_IVOICE->GET_DADOS_INVOICE( IT_PARAM ).
      CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY( IS_STABLE = W_STABLE ).

      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.

      CLEAR: VAR_TOTAL_CAD, VAR_TOTAL_UTIL, VAR_TOTAL_PROV_UTIL.
      LOOP AT IT_SAIDA_ALV_INVOICE ASSIGNING FIELD-SYMBOL(<WA_alv_invoice>).
        ADD <WA_alv_invoice>-VALOR_CAD TO VAR_TOTAL_CAD.
      ENDLOOP.

      LOOP AT IT_SAIDA_ALV_SALDO_AVAR ASSIGNING FIELD-SYMBOL(<WA_alv_saldo_avar>).
        ADD <WA_alv_saldo_avar>-VALOR_CAD TO VAR_TOTAL_UTIL.
        ADD <WA_alv_saldo_avar>-VALOR_ANTEC TO VAR_TOTAL_PROV_UTIL.

        IF <WA_alv_saldo_avar>-CONTRATO_REF IS INITIAL.
          MESSAGE I024(SD) WITH |Informe contrato de referencia|.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF VAR_TOTAL_UTIL > VAR_TOTAL_CAD.
        CLEAR: ZVAR_DIF.
        ZVAR_DIF = VAR_TOTAL_CAD - VAR_TOTAL_UTIL.
        MESSAGE I024(SD) WITH |Valor utilizado não pode ser maior| |que o valor CAD| |saldo: { ZVAR_DIF }|.
        EXIT.
      ENDIF.

      IF VAR_TOTAL_UTIL > VAR_TOTAL_PROV_UTIL.
        MESSAGE I024(SD) WITH 'Valor utilizado não pode ser maior' 'que o valor total provisional'.
        EXIT.
      ENDIF.

      OBJ_IVOICE->SAVE_DADOS_INVOICE(
        EXPORTING
          I_HEADER        = WS_HEADER_IVOICE      " Dados cabeçalho para gerar invoice
          I_ITENS         = IT_SAIDA_ALV_INVOICE  " Dados lotes vinculado a instrução
          I_LOTE_AVARIADO = IT_SAIDA_ALV_SALDO_AVAR
        IMPORTING
          E_ID_INVOICE    = WS_HEADER_IVOICE-ID_INVOICE
        RECEIVING
          R_RESULT        = DATA(VAR_RESULT)                " Retorno
      ).
      IF VAR_RESULT IS NOT INITIAL.
        MESSAGE I024(SD) WITH 'Invoice salvo com sucesso.'.
        FREE: IT_SAIDA_INVOICE.
        IT_SAIDA_INVOICE = OBJ_IVOICE->GET_DADOS_INVOICE( IT_PARAM ).
        CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY( IS_STABLE = W_STABLE ).
        LEAVE TO SCREEN 100.
      ELSE.
        MESSAGE I024(SD) WITH 'Erro ao salvar invoice'.
      ENDIF.
    WHEN 'PRINT'.
      PERFORM FM_PRINT_INVOICE.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form fm_fieldcatalog_gerar_ivoice
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_FIELDCATALOG_GERAR_IVOICE .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_fieldcatalog_gerar_invoice
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_FIELDCATALOG_GERAR_INVOICE .

  DATA: LC_COL_POS TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  FREE: IT_FIELDCAT.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZSDE0093'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCAT.

  IF ZVAR_USER IS INITIAL.
    IF WS_HEADER_IVOICE-DATA_RECEB IS NOT INITIAL.
      VAR_BLOCK = ''.
    ELSE.
      VAR_BLOCK = 'X'.
    ENDIF.
  ELSE.
    VAR_BLOCK = 'X'.
  ENDIF.

  LOOP AT IT_FIELDCAT ASSIGNING FIELD-SYMBOL(<FS_FCAT>).
    CLEAR: <FS_FCAT>-SCRTEXT_L, <FS_FCAT>-SCRTEXT_M, <FS_FCAT>-SCRTEXT_S.
    CASE <FS_FCAT>-FIELDNAME.
      WHEN 'VOLUM'.
        <FS_FCAT>-EDIT    = VAR_BLOCK.
        <FS_FCAT>-DO_SUM  = ABAP_TRUE.
      WHEN 'VOLUM_CONTRATO'.
        <FS_FCAT>-COLTEXT  = 'Volume contratado'.
      WHEN 'INSTRUCAO'.
        <FS_FCAT>-EDIT    = VAR_BLOCK.
        <FS_FCAT>-HOTSPOT = ABAP_TRUE.
      WHEN 'CONTRATO'.
        <FS_FCAT>-COLTEXT  = 'Contrato'.
        <FS_FCAT>-SP_GROUP = ABAP_TRUE. " Agrupamento por contrato
      WHEN 'CONTRATO_CLIENTE'.
        <FS_FCAT>-COLTEXT  = 'Contrato de cliente'.
      WHEN 'NAME1'.
        <FS_FCAT>-COLTEXT = 'Cliente'.
      WHEN 'DESC_PONT_COLE'.
        <FS_FCAT>-COLTEXT = 'Descrição fazenda'.
      WHEN 'TIPO' .
        <FS_FCAT>-COLTEXT = 'Tipo'.
        <FS_FCAT>-EDIT    = VAR_BLOCK.
      WHEN 'TARE'.
        <FS_FCAT>-COLTEXT = 'TARE'.
        <FS_FCAT>-DO_SUM  = ABAP_TRUE.
      WHEN 'GROSS_WEIGHT'.
        <FS_FCAT>-COLTEXT = 'Gross weight'.
        <FS_FCAT>-EDIT    = VAR_BLOCK.
        <FS_FCAT>-DO_SUM  = ABAP_TRUE.
      WHEN 'SAFRA'.
        <FS_FCAT>-COLTEXT = 'Safra'.
      WHEN 'NET_WEGHT'.
        <FS_FCAT>-COLTEXT = 'Net weght'.
        <FS_FCAT>-EDIT    = VAR_BLOCK.
        <FS_FCAT>-DO_SUM  = ABAP_TRUE.
      WHEN 'PRICE_LBS'.
        <FS_FCAT>-COLTEXT = 'Price lbs'.
      WHEN 'PRICE_USS'.
        <FS_FCAT>-COLTEXT = 'Price US$'.
      WHEN 'TOTAL_USS'.
        <FS_FCAT>-COLTEXT = 'Total US$'.
        <FS_FCAT>-DO_SUM  = ABAP_TRUE.
      WHEN 'VALOR_ANTEC'.
        <FS_FCAT>-COLTEXT = 'Antecipação'.
        <FS_FCAT>-DO_SUM  = ABAP_TRUE.
      WHEN 'VALOR_CAD'.
        <FS_FCAT>-COLTEXT = 'Valor CAD'.
        <FS_FCAT>-DO_SUM  = ABAP_TRUE.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_sort_300
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_SORT_300 .


  CLEAR: LS_SORT.
  LS_SORT-FIELDNAME = 'CONTRATO'.
  LS_SORT-UP        = 'X'.
  LS_SORT-SUBTOT    = 'X'. " Subtotal por contrato
  APPEND LS_SORT TO LT_SORT.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  SET_DESC_STATUS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_DESC_STATUS INPUT.

  CLEAR: WS_HEADER_IVOICE-DESC_STATUS.
  IF WS_HEADER_IVOICE-STATUS IS NOT INITIAL.
    SELECT SINGLE DDTEXT FROM DD07T INTO WS_HEADER_IVOICE-DESC_STATUS WHERE DOMNAME EQ 'ZDE_STATUS_INVOICE' AND DOMVALUE_L EQ WS_HEADER_IVOICE-STATUS.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form fm_print_invoice
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_PRINT_INVOICE .
  DATA: VL_NAME       TYPE RS38L_FNAM,
        LS_OPTIONS    TYPE SSFCOMPOP,
        CONTROL       TYPE SSFCTRLOP,
        T_MCOD3       TYPE TABLE OF LFA1,
        T_STCD1       TYPE TABLE OF LFA1,
        L_FORMULARIO  TYPE CHAR1,
        L_LIFNR       TYPE LIFNR,
        L_TABIX       TYPE SY-TABIX,
        L_INPUT       TYPE TDSFNAME,
        V_BUTXT       TYPE T001-BUTXT,
        T_TEXTO_GERAL TYPE TSFTEXT,
        v_total_USD   TYPE DMBTR . "Ajuste impressão valor total usd #190822 - BG


  DATA: ZVAR_QUANT_PESO_TOTAL TYPE P,
        ZVAR_QUANT_PESO_LIQ   TYPE P,
        ZVAR_TOTAL            TYPE P.

  L_INPUT = 'ZSDF0018'.

  SELECT SINGLE BUTXT
  INTO @V_BUTXT
  FROM T001
  WHERE BUKRS EQ @WS_HEADER_IVOICE-BUKRS.

  SELECT *
  INTO TABLE @DATA(LT_ZSDT0292)
  FROM ZSDT0292
  WHERE EMPRESA EQ @WS_HEADER_IVOICE-BUKRS
  AND STATUS  EQ 'A'.

  IF ( LT_ZSDT0292 IS NOT INITIAL )..
    SORT LT_ZSDT0292 BY EMPRESA DATA DESCENDING HORA DESCENDING.
    DATA(LV_NAME) = LT_ZSDT0292[ 1 ]-TEXTO_GERAL.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID                      = 'ST'
        LANGUAGE                = SY-LANGU
        NAME                    = LV_NAME
        OBJECT                  = 'TEXT'
      TABLES
        LINES                   = T_TEXTO_GERAL
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.

  ENDIF.


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = L_INPUT
    IMPORTING
      FM_NAME            = VL_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

*  Impresora
  LS_OPTIONS-TDDEST   = 'LOCL'.     "Disposit. saída
  LS_OPTIONS-TDIMMED  = ABAP_TRUE.  "Saída Imediata
  LS_OPTIONS-TDNEWID  = ABAP_TRUE.  "Nova Ordem SPOOL
  LS_OPTIONS-TDCOVTITLE = |'Provision_Model_1_ { SY-UNAME }_{ SY-DATUM }_{ SY-UZEIT }|. "Titulo
  "PHQL

  CLEAR: IT_CONTRATO.
  SORT IT_SAIDA_ALV_INVOICE BY CONTRATO.
  IT_CONTRATO = IT_SAIDA_ALV_INVOICE.
  IT_TOTAL_PROVISIONAL = IT_SAIDA_ALV_INVOICE.
  DELETE ADJACENT DUPLICATES FROM IT_CONTRATO COMPARING CONTRATO.


  CLEAR: VAR_TOTAL_GERAL_PESO_B, VAR_TOTAL_GERAL_PESO_L, VAR_TOTAL_GERAL_TARE, VAR_TOTAL_GERAL_VOLUM.
  "Ajuste impressão valor total usd #190822 - BG
  CLEAR: VAR_TOTAL_GERAL,  VAR_TOTAL_GERAL_ANTEC,  VAR_TOTAL_GERAL_PESO_B,  VAR_TOTAL_GERAL_PESO_L,  VAR_TOTAL_GERAL_TARE, VAR_TOTAL_GERAL_VOLUM .
  "Ajuste impressão valor total usd #190822 - BG
  LOOP AT IT_CONTRATO ASSIGNING FIELD-SYMBOL(<WS_CONTRATO>).
    CLEAR: <WS_CONTRATO>-VALOR_CAD, <WS_CONTRATO>-VOLUM, <WS_CONTRATO>-GROSS_WEIGHT, <WS_CONTRATO>-TARE, <WS_CONTRATO>-NET_WEGHT, <WS_CONTRATO>-VALOR_ANTEC, <WS_CONTRATO>-VALOR_CAD,
    <WS_CONTRATO>-TOTAL_USS, ZVAR_QUANT_PESO_TOTAL, ZVAR_QUANT_PESO_LIQ, VAR_TOTAL_GERAL_ANTEC, VAR_TOTAL_GERAL_DIFE.
    LOOP AT IT_SAIDA_ALV_INVOICE ASSIGNING FIELD-SYMBOL(<WS_ITENS>) WHERE CONTRATO EQ <WS_CONTRATO>-CONTRATO.
      ADD <WS_ITENS>-VOLUM TO <WS_CONTRATO>-VOLUM.
      ADD <WS_ITENS>-GROSS_WEIGHT TO  <WS_CONTRATO>-GROSS_WEIGHT. "zvar_quant_peso_total.
      ADD <WS_ITENS>-NET_WEGHT TO <WS_CONTRATO>-NET_WEGHT. "zvar_quant_peso_liq.
      ADD <WS_ITENS>-VALOR_ANTEC TO <WS_CONTRATO>-VALOR_ANTEC.
      ADD <WS_ITENS>-VALOR_CAD TO <WS_CONTRATO>-VALOR_CAD.
      "Ajuste impressão valor total usd #190822 - BG - INICIO
      v_total_USD =  ( <WS_ITENS>-NET_WEGHT * <WS_ITENS>-PRICE_USS ).

      <WS_CONTRATO>-TOTAL_USS =  <WS_CONTRATO>-TOTAL_USS + v_total_USD.
      "Ajuste impressão valor total usd #190822 - BG - FIM
    ENDLOOP.

*    <ws_contrato>-gross_weight = zvar_quant_peso_total.
*    <ws_contrato>-net_weght    = zvar_quant_peso_liq.

    IF <WS_CONTRATO>-GROSS_WEIGHT > 0 AND <WS_CONTRATO>-NET_WEGHT > 0.
      <WS_CONTRATO>-TARE = ( <WS_CONTRATO>-GROSS_WEIGHT - <WS_CONTRATO>-NET_WEGHT )."( zvar_quant_peso_total - zvar_quant_peso_liq ).
      " <ws_contrato>-total_uss = ( <ws_contrato>-net_weght * <ws_contrato>-price_uss ).     "Ajuste impressão valor total usd #190822 - BG
    ENDIF.


    ADD <WS_CONTRATO>-TOTAL_USS    TO VAR_TOTAL_GERAL.
    ADD <WS_CONTRATO>-VALOR_ANTEC  TO VAR_TOTAL_GERAL_ANTEC.
    ADD <WS_CONTRATO>-GROSS_WEIGHT TO VAR_TOTAL_GERAL_PESO_B.
    ADD <WS_CONTRATO>-NET_WEGHT    TO VAR_TOTAL_GERAL_PESO_L.
    ADD <WS_CONTRATO>-TARE         TO VAR_TOTAL_GERAL_TARE  .
    ADD <WS_CONTRATO>-VOLUM        TO VAR_TOTAL_GERAL_VOLUM .
*    <ws_contrato>-valor_cad     = ( <ws_contrato>-total_uss - <ws_contrato>-valor_antec ).

  ENDLOOP.
  " Ajustes Impressão Invoice #189775 - BG - INICIO
  IF IT_CONTRATO[] IS NOT INITIAL.
    CONCATENATE 'TO:'<WS_CONTRATO>-NAME1 INTO V_TEXT_TO SEPARATED BY SPACE.
  ENDIF.
  " Ajustes Impressão Invoice #189775 - BG - FIM

  "Totalizar por contrato/provisional.
  SORT IT_TOTAL_PROVISIONAL BY CONTRATO NR_PROVISIONAL.
  DELETE ADJACENT DUPLICATES FROM IT_TOTAL_PROVISIONAL COMPARING CONTRATO NR_PROVISIONAL.
  LOOP AT IT_TOTAL_PROVISIONAL ASSIGNING FIELD-SYMBOL(<WS_PROVISIONAL>).
    CLEAR: <WS_PROVISIONAL>-VOLUM, <WS_PROVISIONAL>-GROSS_WEIGHT,
    <WS_PROVISIONAL>-TARE,
    <WS_PROVISIONAL>-NET_WEGHT,
    <WS_PROVISIONAL>-TOTAL_USS,
    <WS_PROVISIONAL>-VALOR_ANTEC,
    <WS_PROVISIONAL>-VALOR_CAD,
    ZVAR_QUANT_PESO_TOTAL,
    ZVAR_QUANT_PESO_LIQ,
    ZVAR_TOTAL.
    LOOP AT IT_SAIDA_ALV_INVOICE ASSIGNING <WS_ITENS> WHERE CONTRATO EQ <WS_PROVISIONAL>-CONTRATO AND NR_PROVISIONAL EQ <WS_PROVISIONAL>-NR_PROVISIONAL.
      <WS_ITENS>-TOTAL_USS = ( <WS_ITENS>-NET_WEGHT * <WS_ITENS>-PRICE_USS ). "Ajuste impressão valor total usd #190822 - BG
      ADD <WS_ITENS>-VALOR_ANTEC TO  <WS_PROVISIONAL>-VALOR_ANTEC.
      ADD <WS_ITENS>-TOTAL_USS TO  <WS_PROVISIONAL>-TOTAL_USS.
      ADD <WS_ITENS>-VALOR_CAD TO  <WS_PROVISIONAL>-VALOR_CAD.
    ENDLOOP.
    ADD <WS_PROVISIONAL>-VALOR_CAD TO VAR_TOTAL_GERAL_DIFE.
  ENDLOOP.


  "Totalizar lotes avariado.
  FREE: IT_DADOS_LOTE_AV.
  IT_DADOS_LOTE_AV = IT_SAIDA_ALV_SALDO_AVAR.
  DELETE ADJACENT DUPLICATES FROM IT_DADOS_LOTE_AV COMPARING NR_PROVISIONAL.
  LOOP AT IT_DADOS_LOTE_AV ASSIGNING FIELD-SYMBOL(<WA_dados>).
    CLEAR: <WA_dados>-VALOR_ANTEC, <WA_dados>-VALOR_CAD.
    LOOP AT IT_SAIDA_ALV_SALDO_AVAR ASSIGNING FIELD-SYMBOL(<WA_LOTE_AV>) WHERE NR_PROVISIONAL EQ <WA_dados>-NR_PROVISIONAL.
      ADD <WA_LOTE_AV>-VALOR_CAD TO <WA_dados>-VALOR_CAD.
    ENDLOOP.

    IF VAR_TOTAL_GERAL_DIFE IS NOT INITIAL AND <WA_dados>-VALOR_CAD IS NOT INITIAL.
      VAR_TOTAL_GERAL_DIFE = ( VAR_TOTAL_GERAL_DIFE - <WA_dados>-VALOR_CAD ).
    ENDIF.
  ENDLOOP.


  "Numero da invoice.
  V_TEXT_NR_INVOICE = |{ WS_HEADER_IVOICE-ID_INVOICE ALPHA = OUT }|.
  CONDENSE V_TEXT_NR_INVOICE NO-GAPS.
  V_TEXT_SAFRA      = |{ WS_HEADER_IVOICE-SAFRA+2(2) }|.
  CONDENSE V_TEXT_SAFRA NO-GAPS.
  V_NR_INVOICE = |INVOICE: NR. { V_TEXT_NR_INVOICE }/{ V_TEXT_SAFRA }|.

  "Converter data em ingles.
  PERFORM FM_TRANSL_DATE.

  CALL FUNCTION VL_NAME
    EXPORTING
      HEADER                  = WS_HEADER_IVOICE
      TA_TEXTO_GERAL_DINAMICO = T_TEXTO_GERAL
      V_TEXT_TO               = V_TEXT_TO
      V_NR_INVOICE            = V_NR_INVOICE
      V_DATE_TITLE            = V_DATE_TITLE
      V_TOTAL_GERAL           = VAR_TOTAL_GERAL
      V_TOTAL_GERAL_PESO_B    = VAR_TOTAL_GERAL_PESO_B
      V_TOTAL_GERAL_PESO_L    = VAR_TOTAL_GERAL_PESO_L
      V_TOTAL_GERAL_TARE      = VAR_TOTAL_GERAL_TARE
      V_TOTAL_GERAL_VOLUM     = VAR_TOTAL_GERAL_VOLUM
      V_TOTAL_GERAL_ANTEC     = VAR_TOTAL_GERAL_ANTEC
      V_TOTAL_GERAL_DIFE      = VAR_TOTAL_GERAL_DIFE
      V_DATE                  = VAR_DATE
      V_TEXTO_CREDITO         = VAR_TEXTO_CREDITO
    TABLES
      T_ITENS                 = IT_SAIDA_ALV_INVOICE
      T_CONTRATO              = IT_CONTRATO
      T_PROVISIONAL           = IT_TOTAL_PROVISIONAL
      T_DADOS_LOTE_AV         = IT_DADOS_LOTE_AV
    EXCEPTIONS
      FORMATTING_ERROR        = 1
      INTERNAL_ERROR          = 2
      SEND_ERROR              = 3
      USER_CANCELED           = 4
      OTHERS                  = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'S'      NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_transl_date
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_TRANSL_DATE .
  DATA: VAR_MES TYPE CHAR02,
        VAR_ANO TYPE CHAR04,
        VAR_DIA TYPE CHAR02.


  VAR_MES = |{ SY-DATUM+4(2) }|.
  VAR_ANO = |{ SY-DATUM+0(4) }|.
  VAR_DIA = |{ SY-DATUM+6(2) }|.



  CASE VAR_MES.
    WHEN '01'.
      V_DATE_TITLE = |Cuiabá, January { VAR_DIA }th, { VAR_ANO }|.
    WHEN '02'.
      V_DATE_TITLE = |Cuiabá, February { VAR_DIA }th, { VAR_ANO }|.
    WHEN '03'.
      V_DATE_TITLE = |Cuiabá, March { VAR_DIA }th, { VAR_ANO }|.
    WHEN '04'.
      V_DATE_TITLE = |Cuiabá, April { VAR_DIA }th { VAR_ANO }|.
    WHEN '05'.
      V_DATE_TITLE = |Cuiabá, May { VAR_DIA }th, { VAR_ANO }|.
    WHEN '06'.
      V_DATE_TITLE = |Cuiabá, June { VAR_DIA }th, { VAR_ANO }|.
    WHEN '07'.
      V_DATE_TITLE = |Cuiabá, July { VAR_DIA }th, { VAR_ANO }|.
    WHEN '08'.
      V_DATE_TITLE = |Cuiabá, August { VAR_DIA }th, { VAR_ANO }|.
    WHEN '09'.
      V_DATE_TITLE = |Cuiabá, September { VAR_DIA }th, { VAR_ANO }|.
    WHEN '10'.
      V_DATE_TITLE = |Cuiabá, October { VAR_DIA }th, { VAR_ANO }|.
    WHEN '11'.
      V_DATE_TITLE = |Cuiabá, November { VAR_DIA }th, { VAR_ANO }|.
    WHEN '12'.
      V_DATE_TITLE = |Cuiabá, December { VAR_DIA }th, { VAR_ANO }|.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_fieldcatalog_saldo_av
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_FIELDCATALOG_SALDO_AV .

  DATA: LC_COL_POS TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  FREE: IT_FIELDCAT_AV.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZSDE0099'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCAT_AV.


  IF ZVAR_USER IS INITIAL.
    IF WS_HEADER_IVOICE-DATA_RECEB IS NOT INITIAL.
      VAR_BLOCK = ''.
    ELSE.
      VAR_BLOCK = 'X'.
    ENDIF.
  ELSE.
    VAR_BLOCK = 'X'.
  ENDIF.

  LOOP AT IT_FIELDCAT_AV ASSIGNING FIELD-SYMBOL(<FS_FCAT>).
    CLEAR: <FS_FCAT>-SCRTEXT_L, <FS_FCAT>-SCRTEXT_M, <FS_FCAT>-SCRTEXT_S.
    CASE <FS_FCAT>-FIELDNAME.
      WHEN 'CONTRATO'.
        <FS_FCAT>-COLTEXT  = 'Contrato'.
      WHEN 'CONTRATO_REF'.
        <FS_FCAT>-COLTEXT  = 'Contrato Ref'.
        <FS_FCAT>-EDIT     = VAR_BLOCK.
      WHEN 'NR_PROVISIONAL'.
        <FS_FCAT>-COLTEXT  = 'PI'.
      WHEN 'NR_PROVISIONAL_REF'.
        <FS_FCAT>-COLTEXT  = 'PI Ref'.
        <FS_FCAT>-EDIT     = VAR_BLOCK.
*        <fs_fcat>-edit    = abap_true.
*        <fs_fcat>-f4availabl = abap_true.          " Habilita o F4 no campo
      WHEN 'CHARG'.
*        <fs_fcat>-edit    = abap_true.
      WHEN 'VALOR_ANTEC'.
        <FS_FCAT>-COLTEXT = 'PI'.
*        <fs_fcat>-edit    = abap_true.
        <FS_FCAT>-DO_SUM  = ABAP_TRUE.
      WHEN 'VALOR_CAD'.
        <FS_FCAT>-COLTEXT = 'Utilização PI'.
        <FS_FCAT>-EDIT    = VAR_BLOCK.
        <FS_FCAT>-DO_SUM  = ABAP_TRUE.
      WHEN 'OBSERVACAO'.
        <FS_FCAT>-EDIT    = ABAP_TRUE.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form excluir_botoes
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- IT_EXCLUDE
*&---------------------------------------------------------------------*
FORM EXCLUIR_BOTOES  CHANGING P_IT_EXCLUDE TYPE UI_FUNCTIONS.

  DATA: LS_EXCLUDE     TYPE UI_FUNC.

  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_PRINT.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_CHECK.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_GRAPH.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_INFO.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form excluir_botoes_av
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- IT_EXCLUDE_AV
*&---------------------------------------------------------------------*
FORM EXCLUIR_BOTOES_AV  CHANGING P_IT_EXCLUDE TYPE UI_FUNCTIONS.

  DATA: LS_EXCLUDE     TYPE UI_FUNC.

  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
*  if ws_header_ivoice-data_receb is not initial and ws_header_ivoice-valor_receb is not initial.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
*  endif.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_PRINT.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_CHECK.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_GRAPH.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_INFO.
  APPEND LS_EXCLUDE TO P_IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_fieldcatalog_ABAT_PI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_fieldcatalog_ABAT_PI .
  DATA: LC_COL_POS TYPE LVC_COLPOS,
        RG_FIELD   TYPE RANGE OF LVC_S_FCAT-FIELDNAME.

  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  FREE: IT_FIELDCATALOG.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZSDE0101'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG.

  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = 'VOLUM') TO RG_FIELD.
  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = 'TOTAL_PI') TO RG_FIELD.
  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = 'VALOR_CAD_PI') TO RG_FIELD.

  DELETE IT_FIELDCATALOG WHERE FIELDNAME IN RG_FIELD.

  LOOP AT IT_FIELDCATALOG ASSIGNING FIELD-SYMBOL(<FS_FCAT>).
    CLEAR: <FS_FCAT>-SCRTEXT_L, <FS_FCAT>-SCRTEXT_M, <FS_FCAT>-SCRTEXT_S.
    CASE <FS_FCAT>-FIELDNAME.
      WHEN 'NAME1'.
        <FS_FCAT>-COLTEXT = 'Nome do Cliente'.
      WHEN 'ID_INVOICE' .
        <FS_FCAT>-COLTEXT = 'Nr Invoice'.
      WHEN 'NR_PROVISIONAL'.
        <FS_FCAT>-COLTEXT = 'Nº Provisional'.
        <FS_FCAT>-SP_GROUP = ABAP_TRUE.
      WHEN 'INSTRUCAO'.
        <FS_FCAT>-COLTEXT = 'Instrução'.
      WHEN 'CONTRATO'.
        <FS_FCAT>-COLTEXT = 'Contrato'.
      WHEN 'CONTRATO_REF'.
        <FS_FCAT>-COLTEXT = 'Contrato Ref'.
      WHEN 'VOLUM'.
        <FS_FCAT>-COLTEXT = 'Volume PI'. "X
      WHEN 'VOLUM_INV'.
        <FS_FCAT>-COLTEXT = 'Volume Invoice'.
      WHEN 'TOTAL_PI'.
        <FS_FCAT>-COLTEXT = 'Total PI'. "X
        <FS_FCAT>-DO_SUM  = ABAP_TRUE.
      WHEN 'TOTAL_INV'.
        <FS_FCAT>-COLTEXT = 'Total Invoice'.
*        <fs_fcat>-do_sum  = abap_true.
      WHEN 'VALOR_ANTEC_PI'.
        <FS_FCAT>-COLTEXT = 'Antecipação PI'.
*        <fs_fcat>-do_sum  = abap_true.
      WHEN 'VALOR_ANTEC_INV'.
        <FS_FCAT>-COLTEXT = 'Antecipação Invoice'.
        <FS_FCAT>-DO_SUM  = ABAP_TRUE.
      WHEN 'VALOR_CAD_PI'.
        <FS_FCAT>-COLTEXT = 'Valor CAD PI'. "X
        <FS_FCAT>-DO_SUM  = ABAP_TRUE.
      WHEN 'VALOR_CAD_INV'.
        <FS_FCAT>-COLTEXT = 'Valor CAD Invoice'.
        <FS_FCAT>-DO_SUM  = ABAP_TRUE.
      WHEN 'UTIL_PI'.
        <FS_FCAT>-COLTEXT = 'Utilização PI'.
        <FS_FCAT>-DO_SUM  = ABAP_TRUE.
      WHEN 'VALOR_CAD_FINAL'.
        <FS_FCAT>-COLTEXT = 'Valor CAD Final'.
        <FS_FCAT>-DO_SUM  = ABAP_TRUE.
      WHEN 'DIFERENCA_CAD'.
        <FS_FCAT>-COLTEXT = 'Saldo PI'.
      WHEN 'NR_PROVISIONAL_REF'.
        <FS_FCAT>-COLTEXT = 'Nº Provisional Ref'.
        <FS_FCAT>-NO_ZERO = ABAP_TRUE.
      WHEN 'VALOR_RECEB'.
*        <fs_fcat>-edit  = abap_true.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_excluir_invoice
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_EXCLUIR_INVOICE .

  CALL METHOD G_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS.

  DESCRIBE TABLE IT_SELECTED_ROWS LINES LINES.

  IF ( LINES IS INITIAL ).
    MESSAGE TEXT-E01 TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
      READ TABLE IT_SAIDA_INVOICE INTO DATA(WA_SAIDA_INVOICE) INDEX WA_SELECTED_ROWS-INDEX.
      IF SY-SUBRC EQ 0 AND WA_SAIDA_INVOICE-ID_INVOICE IS NOT INITIAL AND WA_SAIDA_INVOICE-DATA_RECEB IS INITIAL.
        DELETE FROM ZSDT0399 WHERE ID_INVOICE EQ WA_SAIDA_INVOICE-ID_INVOICE.
        DELETE FROM ZSDT0400 WHERE ID_INVOICE EQ WA_SAIDA_INVOICE-ID_INVOICE.
        DELETE FROM ZSDT0404 WHERE ID_INVOICE EQ WA_SAIDA_INVOICE-ID_INVOICE.
      ELSE.
        MESSAGE I024(SD) WITH 'Não é permitido excluir o documento' 'invoice selecionado.'.
      ENDIF.
    ENDLOOP.
    IT_SAIDA_INVOICE = OBJ_IVOICE->GET_DADOS_INVOICE( IT_PARAM ).
    CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY( IS_STABLE = W_STABLE ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module TRATA_FIELDS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE TRATA_FIELDS OUTPUT.
  IF ZVAR_USER IS INITIAL.
    IF WS_HEADER_IVOICE-DATA_RECEB IS NOT INITIAL.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 EQ 'GR1'.
          SCREEN-INPUT     = 0. "
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
      CLEAR: VAR_BLOCK.
    ELSE.
      VAR_BLOCK = 'X'.
    ENDIF.
  ELSE.
    VAR_BLOCK = 'X'.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form fm_excluir
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_EXCLUIR .

  DESCRIBE TABLE T_ROWS[] LINES LINES.

  IF ( LINES IS INITIAL ).
    MESSAGE TEXT-E01 TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    LOOP AT T_ROWS INTO WA_SELECTED_ROWS.
      READ TABLE IT_SAIDA_ALV_SALDO_AVAR INTO DATA(WA_SALDO_AVAR) INDEX WA_SELECTED_ROWS-INDEX.
      DELETE IT_SAIDA_ALV_SALDO_AVAR INDEX WA_SELECTED_ROWS-INDEX.
      IF SY-SUBRC EQ 0.
        DELETE FROM ZSDT0404 WHERE CONTRATO EQ WA_SALDO_AVAR-CONTRATO
                              AND  ID_INVOICE EQ WS_HEADER_IVOICE-ID_INVOICE
                              AND  NR_PROVISIONAL  EQ WA_SALDO_AVAR-NR_PROVISIONAL
                              AND  CHARG           EQ WA_SALDO_AVAR-CHARG.
      ENDIF.
    ENDLOOP.

    CALL METHOD G_GRID_AV->REFRESH_TABLE_DISPLAY( IS_STABLE = W_STABLE ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_sort_abat_pi
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_SORT_ABAT_PI .

  CLEAR: LS_SORT. FREE: IT_SORT.
  LS_SORT-FIELDNAME = 'NR_PROVISIONAL'.
  LS_SORT-UP        = 'X'.
  LS_SORT-SUBTOT    = 'X'.
  APPEND LS_SORT TO LT_SORT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_check_user
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FM_CHECK_USER .
  CLEAR: ZVAR_USER.
  FREE: _PARAM.
  "check perfil user.
* // pega os parametros do usuario
  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      USER_NAME           = SY-UNAME
    TABLES
      USER_PARAMETERS     = _PARAM
    EXCEPTIONS
      USER_NAME_NOT_EXIST = 1
      OTHERS              = 2.

  "Check parametro ZCANC_EXTEMP_INT existente no perfil usuario. zpar_edit_invoice
  IF _PARAM[] IS NOT INITIAL.
    READ TABLE _PARAM INTO DATA(LS_PARAM) WITH KEY PARID = 'ZPAR_EDIT_INVOICE'.
    IF SY-SUBRC EQ 0.
      ZVAR_USER = ABAP_TRUE.
    ENDIF.
  ENDIF.
ENDFORM.
