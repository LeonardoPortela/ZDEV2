*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 06/01/2014                                              &*
*& Descrição: Solicitação de Cad. de Fornec. e Clientes               &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descricao                &*
*& ABAP                         03.08.2010                            &*
*& Marcos Faneli   DEVK937122   29.04.2014                            &*
*&--------------------------------------------------------------------&*

REPORT  ZMMR044.
INCLUDE <ICON>.
INCLUDE <CL_ALV_CONTROL>.
TYPE-POOLS: VRM, USTYP, SLIS, F4TYP.
*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
TYPES: BEGIN OF TY_BANCARIO,
       BANKS TYPE ZMMT0052-BANKS,
       BANKL TYPE ZMMT0052-BANKL,
       BANKN TYPE ZMMT0052-BANKN,
       BKONT TYPE ZMMT0052-BKONT,
       BKONTT(20),
       BVTYP TYPE ZMMT0052-BVTYP,
       IBAN  TYPE ZMMT0052-IBAN,
       BANKA TYPE ZMMT0052-BANKA,
       BANKK TYPE ZMMT0052-BANKK,
      END OF TY_BANCARIO,

      BEGIN OF TY_IMPOSTOS,
       QLAND     TYPE ZMMT0053-QLAND,
       WITHT     TYPE ZMMT0053-WITHT,
       WT_WITHCD TYPE ZMMT0053-WT_WITHCD,
       WT_SUBJCT TYPE ZMMT0053-WT_SUBJCT,
      END OF TY_IMPOSTOS.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

DATA: BEGIN OF GT_VALUES OCCURS 0,
        DOMVALUE_L TYPE DOMVALUE_L,
        DDTEXT TYPE VAL_TEXT,
      END OF GT_VALUES.

*Class definition for ALV toolbar
CLASS:      LCL_ALV_TOOLBAR   DEFINITION DEFERRED.

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: G_CUSTOM_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CONTAINER1            TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CONTAINER2            TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GRID1                 TYPE REF TO CL_GUI_ALV_GRID,
      GRID2                 TYPE REF TO CL_GUI_ALV_GRID,
      OBG_TOOLBAR           TYPE REF TO LCL_ALV_TOOLBAR,
      C_ALV_TOOLBARMANAGER  TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      OBG_MANAGER           TYPE REF TO CL_GOS_MANAGER,
      WG_OBJ                TYPE BORIDENT.

*Declaration for toolbar buttons
DATA : TY_TOOLBAR TYPE STB_BUTTON.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: C_X           TYPE C VALUE 'X',
           C_C           TYPE C VALUE 'C',
           C_F           TYPE C VALUE 'F',
           C_R           TYPE C VALUE 'R',
           C_ADD(3)      TYPE C VALUE 'ADD',
           C_DEL(3)      TYPE C VALUE 'DEL',
           C_EXIT(4)     TYPE C VALUE 'EXIT',
           C_BACK(4)     TYPE C VALUE 'BACK',
           C_COPY(4)     TYPE C VALUE 'COPY',
           C_SAVE(4)     TYPE C VALUE 'SAVE',
           C_BLOQ(4)     TYPE C VALUE 'BLOQ',
*           c_gerar(5)    type c value 'GERAR',
           C_ATUAL(5)    TYPE C VALUE 'ATUAL',
           C_GOS(5)      TYPE C VALUE 'ZGOS2',
*           c_print(5)    type c value 'PRINT',
           C_APROV(5)       TYPE C VALUE 'APROV',
           C_MODIF(5)       TYPE C VALUE 'MODIF',
           C_SEARCH(6)      TYPE C VALUE 'SEARCH',
           C_CANCEL(6)      TYPE C VALUE 'CANCEL',
           C_TIPO_PES(8)    TYPE C VALUE 'TIPO_PES',
           C_SHOW_MSGRE(10) TYPE C VALUE 'SHOW_MSGRE'.
*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
*** Declaracoes referente a logica do programa ***
DATA: WG_HEADER      TYPE ZMMT0049,
      WG_FISCAIS     TYPE ZMMT0051,
      WG_CONTAB_PGTO TYPE ZMMT0054,
      WG_COMPRA      TYPE ZMMT0055,
      WG_VENDA       TYPE ZMMT0056,
      WG_ENDEREC_C   TYPE ZMMT0050,
      WG_ENDEREC_R   TYPE ZMMT0050,
      TG_BANCARIO    TYPE TABLE OF TY_BANCARIO WITH HEADER LINE,
      TG_IMPOSTOS    TYPE TABLE OF TY_IMPOSTOS WITH HEADER LINE,
      WG_DESC_BUKRS(20),
      WG_DESC_EKORG(20),
      WG_DESC_VTWEG(20),
      WG_DESC_SPART(27),
      WG_DESC_FDGRV(30),
      WG_DESC_ZAHLS(20),
      WG_DESC_ZGRUP(30),
      WG_DESC_ZTERM(23),
      WG_DESC_ZTERM2(23),
      WG_DESC_KALKS(23),
      WG_DESC_VERSG(23),
      WG_DESC_KTGRD(23),
      WG_DESC_KTOKK(53).


*** Declaracoes referente ao template do programa ***
DATA: OK-CODE     TYPE SY-UCOMM,
      WG_DISPLAY,
      WG_ACAO(10),
      WG_FLAG,
      TG_SELECTEDCELL       TYPE LVC_T_CELL,
      WG_SELECTEDCELL       TYPE LVC_S_CELL.

***** Funcao de Z_DOC_CHECK_NEW
DATA: X_FIELD(30),
      WG_MENSAGEM(30).
DATA: TG_MSG_RET TYPE TABLE OF ZFIWRS0002 WITH HEADER LINE,
      WG_CELL TYPE LVC_S_CELL,
      TG_CELL TYPE LVC_T_CELL.

** Criação de tabela dinamica
DATA: T_FIELDCATALOG        TYPE LVC_T_FCAT,
      W_FIELDCATALOG        TYPE LVC_S_FCAT,
      WA_LAYOUT             TYPE LVC_S_LAYO,
      WA_STABLE             TYPE LVC_S_STBL.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_STRIP_NF'
CONSTANTS: BEGIN OF C_TAB_STRIP,
             TAB1 LIKE SY-UCOMM VALUE 'TAB_STRIP_FC1',
             TAB2 LIKE SY-UCOMM VALUE 'TAB_STRIP_FC2',
             TAB3 LIKE SY-UCOMM VALUE 'TAB_STRIP_FC3',
             TAB4 LIKE SY-UCOMM VALUE 'TAB_STRIP_FC4',
             TAB5 LIKE SY-UCOMM VALUE 'TAB_STRIP_FC5',
             TAB6 LIKE SY-UCOMM VALUE 'TAB_STRIP_FC6',
             TAB7 LIKE SY-UCOMM VALUE 'TAB_STRIP_FC7',
             TAB8 LIKE SY-UCOMM VALUE 'TAB_STRIP_FC8',
           END OF C_TAB_STRIP.

CONTROLS:  TAB_STRIP TYPE TABSTRIP.
DATA:      BEGIN OF G_TAB_STRIP,
             SUBSCREEN   LIKE SY-DYNNR,
             PROG        LIKE SY-REPID VALUE 'ZMMR044',
             PRESSED_TAB LIKE SY-UCOMM VALUE C_TAB_STRIP-TAB1,
           END OF G_TAB_STRIP.
DATA:      OK_CODE LIKE SY-UCOMM.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID VALUE SY-REPID.
*&--------------------------------------------------------------------&*
*& Inicialization                                                     &*
*&--------------------------------------------------------------------&*

CALL SCREEN 100.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS LCL_EVENT_HANDLER DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
                      IMPORTING E_ROW E_COLUMN,

     ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
                     IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM,

     ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
                              IMPORTING E_MODIFIED ET_GOOD_CELLS,

     ON_ONF4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID
             IMPORTING E_FIELDNAME E_FIELDVALUE ES_ROW_NO ER_EVENT_DATA ET_BAD_CELLS E_DISPLAY.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: CONSTRUCTOR
                IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
*Event for toolbar
             ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
               IMPORTING  E_OBJECT,

             HANDLE_USER_COMMAND_BANC FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
               IMPORTING E_UCOMM,

             HANDLE_USER_COMMAND_IMP FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
               IMPORTING E_UCOMM.

ENDCLASS.                    "lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor
  METHOD ON_TOOLBAR.
    DATA: WL_DESACTIVE.
*    break-point.
*   Add customized toolbar buttons.
*    IF wg_docs-docnum IS INITIAL.
*      READ TABLE tg_fields TRANSPORTING NO FIELDS
*        WITH KEY group1 = 'GR1'.
*      IF sy-subrc IS INITIAL.
*        IF wg_fiscal-retorno EQ 'S'.
*          wl_desactive = 1.
*        ELSE.
*          wl_desactive = space.
*        ENDIF.
*      ELSE.
*        wl_desactive = 1.
*      ENDIF.
*    ELSE.
*      wl_desactive = 1.
*    ENDIF.

    IF WG_ACAO EQ C_ADD
    OR WG_ACAO EQ C_MODIF.
      WL_DESACTIVE =  SPACE.

    ELSE.
      WL_DESACTIVE = 1.
    ENDIF.

    TY_TOOLBAR-ICON      =  ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION  =  C_ADD.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

*    IF wg_docs-docnum IS INITIAL.
*      READ TABLE tg_fields TRANSPORTING NO FIELDS
*        WITH KEY group1 = 'GR1'.
*      IF sy-subrc IS INITIAL.
*        wl_desactive = space.
*      ELSE.
*        wl_desactive = 1.
*      ENDIF.
*    ELSE.
*      wl_desactive = 1.
*    ENDIF.
    TY_TOOLBAR-ICON      =  ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION  =  C_DEL.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.
*   variable for Toolbar Button
*    ty_toolbar-icon      =  icon_view_close.
*    ty_toolbar-function  =  c_clos_msg.
*    ty_toolbar-disabled  = space.
*    ty_toolbar-butn_type = 0.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*    CLEAR ty_toolbar.
**   Call reorganize method of toolbar manager to
**   display the toolbar
    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.
  ENDMETHOD.                    "on_toolbar
  METHOD HANDLE_USER_COMMAND_BANC.
*    DATA: tl_itens_aux LIKE TABLE OF tg_itens,
*          wl_itens LIKE LINE OF tg_itens,
*          wl_lines TYPE sy-tabix.
*    REFRESH: tl_itens_aux.
**   User Command Botões Incluidos
**    break abap.
    CASE E_UCOMM.
      WHEN C_ADD.
        APPEND INITIAL LINE TO TG_BANCARIO.
        CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.
**        tl_itens_aux[] = tg_itens[].
**        REFRESH: tg_itens.
**        LOOP AT tl_itens_aux INTO wl_itens.
**          wl_itens-itmnum = sy-tabix * 10.
**          APPEND wl_itens TO tg_itens.
**        ENDLOOP.
**        DESCRIBE TABLE tg_itens LINES wl_lines.
**        CLEAR: wl_itens.
**        wl_itens-itmnum = ( wl_lines + 1 ) * 10 .
**        APPEND wl_itens TO tg_itens.
*
*        CALL METHOD grid1->refresh_table_display
*          EXPORTING
*            is_stable = wa_stable.
      WHEN C_DEL.
        CALL METHOD GRID1->GET_SELECTED_CELLS
          IMPORTING
            ET_CELL = TG_SELECTEDCELL.

        LOOP AT TG_SELECTEDCELL INTO WG_SELECTEDCELL.
          DELETE TG_BANCARIO INDEX WG_SELECTEDCELL-ROW_ID-INDEX.

        ENDLOOP.

        CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.
    ENDCASE.
*  ENDIF.
  ENDMETHOD.                    "HANDLE_USER_COMMAND_BANC
  METHOD HANDLE_USER_COMMAND_IMP.
    CASE E_UCOMM.
      WHEN C_ADD.
        APPEND INITIAL LINE TO TG_IMPOSTOS.
        CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.
      WHEN C_DEL.
        CALL METHOD GRID2->GET_SELECTED_CELLS
          IMPORTING
            ET_CELL = TG_SELECTEDCELL.

        LOOP AT TG_SELECTEDCELL INTO WG_SELECTEDCELL.
          DELETE TG_IMPOSTOS INDEX WG_SELECTEDCELL-ROW_ID-INDEX.

        ENDLOOP.
        CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.
    ENDCASE.
  ENDMETHOD.                    "HANDLE_USER_COMMAND_IMP

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD ON_DOUBLE_CLICK.
  ENDMETHOD.                    "ON_DOUBLE_CLICK
  METHOD ON_DATA_CHANGED.
*    DATA: LS_GOOD  TYPE LVC_S_MODI,
*          LV_VALUE TYPE LVC_VALUE,
*          VL_VALUE TYPE LVC_VALUE.
*
*    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
*                             INTO LS_GOOD
*                             WHERE FIELDNAME = 'BKONT'.
*      LV_VALUE = LS_GOOD-VALUE.
*      CONDENSE LV_VALUE NO-GAPS.
*
*      CASE TG_BANCARIO-BKONTT.
*        WHEN 'CONTA CORRENTE'.
*          LV_VALUE = '01'.
*        WHEN 'POUPANÇA'.
*          LV_VALUE = '02'.
*        WHEN 'CONTA SALÁRIO'.
*          LV_VALUE = '03'.
*        WHEN OTHERS.
*      ENDCASE.
*
*      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*        EXPORTING
*          I_ROW_ID    = LS_GOOD-ROW_ID
*          I_FIELDNAME = 'BKONT'
*          I_VALUE     = LV_VALUE.
*
*    ENDLOOP.
  ENDMETHOD.                    "ON_DATA_CHANGED
  METHOD ON_DATA_CHANGED_FINISHED.
  ENDMETHOD.                    "on_data_changed_finisheD

  METHOD ON_ONF4.
    TYPES:  BEGIN OF TY_FIELD,
              TABNAME     TYPE DD03L-TABNAME,     "Nome da tabela
              FIELDNAME   TYPE DD03L-FIELDNAME,   "Nome de campo
              S(1)        TYPE C,
            END OF TY_FIELD,

            BEGIN OF TY_VALUE,
              TABNAME     TYPE DD03L-TABNAME,     "Nome da tabela
              FIELDNAME   TYPE DD03L-FIELDNAME,   "Nome de campo
              CHAR79(79)  TYPE C,
            END OF TY_VALUE.

    DATA:   BEGIN OF WL_VALUETAB,
                  FIELD(50),
            END OF WL_VALUETAB.

    DATA:   TL_VALUETAB       LIKE TABLE OF WL_VALUETAB,
            TL_FIELD          TYPE TABLE OF TY_FIELD,
            WL_FIELD          TYPE TY_FIELD,
            TL_VALUE          TYPE TABLE OF TY_VALUE,
            WL_VALUE          TYPE TY_VALUE,
            WG_ITENS          LIKE LINE OF TG_BANCARIO,
            WL_INDEX          TYPE SY-TABIX,
            WL_CHAR(20),
            WL_FIELDNAME(30),
            WL_TABNAME(30),
*            IT_BNKA TYPE TABLE OF TY_BANCARIO,
            IT_BNKA TYPE TABLE OF BNKA,
*            WL_BNKA TYPE TY_BANCARIO.
            WL_BNKA TYPE BNKA.

    READ TABLE TG_BANCARIO INTO WG_ITENS INDEX ES_ROW_NO-ROW_ID.

    CASE E_FIELDNAME.
      WHEN 'BANKK'.
        SELECT *
          FROM BNKA
          INTO CORRESPONDING FIELDS OF TABLE IT_BNKA
          WHERE BANKS = WG_ITENS-BANKS.

        WL_FIELDNAME  = 'BANKL'.
        WL_TABNAME    = 'BNKA'.

        LOOP AT IT_BNKA INTO WL_BNKA.
          MOVE: WL_BNKA-BANKL TO WL_VALUETAB-FIELD.
          APPEND WL_VALUETAB TO TL_VALUETAB.

          MOVE: WL_BNKA-BANKA TO WL_VALUETAB-FIELD.
          APPEND WL_VALUETAB TO TL_VALUETAB.

        ENDLOOP.

        WL_FIELD-TABNAME = WL_TABNAME.
        WL_FIELD-FIELDNAME = 'BANKL'.
        WL_FIELD-S = 'X'.
        APPEND WL_FIELD TO TL_FIELD.

        WL_FIELD-TABNAME = WL_TABNAME.
        WL_FIELD-FIELDNAME = 'BANKA'.
        WL_FIELD-S = ' '.
        APPEND WL_FIELD TO TL_FIELD.
    ENDCASE.

    IF WL_FIELDNAME  IS NOT INITIAL
      AND WL_TABNAME    IS NOT INITIAL
      AND TL_FIELD[]    IS NOT INITIAL
      AND TL_VALUETAB[] IS NOT INITIAL.

      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
*        cucol                     = '3'
          FIELDNAME                 = WL_FIELDNAME
          TABNAME                   = WL_TABNAME
        IMPORTING
          INDEX                     = WL_INDEX
          SELECT_VALUE              = WL_CHAR
        TABLES
          FIELDS                    = TL_FIELD
          SELECT_VALUES             = TL_VALUE
          VALUETAB                  = TL_VALUETAB
        EXCEPTIONS
          FIELD_NOT_IN_DDIC         = 001
          MORE_THEN_ONE_SELECTFIELD = 002
          NO_SELECTFIELD            = 003.

      IF SY-SUBRC IS INITIAL.
        CASE E_FIELDNAME.
          WHEN 'BANKK'.
            READ TABLE IT_BNKA INTO WL_BNKA INDEX WL_INDEX.
        ENDCASE.

        IF ES_ROW_NO-ROW_ID GT 0.
          READ TABLE TG_BANCARIO INTO WG_ITENS INDEX ES_ROW_NO-ROW_ID.
          IF SY-SUBRC IS INITIAL.
            CASE E_FIELDNAME.
              WHEN 'BANKK'.
                MOVE: WL_BNKA-BANKL TO WG_ITENS-BANKK,
                      WL_BNKA-BANKA TO WG_ITENS-BANKA,
                      WL_BNKA-BNKLZ TO WG_ITENS-BANKL.
            ENDCASE.
            MODIFY TG_BANCARIO FROM WG_ITENS INDEX ES_ROW_NO-ROW_ID TRANSPORTING BANKK BANKA BANKL.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

**** Método de atualização de dados na Tela
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDMETHOD.                    "ON_ONF4
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
*&SPWIZARD: OUTPUT MODULE FOR TS 'TAB_STRIP_NF'. DO NOT CHANGE THIS LINE
*&SPWIZARD: SETS ACTIVE TAB
MODULE TAB_STRIP_ACTIVE_TAB_SET OUTPUT.
  PERFORM VERIFICA_ERROS.
  TAB_STRIP-ACTIVETAB = G_TAB_STRIP-PRESSED_TAB.
  CASE G_TAB_STRIP-PRESSED_TAB.
    WHEN C_TAB_STRIP-TAB1.
      G_TAB_STRIP-SUBSCREEN = '0101'.
    WHEN C_TAB_STRIP-TAB2.
      G_TAB_STRIP-SUBSCREEN = '0102'.
    WHEN C_TAB_STRIP-TAB3.
      G_TAB_STRIP-SUBSCREEN = '0103'.
    WHEN C_TAB_STRIP-TAB4.
      G_TAB_STRIP-SUBSCREEN = '0104'.
    WHEN C_TAB_STRIP-TAB5.
      G_TAB_STRIP-SUBSCREEN = '0105'.
    WHEN C_TAB_STRIP-TAB6.
      G_TAB_STRIP-SUBSCREEN = '0106'.
    WHEN C_TAB_STRIP-TAB7.
      G_TAB_STRIP-SUBSCREEN = '0107'.
    WHEN C_TAB_STRIP-TAB8.
      G_TAB_STRIP-SUBSCREEN = '0108'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "TAB_STRIP_NF_ACTIVE_TAB_SET OUTPUT

*&SPWIZARD: INPUT MODULE FOR TS 'TAB_STRIP_NF'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE TAB_STRIP_ACTIVE_TAB_GET INPUT.
  OK_CODE = SY-UCOMM.
  CASE OK_CODE.
    WHEN C_TAB_STRIP-TAB1.
      G_TAB_STRIP-PRESSED_TAB = C_TAB_STRIP-TAB1.
    WHEN C_TAB_STRIP-TAB2.
      G_TAB_STRIP-PRESSED_TAB = C_TAB_STRIP-TAB2.
    WHEN C_TAB_STRIP-TAB3.
      G_TAB_STRIP-PRESSED_TAB = C_TAB_STRIP-TAB3.
    WHEN C_TAB_STRIP-TAB4.
      G_TAB_STRIP-PRESSED_TAB = C_TAB_STRIP-TAB4.
    WHEN C_TAB_STRIP-TAB5.
      G_TAB_STRIP-PRESSED_TAB = C_TAB_STRIP-TAB5.
    WHEN C_TAB_STRIP-TAB6.
      G_TAB_STRIP-PRESSED_TAB = C_TAB_STRIP-TAB6.
    WHEN C_TAB_STRIP-TAB7.
      G_TAB_STRIP-PRESSED_TAB = C_TAB_STRIP-TAB7.
    WHEN C_TAB_STRIP-TAB8.
      G_TAB_STRIP-PRESSED_TAB = C_TAB_STRIP-TAB8.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "TAB_STRIP_NF_ACTIVE_TAB_GET INPUT
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VERIFICA_ERROS .
  DATA: WL_T001      TYPE T001,
        WL_T024E     TYPE T024E,
        WL_TVTW      TYPE TVTW,
        WL_TSPA      TYPE TSPA,
        WL_T005S_C   TYPE T005S,
        WL_T005S_R   TYPE T005S,
        WL_T005_C    TYPE T005,
        WL_T005_R    TYPE T005,
        WL_T005_BANK TYPE T005,
        WL_T035      TYPE T035,
        WL_T008      TYPE T008,
        WL_TZGR      TYPE TZGR,
        WL_TCURC     TYPE TCURC,
        WL_TINC      TYPE TINC,
        WL_TVKD      TYPE TVKD,
        WL_TVSG      TYPE TVSG,
        WL_TVKT      TYPE TVKT,
        WL_TVZB      TYPE TVZB,
        WL_TVZB_C    TYPE TVZB,
        WL_T005Q     TYPE T005Q,
        WL_LINHA(4).

  CLEAR: WL_T001, WL_T024E, WL_TVTW, WL_TSPA, WL_T005S_C, WL_T005S_R, WL_T005_C, WL_T005_R,
         WL_T035, WL_T008, WL_TZGR,  WL_TCURC, WL_TINC, WL_TVKD, WL_TVSG, WL_TVKT, WL_TVZB,
         WL_TVZB_C, WL_T005_BANK, WL_LINHA.

  REFRESH: TG_MSG_RET.

  SELECT SINGLE *
    FROM T001
      INTO WL_T001
        WHERE BUKRS = WG_HEADER-BUKRS.

  SELECT SINGLE *
    FROM T024E
      INTO WL_T024E
         WHERE EKORG = WG_HEADER-EKORG.

  SELECT SINGLE *
    FROM TVTW
      INTO WL_TVTW
        WHERE VTWEG = WG_HEADER-VTWEG.

  SELECT SINGLE *
    FROM TSPA
      INTO WL_TSPA
        WHERE SPART = WG_HEADER-SPART.

  SELECT SINGLE *
    FROM T005S
      INTO WL_T005S_C
        WHERE BLAND = WG_ENDEREC_C-REGION.

  SELECT SINGLE *
    FROM T005
      INTO WL_T005_C
        WHERE LAND1 = WG_ENDEREC_C-COUNTRY.

  SELECT SINGLE *
    FROM T005S
      INTO WL_T005S_R
        WHERE BLAND = WG_ENDEREC_R-REGION.

  SELECT SINGLE *
    FROM T005
      INTO WL_T005_R
        WHERE LAND1 = WG_ENDEREC_R-COUNTRY.


  SELECT SINGLE *
    FROM T005
      INTO WL_T005_BANK
        WHERE LAND1 = TG_BANCARIO-BANKS.

  SELECT SINGLE *
    FROM T005Q
      INTO WL_T005Q
        WHERE LAND1 = TG_IMPOSTOS-QLAND.

  SELECT SINGLE *
    FROM T035
      INTO WL_T035
        WHERE GRUPP = WG_CONTAB_PGTO-FDGRV.

  SELECT SINGLE *
    FROM TVZB
      INTO WL_TVZB
        WHERE ZTERM = WG_CONTAB_PGTO-ZTERM.

  SELECT SINGLE *
    FROM T008
      INTO WL_T008
        WHERE ZAHLS = WG_CONTAB_PGTO-ZAHLS.

  SELECT SINGLE *
    FROM TZGR
      INTO WL_TZGR
        WHERE ZGRUP = WG_CONTAB_PGTO-ZGRUP.

  IF WG_HEADER-TIPO EQ 'F'.
    SELECT SINGLE *
      FROM TCURC
        INTO WL_TCURC
          WHERE WAERS = WG_COMPRA-WAERS.

    SELECT SINGLE *
      FROM TVZB
        INTO WL_TVZB_C
          WHERE ZTERM = WG_COMPRA-ZTERM.

    SELECT SINGLE *
      FROM TINC
        INTO WL_TINC
          WHERE INCO1 = WG_COMPRA-INCO1.

  ELSEIF WG_HEADER-TIPO EQ 'C'.
    SELECT SINGLE *
      FROM TCURC
        INTO WL_TCURC
          WHERE WAERS = WG_VENDA-WAERS.

    SELECT SINGLE *
      FROM TVKD
        INTO WL_TVKD
          WHERE KALKS = WG_VENDA-KALKS.

    SELECT SINGLE *
      FROM TVSG
        INTO WL_TVSG
          WHERE VERSG = WG_VENDA-VERSG.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WG_VENDA-KTGRD
      IMPORTING
        OUTPUT = WG_VENDA-KTGRD.


    SELECT SINGLE *
      FROM TVKT
        INTO WL_TVKT
          WHERE KTGRD = WG_VENDA-KTGRD.
  ENDIF.

* Tipo (TIPO)
  IF WG_HEADER-TIPO IS INITIAL.
    MOVE: 'WG_HEADER-TIPO'       TO TG_MSG_RET-FIELD.
    CONCATENATE TEXT-E01  'Tipo de cadastro.'
                     INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

* Empresa (BUKRS)
  IF WG_HEADER-BUKRS IS INITIAL.
    MOVE: 'WG_HEADER-BUKRS'      TO TG_MSG_RET-FIELD.
    CONCATENATE TEXT-E01  'Empresa.'
                     INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND: TG_MSG_RET.
    CLEAR: TG_MSG_RET.

  ELSEIF WL_T001-BUKRS IS INITIAL.
    MOVE: 'WG_HEADER-BUKRS'       TO TG_MSG_RET-FIELD.
    CONCATENATE 'Empresa' TEXT-E02 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

* Org. Compras (EKORG)
  IF WG_HEADER-EKORG IS INITIAL.
    MOVE: 'WG_HEADER-EKORG'      TO TG_MSG_RET-FIELD.
    CONCATENATE TEXT-E01  'Org. Compras.'
                     INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND: TG_MSG_RET.
    CLEAR: TG_MSG_RET.

  ELSEIF WL_T024E-EKORG IS INITIAL.
    MOVE: 'WG_HEADER-EKORG'       TO TG_MSG_RET-FIELD.
    CONCATENATE 'Org. Compras' TEXT-E02 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

  IF WG_HEADER-SPART = C_C.
* Canal distrib (VTWEG)
    IF WG_HEADER-VTWEG IS INITIAL.
      MOVE: 'WG_HEADER-VTWEG'      TO TG_MSG_RET-FIELD.
      CONCATENATE TEXT-E01  'Canal distrib.'
                       INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND: TG_MSG_RET.
      CLEAR: TG_MSG_RET.

    ELSEIF WL_TVTW-VTWEG IS INITIAL.
      MOVE: 'WG_HEADER-VTWEG'       TO TG_MSG_RET-FIELD.
      CONCATENATE 'Canal distrib' TEXT-E02 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

* Setor de Atividade (SPART)
    IF WG_HEADER-SPART IS INITIAL.
      MOVE: 'WG_HEADER-SPART'      TO TG_MSG_RET-FIELD.
      CONCATENATE TEXT-E01  'Setor de Atividade.'
                       INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND: TG_MSG_RET.
      CLEAR: TG_MSG_RET.

    ELSEIF WL_TSPA-SPART IS INITIAL.
      MOVE: 'WG_HEADER-SPART'       TO TG_MSG_RET-FIELD.
      CONCATENATE 'Setor de Atividade' TEXT-E02 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

* Forma de Tratamento (TITLE_MEDI)
  IF WG_HEADER-TITLE_MEDI IS INITIAL.
    MOVE: 'WG_HEADER-TITLE_MEDI'      TO TG_MSG_RET-FIELD.
    CONCATENATE TEXT-E01  'Forma de Tratamento.'
                     INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND: TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

* Nome (NAME1)
  IF WG_HEADER-NAME1 IS INITIAL.
    MOVE: 'WG_HEADER-NAME1'      TO TG_MSG_RET-FIELD.
    CONCATENATE TEXT-E01  'Nome.'
                     INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND: TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

* Nome Fantasia (SORTL)
*  IF WG_HEADER-SORTL IS INITIAL.
*    MOVE: 'WG_HEADER-SORTL'      TO TG_MSG_RET-FIELD.
*    CONCATENATE TEXT-E01  'Nome Fantasia.'
*                     INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*
*    APPEND: TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
*  ENDIF.
* Removido em solicitação do Sr. Marcos Santos Ch.

* Pessoa (TIPO_PESSOA)
  IF WG_HEADER-TIPO_PESSOA IS INITIAL.
    MOVE: 'WG_HEADER-TIPO_PESSOA'      TO TG_MSG_RET-FIELD.
    CONCATENATE TEXT-E01  'Pessoa.'
                     INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND: TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

* -----------Aba-------------
* Aba - Endereço Comercial
* Rua/Avenida (STREET)
  IF WG_ENDEREC_C-STREET IS INITIAL.
    MOVE: 'WG_ENDEREC_C-STREET'         TO TG_MSG_RET-FIELD,
          C_TAB_STRIP-TAB1              TO TG_MSG_RET-ABA.
    CONCATENATE TEXT-E01 'Rua/Avenida.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

* Número (HOUSE_NUM1)
  IF WG_ENDEREC_C-HOUSE_NUM1 IS INITIAL.
    MOVE: 'WG_ENDEREC_C-HOUSE_NUM1'     TO TG_MSG_RET-FIELD,
          C_TAB_STRIP-TAB1              TO TG_MSG_RET-ABA.
    CONCATENATE TEXT-E01 'Número.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

* Bairro (CITY2)
  IF WG_ENDEREC_C-CITY2 IS INITIAL.
    MOVE: 'WG_ENDEREC_C-CITY2'          TO TG_MSG_RET-FIELD,
          C_TAB_STRIP-TAB1              TO TG_MSG_RET-ABA.
    CONCATENATE TEXT-E01 'Bairro.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

*  IF WG_HEADER-TIPO EQ 'C'.
**   Nome da Fazenda (ZFAZENDA)
*    IF WG_ENDEREC_C-ZFAZENDA IS INITIAL.
*      MOVE: 'WG_ENDEREC_C-ZFAZENDA'         TO TG_MSG_RET-FIELD,
*            C_TAB_STRIP-TAB1              TO TG_MSG_RET-ABA.
*      CONCATENATE TEXT-E01 'Nome da Fazenda.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
*
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ENDIF.
*  ENDIF.
* Removido em solicitação do Sr. Marcos Santos Ch.125737

* Cep (POST_CODE1)
  IF WG_ENDEREC_C-POST_CODE1 IS INITIAL.
    MOVE: 'WG_ENDEREC_C-POST_CODE1'     TO TG_MSG_RET-FIELD,
          C_TAB_STRIP-TAB1              TO TG_MSG_RET-ABA.
    CONCATENATE TEXT-E01 'Cep.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

* Município (CITY1)
  IF WG_ENDEREC_C-CITY1 IS INITIAL.
    MOVE: 'WG_ENDEREC_C-CITY1'          TO TG_MSG_RET-FIELD,
          C_TAB_STRIP-TAB1              TO TG_MSG_RET-ABA.
    CONCATENATE TEXT-E01 'Município.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

* Estado (REGION)
  IF WG_ENDEREC_C-REGION IS INITIAL.
    MOVE: 'WG_ENDEREC_C-REGION'         TO TG_MSG_RET-FIELD,
          C_TAB_STRIP-TAB1              TO TG_MSG_RET-ABA.
    CONCATENATE TEXT-E01 'Estado.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.

  ELSEIF WL_T005S_C-BLAND IS INITIAL.
    MOVE: 'WG_ENDEREC_C-REGION'         TO TG_MSG_RET-FIELD,
          C_TAB_STRIP-TAB1              TO TG_MSG_RET-ABA.
    CONCATENATE 'Estado' TEXT-E02 INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

* País (COUNTRY)
  IF WG_ENDEREC_C-COUNTRY IS INITIAL.
    MOVE: 'WG_ENDEREC_C-COUNTRY'        TO TG_MSG_RET-FIELD,
          C_TAB_STRIP-TAB1              TO TG_MSG_RET-ABA.
    CONCATENATE TEXT-E01 'País.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.

  ELSEIF WL_T005_C-LAND1 IS INITIAL.
    MOVE: 'WG_ENDEREC_C-REGION'         TO TG_MSG_RET-FIELD,
          C_TAB_STRIP-TAB1              TO TG_MSG_RET-ABA.
    CONCATENATE 'País' TEXT-E02 INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

* Aba - Endereço Residencial
* Validação para Transação ZFI0046
  IF SY-TCODE <> 'ZFI0046'.
    IF WG_HEADER-TIPO EQ 'F'.
*   Rua/Avenida (STREET)
      IF WG_ENDEREC_R-STREET IS INITIAL.
        MOVE: 'WG_ENDEREC_R-STREET'         TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB2              TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E01 'Rua/Avenida.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*   Número (HOUSE_NUM1)
      IF WG_ENDEREC_R-HOUSE_NUM1 IS INITIAL.
        MOVE: 'WG_ENDEREC_R-HOUSE_NUM1'     TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB2              TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E01 'Número.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*   Bairro (CITY2)
      IF WG_ENDEREC_R-CITY2 IS INITIAL.
        MOVE: 'WG_ENDEREC_R-CITY2'          TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB2              TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E01 'Bairro.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*   Nome da Fazenda (ZFAZENDA)
      IF WG_ENDEREC_R-ZFAZENDA IS INITIAL.
        MOVE: 'WG_ENDEREC_R-ZFAZENDA'       TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB2              TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E01 'Nome da Fazenda.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*   Cep (POST_CODE1)
      IF WG_ENDEREC_R-POST_CODE1 IS INITIAL.
        MOVE: 'WG_ENDEREC_R-POST_CODE1'     TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB2              TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E01 'Cep.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*   Município (CITY1)
      IF WG_ENDEREC_R-CITY1 IS INITIAL.
        MOVE: 'WG_ENDEREC_C-CITY1'          TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB2              TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E01 'Município.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*   Estado (REGION)
      IF WG_ENDEREC_R-REGION IS INITIAL.
        MOVE: 'WG_ENDEREC_R-REGION'         TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB2              TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E01 'Estado.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ELSEIF WL_T005S_R-BLAND IS INITIAL.
        MOVE: 'WG_ENDEREC_R-REGION'         TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB2              TO TG_MSG_RET-ABA.
        CONCATENATE 'Estado' TEXT-E02 INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ENDIF.

*   País (COUNTRY)
      IF WG_ENDEREC_R-COUNTRY IS INITIAL.
        MOVE: 'WG_ENDEREC_R-COUNTRY'        TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB2              TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E01 'País.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ELSEIF WL_T005_R-LAND1 IS INITIAL.
        MOVE: 'WG_ENDEREC_R-REGION'         TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB2              TO TG_MSG_RET-ABA.
        CONCATENATE 'País' TEXT-E02 INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.
  ENDIF.

* Aba - Dados Fiscais
  IF WG_HEADER-TIPO_PESSOA EQ 'J'.
*   CNPJ (STCD1)
    IF WG_FISCAIS-STCD1 IS INITIAL.
      MOVE: 'WG_FISCAIS-STCD1'            TO TG_MSG_RET-FIELD,
            C_TAB_STRIP-TAB3              TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E01 'CNPJ.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

  ELSEIF WG_HEADER-TIPO_PESSOA EQ 'F'.
*   CPF (STCD2)
    IF WG_FISCAIS-STCD2 IS INITIAL.
      MOVE: 'WG_FISCAIS-STCD2'            TO TG_MSG_RET-FIELD,
            C_TAB_STRIP-TAB3              TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E01 'CPF.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

*   Data de nascimento (GBDAT)
    IF WG_FISCAIS-GBDAT IS INITIAL.
      MOVE: 'WG_FISCAIS-GBDAT'            TO TG_MSG_RET-FIELD,
            C_TAB_STRIP-TAB3              TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E01 'Data de nascimento.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

* Aba - Dados Bancários
* Validação para o tipo Cliente
  IF WG_HEADER-TIPO EQ C_F.
    LOOP AT  TG_BANCARIO.
      WL_LINHA = SY-TABIX.

*   Banco (BANKS)
      IF TG_BANCARIO-BANKS IS INITIAL.
        MOVE: 'TG_BANCARIO-BANKS'           TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB4              TO TG_MSG_RET-ABA,
              'GRID1'                       TO TG_MSG_RET-OBJ,
              WL_LINHA                      TO TG_MSG_RET-TABIX.
        CONCATENATE TEXT-E01 'Banco LINHA:' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ELSEIF WL_T005_BANK-LAND1 IS INITIAL.
        MOVE: 'TG_BANCARIO-BANKS'           TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB4              TO TG_MSG_RET-ABA,
              'GRID1'                       TO TG_MSG_RET-OBJ,
              WL_LINHA                      TO TG_MSG_RET-TABIX.
        CONCATENATE TEXT-E01 'Banco LINHA:' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND: TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*   N° Agência bancária (BANKL)
      IF TG_BANCARIO-BANKL IS INITIAL.
        MOVE: 'TG_BANCARIO-BANKL'           TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB4              TO TG_MSG_RET-ABA,
              'GRID1'                       TO TG_MSG_RET-OBJ,
              WL_LINHA                      TO TG_MSG_RET-TABIX.
        CONCATENATE TEXT-E01 'N° Agência bancária LINHA:' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

      IF TG_BANCARIO-BANKA IS INITIAL.
        MOVE: 'TG_BANCARIO-BANKA'           TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB4              TO TG_MSG_RET-ABA,
              'GRID1'                       TO TG_MSG_RET-OBJ,
              WL_LINHA                      TO TG_MSG_RET-TABIX.
        CONCATENATE TEXT-E01 'N° Banco:' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*   Conta bancária (BANKN)
      IF TG_BANCARIO-BANKN IS INITIAL.
        MOVE: 'TG_BANCARIO-BANKN'           TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB4              TO TG_MSG_RET-ABA,
              'GRID1'                       TO TG_MSG_RET-OBJ,
              WL_LINHA                      TO TG_MSG_RET-TABIX.
        CONCATENATE TEXT-E01 'Conta bancária LINHA:' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*   Chv.Ctrl.Bancos (BKONT)
      IF TG_BANCARIO-BKONTT IS INITIAL.
        MOVE: 'TG_BANCARIO-BKONTT'           TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB4              TO TG_MSG_RET-ABA,
              'GRID1'                       TO TG_MSG_RET-OBJ,
              WL_LINHA                      TO TG_MSG_RET-TABIX.
        CONCATENATE TEXT-E01 'Chv.Ctrl.Bancos LINHA:' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDLOOP.
  ENDIF.

* Aba - Dados Impostos
  LOOP AT  TG_IMPOSTOS.
    WL_LINHA = SY-TABIX.

*   País IRF (QLAND)
    IF TG_IMPOSTOS-QLAND IS INITIAL.
      MOVE: 'TG_IMPOSTOS-QLAND'           TO TG_MSG_RET-FIELD,
            C_TAB_STRIP-TAB5              TO TG_MSG_RET-ABA,
            'GRID2'                       TO TG_MSG_RET-OBJ,
            WL_LINHA                      TO TG_MSG_RET-TABIX.
      CONCATENATE TEXT-E01 'País LINHA:' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.

    ELSEIF WL_T005Q-LAND1 IS INITIAL.
      MOVE: 'TG_IMPOSTOS-BANKS'           TO TG_MSG_RET-FIELD,
            C_TAB_STRIP-TAB5              TO TG_MSG_RET-ABA,
            'GRID2'                       TO TG_MSG_RET-OBJ,
            WL_LINHA                      TO TG_MSG_RET-TABIX.
      CONCATENATE TEXT-E01 'País LINHA:' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND: TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

*   Ctg. IRF (WITHT)
    IF TG_IMPOSTOS-WITHT IS INITIAL.
      MOVE: 'TG_IMPOSTOS-WITHT'           TO TG_MSG_RET-FIELD,
            C_TAB_STRIP-TAB5              TO TG_MSG_RET-ABA,
            'GRID2'                       TO TG_MSG_RET-OBJ,
            WL_LINHA                      TO TG_MSG_RET-TABIX.
      CONCATENATE TEXT-E01 'Ctg. IRF LINHA:' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

*   Código. IRF (WT_WITHCD)
    IF TG_IMPOSTOS-WT_WITHCD IS INITIAL.
      MOVE: 'TG_IMPOSTOS-WT_WITHCD'           TO TG_MSG_RET-FIELD,
            C_TAB_STRIP-TAB5              TO TG_MSG_RET-ABA,
            'GRID2'                       TO TG_MSG_RET-OBJ,
            WL_LINHA                      TO TG_MSG_RET-TABIX.
      CONCATENATE TEXT-E01 'Código IRF LINHA:' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

  ENDLOOP.


* Aba - Dados Contáb. Pgto.
* Conta Contábil (AKONT)
* Validação transação ZFI0046
  IF SY-TCODE <> 'ZFI0046'.
    IF WG_CONTAB_PGTO-AKONT IS INITIAL.
      MOVE: 'WG_CONTAB_PGTO-AKONT'        TO TG_MSG_RET-FIELD,
            C_TAB_STRIP-TAB6              TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E01 'Conta Contábil.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

* Grp.Adm.Tesouraria (FDGRV)
    IF WG_CONTAB_PGTO-FDGRV IS INITIAL.
      MOVE: 'WG_CONTAB_PGTO-FDGRV'        TO TG_MSG_RET-FIELD,
            C_TAB_STRIP-TAB6              TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E01 'Grp.Adm.Tesouraria.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.

    ELSEIF WL_T035-GRUPP IS INITIAL.
      MOVE: 'WG_CONTAB_PGTO-FDGRV'        TO TG_MSG_RET-FIELD,
            C_TAB_STRIP-TAB6              TO TG_MSG_RET-ABA.
      CONCATENATE 'Grp.Adm.Tesouraria' TEXT-E02 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND: TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

* Condições (ZTERM)
    IF WG_CONTAB_PGTO-ZTERM IS INITIAL.
      MOVE: 'WG_CONTAB_PGTO-ZTERM'        TO TG_MSG_RET-FIELD,
            C_TAB_STRIP-TAB6              TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E01 'Condições.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.

    ELSEIF WL_TVZB-ZTERM IS INITIAL.
      MOVE: 'WG_CONTAB_PGTO-ZTERM'        TO TG_MSG_RET-FIELD,
            C_TAB_STRIP-TAB6              TO TG_MSG_RET-ABA.
      CONCATENATE 'Condições.' TEXT-E02 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND: TG_MSG_RET.
      CLEAR: TG_MSG_RET.

    ENDIF.

* Forma de Pagamento (ZWELS)
    IF WG_CONTAB_PGTO-ZWELS IS INITIAL.
      MOVE: 'WG_CONTAB_PGTO-ZWELS'        TO TG_MSG_RET-FIELD,
            C_TAB_STRIP-TAB6              TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E01 'Forma de Pagamento.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

* Bloq. Pagamento (ZAHLS)
    IF WG_CONTAB_PGTO-ZAHLS IS NOT INITIAL
      AND WL_T008-ZAHLS IS INITIAL.
      MOVE: 'WG_CONTAB_PGTO-ZAHLS'        TO TG_MSG_RET-FIELD,
            C_TAB_STRIP-TAB6              TO TG_MSG_RET-ABA.
      CONCATENATE 'Bloq. Pagamento' TEXT-E02 INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

* Chv. Agrupamento (ZGRUP)
    IF WG_CONTAB_PGTO-ZGRUP IS NOT INITIAL
      AND WL_T008-ZAHLS IS INITIAL.
      MOVE: 'WG_CONTAB_PGTO-ZAHLS'        TO TG_MSG_RET-FIELD,
            C_TAB_STRIP-TAB6              TO TG_MSG_RET-ABA.
      CONCATENATE 'Chv. Agrupamento' TEXT-E02 INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

* Validação transação ZFI0046
  IF SY-TCODE <> 'ZFI0046'.
    IF WG_HEADER-TIPO EQ 'F'.
* Aba - Dados Compras
* Moeda do Pedido (WAERS)
      IF WG_COMPRA-WAERS IS INITIAL.
        MOVE: 'WG_COMPRA-WAERS'             TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB7              TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E01 'Moeda do Pedido.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ELSEIF WL_TCURC-WAERS IS INITIAL.
        MOVE: 'WG_COMPRA-WAERS'             TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB7              TO TG_MSG_RET-ABA.
        CONCATENATE 'Moeda do Pedido' TEXT-E02 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND: TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*   Condições de Pedido (ZTERM)
      IF WG_COMPRA-ZTERM IS INITIAL.
        MOVE: 'WG_COMPRA-ZTERM'             TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB7              TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E01 'Condições de Pedido.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ELSEIF WL_TVZB_C-ZTERM IS INITIAL.
        MOVE: 'WG_COMPRA-ZTERM'             TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB7              TO TG_MSG_RET-ABA.
        CONCATENATE 'Condições de Pedido' TEXT-E02 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND: TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*   Inconterms (INCO1)
      IF WG_COMPRA-INCO1 IS INITIAL.
        MOVE: 'WG_COMPRA-INCO1'             TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB7              TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E01 'Inconterms.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ELSEIF WL_TINC-INCO1 IS INITIAL.
        MOVE: 'WG_COMPRA-INCO1'             TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB7              TO TG_MSG_RET-ABA.
        CONCATENATE 'Inconterms' TEXT-E02 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND: TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*   Pedido Automático (KZAUT)
      IF WG_COMPRA-KZAUT IS INITIAL.
        MOVE: 'WG_COMPRA-KZAUT'             TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB7              TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E01 'Pedido Automático.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

    ELSEIF WG_HEADER-TIPO EQ 'C'.
*   Aba - Dados Vendas
*   Moeda (WAERS)
      IF WG_VENDA-WAERS IS INITIAL.
        MOVE: 'WG_VENDA-WAERS'              TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB8              TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E01 'Moeda.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ELSEIF WL_TCURC-WAERS IS INITIAL.
        MOVE: 'WG_VENDA-WAERS'              TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB8              TO TG_MSG_RET-ABA.
        CONCATENATE 'Moeda' TEXT-E02 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND: TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*   Esquema Cliente (KALKS)
      IF WG_VENDA-KALKS IS INITIAL.
        MOVE: 'WG_VENDA-KALKS'              TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB8              TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E01 'Esquema Cliente.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ELSEIF WL_TVKD-KALKS IS INITIAL.
        MOVE: 'WG_VENDA-KALKS'              TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB8              TO TG_MSG_RET-ABA.
        CONCATENATE 'Esquema Cliente' TEXT-E02 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND: TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*   Gpo.Est.Cliente (VERSG)
      IF WG_VENDA-VERSG IS INITIAL.
        MOVE: 'WG_VENDA-VERSG'              TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB8              TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E01 'Gpo.Est.Cliente.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ELSEIF WL_TVSG-VERSG IS INITIAL.
        MOVE: 'WG_VENDA-VERSG'              TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB8              TO TG_MSG_RET-ABA.
        CONCATENATE 'Gpo.Est.Cliente' TEXT-E02 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND: TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*   Grupo Classif.Contabil (KTGRD)
      IF WG_VENDA-KTGRD IS INITIAL.
        MOVE: 'WG_VENDA-KTGRD'              TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB8              TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E01 'Grupo Classif. Contabil.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ELSEIF WL_TVKT-KTGRD IS INITIAL.
        MOVE: 'WG_VENDA-KTGRD'              TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB8              TO TG_MSG_RET-ABA.
        CONCATENATE 'Grupo Classif. Contabil' TEXT-E02 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND: TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.


*   Impostos (TAXKD)
      IF WG_VENDA-TAXKD IS INITIAL.
        MOVE: 'WG_VENDA-TAXKD'              TO TG_MSG_RET-FIELD,
              C_TAB_STRIP-TAB8              TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E01 'Impostos.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '100'
*      I_SHOW        = C_X
        I_REPID       = SY-REPID
        I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
        I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.
ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA:  BEGIN OF TL_UCOMM OCCURS 0,
        UCOMM TYPE  SY-UCOMM,
       END OF TL_UCOMM.

  REFRESH:TL_UCOMM.
  CLEAR: TL_UCOMM.

  IF WG_ACAO NE C_ADD
  AND WG_ACAO NE C_MODIF.
    MOVE: C_SAVE TO TL_UCOMM.

    APPEND TL_UCOMM.
    CLEAR: TL_UCOMM.

    IF GRID1 IS NOT INITIAL.
      CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
        IMPORTING
          ET_FIELDCATALOG = T_FIELDCATALOG.
      LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
        W_FIELDCATALOG-EDIT = SPACE.
        MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
      ENDLOOP.
      CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
        EXPORTING
          IT_FIELDCATALOG = T_FIELDCATALOG.
    ENDIF.

    IF GRID2 IS NOT INITIAL.
      CALL METHOD GRID2->GET_FRONTEND_FIELDCATALOG
        IMPORTING
          ET_FIELDCATALOG = T_FIELDCATALOG.
      LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
        W_FIELDCATALOG-EDIT = SPACE.
        MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
      ENDLOOP.
      CALL METHOD GRID2->SET_FRONTEND_FIELDCATALOG
        EXPORTING
          IT_FIELDCATALOG = T_FIELDCATALOG.
    ENDIF.

  ELSEIF WG_ACAO EQ C_ADD
      OR WG_ACAO EQ C_MODIF.

    MOVE: C_ATUAL TO TL_UCOMM.

    APPEND TL_UCOMM.
    CLEAR: TL_UCOMM.

    IF GRID1 IS NOT INITIAL.
      CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
        IMPORTING
          ET_FIELDCATALOG = T_FIELDCATALOG.
      LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
        "Tratar aqui campo editavel Faneli
        IF W_FIELDCATALOG-REF_FIELD NE 'BANKA'
        AND W_FIELDCATALOG-REF_FIELD NE 'BANKL'.
          W_FIELDCATALOG-EDIT = C_X.
          MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
        ENDIF.
      ENDLOOP.
      CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
        EXPORTING
          IT_FIELDCATALOG = T_FIELDCATALOG.
    ENDIF.

    IF GRID2 IS NOT INITIAL.
      CALL METHOD GRID2->GET_FRONTEND_FIELDCATALOG
        IMPORTING
          ET_FIELDCATALOG = T_FIELDCATALOG.
      LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
        W_FIELDCATALOG-EDIT = C_X.
        MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
      ENDLOOP.
      CALL METHOD GRID2->SET_FRONTEND_FIELDCATALOG
        EXPORTING
          IT_FIELDCATALOG = T_FIELDCATALOG.
    ENDIF.
  ENDIF.

  CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WA_STABLE.

  CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WA_STABLE.

  CALL METHOD CL_GUI_CFW=>DISPATCH.
  SET PF-STATUS 'Z001' EXCLUDING TL_UCOMM.
  SET TITLEBAR 'Z001'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA: WL_HEADER LIKE WG_HEADER.


  CALL METHOD GRID1->CHECK_CHANGED_DATA.
  CALL METHOD GRID2->CHECK_CHANGED_DATA.

  CASE SY-UCOMM.
    WHEN C_ADD.
      IF WG_FLAG IS INITIAL.
        PERFORM LIMPA_VARIAVEL USING SY-UCOMM.
        PERFORM GET_NEXT_NUMBER  USING  'ZSOLCAD'
                                        '01'
                               CHANGING WG_HEADER-NR_SOL.

        MOVE: SY-DATUM TO WG_HEADER-DATA_ATUAL,
              SY-UZEIT TO WG_HEADER-HORA_ATUAL.
      ENDIF.
      MOVE C_ADD TO WG_ACAO.
    WHEN C_ATUAL.
      PERFORM LIMPA_VARIAVEL USING SY-UCOMM.
      PERFORM BUSCA_DADOS_DOC.
      PERFORM BUSCA_DADOS.
      MOVE: C_ATUAL TO WG_ACAO.
    WHEN C_SEARCH.
      PERFORM BUSCA_DADOS.


    WHEN C_COPY.
**    Valida se existe documento para ser modificado.
      SELECT SINGLE DOC_SIMULACAO
        FROM ZSDT0040
        INTO WG_HEADER-NR_SOL
         WHERE DOC_SIMULACAO EQ WG_HEADER-NR_SOL.

      IF SY-SUBRC IS INITIAL.
        PERFORM LIMPA_VARIAVEL USING C_ATUAL.
        PERFORM BUSCA_DADOS_DOC.
        PERFORM BUSCA_DADOS.
*        PERFORM get_next_number  USING  'ZSIMULACAO'
*                                       '1'
*                              CHANGING wg_header-doc_simulacao.

        MOVE: SY-DATUM TO WG_HEADER-DATA_ATUAL,
              SY-UZEIT TO WG_HEADER-HORA_ATUAL.

        MOVE C_MODIF TO WG_ACAO.
      ENDIF.
*    WHEN c_descp.
*      PERFORM inputa_desc.
    WHEN C_MODIF.
      SELECT SINGLE *
        FROM ZMMT0049
        INTO WL_HEADER
         WHERE NR_SOL EQ WG_HEADER-NR_SOL.

      IF SY-SUBRC IS INITIAL.
*        call function 'ENQUEUE_EZSDT0040'
*          exporting
*            doc_simulacao  = wg_header-doc_simulacao
*          exceptions
*            foreign_lock   = 1
*            system_failure = 2
*            others         = 3.
*        if sy-subrc <> 0.
*          message id sy-msgid type sy-msgty number sy-msgno
*                  with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*        endif.

        PERFORM LIMPA_VARIAVEL USING C_ATUAL.
        PERFORM BUSCA_DADOS_DOC.
        PERFORM BUSCA_DADOS.

        MOVE C_MODIF TO WG_ACAO.

      ENDIF.

    WHEN C_TIPO_PES.
      PERFORM LIMPA_VARIAVEL USING SY-UCOMM.
    WHEN C_CANCEL.
*      call function 'DEQUEUE_EZSDT0040'
*        exporting
*          doc_simulacao = wg_header-doc_simulacao.
      PERFORM LIMPA_VARIAVEL USING SY-UCOMM.
    WHEN C_SHOW_MSGRE.
      PERFORM VERIFICA_ERROS.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN      = '100'
          I_SHOW        = 'X'
          I_REPID       = SY-REPID
          I_POPUP       = 0
          I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
          I_SET_FIELD   = 'X_FIELD'
          I_SET_CELL    = 'WG_CELL'
          I_SET_OBJ     = 'WG_OBJ'
        IMPORTING
          E_MESSAGEM    = WG_MENSAGEM
        TABLES
          IT_MSGS       = TG_MSG_RET.

    WHEN C_SAVE.
      CALL METHOD GRID1->CHECK_CHANGED_DATA.
      PERFORM VERIFICA_ERROS.
      IF TG_MSG_RET[] IS INITIAL.
        PERFORM GRAVA_DADOS.

      ELSE.
        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
      ENDIF.

      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN      = '100'
          I_SHOW        = 'X'
          I_REPID       = SY-REPID
          I_POPUP       = 0
          I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
          I_SET_FIELD   = 'X_FIELD'
          I_SET_CELL    = 'WG_CELL'
          I_SET_OBJ     = 'WG_OBJ'
        IMPORTING
          E_MESSAGEM    = WG_MENSAGEM
        TABLES
          IT_MSGS       = TG_MSG_RET.

    WHEN 'BACK'
      OR 'EXIT'.
*      call function 'DEQUEUE_EZSDT0040'
*        exporting
*          doc_simulacao = wg_header-doc_simulacao.
      LEAVE TO SCREEN 0.
    WHEN C_APROV.
*      perform modifica_status using 'A'.
*    WHEN c_reprov.
*      perform modifica_status using 'R'.
    WHEN C_BLOQ.
*      perform modifica_status using 'B'.
*    WHEN c_gerar.
*      perform gera_ordem_venda.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS OUTPUT.
  DATA:   WL_REPID     TYPE SY-REPID,
          TL_FUNCTION  TYPE UI_FUNCTIONS,
          WL_FUNCTION  LIKE TL_FUNCTION WITH HEADER LINE,
          LT_F4        TYPE LVC_T_F4 WITH HEADER LINE.

  WL_REPID = SY-REPID.
  PERFORM VERIFICA_ERROS.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      I_SCREEN      = '100'
*      I_SHOW        = space
      I_REPID       = SY-REPID
      I_POPUP       = 0
      I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
      I_SET_FIELD   = 'X_FIELD'
      I_SET_CELL    = 'WG_CELL'
      I_SET_OBJ     = 'WG_OBJ'
    IMPORTING
      E_MESSAGEM    = WG_MENSAGEM
    TABLES
      IT_MSGS       = TG_MSG_RET.

  IF CONTAINER1 IS INITIAL.
    WA_LAYOUT-ZEBRA      = C_X.
    WA_LAYOUT-NO_ROWMARK = C_X.
    WA_STABLE-ROW        = C_X.

    CREATE OBJECT CONTAINER1
      EXPORTING
        CONTAINER_NAME = 'CC_01'.

    CREATE OBJECT GRID1
      EXPORTING
        I_PARENT = CONTAINER1.

    CREATE OBJECT OBG_TOOLBAR
      EXPORTING
        IO_ALV_GRID = GRID1.

    REFRESH LT_F4.
    LT_F4-FIELDNAME = 'BANKK'.
    LT_F4-REGISTER  = 'X'.
    LT_F4-GETBEFORE = 'X'.
    APPEND LT_F4.


    CALL METHOD GRID1->REGISTER_F4_FOR_FIELDS
      EXPORTING
        IT_F4 = LT_F4[].

*      * Register event handler
    SET HANDLER: OBG_TOOLBAR->ON_TOOLBAR FOR GRID1,
                 OBG_TOOLBAR->HANDLE_USER_COMMAND_BANC FOR GRID1.
*                 OBG_TOOLBAR->ON_ONF4 FOR GRID1.

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
*

**    wa_layout-stylefname = 'STYLE'.
    PERFORM: BUILD_DROPDOWN.
    PERFORM MONTAR_LAYOUT USING 'TG_BANCARIO'.
    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
*        i_save               = 'X'
      CHANGING
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = TG_BANCARIO[].

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
*
*
    SET HANDLER:
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED FOR GRID1,
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED FOR GRID1,
              LCL_EVENT_HANDLER=>ON_ONF4 FOR GRID1.
  ELSE.
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDIF.

  IF CONTAINER2 IS INITIAL.
    WA_LAYOUT-ZEBRA      = C_X.
    WA_LAYOUT-NO_ROWMARK = C_X.
    WA_STABLE-ROW        = C_X.

    CREATE OBJECT CONTAINER2
      EXPORTING
        CONTAINER_NAME = 'CC_02'.

    CREATE OBJECT GRID2
      EXPORTING
        I_PARENT = CONTAINER2.

    CREATE OBJECT OBG_TOOLBAR
      EXPORTING
        IO_ALV_GRID = GRID2.

*      * Register event handler
    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR GRID2.
    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND_IMP FOR GRID2.

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
*
**    lt_f4-fieldname = 'MATNR'.
**    lt_f4-register = 'X' .
**    lt_f4-getbefore = 'X' .
**    append lt_f4 .
*
**    wa_layout-stylefname = 'STYLE'.
*
    PERFORM MONTAR_LAYOUT USING 'TG_IMPOSTOS'.
    CALL METHOD GRID2->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
*        i_save               = 'X'
      CHANGING
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = TG_IMPOSTOS[].

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
*
*    CALL METHOD GRID1->REGISTER_F4_FOR_FIELDS
*      EXPORTING
*        IT_F4 = LT_F4[].
*
    SET HANDLER:
*              LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK FOR GRID1,
**              lcl_event_handler=>on_hotspot_click for grid1,
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED FOR GRID1,
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED FOR GRID1.
**              lcl_event_handler=>on_onf4 FOR grid1.
  ELSE.
    CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDIF.
  IF OBG_MANAGER IS NOT INITIAL.
    CLEAR: OBG_MANAGER.
    FREE  OBG_MANAGER.
  ENDIF.
  IF OBG_MANAGER IS INITIAL.

    WG_OBJ-OBJKEY   = WG_HEADER-NR_SOL.
    WG_OBJ-OBJTYPE = C_GOS.
    IF WG_ACAO EQ C_MODIF.
      CREATE OBJECT OBG_MANAGER
        EXPORTING
          IS_OBJECT      = WG_OBJ
          IP_NO_COMMIT   = 'R'
*          IP_MODE        = 'E'
*          IP_NO_INSTANCE = 'X'
        EXCEPTIONS
          OTHERS         = 1.
    ELSE.
      CREATE OBJECT OBG_MANAGER
        EXPORTING
          IS_OBJECT      = WG_OBJ
          IP_NO_COMMIT   = 'R'
          IP_MODE        = 'D'
*          IP_NO_INSTANCE = 'X'
        EXCEPTIONS
          OTHERS         = 1.
    ENDIF.

    IF OBG_MANAGER IS NOT INITIAL.
      OBG_MANAGER->GET_CONTEXT_MENU( ).
*      IF WG_ACAO EQ C_MODIF.
*        OBG_MANAGER->SET_RW_MODE( IP_MODE = 'E' ).
*      ELSE.
*        OBG_MANAGER->SET_RW_MODE( IP_MODE = 'D' ).
*      ENDIF.
    ENDIF.

  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT USING P_TABLE  .
  REFRESH T_FIELDCATALOG.
  IF P_TABLE EQ 'TG_BANCARIO'.
    PERFORM MONTAR_ESTRUTURA USING:
        1 'ZMMT0052'  'BANKS'       'TG_BANCARIO' 'BANKS'     'País'          '8'  'X' ' ' ' ',
        2 ' '         ' '           'TG_BANCARIO' 'BANKK'     'Chave Banco'   ' '  'X' ' ' ' ',
        3 'ZMMT0052'  'BANKA'       'TG_BANCARIO' 'BANKA'     'Banco'         '20' ' ' ' ' ' ',
        4 'ZMMT0052'  'BANKL'       'TG_BANCARIO' 'BANKL'     'Nº.Agência'    ' '  ' ' ' ' ' ',
        5 'ZMMT0052'  'BANKN'       'TG_BANCARIO' 'BANKN'     ' '             ' '  'X' ' ' ' ',
*        6 'ZMMT0052'  'BKONT'       'TG_BANCARIO' 'BKONT'     'Tipo de conta' '15' 'X' ' ' ' ',
        6 ''          ''            'TG_BANCARIO' 'BKONTT'    'Tipo Conta'    '15' 'X' ' ' ' ',
        7 'ZMMT0052'  'BVTYP'       'TG_BANCARIO' 'BVTYP'     'Prioridade'    '15' 'X' ' ' ' ',
        8 'ZMMT0052'  'IBAN'        'TG_BANCARIO' 'IBAN'      ' '             ' '  'X' ' ' ' '.

  ELSEIF P_TABLE EQ 'TG_IMPOSTOS'.
    PERFORM MONTAR_ESTRUTURA USING:
        1 'ZMMT0053'  'QLAND'       'TG_IMPOSTOS' 'QLAND'     ' '             '10' 'X' ' ' ' ',
        2 'ZMMT0053'  'WITHT'       'TG_IMPOSTOS' 'WITHT'     ' '             '7'  'X' ' ' ' ',
        3 'ZMMT0053'  'WT_WITHCD'   'TG_IMPOSTOS' 'WT_WITHCD' ' '             '10' 'X' ' ' ' ',
        4 'ZMMT0053'  'WT_SUBJCT'   'TG_IMPOSTOS' 'WT_SUBJCT' ' '             '7'  'X' ' ' ' '.
  ENDIF.
*  PERFORM VALIDA_LAYOUT TABLES T_FIELDCATALOG
*                        USING SY-UNAME.

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_EDIT)
                            VALUE(P_SUM)
                            VALUE(P_EMPHASIZE).

  CLEAR W_FIELDCATALOG.
  W_FIELDCATALOG-FIELDNAME     = P_FIELD.
  W_FIELDCATALOG-TABNAME       = P_TABNAME.
  W_FIELDCATALOG-REF_TABLE     = P_REF_TABNAME.
  W_FIELDCATALOG-REF_FIELD     = P_REF_FIELDNAME.
  W_FIELDCATALOG-KEY           = ' '.
*  w_fieldcatalog-key_sel       = 'X'.
  W_FIELDCATALOG-EDIT          = P_EDIT.

*  IF WG_DISPLAY IS INITIAL.
*    W_FIELDCATALOG-EDIT          = P_EDIT.
*  ENDIF.

  IF P_FIELD EQ 'BANKS'
  OR P_FIELD EQ 'BANKK'.
    W_FIELDCATALOG-F4AVAILABL  = C_X.
  ENDIF.

  W_FIELDCATALOG-DO_SUM        = P_SUM.
  W_FIELDCATALOG-COL_POS       = P_COL_POS.

  IF P_OUTPUTLEN IS NOT INITIAL.
    W_FIELDCATALOG-OUTPUTLEN      = P_OUTPUTLEN.
  ENDIF.

  W_FIELDCATALOG-NO_OUT        = ' '.
  W_FIELDCATALOG-REPTEXT       = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_S     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_M     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_L     = P_SCRTEXT_L.
  W_FIELDCATALOG-EMPHASIZE     = P_EMPHASIZE.

  IF P_FIELD EQ 'WT_SUBJCT'.
    W_FIELDCATALOG-CHECKBOX    = C_X.
  ENDIF.
*  if p_field eq 'ZWERT'
*  or p_field eq 'VLRTOT'
*  or p_field eq 'CALCU'
*  or p_field eq 'TRUNIT'
*  or p_field eq 'COMPR'.
*    w_fieldcatalog-decimals_o = '000002'.
*  endif.
*

  IF P_FIELD EQ 'BKONTT'. " Cria Droplist
    W_FIELDCATALOG-DRDN_HNDL  = 1.
    W_FIELDCATALOG-DRDN_ALIAS  = C_X.
  ENDIF.
*
*  if p_field eq 'VBELN'.
*    w_fieldcatalog-hotspot = c_x.
*  endif.
*  if p_field eq 'MATNR'.
*    w_fieldcatalog-f4availabl = c_x.
*    w_fieldcatalog-edit_mask  = '==MATN1'.
*  endif.

  APPEND W_FIELDCATALOG TO T_FIELDCATALOG.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  LIMPA_VARIAVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPA_VARIAVEL USING P_ACAO.
  DATA: WL_NR_SOL TYPE ZMMT0049-NR_SOL.

  CLEAR: WL_NR_SOL.

  IF P_ACAO EQ C_CANCEL
   OR P_ACAO EQ C_ATUAL.
    WL_NR_SOL = WG_HEADER-NR_SOL.

    CLEAR: WG_HEADER,
           WG_ENDEREC_C,
           WG_ENDEREC_R,
           WG_FISCAIS,
           WG_CONTAB_PGTO,
           WG_VENDA,
           WG_COMPRA,
           WG_ACAO,
           TG_BANCARIO,
           TG_IMPOSTOS,
           WG_DESC_BUKRS,
           WG_DESC_EKORG,
           WG_DESC_VTWEG,
           WG_DESC_SPART,
           WG_DESC_FDGRV,
           WG_DESC_ZAHLS,
           WG_DESC_ZGRUP,
           WG_DESC_ZTERM,
           WG_DESC_ZTERM2,
           WG_DESC_KALKS,
           WG_DESC_VERSG,
           WG_DESC_KTGRD,
           WG_DESC_KTOKK,
           WG_FLAG,
           X_FIELD,
           TG_MSG_RET,
           WG_CELL,
           WG_OBJ.

*
    REFRESH: TG_IMPOSTOS,
             TG_BANCARIO,
             TG_MSG_RET.
*             style,
*             tg_itens-style,
*             tg_mglobal.
*
    WG_HEADER-NR_SOL = WL_NR_SOL.
  ELSEIF P_ACAO EQ C_ADD.
    CLEAR: WG_HEADER,
           WG_ENDEREC_C,
           WG_ENDEREC_R,
           WG_FISCAIS,
           WG_CONTAB_PGTO,
           WG_VENDA,
           WG_COMPRA,
           WG_ACAO,
           TG_BANCARIO,
           TG_IMPOSTOS,
           WG_DESC_BUKRS,
           WG_DESC_EKORG,
           WG_DESC_VTWEG,
           WG_DESC_SPART,
           WG_DESC_FDGRV,
           WG_DESC_ZAHLS,
           WG_DESC_ZGRUP,
           WG_DESC_ZTERM,
           WG_DESC_ZTERM2,
           WG_DESC_KALKS,
           WG_DESC_VERSG,
           WG_DESC_KTGRD,
           WG_ACAO,
           X_FIELD,
           TG_MSG_RET.

*
    REFRESH: TG_IMPOSTOS,
             TG_BANCARIO,
             TG_MSG_RET.
*             style,
*             tg_itens-style,
*             tg_mglobal.


  ELSEIF P_ACAO EQ C_TIPO_PES.
    CLEAR: WG_FISCAIS-STCD1,
           WG_FISCAIS-STCD2,
           WG_FISCAIS-STCD3,
           WG_FISCAIS-KVERM,
           WG_FISCAIS-TXJCD.

  ELSE.
*    clear: tg_itens.
*    refresh: tg_itens, tg_itens-style.
  ENDIF.

ENDFORM.                    " LIMPA_VARIAVEL
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN OUTPUT.

*  data: values       type vrm_values with header line.
  FIELD-SYMBOLS: <FS_CAMPO> TYPE ANY.

  PERFORM VALIDA_LAYOUT TABLES T_FIELDCATALOG
                        USING SY-UNAME.

  IF WG_ACAO EQ C_ADD
   OR WG_ACAO EQ C_MODIF.

    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'A1'.
        SCREEN-INPUT = '1'.

      ELSEIF SCREEN-GROUP1 EQ 'A2'.
        SCREEN-INPUT = '0'.
      ENDIF.

      IF WG_HEADER-TIPO EQ C_F.
        IF SCREEN-NAME EQ 'TAB_STRIP_TAB7'.
          SCREEN-ACTIVE = 1.
          SCREEN-INVISIBLE = 0.
        ELSEIF SCREEN-NAME EQ 'TAB_STRIP_TAB4'.
          SCREEN-ACTIVE = 1.
          SCREEN-INVISIBLE = 0.
        ELSEIF SCREEN-NAME EQ 'TAB_STRIP_TAB8'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        "Validação do para campo tipo
        IF SCREEN-NAME EQ 'WG_HEADER-VTWEG'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        IF SCREEN-NAME EQ 'WG_HEADER-SPART'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        IF SCREEN-NAME EQ 'WG_DESC_VTWEG'.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        IF SCREEN-NAME EQ 'WG_DESC_SPART'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        IF SCREEN-NAME EQ '%#AUTOTEXT005'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        IF SCREEN-NAME EQ '%#AUTOTEXT006'.
          SCREEN-INVISIBLE = 1.
        ENDIF.
      ELSEIF WG_HEADER-TIPO EQ C_C.
        IF SCREEN-NAME EQ 'TAB_STRIP_TAB7'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ELSEIF SCREEN-NAME EQ 'TAB_STRIP_TAB4'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ELSEIF SCREEN-NAME EQ 'TAB_STRIP_TAB8'.
          SCREEN-ACTIVE = 1.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        "Validação do para campo tipo
        IF SCREEN-NAME EQ 'WG_HEADER-VTWEG'.
          SCREEN-ACTIVE = 1.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        IF SCREEN-NAME EQ 'WG_HEADER-SPART'.
          SCREEN-ACTIVE = 1.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        IF SCREEN-NAME EQ 'WG_DESC_VTWEG'.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        IF SCREEN-NAME EQ 'WG_DESC_SPART'.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        IF SCREEN-NAME EQ '%#AUTOTEXT005'.
          SCREEN-ACTIVE = 1.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        IF SCREEN-NAME EQ '%#AUTOTEXT006'.
          SCREEN-ACTIVE = 1.
          SCREEN-INVISIBLE = 0.
        ENDIF.
      ELSE.
        IF SCREEN-NAME EQ 'TAB_STRIP_TAB7'
        OR SCREEN-NAME EQ 'TAB_STRIP_TAB8'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        IF WG_HEADER-TIPO = ''.
          IF SCREEN-NAME EQ 'WG_HEADER-VTWEG'.
            SCREEN-ACTIVE = 0.
            SCREEN-INVISIBLE = 1.
          ENDIF.
          IF SCREEN-NAME EQ 'WG_HEADER-SPART'.
            SCREEN-ACTIVE = 0.
            SCREEN-INVISIBLE = 1.
          ENDIF.
        ENDIF.
      ENDIF.

** Pessoa Juridica
      IF WG_HEADER-TIPO_PESSOA EQ 'J'.
        IF SCREEN-NAME EQ 'WG_FISCAIS-STCD2'.
*        OR SCREEN-NAME EQ '%#AUTOTEXT002' .
          SCREEN-INPUT = 0.

        ENDIF.

** Pessoa Fisica
      ELSEIF WG_HEADER-TIPO_PESSOA EQ 'F'.
        IF SCREEN-NAME EQ 'WG_FISCAIS-STCD1'
        OR SCREEN-NAME EQ 'WG_FISCAIS-STCD3'
        OR SCREEN-NAME EQ 'WG_FISCAIS-TXJCD'
        OR SCREEN-NAME EQ 'WG_FISCAIS-KVERM'.
          SCREEN-INPUT = 0.

        ENDIF.
      ELSE.
        IF SCREEN-NAME EQ 'WG_FISCAIS-STCD1'
        OR SCREEN-NAME EQ 'WG_FISCAIS-STCD3'
        OR SCREEN-NAME EQ 'WG_FISCAIS-TXJCD'
        OR SCREEN-NAME EQ 'WG_FISCAIS-KVERM'
        OR SCREEN-NAME EQ 'WG_FISCAIS-STCD2'.
          SCREEN-INPUT = 0.

        ENDIF.

      ENDIF.

      IF SY-TCODE = 'ZFI0046'.
        IF SCREEN-NAME EQ 'TAB_STRIP_TAB2'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ELSEIF SCREEN-NAME EQ 'TAB_STRIP_TAB5'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ELSEIF SCREEN-NAME EQ 'TAB_STRIP_TAB6'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ELSEIF SCREEN-NAME EQ 'TAB_STRIP_TAB7'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
      ENDIF.

      MODIFY SCREEN.

    ENDLOOP.

  ELSEIF WG_ACAO EQ C_ATUAL.

    LOOP AT SCREEN.
      IF WG_HEADER-TIPO EQ C_F.
        IF SCREEN-NAME EQ 'TAB_STRIP_TAB7'.
          SCREEN-ACTIVE = 1.
          SCREEN-INVISIBLE = 0.
        ELSEIF SCREEN-NAME EQ 'TAB_STRIP_TAB4'.
          SCREEN-ACTIVE = 1.
          SCREEN-INVISIBLE = 0.
        ELSEIF SCREEN-NAME EQ 'TAB_STRIP_TAB8'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        "Validação do para campo tipo
        IF SCREEN-NAME EQ 'WG_HEADER-VTWEG'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        IF SCREEN-NAME EQ 'WG_HEADER-SPART'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        IF SCREEN-NAME EQ 'WG_DESC_VTWEG'.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        IF SCREEN-NAME EQ 'WG_DESC_SPART'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        IF SCREEN-NAME EQ '%#AUTOTEXT005'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        IF SCREEN-NAME EQ '%#AUTOTEXT006'.
          SCREEN-INVISIBLE = 1.
        ENDIF.
      ELSEIF WG_HEADER-TIPO EQ C_C.
        IF SCREEN-NAME EQ 'TAB_STRIP_TAB7'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ELSEIF SCREEN-NAME EQ 'TAB_STRIP_TAB4'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ELSEIF SCREEN-NAME EQ 'TAB_STRIP_TAB8'.
          SCREEN-ACTIVE = 1.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        "Validação do para campo tipo
        IF SCREEN-NAME EQ 'WG_HEADER-VTWEG'.
          SCREEN-ACTIVE = 1.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        IF SCREEN-NAME EQ 'WG_HEADER-SPART'.
          SCREEN-ACTIVE = 1.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        IF SCREEN-NAME EQ 'WG_DESC_VTWEG'.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        IF SCREEN-NAME EQ 'WG_DESC_SPART'.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        IF SCREEN-NAME EQ '%#AUTOTEXT005'.
          SCREEN-ACTIVE = 1.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        IF SCREEN-NAME EQ '%#AUTOTEXT006'.
          SCREEN-ACTIVE = 1.
          SCREEN-INVISIBLE = 0.
        ENDIF.
      ELSE.
        IF SCREEN-NAME EQ 'TAB_STRIP_TAB7'
        OR SCREEN-NAME EQ 'TAB_STRIP_TAB8'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        IF WG_HEADER-TIPO = ''.
          IF SCREEN-NAME EQ 'WG_HEADER-VTWEG'.
            SCREEN-ACTIVE = 0.
            SCREEN-INVISIBLE = 1.
          ENDIF.
          IF SCREEN-NAME EQ 'WG_HEADER-SPART'.
            SCREEN-ACTIVE = 0.
            SCREEN-INVISIBLE = 1.
          ENDIF.
        ENDIF.
      ENDIF.

** Pessoa Juridica
      IF WG_HEADER-TIPO_PESSOA EQ 'J'.
        IF SCREEN-NAME EQ 'WG_FISCAIS-STCD2'.
*        OR SCREEN-NAME EQ '%#AUTOTEXT002' .
          SCREEN-INPUT = 0.

        ENDIF.

** Pessoa Fisica
      ELSEIF WG_HEADER-TIPO_PESSOA EQ 'F'.
        IF SCREEN-NAME EQ 'WG_FISCAIS-STCD1'
        OR SCREEN-NAME EQ 'WG_FISCAIS-STCD3'
        OR SCREEN-NAME EQ 'WG_FISCAIS-TXJCD'
        OR SCREEN-NAME EQ 'WG_FISCAIS-KVERM'.
          SCREEN-INPUT = 0.

        ENDIF.
      ELSE.
        IF SCREEN-NAME EQ 'WG_FISCAIS-STCD1'
        OR SCREEN-NAME EQ 'WG_FISCAIS-STCD3'
        OR SCREEN-NAME EQ 'WG_FISCAIS-TXJCD'
        OR SCREEN-NAME EQ 'WG_FISCAIS-KVERM'
        OR SCREEN-NAME EQ 'WG_FISCAIS-STCD2'.
          SCREEN-INPUT = 0.

        ENDIF.

      ENDIF.

      IF SY-TCODE = 'ZFI0046'.
        IF SCREEN-NAME EQ 'TAB_STRIP_TAB2'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ELSEIF SCREEN-NAME EQ 'TAB_STRIP_TAB5'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ELSEIF SCREEN-NAME EQ 'TAB_STRIP_TAB6'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ELSEIF SCREEN-NAME EQ 'TAB_STRIP_TAB7'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
      ENDIF.

      MODIFY SCREEN.

    ENDLOOP.
  ENDIF.
*
*ENDLOOP.
*  CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
*    EXPORTING
*      IS_STABLE = WA_STABLE.

  PERFORM VERIFICA_ERROS.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      I_SCREEN      = '100'
*      I_SHOW        = 'X'
      I_REPID       = SY-REPID
      I_POPUP       = 0
      I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
      I_SET_FIELD   = 'X_FIELD'
      I_SET_CELL    = 'WG_CELL'
      I_SET_OBJ     = 'WG_OBJ'
    IMPORTING
      E_MESSAGEM    = WG_MENSAGEM
    TABLES
      IT_MSGS       = TG_MSG_RET.

  IF X_FIELD IS NOT INITIAL.
    SET CURSOR FIELD X_FIELD."'WG_DESC_OPERACAO'.
  ENDIF.

  IF WG_CELL IS NOT INITIAL .
    REFRESH: TG_CELL.
    APPEND WG_CELL TO TG_CELL.
*    CALL METHOD GRID1->SET_SELECTED_CELLS
*      EXPORTING
*        IT_CELLS = TG_CELL[].
  ENDIF.

ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  get_next_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_7995   text
*      -->P_7996   text
*      <--P_VL_NRONC  text
*----------------------------------------------------------------------*
FORM GET_NEXT_NUMBER  USING    P_OBJECT   "TYPE nrobj
                               P_NR_RANGE "TYPE nrnr
                      CHANGING P_NUMBER.

  CLEAR P_NUMBER.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = P_NR_RANGE
      OBJECT                  = P_OBJECT
    IMPORTING
      NUMBER                  = P_NUMBER
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      BUFFER_OVERFLOW         = 7
      OTHERS                  = 8.
  IF SY-SUBRC NE 0.
    CLEAR: P_NUMBER.
    MESSAGE E836(SD) WITH 'O intervalo de numeração,'
                      'não foi encontrado!'.
  ELSE.
    WG_FLAG = C_X.
  ENDIF.

ENDFORM.                    " get_next_number
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_DADOS_DOC.
  DATA: WL_HEADER      TYPE ZMMT0049,
        WL_FISCAIS     TYPE ZMMT0051,
        WL_CONTAB_PGTO TYPE ZMMT0054,
        WL_COMPRA      TYPE ZMMT0055,
        WL_VENDA       TYPE ZMMT0056,
        TL_ENDEREC     TYPE TABLE OF ZMMT0050 WITH HEADER LINE,
        TL_BANCARIO    TYPE TABLE OF ZMMT0052 WITH HEADER LINE,
        TL_IMPOSTOS    TYPE TABLE OF ZMMT0053 WITH HEADER LINE.

  SELECT SINGLE *
    FROM ZMMT0049
    INTO WL_HEADER
     WHERE NR_SOL EQ WG_HEADER-NR_SOL.

  IF SY-SUBRC IS INITIAL.
    SELECT SINGLE *
      FROM ZMMT0051
      INTO WL_FISCAIS
        WHERE NR_SOL EQ WG_HEADER-NR_SOL.

    SELECT SINGLE *
      FROM ZMMT0054
      INTO WL_CONTAB_PGTO
       WHERE NR_SOL EQ WG_HEADER-NR_SOL.

    SELECT SINGLE *
      FROM ZMMT0055
      INTO WL_COMPRA
       WHERE NR_SOL EQ WG_HEADER-NR_SOL.

    SELECT SINGLE *
      FROM ZMMT0056
      INTO WL_VENDA
       WHERE NR_SOL EQ WG_HEADER-NR_SOL.


    SELECT  *
      FROM ZMMT0050
      INTO TABLE TL_ENDEREC
       WHERE NR_SOL EQ WG_HEADER-NR_SOL.

    SELECT  *
      FROM ZMMT0052
      INTO TABLE TL_BANCARIO
       WHERE NR_SOL EQ WG_HEADER-NR_SOL.

    SELECT  *
      FROM ZMMT0053
      INTO TABLE TL_IMPOSTOS
       WHERE NR_SOL EQ WG_HEADER-NR_SOL.

    PERFORM MONTA_DADOS_DOC TABLES TL_ENDEREC
                                   TL_BANCARIO
                                   TL_IMPOSTOS
                            USING  WL_HEADER
                                   WL_FISCAIS
                                   WL_CONTAB_PGTO
                                   WL_COMPRA
                                   WL_VENDA.


  ELSE.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'O documento de solicitação'
                                           'não foi encontrado'.
  ENDIF.
ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  MONTA_DADOS_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_0041  text
*      -->P_WL_0040  text
*----------------------------------------------------------------------*
FORM MONTA_DADOS_DOC  TABLES   TL_ENDEREC  STRUCTURE ZMMT0050
                               TL_BANCARIO STRUCTURE ZMMT0052
                               TL_IMPOSTOS STRUCTURE ZMMT0053
                      USING    WL_HEADER      TYPE ZMMT0049
                               WL_FISCAIS     TYPE ZMMT0051
                               WL_CONTAB_PGTO TYPE ZMMT0054
                               WL_COMPRA      TYPE ZMMT0055
                               WL_VENDA       TYPE ZMMT0056.

  MOVE-CORRESPONDING: WL_HEADER      TO WG_HEADER,
                      WL_FISCAIS     TO WG_FISCAIS,
                      WL_CONTAB_PGTO TO WG_CONTAB_PGTO,
                      WL_COMPRA      TO WG_COMPRA,
                      WL_VENDA       TO WG_VENDA.


  READ TABLE TL_ENDEREC
    WITH KEY TIPO = C_C.
  IF SY-SUBRC IS INITIAL.
    MOVE-CORRESPONDING: TL_ENDEREC TO WG_ENDEREC_C.
  ENDIF.

  READ TABLE TL_ENDEREC
     WITH KEY TIPO = C_R.
  IF SY-SUBRC IS INITIAL.
    MOVE-CORRESPONDING: TL_ENDEREC TO WG_ENDEREC_R.
  ENDIF.

  LOOP AT TL_BANCARIO.
    MOVE-CORRESPONDING TL_BANCARIO TO TG_BANCARIO.

    CASE TG_BANCARIO-BKONT.
      WHEN '01'.
        MOVE 'Conta Corrente' TO TG_BANCARIO-BKONTT.
      WHEN '02'.
        MOVE 'Poupança' TO TG_BANCARIO-BKONTT.
      WHEN '03'.
        MOVE 'Conta Salário' TO TG_BANCARIO-BKONTT.
      WHEN OTHERS.
    ENDCASE.

    APPEND TG_BANCARIO.
    CLEAR: TG_BANCARIO.
  ENDLOOP.

  LOOP AT TL_IMPOSTOS.
    MOVE-CORRESPONDING: TL_IMPOSTOS TO TG_IMPOSTOS.

    APPEND TG_IMPOSTOS.
    CLEAR: TG_IMPOSTOS.
  ENDLOOP.


ENDFORM.                    " MONTA_DADOS_DOC
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GRAVA_DADOS .
  DATA: WL_INPUT_0049 TYPE ZMMT0049,
        WL_INPUT_0051 TYPE ZMMT0051,
        WL_INPUT_0054 TYPE ZMMT0054,
        WL_INPUT_0055 TYPE ZMMT0055,
        WL_INPUT_0056 TYPE ZMMT0056,
        TL_INPUT_0056 TYPE TABLE OF ZMMT0056 WITH HEADER LINE,
        TL_INPUT_0052 TYPE TABLE OF ZMMT0052 WITH HEADER LINE,
        TL_INPUT_0050 TYPE TABLE OF ZMMT0050 WITH HEADER LINE,
        TL_INPUT_0053 TYPE TABLE OF ZMMT0053 WITH HEADER LINE.


  MOVE-CORRESPONDING: WG_HEADER      TO WL_INPUT_0049,
                      WG_FISCAIS     TO WL_INPUT_0051,
                      WG_CONTAB_PGTO TO WL_INPUT_0054,
                      WG_COMPRA      TO WL_INPUT_0055,
                      WG_VENDA       TO WL_INPUT_0056.

  MOVE: SY-UNAME TO WL_INPUT_0049-USNAM,
        SY-DATUM TO WL_INPUT_0049-DATA_ATUAL,
        SY-UZEIT TO WL_INPUT_0049-HORA_ATUAL.

  MOVE: WG_HEADER-NR_SOL TO WG_ENDEREC_C-NR_SOL,
        C_C              TO WG_ENDEREC_C-TIPO,
        WG_HEADER-NR_SOL TO WG_ENDEREC_R-NR_SOL,
        C_R              TO WG_ENDEREC_R-TIPO,
        WG_HEADER-NR_SOL TO WL_INPUT_0051-NR_SOL,
        WG_HEADER-NR_SOL TO WL_INPUT_0054-NR_SOL.

  IF WG_HEADER-TIPO EQ C_F.
    MOVE:  WG_HEADER-NR_SOL TO WL_INPUT_0055-NR_SOL.
  ELSEIF WG_HEADER-TIPO EQ C_C.
    MOVE:  WG_HEADER-NR_SOL TO WL_INPUT_0056-NR_SOL.
  ENDIF.

  APPEND:  WG_ENDEREC_C TO TL_INPUT_0050,
           WG_ENDEREC_R TO TL_INPUT_0050.

  LOOP AT TG_BANCARIO.
    MOVE: WG_HEADER-NR_SOL TO  TL_INPUT_0052-NR_SOL.
    MOVE-CORRESPONDING: TG_BANCARIO TO TL_INPUT_0052.

    CASE TG_BANCARIO-BKONTT.
      WHEN 'CONTA CORRENTE'.
        MOVE '01' TO TL_INPUT_0052-BKONT.
      WHEN 'POUPANÇA'.
        MOVE '02' TO TL_INPUT_0052-BKONT.
      WHEN 'CONTA SALÁRIO'.
        MOVE '03' TO TL_INPUT_0052-BKONT.
      WHEN OTHERS.
    ENDCASE.

    APPEND TL_INPUT_0052.
    CLEAR: TL_INPUT_0052.
  ENDLOOP.

  LOOP AT TG_IMPOSTOS.
    MOVE: WG_HEADER-NR_SOL TO  TL_INPUT_0053-NR_SOL.
    MOVE-CORRESPONDING: TG_IMPOSTOS TO TL_INPUT_0053.

    APPEND TL_INPUT_0053.
    CLEAR: TL_INPUT_0053.
  ENDLOOP.


  DELETE FROM ZMMT0049 WHERE NR_SOL EQ WL_INPUT_0049-NR_SOL.
  DELETE FROM ZMMT0050 WHERE NR_SOL EQ WL_INPUT_0049-NR_SOL.
  DELETE FROM ZMMT0051 WHERE NR_SOL EQ WL_INPUT_0049-NR_SOL.
  DELETE FROM ZMMT0052 WHERE NR_SOL EQ WL_INPUT_0049-NR_SOL.
  DELETE FROM ZMMT0053 WHERE NR_SOL EQ WL_INPUT_0049-NR_SOL.
  DELETE FROM ZMMT0054 WHERE NR_SOL EQ WL_INPUT_0049-NR_SOL.
  IF WG_HEADER-TIPO EQ C_F.
    DELETE FROM ZMMT0055 WHERE NR_SOL EQ WL_INPUT_0049-NR_SOL.
  ELSEIF WG_HEADER-TIPO EQ C_C.
    DELETE FROM ZMMT0056 WHERE NR_SOL EQ WL_INPUT_0049-NR_SOL.
  ENDIF.
*
  MODIFY ZMMT0049 FROM WL_INPUT_0049.
  MODIFY ZMMT0051 FROM WL_INPUT_0051.
  MODIFY ZMMT0054 FROM WL_INPUT_0054.
  IF WG_HEADER-TIPO EQ C_F.
    MODIFY ZMMT0055 FROM WL_INPUT_0055.
  ELSEIF WG_HEADER-TIPO EQ C_C.
    MODIFY ZMMT0056 FROM WL_INPUT_0056.
  ENDIF.
  MODIFY ZMMT0050 FROM TABLE TL_INPUT_0050.
  MODIFY ZMMT0052 FROM TABLE TL_INPUT_0052.
  MODIFY ZMMT0053 FROM TABLE TL_INPUT_0053.

  MESSAGE S836(SD) WITH 'Doc. Solicitação'
                         WL_INPUT_0049-NR_SOL
                         ', criado/modificado com sucesso!'.

*  call function 'DEQUEUE_EZSDT0040'
*    exporting
*      doc_simulacao = wg_header-doc_simulacao.

  PERFORM LIMPA_VARIAVEL USING C_ATUAL.
  WG_ACAO = C_ATUAL.
  LEAVE TO SCREEN 100.
ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_DADOS .
  CLEAR:     WG_DESC_BUKRS,
             WG_DESC_EKORG,
             WG_DESC_VTWEG,
             WG_DESC_SPART,
             WG_DESC_FDGRV,
             WG_DESC_ZAHLS,
             WG_DESC_ZGRUP,
             WG_DESC_ZTERM,
             WG_DESC_ZTERM2,
             WG_DESC_KALKS,
             WG_DESC_VERSG,
             WG_DESC_KTGRD,
             WG_DESC_KTOKK.


  SELECT SINGLE BUTXT
   FROM T001
    INTO WG_DESC_BUKRS
     WHERE SPRAS EQ SY-LANGU
       AND BUKRS EQ WG_HEADER-BUKRS.

  SELECT SINGLE EKOTX
    FROM T024E
     INTO WG_DESC_EKORG
      WHERE EKORG EQ WG_HEADER-EKORG.

  SELECT SINGLE VTEXT
     FROM TVTWT
     INTO WG_DESC_VTWEG
      WHERE SPRAS EQ SY-LANGU
        AND VTWEG EQ WG_HEADER-VTWEG.

  SELECT SINGLE VTEXT
    FROM TSPAT
    INTO WG_DESC_SPART
     WHERE SPRAS EQ SY-LANGU
       AND SPART EQ WG_HEADER-SPART.

  SELECT SINGLE TEXTL
      FROM T035T
      INTO WG_DESC_FDGRV
       WHERE SPRAS EQ SY-LANGU
         AND GRUPP EQ WG_CONTAB_PGTO-FDGRV.

  SELECT SINGLE TEXTL
    FROM T008T
    INTO WG_DESC_ZAHLS
     WHERE SPRAS EQ SY-LANGU
       AND ZAHLS EQ WG_CONTAB_PGTO-ZAHLS.

  SELECT SINGLE TTEXT
   FROM TZGRT
   INTO WG_DESC_ZGRUP
    WHERE SPRAS EQ SY-LANGU
      AND ZGRUP EQ WG_CONTAB_PGTO-ZGRUP.

  SELECT SINGLE VTEXT
      FROM TVZBT
      INTO WG_DESC_ZTERM
       WHERE SPRAS EQ SY-LANGU
         AND ZTERM EQ WG_CONTAB_PGTO-ZTERM.

  SELECT SINGLE VTEXT
     FROM TVZBT
     INTO WG_DESC_ZTERM2
      WHERE SPRAS EQ SY-LANGU
        AND ZTERM EQ WG_COMPRA-ZTERM.

  SELECT SINGLE VTEXT
     FROM TVKDT
     INTO WG_DESC_KALKS
      WHERE SPRAS EQ SY-LANGU
        AND KALKS EQ WG_VENDA-KALKS.

  SELECT SINGLE VTEXT
     FROM TVSGT
     INTO WG_DESC_VERSG
      WHERE SPRAS EQ SY-LANGU
        AND VERSG EQ WG_VENDA-VERSG.

  SELECT SINGLE VTEXT
     FROM TVKTT
     INTO WG_DESC_KTGRD
      WHERE SPRAS EQ SY-LANGU
        AND KTGRD EQ WG_VENDA-KTGRD.

  SELECT SINGLE TXT30
     FROM T077Y
     INTO WG_DESC_KTOKK
      WHERE SPRAS EQ SY-LANGU
        AND KTOKK EQ WG_HEADER-KTOKK.
ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  VALIDA_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UNAME  text
*      -->P_T_FIELDCATALOG  text
*----------------------------------------------------------------------*
FORM VALIDA_LAYOUT  TABLES   TL_FIELDCATALOG STRUCTURE LVC_S_FCAT
                     USING   UNAME.

*  DATA: TL_PARAMETROS TYPE USTYP_T_PARAMETERS,
*        WL_PARAMETROS TYPE USTYP_PARAMETERS,
*        WL_FIELDCATALOG TYPE LVC_S_FCAT,
*        WL_VARIANTE01 TYPE ZVARIANTE01,
*        TL_VARIANTE02_ALV    TYPE TABLE OF ZVARIANTE02 WITH HEADER LINE,
*        TL_VARIANTE02_SCREEN TYPE TABLE OF ZVARIANTE02 WITH HEADER LINE,
*        WL_TABIX TYPE SY-TABIX,
*        WL_ATRIBUTO(30).
*
*  REFRESH: TL_PARAMETROS, TL_VARIANTE02_ALV, TL_VARIANTE02_SCREEN.
*  FIELD-SYMBOLS: <FS_ATRIBUTOS> TYPE ANY.
*
*  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
*    EXPORTING
*      USER_NAME                 = UNAME
**   WITH_TEXT                 =
*    TABLES
*      USER_PARAMETERS           = TL_PARAMETROS
*    EXCEPTIONS
*      USER_NAME_NOT_EXIST       = 1
*      OTHERS                    = 2
*            .
*  IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*  READ TABLE TL_PARAMETROS INTO WL_PARAMETROS
*    WITH KEY PARID = 'ZVARIANTE'.
*  IF SY-SUBRC IS INITIAL.
*    SELECT SINGLE *
*      FROM ZVARIANTE01
*      INTO WL_VARIANTE01
*       WHERE GRPVA EQ WL_PARAMETROS-PARVA
*         AND TCODE EQ SY-TCODE.
*
*    IF SY-SUBRC IS INITIAL.
*      CONDENSE WL_VARIANTE01-GRPVA NO-GAPS.
*      SELECT *
*        FROM ZVARIANTE02
*        INTO TABLE TL_VARIANTE02_ALV
*         WHERE GRPVA   EQ WL_VARIANTE01-GRPVA
*           AND TCODE   EQ SY-TCODE
*           AND ATR_TIP EQ 'ALV'
*           AND DYNNR   EQ SY-DYNNR.
*
*      SELECT *
*        FROM ZVARIANTE02
*        INTO TABLE TL_VARIANTE02_SCREEN
*         WHERE GRPVA   EQ WL_VARIANTE01-GRPVA
*           AND TCODE   EQ SY-TCODE
*           AND ATR_TIP NE 'ALV'
*           AND DYNNR   EQ SY-DYNNR.
*
*    ENDIF.
*    IF TL_VARIANTE02_SCREEN[] IS NOT INITIAL
*    AND ( SY-TCODE NE 'SE38'
*       AND SY-TCODE NE 'SE80' ).
*      LOOP AT SCREEN.
*        READ TABLE TL_VARIANTE02_SCREEN
*          WITH KEY FIELD = SCREEN-NAME.
*
*        IF SY-SUBRC IS INITIAL.
*          IF ( TL_VARIANTE02_SCREEN-ACAO IS NOT INITIAL
*          AND TL_VARIANTE02_SCREEN-ACAO EQ WG_ACAO )
*            OR TL_VARIANTE02_SCREEN-ACAO IS INITIAL.
*            UNASSIGN <FS_ATRIBUTOS>.
*            CONCATENATE 'SCREEN' TL_VARIANTE02_SCREEN-ATR_TIP INTO WL_ATRIBUTO SEPARATED BY '-'.
*            ASSIGN (WL_ATRIBUTO) TO <FS_ATRIBUTOS>.
*            IF <FS_ATRIBUTOS> IS ASSIGNED.
*              <FS_ATRIBUTOS> = TL_VARIANTE02_SCREEN-FATR_VALUE.
*              MODIFY SCREEN.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*    IF TL_VARIANTE02_ALV[] IS INITIAL
*    AND ( SY-TCODE EQ 'SE38'
*       OR SY-TCODE EQ 'SE80' ).
*      EXIT.
*    ENDIF.
*    LOOP AT TL_FIELDCATALOG INTO WL_FIELDCATALOG.
*      WL_TABIX = SY-TABIX.
*      READ TABLE TL_VARIANTE02_ALV
*        WITH KEY FIELD = WL_FIELDCATALOG-FIELDNAME.
*      IF SY-SUBRC IS NOT  INITIAL.
*        IF ( TL_VARIANTE02_SCREEN-ACAO IS NOT INITIAL
*            AND TL_VARIANTE02_SCREEN-ACAO EQ WG_ACAO )
*              OR TL_VARIANTE02_SCREEN-ACAO IS INITIAL.
*          DELETE TL_FIELDCATALOG INDEX WL_TABIX.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*  ELSE.
*    SELECT SINGLE *
*      FROM ZVARIANTE01
*      INTO WL_VARIANTE01
*       WHERE DEFAULT_VAR EQ C_X
*         AND TCODE EQ SY-TCODE.
*
*    IF SY-SUBRC IS INITIAL.
*      SELECT *
*        FROM ZVARIANTE02
*        INTO TABLE TL_VARIANTE02_ALV
*         WHERE GRPVA   EQ WL_VARIANTE01-GRPVA
*           AND TCODE   EQ SY-TCODE
*           AND ATR_TIP EQ 'ALV'
*           AND DYNNR   EQ SY-DYNNR.
*
*      SELECT *
*         FROM ZVARIANTE02
*         INTO TABLE TL_VARIANTE02_SCREEN
*          WHERE GRPVA   EQ WL_VARIANTE01-GRPVA
*            AND TCODE   EQ SY-TCODE
*            AND ATR_TIP NE 'ALV'
*            AND DYNNR   EQ SY-DYNNR.
*    ENDIF.
*    IF TL_VARIANTE02_SCREEN[] IS NOT INITIAL
*        AND ( SY-TCODE NE 'SE38'
*           AND SY-TCODE NE 'SE80' ).
*      LOOP AT SCREEN.
*        READ TABLE TL_VARIANTE02_SCREEN
*          WITH KEY FIELD = SCREEN-NAME.
*
*        IF SY-SUBRC IS INITIAL.
*          IF ( TL_VARIANTE02_SCREEN-ACAO IS NOT INITIAL
*            AND TL_VARIANTE02_SCREEN-ACAO EQ WG_ACAO )
*              OR TL_VARIANTE02_SCREEN-ACAO IS INITIAL.
*            UNASSIGN <FS_ATRIBUTOS>.
*            CONCATENATE 'SCREEN' TL_VARIANTE02_SCREEN-ATR_TIP INTO WL_ATRIBUTO SEPARATED BY '-'.
*            ASSIGN (WL_ATRIBUTO) TO <FS_ATRIBUTOS>.
*            IF <FS_ATRIBUTOS> IS ASSIGNED.
*              <FS_ATRIBUTOS> = TL_VARIANTE02_SCREEN-FATR_VALUE.
*              MODIFY SCREEN.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*
*    IF TL_VARIANTE02_ALV[] IS INITIAL
*    AND ( SY-TCODE EQ 'SE38'
*       OR SY-TCODE EQ 'SE80' ).
*      EXIT.
*    ENDIF.
*    LOOP AT TL_FIELDCATALOG INTO WL_FIELDCATALOG.
*      WL_TABIX = SY-TABIX.
*      READ TABLE TL_VARIANTE02_ALV
*        WITH KEY FIELD = WL_FIELDCATALOG-FIELDNAME.
*      IF SY-SUBRC IS NOT  INITIAL.
*        IF ( TL_VARIANTE02_ALV-ACAO IS NOT INITIAL
*            AND TL_VARIANTE02_ALV-ACAO EQ WG_ACAO )
*              OR TL_VARIANTE02_ALV-ACAO IS INITIAL.
*          DELETE TL_FIELDCATALOG INDEX WL_TABIX.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*  ENDIF.

ENDFORM.                    " VALIDA_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  BUILD_DROPDOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUILD_DROPDOWN .
  DATA:   LS_DROPDOWN      TYPE LVC_S_DRAL,
          LT_DROPDOWN      TYPE LVC_T_DRAL.


  LS_DROPDOWN-HANDLE = '1'.

  LS_DROPDOWN-INT_VALUE = 'Conta Corrente'.
  LS_DROPDOWN-VALUE = 'Conta Corrente'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN.

  LS_DROPDOWN-INT_VALUE = 'Poupança'.
  LS_DROPDOWN-VALUE = 'Poupança'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN.

  LS_DROPDOWN-INT_VALUE = 'Conta Salário'.
  LS_DROPDOWN-VALUE = 'Conta Salário'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN.

* Übergabe der Dropdown-Tabelle an ALV-Grid-Control
  CALL METHOD GRID1->SET_DROP_DOWN_TABLE
    EXPORTING
      IT_DROP_DOWN_ALIAS = LT_DROPDOWN.
ENDFORM.                    "BUILD_DROPDOWN2

*&---------------------------------------------------------------------*
*&      Module  SEARCH_KTOKK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_KTOKK INPUT.
  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
            TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_KTOKK OCCURS 0,
          KTOKK  TYPE T077K-KTOKK,
          TXT30  TYPE T077Y-TXT30,
         END OF TL_KTOKK.

  SELECT KTOKK TXT30
    FROM T077Y
    INTO TABLE TL_KTOKK
    WHERE SPRAS = SY-LANGU.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'KTOKK'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'T077K-KTOKK'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_KTOKK
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDMODULE.                 " SEARCH_KTOKK  INPUT
