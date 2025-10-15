**/===========================================================================\*
**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa  ( welgem.barbosa@amaggi.com.br )                     |*
**|                                                                           |*
**|    + Time Que FEZ    ( suporte.sap@amaggi.com.br )                        |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Carolini Santos ( carolini.santos@amaggi.com.br )                    |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Contratos de Arrendamentos                                                |*
**/===========================================================================\*

REPORT zglr070 MESSAGE-ID zfi.

TABLES: zglt094, zglt092, zglt096, zglt095, j_1bbranch, t001, kna1, lfa1, skat.

CONSTANTS: BEGIN OF gl_acao,
             novo          TYPE sy-ucomm VALUE 'NEW',
             modificar     TYPE sy-ucomm VALUE 'EDIT',
             exibir        TYPE sy-ucomm VALUE 'LOOK',
             voltar        TYPE sy-ucomm VALUE 'BACK',
             sair          TYPE sy-ucomm VALUE 'EXIT',
             cancelar      TYPE sy-ucomm VALUE 'CANCEL',
             salvar        TYPE sy-ucomm VALUE 'SAVE',
             contabilidade TYPE sy-ucomm VALUE 'BTNCON',
             matricula     TYPE sy-ucomm VALUE 'BTNMAT',
             fluxo         TYPE sy-ucomm VALUE 'BTNFLUX',
             cli_for       TYPE sy-ucomm VALUE 'BTNCF',
             arr_pre       TYPE sy-ucomm VALUE 'BTNAP',
           END OF gl_acao.

CONSTANTS: BEGIN OF c_tab_strip,
             tab1 LIKE sy-ucomm VALUE 'TAB_STRIP_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_STRIP_FC2',
           END OF c_tab_strip.
CONTROLS:  tab_strip TYPE TABSTRIP.

TYPES BEGIN OF ty_0092.
        INCLUDE TYPE zglt092.
TYPES: bukrs_d      TYPE butxt,
       lifial_d     TYPE name1,
       fornecedor_d TYPE name1_gp,
       cliente_d    TYPE name1,
       filial_d     TYPE name1.
TYPES END OF ty_0092.

*TYPES BEGIN OF TY_0096.
*        INCLUDE TYPE ZGLT096.
*TYPES: STATUS TYPE CHAR10.
*TYPES END OF TY_0096.
*
*TYPES BEGIN OF TY_0094.
*        INCLUDE TYPE ZGLT094.
*TYPES END OF TY_0094.

TYPES BEGIN OF ty_0095.
        INCLUDE TYPE zglt095.
TYPES: desc_classificacao(20) TYPE c,
       desc_taxa(20)          TYPE c,
       descricao_conta        TYPE zgle0031,
       desc_tp_arrend(20)     TYPE c,
       desc_tp_lanc(20)       TYPE c,
       celltab                TYPE lvc_t_styl.
TYPES END OF ty_0095.

DATA: it_096       TYPE TABLE OF zde_zglt096_alv,
      it_094       TYPE TABLE OF zde_zglt094_alv,
      it_096_save  TYPE TABLE OF zglt096,
      it_094_save  TYPE TABLE OF zglt094,
      it_095       TYPE TABLE OF ty_0095,
      it_095_save  TYPE TABLE OF zglt095,
      it_095_del   TYPE TABLE OF zglt095,
      wa_095       TYPE ty_0095,
      save_092     TYPE zglt092,
      wa_092       TYPE ty_0092,
      save_095     TYPE zglt095,
      ln_mes(2)    TYPE n,
      it_class     TYPE TABLE OF  dd07v,
      it_taxa      TYPE TABLE OF  dd07v,
      it_tp_arrend TYPE TABLE OF  dd07v,
      it_tp_lanc   TYPE TABLE OF  dd07v.

DATA: dg_splitter     TYPE REF TO cl_gui_splitter_container,
      ctl_cccontainer TYPE REF TO cl_gui_container.

CLASS lcl_event_handler DEFINITION DEFERRED.

DATA: grid        TYPE REF TO cl_gui_alv_grid,
      eventos     TYPE REF TO lcl_event_handler,
      it_fieldcat TYPE lvc_t_fcat,
      wa_fieldcat TYPE lvc_s_fcat,
      tl_function TYPE ui_functions,
      wl_function LIKE tl_function  WITH HEADER LINE,
      it_estilo   TYPE lvc_t_styl,
      wa_layout   TYPE lvc_s_layo,
      vg_radiof,
      vg_mode,
      vg_radioc,
      wa_stable   TYPE lvc_s_stbl VALUE 'XX'.

DATA custom  TYPE REF TO cl_gui_custom_container.
DATA grid01  TYPE REF TO cl_gui_alv_grid.
DATA ck_alterou   TYPE char01.
DATA ck_bloqueado TYPE char01.


DATA: BEGIN OF g_tab_strip,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZGLR070',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip-tab1,
      END OF g_tab_strip.

DATA: str  TYPE REF TO data.
DATA: zmes(2)             TYPE c,
      zano(4)             TYPE c,
      v_tp_arrendaento(1) TYPE c,
      _domvalue_l         TYPE dd07v-domvalue_l.
"Tipo 1 e 2
SELECTION-SCREEN BEGIN OF SCREEN 1001.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_cont TYPE zglt094-cod_contrato OBLIGATORY,
            p_mes  LIKE zmes,
            p_ano  LIKE zano.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 1001.

"Tipo 3
SELECTION-SCREEN BEGIN OF SCREEN 1003.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
PARAMETERS : p_cont2  TYPE zglt094-cod_contrato,
             p_dtvenc TYPE zglt094-dt_vencimento.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN END OF SCREEN 1003.

"Tipo 4
SELECTION-SCREEN BEGIN OF SCREEN 1004.
SELECTION-SCREEN BEGIN OF BLOCK b3  WITH FRAME TITLE text-001.
PARAMETERS: p_cont3 TYPE zglt094-cod_contrato OBLIGATORY,
            p_ano3  TYPE zano.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF SCREEN 1004.


*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_CONT-LOW.
*  PERFORM Z_BUSCA_CONTRATOS.
*
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_cont2.
  PERFORM z_busca_contratos.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_cont3.
  PERFORM z_busca_contratos.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dtvenc.

  DATA: BEGIN OF tl_0094 OCCURS 0,
          dt_vencimento TYPE zglt094-dt_vencimento,
          num_parcela   TYPE zglt094-num_parcela,
        END OF tl_0094.


  IF p_cont2 IS INITIAL.
    MESSAGE 'Favor informe o Nº Contrato !'  TYPE 'I'.
    EXIT.
  ELSE.
    SELECT  dt_vencimento
            num_parcela
      FROM zglt094 INTO TABLE tl_0094
      WHERE cod_contrato EQ p_cont2.

    IF sy-subrc EQ 0.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield    = 'DT_VENCIMENTO'
          dynpprog    = sy-repid
          dynpnr      = sy-dynnr
          dynprofield = 'ZGLT094-DT_VENCIMENTO'
          value_org   = 'S'
        TABLES
          value_tab   = tl_0094.
    ELSE.
      MESSAGE 'Nº Contrato não existe!' TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.



CLASS cl_main DEFINITION.
  PUBLIC SECTION.

    DATA: error_in_data  TYPE c,
          error_in_data2 TYPE c.

    CLASS-METHODS run.

    METHODS process_before_output.
    METHODS set_title_and_status.
    METHODS set_screen.
    METHODS set_descricao.
    METHODS set_modelagem.
    METHODS set_modelagem_096 CHANGING rg96 TYPE zde_zglt096_alv.
    METHODS set_modelagem_094 CHANGING rg94 TYPE zde_zglt094_alv.
    METHODS get_help IMPORTING input TYPE char1.
    METHODS set_acao IMPORTING input TYPE char1.
    METHODS set_cancel.
    METHODS set_validar RETURNING VALUE(ck_validado) TYPE char01.
    METHODS set_grava.
    METHODS set_outtab_data EXCEPTIONS data_not_found.
    METHODS display.

    METHODS handle_set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object.
    METHODS handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
    METHODS handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.
    METHODS handle_data_changed2 FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.
    METHODS handle_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.

    "METHODS handle_set_toolbar2 FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object.
    METHODS handle_user_command2 FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
    METHODS handle_data_changed_finished2 FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.
    METHODS add_parcela IMPORTING input TYPE n RETURNING VALUE(return) TYPE int4.
    METHODS get_fieldcatalog IMPORTING input TYPE char1 OPTIONAL RETURNING VALUE(fcat) TYPE lvc_t_fcat.
    METHODS get_sort IMPORTING input TYPE char1 OPTIONAL RETURNING VALUE(sort) TYPE lvc_t_sort.

    METHODS refresh_screen.

    DATA: at_antigo TYPE sy-ucomm.

  PRIVATE SECTION.

    DATA: at_acao  TYPE sy-ucomm VALUE 'NN',
          at_where TYPE string.

    DATA custom_grid1    TYPE REF TO cl_gui_custom_container.
    DATA grid1           TYPE REF TO cl_gui_alv_grid.

    DATA custom_grid2    TYPE REF TO cl_gui_custom_container.

    DATA grid2           TYPE REF TO cl_gui_alv_grid.

    METHODS:
      perform_semantic_checks
        IMPORTING
          pr_data_changed TYPE REF TO cl_alv_changed_data_protocol,
      perform_semantic_checks2
        IMPORTING
          pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.

CLASS lcl_event_f4 DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS: on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING e_display e_fieldname e_fieldvalue er_event_data es_row_no et_bad_cells sender.

ENDCLASS.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.

    METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender,

      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells sender.

  PRIVATE SECTION.
    METHODS:
      perform_semantic_checks
        IMPORTING
          pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.


CLASS lcl_event_f4 IMPLEMENTATION.
  METHOD on_f4.
    TYPES: BEGIN OF ty_field,
             tabname   TYPE dd03l-tabname,
             fieldname TYPE dd03l-fieldname,
             s(1)      TYPE c,
           END OF ty_field.

    TYPES: BEGIN OF ty_value,
             tabname     TYPE dd03l-tabname,      "Nome da tabela
             fieldname   TYPE dd03l-fieldname,    "Nome de campo
             char79(100) TYPE c,
           END OF ty_value.

    TYPES: BEGIN OF ty_h_tbsl,
             bschl TYPE tbsl-bschl,
             koart TYPE tbsl-koart,
             shkzg TYPE tbsl-shkzg,
             ltext TYPE tbslt-ltext,
           END OF ty_h_tbsl.

    TYPES: BEGIN OF ty_gl_acct_cc,
             saknr TYPE skb1-saknr,
             txt20 TYPE skat-txt20,
             bukrs TYPE skb1-bukrs,
             sakan TYPE ska1-sakan,
             ktopl TYPE ska1-ktopl,
             altkt TYPE skb1-altkt,
           END OF    ty_gl_acct_cc.

    TYPES: BEGIN OF  ty_h_tabw_restr,
             bwasl  TYPE tabw-bwasl,
             bwatxt TYPE tabwt-bwatxt,
             xobs   TYPE tabw-xobs,
           END OF ty_h_tabw_restr.

    TYPES: BEGIN OF ty_t074u,
             umskz TYPE t074u-umskz,
             koart TYPE t074u-koart,
             ltext TYPE t074t-ltext,
           END OF ty_t074u.

    DATA: BEGIN OF wl_chave_lcto,
            field(50),
          END OF wl_chave_lcto.

    DATA: BEGIN OF wl_conta,
            field(50),
          END OF wl_conta.

    DATA: BEGIN OF wl_razao_especial,
            field(50),
          END OF wl_razao_especial.

    DATA: BEGIN OF wl_tipo_mov_i,
            field(50),
          END OF wl_tipo_mov_i.

    DATA: BEGIN OF wl_t074u,
            field(50),
          END OF  wl_t074u.


    DATA: tl_chave_lcto     LIKE TABLE OF wl_chave_lcto,
          tl_conta          LIKE TABLE OF wl_conta,
          tl_razao_especial LIKE TABLE OF wl_razao_especial,
          tl_tipo_move_i    LIKE TABLE OF wl_tipo_mov_i,
          tl_t074u          LIKE TABLE OF wl_t074u,
          tl_field          TYPE TABLE OF ty_field,
          it_h_tbsl         TYPE TABLE OF ty_h_tbsl,
          wa_h_tbsl         TYPE ty_h_tbsl,
          it_gl_acct_cc     TYPE TABLE OF ty_gl_acct_cc,
          wa_gl_acct_cc     TYPE ty_gl_acct_cc,
          it_h_tabw_restr   TYPE TABLE OF ty_h_tabw_restr,
          wa_h_tabw_restr   TYPE ty_h_tabw_restr,
          it_t074u          TYPE TABLE OF ty_t074u,
          wa_t074u          TYPE ty_t074u,
          wl_field          TYPE ty_field,
          tl_value          TYPE TABLE OF ty_value,
          wl_value          TYPE ty_value,
          wl_char(20),
          wl_index          TYPE sy-tabix.


    CASE e_fieldname.
      WHEN 'CHAVE_LCTO'.

*        SELECT *
*           FROM TBSL
*          INTO TABLE @DATA(IT_TBSL).
*
*        SELECT *
*          FROM TBSLT
*          INTO TABLE @DATA(IT_TBSLT)
*         FOR ALL ENTRIES IN @IT_TBSL
*          WHERE SPRAS EQ @SY-LANGU
*          AND   BSCHL EQ @IT_TBSL-BSCHL.
*
*        LOOP AT IT_TBSL INTO DATA(WA_TBSL).
*
*          READ TABLE IT_TBSLT INTO DATA(WA_TBSLT) WITH KEY BSCHL = WA_TBSL-BSCHL.
*          IF SY-SUBRC = 0.
*
*            WA_H_TBSL-BSCHL = WA_TBSL-BSCHL.
*            WA_H_TBSL-KOART = WA_TBSL-KOART.
*            WA_H_TBSL-SHKZG = WA_TBSL-SHKZG.
*            WA_H_TBSL-LTEXT = WA_TBSLT-LTEXT.
*            APPEND WA_H_TBSL TO IT_H_TBSL.
*
*            MOVE WA_H_TBSL-BSCHL TO WL_CHAVE_LCTO-FIELD.
*            APPEND WL_CHAVE_LCTO TO TL_CHAVE_LCTO.
*
*            MOVE WA_H_TBSL-KOART TO WL_CHAVE_LCTO-FIELD.
*            APPEND WL_CHAVE_LCTO TO TL_CHAVE_LCTO.
*
*            MOVE WA_H_TBSL-SHKZG TO WL_CHAVE_LCTO-FIELD.
*            APPEND WL_CHAVE_LCTO TO TL_CHAVE_LCTO.
*
*            MOVE WA_H_TBSL-LTEXT TO WL_CHAVE_LCTO-FIELD.
*            APPEND WL_CHAVE_LCTO TO TL_CHAVE_LCTO.
*
*            CLEAR WA_H_TBSL.
*          ENDIF.
*        ENDLOOP.
*
*        WL_FIELD-TABNAME   = 'TBSL'.
*        WL_FIELD-FIELDNAME = 'BSCHL'.
*        WL_FIELD-S         = 'X'.
*        APPEND WL_FIELD TO TL_FIELD.
*
*        WL_FIELD-TABNAME   = 'TBSL'.
*        WL_FIELD-FIELDNAME = 'KOART'.
*        WL_FIELD-S         = ''.
*        APPEND WL_FIELD TO TL_FIELD.
*
*        WL_FIELD-TABNAME   = 'TBSL'.
*        WL_FIELD-FIELDNAME = 'SHKZG'.
*        WL_FIELD-S         = ''.
*        APPEND WL_FIELD TO TL_FIELD.
*
*        WL_FIELD-TABNAME   = 'TBSLT'.
*        WL_FIELD-FIELDNAME = 'LTEXT'.
*        WL_FIELD-S         = ''.
*        APPEND WL_FIELD TO TL_FIELD.
*
*
*        CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
*          EXPORTING
*            FIELDNAME                 = 'BSCHL'
*            TABNAME                   = 'TBSL'
*          IMPORTING
*            INDEX                     = WL_INDEX
*            SELECT_VALUE              = WL_CHAR
*          TABLES
*            FIELDS                    = TL_FIELD
*            SELECT_VALUES             = TL_VALUE
*            VALUETAB                  = TL_CHAVE_LCTO
*          EXCEPTIONS
*            FIELD_NOT_IN_DDIC         = 1
*            MORE_THEN_ONE_SELECTFIELD = 2
*            NO_SELECTFIELD            = 3
*            OTHERS                    = 4.
*
*        IF SY-SUBRC IS INITIAL.
*          READ TABLE IT_H_TBSL INTO WA_H_TBSL INDEX WL_INDEX.
*          IF ES_ROW_NO-ROW_ID GT 0.
*            READ TABLE IT_095 INTO WA_095 INDEX  ES_ROW_NO-ROW_ID.
*            IF SY-SUBRC IS INITIAL.
*              MOVE WA_H_TBSL-BSCHL TO WA_095-CHAVE_LCTO.
*              MODIFY IT_095 FROM WA_095 INDEX  ES_ROW_NO-ROW_ID.
*            ENDIF.
*          ENDIF.
*        ENDIF.

      WHEN 'CONTA'.

*        SELECT *
*          FROM SKB1 INTO TABLE @DATA(IT_SKB1).
*
*        SELECT *
*         FROM SKA1 INTO TABLE @DATA(IT_SKA1)
*         FOR ALL ENTRIES IN @IT_SKB1
*        WHERE SAKNR EQ @IT_SKB1-SAKNR.
*
*        LOOP AT IT_SKB1 INTO DATA(WA_SKB1).
*
*          READ TABLE IT_SKA1 INTO DATA(WA_SKA1) WITH KEY SAKNR = WA_SKB1-SAKNR.
*          IF SY-SUBRC = 0.
*
*            SELECT SINGLE TXT20 FROM SKAT INTO WA_GL_ACCT_CC-TXT20
*              WHERE SPRAS EQ SY-LANGU
*              AND  KTOPL  EQ WA_SKA1-KTOPL
*              AND  SAKNR  EQ WA_SKB1-SAKNR.
*
*            WA_GL_ACCT_CC-BUKRS = WA_SKB1-BUKRS.
*            WA_GL_ACCT_CC-SAKNR = WA_SKB1-SAKNR.
*            WA_GL_ACCT_CC-SAKAN = WA_SKA1-SAKAN.
*            WA_GL_ACCT_CC-KTOPL = WA_SKA1-KTOPL.
*            WA_GL_ACCT_CC-ALTKT = WA_SKB1-ALTKT.
*            APPEND WA_GL_ACCT_CC TO IT_GL_ACCT_CC.
*
*            MOVE WA_GL_ACCT_CC-SAKNR TO WL_CONTA-FIELD.
*            APPEND WL_CONTA TO TL_CONTA.
*
*            MOVE WA_GL_ACCT_CC-TXT20 TO WL_CONTA-FIELD.
*            APPEND WL_CONTA TO TL_CONTA.
*
*            MOVE WA_GL_ACCT_CC-BUKRS TO WL_CONTA-FIELD.
*            APPEND WL_CONTA TO TL_CONTA.
*
*            MOVE WA_GL_ACCT_CC-SAKAN TO WL_CONTA-FIELD.
*            APPEND WL_CONTA TO TL_CONTA.
*
*            MOVE WA_GL_ACCT_CC-KTOPL TO WL_CONTA-FIELD.
*            APPEND WL_CONTA TO TL_CONTA.
*
*            MOVE WA_GL_ACCT_CC-ALTKT TO WL_CONTA-FIELD.
*            APPEND WL_CONTA TO TL_CONTA.
*
*            CLEAR WA_GL_ACCT_CC.
*          ENDIF.
*        ENDLOOP.
*
*        WL_FIELD-TABNAME   = 'SKB1'.
*        WL_FIELD-FIELDNAME = 'SAKNR'.
*        WL_FIELD-S         = 'X'.
*        APPEND WL_FIELD TO TL_FIELD.
*
*        WL_FIELD-TABNAME   = 'SKAT'.
*        WL_FIELD-FIELDNAME = 'TXT20'.
*        WL_FIELD-S = ''.
*        APPEND WL_FIELD TO TL_FIELD.
*
*        WL_FIELD-TABNAME   = 'SKB1'.
*        WL_FIELD-FIELDNAME = 'BUKRS'.
*        WL_FIELD-S         = ''.
*        APPEND WL_FIELD TO TL_FIELD.
*
*        WL_FIELD-TABNAME   = 'SKA1' .
*        WL_FIELD-FIELDNAME = 'SAKAN'.
*        WL_FIELD-S         = ''.
*        APPEND WL_FIELD TO TL_FIELD.
*
*        WL_FIELD-TABNAME   = 'SKA1' .
*        WL_FIELD-FIELDNAME = 'KTOPL'.
*        WL_FIELD-S         = ''.
*        APPEND WL_FIELD TO TL_FIELD.
*
*        WL_FIELD-TABNAME   = 'SKB1' .
*        WL_FIELD-FIELDNAME = 'ALTKT'.
*        WL_FIELD-S         = ''.
*        APPEND WL_FIELD TO TL_FIELD.
*
*
*        CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
*          EXPORTING
*            FIELDNAME                 = 'SAKNR'
*            TABNAME                   = 'SKB1'
*          IMPORTING
*            INDEX                     = WL_INDEX
*            SELECT_VALUE              = WL_CHAR
*          TABLES
*            FIELDS                    = TL_FIELD
*            SELECT_VALUES             = TL_VALUE
*            VALUETAB                  = TL_CONTA
*          EXCEPTIONS
*            FIELD_NOT_IN_DDIC         = 1
*            MORE_THEN_ONE_SELECTFIELD = 2
*            NO_SELECTFIELD            = 3
*            OTHERS                    = 4.
*
*        IF SY-SUBRC IS INITIAL.
*          READ TABLE IT_GL_ACCT_CC INTO WA_GL_ACCT_CC INDEX WL_INDEX.
*          IF ES_ROW_NO-ROW_ID GT 0.
*            READ TABLE IT_095 INTO WA_095 INDEX  ES_ROW_NO-ROW_ID.
*            IF SY-SUBRC IS INITIAL.
*              MOVE WA_GL_ACCT_CC-SAKNR TO WA_095-CONTA.
*              MOVE WA_GL_ACCT_CC-TXT20 TO WA_095-DESCRICAO_CONTA.
*              MODIFY IT_095 FROM WA_095 INDEX  ES_ROW_NO-ROW_ID.
*            ENDIF.
*          ENDIF.
*        ENDIF.

      WHEN 'TIPO_MOV_I'.

*        SELECT *
*          FROM TABW INTO TABLE @DATA(IT_TABW).
*
*        SELECT *
*          FROM TABWT INTO TABLE @DATA(IT_TABWT)
*          FOR  ALL ENTRIES IN @IT_TABW
*          WHERE SPRAS EQ @SY-LANGU
*            AND BWASL EQ @IT_TABW-BWASL.
*
*        LOOP AT  IT_TABW INTO DATA(WA_TABW).
*
*          READ TABLE IT_TABWT INTO DATA(WA_TABWT) WITH KEY BWASL = WA_TABW-BWASL.
*          IF SY-SUBRC = 0.
*            WA_H_TABW_RESTR-BWASL  = WA_TABW-BWASL.
*            WA_H_TABW_RESTR-BWATXT = WA_TABWT-BWATXT.
*            WA_H_TABW_RESTR-XOBS   = WA_TABW-XOBS.
*            APPEND WA_H_TABW_RESTR TO IT_H_TABW_RESTR.
*
*            MOVE WA_H_TABW_RESTR-BWASL TO WL_TIPO_MOV_I-FIELD.
*            APPEND WL_TIPO_MOV_I TO TL_TIPO_MOVE_I.
*
*            MOVE WA_H_TABW_RESTR-BWATXT TO WL_TIPO_MOV_I-FIELD.
*            APPEND WL_TIPO_MOV_I TO TL_TIPO_MOVE_I.
*
*            MOVE WA_H_TABW_RESTR-XOBS TO WL_TIPO_MOV_I-FIELD.
*            APPEND WL_TIPO_MOV_I TO TL_TIPO_MOVE_I.
*
*            CLEAR WA_H_TABW_RESTR.
*          ENDIF.
*        ENDLOOP.
*
*
*        WL_FIELD-TABNAME   = 'TABW'.
*        WL_FIELD-FIELDNAME = 'BWASL'.
*        WL_FIELD-S         = 'X'.
*        APPEND WL_FIELD TO TL_FIELD.
*
*        WL_FIELD-TABNAME   = 'TABWT'.
*        WL_FIELD-FIELDNAME = 'BWATXT'.
*        WL_FIELD-S         = ''.
*        APPEND WL_FIELD TO TL_FIELD.
*
*        WL_FIELD-TABNAME   = 'TABW'.
*        WL_FIELD-FIELDNAME = 'XOBS'.
*        WL_FIELD-S         = ''.
*        APPEND WL_FIELD TO TL_FIELD.
*
*        CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
*          EXPORTING
*            FIELDNAME                 = 'BWASL'
*            TABNAME                   = 'TABW'
*          IMPORTING
*            INDEX                     = WL_INDEX
*            SELECT_VALUE              = WL_CHAR
*          TABLES
*            FIELDS                    = TL_FIELD
*            SELECT_VALUES             = TL_VALUE
*            VALUETAB                  = TL_TIPO_MOVE_I
*          EXCEPTIONS
*            FIELD_NOT_IN_DDIC         = 1
*            MORE_THEN_ONE_SELECTFIELD = 2
*            NO_SELECTFIELD            = 3
*            OTHERS                    = 4.
*
*        IF SY-SUBRC IS INITIAL.
*          READ TABLE IT_H_TABW_RESTR INTO WA_H_TABW_RESTR INDEX WL_INDEX.
*          IF ES_ROW_NO-ROW_ID GT 0.
*            READ TABLE IT_095 INTO WA_095 INDEX  ES_ROW_NO-ROW_ID.
*            IF SY-SUBRC IS INITIAL.
*              MOVE WA_H_TABW_RESTR-BWASL TO WA_095-TIPO_MOV_I.
*              MODIFY IT_095 FROM WA_095 INDEX  ES_ROW_NO-ROW_ID.
*            ENDIF.
*          ENDIF.
*        ENDIF.

      WHEN 'RAZAO_ESPECIAL' .

*        SELECT  SHBKZ  KOART LTEXT
*        FROM T074T  INTO TABLE IT_T074U
*        WHERE SPRAS EQ SY-LANGU.
*
*        LOOP AT IT_T074U INTO WA_T074U.
*
*          MOVE WA_T074U-UMSKZ TO WL_T074U-FIELD.
*          APPEND WL_T074U TO TL_T074U.
*
*          MOVE WA_T074U-KOART TO WL_T074U-FIELD.
*          APPEND WL_T074U TO TL_T074U.
*
*          MOVE WA_T074U-LTEXT TO WL_T074U-FIELD.
*          APPEND WL_T074U TO TL_T074U.
*        ENDLOOP.
*
*        WL_FIELD-TABNAME   = 'T074T'.
*        WL_FIELD-FIELDNAME = 'SHBKZ'.
*        WL_FIELD-S         = 'X'.
*        APPEND WL_FIELD TO TL_FIELD.
*
*        WL_FIELD-TABNAME   = 'T074T'.
*        WL_FIELD-FIELDNAME = 'KOART'.
*        WL_FIELD-S         = ''.
*        APPEND WL_FIELD TO TL_FIELD.
*
*        WL_FIELD-TABNAME   = 'T074T'.
*        WL_FIELD-FIELDNAME = 'LTEXT'.
*        WL_FIELD-S         = ''.
*        APPEND WL_FIELD TO TL_FIELD.
*
*        CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
*          EXPORTING
*            FIELDNAME                 = 'UMSKZ'
*            TABNAME                   = 'T074T'
*          IMPORTING
*            INDEX                     = WL_INDEX
*            SELECT_VALUE              = WL_CHAR
*          TABLES
*            FIELDS                    = TL_FIELD
*            SELECT_VALUES             = TL_VALUE
*            VALUETAB                  = TL_T074U
*          EXCEPTIONS
*            FIELD_NOT_IN_DDIC         = 1
*            MORE_THEN_ONE_SELECTFIELD = 2
*            NO_SELECTFIELD            = 3
*            OTHERS                    = 4.
*
*        IF SY-SUBRC IS INITIAL.
*          READ TABLE IT_T074U INTO WA_T074U INDEX WL_INDEX.
*          IF ES_ROW_NO-ROW_ID GT 0.
*            READ TABLE IT_095 INTO WA_095 INDEX  ES_ROW_NO-ROW_ID.
*            IF SY-SUBRC IS INITIAL.
*              MOVE WA_T074U-UMSKZ TO WA_095-RAZAO_ESPECIAL.
*              MODIFY IT_095 FROM WA_095 INDEX  ES_ROW_NO-ROW_ID.
*            ENDIF.
*          ENDIF.
*        ENDIF.
    ENDCASE.

    CALL METHOD grid->refresh_table_display( is_stable = wa_stable ) .
  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.

    "ERROR_IN_DATA = SPACE.
    CALL METHOD perform_semantic_checks( er_data_changed ).
    "IF ERROR_IN_DATA = 'X'.
    "  CALL METHOD ER_DATA_CHANGED->DISPLAY_PROTOCOL.
    "ENDIF.

  ENDMETHOD.

  METHOD on_data_changed_finished.
    IF e_modified IS NOT INITIAL.
      CALL METHOD grid->refresh_table_display( is_stable = wa_stable ).
    ENDIF.
  ENDMETHOD.


  METHOD perform_semantic_checks.

    CLEAR _domvalue_l.

    LOOP AT pr_data_changed->mt_good_cells INTO DATA(wa_good_cells).

      CASE wa_good_cells-fieldname.

        WHEN 'CHAVE_LCTO'.

          READ TABLE it_095 ASSIGNING FIELD-SYMBOL(<fs_095>) INDEX wa_good_cells-row_id.

          IF wa_good_cells-value IS NOT INITIAL.
            SELECT SINGLE * INTO @DATA(wa_tbsl)
              FROM tbsl
             WHERE bschl EQ @wa_good_cells-value.

            IF sy-subrc IS NOT INITIAL.
              CALL METHOD pr_data_changed->add_protocol_entry
                EXPORTING
                  i_msgid     = 'ZFI'
                  i_msgno     = '096'
                  i_msgty     = 'E'
                  i_msgv1     = wa_good_cells-value
                  i_fieldname = wa_good_cells-fieldname
                  i_row_id    = wa_good_cells-row_id.
            ENDIF.

*            IF wa_tbsl-koart NE 'S' AND wa_tbsl-koart NE 'K' AND wa_tbsl-koart NE 'D'.
*              CALL METHOD pr_data_changed->add_protocol_entry
*                EXPORTING
*                  i_msgid     = 'ZFI'
*                  i_msgno     = '101'
*                  i_msgty     = 'E'
*                  i_msgv1     = wa_tbsl-koart
*                  i_fieldname = wa_good_cells-fieldname
*                  i_row_id    = wa_good_cells-row_id.
*            ENDIF.

            IF <fs_095>-chave_lcto IS NOT INITIAL.
              SELECT SINGLE * INTO @DATA(wa_tbsl_anterior)
                FROM tbsl
               WHERE bschl EQ @<fs_095>-chave_lcto.
            ENDIF.

            READ TABLE pr_data_changed->mt_good_cells INTO DATA(wa_good_cells_conta) WITH KEY fieldname = 'CONTA' row_id = wa_good_cells-row_id.

            IF wa_tbsl-koart NE wa_tbsl_anterior-koart AND sy-subrc IS NOT INITIAL.

              CLEAR: <fs_095>-conta, <fs_095>-descricao_conta.

              CALL METHOD pr_data_changed->modify_cell
                EXPORTING
                  i_row_id    = wa_good_cells-row_id
                  i_fieldname = 'CONTA'
                  i_value     = space.

              CALL METHOD pr_data_changed->modify_cell
                EXPORTING
                  i_row_id    = wa_good_cells-row_id
                  i_fieldname = 'DESCRICAO_CONTA'
                  i_value     = space.
            ENDIF.

            CLEAR: wa_tbsl_anterior, wa_tbsl, wa_good_cells_conta.

          ELSE.

            CLEAR: <fs_095>-conta, <fs_095>-descricao_conta.

            CALL METHOD pr_data_changed->modify_cell
              EXPORTING
                i_row_id    = wa_good_cells-row_id
                i_fieldname = 'CONTA'
                i_value     = space.

            CALL METHOD pr_data_changed->modify_cell
              EXPORTING
                i_row_id    = wa_good_cells-row_id
                i_fieldname = 'DESCRICAO_CONTA'
                i_value     = space.
          ENDIF.

        WHEN 'CONTA'.

          CLEAR: wa_good_cells_conta, wa_tbsl.

          READ TABLE pr_data_changed->mt_good_cells INTO wa_good_cells_conta WITH KEY fieldname = 'CHAVE_LCTO' row_id = wa_good_cells-row_id.
          IF sy-subrc IS INITIAL AND wa_good_cells_conta-value IS NOT INITIAL.
            SELECT SINGLE * INTO @wa_tbsl
              FROM tbsl
             WHERE bschl EQ @wa_good_cells_conta-value.
          ELSE.
            READ TABLE it_095 ASSIGNING <fs_095> INDEX wa_good_cells-row_id.
            IF <fs_095>-chave_lcto IS NOT INITIAL.
              SELECT SINGLE * INTO @wa_tbsl
                FROM tbsl
               WHERE bschl EQ @<fs_095>-chave_lcto.
            ENDIF.
          ENDIF.

          IF wa_good_cells-value IS NOT INITIAL.

            SELECT *
              UP TO 1 ROWS
              FROM skat INTO @DATA(wa_skatx)
              WHERE spras EQ @sy-langu
*                AND   ktopl  EQ @wa_ska1-ktopl
              AND   saknr  EQ @wa_good_cells-value.
            ENDSELECT.

            IF wa_skatx-txt50 IS INITIAL. " *RJF

              CALL METHOD pr_data_changed->modify_cell
                EXPORTING
                  i_row_id    = wa_good_cells-row_id
                  i_fieldname = 'DESCRICAO_CONTA'
                  i_value     = wa_skatx-txt20.
            ELSE.

              CALL METHOD pr_data_changed->modify_cell
                EXPORTING
                  i_row_id    = wa_good_cells-row_id
                  i_fieldname = 'DESCRICAO_CONTA'
                  i_value     = wa_skatx-txt50.
            ENDIF. " rjf

          ENDIF.

          IF wa_good_cells-value IS INITIAL.
            CALL METHOD pr_data_changed->modify_cell
              EXPORTING
                i_row_id    = wa_good_cells-row_id
                i_fieldname = 'DESCRICAO_CONTA'
                i_value     = space.
          ELSEIF wa_tbsl-koart EQ 'S'.

            SELECT  SINGLE *  FROM skb1 INTO  @DATA(wa_skb1)
                    WHERE saknr EQ @wa_good_cells-value.

            SELECT SINGLE *  FROM ska1 INTO  @DATA(wa_ska1)
                    WHERE saknr EQ @wa_skb1-saknr
                      AND saknr NE @space.

            IF sy-subrc EQ 0.

              SELECT SINGLE * FROM skat INTO @DATA(wa_skat)
                WHERE spras EQ @sy-langu
                AND   ktopl  EQ @wa_ska1-ktopl
                AND   saknr  EQ @wa_skb1-saknr.

              CALL METHOD pr_data_changed->modify_cell
                EXPORTING
                  i_row_id    = wa_good_cells-row_id
                  i_fieldname = 'DESCRICAO_CONTA'
                  i_value     = wa_skat-txt50.

            ELSE.
              CALL METHOD pr_data_changed->add_protocol_entry
                EXPORTING
                  i_msgid     = 'ZFI'
                  i_msgno     = '095'
                  i_msgty     = 'E'
                  i_msgv1     = wa_good_cells-value
                  i_fieldname = wa_good_cells-fieldname
                  i_row_id    = wa_good_cells-row_id.
            ENDIF.

          ELSEIF wa_tbsl-koart EQ 'D'.
            "D  Cliente
            "K  Fornecedores
            SELECT SINGLE * INTO @DATA(wa_kna1)
              FROM kna1
             WHERE kunnr EQ @wa_good_cells-value.

            IF sy-subrc IS INITIAL.
              CALL METHOD pr_data_changed->modify_cell
                EXPORTING
                  i_row_id    = wa_good_cells-row_id
                  i_fieldname = 'DESCRICAO_CONTA'
                  i_value     = wa_kna1-name1.
            ELSE.
              CALL METHOD pr_data_changed->add_protocol_entry
                EXPORTING
                  i_msgid     = 'ZFI'
                  i_msgno     = '079'
                  i_msgty     = 'E'
                  i_msgv1     = wa_good_cells-value
                  i_fieldname = wa_good_cells-fieldname
                  i_row_id    = wa_good_cells-row_id.
            ENDIF.

          ELSEIF wa_tbsl-koart EQ 'K'.
            SELECT SINGLE * INTO @DATA(wa_lfa1)
              FROM lfa1
             WHERE lifnr EQ @wa_good_cells-value.

            IF sy-subrc IS INITIAL.
              CALL METHOD pr_data_changed->modify_cell
                EXPORTING
                  i_row_id    = wa_good_cells-row_id
                  i_fieldname = 'DESCRICAO_CONTA'
                  i_value     = wa_lfa1-name1.
            ELSE.
              CALL METHOD pr_data_changed->add_protocol_entry
                EXPORTING
                  i_msgid     = 'ZFI'
                  i_msgno     = '078'
                  i_msgty     = 'E'
                  i_msgv1     = wa_good_cells-value
                  i_fieldname = wa_good_cells-fieldname
                  i_row_id    = wa_good_cells-row_id.
            ENDIF.

          ELSE.

          ENDIF.

        WHEN 'CLASSIFICACAO'.

          READ TABLE it_095 ASSIGNING <fs_095> INDEX wa_good_cells-row_id.
          IF wa_good_cells-value IS NOT INITIAL.

            _domvalue_l = wa_good_cells-value.

            READ TABLE it_class INTO DATA(_class) WITH KEY domvalue_l = _domvalue_l.
            IF sy-subrc EQ 0 .

              CALL METHOD pr_data_changed->modify_cell
                EXPORTING
                  i_row_id    = wa_good_cells-row_id
                  i_fieldname = 'DESC_CLASSIFICACAO'
                  i_value     = _class-ddtext.
            ELSE.

              CALL METHOD pr_data_changed->add_protocol_entry
                EXPORTING
                  i_msgid     = 'ZFI'
                  i_msgno     = '079'
                  i_msgty     = 'E'
                  i_msgv1     = wa_good_cells-value
                  i_fieldname = wa_good_cells-fieldname
                  i_row_id    = wa_good_cells-row_id.
            ENDIF.
          ENDIF.

        WHEN 'TP_TAXA'.

          READ TABLE it_095 ASSIGNING <fs_095> INDEX wa_good_cells-row_id.
          IF wa_good_cells-value IS NOT INITIAL.

            _domvalue_l = wa_good_cells-value.

            READ TABLE it_taxa INTO DATA(_taxa) WITH KEY domvalue_l = _domvalue_l.
            IF sy-subrc EQ 0 .

              CALL METHOD pr_data_changed->modify_cell
                EXPORTING
                  i_row_id    = wa_good_cells-row_id
                  i_fieldname = 'DESC_TAXA'
                  i_value     = _taxa-ddtext.
            ELSE.

              CALL METHOD pr_data_changed->add_protocol_entry
                EXPORTING
                  i_msgid     = 'ZFI'
                  i_msgno     = '079'
                  i_msgty     = 'E'
                  i_msgv1     = wa_good_cells-value
                  i_fieldname = wa_good_cells-fieldname
                  i_row_id    = wa_good_cells-row_id.
            ENDIF.
          ENDIF.

        WHEN 'TP_ARRENDAMENTO'.

          READ TABLE it_095 ASSIGNING <fs_095> INDEX wa_good_cells-row_id.
          IF wa_good_cells-value IS NOT INITIAL.

            _domvalue_l = wa_good_cells-value.

            READ TABLE it_tp_arrend INTO DATA(_tp_arrend) WITH KEY domvalue_l = _domvalue_l.
            IF sy-subrc EQ 0 .

              CALL METHOD pr_data_changed->modify_cell
                EXPORTING
                  i_row_id    = wa_good_cells-row_id
                  i_fieldname = 'DESC_TP_ARREND'
                  i_value     = _tp_arrend-ddtext.
            ELSE.

              CALL METHOD pr_data_changed->add_protocol_entry
                EXPORTING
                  i_msgid     = 'ZFI'
                  i_msgno     = '079'
                  i_msgty     = 'E'
                  i_msgv1     = wa_good_cells-value
                  i_fieldname = wa_good_cells-fieldname
                  i_row_id    = wa_good_cells-row_id.
            ENDIF.
          ENDIF.

        WHEN 'TP_LANCAMENTO'.

          READ TABLE it_095 ASSIGNING <fs_095> INDEX wa_good_cells-row_id.
          IF wa_good_cells-value IS NOT INITIAL.

            _domvalue_l = wa_good_cells-value.

            READ TABLE it_tp_lanc INTO DATA(_tp_lanc) WITH KEY domvalue_l = _domvalue_l.
            IF sy-subrc EQ 0 .

              CALL METHOD pr_data_changed->modify_cell
                EXPORTING
                  i_row_id    = wa_good_cells-row_id
                  i_fieldname = 'DESC_TP_LANC'
                  i_value     = _tp_lanc-ddtext.
            ELSE.

              CALL METHOD pr_data_changed->add_protocol_entry
                EXPORTING
                  i_msgid     = 'ZFI'
                  i_msgno     = '079'
                  i_msgty     = 'E'
                  i_msgv1     = wa_good_cells-value
                  i_fieldname = wa_good_cells-fieldname
                  i_row_id    = wa_good_cells-row_id.
            ENDIF.
          ENDIF.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.


DATA r_main TYPE REF TO cl_main.

CLASS cl_main IMPLEMENTATION.

  METHOD run.
    CREATE OBJECT r_main.
    CALL SCREEN 0001.
  ENDMETHOD.

  METHOD process_before_output.
    "//set title
    me->set_title_and_status( ).

    "//set data
    me->set_outtab_data( ).

    "// set screen
    me->set_screen( ).

    "//display data
    me->display( ).

  ENDMETHOD.

  METHOD set_title_and_status.
    DATA: it_tcode TYPE TABLE OF sy-ucomm.

    CLEAR: it_tcode.

    IF r_main->at_antigo EQ gl_acao-exibir.
      APPEND 'SAVE' TO it_tcode.
    ENDIF.

    SET TITLEBAR 'MAIN_TITLE'.
    SET PF-STATUS 'MAIN_STATUS' EXCLUDING it_tcode.
  ENDMETHOD.

  METHOD set_modelagem.

    DATA: wa_094      TYPE zglt094,
          lv_num_parc TYPE zgle0021,
          lv_dats     TYPE zgle0012.

    CASE wa_092-tp_arrendamento.
      WHEN '01' OR '04' OR '05'.
        v_tp_arrendaento = 'A'.
      WHEN '02' OR '03'.
        v_tp_arrendaento = 'B'.
    ENDCASE.

    IF wa_092-identcf EQ 'C'. "OR wa_092-identcf is initial. "RJF
*      vg_radiof = abap_true.
*    ELSE.
      vg_radiof = abap_false.
      vg_radioc = abap_true.
    ENDIF.

    IF wa_092-identcf EQ 'F'. "OR wa_092-identcf is initial. "RJF
*      vg_radiof = abap_true.
*    ELSE.
      vg_radioc = abap_false.
      vg_radiof = abap_true.
    ENDIF.

                                                            "BUG 99430
*    IF vg_mode EQ 'M' OR vg_mode EQ 'N'.
*      FREE it_094.
*    ENDIF.
*
*    CLEAR lv_num_parc.

    LOOP AT it_096 ASSIGNING FIELD-SYMBOL(<f_096>).

      me->set_modelagem_096(
        CHANGING
          rg96 = <f_096>
      ).

*      IF V_TP_ARRENDAENTO EQ 'B'.
*        IF WA_092-TP_ARRENDAMENTO EQ '02'.
*          <F_096>-SACAS_P_ANO = <F_096>-AREA_HA * <F_096>-SACAS_P_HA.
*        ENDIF.
*        <F_096>-TOTAL_SACAS  = ( <F_096>-SACAS_P_ANO  * <F_096>-PRAZO_SAFRAS ) .
*        <F_096>-VLR_CONTRATO = ( <F_096>-TOTAL_SACAS  * <F_096>-PRECO_SACAS  ).
*      ENDIF.
      "bug 99430 - BG - 05-01-23 inicio
*      IF it_094[] IS INITIAL OR ( vg_mode EQ 'M' OR vg_mode EQ 'N' ).
**        CLEAR lv_num_parc.
*        IF <f_096>-prazo_safras IS NOT INITIAL.
*          DO <f_096>-prazo_safras TIMES.
*            lv_num_parc = lv_num_parc + 1.
**            wa_094-cod_contrato = 'TIPO02'.
*            wa_094-cod_contrato = wa_092-cod_contrato.
*            wa_094-num_parcela = lv_num_parc.
**            wa_094-cli_for = <f_096>-cli_for.
**            wa_094-nome = <f_096>-nome.
*            lv_dats = ( <f_096>-vigencia_de + 365 ).
*            CONCATENATE <f_096>-vigencia_de(4) '/' lv_dats(4) INTO wa_094-safra.
**            wa_094-safra = lv_dats(4).
*            wa_094-periodo_de = <f_096>-vigencia_de.
*            wa_094-periodo_ate = <f_096>-vigencia_ate.
*            wa_094-dt_vencimento = <f_096>-vigencia_ate.
*            wa_094-valor_brl = <f_096>-vlr_contrato / <f_096>-prazo_safras.
**wa_094-VALOR_USD =
**wa_094-NUM_DOCUMENTO =
*            APPEND wa_094 TO it_094.
*            CLEAR wa_094.
*          ENDDO.
*        ENDIF.
*      ENDIF.
      "bug 99430 - BG - 05-01-23 FIM
    ENDLOOP.

    LOOP AT it_094 ASSIGNING FIELD-SYMBOL(<fs_094>).

*      me->set_modelagem_094(
*        CHANGING
*          rg94 = <fs_094>
*      ).

*      CASE WA_092-TP_ARRENDAMENTO.
*        WHEN '01' OR '02'.
*          IF WA_092-TX_USD_FUTURO IS NOT INITIAL AND <F_094>-VALOR_BRL IS NOT INITIAL.
*            <F_094>-VALOR_USD = <F_094>-VALOR_BRL / WA_092-TX_USD_FUTURO.
*          ENDIF.
*        WHEN '03' OR '04'.
*
*          IF WA_092-TX_CONTRATO IS NOT INITIAL.
*            <F_094>-VALOR_USD = <F_094>-VALOR_BRL / WA_092-TX_CONTRATO.
*          ELSE.
*            <F_094>-VALOR_USD = 0.
*          ENDIF.
*
*      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD set_modelagem_096.

    DATA: ld_spa TYPE tslxx12,
          ld_ts  TYPE tslxx12.

    DATA:  lv_valcont         TYPE p DECIMALS 2.

    rg96-status = COND #( WHEN rg96-vigencia_ate < sy-datum THEN 'Encerrado' ELSE 'Vigente' ).

    CASE wa_092-tp_arrendamento.
      WHEN '01'.

      WHEN '02'.
        rg96-sacas_p_ano  = ( rg96-area_ha    * rg96-sacas_p_ha  ).
        rg96-total_sacas  = ( rg96-prazo_safras * rg96-sacas_p_ano ).
*        rg96-vlr_contrato = ( rg96-total_sacas  * rg96-preco_sacas ).

        rg96-vlr_contrato = ( rg96-total_sacas  * rg96-preco_sacas ).
        lv_valcont = CONV #( rg96-vlr_contrato ). "RJF solicitação Amaury para teste arrendodamento alv
        rg96-vlr_contrato = CONV #( lv_valcont ).

      WHEN '03'.
        ld_spa = CONV #( rg96-area_ha    * rg96-sacas_p_ha  ).
        rg96-sacas_p_ano  = ld_spa.
*        rg96-sacas_p_ano  = ( rg96-area_ha    * rg96-sacas_p_ha  ).
*        rg96-total_sacas  = ( rg96-prazo_safras * rg96-sacas_p_ano ).
        ld_ts  = CONV #( rg96-prazo_safras * rg96-sacas_p_ano ).
        rg96-total_sacas  =  ld_ts.
        rg96-vlr_contrato = ( rg96-total_sacas  * rg96-preco_sacas ).

        lv_valcont = CONV #( rg96-vlr_contrato ). "RJF solicitação Amaury para teste arrendodamento alv
        rg96-vlr_contrato = CONV #( lv_valcont ).

      WHEN '05'.
*        rg96-sacas_p_ano  = ( rg96-area_ha    * rg96-sacas_p_ha  ).
*        rg96-total_sacas  = ( rg96-prazo_safras * rg96-sacas_p_ano ).
*        rg96-vlr_contrato = ( rg96-total_sacas  * rg96-preco_sacas ).
        rg96-vlr_contrato = ( rg96-area_ha  * rg96-preco_sacas ).
    ENDCASE.

*    LOOP AT it_094 ASSIGNING FIELD-SYMBOL(<fs_094>).
*
*      me->set_modelagem_094(
*        CHANGING
*          rg94 = <fs_094>
*      ).
*
*    ENDLOOP.

  ENDMETHOD.

  METHOD set_modelagem_094.

    CASE wa_092-tp_arrendamento.

      WHEN '01'.

        IF wa_092-tx_usd_futuro IS NOT INITIAL AND rg94-valor_brl IS NOT INITIAL.
          rg94-valor_usd = rg94-valor_brl / wa_092-tx_usd_futuro.
        ELSE.
          rg94-valor_usd = 0.
        ENDIF.

      WHEN '02'.

        IF rg94-dt_vencimento IS NOT INITIAL.
          LOOP AT it_096 INTO DATA(wa_096) WHERE vigencia_de <= rg94-dt_vencimento AND vigencia_ate >= rg94-dt_vencimento.
            IF wa_096-prazo_safras > 1.
              rg94-valor_brl = ( wa_096-total_sacas * wa_096-preco_sacas ) / wa_096-prazo_safras.
            ELSE.
              rg94-valor_brl = wa_096-total_sacas * wa_096-preco_sacas.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF wa_092-tx_usd_futuro IS NOT INITIAL AND rg94-valor_brl IS NOT INITIAL.
          rg94-valor_usd = rg94-valor_brl / wa_092-tx_usd_futuro.
        ELSE.
          rg94-valor_usd = 0.
        ENDIF.

      WHEN '03' OR '05'."'04'.

        IF wa_092-tx_contrato IS NOT INITIAL.
          rg94-valor_usd = ( rg94-valor_brl  / wa_092-tx_contrato ).
        ELSE.
          rg94-valor_usd = 0.
        ENDIF.

    ENDCASE.

  ENDMETHOD.

  METHOD set_descricao.

    CHECK me->at_antigo NE ( gl_acao-modificar ).

    IF sy-ucomm EQ 'AC_TP'.
      CASE wa_092-tp_arrendamento.
        WHEN '01' OR '04'.
          v_tp_arrendaento = 'A'.
        WHEN '02' OR '03'.
          v_tp_arrendaento = 'B'.
      ENDCASE.
      EXIT.
    ENDIF.

    CLEAR: wa_092-bukrs_d, wa_092-filial_d, wa_092-fornecedor_d, wa_092-cliente_d, at_where.

    IF wa_092-bukrs IS NOT INITIAL.

      SELECT SINGLE butxt
        FROM t001
        INTO wa_092-bukrs_d
        WHERE bukrs EQ wa_092-bukrs.

      at_where =
      COND #( WHEN at_where IS INITIAL
                        THEN | BUKRS EQ '{ wa_092-bukrs }'|
                        ELSE | { at_where } AND BUKRS EQ '{ wa_092-bukrs }'|
            ).

    ENDIF.

    IF wa_092-filial IS NOT INITIAL.

      SELECT SINGLE name1
        FROM t001w
        INTO wa_092-filial_d
        WHERE werks EQ wa_092-filial.

      at_where =
      COND #( WHEN at_where IS INITIAL
                         THEN | FILIAL EQ '{ wa_092-filial }'|
                         ELSE | { at_where } AND FILIAL EQ '{ wa_092-filial }'|
            ).
    ENDIF.

    IF wa_092-bukrs IS NOT INITIAL AND wa_092-filial IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(wa_j_1bbranch)
        FROM j_1bbranch
       WHERE branch EQ @wa_092-filial
         AND bukrs  EQ @wa_092-bukrs.

      IF sy-subrc IS INITIAL.

        wa_092-localizacao = wa_j_1bbranch-name.

*. 121595 Arrendamento - Erro Variação USD A receber e campo município - PSA
*        SELECT SINGLE * INTO @DATA(wa_adrc)
*          FROM adrc
*         WHERE addrnumber EQ @wa_j_1bbranch-adrnr.
*
*        IF sy-subrc IS INITIAL.
*          wa_092-municipio = |{ wa_adrc-city1 }|.
*        ENDIF.

      ENDIF.

    ENDIF.

    IF wa_092-fornecedor IS NOT INITIAL.
      SELECT SINGLE name1
        FROM lfa1
        INTO wa_092-fornecedor_d
        WHERE lifnr EQ wa_092-fornecedor.
    ENDIF.

    IF wa_092-cliente IS NOT INITIAL.

      SELECT SINGLE name1
        FROM kna1
        INTO wa_092-cliente_d
        WHERE kunnr EQ wa_092-cliente.

    ENDIF.

    IF wa_092-tp_arrendamento IS NOT INITIAL.

      at_where =
      COND #( WHEN at_where IS INITIAL
                        THEN | TP_ARRENDAMENTO EQ '{ wa_092-tp_arrendamento }'|
                        ELSE | { at_where } AND TP_ARRENDAMENTO EQ '{ wa_092-tp_arrendamento }'|
             ).
    ENDIF.

    IF wa_092-cod_contrato IS NOT INITIAL.

      at_where =
      COND #( WHEN at_where IS INITIAL
                        THEN | COD_CONTRATO EQ '{ wa_092-cod_contrato }'|
                        ELSE | { at_where } AND COD_CONTRATO EQ '{ wa_092-cod_contrato }'|
            ).

    ENDIF.

  ENDMETHOD.

  METHOD set_screen.

    LOOP AT SCREEN.

      CASE at_antigo.
        WHEN gl_acao-novo.

          IF screen-group1 EQ 'N'.
            screen-input = 1.
            MODIFY SCREEN.
          ENDIF.

          CASE wa_092-tp_arrendamento.

            WHEN '02'.
              IF (  screen-name EQ 'BTNAP' ).
*                screen-input = 0.
                screen-invisible = 0.
                MODIFY SCREEN.
              ENDIF.

            WHEN '03' OR '04'.

              IF screen-group1 EQ 'P'.
                screen-input     = 1.
                screen-invisible = 0.
                MODIFY SCREEN.
              ENDIF.
              IF  wa_092-tp_arrendamento EQ '03'.

                IF  screen-name EQ 'WA_092-ANLN1' OR
                   screen-name EQ 'WA_092-ANBWA' OR
                   screen-name EQ 'WA_092-BEWAR'.

                  screen-input = 0.
                  screen-invisible = 1.
                  MODIFY SCREEN.

                ENDIF.

              ENDIF.

              IF (  screen-name EQ 'WA_092-TX_JUROS_DESC' OR
                    screen-name EQ 'WA_092-TX_USD_FUTURO' OR
                    "screen-name EQ 'WA_092-TX_PIS' OR
                    "screen-name EQ 'WA_092-TX_COFINS' OR
                    screen-name EQ 'TXT_TX_JUROS_DESC' ).

                CLEAR: wa_092-tx_usd_futuro,
                       wa_092-tx_juros_desc.

                screen-input = 0.
                screen-invisible = 1.
                MODIFY SCREEN.
              ENDIF.

            WHEN '05'.
              IF screen-name EQ 'WA_092-BUKRSP' .
                screen-invisible = 0.
                MODIFY SCREEN.
              ENDIF.

              IF
                 screen-name EQ 'WA_092-ANLN1' OR
                 screen-name EQ 'WA_092-ANBWA' OR
                 screen-name EQ 'WA_092-BEWAR' OR
                 screen-name EQ 'WA_092-TX_USD_FUTURO' OR
                screen-name EQ 'WA_092-TX_JUROS_DESC' OR
                screen-name EQ 'TXT_TX_JUROS_DESC'.
                screen-input = 0.
                screen-invisible = 1.
                MODIFY SCREEN.
              ENDIF.
          ENDCASE.

        WHEN gl_acao-exibir.

          IF screen-group2 EQ 'L'. "// Look
            screen-input = 1.
            screen-invisible = 0.
            MODIFY SCREEN.
          ENDIF.

          IF screen-group1 EQ 'P'.
            screen-input     = 0.
            screen-invisible = 0.
            MODIFY SCREEN.
          ENDIF.

          CASE wa_092-tp_arrendamento.

            WHEN '02'.
              IF (  screen-name EQ 'BTNAP' ).
*                screen-input = 0.
                screen-invisible = 0.
                MODIFY SCREEN.
              ENDIF.

            WHEN '03' OR '04'.

              IF  wa_092-tp_arrendamento EQ '03'.

                IF  screen-name EQ 'WA_092-ANLN1' OR
                   screen-name EQ 'WA_092-ANBWA' OR
                   screen-name EQ 'WA_092-BEWAR'.
                ELSE.
                  IF (  screen-name EQ 'WA_092-ANLN1' OR
                  screen-name EQ 'WA_092-ANBWA' OR
                  screen-name EQ 'WA_092-BEWAR' ).

                    screen-input = 1.
                    screen-invisible = 0.
                    MODIFY SCREEN.
                  ENDIF.

                ENDIF.

              ENDIF.

              IF (  screen-name EQ 'WA_092-TX_JUROS_DESC' OR
                    screen-name EQ 'WA_092-TX_USD_FUTURO' OR
                    "screen-name EQ 'WA_092-TX_PIS' OR
                    "screen-name EQ 'WA_092-TX_COFINS' OR
                    screen-name EQ 'TXT_TX_JUROS_DESC' ).

                screen-input = 0.
                screen-invisible = 1.
                MODIFY SCREEN.
              ENDIF.




            WHEN '05'.
              IF screen-name EQ 'WA_092-BUKRSP'.
                screen-invisible = 0.
                MODIFY SCREEN.
              ENDIF.

              IF
                 screen-name EQ 'WA_092-ANLN1' OR
                 screen-name EQ 'WA_092-ANBWA' OR
                 screen-name EQ 'WA_092-BEWAR' OR
                 screen-name EQ 'WA_092-TX_USD_FUTURO' OR
                screen-name EQ 'WA_092-TX_JUROS_DESC' OR
                screen-name EQ 'TXT_TX_JUROS_DESC'.
                screen-input = 0.
                screen-invisible = 1.
                MODIFY SCREEN.
              ENDIF.

          ENDCASE.

          IF screen-name = 'WA_092-TP_ARRENDAMENTO'.
            "SCREEN-INPUT     = 0.
            screen-input     = 1.
            screen-invisible = 0.
            MODIFY SCREEN.
          ENDIF.


        WHEN gl_acao-modificar.

          IF screen-group3 EQ 'E' OR screen-group2 EQ 'D'.
            screen-input = 0.
          ELSEIF screen-group1 EQ 'N' OR screen-group2 EQ 'L' .
            screen-input     = 1.
          ENDIF.
          MODIFY SCREEN.

          CASE wa_092-tp_arrendamento.

            WHEN '02'.
              IF (  screen-name EQ 'BTNAP' ).
*                screen-input = 0.
                screen-invisible = 0.
                MODIFY SCREEN.
              ENDIF.

            WHEN '03' OR '04'.

              IF screen-group1 EQ 'P'.
                screen-input     = 1.
                screen-invisible = 0.
                MODIFY SCREEN.
              ENDIF.

              IF (  screen-name EQ 'WA_092-TX_JUROS_DESC' OR
                    screen-name EQ 'WA_092-TX_USD_FUTURO' OR
                    "screen-name EQ 'WA_092-TX_PIS' OR
                    "screen-name EQ 'WA_092-TX_COFINS' OR
                    screen-name EQ 'TXT_TX_JUROS_DESC' ).

                screen-input = 0.
                screen-invisible = 1.
                MODIFY SCREEN.
              ENDIF.

            WHEN '05'.
              IF screen-name EQ 'WA_092-BUKRSP'.
                screen-invisible = 0.
                MODIFY SCREEN.
              ENDIF.

          ENDCASE.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD set_acao.
    at_acao = input.
  ENDMETHOD.

  METHOD set_cancel.
    CLEAR: v_tp_arrendaento, wa_092, save_092, at_where, it_096, it_096_save, it_094, it_094_save.
    me->set_acao( 'W' ).
  ENDMETHOD.

  METHOD set_outtab_data.

    CHECK at_where IS NOT INITIAL.
    CHECK me->at_antigo NE gl_acao-modificar.
    CHECK me->at_antigo NE gl_acao-novo.
    CHECK sy-ucomm NE 'AC_TP'.

    SELECT SINGLE *
      FROM zglt092
      INTO wa_092
      WHERE (at_where).

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM zglt096
        INTO TABLE it_096
        WHERE cod_contrato EQ wa_092-cod_contrato.
* RJF - ini
      IF it_096[] IS NOT INITIAL.
        SORT it_096 BY vigencia_de. " DESCENDING. 96150
      ENDIF.
* RJF - fim
      SELECT *
        FROM zglt094
        INTO TABLE it_094
        WHERE cod_contrato EQ wa_092-cod_contrato.
    ELSE.
      MESSAGE text-e02 TYPE 'S' RAISING data_not_found.
    ENDIF.
    me->set_modelagem( ).
    me->set_descricao( ).

  ENDMETHOD.

  METHOD display.


    DATA: gs_layout   TYPE lvc_s_layo,
          tl_function TYPE ui_functions,
          wl_function LIKE LINE OF tl_function.

*   // Aba Dados do Contrato
    DATA(_fieldcatalog) = me->get_fieldcatalog( EXPORTING input = '1' ).

    IF custom_grid1 IS NOT INITIAL.
      CALL METHOD grid1->free.
      CALL METHOD custom_grid1->free.
    ENDIF.

    CREATE OBJECT custom_grid1
      EXPORTING
        container_name = 'CC01'.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = custom_grid1.

    SET HANDLER: me->handle_set_toolbar  FOR grid1,
                 me->handle_user_command FOR grid1,
                 me->handle_data_changed FOR grid1,
                 me->handle_data_changed_finished FOR grid1.

*    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
*    APPEND wl_function TO tl_function.

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
*       it_toolbar_excluding = tl_function
        is_layout       = gs_layout
        i_save          = abap_true
      CHANGING
        it_outtab       = it_096
        it_fieldcatalog = _fieldcatalog.

    CALL METHOD grid1->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*   // Aba Pagamentos/Recebimentos
    DATA(_fieldcatalog1) = me->get_fieldcatalog( ).
    DATA(_sort)          = me->get_sort( ).

    IF custom_grid2 IS NOT INITIAL.
      CALL METHOD grid2->free.
      CALL METHOD custom_grid2->free.
    ENDIF.

    CREATE OBJECT custom_grid2
      EXPORTING
        container_name = 'CC02'.

    CREATE OBJECT grid2
      EXPORTING
        i_parent = custom_grid2.

*    SET HANDLER: ME->HANDLE_SET_TOOLBAR FOR GRID2.

    SET HANDLER: me->handle_set_toolbar  FOR grid2,
                 me->handle_user_command2 FOR grid2,
                 me->handle_data_changed2 FOR grid2,
                 me->handle_data_changed_finished2 FOR grid2.

    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
        i_save          = abap_true
      CHANGING
        it_outtab       = it_094
        it_fieldcatalog = _fieldcatalog1
        it_sort         = _sort.

    CALL METHOD grid2->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ENDMETHOD.

  METHOD handle_set_toolbar.
    DATA v_break TYPE abap_bool.

    LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<f_toobar>).

      IF <f_toobar>-function EQ '&&SEP01'.
        v_break = abap_true.
      ENDIF.

      IF v_break IS NOT INITIAL.
        <f_toobar>-disabled = abap_true.
      ENDIF.

      IF <f_toobar>-function EQ '&&SEP03'.
        v_break = abap_false.
      ENDIF.

    ENDLOOP.

    DELETE e_object->mt_toolbar WHERE disabled EQ abap_true.

    CHECK at_antigo NE gl_acao-exibir .

    APPEND VALUE #( butn_type = cntb_btype_sep
                  ) TO e_object->mt_toolbar.

    "CHECK sy-ucomm NE 'TAB_STRIP_FC2' AND tab_strip-activetab NE 'TAB_STRIP_FC2'. "RJF

    APPEND VALUE #( butn_type = cntb_btype_button
                    function  = 'ADD'
                    icon      = '@17@'
                    text      = 'Inserir'
                  ) TO e_object->mt_toolbar.

    APPEND VALUE #( butn_type = cntb_btype_button
                    function  = 'DELL'
                    icon      = '@18@'
                    text      = 'Excluir'
                  ) TO e_object->mt_toolbar.


  ENDMETHOD.


  METHOD handle_user_command.

    CALL METHOD grid1->get_selected_rows
      IMPORTING
        et_index_rows = DATA(selected_row).

    CASE e_ucomm.
      WHEN 'ADD'.
        APPEND VALUE #(
                item_contrato = me->add_parcela( 3 )
                cod_contrato = wa_092-cod_contrato
              ) TO it_096.
      WHEN 'DELL'.

        LOOP AT selected_row INTO DATA(wa_selec_row).
          READ TABLE it_096 INTO DATA(wa_096) INDEX wa_selec_row-index.
          DELETE it_096 INDEX wa_selec_row-index.
        ENDLOOP.

    ENDCASE.

    CHECK grid1 IS NOT INITIAL.

    me->refresh_screen( ).

  ENDMETHOD.

  METHOD handle_user_command2.

    CALL METHOD grid2->get_selected_rows
      IMPORTING
        et_index_rows = DATA(selected_row).

    CASE e_ucomm.
      WHEN 'ADD'.
        APPEND VALUE #(
                        num_parcela = me->add_parcela( 4 )
                        cod_contrato = wa_092-cod_contrato
                      ) TO it_094.
      WHEN 'DELL'.

        LOOP AT selected_row INTO DATA(wa_selec_row).
          READ TABLE it_094 INTO DATA(wa_094) INDEX  wa_selec_row-index.
          DELETE it_094 INDEX wa_selec_row-index.
        ENDLOOP.
    ENDCASE.

    me->refresh_screen( ).

  ENDMETHOD.

  METHOD handle_data_changed.

    IF vg_mode EQ 'E'.
      SORT it_096 BY vigencia_de." DESCENDING. Modif.: 96150
    ENDIF.
    error_in_data = abap_false.
    CALL METHOD perform_semantic_checks( er_data_changed ).
    IF error_in_data = abap_true.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.

* RJF - INI

    LOOP AT er_data_changed->mt_good_cells
                                  INTO DATA(ls_good)
                                  WHERE fieldname = 'CLI_FOR'. " CLI_FOR

      IF ls_good-fieldname EQ 'CLI_FOR'.

        IF vg_radiof IS NOT INITIAL.

          SELECT SINGLE name1
          FROM lfa1
          INTO @DATA(lv_name1f)
          WHERE lifnr EQ @ls_good-value.
          IF sy-subrc IS INITIAL.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'NOME'
                i_value     = lv_name1f.

          ENDIF.

        ELSE. " Customer

          SELECT SINGLE name1
          FROM kna1
          INTO @DATA(lv_name1c)
          WHERE kunnr EQ @ls_good-value.
          IF sy-subrc IS INITIAL.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'NOME'
                i_value     = lv_name1c.

          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.
* RJF - FIM

*        READ TABLE tg_itens_cl INTO wl_itens_cl INDEX ls_good-row_id.

*        SELECT SINGLE *
*        FROM j_1bbranch
*        INTO  @DATA(wl_j_1bbranch)
*        WHERE bukrs = @vg_bukrs
*        AND   branch = @ls_good-value.
*
*        IF sy-subrc IS NOT INITIAL AND wl_j_1bbranch IS INITIAL.
*          MESSAGE 'Filial não pertencente a empresa!' TYPE 'I'.
*
*          lv_value = wl_itens_cl-gsber.
*          CALL METHOD er_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = ls_good-row_id
*              i_fieldname = 'GSBER'
*              i_value     = lv_value.
*          FREE lv_value.
*
**        ELSE.
**
**          lv_value = wl_itens_cl-gsber.
**          CALL METHOD er_data_changed->modify_cell
**            EXPORTING
**              i_row_id    = ls_good-row_id
**              i_fieldname = 'GSBER'
**              i_value     = lv_value.
**          FREE lv_value.
*
*        ENDIF.
*
*      ENDLOOP.
*
** RJF - Fim

    me->refresh_screen( ).

  ENDMETHOD.

  METHOD handle_data_changed2.

    error_in_data2 = abap_false.
    CALL METHOD perform_semantic_checks2( er_data_changed ).
    IF error_in_data2 = abap_true.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.

  ENDMETHOD.

  METHOD handle_data_changed_finished.

    CHECK e_modified IS NOT INITIAL.

    LOOP AT et_good_cells INTO DATA(ls_good).

      DATA(wa_096) = it_096[ ls_good-row_id ].

      READ TABLE it_096 INDEX ls_good-row_id ASSIGNING FIELD-SYMBOL(<fs_096>).

      set_modelagem_096(
        CHANGING
          rg96 = <fs_096>
      ).

*      CASE LS_GOOD-FIELDNAME.
*        WHEN 'VIGENCIA_ATE'.
*          IF WA_096-VIGENCIA_ATE IS NOT INITIAL.
*            WA_096-STATUS = COND #( WHEN WA_096-VIGENCIA_ATE < SY-DATUM THEN 'Encerrado' ELSE 'Vigente' ).
*          ENDIF.
*          MODIFY IT_096 FROM WA_096 INDEX LS_GOOD-ROW_ID.
*
*        WHEN 'PRAZO_SAFRAS' OR 'AREA_HA' OR 'SACAS_P_HA' OR 'PRECO_SACAS' OR 'SACAS_P_ANO'.
*
*
*          IF WA_092-TP_ARRENDAMENTO EQ '02'.
*            WA_096-SACAS_P_ANO  = ( WA_096-AREA_HA    * WA_096-SACAS_P_HA  ).
*          ENDIF.
*
*          WA_096-TOTAL_SACAS  = ( WA_096-PRAZO_SAFRAS * WA_096-SACAS_P_ANO ).
*          WA_096-VLR_CONTRATO = ( WA_096-TOTAL_SACAS  * WA_096-PRECO_SACAS ).
*
*          MODIFY IT_096 FROM WA_096 INDEX LS_GOOD-ROW_ID.
*        WHEN OTHERS.
*          MODIFY IT_096 FROM WA_096 INDEX LS_GOOD-ROW_ID.
*      ENDCASE.

    ENDLOOP.

    me->refresh_screen( ).
  ENDMETHOD.

  METHOD handle_data_changed_finished2.

    CHECK e_modified IS NOT INITIAL.

    LOOP AT et_good_cells INTO DATA(ls_good).

*      DATA(WA_094) = IT_094[ LS_GOOD-ROW_ID ].

      READ TABLE it_094 INDEX ls_good-row_id ASSIGNING FIELD-SYMBOL(<fs_094>).

*      set_modelagem_094(
*        CHANGING
*          rg94 = <fs_094>
*      ).

*      CASE WA_092-TP_ARRENDAMENTO.
*
*        WHEN '01'.
*
*          IF WA_092-TX_USD_FUTURO IS NOT INITIAL AND WA_094-VALOR_BRL IS NOT INITIAL.
*            WA_094-VALOR_USD = WA_094-VALOR_BRL / WA_092-TX_USD_FUTURO.
*          ELSE.
*            WA_094-VALOR_USD = 0.
*          ENDIF.
*
*          MODIFY IT_094 FROM WA_094 INDEX LS_GOOD-ROW_ID.
*
*        WHEN '02'.
*          IF WA_094-DT_VENCIMENTO IS NOT INITIAL.
*            LOOP AT IT_096 INTO DATA(WA_096) WHERE VIGENCIA_DE <= WA_094-DT_VENCIMENTO AND VIGENCIA_ATE >= WA_094-DT_VENCIMENTO.
*              IF WA_096-PRAZO_SAFRAS > 1.
*                WA_094-VALOR_BRL = ( WA_096-TOTAL_SACAS * WA_096-PRECO_SACAS ) / WA_096-PRAZO_SAFRAS.
*              ELSE.
*                WA_094-VALOR_BRL = WA_096-TOTAL_SACAS * WA_096-PRECO_SACAS.
*              ENDIF.
*            ENDLOOP.
*          ENDIF.
*
*          IF WA_092-TX_USD_FUTURO IS NOT INITIAL AND WA_094-VALOR_BRL IS NOT INITIAL.
*            WA_094-VALOR_USD = WA_094-VALOR_BRL / WA_092-TX_USD_FUTURO.
*          ELSE.
*            WA_094-VALOR_USD = 0.
*          ENDIF.
*
*          MODIFY IT_094 FROM WA_094 INDEX LS_GOOD-ROW_ID.
*
*        WHEN '03' OR '04'.
*
*          IF LS_GOOD-FIELDNAME EQ 'VALOR_BRL' OR LS_GOOD-FIELDNAME EQ 'VALOR_USD'.
*            IF WA_092-TX_CONTRATO IS NOT INITIAL.
*              WA_094-VALOR_USD = ( WA_094-VALOR_BRL  / WA_092-TX_CONTRATO ).
*            ELSE.
*              WA_094-VALOR_USD = 0.
*            ENDIF.
*            MODIFY IT_094 FROM WA_094 INDEX LS_GOOD-ROW_ID.
*          ENDIF.
*      ENDCASE.
    ENDLOOP.

    me->refresh_screen( ).

  ENDMETHOD.
  METHOD get_sort.
    DATA:
           st_sort     TYPE lvc_s_sort.

    FREE: st_sort, sort.
* trata a ordenação do campo CARRID
*    st_sort-spos  = '2'.
*    st_sort-fieldname  = 'CLI_FOR'.
**    st_sort-seltext   = 'Nome'.
**    st_sort-level  = '0'.
**    st_sort-tabname    = 'ZDE_ZGLT094_ALV'.
*    st_sort-up         = 'X'. " Ordenado do menor pro maior
**    st_sort-down       = 'X'. " Ordenado do menor pro maior
*    st_sort-comp       = 'X'.
*    st_sort-group      = '*'.
*    st_sort-subtot     = 'X'. " declara o campo como subtotal da soma

***    st_sort-spos  = '2'.
***    st_sort-fieldname  = 'NOME'.
****    st_sort-seltext   = 'Nome'.
****    st_sort-level  = '0'.
****    st_sort-tabname    = 'ZDE_ZGLT094_ALV'.
***    st_sort-up         = 'X'. " Ordenado do menor pro maior
****    st_sort-down       = 'X'. " Ordenado do menor pro maior
***    st_sort-comp       = 'X'.
***    st_sort-group      = '*'.
***    st_sort-subtot     = 'X'. " declara o campo como subtotal da soma
***
***    APPEND st_sort TO sort. " Alimenta a tabela com as info da estruc.
***    CLEAR st_sort.             " Limpa a estrutura para não levar sujeira

** trata a ordenação do campo CARRID
*    st_sort-fieldname  = 'VALOR_BRL'.
**    st_sort-tabname    = 'IT_094'.
*    st_sort-up         = 'X'. " Ordenado do menor pro maior
*    st_sort-subtot     = 'X'. " declara o campo como subtotal da soma
**    st_sort-group      = '*'.
*
*    APPEND st_sort TO sort. " Alimenta a tabela com as info da estruc.
*    CLEAR st_sort.             " Limpa a estrutura para não levar sujeira
*
** trata a ordenação do campo CARRID
*    st_sort-fieldname  = 'VALOR_USD'.
**    st_sort-tabname    = 'IT_094'.
*    st_sort-up         = 'X'. " Ordenado do menor pro maior
*    st_sort-subtot     = 'X'. " declara o campo como subtotal da soma
**    st_sort-group      = '*'.

*    APPEND st_sort TO sort. " Alimenta a tabela com as info da estruc.
*    CLEAR st_sort.             " Limpa a estrutura para não levar sujeira
  ENDMETHOD.

  METHOD get_fieldcatalog.
    DATA: vg_dec TYPE char2.

    IF input EQ '1'.

*      ASSIGN 'TY_0096' TO FIELD-SYMBOL(<FS_STR>).
*      CREATE DATA STR TYPE (<FS_STR>).
*      FCAT = CORRESPONDING LVC_T_FCAT( CL_SALV_DATA_DESCR=>READ_STRUCTDESCR( CAST CL_ABAP_STRUCTDESCR( CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA_REF( STR ) ) ) ).

      CLEAR fcat[].

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name = 'ZDE_ZGLT096_ALV'
        CHANGING
          ct_fieldcat      = fcat.

      LOOP AT fcat ASSIGNING FIELD-SYMBOL(<fcat>).
        <fcat>-edit    =  COND #( WHEN me->at_antigo EQ gl_acao-exibir THEN abap_false ELSE abap_true ).
        "<FCAT>-COL_OPT = ABAP_TRUE.
        <fcat>-tabname   = 'ZDE_ZGLT096_ALV'.

        "<FCAT>-F4AVAILABL = ABAP_FALSE.
        CASE <fcat>-fieldname.
          WHEN 'STATUS'.
            <fcat>-edit      = abap_false.
            <fcat>-coltext   = 'Status'.
            <fcat>-col_pos   = 1.
            <fcat>-outputlen = 6.
*          WHEN 'DT_CONT_ADITIVO'. "RJf data contrato aditivo
*            <fcat>-col_pos   = 2.
*            <fcat>-outputlen = 15.
          WHEN 'VIGENCIA_DE'.
            <fcat>-col_pos   = 3.
            <fcat>-outputlen = 11.
          WHEN 'VIGENCIA_ATE'.
            <fcat>-col_pos   = 4.
            <fcat>-outputlen = 12.
*          WHEN 'CLI_FOR'.
*            IF vg_radiof IS NOT INITIAL.
*              <fcat>-coltext   = 'Cliente/Fornecedor'.
**              <fcat>-edit = abap_true.
**              <fcat>-f4availabl = abap_true.
*              <fcat>-ref_field = 'LIFNR'.
*              <fcat>-ref_table = 'LFA1'.
*              <fcat>-col_pos    = 5.
*              <fcat>-decimals_o = 4.
*              <fcat>-outputlen  = 12.
*            ELSE.
*              <fcat>-coltext   = 'Cliente/Fornecedor'.
**              <fcat>-edit = abap_true.
**              <fcat>-f4availabl = abap_true.
*              <fcat>-ref_field = 'KUNNR'.
*              <fcat>-ref_table = 'KNA1'.
*              <fcat>-col_pos    = 5.
*              <fcat>-decimals_o = 4.
*              <fcat>-outputlen  = 12.
*            ENDIF.
*          WHEN 'NOME'.
*            IF vg_radiof IS NOT INITIAL.
*              <fcat>-coltext   = 'Nome'.
**              <fcat>-edit = abap_true.
**              <fcat>-f4availabl = abap_true.
*              <fcat>-ref_field = 'NAME1'.
*              <fcat>-ref_table = 'LFA1'.
*              <fcat>-col_pos    = 6.
*              <fcat>-decimals_o = 4.
*              <fcat>-outputlen  = 12.
*            ELSE.
*              <fcat>-coltext   = 'Nome'.
**              <fcat>-edit = abap_true.
**              <fcat>-f4availabl = abap_true.
*              <fcat>-ref_field = 'NAME1'.
*              <fcat>-ref_table = 'KNA1'.
*              <fcat>-col_pos    = 6.
*              <fcat>-decimals_o = 4.
*              <fcat>-outputlen  = 12.
*            ENDIF.
          WHEN 'AREA_HA'.
            "<FCAT>-EDIT        =  COND #( WHEN WA_092-TP_ARRENDAMENTO EQ '03' OR ME->AT_ANTIGO EQ GL_ACAO-EXIBIR THEN ABAP_FALSE ELSE ABAP_TRUE ).
            "<FCAT>-EDIT        =  COND #( WHEN ME->AT_ANTIGO EQ GL_ACAO-EXIBIR THEN ABAP_FALSE ELSE ABAP_TRUE ).
            <fcat>-col_pos     = 7.
            <fcat>-decimals_o  = 4.
            <fcat>-outputlen   = 10.
            <fcat>-no_out     = COND #( WHEN wa_092-tp_arrendamento EQ '04' THEN abap_true ELSE abap_false ).
            <fcat>-do_sum     = abap_true. "RJF
          WHEN 'PRAZO_SAFRAS'.
            <fcat>-col_pos    = 8.
            <fcat>-decimals_o = 4.
            <fcat>-outputlen  = 12.
          WHEN 'SACAS_P_HA'.
            <fcat>-col_pos    = 9.
            <fcat>-decimals_o = 15.
            <fcat>-outputlen  = 40.
            <fcat>-no_out     = COND #( WHEN v_tp_arrendaento EQ 'B' THEN abap_false ELSE abap_true ).
          WHEN 'SACAS_P_ANO'.
            "<FCAT>-EDIT       =  COND #( WHEN WA_092-TP_ARRENDAMENTO EQ '02' OR ME->AT_ANTIGO EQ GL_ACAO-EXIBIR THEN ABAP_FALSE ELSE ABAP_TRUE )  .
            <fcat>-edit       =  COND #( WHEN wa_092-tp_arrendamento EQ '02' OR me->at_antigo EQ gl_acao-exibir THEN abap_false ELSE abap_true )  .
            <fcat>-col_pos    = 10.
            <fcat>-decimals_o = 2.
            <fcat>-no_out     = COND #( WHEN v_tp_arrendaento EQ 'B' THEN abap_false ELSE abap_true ).
            <fcat>-outputlen  = 12.
          WHEN 'TOTAL_SACAS'.
            <fcat>-edit = abap_false.
            <fcat>-col_pos    = 11.
            <fcat>-decimals_o = 2.
            <fcat>-no_out     = COND #( WHEN v_tp_arrendaento EQ 'B' THEN abap_false ELSE abap_true ).
            <fcat>-outputlen  = 12.
          WHEN 'PRECO_SACAS'.
*            <FCAT>-EDIT = ABAP_FALSE.
            <fcat>-col_pos    = 12.
            <fcat>-decimals_o = 2.
            <fcat>-no_out     = COND #( WHEN v_tp_arrendaento EQ 'B' OR wa_092-tp_arrendamento EQ '05' THEN abap_false ELSE abap_true ).
            <fcat>-outputlen  = 12.
          WHEN 'VLR_CONTRATO'.

            <fcat>-edit       = abap_false.
            <fcat>-col_pos    = 16.
            <fcat>-decimals_o = 2.

*            IF wa_092-tp_arrendamento EQ '05'.
*              <fcat>-edit       = COND #( WHEN me->at_antigo EQ gl_acao-exibir THEN abap_false  ELSE abap_true ).
*            ELSE.
*              <fcat>-edit       = COND #( WHEN v_tp_arrendaento EQ 'B' OR me->at_antigo EQ gl_acao-exibir THEN abap_false  ELSE abap_true ).
*            ENDIF.
            <fcat>-edit       = COND #( WHEN v_tp_arrendaento EQ 'B' OR me->at_antigo EQ gl_acao-exibir THEN abap_false  ELSE abap_true ).
            <fcat>-outputlen  = 12.
            <fcat>-do_sum     = abap_true. "RJF
          WHEN 'PIS'.
            <fcat>-edit = abap_true.
            <fcat>-col_pos    = 14.
            <fcat>-decimals_o = 2.
            <fcat>-coltext   = 'PIS(%)'.
            <fcat>-no_out     = COND #( WHEN wa_092-tp_arrendamento EQ '05' OR wa_092-tp_arrendamento EQ '03' THEN abap_false ELSE abap_true ).
            <fcat>-outputlen  = 12.
            <fcat>-do_sum     = abap_true.
          WHEN 'COFINS'.
            <fcat>-edit = abap_true.
            <fcat>-col_pos    = 15.
            <fcat>-decimals_o = 2.
            <fcat>-coltext   = 'COFINS(%)'.
            <fcat>-no_out     = COND #( WHEN wa_092-tp_arrendamento EQ '05' OR wa_092-tp_arrendamento EQ '03' THEN abap_false ELSE abap_true ).
            <fcat>-outputlen  = 12.
            <fcat>-do_sum     = abap_true.
          WHEN 'PIS_ARREND'.
            <fcat>-edit = abap_true.
            <fcat>-col_pos    = 16.
            <fcat>-decimals_o = 2.
            <fcat>-coltext   = 'PIS Arrendante(%)'.
            <fcat>-no_out     = COND #( WHEN wa_092-tp_arrendamento EQ '05' THEN abap_false ELSE abap_true ).
            <fcat>-outputlen  = 20.
            <fcat>-do_sum     = abap_true.
          WHEN 'COFINS_ARREND'.
            <fcat>-edit = abap_true.
            <fcat>-col_pos    = 17.
            <fcat>-decimals_o = 2.
            <fcat>-coltext   = 'COFINS Arrendante(%)'.
            <fcat>-no_out     = COND #( WHEN wa_092-tp_arrendamento EQ '05' THEN abap_false ELSE abap_true ).
            <fcat>-outputlen  = 20.
            <fcat>-do_sum     = abap_true.
          WHEN OTHERS.
            <fcat>-no_out = abap_true.
        ENDCASE.
      ENDLOOP.

    ELSE.

*      ASSIGN 'TY_0094' TO <FS_STR>.
*      CREATE DATA STR TYPE (<FS_STR>).
*      FCAT = CORRESPONDING LVC_T_FCAT( CL_SALV_DATA_DESCR=>READ_STRUCTDESCR( CAST CL_ABAP_STRUCTDESCR( CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA_REF( STR ) ) ) ).

      CLEAR fcat[].

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name = 'ZDE_ZGLT094_ALV'
        CHANGING
          ct_fieldcat      = fcat.

      LOOP AT fcat ASSIGNING <fcat>.
        <fcat>-edit       =  COND #( WHEN me->at_antigo EQ gl_acao-exibir THEN abap_false ELSE abap_true ).
        "<FCAT>-COL_OPT    = ABAP_TRUE.
        "<FCAT>-F4AVAILABL = ABAP_FALSE.
        <fcat>-tabname   = 'ZDE_ZGLT094_ALV'.
        CASE <fcat>-fieldname.
          WHEN 'NUM_PARCELA'.
            <fcat>-coltext    = 'Nro.Parcela'.
            <fcat>-edit       = abap_false.
            <fcat>-col_pos    = 1.
            <fcat>-outputlen  = 12.
** RJF - Ini - 2022.09.01
*          WHEN 'CLI_FOR'.
*            <fcat>-coltext    = 'Cliente/Fornecedor'.
*            <fcat>-edit       = abap_false.
*            <fcat>-col_pos    = 2.
*            <fcat>-outputlen  = 12.
*          WHEN 'NOME'.
*            <fcat>-coltext    = 'Nome'.
*            <fcat>-edit       = abap_false.
*            <fcat>-col_pos    = 3.
*            <fcat>-outputlen  = 12.
** RJF - Fim - 2022.09.01
*          WHEN 'SAFRA'. "retirar coluna SAFRA COND #( WHEN me->at_antigo EQ gl_acao-exibir THEN abap_false ELSE abap_true  ).
*            <fcat>-coltext    = 'Safra'.
*            <fcat>-col_pos    = 4.
*            <fcat>-edit       = COND #( WHEN me->at_antigo EQ gl_acao-exibir THEN abap_false ELSE abap_true  ).
*            <fcat>-outputlen  = 10.
**            <fcat>-no_out     = COND #( WHEN wa_092-tp_arrendamento EQ '01' OR wa_092-tp_arrendamento EQ '03' OR wa_092-tp_arrendamento EQ '04' THEN abap_true ELSE abap_false ).
          WHEN 'PERIODO_DE'.
            <fcat>-coltext    = 'Período De'.
            <fcat>-col_pos    = 5.
            <fcat>-edit       = COND #( WHEN me->at_antigo EQ gl_acao-exibir THEN abap_false ELSE abap_true  ).
            <fcat>-outputlen  = 10.
            <fcat>-no_out     = COND #( WHEN wa_092-tp_arrendamento EQ '02' OR wa_092-tp_arrendamento EQ '03' THEN abap_true ELSE abap_false ).
          WHEN 'PERIODO_ATE'.
            <fcat>-coltext    = 'Período Até'.
            <fcat>-col_pos    = 6.
            <fcat>-edit       = COND #( WHEN me->at_antigo EQ gl_acao-exibir THEN abap_false ELSE abap_true  ).
            <fcat>-outputlen  = 11.
            <fcat>-no_out     = COND #( WHEN wa_092-tp_arrendamento EQ '02' OR wa_092-tp_arrendamento EQ '03' THEN abap_true ELSE abap_false ).
          WHEN 'DT_VENCIMENTO'.
            <fcat>-coltext    = 'Data Vencimento'.
            <fcat>-col_pos    = 7.
            <fcat>-outputlen  = 15.
          WHEN 'VALOR_BRL'.
            <fcat>-coltext    = 'Valor BRL'.
            <fcat>-col_pos    = 8.
            <fcat>-decimals_o = 2.
            <fcat>-outputlen  = 12.
            <fcat>-do_sum     = abap_true. "RJF
          WHEN 'VALOR_USD'.
            <fcat>-coltext    = 'Valor USD'.
            <fcat>-edit       = COND #( WHEN me->at_antigo EQ gl_acao-exibir THEN abap_false ELSE abap_true  ).  "bug  99430
            <fcat>-col_pos    = 9.
            <fcat>-decimals_o = 2.
            <fcat>-outputlen  = 12.
            <fcat>-do_sum     = abap_true. "RJF
          WHEN 'NUM_DOCUMENTO'.
            <fcat>-coltext   =  COND #( WHEN wa_092-tp_arrendamento EQ '02'  THEN 'Doc. Contábil Pgto' ELSE  'Documento' ).
            <fcat>-edit      = COND #( WHEN me->at_antigo EQ gl_acao-exibir THEN abap_false ELSE abap_true  ).  "bug  99430
            <fcat>-col_pos   = 10.
            <fcat>-outputlen = 12.
          WHEN 'DT_PAGAMNETO'.
            <fcat>-coltext    = 'Data Pagamento'.
            <fcat>-col_pos    = 11.
            <fcat>-outputlen  = 15.
            <fcat>-no_out     = COND #( WHEN wa_092-tp_arrendamento EQ '02'  THEN abap_false ELSE abap_true ).
          WHEN 'VALOR_PAGAMENTO'.
            <fcat>-coltext    = 'Valor Pagamento'.
            <fcat>-col_pos    = 12.
            <fcat>-decimals_o = 2.
            <fcat>-outputlen  = 12.
            <fcat>-do_sum     = abap_true.
         WHEN 'SACA_ACUMULADA'.
            <fcat>-coltext    = 'Saca Acumulada'.
            <fcat>-col_pos    = 12.
            <fcat>-decimals_o = 2.
            <fcat>-outputlen  = 12.
            <fcat>-no_out     = COND #( WHEN wa_092-tp_arrendamento EQ '03' THEN abap_false ELSE abap_true ).
          WHEN OTHERS.
            <fcat>-no_out = abap_true.
        ENDCASE.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD add_parcela.

    IF input EQ 4.
      return = lines( it_094 ).
    ELSE.
      return = lines( it_096 ).
    ENDIF.
    ADD 1 TO return.
  ENDMETHOD.

  METHOD refresh_screen.

    IF grid1 IS NOT INITIAL.
      grid1->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
    ENDIF.

    IF grid1 IS NOT INITIAL.
      grid2->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
    ENDIF.

  ENDMETHOD.

  METHOD set_validar.

    ck_validado = abap_false.

    """ Validar ZGLT092

    IF wa_092-tp_arrendamento IS INITIAL.
      MESSAGE s082 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

*. 121595 Arrendamento - Erro Variação USD A receber e campo município
    IF wa_092-municipio IS INITIAL.
      MESSAGE s130 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.


    IF wa_092-bukrs IS INITIAL.
      MESSAGE s081 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_t001)
      FROM t001
     WHERE bukrs EQ @wa_092-bukrs.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s075 WITH wa_092-bukrs DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF wa_092-filial IS INITIAL.
      MESSAGE s083 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_j_1bbranch)
      FROM j_1bbranch
     WHERE bukrs  EQ @wa_092-bukrs
       AND branch EQ @wa_092-filial.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s076 WITH wa_092-bukrs wa_092-filial DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF wa_092-cod_contrato IS INITIAL.
      MESSAGE s084 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

* RJF - Ini
*    IF wa_092-fornecedor IS INITIAL AND wa_092-cliente IS INITIAL.
*      MESSAGE s103 DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
*
*    IF wa_092-fornecedor IS NOT INITIAL.
*      TRY .
*          zcl_fornecedores=>zif_parceiros~get_instance(
*            )->set_parceiro( i_parceiro = wa_092-fornecedor
*            )->ck_ativo(
*            )->ck_ativo_empresa( i_empresa = wa_092-bukrs
*            ).
*        CATCH zcx_parceiros INTO DATA(ex_parceiros).
*          ex_parceiros->zif_error~published_erro( EXPORTING i_msgty ='S' i_msgty_display ='E' ).
*          EXIT.
*      ENDTRY.
*    ENDIF.
*
*    IF wa_092-cliente IS NOT INITIAL.
*      TRY .
*          zcl_clientes=>zif_parceiros~get_instance(
*            )->set_parceiro( i_parceiro = wa_092-cliente
*            )->ck_ativo(
*            )->ck_ativo_empresa( i_empresa = wa_092-bukrs
*            ).
*        CATCH zcx_parceiros INTO ex_parceiros.
*          ex_parceiros->zif_error~published_erro( EXPORTING i_msgty ='S' i_msgty_display ='E' ).
*          EXIT.
*      ENDTRY.
*    ENDIF.
* RJF - Fim

    IF ( wa_092-tp_arrendamento EQ '03' OR wa_092-tp_arrendamento EQ '04' ) AND wa_092-tx_contrato LE 0.
      MESSAGE s104 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF wa_092-moeda IS INITIAL.
      MESSAGE s087 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    DATA: wa_moedas TYPE x001.

    CALL FUNCTION 'FI_CURRENCY_INFORMATION'
      EXPORTING
        i_bukrs = wa_092-bukrs
      IMPORTING
        e_x001  = wa_moedas.

    IF wa_092-moeda NE wa_t001-waers AND wa_092-moeda NE wa_moedas-hwae2 AND wa_092-moeda NE wa_moedas-hwae3.
      MESSAGE w088 WITH wa_092-moeda.
    ENDIF.

    DATA(ck_096) = abap_false.
    LOOP AT it_096 INTO DATA(wa_096).

* RJF - Ini

*      IF wa_096-cli_for IS INITIAL.
*        MESSAGE s103 DISPLAY LIKE 'E'.
*        EXIT.
*      ENDIF.
*      IF vg_radiof IS NOT INITIAL.
*        IF wa_096-cli_for IS NOT INITIAL.
*
*          TRY .
*              zcl_fornecedores=>zif_parceiros~get_instance(
*                )->set_parceiro( i_parceiro = wa_096-cli_for
*                )->ck_ativo(
*                )->ck_ativo_empresa( i_empresa = wa_092-bukrs
*                ).
*            CATCH zcx_parceiros INTO DATA(ex_parceiros).
*              ex_parceiros->zif_error~published_erro( EXPORTING i_msgty ='S' i_msgty_display ='E' ).
*              EXIT.
*          ENDTRY.
*        ENDIF.
*      ELSE.
*        IF wa_096-cli_for IS NOT INITIAL.
*          TRY .
*              zcl_clientes=>zif_parceiros~get_instance(
*                )->set_parceiro( i_parceiro = wa_096-cli_for
*                )->ck_ativo(
*                )->ck_ativo_empresa( i_empresa = wa_092-bukrs
*                ).
*            CATCH zcx_parceiros INTO ex_parceiros.
*              ex_parceiros->zif_error~published_erro( EXPORTING i_msgty ='S' i_msgty_display ='E' ).
*              EXIT.
*          ENDTRY.
*        ENDIF.
*      ENDIF.
*
** RJF - Fim

      IF wa_092-tp_arrendamento EQ '01'.

*        IF wa_096-dt_cont_aditivo IS INITIAL.
*          MESSAGE s112 DISPLAY LIKE 'E'.
*          ck_096 = abap_true.
*          EXIT.
*        ENDIF.

        IF wa_096-vigencia_de IS INITIAL.
          MESSAGE s090 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

        IF wa_096-vigencia_ate IS INITIAL.
          MESSAGE s091 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

        IF wa_096-vigencia_ate LT wa_096-vigencia_de.
          MESSAGE s089 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

        "Area (Não Obrigar)

        IF wa_096-prazo_safras IS INITIAL.
          MESSAGE s113 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

        IF wa_096-vlr_contrato IS INITIAL.
          MESSAGE s114 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

      ENDIF.

      IF wa_092-tp_arrendamento EQ '02'.

*        IF wa_096-dt_cont_aditivo IS INITIAL.
*          MESSAGE s112 DISPLAY LIKE 'E'.
*          ck_096 = abap_true.
*          EXIT.
*        ENDIF.

        IF wa_096-vigencia_de IS INITIAL.
          MESSAGE s090 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

        IF wa_096-vigencia_ate IS INITIAL.
          MESSAGE s091 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

        IF wa_096-vigencia_ate LT wa_096-vigencia_de.
          MESSAGE s089 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

        IF wa_096-area_ha IS INITIAL.
          MESSAGE s117 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

        IF wa_096-prazo_safras IS INITIAL.
          MESSAGE s113 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

*        IF wa_096-sacas_p_ha IS INITIAL. PSA
*          MESSAGE s115 DISPLAY LIKE 'E'.
*          ck_096 = abap_true.
*          EXIT.
*        ENDIF.

        IF wa_096-preco_sacas IS INITIAL.
          MESSAGE s116 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

      ENDIF.

      IF wa_092-tp_arrendamento EQ '03'.

*        IF wa_096-dt_cont_aditivo IS INITIAL.
*          MESSAGE s112 DISPLAY LIKE 'E'.
*          ck_096 = abap_true.
*          EXIT.
*        ENDIF.

        IF wa_096-vigencia_de IS INITIAL.
          MESSAGE s090 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

        IF wa_096-vigencia_ate IS INITIAL.
          MESSAGE s091 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

        IF wa_096-vigencia_ate LT wa_096-vigencia_de.
          MESSAGE s089 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

*        IF WA_096-AREA_HA IS INITIAL.
*          MESSAGE S117 DISPLAY LIKE 'E'.
*          CK_096 = ABAP_TRUE.
*          EXIT.
*        ENDIF.

        IF wa_096-prazo_safras IS INITIAL.
          MESSAGE s113 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

*        IF WA_096-SACAS_P_HA IS INITIAL.
*          MESSAGE S115 DISPLAY LIKE 'E'.
*          CK_096 = ABAP_TRUE.
*          EXIT.
*        ENDIF.

        IF wa_096-sacas_p_ano IS INITIAL.
          MESSAGE s120 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

        IF wa_096-preco_sacas IS INITIAL.
          MESSAGE s116 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

      ENDIF.

      IF wa_092-tp_arrendamento EQ '04' OR wa_092-tp_arrendamento EQ '05'.

*        IF wa_096-dt_cont_aditivo IS INITIAL.
*          MESSAGE s112 DISPLAY LIKE 'E'.
*          ck_096 = abap_true.
*          EXIT.
*        ENDIF.

        IF wa_096-vigencia_de IS INITIAL.
          MESSAGE s090 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

        IF wa_096-vigencia_ate IS INITIAL.
          MESSAGE s091 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

        IF wa_096-vigencia_ate LT wa_096-vigencia_de.
          MESSAGE s089 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

*        IF WA_096-AREA_HA IS INITIAL.
*          MESSAGE S117 DISPLAY LIKE 'E'.
*          CK_096 = ABAP_TRUE.
*          EXIT.
*        ENDIF.

        IF wa_096-prazo_safras IS INITIAL.
          MESSAGE s113 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

        IF wa_096-vlr_contrato IS INITIAL.
          MESSAGE s114 DISPLAY LIKE 'E'.
          ck_096 = abap_true.
          EXIT.
        ENDIF.

      ENDIF.

    ENDLOOP.

    CHECK ck_096 = abap_false.

    DATA(ck_094) = abap_false.
    LOOP AT it_094 ASSIGNING FIELD-SYMBOL(<fs_094>).

      IF wa_092-tp_arrendamento EQ '01'.

*108  Deve ser informado o Período De!
        IF <fs_094>-periodo_de IS INITIAL.
          MESSAGE s108 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.

*109  Deve ser informado o Período Até!
        IF <fs_094>-periodo_ate IS INITIAL.
          MESSAGE s109 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.

        IF <fs_094>-periodo_ate LT <fs_094>-periodo_de.
          MESSAGE s092 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.

        IF <fs_094>-dt_vencimento IS INITIAL.
          MESSAGE s093 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.

        IF <fs_094>-valor_brl IS INITIAL.
          MESSAGE s110 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.

        IF <fs_094>-valor_usd IS INITIAL.
          MESSAGE s111 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.

      ENDIF.

      IF wa_092-tp_arrendamento EQ '02'.
*BUG 99430
*        IF <fs_094>-safra IS INITIAL.
*          MESSAGE s107 DISPLAY LIKE 'E'.
*          ck_094 = abap_true.
*          EXIT.
*        ENDIF.
*
*        IF zcl_str=>set( i_texto = CONV #( <fs_094>-safra ) )->matcher( EXPORTING pattern = '[0-9]{4}|[0-9]{4}/[0-9]{4}' ) EQ abap_false.
*          MESSAGE s106 DISPLAY LIKE 'E'.
*          ck_094 = abap_true.
*          EXIT.
*        ENDIF.

        IF <fs_094>-dt_vencimento IS INITIAL.
          MESSAGE s093 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.

        IF <fs_094>-valor_brl IS INITIAL.
          MESSAGE s110 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.

        IF <fs_094>-valor_usd IS INITIAL.
          MESSAGE s111 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.
*      ELSE.
*        CLEAR: <fs_094>-safra.
      ENDIF.

      IF wa_092-tp_arrendamento EQ '03'.

        IF <fs_094>-dt_vencimento IS INITIAL.
          MESSAGE s093 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.
        "110  Deve ser informado o valor BRL!
        "111  Deve ser informado o valor USD!

        IF <fs_094>-valor_brl IS INITIAL.
          MESSAGE s110 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.

        IF <fs_094>-valor_usd IS INITIAL.
          MESSAGE s111 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.

      ENDIF.

      IF wa_092-tp_arrendamento EQ '04' OR wa_092-tp_arrendamento EQ '05'.

*108  Deve ser informado o Período De!
        IF <fs_094>-periodo_de IS INITIAL.
          MESSAGE s108 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.

*109  Deve ser informado o Período Até!
        IF <fs_094>-periodo_ate IS INITIAL.
          MESSAGE s109 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.

        IF <fs_094>-periodo_ate LT <fs_094>-periodo_de.
          MESSAGE s092 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.

        IF <fs_094>-dt_vencimento IS INITIAL.
          MESSAGE s093 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.

        IF <fs_094>-valor_brl IS INITIAL.
          MESSAGE s110 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.

        IF <fs_094>-valor_usd IS INITIAL.
          MESSAGE s111 DISPLAY LIKE 'E'.
          ck_094 = abap_true.
          EXIT.
        ENDIF.

      ENDIF.

    ENDLOOP.

    me->set_modelagem( ).

    CHECK ck_094 = abap_false.

    IF r_main->at_antigo EQ gl_acao-novo.
      SELECT SINGLE * INTO @DATA(wa_zglt092)
        FROM zglt092
       WHERE cod_contrato EQ @wa_092-cod_contrato.

      IF sy-subrc IS INITIAL.
        MESSAGE s105 DISPLAY LIKE 'E' WITH wa_zglt092-cod_contrato.
        EXIT.
      ENDIF.
    ENDIF.

    ck_validado = abap_true.

  ENDMETHOD.

  METHOD set_grava.

    FREE: it_096_save, it_094_save.
    CLEAR: save_092.

*    CASE wa_092-tp_arrendamento.
*      WHEN '01' OR '02'.
*        CLEAR: wa_092-tx_pis, wa_092-tx_cofins.
*    ENDCASE.

    IF vg_radiof IS NOT INITIAL.
      wa_092-identcf = 'F'.
    ELSE.
      wa_092-identcf = 'C'.
    ENDIF.

    MOVE-CORRESPONDING wa_092 TO save_092.

    MODIFY zglt092 FROM save_092.

    LOOP AT it_096 INTO DATA(w_096).
      w_096-cod_contrato = wa_092-cod_contrato.
      APPEND CORRESPONDING #( w_096 ) TO it_096_save.
    ENDLOOP.


    DELETE FROM zglt096 WHERE cod_contrato EQ wa_092-cod_contrato.
    MODIFY zglt096 FROM TABLE it_096_save.

* RJF - Ini
*    LOOP AT it_094 ASSIGNING FIELD-SYMBOL(<fs_0094>).
*      <fs_0094>-cod_contrato = wa_092-cod_contrato.
*      IF wa_092-tp_arrendamento EQ '03'.
*        CLEAR: <fs_0094>-safra,
*               <fs_0094>-periodo_ate,
*               <fs_0094>-periodo_de.
*      ENDIF.
*    ENDLOOP.
* RJF - Fim

    LOOP AT it_094 INTO DATA(w_094).
      w_094-cod_contrato = wa_092-cod_contrato.
      APPEND CORRESPONDING #( w_094 ) TO it_094_save.
    ENDLOOP.

    "it_094_save = it_094.

    DELETE FROM zglt094 WHERE cod_contrato EQ wa_092-cod_contrato.
    MODIFY zglt094 FROM TABLE it_094_save.

    COMMIT WORK AND WAIT.

    MESSAGE 'Registro gravado com sucesso!' TYPE 'S'.

  ENDMETHOD.

  METHOD get_help.

    DATA: t_dynpselect   TYPE TABLE OF dselc,
          t_dynpvaluetab TYPE TABLE OF dval,
          w_help_info    TYPE help_info,
          w_selected     TYPE dynfieldvalue.

    DATA: gt_return_tab TYPE TABLE OF ddshretval,
          gt_dselc      TYPE TABLE OF dselc.


    IF input EQ 'M'.
      w_help_info =
      VALUE #(
                call       = 'V'
                object     = 'F'
                program   = sy-repid
                dynpro     = sy-dynnr
                tabname   = 'ZGLT092'
                fieldname = 'MUNICIPIO'
                spras     = sy-langu
                menufunct = 'HC'
             ).

      CALL FUNCTION 'HELP_START'
        EXPORTING
          help_infos   = w_help_info
        IMPORTING
          select_value = w_selected
        TABLES
          dynpselect   = t_dynpselect
          dynpvaluetab = t_dynpvaluetab.

      IF w_selected IS NOT INITIAL.
        wa_092-municipio = w_selected.
      ENDIF.

    ELSE.

      SELECT tp_arrendamento, bukrs, filial, cod_contrato
        FROM zglt092
        INTO TABLE @DATA(f4_092)
        WHERE (at_where).

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'ID'
          dynpprog        = sy-repid
          dynpnr          = sy-dynnr
          value_org       = 'S'
        TABLES
          value_tab       = f4_092
          return_tab      = gt_return_tab
          dynpfld_mapping = gt_dselc
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.

      IF sy-subrc IS INITIAL AND gt_return_tab[] IS NOT INITIAL.
        wa_092-cod_contrato     = gt_return_tab[ 1 ]-fieldval.
      ENDIF.

    ENDIF.

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = '/00'
      EXCEPTIONS
        function_not_supported = 1
        OTHERS                 = 2.
  ENDMETHOD.


  METHOD perform_semantic_checks.

    "IT_096
    LOOP AT pr_data_changed->mt_good_cells INTO DATA(wa_good_cells).
      CASE wa_good_cells-fieldname.
        WHEN 'VIGENCIA_DE'.

          READ TABLE pr_data_changed->mt_good_cells INTO DATA(wa_good_cells_ate)
           WITH KEY row_id = wa_good_cells-row_id
                    fieldname = 'VIGENCIA_ATE'.
          IF sy-subrc IS NOT INITIAL.
            READ TABLE it_096 INTO DATA(wa_096) INDEX wa_good_cells-row_id.
          ELSE.
            wa_096-vigencia_ate = wa_good_cells_ate-value.
          ENDIF.

          wa_096-vigencia_de = wa_good_cells-value.
          IF wa_096-vigencia_de GT wa_096-vigencia_ate AND wa_096-vigencia_ate IS NOT INITIAL.
            CALL METHOD pr_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = 'ZFI'
                i_msgno     = '089'
                i_msgty     = 'E'
                i_fieldname = wa_good_cells-fieldname
                i_row_id    = wa_good_cells-row_id.
            error_in_data = abap_true.
          ENDIF.
        WHEN 'VIGENCIA_ATE'.

          READ TABLE pr_data_changed->mt_good_cells INTO DATA(wa_good_cells_de)
           WITH KEY row_id = wa_good_cells-row_id
                    fieldname = 'VIGENCIA_DE'.
          IF sy-subrc IS NOT INITIAL.
            READ TABLE it_096 INTO wa_096 INDEX wa_good_cells-row_id.
          ELSE.
            wa_096-vigencia_de = wa_good_cells_de-value.
          ENDIF.

          wa_096-vigencia_ate = wa_good_cells-value.
          IF wa_096-vigencia_de GT wa_096-vigencia_ate AND wa_096-vigencia_ate IS NOT INITIAL.
            CALL METHOD pr_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = 'ZFI'
                i_msgno     = '089'
                i_msgty     = 'E'
                i_fieldname = wa_good_cells-fieldname
                i_row_id    = wa_good_cells-row_id.
            error_in_data = abap_true.
          ENDIF.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD perform_semantic_checks2.

    "IT_094
    LOOP AT pr_data_changed->mt_good_cells INTO DATA(wa_good_cells).
      CASE wa_good_cells-fieldname.
        WHEN 'PERIODO_DE'.

          READ TABLE pr_data_changed->mt_good_cells INTO DATA(wa_good_cells_ate)
           WITH KEY row_id = wa_good_cells-row_id
                    fieldname = 'PERIODO_ATE'.
          IF sy-subrc IS NOT INITIAL.
            READ TABLE it_094 INTO DATA(wa_094) INDEX wa_good_cells-row_id.
          ELSE.
            wa_094-periodo_ate = wa_good_cells_ate-value.
          ENDIF.

          wa_094-periodo_de = wa_good_cells-value.
          IF wa_094-periodo_de GT wa_094-periodo_ate AND wa_094-periodo_ate IS NOT INITIAL.
            CALL METHOD pr_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = 'ZFI'
                i_msgno     = '092'
                i_msgty     = 'E'
                i_fieldname = wa_good_cells-fieldname
                i_row_id    = wa_good_cells-row_id.
            error_in_data = abap_true.
          ENDIF.

        WHEN 'PERIODO_ATE'.

          READ TABLE pr_data_changed->mt_good_cells INTO DATA(wa_good_cells_de)
           WITH KEY row_id = wa_good_cells-row_id
                    fieldname = 'PERIODO_DE'.
          IF sy-subrc IS NOT INITIAL.
            READ TABLE it_094 INTO wa_094 INDEX wa_good_cells-row_id.
          ELSE.
            wa_094-periodo_de = wa_good_cells_de-value.
          ENDIF.

          wa_094-periodo_ate = wa_good_cells-value.
          IF wa_094-periodo_de GT wa_094-periodo_ate AND wa_094-periodo_ate IS NOT INITIAL.
            CALL METHOD pr_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = 'ZFI'
                i_msgno     = '092'
                i_msgty     = 'E'
                i_fieldname = wa_good_cells-fieldname
                i_row_id    = wa_good_cells-row_id.
            error_in_data = abap_true.
          ENDIF.
        WHEN 'SAFRA'.
*          wa_094-safra = wa_good_cells-value.
*          IF wa_094-safra IS NOT INITIAL.
*            IF zcl_str=>set( i_texto = CONV #( wa_094-safra ) )->matcher( EXPORTING pattern = '[0-9]{4}|[0-9]{4}/[0-9]{4}' ) EQ abap_false.
*              CALL METHOD pr_data_changed->add_protocol_entry
*                EXPORTING
*                  i_msgid     = 'ZFI'
*                  i_msgno     = '106'
*                  i_msgty     = 'E'
*                  i_fieldname = wa_good_cells-fieldname
*                  i_row_id    = wa_good_cells-row_id.
*              error_in_data = abap_true.
*            ENDIF.
*          ENDIF.

        WHEN 'DT_PAGAMNETO'. "RJF

          READ TABLE pr_data_changed->mt_good_cells INTO DATA(wa_good_cells_pag)
           WITH KEY row_id = wa_good_cells-row_id
                    fieldname = 'DT_PAGAMNETO'.
          IF sy-subrc IS NOT INITIAL.
            READ TABLE it_094 INTO DATA(wa_094pg) INDEX wa_good_cells-row_id.

            IF sy-subrc IS INITIAL AND wa_094pg-dt_pagamneto IS NOT INITIAL.

              IF wa_good_cells-value NE wa_094pg-dt_pagamneto. "RJF
                DATA: lv_dt(7) TYPE c.

                CONCATENATE wa_094pg-dt_pagamneto+4(2) '/' wa_094pg-dt_pagamneto(4) INTO lv_dt.

                SELECT doc_lcto, doc_atua_preco, est_atua_preco
                  UP TO 1 ROWS
                  INTO @DATA(wa_lcto)
                  FROM zglt096e
                  WHERE cod_contrato EQ @wa_092-cod_contrato
                    AND competencia  EQ @lv_dt.
                ENDSELECT.
                IF ( wa_lcto-doc_lcto IS NOT INITIAL AND wa_lcto-doc_atua_preco IS NOT INITIAL )
                OR ( wa_lcto-doc_lcto IS NOT INITIAL AND wa_lcto-doc_atua_preco IS INITIAL AND wa_lcto-est_atua_preco IS INITIAL ).

                  CALL METHOD pr_data_changed->add_protocol_entry
                    EXPORTING
                      i_msgid     = 'ZFI'
                      i_msgno     = '129'
                      i_msgty     = 'E'
                      i_fieldname = wa_good_cells-fieldname
                      i_row_id    = wa_good_cells-row_id.
                  error_in_data = abap_true.

                ENDIF.
              ENDIF.
            ENDIF.
            wa_094-dt_pagamneto = wa_good_cells-value.
          ENDIF.

      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.
  cl_main=>run( ).

*&---------------------------------------------------------------------*
*&      Module  MAIN_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE main_pbo OUTPUT.

  IF r_main IS INITIAL.
    CREATE OBJECT r_main.
  ENDIF.

  r_main->process_before_output( ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  MAIN_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE main_pai INPUT.

  CASE sy-ucomm.
    WHEN gl_acao-voltar.
      LEAVE TO SCREEN 0.

    WHEN gl_acao-sair.
      LEAVE TO CURRENT TRANSACTION.

    WHEN gl_acao-cancelar.
      r_main->at_antigo = gl_acao-exibir.
      r_main->set_cancel( ).

    WHEN gl_acao-novo OR gl_acao-modificar OR gl_acao-exibir.

      r_main->at_antigo = sy-ucomm.
      r_main->set_acao( sy-ucomm(1) ). "// N: "New" E: "Edit" L: "Look"

      IF sy-ucomm(1) NE 'E'.
        r_main->set_cancel( ).
      ENDIF.

    WHEN gl_acao-salvar.
      IF r_main->set_validar( ) EQ abap_true.
        r_main->set_grava( ).
        r_main->at_antigo = gl_acao-modificar.
        r_main->set_acao( 'E' ). "// N: "New" E: "Edit" L: "Look"
      ENDIF.

    WHEN gl_acao-contabilidade.

      PERFORM z_search_0095.
      CALL SCREEN 0103.
      CLEAR sy-ucomm.

    WHEN gl_acao-matricula.

    WHEN gl_acao-cli_for.
      CALL TRANSACTION 'ZGL077'.

    WHEN gl_acao-arr_pre.
*      CALL TRANSACTION 'ZGL077'.
      CASE wa_092-tp_arrendamento.
        WHEN '02'.
          p_cont = wa_092-cod_contrato.
          CALL SELECTION-SCREEN 1001 STARTING AT 20 05 ENDING AT 150 15.

          ln_mes = p_mes.
          p_mes = ln_mes.

          SUBMIT zglr073  WITH p_cont   = p_cont
                          WITH p_mes    = p_mes
                          WITH p_ano    = p_ano
                          WITH p_tp_arr = wa_092-tp_arrendamento AND RETURN.
      ENDCASE.
* Tela - RJF

    WHEN gl_acao-fluxo.

      CASE wa_092-tp_arrendamento.
        WHEN '01' OR '02'.
          p_cont = wa_092-cod_contrato.
          CALL SELECTION-SCREEN 1001 STARTING AT 20 05 ENDING AT 150 15.

          ln_mes = p_mes.
          p_mes = ln_mes.

          SUBMIT zglr071  WITH p_cont   = p_cont
                          WITH p_mes    = p_mes
                          WITH p_ano    = p_ano
                          WITH p_tp_arr = wa_092-tp_arrendamento AND RETURN.
        WHEN '03'.
          DATA:
            lv_comp      TYPE sy-datum,
            v_ultimo_dia TYPE sy-datum.

          p_cont = wa_092-cod_contrato.

*          p_cont2 = wa_092-cod_contrato.

*          CALL SELECTION-SCREEN 1003 STARTING AT 20 05 ENDING AT 150 15.
          CALL SELECTION-SCREEN 1001 STARTING AT 20 05 ENDING AT 150 15.

          CONCATENATE p_ano p_mes '01' INTO lv_comp.

          CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
            EXPORTING
              day_in            = lv_comp
            IMPORTING
              last_day_of_month = v_ultimo_dia.

          IF sy-subrc IS INITIAL AND v_ultimo_dia IS NOT INITIAL.
            p_dtvenc = v_ultimo_dia.
          ENDIF.

          SUBMIT zglr071  WITH p_cont    = p_cont
                          WITH p_dtvenc  = p_dtvenc
                          WITH p_tp_arr  = wa_092-tp_arrendamento AND RETURN.

        WHEN '04'.
          p_cont3 = wa_092-cod_contrato.

          CALL SELECTION-SCREEN 1004 STARTING AT 20 05 ENDING AT 150 15.

          SUBMIT zglr071  WITH p_cont   = p_cont3
                          WITH p_ano    = p_ano3
                          WITH p_tp_arr = wa_092-tp_arrendamento    AND RETURN.

        WHEN '05'.
          p_cont = wa_092-cod_contrato.
          CALL SELECTION-SCREEN 1001 STARTING AT 20 05 ENDING AT 150 15.
          CONCATENATE p_ano p_mes '01' INTO lv_comp.

          CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
            EXPORTING
              day_in            = lv_comp
            IMPORTING
              last_day_of_month = v_ultimo_dia.

          IF sy-subrc IS INITIAL AND v_ultimo_dia IS NOT INITIAL.
            p_dtvenc = v_ultimo_dia.
          ENDIF.

          SUBMIT zglr071  WITH p_cont    = p_cont
                          WITH p_dtvenc  = p_dtvenc
                          WITH p_tp_arr  = wa_092-tp_arrendamento AND RETURN.

      ENDCASE.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TAB_SET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tab_set OUTPUT.

  tab_strip-activetab = g_tab_strip-pressed_tab.

  g_tab_strip-subscreen =
  SWITCH #(
  g_tab_strip-pressed_tab
                          WHEN c_tab_strip-tab1 THEN '0101'
                          WHEN c_tab_strip-tab2 THEN '0102'
                          ELSE g_tab_strip-pressed_tab  ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TAB_GET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tab_get INPUT.

  IF sy-ucomm EQ 'EDIT'.
    vg_mode = 'M'.
  ELSEIF sy-ucomm EQ 'NEW'.
    vg_mode = 'N'.
  ELSEIF sy-ucomm EQ 'LOOK'.
    vg_mode = 'E'.
  ENDIF.

  g_tab_strip-pressed_tab =
  SWITCH #( sy-ucomm WHEN c_tab_strip-tab1 THEN c_tab_strip-tab1
                     WHEN c_tab_strip-tab2 THEN c_tab_strip-tab2
                     ELSE g_tab_strip-pressed_tab   ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen OUTPUT.
  r_main->set_descricao( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  HELP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE helpm INPUT.
  r_main->get_help( 'M' ).
ENDMODULE.
MODULE helpc INPUT.
  r_main->get_help( 'C' ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_CONTRATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_busca_contratos .

  DATA: BEGIN OF tl_0092 OCCURS 0,
          cod_contrato TYPE zglt092-cod_contrato,
          bukrs        TYPE zglt092-bukrs,
        END OF tl_0092.

  SELECT
        cod_contrato
        bukrs
    FROM zglt092 INTO TABLE tl_0092
   WHERE tp_arrendamento EQ wa_092-tp_arrendamento
    AND  bukrs           EQ wa_092-bukrs
    AND  filial          EQ wa_092-filial.

  IF sy-subrc EQ 0.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'COD_CONTRATO'
        dynpprog    = sy-repid
        dynpnr      = sy-dynnr
        dynprofield = 'ZGLT092-CON_CONTRATO'
        value_org   = 'S'
      TABLES
        value_tab   = tl_0092.

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  MODULE_PAI  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE module_pai OUTPUT.
  DATA: lt_f4 TYPE  lvc_t_f4 WITH HEADER LINE.
  DATA: it_tcode TYPE TABLE OF sy-ucomm.

  IF dg_splitter IS INITIAL.

    CLEAR: it_tcode[].

    CALL FUNCTION 'ZENQUEUE_CPROG'
      EXPORTING
        cprog          = sy-cprog
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc IS NOT INITIAL.
      ck_bloqueado = abap_false.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'W'.
      APPEND 'SAVE' TO it_tcode.
      APPEND 'INS' TO it_tcode.
      APPEND 'DEL' TO it_tcode.
    ELSE.
      ck_bloqueado = abap_true.
    ENDIF.

    PERFORM z_fieldcat.

    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = cl_gui_container=>screen0
        rows    = 1
        columns = 1.

    ctl_cccontainer = dg_splitter->get_container( row = 1 column = 1 ).

    CREATE OBJECT grid
      EXPORTING
        i_parent = ctl_cccontainer.

    wl_function  = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    wa_layout-stylefname = 'CELLTAB'.

    CALL METHOD grid->set_table_for_first_display
      EXPORTING
        "I_SAVE               = 'X'
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_outtab            = it_095
        it_fieldcatalog      = it_fieldcat.


    CALL METHOD grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.


    CALL METHOD grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*    REFRESH LT_F4.
*
*    LT_F4-FIELDNAME  = 'CHAVE_LCTO'.
*    LT_F4-REGISTER   = 'X'.
*    LT_F4-GETBEFORE  = 'X'.
*    LT_F4-CHNGEAFTER = 'X'.
*    INSERT LT_F4 INTO TABLE LT_F4[].

*    LT_F4-FIELDNAME  = 'CONTA'.
*    LT_F4-REGISTER   = 'X'.
*    LT_F4-GETBEFORE  = 'X'.
*    LT_F4-CHNGEAFTER = 'X'.
*    INSERT LT_F4 INTO TABLE LT_F4[].

*    LT_F4-FIELDNAME  = 'TIPO_MOV_I'.
*    LT_F4-REGISTER   = 'X'.
*    LT_F4-GETBEFORE  = 'X'.
*    LT_F4-CHNGEAFTER = 'X'.
*    INSERT LT_F4 INTO TABLE LT_F4[].
*
*    LT_F4-FIELDNAME  = 'RAZAO_ESPECIAL'.
*    LT_F4-REGISTER   = 'X'.
*    LT_F4-GETBEFORE  = 'X'.
*    LT_F4-CHNGEAFTER = 'X'.
*    INSERT LT_F4 INTO TABLE LT_F4[].
*
*    CALL METHOD GRID->REGISTER_F4_FOR_FIELDS
*      EXPORTING
*        IT_F4 = LT_F4[].

*    SET HANDLER LCL_EVENT_F4=>ON_F4 FOR GRID.

    CREATE OBJECT eventos.
    SET HANDLER: eventos->on_data_changed FOR grid,
                 eventos->on_data_changed_finished FOR grid.

  ELSE.

    CALL METHOD grid->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fieldcat.

    CALL METHOD grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

  SET PF-STATUS 'ST_0103' EXCLUDING it_tcode.
  SET TITLEBAR 'TL_0103'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0103 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'INS'.
      APPEND INITIAL LINE TO it_095.
      ck_alterou = abap_true.
    WHEN 'DEL'.

      CALL METHOD grid->get_selected_rows
        IMPORTING
          et_index_rows = DATA(it_selectd).

      IF it_selectd[] IS INITIAL.
        MESSAGE 'Favor selecione uma linha ! ' TYPE 'I'.
        EXIT.
      ELSE.
        ck_alterou = abap_true.
        LOOP AT it_selectd INTO DATA(wa_selectd).
          READ TABLE it_095 INTO wa_095 INDEX wa_selectd-index.
          DELETE it_095 INDEX wa_selectd-index.
          IF wa_095-id_parametro IS NOT INITIAL.
            APPEND CORRESPONDING #( wa_095 ) TO it_095_del.
          ENDIF.
        ENDLOOP.
      ENDIF.

    WHEN 'SAVE'.

      DATA: ck_validado TYPE char01.
      CLEAR: ck_validado.

      PERFORM validar_antes_salvar CHANGING ck_validado.

      IF ck_validado EQ abap_true.
        FREE: it_095_save.

        IF it_095_del IS NOT INITIAL.
          DELETE zglt095 FROM TABLE it_095_del.
          COMMIT WORK.
          REFRESH it_095_del.
        ENDIF.

        LOOP AT it_095 INTO DATA(w_095).
          APPEND CORRESPONDING #( w_095 ) TO it_095_save.
        ENDLOOP.

        LOOP AT it_095_save ASSIGNING FIELD-SYMBOL(<fs_95>) WHERE id_parametro IS INITIAL.
          PERFORM gerar_id_unico CHANGING <fs_95>-id_parametro.
        ENDLOOP.

        MODIFY zglt095 FROM TABLE it_095_save.
        COMMIT WORK.

        CLEAR: ck_alterou.
        MESSAGE 'Dados gravado com sucesso!' TYPE 'S'.
        PERFORM z_search_0095.

      ENDIF.

  ENDCASE.

  CALL METHOD grid->refresh_table_display( is_stable = wa_stable ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  Z_SEARCH_0095
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_search_0095.

  REFRESH it_095.

  PERFORM z_busca_desc_dominio.

  DATA it_zglt095 TYPE TABLE OF zglt095.

  SELECT  *
    FROM zglt095 INTO CORRESPONDING FIELDS OF TABLE it_zglt095.

  LOOP AT it_zglt095 INTO DATA(wa_t095).

    CLEAR: wa_095, _domvalue_l.

    _domvalue_l = wa_t095-classificacao.
    READ TABLE it_class INTO DATA(_class) WITH KEY domvalue_l = _domvalue_l.
    IF sy-subrc EQ 0 .
      wa_095-desc_classificacao = _class-ddtext.
    ENDIF.
    CLEAR _domvalue_l.

    _domvalue_l = wa_t095-tp_taxa.
    READ TABLE it_taxa INTO DATA(_taxa) WITH KEY domvalue_l = _domvalue_l.
    IF sy-subrc EQ 0.
      wa_095-desc_taxa = _taxa-ddtext.
    ENDIF.
    CLEAR _domvalue_l.

    _domvalue_l = wa_t095-tp_arrendamento.
    READ TABLE it_tp_arrend INTO DATA(_tp_arrend) WITH KEY domvalue_l = _domvalue_l.
    IF sy-subrc EQ 0.
      wa_095-desc_tp_arrend = _tp_arrend-ddtext.
    ENDIF.
    CLEAR _domvalue_l.

    _domvalue_l = wa_t095-tp_lancamento.
    READ TABLE it_tp_lanc INTO DATA(_tp_lanc) WITH KEY domvalue_l = _domvalue_l.
    IF sy-subrc EQ 0.
      wa_095-desc_tp_lanc = _tp_lanc-ddtext.
    ENDIF.
    CLEAR _domvalue_l.

    wa_095-id_parametro         =  wa_t095-id_parametro.
    wa_095-tp_arrendamento      =  wa_t095-tp_arrendamento.
    wa_095-classificacao        =  wa_t095-classificacao.
    wa_095-tp_lancamento        =  wa_t095-tp_lancamento.
    wa_095-tp_taxa              =  wa_t095-tp_taxa.
    wa_095-chave_lcto           =  wa_t095-chave_lcto.
    wa_095-conta                =  wa_t095-conta.
    wa_095-razao_especial       =  wa_t095-razao_especial.
*Inicio Alteração - Leandro Ferreira Valentim - 21.09.23 - #122471
***    wa_095-tipo_mov_i           =  wa_t095-tipo_mov_i.
***    wa_095-kostl                =  wa_t095-kostl. " RJF
***    wa_095-bewar                =  wa_t095-bewar. " RJF
*Fim Alteração - Leandro Ferreira Valentim - 21.09.23 - #122471
    wa_095-matnr                =  wa_t095-matnr. " RJF
    wa_095-matkl                =  wa_t095-matkl. " RJF
    wa_095-tx_pis               =  wa_t095-tx_pis.
    wa_095-tx_cofins            =  wa_t095-tx_cofins.
    wa_095-texto_historico      =  wa_t095-texto_historico.

    IF wa_t095-conta IS NOT INITIAL. " RJF
      SELECT *
        UP TO 1 ROWS
        FROM skat INTO @DATA(wa_skatxx)
        WHERE spras EQ @sy-langu
*                AND   ktopl  EQ @wa_ska1-ktopl
        AND   saknr  EQ @wa_t095-conta.
      ENDSELECT.

      IF wa_skatxx-txt50 IS NOT INITIAL. " *RJF
        wa_095-descricao_conta      =  wa_skatxx-txt50.
      ENDIF.
    ENDIF.
*    wa_095-descricao_conta      =  wa_t095-descricao_conta. " RJF

    FREE wa_095-celltab.
    it_estilo =  VALUE #( ( fieldname = 'TP_ARRENDAMENTO'     style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'CLASSIFICACAO'       style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'TP_LANCAMENTO'       style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'TP_TAXA'             style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'CHAVE_LCTO'          style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'CONTA'               style = cl_gui_alv_grid=>mc_style_disabled )
*Inicio Alteração - Leandro Ferreira Valentim - 21.09.23 - #122471
***                          ( fieldname = 'KOSTL'               style = cl_gui_alv_grid=>mc_style_disabled )
***                          ( fieldname = 'TIPO_MOV_I'          style = cl_gui_alv_grid=>mc_style_disabled )
***                          ( fieldname = 'BEWAR'               style = cl_gui_alv_grid=>mc_style_disabled )
*Fim Alteração - Leandro Ferreira Valentim - 21.09.23 - #122471
                          ( fieldname = 'RAZAO_ESPECIAL'      style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'MATNR'               style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'MATKL'               style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'TEXTO_HISTORICO'     style = cl_gui_alv_grid=>mc_style_disabled ) ).
    INSERT LINES OF it_estilo INTO TABLE wa_095-celltab.

    APPEND wa_095 TO it_095.


  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_fieldcat .

  REFRESH it_fieldcat.
  PERFORM preenche_cat USING:
    'TP_ARRENDAMENTO'     'Tipo Contrato'   '13'  'X'  ''   'ZGLT092'  'TP_ARRENDAMENTO' ''  ''  ''  '' '' '' '',
    'DESC_TP_ARREND'      'Descrição'       '20'  ''   ''   ''         ''              ''  ''  ''  '' '' '' '',
    'CLASSIFICACAO'       'Classificação'   '13'  'X'  ''   'ZGLT095'  'CLASSIFICACAO' ''  ''  ''  '' '' '' '',
    'DESC_CLASSIFICACAO'  'Descrição'       '20'  ' '  ''   ''         ''              ''  ''  ''  '' '' '' '',
*    'TP_LANCAMENTO'       'Tipo Lançamento' '15'  'X'  ''   'ZGLT095'  'TP_LANCAMENTO' ''  ''  ''  '' '' '' '',
*    'DESC_TP_LANC'        'Descrição'       '20'  ''   ''   ''         ''              ''  ''  ''  '' '' '' '',
    'TP_TAXA'             'Tipo Taxa'       '09'  'X'  ''   'ZGLT095'  'TP_TAXA'       ''  ''  ''  '' '' '' '',
    'DESC_TAXA'           'Descrição'       '20'  ''   ''   ''         ''              ''  ''  ''  '' '' '' '',
    'CHAVE_LCTO'          'Chave'           '05'  'X'  'X'  'ZGLT095'  'CHAVE_LCTO'    ''  ''  ''  '' '' '' '',
    'CONTA'               'Conta'           '10'  'X'  'X'  'ZGLT095'  'CONTA'         ''  ''  ''  '' '' '' '',
    'DESCRICAO_CONTA'     'Desc.Conta'      '50'  ''   ''   'SKAT'     'TXT50'         ''  ''  ''  '' '' '' '',
*Inicio Alteração - Leandro Valentim Ferreira - 21.09.23 - #122471
***    'KOSTL'               'Centro Custo'    '14'  'X'  'X'  'CSKS'    'KOSTL'          ''  ''  ''  '' '' '' '',
***    'TIPO_MOV_I'          'Tipo Movimento'  '14'  'X'  'X'  'ZGLT095'  ''              ''  ''  ''  '' '' '' '',
***    'BEWAR'               'Tp Mov. Cons.'   '14'  'X'  'X'  'T856T'  'TRTYP'         ''  ''  ''  '' '' '' '',
*Fim Alteração - Leandro Valentim Ferreira - 21.09.23 - #122471
    'RAZAO_ESPECIAL'      'Razão Especial'  '15'  'X'  'X'  'ZGLT095'  ''              ''  ''  ''  '' '' '' '',
    'MATNR'               'Material'        '15'  'X'  'X'  'ZGLT095'     ''              ''  ''  ''  '' '' '' '',
    'MATKL'               'Grp. Material'   '15'  'X'  'X'  'ZGLT095'     ''              ''  ''  ''  '' '' '' '',
    "'TX_PIS'              'Taxa PIS'        '12'  'X'  ''   'ZGLT095'  ''              'DEC'  'P'  '23'  'ZDM_TOTAL_SACAS_14_9' '9' '4' '',
    "'TX_COFINS'           'Taxa COFINS'     '12'  'X'  ''   'ZGLT095'  ''              'DEC'  'P'  '23'  'ZDM_TOTAL_SACAS_14_9' '9' '4' '',
    'TEXTO_HISTORICO'     'Texto'           '50'  'X'  ''   'ZGLT095'  ''              ''     ''  ''  '' '' '' 'X'.

ENDFORM.

FORM preenche_cat USING VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_edit)
                        VALUE(p_f4)
                        VALUE(p_table)
                        VALUE(p_fieldname)
                        VALUE(p_datatype)
                        VALUE(p_inttype)
                        VALUE(p_intlen)
                        VALUE(p_domname)
                        VALUE(p_decimals)
                        VALUE(p_decimals_o)
                        VALUE(p_lowercase).

  wa_fieldcat-fieldname   = p_campo.
  wa_fieldcat-coltext     = p_desc.
  wa_fieldcat-scrtext_l   = p_desc.
  wa_fieldcat-scrtext_m   = p_desc.
  wa_fieldcat-scrtext_s   = p_desc.
  wa_fieldcat-outputlen   = p_tam.
  wa_fieldcat-edit        = p_edit.
  wa_fieldcat-ref_table   = p_table.
  wa_fieldcat-ref_field   = p_fieldname.
  wa_fieldcat-f4availabl  = p_f4.
  wa_fieldcat-datatype    = p_datatype.
  wa_fieldcat-inttype     = p_inttype.
  wa_fieldcat-intlen      = p_intlen.
  wa_fieldcat-domname     = p_domname.
  wa_fieldcat-decimals_o  = p_decimals_o.
  wa_fieldcat-decimals    = p_decimals.
  wa_fieldcat-lowercase   = p_lowercase.

  APPEND wa_fieldcat TO it_fieldcat.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_DESC_DOMINIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_busca_desc_dominio .

  "dominio campo Classificacao
  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = 'ZGLE0002'
      state         = 'A'
      langu         = sy-langu
    TABLES
      dd07v_tab     = it_class
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.


  "dominio campo Classificacao
  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = 'ZGLE0003'
      state         = 'A'
      langu         = sy-langu
    TABLES
      dd07v_tab     = it_taxa
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.

  "dominio campo Tipo Arrendamento
  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = 'ZGLD0001'
      state         = 'A'
      langu         = sy-langu
    TABLES
      dd07v_tab     = it_tp_arrend
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.

  "dominio campo Tipo Arrendamento
  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = 'ZGLD0003'
      state         = 'A'
      langu         = sy-langu
    TABLES
      dd07v_tab     = it_tp_lanc
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  VAL_EMPRESA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE val_empresa INPUT.

  CLEAR: wa_092-bukrs_d.

  CHECK wa_092-bukrs IS NOT INITIAL.

  SELECT SINGLE * INTO @DATA(wa_t001)
    FROM t001
   WHERE bukrs EQ @wa_092-bukrs.

  SELECT *
    UP TO 1 ROWS
    FROM j_1bbranch
    WHERE bukrs = wa_092-bukrs.
  ENDSELECT.

  wa_092-bukrs = wa_t001-bukrs.
  wa_092-bukrs_d = wa_t001-butxt.

  CHECK sy-subrc IS NOT INITIAL.

  MESSAGE e075 WITH wa_092-bukrs.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VAL_FILIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE val_filial INPUT.

  CLEAR: wa_092-filial_d,
         wa_092-localizacao.
  "wa_092-municipio. 121595 Arrendamento - Erro Variação USD A receber e campo município - PSA

  CHECK wa_092-filial IS NOT INITIAL.

  IF wa_092-bukrs IS NOT INITIAL.
    SELECT SINGLE * INTO @DATA(wa_j_1bbranch)
      FROM j_1bbranch
     WHERE bukrs  EQ @wa_092-bukrs
       AND branch EQ @wa_092-filial.

    IF sy-subrc IS INITIAL.

      SELECT SINGLE name1
        FROM t001w
        INTO wa_092-filial_d
        WHERE werks EQ wa_092-filial.

      wa_092-localizacao = wa_j_1bbranch-name.

    ENDIF.
  ENDIF.
*. 121595 Arrendamento - Erro Variação USD A receber e campo município - PSA
*      SELECT SINGLE * INTO @DATA(wa_adrc)
*        FROM adrc
*       WHERE addrnumber EQ @wa_j_1bbranch-adrnr.
*
*      IF sy-subrc IS INITIAL.
*        wa_092-municipio = |{ wa_adrc-city1 }|.
*      ENDIF.
*    ELSE.
*      MESSAGE e076 WITH wa_092-bukrs wa_092-filial.
*    ENDIF.
*
*
*  ELSE.
*    SELECT SINGLE * INTO @wa_j_1bbranch
*      FROM j_1bbranch
*     WHERE branch EQ @wa_092-filial.
*
*    IF sy-subrc IS INITIAL.
*
*      SELECT SINGLE name1
*        FROM t001w
*        INTO wa_092-filial_d
*        WHERE werks EQ wa_092-filial.
*
*      wa_092-localizacao = wa_j_1bbranch-name.
*
*      SELECT SINGLE * INTO @wa_adrc
*        FROM adrc
*       WHERE addrnumber EQ @wa_j_1bbranch-adrnr.
*
*      IF sy-subrc IS INITIAL.
*        wa_092-municipio = |{ wa_adrc-city1 }|.
*      ENDIF.
*    ELSE.
*      MESSAGE e077 WITH wa_092-filial.
*    ENDIF.
*


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VAL_FORNE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE val_forne INPUT.

  CLEAR wa_092-fornecedor_d.

  CHECK wa_092-fornecedor IS NOT INITIAL.

  SELECT SINGLE * INTO @DATA(wa_lfa1)
    FROM lfa1
   WHERE lifnr EQ @wa_092-fornecedor.

  wa_092-fornecedor_d = wa_lfa1-name1.

  CHECK sy-subrc IS NOT INITIAL.

  MESSAGE e078 WITH wa_092-fornecedor.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VAL_CLIENTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE val_cliente INPUT.

  CLEAR: wa_092-cliente_d.

  CHECK wa_092-cliente IS NOT INITIAL.

  SELECT SINGLE * INTO @DATA(wa_kna1)
    FROM kna1
   WHERE kunnr EQ @wa_092-cliente.

  wa_092-cliente_d = wa_kna1-name1.

  CHECK sy-subrc IS NOT INITIAL.

  MESSAGE e079 WITH wa_092-cliente.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VAL_MOEDA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE val_moeda INPUT.

  CHECK wa_092-moeda IS NOT INITIAL.

  SELECT SINGLE * INTO @DATA(wa_tcurc)
    FROM tcurc
   WHERE waers EQ @wa_092-moeda.

  CHECK sy-subrc IS NOT INITIAL.

  MESSAGE e080 WITH wa_092-moeda.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  GERAR_ID_UNICO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<FS_95>_ID_PARAMETRO  text
*----------------------------------------------------------------------*
FORM gerar_id_unico  CHANGING p_id_parametro TYPE zde_id_param_cte_arre.

  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
    EXPORTING
      object           = 'ZGL071P'
    EXCEPTIONS
      foreign_lock     = 1
      object_not_found = 2
      system_failure   = 3
      OTHERS           = 4.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZGL071P'
      quantity                = '0000000001'
      ignore_buffer           = 'X'
    IMPORTING
      number                  = p_id_parametro
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Desbloqueia o objeto de numeração
  CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
    EXPORTING
      object           = 'ZGL071P'
    EXCEPTIONS
      object_not_found = 1
      OTHERS           = 2.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VALIDAR_ANTES_SALVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_CK_VALIDADO  text
*----------------------------------------------------------------------*
FORM validar_antes_salvar  CHANGING p_ck_validado TYPE char01.

  DATA: p_total   TYPE i,
        p_posicao TYPE i,
        texto     TYPE string,
        lc_campos TYPE string,
        lc_chave  TYPE string.

  p_ck_validado = abap_true.

  "IT_095_DEL

  DESCRIBE TABLE it_095 LINES p_total.

  p_posicao = 0.
  texto = |Verificado Registros... { p_posicao } de { p_total } |.

  CALL FUNCTION 'Z_01_DRE_STATUS'
    EXPORTING
      texto     = texto
      p_total   = p_total
      p_posicao = p_posicao.

  SELECT * INTO TABLE @DATA(it_zglt095)
    FROM zglt095.

  "SORT IT_ZGLT095 BY CLASSIFICACAO TP_TAXA CHAVE_LCTO TIPO_MOV_I RAZAO_ESPECIAL.
  "SORT IT_095_DEL BY CLASSIFICACAO TP_TAXA CHAVE_LCTO TIPO_MOV_I RAZAO_ESPECIAL.

  SELECT *
    INTO TABLE @DATA(it_tbsl_val)
    FROM tbsl.

  SORT it_tbsl_val BY bschl.

  LOOP AT it_095 INTO DATA(wa_095).

    DATA(lc_index) = sy-tabix.

    IF wa_095-classificacao IS INITIAL.
      CLEAR: p_ck_validado.
      grid->set_scroll_info_via_id( EXPORTING is_row_info = VALUE #( index = lc_index ) is_col_info = VALUE #( ) ).
      grid->set_selected_rows( EXPORTING it_index_rows = VALUE #( ( index = lc_index ) ) ).
      MESSAGE s097 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF wa_095-tp_taxa IS INITIAL.
      CLEAR: p_ck_validado.
      grid->set_scroll_info_via_id( EXPORTING is_row_info = VALUE #( index = lc_index ) is_col_info = VALUE #( ) ).
      grid->set_selected_rows( EXPORTING it_index_rows = VALUE #( ( index = lc_index ) ) ).
      MESSAGE s098 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF wa_095-chave_lcto IS INITIAL.
      CLEAR: p_ck_validado.
      grid->set_scroll_info_via_id( EXPORTING is_row_info = VALUE #( index = lc_index ) is_col_info = VALUE #( ) ).
      grid->set_selected_rows( EXPORTING it_index_rows = VALUE #( ( index = lc_index ) ) ).
      MESSAGE s099 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF wa_095-texto_historico IS INITIAL.
      CLEAR: p_ck_validado.
      grid->set_scroll_info_via_id( EXPORTING is_row_info = VALUE #( index = lc_index ) is_col_info = VALUE #( ) ).
      grid->set_selected_rows( EXPORTING it_index_rows = VALUE #( ( index = lc_index ) ) ).
      MESSAGE s102 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    READ TABLE it_tbsl_val INTO DATA(wa_tbsl_val) WITH KEY bschl = wa_095-chave_lcto BINARY SEARCH.

    "Se não for conta de cliente/fornecedor deve ser informado a conta
    IF sy-subrc IS NOT INITIAL.
*      CLEAR: p_ck_validado.
*      grid->set_scroll_info_via_id( EXPORTING is_row_info = VALUE #( index = lc_index ) is_col_info = VALUE #( ) ).
*      grid->set_selected_rows( EXPORTING it_index_rows = VALUE #( ( index = lc_index ) ) ).
*      MESSAGE s101 DISPLAY LIKE 'E'.
*      EXIT.
    ELSEIF wa_095-conta IS INITIAL AND wa_tbsl_val-koart NE 'D' AND wa_tbsl_val-koart NE 'K'.
      CLEAR: p_ck_validado.
      grid->set_scroll_info_via_id( EXPORTING is_row_info = VALUE #( index = lc_index ) is_col_info = VALUE #( ) ).
      grid->set_selected_rows( EXPORTING it_index_rows = VALUE #( ( index = lc_index ) ) ).
      MESSAGE s099 DISPLAY LIKE 'E'.
      EXIT.
    ELSEIF wa_095-conta IS NOT INITIAL.
      CASE wa_tbsl_val-koart.
        WHEN 'S'.
          SELECT  SINGLE * FROM skb1 INTO  @DATA(wa_skb1) WHERE saknr EQ @wa_095-conta.
          SELECT SINGLE *  FROM ska1 INTO  @DATA(wa_ska1) WHERE saknr EQ @wa_skb1-saknr AND saknr NE @space.
          IF sy-subrc IS NOT INITIAL.
            CLEAR: p_ck_validado.
            grid->set_scroll_info_via_id( EXPORTING is_row_info = VALUE #( index = lc_index ) is_col_info = VALUE #( ) ).
            grid->set_selected_rows( EXPORTING it_index_rows = VALUE #( ( index = lc_index ) ) ).
            MESSAGE s095 WITH wa_095-conta DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
        WHEN 'D'.

          SELECT SINGLE * INTO @DATA(wa_kna1)
            FROM kna1
           WHERE kunnr EQ @wa_095-conta.

          IF sy-subrc IS NOT INITIAL.
            CLEAR: p_ck_validado.
            grid->set_scroll_info_via_id( EXPORTING is_row_info = VALUE #( index = lc_index ) is_col_info = VALUE #( ) ).
            grid->set_selected_rows( EXPORTING it_index_rows = VALUE #( ( index = lc_index ) ) ).
            MESSAGE s079 WITH wa_095-conta DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

        WHEN 'K'.

          SELECT SINGLE * INTO @DATA(wa_lfa1)
            FROM lfa1
           WHERE lifnr EQ @wa_095-conta.

          IF sy-subrc IS NOT INITIAL.
            CLEAR: p_ck_validado.
            grid->set_scroll_info_via_id( EXPORTING is_row_info = VALUE #( index = lc_index ) is_col_info = VALUE #( ) ).
            grid->set_selected_rows( EXPORTING it_index_rows = VALUE #( ( index = lc_index ) ) ).
            MESSAGE s078 WITH wa_095-conta DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

      ENDCASE.
    ENDIF.

    "Verificar se já existe outro registro com a mesma parametrização.
    READ TABLE it_zglt095
    WITH KEY classificacao  = wa_095-classificacao
             tp_taxa        = wa_095-tp_taxa
             chave_lcto     = wa_095-chave_lcto
             conta          = wa_095-conta
             INTO DATA(wa_zglt095).

    IF sy-subrc IS INITIAL AND wa_zglt095-id_parametro NE wa_095-id_parametro.
      "Se não será apagada tem que dar erro AQUI SOMENTE DEVE ESTAR REGISTROS COM ID
      READ TABLE it_095_del
      WITH KEY classificacao  = wa_095-classificacao
               tp_taxa        = wa_095-tp_taxa
               chave_lcto     = wa_095-chave_lcto
               conta          = wa_095-conta
               TRANSPORTING NO FIELDS.

      IF sy-subrc IS NOT INITIAL.

        CLEAR: p_ck_validado.
        lc_chave = |{ wa_095-classificacao }/{ wa_095-tp_taxa }/{ wa_095-chave_lcto }/{ wa_095-conta }|.
        lc_campos = 'Classif'.
        lc_campos = zcl_string=>concat( s1 = lc_campos s2 = 'Tp.Taxa'  sp = '/' ).
        lc_campos = zcl_string=>concat( s1 = lc_campos s2 = 'Chv'  sp = '/' ).
        lc_campos = zcl_string=>concat( s1 = lc_campos s2 = 'Conta'  sp = '/' ).

        grid->set_scroll_info_via_id(
          EXPORTING
            is_row_info = VALUE #( index = lc_index )
            is_col_info = VALUE #( )
          ).

        grid->set_selected_rows( EXPORTING it_index_rows = VALUE #( ( index = lc_index ) )
          ).

        MESSAGE s094 WITH lc_campos lc_chave DISPLAY LIKE 'E'.
        EXIT.

      ENDIF.
    ENDIF.

    ADD 1 TO p_posicao.
    texto = |Verificado Registros... { p_posicao } de { p_total } |.

    CALL FUNCTION 'Z_01_DRE_STATUS'
      EXPORTING
        texto     = texto
        p_total   = p_total
        p_posicao = p_posicao.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0103_exit INPUT.

  DATA: answer TYPE c.

  IF ck_alterou EQ abap_true.

    DATA(lc_text_question) = |Deseja realmente sair?|.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Parâmetro de Contabilização'
        text_question         = lc_text_question
        text_button_1         = 'Sim'
        icon_button_1         = 'ICON_CHECKED'
        text_button_2         = 'Não'
        icon_button_2         = 'ICON_INCOMPLETE'
        default_button        = '2'
        display_cancel_button = ' '
      IMPORTING
        answer                = answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    CHECK answer EQ 1.

    ck_alterou = abap_false.

  ENDIF.

  IF ck_bloqueado EQ abap_true.
    CALL FUNCTION 'ZDENQUEUE_CPROG'
      EXPORTING
        cprog = sy-cprog.
  ENDIF.

  IF grid IS NOT INITIAL.
    grid->free( ).
  ENDIF.
  CLEAR: grid.

  IF ctl_cccontainer IS NOT INITIAL.
    ctl_cccontainer->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer.

  IF dg_splitter IS NOT INITIAL.
    dg_splitter->free( ).
  ENDIF.
  CLEAR: dg_splitter.

  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  MAIN_PAI_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE main_pai_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SET_MODELAGEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_modelagem INPUT.
  r_main->set_modelagem( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_FILIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_filial INPUT.

  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_branch OCCURS 0,
          branch TYPE j_1bbranch-branch,
          name   TYPE j_1bbranch-name,
        END OF tl_branch.

  IF wa_092-bukrs IS INITIAL.
    GET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
  ENDIF.

  IF wa_092-bukrs IS NOT INITIAL.
    SELECT branch name
      FROM j_1bbranch
      INTO TABLE tl_branch
      WHERE bukrs = wa_092-bukrs.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'BRANCH'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'TL_BRANCH-GSBER'
        value_org       = 'S'
      TABLES
        value_tab       = tl_branch
        return_tab      = tl_return_tab
        dynpfld_mapping = tl_dselc.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VAL_KOSTL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE val_kostl INPUT.

*  CLEAR: wa_092-kostl.
*
*  CHECK wa_092-kostl IS NOT INITIAL.
*
*  SELECT SINGLE *
*    INTO @DATA(wa_csks)
*    FROM csks
*   WHERE bukrs EQ @wa_092-bukrs AND
*         gsber EQ @wa_092-filial AND
**         ( datbi LE @sy-datum or
*         datab LE @sy-datum.
*
*  wa_092-kostl = wa_csks-kostl.
*
*  CHECK sy-subrc IS NOT INITIAL.
*
*  MESSAGE e124 WITH wa_092-kostl.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_KOSTL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_kostl INPUT.

  DATA: tl_return_tab1 TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc1      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_csks OCCURS 0,
          bukrs TYPE csks-bukrs,
          gsber TYPE csks-gsber,
          ktext TYPE cskt-ktext,
          kostl TYPE csks-kostl,
        END OF tl_csks.

*  IF wa_092-bukrs IS INITIAL.
*    GET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
*  ENDIF.

*  IF wa_092-bukrs IS NOT INITIAL AND wa_092-filial IS NOT INITIAL.
  SELECT a~bukrs, a~gsber, b~ktext, a~kostl
    INTO TABLE @tl_csks
    FROM csks AS a
    LEFT JOIN cskt AS b ON a~kokrs EQ b~kokrs AND
                           a~kostl EQ b~kostl
  WHERE a~bukrs EQ @wa_092-bukrs AND
        a~gsber EQ @wa_092-filial AND
        a~datab LE @sy-datum AND
        b~spras EQ @sy-langu.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'BRANCH'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'tl_csks-kostl'
      value_org       = 'S'
    TABLES
      value_tab       = tl_csks
      return_tab      = tl_return_tab1
      dynpfld_mapping = tl_dselc1.

*  ELSE.
*    MESSAGE w125.
*  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_CLI_FOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_cli_for INPUT.

  IF vg_radiof IS NOT INITIAL.

    DATA: tl_return_tabc TYPE TABLE OF ddshretval WITH HEADER LINE,
          tl_dselcc      TYPE TABLE OF dselc      WITH HEADER LINE.

    DATA: BEGIN OF tl_lfa1 OCCURS 0,
            lifnr TYPE lfa1-lifnr,
            name1 TYPE lfa1-name1,
          END OF tl_lfa1.

**-------------------
*
*  CLEAR wa_092-fornecedor_d.
*
*  CHECK wa_092-fornecedor IS NOT INITIAL.
*
*
*
*  wa_092-fornecedor_d = wa_lfa1-name1.
*
*  CHECK sy-subrc IS NOT INITIAL.
*
*  MESSAGE e078 WITH wa_092-fornecedor.
*
**-------------------

    SELECT lifnr, name1 INTO TABLE @DATA(lt_lfa1)
      FROM lfa1.
*   WHERE lifnr EQ @wa_092-fornecedor.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'LIFNR'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'lt_lfa1-lifnr'
        value_org       = 'S'
      TABLES
        value_tab       = tl_lfa1
        return_tab      = tl_return_tabc
        dynpfld_mapping = tl_dselcc.

  ELSE. "Customer



  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  %_P_MES  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lm_p_mes INPUT.

  IF p_mes IS INITIAL.
    MESSAGE e126.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LM_P_ANO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lm_p_ano INPUT.

  IF p_ano IS INITIAL.
    MESSAGE e126.
  ENDIF.

ENDMODULE.
