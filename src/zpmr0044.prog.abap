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
**|    + Bruna Guarez                     |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    +                    |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Monitor de ordem gerado apartir do Mobile                                 |*
**/===========================================================================\*

REPORT zpmr0044.

TABLES: zpmt0017, zpmt0015, v_qpgr_kat, qpct, sscrfields, equz.

TYPES: BEGIN OF ty_nota.
         INCLUDE TYPE zpmt0014.
         TYPES: idlog      TYPE zpmt0018-idlog,
         type       TYPE zpmt0018-type,
         message    TYPE zpmt0018-message,
         eqktx      TYPE eqkt-eqktx ,   " Denominação do objeto técnico
         pltxt      TYPE iflo-pltxt,     " Descrição do local
         cell_color TYPE lvc_t_scol,    " Cor da Célula
         icone      TYPE char5,
       END OF ty_nota.


TYPES:
  BEGIN OF ty_saida,
    aufnr      TYPE zpmt0016-aufnr,
    bloco      TYPE zpmt0016-bloco,
    ktext      TYPE zpmt0016-ktext,
    dtini      TYPE zpmt0016-dtini,
    dtfim      TYPE zpmt0016-dtfim,
    hrini      TYPE zpmt0016-hrini,
    hrfim      TYPE zpmt0016-hrfim,
    statp      TYPE zpmt0016-statp,
    idord      TYPE zpmt0016-idord,
    dtreg      TYPE zpmt0016-dtreg,
    hrreg      TYPE zpmt0016-hrreg,
    equnr      TYPE zpmt0016-equnr,
    tplnr      TYPE iloa-tplnr,
    idapn      TYPE zpmt0015-idapn,
    idopr      TYPE zpmt0017-idopr,
    idlog      TYPE zpmt0018-idlog,
    type       TYPE zpmt0018-type,
    message    TYPE zpmt0018-message,
    shtxt      TYPE itob-shtxt ,   " Denominação do objeto técnico
    pltxt      TYPE iflo-pltxt,     " Descrição do local
    cell_color TYPE lvc_t_scol,    " Cor da Célula
    icone      TYPE char5,
  END OF ty_saida,

  BEGIN OF ty_zpmt0017,
    mandt       TYPE zpmt0017-mandt,
    bloco       TYPE zpmt0017-bloco,
    idopr       TYPE zpmt0017-idopr,
    idord       TYPE zpmt0017-idord,
    vornr       TYPE zpmt0017-vornr,
    werks       TYPE zpmt0017-werks,
    arbpl       TYPE zpmt0017-arbpl,
    gewrk       TYPE zpmt0017-gewrk,
    wergw       TYPE zpmt0017-wergw,
    steus       TYPE zpmt0017-steus,
    pernr       TYPE zpmt0017-pernr,
    ntanf       TYPE zpmt0017-ntanf,
    description TYPE zpmt0017-description,
    phflg       TYPE zpmt0017-phflg,
    arbei       TYPE zpmt0017-arbei,
    arbeh       TYPE zpmt0017-arbeh,
    anzzl       TYPE zpmt0017-anzzl,
    dauno       TYPE zpmt0017-dauno,
    daune       TYPE zpmt0017-daune,
  END OF ty_zpmt0017,


  BEGIN OF ty_zpmt0018,
    mandt   TYPE zpmt0018-mandt,
    idlog   TYPE zpmt0018-idlog,
    idilg   TYPE zpmt0018-idilg,
    field   TYPE zpmt0018-field,
    bloco   TYPE zpmt0018-bloco,
    idord   TYPE zpmt0018-idord,
    idnot   TYPE zpmt0018-idnot,
    type    TYPE zpmt0018-type,
    message TYPE zpmt0018-message,
    inativo TYPE zpmt0018-inativo,

  END OF ty_zpmt0018,

  BEGIN OF ty_saida03,
    idopr TYPE zpmt0017-idopr,

  END OF ty_saida03.

TYPES: BEGIN OF ty_zpmt0019.
         INCLUDE STRUCTURE zpmt0019.
         TYPES: kurztext   TYPE qtxt_code,
         kurztext_2 TYPE qtxt_code,
         kurztext_3 TYPE qtxt_code,
         kurztext_4 TYPE qtxt_code,
       END OF ty_zpmt0019,

       BEGIN OF ty_f4,
         lifnr TYPE lifnr,
         ort01 TYPE ort01_gp,
         lgort TYPE lgort_d,
         lgobe TYPE lgobe,
       END OF ty_f4.

TYPES:
  BEGIN OF ty_0108,
    code       TYPE qcode,
    kurztext   TYPE qtxt_code,
    kurztext_2 TYPE qtxt_code,
    kurztext_3 TYPE qtxt_code,
    kurztext_4 TYPE qtxt_code,
  END OF ty_0108.


DATA:
  gt_saida      TYPE TABLE OF ty_saida,
  gs_saida      TYPE TABLE OF ty_saida,
  gw_saida      TYPE TABLE OF ty_saida,
  it_saida      TYPE TABLE OF ty_saida,
  it_exib_erros TYPE TABLE OF ty_saida,
  gt_nota       TYPE TABLE OF ty_nota,
  gs_nota       TYPE TABLE OF ty_nota,
  gw_nota       TYPE TABLE OF ty_nota,
  w_nota        TYPE zpmt0014,
  t_nota        TYPE TABLE OF ty_zpmt0019,
  wg_itens      TYPE v_qpgr_kat,
  it_qpgr       TYPE TABLE OF  ty_0108,
  ws_qpgr       TYPE  v_qpgr_kat,
  s_nota        TYPE zpmt0019,
  it_msg_log    TYPE TABLE OF ty_zpmt0018,
  it_saida3     TYPE TABLE OF ty_zpmt0017,
  it_zpmt0018   TYPE TABLE OF ty_zpmt0018,
  wa_saida      TYPE ty_saida,
  wa_saida3     TYPE ty_zpmt0017,
  wa_zpmt0018   TYPE ty_zpmt0018,
  it_color      TYPE TABLE OF lvc_s_scol,  " Cor para célula
  wa_color      TYPE          lvc_s_scol,
  it_f4         TYPE TABLE OF  ty_0108,
  it_return     TYPE TABLE OF ddshretval,
  wa_return     LIKE LINE  OF it_return,
  w_zpmt0016    TYPE zpmt0016.

DATA: w_saida TYPE ty_saida,
      aufnr   TYPE aufnr.

FIELD-SYMBOLS: <wa_0108>  TYPE  ty_0108,
               <wa_f4>    TYPE  ty_0108,
               <wa_lfa1>  TYPE lfa1,
               <wa_t001l> TYPE t001l,
               <wa_itens> TYPE qpct.

DATA:
  BEGIN OF g_tabstrip,
    tab1 TYPE char40,
    tab2 TYPE char40,
    tab3 TYPE char40,
    qtd1 TYPE char3 VALUE '0',
    qtd2 TYPE char3 VALUE '0',
    qtd3 TYPE char3 VALUE '0',
  END OF g_tabstrip,

  BEGIN OF g_ts_0100,
    subscreen   LIKE sy-dynnr VALUE '0100',
    program     LIKE sy-repid VALUE 'ZPMR0044',
    pressed_tab LIKE sy-ucomm VALUE 'TAB_DISPONIVEIS',
  END OF g_ts_0100.

CONTROLS itabstrip TYPE TABSTRIP.




*-----------------------------------------------------------------------------*
** DECLARAÇÃO DE VARIÁVEIS                                                    **
*-----------------------------------------------------------------------------*
DATA:
  cap_total   TYPE p DECIMALS 2,
  temp_total  TYPE p DECIMALS 2,
  temp_trab   TYPE p DECIMALS 2,
  gra_ut      TYPE p DECIMALS 2,
  temp_utiz   TYPE p DECIMALS 2,
  lv_segundos TYPE p DECIMALS 2,
  lv_date1    TYPE p,
  lv_date2    TYPE p,
  lv_tabix    TYPE sy-tabix,
  voltando    TYPE char1.



CONSTANTS: lc_u(1)         VALUE 'U',
           lc_a(1)         VALUE 'A',
           lc_1(1)         VALUE '1',
           lc_nops(4)      VALUE 'NOPS',
           lc_no_predit(9) VALUE 'NO_PREDIT'.
*


*----------------------------------------------------*
*                ALV GRID                            *
*----------------------------------------------------*



DATA:
  g_custom_container    TYPE REF TO cl_gui_custom_container,
  g_custom_container2   TYPE REF TO cl_gui_custom_container,
  g_custom_container3   TYPE REF TO cl_gui_custom_container,
  g_container           TYPE REF TO cl_gui_custom_container,
  g_container3          TYPE REF TO cl_gui_custom_container,
  dg_splitter_1         TYPE REF TO cl_gui_splitter_container,
  dg_splitter_2         TYPE REF TO cl_gui_splitter_container,
  dg_parent_1           TYPE REF TO cl_gui_container,
  dg_parent_2           TYPE REF TO cl_gui_container,
  dg_parent_3           TYPE REF TO cl_gui_container,
  dg_parent_4           TYPE REF TO cl_gui_container,
  dg_parent_2a          TYPE REF TO cl_gui_container,
  dg_parent_2a_2        TYPE REF TO cl_gui_container,
  dg_parent_alv         TYPE REF TO cl_gui_container,
  dg_parent_alv_2       TYPE REF TO cl_gui_container,
  obj_custom_0150       TYPE REF TO cl_gui_custom_container,
  obj_custom_0110       TYPE REF TO cl_gui_custom_container,
  obj_custom_0120       TYPE REF TO cl_gui_custom_container,
  obj_custom_0130       TYPE REF TO cl_gui_custom_container,
  obj_custom_0140       TYPE REF TO cl_gui_custom_container,
  obj_custom_0160       TYPE REF TO cl_gui_custom_container,
  picture               TYPE REF TO cl_gui_picture,
  picture_2             TYPE REF TO cl_gui_picture,
*  OBJ_CUSTOM_0110     TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  obj_alv_0150          TYPE REF TO cl_gui_alv_grid,
  obj_alv_0110          TYPE REF TO cl_gui_alv_grid,
  obj_alv_0120          TYPE REF TO cl_gui_alv_grid,
  obj_alv_0130          TYPE REF TO cl_gui_alv_grid,
  obj_alv_0140          TYPE REF TO cl_gui_alv_grid,
  obj_alv_0160          TYPE REF TO cl_gui_alv_grid,
  ctl_alv               TYPE REF TO cl_gui_alv_grid,
  dg_dyndoc_id          TYPE REF TO cl_dd_document,
  dg_dyndoc_id_2        TYPE REF TO cl_dd_document,
  table_element         TYPE REF TO cl_dd_table_element,
  table_element_2       TYPE REF TO cl_dd_table_element,
  column                TYPE REF TO cl_dd_area,
  column_2              TYPE REF TO cl_dd_area,
  table_element2        TYPE REF TO cl_dd_table_element,
  table_element3        TYPE REF TO cl_dd_table_element,
  column_1              TYPE REF TO cl_dd_area,
  column_3              TYPE REF TO cl_dd_area,
  dg_html_cntrl         TYPE REF TO cl_gui_html_viewer,
  dg_html_cntrl_2       TYPE REF TO cl_gui_html_viewer,
  it_exclude_fcode      TYPE ui_functions,
  it_exclude_fcode_0120 TYPE ui_functions,
  wa_exclude_fcode      LIKE LINE OF it_exclude_fcode,
  it_exclude_fcode_2    TYPE ui_functions,
  it_exclude_fcode_3    TYPE ui_functions,
  wa_exclude_fcode_2    LIKE LINE OF it_exclude_fcode,
  wa_exclude_fcode_3    LIKE LINE OF it_exclude_fcode,
  gs_layout             TYPE lvc_s_layo,
  gs_variant            TYPE disvariant,
  it_fieldcatalog       TYPE lvc_t_fcat,
  lt_f4                 TYPE lvc_t_f4 WITH HEADER LINE,
  wa_fieldcatalog       TYPE lvc_s_fcat,
  it_sort               TYPE lvc_t_sort,
  ls_stable             TYPE lvc_s_stbl,
  t_sort                TYPE lvc_t_sort,
  w_sort                TYPE lvc_t_sort WITH HEADER LINE,
  it_fieldcatalog2      TYPE lvc_t_fcat,
  it_fieldcatalog_0120  TYPE lvc_t_fcat,
  wa_fieldcatalog2      TYPE lvc_s_fcat,
  gs_layout2            TYPE lvc_s_layo,
  gs_layout_0120        TYPE lvc_s_layo,
  gs_variant2           TYPE disvariant,
  ctl_alv2              TYPE REF TO cl_gui_alv_grid,
  dg_parent_alv2        TYPE REF TO cl_gui_container,
  ls_stable2            TYPE lvc_s_stbl,
  it_exclude_fcode2     TYPE ui_functions,
  wa_exclude_fcode2     LIKE LINE OF it_exclude_fcode,
  it_fieldcatalog3      TYPE lvc_t_fcat,
  wa_fieldcatalog3      TYPE lvc_s_fcat,
  gs_layout3            TYPE lvc_s_layo,
  gs_variant3           TYPE disvariant,
  ctl_alv3              TYPE REF TO cl_gui_alv_grid,
  dg_parent_alv3        TYPE REF TO cl_gui_container,
  ls_stable3            TYPE lvc_s_stbl,
  it_exclude_fcode3     TYPE ui_functions,
  wa_exclude_fcode3     LIKE LINE OF it_exclude_fcode.

DATA: setor(30) TYPE c.

DATA: "R_EVENT_HANDLER TYPE REF TO LCL_EVENTS_HANDLER,
  i_selected_rows TYPE lvc_t_row,                "Linhas selecionadas
  w_selected_rows TYPE lvc_s_row.                "Colunas Selecionadas

TYPES: BEGIN OF ty_ucomm,
         ucomm TYPE  sy-ucomm,
       END OF ty_ucomm.

DATA: icon_proc TYPE string.



CLASS lcl_eventos DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data
                  et_bad_cells e_display.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

ENDCLASS.


CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.


ENDCLASS.


CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_double_click.


    CHECK e_row IS NOT INITIAL.

    PERFORM select_dados USING  e_row e_column.

  ENDMETHOD.


  METHOD on_data_changed.

  ENDMETHOD.                    "on_data_changed


  METHOD on_data_changed_finished.

    FIELD-SYMBOLS <saida> TYPE ty_saida.

  ENDMETHOD.


ENDCLASS.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-005.
SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN POSITION 01.
PARAMETER: rb_err RADIOBUTTON GROUP g1 USER-COMMAND abc.
SELECTION-SCREEN COMMENT 05(10) text-003 FOR FIELD rb_err.

SELECTION-SCREEN POSITION 30.
PARAMETER: rb_suc RADIOBUTTON GROUP g1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(10) text-004 FOR FIELD rb_suc.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b3.


SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN POSITION 01.
PARAMETER: rb_ord RADIOBUTTON GROUP g2 DEFAULT 'X' USER-COMMAND a.
SELECTION-SCREEN COMMENT 05(10) text-c02 FOR FIELD rb_ord.

SELECTION-SCREEN POSITION 30.
PARAMETER: rb_not RADIOBUTTON GROUP g2 .
SELECTION-SCREEN COMMENT 20(10) text-c03 FOR FIELD rb_not.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME  TITLE text-001.
SELECT-OPTIONS: p_iwerk  FOR equz-iwerk. "EQUIPAMENTO
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.

INITIALIZATION.

  icon_proc = 'Atualizar base de dados para Aplicativo'.

  sscrfields-functxt_01 = icon_proc .
  sscrfields-functxt_02 = 'Exibir status processamento'.

AT SELECTION-SCREEN. "PAI
  CASE sscrfields-ucomm. "pushbutton pressed
    WHEN 'FC01'.

      PERFORM me_proc_job.

    WHEN 'FC02'.
      PERFORM exibir_status_proc.

  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE screen-name.

      WHEN 'RB_ORD' OR 'RB_NOT' OR '%B002010_BLOCK_1000' OR %fc02014_1000 OR %fc03017_1000.

        IF rb_suc IS NOT INITIAL.
          screen-active = '0'.         "show parameters     "n921165
        ELSE.                                               "n921165
          screen-active = '1'.         "Hide parameters     "n921165
        ENDIF.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.





CLASS lcl_eventos IMPLEMENTATION.
  METHOD on_hotspot_click.

    DATA: setor(30) TYPE c.

    CLEAR wa_saida.
    IF rb_ord EQ 'X'.
      READ TABLE gt_saida INTO wa_saida INDEX e_row_id-index.
      CASE e_column_id-fieldname.
        WHEN:'AUFNR'."Ordem Manuteção

          SET PARAMETER ID 'ANR' FIELD wa_saida-aufnr.
          IF wa_saida-statp  NE 'E'.
            CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN . " só visualizar
          ELSE.
            CALL TRANSACTION 'IW32' AND SKIP FIRST SCREEN . " EDITA
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD on_onf4.

    TYPES: BEGIN OF ty_field,
             tabname   TYPE dd03l-tabname,     "Nome da tabela
             fieldname TYPE dd03l-fieldname,   "Nome de campo
             S(1)      TYPE c,
           END OF ty_field,

           BEGIN OF ty_value,
             tabname    TYPE dd03l-tabname,     "Nome da tabela
             fieldname  TYPE dd03l-fieldname,   "Nome de campo
             char79(79) TYPE c,
           END OF ty_value.

    DATA: BEGIN OF wl_valuetab,
            field(50),
          END OF wl_valuetab.

    DATA: tl_valuetab      LIKE TABLE OF wl_valuetab,
          tl_field         TYPE TABLE OF ty_field,
          wl_field         TYPE ty_field,
          tl_value         TYPE TABLE OF ty_value,
          wl_value         TYPE ty_value,
          tl_user          TYPE TABLE OF usrefus,
          wl_user          TYPE usrefus,

*          WG_ITENS         LIKE LINE OF TG_SAIDA,

          wl_index         TYPE sy-tabix,
          wl_char(20),
          wl_fieldname(30),
          wl_tabname(30).


    CASE e_fieldname.
      WHEN 'OTEIL'.
        READ TABLE t_nota INTO DATA(w_not) INDEX sy-tabix.
        SELECT code kurztext
        FROM qpct
        INTO TABLE it_qpgr
          WHERE katalogart EQ w_not-otkat
            AND codegruppe EQ w_not-otgrp.

        CHECK it_qpgr IS NOT INITIAL.

        LOOP AT it_qpgr ASSIGNING <wa_0108>.
          APPEND INITIAL LINE TO it_f4 ASSIGNING <wa_f4>.
          MOVE <wa_0108>-code TO <wa_f4>-code.
          MOVE <wa_0108>-kurztext TO <wa_f4>-kurztext.
        ENDLOOP.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'CODE'
            dynpprog        = sy-repid
            dynpnr          = sy-dynnr
            value_org       = 'S'
          TABLES
            value_tab       = it_f4
            return_tab      = it_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

        IF sy-subrc IS INITIAL.
          LOOP AT it_return INTO wa_return.
            READ TABLE t_nota ASSIGNING FIELD-SYMBOL(<w_not>) INDEX es_row_no-row_id.
            IF sy-subrc IS INITIAL.
              IF wa_return-fieldval IS NOT INITIAL.
                SELECT SINGLE *
                FROM qpct
                INTO @DATA(_qpct)
                  WHERE katalogart EQ @w_not-otkat
                    AND codegruppe EQ @w_not-otgrp
                   AND code EQ @wa_return-fieldval.

                MOVE wa_return-fieldval TO <w_not>-oteil.
                MOVE _qpct-kurztext TO <w_not>-kurztext.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
        CLEAR:w_not.

      WHEN 'FECOD'.
        FREE:  it_qpgr.
        READ TABLE t_nota INTO w_not INDEX sy-tabix.
        SELECT code kurztext
        FROM qpct
        INTO TABLE it_qpgr
          WHERE katalogart EQ w_not-fekat
            AND codegruppe EQ w_not-fegrp.

        CHECK it_qpgr IS NOT INITIAL.

        LOOP AT it_qpgr ASSIGNING <wa_0108>.
          APPEND INITIAL LINE TO it_f4 ASSIGNING <wa_f4>.
          MOVE <wa_0108>-code TO <wa_f4>-code.
          MOVE <wa_0108>-kurztext TO <wa_f4>-kurztext_2.
        ENDLOOP.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'CODE'
            dynpprog        = sy-repid
            dynpnr          = sy-dynnr
            value_org       = 'S'
          TABLES
            value_tab       = it_f4
            return_tab      = it_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

        IF sy-subrc IS INITIAL.
          LOOP AT it_return INTO wa_return.
            READ TABLE t_nota ASSIGNING <w_not> INDEX es_row_no-row_id.
            IF sy-subrc IS INITIAL.
              IF wa_return-fieldval IS NOT INITIAL.
                SELECT SINGLE *
                FROM qpct
                INTO _qpct
                  WHERE katalogart EQ w_not-fekat
                    AND codegruppe EQ w_not-fegrp
                   AND code EQ wa_return-fieldval.

                MOVE wa_return-fieldval TO <w_not>-fecod.
                MOVE _qpct-kurztext TO <w_not>-kurztext_2.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

      WHEN 'URCOD'.
        FREE:  it_qpgr.
        READ TABLE t_nota INTO w_not INDEX sy-tabix.
        SELECT code kurztext
        FROM qpct
        INTO TABLE it_qpgr
          WHERE katalogart EQ w_not-urkat
            AND codegruppe EQ w_not-urgrp.

        CHECK it_qpgr IS NOT INITIAL.

        LOOP AT it_qpgr ASSIGNING <wa_0108>.
          APPEND INITIAL LINE TO it_f4 ASSIGNING <wa_f4>.
          MOVE <wa_0108>-code TO <wa_f4>-code.
          MOVE <wa_0108>-kurztext TO <wa_f4>-kurztext_2.
        ENDLOOP.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'CODE'
            dynpprog        = sy-repid
            dynpnr          = sy-dynnr
            value_org       = 'S'
          TABLES
            value_tab       = it_f4
            return_tab      = it_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

        IF sy-subrc IS INITIAL.
          LOOP AT it_return INTO wa_return.
            READ TABLE t_nota ASSIGNING <w_not> INDEX es_row_no-row_id.
            IF sy-subrc IS INITIAL.
              IF wa_return-fieldval IS NOT INITIAL.
                SELECT SINGLE *
                FROM qpct
                INTO _qpct
                  WHERE katalogart EQ w_not-urkat
                    AND codegruppe EQ w_not-urgrp
                   AND code EQ wa_return-fieldval.

                MOVE wa_return-fieldval TO <w_not>-urcod.
                MOVE _qpct-kurztext TO <w_not>-kurztext_3.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

        CLEAR:w_not.
    ENDCASE.


*
**** Método de atualização de dados na Tela
    CALL METHOD obj_alv_0160->refresh_table_display
      EXPORTING
        is_stable = ls_stable.

  ENDMETHOD.

  METHOD on_data_changed.

    DATA: ls_good    TYPE lvc_s_modi.
    DATA: w_not TYPE ty_zpmt0019.
    DATA: ws_not TYPE ty_zpmt0019.

    CHECK t_nota IS NOT INITIAL.

    CLEAR: w_not, lv_tabix.

*Verificando informações parte do objeto.
    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'OTEIL'.

      READ TABLE t_nota INTO w_not INDEX ls_good-row_id.
      DATA(lv_value) = ls_good-value.
      CONDENSE lv_value.

      IF sy-subrc EQ 0.
        SELECT SINGLE *
        FROM qpct
        INTO @DATA(w_qpct)
          WHERE katalogart = @w_not-otkat
            AND codegruppe = @w_not-otgrp
            AND code       = @lv_value.

        IF w_qpct IS NOT INITIAL.
          MOVE: w_qpct-kurztext TO lv_value.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'KURZTEXT'
              i_value     = lv_value.

          CLEAR: lv_value.
        ELSE.
          MESSAGE text-006 TYPE 'I' DISPLAY LIKE 'E'.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'OTEIL'
              i_value     = w_not-oteil.

        ENDIF.
      ENDIF.
    ENDLOOP.

*Verificando informações de danos.
    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'FECOD'.

      READ TABLE t_nota INTO w_not INDEX ls_good-row_id.
      CLEAR: lv_value.
      lv_value = ls_good-value.
      CONDENSE lv_value.

      CLEAR w_qpct.
      IF sy-subrc EQ 0.
        SELECT SINGLE *
        FROM qpct
        INTO w_qpct
          WHERE katalogart = w_not-fekat
            AND codegruppe = w_not-fegrp
            AND code       = lv_value.


        IF w_qpct IS NOT INITIAL.
          MOVE: w_qpct-kurztext TO lv_value.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'KURZTEXT_2'
              i_value     = lv_value.

          CLEAR: lv_value.
        ELSE.
          MESSAGE text-006 TYPE 'I' DISPLAY LIKE 'E'.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'FECOD'
              i_value     = w_not-fecod.

        ENDIF.
      ENDIF.
    ENDLOOP.


*Verificando informações da causas.
    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'URCOD'.

      READ TABLE t_nota INTO w_not INDEX ls_good-row_id.
      CLEAR: lv_value.
      lv_value = ls_good-value.
      CONDENSE lv_value.

      CLEAR w_qpct.
      IF sy-subrc EQ 0.
        SELECT SINGLE *
        FROM qpct
        INTO w_qpct
          WHERE katalogart = w_not-urkat
            AND codegruppe = w_not-urgrp
            AND code       = lv_value.


        IF w_qpct IS NOT INITIAL.
          MOVE: w_qpct-kurztext TO lv_value.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'KURZTEXT_3'
              i_value     = lv_value.


          CLEAR: lv_value.
        ELSE.
          MESSAGE text-006 TYPE 'I' DISPLAY LIKE 'E'.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'URCOD'
              i_value     = w_not-urcod.

        ENDIF.
      ENDIF.
    ENDLOOP.


*Verificando informações da ação.
    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'MNCOD'.

      READ TABLE t_nota INTO w_not INDEX ls_good-row_id.
      CLEAR: lv_value.
      lv_value = ls_good-value.
      CONDENSE lv_value.

      CLEAR w_qpct.
      IF sy-subrc EQ 0.
        SELECT SINGLE *
        FROM qpct
        INTO w_qpct
          WHERE katalogart = w_not-mnkat
            AND codegruppe = w_not-mngrp
            AND code       = lv_value.


        IF w_qpct IS NOT INITIAL.
          MOVE: w_qpct-kurztext TO lv_value.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'KURZTEXT_4'
              i_value     = lv_value.


          CLEAR: lv_value.
        ELSE.
          MESSAGE text-006 TYPE 'I' DISPLAY LIKE 'E'.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'MNCOD'
              i_value     = w_not-mncod.

        ENDIF.
      ENDIF.
    ENDLOOP.




  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  PERFORM buscar_dados.
  PERFORM zf_execut_alv.

FORM buscar_dados.

  IF rb_err IS NOT INITIAL AND rb_ord IS NOT INITIAL.
*    Selecionando dados ordens modificadas.
    PERFORM sel_ordem_mof.

*    Selecionando dados ordens criadas.
    PERFORM sel_ordem_nova.
    EXIT.
  ENDIF.

  IF rb_suc IS NOT INITIAL.
*       selecionar dados processados.
    PERFORM sel_dados_suc.
    PERFORM sel_not_proc.
    EXIT.
  ENDIF.

  IF rb_err IS NOT INITIAL AND rb_not IS NOT INITIAL.
*    Seleciona dados nota criada.
    PERFORM sel_notas_modif.

    PERFORM sel_notas_nova.
    EXIT.
  ENDIF.




ENDFORM.



FORM zf_execut_alv .
  CALL SCREEN 0400.
ENDFORM.


*Parametros da ALV.
FORM fill_it_fieldcatalog USING VALUE(p_colnum)
                                VALUE(p_fieldname)
                                VALUE(p_tabname)
                                VALUE(p_len)
                                VALUE(p_edit)
                                VALUE(p_icon)
                                VALUE(p_do_sum)
                                VALUE(p_header)
                                VALUE(p_emphasize)
                                VALUE(p_hotspot)
                                VALUE(p_ref_table)
                                VALUE(p_ref_field)
                                VALUE(p_no_zero)
                                VALUE(p_outputlen).



  DATA:  wa_fieldcatalog  TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos     = p_colnum.
  wa_fieldcatalog-fieldname   = p_fieldname.
  wa_fieldcatalog-tabname     = p_tabname.
  wa_fieldcatalog-outputlen   = p_len.
  wa_fieldcatalog-edit        = p_edit.
  wa_fieldcatalog-icon        = p_icon.
  wa_fieldcatalog-do_sum      = p_do_sum.
  wa_fieldcatalog-coltext     = p_header.
  wa_fieldcatalog-emphasize   = p_emphasize.
  wa_fieldcatalog-hotspot     = p_hotspot.
  wa_fieldcatalog-ref_table   = p_ref_table.
  wa_fieldcatalog-ref_field   = p_ref_field.
  wa_fieldcatalog-no_zero     = p_no_zero.
  wa_fieldcatalog-outputlen   = p_outputlen.

  gs_layout-info_fname     = 'ROWCOLOR'.  "Row color
  gs_layout-ctab_fname     = 'CELL_COLOR'.
  gs_layout-excp_conds    = 'X'.
  gs_layout-zebra         = 'X'.
  gs_layout-sel_mode      = 'A'.
  gs_layout-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  gs_layout-totals_bef    = ' '.

  APPEND wa_fieldcatalog TO it_fieldcatalog.


ENDFORM.                    " F_DEFINE_CONTAINER_HEADER

FORM it_fieldcatalog USING VALUE(p_colnum)
                           VALUE(p_field) LIKE dd03d-fieldname
                           VALUE(p_tabname)
                           VALUE(p_len)
                           VALUE(p_edit)
                           VALUE(p_icon)
                           VALUE(p_do_sum)
                           VALUE(p_header)
                           VALUE(p_emphasize)
                           VALUE(p_hotspot)
                           VALUE(p_ref_table)
                           VALUE(p_ref_field)
                           VALUE(p_no_zero)
                           VALUE(p_outputlen).


  DATA:  wa_fieldcatalog  TYPE lvc_s_fcat.
  CLEAR gs_layout.

  wa_fieldcatalog-col_pos     = p_colnum.
  wa_fieldcatalog-fieldname   = p_field.
  wa_fieldcatalog-tabname     = p_tabname.
  wa_fieldcatalog-outputlen   = p_len.
  wa_fieldcatalog-edit        = p_edit.
  wa_fieldcatalog-icon        = p_icon.
  wa_fieldcatalog-do_sum      = p_do_sum.
  wa_fieldcatalog-coltext     = p_header.
  wa_fieldcatalog-emphasize   = p_emphasize.
  wa_fieldcatalog-hotspot     = p_hotspot.
  wa_fieldcatalog-ref_table   = p_ref_table.
  wa_fieldcatalog-ref_field   = p_ref_field.
  wa_fieldcatalog-no_zero     = p_no_zero.
  wa_fieldcatalog-outputlen   = p_outputlen.


*  GS_LAYOUT-INFO_FNAME     = 'ROWCOLOR'.  "Row color
*  GS_LAYOUT-CTAB_FNAME     = 'CELL_COLOR'.
  gs_layout-excp_conds    = 'X'.
  gs_layout-zebra         = 'X'.
  gs_layout-sel_mode      = 'C'.
  gs_layout-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  gs_layout-totals_bef    = ' '.

  CASE p_field.
    WHEN 'OTEIL' OR 'FECOD' OR 'URCOD' OR 'MNCOD'.
      wa_fieldcatalog-f4availabl = abap_true.
  ENDCASE.

  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.                    " F_DEFINE_CONTAINER_HEADER

FORM fill_it_fieldcatalog_0120 USING VALUE(p_colnum)
                                VALUE(p_fieldname)
                                VALUE(p_tabname)
                                VALUE(p_len)
                                VALUE(p_edit)
                                VALUE(p_icon)
                                VALUE(p_do_sum)
                                VALUE(p_header)
                                VALUE(p_emphasize)
                                VALUE(p_hotspot)
                                VALUE(p_ref_table)
                                VALUE(p_ref_field)
                                VALUE(p_no_zero)
                                VALUE(p_outputlen).





  DATA:  wa_fieldcatalog_0120  TYPE lvc_s_fcat.
  CLEAR: wa_fieldcatalog_0120,
  gs_layout_0120.

  wa_fieldcatalog_0120-col_pos     = p_colnum.
  wa_fieldcatalog_0120-fieldname   = p_fieldname.
  wa_fieldcatalog_0120-tabname     = p_tabname.
  wa_fieldcatalog_0120-outputlen   = p_len.
  wa_fieldcatalog_0120-edit        = p_edit.
  wa_fieldcatalog_0120-icon        = p_icon.
  wa_fieldcatalog_0120-do_sum      = p_do_sum.
  wa_fieldcatalog_0120-coltext     = p_header.
  wa_fieldcatalog_0120-emphasize   = p_emphasize.
  wa_fieldcatalog_0120-hotspot     = p_hotspot.
  wa_fieldcatalog_0120-ref_table   = p_ref_table.
  wa_fieldcatalog_0120-ref_table   = p_ref_field.
  wa_fieldcatalog_0120-no_zero     = p_no_zero.
  wa_fieldcatalog_0120-outputlen   = p_outputlen.

  gs_layout_0120-info_fname     = 'ROWCOLOR'.  "Row color
  gs_layout_0120-ctab_fname     = 'CELL_COLOR'.
  gs_layout_0120-excp_conds     = 'X'.
  gs_layout_0120-zebra          = 'X'.
  gs_layout_0120-sel_mode       = 'A'.
  gs_layout_0120-cwidth_opt     = 'X'.     "  Otimizar colunas na tela
  gs_layout_0120-totals_bef     = ' '.

  APPEND wa_fieldcatalog_0120 TO it_fieldcatalog_0120.


ENDFORM.                    " F_DEFINE_CONTAINER_HEADER

FORM fill_it_fieldcatalog2 USING VALUE(p_colnum)
                                VALUE(p_fieldname)
                                VALUE(p_tabname)
                                VALUE(p_len)
                                VALUE(p_edit)
                                VALUE(p_icon)
                                VALUE(p_do_sum)
                                VALUE(p_header)
                                VALUE(p_emphasize)
                                VALUE(p_hotspot)
                                VALUE(p_ref_table)
                                VALUE(p_ref_field)
                                VALUE(p_no_zero)
                                VALUE(p_outputlen).



  DATA:  wa_fieldcatalog2  TYPE lvc_s_fcat.

  wa_fieldcatalog2-col_pos     = p_colnum.
  wa_fieldcatalog2-fieldname   = p_fieldname.
  wa_fieldcatalog2-tabname     = p_tabname.
  wa_fieldcatalog2-outputlen   = p_len.
  wa_fieldcatalog2-edit        = p_edit.
  wa_fieldcatalog2-icon        = p_icon.
  wa_fieldcatalog2-do_sum      = p_do_sum.
  wa_fieldcatalog2-coltext     = p_header.
  wa_fieldcatalog2-emphasize   = p_emphasize.
  wa_fieldcatalog2-hotspot     = p_hotspot.
  wa_fieldcatalog2-ref_table   = p_ref_table.
  wa_fieldcatalog2-ref_table   = p_ref_field.
  wa_fieldcatalog2-no_zero     = p_no_zero.
  wa_fieldcatalog2-outputlen   = p_outputlen.

  gs_layout2-info_fname = 'ROWCOLOR'.  "Row color
  gs_layout2-ctab_fname     = 'CELL_COLOR'.
  gs_layout2-excp_conds    = 'X'.
  gs_layout2-zebra         = 'X'.
  gs_layout2-sel_mode      = 'A'.
  gs_layout2-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  gs_layout2-totals_bef    = ' '.

  APPEND wa_fieldcatalog2 TO it_fieldcatalog2.




ENDFORM.

FORM fill_it_fieldcatalog3 USING VALUE(p_colnum)
                                VALUE(p_fieldname)
                                VALUE(p_tabname)
                                VALUE(p_len)
                                VALUE(p_edit)
                                VALUE(p_icon)
                                VALUE(p_do_sum)
                                VALUE(p_header)
                                VALUE(p_emphasize)
                                VALUE(p_hotspot)
                                VALUE(p_ref_table)
                                VALUE(p_ref_field).




  DATA:  wa_fieldcatalog3  TYPE lvc_s_fcat.

  wa_fieldcatalog3-col_pos     = p_colnum.
  wa_fieldcatalog3-fieldname   = p_fieldname.
  wa_fieldcatalog3-tabname     = p_tabname.
  wa_fieldcatalog3-outputlen   = p_len.
  wa_fieldcatalog3-edit        = p_edit.
  wa_fieldcatalog3-icon        = p_icon.
  wa_fieldcatalog3-do_sum      = p_do_sum.
  wa_fieldcatalog3-coltext     = p_header.
  wa_fieldcatalog3-emphasize   = p_emphasize.
  wa_fieldcatalog3-hotspot     = p_hotspot.
  wa_fieldcatalog3-ref_table   = p_ref_table.
  wa_fieldcatalog3-ref_table   = p_ref_field.


  gs_layout3-excp_conds    = 'X'.
  gs_layout3-zebra         = 'X'.
  gs_layout3-sel_mode      = 'A'.
  "GS_LAYOUT3-CWIDTH_OPT    = 'X'.     "  Otimizar colunas na tela
  gs_layout3-totals_bef    = ' '.

  APPEND wa_fieldcatalog3 TO it_fieldcatalog3.




ENDFORM.


*  Busca a logo Marca e adiciona no cabeçario.
FORM f_pega_imagem  USING    nome_logo
                  CHANGING url.

  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.

  DATA: l_graphic_xstr TYPE xstring.
  DATA: graphic_size   TYPE i.
  DATA: l_graphic_conv TYPE i.
  DATA: l_graphic_offs TYPE i.

  REFRESH graphic_table.

  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.

  WHILE l_graphic_conv > 255.

    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.

  ENDWHILE.

  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.

ENDFORM.                    " F_PEGA_IMAGEM

FORM select_dados  USING e_row
                         e_column.


  IF rb_suc IS NOT INITIAL.
    IF g_ts_0100-subscreen EQ '0100'.
      TRY .
          DATA(w_ordem) = gt_saida[ e_row ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      CASE e_column.

        WHEN 'AUFNR'.
          SET PARAMETER ID 'ANR' FIELD w_ordem-aufnr.
          CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN . " só visualizar

        WHEN 'EQUNR'.
          SET PARAMETER ID 'EQN' FIELD w_ordem-aufnr.
          CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN . " só visualizar

      ENDCASE.
    ELSE.

      TRY .
          DATA(w_nota) = gs_nota[ e_row ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      CASE e_column.

        WHEN 'AUFNR'.
          SET PARAMETER ID 'ANR' FIELD w_nota-aufnr.
          CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN . " só visualizar

        WHEN 'QMNUM'.
          SET PARAMETER ID 'IQM' FIELD w_nota-qmnum.
          CALL TRANSACTION 'IW23' AND SKIP FIRST SCREEN . " só visualizar

        WHEN 'EQUNR'.
          SET PARAMETER ID 'EQN' FIELD w_nota-equnr.
          CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN . " só visualizar

      ENDCASE.

    ENDIF.

  ELSEIF rb_err IS NOT INITIAL AND rb_ord IS NOT INITIAL.

    IF g_ts_0100-subscreen EQ '0100'.
      TRY .
          DATA(wa_saida) = gt_saida[ e_row ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      FREE it_exib_erros.

      CLEAR: it_exib_erros.
      it_exib_erros = gt_saida.


      CASE e_column.

        WHEN 'ICONE'.
*      READ TABLE it_exib_erros INTO DATA(wa_select) WITH KEY bloco = wa_saida-bloco
*                                                         idord = wa_saida-idord.

          CHECK sy-subrc = 0.

          CLEAR:it_msg_log.
          SELECT *
           FROM zpmt0018
           INTO CORRESPONDING FIELDS OF TABLE it_msg_log
           WHERE bloco = wa_saida-bloco
           AND   idord =  wa_saida-idord
           AND   inativo = ' '.

          CLEAR: wa_saida.
          IF it_msg_log IS NOT INITIAL.
            CALL SCREEN 0200.
          ENDIF.

      ENDCASE.

    ELSE.
      CLEAR:wa_saida.
      TRY .
          wa_saida = gs_saida[ e_row ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      FREE it_exib_erros.

      CLEAR: it_exib_erros.
      it_exib_erros = gs_saida.


      CASE e_column.

        WHEN 'ICONE'.

          CHECK sy-subrc = 0.

          CLEAR:it_msg_log.
          SELECT *
           FROM zpmt0018
           INTO CORRESPONDING FIELDS OF TABLE it_msg_log
           WHERE bloco = wa_saida-bloco
           AND   idord =  wa_saida-idord
           AND   inativo = ' '.

          CLEAR: wa_saida.
          IF it_msg_log IS NOT INITIAL.
            CALL SCREEN 0200.
          ENDIF.
      ENDCASE.
    ENDIF.

  ELSEIF rb_err IS NOT INITIAL AND rb_not IS NOT INITIAL.

    IF g_ts_0100-subscreen EQ '0100'.
      CLEAR: w_nota.
      TRY .
          w_nota = gw_nota[ e_row ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      FREE it_exib_erros.

      CASE e_column.

        WHEN 'AUFNR'.
          SET PARAMETER ID 'ANR' FIELD w_nota-aufnr.
          CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN . " só visualizar

        WHEN 'QMNUM'.
          SET PARAMETER ID 'IQM' FIELD w_nota-qmnum.
          CALL TRANSACTION 'IW23' AND SKIP FIRST SCREEN . " só visualizar

        WHEN 'EQUNR'.
          SET PARAMETER ID 'EQN' FIELD w_nota-equnr.
          CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN . " só visualizar


        WHEN 'ICONE'.

          CHECK sy-subrc = 0.

          CLEAR:it_msg_log.
          SELECT *
           FROM zpmt0018
           INTO CORRESPONDING FIELDS OF TABLE it_msg_log
           WHERE idnot =  w_nota-idnot
           AND   inativo = ' '.

          IF it_msg_log IS NOT INITIAL.
            CALL SCREEN 0200.
          ENDIF.

      ENDCASE.

    ELSE.
      CLEAR:w_nota.
      TRY .
          w_nota = gt_nota[ e_row ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      FREE it_exib_erros.

      CASE e_column.

        WHEN 'ICONE'.

          CHECK sy-subrc = 0.

          CLEAR:it_msg_log.
          SELECT *
           FROM zpmt0018
           INTO CORRESPONDING FIELDS OF TABLE it_msg_log
           WHERE idnot =  w_nota-idnot
           AND   inativo = ' '.

          CLEAR: w_nota.
          IF it_msg_log IS NOT INITIAL.
            CALL SCREEN 0200.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDIF.

ENDFORM.

FORM editar_operacoes.

  READ TABLE it_msg_log INTO DATA(wa_select_operacao) INDEX 1.
  CHECK sy-subrc = 0.

  SELECT SINGLE *
    FROM  zpmt0016
  INTO w_zpmt0016
    WHERE bloco = wa_select_operacao-bloco
    AND idord = wa_select_operacao-idord.

  SELECT *
   FROM zpmt0017
   INTO CORRESPONDING FIELDS OF TABLE it_saida3
   WHERE bloco = wa_select_operacao-bloco
   AND  idord =  wa_select_operacao-idord.


*   Selecionando informações cabeçalho da nota.
  FREE: w_nota.
  SELECT SINGLE *
  FROM zpmt0014
  INTO w_nota
    WHERE idnot EQ wa_select_operacao-idnot.

*   Selecionando informações item da nota.
  FREE t_nota.
  SELECT *
  FROM zpmt0019
  INTO TABLE t_nota
  WHERE idnot EQ wa_select_operacao-idnot.


  IF w_zpmt0016-statp IS NOT INITIAL AND w_zpmt0016-statp NE 'P'.
    CALL SCREEN 0300.
    EXIT.

  ELSEIF w_nota-statp IS NOT INITIAL AND w_nota-statp NE 'P'.
    IF t_nota IS NOT INITIAL.
      LOOP AT t_nota ASSIGNING FIELD-SYMBOL(<_nota>).
        IF <_nota>-otgrp IS NOT INITIAL.
          SELECT SINGLE *
             FROM qpct
             INTO @DATA(_qpctq)
             WHERE katalogart EQ @<_nota>-otkat
               AND codegruppe EQ @<_nota>-otgrp
               AND code       EQ @<_nota>-oteil
               AND sprache EQ @sy-langu.
          <_nota>-kurztext = _qpctq-kurztext.
          CLEAR:_qpctq.
        ENDIF.


        IF <_nota>-fegrp IS NOT INITIAL.
          CLEAR:_qpctq.
          SELECT SINGLE *
           FROM qpct
           INTO _qpctq
           WHERE katalogart EQ <_nota>-fekat
             AND codegruppe EQ <_nota>-fegrp
             AND code       EQ <_nota>-fecod
             AND sprache EQ sy-langu.
          <_nota>-kurztext_2 = _qpctq-kurztext.
        ENDIF.

        IF <_nota>-urgrp IS NOT INITIAL.
          CLEAR:_qpctq.
          SELECT SINGLE *
          FROM qpct
          INTO _qpctq
          WHERE katalogart EQ <_nota>-urkat
            AND codegruppe EQ <_nota>-urgrp
            AND code       EQ <_nota>-urcod
            AND sprache EQ sy-langu.
          <_nota>-kurztext_3 = _qpctq-kurztext.

*        ELSEIF <_NOTA>-URGRP IS NOT INITIAL.
*          <_NOTA>-KURZTEXT_3 = _QPCTQ-KURZTEXT.
*
        ENDIF.
      ENDLOOP.
    ENDIF.
    CALL SCREEN 0500.
    EXIT.
  ENDIF.

ENDFORM.

FORM salvar_alteracoes .
  DATA: w_alteracoes TYPE zpmt0017.

  w_zpmt0016-statp = ' '.

  MODIFY zpmt0016 FROM w_zpmt0016.

  LOOP AT it_saida3 INTO DATA(wa_alteracoes).
    MOVE-CORRESPONDING wa_alteracoes TO w_alteracoes.

    MODIFY zpmt0017 FROM w_alteracoes.

  ENDLOOP.

  FREE  it_msg_log.

  SELECT *
       FROM zpmt0018
       INTO CORRESPONDING FIELDS OF TABLE it_msg_log
       WHERE bloco EQ w_zpmt0016-bloco
       AND   idord EQ  w_zpmt0016-idord
       AND   inativo NE abap_true.

  LOOP  AT   it_msg_log INTO DATA(wa_msg_log).

    wa_msg_log-inativo = 'X'.

    MODIFY zpmt0018 FROM wa_msg_log.

  ENDLOOP.
  COMMIT WORK.
ENDFORM.

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.

MODULE status_0100 OUTPUT.
  DATA:
 lst_layout TYPE lvc_s_layo.

  DATA: url(255)                TYPE c,
        p_text                  TYPE sdydo_text_element,
        p_text_2                TYPE sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table,
        p_text_table_2          TYPE sdydo_text_table,
        vl_cont                 TYPE i,
        vl_butxt                TYPE t001-butxt,
        vl_dates1               TYPE char10,
        vl_dates2               TYPE char10.

*  SET PF-STATUS 'ST0100'.
*  SET TITLEBAR 'TITULO100'.

*Se radio button sucesso ou radio button erro e radio button ordem estiver marcado.
  IF rb_suc IS NOT INITIAL OR rb_err IS NOT INITIAL AND rb_ord IS NOT INITIAL.
* Adicionando Logo Marca no Cabeçalho
    IF obj_custom_0110 IS INITIAL.


      PERFORM fill_it_fieldcatalog USING:

     1 'AUFNR'    'T_SAIDA' '25'  ' '  ' '  ' '    'N° Ordem'             ' ' 'X' 'CHAR' ' ' ' ' ' ',
     2 'EQUNR'    'T_SAIDA' '30'  ' '  ' '  ' '    'Equipamento'          ' ' ' ' 'CHAR' ' ' 'X' 'X',
     3 'SHTXT'    'T_SAIDA' '50'  ' '  ' '  ' '    'Desc. Equipamento'    ' ' ' ' 'CHAR' ' ' ' ' 'X',
     4 'PLTXT'    'T_SAIDA' '100'  ' '  ' '  ' '    'Descrição do local'   ' ' ' ' 'CHAR' ' ' ' ' 'X',
     5 'KTEXT'    'T_SAIDA' '100'  ' '  ' '  ' '    'Descrição'            ' ' ' ' 'CHAR' ' ' ' ' 'X',
     6 'STATP'    'T_SAIDA' '10'  ' '  ' '  ' '    'Status'               ' ' ' ' 'CHAR' ' ' ' ' 'X',
     7 'BLOCO'    'T_SAIDA' '30'  ' '  ' '  ' '    'Bloco'                ' ' ' ' 'CHAR' ' ' ' ' ' ',
     8 'ICONE'    'T_SAIDA' '30'  ' '  ' '  ' '    'Editar'               ' ' ' ' 'CHAR' ' ' ' ' ' '.

* Adicionando informação do parametro de entrada no cabeçalho.
      "PERFORM FILL_GS_VARIANT.

      CREATE OBJECT obj_custom_0110
        EXPORTING
          container_name              = 'CONTAINER'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.

      gs_layout-sel_mode   = 'A'.
      "GS_LAYOUT-CWIDTH_OPT = 'X'.
      CLEAR: it_exclude_fcode, it_exclude_fcode[].

      CREATE OBJECT obj_alv_0110
        EXPORTING
          i_parent = obj_custom_0110.

      CALL METHOD obj_alv_0110->set_table_for_first_display
        EXPORTING
          is_layout            = gs_layout
          is_variant           = gs_variant
          it_toolbar_excluding = it_exclude_fcode
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = it_fieldcatalog
          it_outtab            = gt_saida
          it_sort              = it_sort.

      SET HANDLER: lcl_eventos=>on_hotspot_click FOR obj_alv_0110.

      SET HANDLER: lcl_event_handler=>on_double_click FOR obj_alv_0110.

    ENDIF.

    CALL METHOD obj_alv_0110->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

*  Se radio button erro e radio button nota estiver marcado.
  ELSEIF rb_err IS NOT INITIAL AND rb_not IS NOT INITIAL.

    IF obj_custom_0130 IS INITIAL.


      PERFORM fill_it_fieldcatalog USING:

     1 'AUFNR'    'GS_NOTA' '25'  ' '  ' '  ' '    'N° Ordem'             ' ' ' ' 'CHAR' ' ' 'X' 'X',
     2 'QMNUM'    'GS_NOTA' '25'  ' '  ' '  ' '    'N° Nota '             ' ' ' ' 'CHAR' ' ' 'X' 'X',
     3 'QMTXT'    'GS_NOTA' '25'  ' '  ' '  ' '    'Texto breve'          ' ' ' ' 'CHAR' ' ' 'X' 'X',
     4 'EQUNR'    'GS_NOTA' '30'  ' '  ' '  ' '    'Equipamento'          ' ' ' ' 'CHAR' ' ' 'X' 'X',
     5 'EQKTX'    'GS_NOTA' '50'  ' '  ' '  ' '    'Desc. Equipamento'    ' ' ' ' 'CHAR' ' ' ' ' 'X',
     6 'PLTXT'    'GS_NOTA' '100' ' '  ' '  ' '    'Descrição do local'   ' ' ' ' 'CHAR' ' ' ' ' 'X',
     7 'STATP'    'GS_NOTA' '10'  ' '  ' '  ' '    'Status'               ' ' ' ' 'CHAR' ' ' ' ' 'X'.

      IF rb_suc IS INITIAL.
        PERFORM fill_it_fieldcatalog USING:
        8 'ICONE'    'GS_NOTA' '30'  ' '  ' '  ' '    'Editar'               ' ' ' ' 'CHAR' ' ' ' ' ' '.
      ENDIF.

* Adicionando informação do parametro de entrada no cabeçalho.
      "PERFORM FILL_GS_VARIANT.

      CREATE OBJECT obj_custom_0130
        EXPORTING
          container_name              = 'CONTAINER'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.

      gs_layout-sel_mode   = 'A'.
      "GS_LAYOUT-CWIDTH_OPT = 'X'.
      CLEAR: it_exclude_fcode, it_exclude_fcode[].

      CREATE OBJECT obj_alv_0130
        EXPORTING
          i_parent = obj_custom_0130.

      CALL METHOD obj_alv_0130->set_table_for_first_display
        EXPORTING
          is_layout            = gs_layout
*         IS_VARIANT           = GS_VARIANT
          it_toolbar_excluding = it_exclude_fcode_0120
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = it_fieldcatalog
          it_outtab            = gw_nota.
*        IT_SORT              = IT_SORT.

      SET HANDLER: lcl_eventos=>on_hotspot_click FOR obj_alv_0130.

      SET HANDLER: lcl_event_handler=>on_double_click FOR obj_alv_0130.

    ENDIF.

    CALL METHOD obj_alv_0130->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.
  ENDIF.
ENDMODULE.

MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'EDITAR'.
      PERFORM editar_operacoes.
      IF voltando = 'X'.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.
ENDMODULE.

MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST0200'.
  SET TITLEBAR 'TT0200'.


  IF rb_err IS NOT INITIAL AND rb_ord IS NOT INITIAL.
    IF g_custom_container2 IS INITIAL.

      CLEAR it_fieldcatalog2.
      PERFORM fill_it_fieldcatalog2 USING:


      1 'IDORD'    'T_SAIDA' '25'  ' '  ' '  ' '    'ID ORDEM'            ' ' ' ' 'CHAR' ' ' ' ' ' ',
      "2 'BLOCO'    'T_SAIDA' '25'  ' '  ' '  ' '    'BLOCO'               ' ' ' ' 'CHAR' ' ' ' ' ' ',
      3 'TYPE'     'T_SAIDA' '25'  ' '  ' '  ' '    'TIPO DO ERRO'        ' ' ' ' 'CHAR' ' ' ' ' ' ',
      4 'MESSAGE'  'T_SAIDA' '60'  ' '  ' '  ' '    'MENSAGEM'            ' ' ' ' 'CHAR' ' ' ' ' 'X'.


      CREATE OBJECT g_custom_container2
        EXPORTING
          container_name              = 'CONTAINER2'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.


      gs_layout2-sel_mode   = 'A'.
      gs_layout2-cwidth_opt = 'X'.
      CLEAR: it_exclude_fcode2, it_exclude_fcode2[].

      CREATE OBJECT ctl_alv2
        EXPORTING
          "I_PARENT = DG_PARENT_ALV2.
          i_parent = g_custom_container2.

      CALL METHOD ctl_alv2->set_table_for_first_display
        EXPORTING
          is_layout            = gs_layout2
          is_variant           = gs_variant2
          it_toolbar_excluding = it_exclude_fcode2
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = it_fieldcatalog2
          it_outtab            = it_msg_log.


    ENDIF.

    CALL METHOD ctl_alv2->refresh_table_display
      EXPORTING
        is_stable = ls_stable2
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

  ELSE.

    IF g_custom_container2 IS INITIAL.

      CLEAR it_fieldcatalog2.
      PERFORM fill_it_fieldcatalog2 USING:

      1 'IDNOT'     'T_SAIDA' '25'  ' '  ' '  ' '    'ID NOTA'            ' ' ' ' 'CHAR' ' ' ' ' ' ',
      "2 'BLOCO'    'T_SAIDA' '25'  ' '  ' '  ' '    'BLOCO'               ' ' ' ' 'CHAR' ' ' ' ' ' ',
      3 'TYPE'      'T_SAIDA' '25'  ' '  ' '  ' '    'TIPO DO ERRO'        ' ' ' ' 'CHAR' ' ' ' ' ' ',
      4 'MESSAGE'   'T_SAIDA' '60'  ' '  ' '  ' '    'MENSAGEM'            ' ' ' ' 'CHAR' ' ' ' ' 'X'.


      CREATE OBJECT obj_custom_0150
        EXPORTING
          container_name              = 'CONTAINER2'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.


      gs_layout2-sel_mode   = 'A'.
      gs_layout2-cwidth_opt = 'X'.
      CLEAR: it_exclude_fcode2, it_exclude_fcode2[].

      CREATE OBJECT obj_alv_0150
        EXPORTING
          "I_PARENT = DG_PARENT_ALV2.
          i_parent = obj_custom_0150.

      CALL METHOD obj_alv_0150->set_table_for_first_display
        EXPORTING
          is_layout            = gs_layout2
          is_variant           = gs_variant2
          it_toolbar_excluding = it_exclude_fcode2
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = it_fieldcatalog2
          it_outtab            = it_msg_log.

    ENDIF.

    CALL METHOD obj_alv_0150->refresh_table_display
      EXPORTING
        is_stable = ls_stable2
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.


  ENDIF.



ENDMODULE.

MODULE user_command_0300 INPUT.

  CASE  sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM salvar_alteracoes.
      PERFORM chamar_job.

      FREE gt_saida.
      PERFORM buscar_dados.

      DELETE gt_saida WHERE bloco = w_zpmt0016-bloco.

      PERFORM buscar_dados.

      CALL SCREEN 0400.

      CALL METHOD obj_alv_0110->refresh_table_display
        EXPORTING
          is_stable = ls_stable
        EXCEPTIONS
          finished  = 1
          OTHERS    = 2.

      CALL METHOD obj_alv_0120->refresh_table_display
        EXPORTING
          is_stable = ls_stable
        EXCEPTIONS
          finished  = 1
          OTHERS    = 2.
*      FREE IT_MSG_LOG.
*      SELECT *
*             FROM ZPMT0018
*             INTO CORRESPONDING FIELDS OF TABLE IT_MSG_LOG
*             WHERE BLOCO = W_ZPMT0016-BLOCO
*             AND   IDORD =  W_ZPMT0016-IDORD
*             AND   INATIVO = ' '.
*
*      CALL METHOD CTL_ALV2->REFRESH_TABLE_DISPLAY
*        EXPORTING
*          IS_STABLE = LS_STABLE2
*        EXCEPTIONS
*          FINISHED  = 1
*          OTHERS    = 2.
      voltando = 'X'.
      LEAVE TO SCREEN 0.

*      LEAVE TO CURRENT TRANSACTION.

  ENDCASE.
ENDMODULE.

MODULE status_0300 OUTPUT.


  SET PF-STATUS 'STT0300'.
  SET TITLEBAR 'TT0300'.


  IF g_custom_container3 IS INITIAL.
    PERFORM fill_it_fieldcatalog3 USING:

    1 'VORNR'         'IT_SAIDA3' '25'  ' '  ' '  ' '    'N° OPERAÇÃO             ' '  ' ' ' 'CHAR' ' '  ,
    2 'ARBPL'         'IT_SAIDA3' '25'  'X'  ' '  ' '    'CENTRO DE TRABALHO      ' '  ' ' ' 'CHAR' ' '  ,
    3 'DESCRIPTION'   'IT_SAIDA3' '25'  'X'  ' '  ' '    'TXT BREVE               ' '  ' ' ' 'CHAR' ' '  ,
    4 'STEUS'         'IT_SAIDA3' '25'  'X'  ' '  ' '    'CHAVE DE CONTROLE       ' '  ' ' ' 'CHAR' ' '  ,
    "5 'BLOCO'         'IT_SAIDA3' '25'  ' '  ' '  ' '    'BLOCO'                 ' ' ' ' 'CHAR' ' '  ,
    "5 'IDORD'         'IT_SAIDA3' '25'  ' '  ' '  ' '    'ID ORDEM'              ' ' ' ' 'CHAR' ' '  ,
    5 'PERNR'         'IT_SAIDA3' '25'  'X'  ' '  ' '    'N° PESSOAL              ' '  ' ' ' 'CHAR' ' '   ,
    6 'NTANF'         'IT_SAIDA3' '25'  'X'  ' '  ' '    'INICIO OBRIGATÓRIO EM   ' ' ' ' ' 'CHAR' ' '  ,
    7 'ARBEI'         'IT_SAIDA3' '25'  'X'  ' '  ' '    'HORAS TOTAL OPERAÇÃO    ' ' ' ' ' 'CHAR' ' '   ,
    8 'ARBEH'         'IT_SAIDA3' '25'  ' '  ' '  ' '    'UND                     ' ' ' ' ' 'CHAR' ' '  ,
    9 'ANZZL'         'IT_SAIDA3' '25'  'X'  ' '  ' '    'QUANT. DE PESSOAS       ' ' ' ' ' 'CHAR' ' '   ,
   10 'DAUNO'         'IT_SAIDA3' '25'  ' '  ' '  ' '    'HORAS P/ EMPREGADO      ' ' ' ' ' 'CHAR' ' '  ,
   11 'DAUNE'         'IT_SAIDA3' '25'  ' '  ' '  ' '    'UND'                    ' ' ' ' 'CHAR' ' '   .


    CREATE OBJECT g_custom_container3
      EXPORTING
        container_name              = 'CONTAINER3'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.


    gs_layout3-sel_mode   = 'A'.
    gs_layout3-cwidth_opt = 'X'.
    CLEAR: it_exclude_fcode3, it_exclude_fcode3[].

    CREATE OBJECT ctl_alv3
      EXPORTING
        i_parent = g_custom_container3.

    CALL METHOD ctl_alv3->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout3
*       IS_VARIANT           = GS_VARIANT3
        it_toolbar_excluding = it_exclude_fcode3
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog3
        it_outtab            = it_saida3.

    CALL METHOD ctl_alv3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD ctl_alv3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler=>on_data_changed_finished FOR ctl_alv3,
                 lcl_event_handler=>on_data_changed FOR ctl_alv3.

    CALL METHOD ctl_alv3->refresh_table_display
      EXPORTING
        is_stable = ls_stable3
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

  ENDIF.

  ls_stable3-row = 'X'.
  ls_stable3-col = 'X'.

  CALL METHOD ctl_alv3->refresh_table_display
    EXPORTING
      is_stable = ls_stable3
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

ENDMODULE.

FORM cabecario .

*  IF P_IWERK IS NOT INITIAL.
*    LOOP AT P_EQUNR.
*      IF P_EQUNR-OPTION NE 'EQ' AND P_EQUNR-OPTION NE 'BT'.
*        SDYDO_TEXT_ELEMENT = 'Equipamento: Multiplas Seleções'.
*        EXIT.
*      ELSEIF P_EQUNR-OPTION EQ 'BT'.
*        CONCATENATE 'Equipamento:' P_EQUNR-LOW '-' P_EQUNR-HIGH INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
*        EXIT.
*      ELSE.
*        VL_CONT = VL_CONT + 1.
*        IF VL_CONT GT 1.
*          SDYDO_TEXT_ELEMENT = 'Equipamento: Multiplas Seleções'.
*        ELSE.
*          CONCATENATE 'Equipamento:' P_EQUNR-LOW INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
*    CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.
*  ELSE.
*    IF P_EQUNR IS NOT INITIAL.
*      SDYDO_TEXT_ELEMENT = 'Equipamento:'.
*      APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
*    ENDIF.
*  ENDIF.
*  CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.
ENDFORM.

FORM chamar_job .
  DATA(obj_create) = NEW zcl_pm_ordem( ).
  CALL METHOD obj_create->call_report
    EXPORTING
      i_sequen = CONV #( |{ w_zpmt0016-bloco ALPHA = OUT }| )
      i_report = 'ZPMR0045'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0400 OUTPUT.
  SET PF-STATUS 'ST04'.
  SET TITLEBAR 'TIT04'.

*  IF SY-UCOMM EQ 'ENTER'.
*  CASE W_CURSOR_FIELD.
*    WHEN 'TBX_CENTRO'.
*      SET CURSOR FIELD 'TBX_EQUIPAMENTO'.
*  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0400 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.


    WHEN 'EXIT'.
      LEAVE PROGRAM.

    WHEN 'ATUALIZAR'.
      PERFORM buscar_dados.
      PERFORM at_alv.


    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TS_SET_ACTIVE_TAB  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ts_set_active_tab OUTPUT.

  CONDENSE: g_tabstrip-qtd1,
            g_tabstrip-qtd2,
            g_tabstrip-qtd3 NO-GAPS.

  itabstrip-activetab = g_ts_0100-pressed_tab.



  CASE g_ts_0100-pressed_tab.
    WHEN 'TAB_DISPONIVEIS'.
      g_ts_0100-subscreen = '0100'.
    WHEN 'TAB_EMPRESTADOS'.
      g_ts_0100-subscreen = '0120'.
    WHEN OTHERS.
  ENDCASE.

*  IF TBX_CENTRO IS NOT INITIAL.
*  PERFORM MODIF_SCREEN_0100_.
*  ENDIF.

  IF rb_err IS NOT INITIAL AND rb_ord IS NOT INITIAL..
    CONCATENATE '@9Z@ Erro na modificação da ordem' '(' g_tabstrip-qtd1 ')'
    INTO g_tabstrip-tab1 SEPARATED BY space.

    CONCATENATE '@AR@ Erro na criação da ordem' '(' g_tabstrip-qtd2 ')'
    INTO g_tabstrip-tab2 SEPARATED BY space.

  ELSEIF rb_suc IS NOT  INITIAL.

    CONCATENATE '@9Z@ Ordem(s) processadas' '(' g_tabstrip-qtd1 ')'
    INTO g_tabstrip-tab1 SEPARATED BY space.

    CONCATENATE '@0L@ Nota(s) Porcessadas' '(' g_tabstrip-qtd2 ')'
    INTO g_tabstrip-tab2 SEPARATED BY space.

  ELSEIF rb_err IS NOT INITIAL AND rb_not IS NOT  INITIAL.
    CONCATENATE '@0J@ Erro na modificação da nota' '(' g_tabstrip-qtd1 ')'
    INTO g_tabstrip-tab1 SEPARATED BY space.

    CONCATENATE '@0K@ Erro na criação da nota' '(' g_tabstrip-qtd2 ')'
    INTO g_tabstrip-tab2 SEPARATED BY space.
  ENDIF.



ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TS_GET_ACTIVE_TAB  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ts_get_active_tab INPUT.
  CASE sy-ucomm.
    WHEN 'TAB_DISPONIVEIS'. g_ts_0100-pressed_tab = 'TAB_DISPONIVEIS'.
    WHEN 'TAB_EMPRESTADOS'. g_ts_0100-pressed_tab = 'TAB_EMPRESTADOS'.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0120  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0120 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  IF rb_err IS NOT INITIAL AND rb_ord IS NOT INITIAL.

    IF obj_custom_0120 IS INITIAL.


      PERFORM fill_it_fieldcatalog_0120 USING:

     1 'AUFNR'    'T_SAIDA' '25'  ' '  ' '  ' '    'N° Ordem'             ' ' 'X' 'CHAR' ' ' ' ' ' ',
     2 'EQUNR'    'T_SAIDA' '30'  ' '  ' '  ' '    'Equipamento'          ' ' ' ' 'CHAR' ' ' 'X' 'X',
     3 'SHTXT'    'T_SAIDA' '50'  ' '  ' '  ' '    'Desc. Equipamento'    ' ' ' ' 'CHAR' ' ' ' ' 'X',
     4 'PLTXT'    'T_SAIDA' '100'  ' '  ' '  ' '    'Descrição do local'   ' ' ' ' 'CHAR' ' ' ' ' 'X',
     5 'KTEXT'    'T_SAIDA' '100'  ' '  ' '  ' '    'Descrição'            ' ' ' ' 'CHAR' ' ' ' ' 'X',
     6 'STATP'    'T_SAIDA' '10'  ' '  ' '  ' '    'Status'               ' ' ' ' 'CHAR' ' ' ' ' 'X',
     7 'BLOCO'    'T_SAIDA' '30'  ' '  ' '  ' '    'Bloco'                ' ' ' ' 'CHAR' ' ' ' ' ' ',
     8 'ICONE'    'T_SAIDA' '30'  ' '  ' '  ' '    'Editar'               ' ' ' ' 'CHAR' ' ' ' ' ' '.

* Adicionando informação do parametro de entrada no cabeçalho.
      "PERFORM FILL_GS_VARIANT.

      CREATE OBJECT obj_custom_0120
        EXPORTING
          container_name              = 'CONTAINER_0120'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.

      gs_layout-sel_mode   = 'A'.
      "GS_LAYOUT-CWIDTH_OPT = 'X'.
      CLEAR: it_exclude_fcode, it_exclude_fcode[].

      CREATE OBJECT obj_alv_0120
        EXPORTING
          i_parent = obj_custom_0120.

      CALL METHOD obj_alv_0120->set_table_for_first_display
        EXPORTING
          is_layout            = gs_layout_0120
*         IS_VARIANT           = GS_VARIANT
          it_toolbar_excluding = it_exclude_fcode_0120
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = it_fieldcatalog_0120
          it_outtab            = gs_saida.
*        IT_SORT              = IT_SORT.

      SET HANDLER: lcl_eventos=>on_hotspot_click FOR obj_alv_0120.

      SET HANDLER: lcl_event_handler=>on_double_click FOR obj_alv_0120.

    ENDIF.

    CALL METHOD obj_alv_0120->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

  ELSEIF rb_suc IS NOT INITIAL.

    IF obj_custom_0130 IS INITIAL.


      PERFORM fill_it_fieldcatalog_0120 USING:

     2 'AUFNR'    'GS_NOTA' '25'  ' '  ' '  ' '    'N° Ordem'             ' ' ' ' 'CHAR' ' ' 'X' 'X',
     1 'QMNUM'    'GS_NOTA' '25'  ' '  ' '  ' '    'N° Nota '             ' ' ' ' 'CHAR' ' ' 'X' 'X',
     3 'QMTXT'    'GS_NOTA' '25'  ' '  ' '  ' '    'Texto breve'          ' ' ' ' 'CHAR' ' ' 'X' 'X',
     4 'EQUNR'    'GS_NOTA' '30'  ' '  ' '  ' '    'Equipamento'          ' ' ' ' 'CHAR' ' ' 'X' 'X',
     5 'EQKTX'    'GS_NOTA' '50'  ' '  ' '  ' '    'Desc. Equipamento'    ' ' ' ' 'CHAR' ' ' ' ' 'X',
     6 'PLTXT'    'GS_NOTA' '100' ' '  ' '  ' '    'Descrição do local'   ' ' ' ' 'CHAR' ' ' ' ' 'X',
     7 'STATP'    'GS_NOTA' '10'  ' '  ' '  ' '    'Status'               ' ' ' ' 'CHAR' ' ' ' ' 'X'.

      IF rb_suc IS INITIAL.
        PERFORM fill_it_fieldcatalog_0120 USING:
        8 'ICONE'    'GS_NOTA' '30'  ' '  ' '  ' '    'Editar'               ' ' ' ' 'CHAR' ' ' ' ' ' '.
      ENDIF.

* Adicionando informação do parametro de entrada no cabeçalho.
      "PERFORM FILL_GS_VARIANT.

      CREATE OBJECT obj_custom_0130
        EXPORTING
          container_name              = 'CONTAINER_0120'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.

      gs_layout-sel_mode   = 'A'.
      "GS_LAYOUT-CWIDTH_OPT = 'X'.
      CLEAR: it_exclude_fcode, it_exclude_fcode[].

      CREATE OBJECT obj_alv_0130
        EXPORTING
          i_parent = obj_custom_0130.

      CALL METHOD obj_alv_0130->set_table_for_first_display
        EXPORTING
          is_layout            = gs_layout_0120
*         IS_VARIANT           = GS_VARIANT
          it_toolbar_excluding = it_exclude_fcode_0120
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = it_fieldcatalog_0120
          it_outtab            = gs_nota.
*        IT_SORT              = IT_SORT.

      SET HANDLER: lcl_eventos=>on_hotspot_click FOR obj_alv_0130.

      SET HANDLER: lcl_event_handler=>on_double_click FOR obj_alv_0130.

    ENDIF.

    CALL METHOD obj_alv_0130->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

  ELSEIF rb_err IS NOT INITIAL AND rb_not IS NOT INITIAL.

    IF obj_custom_0140 IS INITIAL.


      PERFORM fill_it_fieldcatalog_0120 USING:

     1 'IDNOT'    'GS_NOTA' '25'  ' '  ' '  ' '    'IDNOTA'               ' ' ' ' 'CHAR' ' ' 'X' 'X',
     3 'AUFNR'    'GS_NOTA' '25'  ' '  ' '  ' '    'N° Ordem'             ' ' ' ' 'CHAR' ' ' 'X' 'X',
     2 'QMNUM'    'GS_NOTA' '25'  ' '  ' '  ' '    'N° Nota '             ' ' ' ' 'CHAR' ' ' 'X' 'X',
     4 'QMTXT'    'GS_NOTA' '25'  ' '  ' '  ' '    'Texto breve'          ' ' ' ' 'CHAR' ' ' 'X' 'X',
     5 'EQUNR'    'GS_NOTA' '30'  ' '  ' '  ' '    'Equipamento'          ' ' ' ' 'CHAR' ' ' 'X' 'X',
     6 'EQKTX'    'GS_NOTA' '50'  ' '  ' '  ' '    'Desc. Equipamento'    ' ' ' ' 'CHAR' ' ' ' ' 'X',
     7 'PLTXT'    'GS_NOTA' '100' ' '  ' '  ' '    'Descrição do local'   ' ' ' ' 'CHAR' ' ' ' ' 'X',
     8 'STATP'    'GS_NOTA' '10'  ' '  ' '  ' '    'Status'               ' ' ' ' 'CHAR' ' ' ' ' 'X'.

      IF rb_suc IS INITIAL.
        PERFORM fill_it_fieldcatalog_0120 USING:
        9 'ICONE'    'GS_NOTA' '30'  ' '  ' '  ' '    'Editar'               ' ' ' ' 'CHAR' ' ' ' ' ' '.
      ENDIF.

* Adicionando informação do parametro de entrada no cabeçalho.
      "PERFORM FILL_GS_VARIANT.

      CREATE OBJECT obj_custom_0140
        EXPORTING
          container_name              = 'CONTAINER_0120'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.

      gs_layout-sel_mode   = 'A'.
      "GS_LAYOUT-CWIDTH_OPT = 'X'.
      CLEAR: it_exclude_fcode, it_exclude_fcode[].

      CREATE OBJECT obj_alv_0140
        EXPORTING
          i_parent = obj_custom_0140.

      CALL METHOD obj_alv_0140->set_table_for_first_display
        EXPORTING
          is_layout            = gs_layout_0120
*         IS_VARIANT           = GS_VARIANT
          it_toolbar_excluding = it_exclude_fcode_0120
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = it_fieldcatalog_0120
          it_outtab            = gt_nota.
*        IT_SORT              = IT_SORT.

      SET HANDLER: lcl_eventos=>on_hotspot_click FOR obj_alv_0140.

      SET HANDLER: lcl_event_handler=>on_double_click FOR obj_alv_0140.

    ENDIF.

    CALL METHOD obj_alv_0140->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  SEL_ORDEM_NOVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sel_ordem_nova .

  CLEAR: w_saida.
  FREE: gs_saida, gw_saida.

  SELECT *
    FROM zpmt0016
    INTO CORRESPONDING FIELDS OF TABLE gw_saida
    WHERE aufnr EQ '' AND statp = 'E'
      AND iwerk IN p_iwerk
  ORDER BY dtreg DESCENDING hrreg DESCENDING.

*  SORT gt_nota DESCENDING BY bloco .
*  DELETE ADJACENT DUPLICATES FROM gw_saida COMPARING aufnr.

  IF gw_saida IS NOT INITIAL.

    LOOP AT gw_saida ASSIGNING FIELD-SYMBOL(<wa_saida_simplificada>).


      SELECT SINGLE eqktx FROM eqkt
            INTO  (<wa_saida_simplificada>-shtxt)
            WHERE equnr = <wa_saida_simplificada>-equnr
            AND   spras = 'PT'.

      SELECT SINGLE iflo~pltxt FROM v_equi AS equi
        INNER JOIN iloa AS iloa ON equi~tplnr = iloa~tplnr
        INNER JOIN iflo  AS iflo ON iflo~tplnr = iloa~tplnr
        INTO (<wa_saida_simplificada>-pltxt) WHERE equi~equnr = <wa_saida_simplificada>-equnr.

      SELECT SINGLE *
 FROM zpmt0018
 INTO CORRESPONDING FIELDS OF wa_zpmt0018
 WHERE bloco = <wa_saida_simplificada>-bloco
 AND  idord =  <wa_saida_simplificada>-idord
 AND type IN ( 'S', 'E', 'W' )
 AND inativo = ' '.

      w_saida-aufnr  = |{ <wa_saida_simplificada>-aufnr ALPHA = OUT }|.
      w_saida-bloco  = <wa_saida_simplificada>-bloco.
      w_saida-ktext  = <wa_saida_simplificada>-ktext.
      w_saida-dtini  = <wa_saida_simplificada>-dtini.
      w_saida-dtfim  = <wa_saida_simplificada>-dtfim.
      w_saida-hrini  = <wa_saida_simplificada>-hrini.
      w_saida-hrfim  = <wa_saida_simplificada>-hrfim.
      w_saida-statp  = <wa_saida_simplificada>-statp.
      w_saida-idord  = <wa_saida_simplificada>-idord.
      w_saida-dtreg  = <wa_saida_simplificada>-dtreg.
      w_saida-hrreg  = <wa_saida_simplificada>-hrreg.
      w_saida-equnr  = <wa_saida_simplificada>-equnr.
      w_saida-tplnr  = <wa_saida_simplificada>-tplnr.
      w_saida-idapn  = <wa_saida_simplificada>-idapn.
      w_saida-idopr  = <wa_saida_simplificada>-idopr.
      w_saida-shtxt = <wa_saida_simplificada>-shtxt.
      w_saida-pltxt = <wa_saida_simplificada>-pltxt.
      w_saida-bloco   = <wa_saida_simplificada>-bloco.
      w_saida-type    = wa_zpmt0018-type.

      IF wa_zpmt0018-type NE ' '.

        w_saida-icone   = '@0Z@'.

      ENDIF.

      IF  w_saida-type EQ 'S' OR <wa_saida_simplificada>-statp = 'P'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'SHTXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.

      ELSEIF  w_saida-type EQ 'E' OR <wa_saida_simplificada>-statp = 'E'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'SHTXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.

      ELSEIF  w_saida-type EQ 'W'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'SHTXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
      ENDIF.

      w_saida-cell_color[] = it_color.

      APPEND w_saida TO gs_saida.

      CLEAR: w_saida, wa_zpmt0018.

      FREE: it_color.

    ENDLOOP.

    DESCRIBE TABLE gs_saida LINES g_tabstrip-qtd2.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEL_ORDEM_MOF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sel_ordem_mof .

  CLEAR: w_saida.
  FREE: it_saida, gt_saida.
  SELECT *
  FROM zpmt0016 INTO CORRESPONDING FIELDS OF TABLE it_saida
  WHERE aufnr NE '' AND statp = 'E'
    AND iwerk IN p_iwerk
    ORDER BY
      dtreg DESCENDING hrreg DESCENDING.

  SORT it_saida DESCENDING BY aufnr bloco .
  DELETE ADJACENT DUPLICATES FROM it_saida COMPARING aufnr.


  IF it_saida IS NOT INITIAL.

    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<wa_saida_simplificada>).


      SELECT SINGLE eqktx FROM eqkt
            INTO  (<wa_saida_simplificada>-shtxt)
            WHERE equnr = <wa_saida_simplificada>-equnr
            AND   spras = 'PT'.

      SELECT SINGLE iflo~pltxt FROM v_equi AS equi
        INNER JOIN iloa AS iloa ON equi~tplnr = iloa~tplnr
        INNER JOIN iflo  AS iflo ON iflo~tplnr = iloa~tplnr
        INTO (<wa_saida_simplificada>-pltxt) WHERE equi~equnr = <wa_saida_simplificada>-equnr.

      SELECT SINGLE *
 FROM zpmt0018
 INTO CORRESPONDING FIELDS OF wa_zpmt0018
 WHERE bloco = <wa_saida_simplificada>-bloco
 AND  idord =  <wa_saida_simplificada>-idord
 AND type IN ( 'S', 'E', 'W' )
 AND inativo = ' '.

      w_saida-aufnr  = |{ <wa_saida_simplificada>-aufnr ALPHA = OUT }|.
      w_saida-bloco  = <wa_saida_simplificada>-bloco.
      w_saida-ktext  = <wa_saida_simplificada>-ktext.
      w_saida-dtini  = <wa_saida_simplificada>-dtini.
      w_saida-dtfim  = <wa_saida_simplificada>-dtfim.
      w_saida-hrini  = <wa_saida_simplificada>-hrini.
      w_saida-hrfim  = <wa_saida_simplificada>-hrfim.
      w_saida-statp  = <wa_saida_simplificada>-statp.
      w_saida-idord  = <wa_saida_simplificada>-idord.
      w_saida-dtreg  = <wa_saida_simplificada>-dtreg.
      w_saida-hrreg  = <wa_saida_simplificada>-hrreg.
      w_saida-equnr  = <wa_saida_simplificada>-equnr.
      w_saida-tplnr  = <wa_saida_simplificada>-tplnr.
      w_saida-idapn  = <wa_saida_simplificada>-idapn.
      w_saida-idopr  = <wa_saida_simplificada>-idopr.
      w_saida-shtxt = <wa_saida_simplificada>-shtxt.
      w_saida-pltxt = <wa_saida_simplificada>-pltxt.
      w_saida-bloco   = <wa_saida_simplificada>-bloco.
      w_saida-type    = wa_zpmt0018-type.

      IF wa_zpmt0018-type NE ' '.

        w_saida-icone   = '@0Z@'.

      ENDIF.

      IF  w_saida-type EQ 'S' OR <wa_saida_simplificada>-statp = 'P'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'SHTXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.

      ELSEIF  w_saida-type EQ 'E' OR <wa_saida_simplificada>-statp = 'E'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'SHTXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.

      ELSEIF  w_saida-type EQ 'W'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'SHTXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
      ENDIF.

      w_saida-cell_color[] = it_color.

      APPEND w_saida TO gt_saida.

      CLEAR: w_saida, wa_zpmt0018.

      FREE: it_color.

    ENDLOOP.

    DESCRIBE TABLE gt_saida LINES g_tabstrip-qtd1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEL_DADOS_SUC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sel_dados_suc .

  FREE: it_saida, gt_saida.

  SELECT *
  FROM zpmt0016
    INTO CORRESPONDING FIELDS OF TABLE it_saida
      WHERE aufnr NE '' AND statp = 'P'
        AND iwerk IN p_iwerk
       ORDER BY dtreg DESCENDING hrreg DESCENDING.

  SORT it_saida DESCENDING BY aufnr bloco .
  DELETE ADJACENT DUPLICATES FROM it_saida COMPARING aufnr.

  IF it_saida IS NOT INITIAL.

    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<wa_saida_simplificada>).


      SELECT SINGLE eqktx FROM eqkt
            INTO  (<wa_saida_simplificada>-shtxt)
            WHERE equnr = <wa_saida_simplificada>-equnr
            AND   spras = 'PT'.

      SELECT SINGLE iflo~pltxt FROM v_equi AS equi
        INNER JOIN iloa AS iloa ON equi~tplnr = iloa~tplnr
        INNER JOIN iflo  AS iflo ON iflo~tplnr = iloa~tplnr
        INTO (<wa_saida_simplificada>-pltxt) WHERE equi~equnr = <wa_saida_simplificada>-equnr.

      SELECT SINGLE *
 FROM zpmt0018
 INTO CORRESPONDING FIELDS OF wa_zpmt0018
 WHERE bloco = <wa_saida_simplificada>-bloco
 AND  idord =  <wa_saida_simplificada>-idord
 AND type IN ( 'S', 'E', 'W' )
 AND inativo = ' '.

      w_saida-aufnr  = |{ <wa_saida_simplificada>-aufnr ALPHA = OUT }|.
      w_saida-bloco  = <wa_saida_simplificada>-bloco.
      w_saida-ktext  = <wa_saida_simplificada>-ktext.
      w_saida-dtini  = <wa_saida_simplificada>-dtini.
      w_saida-dtfim  = <wa_saida_simplificada>-dtfim.
      w_saida-hrini  = <wa_saida_simplificada>-hrini.
      w_saida-hrfim  = <wa_saida_simplificada>-hrfim.
      w_saida-statp  = <wa_saida_simplificada>-statp.
      w_saida-idord  = <wa_saida_simplificada>-idord.
      w_saida-dtreg  = <wa_saida_simplificada>-dtreg.
      w_saida-hrreg  = <wa_saida_simplificada>-hrreg.
      w_saida-equnr  = <wa_saida_simplificada>-equnr.
      w_saida-tplnr  = <wa_saida_simplificada>-tplnr.
      w_saida-idapn  = <wa_saida_simplificada>-idapn.
      w_saida-idopr  = <wa_saida_simplificada>-idopr.
      w_saida-shtxt = <wa_saida_simplificada>-shtxt.
      w_saida-pltxt = <wa_saida_simplificada>-pltxt.
      w_saida-bloco   = <wa_saida_simplificada>-bloco.
      w_saida-type    = wa_zpmt0018-type.

      IF wa_zpmt0018-type NE ' '.

        w_saida-icone   = '@0Z@'.

      ENDIF.

      IF  w_saida-type EQ 'S' OR <wa_saida_simplificada>-statp = 'P'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'SHTXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.

      ELSEIF  w_saida-type EQ 'E' OR <wa_saida_simplificada>-statp = 'E'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'SHTXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.

      ELSEIF  w_saida-type EQ 'W'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'SHTXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
      ENDIF.

      w_saida-cell_color[] = it_color.

      APPEND w_saida TO gt_saida.

      CLEAR: w_saida, wa_zpmt0018.

      FREE: it_color.

    ENDLOOP.

    DESCRIBE TABLE gt_saida LINES g_tabstrip-qtd1.


  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEL_NOTAS_NOVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sel_notas_nova .


  FREE: gt_nota.
  SELECT *
  FROM zpmt0014
  INTO CORRESPONDING FIELDS OF TABLE gt_nota
  WHERE qmnum EQ '' AND statp = 'E'
        AND iwerk IN p_iwerk.

  SORT gt_nota ASCENDING BY idnot.

  IF gt_nota IS NOT INITIAL.

    LOOP AT gt_nota ASSIGNING FIELD-SYMBOL(<w_nota>).


      <w_nota>-equnr = |{ <w_nota>-equnr ALPHA = IN }|.
      SELECT SINGLE *
      FROM eqkt
      INTO @DATA(_eqkt)
      WHERE equnr EQ @<w_nota>-equnr
      AND   spras EQ @sy-langu.
      <w_nota>-eqktx   = _eqkt-eqktx.


      SELECT SINGLE *
      FROM iflo
      INTO @DATA(_iflo)
        WHERE tplnr EQ @<w_nota>-tplnr.
      <w_nota>-pltxt   = _iflo-pltxt.


      <w_nota>-idnot = |{ <w_nota>-idnot ALPHA = IN }|.
      SELECT SINGLE *
      FROM zpmt0018
      INTO CORRESPONDING FIELDS OF wa_zpmt0018
      WHERE idnot =  <w_nota>-idnot
      AND type IN ( 'S', 'E', 'W' )
      AND inativo NE abap_true.

      <w_nota>-type    = wa_zpmt0018-type.

      IF wa_zpmt0018-type NE ' '.
        <w_nota>-icone   = '@0Z@'.

      ENDIF.

      IF  <w_nota>-type EQ 'S' OR <w_nota>-statp = 'P'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMNUM'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMTXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQKTX'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.

      ELSEIF  <w_nota>-type EQ 'E' OR <w_nota>-statp = 'E'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMNUM'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMTXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQKTX'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.

      ELSEIF  <w_nota>-type EQ 'W'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMNUM'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMTXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQKTX'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
      ENDIF.

      <w_nota>-cell_color[] = it_color.

*      APPEND w_saida TO gs_saida.

      CLEAR: w_saida, wa_zpmt0018.

      FREE: it_color.

    ENDLOOP.

    DESCRIBE TABLE gt_nota LINES g_tabstrip-qtd2.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEL_NOT_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sel_not_proc .

  FREE:gs_nota.
  SELECT *
  FROM zpmt0014
  INTO CORRESPONDING FIELDS OF TABLE gs_nota
  WHERE qmnum NE '' AND statp = 'P'
        AND iwerk IN p_iwerk

ORDER BY dtreg DESCENDING hrreg DESCENDING.

  SORT gs_nota ASCENDING BY qmnum .
  DELETE ADJACENT DUPLICATES FROM gs_nota COMPARING qmnum.

  IF gs_nota IS NOT INITIAL.

    LOOP AT gs_nota ASSIGNING FIELD-SYMBOL(<w_nota>).


      <w_nota>-equnr = |{ <w_nota>-equnr ALPHA = IN }|.
      SELECT SINGLE *
      FROM eqkt
      INTO @DATA(_eqkt)
      WHERE equnr EQ @<w_nota>-equnr
      AND   spras EQ @sy-langu.
      <w_nota>-eqktx   = _eqkt-eqktx.


      SELECT SINGLE *
      FROM iflo
      INTO @DATA(_iflo)
        WHERE tplnr EQ @<w_nota>-tplnr.
      <w_nota>-pltxt   = _iflo-pltxt.


      <w_nota>-idnot = |{ <w_nota>-idnot ALPHA = IN }|.
      SELECT SINGLE *
      FROM zpmt0018
      INTO CORRESPONDING FIELDS OF wa_zpmt0018
      WHERE idnot =  <w_nota>-idnot
      AND type IN ( 'S', 'E', 'W' )
      AND inativo = ' '.


      <w_nota>-type    = wa_zpmt0018-type.

      IF wa_zpmt0018-type NE ' '.

        w_saida-icone   = '@0Z@'.

      ENDIF.

      IF  <w_nota>-type EQ 'S' OR <w_nota>-statp = 'P'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMNUM'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMTXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQKTX'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.

      ELSEIF  <w_nota>-type EQ 'E' OR <w_nota>-statp = 'E'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMNUM'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMTXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQKTX'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.

      ELSEIF  <w_nota>-type EQ 'W'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMNUM'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMTXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQKTX'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
      ENDIF.

      <w_nota>-cell_color[] = it_color.

*      APPEND w_saida TO gs_saida.

      CLEAR: w_saida, wa_zpmt0018.

      FREE: it_color.

    ENDLOOP.

    DESCRIBE TABLE gs_nota LINES g_tabstrip-qtd2.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEL_NOTAS_MODIF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sel_notas_modif .

  FREE: gw_nota.
  SELECT *
    FROM zpmt0014
    INTO CORRESPONDING FIELDS OF TABLE gw_nota
    WHERE qmnum NE '' AND statp = 'E'
          AND iwerk IN p_iwerk

  ORDER BY dtreg DESCENDING hrreg DESCENDING.

  SORT gw_nota ASCENDING BY qmnum .
  DELETE ADJACENT DUPLICATES FROM gw_nota COMPARING qmnum.


  IF gw_nota IS NOT INITIAL.

    LOOP AT gw_nota ASSIGNING FIELD-SYMBOL(<w_nota>).


      <w_nota>-equnr = |{ <w_nota>-equnr ALPHA = IN }|.
      SELECT SINGLE *
      FROM eqkt
      INTO @DATA(_eqkt)
      WHERE equnr EQ @<w_nota>-equnr
      AND   spras EQ @sy-langu.
      <w_nota>-eqktx   = _eqkt-eqktx.


      SELECT SINGLE *
      FROM iflo
      INTO @DATA(_iflo)
        WHERE tplnr EQ @<w_nota>-tplnr.
      <w_nota>-pltxt   = _iflo-pltxt.


      <w_nota>-idnot = |{ <w_nota>-idnot ALPHA = IN }|.
      SELECT SINGLE *
      FROM zpmt0018
      INTO CORRESPONDING FIELDS OF wa_zpmt0018
      WHERE idnot =  <w_nota>-idnot
      AND type IN ( 'S', 'E', 'W' )
      AND inativo NE abap_true.


      <w_nota>-type    = wa_zpmt0018-type.

      IF wa_zpmt0018-type NE ' '.

        <w_nota>-icone   = '@0Z@'.

      ENDIF.

      IF  <w_nota>-type EQ 'S' OR <w_nota>-statp = 'P'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMNUM'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMTXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQKTX'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '5' color-int = '0'  color-inv = '0' ) TO it_color.

      ELSEIF  <w_nota>-type EQ 'E' OR <w_nota>-statp = 'E'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMNUM'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMTXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQKTX'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '6' color-int = '0'  color-inv = '0' ) TO it_color.

      ELSEIF  <w_nota>-type EQ 'W'.

        CLEAR wa_color.

        APPEND VALUE #( fname = 'AUFNR'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMNUM'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'QMTXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQUNR'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'EQKTX'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'KTEXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'PLTXT'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'STATP'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
        APPEND VALUE #( fname = 'BLOCO'   color-col = '3' color-int = '0'  color-inv = '0' ) TO it_color.
      ENDIF.

      <w_nota>-cell_color[] = it_color.

*      APPEND w_saida TO gs_saida.

      CLEAR: w_saida, wa_zpmt0018.

      FREE: it_color.

    ENDLOOP.

    DESCRIBE TABLE gw_nota LINES g_tabstrip-qtd1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0500 OUTPUT.
  SET PF-STATUS 'ST005'.
  SET TITLEBAR 'TI005'.

  IF obj_custom_0160 IS INITIAL.

    FREE it_fieldcatalog.
    CLEAR: gs_layout..
    PERFORM it_fieldcatalog USING:

   1 'IDCAU'      'T_NOTA' '25'  ''  ''  ' '   'N° ITEM                    ' '' ' ' '' ' ' 'X' '',
   2 'IDNOT'      'T_NOTA' '30'  ''  ' '  ' '   'ID Nota                   ' '' ' ' '' ' ' 'X' '',
*   3 'OTKAT'      'T_NOTA' '50'  ''  ' '  ' '   'Tip catalogo P/ Obj       ' '' ' ' '' ' ' ' ' '',
   4 'OTGRP'      'T_NOTA' '50'  ' '  ' '  ' '   'Grp parte objeto         ' '' ' ' ' ' ' ' ' ' '',
   5 'OTEIL'      'T_NOTA' '50'  'X'  ' '  ' '   'Parte objeto             ' '' ' ' '' ' ' ' ' '',
   6 'KURZTEXT'   'T_NOTA' '50'  ''  ' '  ' '   'Text Objeto               ' '' ' ' '' ' ' ' ' '',
*   7 'FEKAT'      'T_NOTA' '10'  ''  ' '  ' '   'Catalogo problema         ' '' ' ' '' ' ' ' ' '',
   8 'FEGRP'      'T_NOTA' '30'  ' '  ' '  ' '   'Grp problema             ' '' ' ' '' ' ' ' ' '',
   9 'FECOD'      'T_NOTA' '30'  'X'  ' '  ' '   'Problema/Dano            ' '' ' ' '' ' ' ' ' '',
  10 'KURZTEXT_2' 'T_NOTA' '50'  ''  ' '  ' '   'Texto Problema            ' '' ' ' '' ' ' ' ' '',
*  11 'URKAT'      'T_NOTA' '10'  ''  ' '  ' '   'Catalogo causas           ' '' ' ' '' ' ' ' ' '',
  12 'URGRP'      'T_NOTA' '30'  ' '  ' '  ' '   'Grp causas               ' '' ' ' '' ' ' ' ' '',
  13 'URCOD'      'T_NOTA' '30'  'X'  ' '  ' '   'Tipo de causas           ' '' ' ' '' ' ' ' ' '',
  14 'KURZTEXT_3' 'T_NOTA' '50'  ''  ' '  ' '   'Texto causa               ' '' ' ' '' ' ' ' ' '',
*  15 'MNKAT'      'T_NOTA' '10'  ''  ' '  ' '   'Catalogo atividades       ' '' ' ' '' ' ' ' ' '',
  16 'MNGRP'      'T_NOTA' '30'  ' '  ' '  ' '   'Grp ação           ' '' ' ' '' ' ' ' ' '',
  17 'MNCOD'      'T_NOTA' '30'  'X'  ' '  ' '   'Tipo de ação       ' '' ' ' '' ' ' ' ' '',
  18 'KURZTEXT_4' 'T_NOTA' '30'  ' '  ' '  ' '   'Texto Ação      ' '' ' ' '' ' ' ' ' ''.

    REFRESH lt_f4.
    lt_f4[] =
    VALUE #(
             ( fieldname = 'OTEIL' register = abap_true  getbefore = abap_true )
             ( fieldname = 'URCOD' register = abap_true  getbefore = abap_true )
             ( fieldname = 'MNCOD' register = abap_true  getbefore = abap_true )
             ( fieldname = 'FECOD' register = abap_true  getbefore = abap_true )
           ).

    CREATE OBJECT obj_custom_0160
      EXPORTING
        container_name              = 'TL0500'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CLEAR: it_exclude_fcode, it_exclude_fcode[].

    ls_stable-row        = 'X'.

    CREATE OBJECT obj_alv_0160
      EXPORTING
        i_parent = obj_custom_0160.

    CALL METHOD obj_alv_0160->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
*       IS_VARIANT           = GS_VARIANT
        it_toolbar_excluding = it_exclude_fcode
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog[]
        it_outtab            = t_nota.
*        IT_SORT              = IT_SORT.

    CALL METHOD obj_alv_0160->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv_0160->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD obj_alv_0160->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER:
   lcl_eventos=>on_onf4 FOR obj_alv_0160,
   lcl_eventos=>on_data_changed FOR obj_alv_0160,
*    LCL_EVENTOS=>ON_HOTSPOT_CLICK FOR OBJ_ALV_0160,
   lcl_event_handler=>on_double_click FOR obj_alv_0160.

  ENDIF.

  CALL METHOD obj_alv_0160->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'SAVE'.
      PERFORM modify_nota.
      PERFORM chamar_job_not.

      DELETE t_nota WHERE idnot = w_nota-idnot.

      PERFORM buscar_dados.

      CALL SCREEN 0400.

      IF obj_alv_0110 IS NOT INITIAL.
        CALL METHOD obj_alv_0110->refresh_table_display
          EXPORTING
            is_stable = ls_stable
          EXCEPTIONS
            finished  = 1
            OTHERS    = 2.

      ELSEIF obj_alv_0120 IS NOT INITIAL.
        CALL METHOD obj_alv_0120->refresh_table_display
          EXPORTING
            is_stable = ls_stable
          EXCEPTIONS
            finished  = 1
            OTHERS    = 2.

      ELSEIF obj_alv_0130 IS NOT INITIAL.
        CALL METHOD obj_alv_0130->refresh_table_display
          EXPORTING
            is_stable = ls_stable
          EXCEPTIONS
            finished  = 1
            OTHERS    = 2.

      ELSEIF obj_alv_0140 IS NOT INITIAL.
        CALL METHOD obj_alv_0140->refresh_table_display
          EXPORTING
            is_stable = ls_stable
          EXCEPTIONS
            finished  = 1
            OTHERS    = 2.

      ELSEIF obj_alv_0150 IS NOT INITIAL.
        CALL METHOD obj_alv_0150->refresh_table_display
          EXPORTING
            is_stable = ls_stable
          EXCEPTIONS
            finished  = 1
            OTHERS    = 2.

      ELSEIF obj_alv_0160 IS NOT INITIAL.
        CALL METHOD obj_alv_0160->refresh_table_display
          EXPORTING
            is_stable = ls_stable
          EXCEPTIONS
            finished  = 1
            OTHERS    = 2.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BUSCA_OTKAT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_otkat INPUT.



ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  MODIFY_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_nota .

  IF w_nota IS NOT INITIAL.
    w_nota-statp = ' '.
    MODIFY zpmt0014 FROM w_nota.
    COMMIT WORK.
  ENDIF.

  IF t_nota IS NOT INITIAL.
    LOOP AT t_nota ASSIGNING FIELD-SYMBOL(<_nota>).

      MODIFY zpmt0019 FROM <_nota>.
      COMMIT WORK.
    ENDLOOP.
  ENDIF.

  SELECT *
  FROM zpmt0018
  INTO CORRESPONDING FIELDS OF TABLE it_msg_log
  WHERE idnot EQ  w_nota-idnot
  AND   inativo NE abap_true.

  LOOP  AT   it_msg_log INTO DATA(wa_msg_log).

    wa_msg_log-inativo = 'X'.
    MODIFY zpmt0018 FROM wa_msg_log.
    COMMIT WORK.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHAMAR_JOB_NOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chamar_job_not .
  DATA(obj_create) = NEW zcl_pm_ordem( ).
  CALL METHOD obj_create->call_report
    EXPORTING
      i_sequen = CONV #( |{ w_nota-idnot ALPHA = OUT }| )
      i_report = 'ZPMR0046'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM at_alv .

*Atualizar ALVs.

  IF obj_alv_0110 IS NOT INITIAL.
    CALL METHOD obj_alv_0110->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

  ELSEIF obj_alv_0120 IS NOT INITIAL.
    CALL METHOD obj_alv_0120->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

  ELSEIF obj_alv_0130 IS NOT INITIAL.
    CALL METHOD obj_alv_0130->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

  ELSEIF obj_alv_0140 IS NOT INITIAL.
    CALL METHOD obj_alv_0140->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXIBIR_STATUS_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exibir_status_proc .
  DATA: linha_selecionada TYPE slis_selfield.
  DATA: _exit             TYPE c.
  DATA: t_zpmt0052 TYPE TABLE OF zpmt0052.
  SELECT * FROM zpmt0052 INTO TABLE t_zpmt0052 WHERE datum EQ sy-datum.

  IF ( t_zpmt0052 IS NOT INITIAL ).


    SORT t_zpmt0052 DESCENDING BY datum uzeit.

    DATA(tl_fieldcat) = VALUE slis_t_fieldcat_alv(
    ( fieldname = 'DATUM'        seltext_m = 'Data    '  outputlen = '10' )
    ( fieldname = 'UZEIT'        seltext_m = 'Hora    '  outputlen = '07' )
    ( fieldname = 'MSGV1'        seltext_m = 'msg     '  outputlen = '60' )
    ( fieldname = 'MSGTY'        seltext_m = 'Tipo    '  outputlen = '02' )
    ( fieldname = 'TITLE'        seltext_m = 'Desc    '  outputlen = '60' )
    ( fieldname = 'UNAME'        seltext_m = 'Usuario '  outputlen = '12' ) ).

    CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
      EXPORTING
        i_title     = 'Status de processamento'
        i_selection = 'X'
        i_tabname   = 't_zpmt0052'
        i_zebra     = 'X'
        it_fieldcat = tl_fieldcat
      IMPORTING
*       es_selfield = data(linha_selecionada)
        e_exit      = _exit
      TABLES
        t_outtab    = t_zpmt0052.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ME_PROC_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM me_proc_job .

  DATA: dt TYPE string.
  DATA: t_zpmt0052 TYPE TABLE OF zpmt0052.

  FREE: t_zpmt0052.

  "Verifica se tem um job ja em processamento.
  FREE: t_zpmt0052.
  SELECT * FROM zpmt0052 INTO TABLE t_zpmt0052 WHERE msgty EQ 'P'.
  IF sy-subrc EQ 0.
    MESSAGE 'Dados em processamento, aguarde' TYPE 'I' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DATA(tipo) = 'P'.
  DATA(r_return) = 'Em processamento'.

  "Grava processo log.
  APPEND VALUE #(   mandt = sy-mandt
                    datum = sy-datum
                    uzeit = sy-uzeit
                    title = 'Processa dados'
                    uname = sy-uname
                    msgty = tipo
                    msgv1 = r_return ) TO t_zpmt0052.

  IF t_zpmt0052 IS NOT INITIAL.
    MODIFY zpmt0052 FROM TABLE t_zpmt0052.
    COMMIT WORK.
  ENDIF.


  dt = CONV #( sy-datum && '-' && sy-uzeit ).

  "// Processa os Dados carga de dados manutenção para o APP Mobile Amaggi Man
  CALL METHOD zcl_webservic_protheus=>call_report
    EXPORTING
      i_sequen = dt
      i_report = 'ZPMR0071'
*-IR054443 - 14.04.2021 - JT - inicio
      i_uname  = 'JOBADM'.
*-IR054443 - 14.04.2021 - JT - fim
*   "// Finaliza o processo em caso de dados OffLine
  EXIT.
*      ENDIF.


ENDFORM.
