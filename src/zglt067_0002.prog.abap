*----------------------------------------------------------------------*
***INCLUDE ZGLT067_0002 .
*----------------------------------------------------------------------*
TABLES: zde_saldo_cta_banco.

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler_0002 DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: event_handler_0002  TYPE REF TO lcl_event_handler_0002.

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler_0002 IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click_0002 USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "DATA_CHANGED_FINISHED
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

DATA: p_sel_bukrs  TYPE bukrs,
      "P_SEL_WAERS TYPE WAERS_SKB1,
      p_sel_data   TYPE budat,
      p_sel_contas TYPE fagl_range_t_racct.

DATA: it_selecao TYPE TABLE OF zde_saldo_cta_banco WITH HEADER LINE,
      wa_t001    TYPE t001,
      pconta     TYPE saknr.

DATA: ok_code TYPE sy-ucomm.
DATA: tp_comp(1).
DATA: p_datanova TYPE sy-datum..

DATA: it_saldo_contas  TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
      it_saldo_contas2 TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE.

DATA: ctl_alv_0002       TYPE REF TO cl_gui_alv_grid,
      ctl_con_0002       TYPE REF TO cl_gui_custom_container,
      gs_lay_0002        TYPE lvc_s_layo,
      gs_var_0002        TYPE disvariant,
      gs_scroll_col_0002 TYPE lvc_s_col,
      gs_scroll_row_0002 TYPE lvc_s_roid,
      it_catalog_0002    TYPE lvc_t_fcat.

DATA: it_selected_0002 TYPE lvc_t_row,
      wa_selected_0002 TYPE lvc_s_row.

DATA: it_exclude_0002 TYPE ui_functions,
      wa_exclude_0002 LIKE LINE OF it_exclude_0002.

DATA      sd_compen      TYPE zde_saldo_cta_banco-saldod.
DATA      sd_compen_02   TYPE zde_saldo_cta_banco-saldod.
DATA      sd_compen_cop  TYPE zde_saldo_cta_banco-saldod.
DATA      it_skb1           TYPE TABLE OF zsaldo_cta_moeda WITH HEADER LINE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0002 OUTPUT.

  DATA: lc_text_date2 TYPE c LENGTH 10.
  DATA: fs_sort_0002 TYPE lvc_s_sort,
        gt_sort_0002 TYPE lvc_t_sort.

  DATA: it_ucomm2 TYPE TABLE OF sy-ucomm.
  APPEND 'SAVE'       TO it_ucomm2.
  APPEND 'TLOTE'      TO it_ucomm2.
  APPEND 'BAIXAR_DOC' TO it_ucomm2.
  APPEND 'COMPENSAR'  TO it_ucomm2.
  APPEND 'ARQUIVO'    TO it_ucomm2.
  APPEND 'ESTORNO'    TO it_ucomm2.

  SET PF-STATUS 'PF0001' EXCLUDING it_ucomm2.
  SET TITLEBAR 'TL0001' WITH zde_saldo_cta_banco-budat.

  IF ctl_con_0002 IS INITIAL.

    CREATE OBJECT ctl_con_0002
      EXPORTING
        container_name = 'ALV_DOCS2'.

    CREATE OBJECT ctl_alv_0002
      EXPORTING
        i_parent = ctl_con_0002.

    PERFORM fill_it_fieldcatalog_0002.
*   Fill info for layout variant

    PERFORM fill_gs_variant_0002.
*   Set layout parameters for ALV grid

    "GS_LAY_0102-SEL_MODE   = 'A'.
    gs_lay_0002-zebra = 'X'.

*    WRITE zde_saldo_cta_banco-budat TO lc_text_date2.
*
*    CONCATENATE text-003 '-' lc_text_date2 INTO gs_lay_0002-grid_title SEPARATED BY space.

    CLEAR: fs_sort_0002.
    fs_sort_0002-spos       = 1.     "first sorting key
    fs_sort_0002-fieldname  = 'SAKNR'. "fieldname for sort
    fs_sort_0002-up         = 'X'. "sort ascending
    INSERT fs_sort_0002 INTO TABLE gt_sort_0002. "insert to sort table

    CALL METHOD ctl_alv_0002->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_0002
        is_variant           = gs_var_0002
        it_toolbar_excluding = it_exclude_0002
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_catalog_0002        "IT_EXCEPT_QINFO = IT_HINTS
        it_outtab            = it_selecao[]
        it_sort              = gt_sort_0002[].

    CREATE OBJECT event_handler_0002.
    SET HANDLER event_handler_0002->handle_hotspot_click FOR ctl_alv_0002.

    CALL METHOD ctl_alv_0002->refresh_table_display.

  ELSE.
    CALL METHOD ctl_alv_0002->refresh_table_display.
  ENDIF.

  CALL METHOD ctl_alv_0002->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0002
      es_row_no   = gs_scroll_row_0002.

ENDMODULE.                 " STATUS_0002  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0002 INPUT.

  CASE ok_code.
    WHEN 'TROCAR_D'.
      p_datanova = p_sel_data.
      PERFORM input_data CHANGING p_datanova.
      p_sel_data = p_datanova.
*      pdata      = p_datanova.
      PERFORM popula_contas USING p_sel_bukrs p_sel_data p_sel_contas.
      CLEAR ok_code.
    WHEN 'ATUALIZAR'.
      PERFORM popula_contas USING p_sel_bukrs p_sel_data p_sel_contas.
      CLEAR ok_code.
    WHEN 'TLOTE'.
      CALL TRANSACTION 'ZGL016'.
      CLEAR ok_code.
      "WHEN 'TF28'.
      "  CALL TRANSACTION 'F-28'.
      "  CLEAR OK_CODE.
      "WHEN 'TF53'.
      "  CALL TRANSACTION 'F-53'.
      "  CLEAR OK_CODE.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0002  INPUT

*&---------------------------------------------------------------------*
*&      Form  POPULA_CONTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM popula_contas USING pbukrs TYPE bukrs
                         pdata  TYPE budat
                         psaknr TYPE fagl_range_t_racct.

  DATA: vg_mes_pos  TYPE i,
        vg_saldo_mi TYPE hslvt12,
        refe1       TYPE hslvt12.

  DATA: wa_contas         TYPE zlc_emp_contas,
        lc_pdata          TYPE sydatum,
        lc_pdata_01       TYPE sydatum,
        lc_pdata_30       TYPE sydatum,
        it_contas         TYPE zct_emp_contas,
        wa_moedas_empresa TYPE x001,

        it_skb1_aux       TYPE TABLE OF zsaldo_cta_moeda WITH HEADER LINE,
        it_skat           TYPE TABLE OF skat WITH HEADER LINE,
        it_zsaldo         TYPE TABLE OF zsaldo_cta_banco WITH HEADER LINE,
        lc_ryear          TYPE gjahr,
        it_saldos         TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
        it_saldos_2       TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
        it_saldos_3       TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE.

  p_sel_bukrs  = pbukrs.
  p_sel_data   = pdata.
  p_sel_contas = psaknr .

  SELECT SINGLE * INTO wa_t001
    FROM t001
   WHERE bukrs EQ pbukrs.

  CALL FUNCTION 'FI_CURRENCY_INFORMATION'
    EXPORTING
      i_bukrs = wa_t001-bukrs
    IMPORTING
      e_x001  = wa_moedas_empresa.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_skb1
    FROM zsaldo_cta_moeda
   WHERE bukrs EQ pbukrs
     AND saknr IN psaknr.

  CHECK it_skb1[] IS NOT INITIAL.

  MOVE it_skb1[] TO it_skb1_aux[].
  SORT it_skb1_aux BY waers.
  DELETE ADJACENT DUPLICATES FROM it_skb1_aux COMPARING waers.

  CLEAR: it_selecao[].

* RJF - Ini - 110390 - IR133267 - ZFIS36 saldo bancário em moeda EUR - 2023.05.03
  PERFORM saldo_bancario_eur USING pbukrs
                                   pdata
                                   psaknr_aux.
* RJF - Fim - 110390 - IR133267 - ZFIS36 saldo bancário em moeda EUR - 2023.05.03

  LOOP AT it_skb1_aux.

    CLEAR: it_contas.

    IF ( it_skb1_aux-waers EQ wa_t001-waers OR it_skb1_aux-waers EQ wa_moedas_empresa-hwae2 OR it_skb1_aux-waers EQ wa_moedas_empresa-hwae3 ).

      LOOP AT it_skb1 WHERE waers EQ it_skb1_aux-waers.
        wa_contas-bukrs = it_skb1-bukrs.
        wa_contas-saknr = it_skb1-saknr.
        APPEND wa_contas TO it_contas.
      ENDLOOP.

      CALL FUNCTION 'OIUREP_MONTH_FIRST_LAST'
        EXPORTING
          i_date      = pdata
        IMPORTING
          e_first_day = lc_pdata_01
          e_last_day  = lc_pdata_30
        EXCEPTIONS
          wrong_date  = 1
          OTHERS      = 2.

      lc_pdata = lc_pdata_01 - 1.

      lc_ryear = lc_pdata(4).

      "Saldo Mês Anterior
      CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
        EXPORTING
          ryear         = lc_ryear
          contas        = it_contas
          p_gerar_todas = 'X'
        TABLES
          it_saldos     = it_saldos
          it_saldos_2   = it_saldos_2
          it_saldos_3   = it_saldos_3
        EXCEPTIONS
          moeda_nao_adm = 1
          erro_ledger   = 2
          OTHERS        = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      lc_pdata = pdata - 1.

      CLEAR: it_saldo_contas[].

      IF lc_pdata_01 NE pdata.
        CALL FUNCTION 'Z_FI_GL_SALDO_BSIX'
          EXPORTING
            p_dt_inicial    = lc_pdata_01
            p_dt_posicao    = lc_pdata
            contas          = it_contas
            waers           = it_skb1_aux-waers
          TABLES
            it_saldo_contas = it_saldo_contas.
      ENDIF.

      CLEAR: it_saldo_contas2[].

      CALL FUNCTION 'Z_FI_GL_SALDO_BSIX'
        EXPORTING
          p_dt_inicial    = pdata
          p_dt_posicao    = pdata
          contas          = it_contas
          waers           = it_skb1_aux-waers
        TABLES
          it_saldo_contas = it_saldo_contas2.

      SELECT * INTO TABLE it_skat
        FROM skat
         FOR ALL ENTRIES IN it_skb1
       WHERE spras EQ	sy-langu
         AND ktopl EQ	wa_t001-ktopl
         AND saknr EQ	it_skb1-saknr.

      SORT it_skat BY saknr.

*      SELECT * INTO TABLE it_zsaldo
*        FROM zsaldo_cta_banco
*         FOR ALL ENTRIES IN it_skb1
*       WHERE bukrs EQ it_skb1-bukrs
*         AND saknr EQ it_skb1-saknr
*         AND budat EQ pdata.
*
*      SORT it_zsaldo BY saknr.
    ELSE.

      LOOP AT it_skb1 WHERE waers EQ it_skb1_aux-waers.
        wa_contas-bukrs = it_skb1-bukrs.
        wa_contas-saknr = it_skb1-saknr.
        APPEND wa_contas TO it_contas.
      ENDLOOP.

      lc_pdata = pdata - 1.

      CALL FUNCTION 'Z_FI_GL_SALDO_BSIX'
        EXPORTING
          p_dt_posicao    = lc_pdata
          contas          = it_contas
          waers           = it_skb1_aux-waers
        TABLES
          it_saldo_contas = it_saldo_contas.

      CALL FUNCTION 'Z_FI_GL_SALDO_BSIX'
        EXPORTING
          p_dt_inicial    = pdata
          p_dt_posicao    = pdata
          contas          = it_contas
          waers           = it_skb1_aux-waers
        TABLES
          it_saldo_contas = it_saldo_contas2.

    ENDIF.

    SELECT * INTO TABLE it_zsaldo
      FROM zsaldo_cta_banco
       FOR ALL ENTRIES IN it_skb1
     WHERE bukrs EQ it_skb1-bukrs
       AND saknr EQ it_skb1-saknr
       AND budat EQ pdata.

    SORT it_zsaldo BY saknr.

    LOOP AT it_skb1 WHERE waers EQ it_skb1_aux-waers.
      CLEAR: zde_saldo_cta_banco.
      zde_saldo_cta_banco-bukrs      = it_skb1-bukrs.
      zde_saldo_cta_banco-butxt      = wa_t001-butxt.
      zde_saldo_cta_banco-saknr      = it_skb1-saknr.
      zde_saldo_cta_banco-waers      = it_skb1-waers.
      zde_saldo_cta_banco-budat      = pdata.
      zde_saldo_cta_banco-saldoda    = 0.
      zde_saldo_cta_banco-saldod     = 0.
      zde_saldo_cta_banco-saldofinal = 0.
      zde_saldo_cta_banco-saldoex    = 0.
      zde_saldo_cta_banco-saldodif   = 0.

      READ TABLE it_zsaldo WITH KEY saknr = it_skb1-saknr.
      IF sy-subrc IS INITIAL.
        zde_saldo_cta_banco-saldoex = it_zsaldo-saldoex.
      ENDIF.

      READ TABLE it_skat WITH KEY saknr = it_skb1-saknr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        zde_saldo_cta_banco-txt50 = it_skat-txt50.
      ENDIF.

      LOOP AT it_saldo_contas WHERE racct EQ it_skb1-saknr.
        CASE it_saldo_contas-rldnr.
          WHEN 'H'. "Credito
            zde_saldo_cta_banco-saldoda = zde_saldo_cta_banco-saldoda - it_saldo_contas-slvt.
          WHEN 'S'. "Débito
            zde_saldo_cta_banco-saldoda = zde_saldo_cta_banco-saldoda + it_saldo_contas-slvt.
        ENDCASE.
      ENDLOOP.

      "Saldo do Mês Anterior """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      lc_pdata    = lc_pdata_01 - 1.
      vg_mes_pos  = lc_pdata+4(2).
      vg_saldo_mi = 0.

      IF vg_mes_pos EQ 12.
        vg_mes_pos = 16. "Considerar Periodos Extras 13,14,15,16.
      ENDIF.

      IF it_skb1-waers EQ wa_t001-waers.
        "1ª Moeda da Empresa
        LOOP AT it_saldos WHERE racct EQ zde_saldo_cta_banco-saknr.
          vg_saldo_mi = it_saldos-slvt.
          DO vg_mes_pos TIMES
            VARYING refe1 FROM it_saldos-sl01 NEXT it_saldos-sl02.
            ADD refe1 TO vg_saldo_mi.
          ENDDO.
        ENDLOOP.
      ELSE.
        CASE it_skb1-waers.
          WHEN wa_moedas_empresa-hwae2.
            "2ª Moeda da Empresa
            LOOP AT it_saldos_2 WHERE racct EQ zde_saldo_cta_banco-saknr.
              vg_saldo_mi = it_saldos_2-slvt.
              DO vg_mes_pos TIMES
                VARYING refe1 FROM it_saldos_2-sl01 NEXT it_saldos_2-sl02.
                ADD refe1 TO vg_saldo_mi.
              ENDDO.
            ENDLOOP.
          WHEN wa_moedas_empresa-hwae3.
            "3ª Moeda da Empresa
            LOOP AT it_saldos_3 WHERE racct EQ zde_saldo_cta_banco-saknr.
              vg_saldo_mi = it_saldos_3-slvt.
              DO vg_mes_pos TIMES
                VARYING refe1 FROM it_saldos_3-sl01 NEXT it_saldos_3-sl02.
                ADD refe1 TO vg_saldo_mi.
              ENDDO.
            ENDLOOP.
          WHEN OTHERS.
            "1ª Moeda da Empresa
            LOOP AT it_saldos WHERE racct EQ zde_saldo_cta_banco-saknr.
              vg_saldo_mi = it_saldos-slvt.
              DO vg_mes_pos TIMES
                VARYING refe1 FROM it_saldos-sl01 NEXT it_saldos-sl02.
                ADD refe1 TO vg_saldo_mi.
              ENDDO.
            ENDLOOP.
        ENDCASE.
      ENDIF.
      zde_saldo_cta_banco-saldoda = zde_saldo_cta_banco-saldoda + vg_saldo_mi.
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


** RJF - Ini - 2023.05.11
*      LOOP AT it_saldo_contas.
*        CASE it_saldo_contas-rldnr.
*          WHEN 'H'. "Credito

      IF vg_comp IS NOT INITIAL AND it_zgl_saldo[] IS NOT INITIAL.

        IF zde_saldo_cta_banco-saldoda IS NOT INITIAL.
          READ TABLE it_zgl_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>) WITH KEY bukrs =  zde_saldo_cta_banco-bukrs
                                                                              saknr =  zde_saldo_cta_banco-saknr
                                                                              waers =  zde_saldo_cta_banco-waers
                                                                     BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            IF <fs_saldo>-shkzg EQ 'H'.

              zde_saldo_cta_banco-saldoda = zde_saldo_cta_banco-saldoda - abs( <fs_saldo>-wrbtr ).

            ELSEIF <fs_saldo>-shkzg EQ 'S'.

              zde_saldo_cta_banco-saldoda = zde_saldo_cta_banco-saldoda + abs( <fs_saldo>-wrbtr ).

            ENDIF.

          ENDIF.

*        ELSE.
*          READ TABLE it_zgl_saldo ASSIGNING <fs_saldo> WITH KEY bukrs =  zde_saldo_cta_banco-bukrs
*                                                                              saknr =  zde_saldo_cta_banco-saknr
*                                                                             waers =  zde_saldo_cta_banco-waers
*                                                                     BINARY SEARCH.
*          IF sy-subrc IS INITIAL.
*
*            IF <fs_saldo>-shkzg EQ 'H'.
*
*              zde_saldo_cta_banco-saldoda = zde_saldo_cta_banco-saldoda - abs( <fs_saldo>-wrbtr ).
*
*            ELSEIF <fs_saldo>-shkzg EQ 'S'.
*
*              zde_saldo_cta_banco-saldoda = zde_saldo_cta_banco-saldoda + abs( <fs_saldo>-wrbtr ).
*
*            ENDIF.
*
*          ENDIF.
        ENDIF.
*            ELSE.
*              zde_saldo_cta_banco-saldoda = zde_saldo_cta_banco-saldoda - it_saldo_contas-slvt.
      ENDIF.

*          WHEN 'S'. "Débito
*
*            IF vg_comp IS NOT INITIAL AND it_zgl_saldo[] IS NOT INITIAL.
*
*              READ TABLE it_zgl_saldo ASSIGNING <fs_saldo> WITH KEY bukrs = it_saldo_contas-rbukrs
*                                                                    saknr = it_saldo_contas-racct
*                                                                    shkzg = it_saldo_contas-rldnr(1)
*                                                           BINARY SEARCH.
*              IF sy-subrc IS INITIAL.
*                zde_saldo_cta_banco-saldoda = zde_saldo_cta_banco-saldoda + it_saldo_contas-slvt + <fs_saldo>-wrbtr.
*              ENDIF.
*
*            ELSE.
*              zde_saldo_cta_banco-saldoda = zde_saldo_cta_banco-saldoda + it_saldo_contas-slvt.
*            ENDIF.
*
*        ENDCASE.
*      ENDLOOP.
** RJF - Fim - 2023.05.11


*      "Saldo Somente do Dia Atual
      LOOP AT it_saldo_contas2 WHERE racct EQ it_skb1-saknr.
        CASE it_saldo_contas2-rldnr.
          WHEN 'H'. "Credito
            zde_saldo_cta_banco-saldod = zde_saldo_cta_banco-saldod - it_saldo_contas2-slvt.
          WHEN 'S'. "Débito
            zde_saldo_cta_banco-saldod = zde_saldo_cta_banco-saldod + it_saldo_contas2-slvt.
        ENDCASE.
      ENDLOOP.

      "Saldo Final
      zde_saldo_cta_banco-saldofinal = zde_saldo_cta_banco-saldoda + zde_saldo_cta_banco-saldod.

      "Diferênça
      zde_saldo_cta_banco-saldodif   = zde_saldo_cta_banco-saldofinal - zde_saldo_cta_banco-saldoex.

      "Saldo Absoluto
      "ZDE_SALDO_CTA_BANCO-SALDOL   = ABS( ZDE_SALDO_CTA_BANCO-SALDODIF ).

      APPEND zde_saldo_cta_banco TO it_selecao.

    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " POPULA_CONTAS

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0002 .

  DATA: lc_col_pos TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat_0002> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_SALDO_CTA_BANCO'
    CHANGING
      ct_fieldcat      = it_catalog_0002.

  lc_col_pos = 1.

  LOOP AT it_catalog_0002 ASSIGNING <fs_cat_0002>.
    CASE <fs_cat_0002>-fieldname.
      WHEN 'TXT50'.
        <fs_cat_0002>-outputlen = 30.
      WHEN 'BUKRS'.
        <fs_cat_0002>-outputlen = 5.
      WHEN 'SAKNR'.
        <fs_cat_0002>-hotspot = 'X'.
      WHEN 'SALDODA' OR 'SALDOD' OR 'SALDOFINAL' OR  'SALDOEX' OR 'SALDODIF' OR 'SALDOL'.
        <fs_cat_0002>-do_sum    = 'X'.
        <fs_cat_0002>-outputlen = 18.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0002

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0002 .

  gs_var_0002-report      = sy-repid.
  gs_var_0002-handle      = '0002'.
  gs_var_0002-log_group   = abap_false.
  gs_var_0002-username    = abap_false.
  gs_var_0002-variant     = abap_false.
  gs_var_0002-text        = abap_false.
  gs_var_0002-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT_0002

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0002_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0002_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_0002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_hotspot_click_0002
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  IF row_id GT 0.
    READ TABLE it_selecao INDEX row_id INTO zde_saldo_cta_banco.

    CASE fieldname.
      WHEN 'SAKNR'.
        PERFORM chama_tela.
        CALL SCREEN 0001.
        SET TITLEBAR 'TL0001' WITH zde_saldo_cta_banco-budat.
        PERFORM popula_contas USING p_sel_bukrs p_sel_data p_sel_contas.
        CLEAR ok_code.
        IF ctl_con_0002 IS NOT INITIAL.
          CALL METHOD ctl_alv_0002->refresh_table_display.
        ENDIF.
    ENDCASE.
  ENDIF.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK_0002
*&---------------------------------------------------------------------*
*&      Form  SALDO_BANCARIO_EUR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM saldo_bancario_eur USING pbukrs TYPE bukrs
                              p_data TYPE sy-datum
                              psaknr TYPE fagl_range_t_racct.

  DATA: lv_dat      TYPE sy-datum,
        lv_data(10) TYPE c.
  CONSTANTS: lc_name TYPE tvarvc-name VALUE 'MAGGI_DATA_ARCHIVE'.

  SELECT low
    UP TO 1 ROWS
    INTO @DATA(lv_result)
      FROM tvarvc
      WHERE  name EQ @lc_name.
  ENDSELECT.

  IF sy-subrc IS INITIAL AND lv_result IS NOT INITIAL.
    lv_dat = lv_result.
    IF p_data LE lv_dat.
* Message "Para a data xx/xx/xxxx , os documentos foram arquivados.  Para consulta-los usar a transação FBL3N"
      lv_data = |{ p_data+6(2) }| && |'/'{ p_data+4(2) }| && |'/'{ p_data(4) }|.
      MESSAGE i127 WITH lv_data TEXT-023 DISPLAY LIKE 'E'.
    ELSE. " Somar o saldo da nova tabela , na composição do saldo da conta
*
*      IF psaknr IS INITIAL.
*        SELECT *
*          FROM zglt101_saldo
*          INTO TABLE it_zgl_saldo
*          WHERE bukrs EQ pbukrs
*          and waers = wa_t001-waers.
*      ELSE.
*        SELECT *
*          FROM zglt101_saldo
*          INTO TABLE it_zgl_saldo
*          WHERE bukrs EQ pbukrs
*            AND saknr IN psaknr
*           and waers = wa_t001-waers.
*      ENDIF.

      SELECT *
          FROM zglt101_saldo
          INTO TABLE it_zgl_saldo
          FOR ALL ENTRIES IN it_skb1
            WHERE bukrs EQ it_skb1-bukrs
              AND saknr EQ it_skb1-saknr
             AND waers  EQ it_skb1-waers.

      IF sy-subrc IS INITIAL AND it_zgl_saldo[] IS NOT INITIAL.
        SORT it_zgl_saldo BY bukrs saknr shkzg.
        vg_comp = abap_true.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
