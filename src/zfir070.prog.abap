*&---------------------------------------------------------------------*
*& Report  ZFIR070
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfir070.

TABLES: zhrst_efd_e1250m, zhrst_efd_e1250l, lfa1, zhrst_e1250m.


TYPES: BEGIN OF ty_head_e1250,
         bukrs   TYPE zhrst_efd_e1250m-bukrs,
         name1   TYPE t001-butxt,
         branch  TYPE zhrst_efd_e1250m-branch,
         name2   TYPE t001w-name1,
         cpf     TYPE zhrst_efd_e1250m-cpf,
         belnr   TYPE zhrst_efd_e1250m-belnr,
         gjahr   TYPE zhrst_efd_e1250m-gjahr,
         cname   TYPE zhrst_efd_e1250m-cname,
         budat   TYPE zhrst_efd_e1250m-budat,
         budat_h TYPE zhrst_efd_e1250m-budat,
         xblnr   TYPE zhrst_efd_e1250m-xblnr,
         bldat   TYPE zhrst_efd_e1250m-bldat,
         dmbtr   TYPE zhrst_efd_e1250m-dmbtr,
         vrcp    TYPE zhrst_efd_e1250m-vrcp,
         vrrat   TYPE zhrst_efd_e1250m-vrrat,
         vrsenar TYPE zhrst_efd_e1250m-vrsenar,
       END OF ty_head_e1250.

TYPES :
  BEGIN OF ty_selfield,
    index TYPE slis_selfield-tabindex,
  END OF   ty_selfield.


DATA: it_saida            TYPE TABLE OF zhrst_e1250m,
      it_saida_sel        TYPE TABLE OF zhrst_e1250m,
      wa_saida            TYPE zhrst_e1250m,
      it_saida_aux        TYPE TABLE OF zhrst_e1250m,
      wa_saida_aux        TYPE zhrst_e1250m,
      it_head_e1250       TYPE TABLE OF ty_head_e1250,
      wa_head_e1250       TYPE  ty_head_e1250,
      it_pesq             TYPE TABLE OF ty_head_e1250,
      wa_pesq             TYPE  ty_head_e1250,
      it_zhrst_efd_e1250m TYPE TABLE OF zhrst_efd_e1250m,
      wa_zhrst_efd_e1250m TYPE zhrst_efd_e1250m,
      it_e1250_log        TYPE TABLE OF zhrst_efd_e1250l,
      wa_e1250_log        TYPE zhrst_efd_e1250l,
      ck_alterar          TYPE char01.


DATA: ok_code  LIKE sy-ucomm,
      check(2) TYPE c,
      vbukrs   TYPE t001-bukrs.

CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor  IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

DATA: g_container        TYPE scrfname,
      g_grid             TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      obj_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      gt_fieldcat        TYPE lvc_t_fcat,
      g_toolbar          TYPE REF TO lcl_alv_toolbar,
      gs_layout          TYPE lvc_s_layo,
      i_selected_rows    TYPE lvc_t_row,
      tg_selectedcell    TYPE lvc_t_cell,
      wg_selectedcell    TYPE lvc_s_cell,
      tg_selectedrow     TYPE lvc_t_row,
      wg_selectedrow     TYPE lvc_s_row,
      ty_toolbar         TYPE stb_button,
      wa_stable          TYPE lvc_s_stbl,
      tg_msg_ret         TYPE TABLE OF zfiwrs0002 WITH HEADER LINE,
      it_sel_rows        TYPE lvc_t_row,
      wa_sel_rows        TYPE lvc_s_row.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

DATA: wl_repid     TYPE sy-repid,
      tl_function  TYPE ui_functions,
      wl_function  LIKE tl_function WITH HEADER LINE,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura.

DATA: abap         TYPE c,
      abap_edt     TYPE c,
      verro        TYPE c,
      dt_inicio(6) TYPE c,
      dt_fim(6)    TYPE c.


DATA: tela(4) TYPE c VALUE '0102'.

CONSTANTS: v_wt_qsshh01 TYPE p LENGTH 15 DECIMALS 3  VALUE '0.012',
           v_wt_qsshh02 TYPE p LENGTH 15 DECIMALS 3  VALUE '0.001',
           v_wt_qsshh03 TYPE p LENGTH 15 DECIMALS 3  VALUE '0.002'.  "'0.001'.

CLASS lcl_alv_toolbar IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT obj_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Atualizar
    ty_toolbar-icon      = icon_change.
    ty_toolbar-function  = 'ALTERAR'.
    ty_toolbar-quickinfo = text-001.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR: ty_toolbar.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

*    DATA: LC_ST_CARGA     TYPE CHAR01,
*          LC_ST_CARGA_EST TYPE CHAR01.
*
    DATA: et_index_rows  TYPE lvc_t_row.

    CALL METHOD g_grid->get_selected_rows
      IMPORTING
        et_index_rows = et_index_rows.

    IF et_index_rows[] IS INITIAL.
      MESSAGE 'Deve ser selecionado pelo menos um registro!' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CLEAR: it_saida_sel[].

    LOOP AT et_index_rows INTO DATA(wa_index_rows).
      READ TABLE it_saida INTO DATA(wa_saida) INDEX wa_index_rows-index.
      APPEND wa_saida TO it_saida_sel.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'ALTERAR'.
        PERFORM alterar_saidas TABLES it_saida_sel.
    ENDCASE.
  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION

SELECTION-SCREEN BEGIN OF SCREEN 0102 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b1. "WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: p_bukrs  FOR zhrst_efd_e1250m-bukrs   NO INTERVALS NO-EXTENSION MODIF ID t1 NO-DISPLAY,
                p_branch FOR zhrst_efd_e1250m-branch  NO INTERVALS NO-EXTENSION MODIF ID t1,
                p_budat  FOR zhrst_efd_e1250m-budat   NO-EXTENSION MODIF ID t1,
                p_cpf    FOR lfa1-stcd2 NO INTERVALS MODIF ID t1,
                p_belnr  FOR zhrst_efd_e1250m-belnr  NO INTERVALS MODIF ID t1,
                p_docf   FOR zhrst_efd_e1250m-xblnr  NO INTERVALS MODIF ID t1.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 0102.

INITIALIZATION.


START-OF-SELECTION.

  vbukrs = p_bukrs-low.

  wa_head_e1250-bukrs = vbukrs.

  IF wa_head_e1250-bukrs IS NOT INITIAL.
    SELECT SINGLE butxt FROM t001 INTO wa_head_e1250-name1
      WHERE bukrs EQ wa_head_e1250-bukrs.
  ELSE.
    CLEAR wa_head_e1250-name1.
  ENDIF.


  CALL SCREEN 0100.


CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed  FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_double_click.

  ENDMETHOD.

  METHOD on_data_changed.
    DATA: xforn   TYPE lfa1-lifnr,
          v_belnr TYPE ekbe-belnr.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_data_changed).
      LOOP AT it_saida INTO wa_saida.

        CHECK wa_data_changed-row_id EQ sy-tabix.

        CASE wa_data_changed-fieldname.
          WHEN 'BELNR'.
            CLEAR: verro.

            SELECT SINGLE *  FROM bkpf INTO @DATA(wa_bkpf)
              WHERE belnr EQ @wa_data_changed-value+10(10)
              AND   bukrs EQ @p_bukrs-low
              AND   gjahr EQ @p_budat-low+0(4).

            IF wa_bkpf IS INITIAL.
              MESSAGE 'NºDocumento Contábil não existe' TYPE 'I'.
              EXIT.
            ELSE.
              IF p_budat-low+0(6) <> wa_bkpf-budat+0(6).
                MESSAGE 'O Campo Dt. Lançamento deverá ser dentro do periodo informado! ' TYPE 'I'.
                verro = 'X'.
                EXIT.
              ELSE.
                wa_saida-belnr  = wa_data_changed-value.
                wa_saida-budat  = wa_bkpf-budat.
                wa_saida-bldat  = wa_bkpf-bldat.
                wa_saida-gjahr  = wa_bkpf-gjahr.

                SELECT SINGLE * FROM bsik INTO @DATA(wa_bsik)
                                WHERE bukrs  EQ @wa_head_e1250-bukrs
                                AND   belnr  EQ @wa_bkpf-belnr
                                AND   gjahr  EQ @p_budat-low+0(4).

                IF sy-subrc = 0.
                  SELECT SINGLE * FROM t001w INTO @DATA(wa_t001w)
                    WHERE werks EQ @wa_bsik-gsber.

                  wa_saida-branch  = wa_t001w-werks.
                  wa_saida-name2   = wa_t001w-name1.
                  wa_saida-xblnr   = wa_bsik-xblnr.
                  xforn            = wa_bsik-lifnr.
                ELSE.

                  SELECT SINGLE * FROM bsak INTO @DATA(wa_bsak)
                    WHERE bukrs  EQ @wa_head_e1250-bukrs
                    AND   belnr  EQ @wa_data_changed-value+10(10)
                    AND   gjahr  EQ @p_budat-low+0(4).

                  SELECT SINGLE * FROM t001w INTO wa_t001w
                    WHERE werks EQ wa_bsak-gsber.

                  wa_saida-branch  = wa_t001w-werks.
                  wa_saida-name2   = wa_t001w-name1.
                  wa_saida-xblnr   = wa_bsak-xblnr.
                  xforn            = wa_bsak-lifnr.
                ENDIF.

                SELECT SINGLE * FROM lfa1 INTO @DATA(wa_lfa1)
                  WHERE lifnr EQ @xforn.

                wa_saida-cname   = wa_lfa1-name1.
                wa_saida-cpf     = wa_lfa1-stcd2.

                MODIFY it_saida FROM wa_saida INDEX wa_data_changed-row_id.

              ENDIF.
            ENDIF.
          WHEN 'DMBTR'.

            wa_saida-dmbtr    = wa_data_changed-value.
            wa_saida-vrcp     = ( wa_saida-dmbtr * v_wt_qsshh01  ).
            wa_saida-vrrat    = ( wa_saida-dmbtr * v_wt_qsshh02 ).
            wa_saida-vrsenar  = ( wa_saida-dmbtr * v_wt_qsshh03 ).

            MODIFY it_saida FROM wa_saida INDEX wa_data_changed-row_id.
          WHEN 'VRCP'.

            wa_saida-vrcp     =  wa_data_changed-value.
            MODIFY it_saida FROM wa_saida INDEX wa_data_changed-row_id.
          WHEN 'VRRAT'.

            wa_saida-vrrat    = wa_data_changed-value.
            MODIFY it_saida FROM wa_saida INDEX wa_data_changed-row_id.
          WHEN 'VRSENAR'.

            wa_saida-vrsenar  = wa_data_changed-value.
            MODIFY it_saida FROM wa_saida INDEX wa_data_changed-row_id.
        ENDCASE.

        IF wa_saida-belnr IS NOT INITIAL.

          v_belnr = wa_saida-belnr+10(10).

          SELECT SINGLE ebeln
              FROM ekbe
              INTO @DATA(v_ebeln)
              WHERE gjahr = @wa_saida-gjahr    AND
                    belnr = @v_belnr.

          IF ( sy-subrc = 0 ).

            SELECT SINGLE bsart
            FROM ekko
            INTO @DATA(v_bsart)
            WHERE ebeln = @v_ebeln AND
                  bukrs = @wa_saida-bukrs.

          ENDIF.

        ENDIF.

        SELECT SINGLE blart
              FROM bkpf
              INTO @DATA(v_blart)
              WHERE gjahr = @wa_saida-gjahr    AND
                    belnr = @v_belnr.

*       Se documento for do Sigam, não alterar campos acquis e aquis
        IF ( v_blart <> 'SI' ).

          IF v_bsart = 'SGR' OR v_bsart = 'ZGEF'.

            wa_saida-ind_aquis = '1'.
***            wa_saida-ind_acquis = '1'.
            wa_saida-ind_acquis = ' '. "CS2021000436


***            IF  wa_saida-vrrat EQ '0.00' AND wa_saida-vrcp  EQ '0.00'.
***              wa_saida-ind_acquis = ' '.
***            ELSE.
***              wa_saida-ind_acquis = ' '.
***            ENDIF.

          ELSE.

            IF  wa_saida-vrrat EQ '0.00' AND wa_saida-vrcp  EQ '0.00'.
*          WA_SAIDA-IND_ACQUIS = '4'.
              wa_saida-ind_aquis = '4'.
              MODIFY it_saida FROM wa_saida INDEX wa_data_changed-row_id.
            ELSE.
              wa_saida-ind_acquis = ' '. "CS2021000436
***              wa_saida-ind_acquis = '1'.
              wa_saida-ind_aquis = '1'.
              MODIFY it_saida FROM wa_saida INDEX wa_data_changed-row_id.
            ENDIF.

          ENDIF.

        ENDIF.


        CLEAR: v_ebeln, v_bsart, v_belnr.

      ENDLOOP.
    ENDLOOP.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.

  METHOD on_data_changed_finished.

    IF verro = 'X'.
      CLEAR it_saida[].
    ENDIF.

    IF ok_code = '&EDIT'.
      CLEAR wa_saida_aux.

      LOOP AT et_good_cells INTO DATA(wa_good_cell).
        READ TABLE it_saida INTO wa_saida INDEX wa_good_cell-row_id.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING wa_saida TO wa_saida_aux.
          APPEND wa_saida_aux TO it_saida_aux.
        ENDIF.
      ENDLOOP.

      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.

    IF ok_code = '&INS'.
      CLEAR wa_saida_aux.

      LOOP AT et_good_cells INTO wa_good_cell.
        READ TABLE it_saida INTO wa_saida INDEX wa_good_cell-row_id.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING wa_saida TO wa_saida_aux.
          APPEND wa_saida_aux TO it_saida_aux.
        ENDIF.
      ENDLOOP.

      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.

  ENDMETHOD.

  METHOD on_button_click.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.
ENDCLASS.

FORM seleciona_dados.
  FREE: it_zhrst_efd_e1250m[], wa_zhrst_efd_e1250m, it_saida[], wa_saida.
*
  IF wa_head_e1250-bukrs IS INITIAL.
    MESSAGE 'Favor informar a empresa!' TYPE 'I'.
    EXIT.
  ELSEIF  p_budat[] IS INITIAL.
    MESSAGE 'Favor informar Data de Lançamento!' TYPE 'I'.
    EXIT.
  ELSE.

    dt_inicio = p_budat-low+0(6).
    dt_fim    = p_budat-high+0(6).

    IF dt_inicio <> dt_fim.
      MESSAGE 'O intervalo de data deverá ser dentro do mês!' TYPE 'I'.
      EXIT.
    ELSE.

      SELECT * FROM zhrst_efd_e1250m  INTO TABLE it_zhrst_efd_e1250m
       WHERE  ( bukrs EQ wa_head_e1250-bukrs
         AND    budat IN p_budat )
         AND   ( branch IN p_branch AND  belnr  IN p_belnr AND
                xblnr  IN p_docf   AND  cpf    IN p_cpf   ).


      LOOP AT it_zhrst_efd_e1250m INTO wa_zhrst_efd_e1250m.

        SELECT SINGLE * FROM t001w INTO @DATA(wa_t001w)
          WHERE werks EQ @wa_zhrst_efd_e1250m-branch.


        wa_saida-bukrs       = wa_zhrst_efd_e1250m-bukrs.
        wa_saida-branch      = wa_zhrst_efd_e1250m-branch.
        wa_saida-name2       = wa_t001w-name1.
        wa_saida-cpf         = wa_zhrst_efd_e1250m-cpf.
        wa_saida-belnr       = wa_zhrst_efd_e1250m-belnr.
        wa_saida-gjahr       = wa_zhrst_efd_e1250m-gjahr.
        wa_saida-cname       = wa_zhrst_efd_e1250m-cname.
        wa_saida-budat       = wa_zhrst_efd_e1250m-budat.
        wa_saida-xblnr       = wa_zhrst_efd_e1250m-xblnr.
        wa_saida-bldat       = wa_zhrst_efd_e1250m-bldat.
        wa_saida-ind_aquis   = wa_zhrst_efd_e1250m-ind_aquis.
        wa_saida-dmbtr       = wa_zhrst_efd_e1250m-dmbtr.
        wa_saida-vrcp        = wa_zhrst_efd_e1250m-vrcp.
        wa_saida-vrrat       = wa_zhrst_efd_e1250m-vrrat.
        wa_saida-vrsenar     = wa_zhrst_efd_e1250m-vrsenar.
        wa_saida-ind_acquis  = wa_zhrst_efd_e1250m-ind_acquis.

        APPEND wa_saida TO it_saida.

        CLEAR: wa_saida, wa_zhrst_efd_e1250m,  wa_t001w.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: ls_fcat     TYPE lvc_s_fcat,
        pt_fieldcat TYPE lvc_t_fcat.

  SET PF-STATUS 'STATUS'.
  SET TITLEBAR 'TITULO'.

  CLEAR pt_fieldcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZHRST_E1250M'
    CHANGING
      ct_fieldcat      = pt_fieldcat.

  CLEAR ls_fcat.

  LOOP AT pt_fieldcat INTO ls_fcat.
    IF ls_fcat-fieldname EQ 'BUKRS'.
      ls_fcat-col_pos = 1.
      ls_fcat-outputlen = 07.
      ls_fcat-coltext = 'Empresa'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'BRANCH'.
      ls_fcat-col_pos = 3.
      ls_fcat-outputlen = 04.
      ls_fcat-coltext = 'Filial'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'NAME2'.
      ls_fcat-col_pos = 4.
      ls_fcat-outputlen = 30.
      ls_fcat-coltext = 'Descrição'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'CPF'.
      ls_fcat-col_pos = 5.
      ls_fcat-outputlen = 10.
      ls_fcat-coltext = 'CPF'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'CNAME'.
      ls_fcat-col_pos = 6.
      ls_fcat-outputlen = 20.
      ls_fcat-coltext = 'Nome'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'BELNR'.
      ls_fcat-col_pos = 7.
      ls_fcat-edit = abap.
      ls_fcat-outputlen = 10.
      ls_fcat-coltext = 'Nº Documento'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'GJAHR'.
      ls_fcat-col_pos = 8.
      ls_fcat-outputlen = 6.
      ls_fcat-coltext = 'Exercício'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'BUDAT'.
      ls_fcat-col_pos = 9.
      ls_fcat-outputlen = 9.
      ls_fcat-coltext = 'Data Lanç'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'BLDAT'.
      ls_fcat-col_pos = 10.
      ls_fcat-outputlen = 9.
      ls_fcat-coltext = 'Data Doc'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'XBLNR'.
      ls_fcat-col_pos = 11.
      ls_fcat-outputlen = 10.
      ls_fcat-coltext = 'Nº NF'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'DMBTR'.
      ls_fcat-col_pos = 12.
      ls_fcat-edit = abap_edt.
      ls_fcat-outputlen = 10.
      ls_fcat-coltext = 'Valor NF'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'VRCP'.
      ls_fcat-col_pos = 13.
      ls_fcat-edit = abap_edt.
      ls_fcat-outputlen = 10.
      ls_fcat-coltext = 'Valor Funrural'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'VRRAT'.
      ls_fcat-col_pos = 14.
      ls_fcat-edit = abap_edt.
      ls_fcat-outputlen = 10.
      ls_fcat-coltext = 'Valor RAT'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'VRSENAR'.
      ls_fcat-col_pos = 15.
      ls_fcat-edit = abap_edt.
      ls_fcat-outputlen = 10.
      ls_fcat-coltext = 'Valor SENAR'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'IND_AQUIS'.
      ls_fcat-col_pos = 17.
      ls_fcat-outputlen = 07.
      ls_fcat-lzero     = abap_true.
      ls_fcat-coltext = 'Ind.Aquisicao'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'IND_ACQUIS'.
      ls_fcat-col_pos = 16.
      ls_fcat-outputlen = 10.
      ls_fcat-coltext = 'Ind.Opcao'.
      ls_fcat-edit = abap_edt.
      MODIFY pt_fieldcat FROM ls_fcat.
    ENDIF.
  ENDLOOP.


  IF g_container IS INITIAL.

    wa_stable = 'X'.
    g_container = 'CONTAINER01'.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = g_custom_container.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    CALL METHOD cl_gui_cfw=>flush.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
      CHANGING
        it_fieldcatalog      = pt_fieldcat
        it_outtab            = it_saida.

    SET HANDLER:
      lcl_event_handler=>on_button_click FOR g_grid,
      lcl_event_handler=>on_data_changed FOR g_grid,
      lcl_event_handler=>on_data_changed_finished FOR g_grid.

    CREATE OBJECT g_toolbar
      EXPORTING
        io_alv_grid = g_grid.

    SET HANDLER g_toolbar->on_toolbar FOR g_grid.
    SET HANDLER g_toolbar->handle_user_command FOR g_grid.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ELSE.
    CALL METHOD g_grid->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = pt_fieldcat.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: wl_insert LIKE LINE OF it_saida.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SALVAR'.
      IF ok_code IS NOT INITIAL.
        PERFORM salvar_dados.
      ENDIF.
    WHEN '&PESQ'.
      ok_code = '&PESQ'.
      PERFORM seleciona_dados.
    WHEN '&INS'.
      IF wa_head_e1250-bukrs IS INITIAL.
        MESSAGE 'Favor informar a empresa!' TYPE 'I'.
        EXIT.
      ELSEIF  p_budat[] IS INITIAL.
        MESSAGE 'Favor informar Data de Lançamento!' TYPE 'I'.
        EXIT.
      ELSE.
        CLEAR:  dt_fim, dt_inicio.

        dt_inicio = p_budat-low+0(6).
        dt_fim    = p_budat-high+0(6).

        IF dt_inicio <> dt_fim.
          MESSAGE 'O intervalo de data deverá ser dentro do mês!' TYPE 'I'.
          EXIT.
        ELSE.
          FREE it_saida[].
          abap     = 'X'.
          abap_edt = 'X'.
          ok_code  = '&INS'.

          wl_insert-bukrs = wa_head_e1250-bukrs.

          APPEND wl_insert TO it_saida.
        ENDIF.
      ENDIF.
    WHEN '&EDIT'.
      IF wa_head_e1250-bukrs IS INITIAL.
        MESSAGE 'Favor informar a empresa!' TYPE 'I'.
        EXIT.
      ELSEIF  p_budat[] IS INITIAL.
        MESSAGE 'Favor informar Data de Lançamento!' TYPE 'I'.
        EXIT.
      ELSE.
        CLEAR:  dt_fim, dt_inicio.

        dt_inicio = p_budat-low+0(6).
        dt_fim    = p_budat-high+0(6).

        IF dt_inicio <> dt_fim.
          MESSAGE 'O intervalo de data deverá ser dentro do mês!' TYPE 'I'.
          EXIT.
        ELSE.
          abap = ' '.
          abap_edt = 'X'.
          ok_code = '&EDIT'.
        ENDIF.
      ENDIF.
    WHEN '&DEL'.
      IF wa_head_e1250-bukrs IS INITIAL.
        MESSAGE 'Favor informar a empresa!' TYPE 'I'.
        EXIT.
      ELSEIF  p_budat[] IS INITIAL.
        MESSAGE 'Favor informar Data de Lançamento!' TYPE 'I'.
        EXIT.
      ELSE.
        CLEAR:  dt_fim, dt_inicio.

        dt_inicio = p_budat-low+0(6).
        dt_fim    = p_budat-high+0(6).

        IF dt_inicio <> dt_fim.
          MESSAGE 'O intervalo de data deverá ser dentro do mês!' TYPE 'I'.
          EXIT.
        ELSE.
          PERFORM exclusao.
        ENDIF.
      ENDIF.

    WHEN '&LOG'.

      IF wa_head_e1250-bukrs IS INITIAL.
        MESSAGE 'Favor informar a empresa!' TYPE 'I'.
        EXIT.
      ELSEIF  p_budat[] IS INITIAL.
        MESSAGE 'Favor informar Data de Lançamento!' TYPE 'I'.
        EXIT.
      ELSE.

        CALL METHOD g_grid->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

        READ TABLE it_saida INTO wa_saida INDEX   wa_sel_rows-index.

        CHECK sy-subrc EQ 0.

        SELECT * FROM zhrst_efd_e1250l INTO TABLE @DATA(it_zhrst_efd_e1250l)
          WHERE bukrs  EQ @wa_saida-bukrs
          AND   gjahr  EQ @wa_saida-gjahr
          AND   belnr  EQ @wa_saida-belnr+10(10)
          AND   branch EQ @wa_saida-branch.

        CHECK it_zhrst_efd_e1250l[] IS NOT INITIAL.

        SORT it_zhrst_efd_e1250l BY data_atual hora_atual.

        PERFORM monta_alv_log.


        CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
            it_fieldcat           = estrutura[]
            i_save                = 'A'
            i_screen_start_column = 3
            i_screen_start_line   = 3
            i_screen_end_column   = 100
            i_screen_end_line     = 13
          TABLES
            t_outtab              = it_zhrst_efd_e1250l.


      ENDIF.

  ENDCASE.

ENDMODULE.


FORM monta_alv_log.
  REFRESH estrutura[].
  PERFORM monta_estrutura USING:
    1 '' ''      'IT_ZHRST_EFD_E1250L'      'BUKRS'        'Empresa'        '07' '',
    1 '' ''      'IT_ZHRST_EFD_E1250L'      'BRANCH'       'Filial'         '07' '',
    1 '' ''      'IT_ZHRST_EFD_E1250L'      'BELNR'        'Nº Documento'   '10' '',
    1 '' ''      'IT_ZHRST_EFD_E1250L'      'GJAHR'        'Ano'            '04' '',
    1 '' ''      'IT_ZHRST_EFD_E1250L'      'USNAM'        'Usuário'        '10' '',
    1 '' ''      'IT_ZHRST_EFD_E1250L'      'DATA_ATUAL'   'Data'           '10' '',
    1 '' ''      'IT_ZHRST_EFD_E1250L'      'HORA_ATUAL'   'Hora'           '10' '',
    1 '' ''      'IT_ZHRST_EFD_E1250L'      'SISTEMA'      'Sistema'        '06' '',
    1 '' ''      'IT_ZHRST_EFD_E1250L'      'ACAO'         'Ação'           '10' '' .

ENDFORM.

FORM monta_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit).

  CLEAR: wa_estrutura.


  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-outputlen = p_outputlen.


  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.



FORM salvar_dados.

  FREE: it_zhrst_efd_e1250m[], wa_zhrst_efd_e1250m.

  IF ok_code = '&EDIT'.

    IF wa_saida_aux IS NOT INITIAL.

      IF wa_saida_aux-xblnr EQ ' '.
        MESSAGE 'Campo Valor NF obrigatório! ' TYPE 'I'.
        EXIT.
      ELSEIF wa_saida_aux-vrsenar EQ ' '.
        MESSAGE 'Campo Valor SENAR obrigatório! ' TYPE 'I'.
        EXIT.
      ELSEIF ( wa_saida_aux-vrrat = '0.00' AND wa_saida_aux-vrcp <> '0.00' ) OR
             ( wa_saida_aux-vrrat <> '0.00' AND wa_saida_aux-vrcp = '0.00' ).
        MESSAGE 'Para gravar o valor do Funrural e RAT os dois devem esta com valores ou zerado !' TYPE 'I'.
        EXIT.
      ELSE.
        LOOP AT it_saida_aux INTO wa_saida_aux.
          UPDATE zhrst_efd_e1250m SET dmbtr      = wa_saida_aux-dmbtr
                                      vrcp       = wa_saida_aux-vrcp
                                      vrrat      = wa_saida_aux-vrrat
                                      vrsenar    = wa_saida_aux-vrsenar
                                      ind_aquis  = wa_saida_aux-ind_aquis
                                      ind_acquis = wa_saida_aux-ind_acquis
                     WHERE bukrs  EQ wa_saida_aux-bukrs
                      AND  branch EQ wa_saida_aux-branch
                      AND  cpf    EQ wa_saida_aux-cpf
                      AND  belnr  EQ wa_saida_aux-belnr
                      AND  gjahr  EQ wa_saida_aux-gjahr.

          PERFORM log.
        ENDLOOP.

        MESSAGE 'Dados gravado com sucesso !' TYPE 'I'.
        CLEAR ok_code.
        abap     = ' '.
        abap_edt = ' '.
        FREE: it_saida.
        PERFORM seleciona_dados.
      ENDIF.
    ENDIF.

  ELSEIF ok_code = '&INS'.

    PERFORM validacoes.

    IF check  = 'OK'.
      APPEND wa_saida_aux TO it_saida.
      IF wa_saida_aux-branch = '9121'.
        wa_saida_aux-branch = '0121'.
      ENDIF.
      MOVE-CORRESPONDING wa_saida_aux TO wa_zhrst_efd_e1250m.
      PERFORM log.
      MODIFY zhrst_efd_e1250m FROM  wa_zhrst_efd_e1250m.
      MESSAGE 'Dados gravado com sucesso !' TYPE 'I'.
      CLEAR ok_code.
      abap     = ' '.
      abap_edt = ' '.

      FREE: it_saida.
      PERFORM seleciona_dados.

    ENDIF.
  ENDIF.

ENDFORM.

FORM log.
  MOVE-CORRESPONDING wa_saida_aux TO wa_e1250_log.

  wa_e1250_log-data_atual    = sy-datum.
  wa_e1250_log-hora_atual    = sy-uzeit.
  wa_e1250_log-usnam         = sy-uname.
  wa_e1250_log-sistema       = 'SAP'.

  IF ok_code = '&INS'.
    wa_e1250_log-acao          = 'INSERT'.
  ELSEIF ok_code = '&EDIT'.
    wa_e1250_log-acao          = 'EDIT'.
  ELSEIF ok_code = '&DEL'.
    wa_e1250_log-acao          = 'DELE'.
  ENDIF.

  MODIFY zhrst_efd_e1250l FROM wa_e1250_log.
ENDFORM.


FORM validacoes.

  IF wa_saida_aux-ind_aquis = '4'.

    IF wa_saida_aux-dmbtr EQ ' '.
      MESSAGE 'Campo Valor NF obrigatório!' TYPE 'I'.
      EXIT.
    ELSEIF wa_saida_aux-vrsenar EQ ' '.
      MESSAGE 'Campo Valor SENAR obrigatório!' TYPE 'I'.
      EXIT.
    ELSE.
      check = 'OK'.
    ENDIF.

  ELSEIF wa_saida_aux-ind_aquis = '1'.

    IF wa_saida_aux-dmbtr EQ ' '.
      MESSAGE 'Campo Valor NF obrigatório!' TYPE 'I'.
      EXIT.
    ELSEIF wa_saida_aux-vrsenar EQ ' '.
      MESSAGE 'Campo Valor SENAR obrigatório!' TYPE 'I'.
      EXIT.
    ELSEIF wa_saida_aux-vrcp EQ ' '.
      MESSAGE 'Campo Valor Funrural obrigatório!' TYPE 'I'.
      EXIT.
    ELSEIF wa_saida_aux-vrrat EQ ' '.
      MESSAGE 'Campo Valor RAT obrigatório!' TYPE 'I'.
      EXIT.
    ELSE.
      check = 'OK'.
    ENDIF.

  ENDIF.

ENDFORM.

FORM exclusao.

  ok_code = '&DEL'.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  IF it_sel_rows[] IS INITIAL.
    MESSAGE 'Favor selecionar uma linha!' TYPE 'I'.
    EXIT.
  ENDIF.

  LOOP AT  it_sel_rows INTO wa_sel_rows.
    READ TABLE it_saida INTO wa_saida INDEX wa_sel_rows-index.

    DELETE FROM zhrst_efd_e1250m WHERE bukrs   = wa_saida-bukrs  AND
                                       branch  = wa_saida-branch AND
                                       cpf     = wa_saida-cpf    AND
                                       belnr   = wa_saida-belnr  AND
                                       gjahr   = wa_saida-gjahr.

    MOVE-CORRESPONDING wa_saida  TO wa_e1250_log.

    wa_e1250_log-data_atual    = sy-datum.
    wa_e1250_log-hora_atual    = sy-uzeit.
    wa_e1250_log-usnam         = sy-uname.
    wa_e1250_log-sistema       = 'SAP'.

    IF ok_code = '&INS'.
      wa_e1250_log-acao          = 'INSERT'.
    ELSEIF ok_code = '&EDIT'.
      wa_e1250_log-acao          = 'EDIT'.
    ELSEIF ok_code = '&DEL'.
      wa_e1250_log-acao          = 'DELE'.
    ENDIF.

    MODIFY zhrst_efd_e1250l FROM wa_e1250_log.

    CLEAR: wa_saida, wa_e1250_log.

  ENDLOOP.

  CLEAR wa_saida.
  REFRESH it_saida.
  PERFORM seleciona_dados.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  MODIFY_FORMA_LOTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_forma INPUT.
  wa_head_e1250-bukrs = vbukrs.

  IF wa_head_e1250-bukrs IS NOT INITIAL.
    SELECT SINGLE butxt FROM t001 INTO wa_head_e1250-name1
      WHERE bukrs EQ wa_head_e1250-bukrs.
  ELSE.
    CLEAR wa_head_e1250-name1.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ALTERAR_SAIDAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SAIDA_SEL  text
*----------------------------------------------------------------------*
FORM alterar_saidas  TABLES p_it_saida STRUCTURE zhrst_e1250m.

  CLEAR ck_alterar.

  "Alimenta os campos da tela 9001 com o valor dos primeiros registros
  CLEAR: zhrst_e1250m-ind_acquis, zhrst_e1250m-ind_aquis.
  READ TABLE p_it_saida INTO DATA(w_saida_aux) INDEX 1.
  IF sy-subrc IS INITIAL.
    zhrst_e1250m-ind_acquis = w_saida_aux-ind_acquis.
    zhrst_e1250m-ind_aquis = w_saida_aux-ind_aquis.
  ENDIF.

  CALL SCREEN 9001 STARTING AT 05 05.

  IF ck_alterar EQ abap_true.

    SELECT * INTO TABLE @DATA(it_original)
      FROM zhrst_efd_e1250m
       FOR ALL ENTRIES IN @it_saida_sel
     WHERE bukrs  EQ @it_saida_sel-bukrs
       AND branch EQ @it_saida_sel-branch
       AND cpf    EQ @it_saida_sel-cpf
       AND belnr  EQ @it_saida_sel-belnr
       AND gjahr  EQ @it_saida_sel-gjahr.

    CHECK sy-subrc IS INITIAL.

    LOOP AT it_original ASSIGNING FIELD-SYMBOL(<original>).
      <original>-ind_acquis = zhrst_e1250m-ind_acquis.
      <original>-ind_aquis = zhrst_e1250m-ind_aquis.
    ENDLOOP.

    MODIFY zhrst_efd_e1250m FROM TABLE it_original.
    COMMIT WORK AND WAIT.

    LOOP AT it_original INTO DATA(wa_original).
      READ TABLE it_saida
      WITH KEY bukrs   = wa_original-bukrs
               branch  = wa_original-branch
               cpf     = wa_original-cpf
               belnr   = wa_original-belnr
               gjahr   = wa_original-gjahr
      ASSIGNING FIELD-SYMBOL(<ajuste>).

      IF sy-subrc IS INITIAL.
        <ajuste>-ind_acquis = zhrst_e1250m-ind_acquis.
        <ajuste>-ind_aquis = zhrst_e1250m-ind_aquis.
      ENDIF.
    ENDLOOP.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable      = VALUE #( row = abap_true col = abap_true )
        i_soft_refresh = abap_true.

    MESSAGE 'Registro(s) alterado(s)!' TYPE 'S'.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001_exit INPUT.
  CLEAR: ck_alterar.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'PF9001'.
  SET TITLEBAR 'TL9001'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  CASE ok_code.
    WHEN 'ALTERAR'.
      ck_alterar = abap_true.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
