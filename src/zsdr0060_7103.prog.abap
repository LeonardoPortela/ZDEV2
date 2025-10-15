*----------------------------------------------------------------------*
***INCLUDE ZSDR0060_7103.
*----------------------------------------------------------------------*

DATA:
  gs_layout_7103_alv1  TYPE lvc_s_layo,
  it_fieldcatalog_7103 TYPE lvc_t_fcat,
  it_exclude_7103      TYPE ui_functions.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler_7103 DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:

      "FF #154847 - inicio
      toolbar_7103 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,
      "FF #154847 - fim

      user_command_7103 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      data_changed_finished_7103 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed.

*      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
*        IMPORTING et_good_cells.




*
*      handle_hotspot_click_5230  FOR EVENT hotspot_click OF cl_gui_alv_grid
*        IMPORTING e_row_id e_column_id,
*
*      on_double_click_5230 FOR EVENT double_click OF cl_gui_alv_grid
*        IMPORTING e_row e_column,

*      toolbar_5230_alv2 FOR EVENT toolbar OF  cl_gui_alv_grid
*        IMPORTING e_object,
*
*      on_f4_5230 FOR EVENT onf4 OF cl_gui_alv_grid
*        IMPORTING sender e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

CLASS lcl_event_handler_7103 IMPLEMENTATION.

  "FF #154847 - inicio
  METHOD toolbar_7103. "Popup
    DATA wa_tool TYPE stb_button.

    wa_tool-function = 'CRIAR_ITI'.
    wa_tool-icon     = '@AW@'. "Icon car
    wa_tool-quickinfo = 'Criar Itinerário'.
    wa_tool-text = 'Criar Itinerário'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    " Botão "Eliminar Linha" no índice 12 (No mesmo ponto onde era o botão standard) "O standard estava com dump type x ao tratar a linha eliminada.
    wa_tool-function = 'ELIMINAR_LINHA'.
    wa_tool-icon     = '@18@'. "Delete line
    wa_tool-quickinfo = 'Eliminar linha'.
*    wa_tool-text = ''.
    INSERT wa_tool INTO e_object->mt_toolbar INDEX 12.
    CLEAR wa_tool.

  ENDMETHOD.
  "FF #154847 - fim

*  METHOD data_changed_finished.
***   restaurando os dados
**    IF l_deleted = 'X'.
**      gt_solic_frete[] = gt_solic_frete_aux[].
**      CLEAR: l_deleted, gt_solic_frete_aux.
**      REFRESH gt_solic_frete_aux.
**    ENDIF.
***   refresh
**    CALL METHOD ctl_alv1_7103->refresh_table_display.
*
*  ENDMETHOD.

  METHOD data_changed_finished_7103.

    DATA: wa_good_cells TYPE lvc_s_modi,

          wa_carga_5230 TYPE ty_carga_5230,
          wa_lfa1       TYPE lfa1,
          vl_check      TYPE char1,
          lv_valid      TYPE c,
          lv_resposta   TYPE c.


    DATA(lt_cargas) = gt_zsdt0346.
    SORT lt_cargas BY nro_carga.
    DELETE ADJACENT DUPLICATES FROM lt_cargas COMPARING nro_carga.

    LOOP AT  er_data_changed->mt_mod_cells INTO wa_good_cells.
      READ TABLE gt_solic_frete ASSIGNING FIELD-SYMBOL(<fs_frete>) INDEX wa_good_cells-row_id.
      IF sy-subrc IS INITIAL.

        IF wa_good_cells-fieldname EQ 'OPERACAO' AND wa_good_cells-value IS NOT INITIAL.

          SELECT operacao,tipo_transporte
            FROM zsdt0345
            INTO @DATA(ls_operacao)
            UP TO 1 ROWS
            WHERE operacao = @wa_good_cells-value.
          ENDSELECT.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE 'Operação inválida, favor escolher através da ajuda de pesquisa' TYPE 'I'.
            EXIT.
          ELSE.

            IF sy-subrc IS INITIAL.
              <fs_frete>-tipo_transporte = ls_operacao-tipo_transporte.

              CALL METHOD ctl_alv1_7103->refresh_table_display.
            ENDIF.
          ENDIF.

        ELSEIF ( wa_good_cells-fieldname EQ 'PONTO_COLETA'
           AND   wa_good_cells-value IS NOT INITIAL
           AND   <fs_frete>-local_entrega IS NOT INITIAL ) OR
               ( wa_good_cells-fieldname EQ 'LOCAL_ENTREGA'
           AND   wa_good_cells-value IS NOT INITIAL
           AND   <fs_frete>-ponto_coleta IS NOT INITIAL ).

          IF wa_good_cells-fieldname EQ 'PONTO_COLETA'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_good_cells-value
              IMPORTING
                output = <fs_frete>-ponto_coleta.
          ELSEIF wa_good_cells-fieldname EQ 'LOCAL_ENTREGA'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_good_cells-value
              IMPORTING
                output = <fs_frete>-local_entrega.
          ENDIF.

          SELECT SINGLE lzone
            FROM lfa1
            INTO @DATA(lv_azone)
            WHERE lifnr = @<fs_frete>-ponto_coleta.
          IF sy-subrc IS INITIAL.
            SELECT SINGLE lzone
              FROM kna1
              INTO @DATA(lv_lzone)
              WHERE kunnr = @<fs_frete>-local_entrega.
            IF sy-subrc IS INITIAL.
              SELECT a~route,b~bezei
                FROM trolz AS a
                INNER JOIN tvrot AS b
                ON a~route = b~route
                UP TO 1 ROWS
                INTO @DATA(ls_rota)
                WHERE a~lzone = @lv_lzone
                  AND a~azone = @lv_azone
                  AND b~spras = @sy-langu.
              ENDSELECT.
              IF sy-subrc IS INITIAL.
                <fs_frete>-itinerario = ls_rota-route.
                <fs_frete>-desc_itinerario = ls_rota-bezei.
                <fs_frete>-usuario = sy-uname.
                <fs_frete>-data    = sy-datum.
                <fs_frete>-hora    = sy-uzeit.
                CALL METHOD ctl_alv1_7103->refresh_table_display.

                LOOP AT lt_cargas ASSIGNING FIELD-SYMBOL(<fs_cargas>).
                  APPEND INITIAL LINE TO gt_zsdt0346 ASSIGNING FIELD-SYMBOL(<fs_zsdt0346>).
                  MOVE-CORRESPONDING <fs_frete> TO <fs_zsdt0346>.

                  <fs_zsdt0346>-nro_carga = <fs_cargas>-nro_carga.
                ENDLOOP.

              ELSE.
                CALL FUNCTION 'POPUP_TO_CONFIRM'
                  EXPORTING
*                   TITLEBAR       = ' '
*                   DIAGNOSE_OBJECT             = ' '
                    text_question  = 'Itinerário não criado, deseja criar?'
                    text_button_1  = 'Sim'
                    text_button_2  = 'Não'
                  IMPORTING
                    answer         = lv_resposta
                  EXCEPTIONS
                    text_not_found = 1
                    OTHERS         = 2.
                IF sy-subrc = 0.

                  IF lv_resposta EQ '1'.

                    CLEAR: <fs_frete>-itinerario,
                           <fs_frete>-desc_itinerario.
                    CALL METHOD ctl_alv1_7103->refresh_table_display.

                    SET PARAMETER ID 'COD_PC'  FIELD <fs_frete>-ponto_coleta.
                    SET PARAMETER ID 'COD_CLI' FIELD <fs_frete>-local_entrega.

                    CALL TRANSACTION 'ZLES0214'.

                    SELECT a~route b~bezei
                      FROM trolz AS a
                      INNER JOIN tvrot AS b
                      ON a~route = b~route
                      UP TO 1 ROWS
                      INTO ls_rota
                      WHERE a~lzone = lv_lzone
                        AND a~azone = lv_azone
                        AND b~spras = sy-langu.
                    ENDSELECT.
                    IF sy-subrc IS INITIAL.
                      <fs_frete>-itinerario = ls_rota-route.
                      <fs_frete>-desc_itinerario = ls_rota-bezei.
                      <fs_frete>-usuario = sy-uname.
                      <fs_frete>-data    = sy-datum.
                      <fs_frete>-hora    = sy-uzeit.
                      CALL METHOD ctl_alv1_7103->refresh_table_display.

                      LOOP AT lt_cargas ASSIGNING <fs_cargas>.
                        APPEND INITIAL LINE TO gt_zsdt0346 ASSIGNING <fs_zsdt0346>.
                        MOVE-CORRESPONDING <fs_frete> TO <fs_zsdt0346>.

                        <fs_zsdt0346>-nro_carga = <fs_cargas>-nro_carga.
                      ENDLOOP.

                    ENDIF.

                  ENDIF.

                ENDIF.

              ENDIF.

            ELSE.

              MESSAGE 'Local de entrega inválido' TYPE 'S' DISPLAY LIKE 'E'.

            ENDIF.

          ELSE.

            MESSAGE 'Ponto de Coleta inválido' TYPE 'S' DISPLAY LIKE 'E'.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD user_command_7103.

    DATA: it_selected_rows  TYPE lvc_t_row,
          t_row_no          TYPE lvc_t_roid,
          wa_selected_rows  TYPE lvc_s_row,
          it_zsdt0129       TYPE STANDARD TABLE OF zsdt0129,
          wa_zsdt0129       TYPE zsdt0129,
          it_zsdt0133       TYPE STANDARD TABLE OF zsdt0133,
          wa_zsdt0133       TYPE zsdt0133,
          it_carga_aux_5230 TYPE STANDARD TABLE OF ty_carga_5230,
          wa_carga_5230     TYPE ty_carga_5230,
          it_sol_aux_5230   TYPE STANDARD TABLE OF ty_sol_5230,
          wa_sol_aux_5230   TYPE ty_sol_5230,
          wa_sol_5230       TYPE ty_sol_5230,
          it_rsparams       TYPE TABLE OF rsparams,
          wa_rsparams       TYPE rsparams,
          it_zsdt0062       TYPE STANDARD TABLE OF zsdt0062,
          wa_zsdt0062       TYPE zsdt0062,
          vl_lines          TYPE i,
          vl_spart          TYPE zsdt0131-spart,
          vl_vkorg          TYPE zsdt0131-vkorg,
          vl_check          TYPE char1,
          vl_check2         TYPE char1,
          vl_block          TYPE sy-tabix,
          vl_vinc1          TYPE zsdt0131-qtd_vinc,
          vl_vinc2          TYPE zsdt0131-qtd_vinc,
          wa_edit_cg_5230   TYPE ty_edit_cg_5230,
          vl_cont           TYPE i,
          vl_index          TYPE i,
          wa_sol_click_5230 TYPE ty_sol_5230,
          answer,
          lt_zsdt0346       TYPE TABLE OF zsdt0346.

    wa_stable_5230-row = 'X'.
    wa_stable_5230-col = 'X'.


    CLEAR: it_selected_rows, wa_selected_rows, vl_lines.

    IF e_ucomm = 'EDITAR'.

      CALL METHOD ctl_alv1_7103->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      LOOP AT it_selected_rows INTO wa_selected_rows.
*        READ TABLE it_carga_5230 INTO wa_carga_5230 INDEX wa_selected_rows-index.

      ENDLOOP.
    ENDIF.


    "FF #154847 - inicio
    IF e_ucomm = 'CRIAR_ITI'.

      CALL METHOD ctl_alv1_7103->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.


      DATA(lv_lines) = lines( it_selected_rows ).

      IF lv_lines > 1.

        MESSAGE 'Selecione apenas 1 linha.' TYPE 'S' DISPLAY LIKE 'E'.

      ELSE.

        LOOP AT it_selected_rows INTO wa_selected_rows.
          READ TABLE gt_solic_frete INTO DATA(wa_solic_frete) INDEX wa_selected_rows-index.
          CHECK sy-subrc = 0.

          IF wa_solic_frete-itinerario IS NOT INITIAL.
            MESSAGE 'Já existe itinerario para esta linha.' TYPE 'S' DISPLAY LIKE 'E'.
            CONTINUE.
          ENDIF.

          CLEAR: wa_solic_frete-itinerario,
                 wa_solic_frete-desc_itinerario.

          CALL METHOD ctl_alv1_7103->refresh_table_display.

          SELECT SINGLE lifnr
                 FROM lfa1
                 INTO @DATA(lv_fornec)
                 WHERE lzone = @wa_solic_frete-ponto_coleta.

          IF sy-subrc IS INITIAL.

            SELECT SINGLE kunnr
              FROM kna1
              INTO @DATA(lv_cliente)
              WHERE lzone = @wa_solic_frete-local_entrega.

            IF sy-subrc <> 0.
              CLEAR lv_cliente.
            ENDIF.

          ELSE.

            CLEAR lv_fornec. "lv_azone.

          ENDIF.

          SET PARAMETER ID 'COD_PC'  FIELD lv_fornec.
          SET PARAMETER ID 'COD_CLI' FIELD lv_cliente.

          CALL TRANSACTION 'ZLES0214'.

          SELECT a~route, b~bezei
            FROM trolz AS a
            INNER JOIN tvrot AS b
            ON a~route = b~route
            UP TO 1 ROWS
            INTO @DATA(ls_rota)
            WHERE a~lzone = @wa_solic_frete-local_entrega "Cliente
              AND a~azone = @wa_solic_frete-ponto_coleta  "Fornecedor
              AND b~spras = @sy-langu.
          ENDSELECT.

          IF sy-subrc IS INITIAL.

            wa_solic_frete-itinerario = ls_rota-route.
            wa_solic_frete-desc_itinerario = ls_rota-bezei.
            wa_solic_frete-usuario = sy-uname.
            wa_solic_frete-data    = sy-datum.
            wa_solic_frete-hora    = sy-uzeit.

            CALL METHOD ctl_alv1_7103->refresh_table_display.

*            LOOP AT lt_cargas ASSIGNING <fs_cargas>.
*              APPEND INITIAL LINE TO gt_zsdt0346 ASSIGNING <fs_zsdt0346>.
*              MOVE-CORRESPONDING <fs_frete> TO <fs_zsdt0346>.
*
*              <fs_zsdt0346>-nro_carga = <fs_cargas>-nro_carga.
*            ENDLOOP.

          ENDIF.

        ENDLOOP.

      ENDIF.


    ENDIF.


    IF e_ucomm = 'ELIMINAR_LINHA'.

      "FF #154847 - inicio
      SELECT *
        FROM tvarvc
        INTO TABLE @DATA(lt_tvarv)
        WHERE name LIKE 'Z_TIPO_OPER%'
          AND type = 'S'
          AND numb = '0000'.

      IF sy-subrc <> 0.
        CLEAR lt_tvarv[].
      ENDIF.

      CALL METHOD ctl_alv1_7103->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      CLEAR lv_lines.
      lv_lines = lines( it_selected_rows ).

      IF lv_lines > 1.

        MESSAGE 'Selecione apenas 1 linha.' TYPE 'S' DISPLAY LIKE 'E'.

      ELSE.

        READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
        IF sy-subrc = 0.

          READ TABLE gt_solic_frete INTO wa_solic_frete INDEX wa_selected_rows-index.
          IF sy-subrc = 0.

            READ TABLE lt_tvarv WITH KEY high = wa_solic_frete-tipo_transporte TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
              MESSAGE 'Este tipo de transporte não pode ser excluído.' TYPE 'I'.
            ELSE.
              DELETE gt_solic_frete INDEX wa_selected_rows-index.
            ENDIF.
          ENDIF.
        ENDIF.

        " Atualiza o ALV se a linha foi restaurada
        IF ctl_alv1_7103 IS BOUND.
          CALL METHOD ctl_alv1_7103->refresh_table_display
            EXPORTING
              is_stable = _stable.
        ENDIF.

      ENDIF.
    ENDIF.
    "FF #154847 - fim


  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Module STATUS_7103 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_7103 OUTPUT.
  SET PF-STATUS 'STATUS_7103'.
  SET TITLEBAR 'TITLE_7103'.

  IF v_habi_troca_nota IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name EQ 'V_TROCA_NOTA'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_7103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_7103 INPUT.
  CASE sy-ucomm.
    WHEN 'LEAVE' OR 'BACK' OR 'EXIT' OR 'BT_CANCEL'.

      FREE: gt_solic_frete.

      CALL METHOD ctl_alv1_7103->refresh_table_display.
      CALL METHOD ctl_alv1_7103->free.
      CALL METHOD g_custom_container_7103->free.

      CLEAR v_troca_nota.

      FREE: g_custom_container_7103,
            ctl_alv1_7103,
            it_fieldcatalog_7103.

      SET SCREEN 0.
      LEAVE SCREEN .
    WHEN 'BT_CONF'.

      PERFORM f_grava_dados.

    WHEN 'CK_TROCA_NOTA'.

      PERFORM f_troca_nota.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module MONTA_ALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE monta_alv OUTPUT.
  IF g_custom_container_7103 IS INITIAL.

    CREATE OBJECT g_custom_container_7103
      EXPORTING
        container_name              = 'CC_SOLIC_FRETE'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_7103 USING:
    01 'OPERACAO'             'ZSD_ALV_SOLIC_FRETE'     ' '  'X'  ' '  ' '   'X'   ' '   ' '   ' '   '',
    02 'TIPO_TRANSPORTE'      'ZSD_ALV_SOLIC_FRETE'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   '',
    03 'PONTO_COLETA'         'ZSD_ALV_SOLIC_FRETE'     ' '  'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Ponto Coleta',
    04 'LOCAL_ENTREGA'        'ZSD_ALV_SOLIC_FRETE'     ' '  'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Local Entrega',
    05 'ITINERARIO'           'ZSD_ALV_SOLIC_FRETE'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   '',
    06 'DESC_ITINERARIO'      'ZSD_ALV_SOLIC_FRETE'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   ''.

    gs_layout_7103_alv1-sel_mode   = 'A'.
*    gs_layout_7103_alv1-cwidth_opt = 'X'.
*    gs_layout_7103_alv1-edit = abap_true.
    gs_layout_7103_alv1-stylefname = 'CELLSTYLES'.

    PERFORM excluir_botoes_7103 CHANGING it_exclude_7103.   "FF #154847

    CREATE OBJECT ctl_alv1_7103
      EXPORTING
        i_parent = g_custom_container_7103.           "ALV Lote

    SET HANDLER:
      lcl_event_handler_7103=>toolbar_7103 FOR ctl_alv1_7103, "FF #154847
      lcl_event_handler_7103=>user_command_7103 FOR ctl_alv1_7103, "FF #154847
*      lcl_event_handler_5230=>on_double_click_5230 FOR ctl_alv1_5230,
      lcl_event_handler_7103=>data_changed_finished_7103 FOR ctl_alv1_7103.

    CALL METHOD ctl_alv1_7103->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_7103_alv1
*       i_save               = 'A'
        it_toolbar_excluding = it_exclude_7103 "FF #154847
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_7103
        it_outtab            = gt_solic_frete.
*        it_sort         = it_sort_5220.

    CALL METHOD ctl_alv1_7103->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

    CALL METHOD ctl_alv1_7103->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    CALL METHOD ctl_alv1_7103->refresh_table_display( ).

  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_CELLTAB
*&---------------------------------------------------------------------*
FORM fill_celltab_7103  USING    p_edit
                        CHANGING p_it_celltab TYPE lvc_t_styl.

  DATA: wa_celltab     TYPE lvc_s_styl,
        status         TYPE raw4,
        status_inativo TYPE raw4.

  IF p_edit EQ abap_true.
    status = cl_gui_alv_grid=>mc_style_enabled.
  ELSE.
    status = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  p_it_celltab = VALUE #(
                         ( fieldname = 'OPERACAO'         style =  status )
                         ( fieldname = 'TIPO_TRANSPORTE'  style =  status )
                         ( fieldname = 'PONTO_COLETA'     style =  status )
                         ( fieldname = 'LOCAL_ENTREGA'    style =  status )
                         ( fieldname = 'ITINERARIO'       style =  status )
                         ( fieldname = 'DESC_ITINERARIO'  style =  status )
                        ).

ENDFORM.                               " FILL_CELLTAB
*&---------------------------------------------------------------------*
*& Form f_troca_nota
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_troca_nota .

  TYPES: BEGIN OF ty_rotas,
           carga TYPE znro_cg,
           ebeln TYPE ebeln,
           lifnr TYPE lifnr,
           werks TYPE werks_d,
           lzone TYPE lzone,
           azone TYPE azone,
         END OF ty_rotas.

  DATA: lt_rotas       TYPE TABLE OF ty_rotas,
        lv_pedidos     TYPE string,
        lt_solic_frete TYPE TABLE OF ty_solic_frete,
        lt_cellstyle   TYPE lvc_t_styl.

  SELECT SINGLE *
    FROM tvarvc
    INTO @DATA(ls_tvarvc)
    WHERE name = 'Z_TIPO_OPER_COMPRA'
      AND type = 'S'
      AND numb = '000'.

  IF v_troca_nota IS NOT INITIAL.

    DATA(lt_zsdt0346) = gt_zsdt0346.
    SORT lt_zsdt0346 BY nro_carga.
    DELETE ADJACENT DUPLICATES FROM lt_zsdt0346 COMPARING nro_carga.

    IF lt_zsdt0346 IS NOT INITIAL.

      SELECT *
        FROM zsdt0062
        INTO TABLE @DATA(lt_zsdt0062)
        FOR ALL ENTRIES IN @lt_zsdt0346
        WHERE nro_cg = @lt_zsdt0346-nro_carga.
      IF sy-subrc IS INITIAL.

        SORT lt_zsdt0062 BY ebeln.

        DELETE ADJACENT DUPLICATES FROM lt_zsdt0062 COMPARING ebeln.

        SELECT a~ebeln,a~lifn2,b~werks
          FROM ekpa AS a
          INNER JOIN ekpo AS b
          ON a~ebeln = b~ebeln
          INTO TABLE @DATA(lt_ekpa)
          FOR ALL ENTRIES IN @lt_zsdt0062
          WHERE a~ebeln = @lt_zsdt0062-ebeln
            AND a~parvw = 'PR'.
        IF sy-subrc IS INITIAL.
          SORT lt_ekpa BY ebeln.
          DATA(lt_ekpa_aux) = lt_ekpa.
          SORT lt_ekpa_aux BY werks.
          DELETE ADJACENT DUPLICATES FROM lt_ekpa_aux COMPARING werks.

          SELECT vstel,azone
           FROM tvst
           INTO TABLE @DATA(lt_tvst)
           FOR ALL ENTRIES IN @lt_ekpa_aux
           WHERE vstel = @lt_ekpa_aux-werks.
          IF sy-subrc IS INITIAL.
            SORT lt_tvst BY vstel.
          ENDIF.

          lt_ekpa_aux = lt_ekpa.
          SORT lt_ekpa_aux BY lifn2.
          DELETE ADJACENT DUPLICATES FROM lt_ekpa_aux COMPARING lifn2.

          SELECT lifnr,lzone
            FROM lfa1
            INTO TABLE @DATA(lt_lfa1)
            FOR ALL ENTRIES IN @lt_ekpa_aux
            WHERE lifnr = @lt_ekpa_aux-lifn2.
          IF sy-subrc IS INITIAL.
            SORT lt_lfa1 BY lifnr.
          ENDIF.
        ENDIF.

        SELECT a~ebeln,a~lifnr,b~werks
          FROM ekko AS a
          INNER JOIN ekpo AS b
          ON a~ebeln = b~ebeln
          INTO TABLE @DATA(lt_ekko)
          FOR ALL ENTRIES IN @lt_zsdt0062
          WHERE a~ebeln = @lt_zsdt0062-ebeln.
        IF sy-subrc IS INITIAL.
          SORT lt_ekko BY ebeln.

          DATA(lt_ekko_aux) = lt_ekko.
          SORT lt_ekko_aux BY werks.
          DELETE ADJACENT DUPLICATES FROM lt_ekko_aux COMPARING werks.

          SELECT vstel azone
            FROM tvst
            APPENDING TABLE lt_tvst
            FOR ALL ENTRIES IN lt_ekko_aux
            WHERE vstel = lt_ekko_aux-werks.
          IF sy-subrc IS INITIAL.
            SORT lt_tvst BY vstel.
          ENDIF.

          lt_ekko_aux = lt_ekko.
          SORT lt_ekko_aux BY lifnr.
          DELETE ADJACENT DUPLICATES FROM lt_ekko_aux COMPARING lifnr.

          SELECT lifnr lzone
            FROM lfa1
            APPENDING TABLE lt_lfa1
            FOR ALL ENTRIES IN lt_ekko_aux
            WHERE lifnr = lt_ekko_aux-lifnr.
          IF sy-subrc IS INITIAL.
            SORT lt_lfa1 BY lifnr.
          ENDIF.
        ENDIF.

        LOOP AT lt_zsdt0062 ASSIGNING FIELD-SYMBOL(<fs_zsdt0062>).

          READ TABLE lt_ekpa ASSIGNING FIELD-SYMBOL(<fs_ekpa>)
          WITH KEY ebeln = <fs_zsdt0062>-ebeln
          BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            READ TABLE lt_ekko ASSIGNING FIELD-SYMBOL(<fs_ekko>)
            WITH KEY ebeln = <fs_zsdt0062>-ebeln
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              APPEND INITIAL LINE TO lt_rotas ASSIGNING FIELD-SYMBOL(<fs_rotas>).
              <fs_rotas>-carga = <fs_zsdt0062>-nro_cg.
              <fs_rotas>-ebeln = <fs_ekko>-ebeln.
              <fs_rotas>-lifnr = <fs_ekko>-lifnr.
              <fs_rotas>-werks = <fs_ekko>-werks.

              READ TABLE lt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>)
              WITH KEY lifnr = <fs_ekko>-lifnr
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                <fs_rotas>-lzone = <fs_lfa1>-lzone.
              ENDIF.

              READ TABLE lt_tvst ASSIGNING FIELD-SYMBOL(<fs_tvst>)
              WITH KEY vstel = <fs_ekko>-werks
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                <fs_rotas>-azone = <fs_tvst>-azone.
              ENDIF.

            ENDIF.

          ELSE.

            APPEND INITIAL LINE TO lt_rotas ASSIGNING <fs_rotas>.

            <fs_rotas>-carga = <fs_zsdt0062>-nro_cg.
            <fs_rotas>-ebeln = <fs_ekpa>-ebeln.
            <fs_rotas>-lifnr = <fs_ekpa>-lifn2.
            <fs_rotas>-werks = <fs_ekpa>-werks.

            READ TABLE lt_lfa1 ASSIGNING <fs_lfa1>
            WITH KEY lifnr = <fs_ekpa>-lifn2
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              <fs_rotas>-lzone = <fs_lfa1>-lzone.
            ENDIF.

            READ TABLE lt_tvst ASSIGNING <fs_tvst>
            WITH KEY vstel = <fs_ekpa>-werks
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              <fs_rotas>-azone = <fs_tvst>-azone.
            ENDIF.

          ENDIF.

        ENDLOOP.

        SELECT a~lzone,a~azone,a~route,b~bezei
          FROM trolz AS a
          INNER JOIN tvrot AS b
          ON a~route = b~route
          INTO TABLE @DATA(lt_trolz)
          FOR ALL ENTRIES IN @lt_rotas
          WHERE a~lzone = @lt_rotas-azone
            AND a~azone = @lt_rotas-lzone
            AND b~spras = @sy-langu.
        IF sy-subrc IS INITIAL.
          SORT lt_trolz BY lzone azone.
        ENDIF.

        LOOP AT lt_rotas ASSIGNING <fs_rotas>.

          READ TABLE lt_trolz INTO DATA(wa_trolz)           "FF #154797
          WITH KEY lzone = <fs_rotas>-azone
                   azone = <fs_rotas>-lzone.

          IF sy-subrc IS NOT INITIAL.
            CLEAR wa_trolz.
          ENDIF.

          APPEND INITIAL LINE TO lt_solic_frete ASSIGNING FIELD-SYMBOL(<fs_solic_frete>).
          <fs_solic_frete>-nro_carga       = <fs_rotas>-carga.
          <fs_solic_frete>-operacao        = ls_tvarvc-low.
          <fs_solic_frete>-tipo_transporte = ls_tvarvc-high.
          <fs_solic_frete>-itinerario      = wa_trolz-route.
          <fs_solic_frete>-desc_itinerario = wa_trolz-bezei.
          <fs_solic_frete>-usuario         = sy-uname.
          <fs_solic_frete>-data            = sy-datum.
          <fs_solic_frete>-hora            = sy-uzeit.
          <fs_solic_frete>-ponto_coleta    = <fs_rotas>-lzone. "FF #154797
          <fs_solic_frete>-local_entrega   = <fs_rotas>-azone. "FF #154797

          PERFORM fill_celltab_7103 USING abap_false
                                     CHANGING lt_cellstyle.

          <fs_solic_frete>-cellstyles = lt_cellstyle.

          FREE: lt_cellstyle.

          APPEND INITIAL LINE TO gt_zsdt0346 ASSIGNING FIELD-SYMBOL(<fs_zsdt0346>).
          MOVE-CORRESPONDING <fs_solic_frete> TO <fs_zsdt0346>.

          "FF #154797 - inicio
*          ELSE.

*            IF lv_pedidos IS INITIAL.
*              lv_pedidos = <fs_rotas>-ebeln.
*            ELSE.
*              CONCATENATE lv_pedidos '/' <fs_rotas>-ebeln INTO lv_pedidos SEPARATED BY space.
*            ENDIF.

*          ENDIF.
          "FF #154797 - fim

        ENDLOOP.

        "FF #154797 - inicio
*        IF lv_pedidos IS NOT INITIAL.
*
*          CALL FUNCTION 'POPUP_TO_INFORM'
*            EXPORTING
*              titel = 'ERRO'
*              txt1  = 'Para o frete de compra não existe itinerário para o(s) pedido(s)'
*              txt2  = lv_pedidos
*              txt3  = 'criar através da ZLES0214!'.
*
*          FREE:  lt_rotas.
*          CLEAR: lv_pedidos,
*                 v_troca_nota.
*
*          EXIT.

*        ELSE.
        "FF #154797 - fim

        APPEND LINES OF lt_solic_frete TO gt_solic_frete.

*        ENDIF.
      ELSE. "Erro - FF #154797 - inicio

        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = 'ERRO'
            txt1  = 'NÃO EXISTE PEDIDO DE COMPRA VINCULADO A CARGA'
            txt2  = space
            txt3  = space.

        CLEAR: v_troca_nota.

*FF #154797 - fim

      ENDIF.

    ENDIF.

  ELSE.

    DELETE gt_solic_frete WHERE operacao = ls_tvarvc-low.

  ENDIF.

*  DELETE ADJACENT DUPLICATES FROM gt_solic_frete COMPARING nro_carga operacao tipo_transporte ponto_coleta local_entrega. "FF #154847
  gt_solic_frete_bkp[] = gt_solic_frete[].                  "FF #154847

  CALL METHOD ctl_alv1_7103->refresh_table_display( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_grava_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_grava_dados .
  DATA: lt_zsdt0346 TYPE TABLE OF zsdt0346.
  "FF #154847 - inicio
  IF gt_solic_frete_aux[] IS NOT INITIAL.

    gt_solic_frete[] = gt_solic_frete_aux[].

  ENDIF.
  "FF #154847 - fim

  DATA(lt_solic_frete) = gt_solic_frete.
  DELETE lt_solic_frete WHERE itinerario <> space.
  IF lt_solic_frete IS NOT INITIAL.
    MESSAGE 'Todas as etapas do frete devem ter itinerário cadastrados' TYPE 'I'.
    EXIT.
  ELSE.

    IF gt_zsdt0346 IS NOT INITIAL.

      "FF #154847 - inicio
      DELETE gt_zsdt0346 WHERE itinerario IS INITIAL.

      SELECT * FROM zsdt0365 "Tabela com os sequênciais de cotação para cada nro de carga.
      FOR ALL ENTRIES IN @gt_zsdt0346
      WHERE nro_carga = @gt_zsdt0346-nro_carga
      INTO TABLE @DATA(lt_seq_cotacao).

      IF sy-subrc <> 0.
        CLEAR lt_seq_cotacao.
      ENDIF.

      LOOP AT gt_zsdt0346 ASSIGNING FIELD-SYMBOL(<fs_data>).

        READ TABLE lt_seq_cotacao INTO DATA(wa_seq_cotacao) WITH KEY nro_carga = <fs_data>-nro_carga.
        IF sy-subrc = 0.

          " Se o registro da carga já existe, incrementa o sequencial
          <fs_data>-seq_cotacao = wa_seq_cotacao + 1.
          wa_seq_cotacao = <fs_data>-seq_cotacao.

          " Atualiza o valor modificado de volta em lt_seq_cotacao para o MODIFY ao final
          MODIFY lt_seq_cotacao FROM wa_seq_cotacao INDEX sy-tabix.

        ELSE.

          " Se não existir, inicializa o sequencial em 1
          <fs_data>-seq_cotacao = 1.

          " Adiciona um novo registro na tabela interna lt_seq_cotacao para inserir depois
          APPEND VALUE #( nro_carga = <fs_data>-nro_carga seq_cotacao = <fs_data>-seq_cotacao ) TO lt_seq_cotacao.

        ENDIF.


      ENDLOOP.


* Atualiza a tabela ZSDT0365 com os novos sequenciais
      MODIFY zsdt0365 FROM TABLE lt_seq_cotacao.

      "FF #154847 - fim

      MODIFY zsdt0346 FROM TABLE gt_zsdt0346.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.
        MESSAGE 'Solicitação de Frete criada com sucesso!' TYPE 'S'.
      ENDIF.

      READ TABLE gt_zsdt0346 ASSIGNING FIELD-SYMBOL(<fs_zsdt0346>) INDEX 1.
      IF sy-subrc IS INITIAL.
        UPDATE zsdt0133 SET status = '2' WHERE nro_cg = <fs_zsdt0346>-nro_carga.
        IF sy-subrc IS INITIAL.
          COMMIT WORK.

          IF ctl_alv1_5220 IS BOUND.
            READ TABLE it_carga_scot_5220 ASSIGNING FIELD-SYMBOL(<fs_scot>)
            WITH KEY nro_cg = <fs_zsdt0346>-nro_carga.
            IF sy-subrc IS INITIAL.
              <fs_scot>-status = '2'.
              <fs_scot>-icone = '@5D@'.
            ENDIF.

          ENDIF.

          IF ctl_alv1_5230 IS BOUND.

            READ TABLE it_carga_5230 ASSIGNING FIELD-SYMBOL(<fs_scot_5230>)
            WITH KEY nro_cg = <fs_zsdt0346>-nro_carga.
            IF sy-subrc IS INITIAL.
              <fs_scot_5230>-status = '2'.
              <fs_scot_5230>-icone = '@5D@'.
            ENDIF.

          ENDIF.

          SET SCREEN 0.
          LEAVE SCREEN.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.


ENDFORM.
