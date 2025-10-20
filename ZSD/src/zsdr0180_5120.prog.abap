*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5120
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_lote_5120,
         icone        TYPE char6,
         icone2       TYPE char6,
         name1        TYPE lfa1-name1,
         stcd2        TYPE lfa1-stcd2,
         telf1        TYPE lfa1-telf1,
         ctg_transp_d TYPE char10,
         name1_t      TYPE lfa1-name1,
         telf1_t      TYPE lfa1-telf1,
         name2        TYPE t001w-name2.
         INCLUDE STRUCTURE zsdt0129.
       TYPES: END OF ty_lote_5120.

TYPES: BEGIN OF ty_solicitacao_5120,
         name1 TYPE kna1-name1,
         maktx TYPE makt-maktx,
         lifnr TYPE lfa1-lifnr.
         INCLUDE STRUCTURE zsdt0131.
       TYPES: END OF ty_solicitacao_5120.

TYPES: BEGIN OF ty_cliente_5120,
         name1      TYPE kna1-name1,
         obs        TYPE char6,
         cellstyles TYPE lvc_t_styl,
         nr_rot1    TYPE zsdt0130-nr_rot,
         nr_rot2    TYPE zsdt0130-nr_rot.
         INCLUDE    STRUCTURE zsdt0130.
       TYPES: END OF ty_cliente_5120.

TYPES: BEGIN OF ty_sol_5120,
         nro_cg     TYPE zsdt0133-nro_cg,
         bezei      TYPE tvkbt-bezei,
         seq_ent_cg TYPE zsdt0130-seq_ent_cg,
         rot_desc   TYPE zsdt0132-rot_desc,
         name1      TYPE kna1-name1,
         maktx      TYPE makt-maktx,
         tval       TYPE zsdt0133-preco_frete.
         INCLUDE    STRUCTURE zsdt0131.
         TYPES: cellstyles TYPE lvc_t_styl.
TYPES: END OF ty_sol_5120.

TYPES: BEGIN OF ty_carga_5120,
         edit        TYPE char1,
         icone       TYPE char6,
         rot_desc    TYPE zsdt0132-rot_desc,
         tval        TYPE zsdt0133-preco_frete,
         desc_transp TYPE lfa1-name1,
         placa_cav   TYPE zsdt0129-placa_cav,
         placa_car1  TYPE zsdt0129-placa_car1,
         placa_car2  TYPE zsdt0129-placa_car2,
*-CS2019001891 - JT - 04.02.2021 - inicio
         placa_car3  TYPE zsdt0129-placa_car3,
*-CS2019001891 - JT - 04.02.2021 - inicio
         motorista   TYPE zsdt0129-motorista,
         desc_mot    TYPE lfa1-name1,
         dt_entrega  TYPE zsdt0129-dt_entrega,
         inco1       TYPE zsdt0129-inco1.
         INCLUDE STRUCTURE zsdt0133.
         TYPES: cellstyles  TYPE lvc_t_styl.
TYPES: END OF ty_carga_5120.

DATA: it_lote_5120        TYPE STANDARD TABLE OF ty_lote_5120,
      it_lote_5231        TYPE STANDARD TABLE OF ty_lote_5120,
      it_cliente_5120     TYPE STANDARD TABLE OF ty_cliente_5120,
      it_solicitacao_5120 TYPE STANDARD TABLE OF ty_solicitacao_5120,
      it_lfa1_5120        TYPE STANDARD TABLE OF lfa1,
      it_lfa1_t_5120      TYPE STANDARD TABLE OF lfa1,
      it_zsdt0133_5120    TYPE STANDARD TABLE OF zsdt0133,
      it_zsdt0131_5120    TYPE STANDARD TABLE OF zsdt0131,
      it_t001w_5120       TYPE STANDARD TABLE OF t001w.

DATA: g_custom_container_5120      TYPE REF TO cl_gui_custom_container,
      dg_splitter_1_5120           TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2_5120           TYPE REF TO cl_gui_splitter_container,
      dg_parent_1_5120             TYPE REF TO cl_gui_container,
      dg_parent_2_5120             TYPE REF TO cl_gui_container,
      dg_parent_3_5120             TYPE REF TO cl_gui_container,
      ctl_alv1_5120                TYPE REF TO cl_gui_alv_grid,
      ctl_alv2_5120                TYPE REF TO cl_gui_alv_grid,
      ctl_alv3_5120                TYPE REF TO cl_gui_alv_grid,
      gs_layout_5120_alv1          TYPE lvc_s_layo,
      gs_layout_5120_alv2          TYPE lvc_s_layo,
      gs_layout_5120_alv3          TYPE lvc_s_layo,
      it_fieldcatalog_lote_5120    TYPE lvc_t_fcat,
      it_fieldcatalog_sol_5120     TYPE lvc_t_fcat,
      it_fieldcatalog_cliente_5120 TYPE lvc_t_fcat,
      it_sort_zsdt0129_alv         TYPE lvc_t_sort.

DATA: it_sel_rows_lote_5120 TYPE lvc_t_row,
      it_lote_5120_check    TYPE STANDARD TABLE OF ty_lote_5120,
      it_carga_canc_5120    TYPE STANDARD TABLE OF ty_carga_5120,
      wa_carga_canc_5120    TYPE ty_carga_5120,
      vg_unico_lote         TYPE char1,
      vg_dt_ent_unico_lote  TYPE zsdt0129-dt_entrega,
      vg_qtd_kg             TYPE zsdt0131-qtd_vinc.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler_5120 DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      on_double_click_5120 FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      handle_hotspot_click_5120  FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,

      handle_button_click_5120 FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no,

      toolbar_5120 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      user_command_5120 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler_5120 IMPLEMENTATION.

  METHOD on_double_click_5120.

    DATA: vl_row TYPE i.

    PERFORM alv_solicitacao_5120 USING e_row-index.
    PERFORM alv_cliente_5120 USING e_row-index.

    CALL METHOD ctl_alv2_5120->get_frontend_layout
      IMPORTING
        es_layout = gs_layout_5120_alv2.

    gs_layout_5120_alv2-cwidth_opt = abap_true.

    CALL METHOD ctl_alv2_5120->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_5120_alv2.

    CALL METHOD ctl_alv3_5120->get_frontend_layout
      IMPORTING
        es_layout = gs_layout_5120_alv3.

    gs_layout_5120_alv3-cwidth_opt = abap_true.

    CALL METHOD ctl_alv3_5120->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_5120_alv3.

    CALL METHOD ctl_alv2_5120->refresh_table_display
      EXPORTING
        is_stable = _stable.

    CALL METHOD ctl_alv3_5120->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD handle_hotspot_click_5120.

    DATA: wa_cliente_5120 TYPE ty_cliente_5120.

    DATA: wa_bor TYPE borident.

    DATA: wl_name  TYPE thead-tdname.

    IF e_column_id-fieldname EQ 'NR_ROT2'.

      READ TABLE it_cliente_5120 INTO wa_cliente_5120 INDEX e_row_id-index. "IT_CLIENTE_5120_CLICK
      IF sy-subrc IS INITIAL.
        wa_bor-objkey = wa_cliente_5120-nr_rot2.
        wa_bor-objtype = 'ZSDR0061'.
        PERFORM chama_anexo USING wa_bor.
      ENDIF.

    ELSEIF e_column_id-fieldname EQ 'NR_ROT1'.

      READ TABLE it_cliente_5120 INTO wa_cliente_5120 INDEX e_row_id-index. "IT_CLIENTE_5120_CLICK
      IF sy-subrc IS INITIAL.
        wl_name = wa_cliente_5120-nr_rot1.
        PERFORM chama_texto_rot USING wl_name.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    "HANDEL_HOTSPOT_CLICK

  METHOD handle_button_click_5120.

    DATA: wa_cliente_5120 TYPE ty_cliente_5120.

    DATA: wl_name  TYPE thead-tdname.

    IF es_col_id-fieldname EQ 'OBS'.

      READ TABLE it_cliente_5120 INTO wa_cliente_5120 INDEX es_row_no-row_id.
      IF sy-subrc IS INITIAL.
        CONCATENATE wa_cliente_5120-nro_sol '001' INTO wl_name.
        PERFORM chama_texto_obs USING wl_name.
      ENDIF.
    ENDIF.

  ENDMETHOD .

  METHOD toolbar_5120.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'NOVOLOTE'.
    wa_tool-icon     = '@XR@'.
    wa_tool-quickinfo = 'Novo Lote'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'EDITARLOTE'.
    wa_tool-icon     = '@HL@'.
    wa_tool-quickinfo = 'Editar Lote'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

*    IF p_filial IS NOT INITIAL.
*
*      MOVE 3 TO wa_tool-butn_type.
*      APPEND wa_tool TO e_object->mt_toolbar.
*      CLEAR wa_tool.
*
*      wa_tool-function = 'AUTORIZACAO'.
*      wa_tool-icon     = '@96@'.
*      wa_tool-quickinfo = 'Aut. de Embarque'.
*      APPEND wa_tool TO e_object->mt_toolbar.
*      CLEAR wa_tool.
*
*      wa_tool-function  = 'CANCAUTORIZ'.
*      wa_tool-icon      = '@BA@'.
*      wa_tool-quickinfo = 'Cancelar Aut. de Embarque'.
*      APPEND wa_tool TO e_object->mt_toolbar.
*      CLEAR wa_tool.
*
*    ENDIF.
*
*    IF p_corplt IS NOT INITIAL OR
*       p_corpcg IS NOT INITIAL OR
*       p_corppt IS NOT INITIAL.
*
*      MOVE 3 TO wa_tool-butn_type.
*      APPEND wa_tool TO e_object->mt_toolbar.
*      CLEAR wa_tool.
*
*      wa_tool-function = 'REPROVAR'.
*      wa_tool-icon     = '@2W@'.
*      wa_tool-quickinfo = 'Reprovar Lote'.
*      APPEND wa_tool TO e_object->mt_toolbar.
*      CLEAR wa_tool.
*
*      wa_tool-function = 'APROVAR'.
*      wa_tool-icon     = '@4R@'.
*      wa_tool-quickinfo = 'Aprovar Lote'.
*      APPEND wa_tool TO e_object->mt_toolbar.
*      CLEAR wa_tool.
*
*      wa_tool-function = 'GERARCARGA'.
*      wa_tool-icon     = '@7E@'.
*      wa_tool-quickinfo = 'Gerar Carga'.
*      APPEND wa_tool TO e_object->mt_toolbar.
*      CLEAR wa_tool.
*
*    ENDIF.

  ENDMETHOD.

  METHOD user_command_5120.

    DATA: it_zsdt0129           TYPE STANDARD TABLE OF zsdt0129,
          it_sol_5120           TYPE STANDARD TABLE OF ty_sol_5120,
          it_sol_aux_5120       TYPE STANDARD TABLE OF ty_sol_5120,
          it_zsdt0062           TYPE STANDARD TABLE OF zsdt0062,
          wa_sol_aux_5120       TYPE ty_sol_5120,
          wa_sol_5120           TYPE ty_sol_5120,
          wa_sel_rows_lote_5120 TYPE lvc_s_row,
          wa_lote_5120          TYPE ty_lote_5120,
          wa_zsdt0129           TYPE zsdt0129,
          wa_zsdt0133           TYPE zsdt0133,
          wa_zsdt0133_2         TYPE zsdt0133,
          wa_zsdt0062           TYPE zsdt0062,
          vl_lines              TYPE i,
          vl_check              TYPE char1,
          vl_cont               TYPE i,
          vl_vinc1              TYPE zsdt0131-qtd_vinc,
          vl_vinc2              TYPE zsdt0131-qtd_vinc.



    IF e_ucomm = 'NOVOLOTE'.

      vg_subt_lote = '5130'.
      PERFORM clear_5130.

    ELSEIF e_ucomm = 'EDITARLOTE'.

      CALL METHOD ctl_alv1_5120->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows_lote_5120.

      DESCRIBE TABLE it_sel_rows_lote_5120 LINES vl_lines.
      IF vl_lines NE 1.
        MESSAGE text-022 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        CLEAR: vg_lote_editar.
        READ TABLE it_sel_rows_lote_5120 INTO wa_sel_rows_lote_5120 INDEX 1.
        IF sy-subrc IS INITIAL.
          READ TABLE it_lote_5120 INTO wa_lote_5120 INDEX wa_sel_rows_lote_5120-index.
          IF sy-subrc IS INITIAL.

            CLEAR: wa_zsdt0129.

            SELECT SINGLE *
              FROM zsdt0129
              INTO wa_zsdt0129
              WHERE nro_lote EQ wa_lote_5120-nro_lote.

            IF wa_lote_5120-status NE wa_zsdt0129-status.
              MESSAGE text-096 TYPE 'S' DISPLAY LIKE 'E'.
*            ELSEIF wa_zsdt0129-status NE 0 AND p_filial IS NOT INITIAL.
*              MESSAGE text-027 TYPE 'S' DISPLAY LIKE 'E'.
*            ELSEIF ( wa_zsdt0129-status NE 0 AND wa_zsdt0129-status NE 1 ) AND p_corplt IS NOT INITIAL.
*              MESSAGE text-028 TYPE 'S' DISPLAY LIKE 'E'.
            ELSE.

              CALL FUNCTION 'ZENQUEUE_SD_LOTE_INSUMOS'
                EXPORTING
                  chave          = wa_lote_5120-nro_lote
                EXCEPTIONS
                  foreign_lock   = 1
                  system_failure = 2
                  OTHERS         = 3.

              IF sy-subrc <> 0.
                MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
              ELSE.
                vg_lote_editar = wa_lote_5120-nro_lote.
                vg_subt_lote = '5140'.
                PERFORM completa_header_5140.
              ENDIF.
            ENDIF.

          ENDIF.
        ENDIF.

      ENDIF.

    ELSEIF e_ucomm = 'REPROVAR'.

      CLEAR: vl_check, vl_lines, wa_zsdt0129.

      CALL METHOD ctl_alv1_5120->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows_lote_5120.

      DESCRIBE TABLE it_sel_rows_lote_5120 LINES vl_lines.

      IF vl_lines NE 1.
        MESSAGE text-022 TYPE 'S' DISPLAY LIKE 'E'.
        MOVE abap_true TO vl_check.
      ENDIF.

      IF vl_check IS INITIAL.

        READ TABLE it_sel_rows_lote_5120 INTO wa_sel_rows_lote_5120 INDEX 1.
        READ TABLE it_lote_5120 INTO wa_lote_5120 INDEX wa_sel_rows_lote_5120-index.

*      LOOP AT IT_CARGA_5720 INTO WA_CARGA_5720.
*        READ TABLE IT_SEL_ROWS_CARGA_5720 INTO WA_SEL_ROWS_CARGA_5720 WITH KEY INDEX = SY-TABIX.
        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'ZENQUEUE_SD_LOTE_INSUMOS'
            EXPORTING
              chave          = wa_lote_5120-nro_lote
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            MOVE abap_true TO vl_check.
          ENDIF.

          IF vl_check IS INITIAL.

            CLEAR: wa_zsdt0129.

            SELECT SINGLE *
                FROM zsdt0129
                INTO CORRESPONDING FIELDS OF wa_zsdt0129
                WHERE nro_lote EQ wa_lote_5120-nro_lote.

            IF wa_zsdt0129-status NE wa_lote_5120-status.
              MESSAGE text-096 TYPE 'S' DISPLAY LIKE 'E'.
            ELSEIF wa_zsdt0129-status NE '1'.
              MESSAGE text-099 TYPE 'S' DISPLAY LIKE 'E'.
            ELSE.
              wa_zsdt0129-status = 0.
              MODIFY zsdt0129 FROM wa_zsdt0129.
            ENDIF.

            CALL FUNCTION 'ZDENQUEUE_SD_LOTE_INSUMOS'
              EXPORTING
                chave = wa_lote_5120-nro_lote.

          ENDIF.

        ENDIF.

      ENDIF.

    ELSEIF e_ucomm = 'APROVAR'.


      CLEAR: vl_check, vl_lines, wa_zsdt0129.

      CALL METHOD ctl_alv1_5120->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows_lote_5120.

      DESCRIBE TABLE it_sel_rows_lote_5120 LINES vl_lines.

      IF vl_lines NE 1.
        MESSAGE text-022 TYPE 'S' DISPLAY LIKE 'E'.
        MOVE abap_true TO vl_check.
      ENDIF.

      IF vl_check IS INITIAL.

        READ TABLE it_sel_rows_lote_5120 INTO wa_sel_rows_lote_5120 INDEX 1.
        READ TABLE it_lote_5120 INTO wa_lote_5120 INDEX wa_sel_rows_lote_5120-index.

        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'ZENQUEUE_SD_LOTE_INSUMOS'
            EXPORTING
              chave          = wa_lote_5120-nro_lote
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            MOVE abap_true TO vl_check.
          ENDIF.

          IF vl_check IS INITIAL.

            CLEAR: wa_zsdt0129.

            SELECT SINGLE *
                FROM zsdt0129
                INTO CORRESPONDING FIELDS OF wa_zsdt0129
                WHERE nro_lote EQ wa_lote_5120-nro_lote.

            IF wa_zsdt0129-status NE wa_lote_5120-status.
              MESSAGE text-096 TYPE 'S' DISPLAY LIKE 'E'.
            ELSEIF wa_zsdt0129-status NE '0'.
              MESSAGE text-099 TYPE 'S' DISPLAY LIKE 'E'.
            ELSE.
              wa_zsdt0129-status = 1.
              MODIFY zsdt0129 FROM wa_zsdt0129.
            ENDIF.

            CALL FUNCTION 'ZDENQUEUE_SD_LOTE_INSUMOS'
              EXPORTING
                chave = wa_lote_5120-nro_lote.

          ENDIF.

        ENDIF.

      ENDIF.

    ELSEIF e_ucomm = 'GERARCARGA'.

      CLEAR: vl_check, wa_lote_5120.
      PERFORM check_gerar_carga CHANGING vl_check.

      IF vl_check IS INITIAL.

        LOOP AT it_lote_5120_check INTO wa_lote_5120.

          CALL FUNCTION 'ZENQUEUE_SD_LOTE_INSUMOS'
            EXPORTING
              chave          = wa_lote_5120-nro_lote
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            vl_cont = sy-tabix.
            MOVE abap_true TO vl_check.
            EXIT.
          ENDIF.

        ENDLOOP.

        IF vl_cont IS NOT INITIAL.
          LOOP AT it_lote_5120_check INTO wa_lote_5120.
            IF sy-tabix LT vl_cont.

              CALL FUNCTION 'ZDENQUEUE_SD_LOTE_INSUMOS'
                EXPORTING
                  chave = wa_lote_5120-nro_lote.

            ENDIF.
          ENDLOOP.
        ENDIF.

        IF vl_check IS INITIAL.
          PERFORM pesquisa_pop_5121.
          PERFORM constroi_pop_5121.
          CALL SCREEN 5121 STARTING AT 5 5 ENDING AT 160 30.
        ENDIF.

      ENDIF.

    ELSEIF e_ucomm = 'AUTORIZACAO'.

      CLEAR: vl_check, wa_zsdt0133, wa_zsdt0129, wa_zsdt0133_2, it_sol_5120, it_sol_aux_5120, wa_sol_aux_5120, wa_sol_5120.

      CALL METHOD ctl_alv1_5120->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows_lote_5120.

      DESCRIBE TABLE it_sel_rows_lote_5120 LINES vl_lines.

      IF vl_lines NE 1.
        MESSAGE text-022 TYPE 'S' DISPLAY LIKE 'E'.
        MOVE abap_true TO vl_check.
      ENDIF.

      "Consistência se há dados faltantes
      IF vl_check IS INITIAL.

        READ TABLE it_sel_rows_lote_5120 INTO wa_sel_rows_lote_5120 INDEX 1.
        READ TABLE it_lote_5120 INTO wa_lote_5120 INDEX wa_sel_rows_lote_5120-index.
        IF sy-subrc IS INITIAL.

          IF wa_lote_5120-inco1 NE 'FOB' OR wa_lote_5120-nro_cg IS INITIAL.
            MESSAGE text-114 TYPE 'S' DISPLAY LIKE 'E'.
            MOVE abap_true TO vl_check.
          ELSE.

            CLEAR: wa_zsdt0133.

            SELECT SINGLE *
              FROM zsdt0133
              INTO wa_zsdt0133
              WHERE nro_cg EQ wa_lote_5120-nro_cg
                AND status NE 'X'.

            IF sy-subrc IS INITIAL.

              CLEAR: wa_zsdt0129.

              SELECT SINGLE *
                FROM zsdt0129
                INTO wa_zsdt0129
                WHERE nro_cg EQ wa_lote_5120-nro_cg
                  AND status NE 'X'.

            ENDIF.

            IF  wa_zsdt0129-placa_cav IS INITIAL OR wa_zsdt0129-motorista IS INITIAL.
              MESSAGE text-042 TYPE 'S' DISPLAY LIKE 'E'.
              vl_check = abap_true.
            ELSEIF wa_zsdt0133-status LT 4.
              MESSAGE text-071 TYPE 'S' DISPLAY LIKE 'E'.
              vl_check = abap_true.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.

      "Consistência se há alguma ordem não vinculada
      IF vl_check IS INITIAL.

        SELECT *
          FROM zsdt0129
          INTO TABLE it_zsdt0129
          WHERE nro_cg EQ wa_lote_5120-nro_cg
            AND status NE 'X'.

        IF it_zsdt0129 IS NOT INITIAL.

          SELECT *
            FROM zsdt0131
            INTO CORRESPONDING FIELDS OF TABLE it_sol_5120
            FOR ALL ENTRIES IN it_zsdt0129
            WHERE nro_lote EQ it_zsdt0129-nro_lote
              AND status NE 'X'.

          it_sol_aux_5120 = it_sol_5120.

          SORT it_sol_aux_5120 BY nro_cg vbeln posnr ASCENDING.
          DELETE ADJACENT DUPLICATES FROM it_sol_aux_5120 COMPARING nro_cg vbeln posnr.

          SELECT *
            FROM zsdt0062
            INTO TABLE it_zsdt0062
            FOR ALL ENTRIES IN it_zsdt0129
            WHERE nro_cg EQ it_zsdt0129-nro_cg
              AND status EQ 'L'.

          LOOP AT it_sol_aux_5120 INTO wa_sol_aux_5120.
            LOOP AT it_sol_5120 INTO wa_sol_5120 WHERE vbeln  EQ wa_sol_aux_5120-vbeln
                                                   AND posnr  EQ wa_sol_aux_5120-posnr.
              vl_vinc1 = vl_vinc1 + wa_sol_5120-qtd_vinc.
            ENDLOOP.
            LOOP AT it_zsdt0062 INTO wa_zsdt0062 WHERE vbeln  EQ wa_sol_aux_5120-vbeln
                                                   AND posnr  EQ wa_sol_aux_5120-posnr.
              vl_vinc2 = vl_vinc2 + wa_zsdt0062-qtd_vinc.
            ENDLOOP.
            IF vl_vinc1 NE vl_vinc2.
              MESSAGE s000(z_fi) WITH 'Falta vinculação na Carga' wa_sol_aux_5120-nro_cg DISPLAY LIKE 'E'.
              vl_check = abap_true.
            ENDIF.
          ENDLOOP.

          CLEAR: vl_vinc1, vl_vinc2.

        ELSE.
          vl_check = abap_true.
        ENDIF.
      ENDIF.

      "Gera Aut. de Embarque
      IF vl_check IS INITIAL.

        CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
          EXPORTING
            chave          = wa_lote_5120-nro_cg
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
        ELSE.

          PERFORM gerar_pdf_embarque USING wa_lote_5120-nro_cg abap_false.

          IF wa_zsdt0133-status EQ 4.
            SELECT SINGLE *
              FROM zsdt0133
              INTO CORRESPONDING FIELDS OF wa_zsdt0133_2
              WHERE nro_cg EQ wa_lote_5120-nro_cg.

            wa_zsdt0133_2-status = 5.
            MODIFY zsdt0133 FROM wa_zsdt0133_2.
          ENDIF.

          CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
            EXPORTING
              chave = wa_lote_5120-nro_cg.

        ENDIF.
      ENDIF.

    ELSEIF e_ucomm = 'CANCAUTORIZ'.

      CLEAR: it_carga_canc_5120, wa_carga_canc_5120, vl_check, vl_lines, wa_zsdt0133.

      CALL METHOD ctl_alv1_5120->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows_lote_5120.

      DESCRIBE TABLE it_sel_rows_lote_5120 LINES vl_lines.

      IF vl_lines NE 1.
        MESSAGE text-022 TYPE 'S' DISPLAY LIKE 'E'.
        MOVE abap_true TO vl_check.
      ENDIF.

      "Consistência se há dados faltantes
      IF vl_check IS INITIAL.

        READ TABLE it_sel_rows_lote_5120 INTO wa_sel_rows_lote_5120 INDEX 1.
        READ TABLE it_lote_5120 INTO wa_lote_5120 INDEX wa_sel_rows_lote_5120-index.
        IF sy-subrc IS INITIAL.

          IF wa_lote_5120-inco1 NE 'FOB' OR wa_lote_5120-nro_cg IS INITIAL.
            MESSAGE text-114 TYPE 'S' DISPLAY LIKE 'E'.
            MOVE abap_true TO vl_check.
          ELSE.

            CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
              EXPORTING
                chave          = wa_lote_5120-nro_cg
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
              MOVE abap_true TO vl_check.
            ELSE.

              CLEAR: wa_zsdt0133.

              SELECT SINGLE *
                FROM zsdt0133
                INTO wa_zsdt0133
               WHERE nro_cg EQ wa_lote_5120-nro_cg.

              IF wa_zsdt0133-status NE 5.
                MESSAGE text-115 TYPE 'S' DISPLAY LIKE 'E'.
              ELSE.

                wa_carga_canc_5120-nro_cg = wa_lote_5120-nro_cg.
                APPEND wa_carga_canc_5120 TO it_carga_canc_5120.
                PERFORM busca_lotes_produtos_5120 CHANGING vl_check.

                IF vl_check IS INITIAL.

                  PERFORM cancela_autorizacao_5120.

                  CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
                    EXPORTING
                      chave = wa_lote_5120-nro_cg.

                  MESSAGE text-116 TYPE 'S' DISPLAY LIKE 'E'.
                  LEAVE TO SCREEN 5000.
                ELSE.
                  MESSAGE text-093 TYPE 'S' DISPLAY LIKE 'E'.
                ENDIF.

              ENDIF.

              CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
                EXPORTING
                  chave = wa_lote_5120-nro_cg.

            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    LEAVE TO SCREEN 5000.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_5120  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5120 OUTPUT.

  PERFORM seleciona_lote.
  PERFORM ajusta_dados_lote.
  PERFORM alv_tela_lote.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5120  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5120 INPUT.

  CASE sy-ucomm.
    WHEN 'LEG'.
      CALL SCREEN 6001 STARTING AT 5 5 ENDING AT 80 20.
    WHEN OTHERS.
  ENDCASE.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  SELECT_LOTE
*&---------------------------------------------------------------------*
FORM seleciona_lote.

*  IF p_filial IS NOT INITIAL.
*
*    SELECT *
*        FROM zsdt0129
*        INTO CORRESPONDING FIELDS OF TABLE it_lote_5120
*        WHERE inco1      IN p_inco1
*          AND nro_lote   IN p_nlote
*          AND data_atual IN p_dataa
*          AND status     NE 'X'
*          AND EXISTS ( SELECT *
*                         FROM zsdt0131
*                        WHERE zsdt0131~nro_lote   EQ zsdt0129~nro_lote
*                          AND zsdt0131~vkorg      IN p_vkorg
*                          AND zsdt0131~vkbur      IN p_vkbur
*                          AND zsdt0131~spart      EQ p_spart
*                          AND zsdt0131~kunnr      IN p_kunnr
*                          AND zsdt0131~vbeln      IN p_ovcor ).
*
*  ELSEIF p_corplt IS NOT INITIAL.
*
*    SELECT *
*        FROM zsdt0129
*        INTO CORRESPONDING FIELDS OF TABLE it_lote_5120
*        WHERE inco1      IN c_inco1
*          AND nro_lote   IN c_nlote
*          AND data_atual IN c_dataa
*          AND status     NE 'X'
*          AND EXISTS ( SELECT *
*                         FROM zsdt0131
*                        WHERE zsdt0131~nro_lote   EQ zsdt0129~nro_lote
*                          AND zsdt0131~vkorg      IN c_vkorg
*                          AND zsdt0131~vkbur      IN c_vkbur
*                          AND zsdt0131~spart      EQ c_spart
*                          AND zsdt0131~kunnr      IN c_kunnr
*                          AND zsdt0131~vbeln      IN c_ovcor ).
*
*  ENDIF.

  IF it_lote_5120 IS NOT INITIAL.

    SELECT *
      FROM lfa1
      INTO TABLE it_lfa1_5120
      FOR ALL ENTRIES IN it_lote_5120
      WHERE lifnr EQ it_lote_5120-motorista.

    SELECT *
      FROM zsdt0133
      INTO TABLE it_zsdt0133_5120
      FOR ALL ENTRIES IN it_lote_5120
      WHERE nro_cg EQ it_lote_5120-nro_cg.

    SELECT *
      FROM zsdt0131
      INTO TABLE it_zsdt0131_5120
      FOR ALL ENTRIES IN it_lote_5120
      WHERE nro_lote EQ it_lote_5120-nro_lote
        AND status NE 'X'.

    IF it_zsdt0131_5120 IS NOT INITIAL.

      SELECT *
        FROM t001w
        INTO TABLE it_t001w_5120
        FOR ALL ENTRIES IN it_zsdt0131_5120
        WHERE werks EQ it_zsdt0131_5120-vkbur.

    ENDIF.

    IF it_zsdt0133_5120 IS NOT INITIAL.

      SELECT *
        FROM lfa1
        INTO TABLE it_lfa1_t_5120
        FOR ALL ENTRIES IN it_zsdt0133_5120
        WHERE lifnr EQ it_zsdt0133_5120-cod_transportadora.

    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_LOTE
*&---------------------------------------------------------------------*
FORM alv_tela_lote .

  IF g_custom_container_5120 IS INITIAL.

    CREATE OBJECT g_custom_container_5120
      EXPORTING
        container_name              = 'CONTAINER5120'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT dg_splitter_1_5120
      EXPORTING
        parent  = g_custom_container_5120
        rows    = 3
        columns = 1.

    CALL METHOD dg_splitter_1_5120->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1_5120.

    CALL METHOD dg_splitter_1_5120->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_2_5120.

    CALL METHOD dg_splitter_1_5120->get_container
      EXPORTING
        row       = 3
        column    = 1
      RECEIVING
        container = dg_parent_3_5120.

    CALL METHOD dg_splitter_1_5120->set_row_mode
      EXPORTING
        mode = dg_splitter_1_5120->mode_relative.

    CALL METHOD dg_splitter_1_5120->set_row_height
      EXPORTING
        id     = 1
        height = 40.

    CALL METHOD dg_splitter_1_5120->set_row_height
      EXPORTING
        id     = 2
        height = 30.

    CALL METHOD dg_splitter_1_5120->set_row_height
      EXPORTING
        id     = 3
        height = 30.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_lote_5120 USING:
          01 'ICONE'          ' '         ' '  ' '  'X'  ' '   'X'   ' '   ' '   ' '   'Status Lote',
          02 'NRO_LOTE'       ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Lote',
          02 'ICONE2'         ' '         ' '  ' '  'X'  ' '   'X'   ' '   ' '   ' '   'Status Carga',
          03 'NRO_CG'         ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Carga',
          04 'INCO1'          ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Tp Frete',
          05 'MARCA'          ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Marca',
          05 'NAME2'          ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Esc. Vendas',
          06 'CTG_TRANSP_D'   ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Ctg Transporte',
          07 'QTD_TOTAL_KG'   ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Qnt Total Kg',
          08 'DT_ENTREGA'     ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Dt. Entrega',
          09 'PLACA_CAV'      ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Cavalo',
          10 'PLACA_CAR1'     ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Carreta I',
          11 'PLACA_CAR2'     ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Carreta II',
*-CS2019001891 - JT - 04.02.2021 - inicio
          12 'PLACA_CAR3'     ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Dolly',
*-CS2019001891 - JT - 04.02.2021 - fim
          13 'MOTORISTA'      'ZSDT0129'  ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Motorista',
          14 'NAME1'          ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Motorista',
          15 'STCD2'          ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'CPF Mot.',
          16 'TELF1'          ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Fone Mot.',
          17 'NAME1_T'        ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Transportadora',
          18 'TELF1_T'        ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Fone Transp.'.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_sol_5120 USING:
           01 'NRO_SOL'       'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Sol',
           02 'VKBUR'         'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Esc. Vendas',
           03 'KUNNR'         'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Cliente',
           04 'NAME1'         'KNA1'         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nome Cliente',
           05 'AUART'         'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Tp. OV',
           06 'VBELN'         'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Ordem Venda',
           07 'POSNR'         'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Item',
           08 'MATNR'         'MARA'         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód Produto',
           09 'MAKTX'         'MAKT'         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Descrição Produto',
           10 'QTD_VINC'      'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'QTD',
           11 'UM'            'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'U.M.',
           12 'QTD_EMKG'      'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd/Kg',
           13 'LIFNR'         'LFA1'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Fornecedor',
           14 'LOCAL_EMBARQ'  'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Desc. Loc. Emb.'.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_cliente_5120 USING:
          01 'NRO_SOL'        'ZSDT0130'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Sol',
          02 'KUNNR'          'MARA'         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Cliente',
          03 'NAME1'          'MAKT'         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nome',
          04 'SEQ_ENTREGA'    'ZSDT0130'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Seq. Entrega',
          05 'NR_ROT2'        ''             ' '  ' '  ' '  'X'   'X'   ' '   ' '   ' '   'Doc Anexo',
          06 'OBS'            ''             ' '  ' '  'X'  ' '   'X'   ' '   ' '   ' '   'Observação',
          07 'NR_ROT1'        ''             ' '  ' '  ' '  'X'   'X'   ' '   ' '   ' '   'Roteiro'.


    gs_layout_5120_alv1-sel_mode   = 'A'.
    gs_layout_5120_alv1-stylefname = 'CELLSTYLES'.
    gs_layout_5120_alv1-cwidth_opt = 'X'.
    gs_layout_5120_alv1-grid_title = 'Lotes'.
    gs_layout_5120_alv1-smalltitle = 'X' .
    gs_layout_5120_alv2-sel_mode   = 'A'.
    gs_layout_5120_alv2-stylefname = 'CELLSTYLES'.
    gs_layout_5120_alv2-cwidth_opt = 'X'.
    gs_layout_5120_alv2-grid_title = 'Ordens de Venda do Lote'.
    gs_layout_5120_alv2-smalltitle = 'X' .
    gs_layout_5120_alv3-sel_mode   = 'A'.
    gs_layout_5120_alv3-stylefname = 'CELLSTYLES'.
    gs_layout_5120_alv3-cwidth_opt = 'X'.
    gs_layout_5120_alv3-grid_title = 'Clientes'.
    gs_layout_5120_alv3-smalltitle = 'X' .

    PERFORM sort USING 'NRO_LOTE' CHANGING it_sort_zsdt0129_alv.

    CREATE OBJECT ctl_alv1_5120
      EXPORTING
        i_parent = dg_parent_1_5120.           "ALV Lote

    CREATE OBJECT ctl_alv2_5120
      EXPORTING
        i_parent = dg_parent_2_5120.           "ALV Oderm

    CREATE OBJECT ctl_alv3_5120
      EXPORTING
        i_parent = dg_parent_3_5120.          "Alv Cliente

    SET HANDLER:
      lcl_event_handler_5120=>on_double_click_5120 FOR ctl_alv1_5120,
      lcl_event_handler_5120=>user_command_5120 FOR ctl_alv1_5120,
      lcl_event_handler_5120=>toolbar_5120 FOR ctl_alv1_5120.

    CALL METHOD ctl_alv1_5120->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_5120_alv1
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_lote_5120
        it_outtab       = it_lote_5120
        it_sort         = it_sort_zsdt0129_alv.

    CALL METHOD ctl_alv2_5120->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_5120_alv2
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_sol_5120
        it_outtab       = it_solicitacao_5120. "IT_SOLICITACAO_5120_CLICK.

    SET HANDLER:
       lcl_event_handler_5120=>handle_hotspot_click_5120 FOR ctl_alv3_5120,
       lcl_event_handler_5120=>handle_button_click_5120 FOR ctl_alv3_5120.

    CALL METHOD ctl_alv3_5120->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_5120_alv3
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_cliente_5120
        it_outtab       = it_cliente_5120. "IT_CLIENTE_5120_CLICK.

  ELSE.

    CALL METHOD ctl_alv1_5120->get_frontend_layout
      IMPORTING
        es_layout = gs_layout_5120_alv1.

    gs_layout_5120_alv1-cwidth_opt = abap_true.

    CALL METHOD ctl_alv1_5120->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_5120_alv1.

    CALL METHOD ctl_alv1_5120->refresh_table_display
      EXPORTING
        is_stable = _stable.

    CALL METHOD ctl_alv2_5120->refresh_table_display
      EXPORTING
        is_stable = _stable.

    CALL METHOD ctl_alv3_5120->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_SOLICITACAO_5120
*&---------------------------------------------------------------------*
FORM alv_solicitacao_5120 USING VALUE(p_e_row).

  DATA: it_makt             TYPE STANDARD TABLE OF makt,
        it_kna1             TYPE STANDARD TABLE OF kna1,
        it_zsdt0132         TYPE STANDARD TABLE OF zsdt0132,
        wa_kna1             TYPE kna1,
        wa_makt             TYPE makt,
        wa_zsdt0132         TYPE zsdt0132,
        wa_lote_5120        TYPE ty_lote_5120,
        wa_solicitacao_5120 TYPE ty_solicitacao_5120,
        vl_cont             TYPE i.

  READ TABLE it_lote_5120 INTO wa_lote_5120 INDEX p_e_row.

  IF wa_lote_5120 IS NOT INITIAL.

    SELECT *
        FROM zsdt0131
        INTO CORRESPONDING FIELDS OF TABLE it_solicitacao_5120
        WHERE nro_lote EQ wa_lote_5120-nro_lote
          AND status NE 'X'.

    IF it_solicitacao_5120 IS NOT INITIAL.

      SELECT *
        FROM makt
        INTO TABLE it_makt
        FOR ALL ENTRIES IN it_solicitacao_5120
        WHERE matnr EQ it_solicitacao_5120-matnr
          AND spras EQ sy-langu.

      SELECT *
        FROM kna1
        INTO TABLE it_kna1
        FOR ALL ENTRIES IN it_solicitacao_5120
        WHERE kunnr EQ it_solicitacao_5120-kunnr.

      SELECT *
        FROM zsdt0132
        INTO TABLE it_zsdt0132
        FOR ALL ENTRIES IN it_solicitacao_5120
        WHERE nr_rot EQ it_solicitacao_5120-cod_loc_emb.

    ENDIF.

  ENDIF.

  LOOP AT it_solicitacao_5120 INTO wa_solicitacao_5120.
    vl_cont = vl_cont + 1.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_solicitacao_5120-kunnr.
    IF sy-subrc IS INITIAL.
      wa_solicitacao_5120-name1 = wa_kna1-name1.
    ENDIF.

    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_solicitacao_5120-matnr.
    IF sy-subrc IS INITIAL.
      wa_solicitacao_5120-maktx = wa_makt-maktx.
    ENDIF.

    READ TABLE it_zsdt0132 INTO wa_zsdt0132 WITH KEY nr_rot = wa_solicitacao_5120-cod_loc_emb.
    IF sy-subrc IS INITIAL.
      wa_solicitacao_5120-lifnr = wa_zsdt0132-lifnr.
    ENDIF.

    MODIFY it_solicitacao_5120 FROM wa_solicitacao_5120 INDEX vl_cont.

  ENDLOOP.

  CLEAR: vl_cont.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_CLIENTE_5120
*&---------------------------------------------------------------------*
FORM alv_cliente_5120 USING VALUE(p_e_row).

  DATA:it_kna1         TYPE STANDARD TABLE OF kna1,
       tg_texto        TYPE STANDARD TABLE OF tline,
       wa_kna1         TYPE kna1,
       wl_name         TYPE thead-tdname,
       ls_style        TYPE lvc_s_styl,
       wa_lote_5120    TYPE ty_lote_5120,
       wa_cliente_5120 TYPE ty_cliente_5120,
       vl_cont         TYPE i.

  READ TABLE it_lote_5120 INTO wa_lote_5120 INDEX p_e_row.

  IF wa_lote_5120 IS NOT INITIAL.

    SELECT *
      FROM zsdt0130
      INTO CORRESPONDING FIELDS OF TABLE it_cliente_5120
      WHERE nro_lote EQ wa_lote_5120-nro_lote
        AND status NE 'X'.

    IF it_cliente_5120 IS NOT INITIAL.

      SORT it_cliente_5120 BY seq_entrega.

      SELECT *
        FROM kna1
        INTO TABLE it_kna1
        FOR ALL ENTRIES IN it_cliente_5120
        WHERE kunnr EQ it_cliente_5120-kunnr.

    ENDIF.

  ENDIF.

  LOOP AT it_cliente_5120 INTO wa_cliente_5120.

    vl_cont = vl_cont + 1.

    ls_style-fieldname = 'OBS'.
    ls_style-style = cl_gui_alv_grid=>mc_style_button.
    APPEND ls_style TO wa_cliente_5120-cellstyles.

    CONCATENATE wa_cliente_5120-nro_sol '001' INTO wl_name.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'OBSE'
        language                = sy-langu
        name                    = wl_name
        object                  = 'ZTEXTO'
      TABLES
        lines                   = tg_texto
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF tg_texto IS INITIAL.
      wa_cliente_5120-obs = '@1F@'.
    ELSE.
      wa_cliente_5120-obs = '@1E@'.
    ENDIF.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_cliente_5120-kunnr.
    IF sy-subrc IS INITIAL.
      wa_cliente_5120-name1 = wa_kna1-name1.
    ENDIF.

    wa_cliente_5120-nr_rot1 = wa_cliente_5120-nr_rot.
    wa_cliente_5120-nr_rot2 = wa_cliente_5120-nr_rot.

    MODIFY it_cliente_5120 FROM wa_cliente_5120 INDEX vl_cont.

  ENDLOOP.

  CLEAR: vl_cont.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_GERAR_CARGA
*&---------------------------------------------------------------------*
FORM check_gerar_carga CHANGING vl_check.

  DATA: wa_sel_rows_lote_5120       TYPE lvc_s_row,
        wa_lote_5120                TYPE ty_lote_5120,
        it_zsdt0131_check_lote_5120 TYPE STANDARD TABLE OF zsdt0131,
        wa_zsdt0131_check_lote_5120 TYPE zsdt0131,
        vl_inco1                    TYPE zsdt0129-inco1,
        wa_zsdt0129                 TYPE zsdt0129.

  CLEAR: it_zsdt0131_check_lote_5120, wa_zsdt0131_check_lote_5120,
         it_lote_5120_check, vg_qtd_kg, vg_unico_lote, wa_zsdt0129.

  CALL METHOD ctl_alv1_5120->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows_lote_5120.

  IF it_sel_rows_lote_5120 IS NOT INITIAL.

    LOOP AT it_lote_5120 INTO  wa_lote_5120.
      READ TABLE it_sel_rows_lote_5120 INTO wa_sel_rows_lote_5120 WITH KEY index = sy-tabix.
      IF sy-subrc IS INITIAL.
        APPEND  wa_lote_5120 TO it_lote_5120_check.
      ENDIF.
    ENDLOOP.

    "Check Incoterms Diferentes e se Lotes estão Aprovados (Status 1)
    LOOP AT it_lote_5120_check INTO wa_lote_5120.

      SELECT SINGLE *
        FROM zsdt0129
        INTO wa_zsdt0129
        WHERE nro_lote EQ wa_lote_5120-nro_lote.

      IF wa_lote_5120-status NE wa_zsdt0129-status.
        MESSAGE text-096 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
      ELSEIF wa_zsdt0129-status NE 1.
        MESSAGE text-035 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
      ENDIF.
      IF sy-tabix EQ 1.
        vl_inco1 = wa_lote_5120-inco1.
      ENDIF.
      IF wa_lote_5120-inco1 NE vl_inco1.
        MESSAGE text-030 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
      ENDIF.
      vl_inco1 = wa_lote_5120-inco1.
    ENDLOOP.
    CLEAR: vl_inco1.

    "Check Carga FOB com + de 1 Lote
    LOOP AT it_lote_5120_check INTO wa_lote_5120 WHERE inco1 EQ 'FOB'.
      IF sy-tabix EQ 1.
        vl_inco1 = wa_lote_5120-inco1.
      ENDIF.
      IF wa_lote_5120-inco1 EQ vl_inco1 AND sy-tabix GT 1.
        MESSAGE text-031 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
      ENDIF.
      vl_inco1 = wa_lote_5120-inco1.
    ENDLOOP.
    CLEAR: vl_inco1.

    SELECT *
      FROM zsdt0131
      INTO TABLE it_zsdt0131_check_lote_5120
      FOR ALL ENTRIES IN it_lote_5120_check
      WHERE nro_lote EQ it_lote_5120_check-nro_lote
        AND status NE 'X'.

    "Check se Local de Embarque está em Branco e +50.000 kg de carga
    LOOP AT it_zsdt0131_check_lote_5120 INTO wa_zsdt0131_check_lote_5120.
      vg_qtd_kg = vg_qtd_kg + wa_zsdt0131_check_lote_5120-qtd_emkg.
      IF vg_qtd_kg GT 50000.
        MESSAGE text-033 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
      ENDIF.
      IF wa_zsdt0131_check_lote_5120-cod_loc_emb IS INITIAL.
        MESSAGE s000(z_fi) WITH 'Lote' wa_zsdt0131_check_lote_5120-nro_lote 'sem Local de Embarque' DISPLAY LIKE 'E'.
        vl_check = abap_true.
      ENDIF.
    ENDLOOP.

  ELSE.
    MESSAGE text-034 TYPE 'S' DISPLAY LIKE 'E'.
    vl_check = abap_true.
  ENDIF.

  IF vl_check IS INITIAL.

    DESCRIBE TABLE it_lote_5120_check LINES vg_unico_lote.

    IF vg_unico_lote EQ 1.
      READ TABLE it_lote_5120_check INTO wa_lote_5120 INDEX 1.
      vg_dt_ent_unico_lote = wa_lote_5120-dt_entrega.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CLEAR_5120
*&---------------------------------------------------------------------*
FORM clear_5120 .
  CLEAR: it_solicitacao_5120, it_cliente_5120.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  AJUSTA_DADOS_LOTE
*&---------------------------------------------------------------------*
FORM ajusta_dados_lote .

  DATA: wa_lote_5120 TYPE ty_lote_5120,
        wa_zsdt0133  TYPE zsdt0133,
        wa_lfa1      TYPE lfa1,
        wa_t001w     TYPE t001w,
        wa_zsdt0131  TYPE zsdt0131,
        vl_cont      TYPE i.

  LOOP AT it_lote_5120 INTO wa_lote_5120.

    vl_cont = vl_cont + 1.

    READ TABLE it_lfa1_5120 INTO wa_lfa1 WITH KEY lifnr = wa_lote_5120-motorista.
    IF sy-subrc IS INITIAL.
      wa_lote_5120-name1 = wa_lfa1-name1.
      wa_lote_5120-stcd2 = wa_lfa1-stcd2.
      wa_lote_5120-telf1 = wa_lfa1-telf1.
    ENDIF.

    READ TABLE it_zsdt0133_5120 INTO wa_zsdt0133 WITH KEY nro_cg = wa_lote_5120-nro_cg.
    IF sy-subrc IS INITIAL.

      IF wa_zsdt0133-status EQ 2.
        wa_lote_5120-icone2 = '@5D@'.
      ELSEIF wa_zsdt0133-status EQ 3.
        wa_lote_5120-icone2 = '@FD@'.
      ELSEIF wa_zsdt0133-status EQ 4.
        wa_lote_5120-icone2 = '@4A@'.
      ELSEIF wa_zsdt0133-status EQ 5.
        wa_lote_5120-icone2 = '@96@'.
      ELSEIF wa_zsdt0133-status EQ 6.
        wa_lote_5120-icone2 = '@0Q@'.
      ENDIF.

      READ TABLE it_lfa1_t_5120 INTO wa_lfa1 WITH KEY lifnr = wa_zsdt0133-cod_transportadora.
      IF sy-subrc IS INITIAL.
        wa_lote_5120-name1_t = wa_lfa1-name1.
        wa_lote_5120-telf1_t = wa_lfa1-telf1.
      ENDIF.
    ENDIF.

    IF wa_lote_5120-ctg_transp EQ 'A'.
      wa_lote_5120-ctg_transp_d = 'AVULSO'.
    ELSEIF wa_lote_5120-ctg_transp EQ 'B'.
      wa_lote_5120-ctg_transp_d = 'VEÍCULO 27'.
    ELSEIF wa_lote_5120-ctg_transp EQ 'C'.
      wa_lote_5120-ctg_transp_d = 'VEÍCULO 32'.
    ELSEIF wa_lote_5120-ctg_transp EQ 'D'.
      wa_lote_5120-ctg_transp_d = 'VEÍCULO 37'.
    ELSEIF wa_lote_5120-ctg_transp EQ 'E'.
      wa_lote_5120-ctg_transp_d = 'VEÍCULO 50'.
    ENDIF.

    IF wa_lote_5120-status EQ 0.
      wa_lote_5120-icone = '@5C@'.
    ELSEIF wa_lote_5120-status EQ 1.
      wa_lote_5120-icone = '@5B@'.
    ELSEIF wa_lote_5120-status EQ 2.
      wa_lote_5120-icone = '@7E@'.
    ELSEIF wa_lote_5120-status EQ 3.
      wa_lote_5120-icone = '@5D@'.
    ELSEIF wa_lote_5120-status EQ 4.
      wa_lote_5120-icone = '@7Q@'.
    ENDIF.

    READ TABLE it_zsdt0131_5120 INTO wa_zsdt0131 WITH KEY nro_lote = wa_lote_5120-nro_lote.
    IF sy-subrc IS INITIAL.
      READ TABLE it_t001w_5120 INTO wa_t001w WITH KEY werks = wa_zsdt0131-vkbur.
      IF sy-subrc IS INITIAL.
        wa_lote_5120-name2 = wa_t001w-name1.
      ENDIF.
    ENDIF.

    MODIFY it_lote_5120 FROM wa_lote_5120 INDEX vl_cont.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_LOTES_PRODUTOS_5230
*&---------------------------------------------------------------------*
FORM busca_lotes_produtos_5120 CHANGING vl_check.

  DATA: it_zsdt0131 TYPE STANDARD TABLE OF zsdt0131,
        it_zsdt0134 TYPE STANDARD TABLE OF zsdt0134,
        it_zsdt0129 TYPE STANDARD TABLE OF zsdt0129,
        wa_zsdt0134 TYPE zsdt0134,
        vl_cont     TYPE i.

  SELECT *
    FROM zsdt0129
    INTO TABLE it_zsdt0129
    FOR ALL ENTRIES IN it_carga_canc_5120
    WHERE nro_cg EQ it_carga_canc_5120-nro_cg
      AND status NE 'X'.

  SELECT *
    FROM zsdt0131
    INTO TABLE it_zsdt0131
    FOR ALL ENTRIES IN it_zsdt0129
    WHERE nro_lote EQ it_zsdt0129-nro_lote
      AND status   NE 'X'.

  SELECT *
    FROM zsdt0134
    INTO TABLE it_zsdt0134
    FOR ALL ENTRIES IN it_zsdt0131
    WHERE vbeln  EQ it_zsdt0131-vbeln
      AND posnr  EQ it_zsdt0131-posnr
      AND status NE 'X'.

  LOOP AT it_zsdt0134 INTO wa_zsdt0134.
    vl_cont = vl_cont + 1.
    READ TABLE it_carga_canc_5120 WITH KEY nro_cg = wa_zsdt0134-nro_cg TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      wa_zsdt0134-status = 'K'.
    ENDIF.
    MODIFY it_zsdt0134 FROM wa_zsdt0134 INDEX vl_cont TRANSPORTING status.
  ENDLOOP.

  DELETE it_zsdt0134 WHERE status EQ 'K'.

  IF it_zsdt0134 IS NOT INITIAL.
    vl_check = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CANCELA_AUTORIZACAO_5230
*&---------------------------------------------------------------------*
FORM cancela_autorizacao_5120 .

  DATA: it_zsdt0133 TYPE STANDARD TABLE OF zsdt0133,
        wa_zsdt0133 TYPE zsdt0133.

  SELECT *
    FROM zsdt0133
    INTO TABLE it_zsdt0133
    FOR ALL ENTRIES IN it_carga_canc_5120
    WHERE nro_cg EQ it_carga_canc_5120-nro_cg
      AND status NE 'X'.

  LOOP AT it_zsdt0133 INTO wa_zsdt0133.
    IF wa_zsdt0133-status EQ 5.
      wa_zsdt0133-status = 4.
    ENDIF.
    MODIFY zsdt0133 FROM wa_zsdt0133.
  ENDLOOP.

ENDFORM.
