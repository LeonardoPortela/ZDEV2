*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5820
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_carga_5820,
         icone             TYPE char6,
         name1             TYPE lfa1-name1,
         name2             TYPE lfa1-name1,
         name4             TYPE lfa1-name1,
         nome_rtc          TYPE zsdt0259-nome,
         tipo_rtc2         TYPE char20, "*-CS2021000218-31.08.2022-#89492-JT-inicio
         solic_receit      TYPE char6,  "*-CS2021000218-31.08.2022-#90368-JT-inicio
         receituario       TYPE char6,  "*-CS2021000218-31.08.2022-#90368-JT-inicio
         cor(4)            TYPE c,
         desc_status_icone TYPE icon-name. " User Story 153510 // MMSILVA
         INCLUDE STRUCTURE zsdt0139.
TYPES: END OF ty_carga_5820.

TYPES: BEGIN OF ty_ordem_5820,
         nro_cgd     TYPE zsdt0139-nro_cgd,
         inco1       TYPE zsdt0140-inco1,
         matnr       TYPE makt-matnr,
         maktx       TYPE makt-maktx,
         qtd_vinc_lt TYPE zsdt0131-qtd_vinc,
         meins       TYPE vbap-meins,
         qtd_emkg    TYPE zsdt0131-qtd_emkg,
         lgort       TYPE zsdt0134-lgort, "RJF - #124531
         cor(4)      TYPE c,
         lock        TYPE char1.  "*-CS2021000218-14.10.2022-#91701-JT-inicio
         INCLUDE STRUCTURE zsdt0082.
TYPES: END OF ty_ordem_5820.

TYPES: BEGIN OF ty_nlote_5820,
         edit       TYPE char1.
         INCLUDE STRUCTURE zsdt0134.
TYPES:   cellstyles TYPE lvc_t_styl.
TYPES: END OF ty_nlote_5820.

DATA: g_custom_container_5820    TYPE REF TO cl_gui_custom_container,
      dg_splitter_1_5820         TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2_5820         TYPE REF TO cl_gui_splitter_container,
      dg_parent_1_5820           TYPE REF TO cl_gui_container,
      dg_parent_2_5820           TYPE REF TO cl_gui_container,
      dg_parent_3_5820           TYPE REF TO cl_gui_container,
      dg_parent_4_5820           TYPE REF TO cl_gui_container,
      ctl_alv1_5820              TYPE REF TO cl_gui_alv_grid,
      ctl_alv2_5820              TYPE REF TO cl_gui_alv_grid,
      ctl_alv3_5820              TYPE REF TO cl_gui_alv_grid,
      gs_layout_5820_alv1        TYPE lvc_s_layo,
      gs_layout_5820_alv2        TYPE lvc_s_layo,
      gs_layout_5820_alv3        TYPE lvc_s_layo,
      it_fieldcatalog_carga_5820 TYPE lvc_t_fcat,
      it_fieldcatalog_ordem_5820 TYPE lvc_t_fcat,
      it_fieldcatalog_nlote_5820 TYPE lvc_t_fcat,
      it_sort_5820               TYPE lvc_t_sort,
      it_exclude_nlote_5820      TYPE ui_functions.

DATA: it_carga_5820       TYPE STANDARD TABLE OF ty_carga_5820,
      it_ordem_5820       TYPE STANDARD TABLE OF ty_ordem_5820,
      it_ordem_check_5820 TYPE STANDARD TABLE OF ty_ordem_5820,
      it_nlote_5820       TYPE STANDARD TABLE OF ty_nlote_5820,
      it_carga_aux_5820   TYPE STANDARD TABLE OF ty_carga_5820.

DATA: vg_carga_5820    TYPE ty_carga_5820,
      vg_ordem_5820    TYPE ty_ordem_5820,
      vg_nlote_nro_cgd TYPE zsdt0139-nro_cgd.

DATA: vg_tela_5821 TYPE char1.

DATA: w_ordem_carga      TYPE zsdt0304,           "*-CS2021000218-14.10.2022-#91701-JT-inicio
      t_retorno          TYPE TABLE OF zsdt0134,  "*-CS2021000218-14.10.2022-#91701-JT-inicio
      l_click_ordem      TYPE char1,              "*-CS2021000218-14.10.2022-#91701-JT-inicio
      l_tabix2           TYPE sy-tabix,           "*-CS2021000218-14.10.2022-#91701-JT-inicio
      l_tabix_ordem_5820 TYPE sy-tabix,           "*-CS2021000218-14.10.2022-#91701-JT-inicio
      l_tot_lfimg        TYPE zsdt0134-lfimg,     "*-CS2021000218-14.10.2022-#91701-JT-inicio
      l_lgort            TYPE zsdt0134-lgort,     " RJF - #124531
      l_erro             TYPE char1.              "*-CS2021000218-14.10.2022-#91701-JT-inicio

*-CS2021000218-27.10.2022-#91184-JT-inicio
DATA: it_zsdt0302 TYPE TABLE OF zsdt0302,
      wa_zsdt0302 TYPE zsdt0302,
      it_zsdt0298 TYPE TABLE OF zsdt0298,
      wa_zsdt0298 TYPE zsdt0298.
*-CS2021000218-27.10.2022-#91184-JT-fim

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler_5820 DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      toolbar_5820 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      user_command_5820 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      data_changed_finished_5820 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      on_double_click_5820 FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      on_double_click_ordem_5820 FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      toolbar_nlote_5820 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      on_f4_5820 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING sender
                  e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler_5820 IMPLEMENTATION.

  METHOD toolbar_5820.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'ROMANEIO'.
    wa_tool-icon     = '@0Q@'.
    wa_tool-quickinfo = 'Gerar Romaneio'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'LISTAROM'.
    wa_tool-icon     = '@D8@'.
    wa_tool-quickinfo = 'Listar Romaneios'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

*-CS2021000218-31.08.2022-#89492-JT-inicio
    wa_tool-function = 'EDITARTC'.
    wa_tool-icon     = icon_change_password.
    wa_tool-quickinfo = 'Editar RTC'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.
*-CS2021000218-31.08.2022-#89492-JT-inicio

  ENDMETHOD.             "DISPLAY

  METHOD toolbar_nlote_5820.

    DATA wa_tool TYPE stb_button.
    DATA: wa_ordem_5820       TYPE ty_ordem_5820.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

*-CS2021000218-14.10.2022-#91701-JT-inicio
*    wa_tool-function = 'ADD_ROW'.
*    wa_tool-icon     = '@17@'.
*    wa_tool-quickinfo = 'Adicionar Linha'.
*    APPEND wa_tool TO e_object->mt_toolbar.
*    CLEAR wa_tool.
*
*    wa_tool-function = 'DEL_ROW'.
*    wa_tool-icon     = '@18@'.
*    wa_tool-quickinfo = 'Eliminar Linha'.
*    APPEND wa_tool TO e_object->mt_toolbar.
*    CLEAR wa_tool.
*-CS2021000218-14.10.2022-#91701-JT-fim

*-CS2021000218-14.10.2022-#91701-JT-inicio
    READ TABLE it_ordem_5820 INTO wa_ordem_5820 WITH KEY nro_cgd = vg_ordem_5820-nro_cgd
                                                         vbeln   = vg_ordem_5820-vbeln
                                                         posnr   = vg_ordem_5820-posnr
                                                         nr_rot  = vg_ordem_5820-nr_rot.

    wa_tool-function  = 'EDIT_ROW'.

    IF wa_ordem_5820-lock   = abap_true OR
       vg_carga_5820-status = 2.
      wa_tool-icon      = '@10@'.
      wa_tool-quickinfo = 'Visualizar Linha'.
      wa_tool-text      = 'Visualizar Lote'.
    ELSE.
      wa_tool-icon      = '@0Z@'.
      wa_tool-quickinfo = 'Editar Linha'.
      wa_tool-text      = 'Editar Lote'.
    ENDIF.
*-CS2021000218-14.10.2022-#91701-JT-fim

*-CS2021000218-14.10.2022-#91701-JT-inicio
    IF l_click_ordem = abap_false.
      wa_tool-disabled  = abap_true.
    ELSE.
      wa_tool-disabled  = abap_false.
    ENDIF.
*-CS2021000218-14.10.2022-#91701-JT-fim

    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

*-CS2021000218-14.10.2022-#91701-JT-inicio
*    MOVE 3 TO wa_tool-butn_type.
*    APPEND wa_tool TO e_object->mt_toolbar.
*    CLEAR wa_tool.
*
*    wa_tool-function = 'SAVE_NLOTE'.
*    wa_tool-icon     = '@2L@'.
*    wa_tool-quickinfo = 'Salvar'.
*    APPEND wa_tool TO e_object->mt_toolbar.
*    CLEAR wa_tool.
*-CS2021000218-14.10.2022-#91701-JT-fim

  ENDMETHOD.             "DISPLAY

  METHOD user_command_5820.

    DATA: wa_nlote_5820    TYPE ty_nlote_5820,
          wa_ordem_5820    TYPE ty_ordem_5820,
          wa_carga_5820    TYPE ty_carga_5820,
          it_selected_rows TYPE lvc_t_row,
          wa_selected_rows TYPE lvc_s_row,
          vl_lfimg         TYPE zsdt0134-lfimg,
          vl_check_rom     TYPE char1,
          vl_check_initial TYPE char1,
          vl_lines         TYPE i,
          l_excluiu_rom    TYPE char1.

    IF e_ucomm = 'ROMANEIO'.

      CLEAR: it_selected_rows, wa_selected_rows, vl_check_rom, it_carga_aux_5820, vl_lines, wa_carga_5820.

      CALL METHOD ctl_alv1_5820->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      DESCRIBE TABLE it_selected_rows LINES vl_lines.

      IF vl_lines NE 1.
        MESSAGE TEXT-103 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
        READ TABLE it_carga_5820 INTO wa_carga_5820 INDEX wa_selected_rows-index.
        IF sy-subrc IS INITIAL.

          CLEAR: vl_status.

          SELECT SINGLE status
            FROM zsdt0139
            INTO vl_status
            WHERE nro_cgd EQ wa_carga_5820-nro_cgd.

          IF wa_carga_5820-status NE vl_status.
            MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
          ELSEIF vl_status NE 1.
            MESSAGE TEXT-100 TYPE 'S' DISPLAY LIKE 'E'.
          ELSE.

            CALL FUNCTION 'ZENQUEUE_SD_CARGAD_INSUMOS'
              EXPORTING
                chave          = wa_carga_5820-nro_cgd
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            ELSE.
              APPEND wa_carga_5820 TO it_carga_aux_5820.
            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

      IF it_carga_aux_5820 IS NOT INITIAL.

        PERFORM check_ordens_rom_5820 USING sy-tabix CHANGING vl_check_rom.

        IF vl_check_rom IS INITIAL.
          PERFORM salva_romaneios_5820 CHANGING vl_check_rom.

          IF vl_check_rom IS INITIAL.

            CLEAR wa_carga_5820.
            READ TABLE it_carga_aux_5820 INTO wa_carga_5820 INDEX 1.

            wa_carga_5820-status = 2.
            wa_carga_5820-icone  = '@0Q@'.
            PERFORM ajusta_status_receita    USING '2'
                                          CHANGING wa_carga_5820.
            MODIFY it_carga_5820 FROM wa_carga_5820 INDEX wa_selected_rows-index.
          ENDIF.

        ENDIF.

        CALL FUNCTION 'ZDENQUEUE_SD_CARGAD_INSUMOS'
          EXPORTING
            chave = wa_carga_5820-nro_cgd.

        CALL METHOD ctl_alv1_5820->refresh_table_display
          EXPORTING
            is_stable = _stable.

      ENDIF.

    ELSEIF e_ucomm = 'LISTAROM'.

      CLEAR: it_selected_rows, wa_selected_rows, vg_cg_para_rom, vg_tela_5821, vl_lines, wa_carga_5820.

      CALL METHOD ctl_alv1_5820->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      DESCRIBE TABLE it_selected_rows LINES vl_lines.

      IF vl_lines NE 1.
        MESSAGE TEXT-045 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.

        READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
        READ TABLE it_carga_5820 INTO wa_carga_5820 INDEX wa_selected_rows-index.

        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'ZENQUEUE_SD_CARGAD_INSUMOS'
            EXPORTING
              chave          = wa_carga_5820-nro_cgd
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
          ELSE.

            vg_cg_para_rom = wa_carga_5820-nro_cgd.

*-CS2021000218-24.10.2022-#90368-JT-inicio
            CALL FUNCTION 'ZSD_POPUP_LISTAR_ROMANEIOS'
              EXPORTING
                i_nro_cg           = vg_cg_para_rom
              IMPORTING
                e_excluiu_romaneio = l_excluiu_rom.

            IF l_excluiu_rom = abap_true.
              READ TABLE it_carga_5820 INTO wa_carga_5820 WITH KEY nro_cgd = vg_cg_para_rom.
              IF sy-subrc IS INITIAL.
                l_tabix2 = sy-tabix.
                wa_carga_5820-status = 1.
                wa_carga_5820-icone  = '@5B@'.
                PERFORM ajusta_status_receita    USING '2'
                                              CHANGING wa_carga_5820.
                MODIFY it_carga_5820 FROM wa_carga_5820 INDEX l_tabix2.
              ENDIF.
            ELSE.
              READ TABLE it_carga_5820 INTO wa_carga_5820 WITH KEY nro_cgd = vg_cg_para_rom.
              IF sy-subrc IS INITIAL.
                l_tabix2 = sy-tabix.
                PERFORM ajusta_status_receita    USING '2'
                                              CHANGING wa_carga_5820.
                MODIFY it_carga_5820 FROM wa_carga_5820 INDEX l_tabix2.
              ENDIF.
            ENDIF.

            CALL METHOD ctl_alv1_5820->get_frontend_layout
              IMPORTING
                es_layout = gs_layout_5820_alv1.

            gs_layout_5820_alv1-cwidth_opt = abap_true.

            CALL METHOD ctl_alv1_5820->set_frontend_layout
              EXPORTING
                is_layout = gs_layout_5820_alv1.

            CALL METHOD ctl_alv1_5820->refresh_table_display
              EXPORTING
                is_stable = _stable.

*           CALL SCREEN 5821 STARTING AT 5 5 ENDING AT 80 20.
*-CS2021000218-24.10.2022-#90368-JT-fim

          ENDIF.

        ENDIF.

      ENDIF.

    ELSEIF e_ucomm = 'ADD_ROW'.

      IF vg_ordem_5820 IS NOT INITIAL.

        CLEAR: wa_nlote_5820.
        wa_nlote_5820-edit    = abap_true.
        wa_nlote_5820-vbeln   = vg_ordem_5820-vbeln.
        wa_nlote_5820-posnr   = vg_ordem_5820-posnr.
*-CS2019001891 - JT - 04.02.2021 - inicio
        wa_nlote_5820-nr_rot  = vg_ordem_5820-nr_rot.
*-CS2019001891 - JT - 04.02.2021 - fim
        wa_nlote_5820-nro_cg  = vg_ordem_5820-nro_cgd.
        APPEND wa_nlote_5820 TO it_nlote_5820.
        PERFORM bloqueia_linhas_nlote_5820.

        CALL METHOD ctl_alv3_5820->refresh_table_display
          EXPORTING
            is_stable = _stable.
      ENDIF.

    ELSEIF e_ucomm = 'DEL_ROW'.

      CLEAR: it_selected_rows, wa_selected_rows.

      CALL METHOD ctl_alv3_5820->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      LOOP AT it_selected_rows INTO wa_selected_rows.
        READ TABLE it_nlote_5820 INTO wa_nlote_5820 INDEX wa_selected_rows-index.
        IF sy-subrc IS INITIAL.
          wa_nlote_5820-charg = 'DELETE'.
          MODIFY it_nlote_5820 FROM wa_nlote_5820 INDEX wa_selected_rows-index..
          CLEAR: wa_nlote_5820.
        ENDIF.
      ENDLOOP.

      DELETE it_nlote_5820 WHERE charg EQ 'DELETE'.
      CALL METHOD ctl_alv3_5820->refresh_table_display
        EXPORTING
          is_stable = _stable.

    ELSEIF e_ucomm = 'EDIT_ROW'.

*-CS2021000218-14.10.2022-#91701-JT-inicio
*     IF vg_carga_5820-status = 2.
*       MESSAGE s024(sd) WITH 'Não é possível editar Carga com Romaneio Gerado!' DISPLAY LIKE 'E'.
*       EXIT.
*     ENDIF.
*
*     IF l_click_ordem = abap_false.
*       MESSAGE s024(sd) WITH 'Escolher uma Ordem de Carga!' DISPLAY LIKE 'E'.
*       EXIT.
*     ENDIF.

      READ TABLE it_ordem_5820 INTO wa_ordem_5820 WITH KEY nro_cgd = vg_ordem_5820-nro_cgd
                                                           vbeln   = vg_ordem_5820-vbeln
                                                           posnr   = vg_ordem_5820-posnr
                                                           nr_rot  = vg_ordem_5820-nr_rot.
      l_tabix_ordem_5820 = sy-tabix.

      CHECK sy-subrc = 0.

      MOVE-CORRESPONDING wa_ordem_5820         TO w_ordem_carga.
      MOVE               vg_carga_5820-status  TO w_ordem_carga-status.

      CALL FUNCTION 'ZSD_POPUP_LOTES_MATERIAL'
        EXPORTING
          i_ordem_carga = w_ordem_carga.
*-CS2021000218-14.10.2022-#91701-JT-fim

*-CS2021000218-14.10.2022-#91701-JT-inicio
      SELECT *
        FROM zsdt0134
        INTO CORRESPONDING FIELDS OF TABLE it_nlote_5820
        WHERE nro_cg EQ wa_ordem_5820-nro_cgd
          AND vbeln  EQ wa_ordem_5820-vbeln
          AND posnr  EQ wa_ordem_5820-posnr
          AND nr_rot EQ wa_ordem_5820-nr_rot
          AND status NE 'X'.

      IF sy-subrc <> 0.
        SELECT *
          FROM zsdt0134
          INTO CORRESPONDING FIELDS OF TABLE it_nlote_5820
          WHERE nro_cg EQ wa_ordem_5820-nro_cgd
            AND vbeln  EQ wa_ordem_5820-vbeln
            AND posnr  EQ wa_ordem_5820-posnr
            AND nr_rot EQ ''
            AND status NE 'X'.


        LOOP AT it_nlote_5820  INTO wa_nlote_5820.
          wa_nlote_5820-nr_rot    = wa_ordem_5820-nr_rot.
          MODIFY it_nlote_5820 FROM wa_nlote_5820 INDEX sy-tabix.
        ENDLOOP.
      ENDIF.

*      SELECT vbeln, posnr, lgort
*        FROM vbap
*        INTO TABLE @DATA(gt_vbap)
*        WHERE vbeln EQ @wa_ordem_5820-vbeln
*          AND posnr EQ @wa_ordem_5820-posnr.

      FREE: l_tot_lfimg.
      LOOP AT it_nlote_5820 INTO wa_nlote_5820.
        IF wa_nlote_5820-lgort IS NOT INITIAL. " RJF - #124531
          l_lgort = wa_nlote_5820-lgort.       " RJF - #124531
        ENDIF.                                 " RJF - #124531
        l_tot_lfimg = l_tot_lfimg + wa_nlote_5820-lfimg.
      ENDLOOP.
*      wa_ordem_5820-lgort = l_lgort. " RJF - #124531
      wa_ordem_5820-qtd_vinc_lt = l_tot_lfimg.
      MODIFY it_ordem_5820   FROM wa_ordem_5820 INDEX l_tabix_ordem_5820.

*      CLEAR: it_selected_rows, wa_selected_rows.
*
*      CALL METHOD ctl_alv3_5820->get_selected_rows
*        IMPORTING
*          et_index_rows = it_selected_rows.
*
*      LOOP AT it_selected_rows INTO wa_selected_rows.
*        READ TABLE it_nlote_5820 INTO wa_nlote_5820 INDEX wa_selected_rows-index.
*        IF sy-subrc IS INITIAL.
*          MOVE abap_true TO wa_nlote_5820-edit.
*          MODIFY it_nlote_5820 FROM wa_nlote_5820 INDEX wa_selected_rows-index..
*          CLEAR: wa_nlote_5820.
*        ENDIF.
*      ENDLOOP.
*
      PERFORM bloqueia_linhas_nlote_5820.
*-CS2021000218-14.10.2022-#91701-JT-fim

      CALL METHOD ctl_alv2_5820->refresh_table_display
        EXPORTING
          is_stable = _stable.

      CALL METHOD ctl_alv3_5820->refresh_table_display
        EXPORTING
          is_stable = _stable.

*-CS2021000218-31.08.2022-#89492-JT-inicio
    ELSEIF e_ucomm = 'EDITARTC'.

      CLEAR: it_selected_rows, wa_selected_rows.

      CALL METHOD ctl_alv1_5820->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      DESCRIBE TABLE it_selected_rows LINES vl_lines.

      IF vl_lines <> 1.
        MESSAGE TEXT-045 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
        READ TABLE it_carga_5820    INTO wa_carga_5820    INDEX wa_selected_rows-index.

        CALL FUNCTION 'ZENQUEUE_SD_CARGAD_INSUMOS'
          EXPORTING
            chave          = wa_carga_5820-nro_cgd
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
        ELSE.
          PERFORM f_edita_rtc CHANGING wa_carga_5820.

          MODIFY it_carga_5820 FROM wa_carga_5820 INDEX wa_selected_rows-index TRANSPORTING nome_rtc tipo_rtc tipo_rtc2.

          CALL FUNCTION 'ZDENQUEUE_SD_CARGAD_INSUMOS'
            EXPORTING
              chave = wa_carga_5820-nro_cgd.

          CALL METHOD ctl_alv1_5820->refresh_table_display
            EXPORTING
              is_stable = _stable.
        ENDIF.
      ENDIF.
*-CS2021000218-31.08.2022-#89492-JT-fim

    ELSEIF e_ucomm = 'SAVE_NLOTE'.

      DATA: wa_zsdt0134 TYPE zsdt0134,
            it_zsdt0134 TYPE STANDARD TABLE OF zsdt0134.

      IF vg_carga_5820-status EQ 1.

        CLEAR: vl_check_initial.

        "Check se a quantidade de Lotes a serem salvos não ultrapassa o volume vinculado
        "ou dados em branco
        LOOP AT it_nlote_5820 INTO wa_nlote_5820.

          IF wa_nlote_5820-lgort IS NOT INITIAL. " RJF - #124531
            l_lgort = wa_nlote_5820-lgort.       " RJF - #124531
          ENDIF.                                 " RJF - #124531

          IF wa_nlote_5820-charg IS INITIAL OR
             wa_nlote_5820-lfimg IS INITIAL.
            vl_check_initial = abap_true.
          ENDIF.
          vl_lfimg = vl_lfimg + wa_nlote_5820-lfimg.
        ENDLOOP.

        READ TABLE it_ordem_5820 INTO wa_ordem_5820 WITH KEY nro_cgd = vg_ordem_5820-nro_cgd
                                                             vbeln  = vg_ordem_5820-vbeln
                                                             posnr  = vg_ordem_5820-posnr
                                                             nr_rot = vg_ordem_5820-nr_rot.

        IF vl_lfimg LE wa_ordem_5820-qte_lib AND vl_check_initial NE abap_true.

          LOOP AT it_ordem_5820 INTO wa_ordem_5820 WHERE nro_cgd EQ vg_ordem_5820-nro_cgd
                                                     AND vbeln  EQ vg_ordem_5820-vbeln
                                                     AND posnr  EQ vg_ordem_5820-posnr
                                                     AND nr_rot EQ vg_ordem_5820-nr_rot.
            wa_ordem_5820-lgort = l_lgort. " RJF - #124531
            wa_ordem_5820-qtd_vinc_lt = vl_lfimg.
            MODIFY it_ordem_5820 FROM wa_ordem_5820 INDEX sy-tabix.
          ENDLOOP.

          LOOP AT it_nlote_5820 INTO wa_nlote_5820 WHERE edit EQ abap_true.
            wa_zsdt0134-nro_cg    = wa_nlote_5820-nro_cg.
            wa_zsdt0134-vbeln      = wa_nlote_5820-vbeln.
            wa_zsdt0134-posnr      = wa_nlote_5820-posnr.
*-CS2019001891 - JT - 04.02.2021 - inicio
            wa_zsdt0134-nr_rot     = wa_nlote_5820-nr_rot.
*-CS2019001891 - JT - 04.02.2021 - fim
            wa_zsdt0134-charg      = wa_nlote_5820-charg.
            wa_zsdt0134-lfimg      = wa_nlote_5820-lfimg.
            wa_zsdt0134-status     = wa_nlote_5820-status.
            wa_zsdt0134-usnam      = wa_nlote_5820-usnam.
            wa_zsdt0134-data_atual = wa_nlote_5820-data_atual.
            wa_zsdt0134-hora_atual = wa_nlote_5820-hora_atual.
            MODIFY zsdt0134 FROM wa_zsdt0134.
            wa_nlote_5820-edit = abap_false.
            MODIFY it_nlote_5820 FROM wa_nlote_5820 INDEX sy-tabix.
          ENDLOOP.

          SELECT *
            FROM zsdt0134
            INTO TABLE it_zsdt0134
            WHERE nro_cg EQ vg_ordem_5820-nro_cgd
              AND vbeln  EQ vg_ordem_5820-vbeln
              AND posnr  EQ vg_ordem_5820-posnr
              AND nr_rot EQ vg_ordem_5820-nr_rot.

          LOOP AT it_zsdt0134 INTO wa_zsdt0134.
            READ TABLE it_nlote_5820 INTO wa_nlote_5820 WITH KEY nro_cg  = wa_zsdt0134-nro_cg
                                                                  vbeln  = wa_zsdt0134-vbeln
                                                                  posnr  = wa_zsdt0134-posnr
                                                                  nr_rot = wa_zsdt0134-nr_rot
                                                                  charg  = wa_zsdt0134-charg.
            IF sy-subrc IS NOT INITIAL.
              wa_zsdt0134-status = 'X'.
              wa_zsdt0134-user_canc = sy-uname.
              wa_zsdt0134-dt_canc = sy-datum.
              wa_zsdt0134-hr_can = sy-uzeit.
              MODIFY zsdt0134 FROM wa_zsdt0134.
            ENDIF.

          ENDLOOP.

          CALL METHOD ctl_alv3_5820->get_frontend_layout
            IMPORTING
              es_layout = gs_layout_5820_alv3.

          gs_layout_5820_alv3-cwidth_opt = abap_true.

          CALL METHOD ctl_alv3_5820->set_frontend_layout
            EXPORTING
              is_layout = gs_layout_5820_alv3.

          PERFORM bloqueia_linhas_nlote_5820.
          CALL METHOD ctl_alv3_5820->refresh_table_display
            EXPORTING
              is_stable = _stable.

          MESSAGE TEXT-049 TYPE 'S'.

          CALL METHOD ctl_alv2_5820->refresh_table_display
            EXPORTING
              is_stable = _stable.

        ELSEIF vl_check_initial EQ abap_true.
          MESSAGE TEXT-053 TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          MESSAGE TEXT-048 TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

        CLEAR: vl_lfimg.

      ENDIF.

    ENDIF.
  ENDMETHOD.                    "USER_COMMAND

  METHOD data_changed_finished_5820.

    DATA: it_ret_5820   TYPE STANDARD TABLE OF ddshretval,
          it_f4_5820    TYPE STANDARD TABLE OF ty_f4_nlote,
          vl_cont       TYPE i,
          it_zsdt0062   TYPE STANDARD TABLE OF zsdt0062,
          it_mch1       TYPE STANDARD TABLE OF mch1,
          wa_mch1       TYPE mch1,
          wa_good_cells TYPE lvc_s_modi,
          wa_nlote_5820 TYPE ty_nlote_5820.

    DATA: wa_f4_5820 TYPE ty_f4_nlote,
          wa_ret     TYPE ddshretval,
          wa_modi    TYPE lvc_s_modi.


    IF e_modified EQ abap_true.
      READ TABLE et_good_cells INTO wa_good_cells INDEX 1.
      IF wa_good_cells-fieldname EQ 'CHARG'.

        READ TABLE it_nlote_5820 INTO wa_nlote_5820 INDEX wa_good_cells-row_id.
        IF sy-subrc IS INITIAL AND wa_nlote_5820-edit EQ abap_true.


          FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

          SELECT *
            FROM mchb
            INTO CORRESPONDING FIELDS OF TABLE it_f4_5820
            WHERE matnr EQ vg_ordem_5820-matnr
              AND werks EQ vg_ordem_5820-werks.

          IF it_f4_5820 IS NOT INITIAL.

            SELECT *
              FROM mch1
              INTO TABLE it_mch1
              FOR ALL ENTRIES IN it_f4_5820
              WHERE charg EQ it_f4_5820-charg
                AND matnr EQ it_f4_5820-matnr.

          ENDIF.

          LOOP AT it_f4_5820 INTO wa_f4_5820.
            vl_cont = vl_cont + 1.
            READ TABLE it_mch1 INTO wa_mch1 WITH KEY matnr = wa_f4_5820-matnr
                                                     charg = wa_f4_5820-charg.
            IF sy-subrc IS INITIAL.
              wa_f4_5820-vfdat = wa_mch1-vfdat.
              MODIFY it_f4_5820 FROM wa_f4_5820 INDEX vl_cont.
            ENDIF.
          ENDLOOP.

          READ TABLE it_f4_5820 WITH KEY charg = wa_good_cells-value TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.
            CLEAR wa_nlote_5820-charg.
            MODIFY it_nlote_5820 FROM wa_nlote_5820 INDEX wa_good_cells-row_id.
          ENDIF.

        ENDIF.

      ENDIF.

      CALL METHOD ctl_alv3_5820->refresh_table_display
        EXPORTING
          is_stable = _stable.

    ENDIF.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_

  METHOD on_double_click_5820.

    DATA: vl_row TYPE i.

    CLEAR: it_ordem_5820,
           l_click_ordem.

    PERFORM alv_carga_click_5820 USING e_row-index.
    CLEAR: it_nlote_5820.

    CALL METHOD ctl_alv2_5820->get_frontend_layout
      IMPORTING
        es_layout = gs_layout_5820_alv2.

    gs_layout_5820_alv2-cwidth_opt = abap_true.

    CALL METHOD ctl_alv2_5820->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_5820_alv2.

    CALL METHOD ctl_alv1_5820->refresh_table_display
      EXPORTING
        is_stable = _stable.
    CALL METHOD ctl_alv2_5820->refresh_table_display
      EXPORTING
        is_stable = _stable.
    CALL METHOD ctl_alv3_5820->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD on_double_click_ordem_5820.

    DATA: vl_row TYPE i.

    PERFORM alv_ordem_click_5820 USING e_row-index.
    PERFORM bloqueia_linhas_nlote_5820.

    CALL METHOD ctl_alv2_5820->refresh_table_display
      EXPORTING
        is_stable = _stable.

    CALL METHOD ctl_alv3_5820->get_frontend_layout
      IMPORTING
        es_layout = gs_layout_5820_alv3.

    gs_layout_5820_alv3-cwidth_opt = abap_true.

    CALL METHOD ctl_alv3_5820->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_5820_alv3.

    CALL METHOD ctl_alv3_5820->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDMETHOD.

  METHOD on_f4_5820.

    DATA: it_ret_5820 TYPE STANDARD TABLE OF ddshretval,
          it_f4_5820  TYPE STANDARD TABLE OF ty_f4_nlote_def,
          vl_cont     TYPE i,
          it_zsdt0062 TYPE STANDARD TABLE OF zsdt0062,
          it_mch1     TYPE STANDARD TABLE OF mch1,
          it_mslb     TYPE STANDARD TABLE OF mslb,
          wa_0140     TYPE zsdt0140,
          wa_0134     TYPE zsdt0134,
          wa_mch1     TYPE mch1,
          qtd_soma    TYPE brgew.

    DATA: wa_f4_5820    TYPE ty_f4_nlote_def,
          wa_nlote_5820 TYPE ty_nlote_5820,
          wa_ret        TYPE ddshretval,
          wa_modi       TYPE lvc_s_modi,
          wa_mslb       TYPE mslb.

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

    READ TABLE it_nlote_5820 INTO wa_nlote_5820 INDEX es_row_no-row_id.
    IF sy-subrc IS INITIAL AND wa_nlote_5820-edit EQ abap_true.

      SELECT *
        FROM mchb
        INTO CORRESPONDING FIELDS OF TABLE it_f4_5820
        WHERE matnr EQ vg_ordem_5820-matnr
          AND werks EQ vg_ordem_5820-werks.

      SELECT *
        FROM mslb
        INTO TABLE it_mslb
        WHERE matnr EQ vg_ordem_5820-matnr
          AND werks EQ vg_ordem_5820-werks.

      IF it_f4_5820 IS NOT INITIAL.

        SELECT *
          FROM mch1
          INTO TABLE it_mch1
          FOR ALL ENTRIES IN it_f4_5820
          WHERE charg EQ it_f4_5820-charg
            AND matnr EQ it_f4_5820-matnr.

      ENDIF.

      LOOP AT it_mslb INTO wa_mslb.
        wa_f4_5820-charg = wa_mslb-charg.
        wa_f4_5820-werks = wa_mslb-werks.
        wa_f4_5820-matnr = wa_mslb-matnr.
        wa_f4_5820-clabs = wa_mslb-lblab.
        APPEND wa_f4_5820 TO it_f4_5820.
      ENDLOOP.

      LOOP AT it_f4_5820 INTO wa_f4_5820.
        vl_cont = vl_cont + 1.
        READ TABLE it_mch1 INTO wa_mch1 WITH KEY matnr = wa_f4_5820-matnr
                                                 charg = wa_f4_5820-charg.
        IF sy-subrc IS INITIAL.
          wa_f4_5820-vfdat = wa_mch1-vfdat.
          MODIFY it_f4_5820 FROM wa_f4_5820 INDEX vl_cont.
        ENDIF.
      ENDLOOP.

    ENDIF.

    DELETE it_f4_5820 WHERE clabs IS INITIAL.


    LOOP AT it_f4_5820 INTO wa_f4_5820.

      SELECT * FROM zsdt0140
        INTO TABLE @DATA(tl_zsdt0140)
*        WHERE POSNR  EQ @WA_NLOTE_5820-POSNR
         WHERE matnr  EQ @wa_f4_5820-matnr
          AND status NE 'X'.


      IF tl_zsdt0140 IS NOT INITIAL.

        SELECT * FROM zsdt0134
        INTO TABLE @DATA(tl_zsdt0134)
        FOR ALL ENTRIES IN @tl_zsdt0140
        WHERE charg  EQ @wa_f4_5820-charg
          AND posnr  EQ @tl_zsdt0140-posnr
          AND vbeln  EQ @tl_zsdt0140-vbeln
          AND nro_cg EQ @tl_zsdt0140-nro_cgd
          AND status NE 'X'.

        IF tl_zsdt0134 IS NOT INITIAL.
          LOOP AT tl_zsdt0134 INTO wa_0134.
            IF wa_0134-ch_referencia IS INITIAL.
              qtd_soma = wa_0134-lfimg + qtd_soma.
            ELSE.
              SELECT * FROM zsdt0001
                INTO TABLE @DATA(tl_zsdt0001)
                WHERE ch_referencia EQ @wa_0134-ch_referencia.

              IF tl_zsdt0001 IS NOT INITIAL.
                DATA: wl_zsdt0001 TYPE zsdt0001.

                READ TABLE tl_zsdt0001 INTO wl_zsdt0001 INDEX 1.
                IF wl_zsdt0001-doc_rem IS INITIAL.
                  qtd_soma = wa_0134-lfimg + qtd_soma.
                  CLEAR wl_zsdt0001.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
          wa_f4_5820-qt_carga = qtd_soma.
          MODIFY it_f4_5820 FROM wa_f4_5820.
          CLEAR qtd_soma.
        ENDIF.
      ENDIF.
    ENDLOOP.



    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CHARG'
        window_title    = 'Lista de Lotes'(002)
        value_org       = 'S'
      TABLES
        value_tab       = it_f4_5820
        return_tab      = it_ret_5820
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc = 0.
      ASSIGN er_event_data->m_data->* TO <itab>.
      READ TABLE it_ret_5820 INTO wa_ret INDEX 1.
      wa_modi-row_id   = es_row_no-row_id.
      wa_modi-fieldname = 'CHARG'.
      wa_modi-value     = wa_ret-fieldval.
      APPEND wa_modi TO <itab>.
    ENDIF.

    er_event_data->m_event_handled = 'X'."(to inform grid that f4 was handled manually)

  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_5820  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5820 OUTPUT.

  CLEAR: it_ordem_5820, it_nlote_5820.

  PERFORM seleciona_carga_5820.
  PERFORM completa_carga_5820.
  PERFORM alv_tela_produtos_5820.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5820  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5820 INPUT.

  CASE sy-ucomm.
    WHEN 'LEG'.
      CALL SCREEN 6001 STARTING AT 5 5 ENDING AT 80 20.
    WHEN OTHERS.
  ENDCASE.


  CALL FUNCTION 'ZDENQUEUE_SD_CARGAD_INSUMOS'
    EXPORTING
      chave = vg_carga_5820-nro_cgd.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ALV_TELA PRODUTOS
*&---------------------------------------------------------------------*
FORM alv_tela_produtos_5820.

  IF g_custom_container_5820 IS INITIAL.

    CREATE OBJECT g_custom_container_5820
      EXPORTING
        container_name              = 'CONTAINER5820'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT dg_splitter_1_5820
      EXPORTING
        parent  = g_custom_container_5820
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1_5820->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1_5820.

    CALL METHOD dg_splitter_1_5820->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_2_5820.

    CREATE OBJECT dg_splitter_2_5820
      EXPORTING
        parent  = dg_parent_2_5820
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2_5820->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_3_5820.

    CALL METHOD dg_splitter_2_5820->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_4_5820.

    CALL METHOD dg_splitter_2_5820->set_column_mode
      EXPORTING
        mode = dg_splitter_2_5820->mode_relative.

    CALL METHOD dg_splitter_2_5820->set_column_width
      EXPORTING
        id    = 1
        width = 55. "65.  "*-CS2021000218-14.10.2022-#91701-JT-inicio

    CALL METHOD dg_splitter_2_5820->set_column_width
      EXPORTING
        id    = 2
        width = 45. "35.  "*-CS2021000218-14.10.2022-#91701-JT-inicio

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_carga_5820 USING:
           01 'ICONE'              ' '                ' '  ' '  'X'  ' '   'X'   ' '   ' '   ' '   'Status',
           02 'NRO_CGD'            'ZSDT0139'         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nr. Carga',
           03 'COD_CE'             'ZSDT0139'         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Central Emb.',
           04 'NAME1'              ' '                ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Central Embalagem',
           05 'COD_TR'             'ZSDT0139'         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Transp.',
           06 'NAME2'              ' '                ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Transportador',
           07 'COD_AR'             'ZSDT0139'         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Armazém',
           08 'NAME4'              ' '                ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Armazém',
*-CS2019001891 - JT - 04.02.2021 - inicio
           09 'TIPO_RTC2'          ' '                ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Tipo RTC',
           10 'NOME_RTC'           ' '                ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'RTC',
*-CS2019001891 - JT - 04.02.2021 - fim
           11 'DATA_ATUAL'         'ZSDT0139'         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Data Carga',
           12 'SOLIC_RECEIT'       ' '                ' '  ' '  'X'  ' '   'X'   ' '   ' '   ' '   'Solic.Receituário',
           13 'RECEITUARIO'        ' '                ' '  ' '  'X'  ' '   'X'   ' '   ' '   ' '   'Receituário',
           14 'DESC_STATUS_ICONE'  ' '                ' '  ' '  ''   ' '   ''    ' '   ' '   ' '   'Desc. Status Icone'. " User Story 153510 // MMSILVA

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_ordem_5820 USING:
          01 'NRO_CGD'              'ZSDT0139'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nr. Carga',
          02 'VBELN'                'ZSDT0134'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Ordem Venda',
          03 'POSNR'                ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Item',
*-CS2019001891 - JT - 04.02.2021 - inicio
          04 'NR_ROT'               ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Roteiro',
*-CS2019001891 - JT - 04.02.2021 - fim
          05 'MATNR'                'MARA'         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Material',
          06 'MAKTX'                'MAKTX'        ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Desc. Material',
          07 'WERKS'                ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Centro F.',
          08 'LGORT'                'ZSDT0134'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Depósito', " RJF - #124531
          08 'QTE_LIB'              'ZSDT0082'             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd.',
          09 'QTD_VINC_LT'          ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd. c/ Lote',
          10 'MEINS'                ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'U.M.'.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_nlote_5820 USING:
          01 'NRO_CG'               'ZSDT0134'     ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nr. Carga',
          02 'VBELN'                'ZSDT0134'     ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Ordem Venda',
          03 'POSNR'                'ZSDT0134'     ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Item',
          04 'CHARG'                'ZSDT0134'     ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Lote',
*-CS2021000218-14.10.2022-#91701-JT-inicio
          05 'LGORT'                'ZSDT0134'     ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Depósito',
          06 'LIFNR'                'ZSDT0134'     ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Armazém',
*-CS2021000218-14.10.2022-#91701-JT-fim
          07 'LFIMG'                'ZSDT0134'     ' '     'X'  ' '  ' '   'X'   ' '   'X'   ' '   'Quantidade'.

    gs_layout_5820_alv1-sel_mode   = 'A'.
    gs_layout_5820_alv1-stylefname = 'CELLSTYLES'.
    gs_layout_5820_alv1-cwidth_opt = 'X'.
    gs_layout_5820_alv1-info_fname = 'COR'.
    gs_layout_5820_alv1-smalltitle = 'X'.
    gs_layout_5820_alv1-grid_title = 'Cargas'.
    gs_layout_5820_alv2-sel_mode   = 'A'.
    gs_layout_5820_alv2-stylefname = 'CELLSTYLES'.
    gs_layout_5820_alv2-cwidth_opt = 'X'.
    gs_layout_5820_alv2-info_fname = 'COR'.
    gs_layout_5820_alv2-grid_title = 'Ordens da Carga'.
    gs_layout_5820_alv2-smalltitle = 'X'.
    gs_layout_5820_alv3-sel_mode   = 'A'.
    gs_layout_5820_alv3-stylefname = 'CELLSTYLES'.
    gs_layout_5820_alv3-cwidth_opt = 'X'.
    gs_layout_5820_alv3-info_fname = 'COR'.
    gs_layout_5820_alv3-smalltitle = 'X'.
    gs_layout_5820_alv3-grid_title = 'Lotes de Produto'.

*-CS2021000218-22.12.2022-#91290-JT-inicio
    PERFORM sort USING 'NRO_CGD' CHANGING it_sort_5820.
*-CS2021000218-22.12.2022-#91290-JT-fim

    PERFORM excluir_botoes_5820 CHANGING it_exclude_nlote_5820.

    CREATE OBJECT ctl_alv1_5820
      EXPORTING
        i_parent = dg_parent_1_5820.           "ALV Carga

    CREATE OBJECT ctl_alv2_5820
      EXPORTING
        i_parent = dg_parent_3_5820.           "ALV Ordens

    CREATE OBJECT ctl_alv3_5820
      EXPORTING
        i_parent = dg_parent_4_5820.           "ALV Fase

    PERFORM registrar_f4_5820.

    SET HANDLER:
      lcl_event_handler_5820=>toolbar_5820 FOR ctl_alv1_5820,
      lcl_event_handler_5820=>user_command_5820 FOR ctl_alv1_5820,
      lcl_event_handler_5820=>on_double_click_5820 FOR ctl_alv1_5820.

    CALL METHOD ctl_alv1_5820->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_5820_alv1
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_carga_5820
        it_outtab       = it_carga_5820
        it_sort         = it_sort_5820.

    SET HANDLER:
          lcl_event_handler_5820=>on_double_click_ordem_5820 FOR ctl_alv2_5820.

    CALL METHOD ctl_alv2_5820->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_5820_alv2
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_ordem_5820
        it_outtab       = it_ordem_5820.

    SET HANDLER:
      lcl_event_handler_5820=>toolbar_nlote_5820 FOR ctl_alv3_5820,
      lcl_event_handler_5820=>user_command_5820 FOR ctl_alv3_5820,
      lcl_event_handler_5820=>data_changed_finished_5820 FOR ctl_alv3_5820,
      lcl_event_handler_5820=>on_f4_5820 FOR ctl_alv3_5820.

    CALL METHOD ctl_alv3_5820->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_5820_alv3
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_nlote_5820
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_nlote_5820
        it_outtab            = it_nlote_5820.

    CALL METHOD ctl_alv3_5820->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

  ELSE.

    CALL METHOD ctl_alv1_5820->refresh_table_display
      EXPORTING
        is_stable = _stable.
    CALL METHOD ctl_alv2_5820->refresh_table_display
      EXPORTING
        is_stable = _stable.
    CALL METHOD ctl_alv3_5820->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_CARGA_5820
*&---------------------------------------------------------------------*
FORM seleciona_carga_5820 .

  SELECT *
    FROM zsdt0139
    INTO CORRESPONDING FIELDS OF TABLE it_carga_5820
    WHERE EXISTS ( SELECT *
                     FROM zsdt0140
                     INNER JOIN zsdt0082 ON zsdt0140~nro_sol EQ zsdt0082~nro_sol
                                            AND zsdt0140~seq EQ zsdt0082~seq
                     WHERE zsdt0140~nro_cgd    EQ zsdt0139~nro_cgd
                       AND zsdt0140~nro_cgd    IN d_numcg
                       AND zsdt0140~data_atual IN d_datab
                       AND zsdt0140~inco1      IN d_inco1
                       AND zsdt0139~status     NE 'X'
                       AND zsdt0082~vkorg      IN d_vkorg
                       AND zsdt0082~vkbur      IN d_vkbur
                       AND zsdt0082~spart      EQ d_spart
                       AND zsdt0082~vbeln      IN d_ovcor
                       AND zsdt0140~kunnr      IN d_kunnr )
    AND status GE 1.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COMPLETA_CARGA_5820
*&---------------------------------------------------------------------*
FORM completa_carga_5820 .

  DATA: it_lfa1       TYPE STANDARD TABLE OF lfa1,
        wa_lfa1       TYPE lfa1,
        it_zsdt0259   TYPE TABLE OF zsdt0259,
        wa_zsdt0259   TYPE zsdt0259,
        wa_carga_5820 TYPE ty_carga_5820,
        vl_cont_ra    TYPE i,
        vl_cont       TYPE i.

  SELECT DISTINCT
      zsdt0139~status
      zsdt0139~nro_cgd
      zsdt0139~cod_ce
      zsdt0139~cod_tr
      zsdt0139~cod_ar
      zsdt0139~cpf_rtc
      zsdt0139~data_atual
      zsdt0139~tipo_rtc   "*-CS2021000218-30.08.2022-#893743-JT-inicio
      FROM zsdt0139
      INNER JOIN zsdt0140 ON zsdt0140~nro_cgd = zsdt0139~nro_cgd
      INNER JOIN zsdt0082 ON zsdt0140~nro_sol = zsdt0082~nro_sol
                             AND zsdt0140~seq = zsdt0082~seq
      INNER JOIN vbkd ON zsdt0082~vbeln = vbkd~vbeln
      INNER JOIN vbpa ON vbkd~vbeln = vbpa~vbeln
        INTO CORRESPONDING FIELDS OF TABLE it_carga_5820
        WHERE zsdt0139~nro_cgd    IN d_numcg
          AND zsdt0139~data_atual IN d_datab
          AND zsdt0139~status     NE 'X'
          AND zsdt0082~vkorg      IN d_vkorg
          AND zsdt0082~spart      EQ d_spart
          AND zsdt0082~vkbur      IN d_vkbur
          AND vbkd~inco1          IN d_inco1
          AND vbpa~kunnr          IN d_kunnr
          AND zsdt0139~status     GE 1.

  IF it_carga_5820 IS NOT INITIAL.

    SELECT *
      FROM lfa1
      INTO TABLE it_lfa1
      FOR ALL ENTRIES IN it_carga_5820
      WHERE lifnr EQ it_carga_5820-cod_ce.

    SELECT *
      FROM lfa1
      APPENDING TABLE it_lfa1
      FOR ALL ENTRIES IN it_carga_5820
      WHERE lifnr EQ it_carga_5820-cod_tr.

    SELECT *
      FROM lfa1
      APPENDING TABLE it_lfa1
      FOR ALL ENTRIES IN it_carga_5820
      WHERE lifnr EQ it_carga_5820-cod_ar.

*-CS2019001891 - JT - 04.02.2021 - inicio
    SELECT *
      FROM zsdt0259
      INTO TABLE it_zsdt0259
       FOR ALL ENTRIES IN it_carga_5820
      WHERE cpf  EQ it_carga_5820-cpf_rtc.

    SORT it_zsdt0259 BY cpf.
*-CS2019001891 - JT - 04.02.2021 - fim


*-CS2021000218-27.10.2022-#91184-JT-inicio
    SELECT *
      FROM zsdt0302
      INTO TABLE it_zsdt0302
       FOR ALL ENTRIES IN it_carga_5820
     WHERE nro_cgd  EQ it_carga_5820-nro_cgd.

    SELECT *
      FROM zsdt0298
      INTO TABLE it_zsdt0298
       FOR ALL ENTRIES IN it_carga_5820
     WHERE nro_cgd   EQ it_carga_5820-nro_cgd
       AND cancelado EQ abap_off.

    SORT it_zsdt0302 BY nro_cgd ch_referencia DESCENDING.
    DELETE ADJACENT DUPLICATES FROM it_zsdt0302 COMPARING nro_cgd.

*   LOOP AT it_zsdt0302 INTO wa_zsdt0302.
*     DATA(l_tabix_302) = sy-tabix.
*     READ TABLE it_zsdt0298 INTO wa_zsdt0298 WITH KEY nro_cgd       = wa_zsdt0302-nro_cgd
*                                                      ch_referencia = wa_zsdt0302-ch_referencia.
*     IF sy-subrc <> 0.
*       DELETE it_zsdt0302 INDEX l_tabix_302.
*     ENDIF.
*   ENDLOOP.
*-CS2021000218-27.10.2022-#91184-JT-inicio

  ENDIF.


  LOOP AT it_carga_5820 INTO wa_carga_5820.

    vl_cont = vl_cont + 1.

    IF wa_carga_5820-status EQ 0.
      wa_carga_5820-icone = '@5C@'.
      wa_carga_5820-desc_status_icone = 'Lote não Aprovado'. " User Story 153510 // MMSILVA
    ELSEIF wa_carga_5820-status EQ 1.
      wa_carga_5820-icone = '@5B@'.
      wa_carga_5820-desc_status_icone = 'Lote Aprovado'. " User Story 153510 // MMSILVA
    ELSEIF wa_carga_5820-status EQ 2.
      wa_carga_5820-icone = '@0Q@'.
      wa_carga_5820-desc_status_icone = 'Romaneio Gerado'. " User Story 153510 // MMSILVA
    ENDIF.

    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_carga_5820-cod_ce.
    IF sy-subrc IS INITIAL.
      wa_carga_5820-name1 = wa_lfa1-name1.
    ENDIF.

    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_carga_5820-cod_tr.
    IF sy-subrc IS INITIAL.
      wa_carga_5820-name2 = wa_lfa1-name1.
    ENDIF.

    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_carga_5820-cod_ar.
    IF sy-subrc IS INITIAL.
      wa_carga_5820-name4 = wa_lfa1-name1.
    ENDIF.

*-CS2019001891 - JT - 04.02.2021 - inicio
    READ TABLE it_zsdt0259 INTO wa_zsdt0259 WITH KEY cpf = wa_carga_5820-cpf_rtc
                                            BINARY SEARCH.
    IF sy-subrc = 0.
      wa_carga_5820-nome_rtc = wa_zsdt0259-nome.
    ENDIF.
*-CS2019001891 - JT - 04.02.2021 - fim

*-CS2021000218-31.08.2022-#89492-JT-inicio
    IF wa_carga_5820-tipo_rtc = 'T'.
      wa_carga_5820-tipo_rtc2 = 'Terceiro'.
    ELSE.
      wa_carga_5820-tipo_rtc2 = 'Proprio'.
    ENDIF.
*-CS2021000218-31.08.2022-#89492-JT-fim

*-CS2021000218-27.10.2022-#91184-JT-inicio
    PERFORM ajusta_status_receita    USING '1'
                                  CHANGING wa_carga_5820.
*-CS2021000218-27.10.2022-#91184-JT-inicio

    "US #154513 - MMSILVA - 28.02.2025 - Inicio
    READ TABLE it_zsdt0302 INTO wa_zsdt0302 WITH KEY nro_cgd = wa_carga_5820-nro_cgd.

    SELECT SINGLE nro_nf_prod FROM zsdt0001 WHERE ch_referencia EQ @wa_zsdt0302-ch_referencia AND nro_cg EQ @wa_zsdt0302-nro_cgd INTO @DATA(wa_zsdt0001).

    IF sy-subrc IS INITIAL.
      SELECT SINGLE docsta FROM j_1bnfe_active WHERE docnum EQ @wa_zsdt0001 INTO @DATA(wa_j_1bnfe_active).
      IF sy-subrc IS INITIAL.
        IF ( wa_j_1bnfe_active EQ '1').

          wa_carga_5820-icone = '@5Y@'.
          wa_carga_5820-desc_status_icone = 'Nota Fiscal Autorizada'.
        ENDIF.
      ENDIF.
    ENDIF.
    "US #154513 - MMSILVA - 28.02.2025 - Fim

    MODIFY it_carga_5820 FROM wa_carga_5820 INDEX vl_cont.

  ENDLOOP.

  SORT it_carga_5820 BY nro_cgd.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_SOL_CLICK_5820
*&---------------------------------------------------------------------*
FORM alv_carga_click_5820  USING    p_e_row_index.

  DATA: it_ordem_aux_5820 TYPE STANDARD TABLE OF ty_ordem_5820,
        it_makt           TYPE STANDARD TABLE OF makt,
        wa_makt           TYPE makt,
        wa_carga_5820     TYPE ty_carga_5820,
        wa_ordem_5820     TYPE ty_ordem_5820,
        wa_ordem_aux_5820 TYPE ty_ordem_5820,
        vl_cont           TYPE i,
        vl_qtd_vinc       TYPE zsdt0131-qtd_vinc,
        it_zsdt0129       TYPE STANDARD TABLE OF zsdt0129,
        wa_zsdt0129       TYPE zsdt0129,
        it_zsdt0134       TYPE STANDARD TABLE OF zsdt0134,
        wa_zsdt0134       TYPE zsdt0134.

  LOOP AT it_carga_5820 INTO wa_carga_5820 WHERE nro_cgd EQ vg_carga_5820-nro_cgd.

    vl_cont = sy-tabix.

    CALL FUNCTION 'ZDENQUEUE_SD_CARGAD_INSUMOS'
      EXPORTING
        chave = wa_carga_5820-nro_cgd.

    CLEAR: wa_carga_5820-cor.
    MODIFY it_carga_5820 FROM wa_carga_5820 INDEX vl_cont.

  ENDLOOP.

  CLEAR: vg_carga_5820, vl_cont.

  READ TABLE it_carga_5820 INTO wa_carga_5820 INDEX p_e_row_index.
  IF sy-subrc IS INITIAL.
    wa_carga_5820-cor = 'C300'.
    vg_carga_5820 = wa_carga_5820.

    CALL FUNCTION 'ZENQUEUE_SD_CARGAD_INSUMOS'
      EXPORTING
        chave          = wa_carga_5820-nro_cgd
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    ELSE.
      MODIFY it_carga_5820 FROM wa_carga_5820 INDEX p_e_row_index.

      SELECT *
            FROM zsdt0140
            INNER JOIN zsdt0082 ON zsdt0140~nro_sol = zsdt0082~nro_sol AND
                                   zsdt0140~seq   = zsdt0082~seq
            INTO CORRESPONDING FIELDS OF TABLE it_ordem_5820
            WHERE zsdt0140~status  NE 'X'
              AND zsdt0140~nro_cgd EQ wa_carga_5820-nro_cgd.

      IF it_ordem_5820 IS NOT INITIAL.

        SELECT *
          FROM zsdt0134
          INTO TABLE it_zsdt0134
          FOR ALL ENTRIES IN it_ordem_5820
          WHERE status NE 'X'
            AND vbeln  EQ it_ordem_5820-vbeln
            AND posnr  EQ it_ordem_5820-posnr
            AND nro_cg EQ wa_carga_5820-nro_cgd.

        SELECT *
          FROM makt
          INTO TABLE it_makt
          FOR ALL ENTRIES IN it_ordem_5820
          WHERE matnr EQ it_ordem_5820-matnr.

        SELECT vbeln, posnr, lgort
        FROM vbap
        INTO TABLE @DATA(it_vbap)
          FOR ALL ENTRIES IN @it_ordem_5820
        WHERE vbeln EQ @it_ordem_5820-vbeln
          AND posnr EQ @it_ordem_5820-posnr.

      ENDIF.

      it_ordem_aux_5820 = it_ordem_5820.

*-CS2019001891 - JT - 04.02.2021 - inicio
*     SORT it_ordem_5820 BY vbeln posnr ASCENDING.
*     DELETE ADJACENT DUPLICATES FROM it_ordem_5820 COMPARING vbeln posnr.
      SORT it_ordem_5820 BY vbeln posnr nr_rot ASCENDING.
      DELETE ADJACENT DUPLICATES FROM it_ordem_5820 COMPARING vbeln posnr nr_rot.
*-CS2019001891 - JT - 04.02.2021 - inicio

      LOOP AT it_ordem_5820 INTO wa_ordem_5820.

        vl_cont = vl_cont + 1.

        READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_ordem_5820-matnr.
        IF sy-subrc IS INITIAL.
          wa_ordem_5820-maktx = wa_makt-maktx.
        ENDIF.

        LOOP AT it_ordem_aux_5820 INTO wa_ordem_aux_5820 WHERE vbeln EQ wa_ordem_5820-vbeln
                                                           AND posnr EQ wa_ordem_5820-posnr.
          vl_qtd_vinc = vl_qtd_vinc + wa_ordem_aux_5820-qte_lib.
        ENDLOOP.

        LOOP AT it_zsdt0134 INTO wa_zsdt0134 WHERE vbeln EQ  wa_ordem_5820-vbeln
                                               AND posnr EQ  wa_ordem_5820-posnr
                                               AND nro_cg EQ wa_ordem_5820-nro_cgd.

          wa_ordem_5820-lgort = wa_zsdt0134-lgort. " RJF - #124531

          wa_ordem_5820-qtd_vinc_lt = wa_ordem_5820-qtd_vinc_lt + wa_zsdt0134-lfimg.
        ENDLOOP.

        LOOP AT it_vbap INTO DATA(wa_vbap) WHERE vbeln EQ  wa_ordem_5820-vbeln
                                               AND posnr EQ  wa_ordem_5820-posnr.

          wa_ordem_5820-lgort = wa_vbap-lgort. " AOENNING - #124531
          CLEAR: wa_vbap.
        ENDLOOP.

        wa_ordem_5820-qte_lib = vl_qtd_vinc.

        MODIFY it_ordem_5820 FROM wa_ordem_5820 INDEX vl_cont.
        CLEAR: vl_qtd_vinc.

      ENDLOOP.
      FREE: it_vbap.

    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_ORDEM_CLICK_5820
*&---------------------------------------------------------------------*
FORM alv_ordem_click_5820 USING p_e_row_index.

  DATA: wa_ordem_5820 TYPE ty_ordem_5820,
        wa_nlote_5820 TYPE ty_nlote_5820,
        l_lock        TYPE char1.

  FREE: l_lock.

  LOOP AT it_ordem_5820 INTO wa_ordem_5820.
    CLEAR: wa_ordem_5820-cor,
           wa_ordem_5820-lock.  "*-CS2021000218-14.10.2022-#91701-JT-inicio
    MODIFY it_ordem_5820 FROM wa_ordem_5820 INDEX sy-tabix.
  ENDLOOP.

  READ TABLE it_ordem_5820 INTO wa_ordem_5820 INDEX p_e_row_index.
  IF sy-subrc IS INITIAL.

*-CS2021000218-14.10.2022-#91701-JT-inicio
    CALL FUNCTION 'ZDENQUEUE_SD_MATERIAL_INSUMOS'
      EXPORTING
        chave = vg_ordem_5820-matnr.

    CALL FUNCTION 'ZENQUEUE_SD_MATERIAL_INSUMOS'
      EXPORTING
        chave          = wa_ordem_5820-matnr
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      l_lock = abap_true.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    ENDIF.
*-CS2021000218-14.10.2022-#91701-JT-fim

    l_click_ordem = abap_true.

    SELECT *
      FROM zsdt0134
      INTO CORRESPONDING FIELDS OF TABLE it_nlote_5820
      WHERE nro_cg EQ wa_ordem_5820-nro_cgd
        AND vbeln  EQ wa_ordem_5820-vbeln
        AND posnr  EQ wa_ordem_5820-posnr
*-CS2019001891 - JT - 04.02.2021 - inicio
        AND nr_rot EQ wa_ordem_5820-nr_rot
*-CS2019001891 - JT - 04.02.2021 - fim
        AND status NE 'X'.

*-CS2019001891 - JT - 04.02.2021 - inicio
    IF sy-subrc <> 0.
      SELECT *
        FROM zsdt0134
        INTO CORRESPONDING FIELDS OF TABLE it_nlote_5820
        WHERE nro_cg EQ wa_ordem_5820-nro_cgd
          AND vbeln  EQ wa_ordem_5820-vbeln
          AND posnr  EQ wa_ordem_5820-posnr
          AND nr_rot EQ ''
          AND status NE 'X'.

      LOOP AT it_nlote_5820  INTO wa_nlote_5820.

        IF wa_nlote_5820-lgort IS NOT INITIAL. " RJF - #124531
          l_lgort = wa_nlote_5820-lgort.       " RJF - #124531
        ENDIF.                                 " RJF - #124531

        wa_nlote_5820-nr_rot    = wa_ordem_5820-nr_rot.
        MODIFY it_nlote_5820 FROM wa_nlote_5820 INDEX sy-tabix.
      ENDLOOP.
    ENDIF.
*-CS2019001891 - JT - 04.02.2021 - fim

    CLEAR: vg_ordem_5820.

    vg_ordem_5820 = wa_ordem_5820.
    wa_ordem_5820-cor  = 'C300'.
    IF wa_ordem_5820-lgort IS INITIAL. " RJF - #124531
      wa_ordem_5820-lgort = l_lgort. " RJF - #124531
    ENDIF. " RJF - #124531
    wa_ordem_5820-lock = l_lock.  "*-CS2021000218-14.10.2022-#91701-JT-inicio
    MODIFY it_ordem_5820 FROM wa_ordem_5820 INDEX p_e_row_index.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BLOQUEIA_LINHAS_nlote_5820
*&---------------------------------------------------------------------*
FORM bloqueia_linhas_nlote_5820 .
  DATA: vl_cont       TYPE i,
        it_celltab    TYPE lvc_t_styl,
        wa_nlote_5820 TYPE ty_nlote_5820.

  LOOP AT it_nlote_5820 INTO wa_nlote_5820.
    vl_cont = vl_cont + 1.
    CLEAR: it_celltab, wa_nlote_5820-cellstyles.
    PERFORM fill_celltab_5820 USING wa_nlote_5820-edit
                         CHANGING it_celltab.
    CLEAR wa_nlote_5820-cellstyles.
    INSERT LINES OF it_celltab INTO TABLE wa_nlote_5820-cellstyles.
    MODIFY it_nlote_5820 FROM wa_nlote_5820 INDEX vl_cont.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_CELLTAB_5820
*&---------------------------------------------------------------------*
FORM fill_celltab_5820  USING    p_edit
                        CHANGING p_it_celltab TYPE lvc_t_styl.

  DATA: wa_celltab TYPE lvc_s_styl,
        status     TYPE raw4.

  IF p_edit EQ abap_true.
    status = cl_gui_alv_grid=>mc_style_disabled.  "mc_style_enabled. "*-CS2021000218-14.10.2022-#91701-JT-inicio
  ELSE.
    status = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  wa_celltab-fieldname = 'CHARG'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'LFIMG'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.

ENDFORM.                               " FILL_CELLTAB

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_BOTOES_5820
*&---------------------------------------------------------------------*
FORM excluir_botoes_5820 CHANGING p_it_exclude TYPE ui_functions.

  DATA: ls_exclude     TYPE ui_func.

  ls_exclude = cl_gui_alv_grid=>mc_fc_excl_all.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  REGISTRAR_F4_5130
*&---------------------------------------------------------------------*
FORM registrar_f4_5820 .

  DATA: gs_f4      TYPE lvc_s_f4,
        gt_f4_5820 TYPE lvc_t_f4.

  gs_f4-fieldname  = 'CHARG'.
  gs_f4-register   = 'X'.
  gs_f4-getbefore  = space.
  gs_f4-chngeafter = space.
  APPEND gs_f4 TO gt_f4_5820.

  CALL METHOD ctl_alv3_5820->register_f4_for_fields
    EXPORTING
      it_f4 = gt_f4_5820.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SALVA_ROMANEIOS
*&---------------------------------------------------------------------*
FORM salva_romaneios_5820 CHANGING vl_check_rom.

  DATA: it_zsdt0040             TYPE STANDARD TABLE OF zsdt0040,
        it_zsdt0041             TYPE STANDARD TABLE OF zsdt0041,
        it_zsdt0090             TYPE STANDARD TABLE OF zsdt0090,
        it_zsdt0136             TYPE STANDARD TABLE OF zsdt0136,
        it_zsdt0001             TYPE STANDARD TABLE OF zsdt0001,
        it_zsdt0001_gravou      TYPE STANDARD TABLE OF zsdt0001,
        it_zsdt0134             TYPE STANDARD TABLE OF zsdt0134,
        it_zsdt0139             TYPE STANDARD TABLE OF zsdt0139,
        it_zsdt0134_aux         TYPE STANDARD TABLE OF zsdt0134,
        it_ordem_check_aux_5820 TYPE STANDARD TABLE OF ty_ordem_5820,
        it_vbpa                 TYPE STANDARD TABLE OF vbpa,
        it_vbap                 TYPE STANDARD TABLE OF vbap,
        it_mara                 TYPE STANDARD TABLE OF mara,
        wa_ordem_check_aux_5820 TYPE ty_ordem_5820,
        wa_ordem_5820           TYPE ty_ordem_5820,
        wa_zsdt0041             TYPE zsdt0041,
        wa_zsdt0040             TYPE zsdt0040,
        wa_zsdt0090             TYPE zsdt0090,
        wa_zsdt0001             TYPE zsdt0001,
        wa_zsdt0001_item        TYPE zsdt0001_item,
        wa_zsdt0136             TYPE zsdt0136,
        wa_zsdt0139             TYPE zsdt0139,
        wa_zsdt0134             TYPE zsdt0134,
        wa_carga_5820           TYPE ty_carga_5820,
        wa_vbpa                 TYPE vbpa,
        wa_vbap                 TYPE vbap,
        wa_mara                 TYPE mara,
        chave                   TYPE char8,
        vl_doc_simulacao        TYPE zsdt0041-doc_simulacao,
        vl_safra                TYPE zsdt0040-safra,
        vl_gravou               TYPE char1,
        vl_qtd_kg               TYPE zsdt0131-qtd_emkg.


  SELECT *
    FROM zsdt0140
    INNER JOIN zsdt0082 ON zsdt0140~nro_sol = zsdt0082~nro_sol AND
                           zsdt0140~seq   = zsdt0082~seq
    INTO CORRESPONDING FIELDS OF TABLE it_ordem_check_5820
    FOR ALL ENTRIES IN it_carga_aux_5820
    WHERE zsdt0140~status  NE 'X'
      AND zsdt0140~nro_cgd EQ it_carga_aux_5820-nro_cgd.

  IF it_ordem_check_5820 IS NOT INITIAL.

    it_ordem_check_aux_5820 = it_ordem_check_5820.

*-CS2019001891 - JT - 04.02.2021 - inicio
*   SORT it_ordem_check_5820 BY nro_cgd vbeln ASCENDING.
*   DELETE ADJACENT DUPLICATES FROM it_ordem_check_5820 COMPARING nro_cgd vbeln.
    SORT it_ordem_check_5820 BY nro_cgd vbeln nr_rot ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_ordem_check_5820 COMPARING nro_cgd vbeln nr_rot.
*-CS2019001891 - JT - 04.02.2021 - fim

    SELECT *
      FROM zsdt0041
      INTO TABLE it_zsdt0041
      FOR ALL ENTRIES IN it_ordem_check_5820
      WHERE vbeln EQ it_ordem_check_5820-vbeln.

    SELECT *
      FROM zsdt0090
      INTO TABLE it_zsdt0090
      FOR ALL ENTRIES IN it_ordem_check_5820
      WHERE vbeln EQ it_ordem_check_5820-vbeln.

    IF it_zsdt0041 IS NOT INITIAL.

      SELECT *
        FROM zsdt0040
        INTO TABLE it_zsdt0040
        FOR ALL ENTRIES IN it_zsdt0041
        WHERE doc_simulacao EQ it_zsdt0041-doc_simulacao.

    ENDIF.

    IF it_zsdt0090 IS NOT INITIAL.

      SELECT *
        FROM zsdt0040
        APPENDING TABLE it_zsdt0040
        FOR ALL ENTRIES IN it_zsdt0090
        WHERE doc_simulacao EQ it_zsdt0090-doc_simulacao.

    ENDIF.

    SELECT *
      FROM zsdt0136
      INTO TABLE it_zsdt0136
      FOR ALL ENTRIES IN it_ordem_check_5820
      WHERE werks EQ it_ordem_check_5820-werks.

  ENDIF.

  CLEAR: vl_cont.

  LOOP AT it_ordem_check_5820 INTO wa_ordem_5820.

    vl_cont = vl_cont + 1.
    CLEAR: vl_qtd_kg.

    READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_ordem_5820-vbeln.
    IF sy-subrc IS NOT INITIAL.
      READ TABLE it_zsdt0090 INTO wa_zsdt0090 WITH KEY vbeln = wa_ordem_5820-vbeln.
      IF sy-subrc IS INITIAL.
        vl_doc_simulacao = wa_zsdt0090-doc_simulacao.
      ENDIF.
    ELSE.
      vl_doc_simulacao = wa_zsdt0041-doc_simulacao.
    ENDIF.

    IF vl_doc_simulacao IS NOT INITIAL.
      READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = vl_doc_simulacao.
      IF sy-subrc IS INITIAL.
        vl_safra = wa_zsdt0040-safra.
      ENDIF.
    ENDIF.

    READ TABLE it_zsdt0136 WITH KEY werks = wa_ordem_5820-werks
                                    safra = vl_safra TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE s000(z_fi) WITH TEXT-125 wa_ordem_5820-werks TEXT-126 vl_safra DISPLAY LIKE 'E'.
*      MESSAGE TEXT-055 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check_rom = abap_true.
      EXIT.
    ENDIF.

    LOOP AT it_ordem_check_aux_5820 INTO wa_ordem_check_aux_5820 WHERE nro_cgd EQ wa_ordem_5820-nro_cgd
                                                                   AND vbeln   EQ wa_ordem_5820-vbeln
*------------------------------------------------------------------CS2019001891 - JT - 04.02.2021 - inicio
                                                                   AND nr_rot  EQ wa_ordem_5820-nr_rot.
*------------------------------------------------------------------CS2019001891 - JT - 04.02.2021 - fim
      vl_qtd_kg = wa_ordem_check_aux_5820-qte_lib + vl_qtd_kg.
    ENDLOOP.
    CLEAR wa_ordem_5820-qtd_emkg.
    wa_ordem_5820-qtd_emkg = vl_qtd_kg.
    MODIFY it_ordem_check_5820 FROM wa_ordem_5820 INDEX vl_cont TRANSPORTING qtd_emkg.

  ENDLOOP.

  IF vl_check_rom IS INITIAL.

    CREATE OBJECT zcl_romaneio.

    SELECT *
       FROM vbpa
       INTO TABLE it_vbpa
       FOR ALL ENTRIES IN it_ordem_check_5820
       WHERE vbeln EQ it_ordem_check_5820-vbeln
         AND ( parvw EQ 'PC' OR
               parvw EQ 'AG' ).

    SELECT *
      FROM zsdt0139
      INTO TABLE it_zsdt0139
      FOR ALL ENTRIES IN it_ordem_check_5820
      WHERE nro_cgd EQ it_ordem_check_5820-nro_cgd
        AND status  NE 'X'.

    LOOP AT it_ordem_check_5820 INTO wa_ordem_5820.

      CLEAR: wa_zsdt0001, vl_gravou, it_zsdt0134_aux, it_vbap, it_mara.

      wa_zsdt0001-tp_movimento = 'S'.
      wa_zsdt0001-vbeln        = wa_ordem_5820-vbeln.
      wa_zsdt0001-nr_rot       = wa_ordem_5820-nr_rot.
      wa_zsdt0001-dt_movimento = sy-datum.
      wa_zsdt0001-peso_liq     = wa_ordem_5820-qtd_emkg.
      wa_zsdt0001-peso_fiscal  = wa_ordem_5820-qtd_emkg.
      wa_zsdt0001-peso_subtotal = wa_ordem_5820-qtd_emkg. "IR096917 - 01.06.2012 - RMNI

      READ TABLE it_zsdt0139 INTO wa_zsdt0139 WITH KEY nro_cgd = wa_ordem_5820-nro_cgd.
      IF sy-subrc IS INITIAL.
        wa_zsdt0001-motorista  = wa_zsdt0139-cod_mt.
        wa_zsdt0001-placa_cav  = wa_zsdt0139-placa_cav.
        wa_zsdt0001-placa_car1 = wa_zsdt0139-placa_car1.
        wa_zsdt0001-placa_car2 = wa_zsdt0139-placa_car2.
*-CS2019001891 - JT - 04.02.2021 - inicio
        wa_zsdt0001-placa_car3 = wa_zsdt0139-placa_car3.
*-CS2019001891 - JT - 04.02.2021 - fim
        wa_zsdt0001-agente_frete = wa_zsdt0139-cod_tr.
      ENDIF.

      CLEAR: wa_zsdt0139.

      READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_ordem_5820-vbeln.
      IF sy-subrc IS NOT INITIAL.
        READ TABLE it_zsdt0090 INTO wa_zsdt0090 WITH KEY vbeln = wa_ordem_5820-vbeln.
        IF sy-subrc IS INITIAL.
          vl_doc_simulacao = wa_zsdt0090-doc_simulacao.
        ENDIF.
      ELSE.
        vl_doc_simulacao = wa_zsdt0041-doc_simulacao.
      ENDIF.

      IF vl_doc_simulacao IS NOT INITIAL.
        READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = vl_doc_simulacao.
        IF sy-subrc IS INITIAL.
          vl_safra = wa_zsdt0040-safra.
        ENDIF.
      ENDIF.

      wa_zsdt0001-nr_safra    = vl_safra.
      wa_zsdt0001-bukrs       = wa_ordem_5820-vkorg.
      wa_zsdt0001-branch      = wa_ordem_5820-werks.

      READ TABLE it_vbpa INTO wa_vbpa WITH KEY vbeln = wa_ordem_5820-vbeln
                                               parvw = 'PC'.
      IF sy-subrc IS INITIAL.
        wa_zsdt0001-parid = wa_vbpa-lifnr.
      ENDIF.

      READ TABLE it_vbpa INTO wa_vbpa WITH KEY vbeln = wa_ordem_5820-vbeln
                                               parvw = 'AG'.
      IF sy-subrc IS INITIAL.
        wa_zsdt0001-id_cli_dest = wa_vbpa-kunnr.
      ENDIF.

      IF wa_ordem_5820-inco1 EQ 'FOB'.
        wa_zsdt0001-tp_frete = 'F'.
      ELSE.
        wa_zsdt0001-tp_frete = 'C'.
      ENDIF.

      wa_zsdt0001-id_interface = '52'.
      wa_zsdt0001-nro_cg = wa_ordem_5820-nro_cgd.

      zcl_romaneio->zif_cadastro~novo_registro( ).
      zcl_romaneio->set_cabecalho_romaneio( i_romaneio = wa_zsdt0001 ).

      SELECT *
        FROM zsdt0134
        INTO TABLE it_zsdt0134_aux
        WHERE nro_cg EQ wa_ordem_5820-nro_cgd
          AND vbeln  EQ wa_ordem_5820-vbeln
          AND nr_rot EQ wa_ordem_5820-nr_rot
          AND status NE 'X'.

      SELECT *
      FROM vbap
      INTO TABLE it_vbap
      FOR ALL ENTRIES IN it_zsdt0134_aux
      WHERE vbeln EQ it_zsdt0134_aux-vbeln
        AND posnr EQ it_zsdt0134_aux-posnr.

      SELECT *
        FROM mara
        INTO TABLE it_mara
        FOR ALL ENTRIES IN it_vbap
        WHERE matnr EQ it_vbap-matnr.

      LOOP AT it_zsdt0134_aux INTO wa_zsdt0134.

        CLEAR: wa_zsdt0001_item.
        wa_zsdt0001_item-vbeln = wa_zsdt0134-vbeln.
        wa_zsdt0001_item-posnr = wa_zsdt0134-posnr.
        wa_zsdt0001_item-nr_rot = wa_zsdt0134-nr_rot.
        wa_zsdt0001_item-charg = wa_zsdt0134-charg.
        wa_zsdt0001_item-lfimg = wa_zsdt0134-lfimg.

        READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_zsdt0134-vbeln
                                                 posnr = wa_zsdt0134-posnr.
        IF sy-subrc IS INITIAL.
          wa_zsdt0001_item-matnr = wa_vbap-matnr.
          wa_zsdt0001_item-meins = wa_vbap-meins.

          READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_vbap-matnr.
          IF sy-subrc IS INITIAL.
            wa_zsdt0001_item-brgew = wa_zsdt0001_item-lfimg * wa_mara-brgew.
            wa_zsdt0001_item-ntgew = wa_zsdt0001_item-lfimg * wa_mara-brgew.
          ENDIF.

        ENDIF.

        zcl_romaneio->add_item( CHANGING i_item = wa_zsdt0001_item ).

      ENDLOOP.

      TRY.
          zcl_romaneio->zif_cadastro~gravar_registro( RECEIVING i_gravou = vl_gravou ).
        CATCH zcx_cadastro INTO zcx_cadastro.
          zcx_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

      IF vl_gravou IS INITIAL.
        EXIT.
      ELSE.
        CLEAR: wa_zsdt0001.
        zcl_romaneio->get_registro( IMPORTING e_registro = wa_zsdt0001 ).
        APPEND wa_zsdt0001 TO it_zsdt0001_gravou.
      ENDIF.

    ENDLOOP.

  ENDIF.

  IF vl_gravou IS NOT INITIAL.

    CLEAR: it_zsdt0134_aux.

    SELECT *
            FROM zsdt0134
            INTO TABLE it_zsdt0134_aux
            FOR ALL ENTRIES IN it_zsdt0001_gravou
            WHERE nro_cg EQ it_zsdt0001_gravou-nro_cg
              AND vbeln  EQ it_zsdt0001_gravou-vbeln
              AND nr_rot EQ it_zsdt0001_gravou-nr_rot
              AND status NE 'X'.

    LOOP AT it_zsdt0134_aux INTO wa_zsdt0134.
      READ TABLE it_zsdt0001_gravou INTO wa_zsdt0001 WITH KEY nro_cg = wa_zsdt0134-nro_cg
                                                              vbeln  = wa_zsdt0134-vbeln
                                                              nr_rot = wa_zsdt0134-nr_rot.
      IF sy-subrc IS INITIAL.
        wa_zsdt0134-ch_referencia = wa_zsdt0001-ch_referencia.
      ENDIF.
      MODIFY zsdt0134 FROM wa_zsdt0134.
    ENDLOOP.

*-CS2021000218-01.11.2022-#90519-JT-inicio
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 50
        text       = 'Gerando Receituário Agronômico...'.

*----------------------------------
*-- gerar solicitacao receita
*----------------------------------
    FREE: l_erro.

    LOOP AT it_zsdt0001_gravou INTO wa_zsdt0001.
      TRY.
          zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
             )->set_gerar_sol_ra( EXPORTING i_nro_cgd       = wa_zsdt0001-nro_cg
                                            i_ch_referencia = wa_zsdt0001-ch_referencia
                                            i_exibe_popup   = abap_true ).

        CATCH zcx_integracao INTO DATA(ex_integra).
          l_erro = abap_true.
          ex_integra->zif_error~published_erro( i_msgty = 'S' ).
        CATCH zcx_error INTO DATA(ex_error).
          l_erro = abap_true.
          ex_error->zif_error~published_erro(   i_msgty = 'S' ).
      ENDTRY.

      READ TABLE it_carga_aux_5820 INTO wa_carga_5820 WITH KEY nro_cgd = wa_zsdt0001-nro_cg.
      IF sy-subrc IS INITIAL.
        l_tabix2 = sy-tabix.
        PERFORM ajusta_status_receita    USING '2'
                                      CHANGING wa_carga_5820.
        MODIFY it_carga_aux_5820 FROM wa_carga_5820 INDEX l_tabix2.
      ENDIF.
    ENDLOOP.

    IF l_erro = abap_false.
      MESSAGE s024(sd) WITH 'Receituário Agronômico gerado com Sucesso!'.
*   ELSE.
*     MESSAGE s024(sd) WITH 'Não foi possível gerar ' 'Receituário Agronômico!' DISPLAY LIKE 'E'.
    ENDIF.

    CALL METHOD ctl_alv1_5820->refresh_table_display
      EXPORTING
        is_stable = _stable.
*-CS2021000218-01.11.2022-#90519-JT-fim

  ELSE.

    LOOP AT it_zsdt0001_gravou INTO wa_zsdt0001.
      TRY.
          zcl_romaneio->set_registro( EXPORTING i_id_registro = wa_zsdt0001-ch_referencia ).
        CATCH zcx_cadastro INTO zcx_cadastro.
          zcx_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.
      zcl_romaneio->excluir_registro( ).
    ENDLOOP.

    MOVE abap_true TO vl_check_rom.

  ENDIF.

  IF vl_check_rom IS INITIAL.

    LOOP AT it_carga_aux_5820 INTO wa_carga_5820.

      SELECT SINGLE *
        FROM zsdt0139
        INTO wa_zsdt0139
        WHERE nro_cgd EQ wa_carga_5820-nro_cgd.

      wa_zsdt0139-status = '2'.
      MODIFY zsdt0139 FROM wa_zsdt0139.
    ENDLOOP.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_SOL_CLICK_5820
*&---------------------------------------------------------------------*
FORM check_ordens_rom_5820  USING    p_e_row_index
                            CHANGING vl_check_rom.

  DATA: it_ordem_check_aux_5820 TYPE STANDARD TABLE OF ty_ordem_5820,
        it_ordem_aux_5820       TYPE STANDARD TABLE OF ty_ordem_5820,
        wa_ordem_aux_5820       TYPE ty_ordem_5820,
        wa_carga_5820           TYPE ty_carga_5820,
        wa_ordem_5820           TYPE ty_ordem_5820,
        wa_ordem_check_aux_5820 TYPE ty_ordem_5820,
        vl_cont                 TYPE i,
        vl_qtd_vinc             TYPE zsdt0131-qtd_vinc,
        it_zsdt0134             TYPE STANDARD TABLE OF zsdt0134,
        wa_zsdt0134             TYPE zsdt0134.

  SELECT *
    FROM zsdt0140
    INNER JOIN zsdt0082 ON zsdt0140~seq     = zsdt0082~seq   AND
                           zsdt0140~nro_sol = zsdt0082~nro_sol
    INTO CORRESPONDING FIELDS OF TABLE it_ordem_check_5820
    FOR ALL ENTRIES IN it_carga_aux_5820
    WHERE zsdt0140~status  NE 'X'
      AND zsdt0140~nro_cgd EQ it_carga_aux_5820-nro_cgd.

  SELECT *
    FROM zsdt0134
    INTO TABLE it_zsdt0134
    FOR ALL ENTRIES IN it_ordem_check_5820
    WHERE status NE 'X'
      AND nro_cg EQ it_ordem_check_5820-nro_cgd
      AND vbeln  EQ it_ordem_check_5820-vbeln
      AND posnr  EQ it_ordem_check_5820-posnr
      AND nr_rot EQ it_ordem_check_5820-nr_rot.

  it_ordem_aux_5820 = it_ordem_check_5820.

*-CS2019001891 - JT - 04.02.2021 - inicio
* SORT it_ordem_check_5820 BY vbeln posnr ASCENDING.
* DELETE ADJACENT DUPLICATES FROM it_ordem_check_5820 COMPARING vbeln posnr.
  SORT it_ordem_check_5820 BY vbeln posnr nr_rot ASCENDING.
  DELETE ADJACENT DUPLICATES FROM it_ordem_check_5820 COMPARING vbeln posnr nr_rot.
*-CS2019001891 - JT - 04.02.2021 - fim

  LOOP AT it_ordem_check_5820 INTO wa_ordem_5820.

    vl_cont = vl_cont + 1.

    LOOP AT it_zsdt0134 INTO wa_zsdt0134 WHERE vbeln  EQ wa_ordem_5820-vbeln
                                           AND posnr  EQ wa_ordem_5820-posnr
                                           AND nr_rot EQ wa_ordem_5820-nr_rot
                                           AND nro_cg EQ wa_ordem_5820-nro_cgd.

      wa_ordem_5820-qtd_vinc_lt = wa_ordem_5820-qtd_vinc_lt + wa_zsdt0134-lfimg.
    ENDLOOP.

    CLEAR: wa_ordem_5820-qte_lib.

    LOOP AT it_ordem_aux_5820 INTO wa_ordem_aux_5820 WHERE vbeln   EQ wa_ordem_5820-vbeln
                                                       AND posnr   EQ wa_ordem_5820-posnr
                                                       AND nr_rot  EQ wa_ordem_5820-nr_rot
                                                       AND nro_cgd EQ wa_ordem_5820-nro_cgd.

      wa_ordem_5820-qte_lib = wa_ordem_5820-qte_lib + wa_ordem_aux_5820-qte_lib.
    ENDLOOP.

    IF wa_ordem_5820-qte_lib NE wa_ordem_5820-qtd_vinc_lt.
      vl_check_rom = abap_true.
      MESSAGE s000(z_fi) WITH TEXT-050 wa_ordem_5820-nro_cgd DISPLAY LIKE 'E'.
    ENDIF.

  ENDLOOP.

ENDFORM.

*-CS2021000218-27.10.2022-#91184-JT-inicio
*&---------------------------------------------------------------------*
*&      Form ajusta_status_receita
*&---------------------------------------------------------------------*
FORM ajusta_status_receita     USING p_param
                            CHANGING p_carga_5820 TYPE ty_carga_5820.

  DATA: vl_cont_ra     TYPE i,
        vl_cont_receit TYPE i,
        vl_quant       TYPE i,
        vl_assina      TYPE char1.

  IF p_param = 2.
    DELETE it_zsdt0302 WHERE nro_cgd  EQ p_carga_5820-nro_cgd.
    DELETE it_zsdt0298 WHERE nro_cgd  EQ p_carga_5820-nro_cgd.

    SELECT *
      FROM zsdt0302
 APPENDING TABLE it_zsdt0302
     WHERE nro_cgd  EQ p_carga_5820-nro_cgd.

    SELECT *
      FROM zsdt0298
 APPENDING TABLE it_zsdt0298
     WHERE nro_cgd   EQ p_carga_5820-nro_cgd
       AND cancelado EQ abap_off.

    SORT it_zsdt0302 BY nro_cgd ch_referencia DESCENDING.
    DELETE ADJACENT DUPLICATES FROM it_zsdt0302 COMPARING nro_cgd.
  ENDIF.

*  CLEAR vl_quant.
*  LOOP AT it_zsdt0302 INTO wa_zsdt0302 WHERE nro_cgd = wa_zsdt0302-nro_cgd.
*    vl_quant = vl_quant + 1.
*  ENDLOOP.
*
*  IF vl_quant > 1.
*    LOOP AT it_zsdt0302 INTO wa_zsdt0302 WHERE nro_cgd = wa_zsdt0302-nro_cgd.
*      DATA(l_tabix_302) = sy-tabix.
*      READ TABLE it_zsdt0298 INTO wa_zsdt0298 WITH KEY nro_cgd       = wa_zsdt0302-nro_cgd
*                                                       ch_referencia = wa_zsdt0302-ch_referencia.
*      IF sy-subrc <> 0.
*        DELETE it_zsdt0302 INDEX l_tabix_302.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

  IF p_carga_5820-status = '0' OR
     p_carga_5820-status = '1'.
    p_carga_5820-solic_receit = abap_off.
    p_carga_5820-receituario = abap_off.
    EXIT.
  ENDIF.

  CLEAR wa_zsdt0302.
  READ TABLE it_zsdt0302 INTO wa_zsdt0302 WITH KEY nro_cgd = p_carga_5820-nro_cgd.

  IF sy-subrc <> 0.
    p_carga_5820-solic_receit = abap_off.
    p_carga_5820-receituario = abap_off.
    EXIT.
  ENDIF.

  IF wa_zsdt0302-gera_solicitacao_ra = abap_off.
    p_carga_5820-solic_receit =  '@00@'.  "led cinza
    p_carga_5820-receituario =  '@00@'.   "led cinza
    EXIT.
  ENDIF.

  CLEAR: vl_cont_ra, vl_cont_receit, vl_assina, wa_zsdt0298.
  LOOP AT it_zsdt0298 INTO wa_zsdt0298 WHERE nro_cgd = p_carga_5820-nro_cgd.
    IF wa_zsdt0298-status = '4'.
      vl_cont_receit = vl_cont_receit + 1.
    ENDIF.
    IF wa_zsdt0298-tipo_assinatura = '0'.
      vl_assina = abap_false.
    ELSE.
      vl_assina = abap_true.
    ENDIF.
    vl_cont_ra = vl_cont_ra + 1.
  ENDLOOP.

  IF sy-subrc <> 0 AND wa_zsdt0302-gera_solicitacao_ra = abap_on.
    p_carga_5820-solic_receit = '@5C@'.  "led vermelho
    p_carga_5820-receituario = '@5C@'.   "led vermelho
    EXIT.
  ENDIF.

  IF     vl_cont_ra <> wa_zsdt0302-qtd_solicitacao_ra.
    p_carga_5820-solic_receit = '@5C@'.  "led vermelho
    p_carga_5820-receituario  = '@5C@'.  "led vermelho
  ELSEIF vl_cont_ra = vl_cont_receit.
    p_carga_5820-solic_receit = '@5B@'.  "led verde
    p_carga_5820-receituario  = '@5B@'.  "led verde
  ELSEIF wa_zsdt0298-status = '0' OR
         wa_zsdt0298-status = '1' OR
         wa_zsdt0298-status = '2'.
    p_carga_5820-solic_receit = '@5B@'.  "led verde
    p_carga_5820-receituario  = '@5D@'.  "led amarelo
*   p_carga_5820-receituario  = COND #( WHEN vl_assina = abap_false THEN '@5B@'
*                                                                   ELSE '@5D@' ).
  ELSE.
    p_carga_5820-solic_receit = '@5B@'.  "led verde
    p_carga_5820-receituario  = '@5D@'.  "led amarelo
*   p_carga_5820-receituario  = COND #( WHEN vl_assina = abap_false THEN '@5B@'
*                                                                   ELSE '@5D@' ).
  ENDIF.

ENDFORM.
*-CS2021000218-27.10.2022-#91184-JT-fim

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
