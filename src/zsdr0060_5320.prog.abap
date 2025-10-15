*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5320
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_carga_5320,
         icone             TYPE char6,
         desc_transp       TYPE lfa1-name1,
         placa_cav         TYPE zsdt0129-placa_cav,
         placa_car1        TYPE zsdt0129-placa_car1,
         placa_car2        TYPE zsdt0129-placa_car2,
         placa_car3        TYPE zsdt0129-placa_car3,
         motorista         TYPE zsdt0129-motorista,
         desc_mot          TYPE lfa1-name1,
         dt_entrega        TYPE zsdt0129-dt_entrega,
         inco1             TYPE zsdt0129-inco1,
         cor(4)            TYPE c,
*        User Story 153510 - MMSILVA
         desc_status_icone TYPE icon-name.
*        User Story 153510 - MMSILVA
         INCLUDE TYPE zsdt0133.
TYPES: END OF ty_carga_5320.

TYPES: BEGIN OF ty_ordem_5320,
         nro_cg      TYPE zsdt0133-nro_cg,
         maktx       TYPE makt-maktx,
         qtd_vinc_lt TYPE zsdt0131-qtd_vinc,
         nr_rot      TYPE zsdt0082-nr_rot,
         cor(4)      TYPE c.
         INCLUDE STRUCTURE zsdt0131.
TYPES: END OF ty_ordem_5320.

TYPES: BEGIN OF ty_nlote_5320,
         edit       TYPE char1.
         INCLUDE STRUCTURE zsdt0134.
TYPES:   cellstyles TYPE lvc_t_styl.
TYPES: END OF ty_nlote_5320.

TYPES: BEGIN OF ty_sol_5320,
         nro_cg     TYPE zsdt0133-nro_cg,
         bezei      TYPE tvkbt-bezei,
         seq_ent_cg TYPE zsdt0130-seq_ent_cg,
         rot_desc   TYPE zsdt0132-rot_desc,
         name1      TYPE kna1-name1,
         maktx      TYPE makt-maktx,
         tval       TYPE zsdt0133-preco_frete.
         INCLUDE    STRUCTURE zsdt0131.
TYPES:   cellstyles TYPE lvc_t_styl.
TYPES: END OF ty_sol_5320.

DATA: g_custom_container_5320    TYPE REF TO cl_gui_custom_container,
      dg_splitter_1_5320         TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2_5320         TYPE REF TO cl_gui_splitter_container,
      dg_splitter_3_5320         TYPE REF TO cl_gui_splitter_container,
      dg_parent_1_5320           TYPE REF TO cl_gui_container,
      dg_parent_2_5320           TYPE REF TO cl_gui_container,
      dg_parent_3_5320           TYPE REF TO cl_gui_container,
      dg_parent_4_5320           TYPE REF TO cl_gui_container,
      dg_parent_5_5320           TYPE REF TO cl_gui_container,
      dg_parent_6_5320           TYPE REF TO cl_gui_container,
      ctl_alv1_5320              TYPE REF TO cl_gui_alv_grid,
      ctl_alv2_5320              TYPE REF TO cl_gui_alv_grid,
      ctl_alv3_5320              TYPE REF TO cl_gui_alv_grid,
      ctl_alv4_5320              TYPE REF TO cl_gui_alv_grid,
      gs_layout_5320_alv1        TYPE lvc_s_layo,
      gs_layout_5320_alv3        TYPE lvc_s_layo,
      gs_layout_5320_alv4        TYPE lvc_s_layo,
      it_fieldcatalog_carga_5320 TYPE lvc_t_fcat,
      it_fieldcatalog_ordem_5320 TYPE lvc_t_fcat,
      it_fieldcatalog_nlote_5320 TYPE lvc_t_fcat,
      it_sort_5320               TYPE lvc_t_sort,
      it_exclude_nlote_5320      TYPE ui_functions,
      it_exclude_fase_5320       TYPE ui_functions.

DATA: it_carga_5320       TYPE STANDARD TABLE OF ty_carga_5320,
      t_carga             TYPE STANDARD TABLE OF zsdt0129,
      it_ordem_5320       TYPE STANDARD TABLE OF ty_ordem_5320,
      it_ordem_check_5320 TYPE STANDARD TABLE OF ty_ordem_5320,
      it_nlote_5320       TYPE STANDARD TABLE OF ty_nlote_5320,
      it_sol_5320         TYPE STANDARD TABLE OF ty_sol_5320,
      it_carga_aux_5320   TYPE STANDARD TABLE OF ty_carga_5320.

DATA: vg_carga_5320         TYPE ty_carga_5320,
      vg_nlote_vbeln        TYPE zsdt0131-vbeln,
      vg_nlote_posnr        TYPE zsdt0131-posnr,
      vg_nlote_nr_rot       TYPE zsdt0082-nr_rot,
      vg_nlote_nro_cg       TYPE zsdt0133-nro_cg,
      vg_nlote_matnr        TYPE zsdt0131-matnr,
      vg_cg_para_rom        TYPE zsdt0133-nro_cg,
      vg_nlote_werks        TYPE zsdt0131-werks,
      vg_carga_click_status TYPE zsdt0133-status,
      vl_check2             TYPE char1,
      wa_zsdt0129           TYPE zsdt0129,
      it_sol_aux_5320       TYPE STANDARD TABLE OF ty_sol_5320,
      wa_sol_aux_5320       TYPE ty_sol_5320,
      it_zsdt0062           TYPE STANDARD TABLE OF zsdt0062,
      wa_zsdt0062_1         TYPE zsdt0062,
      wa_sol_5320           TYPE ty_sol_5320,
      vl_vinc1              TYPE zsdt0131-qtd_vinc,
      vl_vinc2              TYPE zsdt0131-qtd_vinc.

DATA: it_carga_canc_5320 TYPE TABLE OF ty_carga_5320.

DATA: wa_stable_5320 TYPE lvc_s_stbl .

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler_5320 DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      toolbar_5320 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      toolbar_ordem_5320 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      user_command_5320 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      data_changed_finished_5320 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      on_double_click_5320 FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      on_double_click_ordem_5320 FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      toolbar_nlote_5320 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      toolbar_fase_5320 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      on_f4_5320 FOR EVENT onf4 OF cl_gui_alv_grid
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
CLASS lcl_event_handler_5320 IMPLEMENTATION.

  METHOD toolbar_5320.

    DATA wa_tool TYPE stb_button.

    APPEND:
    VALUE #( butn_type = 3 ) TO e_object->mt_toolbar,
    VALUE #( function = 'ROMANEIO'    icon = '@0Q@' quickinfo = 'Gerar Romaneio' ) TO e_object->mt_toolbar,
    VALUE #( function = 'LISTAROM'    icon = '@D8@' quickinfo = 'Listar Romaneios' ) TO e_object->mt_toolbar,
    VALUE #( butn_type = 3 ) TO e_object->mt_toolbar,
    VALUE #( function = 'AUTORIZACAO' icon = '@96@' quickinfo = 'Aut. de Embarque' ) TO e_object->mt_toolbar,
    VALUE #( butn_type = 3 ) TO e_object->mt_toolbar,
    VALUE #( function = 'CANCAUTORIZ' icon = '@BA@' quickinfo = 'Cancelar Aut. de Embarque' ) TO e_object->mt_toolbar.

*    MOVE 3 TO WA_TOOL-BUTN_TYPE.
*    APPEND WA_TOOL TO E_OBJECT->MT_TOOLBAR.
*    CLEAR WA_TOOL.

*    WA_TOOL-FUNCTION = 'ROMANEIO'.
*    WA_TOOL-ICON     = '@0Q@'.
*    WA_TOOL-QUICKINFO = 'Gerar Romaneio'.
*    APPEND WA_TOOL TO E_OBJECT->MT_TOOLBAR.
*    CLEAR WA_TOOL.

*    WA_TOOL-FUNCTION = 'LISTAROM'.
*    WA_TOOL-ICON     = '@D8@'.
*    WA_TOOL-QUICKINFO = 'Listar Romaneios'.
*    APPEND WA_TOOL TO E_OBJECT->MT_TOOLBAR.
*    CLEAR WA_TOOL.

*    MOVE 3 TO WA_TOOL-BUTN_TYPE.
*    APPEND WA_TOOL TO E_OBJECT->MT_TOOLBAR.
*    CLEAR WA_TOOL.

*    WA_TOOL-FUNCTION = 'AUTORIZACAO'.
*    WA_TOOL-ICON     = '@96@'.
*    WA_TOOL-QUICKINFO = 'Aut. de Embarque'.
*    APPEND WA_TOOL TO E_OBJECT->MT_TOOLBAR.
*    CLEAR WA_TOOL.




  ENDMETHOD.             "DISPLAY

  METHOD toolbar_ordem_5320.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'PEDIDOS'.
    wa_tool-icon     = '@BB@'.
    wa_tool-quickinfo = 'Listar Pedidos'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

  ENDMETHOD.             "DISPLAY

  METHOD toolbar_nlote_5320.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'ADD_ROW'.
    wa_tool-icon     = '@17@'.
    wa_tool-quickinfo = 'Adicionar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'DEL_ROW'.
    wa_tool-icon     = '@18@'.
    wa_tool-quickinfo = 'Eliminar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'EDIT_ROW'.
    wa_tool-icon     = '@0Z@'.
    wa_tool-quickinfo = 'Editar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'SAVE_NLOTE'.
    wa_tool-icon     = '@2L@'.
    wa_tool-quickinfo = 'Salvar'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

  ENDMETHOD.             "DISPLAY

  METHOD toolbar_fase_5320.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = '1'.
    wa_tool-icon     = '@17@'.
    wa_tool-quickinfo = 'Adicionar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = '2'.
    wa_tool-icon     = '@18@'.
    wa_tool-quickinfo = 'Eliminar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = '3'.
    wa_tool-icon     = '@0Z@'.
    wa_tool-quickinfo = 'Editar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = '4'.
    wa_tool-icon     = '@2L@'.
    wa_tool-quickinfo = 'Salvar'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

  ENDMETHOD.             "DISPLAY

  METHOD user_command_5320.

    DATA: wa_nlote_5320    TYPE ty_nlote_5320,
          wa_ordem_5320    TYPE ty_ordem_5320,
          wa_carga_5320    TYPE ty_carga_5320,
*          IT_CARGA_CANC_5320 TYPE TABLE OF TY_CARGA_5320,
          it_selected_rows TYPE lvc_t_row,
          wa_selected_rows TYPE lvc_s_row,
          vl_lfimg         TYPE zsdt0134-lfimg,
          vl_check_rom     TYPE char1,
          vl_check_initial TYPE char1,
          vl_lines         TYPE i.

    IF e_ucomm = 'ROMANEIO'.

      CLEAR: it_selected_rows, wa_selected_rows, vl_check_rom, it_carga_aux_5320, vl_lines.

      CALL METHOD ctl_alv1_5320->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      DESCRIBE TABLE it_selected_rows LINES vl_lines.

      IF vl_lines NE 1.
        MESSAGE TEXT-054 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
        READ TABLE it_carga_5320 INTO wa_carga_5320 INDEX wa_selected_rows-index.
        IF sy-subrc IS INITIAL.

          CLEAR: wa_zsdt0133.

          SELECT SINGLE status
            FROM zsdt0133
            INTO vl_status
            WHERE nro_cg EQ wa_carga_5320-nro_cg.

          IF wa_carga_5320-status NE vl_status.
            MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
          ELSEIF vl_status NE 5.
            MESSAGE TEXT-100 TYPE 'S' DISPLAY LIKE 'E'.
          ELSE.

            CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
              EXPORTING
                chave          = wa_carga_5320-nro_cg
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            ELSE.
              APPEND wa_carga_5320 TO it_carga_aux_5320.
            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

      IF it_carga_aux_5320 IS NOT INITIAL.

        PERFORM check_ordens_rom_5320 USING sy-tabix CHANGING vl_check_rom.

        IF vl_check_rom IS INITIAL.
          PERFORM salva_romaneios CHANGING vl_check_rom.

          IF vl_check_rom IS INITIAL.

            CLEAR wa_carga_5320.
            READ TABLE it_carga_aux_5320 INTO wa_carga_5320 INDEX 1.

            wa_carga_5320-status = 6.
            wa_carga_5320-icone = '@0Q@'.
            wa_carga_5320-desc_status_icone = 'Romaneio Gerado'.
            MODIFY it_carga_5320 FROM wa_carga_5320 INDEX wa_selected_rows-index.

          ENDIF.

          CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
            EXPORTING
              chave = wa_carga_5320-nro_cg.

          CALL METHOD ctl_alv1_5320->refresh_table_display
            EXPORTING
              is_stable = _stable.

        ENDIF.

      ENDIF.

    ELSEIF e_ucomm = 'LISTAROM'.

      CLEAR: it_selected_rows, wa_selected_rows, vg_cg_para_rom, vl_lines, wa_carga_5320.

      CALL METHOD ctl_alv1_5320->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      DESCRIBE TABLE it_selected_rows LINES vl_lines.

      IF vl_lines NE 1.
        MESSAGE TEXT-045 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.

        READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
        READ TABLE it_carga_5320 INTO wa_carga_5320 INDEX wa_selected_rows-index.

        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
            EXPORTING
              chave          = wa_carga_5320-nro_cg
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
          ELSE.
            vg_cg_para_rom = wa_carga_5320-nro_cg.

            CALL SCREEN 5321 STARTING AT 5 5 ENDING AT 80 20.

          ENDIF.

        ENDIF.

      ENDIF.

    ELSEIF e_ucomm = 'ADD_ROW'.
      DATA:          v_matkl          TYPE mara-matkl.

      IF vg_nlote_vbeln  IS NOT INITIAL AND
         vg_nlote_posnr  IS NOT INITIAL AND
         vg_nlote_nro_cg IS NOT INITIAL.

        CLEAR: wa_nlote_5320.
        wa_nlote_5320-edit    = abap_true.
        wa_nlote_5320-vbeln   = vg_nlote_vbeln.
        wa_nlote_5320-posnr   = vg_nlote_posnr.
*-CS2019001891 - JT - 04.02.2021 - inicio
        wa_nlote_5320-nr_rot  = vg_nlote_nr_rot.
*-CS2019001891 - JT - 04.02.2021 - fim
        wa_nlote_5320-nro_cg  = vg_nlote_nro_cg.

        "CS2017002830 - 12.12.2017 - Ini
        IF vg_nlote_matnr IS NOT INITIAL.
          SELECT SINGLE *
            FROM mara INTO @DATA(_wl_mara)
           WHERE matnr = @vg_nlote_matnr.

*----> CS1059298 / IR124784 ---->
*          IF ( sy-subrc = 0 ) AND ( _wl_mara-matkl = '700230' OR _wl_mara-matkl = '700240' ) .
          IF ( sy-subrc = 0 ).
            SELECT SINGLE valfrom INTO v_matkl
             FROM setleaf
             WHERE setname = 'ZSDR0060_LOCK_CAMPO'
             AND valfrom EQ  _wl_mara-matkl.
            IF sy-subrc EQ 0.
*<---- CS1059298 / IR124784 <----
              "wa_nlote_5320-brgew = _wl_mara-brgew." DEVK9A1VP2 - SD - Ajustar peso real dos lotes de sementes #129907
            ENDIF.
          ENDIF.
        ENDIF.
        "CS2017002830 - 12.12.2017 - Fim

        APPEND wa_nlote_5320 TO it_nlote_5320.
        PERFORM bloqueia_linhas_nlote_5320.

        CALL METHOD ctl_alv4_5320->refresh_table_display
          EXPORTING
            is_stable = _stable.
      ENDIF.

    ELSEIF e_ucomm = 'DEL_ROW'.

      CLEAR: it_selected_rows, wa_selected_rows.

      CALL METHOD ctl_alv4_5320->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      LOOP AT it_selected_rows INTO wa_selected_rows.
        READ TABLE it_nlote_5320 INTO wa_nlote_5320 INDEX wa_selected_rows-index.
        IF sy-subrc IS INITIAL.
          wa_nlote_5320-charg = 'DELETE'.
          MODIFY it_nlote_5320 FROM wa_nlote_5320 INDEX wa_selected_rows-index..
          CLEAR: wa_nlote_5320.
        ENDIF.
      ENDLOOP.

      DELETE it_nlote_5320 WHERE charg EQ 'DELETE'.
      CALL METHOD ctl_alv4_5320->refresh_table_display
        EXPORTING
          is_stable = _stable.

    ELSEIF e_ucomm = 'EDIT_ROW'.

      CLEAR: it_selected_rows, wa_selected_rows.

      CALL METHOD ctl_alv4_5320->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      LOOP AT it_selected_rows INTO wa_selected_rows.
        READ TABLE it_nlote_5320 INTO wa_nlote_5320 INDEX wa_selected_rows-index.
        IF sy-subrc IS INITIAL.
          MOVE abap_true TO wa_nlote_5320-edit.
          MODIFY it_nlote_5320 FROM wa_nlote_5320 INDEX wa_selected_rows-index..
          CLEAR: wa_nlote_5320.
        ENDIF.
      ENDLOOP.

      PERFORM bloqueia_linhas_nlote_5320.
      CALL METHOD ctl_alv4_5320->refresh_table_display
        EXPORTING
          is_stable = _stable.

    ELSEIF e_ucomm = 'SAVE_NLOTE'.

      DATA: wa_zsdt0134 TYPE zsdt0134,
            it_zsdt0134 TYPE STANDARD TABLE OF zsdt0134.

      DATA vl TYPE char1.

      IF NOT it_nlote_5320 IS INITIAL.
        DATA(vl_nro_cg) = it_nlote_5320[ 1 ]-nro_cg.
      ENDIF.

      SELECT c~*
        FROM zsdt0129 AS a
           INNER JOIN zsdt0131 AS b ON b~nro_lote EQ a~nro_lote
           INNER JOIN zsdt0132 AS c ON c~nr_rot EQ b~cod_loc_emb
            INTO TABLE @DATA(t_0132)
         WHERE a~nro_cg EQ @vl_nro_cg.
      IF line_exists( t_0132[ armazem = abap_true ]  ).
        DATA(vl_check) = abap_true. "// É Armazem
      ELSE.
        CLEAR vl_check. "// Não é Armazem
      ENDIF.

      CLEAR vl.
      IF vl_check IS INITIAL.
        IF vg_carga_click_status NE 5.
          MESSAGE TEXT-146 TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          vl = abap_true.
        ENDIF.
      ELSE.
*        IF VG_CARGA_CLICK_STATUS NE 4.
*          MESSAGE TEXT-147 TYPE 'S' DISPLAY LIKE 'E'.
*        ELSE.
        vl = abap_true.
*        ENDIF.
      ENDIF.

      IF vl IS NOT INITIAL.
        CLEAR: vl_check_initial.

        "Check se a quantidade de Lotes a serem salvos não ultrapassa o volume vinculado
        "ou dados em branco
        LOOP AT it_nlote_5320 INTO wa_nlote_5320.
          IF wa_nlote_5320-charg IS INITIAL OR
             wa_nlote_5320-lfimg IS INITIAL OR
             wa_nlote_5320-nr_fase IS INITIAL.

            vl_check_initial = abap_true.
          ENDIF.
          vl_lfimg = vl_lfimg + wa_nlote_5320-lfimg.
        ENDLOOP.

*        PERFORM CHECK_VOLUME_PEDIDO CHANGING VL_CHECK_INITIAL.

        READ TABLE it_ordem_5320 INTO wa_ordem_5320 WITH KEY nro_cg = vg_nlote_nro_cg
                                                             vbeln  = vg_nlote_vbeln
                                                             posnr  = vg_nlote_posnr
                                                             nr_rot = vg_nlote_nr_rot.

        IF vl_lfimg LE wa_ordem_5320-qtd_vinc AND vl_check_initial NE abap_true.

          LOOP AT it_ordem_5320 INTO wa_ordem_5320 WHERE nro_cg EQ vg_nlote_nro_cg
                                                     AND vbeln  EQ vg_nlote_vbeln
                                                     AND posnr  EQ vg_nlote_posnr
                                                     AND nr_rot EQ vg_nlote_nr_rot.
            wa_ordem_5320-qtd_vinc_lt = vl_lfimg.
            MODIFY it_ordem_5320 FROM wa_ordem_5320 INDEX sy-tabix.
          ENDLOOP.

          LOOP AT it_nlote_5320 INTO wa_nlote_5320 WHERE edit EQ abap_true.
            wa_zsdt0134-nro_cg     = wa_nlote_5320-nro_cg.
            wa_zsdt0134-vbeln      = wa_nlote_5320-vbeln.
            wa_zsdt0134-posnr      = wa_nlote_5320-posnr.
*-CS2019001891 - JT - 04.02.2021 - inicio
            wa_zsdt0134-nr_rot     = wa_nlote_5320-nr_rot.
*-CS2019001891 - JT - 04.02.2021 - fim
            wa_zsdt0134-charg      = wa_nlote_5320-charg.
            wa_zsdt0134-lfimg      = wa_nlote_5320-lfimg.
            wa_zsdt0134-brgew      = wa_nlote_5320-brgew. "CS2017002830 - 12.12.2017
            wa_zsdt0134-peso_liq_brt = wa_nlote_5320-peso_liq_brt. "CS2017002830 - 12.12.2017
            wa_zsdt0134-ebeln      = wa_nlote_5320-ebeln.
            wa_zsdt0134-status     = wa_nlote_5320-status.
            wa_zsdt0134-usnam      = sy-uname.
            wa_zsdt0134-data_atual = sy-datum.
            wa_zsdt0134-hora_atual = sy-uzeit.
            wa_zsdt0134-categoria  = wa_nlote_5320-categoria.
            wa_zsdt0134-nr_fase    = wa_nlote_5320-nr_fase.
            MODIFY zsdt0134 FROM wa_zsdt0134.
            wa_nlote_5320-edit = abap_false.
            MODIFY it_nlote_5320 FROM wa_nlote_5320 INDEX sy-tabix.
          ENDLOOP.

          SELECT *
            FROM zsdt0134
            INTO TABLE it_zsdt0134
            WHERE nro_cg EQ vg_nlote_nro_cg
              AND vbeln  EQ vg_nlote_vbeln
              AND posnr  EQ vg_nlote_posnr
              AND nr_rot EQ vg_nlote_nr_rot.

          LOOP AT it_zsdt0134 INTO wa_zsdt0134.
            READ TABLE it_nlote_5320 INTO wa_nlote_5320 WITH KEY nro_cg  = wa_zsdt0134-nro_cg
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

          CALL METHOD ctl_alv4_5320->get_frontend_layout
            IMPORTING
              es_layout = gs_layout_5320_alv4.

          gs_layout_5320_alv4-cwidth_opt = abap_true.

          CALL METHOD ctl_alv4_5320->set_frontend_layout
            EXPORTING
              is_layout = gs_layout_5320_alv4.

          PERFORM bloqueia_linhas_nlote_5320.
          CALL METHOD ctl_alv4_5320->refresh_table_display
            EXPORTING
              is_stable = _stable.

          MESSAGE TEXT-049 TYPE 'S'.

          CALL METHOD ctl_alv3_5320->refresh_table_display
            EXPORTING
              is_stable = _stable.

        ELSEIF vl_check_initial EQ abap_true.
          MESSAGE TEXT-053 TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          MESSAGE TEXT-048 TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

        CLEAR: vl_lfimg.

      ENDIF.

    ELSEIF e_ucomm = 'PEDIDOS'.

      IF it_ordem_5320 IS NOT INITIAL.
        CALL SCREEN 5322 STARTING AT 5 5 ENDING AT 80 20.
        LEAVE TO SCREEN 5000.
      ENDIF.

    ELSEIF e_ucomm = 'AUTORIZACAO'.

      CLEAR: it_selected_rows, wa_selected_rows.

      CALL METHOD ctl_alv1_5320->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      LOOP AT it_selected_rows INTO wa_selected_rows.
        READ TABLE it_carga_5320 INTO wa_carga_5320 INDEX wa_selected_rows-index.
        IF wa_carga_5320-status LT '4'. " IGUAL A IF WA_CARGA_5320-STATUS < '4'
          MESSAGE TEXT-141 TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.

          SELECT c~*
            FROM zsdt0129 AS a
               INNER JOIN zsdt0131 AS b ON b~nro_lote EQ a~nro_lote
               INNER JOIN zsdt0132 AS c ON c~nr_rot EQ b~cod_loc_emb
                INTO TABLE @t_0132
             WHERE a~nro_cg EQ @wa_carga_5320-nro_cg.
*         "// Se o Local de Embarque for do Tipo Armazem não Cancelar a Autorização de Embarque
          IF NOT line_exists( t_0132[ armazem = abap_true ] ).
            MESSAGE TEXT-145 TYPE 'S' DISPLAY LIKE 'E'.
            vl_check = abap_true.
            EXIT.
          ENDIF.

          IF line_exists( t_0132[ transportadora = abap_false ] ).
            DATA(wa_5320) = it_carga_5320[ nro_cg = wa_carga_5320-nro_cg ].

            IF wa_5320-placa_cav IS INITIAL OR
*               WA_5230-PLACA_CAR1 IS INITIAL OR
               wa_5320-motorista IS INITIAL.
              MESSAGE TEXT-042 TYPE 'S' DISPLAY LIKE 'E'.
              vl_check = abap_true.
              EXIT.
            ENDIF.
          ENDIF.

          " Valida se a carga está com todos os lotes de Produtos vinculados
*          SELECT SUM( QTD_EMKG )
*            FROM ZSDT0131 AS A
*              INNER JOIN ZSDT0129 AS B ON A~NRO_LOTE EQ B~NRO_LOTE
*            INTO @DATA(VL_0131)
*            WHERE A~STATUS NE @ABAP_TRUE
*              AND B~STATUS NE @ABAP_TRUE
*              AND B~NRO_CG EQ @WA_CARGA_5320-NRO_CG.
*
*          IF SY-SUBRC IS INITIAL.
*
*            SELECT SUM( PESO_LIQ_BRT )
*              FROM ZSDT0134
*                  INTO @DATA(VL_0134)
*              WHERE NRO_CG EQ @WA_CARGA_5320-NRO_CG
*                AND STATUS NE @ABAP_TRUE.
*
*            IF VL_0134 NE VL_0131.
*              MESSAGE 'Quantidade de Lotes Divergem da quantidade das Ordens!' TYPE 'S' DISPLAY LIKE 'E'.
*              VL_CHECK = ABAP_TRUE.
*              EXIT.
*            ENDIF.
*          ENDIF.

          CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
            EXPORTING
              chave          = wa_carga_5320-nro_cg
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            vl_check = abap_true.
            EXIT.
          ENDIF.

          IF wa_carga_5320-status >= 4.
            PERFORM gerar_pdf_embarque USING wa_carga_5320-nro_cg abap_true.
            IF wa_carga_5320-status EQ 4.

              UPDATE zsdt0133
                SET status = 5
                WHERE nro_cg EQ wa_carga_5320-nro_cg.

              wa_carga_5320-status = 5.
              wa_carga_5320-icone  = '@96@'.
              wa_carga_5320-desc_status_icone  = 'Embarque Autorizado'.

              MODIFY it_carga_5320 FROM wa_carga_5320 INDEX wa_selected_rows-index.

            ENDIF.

          ENDIF.

          CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
            EXPORTING
              chave = wa_carga_5320-nro_cg.

*          LEAVE TO SCREEN 5000.
*
          CALL METHOD ctl_alv1_5320->refresh_table_display
            EXPORTING
              is_stable = _stable.

        ENDIF.
      ENDLOOP.

    ELSEIF e_ucomm = 'CANCAUTORIZ'.

      CLEAR:
            it_carga_canc_5320,
            vl_check,
            vl_lines,
            wa_zsdt0133.

      CALL METHOD ctl_alv1_5320->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      DESCRIBE TABLE it_selected_rows LINES vl_lines.

      IF vl_lines NE 1.
        MESSAGE TEXT-054 TYPE 'S' DISPLAY LIKE 'E'.
        MOVE abap_true TO vl_check.
      ENDIF.

      IF vl_check IS INITIAL.

        READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
        READ TABLE it_carga_5320 INTO wa_carga_5320 INDEX wa_selected_rows-index.

        IF sy-subrc IS INITIAL.

          IF wa_carga_5320-status EQ '5'.

*             "// Busca dados de Local de Entrega para validação
            SELECT c~*
              FROM zsdt0129 AS a
              INNER JOIN zsdt0131 AS b ON b~nro_lote EQ a~nro_lote
              INNER JOIN zsdt0132 AS c ON c~nr_rot EQ b~cod_loc_emb
              INTO TABLE @t_0132
              WHERE a~nro_cg EQ @wa_carga_5320-nro_cg.
*             "// Se o Local de Embarque for do Tipo Armazem não Cancelar a Autorização de Embarque
            IF NOT line_exists( t_0132[ armazem = abap_true ] ).
              MESSAGE TEXT-148 TYPE 'S' DISPLAY LIKE 'E'.
              vl_check = abap_true.
              EXIT.
            ENDIF.
          ENDIF.

          CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
            EXPORTING
              chave          = wa_carga_5320-nro_cg
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
             WHERE nro_cg EQ wa_carga_5320-nro_cg.

            IF wa_zsdt0133-status NE wa_carga_5320-status.
              MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
            ELSE.

              APPEND wa_carga_5320 TO it_carga_canc_5320.
              IF NOT line_exists( t_0132[ armazem = abap_true ] ).
                PERFORM busca_lotes_produtos_5320 CHANGING vl_check.
              ENDIF.

              IF vl_check IS INITIAL.

                PERFORM cancela_autorizacao_5320.

                CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
                  EXPORTING
                    chave = wa_carga_5320-nro_cg.

                LEAVE TO SCREEN 5000.
              ELSE.
                MESSAGE TEXT-093 TYPE 'S' DISPLAY LIKE 'E'.
              ENDIF.

            ENDIF.

            CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
              EXPORTING
                chave = wa_carga_5320-nro_cg.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.
  ENDMETHOD.                    "USER_COMMAND

  METHOD data_changed_finished_5320.

    DATA: it_stable     TYPE lvc_s_stbl,
          wa_good_cells TYPE lvc_s_modi,
          wa_nlote_5320 TYPE ty_nlote_5320,
          it_f4_5320    TYPE STANDARD TABLE OF ty_f4_nlote,
          vl_cont       TYPE i,
          it_zsdt0062   TYPE STANDARD TABLE OF zsdt0062,
          it_ekbe       TYPE STANDARD TABLE OF ekbe,
          wa_ekbe       TYPE ekbe,
          it_ekpo       TYPE STANDARD TABLE OF ekpo,
          wa_ekpo       TYPE ekpo,
          it_ekbe_aux   TYPE STANDARD TABLE OF ekbe.

    it_stable-row = 'X'.
    it_stable-col = 'X'.

    IF e_modified EQ abap_true.
      READ TABLE et_good_cells INTO wa_good_cells INDEX 1.
      CHECK sy-subrc = 0.

      CASE wa_good_cells-fieldname.
        WHEN 'CHARG'.

          "CS2017002830 - 12.12.2017 - Ini
        WHEN 'BRGEW' OR 'LFIMG'.
          READ TABLE it_nlote_5320 ASSIGNING FIELD-SYMBOL(<fs_nlote_5320>) INDEX wa_good_cells-row_id.
          CHECK ( sy-subrc = 0 ).

          IF <fs_nlote_5320>-brgew = 0.
            CLEAR: <fs_nlote_5320>-peso_liq_brt.
          ENDIF.

          IF ( <fs_nlote_5320>-lfimg <> 0 ) AND ( <fs_nlote_5320>-brgew <> 0 ).
            <fs_nlote_5320>-peso_liq_brt = <fs_nlote_5320>-lfimg * <fs_nlote_5320>-brgew.
          ENDIF.
          "CS2017002830 - 12.12.2017 - Fim
      ENDCASE.

      CALL METHOD ctl_alv4_5320->refresh_table_display
        EXPORTING
          is_stable = it_stable.

    ENDIF.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_

  METHOD on_double_click_5320.

    DATA: vl_row TYPE i.

    PERFORM alv_carga_click_5320 USING e_row-index.
    CLEAR: it_nlote_5320, vg_nlote_vbeln, vg_nlote_posnr, vg_nlote_nro_cg.

    CALL METHOD ctl_alv3_5320->get_frontend_layout
      IMPORTING
        es_layout = gs_layout_5320_alv3.

    gs_layout_5320_alv3-cwidth_opt = abap_true.

    CALL METHOD ctl_alv3_5320->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_5320_alv3.

    CALL METHOD ctl_alv1_5320->refresh_table_display
      EXPORTING
        is_stable = _stable.

    CALL METHOD ctl_alv3_5320->refresh_table_display
      EXPORTING
        is_stable = _stable.

    CALL METHOD ctl_alv4_5320->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD on_double_click_ordem_5320.

    DATA: vl_row TYPE i.

    PERFORM alv_ordem_click_5320 USING e_row-index.
    PERFORM bloqueia_linhas_nlote_5320.

    CALL METHOD ctl_alv3_5320->refresh_table_display
      EXPORTING
        is_stable = _stable.

    CALL METHOD ctl_alv4_5320->get_frontend_layout
      IMPORTING
        es_layout = gs_layout_5320_alv4.

    gs_layout_5320_alv4-cwidth_opt = abap_true.

    CALL METHOD ctl_alv4_5320->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_5320_alv4.

    CALL METHOD ctl_alv4_5320->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDMETHOD.

  METHOD on_f4_5320.

    DATA: it_ret_5320 TYPE STANDARD TABLE OF ddshretval,
          it_f4_5320  TYPE STANDARD TABLE OF ty_f4_nlote,
          it_f4_nfase TYPE STANDARD TABLE OF ty_f4_nfase,
          vl_cont     TYPE i,
          it_zsdt0062 TYPE STANDARD TABLE OF zsdt0062,
          it_ekpo     TYPE STANDARD TABLE OF ekpo,
          wa_ekpo     TYPE ekpo,
          it_ekbe     TYPE STANDARD TABLE OF ekbe,
          wa_ekbe     TYPE ekbe,
          it_ekbe_aux TYPE STANDARD TABLE OF ekbe,
          it_mchb     TYPE STANDARD TABLE OF mchb,
          wa_mchb     TYPE mchb,
          it_mch1     TYPE STANDARD TABLE OF mch1,
          wa_mch1     TYPE mch1,
          it_mseg     TYPE STANDARD TABLE OF mseg,
          wa_mseg     TYPE mseg,
          it_mslb     TYPE STANDARD TABLE OF mslb,
          wa_mslb     TYPE mslb,
          it_zmmt0102 TYPE STANDARD TABLE OF zmmt0102,
          wa_zmmt0102 TYPE zmmt0102.

    DATA: wa_f4_5320    TYPE ty_f4_nlote,
          wa_f4_nfase   TYPE ty_f4_nfase,
          wa_nlote_5320 TYPE ty_nlote_5320,
          wa_ret        TYPE ddshretval,
          wa_modi       TYPE lvc_s_modi,
          l_lfimg       TYPE zsdt0134-lfimg,
          l_saldo_fase  TYPE zsdt0134-lfimg.

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

    DATA : it_fmap      TYPE STANDARD TABLE OF dselc,
           wa_fmap      TYPE dselc,
           it_field_tab TYPE TABLE OF dfies,
           wa_field_tab TYPE dfies.

    DATA: vg_charg TYPE mch1-charg.

    IF e_fieldname = 'CHARG'.

      wa_fmap-fldname = 'F0001'.
      wa_fmap-dyfldname = 'CHARG'.
      APPEND wa_fmap TO it_fmap.
      wa_fmap-fldname = 'F0002'.
      wa_fmap-dyfldname = 'EBELN'.
      APPEND wa_fmap TO it_fmap.
      wa_fmap-fldname = 'F0008'.
      wa_fmap-dyfldname = 'CATEGORIA'.
      APPEND wa_fmap TO it_fmap.

      READ TABLE it_nlote_5320 INTO wa_nlote_5320 INDEX es_row_no-row_id.
      IF sy-subrc IS INITIAL AND wa_nlote_5320-edit EQ abap_true.

        "-----------------

        SELECT *
          FROM mchb
          INTO TABLE it_mchb
          WHERE matnr EQ vg_nlote_matnr
            AND werks EQ vg_nlote_werks.

        SELECT *
          FROM mslb
          INTO TABLE it_mslb
          WHERE matnr EQ vg_nlote_matnr
            AND werks EQ vg_nlote_werks.


        IF it_mchb IS NOT INITIAL.

          SELECT *
            FROM mch1
            INTO TABLE it_mch1
            FOR ALL ENTRIES IN it_mchb
            WHERE charg EQ it_mchb-charg
              AND matnr EQ it_mchb-matnr.

        ENDIF.

        IF it_mslb IS NOT INITIAL.

          SELECT *
            FROM mch1
            APPENDING TABLE it_mch1
            FOR ALL ENTRIES IN it_mslb
            WHERE charg EQ it_mslb-charg
              AND matnr EQ it_mslb-matnr.

        ENDIF.

        SELECT *
          FROM zmmt0102
          APPENDING TABLE it_zmmt0102
          FOR ALL ENTRIES IN it_mchb
         WHERE charg EQ it_mchb-charg
          AND  matnr EQ it_mchb-matnr.



        LOOP AT it_mchb INTO wa_mchb.

          wa_f4_5320-charg = wa_mchb-charg.
          wa_f4_5320-lgort = wa_mchb-lgort.
          wa_f4_5320-werks = wa_mchb-werks.
          wa_f4_5320-matnr = wa_mchb-matnr.

          READ TABLE it_mseg INTO wa_mseg WITH KEY charg = wa_mchb-charg
                                                   lgort = wa_mchb-lgort
                                                   werks = wa_mchb-werks
                                                   matnr = wa_mchb-matnr.
          IF sy-subrc IS INITIAL.
            READ TABLE it_ekbe INTO wa_ekbe WITH KEY belnr = wa_mseg-mblnr
                                                     gjahr = wa_mseg-mjahr
                                                     matnr = wa_mseg-matnr.
            IF sy-subrc IS INITIAL.
              wa_f4_5320-ebeln = wa_ekbe-ebeln.
            ENDIF.
          ENDIF.

          READ TABLE it_mch1 INTO wa_mch1 WITH KEY matnr = wa_f4_5320-matnr
                                                   charg = wa_f4_5320-charg.
          IF sy-subrc IS INITIAL.
            wa_f4_5320-vfdat = wa_mch1-vfdat.
          ENDIF.

          wa_f4_5320-clabs = wa_mchb-clabs.


          READ TABLE it_zmmt0102 INTO wa_zmmt0102 WITH KEY  matnr = wa_f4_5320-matnr
                                                            charg = wa_f4_5320-charg.
          IF sy-subrc = 0.
            wa_f4_5320-categoria = wa_zmmt0102-categoria.
          ENDIF.

          APPEND wa_f4_5320 TO it_f4_5320.
          CLEAR wa_f4_5320.

        ENDLOOP.

        LOOP AT it_mslb INTO wa_mslb.

          wa_f4_5320-charg = wa_mslb-charg.
          wa_f4_5320-werks = wa_mslb-werks.
          wa_f4_5320-matnr = wa_mslb-matnr."

          "CS2022000698 - ZSDT0112 - exibir o código do fornecedor na tela de vinculação de lotes / Anderson Oenning
*          wa_f4_5320-lifnr = wa_mslb-lifnr.
          "CS2022000698 - ZSDT0112 - exibir o código do fornecedor na tela de vinculação de lotes / Anderson Oenning

          READ TABLE it_mch1 INTO wa_mch1 WITH KEY matnr = wa_f4_5320-matnr
                                                   charg = wa_f4_5320-charg.
          IF sy-subrc IS INITIAL.
            wa_f4_5320-vfdat = wa_mch1-vfdat.
          ENDIF.

          wa_f4_5320-clabs = wa_mslb-lblab.

          APPEND wa_f4_5320 TO it_f4_5320.
          CLEAR wa_f4_5320.

        ENDLOOP.

        "------------------------

      ENDIF.

      DELETE it_f4_5320 WHERE clabs IS INITIAL.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'CHARG'
          window_title    = 'Lista de Lotes'(002)
          value_org       = 'S'
        TABLES
          value_tab       = it_f4_5320
          return_tab      = it_ret_5320
          dynpfld_mapping = it_fmap
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.


      IF sy-subrc = 0.
        ASSIGN er_event_data->m_data->* TO <itab>.
        READ TABLE it_ret_5320 INTO wa_ret INDEX 1.
        wa_modi-row_id   = es_row_no-row_id.
        wa_modi-fieldname = 'CHARG'.
        wa_modi-value     = wa_ret-fieldval.
        APPEND wa_modi TO <itab>.

        " Início - DEVK9A1VP2 - 17.02.2024 SD - Ajustar peso real dos lotes de sementes #129907 RSA
        vg_charg = wa_modi-value.
        SELECT SINGLE cuobj_bm
               FROM mch1
               INTO @DATA(v_cuobj_bm)
               WHERE matnr EQ @vg_nlote_matnr
               AND   charg EQ @vg_charg.

        SELECT atinn UP TO 1 ROWS
               FROM cabn
               INTO @DATA(v_atinn)
               WHERE atnam = 'PESO_BAG'.
        ENDSELECT.


        IF NOT v_cuobj_bm IS INITIAL AND NOT v_atinn IS INITIAL.
          SELECT dec_value_from UP TO 1 ROWS
                 FROM ausp
                 INTO @DATA(v_dec_value_from)
                 WHERE objek EQ @v_cuobj_bm
                 AND   atinn EQ @v_atinn
                 AND   klart EQ '023'.
          ENDSELECT.

          IF NOT v_dec_value_from IS INITIAL.
            READ TABLE it_nlote_5320 INTO wa_nlote_5320 INDEX es_row_no-row_id.
            IF sy-subrc IS INITIAL AND wa_nlote_5320-edit EQ abap_true.
              wa_nlote_5320-brgew = v_dec_value_from.
              MODIFY it_nlote_5320 FROM wa_nlote_5320 INDEX sy-tabix.
            ENDIF.
          ENDIF.
        ENDIF.
        " Fim - DEVK9A1VP2 - 17.02.2024 SD - Ajustar peso real dos lotes de sementes #129907 RSA

        READ TABLE it_ret_5320 INTO wa_ret INDEX 2.
        wa_modi-fieldname = 'EBELN'.
        wa_modi-value     = wa_ret-fieldval.
        APPEND wa_modi TO <itab>.
        READ TABLE it_ret_5320 INTO wa_ret INDEX 3.
        wa_modi-fieldname = 'CATEGORIA'.
        wa_modi-value     = wa_ret-fieldval.
        APPEND wa_modi TO <itab>.
      ENDIF.

      er_event_data->m_event_handled = 'X'."(to inform grid that f4 was handled manually)

    ELSEIF e_fieldname = 'NR_FASE'.

*-CS2019001891 - JT - 03.02.2021 - inicio
      FREE: it_field_tab.

      wa_fmap-fldname = 'F0001'.
      wa_fmap-dyfldname = 'NR_FASE'.
      wa_fmap-fldname = 'F0002'.
      wa_fmap-dyfldname = 'CATEGORIA'.
      APPEND wa_fmap TO it_fmap.
      wa_fmap-fldname = 'F0003'.
      wa_fmap-dyfldname = 'MATNR'.
      APPEND wa_fmap TO it_fmap.

      PERFORM f_fieldinfo_get USING 'ZMMT0102'
                                    'NR_FASE'
                           CHANGING wa_field_tab.
      wa_field_tab-tabname   = 'IT_F4_NFASE'.
      wa_field_tab-position  = 1.
      wa_field_tab-offset    = 0.
      APPEND wa_field_tab  TO it_field_tab.

      PERFORM f_fieldinfo_get USING 'ZMMT0102'
                                    'CATEGORIA'
                           CHANGING wa_field_tab.
      wa_field_tab-tabname   = 'IT_F4_NFASE'.
      wa_field_tab-position  = 2.
      wa_field_tab-offset    = 40.
      wa_field_tab-reptext   = 'Categoria'.
      APPEND wa_field_tab  TO it_field_tab.

      PERFORM f_fieldinfo_get USING 'ZMMT0102'
                                    'MATNR'
                           CHANGING wa_field_tab.
      wa_field_tab-tabname   = 'IT_F4_NFASE'.
      wa_field_tab-position  = 3.
      wa_field_tab-offset    = 44.
      wa_field_tab-reptext   = 'Material'.
      APPEND wa_field_tab  TO it_field_tab.

      PERFORM f_fieldinfo_get USING 'ZMMT0102'
                                    'MATNR'
                           CHANGING wa_field_tab.
      wa_field_tab-tabname   = 'IT_F4_NFASE'.
      wa_field_tab-fieldname = 'MENGE'.
      wa_field_tab-position  = 4.
      wa_field_tab-offset    = 80.
      wa_field_tab-reptext   = 'Quant.Entrada'.
      APPEND wa_field_tab  TO it_field_tab.

      PERFORM f_fieldinfo_get USING 'ZSDT0102'
                                    'MATNR'
                           CHANGING wa_field_tab.
      wa_field_tab-tabname   = 'IT_F4_NFASE'.
      wa_field_tab-fieldname = 'LFIMG'.
      wa_field_tab-position  = 5.
      wa_field_tab-offset    = 116.
      wa_field_tab-reptext   = 'Quant.Saida'.
      APPEND wa_field_tab  TO it_field_tab.

      PERFORM f_fieldinfo_get USING 'ZMMT0102'
                                    'MATNR'
                           CHANGING wa_field_tab.
      wa_field_tab-tabname   = 'IT_F4_NFASE'.
      wa_field_tab-fieldname = 'SDO_FASE'.
      wa_field_tab-position  = 6.
      wa_field_tab-offset    = 152.
      wa_field_tab-reptext   = 'Saldo Fase'.
      APPEND wa_field_tab  TO it_field_tab.
*-CS2019001891 - JT - 03.02.2021 - fim
*
      READ TABLE it_nlote_5320 INTO wa_nlote_5320 INDEX es_row_no-row_id.
      IF sy-subrc IS INITIAL AND wa_nlote_5320-edit EQ abap_true.

        SELECT * FROM zmmt0102  APPENDING TABLE it_zmmt0102
         WHERE charg EQ wa_nlote_5320-charg
           AND  matnr EQ vg_nlote_matnr.

        LOOP AT it_zmmt0102 INTO wa_zmmt0102.

          CLEAR l_lfimg.

*-CS2019001891 - JT - 03.02.2021 - inicio
          SELECT lfimg
            INTO l_lfimg
            FROM zsdt0134
              UP TO 1 ROWS
           WHERE charg   = wa_zmmt0102-charg
             AND nr_fase = wa_zmmt0102-nr_fase.
          ENDSELECT.
*-CS2019001891 - JT - 03.02.2021 - fim

          wa_f4_nfase-categoria   = wa_zmmt0102-categoria.
          wa_f4_nfase-matnr       = wa_zmmt0102-matnr.
          wa_f4_nfase-nr_fase     = wa_zmmt0102-nr_fase.
*-CS2019001891 - JT - 03.02.2021 - inicio
          wa_f4_nfase-menge       = wa_zmmt0102-menge.
          wa_f4_nfase-lfimg       = l_lfimg.
          l_saldo_fase            = wa_zmmt0102-menge - l_lfimg.
          wa_f4_nfase-sdo_fase    = l_saldo_fase.
*-CS2019001891 - JT - 03.02.2021 - fim

          APPEND wa_f4_nfase TO it_f4_nfase.

          CLEAR: wa_zmmt0102, wa_f4_nfase.
        ENDLOOP.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'NR_FASE'
            window_title    = 'Lista de Fase'(002)
            value_org       = 'S'
          TABLES
            value_tab       = it_f4_nfase
            return_tab      = it_ret_5320
            dynpfld_mapping = it_fmap
            field_tab       = it_field_tab
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

        IF sy-subrc = 0.
          ASSIGN er_event_data->m_data->* TO <itab>.
          READ TABLE it_ret_5320 INTO wa_ret INDEX 1.
          wa_modi-row_id   = es_row_no-row_id.
          wa_modi-fieldname = 'NR_FASE'.
          wa_modi-value     = wa_ret-fieldval.
          APPEND wa_modi TO <itab>.
        ENDIF.

        er_event_data->m_event_handled = 'X'."(to inform grid that f4 was handled manually)

      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_5320  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5320 OUTPUT.

  PERFORM seleciona_carga_5320.
  PERFORM completa_carga_5320.
  PERFORM alv_tela_produtos_5320.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5320  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5320 INPUT.

  CLEAR: it_ordem_5320, it_nlote_5320.

  CASE sy-ucomm.
    WHEN 'LEG'.
      CALL SCREEN 6001 STARTING AT 5 5 ENDING AT 80 20.
    WHEN OTHERS.
  ENDCASE.

  CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
    EXPORTING
      chave = vg_carga_5320-nro_cg.



ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ALV_TELA PRODUTOS
*&---------------------------------------------------------------------*
FORM alv_tela_produtos_5320.

  IF g_custom_container_5320 IS INITIAL.

    CREATE OBJECT g_custom_container_5320
      EXPORTING
        container_name              = 'CONTAINER5320'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT dg_splitter_1_5320
      EXPORTING
        parent  = g_custom_container_5320
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1_5320->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1_5320.

    CALL METHOD dg_splitter_1_5320->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_2_5320.

    CREATE OBJECT dg_splitter_2_5320
      EXPORTING
        parent  = dg_parent_1_5320
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2_5320->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_3_5320.

    CALL METHOD dg_splitter_2_5320->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_4_5320.

    CREATE OBJECT dg_splitter_3_5320
      EXPORTING
        parent  = dg_parent_2_5320
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_3_5320->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_5_5320.

    CALL METHOD dg_splitter_3_5320->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_6_5320.

    CALL METHOD dg_splitter_2_5320->set_column_mode
      EXPORTING
        mode = dg_splitter_2_5320->mode_relative.

    CALL METHOD dg_splitter_2_5320->set_column_width
      EXPORTING
        id    = 1
        width = 100.

    CALL METHOD dg_splitter_2_5320->set_column_width
      EXPORTING
        id    = 2
        width = 0.

    CALL METHOD dg_splitter_3_5320->set_column_mode
      EXPORTING
        mode = dg_splitter_3_5320->mode_relative.

    CALL METHOD dg_splitter_3_5320->set_column_width
      EXPORTING
        id    = 1
        width = 50.

    CALL METHOD dg_splitter_3_5320->set_column_width
      EXPORTING
        id    = 2
        width = 50.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_carga_5320 USING:
          01 'ICONE'                ''             ' '  ' '  'X'  ' '   'X'   ' '   ' '   ' '   'Status',
          02 'NRO_CG'               'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Carga',
          03 'COD_TRANSPORTADORA'   'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Transp.',
          04 'DESC_TRANSP'          'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nome Transp.',
          05 'PLACA_CAV'            'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Plc. Cavalo',
          06 'PLACA_CAR1'           'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Plc. Car. I',
          07 'PLACA_CAR2'           'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Plc. Car. II',
          08 'PLACA_CAR3'           'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Plc. Dolly',
          09 'DT_ENTREGA'           'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Prev. Dt. Ent.',
          10 'INCO1'                ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Tp. Frt.',
          11 'DATA_ATUAL'           'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Dt. Criação Carga',
*         User Story 153510 - MMSILVA
          12 'DESC_STATUS_ICONE'    ''             ' '  ' '  ' '  ' '   ''    ' '   ' '   ' '   'Desc. Status Icone'.
*         User Story 153510 - MMSILVA

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_ordem_5320 USING:
       01 'NRO_CG'               ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nr. Carga',
       02 'VBELN'                ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Ordem Venda',
       03 'POSNR'                ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Item',
       04 'NR_ROT'               ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Roteiro',
       05 'MATNR'                'MARA'         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Material',
       06 'MAKTX'                'MAKTX'        ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Desc. Material',
       07 'WERKS'                ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Centro F.',
       08 'QTD_VINC'             ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd.',
       09 'QTD_VINC_LT'          ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd. c/ Lote',
       10 'UM'                   ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'U.M.'.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_nlote_5320 USING:
          01 'NRO_CG'               ''             ' '      ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nr. Carga',
          02 'VBELN'                ''             ' '      ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Ordem Venda',
          03 'POSNR'                ''             ' '      ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Item',
          04 'CHARG'                'ZSDT0134'     'C310'   ' '  ' '  ' '   'X'   ' '   ' '   'X'   'Lote',
          05 'CATEGORIA'            ''             ' '      ' '  ' '  ' '   'X'   '  '  ' '   ' '   'Categoria',
          06 'NR_FASE'              'ZSDT0134'     'C310'   ' '  ' '  ' '   'X'   '  '  ' '   'X'   'Fase',
          07 'EBELN'                ''             ' '      ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Pedido',
          08 'LFIMG'                'ZSDT0134'     ' '      'X'  ' '  ' '   'X'   ' '   'X'   ' '   'Quantidade',
          09 'BRGEW'                'ZSDT0134'     ' '      'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Peso Unit.',           "CS2017002830 - 12.12.2017
          10 'PESO_LIQ_BRT'         'ZSDT0134'     ' '      ' '  ' '  ' '   'X'   ' '   'X'   ' '   'Peso Bruto/Liq.'.      "CS2017002830 - 12.12.2017

    gs_layout_5320_alv1-sel_mode   = 'A'.
    gs_layout_5320_alv1-stylefname = 'CELLSTYLES'.
    gs_layout_5320_alv1-cwidth_opt = 'X'.
    gs_layout_5320_alv1-info_fname = 'COR'.
    gs_layout_5320_alv1-smalltitle = 'X'.
    gs_layout_5320_alv1-grid_title = 'Cargas'.
    gs_layout_5320_alv3-sel_mode   = 'A'.
    gs_layout_5320_alv3-stylefname = 'CELLSTYLES'.
    gs_layout_5320_alv3-cwidth_opt = 'X'.
    gs_layout_5320_alv3-info_fname = 'COR'.
    gs_layout_5320_alv3-smalltitle = 'X'.
    gs_layout_5320_alv3-grid_title = 'Ordens da Carga'.
    gs_layout_5320_alv4-sel_mode   = 'A'.
    gs_layout_5320_alv4-stylefname = 'CELLSTYLES'.
    gs_layout_5320_alv4-cwidth_opt = 'X'.
    gs_layout_5320_alv4-info_fname = 'COR'.
    gs_layout_5320_alv4-smalltitle = 'X'.
    gs_layout_5320_alv4-grid_title = 'Lotes de Produto'.

    PERFORM sort USING 'NRO_CG' CHANGING it_sort_5320.
    PERFORM excluir_botoes_5320 CHANGING it_exclude_nlote_5320.
    PERFORM excluir_botoes_5320 CHANGING it_exclude_fase_5320.

    CREATE OBJECT ctl_alv1_5320
      EXPORTING
        i_parent = dg_parent_3_5320.           "ALV Carga

    CREATE OBJECT ctl_alv3_5320
      EXPORTING
        i_parent = dg_parent_5_5320.           "ALV OV

    CREATE OBJECT ctl_alv4_5320
      EXPORTING
        i_parent = dg_parent_6_5320.           "ALV Lote de Produto

    PERFORM registrar_f4_5320.

    SET HANDLER:
      lcl_event_handler_5320=>toolbar_5320 FOR ctl_alv1_5320,
      lcl_event_handler_5320=>user_command_5320 FOR ctl_alv1_5320,
      lcl_event_handler_5320=>on_double_click_5320 FOR ctl_alv1_5320.

    CALL METHOD ctl_alv1_5320->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_5320_alv1
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_carga_5320
        it_outtab       = it_carga_5320
        it_sort         = it_sort_5320.

    SET HANDLER:
          lcl_event_handler_5320=>on_double_click_ordem_5320 FOR ctl_alv3_5320,
          lcl_event_handler_5320=>user_command_5320 FOR ctl_alv3_5320,
          lcl_event_handler_5320=>toolbar_ordem_5320 FOR ctl_alv3_5320.

    CALL METHOD ctl_alv3_5320->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_5320_alv3
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_ordem_5320
        it_outtab       = it_ordem_5320.
    "IT_SORT         = IT_SORT_5320.

    SET HANDLER:
      lcl_event_handler_5320=>toolbar_nlote_5320 FOR ctl_alv4_5320,
      lcl_event_handler_5320=>user_command_5320 FOR ctl_alv4_5320,
      lcl_event_handler_5320=>data_changed_finished_5320 FOR ctl_alv4_5320,
      lcl_event_handler_5320=>on_f4_5320 FOR ctl_alv4_5320.

    CALL METHOD ctl_alv4_5320->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_5320_alv4
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_nlote_5320
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_nlote_5320
        it_outtab            = it_nlote_5320.

    CALL METHOD ctl_alv4_5320->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

  ELSE.

    CALL METHOD ctl_alv1_5320->refresh_table_display
      EXPORTING
        is_stable = _stable.

    CALL METHOD ctl_alv3_5320->refresh_table_display
      EXPORTING
        is_stable = _stable.

    CALL METHOD ctl_alv4_5320->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_CARGA_5320
*&---------------------------------------------------------------------*
FORM seleciona_carga_5320 .

  IF p_corplt IS NOT INITIAL.

    "Se Corporativo veio do filtro Lote
*    SELECT *
*    FROM ZSDT0133
*    INTO CORRESPONDING FIELDS OF TABLE IT_CARGA_5320
*    WHERE DATA_ATUAL IN C_DATAA
*    AND EXISTS ( SELECT *
*                         FROM ZSDT0129
*                         INNER JOIN ZSDT0131 ON ZSDT0129~NRO_LOTE EQ ZSDT0131~NRO_LOTE
*                         WHERE ZSDT0129~NRO_CG EQ ZSDT0133~NRO_CG
*                           AND ZSDT0129~INCO1  IN C_INCO1
*                           AND ZSDT0129~STATUS NE 'X'
*                           AND ZSDT0131~VKORG  IN C_VKORG
*                           AND ZSDT0131~SPART  EQ C_SPART
*                           AND ZSDT0131~KUNNR  IN C_KUNNR
*                           AND ZSDT0131~STATUS NE 'X' )
*                           AND STATUS GE 4.


*****SELECIOANDO INFORMAÇÕES DA CARGA.
    SELECT *
    FROM zsdt0133
    INTO CORRESPONDING FIELDS OF TABLE it_carga_5320
    WHERE data_atual IN c_dataa
      AND status GE 4.

*-CS2019001891 - JT - 04.02.2021 - inicio
    DELETE it_carga_5320 WHERE status = abap_true.
*-CS2019001891 - JT - 04.02.2021 - fim

    IF it_carga_5320 IS NOT INITIAL.
*
      FREE: t_carga.
      SELECT *
      FROM zsdt0129
        INTO TABLE t_carga
        FOR ALL ENTRIES IN it_carga_5320
        WHERE nro_cg EQ it_carga_5320-nro_cg
          AND inco1  IN c_inco1
          AND status NE 'X'.
*
*      SELECT *
*      FROM ZSDT0129 AS A
*        INNER JOIN ZSDT0131 AS B ON B~NRO_LOTE EQ A~NRO_LOTE
*        INTO CORRESPONDING FIELDS OF TABLE T_CARGA
*        FOR ALL ENTRIES IN IT_CARGA_5320
*        WHERE A~NRO_CG EQ IT_CARGA_5320-NRO_CG
*          AND A~INCO1  IN C_INCO1
*          AND A~STATUS NE 'X'
*          AND B~VKORG  IN C_VKORG
*          AND B~SPART  EQ C_SPART
*          AND B~KUNNR  IN C_KUNNR
*          AND B~STATUS NE 'X'.

      IF t_carga IS NOT INITIAL.
        SELECT *
        FROM zsdt0131
        INTO TABLE @DATA(t_zsdt0131)
          FOR ALL ENTRIES IN @t_carga
          WHERE nro_lote EQ @t_carga-nro_lote
            AND vkorg  IN @c_vkorg
            AND spart  EQ @c_spart
            AND kunnr  IN @c_kunnr
            AND status NE 'X'.
      ENDIF.
      LOOP AT it_carga_5320 ASSIGNING FIELD-SYMBOL(<w_carga_5320>).
        LOOP AT t_carga ASSIGNING FIELD-SYMBOL(<w_zsdt0129>) WHERE nro_cg EQ <w_carga_5320>-nro_cg
                                                                  AND inco1  IN c_inco1
                                                                  AND status NE abap_true.
          <w_carga_5320>-placa_cav   = <w_zsdt0129>-placa_cav.
          <w_carga_5320>-placa_car1  = <w_zsdt0129>-placa_car1.
          <w_carga_5320>-placa_car2  = <w_zsdt0129>-placa_car2.
          <w_carga_5320>-placa_car3  = <w_zsdt0129>-placa_car3.
          <w_carga_5320>-motorista   = <w_zsdt0129>-motorista.
          <w_carga_5320>-dt_entrega  = <w_zsdt0129>-dt_entrega.
        ENDLOOP.
      ENDLOOP.
    ENDIF.


  ELSEIF p_corpcg IS NOT INITIAL OR p_corppt IS NOT INITIAL.

    "Se corporativo veio do filtro de Carga ou Produtos
*    SELECT *
*      FROM ZSDT0133 AS A
*      INTO CORRESPONDING FIELDS OF TABLE IT_CARGA_5320
*      WHERE DATA_ATUAL IN C_DATAB
*        AND NRO_CG     IN C_NUMCG
*        AND EXISTS ( SELECT *
*                    FROM ZSDT0129 AS B
*                    INNER JOIN ZSDT0131 AS C ON C~NRO_LOTE EQ B~NRO_LOTE
*                    WHERE B~NRO_CG EQ A~NRO_CG
*                    AND B~INCO1  IN C_INCO1
*                    AND B~STATUS NE 'X'
*                    AND C~VKORG  IN C_VKORG
*                    AND C~SPART  EQ C_SPART
*                    AND C~KUNNR  IN C_KUNNR
*                    AND C~STATUS NE 'X' )
*                    AND STATUS GE 4.

*****Selecioando informações da carga.
    SELECT *
    FROM zsdt0133
    INTO CORRESPONDING FIELDS OF TABLE it_carga_5320
    WHERE data_atual IN c_datab
      AND nro_cg     IN c_numcg
      AND status GE 4.

*-CS2019001891 - JT - 04.02.2021 - inicio
    DELETE it_carga_5320 WHERE status = abap_true.
*-CS2019001891 - JT - 04.02.2021 - fim

    IF it_carga_5320 IS NOT INITIAL.
*
      FREE: t_carga.
      SELECT *
      FROM zsdt0129
        INTO TABLE t_carga
        FOR ALL ENTRIES IN it_carga_5320
        WHERE nro_cg EQ it_carga_5320-nro_cg
          AND inco1  IN c_inco1
          AND status NE 'X'.
*
*      SELECT *
*      FROM ZSDT0129 AS A
*        INNER JOIN ZSDT0131 AS B ON B~NRO_LOTE EQ A~NRO_LOTE
*        INTO CORRESPONDING FIELDS OF TABLE T_CARGA
*        FOR ALL ENTRIES IN IT_CARGA_5320
*        WHERE A~NRO_CG EQ IT_CARGA_5320-NRO_CG
*          AND A~INCO1  IN C_INCO1
*          AND A~STATUS NE 'X'
*          AND B~VKORG  IN C_VKORG
*          AND B~SPART  EQ C_SPART
*          AND B~KUNNR  IN C_KUNNR
*          AND B~STATUS NE 'X'.

      IF t_carga IS NOT INITIAL.
        FREE: t_zsdt0131.
        SELECT *
        FROM zsdt0131
        INTO TABLE t_zsdt0131
          FOR ALL ENTRIES IN t_carga
          WHERE nro_lote EQ t_carga-nro_lote
            AND vkorg  IN c_vkorg
            AND spart  EQ c_spart
            AND kunnr  IN c_kunnr
            AND status NE 'X'.
      ENDIF.
      LOOP AT it_carga_5320 ASSIGNING <w_carga_5320>.
        LOOP AT t_carga ASSIGNING <w_zsdt0129> WHERE nro_cg EQ <w_carga_5320>-nro_cg
                                                                  AND inco1  IN c_inco1
                                                                  AND status NE abap_true.
          <w_carga_5320>-placa_cav   = <w_zsdt0129>-placa_cav.
          <w_carga_5320>-placa_car1  = <w_zsdt0129>-placa_car1.
          <w_carga_5320>-placa_car2  = <w_zsdt0129>-placa_car2.
          <w_carga_5320>-placa_car3  = <w_zsdt0129>-placa_car3.
          <w_carga_5320>-motorista   = <w_zsdt0129>-motorista.
          <w_carga_5320>-dt_entrega  = <w_zsdt0129>-dt_entrega.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COMPLETA_CARGA_5320
*&---------------------------------------------------------------------*
FORM completa_carga_5320 .

  DATA: wa_carga_5320 TYPE ty_carga_5320,
        it_zsdt0129   TYPE STANDARD TABLE OF zsdt0129,
        wa_zsdt0129   TYPE zsdt0129,
        it_lfa1       TYPE STANDARD TABLE OF lfa1,
        wa_lfa1       TYPE lfa1,
        vl_cont       TYPE i.

  SELECT *
    FROM zsdt0129
    INTO TABLE it_zsdt0129
    FOR ALL ENTRIES IN it_carga_5320
    WHERE nro_cg EQ it_carga_5320-nro_cg
      AND status NE 'X'.

  SELECT *
    FROM lfa1
    INTO TABLE it_lfa1
    FOR ALL ENTRIES IN it_carga_5320
    WHERE lifnr EQ it_carga_5320-cod_transportadora.

  LOOP AT it_carga_5320 INTO wa_carga_5320.
    vl_cont = vl_cont + 1.

*    User Story 153510 // MMSILVA

*    wa_carga_5320-icone =
*    SWITCH #( wa_carga_5320-status WHEN '4' THEN '@4A@'
*                                   WHEN '5' THEN '@96@'
*                                   WHEN '6' THEN '@0Q@'
*            ).


    IF wa_carga_5320-status EQ 4.
      wa_carga_5320-icone = '@4A@'.
      wa_carga_5320-desc_status_icone = 'Frete Contratado'.
    ELSEIF wa_carga_5320-status EQ 5.
      wa_carga_5320-icone = '@96@'.
      wa_carga_5320-desc_status_icone = 'Embarque Autorizado'.
    ELSEIF wa_carga_5320-status EQ 6.
      wa_carga_5320-icone = '@0Q@'.
      wa_carga_5320-desc_status_icone = 'Romaneio Gerado'.
    ENDIF.

*   User Story 153510 // MMSILVA

    READ TABLE it_zsdt0129 INTO wa_zsdt0129 WITH KEY nro_cg = wa_carga_5320-nro_cg.
    IF sy-subrc IS INITIAL.

      IF wa_carga_5320-status EQ '4'.
        SELECT COUNT(*)
          FROM zsdt0131 AS a
          INNER JOIN zsdt0132 AS c ON c~nr_rot EQ a~cod_loc_emb
              WHERE a~nro_lote EQ wa_zsdt0129-nro_lote
                AND c~armazem  EQ abap_true.
        IF sy-subrc IS NOT INITIAL.
          wa_carga_5320-icone = 'XXXX'.
        ENDIF.
      ENDIF.

      wa_carga_5320-placa_cav = wa_zsdt0129-placa_cav.
      wa_carga_5320-placa_car1 = wa_zsdt0129-placa_car1.
      wa_carga_5320-placa_car2 = wa_zsdt0129-placa_car2.
      wa_carga_5320-placa_car3 = wa_zsdt0129-placa_car3.
      wa_carga_5320-dt_entrega = wa_zsdt0129-dt_entrega.
      wa_carga_5320-inco1 = wa_zsdt0129-inco1.
    ENDIF.

    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_carga_5320-cod_transportadora.
    IF sy-subrc IS INITIAL.
      wa_carga_5320-desc_transp = wa_lfa1-name1.
    ENDIF.

    "US #154513 - MMSILVA - 28.02.2025 - Inicio
    SELECT SINGLE nro_nf_prod FROM zsdt0001 WHERE nro_cg EQ @wa_carga_5320-nro_cg AND placa_cav EQ @wa_carga_5320-placa_cav INTO @DATA(wa_zsdt0001).

    IF sy-subrc IS INITIAL.
      SELECT SINGLE docsta FROM j_1bnfe_active WHERE docnum EQ @wa_zsdt0001 INTO @DATA(wa_j_1bnfe_active).
      IF sy-subrc IS INITIAL.
        IF ( wa_j_1bnfe_active EQ '1').

          wa_carga_5320-icone = '@5Y@'.
          wa_carga_5320-desc_status_icone = 'Nota Fiscal Autorizada'.
        ENDIF.
      ENDIF.
    ENDIF.
    "US #154513 - MMSILVA - 28.02.2025 - Fim

    MODIFY it_carga_5320 FROM wa_carga_5320 INDEX vl_cont.

  ENDLOOP.

  DELETE it_carga_5320 WHERE icone EQ 'XXXX'.

  CLEAR: vl_cont.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_SOL_CLICK_5320
*&---------------------------------------------------------------------*
FORM alv_carga_click_5320  USING    p_e_row_index.

  DATA: it_ordem_aux_5320 TYPE STANDARD TABLE OF ty_ordem_5320,
        it_makt           TYPE STANDARD TABLE OF makt,
        wa_makt           TYPE makt,
        wa_carga_5320     TYPE ty_carga_5320,
        wa_ordem_5320     TYPE ty_ordem_5320,
        wa_ordem_aux_5320 TYPE ty_ordem_5320,
        vl_cont           TYPE i,
        vl_tabix          TYPE sy-tabix,
        vl_qtd_vinc       TYPE zsdt0131-qtd_vinc,
        it_zsdt0082       TYPE TABLE OF zsdt0082,
        wa_zsdt0082       TYPE zsdt0082,
        it_zsdt0129       TYPE STANDARD TABLE OF zsdt0129,
        wa_zsdt0129       TYPE zsdt0129,
        it_zsdt0134       TYPE STANDARD TABLE OF zsdt0134,
        wa_zsdt0134       TYPE zsdt0134.

  CLEAR: it_ordem_5320.

  LOOP AT it_carga_5320 INTO wa_carga_5320 WHERE cor IS NOT INITIAL.

    vl_cont = sy-tabix.

    CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
      EXPORTING
        chave = wa_carga_5320-nro_cg.

    CLEAR: wa_carga_5320-cor.
    MODIFY it_carga_5320 FROM wa_carga_5320 INDEX vl_cont.

  ENDLOOP.

  CLEAR: vl_cont, wa_carga_5320, vg_carga_5320.

  READ TABLE it_carga_5320 INTO wa_carga_5320 INDEX p_e_row_index.
  IF sy-subrc IS INITIAL.

    CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
      EXPORTING
        chave          = wa_carga_5320-nro_cg
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    vg_carga_5320 = wa_carga_5320.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    ELSE.

      SELECT *
        FROM zsdt0131
        INTO CORRESPONDING FIELDS OF TABLE it_ordem_5320
        WHERE status NE 'X'
          AND EXISTS ( SELECT *
                         FROM zsdt0129
                        WHERE nro_cg   EQ wa_carga_5320-nro_cg
                          AND nro_lote EQ zsdt0131~nro_lote
                          AND status   NE 'X' ).

      IF it_ordem_5320 IS NOT INITIAL.

        SELECT *
          FROM zsdt0134
          INTO TABLE it_zsdt0134
          FOR ALL ENTRIES IN it_ordem_5320
          WHERE status NE 'X'
            AND vbeln  EQ it_ordem_5320-vbeln
            AND posnr  EQ it_ordem_5320-posnr
            AND nro_cg EQ wa_carga_5320-nro_cg.

        SELECT *
          FROM makt
          INTO TABLE it_makt
          FOR ALL ENTRIES IN it_ordem_5320
          WHERE matnr EQ it_ordem_5320-matnr.

        SELECT *
          FROM zsdt0129
          INTO TABLE it_zsdt0129
          FOR ALL ENTRIES IN it_ordem_5320
          WHERE nro_lote EQ it_ordem_5320-nro_lote
            AND status   NE 'X'.

*-CS2019001891 - JT - 04.02.2021 - inicio
        SELECT *
          FROM zsdt0082
          INTO TABLE it_zsdt0082
          FOR ALL ENTRIES IN it_ordem_5320
          WHERE nro_sol EQ it_ordem_5320-nro_sol
            AND status  EQ '1'.

        SORT it_zsdt0082 BY nro_sol.

        LOOP AT it_ordem_5320 INTO wa_ordem_5320.
          vl_tabix = sy-tabix.

          READ TABLE it_zsdt0082 INTO wa_zsdt0082 WITH KEY nro_sol = wa_ordem_5320-nro_sol
                                 BINARY SEARCH.
          IF sy-subrc = 0.
            wa_ordem_5320-nr_rot    = wa_zsdt0082-nr_rot.
            MODIFY it_ordem_5320 FROM wa_ordem_5320 INDEX vl_tabix.
          ENDIF.
        ENDLOOP.
*-CS2019001891 - JT - 04.02.2021 - fim

      ENDIF.

      it_ordem_aux_5320 = it_ordem_5320.

*-CS2019001891 - JT - 04.02.2021 - inicio
*     SORT it_ordem_5320 BY vbeln posnr ASCENDING.
*     DELETE ADJACENT DUPLICATES FROM it_ordem_5320 COMPARING vbeln posnr.
      SORT it_ordem_5320 BY vbeln posnr nr_rot ASCENDING.
      DELETE ADJACENT DUPLICATES FROM it_ordem_5320 COMPARING vbeln posnr nr_rot.
*-CS2019001891 - JT - 04.02.2021 - fim

      LOOP AT it_ordem_5320 INTO wa_ordem_5320.

        vl_cont = vl_cont + 1.

        IF wa_ordem_5320-UM = 'BIG'.
          wa_ordem_5320-UM = 'BAG'.
        ENDIF.

        READ TABLE it_zsdt0129 INTO wa_zsdt0129 WITH KEY nro_lote = wa_ordem_5320-nro_lote.
        IF sy-subrc IS INITIAL.
          wa_ordem_5320-nro_cg = wa_zsdt0129-nro_cg.
        ENDIF.

        READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_ordem_5320-matnr.
        IF sy-subrc IS INITIAL.
          wa_ordem_5320-maktx = wa_makt-maktx.
        ENDIF.

        LOOP AT it_ordem_aux_5320 INTO wa_ordem_aux_5320 WHERE vbeln  EQ wa_ordem_5320-vbeln
                                                           AND posnr  EQ wa_ordem_5320-posnr
                                                           AND nr_rot EQ wa_ordem_5320-nr_rot.
          vl_qtd_vinc = vl_qtd_vinc + wa_ordem_aux_5320-qtd_vinc.
        ENDLOOP.

        LOOP AT it_zsdt0134 INTO wa_zsdt0134 WHERE vbeln EQ  wa_ordem_5320-vbeln
                                               AND posnr EQ  wa_ordem_5320-posnr
                                               AND nro_cg EQ wa_ordem_5320-nro_cg.
          wa_ordem_5320-qtd_vinc_lt = wa_ordem_5320-qtd_vinc_lt + wa_zsdt0134-lfimg.
        ENDLOOP.

        wa_ordem_5320-qtd_vinc = vl_qtd_vinc.

        MODIFY it_ordem_5320 FROM wa_ordem_5320 INDEX vl_cont.
        CLEAR: vl_qtd_vinc.

      ENDLOOP.

      wa_carga_5320-cor = 'C300'.
      MODIFY it_carga_5320 FROM wa_carga_5320 INDEX p_e_row_index.

      CLEAR: vg_carga_click_status.
      vg_carga_click_status = wa_carga_5320-status.

    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_ORDEM_CLICK_5320
*&---------------------------------------------------------------------*
FORM alv_ordem_click_5320 USING p_e_row_index.

  DATA: wa_ordem_5320 TYPE ty_ordem_5320,
        wa_nlote_5320 TYPE ty_nlote_5320.

  LOOP AT it_ordem_5320 INTO wa_ordem_5320.
    CLEAR: wa_ordem_5320-cor.
    MODIFY it_ordem_5320 FROM wa_ordem_5320 INDEX sy-tabix.
  ENDLOOP.

  READ TABLE it_ordem_5320 INTO wa_ordem_5320 INDEX p_e_row_index.
  IF sy-subrc IS INITIAL.

    SELECT *
      FROM zsdt0134
      INTO CORRESPONDING FIELDS OF TABLE it_nlote_5320
      WHERE nro_cg EQ wa_ordem_5320-nro_cg
        AND vbeln  EQ wa_ordem_5320-vbeln
        AND posnr  EQ wa_ordem_5320-posnr
*-CS2019001891 - JT - 04.02.2021 - inicio
        AND nr_rot EQ wa_ordem_5320-nr_rot
*-CS2019001891 - JT - 04.02.2021 - fim
        AND status NE 'X'.

*-CS2019001891 - JT - 04.02.2021 - inicio
    IF sy-subrc <> 0.
      SELECT *
        FROM zsdt0134
        INTO CORRESPONDING FIELDS OF TABLE it_nlote_5320
        WHERE nro_cg EQ wa_ordem_5320-nro_cg
          AND vbeln  EQ wa_ordem_5320-vbeln
          AND posnr  EQ wa_ordem_5320-posnr
*-CS2019001891 - JT - 04.02.2021 - inicio
          AND nr_rot EQ ''
*-CS2019001891 - JT - 04.02.2021 - fim
          AND status NE 'X'.

      LOOP AT it_nlote_5320  INTO wa_nlote_5320.
        wa_nlote_5320-nr_rot    = wa_ordem_5320-nr_rot.
        MODIFY it_nlote_5320 FROM wa_nlote_5320 INDEX sy-tabix.
      ENDLOOP.
    ENDIF.
*-CS2019001891 - JT - 04.02.2021 - fim

    "CS2017002830 - 12.12.2017 - Ini
    LOOP AT it_nlote_5320 ASSIGNING FIELD-SYMBOL(<fs_nlote_5320>).

      IF <fs_nlote_5320>-brgew = 0.
        CLEAR: <fs_nlote_5320>-peso_liq_brt.
      ENDIF.

      CHECK <fs_nlote_5320>-lfimg <> 0 AND <fs_nlote_5320>-brgew <> 0.
      <fs_nlote_5320>-peso_liq_brt =  <fs_nlote_5320>-lfimg * <fs_nlote_5320>-brgew.
    ENDLOOP.
    "CS2017002830 - 12.12.2017 - Fim

    CLEAR: vg_nlote_vbeln, vg_nlote_posnr, vg_nlote_nro_cg,
           vg_nlote_matnr, vg_nlote_werks, vg_nlote_nr_rot.

    vg_nlote_vbeln  = wa_ordem_5320-vbeln.
    vg_nlote_posnr  = wa_ordem_5320-posnr.
*-CS2019001891 - JT - 04.02.2021 - inicio
    vg_nlote_nr_rot = wa_ordem_5320-nr_rot.
*-CS2019001891 - JT - 04.02.2021 - fim
    vg_nlote_nro_cg = wa_ordem_5320-nro_cg.
    vg_nlote_matnr  = wa_ordem_5320-matnr.
    vg_nlote_werks  = wa_ordem_5320-werks.

    wa_ordem_5320-cor = 'C300'.
    MODIFY it_ordem_5320 FROM wa_ordem_5320 INDEX p_e_row_index.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BLOQUEIA_LINHAS_nlote_5320
*&---------------------------------------------------------------------*
FORM bloqueia_linhas_nlote_5320 .
  DATA: vl_cont       TYPE i,
        it_celltab    TYPE lvc_t_styl,
        wa_nlote_5320 TYPE ty_nlote_5320.

  LOOP AT it_nlote_5320 INTO wa_nlote_5320.
    vl_cont = vl_cont + 1.
    CLEAR: it_celltab, wa_nlote_5320-cellstyles.
    "REFRESH IT_CELLTAB.
    PERFORM fill_celltab_5320 USING wa_nlote_5320-edit
                         CHANGING it_celltab.
    CLEAR wa_nlote_5320-cellstyles.
    INSERT LINES OF it_celltab INTO TABLE wa_nlote_5320-cellstyles.
    MODIFY it_nlote_5320 FROM wa_nlote_5320 INDEX vl_cont.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_CELLTAB_5320
*&---------------------------------------------------------------------*
FORM fill_celltab_5320  USING    p_edit
                        CHANGING p_it_celltab TYPE lvc_t_styl.

  DATA: wa_celltab TYPE lvc_s_styl,
        status     TYPE raw4,
        v_matkl    TYPE mara-matkl.

  "CS2017002830 - 12.12.2017 - Ini
  wa_celltab-fieldname = 'BRGEW'.
  wa_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
  IF ( p_edit EQ abap_true ) AND ( vg_nlote_matnr IS NOT INITIAL ).
    SELECT SINGLE matkl
      FROM mara INTO @DATA(_v_matkl)
     WHERE matnr = @vg_nlote_matnr.
*   IF ( sy-subrc = 0 ) AND (  _v_matkl = '700230' OR _v_matkl = '700240' ).
*----> CS1059298 / IR124784 ---->
    IF ( sy-subrc = 0 ).
      SELECT SINGLE valfrom INTO v_matkl
        FROM setleaf
        WHERE setname = 'ZSDR0060_LOCK_CAMPO'
          AND valfrom EQ _v_matkl.
      IF sy-subrc EQ 0.
*<----> CS1059298 / IR124784 ----
        wa_celltab-style   = cl_gui_alv_grid=>mc_style_enabled.
      ENDIF.
    ENDIF.
  ENDIF.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  CLEAR: wa_celltab.
  "CS2017002830 - 12.12.2017 - Fim


  IF p_edit EQ abap_true.
    status = cl_gui_alv_grid=>mc_style_enabled.
  ELSE.
    status = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  wa_celltab-fieldname = 'LFIMG'.
  wa_celltab-style = status.

  INSERT wa_celltab INTO TABLE p_it_celltab.


ENDFORM.                               " FILL_CELLTAB

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_BOTOES_5320
*&---------------------------------------------------------------------*
FORM excluir_botoes_5320 CHANGING p_it_exclude TYPE ui_functions.

  DATA: ls_exclude     TYPE ui_func.

  ls_exclude = cl_gui_alv_grid=>mc_fc_excl_all.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  REGISTRAR_F4_5130
*&---------------------------------------------------------------------*
FORM registrar_f4_5320 .

  DATA: gs_f4      TYPE lvc_s_f4,
        gt_f4_5320 TYPE lvc_t_f4.

  gs_f4-fieldname  = 'CHARG'.
  gs_f4-register   = 'X'.
  gs_f4-getbefore  = space.
  gs_f4-chngeafter = space.
  APPEND gs_f4 TO gt_f4_5320.

  gs_f4-fieldname  = 'NR_FASE'.
  gs_f4-register   = 'X'.
  gs_f4-getbefore  = space.
  gs_f4-chngeafter = space.
  APPEND gs_f4 TO gt_f4_5320.

  CALL METHOD ctl_alv4_5320->register_f4_for_fields
    EXPORTING
      it_f4 = gt_f4_5320.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SALVA_ROMANEIOS
*&---------------------------------------------------------------------*
FORM salva_romaneios CHANGING vl_check_rom.

  DATA: it_zsdt0040             TYPE STANDARD TABLE OF zsdt0040,
        it_zsdt0041             TYPE STANDARD TABLE OF zsdt0041,
        it_zsdt0090             TYPE STANDARD TABLE OF zsdt0090,
        it_zsdt0136             TYPE STANDARD TABLE OF zsdt0136,
        it_zsdt0129             TYPE STANDARD TABLE OF zsdt0129,
        it_zsdt0082             TYPE STANDARD TABLE OF zsdt0082,
        it_zsdt0001             TYPE STANDARD TABLE OF zsdt0001,
        it_zsdt0001_gravou      TYPE STANDARD TABLE OF zsdt0001,
        it_zsdt0134             TYPE STANDARD TABLE OF zsdt0134,
        it_0133                 TYPE STANDARD TABLE OF zsdt0133,
        it_zsdt0134_aux         TYPE STANDARD TABLE OF zsdt0134,
        it_vbpa                 TYPE STANDARD TABLE OF vbpa,
        it_vbap                 TYPE STANDARD TABLE OF vbap,
        it_mara                 TYPE STANDARD TABLE OF mara,
        it_ordem_check_aux_5320 TYPE STANDARD TABLE OF ty_ordem_5320,
        wa_ordem_5320           TYPE ty_ordem_5320,
        wa_zsdt0041             TYPE zsdt0041,
        wa_zsdt0040             TYPE zsdt0040,
        wa_zsdt0082             TYPE zsdt0082,
        wa_zsdt0090             TYPE zsdt0090,
        wa_zsdt0001             TYPE zsdt0001,
        wa_zsdt0001_item        TYPE zsdt0001_item,
        wa_zsdt0136             TYPE zsdt0136,
        wa_zsdt0134             TYPE zsdt0134,
        wa_0133                 TYPE zsdt0133,
        wa_zsdt0129             TYPE zsdt0129,
        wa_ordem_check_aux_5320 TYPE ty_ordem_5320,
        wa_carga_5320           TYPE ty_carga_5320,
        wa_vbpa                 TYPE vbpa,
        wa_vbap                 TYPE vbap,
        wa_mara                 TYPE mara,
        chave                   TYPE char8,
        vl_brgew_tot            TYPE zsdt0001_item-brgew,
        vl_doc_simulacao        TYPE zsdt0041-doc_simulacao,
        vl_safra                TYPE zsdt0040-safra,
        vl_tabix                TYPE sy-tabix,
        vl_cont                 TYPE i,
        vl_cont2                TYPE i,
        vl_qtd_kg               TYPE zsdt0131-qtd_emkg,
        vl_gravou               TYPE char1.

  CLEAR: it_zsdt0001_gravou.

  SELECT *
    FROM zsdt0131
    INTO CORRESPONDING FIELDS OF TABLE it_ordem_check_5320
    FOR ALL ENTRIES IN it_carga_aux_5320
    WHERE status NE 'X'
      AND EXISTS ( SELECT *
                     FROM zsdt0129
                    WHERE nro_cg   EQ it_carga_aux_5320-nro_cg
                      AND nro_lote EQ zsdt0131~nro_lote
                      AND status   NE 'X' ).

*-CS2019001891 - JT - 04.02.2021 - inicio
  IF it_ordem_check_5320[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0082
      INTO TABLE it_zsdt0082
      FOR ALL ENTRIES IN it_ordem_check_5320
      WHERE nro_sol EQ it_ordem_check_5320-nro_sol
        AND status  EQ '1'.

    SORT it_zsdt0082 BY nro_sol.
  ENDIF.

  LOOP AT it_ordem_check_5320 INTO wa_ordem_5320.
    vl_tabix = sy-tabix.

    READ TABLE it_zsdt0082       INTO wa_zsdt0082 WITH KEY nro_sol = wa_ordem_5320-nro_sol
                                 BINARY SEARCH.
    IF sy-subrc = 0.
      wa_ordem_5320-nr_rot          = wa_zsdt0082-nr_rot.
      MODIFY it_ordem_check_5320 FROM wa_ordem_5320 INDEX vl_tabix.
    ENDIF.
  ENDLOOP.
*-CS2019001891 - JT - 04.02.2021 - fim

  IF it_ordem_check_5320 IS NOT INITIAL.

    it_ordem_check_aux_5320 = it_ordem_check_5320.

*-CS2019001891 - JT - 04.02.2021 - inicio
*   SORT it_ordem_check_5320 BY nro_cg vbeln ASCENDING.
*   DELETE ADJACENT DUPLICATES FROM it_ordem_check_5320 COMPARING nro_cg vbeln.
    SORT it_ordem_check_5320 BY nro_cg vbeln nr_rot ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_ordem_check_5320 COMPARING nro_cg vbeln nr_rot.
*-CS2019001891 - JT - 04.02.2021 - fim

    SELECT *
      FROM zsdt0133
      INTO TABLE it_0133
      FOR ALL ENTRIES IN it_carga_aux_5320
      WHERE  nro_cg  EQ it_carga_aux_5320-nro_cg.


    SELECT *
      FROM zsdt0041
      INTO TABLE it_zsdt0041
      FOR ALL ENTRIES IN it_ordem_check_5320
      WHERE vbeln EQ it_ordem_check_5320-vbeln.

    SELECT *
      FROM zsdt0090
      INTO TABLE it_zsdt0090
      FOR ALL ENTRIES IN it_ordem_check_5320
      WHERE vbeln EQ it_ordem_check_5320-vbeln.

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
      FOR ALL ENTRIES IN it_ordem_check_5320
      WHERE werks EQ it_ordem_check_5320-werks.

    SELECT *
      FROM zsdt0129
      INTO TABLE it_zsdt0129
      FOR ALL ENTRIES IN it_ordem_check_5320
      WHERE nro_lote EQ it_ordem_check_5320-nro_lote
        AND status   NE 'X'.

  ENDIF.

  LOOP AT it_ordem_check_5320 INTO wa_ordem_5320.

    vl_cont = vl_cont + 1.
    CLEAR: vl_qtd_kg.

    READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_ordem_5320-vbeln.
    IF sy-subrc IS NOT INITIAL.
      READ TABLE it_zsdt0090 INTO wa_zsdt0090 WITH KEY vbeln = wa_ordem_5320-vbeln.
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

    READ TABLE it_zsdt0136 WITH KEY werks = wa_ordem_5320-werks
                                    safra = vl_safra TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE s000(z_fi) WITH TEXT-125 wa_ordem_5320-werks TEXT-126 vl_safra DISPLAY LIKE 'E'.
*      MESSAGE TEXT-125 TYPE 'S' DISPLAY LIKE 'E' with SAFRA.
      vl_check_rom = abap_true.
      EXIT.
    ENDIF.

    LOOP AT it_ordem_check_aux_5320 INTO wa_ordem_check_aux_5320 WHERE nro_cg EQ wa_ordem_5320-nro_cg
                                                                   AND vbeln  EQ wa_ordem_5320-vbeln
*------------------------------------------------------------------CS2019001891 - JT - 04.02.2021 - inicio
                                                                   AND nr_rot EQ wa_ordem_5320-nr_rot.
*------------------------------------------------------------------CS2019001891 - JT - 04.02.2021 - fim
      vl_qtd_kg = wa_ordem_check_aux_5320-qtd_emkg + vl_qtd_kg.
    ENDLOOP.
    CLEAR wa_ordem_5320-qtd_emkg.
    wa_ordem_5320-qtd_emkg = vl_qtd_kg.
    MODIFY it_ordem_check_5320 FROM wa_ordem_5320 INDEX vl_cont TRANSPORTING qtd_emkg.

  ENDLOOP.

  IF vl_check_rom IS INITIAL.

    CREATE OBJECT zcl_romaneio.

    SELECT *
       FROM vbpa
       INTO TABLE it_vbpa
       FOR ALL ENTRIES IN it_ordem_check_5320
       WHERE vbeln EQ it_ordem_check_5320-vbeln
         AND ( parvw EQ 'PC' OR
               parvw EQ 'AG' ).

    LOOP AT it_ordem_check_5320 INTO wa_ordem_5320.

      CLEAR: wa_zsdt0001, vl_gravou, it_zsdt0134_aux, it_vbap, it_mara.

      wa_zsdt0001-tp_movimento = 'S'.
      wa_zsdt0001-vbeln = wa_ordem_5320-vbeln.
      wa_zsdt0001-nr_rot = wa_ordem_5320-nr_rot.
      wa_zsdt0001-dt_movimento = sy-datum.
      wa_zsdt0001-peso_liq    = wa_ordem_5320-qtd_emkg.
      wa_zsdt0001-peso_fiscal = wa_ordem_5320-qtd_emkg.

      READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_ordem_5320-vbeln.
      IF sy-subrc IS NOT INITIAL.
        READ TABLE it_zsdt0090 INTO wa_zsdt0090 WITH KEY vbelv = wa_ordem_5320-vbeln.
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

      wa_zsdt0001-nr_safra = vl_safra.
      wa_zsdt0001-bukrs = wa_ordem_5320-vkorg.
      wa_zsdt0001-branch = wa_ordem_5320-werks.

      READ TABLE it_vbpa INTO wa_vbpa WITH KEY vbeln = wa_ordem_5320-vbeln
                                               parvw = 'PC'.
      IF sy-subrc IS INITIAL.
        wa_zsdt0001-parid = wa_vbpa-lifnr.
      ENDIF.

      READ TABLE it_vbpa INTO wa_vbpa WITH KEY vbeln = wa_ordem_5320-vbeln
                                               parvw = 'AG'.
      IF sy-subrc IS INITIAL.
        wa_zsdt0001-id_cli_dest = wa_vbpa-kunnr.
      ENDIF.

      READ TABLE it_zsdt0129 INTO wa_zsdt0129 WITH KEY nro_lote = wa_ordem_5320-nro_lote.

      IF wa_zsdt0129-inco1 EQ 'FOB'.
        wa_zsdt0001-tp_frete = 'F'.
      ELSE.
        wa_zsdt0001-tp_frete = 'C'.
      ENDIF.

      wa_zsdt0001-placa_cav = wa_zsdt0129-placa_cav.
      wa_zsdt0001-placa_car1 = wa_zsdt0129-placa_car1.
      wa_zsdt0001-placa_car2 = wa_zsdt0129-placa_car2.
      wa_zsdt0001-placa_car3 = wa_zsdt0129-placa_car3.
      wa_zsdt0001-motorista = wa_zsdt0129-motorista.
      wa_zsdt0001-id_interface = '48'.
      wa_zsdt0001-nro_cg = wa_zsdt0129-nro_cg.

      READ TABLE it_0133 INTO wa_0133 WITH KEY nro_cg = wa_zsdt0129-nro_cg.


      wa_zsdt0001-agente_frete = wa_0133-cod_transportadora.

      zcl_romaneio->zif_cadastro~novo_registro( ).
      zcl_romaneio->set_cabecalho_romaneio( i_romaneio = wa_zsdt0001 ).

      "Salva CH_REF na ZSDT0134
      SELECT *
        FROM zsdt0134
        INTO TABLE it_zsdt0134_aux
        WHERE nro_cg EQ wa_zsdt0129-nro_cg
          AND vbeln  EQ wa_ordem_5320-vbeln
          AND nr_rot EQ wa_ordem_5320-nr_rot
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

      CLEAR: vl_brgew_tot. "CS2017002830 - 12.12.2017

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
          wa_zsdt0001_item-meins = wa_vbap-meins.
          wa_zsdt0001_item-matnr = wa_vbap-matnr.

          READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_vbap-matnr.
          IF sy-subrc IS INITIAL.
            IF wa_zsdt0134-brgew IS NOT INITIAL. "CS2017002830 - 12.12.2017
              wa_zsdt0001_item-brgew = wa_zsdt0001_item-lfimg * wa_zsdt0134-brgew.
              wa_zsdt0001_item-ntgew = wa_zsdt0001_item-lfimg * wa_zsdt0134-brgew.
            ELSE.
              wa_zsdt0001_item-brgew = wa_zsdt0001_item-lfimg * wa_mara-brgew.
              wa_zsdt0001_item-ntgew = wa_zsdt0001_item-lfimg * wa_mara-brgew.
            ENDIF.

            ADD wa_zsdt0001_item-brgew TO vl_brgew_tot. "CS2017002830 - 12.12.2017
          ENDIF.

        ENDIF.

        zcl_romaneio->add_item( CHANGING i_item = wa_zsdt0001_item ).

      ENDLOOP.

      "CS2017002830 - 12.12.2017 - Ini
      IF ( it_zsdt0134_aux[] IS NOT INITIAL ) AND ( vl_brgew_tot <> 0 ).
        wa_zsdt0001-peso_liq    = vl_brgew_tot.
        wa_zsdt0001-peso_fiscal = vl_brgew_tot.
        wa_zsdt0001-peso_subtotal = vl_brgew_tot. "IR096917 - 01.06.2012 - RMNI
        zcl_romaneio->set_cabecalho_romaneio( i_romaneio = wa_zsdt0001 ).
      ENDIF.
      "CS2017002830 - 12.12.2017 - Fim

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

      CLEAR: wa_0133.
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
      vl_cont2 = sy-tabix.
      READ TABLE it_zsdt0001_gravou INTO wa_zsdt0001 WITH KEY nro_cg = wa_zsdt0134-nro_cg
                                                              vbeln  = wa_zsdt0134-vbeln
                                                              nr_rot = wa_zsdt0134-nr_rot.
      IF sy-subrc IS INITIAL.
        wa_zsdt0134-ch_referencia = wa_zsdt0001-ch_referencia.
      ENDIF.
      MODIFY zsdt0134 FROM wa_zsdt0134.
    ENDLOOP.

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

    LOOP AT it_carga_aux_5320 INTO wa_carga_5320.

      SELECT SINGLE *
        FROM zsdt0133
        INTO wa_zsdt0133
        WHERE nro_cg EQ wa_carga_5320-nro_cg.

      wa_zsdt0133-status = '6'.
      MODIFY zsdt0133 FROM wa_zsdt0133.
    ENDLOOP.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_SOL_CLICK_5320
*&---------------------------------------------------------------------*
FORM check_ordens_rom_5320  USING    p_e_row_index
                                 CHANGING vl_check_rom.

  DATA: it_ordem_check_aux_5320 TYPE STANDARD TABLE OF ty_ordem_5320,
        wa_carga_5320           TYPE ty_carga_5320,
        wa_ordem_5320           TYPE ty_ordem_5320,
        wa_ordem_check_aux_5320 TYPE ty_ordem_5320,
        vl_cont                 TYPE i,
        vl_qtd_vinc             TYPE zsdt0131-qtd_vinc,
        it_zsdt0129             TYPE STANDARD TABLE OF zsdt0129,
        wa_zsdt0129             TYPE zsdt0129,
        it_zsdt0134             TYPE STANDARD TABLE OF zsdt0134,
        wa_zsdt0134             TYPE zsdt0134,
        it_zsdt0082             TYPE STANDARD TABLE OF zsdt0082,
        wa_zsdt0082             TYPE zsdt0082,
        vl_tabix                TYPE sy-tabix.

  CLEAR: it_ordem_check_5320.

  SELECT *
    FROM zsdt0131
    INTO CORRESPONDING FIELDS OF TABLE it_ordem_check_5320
    FOR ALL ENTRIES IN it_carga_aux_5320
    WHERE status NE 'X'
      AND EXISTS ( SELECT *
                     FROM zsdt0129
                    WHERE nro_cg   EQ it_carga_aux_5320-nro_cg
                      AND nro_lote EQ zsdt0131~nro_lote
                      AND status   NE 'X' ).

  SELECT *
    FROM zsdt0134
    INTO TABLE it_zsdt0134
    FOR ALL ENTRIES IN it_carga_aux_5320
    WHERE status NE 'X'
      AND nro_cg EQ it_carga_aux_5320-nro_cg.

*-CS2019001891 - JT - 04.02.2021 - inicio
  IF it_ordem_check_5320[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0082
      INTO TABLE it_zsdt0082
      FOR ALL ENTRIES IN it_ordem_check_5320
      WHERE nro_sol EQ it_ordem_check_5320-nro_sol
        AND status  EQ '1'.

    SORT it_zsdt0082 BY nro_sol.
  ENDIF.

  LOOP AT it_ordem_check_5320 INTO wa_ordem_5320.
    vl_tabix = sy-tabix.

    READ TABLE it_zsdt0082       INTO wa_zsdt0082 WITH KEY nro_sol = wa_ordem_5320-nro_sol
                                 BINARY SEARCH.
    IF sy-subrc = 0.
      wa_ordem_5320-nr_rot          = wa_zsdt0082-nr_rot.
      MODIFY it_ordem_check_5320 FROM wa_ordem_5320 INDEX vl_tabix.
    ENDIF.
  ENDLOOP.
*-CS2019001891 - JT - 04.02.2021 - fim

  IF it_ordem_check_5320 IS NOT INITIAL.

    SELECT *
      FROM zsdt0129
      INTO TABLE it_zsdt0129
      FOR ALL ENTRIES IN it_ordem_check_5320
      WHERE nro_lote EQ it_ordem_check_5320-nro_lote
        AND status   NE 'X'.

  ENDIF.

  it_ordem_check_aux_5320 = it_ordem_check_5320.

  SORT it_ordem_check_5320 BY vbeln posnr nr_rot ASCENDING.
  DELETE ADJACENT DUPLICATES FROM it_ordem_check_5320 COMPARING vbeln posnr nr_rot.

  LOOP AT it_ordem_check_5320 INTO wa_ordem_5320.

    vl_cont = vl_cont + 1.

    READ TABLE it_zsdt0129 INTO wa_zsdt0129 WITH KEY nro_lote = wa_ordem_5320-nro_lote.
    IF sy-subrc IS INITIAL.
      wa_ordem_5320-nro_cg = wa_zsdt0129-nro_cg.
    ENDIF.

    LOOP AT it_ordem_check_aux_5320 INTO wa_ordem_check_aux_5320 WHERE vbeln  EQ wa_ordem_5320-vbeln
                                                                   AND posnr  EQ wa_ordem_5320-posnr
                                                                   AND nr_rot EQ wa_ordem_5320-nr_rot.
      vl_qtd_vinc = vl_qtd_vinc + wa_ordem_check_aux_5320-qtd_vinc.
    ENDLOOP.

    LOOP AT it_zsdt0134 INTO wa_zsdt0134 WHERE vbeln  EQ wa_ordem_5320-vbeln
                                           AND posnr  EQ wa_ordem_5320-posnr
                                           AND nr_rot EQ wa_ordem_5320-nr_rot
                                           AND nro_cg EQ wa_ordem_5320-nro_cg.

      wa_ordem_5320-qtd_vinc_lt = wa_ordem_5320-qtd_vinc_lt + wa_zsdt0134-lfimg.
    ENDLOOP.

    wa_ordem_5320-qtd_vinc = vl_qtd_vinc.

    IF wa_ordem_5320-qtd_vinc NE wa_ordem_5320-qtd_vinc_lt.
      vl_check_rom = abap_true.
      MESSAGE s000(z_fi) WITH TEXT-050 wa_ordem_5320-nro_cg DISPLAY LIKE 'E'.
    ENDIF.

    MODIFY it_ordem_check_5320 FROM wa_ordem_5320 INDEX vl_cont.
    CLEAR: vl_qtd_vinc.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_VOLUME_PEDIDO
*&---------------------------------------------------------------------*
FORM check_volume_pedido  CHANGING vl_check_initial.

  DATA: it_zsdt0062     TYPE STANDARD TABLE OF zsdt0062,
        wa_zsdt0062     TYPE zsdt0062,
        it_zsdt0134     TYPE STANDARD TABLE OF zsdt0134,
        wa_zsdt0134     TYPE zsdt0134,
        it_zsdt0134_aux TYPE STANDARD TABLE OF zsdt0134,
        wa_zsdt0134_aux TYPE zsdt0134,
        vl_qtd_vinc1    TYPE zsdt0062-qtd_vinc,
        vl_qtd_vinc2    TYPE zsdt0134-lfimg.

  SELECT *
    FROM zsdt0062
    INTO TABLE it_zsdt0062
    FOR ALL ENTRIES IN it_nlote_5320
    WHERE matnr  EQ vg_nlote_matnr
      AND nro_cg EQ vg_nlote_nro_cg
      AND ebeln  EQ it_nlote_5320-ebeln.

  SELECT *
    FROM zsdt0134
    INTO TABLE it_zsdt0134
    WHERE nro_cg EQ vg_nlote_nro_cg
      AND vbeln  EQ vg_nlote_vbeln
      AND posnr  EQ vg_nlote_posnr.

  it_zsdt0134_aux = it_zsdt0134.

  SORT it_zsdt0134 BY ebeln.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0134 COMPARING ebeln.

  LOOP AT it_zsdt0134 INTO wa_zsdt0134.
    LOOP AT it_zsdt0062 INTO wa_zsdt0062 WHERE ebeln EQ wa_zsdt0134-ebeln.
      vl_qtd_vinc1 = vl_qtd_vinc1 + wa_zsdt0062-qtd_vinc.
    ENDLOOP.
    LOOP AT it_zsdt0134_aux INTO wa_zsdt0134_aux WHERE ebeln EQ wa_zsdt0134-ebeln.
      vl_qtd_vinc2 = vl_qtd_vinc2 + wa_zsdt0134_aux-lfimg.
    ENDLOOP.
    IF vl_qtd_vinc1 LT vl_qtd_vinc2.
      vl_check_initial = abap_true.
    ENDIF.
  ENDLOOP.

ENDFORM.




*&---------------------------------------------------------------------*
*&      Form  GERAR_PDF_EMBARQUE
*&---------------------------------------------------------------------*
FORM gerar_pdf_embarque_prod USING p_nro_cg.

  TYPES: BEGIN OF ty_vbak,
           vbeln TYPE vbak-vbeln,
           posnr TYPE vbap-posnr,
           knumv TYPE vbak-knumv,
         END OF ty_vbak,

         BEGIN OF ty_seq,
           nr_cg TYPE zsdt0129-nro_cg,
           ini   TYPE n LENGTH 2,
           fim   TYPE n LENGTH 2,
         END OF ty_seq.

  DATA: it_zsdt0129 TYPE TABLE OF zsdt0129 WITH HEADER LINE,
        it_kna1     TYPE TABLE OF kna1 WITH HEADER LINE,
        it_zsdt0131 TYPE TABLE OF zsdt0131 WITH HEADER LINE,
        it_zsdt0132 TYPE TABLE OF zsdt0132 WITH HEADER LINE,
        it_adr6     TYPE TABLE OF adr6 WITH HEADER LINE,
        it_vbak     TYPE TABLE OF ty_vbak WITH HEADER LINE,
        it_fat      TYPE TABLE OF zembarquefat WITH HEADER LINE,
        it_pro      TYPE TABLE OF zembarquepro WITH HEADER LINE,
        it_com      TYPE TABLE OF zembarquecom WITH HEADER LINE,
        wa_top      TYPE zembarquetopo,
        wa_tra      TYPE zembarquetrans,
        it_exc      TYPE TABLE OF zsdt0135 WITH HEADER LINE,
        nro_cg      TYPE zsdt0133-nro_cg.

  DATA: vl_formname TYPE tdsfname,
        vl_name     TYPE rs38l_fnam,
        wa_import   TYPE zcontrato_insumos,
        it_itens_im TYPE TABLE OF zcontrato_insumos_itens.

  DATA r_seq TYPE TABLE OF ty_seq WITH HEADER LINE.

  nro_cg = p_nro_cg.

* Cabeçario de Carga
  SELECT * FROM zsdt0133
    INTO TABLE @DATA(it_zsdt0133)
    WHERE nro_cg EQ @nro_cg.

  CHECK NOT it_zsdt0133[] IS INITIAL.

* Cabeçario de Lote
  SELECT * FROM zsdt0129
    INTO TABLE it_zsdt0129
    FOR ALL ENTRIES IN it_zsdt0133
    WHERE nro_cg EQ it_zsdt0133-nro_cg.

  CHECK NOT it_zsdt0129[] IS INITIAL.

** Marca da Carga
*  SELECT * FROM ZSDT0135
*    INTO TABLE @DATA(IT_ZSDT0135)
*    FOR  ALL ENTRIES IN @IT_ZSDT0129
*    WHERE WRKST EQ @IT_ZSDT0129-MARCA.

* Cliente do Lote
  SELECT * FROM zsdt0130
     INTO TABLE @DATA(it_zsdt0130)
     FOR ALL ENTRIES IN @it_zsdt0129
     WHERE nro_lote EQ @it_zsdt0129-nro_lote
       AND status NE 'X'.

* Ordem do Lote
  SELECT * FROM zsdt0131
    INTO TABLE it_zsdt0131
    FOR ALL ENTRIES IN it_zsdt0129
    WHERE nro_lote EQ it_zsdt0129-nro_lote
      AND status NE 'X'.

  IF NOT it_zsdt0131[] IS INITIAL.

* Controle de Vinculação Pedido X Ordem
    SELECT * FROM zsdt0062
      INTO TABLE @DATA(it_zsdt0062)
      FOR ALL ENTRIES IN @it_zsdt0131
      WHERE nro_cg EQ @nro_cg
        AND vbeln  EQ @it_zsdt0131-vbeln
        AND status EQ 'L'.

    IF NOT it_zsdt0062[] IS INITIAL.
* Cabeçario do Pedido
      SELECT * FROM ekko
        INTO TABLE @DATA(it_ekko)
        FOR ALL ENTRIES IN @it_zsdt0062
        WHERE ebeln EQ @it_zsdt0062-ebeln.
    ENDIF.

    SELECT * FROM zsdt0132
      INTO TABLE it_zsdt0132
      FOR ALL ENTRIES IN it_zsdt0131
      WHERE nr_rot EQ it_zsdt0131-cod_loc_emb.

*      SELECT * FROM ZSDT0132
*      INTO TABLE IT_ZSDT0132
*      FOR ALL ENTRIES IN IT_ZSDT0133
*      WHERE LIFNR EQ IT_ZSDT0133-COD_TRANSPORTADORA
*      AND MARCA EQ IT_ZSDT0133-.


  ENDIF.

* Dados do Fornecedor
*####################################################
  IF NOT it_ekko[] IS INITIAL.
    SELECT * FROM lfa1
      INTO TABLE @DATA(it_lfa1)
      FOR  ALL ENTRIES IN @it_ekko
      WHERE lifnr EQ @it_ekko-lifnr.
  ENDIF.

  IF NOT it_zsdt0132[] IS INITIAL.
    SELECT * FROM lfa1
      APPENDING TABLE it_lfa1
      FOR ALL ENTRIES IN it_zsdt0132
      WHERE lifnr EQ it_zsdt0132-lifnr.
  ENDIF.

  IF NOT it_zsdt0133[] IS INITIAL.
    SELECT * FROM lfa1
      APPENDING TABLE it_lfa1
      FOR ALL ENTRIES IN it_zsdt0133
      WHERE lifnr EQ it_zsdt0133-cod_transportadora.
  ENDIF.

  IF NOT it_zsdt0129[] IS  INITIAL.
    SELECT * FROM lfa1
      APPENDING TABLE it_lfa1
      FOR ALL ENTRIES IN it_zsdt0129
      WHERE lifnr EQ it_zsdt0129-motorista.

  ENDIF.

  IF NOT it_zsdt0131[] IS INITIAL.
    SELECT * FROM kna1
      INTO TABLE it_kna1
      FOR ALL ENTRIES IN it_zsdt0131
      WHERE kunnr EQ it_zsdt0131-kunnr.

    SELECT * FROM makt
      INTO TABLE @DATA(it_makt)
      FOR ALL ENTRIES IN @it_zsdt0131
      WHERE matnr EQ @it_zsdt0131-matnr.
  ENDIF.
*####################################################

  IF NOT it_zsdt0129[] IS INITIAL.
    SELECT zsdt0131~vbeln
           zsdt0131~posnr
           vbak~knumv
        FROM zsdt0131
        INNER JOIN vbak ON zsdt0131~vbeln = vbak~vbeln
        INTO CORRESPONDING FIELDS OF TABLE it_vbak
        FOR ALL ENTRIES IN it_zsdt0129
        WHERE nro_lote EQ it_zsdt0129-nro_lote
        AND status NE abap_true.

    IF NOT it_vbak[] IS INITIAL.
      SELECT * FROM konv
        INTO TABLE @DATA(it_konv)
        FOR ALL ENTRIES IN @it_vbak
        WHERE knumv EQ @it_vbak-knumv
         AND kposn EQ @it_vbak-posnr
         AND kschl EQ 'PR00'.
    ENDIF.
  ENDIF.

  IF NOT it_kna1[] IS INITIAL.

    SELECT * FROM adr6
      INTO TABLE it_adr6
      FOR ALL ENTRIES IN it_kna1
      WHERE addrnumber EQ it_kna1-adrnr.

    SELECT * FROM adr2
      INTO TABLE @DATA(it_adr2)
      FOR ALL ENTRIES IN @it_kna1
      WHERE addrnumber EQ @it_kna1-adrnr.

  ENDIF.

  DATA(it_0131) = it_zsdt0131[].
  SORT it_0131 BY kunnr.
  DELETE ADJACENT DUPLICATES FROM it_0131 COMPARING kunnr.



  LOOP AT it_0131 INTO DATA(wa_0131).
*    BREAK-POINT.
*    IF NOT LINE_EXISTS( IT_ZSDT0135[  WRKST = IT_ZSDT0129[ NRO_LOTE = WA_0131-NRO_LOTE  ]-MARCA ] ).
*
*      DATA WKNA1 TYPE KNA1.
*
*      SELECT SINGLE *
*        FROM KNA1
*        INTO WKNA1
*        WHERE KUNNR EQ WA_0131-WERKS.
*
*      IT_FAT-CLIENTE  = WKNA1-NAME1.
*      IT_FAT-ENDERECO = WKNA1-STRAS.
*      IT_FAT-CPF_CNPJ = WKNA1-STCD1.
*      IT_FAT-INS_EST  = WKNA1-STCD3.
*      IT_FAT-CIDADE   = WKNA1-ORT01.
*      IT_FAT-UF       = WKNA1-REGIO.
*
*      SELECT SINGLE SMTP_ADDR FROM ADR6
*        INTO IT_FAT-EMAIL
*      WHERE ADDRNUMBER EQ WKNA1-ADRNR.
*
*      SELECT SINGLE TEL_NUMBER FROM ADR2
*        INTO IT_FAT-CONTATO
*        WHERE ADDRNUMBER EQ WKNA1-ADRNR.
*
*      APPEND IT_FAT.
*      CLEAR IT_FAT.
*
*      EXIT.
*    ENDIF.

    TRY .
        it_zsdt0132 = it_zsdt0132[ nr_rot = wa_0131-cod_loc_emb ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR it_zsdt0132.
    ENDTRY.

    TRY .
        it_kna1 = it_kna1[ kunnr = wa_0131-kunnr  ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR it_kna1.
    ENDTRY.

    TRY .
        it_adr6 = it_adr6[ addrnumber = it_kna1-adrnr ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR it_adr6.
    ENDTRY.

    it_fat-cliente  = it_kna1-name1.
    it_fat-endereco = it_zsdt0132-rot_desc.
    it_fat-cpf_cnpj = it_kna1-stcd1.
    it_fat-ins_est  = it_kna1-stcd3.
    it_fat-cidade   = it_zsdt0132-city1.
    it_fat-uf       = it_zsdt0132-uf.
    it_fat-email    = it_adr6-smtp_addr.
    it_fat-contato  = it_zsdt0132-tel_number.

    IF it_fat-cliente IS INITIAL .
      it_fat-cliente  = '_                          _'.
    ENDIF.
    IF it_fat-endereco  IS INITIAL.
      it_fat-endereco = '_                          _'.
    ENDIF.
    IF it_fat-cpf_cnpj IS INITIAL.
      it_fat-cpf_cnpj = '_                          _'.
    ENDIF.
    IF it_fat-ins_est  IS INITIAL.
      it_fat-ins_est  = '_                          _'.
    ENDIF.
    IF it_fat-cidade   IS INITIAL.
      it_fat-cidade   = '_                          _'.
    ENDIF.
    IF it_fat-uf       IS INITIAL.
      it_fat-uf       = '_                          _'.
    ENDIF.
    IF it_fat-email    IS INITIAL.
      it_fat-email    = '_                          _'.
    ENDIF.
    IF it_fat-contato  IS INITIAL.
      it_fat-contato  = '_                          _'.
    ENDIF.

    APPEND it_fat.
    CLEAR it_fat.
    EXIT.
  ENDLOOP.

  LOOP AT it_zsdt0129.

    wa_top-nro_cg = it_zsdt0129-nro_cg.
    wa_top-dt_emissao = sy-datum.
    wa_top-icon = it_zsdt0129-inco1.



    LOOP AT it_zsdt0131 INTO DATA(wa_zsdt0131) WHERE nro_lote EQ it_zsdt0129-nro_lote.

      wa_top-spart = wa_zsdt0131-spart.

      CASE wa_top-spart.
        WHEN '04'.
          wa_top-desc_spart = 'SEMENTES'.
        WHEN '02'.
          wa_top-desc_spart = 'FERTILIZANTES'.
      ENDCASE.

      it_pro-nro_cg     = it_zsdt0129-nro_cg.

      it_pro-vkbur      = wa_zsdt0131-vkbur.
      it_pro-vbeln      = wa_zsdt0131-vbeln.

      it_pro-qtd_vinc   = wa_zsdt0131-qtd_vinc.
      it_pro-um         = wa_zsdt0131-um.
      it_pro-qtd_emkg   = wa_zsdt0131-qtd_emkg.

      TRY .
          DATA(wa_zsdt0132) = it_zsdt0132[ nr_rot = wa_zsdt0131-cod_loc_emb ].
          it_pro-local      = wa_zsdt0132-rot_desc.
          it_pro-city1      = wa_zsdt0132-city1.
          it_pro-uf         = wa_zsdt0132-uf.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY .
          DATA(wa_zsdt0130) = it_zsdt0130[ nro_lote = it_zsdt0129-nro_lote
                                           nro_sol = wa_zsdt0131-nro_sol ].
          it_pro-seq_ent_cg = wa_zsdt0130-seq_ent_cg.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY .
          it_pro-name1 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-name1.
          it_pro-mcod3 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-mcod3.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY .
          it_pro-maktx = it_makt[ matnr = wa_zsdt0131-matnr ]-maktx.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY .
          wa_zsdt0132 = it_zsdt0132[ nr_rot = wa_zsdt0130-nr_rot ].
          it_pro-fazenda    = wa_zsdt0132-rot_desc.
          it_pro-tel_number = wa_zsdt0132-tel_number.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      it_pro-nr_rot = wa_zsdt0130-nr_rot.

      TRY .
          DATA(wa_vbak) = it_vbak[ vbeln = wa_zsdt0131-vbeln
                                   posnr = wa_zsdt0131-posnr ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY .
          DATA(wa_konv) = it_konv[ knumv = wa_vbak-knumv
                                   kposn = wa_vbak-posnr ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      CASE wa_konv-kmein.
        WHEN 'TO'.
          it_pro-preco_frete = ( ( ( ( wa_zsdt0131-qtd_vinc * wa_zsdt0131-brgew ) * wa_konv-kbetr ) / 1000 ) * wa_konv-kkurs ).
        WHEN OTHERS.
          it_pro-preco_frete = ( ( wa_zsdt0131-qtd_vinc * wa_konv-kbetr ) * wa_konv-kkurs ).
      ENDCASE.

      r_seq-ini    = it_pro-seq_ent_cg.
      r_seq-nr_cg  = it_pro-nro_cg.
      APPEND r_seq.
      CLEAR r_seq.

      TRY.
          DATA(wa_0132) = it_zsdt0132[ nr_rot = wa_zsdt0131-cod_loc_emb ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      LOOP AT it_zsdt0062 INTO DATA(wa_zsdt0062) WHERE vbeln EQ wa_zsdt0131-vbeln AND nro_cg EQ it_zsdt0129-nro_cg.

        it_com-local_embarque = wa_0132-rot_desc.
        it_com-municipio = wa_0132-city1.
        it_com-endereco = it_lfa1[ lifnr = wa_0132-lifnr ]-stras.
        it_com-uf = wa_0132-uf.

        TRY .
            it_com-fornecedor = it_lfa1[ lifnr = it_ekko[ lifnr = wa_zsdt0062-lifnr ]-lifnr ]-name1.
          CATCH cx_sy_itab_line_not_found.
            CLEAR it_com-fornecedor.
        ENDTRY.

        TRY .
            it_com-ihrez = it_ekko[ ebeln = wa_zsdt0062-ebeln ]-ihrez.
          CATCH cx_sy_itab_line_not_found.
            CLEAR it_com-ihrez.
        ENDTRY.

*        IF NOT LINE_EXISTS( IT_ZSDT0135[  WRKST = IT_ZSDT0129-MARCA ] ).
        it_pro-wrkst = it_com-ihrez.
*        ELSE.
*          IT_PRO-WRKST = WA_ZSDT0062-VBELN.
*        ENDIF.

        it_com-ebeln = wa_zsdt0062-ebeln.

        IF NOT line_exists( it_com[ ebeln = wa_zsdt0062-ebeln ] ).
          APPEND it_com.
        ENDIF.

        CLEAR it_com.

      ENDLOOP.

      APPEND it_pro .
      CLEAR it_pro .

    ENDLOOP.

  ENDLOOP.

  SORT r_seq BY nr_cg ini.
  DELETE ADJACENT DUPLICATES FROM r_seq COMPARING nr_cg ini.

  DATA cont TYPE n LENGTH 2.

  LOOP AT r_seq .

    cont = 0.

    LOOP AT r_seq WHERE nr_cg EQ r_seq-nr_cg.
      ADD 1 TO cont.
    ENDLOOP.

    LOOP AT r_seq ASSIGNING FIELD-SYMBOL(<seq>) WHERE nr_cg EQ r_seq-nr_cg.
      <seq>-fim    = cont.
      SUBTRACT 1 FROM cont.
    ENDLOOP.

  ENDLOOP.

  LOOP AT it_pro  ASSIGNING FIELD-SYMBOL(<exc>).

    r_seq = r_seq[ nr_cg = <exc>-nro_cg
                     ini = <exc>-seq_ent_cg ].
    IF r_seq-ini EQ <exc>-seq_ent_cg AND
       r_seq-nr_cg EQ <exc>-nro_cg.
      <exc>-seq_ent_cg = r_seq-fim.
    ENDIF.

  ENDLOOP.

  SORT it_pro[] BY seq_ent_cg.

  LOOP AT it_zsdt0133 INTO DATA(wa_0133).

    TRY .
        wa_tra-transportadora = it_lfa1[ lifnr = wa_0133-cod_transportadora ]-name1.
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_tra-transportadora.
    ENDTRY.

    TRY .
        wa_tra-cnpj  = it_lfa1[ lifnr = wa_0133-cod_transportadora ]-stcd1.
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_tra-cnpj.
    ENDTRY.

  ENDLOOP.

  vl_formname = 'ZSDF0013'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_formname
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'. "SY-MSGTY
*    EXIT.
  ENDIF.

  CALL FUNCTION vl_name
    EXPORTING
      wa_top           = wa_top
      wa_tra           = wa_tra
    TABLES
      it_fat           = it_fat
      it_pro           = it_pro
      it_com           = it_com
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'. "SY-MSGTY
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_LOTES_PRODUTOS_5320
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VL_CHECK  text
*----------------------------------------------------------------------*
FORM busca_lotes_produtos_5320  CHANGING vl_check.

  DATA: it_zsdt0131 TYPE STANDARD TABLE OF zsdt0131,
        it_zsdt0134 TYPE STANDARD TABLE OF zsdt0134,
        it_zsdt0129 TYPE STANDARD TABLE OF zsdt0129,
        wa_zsdt0134 TYPE zsdt0134,
        vl_cont     TYPE i.

  SELECT *
    FROM zsdt0129
    INTO TABLE it_zsdt0129
    FOR ALL ENTRIES IN it_carga_canc_5320
    WHERE nro_cg EQ it_carga_canc_5320-nro_cg
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
    READ TABLE it_carga_canc_5320 WITH KEY nro_cg = wa_zsdt0134-nro_cg TRANSPORTING NO FIELDS.
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
*&      Form  CANCELA_AUTORIZACAO_5320
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cancela_autorizacao_5320 .

  DATA: it_zsdt0133 TYPE STANDARD TABLE OF zsdt0133,
        wa_zsdt0133 TYPE zsdt0133.

  SELECT *
    FROM zsdt0133
    INTO TABLE it_zsdt0133
    FOR ALL ENTRIES IN it_carga_canc_5320
    WHERE nro_cg EQ it_carga_canc_5320-nro_cg
      AND status NE 'X'.

  LOOP AT it_zsdt0133 INTO wa_zsdt0133.
    IF wa_zsdt0133-status EQ 5.
      wa_zsdt0133-status = 4.
      it_carga_5320[ nro_cg = wa_zsdt0133-nro_cg ]-status = 4.
    ENDIF.
    MODIFY zsdt0133 FROM wa_zsdt0133.
  ENDLOOP.

ENDFORM.

FORM f_fieldinfo_get USING p_tabname
                           p_fieldname
                  CHANGING p_field_tab.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = p_tabname
      fieldname      = p_fieldname
      lfieldname     = p_fieldname
    IMPORTING
      dfies_wa       = p_field_tab
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.

ENDFORM.
