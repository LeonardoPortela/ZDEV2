*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5720
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_carga_5720,
         icone  TYPE char6,
         name1  TYPE lfa1-name1,
         name2  TYPE lfa1-name1,
         name3  TYPE lfa1-name1,
         inco1  TYPE vbkd-inco1,
         cor(4) TYPE c.
         INCLUDE STRUCTURE zsdt0139.
       TYPES: END OF ty_carga_5720.

TYPES: BEGIN OF ty_solicitacao_5720,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
         inco1 TYPE vbkd-inco1,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
         meins TYPE vbap-meins.
         INCLUDE STRUCTURE zsdt0082.
       TYPES: END OF ty_solicitacao_5720.

DATA: it_carga_5720 TYPE STANDARD TABLE OF ty_carga_5720,
      it_sol_5720   TYPE STANDARD TABLE OF ty_solicitacao_5720.

DATA: g_custom_container_5720    TYPE REF TO cl_gui_custom_container,
      dg_splitter_1_5720         TYPE REF TO cl_gui_splitter_container,
      dg_parent_1_5720           TYPE REF TO cl_gui_container,
      dg_parent_2_5720           TYPE REF TO cl_gui_container,
      ctl_alv1_5720              TYPE REF TO cl_gui_alv_grid,
      ctl_alv2_5720              TYPE REF TO cl_gui_alv_grid,
      gs_layout_5720_alv1        TYPE lvc_s_layo,
      gs_layout_5720_alv2        TYPE lvc_s_layo,
      it_fieldcatalog_carga_5720 TYPE lvc_t_fcat,
      it_fieldcatalog_sol_5720   TYPE lvc_t_fcat,
      it_sort_carga_5720         TYPE lvc_t_sort,
      it_sort_sol_5720           TYPE lvc_t_sort.

DATA: it_sel_rows_lote_5720 TYPE lvc_t_row,
      vg_carga_5720         TYPE ty_carga_5720.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler_5720 DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click_5720 FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      toolbar_5720 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      user_command_5720 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler_5720 IMPLEMENTATION.

  METHOD on_double_click_5720.

    DATA: it_vbkd       TYPE STANDARD TABLE OF vbkd,
          it_vbpa       TYPE STANDARD TABLE OF vbpa,
          it_vbap       TYPE STANDARD TABLE OF vbap,
          it_kna1       TYPE STANDARD TABLE OF kna1,
          it_makt       TYPE STANDARD TABLE OF makt,
          it_zsdt0140   TYPE STANDARD TABLE OF zsdt0140,
          wa_vbkd       TYPE vbkd,
          wa_vbpa       TYPE vbpa,
          wa_vbap       TYPE vbap,
          wa_kna1       TYPE kna1,
          wa_makt       TYPE makt,
          wa_carga_5720 TYPE ty_carga_5720,
          wa_sol_5720   TYPE ty_solicitacao_5720,
          vl_cont       TYPE i.

    CLEAR: vg_carga_5720.

    LOOP AT it_carga_5720 INTO wa_carga_5720 WHERE cor IS NOT INITIAL.
      CLEAR: wa_carga_5720-cor.
      MODIFY it_carga_5720 FROM wa_carga_5720 INDEX sy-tabix.
    ENDLOOP.

    READ TABLE it_carga_5720 INTO wa_carga_5720 INDEX e_row-index.
    IF sy-subrc IS INITIAL.
      wa_carga_5720-cor = 'C300'.
      vg_carga_5720 = wa_carga_5720.
      MODIFY it_carga_5720 FROM wa_carga_5720 INDEX e_row-index.
    ENDIF.

    SELECT *
      FROM zsdt0140
      INTO CORRESPONDING FIELDS OF TABLE it_zsdt0140
      WHERE nro_cgd   EQ vg_carga_5720-nro_cgd
        AND status NE 'X'.

    IF it_zsdt0140 IS NOT INITIAL.

      SELECT *
        FROM zsdt0082
        INTO CORRESPONDING FIELDS OF TABLE it_sol_5720
        FOR ALL ENTRIES IN it_zsdt0140
        WHERE nro_sol EQ it_zsdt0140-nro_sol
          AND seq     EQ it_zsdt0140-seq
          AND status  NE 'X'.

      IF it_sol_5720 IS NOT INITIAL.

        SELECT *
          FROM vbkd
          INTO TABLE it_vbkd
          FOR ALL ENTRIES IN it_sol_5720
          WHERE vbeln EQ it_sol_5720-vbeln.

        SELECT *
          FROM vbap
          INTO TABLE it_vbap
          FOR ALL ENTRIES IN it_sol_5720
          WHERE vbeln EQ it_sol_5720-vbeln
            AND posnr EQ it_sol_5720-posnr.

        IF it_vbap IS NOT INITIAL.

          SELECT *
            FROM makt
            INTO TABLE it_makt
            FOR ALL ENTRIES IN it_vbap
            WHERE matnr EQ it_vbap-matnr.

        ENDIF.

        SELECT *
          FROM vbpa
          INTO TABLE it_vbpa
          FOR ALL ENTRIES IN it_sol_5720
          WHERE vbeln EQ it_sol_5720-vbeln
          AND parvw   EQ 'AG'.

        IF it_vbpa IS NOT INITIAL.

          SELECT *
            FROM kna1
            INTO TABLE it_kna1
            FOR ALL ENTRIES IN it_vbpa
            WHERE kunnr EQ it_vbpa-kunnr.

        ENDIF.

      ENDIF.

    ENDIF.

    LOOP AT it_sol_5720 INTO wa_sol_5720.

      vl_cont = vl_cont + 1.

      READ TABLE it_vbpa INTO wa_vbpa WITH KEY vbeln = wa_sol_5720-vbeln.
      IF sy-subrc IS INITIAL.
        wa_sol_5720-kunnr = wa_vbpa-kunnr.
        READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_sol_5720-kunnr.
        IF sy-subrc IS INITIAL.
          wa_sol_5720-name1 = wa_kna1-name1.
        ENDIF.
      ENDIF.

      READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_sol_5720-vbeln.
      IF sy-subrc IS INITIAL.
        wa_sol_5720-inco1 = wa_vbkd-inco1.
      ENDIF.

      READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_sol_5720-vbeln
                                               posnr = wa_sol_5720-posnr.
      IF sy-subrc IS INITIAL.
        wa_sol_5720-meins = wa_vbap-meins.
        wa_sol_5720-matnr = wa_vbap-matnr.
        READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_sol_5720-matnr.
        IF sy-subrc IS INITIAL.
          wa_sol_5720-maktx = wa_makt-maktx.
        ENDIF.
      ENDIF.

      MODIFY it_sol_5720 FROM wa_sol_5720 INDEX vl_cont.

    ENDLOOP.

    CALL METHOD ctl_alv2_5720->get_frontend_layout
      IMPORTING
        es_layout = gs_layout_5720_alv2.

    gs_layout_5720_alv2-cwidth_opt = abap_true.

    CALL METHOD ctl_alv2_5720->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_5720_alv2.

    CALL METHOD ctl_alv1_5720->refresh_table_display
      EXPORTING
        is_stable = _stable.
    CALL METHOD ctl_alv2_5720->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD toolbar_5720.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'NOVACARGA'.
    wa_tool-icon     = '@XR@'.
    wa_tool-quickinfo = 'Nova Carga'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'EDITARCARGA'.
    wa_tool-icon     = '@HL@'.
    wa_tool-quickinfo = 'Editar Carga'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'REPROVAR'.
    wa_tool-icon     = '@2W@'.
    wa_tool-quickinfo = 'Reprovar Carga'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'APROVAR'.
    wa_tool-icon     = '@4R@'.
    wa_tool-quickinfo = 'Aprovar Carga'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

  ENDMETHOD.             "DISPLAY

  METHOD user_command_5720.

    DATA:it_sel_rows_carga_5720 TYPE lvc_t_row,
         it_zsdt0134            TYPE STANDARD TABLE OF zsdt0134,
         it_zsdt0140            TYPE STANDARD TABLE OF zsdt0140,
         wa_sel_rows_carga_5720 TYPE lvc_s_row,
         wa_carga_5720          TYPE ty_carga_5720,
         wa_zsdt0139            TYPE zsdt0139,
         vl_lines               TYPE i,
         vl_check               TYPE char1.

    IF e_ucomm = 'NOVACARGA'.

      vg_subt_lote = '5730'.
      PERFORM clear_5730.

    ELSEIF e_ucomm = 'EDITARCARGA'.

      CALL METHOD ctl_alv1_5720->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows_carga_5720.

      DESCRIBE TABLE it_sel_rows_carga_5720 LINES vl_lines.
      IF vl_lines NE 1.
        MESSAGE text-041 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        CLEAR: vg_lote_editar.
        READ TABLE it_sel_rows_carga_5720 INTO wa_sel_rows_carga_5720 INDEX 1.
        IF sy-subrc IS INITIAL.
          READ TABLE it_carga_5720 INTO wa_carga_5720 INDEX wa_sel_rows_carga_5720-index.
          IF sy-subrc IS INITIAL.

            CLEAR: wa_zsdt0139.

            CLEAR: wa_zsdt0139.

            SELECT SINGLE *
              FROM zsdt0139
              INTO wa_zsdt0139
              WHERE nro_cgd EQ wa_carga_5720-nro_cgd.

            IF wa_carga_5720-status NE wa_zsdt0139-status.
              MESSAGE text-096 TYPE 'S' DISPLAY LIKE 'E'.
            ELSEIF wa_zsdt0139-status GE 1.
              MESSAGE text-086 TYPE 'S' DISPLAY LIKE 'E'.
            ELSE.
              CALL FUNCTION 'ZENQUEUE_SD_CARGAD_INSUMOS'
                EXPORTING
                  chave          = wa_carga_5720-nro_cgd
                EXCEPTIONS
                  foreign_lock   = 1
                  system_failure = 2
                  OTHERS         = 3.

              IF sy-subrc <> 0.
                MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
              ELSE.
                vg_lote_editar = wa_carga_5720-nro_cgd.
                vg_subt_lote = '5740'.
                PERFORM completa_header_5740.
              ENDIF.
            ENDIF.

          ENDIF.
        ENDIF.

      ENDIF.

    ELSEIF e_ucomm = 'REPROVAR'.

      CLEAR: vl_check, vl_lines, wa_zsdt0139, it_zsdt0134, it_zsdt0140.

      CALL METHOD ctl_alv1_5720->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows_carga_5720.

      DESCRIBE TABLE it_sel_rows_carga_5720 LINES vl_lines.

      IF vl_lines NE 1.
        MESSAGE text-054 TYPE 'S' DISPLAY LIKE 'E'.
        MOVE abap_true TO vl_check.
      ENDIF.

      IF vl_check IS INITIAL.

        READ TABLE it_sel_rows_carga_5720 INTO wa_sel_rows_carga_5720 INDEX 1.
        READ TABLE it_carga_5720 INTO wa_carga_5720 INDEX wa_sel_rows_carga_5720-index.

        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'ZENQUEUE_SD_CARGAD_INSUMOS'
            EXPORTING
              chave          = wa_carga_5720-nro_cgd
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            MOVE abap_true TO vl_check.
          ENDIF.

          IF vl_check IS INITIAL.

            CLEAR: wa_zsdt0139.

            SELECT SINGLE *
                FROM zsdt0139
                INTO CORRESPONDING FIELDS OF wa_zsdt0139
                WHERE nro_cgd EQ wa_carga_5720-nro_cgd.

            IF wa_zsdt0139-status NE wa_carga_5720-status.
              MESSAGE text-096 TYPE 'S' DISPLAY LIKE 'E'.
            ELSEIF wa_zsdt0139-status NE '1'.
              MESSAGE text-099 TYPE 'S' DISPLAY LIKE 'E'.
            ELSE.

              SELECT *
                FROM zsdt0140
                INTO TABLE it_zsdt0140
                WHERE nro_cgd EQ wa_carga_5720-nro_cgd
                  AND status NE 'X'.

              SELECT *
                FROM zsdt0134
                INTO TABLE it_zsdt0134
                FOR ALL ENTRIES IN it_zsdt0140
                WHERE nro_cg EQ it_zsdt0140-nro_cgd
                  AND vbeln  EQ it_zsdt0140-vbeln
                  AND status NE 'X'.

*              SELECT SINGLE *
*                FROM ZSDT0134
*                INTO CORRESPONDING FIELDS OF WA_ZSDT0134
*                WHERE NRO_CG EQ WA_CARGA_5720-NRO_CGD
*                  AND STATUS NE 'X'.

              IF it_zsdt0134 IS NOT INITIAL.
                MESSAGE text-123 TYPE 'S' DISPLAY LIKE 'E'.
              ELSE.
                wa_zsdt0139-status = 0.
                MODIFY zsdt0139 FROM wa_zsdt0139.
                MESSAGE text-124 TYPE 'S' DISPLAY LIKE 'E'.
              ENDIF.

            ENDIF.

            CALL FUNCTION 'ZDENQUEUE_SD_CARGAD_INSUMOS'
              EXPORTING
                chave = wa_carga_5720-nro_cgd.

          ENDIF.

        ENDIF.

      ENDIF.

    ELSEIF e_ucomm = 'APROVAR'.

      CLEAR: vl_check, vl_lines, wa_zsdt0139.

      CALL METHOD ctl_alv1_5720->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows_carga_5720.

      DESCRIBE TABLE it_sel_rows_carga_5720 LINES vl_lines.

      IF vl_lines NE 1.
        MESSAGE text-054 TYPE 'S' DISPLAY LIKE 'E'.
        MOVE abap_true TO vl_check.
      ENDIF.

      IF vl_check IS INITIAL.

        READ TABLE it_sel_rows_carga_5720 INTO wa_sel_rows_carga_5720 INDEX 1.
        READ TABLE it_carga_5720 INTO wa_carga_5720 INDEX wa_sel_rows_carga_5720-index.

        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'ZENQUEUE_SD_CARGAD_INSUMOS'
            EXPORTING
              chave          = wa_carga_5720-nro_cgd
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            MOVE abap_true TO vl_check.
          ENDIF.

          IF vl_check IS INITIAL.

            CLEAR: wa_zsdt0139.

            SELECT SINGLE *
                FROM zsdt0139
                INTO CORRESPONDING FIELDS OF wa_zsdt0139
                WHERE nro_cgd EQ wa_carga_5720-nro_cgd.

            IF wa_zsdt0139-status NE wa_carga_5720-status.
              MESSAGE text-096 TYPE 'S' DISPLAY LIKE 'E'.
            ELSEIF wa_zsdt0139-status NE '0'.
              MESSAGE text-099 TYPE 'S' DISPLAY LIKE 'E'.
            ELSE.
              wa_zsdt0139-status = 1.
              MODIFY zsdt0139 FROM wa_zsdt0139.
            ENDIF.

            CALL FUNCTION 'ZDENQUEUE_SD_CARGAD_INSUMOS'
              EXPORTING
                chave = wa_carga_5720-nro_cgd.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    LEAVE TO SCREEN 5000.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_5720  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5720 OUTPUT.

  PERFORM seleciona_carga_5720.
  PERFORM alv_tela_carga_def.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5720  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_5720 INPUT.

  CASE sy-ucomm.
    WHEN 'LEG'.
      CALL SCREEN 6001 STARTING AT 5 5 ENDING AT 80 20.
    WHEN OTHERS.
  ENDCASE.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  SELECT_LOTE
*&---------------------------------------------------------------------*
FORM seleciona_carga_5720.

  DATA: it_lfa1       TYPE STANDARD TABLE OF lfa1,
        it_zsdt0140   TYPE STANDARD TABLE OF zsdt0140,
        wa_zsdt0140   TYPE zsdt0140,
        wa_lfa1       TYPE lfa1,
        wa_carga_5720 TYPE ty_carga_5720,
        vl_cont       TYPE i.

  SELECT DISTINCT
    zsdt0139~status
    zsdt0139~nro_cgd
    zsdt0139~cod_ce
    zsdt0139~cod_tr
    zsdt0139~cod_mt
    zsdt0139~placa_cav
    zsdt0139~placa_car1
    zsdt0139~placa_car2
*-CS2019001891 - JT - 04.02.2021 - inicio
    zsdt0139~placa_car3
*-CS2019001891 - JT - 04.02.2021 - fim
    zsdt0139~data_atual
    FROM zsdt0139
    INNER JOIN zsdt0140 ON zsdt0140~nro_cgd = zsdt0139~nro_cgd
    INNER JOIN zsdt0082 ON zsdt0140~nro_sol = zsdt0082~nro_sol
                           AND zsdt0140~seq = zsdt0082~seq
    INNER JOIN vbkd ON zsdt0082~vbeln = vbkd~vbeln
    INNER JOIN vbpa ON vbkd~vbeln = vbpa~vbeln
      INTO CORRESPONDING FIELDS OF TABLE it_carga_5720
      WHERE zsdt0139~nro_cgd    IN d_numcg
        AND zsdt0139~data_atual IN d_datab
        AND zsdt0082~vbeln      IN d_ovcor
        AND zsdt0139~status     NE 'X'
        AND zsdt0082~vkorg      IN d_vkorg
        AND zsdt0082~spart      EQ d_spart
        AND zsdt0082~vkbur      IN d_vkbur
        AND zsdt0082~vbeln      IN d_ovcor
  AND vbkd~inco1          IN d_inco1
  AND vbpa~kunnr          IN d_kunnr.

  IF it_carga_5720 IS NOT INITIAL.

    SELECT *
      FROM lfa1
      INTO TABLE it_lfa1
      FOR ALL ENTRIES IN it_carga_5720
      WHERE lifnr EQ it_carga_5720-cod_ce.

    SELECT *
      FROM lfa1
      APPENDING TABLE it_lfa1
      FOR ALL ENTRIES IN it_carga_5720
      WHERE lifnr EQ it_carga_5720-cod_tr.

    SELECT *
      FROM lfa1
      APPENDING TABLE it_lfa1
      FOR ALL ENTRIES IN it_carga_5720
      WHERE lifnr EQ it_carga_5720-cod_mt.



    SELECT *
      FROM zsdt0140
      INTO TABLE it_zsdt0140
      FOR ALL ENTRIES IN it_carga_5720
      WHERE nro_cgd EQ it_carga_5720-nro_cgd
        AND status NE 'X'.

  ENDIF.

  LOOP AT it_carga_5720 INTO wa_carga_5720.

    vl_cont = vl_cont + 1.

    IF wa_carga_5720-status EQ 0.
      wa_carga_5720-icone = '@5C@'.
    ELSEIF wa_carga_5720-status EQ 1.
      wa_carga_5720-icone = '@5B@'.
    ELSEIF wa_carga_5720-status EQ 2.
      wa_carga_5720-icone = '@0Q@'.
    ENDIF.

    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_carga_5720-cod_ce.
    IF sy-subrc IS INITIAL.
      wa_carga_5720-name1 = wa_lfa1-name1.
    ENDIF.

    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_carga_5720-cod_tr.
    IF sy-subrc IS INITIAL.
      wa_carga_5720-name2 = wa_lfa1-name1.
    ENDIF.

    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_carga_5720-cod_mt.
    IF sy-subrc IS INITIAL.
      wa_carga_5720-name3 = wa_lfa1-name1.
    ENDIF.

    READ TABLE it_zsdt0140 INTO wa_zsdt0140 WITH KEY nro_cgd = wa_carga_5720-nro_cgd.
    IF sy-subrc IS INITIAL.
      wa_carga_5720-inco1 = wa_zsdt0140-inco1.
    ENDIF.

    MODIFY it_carga_5720 FROM wa_carga_5720 INDEX vl_cont.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_LOTE
*&---------------------------------------------------------------------*
FORM alv_tela_carga_def.

  IF g_custom_container_5720 IS INITIAL.

    CREATE OBJECT g_custom_container_5720
      EXPORTING
        container_name              = 'CONTAINER5720'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT dg_splitter_1_5720
      EXPORTING
        parent  = g_custom_container_5720
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1_5720->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1_5720.

    CALL METHOD dg_splitter_1_5720->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_2_5720.

    CALL METHOD dg_splitter_1_5720->set_row_mode
      EXPORTING
        mode = dg_splitter_1_5720->mode_relative.

    CALL METHOD dg_splitter_1_5720->set_row_height
      EXPORTING
        id     = 1
        height = 50.

    CALL METHOD dg_splitter_1_5720->set_row_height
      EXPORTING
        id     = 2
        height = 50.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_carga_5720 USING:
          01 'ICONE'        ' '         ' '  ' '  'X'  ' '   'X'   ' '   ' '   ' '   'Status',
          02 'NRO_CGD'      'ZSDT0139'  ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nr. Carga',
          03 'INCO1'        'ZSDT0139'  ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Incoterm',
          04 'COD_CE'       'ZSDT0139'  ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Central Emb.',
          05 'NAME1'        ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Central Embalagem',
          06 'COD_TR'       'ZSDT0139'  ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Transp.',
          07 'NAME2'        ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Transportador',
          08 'COD_MT'       'ZSDT0139'  ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Motorista',
          09 'NAME3'        ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Motorista',
          10 'PLACA_CAV'    'ZSDT0139'  ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Cavalo',
          11 'PLACA_CAR1'   'ZSDT0139'  ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Carreta I',
          12 'PLACA_CAR2'   'ZSDT0139'  ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Carreta II',
*-CS2019001891 - JT - 04.02.2021 - inicio
          13 'PLACA_CAR3'   'ZSDT0139'  ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Dolly',
*-CS2019001891 - JT - 04.02.2021 - fim
          14 'DATA_ATUAL'   'ZSDT0139'  ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Data Carga'.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_sol_5720 USING:
           01 'NRO_SOL'       'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Sol',
           02 'VKBUR'         'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Esc. Vendas',
           03 'KUNNR'         'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Cliente',
           04 'NAME1'         'KNA1'         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nome Cliente',
           05 'AUART'         'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Tp. OV',
           06 'VBELN'         'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Ordem Venda',
           07 'POSNR'         'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Item',
           08 'MATNR'         'MARA'         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód Produto',
           09 'MAKTX'         'MAKT'         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Descrição Produto',
           10 'QTE_LIB'       'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'QTD',
           11 'MEINS'         'ZSDT0131'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'U.M.',
           12 'INCO1'         ' '            ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Tp. Frt.',
           13 'NR_ROT'        'ZSDT0082'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Roteiro'.

    gs_layout_5720_alv1-sel_mode   = 'A'.
    gs_layout_5720_alv1-stylefname = 'CELLSTYLES'.
    gs_layout_5720_alv1-cwidth_opt = 'X'.
    gs_layout_5720_alv1-info_fname = 'COR'.
    gs_layout_5720_alv1-smalltitle = 'X'.
    gs_layout_5720_alv1-grid_title = 'Cargas'.
    gs_layout_5720_alv2-sel_mode   = 'A'.
    gs_layout_5720_alv2-stylefname = 'CELLSTYLES'.
    gs_layout_5720_alv2-cwidth_opt = 'X'.
    gs_layout_5720_alv2-info_fname = 'COR'.
    gs_layout_5720_alv2-smalltitle = 'X'.
    gs_layout_5720_alv2-grid_title = 'Ordens de Venda da Carga'.

    PERFORM sort USING 'NRO_CGD' CHANGING it_sort_carga_5720.
    PERFORM sort USING 'NRO_SOL' CHANGING it_sort_sol_5720.

    CREATE OBJECT ctl_alv1_5720
      EXPORTING
        i_parent = dg_parent_1_5720.           "ALV Lote

    CREATE OBJECT ctl_alv2_5720
      EXPORTING
        i_parent = dg_parent_2_5720.           "ALV Oderm

    SET HANDLER:
      lcl_event_handler_5720=>on_double_click_5720 FOR ctl_alv1_5720,
      lcl_event_handler_5720=>user_command_5720 FOR ctl_alv1_5720,
      lcl_event_handler_5720=>toolbar_5720 FOR ctl_alv1_5720.

    CALL METHOD ctl_alv1_5720->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_5720_alv1
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_carga_5720
        it_outtab       = it_carga_5720
        it_sort         = it_sort_carga_5720.

    CALL METHOD ctl_alv2_5720->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_5720_alv2
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_sol_5720
        it_outtab       = it_sol_5720 "IT_SOLICITACAO_5720_CLICK.
        it_sort         = it_sort_sol_5720.

  ELSE.

    CALL METHOD ctl_alv1_5720->get_frontend_layout
      IMPORTING
        es_layout = gs_layout_5720_alv1.

    gs_layout_5720_alv1-cwidth_opt = abap_true.

    CALL METHOD ctl_alv1_5720->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_5720_alv1.

    CALL METHOD ctl_alv1_5720->refresh_table_display
      EXPORTING
        is_stable = _stable.
    CALL METHOD ctl_alv2_5720->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CLEAR_5720
*&---------------------------------------------------------------------*
FORM clear_5720.
  CLEAR: it_sol_5720.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GRAVAR TABELA TRANSPORTE
** Este FOEM é chamado no metodo GRAVAR_ROMANEIO_FERTILIZANTE
*  da classe ZCL_NFE_INBOUND
*&---------------------------------------------------------------------*
FORM grava_informacoes_caminhao USING p_nro_sol
                                      p_seq
                                      p_seq_cam
                                      p_filial_resp
                                      p_qte_embarq
                                      p_placa_cav
                                      p_placa_car1
                                      p_placa_car2
                                      p_placa_car3
                                      p_motorista.

  DATA: wa_caminhao_aux_5420 TYPE ty_caminhao_5420,
        wa_caminhao_aux_5520 TYPE ty_caminhao_5520,
        wa_caminhao_aux_5620 TYPE ty_caminhao_5620.

  FREE: it_caminhao_aux_5420,
        it_caminhao_aux_5520,
        it_caminhao_aux_5620.

  CREATE OBJECT zcl_romaneio.

  wa_caminhao_aux_5420-nro_sol     = p_nro_sol.
  wa_caminhao_aux_5420-seq         = p_seq.
  wa_caminhao_aux_5420-seq_cam     = p_seq_cam.
  wa_caminhao_aux_5420-filial_resp = p_filial_resp.
  wa_caminhao_aux_5420-qtd_embarq  = p_qte_embarq.
  wa_caminhao_aux_5420-placa_cav   = p_placa_cav.
*-IR063194 - 21.07.2021 - JT - inicio
  wa_caminhao_aux_5420-placa_car1  = p_placa_car1.
  wa_caminhao_aux_5420-placa_car2  = p_placa_car2.
  wa_caminhao_aux_5420-placa_car3  = p_placa_car3.
  wa_caminhao_aux_5420-motorista   = p_motorista.
*-IR063194 - 21.07.2021 - JT - fim

  APPEND wa_caminhao_aux_5420     TO it_caminhao_aux_5420.

  MOVE-CORRESPONDING wa_caminhao_aux_5420 TO  wa_caminhao_aux_5520.
  APPEND wa_caminhao_aux_5520     TO it_caminhao_aux_5520.

  MOVE-CORRESPONDING wa_caminhao_aux_5420 TO  wa_caminhao_aux_5620.
  APPEND wa_caminhao_aux_5620     TO it_caminhao_aux_5620.

*-----------------------------------------------
*-desativa commit ao salvar romaneio
*-----------------------------------------------
  zcl_romaneio->set_commit( i_not_commit = abap_true ).

ENDFORM.
*-CS2019001896 - 05.01.2021 - fim
