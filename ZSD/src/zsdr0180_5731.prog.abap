*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5731
*&---------------------------------------------------------------------*

DATA: it_sol_pop_5731  TYPE STANDARD TABLE OF ty_solicitacao_5730.

DATA: g_custom_container_pop_5731 TYPE REF TO cl_gui_custom_container,
      ctl_alv1_pop_5731           TYPE REF TO cl_gui_alv_grid,
      gs_layout_pop_5731          TYPE lvc_s_layo,
      it_fieldcatalog_pop_5731    TYPE lvc_t_fcat,
      it_exclude_pop_5731         TYPE ui_functions.

*-CS2021000218-#105687-02.03.2023-JT-inicio
*&---------------------------------------------------------------------*
*& classes
*&---------------------------------------------------------------------*
CLASS lcl_event_handler_5731 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed       FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,

      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no sender.
ENDCLASS.

CLASS lcl_event_handler_5731 IMPLEMENTATION.
  METHOD on_data_changed.

    DATA: l_value     TYPE lvc_value.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_good_cellsx) WHERE fieldname = 'CHECK'.
      l_value = wa_good_cellsx-value.

      READ TABLE it_sol_pop_5731 INTO DATA(wa_sol_5731) INDEX wa_good_cellsx-row_id.
      wa_sol_5731-cor    = COND #( WHEN l_value = abap_true THEN 'C310'
                                                            ELSE abap_off ).
      MODIFY it_sol_pop_5731   FROM      wa_sol_5731  INDEX wa_good_cellsx-row_id.
    ENDLOOP.

    CALL METHOD ctl_alv1_pop_5731->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDMETHOD.

  METHOD on_hotspot_click.

    CASE e_column_id.
      WHEN 'CHECK'.
        READ TABLE it_sol_pop_5731 INTO DATA(wa_sol_5731) INDEX e_row_id-index.
        wa_sol_5731-check  = COND #( WHEN wa_sol_5731-check = abap_off THEN abap_on
                                                                       ELSE abap_off ).
        wa_sol_5731-cor    = COND #( WHEN wa_sol_5731-check = abap_off THEN abap_off
                                                                       ELSE 'C310' ).
        MODIFY it_sol_pop_5731 FROM wa_sol_5731  INDEX e_row_id-index.
    ENDCASE.

    CALL METHOD ctl_alv1_pop_5731->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDMETHOD.

ENDCLASS.
*-CS2021000218-#105687-02.03.2023-JT-fim

*&---------------------------------------------------------------------*
*&      Module  STATUS_5731  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5731 OUTPUT.

  SET PF-STATUS 'PF5131'.
  SET TITLEBAR  'T5131'.

  PERFORM pesquisa_pop_5731.
  PERFORM mostra_pop_5731.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5731  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5731 INPUT.

  DATA: it_sol_pop_5731_check TYPE STANDARD TABLE OF ty_solicitacao_5730,
        wa_sol_5730           TYPE ty_solicitacao_5730,
        wa_sol_5730_check     TYPE ty_solicitacao_5730,
        vl_lines              TYPE i.

  ctl_alv1_pop_5731->check_changed_data( ).

  CASE sy-ucomm.
    WHEN 'SALVAR'.

      PERFORM desbloqueia_sol_pop_5731 USING ' ' ' '.
      DELETE it_sol_pop_5731 WHERE check NE abap_true OR cor EQ 'C600'.

      it_sol_pop_5731_check = it_sol_pop_5731.
      SORT it_sol_pop_5731_check BY vbeln nr_rot inco1.
      DELETE ADJACENT DUPLICATES FROM it_sol_pop_5731_check COMPARING vbeln nr_rot inco1.
      DESCRIBE TABLE it_sol_pop_5731_check LINES vl_lines.

      IF vl_lines NE 1.
        IF vl_lines EQ 0.
          MESSAGE TEXT-101 TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          MESSAGE TEXT-068 TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
        PERFORM desbloqueia_sol_pop_5731 USING 'X' 'X'.
      ELSE.
        READ TABLE it_sol_5730 INTO wa_sol_5730 INDEX 1.
        IF sy-subrc IS INITIAL.
          READ TABLE it_sol_pop_5731_check INTO wa_sol_5730_check INDEX 1.
          IF wa_sol_5730-vbeln  EQ wa_sol_5730_check-vbeln AND
             wa_sol_5730-nr_rot EQ wa_sol_5730_check-nr_rot AND
             wa_sol_5730-inco1  EQ wa_sol_5730_check-inco1.

            APPEND LINES OF it_sol_pop_5731 TO it_sol_5730.

            CALL METHOD ctl_alv1_5730->get_frontend_layout
              IMPORTING
                es_layout = gs_layout_5730.

            gs_layout_5730-cwidth_opt = abap_true.

            CALL METHOD ctl_alv1_5730->set_frontend_layout
              EXPORTING
                is_layout = gs_layout_5730.

            LEAVE TO SCREEN 0.

          ELSE.
            MESSAGE TEXT-068 TYPE 'S' DISPLAY LIKE 'E'.
            PERFORM desbloqueia_sol_pop_5741 USING 'X' 'X'.
          ENDIF.
*          MESSAGE TEXT-068 TYPE 'S' DISPLAY LIKE 'E'.
*          PERFORM DESBLOQUEIA_SOL_POP_5731 USING 'X' 'X'.
        ELSE.

          APPEND LINES OF it_sol_pop_5731 TO it_sol_5730.

          CALL METHOD ctl_alv1_5730->get_frontend_layout
            IMPORTING
              es_layout = gs_layout_5730.

          gs_layout_5730-cwidth_opt = abap_true.

          CALL METHOD ctl_alv1_5730->set_frontend_layout
            EXPORTING
              is_layout = gs_layout_5730.

          LEAVE TO SCREEN 0.

        ENDIF.
        PERFORM desbloqueia_sol_pop_5731 USING 'X' 'X'.
      ENDIF.

    WHEN 'CANCEL'.
      PERFORM desbloqueia_sol_pop_5731 USING 'X' ' '.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_POP_5731
*&---------------------------------------------------------------------*
FORM pesquisa_pop_5731.

  DATA: it_vbkd         TYPE STANDARD TABLE OF vbkd,
        it_makt         TYPE STANDARD TABLE OF makt,
        it_vbpa         TYPE STANDARD TABLE OF vbpa,
        it_kna1         TYPE STANDARD TABLE OF kna1,
        wa_vbkd         TYPE vbkd,
        wa_makt         TYPE makt,
        wa_vbpa         TYPE vbpa,
        wa_kna1         TYPE kna1,
        vl_cont         TYPE i,
        wa_sol_pop_5731 TYPE ty_solicitacao_5730,
        wa_zsdt0131     TYPE zsdt0131,
        vl_lifnr        TYPE zsdt0132-lifnr,
        vl_cod_loc_emb  TYPE zsdt0131-cod_loc_emb,
        vl_local_embarq TYPE zsdt0131-local_embarq,
        chave           TYPE zde_chave_sol,
        lv_preco_unit   TYPE konv-kwert,
        lv_vlr_dolar    TYPE konv-kwert.

  SELECT vbap~matnr
         vbkd~inco1
         mara~meins
         zsdt0082~mandt
         zsdt0082~nro_sol
         zsdt0082~seq
         zsdt0082~vbeln
         zsdt0082~posnr
         zsdt0082~vkorg
         zsdt0082~spart
         zsdt0082~vkgrp
         zsdt0082~vkbur
         zsdt0082~auart
         zsdt0082~werks
         zsdt0082~qte_lib
         zsdt0082~status
         zsdt0082~dt_entrega
         zsdt0082~nr_rot
      INTO CORRESPONDING FIELDS OF TABLE it_sol_pop_5731
      FROM zsdt0082
      INNER JOIN vbap ON zsdt0082~vbeln = vbap~vbeln AND
                         zsdt0082~posnr = vbap~posnr
      INNER JOIN mara ON mara~matnr = vbap~matnr
      INNER JOIN vbak ON vbak~vbeln = vbap~vbeln
    INNER JOIN vbkd ON vbak~vbeln = vbkd~vbeln
        WHERE zsdt0082~vkbur  IN d_vkbur
          AND zsdt0082~vkorg  IN d_vkorg
          AND zsdt0082~spart  EQ d_spart
          AND vbak~kunnr IN d_kunnr
          AND zsdt0082~seq    NE 1
          AND zsdt0082~status EQ 2
          AND vbkd~inco1 IN d_inco1.


  IF it_sol_pop_5731 IS NOT INITIAL.

    SELECT *
      FROM makt
      INTO TABLE it_makt
      FOR ALL ENTRIES IN it_sol_pop_5731
      WHERE matnr EQ it_sol_pop_5731-matnr.

    SELECT *
      FROM vbpa
      INTO TABLE it_vbpa
      FOR ALL ENTRIES IN it_sol_pop_5731
      WHERE vbeln EQ it_sol_pop_5731-vbeln
        AND parvw EQ 'AG'.

    IF it_vbpa IS NOT INITIAL.

      SELECT *
            FROM kna1
            INTO TABLE it_kna1
            FOR ALL ENTRIES IN it_vbpa
            WHERE kunnr EQ it_vbpa-kunnr.

    ENDIF.

    DATA(lt_5371) = it_sol_pop_5731.
    SORT lt_5371 BY vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_5371 COMPARING vbeln.

    SELECT vbeln, knumv, waerk
      FROM vbak
      INTO TABLE @DATA(lt_vbak)
      FOR ALL ENTRIES IN @lt_5371
      WHERE vbeln = @lt_5371-vbeln.
    IF sy-subrc IS INITIAL.
      SORT lt_vbak BY vbeln.

      SELECT *
        FROM vbap
        INTO TABLE @DATA(lt_vbap)
        FOR ALL ENTRIES IN @lt_vbak
        WHERE vbeln = @lt_vbak-vbeln.
      IF sy-subrc IS INITIAL.
        SORT lt_vbap BY vbeln posnr.
      ENDIF.

      SELECT *
        FROM vbkd
        INTO TABLE @DATA(lt_vbkd)
        FOR ALL ENTRIES IN @lt_vbak
        WHERE vbeln = @lt_vbak-vbeln.
      IF sy-subrc IS INITIAL.
        SORT lt_vbkd BY vbeln.
      ENDIF.

      DATA(lt_vbak_aux) = lt_vbak.
      SORT lt_vbak_aux BY knumv.
      DELETE ADJACENT DUPLICATES FROM lt_vbak_aux COMPARING knumv.

      SELECT *
        FROM v_konv_cds
        INTO TABLE @DATA(lt_konv)
        FOR ALL ENTRIES IN @lt_vbak_aux
        WHERE knumv = @lt_vbak_aux-knumv.
      IF sy-subrc IS INITIAL.
        SORT lt_konv BY knumv kposn kschl.
      ENDIF.
    ENDIF.

  ENDIF.

  LOOP AT it_sol_pop_5731 INTO wa_sol_pop_5731.

    vl_cont = vl_cont + 1.

    READ TABLE it_sol_5730 WITH KEY nro_sol = wa_sol_pop_5731-nro_sol
                                    seq     = wa_sol_pop_5731-seq TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      wa_sol_pop_5731-usuario_sol = '999'.        "Para eliminar o que já está na tela 5130
    ENDIF.

    IF wa_sol_pop_5731-usuario_sol NE '999'.

      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_sol_pop_5731-matnr.
      IF sy-subrc IS INITIAL.
        wa_sol_pop_5731-maktx = wa_makt-maktx.
      ENDIF.

      READ TABLE it_vbpa INTO wa_vbpa WITH KEY vbeln = wa_sol_pop_5731-vbeln.
      IF sy-subrc IS INITIAL.
        wa_sol_pop_5731-kunnr = wa_vbpa-kunnr.
        READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbpa-kunnr.
        IF sy-subrc IS INITIAL.
          wa_sol_pop_5731-name1 = wa_kna1-name1.
        ENDIF.
      ENDIF.

      CONCATENATE wa_sol_pop_5731-nro_sol wa_sol_pop_5731-seq wa_sol_pop_5731-vbeln wa_sol_pop_5731-posnr INTO chave.

      CALL FUNCTION 'ZENQUEUE_SD_SOL_INSUMOS'
        EXPORTING
          chave          = chave
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc <> 0.
        wa_sol_pop_5731-cor = 'C600'.
      ENDIF.

      CLEAR: vl_qtd_usada, chave.

    ENDIF.

    READ TABLE lt_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>)
    WITH KEY vbeln = wa_sol_pop_5731-vbeln
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      wa_sol_pop_5731-moeda = <fs_vbak>-waerk.

      READ TABLE lt_vbap ASSIGNING FIELD-SYMBOL(<fs_vbap>)
      WITH KEY vbeln = <fs_vbak>-vbeln
               posnr = wa_sol_pop_5731-posnr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        READ TABLE lt_konv ASSIGNING FIELD-SYMBOL(<fs_konv>)
        WITH KEY knumv = <fs_vbak>-knumv
                 kposn = wa_sol_pop_5731-posnr
                 kschl = 'ICMI'
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          IF <fs_vbak>-waerk = 'USD'.

            IF <fs_konv>-kmein EQ wa_sol_pop_5731-meins.
              lv_preco_unit = <fs_konv>-kwert / <fs_vbap>-kwmeng.
              lv_vlr_dolar  = lv_preco_unit * wa_sol_pop_5731-qte_lib.

              READ TABLE lt_vbkd ASSIGNING FIELD-SYMBOL(<fs_vbkd>)
              WITH KEY vbeln = <fs_vbak>-vbeln
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                IF <fs_vbkd>-kurrf IS NOT INITIAL.

                  wa_sol_pop_5731-valor_brl = lv_vlr_dolar * <fs_vbkd>-kurrf.

                ELSEIF <fs_vbkd>-kursk IS NOT INITIAL.

                  wa_sol_pop_5731-valor_brl = lv_vlr_dolar * <fs_vbkd>-kursk.

                ENDIF.

              ENDIF.

            ELSEIF <fs_konv>-kmein EQ 'TO' AND wa_sol_pop_5731-meins EQ 'KG'.

              DATA(lv_vlr_to_dolar) = wa_sol_pop_5731-qte_lib * <fs_konv>-kwert.
              lv_vlr_dolar = lv_vlr_to_dolar / 1000.

              READ TABLE lt_vbkd ASSIGNING <fs_vbkd>
              WITH KEY vbeln = <fs_vbak>-vbeln
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                IF <fs_vbkd>-kurrf IS NOT INITIAL.

                  wa_sol_pop_5731-valor_brl = lv_vlr_dolar * <fs_vbkd>-kurrf.

                ELSEIF <fs_vbkd>-kursk IS NOT INITIAL.

                  wa_sol_pop_5731-valor_brl = lv_vlr_dolar * <fs_vbkd>-kursk.

                ENDIF.

              ENDIF.

            ELSEIF <fs_konv>-kmein EQ 'KG' AND wa_sol_pop_5731-meins EQ 'TO'.

              DATA(lv_vlr_kg_dolar) = wa_sol_pop_5731-qte_lib * <fs_konv>-kwert.
              lv_vlr_to_dolar = lv_vlr_kg_dolar * 1000.

              READ TABLE lt_vbkd ASSIGNING <fs_vbkd>
              WITH KEY vbeln = <fs_vbak>-vbeln
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                IF <fs_vbkd>-kurrf IS NOT INITIAL.

                  wa_sol_pop_5731-valor_brl = lv_vlr_dolar * <fs_vbkd>-kurrf.

                ELSEIF <fs_vbkd>-kursk IS NOT INITIAL.

                  wa_sol_pop_5731-valor_brl = lv_vlr_dolar * <fs_vbkd>-kursk.

                ENDIF.

              ENDIF.

            ENDIF.

          ELSEIF <fs_vbak>-waerk = 'BRL'.

            IF <fs_konv>-kmein EQ wa_sol_pop_5731-meins.

              lv_preco_unit = <fs_konv>-kwert / <fs_vbap>-kwmeng.
              wa_sol_pop_5731-valor_brl = lv_preco_unit * wa_sol_pop_5731-qte_lib.


            ELSEIF <fs_konv>-kmein EQ 'TO' AND wa_sol_pop_5731-meins EQ 'KG'.

              lv_vlr_to_dolar = wa_sol_pop_5731-qte_lib * <fs_konv>-kwert.
              wa_sol_pop_5731-valor_brl = lv_vlr_to_dolar / 1000.


            ELSEIF <fs_konv>-kmein EQ 'KG' AND wa_sol_pop_5731-meins EQ 'TO'.

              lv_vlr_kg_dolar = wa_sol_pop_5731-qte_lib * <fs_konv>-kwert.
              wa_sol_pop_5731-valor_brl = lv_vlr_kg_dolar * 1000.

            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    MODIFY it_sol_pop_5731 FROM wa_sol_pop_5731 INDEX vl_cont.

  ENDLOOP.

  DELETE it_sol_pop_5731 WHERE usuario_sol EQ '999'.
  CLEAR: vl_cont.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_POP
*&---------------------------------------------------------------------*
FORM mostra_pop_5731 .

  IF g_custom_container_pop_5731 IS INITIAL.

    CREATE OBJECT g_custom_container_pop_5731
      EXPORTING
        container_name              = 'CONTAINER5731'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

*-CS2021000218-BUG 99240-22.12.2022-JT-inicio
    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_pop_5731 USING:
          01 'CHECK'          ''         ' '  'X' ' '  ' '   'X'   'X'   ' '   ' '   'Nro Lote',
          02 'NRO_SOL'        'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Nro Sol',
          03 'VBELN'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Ordem',
          04 'NAME1'          'KNA1'     ' '  ' ' ' '  ' '   ''   ' '   ' '   ' '   'Desc. Cliente',
          05 'WERKS'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Centro Forn',
          06 'INCO1'          'VBKD'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Incoterm',
          07 'MATNR'          'MARA'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Material',
          08 'MAKTX'          'MAKT'     ' '  ' ' ' '  ' '   ''   ' '   ' '   ' '   'Descrição',
          09 'QTE_LIB'        ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Saldo à Formar Lote',
          10 'VALOR_BRL'      ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Valor BRL',
          11 'NR_ROT'         'ZSDT0132' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Roteiro',
          12 'KUNNR'          'VBPA'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Cliente',
          13 'POSNR'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Item',
          14 'MOEDA'          ''         ' '  ' ' ' '  ' '   ''   ' '   ' '   ' '   'Moeda do Documento'.
*          14 'AUART'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Tipo OV',
*          15 'VKORG'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Org. Vendas',
*          16 'SPART'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Setor Atv.',
*          17 'MEINS'          'VBAP'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'U.M.'.
*-CS2021000218-BUG 99240-22.12.2022-JT-fim

    gs_layout_pop_5731-sel_mode   = 'A'.
    gs_layout_pop_5731-stylefname = 'CELLSTYLES'.
*    gs_layout_pop_5731-cwidth_opt = 'X'.
    gs_layout_pop_5731-info_fname = 'COR'.

    PERFORM excluir_botoes CHANGING it_exclude_pop_5731.

    CREATE OBJECT ctl_alv1_pop_5731
      EXPORTING
        i_parent = g_custom_container_pop_5731.           "ALV Lote

*-CS2021000218-#105687-02.03.2023-JT-inicio
    SET HANDLER: lcl_event_handler_5731=>on_hotspot_click FOR ctl_alv1_pop_5731,
                 lcl_event_handler_5731=>on_data_changed  FOR ctl_alv1_pop_5731.
*-CS2021000218-#105687-02.03.2023-JT-fim

    CALL METHOD ctl_alv1_pop_5731->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_pop_5731
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_pop_5731
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_pop_5731
        it_outtab            = it_sol_pop_5731.

*-CS2021000218-#105687-02.03.2023-JT-inicio
    CALL METHOD ctl_alv1_pop_5731->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
*-CS2021000218-#105687-02.03.2023-JT-fim

  ELSE.
    CALL METHOD ctl_alv1_pop_5731->refresh_table_display
      EXPORTING
        is_stable = _stable.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5731_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5731_exit INPUT.

  PERFORM desbloqueia_sol_pop_5731 USING 'X' ' '.

  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  DESBLOQUEIA_SOL_POP_5731
*&---------------------------------------------------------------------*
FORM desbloqueia_sol_pop_5731 USING VALUE(p_01)
                                    VALUE(p_02).

  DATA: wa_sol_pop_5731 TYPE ty_solicitacao_5730,
        chave_exit_5731 TYPE zde_chave_sol.

  LOOP AT it_sol_pop_5731 INTO wa_sol_pop_5731 WHERE cor IS INITIAL
                                                 AND ( check EQ p_01 OR
                                                       check EQ p_02 ) .

    CONCATENATE wa_sol_pop_5731-nro_sol wa_sol_pop_5731-seq wa_sol_pop_5731-vbeln wa_sol_pop_5731-posnr INTO chave_exit_5731.

    CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
      EXPORTING
        chave = chave_exit_5731.

  ENDLOOP.

ENDFORM.
