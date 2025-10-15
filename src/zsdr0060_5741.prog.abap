*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5741
*&---------------------------------------------------------------------*

DATA: it_sol_pop_5741  TYPE STANDARD TABLE OF ty_sol_5740.

DATA: g_custom_container_pop_5741 TYPE REF TO cl_gui_custom_container,
      ctl_alv1_pop_5741           TYPE REF TO cl_gui_alv_grid,
      gs_layout_pop_5741          TYPE lvc_s_layo,
      it_fieldcatalog_pop_5741    TYPE lvc_t_fcat,
      it_exclude_pop_5741         TYPE ui_functions.

*-CS2021000218-#105687-02.03.2023-JT-inicio
*&---------------------------------------------------------------------*
*& classes
*&---------------------------------------------------------------------*
CLASS lcl_event_handler_5741 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed       FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,

      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no sender.
ENDCLASS.

CLASS lcl_event_handler_5741 IMPLEMENTATION.
  METHOD on_data_changed.

    DATA: l_value     TYPE lvc_value.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_good_cellsx) WHERE fieldname = 'CHECK'.
      l_value = wa_good_cellsx-value.

      READ TABLE it_sol_pop_5741 INTO DATA(wa_sol_5741) INDEX wa_good_cellsx-row_id.
      wa_sol_5741-cor    = COND #( WHEN l_value = abap_true THEN 'C310'
                                                            ELSE abap_off ).
      MODIFY it_sol_pop_5741   FROM      wa_sol_5741  INDEX wa_good_cellsx-row_id.
    ENDLOOP.

    CALL METHOD ctl_alv1_pop_5741->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDMETHOD.

  METHOD on_hotspot_click.

    CASE e_column_id.
      WHEN 'CHECK'.
        READ TABLE it_sol_pop_5741 INTO DATA(wa_sol_5741) INDEX e_row_id-index.
        wa_sol_5741-check  = COND #( WHEN wa_sol_5741-check = abap_off THEN abap_on
                                                                       ELSE abap_off ).
        wa_sol_5741-cor    = COND #( WHEN wa_sol_5741-check = abap_off THEN abap_off
                                                                       ELSE 'C310' ).
        MODIFY it_sol_pop_5741 FROM wa_sol_5741  INDEX e_row_id-index.
    ENDCASE.

    CALL METHOD ctl_alv1_pop_5741->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDMETHOD.

ENDCLASS.
*-CS2021000218-#105687-02.03.2023-JT-fim

*&---------------------------------------------------------------------*
*&      Module  STATUS_5741  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5741 OUTPUT.

  SET PF-STATUS 'PF5131'.
  SET TITLEBAR  'T5131'.

  PERFORM pesquisa_pop_5741.
  PERFORM mostra_pop_5741.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5741  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5741 INPUT.

  ctl_alv1_pop_5741->check_changed_data( ).

  CASE sy-ucomm.
    WHEN 'SALVAR'.

      DATA: it_sol_pop_5741_check TYPE STANDARD TABLE OF ty_sol_5740,
            wa_sol_5740           TYPE ty_sol_5740,
            wa_sol_5740_check     TYPE ty_sol_5740,
            vl_lines_             TYPE i.

      PERFORM desbloqueia_sol_pop_5741 USING ' ' ' '.
      DELETE it_sol_pop_5741 WHERE check NE abap_true OR cor EQ 'C600'.
      it_sol_pop_5741_check = it_sol_pop_5741.
      SORT it_sol_pop_5741_check BY vbeln nr_rot inco1.
      DELETE ADJACENT DUPLICATES FROM it_sol_pop_5741_check COMPARING vbeln nr_rot inco1.
      DESCRIBE TABLE it_sol_pop_5741_check LINES vl_lines_.
      IF vl_lines_ NE 1.
        IF vl_lines EQ 0.
          MESSAGE text-101 TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          MESSAGE text-068 TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
        PERFORM desbloqueia_sol_pop_5741 USING 'X' 'X'.
      ELSE.
        READ TABLE it_sol_5740 INTO wa_sol_5740 INDEX 1.
        IF sy-subrc IS INITIAL.
          READ TABLE it_sol_pop_5741_check INTO wa_sol_5740_check INDEX 1.
          IF wa_sol_5740-vbeln  EQ wa_sol_5740_check-vbeln AND
             wa_sol_5740-nr_rot EQ wa_sol_5740_check-nr_rot AND
             wa_sol_5740-inco1  EQ wa_sol_5740_check-inco1.
            APPEND LINES OF it_sol_pop_5741 TO it_sol_5740.
          ELSE.
            MESSAGE text-068 TYPE 'S' DISPLAY LIKE 'E'.
            PERFORM desbloqueia_sol_pop_5741 USING 'X' 'X'.
          ENDIF.
        ELSE.
          APPEND LINES OF it_sol_pop_5741 TO it_sol_5740.
        ENDIF.


      ENDIF.

      CALL METHOD ctl_alv1_5740->get_frontend_layout
        IMPORTING
          es_layout = gs_layout_5740.

      gs_layout_5740-cwidth_opt = abap_true.

      CALL METHOD ctl_alv1_5740->set_frontend_layout
        EXPORTING
          is_layout = gs_layout_5740.

      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      PERFORM desbloqueia_sol_pop_5741 USING 'X' ' '.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_POP_5741
*&---------------------------------------------------------------------*
FORM pesquisa_pop_5741.

  DATA: it_vbkd         TYPE STANDARD TABLE OF vbkd,
        it_makt         TYPE STANDARD TABLE OF makt,
        it_vbpa         TYPE STANDARD TABLE OF vbpa,
        it_kna1         TYPE STANDARD TABLE OF kna1,
        wa_vbkd         TYPE vbkd,
        wa_makt         TYPE makt,
        wa_vbpa         TYPE vbpa,
        wa_kna1         TYPE kna1,
        vl_cont         TYPE i,
        wa_sol_pop_5741 TYPE ty_sol_5740,
        wa_zsdt0131     TYPE zsdt0131,
        vl_lifnr        TYPE zsdt0132-lifnr,
        vl_cod_loc_emb  TYPE zsdt0131-cod_loc_emb,
        vl_local_embarq TYPE zsdt0131-local_embarq,
        chave           TYPE zde_chave_sol.

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
      INTO CORRESPONDING FIELDS OF TABLE it_sol_pop_5741
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


  IF it_sol_pop_5741 IS NOT INITIAL.

    SELECT *
      FROM makt
      INTO TABLE it_makt
      FOR ALL ENTRIES IN it_sol_pop_5741
      WHERE matnr EQ it_sol_pop_5741-matnr.

    SELECT *
      FROM vbpa
      INTO TABLE it_vbpa
      FOR ALL ENTRIES IN it_sol_pop_5741
      WHERE vbeln EQ it_sol_pop_5741-vbeln
        AND parvw EQ 'AG'.

    IF it_vbpa IS NOT INITIAL.

      SELECT *
            FROM kna1
            INTO TABLE it_kna1
            FOR ALL ENTRIES IN it_vbpa
            WHERE kunnr EQ it_vbpa-kunnr.

    ENDIF.

  ENDIF.

  LOOP AT it_sol_pop_5741 INTO wa_sol_pop_5741.

    vl_cont = vl_cont + 1.

    READ TABLE it_sol_5740 WITH KEY nro_sol = wa_sol_pop_5741-nro_sol
                                    seq     = wa_sol_pop_5741-seq TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      wa_sol_pop_5741-usuario_sol = '999'.        "Para eliminar o que já está na tela 5130
    ENDIF.

    IF wa_sol_pop_5741-usuario_sol NE '999'.

      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_sol_pop_5741-matnr.
      IF sy-subrc IS INITIAL.
        wa_sol_pop_5741-maktx = wa_makt-maktx.
      ENDIF.

      READ TABLE it_vbpa INTO wa_vbpa WITH KEY vbeln = wa_sol_pop_5741-vbeln.
      IF sy-subrc IS INITIAL.
        wa_sol_pop_5741-kunnr = wa_vbpa-kunnr.
        READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbpa-kunnr.
        IF sy-subrc IS INITIAL.
          wa_sol_pop_5741-name1 = wa_kna1-name1.
        ENDIF.
      ENDIF.

      CONCATENATE wa_sol_pop_5741-nro_sol wa_sol_pop_5741-seq wa_sol_pop_5741-vbeln wa_sol_pop_5741-posnr INTO chave.

      CALL FUNCTION 'ZENQUEUE_SD_SOL_INSUMOS'
        EXPORTING
          chave          = chave
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc <> 0.
        wa_sol_pop_5741-cor = 'C600'.
      ENDIF.

      CLEAR: vl_qtd_usada, chave.

    ENDIF.

    MODIFY it_sol_pop_5741 FROM wa_sol_pop_5741 INDEX vl_cont.

  ENDLOOP.

  DELETE it_sol_pop_5741 WHERE usuario_sol EQ '999'.
  CLEAR: vl_cont.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_POP
*&---------------------------------------------------------------------*
FORM mostra_pop_5741 .

  IF g_custom_container_pop_5741 IS INITIAL.

    CREATE OBJECT g_custom_container_pop_5741
      EXPORTING
        container_name              = 'CONTAINER5741'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_pop_5741 USING:
          01 'CHECK'          ''         ' '  'X' ' '  ' '   'X'   'X'   ' '   ' '   'Nro Lote',
          02 'NRO_SOL'        'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Nro Sol',
          03 'KUNNR'          'VBPA'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Cliente',
          04 'NAME1'          'KNA1'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Desc. Cliente',
          05 'VBELN'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Ordem',
          06 'POSNR'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Item',
          07 'AUART'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Tipo OV',
          08 'INCO1'          'VBKD'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Incoterm',
          09 'VKORG'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Org. Vendas',
          10 'SPART'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Setor Atv.',
          11 'WERKS'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Centro Forn',
          12 'MATNR'          'MARA'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Material',
          13 'MAKTX'          'MAKT'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Descrição',
          14 'QTE_LIB'        ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Saldo à Formar Lote',
          15 'MEINS'          'VBAP'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'U.M.',
          16 'NR_ROT'         'ZSDT0132' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Roteiro'.

    gs_layout_pop_5741-sel_mode   = 'A'.
    gs_layout_pop_5741-stylefname = 'CELLSTYLES'.
    gs_layout_pop_5741-cwidth_opt = 'X'.
    gs_layout_pop_5741-info_fname = 'COR'.

    PERFORM excluir_botoes CHANGING it_exclude_pop_5741.

    CREATE OBJECT ctl_alv1_pop_5741
      EXPORTING
        i_parent = g_custom_container_pop_5741.           "ALV Lote

*-CS2021000218-#105687-02.03.2023-JT-inicio
    SET HANDLER: lcl_event_handler_5741=>on_hotspot_click FOR ctl_alv1_pop_5741,
                 lcl_event_handler_5741=>on_data_changed  FOR ctl_alv1_pop_5741.
*-CS2021000218-#105687-02.03.2023-JT-fim

    CALL METHOD ctl_alv1_pop_5741->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_pop_5741
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_pop_5741
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_pop_5741
        it_outtab            = it_sol_pop_5741.

*-CS2021000218-#105687-02.03.2023-JT-inicio
    CALL METHOD ctl_alv1_pop_5741->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
*-CS2021000218-#105687-02.03.2023-JT-fim

  ELSE.
    CALL METHOD ctl_alv1_pop_5741->refresh_table_display
      EXPORTING
        is_stable = _stable.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5741_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5741_exit INPUT.
  PERFORM desbloqueia_sol_pop_5741 USING 'X' ' '.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  DESBLOQUEIA_SOL_POP_5741
*&---------------------------------------------------------------------*
FORM desbloqueia_sol_pop_5741 USING VALUE(p_01)
                                    VALUE(p_02).

  DATA: wa_sol_pop_5741 TYPE ty_sol_5740,
        chave_exit_5741 TYPE zde_chave_sol.

  LOOP AT it_sol_pop_5741 INTO wa_sol_pop_5741 WHERE cor IS INITIAL
                                                 AND ( check EQ p_01 OR
                                                       check EQ p_02 ) .

    CONCATENATE wa_sol_pop_5741-nro_sol wa_sol_pop_5741-seq wa_sol_pop_5741-vbeln wa_sol_pop_5741-posnr INTO chave_exit_5741.

    CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
      EXPORTING
        chave = chave_exit_5741.

  ENDLOOP.

ENDFORM.
