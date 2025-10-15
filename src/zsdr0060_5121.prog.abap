*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5121
*&---------------------------------------------------------------------*

DATA: g_custom_container_pop_5121 TYPE REF TO cl_gui_custom_container,
      ctl_alv1_pop_5121           TYPE REF TO cl_gui_alv_grid,
      gs_layout_pop_5121          TYPE lvc_s_layo,
      it_fieldcatalog_pop_5121    TYPE lvc_t_fcat,
      it_exclude_pop_5121         TYPE ui_functions.

TYPES: BEGIN OF ty_cliente_5121,
         name1      TYPE kna1-name1,
         obs        TYPE char6,
         cellstyles TYPE lvc_t_styl,
         nr_rot1    TYPE zsdt0130-nr_rot,
         nr_rot2    TYPE zsdt0130-nr_rot.
         INCLUDE    STRUCTURE zsdt0130.
TYPES: END OF ty_cliente_5121.

DATA: it_cliente_pop_5121 TYPE STANDARD TABLE OF ty_cliente_5121,
      it_kna1_5121        TYPE STANDARD TABLE OF kna1,
      wa_kna1_5121        TYPE kna1.

DATA: wa_dados_carga-dt_entrega TYPE zsdt0129-dt_entrega.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler_5121 DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      handle_hotspot_click_5121  FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,

      handle_button_click_5121 FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no,

      data_changed_finished_5121 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler_5121 IMPLEMENTATION.

  METHOD handle_hotspot_click_5121.

    DATA: wa_cliente_5120 TYPE ty_cliente_5120.

    DATA: wa_bor TYPE borident.

    DATA: wl_name  TYPE thead-tdname.

    IF e_column_id-fieldname EQ 'NR_ROT2'.

      READ TABLE it_cliente_pop_5121 INTO wa_cliente_5120 INDEX e_row_id-index.
      IF sy-subrc IS INITIAL.
        wa_bor-objkey = wa_cliente_5120-nr_rot2.
        wa_bor-objtype = 'ZSDR0061'.
        PERFORM chama_anexo USING wa_bor.
      ENDIF.

    ELSEIF e_column_id-fieldname EQ 'NR_ROT1'.

      READ TABLE it_cliente_pop_5121 INTO wa_cliente_5120 INDEX e_row_id-index.
      IF sy-subrc IS INITIAL.
        wl_name = wa_cliente_5120-nr_rot1.
        PERFORM chama_texto_rot USING wl_name.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    "HANDEL_HOTSPOT_CLICK

  METHOD handle_button_click_5121.

    DATA: wa_cliente_5120 TYPE ty_cliente_5120.

    DATA: wl_name  TYPE thead-tdname.

    IF es_col_id-fieldname EQ 'OBS'.

      READ TABLE it_cliente_pop_5121 INTO wa_cliente_5120 INDEX es_row_no-row_id.
      IF sy-subrc IS INITIAL.
        CONCATENATE wa_cliente_5120-nro_sol '001' INTO wl_name.
        PERFORM chama_texto_obs USING wl_name.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD data_changed_finished_5121.

    DATA: wa_good_cells TYPE lvc_s_modi.

    READ TABLE et_good_cells INTO wa_good_cells INDEX 1.

    IF e_modified EQ abap_true.
      IF wa_good_cells-fieldname EQ 'SEQ_ENT_CG'.
        PERFORM atualiza_seq_entrega_5121 USING wa_good_cells.
        vg_subt_lote = '5121'.
        LEAVE TO SCREEN 5000.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_5121  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5121 OUTPUT.

  SET PF-STATUS 'PF5131'.
  SET TITLEBAR  'T5121'.

  PERFORM mostra_pop_5121.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5121  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5121 INPUT.

  ctl_alv1_pop_5121->check_changed_data( ).

  CASE sy-ucomm.
    WHEN 'SALVAR'.
      CLEAR: vl_check.
      PERFORM check_parametro_5121 CHANGING vl_check.
      IF vl_check IS INITIAL.
        PERFORM salva_carga_5121.
        PERFORM desbloqueia_montagem_5121.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANCEL'.
      PERFORM desbloqueia_montagem_5121.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_POP
*&---------------------------------------------------------------------*
FORM mostra_pop_5121.

  FIELD-SYMBOLS: <wa_fieldcatalog_pop_5121> TYPE lvc_s_fcat.

  IF g_custom_container_pop_5121 IS INITIAL.

    CREATE OBJECT g_custom_container_pop_5121
      EXPORTING
        container_name              = 'CONTAINER5121'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_pop_5121 USING:
          01 'NRO_LOTE'       'ZSDT0129_ALV' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Nro Lote',
          02 'NRO_SOL'        'ZSDT0130'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Nro Sol',
          03 'KUNNR'          'ZSDT0130'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Cliente',
          04 'NAME1'          ''             ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Nome'.

    IF vg_unico_lote EQ 1.
      PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_pop_5121 USING:
        05 'SEQ_ENT_CG'    'ZSDT0130'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Seq. Entrega'.
    ELSE.
      PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_pop_5121 USING:
        05 'SEQ_ENT_CG'    'ZSDT0130'     ' '  'X' ' '  ' '   'X'   ' '   ' '   ' '   'Seq. Entrega'.
    ENDIF.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_pop_5121 USING:
          06 'NR_ROT1'        'ZSDT0130'     ' '  ' ' ' '  'X'   'X'   ' '   ' '   ' '   'Roteiro',
          07 'OBS'            ''             ' '  ' ' 'X'  ' '   'X'   ' '   ' '   ' '   'Observação',
          08 'NR_ROT2'        'ZSDT0130'     ' '  ' ' ' '  'X'   'X'   ' '   ' '   ' '   'Doc Anexo'.

    gs_layout_pop_5121-sel_mode   = 'A'.
    gs_layout_pop_5121-stylefname = 'CELLSTYLES'.
    gs_layout_pop_5121-cwidth_opt = 'X'.

    PERFORM excluir_botoes CHANGING it_exclude_pop_5121.

    CREATE OBJECT ctl_alv1_pop_5121
      EXPORTING
        i_parent = g_custom_container_pop_5121.           "ALV Lote

    SET HANDLER:
      lcl_event_handler_5121=>handle_hotspot_click_5121 FOR ctl_alv1_pop_5121,
      lcl_event_handler_5121=>handle_button_click_5121 FOR ctl_alv1_pop_5121.

    CALL METHOD ctl_alv1_pop_5121->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_pop_5121
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_pop_5121
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_pop_5121
        it_outtab            = it_cliente_pop_5121.

    CALL METHOD ctl_alv1_pop_5121->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

  ELSE.

    CALL METHOD ctl_alv1_pop_5121->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = it_fieldcatalog_pop_5121.

    READ TABLE it_fieldcatalog_pop_5121 ASSIGNING <wa_fieldcatalog_pop_5121> WITH KEY fieldname = 'SEQ_ENT_CG'.
    IF sy-subrc IS INITIAL.
      IF vg_unico_lote EQ 1.
        CLEAR: <wa_fieldcatalog_pop_5121>-edit.
      ELSE.
        <wa_fieldcatalog_pop_5121>-edit = 'X'.
      ENDIF.
    ENDIF.

    CALL METHOD ctl_alv1_pop_5121->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fieldcatalog_pop_5121.

    CALL METHOD ctl_alv1_pop_5121->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5121_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5121_exit INPUT.

  PERFORM desbloqueia_montagem_5121.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_POP_5121
*&---------------------------------------------------------------------*
FORM pesquisa_pop_5121 .

  SELECT *
    FROM zsdt0130
    INTO CORRESPONDING FIELDS OF TABLE it_cliente_pop_5121
    FOR ALL ENTRIES IN it_lote_5120_check
    WHERE nro_lote EQ it_lote_5120_check-nro_lote
      AND status NE 'X'.

  IF it_cliente_pop_5121 IS NOT INITIAL.

    SELECT *
        FROM kna1
        INTO TABLE it_kna1_5121
        FOR ALL ENTRIES IN it_cliente_pop_5121
        WHERE kunnr EQ it_cliente_pop_5121-kunnr.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONSTROI_POP_5121
*&---------------------------------------------------------------------*
FORM constroi_pop_5121.

  DATA: ls_style        TYPE lvc_s_styl,
        wa_lote_5120    TYPE ty_lote_5120,
        wa_cliente_5120 TYPE ty_cliente_5120,
        wa_kna1         TYPE kna1,
        vl_cont         TYPE i.

  DATA: wl_name  TYPE thead-tdname,
        tg_texto TYPE STANDARD TABLE OF tline.

  CLEAR wa_dados_carga-dt_entrega.

  IF vg_unico_lote EQ 1.
    wa_dados_carga-dt_entrega = vg_dt_ent_unico_lote.
  ENDIF.

  LOOP AT it_cliente_pop_5121 INTO wa_cliente_5120.

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

    READ TABLE it_kna1_5121 INTO wa_kna1_5121 WITH KEY kunnr = wa_cliente_5120-kunnr.
    IF sy-subrc IS INITIAL.
      wa_cliente_5120-name1 = wa_kna1_5121-name1.
    ENDIF.

    wa_cliente_5120-seq_ent_cg = wa_cliente_5120-seq_entrega.
    wa_cliente_5120-nr_rot1    = wa_cliente_5120-nr_rot.
    wa_cliente_5120-nr_rot2    = wa_cliente_5120-nr_rot.

    MODIFY it_cliente_pop_5121 FROM wa_cliente_5120 INDEX vl_cont.

  ENDLOOP.

  CLEAR: vl_cont.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_SEQ_ENTREGA_5121
*&---------------------------------------------------------------------*
FORM atualiza_seq_entrega_5121 USING wa_good_cells TYPE lvc_s_modi.

  DATA: vl_roteiro      TYPE zsdt0130-nr_rot,
        wa_cliente_5121 TYPE ty_cliente_5121.

  READ TABLE it_cliente_pop_5121 INTO wa_cliente_5121 INDEX wa_good_cells-row_id.

  vl_roteiro = wa_cliente_5121-nr_rot1.

  LOOP AT it_cliente_pop_5121 INTO wa_cliente_5121 WHERE nr_rot1 EQ vl_roteiro.
    wa_cliente_5121-seq_ent_cg = wa_good_cells-value.
    MODIFY it_cliente_pop_5121 FROM wa_cliente_5121 INDEX sy-tabix.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_PARAMETRO_5121
*&---------------------------------------------------------------------*
FORM check_parametro_5121 CHANGING vl_check.

  DATA: wa_cliente_5121       TYPE ty_cliente_5121,
        it_cliente_5121_check TYPE STANDARD TABLE OF ty_cliente_5121.

  "Check data de entrega em branco
  IF wa_dados_carga-dt_entrega IS INITIAL.
    MESSAGE TEXT-036 TYPE 'S' DISPLAY LIKE 'E'.
    vl_check = abap_true.
  ENDIF.

  SELECT c~*
    FROM zsdt0129 AS a
    INNER JOIN zsdt0131 AS b ON b~nro_lote EQ a~nro_lote
    INNER JOIN zsdt0132 AS c ON c~nr_rot EQ b~cod_loc_emb
      INTO TABLE @DATA(t_0132)
        FOR ALL ENTRIES IN @it_cliente_pop_5121
        WHERE a~nro_lote EQ @it_cliente_pop_5121-nro_lote
          AND b~status NE 'X'.

  IF sy-subrc IS INITIAL.
    DATA(t_0132_aux) = t_0132.
    SORT t_0132_aux BY transp_resp.
    DELETE ADJACENT DUPLICATES FROM t_0132_aux COMPARING transp_resp.
    IF lines( t_0132_aux ) > 1.
      MESSAGE TEXT-136 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true. EXIT.
    ENDIF.

    t_0132_aux = t_0132.
    SORT t_0132_aux BY armazem.
    DELETE ADJACENT DUPLICATES FROM t_0132_aux COMPARING armazem.
    IF lines( t_0132_aux ) > 1.
      MESSAGE TEXT-137 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true. EXIT.
    ENDIF.

    t_0132_aux = t_0132.
    SORT t_0132_aux BY transportadora.
    DELETE ADJACENT DUPLICATES FROM t_0132_aux COMPARING transportadora.
    IF lines( t_0132_aux ) > 1.
      MESSAGE TEXT-138 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true. EXIT.
    ENDIF.

  ENDIF.

  LOOP AT it_cliente_pop_5121 INTO wa_cliente_5121.
    "Check solicitação sem sequência de entrega
    IF wa_cliente_5121-seq_ent_cg IS INITIAL.
      MESSAGE TEXT-037 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true. EXIT.
    ENDIF.
  ENDLOOP.

  "Check solicitação sem sequência correta
  it_cliente_5121_check = it_cliente_pop_5121.
  SORT it_cliente_5121_check BY seq_ent_cg ASCENDING.
  DELETE ADJACENT DUPLICATES FROM it_cliente_5121_check COMPARING seq_ent_cg.
  LOOP AT it_cliente_5121_check INTO wa_cliente_5121.
    IF wa_cliente_5121-seq_ent_cg NE sy-tabix.
*      "// Removido a Obrigatoriedade conforme alinhado com Key User.
*      MESSAGE TEXT-037 TYPE 'S' DISPLAY LIKE 'E'.
*      VL_CHECK = ABAP_TRUE. EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SALVA_CARGA_5121
*&---------------------------------------------------------------------*
FORM salva_carga_5121.

  DATA: it_zsdt0129     TYPE STANDARD TABLE OF zsdt0129,
        it_zsdt0130     TYPE STANDARD TABLE OF zsdt0130,
        wa_zsdt0129     TYPE zsdt0129,
        wa_zsdt0130     TYPE zsdt0130,
        wa_zsdt0133     TYPE zsdt0133,
        wa_cliente_5121 TYPE ty_cliente_5121,
        wa_zsdt0135     TYPE zsdt0135,
        vl_n_cg         TYPE zsdt0129-nro_cg,
        vl_inco1        TYPE zsdt0129-inco1,
        vl_marca        TYPE zsdt0129-marca.

  SELECT *
    FROM zsdt0129
    INTO TABLE it_zsdt0129
    FOR ALL ENTRIES IN it_cliente_pop_5121
    WHERE nro_lote EQ it_cliente_pop_5121-nro_lote.

  SELECT *
    FROM zsdt0130
    INTO TABLE it_zsdt0130
    FOR ALL ENTRIES IN it_cliente_pop_5121
    WHERE nro_lote EQ it_cliente_pop_5121-nro_lote
      AND nro_sol  EQ it_cliente_pop_5121-nro_sol
      AND status   NE 'X'.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZSD_INS_CG'
    IMPORTING
      number                  = vl_n_cg
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  LOOP AT it_zsdt0129 INTO wa_zsdt0129.
    vl_inco1 = wa_zsdt0129-inco1.
    vl_marca = wa_zsdt0129-marca.
    wa_zsdt0129-nro_cg = vl_n_cg.
    wa_zsdt0129-dt_entrega = wa_dados_carga-dt_entrega.
    wa_zsdt0129-status = 2.
    MODIFY zsdt0129 FROM wa_zsdt0129.
  ENDLOOP.

  LOOP AT it_zsdt0130 INTO wa_zsdt0130.
    READ TABLE it_cliente_pop_5121 INTO wa_cliente_5121 WITH KEY nro_lote = wa_zsdt0130-nro_lote
                                                                 nro_sol  = wa_zsdt0130-nro_sol.
    IF sy-subrc IS INITIAL.
      wa_zsdt0130-seq_ent_cg = wa_cliente_5121-seq_ent_cg.
    ENDIF.

    MODIFY zsdt0130 FROM wa_zsdt0130.
  ENDLOOP.

  wa_zsdt0133-nro_cg = vl_n_cg.
  wa_zsdt0133-qtd_total_kg = vg_qtd_kg.

  IF wa_zsdt0133-qtd_total_kg LE 23000.
    wa_zsdt0133-ctg_transp = 'A'.
  ELSEIF wa_zsdt0133-qtd_total_kg LE 27000.
    wa_zsdt0133-ctg_transp = 'B'.
  ELSEIF wa_zsdt0133-qtd_total_kg LE 32000.
    wa_zsdt0133-ctg_transp = 'C'.
  ELSEIF wa_zsdt0133-qtd_total_kg LE 37000.
    wa_zsdt0133-ctg_transp = 'D'.
  ELSEIF wa_zsdt0133-qtd_total_kg LE 50000.
    wa_zsdt0133-ctg_transp = 'E'.
  ENDIF.

*** Inicio - Rubenilson - 10.09.24 - #144012
READ TABLE it_zsdt0129 ASSIGNING FIELD-SYMBOL(<fs_zsdt0129>) INDEX 1.
IF sy-subrc is INITIAL.
  wa_zsdt0133-armazem_org = <fs_zsdt0129>-armazem_org.
ENDIF.
*** Fim - Rubenilson - 10.09.24 - #144012

*Removido para que começe a enchergar a tabela zsdt0131
*  SELECT SINGLE *
*    FROM ZSDT0135
*    INTO WA_ZSDT0135
*    WHERE WRKST EQ VL_MARCA.

  "Passa para status - frete contratado (4) direto
  "se é FOB ou se é marca de responsabilidade da transp.
  IF vl_inco1 EQ 'FOB'.
    wa_zsdt0133-status = 4.
*  ELSEIF WA_ZSDT0135 IS NOT INITIAL.
*    WA_ZSDT0133-STATUS = 4.
  ELSE.
*** Inicio - Rubenilson - 14.08.24 - Cockpit de fretes
    WA_ZSDT0133-STATUS = 2.
"    wa_zsdt0133-status = 1. "Novo status Carga Criada.
*** Inicio - Rubenilson - 14.08.24 - Cockpit de fretes
  ENDIF.

  SELECT b~*
    FROM zsdt0131 AS a
      INNER JOIN zsdt0132 AS b ON b~nr_rot EQ a~cod_loc_emb
      INTO TABLE @DATA(t_0132)
        FOR ALL ENTRIES IN @it_zsdt0129
          WHERE a~nro_lote EQ @it_zsdt0129-nro_lote.

  DATA(t_0132_aux) = t_0132.

  SORT t_0132 BY transportadora.
  DELETE ADJACENT DUPLICATES FROM t_0132 COMPARING transportadora.

  IF lines( t_0132 ) EQ 1.
    wa_zsdt0133-cod_transportadora = t_0132[ 1 ]-transportadora.
    IF wa_zsdt0133-cod_transportadora IS NOT INITIAL.
      wa_zsdt0133-status = 4.
    ENDIF.
  ENDIF.

  wa_zsdt0133-usnam = sy-uname.
  wa_zsdt0133-data_atual = sy-datum.
  wa_zsdt0133-hora_atual = sy-uzeit.

  INSERT zsdt0133 FROM wa_zsdt0133.
  MESSAGE s000(z_fi) WITH 'Carga' vl_n_cg 'gerada' .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DESBLOQUEIA_MONTAGEM_5121
*&---------------------------------------------------------------------*
FORM desbloqueia_montagem_5121 .

  DATA: wa_lote_5120 TYPE ty_lote_5120.

  LOOP AT it_lote_5120_check INTO wa_lote_5120.

    CALL FUNCTION 'ZDENQUEUE_SD_LOTE_INSUMOS'
      EXPORTING
        chave = wa_lote_5120-nro_lote.

  ENDLOOP.

ENDFORM.
