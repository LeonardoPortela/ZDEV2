*----------------------------------------------------------------------*
***INCLUDE ZMMR153_USER_8005.
*----------------------------------------------------------------------*

CLASS lcl_event_handler_8005 DEFINITION.
  PUBLIC SECTION.
    DATA: validar_data  TYPE c,
          error_in_data TYPE c,
          ls_good       TYPE lvc_s_modi,
          lv_value      TYPE lvc_value.

    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
    METHODS data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.
    METHODS data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.

  PRIVATE SECTION.
    TYPES: ddshretval_table TYPE TABLE OF ddshretval.
    METHODS: perform_semantic_checks
      IMPORTING
        pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: cccontainer_8005     TYPE REF TO cl_gui_custom_container,
      ctl_alv_8005         TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog_8005 TYPE lvc_t_fcat,
      gs_variant_8005      TYPE disvariant,
      gs_layout_8005       TYPE lvc_s_layo,
      event_handler_8005   TYPE REF TO lcl_event_handler_8005,
      wa_stable_8005       TYPE lvc_s_stbl.

DATA: it_blocos_livres TYPE TABLE OF zde_zsdt0001fd_disp_alv,
      wa_blocos_livres TYPE zde_zsdt0001fd_disp_alv.

CLASS lcl_event_handler_8005 IMPLEMENTATION.

  METHOD handle_hotspot_click.
    "
  ENDMETHOD.                    "handle_hotspot_click

  METHOD data_changed_finished.

    IF e_modified IS NOT INITIAL.
      wa_stable_8005-row = abap_true.
      wa_stable_8005-col = abap_true.
      CALL METHOD ctl_alv_8005->refresh_table_display
        EXPORTING
          is_stable = wa_stable_8005.
    ENDIF.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_

  METHOD data_changed.
    error_in_data = space.
    CALL METHOD perform_semantic_checks( er_data_changed ).
    IF error_in_data = 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.
  ENDMETHOD.                    "on_data_chaged

  METHOD perform_semantic_checks.

    DATA: lc_qt_fardos TYPE zde_qtd_vincular_algodao,
          "LC_QT_VINCULAR TYPE ZDE_QTD_VINCULAR_ALGODAO,
          lv_value     TYPE lvc_value,
          lv_value2    TYPE lvc_value.

    DATA: lc_peso_fardo_sem_emb TYPE ty_valor,
          lc_peso_fardo_com_emb TYPE ty_valor,
          lc_peso_embalagem     TYPE ty_valor.

    error_in_data = abap_false.

    PERFORM calcula_media CHANGING lc_peso_fardo_sem_emb lc_peso_fardo_com_emb lc_peso_embalagem.

    LOOP AT pr_data_changed->mt_good_cells INTO DATA(ls_good)
      WHERE fieldname EQ 'QT_FARDOS_VINCULAR'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      lc_qt_fardos = lv_value.

      READ TABLE it_blocos_livres ASSIGNING FIELD-SYMBOL(<fs_vincular>) INDEX ls_good-row_id.

      "Valida Saldo de Quantidade de Fardos """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF <fs_vincular>-qt_fardos_saldo LT lc_qt_fardos.

        WRITE <fs_vincular>-qt_fardos_saldo TO lv_value.
        CONDENSE lv_value NO-GAPS.
        WRITE lc_qt_fardos TO lv_value2.
        CONDENSE lv_value2 NO-GAPS.

        "Se Estiver Errado o Valor
        CALL METHOD pr_data_changed->add_protocol_entry
          EXPORTING
            i_msgid     = 'ZCARGA'
            i_msgno     = '273'
            i_msgty     = 'E'
            i_msgv1     = lv_value
            i_msgv2     = lv_value2
            i_fieldname = ls_good-fieldname
            i_row_id    = ls_good-row_id.

        error_in_data = abap_true.
      ENDIF.

      DATA(lc_qt_vincular) = lc_qt_fardos * lc_peso_fardo_sem_emb.

      "Valida Saldo de Quantidade de Kilos """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF <fs_vincular>-qt_peso_saldo LT lc_qt_vincular.

        WRITE <fs_vincular>-qt_peso_saldo TO lv_value.
        CONDENSE lv_value NO-GAPS.
        WRITE lc_qt_vincular TO lv_value2.
        CONDENSE lv_value2 NO-GAPS.

        "Se Estiver Errado o Valor
        CALL METHOD pr_data_changed->add_protocol_entry
          EXPORTING
            i_msgid     = 'ZCARGA'
            i_msgno     = '259'
            i_msgty     = 'W'
            i_msgv1     = lv_value
            i_msgv2     = lv_value2
            i_fieldname = ls_good-fieldname
            i_row_id    = ls_good-row_id.

        error_in_data = abap_true.
      ENDIF.

      <fs_vincular>-qt_peso_vincular = lc_qt_vincular.

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Module  STATUS_8005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_8005 OUTPUT.

  DATA: it_function_8005 TYPE ui_functions.

  SET PF-STATUS 'PF8001'.
  SET TITLEBAR 'TL8005' WITH wa_ordens_venda_alv-nr_ordem_venda.

  IF cccontainer_8005 IS INITIAL.

    CREATE OBJECT cccontainer_8005
      EXPORTING
        container_name = 'ALV_BLOCOS'.

    CREATE OBJECT ctl_alv_8005
      EXPORTING
        i_parent = cccontainer_8005.

    PERFORM fill_it_fieldcatalog_8005.

    PERFORM fill_gs_variant_8005.

    CLEAR: it_function_8005[].
    APPEND cl_gui_alv_grid=>mc_fc_excl_all TO it_function_8005.

    CALL METHOD ctl_alv_8005->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_8005
        is_variant           = gs_variant_8005
        i_save               = 'A'
        it_toolbar_excluding = it_function_8005
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_8005
        it_outtab            = it_blocos_livres[].

    CALL METHOD ctl_alv_8005->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD ctl_alv_8005->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CREATE OBJECT event_handler_8005.
    SET HANDLER event_handler_8005->handle_hotspot_click FOR ctl_alv_8005.
    SET HANDLER event_handler_8005->data_changed FOR ctl_alv_8005.
    SET HANDLER event_handler_8005->data_changed_finished FOR ctl_alv_8005.
  ENDIF.

  wa_stable_8005-row = abap_true.
  wa_stable_8005-col = abap_true.
  CALL METHOD ctl_alv_8005->refresh_table_display
    EXPORTING
      is_stable = wa_stable_8005.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8005_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_8005_exit INPUT.

  PERFORM limpar_8005.

  CLEAR: ok_code, ck_add_bloco.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_8005 INPUT.

  CASE ok_code.
    WHEN 'CONF'.
      CLEAR: ok_code.
      ck_add_bloco = abap_true.
      PERFORM add_blocos_ordem_venda.
      PERFORM limpar_8005.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_8005
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM limpar_8005 .

  CLEAR: event_handler_8005.

  IF ctl_alv_8005 IS NOT INITIAL.
    ctl_alv_8005->free(
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR: ctl_alv_8005.

  IF cccontainer_8005 IS NOT INITIAL.
    cccontainer_8005->free(
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR: cccontainer_8005.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_8005
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_8005 .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog_8005[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZSDT0001FD_DISP_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcatalog_8005.

  lc_col_pos = 13.

  LOOP AT it_fieldcatalog_8005 ASSIGNING <fs_cat>.

    "<FS_CAT>-TABNAME = 'ZDE_ZSDT0001FD_DISP_ALV'.

    CASE <fs_cat>-fieldname.
      WHEN 'DS_PONTO_C'.
        <fs_cat>-outputlen = 33.
        <fs_cat>-col_pos   = 1.

      WHEN 'NM_BLOCO'.
        <fs_cat>-outputlen = 6.
        <fs_cat>-col_pos   = 2.

      WHEN 'QT_FARDOS'.
        <fs_cat>-do_sum    = abap_true.
        <fs_cat>-outputlen = 08.
        <fs_cat>-col_pos   = 3.

      WHEN 'QT_PESO'.
        <fs_cat>-do_sum    = abap_true.
        <fs_cat>-outputlen = 11.
        <fs_cat>-col_pos   = 4.

      WHEN 'QT_FARDOS_VINCULAR'.
        <fs_cat>-do_sum    = abap_true.
        <fs_cat>-edit      = abap_true.
        <fs_cat>-outputlen = 08.
        <fs_cat>-col_pos   = 5.

      WHEN 'QT_PESO_VINCULAR'.
        <fs_cat>-do_sum    = abap_true.
        <fs_cat>-outputlen = 11.
        <fs_cat>-col_pos   = 6.

      WHEN 'QT_FARDOS_SALDO'.
        <fs_cat>-do_sum    = abap_true.
        <fs_cat>-outputlen = 08.
        <fs_cat>-col_pos   = 7.

      WHEN 'QT_PESO_SALDO'.
        <fs_cat>-do_sum    = abap_true.
        <fs_cat>-outputlen = 11.
        <fs_cat>-col_pos   = 8.

      WHEN 'DS_SAFRA'.
        <fs_cat>-just      = 'C'.
        <fs_cat>-outputlen = 8.
        <fs_cat>-col_pos   = 9.

      WHEN 'ID_WERKS'.
        <fs_cat>-just      = 'C'.
        <fs_cat>-outputlen = 8.
        <fs_cat>-col_pos   = 10.

      WHEN 'DS_CONTRATO'.
        <fs_cat>-outputlen = 10.
        <fs_cat>-col_pos   = 11.

      WHEN 'DS_INSTRUCAO'.
        <fs_cat>-outputlen = 10.
        <fs_cat>-col_pos   = 12.

      WHEN OTHERS.
        <fs_cat>-no_out = abap_true.
        <fs_cat>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_8005
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_8005 .

  gs_variant_8005-report      = sy-repid.
  gs_variant_8005-handle      = '8005'.
  gs_variant_8005-log_group   = abap_false.
  gs_variant_8005-username    = abap_false.
  gs_variant_8005-variant     = abap_false.
  gs_variant_8005-text        = abap_false.
  gs_variant_8005-dependvars  = abap_false.

  gs_layout_8005-sel_mode     = 'A'.
  gs_layout_8005-zebra        = abap_false.
  "GS_LAYOUT_8005-CWIDTH_OPT   = ABAP_TRUE.
  "GS_LAYOUT_8005-EDIT_MODE    = ABAP_TRUE.
  gs_layout_8005-info_fname   = 'LINE_COLOR'.
  gs_layout_8005-stylefname   = 'STYLE'.
  gs_layout_8005-ctab_fname   = 'COLOR_CELL'.
  gs_layout_8005-grid_title   = TEXT-047.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GERAR_SALDO_DE_BLOCOS_FARDOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gerar_saldo_de_blocos_fardos .

  DATA: it_zsdt0045 TYPE TABLE OF zsdt0045.

  DATA: lc_peso_fardo_sem_emb TYPE ty_valor,
        lc_peso_fardo_com_emb TYPE ty_valor,
        lc_peso_embalagem     TYPE ty_valor.

  CLEAR: it_blocos_livres, it_blocos_livres[], it_zsdt0045, it_zsdt0045[].

  CHECK wa_ordens_venda_alv-nr_ordem_venda IS NOT INITIAL.

  PERFORM calcula_media CHANGING lc_peso_fardo_sem_emb lc_peso_fardo_com_emb lc_peso_embalagem.

  TRY .
      "Get Ponto de Coleta
      zcl_ordem_venda=>zif_ordem_venda~get_instance(
        )->set_ordem_venda( i_vbeln = wa_ordens_venda_alv-nr_ordem_venda
        )->get_partiner( EXPORTING i_funcao_partiner = 'PC' IMPORTING e_partiner = DATA(e_partiner)
        ).
    CATCH zcx_ordem_venda INTO ex_ordem_venda.    " .
      ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      EXIT.
  ENDTRY.

  "Tabela de Solicitação de Ordem de Venda – Formação de Lote
  SELECT * INTO TABLE @DATA(it_zsdt0066)
    FROM zsdt0066
   WHERE vbeln   EQ @wa_ordens_venda_alv-nr_ordem_venda
     AND charg   EQ @zde_zsdt0001cg_alv-nr_safra
     AND werks   EQ @zde_zsdt0001cg_alv-id_branch
     AND vbeln   NE @space
     AND ponto_c EQ @e_partiner-lifnr.

  CHECK sy-subrc IS INITIAL.

  "Tabela de Itens da Solicitação de Venda
  SELECT * INTO TABLE @DATA(it_zsdt0053)
    FROM zsdt0053
     FOR ALL ENTRIES IN @it_zsdt0066
   WHERE nro_sol_ov EQ @it_zsdt0066-nro_sol_ov.

  LOOP AT it_zsdt0066 INTO DATA(wa_zsdt0066).
    "Tabela de Instruções de Embarque de Algodão
    SELECT * APPENDING TABLE @it_zsdt0045
      FROM zsdt0045
     WHERE objecttable EQ 'ZSDT0051'
       AND safra       EQ @zde_zsdt0001cg_alv-nr_safra
       AND werks       EQ @zde_zsdt0001cg_alv-id_branch
       AND objek       EQ @wa_zsdt0066-nro_sol_ov.
  ENDLOOP.

*alterado por guilherme rabelo inicio .

  DATA: tipo    TYPE TABLE OF rgsb4,
        w_tipo  TYPE rgsb4,
        v_btgew TYPE zsdt0045-btgew.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr         = 'ZMM0127_TOLERANCIA_PESO_'
      class         = '0000'
    TABLES
      set_values    = tipo
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.


  READ TABLE tipo INTO w_tipo INDEX 1.
  IF sy-subrc = 0.
    LOOP AT it_zsdt0045 INTO DATA(ls_zsdt0045).

      v_btgew = ls_zsdt0045-btgew / 100 * w_tipo-from.
      ls_zsdt0045-btgew = ls_zsdt0045-btgew + v_btgew.

      MODIFY it_zsdt0045 FROM ls_zsdt0045.

    ENDLOOP.
  ENDIF.

*alterado por guilherme rabelo inicio .

  SORT it_zsdt0045 BY zseq_inst objek objecttable.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0045 COMPARING zseq_inst objek objecttable.

  CHECK it_zsdt0045[] IS NOT INITIAL.

  SELECT fd~zseq_inst,
         fd~objek,
         fd~objecttable,
         fd~qt_fardos,
         fd~ps_fardos_liqui
    INTO TABLE @DATA(it_zsdt0001fd)
    FROM zsdt0001fd AS fd
   INNER JOIN zsdt0001cg AS cg ON cg~id_carga EQ fd~id_carga
     FOR ALL ENTRIES IN @it_zsdt0045
   WHERE fd~zseq_inst    EQ @it_zsdt0045-zseq_inst
     AND fd~objek        EQ @it_zsdt0045-objek
     AND fd~objecttable  EQ @it_zsdt0045-objecttable
     AND fd~id_carga     NE @zde_zsdt0001cg_alv-id_carga
     AND cg~tp_status    NE @zif_carga=>st_status_cancelada.

  LOOP AT it_zsdt0066 INTO wa_zsdt0066.

    CLEAR: wa_blocos_livres.

    SELECT SINGLE name1 INTO @wa_blocos_livres-ds_ponto_c
      FROM lfa1
     WHERE lifnr EQ @wa_zsdt0066-ponto_c.

    wa_blocos_livres-nr_ordem_venda = wa_zsdt0066-vbeln.
    wa_blocos_livres-cd_ponto_c     = wa_zsdt0066-ponto_c.
    wa_blocos_livres-ds_instrucao   = wa_zsdt0066-instrucao.

    DATA: lc_fardos_vinculados TYPE p LENGTH 16 DECIMALS 0.
    DATA: lc_peso_vinculados   TYPE p LENGTH 16 DECIMALS 0.

    LOOP AT it_zsdt0045 INTO DATA(wa_it_zsdt0045)
      WHERE objek     EQ wa_zsdt0066-nro_sol_ov
        AND safra     EQ zde_zsdt0001cg_alv-nr_safra
        AND werks     EQ zde_zsdt0001cg_alv-id_branch
        AND instrucao EQ wa_zsdt0066-instrucao
        AND ponto_c   EQ wa_zsdt0066-ponto_c.

      wa_blocos_livres-zseq_inst       = wa_it_zsdt0045-zseq_inst.
      wa_blocos_livres-objek           = wa_it_zsdt0045-objek.
      wa_blocos_livres-objecttable     = wa_it_zsdt0045-objecttable.
      wa_blocos_livres-ds_contrato     = wa_it_zsdt0045-contrato.
      wa_blocos_livres-nm_bloco        = wa_it_zsdt0045-charg.
      wa_blocos_livres-ds_safra        = wa_it_zsdt0045-safra.
      wa_blocos_livres-id_werks        = wa_it_zsdt0045-werks.

      "Volumes Originais
      wa_blocos_livres-qt_peso         = wa_it_zsdt0045-btgew.
      wa_blocos_livres-qt_fardos       = wa_it_zsdt0045-quantidade.

      lc_fardos_vinculados = 0.
      lc_peso_vinculados   = 0.

      LOOP AT it_zsdt0001fd INTO DATA(wa_zsdt0001fd)
         WHERE zseq_inst    EQ wa_blocos_livres-zseq_inst
           AND objek        EQ wa_blocos_livres-objek
           AND objecttable  EQ wa_blocos_livres-objecttable.
        ADD wa_zsdt0001fd-qt_fardos       TO lc_fardos_vinculados.
        ADD wa_zsdt0001fd-ps_fardos_liqui TO lc_peso_vinculados.
      ENDLOOP.

      "Saldos com base nos volumes originais
      wa_blocos_livres-qt_fardos_saldo = wa_blocos_livres-qt_fardos - lc_fardos_vinculados.
      wa_blocos_livres-qt_peso_saldo   = wa_blocos_livres-qt_peso - lc_peso_vinculados.

      wa_blocos_livres-qt_fardos_vincular = 0.
      wa_blocos_livres-qt_peso_vincular   = 0.
      LOOP AT it_blocos_vincu INTO DATA(wa_blocos_vincu)
         WHERE nr_ordem_venda   EQ wa_blocos_livres-nr_ordem_venda
           AND nr_pedido_compra EQ wa_blocos_livres-nr_pedido_compra
           AND zseq_inst        EQ wa_blocos_livres-zseq_inst
           AND objek            EQ wa_blocos_livres-objek
           AND objecttable      EQ wa_blocos_livres-objecttable.
        "Fardos a Vincular
        ADD wa_blocos_vincu-qt_fardos TO wa_blocos_livres-qt_fardos_vincular.
      ENDLOOP.
      wa_blocos_livres-qt_peso_vincular = wa_blocos_livres-qt_fardos_vincular * lc_peso_fardo_sem_emb.

      APPEND wa_blocos_livres TO it_blocos_livres.

    ENDLOOP.

  ENDLOOP.

  SORT it_blocos_livres BY zseq_inst objek objecttable.
  DELETE ADJACENT DUPLICATES FROM it_blocos_livres COMPARING zseq_inst objek objecttable.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADD_BLOCOS_ORDEM_VENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_blocos_ordem_venda .

  DATA: lc_bloco TYPE zde_zsdt0001fd_alv.

  DATA: lc_fardos_ov TYPE volum_ap.

  LOOP AT it_blocos_vincu INTO DATA(wa_blocos_vincu)
     WHERE nr_ordem_venda EQ wa_ordens_venda_alv-nr_ordem_venda.

    READ TABLE it_blocos_livres INTO DATA(wa_blocos_livres)
    WITH KEY nr_ordem_venda = wa_blocos_vincu-nr_ordem_venda
             zseq_inst = wa_blocos_vincu-zseq_inst
             objek = wa_blocos_vincu-objek
             objecttable = wa_blocos_vincu-objecttable.

    IF sy-subrc IS INITIAL AND wa_blocos_livres-qt_fardos_vincular EQ 0.
      CLEAR: lc_bloco.
      lc_bloco-nr_ordem_venda = wa_blocos_livres-nr_ordem_venda.
      lc_bloco-nr_pedido_compra = wa_blocos_livres-nr_pedido_compra.
      lc_bloco-zseq_inst = wa_blocos_livres-zseq_inst.
      lc_bloco-objek = wa_blocos_livres-objek.
      lc_bloco-objecttable = wa_blocos_livres-objecttable.
      objeto->set_excluir_bloco( i_bloco = lc_bloco ).
    ENDIF.
  ENDLOOP.

  CLEAR: lc_fardos_ov.
  LOOP AT it_blocos_livres INTO wa_blocos_livres WHERE qt_fardos_vincular GT 0.

    TRY .

        CLEAR: lc_bloco.
        lc_bloco-id_carga       = zde_zsdt0001cg_alv-id_carga.
        lc_bloco-nr_ordem_venda = wa_blocos_livres-nr_ordem_venda.
        lc_bloco-zseq_inst      = wa_blocos_livres-zseq_inst.
        lc_bloco-objek          = wa_blocos_livres-objek.
        lc_bloco-objecttable    = wa_blocos_livres-objecttable.
        lc_bloco-cd_ponto_c     = wa_blocos_livres-cd_ponto_c.
        lc_bloco-ds_ponto_c     = wa_blocos_livres-ds_ponto_c.
        lc_bloco-ds_contrato    = wa_blocos_livres-ds_contrato.
        lc_bloco-ds_instrucao   = wa_blocos_livres-ds_instrucao.
        lc_bloco-nm_bloco       = wa_blocos_livres-nm_bloco.
        lc_bloco-qt_fardos      = wa_blocos_livres-qt_fardos_vincular.
        lc_bloco-ds_safra       = wa_blocos_livres-ds_safra.
        lc_bloco-id_werks       = wa_blocos_livres-id_werks.

        ADD wa_blocos_livres-qt_fardos_vincular TO lc_fardos_ov.

        objeto->set_add_bloco( EXPORTING i_bloco = lc_bloco ).
      CATCH zcx_carga INTO ex_carga.    "
        ex_carga->published_erro( i_msgty = 'W' ).
    ENDTRY.

  ENDLOOP.

  "Seta Total Fardos vinculados para a Ordem de Venda
  TRY.
      objeto->set_volume_ordem_venda(
           i_vbeln  = wa_ordens_venda_alv-nr_ordem_venda
           i_volume = lc_fardos_ov ).
    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( i_msgty = 'W' ).
  ENDTRY.

  PERFORM seta_carga.

  CLEAR: it_blocos_livres.

ENDFORM.
