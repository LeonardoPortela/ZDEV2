*----------------------------------------------------------------------*
***INCLUDE LZSD_SIGAM_INTERFACEF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_REFRESH_ALL
*&---------------------------------------------------------------------*
FORM f_refresh_all .

  CLEAR: gv_screen_status, gv_param,
         gt_0005_global,gt_0005_global, gt_0006,
         gv_cursor, gw_bapiret, gt_0006_global,
         gt_lfa1, go_005_alv, go_006_alv,
         go_cc005_container, go_cc006_container.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DEFINE_PARAM
*&---------------------------------------------------------------------*
FORM f_define_param USING p_tcode TYPE sy-tcode
                         p_repro TYPE c
                 CHANGING p_erro TYPE c.

  DATA lw_0002 TYPE zsdt0002us.
  DATA lw_ret TYPE c.

  gv_param-tcode = p_tcode.

  gv_param-bukrs = ''.
  gv_param-matnr = ''.

  IF gv_param-bukrs IS INITIAL.

    SELECT SINGLE * FROM zsdt0002us
      INTO lw_0002
        WHERE us_name = sy-uname
          AND tcode = p_tcode.

    IF sy-subrc NE 0 OR p_repro = 'X'.

      PERFORM f_poup_get_values CHANGING lw_0002 lw_ret.

      IF lw_ret IS NOT INITIAL.
        p_erro = 'X'.
        EXIT.
      ELSE.

        lw_0002-tcode = p_tcode.
        lw_0002-us_name = sy-uname.

        MODIFY zsdt0002us FROM lw_0002.

      ENDIF.

    ENDIF.

    gv_param-bukrs = lw_0002-bukrs.
    gv_param-matnr = lw_0002-matnr.

    WRITE gv_param-matnr TO gv_param-matnr_txt NO-ZERO LEFT-JUSTIFIED.

  ENDIF.

  CASE gv_param-tcode.
    WHEN 'ZSDT0132N'.

      "Solicitação Ordem Venda e Pedido Transf

      gv_param-titulo = 'Solic. OV e Pedido Transf: Empresa &1 Material: &2'.
      gv_param-titulo_btn_1 = 'Pesquisar solicitações'.
      gv_param-titulo_btn_2 = 'Nova solicitação'.

      gv_param-branch_txt = 'Filial OV'.
      gv_param-popup_txt = 'Ordem de venda(OV) tem referência a:'.
      gv_param-alv_text = 'OVs geradas para o lote selecionado'.
      gv_param-qtde_txt = 'Qt.OVs'.

      gv_param-tcode_search = gv_param-tcode.

    WHEN 'ME21N'.

      gv_param-titulo = 'Pedido de Transferencia: Empresa &1 Material: &2'.
      gv_param-titulo_btn_1 = 'Pesquisar Pedido'.
      gv_param-titulo_btn_2 = 'Novo Pedido'.

      gv_param-branch_txt = 'Filial Pedido'.
      gv_param-popup_txt = 'Pedido de transferência tem referência a:'.
      gv_param-alv_text = 'Pedidos gerados para o lote selecionado'.
      gv_param-qtde_txt = 'Qt.Ped'.

      gv_param-tcode_search = 'ME23N'.

    WHEN OTHERS.

      p_erro = 'X'.

      gv_param-msgty = 'E'.
      gv_param-msgtx = `A transação ` && p_tcode && ` não está configurada para o SIGAM`.


  ENDCASE.

  REPLACE '&1' IN gv_param-titulo WITH gv_param-bukrs.
  REPLACE '&2' IN gv_param-titulo WITH gv_param-matnr_txt.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POUP_GET_VALUES
*&---------------------------------------------------------------------*
FORM f_poup_get_values CHANGING p_info TYPE zsdt0002us
                                p_ret TYPE c.

  DATA lt_val TYPE TABLE OF sval.

  APPEND INITIAL LINE TO lt_val ASSIGNING FIELD-SYMBOL(<fs_val>).

  <fs_val>-tabname = 'ZSDT0002US'.
  <fs_val>-fieldname = 'BUKRS'.
  <fs_val>-field_obl = 'X'.

  APPEND INITIAL LINE TO lt_val ASSIGNING <fs_val>.

  <fs_val>-tabname = 'ZSDT0002US'.
  <fs_val>-fieldname = 'MATNR'.
  <fs_val>-field_obl = 'X'.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = TEXT-001
      start_column    = '5'
      start_row       = '5'
    IMPORTING
      returncode      = p_ret
    TABLES
      fields          = lt_val
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    p_ret = 'A'. " <-- ABORT..
    EXIT.
  ENDIF.

  READ TABLE lt_val ASSIGNING <fs_val>
    WITH KEY fieldname = 'BUKRS'.

  IF sy-subrc EQ 0.
    p_info-bukrs = <fs_val>-value.
  ENDIF.

  READ TABLE lt_val ASSIGNING <fs_val>
    WITH KEY fieldname = 'MATNR'.

  IF sy-subrc EQ 0.
    p_info-matnr = <fs_val>-value.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_REDEFINE_PARAM
*&---------------------------------------------------------------------*
FORM f_redefine_param .

  DATA lv_param TYPE zsde0011.
  DATA lv_erro TYPE c.

  lv_param = gv_param.

  PERFORM f_define_param
    USING lv_param-tcode 'X'
  CHANGING lv_erro.

  CHECK lv_erro IS NOT INITIAL.

  " SE DER ERRO, VOLTA AO QUE ESTAVA ANTES, POPUP CANCELADO
  gv_param = lv_param.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXECUTA_BUSCA_SIGAM
*&---------------------------------------------------------------------*
FORM f_executa_busca_sigam .

  DATA lv_proc_normal TYPE c.
  DATA lv_existe TYPE c.
  DATA lv_selec TYPE c.

  DATA lw_popup TYPE zsde0003.

  PERFORM f_check_bukrs_stvarv
    USING gv_param-bukrs
 CHANGING lv_existe.

  " Se não estiver , abrir a tela da ZSDT0132 como é hoje.
  IF lv_existe IS INITIAL.
    lv_proc_normal = 'X'.
  ENDIF.

  PERFORM f_check_matnr_stvarv
    USING gv_param-matnr
 CHANGING lv_existe.

  " Se o material pertence a um dos grupos de mercadoria deve-se verificar
  " se ele não esta cadastrado como exceção na nova tabela ZSDT02xx
  IF lv_existe = 'X'.

    PERFORM f_check_matnr_excecao
      USING gv_param-matnr
   CHANGING lv_existe.

    "Se estiver cadastrado como exceção , abrir a tela da ZSDT0132 como ela é atualmente.
    IF lv_existe = 'X'.

      lv_proc_normal = 'X'.

    ENDIF.

  ELSE.

    lv_proc_normal = 'X'.

  ENDIF.

  IF lv_proc_normal = 'X'.

    PERFORM f_call_transaction.

  ELSE.

    CALL FUNCTION 'ZMM_POPUP_REFERENCIA_PRECO'
      EXPORTING
        iv_text        = gv_param-popup_txt
      IMPORTING
        ew_dados_popup = lw_popup
        ev_ret         = lv_selec.

    IF lv_selec = '2'.
      EXIT.
      "lv_proc_normal = 'X'.
    ENDIF.

    IF lw_popup-preco_afixar = 'X'.
      lv_proc_normal = 'X'.
    ENDIF.

    IF lw_popup-preco_fixo = 'X' AND lw_popup-cif = 'X'.
      lv_proc_normal = 'X'.
    ENDIF.

    IF lv_proc_normal = 'X'.

      PERFORM f_call_transaction.
      "CALL TRANSACTION gv_param-tcode.

    ELSE.

      zsde0004-bukrs = gv_param-bukrs.
      zsde0004-matnr = gv_param-matnr.

      zsde0004-rb_lotes_filial = 'X'.
      zsde0004-rb_lotes_outras = ''.
      zsde0004-rb_lotes_todos = ''.

      " limpa as tabelas globais
      CLEAR: gt_0005_global, gt_0006_global, gt_0005, gt_0006.

      PERFORM f_refresh_grid USING 'X' 'X'.

      CALL SCREEN 9200.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_BUKRS_STVARV
*&---------------------------------------------------------------------*
FORM f_check_bukrs_stvarv USING p_bukrs TYPE bukrs
                       CHANGING p_existe TYPE c.

  CLEAR p_existe.

  SELECT COUNT(*) FROM tvarvc
    WHERE name = 'EMPRESA_P&L_FRETE'
      AND low = p_bukrs.

  IF sy-dbcnt > 0.
    p_existe = 'X'.
  ENDIF.

  IF p_existe = space.

    "PERFORM f_put_mensagem USING 'E' 'Empresa não cadastrada'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_MATNR_STVARV
*&---------------------------------------------------------------------*
FORM f_check_matnr_stvarv USING p_matnr TYPE matnr
                       CHANGING p_existe TYPE c.

  CLEAR p_existe.

  SELECT SINGLE matkl FROM mara
    INTO @DATA(lv_matkl)
      WHERE matnr = @p_matnr.

  CHECK sy-subrc EQ 0.

  SELECT COUNT(*) FROM tvarvc
    WHERE name = 'MAGGI_GR_GRAOS'
      AND low = lv_matkl.

  IF sy-dbcnt > 0.
    p_existe = 'X'.
  ENDIF.

  SELECT COUNT(*) FROM tvarvc
    WHERE name = 'MAGGI_GR_ALGODAO'
      AND low = lv_matkl.

  IF sy-dbcnt > 0.
    p_existe = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_MATNR_EXCECAO
*&---------------------------------------------------------------------*
FORM f_check_matnr_excecao USING p_matnr TYPE matnr
                       CHANGING p_existe TYPE c.

  CLEAR p_existe.

  SELECT COUNT(*) FROM zsdt0291
    WHERE matnr = p_matnr.

  IF sy-dbcnt > 0.
    p_existe = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_RADIO_CLICKED
*&---------------------------------------------------------------------*
FORM f_radio_clicked.

  CASE 'X'.
    WHEN zsde0003-preco_fixo.
      gv_9100_hide = space.
    WHEN zsde0003-preco_afixar.
      gv_9100_hide = 'X'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_ALV_GT_005
*&---------------------------------------------------------------------*
FORM f_display_alv_gt_005 .

  IF go_cc005_container IS INITIAL.

    " Create a custom container control for our ALV Control
    CREATE OBJECT go_cc005_container
      EXPORTING
        container_name              = 'CC_005'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc NE 0.
      MESSAGE i000(zpp) WITH 'The custom control could not be created'.
      RETURN.
    ENDIF.

  ENDIF.

  PERFORM f_create_alv_005.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_ALV_005
*&---------------------------------------------------------------------*
FORM f_create_alv_005 .

  DATA lw_settings TYPE lvc_s_glay.
  DATA lw_layout TYPE lvc_s_layo.
  DATA lt_fieldcat TYPE lvc_t_fcat.

  DATA lo_events TYPE REF TO cl_salv_events_table.
  DATA lo_handle TYPE REF TO lcl_event_handler.

  CHECK go_005_alv IS INITIAL.

  CHECK gt_0005_global IS NOT INITIAL.

  CREATE OBJECT go_005_alv
    EXPORTING
      i_parent = go_cc005_container.

  lw_layout-grid_title = 'Lotes de compra'.

  lw_layout-sel_mode = 'A'.
  "lw_layout-no_headers = 'X'.
  "lw_layout-no_toolbar = 'X'. XXXXX
  "lw_layout-col_opt = 'X'.
  lw_layout-cwidth_opt = 'X'.

  lw_layout-box_fname = 'SELEC'.
  lw_layout-zebra = 'X'.
  "lw_layout-

  lw_settings-edt_cll_cb = 'X'.

  "PERFORM f_append_005_test.

  PERFORM f_get_fieldcat USING 'ZSDE0005' CHANGING lt_fieldcat.

  DELETE lt_fieldcat WHERE fieldname = 'SELEC'.

  READ TABLE lt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_field>)
    WITH KEY fieldname = 'ICON_PROD_PERM'.

  IF sy-subrc EQ 0.
    <fs_field>-just = 'C'.
  ENDIF.

  PERFORM f_coluna_descr
    USING 'FILIAL_PED'
          gv_param-branch_txt
 CHANGING lt_fieldcat.

  PERFORM f_coluna_descr
    USING 'QTDE_006'
          gv_param-qtde_txt
 CHANGING lt_fieldcat.

  READ TABLE lt_fieldcat ASSIGNING <fs_field>
    WITH KEY fieldname = 'QTDE_006'.

  IF sy-subrc EQ 0.
    <fs_field>-outputlen = 6.
  ENDIF.


  DELETE lt_fieldcat WHERE fieldname = 'BUKRS'.
  DELETE lt_fieldcat WHERE fieldname = 'NU_COMPRA'.
  DELETE lt_fieldcat WHERE fieldname = 'ID_COMPRA'.

  DELETE lt_fieldcat WHERE fieldname = 'MAKTX'.
  DELETE lt_fieldcat WHERE fieldname = 'TIPOOPERACAO'.
  DELETE lt_fieldcat WHERE fieldname = 'TRANSGENIA'.
  DELETE lt_fieldcat WHERE fieldname = 'ANO'.
  DELETE lt_fieldcat WHERE fieldname = 'MES'.
  DELETE lt_fieldcat WHERE fieldname = 'PERIODO'.
  DELETE lt_fieldcat WHERE fieldname = 'COMPRA'.
  DELETE lt_fieldcat WHERE fieldname = 'PRODUTODERIVADO'.
  DELETE lt_fieldcat WHERE fieldname = 'ZFIC_BRANCH'.
  DELETE lt_fieldcat WHERE fieldname = 'LCL_ENTR_BUKRS'.
  DELETE lt_fieldcat WHERE fieldname = 'LCL_ENTR_BRANCH'.
  DELETE lt_fieldcat WHERE fieldname = 'PROD_PERM_TAB'.

*  CALL METHOD go_005_alv->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CREATE OBJECT lo_handle.

  SET HANDLER lo_handle->handle_double_click FOR go_005_alv.
  "set HANDLER lo_handle->
  "SET HANDLER lo_handle->handle_data_changed FOR go_005_alv.

  "SET HANDLER lo_handle->handle_top_of_page FOR go_005_alv.

  PERFORM f_filtro_lote_alv.

  " Configuration for first display.
  CALL METHOD go_005_alv->set_table_for_first_display
    EXPORTING
      is_layout       = lw_layout
      i_save          = 'A'
    CHANGING
      it_outtab       = gt_0005 "gt_0005_global
      it_fieldcatalog = lt_fieldcat.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_ALV_GT_006
*&---------------------------------------------------------------------*
FORM f_display_alv_gt_006 .

  DATA lw_layout TYPE lvc_s_layo.
  DATA lt_fieldcat TYPE lvc_t_fcat.

  IF go_cc006_container IS INITIAL.

    " Create a custom container control for our ALV Control
    CREATE OBJECT go_cc006_container
      EXPORTING
        container_name              = 'CC_006'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc NE 0.
      MESSAGE i000(zpp) WITH 'The custom control could not be created'.
      RETURN.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_APPEND_005_TEST
*&---------------------------------------------------------------------*
FORM f_append_006_test .

*  CHECK sy-sysid = 'DEV'.
*
*  DO 15 TIMES.
*
*    APPEND INITIAL LINE TO gt_0006 ASSIGNING FIELD-SYMBOL(<fs_teste>).
*
*    <fs_teste>-ov_pedido = '01010101'.
*    <fs_teste>-filial = '0111'.
*    <fs_teste>-auart = 'ZICC'.
*    <fs_teste>-id_compra = '01010101'.
*    <fs_teste>-nu_compra = '01010101'.
*    <fs_teste>-lote = '01010101'.
*    <fs_teste>-zmeng = '10'.
*    <fs_teste>-zieme = 'UN'.
*    <fs_teste>-rfmng = '10'.
*    <fs_teste>-matnr = '01010101'.
*    <fs_teste>-produtor = '01010101'.
*    <fs_teste>-desc_produtor = 'XXXXXXXXXXX'.
*    <fs_teste>-ponto_c = '01010101'.
*    <fs_teste>-desc_ponto_c = 'XXXXXXXXXXX'.
*    <fs_teste>-entrega = '01010101'.
*    <fs_teste>-desc_entrega = 'XXXXXXXXXXX'.
*    <fs_teste>-terminal = '01010101'.
*    <fs_teste>-desc_terminal = 'XXXXXXXXXXX'.
*  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_get_fieldcat USING p_struct TYPE tabname
                 CHANGING p_tab TYPE lvc_t_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_buffer_active        = space
      i_structure_name       = p_struct
    CHANGING
      ct_fieldcat            = p_tab
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f_coluna_descr
*&---------------------------------------------------------------------*
FORM f_coluna_descr USING p_fieldname TYPE slis_fieldname
                          p_text TYPE scrtext_l
                 CHANGING p_tab TYPE lvc_t_fcat..

  READ TABLE p_tab ASSIGNING FIELD-SYMBOL(<fs_cat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  <fs_cat>-scrtext_s = p_text.
  <fs_cat>-scrtext_m = p_text.
  <fs_cat>-scrtext_l = p_text.
  <fs_cat>-reptext = p_text.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ON_CLICK
*&---------------------------------------------------------------------*
FORM f_on_click USING e_row TYPE lvc_s_row
                      e_column TYPE lvc_s_col
                      es_row_no TYPE lvc_s_roid.

  READ TABLE gt_0005 ASSIGNING FIELD-SYMBOL(<fs_005>)
    INDEX e_row-index.

*  READ TABLE gt_0005_global ASSIGNING FIELD-SYMBOL(<fs_005>)
*    INDEX e_row-index.

  CHECK sy-subrc EQ 0.

  IF e_column = 'ICON_PROD_PERM'.

    CALL FUNCTION 'ZMM_ALV_GRID_POPUP'
      EXPORTING
        i_title = 'Produtores permitidos'
*     IMPORTING
*       ET_ROWS =
      TABLES
        it_alv  = <fs_005>-prod_perm_tab.

  ELSE.

    CLEAR gt_0006.

    LOOP AT gt_0006_global ASSIGNING FIELD-SYMBOL(<fs_0006>)
      WHERE nu_compra = <fs_005>-nu_compra
        AND lote = <fs_005>-lote.
      "AND tcode = gv_param-tcode.

      APPEND <fs_0006> TO gt_0006.

    ENDLOOP.

    PERFORM f_append_006_test.

    PERFORM f_create_alv_006.

    PERFORM f_refresh_grid USING space 'X'.

    IF gt_0006 IS INITIAL.

      PERFORM f_put_mensagem
        USING 'W'
              'Não há OV/Pedidos de transferencia gerados para esse lote de compra'.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_ALV_006
*&---------------------------------------------------------------------*
FORM f_create_alv_006 .

  DATA lw_layout TYPE lvc_s_layo.
  DATA lt_fieldcat TYPE lvc_t_fcat.

  DATA lo_events TYPE REF TO cl_salv_events_table.
  DATA lo_handle TYPE REF TO lcl_event_handler.

  CHECK go_006_alv IS INITIAL.

  CHECK gt_0006 IS NOT INITIAL.

  CREATE OBJECT go_006_alv
    EXPORTING
      i_parent = go_cc006_container.


  lw_layout-grid_title = gv_param-alv_text.
  lw_layout-sel_mode = 'A'.
  "lw_layout-no_headers = 'X'.
  lw_layout-no_toolbar = 'X'.
  lw_layout-col_opt = 'X'.
  lw_layout-cwidth_opt = 'X'.
  "lw_layout-box_fname = 'SELEC'.
  lw_layout-zebra = 'X'.
  "lw_layout-

  "PERFORM f_append_006_test.

  PERFORM f_get_fieldcat USING 'ZSDE0006' CHANGING lt_fieldcat.

  PERFORM f_fieldcat_conf USING 'R' CHANGING lt_fieldcat.

  PERFORM f_coluna_descr
    USING 'FILIAL_PED'
          gv_param-branch_txt
 CHANGING lt_fieldcat.

  PERFORM f_coluna_descr
    USING 'LOTE'
          'Lote'
 CHANGING lt_fieldcat.

  DELETE lt_fieldcat WHERE fieldname = 'TCODE'.

*  CALL METHOD go_006_alv->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*  CREATE OBJECT lo_handle.
*
*  SET HANDLER lo_handle->handle_double_click FOR go_006_alv.

  "SET HANDLER lo_handle->handle_top_of_page FOR go_006_alv.


  " Configuration for first display.
  CALL METHOD go_006_alv->set_table_for_first_display
    EXPORTING
      is_layout       = lw_layout
    CHANGING
      it_outtab       = gt_0006
      it_fieldcatalog = lt_fieldcat.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH_GRID
*&---------------------------------------------------------------------*
FORM f_refresh_grid USING p_005 TYPE c
                          p_006 TYPE c.

  DATA lw_stable TYPE lvc_s_stbl.

  lw_stable-row = 'X'.
  lw_stable-col = 'X'.

  IF p_005 = 'X'.

    IF go_005_alv IS NOT INITIAL.

      CALL METHOD go_005_alv->refresh_table_display
        EXPORTING
          is_stable      = lw_stable
          i_soft_refresh = 'X'
        EXCEPTIONS
          finished       = 1
          OTHERS         = 2.

    ENDIF.

  ENDIF.

  IF p_006 = 'X'.

    IF go_006_alv IS NOT INITIAL.

      CALL METHOD go_006_alv->refresh_table_display
        EXPORTING
          is_stable      = lw_stable
          i_soft_refresh = 'X'
        EXCEPTIONS
          finished       = 1
          OTHERS         = 2.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCAR
*&---------------------------------------------------------------------*
FORM f_buscar_lote .

  DATA lv_erro TYPE c.
  DATA lv_mess TYPE string.
  DATA lw_request TYPE zsde0007.

  lw_request-idprodutor = zsde0004-produtor.

  lw_request-idmaterial = zsde0004-matnr.
  SHIFT lw_request-idmaterial LEFT DELETING LEADING '0'.

  lw_request-idfilial = zsde0004-filial.
  lw_request-idpontocoleta = zsde0004-ponto_coleta.
  lw_request-safra = zsde0004-nr_safra.
  lw_request-idporto = zsde0004-porto.
  lw_request-idlocalentrega = zsde0004-local_entrega.

  PERFORM f_validacao_campos CHANGING lv_erro.

  CHECK lv_erro IS INITIAL.

  TRY.

      CLEAR gt_0005_global.
      "CLEAR gt_0005.
      CLEAR gt_0006.

      CLEAR gt_0006_global.

      gt_0005_global = zcl_integracao_sigam_lote=>zif_integracao_sigam_lote~enviar_sigam( lw_request ).

      PERFORM f_seleciona_zfic.

      PERFORM f_filtro_lote_alv.

      PERFORM f_refresh_grid USING 'X' 'X'.

      IF gt_0005_global IS NOT INITIAL.
        PERFORM f_put_mensagem USING 'S' 'Consulta realizada com sucesso'.
      ELSE.
        PERFORM f_put_mensagem USING 'W' 'Consulta realizada, mas sem dados de retorno'.
      ENDIF.

    CATCH zcx_integracao INTO DATA(lo_int).

      lv_mess = lo_int->get_longtext( ).

      PERFORM f_put_mensagem USING 'E' lv_mess.

      "gv_error_num = 1.

    CATCH zcx_error INTO DATA(lo_int_e).

      lv_mess = lo_int_e->get_longtext( ).

      PERFORM f_put_mensagem USING 'E' lv_mess.

      "gv_error_num = 1.

  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PUT_MENSAGEM
*&---------------------------------------------------------------------*
FORM f_put_mensagem USING p_type TYPE c
                          p_string TYPE string.

  DATA lv_msgty TYPE sy-msgty.

  CHECK p_string IS NOT INITIAL.

  IF p_type IS INITIAL.
    lv_msgty = 'E'.
  ELSE.
    lv_msgty = p_type.
  ENDIF.

  DATA: lt_trtexts     TYPE trtexts,
        lw_trtexts     TYPE trtext,
        lv_texto(4000).

  DATA lv_msg1 TYPE sy-msgv1.
  DATA lv_msg2 TYPE sy-msgv1.
  DATA lv_msg3 TYPE sy-msgv1.
  DATA lv_msg4 TYPE sy-msgv1.

  lv_texto = p_string.

  CLEAR gw_bapiret.

  CALL FUNCTION 'TR_SPLIT_TEXT'
    EXPORTING
      iv_text  = lv_texto
      iv_len   = 30
    IMPORTING
      et_lines = lt_trtexts.

  LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

    CASE sy-tabix.
      WHEN 1.
        gw_bapiret-message_v1 = <fs_line>.
      WHEN 2.
        gw_bapiret-message_v2 = <fs_line>.
      WHEN 3.
        gw_bapiret-message_v3 = <fs_line>.
      WHEN 4.
        gw_bapiret-message_v4 = <fs_line>.
    ENDCASE.

  ENDLOOP.

  gw_bapiret-id = 'DS'.
  gw_bapiret-type = lv_msgty.
  gw_bapiret-number = '016'.

  MESSAGE ID gw_bapiret-id TYPE 'S' NUMBER gw_bapiret-number
    WITH gw_bapiret-message_v1 gw_bapiret-message_v2
         gw_bapiret-message_v3 gw_bapiret-message_v4 DISPLAY LIKE gw_bapiret-type.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICAR_CAMPOS
*&---------------------------------------------------------------------*
FORM f_validacao_campos CHANGING p_erro TYPE c.

  IF zsde0004-nr_safra IS INITIAL.

    PERFORM f_put_mensagem USING 'E' 'O campo safra deve ser preenchido'.

    gv_cursor = 'ZSDE0004-NR_SAFRA'.

    p_erro = 'X'.

    "gv_error_num = 1.

    EXIT.

  ENDIF.

  IF zsde0004-filial IS INITIAL.

    PERFORM f_put_mensagem USING 'E' 'O campo filial deve ser preenhcido'.

    gv_cursor = 'ZSDE0004-FILIAL'.

    p_erro = 'X'.

    "gv_error_num = 1.

    EXIT.

  ENDIF.

  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
    ID 'WERKS' FIELD  zsde0004-filial
    ID 'ACTVT' FIELD '03'.

  IF sy-subrc NE 0 AND sy-uname NE 'RBLIMA'. " #DEBUG

    PERFORM f_put_mensagem USING 'E' 'Sem autorização para a filia selecionada'.

    p_erro = 'X'.

    "gv_error_num = 2.

    EXIT.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCAR_ORDENS
*&---------------------------------------------------------------------*
FORM f_buscar_ordens USING p_refresh TYPE c.

  CHECK gt_0005_global IS NOT INITIAL.

  IF p_refresh = 'X'.
    CLEAR gt_0006_global[].
  ENDIF.

  CHECK gt_0006_global IS INITIAL.

  CLEAR gt_187[].

  SELECT * FROM zsdt0187
    INTO TABLE gt_187
      FOR ALL ENTRIES IN gt_0005_global
        WHERE nu_compra = gt_0005_global-nu_compra
          AND nu_lote = gt_0005_global-lote.

  CHECK sy-subrc EQ 0.

  " Dados de OV
  PERFORM f_buscar_dados_ov.

  " Dados de Pedido de Compra
  PERFORM f_buscar_dados_pc.

  PERFORM f_preenche_qtde_006.

  PERFORM f_filtro_lote_alv.

  PERFORM f_refresh_grid USING 'X' space.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GLOBAL_TO_ORDENS
*&---------------------------------------------------------------------*
FORM f_global_to_ordens .

  gt_0006[] = gt_0006_global.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_SELECAO
*&---------------------------------------------------------------------*
FORM f_valida_selecao .

  " 29.09.2022 - RAMON 93305 -->
  "exit.
  " 29.09.2022 - RAMON 93305 --<

  IF go_005_alv IS INITIAL.
    "CLEAR gv_error_num.
    LEAVE TO SCREEN 0.
    EXIT.
  ENDIF.

  PERFORM f_clear_select_0005.

  CALL METHOD go_005_alv->get_selected_rows
    IMPORTING
      et_index_rows = DATA(lt_index_rows)
      et_row_no     = DATA(et_row_no).

  LOOP AT lt_index_rows ASSIGNING FIELD-SYMBOL(<fs_row>).

    READ TABLE gt_0005 ASSIGNING FIELD-SYMBOL(<fs_0005>)
      INDEX <fs_row>-index.

    CHECK sy-subrc EQ 0.

    READ TABLE gt_0005_global ASSIGNING FIELD-SYMBOL(<fs_0005_global>)
      WITH KEY bukrs      = <fs_0005>-bukrs
               nu_compra  = <fs_0005>-nu_compra
               id_compra  = <fs_0005>-id_compra
               filial_ped = <fs_0005>-filial_ped
               filial     = <fs_0005>-filial
               tp_frete   = <fs_0005>-tp_frete
               lote       = <fs_0005>-lote.

    IF sy-subrc NE 0.
      PERFORM f_put_mensagem USING 'E' 'Selecionar um lote'.
    ELSE.
      <fs_0005_global>-selec = 'X'.
      "CLEAR gv_error_num.
      "LEAVE TO SCREEN 0.
    ENDIF.

  ENDLOOP.

*  READ TABLE gt_0005_global TRANSPORTING NO FIELDS
*    WITH KEY selec = 'X'.
*
*  IF sy-subrc NE 0.
*    PERFORM f_put_mensagem USING 'E' 'Selecionar um lote'.
*  ELSE.
*    CLEAR gv_error_num.
*    LEAVE TO SCREEN 0.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CLEAR_SELECT_0005
*&---------------------------------------------------------------------*
FORM f_clear_select_0005 .

  LOOP AT gt_0005_global ASSIGNING FIELD-SYMBOL(<fs_0005>) WHERE selec = 'X'.
    CLEAR <fs_0005>-selec.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSAR
*&---------------------------------------------------------------------*
FORM f_processar .

  DATA lw_line TYPE zsde0005.
  DATA lv_count TYPE i.

  PERFORM f_preenche_textos.

  CHECK gt_0005_global IS NOT INITIAL.

  LOOP AT gt_0005_global ASSIGNING FIELD-SYMBOL(<fs_0005>) WHERE selec = 'X'.
    lw_line = <fs_0005>.
    ADD 1 TO lv_count.
  ENDLOOP.

  IF lv_count > 1.
    PERFORM f_put_mensagem USING 'E' 'Selecionar apenas um lote'.
    EXIT.
  ENDIF.

  CLEAR gt_0006.

  LOOP AT gt_0006_global ASSIGNING FIELD-SYMBOL(<fs_0006>).

    APPEND <fs_0006> TO gt_0006.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_TEXTOS
*&---------------------------------------------------------------------*
FORM f_preenche_textos .

  IF zsde0004-produtor IS NOT INITIAL.

    PERFORM f_get_descr_lfa1
      USING zsde0004-produtor
   CHANGING zsde0004-desc_produtor.

  ENDIF.

  IF zsde0004-porto IS NOT INITIAL.

    PERFORM f_get_descr_lfa1
      USING zsde0004-porto
   CHANGING zsde0004-desc_porto.

  ENDIF.

  IF zsde0004-ponto_coleta IS NOT INITIAL.

    PERFORM f_get_descr_lfa1
      USING zsde0004-ponto_coleta
   CHANGING zsde0004-desc_coleta.

  ENDIF.

  IF zsde0004-matnr IS NOT INITIAL.

    PERFORM f_get_descr_makt
      USING zsde0004-matnr
   CHANGING zsde0004-maktx.

  ENDIF.

  IF zsde0004-filial IS NOT INITIAL.

    PERFORM f_get_descr_branch
      USING zsde0004-bukrs
            zsde0004-filial
   CHANGING zsde0004-desc_filial.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_DESC
*&---------------------------------------------------------------------*
FORM f_get_descr_branch USING p_bukrs TYPE bukrs
                              p_branch TYPE j_1bbranc_
                     CHANGING p_descr TYPE name1.

  READ TABLE gt_branch TRANSPORTING NO FIELDS
    WITH KEY bukrs = p_bukrs
             branch = p_branch.

  IF sy-subrc NE 0.

    SELECT SINGLE * FROM j_1bbranch
      INTO @DATA(lw_branch)
        WHERE bukrs  = @p_bukrs
          AND branch = @p_branch.

    APPEND lw_branch TO gt_branch.

  ENDIF.

  READ TABLE gt_branch ASSIGNING FIELD-SYMBOL(<fs_branc>)
    WITH KEY bukrs = p_bukrs
             branch = p_branch.

  IF sy-subrc EQ 0.
    p_descr = <fs_branc>-name.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_DESC
*&---------------------------------------------------------------------*
FORM f_get_descr_lfa1 USING p_lifnr TYPE lifnr
                   CHANGING p_descr TYPE name1_gp.

  READ TABLE gt_lfa1 TRANSPORTING NO FIELDS
    WITH KEY lifnr = p_lifnr.

  IF sy-subrc NE 0.

    SELECT SINGLE * FROM lfa1
      INTO @DATA(lw_lfa1)
        WHERE lifnr = @p_lifnr.

    APPEND lw_lfa1 TO gt_lfa1.

  ENDIF.

  READ TABLE gt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>)
    WITH KEY lifnr = p_lifnr.

  IF sy-subrc EQ 0.
    p_descr = <fs_lfa1>-name1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_DESC
*&---------------------------------------------------------------------*
FORM f_get_descr_kna1 USING p_kunnr TYPE kunnr
                   CHANGING p_descr TYPE name1_gp.

  READ TABLE gt_kna1 TRANSPORTING NO FIELDS
    WITH KEY kunnr = p_kunnr.

  IF sy-subrc NE 0.

    SELECT SINGLE * FROM kna1
      INTO @DATA(lw_kna1)
        WHERE kunnr = @p_kunnr.

    APPEND lw_kna1 TO gt_kna1.

  ENDIF.

  READ TABLE gt_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>)
    WITH KEY kunnr = p_kunnr.

  IF sy-subrc EQ 0.
    p_descr = <fs_kna1>-name1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_DESCR_MAKT
*&---------------------------------------------------------------------*
FORM f_get_descr_makt USING p_matnr TYPE matnr
                   CHANGING p_descr TYPE maktx.

  READ TABLE gt_makt TRANSPORTING NO FIELDS
    WITH KEY matnr = p_matnr.

  IF sy-subrc NE 0.

    SELECT SINGLE * FROM makt
      INTO @DATA(lw_makt)
        WHERE matnr = @p_matnr
      AND spras = @sy-langu.

    APPEND lw_makt TO gt_makt.

  ENDIF.

  READ TABLE gt_makt ASSIGNING FIELD-SYMBOL(<fs_makt>)
    WITH KEY matnr = p_matnr.

  IF sy-subrc EQ 0.
    p_descr = <fs_makt>-maktx.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TRATAR_ZFIC
*&---------------------------------------------------------------------*
FORM f_tratar_zfic .

  CHECK gt_0005_global IS NOT INITIAL.

  DATA(lt_005_temp) = gt_0005_global.

  SELECT bukrs,branch FROM j_1bbranch
    INTO TABLE @DATA(lt_branch_lr)
      FOR ALL ENTRIES IN @gt_0005_global
        WHERE branch = @gt_0005_global-zfic_branch.

  CHECK sy-subrc EQ 0.

  SELECT bukrs,branch FROM j_1bbranch
    INTO TABLE @DATA(lt_branch_x)
      FOR ALL ENTRIES IN @gt_0005_global
        WHERE branch = @gt_0005_global-filial.

  CHECK sy-subrc EQ 0.

  LOOP AT lt_005_temp ASSIGNING FIELD-SYMBOL(<fs_005>).

    READ TABLE lt_branch_lr ASSIGNING FIELD-SYMBOL(<fs_lr>)
      WITH KEY branch = <fs_005>-zfic_branch.

    CHECK sy-subrc EQ 0.

    READ TABLE lt_branch_x ASSIGNING FIELD-SYMBOL(<fs_x>)
      WITH KEY branch = <fs_005>-filial.

    CHECK sy-subrc EQ 0.

    CHECK <fs_lr>-bukrs = <fs_x>-bukrs.

    DELETE gt_0005_global WHERE bukrs = <fs_005>-bukrs
                     AND nu_compra = <fs_005>-nu_compra
                     AND id_compra = <fs_005>-id_compra
                     AND filial =  <fs_005>-filial.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CALL_PARAM_TRANSACTION
*&---------------------------------------------------------------------*
FORM f_call_param_transaction .

  DATA lv_subrc TYPE sy-subrc.
  DATA lv_mess TYPE string.
  DATA lv_erro TYPE c.

  " 29.09.2022 - RAMON ->>
  READ TABLE gt_0005_global INTO DATA(zsde0005) WITH KEY selec = 'X'.
*
  "CHECK sy-subrc EQ 0.
  " 29.09.20202 - RAMON -<<

  BREAK rblima.

  CASE gv_param-tcode.
    WHEN 'ZSDT0132N'.

      "IF sy-uname NE 'RBLIMA'.

*        " 29.09.20202 - RAMON ->>
*      PERFORM f_check_line_selected
*        USING zsde0005
*     CHANGING lv_erro.
*
*      "ENDIF.
*
*      CHECK lv_erro IS INITIAL.
      " 29.09.20202 - RAMON -<<

      SET PARAMETER ID 'BKR' FIELD gv_param-bukrs.
      SET PARAMETER ID 'MTN' FIELD gv_param-matnr.

      " VERIFICAR O INCLUDE ZSDR0084FORM - TRANSAÇÃO: ZSDT0132
      SUBMIT zsdr0084 WITH p_tab = gt_0005_global[] AND RETURN.

    WHEN 'ME21N'.


      TRY .

          " 1 - Local de Entrega deve ser uma filial(Usar a Classe ZCL_CLIENTES->CK_PARCEIRO_LOCAL_NEGOCIO para verificação )
          zcl_clientes=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = zsde0005-local_entrega
          )->ck_parceiro_local_negocio(  ).

        CATCH zcx_parceiros.

          lv_mess = `O Local de entrega ` && zsde0005-local_entrega && ` do lote de compra não é uma filial`.

          PERFORM f_put_mensagem USING 'E' lv_mess.
          EXIT.

      ENDTRY.

      " Se Filial (Local de entrega ) e Filial do lote  pertencerem  a mesma empresa.  , ignorar a informar JSON-ProdutoDerivado
      IF ( zsde0005-lcl_entr_bukrs NE zsde0005-bukrs ) AND zsde0005-produtoderivado = 'S'.

        lv_mess = `Produto derivado é invalido para essa seleção`.

        PERFORM f_put_mensagem USING 'E' lv_mess.

        EXIT.

      ENDIF.

      " 2 - Filial (Local de entrega ) e Filial do lote  devem pertencer a mesma empresa.
      IF zsde0005-lcl_entr_bukrs NE zsde0005-bukrs.

        lv_mess = `A filial do local de entrega não é da mesma empresa da filial do Lote de compra`.

        PERFORM f_put_mensagem USING 'E' lv_mess.
        EXIT.

      ENDIF.

      gv_process = 'S'.

      PERFORM check_authority_tcode
        USING gv_param-tcode_search
     CHANGING lv_subrc.

      IF lv_subrc EQ 0.

        CALL FUNCTION 'ZMM_ME21N_DADOS_SIGAM_MEMORY'
          EXPORTING
            i_write   = 'X'
          CHANGING
            c_process = gv_process
            cs_lote   = zsde0005
            cs_screen = zsde0004.

        CALL TRANSACTION gv_param-tcode.

      ENDIF.

      CALL FUNCTION 'ZMM_ME21N_DADOS_SIGAM_MEMORY'
        EXPORTING
          i_clear = 'X'.

    WHEN OTHERS.
  ENDCASE.

  PERFORM f_buscar_ordens USING 'X'.

  PERFORM f_filtro_lote_alv.

  PERFORM f_refresh_grid USING 'X' space.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_CAB
*&---------------------------------------------------------------------*
FORM f_processa_cab USING p_header TYPE REF TO if_purchase_order_mm.

  DATA lo_header  TYPE REF TO cl_po_header_handle_mm.
  DATA ls_header TYPE mepoheader.

  lo_header ?= p_header.

  ls_header = p_header->get_data( ).

  ls_header-bukrs = gs_lote-bukrs. "'0001'.
  ls_header-bsart = 'ZUB'.
  ls_header-reswk = gs_lote-filial_ped.
  ls_header-ekorg = 'OC01'.
  ls_header-ekgrp = 'G01'.

  ls_header-aedat = sy-datum.
  ls_header-ernam = sy-uname.
  ls_header-pincr = '00010'.
  ls_header-spras = sy-langu.
  ls_header-bedat = sy-datum.
  ls_header-upinc = '00001'.
  ls_header-stceg_l = 'BR'.

  ls_header-mandt = sy-mandt.
  ls_header-bstyp = 'F'.
  ls_header-bsakz = 'T'.
  ls_header-lands = 'BR'.
  ls_header-id = '1'.

  lo_header->my_ibs_firewall_on = 'X'.
  lo_header->my_cust_firewall_on = 'X'.

  lo_header->set_data( ls_header ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM  F_PROCESSA_ITEM
*&---------------------------------------------------------------------*
FORM f_processa_item USING p_header TYPE REF TO if_purchase_order_mm.

  TYPES: BEGIN OF sorted_by_flushable,
           flushable      TYPE REF TO if_flush_transport_mm,
           changed_models TYPE mmpur_models,
         END OF sorted_by_flushable.

  DATA ls_flush_todo  TYPE sorted_by_flushable.
  DATA ls_item TYPE mepoitem.

  DATA lt_model TYPE TABLE OF mmpur_models.
  DATA lo_header  TYPE REF TO cl_po_header_handle_mm.

  lo_header ?= p_header.

  CALL METHOD lo_header->if_purchase_order_mm~create_item
    RECEIVING
      re_item = DATA(lo_item).

  ls_item-id = '2'.
  ls_item-ebelp = '00010'.
  ls_item-matnr = gs_lote-matnr.
  ls_item-ematn = gs_lote-matnr.
  ls_item-menge = gs_lote-saldo.

  PERFORM f_local_to_werks
    USING gs_lote-local_entrega
 CHANGING ls_item-werks.

  "  ls_item-werks = lv_werks.
  ls_item-lgort = 'ARMZ'.
  ls_item-charg = gs_lote-nr_safra.

  lo_item->set_data( ls_item ).

  ls_flush_todo-flushable = lo_header.

  APPEND INITIAL LINE TO ls_flush_todo-changed_models ASSIGNING FIELD-SYMBOL(<fs_model>).

  <fs_model>-model ?= lo_item.

  CALL METHOD lo_header->if_flush_transport_mm~start
    EXPORTING
      im_models    = ls_flush_todo-changed_models
    EXCEPTIONS
      illegal_call = 01
      error        = 02.

  IF NOT sy-subrc IS INITIAL.
    "l_at_least_one_failed = mmpur_yes.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM  F_PROCESSA_PARCEIRO
*&---------------------------------------------------------------------*
FORM f_processa_parceiro USING p_header TYPE REF TO if_purchase_order_mm.

  DATA x_mmpa TYPE TABLE OF mmpa.
  DATA y_mmpa TYPE TABLE OF mmpa.

  DATA(ls_header) = p_header->get_data( ).

  APPEND INITIAL LINE TO x_mmpa ASSIGNING FIELD-SYMBOL(<fs_mmpa>).
  <fs_mmpa>-mandt = sy-mandt.
  <fs_mmpa>-ekorg = ls_header-ekorg.
  <fs_mmpa>-parvw = 'PR'.
  <fs_mmpa>-parza = '001'.
  <fs_mmpa>-ernam = sy-uname.
  <fs_mmpa>-erdat = sy-datum.
  <fs_mmpa>-lifn2 = gs_lote-ponto_coleta.

  y_mmpa = x_mmpa.

  CALL FUNCTION 'MM_MAINTAIN_PARTNERS'
    EXPORTING
      application    = 'P'
      bstyp          = ls_header-bstyp
      ekorg          = ls_header-ekorg
      lifnr          = gs_lote-ponto_coleta
      pargr          = '0003'
      bukrs          = ls_header-bukrs
      subscreen_mode = 'PBO'
      aktyp          = 'A'
    TABLES
      x_mmpa         = x_mmpa
      y_mmpa         = y_mmpa.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_PROCESS
*&---------------------------------------------------------------------*
FORM f_check_process CHANGING p_erro TYPE c.

  CALL FUNCTION 'ZMM_ME21N_DADOS_SIGAM_MEMORY'
    CHANGING
      c_process = gv_process
      cs_lote   = gs_lote
      cs_screen = gs_screen.

  CHECK gv_process IS INITIAL.

  p_erro = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_ZSDT0187
*&---------------------------------------------------------------------*
FORM f_save_zsdt0187_2 USING p_0158 TYPE zsdt0158.

*  DATA ls_187 TYPE zsdt0187.
*
*  DATA lo_header TYPE REF TO cl_po_header_handle_mm.
*  DATA lo_item    TYPE REF TO cl_po_item_handle_mm.
*
*  DATA ls_item TYPE mepoitem.
*
*  CREATE OBJECT lo_header
*    EXPORTING
*      im_po_number = p_0158-ebeln
*    EXCEPTIONS
*      failure      = 1
*      OTHERS       = 2.
*
*  IF sy-subrc <> 0.
*    EXIT.
*  ENDIF.
*
*  lo_header->get_data(  IMPORTING ex_data = DATA(ls_header)  ).
*  "lo_header->get_items( IMPORTING ex_items = DATA(lo_items)   ).
*
*  "CHECK lo_items IS NOT INITIAL.
*
*  "lo_item = lo_items[ 1 ].
*
*  "lo_item->get_data( IMPORTING ex_data = ls_item ).
*
*  CHECK ls_header-ebeln IS NOT INITIAL.
*
*  CALL FUNCTION 'NUMBER_GET_NEXT'
*    EXPORTING
*      nr_range_nr             = '01'
*      object                  = 'Z_ID_VINC'
*    IMPORTING
*      number                  = ls_187-id_vinculo
*    EXCEPTIONS
*      interval_not_found      = 1
*      number_range_not_intern = 2
*      object_not_found        = 3
*      quantity_is_0           = 4
*      quantity_is_not_1       = 5
*      interval_overflow       = 6
*      buffer_overflow         = 7
*      OTHERS                  = 8.
*
*  IF sy-subrc <> 0.
*    "PERFORM f_mensagem_sistema.
*    EXIT.
*  ENDIF.
*
*  "ls_187-nu_compra = p_0158-nu_compra.
*  "ls_187-nu_lote = p_0158-lote.
*
*  ls_187-id_forn = p_0158-produtor.
*  ls_187-safra = p_0158-nr_safra.
*  ls_187-bukrs = p_0158-bukrs.
*  ls_187-werks = p_0158-filial.
*
*  ls_187-id_ponto_coleta = p_0158-ponto_coleta.
*  ls_187-id_local_destino = p_0158-local_entrega.
*
*  PERFORM f_preenche_porto
*    USING p_0158-porto
* CHANGING ls_187-id_terminal.
*
*  "ls_187-id_terminal = gs_lote-porto.
*  ls_187-compra = gs_lote-id_compra.
*
*  ls_187-id_produto = p_0158-matnr.
*  ls_187-quantidade = p_0158-qtde.
*  ls_187-saldo_entregar = p_0158-saldo.
*  ls_187-unidade = p_0158-un_lote.
*  ls_187-tp_operacao = p_0158-tipooperacao.
*  ls_187-tp_producao = p_0158-transgenia.
*
*  ls_187-tp_frete_compra = p_0158-tp_frete.
*  ls_187-ano_liquidez = p_0158-ano.
*  ls_187-mes_liquidez = p_0158-mes.
*  ls_187-periodo_liquidez = p_0158-periodo.
*  ls_187-produto_derivado = p_0158-produtoderivado.
*
*  ls_187-data_atual = sy-datum.
*  ls_187-hora_atual = sy-uzeit.
*
*  ls_187-ebeln = ls_header-ebeln.
*  ls_187-ebelp = ls_item-ebelp.
*  ls_187-bsart = ls_header-bsart.
*
*  MODIFY zsdt0187 FROM ls_187.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_ZSDT0187
*&---------------------------------------------------------------------*
FORM f_save_zsdt0187 USING p_header TYPE REF TO if_purchase_order_mm.

  DATA ls_187 TYPE zsdt0187.
  DATA lo_header TYPE REF TO cl_po_header_handle_mm.
  DATA lo_item    TYPE REF TO cl_po_item_handle_mm.

  DATA ls_item TYPE mepoitem.

  lo_header ?= p_header.

  DATA(ls_header) = p_header->get_data( ).

  DATA(lo_items) = p_header->get_items( ).

  CHECK lo_items IS NOT INITIAL.

  lo_item ?= lo_items[ 1 ]-item.

  lo_item->get_data( IMPORTING ex_data = ls_item ).

  CHECK ls_header-ebeln IS NOT INITIAL.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'Z_ID_VINC'
    IMPORTING
      number                  = ls_187-id_vinculo
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF sy-subrc <> 0.
    "PERFORM f_mensagem_sistema.
    EXIT.
  ENDIF.

  ls_187-nu_compra = gs_lote-nu_compra.
  ls_187-nu_lote = gs_lote-lote.

  ls_187-id_forn = gs_lote-produtor.
  ls_187-safra = gs_lote-nr_safra.
  ls_187-bukrs = gs_lote-bukrs.
  ls_187-werks = gs_lote-filial.

  ls_187-id_ponto_coleta = gs_lote-ponto_coleta.
  ls_187-id_local_destino = gs_lote-local_entrega.

  PERFORM f_preenche_porto
    USING gs_lote-porto
 CHANGING ls_187-id_terminal.

  "ls_187-id_terminal = gs_lote-porto.
  ls_187-compra = gs_lote-id_compra.

  ls_187-id_produto = gs_lote-matnr.
  ls_187-quantidade = gs_lote-qtde.
  ls_187-saldo_entregar = gs_lote-saldo.
  ls_187-unidade = gs_lote-un_lote.
  ls_187-tp_operacao = gs_lote-tipooperacao.
  ls_187-tp_producao = gs_lote-transgenia.

  ls_187-tp_frete_compra = gs_lote-tp_frete.
  ls_187-ano_liquidez = gs_lote-ano.
  ls_187-mes_liquidez = gs_lote-mes.
  ls_187-periodo_liquidez = gs_lote-periodo.
  ls_187-produto_derivado = gs_lote-produtoderivado.

  ls_187-data_atual = sy-datum.
  ls_187-hora_atual = sy-uzeit.

  ls_187-ebeln = ls_header-ebeln.
  ls_187-ebelp = ls_item-ebelp.
  ls_187-bsart = ls_header-bsart.

  MODIFY zsdt0187 FROM ls_187.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_EMAIL_CHANGE_LOTE
*&---------------------------------------------------------------------*
FORM check_email_change_lote USING p_header TYPE REF TO if_purchase_order_mm.

  DATA lo_header TYPE REF TO cl_po_header_handle_mm.
  DATA lo_item    TYPE REF TO cl_po_item_handle_mm.

  DATA ls_item TYPE mepoitem.
  DATA lt_html TYPE html_table.

  DATA lv_werks TYPE werks_d.

  lo_header ?= p_header.

  DATA(ls_header) = p_header->get_data( ).

  DATA(lo_items) = p_header->get_items( ).

  DATA(lt_partn) = lo_header->if_purchasing_document~get_partners( ).

  CHECK lo_items IS NOT INITIAL.

  lo_item ?= lo_items[ 1 ]-item.

  lo_item->get_data( IMPORTING ex_data = ls_item ).

  IF lt_partn IS NOT INITIAL.
    DATA(ls_partn) = lt_partn[ 1 ].
  ENDIF.


  PERFORM f_add_html_line
    USING 'Tipo Pedido'
          gs_lote-nu_compra
          'ZUB'
          ls_header-bsart
 CHANGING lt_html.

  PERFORM f_add_html_line
    USING 'Centro Forn.'
          gs_lote-nu_compra
          gs_lote-filial
          ls_header-reswk
 CHANGING lt_html.

  PERFORM f_add_html_line
    USING 'Ponto Coleta'
          gs_lote-nu_compra
          'PR'
          ls_partn-data-parvw
 CHANGING lt_html.

  PERFORM f_add_html_line
    USING 'Ponto Coleta'
          gs_lote-nu_compra
          gs_lote-ponto_coleta
          ls_partn-data-parno
 CHANGING lt_html.

  PERFORM f_add_html_line
    USING 'Material'
          gs_lote-nu_compra
          gs_lote-matnr
          ls_item-matnr
 CHANGING lt_html.

  PERFORM f_local_to_werks
    USING gs_lote-local_entrega
 CHANGING lv_werks.

  PERFORM f_add_html_line
    USING 'Local Entrega'
          gs_lote-nu_compra
          lv_werks
          ls_item-werks
 CHANGING lt_html.

  PERFORM f_add_html_line
    USING 'Safra'
          gs_lote-nu_compra
          gs_lote-nr_safra
          ls_item-charg
 CHANGING lt_html.

  PERFORM f_add_html_close CHANGING lt_html.

  CHECK lt_html IS NOT INITIAL.

  CALL METHOD zcl_send_email=>send_static
    EXPORTING
      i_receivers = 'EMPRESA_P&L_EMAIL'
      i_subject   = 'Lotes de compra com alteração de dados'
      i_body      = lt_html.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f_check_change_email
*&---------------------------------------------------------------------*
FORM f_add_html_line USING p_field TYPE c
                           p_nu_compra TYPE num8
                           p_line_old TYPE any
                           p_line_new TYPE any
                  CHANGING p_html TYPE html_table.

  DATA lw_html TYPE w3html.

  DATA lv_old TYPE c LENGTH 100.
  DATA lv_new TYPE c LENGTH 100.

  CHECK p_line_new IS NOT INITIAL.

  CHECK p_line_old NE p_line_new.

  WRITE p_line_old TO lv_old LEFT-JUSTIFIED.
  WRITE p_line_new TO lv_new LEFT-JUSTIFIED.

  IF p_html IS INITIAL.
    PERFORM f_add_html_ini CHANGING p_html.
  ENDIF.

  lw_html-line = `<tr style="height: 36px;">`.
  APPEND lw_html TO p_html.

  lw_html-line = `<td style="width: 135.516px; height: 36px;" height="20">` && p_nu_compra && `</td>`.
  APPEND lw_html TO p_html.

  lw_html-line = `<td style="width: 115.297px; height: 36px;">` && p_field && `</td>`.
  APPEND lw_html TO p_html.

  lw_html-line = `<td style="width: 210.094px; height: 36px;">` && lv_old && `</td>`.
  APPEND lw_html TO p_html.

  lw_html-line = `<td style="width: 209.094px; height: 36px;">` && lv_new && `</td></tr>`.
  APPEND lw_html TO p_html.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f_check_change_email
*&---------------------------------------------------------------------*
FORM f_add_html_ini CHANGING p_html TYPE html_table.

  DATA lw_html TYPE w3html.

  lw_html-line = `<table style="height: 201px;background-color:#FCFCFC;" width="100%"><tbody>`.
  APPEND lw_html TO p_html.

  lw_html-line = `<tr style="height: 21px;background-color:#D9D9D9" >`.
  APPEND lw_html TO p_html.

  lw_html-line = `<td style="width: 135.516px; height: 21px;" height="21"><strong>Lote</strong></td>`.
  APPEND lw_html TO p_html.

  lw_html-line = `<td style="width: 115.297px; height: 21px;"><strong>Dado</strong></td>`.
  APPEND lw_html TO p_html.

  lw_html-line = `<td style="width: 210.094px; height: 21px;"><strong>Antes</strong></td>`.
  APPEND lw_html TO p_html.

  lw_html-line = `<td style="width: 209.094px; height: 21px;"><strong>Atual</strong></td></tr>`.
  APPEND lw_html TO p_html.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f_check_change_email
*&---------------------------------------------------------------------*
FORM f_add_html_close CHANGING p_html TYPE html_table.

  CHECK p_html IS NOT INITIAL.

  APPEND INITIAL LINE TO p_html ASSIGNING FIELD-SYMBOL(<fs_html>).

  <fs_html>-line = `</tr></tbody></table>`.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LOCAL_TO_WERKS
*&---------------------------------------------------------------------*
FORM f_local_to_werks USING p_kunnr TYPE kunnr
                   CHANGING p_werks TYPE werks_d.

  DATA lv_werks TYPE kunnr.

  CHECK p_kunnr IS NOT INITIAL.

  lv_werks = gs_lote-local_entrega.

  SHIFT lv_werks LEFT DELETING LEADING '0'.

  UNPACK lv_werks TO p_werks.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCAR_DADOS_OV
*&---------------------------------------------------------------------*
FORM f_buscar_dados_ov .

  DATA lt_vbak TYPE TABLE OF vbak.
  DATA lt_vbfa TYPE TABLE OF vbfa.
  DATA lt_187 TYPE TABLE OF zsdt0187.

  lt_187 = gt_187.

  SORT lt_187 BY nro_sol_ov ASCENDING.

  DELETE lt_187 WHERE nro_sol_ov IS INITIAL.

  CHECK lt_187 IS NOT INITIAL.

  SELECT * FROM zsdt0066
    INTO TABLE gt_066
      FOR ALL ENTRIES IN lt_187
        WHERE nro_sol_ov  = lt_187-nro_sol_ov
          "AND ponto_c = gt_187-id_forn
          AND vbeln <> space.

  CHECK sy-subrc EQ 0.

  SELECT * FROM vbak
    INTO TABLE lt_vbak
    FOR ALL ENTRIES IN gt_066
      WHERE vbeln = gt_066-vbeln.

  CHECK sy-subrc EQ 0.

  SELECT * FROM vbfa
    INTO TABLE lt_vbfa
      FOR ALL ENTRIES IN gt_066
        WHERE vbeln = gt_066-vbeln
          AND posnv = '000010'
          AND vbtyp_n = 'J'.

  LOOP AT gt_066 ASSIGNING FIELD-SYMBOL(<fs_066>).

    READ TABLE lt_187 ASSIGNING FIELD-SYMBOL(<fs_187>)
      WITH KEY nro_sol_ov = <fs_066>-nro_sol_ov.

    CHECK sy-subrc EQ 0.

    READ TABLE lt_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>)
      WITH KEY vbeln = <fs_066>-vbeln.

    CHECK sy-subrc EQ 0.

    READ TABLE gt_0005_global ASSIGNING FIELD-SYMBOL(<fs_0005>)
      WITH KEY nu_compra = <fs_187>-nu_compra
               lote      = <fs_187>-nu_lote.

    CHECK sy-subrc EQ 0.

    APPEND INITIAL LINE TO gt_0006_global ASSIGNING FIELD-SYMBOL(<fs_0006>).

    <fs_0006>-nu_compra = <fs_0005>-nu_compra.
    <fs_0006>-lote = <fs_0005>-lote.
    <fs_0006>-ov_pedido = <fs_066>-vbeln.
    <fs_0006>-filial = <fs_187>-werks.
    <fs_0006>-auart = <fs_vbak>-auart.
    <fs_0006>-id_compra = <fs_187>-compra.
    <fs_0006>-zmeng = <fs_066>-zmeng.
    <fs_0006>-zieme = <fs_066>-zieme.

    LOOP AT lt_vbfa ASSIGNING FIELD-SYMBOL(<fs_vbfa>) WHERE vbeln = <fs_066>-vbeln.

      IF <fs_vbfa>-plmin = '-'.
        <fs_vbfa>-rfmng = <fs_vbfa>-rfmng * -1.
      ENDIF.

      ADD <fs_vbfa>-rfmng TO <fs_0006>-rfmng.

    ENDLOOP.

    <fs_0006>-matnr = <fs_066>-matnr.
    <fs_0006>-produtor = <fs_187>-id_forn.
    <fs_0006>-desc_produtor = <fs_0005>-desc_produtor.
    <fs_0006>-ponto_c = <fs_066>-ponto_c.
    <fs_0006>-desc_ponto_c = <fs_0005>-desc_coleta.
    <fs_0006>-entrega = <fs_066>-lentrega.
    <fs_0006>-desc_entrega = <fs_0005>-desc_local.
    <fs_0006>-terminal = <fs_066>-terminal.
    <fs_0006>-desc_terminal = <fs_0005>-desc_porto.
    <fs_0006>-tcode = 'ZSDT0132N'.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCAR_DADOS_PC
*&---------------------------------------------------------------------*
FORM f_buscar_dados_pc .

  DATA lt_ekko TYPE TABLE OF ekko.
  DATA lt_ekpo TYPE TABLE OF ekpo.
  DATA lt_ekbe TYPE TABLE OF ekbe.

  DATA lt_187 TYPE TABLE OF zsdt0187.

  lt_187 = gt_187.

  SORT lt_187 BY ebeln ASCENDING.

  DELETE lt_187 WHERE ebeln IS INITIAL.

  CHECK lt_187 IS NOT INITIAL.

  SELECT * FROM ekko
    INTO TABLE lt_ekko
      FOR ALL ENTRIES IN lt_187
        WHERE ebeln = lt_187-ebeln.

  DELETE lt_ekko WHERE loekz = 'X'.

  CHECK lt_ekko IS NOT INITIAL.

  SELECT * FROM ekpo
    INTO TABLE lt_ekpo
       FOR ALL ENTRIES IN lt_ekko
        WHERE ebeln = lt_ekko-ebeln.

  DELETE lt_ekpo WHERE loekz = 'X'.

  CHECK lt_ekpo IS NOT INITIAL.

  SELECT * FROM ekbe
    INTO TABLE lt_ekbe
      FOR ALL ENTRIES IN lt_ekpo
        WHERE ebeln = lt_ekpo-ebeln
          AND ebelp = lt_ekpo-ebelp.

  LOOP AT lt_ekpo ASSIGNING FIELD-SYMBOL(<fs_ekpo>).

    READ TABLE lt_ekko ASSIGNING FIELD-SYMBOL(<fs_ekko>)
      WITH KEY ebeln = <fs_ekpo>-ebeln.

    CHECK sy-subrc EQ 0.

    READ TABLE lt_187 ASSIGNING FIELD-SYMBOL(<fs_187>)
      WITH KEY ebeln = <fs_ekpo>-ebeln
               ebelp = <fs_ekpo>-ebelp.

    CHECK sy-subrc EQ 0.

    READ TABLE gt_0005_global ASSIGNING FIELD-SYMBOL(<fs_0005>)
      WITH KEY nu_compra = <fs_187>-nu_compra
               lote      = <fs_187>-nu_lote.

    CHECK sy-subrc EQ 0.

    APPEND INITIAL LINE TO gt_0006_global ASSIGNING FIELD-SYMBOL(<fs_0006>).

    <fs_0006>-ov_pedido = <fs_187>-ebeln.
    <fs_0006>-nu_compra = <fs_187>-nu_compra.
    <fs_0006>-lote = <fs_187>-nu_lote.
    <fs_0006>-ebeln = <fs_187>-ebeln.
    <fs_0006>-ebelp = <fs_187>-ebelp.

    <fs_0006>-auart = <fs_187>-bsart.

    <fs_0006>-filial = <fs_187>-werks.
    "<fs_0006>-bsart = <fs_ekko>-bsart.
    <fs_0006>-id_compra = <fs_187>-compra.
    <fs_0006>-zmeng = <fs_ekpo>-menge.

    LOOP AT lt_ekbe ASSIGNING FIELD-SYMBOL(<fs_ekbe>)
        WHERE ebeln = <fs_187>-ebeln
          AND ebelp = <fs_ekpo>-ebelp.

      IF <fs_ekbe>-shkzg = 'H'.
        ADD <fs_ekbe>-menge TO <fs_0006>-rfmng.
      ELSE. "<fs_ekbe>-SHKZG = 'S'.
        SUBTRACT <fs_ekbe>-menge FROM <fs_0006>-rfmng.
      ENDIF.

    ENDLOOP.

    IF <fs_0006>-rfmng < 0.
      <fs_0006>-rfmng = <fs_0006>-rfmng * -1.
    ENDIF.

    <fs_0006>-zieme = <fs_ekpo>-meins.

    <fs_0006>-matnr = <fs_ekpo>-matnr.
    <fs_0006>-produtor = <fs_187>-id_forn.
    <fs_0006>-desc_produtor = <fs_0005>-desc_produtor.
    <fs_0006>-ponto_c = <fs_0005>-ponto_coleta.
    <fs_0006>-desc_ponto_c = <fs_0005>-desc_coleta.

    <fs_0006>-entrega = <fs_0005>-local_entrega.
    <fs_0006>-tcode = 'ME21N'.

    IF <fs_0006>-desc_terminal IS INITIAL.

      SELECT SINGLE name1 FROM lfa1
        INTO <fs_0006>-desc_entrega
          WHERE lifnr = <fs_0006>-entrega.

    ENDIF.

    "<fs_0006>-desc_entrega = <fs_0005>-desc_local.
    <fs_0006>-terminal = <fs_0005>-porto.
    "<fs_0006>-desc_terminal = <fs_0005>-desc_porto.

    IF <fs_0006>-desc_terminal IS INITIAL.

      SELECT SINGLE name1 FROM lfa1
        INTO <fs_0006>-desc_terminal
          WHERE lifnr = <fs_0006>-terminal.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTHORITY_TCODE
*&---------------------------------------------------------------------*
FORM check_authority_tcode USING p_tcode LIKE sy-tcode
                        CHANGING p_subrc LIKE sy-subrc.
  p_subrc = 0.

  CALL FUNCTION 'SUSR_AUTHORITY_CHECK_S_TCODE'
    EXPORTING
      tcode = p_tcode
    IMPORTING
      rc    = p_subrc.

  IF p_subrc <> 0.
    EXIT.
  ENDIF.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = p_tcode
    EXCEPTIONS
      ok     = 00
      not_ok = 02.

  p_subrc = sy-subrc.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PESQUISAR
*&---------------------------------------------------------------------*
FORM f_pesquisar .

  DATA lv_subrc TYPE sy-subrc.

  PERFORM check_authority_tcode
    USING gv_param-tcode_search
 CHANGING lv_subrc.

  CHECK lv_subrc EQ 0.

  CALL TRANSACTION gv_param-tcode_search.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_LINE_SELECTED
*&---------------------------------------------------------------------*
FORM f_check_line_selected USING p_line TYPE zsde0005
                        CHANGING p_erro TYPE c.

  DATA lv_mess TYPE string.

  IF p_line-tp_frete = 'CIF'.

    lv_mess = `O tipo de frete CIF é invalido para essa seleção`.

    PERFORM f_put_mensagem USING 'E' lv_mess.

    p_erro = 'X'.

    EXIT.

  ENDIF.

*  IF p_line-produtoderivado = 'S'.
*
*    lv_mess = `Produto derivado é invalido para essa seleção`.
*
*    PERFORM f_put_mensagem USING 'E' lv_mess.
*
*    p_erro = 'X'.
*
*    EXIT.
*
*  ENDIF.

  IF p_line-local_entrega = p_line-filial.

    lv_mess = `Local de entrega não pode ser igual ao campo filial`.

    PERFORM f_put_mensagem USING 'E' lv_mess.

    p_erro = 'X'.

    EXIT.

  ENDIF.

  READ TABLE gt_branch_lr ASSIGNING FIELD-SYMBOL(<fs_lr>)
    WITH KEY branch = p_line-zfic_branch.

  CHECK sy-subrc EQ 0.

  READ TABLE gt_branch_x ASSIGNING FIELD-SYMBOL(<fs_x>)
    WITH KEY branch = p_line-filial.

  CHECK sy-subrc EQ 0.

  IF <fs_lr>-bukrs = <fs_x>-bukrs.

    lv_mess = `Filial ZFIC igual filial do lote`.

    PERFORM f_put_mensagem USING 'E' lv_mess.

    p_erro = 'X'.

    EXIT.

  ENDIF.

  "PERFORM f_tratar_zfic.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_ZFIC
*&---------------------------------------------------------------------*
FORM f_seleciona_zfic .

  CHECK gt_0005_global IS NOT INITIAL.

  SELECT * FROM j_1bbranch
    INTO TABLE gt_branch_lr
      FOR ALL ENTRIES IN gt_0005_global
        WHERE branch = gt_0005_global-zfic_branch.

  CHECK sy-subrc EQ 0.

  SELECT * FROM j_1bbranch
    INTO TABLE gt_branch_x
      FOR ALL ENTRIES IN gt_0005_global
        WHERE branch = gt_0005_global-filial.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_PORTO
*&---------------------------------------------------------------------*
FORM f_preenche_porto USING p_porto TYPE c
                   CHANGING p_terminal TYPE lifnr.

  DATA lt_split TYPE TABLE OF kunnr.

  SPLIT p_porto AT ',' INTO TABLE lt_split.

  LOOP AT lt_split ASSIGNING FIELD-SYMBOL(<fs_porto>).

    CHECK <fs_porto> IS NOT INITIAL.

    p_terminal = <fs_porto>.

    EXIT.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT_CONF
*&---------------------------------------------------------------------*
FORM f_fieldcat_conf USING p_type TYPE c
                  CHANGING p_fc_tab TYPE lvc_t_fcat.

  " C - CABEÇALHO / gt_0005_global
  " R - RODAPE / GT_0006

  CASE gv_param-tcode.
    WHEN 'ME21N'.

      IF p_type = 'R'.

        DELETE p_fc_tab WHERE fieldname = 'LOTE'.
        DELETE p_fc_tab WHERE fieldname = 'NU_COMPRA'.
        DELETE p_fc_tab WHERE fieldname = 'ZIEME'.

        DELETE p_fc_tab WHERE fieldname = 'EBELN'.
        DELETE p_fc_tab WHERE fieldname = 'EBELP'.

      ENDIF.

    WHEN 'ZSDT0132N'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FILTRO_LOTE_ALV
*&---------------------------------------------------------------------*
FORM f_filtro_lote_alv .

  gt_0005 = gt_0005_global.

  "CHECK gt_0005 IS NOT INITIAL.

  CASE 'X'.
    WHEN zsde0004-rb_lotes_filial.

      " Traz os iguais, apaga os diferentes
      DELETE gt_0005 WHERE filial <> zsde0004-filial.

    WHEN zsde0004-rb_lotes_outras.

      " Traz os diferentes, apaga os iguais
      DELETE gt_0005 WHERE filial = zsde0004-filial.

    WHEN zsde0004-rb_lotes_todos.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_user_command
*&---------------------------------------------------------------------*
FORM f_user_command_popup USING p_ucomm TYPE salv_de_function.

  CASE p_ucomm.

    WHEN 'PROC'.

      gv_popup_processa = 'X'.

      LEAVE TO SCREEN 0.
    WHEN OTHERS.

      gv_popup_processa = space.

      LEAVE TO SCREEN 0.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_fill_selected_rows
*&---------------------------------------------------------------------*
FORM f_fill_selected_rows_popup .

  DATA: lo_selections TYPE REF TO cl_salv_selections.

  DATA lt_rows TYPE salv_t_row.

  DATA ls_row TYPE i.

  CLEAR gt_rows.

  lo_selections = go_alv->get_selections( ).

  gt_rows = lo_selections->get_selected_rows( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_QTDE_006
*&---------------------------------------------------------------------*
FORM f_preenche_qtde_006 .

  CHECK gt_187 IS NOT INITIAL.

  LOOP AT gt_0005_global ASSIGNING FIELD-SYMBOL(<fs_0005>).

    CLEAR <fs_0005>-qtde_006.

    LOOP AT gt_0006_global ASSIGNING FIELD-SYMBOL(<fs_0006>)
      WHERE nu_compra = <fs_0005>-nu_compra
        AND lote = <fs_0005>-lote.

      "CHECK gv_param-tcode = <fs_0006>-tcode.

*      CASE gv_param-tcode.
*        WHEN 'ZSDT0132N'.
*          CHECK <fs_0006>-ov_pedido IS NOT INITIAL.
*          CHECK <fs_0006>-ebeln IS INITIAL.
*        WHEN 'ME21N'.
*          CHECK <fs_0006>-ebeln IS NOT INITIAL.
*      ENDCASE.

      ADD 1 TO <fs_0005>-qtde_006.

    ENDLOOP.

  ENDLOOP.

  "PERFORM f_refresh_grid USING 'X' space.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CALL_TRANSACTION
*&---------------------------------------------------------------------*
FORM f_call_transaction .

  CHECK gv_param-tcode IS NOT INITIAL.

  SET PARAMETER ID 'BKR' FIELD gv_param-bukrs.
  SET PARAMETER ID 'MTN' FIELD gv_param-matnr.

  CALL TRANSACTION gv_param-tcode.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_RECUPERA_REGISTRO
*&---------------------------------------------------------------------*
FORM f_recupera_registro USING p_seq TYPE c
                      CHANGING p_0158 TYPE zsdt0158.

  " se nao for initial, então nao precisa selecionar de novo
  CHECK p_0158 IS INITIAL.

  IF p_seq IS NOT INITIAL.

    SELECT SINGLE * FROM zsdt0158
      INTO p_0158
        WHERE sequencial = p_seq
        AND status = 'L'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_REG_SOLIC
*&---------------------------------------------------------------------*
FORM f_valida_reg_solic USING p_0158 TYPE zsdt0158
                    CHANGING p_erro TYPE c
                             p_ret TYPE bapiret2_t.

  IF p_0158 IS INITIAL.

    " Não encontrado

    p_erro = 'X'.
    EXIT.

  ENDIF.

  IF p_0158-status NE 'L'.

    " status não é liberado

    p_erro = 'X'.
    EXIT.

  ENDIF.

  IF p_0158-nro_sol_ov IS NOT INITIAL.

    " já existe numero de solicitação

    p_erro = 'X'.
    EXIT.

  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GERA_NUMERACAO
*&---------------------------------------------------------------------*
FORM f_gera_numeracao CHANGING p_0158 TYPE zsdt0158.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZNR_SOL_OV'
    IMPORTING
      number                  = p_0158-nro_sol_ov
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GERA_PEDIDO_COMPRAS
*&---------------------------------------------------------------------*
FORM f_gera_pedido_compras USING p_commit TYPE flag
                                 p_0158 TYPE zsdt0158
                                 p_route TYPE route
                        CHANGING p_ebeln TYPE ebeln
                                 p_ret TYPE bapiret2_t.

  DATA lw_header TYPE bapimepoheader.
  DATA lw_headerx TYPE bapimepoheaderx.

  DATA lt_item TYPE TABLE OF bapimepoitem.
  DATA lt_itemx TYPE TABLE OF bapimepoitemx.

  DATA lt_partner TYPE TABLE OF bapiekkop.

  DATA lt_shipping TYPE TABLE OF bapiitemship.
  DATA lt_shippingx TYPE TABLE OF bapiitemshipx.

  DATA lw_exp LIKE bapimepoheader.

  CLEAR p_ret.

  PERFORM f_preenche_cab USING p_0158 CHANGING lw_header lw_headerx.

  PERFORM f_preenche_item USING p_0158 CHANGING lt_item lt_itemx.

  CHECK lt_item IS NOT INITIAL.

  APPEND INITIAL LINE TO lt_partner ASSIGNING FIELD-SYMBOL(<fs_partner>).

  <fs_partner>-partnerdesc = 'PR'.
  <fs_partner>-langu = sy-langu .
  <fs_partner>-buspartno = p_0158-id_ponto_coleta.

  APPEND INITIAL LINE TO lt_shipping ASSIGNING FIELD-SYMBOL(<fs_shipping>).
  <fs_shipping>-po_item = lt_item[ 1 ]-po_item.
  <fs_shipping>-route = p_route.

  APPEND INITIAL LINE TO lt_shippingx ASSIGNING FIELD-SYMBOL(<fs_shippingx>).
  <fs_shippingx>-po_item = lt_item[ 1 ]-po_item.
  <fs_shippingx>-route = abap_true.

  CALL FUNCTION 'BAPI_PO_CREATE1' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      poheader    = lw_header
      poheaderx   = lw_headerx
    IMPORTING
      expheader   = lw_exp
    TABLES
      return      = p_ret
      poitem      = lt_item
      poitemx     = lt_itemx
      popartner   = lt_partner
      poshipping  = lt_shipping
      poshippingx = lt_shippingx.

  " Não existe linha de erro
  CHECK NOT line_exists( p_ret[ type = 'E' ] ).

  p_ebeln = lw_exp-po_number.

  CHECK p_commit = abap_true.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_CAB
*&---------------------------------------------------------------------*
FORM f_preenche_cab USING p_0158 TYPE zsdt0158
                 CHANGING p_header TYPE bapimepoheader
                          p_headerx TYPE bapimepoheaderx.

  p_header-comp_code   = p_0158-bukrs.
  p_header-doc_type       = 'ZUB'.
  p_header-suppl_plnt     = p_0158-filial.
  p_header-purch_org     = 'OC01'.
  p_header-pur_group     = 'G01'.
  p_header-currency        = 'BRL'.
  p_header-creat_date     = sy-datum.

  p_headerx-comp_code  = abap_true.
  p_headerx-doc_type   = abap_true.
  p_headerx-suppl_plnt = abap_true.
  p_headerx-purch_org  = abap_true.
  p_headerx-pur_group  = abap_true.
  p_headerx-currency   = abap_true.
  p_headerx-creat_date = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_ITEM
*&---------------------------------------------------------------------*
FORM f_preenche_item USING p_0158 TYPE zsdt0158
                  CHANGING p_item TYPE bapimepoitem_tp
                           p_itemx TYPE bapimepoitemx_tp.

  DATA: lit_ekpo_ext_memory TYPE me_ekpo.

  DATA lo_cliente TYPE REF TO zcl_clientes.

  FREE MEMORY ID 'LIT_EKPO_EXTENSION'.

  APPEND INITIAL LINE TO p_item ASSIGNING FIELD-SYMBOL(<fs_item>).
  APPEND INITIAL LINE TO p_itemx ASSIGNING FIELD-SYMBOL(<fs_itemx>).

  <fs_item>-po_item = '00010'.
* ---> S4 Migration - 04/07/2023 - FTM - Início
  <fs_item>-material = p_0158-id_produto.
  DATA(v_len) = strlen( p_0158-id_produto ).
  IF v_len > 18.
    <fs_item>-material_long = p_0158-id_produto.
  ELSE.
    <fs_item>-material      = p_0158-id_produto.
  ENDIF.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
  lo_cliente ?= zcl_clientes=>zif_parceiros~get_instance( ).

  TRY.

      lo_cliente->zif_parceiros~set_parceiro( p_0158-id_local_destino ).

      CALL METHOD lo_cliente->zif_parceiros~ck_parceiro_local_negocio
        IMPORTING
          e_j_1bbranch = DATA(lw_branch)
        RECEIVING
          r_parceiro   = DATA(lv_parc).

    CATCH zcx_parceiros .

  ENDTRY.

  <fs_item>-plant = lw_branch-branch.

  <fs_item>-stge_loc = p_0158-lgort.
  <fs_item>-quantity = p_0158-quantidade.
  <fs_item>-po_unit = p_0158-unidade.
  <fs_item>-batch = p_0158-safra.
  <fs_item>-incoterms1 = p_0158-tp_frete.
  <fs_item>-incoterms2 = p_0158-tp_frete.
  <fs_item>-conf_ctrl = space.

  <fs_itemx>-po_item = <fs_item>-po_item.
  <fs_itemx>-po_itemx = abap_true.
* ---> S4 Migration - 04/07/2023 - FTM - Início
*  <fs_itemx>-material = abap_true.
  IF <fs_item>-material IS NOT INITIAL.
    <fs_itemx>-material      = abap_true.
  ELSEIF <fs_item>-material_long IS NOT INITIAL.
    <fs_itemx>-material_long = abap_true.
  ENDIF.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
  <fs_itemx>-plant =  abap_true.
  <fs_itemx>-stge_loc =  abap_true.
  <fs_itemx>-quantity =  abap_true.
  <fs_itemx>-po_unit =  abap_true.
  <fs_itemx>-batch =  abap_true.
  <fs_itemx>-incoterms1 =  abap_true.
  <fs_itemx>-incoterms2 =  abap_true.
  <fs_itemx>-conf_ctrl =  abap_true.


  CLEAR: lit_ekpo_ext_memory[].
  CASE p_0158-kvgr5.
    WHEN '002'.
      DATA(_emite_frete_ent) = abap_true.
    WHEN OTHERS.
      _emite_frete_ent       = abap_false.
  ENDCASE.

  APPEND VALUE #( zkvgr3      =  p_0158-tp_producao
                  zkvgr4      =  p_0158-kvgr4
                  ztrocanota  =  p_0158-ck_troca_nota
                  zckfreteent =  _emite_frete_ent ) TO lit_ekpo_ext_memory.

  EXPORT lit_ekpo_ext_memory TO MEMORY ID 'LIT_EKPO_EXTENSION'.

ENDFORM.
