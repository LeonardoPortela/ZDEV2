*----------------------------------------------------------------------*
***INCLUDE LZSD_ZSDT0132F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH_ALL
*&---------------------------------------------------------------------*
FORM f_refresh_all .

  CLEAR: gv_ucomm, gt_0005, gt_0006,
         gv_error_num, gv_cursor, gw_bapiret,
         gt_0006_global, gt_lfa1, go_005_alv, go_cc005_container.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_INFO
*&---------------------------------------------------------------------*
FORM f_seleciona_info  CHANGING p_info TYPE zsdt0002us.

  SELECT SINGLE * FROM zsdt0002us
    INTO p_info
      WHERE us_name = sy-uname.

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
      popup_title     = text-001
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

  p_info-us_name = sy-uname.

  MODIFY zsdt0002us FROM p_info.

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
*&      Form  F_POPUP_PRECO
*&---------------------------------------------------------------------*
FORM f_popup_preco CHANGING p_selec TYPE c.

  CLEAR p_selec.

  CALL FUNCTION 'K_KKB_POPUP_RADIO2'
    EXPORTING
      i_title   = 'Compra SIGAM:'
      i_text1   = 'Preço Fixo'
      i_text2   = 'Preço a Fixar'
      i_default = '1'
    IMPORTING
      i_result  = p_selec
    EXCEPTIONS
      cancel    = 1
      OTHERS    = 2.

  IF sy-subrc <> 0.
    CLEAR p_selec.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TIPO_FRETE
*&---------------------------------------------------------------------*
FORM f_popup_tipo_frete CHANGING p_selec TYPE c.

  CLEAR p_selec.

  CALL FUNCTION 'K_KKB_POPUP_RADIO2'
    EXPORTING
      i_title   = 'Tipo frete lote de compra:'
      i_text1   = 'FOB'
      i_text2   = 'CIF'
      i_default = '1'
    IMPORTING
      i_result  = p_selec
    EXCEPTIONS
      cancel    = 1
      OTHERS    = 2.

  IF sy-subrc <> 0.
    CLEAR p_selec.
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

      CLEAR gt_0005.

      CLEAR gt_0006_global.

      gt_0005 = zcl_integracao_sigam_lote=>zif_integracao_sigam_lote~enviar_sigam( lw_request ).

      PERFORM f_tratar_zfic.

      PERFORM f_append_005_test.

      "PERFORM f_create_alv_005.
      PERFORM f_refresh_grid USING 'X' space.

      IF gt_0005 IS NOT INITIAL.
        PERFORM f_put_mensagem USING 'S' 'Consulta realizada com sucesso'.
      ELSE.
        PERFORM f_put_mensagem USING 'W' 'Consulta realizada, mas sem dados de retorno'.
      ENDIF.

    CATCH zcx_integracao INTO DATA(lo_int).

      lv_mess = lo_int->get_longtext( ).

      PERFORM f_put_mensagem USING 'E' lv_mess.

      gv_error_num = 1.

    CATCH zcx_error INTO DATA(lo_int_e).

      lv_mess = lo_int_e->get_longtext( ).

      PERFORM f_put_mensagem USING 'E' lv_mess.

      gv_error_num = 1.

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

    gv_error_num = 1.

    EXIT.

  ENDIF.

  IF zsde0004-filial IS INITIAL.

    PERFORM f_put_mensagem USING 'E' 'O campo filial deve ser preenhcido'.

    gv_cursor = 'ZSDE0004-FILIAL'.

    p_erro = 'X'.

    gv_error_num = 1.

    EXIT.

  ENDIF.

  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
    ID 'WERKS' FIELD  zsde0004-filial
    ID 'ACTVT' FIELD '03'.

  IF sy-subrc NE 0 AND sy-uname NE 'RBLIMA'. " #DEBUG

    PERFORM f_put_mensagem USING 'E' 'Sem autorização para a filia selecionada'.
    p_erro = 'X'.

    gv_error_num = 2.

    EXIT.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCAR_ORDENS
*&---------------------------------------------------------------------*
FORM f_buscar_ordens .

  DATA lt_187 TYPE TABLE OF zsdt0187.
  DATA lt_066 TYPE TABLE OF zsdt0066.
  DATA lt_vbak TYPE TABLE OF vbak.
  DATA lt_vbfa TYPE TABLE OF vbfa.

  CHECK gt_0005 IS NOT INITIAL.
  CHECK gt_0006_global IS INITIAL.

  SELECT * FROM zsdt0187
    INTO TABLE lt_187
      FOR ALL ENTRIES IN gt_0005
        WHERE nu_compra = gt_0005-nu_compra
          AND nu_lote = gt_0005-lote.

  CHECK sy-subrc EQ 0.

  SELECT * FROM zsdt0066
    INTO TABLE lt_066
      FOR ALL ENTRIES IN lt_187
        WHERE nro_sol_ov  = lt_187-nro_sol_ov
          "AND ponto_c = lt_187-id_forn
          AND vbeln <> space.

  CHECK sy-subrc EQ 0.

  SELECT * FROM vbak
    INTO TABLE lt_vbak
    FOR ALL ENTRIES IN lt_066
      WHERE vbeln = lt_066-vbeln.

  CHECK sy-subrc EQ 0.

  SELECT * FROM vbfa
    INTO TABLE lt_vbfa
      FOR ALL ENTRIES IN lt_066
        WHERE vbeln = lt_066-vbeln
          AND posnv = '000010'
          AND vbtyp_n = 'J'.

  LOOP AT lt_066 ASSIGNING FIELD-SYMBOL(<fs_066>).

    READ TABLE lt_187 ASSIGNING FIELD-SYMBOL(<fs_187>)
      WITH KEY nro_sol_ov = <fs_066>-nro_sol_ov.

    CHECK sy-subrc EQ 0.

    READ TABLE lt_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>)
      WITH KEY vbeln = <fs_066>-vbeln.

    CHECK sy-subrc EQ 0.

    READ TABLE gt_0005 ASSIGNING FIELD-SYMBOL(<fs_0005>)
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

  ENDLOOP.

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
  "BREAK-POINT.
  IF go_005_alv IS INITIAL.
    CLEAR gv_error_num.
    LEAVE TO SCREEN 0.
    EXIT.
  ENDIF.

  CALL METHOD go_005_alv->get_selected_rows
    IMPORTING
      et_index_rows = DATA(lt_index_rows)
      et_row_no     = DATA(et_row_no).


  LOOP AT lt_index_rows ASSIGNING FIELD-SYMBOL(<fs_row>).

    READ TABLE gt_0005 ASSIGNING FIELD-SYMBOL(<fs_0005>) INDEX <fs_row>-index.

    IF sy-subrc NE 0.
      PERFORM f_put_mensagem USING 'E' 'Selecionar um lote'.
    ELSE.
      <fs_0005>-selec = 'X'.
      CLEAR gv_error_num.
      LEAVE TO SCREEN 0.
    ENDIF.

  ENDLOOP.

*  READ TABLE gt_0005 TRANSPORTING NO FIELDS
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
*&      Form  F_PROCESSAR
*&---------------------------------------------------------------------*
FORM f_processar .

  DATA lw_line TYPE zsde0005.
  DATA lv_count TYPE i.

  PERFORM f_preenche_textos.

  CHECK gt_0005 IS NOT INITIAL.

  LOOP AT gt_0005 ASSIGNING FIELD-SYMBOL(<fs_0005>) WHERE selec = 'X'.
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
*&      Form  F_DISPLAY_ALV_9300
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

*    CREATE OBJECT go_005_alv
*      EXPORTING
*        i_parent = go_cc005_container.
*
*    lw_layout-sel_mode = 'B'.
*    "lw_layout-no_headers = 'X'.
*    lw_layout-no_toolbar = 'X'.
*    "lw_layout-col_opt = 'X'.
*    lw_layout-cwidth_opt = 'X'.
*    lw_layout-box_fname = 'SELEC'.
*    lw_layout-zebra = 'X'.
*    "lw_layout-
*
*    "PERFORM f_append_005_test.
*
*    PERFORM f_get_fieldcat USING 'ZSDE0005' CHANGING lt_fieldcat.
*
*    DELETE lt_fieldcat WHERE fieldname = 'SELEC'.
*    DELETE lt_fieldcat WHERE fieldname = 'BUKRS'.
*    DELETE lt_fieldcat WHERE fieldname = 'NU_COMPRA'.
*    DELETE lt_fieldcat WHERE fieldname = 'ID_COMPRA'.
*
*    DELETE lt_fieldcat WHERE fieldname = 'MAKTX'.
*    DELETE lt_fieldcat WHERE fieldname = 'TIPOOPERACAO'.
*    DELETE lt_fieldcat WHERE fieldname = 'TRANSGENIA'.
*    DELETE lt_fieldcat WHERE fieldname = 'ANO'.
*    DELETE lt_fieldcat WHERE fieldname = 'MES'.
*    DELETE lt_fieldcat WHERE fieldname = 'PERIODO'.
*    DELETE lt_fieldcat WHERE fieldname = 'COMPRA'.
*    DELETE lt_fieldcat WHERE fieldname = 'PRODUTODERIVADO'.
*
*    CALL METHOD go_005_alv->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*    CREATE OBJECT lo_handle.
*
*    SET HANDLER lo_handle->handle_double_click FOR go_005_alv.
*
*    " Configuration for first display.
*    CALL METHOD go_005_alv->set_table_for_first_display
*      EXPORTING
*        is_layout       = lw_layout
*      CHANGING
*        it_outtab       = gt_0005
*        it_fieldcatalog = lt_fieldcat.
*
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_ALV_9300
*&---------------------------------------------------------------------*
FORM f_display_alv_gt_006 .

  DATA lw_layout TYPE lvc_s_layo.
  DATA lt_fieldcat TYPE lvc_t_fcat.

  DATA lo_events TYPE REF TO cl_salv_events_table.
  DATA lo_handle TYPE REF TO lcl_event_handler.

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

  "PERFORM f_create_alv_006.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_APPEND_005_TEST
*&---------------------------------------------------------------------*
FORM f_append_005_test .

  CHECK sy-sysid = 'DEV'.

  CLEAR gt_0005.

  DO 15 TIMES.

    APPEND INITIAL LINE TO gt_0005 ASSIGNING FIELD-SYMBOL(<fs_teste>).

    <fs_teste>-bukrs = '0001'.
    <fs_teste>-nu_compra = sy-uzeit.
    <fs_teste>-id_compra = '32281796301'.
    <fs_teste>-filial = '0111'.
    <fs_teste>-tp_frete = 'FOB'.
    <fs_teste>-compra = '21-01-011-00252-1'.
    <fs_teste>-qtde = '3000000.000'.
    <fs_teste>-saldo = '3000000.000'.
    <fs_teste>-matnr = '000000000000119892'.
    <fs_teste>-maktx = 'SOJA EM GRAOS ADQ TERCEIROS'.
    <fs_teste>-un_lote = 'KG'.
    <fs_teste>-nr_safra = '2021'.
    <fs_teste>-tipooperacao = 'LOTE DE COMPRA'.
    <fs_teste>-transgenia = 'RR'.
    <fs_teste>-ano = '2021'.
    <fs_teste>-mes = '6'.
    <fs_teste>-periodo = '0'.
    <fs_teste>-lote =  sy-uzeit.
    <fs_teste>-produtor = '0000206115'.
    <fs_teste>-desc_produtor = 'PEDRO JACYR BONGIOLO'.
    <fs_teste>-ponto_coleta = '0000206115'.
    <fs_teste>-desc_coleta = 'PEDRO JACYR BONGIOLO'.
    <fs_teste>-local_entrega = '0000001003'.
    <fs_teste>-desc_local = 'HERMASA NAVEGACAO DA AMAZONIA LTDA'.
    <fs_teste>-porto = '0000120372, 0000120185, 0000211700'.
    <fs_teste>-desc_porto = ' '.

    IF sy-uzeit+4(2) > 10.
      EXIT.
    ENDIF.

  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_APPEND_005_TEST
*&---------------------------------------------------------------------*
FORM f_append_006_test .

  CHECK sy-sysid = 'DEV'.

  DO 15 TIMES.

    APPEND INITIAL LINE TO gt_0006 ASSIGNING FIELD-SYMBOL(<fs_teste>).

    <fs_teste>-ov_pedido = '01010101'.
    <fs_teste>-filial = '0111'.
    <fs_teste>-auart = 'ZICC'.
    <fs_teste>-id_compra = '01010101'.
    <fs_teste>-nu_compra = '01010101'.
    <fs_teste>-lote = '01010101'.
    <fs_teste>-zmeng = '10'.
    <fs_teste>-zieme = 'UN'.
    <fs_teste>-rfmng = '10'.
    <fs_teste>-matnr = '01010101'.
    <fs_teste>-produtor = '01010101'.
    <fs_teste>-desc_produtor = 'XXXXXXXXXXX'.
    <fs_teste>-ponto_c = '01010101'.
    <fs_teste>-desc_ponto_c = 'XXXXXXXXXXX'.
    <fs_teste>-entrega = '01010101'.
    <fs_teste>-desc_entrega = 'XXXXXXXXXXX'.
    <fs_teste>-terminal = '01010101'.
    <fs_teste>-desc_terminal = 'XXXXXXXXXXX'.
  ENDDO.

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

*  LOOP AT gt_0005 ASSIGNING FIELD-SYMBOL(<fs_005>).
*    <fs_005>-selec = space.
*  ENDLOOP.

  READ TABLE gt_0005 ASSIGNING FIELD-SYMBOL(<fs_005>)
    INDEX e_row-index.

  CHECK sy-subrc EQ 0.

  "<fs_005>-selec = 'X'.

  CLEAR gt_0006.

  LOOP AT gt_0006_global ASSIGNING FIELD-SYMBOL(<fs_0006>)
    WHERE nu_compra = <fs_005>-nu_compra
      AND lote = <fs_005>-lote.

    APPEND <fs_0006> TO gt_0006.

  ENDLOOP.

  PERFORM f_append_006_test.

  PERFORM f_create_alv_006.

  PERFORM f_refresh_grid USING space 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ON_CLICK
*&---------------------------------------------------------------------*
FORM f_on_data_changed USING er_data_change TYPE REF TO cl_alv_changed_data_protocol.

  "BREAK-POINT.

*  LOOP AT er_data_change->mt_mod_cells ASSIGNING FIELD-SYMBOL(<fs_mod_cells>).
*
*    READ TABLE gt_0005 ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX <fs_mod_cells>-row_id.
*
*    IF sy-subrc = 0.
*
*      IF <fs_mod_cells>-fieldname = 'SELEC'.
*        <fs_saida>-selec = <fs_mod_cells>-value.
*      ENDIF.
*
*    ENDIF.
*
*
*  ENDLOOP.



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
          is_stable = lw_stable
        EXCEPTIONS
          finished  = 1
          OTHERS    = 2.

    ENDIF.

  ENDIF.

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

  CHECK gt_0005 IS NOT INITIAL.

  CREATE OBJECT go_005_alv
    EXPORTING
      i_parent = go_cc005_container.

  lw_layout-sel_mode = 'A'.
  "lw_layout-no_headers = 'X'.
  "lw_layout-no_toolbar = 'X'. XXXXX
  "lw_layout-col_opt = 'X'.
  lw_layout-cwidth_opt = 'X'.

  lw_layout-box_fname = 'SELEC'.
  lw_layout-zebra = 'X'.
  "lw_layout-

  lw_settings-edt_cll_cb = 'X'.

  PERFORM f_append_005_test.

  PERFORM f_get_fieldcat USING 'ZSDE0005' CHANGING lt_fieldcat.

  DELETE lt_fieldcat WHERE fieldname = 'SELEC'.

*  READ TABLE lt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_field>)
*    WITH KEY fieldname = 'SELEC'.
*
*  IF sy-subrc EQ 0.
*    PERFORM f_coluna_descr USING 'SELEC' 'Selec' CHANGING lt_fieldcat.
*    <fs_field>-edit = 'X'.
*    <fs_field>-checkbox = 'X'.
*    <fs_field>-just = 'C'.
*    <fs_field>-dd_outlen = 000010.
*  ENDIF.

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

*  CALL METHOD go_005_alv->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CREATE OBJECT lo_handle.

  SET HANDLER lo_handle->handle_double_click FOR go_005_alv.
  SET HANDLER lo_handle->handle_data_changed FOR go_005_alv.

  " Configuration for first display.
  CALL METHOD go_005_alv->set_table_for_first_display
    EXPORTING
      is_layout       = lw_layout
      i_save          = 'A'
    CHANGING
      it_outtab       = gt_0005
      it_fieldcatalog = lt_fieldcat.

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

  DELETE lt_fieldcat WHERE fieldname = 'LOTE'.
  DELETE lt_fieldcat WHERE fieldname = 'NU_COMPRA'.
  DELETE lt_fieldcat WHERE fieldname = 'ZIEME'.

*  CALL METHOD go_006_alv->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*  CREATE OBJECT lo_handle.
*
*  SET HANDLER lo_handle->handle_double_click FOR go_006_alv.

  " Configuration for first display.
  CALL METHOD go_006_alv->set_table_for_first_display
    EXPORTING
      is_layout       = lw_layout
    CHANGING
      it_outtab       = gt_0006
      it_fieldcatalog = lt_fieldcat.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TRATAR_ZFIC
*&---------------------------------------------------------------------*
FORM f_tratar_zfic .

  CHECK gt_0005 IS NOT INITIAL.

  DATA(lt_005_temp) = gt_0005.

  SELECT bukrs,branch FROM j_1bbranch
    INTO TABLE @DATA(lt_branch_lr)
      FOR ALL ENTRIES IN @gt_0005
        WHERE branch = @gt_0005-zfic_branch.

  CHECK sy-subrc EQ 0.

  SELECT bukrs,branch FROM j_1bbranch
    INTO TABLE @DATA(lt_branch_x)
      FOR ALL ENTRIES IN @gt_0005
        WHERE branch = @gt_0005-filial.

  CHECK sy-subrc EQ 0.

  LOOP AT lt_005_temp ASSIGNING FIELD-SYMBOL(<fs_005>).

    READ TABLE lt_branch_lr ASSIGNING FIELD-SYMBOL(<fs_lr>)
      WITH KEY branch = <fs_005>-zfic_branch.

    CHECK sy-subrc EQ 0.

    READ TABLE lt_branch_x ASSIGNING FIELD-SYMBOL(<fs_x>)
      WITH KEY branch = <fs_005>-filial.

    CHECK sy-subrc EQ 0.

    CHECK <fs_lr>-bukrs = <fs_x>-bukrs.

    DELETE gt_0005 WHERE bukrs = <fs_005>-bukrs
                     AND nu_compra = <fs_005>-nu_compra
                     AND id_compra = <fs_005>-id_compra
                     AND filial =  <fs_005>-filial.
  ENDLOOP.

ENDFORM.
