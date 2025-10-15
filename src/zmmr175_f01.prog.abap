*&---------------------------------------------------------------------*
*&  Include           ZMMR175_F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  FORM F_SELECIONA_DADOS_BSAK
*----------------------------------------------------------------------*
FORM f_seleciona_dados_bsak.

  SELECT a~bukrs a~gjahr a~belnr a~augbl a~dmbtr a~xblnr a~ebeln a~ebelp a~augdt b~bsart
    FROM bsak AS a
    INNER JOIN ekko AS b
      ON b~ebeln = a~ebeln
    INTO TABLE git_bsak
    WHERE a~bukrs IN s_bukrs
      AND a~augdt IN s_augdt
      AND a~gjahr IN s_gjahr
      AND a~ebeln IS NOT NULL
      AND a~blart IN s_blart
      AND b~bsart IN gra_bsart.

*  IF sy-subrc IS INITIAL.
*    PERFORM f_grava_tabela_z_fatura.
*  ENDIF.

ENDFORM.
FORM f_monta_range_tab_z_bsak.

  LOOP AT git_bsak INTO DATA(lwa_bsak).
    gwa_id_integr = 'IEQ'.
    gwa_id_integr-low = 'F' && lwa_bsak-bukrs &&
                               lwa_bsak-gjahr &&
                               lwa_bsak-belnr.

    APPEND gwa_id_integr TO gra_id_integr.
  ENDLOOP.

ENDFORM.
FORM f_monta_range_tab_z_ekbe.

  LOOP AT git_ekbe INTO DATA(lwa_ekbe).
    gwa_id_integr = 'IEQ'.
    gwa_id_integr-low = 'A' && lwa_ekbe-bukrs &&
                               lwa_ekbe-gjahr &&
                               lwa_ekbe-belnr.

    APPEND gwa_id_integr TO gra_id_integr.
  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  FORM F_GRAVA_TABELA_Z_FATURA
*----------------------------------------------------------------------*
FORM f_grava_tabela_z_fatura.

  DATA: lwa_zintegrcoupa01 TYPE zintegrcoupa01.

  DATA: lit_zintegrcoupa01 TYPE TABLE OF zintegrcoupa01.

  GET TIME.

  LOOP AT git_bsak INTO DATA(lwa_bsak).
    lwa_zintegrcoupa01-id_integr  = 'F' && lwa_bsak-bukrs &&
                                           lwa_bsak-gjahr &&
                                           lwa_bsak-belnr.
    lwa_zintegrcoupa01-ident_proc = 'PG'.
    lwa_zintegrcoupa01-dt_atual   = sy-datum.
    lwa_zintegrcoupa01-hr_atual   = sy-uzeit.
    APPEND lwa_zintegrcoupa01 TO lit_zintegrcoupa01.
  ENDLOOP.

  MODIFY zintegrcoupa01 FROM TABLE lit_zintegrcoupa01.
  IF sy-subrc IS INITIAL.
    PERFORM f_commit_work.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  FORM F_COMMIT_WORK
*----------------------------------------------------------------------*
FORM f_commit_work.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  CALL FUNCTION 'DB_COMMIT'.

ENDFORM.
*----------------------------------------------------------------------*
*  FORM F_SELECIONA_DADOS_Z
*----------------------------------------------------------------------*
FORM f_seleciona_dados_z.

  SELECT *
    FROM zintegrcoupa01
    APPENDING TABLE git_zintegrcoupa01
    WHERE ident_proc = 'PG'
      AND id_integr IN gra_id_integr.

ENDFORM.
*----------------------------------------------------------------------*
*  FORM F_SELECIONA_DADOS_FAT_ADI
*----------------------------------------------------------------------*
FORM f_seleciona_dados_fat_adi.

  DATA: lra_bukrs_f TYPE RANGE OF bukrs,
        lra_gjahr_f TYPE RANGE OF gjahr,
        lra_belnr_f TYPE RANGE OF belnr_d,
        lra_bukrs_a TYPE RANGE OF bukrs,
        lra_gjahr_a TYPE RANGE OF gjahr,
        lra_belnr_a TYPE RANGE OF belnr_d.

  DATA: lwa_bukrs LIKE LINE OF lra_bukrs_a,
        lwa_gjahr LIKE LINE OF lra_gjahr_a,
        lwa_belnr LIKE LINE OF lra_belnr_a.

  REFRESH: git_bsak.

  lwa_bukrs = 'IEQ'.
  lwa_gjahr = 'IEQ'.
  lwa_belnr = 'IEQ'.

  LOOP AT git_zintegrcoupa01 INTO DATA(lwa_zintegrcoupa01).
    IF lwa_zintegrcoupa01-id_integr(1) = 'F'.
      lwa_bukrs-low = lwa_zintegrcoupa01-id_integr+1(4).
      APPEND lwa_bukrs TO lra_bukrs_f.

      lwa_gjahr-low = lwa_zintegrcoupa01-id_integr+5(4).
      APPEND lwa_gjahr TO lra_gjahr_f.

      lwa_belnr-low = lwa_zintegrcoupa01-id_integr+9(10).
      APPEND lwa_belnr TO lra_belnr_f.


    ELSEIF lwa_zintegrcoupa01-id_integr(1) = 'A'.
      lwa_bukrs-low = lwa_zintegrcoupa01-id_integr+1(4).
      APPEND lwa_bukrs TO lra_bukrs_a.

      lwa_gjahr-low = lwa_zintegrcoupa01-id_integr+5(4).
      APPEND lwa_gjahr TO lra_gjahr_a.

      lwa_belnr-low = lwa_zintegrcoupa01-id_integr+9(10).
      APPEND lwa_belnr TO lra_belnr_a.
    ENDIF.
  ENDLOOP.

  SORT: lra_bukrs_f,
        lra_gjahr_f,
        lra_belnr_f,
        lra_bukrs_a,
        lra_gjahr_a,
        lra_belnr_a.

  DELETE ADJACENT DUPLICATES FROM lra_bukrs_f COMPARING ALL FIELDS.
  DELETE ADJACENT DUPLICATES FROM lra_gjahr_f COMPARING ALL FIELDS.
  DELETE ADJACENT DUPLICATES FROM lra_belnr_f COMPARING ALL FIELDS.
  DELETE ADJACENT DUPLICATES FROM lra_bukrs_a COMPARING ALL FIELDS.
  DELETE ADJACENT DUPLICATES FROM lra_gjahr_a COMPARING ALL FIELDS.
  DELETE ADJACENT DUPLICATES FROM lra_belnr_a COMPARING ALL FIELDS.

  SELECT a~bukrs a~gjahr a~belnr a~augbl a~dmbtr a~xblnr a~ebeln a~ebelp a~augdt b~bsart
    FROM bsak AS a
    INNER JOIN ekko AS b
      ON b~ebeln = a~ebeln
    INTO TABLE git_bsak
    WHERE a~bukrs IN lra_bukrs_f
      AND a~gjahr IN lra_gjahr_f
      AND a~belnr IN lra_belnr_f
      AND b~bsart   IN gra_bsart.

  SELECT a~belnr a~gjahr a~budat a~dmbtr a~ebeln a~ebelp b~bsart b~bukrs
    FROM ekbe AS a
    INNER JOIN ekko AS b
      ON b~ebeln = a~ebeln
    INTO TABLE git_ekbe
    WHERE a~belnr IN lra_belnr_a
      AND a~gjahr IN lra_gjahr_a
      AND b~bsart IN gra_bsart.

ENDFORM.
*----------------------------------------------------------------------*
*  FORM F_SELECIONA_DADOS_EKBE
*----------------------------------------------------------------------*
FORM f_seleciona_dados_ekbe.

  SELECT a~belnr a~gjahr a~budat a~dmbtr a~ebeln a~ebelp b~bsart b~bukrs
    FROM ekbe AS a
    INNER JOIN ekko AS b
      ON b~ebeln = a~ebeln
    INTO TABLE git_ekbe
    WHERE a~vgabe IN s_vgabe
      AND a~budat IN s_budat
      AND a~gjahr IN s_gjahr2
      AND b~bukrs IN s_bukrs
      AND b~bsart IN gra_bsart..
*  IF sy-subrc IS INITIAL.
*    PERFORM f_grava_tabela_z_adiant.
*  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*  FORM F_SELECIONA_DADOS_Z_ADIANT
*----------------------------------------------------------------------*
FORM f_grava_tabela_z_adiant.

  DATA: lwa_zintegrcoupa01 TYPE zintegrcoupa01.

  DATA: lit_zintegrcoupa01 TYPE TABLE OF zintegrcoupa01.

  GET TIME.

  LOOP AT git_ekbe INTO DATA(lwa_ekbe).
    lwa_zintegrcoupa01-id_integr  = 'A' && lwa_ekbe-bukrs &&
                                           lwa_ekbe-gjahr &&
                                           lwa_ekbe-belnr.
    lwa_zintegrcoupa01-ident_proc = 'PG'.
    lwa_zintegrcoupa01-dt_atual   = sy-datum.
    lwa_zintegrcoupa01-hr_atual   = sy-uzeit.
    APPEND lwa_zintegrcoupa01 TO lit_zintegrcoupa01.
  ENDLOOP.

  MODIFY zintegrcoupa01 FROM TABLE lit_zintegrcoupa01.
  IF sy-subrc IS INITIAL.
    PERFORM f_commit_work.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  FORM F_VALIDA_PARAMETROS
*----------------------------------------------------------------------*
FORM f_valida_parametros.

  DATA: lva_data_anterior TYPE sy-datum,
        lva_data_atual    TYPE sy-datum.

  IF sy-batch IS NOT INITIAL.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lva_data_atual
        days      = 1
        months    = 0
        signum    = '-'
        years     = 0
      IMPORTING
        calc_date = lva_data_anterior.

    s_augdt = 'IEQ'.
    s_augdt-low = sy-datum.
    APPEND s_augdt.

    s_augdt-low = lva_data_anterior.
    APPEND s_augdt.

    s_budat = 'IEQ'.
    s_budat-low = sy-datum.
    APPEND s_budat.

    s_augdt-low = lva_data_anterior.
    APPEND s_augdt.

    s_gjahr = 'IEQ'.
    s_gjahr-low = sy-datum(4).
    APPEND s_gjahr.

    s_gjahr-low = lva_data_anterior(4).
    APPEND s_gjahr.

    s_gjahr2 = 'IEQ'.
    s_gjahr2-low = sy-datum(4).
    APPEND s_gjahr2.

    s_gjahr2-low = lva_data_anterior(4).
    APPEND s_gjahr2.
  ENDIF.

  IF s_bukrs[] IS INITIAL AND
     s_augdt[] IS INITIAL AND
     s_augdt[] IS INITIAL AND
     s_gjahr[] IS INITIAL AND
     s_blart[] IS INITIAL.
    MESSAGE 'Preencher 1 filtro da fatura' TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_vgabe[] IS INITIAL AND
     s_budat[] IS INITIAL AND
     s_gjahr2[] IS INITIAL.
    MESSAGE 'Preencher 1 filtro do adiantamento' TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.


ENDFORM.
*----------------------------------------------------------------------*
*  FORM F_SELECIONA_SET
*----------------------------------------------------------------------*
FORM f_seleciona_set.

  "SET tipo de pedidos
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      client        = sy-mandt
      setnr         = 'MAGGI_PEDIDOS_COUPA'
      table         = 'EKKO'
      class         = '0000'
      fieldname     = 'EBELN'
    TABLES
      set_values    = git_set
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  LOOP AT git_set INTO DATA(lwa_set).
    IF lwa_set-from = lwa_set-to.
      gwa_bsart = 'IEQ'.
      gwa_bsart-low = lwa_set-from.
      APPEND gwa_bsart TO gra_bsart.
    ELSE.
      gwa_bsart = 'IBT'.
      gwa_bsart-low = lwa_set-from.
      gwa_bsart-high = lwa_set-to.
      APPEND gwa_bsart TO gra_bsart.
    ENDIF.
  ENDLOOP.

  "SET de empresas
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      client        = sy-mandt
      setnr         = 'MAGGI_EMPRESAS_COUPA'
      table         = 'T001'
      class         = '0000'
      fieldname     = 'BUKRS'
    TABLES
      set_values    = git_set
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  LOOP AT git_set INTO lwa_set.
    IF lwa_set-from = lwa_set-to.
      s_bukrs       = 'IEQ'.
      s_bukrs-low   = lwa_set-from.
      APPEND s_bukrs.
    ELSE.
      s_bukrs      = 'IBT'.
      s_bukrs-low  = lwa_set-from.
      s_bukrs-high = lwa_set-to.
      APPEND s_bukrs.
    ENDIF.
  ENDLOOP.

  " VBAE
  s_vgabe = 'IEQ'.
  s_vgabe-low = 4.
  APPEND s_vgabe.

  " BLART
  s_blart = 'IEQ'.
  s_blart-low = 'RE'.
  APPEND s_blart.

ENDFORM.
*----------------------------------------------------------------------*
*  FORM F_ENVIAR_INFO_COUPA
*----------------------------------------------------------------------*
FORM f_enviar_info_coupa.

  DATA: lva_historico_faturamento TYPE string,
        lva_data                  TYPE char10,
        lva_dmbtr                 TYPE char20.

  REFRESH: git_zintegrcoupa01_aux.

  SORT: git_zintegrcoupa01 BY id_integr.

  " Envio das informações de fatura
  LOOP AT git_bsak INTO DATA(lwa_bsak).

    DATA(lva_id) = 'F' && lwa_bsak-bukrs &&
                          lwa_bsak-gjahr &&
                          lwa_bsak-belnr.

    READ TABLE git_zintegrcoupa01 WITH KEY id_integr = lva_id
                                           status    = 'S'
                                           TRANSPORTING NO FIELDS
                                           BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      CONTINUE.
    ENDIF.

    PERFORM f_recupera_id USING lwa_bsak-ebeln.

    IF gva_id IS INITIAL.
      CONCATENATE 'Pedido de compras' lwa_bsak-ebeln 'não encontrado no Coupa, será processado o próximo registro' INTO DATA(l_message) SEPARATED BY space.
      MESSAGE l_message TYPE 'I'.
      CONTINUE.
    ENDIF.

    CLEAR: lva_data.
    lva_data = lwa_bsak-augdt+6(2) && '.' &&
               lwa_bsak-augdt+4(2) && '.' &&
               lwa_bsak-augdt(4).

    lva_dmbtr = lwa_bsak-dmbtr.
    TRANSLATE lva_dmbtr USING ',..,'.
    CONDENSE lva_dmbtr NO-GAPS.

    CONCATENATE 'Doc.SAP' lwa_bsak-belnr '-'
                'FATURA' '-'
                'Nro.Nota' lwa_bsak-xblnr '-'
                'Dt.Pagamento' lva_data '-'
                'Valor' lva_dmbtr
                INTO lva_historico_faturamento
                SEPARATED BY space.

    PERFORM: f_envio_coupa USING 'GET' CHANGING lva_historico_faturamento,
             f_envio_coupa USING 'PUT' CHANGING lva_historico_faturamento,
             f_preenche_saida_bsak USING lwa_bsak lva_historico_faturamento,
             f_atualiza_dados_z USING lwa_bsak-bukrs lwa_bsak-gjahr lwa_bsak-belnr 'F'.

    CLEAR: lva_historico_faturamento.
  ENDLOOP.

  " Envio das informações de adiantamento
  LOOP AT git_ekbe INTO DATA(lwa_ekbe).

    lva_id = 'A' && lwa_ekbe-bukrs &&
                    lwa_ekbe-gjahr &&
                    lwa_ekbe-belnr.

    READ TABLE git_zintegrcoupa01 WITH KEY id_integr = lva_id
                                           status    = 'S'
                                           TRANSPORTING NO FIELDS
                                           BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      CONTINUE.
    ENDIF.

    PERFORM f_recupera_id USING lwa_ekbe-ebeln.

    IF gva_id IS INITIAL.
      CONCATENATE 'Pedido de compras' lwa_bsak-ebeln 'não encontrado no Coupa, será processado o próximo registro' INTO l_message SEPARATED BY space.
      MESSAGE l_message TYPE 'I'.
      CONTINUE.
    ENDIF.

    CLEAR: lva_data.
    lva_data = lwa_ekbe-budat+6(2) && '.' &&
               lwa_ekbe-budat+4(2) && '.' &&
               lwa_ekbe-budat(4).

    lva_dmbtr = lwa_ekbe-dmbtr.
    TRANSLATE lva_dmbtr USING ',..,'.
    CONDENSE lva_dmbtr NO-GAPS.

    CONCATENATE 'Doc.SAP' lwa_ekbe-belnr '-'
                'ADIANTAMENTO' '-'
                'Dt.Pagamento' lva_data '-'
                'Valor' lva_dmbtr
                INTO lva_historico_faturamento
                SEPARATED BY space.

    PERFORM: f_envio_coupa USING 'GET' CHANGING lva_historico_faturamento,
             f_envio_coupa USING 'PUT' CHANGING lva_historico_faturamento,
             f_preenche_saida_ekbe USING lwa_ekbe lva_historico_faturamento,
             f_atualiza_dados_z USING lwa_ekbe-bukrs lwa_ekbe-gjahr lwa_ekbe-belnr 'A'.

    CLEAR: lva_historico_faturamento.
  ENDLOOP.

  " Atualiza tabela Z
  IF git_zintegrcoupa01_aux[] IS NOT INITIAL.
    MODIFY zintegrcoupa01 FROM TABLE git_zintegrcoupa01_aux.
    IF sy-subrc IS INITIAL.
      PERFORM f_commit_work.
    ENDIF.
  ENDIF.


ENDFORM.
*----------------------------------------------------------------------*
*  FORM F_ATUALIZA_DADOS_Z
*----------------------------------------------------------------------*
FORM f_atualiza_dados_z USING iva_bukrs iva_gjahr iva_belnr iva_tipo.

  DATA: lwa_zintegrcoupa01 TYPE zintegrcoupa01.

  GET TIME.

  lwa_zintegrcoupa01-id_integr  = iva_tipo && iva_bukrs && iva_gjahr && iva_belnr..
  lwa_zintegrcoupa01-ident_proc = 'PG'.
  lwa_zintegrcoupa01-dt_atual   = sy-datum.
  lwa_zintegrcoupa01-hr_atual   = sy-uzeit.
  lwa_zintegrcoupa01-status     = 'S'.
  APPEND lwa_zintegrcoupa01 TO git_zintegrcoupa01_aux.

ENDFORM.
*----------------------------------------------------------------------*
*  FORM F_PREENCHE_SAIDA_BSAK
*----------------------------------------------------------------------*
FORM f_preenche_saida_bsak USING iwa_bsak TYPE ty_bsak
                                 iva_historico.

  DATA: lwa_saida TYPE ty_saida.

  lwa_saida-tipo      = 'FATURAMENTO'.
  lwa_saida-bukrs     = iwa_bsak-bukrs.
  lwa_saida-gjahr     = iwa_bsak-gjahr.
  lwa_saida-belnr     = iwa_bsak-belnr.
  lwa_saida-augbl     = iwa_bsak-augbl.
  lwa_saida-dmbtr     = iwa_bsak-dmbtr.
  lwa_saida-xblnr     = iwa_bsak-xblnr.
  lwa_saida-ebeln     = iwa_bsak-ebeln.
  lwa_saida-ebelp     = iwa_bsak-ebelp.
  lwa_saida-augdt     = iwa_bsak-augdt.
  lwa_saida-bsart     = iwa_bsak-bsart.
  lwa_saida-historico = iva_historico.
  lwa_saida-id        = gva_id.
  APPEND lwa_saida TO git_saida.

ENDFORM.
*----------------------------------------------------------------------*
*  FORM F_PREENCHE_SAIDA_EKBE
*----------------------------------------------------------------------*
FORM f_preenche_saida_ekbe USING iwa_ekbe TYPE ty_ekbe
                                 iva_historico.

  DATA: lwa_saida TYPE ty_saida.

  lwa_saida-tipo      = 'ADIANTAMENTO'.
  lwa_saida-belnr     = iwa_ekbe-belnr.
  lwa_saida-gjahr     = iwa_ekbe-gjahr.
  lwa_saida-augbl     = iwa_ekbe-budat.
  lwa_saida-dmbtr     = iwa_ekbe-dmbtr.
  lwa_saida-ebeln     = iwa_ekbe-ebeln.
  lwa_saida-ebelp     = iwa_ekbe-ebelp.
  lwa_saida-bsart     = iwa_ekbe-bsart.
  lwa_saida-bukrs     = iwa_ekbe-bukrs.
  lwa_saida-historico = iva_historico.
  lwa_saida-id        = gva_id.
  APPEND lwa_saida TO git_saida.

ENDFORM.
*----------------------------------------------------------------------*
*  FORM F_EXIBE_ALV
*----------------------------------------------------------------------*
FORM f_exibe_alv.

  DATA: lo_columns TYPE REF TO cl_salv_columns_table,
        lo_column  TYPE REF TO cl_salv_column.

  DATA: lwa_key     TYPE salv_s_layout_key.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table   = go_table
        CHANGING
          t_table        = git_saida ).
    CATCH cx_salv_msg.
  ENDTRY.

  "Layout
  DATA(lo_functions) = go_table->get_functions( ).
  lo_functions->set_all( abap_true ).
  lo_functions->set_export_send( abap_false ).

  "Botão salvar
  lo_functions->set_layout_maintain( abap_true ).
  lo_functions->set_layout_change( abap_true ).
  lo_functions->set_layout_load( abap_true ).
  lo_functions->set_layout_save( abap_true ).

  "Seleciona e Salva layout criado pelo usúario
  DATA(lo_layout) = go_table->get_layout( ).
  lwa_key-report = sy-repid.
  lo_layout->set_key( value = lwa_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

  "Seleciona mais de uma linha (BOX)
  DATA(lo_select) = go_table->get_selections( ).
  lo_select->set_selection_mode( 3 ).

  "Zebra
  DATA(lo_display) = go_table->get_display_settings( ).
  lo_display->set_striped_pattern( abap_true ).

  "Ajusta as Colunas
  lo_columns = go_table->get_columns( ).
  lo_columns->set_optimize( 'X' ).

  lo_column = lo_columns->get_column( 'TIPO' ).
  lo_column->set_short_text( 'Processo' ).

  lo_column = lo_columns->get_column( 'ID' ).
  lo_column->set_short_text( 'ID Coupsa' ).

  lo_column = lo_columns->get_column( 'HISTORICO' ).
  lo_column->set_short_text( 'Hist.Fat' ).
  lo_column->set_long_text( 'Histórico de faturamento' ).

  "Apresenta o ALV
  IF git_saida IS NOT INITIAL.
    go_table->display( ).
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*  FORM F_RECUPERA_ID
*----------------------------------------------------------------------*
FORM f_recupera_id USING iva_ebeln TYPE ebeln.

  CONSTANTS gc_service TYPE /ui2/service_name VALUE 'COUPA_INT_HIST_PED_COMPRA'.

  FREE: go_int_ped.

  CLEAR: gva_id.

  CREATE OBJECT go_int_ped
    EXPORTING
      i_servico = gc_service.

  " Buscas os dados no Coupa
  FREE: git_filter.
  REFRESH: git_xml_header.

  "Adiciona informações da fatura
  APPEND INITIAL LINE TO git_filter REFERENCE INTO DATA(lo_wa_filter).
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = iva_ebeln
    IMPORTING
      output = lo_wa_filter->value.

  lo_wa_filter->field = 'po-number'.

  " Integração
  go_int_ped->zif_integracao_coupa_ped_comp~set_ds_url( e_metodo   = 'GET'
                                                        it_filter  = git_filter ).

  go_int_ped->zif_integracao_coupa_ped_comp~set_send_msg(
                  IMPORTING e_id_integracao = DATA(e_id_integracao_post)
                            e_integracao    = DATA(e_integracao_post) ).

  go_int_ped->realiza_quebra_xml( ).

  "Recupera informação
  git_xml_header = go_int_ped->get_xml_header( ).

  "Id do pedido
  READ TABLE git_xml_header INTO gwa_xml_header
                              WITH KEY po_number = iva_ebeln
                                       BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    gva_id = gwa_xml_header-id.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  FORM F_ENVIO_COUPA
*----------------------------------------------------------------------*
FORM f_envio_coupa USING i_metodo CHANGING cva_historico_faturamento TYPE string.

  DATA: lva_metodo   TYPE string,
        lva_http_url TYPE string.

  lva_http_url = '/purchase_orders/' && gva_id && '?fields=["id",{"custom_fields": {}}]'.
  lva_metodo   = i_metodo.

  DATA(lo_object) = NEW zcl_integracao_hist_fat_coupa( iwa_historico_faturamento = cva_historico_faturamento ).

  IF i_metodo = 'PUT'.
    lo_object->zif_integracao_hist_fat_coupa~get_xml( IMPORTING e_xml = DATA(lva_xml) ).
  ENDIF.

  lo_object->zif_integracao_hist_fat_coupa~set_ds_url( e_http_url = lva_http_url e_metodo = lva_metodo
    )->set_ds_data( i_xml = lva_xml
    )->set_send_msg( IMPORTING e_id_integracao = DATA(id_integracao)
                               e_integracao    = DATA(e_integracao) ).

  lo_object->zif_integracao_hist_fat_coupa~get_retorno( IMPORTING e_historico = DATA(lva_historico)
                                                                  e_retorno   = DATA(e_retorno) ).

  IF i_metodo = 'GET'.
    IF lva_historico IS NOT INITIAL.
      CONCATENATE lva_historico '|' cva_historico_faturamento
      INTO cva_historico_faturamento SEPARATED BY space.
    ENDIF.

  ENDIF.

  FREE: lo_object.

ENDFORM.
