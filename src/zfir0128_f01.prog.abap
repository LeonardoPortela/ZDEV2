*&---------------------------------------------------------------------*
*& Include          ZFIR0128_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_seleciona_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_seleciona_dados .
  zcl_busca_dados_averbacao_nf=>busca_dados(
  EXPORTING
    i_bukrs = s_bukrs[]
    i_werks = s_werks[]
    i_cfop  = s_cfop[]
    i_matnr = s_matnr[]
    i_chave_nfe = s_chave[]
    i_data_doc  = s_docdat[]
    i_docnum    = s_docnum[]
    i_kunnr     = s_kunnr[]
    i_dt_evento = s_dteve[]
   IMPORTING
     e_retorno_Dados = t_dados
     e_msg_erro      = DATA(lv_msg_erro) ).

  IF lv_msg_erro IS NOT INITIAL.
    MESSAGE lv_msg_erro TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_exibe_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_exibe_alv .

  DATA:lo_aggrs       TYPE REF TO cl_salv_aggregations,
       lo_sort        TYPE REF TO cl_salv_sorts,
       lo_sort_column TYPE REF TO cl_salv_sort.

  DATA: gr_layout TYPE REF TO cl_salv_layout.
  DATA: key TYPE salv_s_layout_key.

  IF t_dados IS INITIAL.
    RETURN.
  ENDIF.

  CALL METHOD cl_salv_table=>factory
    EXPORTING
      list_display = if_salv_c_bool_sap=>false
    IMPORTING
      r_salv_table = gr_alv
    CHANGING
      t_table      = t_dados.

  gr_alv->get_functions( )->set_all( ).

  DATA: lr_columns TYPE REF TO cl_salv_columns_table,
        lr_column  TYPE REF TO cl_salv_column_table.

  lr_columns = gr_alv->get_columns( ).

  TRY.
      lr_column ?= lr_columns->get_column( 'FILIAL' ).
      lr_column->set_short_text( 'Filial' ).
      lr_column->set_medium_text( 'Filial' ).
      lr_column->set_long_text( 'Filial' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'DOC_NUM' ).
      lr_column->set_short_text( 'Nr. Doc' ).
      lr_column->set_medium_text( 'Nr. Doc' ).
      lr_column->set_long_text( 'Nr. Doc' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'DOCDAT' ).
      lr_column->set_short_text( 'Dt Doc' ).
      lr_column->set_medium_text( 'Dt Doc' ).
      lr_column->set_long_text( 'Dt Doc' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'NR_NOTA' ).
      lr_column->set_short_text( 'Nr. Nota' ).
      lr_column->set_medium_text( 'Nr. Nota' ).
      lr_column->set_long_text( 'Nr. Nota' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'CHAVE_NFE' ).
      lr_column->set_short_text( 'Chave NFe' ).
      lr_column->set_medium_text( 'Chave NFe' ).
      lr_column->set_long_text( 'Chave NFe' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'CLIENTE' ).
      lr_column->set_short_text( 'Cliente' ).
      lr_column->set_medium_text( 'Cliente' ).
      lr_column->set_long_text( 'Cliente' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
*SMC
  TRY.
      lr_column ?= lr_columns->get_column( 'DESC_CLIENTE' ).
      lr_column->set_short_text( 'Des.C' ).
      lr_column->set_medium_text( 'Desc. Cl' ).
      lr_column->set_long_text( 'Descrição Cliente' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
*SMC

  TRY.
      lr_column ?= lr_columns->get_column( 'CPF_CNPJ' ).
      lr_column->set_short_text( 'CPF/CNPJ' ).
      lr_column->set_medium_text( 'CPF/CNPJ' ).
      lr_column->set_long_text( 'CPF/CNPJ' ).
      lr_column->set_leading_zero( abap_false ).
      lr_column->set_decimals( '' ).

    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lr_column ?= lr_columns->get_column( 'UF' ).
      lr_column->set_short_text( 'UF cli/for' ).
      lr_column->set_medium_text( 'UF cli/for' ).
      lr_column->set_long_text( 'UF cli/for' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'UNIDADE' ).
      lr_column->set_short_text( 'UM' ).
      lr_column->set_medium_text( 'UM' ).
      lr_column->set_long_text( 'UM' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'PRODUTO' ).
      lr_column->set_short_text( 'Produto' ).
      lr_column->set_medium_text( 'Produto' ).
      lr_column->set_long_text( 'Produto' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'DESCR_MATERIAL' ).
      lr_column->set_short_text( 'Descrição' ).
      lr_column->set_medium_text( 'Descrição' ).
      lr_column->set_long_text( 'Descrição' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'QUANTIDADE' ).
      lr_column->set_short_text( 'Quantidade' ).
      lr_column->set_medium_text( 'Quantidade' ).
      lr_column->set_long_text( 'Quantidade' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'VLR_UNIT' ).
      lr_column->set_short_text( 'Vlr. Unit' ).
      lr_column->set_medium_text( 'Vlr. Unit' ).
      lr_column->set_long_text( 'Vlr. Unit' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'VALOR_NF' ).
      lr_column->set_short_text( 'Vlr NF' ).
      lr_column->set_medium_text( 'Vlr NF' ).
      lr_column->set_long_text( 'Vlr NF' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'INCOTERMS' ).
      lr_column->set_short_text( 'Incoterms' ).
      lr_column->set_medium_text( 'Incoterms' ).
      lr_column->set_long_text( 'Incoterms' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'DUE' ).
      lr_column->set_short_text( 'Due' ).
      lr_column->set_medium_text( 'Due' ).
      lr_column->set_long_text( 'Due' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'QTD_AVERBADA' ).
      lr_column->set_short_text( 'Qtd Averb' ).
      lr_column->set_medium_text( 'Qtd Averb' ).
      lr_column->set_long_text( 'Qtd Averb' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'DATA_AVERBACAO' ).
      lr_column->set_short_text( 'Dt. Averb' ).
      lr_column->set_medium_text( 'Dt. Averb' ).
      lr_column->set_long_text( 'Dt. Averb' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.


  TRY.
      lr_column ?= lr_columns->get_column( 'DATA_EMBARQUE' ).
      lr_column->set_short_text( 'Dt. Emb' ).
      lr_column->set_medium_text( 'Dt. Emb' ).
      lr_column->set_long_text( 'Dt. Emb' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'QTD_NAO_AVERBADA' ).
      lr_column->set_short_text( 'QtdNAverb' ).
      lr_column->set_medium_text( 'Qtd. Não Averb' ).
      lr_column->set_long_text( 'Qtd. Não Averb' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

* Add totals.
  lo_aggrs = gr_alv->get_aggregations( ).

  TRY.
      CALL METHOD lo_aggrs->add_aggregation
        EXPORTING
          columnname  = 'QUANTIDADE'
          aggregation = if_salv_c_aggregation=>total.

      CALL METHOD lo_aggrs->add_aggregation
        EXPORTING
          columnname  = 'VLR_UNIT'
          aggregation = if_salv_c_aggregation=>total.

      CALL METHOD lo_aggrs->add_aggregation
        EXPORTING
          columnname  = 'VALOR_NF'
          aggregation = if_salv_c_aggregation=>total.

      CALL METHOD lo_aggrs->add_aggregation
        EXPORTING
          columnname  = 'QTD_NAO_AVERBADA'
          aggregation = if_salv_c_aggregation=>total.

    CATCH cx_salv_data_error cx_salv_not_found cx_salv_existing. "#EC NO_HANDLER
  ENDTRY.

* Add subtotals.
  CALL METHOD gr_alv->get_sorts
    RECEIVING
      value = lo_sort.

  TRY.
      CALL METHOD lo_sort->add_sort
        EXPORTING
          columnname = 'CHAVE_NFE'
        RECEIVING
          value      = lo_sort_column.

      CALL METHOD lo_sort_column->set_subtotal
        EXPORTING
          value = if_salv_c_bool_sap=>true.
    CATCH cx_salv_data_error cx_salv_not_found cx_salv_existing. "#EC NO_HANDLER
  ENDTRY.

  gr_layout = gr_alv->get_layout( ).
  key-report = sy-repid.
  gr_layout->set_key( key ).
  gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

  gr_alv->display( ).
ENDFORM.
