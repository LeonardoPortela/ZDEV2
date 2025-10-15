*&---------------------------------------------------------------------*
*&  Include           ZMMR173_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  FORM F_PREENCHE_CAMPOS_TELA
*&---------------------------------------------------------------------*
FORM f_preenche_campos_tela.

  GET TIME.
  p_nome = sy-datum+6(2) &&
           sy-datum+4(2) &&
           sy-datum(4) && '_' &&
           sy-uzeit(2) && ':' &&
           sy-uzeit(2) && '.csv'.

ENDFORM.
*&---------------------------------------------------------------------*
*&  FORM F_MODIFICA_TELA
*&---------------------------------------------------------------------*
FORM f_modifica_tela.

  IF p_local = abap_true.
    LOOP AT SCREEN.
      "Mostra os campos
      IF screen-group1 = 'T1'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ELSEIF screen-group1 = 'T2'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      "Esconde os campos
      IF screen-group1 = 'T1'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ELSEIF screen-group1 = 'T1'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&  FORM F_AJUDA_DE_PESQUISA
*&---------------------------------------------------------------------*
FORM f_ajuda_de_pesquisa CHANGING c_arquivo.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      mask          = ' '
    CHANGING
      file_name     = c_arquivo
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&  FORM F_VALIDA_CAMPOS_TELA
*&---------------------------------------------------------------------*
FORM f_valida_campos_tela.

  IF s_data-low IS INITIAL.
    MESSAGE 'Erro: Preencher campo data do lançamento.' TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_bukrs-low IS INITIAL.
    MESSAGE 'Erro: Preencher campo empresa.' TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_data-high IS NOT INITIAL.
    DATA(lv_data) = s_data-high - s_data-low.
    IF lv_data > 30.
      MESSAGE 'Erro: Intervalo de datas não pode ser maior que 30 dias.' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&  FORM F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM f_seleciona_dados.

  SELECT belnr budat bukrs lifnr waers kursf gjahr zterm
    FROM rbkp
    INTO TABLE git_rbkp
    WHERE budat IN s_data
      AND bukrs IN s_bukrs.

  IF sy-subrc IS INITIAL.

    SELECT lifnr name1
      FROM lfa1
      INTO TABLE git_lfa1
      FOR ALL ENTRIES IN git_rbkp
      WHERE lifnr = git_rbkp-lifnr.

    SELECT belnr gjahr buzei ebeln ebelp matnr werks wrbtr menge
      FROM rseg
      INTO TABLE git_rseg
      FOR ALL ENTRIES IN git_rbkp
      WHERE belnr = git_rbkp-belnr
        AND gjahr = git_rbkp-gjahr.

    IF sy-subrc IS INITIAL.

      SELECT matnr maktx
        FROM makt
        INTO TABLE git_makt
        FOR ALL ENTRIES IN git_rseg
        WHERE matnr = git_rseg-matnr.

      SELECT ebeln bsart kufix inco1
        FROM ekko
        INTO TABLE git_ekko
        FOR ALL ENTRIES IN git_rseg
        WHERE ebeln = git_rseg-ebeln.

      SELECT ebeln ebelp matnr knttp mwskz bednr
        FROM ekpo
        INTO TABLE git_ekpo
        FOR ALL ENTRIES IN git_rseg
        WHERE ebeln = git_rseg-ebeln
          AND ebelp = git_rseg-ebelp.

      IF sy-subrc IS INITIAL.

        SELECT ebeln ebelp sakto kostl anln1 anln2 aufnr
          FROM ekkn
          INTO TABLE git_ekkn
          FOR ALL ENTRIES IN git_ekpo
          WHERE ebeln = git_ekpo-ebeln
            AND ebelp = git_ekpo-ebelp.
      ENDIF.

    ENDIF.
  ELSE.
    MESSAGE 'Erro: Não foi encontado nenhum registro para os filtros utilizados.' TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT: git_lfa1 BY lifnr,
        git_rseg BY belnr gjahr,
        git_makt BY matnr,
        git_ekko BY ebeln,
        git_ekpo BY ebeln ebelp,
        git_ekkn BY ebeln ebelp.
ENDFORM.
*&---------------------------------------------------------------------*
*&  FORM F_MONTA_SAIDA
*&---------------------------------------------------------------------*
FORM f_monta_saida.

  PERFORM: f_monta_cabecalho_fixo.

  LOOP AT git_rbkp INTO gwa_rbkp.

    PERFORM: f_monta_cabecalho,
             f_monta_itens,
             f_monta_account,
             f_limpa_variaveis.

  ENDLOOP.

  PERFORM f_monta_saida_csv.

ENDFORM.
*&---------------------------------------------------------------------*
*&  FORM F_MONTA_CABECALHO_FIXO
*&---------------------------------------------------------------------*
FORM f_monta_cabecalho_fixo.

  DATA: lwa_saida  TYPE ty_saida.

  " Informações do cabeçalho
  lwa_saida-campo1  = 'Header'.
  lwa_saida-campo2  = 'Action'.
  lwa_saida-campo3  = 'PO Number'.
  lwa_saida-campo4  = 'Supplier Name'.
  lwa_saida-campo5  = 'Supplier Number'.
  lwa_saida-campo6  = 'Currency Code'.
  lwa_saida-campo7  = 'Chart of Account Name'.
  lwa_saida-campo8  = 'Payment Term Code'.
  lwa_saida-campo9  = 'Shipping Term Code'.
  lwa_saida-campo10 = 'Ship To Location Code'.
  lwa_saida-campo11 = 'Transmission Method Override'.
  lwa_saida-campo12 = 'status_da_integrao'.
  lwa_saida-campo13 = 'taxa_de_cambio'.
  lwa_saida-campo14 = 'fornecedor_parceiro'.
  lwa_saida-campo15 = 'tipo_de_pedido'.
  APPEND lwa_saida TO git_saida.


  CLEAR: lwa_saida.
  " Informações dos itens
  lwa_saida-campo1  = 'Line'.
  lwa_saida-campo2  = 'Action'.
  lwa_saida-campo3  = 'Contract Number'.
  lwa_saida-campo4  = 'Contract Name'.
  lwa_saida-campo5  = 'Line Number'.
  lwa_saida-campo6  = 'Catalog Item Name'.
  lwa_saida-campo7  = 'Non Catalog Item Description'.
  lwa_saida-campo8  = 'Quantity'.
  lwa_saida-campo9  = 'Price'.
  lwa_saida-campo10 = 'Need By Date'.
  lwa_saida-campo11 = 'UOM Code'.
  lwa_saida-campo12 = 'Commodity Name'.
  lwa_saida-campo13 = 'entrega_futura'.
  lwa_saida-campo14 = 'condio_de_pagamento' .
  lwa_saida-campo15 = 'taxa_fixa'.
  lwa_saida-campo16 = 'taxa_d_cmbio'.
  lwa_saida-campo17 = 'iva'.
  lwa_saida-campo18 = 'n_acompanhamento'.
  APPEND lwa_saida TO git_saida.

  CLEAR: lwa_saida.
  " Informações dos account
  lwa_saida-campo1  = 'Account Allocation'.
  lwa_saida-campo2  = 'Line Number'.
  lwa_saida-campo3  = 'Amount'.
  lwa_saida-campo4  = 'Percent'.
  lwa_saida-campo5  = 'Account Name'.
  lwa_saida-campo6  = 'Account Code'.
  lwa_saida-campo7  = 'Account Segment 1'.
  lwa_saida-campo8  = 'Account Segment 2'.
  lwa_saida-campo9  = 'Account Segment 3'.
  lwa_saida-campo10 = 'Account Segment 4'.
  lwa_saida-campo11 = 'Account Segment 5'.
  lwa_saida-campo12 = 'Account Segment 6'.
  lwa_saida-campo13 = 'Account Segment 7'.
  lwa_saida-campo14 = 'Budget Period Name'.
  APPEND lwa_saida TO git_saida.


ENDFORM.
*&---------------------------------------------------------------------*
*&  FORM F_CONVERTE_STRING_TO_XSTRING
*&---------------------------------------------------------------------*
FORM f_converte_string_to_xtring USING i_string
                                CHANGING c_xstring.

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = i_string
    IMPORTING
      buffer = c_xstring
    EXCEPTIONS
      failed = 1
      OTHERS = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&  FORM F_MONTA_CABECALHO
*&---------------------------------------------------------------------*
FORM f_monta_cabecalho.

  DATA: lwa_saida TYPE ty_saida.

  gwa_cabecalho-po_number         = gwa_rbkp-belnr.
  gwa_cabecalho-supplier_number   = gwa_rbkp-lifnr.
  gwa_cabecalho-currency_code     = gwa_rbkp-waers.
  gwa_cabecalho-payment_term_code = gwa_rbkp-zterm.

  IF gwa_rbkp-waers = 'BRL'.
    CLEAR: gwa_itens-taxa_de_cambio.
  ELSE.
    gwa_itens-taxa_de_cambio = gwa_rbkp-kursf.
  ENDIF.

  READ TABLE git_lfa1 INTO gwa_lfa1
                  WITH KEY lifnr = gwa_rbkp-lifnr
                           BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    gwa_cabecalho-supplier_name = gwa_lfa1-name1.
  ENDIF.

  READ TABLE git_rseg INTO gwa_rseg
                  WITH KEY belnr = gwa_rbkp-belnr
                           gjahr = gwa_rbkp-gjahr
                           BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    gwa_cabecalho-ship_location_code = gwa_rseg-werks.

    READ TABLE git_ekko INTO gwa_ekko
                    WITH KEY ebeln = gwa_rseg-ebeln
                    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      gwa_cabecalho-shipping_tem_code = gwa_ekko-inco1.
      gwa_cabecalho-tipo_pedido       = gwa_ekko-bsart.
    ENDIF.
  ENDIF.

  lwa_saida-campo1  = 'Header'.
  lwa_saida-campo2  = gwa_cabecalho-action.
  lwa_saida-campo3  = gwa_cabecalho-po_number.
  lwa_saida-campo4  = gwa_cabecalho-supplier_name.
  lwa_saida-campo5  = gwa_cabecalho-supplier_number.
  lwa_saida-campo6  = gwa_cabecalho-currency_code.
  lwa_saida-campo7  = gwa_cabecalho-chart_account_name.
  lwa_saida-campo8  = gwa_cabecalho-payment_term_code.
  lwa_saida-campo9  = gwa_cabecalho-shipping_tem_code.
  lwa_saida-campo10 = gwa_cabecalho-ship_location_code.
  lwa_saida-campo11 = gwa_cabecalho-transmission_override.
  lwa_saida-campo12 = gwa_cabecalho-status_integracao.
  lwa_saida-campo13 = gwa_cabecalho-taxa_cambio.
  lwa_saida-campo14 = gwa_cabecalho-fornecedor_parceiro.
  lwa_saida-campo15 = gwa_cabecalho-tipo_pedido.
  APPEND lwa_saida TO git_saida.

ENDFORM.
*&---------------------------------------------------------------------*
*&  FORM F_MONTA_ITENS
*&---------------------------------------------------------------------*
FORM f_monta_itens.

  DATA: lwa_saida TYPE ty_saida.

  READ TABLE git_rseg INTO gwa_rseg
                  WITH KEY belnr = gwa_rbkp-belnr
                           gjahr = gwa_rbkp-gjahr
                           BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    gwa_itens-line_number    = gwa_rseg-ebelp.
    gwa_itens-quantity       = gwa_rseg-menge.
    gwa_itens-price          = gwa_rseg-wrbtr.

    IF gwa_rbkp-waers = 'BRL'.
      CLEAR: gwa_itens-taxa_de_cambio.
    ELSE.
      gwa_itens-taxa_de_cambio = gwa_rbkp-kursf.
    ENDIF.

    READ TABLE git_makt INTO gwa_makt
                    WITH KEY matnr = gwa_rseg-matnr
                             BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      PERFORM f_input_outpu_matnr USING '' CHANGING gwa_makt-matnr.
      gwa_itens-non_catalog_item_description = gwa_makt-matnr && ' - ' &&
                                               gwa_makt-maktx.
    ENDIF.

    READ TABLE git_ekpo INTO gwa_ekpo
                    WITH KEY ebeln = gwa_rseg-ebeln
                             ebelp = gwa_rseg-ebelp
                             BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      gwa_itens-iva              = gwa_ekpo-mwskz.
      gwa_itens-n_acompanhamento = gwa_ekpo-bednr.
    ENDIF.

    READ TABLE git_ekko INTO gwa_ekko
                    WITH KEY ebeln = gwa_rseg-ebeln
                             BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      gwa_itens-taxa_fixa = gwa_ekko-kufix.
    ENDIF.
  ENDIF.

  lwa_saida-campo1  = 'Line'.
  lwa_saida-campo2  = gwa_itens-action.
  lwa_saida-campo3  = gwa_itens-contract_number.
  lwa_saida-campo4  = gwa_itens-contract_name.
  lwa_saida-campo5  = gwa_itens-line_number.
  lwa_saida-campo6  = gwa_itens-catalog_item_name.
  lwa_saida-campo7  = gwa_itens-non_catalog_item_description.
  lwa_saida-campo8  = gwa_itens-quantity.
  lwa_saida-campo9  = gwa_itens-price.
  lwa_saida-campo10 = gwa_itens-need_by_date.
  lwa_saida-campo11 = gwa_itens-uom_code.
  lwa_saida-campo12 = gwa_itens-commodity_name.
  lwa_saida-campo13 = gwa_itens-entrega_fatura.
  lwa_saida-campo14 = gwa_itens-condio_de_pagamento.
  lwa_saida-campo15 = gwa_itens-taxa_fixa.
  lwa_saida-campo16 = gwa_itens-taxa_de_cambio.
  lwa_saida-campo17 = gwa_itens-iva.
  lwa_saida-campo18 = gwa_itens-n_acompanhamento.
  APPEND lwa_saida TO git_saida.

ENDFORM.
FORM f_input_outpu_matnr USING iva_input CHANGING cva_matnr.

  IF iva_input IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = cva_matnr
      IMPORTING
        output       = cva_matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = cva_matnr
      IMPORTING
        output = cva_matnr.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&  FORM F_MONTA_ACCOUNT
*&---------------------------------------------------------------------*
FORM f_monta_account.

  DATA: lwa_saida TYPE ty_saida.

  READ TABLE git_rseg INTO gwa_rseg
                  WITH KEY belnr = gwa_rbkp-belnr
                           gjahr = gwa_rbkp-gjahr
                           BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    gwa_account-line_number      = gwa_rseg-ebelp.
    gwa_account-amount           = gwa_rseg-wrbtr.
    gwa_account-account_segment1 = gwa_rbkp-bukrs.
    gwa_account-account_segment2 = gwa_rseg-werks.

    READ TABLE git_ekpo INTO gwa_ekpo
                    WITH KEY ebeln = gwa_rseg-ebeln
                             ebelp = gwa_rseg-ebelp
                             BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      CASE gwa_ekpo-knttp.
        WHEN ''.
          gwa_account-account_segment3 = 'E'.
        WHEN 'K'.
          gwa_account-account_segment3 = 'K'.
        WHEN 'A'.
          gwa_account-account_segment3 = 'A'.
        WHEN 'F'.
          gwa_account-account_segment3 = 'FI'.
      ENDCASE.
    ENDIF.

    READ TABLE git_ekkn INTO gwa_ekkn
                    WITH KEY ebeln = gwa_rseg-ebeln
                             ebelp = gwa_rseg-ebelp
                             BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      gwa_account-account_segment4 = gwa_ekkn-kostl.
      gwa_account-account_segment5 = gwa_ekkn-aufnr.
      gwa_account-account_segment7 = gwa_ekkn-sakto.
    ENDIF.
  ENDIF.

  lwa_saida-campo1  = 'Account'.
  lwa_saida-campo2  = gwa_account-line_number.
  lwa_saida-campo3  = gwa_account-amount.
  lwa_saida-campo4  = gwa_account-percent.
  lwa_saida-campo5  = gwa_account-account_name.
  lwa_saida-campo6  = gwa_account-account_code.
  lwa_saida-campo7  = gwa_account-account_segment1.
  lwa_saida-campo8  = gwa_account-account_segment2.
  lwa_saida-campo9  = gwa_account-account_segment3.
  lwa_saida-campo10 = gwa_account-account_segment4.
  lwa_saida-campo11 = gwa_account-account_segment5.
  lwa_saida-campo12 = gwa_account-account_segment6.
  lwa_saida-campo13 = gwa_account-account_segment7.
  lwa_saida-campo14 = gwa_account-budget_period_name.
  APPEND lwa_saida TO git_saida.

ENDFORM.
*&---------------------------------------------------------------------*
*&  FORM F_MONTA_SAIDA_CSV
*&---------------------------------------------------------------------*
FORM f_monta_saida_csv.

  CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
    EXPORTING
      i_field_seperator    = ';'
    TABLES
      i_tab_sap_data       = git_saida
    CHANGING
      i_tab_converted_data = git_saida_csv
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&  FORM F_LIMPA_VARIAVEIS
*&---------------------------------------------------------------------*
FORM f_limpa_variaveis.

  CLEAR: gwa_rseg, gwa_makt, gwa_lfa1, gwa_ekko,
         gwa_ekpo, gwa_ekkn, gwa_cabecalho, gwa_itens,
         gwa_account.

ENDFORM.
*&---------------------------------------------------------------------*
*&  FORM F_GRAVA_ARQUIVO
*&---------------------------------------------------------------------*
FORM f_grava_arquivo.

  DATA: lo_ref_client TYPE REF TO if_http_client.

  IF p_local IS NOT INITIAL.
    PERFORM f_converte_tabela_csv.
  ELSEIF p_servi IS NOT INITIAL.
    PERFORM f_envia_arquivo.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&  FORM F_CONVERTE_TABELA_CSV
*&---------------------------------------------------------------------*
FORM f_converte_tabela_csv.

  DATA: lva_tamanho  TYPE i,
        lva_filename TYPE string.

  lva_filename = p_file.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lva_filename
    IMPORTING
      filelength              = lva_tamanho
    TABLES
      data_tab                = git_saida_csv
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ENVIA_ARQUIVO
*&---------------------------------------------------------------------*
FORM f_envia_arquivo .

  DATA lv_xstring TYPE xstring.

  PERFORM f_transforma_csv_to_xstring CHANGING lv_xstring.

  TRY.

      zcl_integracao_coupa_ftp=>zif_integracao_coupa_ftp~enviar_coupa( EXPORTING i_xfile = lv_xstring i_file_name = p_nome ).

    CATCH zcx_integracao INTO DATA(ex_int).
      "ex_int->get_longtext( ).
    CATCH zcx_error INTO DATA(ex_erro).
      "ex_erro->get_longtext( ).
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TRANSFORMA_CSV_TO_STRING
*&---------------------------------------------------------------------*
FORM f_transforma_csv_to_xstring CHANGING p_xstring TYPE xstring.

  DATA lv_string TYPE string.

  CLEAR p_xstring.

  LOOP AT git_saida_csv ASSIGNING FIELD-SYMBOL(<fs_line>).

    lv_string = lv_string && <fs_line> && cl_abap_char_utilities=>cr_lf.

  ENDLOOP.

  CHECK lv_string IS NOT INITIAL.

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = lv_string
    IMPORTING
      buffer = p_xstring.

ENDFORM.
