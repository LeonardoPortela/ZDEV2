CLASS lcl_utilities IMPLEMENTATION. "Utilidades para todos as clases.

  METHOD calculate_days_by_datum.
    rv_date = sy-datum - iv_number_of_days.
  ENDMETHOD.

  METHOD remover_zeros_esquerda.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = iv_input
      IMPORTING
        output = ev_ouput.

  ENDMETHOD.

  METHOD adicionar_zeros_esquerda.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iv_input
      IMPORTING
        output = ev_ouput.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_coupa_integration_log IMPLEMENTATION. "Classe responsável por administrar log de integração - Tabela ZINTEGRCOUPA01.

  METHOD insert_new_log_key.

    DATA: ls_zintegrcoupa01 TYPE zintegrcoupa01.

    FREE: ls_zintegrcoupa01.

    ls_zintegrcoupa01-id_integr  = iv_zcoupa_integration_key-id_integr.
    ls_zintegrcoupa01-ident_proc = iv_zcoupa_integration_key-ident_proc.
    ls_zintegrcoupa01-dt_atual   = sy-datum.
    ls_zintegrcoupa01-hr_atual   = sy-uzeit.
    ls_zintegrcoupa01-status     = space.
    APPEND ls_zintegrcoupa01 TO me->gt_zintegrcoupa01.

  ENDMETHOD.

  METHOD set_executed_status.

    READ TABLE me->gt_zintegrcoupa01 ASSIGNING FIELD-SYMBOL(<zintegrcoupa0>) WITH KEY id_integr  = iv_zcoupa_integration_key-id_integr
                                                                                      ident_proc = iv_zcoupa_integration_key-ident_proc.
    IF sy-subrc IS INITIAL.
      <zintegrcoupa0>-status = 'S'. "Status - Integração processa com sucesso.
    ENDIF.

  ENDMETHOD.

  METHOD save_log.

    CHECK me->gt_zintegrcoupa01 IS NOT INITIAL.

    MODIFY zintegrcoupa01 FROM TABLE me->gt_zintegrcoupa01.

    COMMIT WORK.

  ENDMETHOD.

  METHOD check_existence_of_key.

    SELECT COUNT(*)
      FROM zintegrcoupa01
      WHERE id_integr  = iv_zcoupa_integration_key-id_integr
      AND   ident_proc = iv_zcoupa_integration_key-ident_proc
      AND   status     = 'S'.
    IF sy-subrc IS INITIAL.
      rv_existence = 'X'.
    ELSE.
      me->insert_new_log_key( iv_zcoupa_integration_key
                                   = iv_zcoupa_integration_key ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_supplier_data_coupa IMPLEMENTATION. "Responsabilidade -> Buscar dados fornecedores para importação Coupa.

  METHOD constructor.

    IF iv_days IS INITIAL.
      me->gv_amount_days_ago = 1.
    ELSE.
      me->gv_amount_days_ago = iv_days.
    ENDIF.

    rg_lifnr[] = iv_lifnr[].

    me->gv_base_date = lcl_utilities=>calculate_days_by_datum( iv_number_of_days
                                                                 = me->gv_amount_days_ago ).

    IF rg_lifnr[] IS NOT INITIAL.
      CLEAR: me->gv_base_date.
    ENDIF.

    CREATE OBJECT me->go_coupa_integration_log.

  ENDMETHOD.

  METHOD execute.
    me->build_data( ).
  ENDMETHOD.

  METHOD build_data.

    DATA: l_id    TYPE srt_xml_data-tag_value,
          l_como  TYPE srt_xml_data-tag_value,
          l_lifnr TYPE ty_lifnr,
          l_cnpj  TYPE c LENGTH 18.

    DATA: lt_supplier_address  TYPE zcoupa_supplier_address_tt,
          ls_supplier_address  TYPE zcoupa_supplier_address,
          ls_integration_key   TYPE zcoupa_integration_key,
          lv_cnpj              TYPE char18,
          lv_comodity_name     TYPE srt_xml_data-tag_value,
          lv_id_adress_primary TYPE srt_xml_data-tag_value,
          lv_id_adress_suplier TYPE srt_xml_data-tag_value,
          lv_id_contact        TYPE srt_xml_data-tag_value.


    "Fornecedores Criados
    SELECT lfa1~lifnr,      lfa1~name1,      lfa1~name2,
           lfa1~stcd1,      lfa1~stcd2,      lfa1~stcd3,
           lfa1~stkzn,      lfa1~loevm,      lfa1~sperr,      lfa1~sperq,
           lfa1~ktokk,      lfa1~gbdat,      lfa1~anred,
           lfa1~adrnr,      lfb1~bukrs,      lfb1~pernr,
           lfb1~zterm,      adrc~addrnumber, adrc~house_num1,
           adrc~name_co,    adrc~city1,      adrc~region,     adrc~street,
           adrc~post_code1, adrc~sort1,      adrc~sort2,
           lfa1~gbort,      lfa1~stenr      "lfa1~telf1,       lfa1~telf2 "128328-Integração de dados de contato Coupa x SAP - 20.06.2024 - Vitor Rienzo
      FROM lfa1
      INNER JOIN lfb1
      ON lfa1~lifnr = lfb1~lifnr
      INNER JOIN adrc
      ON lfa1~adrnr = adrc~addrnumber
      INTO TABLE @DATA(gt_supplier_created)
      WHERE lfa1~lifnr IN @me->rg_lifnr
      AND   lfa1~erdat GE @me->gv_base_date
      AND EXISTS ( SELECT * "Verificar se fornecedor está no set de empresas que utilizam o Coupa
                    FROM  setleaf
                    WHERE setname = 'MAGGI_EMPRESAS_COUPA'
                    AND   valfrom = lfb1~bukrs
                  AND EXISTS ( SELECT *
                                  FROM setleaf
                                  WHERE setname EQ 'MAGGI_COUPA_FORN'
                                  AND   valfrom = lfa1~ktokk ) ).
    IF sy-subrc IS INITIAL.
      SELECT lfbk~lifnr, lfbk~bankl, lfbk~bankn, bnka~banka
        INTO TABLE @DATA(lt_bancos_created)
        FROM lfbk
        INNER JOIN bnka
        ON  lfbk~banks = bnka~banks
        AND lfbk~bankl = bnka~bankl
        FOR ALL ENTRIES IN @gt_supplier_created
        WHERE lfbk~lifnr = @gt_supplier_created-lifnr
        AND   lfbk~bvtyp = '0001'
        AND   lfbk~banks = 'BR'.
      IF sy-subrc IS INITIAL.
        SORT lt_bancos_created BY lifnr.
      ENDIF.

*>>>Begin-128328-Integração de dados de contato Coupa x SAP - 20.06.2024 - Vitor Rienzo
***      SELECT addrnumber, smtp_addr
***        FROM adr6
***        INTO TABLE @DATA(lt_adr6_created)
***        FOR ALL ENTRIES IN @gt_supplier_created
***        WHERE addrnumber = @gt_supplier_created-adrnr.
*<<<End-128328-Integração de dados de contato Coupa x SAP - 20.06.2024 - Vitor Rienzo

    ENDIF.

    SORT gt_supplier_created BY lifnr bukrs.

    LOOP AT gt_supplier_created INTO DATA(ls_supplier_created).
      CLEAR: gs_coupa_supplier_create, ls_integration_key.

      FREE: lt_supplier_address, ls_supplier_address, l_id, l_cnpj, l_como.

      "<empresas>
      LOOP AT gt_supplier_created INTO DATA(ls_created) WHERE lifnr = ls_supplier_created-lifnr.
        CONCATENATE gs_coupa_supplier_create-custom_fields-empresas ls_created-bukrs '/' INTO gs_coupa_supplier_create-custom_fields-empresas.
      ENDLOOP.

      READ TABLE lt_bancos_created INTO DATA(ls_bancos_created) WITH KEY lifnr = ls_supplier_created-lifnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        "CONCATENATE 'Banco:' ls_bancos_created-bankl(3) '-' ls_bancos_created-banka '-' 'Ag.' ls_bancos_created-bankl+5 '-' 'Conta:' ls_bancos_created-bankn INTO gs_coupa_supplier_create-account_number SEPARATED BY space.
        CONCATENATE 'Banco:' ls_bancos_created-bankl(3) '-' ls_bancos_created-banka '-' 'Ag.' ls_bancos_created-bankl+4 '-' 'Conta:' ls_bancos_created-bankn INTO gs_coupa_supplier_create-account_number SEPARATED BY space.

      ENDIF.

      lcl_utilities=>remover_zeros_esquerda( EXPORTING
                                              iv_input = ls_supplier_created-lifnr
                                             IMPORTING
                                              ev_ouput = ls_supplier_created-lifnr ).

      READ TABLE me->gt_coupa_supplier_create WITH KEY number = ls_supplier_created-lifnr TRANSPORTING NO FIELDS.
      IF sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

*>>>Begin-128328-Integração de dados de contato Coupa x SAP - 20.06.2024 - Vitor Rienzo
***      DATA(ls_adr6_created) = VALUE #( lt_adr6_created[ addrnumber = ls_supplier_created-adrnr ] OPTIONAL ).
*<<<End-128328-Integração de dados de contato Coupa x SAP - 20.06.2024 - Vitor Rienzo

      IF ls_supplier_created-stkzn = abap_false.
        "<name>
        CONCATENATE ls_supplier_created-lifnr '-' ls_supplier_created-name1 ls_supplier_created-name2 '-' ls_supplier_created-stcd1 INTO gs_coupa_supplier_create-name SEPARATED BY space.

        IF ls_supplier_created-ktokk EQ 'ZFEX'
*-CS1101072-#RIMINI-06.06.2023-BEGIN
          OR ls_supplier_created-ktokk EQ 'ZIMP'.
*-CS1101072-#RIMINI-06.06.2023-END
          gs_coupa_supplier_create-custom_fields-cnpj = ls_supplier_created-lifnr.
          l_cnpj = ls_supplier_created-lifnr.
        ELSE.
          gs_coupa_supplier_create-custom_fields-cnpj = ls_supplier_created-stcd1.
          l_cnpj = ls_supplier_created-stcd1.
        ENDIF.


        l_id  = zcl_integracao_supplier_coupa=>zif_integracao_supplier_coupa~get_id_fornecedor_by_cnpj( EXPORTING
                                                                                                         iv_cnpj = l_cnpj
                                                                                                        IMPORTING
                                                                                                          e_commodity_name = l_como ).
      ELSE.
        "<name>
        CONCATENATE ls_supplier_created-lifnr '-' ls_supplier_created-name1 ls_supplier_created-name2 '-' ls_supplier_created-stcd2 INTO gs_coupa_supplier_create-name SEPARATED BY space.

        l_id = zcl_integracao_supplier_coupa=>zif_integracao_supplier_coupa~get_id_fornecedor_by_cpf( EXPORTING
                                                                                                       iv_cpf           = ls_supplier_created-stcd2
                                                                                                      IMPORTING
                                                                                                       e_commodity_name = l_como ).
      ENDIF.
      "<display-name>
*-CS1101882-#RIMINI-06.21.2023-
*      CONCATENATE ls_supplier_created-sort1 ls_supplier_created-sort2 '-' ls_supplier_created-region INTO gs_coupa_supplier_create-display_name.
      CONCATENATE ls_supplier_created-sort1
                  '-'
                  ls_supplier_created-region
             INTO gs_coupa_supplier_create-display_name.
*-CS1101882-#RIMINI-06.21.2023-END
      "<number>
      gs_coupa_supplier_create-number = ls_supplier_created-lifnr.
      "<status>
      gs_coupa_supplier_create-status = 'active'.
*      gs_coupa_supplier_create-status = 'desbloqueado'.
      "<po-method>
      gs_coupa_supplier_create-po_method = 'prompt'.
*      "<po-email>
*      gs_coupa_supplier_create-po_email  = ls_supplier_created-smtp_addr.
*      IF gs_coupa_supplier_create-po_email IS INITIAL.
*        gs_coupa_supplier_create-po_email = 'dummy@amaggi.com.br'.
*      ENDIF.
      "<buyer-hold>
      gs_coupa_supplier_create-buyer_hold = 'true'.
      "<tax-id>
      IF ls_supplier_created-ktokk EQ 'ZFEX'
*-CS1101072-#RIMINI-06.06.2023-BEGIN
          OR ls_supplier_created-ktokk EQ 'ZIMP'.
*-CS1101072-#RIMINI-06.06.2023-END
*        gs_coupa_supplier_create-tax_id = ls_supplier_created-lifnr.
        gs_coupa_supplier_create-custom_fields-tax_registration-number = ls_supplier_created-lifnr.
        gs_coupa_supplier_create-custom_fields-cnpjcpfvat = ls_supplier_created-lifnr.
      ELSE.
        IF ls_supplier_created-stcd2 IS NOT INITIAL.
          gs_coupa_supplier_create-custom_fields-tax_registration-number = ls_supplier_created-stcd2.
          gs_coupa_supplier_create-custom_fields-cnpjcpfvat = ls_supplier_created-stcd2.
        ELSE.
          gs_coupa_supplier_create-custom_fields-tax_registration-number = ls_supplier_created-stcd1.
          gs_coupa_supplier_create-custom_fields-cnpjcpfvat = ls_supplier_created-stcd1.
        ENDIF.
      ENDIF.

      "<on-hold>
      gs_coupa_supplier_create-on_hold = 'false'.
      "<invoice-matching-level>
      gs_coupa_supplier_create-invoice_matching_level = '3-way'.

      "<custom-fields>

      "<external-ref-code>
      gs_coupa_supplier_create-custom_fields-tipo_de_fornecedor-external_ref_code = ls_supplier_created-ktokk.
      "<status-homologacao>
      gs_coupa_supplier_create-custom_fields-status_homologacao-external_ref_code = 'homologado'.
      "<inscricao-estadual>
      IF ls_supplier_created-stkzn = abap_false.
        gs_coupa_supplier_create-custom_fields-inscricao_estadual = ls_supplier_created-stcd3.
      ENDIF.
      "<email-nota-fiscal>
      gs_coupa_supplier_create-custom_fields-email_nota_fiscal = 'nfe.fiscal@amaggi.com.br'.
      "<rg>
      IF ls_supplier_created-stkzn = abap_true.
        "gs_coupa_supplier_create-custom_fields-rg = ls_supplier_created-stcd3.
      ENDIF.
      "<orgao-expedidor>
      gs_coupa_supplier_create-custom_fields-orgao_expedidor = ls_supplier_created-gbort.
      "<pis-nit>
      gs_coupa_supplier_create-custom_fields-pis_nit = ls_supplier_created-stenr.
      "<matrcula-funcionrio>
      gs_coupa_supplier_create-custom_fields-matrcula_funcionrio = ls_supplier_created-pernr.
      "<no-possui-ie-ou-isento>
      IF ls_supplier_created-stcd3 = 'isento'.
        gs_coupa_supplier_create-custom_fields-no_possui_ie_ou_isento = 'true'.
      ELSE.
        gs_coupa_supplier_create-custom_fields-no_possui_ie_ou_isento = 'false'.
      ENDIF.
      "<forma-de-tratamento>
      gs_coupa_supplier_create-custom_fields-forma_de_tratamento = ls_supplier_created-anred.
      "<organizao-de-compras>
      gs_coupa_supplier_create-custom_fields-organizao_de_compras = 'OC01'.
      "<cpf>
      gs_coupa_supplier_create-custom_fields-cpf = ls_supplier_created-stcd2.

      "<primary-contact> -> <email>
      " gs_coupa_supplier_create-primary_contact-email = ls_supplier_created-smtp_addr.
      "<primary-contact> -> <name-given>
      CONCATENATE ls_supplier_created-name1 ls_supplier_created-name2 INTO gs_coupa_supplier_create-primary_contact-name_given.
      "<primary-contact> -> <name-family>
      gs_coupa_supplier_create-primary_contact-name_family = ls_supplier_created-name2.
*      "<primary-address> -> <street1>
*      CONCATENATE ls_supplier_created-street ',' ls_supplier_created-house_num1 INTO gs_coupa_supplier_create-primary_address-street1.
*      "<primary-address> -> <street2>
*      gs_coupa_supplier_create-primary_address-street2 = ls_supplier_created-name_co.
*      "<primary-address> -> <city>
*      gs_coupa_supplier_create-primary_address-city = ls_supplier_created-city1.
*      "<primary-address> -> <state>
*      gs_coupa_supplier_create-primary_address-state = ls_supplier_created-region.
*      "<primary-address> -> <postal-code>
*      gs_coupa_supplier_create-primary_address-postal_code = ls_supplier_created-post_code1.


      CONCATENATE ls_supplier_created-street ',' ls_supplier_created-house_num1 INTO ls_supplier_address-street1.
      ls_supplier_address-street2     = ls_supplier_created-name_co.
      ls_supplier_address-city        = ls_supplier_created-city1.
      ls_supplier_address-state       = ls_supplier_created-region.
      ls_supplier_address-postal_code = ls_supplier_created-post_code1.
      APPEND ls_supplier_address TO lt_supplier_address.
      "<supplier-address>
      gs_coupa_supplier_create-supplier_addresses = lt_supplier_address.

*      gs_coupa_supplier_create-custom_fields-tax_id_amaggi = gs_coupa_supplier_create-tax_id.
      gs_coupa_supplier_create-custom_fields-razo_social   = ls_supplier_created-name1.

      ls_integration_key-id_integr  = ls_supplier_created-lifnr.
      ls_integration_key-ident_proc = 'FO'.

      CHECK me->go_coupa_integration_log->check_existence_of_key( iv_zcoupa_integration_key
                                                                   = ls_integration_key ) EQ abap_false.

      IF l_id IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_supplier_created-lifnr
          IMPORTING
            output = l_lifnr-lifnr.
        APPEND l_lifnr TO gt_lifnr. "Se já existe o registro, adiciona como modificação
        CONTINUE.
      ENDIF.

*>>>Begin-128328-Integração de dados de contato Coupa x SAP - 20.06.2024 - Vitor Rienzo
      "<supplier-information-contacts>
***      gs_coupa_supplier_create-supplier_information_contacts-name_fullname = ls_supplier_created-name1.
***      gs_coupa_supplier_create-supplier_information_contacts-email         = ls_adr6_created-smtp_addr.
***
***      "<phone-mobile>
***      gs_coupa_supplier_create-supplier_information_contacts-phone_mobile-country_code = ''.
***      gs_coupa_supplier_create-supplier_information_contacts-phone_mobile-area_code    = ''.
***      gs_coupa_supplier_create-supplier_information_contacts-phone_mobile-number       = ls_supplier_created-telf2.
***
***      "<phone-work>
***      gs_coupa_supplier_create-supplier_information_contacts-phone_work-country_code   = ''.
***      gs_coupa_supplier_create-supplier_information_contacts-phone_work-area_code      = ''.
***      gs_coupa_supplier_create-supplier_information_contacts-phone_work-number         = ls_supplier_created-telf1.
      "</supplier-information-contacts>
*<<<End-128328-Integração de dados de contato Coupa x SAP - 20.06.2024 - Vitor Rienzo

      APPEND gs_coupa_supplier_create TO me->gt_coupa_supplier_create.
    ENDLOOP.

    FREE: gt_supplier_created, ls_supplier_created, gs_coupa_supplier_create, ls_supplier_address, lt_supplier_address, ls_integration_key.

    IF me->gv_base_date IS INITIAL.
      me->gv_base_date = lcl_utilities=>calculate_days_by_datum( iv_number_of_days
                                                                   = 1 ).
    ENDIF.

    "Fornecedores Modificados.
    SELECT objectid, objectclas
      FROM cdhdr
      INTO TABLE @DATA(gt_cdhdr)
      WHERE udate      GE @me->gv_base_date
      AND   objectclas IN ( 'KRED', 'ADRESSE' )
      AND   tcode      IN ( 'XK02', 'XK03', 'XK05', 'XK06' ).
    IF sy-subrc IS INITIAL.
      LOOP AT gt_cdhdr INTO DATA(ls_cdhdr).
        IF ls_cdhdr-objectclas EQ 'ADRESSE'.
          SELECT lifnr
            FROM lfa1
            INTO TABLE @DATA(lt_lifnr)
            WHERE adrnr = @ls_cdhdr-objectid+4.
          IF sy-subrc IS INITIAL.
            APPEND LINES OF lt_lifnr TO gt_lifnr.
          ENDIF.
        ELSEIF ls_cdhdr-objectclas EQ 'KRED'.
          APPEND VALUE #( lifnr = ls_cdhdr-objectid ) TO gt_lifnr.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF gt_lifnr IS NOT INITIAL.

      SORT  gt_lifnr BY lifnr.
      DELETE ADJACENT DUPLICATES FROM gt_lifnr COMPARING lifnr.

      SELECT lfa1~lifnr,      lfa1~name1,      lfa1~name2,
             lfa1~stcd1,      lfa1~stcd2,      lfa1~stcd3,
             lfa1~ktokk,      lfa1~gbdat,      lfa1~anred,
             lfa1~sperq,      lfa1~adrnr,      lfb1~bukrs,
             lfb1~pernr,      lfa1~loevm,      lfa1~nodel,
             lfa1~sperr,      lfb1~zterm,      adrc~addrnumber,
             adrc~house_num1, adrc~name_co,    adrc~city1,
             adrc~region,     adrc~post_code1, adrc~sort1,
             adrc~sort2,      lfa1~stkzn,
             adrc~street,     lfa1~gbort,      lfa1~stenr
**             lfa1~telf1,      lfa1~telf2 "128328-Integração de dados de contato Coupa x SAP - 20.06.2024 - Vitor Rienzo
        FROM lfa1
        INNER JOIN lfb1
        ON lfa1~lifnr = lfb1~lifnr
        INNER JOIN adrc
        ON lfa1~adrnr = adrc~addrnumber
        INTO TABLE @DATA(gt_supplier_modified)
        FOR ALL ENTRIES IN @gt_lifnr
        WHERE lfa1~lifnr IN @me->rg_lifnr
        AND   lfa1~lifnr EQ @gt_lifnr-lifnr
        AND EXISTS ( SELECT * "Verificar se fornecedor está no set de empresas que utilizam o Coupa
                      FROM  setleaf
                      WHERE setname = 'MAGGI_EMPRESAS_COUPA'
                      AND   valfrom = lfb1~bukrs
                    AND EXISTS ( SELECT *
                                    FROM setleaf
                                    WHERE setname EQ 'MAGGI_COUPA_FORN'
                                    AND   valfrom = lfa1~ktokk ) ).
      IF sy-subrc IS INITIAL.

        SELECT lfbk~lifnr, lfbk~bankl, lfbk~bankn, bnka~banka
          INTO TABLE @DATA(lt_bancos_modified)
          FROM lfbk
          INNER JOIN bnka
          ON  lfbk~banks = bnka~banks
          AND lfbk~bankl = bnka~bankl
          FOR ALL ENTRIES IN @gt_lifnr
          WHERE lfbk~lifnr = @gt_lifnr-lifnr
          AND   lfbk~bvtyp = '0001'
          AND   lfbk~banks = 'BR'.
        IF sy-subrc IS INITIAL.
          SORT lt_bancos_modified BY lifnr.
        ENDIF.

*>>>Begin-128328-Integração de dados de contato Coupa x SAP - 20.06.2024 - Vitor Rienzo
***      SELECT addrnumber, smtp_addr
***        FROM adr6
***        INTO TABLE @DATA(lt_adr6_modified)
***        FOR ALL ENTRIES IN @gt_supplier_modified
***        WHERE addrnumber = @gt_supplier_modified-adrnr.
*<<<End-128328-Integração de dados de contato Coupa x SAP - 20.06.2024 - Vitor Rienzo

      ENDIF.

      SORT gt_supplier_modified BY lifnr bukrs.

      LOOP AT gt_supplier_modified INTO DATA(ls_supplier_modified).
        CLEAR: gs_coupa_supplier_modify, ls_supplier_address, lt_supplier_address, ls_integration_key, lv_id_adress_primary, lv_id_adress_suplier, lv_id_contact.

        FREE: lt_supplier_address, ls_supplier_address.

        "<empresas>
        LOOP AT gt_supplier_modified INTO DATA(ls_modified) WHERE lifnr = ls_supplier_modified-lifnr.
          CONCATENATE gs_coupa_supplier_modify-custom_fields-empresas ls_modified-bukrs '/' INTO gs_coupa_supplier_modify-custom_fields-empresas.
        ENDLOOP.

        READ TABLE lt_bancos_modified INTO DATA(ls_bancos_modified) WITH KEY lifnr = ls_supplier_modified-lifnr BINARY SEARCH.
        IF sy-subrc IS INITIAL.
*          CONCATENATE 'Banco:' ls_bancos_modified-bankl(3) '-' ls_bancos_modified-banka '-' 'Ag.' ls_bancos_modified-bankl+5 '-' 'Conta:' ls_bancos_modified-bankn INTO gs_coupa_supplier_modify-account_number SEPARATED BY space..
          CONCATENATE 'Banco:' ls_bancos_modified-bankl(3) '-' ls_bancos_modified-banka '-' 'Ag.' ls_bancos_modified-bankl+4 '-' 'Conta:' ls_bancos_modified-bankn INTO gs_coupa_supplier_modify-account_number SEPARATED BY space..
        ENDIF.

        lcl_utilities=>remover_zeros_esquerda( EXPORTING
                                                iv_input = ls_supplier_modified-lifnr
                                               IMPORTING
                                                ev_ouput = ls_supplier_modified-lifnr  ).

        READ TABLE gt_coupa_supplier_modify WITH KEY number = ls_supplier_modified-lifnr TRANSPORTING NO FIELDS.
        IF sy-subrc IS INITIAL.
          CONTINUE.
        ENDIF.

*>>>Begin-128328-Integração de dados de contato Coupa x SAP - 20.06.2024 - Vitor Rienzo
***        DATA(ls_adr6_modified) = VALUE #( lt_adr6_modified[ addrnumber = ls_supplier_modified-adrnr ] OPTIONAL ).
*<<<End-128328-Integração de dados de contato Coupa x SAP - 20.06.2024 - Vitor Rienzo

        "<name>
        IF ls_supplier_modified-stkzn = abap_false.
          "<name>
          CONCATENATE ls_supplier_modified-lifnr '-' ls_supplier_modified-name1 ls_supplier_modified-name2 '-' ls_supplier_modified-stcd1
                                                     INTO gs_coupa_supplier_modify-name SEPARATED BY space.

          gs_coupa_supplier_modify-custom_fields-cnpj = ls_supplier_modified-stcd1.

          IF ls_supplier_modified-ktokk EQ 'ZFEX' "Fornecedor estrangeiro
*-CS1101072-#RIMINI-06.06.2023-BEGIN
            OR ls_supplier_modified-ktokk EQ 'ZIMP'.
*-CS1101072-#RIMINI-06.06.2023-END
            lv_cnpj = ls_supplier_modified-lifnr.
            gs_coupa_supplier_modify-custom_fields-cnpjcpfvat = ls_supplier_modified-lifnr.
          ELSEIF ls_supplier_modified-stcd2 IS NOT INITIAL.
            lv_cnpj = ls_supplier_modified-stcd2.
            gs_coupa_supplier_modify-custom_fields-cnpjcpfvat = ls_supplier_modified-stcd2.
          ELSE.
            lv_cnpj = ls_supplier_modified-stcd1.
            gs_coupa_supplier_modify-custom_fields-cnpjcpfvat = ls_supplier_modified-stcd1.
          ENDIF.
          "<id>
          gs_coupa_supplier_modify-id = zcl_integracao_supplier_coupa=>zif_integracao_supplier_coupa~get_id_fornecedor_by_cnpj( EXPORTING
                                                                                                                                 iv_cnpj = lv_cnpj
                                                                                                                                IMPORTING
                                                                                                                                 e_commodity_name    = lv_comodity_name
                                                                                                                                 e_id_adress_primary = lv_id_adress_primary
                                                                                                                                 e_id_adress_suplier = lv_id_adress_suplier
                                                                                                                                 e_id_contact        = lv_id_contact ).
        ELSE.
          "<name>
          CONCATENATE ls_supplier_modified-lifnr '-' ls_supplier_modified-name1 ls_supplier_modified-name2 '-' ls_supplier_modified-stcd2
                                                     INTO gs_coupa_supplier_modify-name SEPARATED BY space.

          IF ls_supplier_modified-ktokk EQ 'ZFEX'
*-CS1101072-#RIMINI-06.06.2023-BEGIN
            OR ls_supplier_modified-ktokk EQ 'ZIMP'.
*-CS1101072-#RIMINI-06.06.2023-END
            gs_coupa_supplier_modify-tax_id = ls_supplier_modified-lifnr.
            gs_coupa_supplier_modify-custom_fields-cnpjcpfvat = ls_supplier_modified-lifnr.
          ELSEIF ls_supplier_modified-stcd2 IS NOT INITIAL.
            gs_coupa_supplier_modify-tax_id = ls_supplier_modified-stcd2.
            gs_coupa_supplier_modify-custom_fields-cnpjcpfvat = ls_supplier_modified-stcd2.
          ENDIF.
          "<id>
          gs_coupa_supplier_modify-id = zcl_integracao_supplier_coupa=>zif_integracao_supplier_coupa~get_id_fornecedor_by_cpf( EXPORTING
                                                                                                                                iv_cpf = ls_supplier_modified-stcd2
                                                                                                                               IMPORTING
                                                                                                                                 e_commodity_name    = lv_comodity_name
                                                                                                                                 e_id_adress_primary = lv_id_adress_primary
                                                                                                                                 e_id_adress_suplier = lv_id_adress_suplier
                                                                                                                                 e_id_contact        = lv_id_contact ).
        ENDIF.
        "<display-name>
*-CS1101882-#RIMINI-06.21.2023-BEGIN
*        CONCATENATE ls_supplier_modified-sort1 ls_supplier_modified-sort2 '-' ls_supplier_modified-region INTO gs_coupa_supplier_modify-display_name.
        CONCATENATE ls_supplier_modified-sort1
                    '-'
                    ls_supplier_modified-region
               INTO gs_coupa_supplier_modify-display_name.
*-CS1101882-#RIMINI-06.21.2023-END
        "<number>
        gs_coupa_supplier_modify-number = ls_supplier_modified-lifnr.
        "<status>
        IF ls_supplier_modified-loevm EQ 'X'  OR ls_supplier_modified-sperr EQ 'X'
        OR ls_supplier_modified-sperq EQ '99' OR ls_supplier_modified-nodel EQ 'X'.
          gs_coupa_supplier_modify-status = 'inactive'.
*          gs_coupa_supplier_modify-status = 'bloqueado'.
        ELSE.
          gs_coupa_supplier_modify-status = 'active'.
*          gs_coupa_supplier_modify-status = 'desbloqueado'.
        ENDIF.

        gs_coupa_supplier_modify-buyer_hold = 'true'.
        "<on-hold>
        gs_coupa_supplier_modify-on_hold = 'false'.
        "<invoice-matching-level>
        gs_coupa_supplier_modify-invoice_matching_level = '3-way'.

        IF lv_comodity_name EQ 'Outros - (Não Crítico)'.
          gs_coupa_supplier_modify-custom_fields-status_homologacao-external_ref_code = 'homologado'.
        ELSE.
          gs_coupa_supplier_modify-custom_fields-status_homologacao-external_ref_code = 'Em processo de homologação'.
        ENDIF.

        "<external-ref-code>
        gs_coupa_supplier_modify-custom_fields-tipo_de_fornecedor-external_ref_code = ls_supplier_modified-ktokk.
        "<inscricao-estadual>
        IF ls_supplier_modified-stkzn = abap_false.
          gs_coupa_supplier_modify-custom_fields-inscricao_estadual = ls_supplier_modified-stcd3.
        ENDIF.
        "<email-nota-fiscal>
        gs_coupa_supplier_modify-custom_fields-email_nota_fiscal = 'nfe.fiscal@amaggi.com.br'.
        "<rg>
        IF ls_supplier_modified-stkzn = abap_true.
          "gs_coupa_supplier_modify-custom_fields-rg = ls_supplier_modified-stcd3.
        ENDIF.
        "<orgao-expedidor>
        gs_coupa_supplier_modify-custom_fields-orgao_expedidor = ls_supplier_modified-gbort.
        "<pis-nit>
        gs_coupa_supplier_modify-custom_fields-pis_nit = ls_supplier_modified-stenr.
        "<matrcula-funcionrio>
        gs_coupa_supplier_modify-custom_fields-matrcula_funcionrio = ls_supplier_modified-pernr.
        "<no-possui-ie-ou-isento>
        IF ls_supplier_modified-stcd3 = 'isento'.
          gs_coupa_supplier_modify-custom_fields-no_possui_ie_ou_isento = 'true'.
        ELSE.
          gs_coupa_supplier_modify-custom_fields-no_possui_ie_ou_isento = 'false'.
        ENDIF.
        "<forma-de-tratamento>
        gs_coupa_supplier_modify-custom_fields-forma_de_tratamento = ls_supplier_modified-anred.
        "<organizao-de-compras>
        gs_coupa_supplier_modify-custom_fields-organizao_de_compras = 'OC01'.
        "<cpf>
        gs_coupa_supplier_modify-custom_fields-cpf = ls_supplier_modified-stcd2.

        gs_coupa_supplier_modify-custom_fields-tax_id_amaggi = gs_coupa_supplier_modify-tax_id.
        gs_coupa_supplier_modify-custom_fields-razo_social   = ls_supplier_modified-name1.

*>>>Begin-128328-Integração de dados de contato Coupa x SAP - 20.06.2024 - Vitor Rienzo
      "<supplier-information-contacts>
***        gs_coupa_supplier_modify-supplier_information_contacts-id            = '138'.
***        gs_coupa_supplier_modify-supplier_information_contacts-kind          = 'Primary'.
***        gs_coupa_supplier_modify-supplier_information_contacts-name_fullname = ls_supplier_modified-name1.
***        gs_coupa_supplier_modify-supplier_information_contacts-email         = ls_adr6_modified-smtp_addr.
***
***        "<phone-mobile>
***        gs_coupa_supplier_modify-supplier_information_contacts-phone_mobile-country_code = ''.
***        gs_coupa_supplier_modify-supplier_information_contacts-phone_mobile-area_code    = ''.
***        gs_coupa_supplier_modify-supplier_information_contacts-phone_mobile-number       = ls_supplier_modified-telf2.
***
***        "<phone-work>
***        gs_coupa_supplier_modify-supplier_information_contacts-phone_work-country_code   = ''.
***        gs_coupa_supplier_modify-supplier_information_contacts-phone_work-area_code      = ''.
***        gs_coupa_supplier_modify-supplier_information_contacts-phone_work-number         = ls_supplier_modified-telf1.
      "</supplier-information-contacts>
*<<<End-128328-Integração de dados de contato Coupa x SAP - 20.06.2024 - Vitor Rienzo

        APPEND gs_coupa_supplier_modify TO me->gt_coupa_supplier_modify.
      ENDLOOP.

    ENDIF.

    FREE: gt_cdhdr, ls_cdhdr, gt_lifnr, ls_supplier_address, lt_supplier_address, ls_supplier_modified, gt_supplier_modified, ls_integration_key.



  ENDMETHOD.

  METHOD get_import_data_create.
    rt_import_data = me->gt_coupa_supplier_create.
  ENDMETHOD.

  METHOD get_import_data_modify.
    rt_import_data = me->gt_coupa_supplier_modify.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv_return IMPLEMENTATION.

  METHOD constructor.

    cl_salv_table=>factory( IMPORTING
                              r_salv_table = go_alv
                            CHANGING
                              t_table      = gt_alv_data ).

    me->config_alv_columns( ).
    me->config_alv_functions( ).
    me->config_alv_events( ).

  ENDMETHOD.

  METHOD append_new_line.

    DATA: ls_alv TYPE zcoupa_import_data_alv.

    ls_alv-id_referencia  = iv_import_data-id_referencia.
    ls_alv-tp_referencia  = iv_import_data-tp_referencia.
    ls_alv-xml_icon       = '@0U@'.

    IF iv_integration_log-nm_code NE 200 AND
       iv_integration_log-nm_code NE 201 AND
       iv_integration_log-nm_code NE 202.
      ls_alv-status = '@02@'.
      ls_alv-xml    = iv_integration_log-ds_data_retorno.
    ELSE.
      ls_alv-status = '@01@'.
      ls_alv-xml    = iv_import_data-xml.
    ENDIF.

    APPEND ls_alv TO me->gt_alv_data.

  ENDMETHOD.

  METHOD config_alv_columns.

    go_columns = go_alv->get_columns( ).
    go_column ?= go_columns->get_column( 'XML' ).
    go_column->set_visible( abap_false ).

    go_columns = go_alv->get_columns( ).
    go_column ?= go_columns->get_column( 'XML_ICON' ).
    go_column->set_icon( abap_true ).
    go_column->set_cell_type( EXPORTING
                                value = if_salv_c_cell_type=>hotspot ).
    go_column->set_short_text( 'XML').

    go_columns = go_alv->get_columns( ).
    go_column ?= go_columns->get_column( 'STATUS' ).
    go_column->set_icon( abap_true ).
    go_column->set_short_text( 'Status').

  ENDMETHOD.

  METHOD config_alv_functions.

    go_funct = go_alv->get_functions( ).
    go_funct->set_all( abap_true ).

  ENDMETHOD.

  METHOD config_alv_events.

    go_events = go_alv->get_event( ).
    SET HANDLER go_alv_return->on_link_click FOR go_events.

  ENDMETHOD.

  METHOD display.

    go_alv->display( ).

  ENDMETHOD.

  METHOD on_link_click.

    READ TABLE gt_alv_data INTO DATA(ls_alv_data) INDEX row.
    IF sy-subrc IS INITIAL.
      CALL METHOD cl_abap_browser=>show_xml
        EXPORTING
          xml_string = ls_alv_data-xml.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
