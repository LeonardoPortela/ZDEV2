FUNCTION z_sd_autorizacao_embarque.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(NRO_SOL) TYPE  ZSDT0138-NRO_SOL
*"     REFERENCE(SEQ_CAM) TYPE  ZSDT0138-SEQ_CAM
*"     REFERENCE(SEQ) TYPE  ZSDT0138-SEQ
*"     REFERENCE(FRETE) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(FILIAL_RESP) TYPE  ZSDT0138-FILIAL_RESP OPTIONAL
*"     REFERENCE(PTO_COLETA) TYPE  LIFNR OPTIONAL
*"----------------------------------------------------------------------

  DATA: it_saida TYPE TABLE OF zauto_embarque.
  DATA: wa_saida TYPE zauto_embarque.

  DATA: vl_formname TYPE tdsfname,
        vl_name     TYPE rs38l_fnam.

  DATA: wl_name      TYPE thead-tdname,
        tg_texto     TYPE TABLE OF tline WITH HEADER LINE,
        tg_obser     TYPE TABLE OF tline WITH HEADER LINE,
        wa_obser     TYPE tline,
        tg_texto_132 TYPE TABLE OF tline WITH HEADER LINE.

  DATA: filial TYPE kna1-kunnr.

  SELECT * FROM zsdt0138
    INTO TABLE @DATA(it_zsdt0138)
*      WHERE NRO_SOL EQ '00418'
*        AND SEQ_CAM EQ '001'
*        AND SEQ EQ '002'.

*    001  00418 002
      WHERE nro_sol     EQ @nro_sol
        AND seq_cam     EQ @seq_cam
        AND seq         EQ @seq
*-------CS2019001896 - 11.01.2021 - inicio
        AND filial_resp EQ @filial_resp
*-------CS2019001896 - 11.01.2021 - inicio
        AND status      NE 'X'.
  .
  CHECK NOT it_zsdt0138 IS INITIAL.

  SELECT * FROM zsdt0082
    INTO TABLE @DATA(it_zsdt0082)
     FOR ALL ENTRIES IN @it_zsdt0138
       WHERE nro_sol EQ @it_zsdt0138-nro_sol
        AND seq EQ @it_zsdt0138-seq.

  IF NOT it_zsdt0082 IS INITIAL.
    SELECT * FROM vbap
    INTO TABLE @DATA(it_vbap)
      FOR ALL ENTRIES IN @it_zsdt0082
        WHERE vbeln EQ @it_zsdt0082-vbeln
          AND posnr EQ @it_zsdt0082-posnr.

    SELECT * FROM vbkd
      INTO TABLE @DATA(it_vbkd)
        FOR ALL ENTRIES IN @it_zsdt0082
          WHERE vbeln EQ @it_zsdt0082-vbeln.

  ENDIF.

  SELECT * FROM tvbur
    INTO TABLE @DATA(it_tvbur)
    FOR ALL ENTRIES IN @it_zsdt0138
    WHERE vkbur EQ @it_zsdt0138-filial_resp.

  IF it_tvbur IS NOT INITIAL.
    SELECT * FROM adrc
      INTO TABLE @DATA(it_adrc)
      FOR ALL ENTRIES IN @it_tvbur
      WHERE addrnumber EQ @it_tvbur-adrnr.
  ENDIF.

  SELECT * FROM ekko
  INTO TABLE @DATA(it_ekko)
    FOR ALL ENTRIES IN @it_zsdt0138
      WHERE ebeln EQ @it_zsdt0138-ebeln.

  IF it_ekko IS NOT INITIAL.
    SELECT * FROM lfa1
      INTO TABLE @DATA(it_lfa1)
      FOR  ALL ENTRIES IN @it_ekko
      WHERE lifnr EQ @it_ekko-lifnr.
  ENDIF.
*** Inicio - Rubenilson Pereira - 14.06.2022 - #56497
  SELECT * FROM lfa1
    APPENDING TABLE it_lfa1
    WHERE lifnr EQ pto_coleta.
*** Fim - Rubenilson Pereira - 14.06.2022 - #56497

  SELECT * FROM lfa1
    APPENDING TABLE it_lfa1
    FOR  ALL ENTRIES IN it_zsdt0138
    WHERE lifnr EQ it_zsdt0138-motorista.

  SELECT * FROM lfa1
    APPENDING TABLE it_lfa1
    FOR  ALL ENTRIES IN it_zsdt0138
    WHERE lifnr EQ it_zsdt0138-cod_transportadora.

  LOOP AT it_zsdt0138 INTO DATA(wa_0138).

    wa_saida-emissao = sy-datum.
    wa_saida-validade = sy-datum + 3.

    TRY .
        DATA(wa_vbkd) = it_vbkd[ vbeln = it_zsdt0082[ nro_sol = wa_0138-nro_sol
                                                 seq     = wa_0138-seq ]-vbeln ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_vbkd.
    ENDTRY.

    wa_saida-frete = wa_vbkd-inco1.

    TRY .
        DATA(wa_adrc) = it_adrc[ addrnumber = it_tvbur[ vkbur = wa_0138-filial_resp ]-adrnr ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    TRY .
        filial = it_zsdt0082[ nro_sol = wa_0138-nro_sol seq     = wa_0138-seq ]-werks.
      CATCH cx_sy_itab_line_not_found.
        CLEAR filial.
    ENDTRY.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = filial
      IMPORTING
        output = filial.

    SELECT SINGLE *
      FROM kna1
      INTO @DATA(wa_kna1)
      WHERE kunnr EQ @filial.


    wa_saida-cliente  = wa_kna1-name1.
    wa_saida-cidade   = wa_kna1-ort01.
    wa_saida-uf       = wa_kna1-regio.

    SELECT SINGLE tel_number
      FROM adr2
      INTO wa_saida-tel
      WHERE addrnumber EQ wa_kna1-adrnr.

    IF NOT wa_kna1-stcd1 IS INITIAL.
      WRITE wa_kna1-stcd1 USING EDIT MASK 'RR__.___.___/____-__'
            TO wa_saida-cpf_cnpj.
    ENDIF.

    wa_saida-i_e      = wa_kna1-stcd3.

*    WA_SAIDA-CLIENTE = WA_ADRC-NAME1.
*    WA_SAIDA-CIDADE  = WA_ADRC-CITY1.
*    WA_SAIDA-UF      = WA_ADRC-REGION.
*    WA_SAIDA-TEL     = WA_ADRC-TEL_NUMBER.
*    CASE WA_0138-FILIAL_RESP.
*      WHEN 'TROO'.
*        WA_SAIDA-CPF_CNPJ = '77.294.254/0016-70'.
*        WA_SAIDA-I_E      = '131511726'.
*      WHEN 'TPGA'.
*        WA_SAIDA-CPF_CNPJ = '77.294.254/0020-57'.
*        WA_SAIDA-I_E      = '1180830088'.
*    ENDCASE.

*** Inicio - Rubenilson Pereira - 14.06.2022 - #56497
    TRY .
        DATA(wa_lfa1) = it_lfa1[ lifnr = pto_coleta ].
      CATCH cx_sy_itab_line_not_found.
*** Fim - Rubenilson Pereira - 14.06.2022 - #56497
        TRY .
            wa_lfa1 = it_lfa1[ lifnr = it_ekko[ ebeln = wa_0138-ebeln ]-lifnr ].
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
    ENDTRY.

    wa_saida-fornecedor = wa_lfa1-name1.

    TRY .
        wa_saida-pedido_fornecedor = it_ekko[ ebeln = wa_0138-ebeln ]-ihrez.
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_saida-pedido_fornecedor.
    ENDTRY.

    wa_saida-pedido_amaggi = wa_0138-ebeln.


    wa_saida-municipio     = wa_lfa1-ort01.
    wa_saida-uf_f          = wa_lfa1-regio.
    wa_saida-endereco      = |{ wa_lfa1-stras }-{ wa_lfa1-ort02 }|.

    SELECT SINGLE nr_forn
      FROM zsdt0062
      INTO wa_saida-nr_forn
      WHERE ebeln EQ wa_0138-ebeln
      AND nro_sol EQ wa_0138-nro_sol
      AND seq EQ wa_0138-seq
      AND status NE 'E'.

    LOOP AT it_zsdt0082 INTO DATA(wa_0082) WHERE nro_sol = wa_0138-nro_sol AND
                                                 seq     = wa_0138-seq.

      SHIFT wa_0138-nro_sol LEFT DELETING LEADING '0'.
      SHIFT wa_0138-seq     LEFT DELETING LEADING '0'.
      SHIFT wa_0138-seq_cam LEFT DELETING LEADING '0'.

*-----CS2019001896 - 11.01.2021- inicio
      wa_saida-ordem = |{ wa_0138-nro_sol }/{ wa_0138-seq }-{ wa_0138-seq_cam }|.
      CONCATENATE wa_saida-ordem
                  wa_0138-filial_resp
             INTO wa_saida-ordem SEPARATED BY space.
*-----CS2019001896 - 11.01.2021- fim

      wa_saida-ordem_venda = wa_0082-vbeln.
      wa_saida-posnr      = wa_0082-posnr.

      TRY .
          DATA(wa_vbap) = it_vbap[ vbeln = wa_0082-vbeln
                                   posnr = wa_0082-posnr ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      wa_saida-descricao   = wa_vbap-arktx.

      wa_saida-volume = wa_0138-qtd_embarq.
      wa_saida-unidade = wa_0138-um.

      TRY .
          wa_lfa1 = it_lfa1[ lifnr = wa_0138-cod_transportadora ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      wa_saida-transportadora = wa_lfa1-name1.

      IF NOT wa_lfa1-stcd1 IS INITIAL.
        WRITE wa_lfa1-stcd1 USING EDIT MASK 'RR__.___.___/____-__'
              TO wa_saida-cnpj.
      ENDIF.

      TRY .
          wa_lfa1 = it_lfa1[ lifnr = wa_0138-motorista ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      wa_saida-nome_motorista  = wa_lfa1-name1.

      IF NOT wa_lfa1-stcd2 IS INITIAL.
        WRITE wa_lfa1-stcd2 USING EDIT MASK 'RR___.___.___-__'
              TO wa_saida-cpf.
      ENDIF.

      wa_saida-contato         = |{ wa_lfa1-telf1 } / { wa_lfa1-telf2 }|.

      wa_saida-placa_cavalo    = wa_0138-placa_cav.
      wa_saida-placa_carreta1 = wa_0138-placa_car1.
      wa_saida-placa_carreta2 = wa_0138-placa_car2.
      wa_saida-placa_carreta3 = wa_0138-placa_car3.
      wa_saida-valor_frete  = wa_0138-preco_frete.
      wa_saida-adiantamento = wa_0138-adiantamento.
      wa_saida-observacao   = wa_0138-observacao.
      wa_saida-roteiro      = wa_0082-nr_rot.
      wa_saida-exibe_frete  = frete.

      IF wa_0138-pto_col IS NOT INITIAL.
        SELECT SINGLE name1 INTO
           wa_saida-local_embarque
          FROM lfa1
          WHERE lifnr = wa_0138-pto_col.
      ENDIF.
      wa_saida-filial_resp = wa_0138-filial_resp.

      SELECT SINGLE ped_imp INTO wa_saida-ped_imp
        FROM zsdt0137
         WHERE nro_sol = wa_0138-nro_sol
           AND seq     = wa_0138-seq
           AND filial_resp = wa_0138-filial_resp.

      wl_name = wa_0082-nr_rot.

      FREE: tg_texto, tg_obser.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id        = 'ZROT'
          language  = sy-langu
          name      = wl_name
          object    = 'ZSDROTEIRO'
        TABLES
          lines     = tg_texto
        EXCEPTIONS
          id        = 1
          language  = 2
          name      = 3
          not_found = 4
          OTHERS    = 5.

*-----CS2019001891 - 08.03.2021 - JT - inicio
      CONCATENATE nro_sol seq_cam seq filial_resp
             INTO wl_name.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id        = 'Z001'
          language  = sy-langu
          name      = wl_name
          object    = 'ZOBSERVAC'
        TABLES
          lines     = tg_obser
        EXCEPTIONS
          id        = 1
          language  = 2
          name      = 3
          not_found = 4
          OTHERS    = 5.

*-----limita a 6 linhas
      DELETE tg_obser WHERE tdline = ''.

      LOOP AT tg_obser INTO wa_obser.
        IF sy-tabix > 5.
          DELETE tg_obser INDEX sy-tabix.
          CONTINUE.
        ENDIF.
        IF wa_saida-observacao IS INITIAL.
          wa_saida-observacao   = wa_obser-tdline.
        ENDIF.
      ENDLOOP.

*     IF tg_obser[] IS NOT INITIAL.
*       READ TABLE tg_obser INTO wa_obser INDEX 1.
*       wa_saida-observacao   = wa_obser-tdline.
*     ENDIF.
*-----CS2019001891 - 08.03.2021 - JT - fim

      APPEND wa_saida TO it_saida.

    ENDLOOP.

    IF wa_0082-nr_rot IS NOT INITIAL.
      SELECT SINGLE * INTO @DATA(ls_0132)
        FROM zsdt0132
        WHERE nr_rot = @wa_0082-nr_rot.
      IF sy-subrc = 0.
        CONCATENATE ls_0132-rot_desc ','
                    ls_0132-city1 ','
                    ls_0132-uf ','
                    ls_0132-tel_number INTO tg_texto_132-tdline SEPARATED BY space.
        APPEND tg_texto_132 TO tg_texto_132.
      ENDIF.

      LOOP AT tg_texto INTO tg_texto.
        APPEND tg_texto TO tg_texto_132.
      ENDLOOP.
      tg_texto[] = tg_texto_132[].
    ENDIF.
  ENDLOOP.

  vl_formname = 'ZSDF0004'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_formname
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  CALL FUNCTION vl_name
    TABLES
      it_embarque      = it_saida
      it_texto         = tg_texto
      it_obser         = tg_obser
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.

ENDFUNCTION.
