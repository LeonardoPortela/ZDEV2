class ZCL_FATURAMENTO definition
  public
  final
  create public .

public section.

  interfaces ZIF_FATURAMENTO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FATURAMENTO IMPLEMENTATION.


  method zif_faturamento~get_agente_frete.

    data: rgcnpj type range of stcd1.

    r_if_faturamento = me.

    try.
        me->zif_faturamento~get_tipo_veiculo( exporting i_placa = i_placa importing e_tipo = data(e_tipo) e_proprietario = data(e_proprietario) ).
      catch zcx_faturamento .
      catch zcx_error .
    endtry.

    if e_tipo ne zif_faturamento=>st_tp_prop_veiculo_proprio.
      raise exception type zcx_faturamento
        exporting
          textid = value #( msgid = zcx_faturamento=>zcx_veiculo_nao_proprio-msgid
                            msgno = zcx_faturamento=>zcx_veiculo_nao_proprio-msgno
                            attr1 = i_placa )
          msgid  = zcx_faturamento=>zcx_veiculo_nao_proprio-msgid
          msgno  = zcx_faturamento=>zcx_veiculo_nao_proprio-msgno
          msgty  = 'E'
          msgv1  = conv #( i_placa ).
    endif.

    rgcnpj = value #( sign = 'I' option = 'CP' ( low = e_proprietario-stcd1(8) && '*' ) ).

*-CS2022000236 - 25.02.2022 - JT - inicio
    if i_bukrs is initial.
*-CS2022000236 - 25.02.2022 - JT - fim
      select single * into @data(wa_lfa1)
        from lfa1
       where regio eq @i_uf_origem_mercadoria "Estado do Local de Coleta
         and ktokk eq 'ZFIC'
         and bahns ne @space
         and dlgrp eq '0001'
         and stcd1 in @rgcnpj.

      if sy-subrc is not initial.
        raise exception type zcx_faturamento
          exporting
            textid = value #( msgid = zcx_faturamento=>zcx_sem_agente_frete-msgid
                              msgno = zcx_faturamento=>zcx_sem_agente_frete-msgno
                              attr1 = i_uf_origem_mercadoria
                              attr2 = i_placa )
            msgid  = zcx_faturamento=>zcx_sem_agente_frete-msgid
            msgno  = zcx_faturamento=>zcx_sem_agente_frete-msgno
            msgv1  = conv #( i_uf_origem_mercadoria )
            msgv2  = conv #( i_placa )
            msgty  = 'E'.
      endif.

      e_agente_frete = wa_lfa1-lifnr.
    else.
*-CS2022000236 - 25.02.2022 - JT - inicio
      select * into @data(w_zlest0207)
        from zlest0207
          up to 1 rows
       where bukrs eq @i_bukrs
         and regio eq @i_uf_origem_mercadoria. "Estado do Local de Coleta
      endselect.

      if sy-subrc is not initial.
        raise exception type zcx_faturamento
          exporting
            textid = value #( msgid = zcx_faturamento=>zcx_sem_agente_frete-msgid
                              msgno = zcx_faturamento=>zcx_sem_agente_frete-msgno
                              attr1 = i_uf_origem_mercadoria
                              attr2 = i_placa )
            msgid  = zcx_faturamento=>zcx_sem_agente_frete-msgid
            msgno  = zcx_faturamento=>zcx_sem_agente_frete-msgno
            msgv1  = conv #( i_uf_origem_mercadoria )
            msgv2  = conv #( i_placa )
            msgty  = 'E'.
      endif.

                                                            "BUG 184244
      data v_tipo_agente(1).
      v_tipo_agente = i_tipo_agente.
      if e_tipo = 'P'. "VEICULO PROPRIO
        select single *
          from j_1bbranch
          into @data(w_j_1bbranch)
          where stcd1 = @e_proprietario-stcd1. "Empresa que pertence o proprietário
        if w_j_1bbranch-bukrs = i_bukrs and  v_tipo_agente ne 3.
          v_tipo_agente = '2'.
        endif.
      endif.
                                                            "BUG 184244
      case v_tipo_agente.
        when '1'.
          e_agente_frete = w_zlest0207-tdlnr.
        when '2'.
          e_agente_frete = w_zlest0207-tdlnr_frota.
        when '3'.
          e_agente_frete = w_zlest0207-tdlnr_sub.
        when others.
          e_agente_frete = w_zlest0207-tdlnr.
      endcase.
*-CS2022000236 - 25.02.2022 - JT - fim
    endif.

  endmethod.


  METHOD zif_faturamento~get_auth_document.

    DATA: l_docnum TYPE j_1bnfdoc-docnum.

    FREE: e_erro.

    l_docnum = i_docnum.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = l_docnum
      IMPORTING
        output = l_docnum.

    IF l_docnum IS INITIAL.
      e_erro = abap_true.
      EXIT.
    ENDIF.

    SELECT SINGLE *
      FROM j_1bnfe_active
      INTO @DATA(w_active_doc)
     WHERE docnum = @l_docnum.

    IF sy-subrc EQ 0 AND
       w_active_doc-docsta = '1' AND w_active_doc-scssta <> '2'
                                 AND w_active_doc-cancel <> abap_true.

      SELECT SINGLE *
        FROM j_1bnfdoc
        INTO @DATA(w_doc_ck)
       WHERE docnum = @l_docnum.

      IF w_doc_ck-candat IS INITIAL.
        e_erro = abap_false.
      ELSE.
        e_erro = abap_true.
      ENDIF.
    ELSE.
      e_erro = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_FATURAMENTO~GET_CK_AVERBA_SEGURO.

    R_IF_FATURAMENTO = ME.

    """""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT SINGLE * INTO @DATA(WA_J1_BNFDOC)
      FROM J_1BNFDOC
     WHERE DOCNUM EQ @I_DOCNUM.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_FATURAMENTO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_DOC_FISCAL-MSGID
                            MSGNO = ZCX_FATURAMENTO=>ZCX_DOC_FISCAL-MSGNO
                            ATTR1 = I_DOCNUM )
          MSGID  = ZCX_FATURAMENTO=>ZCX_DOC_FISCAL-MSGID
          MSGNO  = ZCX_FATURAMENTO=>ZCX_DOC_FISCAL-MSGNO
          MSGV1  = CONV #( I_DOCNUM )
          MSGTY  = 'E'.
    ENDIF.

    IF ME->ZIF_FATURAMENTO~AT_SEGURO_FRETE EQ ABAP_FALSE.
      RAISE EXCEPTION TYPE ZCX_FATURAMENTO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_PROC_SEGURO-MSGID
                            MSGNO = ZCX_FATURAMENTO=>ZCX_PROC_SEGURO-MSGNO )
          MSGID  = ZCX_FATURAMENTO=>ZCX_PROC_SEGURO-MSGID
          MSGNO  = ZCX_FATURAMENTO=>ZCX_PROC_SEGURO-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    CASE WA_J1_BNFDOC-MODEL.
      WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_NFE.

        CHECK NOT ( ME->ZIF_FATURAMENTO~AT_TIPO_VEICULO   = ZIF_FATURAMENTO=>ST_TP_PROP_VEICULO_PROPRIO AND
                    ME->ZIF_FATURAMENTO~AT_TIPO_REMETENTE = ZIF_FATURAMENTO=>ST_TP_REME_PROPRIO ).

        RAISE EXCEPTION TYPE ZCX_FATURAMENTO
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_PROC_SEGURO-MSGID
                              MSGNO = ZCX_FATURAMENTO=>ZCX_PROC_SEGURO-MSGNO )
            MSGID  = ZCX_FATURAMENTO=>ZCX_PROC_SEGURO-MSGID
            MSGNO  = ZCX_FATURAMENTO=>ZCX_PROC_SEGURO-MSGNO
            MSGTY  = 'E'.

      WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_CTE.

        CHECK ME->ZIF_FATURAMENTO~AT_CONHECIMENTO EQ ABAP_FALSE.

        RAISE EXCEPTION TYPE ZCX_FATURAMENTO
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_PROC_SEGURO-MSGID
                              MSGNO = ZCX_FATURAMENTO=>ZCX_PROC_SEGURO-MSGNO )
            MSGID  = ZCX_FATURAMENTO=>ZCX_PROC_SEGURO-MSGID
            MSGNO  = ZCX_FATURAMENTO=>ZCX_PROC_SEGURO-MSGNO
            MSGTY  = 'E'.

    ENDCASE.

    RAISE EXCEPTION TYPE ZCX_FATURAMENTO
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_PROC_SEGURO-MSGID
                          MSGNO = ZCX_FATURAMENTO=>ZCX_PROC_SEGURO-MSGNO )
        MSGID  = ZCX_FATURAMENTO=>ZCX_PROC_SEGURO-MSGID
        MSGNO  = ZCX_FATURAMENTO=>ZCX_PROC_SEGURO-MSGNO
        MSGTY  = 'E'.

  ENDMETHOD.


  METHOD ZIF_FATURAMENTO~GET_CK_EMISSAO_MDFE.

    R_IF_FATURAMENTO = ME.

    """""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT SINGLE * INTO @DATA(WA_J1_BNFDOC)
      FROM J_1BNFDOC
     WHERE DOCNUM EQ @I_DOCNUM.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_FATURAMENTO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_DOC_FISCAL-MSGID
                            MSGNO = ZCX_FATURAMENTO=>ZCX_DOC_FISCAL-MSGNO
                            ATTR1 = I_DOCNUM )
          MSGID  = ZCX_FATURAMENTO=>ZCX_DOC_FISCAL-MSGID
          MSGNO  = ZCX_FATURAMENTO=>ZCX_DOC_FISCAL-MSGNO
          MSGV1  = CONV #( I_DOCNUM )
          MSGTY  = 'E'.
    ENDIF.

    IF ME->ZIF_FATURAMENTO~AT_MANIFESTO EQ ABAP_FALSE.
      RAISE EXCEPTION TYPE ZCX_FATURAMENTO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_PROC_MDFE-MSGID
                            MSGNO = ZCX_FATURAMENTO=>ZCX_PROC_MDFE-MSGNO )
          MSGID  = ZCX_FATURAMENTO=>ZCX_PROC_MDFE-MSGID
          MSGNO  = ZCX_FATURAMENTO=>ZCX_PROC_MDFE-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    CASE WA_J1_BNFDOC-MODEL.
      WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_NFE.

        "Se Emite CT-e
        CHECK ME->ZIF_FATURAMENTO~AT_CONHECIMENTO EQ ABAP_TRUE.

        RAISE EXCEPTION TYPE ZCX_FATURAMENTO
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_PROC_MDFE_NFE-MSGID
                              MSGNO = ZCX_FATURAMENTO=>ZCX_PROC_MDFE_NFE-MSGNO )
            MSGID  = ZCX_FATURAMENTO=>ZCX_PROC_MDFE_NFE-MSGID
            MSGNO  = ZCX_FATURAMENTO=>ZCX_PROC_MDFE_NFE-MSGNO
            MSGTY  = 'E'.

      WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_CTE.

        CHECK ME->ZIF_FATURAMENTO~AT_CONHECIMENTO EQ ABAP_FALSE.

        RAISE EXCEPTION TYPE ZCX_FATURAMENTO
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_PROC_MDFE_CTE-MSGID
                              MSGNO = ZCX_FATURAMENTO=>ZCX_PROC_MDFE_CTE-MSGNO )
            MSGID  = ZCX_FATURAMENTO=>ZCX_PROC_MDFE_CTE-MSGID
            MSGNO  = ZCX_FATURAMENTO=>ZCX_PROC_MDFE_CTE-MSGNO
            MSGTY  = 'E'.

    ENDCASE.

    RAISE EXCEPTION TYPE ZCX_FATURAMENTO
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_PROC_MDFE-MSGID
                          MSGNO = ZCX_FATURAMENTO=>ZCX_PROC_MDFE-MSGNO )
        MSGID  = ZCX_FATURAMENTO=>ZCX_PROC_MDFE-MSGID
        MSGNO  = ZCX_FATURAMENTO=>ZCX_PROC_MDFE-MSGNO
        MSGTY  = 'E'.

  ENDMETHOD.


  METHOD zif_faturamento~get_data_url.

    DATA : l_url       TYPE string,
           l_content   TYPE xstring,
           http_client TYPE REF TO if_http_client.

    FREE: e_data,
          e_len.

    CHECK i_pula_check = abap_false.

    l_url =  i_filename.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = l_url
      IMPORTING
        client             = http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    IF sy-subrc = 0.
      http_client->send( ).
      http_client->receive( ).
      l_content  = http_client->response->get_data( ).
      http_client->close( ).
      e_data     = l_content.
      e_len      = xstrlen( l_content ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_faturamento~get_documentos_faturamento.

    DATA: l_imp_doc        TYPE j_1bdocnum,
          l_docnum         TYPE j_1bnfdoc-docnum,
          l_id_ordem       TYPE zde_id_ordem,
          l_nr_safra       TYPE zsdt0001od-nr_safra,
          l_nr_safra2      TYPE zpmt0054-safra,
          l_dados_selo     TYPE xstring,
          l_dados_romaneio TYPE xstring,
          l_pdf_romaneio   TYPE xstring,
          l_pdf_assinado   TYPE xstring,
          l_declaracao     TYPE xstring,
          l_url_mdfe       TYPE zsdt0102-url_sefaz,
          l_cd_ciot        TYPE zcte_ciot-cd_ciot,    "*-#130491-04.04.2024-JT-inicio
          l_url_contrato   TYPE zcte_ciot-link_contrato,
          l_url_cpedagio   TYPE zcte_ciot-link_carga_pedagio,
          l_url_pedag      TYPE zcte_ciot-link_pedagio, "FF #167355
          l_erro_rom       TYPE char01,
          w_url            TYPE zib_nfe,
          w_pdf_files      TYPE zsde_pdf_files,
          t_pdf_assina     TYPE zsdt_pdf_files,
          obj_romaneio     TYPE REF TO zcl_romaneio,
          obj_ciot         TYPE REF TO zcl_ciot.  "*-#130491-04.04.2024-JT

    FREE: t_pdf_files.

*--------------------------------------------------
*-- selecionar romaneio
*--------------------------------------------------
    SELECT *
      FROM zsdt0001
      INTO @DATA(w_zsdt0001)
        UP TO 1 ROWS
     WHERE ch_referencia = @i_ch_referencia.
    ENDSELECT.

    CHECK sy-subrc = 0.

*-----------------------------------------
*-- Danfe.
*-----------------------------------------
    IF w_zsdt0001-nro_nf_prod IS NOT INITIAL.

      l_imp_doc = w_zsdt0001-nro_nf_prod.

      IF zif_faturamento~get_auth_document( EXPORTING i_docnum = l_imp_doc ) = abap_false.
        FREE: w_url.

        SELECT SINGLE *
          INTO w_url
          FROM zib_nfe
         WHERE docnum        = l_imp_doc
           AND ds_url_danfe <> space.

        IF sy-subrc = 0 AND w_url-ds_url_danfe IS NOT INITIAL.
          FREE: w_pdf_files.
          w_pdf_files-filename  = w_url-ds_url_danfe.
          w_pdf_files-tipo_doc  = '01'.
          zif_faturamento~get_data_url( EXPORTING i_filename   = CONV #( w_url-ds_url_danfe )
                                                  i_pula_check = i_pula_check
                                        IMPORTING e_data       = w_pdf_files-data
                                                  e_len        = w_pdf_files-len ).
          APPEND w_pdf_files   TO t_pdf_files.
        ENDIF.
      ENDIF.
    ENDIF.

*-----------------------------------------
*-- Dacte
*-----------------------------------------
    IF w_zsdt0001-nro_nf_frete IS NOT INITIAL.

      l_imp_doc = w_zsdt0001-nro_nf_frete.

      IF zif_faturamento~get_auth_document( EXPORTING i_docnum = l_imp_doc ) = abap_false.
        FREE: w_url.

        SELECT SINGLE *
          INTO w_url
          FROM zib_nfe
         WHERE docnum        = l_imp_doc
           AND ds_url_danfe <> space.

        IF sy-subrc = 0 AND w_url-ds_url_danfe IS NOT INITIAL.
          FREE: w_pdf_files.
          w_pdf_files-filename  = w_url-ds_url_danfe.
          w_pdf_files-tipo_doc  = '02'.
          zif_faturamento~get_data_url( EXPORTING i_filename   = CONV #( w_url-ds_url_danfe )
                                                  i_pula_check = i_pula_check
                                        IMPORTING e_data       = w_pdf_files-data
                                                  e_len        = w_pdf_files-len ).
          APPEND w_pdf_files   TO t_pdf_files.
        ENDIF.

*------ Verificar Carta.
        IF i_pula_check = abap_false.
          FREE: l_declaracao.
          CALL FUNCTION 'Z_SD_PRINT_DECLARA'
            EXPORTING
              doc_numero   = l_imp_doc
              imprimir     = ''
              gera_pdf     = 'N'
            IMPORTING
              e_declaracao = l_declaracao.

          IF l_declaracao IS NOT INITIAL.
            FREE: w_pdf_files.
            w_pdf_files-filename  = 'Declaração do Motorista'.
            w_pdf_files-tipo_doc  = '02'.
            w_pdf_files-data      = l_declaracao.
            w_pdf_files-len       = xstrlen( l_declaracao ).
            APPEND w_pdf_files   TO t_pdf_files.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*-----------------------------------------
*-- MDFE
*-----------------------------------------
    FREE: l_url_mdfe.

    SELECT *
      INTO TABLE @DATA(it_zsdt0105)
      FROM zsdt0105
     WHERE docnum = @l_imp_doc.

    LOOP AT it_zsdt0105 INTO DATA(w_zsdt0105).
      FREE: l_url_mdfe.

      SELECT SINGLE url_sefaz
        INTO l_url_mdfe
        FROM zsdt0102
       WHERE docnum     = w_zsdt0105-docnum_ref
         AND autorizado = abap_true
         AND estornado  = abap_false
         AND cancel     = abap_false.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF l_url_mdfe IS NOT INITIAL.
      FREE: w_pdf_files.
      w_pdf_files-filename  = l_url_mdfe.
      w_pdf_files-tipo_doc  = '04'.
      zif_faturamento~get_data_url( EXPORTING i_filename   = CONV #( l_url_mdfe )
                                              i_pula_check = i_pula_check
                                    IMPORTING e_data       = w_pdf_files-data
                                              e_len        = w_pdf_files-len ).
      APPEND w_pdf_files   TO t_pdf_files.
    ENDIF.

*-----------------------------------------
*-- Contrato de Frete e Pedágio
*-----------------------------------------
    FREE: l_url_contrato,
          l_url_cpedagio.

*   IF i_pula_check = abap_false.  "*-#143386-11.07.2024-JT-inicio-comentado
    SELECT SINGLE    cd_ciot link_contrato  link_carga_pedagio link_pedagio  "FF #167355
             INTO (l_cd_ciot,l_url_contrato,l_url_cpedagio, l_url_pedag ) "FF #167355
             FROM zcte_ciot
            WHERE docnum = l_imp_doc.

    "TRANSLATE l_url_contrato TO LOWER CASE.

    IF l_url_contrato IS NOT INITIAL.
      IF i_pula_check = abap_false.  "*-#143386-11.07.2024-JT
        FREE: w_pdf_files.
        w_pdf_files-filename  = l_url_contrato.
        w_pdf_files-tipo_doc  = '03'.

*-#130491-04.04.2024-JT-inicio
        IF l_url_contrato(4) <> 'http'.
          CREATE OBJECT obj_ciot.
          w_pdf_files-data    = obj_ciot->get_pdf_arquivos_viagem( i_cd_ciot = l_cd_ciot i_tipoarq = 'CONTRATO' ).
          w_pdf_files-len     = xstrlen( w_pdf_files-data ).
        ELSE.
          zif_faturamento~get_data_url( EXPORTING i_filename   = CONV #( l_url_contrato )
                                                  i_pula_check = i_pula_check
                                        IMPORTING e_data       = w_pdf_files-data
                                                  e_len        = w_pdf_files-len ).
        ENDIF.
*-#130491-04.04.2024-JT-fim
        APPEND w_pdf_files   TO t_pdf_files.
*-#143386-11.07.2024-JT-inicio
      ELSE.
        FREE: w_pdf_files.
        w_pdf_files-filename  = l_url_contrato.
        w_pdf_files-tipo_doc  = '03'.
        APPEND w_pdf_files   TO t_pdf_files.
      ENDIF.
*-#143386-11.07.2024-JT-fim
    ENDIF.

"FF #167355 - inicio
    IF l_url_pedag IS NOT INITIAL.
      IF i_pula_check = abap_false.

        w_pdf_files-filename  = l_url_pedag.
        w_pdf_files-tipo_doc  = '10'. "Pedágio

        IF l_url_pedag(4) <> 'http'.
          CREATE OBJECT obj_ciot.
          w_pdf_files-data    = obj_ciot->get_pdf_arquivos_viagem( i_cd_ciot = l_cd_ciot i_tipoarq = 'PEDAGIO' ).
          w_pdf_files-len     = xstrlen( w_pdf_files-data ).
        ELSE.
          zif_faturamento~get_data_url( EXPORTING i_filename   = CONV #( l_url_pedag )
                                                  i_pula_check = i_pula_check
                                        IMPORTING e_data       = w_pdf_files-data
                                                  e_len        = w_pdf_files-len ).
        ENDIF.

        APPEND w_pdf_files   TO t_pdf_files.

      ELSE.
        FREE: w_pdf_files.
        w_pdf_files-filename  = l_url_pedag.
        w_pdf_files-tipo_doc  = '10'. "pedágio
        APPEND w_pdf_files   TO t_pdf_files.
      ENDIF.

    ENDIF.
"FF #167355 - fim



*   ENDIF.

*-----------------------------------------
*-- ROMANEIO
*-----------------------------------------
    CLEAR l_erro_rom.

    IF i_pula_check = abap_false.

      IF w_zsdt0001-id_interface NE zcl_romaneio=>interface_fert_sem_peso_opus AND
         w_zsdt0001-id_interface NE zcl_romaneio=>interface_defe_sem_peso_opus AND
         w_zsdt0001-id_interface NE zcl_romaneio=>interface_seme_sem_peso_opus.

        TRY .
            CREATE OBJECT obj_romaneio.
            obj_romaneio->set_registro( i_id_registro = w_zsdt0001-ch_referencia ).
            CHECK sy-subrc IS INITIAL.
            obj_romaneio->imprimir_prd( EXPORTING i_nao_imprimir = abap_true
                                        IMPORTING e_data_xtring  = l_dados_romaneio ).
          CATCH zcx_cadastro INTO DATA(lc_ex_cadastro).
            l_erro_rom = abap_true.
          CATCH zcx_romaneio INTO DATA(lc_ex_romaneio).
            l_erro_rom = abap_true.
        ENDTRY.

        IF l_erro_rom = abap_false.
          FREE: w_pdf_files.
          w_pdf_files-filename  = 'Romaneio de Saída'.
          w_pdf_files-tipo_doc  = '06'.
          w_pdf_files-data      =  l_dados_romaneio.
          w_pdf_files-len       =  xstrlen( l_dados_romaneio ).
          APPEND w_pdf_files   TO t_pdf_files.
        ENDIF.

        CLEAR: obj_romaneio.
      ENDIF.
    ELSE.
      FREE: w_pdf_files.
      w_pdf_files-filename  = 'Romaneio de Saída'.
      w_pdf_files-tipo_doc  = '06'.
      APPEND w_pdf_files   TO t_pdf_files.
    ENDIF.

*-----------------------------------------
*-- Selo
*-----------------------------------------
    IF i_pula_check = abap_false.
      l_docnum   = w_zsdt0001-nro_nf_prod.
      l_id_ordem = w_zsdt0001-id_ordem.

      IF l_id_ordem IS INITIAL.
        SELECT id_ordem
          INTO l_id_ordem
          FROM zsdt0001
            UP TO 1 ROWS
         WHERE ch_referencia = w_zsdt0001-ch_referencia
           AND tp_movimento  = w_zsdt0001-tp_movimento.
        ENDSELECT.
      ENDIF.

      SELECT nr_safra
        INTO l_nr_safra
        FROM zsdt0001od
          UP TO 1 ROWS
       WHERE id_ordem = l_id_ordem.
      ENDSELECT.

      l_nr_safra2 = l_nr_safra.

      CALL FUNCTION 'ZSD_IMPRIME_SELO'
        EXPORTING
          i_docnum                 = l_docnum
          i_safra                  = l_nr_safra2
          i_imprime_selo           = ' '
        IMPORTING
          e_xstring_document       = l_dados_selo
        EXCEPTIONS
          documento_nao_autorizado = 1
          documento_nao_imprimir   = 2
          OTHERS                   = 3.

      IF sy-subrc = 0 AND l_dados_selo IS NOT INITIAL.
        FREE: w_pdf_files.
        w_pdf_files-filename  = 'Impressão do Selo'.
        w_pdf_files-tipo_doc  = '05'.
        w_pdf_files-data      =  l_dados_selo.
        w_pdf_files-len       =  xstrlen( l_dados_selo ).
        APPEND w_pdf_files   TO t_pdf_files.
      ENDIF.
    ENDIF.

*-CS2021000218-16.11.2022-#90706-JT-inicio
*-----------------------------------------
*-- Solicitacao de Receita Agronomica Assinada
*-----------------------------------------
    IF i_pula_check = abap_false.
      TRY .
          zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
             )->get_pdf_documento_assinado( EXPORTING i_nro_cgd            = w_zsdt0001-nro_cg
                                                      i_ch_referencia      = w_zsdt0001-ch_referencia
                                            IMPORTING t_pdf_docto_assinado = t_pdf_assina ).

        CATCH zcx_integracao INTO DATA(ex_integra).
        CATCH zcx_error INTO DATA(ex_error).
      ENDTRY.

      IF t_pdf_assina[] IS NOT INITIAL.
        APPEND LINES OF t_pdf_assina[]  TO t_pdf_files[].
      ENDIF.
    ENDIF.
*-CS2021000218-16.11.2022-#90706-JT-fim

*-CS2021000218-26.05.2023-#108752-JT-inicio
*-----------------------------------------
*-- Relatorio Romaneio Algodao
*-----------------------------------------
    IF i_pula_check = abap_false.
      CALL FUNCTION 'ZSD_IMPRIME_REL_ROMANEIO'
        EXPORTING
          i_ch_referencia           = w_zsdt0001-ch_referencia
        IMPORTING
          e_xstring_document        = l_pdf_romaneio
        EXCEPTIONS
          romaneio_nao_encontrado   = 1
          romaneio_em_estorno       = 2
          erro_impressao_formulario = 3
          OTHERS                    = 4.

      IF sy-subrc = 0.
        FREE: w_pdf_files.
        w_pdf_files-filename  = 'Relatório Romaneio Algd'.
        w_pdf_files-tipo_doc  = '09'.
        w_pdf_files-data      =  l_pdf_romaneio.
        w_pdf_files-len       =  xstrlen( l_pdf_romaneio ).
        APPEND w_pdf_files   TO t_pdf_files.
      ENDIF.
    ENDIF.
*-CS2021000218-26.05.2023-#108752-JT-fim

  ENDMETHOD.


  METHOD zif_faturamento~get_documentos_obrigatorios.

    DATA: l_danfe            TYPE char01,
          l_dacte            TYPE char01,
          l_contrato         TYPE char01,
          l_mdfe             TYPE char01,
          l_romaneio         TYPE char01,
          t_pdf_files_new    TYPE zsdt_pdf_files,
          w_pdf_files_new    TYPE zsde_pdf_files,
          w_doctos_faltantes TYPE zsde_doctos_faltantes,
          merged_document    TYPE xstring.

    FREE:  l_danfe,
           l_dacte,
           l_contrato,
           l_mdfe,
           l_romaneio,
           t_pdf_files_new,
           t_doctos_faltantes,
           e_faltam_documentos.

*-----------------------------------------
* verifica documentos obrigatorios para envio
*-----------------------------------------
    TRY.
        zcl_faturamento=>zif_faturamento~get_instance(
          )->get_processo_emissao_docs(
          EXPORTING
            i_ch_romaneio      = i_ch_referencia
          IMPORTING
            e_nota_fiscal      = l_danfe
            e_conhecimento     = l_dacte
            e_pag_frete        = l_contrato
            e_manifesto        = l_mdfe ).

      CATCH zcx_faturamento INTO DATA(_zcx_fat).
      CATCH zcx_error       INTO DATA(_zcx_error).
    ENDTRY.

    SELECT SINGLE *
      from zsdt0001 INTO @DATA(lwa_zsdt0001)
     WHERE ch_referencia eq @i_ch_referencia.

    CHECK sy-subrc eq 0.

    if lwa_zsdt0001-id_interface ne zcl_romaneio=>interface_fert_sem_peso_opus and
       lwa_zsdt0001-id_interface ne zcl_romaneio=>interface_defe_sem_peso_opus and
       lwa_zsdt0001-id_interface ne zcl_romaneio=>interface_seme_sem_peso_opus.
      l_romaneio = abap_true.
    endif.

*-----------------------------------------
* obtem documentos faturamento
*-----------------------------------------
    IF t_pdf_files[] IS INITIAL.
      TRY.
          t_pdf_files_new = zcl_faturamento=>zif_faturamento~get_instance(
                              )->get_documentos_faturamento( EXPORTING i_ch_referencia  = i_ch_referencia
                                                                       i_pula_check     = abap_true
                              ).

        CATCH zcx_faturamento.
        CATCH zcx_error.
      ENDTRY.
    ELSE.
      t_pdf_files_new[] = t_pdf_files[].
    ENDIF.

*-----------------------------------------
* verifica arquivos corrompidos
*-----------------------------------------
    TRY.
        t_pdf_files_new = zcl_faturamento=>zif_faturamento~get_instance(
                            )->get_valida_merge_pdf( EXPORTING t_pdf_files = t_pdf_files_new
                            ).

      CATCH zcx_faturamento.
      CATCH zcx_error.
    ENDTRY.

*--------------------------------------------
* valida arquivos obrigatorios
*--------------------------------------------
    IF l_danfe = abap_true.
      READ TABLE t_pdf_files_new  INTO w_pdf_files_new WITH KEY tipo_doc = '01'.
      IF sy-subrc <> 0.
        e_faltam_documentos         = abap_true.
        w_doctos_faltantes-tipo_doc = '01'.
        w_doctos_faltantes-mensagem = 'Não foi encontrada DANFE'.
        APPEND w_doctos_faltantes  TO t_doctos_faltantes.
      ENDIF.
    ENDIF.

    IF l_dacte = abap_true.
      READ TABLE t_pdf_files_new  INTO w_pdf_files_new WITH KEY tipo_doc = '02'.
      IF sy-subrc <> 0.
        e_faltam_documentos         = abap_true.
        w_doctos_faltantes-tipo_doc = '02'.
        w_doctos_faltantes-mensagem = 'Não foi encontrada DACTE'.
        APPEND w_doctos_faltantes  TO t_doctos_faltantes.
      ENDIF.
    ENDIF.

    IF l_contrato = abap_true.
      READ TABLE t_pdf_files_new  INTO w_pdf_files_new WITH KEY tipo_doc = '03'.
      IF sy-subrc <> 0.
        e_faltam_documentos         = abap_true.
        w_doctos_faltantes-tipo_doc = '03'.
        w_doctos_faltantes-mensagem = 'Não foi encontrado o Contrato'.
        APPEND w_doctos_faltantes  TO t_doctos_faltantes.
      ENDIF.
    ENDIF.

    IF l_mdfe = abap_true.
      READ TABLE t_pdf_files_new  INTO w_pdf_files_new WITH KEY tipo_doc = '04'.
      IF sy-subrc <> 0.
        e_faltam_documentos         = abap_true.
        w_doctos_faltantes-tipo_doc = '04'.
        w_doctos_faltantes-mensagem = 'Não foi encontrado MDF-e'.
        APPEND w_doctos_faltantes  TO t_doctos_faltantes.
      ENDIF.
    ENDIF.

    IF l_romaneio = abap_true.
      READ TABLE t_pdf_files_new  INTO w_pdf_files_new WITH KEY tipo_doc = '06'.
      IF sy-subrc <> 0.
        e_faltam_documentos         = abap_true.
        w_doctos_faltantes-tipo_doc = '06'.
        w_doctos_faltantes-mensagem = 'Não foi encontrado Romaneio de Saída'.
        APPEND w_doctos_faltantes  TO t_doctos_faltantes.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_FATURAMENTO~GET_INSTANCE.

    IF ZIF_FATURAMENTO~AT_IF_FATURAMENTO IS NOT BOUND.
      CREATE OBJECT ZIF_FATURAMENTO~AT_IF_FATURAMENTO TYPE ZCL_FATURAMENTO.
    ENDIF.

    R_IF_FATURAMENTO = ZIF_FATURAMENTO~AT_IF_FATURAMENTO.

  ENDMETHOD.


  METHOD zif_faturamento~get_merge_pdf.

    DATA: pdf_merger    TYPE REF TO cl_rspo_pdf_merge,
          ex_pdf_merger TYPE REF TO cx_rspo_pdf_merge,
          l_ex_txt      TYPE string,
          l_rc          TYPE i VALUE 0,
          l_docindex    TYPE i VALUE 0,
          l_errordoc    TYPE xstring.

    FREE: e_merged_pdf.

*--------------------------------------------------
*-- valida se cada PDF esta ok
*--------------------------------------------------
    LOOP AT t_pdf_files INTO DATA(w_pdf).
      DATA(l_tabix) = sy-tabix.

      TRY.
          CREATE OBJECT pdf_merger.
        CATCH cx_rspo_pdf_merge INTO ex_pdf_merger.
          l_ex_txt = ex_pdf_merger->get_text( ).
          EXIT.
      ENDTRY.

      pdf_merger->add_document( w_pdf-data ).
      pdf_merger->merge_documents(     IMPORTING merged_document = e_merged_pdf
                                                 rc              = l_rc ).
      IF l_rc <> 0.
        DELETE t_pdf_files INDEX l_tabix.
      ENDIF.
    ENDLOOP.

*--------------------------------------------------
*-- unificador pdf
*--------------------------------------------------
    TRY.
        CREATE OBJECT pdf_merger.
      CATCH cx_rspo_pdf_merge INTO ex_pdf_merger.
        l_ex_txt = ex_pdf_merger->get_text( ).
        EXIT.
    ENDTRY.

*--------------------------------------------------
*-- Add documents to attribut table of PDF merger
*--------------------------------------------------
    LOOP AT t_pdf_files INTO DATA(w_pdf_files).
      pdf_merger->add_document( w_pdf_files-data ).
    ENDLOOP.

*--------------------------------------------------
*-- Call kernel method to do the merge of the specified files.
*--------------------------------------------------
    pdf_merger->merge_documents(     IMPORTING merged_document = e_merged_pdf
                                               rc              = l_rc ).

*--------------------------------------------------
*-- Get index of failed document
*--------------------------------------------------
    IF l_rc <> 0.
      pdf_merger->get_err_doc_index( IMPORTING index           = l_docindex ).
      pdf_merger->get_document(      EXPORTING index           = l_docindex
                                     IMPORTING document        = l_errordoc ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_faturamento~get_processo_emissao_docs.

    r_if_faturamento = me.

    CLEAR:
      e_tipo_veiculo,
      e_tipo_remetente,
      e_tp_frete,
      e_nota_fiscal,
      e_pedagio,
      e_doc_custo,
      e_doc_trans,
      e_conhecimento,
      e_pag_frete,
      e_manifesto,
      e_seguro_frete,
      e_uf_origem_mercadoria,
      e_placa_cavalo,
      me->zif_faturamento~at_tipo_veiculo,
      me->zif_faturamento~at_tipo_remetente,
      me->zif_faturamento~at_tp_frete,
      me->zif_faturamento~at_nota_fiscal,
      me->zif_faturamento~at_pedagio,
      me->zif_faturamento~at_doc_custo,
      me->zif_faturamento~at_doc_trans,
      me->zif_faturamento~at_conhecimento,
      me->zif_faturamento~at_pag_frete,
      me->zif_faturamento~at_manifesto,
      me->zif_faturamento~at_seguro_frete,
      me->zif_faturamento~at_romaneio,
      me->zif_faturamento~at_uf_origem_mercadoria,
      me->zif_faturamento~at_placa_cavalo.

    IF i_ch_romaneio IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(wa_zsdt0001)
        FROM zsdt0001
       WHERE ch_referencia EQ @i_ch_romaneio
         AND tp_movimento  EQ 'S'.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_faturamento
          EXPORTING
            textid = VALUE #( msgid = zcx_faturamento=>zcx_sem_romaneio-msgid
                              msgno = zcx_faturamento=>zcx_sem_romaneio-msgno )
            msgid  = zcx_faturamento=>zcx_sem_romaneio-msgid
            msgno  = zcx_faturamento=>zcx_sem_romaneio-msgno
            msgty  = 'E'.
      ENDIF.

      TRY.
          zcl_ordem_venda=>zif_ordem_venda~get_instance(
           )->set_ordem_venda( i_vbeln = CONV #( wa_zsdt0001-vbeln )
           )->get_ordem_venda( IMPORTING e_ordem_venda = DATA(wl_vbak)
           )->get_centro( IMPORTING e_parid_werks = DATA(v_parid_rom) ).

          IF v_parid_rom IS NOT INITIAL.
            wa_zsdt0001-parid = v_parid_rom.
          ENDIF.
        CATCH zcx_ordem_venda.
      ENDTRY.

      "Situações onde determinado o Parceiro WL(Remetente Mercadoria) no documento aviso/remessa vinculado a VT
      IF wa_zsdt0001-doc_transp IS NOT INITIAL.
        wa_zsdt0001-doc_transp = |{ wa_zsdt0001-doc_transp ALPHA = IN }|.

        TRY.
            me->zif_faturamento~get_romaneio_from_transporte( EXPORTING i_tknum    = CONV #( wa_zsdt0001-doc_transp )
                                                              IMPORTING e_zsdt0001 = DATA(_wl_zsdt0001_tmp) ).

            IF _wl_zsdt0001_tmp-parid <> wa_zsdt0001-parid.
              wa_zsdt0001 = _wl_zsdt0001_tmp.
            ENDIF.
          CATCH zcx_faturamento.
          CATCH zcx_error.
        ENDTRY.
      ENDIF.


    ELSEIF i_docnum IS NOT INITIAL.

      SELECT SINGLE *
        FROM j_1bnflin INTO @DATA(_wl_lin)
       WHERE docnum EQ @i_docnum.

      IF sy-subrc EQ 0.

        SELECT SINGLE *
          FROM j_1bnfdoc INTO @DATA(_wl_doc)
         WHERE docnum EQ @i_docnum.

        IF ( sy-subrc EQ 0 ).

          CASE _wl_lin-reftyp.
            WHEN 'LI'.

              IF ( _wl_doc-model EQ zif_doc_eletronico=>at_st_model_nfe ).

                e_nota_fiscal   = abap_true.
                e_doc_trans     = abap_false.
                e_doc_custo     = abap_false.
                e_manifesto     = abap_true.
                e_seguro_frete  = abap_false.
                e_pedagio       = abap_false.
                e_pag_frete     = abap_false.
                e_conhecimento  = abap_false.

                me->zif_faturamento~at_nota_fiscal          = e_nota_fiscal.
                me->zif_faturamento~at_doc_trans            = e_doc_trans.
                me->zif_faturamento~at_doc_custo            = e_doc_custo.
                me->zif_faturamento~at_manifesto            = e_manifesto.
                me->zif_faturamento~at_seguro_frete         = e_seguro_frete.
                me->zif_faturamento~at_pedagio              = e_pedagio.
                me->zif_faturamento~at_pag_frete            = e_pag_frete.
                me->zif_faturamento~at_conhecimento         = e_conhecimento.
                EXIT.

              ENDIF.

            WHEN 'ZW'.

              SELECT SINGLE *
                FROM zfiwrt0008 INTO @DATA(wl_zfiwrt0008)
               WHERE docnum EQ @i_docnum.

              IF sy-subrc EQ 0.
                e_tp_frete = wl_zfiwrt0008-inco1.
              ENDIF.

          ENDCASE.

        ENDIF.
      ENDIF.

      me->zif_faturamento~get_romaneio_from_fiscal( EXPORTING i_docnum = i_docnum IMPORTING e_zsdt0001 = wa_zsdt0001 e_tp_frete = DATA(e_tp_frete_rem) ).

    ELSEIF i_tknum IS NOT INITIAL.

      me->zif_faturamento~get_romaneio_from_transporte( EXPORTING i_tknum = i_tknum IMPORTING e_zsdt0001 = wa_zsdt0001 e_tp_frete = DATA(e_tp_frete_tknum) ).

    ELSE.
      RAISE EXCEPTION TYPE zcx_faturamento
        EXPORTING
          textid = VALUE #( msgid = zcx_faturamento=>zcx_erro_chamada_get_01-msgid
                            msgno = zcx_faturamento=>zcx_erro_chamada_get_01-msgno )
          msgid  = zcx_faturamento=>zcx_erro_chamada_get_01-msgid
          msgno  = zcx_faturamento=>zcx_erro_chamada_get_01-msgno
          msgty  = 'E'.
    ENDIF.

    IF wa_zsdt0001-placa_cav IS INITIAL AND i_tknum IS INITIAL.
      "003  Romaneio &1 sem placa de veículo de tração!
      RAISE EXCEPTION TYPE zcx_faturamento
        EXPORTING
          textid = VALUE #( msgid = zcx_faturamento=>zcx_rom_sem_cavalo-msgid
                            msgno = zcx_faturamento=>zcx_rom_sem_cavalo-msgno
                            attr1 = wa_zsdt0001-nr_romaneio )
          msgid  = zcx_faturamento=>zcx_rom_sem_cavalo-msgid
          msgno  = zcx_faturamento=>zcx_rom_sem_cavalo-msgno
          msgty  = 'E'
          msgv1  = CONV #( wa_zsdt0001-nr_romaneio ).
    ENDIF.

    """ Tipo do Veículo """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    e_placa_cavalo = wa_zsdt0001-placa_cav.

    TRY.
        me->zif_faturamento~get_tipo_veiculo(
          EXPORTING
            i_placa        = e_placa_cavalo
            i_tknum        = i_tknum
          IMPORTING
            e_tipo         = e_tipo_veiculo
            e_proprietario = DATA(e_proprietario) ).
      CATCH zcx_faturamento .
        e_tipo_veiculo = zif_faturamento=>st_tp_prop_veiculo_terceiro.
      CATCH zcx_error .
        e_tipo_veiculo = zif_faturamento=>st_tp_prop_veiculo_terceiro.
    ENDTRY.

    """ Tipo do Frete """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*   "// US-169528 25/07/2025 WBARBOSA INICIO
    IF i_ch_romaneio IS NOT INITIAL OR
       i_tknum IS NOT INITIAL.

      IF i_ch_romaneio IS NOT INITIAL.

        SELECT SINGLE *
          FROM zsdt0001 INTO @DATA(ls_zsdt0001)
         WHERE ch_referencia EQ @i_ch_romaneio
           AND tp_movimento  EQ 'S'
           AND id_interface  EQ '48'. "// Sementes

      ELSEIF i_tknum IS NOT INITIAL.

        SELECT SINGLE *
          FROM zsdt0001 INTO ls_zsdt0001
         WHERE doc_transp   EQ i_tknum
           AND tp_movimento EQ 'S'
           AND id_interface EQ '48'. "// Sementes

      ELSE.
        sy-subrc = 4.
      ENDIF.

      IF sy-subrc IS INITIAL.

        CALL METHOD zcl_carga_saida_insumos=>get_agente_frete
          EXPORTING
            i_bukrs  = ls_zsdt0001-bukrs
            i_branch = ls_zsdt0001-branch
            i_nro_cg = ls_zsdt0001-nro_cg
          IMPORTING
            e_inco1  = e_tp_frete.

      ENDIF.
    ENDIF.
*   "// US-169528 25/07/2025 WBARBOSA FIM

    IF e_tp_frete IS INITIAL.

      TRY.
          zcl_ordem_venda=>zif_ordem_venda~get_instance(
            )->set_ordem_venda( i_vbeln = wa_zsdt0001-vbeln
            )->get_tipo_frete( IMPORTING e_tipo_frete = e_tp_frete
            ).
        CATCH zcx_ordem_venda INTO DATA(ex_ordem).

          "Se não Achou a Ordem de Venda procura o Pedido de Compra
          IF zcx_ordem_venda=>zcx_ordem_venda_nao_existe-msgid EQ ex_ordem->msgid AND
             zcx_ordem_venda=>zcx_ordem_venda_nao_existe-msgno EQ ex_ordem->msgno.

            TRY .
                zcl_pedido_compra=>get_instance(
                  )->set_pedido( i_ebeln = wa_zsdt0001-vbeln
                  )->get_tipo_frete( IMPORTING e_tipo_frete = e_tp_frete
                  ).

                IF e_tp_frete_tknum IS NOT INITIAL.
                  e_tp_frete = e_tp_frete_tknum.
                ENDIF.

                IF e_tp_frete_rem IS NOT INITIAL.
                  e_tp_frete = e_tp_frete_rem.
                ENDIF.

              CATCH zcx_pedido_compra INTO DATA(ex_pedido_compra).
                RAISE EXCEPTION TYPE zcx_error
                  EXPORTING
                    textid = VALUE #( msgid  = ex_pedido_compra->msgid
                                      msgno  = ex_pedido_compra->msgno
                                      attr1  = CONV #( ex_pedido_compra->msgv1 )
                                      attr2  = CONV #( ex_pedido_compra->msgv2 )
                                      attr3  = CONV #( ex_pedido_compra->msgv3 )
                                      attr4  = CONV #( ex_pedido_compra->msgv4 ) )
                    msgty  = 'E'
                    msgid  = ex_pedido_compra->msgid
                    msgno  = ex_pedido_compra->msgno
                    msgv1  = ex_pedido_compra->msgv1
                    msgv2  = ex_pedido_compra->msgv2
                    msgv3  = ex_pedido_compra->msgv3
                    msgv4  = ex_pedido_compra->msgv4.
            ENDTRY.

          ELSE.
            RAISE EXCEPTION TYPE zcx_error
              EXPORTING
                textid = VALUE #( msgid  = ex_ordem->msgid
                                  msgno  = ex_ordem->msgno
                                  attr1  = CONV #( ex_ordem->msgv1 )
                                  attr2  = CONV #( ex_ordem->msgv2 )
                                  attr3  = CONV #( ex_ordem->msgv3 )
                                  attr4  = CONV #( ex_ordem->msgv4 ) )
                msgty  = 'E'
                msgid  = ex_ordem->msgid
                msgno  = ex_ordem->msgno
                msgv1  = ex_ordem->msgv1
                msgv2  = ex_ordem->msgv2
                msgv3  = ex_ordem->msgv3
                msgv4  = ex_ordem->msgv4.
          ENDIF.
      ENDTRY.

    ENDIF.

*    SELECT SINGLE * INTO @DATA(WA_ZLEST0002)
*      FROM ZLEST0002
*     WHERE PC_VEICULO EQ @WA_ZSDT0001-PLACA_CAV.
*
*    IF SY-SUBRC IS NOT INITIAL.
*      IF E_TP_FRETE NE ZIF_CARGA=>ST_TP_FRETE_FOB.
*        "004  Veículo &1 não cadastrado na ZLES0003!
*        RAISE EXCEPTION TYPE ZCX_FATURAMENTO
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_VEICULO_SEM_CAD-MSGID
*                              MSGNO = ZCX_FATURAMENTO=>ZCX_VEICULO_SEM_CAD-MSGNO
*                              ATTR1 = WA_ZSDT0001-PLACA_CAV )
*            MSGID  = ZCX_FATURAMENTO=>ZCX_VEICULO_SEM_CAD-MSGID
*            MSGNO  = ZCX_FATURAMENTO=>ZCX_VEICULO_SEM_CAD-MSGNO
*            MSGTY  = 'E'
*            MSGV1  = CONV #( WA_ZSDT0001-PLACA_CAV ).
*      ELSE.
*        E_TIPO_VEICULO = ZIF_FATURAMENTO=>ST_TP_PROP_VEICULO_TERCEIRO.
*      ENDIF.
*    ELSE.
*
*      SELECT SINGLE * INTO @DATA(WA_LIFNR_PROP)
*        FROM LFA1
*       WHERE LIFNR EQ @WA_ZLEST0002-PROPRIETARIO.
*
*      IF SY-SUBRC IS NOT INITIAL.
*        "005  Não encontrado o proprietário do veículo &1!
*        RAISE EXCEPTION TYPE ZCX_FATURAMENTO
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_VEICULO_PROPRIETARIO-MSGID
*                              MSGNO = ZCX_FATURAMENTO=>ZCX_VEICULO_PROPRIETARIO-MSGNO
*                              ATTR1 = WA_ZSDT0001-PLACA_CAV )
*            MSGID  = ZCX_FATURAMENTO=>ZCX_VEICULO_PROPRIETARIO-MSGID
*            MSGNO  = ZCX_FATURAMENTO=>ZCX_VEICULO_PROPRIETARIO-MSGNO
*            MSGTY  = 'E'
*            MSGV1  = CONV #( WA_ZSDT0001-PLACA_CAV ).
*      ENDIF.
*
*      IF WA_LIFNR_PROP-STKZN EQ ABAP_TRUE.
*        E_TIPO_VEICULO = ZIF_FATURAMENTO=>ST_TP_PROP_VEICULO_TERCEIRO.
*      ELSEIF WA_LIFNR_PROP-KTOKK NE 'ZFIC'.
*        E_TIPO_VEICULO = ZIF_FATURAMENTO=>ST_TP_PROP_VEICULO_TERCEIRO.
*      ELSE.
*        E_TIPO_VEICULO = ZIF_FATURAMENTO=>ST_TP_PROP_VEICULO_PROPRIO.
*      ENDIF.
*    ENDIF.

    """ Tipo do Remetente """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF wa_zsdt0001-tp_movimento EQ 'E'.

      SELECT SINGLE * INTO @DATA(wa_t0001)
        FROM t001w
       WHERE werks EQ @wa_zsdt0001-branch.

      IF sy-subrc IS INITIAL.
        DATA: p_parid TYPE lfa1-lifnr.
        p_parid = wa_t0001-j_1bbranch.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = p_parid
          IMPORTING
            output = p_parid.

        SELECT SINGLE * INTO @DATA(wa_lifnr_reme)
          FROM lfa1
         WHERE lifnr EQ @p_parid.
      ENDIF.

    ELSE.

      SELECT SINGLE * INTO @wa_lifnr_reme
        FROM lfa1
       WHERE lifnr EQ @wa_zsdt0001-parid.

    ENDIF.

    IF sy-subrc IS NOT INITIAL.
      "006  Não encontrado o Remetente da Mercadoria &1!
      RAISE EXCEPTION TYPE zcx_faturamento
        EXPORTING
          textid = VALUE #( msgid = zcx_faturamento=>zcx_remetente_forn-msgid
                            msgno = zcx_faturamento=>zcx_remetente_forn-msgno
                            attr1 = wa_zsdt0001-parid )
          msgid  = zcx_faturamento=>zcx_remetente_forn-msgid
          msgno  = zcx_faturamento=>zcx_remetente_forn-msgno
          msgty  = 'E'
          msgv1  = CONV #( wa_zsdt0001-parid ).
    ENDIF.

    e_uf_origem_mercadoria = wa_lifnr_reme-regio.

    IF wa_lifnr_reme-stkzn EQ abap_false AND e_proprietario-stkzn EQ abap_false.
      IF zcl_string=>length( text = CONV #( e_proprietario-stcd1 ) ) EQ 14.
        DATA(cl_raiz_prop) = e_proprietario-stcd1(8).
      ENDIF.
      IF zcl_string=>length( text = CONV #( wa_lifnr_reme-stcd1 ) ) EQ 14.
        DATA(cl_raiz_reme) = wa_lifnr_reme-stcd1(8).
      ENDIF.
    ELSE.
      CLEAR: cl_raiz_prop, cl_raiz_reme.
    ENDIF.

    "Tipo do Remetente em Relação ao Caminhão
    IF wa_lifnr_reme-stkzn EQ abap_true.

      "Amaggi não tem Pessoa Física
      e_tipo_remetente = zif_faturamento=>st_tp_reme_terceiro.

    ELSEIF wa_lifnr_reme-ktokk NE 'ZFIC'.

      "Se o Remente não for ZFIC não é empresa do Grupo
      e_tipo_remetente = zif_faturamento=>st_tp_reme_terceiro.

    ELSEIF wa_lifnr_reme-ktokk EQ 'ZFIC' AND e_proprietario-ktokk EQ 'ZFIC'.

      "Se os Dois São empresa do Grupo só pode Ser Próprio ou Intercompany
      IF cl_raiz_prop EQ cl_raiz_reme AND cl_raiz_reme IS NOT INITIAL.
        e_tipo_remetente = zif_faturamento=>st_tp_reme_proprio.
      ELSE.
        e_tipo_remetente = zif_faturamento=>st_tp_reme_intercompany.
      ENDIF.

    ELSE.

      "Senão é Sempre Terceiro
      e_tipo_remetente = zif_faturamento=>st_tp_reme_terceiro.

    ENDIF.


    IF e_tipo_veiculo EQ zif_faturamento=>st_tp_prop_veiculo_proprio.

      " Veículo Próprio, Frete CIF , CPT, FOB, CFR
      CASE e_tp_frete.
        WHEN space.

          e_nota_fiscal   = abap_true.
          e_doc_trans     = abap_true.
          e_doc_custo     = abap_true.
          e_manifesto     = abap_true.
          e_seguro_frete  = abap_true.
          e_pedagio       = abap_true.
          e_pag_frete     = abap_true.
          e_conhecimento  = abap_true.

        WHEN zif_carga=>st_tp_frete_fob OR zif_carga=>st_tp_frete_cfr.

          e_nota_fiscal   = abap_true.
          e_doc_trans     = abap_true.
          e_doc_custo     = abap_true.
          e_conhecimento  = abap_true.
          e_manifesto     = abap_true.
          e_seguro_frete  = abap_true.
          e_pedagio       = abap_true.
          e_pag_frete     = abap_false.

        WHEN zif_carga=>st_tp_frete_cif.

          e_nota_fiscal   = abap_true.
          e_doc_trans     = abap_true.
          e_doc_custo     = abap_true.
          e_manifesto     = abap_true.
          e_seguro_frete  = abap_true.
          e_pedagio       = abap_true.
          e_pag_frete     = abap_false.

          CASE e_tipo_remetente.
            WHEN zif_faturamento=>st_tp_reme_proprio.
              "Remetente Próprio
              e_conhecimento  = abap_false. " Não Emite Conhecimento
            WHEN zif_faturamento=>st_tp_reme_intercompany OR zif_faturamento=>st_tp_reme_terceiro.
              "Remetente Intercompany ou Terceiro
              e_conhecimento  = abap_true. " Emite Conhecimento
          ENDCASE.

        WHEN zif_carga=>st_tp_frete_cpt.

          e_nota_fiscal   = abap_true.
          e_doc_trans     = abap_true.
          e_doc_custo     = abap_true.
          e_manifesto     = abap_false.
          e_seguro_frete  = abap_false.
          e_pedagio       = abap_false.
          e_pag_frete     = abap_false.
          e_conhecimento  = abap_false.

      ENDCASE.

    ELSEIF e_tipo_veiculo EQ zif_faturamento=>st_tp_prop_veiculo_terceiro.

      CASE e_tp_frete.
        WHEN space.

          e_nota_fiscal   = abap_true.
          e_doc_trans     = abap_true.
          e_doc_custo     = abap_true.
          e_manifesto     = abap_true.
          e_seguro_frete  = abap_true.
          e_pedagio       = abap_true.
          e_pag_frete     = abap_true.
          e_conhecimento  = abap_true.

        WHEN zif_carga=>st_tp_frete_fob OR zif_carga=>st_tp_frete_cfr.

          e_nota_fiscal   = abap_true.
          e_doc_trans     = abap_false.
          e_doc_custo     = abap_false.
          e_conhecimento  = abap_false.
          e_manifesto     = abap_true.
          e_seguro_frete  = abap_false.
          e_pedagio       = abap_false.
          e_pag_frete     = abap_false.

        WHEN zif_carga=>st_tp_frete_cif.

          e_nota_fiscal   = abap_true.
          e_doc_trans     = abap_true.
          e_doc_custo     = abap_true.
          e_manifesto     = abap_true.
          e_seguro_frete  = abap_true.
          e_pedagio       = abap_true.
          e_pag_frete     = abap_true.
          e_conhecimento  = abap_true.

        WHEN zif_carga=>st_tp_frete_cpt.

          e_nota_fiscal   = abap_true.
          e_doc_trans     = abap_true.
          e_doc_custo     = abap_true.
          e_manifesto     = abap_false.
          e_seguro_frete  = abap_false.
          e_pedagio       = abap_false.
          e_pag_frete     = abap_false.
          e_conhecimento  = abap_false.

      ENDCASE.

    ENDIF.

    me->zif_faturamento~at_tipo_veiculo         = e_tipo_veiculo.
    me->zif_faturamento~at_tipo_remetente       = e_tipo_remetente.
    me->zif_faturamento~at_tp_frete             = e_tp_frete.
    me->zif_faturamento~at_nota_fiscal          = e_nota_fiscal.
    me->zif_faturamento~at_pedagio              = e_pedagio.
    me->zif_faturamento~at_doc_custo            = e_doc_custo.
    me->zif_faturamento~at_doc_trans            = e_doc_trans.
    me->zif_faturamento~at_conhecimento         = e_conhecimento.
    me->zif_faturamento~at_pag_frete            = e_pag_frete.
    me->zif_faturamento~at_manifesto            = e_manifesto.
    me->zif_faturamento~at_seguro_frete         = e_seguro_frete.
    me->zif_faturamento~at_romaneio             = wa_zsdt0001.
    me->zif_faturamento~at_uf_origem_mercadoria = e_uf_origem_mercadoria.
    me->zif_faturamento~at_placa_cavalo         = e_placa_cavalo.

  ENDMETHOD.


  METHOD ZIF_FATURAMENTO~GET_ROMANEIO_FROM_FISCAL.

    DATA: IT_ITENS_NFS TYPE TABLE OF J_1BNFLIN,
          WL_ITENS_NF  TYPE J_1BNFLIN,

          IT_EKES      TYPE TABLE OF EKES,
          WL_EKES      TYPE EKES.

    DATA: V_XBLNR          TYPE EKES-XBLNR,
          V_NFE            TYPE C LENGTH 9,
          V_SERIE          TYPE C LENGTH 3,
          V_COUNT_AVISO    TYPE I.

    R_IF_FATURAMENTO = ME.

    CLEAR: E_ZSDT0001, E_TP_FRETE.

    """""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT SINGLE * INTO @DATA(WA_J1_BNFDOC)
      FROM J_1BNFDOC
     WHERE DOCNUM EQ @I_DOCNUM.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_FATURAMENTO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_DOC_FISCAL-MSGID
                            MSGNO = ZCX_FATURAMENTO=>ZCX_DOC_FISCAL-MSGNO
                            ATTR1 = I_DOCNUM )
          MSGID  = ZCX_FATURAMENTO=>ZCX_DOC_FISCAL-MSGID
          MSGNO  = ZCX_FATURAMENTO=>ZCX_DOC_FISCAL-MSGNO
          MSGV1  = CONV #( I_DOCNUM )
          MSGTY  = 'E'.
    ENDIF.

    SELECT SINGLE * INTO @DATA(WA_J_1BNFLIN)
      FROM J_1BNFLIN
     WHERE DOCNUM EQ @I_DOCNUM.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_FATURAMENTO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_DOC_FISCAL-MSGID
                            MSGNO = ZCX_FATURAMENTO=>ZCX_DOC_FISCAL-MSGNO
                            ATTR1 = I_DOCNUM )
          MSGID  = ZCX_FATURAMENTO=>ZCX_DOC_FISCAL-MSGID
          MSGNO  = ZCX_FATURAMENTO=>ZCX_DOC_FISCAL-MSGNO
          MSGV1  = CONV #( I_DOCNUM )
          MSGTY  = 'E'.
    ENDIF.

    CASE WA_J1_BNFDOC-MODEL.
      WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_NFE.

        CASE WA_J_1BNFLIN-REFTYP.
          WHEN 'MD'.

            "Buscar Documento Material
            SELECT SINGLE * INTO @DATA(WA_MKPF)
              FROM MKPF
             WHERE MBLNR EQ @WA_J_1BNFLIN-REFKEY(10)
               AND MJAHR EQ @WA_J_1BNFLIN-REFKEY+10(4).

            IF SY-SUBRC IS NOT INITIAL.
              "Erro Doc. Material
              RAISE EXCEPTION TYPE ZCX_FATURAMENTO
                EXPORTING
                  TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_SEM_DOC_MATERIAL-MSGID
                                    MSGNO = ZCX_FATURAMENTO=>ZCX_SEM_DOC_MATERIAL-MSGNO )
                  MSGID  = ZCX_FATURAMENTO=>ZCX_SEM_DOC_MATERIAL-MSGID
                  MSGNO  = ZCX_FATURAMENTO=>ZCX_SEM_DOC_MATERIAL-MSGNO
                  MSGTY  = 'E'.
            ENDIF.

            DATA(LC_REMESSA) = WA_MKPF-LE_VBELN.

          WHEN 'BI'.
            "Buscar Fatura
            SELECT SINGLE * INTO @DATA(WA_VBRP)
              FROM VBRP
             WHERE VBELN EQ @WA_J_1BNFLIN-REFKEY
               AND POSNR EQ @WA_J_1BNFLIN-REFITM.

            IF SY-SUBRC IS NOT INITIAL.
              "Erro Fatura
              RAISE EXCEPTION TYPE ZCX_FATURAMENTO
                EXPORTING
                  TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_SEM_DOC_FATURA-MSGID
                                    MSGNO = ZCX_FATURAMENTO=>ZCX_SEM_DOC_FATURA-MSGNO )
                  MSGID  = ZCX_FATURAMENTO=>ZCX_SEM_DOC_FATURA-MSGID
                  MSGNO  = ZCX_FATURAMENTO=>ZCX_SEM_DOC_FATURA-MSGNO
                  MSGTY  = 'E'.
            ENDIF.

            LC_REMESSA = WA_VBRP-VGBEL.

          WHEN 'ZW'.

            CLEAR: V_NFE, V_SERIE, V_XBLNR.

            IF WA_J1_BNFDOC-NFE = 'X'.
              V_NFE = |{ WA_J1_BNFDOC-NFENUM ALPHA = OUT }|.
            ELSE.
              V_NFE = |{ WA_J1_BNFDOC-NFNUM  ALPHA = OUT }|.
            ENDIF.

            V_SERIE = |{ WA_J1_BNFDOC-SERIES ALPHA = OUT }|.

            CONCATENATE V_NFE '-' V_SERIE INTO V_XBLNR.

            SELECT SINGLE *
              FROM ZFIWRT0008 INTO @DATA(_WL_ZFIWRT0008)
             WHERE SEQ_LCTO EQ @WA_J_1BNFLIN-REFKEY(10).

            IF SY-SUBRC IS NOT INITIAL.
              "Erro Doc. ZNFW
              RAISE EXCEPTION TYPE ZCX_FATURAMENTO
                EXPORTING
                  TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_SEM_DOC_ZNFW-MSGID
                                    MSGNO = ZCX_FATURAMENTO=>ZCX_SEM_DOC_ZNFW-MSGNO )
                  MSGID  = ZCX_FATURAMENTO=>ZCX_SEM_DOC_ZNFW-MSGID
                  MSGNO  = ZCX_FATURAMENTO=>ZCX_SEM_DOC_ZNFW-MSGNO
                  MSGTY  = 'E'.
            ENDIF.

            SELECT SINGLE *
              FROM ZFIWRT0019 INTO @DATA(_WL_ZFIWRT0019)
             WHERE SEQ_LCTO EQ @_WL_ZFIWRT0008-SEQ_LCTO.

            IF ( SY-SUBRC IS NOT INITIAL ) OR ( _WL_ZFIWRT0019-PLACA IS INITIAL ).
              RAISE EXCEPTION TYPE ZCX_FATURAMENTO
                EXPORTING
                  TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_DOC_ZNFW_SEM_TRANSP-MSGID
                                    MSGNO = ZCX_FATURAMENTO=>ZCX_DOC_ZNFW_SEM_TRANSP-MSGNO
                                    ATTR1 = CONV #( _WL_ZFIWRT0008-SEQ_LCTO )
                                   )
                  MSGID  = ZCX_FATURAMENTO=>ZCX_DOC_ZNFW_SEM_TRANSP-MSGID
                  MSGNO  = ZCX_FATURAMENTO=>ZCX_DOC_ZNFW_SEM_TRANSP-MSGNO
                  MSGV1  = CONV #( _WL_ZFIWRT0008-SEQ_LCTO )
                  MSGTY  = 'E'.
            ENDIF.

            SELECT SINGLE *
              FROM ZFIWRT0015 INTO @DATA(_WL_ZFIWRT0015_WL)
             WHERE SEQ_LCTO EQ @_WL_ZFIWRT0008-SEQ_LCTO
               AND PARVW    EQ 'WL'.

            IF SY-SUBRC IS NOT INITIAL.
              RAISE EXCEPTION TYPE ZCX_FATURAMENTO
                EXPORTING
                  TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_DOC_ZNFW_SEM_PARC_WL-MSGID
                                    MSGNO = ZCX_FATURAMENTO=>ZCX_DOC_ZNFW_SEM_PARC_WL-MSGNO
                                    ATTR1 = CONV #( _WL_ZFIWRT0008-SEQ_LCTO )
                                   )
                  MSGID  = ZCX_FATURAMENTO=>ZCX_DOC_ZNFW_SEM_PARC_WL-MSGID
                  MSGNO  = ZCX_FATURAMENTO=>ZCX_DOC_ZNFW_SEM_PARC_WL-MSGNO
                  MSGV1  = CONV #( _WL_ZFIWRT0008-SEQ_LCTO )
                  MSGTY  = 'E'.
            ENDIF.

            SELECT SINGLE *
              FROM ZSDT0001 INTO @DATA(_WL_0001)
             WHERE SEQ_LCTO = @_WL_ZFIWRT0008-SEQ_LCTO.

            IF SY-SUBRC EQ 0.

              IF _WL_ZFIWRT0008-EBELN IS INITIAL.
                "Erro Doc. ZNFW
                RAISE EXCEPTION TYPE ZCX_FATURAMENTO
                  EXPORTING
                    TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_DOC_ZNFW_SEM_PED-MSGID
                                      MSGNO = ZCX_FATURAMENTO=>ZCX_DOC_ZNFW_SEM_PED-MSGNO
                                      ATTR1 = CONV #( _WL_ZFIWRT0008-SEQ_LCTO )
                                      )
                    MSGID  = ZCX_FATURAMENTO=>ZCX_DOC_ZNFW_SEM_PED-MSGID
                    MSGNO  = ZCX_FATURAMENTO=>ZCX_DOC_ZNFW_SEM_PED-MSGNO
                    MSGV1  = CONV #( _WL_ZFIWRT0008-SEQ_LCTO )
                    MSGTY  = 'E'.
              ENDIF.

              V_COUNT_AVISO = 0.

              CLEAR: IT_EKES[].
              SELECT *
                FROM EKES INTO TABLE IT_EKES
               WHERE EBELN EQ _WL_ZFIWRT0008-EBELN.

              LOOP AT IT_EKES INTO WL_EKES WHERE XBLNR EQ V_XBLNR.
                ADD 1 TO V_COUNT_AVISO.
                LC_REMESSA = WL_EKES-VBELN.
              ENDLOOP.

              IF V_COUNT_AVISO > 1.

                RAISE EXCEPTION TYPE ZCX_FATURAMENTO
                  EXPORTING
                    TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_AVISOS_MESMA_NF-MSGID
                                      MSGNO = ZCX_FATURAMENTO=>ZCX_AVISOS_MESMA_NF-MSGNO
                                      ATTR1 = CONV #( V_XBLNR )
                                      ATTR2 = CONV #( _WL_ZFIWRT0008-EBELN )
                                      )
                    MSGID  = ZCX_FATURAMENTO=>ZCX_AVISOS_MESMA_NF-MSGID
                    MSGNO  = ZCX_FATURAMENTO=>ZCX_AVISOS_MESMA_NF-MSGNO
                    MSGV1  = CONV #( V_XBLNR )
                    MSGV2  = CONV #( _WL_ZFIWRT0008-EBELN )
                    MSGTY  = 'E'.

              ENDIF.

            ENDIF.

            "Definição Dados Romaneio.
            E_ZSDT0001-PLACA_CAV    = _WL_ZFIWRT0019-PLACA.
            E_ZSDT0001-PLACA_CAR1   = _WL_ZFIWRT0019-PLACA_CAR1.
            E_ZSDT0001-PLACA_CAR2   = _WL_ZFIWRT0019-PLACA_CAR2.
            E_ZSDT0001-PLACA_CAR3   = _WL_ZFIWRT0019-PLACA_CAR3.
            E_ZSDT0001-MOTORISTA    = _WL_ZFIWRT0019-MOTORISTA.
            E_ZSDT0001-VBELN        = _WL_ZFIWRT0008-EBELN.
            E_ZSDT0001-NR_ROMANEIO  = '999999999'.
            E_ZSDT0001-PARID        = _WL_ZFIWRT0015_WL-PARID.
            EXIT.

        ENDCASE.

        "Não Achou a Remessa/Aviso Recebimento
        IF LC_REMESSA IS INITIAL.
          "Erro Remessa
          RAISE EXCEPTION TYPE ZCX_FATURAMENTO
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_SEM_DOC_REMESSA-MSGID
                                MSGNO = ZCX_FATURAMENTO=>ZCX_SEM_DOC_REMESSA-MSGNO )
              MSGID  = ZCX_FATURAMENTO=>ZCX_SEM_DOC_REMESSA-MSGID
              MSGNO  = ZCX_FATURAMENTO=>ZCX_SEM_DOC_REMESSA-MSGNO
              MSGTY  = 'E'.
        ENDIF.

        "Buscar Remessa
        SELECT SINGLE * INTO @DATA(WA_LIKP)
          FROM LIKP
         WHERE VBELN EQ @LC_REMESSA.

        IF SY-SUBRC IS NOT INITIAL.
          "Erro Remessa
          RAISE EXCEPTION TYPE ZCX_FATURAMENTO
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_SEM_DOC_REMESSA-MSGID
                                MSGNO = ZCX_FATURAMENTO=>ZCX_SEM_DOC_REMESSA-MSGNO )
              MSGID  = ZCX_FATURAMENTO=>ZCX_SEM_DOC_REMESSA-MSGID
              MSGNO  = ZCX_FATURAMENTO=>ZCX_SEM_DOC_REMESSA-MSGNO
              MSGTY  = 'E'.
        ENDIF.

        SY-SUBRC = 1.

        "Buscar Romaneio de Saída
        IF WA_LIKP-XBLNR IS NOT INITIAL.
          SELECT SINGLE * INTO @E_ZSDT0001
            FROM ZSDT0001
           WHERE CH_REFERENCIA EQ @WA_LIKP-XBLNR
             AND TP_MOVIMENTO  EQ 'S'.
        ENDIF.

        IF SY-SUBRC IS NOT INITIAL.
          SELECT SINGLE * INTO @E_ZSDT0001
            FROM ZSDT0001
           WHERE DOC_REM      EQ @WA_LIKP-VBELN
             AND TP_MOVIMENTO EQ 'S'.
        ENDIF.

        IF SY-SUBRC IS NOT INITIAL.
          SELECT SINGLE * INTO @E_ZSDT0001
            FROM ZSDT0001
           WHERE DOC_AVISO      EQ @WA_LIKP-VBELN
             AND TP_MOVIMENTO EQ 'S'.
        ENDIF.

        "Busca VT e tentar recuperar Romaneio pela VT
        IF SY-SUBRC IS NOT INITIAL.
          IF WA_LIKP-VBELN IS NOT INITIAL.

            SELECT SINGLE *
              FROM VTTP INTO @DATA(_WL_VTTP)
             WHERE VBELN EQ @WA_LIKP-VBELN.

            IF SY-SUBRC IS NOT INITIAL..
              RAISE EXCEPTION TYPE ZCX_FATURAMENTO
                EXPORTING
                  TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_SEM_DOC_TRANSPORTE-MSGID
                                    MSGNO = ZCX_FATURAMENTO=>ZCX_SEM_DOC_TRANSPORTE-MSGNO )
                  MSGID  = ZCX_FATURAMENTO=>ZCX_SEM_DOC_TRANSPORTE-MSGID
                  MSGNO  = ZCX_FATURAMENTO=>ZCX_SEM_DOC_TRANSPORTE-MSGNO
                  MSGTY  = 'E'.
            ENDIF.

            ME->ZIF_FATURAMENTO~GET_ROMANEIO_FROM_TRANSPORTE( EXPORTING I_TKNUM = CONV #( _WL_VTTP-TKNUM ) IMPORTING E_ZSDT0001 = E_ZSDT0001 E_TP_FRETE = E_TP_FRETE ).
          ENDIF.
        ENDIF.

        IF WA_J1_BNFDOC-FORM IS NOT INITIAL.
          E_ZSDT0001-PARID = |{ WA_J1_BNFDOC-BRANCH ALPHA = IN }| .
        ENDIF.

        IF E_ZSDT0001 IS INITIAL.
          "Erro Romaneio de Saída
          RAISE EXCEPTION TYPE ZCX_FATURAMENTO
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_SEM_ROMANEIO_SAIDA-MSGID
                                MSGNO = ZCX_FATURAMENTO=>ZCX_SEM_ROMANEIO_SAIDA-MSGNO )
              MSGID  = ZCX_FATURAMENTO=>ZCX_SEM_ROMANEIO_SAIDA-MSGID
              MSGNO  = ZCX_FATURAMENTO=>ZCX_SEM_ROMANEIO_SAIDA-MSGNO
              MSGTY  = 'E'.
        ENDIF.

      WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_CTE.

        "Fatura de Serviço
        SELECT SINGLE * INTO @WA_VBRP
          FROM VBRP
         WHERE VBELN EQ @WA_J_1BNFLIN-REFKEY(10)
           AND POSNR EQ @WA_J_1BNFLIN-REFITM.

        IF SY-SUBRC IS NOT INITIAL.
          "Erro Fatura de Serviço
          RAISE EXCEPTION TYPE ZCX_FATURAMENTO
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_SEM_DOC_FATURA-MSGID
                                MSGNO = ZCX_FATURAMENTO=>ZCX_SEM_DOC_FATURA-MSGNO )
              MSGID  = ZCX_FATURAMENTO=>ZCX_SEM_DOC_FATURA-MSGID
              MSGNO  = ZCX_FATURAMENTO=>ZCX_SEM_DOC_FATURA-MSGNO
              MSGTY  = 'E'.
        ENDIF.

        "Ordem de Venda
        SELECT SINGLE * INTO @DATA(WA_VBAK)
          FROM VBAK
         WHERE VBELN EQ @WA_VBRP-AUBEL.

        IF SY-SUBRC IS NOT INITIAL OR WA_VBAK-TKNUM IS INITIAL.
          "Erro Ordem de Venda
          RAISE EXCEPTION TYPE ZCX_FATURAMENTO
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_SEM_ORDEM_VENDA_FRETE-MSGID
                                MSGNO = ZCX_FATURAMENTO=>ZCX_SEM_ORDEM_VENDA_FRETE-MSGNO )
              MSGID  = ZCX_FATURAMENTO=>ZCX_SEM_ORDEM_VENDA_FRETE-MSGID
              MSGNO  = ZCX_FATURAMENTO=>ZCX_SEM_ORDEM_VENDA_FRETE-MSGNO
              MSGTY  = 'E'.
        ENDIF.

        "Documento de Transporte
        SELECT SINGLE * INTO @DATA(WA_VTTK)
          FROM VTTK
         WHERE TKNUM EQ @WA_VBAK-TKNUM.

        IF SY-SUBRC IS NOT INITIAL.
          "Erro Documento de Transporte
          RAISE EXCEPTION TYPE ZCX_FATURAMENTO
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_SEM_DOC_TRANSPORTE-MSGID
                                MSGNO = ZCX_FATURAMENTO=>ZCX_SEM_DOC_TRANSPORTE-MSGNO )
              MSGID  = ZCX_FATURAMENTO=>ZCX_SEM_DOC_TRANSPORTE-MSGID
              MSGNO  = ZCX_FATURAMENTO=>ZCX_SEM_DOC_TRANSPORTE-MSGNO
              MSGTY  = 'E'.
        ENDIF.

        CALL FUNCTION 'Z_LES_NOTAS_VT'
          EXPORTING
            I_VTTK       = WA_VTTK
          TABLES
            IT_ITEM_NOTA = IT_ITENS_NFS.

        CLEAR: WL_ITENS_NF.
        READ TABLE IT_ITENS_NFS INDEX 1 INTO WL_ITENS_NF.

        IF ( IT_ITENS_NFS[] IS INITIAL ) OR ( WL_ITENS_NF-REFTYP EQ 'LI' ).
          "Sem Nota Fiscal p/ CT-e
*          RAISE EXCEPTION TYPE ZCX_FATURAMENTO
*            EXPORTING
*              TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_SEM_NOTA_CTE-MSGID
*                                MSGNO = ZCX_FATURAMENTO=>ZCX_SEM_NOTA_CTE-MSGNO )
*              MSGID  = ZCX_FATURAMENTO=>ZCX_SEM_NOTA_CTE-MSGID
*              MSGNO  = ZCX_FATURAMENTO=>ZCX_SEM_NOTA_CTE-MSGNO
*              MSGTY  = 'E'.
          ME->ZIF_FATURAMENTO~GET_ROMANEIO_FROM_TRANSPORTE( EXPORTING I_TKNUM = WA_VTTK-TKNUM IMPORTING E_ZSDT0001 = E_ZSDT0001 E_TP_FRETE = E_TP_FRETE ).
        ELSE.
          READ TABLE IT_ITENS_NFS INDEX 1 INTO DATA(WA_ITENS_NFS).

          TRY .

              ME->ZIF_FATURAMENTO~GET_ROMANEIO_FROM_FISCAL(
                EXPORTING
                  I_DOCNUM         = WA_ITENS_NFS-DOCNUM   " Nº documento
                IMPORTING
                  E_ZSDT0001       = E_ZSDT0001
              ).
            CATCH ZCX_FATURAMENTO.
              ME->ZIF_FATURAMENTO~GET_ROMANEIO_FROM_TRANSPORTE( EXPORTING I_TKNUM = WA_VTTK-TKNUM IMPORTING E_ZSDT0001 = E_ZSDT0001 ).
            CATCH ZCX_ERROR.
              ME->ZIF_FATURAMENTO~GET_ROMANEIO_FROM_TRANSPORTE( EXPORTING I_TKNUM = WA_VTTK-TKNUM IMPORTING E_ZSDT0001 = E_ZSDT0001 ).
          ENDTRY.
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_faturamento~get_romaneio_from_transporte.

    DATA: wl_vbak TYPE vbak.

    CLEAR: e_tp_frete.

    r_if_faturamento = me.

    CLEAR: e_zsdt0001.

    SELECT SINGLE *
      FROM vttk INTO @DATA(_wl_vttk)
     WHERE tknum EQ @i_tknum.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_faturamento
        EXPORTING
          textid = VALUE #( msgid = zcx_faturamento=>zcx_sem_doc_transporte-msgid
                            msgno = zcx_faturamento=>zcx_sem_doc_transporte-msgno )
          msgid  = zcx_faturamento=>zcx_sem_doc_transporte-msgid
          msgno  = zcx_faturamento=>zcx_sem_doc_transporte-msgno
          msgty  = 'E'.
    ENDIF.

    SELECT SINGLE *
      FROM vttp INTO @DATA(_wl_vttp)
     WHERE tknum EQ @i_tknum.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_faturamento
        EXPORTING
          textid = VALUE #( msgid = zcx_faturamento=>zcx_sem_doc_transporte-msgid
                            msgno = zcx_faturamento=>zcx_sem_doc_transporte-msgno )
          msgid  = zcx_faturamento=>zcx_sem_doc_transporte-msgid
          msgno  = zcx_faturamento=>zcx_sem_doc_transporte-msgno
          msgty  = 'E'.
    ENDIF.

    SELECT SINGLE *
      FROM likp INTO @DATA(_wl_likp)
     WHERE vbeln EQ @_wl_vttp-vbeln.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_faturamento
        EXPORTING
          textid = VALUE #( msgid = zcx_faturamento=>zcx_sem_doc_remessa-msgid
                            msgno = zcx_faturamento=>zcx_sem_doc_remessa-msgno )
          msgid  = zcx_faturamento=>zcx_sem_doc_remessa-msgid
          msgno  = zcx_faturamento=>zcx_sem_doc_remessa-msgno
          msgty  = 'E'.
    ENDIF.

    SELECT SINGLE *
      FROM lips INTO @DATA(_wl_lips)
     WHERE vbeln EQ @_wl_vttp-vbeln.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_faturamento
        EXPORTING
          textid = VALUE #( msgid = zcx_faturamento=>zcx_sem_doc_remessa-msgid
                            msgno = zcx_faturamento=>zcx_sem_doc_remessa-msgno )
          msgid  = zcx_faturamento=>zcx_sem_doc_remessa-msgid
          msgno  = zcx_faturamento=>zcx_sem_doc_remessa-msgno
          msgty  = 'E'.
    ENDIF.

    "Definição Dados Romaneio.
    e_zsdt0001-placa_cav    = _wl_vttk-text1(7).  "Placa Cavalo
    e_zsdt0001-placa_car1   = _wl_vttk-text2(7).
    e_zsdt0001-placa_car2   = _wl_vttk-text3(7).
    e_zsdt0001-placa_car3   = _wl_vttk-text4(7).
    e_zsdt0001-vbeln        = _wl_lips-vgbel.   "Ordem
    e_zsdt0001-nr_romaneio  = '999999999'.
    e_zsdt0001-branch       = _wl_lips-werks.

    "" RMNI - 02/06/2022 - BOC - IR096917

    e_zsdt0001-peso_subtotal = _wl_likp-btgew.

    SELECT vbeln, posnr  UP TO 1 ROWS
      FROM vbrp
      INTO @DATA(ls_vbrp)
     WHERE vgbel = @_wl_lips-vbeln
      AND vgpos =  @_wl_lips-posnr AND DRAFT = @SPACE .
    ENDSELECT.

    IF sy-subrc EQ 0.


      SELECT * FROM j_1bnflin
        INTO TABLE @DATA(lt_lin)
        WHERE refkey = @ls_vbrp-vbeln
        AND   refitm = @ls_vbrp-posnr.

      IF sy-subrc EQ 0.
        LOOP AT lt_lin INTO DATA(ls_lin).

          ADD ls_lin-netwr TO e_zsdt0001-netwr.
        ENDLOOP.
      ENDIF.

    ENDIF.

    "" RMNI - 02/06/2022 - EOC - IR096917


*1  Transporte de partida com carga
*2  Transporte de chegada com carga
*3  Transporte de partida vazio
*4  Transporte de chegada vazio
    CASE _wl_vttk-abfer.
      WHEN '1'.
        e_zsdt0001-tp_movimento = 'S'.
      WHEN '2'.
        e_zsdt0001-tp_movimento = 'E'.
        TRY .
            "Nº do agente de frete
            zcl_fornecedores=>zif_parceiros~get_instance( )->set_parceiro( i_parceiro = _wl_vttk-tdlnr  )->ck_parceiro_local_negocio( ).
            e_tp_frete = zif_carga=>st_tp_frete_cif.
          CATCH zcx_parceiros.    " .
            e_tp_frete = zif_carga=>st_tp_frete_cpt.
        ENDTRY.
    ENDCASE.

    SELECT SINGLE *
      FROM vtpa INTO @DATA(_wl_vtpa)
     WHERE vbeln EQ @_wl_vttk-tknum
       AND parvw EQ 'MT'.

    IF ( sy-subrc EQ 0 ) AND ( _wl_vttk-tknum IS NOT INITIAL ) AND ( _wl_vtpa-lifnr IS NOT INITIAL ).
      e_zsdt0001-motorista = _wl_vtpa-lifnr.
    ENDIF.

    IF _wl_likp-vbtyp EQ '7'.
      e_zsdt0001-parid = _wl_likp-lifnr.

      SELECT SINGLE *
        FROM vbpa INTO @DATA(_wl_vbpa)
       WHERE vbeln EQ @_wl_vttp-vbeln
         AND parvw EQ 'WL'.

      IF sy-subrc EQ 0.
        e_zsdt0001-parid  = _wl_vbpa-lifnr.
      ENDIF.

      e_zsdt0001-tp_movimento = 'E'.
      EXIT.
    ENDIF.

    "Remetente
    TRY.
        zcl_ordem_venda=>zif_ordem_venda~get_instance(
          )->set_ordem_venda( i_vbeln = CONV #( e_zsdt0001-vbeln )
          )->get_ordem_venda( IMPORTING e_ordem_venda = wl_vbak
          )->get_centro( IMPORTING e_parid_werks = e_zsdt0001-parid ).

        IF wl_vbak-auart = 'ZTER'.

          TRY.
              DATA(at_kna1) =
              CAST zcl_clientes(
              zcl_clientes=>zif_parceiros~get_instance(
              )->set_parceiro( i_parceiro = CONV #( wl_vbak-kunnr )
              ) )->at_kna1.

              zcl_fornecedores=>zif_parceiros~get_instance(
               )->set_parceiro_cnpj_cpf_ie(
                 EXPORTING
                   i_cnpj             =   CONV #( at_kna1-stcd1 )
                   i_insc_estatual    =   CONV #( at_kna1-stcd3 )
               )->get_id_parceiro( IMPORTING e_parceiro = DATA(_parid_lifnr) ).

              e_zsdt0001-parid = _parid_lifnr.

            CATCH zcx_parceiros INTO DATA(ex_parceiros_k).
              CLEAR: e_zsdt0001-parid.
          ENDTRY.

          IF e_zsdt0001-parid IS INITIAL.
            RAISE EXCEPTION TYPE zcx_faturamento
              EXPORTING
                textid = VALUE #( msgid = zcx_faturamento=>zcx_sem_lifnr_emi_ordem-msgid
                                  msgno = zcx_faturamento=>zcx_sem_lifnr_emi_ordem-msgno
                                  attr1 = CONV #( wl_vbak-vbeln ) )
                msgid  = zcx_faturamento=>zcx_sem_lifnr_emi_ordem-msgid
                msgno  = zcx_faturamento=>zcx_sem_lifnr_emi_ordem-msgno
                msgty  = 'E'
                msgv1  = CONV #( wl_vbak-vbeln ).
          ENDIF.
        ENDIF.

      CATCH zcx_ordem_venda INTO DATA(ex_ordem).

        "Se não Achou a Ordem de Venda procura o Pedido de Compra
        IF zcx_ordem_venda=>zcx_ordem_venda_nao_existe-msgid EQ ex_ordem->msgid AND
           zcx_ordem_venda=>zcx_ordem_venda_nao_existe-msgno EQ ex_ordem->msgno.

          TRY.
              zcl_pedido_compra=>get_instance(
                )->set_pedido( i_ebeln = CONV #( e_zsdt0001-vbeln )
                )->get_centro( IMPORTING e_parid_werks = e_zsdt0001-parid ).
            CATCH zcx_pedido_compra INTO DATA(ex_pedido_compra).
          ENDTRY.
        ENDIF.

    ENDTRY.

  ENDMETHOD.


  METHOD zif_faturamento~get_romaneio_trocanota.

    FREE: e_troca_nota.

    IF i_vbeln IS NOT INITIAL.
      DATA(l_doc_rem) = i_vbeln.
    ELSE.
*-----romaneio
      SELECT vbeln, doc_rem ,fat_contingencia_ecc, id_ordem
        FROM zsdt0001
        INTO (@DATA(l_vbeln), @l_doc_rem, @DATA(l_fat_contingencia_ecc), @DATA(l_id_ordem)) "#150020-27.08.2024-JT
          UP TO 1 ROWS
       WHERE ch_referencia = @i_ch_referencia.
      ENDSELECT.

      CHECK sy-subrc = 0 AND l_fat_contingencia_ecc EQ abap_false AND l_id_ordem IS NOT INITIAL. "#150020-27.08.2024-JT
    ENDIF.

    IF l_doc_rem IS NOT INITIAL.
      SELECT vgbel, vgpos
        FROM lips
        INTO (@DATA(l_vgbel), @DATA(l_vgpos))
          UP TO 1 ROWS
       WHERE vbeln EQ @l_doc_rem.
      ENDSELECT.

*-----Ordem de venda
      SELECT ztrocanota
        INTO @DATA(l_ztrocanota_ov)
        FROM vbak
          UP TO 1 ROWS
       WHERE vbeln EQ @l_vgbel.
      ENDSELECT.

*-----Pedido compras
      IF sy-subrc <> 0.
        SELECT ztrocanota
          INTO @DATA(l_ztrocanota_pc)
          FROM ekpo
            UP TO 1 ROWS
         WHERE ebeln EQ @l_vgbel
           AND ebelp EQ @l_vgpos.
        ENDSELECT.
      ENDIF.

    ELSE.

*-----Ordem de venda
      SELECT ztrocanota
        INTO l_ztrocanota_ov
        FROM vbak
          UP TO 1 ROWS
       WHERE vbeln EQ l_vbeln.
      ENDSELECT.

*-----Pedido compras
      IF sy-subrc <> 0.
        SELECT ztrocanota
          INTO l_ztrocanota_pc
          FROM ekpo
            UP TO 1 ROWS
         WHERE ebeln EQ l_vbeln.
        ENDSELECT.
      ENDIF.
    ENDIF.

    IF l_ztrocanota_ov = abap_true OR
       l_ztrocanota_pc = abap_true.
      e_troca_nota = abap_true.
    ENDIF.

  ENDMETHOD.


  method ZIF_FATURAMENTO~GET_TIPO_TRANSPORTE.

*-------------------------------------------------*
*  VSART - Tipo de expedição
*-------------------------------------------------*

*    01	Rodoviario
*    02	Ferroviário
*    03	Navegação fluvial
*    04	Marítimo
*    05	Aéreo
*    06	Correio, serv.postal
*    07	Multimodal

*-------------------------------------------------*
*  LAUFK - Código de percurso
*-------------------------------------------------*

*    1  Percurso preliminar
*    2  Percurso principal
*    3  Percurso subsequente
*    4  Percurso direto
*    5  Percurso de regresso

*-------------------------------------------------*

    TYPES: BEGIN OF TY_ZSDT0011,
             VSART          TYPE TVTK-VSART,
             LAUFK          TYPE TVTK-LAUFK,
             TP_MOVIMENTO   TYPE ZSDT0011-TP_MOVIMENTO,
             AUART          TYPE ZSDT0011-AUART,
             SHTYP          TYPE ZSDT0011-SHTYP,
             BSART          TYPE ZSDT0011-BSART,
             DEL_REG        TYPE CHAR01,
           END OF TY_ZSDT0011.

    DATA: WL_LFA1_SP TYPE LFA1,
          WL_LFA1_Z1 TYPE LFA1,
          WL_KNA1_LR TYPE KNA1.

    DATA: V_LAUFK    TYPE TVTK-LAUFK.

    DATA: IT_ZSDT0011 TYPE TABLE OF TY_ZSDT0011.

    R_IF_FATURAMENTO = ME.

    CLEAR: E_SHTYP, WL_LFA1_SP, WL_LFA1_Z1, WL_KNA1_LR, IT_ZSDT0011[], V_LAUFK.

    IF I_VSART IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_FATURAMENTO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_TP_EXPED_OBRIGATORIO-MSGID
                            MSGNO = ZCX_FATURAMENTO=>ZCX_TP_EXPED_OBRIGATORIO-MSGNO )
          MSGID  = ZCX_FATURAMENTO=>ZCX_TP_EXPED_OBRIGATORIO-MSGID
          MSGNO  = ZCX_FATURAMENTO=>ZCX_TP_EXPED_OBRIGATORIO-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    "Check Agente Frete
    IF I_PARID_SP IS NOT INITIAL.
      SELECT SINGLE *
        FROM LFA1 INTO WL_LFA1_SP
       WHERE LIFNR EQ I_PARID_SP.

      IF SY-SUBRC NE 0.
        RAISE EXCEPTION TYPE ZCX_FATURAMENTO
           EXPORTING
             TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_PARC_SP_NOT_FOUND-MSGID
                               MSGNO = ZCX_FATURAMENTO=>ZCX_PARC_SP_NOT_FOUND-MSGNO
                               ATTR1 = CONV #( I_PARID_SP ) )
             MSGID  = ZCX_FATURAMENTO=>ZCX_PARC_SP_NOT_FOUND-MSGID
             MSGNO  = ZCX_FATURAMENTO=>ZCX_PARC_SP_NOT_FOUND-MSGNO
             MSGTY  = 'E'
             MSGV1  = CONV #( I_PARID_SP ).
      ENDIF.
    ENDIF.

    "Check Local Entrega
    IF I_PARID_LR IS NOT INITIAL.
      SELECT SINGLE *
        FROM KNA1 INTO WL_KNA1_LR
       WHERE KUNNR EQ I_PARID_LR.

      IF SY-SUBRC NE 0.
        RAISE EXCEPTION TYPE ZCX_FATURAMENTO
           EXPORTING
             TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_PARC_LR_NOT_FOUND-MSGID
                               MSGNO = ZCX_FATURAMENTO=>ZCX_PARC_LR_NOT_FOUND-MSGNO
                               ATTR1 = CONV #( I_PARID_LR ) )
             MSGID  = ZCX_FATURAMENTO=>ZCX_PARC_LR_NOT_FOUND-MSGID
             MSGNO  = ZCX_FATURAMENTO=>ZCX_PARC_LR_NOT_FOUND-MSGNO
             MSGTY  = 'E'
             MSGV1  = CONV #( I_PARID_LR ).
      ENDIF.
    ENDIF.

    "Check Terminal Porto
    IF I_PARID_Z1 IS NOT INITIAL.
      SELECT SINGLE *
        FROM LFA1 INTO WL_LFA1_Z1
       WHERE LIFNR EQ I_PARID_Z1.

      IF SY-SUBRC NE 0.
        RAISE EXCEPTION TYPE ZCX_FATURAMENTO
           EXPORTING
             TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_PARC_Z1_NOT_FOUND-MSGID
                               MSGNO = ZCX_FATURAMENTO=>ZCX_PARC_Z1_NOT_FOUND-MSGNO
                               ATTR1 = CONV #( I_PARID_Z1 ) )
             MSGID  = ZCX_FATURAMENTO=>ZCX_PARC_Z1_NOT_FOUND-MSGID
             MSGNO  = ZCX_FATURAMENTO=>ZCX_PARC_Z1_NOT_FOUND-MSGNO
             MSGTY  = 'E'
             MSGV1  = CONV #( I_PARID_Z1 ).
      ENDIF.
    ENDIF.

    IF I_TIPO_OV IS NOT INITIAL. "Ordem Venda

      SELECT *
        FROM ZSDT0011 INTO CORRESPONDING FIELDS OF TABLE IT_ZSDT0011
       WHERE TP_MOVIMENTO EQ I_TIPO_MOV
         AND AUART        EQ I_TIPO_OV.

      DELETE IT_ZSDT0011 WHERE SHTYP IS INITIAL.

      LOOP AT IT_ZSDT0011 ASSIGNING FIELD-SYMBOL(<FS_ZSDT0011>).

        SELECT SINGLE *
          FROM TVTK INTO @DATA(_WL_TVTK)
         WHERE SHTYP EQ @<FS_ZSDT0011>-SHTYP.

        IF SY-SUBRC NE 0.
          <FS_ZSDT0011>-DEL_REG = ABAP_TRUE.
          CONTINUE.
        ENDIF.

        <FS_ZSDT0011>-LAUFK = _WL_TVTK-LAUFK. "Código de percurso
        <FS_ZSDT0011>-VSART = _WL_TVTK-VSART. "Tipo de expedição

      ENDLOOP.

      DELETE IT_ZSDT0011 WHERE DEL_REG = ABAP_TRUE.

      IF WL_LFA1_SP-DLGRP = '0007'. "Caso Agente Frete se Multimodal.....

        "Buscar Tipo Transporte Multimodal para o Tipo de O.V
        LOOP AT IT_ZSDT0011 INTO DATA(WL_ZSDT0011) WHERE VSART EQ '07'.
          E_SHTYP = WL_ZSDT0011-SHTYP.
          EXIT.
        ENDLOOP.

        RETURN.
      ENDIF.

      "Se informado Local de Entrega e Terminal Portuario
      IF ( WL_KNA1_LR IS NOT INITIAL ) AND ( WL_LFA1_Z1 IS NOT INITIAL ).

        IF ( WL_KNA1_LR-STCD1 NE WL_LFA1_Z1-STCD1 ). "É um Transbordo

          "Buscar Tipo Transporte com código de Percurso Preliminar e Tipo Expedição Informado no Parametro para o Tipo de O.V
          LOOP AT IT_ZSDT0011 INTO WL_ZSDT0011 WHERE VSART EQ I_VSART
                                                 AND LAUFK EQ '1'.     "Percurso preliminar
            E_SHTYP = WL_ZSDT0011-SHTYP.
            EXIT.
          ENDLOOP.

        ELSE. "Não é transbordo

          "Buscar Tipo Transporte com código de Percurso Direto e Tipo Expedição Informado no Parametro para o Tipo de O.V
          LOOP AT IT_ZSDT0011 INTO WL_ZSDT0011 WHERE VSART EQ I_VSART
                                                 AND LAUFK EQ '4'.  "Percurso direto
            E_SHTYP = WL_ZSDT0011-SHTYP.
            EXIT.
          ENDLOOP.

        ENDIF.

        RETURN.

      ELSE. "Se não informado Local de Entrega e Terminal Portuario

        V_LAUFK = '4'.  "Percurso direto

        IF I_LAUFK IS NOT INITIAL.
          V_LAUFK = I_LAUFK.
        ENDIF.

        "Buscar Tipo Transporte com código determinado e Tipo Expedição Informado no Parametro para o Tipo de O.V
        LOOP AT IT_ZSDT0011 INTO WL_ZSDT0011 WHERE VSART EQ I_VSART
                                               AND LAUFK EQ V_LAUFK.
          E_SHTYP = WL_ZSDT0011-SHTYP.
          EXIT.
        ENDLOOP.

      ENDIF.


    ELSEIF I_TIPO_PEDIDO IS NOT INITIAL.  " Pedido

      SELECT SINGLE *
        FROM ZSDT0011 INTO WL_ZSDT0011
       WHERE TP_MOVIMENTO EQ I_TIPO_MOV
         AND BSART        EQ I_TIPO_PEDIDO
         AND SHTYP        NE SPACE.

      IF SY-SUBRC EQ 0.
        E_SHTYP = WL_ZSDT0011-SHTYP.
      ENDIF.

    ENDIF.


  endmethod.


  METHOD ZIF_FATURAMENTO~GET_TIPO_VEICULO.

    R_IF_FATURAMENTO = ME.

    DATA: V_PLACA_CK TYPE ZLEST0002-PC_VEICULO.

    CLEAR: E_TIPO, E_PROPRIETARIO.

    V_PLACA_CK = I_PLACA.

    IF I_TKNUM IS NOT INITIAL.
      SELECT SINGLE *
        FROM VTTK INTO @DATA(WL_VTTK)
       WHERE TKNUM EQ @I_TKNUM.

      IF SY-SUBRC EQ 0.

        IF WL_VTTK-TEXT1 IS INITIAL.

          SELECT SINGLE * INTO @E_PROPRIETARIO
            FROM LFA1
           WHERE LIFNR EQ @WL_VTTK-TDLNR.

          IF E_PROPRIETARIO-STKZN EQ ABAP_TRUE.
            E_TIPO = ZIF_FATURAMENTO=>ST_TP_PROP_VEICULO_TERCEIRO.
          ELSEIF E_PROPRIETARIO-KTOKK NE 'ZFIC'.
            E_TIPO = ZIF_FATURAMENTO=>ST_TP_PROP_VEICULO_TERCEIRO.
          ELSE.
            E_TIPO = ZIF_FATURAMENTO=>ST_TP_PROP_VEICULO_PROPRIO.
          ENDIF.
          EXIT.

        ELSE.
          V_PLACA_CK = WL_VTTK-TEXT1(7).
        ENDIF.

      ENDIF.
    ENDIF.

    """ Tipo do Veículo """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT SINGLE * INTO @DATA(WA_ZLEST0002)
      FROM ZLEST0002
     WHERE PC_VEICULO EQ @V_PLACA_CK.

    IF SY-SUBRC IS NOT INITIAL.
      "004  Veículo &1 não cadastrado na ZLES0003!
      RAISE EXCEPTION TYPE ZCX_FATURAMENTO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_VEICULO_SEM_CAD-MSGID
                            MSGNO = ZCX_FATURAMENTO=>ZCX_VEICULO_SEM_CAD-MSGNO
                            ATTR1 = V_PLACA_CK )
          MSGID  = ZCX_FATURAMENTO=>ZCX_VEICULO_SEM_CAD-MSGID
          MSGNO  = ZCX_FATURAMENTO=>ZCX_VEICULO_SEM_CAD-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( V_PLACA_CK ).
    ENDIF.

    SELECT SINGLE *
      FROM SETLEAF INTO @DATA(_SETLEAF)
     WHERE SETNAME EQ 'ZLES0040_USER_GER_MIRO'
       AND VALFROM EQ @SY-UNAME.

    IF SY-SUBRC EQ 0.
      E_TIPO = ZIF_FATURAMENTO=>ST_TP_PROP_VEICULO_TERCEIRO.
      RETURN.
    ENDIF.

    IF I_TKNUM IS NOT INITIAL.
      SELECT SINGLE *
        FROM VTPA INTO @DATA(WL_VTPA)
       WHERE VBELN EQ @I_TKNUM
         AND PARVW EQ 'PV'.

      IF ( SY-SUBRC EQ 0 ) AND ( WL_VTPA-LIFNR IS NOT INITIAL ).
        WA_ZLEST0002-PROPRIETARIO = WL_VTPA-LIFNR.
      ENDIF.
    ENDIF.

    SELECT SINGLE * INTO @E_PROPRIETARIO
      FROM LFA1
     WHERE LIFNR EQ @WA_ZLEST0002-PROPRIETARIO.

    IF SY-SUBRC IS NOT INITIAL.
      "005  Não encontrado o proprietário do veículo &1!
      RAISE EXCEPTION TYPE ZCX_FATURAMENTO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FATURAMENTO=>ZCX_VEICULO_PROPRIETARIO-MSGID
                            MSGNO = ZCX_FATURAMENTO=>ZCX_VEICULO_PROPRIETARIO-MSGNO
                            ATTR1 = V_PLACA_CK )
          MSGID  = ZCX_FATURAMENTO=>ZCX_VEICULO_PROPRIETARIO-MSGID
          MSGNO  = ZCX_FATURAMENTO=>ZCX_VEICULO_PROPRIETARIO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( V_PLACA_CK ).
    ENDIF.

    IF E_PROPRIETARIO-STKZN EQ ABAP_TRUE.
      E_TIPO = ZIF_FATURAMENTO=>ST_TP_PROP_VEICULO_TERCEIRO.
    ELSEIF E_PROPRIETARIO-KTOKK NE 'ZFIC'.
      E_TIPO = ZIF_FATURAMENTO=>ST_TP_PROP_VEICULO_TERCEIRO.
    ELSE.
      E_TIPO = ZIF_FATURAMENTO=>ST_TP_PROP_VEICULO_PROPRIO.
    ENDIF.


  ENDMETHOD.


  METHOD zif_faturamento~get_valida_merge_pdf.

    DATA: pdf_merger    TYPE REF TO cl_rspo_pdf_merge,
          ex_pdf_merger TYPE REF TO cx_rspo_pdf_merge,
          l_ex_txt      TYPE string,
          l_rc          TYPE i VALUE 0,
          l_docindex    TYPE i VALUE 0,
          l_errordoc    TYPE xstring,
          l_merged_pdf  TYPE zde_data_xstring.

    t_pdf_files_new[] = t_pdf_files[].

*--------------------------------------------------
*-- valida se cada PDF esta ok
*--------------------------------------------------
    LOOP AT t_pdf_files_new INTO DATA(w_pdf).
      DATA(l_tabix) = sy-tabix.

      CHECK w_pdf-data IS NOT INITIAL.

      TRY.
          CREATE OBJECT pdf_merger.
        CATCH cx_rspo_pdf_merge INTO ex_pdf_merger.
          l_ex_txt = ex_pdf_merger->get_text( ).
          EXIT.
      ENDTRY.

      pdf_merger->add_document( w_pdf-data ).
      pdf_merger->merge_documents( IMPORTING merged_document = l_merged_pdf
                                             rc              = l_rc ).
      IF l_rc <> 0.
        DELETE t_pdf_files_new INDEX l_tabix.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_faturamento~get_check_rem_conta_ordem.

    DATA: t_set    TYPE TABLE OF rgsb4,
          w_set    TYPE rgsb4,
          t_tvarvc TYPE TABLE OF tvarvc,
          w_tvarvc TYPE tvarvc,
          r_cfop   TYPE RANGE OF j_1bnflin-cfop,
          w_cfop   LIKE LINE OF r_cfop,
          r_matkl  TYPE RANGE OF j_1bnflin-matkl,
          w_matkl  LIKE LINE OF r_matkl.

    FREE: r_rem_conta_ordem.

    CHECK i_zsdt0001-ch_referencia IS NOT INITIAL.

*---------------------------------
* ler set
*---------------------------------
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        class         = '0000'
        setnr         = 'MAGGI_CFOP_VENDA_IND'
      TABLES
        set_values    = t_set
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.

    LOOP AT t_set INTO w_set.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = w_set-from ) TO r_cfop.
    ENDLOOP.

*---------------------------------
* ler TVARVset
*---------------------------------
    SELECT *
      FROM tvarvc
      INTO TABLE t_tvarvc
     WHERE name = 'MAGGI_GR_FERTILIZANTES'.

    LOOP AT t_tvarvc INTO w_tvarvc.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = w_tvarvc-low ) TO r_matkl.
    ENDLOOP.

*---------------------------------
* ler OV
*---------------------------------
    SELECT matkl, j_1bcfop
      FROM vbap
      INTO @DATA(w_vbap)
        UP TO 1 ROWS
     WHERE vbeln = @i_zsdt0001-vbeln
       AND matnr = @i_zsdt0001-matnr.
    ENDSELECT.

    IF sy-subrc         = 0           AND
       w_vbap-j_1bcfop IN r_cfop[]    AND
       w_vbap-matkl    IN r_matkl[]   AND
       r_cfop[]        IS NOT INITIAL AND
       r_matkl[]       IS NOT INITIAL.
      r_rem_conta_ordem = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_faturamento~get_gera_vt_frota_propria.

    FREE: r_gera_transp.

    TRY.
        zif_faturamento~get_processo_emissao_docs(
          EXPORTING
            i_ch_romaneio      = i_ch_referencia
          IMPORTING
            e_tipo_veiculo     = DATA(_tipo_veiculo)
            e_tipo_remetente   = DATA(_tipo_remetente)
            e_tp_frete         = DATA(_tp_frete) ).

      CATCH zcx_faturamento INTO DATA(_zcx_fat).
        RETURN.
      CATCH zcx_error       INTO DATA(_zcx_error).
        RETURN.
    ENDTRY.

    IF _tipo_veiculo  = 'P ' AND _tipo_remetente = 'P' AND _tp_frete = 'CIF'.
      r_gera_transp = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_faturamento~get_status_gerou_vt_vi.

    DATA: lc_romaneio        TYPE zsdt0001,
          lc_gera_transp     TYPE char01,
          lc_times           TYPE i,
          lc_rem_conta_ordem TYPE char01.

    FREE: e_tknum, e_fknum.

    r_gerou_vt = abap_true.

    IF sy-tcode = 'ZNFE'.
      SELECT SINGLE *
        FROM tvarvc
        INTO @DATA(tvarvc_bloq_frete_vt_vi)
       WHERE name = 'ZNFE_NO_BLOCK_FRETE_CPT'.

      CHECK sy-subrc <> 0.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0001
      INTO @DATA(_zsdt0001)
     WHERE ch_referencia = @i_ch_referencia.

    CHECK sy-subrc = 0 AND _zsdt0001-tp_movimento = 'S'.

    CHECK _zsdt0001-agente_frete IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = _zsdt0001-agente_frete
      IMPORTING
        output = _zsdt0001-agente_frete.

    SELECT SINGLE ktokk
      FROM lfa1
      INTO @DATA(_ktokk)
     WHERE lifnr = @_zsdt0001-agente_frete.

    CHECK sy-subrc = 0.

*-- Checa frota propria
    lc_gera_transp     = zif_faturamento~get_gera_vt_frota_propria( i_ch_referencia ).

*-- Checa remessa conta e ordem
    lc_rem_conta_ordem = zif_faturamento~get_check_rem_conta_ordem( _zsdt0001 ).

*-- remessa
    SELECT SINGLE inco1
      FROM likp
      INTO @DATA(_inco1)
     WHERE vbeln = @_zsdt0001-doc_rem.

    CHECK ( sy-subrc = 0   AND   _inco1 = 'CPT' ) OR lc_gera_transp = abap_true.
    CHECK _ktokk <> 'ZFIC'  OR ( _ktokk = 'ZFIC' AND lc_gera_transp = abap_true ).
    CHECK lc_rem_conta_ordem = abap_false.

*-----------------------------------------------------
*-- verifica VI gerada apos a solicitacao de gerar VT
*-----------------------------------------------------
    IF _zsdt0001-fknum IS NOT INITIAL.
      r_gerou_vt = abap_true.
      e_tknum    = _zsdt0001-doc_transp.
      e_fknum    = _zsdt0001-fknum.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = _zsdt0001-fknum
        IMPORTING
          output = _zsdt0001-fknum.

      lc_times = COND #( WHEN i_check_doc_gerado = abap_true THEN 5
                                                             ELSE 1 ).

      DO lc_times TIMES.
        SELECT SINGLE *
          FROM vfkp
          INTO @DATA(_vfkp)
         WHERE fknum = @_zsdt0001-fknum
           AND fkpty = 'Z001'.

        IF sy-subrc <> 0.
          WAIT UP TO 5 SECONDS.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.

      IF sy-subrc = 0.
        IF _vfkp-netwr = 0.
          RAISE EXCEPTION TYPE zcx_error
            EXPORTING
              textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                                msgno = zcx_error=>zcx_erro_geral-msgno
                                attr1 = CONV #( |Documento de Custo { _zsdt0001-fknum } | )
                                attr2 = CONV #( | criado sem valor!| ) )
              msgid  = zcx_error=>zcx_erro_geral-msgid
              msgno  = zcx_error=>zcx_erro_geral-msgno
              msgty  = 'E'
              msgv1  = CONV #( |Documento de Custo { _zsdt0001-fknum } | )
              msgv2  = CONV #( | criado sem valor!| ).
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( |Documento de custo não gerado para o romaneio | )
                              attr2 = CONV #( |{ _zsdt0001-nr_romaneio }!| ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( |Documento de custo não gerado para o romaneio | )
            msgv2  = CONV #( |{ _zsdt0001-nr_romaneio }!| ).
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_error
        EXPORTING
          textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                            msgno = zcx_error=>zcx_erro_geral-msgno
                            attr1 = CONV #( |Documento de custo não gerado para o romaneio | )
                            attr2 = CONV #( |{ _zsdt0001-nr_romaneio }!| ) )
          msgid  = zcx_error=>zcx_erro_geral-msgid
          msgno  = zcx_error=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = CONV #( |Documento de custo não gerado para o romaneio | )
          msgv2  = CONV #( |{ _zsdt0001-nr_romaneio }!| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_faturamento~set_estornar_biodiesel_frota.

    DATA: lc_ch_ref       TYPE zch_ref,
          lc_invoice_migo TYPE bapi2017_gm_head_ret,
          w_return        TYPE bapiret2,
          t_return        TYPE TABLE OF bapiret2.

    r_estorno_ok = abap_true.

*----------------------------------
*-- buscar doc material biodiesel
*----------------------------------
    lc_ch_ref = 'BIO' && i_ch_referencia.

    SELECT mkpf~mblnr, mkpf~mjahr
      INTO TABLE @DATA(t_mkpf)
      FROM mkpf
      INNER JOIN mseg  ON mseg~mblnr = mkpf~mblnr
                      AND mseg~mjahr = mkpf~mjahr
     WHERE mkpf~bktxt = @lc_ch_ref
       AND mseg~smbln = @abap_off.

    CHECK sy-subrc = 0.

    SORT t_mkpf BY mblnr DESCENDING.

    READ TABLE t_mkpf INTO DATA(w_mkpf) INDEX 1.

    SELECT smbln
      INTO @DATA(_mseg)
        UP TO 1 ROWS
      FROM mseg
     WHERE smbln = @w_mkpf-mblnr
       AND gjahr = @w_mkpf-mjahr.
    ENDSELECT.

    CHECK sy-subrc <> 0.

*----------------------------------
*-- estorna mov material
*----------------------------------
    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
      EXPORTING
        materialdocument    = w_mkpf-mblnr
        matdocumentyear     = w_mkpf-mjahr
        goodsmvt_pstng_date = sy-datum
      IMPORTING
        goodsmvt_headret    = lc_invoice_migo
      TABLES
        return              = t_return.

    IF lc_invoice_migo IS NOT INITIAL.
      r_estorno_ok = abap_true.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      WAIT UP TO 5 SECONDS.
    ELSE.
      r_estorno_ok = abap_false.

      READ TABLE t_return INTO w_return WITH KEY type = 'E'.

      IF sy-subrc = 0.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = w_return-id
                              msgno = w_return-number
                              attr1 = CONV #( w_return-message_v1 )
                              attr2 = CONV #( w_return-message_v2 )
                              attr3 = CONV #( w_return-message_v3 )
                              attr4 = CONV #( w_return-message_v4 ) )
            msgid  = w_return-id
            msgno  = w_return-number
            msgty  = 'S' "w_return-type
            msgv1  = CONV #( w_return-message_v1 )
            msgv2  = CONV #( w_return-message_v2 )
            msgv3  = CONV #( w_return-message_v3 )
            msgv4  = CONV #( w_return-message_v4 ).
      ELSE.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Biodiesel: ' )
                              attr2 = CONV #( 'Erro Estorno Doc.Materal:' )
                              attr3 = CONV #( lc_invoice_migo-mat_doc && '/' && lc_invoice_migo-doc_year ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'S'
            msgv1  = CONV #( 'Biodiesel: ' )
            msgv2  = CONV #( 'Erro Estorno Doc.Materal:' )
            msgv3  = CONV #( lc_invoice_migo-mat_doc && '/' && lc_invoice_migo-doc_year ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_faturamento~set_transfere_biodiesel_frota.

    TYPES: BEGIN OF ty_dados_mr21,
             data_vencimento  TYPE zmmt0178-data_vencimento,
             tipo             TYPE zmmt0178-tipo,
             status           TYPE icon-id,
             werks            TYPE zmmt0178-werks,
             matnr            TYPE zmmt0178-matnr,
             maktx            TYPE zmmt0178-maktx,
             vprsv            TYPE zmmt0178-vprsv,
             vlr_brl          TYPE zmmt0178-vlr_brl,
             vlr_usd          TYPE zmmt0178-vlr_usd,
             stprs            TYPE zmmt0178-stprs,
             aval_atual_brl   TYPE zmmt0178-aval_atual_brl,
             aval_atual_usd   TYPE zmmt0178-aval_atual_usd,
             aval_atual_stprs TYPE zmmt0178-aval_atual_stprs,
             vlr_brl_old      TYPE zmmt0178-vlr_brl_old,
             vlr_brl_new      TYPE zmmt0178-vlr_brl_new,
             vlr_usd_old      TYPE zmmt0178-vlr_usd_old,
             vlr_usd_new      TYPE zmmt0178-vlr_usd_new,
             stprs_old        TYPE zmmt0178-stprs_old,
             stprs_new        TYPE zmmt0178-stprs_new,
             data_ref         TYPE zmmt0178-data_vencimento,
             doc_mr22         TYPE zmmt0178-doc_mr22,
             message          TYPE char100,
             usuario          TYPE zmmt0178-usuario,
             data_modif       TYPE zmmt0178-data_modif,
             timestamp        TYPE zmmt0178-timestamp,
             modify           TYPE char1,
           END   OF ty_dados_mr21.

    DATA: lc_matnr_de        TYPE matnr,
          lc_matnr_para      TYPE matnr,
          lc_lgort_de        TYPE lgort_d,
          lc_lgort_para      TYPE lgort_d,
          lc_mat_doc         TYPE bapi2017_gm_head_ret-mat_doc,
          lc_doc_year        TYPE bapi2017_gm_head_ret-doc_year,
          lc_gdatu           TYPE gdatu_inv,
          lc_tx_cambio       TYPE ukurs_curr,
          lc_dados_mr21      TYPE ty_dados_mr21,
          w_code             TYPE bapi2017_gm_code,
          w_goodsmvt_header  TYPE bapi2017_gm_head_01,
          w_goodsmvt_item    TYPE bapi2017_gm_item_create,
          t_goodsmvt_item    TYPE TABLE OF bapi2017_gm_item_create,
          w_return           TYPE bapiret2,
          t_return           TYPE TABLE OF bapiret2,
          lc_obj_zcl_util_sd TYPE REF TO zcl_util_sd,
          lc_obj_mr21        TYPE REF TO zcl_mm_mr21_mr22.

    r_biodiesel_ok = abap_off.

*-- tx cambio, mr21
    CREATE OBJECT: lc_obj_zcl_util_sd, lc_obj_mr21.

*----------------------------------
*-- buscar romaneio
*----------------------------------
    SELECT SINGLE *
      INTO @DATA(_zsdt0001)
      FROM zsdt0001
     WHERE ch_referencia = @i_ch_referencia.

    CHECK sy-subrc = 0.

*----------------------------------
*-- check biodiesel
*----------------------------------
    SELECT SINGLE zsdt0053~*
      INTO @DATA(_zsdt0053)
      FROM zsdt0051
     INNER JOIN zsdt0053 ON zsdt0053~nro_sol_ov = zsdt0051~nro_sol_ov
                        AND zsdt0053~vbeln      = @_zsdt0001-vbeln
     WHERE zsdt0051~auart = 'ZUB'.

    CHECK sy-subrc = 0.

*----------------------------------
*-- de para bosdiesel
*----------------------------------
    SELECT SINGLE *
      FROM tvarvc
      INTO @DATA(w_tvarv)
     WHERE name = 'ZLES0136_DEPARA_BIODIESEL'.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_error
        EXPORTING
          textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                            msgno = zcx_error=>zcx_erro_geral-msgno
                            attr1 = CONV #( 'Biodiesel: ' )
                            attr2 = CONV #( 'De-Para Biodiesel não Encontrado!' ) )
          msgid  = zcx_error=>zcx_erro_geral-msgid
          msgno  = zcx_error=>zcx_erro_geral-msgno
          msgty  = 'S'
          msgv1  = CONV #( 'Biodiesel: ' )
          msgv2  = CONV #( 'De-Para Biodiesel não Encontrado!' ).
    ENDIF.

*----------------------------------
*-- deposito DE
*----------------------------------
    SELECT SINGLE lgort
      INTO @DATA(_lgort_de)
      FROM ekpo
     WHERE ebeln = @_zsdt0001-vbeln.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_error
        EXPORTING
          textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                            msgno = zcx_error=>zcx_erro_geral-msgno
                            attr1 = CONV #( 'Biodiesel: ' )
                            attr2 = CONV #( 'Deposito Ped.Compras não Encontrado!' ) )
          msgid  = zcx_error=>zcx_erro_geral-msgid
          msgno  = zcx_error=>zcx_erro_geral-msgno
          msgty  = 'S'
          msgv1  = CONV #( 'Biodiesel: ' )
          msgv2  = CONV #( 'Deposito Ped.Compras não Encontrado!' ).
    ENDIF.

    SPLIT w_tvarv-low  AT '|' INTO  lc_matnr_de   lc_lgort_de.
    SPLIT w_tvarv-high AT '|' INTO  lc_matnr_para lc_lgort_para.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = lc_matnr_de
      IMPORTING
        output = lc_matnr_de.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = lc_matnr_para
      IMPORTING
        output = lc_matnr_para.

*----------------------------------
*-- deposito PARA
*----------------------------------
    SELECT SINGLE lgort
      INTO @DATA(_lgort_para)
      FROM mard
     WHERE matnr = @lc_matnr_para
       AND werks = @_zsdt0001-branch.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_error
        EXPORTING
          textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                            msgno = zcx_error=>zcx_erro_geral-msgno
                            attr1 = CONV #( 'Biodiesel: ' )
                            attr2 = CONV #( 'Deposito Destino não Encontrado!' ) )
          msgid  = zcx_error=>zcx_erro_geral-msgid
          msgno  = zcx_error=>zcx_erro_geral-msgno
          msgty  = 'S'
          msgv1  = CONV #( 'Biodiesel: ' )
          msgv2  = CONV #( 'Deposito Destino não Encontrado!' ).
    ENDIF.

*----------------------------------
*-- transferencia estoque
*----------------------------------
*---header
    w_code-gm_code                = '06'.
    w_goodsmvt_header-pstng_date  = sy-datum.
    w_goodsmvt_header-doc_date    = sy-datum.
    w_goodsmvt_header-header_txt  = 'BIO' && _zsdt0001-ch_referencia.
    w_goodsmvt_header-ref_doc_no  = _zsdt0001-vbeln.

*---itens
    w_goodsmvt_item-material      = lc_matnr_de.
    w_goodsmvt_item-plant         = _zsdt0001-branch.
    w_goodsmvt_item-stge_loc      = lc_lgort_de.   "'PR01'.  "_lgort_de
    w_goodsmvt_item-batch         = _zsdt0001-nr_safra.
    w_goodsmvt_item-move_mat      = lc_matnr_para.
    w_goodsmvt_item-move_plant    = _zsdt0001-branch.
    w_goodsmvt_item-move_stloc    = lc_lgort_para. "'CO01'.  "_lgort_para.
    w_goodsmvt_item-move_batch    = _zsdt0001-nr_safra.
    w_goodsmvt_item-move_type     = '309'.
    w_goodsmvt_item-entry_qnt     = _zsdt0001-qtde_remessa.
    APPEND w_goodsmvt_item       TO t_goodsmvt_item.

*-- Executa a BAPI
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = w_goodsmvt_header
        goodsmvt_code    = w_code
      IMPORTING
        materialdocument = lc_mat_doc
        matdocumentyear  = lc_doc_year
      TABLES
        goodsmvt_item    = t_goodsmvt_item
        return           = t_return.

    IF lc_mat_doc IS INITIAL.
      READ TABLE t_return INTO w_return WITH KEY type = 'E'.

      IF sy-subrc = 0.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = w_return-id
                              msgno = w_return-number
                              attr1 = CONV #( w_return-message_v1 )
                              attr2 = CONV #( w_return-message_v2 )
                              attr3 = CONV #( w_return-message_v3 )
                              attr4 = CONV #( w_return-message_v4 ) )
            msgid  = w_return-id
            msgno  = w_return-number
            msgty  = 'S' "w_return-type
            msgv1  = CONV #( w_return-message_v1 )
            msgv2  = CONV #( w_return-message_v2 )
            msgv3  = CONV #( w_return-message_v3 )
            msgv4  = CONV #( w_return-message_v4 ).
      ELSE.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Biodiesel: ' )
                              attr2 = CONV #( 'Erro na Transferencia de Estoque!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'S'
            msgv1  = CONV #( 'Biodiesel: ' )
            msgv2  = CONV #( 'Erro na Transferencia de Estoque!' ).
      ENDIF.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

*----------------------------------
*-- obter taxa cambio
*----------------------------------
    MOVE _zsdt0001-dt_movimento TO lc_gdatu.

    lc_obj_zcl_util_sd->set_kurst('B').
    lc_obj_zcl_util_sd->set_waerk('USD').
    lc_obj_zcl_util_sd->set_tcurr('BRL').
    lc_obj_zcl_util_sd->set_data( lc_gdatu ).

    lc_tx_cambio = abs( lc_obj_zcl_util_sd->taxa_cambio( ) ).

*----------------------------------
*-- executar mr21
*----------------------------------
    lc_dados_mr21-werks   = _zsdt0001-branch.
    lc_dados_mr21-matnr   = lc_matnr_para.
    lc_dados_mr21-vlr_brl = _zsdt0053-dmbtr       / 1000.
    lc_dados_mr21-vlr_usd = lc_dados_mr21-vlr_brl / lc_tx_cambio.

    lc_obj_mr21->set_dados_shdb( i_bukrs      = _zsdt0001-bukrs
                                 i_data       = sy-datum
                                 i_referencia = 'BIODIESEL' ).

    lc_obj_mr21->batch_input_mr21( is_alv     = lc_dados_mr21 ).

*-US163737-15.01.2025-#163737-JT-inicio
*   lc_obj_mr21->call_transaction_mr21_mr22( ).
    lc_obj_mr21->submit_mr21_mr22( ).
*-US163737-15.01.2025-#163737-JT-fim

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ENDMETHOD.
ENDCLASS.
