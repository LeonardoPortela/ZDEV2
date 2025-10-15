class ZCL_BOLETIM_PRODUCAO_02 definition
  public
  inheriting from ZCL_BOLETIM_PRODUCAO
  final
  create public .

public section.

  methods ZIF_BOLETIM_PRODUCAO~ESTORNO_DOC_PRODUCAO
    redefinition .
  methods ZIF_BOLETIM_PRODUCAO~GERAR_DOC_PRODUCAO_01
    redefinition .
  methods ZIF_BOLETIM_PRODUCAO~GERAR_DOC_PRODUCAO_02
    redefinition .
  methods ZIF_BOLETIM_PRODUCAO~GERAR_DOC_PRODUCAO_03
    redefinition .
  methods ZIF_BOLETIM_PRODUCAO~GERAR_DOC_PRODUCAO_04
    redefinition .
  methods ZIF_BOLETIM_PRODUCAO~GERAR_DOC_PRODUCAO_05
    redefinition .
  methods ZIF_BOLETIM_PRODUCAO~GERAR_RATEIO_PRODUTOS
    redefinition .
  methods ZIF_BOLETIM_PRODUCAO~GET_KEY_DOCS
    redefinition .
  methods ZIF_BOLETIM_PRODUCAO~GET_PERC_RENDIMENTO
    redefinition .
  methods ZIF_BOLETIM_PRODUCAO~GET_TP_PRODUTOS_PRODUCAO
    redefinition .
  methods ZIF_BOLETIM_PRODUCAO~GET_TP_PRODUTOS_RENDIMENTO
    redefinition .
  methods ZIF_BOLETIM_PRODUCAO~GRAVAR_REGISTRO
    redefinition .
  methods ZIF_BOLETIM_PRODUCAO~VALIDAR_REGISTRO
    redefinition .
  methods ZIF_BOLETIM_PRODUCAO~VALIDAR_MATERIAIS_DOC_PROD
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BOLETIM_PRODUCAO_02 IMPLEMENTATION.


  METHOD zif_boletim_producao~estorno_doc_producao.
    DATA: wl_confirmation_es TYPE bapi_rm_datkey-cancconfirmation,
          es_confirmation    TYPE bapi_rm_datkey-confirmation,
          wa_return          TYPE bapiret2.

    CLEAR: r_mblnr_estorno, wl_confirmation_es, es_confirmation, wa_return.

*    me->zif_boletim_producao~check_desaprovacao( i_id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim  ).

    "Verificar se o boletim esta aprovado.
    SELECT SINGLE *
          FROM zsdt0246 INTO @DATA(wl_0246)
         WHERE id_boletim EQ @me->zif_boletim_producao~at_cabecalho-id_boletim.

    IF wl_0246-aprovado EQ abap_true.
       RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_aprovado-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_aprovado-msgno
                            attr1 = CONV #( 'Id. Boletim' && me->zif_boletim_producao~at_cabecalho-id_boletim )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_aprovado-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_aprovado-msgid
          msgv1  = CONV #( 'Documento Boletim Id:' && me->zif_boletim_producao~at_cabecalho-id_boletim ).
    ENDIF.

    CHECK i_mblnr IS NOT INITIAL.

    me->zif_boletim_producao~check_permissao_modificacao( EXPORTING i_mblnr = i_mblnr ).

    SELECT SINGLE *
      FROM blpp INTO @DATA(wl_blpp)
     WHERE belnr EQ @i_mblnr.

    IF sy-subrc NE 0.

      "Verificar documento se exite na tabela MSEG.
      SELECT SINGLE *
      FROM mseg
      INTO @DATA(ws_mseg)
      WHERE mblnr EQ @i_mblnr.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                              msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                              attr1 = CONV #( 'Doc. Confirmação' )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
            msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
            msgv1  = CONV #( 'Doc. Confirmação' ).

      ELSE.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = sy-tabix
            text       = |Estornando Documento { ws_mseg-mblnr } ...|.

        DATA: res       TYPE bapi2017_gm_head_ret,
              tl_return TYPE TABLE OF bapiret2.

        CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
          EXPORTING
            materialdocument = ws_mseg-mblnr
            matdocumentyear  = ws_mseg-mjahr
          IMPORTING
            goodsmvt_headret = res
          TABLES
            return           = tl_return.

        IF res-mat_doc IS NOT INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          r_mblnr_estorno = res-mat_doc.

          MESSAGE |Documento de Estorno: { res-mat_doc } gerado com sucesso!| TYPE 'S'.

        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          zcl_boletim_producao=>zif_boletim_producao~gera_erro_geral(
            EXPORTING
              i_msgv1              =   CONV #( wa_return-message_v1 )
              i_msgv2              =   CONV #( wa_return-message_v2 )
              i_msgv3              =   CONV #( wa_return-message_v3 )
              i_msgv4              =   CONV #( wa_return-message_v4 )
              i_msgid              =   CONV #( wa_return-id )
              i_msgno              =   CONV #( wa_return-number )  ).

        ENDIF.
      ENDIF.
    ELSE.


      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = sy-tabix
          text       = |Estornando Documento { i_mblnr } ...|.


      es_confirmation = wl_blpp-prtnr.

      CALL FUNCTION 'BAPI_REPMANCONF1_CANCEL'
        EXPORTING
          confirmation     = es_confirmation
          postdate         = zif_boletim_producao~at_date
        IMPORTING
          cancconfirmation = wl_confirmation_es
          return           = wa_return.

      IF wa_return-type = 'E'.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        zcl_boletim_producao=>zif_boletim_producao~gera_erro_geral(
          EXPORTING
            i_msgv1              =   CONV #( wa_return-message_v1 )
            i_msgv2              =   CONV #( wa_return-message_v2 )
            i_msgv3              =   CONV #( wa_return-message_v3 )
            i_msgv4              =   CONV #( wa_return-message_v4 )
            i_msgid              =   CONV #( wa_return-id )
            i_msgno              =   CONV #( wa_return-number )  ).
      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        DO 100 TIMES.

          SELECT SINGLE *
            FROM blpp INTO @DATA(gs_blpp)
           WHERE prtnr = @wl_confirmation_es
             AND prtps = '0001'.

          IF sy-subrc = 0.

            COMMIT WORK AND WAIT .

            r_mblnr_estorno = gs_blpp-belnr.

            MESSAGE |Documento de Estorno: { gs_blpp-belnr } gerado com sucesso!| TYPE 'S'.

            EXIT.

          ELSE.
            WAIT UP TO 1 SECONDS.
          ENDIF.

        ENDDO.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_doc_producao_01.
    DATA: wl_zsdt0252      TYPE zsdt0252.

    DATA: es_bflushflags   TYPE bapi_rm_flg,
          es_bflushdatagen TYPE bapi_rm_datgen,
          es_confirmation  TYPE bapi_rm_datkey-confirmation,
          gs_blpp          TYPE blpp,
          it_return        TYPE TABLE OF bapiret2,
          wa_mara          TYPE mara,
          wa_return        TYPE bapiret2.

    DATA: it_goodsmovements  TYPE TABLE OF bapi2017_gm_item_create,
          wa_goodsmovements  TYPE bapi2017_gm_item_create,
          it_bapi_char_batch TYPE TABLE OF bapi_char_batch,
          wa_bapi_char_batch TYPE bapi_char_batch,
          es_goodsmvt_header TYPE bapi2017_gm_head_01,
          es_goodsmvt_code   TYPE bapi2017_gm_code.

    DATA: vg_matnr TYPE char18.

    r_if_boletim_producao = me.

    CLEAR: e_mblnr.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_doc_prod_01.

*---------------------------------------------------------------------------------------------------------*
*   Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    me->zif_boletim_producao~check_aprovacao( i_id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim  ).

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_false.
      me->zif_boletim_producao~gerar_rateio_produtos( ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0252 INTO wl_zsdt0252
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
*       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ 1.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    IF wl_zsdt0252-doc_prod_01 IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                            attr1 = CONV #( 'Documento Produção 01' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
          msgv1  = CONV #( 'Documento Produção 01' ).
    ELSE.

      me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs = CONV #( _key_lcto )
                                                       IMPORTING e_mblnr    = e_mblnr ).

      IF e_mblnr IS NOT INITIAL.

        wl_zsdt0252-doc_prod_01     = e_mblnr.
        wl_zsdt0252-ano_doc_prod_01 = me->zif_boletim_producao~at_cabecalho-dt_lancamento(4).
        MODIFY zsdt0252 FROM wl_zsdt0252.

        IF sy-subrc NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                                msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                                attr1 = CONV #( 'Documento Produção 01' )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
              msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
              msgv1  = CONV #( 'Documento Produção 01' ).
        ENDIF.

        MESSAGE |Documento de Produção: { e_mblnr } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_true.
      "Validar Lançamento Entrada Industrialização
      DATA(_valida_lcto_ent_ind) = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto    = wl_zsdt0252-seqlcto_ent_ind ).

      IF _valida_lcto_ent_ind EQ abap_false.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_not_generate-msgid
                              msgno = zcx_boletim_producao=>zcx_document_not_generate-msgno
                              attr1 = CONV #( 'NF Ent.Industrialização( Filial:' && wl_zsdt0252-branch && ')' )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_document_not_generate-msgno
            msgid  = zcx_boletim_producao=>zcx_document_not_generate-msgid
            msgv1  = CONV #( 'NF Ent.Industrialização( Filial:' && wl_zsdt0252-branch && ')' ).
      ENDIF.
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Gerando Documento de Produção 01 ( Filial: { wl_zsdt0252-branch } )|.

*--------------------------------------------------------------------------------------------*
*   Limpeza Variaveis
*--------------------------------------------------------------------------------------------*
    CLEAR: es_bflushflags, es_bflushdatagen, es_confirmation, it_goodsmovements[], wa_return, it_return[].

*--------------------------------------------------------------------------------------------*
*   Montagem Cabeçalho
*--------------------------------------------------------------------------------------------*

    es_bflushflags-bckfltype        = '01'.

    "Oleo Neutro Industrializado

*------------------------Inicio verifica produto terceiro / CS20230000708 / AOENNING
    IF me->zif_boletim_producao~AT_OI_MAT_TERCEIRO EQ ABAP_FALSE.
    DATA(wl_zsdt0250) = me->zif_boletim_producao~get_material_boletim( i_tp_produto =  'NI'). "Oleo Neutro Industrializado próprio.
    ELSE.
    wl_zsdt0250       = me->zif_boletim_producao~get_material_boletim( i_tp_produto =  'NT'). "Oleo Neutro Industrializado de terceiro.
    ENDIF.
*------------------------Fim verifica produto terceiro / CS20230000708 / AOENNING

    es_bflushdatagen-postdate       = me->zif_boletim_producao~at_cabecalho-dt_lancamento. "Data Lançamento
    es_bflushdatagen-docdate        = me->zif_boletim_producao~at_cabecalho-dt_producao.   "Data Documento
    es_bflushdatagen-docheadertxt   = _key_lcto.
    es_bflushdatagen-prodplant      = wl_zsdt0252-werks_v.                                 "Centro

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.

*---> 30/06/2023 - Migração S4 - JP
   es_bflushdatagen-materialnr     = wl_zsdt0250-matnr.                                   "Material
    DATA: v_len TYPE sy-tabix.
    v_len = strlen( wl_zsdt0250-matnr ).
    CLEAR: es_bflushdatagen-materialnr, es_bflushdatagen-materialnr_long.
    IF v_len > 18.
      es_bflushdatagen-materialnr      = wl_zsdt0250-matnr.                             "Material
    ELSE.
      es_bflushdatagen-materialnr_long = wl_zsdt0250-matnr.                             "Material
    ENDIF.
*---> 30/06/2023 - Migração S4 - JP



    es_bflushdatagen-materialnr     = wl_zsdt0250-matnr.
    es_bflushdatagen-backflquant    = wl_zsdt0252-qtde_ni_doc_01.                          "Quantidade
*    es_bflushdatagen-unitofmeasure  = 'KG'.                                                "UM
    me->zif_boletim_producao~get_und_material( EXPORTING i_matnr =  wl_zsdt0250-matnr IMPORTING e_meins = es_bflushdatagen-unitofmeasure ).
    es_bflushdatagen-prodversion    = me->zif_boletim_producao~get_versao( i_matnr = CONV #( wl_zsdt0250-matnr  ) i_werks = CONV #( wl_zsdt0252-werks_v ) ).
*    es_bflushdatagen-prodversion = |{ es_bflushdatagen-prodversion ALPHA = IN }|.
*    es_bflushdatagen-batch          = wl_zsdt0252-charg.                                   "Lote
    es_bflushdatagen-storageloc     = me->zif_boletim_producao~at_cabecalho-lgort_prod.    "Deposito PR01

*--------------------------------------------------------------------------------------------*
*   Montagem Itens
*--------------------------------------------------------------------------------------------*
    "
    "Oleo Degomado Industrializado
    CLEAR: wa_goodsmovements.


*------------------------Inicio verifica produto terceiro / CS20230000708 / AOENNING
    IF me->zif_boletim_producao~AT_OI_MAT_TERCEIRO EQ ABAP_FALSE.
    wl_zsdt0250 = me->zif_boletim_producao~get_material_boletim( i_tp_produto =  'NI'). "Oleo Neutro Industrializado próprio.
    ELSE.
    wl_zsdt0250       = me->zif_boletim_producao~get_material_boletim( i_tp_produto =  'NT'). "Oleo Neutro Industrializado de terceiro.
    ENDIF.
*------------------------Fim verifica produto terceiro / CS20230000708 / AOENNING

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.

*---> 30/06/2023 - Migração S4 - JP
   wa_goodsmovements-material         = wl_zsdt0250-matnr.
    v_len = strlen( wl_zsdt0250-matnr ).
    CLEAR: wa_goodsmovements-material, wa_goodsmovements-material_long.
    IF v_len > 18.
      wa_goodsmovements-material_long  = wl_zsdt0250-matnr.
    ELSE.
      wa_goodsmovements-material       = wl_zsdt0250-matnr.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP


    wa_goodsmovements-material      = wl_zsdt0250-matnr.
    wa_goodsmovements-plant         = wl_zsdt0252-werks_v.        "Centro
    wa_goodsmovements-stge_loc      = wl_zsdt0252-lgort_v.        "Deposito Consumo "PR01
*    wa_goodsmovements-batch         = wl_zsdt0252-charg.          "Lote Consumo
    wa_goodsmovements-move_type     = '131'.                      "Tipo Movimento
    wa_goodsmovements-entry_qnt     = wl_zsdt0252-qtde_ni_doc_01. "Quantidade Consumo
*    wa_goodsmovements-entry_uom     = 'KG'.                       "UM Consumo
    me->zif_boletim_producao~get_und_material( EXPORTING i_matnr =  wl_zsdt0250-matnr IMPORTING e_meins = wa_goodsmovements-entry_uom ).
    APPEND wa_goodsmovements TO it_goodsmovements.

    me->zif_boletim_producao~check_estoque_produtos( i_itens = it_goodsmovements ).


    "Oleo Neutro
    CLEAR: wa_goodsmovements.

    wl_zsdt0250 = me->zif_boletim_producao~get_material_boletim( i_tp_produto =  'ON').

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.

*---> 30/06/2023 - Migração S4 - JP
   wa_goodsmovements-material         = wl_zsdt0250-matnr.
    v_len = strlen( wl_zsdt0250-matnr ).
    CLEAR: wa_goodsmovements-material, wa_goodsmovements-material_long.
    IF v_len > 18.
      wa_goodsmovements-material_long  = wl_zsdt0250-matnr.
    ELSE.
      wa_goodsmovements-material       = wl_zsdt0250-matnr.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP


    wa_goodsmovements-material      = wl_zsdt0250-matnr.
    wa_goodsmovements-plant         = wl_zsdt0252-werks_v.        "Centro
    wa_goodsmovements-stge_loc      = wl_zsdt0252-lgort_v.        "Deposito Consumo "PR01
*    wa_goodsmovements-batch         = wl_zsdt0252-charg.          "Lote Consumo
    wa_goodsmovements-move_type     = '261'.                      "Tipo Movimento
    wa_goodsmovements-entry_qnt     = wl_zsdt0252-qtde_on_doc_01. "Quantidade Consumo
*    wa_goodsmovements-entry_uom     = 'KG'.                       "UM Consumo
    me->zif_boletim_producao~get_und_material( EXPORTING i_matnr =  wl_zsdt0250-matnr IMPORTING e_meins = wa_goodsmovements-entry_uom ).
    APPEND wa_goodsmovements TO it_goodsmovements.

    me->zif_boletim_producao~check_estoque_produtos( i_itens = it_goodsmovements ).

    DATA(it_goodsmovements_aux) = it_goodsmovements[].


    APPEND VALUE #(
                    material       = wl_zsdt0250-matnr
*---> 29/06/2023 - Migração S4 - JP - tamanho do campo MATNR mudou.
                    material_long  = es_bflushdatagen-materialnr_long
*<--- 29/06/2023 - Migração S4 - JP - fim
                    plant     = es_bflushdatagen-prodplant
                    stge_loc  = es_bflushdatagen-storageloc
                    batch     = es_bflushdatagen-batch
                    entry_qnt = es_bflushdatagen-backflquant ) TO it_goodsmovements_aux.

    me->zif_boletim_producao~bloqueio_desbloqueio_produtos( EXPORTING i_itens = it_goodsmovements_aux
                                                                      i_acao  = 'B' ).


    CALL FUNCTION 'BAPI_REPMANCONF1_CREATE_MTS'       "#EC CI_USAGE_OK[2438131]
      EXPORTING
        bflushflags    = es_bflushflags
        bflushdatagen  = es_bflushdatagen
      IMPORTING
        confirmation   = es_confirmation
        return         = wa_return
      TABLES
        goodsmovements = it_goodsmovements.

    IF wa_return-type = 'E'.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      me->zif_boletim_producao~bloqueio_desbloqueio_produtos( EXPORTING i_itens = it_goodsmovements_aux
                                                                        i_acao  = 'D' ).

      me->zif_boletim_producao~gera_erro_geral(
        EXPORTING
          i_msgv1              =   CONV #( wa_return-message_v1 )
          i_msgv2              =   CONV #( wa_return-message_v2 )
          i_msgv3              =   CONV #( wa_return-message_v3 )
          i_msgv4              =   CONV #( wa_return-message_v4 )
          i_msgid              =   CONV #( wa_return-id )
          i_msgno              =   CONV #( wa_return-number )  ).

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      DO 100 TIMES.

        SELECT SINGLE *
          FROM blpp INTO gs_blpp
         WHERE prtnr = es_confirmation
           AND prtps = '0001'.

        IF sy-subrc = 0.

          wl_zsdt0252-doc_prod_01     = gs_blpp-belnr.
          wl_zsdt0252-ano_doc_prod_01 = me->zif_boletim_producao~at_cabecalho-dt_lancamento(4).
          MODIFY zsdt0252 FROM wl_zsdt0252.

          COMMIT WORK AND WAIT .

          e_mblnr = gs_blpp-belnr.

          MESSAGE |Documento de Produção: { gs_blpp-belnr } gerado com sucesso!| TYPE 'S'.

          EXIT.

        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.

      ENDDO.

      me->zif_boletim_producao~bloqueio_desbloqueio_produtos( EXPORTING i_itens = it_goodsmovements_aux
                                                                        i_acao  = 'D' ).

    ENDIF.
  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_doc_producao_02.
    TYPES:
      BEGIN OF ty_itens,
        status   TYPE icon-id,
        matnr    TYPE makt-matnr,
        maktx    TYPE makt-maktx,
        meins    TYPE mara-meins,
        quantity TYPE erfmg,
        id       TYPE char10,
        reserva  TYPE bapirkpfc-res_no,
      END OF ty_itens.

    DATA:wl_zsdt0252      TYPE zsdt0252.

    DATA: vg_matnr TYPE char18.

    DATA: v_len TYPE sy-tabix.

    DATA: es_bflushflags     TYPE bapi_rm_flg,
          es_bflushdatagen   TYPE bapi_rm_datgen,
          es_confirmation    TYPE bapi_rm_datkey-confirmation,
          ls_header          TYPE bapi2017_gm_head_01,
          ls_code            TYPE bapi2017_gm_code,
          ls_item            TYPE bapi2017_gm_item_create,
          lt_item            TYPE STANDARD TABLE OF bapi2017_gm_item_create,
          lt_return          TYPE STANDARD TABLE OF bapiret2,
          ls_return          TYPE bapiret2,
          res_docmat         TYPE bapi2017_gm_head_ret-mat_doc,
          gs_blpp            TYPE blpp,
          it_return          TYPE TABLE OF bapiret2,
          wa_return          TYPE bapiret2,
          meins              TYPE meins,
          items_reservation  TYPE TABLE OF bapiresbc,
          t_header           TYPE TABLE OF bapirkpfc,
          w_header           TYPE bapirkpfc,
          return             TYPE TABLE OF bapireturn,
          wa_reserva_change  TYPE bapi2093_res_item_change,
          it_reserva_changex TYPE TABLE OF bapi2093_res_item_changex,
          wa_reserva_changex TYPE bapi2093_res_item_changex,
          it_reserva_return  TYPE TABLE OF bapiret2,
          reserva            TYPE bapirkpfc-res_no,
          it_reserva_change  TYPE TABLE OF bapi2093_res_item_change.

    DATA: it_goodsmovements     TYPE TABLE OF bapi2017_gm_item_create,
          it_goodsmovements_aux TYPE TABLE OF bapi2017_gm_item_create,
          wa_goodsmovements     TYPE bapi2017_gm_item_create,
          it_bapi_char_batch    TYPE TABLE OF bapi_char_batch,
          wa_bapi_char_batch    TYPE bapi_char_batch,
          es_goodsmvt_header    TYPE bapi2017_gm_head_01,
          es_goodsmvt_code      TYPE bapi2017_gm_code,
          saldo                 TYPE menge_d,
          item                  TYPE ty_itens.

    r_if_boletim_producao = me.

    CLEAR: e_mblnr.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_doc_prod_02.

*---------------------------------------------------------------------------------------------------------*
* Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0252 INTO wl_zsdt0252
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ 1.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    IF wl_zsdt0252-doc_prod_02 IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                            attr1 = CONV #( 'Documento Produção 02' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
          msgv1  = CONV #( 'Documento Produção 02' ).
    ELSE.

      me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs = CONV #( _key_lcto )
                                                       IMPORTING e_mblnr    = e_mblnr ).

      IF e_mblnr IS NOT INITIAL.

        wl_zsdt0252-doc_prod_02     = e_mblnr.
        wl_zsdt0252-ano_doc_prod_02 = me->zif_boletim_producao~at_cabecalho-dt_lancamento(4).
        MODIFY zsdt0252 FROM wl_zsdt0252.

        IF sy-subrc NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                                msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                                attr1 = CONV #( 'Documento Produção 02' )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
              msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
              msgv1  = CONV #( 'Documento Produção 02' ).
        ENDIF.

        MESSAGE |Documento de Produção: { e_mblnr } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    "Validar Doc. Prod. 01
    me->zif_boletim_producao~check_doc_material_valido( i_mblnr      = wl_zsdt0252-doc_prod_01
                                                                          i_with_raise = abap_true
                                                                          i_ds_doc     = 'Doc.Produção 01( Filial:' && wl_zsdt0252-branch && ')'
                                                                         ).

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Gerando Documento de Produção 02 ( Filial: { wl_zsdt0252-branch } )|.


**--------------------------------------------------------------------------------------------*
**   Montagem Cabecalho
**--------------------------------------------------------------------------------------------*
    CLEAR: ls_header, lt_item[], ls_item.
    ls_header = VALUE #( pstng_date = sy-datum
                           doc_date = sy-datum
                         ref_doc_no = ''
                         header_txt = _key_lcto
                         pr_uname   = sy-uname ).

    ls_code-gm_code      = '03'.


**--------------------------------------------------------------------------------------------*
**   Montagem Itens
**--------------------------------------------------------------------------------------------*
    FREE: it_goodsmovements_aux.

    "Metanol
    DATA(wl_zsdt0250) = me->zif_boletim_producao~get_material_boletim( i_tp_produto =  'ME').
    me->zif_boletim_producao~get_und_material( EXPORTING i_matnr = wl_zsdt0250-matnr IMPORTING e_meins = meins ).

*****************************************************************Metanol


    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.

    DATA: v_qtde  TYPE zsdt0252-qtde_me_doc_02,
          wa_mara TYPE mara.

*    v_qtde = wl_zsdt0252-qtde_me_doc_02.

*<--- 30/06/2023 - Migração S4 - JP
    v_len = strlen( wl_zsdt0250-matnr ).
    DATA(v_material_long) = wl_zsdt0250-matnr.
    DATA(v_material)      = wl_zsdt0250-matnr.
    IF v_len > 18.
      CLEAR: v_material.
    ELSE.
      CLEAR: v_material_long.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP

*material é controlado por lote(mara) lote na mara


    SELECT * FROM mara
      INTO wa_mara
      WHERE matnr = wl_zsdt0250-matnr
        AND xchpf = 'X'.
    ENDSELECT.

    IF sy-subrc = 0.


      SELECT   matnr,
               werks,
               lgort,
               charg,
               ersda,
               clabs
               FROM mchb
               INTO TABLE @DATA(t_mchb)
               WHERE  matnr = @wl_zsdt0250-matnr
                 AND clabs > 0
                 AND lgort EQ 'IN02'
                 AND werks EQ @wl_zsdt0252-werks_v
               ORDER BY ersda DESCENDING.

      "Check quantidade total em estoque de todos os lotes.
      IF sy-subrc = 0.

        LOOP AT t_mchb INTO DATA(wa_mchb).
          ADD wa_mchb-clabs TO v_qtde.
        ENDLOOP.
*
        IF v_qtde >= wl_zsdt0252-qtde_me_doc_02.
*
          CLEAR: wa_mchb, saldo.

          LOOP AT t_mchb INTO wa_mchb.

            IF wl_zsdt0252-qtde_me_doc_02 EQ 0.
              EXIT.
            ENDIF.

            "Verifica se o lote tem saldo suficiente para quantidade solicitada.
            IF wa_mchb-clabs >= wl_zsdt0252-qtde_me_doc_02.
              saldo = 0.
            ELSE.
              saldo = wl_zsdt0252-qtde_me_doc_02 - wa_mchb-clabs.
              wl_zsdt0252-qtde_me_doc_02 = wa_mchb-clabs.
            ENDIF.

            APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                        material       = wl_zsdt0250-matnr
                         material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                         plant       = wl_zsdt0252-werks_v
                         stge_loc    = 'IN02'"wl_zsdt0252-lgort_v
                         entry_qnt   = wl_zsdt0252-qtde_me_doc_02
                         entry_uom   = meins
                         batch       = wa_mchb-charg
                         move_type   = '201'"wa_mchb-charg
                         costcenter  = '0010550463'         "10550462
                         gl_account  = '0000412048'         "'412048'
                       ) TO lt_item.


            APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                           material       = wl_zsdt0250-matnr
                            material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                            plant     = wl_zsdt0252-werks_v
                            stge_loc  = 'IN02'"wl_zsdt0252-lgort_v
                            entry_qnt = wl_zsdt0252-qtde_me_doc_02 ) TO it_goodsmovements_aux.


            wl_zsdt0252-qtde_me_doc_02 = saldo.
            CLEAR: saldo.

          ENDLOOP.

        ELSE.

          "Retornar erro saldo lote.
          zcl_boletim_producao=>zif_boletim_producao~gera_erro_geral(
          EXPORTING
            i_msgv1              =   | Saldo insuficiente para o { wl_zsdt0250-matnr }|
            i_msgv2              =   CONV #( wa_return-message_v2 )
            i_msgv3              =   CONV #( wa_return-message_v3 )
            i_msgv4              =   CONV #( wa_return-message_v4 )
            i_msgid              =   CONV #( wa_return-id )
            i_msgno              =   CONV #( wa_return-number )  ).
        ENDIF.



      ELSE.
        "Retornar erro saldo lote.
        zcl_boletim_producao=>zif_boletim_producao~gera_erro_geral(
        EXPORTING
          i_msgv1              =   | { wl_zsdt0250-matnr } não possuem lote com saldo |
          i_msgv2              =   CONV #( wa_return-message_v2 )
          i_msgv3              =   CONV #( wa_return-message_v3 )
          i_msgv4              =   CONV #( wa_return-message_v4 )
          i_msgid              =   CONV #( wa_return-id )
          i_msgno              =   CONV #( wa_return-number )  ).
      ENDIF.

    ELSE.

      APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                        material       = wl_zsdt0250-matnr
                         material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                         plant       = wl_zsdt0252-werks_v
                         stge_loc    = 'IN02'"wl_zsdt0252-lgort_v
                         entry_qnt   = wl_zsdt0252-qtde_me_doc_02 "Metanol
                         entry_uom   = meins
                         move_type   = '201'
                         costcenter  = '0010550463'         "10550462
                         gl_account  = '0000412048'         "'412048'
                       ) TO lt_item.

      APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                     material       = wl_zsdt0250-matnr
                      material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                      plant     = wl_zsdt0252-werks_v
                      stge_loc  = 'IN02'"wl_zsdt0252-lgort_v
                      entry_qnt = wl_zsdt0252-qtde_me_doc_02 ) TO it_goodsmovements_aux.

    ENDIF.

*********************************************************"Metilato de sódio
    CLEAR: wl_zsdt0250, saldo, v_qtde.
    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'MS').
    me->zif_boletim_producao~get_und_material( EXPORTING i_matnr = wl_zsdt0250-matnr IMPORTING e_meins = meins ).

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.

*<--- 30/06/2023 - Migração S4 - JP
    v_len = strlen( wl_zsdt0250-matnr ).
    IF v_len > 18.
      CLEAR: v_material.
      v_material_long = wl_zsdt0250-matnr.
    ELSE.
      v_material      = wl_zsdt0250-matnr.
      CLEAR: v_material_long.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP

    SELECT * FROM mara
           INTO @DATA(wa_mara2)
           WHERE matnr = @wl_zsdt0250-matnr
             AND xchpf = 'X'.
    ENDSELECT.

    IF sy-subrc EQ 0.
      "Verifica lote com saldo para material.
      SELECT matnr,
             werks,
             lgort,
             charg,
             ersda,
             clabs
             FROM mchb
             INTO TABLE @DATA(t_mchb2)
             WHERE  matnr = @wl_zsdt0250-matnr
               AND clabs > 0
               AND lgort EQ 'IN02'
               AND werks EQ @wl_zsdt0252-werks_v
             ORDER BY ersda DESCENDING.


      "Check quantidade total em estoque de todos os lotes.
      IF sy-subrc EQ 0.

        LOOP AT t_mchb2 INTO DATA(wa_mchb6).
          ADD wa_mchb6-clabs TO v_qtde.
        ENDLOOP.
*
        IF v_qtde >= wl_zsdt0252-qtde_ms_doc_02.
*
          CLEAR: wa_mchb.
          LOOP AT t_mchb2 INTO wa_mchb.

            IF wl_zsdt0252-qtde_ms_doc_02 EQ 0.
              EXIT.
            ENDIF.

            "Verifica se o lote tem saldo suficiente para quantidade solicitada.
            IF wa_mchb-clabs >= wl_zsdt0252-qtde_ms_doc_02.
              saldo = 0.
            ELSE.
              saldo = wl_zsdt0252-qtde_ms_doc_02 - wa_mchb-clabs.
              wl_zsdt0252-qtde_ms_doc_02 = wa_mchb-clabs.
            ENDIF.


            APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                         material       = wl_zsdt0250-matnr
                          material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                           plant       = wl_zsdt0252-werks_v
                           stge_loc    = 'IN02'"wl_zsdt0252-lgort_v
                           entry_qnt   = wl_zsdt0252-qtde_ms_doc_02 "Metanol
                           entry_uom   = meins
                           batch       = wa_mchb-charg
                           move_type   = '201'"wa_mchb-charg
                           costcenter  = '0010550463'       "10550462
                           gl_account  = '0000412049'       "'412048'
                         ) TO lt_item.

            APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                     material       = wl_zsdt0250-matnr
                      material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                      plant     = wl_zsdt0252-werks_v
                      stge_loc  = 'IN02'"wl_zsdt0252-lgort_v
                      entry_qnt = wl_zsdt0252-qtde_ms_doc_02 ) TO it_goodsmovements_aux.

            wl_zsdt0252-qtde_ms_doc_02 = saldo.

          ENDLOOP.

        ELSE.

          "Retornar erro saldo lote.
          zcl_boletim_producao=>zif_boletim_producao~gera_erro_geral(
          EXPORTING
            i_msgv1              =   | Saldo insuficiente para o { wl_zsdt0250-matnr }|
            i_msgv2              =   CONV #( wa_return-message_v2 )
            i_msgv3              =   CONV #( wa_return-message_v3 )
            i_msgv4              =   CONV #( wa_return-message_v4 )
            i_msgid              =   CONV #( wa_return-id )
            i_msgno              =   CONV #( wa_return-number )  ).

        ENDIF.
      ELSE.
        "Retornar erro saldo lote.
        zcl_boletim_producao=>zif_boletim_producao~gera_erro_geral(
        EXPORTING
          i_msgv1              =   | { wl_zsdt0250-matnr } não possuem lote com saldo |
          i_msgv2              =   CONV #( wa_return-message_v2 )
          i_msgv3              =   CONV #( wa_return-message_v3 )
          i_msgv4              =   CONV #( wa_return-message_v4 )
          i_msgid              =   CONV #( wa_return-id )
          i_msgno              =   CONV #( wa_return-number )  ).
      ENDIF.
    ELSE.

      APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                        material       = wl_zsdt0250-matnr
                         material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                         plant       = wl_zsdt0252-werks_v
                         stge_loc    = 'IN02'"wl_zsdt0252-lgort_v
                         entry_qnt   = wl_zsdt0252-qtde_ms_doc_02 "Metanol
                         entry_uom   = meins
                         move_type   = '201'
                         costcenter  = '0010550463'         "10550462
                         gl_account  = '0000412049'         "'412048'
                       ) TO lt_item.

      APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
*                    material       = wl_zsdt0250-matnr
                     material       = v_material
                     material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                     plant     = wl_zsdt0252-werks_v
                     stge_loc  = 'IN02'"wl_zsdt0252-lgort_v
                     entry_qnt = wl_zsdt0252-qtde_ms_doc_02 ) TO it_goodsmovements_aux.

    ENDIF.


**************************************************************"Acido Citrico
    CLEAR: wl_zsdt0250, saldo, v_qtde.
    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'AC').
    me->zif_boletim_producao~get_und_material( EXPORTING i_matnr = wl_zsdt0250-matnr IMPORTING e_meins = meins ).

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.


*<--- 30/06/2023 - Migração S4 - JP
    v_len = strlen( wl_zsdt0250-matnr ).
    IF v_len > 18.
      CLEAR: v_material.
      v_material_long = wl_zsdt0250-matnr.
    ELSE.
      v_material      = wl_zsdt0250-matnr.
      CLEAR: v_material_long.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP

    SELECT * FROM mara
          INTO @DATA(wa_mara3)
          WHERE matnr = @wl_zsdt0250-matnr
            AND xchpf = 'X'.
    ENDSELECT.

    IF sy-subrc EQ 0.
      "Verifica lote com saldo para material.
      SELECT matnr,
             werks,
             lgort,
             charg,
             ersda,
             clabs
             FROM mchb
             INTO TABLE @DATA(t_mchb4)
             WHERE  matnr = @wl_zsdt0250-matnr
             AND clabs > 0
             AND lgort EQ 'IN02'
             AND werks EQ @wl_zsdt0252-werks_v
             ORDER BY ersda DESCENDING.


      "Check quantidade total em estoque de todos os lotes.
      IF sy-subrc EQ 0.

        LOOP AT t_mchb4 INTO DATA(wa_mchb4).
          ADD wa_mchb4-clabs TO v_qtde.
        ENDLOOP.
*
        IF v_qtde >= wl_zsdt0252-qtde_ac_doc_02.
*
          CLEAR: wa_mchb.
          LOOP AT t_mchb4 INTO wa_mchb.
*
            IF wl_zsdt0252-qtde_ac_doc_02 EQ 0.
              EXIT.
            ENDIF.

            "Verifica se o lote tem saldo suficiente para quantidade solicitada.
            IF wa_mchb-clabs >= wl_zsdt0252-qtde_ac_doc_02.
              saldo = 0.
            ELSE.
              saldo = wl_zsdt0252-qtde_ac_doc_02 - wa_mchb-clabs.
              wl_zsdt0252-qtde_ac_doc_02 = wa_mchb-clabs.
            ENDIF.



            APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                             material       = wl_zsdt0250-matnr
                              material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                              plant       = wl_zsdt0252-werks_v
                              stge_loc    = 'IN02'"wl_zsdt0252-lgort_v
                              entry_qnt   =  wl_zsdt0252-qtde_ac_doc_02
                              entry_uom   = meins
                              batch       = wa_mchb-charg
                              move_type   = '201'"wa_mchb-charg
                              costcenter  = '0010550463'    "10550462
                              gl_account  = '0000412007'    "'412048'
                            ) TO lt_item.

            APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                           material       = wl_zsdt0250-matnr
                            material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                            plant     = wl_zsdt0252-werks_v
                            stge_loc  = 'IN02'"wl_zsdt0252-lgort_v
                            entry_qnt = wl_zsdt0252-qtde_ac_doc_02 ) TO it_goodsmovements_aux.


            wl_zsdt0252-qtde_ac_doc_02 = saldo.
          ENDLOOP.
        ELSE.
          "Retornar erro saldo lote.
          zcl_boletim_producao=>zif_boletim_producao~gera_erro_geral(
          EXPORTING
            i_msgv1              =   | Saldo insuficiente para o { wl_zsdt0250-matnr }|
            i_msgv2              =   CONV #( wa_return-message_v2 )
            i_msgv3              =   CONV #( wa_return-message_v3 )
            i_msgv4              =   CONV #( wa_return-message_v4 )
            i_msgid              =   CONV #( wa_return-id )
            i_msgno              =   CONV #( wa_return-number )  ).

        ENDIF.
      ELSE.

        "Retornar erro saldo lote.
        zcl_boletim_producao=>zif_boletim_producao~gera_erro_geral(
        EXPORTING
          i_msgv1              =   | { wl_zsdt0250-matnr } não possuem lote com saldo |
          i_msgv2              =   CONV #( wa_return-message_v2 )
          i_msgv3              =   CONV #( wa_return-message_v3 )
          i_msgv4              =   CONV #( wa_return-message_v4 )
          i_msgid              =   CONV #( wa_return-id )
          i_msgno              =   CONV #( wa_return-number )  ).
      ENDIF.
    ELSE.


      "Caso não seja contralado por lote.

      APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                           material       = wl_zsdt0250-matnr
                            material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                            plant       = wl_zsdt0252-werks_v
                            stge_loc    = 'IN02'"wl_zsdt0252-lgort_v
                            entry_qnt   = wl_zsdt0252-qtde_ac_doc_02 "Metanol
                            entry_uom   = meins
                            move_type   = '201'
                            costcenter  = '0010550463'      "10550462
                            gl_account  = '0000412007'      "'412048'
                          ) TO lt_item.

      APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                     material       = wl_zsdt0250-matnr
                      material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                      plant     = wl_zsdt0252-werks_v
                      stge_loc  = 'IN02'"wl_zsdt0252-lgort_v
                      entry_qnt = wl_zsdt0252-qtde_ac_doc_02 ) TO it_goodsmovements_aux.

    ENDIF.


**********************************************Soda Caustica
    CLEAR: wl_zsdt0250, saldo, v_qtde.
    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'SC').
    me->zif_boletim_producao~get_und_material( EXPORTING i_matnr = wl_zsdt0250-matnr IMPORTING e_meins = meins ).

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.

*<--- 30/06/2023 - Migração S4 - JP
    v_len = strlen( wl_zsdt0250-matnr ).
    IF v_len > 18.
      CLEAR: v_material.
      v_material_long = wl_zsdt0250-matnr.
    ELSE.
      v_material      = wl_zsdt0250-matnr.
      CLEAR: v_material_long.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP

    SELECT * FROM mara
          INTO @DATA(wa_mara5)
          WHERE matnr = @wl_zsdt0250-matnr
            AND xchpf = 'X'.
    ENDSELECT.

    IF sy-subrc EQ 0.
      "Verifica lote com saldo para material.
      SELECT matnr,
             werks,
             lgort,
             charg,
             ersda,
             clabs
             FROM mchb
             INTO TABLE @DATA(t_mchb5)
             WHERE  matnr = @wl_zsdt0250-matnr
               AND clabs > 0
               AND lgort EQ 'IN02'
               AND werks EQ @wl_zsdt0252-werks_v
             ORDER BY ersda DESCENDING.


      "Check quantidade total em estoque de todos os lotes.
      IF sy-subrc EQ 0.

        LOOP AT t_mchb5 INTO DATA(wa_mchb2).
          ADD wa_mchb2-clabs TO v_qtde.
        ENDLOOP.
*
        IF v_qtde >= wl_zsdt0252-qtde_sc_doc_02.
*
          CLEAR: wa_mchb.
          LOOP AT t_mchb5 INTO wa_mchb.
*
            IF wl_zsdt0252-qtde_sc_doc_02 EQ 0.
              EXIT.
            ENDIF.

            "Verifica se o lote tem saldo suficiente para quantidade solicitada.
            IF wa_mchb-clabs >= wl_zsdt0252-qtde_sc_doc_02.
              saldo = 0.
            ELSE.
              saldo = wl_zsdt0252-qtde_sc_doc_02 - wa_mchb-clabs.
              wl_zsdt0252-qtde_sc_doc_02 = wa_mchb-clabs.
            ENDIF.

            APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                               material       = wl_zsdt0250-matnr
                                material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                                plant       = wl_zsdt0252-werks_v
                                stge_loc    = 'IN02'"wl_zsdt0252-lgort_v
                                entry_qnt   = wl_zsdt0252-qtde_sc_doc_02
                                entry_uom   = meins
                                batch       = wa_mchb-charg
                                move_type   = '201'"wa_mchb-charg
                                costcenter  = '0010550463'  "10550462
                                gl_account  = '0000412007'  "'412048'
                              ) TO lt_item.

            APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                          material       = wl_zsdt0250-matnr
                            material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                            plant     = wl_zsdt0252-werks_v
                            stge_loc  = 'IN02'"wl_zsdt0252-lgort_v
                            entry_qnt = wl_zsdt0252-qtde_sc_doc_02 ) TO it_goodsmovements_aux.

            wl_zsdt0252-qtde_sc_doc_02 = saldo.
          ENDLOOP.
        ELSE.
          "Retornar erro saldo lote.
          zcl_boletim_producao=>zif_boletim_producao~gera_erro_geral(
          EXPORTING
            i_msgv1              =   | Saldo insuficiente para o { wl_zsdt0250-matnr }|
            i_msgv2              =   CONV #( wa_return-message_v2 )
            i_msgv3              =   CONV #( wa_return-message_v3 )
            i_msgv4              =   CONV #( wa_return-message_v4 )
            i_msgid              =   CONV #( wa_return-id )
            i_msgno              =   CONV #( wa_return-number )  ).

        ENDIF.
      ELSE.

        "Retornar erro saldo lote.
        zcl_boletim_producao=>zif_boletim_producao~gera_erro_geral(
        EXPORTING
          i_msgv1              =   | { wl_zsdt0250-matnr } não possuem lote com saldo |
          i_msgv2              =   CONV #( wa_return-message_v2 )
          i_msgv3              =   CONV #( wa_return-message_v3 )
          i_msgv4              =   CONV #( wa_return-message_v4 )
          i_msgid              =   CONV #( wa_return-id )
          i_msgno              =   CONV #( wa_return-number )  ).
      ENDIF.
    ELSE.


      "Caso não seja contralado por lote.
      APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                        material       = wl_zsdt0250-matnr
                         material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                         plant       = wl_zsdt0252-werks_v
                         stge_loc    = 'IN02'"wl_zsdt0252-lgort_v
                         entry_qnt   = wl_zsdt0252-qtde_sc_doc_02 "Metanol
                         entry_uom   = meins
                         move_type   = '201'
                         costcenter  = '0010550463'         "10550462
                         gl_account  = '0000412007'         "'412048'
                       ) TO lt_item.

      APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                     material       = wl_zsdt0250-matnr
                      material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                      plant     = wl_zsdt0252-werks_v
                      stge_loc  = 'IN02'"wl_zsdt0252-lgort_v
                      entry_qnt = wl_zsdt0252-qtde_sc_doc_02 ) TO it_goodsmovements_aux.

    ENDIF.

********************************************************Acido Cloridrico
    CLEAR: wl_zsdt0250, v_qtde, saldo.
    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'AL').
    me->zif_boletim_producao~get_und_material( EXPORTING i_matnr = wl_zsdt0250-matnr IMPORTING e_meins = meins ).

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.

*<--- 30/06/2023 - Migração S4 - JP
    v_len = strlen( wl_zsdt0250-matnr ).
    IF v_len > 18.
      CLEAR: v_material.
      v_material_long = wl_zsdt0250-matnr.
    ELSE.
      v_material      = wl_zsdt0250-matnr.
      CLEAR: v_material_long.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP

    SELECT * FROM mara
             INTO @DATA(wa_mara6)
             WHERE matnr = @wl_zsdt0250-matnr
               AND xchpf = 'X'.
    ENDSELECT.

    IF sy-subrc EQ 0.
      "Verifica lote com saldo para material.
      SELECT matnr,
             werks,
             lgort,
             charg,
             ersda,
             clabs
             FROM mchb
             INTO TABLE @DATA(t_mchb7)
             WHERE  matnr = @wl_zsdt0250-matnr
               AND clabs > 0
               AND lgort EQ 'IN02'
               AND werks EQ @wl_zsdt0252-werks_v
             ORDER BY ersda DESCENDING.


      "Check quantidade total em estoque de todos os lotes.
      IF sy-subrc EQ 0.

        LOOP AT t_mchb7 INTO DATA(wa_mchb3).
          ADD wa_mchb3-clabs TO v_qtde.
        ENDLOOP.
*
        IF v_qtde >= wl_zsdt0252-qtde_al_doc_02.
*
          CLEAR: wa_mchb.
          LOOP AT t_mchb7 INTO wa_mchb.
*
            IF wl_zsdt0252-qtde_al_doc_02 EQ 0.
              EXIT.
            ENDIF.

            "Verifica se o lote tem saldo suficiente para quantidade solicitada.
            IF wa_mchb-clabs >= wl_zsdt0252-qtde_al_doc_02.
              saldo = 0.
            ELSE.
              saldo = wl_zsdt0252-qtde_al_doc_02 - wa_mchb-clabs.
              wl_zsdt0252-qtde_al_doc_02 = wa_mchb-clabs.
            ENDIF.

            APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                     material       = wl_zsdt0250-matnr
                      material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                      plant       = wl_zsdt0252-werks_v
                      stge_loc    = 'IN02'"wl_zsdt0252-lgort_v
                      entry_qnt   =  wl_zsdt0252-qtde_al_doc_02
                      entry_uom   = meins
                      batch       = wa_mchb-charg
                      move_type   = '201'"wa_mchb-charg
                      costcenter  = '0010550463'            "10550462
                      gl_account  = '0000412007'            "'412048'
                    ) TO lt_item.

            APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                           material       = wl_zsdt0250-matnr
                            material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                            plant     = wl_zsdt0252-werks_v
                            stge_loc  = 'IN02'"wl_zsdt0252-lgort_v
                            entry_qnt = wl_zsdt0252-qtde_al_doc_02 ) TO it_goodsmovements_aux.

            wl_zsdt0252-qtde_al_doc_02 = saldo.
          ENDLOOP.
        ELSE.
          "Retornar erro saldo lote.
          zcl_boletim_producao=>zif_boletim_producao~gera_erro_geral(
          EXPORTING
            i_msgv1              =   | Saldo insuficiente para o { wl_zsdt0250-matnr }|
            i_msgv2              =   CONV #( wa_return-message_v2 )
            i_msgv3              =   CONV #( wa_return-message_v3 )
            i_msgv4              =   CONV #( wa_return-message_v4 )
            i_msgid              =   CONV #( wa_return-id )
            i_msgno              =   CONV #( wa_return-number )  ).

        ENDIF.
      ELSE.

        "Retornar erro saldo lote.
        zcl_boletim_producao=>zif_boletim_producao~gera_erro_geral(
        EXPORTING
          i_msgv1              =   | { wl_zsdt0250-matnr } não possuem lote com saldo |
          i_msgv2              =   CONV #( wa_return-message_v2 )
          i_msgv3              =   CONV #( wa_return-message_v3 )
          i_msgv4              =   CONV #( wa_return-message_v4 )
          i_msgid              =   CONV #( wa_return-id )
          i_msgno              =   CONV #( wa_return-number )  ).
      ENDIF.
    ELSE.


      "Caso não seja contralado por lote.
      APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                       material       = wl_zsdt0250-matnr
                        material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                        plant       = wl_zsdt0252-werks_v
                        stge_loc    = 'IN02'"wl_zsdt0252-lgort_v
                        entry_qnt   = wl_zsdt0252-qtde_al_doc_02
                        entry_uom   = meins
                        move_type   = '201'
                        costcenter  = '0010550463'          "10550462
                        gl_account  = '0000412007'          "'412048'
                      ) TO lt_item.


      APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                     material       = wl_zsdt0250-matnr
                      material_long  = v_material_long
*<--- 30/06/2023 - Migração S4 - JP
                      plant     = wl_zsdt0252-werks_v
                      stge_loc  = 'IN02'"wl_zsdt0252-lgort_v
                      entry_qnt = wl_zsdt0252-qtde_al_doc_02 ) TO it_goodsmovements_aux.


    ENDIF.


    me->zif_boletim_producao~bloqueio_desbloqueio_produtos( EXPORTING i_itens = it_goodsmovements_aux
                                                                      i_acao  = 'B' ).

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        goodsmvt_header  = ls_header
        goodsmvt_code    = ls_code
      IMPORTING
        materialdocument = res_docmat
      TABLES
        goodsmvt_item    = lt_item
        return           = lt_return.

    CLEAR: wa_return.
    READ TABLE lt_return INTO wa_return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      me->zif_boletim_producao~bloqueio_desbloqueio_produtos( EXPORTING i_itens = it_goodsmovements_aux
                                                                        i_acao  = 'D' ).

      zcl_boletim_producao=>zif_boletim_producao~gera_erro_geral(
        EXPORTING
          i_msgv1              =   CONV #( wa_return-message_v1 )
          i_msgv2              =   CONV #( wa_return-message_v2 )
          i_msgv3              =   CONV #( wa_return-message_v3 )
          i_msgv4              =   CONV #( wa_return-message_v4 )
          i_msgid              =   CONV #( wa_return-id )
          i_msgno              =   CONV #( wa_return-number )  ).


    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      wl_zsdt0252-doc_prod_02     = res_docmat.
      wl_zsdt0252-ano_doc_prod_02 = me->zif_boletim_producao~at_cabecalho-dt_lancamento(4).
      MODIFY zsdt0252 FROM wl_zsdt0252.

      COMMIT WORK AND WAIT .

      e_mblnr = res_docmat.

      MESSAGE |Documento de Produção: { e_mblnr } gerado com sucesso!| TYPE 'S'.

*      EXIT.

      me->zif_boletim_producao~bloqueio_desbloqueio_produtos( EXPORTING i_itens = it_goodsmovements_aux
                                                                        i_acao  = 'D' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_doc_producao_03.
    DATA: wl_zsdt0252      TYPE zsdt0252.

    DATA: es_bflushflags   TYPE bapi_rm_flg,
          es_bflushdatagen TYPE bapi_rm_datgen,
          es_confirmation  TYPE bapi_rm_datkey-confirmation,
          gs_blpp          TYPE blpp,
          it_return        TYPE TABLE OF bapiret2,
          wa_return        TYPE bapiret2.

    DATA: it_goodsmovements  TYPE TABLE OF bapi2017_gm_item_create,
          wa_goodsmovements  TYPE bapi2017_gm_item_create,
          it_bapi_char_batch TYPE TABLE OF bapi_char_batch,
          wa_bapi_char_batch TYPE bapi_char_batch,
          es_goodsmvt_header TYPE bapi2017_gm_head_01,
          es_goodsmvt_code   TYPE bapi2017_gm_code.

    r_if_boletim_producao = me.

    DATA: VG_MATNR TYPE CHAR18.

    CLEAR: e_mblnr.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_doc_prod_03.

*---------------------------------------------------------------------------------------------------------*
*   Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0252 INTO wl_zsdt0252
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ 1.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    IF wl_zsdt0252-doc_prod_03 IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                            attr1 = CONV #( 'Documento Produção 03' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
          msgv1  = CONV #( 'Documento Produção 03' ).
    ELSE.

      me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs = CONV #( _key_lcto )
                                                       IMPORTING e_mblnr    = e_mblnr ).

      IF e_mblnr IS NOT INITIAL.

        wl_zsdt0252-doc_prod_03     = e_mblnr.
        wl_zsdt0252-ano_doc_prod_03 = me->zif_boletim_producao~at_cabecalho-dt_lancamento(4).
        MODIFY zsdt0252 FROM wl_zsdt0252.

        IF sy-subrc NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                                msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                                attr1 = CONV #( 'Documento Produção 03' )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
              msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
              msgv1  = CONV #( 'Documento Produção 03' ).
        ENDIF.

        MESSAGE |Documento de Produção: { e_mblnr } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    "Validar Doc. Prod. 02
    me->zif_boletim_producao~check_doc_material_valido( i_mblnr = wl_zsdt0252-doc_prod_02
                                                                  i_with_raise = abap_true
                                                                  i_ds_doc     = 'Doc.Produção 02( Filial:' && wl_zsdt0252-branch && ')'
                                                                  ).


    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Gerando Documento de Produção 03 ( Filial: { wl_zsdt0252-branch } )|.

*--------------------------------------------------------------------------------------------*
*   Limpeza Variaveis
*--------------------------------------------------------------------------------------------*
    CLEAR: es_bflushflags, es_bflushdatagen, es_confirmation, it_goodsmovements[], wa_return, it_return[].

*--------------------------------------------------------------------------------------------*
*   Montagem Cabeçalho
*--------------------------------------------------------------------------------------------*

    es_bflushflags-bckfltype        = '01'.

    "Biodiesel
    DATA(wl_zsdt0250) = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'BD').

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.

    es_bflushdatagen-postdate       = me->zif_boletim_producao~at_cabecalho-dt_lancamento. "Data Lançamento
    es_bflushdatagen-docdate        = me->zif_boletim_producao~at_cabecalho-dt_producao.   "Data Documento
    es_bflushdatagen-docheadertxt   = _key_lcto.
    es_bflushdatagen-prodplant      = wl_zsdt0252-werks_v.                                 "Centro

*---> 30/06/2023 - Migração S4 - JP
*   es_bflushdatagen-materialnr     = wl_zsdt0250-matnr.                                   "Material
    DATA: v_len TYPE sy-tabix.
    v_len = strlen( wl_zsdt0250-matnr ).
    IF v_len > 18.
      CLEAR: es_bflushdatagen-materialnr.
      es_bflushdatagen-materialnr_long  = wl_zsdt0250-matnr.
    ELSE.
      es_bflushdatagen-materialnr       = wl_zsdt0250-matnr.
      CLEAR: es_bflushdatagen-materialnr_long.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP

    es_bflushdatagen-backflquant    = wl_zsdt0252-qtde_bd_doc_03.                          "Quantidade

    me->zif_boletim_producao~get_und_material(
      EXPORTING
        i_matnr =     wl_zsdt0250-matnr
      IMPORTING
        e_meins =     es_bflushdatagen-unitofmeasure " Unidade de medida básica
    ).
    es_bflushdatagen-prodversion    = zcl_boletim_producao=>zif_boletim_producao~get_versao( i_matnr = CONV #( wl_zsdt0250-matnr  ) i_werks = CONV #( wl_zsdt0252-werks_v ) ).
*    es_bflushdatagen-prodversion = |{ es_bflushdatagen-prodversion ALPHA = IN }|.                                               "Versão
    es_bflushdatagen-batch          = wl_zsdt0252-charg.      "Lote
    es_bflushdatagen-storageloc     = wl_zsdt0252-lgort_v.    "Deposito "PR01

*--------------------------------------------------------------------------------------------*
*   Montagem Itens
*--------------------------------------------------------------------------------------------*

    "Biodiesel
    CLEAR: wa_goodsmovements.

    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'BD').

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.

*---> 30/06/2023 - Migração S4 - JP
*   wa_goodsmovements-material    = wl_zsdt0250-matnr.                                   "Material Consumo

    v_len = strlen( wl_zsdt0250-matnr ).

    IF v_len > 18.
      wa_goodsmovements-material_long  = wl_zsdt0250-matnr.
    ELSE.
      wa_goodsmovements-material       = wl_zsdt0250-matnr.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP

    wa_goodsmovements-plant         = wl_zsdt0252-werks_v.                                "Centro
    wa_goodsmovements-stge_loc      = me->zif_boletim_producao~at_cabecalho-lgort_prod.   "Deposito "PR01
    wa_goodsmovements-batch         = wl_zsdt0252-charg.                                  "Lote Consumo
    wa_goodsmovements-move_type     = '131'.                                              "Tipo Movimento
    wa_goodsmovements-entry_qnt     = wl_zsdt0252-qtde_bd_doc_03.                         "Quantidade Consumo

    me->zif_boletim_producao~get_und_material(
      EXPORTING
        i_matnr =     wl_zsdt0250-matnr
      IMPORTING
        e_meins =     wa_goodsmovements-entry_uom " Unidade de medida básica
    ).
    APPEND wa_goodsmovements TO it_goodsmovements.

    me->zif_boletim_producao~check_estoque_produtos( i_itens = it_goodsmovements ).




    "Glicerina
    CLEAR: wa_goodsmovements.

    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'GL').

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.

*---> 30/06/2023 - Migração S4 - JP
*   wa_goodsmovements-material    = wl_zsdt0250-matnr.                                   "Material Consumo

    v_len = strlen( wl_zsdt0250-matnr ).

    IF v_len > 18.
      wa_goodsmovements-material_long  = wl_zsdt0250-matnr.
    ELSE.
      wa_goodsmovements-material       = wl_zsdt0250-matnr.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP

    wa_goodsmovements-plant         = wl_zsdt0252-werks_v.                                "Centro
    wa_goodsmovements-stge_loc      = me->zif_boletim_producao~at_cabecalho-lgort_prod.   "Deposito  "PR01
*    wa_goodsmovements-batch         = wl_zsdt0252-charg.                                  "Lote Consumo
    wa_goodsmovements-move_type     = '531'.                                              "Tipo Movimento
    wa_goodsmovements-entry_qnt     = wl_zsdt0252-qtde_gl_doc_03.                         "Quantidade Consumo

    me->zif_boletim_producao~get_und_material(
      EXPORTING
        i_matnr =     wl_zsdt0250-matnr
      IMPORTING
        e_meins =     wa_goodsmovements-entry_uom " Unidade de medida básica
    ).
    APPEND wa_goodsmovements TO it_goodsmovements.

    me->zif_boletim_producao~check_estoque_produtos( i_itens = it_goodsmovements ).


    "Acido Graxo
    CLEAR: wa_goodsmovements.

    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'AG').

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.

*---> 30/06/2023 - Migração S4 - JP
*   wa_goodsmovements-material    = wl_zsdt0250-matnr.                                   "Material Consumo

    v_len = strlen( wl_zsdt0250-matnr ).

    IF v_len > 18.
      wa_goodsmovements-material_long  = wl_zsdt0250-matnr.
    ELSE.
      wa_goodsmovements-material       = wl_zsdt0250-matnr.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP

    wa_goodsmovements-plant         = wl_zsdt0252-werks_v.                                "Centro
    wa_goodsmovements-stge_loc      = me->zif_boletim_producao~at_cabecalho-lgort_prod.   "Deposito  "PR01
*    wa_goodsmovements-batch         = wl_zsdt0252-charg.                                  "Lote Consumo
    wa_goodsmovements-move_type     = '531'.                                              "Tipo Movimento
    wa_goodsmovements-entry_qnt     = wl_zsdt0252-qtde_ag_doc_03.                         "Quantidade Consumo

    me->zif_boletim_producao~get_und_material(
      EXPORTING
        i_matnr =     wl_zsdt0250-matnr
      IMPORTING
        e_meins =     wa_goodsmovements-entry_uom " Unidade de medida básica
    ).
    APPEND wa_goodsmovements TO it_goodsmovements.

    me->zif_boletim_producao~check_estoque_produtos( i_itens = it_goodsmovements ).




    "Oleo Neutro Industrialização
    CLEAR: wa_goodsmovements.

*------------------------Inicio verifica produto terceiro / CS20230000708 / AOENNING
    IF me->zif_boletim_producao~AT_OI_MAT_TERCEIRO EQ ABAP_FALSE.
    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'NI'). "Oleo Neutro Industrializado próprio.
    ELSE.
    wl_zsdt0250       = me->zif_boletim_producao~get_material_boletim( i_tp_produto =  'NT'). "Oleo Neutro Industrializado de terceiro.
    ENDIF.
*------------------------Fim verifica produto terceiro / CS20230000708 / AOENNING

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.

*---> 30/06/2023 - Migração S4 - JP
*   wa_goodsmovements-material    = wl_zsdt0250-matnr.                                   "Material Consumo

    v_len = strlen( wl_zsdt0250-matnr ).

    IF v_len > 18.
      wa_goodsmovements-material_long  = wl_zsdt0250-matnr.
    ELSE.
      wa_goodsmovements-material       = wl_zsdt0250-matnr.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP

    wa_goodsmovements-plant         = wl_zsdt0252-werks_v.                                "Centro
    wa_goodsmovements-stge_loc      = me->zif_boletim_producao~at_cabecalho-lgort_prod.   "Deposito "PR01
*    wa_goodsmovements-batch         = wl_zsdt0252-charg.                                  "Lote Consumo
    wa_goodsmovements-move_type     = '261'.                                              "Tipo Movimento
    wa_goodsmovements-entry_qnt     = wl_zsdt0252-qtde_ni_doc_01.                         "Quantidade Consumo

    me->zif_boletim_producao~get_und_material(
      EXPORTING
        i_matnr =     wl_zsdt0250-matnr
      IMPORTING
        e_meins =     wa_goodsmovements-entry_uom " Unidade de medida básica
    ).
    APPEND wa_goodsmovements TO it_goodsmovements.

    me->zif_boletim_producao~check_estoque_produtos( i_itens = it_goodsmovements ).


    DATA(it_goodsmovements_aux) = it_goodsmovements[].

    APPEND VALUE #(
*<--- 30/06/2023 - Migração S4 - JP
                    material  = es_bflushdatagen-materialnr
                    material_long  = es_bflushdatagen-materialnr_long
*<--- 30/06/2023 - Migração S4 - JP
                    plant     = es_bflushdatagen-prodplant
                    stge_loc  = es_bflushdatagen-storageloc
                    batch     = es_bflushdatagen-batch
                    entry_qnt = es_bflushdatagen-backflquant ) TO it_goodsmovements_aux.

    me->zif_boletim_producao~bloqueio_desbloqueio_produtos( EXPORTING i_itens = it_goodsmovements_aux
                                                                      i_acao  = 'B' ).


    CALL FUNCTION 'BAPI_REPMANCONF1_CREATE_MTS' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        bflushflags    = es_bflushflags
        bflushdatagen  = es_bflushdatagen
      IMPORTING
        confirmation   = es_confirmation
        return         = wa_return
      TABLES
        goodsmovements = it_goodsmovements.

    IF wa_return-type = 'E'.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      me->zif_boletim_producao~bloqueio_desbloqueio_produtos( EXPORTING i_itens = it_goodsmovements_aux
                                                                        i_acao  = 'D' ).

      zcl_boletim_producao=>zif_boletim_producao~gera_erro_geral(
        EXPORTING
          i_msgv1              =   CONV #( wa_return-message_v1 )
          i_msgv2              =   CONV #( wa_return-message_v2 )
          i_msgv3              =   CONV #( wa_return-message_v3 )
          i_msgv4              =   CONV #( wa_return-message_v4 )
          i_msgid              =   CONV #( wa_return-id )
          i_msgno              =   CONV #( wa_return-number )  ).

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      DO 100 TIMES.

        SELECT SINGLE *
          FROM blpp INTO gs_blpp
         WHERE prtnr = es_confirmation
           AND prtps = '0001'.

        IF sy-subrc = 0.

          wl_zsdt0252-doc_prod_03     = gs_blpp-belnr.
          wl_zsdt0252-ano_doc_prod_03 = me->zif_boletim_producao~at_cabecalho-dt_lancamento(4).
          MODIFY zsdt0252 FROM wl_zsdt0252.

          COMMIT WORK AND WAIT .

          e_mblnr = gs_blpp-belnr.

          MESSAGE |Documento de Produção: { gs_blpp-belnr } gerado com sucesso!| TYPE 'S'.

          EXIT.

        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.

      ENDDO.

      me->zif_boletim_producao~bloqueio_desbloqueio_produtos( EXPORTING i_itens = it_goodsmovements_aux
                                                                        i_acao  = 'D' ).

    ENDIF.
  ENDMETHOD.


  method ZIF_BOLETIM_PRODUCAO~GERAR_DOC_PRODUCAO_04.

  endmethod.


  method ZIF_BOLETIM_PRODUCAO~GERAR_DOC_PRODUCAO_05.

  endmethod.


  METHOD zif_boletim_producao~gerar_rateio_produtos.
    DATA: t_zsdt0252 TYPE TABLE OF zsdt0252,
          t_zsdt0247 TYPE TABLE OF zsdt0247,
          t_zsdt0248 TYPE TABLE OF zsdt0248.

    FREE: t_zsdt0252[], t_zsdt0247, t_zsdt0248.

    SELECT *
      FROM zsdt0252 INTO TABLE t_zsdt0252
     WHERE id_boletim EQ me->zif_boletim_producao~at_cabecalho-id_boletim.

    IF t_zsdt0252[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    SELECT * FROM zsdt0247 INTO TABLE t_zsdt0247 WHERE id_boletim EQ me->zif_boletim_producao~at_cabecalho-id_boletim.
    SELECT * FROM zsdt0248 INTO TABLE t_zsdt0248 WHERE id_boletim EQ me->zif_boletim_producao~at_cabecalho-id_boletim.

    "Seta dados de produção.
    LOOP AT t_zsdt0247 ASSIGNING FIELD-SYMBOL(<ws_zsdt0247>).
      LOOP AT t_zsdt0252 ASSIGNING FIELD-SYMBOL(<ws_zsdt0252>) WHERE id_boletim EQ <ws_zsdt0247>-id_boletim.
        CASE <ws_zsdt0247>-tp_produto_producao.
          WHEN 'NI'.
            <ws_zsdt0252>-qtde_ni_doc_01 = <ws_zsdt0247>-qtde_consumo.
            <ws_zsdt0252>-qtde_on_doc_01 = <ws_zsdt0247>-qtde_consumo.
          WHEN 'ME'.
            <ws_zsdt0252>-qtde_me_doc_02 = <ws_zsdt0247>-qtde_consumo.
          WHEN 'MS'.
            <ws_zsdt0252>-qtde_ms_doc_02 = <ws_zsdt0247>-qtde_consumo.
          WHEN 'AC'.
            <ws_zsdt0252>-qtde_ac_doc_02 = <ws_zsdt0247>-qtde_consumo.
          WHEN 'SC'.
            <ws_zsdt0252>-qtde_sc_doc_02 = <ws_zsdt0247>-qtde_consumo.
          WHEN 'AL'.
            <ws_zsdt0252>-qtde_al_doc_02 = <ws_zsdt0247>-qtde_consumo.
          WHEN 'AF'.
            <ws_zsdt0252>-qtde_af_doc_02 = <ws_zsdt0247>-qtde_consumo.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    "Seta dados rendimentos.
    LOOP AT t_zsdt0248 ASSIGNING FIELD-SYMBOL(<ws_zsdt0248>).
      LOOP AT t_zsdt0252 ASSIGNING <ws_zsdt0252> WHERE id_boletim EQ <ws_zsdt0247>-id_boletim.
        CASE <ws_zsdt0248>-tp_produto_producao.
          WHEN 'BD'.
            <ws_zsdt0252>-qtde_bd_doc_03 = <ws_zsdt0248>-qtde.
          WHEN 'GL'.
            <ws_zsdt0252>-qtde_gl_doc_03 = <ws_zsdt0248>-qtde.
          WHEN 'AG'.
            <ws_zsdt0252>-qtde_ag_doc_03 = <ws_zsdt0248>-qtde.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    MODIFY zsdt0252 FROM TABLE t_zsdt0252.
    COMMIT WORK.
  ENDMETHOD.


  METHOD zif_boletim_producao~get_key_docs.
    CLEAR: r_key_docs.

    CASE me->zif_boletim_producao~at_cabecalho-tp_boletim.
      WHEN '02' OR '03'.
        r_key_docs   = 'BP' && i_zsdt0252-id_boletim && i_zsdt0252-branch && '0000' && i_zsdt0252-id_agrp.
      WHEN OTHERS.
        r_key_docs   = 'BP' && i_zsdt0252-id_boletim && i_zsdt0252-branch && i_zsdt0252-charg && i_zsdt0252-id_agrp.
    ENDCASE.

    IF strlen( r_key_docs ) NE 23.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Key Documento(23 Char)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Key Documento(23 Char)' ).

    ENDIF.
  ENDMETHOD.


  method ZIF_BOLETIM_PRODUCAO~GET_PERC_RENDIMENTO.
DATA: V_TOT_PRODUTOS_PROD TYPE ZSDT0247-QTDE_CONSUMO.

    DATA: V_MSG_AUX TYPE C LENGTH 200.

    CLEAR: V_TOT_PRODUTOS_PROD.
    LOOP AT I_ZSDT0247 INTO DATA(WL_ZSDT0247).
      ADD WL_ZSDT0247-QTDE_CONSUMO TO V_TOT_PRODUTOS_PROD.
    ENDLOOP.

    LOOP AT C_ZSDT0248 ASSIGNING FIELD-SYMBOL(<FS_ZSDT0248>).

      V_MSG_AUX = ZCL_UTIL=>GET_DESC_VALUE_DOMAIN(  I_DOMNAME  = 'ZDM_TP_PRODUTO_PRODUCAO'
                                                    I_DOMVALUE = CONV #( <FS_ZSDT0248>-TP_PRODUTO_PRODUCAO ) ).

      CLEAR: <FS_ZSDT0248>-PERC_RENDIMENTO.

      CHECK <FS_ZSDT0248>-QTDE > 0.

      TRY.

        CASE <FS_ZSDT0248>-TP_PRODUTO_PRODUCAO.
          WHEN 'BD'. "Biodiesel

            READ TABLE I_ZSDT0247 INTO WL_ZSDT0247 WITH KEY TP_PRODUTO_PRODUCAO = 'NI'.
            CHECK ( SY-SUBRC EQ 0 ) AND ( WL_ZSDT0247-QTDE_CONSUMO > 0 ).

            <FS_ZSDT0248>-PERC_RENDIMENTO = ( <FS_ZSDT0248>-QTDE / WL_ZSDT0247-QTDE_CONSUMO ) * 100.


          WHEN 'AG'. "Acido graxo

            READ TABLE I_ZSDT0247 INTO WL_ZSDT0247 WITH KEY TP_PRODUTO_PRODUCAO = 'NI'.
            CHECK ( SY-SUBRC EQ 0 ) AND ( WL_ZSDT0247-QTDE_CONSUMO > 0 ).

            <FS_ZSDT0248>-PERC_RENDIMENTO = ( <FS_ZSDT0248>-QTDE / WL_ZSDT0247-QTDE_CONSUMO  ) * 100.

        ENDCASE.

      CATCH CX_SY_ARITHMETIC_OVERFLOW.
        MESSAGE |Não foi possivel calcular o Rendimento % do produto { V_MSG_AUX }! Verificar dados informados!| TYPE 'I'.
        CLEAR:  <FS_ZSDT0248>-QTDE.
      ENDTRY.


    ENDLOOP.

  endmethod.


  METHOD zif_boletim_producao~get_tp_produtos_producao.
    CLEAR: r_tp_produtos[].

    DATA(wl_zsdt0250) = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'NI'). "Óleo neutro industrializado
    APPEND wl_zsdt0250 TO r_tp_produtos.

*    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'ON'). "Oleo neutro
*    APPEND wl_zsdt0250 TO r_tp_produtos.

     wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'ME'). "Metano
    APPEND wl_zsdt0250 TO r_tp_produtos.

     wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'MS'). "Metilato de sodio
    APPEND wl_zsdt0250 TO r_tp_produtos.

     wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'AC'). "Acido citrico
    APPEND wl_zsdt0250 TO r_tp_produtos.

     wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'SC'). "Soda caustica
    APPEND wl_zsdt0250 TO r_tp_produtos.

    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'AL'). "Acido cloridico
    APPEND wl_zsdt0250 TO r_tp_produtos.
  ENDMETHOD.


  method ZIF_BOLETIM_PRODUCAO~GET_TP_PRODUTOS_RENDIMENTO.
    CLEAR: R_TP_PRODUTOS[].

    DATA(WL_ZSDT0250) = ZCL_BOLETIM_PRODUCAO=>ZIF_BOLETIM_PRODUCAO~GET_MATERIAL_BOLETIM( I_TP_PRODUTO =  'BD'). "Biodiesel
    APPEND WL_ZSDT0250 TO R_TP_PRODUTOS.

    WL_ZSDT0250 = ZCL_BOLETIM_PRODUCAO=>ZIF_BOLETIM_PRODUCAO~GET_MATERIAL_BOLETIM( I_TP_PRODUTO =  'GL'). " Glicerina
    APPEND WL_ZSDT0250 TO R_TP_PRODUTOS.

    WL_ZSDT0250 = ZCL_BOLETIM_PRODUCAO=>ZIF_BOLETIM_PRODUCAO~GET_MATERIAL_BOLETIM( I_TP_PRODUTO =  'AG'). "Acido graxo
    APPEND WL_ZSDT0250 TO R_TP_PRODUTOS.

*    WL_ZSDT0250 = ZCL_BOLETIM_PRODUCAO=>ZIF_BOLETIM_PRODUCAO~GET_MATERIAL_BOLETIM( I_TP_PRODUTO =  'NI'). "Óleo neutro industrializado
*    APPEND WL_ZSDT0250 TO R_TP_PRODUTOS.

  endmethod.


  METHOD zif_boletim_producao~gravar_registro.
    DATA: wl_0252      TYPE zsdt0252,
          v_qte_sg     TYPE zsdt0247-qtde_consumo,
          v_perc_dif   TYPE zsdt0247-perc_total,
          v_perc_tol   TYPE zsdt0247-perc_total VALUE 1,
          v_perc       TYPE zsdt0247-perc_total VALUE 100,
          v_perc_total TYPE zsdt0247-perc_total.

    r_if_boletim_producao = me.

    me->zif_boletim_producao~validar_registro( ).

    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZBOL_PROD'
        IMPORTING
          number                  = me->zif_boletim_producao~at_cabecalho-id_boletim
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      IF ( sy-subrc IS NOT INITIAL ) OR ( me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL ).
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_obj_nro_bol_not_found-msgid
                              msgno = zcx_boletim_producao=>zcx_obj_nro_bol_not_found-msgno
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_obj_nro_bol_not_found-msgno
            msgid  = zcx_boletim_producao=>zcx_obj_nro_bol_not_found-msgid.
      ENDIF.

      me->zif_boletim_producao~at_cabecalho-dt_registro     = sy-datum.
      me->zif_boletim_producao~at_cabecalho-hr_registro     = sy-uzeit.
      me->zif_boletim_producao~at_cabecalho-us_registro     = sy-uname.

*      "Determinar Produtos com Destinação Mercado Interno -- Regra Temporario - Ini
*      IF ME->ZIF_BOLETIM_PRODUCAO~AT_CABECALHO-COM_NF EQ ABAP_TRUE.
*        SELECT SINGLE *
*          FROM SETLEAF INTO @DATA(LWA_GERAR_OD)
*         WHERE SETNAME EQ 'ZSDT0170_NOT_GERAR_PROD'
*           AND VALFROM EQ 'OD'.
*
*        IF SY-SUBRC EQ 0.
*          ME->ZIF_BOLETIM_PRODUCAO~AT_CABECALHO-OD_DEST_MI = ABAP_TRUE.
*        ENDIF.
*
*        SELECT SINGLE *
*          FROM SETLEAF INTO @DATA(LWA_GERAR_FH)
*         WHERE SETNAME EQ 'ZSDT0170_NOT_GERAR_PROD'
*           AND VALFROM EQ 'FH'.
*
*        IF SY-SUBRC EQ 0.
*          ME->ZIF_BOLETIM_PRODUCAO~AT_CABECALHO-FH_DEST_MI = ABAP_TRUE.
*        ENDIF.
*
*        SELECT SINGLE *
*          FROM SETLEAF INTO @DATA(LWA_GERAR_FC)
*         WHERE SETNAME EQ 'ZSDT0170_NOT_GERAR_PROD'
*           AND VALFROM EQ 'FC'.
*
*        IF SY-SUBRC EQ 0.
*          ME->ZIF_BOLETIM_PRODUCAO~AT_CABECALHO-FC_DEST_MI = ABAP_TRUE.
*        ENDIF.
*      ENDIF.
      "Determinar Produtos com Destinação Mercado Interno -- Regra Temporario - Fim

    ENDIF.

*----------------------------------------------------------------------------------------------------------*
*   Atribuir ID Boletim
*----------------------------------------------------------------------------------------------------------*
    LOOP AT me->zif_boletim_producao~at_dados_producao ASSIGNING FIELD-SYMBOL(<fs_dados_prod>).
      <fs_dados_prod>-id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim.
    ENDLOOP.

    LOOP AT me->zif_boletim_producao~at_dados_rendimento ASSIGNING FIELD-SYMBOL(<fs_dados_rend>).
      <fs_dados_rend>-id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim.
    ENDLOOP.

*----------------------------------------------------------------------------------------------------------*
*   Gravar Dados
*----------------------------------------------------------------------------------------------------------*

    "Calcular Percentual.
    CLEAR: v_qte_sg, v_perc_total.
    LOOP AT me->zif_boletim_producao~at_dados_producao ASSIGNING <fs_dados_prod>.
      ADD <fs_dados_prod>-qtde_consumo TO v_qte_sg.
    ENDLOOP.

    LOOP AT me->zif_boletim_producao~at_dados_producao ASSIGNING <fs_dados_prod>.
      <fs_dados_prod>-perc_total = ( <fs_dados_prod>-qtde_consumo / v_qte_sg ) * 100.
      ADD <fs_dados_prod>-perc_total TO v_perc_total.
    ENDLOOP.

    CLEAR: v_perc_dif.
    v_perc_dif = ( v_perc_total - v_perc ).

    IF ( v_perc_dif > v_perc_tol ). "Tolerancia percentual 1%.
*    IF ( v_perc_total NE 100 ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                            msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                            attr1 = CONV #( 'Qtde. não rateada em 100%!' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
          msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
          msgv1  = CONV #( 'Qtde. não rateada em 100%!' ).
    ENDIF.

    MODIFY zsdt0246 FROM me->zif_boletim_producao~at_cabecalho.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_gravar_dados-msgid
                            msgno = zcx_boletim_producao=>zcx_error_gravar_dados-msgno
                            attr1 = CONV #( 'ZSDT0246' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_gravar_dados-msgno
          msgid  = zcx_boletim_producao=>zcx_error_gravar_dados-msgid
          msgv1  = CONV #( 'ZSDT0246' ).
    ENDIF.

    MODIFY zsdt0247 FROM TABLE me->zif_boletim_producao~at_dados_producao.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_gravar_dados-msgid
                            msgno = zcx_boletim_producao=>zcx_error_gravar_dados-msgno
                            attr1 = CONV #( 'ZSDT0247' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_gravar_dados-msgno
          msgid  = zcx_boletim_producao=>zcx_error_gravar_dados-msgid
          msgv1  = CONV #( 'ZSDT0247' ).
    ENDIF.

    MODIFY zsdt0248 FROM TABLE me->zif_boletim_producao~at_dados_rendimento.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_gravar_dados-msgid
                            msgno = zcx_boletim_producao=>zcx_error_gravar_dados-msgno
                            attr1 = CONV #( 'ZSDT0248' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_gravar_dados-msgno
          msgid  = zcx_boletim_producao=>zcx_error_gravar_dados-msgid
          msgv1  = CONV #( 'ZSDT0248' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_false.

      CLEAR: wl_0252.

      DELETE FROM zsdt0252 WHERE id_boletim EQ me->zif_boletim_producao~at_cabecalho-id_boletim.

      wl_0252-id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim.
      wl_0252-branch     = me->zif_boletim_producao~at_cabecalho-branch.
      wl_0252-charg      = me->zif_boletim_producao~at_cabecalho-charg.
      wl_0252-werks_v    = me->zif_boletim_producao~at_cabecalho-branch.
      wl_0252-lgort_v    = me->zif_boletim_producao~at_cabecalho-lgort_prod.
      wl_0252-id_agrp    = 1.

      wl_0252-key_docs   = me->zif_boletim_producao~get_key_docs( i_zsdt0252 = wl_0252 ).

      LOOP AT me->zif_boletim_producao~at_dados_producao INTO DATA(wl_dados_prod).
        ADD wl_dados_prod-qtde_consumo TO wl_0252-qtde_vinc.
      ENDLOOP.

      MODIFY zsdt0252 FROM wl_0252.

    ENDIF.

    COMMIT WORK.

    MESSAGE s005 WITH me->zif_boletim_producao~at_cabecalho-id_boletim.

    e_id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim.
  ENDMETHOD.


  METHOD zif_boletim_producao~validar_materiais_doc_prod.

    DATA: it_mseg        TYPE TABLE OF mseg,
          wl_zsdt0252_ck TYPE zsdt0252.

    DATA: v_mblnr       TYPE mkpf-mblnr,
          v_id_doc_prod TYPE c LENGTH 02.

    CLEAR: r_validado, it_mseg[], v_id_doc_prod.

    IF i_mblnr IS NOT INITIAL.

      SELECT SINGLE *
        FROM mkpf INTO @DATA(wl_mkpf)
       WHERE mblnr EQ @i_mblnr.

      CHECK ( sy-subrc EQ 0 ) AND ( strlen( wl_mkpf-bktxt ) EQ 25 ) AND ( wl_mkpf-bktxt(2) EQ 'BP' ). "Documento Boletim Produção

      wl_zsdt0252_ck-id_boletim = wl_mkpf-bktxt+02(10).
      wl_zsdt0252_ck-branch     = wl_mkpf-bktxt+12(04).
      wl_zsdt0252_ck-charg      = wl_mkpf-bktxt+16(04).
      wl_zsdt0252_ck-id_agrp    = wl_mkpf-bktxt+20(03).

      DATA(_ordem_doc_check_str) = wl_mkpf-bktxt+23(02).

      "Validar Materiais Documento Produção
      CASE _ordem_doc_check_str.
        WHEN zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_doc_prod_01.
          v_id_doc_prod = '01'.
        WHEN zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_doc_prod_02.
          v_id_doc_prod = '02'.
        WHEN zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_doc_prod_03.
          v_id_doc_prod = '03'.
        WHEN zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_doc_prod_04.
          v_id_doc_prod = '04'.
        WHEN zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_doc_prod_05.
          v_id_doc_prod = '05'.
      ENDCASE.

      v_mblnr = i_mblnr.

    ELSE.

      v_id_doc_prod = i_id_doc_prod.

      wl_zsdt0252_ck-id_boletim  =  i_zsdt0252-id_boletim.
      wl_zsdt0252_ck-branch      =  i_zsdt0252-branch.
      wl_zsdt0252_ck-charg       =  i_zsdt0252-charg.
      wl_zsdt0252_ck-id_agrp     =  i_zsdt0252-id_agrp.

      CASE v_id_doc_prod.
        WHEN '01'.
          v_mblnr = wl_zsdt0252_ck-doc_prod_01.
        WHEN '02'.
          v_mblnr = wl_zsdt0252_ck-doc_prod_02.
        WHEN '03'.
          v_mblnr = wl_zsdt0252_ck-doc_prod_03.
        WHEN '04'.
          v_mblnr = wl_zsdt0252_ck-doc_prod_04.
        WHEN '05'.
          v_mblnr = wl_zsdt0252_ck-doc_prod_05.
      ENDCASE.

    ENDIF.

    CHECK v_id_doc_prod IS NOT INITIAL.

    IF wl_zsdt0252_ck-charg IS INITIAL OR wl_zsdt0252_ck-charg EQ '0000'.
      SELECT SINGLE *
      FROM zsdt0252 INTO @DATA(wl_zsdt0252)
      WHERE id_boletim EQ @wl_zsdt0252_ck-id_boletim
      AND branch     EQ @wl_zsdt0252_ck-branch
*       AND charg      EQ @wl_zsdt0252_ck-charg
      AND id_agrp    EQ @wl_zsdt0252_ck-id_agrp.
    ELSE.
      CLEAR: wl_zsdt0252.
      SELECT SINGLE *
      FROM zsdt0252 INTO wl_zsdt0252
      WHERE id_boletim EQ wl_zsdt0252_ck-id_boletim
      AND branch       EQ   wl_zsdt0252_ck-branch
      AND charg        EQ   wl_zsdt0252_ck-charg
      AND id_agrp      EQ   wl_zsdt0252_ck-id_agrp.
    ENDIF.


    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    SELECT *
      FROM mseg INTO TABLE it_mseg
     WHERE mblnr EQ v_mblnr.

    CHECK ( it_mseg[] IS NOT INITIAL ) AND ( v_mblnr IS NOT INITIAL ).

    CASE v_id_doc_prod.
      WHEN '01'.

        "Óleo neutro industrializado
        DATA(wl_zsdt0250) = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'NI'). "
        READ TABLE it_mseg INTO DATA(wl_mseg) WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

        "Oleo neutro
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'ON'). "
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

      WHEN '02'.

        "Metano
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'ME').
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

        "Metilato de sodio
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'MS').
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

        "Acido citrico
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'AC').
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

        "Soda caustica
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'SC').
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

        "Acido cloridico
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'AL').
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

      WHEN '03'.
        "Biodiesel
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'BD'). "
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

        "Glicerina
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'GL').
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

        "Acido graxo
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'AG').
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

        "Óleo neutro industrializado
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'NI').
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

      WHEN '04'.

*        "Farelo Comum
*        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'BO').
*        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
*        CHECK sy-subrc EQ 0.

*        "Soja Industrialização
*        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'SI').
*        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
*        CHECK sy-subrc EQ 0.


      WHEN '05'.

*        "Casca Peletizada
*        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'CP').
*        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
*        CHECK sy-subrc EQ 0.
*
*        "Casca Moida
*        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'CM').
*        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
*        CHECK sy-subrc EQ 0.

    ENDCASE.

    r_validado = abap_true.

  ENDMETHOD.


  METHOD zif_boletim_producao~validar_registro.

    DATA: it_produtos_prod TYPE zsdt0250_t,
          it_produtos_rend TYPE zsdt0250_t.

    DATA: v_tot_consumo_prod TYPE zsdt0247-qtde_consumo.

    DATA: v_msg_aux    TYPE c LENGTH 200,
          v_msg_aux_01 TYPE c LENGTH 200.

    r_if_boletim_producao = me.

    it_produtos_prod = me->zif_boletim_producao~get_tp_produtos_producao( ).
    it_produtos_rend = me->zif_boletim_producao~get_tp_produtos_rendimento( ).

*-----------------------------------------------------------------------------------------------------*
*   Dados Boletim Produção
*-----------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-dt_producao IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Data Produção' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Data Produção' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-dt_lancamento IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Data Lançamento' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Data Lançamento' ).
    ENDIF.

*    IF  me->zif_boletim_producao~at_cabecalho-charg IS INITIAL.
*      RAISE EXCEPTION TYPE zcx_boletim_producao
*        EXPORTING
*          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
*                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
*                            attr1 = CONV #( 'Lote' )
*                           )
*          msgty  = 'E'
*          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
*          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
*          msgv1  = CONV #( 'Lote' ).
*    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-branch IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Filial' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0253 INTO @DATA(wl_0253)
     WHERE branch EQ @me->zif_boletim_producao~at_cabecalho-branch.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( |Parâmetro Filial: { me->zif_boletim_producao~at_cabecalho-branch } ZSDT0169| )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( |Parâmetro Filial: { me->zif_boletim_producao~at_cabecalho-branch } ZSDT0169| ).
    ELSE.

*      me->zif_boletim_producao~at_cabecalho-com_nf = wl_0253-emissao_nf.

*      IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_true.
*        me->zif_boletim_producao~at_cabecalho-lgort_prod = 'PR01'.
*      ELSE.
        me->zif_boletim_producao~at_cabecalho-lgort_prod = 'PR01'.
*      ENDIF.

    ENDIF.


    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS NOT INITIAL. "Alterando Boletim
      SELECT SINGLE *
        FROM zsdt0246 INTO @DATA(wl_zsdt0246_old)
       WHERE id_boletim EQ @me->zif_boletim_producao~at_cabecalho-id_boletim.

      IF ( sy-subrc EQ 0 ) AND ( wl_zsdt0246_old-branch NE me->zif_boletim_producao~at_cabecalho-branch ).
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_change_filial-msgid
                              msgno = zcx_boletim_producao=>zcx_change_filial-msgno
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_change_filial-msgno
            msgid  = zcx_boletim_producao=>zcx_change_filial-msgid.
      ENDIF.
    ENDIF.

    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
      ID 'WERKS' FIELD  me->zif_boletim_producao~at_cabecalho-branch
      ID 'ACTVT' FIELD '03'.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_not_acess_filial-msgid
                            msgno = zcx_boletim_producao=>zcx_not_acess_filial-msgno
                            attr1 = CONV #( me->zif_boletim_producao~at_cabecalho-branch  )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_not_acess_filial-msgno
          msgid  = zcx_boletim_producao=>zcx_not_acess_filial-msgid
          msgv1  = CONV #( me->zif_boletim_producao~at_cabecalho-branch ).
    ENDIF.

*    IF me->zif_boletim_producao~at_cabecalho-produto_rem_ind IS INITIAL.
*      RAISE EXCEPTION TYPE zcx_boletim_producao
*        EXPORTING
*          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
*                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
*                            attr1 = CONV #( 'Produto Remessa Industrilização' )
*                           )
*          msgty  = 'E'
*          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
*          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
*          msgv1  = CONV #( 'Produto Remessa Industrilização' ).
*    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS NOT INITIAL.
      me->zif_boletim_producao~check_permissao_modificacao( i_id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim ).
    ENDIF.

*-----------------------------------------------------------------------------------------------------*
*   Dados Produção Soja
*-----------------------------------------------------------------------------------------------------*
    DATA(_dados_prod_inf) = abap_false.
    DATA(_gerar_ni)       = abap_false.
    DATA(_gerar_on)       = abap_false.
    DATA(_gerar_me)       = abap_false.
    DATA(_gerar_ms)       = abap_false.
    DATA(_gerar_ac)       = abap_false.
    DATA(_gerar_sc)       = abap_false.
    DATA(_gerar_al)       = abap_false.

    v_tot_consumo_prod = 0.

    LOOP AT it_produtos_prod INTO DATA(wl_produto_prod).

      v_msg_aux = zcl_util=>get_desc_value_domain(  i_domname  = 'ZDM_TP_PRODUTO_PRODUCAO'
                                                    i_domvalue = CONV #( wl_produto_prod-tp_produto_producao ) ).

      CONCATENATE 'da Produção de Biodiesel( Consumo ' v_msg_aux ')' INTO v_msg_aux SEPARATED BY space.

      READ TABLE me->zif_boletim_producao~at_dados_producao INTO DATA(wl_dados_prod) WITH KEY tp_produto_producao = wl_produto_prod-tp_produto_producao.

      IF ( sy-subrc EQ 0  ) AND ( wl_dados_prod-qtde_consumo GT 0  ).

        _dados_prod_inf = abap_true.

        CASE wl_dados_prod-tp_produto_producao.
          WHEN 'NI'.
            _gerar_ni = abap_true.
          WHEN 'ON'.
            _gerar_on = abap_true.

          WHEN 'ME'.
            _gerar_me = abap_true.

          WHEN 'MS'.
            _gerar_ms = abap_true.

          WHEN 'AC'.
            _gerar_ac = abap_true.

          WHEN 'SC'.
            _gerar_sc = abap_true.

          WHEN 'AL'.
            _gerar_al = abap_true.
        ENDCASE.

        IF ( wl_dados_prod-tp_produto_producao IS INITIAL ) OR
           ( wl_dados_prod-unid_consumo        IS INITIAL ).

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_dados_incompletos-msgid
                                msgno = zcx_boletim_producao=>zcx_dados_incompletos-msgno
                                attr1 = CONV #( v_msg_aux )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_dados_incompletos-msgno
              msgid  = zcx_boletim_producao=>zcx_dados_incompletos-msgid
              msgv1  = CONV #( v_msg_aux ).

        ENDIF.

        ADD wl_dados_prod-qtde_consumo TO v_tot_consumo_prod.

      ENDIF.

    ENDLOOP.

    IF ( _dados_prod_inf EQ abap_false ).
      v_msg_aux = 'Dados Produção de Biodiesel!'.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_dados_incompletos-msgid
                            msgno = zcx_boletim_producao=>zcx_dados_incompletos-msgno
                            attr1 = CONV #( v_msg_aux )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_dados_incompletos-msgno
          msgid  = zcx_boletim_producao=>zcx_dados_incompletos-msgid
          msgv1  = CONV #( v_msg_aux ).
    ENDIF.

    IF  me->zif_boletim_producao~at_cabecalho-qtde_si IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Quantidade Biodiesel Industrialização' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Quantidade Biodiesel Industrialização' ).
    ENDIF.

*-----------------------------------------------------------------------------------------------------*
*   Dados Rendimento
*-----------------------------------------------------------------------------------------------------*
    LOOP AT it_produtos_rend INTO DATA(wl_produto_rend).

      READ TABLE me->zif_boletim_producao~at_dados_rendimento INTO DATA(wl_dados_rend) WITH KEY tp_produto_producao = wl_produto_rend-tp_produto_producao.

      IF ( sy-subrc NE 0 ).

        v_msg_aux = zcl_util=>get_desc_value_domain(  i_domname  = 'ZDM_TP_PRODUTO_PRODUCAO'
                                                      i_domvalue = CONV #( wl_produto_rend-tp_produto_producao ) ).

        CONCATENATE 'do Rendimento( Produto ' v_msg_aux ')' INTO v_msg_aux SEPARATED BY space.

        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_dados_incompletos-msgid
                              msgno = zcx_boletim_producao=>zcx_dados_incompletos-msgno
                              attr1 = CONV #( v_msg_aux )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_dados_incompletos-msgno
            msgid  = zcx_boletim_producao=>zcx_dados_incompletos-msgid
            msgv1  = CONV #( v_msg_aux ).
      ENDIF.

    ENDLOOP.



    LOOP AT me->zif_boletim_producao~at_dados_rendimento INTO wl_dados_rend.

      DATA(_erro) = abap_false.

      CASE wl_dados_rend-tp_produto_producao.
        WHEN 'BD'. "Biodiesel
          CHECK _gerar_ni EQ abap_true.
        WHEN 'GL'. " Glicerina
          CHECK _gerar_on EQ abap_true.

        WHEN 'AG'. "Acido graxo
          CHECK _gerar_me EQ abap_true.

        WHEN 'NI'. "Óleo neutro industrializado
          CHECK _gerar_ms EQ abap_true.

        WHEN OTHERS.
*          CHECK ( wl_dados_rend-tp_produto_producao NE 'RS' ) AND  "Residuo de Soja.
*                ( wl_dados_rend-tp_produto_producao NE 'CP' ) AND  "Casca Peletizada
*                ( wl_dados_rend-tp_produto_producao NE 'CM' ).     "Casca Moida
      ENDCASE.

      IF ( wl_dados_rend-qtde                LE 0       ) OR
*         ( wl_dados_rend-perc_rendimento     LE 0       ) OR
         ( wl_dados_rend-tp_produto_producao IS INITIAL ) OR
         ( wl_dados_rend-unid                IS INITIAL ).
        _erro = abap_true.
      ENDIF.

      v_msg_aux = zcl_util=>get_desc_value_domain(  i_domname  = 'ZDM_TP_PRODUTO_PRODUCAO'
                                                    i_domvalue = CONV #( wl_dados_rend-tp_produto_producao ) ).

      CONCATENATE 'do Rendimento -> Produto'  '-' v_msg_aux INTO v_msg_aux SEPARATED BY space.

      IF _erro = abap_true.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_dados_incompletos-msgid
                              msgno = zcx_boletim_producao=>zcx_dados_incompletos-msgno
                              attr1 = CONV #( v_msg_aux )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_dados_incompletos-msgno
            msgid  = zcx_boletim_producao=>zcx_dados_incompletos-msgid
            msgv1  = CONV #( v_msg_aux ).
      ENDIF.

*  Mercado Interno
      IF  wl_dados_rend-qtde < wl_dados_rend-qtde_mi.

        CLEAR  v_msg_aux.
        v_msg_aux = zcl_util=>get_desc_value_domain(  i_domname  = 'ZDM_TP_PRODUTO_PRODUCAO'
                                                           i_domvalue = CONV #( wl_dados_rend-tp_produto_producao ) ).


        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_dados_incompletos-msgid
                              msgno = '040'
                              attr1 = CONV #( v_msg_aux )
                             )
            msgty  = 'E'
            msgno  = '040'
            msgid  = zcx_boletim_producao=>zcx_dados_incompletos-msgid
            msgv1  = CONV #( v_msg_aux ).
      ENDIF.


    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
