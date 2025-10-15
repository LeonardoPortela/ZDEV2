class ZCL_BOLETIM_PRODUCAO_01 definition
  public
  inheriting from ZCL_BOLETIM_PRODUCAO
  final
  create public .

public section.

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
  methods ZIF_BOLETIM_PRODUCAO~GET_PERC_RENDIMENTO
    redefinition .
  methods ZIF_BOLETIM_PRODUCAO~GET_TP_PRODUTOS_PRODUCAO
    redefinition .
  methods ZIF_BOLETIM_PRODUCAO~GET_TP_PRODUTOS_RENDIMENTO
    redefinition .
  methods ZIF_BOLETIM_PRODUCAO~VALIDAR_MATERIAIS_DOC_PROD
    redefinition .
  methods ZIF_BOLETIM_PRODUCAO~GERAR_RATEIO_PRODUTOS
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BOLETIM_PRODUCAO_01 IMPLEMENTATION.


  METHOD zif_boletim_producao~gerar_doc_producao_01.

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

    DATA: v_len TYPE sy-tabix.

    es_bflushflags-bckfltype        = '01'.

    "Soja Industrialização
    DATA(wl_zsdt0250) = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'SI').

    es_bflushdatagen-postdate       = me->zif_boletim_producao~at_cabecalho-dt_lancamento. "Data Lançamento
    es_bflushdatagen-docdate        = me->zif_boletim_producao~at_cabecalho-dt_producao.   "Data Documento
    es_bflushdatagen-docheadertxt   = _key_lcto.
    es_bflushdatagen-prodplant      = wl_zsdt0252-werks_v.                                 "Centro

    "==========================================================Ajuste projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.

    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==========================================================ajuste projeto sap rise / aoenning.

    "---> 30/06/2023 - migração s4 - jp
    es_bflushdatagen-materialnr     = wl_zsdt0250-matnr.                                   "Material
    v_len = strlen( wl_zsdt0250-matnr ).
    CLEAR: es_bflushdatagen-materialnr, es_bflushdatagen-materialnr_long.
    IF v_len > 18.
      es_bflushdatagen-materialnr_long  = wl_zsdt0250-matnr.
    ELSE.
      es_bflushdatagen-materialnr       = wl_zsdt0250-matnr.
    ENDIF.
    "<--- 30/06/2023 - migração s4 - jp




    es_bflushdatagen-backflquant    = wl_zsdt0252-qtde_si_doc_01.                          "Quantidade
    es_bflushdatagen-unitofmeasure  = 'KG'.
    me->zif_boletim_producao~get_und_material( EXPORTING i_matnr =  wl_zsdt0250-matnr IMPORTING e_meins = es_bflushdatagen-unitofmeasure ).                                                "UM
    es_bflushdatagen-prodversion    = zcl_boletim_producao=>zif_boletim_producao~get_versao( i_matnr = CONV #( wl_zsdt0250-matnr  ) i_werks = CONV #( wl_zsdt0252-werks_v ) ).
    es_bflushdatagen-batch          = wl_zsdt0252-charg.                                   "Lote
    es_bflushdatagen-storageloc     = me->zif_boletim_producao~at_cabecalho-lgort_prod.    "Deposito

*--------------------------------------------------------------------------------------------*
*   Montagem Itens
*--------------------------------------------------------------------------------------------*

    "Soja em Grãos
    CLEAR: wa_goodsmovements.

    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'SG').

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.


*---> 30/06/2023 - Migração S4 - JP
    wa_goodsmovements-material      = wl_zsdt0250-matnr.                                   "Material
    v_len = strlen( wl_zsdt0250-matnr ).
    CLEAR: wa_goodsmovements-material, wa_goodsmovements-material_long.
    IF v_len > 18.
      wa_goodsmovements-material_long  = wl_zsdt0250-matnr.
    ELSE.
      wa_goodsmovements-material       = wl_zsdt0250-matnr.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP


    wa_goodsmovements-plant         = wl_zsdt0252-werks_v.        "Centro
    wa_goodsmovements-stge_loc      = wl_zsdt0252-lgort_v.        "Deposito Consumo
    wa_goodsmovements-batch         = wl_zsdt0252-charg.          "Lote Consumo
    wa_goodsmovements-move_type     = '261'.                      "Tipo Movimento
    wa_goodsmovements-entry_qnt     = wl_zsdt0252-qtde_sg_doc_01. "Quantidade Consumo
    wa_goodsmovements-entry_uom     = 'KG'.
    APPEND wa_goodsmovements TO it_goodsmovements.

    "Casca Moida
    IF wl_zsdt0252-qtde_cm_doc_01 > 0.

      CLEAR: wa_goodsmovements.

      wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'CM').

      "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
      CLEAR: vg_matnr.
      wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
      vg_matnr = wl_zsdt0250-matnr.
      vg_matnr = |{ vg_matnr ALPHA = IN }|.
      CLEAR: wl_zsdt0250-matnr.
      wl_zsdt0250-matnr = vg_matnr.
      "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.


*---> 30/06/2023 - Migração S4 - JP
      wa_goodsmovements-material      = wl_zsdt0250-matnr.          "Material Consumo
      v_len = strlen( wl_zsdt0250-matnr ).
      CLEAR: wa_goodsmovements-material, wa_goodsmovements-material_long.
      IF v_len > 18.
        wa_goodsmovements-material_long  = wl_zsdt0250-matnr.
      ELSE.
        wa_goodsmovements-material       = wl_zsdt0250-matnr.
      ENDIF.
*<--- 30/06/2023 - Migração S4 - JP


      wa_goodsmovements-plant         = wl_zsdt0252-werks_v.        "Centro
      wa_goodsmovements-stge_loc      = 'PR01'.                     "Deposito Consumo
      wa_goodsmovements-batch         = wl_zsdt0252-charg.          "Lote Consumo
      wa_goodsmovements-move_type     = '531'.                      "Tipo Movimento
      wa_goodsmovements-entry_qnt     = wl_zsdt0252-qtde_cm_doc_01. "Quantidade Consumo
      wa_goodsmovements-entry_uom     = 'KG'.
      APPEND wa_goodsmovements TO it_goodsmovements.

    ENDIF.

    "Residuo de Soja
    IF wl_zsdt0252-qtde_rs_doc_01 > 0.

      CLEAR: wa_goodsmovements.

      wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'RS').

      "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
      CLEAR: vg_matnr.
      wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
      vg_matnr = wl_zsdt0250-matnr.
      vg_matnr = |{ vg_matnr ALPHA = IN }|.
      CLEAR: wl_zsdt0250-matnr.
      wl_zsdt0250-matnr = vg_matnr.
      "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.


*---> 30/06/2023 - Migração S4 - JP
      wa_goodsmovements-material      = wl_zsdt0250-matnr.          "Material Consumo
      v_len = strlen( wl_zsdt0250-matnr ).
      CLEAR: wa_goodsmovements-material, wa_goodsmovements-material_long.
      IF v_len > 18.
        wa_goodsmovements-material_long  = wl_zsdt0250-matnr.
      ELSE.
        wa_goodsmovements-material       = wl_zsdt0250-matnr.
      ENDIF.
*<--- 30/06/2023 - Migração S4 - JP


      wa_goodsmovements-plant         = wl_zsdt0252-werks_v.        "Centro
      wa_goodsmovements-stge_loc      = 'PR01'.                     "Deposito Consumo
      wa_goodsmovements-batch         = wl_zsdt0252-charg.          "Lote Consumo
      wa_goodsmovements-move_type     = '531'.                      "Tipo Movimento
      wa_goodsmovements-entry_qnt     = wl_zsdt0252-qtde_rs_doc_01. "Quantidade Consumo
      wa_goodsmovements-entry_uom     = 'KG'.                       "UM Consumo
      APPEND wa_goodsmovements TO it_goodsmovements.

    ENDIF.

    me->zif_boletim_producao~check_estoque_produtos( i_itens = it_goodsmovements ).

    DATA(it_goodsmovements_aux) = it_goodsmovements[].

    "==========================================================Ajuste projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    es_bflushdatagen-materialnr = |{ es_bflushdatagen-materialnr ALPHA = OUT }|.
    vg_matnr = es_bflushdatagen-materialnr.

    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    es_bflushdatagen-materialnr  = vg_matnr.
*      wa_goodsmovements-material     = vg_matnr.                                   "Material
    "==========================================================Ajuste projeto SAP RISE / AOENNING.

    APPEND VALUE #(
                    material       = es_bflushdatagen-materialnr
*---> 30/06/2023 - Migração S4 - JP
                    material_long  = es_bflushdatagen-materialnr_long
*---> 30/06/2023 - Migração S4 - JP
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

    CLEAR: e_mblnr.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_doc_prod_02.

    DATA: vg_matnr TYPE char18.

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

*--------------------------------------------------------------------------------------------*
*   Limpeza Variaveis
*--------------------------------------------------------------------------------------------*
    CLEAR: es_bflushflags, es_bflushdatagen, es_confirmation, it_goodsmovements[], wa_return, it_return[].

*--------------------------------------------------------------------------------------------*
*   Montagem Cabeçalho
*--------------------------------------------------------------------------------------------*

    es_bflushflags-bckfltype        = '01'.

    "Oleo Degomado
    DATA(wl_zsdt0250) = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'OD').

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
    DATA(v_len) = strlen( wl_zsdt0250-matnr ).
    CLEAR: es_bflushdatagen-materialnr, es_bflushdatagen-materialnr_long.
    IF v_len > 18.
      es_bflushdatagen-materialnr_long  = wl_zsdt0250-matnr.
    ELSE.
      es_bflushdatagen-materialnr       = wl_zsdt0250-matnr.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP



    es_bflushdatagen-materialnr     = vg_matnr .
    es_bflushdatagen-backflquant    = wl_zsdt0252-qtde_od_doc_02.                          "Quantidade
    es_bflushdatagen-unitofmeasure  = 'KG'.                                                "UM
    es_bflushdatagen-prodversion    = zcl_boletim_producao=>zif_boletim_producao~get_versao( i_matnr = CONV #( wl_zsdt0250-matnr  ) i_werks = CONV #( wl_zsdt0252-werks_v ) ).                                                 "Versão
    es_bflushdatagen-batch          = wl_zsdt0252-charg.                                   "Lote
    es_bflushdatagen-storageloc     = 'PR01'.                                              "Deposito

*--------------------------------------------------------------------------------------------*
*   Montagem Itens
*--------------------------------------------------------------------------------------------*

    "Soja Industrialização
    CLEAR: wa_goodsmovements.

    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'SI').

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.

*---> 30/06/2023 - Migração S4 - JP
    wa_goodsmovements-material      = wl_zsdt0250-matnr.                                  "Material Consumo
    v_len = strlen( wl_zsdt0250-matnr ).
    CLEAR: wa_goodsmovements-material, wa_goodsmovements-material_long.
    IF v_len > 18.
      wa_goodsmovements-material_long  = wl_zsdt0250-matnr.
    ELSE.
      wa_goodsmovements-material       = wl_zsdt0250-matnr.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP

    wa_goodsmovements-material      = vg_matnr.
    wa_goodsmovements-plant         = wl_zsdt0252-werks_v.                                "Centro
    wa_goodsmovements-stge_loc      = me->zif_boletim_producao~at_cabecalho-lgort_prod.   "Deposito
    wa_goodsmovements-batch         = wl_zsdt0252-charg.                                  "Lote Consumo
    wa_goodsmovements-move_type     = '261'.                                              "Tipo Movimento
    wa_goodsmovements-entry_qnt     = wl_zsdt0252-qtde_si_doc_02.                         "Quantidade Consumo
    wa_goodsmovements-entry_uom     = 'KG'.                                               "UM Consumo
    APPEND wa_goodsmovements TO it_goodsmovements.

    me->zif_boletim_producao~check_estoque_produtos( i_itens = it_goodsmovements ).

    DATA(it_goodsmovements_aux) = it_goodsmovements[].

    APPEND VALUE #( material  = es_bflushdatagen-materialnr
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

          wl_zsdt0252-doc_prod_02     = gs_blpp-belnr.
          wl_zsdt0252-ano_doc_prod_02 = me->zif_boletim_producao~at_cabecalho-dt_lancamento(4).
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

    DATA: vg_matnr TYPE char18.


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

    CHECK wl_zsdt0252-qtde_fh_doc_03 IS NOT INITIAL.        "US 72963

    "Validar Doc. Prod. 02
    me->zif_boletim_producao~check_doc_material_valido( i_mblnr      = wl_zsdt0252-doc_prod_02
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

    "Farelo Hipro
    DATA(wl_zsdt0250) = me->zif_boletim_producao~get_material_boletim( i_tp_produto =  'FH').

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
    DATA(v_len) = strlen( wl_zsdt0250-matnr ).
    CLEAR: es_bflushdatagen-materialnr, es_bflushdatagen-materialnr_long.
    IF v_len > 18.
      es_bflushdatagen-materialnr_long  = wl_zsdt0250-matnr.
    ELSE.
      es_bflushdatagen-materialnr       = wl_zsdt0250-matnr.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP

    es_bflushdatagen-materialnr     = vg_matnr.
    es_bflushdatagen-backflquant    = wl_zsdt0252-qtde_fh_doc_03.                          "Quantidade
    es_bflushdatagen-unitofmeasure  = 'KG'.                                                "UM
    es_bflushdatagen-prodversion    = zcl_boletim_producao=>zif_boletim_producao~get_versao( i_matnr = CONV #( wl_zsdt0250-matnr ) i_werks = CONV #( wl_zsdt0252-werks_v ) ).                                                 "Versão
    es_bflushdatagen-batch          = wl_zsdt0252-charg.                                   "Lote
    es_bflushdatagen-storageloc     = 'PR01'.                                              "Deposito

*--------------------------------------------------------------------------------------------*
*   Montagem Itens
*--------------------------------------------------------------------------------------------*

    "Soja Industrialização
    CLEAR: wa_goodsmovements.

    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'SI').

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.


*---> 30/06/2023 - Migração S4 - JP
   wa_goodsmovements-material      = wl_zsdt0250-matnr.                                  "Material Consumo
    v_len = strlen( wl_zsdt0250-matnr ).
    CLEAR: wa_goodsmovements-material, wa_goodsmovements-material_long.
    IF v_len > 18.
      wa_goodsmovements-material_long  = wl_zsdt0250-matnr.
    ELSE.
      wa_goodsmovements-material       = wl_zsdt0250-matnr.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP


    wa_goodsmovements-material      = vg_matnr.
    wa_goodsmovements-plant         = wl_zsdt0252-werks_v.                                "Centro
    wa_goodsmovements-stge_loc      = me->zif_boletim_producao~at_cabecalho-lgort_prod.   "Deposito
    wa_goodsmovements-batch         = wl_zsdt0252-charg.                                  "Lote Consumo
    wa_goodsmovements-move_type     = '261'.                                              "Tipo Movimento
    wa_goodsmovements-entry_qnt     = wl_zsdt0252-qtde_si_doc_03.                         "Quantidade Consumo
    wa_goodsmovements-entry_uom     = 'KG'.                                               "UM Consumo
    APPEND wa_goodsmovements TO it_goodsmovements.

    me->zif_boletim_producao~check_estoque_produtos( i_itens = it_goodsmovements ).

    DATA(it_goodsmovements_aux) = it_goodsmovements[].

    APPEND VALUE #( material  = es_bflushdatagen-materialnr
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
DATA: WL_ZSDT0252      TYPE ZSDT0252.

    DATA: ES_BFLUSHFLAGS   TYPE BAPI_RM_FLG,
          ES_BFLUSHDATAGEN TYPE BAPI_RM_DATGEN,
          ES_CONFIRMATION  TYPE BAPI_RM_DATKEY-CONFIRMATION,
          GS_BLPP          TYPE BLPP,
          IT_RETURN        TYPE TABLE OF BAPIRET2,
          WA_RETURN        TYPE BAPIRET2.

    DATA: IT_GOODSMOVEMENTS  TYPE TABLE OF BAPI2017_GM_ITEM_CREATE,
          WA_GOODSMOVEMENTS  TYPE BAPI2017_GM_ITEM_CREATE,
          IT_BAPI_CHAR_BATCH TYPE TABLE OF BAPI_CHAR_BATCH,
          WA_BAPI_CHAR_BATCH TYPE BAPI_CHAR_BATCH,
          ES_GOODSMVT_HEADER TYPE BAPI2017_GM_HEAD_01,
          ES_GOODSMVT_CODE   TYPE BAPI2017_GM_CODE.

    R_IF_BOLETIM_PRODUCAO = ME.

    DATA: VG_MATNR TYPE CHAR18.

    CLEAR: E_MBLNR.

    DATA(_KEY_LCTO) = I_ZSDT0252-KEY_DOCS && ME->ZIF_BOLETIM_PRODUCAO~AT_ID_ORDEM_DOC_PROD_04.

*---------------------------------------------------------------------------------------------------------*
*   Validações
*---------------------------------------------------------------------------------------------------------*
    IF ME->ZIF_BOLETIM_PRODUCAO~AT_CABECALHO-ID_BOLETIM IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_BOLETIM_PRODUCAO
         EXPORTING
           TEXTID = VALUE #( MSGID = ZCX_BOLETIM_PRODUCAO=>ZCX_DATA_NOT_INFORMED-MSGID
                             MSGNO = ZCX_BOLETIM_PRODUCAO=>ZCX_DATA_NOT_INFORMED-MSGNO
                             ATTR1 = CONV #( 'Id. Boletim' )
                            )
           MSGTY  = 'E'
           MSGNO  = ZCX_BOLETIM_PRODUCAO=>ZCX_DATA_NOT_INFORMED-MSGNO
           MSGID  = ZCX_BOLETIM_PRODUCAO=>ZCX_DATA_NOT_INFORMED-MSGID
           MSGV1 = CONV #( 'Id. Boletim' ).
    ENDIF.

    SELECT SINGLE *
      FROM ZSDT0252 INTO WL_ZSDT0252
     WHERE ID_BOLETIM EQ I_ZSDT0252-ID_BOLETIM
       AND BRANCH     EQ I_ZSDT0252-BRANCH
       AND CHARG      EQ I_ZSDT0252-CHARG
       AND ID_AGRP    EQ 1.

    IF SY-SUBRC NE 0.
      RAISE EXCEPTION TYPE ZCX_BOLETIM_PRODUCAO
       EXPORTING
         TEXTID = VALUE #( MSGID = ZCX_BOLETIM_PRODUCAO=>ZCX_DATA_NOT_FOUND-MSGID
                           MSGNO = ZCX_BOLETIM_PRODUCAO=>ZCX_DATA_NOT_FOUND-MSGNO
                           ATTR1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                          )
         MSGTY  = 'E'
         MSGNO  = ZCX_BOLETIM_PRODUCAO=>ZCX_DATA_NOT_FOUND-MSGNO
         MSGID  = ZCX_BOLETIM_PRODUCAO=>ZCX_DATA_NOT_FOUND-MSGID
         MSGV1 = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    IF WL_ZSDT0252-DOC_PROD_04 IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_BOLETIM_PRODUCAO
         EXPORTING
           TEXTID = VALUE #( MSGID = ZCX_BOLETIM_PRODUCAO=>ZCX_DOCUMENT_GENERATE-MSGID
                             MSGNO = ZCX_BOLETIM_PRODUCAO=>ZCX_DOCUMENT_GENERATE-MSGNO
                             ATTR1 = CONV #( 'Documento Produção 04' )
                            )
           MSGTY  = 'E'
           MSGNO  = ZCX_BOLETIM_PRODUCAO=>ZCX_DOCUMENT_GENERATE-MSGNO
           MSGID  = ZCX_BOLETIM_PRODUCAO=>ZCX_DOCUMENT_GENERATE-MSGID
           MSGV1 = CONV #( 'Documento Produção 04' ).
    ELSE.

      ME->ZIF_BOLETIM_PRODUCAO~GET_DOC_BOLETIM_VALIDO( EXPORTING I_KEY_DOCS = CONV #( _KEY_LCTO )
                                                       IMPORTING E_MBLNR    = E_MBLNR ).

      IF E_MBLNR IS NOT INITIAL.

        WL_ZSDT0252-DOC_PROD_04     = E_MBLNR.
        WL_ZSDT0252-ANO_DOC_PROD_04 = ME->ZIF_BOLETIM_PRODUCAO~AT_CABECALHO-DT_LANCAMENTO(4).
        MODIFY ZSDT0252 FROM WL_ZSDT0252.

        IF SY-SUBRC NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE ZCX_BOLETIM_PRODUCAO
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_BOLETIM_PRODUCAO=>ZCX_ERROR_GENERATE_DOC-MSGID
                                MSGNO = ZCX_BOLETIM_PRODUCAO=>ZCX_ERROR_GENERATE_DOC-MSGNO
                                ATTR1 = CONV #( 'Documento Produção 04' )
                               )
              MSGTY  = 'E'
              MSGNO  = ZCX_BOLETIM_PRODUCAO=>ZCX_ERROR_GENERATE_DOC-MSGNO
              MSGID  = ZCX_BOLETIM_PRODUCAO=>ZCX_ERROR_GENERATE_DOC-MSGID
              MSGV1 = CONV #( 'Documento Produção 04' ).
        ENDIF.

        MESSAGE |Documento de Produção: { E_MBLNR } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    CHECK  WL_ZSDT0252-QTDE_FC_DOC_04 IS NOT INITIAL. "US 72963

    "Validar Doc. Prod. 02
    ME->ZIF_BOLETIM_PRODUCAO~CHECK_DOC_MATERIAL_VALIDO( I_MBLNR      = WL_ZSDT0252-DOC_PROD_02
                                                                          I_WITH_RAISE = ABAP_TRUE
                                                                          I_DS_DOC     = 'Doc.Produção 02( Filial:' && WL_ZSDT0252-BRANCH && ')'
                                                                         ).


    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = SY-TABIX
        TEXT       = |Gerando Documento de Produção 04 ( Filial: { WL_ZSDT0252-BRANCH } )|.

*--------------------------------------------------------------------------------------------*
*   Limpeza Variaveis
*--------------------------------------------------------------------------------------------*
    CLEAR: ES_BFLUSHFLAGS, ES_BFLUSHDATAGEN, ES_CONFIRMATION, IT_GOODSMOVEMENTS[], WA_RETURN, IT_RETURN[].

*--------------------------------------------------------------------------------------------*
*   Montagem Cabeçalho
*--------------------------------------------------------------------------------------------*

    ES_BFLUSHFLAGS-BCKFLTYPE        = '01'.

    "Farelo Comum
    DATA(WL_ZSDT0250) = ZCL_BOLETIM_PRODUCAO=>ZIF_BOLETIM_PRODUCAO~GET_MATERIAL_BOLETIM( I_TP_PRODUTO =  'FC').

    ES_BFLUSHDATAGEN-POSTDATE       = ME->ZIF_BOLETIM_PRODUCAO~AT_CABECALHO-DT_LANCAMENTO. "Data Lançamento
    ES_BFLUSHDATAGEN-DOCDATE        = ME->ZIF_BOLETIM_PRODUCAO~AT_CABECALHO-DT_PRODUCAO.   "Data Documento
    ES_BFLUSHDATAGEN-DOCHEADERTXT   = _KEY_LCTO.
    ES_BFLUSHDATAGEN-PRODPLANT      = WL_ZSDT0252-WERKS_V.                                 "Centro

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.


*---> 30/06/2023 - Migração S4 - JP
   ES_BFLUSHDATAGEN-MATERIALNR     = WL_ZSDT0250-MATNR.                                   "Material
    data(v_len) = strlen( wl_zsdt0250-matnr ).
    CLEAR: es_bflushdatagen-materialnr, es_bflushdatagen-materialnr_long.
    IF v_len > 18.
      es_bflushdatagen-materialnr_long  = wl_zsdt0250-matnr.
    ELSE.
      es_bflushdatagen-materialnr       = wl_zsdt0250-matnr.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP


    es_bflushdatagen-materialnr     = vg_matnr.
    ES_BFLUSHDATAGEN-BACKFLQUANT    = WL_ZSDT0252-QTDE_FC_DOC_04.                          "Quantidade
    ES_BFLUSHDATAGEN-UNITOFMEASURE  = 'KG'.                                                "UM
    ES_BFLUSHDATAGEN-PRODVERSION    = ZCL_BOLETIM_PRODUCAO=>ZIF_BOLETIM_PRODUCAO~GET_VERSAO( I_MATNR = CONV #( WL_ZSDT0250-MATNR  ) I_WERKS = CONV #( WL_ZSDT0252-WERKS_V ) ).                                                 "Versão
    ES_BFLUSHDATAGEN-BATCH          = WL_ZSDT0252-CHARG.                                   "Lote
    ES_BFLUSHDATAGEN-STORAGELOC     = 'PR01'.                                              "Deposito

*--------------------------------------------------------------------------------------------*
*   Montagem Itens
*--------------------------------------------------------------------------------------------*

    "Soja Industrialização
    CLEAR: WA_GOODSMOVEMENTS.

    WL_ZSDT0250 = ZCL_BOLETIM_PRODUCAO=>ZIF_BOLETIM_PRODUCAO~GET_MATERIAL_BOLETIM( I_TP_PRODUTO =  'SI').

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.


*---> 30/06/2023 - Migração S4 - JP
   WA_GOODSMOVEMENTS-MATERIAL      = WL_ZSDT0250-MATNR.                                "Material Consumo
    v_len = strlen( wl_zsdt0250-matnr ).
    CLEAR: wa_goodsmovements-material, wa_goodsmovements-material_long.
    IF v_len > 18.
      wa_goodsmovements-material_long  = wl_zsdt0250-matnr.
    ELSE.
      wa_goodsmovements-material       = wl_zsdt0250-matnr.
    ENDIF.
*<--- 30/06/2023 - Migração S4 - JP

    wa_goodsmovements-material      = vg_matnr.
    WA_GOODSMOVEMENTS-PLANT         = WL_ZSDT0252-WERKS_V.                              "Centro
    WA_GOODSMOVEMENTS-STGE_LOC      = ME->ZIF_BOLETIM_PRODUCAO~AT_CABECALHO-LGORT_PROD. "Deposito
    WA_GOODSMOVEMENTS-BATCH         = WL_ZSDT0252-CHARG.                                "Lote Consumo
    WA_GOODSMOVEMENTS-MOVE_TYPE     = '261'.                                            "Tipo Movimento
    WA_GOODSMOVEMENTS-ENTRY_QNT     = WL_ZSDT0252-QTDE_SI_DOC_04.                       "Quantidade Consumo
    WA_GOODSMOVEMENTS-ENTRY_UOM     = 'KG'.                                             "UM Consumo
    APPEND WA_GOODSMOVEMENTS TO IT_GOODSMOVEMENTS.

    ME->ZIF_BOLETIM_PRODUCAO~CHECK_ESTOQUE_PRODUTOS( I_ITENS = IT_GOODSMOVEMENTS ).

    DATA(IT_GOODSMOVEMENTS_AUX) = IT_GOODSMOVEMENTS[].

    APPEND VALUE #(
                    MATERIAL  = ES_BFLUSHDATAGEN-MATERIALNR
*<--- 30/06/2023 - Migração S4 - JP
                    MATERIAL_LONG  = ES_BFLUSHDATAGEN-MATERIALNR_LONG
*<--- 30/06/2023 - Migração S4 - JP
                    PLANT     = ES_BFLUSHDATAGEN-PRODPLANT
                    STGE_LOC  = ES_BFLUSHDATAGEN-STORAGELOC
                    BATCH     = ES_BFLUSHDATAGEN-BATCH
                    ENTRY_QNT = ES_BFLUSHDATAGEN-BACKFLQUANT ) TO IT_GOODSMOVEMENTS_AUX.

    ME->ZIF_BOLETIM_PRODUCAO~BLOQUEIO_DESBLOQUEIO_PRODUTOS( EXPORTING I_ITENS = IT_GOODSMOVEMENTS_AUX
                                                                      I_ACAO  = 'B' ).

    CALL FUNCTION 'BAPI_REPMANCONF1_CREATE_MTS' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        BFLUSHFLAGS    = ES_BFLUSHFLAGS
        BFLUSHDATAGEN  = ES_BFLUSHDATAGEN
      IMPORTING
        CONFIRMATION   = ES_CONFIRMATION
        RETURN         = WA_RETURN
      TABLES
        GOODSMOVEMENTS = IT_GOODSMOVEMENTS.

    IF WA_RETURN-TYPE = 'E'.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      ME->ZIF_BOLETIM_PRODUCAO~BLOQUEIO_DESBLOQUEIO_PRODUTOS( EXPORTING I_ITENS = IT_GOODSMOVEMENTS_AUX
                                                                        I_ACAO  = 'D' ).

      ZCL_BOLETIM_PRODUCAO=>ZIF_BOLETIM_PRODUCAO~GERA_ERRO_GERAL(
        EXPORTING
          I_MSGV1              =   CONV #( WA_RETURN-MESSAGE_V1 )
          I_MSGV2              =   CONV #( WA_RETURN-MESSAGE_V2 )
          I_MSGV3              =   CONV #( WA_RETURN-MESSAGE_V3 )
          I_MSGV4              =   CONV #( WA_RETURN-MESSAGE_V4 )
          I_MSGID              =   CONV #( WA_RETURN-ID )
          I_MSGNO              =   CONV #( WA_RETURN-NUMBER )  ).

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      DO 100 TIMES.

        SELECT SINGLE *
          FROM BLPP INTO GS_BLPP
         WHERE PRTNR = ES_CONFIRMATION
           AND PRTPS = '0001'.

        IF SY-SUBRC = 0.

          WL_ZSDT0252-DOC_PROD_04     = GS_BLPP-BELNR.
          WL_ZSDT0252-ANO_DOC_PROD_04 = ME->ZIF_BOLETIM_PRODUCAO~AT_CABECALHO-DT_LANCAMENTO(4).
          MODIFY ZSDT0252 FROM WL_ZSDT0252.

          COMMIT WORK AND WAIT .

          E_MBLNR = GS_BLPP-BELNR.

          MESSAGE |Documento de Produção: { GS_BLPP-BELNR } gerado com sucesso!| TYPE 'S'.

          EXIT.

        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.

      ENDDO.

      ME->ZIF_BOLETIM_PRODUCAO~BLOQUEIO_DESBLOQUEIO_PRODUTOS( EXPORTING I_ITENS = IT_GOODSMOVEMENTS_AUX
                                                                        I_ACAO  = 'D' ).

    ENDIF.

  endmethod.


  METHOD zif_boletim_producao~gerar_doc_producao_05.
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

    CLEAR: e_mblnr.
    DATA: vg_matnr TYPE char18.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_doc_prod_05.

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

    IF wl_zsdt0252-doc_prod_05 IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                            attr1 = CONV #( 'Documento Produção 05' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
          msgv1  = CONV #( 'Documento Produção 05' ).
    ELSE.

      me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs = CONV #( _key_lcto )
                                                       IMPORTING e_mblnr    = e_mblnr ).

      IF e_mblnr IS NOT INITIAL.

        wl_zsdt0252-doc_prod_05     = e_mblnr.
        wl_zsdt0252-ano_doc_prod_05 = me->zif_boletim_producao~at_cabecalho-dt_lancamento(4).
        MODIFY zsdt0252 FROM wl_zsdt0252.

        IF sy-subrc NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                                msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                                attr1 = CONV #( 'Documento Produção 05' )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
              msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
              msgv1  = CONV #( 'Documento Produção 05' ).
        ENDIF.

        MESSAGE |Documento de Produção: { e_mblnr } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    CHECK wl_zsdt0252-qtde_cp_doc_05 IS NOT INITIAL.        "US 72963

    IF ( wl_zsdt0252-qtde_fh_doc_03 IS NOT INITIAL ).
      "Validar Doc. Prod. 03
      DATA(_valida_doc_prod) = me->zif_boletim_producao~check_doc_material_valido( i_mblnr = wl_zsdt0252-doc_prod_03 ).

      IF _valida_doc_prod EQ abap_false.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_not_generate-msgid
                              msgno = zcx_boletim_producao=>zcx_document_not_generate-msgno
                              attr1 = CONV #( 'Doc.Produção 03( Filial:' && wl_zsdt0252-branch && ')' )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_document_not_generate-msgno
            msgid  = zcx_boletim_producao=>zcx_document_not_generate-msgid
            msgv1  = CONV #( 'Doc.Produção 03( Filial:' && wl_zsdt0252-branch && ')' ).
      ENDIF.
    ENDIF.

    IF ( wl_zsdt0252-qtde_fc_doc_04 IS NOT INITIAL ).
      "Validar Doc. Prod. 04
      _valida_doc_prod = me->zif_boletim_producao~check_doc_material_valido( i_mblnr = wl_zsdt0252-doc_prod_04 ).

      IF _valida_doc_prod EQ abap_false.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_not_generate-msgid
                              msgno = zcx_boletim_producao=>zcx_document_not_generate-msgno
                              attr1 = CONV #( 'Doc.Produção 04( Filial:' && wl_zsdt0252-branch && ')' )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_document_not_generate-msgno
            msgid  = zcx_boletim_producao=>zcx_document_not_generate-msgid
            msgv1  = CONV #( 'Doc.Produção 04( Filial:' && wl_zsdt0252-branch && ')' ).
      ENDIF.
    ENDIF.

    IF ( wl_zsdt0252-qtde_cp_doc_05 <= 0 ) OR ( wl_zsdt0252-qtde_cm_doc_05 <= 0 ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_sem_saldo_geracao_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_sem_saldo_geracao_doc-msgno
                            attr1 = CONV #( 'Doc.Produção 05( Filial:' && wl_zsdt0252-branch && ')' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_sem_saldo_geracao_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_sem_saldo_geracao_doc-msgid
          msgv1  = CONV #( 'Doc.Produção 05( Filial:' && wl_zsdt0252-branch && ')' ).
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Gerando Documento de Produção 05 ( Filial: { wl_zsdt0252-branch } )|.

*--------------------------------------------------------------------------------------------*
*   Limpeza Variaveis
*--------------------------------------------------------------------------------------------*
    CLEAR: es_bflushflags, es_bflushdatagen, es_confirmation, it_goodsmovements[], wa_return, it_return[].

*--------------------------------------------------------------------------------------------*
*   Montagem Cabeçalho
*--------------------------------------------------------------------------------------------*

    es_bflushflags-bckfltype        = '01'.

    "Casca Peletizada
    DATA(wl_zsdt0250) = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'CP').

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
    es_bflushdatagen-materialnr     = wl_zsdt0250-matnr.                                            "Material
    es_bflushdatagen-backflquant    = wl_zsdt0252-qtde_cp_doc_05.                          "Quantidade
    es_bflushdatagen-unitofmeasure  = 'KG'.                                                "UM
    es_bflushdatagen-prodversion    = zcl_boletim_producao=>zif_boletim_producao~get_versao( i_matnr = CONV #( wl_zsdt0250-matnr  ) i_werks = CONV #( wl_zsdt0252-werks_v ) ).                                                 "Versão
    es_bflushdatagen-batch          = wl_zsdt0252-charg.                                   "Lote
    es_bflushdatagen-storageloc     = 'PR01'.                                              "Deposito

*--------------------------------------------------------------------------------------------*
*   Montagem Itens
*--------------------------------------------------------------------------------------------*

    "Casca Moida
    CLEAR: wa_goodsmovements.

    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'CM').

    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.
    CLEAR: vg_matnr.
    wl_zsdt0250-matnr = |{ wl_zsdt0250-matnr ALPHA = OUT }|.
    vg_matnr = wl_zsdt0250-matnr.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: wl_zsdt0250-matnr.
    wl_zsdt0250-matnr = vg_matnr.
    "==============================================Ajuste tamanho caractere material projeto SAP RISE / AOENNING.

    wa_goodsmovements-material      = wl_zsdt0250-matnr.                   "Material Consumo
    wa_goodsmovements-plant         = wl_zsdt0252-werks_v.        "Centro
    wa_goodsmovements-stge_loc      = 'PR01'.                     "Deposito Consumo
    wa_goodsmovements-batch         = wl_zsdt0252-charg.          "Lote Consumo
    wa_goodsmovements-move_type     = '261'.                      "Tipo Movimento
    wa_goodsmovements-entry_qnt     = wl_zsdt0252-qtde_cm_doc_05. "Quantidade Consumo
    wa_goodsmovements-entry_uom     = 'KG'.                       "UM Consumo
    APPEND wa_goodsmovements TO it_goodsmovements.

    me->zif_boletim_producao~check_estoque_produtos( i_itens = it_goodsmovements ).

    DATA(it_goodsmovements_aux) = it_goodsmovements[].

    APPEND VALUE #( material  = es_bflushdatagen-materialnr
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

          wl_zsdt0252-doc_prod_05     = gs_blpp-belnr.
          wl_zsdt0252-ano_doc_prod_05 = me->zif_boletim_producao~at_cabecalho-dt_lancamento(4).
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


  METHOD zif_boletim_producao~gerar_rateio_produtos.
    DATA: t_zsdt0252 TYPE TABLE OF zsdt0252.

    DATA: v_qtde_vinc   TYPE zsdt0247-qtde_consumo,
          v_perc_vinc   TYPE zsdt0252-perc_vinc_filial,
          v_perc_agr    TYPE zsdt0252-perc_vinc_agr,

          v_perc_fh_tot TYPE zsdt0247-perc_total,
          v_perc_fc_tot TYPE zsdt0247-perc_total,
          v_gerar_fh    TYPE c,
          v_gerar_fc    TYPE c.

    DATA: v_tot_sg        TYPE zsdt0247-qtde_consumo,
          v_tot_si_od     TYPE zsdt0247-qtde_consumo,
          v_tot_si_fh     TYPE zsdt0247-qtde_consumo,
          v_tot_si_fc     TYPE zsdt0247-qtde_consumo,
          v_tot_si_fh_fc  TYPE zsdt0247-qtde_consumo,
          v_qtde_si_fh    TYPE zsdt0247-qtde_consumo,
          v_qtde_si_fc    TYPE zsdt0247-qtde_consumo,
          v_qtde_si_fh_fc TYPE zsdt0247-qtde_consumo.


    DATA: v_rat_qtde_sg_doc_01 TYPE  zsdt0252-qtde_sg_doc_01,
          v_rat_qtde_si_doc_01 TYPE  zsdt0252-qtde_si_doc_01,
          v_rat_qtde_cm_doc_01 TYPE  zsdt0252-qtde_cm_doc_01,
          v_rat_qtde_rs_doc_01 TYPE  zsdt0252-qtde_rs_doc_01,

          v_rat_qtde_si_doc_02 TYPE  zsdt0252-qtde_si_doc_02,
          v_rat_qtde_od_doc_02 TYPE  zsdt0252-qtde_od_doc_02,

          v_rat_qtde_si_doc_03 TYPE  zsdt0252-qtde_si_doc_03,
          v_rat_qtde_fh_doc_03 TYPE  zsdt0252-qtde_fh_doc_03,

          v_rat_qtde_si_doc_04 TYPE  zsdt0252-qtde_si_doc_04,
          v_rat_qtde_fc_doc_04 TYPE  zsdt0252-qtde_fc_doc_04,

          v_rat_qtde_cp_doc_05 TYPE  zsdt0252-qtde_cp_doc_05,
          v_rat_qtde_cm_doc_05 TYPE  zsdt0252-qtde_cm_doc_05,

          v_rat_qtde_od_me     TYPE  zsdt0252-qtde_od_me,
          v_rat_qtde_od_mi     TYPE  zsdt0252-qtde_od_mi,

          v_rat_qtde_fh_me     TYPE  zsdt0252-qtde_fh_me,
          v_rat_qtde_fh_mi     TYPE  zsdt0252-qtde_fh_mi,

          v_rat_qtde_fc_me     TYPE  zsdt0252-qtde_fc_me,
          v_rat_qtde_fc_mi     TYPE  zsdt0252-qtde_fc_mi.


    DATA: v_dif      TYPE  zsdt0252-qtde_sg_doc_01,
          v_qtde_tot TYPE  zsdt0252-qtde_sg_doc_01.

    DATA: v_rat_perc_filial TYPE  zsdt0252-perc_vinc_filial,
          v_rat_perc_agr    TYPE  zsdt0252-perc_vinc_agr,
          v_dif_perc        TYPE  zsdt0252-perc_vinc_agr.

    DATA: wl_zsdt0253 TYPE zsdt0253.

*------------------------------------------------------------------------------------------------------------------------------------*
*   Validações
*------------------------------------------------------------------------------------------------------------------------------------*

    CLEAR: wl_zsdt0253.
    SELECT SINGLE * FROM zsdt0253 INTO wl_zsdt0253 WHERE branch EQ me->zif_boletim_producao~at_cabecalho-branch.

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

    CLEAR: t_zsdt0252[].

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

    DATA(_generate_rateio) = abap_true.

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_true.

      LOOP AT t_zsdt0252 INTO DATA(wl_zsdt0252) WHERE seqlcto_devol IS NOT INITIAL.
        _generate_rateio = abap_false.
      ENDLOOP.

    ELSE.

      LOOP AT t_zsdt0252 INTO wl_zsdt0252 WHERE doc_prod_01 IS NOT INITIAL.
        _generate_rateio = abap_false.
      ENDLOOP.

    ENDIF.

    CHECK _generate_rateio EQ abap_true.

    IF wl_zsdt0253-perc_si_od IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Perc.Soja Ind.x Óleo D.(ZSDT0169)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Perc.Soja Ind.x Óleo D.(ZSDT0169)' ).
    ENDIF.

    IF wl_zsdt0253-perc_si_fh_fc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Perc. Soja Ind. x Farelo(ZSDT0169)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Perc. Soja Ind. x Farelo(ZSDT0169)' ).
    ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------*
*   Recuperar Totais
*------------------------------------------------------------------------------------------------------------------------------------*
    CLEAR: v_tot_sg,
           v_perc_fh_tot,
           v_perc_fc_tot,
           v_gerar_fh,
           v_gerar_fc.

    "Soja em Graos
    CLEAR: v_tot_sg.
    LOOP AT me->zif_boletim_producao~at_dados_producao INTO DATA(wl_dados_producao).
      ADD wl_dados_producao-qtde_consumo TO v_tot_sg.

      IF ( wl_dados_producao-tp_produto_producao = 'FH' ) AND ( wl_dados_producao-qtde_consumo > 0 ).
        v_gerar_fh    = abap_true.
        v_perc_fh_tot = wl_dados_producao-perc_total.
      ENDIF.

      IF ( wl_dados_producao-tp_produto_producao = 'FC' ) AND ( wl_dados_producao-qtde_consumo > 0 ).
        v_gerar_fc    = abap_true.
        v_perc_fc_tot = wl_dados_producao-perc_total.
      ENDIF.
    ENDLOOP.

    IF ( v_gerar_fh EQ abap_false ) AND ( v_gerar_fc EQ abap_false ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                            msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                            attr1 = CONV #( 'Não informada quantidade Farelo Hipro ou Comum!' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
          msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
          msgv1  = CONV #( 'Não informada quantidade Farelo Hipro ou Comum!' ).
    ENDIF.

    IF ( v_tot_sg <= 0 ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                            msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                            attr1 = CONV #( 'Quantidade Soja em Grãos não encontrada!' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
          msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
          msgv1  = CONV #( 'Quantidade Soja em Grãos não encontrada!' ).
    ENDIF.

    "Soja Industrialização
    IF ( me->zif_boletim_producao~at_cabecalho-qtde_si <= 0 ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                            msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                            attr1 = CONV #( 'Quantidade Soja Industrialização não encontrada!' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
          msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
          msgv1  = CONV #( 'Quantidade Soja Industrialização não encontrada!' ).
    ENDIF.

    "Farelo Hipro
    READ TABLE me->zif_boletim_producao~at_dados_rendimento INTO DATA(wl_dados_fh) WITH KEY tp_produto_producao = 'FH'.
    IF ( ( sy-subrc NE 0 ) OR ( wl_dados_fh-qtde <= 0 ) ) AND ( v_gerar_fh EQ abap_true ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                            msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                            attr1 = CONV #( 'Quantidade Farelo Hipro não encontrada!' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
          msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
          msgv1  = CONV #( 'Quantidade Farelo Hipro não encontrada!' ).
    ENDIF.

    "Farelo Comum
    READ TABLE me->zif_boletim_producao~at_dados_rendimento INTO DATA(wl_dados_fc) WITH KEY tp_produto_producao = 'FC'.
    IF ( ( sy-subrc NE 0 ) OR ( wl_dados_fc-qtde <= 0 ) ) AND ( v_gerar_fc EQ abap_true ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                            msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                            attr1 = CONV #( 'Quantidade Farelo Comum não encontrada!' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
          msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
          msgv1  = CONV #( 'Quantidade Farelo Comum não encontrada!' ).
    ENDIF.

    "Oleo Degomado
    READ TABLE me->zif_boletim_producao~at_dados_rendimento INTO DATA(wl_dados_od) WITH KEY tp_produto_producao = 'OD'.
    IF ( sy-subrc NE 0 ) OR ( wl_dados_od-qtde <= 0 ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                            msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                            attr1 = CONV #( 'Quantidade Oleo Degomado não encontrada!' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
          msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
          msgv1  = CONV #( 'Quantidade Oleo Degomado não encontrada!' ).
    ENDIF.

    "Casca Moida
    READ TABLE me->zif_boletim_producao~at_dados_rendimento INTO DATA(wl_dados_cm) WITH KEY tp_produto_producao = 'CM'.
    IF ( sy-subrc NE 0 ) OR ( wl_dados_cm-qtde <= 0 ).
      CLEAR: wl_dados_cm.
    ENDIF.

    "Casca Peletizada
    READ TABLE me->zif_boletim_producao~at_dados_rendimento INTO DATA(wl_dados_cp) WITH KEY tp_produto_producao = 'CP'.
    IF ( sy-subrc NE 0 ) OR ( wl_dados_cp-qtde <= 0 ).
      CLEAR: wl_dados_cp.
    ENDIF.

    "Residuo de Soja
    READ TABLE me->zif_boletim_producao~at_dados_rendimento INTO DATA(wl_dados_rs) WITH KEY tp_produto_producao = 'RS'.
    IF ( sy-subrc NE 0 ) OR ( wl_dados_rs-qtde <= 0 ).
      CLEAR: wl_dados_rs.
    ENDIF.

    v_tot_si_od    = ( wl_zsdt0253-perc_si_od    * me->zif_boletim_producao~at_cabecalho-qtde_si ) / 100. "Total de Soja Ind. que irá gerar com o Óleo Degomado
    v_tot_si_fh_fc = ( wl_zsdt0253-perc_si_fh_fc * me->zif_boletim_producao~at_cabecalho-qtde_si ) / 100. "Total de Soja Ind. que irá gerar com os Farelos(Hipro e Comum)
    v_tot_si_fh    = ( v_perc_fh_tot * v_tot_si_fh_fc ) / 100.
    v_tot_si_fc    = ( v_perc_fc_tot * v_tot_si_fh_fc ) / 100.

    IF ( v_tot_si_od + v_tot_si_fh + v_tot_si_fc ) NE ( me->zif_boletim_producao~at_cabecalho-qtde_si ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                            msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                            attr1 = CONV #( 'Qtde. Soja Industrialização não rateada em 100%!' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
          msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
          msgv1  = CONV #( 'Qtde. Soja Industrialização não rateada em 100%!' ).
    ENDIF.



    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = 'Gerando Rateio de Produtos...'.

*-----------------------------------------------------------------------------------*
*   Limpa Quantidades/Percentuais
*-----------------------------------------------------------------------------------*
    LOOP AT t_zsdt0252 ASSIGNING FIELD-SYMBOL(<fs_sdt0252>).

      CLEAR: <fs_sdt0252>-perc_vinc_agr,
             <fs_sdt0252>-perc_vinc_filial.

      CLEAR: <fs_sdt0252>-qtde_sg_doc_01,
             <fs_sdt0252>-qtde_si_doc_01,
             <fs_sdt0252>-qtde_cm_doc_01,
             <fs_sdt0252>-qtde_rs_doc_01,

             <fs_sdt0252>-qtde_si_doc_02,
             <fs_sdt0252>-qtde_od_doc_02,

             <fs_sdt0252>-qtde_si_doc_03,
             <fs_sdt0252>-qtde_fh_doc_03,

             <fs_sdt0252>-qtde_si_doc_04,
             <fs_sdt0252>-qtde_fc_doc_04,

             <fs_sdt0252>-qtde_cp_doc_05,
             <fs_sdt0252>-qtde_cm_doc_05.

      MODIFY zsdt0252 FROM <fs_sdt0252>.

    ENDLOOP.

*-----------------------------------------------------------------------------------*
*   Gerar Percentuais de Vinculação Agrupador
*-----------------------------------------------------------------------------------*

    LOOP AT t_zsdt0252 ASSIGNING <fs_sdt0252>.

      CLEAR: v_qtde_vinc.

      <fs_sdt0252>-perc_vinc_agr = ( <fs_sdt0252>-qtde_vinc / v_tot_sg ) * 100.

      CLEAR: <fs_sdt0252>-qtde_sg_doc_01,
             <fs_sdt0252>-qtde_si_doc_01,
             <fs_sdt0252>-qtde_cm_doc_01,
             <fs_sdt0252>-qtde_rs_doc_01,

             <fs_sdt0252>-qtde_si_doc_02,
             <fs_sdt0252>-qtde_od_doc_02,

             <fs_sdt0252>-qtde_si_doc_03,
             <fs_sdt0252>-qtde_fh_doc_03,

             <fs_sdt0252>-qtde_si_doc_04,
             <fs_sdt0252>-qtde_fc_doc_04,

             <fs_sdt0252>-qtde_cp_doc_05,
             <fs_sdt0252>-qtde_cm_doc_05.

      MODIFY zsdt0252 FROM <fs_sdt0252>.

    ENDLOOP.

    "Ajusta diferença percentual agrupador rateado(caso tenha)

    CLEAR: v_rat_perc_agr.
    LOOP AT t_zsdt0252 INTO wl_zsdt0252.
      ADD wl_zsdt0252-perc_vinc_agr TO v_rat_perc_agr.
    ENDLOOP.

    LOOP AT t_zsdt0252 ASSIGNING <fs_sdt0252> WHERE id_agrp EQ 1.

      "Perc. Agrp.
      CLEAR: v_dif_perc .

      v_dif_perc = 100 - v_rat_perc_agr.
      IF ( v_dif_perc NE 0 ).

        IF ( abs( v_dif_perc ) <= '0.02' ).
          ADD v_dif_perc TO <fs_sdt0252>-perc_vinc_agr.
        ELSE.
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                                msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                                attr1 = CONV #( 'Diferença Rateio Perc.Vinc.Agr:' && v_dif_perc )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
              msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
              msgv1  = CONV #( 'Diferença Rateio Perc.Vinc.Agr:' && v_dif_perc ).
        ENDIF.
      ENDIF.

      MODIFY zsdt0252 FROM <fs_sdt0252>.

      EXIT.

    ENDLOOP.

*-----------------------------------------------------------------------------------*
*   Totalizar Percentuais de Vinculação Filial
*-----------------------------------------------------------------------------------*
    LOOP AT t_zsdt0252 ASSIGNING <fs_sdt0252> WHERE id_agrp EQ 1.

      CLEAR: v_perc_vinc , v_qtde_vinc.

      SELECT SUM( perc_vinc_agr ) SUM( qtde_vinc )
        FROM zsdt0252 INTO ( v_perc_vinc , v_qtde_vinc )
       WHERE id_boletim EQ <fs_sdt0252>-id_boletim
         AND branch     EQ <fs_sdt0252>-branch
         AND charg      EQ <fs_sdt0252>-charg.

      IF sy-subrc EQ 0.

        <fs_sdt0252>-perc_vinc_filial = v_perc_vinc.
        <fs_sdt0252>-qtde_sg_doc_01   = v_qtde_vinc.

        MODIFY zsdt0252 FROM <fs_sdt0252>.

      ELSE.
        ROLLBACK WORK.

        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_gravar_dados-msgid
                              msgno = zcx_boletim_producao=>zcx_error_gravar_dados-msgno
                              attr1 = CONV #( 'ZSDT0252: Id. Agrp 1'  )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_error_gravar_dados-msgno
            msgid  = zcx_boletim_producao=>zcx_error_gravar_dados-msgid
            msgv1  = CONV #( 'ZSDT0252: Id. Agrp 1' ).
      ENDIF.

    ENDLOOP.

*---------------------------------------------------------------------------------*
*   Valida percentual filial/agrupador rateado
*---------------------------------------------------------------------------------*
    CLEAR: v_perc_vinc, v_perc_agr.

    SELECT SUM( perc_vinc_filial )
      FROM zsdt0252 INTO v_perc_vinc
     WHERE id_boletim EQ me->zif_boletim_producao~at_cabecalho-id_boletim.

    IF v_perc_vinc NE 100.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_tot_vinculado-msgid
                            msgno = zcx_boletim_producao=>zcx_erro_tot_vinculado-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_erro_tot_vinculado-msgno
          msgid  = zcx_boletim_producao=>zcx_erro_tot_vinculado-msgid.
    ENDIF.


    SELECT SUM( perc_vinc_agr )
      FROM zsdt0252 INTO v_perc_agr
     WHERE id_boletim EQ me->zif_boletim_producao~at_cabecalho-id_boletim.

    IF v_perc_agr NE 100.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_tot_vinculado-msgid
                            msgno = zcx_boletim_producao=>zcx_erro_tot_vinculado-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_erro_tot_vinculado-msgno
          msgid  = zcx_boletim_producao=>zcx_erro_tot_vinculado-msgid.
    ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------*
*   Calcular Proporção de Quantidades.
*------------------------------------------------------------------------------------------------------------------------------------*
    CLEAR: v_rat_qtde_sg_doc_01,
           v_rat_qtde_si_doc_01,
           v_rat_qtde_cm_doc_01,
           v_rat_qtde_rs_doc_01,

           v_rat_qtde_si_doc_02,
           v_rat_qtde_od_doc_02,

           v_rat_qtde_si_doc_03,
           v_rat_qtde_fh_doc_03,

           v_rat_qtde_si_doc_04,
           v_rat_qtde_fc_doc_04,

           v_rat_qtde_cp_doc_05,
           v_rat_qtde_cm_doc_05.

    LOOP AT t_zsdt0252 ASSIGNING <fs_sdt0252> WHERE id_agrp EQ 1.

*------------------------------------------------------------------------------------------------------------------------------------*
*     Doc. Produção 01
*------------------------------------------------------------------------------------------------------------------------------------*

      "Soja em Grãos
      "Quantidade do documento já definido em passo anterior...

      "Soja Industrialização
      <fs_sdt0252>-qtde_si_doc_01 = ( <fs_sdt0252>-perc_vinc_filial * me->zif_boletim_producao~at_cabecalho-qtde_si ) / 100.

      "Casca Moida
      <fs_sdt0252>-qtde_cm_doc_01 = ( <fs_sdt0252>-perc_vinc_filial * wl_dados_cm-qtde ) / 100.

      "Residuo de Soja
      <fs_sdt0252>-qtde_rs_doc_01 = ( <fs_sdt0252>-perc_vinc_filial * wl_dados_rs-qtde ) / 100.

*------------------------------------------------------------------------------------------------------------------------------------*
*     Doc. Produção 02
*------------------------------------------------------------------------------------------------------------------------------------*

      "Oleo Degomado
      <fs_sdt0252>-qtde_od_doc_02 = ( <fs_sdt0252>-perc_vinc_filial * wl_dados_od-qtde ) / 100.

      "Soja Idustrialização
      <fs_sdt0252>-qtde_si_doc_02 = ( wl_zsdt0253-perc_si_od * <fs_sdt0252>-qtde_si_doc_01 ) / 100.

*------------------------------------------------------------------------------------------------------------------------------------*
*     Doc. Produção 03
*------------------------------------------------------------------------------------------------------------------------------------*

      IF wl_dados_fh-qtde >= 0.
        "Farelo Hipro
        <fs_sdt0252>-qtde_fh_doc_03 = ( <fs_sdt0252>-perc_vinc_filial * wl_dados_fh-qtde ) / 100.

        "Soja Idustrialização
        v_qtde_si_fh_fc = ( wl_zsdt0253-perc_si_fh_fc * <fs_sdt0252>-qtde_si_doc_01 ) / 100. "Total de Soja Ind. que irá gerar com os Farelos(Hipro e Comum)
        v_qtde_si_fh    = ( v_perc_fh_tot * v_qtde_si_fh_fc ) / 100.

        <fs_sdt0252>-qtde_si_doc_03 = v_qtde_si_fh.
      ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------*
*     Doc. Produção 04
*------------------------------------------------------------------------------------------------------------------------------------*

      IF wl_dados_fc-qtde >= 0.
        "Farelo Comum
        <fs_sdt0252>-qtde_fc_doc_04 = ( <fs_sdt0252>-perc_vinc_filial * wl_dados_fc-qtde ) / 100.

        "Soja Idustrialização
        v_qtde_si_fh_fc = ( wl_zsdt0253-perc_si_fh_fc * <fs_sdt0252>-qtde_si_doc_01 ) / 100. "Total de Soja Ind. que irá gerar com os Farelos(Hipro e Comum)
        v_qtde_si_fc    = ( v_perc_fc_tot * v_qtde_si_fh_fc ) / 100.

        <fs_sdt0252>-qtde_si_doc_04 = v_qtde_si_fc.
      ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------*
*     Doc. Produção 05
*------------------------------------------------------------------------------------------------------------------------------------*

      IF wl_dados_cp-qtde >= 0.
        <fs_sdt0252>-qtde_cp_doc_05  = ( <fs_sdt0252>-perc_vinc_filial * wl_dados_cp-qtde ) / 100.
        <fs_sdt0252>-qtde_cm_doc_05  = <fs_sdt0252>-qtde_cp_doc_05.
      ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------*
*     Confere Quantidade SI Doc.Produção 1 2 3 4
*------------------------------------------------------------------------------------------------------------------------------------*
      CLEAR: v_dif.

      v_dif = <fs_sdt0252>-qtde_si_doc_01 - ( <fs_sdt0252>-qtde_si_doc_02 + <fs_sdt0252>-qtde_si_doc_03 + <fs_sdt0252>-qtde_si_doc_04 ).
      IF ( v_dif NE 0 ) AND ( <fs_sdt0252>-qtde_si_doc_01 > 0 ).

        IF ( abs( v_dif ) <= 100 ).
          ADD v_dif TO <fs_sdt0252>-qtde_si_doc_02.
          ADD v_dif TO v_tot_si_od.
        ELSE.
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                                msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                                attr1 = CONV #( 'Dif. Rateio Soja Ind.(Doc.01,02,03,04):' && v_dif )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
              msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
              msgv1  = CONV #( 'Dif. Rateio Soja Ind.(Doc.01,02,03,04):' && v_dif ).
        ENDIF.
      ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------*
*     Quantidade Mercado Externo
*------------------------------------------------------------------------------------------------------------------------------------*

      "Oleo Degomado
      IF wl_dados_od-qtde_me >= 0.
        <fs_sdt0252>-qtde_od_me = ( <fs_sdt0252>-perc_vinc_filial * wl_dados_od-qtde_me ) / 100.
      ENDIF.

      IF wl_dados_od-qtde_mi >= 0.
        <fs_sdt0252>-qtde_od_mi = ( <fs_sdt0252>-perc_vinc_filial * wl_dados_od-qtde_mi ) / 100.
      ENDIF.

      "Farelo Hipro
      IF wl_dados_fh-qtde_me >= 0.
        <fs_sdt0252>-qtde_fh_me = ( <fs_sdt0252>-perc_vinc_filial * wl_dados_fh-qtde_me ) / 100.
      ENDIF.

      IF wl_dados_fh-qtde_mi >= 0.
        <fs_sdt0252>-qtde_fh_mi = ( <fs_sdt0252>-perc_vinc_filial * wl_dados_fh-qtde_mi ) / 100.
      ENDIF.

      "Farelo Comum
      IF wl_dados_fc-qtde_me >= 0.
        <fs_sdt0252>-qtde_fc_me = ( <fs_sdt0252>-perc_vinc_filial * wl_dados_fc-qtde_me ) / 100.
      ENDIF.

      IF wl_dados_fc-qtde_mi >= 0.
        <fs_sdt0252>-qtde_fc_mi = ( <fs_sdt0252>-perc_vinc_filial * wl_dados_fc-qtde_mi ) / 100.
      ENDIF.

      MODIFY zsdt0252 FROM <fs_sdt0252>.

      ADD <fs_sdt0252>-qtde_sg_doc_01  TO  v_rat_qtde_sg_doc_01.
      ADD <fs_sdt0252>-qtde_si_doc_01  TO  v_rat_qtde_si_doc_01.
      ADD <fs_sdt0252>-qtde_cm_doc_01  TO  v_rat_qtde_cm_doc_01.
      ADD <fs_sdt0252>-qtde_rs_doc_01  TO  v_rat_qtde_rs_doc_01.

      ADD <fs_sdt0252>-qtde_si_doc_02  TO  v_rat_qtde_si_doc_02.
      ADD <fs_sdt0252>-qtde_od_doc_02  TO  v_rat_qtde_od_doc_02.

      ADD <fs_sdt0252>-qtde_si_doc_03  TO  v_rat_qtde_si_doc_03.
      ADD <fs_sdt0252>-qtde_fh_doc_03  TO  v_rat_qtde_fh_doc_03.

      ADD <fs_sdt0252>-qtde_si_doc_04  TO  v_rat_qtde_si_doc_04.
      ADD <fs_sdt0252>-qtde_fc_doc_04  TO  v_rat_qtde_fc_doc_04.

      ADD <fs_sdt0252>-qtde_cp_doc_05  TO  v_rat_qtde_cp_doc_05.
      ADD <fs_sdt0252>-qtde_cm_doc_05  TO  v_rat_qtde_cm_doc_05.

      ADD <fs_sdt0252>-qtde_od_me  TO  v_rat_qtde_od_me.
      ADD <fs_sdt0252>-qtde_od_mi  TO  v_rat_qtde_od_mi.

      ADD <fs_sdt0252>-qtde_fh_me  TO  v_rat_qtde_fh_me.
      ADD <fs_sdt0252>-qtde_fh_mi  TO  v_rat_qtde_fh_mi.

      ADD <fs_sdt0252>-qtde_fc_me  TO  v_rat_qtde_fc_me.
      ADD <fs_sdt0252>-qtde_fc_mi  TO  v_rat_qtde_fc_mi.

    ENDLOOP.

    "Checar Diferenças
    LOOP AT t_zsdt0252 ASSIGNING <fs_sdt0252> WHERE id_agrp EQ 1.

*------------------------------------------------------------------------------------------------------------------------------------*
*     Doc. Produção 01
*------------------------------------------------------------------------------------------------------------------------------------*

      "Soja Industrialização
      CLEAR: v_dif.

      v_dif = me->zif_boletim_producao~at_cabecalho-qtde_si - v_rat_qtde_si_doc_01.
      IF ( v_dif NE 0 ) AND ( me->zif_boletim_producao~at_cabecalho-qtde_si > 0 ).

        IF ( abs( v_dif ) <= 100 ).
          ADD v_dif TO <fs_sdt0252>-qtde_si_doc_01.
        ELSE.
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                                msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                                attr1 = CONV #( 'Diferença Rateio Soja Industrialização(Doc.01):' && v_dif )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
              msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
              msgv1  = CONV #( 'Diferença Rateio Soja Industrialização(Doc.01):' && v_dif ).
        ENDIF.
      ENDIF.


      "Casca Moida
      CLEAR: v_dif.

      v_dif = wl_dados_cm-qtde - v_rat_qtde_cm_doc_01.
      IF ( v_dif NE 0 ) AND ( wl_dados_cm-qtde > 0 ).

        IF ( abs( v_dif ) <= 100 ).
          ADD v_dif TO <fs_sdt0252>-qtde_cm_doc_01.
        ELSE.
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                                msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                                attr1 = CONV #( 'Diferença Rateio Casca Moida(Doc.01):' && v_dif )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
              msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
              msgv1  = CONV #( 'Diferença Rateio Casca Moida(Doc.01):' && v_dif ).
        ENDIF.
      ENDIF.

      "Residuo de Soja
      CLEAR: v_dif.

      v_dif = wl_dados_rs-qtde - v_rat_qtde_rs_doc_01.
      IF ( v_dif NE 0 ) AND ( wl_dados_rs-qtde > 0 ).

        IF ( abs( v_dif ) <= 100 ).
          ADD v_dif TO <fs_sdt0252>-qtde_rs_doc_01.
        ELSE.
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                                msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                                attr1 = CONV #( 'Diferença Rateio Residuo de Soja(Doc.01):' && v_dif )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
              msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
              msgv1  = CONV #( 'Diferença Rateio Residuo de Soja(Doc.01):' && v_dif ).
        ENDIF.

      ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------*
*     Doc. Produção 02
*------------------------------------------------------------------------------------------------------------------------------------*

      "Soja idustrialização
      CLEAR: v_dif.

      v_dif = v_tot_si_od - v_rat_qtde_si_doc_02.
      IF ( v_dif NE 0 ) AND ( v_tot_si_od > 0 ).
        IF ( abs( v_dif ) <= 100 ).
          ADD v_dif TO <fs_sdt0252>-qtde_si_doc_02.
        ELSE.
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                                msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                                attr1 = CONV #( 'Diferença Rateio Soja Ind.(Doc.02):' && v_dif )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
              msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
              msgv1  = CONV #( 'Diferença Rateio Soja Ind.(Doc.02):' && v_dif ).
        ENDIF.

      ENDIF.

      "Oleo Degomado
      CLEAR: v_dif.

      v_dif = wl_dados_od-qtde - v_rat_qtde_od_doc_02.
      IF ( v_dif NE 0 ) AND ( wl_dados_od-qtde > 0 ).

        IF ( abs( v_dif ) <= 100 ).
          ADD v_dif TO <fs_sdt0252>-qtde_od_doc_02.
        ELSE.
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                                msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                                attr1 = CONV #( 'Diferença Rateio Oleo Degomado(Doc.02):' && v_dif )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
              msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
              msgv1  = CONV #( 'Diferença Rateio Oleo Degomado(Doc.02):' && v_dif ).
        ENDIF.

      ENDIF.


*------------------------------------------------------------------------------------------------------------------------------------*
*     Doc. Produção 03
*------------------------------------------------------------------------------------------------------------------------------------*

      "Soja Industrialização
      CLEAR: v_dif.

      v_dif = v_tot_si_fh - v_rat_qtde_si_doc_03.
      IF ( v_dif NE 0 ) AND ( v_tot_si_fh > 0 ).
        IF ( abs( v_dif ) <= 100 ).
          "ADD v_dif TO <fs_sdt0252>-qtde_si_doc_03. Não é necessario .. Diferença é aplicada no Doc. Prod. 02
        ELSE.
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                                msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                                attr1 = CONV #( 'Diferença Rateio Soja Ind.(Doc.03):' && v_dif )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
              msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
              msgv1  = CONV #( 'Diferença Rateio Soja Ind.(Doc.03):' && v_dif ).
        ENDIF.

      ENDIF.

      "Farelo Hipro
      CLEAR: v_dif.

      v_dif = wl_dados_fh-qtde - v_rat_qtde_fh_doc_03.
      IF ( v_dif NE 0 ) AND ( wl_dados_fh-qtde > 0 ).

        IF ( abs( v_dif ) <= 100 ).
          ADD v_dif TO <fs_sdt0252>-qtde_fh_doc_03.
        ELSE.
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                                msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                                attr1 = CONV #( 'Diferença Rateio Farelo Hipro(Doc.03):' && v_dif )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
              msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
              msgv1  = CONV #( 'Diferença Rateio Farelo Hipro(Doc.03):' && v_dif ).
        ENDIF.

      ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------*
*     Doc. Produção 04
*------------------------------------------------------------------------------------------------------------------------------------*

      "Soja Industrialização
      CLEAR: v_dif.

      v_dif = v_tot_si_fc - v_rat_qtde_si_doc_04.
      IF ( v_dif NE 0 ) AND ( v_tot_si_fc > 0 ).
        IF ( abs( v_dif ) <= 100 ).
          "ADD v_dif TO <fs_sdt0252>-qtde_si_doc_04. "Não é necessario .. Diferença é aplicada no Doc. Prod. 02
        ELSE.
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                                msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                                attr1 = CONV #( 'Diferença Rateio Soja Ind.(Doc.04):' && v_dif )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
              msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
              msgv1  = CONV #( 'Diferença Rateio Soja Ind.(Doc.04):' && v_dif ).
        ENDIF.

      ENDIF.

      "Farelo Comum
      CLEAR: v_dif.

      v_dif = wl_dados_fc-qtde - v_rat_qtde_fc_doc_04.
      IF ( v_dif NE 0 ) AND ( wl_dados_fc-qtde > 0 ).

        IF ( abs( v_dif ) <= 100 ).
          ADD v_dif TO <fs_sdt0252>-qtde_fc_doc_04.
        ELSE.
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                                msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                                attr1 = CONV #( 'Diferença Rateio Farelo Comum(Doc.04):' && v_dif )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
              msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
              msgv1  = CONV #( 'Diferença Rateio Farelo Comum(Doc.04):' && v_dif ).
        ENDIF.

      ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------*
*     Doc. Produção 05
*------------------------------------------------------------------------------------------------------------------------------------*

      "Casca Peletizada
      CLEAR: v_dif.

      v_dif = wl_dados_cp-qtde - v_rat_qtde_cp_doc_05.
      IF ( v_dif NE 0 ) AND ( wl_dados_cp-qtde > 0 ).

        IF ( abs( v_dif ) <= 100 ).
          ADD v_dif TO <fs_sdt0252>-qtde_cp_doc_05.
        ELSE.
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                                msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                                attr1 = CONV #( 'Diferença Rateio Casca Peletizada(Doc.05):' && v_dif )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
              msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
              msgv1  = CONV #( 'Diferença Rateio Farelo Casca Peletizada(Doc.05):' && v_dif ).
        ENDIF.
      ENDIF.

      "Casca Moida
      CLEAR: v_dif.

      v_dif = wl_dados_cp-qtde - v_rat_qtde_cm_doc_05.
      IF ( v_dif NE 0 ) AND ( wl_dados_cp-qtde > 0 ).

        IF ( abs( v_dif ) <= 100 ).
          ADD v_dif TO <fs_sdt0252>-qtde_cm_doc_05.
        ELSE.
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                                msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                                attr1 = CONV #( 'Diferença Rateio Casca Moida(Doc.05):' && v_dif )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
              msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
              msgv1  = CONV #( 'Diferença Rateio Farelo Casca Moida(Doc.05):' && v_dif ).
        ENDIF.
      ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------*
*         "Checar Diferenças Mercado Externo \ Mercado Interno
*------------------------------------------------------------------------------------------------------------------------------------*

*     "Oleo Degomado ME
      CLEAR: v_dif.

      v_dif = wl_dados_od-qtde_me - v_rat_qtde_od_me.
      IF ( v_dif NE 0 ) AND ( wl_dados_od-qtde_me > 0 ).

        IF ( abs( v_dif ) <= 100 ).
          ADD v_dif TO <fs_sdt0252>-qtde_od_me.
        ELSE.
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                                msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                                attr1 = CONV #( 'Diferença Rateio Oleo Degomado ME:' && v_dif )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
              msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
              msgv1  = CONV #( 'Diferença Rateio Oleo Degomado ME:' && v_dif ).
        ENDIF.

      ENDIF.


*     "Oleo Degomado MI
      <fs_sdt0252>-qtde_od_mi  = <fs_sdt0252>-qtde_od_doc_02 - <fs_sdt0252>-qtde_od_me.

*----------------------------------------------
      "Farelo Hipro ME
*----------------------------------------------
      "Farelo Hipro ME
      CLEAR: v_dif.

      v_dif = wl_dados_fh-qtde_me - v_rat_qtde_fh_me.
      IF ( v_dif NE 0 ) AND ( wl_dados_fh-qtde_me > 0 ).

        IF ( abs( v_dif ) <= 100 ).
          ADD v_dif TO <fs_sdt0252>-qtde_fh_me.
        ELSE.
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                                msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                                attr1 = CONV #( 'Diferença Rateio Farelo Hipro ME:' && v_dif )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
              msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
              msgv1  = CONV #( 'Diferença Rateio Farelo Hipro ME:' && v_dif ).
        ENDIF.

      ENDIF.

      "Farelo Hipro MI
      <fs_sdt0252>-qtde_fh_mi  = <fs_sdt0252>-qtde_fh_doc_03 - <fs_sdt0252>-qtde_fh_me.

*----------------------------------------------
      "Farelo Comum
*----------------------------------------------
      "Farelo Comum ME
      CLEAR: v_dif.

      v_dif = wl_dados_fc-qtde_me - v_rat_qtde_fc_me.
      IF ( v_dif NE 0 ) AND ( wl_dados_fc-qtde > 0 ).

        IF ( abs( v_dif ) <= 100 ).
          ADD v_dif TO <fs_sdt0252>-qtde_fc_me.
        ELSE.
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                                msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                                attr1 = CONV #( 'Diferença Rateio Farelo Comum ME:' && v_dif )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
              msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
              msgv1  = CONV #( 'Diferença Rateio Farelo Comum ME:' && v_dif ).
        ENDIF.

      ENDIF.

      "Farelo Comum MI
      <fs_sdt0252>-qtde_fc_mi  = <fs_sdt0252>-qtde_fc_doc_04 - <fs_sdt0252>-qtde_fc_me.

      MODIFY zsdt0252 FROM <fs_sdt0252>.

      EXIT.

    ENDLOOP.

    COMMIT WORK.

  ENDMETHOD.


  METHOD zif_boletim_producao~get_perc_rendimento.
    DATA: v_tot_produtos_prod TYPE zsdt0247-qtde_consumo.

    DATA: v_msg_aux TYPE c LENGTH 200.

    CLEAR: v_tot_produtos_prod.
    LOOP AT i_zsdt0247 INTO DATA(wl_zsdt0247).
      ADD wl_zsdt0247-qtde_consumo TO v_tot_produtos_prod.
    ENDLOOP.

    LOOP AT c_zsdt0248 ASSIGNING FIELD-SYMBOL(<fs_zsdt0248>).

      v_msg_aux = zcl_util=>get_desc_value_domain(  i_domname  = 'ZDM_TP_PRODUTO_PRODUCAO'
                                                    i_domvalue = CONV #( <fs_zsdt0248>-tp_produto_producao ) ).

      CLEAR: <fs_zsdt0248>-perc_rendimento.

      CHECK <fs_zsdt0248>-qtde > 0.

      TRY.

          CASE <fs_zsdt0248>-tp_produto_producao.
            WHEN 'FC'. "Farelo Comum

              READ TABLE i_zsdt0247 INTO wl_zsdt0247 WITH KEY tp_produto_producao = 'FC'.
              CHECK ( sy-subrc EQ 0 ) AND ( wl_zsdt0247-qtde_consumo > 0 ).

              <fs_zsdt0248>-perc_rendimento = ( <fs_zsdt0248>-qtde / wl_zsdt0247-qtde_consumo ) * 100.

            WHEN 'FH'. "Farelo Hipro

              READ TABLE i_zsdt0247 INTO wl_zsdt0247 WITH KEY tp_produto_producao = 'FH'.
              CHECK ( sy-subrc EQ 0 ) AND ( wl_zsdt0247-qtde_consumo > 0 ).

              <fs_zsdt0248>-perc_rendimento = ( <fs_zsdt0248>-qtde / wl_zsdt0247-qtde_consumo  ) * 100.

            WHEN 'OD'. "Oleo Degomado

              CHECK ( v_tot_produtos_prod > 0 ).

              <fs_zsdt0248>-perc_rendimento = ( <fs_zsdt0248>-qtde / v_tot_produtos_prod  ) * 100.

            WHEN 'CM'. "Casca Moida

              READ TABLE i_zsdt0247 INTO wl_zsdt0247 WITH KEY tp_produto_producao = 'FH'.
              CHECK ( sy-subrc EQ 0 ) AND ( wl_zsdt0247-qtde_consumo > 0 ).

              <fs_zsdt0248>-perc_rendimento = ( <fs_zsdt0248>-qtde / wl_zsdt0247-qtde_consumo  ) * 100.

            WHEN 'RS'. "Residuo Soja

              READ TABLE i_zsdt0247 INTO wl_zsdt0247 WITH KEY tp_produto_producao = 'FH'.
              CHECK ( sy-subrc EQ 0 ) AND ( wl_zsdt0247-qtde_consumo > 0 ).

              <fs_zsdt0248>-perc_rendimento = ( <fs_zsdt0248>-qtde / wl_zsdt0247-qtde_consumo  ) * 100.

            WHEN 'CP'. "Casca Peletizada
          ENDCASE.

        CATCH cx_sy_arithmetic_overflow.
          MESSAGE |Não foi possivel calcular o Rendimento % do produto { v_msg_aux }! Verificar dados informados!| TYPE 'I'.
          CLEAR:  <fs_zsdt0248>-qtde.
      ENDTRY.


    ENDLOOP.

  ENDMETHOD.


  METHOD zif_boletim_producao~get_tp_produtos_producao.
    CLEAR: r_tp_produtos[].

    DATA(wl_zsdt0250) = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'FC'). "Farelo Comum
    APPEND wl_zsdt0250 TO r_tp_produtos.

    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'FH'). "Farelo Hipro
    APPEND wl_zsdt0250 TO r_tp_produtos.
  ENDMETHOD.


  METHOD zif_boletim_producao~get_tp_produtos_rendimento.
    CLEAR: r_tp_produtos[].

    DATA(wl_zsdt0250) = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'FC'). "Farelo Comum
    APPEND wl_zsdt0250 TO r_tp_produtos.

    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'FH'). "Farelo Hipro
    APPEND wl_zsdt0250 TO r_tp_produtos.

    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'OD'). "Óleo Degomado
    APPEND wl_zsdt0250 TO r_tp_produtos.

    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'CM'). "Casca Moida
    APPEND wl_zsdt0250 TO r_tp_produtos.

    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'CP'). "Casca Peletizada
    APPEND wl_zsdt0250 TO r_tp_produtos.

    wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'RS'). "Residuo de Soja
    APPEND wl_zsdt0250 TO r_tp_produtos.
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

    SELECT SINGLE *
      FROM zsdt0252 INTO @DATA(wl_zsdt0252)
     WHERE id_boletim EQ @wl_zsdt0252_ck-id_boletim
       AND branch     EQ @wl_zsdt0252_ck-branch
       AND charg      EQ @wl_zsdt0252_ck-charg
       AND id_agrp    EQ @wl_zsdt0252_ck-id_agrp.

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

        "Soja Industrialização
        DATA(wl_zsdt0250) = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'SI').
        READ TABLE it_mseg INTO DATA(wl_mseg) WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

        "Soja em Grãos
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'SG').
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

        "Casca Moida
        IF wl_zsdt0252-qtde_cm_doc_01 > 0.
          wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'CM').
          READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
          CHECK sy-subrc EQ 0.
        ENDIF.

        "Residuo de Soja
        IF wl_zsdt0252-qtde_rs_doc_01 > 0.
          wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'RS').
          READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
          CHECK sy-subrc EQ 0.
        ENDIF.

      WHEN '02'.

        "Oleo Degomado
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'OD').
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

        "Soja Industrialização
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'SI').
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

      WHEN '03'.

        "Farelo Hipro
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'FH').
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

        "Soja Industrialização
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'SI').
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

      WHEN '04'.

        "Farelo Comum
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'FC').
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

        "Soja Industrialização
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'SI').
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.


      WHEN '05'.

        "Casca Peletizada
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'CP').
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

        "Casca Moida
        wl_zsdt0250 = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'CM').
        READ TABLE it_mseg INTO wl_mseg WITH KEY matnr = wl_zsdt0250-matnr.
        CHECK sy-subrc EQ 0.

    ENDCASE.

    r_validado = abap_true.
  ENDMETHOD.
ENDCLASS.
