CLASS zcx_carga DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF zcx_ordem_vencida,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_vencida .
    CONSTANTS:
      BEGIN OF zcx_erro_geral,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF zcx_erro_geral .
    CONSTANTS:
      BEGIN OF zcx_ordem_fechada,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_fechada .
    CONSTANTS:
      BEGIN OF zcx_ordem_cancelada,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_cancelada .
    CONSTANTS:
      BEGIN OF zcx_placa_trator,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_placa_trator .
    CONSTANTS:
      BEGIN OF zcx_placa_reboque1,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_placa_reboque1 .
    CONSTANTS:
      BEGIN OF zcx_placa_reboque2,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_placa_reboque2 .
    CONSTANTS:
      BEGIN OF zcx_placa_reboque3,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_placa_reboque3 .
    CONSTANTS:
      BEGIN OF zcx_ordem_safra,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_safra .
    CONSTANTS:
      BEGIN OF zcx_ordem_empresa,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_empresa .
    CONSTANTS:
      BEGIN OF zcx_ordem_filial,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_filial .
    CONSTANTS:
      BEGIN OF zcx_ordem_empresaag,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_empresaag .
    CONSTANTS:
      BEGIN OF zcx_ordem_filialag,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '016',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_filialag .
    CONSTANTS:
      BEGIN OF zcx_ordem_produto,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '017',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_produto .
    CONSTANTS:
      BEGIN OF zcx_ordem_motorista,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '018',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_motorista .
    CONSTANTS:
      BEGIN OF zcx_ordem_proprietario,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '019',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_proprietario .
    CONSTANTS:
      BEGIN OF zcx_carga_aberta,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '020',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_carga_aberta .
    CONSTANTS:
      BEGIN OF zcx_carga_fechada,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '021',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_carga_fechada .
    CONSTANTS:
      BEGIN OF zcx_carga_conferida,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '022',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_carga_conferida .
    CONSTANTS:
      BEGIN OF zcx_carga_cancelada,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '023',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_carga_cancelada .
    CONSTANTS:
      BEGIN OF zcx_carga_nao_aberta,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '024',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_carga_nao_aberta .
    CONSTANTS:
      BEGIN OF zcx_ordem_carga_safra,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '025',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_carga_safra .
    CONSTANTS:
      BEGIN OF zcx_ordem_carga_empresa,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '026',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_carga_empresa .
    CONSTANTS:
      BEGIN OF zcx_ordem_carga_filial,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '027',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_carga_filial .
    CONSTANTS:
      BEGIN OF zcx_tara_maior_bruto,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '030',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_tara_maior_bruto .
    CONSTANTS:
      BEGIN OF zcx_ordem_venda_not_found,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '031',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_venda_not_found .
    CONSTANTS:
      BEGIN OF zcx_ordem_venda_partiner,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '032',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_venda_partiner .
    CONSTANTS:
      BEGIN OF zcx_placa_sem_cadastro,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '067',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_placa_sem_cadastro .
    CONSTANTS:
      BEGIN OF zcx_placa_nao_tracao,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '068',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_placa_nao_tracao .
    CONSTANTS:
      BEGIN OF zcx_placa_nao_reboque,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '069',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_placa_nao_reboque .
    CONSTANTS:
      BEGIN OF zcx_placa_sem_qtd_eixo,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '070',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_placa_sem_qtd_eixo .
    CONSTANTS:
      BEGIN OF zcx_centro_a_fixar_deposito,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '078',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF zcx_centro_a_fixar_deposito .
    CONSTANTS:
      BEGIN OF zcx_sem_pedido_compra,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '079',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF zcx_sem_pedido_compra .
    CONSTANTS:
      BEGIN OF zcx_estornar_rom_ent,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '080',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_estornar_rom_ent .
    CONSTANTS:
      BEGIN OF zcx_existe_estorno_pendente,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '082',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_existe_estorno_pendente .
    CONSTANTS:
      BEGIN OF zcx_existe_estorno_erro,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '083',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_existe_estorno_erro .
    CONSTANTS:
      BEGIN OF zcx_job_entrada_exec,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '084',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_job_entrada_exec .
    CONSTANTS:
      BEGIN OF zcx_msg_estorno_erro,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '085',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_msg_estorno_erro .
    CONSTANTS:
      BEGIN OF zcx_solic_estorno_andamento,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '086',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_solic_estorno_andamento .
    CONSTANTS:
      BEGIN OF zcx_sem_registro_entrada,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '087',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_registro_entrada .
    CONSTANTS:
      BEGIN OF zcx_sem_registros_msg_inteface,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '088',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_registros_msg_inteface .
    CONSTANTS:
      BEGIN OF zcx_romaneio_saida_doc,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '089',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_romaneio_saida_doc .
    CONSTANTS:
      BEGIN OF zcx_peso_liq_subtotal,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '096',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_peso_liq_subtotal .
    CONSTANTS:
      BEGIN OF zcx_erro_descontos,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '097',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_descontos .
    CONSTANTS:
      BEGIN OF zcx_peso_subtotal,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '099',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_peso_subtotal .
    CONSTANTS:
      BEGIN OF zcx_peso_liquido,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '100',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_peso_liquido .
    CONSTANTS:
      BEGIN OF zcx_peso_desc_umidade,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '101',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_peso_desc_umidade .
    CONSTANTS:
      BEGIN OF zcx_peso_desc_impureza,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '102',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_peso_desc_impureza .
    CONSTANTS:
      BEGIN OF zcx_peso_desc_avariado,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '103',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_peso_desc_avariado .
    CONSTANTS:
      BEGIN OF zcx_peso_desc_ardido,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '104',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_peso_desc_ardido .
    CONSTANTS:
      BEGIN OF zcx_peso_desc_quebrado,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '105',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_peso_desc_quebrado .
    CONSTANTS:
      BEGIN OF zcx_peso_desc_esverdeado,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '106',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_peso_desc_esverdeado .
    CONSTANTS:
      BEGIN OF zcx_notas_sem_docnum,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '110',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_notas_sem_docnum .
    CONSTANTS:
      BEGIN OF zcx_peso_desc_carunchado,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '246',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_peso_desc_carunchado .
    CONSTANTS:
      BEGIN OF zcx_sem_doc_fiscal,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '111',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_doc_fiscal .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_fornecedor,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '064',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_fornecedor .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_ie_forn,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '112',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_ie_forn .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_nf_numero,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '048',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_nf_numero .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_nf_serie,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '049',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_nf_serie .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_nf_dt_emissao,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '050',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_nf_dt_emissao .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_nf_quantidade,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '051',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_nf_quantidade .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_nf_valor_total,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '052',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_nf_valor_total .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_dt_venc_form,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '113',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_dt_venc_form .
    CONSTANTS:
      BEGIN OF zcx_pessoa_fis_nfe,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '114',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_pessoa_fis_nfe .
    CONSTANTS:
      BEGIN OF zcx_pessoa_jus_papel,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '115',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_pessoa_jus_papel .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_qt_fardo,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '267',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_qt_fardo .
    CONSTANTS:
      BEGIN OF zcx_nota_fiscal_informada,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '116',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nota_fiscal_informada .
*-US 128284-28-06-2024-#128284-RJF-inicio
    CONSTANTS:
      BEGIN OF zcx_nota_fiscal_qtd,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '116',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nota_fiscal_qtd.
*-US 128284-28-06-2024-#128284-RJF-fim
    CONSTANTS:
      BEGIN OF zcx_nota_fiscal_lancada,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '117',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF zcx_nota_fiscal_lancada .
    CONSTANTS:
      BEGIN OF zcx_estornar_entrada,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '118',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_estornar_entrada .
    CONSTANTS:
      BEGIN OF zcx_erro_psq_banco_dados,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '120',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_psq_banco_dados .
    CONSTANTS:
      BEGIN OF zcx_nf_propria_nfe,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '121',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nf_propria_nfe .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_outro_part,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '028',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_outro_part .
    CONSTANTS:
      BEGIN OF zcx_nao_inf_outro_part,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '029',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_inf_outro_part .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_safra,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '036',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_safra .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_trator,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '041',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_trator .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_doc_fiscal,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '047',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_doc_fiscal .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_lc_entrega,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '046',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_lc_entrega .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_ps_liquido,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '045',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_ps_liquido .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_ps_subtotal,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '044',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_ps_subtotal .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_ps_tara,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '043',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_ps_tara .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_ps_bruto,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '042',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_ps_bruto .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_produto,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '040',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_produto .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_ordem_venda,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '038',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_ordem_venda .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_tp_entrada,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '039',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_tp_entrada .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_lc_negocio,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '037',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_lc_negocio .
    CONSTANTS:
      BEGIN OF zcx_nao_permitido_md_fiscal,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '055',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_permitido_md_fiscal .
    CONSTANTS:
      BEGIN OF zcx_te_md_fiscal_sem_param,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '056',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_te_md_fiscal_sem_param .
    CONSTANTS:
      BEGIN OF zcx_te_sem_bnc_empresa,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '062',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_te_sem_bnc_empresa .
    CONSTANTS:
      BEGIN OF zcx_te_sem_ch_bloqueio,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '061',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_te_sem_ch_bloqueio .
    CONSTANTS:
      BEGIN OF zcx_te_sem_ct_fiscal,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '057',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_te_sem_ct_fiscal .
    CONSTANTS:
      BEGIN OF zcx_te_sem_form_pagamento,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '060',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_te_sem_form_pagamento .
    CONSTANTS:
      BEGIN OF zcx_te_sem_iva,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '059',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_te_sem_iva .
    CONSTANTS:
      BEGIN OF zcx_te_sem_tp_mov_merc,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '058',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_te_sem_tp_mov_merc .
    CONSTANTS:
      BEGIN OF zcx_te_somente_fisica,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '065',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_te_somente_fisica .
    CONSTANTS:
      BEGIN OF zcx_te_somente_juridica,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '066',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_te_somente_juridica .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_motorista,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '073',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_motorista .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_ticket,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '075',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_ticket .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_proprietario,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '074',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_proprietario .
    CONSTANTS:
      BEGIN OF zcx_ordem_fornecedor,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '081',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_fornecedor .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_ie_prod,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '093',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_ie_prod .
    CONSTANTS:
      BEGIN OF zcx_forn_sem_parametro,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '095',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF zcx_forn_sem_parametro .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_lc_coleta,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '107',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_lc_coleta .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_lc_destino,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '108',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_lc_destino .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_lc_descarga,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '109',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_lc_descarga .
    CONSTANTS:
      BEGIN OF zcx_errp_ps_subtotal,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '063',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_errp_ps_subtotal .
    CONSTANTS:
      BEGIN OF zcx_le_sem_param_lc_negocio,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '053',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_le_sem_param_lc_negocio .
    CONSTANTS:
      BEGIN OF zcx_le_sem_param_material,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '054',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_le_sem_param_material .
    CONSTANTS:
      BEGIN OF zcx_ie_erro_fornecedor,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '094',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ie_erro_fornecedor .
    CONSTANTS:
      BEGIN OF zcx_nota_deve_ser_normal,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '125',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nota_deve_ser_normal .
    CONSTANTS:
      BEGIN OF zcx_erro_preco_saca,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '126',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_preco_saca .
    CONSTANTS:
      BEGIN OF zcx_erro_ticket_utilizado,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '127',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_ticket_utilizado .
    CONSTANTS:
      BEGIN OF zcx_job_estorno_exec,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '128',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_job_estorno_exec .
    CONSTANTS:
      BEGIN OF zcx_erro_romaneio_nota,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '129',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_romaneio_nota .
    CONSTANTS:
      BEGIN OF zcx_erro_romaneio_nota_falta,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '130',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_romaneio_nota_falta .
    CONSTANTS:
      BEGIN OF zcx_ordem_destino,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '131',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_destino .
    CONSTANTS:
      BEGIN OF zcx_ordem_descarga,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '132',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_descarga .
    CONSTANTS:
      BEGIN OF zcx_nao_gerou_romaneio_saida,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '133',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_gerou_romaneio_saida .
    CONSTANTS:
      BEGIN OF zcx_estornar_rom_saida,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '134',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_estornar_rom_saida .
    CONSTANTS:
      BEGIN OF zcx_data_emissao_nf,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '136',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_data_emissao_nf .
    CONSTANTS:
      BEGIN OF zcx_data_formulario_venc,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '135',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_data_formulario_venc .
    CONSTANTS:
      BEGIN OF zcx_sem_retorno_webservice,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '138',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_retorno_webservice .
    CONSTANTS:
      BEGIN OF zcx_tp_entrada_nao_permitido,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '139',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_tp_entrada_nao_permitido .
    CONSTANTS:
      BEGIN OF zcx_fornecedor_nao_classifica,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '140',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_fornecedor_nao_classifica .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_cfop,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '141',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_cfop .
    CONSTANTS:
      BEGIN OF zcx_cfop_nao_permitido_te,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '142',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_cfop_nao_permitido_te .
    CONSTANTS:
      BEGIN OF zcx_cfop_nao_permitido_forn,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '143',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_cfop_nao_permitido_forn .
    CONSTANTS:
      BEGIN OF zcx_xml_nfe_nao_recebido,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '144',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_xml_nfe_nao_recebido .
    CONSTANTS:
      BEGIN OF zcx_xml_nfe_cfop_invalido,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '145',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_xml_nfe_cfop_invalido .
    CONSTANTS:
      BEGIN OF zcx_ordem_venda_cif_od,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '148',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_venda_cif_od .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_chave_nfe,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '149',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_chave_nfe .
    CONSTANTS:
      BEGIN OF zcx_nfe_nao_distribuida,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '150',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nfe_nao_distribuida .
    CONSTANTS:
      BEGIN OF zcx_nfe_item_nao_distribuido,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '151',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nfe_item_nao_distribuido .
    CONSTANTS:
      BEGIN OF zcx_nfe_item_unidade,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '152',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nfe_item_unidade .
    CONSTANTS:
      BEGIN OF zcx_nao_teste_amaggi_positivo,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '153',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_teste_amaggi_positivo .
    CONSTANTS:
      BEGIN OF zcx_sem_saldo_ord_venda,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '155',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_saldo_ord_venda .
    CONSTANTS:
      BEGIN OF zcx_erro_peso_ord_venda,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '156',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_peso_ord_venda .
    CONSTANTS:
      BEGIN OF zcx_saldo_estoque,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '157',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF zcx_saldo_estoque .
    CONSTANTS:
      BEGIN OF zcx_centro_a_fixar,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '158',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_centro_a_fixar .
    CONSTANTS:
      BEGIN OF zcx_erro_tomador_nfe,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '159',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_tomador_nfe .
    CONSTANTS:
      BEGIN OF zcx_erro_agente_proprietario,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '160',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_agente_proprietario .
    CONSTANTS:
      BEGIN OF zcx_erro_determinar_transg,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '161',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_determinar_transg .
    CONSTANTS:
      BEGIN OF zcx_erro_proprietario,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '162',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_proprietario .
    CONSTANTS:
      BEGIN OF zcx_cfop_unico_carga,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '163',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_cfop_unico_carga .
    CONSTANTS:
      BEGIN OF zcx_obg_emp_classificadora,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '164',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_emp_classificadora .
    CONSTANTS:
      BEGIN OF zcx_obg_resultado_class,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '165',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_resultado_class .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_safra_psq,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '168',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_safra_psq .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_empresa_psq,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '169',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_empresa_psq .
    CONSTANTS:
      BEGIN OF zcx_obg_inf_filial_psq,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '170',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_inf_filial_psq .
    CONSTANTS:
      BEGIN OF zcx_obg_class_umidade,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '171',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_class_umidade .
    CONSTANTS:
      BEGIN OF zcx_obg_class_impureza,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '172',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_class_impureza .
    CONSTANTS:
      BEGIN OF zcx_obg_class_avariado,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '173',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_class_avariado .
    CONSTANTS:
      BEGIN OF zcx_obg_class_ardido,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '174',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_class_ardido .
    CONSTANTS:
      BEGIN OF zcx_obg_class_quebrado,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '175',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_class_quebrado .
    CONSTANTS:
      BEGIN OF zcx_obg_class_esverdeado,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '176',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_class_esverdeado .
    CONSTANTS:
      BEGIN OF zcx_sol_manut_reservada,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '177',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sol_manut_reservada .
    CONSTANTS:
      BEGIN OF zcx_workflow_aprovado,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '178',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_workflow_aprovado .
    CONSTANTS:
      BEGIN OF zcx_obg_class_carunchado,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '247',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obg_class_carunchado .
    CONSTANTS:
      BEGIN OF zcx_workflow_recusado,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '179',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_workflow_recusado .
    CONSTANTS:
      BEGIN OF zcx_tipo_aceite_manut_rom,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '182',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_tipo_aceite_manut_rom .
    CONSTANTS:
      BEGIN OF zcx_tipo_resposta_manut_rom,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '183',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_tipo_resposta_manut_rom .
    CONSTANTS:
      BEGIN OF zcx_param_sobra_material,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '184',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_param_sobra_material .
    CONSTANTS:
      BEGIN OF zcx_nao_gerou_sobra,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '185',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_gerou_sobra .
    CONSTANTS:
      BEGIN OF zcx_nao_estornou_sobra,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '186',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_estornou_sobra .
    CONSTANTS:
      BEGIN OF zcx_nao_alterou_rom_ent_sap,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '187',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_alterou_rom_ent_sap .
    CONSTANTS:
      BEGIN OF zcx_nao_altera_original,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '188',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_altera_original .
    CONSTANTS:
      BEGIN OF zcx_erro_ncm,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '189',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_ncm .
    CONSTANTS:
      BEGIN OF zcx_erro_ncm_desc,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '190',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF zcx_erro_ncm_desc .
    CONSTANTS:
      BEGIN OF zcx_sol_manut_executada,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '191',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sol_manut_executada .
    CONSTANTS:
      BEGIN OF zcx_nota_romaneio_entrada,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '192',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nota_romaneio_entrada .
    CONSTANTS:
      BEGIN OF zcx_pedido_compra_nao_enc,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '209',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_pedido_compra_nao_enc .
    CONSTANTS:
      BEGIN OF zcx_pedido_compra_ser_fob,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '210',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_pedido_compra_ser_fob .
    CONSTANTS:
      BEGIN OF zcx_pedido_compra_ef_saida,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '211',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_pedido_compra_ef_saida .
    CONSTANTS:
      BEGIN OF zcx_pedido_compra_safra,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '212',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_pedido_compra_safra .
    CONSTANTS:
      BEGIN OF zcx_pedido_compra_ef_entra,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '213',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_pedido_compra_ef_entra .
    CONSTANTS:
      BEGIN OF zcx_carga_nao_encontrada,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '214',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_carga_nao_encontrada .
    CONSTANTS:
      BEGIN OF zcx_sol_manutencao_process,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '215',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sol_manutencao_process .
    CONSTANTS:
      BEGIN OF zcx_sol_manutencao_wait,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '216',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sol_manutencao_wait .
    CONSTANTS:
      BEGIN OF zcx_carga_ent_nao_conferida,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '229',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_carga_ent_nao_conferida .
    CONSTANTS:
      BEGIN OF zcx_nao_inf_material,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '237',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_inf_material .
    CONSTANTS:
      BEGIN OF zcx_saida_nao_automatica,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '250',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_saida_nao_automatica .
    CONSTANTS:
      BEGIN OF zcx_erro_soma_avariado,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '236',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_soma_avariado .
    CONSTANTS:
      BEGIN OF zcx_saida_automatica_exec,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '251',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_saida_automatica_exec .
    CONSTANTS:
      BEGIN OF zcx_doc_transporte_nao_existe,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '252',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_doc_transporte_nao_existe .
    CONSTANTS:
      BEGIN OF zcx_rom_status_opus,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '254',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_rom_status_opus .
    CONSTANTS:
      BEGIN OF zcx_ordem_coleta,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '255',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_coleta .
    CONSTANTS:
      BEGIN OF zcx_nfe_many_rows,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '256',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nfe_many_rows .
    CONSTANTS:
      BEGIN OF zcx_erro_observacao_nota,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '257',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_observacao_nota .
    CONSTANTS:
      BEGIN OF zcx_nr_ticket_grande,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '258',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nr_ticket_grande .
    CONSTANTS:
      BEGIN OF zcx_nota_take_bloco_acima,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '260',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nota_take_bloco_acima .
    CONSTANTS:
      BEGIN OF zcx_nota_take_bloco_difer,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '261',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nota_take_bloco_difer .
    CONSTANTS:
      BEGIN OF zcx_sol_manu_nao_confe,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '262',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sol_manu_nao_confe .
    CONSTANTS:
      BEGIN OF zcx_first_rom_manu_exclui,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '263',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_first_rom_manu_exclui .
    CONSTANTS:
      BEGIN OF zcx_ov_saldo_fardo,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '265',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ov_saldo_fardo .
    CONSTANTS:
      BEGIN OF zcx_ov_saldo_volume,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '266',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ov_saldo_volume .
    CONSTANTS:
      BEGIN OF zcx_oc_vinculada_ov,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '268',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_oc_vinculada_ov .
    CONSTANTS:
      BEGIN OF zcx_nota_fardos_acima,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '269',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nota_fardos_acima .
    CONSTANTS:
      BEGIN OF zcx_nota_fardos_difer,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '270',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nota_fardos_difer .
    CONSTANTS:
      BEGIN OF zcx_nota_without_take_bloco,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '271',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nota_without_take_bloco .
    CONSTANTS:
      BEGIN OF zcx_nota_without_fardo,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '272',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nota_without_fardo .
    CONSTANTS:
      BEGIN OF zcx_dif_ordem_tipo_frete,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '274',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_dif_ordem_tipo_frete .
    CONSTANTS:
      BEGIN OF zcx_dif_ordem_ponto_coleta,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '275',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_dif_ordem_ponto_coleta .
    CONSTANTS:
      BEGIN OF zcx_dif_ordem_local_entrega,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '276',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_dif_ordem_local_entrega .
    CONSTANTS:
      BEGIN OF zcx_dif_ordem_destino,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '277',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_dif_ordem_destino .
    CONSTANTS:
      BEGIN OF zcx_bloco_saldo_fardo,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '259',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_bloco_saldo_fardo .
    CONSTANTS:
      BEGIN OF zcx_bloco_saldo_peso,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '273',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_bloco_saldo_peso .
    CONSTANTS:
      BEGIN OF zcx_bloco_nao_encontrado,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '278',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_bloco_nao_encontrado .
    CONSTANTS:
      BEGIN OF zcx_serie_sigam,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '282',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_serie_sigam .
    CONSTANTS:
      BEGIN OF zcx_qtd_nro_nota_fiscal,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '283',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_qtd_nro_nota_fiscal .
    CONSTANTS:
      BEGIN OF zcx_ped_compra_retorno_obg,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '284',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ped_compra_retorno_obg .
    CONSTANTS:
      BEGIN OF zcx_ped_compra_retorno_err,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '285',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ped_compra_retorno_err .
    CONSTANTS:
      BEGIN OF zcx_nf_exportada,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '286',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nf_exportada .
    CONSTANTS:
      BEGIN OF zcx_tipo_entrada_cfop,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '287',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_tipo_entrada_cfop .
    CONSTANTS:
      BEGIN OF zcx_ck_revisao,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '294',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ck_revisao .
    CONSTANTS:
      BEGIN OF zcx_tpentrada_empresa_err,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '146',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_tpentrada_empresa_err .
    CONSTANTS:
      BEGIN OF zcx_valida_safra,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_valida_safra .
    CONSTANTS:
      BEGIN OF zcx_dif_material,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '296',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_dif_material .
    CONSTANTS:
      BEGIN OF zcx_ordem_venda_contrato,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '297',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ordem_venda_contrato .
    CONSTANTS:
      BEGIN OF zcx_dif_contrato,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '298',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_dif_contrato .

    CONSTANTS:
      BEGIN OF zcx_err_ordext,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '300',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_err_ordext .

    CONSTANTS:
      BEGIN OF zcx_err_te,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '301',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_err_te .

    CONSTANTS:
      BEGIN OF zcx_err_irf,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '302',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_err_irf.

    CONSTANTS:
      BEGIN OF zcx_err_regio,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '303',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_err_regio.

    CONSTANTS:
      BEGIN OF zcx_erro_entrada_opus,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '304',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF zcx_erro_entrada_opus.

    CONSTANTS:
      BEGIN OF zcx_erro_ov_cfop,
        msgid TYPE symsgid VALUE 'ZCARGA',
        msgno TYPE symsgno VALUE '305',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF zcx_erro_ov_cfop.

    DATA msgty TYPE syst_msgty .
    DATA msgno TYPE syst_msgno .
    DATA msgv1 TYPE syst_msgv .
    DATA msgv2 TYPE syst_msgv .
    DATA msgv3 TYPE syst_msgv .
    DATA msgv4 TYPE syst_msgv .
    DATA msgid TYPE syst_msgid .
    DATA transacao TYPE tcode .
*  data ZCX_ERR_ORDEXT .

    METHODS constructor
      IMPORTING
        !textid         LIKE if_t100_message=>t100key OPTIONAL
        !previous       LIKE previous OPTIONAL
        !msgty          TYPE syst_msgty OPTIONAL
        !msgno          TYPE syst_msgno OPTIONAL
        !msgv1          TYPE syst_msgv OPTIONAL
        !msgv2          TYPE syst_msgv OPTIONAL
        !msgv3          TYPE syst_msgv OPTIONAL
        !msgv4          TYPE syst_msgv OPTIONAL
        !msgid          TYPE syst_msgid OPTIONAL
        !transacao      TYPE tcode OPTIONAL
        !zcx_err_ordext LIKE zcx_err_ordext OPTIONAL .
    METHODS published_erro
      IMPORTING
        !i_msgty         TYPE syst_msgty OPTIONAL
        !i_msgty_display TYPE syst_msgty OPTIONAL .
protected section.
private section.
ENDCLASS.



CLASS ZCX_CARGA IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGTY = MSGTY .
me->MSGNO = MSGNO .
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
me->MSGID = MSGID .
me->TRANSACAO = TRANSACAO .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD PUBLISHED_ERRO.

    DATA: P_MSGTY	        TYPE SYST_MSGTY,
          P_MSGTY_DISPLAY	TYPE SYST_MSGTY.


    IF I_MSGTY IS NOT INITIAL.
      P_MSGTY = I_MSGTY.
    ELSE.
      P_MSGTY = ME->MSGTY.
    ENDIF.

    IF I_MSGTY_DISPLAY IS NOT INITIAL.
      P_MSGTY_DISPLAY = I_MSGTY_DISPLAY.
    ELSE.
      P_MSGTY_DISPLAY = ME->MSGTY.
    ENDIF.

    IF ME->TRANSACAO IS NOT INITIAL.
      IF ME->MSGV1 IS INITIAL.
        ME->MSGV1 = ME->TRANSACAO.
      ENDIF.
      IF ME->MSGV2 IS INITIAL.
        ME->MSGV2 = ME->TRANSACAO.
      ENDIF.
      IF ME->MSGV3 IS INITIAL.
        ME->MSGV3 = ME->TRANSACAO.
      ENDIF.
      IF ME->MSGV4 IS INITIAL.
        ME->MSGV4 = ME->TRANSACAO.
      ENDIF.
    ENDIF.

    MESSAGE ID ME->MSGID TYPE P_MSGTY NUMBER ME->MSGNO WITH ME->MSGV1 ME->MSGV2 ME->MSGV3 ME->MSGV4 DISPLAY LIKE P_MSGTY_DISPLAY.

  ENDMETHOD.
ENDCLASS.
