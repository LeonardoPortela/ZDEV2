@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Solicitações Entrega Insumos - Analitico - Projeção'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZC_SD_SOL_ENT_INS_ANALITICO
  as projection on ZI_SD_SOL_ENT_INS_ANALITICO
{
       @EndUserText.label: 'Sol.Nro.'
  key  solic_nro,

       @EndUserText.label: 'Sol.Seq'
  key  solic_seq,

       @EndUserText.label: 'Sol.OV'
  key  solic_ordem,

       @EndUserText.label: 'Sol.OV.Itm.'
  key  solic_ordem_item,

       @EndUserText.label: 'Sol.Linha Saldo'
  key  solic_line_saldo,

       @EndUserText.label: 'Dist.Nro.Sol'
  key  distrib_nro_sol,

       @EndUserText.label: 'Dist.Seq.'
  key  distrib_seq,

       @EndUserText.label: 'Dist.OV'
  key  distrib_ordem,

       @EndUserText.label: 'Dist.OV.Itm'
  key  distrib_ordem_item,

       @EndUserText.label: 'Dist.Linha Saldo'
  key  distrib_line_saldo,

       @EndUserText.label: 'Carga.Nro'
  key  carga_nro,

       @EndUserText.label: 'Carga.Sol.'
  key  carga_nro_sol,

       @EndUserText.label: 'Carga.Seq.'
  key  carga_seq_lib,

       @EndUserText.label: 'Carga.OV'
  key  carga_ordem,

       @EndUserText.label: 'Carga.OV Item'
  key  carga_ordem_item,

       @EndUserText.label: 'Sol.Origem'
       solic_nro_sol_origem,

       @EndUserText.label: 'Org.Venda'
       solic_organizacao_venda,

       @EndUserText.label: 'St.Atv.'
       solic_setor_atividade,

       @EndUserText.label: 'Esc.Venda.'
       solic_escritorio_venda,

       @EndUserText.label: 'Cen.Fat.'
       solic_centro_faturamento,

       @EndUserText.label: 'Tp.OV'
       solic_tipo_ov,
       
       @EndUserText.label: 'Material'
       @ObjectModel.text.element: ['material_name']
       @Consumption.valueHelpDefinition: [{ entity:  { name: 'I_MaterialText', element: 'Material' } }]
       material,
       
       @EndUserText.label: 'Ds.Material'
       material_name,
       
       @EndUserText.label: 'Cultivar'
       material_cutivar,

       @EndUserText.label: 'Cliente'
       @ObjectModel.text.element: ['solic_ordem_ds_cliente']
       @Consumption.valueHelpDefinition: [{ entity:  { name: 'ZI_SD_CLIENTE_VH', element: 'Customer' } }]
       solic_ordem_cliente,
       solic_ordem_ds_cliente,

       @EndUserText.label: 'Nr.Rot.Ent.'
       solic_nr_roteiro_entrega,

       @EndUserText.label: 'Ds.Rot.Ent.'
       solic_ds_roteiro_entrega,

       @EndUserText.label: 'Sol.Qtde'
       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       solic_quantidade,
       
       @EndUserText.label: 'Sol.Qtd.Dist.'
       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       solic_qtde_distrib,

       @EndUserText.label: 'Sol.Sld.Dist.'
       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       solic_saldo_distribuir,

       @EndUserText.label: 'Unid.'
       solic_unidade,

       @EndUserText.label: 'Sol.Dt.'
       solic_data,

       @EndUserText.label: 'Dt.Sol.Raiz'
       solic_data_sol_raiz,

       @EndUserText.label: 'Nro.Sol.Raiz'
       solic_nro_sol_raiz,

       @EndUserText.label: 'Sol.Us.Criação'
       solic_usuario_criacao,

       @EndUserText.label: 'Sol.St.'
       @ObjectModel.text.element: ['solic_ds_status']
       @Consumption.valueHelpDefinition: [{ entity:  { name: 'ZI_SD_STATUS_SOLIC_INSUMOS_VH', element: 'status' } }]
       solic_status,

       @EndUserText.label: 'Sol.Ds.St.'
       solic_ds_status,

       @EndUserText.label: 'Sol.Dt.Canc.'
       solic_data_cancelamento,

       @EndUserText.label: 'Sol.Us.Canc.'
       solic_usuario_cancelamento,

       @EndUserText.label: 'Dt.Entrega.Sol.'
       solic_data_entrega,
       @EndUserText.label: 'Tp.Oper.'
       solic_tipo_operacao,

       @EndUserText.label: 'Org.Est.'
       solic_origem_estoque,

       @EndUserText.label: 'Sol.Pedido'
       solic_pedido,

       @EndUserText.label: 'Sol.Ped.Itm'
       solic_pedido_item,

       @EndUserText.label: 'Sol.Forn.Arm.'
       solic_fornecedor_arm,

       @EndUserText.label: 'Sol.Lote'
       solic_lote,
       @EndUserText.label: 'Sol.Marca'
       solic_marca,

       @EndUserText.label: 'Sol.Cg.Auto.'
       solic_carga_automatica,

       @EndUserText.label: 'Sol.Flexib.'
       solic_flexibilidade,

       @EndUserText.label: 'Sol.Prioridade'
       solic_prioridade,

       @EndUserText.label: 'Sol.Transf.Forn.'
       solic_transf_no_fornecedor,

       @EndUserText.label: 'Dist.Qtde'
       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       distrib_quantidade,
       
       @EndUserText.label: 'Dist.Qtde.Carga'
       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       distrib_qtde_em_carga,

       @EndUserText.label: 'Dist.Sld.Form.Carga'
       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       distrib_saldo_formar_carga,

       @EndUserText.label: 'Dist.Dt.'
       distrib_data,

       @EndUserText.label: 'Dist.Nr.Rot.Coleta'
       distrib_nr_roteiro_coleta,

       @EndUserText.label: 'Dist.Ds.Rot.Coleta'
       distrib_ds_roteiro_coleta,

       @EndUserText.label: 'Dist.Us.'
       distrib_usuario,

       @EndUserText.label: 'Dist.St.'
       @ObjectModel.text.element: ['distrib_ds_status']
       @Consumption.valueHelpDefinition: [{ entity:  { name: 'ZI_SD_STATUS_DISTRIB_INSUMO_VH', element: 'status' } }]
       --@Consumption.filter: { selectionType: #SINGLE, defaultValue: '!2'  }              
       distrib_status,

       @EndUserText.label: 'Dist.Ds.St.'
       distrib_ds_status,

       @EndUserText.label: 'Dist.Dt.Canc.'
       distrib_data_cancelamento,

       @EndUserText.label: 'Dist.Us.Canc.'
       distrib_usuario_cancelamento,



       @EndUserText.label: 'Carga.Qtde'
       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       carga_quantidade,

       @EndUserText.label: 'Carga.Id.Safra.C.'
       carga_id_safra_control,

       @EndUserText.label: 'Carga.Viagem Carguero'
       carga_id_viagem_carguero,

       @EndUserText.label: 'Carga.Incoterms'
       carga_incoterms,

       @EndUserText.label: 'Carga.Dt.Criação'
       carga_data_criacao,

       @EndUserText.label: 'Carga.Us.Criação'
       carga_usuario_criacao,

       @EndUserText.label: 'Carga.Dt.Env.Cot.'
       carga_data_envio_cotacao,

       @EndUserText.label: 'Carga.Dt.Contrato.Frete'
       carga_data_frete_contratado,

       @EndUserText.label: 'Carga.Dt.Aut.Emb.'
       carga_data_aut_embarque,

       @EndUserText.label: 'Carga.Dt.Carregamento'
       carga_data_carregamento,

       @EndUserText.label: 'Carga.Dt.Conf.'
       carga_data_conferencia,

       @EndUserText.label: 'Carga.Dt.Faturamento'
       carga_data_finaliza_fatura,

       @EndUserText.label: 'Carga.Dt.Canc.'
       carga_data_cancelamento,

       @EndUserText.label: 'Carga.Hr.Canc.'
       carga_hora_cancelamento,

       @EndUserText.label: 'Carga.Us.Canc.'
       carga_usuario_cancelamento,

       @EndUserText.label: 'Carga.St.'
       @ObjectModel.text.element: ['carga_ds_status']
       @Consumption.valueHelpDefinition: [{ entity:  { name: 'ZI_SD_STATUS_CARGA_INSUMOS_VH', element: 'status' } }]
       carga_status,

       @EndUserText.label: 'Carga.Ds.St.'
       carga_ds_status,

       @EndUserText.label: 'Carga.St.Entrega'
       carga_status_entrega,

       @EndUserText.label: 'Dias Distrib.'
       dias_distribuicao,

       @EndUserText.label: 'Dias Criar.Crg.'
       dias_criacao_carga,

       @EndUserText.label: 'Dias Env.Cot.'
       dias_envio_cotacao_carga,

       @EndUserText.label: 'Dias Cont.Frete'
       dias_contratacao_frete_carga,

       @EndUserText.label: 'Dias Aut.Emb.'
       dias_autorizacao_emb_carga,

       @EndUserText.label: 'Dias Carreg.'
       dias_carregamento_carga,

       @EndUserText.label: 'Dias Conf.'
       dias_conferencia_carga,

       @EndUserText.label: 'Dias Fat.Carga'
       dias_faturamento_carga,

       @EndUserText.label: 'Sit.Distrib.'
       situacao_distrib,

       @EndUserText.label: 'Sit.Distrib.Crit.'
       situacao_distrib_crit,

       @EndUserText.label: 'Sit.Criar.Crg.'
       situacao_criacao_carga,

       @EndUserText.label: 'Sit.Criar.Crg.Crit.'
       situacao_criacao_carga_crit,

       @EndUserText.label: 'Sit.Env.Cotação'
       situacao_envio_cotacao,

       @EndUserText.label: 'Sit.Env.Cotação Crit.'
       situacao_envio_cotacao_crit,

       @EndUserText.label: 'Sit.Cont.Frete'
       situacao_contratacao_frete,

       @EndUserText.label: 'Sit.Cont.Frete.Crit'
       situacao_contrat_frete_crit,

       @EndUserText.label: 'Sit.Aut.Emb.'
       situacao_autorizacao_embarque,

       @EndUserText.label: 'Sit.Aut.Emb.Crit'
       situacao_aut_embarque_crit,

       @EndUserText.label: 'Sit.Carregamento'
       situacao_carregamento,

       @EndUserText.label: 'Sit.Carregamento Crit.'
       situacao_carregamento_crit,

       @EndUserText.label: 'Sit.Conferência'
       situacao_conferencia,

       @EndUserText.label: 'Sit.Conferência Crit.'
       situacao_conferencia_crit,

       @EndUserText.label: 'Sit.Faturamento'
       situacao_faturamento,

       @EndUserText.label: 'Sit.Faturamento Crit.'
       situacao_faturamento_crit
       


}
