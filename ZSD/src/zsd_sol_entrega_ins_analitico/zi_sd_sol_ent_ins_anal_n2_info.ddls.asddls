@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Solicitações Entrega Insumos - Analitico'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SD_SOL_ENT_INS_ANAL_N2_INFO
  as select from    ZI_SD_SOL_ENT_INS_ANALISE_INFO as sl

    left outer join zsdt0423                       as pz on pz.id = '0'

    inner join      zsdt0060                       as PE on  sl.solic_escritorio_venda = PE.vkbur
                                                         and PE.programa               = 'ZSDR016'
                                                         and PE.usnam                  = $session.user

{

  key  sl.solic_nro                    as solic_nro,
  key  sl.solic_seq                    as solic_seq,
  key  sl.solic_ordem                  as solic_ordem,
  key  sl.solic_ordem_item             as solic_ordem_item,
  key  sl.solic_line_saldo             as solic_line_saldo,


  key  sl.distrib_nro_sol              as distrib_nro_sol,
  key  sl.distrib_seq                  as distrib_seq,
  key  sl.distrib_ordem                as distrib_ordem,
  key  sl.distrib_ordem_item           as distrib_ordem_item,
  key  sl.distrib_line_saldo           as distrib_line_saldo,


  key  sl.carga_nro                    as carga_nro,
  key  sl.carga_nro_sol                as carga_nro_sol,
  key  sl.carga_seq_lib                as carga_seq_lib,
  key  sl.carga_ordem                  as carga_ordem,
  key  sl.carga_ordem_item             as carga_ordem_item,

       /* Solicitações */
       sl.solic_nro_sol_origem         as solic_nro_sol_origem,
       sl.solic_organizacao_venda      as solic_organizacao_venda,
       sl.solic_setor_atividade        as solic_setor_atividade,
       sl.solic_escritorio_venda       as solic_escritorio_venda,
       sl.solic_centro_faturamento     as solic_centro_faturamento,
       sl.solic_tipo_ov                as solic_tipo_ov,

       sl.material                     as material,
       sl.material_name                as material_name,
       sl.material_cutivar             as material_cutivar,


       sl.solic_ordem_cliente          as solic_ordem_cliente,
       sl.solic_ordem_ds_cliente       as solic_ordem_ds_cliente,
       sl.solic_nr_roteiro_entrega     as solic_nr_roteiro_entrega,
       sl.solic_ds_roteiro_entrega     as solic_ds_roteiro_entrega,


       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       sl.solic_quantidade             as solic_quantidade,

       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       sl.solic_qtde_distrib           as solic_qtde_distrib,

       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       sl.solic_saldo_distribuir       as solic_saldo_distribuir,
       sl.solic_unidade                as solic_unidade,
       sl.solic_data                   as solic_data,
       sl.solic_data_sol_raiz          as solic_data_sol_raiz,
       sl.solic_nro_sol_raiz           as solic_nro_sol_raiz,
       sl.solic_usuario_criacao        as solic_usuario_criacao,
       sl.solic_status                 as solic_status,
       sl.solic_ds_status              as solic_ds_status,
       sl.solic_data_cancelamento      as solic_data_cancelamento,
       sl.solic_usuario_cancelamento   as solic_usuario_cancelamento,
       sl.solic_data_entrega           as solic_data_entrega,
       sl.solic_tipo_operacao          as solic_tipo_operacao,
       sl.solic_origem_estoque         as solic_origem_estoque,
       sl.solic_pedido                 as solic_pedido,
       sl.solic_pedido_item            as solic_pedido_item,
       sl.solic_fornecedor_arm         as solic_fornecedor_arm,
       sl.solic_lote                   as solic_lote,
       sl.solic_marca                  as solic_marca,
       sl.solic_carga_automatica       as solic_carga_automatica,
       sl.solic_flexibilidade          as solic_flexibilidade,
       sl.solic_prioridade             as solic_prioridade,
       sl.solic_transf_no_fornecedor   as solic_transf_no_fornecedor,

       /* Distribuições */

       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       sl.distrib_quantidade           as distrib_quantidade,

       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       sl.distrib_qtde_em_carga        as distrib_qtde_em_carga,

       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       sl.distrib_saldo_formar_carga   as distrib_saldo_formar_carga,
       sl.distrib_data                 as distrib_data,
       sl.distrib_nr_roteiro_coleta    as distrib_nr_roteiro_coleta,
       sl.distrib_ds_roteiro_coleta    as distrib_ds_roteiro_coleta,
       sl.distrib_usuario              as distrib_usuario,
       sl.distrib_status               as distrib_status,
       sl.distrib_ds_status            as distrib_ds_status,
       sl.distrib_data_cancelamento    as distrib_data_cancelamento,
       sl.distrib_usuario_cancelamento as distrib_usuario_cancelamento,

       /* Cargas */
       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       sl.carga_quantidade             as carga_quantidade,
       sl.carga_id_safra_control       as carga_id_safra_control,
       sl.carga_id_viagem_carguero     as carga_id_viagem_carguero,
       sl.carga_incoterms              as carga_incoterms,
       sl.carga_data_criacao           as carga_data_criacao,
       sl.carga_usuario_criacao        as carga_usuario_criacao,
       sl.carga_data_envio_cotacao     as carga_data_envio_cotacao,
       sl.carga_data_frete_contratado  as carga_data_frete_contratado,
       sl.carga_data_aut_embarque      as carga_data_aut_embarque,
       sl.carga_data_carregamento      as carga_data_carregamento,
       sl.carga_data_conferencia       as carga_data_conferencia,
       sl.carga_data_finaliza_fatura   as carga_data_finaliza_fatura,
       sl.carga_data_cancelamento      as carga_data_cancelamento,
       sl.carga_hora_cancelamento      as carga_hora_cancelamento,
       sl.carga_usuario_cancelamento   as carga_usuario_cancelamento,
       sl.carga_status                 as carga_status,
       sl.carga_ds_status              as carga_ds_status,
       sl.carga_status_entrega         as carga_status_entrega,

       /* Prazos */
       sl.dias_distribuicao            as dias_distribuicao,
       sl.dias_criacao_carga           as dias_criacao_carga,
       sl.dias_envio_cotacao_carga     as dias_envio_cotacao_carga,
       sl.dias_contratacao_frete_carga as dias_contratacao_frete_carga,
       sl.dias_autorizacao_emb_carga   as dias_autorizacao_emb_carga,
       sl.dias_carregamento_carga      as dias_carregamento_carga,
       sl.dias_conferencia_carga       as dias_conferencia_carga,
       sl.dias_faturamento_carga       as dias_faturamento_carga,

       /* Prazos - Flags de Alerta e Critico */

       /* Distribuição */
       case
         when ( coalesce( pz.distrib_solic_critico, 0 ) > 0 ) and ( sl.dias_distribuicao > coalesce( pz.distrib_solic_critico, 0 ) )             then
          'C'
         else
           case
             when ( coalesce( pz.distrib_solic_alerta, 0 ) > 0 ) and ( sl.dias_distribuicao > coalesce( pz.distrib_solic_alerta, 0 ) )             then
              'A'
            end
       end                             as situacao_distrib,

       /* Criação Carga */
       case
         when ( coalesce( pz.criar_carga_critico, 0 ) > 0 ) and ( sl.dias_criacao_carga > coalesce( pz.criar_carga_critico, 0 ) )             then
          'C'
         else
           case
             when ( coalesce( pz.criar_carga_alerta, 0 ) > 0 ) and ( sl.dias_criacao_carga > coalesce( pz.criar_carga_alerta, 0 ) )              then
              'A'
            end
       end                             as situacao_criacao_carga,


       /* Envio Cotação */
       case
         when ( coalesce( pz.envio_cotacao_critico, 0 ) > 0 ) and ( sl.dias_envio_cotacao_carga > coalesce( pz.envio_cotacao_critico, 0 ) )             then
          'C'
         else
           case
             when ( coalesce( pz.envio_cotacao_alerta, 0 ) > 0 ) and ( sl.dias_envio_cotacao_carga > coalesce( pz.envio_cotacao_alerta, 0 ) )           then
              'A'
            end
       end                             as situacao_envio_cotacao,

       /* Contratação Frete */
       case
         when ( coalesce( pz.contrata_frete_critico, 0 ) > 0 ) and ( sl.dias_contratacao_frete_carga > coalesce( pz.contrata_frete_critico, 0 ) )             then
          'C'
         else
           case
             when ( coalesce( pz.contrata_frete_alerta, 0 ) > 0 ) and ( sl.dias_contratacao_frete_carga > coalesce( pz.contrata_frete_alerta, 0 ) )           then
              'A'
            end
       end                             as situacao_contratacao_frete,


       /* Autorizar Embarque */
       case
         when ( coalesce( pz.autoriza_embarque_critico, 0 ) > 0 ) and ( sl.dias_autorizacao_emb_carga > coalesce( pz.autoriza_embarque_critico, 0 ) )             then
          'C'
         else
           case
             when ( coalesce( pz.autoriza_embarque_alerta, 0 ) > 0 ) and ( sl.dias_autorizacao_emb_carga > coalesce( pz.autoriza_embarque_alerta, 0 ) )   then
              'A'
            end
       end                             as situacao_autorizacao_embarque,

       /* Carregamento  */
       case
         when ( coalesce( pz.carregamento_critico, 0 ) > 0 ) and ( sl.dias_carregamento_carga > coalesce( pz.carregamento_critico, 0 ) )             then
          'C'
         else
           case
             when ( coalesce( pz.carregamento_alerta, 0 ) > 0 ) and ( sl.dias_carregamento_carga > coalesce( pz.carregamento_alerta, 0 ) )       then
              'A'
            end
       end                             as situacao_carregamento,

       /* Conferencia  */
       case
         when ( coalesce( pz.conferencia_carga_critico, 0 ) > 0 ) and ( sl.dias_conferencia_carga > coalesce( pz.conferencia_carga_critico, 0 ) )             then
          'C'
         else
           case
             when ( coalesce( pz.conferencia_carga_alerta, 0 ) > 0 ) and ( sl.dias_conferencia_carga > coalesce( pz.conferencia_carga_alerta, 0 ) )  then
              'A'
            end
       end                             as situacao_conferencia,

       /* Faturamento  */
       case
         when ( coalesce( pz.faturamento_critico, 0 ) > 0 ) and ( sl.dias_faturamento_carga > coalesce( pz.faturamento_critico, 0 ) )             then
          'C'
         else
           case
             when ( coalesce( pz.faturamento_alerta, 0 ) > 0 ) and ( sl.dias_faturamento_carga > coalesce( pz.faturamento_alerta, 0 ) )               then
              'A'
            end
       end                             as situacao_faturamento





}
