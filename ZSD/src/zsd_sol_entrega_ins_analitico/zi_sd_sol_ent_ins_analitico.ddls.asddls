@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Solicitações Entrega Insumos - Analitico'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZI_SD_SOL_ENT_INS_ANALITICO
  as select from ZI_SD_SOL_ENT_INS_ANAL_N2_INFO as Solicitacao

{

  key       solic_nro,
  key       solic_seq,
  key       solic_ordem,
  key       solic_ordem_item,
  key       solic_line_saldo,

  key       distrib_nro_sol,
  key       distrib_seq,
  key       distrib_ordem,
  key       distrib_ordem_item,
  key       distrib_line_saldo,

  key       carga_nro,
  key       carga_nro_sol,
  key       carga_seq_lib,
  key       carga_ordem,
  key       carga_ordem_item,

            solic_nro_sol_origem,
            solic_organizacao_venda,
            solic_setor_atividade,
            solic_escritorio_venda,
            solic_centro_faturamento,
            solic_tipo_ov,

            material,
            material_name,
            material_cutivar,

            solic_ordem_cliente,
            solic_ordem_ds_cliente,
            solic_nr_roteiro_entrega,
            solic_ds_roteiro_entrega,

            @Semantics.quantity.unitOfMeasure: 'solic_unidade'
            solic_quantidade,

            @Semantics.quantity.unitOfMeasure: 'solic_unidade'
            solic_qtde_distrib,

            @Semantics.quantity.unitOfMeasure: 'solic_unidade'
            solic_saldo_distribuir,
            solic_unidade,
            solic_data,
            solic_data_sol_raiz,
            solic_nro_sol_raiz,
            solic_usuario_criacao,
            solic_status,
            solic_ds_status,
            solic_data_cancelamento,
            solic_usuario_cancelamento,
            solic_data_entrega,
            solic_tipo_operacao,
            solic_origem_estoque,
            solic_pedido,
            solic_pedido_item,
            solic_fornecedor_arm,
            solic_lote,
            solic_marca,
            solic_carga_automatica,
            solic_flexibilidade,
            solic_prioridade,
            solic_transf_no_fornecedor,

            @Semantics.quantity.unitOfMeasure: 'solic_unidade'
            distrib_quantidade,

            @Semantics.quantity.unitOfMeasure: 'solic_unidade'
            distrib_qtde_em_carga,

            @Semantics.quantity.unitOfMeasure: 'solic_unidade'
            distrib_saldo_formar_carga,
            distrib_data,
            distrib_nr_roteiro_coleta,
            distrib_ds_roteiro_coleta,
            distrib_usuario,
            distrib_status,
            distrib_ds_status,
            distrib_data_cancelamento,
            distrib_usuario_cancelamento,

            @Semantics.quantity.unitOfMeasure: 'solic_unidade'
            carga_quantidade,
            carga_id_safra_control,
            carga_id_viagem_carguero,
            carga_incoterms,
            carga_data_criacao,
            carga_usuario_criacao,
            carga_data_envio_cotacao,
            carga_data_frete_contratado,
            carga_data_aut_embarque,
            carga_data_carregamento,
            carga_data_conferencia,
            carga_data_finaliza_fatura,
            carga_data_cancelamento,
            carga_hora_cancelamento,
            carga_usuario_cancelamento,
            carga_status,
            carga_ds_status,
            carga_status_entrega,

            dias_distribuicao,
            dias_criacao_carga,
            dias_envio_cotacao_carga,
            dias_contratacao_frete_carga,
            dias_autorizacao_emb_carga,
            dias_carregamento_carga,
            dias_conferencia_carga,
            dias_faturamento_carga,

            /*Situação Distribuição */
            case( situacao_distrib )
               when 'A' then 'Alerta'
               when 'C' then 'Crítico'
               else ''
            end as situacao_distrib,

            case( situacao_distrib )
              when 'A' then 2
              when 'C' then 1
              else 3
            end as situacao_distrib_crit,

            /*Situação Criação Carga */
            case( situacao_criacao_carga )
              when 'A' then 'Alerta'
              when 'C' then 'Crítico'
              else ''
            end as situacao_criacao_carga,

            case( situacao_criacao_carga )
              when 'A' then 2
              when 'C' then 1
              else 3
            end as situacao_criacao_carga_crit,

            /*Situação Envio Cotação */
            case( situacao_envio_cotacao )
              when 'A' then 'Alerta'
              when 'C' then 'Crítico'
              else ''
            end as situacao_envio_cotacao,

            case( situacao_envio_cotacao )
              when 'A' then 2
              when 'C' then 1
              else 3
            end as situacao_envio_cotacao_crit,


            /*Situação Contratação */
            case( situacao_contratacao_frete )
              when 'A' then 'Alerta'
              when 'C' then 'Crítico'
              else ''
            end as situacao_contratacao_frete,

            case( situacao_contratacao_frete )
              when 'A' then 2
              when 'C' then 1
              else 3
            end as situacao_contrat_frete_crit,

            /*Situação Autorização Embarque */
            case( situacao_autorizacao_embarque )
              when 'A' then 'Alerta'
              when 'C' then 'Crítico'
              else ''
            end as situacao_autorizacao_embarque,

            case( situacao_autorizacao_embarque )
              when 'A' then 2
              when 'C' then 1
              else 3
            end as situacao_aut_embarque_crit,

            /*Situação Carregamento */
            case( situacao_carregamento )
              when 'A' then 'Alerta'
              when 'C' then 'Crítico'
              else ''
            end as situacao_carregamento,

            case( situacao_carregamento )
              when 'A' then 2
              when 'C' then 1
              else 3
            end as situacao_carregamento_crit,

            /*Situação Conferencia */
            case( situacao_conferencia )
              when 'A' then 'Alerta'
              when 'C' then 'Crítico'
              else ''
            end as situacao_conferencia,

            case( situacao_conferencia )
              when 'A' then 2
              when 'C' then 1
              else 3
            end as situacao_conferencia_crit,

            /*Situação Faturamento */
            case( situacao_faturamento )
              when 'A' then 'Alerta'
              when 'C' then 'Crítico'
              else ''
            end as situacao_faturamento,

            case( situacao_faturamento )
              when 'A' then 2
              when 'C' then 1
              else 3
            end as situacao_faturamento_crit



}
