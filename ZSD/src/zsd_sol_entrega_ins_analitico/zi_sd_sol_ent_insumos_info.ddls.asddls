@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Solicitações Entrega Insumos'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SD_SOL_ENT_INSUMOS_INFO
  as

  select from       zsdt0082                    as sl

  /* Solicitação Origem */
    left outer join ZI_SD_SOL_ENT_ORIG_INS_INFO as og on sl.nro_sol = og.nro_sol
    left outer join zsdt0082                    as o1 on  og.nro_sol_raiz = o1.nro_sol
                                                      and o1.seq          = '001'
    left outer join vbak                        as ov on sl.vbeln = ov.vbeln
    left outer join kna1                        as kn on ov.kunnr = kn.kunnr

    inner join      vbap                        as vb on  sl.vbeln = vb.vbeln
                                                      and sl.posnr = vb.posnr

    inner join      I_MaterialText              as mt on  vb.matnr    = mt.Material
                                                      and mt.Language = $session.system_language

    left outer join ZI_MM_CULTIVAR_MAT_INFO     as cv on mt.Material = cv.material

    left outer join zsdt0132                    as re on sl.nr_rot = re.nr_rot

    left outer join dd07t                       as st on  st.domname    = 'ZSDD019'
                                                      and st.domvalue_l = sl.status
                                                      and st.ddlanguage = 'P'

{
  key sl.nro_sol                      as nro_sol,
  key sl.seq                          as seq,
  key sl.vbeln                        as ordem,
  key sl.posnr                        as ordem_item,
      vb.matnr                        as material,
      mt.MaterialName                 as material_name,
      cv.cultivar                     as material_cutivar,
      ov.kunnr                        as ordem_cliente,
      kn.name1                        as ordem_ds_cliente,
      re.nr_rot                       as nr_roteiro_entrega,
      re.rot_desc                     as ds_roteiro_entrega,
      sl.nro_sol_origem               as nro_sol_origem,
      sl.vkorg                        as organizacao_venda,
      sl.spart                        as setor_atividade,
      sl.vkbur                        as escritorio_venda,
      sl.werks                        as centro_faturamento,
      sl.auart                        as tipo_ov,
      @Semantics.quantity.unitOfMeasure: 'unidade'
      sl.qte_sol                      as qtde_solicitacao,
      vb.kmein                        as unidade,
      sl.dt_sol                       as data_solicitacao,

      case
        when coalesce( o1.dt_sol, '00000000' ) <> '00000000' then
           o1.dt_sol
        else
           sl.dt_sol
      end                             as data_solicitacao_raiz,

      case
        when coalesce( o1.nro_sol, '0000000000' ) <> '0000000000' then
           o1.nro_sol
        else
           sl.nro_sol
      end                             as nro_solicitacao_raiz,


      sl.usuario_sol                  as usuario_solicitacao,
      sl.status                       as status,
      st.ddtext                       as ds_status,
      cast( sl.dt_canc as abap.dats ) as data_cancelamento,
      sl.user_canc                    as user_cancelamento,
      sl.dt_entrega                   as data_entrega,
      sl.tp_oper                      as tipo_operacao,
      sl.origem_estoque               as origem_estoque,
      sl.ebeln                        as pedido,
      sl.ebelp                        as pedido_item,
      sl.lifnr_arm                    as fornecedor_arm,
      sl.charg                        as lote,
      sl.marca                        as marca,
      sl.carga_automatica             as carga_automatica,
      sl.flexibilidade                as flexibilidade,
      sl.prioridade                   as prioridade,
      sl.transf_no_fornecedor         as transf_no_fornecedor
}
where
  sl.seq = '001'
