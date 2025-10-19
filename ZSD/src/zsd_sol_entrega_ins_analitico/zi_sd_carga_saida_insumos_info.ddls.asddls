@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cargas Saida Insumos'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SD_CARGA_SAIDA_INSUMOS_INFO
  as

  select from       zsdt0131                      as ic

    inner join      vbap                          as vb on  ic.vbeln = vb.vbeln
                                                        and ic.posnr = vb.posnr

    inner join      zsdt0129                      as lt on ic.nro_lote = lt.nro_lote

    inner join      zsdt0133                      as cg on lt.nro_cg = cg.nro_cg

  /*Data Carregamento*/
    left outer join ZI_SD_SOL_ENT_INSUMOS_N4_INFO as dc on cg.nro_cg = dc.nro_cg

  /*Status Carga*/
    left outer join ZI_SD_ST_CG_SAI_INSUMOS       as st on cg.nro_cg = st.nro_cg

  /*Dados Faturamento Carga*/
    left outer join ZI_SD_ST_ROM_CG_SAI_INS_INFO  as ft on cg.nro_cg = ft.nro_cg



{

  key  cg.nro_cg                       as nro_cg,
  key  ic.nro_sol                      as nro_sol,
  key  ic.seq                          as seq_lib,
  key  ic.vbeln                        as ordem,
  key  ic.posnr                        as ordem_item,
       @Semantics.quantity.unitOfMeasure: 'unidade'
       ic.qtd_vinc                     as qtde_carga,
       vb.kmein                        as unidade,
       cg.data_atual                   as data_criacao,
       cg.usnam                        as usuario_criacao,

       case
         when ( coalesce( cg.dt_envio_cotacao, '00000000' ) = '00000000' ) and
              ( coalesce( cg.data_atual, '00000000' ) <> '00000000' ) then
            cg.data_atual
         else
            cg.dt_envio_cotacao
       end                             as data_envio_cotacao,

       case
         when ( coalesce( cg.dt_frete_contratado, '00000000' ) = '00000000' ) and
              ( coalesce( cg.dt_autorizacao_embarque, '00000000' ) <> '00000000' ) then
            cg.dt_autorizacao_embarque
         else
            cg.dt_frete_contratado
       end                             as data_frete_contratado,
       
       
       cg.dt_autorizacao_embarque as data_autorizacao_embarque,
              
       
       case
         when ( coalesce( dc.dt_carregamento, '00000000' ) = '00000000' ) and
              ( substring( st.status,1,1)  = '7' ) and 
              ( coalesce( ft.dt_finalizacao_faturamento, '00000000' ) <> '00000000' ) then
            ft.dt_finalizacao_faturamento
         else
            dc.dt_carregamento
       end   as data_carregamento,
       
       case
         when ( coalesce( cg.dt_conferencia, '00000000' ) = '00000000' ) and
              ( substring( st.status,1,1)  = '7' ) and 
              ( coalesce( ft.dt_finalizacao_faturamento, '00000000' ) <> '00000000' ) then
            ft.dt_finalizacao_faturamento
         else
            cg.dt_conferencia
       end   as data_conferencia,

       case
        when substring( st.status,1,1)  = '7' then
           ft.dt_finalizacao_faturamento
       end                             as data_finalizacao_faturamento,
       
       cg.id_carga_safra_control       as id_carga_safra_control,
       lt.inco1                        as inco1,
       cg.viagem_id                    as viagem_id,


       cast( cg.dt_canc as abap.dats ) as data_cancelamento,
       cg.hr_can                       as hora_cancelamento,
       cg.user_canc                    as usuario_cancelamento,
       st.status                       as status_carga,
       st.ds_status                    as ds_status_carga,
       case
        when lt.dt_entrega < cast( $session.system_date as abap.dats ) then 'Atraso'
        when lt.dt_entrega = cast( $session.system_date as abap.dats ) then 'Para hoje'
        when lt.dt_entrega > cast( $session.system_date as abap.dats ) then 'No Prazo'
        else ''
       end                             as status_entrega

}
where
  ic.status <> 'X'
