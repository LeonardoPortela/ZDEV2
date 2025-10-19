@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Distribuições Solicitações Entrega Insumos'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SD_DIST_SOL_INSUMOS_INFO
  as

  select from       zsdt0082 as sl

    inner join      vbap     as vb on  sl.vbeln = vb.vbeln
                                   and sl.posnr = vb.posnr

    left outer join zsdt0132 as rc on sl.nr_rot_pc = rc.nr_rot

    left outer join dd07t    as st on  st.domname    = 'ZSDD019'
                                   and st.domvalue_l = sl.status
                                   and st.ddlanguage = 'P'

{

  key sl.nro_sol                       as nro_sol,
  key sl.seq                           as seq,
  key sl.vbeln                         as ordem,
  key sl.posnr                         as ordem_item,
      @Semantics.quantity.unitOfMeasure: 'unidade'
      sl.qte_lib                       as qtde_liberacao,
      @Semantics.quantity.unitOfMeasure: 'unidade'
      cast( 0 as abap.quan(13,3) )     as saldo_formar_carga,
      vb.kmein                         as unidade,
      rc.nr_rot                        as nr_roteiro_coleta,
      rc.rot_desc                      as ds_roteiro_coleta,
      cast( sl.dt_liber as abap.dats ) as data_liberacao,
      sl.usuario_lib                   as usuario_liberacao,
      sl.status                        as status,
      st.ddtext                        as ds_status,
      cast(sl.dt_canc  as abap.dats)   as data_cancelamento,
      sl.user_canc                     as usuario_cancelamento

}
where
  sl.seq <> '001'
