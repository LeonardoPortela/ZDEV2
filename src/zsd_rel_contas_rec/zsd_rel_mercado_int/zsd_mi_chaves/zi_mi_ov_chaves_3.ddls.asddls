@AbapCatalog.sqlViewName: 'ZVMIOVCHAVES3'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca dados de chaves das OV'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_CHAVES_3
  as select from vbak     as vbak
    inner join   zsdt0053 as z53   on z53.vbeln = vbak.vbeln
    inner join   tvarvc   as Spart on  Spart.name = 'Z_SPART_REL_MERCADO_INTER'
                                   and Spart.low  = vbak.spart
{
  key z53.nro_sol_ov                         as Simulador,
  key z53.posnr                              as ItemSimulador,
  key z53.vbeln                              as Ordem,
  key vbak.bukrs_vf                          as Empresa,
  key z53.vbeln                              as OrdemDerivada,
      cast(concat('1',z53.status) as char2 ) as origem

}
where
      z53.status <> 'Y'

union all


select from  vbak     as vbak
  inner join zsdt0053 as z53   on z53.vbeln = vbak.vbeln
  inner join tvarvc   as Spart on  Spart.name = 'Z_SPART_REL_MERCADO_INTER'
                               and Spart.low  = vbak.spart
  inner join vbfa     as vbfa  on  vbfa.vbeln   = z53.vbeln
                               and vbfa.vbtyp_n = 'H'
                               and vbfa.vbtyp_v = 'C'
{
  key z53.nro_sol_ov                         as Simulador,
  key z53.posnr                              as ItemSimulador,
  key z53.doc_precedente                     as Ordem,
  key vbak.bukrs_vf                          as Empresa,
  key vbak.vbeln                             as OrdemDerivada,
      cast(concat('2',z53.status) as char2 ) as origem
}
where
  z53.status = 'Y'

union

select from       vbak     as OV
  inner join      tvarvc   as Spart on  Spart.name = 'Z_SPART_REL_MERCADO_INTER'
                                    and Spart.low  = OV.spart
  left outer join zsdt0053 as z53   on z53.vbeln = OV.vbeln
{
  key z53.nro_sol_ov as Simulador,
  key z53.posnr      as ItemSimulador,
  key OV.vbeln       as Ordem,
  key OV.bukrs_vf    as Empresa,
  key OV.vbeln       as OrdemDerivada,

      case coalesce(z53.status,'') when '' then
        cast('30' as char2 )
      else
        cast(concat('3',z53.status) as char2 )
      end            as origem
}
where
  z53.nro_sol_ov is not null

union select from ZI_MI_OV_CHAVES_EXT
{
  key Simulador,
  key ItemSimulador,
  key Ordem,
  key Empresa,
  key OrdemDerivada,

      cast('40' as char2 ) as origem
}
union select from ZI_MI_OV_CHAVES_SEM
{
  key Simulador,
  key ItemSimulador,
  key Ordem,
  key Empresa,
  key OrdemDerivada,

      cast('50' as char2 ) as origem
}
