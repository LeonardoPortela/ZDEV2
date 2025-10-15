@AbapCatalog.sqlViewName: 'ZVMITIPOOVCHAV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca os tipos de ordem de venda a partir do parametro'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_CHAVES_TP_ORD
  as select from    ZI_MI_OV_CHAVES as Ordem

    inner join      vbak            as OV  on Ordem.OrdemDerivada = OV.vbeln

    left outer join tvarvc          as Dev on  Dev.name = 'ZSDT0060_TP_OV_DEVOLUCAO'
                                           and Dev.low  = OV.auart

{
  key Ordem.Simulador,
  key Ordem.ItemSimulador,
  key Ordem.Ordem,
  key Ordem.OrdemDerivada,

      case coalesce( Dev.low, '' )
      when ''
          then 'C'
       else
          'D'   //OV normal ou complemento
       end as TpOV

}
