@AbapCatalog.sqlViewName: 'ZVMIOVFATEST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca estornos de faturamento'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FAT_EST
  as select from    vbak     as Ordem

    inner join      zsdt0350 as Conf on  Ordem.auart = Conf.auart
                                     and Conf.acao   = 'E'

    left outer join vbfa     as Docs on  Docs.vbelv   = Ordem.vbeln
                                     and Docs.vbtyp_n = Conf.vbtyp_n
                                     and Docs.vbtyp_v = Conf.vbtyp_v
                                     and Docs.stufe   = Conf.stufe

{
  key Ordem.vbeln as Ordem,
  key Docs.vbeln  as Faturamento,
      Docs.rfmng  as Qtd

}
