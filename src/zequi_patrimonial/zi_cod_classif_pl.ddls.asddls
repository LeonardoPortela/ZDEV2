@AbapCatalog.sqlViewName: 'ZVCODRECLAS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca Codigos de reclassificação'
define view ZI_COD_CLASSIF_PL
  as select distinct from zglt041
{
  key saknr                as Saknr,
  key cod_clas_bal         as CodClasBal,
      max( cod_clas_not2 ) as CodClasNot2
}

where
  cod_clas_bal like '23%'
group by
  saknr,
  cod_clas_bal
