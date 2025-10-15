@AbapCatalog.sqlViewName: 'ZIINCOMPL9041'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca dados na 90 e relação com a 41'
define view ZI_IN_DADOS_COMPL90_41
  as select from ZI_IN_DADOS_COMPL90  as z90_1
    inner join   ZI_IN_DADOS_ZSDT0090 as z90_2 on  z90_2.doc_simulacao = z90_1.doc_simulacao
                                               and z90_2.vbeln         = z90_1.vbeln_p
{
  key z90_2.doc_simulacao,
  key z90_2.vbeln_p,
  key z90_1.vbeln
}
