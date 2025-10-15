@AbapCatalog.sqlViewName: 'ZVDADOS0090EX'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'OVs somente na ZSDT0090 excl 0041'
define view ZI_IN_DADOS_ZSDT0090_EX
  as select distinct from zsdt0090 as z90
    inner join            zsdt0041 as z41   on z41.vbeln = z90.vbelv
    left outer join       zsdt0041 as z41_2 on z41_2.vbeln = z90.vbeln

{
  key z90.doc_simulacao,
  key z90.vbelv as vbeln_p,
  key z90.vbeln

}
where
       not(
         z90.categoria     =  'F'
         or z90.categoria  =  'G'
         or z90.categoria  =  'P'
       )
  and  z90.estorno         <> 'X'
  and  z41_2.doc_simulacao is null

  and


       z90.vbeln           is not initial
