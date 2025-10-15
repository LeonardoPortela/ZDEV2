@AbapCatalog.sqlViewName: 'ZVIN9041CHECK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '90 com check na 41'
define view ZI_IN_DADOS_90_41_CHECK
  as select distinct from    zsdt0090 as z90
    left outer join zsdt0041 as z41 on  z41.doc_simulacao = z90.doc_simulacao
                                    and z41.vbeln         = z90.vbelv
{
  key z90.doc_simulacao,
  key z90.vbeln,
  key z90.vbelv,

      case coalesce(z41.vbeln,'') when '' then
        ''
      else
        'X'
      end as com_41

}
where
       z90.vbeln      is not initial
  and  z90.vbelv      <> z90.vbeln
  and  not(
     z90.categoria    =  'F'
     or z90.categoria =  'G'
     or z90.categoria =  'P'
   )
  and  z90.estorno    <> 'X'
