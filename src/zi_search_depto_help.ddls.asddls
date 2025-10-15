@AbapCatalog.sqlViewName: 'ZI_DEPTO_HELP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Pesquisa de Departamento no Help F1/F4'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_SEARCH_DEPTO_HELP as select distinct from zimp_cad_depto 
{
 key dep_resp ,
  dep_resp_desc  
}
