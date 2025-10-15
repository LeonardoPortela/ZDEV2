@AbapCatalog.sqlViewName: 'ZVHTIPOREF'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ajuda de pesquisa - Tipo Refleza'
define view ZI_VH_TIPO_REFLEXA
  as select from dd07t
{
  key domvalue_l                                    as TIPO,
      concat( domvalue_l ,concat( '-' ,  ddtext ) ) as Descricao


}
where
      domname    = 'Z_TP_LAN_EQ'
  and ddlanguage = $session.system_language
