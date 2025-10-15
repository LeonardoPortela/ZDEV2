@AbapCatalog.sqlViewName: 'ZVOVSIMAGRP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agrupa busca dados OV simulador de vendas'
define view ZI_SALESORDER_SIMUL_AGRP
  as select distinct from ZI_SALESORDER_SIMULADOR
{
  key vbeln,
  key DocSimu,
      erdat,
      auart,
      vkbur,
      kunnr,
      waerk,
      vkorg,
      vkgrp,
      bukrs_vf
}
