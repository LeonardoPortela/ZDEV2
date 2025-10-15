@AbapCatalog.sqlViewName: 'ZVINOVDADOS2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados Basicos para OV'
define view ZI_IN_OV_DADOS_2
  as select from ZI_IN_OV_DADOS
{
  key vbeln,
      bukrs,
      vkbur,
      vkgrp,
      vkgrp_name,
      auart,
      kunnr,
      name1,
      zterm,
      zterm_name,
      erdat,
      waerk,
      kurrf,
      valdt,

      fator_devol,

      kwmeng,
      zmeng,
      vtweg,
      spart,
      ( netwr * fator_devol )      as netwr,
      ( mwsbp * fator_devol )      as mwsbp,
      ( vlr_tot_ov * fator_devol ) as vlr_tot_ov
}
