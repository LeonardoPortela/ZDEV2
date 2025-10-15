@AbapCatalog.sqlViewName: 'ZVOVSIMITEMS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca dados OV com simulador de vendas'
define view ZI_SALESORDER_SIMULADOR
  as select from vbak     as A

    inner join   zsdt0041 as b on A.vbeln = b.vbeln
{

  key A.vbeln,
  key b.doc_simulacao as DocSimu,
      A.erdat,
      A.auart,
      A.vkbur,
      A.kunnr,
      A.waerk,
      A.vkorg,
      A.vkgrp,
      A.bukrs_vf
}

union select from vbak     as A

  inner join      zsdt0090 as b on A.vbeln = b.vbeln
{

  key A.vbeln,
  key b.doc_simulacao as DocSimu,
      A.erdat,
      A.auart,
      A.vkbur,
      A.kunnr,
      A.waerk,
      A.vkorg,
      A.vkgrp,
      A.bukrs_vf
}
