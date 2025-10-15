@AbapCatalog.sqlViewName: 'ZVINFILTRODTVENC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Filtro por data de vencimento'
define view ZI_IN_FILTRO_DT_VENC
  as select distinct from ZI_IN_SIMU_OV    as relacao
  //inner join   ZI_IN_DADOS_SIM as simulador  on relacao.doc_simulacao = simulador.doc_simulacao
    inner join            ZI_IN_OV_DADOS_2 as doc_vendas on relacao.vbeln = doc_vendas.vbeln
    left outer join ZI_IN_LCTOS_OV     as lctos      on relacao.vbeln = lctos.vbeln_va

{
  key relacao.doc_simulacao,
  key relacao.vbeln_p,
  key relacao.vbeln,
      doc_vendas.bukrs,
      doc_vendas.kunnr,
      doc_vendas.vkbur,
      doc_vendas.valdt,
      doc_vendas.vtweg,
      doc_vendas.spart,
      doc_vendas.auart,
      doc_vendas.waerk,
      doc_vendas.erdat,
      lctos.data_pgto
      
}
