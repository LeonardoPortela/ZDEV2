@AbapCatalog.sqlViewName: 'ZIAVERBACAO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca dados das tabelas de averbação'
define view ZI_SD_DADOS_AVERBACAO
  as select distinct from    ZI_SD_DADOS_J_1BNFE_ACTIVE as NFE
    inner join zib_nfe_dist_avb           as avb on avb.chave_nfe = NFE.chave  
   inner join zib_nfe_dist_avi           as avi on avi.chave_nfe = NFE.chave and avi.seq_evento = avb.seq_evento
{
  key NFE.DOCNUM       as docnum,
  key NFE.chave        as chave,
      //avb.dt_evento    as dt_evento,
      avb.cnpj         as cnpj,
      avi.n_due        as due,
      sum(avi.qitem)       as qitem,
      avi.dt_averbacao as dt_averb,
      avi.dt_embarque  as dt_embarque
}
group by
NFE.DOCNUM,
NFE.chave,
--avb.dt_evento,
avb.cnpj,
avi.n_due,
avi.dt_averbacao,
avi.dt_embarque
