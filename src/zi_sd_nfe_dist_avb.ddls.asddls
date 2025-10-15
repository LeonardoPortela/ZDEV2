@AbapCatalog.sqlViewName: 'ZVIAVERB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados averbação'
define view ZI_SD_NFE_DIST_AVB as select distinct from zib_nfe_dist_avb as avb
inner join zib_nfe_dist_avi as avi on avi.chave_nfe = avb.chave_nfe and avi.seq_evento = avb.seq_evento
{
    key avb.chave_nfe,
    key avb.seq_evento,
        avb.dt_evento,
        avi.n_due
}
