@AbapCatalog.sqlViewName: 'ZVSDROMNF'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados Romaneio - Nf'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_SD_DADOS_ROM_NF
  as select from zsdt0134  as Romaneio

    inner join   zsdt0001  as RomNF on Romaneio.ch_referencia = RomNF.ch_referencia

    inner join   j_1bnfdoc as NF    on NF.docnum = RomNF.nro_nf_prod

{
  key Romaneio.vbeln                               as Vbeln,
  key Romaneio.posnr                               as Posnr,
  key Romaneio.nro_cg                              as NroCg,
  key Romaneio.nr_rot                              as NrRot,
      Romaneio.ch_referencia                       as ChReferencia,
      concat( concat( NF.nfenum, '-'), NF.series ) as Nf,
      RomNF.nro_nf_prod,
      NF.docnum,
      NF.nfenum,
      NF.series
}
