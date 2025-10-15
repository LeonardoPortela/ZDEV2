@AbapCatalog.sqlViewName: 'ZVSDVINCFLOTE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Vinculo entre a formação de lote'
@Metadata.ignorePropagatedAnnotations: true
define view ZISD_VINCULO_FLOTE
  as select from zsdtvinc_p_flote as _vinc_flote
{
  key _vinc_flote.docnum_flote  as DocnumFlote,
  key _vinc_flote.docnum_eprod  as DocnumEprod,
  key _vinc_flote.id_vinc       as IdVinc,
      _vinc_flote.docnum_ref    as DocnumRef,
      _vinc_flote.chave_nfe     as ChaveNfe,
      _vinc_flote.qtd_vinc      as QtdVinc,
      _vinc_flote.vinculada_xml as VinculadaXml,
      _vinc_flote.vinc_virtual  as VincVirtual,
      _vinc_flote.us_criacao    as UsCriacao,
      _vinc_flote.dt_criacao    as DtCriacao,
      _vinc_flote.hr_criacao    as HrCriacao,
      _vinc_flote.manual        as Manual,
      _vinc_flote.cancel        as Cancel,
      _vinc_flote.us_cancel     as UsCancel,
      _vinc_flote.dt_cancel     as DtCancel,
      _vinc_flote.hr_cancel     as HrCancel
}
