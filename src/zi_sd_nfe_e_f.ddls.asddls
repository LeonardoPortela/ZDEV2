@AbapCatalog.sqlViewName: 'ZVDSNFEEF'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Notas de Entrada e Formação de Lote'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_SD_NFE_E_F
  as select from I_BR_NFItem as _nfitem
{

  key _nfitem.BR_NotaFiscal          as docnum,
  key _nfitem.BR_NotaFiscalItem      as itmnum,

      _nfitem.Material               as matnr,
      _nfitem.BR_CFOPCode            as cfop,
      _nfitem.MaterialGroup          as matkl,
      _nfitem.Plant                  as werks,
      _BR_NotaFiscal.BusinessPlace   as branch,
      _BR_NotaFiscal.BR_NFIssueDate  as docdat,
      _nfitem.QuantityInBaseUnit     as menge,
      ''                             as CompraFimEspecifico,
      ''                             as EUDR,
      ''                             as EntradaTransferencia,
      ''                             as FormacaoLote,
      ''                             as RomaneioCompleto,
      _BR_NotaFiscal.BR_NFIsCanceled as cancel
}
