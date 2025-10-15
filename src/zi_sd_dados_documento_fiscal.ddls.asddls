@AbapCatalog.sqlViewName: 'ZIDOCFISC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca dados do documento fiscal'
define view ZI_SD_DADOS_DOCUMENTO_FISCAL
  as select distinct from j_1bnfdoc             as DOC
    inner join   j_1bnflin             as LIN  on DOC.docnum = LIN.docnum
    inner join   ZI_SD_DADOS_AVERBACAO as nfe  on nfe.docnum = DOC.docnum
    inner join   kna1                  as kna1 on kna1.kunnr = DOC.parid
    inner join   ZI_SD_NFE_DIST_AVB as avb on avb.chave_nfe = nfe.chave and
                                              avb.n_due = nfe.due
{
  key DOC.docnum            as DOC_NUM,
      LIN.itmnum            as itmnum,
      DOC.bukrs             as bukrs,
      LIN.werks             as filial,
      LIN.cfop              as cfop,
      DOC.regio             as uf,
      LIN.matnr             as produto,
      LIN.maktx             as DESCR_MATERIAL,
      @Semantics.quantity.unitOfMeasure: 'unidade'
      LIN.menge             as quantidade,
      LIN.meins             as unidade,
      LIN.netwr             as VALOR_NF,
      LIN.netpr             as vlr_unit,
      LIN.nbm               as nbm,
      DOC.inco1             as incoterms,
      DOC.docdat            as docdat,
      DOC.nfenum            as NR_NOTA,
      DOC.parid             as CLIENTE,
      kna1.name1            as DESC_CLIENTE,
      DOC.branch            as branch,
      nfe.chave             as CHAVE_NFE,
      avb.dt_evento         as dt_evento,
      case when kna1.stcd1 is not initial then
      kna1.stcd1
      else
      kna1.stcd2
      end as CPF_CNPJ,
      nfe.due               as due,
      nfe.qitem             as QTD_AVERBADA,
      nfe.dt_averb          as DATA_AVERBACAO,
      nfe.dt_embarque       as DATA_EMBARQUE,
      LIN.menge - nfe.qitem as QTD_NAO_AVERBADA

}
