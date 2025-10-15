@AbapCatalog.sqlViewName: 'ZIACTIVE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca chaves da nota'
define view ZI_SD_DADOS_J_1BNFE_ACTIVE as select from j_1bnfe_active as NFE
{
    key docnum as DOCNUM,
        concat(concat(concat(concat(NFE.regio,NFE.nfyear),concat(NFE.nfmonth,NFE.stcd1)),
        concat(NFE.model,NFE.serie)),concat(concat(NFE.nfnum9,NFE.docnum9),NFE.cdv)) as chave
}
