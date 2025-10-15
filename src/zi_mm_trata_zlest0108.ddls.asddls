@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Coloca zeros a esquerda do campo DOC_TRANSP'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_TRATA_ZLEST0108 as select from zlest0108
{
    key vbeln,
    lpad(doc_transp,16,'0') as tknum
}

where doc_transp > '0'
