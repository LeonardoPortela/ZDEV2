@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Vinculo de Formação de Lote'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SDTVINC_P_FLOTE 
    as select from zsdtvinc_p_flote
{
    key docnum_flote as DocnumFlote,
    key docnum_eprod as DocnumEprod,
    key id_vinc as IdVinc,
    docnum_ref as DocnumRef,
    chave_nfe as ChaveNfe,
    vinculada_xml as VinculadaXml,
    vinc_virtual as VincVirtual,
    us_criacao as UsCriacao,
    dt_criacao as DtCriacao,
    hr_criacao as HrCriacao,
    cancel as Cancel,
    us_cancel as UsCancel,
    dt_cancel as DtCancel,
    hr_cancel as HrCancel
}
