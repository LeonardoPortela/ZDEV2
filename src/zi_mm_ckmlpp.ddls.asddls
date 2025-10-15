@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BUSCA DADOS ckmlhd x ckmlpp' 
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable : true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_CKMLPP
  with parameters
    p_bwkey : bwkey,
    p_bdatj : bdatj,
    p_poper : poper
  as select from ckmlhd as a
    inner join   ckmlpp as b on b.kalnr = a.kalnr
{
       @Search.defaultSearchElement : true
  key  b.kalnr,
  key  max(concat(b.bdatj, b.poper)) as ANOMES
}
where
      1                        =  1
  and a.bwkey                  = $parameters.p_bwkey
  and concat(b.bdatj, b.poper) <= concat($parameters.p_bdatj, $parameters.p_poper)

group by
  b.kalnr
