@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Condições de uma IVA - MM'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_MM_TAX_CODE_CONDITIONS
  as select from a003   as a
    inner join   konp   as K on  a.knumh = K.knumh
                             and a.mwskz = K.mwsk1
    inner join   j_1baj as J on K.kschl = J.taxtyp

{
  a.aland  as aland,
  a.mwskz  as mwskz,
  J.taxtyp as taxtyp,
  J.taxgrp as taxgrp
}
where
  a.kappl = 'TX'
