@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help para Tipo de ordem'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define view entity ZI_PP_TIPO_ORDEM_VH
  as select from dd07l as Status
    join         dd07t as Text on  Text.domname  = Status.domname
                               and Text.as4local = Status.as4local
                               and Text.valpos   = Status.valpos
                               and Text.as4vers  = Status.as4vers

  association [0..1] to I_Language as _Language on $projection.Language = _Language.Language
{
      @UI.textArrangement: #TEXT_ONLY
      @ObjectModel.text.element: ['Descricao']
  key cast ( substring( Status.domvalue_l, 1, 1 ) as zde_tipo_ordem preserving type ) as Tipo,

      @Semantics.language: true
      @UI.hidden: true
  key cast( Text.ddlanguage as spras preserving type )                                as Language,

      substring ( Text.ddtext, 1, 60 )                                                as Descricao,

      // Association
      _Language
}
where
      Status.domname  = 'ZDO_TIPO_ORDEM'
  and Status.as4local = 'A'
