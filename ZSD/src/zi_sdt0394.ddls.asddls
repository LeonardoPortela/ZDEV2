@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Usuario exceção para ZSDT0044'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

@ObjectModel.modelCategory: #BUSINESS_OBJECT
@ObjectModel.compositionRoot: true
@ObjectModel.transactionalProcessingEnabled: true
@ObjectModel.createEnabled: true
@ObjectModel.updateEnabled: true
@ObjectModel.deleteEnabled: true
@ObjectModel.writeActivePersistence: 'zsdt0394'

@OData.publish: true

@UI: {
  headerInfo: { typeName: 'Relatorio de Usuario',
                typeNamePlural: 'Relatorio de Usuario',
                title: { type: #STANDARD, label: 'Relatorio de Usuario', value: 'usuario' } } }

define view entity ZI_SDT0394
  as select from zsdt0394
{
  key usuario     as Usuario,
      user_create as UserCreate,
      date_create as DateCreate,
      time_create as TimeCreate
}
