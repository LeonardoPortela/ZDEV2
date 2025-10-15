@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Materiais e caracteristicas'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_IN_MAT_CARACT
  as select from kssk as Obj

    inner join   klah as DescClasse on DescClasse.clint = Obj.clint
    inner join   ksml as Caract     on Obj.clint = Caract.clint
    inner join   cabn as DescCarac  on Caract.imerk = DescCarac.atinn
    inner join   ausp as ValorClasf on  ValorClasf.objek = Obj.objek
                                    and ValorClasf.atinn = Caract.imerk
{
  key cast(Obj.objek as matnr ) as Material,
      Obj.clint                 as IdClasse,
      Caract.imerk              as IDClassif,
      DescClasse.class          as DescClasse,
      DescCarac.atnam           as Classificacao,
      ValorClasf.atwrt          as ValorClasf
}
where
      Obj.clint       = '0000007763'
  and DescCarac.atnam = 'NOVA_CULTIVAR'
