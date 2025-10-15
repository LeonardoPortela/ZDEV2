@AbapCatalog.sqlViewName: 'ZVMMNOMUSER'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca nome usuario'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MM_NOME_USUARIO
  as select from usr21 as User

  association to adrp as Nome on User.persnumber = Nome.persnumber

{
  key User.bname     as Usuario,
      Nome.name_text as Nome
}
