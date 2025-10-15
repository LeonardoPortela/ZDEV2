@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZI_USUARIOS_ZDEVOPS'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_USUARIOS_ZDEVOPS as select from zusers_devops
{
key nome
}
