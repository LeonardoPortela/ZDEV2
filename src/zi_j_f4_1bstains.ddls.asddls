@AbapCatalog.sqlViewName: 'ZI_F4_1BSTAINS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Inscrição Estadual adaptação match code'
//*-US 138094-15-07-2024-#138094-RJF-inicio
define view ZI_J_F4_1BSTAINS as
select distinct from zib_nfe_dist_ter as z
        inner join lfa1 as l
         on z.forne_cnpj = l.stcd1
{
    LTRIM( z.forne_ie,'0' ) as forne_ie, z.forne_cnpj, l.name1    
}
where z.forne_ie <> ''
//*-US 138094-15-07-2024-#138094-RJF-fim
