@AbapCatalog.sqlViewName: 'ZVMIOVSALDO2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Registra o Saldo pelo Simulador'
define view ZI_MI_OV_SALDO_2
  as select from ZI_MI_OV_PRINCIPAL_3
{
  key Simulador,
  key Ordem,
        
      sum( 
      
        case when TpOV <> 'ZTRG' then
            case when Moeda = 'BRL' then
                SaldoOV
            else
                SaldoOVUSD
            end 
        else
            0 
        end )        as SaldoOV,
      sum( 
      
        case when TpOV <> 'ZTRG' then 
            case when SaldoJurosFiltro < 0 then 0 else SaldoJurosFiltro end 
        else
            0
        end ) as SaldoJurosFiltro
}
group by
  Simulador,
  Ordem
