@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Solicitação Entrega Insumos - Origem'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SD_SOL_ENT_ORIG_INS_INFO
  as

  select from       zsdt0082 as f01
    left outer join zsdt0082 as f02 on f01.nro_sol_origem = f02.nro_sol
    left outer join zsdt0082 as f03 on f02.nro_sol_origem = f03.nro_sol
    left outer join zsdt0082 as f04 on f03.nro_sol_origem = f04.nro_sol
    left outer join zsdt0082 as f05 on f04.nro_sol_origem = f05.nro_sol
    left outer join zsdt0082 as f06 on f05.nro_sol_origem = f06.nro_sol
    left outer join zsdt0082 as f07 on f06.nro_sol_origem = f07.nro_sol
    left outer join zsdt0082 as f08 on f07.nro_sol_origem = f08.nro_sol
    left outer join zsdt0082 as f09 on f08.nro_sol_origem = f09.nro_sol
    left outer join zsdt0082 as f10 on f09.nro_sol_origem = f10.nro_sol

{
  
  f01.nro_sol,
  
  max(
  coalesce( f10.nro_sol,
  coalesce( f09.nro_sol,
  coalesce( f08.nro_sol,
  coalesce( f07.nro_sol,
  coalesce( f06.nro_sol,
  coalesce( f05.nro_sol,
  coalesce( f04.nro_sol,
  coalesce( f03.nro_sol,
            f02.nro_sol           
  ) ) ) ) ) ) ) ) 
  
  ) as nro_sol_raiz

}

group by f01.nro_sol
