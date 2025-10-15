@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'teste'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zi_mm_status_carga2 as select from zmmt0201              as header
    left outer join ZI_MM_QTD_ITENS_CARGA as itens on header.nro_cg = itens.nro_cg
    left outer join ZI_MM_QTD_NOTAS       as notas on header.nro_cg = notas.nro_cg
    left outer join zsdt0391              as frete on frete.nro_cg = header.nro_cg
    left outer join ZI_MM_CONFERE_CARGA   as zib   on zib.nro_cg = header.nro_cg
    left outer join ZI_MM_CALC_SALDO_CARGA as saldo_zib on saldo_zib.nro_cg = header.nro_cg
    left outer join ZI_MM_VALIDA_CARGA_FRETE as carga_frete on carga_frete.nro_cg = header.nro_cg
    left outer join ZI_MM_VALIDA_FRETE_PAGO as frete_pago on frete_pago.nro_cg = header.nro_cg
    left outer join ZI_MM_QTD_NOTAS_2 as notas2 on notas2.nro_cg = header.nro_cg
    left outer join zi_mm_conf_parcial as conf_parc on conf_parc.nro_cg = header.nro_cg
    left outer join zi_mm_conf_total as conf_tot on conf_tot.nro_cg = header.nro_cg
{
  key header.nro_cg,

      // Valida cancelamento parcial ou total
      case ( header.cancel_parcial )
          when 'X'
          then '1'
      else
          case ( header.cancel)
          when 'X'
          then '2'
      else
      //Valida se há itens para a carga
          case ( coalesce(itens.qtd,0) )
          when 0
          then '3'
      else
          case when coalesce(itens.qtd,0) > 0
          then 
              case when frete.dt_cotacao_frete = '00000000'
                then '5'
              else
                case when frete.dt_contratacao_frete = '00000000'
                  then '4'
                else
                  case when header.dt_autorizacao_embarque = '00000000'
                    then '6'
                  else
                    case when header.transf_no_fornecedor = 'X' and coalesce(notas.qtd,0) = 0
                      then '7'
                    else
                      case when header.transf_no_fornecedor = 'X' and coalesce(notas2.qtd,0) = 0
                        then '8'
                      else
                        case when header.transf_no_fornecedor = '' and coalesce(notas2.qtd,0) = 0
                          then '9'
                        else
                          case when header.transf_no_fornecedor = '' and coalesce(notas2.qtd,0) > 0
                            then 
                              case when conf_parc.qtd > 0 and conf_tot.qtd = 0
                                then cast('10' as abap.char(2))
                              else
                                case when conf_parc.qtd > 0 and conf_tot.qtd > 0 and coalesce(zib.qtd,0) = 0
                                  then cast('11' as abap.char(2))
                                else
                                  case when conf_parc.qtd > 0 and conf_tot.qtd > 0 and coalesce(zib.qtd,0) > 0
                                    then cast('13' as abap.char(2))
                                  else
                                    case when conf_parc.qtd = 0 and conf_tot.qtd > 0 and coalesce(zib.qtd,0) > 0
                                      then cast('12' as abap.char(2))
                                    else
                                      case when conf_parc.qtd = 0 and conf_tot.qtd > 0 and coalesce(zib.qtd,0) = 0
                                        then
                                          case when carga_frete.fknum = ''
                                            then cast('14' as abap.char(2))
                                          else
                                            case when frete_pago.stblg = ''
                                              then cast('15' as abap.char(2))
                                            else cast('16' as abap.char(2))
                      
       end end end end end end end end end end end end end end end end end end as status
}
