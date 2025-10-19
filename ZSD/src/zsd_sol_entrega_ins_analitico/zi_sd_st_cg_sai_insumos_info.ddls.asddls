@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Solicitações Entrega Insumos'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SD_ST_CG_SAI_INSUMOS_INFO
  as

  select from       zsdt0133                     as cg
    left outer join ZI_SD_ST_ROM_CG_SAI_INS_INFO as sf on cg.nro_cg = sf.nro_cg
    left outer join ZI_SD_CG_SAI_INS_COMP_INFO   as cp on cg.nro_cg = cp.nro_cg

{
  key cg.nro_cg,

      case
        when cg.status = 'X' then
           'X' /* Carga Cancelada' */
        else

          case
             when cg.versao_processo = '00' then

                case
                   when cg.status = '6' then
                     '7' /* Romaneio Finalizado */
                   else
                     cg.status
                end

             else

                 case

                   when coalesce( sf.count_romaneio_pendente, 0 ) = 0 and coalesce( sf.count_romaneio_finalizado, 0 ) > 0 then
                      '7' /* Romaneio Finalizado */
                   else
                      case
                        when coalesce( sf.count_romaneio_pendente, 0 ) > 0 then
                          '6' /* Romaneio Gerado */
                        else
                            case
                               when cg.carga_conferida = 'X' then
                                  'D' /* Carga Conferida Total */
                               else

                                    case
                                     when coalesce( cp.count_nf_venda, 0 ) > 0 then
                                       'B' /* Carga com NF Fornecedor */
                                     else
                                        case
                                          when coalesce( cp.count_nf_transf_forn, 0 ) > 0 then
                                           'A' /* Carga com NF Transf. Fornecedor */
                                          else
                                             case
                                                when coalesce( cp.count_bordero, 0 ) > 0 then
                                                'C' /* Carga com Bordero CD */
                                                else

                                                   case
                                                   when cg.dt_autorizacao_embarque <> '00000000' then
                                                    '5' /* Embarque Autorizado */
                                                     else

                                                       case
                                                       
                                                       when cg.dt_frete_contratado <> '00000000' then
                                                        '4' /* Frete Contratado */
                                                         else

                                                            case
                                                            when cg.integrar_carguero = 'X' or cg.carga_em_cotacao = 'X' then
                                                            '3' /* Carga em Cotação */
                                                              else
                                                              
                                                                case
                                                                   when cg.integrar_carguero = 'X' or cg.carga_em_cotacao = 'X' then
                                                                      '1' /* Carga Criada */
                                                                 end

                                                            end

                                                       end


                                                   end
                                             end

                                        end


                                  end

                            end

                      end

                  end

          end

      end as status_carga

}
