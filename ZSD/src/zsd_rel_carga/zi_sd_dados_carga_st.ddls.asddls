@AbapCatalog.sqlViewName: 'ZVSDCARGAST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Status Carga'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_SD_DADOS_CARGA_ST
  as select distinct from zsdt0133 as Carga

    left outer join       zsdt0410 as NFTranf           on  Carga.nro_cg     = NFTranf.nro_cg
                                                        and NFTranf.processo = '1'

    left outer join       zsdt0410 as NFCompra          on  Carga.nro_cg      = NFCompra.nro_cg
                                                        and NFCompra.processo = '2'
    left outer join       zsdt0001 as RomaneioEmAberto  on  Carga.nro_cg             =  RomaneioEmAberto.nro_cg
                                                        and RomaneioEmAberto.st_proc <> '99'

    left outer join       zsdt0001 as RomaneioEncerrado on  Carga.nro_cg             = RomaneioEmAberto.nro_cg
                                                        and RomaneioEmAberto.st_proc = '99'

{
  key Carga.nro_cg as Carga,

      case coalesce(RomaneioEmAberto.nro_cg, '')

        when Carga.nro_cg
            then '6' //Romaneio Gerado
        else
            case coalesce(RomaneioEncerrado.nro_cg, '')
              when Carga.nro_cg
            then '7' //Romaneio Finalizado
            else
               case Carga.carga_conferida
                 when 'X'
               then 'D' //Carga conferida Total
                 else

                    case coalesce( NFCompra.nro_cg, '' )

                      when Carga.nro_cg
                         then 'B' //Carga com NF Fornecedor
                      else

                        case coalesce( NFTranf.nro_cg, '' )

                        when Carga.nro_cg
                             then 'A' //Carga com NF Transf. Fornecedor

                        else

                          case Carga.doc_bordero_anexado
                            when ''
                              then case Carga.dt_autorizacao_embarque
                                when ''
                                 then case Carga.cod_transportadora
                                    when ''
                                     then
                                       case Carga.integrar_carguero
                                        when 'X'
                                           then '3' //Carga em Cotação
                                        else
                                            case Carga.carga_em_cotacao
                                            when 'X'
                                            then '3' //Carga em Cotação
                                            else
                                                '1' //Criado
                                            end
                                        end
                                    else
                                     '4' //Frete Contratado
                                     end
                                else
                                  '5' //Embarque Autorizado
                              end

                            else
                             'C' //Carga com Bordero CD

                          end

                        end

                      end

                  end

              end

        end        as StatusCarga



}
