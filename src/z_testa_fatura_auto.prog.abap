*&---------------------------------------------------------------------*
*& Report Z_TESTA_FATURA_AUTO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_testa_fatura_auto.

DATA: l_info         TYPE zde_integracao_http_config,
      o_cdata        TYPE string,
      server         TYPE REF TO if_http_server,
      lob_integracao TYPE REF TO zif_integracao_inbound.

*l_info-ds_body   = '{"romaneio":[{"ch_referencia_romaneio":"17789484802" }]}'.

l_info-ds_body   = '{"romaneio":[{"ch_referencia_romaneio":"17789557802" }]}'.

*l_info-ds_body   = '{"romaneio":[{"ch_referencia_romaneio":"17789484802"}],"inicia_faturamento":true,"informacoesfrete":{' &&
*                    '"agentefrete":{"codigo":"1006"},"administradorfrete":{"codigo":"09","descricao":"(TipFrete) UNIK S.A."' &&
*                    '},"adiantamento":{"valor":"3465.00"}},"informacoespedagio":{"creditapedagio":{"credita":true},' &&
*                    '"idrotaadministradora":"108215","qtdeeixos":"009","valor":"271.80","administradorpedagio":{"codigo":"09",' &&
*                    '"descricao":"(TipFrete) UNIK S.A."},"cartaopedagio":{"codigo":"O","descricao":"Visa Cargo (TipBank)"},' &&
*                    '"municipios":{"origem":{"uf":"MT","codigoibge":"5107792"},"destino":{"uf":"MT","codigoibge":"5107602"}},' &&
*                    '"ufspercurso":[{"ordem":"1","uf":"MT"},{"ordem":"2","uf":"GO"}]},"textofaturamento":"Teste Faturamento Automatico"}'.

l_info-ds_body   = '{"romaneio":[{"ch_referencia_romaneio":"17789557802"}],"inicia_faturamento":true,"informacoesfrete":{' &&
                    '"agentefrete":{"codigo":"1006"},"administradorfrete":{"codigo":"09","descricao":"(TipFrete) UNIK S.A."' &&
                    '},"adiantamento":{"valor":"3465.00"}},"informacoespedagio":{"creditapedagio":{"credita":true},' &&
                    '"idrotaadministradora":"108215","qtdeeixos":"009","valor":"271.80","administradorpedagio":{"codigo":"09",' &&
                    '"descricao":"(TipFrete) UNIK S.A."},"cartaopedagio":{"codigo":"O","descricao":"Visa Cargo (TipBank)"},' &&
                    '"municipios":{"origem":{"uf":"MT","codigoibge":"5107792"},"destino":{"uf":"MT","codigoibge":"5107602"}},' &&
                    '"ufspercurso":[{"ordem":"1","uf":"MT"},{"ordem":"2","uf":"GO"}]},"textofaturamento":"Teste Faturamento Automatico"}'.

l_info-ds_metodo = 'POST'.

TRY.
    FREE: lob_integracao.
    CREATE OBJECT lob_integracao TYPE zcl_faturamento_automatico.

    lob_integracao->set_data( i_info = l_info
                 )->processar_requisicao( IMPORTING e_msg              = o_cdata
                                                    e_zintegracao_log  = DATA(e_zintegracao_log)
                 )->configure_server( i_http_server  = server ).
  CATCH cx_root.
ENDTRY.

BREAK-POINT.
