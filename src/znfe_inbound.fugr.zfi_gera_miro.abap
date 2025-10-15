function zfi_gera_miro.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_CHAVE_NFE) TYPE  ZDE_CHAVE_DOC_E
*"     VALUE(I_CK_ESTORNAR) TYPE  CHAR01 DEFAULT ' '
*"     VALUE(I_CPF) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     VALUE(E_MESSAGEM_ERRO) TYPE  STRING
*"     VALUE(E_SUCESSO) TYPE  CHAR01
*"     VALUE(E_BELNR) TYPE  RE_BELNR
*"     VALUE(E_GJAHR) TYPE  GJAHR
*"----------------------------------------------------------------------
  data: nfe       type ref to zcl_nfe_inbound,
        cpf_limpo type string.

  e_sucesso = '0'.

  "Encontrar Usuário pelo CPF
  if i_cpf is not initial.

    cpf_limpo = i_cpf.
    replace all occurrences of '.' in cpf_limpo with '' ignoring case.
    replace all occurrences of '-' in cpf_limpo with '' ignoring case.

    data(qtd_cpf) = strlen( cpf_limpo ).
    if qtd_cpf gt 11.
      message s127 with cpf_limpo into e_messagem_erro.
      exit.
    endif.

    select * into table @data(it_adcp)
      from adcp
     where fax_number eq @cpf_limpo.

    if sy-subrc is not initial.
      message s128 with cpf_limpo into e_messagem_erro.
      exit.
    endif.

    select * into table @data(it_usr21)
      from usr21
      for all entries in @it_adcp
    where persnumber eq @it_adcp-persnumber.

    if sy-subrc is not initial.
      message s128 with cpf_limpo into e_messagem_erro.
      exit.
    endif.

    describe table it_usr21 lines data(qtd_usuarios).
    if qtd_usuarios gt 1.
      message s129 with cpf_limpo into e_messagem_erro.
      exit.
    endif.

    read table it_usr21 index 1 into data(wa_usr21).

  endif.
  try .
      create object nfe.

      "Localizando a NF-e
      nfe->zif_cadastro~set_registro( i_id_registro = i_chave_nfe ).

      case i_ck_estornar.
        when abap_true.

          nfe->nfe_inbound_cancela_fatura( ).
          e_sucesso = '1'.

        when abap_false.

          "Aceite Fatura
          nfe->ck_ignora_data_se_vencimento = abap_true.
          nfe->set_aceitar_faturar( exporting i_us_miro = wa_usr21-bname ).
          if nfe->zif_cadastro~gravar_registro( ) eq abap_true.
            data(nota) = nfe->get_info_nota( ).
            e_belnr   = nota-nfe_base-belnr.
            e_gjahr   = nota-nfe_base-gjahr.
            if e_belnr is not initial and e_gjahr is not initial.
              e_sucesso = '1'.
            endif.
          else.
            data(retorno) = nfe->mensagens_retorno.
            nfe->free( ).
            clear: nfe.
            loop at retorno into data(wa_retorno) where type eq 'E'.
              message id wa_retorno-id type wa_retorno-type number wa_retorno-number
                with wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4 into e_messagem_erro.
              exit.
            endloop.
            message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into e_messagem_erro.
            exit.
          endif.
      endcase.

    catch zcx_miro_exception into ex_miro.
      ex_miro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    catch zcx_cadastro into ex_cadastro.
      message id ex_cadastro->msgid type 'S' number ex_cadastro->msgno with ex_cadastro->msgv1 ex_cadastro->msgv2 ex_cadastro->msgv3 ex_cadastro->msgv4 into e_messagem_erro.
    catch zcx_pedido_compra_exception into data(ex_pedido).
      message id ex_pedido->msgid type 'S' number ex_pedido->msgno with ex_pedido->msgv1 ex_pedido->msgv2 ex_pedido->msgv3 ex_pedido->msgv4 into e_messagem_erro.
    catch zcx_charg_exception into data(ex_charg).
      message id ex_charg->msgid type 'S' number ex_charg->msgno with ex_charg->msgv1 ex_charg->msgv2 ex_charg->msgv3 ex_charg->msgv4 into e_messagem_erro.
    catch zcx_nfe_inbound_exception into data(ex_nfein).
      message id ex_nfein->msgid type 'S' number ex_nfein->msgno with ex_nfein->msgv1 ex_nfein->msgv2 ex_nfein->msgv3 ex_nfein->msgv4 into e_messagem_erro.
  endtry.

  if nfe is not initial.
    nfe->free( ).
    clear: nfe.
  endif.



endfunction.
