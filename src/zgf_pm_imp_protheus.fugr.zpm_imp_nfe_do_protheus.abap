FUNCTION zpm_imp_nfe_do_protheus.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_NFE) TYPE  ZPME0039_T
*"  EXPORTING
*"     REFERENCE(E_NFE) TYPE  ZPME0043_T
*"----------------------------------------------------------------------
*&                 AMAGGI - Projeto
*&---------------------------------------------------------------------*
*& Criado por:  Anderson Oenning ( AO ) - Amaggi
*& Data      : 07/08/2019
*& Pedido por: Cleudo Ferreira
*& Chamado/Descrição : CS2019001199 - Interface recebimento de faturas de combustivel frota Amaggi - Posto Miriam
*&---------------------------------------------------------------------*
*& Histórico de Alterações:                                            *
*&---------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                *
*&---------------------------------------------------------------------*
*&             |            |               |                          *
*&--------------------------------------------------------------------
*&
*&--------------------------------------------------------------------
****Recebendo informações.

  DATA: nfe_ter TYPE REF TO zcl_nfe_inbound.
  CREATE OBJECT nfe_ter.
  DATA: t_pedido TYPE TABLE OF ekpo.
  DATA: gw_pedido TYPE zib_nfe_dist_ped.
  DATA: gw_zpmt0030        TYPE TABLE OF zpmt0030,
        w_zib_nfe_dist_ter TYPE zib_nfe_dist_ter.

  LOOP AT i_nfe INTO DATA(imp_nfe).


    DATA(chave_nfe) = |{ imp_nfe-chave_nfe ALPHA = IN }|.
    DATA(_fatura) = |{ imp_nfe-fatura ALPHA = IN }|.

*&--------------------------------------------------------------------
* "//Validando chave.
    TRY .
        nfe_ter->zif_cadastro~set_registro( i_id_registro = chave_nfe ).
        nfe_ter->set_info_sap( ).
        nfe_ter->set_coleta_tudo( i_ck_coleta_tudo = abap_true ).
        nfe_ter->ck_ignora_data_se_vencimento = abap_true.
        DATA(cabecalho) = nfe_ter->get_cabecalho_nota( ).
        DATA(lc_info) = nfe_ter->get_info_nota( ).
      CATCH zcx_cadastro INTO DATA(ex_cadastro).
        DATA(lc_msg_interna) = ex_cadastro->get_text( ).
        nfe_ter->free( ).
    ENDTRY.

    IF lc_info-nfe_base-numero IS NOT INITIAL.
      lc_info-nfe_base-numero = |{ lc_info-nfe_base-numero ALPHA = IN }|.
      DATA(w_pedido) = |{ imp_nfe-pedido ALPHA = IN }|.


      "Verificar se a chave Nfe ja foi processada.
*&--------------------------------------------------------------------
      CLEAR: w_zib_nfe_dist_ter.
      SELECT SINGLE * FROM zib_nfe_dist_ter
      INTO w_zib_nfe_dist_ter
      WHERE chave_nfe EQ chave_nfe
        AND belnr NE space.
      "Se encontrar informações é porque a NFe ja foi criado MIRO e não pode ser mais processadas,
      "Retonar a msg que a NFe ja foi processada.
      IF w_zib_nfe_dist_ter IS NOT INITIAL.
        APPEND VALUE #(  e_cod_retorn  = '999'
                        e_desc_retorn = 'Erro! chave -> ' && imp_nfe-chave_nfe && ' ja foi processada'
                         ) TO e_nfe.
        CONTINUE.
      ENDIF.



*&--------------------------------------------------------------------
*&--------------------------------------------------------------------
*"//    validar pedido.
      SELECT *
      FROM ekpo
      INTO TABLE t_pedido
        WHERE ebeln EQ w_pedido.

      IF  t_pedido IS NOT INITIAL.

*&--------------------------------------------------------------------
*"//    Criar migo.

*        TRY .
*
*            LOOP AT LC_INFO-NFE_PEDIDOS_ALV INTO DATA(WA_PEDIDOS_VINCULADOS).
*
*              GW_PEDIDO = VALUE #(  CHAVE_NFE = CHAVE_NFE
*                                    EBELN  = WA_PEDIDOS_VINCULADOS-EBELN
*                                    EBELP  = WA_PEDIDOS_VINCULADOS-EBELP ).
*
*              NFE_TER->ADD_PEDIDO_NOTA( EXPORTING I_PEDIDO = GW_PEDIDO I_EXCLUIR = ABAP_TRUE ).
*
*            ENDLOOP.
*
*            LOOP AT T_PEDIDO ASSIGNING FIELD-SYMBOL(<W_PEDIDO>) WHERE EBELN EQ W_PEDIDO.
*              GW_PEDIDO = VALUE #(  CHAVE_NFE = CHAVE_NFE
*                                  EBELN  = <W_PEDIDO>-EBELN
*                                  EBELP  = <W_PEDIDO>-EBELP
*                                  MATNR  = <W_PEDIDO>-MATNR
*                                  MENGE  = <W_PEDIDO>-MENGE
*                                  MEINS  = <W_PEDIDO>-MEINS
*                                  NETPR  = <W_PEDIDO>-NETPR
*                                  ).
*
*
*              NFE_TER->ADD_PEDIDO_NOTA( EXPORTING I_PEDIDO = GW_PEDIDO RECEIVING  R_ALV = DATA(R_ALV) ).
*            ENDLOOP.

*            "72 horas (dias úteis)
*            ZCL_MIRO=>GET_PROXIMO_VENC_FATURA( IMPORTING E_DATA_VENCIMENTO = DATA(E_DATA_VENCIMENTO)  ).
*
*            NFE_TER->SET_DT_VENCIMENTO( I_DT_VENCIMENTO = E_DATA_VENCIMENTO ).
*            NFE_TER->SET_ACEITAR_DOCUMENTO( ).
*            NFE_TER->SET_ACEITAR_FISICO( ).
*            NFE_TER->SET_ACEITAR_FATURAR( ).
*            NFE_TER->ZIF_CADASTRO~GRAVAR_REGISTRO( RECEIVING I_GRAVOU = DATA(I_GRAVOU) ).
*            DATA(LC_CABECALHO) = NFE_TER->GET_CABECALHO_NOTA( ).
*            NFE_TER->FREE( ).
*          CATCH ZCX_CADASTRO.
*            NFE_TER->FREE( ).
*        ENDTRY.
*
        APPEND VALUE #( chave_nfe = chave_nfe
                               fatura    = _fatura
*                              LIFNR      = LC_CABECALHO-P_EMISSOR
                              pedido     = w_pedido
                              nfe        = lc_info-nfe_base-numero
*                              MBLNR      = LC_CABECALHO-MBLNR
*                              BELNR      = LC_CABECALHO-BELNR
*                              DOCNUM_NFE = LC_CABECALHO-DOCNUM_NFE
                             ) TO gw_zpmt0030.


        APPEND VALUE #(  e_cod_retorn  = '0'
                         e_desc_retorn = 'NFe -> ' && lc_info-nfe_base-numero && ' gravada com sucesso'
                         )  TO e_nfe.
*&--------------------------------------------------------------------
*//    Gravando retornando mensagem.
      ELSE.
        APPEND VALUE #(  e_cod_retorn  = '999'
                         e_desc_retorn = 'Erro! pedido -> ' && w_pedido && ' desconhecida'
                          ) TO e_nfe.

      ENDIF.

    ELSE.
      APPEND VALUE #(  e_cod_retorn  = '999'
                       e_desc_retorn = 'Erro! chave -> ' && imp_nfe-chave_nfe && ' não encontrado'
                        ) TO e_nfe.

    ENDIF.

    CLEAR: imp_nfe, chave_nfe, _fatura, _fatura, lc_info.
  ENDLOOP.

*&--------------------------------------------------------------------
*//    Gravando informações na tabela.
  MODIFY zpmt0030 FROM TABLE gw_zpmt0030.
  COMMIT WORK.

  READ TABLE gw_zpmt0030 INTO DATA(w_retorn) INDEX 1.
  IF sy-subrc EQ 0.
    nfe_ter->free( ).
    CLEAR: lc_info, nfe_ter.

*   "// Processa os Dados que foi Enviado pelo Mobile
    CALL METHOD zcl_webservic_protheus=>call_report
      EXPORTING
        i_sequen   = CONV #( |{ w_retorn-nfe ALPHA = OUT }| )
        i_report   = 'ZPMR0055'
        i_external = abap_true.  " 31.10.2024 - RAMON - 156855

*   "// Finaliza o processo em caso de dados OffLine
    EXIT.
  ENDIF.

ENDFUNCTION.
