FUNCTION ZSOLICITA_NOVO_TRANSPORTE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IN_CNPJ_REMETENTE) TYPE  J_1BCGC
*"     REFERENCE(IN_CPF_REMENTENTE) TYPE  J_1BCPF
*"     REFERENCE(IN_IE_REMENTENTE) TYPE  J_1BSTAINS
*"     REFERENCE(IN_CNPJ_DESTINATARIO) TYPE  J_1BCGC
*"     REFERENCE(IN_IE_DESTINATARIO) TYPE  J_1BSTAINS
*"  EXPORTING
*"     REFERENCE(E_PROTOCOLO) TYPE  STRING
*"     REFERENCE(E_MENSAGEM_ERRO) TYPE  STRING
*"----------------------------------------------------------------------

  "Gerar Protocolo
  DATA I_FILTRO	TYPE ZDE_FILTRO_ZSDT0001OD.

  "Buscar Remetente """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  TRY .
      ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
      )->SET_PARCEIRO_CNPJ_CPF_IE(
        EXPORTING
          I_CNPJ          = IN_CNPJ_REMETENTE
          I_CPF           = IN_CPF_REMENTENTE
          I_INSC_ESTATUAL = IN_IE_REMENTENTE
      )->GET_ID_PARCEIRO(
         IMPORTING
           E_PARCEIRO    = DATA(E_REMETENTE)
      ).
    CATCH ZCX_PARCEIROS INTO DATA(EX_PARCEIRO).    " .
      MESSAGE ID EX_PARCEIRO->MSGID TYPE 'S' NUMBER EX_PARCEIRO->MSGNO WITH EX_PARCEIRO->MSGV1 EX_PARCEIRO->MSGV2 EX_PARCEIRO->MSGV3 EX_PARCEIRO->MSGV4 INTO E_MENSAGEM_ERRO.
      CONCATENATE 'Remetente: ' E_MENSAGEM_ERRO INTO E_MENSAGEM_ERRO SEPARATED BY SPACE.
      EXIT.
  ENDTRY.

  "Buscar Destinatário """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  TRY .
      ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
      )->SET_PARCEIRO_CNPJ_CPF_IE(
        EXPORTING
          I_CNPJ          = IN_CNPJ_DESTINATARIO
          I_INSC_ESTATUAL = IN_IE_DESTINATARIO
      )->GET_ID_PARCEIRO(
         IMPORTING
           E_PARCEIRO    = DATA(E_DESTINATARIO)
      )->CK_PARCEIRO_LOCAL_NEGOCIO(
      ).
    CATCH ZCX_PARCEIROS INTO EX_PARCEIRO.
      MESSAGE ID EX_PARCEIRO->MSGID TYPE 'S' NUMBER EX_PARCEIRO->MSGNO WITH EX_PARCEIRO->MSGV1 EX_PARCEIRO->MSGV2 EX_PARCEIRO->MSGV3 EX_PARCEIRO->MSGV4 INTO E_MENSAGEM_ERRO.
      CONCATENATE 'Destinatário: ' E_MENSAGEM_ERRO INTO E_MENSAGEM_ERRO SEPARATED BY SPACE.
      EXIT.
  ENDTRY.

  "Buscar

*  TRY .
*      I_FILTRO-INRORDEM[] = VALUE #().
*      ZCL_ORDEM_CARREGAMENTO=>GET_ORDEM_CARREGAMENTO( I_FILTRO = VALUE #(  ) ).
**      CATCH ZCX_ORDEM_CARREGAMENTO.    "
*      CATCH .
*
*    ENDTRY.
*
*
*    TRY .
*        ZCL_DOC_FISCAL_FT_ENTRADA=>ZIF_DOC_FISCAL_FT_ENTRADA~GET_INSTANCE(
*         )->SET_NEW_DOCUMENTO_ORDEM_CARREG(
*          EXPORTING
*            I_ID_ORDEM                =     " Ordem de Carregamento
*            I_ID_LOCAL_COLETA         =     " Nº conta do fornecedor
*            I_ID_ENTRADA              =     " Código do Tipo de Entrada
*            I_PESO_LIQ                =     " Peso líquido
*            I_NOTA_FISCAL             =     " Inf. de Nota Fiscal de Mercadoria
*            I_ID_ROTA_REPOM           =     " Identificador de Rota REPOM
*            I_ID_PERCURSO_REPOM       =     " Identificador de Percurso REPOM
*            I_NR_CARTAO_REPOM         =     " Cartão REPOM
**    RECEIVING
**      R_INSTANCIA               =     " Interface de Frete de Entrada
*        ).
**    CATCH ZCX_DOC_FISCAL_FT_ENTRADA.    "
*    ENDTRY.

  ENDFUNCTION.
