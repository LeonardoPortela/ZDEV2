*----------------------------------------------------------------------*
***INCLUDE LZSD_ATUALIZA_NFEF01 .                                      *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  Z_FORM_LOTE                                              *
*&---------------------------------------------------------------------*
*                        Retorno Formação Lote                         *
*----------------------------------------------------------------------*
FORM Z_FORM_LOTE USING P_NOTAS TYPE ZIB_NOTA_FISCAL_SAP
                       P_DOC   TYPE J_1BNFDOC.

  CASE P_NOTAS-DOCSTAT.
    WHEN '1'.
      CASE P_NOTAS-TP_AUTHCOD.
*       Autorização/ Contigência
        WHEN '1' OR '5'.

          UPDATE ZSDT_RETLOTE
             SET STATUS     = 'V'
                 NF_RETORNO = P_DOC-NFENUM
           WHERE DOCNUM_RET EQ P_DOC-DOCNUM.

          UPDATE ZSDT_EXPORT
             SET STATUS = 'X'
                 NF_RETORNO = P_DOC-NFENUM
           WHERE DOCNUM EQ P_DOC-DOCNUM.

          "Ajusta Romaneio de Entrada e Cria Romaneio de Saída
          TRY .
              ZCL_CARGA_RECEBIMENTO=>ZIF_CARGA~SET_NOTA_ENTRADA_PROPRIA( I_DOCNUM = P_DOC-DOCNUM ).
            CATCH ZCX_CADASTRO.    "
            CATCH ZCX_CARGA.    "
            CATCH ZCX_PARCEIROS.    "
            CATCH ZCX_ORDEM_VENDA.    "
            CATCH ZCX_JOB.    "
            CATCH ZCX_PEDIDO_COMPRA_EXCEPTION.    "
            CATCH ZCX_ORDEM_CARREGAMENTO.    "
          ENDTRY.

*       Cancelamento/Estorno
        WHEN '2'.

          DELETE FROM ZSDT_RETLOTE
            WHERE DOCNUM_RET EQ P_DOC-DOCNUM.
          DELETE FROM ZSDT_EXPORT
           WHERE DOCNUM EQ P_DOC-DOCNUM.

          "Exclui Romaneio de Saída
          TRY .
              ZCL_CARGA_RECEBIMENTO=>ZIF_CARGA~GET_VERIFICA_EXISTE_SAIDA( I_DOCNUM = P_DOC-DOCNUM I_EXCLUIR_ROMANEIO = ABAP_TRUE ).
            CATCH ZCX_CARGA.
            CATCH ZCX_CADASTRO.
          ENDTRY.

      ENDCASE.
  ENDCASE.

ENDFORM.                    " Z_FORM_LOTE
