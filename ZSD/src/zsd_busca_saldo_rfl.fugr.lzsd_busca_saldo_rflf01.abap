*&-------------------------------------------------------------------------------------------------------*
*& Método         : INCLUDE LZSD_BUSCA_SALDO_RFLF01                                  *
*& Chamado        : USER STORY 163355                                                                    *
*& Data           : 31/01/2025                                                                           *
*& Especificado   : Paulo Quevedo                                                                        *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 31/01/2025|DEVK9A1XAW |NSEGATIN       | Define a Nota Fiscal de Saída conforme RA e o EUDR (163355).  *
*--------------------------------------------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& FORM ZF_DEFINE_NOTA_SAIDA
*&---------------------------------------------------------------------*
*& Define a Nota Fiscal de Saída conforme RA e o EUDR
*&---------------------------------------------------------------------*
*&      -->PT_FLOTE         Tabela de vinculo entre a formação de lote
*&      -->UV_CODIGO_RA     Código Recinto Alfandegado Embarque
*&      -->UV_EUDR          Atende critérios Europeu
*&      -->UV_RETORNAR_EUDR Listar também notas EUDR
*&      -->UV_DIRECT        Tipo de movimentação
*&---------------------------------------------------------------------*
FORM zf_define_nota_saida TABLES pt_flote             STRUCTURE zsdtvinc_p_flote
                           USING uv_codigo_ra         TYPE      zde_codigo_ra_embarque
                                 uv_eudr              TYPE      zeudr
                                 uv_retornar_eudr     TYPE      c
                                 uv_direct            TYPE      ztp_movi.

  DATA: tl_docnum    TYPE j_1bnfe_t_docnum,
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Início de Alteração
        ls_zlest0146 TYPE zlest0146.
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Fim de Alteração

  LOOP AT pt_flote INTO DATA(el_flote).
* Verifica o Tipo de movimentação
    CASE uv_direct.
      WHEN sy-abcde+18(1). "S-Saída
        APPEND el_flote-docnum_flote TO tl_docnum.

      WHEN sy-abcde+4(1).  "E-Entrada
        APPEND el_flote-docnum_eprod TO tl_docnum.

      WHEN OTHERS.
*     Do nothing
    ENDCASE.

  ENDLOOP.
* Consulta/Valida Term. de Embarque de NFe Saída por RA e EUDR
  zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->cnst_e_valida_term_nfe_saida(
  EXPORTING
    it_docnum =         tl_docnum
    i_cod_ra_embarque = uv_codigo_ra
    i_eudr            = uv_eudr
    i_retornar_eudr   = uv_retornar_eudr
    i_direct          = uv_direct
  IMPORTING
    et_consulta_terminal = DATA(tl_consulta_terminal) ).

  LOOP AT pt_flote INTO el_flote.
    DATA(vl_tabix) = sy-tabix.
    READ TABLE tl_consulta_terminal TRANSPORTING NO FIELDS WITH KEY docnum = el_flote-docnum_flote.

    IF NOT sy-subrc IS INITIAL.
      DELETE pt_flote INDEX vl_tabix.
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Início de Alteração
      CONTINUE.
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Fim de Alteração

    ENDIF.

*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Início de Alteração
    CLEAR ls_zlest0146.

    READ TABLE t_docnum_prod_rfl TRANSPORTING NO FIELDS
                                 WITH KEY docnum = el_flote-docnum_eprod
                                                           BINARY SEARCH.

    IF  sy-subrc   IS INITIAL
    AND v_tp_vinc1 IS NOT INITIAL.

      CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
        EXPORTING
          i_docnum    = el_flote-docnum_flote
        IMPORTING
          e_zlest0146 = ls_zlest0146.

      IF ls_zlest0146-dt_recepcao IS INITIAL.
        DELETE pt_flote INDEX vl_tabix.
        CONTINUE.
      ENDIF.

    ENDIF.
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Fim de Alteração

  ENDLOOP.

ENDFORM.
