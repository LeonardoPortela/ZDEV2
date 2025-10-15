FUNCTION Z_NFE_ADD_HEADER_FIELDS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_HEADER) TYPE  ZBRNFE_DANFE_CABECALHO
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM
*"     REFERENCE(I_NFE_TYPE) TYPE  J_1BNFTYPE
*"     REFERENCE(I_ADDR1_VAL) TYPE  ADDR1_VAL
*"  EXPORTING
*"     REFERENCE(E_HEADER) TYPE  ZBRNFE_DANFE_CABECALHO
*"  EXCEPTIONS
*"      ERRO_DADOS
*"----------------------------------------------------------------------

*---- Carrega dados basicos
  e_header = i_header.

*---- Determina parceiros adicionais
  PERFORM z_parceiros_add USING    i_header
                                   i_docnum
                                   i_nfe_type
                          CHANGING e_header.

*---- Adequação dos paramentros da filial
  PERFORM z_descricao_filia USING i_header
                                  i_addr1_val
                            CHANGING e_header.

ENDFUNCTION.
