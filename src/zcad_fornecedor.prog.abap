*&---------------------------------------------------------------------*
*& Report ZCAD_FORNECEDOR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcad_fornecedor.

DATA: lva_cpf_transp TYPE string,
      l_bukrs        TYPE bukrs,
      l_branch       TYPE j_1bbranc_,
      lc_transp      TYPE zde_cad_fonecedor,  "*-CS2021000253-26.04.2024-#59941-JT
      lc_ord_carrega TYPE REF TO zcl_integracao_ord_carrega,
      i_inbound      TYPE zde_carguero_requisicao.

PARAMETERS: p_bukrs  TYPE bukrs DEFAULT '0001',
            p_branch TYPE j_1bbranc_ DEFAULT '0114'.

*=======================================================================================
i_inbound-data-transportador-documentos-cpf_cnpj = '29356146000156'.
i_inbound-data-transportador-documentos-razao_social = 'ALMEIDA PORTO TRANSPORTES LTDA'.
i_inbound-data-transportador-documentos-nome_fantasia = ''.
i_inbound-data-transportador-documentos-inscricao_estadual = '9076985054'.
i_inbound-data-transportador-documentos-rntrc = '050639044'.
i_inbound-data-transportador-documentos-pis_transportador_tac = ''.
i_inbound-data-transportador-endereco-logradouro = 'Avenida Nossa Senhora de Fátima'.
i_inbound-data-transportador-endereco-numero = '347'.
i_inbound-data-transportador-endereco-bairro = 'Jardim Porto Alegre'.
i_inbound-data-transportador-tipo-descricao = 'ETC'.
i_inbound-data-transportador-endereco-cep = '85906230'.
i_inbound-data-transportador-endereco-uf = 'PR'.
i_inbound-data-transportador-endereco-municipio-nome = 'Toledo'.
i_inbound-data-transportador-endereco-municipio-codigo_ibge = '4127700'.
i_inbound-data-transportador-contatos-email = ''.
i_inbound-data-transportador-contatos-telefone = '4530530090'.
i_inbound-data-transportador-data_nascimento = '0001-01-01T04:00:00Z'.
i_inbound-data-transportador-sexo = ''.
i_inbound-data-transportador-filiacao-nome_mae = ''.
*=======================================================================================

CREATE OBJECT lc_ord_carrega.

CLEAR: lc_transp.

IF zcl_string=>length( text = i_inbound-data-transportador-documentos-cpf_cnpj ) GT 11.
  lc_transp-ds_doc_cnpj = i_inbound-data-transportador-documentos-cpf_cnpj.
ELSE.
  lc_transp-ds_doc_cpf  = i_inbound-data-transportador-documentos-cpf_cnpj.
  lva_cpf_transp        = lc_transp-ds_doc_cpf.
ENDIF.

lc_transp-ds_razao_social           = i_inbound-data-transportador-documentos-razao_social.
lc_transp-ds_nome_fantazia          = i_inbound-data-transportador-documentos-nome_fantasia.
lc_transp-ds_doc_ie                 = i_inbound-data-transportador-documentos-inscricao_estadual.
lc_transp-ds_doc_rntrc              = i_inbound-data-transportador-documentos-rntrc.
lc_transp-ds_doc_pis                = i_inbound-data-transportador-documentos-pis_transportador_tac.
lc_transp-ds_logradouro_rua         = i_inbound-data-transportador-endereco-logradouro.
lc_transp-ds_logradouro_nro         = i_inbound-data-transportador-endereco-numero.
lc_transp-ds_logradouro_bairro      = i_inbound-data-transportador-endereco-bairro.

CASE  i_inbound-data-transportador-tipo-descricao.
  WHEN 'CTC'.
    lc_transp-ds_indtyp  = 'Z3'.
  WHEN 'TAC'.
    IF lva_cpf_transp IS NOT INITIAL.
      lc_transp-ds_stcd5 = '782510/712'.
    ENDIF.
  WHEN OTHERS.
ENDCASE.

"Validar CEP
FIND REGEX '[0-9]{8}' IN i_inbound-data-transportador-endereco-cep.
IF sy-subrc IS INITIAL.
  lc_transp-ds_logradouro_cep = i_inbound-data-transportador-endereco-cep(5) && '-' && i_inbound-data-transportador-endereco-cep+5(3).
ENDIF.

lc_transp-ds_logradouro_uf          = i_inbound-data-transportador-endereco-uf.
lc_transp-ds_logradouro_cidade      = i_inbound-data-transportador-endereco-municipio-nome.
lc_transp-ds_logradouro_cidade_ibge = i_inbound-data-transportador-endereco-municipio-codigo_ibge.
lc_transp-ds_email                  = i_inbound-data-transportador-contatos-email.
lc_transp-ds_telefone               = i_inbound-data-transportador-contatos-telefone.
lc_transp-dt_nascimento             = i_inbound-data-transportador-data_nascimento.
lc_transp-ds_sexo                   = i_inbound-data-transportador-sexo.
lc_transp-ds_nome_mae               = i_inbound-data-transportador-filiacao-nome_mae.

DATA(lc_erro_transp) = abap_false.

TRY.
    lc_ord_carrega->set_cad_fornecedor_frete(
       EXPORTING
         i_bukrs          = p_bukrs
         i_branch         = p_branch
         i_fornecedor     = lc_transp
       IMPORTING
         e_parceiro       = DATA(e_parceiro_transp)
         e_msg            = DATA(e_msg_transp)
         e_msg_validacao  = DATA(e_msg_val_transp)
         e_outbound_forne = DATA(e_outbound_forne_transp)
     ).
  CATCH zcx_integracao INTO DATA(ex_integracao_transp).
    lc_erro_transp = abap_true.
    DATA(e_msg_erro_transp) = ex_integracao_transp->zif_error~get_msg_erro( ).
  CATCH zcx_error INTO DATA(ex_error_transp).
    lc_erro_transp = abap_true.
    e_msg_erro_transp = ex_error_transp->zif_error~get_msg_erro( ).
ENDTRY.
