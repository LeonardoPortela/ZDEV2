*----------------------------------------------------------------------*
***INCLUDE LZBRNFE_ADDF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_FORMATA_CPF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_J_1BINNAD_CPF  text
*      <--P_E_HEADER_RETI_CNPJ  text
*----------------------------------------------------------------------*
FORM zf_formata_cpf  USING    p_cpf
                    CHANGING p_cpf_format.

  DATA lc_cpf_aux TYPE pbr99_cpf.

  CLEAR p_cpf_format.

  CHECK NOT p_cpf IS INITIAL.

  lc_cpf_aux = p_cpf.

  CALL FUNCTION 'HR_BR_CHECK_CPF_FORMAT'
    EXPORTING
      cpf_number               = lc_cpf_aux
    IMPORTING
      cpf_number_formatted     = lc_cpf_aux
    EXCEPTIONS
      cpf_format_not_supported = 1
      cpf_check_digit          = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  MOVE lc_cpf_aux TO p_cpf_format.

ENDFORM.                    " ZF_FORMATA_CPF
*&---------------------------------------------------------------------*
*&      Form  ZF_FORMATA_CGC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_J_1BINNAD_CGC  text
*      <--P_E_HEADER_RETI_CNPJ  text
*----------------------------------------------------------------------*
FORM zf_formata_cgc   USING    p_cgc
                     CHANGING p_cgc_format.


  DATA lc_cgc_aux TYPE pbr99_cgc.

  CLEAR p_cgc_format.

  CHECK NOT p_cgc IS INITIAL.

  CHECK p_cgc > 0.

  lc_cgc_aux = p_cgc.

  CALL FUNCTION 'HR_BR_CHECK_CGC_FORMAT'
    EXPORTING
      cgc_number               = lc_cgc_aux
    IMPORTING
      cgc_number_formatted     = lc_cgc_aux
    EXCEPTIONS
      cgc_format_not_supported = 1
      cgc_check_digit          = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  MOVE lc_cgc_aux TO p_cgc_format.

ENDFORM.                    " ZF_FORMATA_CGC
*&---------------------------------------------------------------------*
*&      Form  Z_PARCEIROS_ADD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_HEADER  text
*      -->P_I_DOCNUM  text
*      -->P_I_NFE_TYPE  text
*      <--P_E_HEADER  text
*----------------------------------------------------------------------*
FORM z_parceiros_add  USING    i_header TYPE  zbrnfe_danfe_cabecalho
                               i_docnum TYPE  j_1bdocnum
                               i_nfe_type TYPE  j_1bnftype
                      CHANGING e_header TYPE  zbrnfe_danfe_cabecalho.
*---- Dados Auxiliares
  DATA: tl_j_1bnfe_par_map TYPE TABLE OF j_1bnfe_par_map,
        tl_j_1bnfnad       TYPE TABLE OF j_1bnfnad,
        el_t005t           TYPE t005t,
        el_j_1bnfe_par_map TYPE j_1bnfe_par_mapv,
        el_j_1bnfnad       TYPE j_1bnfnad,
        el_parc_recebedor  TYPE j_1binnad.

*---- Buscar dados de configuração de dados adicionais
*---- Só executar processo se encontrar configuração valida
  SELECT * INTO TABLE tl_j_1bnfe_par_map
    FROM j_1bnfe_par_map
    WHERE nfe_type = i_nfe_type.
  IF sy-subrc EQ  0.

**---- Busca os parceiros da ordem
    SELECT * INTO TABLE tl_j_1bnfnad
      FROM j_1bnfnad
      WHERE docnum = i_docnum.
    IF sy-subrc EQ  0.


*---- Carrega dados do local de entrega
      READ TABLE tl_j_1bnfe_par_map INTO el_j_1bnfe_par_map
                          WITH KEY partner_role = '0'. " Delivery Place
      IF sy-subrc EQ 0.
        READ TABLE tl_j_1bnfnad INTO el_j_1bnfnad
                          WITH KEY parvw = el_j_1bnfe_par_map-parvw.
        IF sy-subrc EQ 0.

*---- Nome
          CONCATENATE el_j_1bnfnad-parid
                      ' - '
                      el_j_1bnfnad-name1
                      el_j_1bnfnad-name2
                 INTO e_header-reti_nome
                 SEPARATED BY space.

*--- DEST_CNPJ - Destinatário - CNPJ (Formatado)
          IF el_j_1bnfnad-cgc IS INITIAL.
            PERFORM zf_formata_cpf
              USING    el_j_1bnfnad-cpf
              CHANGING e_header-reti_cnpj.
          ELSE.
            PERFORM zf_formata_cgc
              USING    el_j_1bnfnad-cgc
              CHANGING e_header-reti_cnpj.
          ENDIF.

*---- Endereço
          e_header-reti_endereco = el_j_1bnfnad-stras.

*---- Bairro
          e_header-reti_bairro = el_j_1bnfnad-ort02.

*---- CEP
          e_header-reti_cep = el_j_1bnfnad-pstlz.

*---- Cidade
          e_header-reti_cidade = el_j_1bnfnad-ort01.

*---- Telefone
          e_header-reti_fone = el_j_1bnfnad-telf1.

*---- Destinatário - UF
          e_header-reti_uf  = el_j_1bnfnad-regio.

*---- Inscrição Estadual
          e_header-reti_insc_est = el_j_1bnfnad-stains.

*---- Se parceiro não for do pais
          IF el_j_1bnfnad-land1 <> 'BR'.

*--- Uf sera externa
            e_header-reti_uf  =  'EX'.

*--- Dados do pais
            SELECT SINGLE *
               FROM t005t
               INTO el_t005t
              WHERE spras = sy-langu
                AND land1 = el_j_1bnfnad-land1.

            CONCATENATE el_j_1bnfnad-ort01 '-'
                        el_t005t-landx50
                   INTO e_header-reti_cidade
              SEPARATED BY space.

            TRANSLATE e_header-reti_cidade TO UPPER CASE.
          ENDIF.
        ENDIF.
      ENDIF.

*---- Carrega dados do local de retirada
      READ TABLE tl_j_1bnfe_par_map INTO el_j_1bnfe_par_map
                          WITH KEY partner_role = '1'. " Withdrawal Place
      IF sy-subrc EQ 0.
        READ TABLE tl_j_1bnfnad INTO el_j_1bnfnad
                          WITH KEY parvw = el_j_1bnfe_par_map-parvw.

        IF sy-subrc EQ 0.


*---- Nome
          CONCATENATE el_j_1bnfnad-parid
                      ' - '
                      el_j_1bnfnad-name1
                      el_j_1bnfnad-name2
                 INTO e_header-entr_nome
                 SEPARATED BY space.

*--- DEST_CNPJ - Destinatário - CNPJ (Formatado)
          IF el_j_1bnfnad-cgc IS INITIAL.
            PERFORM zf_formata_cpf
              USING    el_j_1bnfnad-cpf
              CHANGING e_header-entr_cnpj.
          ELSE.
            PERFORM zf_formata_cgc
              USING    el_j_1bnfnad-cgc
              CHANGING e_header-entr_cnpj.
          ENDIF.

*---- Endereço
          e_header-entr_endereco = el_j_1bnfnad-stras.

*---- Bairro
          e_header-entr_bairro = el_j_1bnfnad-ort02.

*---- CEP
          e_header-entr_cep = el_j_1bnfnad-pstlz.

*---- Cidade
          e_header-entr_cidade = el_j_1bnfnad-ort01.

*---- Telefone
          e_header-entr_fone = el_j_1bnfnad-telf1.

*---- Destinatário - UF
          e_header-entr_uf  = el_j_1bnfnad-regio.

*---- Inscrição Estadual
          e_header-entr_insc_est = el_j_1bnfnad-stains.

*---- Se parceiro não for do pais
          IF el_j_1bnfnad-land1 <> 'BR'.

*--- Uf sera externa
            e_header-entr_uf  =  'EX'.

*--- Dados do pais
            SELECT SINGLE *
               FROM t005t
               INTO el_t005t
              WHERE spras = sy-langu
                AND land1 = el_j_1bnfnad-land1.

            CONCATENATE el_j_1bnfnad-ort01 '-'
                        el_t005t-landx50
                   INTO e_header-entr_cidade
              SEPARATED BY space.

            TRANSLATE e_header-entr_cidade TO UPPER CASE.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " Z_PARCEIROS_ADD
*&---------------------------------------------------------------------*
*&      Form  Z_DESCRIÇÃO_FILIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_HEADER  text
*      -->P_I_ADDR1_VAL  text
*      <--P_E_HEADER  text
*----------------------------------------------------------------------*
FORM z_descricao_filia  USING    i_header    TYPE zbrnfe_danfe_cabecalho
                                 i_addr1_val TYPE addr1_val
                        CHANGING e_header    TYPE zbrnfe_danfe_cabecalho.

*---- Variavel aux
  DATA: vl_string   TYPE string,
        tl_text_tab	TYPE STANDARD TABLE OF string,"TABLES PARAM
        wl_text_tab	LIKE LINE OF tl_text_tab.

  CLEAR vl_string.

  IF NOT i_addr1_val-street IS INITIAL.
    vl_string = i_addr1_val-street.
  ENDIF.

  IF NOT i_addr1_val-house_num1 IS INITIAL.
    CONCATENATE vl_string ', ' i_addr1_val-house_num1 INTO vl_string.
  ENDIF.

  IF NOT i_addr1_val-house_num2 IS INITIAL.
    CONCATENATE vl_string ', ' i_addr1_val-house_num2 INTO vl_string.
  ENDIF.

  IF NOT i_addr1_val-str_suppl1 IS INITIAL.
    CONCATENATE vl_string ' -' i_addr1_val-str_suppl1 INTO vl_string.
  ENDIF.

  IF NOT  i_addr1_val-city2 IS INITIAL.
    CONCATENATE vl_string ' -' i_addr1_val-city2 INTO vl_string.
  ENDIF.

  IF NOT vl_string IS INITIAL.
*---- Concatenar todos os valores fundamentais
    CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
      EXPORTING
        text                = vl_string
        flag_no_line_breaks = 'X'
        line_length         = '35'
      TABLES
        text_tab            = tl_text_tab.

    LOOP AT tl_text_tab INTO wl_text_tab.
      CASE sy-tabix.
        WHEN '1'.
          e_header-sadr-stras = wl_text_tab.
        WHEN '2'.
          e_header-sadr-strs2 = wl_text_tab.
        WHEN '3'.
          e_header-strs3 = wl_text_tab.
        WHEN OTHERS.
          EXIT.
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " Z_DESCRIÇÃO_FILIA
