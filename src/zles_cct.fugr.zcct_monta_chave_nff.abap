FUNCTION zcct_monta_chave_nff .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_NUMNF) TYPE  J_1BNFNUM9
*"     REFERENCE(I_AAMM_EMISSAO) TYPE  CHAR04
*"     REFERENCE(I_SERIE) TYPE  J_1BSERIES
*"     REFERENCE(I_CNPJ) TYPE  STCD1 OPTIONAL
*"     REFERENCE(I_CPF) TYPE  STCD2 OPTIONAL
*"     REFERENCE(I_CUF_IBGE) TYPE  ZCHAR02 OPTIONAL
*"     REFERENCE(I_CUF_SIGLA) TYPE  ZCHAR02 OPTIONAL
*"     REFERENCE(I_MODELO) TYPE  J_1BMODEL
*"  EXPORTING
*"     REFERENCE(E_CHAVE_NFF) TYPE  ZDE_CHAVE_NFF
*"  CHANGING
*"     REFERENCE(C_RETORNO) TYPE  ZDE_RETORNO_PROC
*"----------------------------------------------------------------------


  DATA: V_INF_PARID       TYPE TY_INF_PARID,
        V_C14             TYPE C LENGTH 14,
        V_SERIE           TYPE J_1BNFE_ACTIVE-SERIE,
        V_NFNUM9          TYPE J_1BNFE_ACTIVE-NFNUM9,
        V_SIGLA_UF        TYPE ZCHAR02,
        V_MSG             TYPE STRING.

  CLEAR: e_chave_nff.

  IF i_cuf_sigla IS NOT INITIAL.
    v_sigla_uf = i_cuf_sigla.
  ELSEIF i_cuf_ibge IS NOT INITIAL.
    ZCL_ESTADO=>ZIF_ESTADO~GET_INSTANCE( )->get_sigla_estado( EXPORTING
                                                                 i_id_bacen = CONV #( i_cuf_ibge )
                                                              IMPORTING
                                                                 e_uf       =  DATA(_sigla_uf) ).
    v_sigla_uf = _sigla_uf.
  ENDIF.

  IF I_CPF IS NOT INITIAL.
    V_C14  = I_CPF.
  ELSE.
    V_C14  = I_CNPJ.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = V_C14
   IMPORTING
      OUTPUT = V_C14.

  V_SERIE   = I_SERIE.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = V_SERIE
    IMPORTING
      OUTPUT = V_SERIE.

  v_nfnum9  = I_NUMNF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = V_NFNUM9
    IMPORTING
      OUTPUT = V_NFNUM9.

  IF ( strlen( v_serie ) NE 3 ).
    MESSAGE s069 WITH v_nfnum9 INTO v_msg.
    c_retorno-type     = 'E'.
    c_retorno-msgno    = sy-msgno.
    c_retorno-texto    = v_msg.
    RETURN.
  ENDIF.

  IF ( strlen( v_nfnum9 ) NE 9 ) OR ( v_nfnum9 EQ '000000000'  ).
    MESSAGE s070 WITH v_nfnum9 INTO v_msg.
    c_retorno-type     = 'E'.
    c_retorno-msgno    = sy-msgno.
    c_retorno-texto    = v_msg.
    RETURN.
  ENDIF.

  e_chave_nff   = 'F'                          &&
                  v_sigla_uf                   &&
                  i_aamm_emissao+0(2)          &&
                  i_aamm_emissao+2(2)          &&
                  v_c14                        &&
                  i_modelo                     &&
                  v_serie                      &&
                  v_nfnum9.


  IF strlen( e_chave_nff ) NE 35.
    MESSAGE s080 WITH v_nfnum9 INTO v_msg.
    c_retorno-type     = 'E'.
    c_retorno-msgno    = sy-msgno.
    c_retorno-texto    = v_msg.
    RETURN.
  ENDIF.


ENDFUNCTION.
