interface ZIF_CADASTRO
  public .


  data CK_ALTEROU type CHAR01 .

  methods GET_REGISTRO
    exporting
      !E_REGISTRO type ANY .
  methods SET_REGISTRO
    importing
      !I_ID_REGISTRO type ANY
      !I_NAO_LIMPA type CHAR01 optional
    raising
      ZCX_CADASTRO .
  methods GRAVAR_REGISTRO
    importing
      !I_JOB type CHAR01 optional
    returning
      value(I_GRAVOU) type CHAR01
    raising
      ZCX_CADASTRO .
  methods LIMPAR_REGISTRO
    importing
      !I_NAO_LIMPA type CHAR01 optional .
  methods NOVO_REGISTRO .
  methods VALIDAR_REGISTRO
    returning
      value(E_VALIDOU) type CHAR01 .
  methods EXCLUIR_REGISTRO
    returning
      value(I_EXCLUIU) type CHAR01 .
  methods VALIDAR_EXCLUSAO
    returning
      value(E_VALIDOU) type CHAR01 .
  methods VALIDA_ATRIBUTO_ALTERAVEL
    importing
      !I_CAMPO type NAME_FELD
    returning
      value(R_PERMITIDO) type CHAR01 .
  methods CHECK_ENV_APROV_TAXA
    returning
      value(E_VALIDOU) type CHAR01 .
  methods SET_REGISTROEX
    importing
      !I_ID_REGISTRO type ANY
    raising
      ZCX_CADASTRO .
endinterface.
