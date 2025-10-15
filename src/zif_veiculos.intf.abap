interface ZIF_VEICULOS
  public .


  class-data AT_IF_VEICULOS type ref to ZIF_VEICULOS .
  data AT_VEICULO type ZLEST0002 .

  class-methods GET_INSTANCE
    returning
      value(R_IF_VEICULOS) type ref to ZIF_VEICULOS
    raising
      ZCX_VEICULOS .
  methods SET_VEICULO
    importing
      !I_PLACA type ZPC_VEICULO
    returning
      value(R_IF_VEICULOS) type ref to ZIF_VEICULOS
    raising
      ZCX_VEICULOS .
  methods GET_TIPO_CONTRATO
    exporting
      !E_TIPO_CONTRATO type VTTK_ADD01
    returning
      value(R_IF_VEICULOS) type ref to ZIF_VEICULOS
    raising
      ZCX_VEICULOS .
  methods GET_VALIDA_PLACA
    importing
      !I_PLACA type STRING optional
    returning
      value(R_IF_VEICULOS) type ref to ZIF_VEICULOS
    raising
      ZCX_VEICULOS .
  methods GET_CK_VEICULO_TRACAO
    returning
      value(R_IF_VEICULOS) type ref to ZIF_VEICULOS
    raising
      ZCX_VEICULOS .
  methods GET_CK_VEICULO_REBOQUE
    returning
      value(R_IF_VEICULOS) type ref to ZIF_VEICULOS
    raising
      ZCX_VEICULOS .
endinterface.
