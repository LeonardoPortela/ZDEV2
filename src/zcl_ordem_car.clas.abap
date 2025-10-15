class ZCL_ORDEM_CAR definition
  public
  final
  create public .

*"* public components of class ZCL_ORDEM_CAR
*"* do not include other source files here!!!
public section.

  data AT_ZSDT0001OD type ZSDT0001OD .

  methods SET_MOTORISTA
    importing
      value(I_MOTORISTA) type ZMOTORISTA .
  methods SET_PLACA_CAV
    importing
      value(I_PLACA_CAV) type ZPLACA .
  methods SET_PLACA1
    importing
      value(I_PLACA1) type ZPLACA .
  methods SET_PLACA2
    importing
      value(I_PLACA2) type ZPLACA .
  methods SET_PLACA3
    importing
      value(I_PLACA3) type ZPLACA .
  methods GET_MOTORISTA
    returning
      value(E_MOTORISTA) type ZMOTORISTA .
  methods GET_PLACA_CAV
    returning
      value(E_PLACA_CAV) type ZPLACA .
  methods GET_PLACA1
    returning
      value(E_PLACA1) type ZPLACA .
  methods GET_PLACA2
    returning
      value(E_PLACA2) type ZPLACA .
  methods GET_PLACA3
    returning
      value(E_PLACA3) type ZPLACA .
  methods SET_MENSAGEM_RET
    importing
      value(I_MENSAGEM_RET) type STRING .
  methods GET_MENSAGEM_RET
    returning
      value(E_MENSAGEM_RET) type STRING .
protected section.
*"* protected components of class ZCL_ORDEM_CAR
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_ORDEM_CAR
*"* do not include other source files here!!!

  data AT_MOTORISTA type ZMOTORISTA .
  data AT_PLACA_CAV type ZPLACA .
  data AT_PLACA1 type ZPLACA .
  data AT_PLACA2 type ZPLACA .
  data AT_PLACA3 type ZPLACA .
  data AT_MENSAGEM_RET type STRING .
ENDCLASS.



CLASS ZCL_ORDEM_CAR IMPLEMENTATION.


method GET_MENSAGEM_RET.

  E_MENSAGEM_RET = ME->AT_MENSAGEM_RET.

endmethod.


method GET_MOTORISTA.
  E_MOTORISTA = ME->AT_MOTORISTA.
endmethod.


method GET_PLACA1.
  E_PLACA1 = ME->AT_PLACA1.
endmethod.


method GET_PLACA2.
  E_PLACA2 = ME->AT_PLACA2.
endmethod.


method GET_PLACA3.
  E_PLACA3 = ME->AT_PLACA3.
endmethod.


method GET_PLACA_CAV.
 E_PLACA_CAV = ME->AT_PLACA_CAV.
endmethod.


method SET_MENSAGEM_RET.

  ME->AT_MENSAGEM_RET = I_MENSAGEM_RET.

endmethod.


method SET_MOTORISTA.
 ME->AT_MOTORISTA = I_MOTORISTA.
endmethod.


method SET_PLACA1.
  ME->AT_PLACA1 = I_PLACA1.
endmethod.


method SET_PLACA2.
  ME->AT_PLACA2 = I_PLACA2.
endmethod.


method SET_PLACA3.
 ME->AT_PLACA3 = I_PLACA3.
endmethod.


method SET_PLACA_CAV.
 ME->AT_PLACA_CAV = I_PLACA_CAV.
endmethod.
ENDCLASS.
