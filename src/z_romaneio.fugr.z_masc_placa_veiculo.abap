FUNCTION z_masc_placa_veiculo.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_PLACA) TYPE  ZPLACA
*"  EXPORTING
*"     REFERENCE(E_RETURN) TYPE  P
*"----------------------------------------------------------------------

  DATA:
    v_letras_1(1)  TYPE c,
    v_letras_2(1)  TYPE c,
    v_letras_3(1)  TYPE c,
    v_numeros(1)   TYPE c,
    v_generico(1)  TYPE c,
    v_numeros_1(1) TYPE c,
    v_numeros_2(1) TYPE c.


  v_letras_1  = i_placa+0(1).
  v_letras_2  = i_placa+1(2).
  v_letras_3  = i_placa+2(3).
  v_numeros   = i_placa+3(1).
  v_generico  = i_placa+4(1).
  v_numeros_1  = i_placa+5(1).
  v_numeros_2 = i_placa+6(1).

  "Inicio validação.
  CLEAR: e_return.

  "Valida se a variavel é letra.
  IF  NOT v_letras_1 CA sy-abcde.
    e_return = 1.
  ENDIF.

  "Valida se a variavel é letra.
  IF  NOT v_letras_2 CA sy-abcde.
    e_return = 2.
  ENDIF.

  "Valida se a variavel é letra.
  IF  NOT v_letras_3 CA sy-abcde.
    e_return = 3.
  ENDIF.

  "Valida se a variavel é numero.
  IF NOT v_numeros CA '0123456789'.
    e_return = 4.
  ENDIF.

  "Valida se a variavel é letra ou numero
  IF NOT v_generico CA sy-abcde.
    IF NOT v_generico CA '0123456789'.
      e_return = 5.
    ENDIF.
  ENDIF.

IF NOT v_generico CA '0123456789'.
  IF NOT v_generico CA sy-abcde.
    e_return = 5.
  ENDIF.
ENDIF.


  IF NOT v_numeros_1 CA '0123456789'.
    e_return = 6.
  ENDIF.

  IF NOT v_numeros_2 CA '0123456789'.
    e_return = 7.
  ENDIF.

ENDFUNCTION.
