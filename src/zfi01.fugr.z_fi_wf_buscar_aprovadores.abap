FUNCTION Z_FI_WF_BUSCAR_APROVADORES.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(KKBER) TYPE  KKBER
*"     REFERENCE(WAERS) TYPE  WAERS
*"     REFERENCE(KLIMK) TYPE  KLIMK
*"  EXPORTING
*"     REFERENCE(V_ERRO) TYPE  CMP_ERROR
*"  TABLES
*"      T_APROVADORES STRUCTURE  ZFIT_APROVADORES
*"  EXCEPTIONS
*"      APROVADOR_NAO_ENCONTRADO
*"----------------------------------------------------------------------

* Buscar Aprovadores do workflow de Administração de
* Limite de Crédito - WS99900007.
* Caso nenhum aprovador seja encontrado na tabela,
* o parâmetro de exportação V_ERRO retornará 'X' e
* será selecionado o aprovador administrador do WF
* cadastrado com a área de crédito = 9999.

CONSTANTS: c_9999(4) TYPE c VALUE '9999',
           c_x(1)    TYPE c VALUE 'X'.

  SELECT *
    FROM zfit_aprovadores
    INTO TABLE t_aprovadores
   WHERE KKBER = kkber   AND
         WAERS = waers   AND
         LIMITE_DE <= klimk.

  IF NOT sy-subrc IS INITIAL.

    SELECT *
      FROM zfit_aprovadores
      INTO TABLE t_aprovadores
     WHERE KKBER = c_9999.

    IF NOT sy-subrc IS INITIAL.
      RAISE APROVADOR_NAO_ENCONTRADO.
    ELSE.
      v_erro = c_x.
    ENDIF.

  ENDIF.

ENDFUNCTION.
