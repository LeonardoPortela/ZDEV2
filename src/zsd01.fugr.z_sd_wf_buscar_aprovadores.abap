FUNCTION Z_SD_WF_BUSCAR_APROVADORES.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(VKORG) TYPE  VKORG
*"     REFERENCE(KLIMK) TYPE  KLIMK
*"  EXPORTING
*"     REFERENCE(V_ERRO) TYPE  CMP_ERROR
*"  TABLES
*"      T_APROVADORES STRUCTURE  ZSDT_APROVADORES
*"  EXCEPTIONS
*"      APROVADOR_NAO_ENCONTRADO
*"----------------------------------------------------------------------

* Buscar Aprovadores do workflow de
* Limite de Crédito - WS99900008.
* Caso nenhum aprovador seja encontrado na tabela,
* o parâmetro de exportação V_ERRO retornará 'X' e
* será selecionado o aprovador administrador do WF
* cadastrado com a Organização de Vendas = 9999.

CONSTANTS: c_9999(4) TYPE c VALUE '9999',
           c_x(1)    TYPE c VALUE 'X'.

  SELECT *
    FROM zsdt_aprovadores
    INTO TABLE t_aprovadores
   WHERE VKORG = vkorg   AND
         VALOR_DE <= klimk.

  IF NOT sy-subrc IS INITIAL.

    SELECT *
      FROM zsdt_aprovadores
      INTO TABLE t_aprovadores
     WHERE VKORG = c_9999.

    IF NOT sy-subrc IS INITIAL.
      RAISE APROVADOR_NAO_ENCONTRADO.
    ELSE.
      v_erro = c_x.
    ENDIF.

  ENDIF.

ENDFUNCTION.
