FUNCTION Z_NFE_XML_VALIDAR.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CHAVE) TYPE  STRING
*"     REFERENCE(I_MATNR) TYPE  STRING OPTIONAL
*"     REFERENCE(I_MATKL) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_RETORNO) TYPE  ZDE_VAL_XML_NFE
*"----------------------------------------------------------------------

  IF I_MATNR IS NOT INITIAL AND I_MATKL IS NOT INITIAL.

    ZCL_NFE_XML=>ZIF_NFE_XML~GET_INSTANCE(
      )->SET_REGISTRO( I_CHAVE = CONV #( I_CHAVE )
      )->GET_VALIDAR(
      EXPORTING
        I_MATERIAL       = CONV #( ZCL_STRING=>LPAD( I_STR = I_MATNR I_QTD  = 18 I_CHAR = '0' ) )   " Nº do material
        I_GRUPO_MATERIAL = CONV #( ZCL_STRING=>LPAD( I_STR = I_MATKL I_QTD  = 09 I_CHAR = '0' ) )   " Grupo de mercadorias
      IMPORTING
        E_VALIDACAO      = E_RETORNO " Estrutura de Retorno de Validação de XML de NF-e
      ).

  ELSEIF I_MATNR IS NOT INITIAL AND I_MATKL IS INITIAL.

    ZCL_NFE_XML=>ZIF_NFE_XML~GET_INSTANCE(
      )->SET_REGISTRO( I_CHAVE = CONV #( I_CHAVE )
      )->GET_VALIDAR(
      EXPORTING
        I_MATERIAL       = CONV #( ZCL_STRING=>LPAD( I_STR = I_MATNR I_QTD  = 18 I_CHAR = '0' ) )   " Nº do material
      IMPORTING
        E_VALIDACAO      = E_RETORNO " Estrutura de Retorno de Validação de XML de NF-e
      ).

  ELSEIF I_MATNR IS INITIAL AND I_MATKL IS NOT INITIAL.

    ZCL_NFE_XML=>ZIF_NFE_XML~GET_INSTANCE(
      )->SET_REGISTRO( I_CHAVE = CONV #( I_CHAVE )
      )->GET_VALIDAR(
      EXPORTING
        I_GRUPO_MATERIAL = CONV #( ZCL_STRING=>LPAD( I_STR = I_MATKL I_QTD  = 09 I_CHAR = '0' ) )   " Grupo de mercadorias
      IMPORTING
        E_VALIDACAO      = E_RETORNO " Estrutura de Retorno de Validação de XML de NF-e
      ).

  ELSE.

    ZCL_NFE_XML=>ZIF_NFE_XML~GET_INSTANCE(
      )->SET_REGISTRO( I_CHAVE = CONV #( I_CHAVE )
      )->GET_VALIDAR(
      IMPORTING
        E_VALIDACAO      = E_RETORNO " Estrutura de Retorno de Validação de XML de NF-e
      ).

  ENDIF.

ENDFUNCTION.
