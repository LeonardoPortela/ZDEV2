*&---------------------------------------------------------------------*
*&  Include           Z_TIPO_OPERACAO
*&---------------------------------------------------------------------*


CLASS Z_TIPO_OPERACAO DEFINITION.
  PUBLIC SECTION.
    METHODS: Z_CREATE_NEW_OPERACAO.
    METHODS: Z_SALVAR_REGISTROS.

ENDCLASS.                    "Z_TIPO_OPERACAO DEFINITION


*----------------------------------------------------------------------*
*       CLASS Z_TIPO_OPERACAO IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS Z_TIPO_OPERACAO IMPLEMENTATION.
  METHOD Z_CREATE_NEW_OPERACAO.
    OP_MODE = 'NEW'.

    CLEAR: WL_SAIDA_CABECALHO, GT_SAIDA_ITENS, WL_MENSAGEM,
           GT_MSG_RETURN, S_WERKS, WL_EMPRESA.
  ENDMETHOD.                    "Z_CREATE_NEW_OPERACAO

  METHOD Z_SALVAR_REGISTROS.
    DATA: REF_UTILS TYPE REF TO Z_UTILS.
    CREATE OBJECT REF_UTILS.

    REF_UTILS->Z_BAPI_CONTRACT_CREATE( ).

  ENDMETHOD.                    "Z_SALVAR_REGISTROS
ENDCLASS.                    "Z_TIPO_OPERACAO IMPLEMENTATION
