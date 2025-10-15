class ZCL_MIRO_FRETE definition
  public
  final
  create public .

public section.

  methods CRIAR_MIRO_FRETE .
  methods CONSTRUCTOR .
protected section.
private section.

  data AT_MIRO type ref to ZCL_MIRO .
ENDCLASS.



CLASS ZCL_MIRO_FRETE IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    CREATE OBJECT ME->AT_MIRO.

  ENDMETHOD.


  METHOD CRIAR_MIRO_FRETE.

*
*I_CABECALHO  TYPE ZDE_MIRO_CABECALHO
*I_ITENS  TYPE ZDE_MIRO_ITENS_T
*I_CONTAS	TYPE ZBAPI_INCINV_GL_ACCOUNT_T
*I_LINES_MIRO_TEXTO	TYPE TLINE_T
*I_BAPI_WAIT  TYPE BAPIWAIT  DEFAULT 'X'
*E_INVOICEDOCNUMBER	TYPE RE_BELNR
*E_FISCALYEAR	TYPE GJAHR
*E_RETORNO  TYPE BAPIRET2_T
*value( R_GEROU )	TYPE CHAR01
*
*    CALL METHOD ME->AT_MIRO->CRIAR
*      EXPORTING
*        I_CABECALHO        =
*        I_ITENS            =
*        I_CONTAS           =
*        I_LINES_MIRO_TEXTO =
**        I_BAPI_WAIT        = 'X'
**      IMPORTING
**        E_INVOICEDOCNUMBER =
**        E_FISCALYEAR       =
**        E_RETORNO          =
*      RECEIVING
*        R_GEROU            =
**      EXCEPTIONS
**        DATA_FI_MM_NAO     = 1
**        ERRO               = 2
**        OTHERS             = 3
*            .
*
*    IF SY-SUBRC <> 0.
**     Implement suitable error handling here
*    ENDIF.


  ENDMETHOD.
ENDCLASS.
