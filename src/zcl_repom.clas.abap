class ZCL_REPOM definition
  public
  inheriting from ZCL_MONTA_XML
  create public .

public section.
protected section.

  methods GET_AUTENTICA
    importing
      !I_OBJETO type ref to ZIF_REPOM
    returning
      value(XML) type STRING .
private section.
ENDCLASS.



CLASS ZCL_REPOM IMPLEMENTATION.


  METHOD GET_AUTENTICA.

    DATA: OB_MONTA TYPE REF TO ZCL_MONTA_XML.

    DATA: WA_ZLEST0085 TYPE ZLEST0085.

    SELECT SINGLE *
      INTO WA_ZLEST0085
      FROM ZLEST0085
     WHERE OPERADORA EQ 'REP'
       AND BUKRS     EQ I_OBJETO->BUKRS
       AND BRANCH    EQ I_OBJETO->BRANCH.

    IF SY-SUBRC IS INITIAL.

      CREATE OBJECT OB_MONTA.
      OB_MONTA->LIMPAR( ).
      OB_MONTA->CTNAF( EXPORTING TAG = 'strCliente' VALOR = WA_ZLEST0085-USUARIO ).
      OB_MONTA->CTNAF( EXPORTING TAG = 'strAssinaturaDigital' VALOR = WA_ZLEST0085-SENHA ).
      OB_MONTA->GET_XML( IMPORTING E_XML_TEXTO = XML ).
      CLEAR: OB_MONTA.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
