CLEAR: v_cpf_cnpj,
       v_doc.

IF NOT NOTA_FISCAL-CARRIER-cgc IS INITIAL.
  CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
    EXPORTING
      input         = NOTA_FISCAL-CARRIER-cgc
    IMPORTING
      OUTPUT        = v_doc.

  v_cpf_cnpj = v_doc(18).

ELSE.
  CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
    EXPORTING
      input         = NOTA_FISCAL-CARRIER-cpf(11)
    IMPORTING
      OUTPUT        = v_doc.

  v_cpf_cnpj = v_doc(14).

ENDIF.






















