"Name: \TY:CL_EDOC_BR_CNPJ_FM_WRAP\IN:IF_EDOC_BR_CNPJ_FM_WRAP\ME:GET_CUSTOMER_FROM_CNPJ\SE:BEGIN\EI
ENHANCEMENT 0 ZDRC_GET_CUSTOMER_FROM_CNPJ.

   CALL FUNCTION 'J_1BNFE_GET_CUSTOMER_FROM_CNPJ'
      EXPORTING
        i_cnpj             = iv_cnpj
      IMPORTING
        et_customer        = rt_customer
      EXCEPTIONS
        not_found          = 1
        missing_parameters = 2
        OTHERS             = 3.

   EXIT.

ENDENHANCEMENT.
