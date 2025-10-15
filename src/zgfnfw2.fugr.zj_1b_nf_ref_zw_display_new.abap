FUNCTION ZJ_1B_NF_REF_ZW_DISPLAY_NEW.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(REF_KEY) TYPE  J_1BNFLIN-REFKEY
*"--------------------------------------------------------------------
  DATA: opt        TYPE ctu_params.

  PERFORM f_preencher_dynpro USING:
          'X' 'ZWRR0002'             '0100',
          ' ' 'P_SEQ_LCTO'           ref_key,
          ' ' 'BDC_OKCODE'           'SEARCH'.

  opt-dismode = 'E'.
  opt-defsize = ' '.
  opt-RACOMMIT = 'X'.

  CALL TRANSACTION 'ZNFW0002_D' USING tl_bdc OPTIONS FROM opt.

ENDFUNCTION.
