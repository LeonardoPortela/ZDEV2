

PROCESS BEFORE OUTPUT.

  MODULE tc_cond_list_change_tc_attr.
  MODULE status_2000.
  MODULE set_richtext.
  MODULE set_input_field.

  " 31.03.2023 - 107971 - RBL ------>
*** Inicio - ALX
  "MODULE set_iss_2000.
*** Fim - ALX
  " 31.03.2023 - 107971 - RBL ------<

  LOOP AT gt_scr_condition_list INTO gw_scr_condition_list
                                WITH CONTROL tc_condition_list
                                CURSOR tc_condition_list-current_line.

  ENDLOOP.

PROCESS AFTER INPUT.

  MODULE user_command_2000.

  "FIELD zspayment_data_nfse_inbound-dt_vencimento MODULE valida_data.

  LOOP AT gt_scr_condition_list.
    CHAIN.
      FIELD gw_scr_condition_list-base
                            MODULE tax_amount ON REQUEST.
    ENDCHAIN.
  ENDLOOP.

* US #170323 - MMSILVA - 02.05.2025 - Inicio
  FIELD zspayment_data_nfse_inbound-ck_fpol
  MODULE valida_data ON REQUEST.

  FIELD zspayment_data_nfse_inbound-obs_financeira
  MODULE valida_data ON REQUEST.
* US #170323 - MMSILVA - 02.05.2025 - Fim

PROCESS ON VALUE-REQUEST.
  FIELD zspayment_data_nfse_inbound-bvtyp MODULE zm_sh_bvtyp.
  "FIELD zspayment_data_nfse_inbound-pymt_meth MODULE zm_sh_pagam.
