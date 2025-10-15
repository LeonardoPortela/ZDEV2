*&---------------------------------------------------------------------*
*& Include ZLES_TRATAR_CTE_SENDER
*&---------------------------------------------------------------------*

    READ TABLE ls_cte_data-partner_300_tab INTO ls_partner WITH KEY id = ls_cte_data-partner_ref-partner_id_rem.

    CHECK sy-subrc = 0.

    IF ls_partner-uf     = 'EX'       AND
       <ls_sender>-cnpj IS INITIAL    AND
       ls_partner-ie    IS INITIAL    AND
       ls_partner-c_mun  = '9999999'.
      <ls_sender>-cnpj              = '00000000000000'.
      <ls_sender>-state_inscription = 'ISENTO'.
    ENDIF.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
