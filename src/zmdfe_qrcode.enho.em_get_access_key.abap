METHOD GET_ACCESS_KEY .

    DATA: ls_nfe_active TYPE j_1b_nfe_access_key.

    MOVE-CORRESPONDING is_electronic_document TO ls_nfe_active.

    rv_nfe_access_key = ls_nfe_active.

ENDMETHOD.
