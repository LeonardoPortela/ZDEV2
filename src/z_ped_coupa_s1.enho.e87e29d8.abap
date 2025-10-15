"Name: \FU:MS_ACCOUNT_ASSIGNMENT_PACKAGE\SE:END\EI
ENHANCEMENT 0 Z_PED_COUPA_S1.
*
    data: v_NETWR type ESLL-NETWR value '0.10'.

    FIELD-SYMBOLS <fs_comsrv> TYPE COMSRV.
    ASSIGN ('(SAPLMLSP)COMSRV') TO <fs_comsrv>.
    CHECK <fs_comsrv> IS ASSIGNED.
    CHECK <fs_comsrv> IS NOT INITIAL.
    "
    IF <fs_comsrv>-MWSKZ      EQ 'S1'  AND
       <fs_comsrv>-BSART+0(1) EQ 'Y'  AND
       E_TOTAL                NE TOTAL AND
       TOTAL                  LE v_NETWR.
       <fs_comsrv>-netpr = E_TOTAL.
       TOTAL = E_TOTAL.
       clear change_account.
    ENDIF.

ENDENHANCEMENT.
