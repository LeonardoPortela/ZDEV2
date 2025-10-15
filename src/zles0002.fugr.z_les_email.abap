FUNCTION z_les_email.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(V_POSTO) TYPE  ZCODPOSTO OPTIONAL
*"     REFERENCE(V_TRANS) TYPE  LIFNR OPTIONAL
*"     REFERENCE(V_STATUS) TYPE  ZBL DEFAULT '2'
*"  TABLES
*"      LT_TRANS STRUCTURE  LFA1 OPTIONAL
*"      LT_EMAIS STRUCTURE  ADR6
*"----------------------------------------------------------------------

  CLEAR: lt_emais[].

  IF ( v_trans IS INITIAL ) AND ( lt_trans[] IS INITIAL ).
    SELECT smtp_addr
      INTO CORRESPONDING FIELDS OF TABLE lt_emais
      FROM zlest0029
     WHERE codposto = v_posto
       AND status   = v_status
       AND smtp_addr > space.
  ELSE.
    IF lt_trans[] IS INITIAL.
      SELECT smtp_addr
        INTO CORRESPONDING FIELDS OF TABLE lt_emais
        FROM zlest0029
       WHERE codposto = v_posto
         AND ( codtrp = v_trans
          OR   codtrp = space )
         AND status   = v_status
         AND smtp_addr > space.
    ELSE.
      SELECT smtp_addr
        INTO CORRESPONDING FIELDS OF TABLE lt_emais
        FROM zlest0029
         FOR ALL ENTRIES IN lt_trans
       WHERE codposto = v_posto
         AND ( codtrp = lt_trans-lifnr
          OR   codtrp = space )
         AND status   = v_status
         AND smtp_addr > space.
    ENDIF.
  ENDIF.

ENDFUNCTION.
