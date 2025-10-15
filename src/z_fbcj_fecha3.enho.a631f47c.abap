"Name: \FU:FCJ_DELETE_ENTRY\SE:BEGIN\EI
ENHANCEMENT 0 Z_FBCJ_FECHA3.
*
  DATA: E_STATUS TYPE CHAR0001,
        E_MESSA  TYPE CHAR0064.

  CLEAR: e_new_period, E_STATUS.
  LOOP AT itcj_sel_postings INTO lscj_posting.

    CHECK lscj_posting-document_status = 'P'. "Lançado com êxito (interface FI)

    CLEAR: E_STATUS, E_MESSA.

    CALL FUNCTION 'Z_CONTROLE_FECHAMES'
      EXPORTING
        I_BUKRS  = lscj_posting-COMP_CODE
        I_DATA   = lscj_posting-POSTING_DATE
      IMPORTING
        E_STATUS = E_STATUS
        E_MESSA  = E_MESSA
      EXCEPTIONS
        ERROR    = 1
        OTHERS   = 2.

    IF SY-SUBRC <> 0.
    ENDIF.

    IF  E_STATUS = 'E'.
      MESSAGE E016(Z01) with e_MESSA.
    ENDIF.
  ENDLOOP.

ENDENHANCEMENT.
