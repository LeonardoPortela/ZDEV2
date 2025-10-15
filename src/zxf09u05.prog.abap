*&---------------------------------------------------------------------*
*&  Include           ZXF09U05
*&---------------------------------------------------------------------*
DATA: E_STATUS(1),
      E_MESSA(64).

IF T_VBKPF-BUKRS IS NOT INITIAL.
  CALL FUNCTION 'Z_CONTROLE_FECHAMES'
    EXPORTING
      I_BUKRS  = T_VBKPF-BUKRS
      I_DATA   = T_VBKPF-BUDAT
    IMPORTING
      E_STATUS = E_STATUS
      E_MESSA  = E_MESSA
    EXCEPTIONS
      ERROR    = 1
      OTHERS   = 2.

  IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  IF  E_STATUS = 'E'.
    MESSAGE E000(Z01) WITH E_MESSA.
    EXIT.
  ENDIF.
ENDIF.
