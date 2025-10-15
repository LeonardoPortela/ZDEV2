*&---------------------------------------------------------------------*
*&  Include           ZXCOFU04
*&---------------------------------------------------------------------*
DATA: VG_BUKRS        TYPE J_1BBRANCH-BRANCH,
      E_STATUS(1),
      E_MESSA(64).

IF AFRUD_IMP-WERKS IS NOT INITIAL.
  SELECT SINGLE BUKRS
     INTO VG_BUKRS
     FROM J_1BBRANCH
   WHERE BRANCH = AFRUD_IMP-WERKS.

  IF SY-SUBRC = 0.
    CALL FUNCTION 'Z_CONTROLE_FECHAMES'
      EXPORTING
        I_BUKRS  = VG_BUKRS
        I_DATA   = AFRUD_IMP-BUDAT
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
ENDIF.
