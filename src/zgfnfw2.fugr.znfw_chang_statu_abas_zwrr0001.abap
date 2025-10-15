FUNCTION znfw_chang_statu_abas_zwrr0001.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_OPERACAO) TYPE  ZFIWRT0017-OPERACAO
*"     REFERENCE(I_USUARIO) TYPE  ZFIWRT0017-USNAM
*"     REFERENCE(I_STATUS) TYPE  ZFIWRT0017-ABA_BLQ OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_STATUS_OPR) TYPE  ZFIWRT0017-ABA_BLQ
*"  TABLES
*"      ET_STATUS_ABAS STRUCTURE  ZFIWRT0017
*"  EXCEPTIONS
*"      USER_NOT_FOUND
*"----------------------------------------------------------------------

  DATA: tl_0016         TYPE TABLE OF zfiwrt0016 WITH HEADER LINE,
        tl_0017         TYPE TABLE OF zfiwrt0017 WITH HEADER LINE,
        tl_chang_status TYPE TABLE OF zfiwrt0017 WITH HEADER LINE,
        wl_dzeile       TYPE zfiwrt0017-dzeile,
        wl_status_aux   TYPE zfiwrt0017-aba_blq,
        wl_flag.

  CLEAR: wl_dzeile, wl_status_aux, wl_flag.
  REFRESH: tl_0016, tl_0017, tl_chang_status, et_status_abas.

  CALL FUNCTION 'ZNFW_CHECK_STATU_ABAS_OPR'
    EXPORTING
      i_operacao       = i_operacao
    IMPORTING
      e_status_opr     = e_status_opr
    TABLES
      et_status_abas   = tl_0017
    EXCEPTIONS
      status_not_found = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  IF i_status IS INITIAL.
    SELECT *
        FROM zfiwrt0016
        INTO TABLE tl_0016
         WHERE usnam EQ i_usuario.

    IF sy-subrc IS NOT INITIAL.
      RAISE user_not_found.

    ENDIF.
    SORT: tl_0017 BY dzeile DESCENDING.
    READ TABLE tl_0017 INDEX 1.
    wl_dzeile = tl_0017-dzeile.

    SORT tl_0017 BY aba.
    LOOP AT tl_0016.
      IF sy-tabix EQ 1.
        READ TABLE tl_0017
          WITH KEY aba =  tl_0016-aba
                 BINARY SEARCH.


        IF tl_0017-aba_blq EQ 'L'.
          wl_status_aux = 'B'.
        ELSE.
          wl_status_aux = 'L'.
        ENDIF.
*      ELSE.
*       wl_status_aux
      ENDIF.
      ADD 1 TO wl_dzeile.

      MOVE: sy-mandt       TO tl_chang_status-mandt,
            i_operacao     TO tl_chang_status-operacao,
            tl_0016-aba    TO tl_chang_status-aba,
            wl_status_aux  TO tl_chang_status-aba_blq,
            wl_dzeile      TO tl_chang_status-dzeile,
            sy-uname       TO tl_chang_status-usnam,
            sy-datum       TO tl_chang_status-erdat,
            sy-uzeit       TO tl_chang_status-erzet.

*      ADD 1 TO tl_chang_status-dzeile.
      APPEND tl_chang_status.
      CLEAR : tl_0017.
    ENDLOOP.


    LOOP AT tl_0017.
      READ TABLE tl_chang_status
        WITH KEY aba = tl_0017-aba.

      IF sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      APPEND tl_0017 TO tl_chang_status.
    ENDLOOP.
    e_status_opr = 'L'.
    READ TABLE tl_chang_status
      WITH KEY aba_blq = 'B'.
    IF sy-subrc IS INITIAL.
      e_status_opr = 'B'.
    ENDIF.
  ELSE.
    SORT: tl_0017 BY dzeile DESCENDING.
    READ TABLE tl_0017 INDEX 1.

    MOVE: sy-mandt       TO tl_chang_status-mandt,
          i_operacao     TO tl_chang_status-operacao,
          'TAB_1'        TO tl_chang_status-aba,
          i_status       TO tl_chang_status-aba_blq,
          tl_0017-dzeile TO tl_chang_status-dzeile,
          sy-uname       TO tl_chang_status-usnam,
          sy-datum       TO tl_chang_status-erdat,
          sy-uzeit       TO tl_chang_status-erzet.

    ADD 1 TO tl_chang_status-dzeile.
    APPEND tl_chang_status.

    MOVE: 'TAB_2'        TO tl_chang_status-aba.
    ADD 1 TO tl_chang_status-dzeile.
    APPEND tl_chang_status.

    MOVE: 'TAB_3'        TO tl_chang_status-aba.
    ADD 1 TO tl_chang_status-dzeile.
    APPEND tl_chang_status.

    MOVE: 'TAB_4'        TO tl_chang_status-aba.
    ADD 1 TO tl_chang_status-dzeile.
    APPEND tl_chang_status.

    MOVE: 'TAB_5'        TO tl_chang_status-aba.
    ADD 1 TO tl_chang_status-dzeile.
    APPEND tl_chang_status.

    MOVE: 'TAB_6'        TO tl_chang_status-aba.
    ADD 1 TO tl_chang_status-dzeile.
    APPEND tl_chang_status.

    MOVE: 'TAB_7'        TO tl_chang_status-aba.
    ADD 1 TO tl_chang_status-dzeile.
    APPEND tl_chang_status.

    MOVE: 'TAB_8'        TO tl_chang_status-aba.
    ADD 1 TO tl_chang_status-dzeile.
    APPEND tl_chang_status.
  ENDIF.
  IF tl_chang_status[] IS NOT INITIAL.
    MODIFY zfiwrt0017 FROM TABLE tl_chang_status.
    COMMIT WORK AND WAIT.
    et_status_abas[] = tl_chang_status[].
  ENDIF.

ENDFUNCTION.
