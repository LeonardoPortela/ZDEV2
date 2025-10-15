*----------------------------------------------------------------------*
***INCLUDE LZHCMT_PY_0005F02.
*----------------------------------------------------------------------*

FORM valida_dados.

  DATA: l_aprovador TYPE zhcmt_py_0005-aprovador_tmp,
        l_dtini     TYPE zhcmt_py_0005-dt_val_de_tmp,
        l_dtfim     TYPE zhcmt_py_0005-dt_val_ate_tmp.

  LOOP AT total.

    l_aprovador = total+45(20).
    l_dtini     = total+57(08).
    l_dtfim     = total+65(08).

    IF l_aprovador IS NOT INITIAL.
      IF l_dtini  IS INITIAL OR
         l_dtfim  IS INITIAL.
        MESSAGE s000(fb) WITH 'Informar Periodo validade Temporário' DISPLAY LIKE 'E'.
        vim_abort_saving = abap_true.
        EXIT.
      ENDIF.
    ENDIF.

    IF l_dtini IS NOT INITIAL.
      IF l_aprovador IS INITIAL OR
         l_dtfim IS INITIAL.
        MESSAGE s000(fb) WITH 'Informar Usuário e/ou Periodo validade Temporário' DISPLAY LIKE 'E'.
        vim_abort_saving = abap_true.
        EXIT.
      ENDIF.
    ENDIF.

    IF l_dtfim IS NOT INITIAL.
      IF l_aprovador IS INITIAL OR
         l_dtini     IS INITIAL.
        MESSAGE s000(fb) WITH 'Informar Usuário e/ou Periodo validade Temporário' DISPLAY LIKE 'E'.
        vim_abort_saving = abap_true.
        EXIT.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
