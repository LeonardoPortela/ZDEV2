DATA: w_smart TYPE zaae_termo_quadro,
      w_exec  TYPE zaae_termo_executor,
      t_exec  TYPE zaat_termo_executor,
      l_lines TYPE i.

t_exec[] = it_smart_exec[].

CLEAR l_total_status.

LOOP AT it_smart_quadro INTO w_smart.
  l_total_status = l_total_status + w_smart-qntde.
ENDLOOP.

CLEAR:w_exec.
READ TABLE t_exec INTO w_exec INDEX 1.
IF sy-subrc = 0.
l_nome_exec1 = w_exec-nome_colab.
CLEAR:w_exec.
ENDIF.

CLEAR:w_exec.
READ TABLE t_exec INTO w_exec INDEX 2.
IF sy-subrc = 0.
l_nome_exec2 = w_exec-nome_colab.
CLEAR:w_exec.
ENDIF.

CLEAR:w_exec.
READ TABLE t_exec INTO w_exec INDEX 3.
IF sy-subrc = 0.
l_nome_exec3 = w_exec-nome_colab.
CLEAR:w_exec.
ENDIF.

CLEAR:w_exec.
READ TABLE t_exec INTO w_exec INDEX 4.
IF sy-subrc = 0.
l_nome_exec4 = w_exec-nome_colab.
CLEAR:w_exec.
ENDIF.


















