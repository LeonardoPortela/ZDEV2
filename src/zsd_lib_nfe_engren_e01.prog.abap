*----------------------------------------------------------------------*
* INITIALIZATION                                                       *
*----------------------------------------------------------------------*
initialization.

  "move text-s03 to sscrfields-functxt_01.
  "selection-screen function key 1.

  perform f_iniciar.

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN.                                               *
*----------------------------------------------------------------------*
at selection-screen.
  case sy-ucomm.
    when 'FC01'.
      perform f_consulta_nfe_liberada.
  endcase.

*----------------------------------------------------------------------*
*  START-OF-SELECTION                                                  *
*----------------------------------------------------------------------*
start-of-selection.

  perform f_selecao_dados.

* Se não conseguiu selecionar registros, msg de erro
  if t_alv[] is initial.
*   Não foram encontrados registros.
    if sy-batch eq ' '.
      message i368(00) with text-002.
      exit.
    else.
      write / text-002.
    endif.
    "  else.
* Limpa memória de logs e gera mensagem de Início de Processamento.
    perform f_trata_logs using abap_true.

    "    perform processar_dados.
* Carregar os arquivos gerados pelos sistemas legados
    perform f_trata_logs using space.
  endif.

end-of-selection.

  check p_erro is initial.

  if t_alv[] is initial.
*   Não foram encontrados registros.
    if sy-batch eq ' '.
      message i368(00) with text-002.
      exit.
    else.
      write / text-002.
    endif.
  else.
    perform apresentar_resultados.
  endif.
