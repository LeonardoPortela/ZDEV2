PROCESS BEFORE OUTPUT.
*{   INSERT         DEVK9A1R40                                        1
  MODULE status_0300.

  CALL SUBSCREEN: obs INCLUDING sy-repid '0500'.

*}   INSERT
PROCESS AFTER INPUT.
*{   INSERT         DEVK9A1R40                                        1
  CALL SUBSCREEN: obs.
  MODULE user_command_0300.

  FIELD wa_edit-zterm MODULE atualiza_tela ON CHAIN-REQUEST.
  FIELD wa_edit-data_pgto MODULE limpa_juros ON CHAIN-REQUEST.
**<<<------"163316 - NMS - INI------>>>
*  FIELD wa_edit-doc_cont MODULE zm_obligatory_check."171608 - SMC
  FIELD wa_edit-doc_cont MODULE zm_doc_cont ON CHAIN-REQUEST.
**<<<------"163316 - NMS - FIM------>>>
*}   INSERT
PROCESS ON VALUE-REQUEST.
*{   INSERT         DEVK9A1R40                                        1
  FIELD wa_edit-forma_pag MODULE f4_form_pgto.
*}   INSERT
