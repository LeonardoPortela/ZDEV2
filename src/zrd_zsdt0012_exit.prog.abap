*&-------------------------------------------------------------------------------------------------------*
*& Report         : ZRD_ZSDT0012_EXIT                                                                    *
*& Chamado        : USER STORY 169500                                                                    *
*& Data           : 07/05/2025                                                                           *
*& Especificado   : Paulo Quevedo                                                                    *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data      | Request    | Autor         | Alteração                                                   *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 07/05/2025 | DEVK9A2BIM | NSEGATIN      | Ajuste Melhorias Parte 2 - Exit da ZREGISTER_DATA tela 298  *
*&                                         | Limite de dias (Dev Inicial).                               *
*--------------------------------------------------------------------------------------------------------*
report zrd_zsdt0012_exit.
*&----------------------------------------------------------------------------------*
*&      Form  ZF_LIMIT_SELECT_OPTION
*&----------------------------------------------------------------------------------*
*       Exit da ZREGISTER_DATA tela 298
*-----------------------------------------------------------------------------------*
*       --> P_REGISTRO_MANTER WA do dados da Tabela da respectiva Exit.
*-----------------------------------------------------------------------------------*
form f_exit_zsdt0012_0001 changing p_registro_manter type any.

  data: el_zsdt0012 type zsdt0012.

  clear: el_zsdt0012.

  el_ZSDT0012-user_create = sy-uname.
  el_ZSDT0012-date_create = sy-datum.
  el_ZSDT0012-time_create = sy-uzeit.

  move-corresponding el_ZSDT0012 to p_registro_manter.

endform.

form f_exit_zsdt0012_0002 using p_registro_manter type any
                       changing p_error.

  data: wl_zsdt0012 type zsdt0012.

  move-corresponding p_registro_manter to wl_zsdt0012.

  select * from zsdt0012 into table @data(it_zsdt0012).
  if it_zsdt0012 is not initial.
    delete from zsdt0012.
    commit work.
*    message i024(sd) with 'TESTE'.
*    exit.
  endif.

endform.
