*----------------------------------------------------------------------*
*& Report  ZRD_ZSDT0278_EXIT
*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Monitor de Recebimentos de Frete Frota Própria          *
* Transação..: ZSDT0168                                                *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data | Nome | Request | Descrição                                    *
*----------------------------------------------------------------------*
* 20/04/21 | JBARBOSA | DEVK9A0SGV  | Desenvolvimento Inicial          *
*----------------------------------------------------------------------*
REPORT zrd_zsdt0278_exit.

*&---------------------------------------------------------------------*
*&  "Set atributos iniciais ao incluir um novo registro
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0278_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0278 TYPE zsdt0278_out.

  CLEAR: wl_zsdt0278.

  wl_zsdt0278-usuario_c  = sy-uname.
  wl_zsdt0278-data_c     = sy-datum.
  wl_zsdt0278-hora_c     = sy-uzeit.

  MOVE-CORRESPONDING wl_zsdt0278 TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Validações antes da gravação do registro
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0278_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

* Declaração de Estruturas
  DATA: w_zsdt0278 TYPE zsdt0278_out,
        l_msg      TYPE bapiret2-message.

  MOVE-CORRESPONDING p_registro_manter TO w_zsdt0278.

  IF w_zsdt0278-bukrs IS NOT INITIAL.

    "Empresas
    SELECT SINGLE butxt FROM t001
      INTO (w_zsdt0278-butxt)
      WHERE bukrs = w_zsdt0278-bukrs.

    IF sy-subrc IS NOT INITIAL.
      p_error = abap_true.
      MESSAGE 'Empresa não encontrada!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDIF.

  IF w_zsdt0278-bname IS NOT INITIAL.

    "Nome do usuário
    SELECT SINGLE name_text FROM v_username
      INTO (w_zsdt0278-name1)
      WHERE bname = w_zsdt0278-bname.

    IF sy-subrc IS NOT INITIAL.
      p_error = abap_true.
      MESSAGE 'Usuário não encontrado!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDIF.

  MOVE-CORRESPONDING w_zsdt0278 TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Preenche campos de leitura
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0278_0004 CHANGING p_registro_manter TYPE any.

  DATA: w_zsdt0278 TYPE zsdt0278_out.

  MOVE-CORRESPONDING p_registro_manter TO w_zsdt0278.

  "Empresas
  IF w_zsdt0278-bukrs IS NOT INITIAL.
    SELECT SINGLE butxt FROM t001
      INTO (w_zsdt0278-butxt)
      WHERE bukrs = w_zsdt0278-bukrs.
  ENDIF.

* Nome do usuário
  IF w_zsdt0278-bname IS NOT INITIAL.
    SELECT SINGLE name_text FROM v_username
      INTO (w_zsdt0278-name1)
      WHERE bname = w_zsdt0278-bname.
  ENDIF.

  MOVE-CORRESPONDING w_zsdt0278 TO p_registro_manter.

ENDFORM.

**----------------------------------------------------------------------*
*"Validações antes de apagar.
**----------------------------------------------------------------------*
FORM f_exit_zsdt0278_0006 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: w_zsdt0278     TYPE zsdt0278_out,
        w_zsdt0278_log TYPE zsdt0278_log.

  MOVE-CORRESPONDING p_registro_manter TO w_zsdt0278.

  MOVE-CORRESPONDING w_zsdt0278 TO w_zsdt0278_log.

  "Gravar registro tabela de log
  SELECT COUNT(*) FROM zsdt0278_log
    INTO  w_zsdt0278_log-seq
    WHERE bukrs = w_zsdt0278-bukrs
      AND bname = w_zsdt0278-bname.

  w_zsdt0278_log-data_i     = sy-datum.
  w_zsdt0278_log-usuario_i  = sy-uname.
  w_zsdt0278_log-hora_i     = sy-uzeit.

  MODIFY zsdt0278_log FROM w_zsdt0278_log.
  COMMIT WORK .

  MOVE-CORRESPONDING w_zsdt0278 TO p_registro_manter.

ENDFORM.
