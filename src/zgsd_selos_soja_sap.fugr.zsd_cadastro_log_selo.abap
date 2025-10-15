FUNCTION zsd_cadastro_log_selo.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(I_SAFRA) TYPE  ZDEPM_SAFRA OPTIONAL
*"  EXCEPTIONS
*"      NAO_EXISTE_LOG
*"----------------------------------------------------------------------

  FREE: t_zpmt0055,
        t_zpmx0055,
        t_idd07v,
        t_idd07v2.

*-selos
  SELECT *
    FROM zpmt0055
    INTO TABLE t_zpmt0055
   WHERE werks = i_werks
     AND safra = i_safra.

  IF sy-subrc <> 0.
    RAISE nao_existe_log.
    EXIT.
  ENDIF.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'ZIMAGEM'
      text           = 'X'
      langu          = sy-langu
    TABLES
      dd07v_tab      = t_idd07v
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'ZDEPM_CICLO_SEM'
      text           = 'X'
      langu          = sy-langu
    TABLES
      dd07v_tab      = t_idd07v2
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

*-monta saida
  LOOP AT t_zpmt0055 INTO w_zpmt0055.

    CLEAR w_idd07v.
    READ TABLE t_idd07v  INTO w_idd07v  WITH KEY domvalue_l = w_zpmt0055-imagem.

    CLEAR w_idd07v2.
    READ TABLE t_idd07v2 INTO w_idd07v2 WITH KEY domvalue_l = w_zpmt0055-ciclo_sem.

    MOVE-CORRESPONDING w_zpmt0055 TO w_zpmx0055.

    IF     w_zpmt0055-atividade = 'A'.
      w_zpmx0055-acao = 'Alteração'.
    ELSEIF w_zpmt0055-atividade = 'ES'.
      w_zpmx0055-acao = 'Exclusão'.
    ENDIF.

    w_zpmx0055-selo       = w_idd07v-ddtext.
    w_zpmx0055-ciclo_desc = w_idd07v2-ddtext.

    APPEND w_zpmx0055  TO t_zpmx0055.
  ENDLOOP.

  SORT t_zpmx0055 BY data_reg DESCENDING
                     hora_reg DESCENDING.

*-exibir logcadastro
  CALL SCREEN 100 STARTING AT 40   3
                    ENDING AT 150 15.

ENDFUNCTION.
