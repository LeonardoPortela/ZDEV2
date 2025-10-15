*&---------------------------------------------------------------------*
*& Include          ZFIS43_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_seleciona_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*

FORM f_seleciona_dados .

  SELECT a~bukrs a~branch
    FROM j_1bbranch AS a
    INNER JOIN adrc AS b
    ON a~adrnr = b~addrnumber
    INTO TABLE t_branch
    WHERE a~bukrs IN s_bukrs
      AND a~branch IN s_branch
      AND b~region IN s_uf.
  IF sy-subrc IS INITIAL.
    SORT t_branch BY branch.
  ENDIF.

  IF rb_conf IS NOT INITIAL AND t_branch IS NOT INITIAL.

    DATA(lt_branch) = t_branch.
    SORT lt_branch BY branch.
    DELETE ADJACENT DUPLICATES FROM lt_branch COMPARING branch.

    SELECT *
      FROM zfis_filial_conf
      INTO TABLE t_filial_conf
      FOR ALL ENTRIES IN lt_branch
      WHERE filial = lt_branch-branch.
    IF sy-subrc IS INITIAL.
      SORT t_filial_conf BY filial.
    ENDIF.

  ELSEIF rb_desc IS NOT INITIAL AND t_branch IS NOT INITIAL.

    DATA(lt_branch1) = t_branch.
    SORT lt_branch1 BY branch.
    DELETE ADJACENT DUPLICATES FROM lt_branch1 COMPARING branch.

    SELECT *
      FROM zfis_filial_desc
      INTO TABLE t_filial_desc
      FOR ALL ENTRIES IN lt_branch1
      WHERE branch = lt_branch1-branch.
    IF sy-subrc IS INITIAL.
      SORT t_filial_desc BY branch.
    ENDIF.

  ELSE.

    MESSAGE 'Nenhum registro encontrado!' TYPE 'E'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_monta_dados .
  DATA: lt_celltab    TYPE lvc_t_styl,
        wa_celltab    TYPE lvc_s_styl,
        ls_saida_conf TYPE ty_filial_conf.

  IF rb_conf IS NOT INITIAL.



    LOOP AT t_branch ASSIGNING FIELD-SYMBOL(<fs_branch>).

      CLEAR: lt_celltab,
             ls_saida_conf.

      wa_celltab-fieldname = 'FILIAL'.
      wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT wa_celltab INTO TABLE lt_celltab.

      ls_saida_conf-celltab = lt_celltab.
      ls_saida_conf-filial  = <fs_branch>-branch.

      READ TABLE t_filial_conf TRANSPORTING NO FIELDS
      WITH KEY filial = <fs_branch>-branch
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        LOOP AT t_filial_conf ASSIGNING FIELD-SYMBOL(<fs_conf>) FROM sy-tabix.
          IF <fs_conf>-filial <> <fs_branch>-branch.
            EXIT.
          ENDIF.

          wa_celltab-fieldname = 'TIPO_NOTA'.
          wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT wa_celltab INTO TABLE lt_celltab.

          ls_saida_conf-celltab = lt_celltab.
          MOVE-CORRESPONDING <fs_conf> TO ls_saida_conf.
          APPEND ls_saida_conf TO t_saida_conf.

        ENDLOOP.

        CONTINUE.

      ENDIF.

      APPEND ls_saida_conf TO t_saida_conf.
      CLEAR ls_saida_conf.

    ENDLOOP.

    SORT t_saida_conf BY filial.

  ELSEIF rb_desc IS NOT INITIAL.

    wa_celltab-fieldname = 'BRANCH'.
    wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT wa_celltab INTO TABLE lt_celltab.

    LOOP AT t_branch ASSIGNING <fs_branch>.

      APPEND INITIAL LINE TO t_saida_desc ASSIGNING FIELD-SYMBOL(<fs_saida_desc>).

      <fs_saida_desc>-branch = <fs_branch>-branch.

      READ TABLE t_filial_desc ASSIGNING FIELD-SYMBOL(<fs_desc>)
      WITH KEY branch = <fs_branch>-branch
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING <fs_desc> TO <fs_saida_desc>.
      ENDIF.

      <fs_saida_desc>-celltab = lt_celltab.

    ENDLOOP.

    SORT t_saida_desc BY branch.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_exibe_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_exibe_alv .
  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_salvar_conf
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_salvar_conf .
  DATA: l_valid TYPE c.

  CALL METHOD g_grid->check_changed_data
    IMPORTING
      e_valid = l_valid.

  DATA(lt_saida_conf) = t_saida_conf.
  DELETE lt_saida_conf WHERE dias_corte IS INITIAL AND
                            flag_apos_registro IS INITIAL AND
                            hora_fix_conf IS INITIAL AND
                            tipo_nota IS INITIAL.

  IF lt_saida_conf IS NOT INITIAL.

    LOOP AT lt_saida_conf ASSIGNING FIELD-SYMBOL(<fs_saida_conf>).
      IF <fs_saida_conf>-hora_fix_conf IS NOT INITIAL AND <fs_saida_conf>-flag_apos_registro IS NOT INITIAL.
        MESSAGE 'Colunas Hora Fixa e Dinâmico, não podem ser usados simultâneamente!' TYPE 'E'.
      ELSEIF <fs_saida_conf>-hora_fix_conf IS INITIAL AND <fs_saida_conf>-flag_apos_registro IS INITIAL.
        MESSAGE 'Obrigatório preenchimento da Colunas Hora Fixa ou Dinâmico!' TYPE 'E'.
      ENDIF.

    ENDLOOP.

    FREE t_filial_conf.

    MOVE-CORRESPONDING lt_saida_conf TO t_filial_conf.

    MODIFY zfis_filial_conf FROM TABLE t_filial_conf.
    IF sy-subrc IS INITIAL.
      COMMIT WORK.
      MESSAGE 'Dados salvos com sucesso!' TYPE 'S'.

      CALL METHOD g_grid->refresh_table_display.
    ENDIF.

  ELSE.

    MESSAGE 'Nenhuma modificação feita, nenhum dado foi salvo!' TYPE 'E'.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_salvar_desc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_salvar_desc .
  DATA: l_valid TYPE c.

  CALL METHOD g_grid->check_changed_data
    IMPORTING
      e_valid = l_valid.

  DATA(lt_saida_desc) = t_saida_desc.
  DELETE lt_saida_desc WHERE dias_descon IS INITIAL AND
                            data_fixa IS INITIAL AND
                            dias_envio_email IS INITIAL.

  IF lt_saida_desc IS NOT INITIAL.

    FREE t_filial_desc.

    MOVE-CORRESPONDING lt_saida_desc TO t_filial_desc.

    MODIFY zfis_filial_desc FROM TABLE t_filial_desc.
    IF sy-subrc IS INITIAL.
      COMMIT WORK.
      MESSAGE 'Dados salvos com sucesso!' TYPE 'S'.

      CALL METHOD g_grid->refresh_table_display.
    ENDIF.

  ELSE.

    MESSAGE 'Nenhuma modificação feita, nenhum dado foi salvo!' TYPE 'E'.

  ENDIF.
ENDFORM.
