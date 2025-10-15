*----------------------------------------------------------------------*
***INCLUDE ZREGISTER_DATA_EXIT.
*----------------------------------------------------------------------*


FORM f_exit_0001 CHANGING p_registro_manter TYPE any.

  "Set atributos iniciais ao incluir um novo registro

  CONCATENATE 'F_EXIT_' p_db_tab '_0001' INTO DATA(form_exit_0001).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  PERFORM (form_exit_0001) IN PROGRAM (program_exit) IF FOUND CHANGING p_registro_manter.

ENDFORM.


FORM f_exit_0002 USING p_registro_manter TYPE any
              CHANGING p_error.

  "Validações antes da gravação do registro

  CONCATENATE 'F_EXIT_' p_db_tab '_0002' INTO DATA(form_exit_0002).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  sy-ucomm = vg_operacao.

  PERFORM (form_exit_0002) IN PROGRAM (program_exit) IF FOUND
      USING p_registro_manter
   CHANGING p_error.

ENDFORM.


FORM f_exit_0003 CHANGING p_registro_manter TYPE any.

  "Set atributos antes de gravar o registro

  CONCATENATE 'F_EXIT_' p_db_tab '_0003' INTO DATA(form_exit_0003).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  PERFORM (form_exit_0003) IN PROGRAM (program_exit) IF FOUND CHANGING p_registro_manter.

ENDFORM.

FORM f_exit_0004 CHANGING p_saida TYPE any.

  "Set atributos antes append saida..

  CONCATENATE 'F_EXIT_' p_db_tab '_0004' INTO DATA(form_exit_0004).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  PERFORM (form_exit_0004) IN PROGRAM (program_exit) IF FOUND CHANGING p_saida.

ENDFORM.

FORM f_exit_0005 CHANGING p_registro_manter TYPE any.

  "Execute on PBO

  CONCATENATE 'F_EXIT_' p_db_tab '_0005' INTO DATA(form_exit_0005).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  sy-ucomm = vg_operacao.
  PERFORM (form_exit_0005) IN PROGRAM (program_exit) IF FOUND CHANGING p_registro_manter.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIT_0006
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<FS_WA_SAIDA>  text
*----------------------------------------------------------------------*
FORM f_exit_0006  USING p_saida CHANGING p_error.
  "Validações antes de apagar.

  CONCATENATE 'F_EXIT_' p_db_tab '_0006' INTO DATA(form_exit_0006).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  PERFORM (form_exit_0006) IN PROGRAM (program_exit) IF FOUND
      USING p_saida
   CHANGING p_error.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIT_0007
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<FS_WA_SAIDA>  text
*----------------------------------------------------------------------*
FORM f_exit_0007  TABLES t_saida.
  "Validações antes de apagar.

  CONCATENATE 'F_EXIT_' p_db_tab '_0007' INTO DATA(form_exit_0007).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  PERFORM (form_exit_0007) IN PROGRAM (program_exit) IF FOUND
      TABLES t_saida.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_EXIT_0008
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<FS_WA_SAIDA>  text
*----------------------------------------------------------------------*
FORM f_exit_0008 CHANGING VALUE(p_col_pos)       TYPE i
                          VALUE(p_ref_tabname)   LIKE dd02d-tabname
                          VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                          VALUE(p_tabname)       LIKE dd02d-tabname
                          VALUE(p_field)         LIKE dd03d-fieldname
                          VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                          VALUE(p_outputlen)
                          VALUE(p_edit)
                          VALUE(p_sum)
                          VALUE(p_emphasize)
                          VALUE(p_just)
                          VALUE(p_hotspot)
                          VALUE(p_f4)
                          VALUE(p_check).

  CONCATENATE 'F_EXIT_' p_db_tab '_0008' INTO DATA(form_exit_0008).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  PERFORM (form_exit_0008) IN PROGRAM (program_exit) IF FOUND
                              CHANGING p_col_pos
                                       p_ref_tabname
                                       p_ref_fieldname
                                       p_tabname
                                       p_field
                                       p_scrtext_l
                                       p_outputlen
                                       p_edit
                                       p_sum
                                       p_emphasize
                                       p_just
                                       p_hotspot
                                       p_f4
                                       p_check.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_EXIT_0008_v2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<FS_WA_SAIDA>  text
*----------------------------------------------------------------------*
FORM f_exit_0008_v2 CHANGING p_fcat_out TYPE lvc_s_fcat.

  CONCATENATE 'F_EXIT_' p_db_tab '_0008_V2' INTO DATA(form_exit_0008_v2).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  PERFORM (form_exit_0008_v2) IN PROGRAM (program_exit) IF FOUND
    CHANGING  p_fcat_out.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_EXIT_0007
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<FS_WA_SAIDA>  text
*----------------------------------------------------------------------*
FORM f_exit_0009 TABLES it_excl_toolbar.

  CONCATENATE 'F_EXIT_' p_db_tab '_0009' INTO DATA(form_exit_0009).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  PERFORM (form_exit_0009) IN PROGRAM (program_exit) IF FOUND
                               TABLES it_excl_toolbar
                                USING p_db_tab.

ENDFORM.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM f_exit_0010  TABLES t_saida.


  CONCATENATE 'F_EXIT_' p_db_tab '_0010' INTO DATA(form_exit_0010).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  PERFORM (form_exit_0010) IN PROGRAM (program_exit) IF FOUND
      TABLES t_saida.

ENDFORM.


FORM f_exit_0011 CHANGING p_registro_manter TYPE any
                          p_saida           TYPE any.

  "Set atributos antes de gravar o registro

  CONCATENATE 'F_EXIT_' p_db_tab '_0011' INTO DATA(form_exit_0011).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  PERFORM (form_exit_0011) IN PROGRAM (program_exit) IF FOUND CHANGING p_registro_manter p_saida.

ENDFORM.

FORM f_exit_0012 CHANGING p_registro_manter TYPE any
                          p_saida           TYPE any.

  "Set atributos antes de gravar o registro

  CONCATENATE 'F_EXIT_' p_db_tab '_0012' INTO DATA(form_exit_0012).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  PERFORM (form_exit_0012) IN PROGRAM (program_exit) IF FOUND CHANGING p_registro_manter p_saida.

ENDFORM.


FORM f_exit_0013 TABLES t_saida.

  CONCATENATE 'F_EXIT_' p_db_tab '_0013' INTO DATA(form_exit_0013).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  PERFORM (form_exit_0013) IN PROGRAM (program_exit) IF FOUND
      TABLES t_saida.
ENDFORM.

FORM f_exit_0014  USING p_saida CHANGING p_break.


  CONCATENATE 'F_EXIT_' p_db_tab '_0014' INTO DATA(form_exit_0014).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  PERFORM (form_exit_0014) IN PROGRAM (program_exit) IF FOUND
      USING p_saida
   CHANGING p_break.
ENDFORM.
FORM f_exit_0015  CHANGING p_saida.


  CONCATENATE 'F_EXIT_' p_db_tab '_0015' INTO DATA(form_exit_0015).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  sy-ucomm = vg_operacao.
  PERFORM (form_exit_0015) IN PROGRAM (program_exit) IF FOUND
      CHANGING p_saida.
ENDFORM.

FORM f_exit_0016 USING p_ucomm           TYPE sy-ucomm
              CHANGING p_registro_manter TYPE any
                       p_saida           TYPE any.

  "Set atributos antes de gravar o registro

  CONCATENATE 'F_EXIT_' p_db_tab '_0016' INTO DATA(form_exit).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  PERFORM (form_exit) IN PROGRAM (program_exit) IF FOUND USING p_ucomm CHANGING p_registro_manter p_saida.

ENDFORM.
FORM f_exit_0017 USING p_tipo .

  "Set atributos antes de gravar o registro

  CONCATENATE 'F_EXIT_' p_db_tab '_0017' INTO DATA(form_exit).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  PERFORM (form_exit) IN PROGRAM (program_exit) IF FOUND USING p_tipo.

ENDFORM.
FORM f_exit_0018 USING  p_saida
                        p_column_id
                        p_row_id.

  "Set atributos antes de gravar o registro

  CONCATENATE 'F_EXIT_' p_db_tab '_0018' INTO DATA(form_exit).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  PERFORM (form_exit) IN PROGRAM (program_exit) IF FOUND
  USING  p_saida
         p_column_id
         p_row_id.

ENDFORM.

FORM f_exit_0019 USING p_registro_search TYPE any
              CHANGING p_error.

  DATA: lwa_cond  TYPE rsds_where.

  "Validações antes da pesquisar o registro
  CONCATENATE 'F_EXIT_' p_db_tab '_0019' INTO DATA(form_exit_0019).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  sy-ucomm = vg_operacao.

  PERFORM (form_exit_0019) IN PROGRAM (program_exit) IF FOUND
      USING p_registro_search
   CHANGING p_error
            lwa_cond.

  gwa_cond_search = lwa_cond.

ENDFORM.
*FORM f_exit_0020  CHANGING p_refresh_selecao.
*
* " Flag para refresh
*  CONCATENATE 'F_EXIT_' p_db_tab '_0020' INTO DATA(form_exit_0020).
*  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).
*
*  PERFORM (form_exit_0020) IN PROGRAM (program_exit) IF FOUND
*      CHANGING p_refresh_selecao.
*ENDFORM.

*   MMSILVA - 11.02.2025 - #163322 - Inicio
FORM f_exit_0020.

  CONCATENATE 'F_EXIT_' p_db_tab '_0020' INTO DATA(form_exit_0020).
  CONCATENATE 'ZRD_' p_db_tab '_EXIT' INTO DATA(program_exit).

  PERFORM (form_exit_0020) IN PROGRAM (program_exit).

ENDFORM.
*   MMSILVA - 11.02.2025 - #163322 - Fim
