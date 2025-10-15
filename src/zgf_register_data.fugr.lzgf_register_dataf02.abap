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
*&      Form  F_EXIT_0007
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
                                       p_check .

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
