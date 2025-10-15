*&---------------------------------------------------------------------*
*& Report  ZFIS40
*&
*&---------------------------------------------------------------------*
*& JOB - Processa comprovantes reciclagem diretorio Finnet
*&---------------------------------------------------------------------*
*& Developer.: Anderson Oenning
*& Data......: 01.06.2021
*&---------------------------------------------------------------------*
*& Obs.: Deleta arquivo do diretorio /usr/sap/FINNET/ENTRADA/ que ja
*& foram processados com iniciais do nome 'COBR'.
*&---------------------------------------------------------------------*

REPORT zfis40.

DATA: gt_dir_unix   TYPE TABLE OF epsfili,
      gt_dir_retorn TYPE TABLE OF epsfili,
      v_dir_from    LIKE epsf-epsdirnam,
      file_dir_from LIKE epsf-epsdirnam,
      v_dir_to      LIKE epsf-epsdirnam.

DATA: r_name TYPE RANGE OF epsfilnam.


START-OF-SELECTION.
  SELECT SINGLE COUNT(*) INTO @DATA(vg_job)
      FROM tbtco
     WHERE jobname EQ 'PROCESSA_RECICLAGEM_FINNET'
       AND status EQ 'R'.
*
  IF ( vg_job EQ 1 ).
    PERFORM f_processa_arquivos_unix.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_ARQUIVOS_UNIX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_processa_arquivos_unix .
  v_dir_from = '/usr/sap/FINNET/ENTRADA/'.
  v_dir_to   = '/usr/sap/FINNET/ENTRADA/BACKUP_RETORNO/'.

  "Seleciona arquivo a ser processado.
  CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
    EXPORTING
      dir_name               = v_dir_from
    TABLES
      dir_list               = gt_dir_unix
    EXCEPTIONS
      invalid_eps_subdir     = 1
      sapgparam_failed       = 2
      build_directory_failed = 3
      no_authorization       = 4
      read_directory_failed  = 5
      too_many_read_errors   = 6
      empty_directory_list   = 7
      OTHERS                 = 8.


  CHECK gt_dir_unix IS NOT INITIAL.
  "Seleção de arquivo com incial "COBR"
  r_name = VALUE #( FOR ls IN gt_dir_unix WHERE ( name(4) EQ 'COBR' )
                  ( sign = 'I' option = 'EQ' low = ls-name ) ).

  CHECK r_name IS NOT INITIAL.
  DELETE gt_dir_unix WHERE name NOT IN r_name.


  "Seleciona arquivo pasta BKP.
  CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
    EXPORTING
      dir_name               = v_dir_to
    TABLES
      dir_list               = gt_dir_retorn
    EXCEPTIONS
      invalid_eps_subdir     = 1
      sapgparam_failed       = 2
      build_directory_failed = 3
      no_authorization       = 4
      read_directory_failed  = 5
      too_many_read_errors   = 6
      empty_directory_list   = 7
      OTHERS                 = 8.


  IF gt_dir_unix IS NOT INITIAL.
    SORT: gt_dir_unix BY name, gt_dir_retorn BY name.

    "Selecionar arquivo ja processados.
    LOOP AT gt_dir_unix ASSIGNING FIELD-SYMBOL(<ls_dir>).
      READ TABLE gt_dir_retorn INTO DATA(ws_dir) WITH KEY name = <ls_dir>-name BINARY SEARCH.
      IF sy-subrc EQ 0.
        file_dir_from = |{ v_dir_from }{ <ls_dir>-name }|.
        DELETE DATASET file_dir_from.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
