*&---------------------------------------------------------------------*
*& Report  ZLESR0096
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zlesr0096.

DATA: it_zlest0121 TYPE TABLE OF zlest0121 WITH HEADER LINE.
DATA: it_zlest0122 TYPE TABLE OF zlest0122 WITH HEADER LINE.

PARAMETER p_file  TYPE rlgrap-filename DEFAULT ''.
PARAMETER p_filer TYPE rlgrap-filename DEFAULT ''.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = p_file
      mask             = ',*.xlsx.'
      mode             = 'O'
      title            = 'Arquivo a importar !'
    IMPORTING
      filename         = p_file
    EXCEPTIONS
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_filer.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = p_filer
      mask             = ',*.xlsx.'
      mode             = 'O'
      title            = 'Arquivo a importar !'
    IMPORTING
      filename         = p_filer
    EXCEPTIONS
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.

START-OF-SELECTION.

  DATA: t_excel   LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE.
  DATA: t_excel2  LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE.

  IF p_file IS NOT INITIAL.
    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = p_file
        i_begin_col             = 1
        i_begin_row             = 3
        i_end_col               = 25
        i_end_row               = 1000
      TABLES
        intern                  = t_excel
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
  ENDIF.

  IF p_filer IS NOT INITIAL.
    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = p_filer
        i_begin_col             = 1
        i_begin_row             = 3
        i_end_col               = 25
        i_end_row               = 1000
      TABLES
        intern                  = t_excel2
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
  ENDIF.

  CLEAR: it_zlest0121[], it_zlest0122[].

  LOOP AT t_excel.
    CASE t_excel-col.
      WHEN 1.
        it_zlest0121-bukrs = t_excel-value.
      WHEN 2.
        it_zlest0121-branch = t_excel-value.
      WHEN 3.
        it_zlest0121-cd_pais = t_excel-value.
      WHEN 4.
        it_zlest0121-cd_cid_origem = t_excel-value.
      WHEN 5.
        it_zlest0121-cd_cid_destino = t_excel-value.
      WHEN 6.
        it_zlest0121-tp_proc_transp = t_excel-value.
      WHEN 7.
        it_zlest0121-tp_ida_volta = t_excel-value.
      WHEN 8.
        it_zlest0121-ds_observacao = t_excel-value.
      WHEN 9.
        it_zlest0121-tp_st_rota = t_excel-value.
      WHEN 10.
        it_zlest0121-ck_ativo = t_excel-value.
      WHEN 11.
        it_zlest0121-id_rota = t_excel-value.
        APPEND it_zlest0121.
    ENDCASE.
  ENDLOOP.

  LOOP AT t_excel2.
    CASE t_excel2-col.
      WHEN 1.
        it_zlest0122-id_rota = t_excel2-value.
      WHEN 2.
        it_zlest0122-id_rota_repom = t_excel2-value.
      WHEN 3.
        it_zlest0122-id_percurso_repom = t_excel2-value.
      WHEN 4.
        it_zlest0122-ds_percurso_repom = t_excel2-value.
      WHEN 5.
        it_zlest0122-cd_pais = t_excel2-value.
      WHEN 6.
        it_zlest0122-cd_cid_origem = t_excel2-value.
      WHEN 7.
        it_zlest0122-ds_cid_origem = t_excel2-value.
      WHEN 8.
        it_zlest0122-cd_cid_destino = t_excel2-value.
      WHEN 9.
        it_zlest0122-ds_cid_destino = t_excel2-value.
      WHEN 10.
        it_zlest0122-nr_km_ida = t_excel2-value.
      WHEN 11.
        it_zlest0122-nr_km_volta = t_excel2-value.
      WHEN 12.
        it_zlest0122-tp_proc_transp = t_excel2-value.
        APPEND it_zlest0122.
    ENDCASE.
  ENDLOOP.

  IF it_zlest0121[] IS NOT INITIAL.
    MODIFY zlest0121 FROM TABLE it_zlest0121.
    COMMIT WORK.
  ENDIF.

  IF it_zlest0122[] IS NOT INITIAL.
    MODIFY zlest0122 FROM TABLE it_zlest0122.
    COMMIT WORK.
  ENDIF.
