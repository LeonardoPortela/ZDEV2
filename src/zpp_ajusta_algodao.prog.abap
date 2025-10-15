*&---------------------------------------------------------------------*
*& Report ZPP_AJUSTA_ALGODAO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpp_ajusta_algodao.

TYPE-POOLS: truxs.

*——————————————————————–*
* Declaração de variáveis                                            *
*——————————————————————–*
DATA: it_raw TYPE truxs_t_text_data.

TYPES: BEGIN OF ty_final,
         col1  TYPE string,
         col2  TYPE string,
         col3  TYPE string,
         col4  TYPE string,
         col5  TYPE string,
         col6  TYPE string,
         col7  TYPE string,
         col8  TYPE string,
         col9  TYPE string,
         col10 TYPE string,
         col11 TYPE string,
         col12 TYPE string,
         col13 TYPE string.
TYPES: END OF  ty_final.

DATA: t_final     TYPE TABLE OF ty_final,
      w_final     TYPE ty_final,
      t_0002      TYPE TABLE OF zppt0002,
      w_0002      TYPE zppt0002,
      w_0002_del  TYPE zppt0002,
      l_id_cotton TYPE zppt0002-id_cotton.

*——————————————————————–*
*** Parâmetros de seleção
*——————————————————————–*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS:     p_file LIKE rlgrap-filename DEFAULT 'C:\CONVERSAO.XLS' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN  ON VALUE-REQUEST FOR p_file.
  PERFORM procura_arquivo.

START-OF-SELECTION.
  PERFORM captura_arquivo.
  PERFORM f_elimina_tab.

FORM procura_arquivo.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ''
      def_path         = 'C:\'
      mask             = ',Excel, *.xlsx.'
      mode             = 'O'
      title            = 'Selecionar arquivo para importação'
    IMPORTING
      filename         = p_file
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

ENDFORM.

FORM captura_arquivo .

* Ler Arquivo.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR    =
      i_line_header        = 'X'
      i_tab_raw_data       = it_raw
      i_filename           = p_file
    TABLES
      i_tab_converted_data = t_final[]
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

FORM f_elimina_tab.

  DELETE t_final WHERE col13 = '2' OR col13 = abap_off.

  LOOP AT t_final INTO w_final.

    l_id_cotton = w_final-col10.

    SELECT *
      FROM zppt0002
      INTO TABLE t_0002
     WHERE id_cotton = l_id_cotton.

    LOOP AT t_0002 INTO w_0002.
      MODIFY zppt0002_del     FROM w_0002.
      DELETE zppt0002         FROM w_0002.
    ENDLOOP.
    COMMIT WORK.

  ENDLOOP.

ENDFORM.
