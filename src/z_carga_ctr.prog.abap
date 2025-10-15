*&---------------------------------------------------------------------*
*& Report ZPABLO011
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_carga_ctr.

DATA: lv_file_path TYPE string,
      lt_data      TYPE STANDARD TABLE OF string WITH HEADER LINE.

DATA ano(4) TYPE c.
DATA mes(2) TYPE c.
DATA dia(2) TYPE c.
DATA hr(8) TYPE c.
DATA dt(10) TYPE c.

*TYPES: BEGIN OF ty_zmmt0177,
*         contract_id     TYPE zmmt0177-contract_id,
*         item_number     TYPE zmmt0177-item_number,
*         price           TYPE zmmt0177-price,
*         supplier_number TYPE zmmt0177-supplier_number,
*         created_at      TYPE zmmt0177-created_at,
*         updated_at      TYPE zmmt0177-updated_at,
*         purchasable     TYPE zmmt0177-purchasable,
*       END OF ty_zmmt0177.

TYPES: BEGIN OF ty_tmp,
         line(150) TYPE c,
       END OF ty_tmp.

DATA: it_tmp TYPE STANDARD TABLE OF ty_tmp INITIAL SIZE 0.
DATA: it_zmmt0177 TYPE STANDARD TABLE OF zmmt0177 WITH HEADER LINE.
DATA: wa_zmmt0177 TYPE zmmt0177.

" Specify the file path
lv_file_path = 'C:\Amaggi\Consolidado1405.csv'.

" Upload the file
CALL FUNCTION 'GUI_UPLOAD'
  EXPORTING
    filename                 = lv_file_path
  TABLES
    data_tab                 = lt_data[]
  EXCEPTIONS
    file_open_error          = 1
    file_read_error          = 2
    no_batch                 = 3
    gui_refused_filetransfer = 4
    invalid_type             = 5
    no_authority             = 6
    OTHERS                   = 7.
IF sy-subrc <> 0.
  MESSAGE 'Error uploading file' TYPE 'E'.
  RETURN.
ELSE.

  LOOP AT lt_data[] ASSIGNING FIELD-SYMBOL(<insert_table>).
    IF sy-tabix = 1.
    ELSE.
      SPLIT <insert_table> AT ';' INTO TABLE it_tmp.

      LOOP AT it_tmp ASSIGNING FIELD-SYMBOL(<convert_values>).
        "CONDENSE <convert_values>-line NO-GAPS.
        CASE sy-tabix.
          WHEN 1.
            it_zmmt0177-contract_id = <convert_values>-line.
          WHEN 2.

            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input  = <convert_values>-line
              IMPORTING
                output = it_zmmt0177-item_number.

          WHEN 3.
            it_zmmt0177-price = <convert_values>-line.
          WHEN 4.
            it_zmmt0177-supplier_number = <convert_values>-line.
          WHEN 5.
            CLEAR: ano,mes,dia,hr,dt.
            dt = <convert_values>-line+0(10).
            REPLACE ALL OCCURRENCES OF '/' IN dt WITH ''.
            REPLACE ALL OCCURRENCES OF '-' IN dt WITH ''.
            CONDENSE dt NO-GAPS.

            ano = dt+0(4).
            mes = dt+4(2).
            dia = dt+6(2).

            it_zmmt0177-created_at = |{ ano }{ mes }{ dia }|.
          WHEN 6.
            CLEAR: ano,mes,dia,hr,dt.
            dt = <convert_values>-line+0(10).
            REPLACE ALL OCCURRENCES OF '/' IN dt WITH ''.
            REPLACE ALL OCCURRENCES OF '-' IN dt WITH ''.
            CONDENSE dt NO-GAPS.

            ano = dt+0(4).
            mes = dt+4(2).
            dia = dt+6(2).
            it_zmmt0177-updated_at = |{ ano }{ mes }{ dia }|.
          WHEN 7.
            it_zmmt0177-purchasable = <convert_values>-line.
        ENDCASE.
      ENDLOOP.
      APPEND it_zmmt0177 TO it_zmmt0177[].

    ENDIF.
  ENDLOOP.

  MODIFY zmmt0177 FROM TABLE it_zmmt0177[].

ENDIF.
