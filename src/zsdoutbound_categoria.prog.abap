*&---------------------------------------------------------------------*
*& Report  ZSDOUTBOUND_CATEGORIA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zsdoutbound_categoria.

DATA: it_categoria TYPE TABLE OF j_1baa.

SELECT * INTO TABLE it_categoria
  FROM j_1baa.

*--> 23.08.2023 00:16:47 - Migração S4 – ML - Início
*CALL FUNCTION 'Z_SD_OUTBOUND_CATEGORIA' IN BACKGROUND TASK
*  DESTINATION 'XI_CATEGORIA_NOTA'
*  TABLES
*    it_categoria = it_categoria[].

DATA: lv_rfc TYPE rfcdest.

CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_SD_OUTBOUND_CATEGORIA'.

CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
  EXPORTING
    i_fm          = c_fm
  IMPORTING
    e_rfc         = lv_rfc
  EXCEPTIONS
    no_rfc        = 1
    no_rfc_config = 2
    OTHERS        = 3.

IF sy-subrc EQ 0.
  CALL FUNCTION c_fm IN BACKGROUND TASK
    DESTINATION lv_rfc
    TABLES
      it_categoria = it_categoria[].
ELSE.
  CALL FUNCTION c_fm IN BACKGROUND TASK
    TABLES
      it_categoria = it_categoria[].
ENDIF.

COMMIT WORK.
*<-- 23.08.2023 00:16:47 - Migração S4 – ML – Fim
