*&---------------------------------------------------------------------*
*& Report  ZRD_ZLEST0208_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_v_t799battrib06c_exit.

"Programa anterior: ZLESR0150

FORM f_exit_v_t799battrib06c_0001 CHANGING p_registro_manter TYPE any.

*  DATA: wl_zlest0208 TYPE zlest0208.
*
*  CLEAR: wl_zlest0208.
*
*  wl_zlest0208-zdt_atual = sy-datum.
*  wl_zlest0208-zhr_atual = sy-uzeit.
*  wl_zlest0208-usnam = sy-uname.
*
*  MOVE-CORRESPONDING wl_zlest0208 TO p_registro_manter.

ENDFORM.

FORM f_exit_v_t799battrib06c_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

*  DATA: wl_zlest0208 TYPE zlest0208.
*
*  CLEAR: wl_zlest0208.
*
*  MOVE-CORRESPONDING p_registro_manter TO wl_zlest0208.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wl_zlest0208-saknr
*    IMPORTING
*      output = wl_zlest0208-saknr.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wl_zlest0208-kostl
*    IMPORTING
*      output = wl_zlest0208-kostl.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wl_zlest0208-matnr
*    IMPORTING
*      output = wl_zlest0208-matnr.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*    EXPORTING
*      input  = wl_zlest0208-matkl
*    IMPORTING
*      output = wl_zlest0208-matkl.
*
*  CLEAR: p_error.

ENDFORM.

FORM f_exit_v_t799battrib06c_0003 CHANGING p_registro_manter TYPE any.

*  DATA: wl_zlest0208 TYPE zlest0208.
*
*  CLEAR: wl_zlest0208.
*
*  MOVE-CORRESPONDING p_registro_manter TO wl_zlest0208.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wl_zlest0208-saknr
*    IMPORTING
*      output = wl_zlest0208-saknr.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wl_zlest0208-kostl
*    IMPORTING
*      output = wl_zlest0208-kostl.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wl_zlest0208-matnr
*    IMPORTING
*      output = wl_zlest0208-matnr.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*    EXPORTING
*      input  = wl_zlest0208-matkl
*    IMPORTING
*      output = wl_zlest0208-matkl.
*
*
*
*
*  wl_zlest0208-zdt_atual = sy-datum.
*  wl_zlest0208-zhr_atual = sy-uzeit.
*  wl_zlest0208-usnam = sy-uname.
*
*  MOVE-CORRESPONDING wl_zlest0208 TO p_registro_manter.

ENDFORM.

FORM f_exit_v_t799battrib06c_0004 CHANGING p_saida TYPE any.

*  DATA: wl_zlest0208_out TYPE zlest0208_out,
*        v_matnr          TYPE mara-matnr,
*        v_saknr          TYPE skat-saknr,
*        v_kostl          TYPE cskt-kostl,
*        v_matkl          TYPE t023t-matkl.
*
*
*  CLEAR: wl_zlest0208_out.
*
*  MOVE-CORRESPONDING p_saida TO wl_zlest0208_out.
*
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wl_zlest0208_out-saknr
*    IMPORTING
*      output = v_saknr.
*
*  SELECT SINGLE txt50
*  INTO wl_zlest0208_out-descr_saknr
*  FROM skat
*  WHERE spras = sy-langu
*  AND   saknr = v_saknr.
*
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wl_zlest0208_out-kostl
*    IMPORTING
*      output = v_kostl.
*
*  SELECT SINGLE ltext
*    INTO  wl_zlest0208_out-descr_kostl
*   FROM cskt
*  WHERE spras = sy-langu
*    AND kokrs = 'MAGI'
*    AND datbi >= sy-datum
*    AND kostl =  v_kostl .
*
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wl_zlest0208_out-matnr
*    IMPORTING
*      output = v_matnr.
*
*  SELECT SINGLE maktx
*    INTO wl_zlest0208_out-descr_matnr
*    FROM makt
*    WHERE spras = sy-langu
*    AND   matnr = v_matnr.
*
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*    EXPORTING
*      input  = wl_zlest0208_out-matkl
*    IMPORTING
*      output = v_matkl.
*
*
*  SELECT SINGLE wgbez60
*    INTO wl_zlest0208_out-descr_matkl
*   FROM t023t
*   WHERE spras = sy-langu
*    AND  matkl = v_matkl .
*
*
*
*  MOVE-CORRESPONDING wl_zlest0208_out TO p_saida.

ENDFORM.
