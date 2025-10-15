*&---------------------------------------------------------------------*
*& Report  ZRD_zfit0177_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0177_exit.

FORM f_exit_zfit0177_0001 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zfit0177_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zfit0177 TYPE zfit0177_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zfit0177.

  IF w_zfit0177-pais IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar código do país'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zfit0177-moeda IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar o código da moeda'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zfit0177_0004 CHANGING p_registro_manter TYPE any.
  DATA: w_zfit0177 TYPE zfit0177_out.
  CLEAR w_zfit0177.

  MOVE-CORRESPONDING p_registro_manter  TO w_zfit0177.

  SELECT SINGLE *
         FROM t005t
         INTO @DATA(w_t005)
        WHERE land1 = @w_zfit0177-pais.
  IF sy-subrc = 0.
    w_zfit0177-desc_pais = w_t005-landx.
  ENDIF.

  MOVE-CORRESPONDING w_zfit0177 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0177_0005 CHANGING p_registro_manter TYPE any.

  FIELD-SYMBOLS: <fs_name1> TYPE any,
                 <fs_name2> TYPE any.

  DATA: w_zfit0177 TYPE zfit0177_out.

  CLEAR w_zfit0177.

  ASSIGN ('(ZREGISTER_DATA)DD03L-PRECFIELD') TO <fs_name1>.
  IF sy-subrc = 0.
    CLEAR <fs_name1>.
  ENDIF.

  MOVE-CORRESPONDING p_registro_manter  TO w_zfit0177.

  SELECT SINGLE *
           FROM t005t
           INTO @DATA(w_t005)
          WHERE land1 = @w_zfit0177-pais.
  IF sy-subrc = 0.
    ASSIGN ('(ZREGISTER_DATA)DD03L-PRECFIELD') TO <fs_name1>.
    IF sy-subrc = 0.
      <fs_name1> = w_t005-landx.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_exit_zfit0177_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

ENDFORM.

FORM f_exit_zfit0177_0008 CHANGING p_col_pos
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


  IF p_ref_tabname = 'ZFIT0177_OUT' AND
   p_field       = 'DESC_PAIS'.
    p_scrtext_l = 'Descrição País'.
    p_outputlen = 60.
  ENDIF.


ENDFORM.

FORM f_exit_zfit0177_0009  TABLES it_excl_toolbar
                            USING p_db_tab.
ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
