*&---------------------------------------------------------------------*
*& Report  ZRD_zfit0174_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0174_exit.



FORM f_exit_zfit0174_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zfit0174 TYPE zfit0174_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zfit0174.

  SELECT SINGLE *
           FROM t001
           INTO @DATA(w_t001)
          WHERE bukrs = @w_zfit0174-bukrs.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Empresa informada está incorreta.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM t059p
           INTO @DATA(w_t059p)
          WHERE land1 = @w_t001-land1
            AND witht = @w_zfit0174-witht.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Código para categoria de imposto retido na fonte'
                     ' informado está incorreto.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM t059z
           INTO @DATA(w_t059z)
          WHERE land1     = @w_t001-land1
            AND witht     = @w_zfit0174-witht
            AND wt_withcd = @w_zfit0174-wt_withcd.

  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Código de imposto retido na fonte'
                     ' informado está incorreto.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zfit0174_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0174    TYPE zfit0174.

  CLEAR: wl_zfit0174.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zfit0174.

  wl_zfit0174-datum = sy-datum.
  wl_zfit0174-uzeit = sy-uzeit.
  wl_zfit0174-usnam = sy-uname.

  MOVE-CORRESPONDING wl_zfit0174 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0174_0004 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zfit0174_0005 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zfit0174_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.
ENDFORM.

FORM f_exit_zfit0174_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZFIT0174_OUT' AND
     p_field       = 'USNAM'.
    p_scrtext_l = 'Usuário'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0174_OUT' AND
     p_field       = 'DATUM'.
    p_scrtext_l = 'Data Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0174_OUT' AND
     p_field       = 'UZEIT'.
    p_scrtext_l = 'Hora Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0174_OUT' AND
     p_field       = 'INCIDE'.
    p_scrtext_l = 'Incidir?'.
    p_check     = abap_true.
    p_outputlen = 10.
    p_f4           = abap_true.
  ENDIF.

ENDFORM.

FORM f_exit_zfit0174_0009  TABLES pt_excl_toolbar
                            USING p_db_tab.
ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
